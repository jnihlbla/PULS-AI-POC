000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2319700.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   98/01/20.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    PROGRAMMET ÄR EN KOPIA AV W23196                                     
001000*               W23196 KÖRS 1GGR/NDC                                      
001100*               W23197 BEHANDLAR NDC I USA                                
001200*    FUNKTION:                                                            
001300*      - LÄSER FIL W23195/NDC                                             
001400*      - SKAPAR LISTA REFILL UPPFÖLJNING NDC USA TOT                      
001500*                                                                         
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000     EJECT                                                                
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- GRUNDFIL ANALYS-LISTOR                                     
002400     SELECT W23195                     ASSIGN TO W23197D1.                
002500*          --- LISTA                                                      
002600     SELECT W23197-001                 ASSIGN TO W23197D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W23195                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500     SKIP2                                                                
003600*01  -COPY W23195        -L.                                              
003700     SKIP3                                                                
003800     SKIP3                                                                
003900 FD  W23197-001                                                           
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300 01  W23197-001-RAD              PIC X(169).                              
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W2317100'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  SW-KVOI-TRAFF               PIC X       VALUE 'N'.                   
005300 77  SW-ARTIKEL-SAKNAS-WDK7      PIC X       VALUE 'N'.                   
005400 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
005500 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
005700 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
005800 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
005900 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
006000 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
006200 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
006300 77  NDC-IX                      PIC 9(2)    VALUE ZERO COMP-3.           
006400 77  KVOI-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006500 77  ART-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  ART-IX-MAX                  PIC S9(3)   VALUE +72  COMP-3.           
006700 77  PSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006800 77  PSUM-IX-MAX                 PIC S9(3)   VALUE +9   COMP-3.           
006900 77  FSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007000 77  FSUM-IX-MAX                 PIC S9(3)   VALUE +8   COMP-3.           
007100 77  WS-KVAKS                    PIC S9(11)  VALUE ZERO COMP-3.           
007200 77  WS-KVLS-TOT                 PIC S9(11)  VALUE ZERO COMP-3.           
007300 77  WS-KVOKS-TOT                PIC S9(11)  VALUE ZERO COMP-3.           
007400 77  WS-KVOI                     PIC S9(11)V9(2)                          
007500                                   VALUE ZERO COMP-3.                     
007600 77  WS-KVOI-TOT-AAR             PIC S9(11)     VALUE ZERO COMP-3.        
007700 77  WS-KVOI-TOT-VECKA           PIC S9(11)     VALUE ZERO COMP-3.        
007800 77  WS-KVOI-TOT-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
007900 77  WS-KVOI-TEO-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
008000 77  WS-KVOT-SAKNAS-WDK7         PIC 9(9)       VALUE ZERO.               
008100 77  WS-KVOT-CDC-SAKNAS-WDK7     PIC 9(9)       VALUE ZERO.               
008200 77  WS-KVDISP                   PIC S9(11)     VALUE ZERO COMP-3.        
008300 77  WS-KVDISP-PR                PIC S9(11)V9(2)                          
008400                                   VALUE ZERO COMP-3.                     
008500 77  WS-SUMMA                    PIC S9(11)V9(2)                          
008600                                   VALUE ZERO COMP-3.                     
008700 77  WS-SUMMA-TOT                PIC S9(11)V9(2)                          
008800                                   VALUE ZERO COMP-3.                     
008900 77  WS-SUMMA-KR                 PIC S9(11)      VALUE ZERO.              
009000 77  WS-SERVG                    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
009100 77  WS-OLAGER                   PIC S9(11)      VALUE ZERO.              
009200 77  WS-MLAGER                   PIC S9(11)      VALUE ZERO.              
009300 77  WS-OMSHAST                  PIC 9(11)V9(1)  VALUE ZERO.              
009400 77  WS-PROC                     PIC S9(3)V9(1)  VALUE ZERO.              
009500 77  WS-KVPB-VECKA-SDC           PIC S9(6)V9(2)  VALUE ZERO.              
009600 77  WS-KVPB-DAG-SDC-NORM        PIC S9(6)V9(2)  VALUE ZERO.              
009700 77  WS-LT-BEHOV-SDC-NORM        PIC S9(7)V9(2)  VALUE ZERO.              
009800 77  WS-FIXAD-SUMMA              PIC S9(16)      VALUE ZERO.              
009900                                                                          
010000 77  FELTEXT                     PIC X(80)       VALUE SPACE.             
010100                                                                          
010200 77  W23195-EOF-SW                    PIC X       VALUE 'N'.              
010300     88  END-OF-W23195                VALUE 'J'.                          
010400                                                                          
010500 01  DAGENS-DATUM                PIC 9(6).                                
010600 01  FILLER REDEFINES DAGENS-DATUM.                                       
010700     03  DAGENS-AAR              PIC 9(2).                                
010800     03  DAGENS-MAANAD           PIC 9(2).                                
010900     03  DAGENS-DAG              PIC 9(2).                                
011000                                                                          
011100 01  DAGENS-VECKA                PIC 9(4).                                
011200 01  FILLER REDEFINES DAGENS-VECKA.                                       
011300     03  D-VECKA-AAR             PIC 9(2).                                
011400     03  D-VECKA-VECKA           PIC 9(2).                                
011500                                                                          
011600 01  VECKOR.                                                              
011700     03  AAVVD                   PIC 9(5).                                
011800     03  FILLER REDEFINES AAVVD.                                          
011900         05  AAVV                PIC 9(4).                                
012000         05  D                   PIC 9(1).                                
012100                                                                          
012200     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
012300     EJECT                                                                
012400 01  ART-TABELL.                                                          
012500     03 ART-RAD OCCURS 72.                                                
012600        05  ART-KVANT-AKT        PIC S9(9)      VALUE ZERO COMP-3.        
012700        05  ART-KVANT-PAS        PIC S9(9)      VALUE ZERO COMP-3.        
012800        05  ART-PROC-KVANT-A     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
012900        05  ART-PROC-KVANT-P     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013000        05  ART-KVDISP-AKT       PIC S9(11)     VALUE ZERO COMP-3.        
013100        05  ART-PROC-KVDISP-A    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013200        05  ART-KVDISP-PAS       PIC S9(11)     VALUE ZERO COMP-3.        
013300        05  ART-PROC-KVDISP-P    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013400        05  ART-LS-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
013500        05  ART-PROC-LS-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013600        05  ART-LS-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
013700        05  ART-PROC-LS-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013800        05  ART-AK-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
013900        05  ART-PROC-AK-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014000        05  ART-AK-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
014100        05  ART-PROC-AK-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014200        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
014300        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014400        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
014500        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014600        05  ART-MLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
014700        05  ART-PROC-MLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014800        05  ART-KVOT             PIC S9(9)      VALUE ZERO COMP-3.        
014900        05  ART-PROC-KVOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015000        05  ART-SPLIT            PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015100        05  ART-OMSHAST-DISP     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015200        05  ART-OMSHAST-PROC-D   PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015300        05  ART-OMSHAST-LS       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015400        05  ART-OMSHAST-PROC-LS  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015500        05  ART-SERVG-BTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015600        05  ART-SERVG-NTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015700*******  ARBETSFÄLT                                                       
015800        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
015900        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
016000        05  WS-ART-MLAGER        PIC S9(11)V9(2) VALUE ZERO.              
016100        05  WS-ART-LS-AKT        PIC S9(11)      VALUE ZERO.              
016200        05  WS-ART-LS-PAS        PIC S9(11)      VALUE ZERO.              
016300        05  WS-ART-LS-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
016400        05  WS-ART-LS-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
016500        05  WS-ART-KVLS-AKT      PIC S9(11)      VALUE ZERO.              
016600        05  WS-ART-KVLS-PAS      PIC S9(11)      VALUE ZERO.              
016700        05  WS-ART-KVDISP-AKT    PIC S9(11)      VALUE ZERO.              
016800        05  WS-ART-KVDISP-PAS    PIC S9(11)      VALUE ZERO.              
016900        05  WS-ART-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.              
017000        05  WS-ART-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.              
017100        05  WS-ART-KVOKS-AKT     PIC S9(11)      VALUE ZERO.              
017200        05  WS-ART-KVOKS-PAS     PIC S9(11)      VALUE ZERO.              
017300        05  WS-ART-OK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
017400        05  WS-ART-OK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
017500        05  WS-ART-KVAKS-AKT     PIC S9(11)      VALUE ZERO.              
017600        05  WS-ART-KVAKS-PAS     PIC S9(11)      VALUE ZERO.              
017700        05  WS-ART-AK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
017800        05  WS-ART-AK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
017900        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
018000        05  WS-ART-KVOI-AKT      PIC S9(9)      VALUE ZERO COMP-3.        
018100        05  WS-ART-KVOI-PAS      PIC S9(9)      VALUE ZERO COMP-3.        
018200        05  WS-ART-KVOI-TEO      PIC S9(9)      VALUE ZERO COMP-3.        
018300        05  WS-ART-KVOI-SAK      PIC S9(9)      VALUE ZERO COMP-3.        
018400        05  WS-ART-KVOI-CDC-AKT  PIC S9(9)      VALUE ZERO COMP-3.        
018500        05  WS-ART-KVOI-CDC-PAS  PIC S9(9)      VALUE ZERO COMP-3.        
018600        05  WS-ART-KVOI-CDC-TEO  PIC S9(9)      VALUE ZERO COMP-3.        
018700        05  WS-ART-KVOI-CDC-SAK  PIC S9(9)      VALUE ZERO COMP-3.        
018800        05  WS-ART-SUINKORD      PIC S9(16)V9(2)                          
018900                                                VALUE ZERO COMP-3.        
019000        05  WS-ART-SUFYSAVP      PIC S9(16)V9(2)                          
019100                                                VALUE ZERO COMP-3.        
019200        05  WS-ART-SUAVBRP       PIC S9(16)V9(2)                          
019300                                                VALUE ZERO COMP-3.        
019400        05  WS-ART-SULAGERB      PIC S9(16)V9(2)                          
019500                                                VALUE ZERO COMP-3.        
019600        05  WS-ART-SUSORTB       PIC S9(16)V9(2)                          
019700                                                VALUE ZERO COMP-3.        
019800     EJECT                                                                
019900 01  PSUM-TABELL.                                                         
020000     03 PSUM-RAD OCCURS 9.                                                
020100        05  PSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
020200        05  PSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
020300        05  PSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020400        05  PSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020500        05  PSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
020600        05  PSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020700        05  PSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
020800        05  PSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020900        05  PSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
021000        05  PSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021100        05  PSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
021200        05  PSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021300        05  PSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
021400        05  PSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021500        05  PSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
021600        05  PSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021700        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
021800        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021900        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
022000        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022100        05  PSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
022200        05  PSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022300        05  PSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
022400        05  PSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022500        05  PSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022600        05  PSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022700        05  PSUM-OMSHAST-PROC-D  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
022800        05  PSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022900        05  PSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
023000        05  PSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023100        05  PSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023200*******  ARBETSFÄLT                                                       
023300        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
023400        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
023500        05  WS-PSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
023600        05  WS-PSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
023700        05  WS-PSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
023800        05  WS-PSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
023900        05  WS-PSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
024000        05  WS-PSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
024100        05  WS-PSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
024200        05  WS-PSUM-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.             
024300        05  WS-PSUM-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.             
024400        05  WS-PSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
024500        05  WS-PSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
024600        05  WS-PSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
024700        05  WS-PSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
024800        05  WS-PSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
024900        05  WS-PSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
025000        05  WS-PSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
025100        05  WS-PSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
025200        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
025300        05  WS-PSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
025400        05  WS-PSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
025500        05  WS-PSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
025600        05  WS-PSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
025700        05  WS-PSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
025800        05  WS-PSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
025900        05  WS-PSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
026000        05  WS-PSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
026100        05  WS-PSUM-SUINKORD     PIC S9(16)V9(2)                          
026200                                                VALUE ZERO COMP-3.        
026300        05  WS-PSUM-SUFYSAVP     PIC S9(16)V9(2)                          
026400                                                VALUE ZERO COMP-3.        
026500        05  WS-PSUM-SUAVBRP      PIC S9(16)V9(2)                          
026600                                                VALUE ZERO COMP-3.        
026700        05  WS-PSUM-SULAGERB     PIC S9(16)V9(2)                          
026800                                                VALUE ZERO COMP-3.        
026900        05  WS-PSUM-SUSORTB      PIC S9(16)V9(2)                          
027000                                                VALUE ZERO COMP-3.        
027100     EJECT                                                                
027200 01  FSUM-TABELL.                                                         
027300     03 FSUM-RAD OCCURS 8.                                                
027400        05  FSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
027500        05  FSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
027600        05  FSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027700        05  FSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027800        05  FSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
027900        05  FSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028000        05  FSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
028100        05  FSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028200        05  FSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
028300        05  FSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028400        05  FSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
028500        05  FSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028600        05  FSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
028700        05  FSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028800        05  FSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
028900        05  FSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029000        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029100        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029200        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029300        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029400        05  FSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029500        05  FSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029600        05  FSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
029700        05  FSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029800        05  FSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029900        05  FSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030000        05  FSUM-OMSHAST-PROC-D PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030100        05  FSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030200        05  FSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
030300        05  FSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030400        05  FSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030500*******  ARBETSFÄLT                                                       
030600        05  WS-FSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
030700        05  WS-FSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
030800        05  WS-FSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
030900        05  WS-FSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
031000        05  WS-FSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
031100        05  WS-FSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
031200        05  WS-FSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
031300        05  WS-FSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
031400        05  WS-FSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
031500        05  WS-FSUM-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.            
031600        05  WS-FSUM-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.            
031700        05  WS-FSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
031800        05  WS-FSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
031900        05  WS-FSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032000        05  WS-FSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
032100        05  WS-FSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
032200        05  WS-FSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
032300        05  WS-FSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032400        05  WS-FSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
032500        05  WS-FSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
032600        05  WS-FSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
032700        05  WS-FSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
032800        05  WS-FSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
032900        05  WS-FSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
033000        05  WS-FSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
033100        05  WS-FSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
033200        05  WS-FSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
033300        05  WS-FSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
033400        05  WS-FSUM-SUINKORD     PIC S9(16)V9(2)                          
033500                                                VALUE ZERO COMP-3.        
033600        05  WS-FSUM-SUFYSAVP     PIC S9(16)V9(2)                          
033700                                                VALUE ZERO COMP-3.        
033800        05  WS-FSUM-SUAVBRP      PIC S9(16)V9(2)                          
033900                                                VALUE ZERO COMP-3.        
034000        05  WS-FSUM-SULAGERB     PIC S9(16)V9(2)                          
034100                                                VALUE ZERO COMP-3.        
034200        05  WS-FSUM-SUSORTB      PIC S9(16)V9(2)                          
034300                                                VALUE ZERO COMP-3.        
034400     EJECT                                                                
034500 01  TOTAL-RUTA.                                                          
034600     03  TOT-KVANT-AKT          PIC S9(9)      VALUE ZERO COMP-3.         
034700     03  TOT-KVANT-PAS          PIC S9(9)      VALUE ZERO COMP-3.         
034800     03  TOT-KVDISP-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
034900     03  TOT-KVDISP-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
035000     03  TOT-LS-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
035100     03  TOT-LS-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
035200     03  TOT-AK-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
035300     03  TOT-AK-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
035400     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035500     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035600     03  TOT-MLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035700     03  TOT-PROC-MLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035800     03  TOT-KVOT               PIC S9(9)      VALUE ZERO COMP-3.         
035900     03  TOT-PROC-KVOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036000     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
036100     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036200     03  TOT-SPLIT              PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036300     03  TOT-OMSHAST-DISP       PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036400     03  TOT-OMSHAST-LS         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036500     03  TOT-SERVG-BTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036600     03  TOT-SERVG-NTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036700*******  ARBETSFÄLT                                                       
036800     03  WS-TOT-SLAGER          PIC S9(11)V9(2) VALUE ZERO.               
036900     03  WS-TOT-OLAGER          PIC S9(11)V9(2) VALUE ZERO.               
037000     03  WS-TOT-MLAGER          PIC S9(11)V9(2) VALUE ZERO.               
037100     03  WS-TOT-LS-AKT          PIC S9(11)      VALUE ZERO.               
037200     03  WS-TOT-LS-PAS          PIC S9(11)      VALUE ZERO.               
037300     03  WS-TOT-LS-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
037400     03  WS-TOT-LS-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
037500     03  WS-TOT-KVDISP-AKT      PIC S9(11)      VALUE ZERO.               
037600     03  WS-TOT-KVDISP-PAS      PIC S9(11)      VALUE ZERO.               
037700     03  WS-TOT-KVDISP-PR-AKT   PIC S9(11)V9(2) VALUE ZERO.               
037800     03  WS-TOT-KVDISP-PR-PAS   PIC S9(11)V9(2) VALUE ZERO.               
037900     03  WS-TOT-KVOKS-AKT       PIC S9(11)      VALUE ZERO.               
038000     03  WS-TOT-KVOKS-PAS       PIC S9(11)      VALUE ZERO.               
038100     03  WS-TOT-OK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
038200     03  WS-TOT-OK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
038300     03  WS-TOT-KVAKS-AKT       PIC S9(11)      VALUE ZERO.               
038400     03  WS-TOT-KVAKS-PAS       PIC S9(11)      VALUE ZERO.               
038500     03  WS-TOT-AK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
038600     03  WS-TOT-AK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
038700     03  WS-TOT-KVOI            PIC S9(11)V9(2) VALUE ZERO.               
038800     03  WS-TOT-KVOI-AKT        PIC S9(9)       VALUE ZERO.               
038900     03  WS-TOT-KVOI-PAS        PIC S9(9)       VALUE ZERO.               
039000     03  WS-TOT-KVOI-TEO        PIC S9(9)       VALUE ZERO.               
039100     03  WS-TOT-KVOI-SAK        PIC S9(9)       VALUE ZERO.               
039200     03  WS-TOT-KVOI-CDC-AKT    PIC S9(9)       VALUE ZERO.               
039300     03  WS-TOT-KVOI-CDC-PAS    PIC S9(9)       VALUE ZERO.               
039400     03  WS-TOT-KVOI-CDC-TEO    PIC S9(9)       VALUE ZERO.               
039500     03  WS-TOT-KVOI-CDC-SAK    PIC S9(9)       VALUE ZERO.               
039600     03  WS-TOT-SUINKORD        PIC S9(16)V9(2)                           
039700                                                VALUE ZERO COMP-3.        
039800     03  WS-TOT-SUFYSAVP        PIC S9(16)V9(2)                           
039900                                                VALUE ZERO COMP-3.        
040000     03  WS-TOT-SUAVBRP         PIC S9(16)V9(2)                           
040100                                                VALUE ZERO COMP-3.        
040200     03  WS-TOT-SULAGERB        PIC S9(16)V9(2)                           
040300                                                VALUE ZERO COMP-3.        
040400     03  WS-TOT-SUSORTB         PIC S9(16)V9(2)                           
040500                                                VALUE ZERO COMP-3.        
040600     EJECT                                                                
040700*      --- VALID IDDC CODES                                               
040800*                                                                         
040900*01    -COPY WWDC99                                                       
041000*                                                                         
041100*01    -COPY WWDCKONS                                                     
041200                                                                          
041300 01  W-IDDC-SEND                 PIC X(2).                                
041400 01  W-IDDC-REC                  PIC X(2) VALUE SPACE.                    
041500 01  W-IDDC                      PIC X(2).                                
041600 01  WS-DC-TABELL.                                                        
041700     03 DC-TABELL OCCURS 500.                                             
041800        05  WS-IDDC-B601         PIC X(2) VALUE SPACE.                    
041900        05  WS-IDDC-B616         PIC X(2) VALUE SPACE.                    
042000        05  WS-KVDLTID-TOT       PIC 9(3) VALUE ZERO.                     
042100 01  IDDC-IX                     PIC 9(3).                                
042200 01  IDDC-IX-MAX                 PIC 9(3) VALUE 500.                      
042300                                                                          
042400       EJECT                                                              
042500 01  DYNAMISKA-SUBPROGRAM.                                                
042600*                                                                         
042700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
042800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
042900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
043000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
043100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
043200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
043300                                                                          
043400     EJECT                                                                
043500 01  PARAM-TILL-DATKORT.                                                  
043600     03  PROG-ID                 PIC X(8)    VALUE 'W2319700'.            
043700     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
043800*03  -COPY WDATKORT                                                       
043900     EJECT                                                                
044000*03  -COPY WDATAREA                                                       
044100     EJECT                                                                
044200*    --- PARAMETRAR TILL POSTSUM                                          
044300*01  -COPY W0005   -PRE  POSTSUM-                                         
044400     EJECT                                                                
044500 01  NDC-POST.                                                            
044600     03  FILLER             PIC X(2).                                     
044700     03  AKTUELLT-NDC       PIC X(2).                                     
044800     03  FILLER             PIC X(76).                                    
044900     EJECT                                                                
045000 01  IN-AREA-START               PIC X(24)   VALUE                        
045100                                 'IN-AREA-START    '.                     
045200*01  AREA  -COPY W23195     -PRE IN-                                      
045300     EJECT                                                                
045400 01  W001-AREA-START             PIC X(24)   VALUE                        
045500                                 'W001-AREA-START  '.                     
045600     SKIP2                                                                
045700 01  W001-HJALPAREOR.                                                     
045800*                                                                         
045900     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
046000     03  W001-ANTAL-RADER                                                 
046100                                 PIC 9(3)    VALUE 999.                   
046200     03  W001-MAX-RADER-PER-SIDA                                          
046300                                 PIC 9(3)    VALUE 63.                    
046400     03  W001-MAX-POSITIONER-PER-RAD                                      
046500                                 PIC 9(3)    VALUE 165.                   
046600     03  W001-LISTNR             PIC X(11)   VALUE SPACE.                 
046700     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
046800     SKIP2                                                                
046900 01  W001-RAD.                                                            
047000     03  FILLER                  PIC X(165)  VALUE SPACE.                 
047100     EJECT                                                                
047200 01  W001-RUBRIK1.                                                        
047300*                                                                         
047400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
047500     03  FILLER                  PIC X(18)                                
047600                              VALUE 'VOLVO CAR PARTS   '.                 
047700     03  W001-LISTID             PIC X(12)                                
047800                                 VALUE SPACE.                             
047900     03  FILLER                  PIC X(20)                                
048000             VALUE 'FOLLOW-UP REFILL    '.                                
048100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
048200     03  FILLER                  PIC X(4)    VALUE 'NDC '.                
048300     03  FILLER                  PIC X(2)    VALUE SPACE.                 
048400     03  W001-AKTUELLT-IDDC      PIC X(11)   VALUE SPACE.                 
048500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
048600     03  FILLER                  PIC X(6)    VALUE 'WEEK  '.              
048700     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
048800     03  FILLER                  PIC X(37)   VALUE SPACE.                 
048900     03  W001-DATUM              PIC XXBXXBXX.                            
049000     03  FILLER                  PIC X(6)    VALUE SPACE.                 
049100     03  FILLER                  PIC X(5)    VALUE 'PAGE '.               
049200     03  W001-SID                PIC Z(4)9.                               
049300     EJECT                                                                
049400 01  W001-RUBRIK3.                                                        
049500*                                                                         
049600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
049700     03  FILLER                  PIC X(11)                                
049800                             VALUE 'PRICE CLASS'.                         
049900     03  FILLER                  PIC X(11)   VALUE SPACE.                 
050000     03  FILLER                  PIC X(7)    VALUE 'A      '.             
050100     03  FILLER                  PIC X(7)    VALUE SPACE.                 
050200     03  FILLER                  PIC X(8)    VALUE 'B       '.            
050300     03  FILLER                  PIC X(6)    VALUE SPACE.                 
050400     03  FILLER                  PIC X(8)    VALUE 'C       '.            
050500     03  FILLER                  PIC X(6)    VALUE SPACE.                 
050600     03  FILLER                  PIC X(9)    VALUE 'D        '.           
050700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
050800     03  FILLER                  PIC X(9)    VALUE 'E        '.           
050900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
051000     03  FILLER                  PIC X(10)   VALUE 'F         '.          
051100     03  FILLER                  PIC X(4)    VALUE SPACE.                 
051200     03  FILLER                  PIC X(10)   VALUE 'G         '.          
051300     03  FILLER                  PIC X(4)    VALUE SPACE.                 
051400     03  FILLER                  PIC X(10)   VALUE 'H         '.          
051500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
051600     03  FILLER                  PIC X(6)    VALUE 'TOTAL '.              
051700     EJECT                                                                
051800 01  W001-DETALJRAD-1.                                                    
051900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052000     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
052100     03  FILLER                  PIC X       VALUE SPACE.                 
052200     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  A'.        
052300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052400     03  W001-DET1-KVANTA        PIC Z(7)9.                               
052500     03  FILLER                  PIC X       VALUE SPACE.                 
052600     03  W001-DET1-P-KVANTA      PIC Z9.9.                                
052700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052800     03  W001-DET1-KVANTB        PIC Z(7)9.                               
052900     03  FILLER                  PIC X       VALUE SPACE.                 
053000     03  W001-DET1-P-KVANTB      PIC Z9.9.                                
053100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053200     03  W001-DET1-KVANTC        PIC Z(7)9.                               
053300     03  FILLER                  PIC X       VALUE SPACE.                 
053400     03  W001-DET1-P-KVANTC      PIC Z9.9.                                
053500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053600     03  W001-DET1-KVANTD        PIC Z(7)9.                               
053700     03  FILLER                  PIC X       VALUE SPACE.                 
053800     03  W001-DET1-P-KVANTD      PIC Z9.9.                                
053900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054000     03  W001-DET1-KVANTE        PIC Z(7)9.                               
054100     03  FILLER                  PIC X       VALUE SPACE.                 
054200     03  W001-DET1-P-KVANTE      PIC Z9.9.                                
054300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054400     03  W001-DET1-KVANTF        PIC Z(7)9.                               
054500     03  FILLER                  PIC X       VALUE SPACE.                 
054600     03  W001-DET1-P-KVANTF      PIC Z9.9.                                
054700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054800     03  W001-DET1-KVANTG        PIC Z(7)9.                               
054900     03  FILLER                  PIC X       VALUE SPACE.                 
055000     03  W001-DET1-P-KVANTG      PIC Z9.9.                                
055100     03  FILLER                  PIC X       VALUE SPACE.                 
055200     03  W001-DET1-KVANTH        PIC Z(7)9.                               
055300     03  FILLER                  PIC X       VALUE SPACE.                 
055400     03  W001-DET1-P-KVANTH      PIC Z9.9.                                
055500     03  FILLER                  PIC X       VALUE SPACE.                 
055600     03  W001-DET1-TOT           PIC Z(13)9.                              
055700     03  FILLER                  PIC X       VALUE SPACE.                 
055800     03  W001-DET1-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
055900     EJECT                                                                
056000 01  W001-DETALJRAD-2.                                                    
056100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056200     03  W001-DET2-PRISKLASS     PIC X       VALUE SPACE.                 
056300     03  FILLER                  PIC X       VALUE SPACE.                 
056400     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  P'.        
056500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056600     03  W001-DET2-KVANTA        PIC Z(7)9.                               
056700     03  FILLER                  PIC X       VALUE SPACE.                 
056800     03  W001-DET2-P-KVANTA      PIC Z9.9.                                
056900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057000     03  W001-DET2-KVANTB        PIC Z(7)9.                               
057100     03  FILLER                  PIC X       VALUE SPACE.                 
057200     03  W001-DET2-P-KVANTB      PIC Z9.9.                                
057300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057400     03  W001-DET2-KVANTC        PIC Z(7)9.                               
057500     03  FILLER                  PIC X       VALUE SPACE.                 
057600     03  W001-DET2-P-KVANTC      PIC Z9.9.                                
057700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057800     03  W001-DET2-KVANTD        PIC Z(7)9.                               
057900     03  FILLER                  PIC X       VALUE SPACE.                 
058000     03  W001-DET2-P-KVANTD      PIC Z9.9.                                
058100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058200     03  W001-DET2-KVANTE        PIC Z(7)9.                               
058300     03  FILLER                  PIC X       VALUE SPACE.                 
058400     03  W001-DET2-P-KVANTE      PIC Z9.9.                                
058500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058600     03  W001-DET2-KVANTF        PIC Z(7)9.                               
058700     03  FILLER                  PIC X       VALUE SPACE.                 
058800     03  W001-DET2-P-KVANTF      PIC Z9.9.                                
058900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059000     03  W001-DET2-KVANTG        PIC Z(7)9.                               
059100     03  FILLER                  PIC X       VALUE SPACE.                 
059200     03  W001-DET2-P-KVANTG      PIC Z9.9.                                
059300     03  FILLER                  PIC X       VALUE SPACE.                 
059400     03  W001-DET2-KVANTH        PIC Z(7)9.                               
059500     03  FILLER                  PIC X       VALUE SPACE.                 
059600     03  W001-DET2-P-KVANTH      PIC Z9.9.                                
059700     03  FILLER                  PIC X       VALUE SPACE.                 
059800     03  W001-DET2-TOT           PIC Z(13)9.                              
059900     03  FILLER                  PIC X       VALUE SPACE.                 
060000     03  W001-DET2-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
060100     EJECT                                                                
060200 01  W001-DETALJRAD-3.                                                    
060300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
060400     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND A'.        
060500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060600     03  W001-DET3-DLAGERA       PIC Z(7)9.                               
060700     03  FILLER                  PIC X       VALUE SPACE.                 
060800     03  W001-DET3-P-DLAGERA     PIC Z9.9.                                
060900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061000     03  W001-DET3-DLAGERB       PIC Z(7)9.                               
061100     03  FILLER                  PIC X       VALUE SPACE.                 
061200     03  W001-DET3-P-DLAGERB     PIC Z9.9.                                
061300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061400     03  W001-DET3-DLAGERC       PIC Z(7)9.                               
061500     03  FILLER                  PIC X       VALUE SPACE.                 
061600     03  W001-DET3-P-DLAGERC     PIC Z9.9.                                
061700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061800     03  W001-DET3-DLAGERD       PIC Z(7)9.                               
061900     03  FILLER                  PIC X       VALUE SPACE.                 
062000     03  W001-DET3-P-DLAGERD     PIC Z9.9.                                
062100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062200     03  W001-DET3-DLAGERE       PIC Z(7)9.                               
062300     03  FILLER                  PIC X       VALUE SPACE.                 
062400     03  W001-DET3-P-DLAGERE     PIC Z9.9.                                
062500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062600     03  W001-DET3-DLAGERF       PIC Z(7)9.                               
062700     03  FILLER                  PIC X       VALUE SPACE.                 
062800     03  W001-DET3-P-DLAGERF     PIC Z9.9.                                
062900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063000     03  W001-DET3-DLAGERG       PIC Z(7)9.                               
063100     03  FILLER                  PIC X       VALUE SPACE.                 
063200     03  W001-DET3-P-DLAGERG     PIC Z9.9.                                
063300     03  FILLER                  PIC X       VALUE SPACE.                 
063400     03  W001-DET3-DLAGERH       PIC Z(7)9.                               
063500     03  FILLER                  PIC X       VALUE SPACE.                 
063600     03  W001-DET3-P-DLAGERH     PIC Z9.9.                                
063700     03  FILLER                  PIC X       VALUE SPACE.                 
063800     03  W001-DET3-TOT           PIC Z(13)9.                              
063900     03  FILLER                  PIC X       VALUE SPACE.                 
064000     03  W001-DET3-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
064100 01  W001-DETALJRAD-4.                                                    
064200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
064300     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND P'.        
064400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064500     03  W001-DET4-DLAGERA       PIC Z(7)9.                               
064600     03  FILLER                  PIC X       VALUE SPACE.                 
064700     03  W001-DET4-P-DLAGERA     PIC Z9.9.                                
064800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064900     03  W001-DET4-DLAGERB       PIC Z(7)9.                               
065000     03  FILLER                  PIC X       VALUE SPACE.                 
065100     03  W001-DET4-P-DLAGERB     PIC Z9.9.                                
065200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065300     03  W001-DET4-DLAGERC       PIC Z(7)9.                               
065400     03  FILLER                  PIC X       VALUE SPACE.                 
065500     03  W001-DET4-P-DLAGERC     PIC Z9.9.                                
065600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065700     03  W001-DET4-DLAGERD       PIC Z(7)9.                               
065800     03  FILLER                  PIC X       VALUE SPACE.                 
065900     03  W001-DET4-P-DLAGERD     PIC Z9.9.                                
066000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066100     03  W001-DET4-DLAGERE       PIC Z(7)9.                               
066200     03  FILLER                  PIC X       VALUE SPACE.                 
066300     03  W001-DET4-P-DLAGERE     PIC Z9.9.                                
066400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066500     03  W001-DET4-DLAGERF       PIC Z(7)9.                               
066600     03  FILLER                  PIC X       VALUE SPACE.                 
066700     03  W001-DET4-P-DLAGERF     PIC Z9.9.                                
066800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066900     03  W001-DET4-DLAGERG       PIC Z(7)9.                               
067000     03  FILLER                  PIC X       VALUE SPACE.                 
067100     03  W001-DET4-P-DLAGERG     PIC Z9.9.                                
067200     03  FILLER                  PIC X       VALUE SPACE.                 
067300     03  W001-DET4-DLAGERH       PIC Z(7)9.                               
067400     03  FILLER                  PIC X       VALUE SPACE.                 
067500     03  W001-DET4-P-DLAGERH     PIC Z9.9.                                
067600     03  FILLER                  PIC X       VALUE SPACE.                 
067700     03  W001-DET4-TOT           PIC Z(13)9.                              
067800     03  FILLER                  PIC X       VALUE SPACE.                 
067900     03  W001-DET4-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
068000     EJECT                                                                
068100 01  W001-DETALJRAD-5.                                                    
068200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
068300     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. A'.        
068400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068500     03  W001-DET5-LLAGERA       PIC Z(7)9.                               
068600     03  FILLER                  PIC X       VALUE SPACE.                 
068700     03  W001-DET5-P-LLAGERA     PIC Z9.9.                                
068800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068900     03  W001-DET5-LLAGERB       PIC Z(7)9.                               
069000     03  FILLER                  PIC X       VALUE SPACE.                 
069100     03  W001-DET5-P-LLAGERB     PIC Z9.9.                                
069200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069300     03  W001-DET5-LLAGERC       PIC Z(7)9.                               
069400     03  FILLER                  PIC X       VALUE SPACE.                 
069500     03  W001-DET5-P-LLAGERC     PIC Z9.9.                                
069600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069700     03  W001-DET5-LLAGERD       PIC Z(7)9.                               
069800     03  FILLER                  PIC X       VALUE SPACE.                 
069900     03  W001-DET5-P-LLAGERD     PIC Z9.9.                                
070000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070100     03  W001-DET5-LLAGERE       PIC Z(7)9.                               
070200     03  FILLER                  PIC X       VALUE SPACE.                 
070300     03  W001-DET5-P-LLAGERE     PIC Z9.9.                                
070400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070500     03  W001-DET5-LLAGERF       PIC Z(7)9.                               
070600     03  FILLER                  PIC X       VALUE SPACE.                 
070700     03  W001-DET5-P-LLAGERF     PIC Z9.9.                                
070800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070900     03  W001-DET5-LLAGERG       PIC Z(7)9.                               
071000     03  FILLER                  PIC X       VALUE SPACE.                 
071100     03  W001-DET5-P-LLAGERG     PIC Z9.9.                                
071200     03  FILLER                  PIC X       VALUE SPACE.                 
071300     03  W001-DET5-LLAGERH       PIC Z(7)9.                               
071400     03  FILLER                  PIC X       VALUE SPACE.                 
071500     03  W001-DET5-P-LLAGERH     PIC Z9.9.                                
071600     03  FILLER                  PIC X       VALUE SPACE.                 
071700     03  W001-DET5-TOT           PIC Z(13)9.                              
071800     03  FILLER                  PIC X       VALUE SPACE.                 
071900     03  W001-DET5-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
072000     EJECT                                                                
072100 01  W001-DETALJRAD-6.                                                    
072200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
072300     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. P'.        
072400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072500     03  W001-DET6-LLAGERA       PIC Z(7)9.                               
072600     03  FILLER                  PIC X       VALUE SPACE.                 
072700     03  W001-DET6-P-LLAGERA     PIC Z9.9.                                
072800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072900     03  W001-DET6-LLAGERB       PIC Z(7)9.                               
073000     03  FILLER                  PIC X       VALUE SPACE.                 
073100     03  W001-DET6-P-LLAGERB     PIC Z9.9.                                
073200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073300     03  W001-DET6-LLAGERC       PIC Z(7)9.                               
073400     03  FILLER                  PIC X       VALUE SPACE.                 
073500     03  W001-DET6-P-LLAGERC     PIC Z9.9.                                
073600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073700     03  W001-DET6-LLAGERD       PIC Z(7)9.                               
073800     03  FILLER                  PIC X       VALUE SPACE.                 
073900     03  W001-DET6-P-LLAGERD     PIC Z9.9.                                
074000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074100     03  W001-DET6-LLAGERE       PIC Z(7)9.                               
074200     03  FILLER                  PIC X       VALUE SPACE.                 
074300     03  W001-DET6-P-LLAGERE     PIC Z9.9.                                
074400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074500     03  W001-DET6-LLAGERF       PIC Z(7)9.                               
074600     03  FILLER                  PIC X       VALUE SPACE.                 
074700     03  W001-DET6-P-LLAGERF     PIC Z9.9.                                
074800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074900     03  W001-DET6-LLAGERG       PIC Z(7)9.                               
075000     03  FILLER                  PIC X       VALUE SPACE.                 
075100     03  W001-DET6-P-LLAGERG     PIC Z9.9.                                
075200     03  FILLER                  PIC X       VALUE SPACE.                 
075300     03  W001-DET6-LLAGERH       PIC Z(7)9.                               
075400     03  FILLER                  PIC X       VALUE SPACE.                 
075500     03  W001-DET6-P-LLAGERH     PIC Z9.9.                                
075600     03  FILLER                  PIC X       VALUE SPACE.                 
075700     03  W001-DET6-TOT           PIC Z(13)9.                              
075800     03  FILLER                  PIC X       VALUE SPACE.                 
075900     03  W001-DET6-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
076000     EJECT                                                                
076100 01  W001-DETALJRAD-7.                                                    
076200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
076300     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   A'.        
076400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076500     03  W001-DET7-ALAGERA       PIC Z(7)9.                               
076600     03  FILLER                  PIC X       VALUE SPACE.                 
076700     03  W001-DET7-P-ALAGERA     PIC Z9.9.                                
076800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076900     03  W001-DET7-ALAGERB       PIC Z(7)9.                               
077000     03  FILLER                  PIC X       VALUE SPACE.                 
077100     03  W001-DET7-P-ALAGERB     PIC Z9.9.                                
077200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077300     03  W001-DET7-ALAGERC       PIC Z(7)9.                               
077400     03  FILLER                  PIC X       VALUE SPACE.                 
077500     03  W001-DET7-P-ALAGERC     PIC Z9.9.                                
077600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077700     03  W001-DET7-ALAGERD       PIC Z(7)9.                               
077800     03  FILLER                  PIC X       VALUE SPACE.                 
077900     03  W001-DET7-P-ALAGERD     PIC Z9.9.                                
078000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078100     03  W001-DET7-ALAGERE       PIC Z(7)9.                               
078200     03  FILLER                  PIC X       VALUE SPACE.                 
078300     03  W001-DET7-P-ALAGERE     PIC Z9.9.                                
078400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078500     03  W001-DET7-ALAGERF       PIC Z(7)9.                               
078600     03  FILLER                  PIC X       VALUE SPACE.                 
078700     03  W001-DET7-P-ALAGERF     PIC Z9.9.                                
078800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078900     03  W001-DET7-ALAGERG       PIC Z(7)9.                               
079000     03  FILLER                  PIC X       VALUE SPACE.                 
079100     03  W001-DET7-P-ALAGERG     PIC Z9.9.                                
079200     03  FILLER                  PIC X       VALUE SPACE.                 
079300     03  W001-DET7-ALAGERH       PIC Z(7)9.                               
079400     03  FILLER                  PIC X       VALUE SPACE.                 
079500     03  W001-DET7-P-ALAGERH     PIC Z9.9.                                
079600     03  FILLER                  PIC X       VALUE SPACE.                 
079700     03  W001-DET7-TOT           PIC Z(13)9.                              
079800     03  FILLER                  PIC X       VALUE SPACE.                 
079900     03  W001-DET7-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
080000     EJECT                                                                
080100 01  W001-DETALJRAD-8.                                                    
080200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
080300     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   P'.        
080400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080500     03  W001-DET8-ALAGERA       PIC Z(7)9.                               
080600     03  FILLER                  PIC X       VALUE SPACE.                 
080700     03  W001-DET8-P-ALAGERA     PIC Z9.9.                                
080800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080900     03  W001-DET8-ALAGERB       PIC Z(7)9.                               
081000     03  FILLER                  PIC X       VALUE SPACE.                 
081100     03  W001-DET8-P-ALAGERB     PIC Z9.9.                                
081200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081300     03  W001-DET8-ALAGERC       PIC Z(7)9.                               
081400     03  FILLER                  PIC X       VALUE SPACE.                 
081500     03  W001-DET8-P-ALAGERC     PIC Z9.9.                                
081600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081700     03  W001-DET8-ALAGERD       PIC Z(7)9.                               
081800     03  FILLER                  PIC X       VALUE SPACE.                 
081900     03  W001-DET8-P-ALAGERD     PIC Z9.9.                                
082000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082100     03  W001-DET8-ALAGERE       PIC Z(7)9.                               
082200     03  FILLER                  PIC X       VALUE SPACE.                 
082300     03  W001-DET8-P-ALAGERE     PIC Z9.9.                                
082400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082500     03  W001-DET8-ALAGERF       PIC Z(7)9.                               
082600     03  FILLER                  PIC X       VALUE SPACE.                 
082700     03  W001-DET8-P-ALAGERF     PIC Z9.9.                                
082800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082900     03  W001-DET8-ALAGERG       PIC Z(7)9.                               
083000     03  FILLER                  PIC X       VALUE SPACE.                 
083100     03  W001-DET8-P-ALAGERG     PIC Z9.9.                                
083200     03  FILLER                  PIC X       VALUE SPACE.                 
083300     03  W001-DET8-ALAGERH       PIC Z(7)9.                               
083400     03  FILLER                  PIC X       VALUE SPACE.                 
083500     03  W001-DET8-P-ALAGERH     PIC Z9.9.                                
083600     03  FILLER                  PIC X       VALUE SPACE.                 
083700     03  W001-DET8-TOT           PIC Z(13)9.                              
083800     03  FILLER                  PIC X       VALUE SPACE.                 
083900     03  W001-DET8-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
084000     EJECT                                                                
084100 01  W001-DETALJRAD-9.                                                    
084200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
084300     03  FILLER                  PIC X(12)   VALUE 'OVERSTOCK   '.        
084400     03  FILLER                  PIC X       VALUE SPACE.                 
084500     03  W001-DET9-OLAGERA       PIC Z(7)9.                               
084600     03  FILLER                  PIC X       VALUE SPACE.                 
084700     03  W001-DET9-P-OLAGERA     PIC Z9.9    BLANK WHEN ZERO.             
084800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084900     03  W001-DET9-OLAGERB       PIC Z(7)9.                               
085000     03  FILLER                  PIC X       VALUE SPACE.                 
085100     03  W001-DET9-P-OLAGERB     PIC Z9.9    BLANK WHEN ZERO.             
085200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085300     03  W001-DET9-OLAGERC       PIC Z(7)9.                               
085400     03  FILLER                  PIC X       VALUE SPACE.                 
085500     03  W001-DET9-P-OLAGERC     PIC Z9.9    BLANK WHEN ZERO.             
085600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085700     03  W001-DET9-OLAGERD       PIC Z(7)9.                               
085800     03  FILLER                  PIC X       VALUE SPACE.                 
085900     03  W001-DET9-P-OLAGERD     PIC Z9.9    BLANK WHEN ZERO.             
086000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086100     03  W001-DET9-OLAGERE       PIC Z(7)9.                               
086200     03  FILLER                  PIC X       VALUE SPACE.                 
086300     03  W001-DET9-P-OLAGERE     PIC Z9.9    BLANK WHEN ZERO.             
086400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086500     03  W001-DET9-OLAGERF       PIC Z(7)9.                               
086600     03  FILLER                  PIC X       VALUE SPACE.                 
086700     03  W001-DET9-P-OLAGERF     PIC Z9.9    BLANK WHEN ZERO.             
086800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086900     03  W001-DET9-OLAGERG       PIC Z(7)9.                               
087000     03  FILLER                  PIC X       VALUE SPACE.                 
087100     03  W001-DET9-P-OLAGERG     PIC Z9.9    BLANK WHEN ZERO.             
087200     03  FILLER                  PIC X       VALUE SPACE.                 
087300     03  W001-DET9-OLAGERH       PIC Z(7)9.                               
087400     03  FILLER                  PIC X       VALUE SPACE.                 
087500     03  W001-DET9-P-OLAGERH     PIC Z9.9    BLANK WHEN ZERO.             
087600     03  FILLER                  PIC X       VALUE SPACE.                 
087700     03  W001-DET9-TOT           PIC Z(13)9.                              
087800     03  FILLER                  PIC X       VALUE SPACE.                 
087900     03  W001-DET9-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
088000     EJECT                                                                
088100 01  W001-DETALJRAD-10.                                                   
088200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
088300     03  FILLER                  PIC X(12)   VALUE 'SAF. STOCK  '.        
088400     03  FILLER                  PIC X       VALUE SPACE.                 
088500     03  W001-DET10-SLAGERA      PIC Z(7)9.                               
088600     03  FILLER                  PIC X       VALUE SPACE.                 
088700     03  W001-DET10-P-SLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
088800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088900     03  W001-DET10-SLAGERB      PIC Z(7)9.                               
089000     03  FILLER                  PIC X       VALUE SPACE.                 
089100     03  W001-DET10-P-SLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
089200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089300     03  W001-DET10-SLAGERC      PIC Z(7)9.                               
089400     03  FILLER                  PIC X       VALUE SPACE.                 
089500     03  W001-DET10-P-SLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
089600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089700     03  W001-DET10-SLAGERD      PIC Z(7)9.                               
089800     03  FILLER                  PIC X       VALUE SPACE.                 
089900     03  W001-DET10-P-SLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
090000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090100     03  W001-DET10-SLAGERE      PIC Z(7)9.                               
090200     03  FILLER                  PIC X       VALUE SPACE.                 
090300     03  W001-DET10-P-SLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
090400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090500     03  W001-DET10-SLAGERF      PIC Z(7)9.                               
090600     03  FILLER                  PIC X       VALUE SPACE.                 
090700     03  W001-DET10-P-SLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
090800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090900     03  W001-DET10-SLAGERG      PIC Z(7)9.                               
091000     03  FILLER                  PIC X       VALUE SPACE.                 
091100     03  W001-DET10-P-SLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
091200     03  FILLER                  PIC X       VALUE SPACE.                 
091300     03  W001-DET10-SLAGERH      PIC Z(7)9.                               
091400     03  FILLER                  PIC X       VALUE SPACE.                 
091500     03  W001-DET10-P-SLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
091600     03  FILLER                  PIC X       VALUE SPACE.                 
091700     03  W001-DET10-TOT          PIC Z(13)9.                              
091800     03  FILLER                  PIC X       VALUE SPACE.                 
091900     03  W001-DET10-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
092000     EJECT                                                                
092100 01  W001-DETALJRAD-11.                                                   
092200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
092300     03  FILLER                  PIC X(12)   VALUE 'AVERAGE ST. '.        
092400     03  FILLER                  PIC X       VALUE SPACE.                 
092500     03  W001-DET11-MLAGERA      PIC Z(7)9.                               
092600     03  FILLER                  PIC X       VALUE SPACE.                 
092700     03  W001-DET11-P-MLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
092800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092900     03  W001-DET11-MLAGERB      PIC Z(7)9.                               
093000     03  FILLER                  PIC X       VALUE SPACE.                 
093100     03  W001-DET11-P-MLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
093200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093300     03  W001-DET11-MLAGERC      PIC Z(7)9.                               
093400     03  FILLER                  PIC X       VALUE SPACE.                 
093500     03  W001-DET11-P-MLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
093600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093700     03  W001-DET11-MLAGERD      PIC Z(7)9.                               
093800     03  FILLER                  PIC X       VALUE SPACE.                 
093900     03  W001-DET11-P-MLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
094000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094100     03  W001-DET11-MLAGERE      PIC Z(7)9.                               
094200     03  FILLER                  PIC X       VALUE SPACE.                 
094300     03  W001-DET11-P-MLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
094400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094500     03  W001-DET11-MLAGERF      PIC Z(7)9.                               
094600     03  FILLER                  PIC X       VALUE SPACE.                 
094700     03  W001-DET11-P-MLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
094800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094900     03  W001-DET11-MLAGERG      PIC Z(7)9.                               
095000     03  FILLER                  PIC X       VALUE SPACE.                 
095100     03  W001-DET11-P-MLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
095200     03  FILLER                  PIC X       VALUE SPACE.                 
095300     03  W001-DET11-MLAGERH      PIC Z(7)9.                               
095400     03  FILLER                  PIC X       VALUE SPACE.                 
095500     03  W001-DET11-P-MLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
095600     03  FILLER                  PIC X       VALUE SPACE.                 
095700     03  W001-DET11-TOT          PIC Z(13)9.                              
095800     03  FILLER                  PIC X       VALUE SPACE.                 
095900     03  W001-DET11-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
096000     EJECT                                                                
096100 01  W001-DETALJRAD-12.                                                   
096200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096300     03  W001-DET12-PRISKLASS    PIC X       VALUE SPACE.                 
096400     03  FILLER                  PIC X       VALUE SPACE.                 
096500     03  FILLER                  PIC X(12)   VALUE 'NO INCOM ORD'.        
096600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096700     03  W001-DET12-KVOTA        PIC Z(7)9.                               
096800     03  FILLER                  PIC X       VALUE SPACE.                 
096900     03  W001-DET12-P-KVOTA      PIC Z9.9.                                
097000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097100     03  W001-DET12-KVOTB        PIC Z(7)9.                               
097200     03  FILLER                  PIC X       VALUE SPACE.                 
097300     03  W001-DET12-P-KVOTB      PIC Z9.9.                                
097400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097500     03  W001-DET12-KVOTC        PIC Z(7)9.                               
097600     03  FILLER                  PIC X       VALUE SPACE.                 
097700     03  W001-DET12-P-KVOTC      PIC Z9.9.                                
097800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097900     03  W001-DET12-KVOTD        PIC Z(7)9.                               
098000     03  FILLER                  PIC X       VALUE SPACE.                 
098100     03  W001-DET12-P-KVOTD      PIC Z9.9.                                
098200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098300     03  W001-DET12-KVOTE        PIC Z(7)9.                               
098400     03  FILLER                  PIC X       VALUE SPACE.                 
098500     03  W001-DET12-P-KVOTE      PIC Z9.9.                                
098600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098700     03  W001-DET12-KVOTF        PIC Z(7)9.                               
098800     03  FILLER                  PIC X       VALUE SPACE.                 
098900     03  W001-DET12-P-KVOTF      PIC Z9.9.                                
099000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
099100     03  W001-DET12-KVOTG        PIC Z(7)9.                               
099200     03  FILLER                  PIC X       VALUE SPACE.                 
099300     03  W001-DET12-P-KVOTG      PIC Z9.9.                                
099400     03  FILLER                  PIC X       VALUE SPACE.                 
099500     03  W001-DET12-KVOTH        PIC Z(7)9.                               
099600     03  FILLER                  PIC X       VALUE SPACE.                 
099700     03  W001-DET12-P-KVOTH      PIC Z9.9.                                
099800     03  FILLER                  PIC X       VALUE SPACE.                 
099900     03  W001-DET12-TOT          PIC Z(13)9.                              
100000     03  FILLER                  PIC X       VALUE SPACE.                 
100100     03  W001-DET12-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
100200     EJECT                                                                
100300 01  W001-DETALJRAD-13.                                                   
100400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
100500     03  FILLER                  PIC X(13) VALUE 'SPLIT FACTOR '.         
100600     03  FILLER                  PIC X(4)    VALUE SPACE.                 
100700     03  W001-DET13-SPLITA       PIC Z9.9.                                
100800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
100900     03  W001-DET13-SPLITB       PIC Z9.9.                                
101000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101100     03  W001-DET13-SPLITC       PIC Z9.9.                                
101200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101300     03  W001-DET13-SPLITD       PIC Z9.9.                                
101400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101500     03  W001-DET13-SPLITE       PIC Z9.9.                                
101600     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101700     03  W001-DET13-SPLITF       PIC Z9.9.                                
101800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101900     03  W001-DET13-SPLITG       PIC Z9.9.                                
102000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
102100     03  W001-DET13-SPLITH       PIC Z9.9.                                
102200     03  FILLER                  PIC X(16)   VALUE SPACE.                 
102300     03  W001-DET13-TOT          PIC Z9.9.                                
102400     03  FILLER                  PIC X       VALUE SPACE.                 
102500     03  W001-DET13-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
102600     EJECT                                                                
102700 01  W001-DETALJRAD-14.                                                   
102800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
102900     03  FILLER                  PIC X(14) VALUE 'TOR        SOH'.        
103000     03  W001-DET14-OMSHASTA     PIC Z(4)9.9.                             
103100     03  FILLER                  PIC X       VALUE SPACE.                 
103200     03  W001-DET14-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
103300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103400     03  W001-DET14-OMSHASTB     PIC Z(5)9.9.                             
103500     03  FILLER                  PIC X       VALUE SPACE.                 
103600     03  W001-DET14-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
103700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103800     03  W001-DET14-OMSHASTC     PIC Z(5)9.9.                             
103900     03  FILLER                  PIC X       VALUE SPACE.                 
104000     03  W001-DET14-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
104100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104200     03  W001-DET14-OMSHASTD     PIC Z(5)9.9.                             
104300     03  FILLER                  PIC X       VALUE SPACE.                 
104400     03  W001-DET14-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
104500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104600     03  W001-DET14-OMSHASTE     PIC Z(5)9.9.                             
104700     03  FILLER                  PIC X       VALUE SPACE.                 
104800     03  W001-DET14-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
104900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105000     03  W001-DET14-OMSHASTF     PIC Z(5)9.9.                             
105100     03  FILLER                  PIC X       VALUE SPACE.                 
105200     03  W001-DET14-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
105300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105400     03  W001-DET14-OMSHASTG     PIC Z(5)9.9.                             
105500     03  FILLER                  PIC X       VALUE SPACE.                 
105600     03  W001-DET14-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
105700     03  FILLER                  PIC X       VALUE SPACE.                 
105800     03  W001-DET14-OMSHASTH     PIC Z(5)9.9.                             
105900     03  FILLER                  PIC X       VALUE SPACE.                 
106000     03  W001-DET14-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
106100     03  FILLER                  PIC X       VALUE SPACE.                 
106200     03  W001-DET14-TOT          PIC Z(11)9.9.                            
106300     03  FILLER                  PIC X       VALUE SPACE.                 
106400     03  W001-DET14-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
106500     EJECT                                                                
106600 01  W001-DETALJRAD-15.                                                   
106700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
106800     03  FILLER                  PIC X(14) VALUE 'TOR BAL+AK+GIT'.        
106900     03  W001-DET15-OMSHASTA     PIC Z(4)9.9.                             
107000     03  FILLER                  PIC X       VALUE SPACE.                 
107100     03  W001-DET15-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
107200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107300     03  W001-DET15-OMSHASTB     PIC Z(5)9.9.                             
107400     03  FILLER                  PIC X       VALUE SPACE.                 
107500     03  W001-DET15-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
107600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107700     03  W001-DET15-OMSHASTC     PIC Z(5)9.9.                             
107800     03  FILLER                  PIC X       VALUE SPACE.                 
107900     03  W001-DET15-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
108000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108100     03  W001-DET15-OMSHASTD     PIC Z(5)9.9.                             
108200     03  FILLER                  PIC X       VALUE SPACE.                 
108300     03  W001-DET15-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
108400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108500     03  W001-DET15-OMSHASTE     PIC Z(5)9.9.                             
108600     03  FILLER                  PIC X       VALUE SPACE.                 
108700     03  W001-DET15-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
108800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108900     03  W001-DET15-OMSHASTF     PIC Z(5)9.9.                             
109000     03  FILLER                  PIC X       VALUE SPACE.                 
109100     03  W001-DET15-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
109200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109300     03  W001-DET15-OMSHASTG     PIC Z(5)9.9.                             
109400     03  FILLER                  PIC X       VALUE SPACE.                 
109500     03  W001-DET15-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
109600     03  FILLER                  PIC X       VALUE SPACE.                 
109700     03  W001-DET15-OMSHASTH     PIC Z(5)9.9.                             
109800     03  FILLER                  PIC X       VALUE SPACE.                 
109900     03  W001-DET15-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
110000     03  FILLER                  PIC X       VALUE SPACE.                 
110100     03  W001-DET15-TOT          PIC Z(11)9.9.                            
110200     03  FILLER                  PIC X       VALUE SPACE.                 
110300     03  W001-DET15-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
110400     EJECT                                                                
110500 01  W001-DETALJRAD-16.                                                   
110600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
110700     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE G'.        
110800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
110900     03  W001-DET16-SERVG-BTOA   PIC Z9.9.                                
111000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111100     03  W001-DET16-SERVG-BTOB   PIC Z9.9.                                
111200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111300     03  W001-DET16-SERVG-BTOC   PIC Z9.9.                                
111400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111500     03  W001-DET16-SERVG-BTOD   PIC Z9.9.                                
111600     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111700     03  W001-DET16-SERVG-BTOE   PIC Z9.9.                                
111800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111900     03  W001-DET16-SERVG-BTOF   PIC Z9.9.                                
112000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112100     03  W001-DET16-SERVG-BTOG   PIC Z9.9.                                
112200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112300     03  W001-DET16-SERVG-BTOH   PIC Z9.9.                                
112400     03  FILLER                  PIC X(5)    VALUE SPACE.                 
112500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
112600     03  W001-DET16-TOT          PIC Z9.9.                                
112700     03  FILLER                  PIC X       VALUE SPACE.                 
112800     03  W001-DET16-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
112900     EJECT                                                                
113000 01  W001-DETALJRAD-17.                                                   
113100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
113200     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE N'.        
113300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
113400     03  W001-DET17-SERVG-NTOA   PIC Z9.9.                                
113500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113600     03  W001-DET17-SERVG-NTOB   PIC Z9.9.                                
113700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113800     03  W001-DET17-SERVG-NTOC   PIC Z9.9.                                
113900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114000     03  W001-DET17-SERVG-NTOD   PIC Z9.9.                                
114100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114200     03  W001-DET17-SERVG-NTOE   PIC Z9.9.                                
114300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114400     03  W001-DET17-SERVG-NTOF   PIC Z9.9.                                
114500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114600     03  W001-DET17-SERVG-NTOG   PIC Z9.9.                                
114700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114800     03  W001-DET17-SERVG-NTOH   PIC Z9.9.                                
114900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
115000     03  FILLER                  PIC X(11)   VALUE SPACE.                 
115100     03  W001-DET17-TOT          PIC Z9.9.                                
115200     03  FILLER                  PIC X       VALUE SPACE.                 
115300     03  W001-DET17-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
115400     EJECT                                                                
115500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
115600*                                                                         
115700     EJECT                                                                
115800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
115900     SKIP3                                                                
116000 01  NYCKLAR-TILL-DLI.                                                    
116100     03  W-IDDC-B6-X.                                                     
116200         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
116300     SKIP2                                                                
116400*    --- STATUS-KOD FRÅN IMS                                              
116500 01  STATUS-WS                   PIC XX.                                  
116600     88  SEGMENT-FINNS                       VALUE '  '.                  
116700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
116800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
116900     SKIP2                                                                
117000 01  GODK-STATUSKODER.                                                    
117100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
117200     SKIP3                                                                
117300 01  SSA1                        PIC X(64).                               
117400 01  SSA2                        PIC X(64).                               
117500     EJECT                                                                
117600*    --- IMS FUNKTIONSKODER                                               
117700*01  -COPY W0003                                                          
117800     EJECT                                                                
117900*    ---  DLI INPUT-OUTPUT AREA                                           
118000 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
118100 01   DLI-IO-AREA-B6      PIC X(900).                                     
118200 01   DLI-IO-AREA-B601    REDEFINES DLI-IO-AREA-B6.                       
118300*     03  -COPY WDB601                                                    
118400     EJECT                                                                
118500 01   DLI-IO-AREA-B616    REDEFINES DLI-IO-AREA-B6.                       
118600*     03  -COPY WDB616                                                    
118700     EJECT                                                                
118800 LINKAGE SECTION.                                                         
118900                                                                          
119000*01  -COPY W0008      -PRE WDB6-                                          
119100     05  FILLER                  PIC X.                                   
119200 PROCEDURE DIVISION USING WDB6-PCB.                                       
119300                                                                          
119400     PERFORM A-INIT                                                       
119500     PERFORM B-SKAPA-LISTA                                                
119600     PERFORM C-SKRIV-LISTA                                                
119700     PERFORM Z-FINIT                                                      
119800                                                                          
119900     MOVE ZERO TO RETURN-CODE                                             
120000     GOBACK                                                               
120100     .                                                                    
120200     EJECT                                                                
120300 A-INIT SECTION.                                                          
120400                                                                          
120500     OPEN INPUT  W23195                                                   
120600     OPEN OUTPUT W23197-001                                               
120700                                                                          
120800     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
120900     MOVE D-AAR    TO DAGENS-AAR                                          
121000                      D-VECKA-AAR                                         
121100     MOVE D-MAANAD TO DAGENS-MAANAD                                       
121200     MOVE D-VECKA  TO D-VECKA-VECKA                                       
121300     MOVE D-DAG    TO DAGENS-DAG                                          
121400     MOVE DAGENS-DATUM TO W001-DATUM                                      
121500     MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                              
121600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
121700                                                                          
121800     MOVE 'NDC USA   ' TO W001-AKTUELLT-IDDC                              
121900     MOVE 'W23197-001' TO W001-LISTNR                                     
122000                          W001-LISTID                                     
122100                                                                          
122200     PERFORM AA-LADDA-DC-TABELL                                           
122300     .                                                                    
122400     EJECT                                                                
122500 AA-LADDA-DC-TABELL SECTION.                                              
122600                                                                          
122700                                                                          
122800     INITIALIZE WS-DC-TABELL                                              
122900     MOVE 1 TO IDDC-IX                                                    
123000     PERFORM IMS-GET-WDB6                                                 
123100                                                                          
123200     PERFORM UNTIL SEGMENT-SLUT                                           
123300                OR IDDC-IX > IDDC-IX-MAX                                  
123400                                                                          
123500        IF WDB6-SEG-NAME-FB = 'WDB601  '                                  
123600           MOVE DCS-IDDC       TO W-IDDC                                  
123700        END-IF                                                            
123800                                                                          
123900        IF WDB6-SEG-NAME-FB = 'WDB616  '                                  
124000           MOVE W-IDDC           TO WS-IDDC-B601   (IDDC-IX)              
124100           MOVE REF-IDDC-REF     TO WS-IDDC-B616   (IDDC-IX)              
124200           MOVE REF-KVDLTID-TOT  TO WS-KVDLTID-TOT (IDDC-IX)              
124300           ADD 1 TO IDDC-IX                                               
124400        END-IF                                                            
124500                                                                          
124600        PERFORM IMS-GET-WDB6                                              
124700     END-PERFORM                                                          
124800                                                                          
124900     .                                                                    
125000     EJECT                                                                
125100 B-SKAPA-LISTA SECTION.                                                   
125200                                                                          
125300     PERFORM BA-NOLLSTALL                                                 
125400                                                                          
125500     MOVE WC-CDC-SE    TO W-IDDC-SEND                                     
125600     PERFORM S01-LAS-W23195                                               
125700     PERFORM UNTIL END-OF-W23195                                          
125800        MOVE IN-IDDC TO WS-IDDC                                           
125900        IF NDC-US                                                         
126000           IF IN-IDDC NOT = W-IDDC-REC                                    
126100              MOVE IN-IDDC TO W-IDDC-REC                                  
126200              PERFORM S10-HITTA-KVDLTID                                   
126300           END-IF                                                         
126400           PERFORM BB-SKAPA-TABELLER                                      
126500        END-IF                                                            
126600        PERFORM S01-LAS-W23195                                            
126700     END-PERFORM                                                          
126800                                                                          
126900     PERFORM BC-SUMMERA                                                   
127000     .                                                                    
127100     EJECT                                                                
127200 BA-NOLLSTALL SECTION.                                                    
127300******************************************************************        
127400*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
127500*  FREKVENSKLASS                                                 *        
127600*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
127700*     FREKV-KLASSER  = A B C D E F G                             *        
127800*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
127900*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
128000*                 TOTAL-SUMMERING                                *        
128100* ****************************************************************        
128200                                                                          
128300******* NOLLSTÄLLNING AV 72 'RUTOR' PER PRISKLASS/FREKVKLASS              
128400                                                                          
128500     MOVE +1  TO ART-IX                                                   
128600     MOVE +72 TO ART-IX-MAX                                               
128700     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
128800        MOVE ZERO TO     ART-KVANT-AKT(ART-IX)                            
128900                         ART-KVANT-PAS(ART-IX)                            
129000                         ART-PROC-KVANT-A(ART-IX)                         
129100                         ART-PROC-KVANT-P(ART-IX)                         
129200                         ART-KVDISP-AKT(ART-IX)                           
129300                         ART-PROC-KVDISP-A(ART-IX)                        
129400                         ART-KVDISP-PAS(ART-IX)                           
129500                         ART-PROC-KVDISP-P(ART-IX)                        
129600                         ART-LS-AKT(ART-IX)                               
129700                         ART-PROC-LS-A(ART-IX)                            
129800                         ART-LS-PAS(ART-IX)                               
129900                         ART-PROC-LS-P(ART-IX)                            
130000                         ART-AK-AKT(ART-IX)                               
130100                         ART-PROC-AK-A(ART-IX)                            
130200                         ART-AK-PAS(ART-IX)                               
130300                         ART-PROC-AK-P(ART-IX)                            
130400                         ART-OLAGER(ART-IX)                               
130500                         ART-PROC-OLAGER(ART-IX)                          
130600                         ART-SLAGER(ART-IX)                               
130700                         ART-PROC-SLAGER(ART-IX)                          
130800                         ART-MLAGER(ART-IX)                               
130900                         ART-PROC-MLAGER(ART-IX)                          
131000                         ART-KVOT(ART-IX)                                 
131100                         ART-PROC-KVOT(ART-IX)                            
131200                         ART-SPLIT(ART-IX)                                
131300                         ART-OMSHAST-DISP(ART-IX)                         
131400                         ART-OMSHAST-PROC-D(ART-IX)                       
131500                         ART-OMSHAST-LS(ART-IX)                           
131600                         ART-OMSHAST-PROC-LS(ART-IX)                      
131700                         ART-SERVG-BTO(ART-IX)                            
131800                         ART-SERVG-NTO(ART-IX)                            
131900****************                                                          
132000                         WS-ART-SLAGER(ART-IX)                            
132100                         WS-ART-OLAGER(ART-IX)                            
132200                         WS-ART-MLAGER(ART-IX)                            
132300                         WS-ART-KVLS-AKT(ART-IX)                          
132400                         WS-ART-KVLS-PAS(ART-IX)                          
132500                         WS-ART-LS-AKT(ART-IX)                            
132600                         WS-ART-LS-PAS(ART-IX)                            
132700                         WS-ART-LS-PR-AKT(ART-IX)                         
132800                         WS-ART-LS-PR-PAS(ART-IX)                         
132900                         WS-ART-KVDISP-AKT(ART-IX)                        
133000                         WS-ART-KVDISP-PAS(ART-IX)                        
133100                         WS-ART-KVDISP-PR-AKT(ART-IX)                     
133200                         WS-ART-KVDISP-PR-PAS(ART-IX)                     
133300                         WS-ART-KVOKS-AKT(ART-IX)                         
133400                         WS-ART-KVOKS-PAS(ART-IX)                         
133500                         WS-ART-OK-PR-AKT(ART-IX)                         
133600                         WS-ART-OK-PR-PAS(ART-IX)                         
133700                         WS-ART-KVAKS-AKT(ART-IX)                         
133800                         WS-ART-KVAKS-PAS(ART-IX)                         
133900                         WS-ART-AK-PR-AKT(ART-IX)                         
134000                         WS-ART-AK-PR-PAS(ART-IX)                         
134100                         WS-ART-KVOI(ART-IX)                              
134200                         WS-ART-KVOI-AKT(ART-IX)                          
134300                         WS-ART-KVOI-PAS(ART-IX)                          
134400                         WS-ART-KVOI-TEO(ART-IX)                          
134500                         WS-ART-KVOI-SAK(ART-IX)                          
134600                         WS-ART-KVOI-CDC-AKT(ART-IX)                      
134700                         WS-ART-KVOI-CDC-PAS(ART-IX)                      
134800                         WS-ART-KVOI-CDC-TEO(ART-IX)                      
134900                         WS-ART-KVOI-CDC-SAK(ART-IX)                      
135000                         WS-ART-SUINKORD(ART-IX)                          
135100                         WS-ART-SUFYSAVP(ART-IX)                          
135200                         WS-ART-SUAVBRP(ART-IX)                           
135300                         WS-ART-SULAGERB(ART-IX)                          
135400                         WS-ART-SUSORTB(ART-IX)                           
135500                                                                          
135600        ADD +1 TO ART-IX                                                  
135700     END-PERFORM                                                          
135800                                                                          
135900******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
136000*******                          OBEROENDE AV FREKVENSKLASS               
136100                                                                          
136200     MOVE +1 TO PSUM-IX                                                   
136300     MOVE +9 TO PSUM-IX-MAX                                               
136400     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
136500        MOVE ZERO TO     PSUM-KVANT-AKT(PSUM-IX)                          
136600                         PSUM-KVANT-PAS(PSUM-IX)                          
136700                         PSUM-PROC-KVANT-A(PSUM-IX)                       
136800                         PSUM-PROC-KVANT-P(PSUM-IX)                       
136900                         PSUM-KVDISP-AKT(PSUM-IX)                         
137000                         PSUM-PROC-KVDISP-A(PSUM-IX)                      
137100                         PSUM-KVDISP-PAS(PSUM-IX)                         
137200                         PSUM-PROC-KVDISP-P(PSUM-IX)                      
137300                         PSUM-LS-AKT(PSUM-IX)                             
137400                         PSUM-PROC-LS-A(PSUM-IX)                          
137500                         PSUM-LS-PAS(PSUM-IX)                             
137600                         PSUM-PROC-LS-P(PSUM-IX)                          
137700                         PSUM-AK-AKT(PSUM-IX)                             
137800                         PSUM-PROC-AK-A(PSUM-IX)                          
137900                         PSUM-AK-PAS(PSUM-IX)                             
138000                         PSUM-PROC-AK-P(PSUM-IX)                          
138100                         PSUM-OLAGER(PSUM-IX)                             
138200                         PSUM-PROC-OLAGER(PSUM-IX)                        
138300                         PSUM-SLAGER(PSUM-IX)                             
138400                         PSUM-PROC-SLAGER(PSUM-IX)                        
138500                         PSUM-MLAGER(PSUM-IX)                             
138600                         PSUM-PROC-MLAGER(PSUM-IX)                        
138700                         PSUM-KVOT(PSUM-IX)                               
138800                         PSUM-PROC-KVOT(PSUM-IX)                          
138900                         PSUM-SPLIT(PSUM-IX)                              
139000                         PSUM-OMSHAST-DISP(PSUM-IX)                       
139100                         PSUM-OMSHAST-PROC-D(PSUM-IX)                     
139200                         PSUM-OMSHAST-LS(PSUM-IX)                         
139300                         PSUM-OMSHAST-PROC-LS(PSUM-IX)                    
139400                         PSUM-SERVG-BTO(PSUM-IX)                          
139500                         PSUM-SERVG-NTO(PSUM-IX)                          
139600*************                                                             
139700                         WS-PSUM-SLAGER(PSUM-IX)                          
139800                         WS-PSUM-OLAGER(PSUM-IX)                          
139900                         WS-PSUM-MLAGER(PSUM-IX)                          
140000                         WS-PSUM-LS-AKT(PSUM-IX)                          
140100                         WS-PSUM-LS-PAS(PSUM-IX)                          
140200                         WS-PSUM-LS-PR-AKT(PSUM-IX)                       
140300                         WS-PSUM-LS-PR-PAS(PSUM-IX)                       
140400                         WS-PSUM-KVDISP-AKT(PSUM-IX)                      
140500                         WS-PSUM-KVDISP-PAS(PSUM-IX)                      
140600                         WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                   
140700                         WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                   
140800                         WS-PSUM-KVOKS-AKT(PSUM-IX)                       
140900                         WS-PSUM-KVOKS-PAS(PSUM-IX)                       
141000                         WS-PSUM-OK-PR-AKT(PSUM-IX)                       
141100                         WS-PSUM-OK-PR-PAS(PSUM-IX)                       
141200                         WS-PSUM-KVAKS-AKT(PSUM-IX)                       
141300                         WS-PSUM-KVAKS-PAS(PSUM-IX)                       
141400                         WS-PSUM-AK-PR-AKT(PSUM-IX)                       
141500                         WS-PSUM-AK-PR-PAS(PSUM-IX)                       
141600                         WS-PSUM-KVOI(PSUM-IX)                            
141700                         WS-PSUM-KVOI-AKT(PSUM-IX)                        
141800                         WS-PSUM-KVOI-PAS(PSUM-IX)                        
141900                         WS-PSUM-KVOI-TEO(PSUM-IX)                        
142000                         WS-PSUM-KVOI-SAK(PSUM-IX)                        
142100                         WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                    
142200                         WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                    
142300                         WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                    
142400                         WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                    
142500                         WS-PSUM-SUINKORD(PSUM-IX)                        
142600                         WS-PSUM-SUFYSAVP(PSUM-IX)                        
142700                         WS-PSUM-SUAVBRP(PSUM-IX)                         
142800                         WS-PSUM-SULAGERB(PSUM-IX)                        
142900                         WS-PSUM-SUSORTB(PSUM-IX)                         
143000                                                                          
143100        ADD +1 TO PSUM-IX                                                 
143200     END-PERFORM                                                          
143300                                                                          
143400******* NOLLSTÄLLNING AV 7 'RUTOR' TOTALSUMMA PER FREKVENSKLASS           
143500*******                            OBEROENDE AV PRISKLASS                 
143600                                                                          
143700     MOVE +1 TO FSUM-IX                                                   
143800     MOVE +8 TO FSUM-IX-MAX                                               
143900     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
144000        MOVE ZERO TO     FSUM-KVANT-AKT(FSUM-IX)                          
144100                         FSUM-KVANT-PAS(FSUM-IX)                          
144200                         FSUM-PROC-KVANT-A(FSUM-IX)                       
144300                         FSUM-PROC-KVANT-P(FSUM-IX)                       
144400                         FSUM-KVDISP-AKT(FSUM-IX)                         
144500                         FSUM-PROC-KVDISP-A(FSUM-IX)                      
144600                         FSUM-KVDISP-PAS(FSUM-IX)                         
144700                         FSUM-PROC-KVDISP-P(FSUM-IX)                      
144800                         FSUM-LS-AKT(FSUM-IX)                             
144900                         FSUM-PROC-LS-A(FSUM-IX)                          
145000                         FSUM-LS-PAS(FSUM-IX)                             
145100                         FSUM-PROC-LS-P(FSUM-IX)                          
145200                         FSUM-AK-AKT(FSUM-IX)                             
145300                         FSUM-PROC-AK-A(FSUM-IX)                          
145400                         FSUM-AK-PAS(FSUM-IX)                             
145500                         FSUM-PROC-AK-P(FSUM-IX)                          
145600                         FSUM-OLAGER(FSUM-IX)                             
145700                         FSUM-PROC-OLAGER(FSUM-IX)                        
145800                         FSUM-SLAGER(FSUM-IX)                             
145900                         FSUM-PROC-SLAGER(FSUM-IX)                        
146000                         FSUM-MLAGER(FSUM-IX)                             
146100                         FSUM-PROC-MLAGER(FSUM-IX)                        
146200                         FSUM-KVOT(FSUM-IX)                               
146300                         FSUM-PROC-KVOT(FSUM-IX)                          
146400                         FSUM-SPLIT(FSUM-IX)                              
146500                         FSUM-OMSHAST-DISP(FSUM-IX)                       
146600                         FSUM-OMSHAST-PROC-D(FSUM-IX)                     
146700                         FSUM-OMSHAST-LS(FSUM-IX)                         
146800                         FSUM-OMSHAST-PROC-LS(FSUM-IX)                    
146900                         FSUM-SERVG-BTO(FSUM-IX)                          
147000                         FSUM-SERVG-NTO(FSUM-IX)                          
147100*****************                                                         
147200                         WS-FSUM-SLAGER(FSUM-IX)                          
147300                         WS-FSUM-OLAGER(FSUM-IX)                          
147400                         WS-FSUM-MLAGER(FSUM-IX)                          
147500                         WS-FSUM-LS-AKT(FSUM-IX)                          
147600                         WS-FSUM-LS-PAS(FSUM-IX)                          
147700                         WS-FSUM-LS-PR-AKT(FSUM-IX)                       
147800                         WS-FSUM-LS-PR-PAS(FSUM-IX)                       
147900                         WS-FSUM-KVDISP-AKT(FSUM-IX)                      
148000                         WS-FSUM-KVDISP-PAS(FSUM-IX)                      
148100                         WS-FSUM-KVDISP-PR-AKT(FSUM-IX)                   
148200                         WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                   
148300                         WS-FSUM-KVOKS-AKT(FSUM-IX)                       
148400                         WS-FSUM-KVOKS-PAS(FSUM-IX)                       
148500                         WS-FSUM-OK-PR-AKT(FSUM-IX)                       
148600                         WS-FSUM-OK-PR-PAS(FSUM-IX)                       
148700                         WS-FSUM-KVAKS-AKT(FSUM-IX)                       
148800                         WS-FSUM-KVAKS-PAS(FSUM-IX)                       
148900                         WS-FSUM-AK-PR-AKT(FSUM-IX)                       
149000                         WS-FSUM-AK-PR-PAS(FSUM-IX)                       
149100                         WS-FSUM-KVOI(FSUM-IX)                            
149200                         WS-FSUM-KVOI-AKT(FSUM-IX)                        
149300                         WS-FSUM-KVOI-PAS(FSUM-IX)                        
149400                         WS-FSUM-KVOI-TEO(FSUM-IX)                        
149500                         WS-FSUM-KVOI-SAK(FSUM-IX)                        
149600                         WS-FSUM-KVOI-CDC-AKT(FSUM-IX)                    
149700                         WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                    
149800                         WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                    
149900                         WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                    
150000                         WS-FSUM-SUINKORD(FSUM-IX)                        
150100                         WS-FSUM-SUFYSAVP(FSUM-IX)                        
150200                         WS-FSUM-SUAVBRP(FSUM-IX)                         
150300                         WS-FSUM-SULAGERB(FSUM-IX)                        
150400                         WS-FSUM-SUSORTB(FSUM-IX)                         
150500                                                                          
150600        ADD +1 TO FSUM-IX                                                 
150700     END-PERFORM                                                          
150800                                                                          
150900******* NOLLSTÄLLNING AV TOTALRUTA                                        
151000                                                                          
151100     MOVE ZERO TO     TOT-KVANT-AKT                                       
151200                      TOT-KVANT-PAS                                       
151300                      TOT-KVDISP-AKT                                      
151400                      TOT-KVDISP-PAS                                      
151500                      TOT-LS-AKT                                          
151600                      TOT-LS-PAS                                          
151700                      TOT-AK-AKT                                          
151800                      TOT-AK-PAS                                          
151900                      TOT-SLAGER                                          
152000                      TOT-MLAGER                                          
152100                      TOT-OLAGER                                          
152200                      TOT-KVOT                                            
152300                      TOT-PROC-OLAGER                                     
152400                      TOT-PROC-MLAGER                                     
152500                      TOT-PROC-SLAGER                                     
152600                      TOT-SPLIT                                           
152700                      TOT-OMSHAST-DISP                                    
152800                      TOT-OMSHAST-LS                                      
152900                      TOT-SERVG-BTO                                       
153000                      TOT-SERVG-NTO                                       
153100************                                                              
153200                      WS-TOT-SLAGER                                       
153300                      WS-TOT-OLAGER                                       
153400                      WS-TOT-MLAGER                                       
153500                      WS-TOT-LS-AKT                                       
153600                      WS-TOT-LS-PAS                                       
153700                      WS-TOT-LS-PR-AKT                                    
153800                      WS-TOT-LS-PR-PAS                                    
153900                      WS-TOT-KVDISP-AKT                                   
154000                      WS-TOT-KVDISP-PAS                                   
154100                      WS-TOT-KVDISP-PR-AKT                                
154200                      WS-TOT-KVDISP-PR-PAS                                
154300                      WS-TOT-KVOKS-AKT                                    
154400                      WS-TOT-KVOKS-PAS                                    
154500                      WS-TOT-OK-PR-AKT                                    
154600                      WS-TOT-OK-PR-PAS                                    
154700                      WS-TOT-KVAKS-AKT                                    
154800                      WS-TOT-KVAKS-PAS                                    
154900                      WS-TOT-AK-PR-AKT                                    
155000                      WS-TOT-AK-PR-PAS                                    
155100                      WS-TOT-KVOI                                         
155200                      WS-TOT-KVOI-AKT                                     
155300                      WS-TOT-KVOI-PAS                                     
155400                      WS-TOT-KVOI-TEO                                     
155500                      WS-TOT-KVOI-SAK                                     
155600                      WS-TOT-KVOI-CDC-AKT                                 
155700                      WS-TOT-KVOI-CDC-PAS                                 
155800                      WS-TOT-KVOI-CDC-TEO                                 
155900                      WS-TOT-KVOI-CDC-SAK                                 
156000                      WS-TOT-SUINKORD                                     
156100                      WS-TOT-SUFYSAVP                                     
156200                      WS-TOT-SUAVBRP                                      
156300                      WS-TOT-SULAGERB                                     
156400                      WS-TOT-SUSORTB                                      
156500     .                                                                    
156600     EJECT                                                                
156700 BB-SKAPA-TABELLER SECTION.                                               
156800                                                                          
156900     PERFORM BBA-SAETT-ART-IX                                             
157000     IF SW-ARTIKEL-SAKNAS-WDK7 = JA                                       
157100        PERFORM BBC-UPPDAT-SAKN-ART                                       
157200     END-IF                                                               
157300     IF ART-IX > ZERO                                                     
157400        PERFORM BBB-UPPDATERA-TABELLER                                    
157500     END-IF                                                               
157600     .                                                                    
157700     EJECT                                                                
157800 BBA-SAETT-ART-IX SECTION.                                                
157900******************************************************************        
158000* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
158100******************************************************************        
158200                                                                          
158300     MOVE NEJ TO SW-ARTIKEL-SAKNAS-WDK7                                   
158400     EVALUATE TRUE                                                        
158500     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
158600          MOVE +1 TO ART-IX                                               
158700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
158800          MOVE +2 TO ART-IX                                               
158900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
159000          MOVE +3 TO ART-IX                                               
159100     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
159200          MOVE +4 TO ART-IX                                               
159300     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
159400          MOVE +5 TO ART-IX                                               
159500     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
159600          MOVE +6 TO ART-IX                                               
159700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
159800          MOVE +7 TO ART-IX                                               
159900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
160000          MOVE +8 TO ART-IX                                               
160100                                                                          
160200     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
160300          MOVE +9 TO ART-IX                                               
160400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
160500          MOVE +10 TO ART-IX                                              
160600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
160700          MOVE +11 TO ART-IX                                              
160800     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
160900          MOVE +12 TO ART-IX                                              
161000     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
161100          MOVE +13 TO ART-IX                                              
161200     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
161300          MOVE +14 TO ART-IX                                              
161400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
161500          MOVE +15 TO ART-IX                                              
161600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
161700          MOVE +16 TO ART-IX                                              
161800                                                                          
161900     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
162000          MOVE +17 TO ART-IX                                              
162100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
162200          MOVE +18 TO ART-IX                                              
162300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
162400          MOVE +19 TO ART-IX                                              
162500     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
162600          MOVE +20 TO ART-IX                                              
162700     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
162800          MOVE +21 TO ART-IX                                              
162900     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
163000          MOVE +22 TO ART-IX                                              
163100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
163200          MOVE +23 TO ART-IX                                              
163300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
163400          MOVE +24 TO ART-IX                                              
163500                                                                          
163600     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
163700          MOVE +25 TO ART-IX                                              
163800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
163900          MOVE +26 TO ART-IX                                              
164000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
164100          MOVE +27 TO ART-IX                                              
164200     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
164300          MOVE +28 TO ART-IX                                              
164400     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
164500          MOVE +29 TO ART-IX                                              
164600     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
164700          MOVE +30 TO ART-IX                                              
164800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
164900          MOVE +31 TO ART-IX                                              
165000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
165100          MOVE +32 TO ART-IX                                              
165200                                                                          
165300     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
165400          MOVE +33 TO ART-IX                                              
165500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
165600          MOVE +34 TO ART-IX                                              
165700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
165800          MOVE +35 TO ART-IX                                              
165900     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
166000          MOVE +36 TO ART-IX                                              
166100     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
166200          MOVE +37 TO ART-IX                                              
166300     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
166400          MOVE +38 TO ART-IX                                              
166500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
166600          MOVE +39 TO ART-IX                                              
166700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
166800          MOVE +40 TO ART-IX                                              
166900                                                                          
167000     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
167100          MOVE +41 TO ART-IX                                              
167200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
167300          MOVE +42 TO ART-IX                                              
167400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
167500          MOVE +43 TO ART-IX                                              
167600     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
167700          MOVE +44 TO ART-IX                                              
167800     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
167900          MOVE +45 TO ART-IX                                              
168000     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
168100          MOVE +46 TO ART-IX                                              
168200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
168300          MOVE +47 TO ART-IX                                              
168400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
168500          MOVE +48 TO ART-IX                                              
168600                                                                          
168700     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
168800          MOVE +49 TO ART-IX                                              
168900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
169000          MOVE +50 TO ART-IX                                              
169100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
169200          MOVE +51 TO ART-IX                                              
169300     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
169400          MOVE +52 TO ART-IX                                              
169500     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
169600          MOVE +53 TO ART-IX                                              
169700     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
169800          MOVE +54 TO ART-IX                                              
169900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
170000          MOVE +55 TO ART-IX                                              
170100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
170200          MOVE +56 TO ART-IX                                              
170300                                                                          
170400     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
170500          MOVE +57 TO ART-IX                                              
170600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
170700          MOVE +58 TO ART-IX                                              
170800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
170900          MOVE +59 TO ART-IX                                              
171000     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
171100          MOVE +60 TO ART-IX                                              
171200     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
171300          MOVE +61 TO ART-IX                                              
171400     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
171500          MOVE +62 TO ART-IX                                              
171600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
171700          MOVE +63 TO ART-IX                                              
171800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
171900          MOVE +64 TO ART-IX                                              
172000                                                                          
172100     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
172200          MOVE +65 TO ART-IX                                              
172300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
172400          MOVE +66 TO ART-IX                                              
172500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
172600          MOVE +67 TO ART-IX                                              
172700     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
172800          MOVE +68 TO ART-IX                                              
172900     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
173000          MOVE +69 TO ART-IX                                              
173100     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
173200          MOVE +70 TO ART-IX                                              
173300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
173400          MOVE +71 TO ART-IX                                              
173500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
173600          MOVE +72 TO ART-IX                                              
173700     WHEN OTHER                                                           
173800          MOVE ZERO TO ART-IX                                             
173900          IF IN-KDPRISKL = SPACE AND IN-KDFREKKL = SPACE                  
174000             MOVE JA TO SW-ARTIKEL-SAKNAS-WDK7                            
174100          END-IF                                                          
174200     END-EVALUATE                                                         
174300     .                                                                    
174400     EJECT                                                                
174500 BBB-UPPDATERA-TABELLER SECTION.                                          
174600                                                                          
174700*********  ANTAL ARTIKLAR                                                 
174800     IF IN-KDREFSTA = 'A'                                                 
174900        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
175000     ELSE                                                                 
175100        IF IN-KDREFSTA = 'P'                                              
175200           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
175300        END-IF                                                            
175400     END-IF                                                               
175500                                                                          
175600*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
175700     IF IN-KDREFSTA = 'A'                                                 
175800        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
175900        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
176000        COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                           
176100        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                        
176200        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
176300                                                                          
176400        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                         
176500        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
176600                                                                          
176700        COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                          
176800        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
176900                                                                          
177000        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
177100        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
177200        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                         
177300        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
177400     ELSE                                                                 
177500        IF IN-KDREFSTA = 'P'                                              
177600           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
177700           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
177800           COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                        
177900           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                     
178000           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
178100                                                                          
178200           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                      
178300           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
178400                                                                          
178500           COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                       
178600           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
178700                                                                          
178800           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
178900           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
179000           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                      
179100           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
179200        END-IF                                                            
179300     END-IF                                                               
179400                                                                          
179500*********  OMSÄTTNINGSHASTIGHET                                           
179600     MOVE +1 TO KVOI-IX                                                   
179700     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
179800     PERFORM UNTIL KVOI-IX > 53                                           
179900        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
180000        ADD +1 TO KVOI-IX                                                 
180100     END-PERFORM                                                          
180200                                                                          
180300     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRARTSTD                      
180400     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
180500                                                                          
180600*********  SÄKERHETSLAGER/ARTIKEL                                         
180700     IF IN-KDREFSTA = 'A'                                                 
180800        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRARTSTD                      
180900        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
181000     END-IF                                                               
181100                                                                          
181200*********  ÖVERLAGER/ARTIKEL                                              
181300     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
181400     IF WS-KVDISP > IN-KVREFOVL                                           
181500        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
181600        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRARTSTD                        
181700        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
181800     END-IF                                                               
181900                                                                          
182000*********  MEDELLAGER/ARTIKEL                                             
182100     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
182200     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
182300     COMPUTE WS-LT-BEHOV-SDC-NORM = WS-KVDLTID-TOT (IDDC-IX)              
182400                                      * WS-KVPB-DAG-SDC-NORM              
182500     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
182600                        + (IN-KVREFBER / 2)                               
182700     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRARTSTD                           
182800     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
182900                                                                          
183000*********  SERVICEGRAD OCH SPLITFAKTOR ORDERRADER/ARTIKEL                 
183100                                                                          
183200     MOVE +1 TO KVOI-IX                                                   
183300     MOVE NEJ TO SW-KVOI-TRAFF                                            
183400     PERFORM UNTIL KVOI-IX > 5                                            
183500        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
183600           MOVE JA TO SW-KVOI-TRAFF                                       
183700           IF IN-KDREFSTA = 'A'                                           
183800              ADD IN-KVOT-INNEV(KVOI-IX)                                  
183900                                TO WS-ART-KVOI-AKT(ART-IX)                
184000              ADD IN-KVOT-CDC-INNEV(KVOI-IX)                              
184100                                TO WS-ART-KVOI-CDC-AKT(ART-IX)            
184200           ELSE                                                           
184300              IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                     
184400                 ADD IN-KVOT-INNEV(KVOI-IX)                               
184500                                TO WS-ART-KVOI-TEO(ART-IX)                
184600                 ADD IN-KVOT-CDC-INNEV(KVOI-IX)                           
184700                                TO WS-ART-KVOI-CDC-TEO(ART-IX)            
184800              ELSE                                                        
184900                 IF IN-KDREFSTA = 'P'                                     
185000                    ADD IN-KVOT-INNEV(KVOI-IX)                            
185100                                   TO WS-ART-KVOI-PAS(ART-IX)             
185200                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
185300                                   TO WS-ART-KVOI-CDC-PAS(ART-IX)         
185400                 ELSE                                                     
185500                    ADD IN-KVOT-INNEV(KVOI-IX)                            
185600                                   TO WS-ART-KVOI-SAK(ART-IX)             
185700                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
185800                                   TO WS-ART-KVOI-CDC-SAK(ART-IX)         
185900                 END-IF                                                   
186000              END-IF                                                      
186100           END-IF                                                         
186200        END-IF                                                            
186300        ADD +1 TO KVOI-IX                                                 
186400     END-PERFORM                                                          
186500                                                                          
186600     IF SW-KVOI-TRAFF = NEJ                                               
186700        MOVE D-VECKA-VECKA TO KVOI-IX                                     
186800        IF IN-KDREFSTA = 'A'                                              
186900           ADD IN-KVOT-RULL(KVOI-IX)                                      
187000                              TO WS-ART-KVOI-AKT(ART-IX)                  
187100           ADD IN-KVOT-CDC-RULL(KVOI-IX)                                  
187200                              TO WS-ART-KVOI-CDC-AKT(ART-IX)              
187300        ELSE                                                              
187400           IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                        
187500              ADD IN-KVOT-RULL(KVOI-IX)                                   
187600                              TO WS-ART-KVOI-TEO(ART-IX)                  
187700              ADD IN-KVOT-CDC-RULL(KVOI-IX)                               
187800                              TO WS-ART-KVOI-CDC-TEO(ART-IX)              
187900           ELSE                                                           
188000              IF IN-KDREFSTA = 'P'                                        
188100                 ADD IN-KVOT-RULL(KVOI-IX)                                
188200                                 TO WS-ART-KVOI-PAS(ART-IX)               
188300                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
188400                                 TO WS-ART-KVOI-CDC-PAS(ART-IX)           
188500              ELSE                                                        
188600                 ADD IN-KVOT-RULL(KVOI-IX)                                
188700                                 TO WS-ART-KVOI-SAK(ART-IX)               
188800                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
188900                                 TO WS-ART-KVOI-CDC-SAK(ART-IX)           
189000              END-IF                                                      
189100           END-IF                                                         
189200        END-IF                                                            
189300     END-IF                                                               
189400                                                                          
189500*********  SERVICEGRAD OCH SPLITFAKTOR FRÅN SRS                           
189600                                                                          
189700     MOVE IN-SUINKORD    TO WS-FIXAD-SUMMA                                
189800     ADD WS-FIXAD-SUMMA  TO WS-ART-SUINKORD(ART-IX)                       
189900     ADD IN-SUFYSAVP     TO WS-ART-SUFYSAVP(ART-IX)                       
190000     ADD IN-SUAVBRP      TO WS-ART-SUAVBRP (ART-IX)                       
190100     MOVE IN-SULAGERB    TO WS-FIXAD-SUMMA                                
190200     ADD WS-FIXAD-SUMMA  TO WS-ART-SULAGERB(ART-IX)                       
190300     MOVE IN-SUSORTB     TO WS-FIXAD-SUMMA                                
190400     ADD WS-FIXAD-SUMMA  TO WS-ART-SUSORTB (ART-IX)                       
190500     .                                                                    
190600     EJECT                                                                
190700 BBC-UPPDAT-SAKN-ART SECTION.                                             
190800                                                                          
190900     MOVE +1 TO KVOI-IX                                                   
191000     MOVE NEJ TO SW-KVOI-TRAFF                                            
191100     PERFORM UNTIL KVOI-IX > 5                                            
191200        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
191300           MOVE JA TO SW-KVOI-TRAFF                                       
191400           ADD IN-KVOT-INNEV(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7              
191500           ADD IN-KVOT-CDC-INNEV(KVOI-IX)                                 
191600                                    TO WS-KVOT-CDC-SAKNAS-WDK7            
191700        END-IF                                                            
191800        ADD +1 TO KVOI-IX                                                 
191900     END-PERFORM                                                          
192000                                                                          
192100     IF SW-KVOI-TRAFF = NEJ                                               
192200        MOVE D-VECKA-VECKA TO KVOI-IX                                     
192300        ADD IN-KVOT-RULL(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7                  
192400        ADD IN-KVOT-CDC-RULL(KVOI-IX) TO WS-KVOT-CDC-SAKNAS-WDK7          
192500     END-IF                                                               
192600     .                                                                    
192700     EJECT                                                                
192800 BC-SUMMERA SECTION.                                                      
192900                                                                          
193000     PERFORM BCA-SUMMERA-RUTA                                             
193100     PERFORM BCB-SUMMERA-PRISKLASS                                        
193200     PERFORM BCC-SUMMERA-FREKVENSKLASS                                    
193300     PERFORM BCD-SUMMERA-TOTAL                                            
193400     PERFORM BCE-BERAKNINGAR-AV-TOTAL                                     
193500     .                                                                    
193600     EJECT                                                                
193700 BCA-SUMMERA-RUTA SECTION.                                                
193800                                                                          
193900     MOVE +1  TO ART-IX                                                   
194000     MOVE +72 TO ART-IX-MAX                                               
194100     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
194200                                                                          
194300*********  DISP LAGER VÄRDE/RUTA                                          
194400        IF WS-ART-KVDISP-PR-AKT(ART-IX) = ZERO                            
194500           CONTINUE                                                       
194600        ELSE                                                              
194700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
194800                          WS-ART-KVDISP-PR-AKT(ART-IX) / 1000             
194900           MOVE WS-SUMMA-KR TO ART-KVDISP-AKT(ART-IX)                     
195000        END-IF                                                            
195100                                                                          
195200        IF WS-ART-KVDISP-PR-PAS(ART-IX) = ZERO                            
195300           CONTINUE                                                       
195400        ELSE                                                              
195500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
195600                          WS-ART-KVDISP-PR-PAS(ART-IX) / 1000             
195700           MOVE WS-SUMMA-KR TO ART-KVDISP-PAS(ART-IX)                     
195800        END-IF                                                            
195900                                                                          
196000*********  LAGERVÄRDE/RUTA                                                
196100        IF WS-ART-LS-PR-AKT(ART-IX) = ZERO                                
196200           CONTINUE                                                       
196300        ELSE                                                              
196400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
196500                          WS-ART-LS-PR-AKT(ART-IX) / 1000                 
196600           MOVE WS-SUMMA-KR TO ART-LS-AKT(ART-IX)                         
196700        END-IF                                                            
196800                                                                          
196900        IF WS-ART-LS-PR-PAS(ART-IX) = ZERO                                
197000           CONTINUE                                                       
197100        ELSE                                                              
197200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
197300                          WS-ART-LS-PR-PAS(ART-IX) / 1000                 
197400           MOVE WS-SUMMA-KR TO ART-LS-PAS(ART-IX)                         
197500        END-IF                                                            
197600                                                                          
197700*********  AK-VÄRDE/RUTA                                                  
197800        IF WS-ART-AK-PR-AKT(ART-IX) = ZERO                                
197900           CONTINUE                                                       
198000        ELSE                                                              
198100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
198200                          WS-ART-AK-PR-AKT(ART-IX) / 1000                 
198300           MOVE WS-SUMMA-KR TO ART-AK-AKT(ART-IX)                         
198400        END-IF                                                            
198500                                                                          
198600        IF WS-ART-AK-PR-PAS(ART-IX) = ZERO                                
198700           CONTINUE                                                       
198800        ELSE                                                              
198900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
199000                          WS-ART-AK-PR-PAS(ART-IX) / 1000                 
199100           MOVE WS-SUMMA-KR TO ART-AK-PAS(ART-IX)                         
199200        END-IF                                                            
199300                                                                          
199400*********  SÄKERHETSLAGER/RUTA                                            
199500        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
199600           CONTINUE                                                       
199700        ELSE                                                              
199800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
199900                          WS-ART-SLAGER(ART-IX) / 1000                    
200000           MOVE WS-SUMMA-KR TO ART-SLAGER(ART-IX)                         
200100        END-IF                                                            
200200                                                                          
200300*********  ÖVERLAGER/RUTA                                                 
200400        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
200500           CONTINUE                                                       
200600        ELSE                                                              
200700           COMPUTE WS-SUMMA-KR ROUNDED                                    
200800                              = WS-ART-OLAGER(ART-IX) / 1000              
200900           MOVE WS-SUMMA-KR TO ART-OLAGER(ART-IX)                         
201000        END-IF                                                            
201100                                                                          
201200*********  MEDELLAGER/RUTA                                                
201300        IF WS-ART-MLAGER(ART-IX) = ZERO                                   
201400           CONTINUE                                                       
201500        ELSE                                                              
201600           COMPUTE WS-SUMMA-KR =                                          
201700                                WS-ART-MLAGER(ART-IX) / 1000              
201800           ADD WS-SUMMA-KR TO ART-MLAGER(ART-IX)                          
201900        END-IF                                                            
202000                                                                          
202100*********  OMSHASTIGHET/RUTA                                              
202200        COMPUTE WS-SUMMA = WS-ART-KVDISP-PR-AKT(ART-IX) +                 
202300                           WS-ART-KVDISP-PR-PAS(ART-IX)                   
202400        IF WS-SUMMA = ZERO                                                
202500           CONTINUE                                                       
202600        ELSE                                                              
202700           COMPUTE WS-OMSHAST ROUNDED =                                   
202800               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
202900           MOVE WS-OMSHAST TO ART-OMSHAST-DISP(ART-IX)                    
203000        END-IF                                                            
203100                                                                          
203200        COMPUTE WS-SUMMA = WS-ART-LS-PR-AKT(ART-IX) +                     
203300                           WS-ART-AK-PR-AKT(ART-IX) +                     
203400                           WS-ART-LS-PR-PAS(ART-IX) +                     
203500                           WS-ART-AK-PR-PAS(ART-IX)                       
203600        IF WS-SUMMA = ZERO                                                
203700           CONTINUE                                                       
203800        ELSE                                                              
203900           COMPUTE WS-OMSHAST ROUNDED =                                   
204000               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
204100           MOVE WS-OMSHAST TO ART-OMSHAST-LS(ART-IX)                      
204200        END-IF                                                            
204300                                                                          
204400*********  SERVICEGRAD BRUTTO/RUTA                                        
204500        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
204600           MOVE 99.9   TO ART-SERVG-BTO(ART-IX)                           
204700        ELSE                                                              
204800           COMPUTE WS-SERVG ROUNDED =                                     
204900             WS-ART-SUAVBRP(ART-IX) * 100 /                               
205000                          WS-ART-SUINKORD(ART-IX)                         
205100           IF WS-SERVG = 100.0                                            
205200              MOVE 99.9 TO ART-SERVG-BTO(ART-IX)                          
205300           ELSE                                                           
205400              MOVE WS-SERVG TO ART-SERVG-BTO(ART-IX)                      
205500           END-IF                                                         
205600        END-IF                                                            
205700                                                                          
205800*********  SERVICEGRAD NETTO/RUTA                                         
205900        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
206000           MOVE 99.9   TO ART-SERVG-NTO(ART-IX)                           
206100        ELSE                                                              
206200           COMPUTE WS-SERVG ROUNDED =                                     
206300             (WS-ART-SUAVBRP(ART-IX) - WS-ART-SUFYSAVP(ART-IX))           
206400                           * 100 /                                        
206500                          WS-ART-SUINKORD(ART-IX)                         
206600           IF WS-SERVG = 100.0                                            
206700              MOVE 99.9 TO ART-SERVG-NTO(ART-IX)                          
206800           ELSE                                                           
206900              MOVE WS-SERVG TO ART-SERVG-NTO(ART-IX)                      
207000           END-IF                                                         
207100        END-IF                                                            
207200                                                                          
207300*********  SPLITFAKTOR/RUTA                                               
207400        IF (WS-ART-SUINKORD(ART-IX) +                                     
207500            WS-ART-SULAGERB(ART-IX) +                                     
207600            WS-ART-SUSORTB (ART-IX))  = ZERO                              
207700           MOVE 99.9 TO ART-SPLIT(ART-IX)                                 
207800        ELSE                                                              
207900           COMPUTE WS-SERVG ROUNDED = (WS-ART-SUINKORD(ART-IX) +          
208000                         WS-ART-SULAGERB (ART-IX)) * 100                  
208100                        / (WS-ART-SUINKORD(ART-IX) +                      
208200                           WS-ART-SULAGERB(ART-IX) +                      
208300                           WS-ART-SUSORTB(ART-IX))                        
208400           IF WS-SERVG = 100.0                                            
208500              MOVE 99.9 TO ART-SPLIT(ART-IX)                              
208600           ELSE                                                           
208700              MOVE WS-SERVG TO ART-SPLIT(ART-IX)                          
208800           END-IF                                                         
208900        END-IF                                                            
209000*********  ORDERTRÄFFAR/RUTA                                              
209100*       COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
209200*                                   WS-ART-KVOI-PAS(ART-IX) +             
209300*                                   WS-ART-KVOI-TEO(ART-IX) +             
209400*                                   WS-ART-KVOI-SAK(ART-IX)               
209500*       ADD WS-KVOI-TOT-VECKA TO ART-KVOT(ART-IX)                         
209600                                                                          
209700        MOVE WS-ART-SUINKORD(ART-IX) TO ART-KVOT(ART-IX)                  
209800                                                                          
209900        ADD +1  TO ART-IX                                                 
210000     END-PERFORM                                                          
210100     .                                                                    
210200     EJECT                                                                
210300 BCB-SUMMERA-PRISKLASS SECTION.                                           
210400******************************************************************        
210500* SUMMERING PER PRISKLASS                                        *        
210600******************************************************************        
210700                                                                          
210800     MOVE +1 TO PSUM-IX                                                   
210900                ART-IX                                                    
211000     MOVE +8 TO ART-IX-MAX                                                
211100                                                                          
211200     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
211300        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
211400                                                                          
211500          ADD ART-KVANT-AKT(ART-IX) TO PSUM-KVANT-AKT(PSUM-IX)            
211600          ADD ART-KVANT-PAS(ART-IX) TO PSUM-KVANT-PAS(PSUM-IX)            
211700          ADD ART-KVOT(ART-IX)      TO PSUM-KVOT(PSUM-IX)                 
211800          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-PSUM-KVOI-AKT(PSUM-IX)        
211900          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-PSUM-KVOI-PAS(PSUM-IX)        
212000          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-PSUM-KVOI-TEO(PSUM-IX)        
212100          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-PSUM-KVOI-SAK(PSUM-IX)        
212200          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
212300                                  TO WS-PSUM-KVOI-CDC-AKT(PSUM-IX)        
212400          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
212500                                  TO WS-PSUM-KVOI-CDC-PAS(PSUM-IX)        
212600          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
212700                                  TO WS-PSUM-KVOI-CDC-TEO(PSUM-IX)        
212800          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
212900                                  TO WS-PSUM-KVOI-CDC-SAK(PSUM-IX)        
213000          ADD WS-ART-KVOI(ART-IX) TO WS-PSUM-KVOI(PSUM-IX)                
213100          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
213200                             TO WS-PSUM-KVDISP-PR-AKT (PSUM-IX)           
213300          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
213400                             TO WS-PSUM-OK-PR-AKT(PSUM-IX)                
213500          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
213600                             TO WS-PSUM-LS-PR-AKT(PSUM-IX)                
213700          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
213800                             TO WS-PSUM-AK-PR-AKT(PSUM-IX)                
213900          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
214000                             TO WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
214100          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
214200                             TO WS-PSUM-OK-PR-PAS(PSUM-IX)                
214300          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
214400                             TO WS-PSUM-LS-PR-PAS(PSUM-IX)                
214500          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
214600                             TO WS-PSUM-AK-PR-PAS(PSUM-IX)                
214700          ADD WS-ART-OLAGER(ART-IX)   TO WS-PSUM-OLAGER(PSUM-IX)          
214800          ADD WS-ART-MLAGER(ART-IX)   TO WS-PSUM-MLAGER(PSUM-IX)          
214900          ADD WS-ART-SLAGER(ART-IX)   TO WS-PSUM-SLAGER(PSUM-IX)          
215000          ADD WS-ART-SUINKORD(ART-IX) TO WS-PSUM-SUINKORD(PSUM-IX)        
215100          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-PSUM-SUFYSAVP(PSUM-IX)        
215200          ADD WS-ART-SUAVBRP (ART-IX) TO WS-PSUM-SUAVBRP (PSUM-IX)        
215300          ADD WS-ART-SULAGERB(ART-IX) TO WS-PSUM-SULAGERB(PSUM-IX)        
215400          ADD WS-ART-SUSORTB (ART-IX) TO WS-PSUM-SUSORTB (PSUM-IX)        
215500                                                                          
215600          ADD +1 TO ART-IX                                                
215700        END-PERFORM                                                       
215800                                                                          
215900        ADD +1 TO PSUM-IX                                                 
216000        ADD +8 TO ART-IX-MAX                                              
216100     END-PERFORM                                                          
216200                                                                          
216300     MOVE +1 TO PSUM-IX                                                   
216400     MOVE +9 TO PSUM-IX-MAX                                               
216500     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
216600                                                                          
216700*********  DISP LAGER VÄRDE/PRISKLASS                                     
216800        IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) = ZERO                          
216900           CONTINUE                                                       
217000        ELSE                                                              
217100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
217200                     WS-PSUM-KVDISP-PR-AKT(PSUM-IX) / 1000                
217300           MOVE WS-SUMMA-KR TO PSUM-KVDISP-AKT(PSUM-IX)                   
217400        END-IF                                                            
217500                                                                          
217600        IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) = ZERO                          
217700           CONTINUE                                                       
217800        ELSE                                                              
217900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
218000                     WS-PSUM-KVDISP-PR-PAS(PSUM-IX) / 1000                
218100           MOVE WS-SUMMA-KR TO PSUM-KVDISP-PAS(PSUM-IX)                   
218200        END-IF                                                            
218300                                                                          
218400*********  LAGERVÄRDE/PRISKLASS                                           
218500        IF WS-PSUM-LS-PR-AKT(PSUM-IX) = ZERO                              
218600           CONTINUE                                                       
218700        ELSE                                                              
218800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
218900                     WS-PSUM-LS-PR-AKT(PSUM-IX) / 1000                    
219000           MOVE WS-SUMMA-KR TO PSUM-LS-AKT(PSUM-IX)                       
219100        END-IF                                                            
219200                                                                          
219300        IF WS-PSUM-LS-PR-PAS(PSUM-IX) = ZERO                              
219400           CONTINUE                                                       
219500        ELSE                                                              
219600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
219700                     WS-PSUM-LS-PR-PAS(PSUM-IX) / 1000                    
219800           MOVE WS-SUMMA-KR TO PSUM-LS-PAS(PSUM-IX)                       
219900        END-IF                                                            
220000                                                                          
220100*********  AK-VÄRDE/PRISKLASS                                             
220200        IF WS-PSUM-AK-PR-AKT(PSUM-IX) = ZERO                              
220300           CONTINUE                                                       
220400        ELSE                                                              
220500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
220600                     WS-PSUM-AK-PR-AKT(PSUM-IX) / 1000                    
220700           MOVE WS-SUMMA-KR TO PSUM-AK-AKT(PSUM-IX)                       
220800        END-IF                                                            
220900                                                                          
221000        IF WS-PSUM-AK-PR-PAS(PSUM-IX) = ZERO                              
221100           CONTINUE                                                       
221200        ELSE                                                              
221300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
221400                     WS-PSUM-AK-PR-PAS(PSUM-IX) / 1000                    
221500           MOVE WS-SUMMA-KR TO PSUM-AK-PAS(PSUM-IX)                       
221600        END-IF                                                            
221700                                                                          
221800*********  SÄKERHETSLAGER/PRISKLASS                                       
221900        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
222000           CONTINUE                                                       
222100        ELSE                                                              
222200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
222300                          WS-PSUM-SLAGER(PSUM-IX) / 1000                  
222400           MOVE WS-SUMMA-KR TO PSUM-SLAGER(PSUM-IX)                       
222500        END-IF                                                            
222600                                                                          
222700*********  ÖVERLAGER/PRISKLASS                                            
222800        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
222900           CONTINUE                                                       
223000        ELSE                                                              
223100           COMPUTE WS-SUMMA-KR ROUNDED                                    
223200                     = WS-PSUM-OLAGER(PSUM-IX) / 1000                     
223300           MOVE WS-SUMMA-KR TO PSUM-OLAGER(PSUM-IX)                       
223400        END-IF                                                            
223500                                                                          
223600*********  MEDELLAGER/PRISKLASS                                           
223700        IF WS-PSUM-MLAGER(PSUM-IX) = ZERO                                 
223800           CONTINUE                                                       
223900        ELSE                                                              
224000           COMPUTE WS-SUMMA-KR =                                          
224100                       WS-PSUM-MLAGER(PSUM-IX) / 1000                     
224200           ADD WS-SUMMA-KR TO PSUM-MLAGER(PSUM-IX)                        
224300        END-IF                                                            
224400                                                                          
224500*********  SERVICEGRAD BRUTTO/PRISKLASS                                   
224600        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
224700           MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                           
224800        ELSE                                                              
224900           COMPUTE WS-SERVG ROUNDED =                                     
225000             WS-PSUM-SUAVBRP(PSUM-IX) * 100 /                             
225100                          WS-PSUM-SUINKORD(PSUM-IX)                       
225200           IF WS-SERVG = 100.0                                            
225300              MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                        
225400           ELSE                                                           
225500              MOVE WS-SERVG TO PSUM-SERVG-BTO(PSUM-IX)                    
225600           END-IF                                                         
225700        END-IF                                                            
225800                                                                          
225900*********  SERVICEGRAD NETTO/PRISKLASS                                    
226000        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
226100           MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                           
226200        ELSE                                                              
226300           COMPUTE WS-SERVG ROUNDED =                                     
226400            (WS-PSUM-SUAVBRP(PSUM-IX) - WS-PSUM-SUFYSAVP(PSUM-IX))        
226500                           * 100 /                                        
226600                          WS-PSUM-SUINKORD(PSUM-IX)                       
226700           IF WS-SERVG = 100.0                                            
226800              MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                        
226900           ELSE                                                           
227000              MOVE WS-SERVG TO PSUM-SERVG-NTO(PSUM-IX)                    
227100           END-IF                                                         
227200        END-IF                                                            
227300                                                                          
227400*********  SPLITFAKTOR/PRISKLASS                                          
227500        IF (WS-PSUM-SUINKORD(PSUM-IX) +                                   
227600           WS-PSUM-SULAGERB(PSUM-IX) +                                    
227700           WS-PSUM-SUSORTB(PSUM-IX)) = ZERO                               
227800              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
227900        ELSE                                                              
228000           COMPUTE WS-SERVG ROUNDED = (WS-PSUM-SUINKORD(PSUM-IX)          
228100                       + WS-PSUM-SULAGERB (PSUM-IX)) * 100                
228200                        / (WS-PSUM-SUINKORD(PSUM-IX) +                    
228300                           WS-PSUM-SULAGERB(PSUM-IX) +                    
228400                           WS-PSUM-SUSORTB(PSUM-IX))                      
228500           IF WS-SERVG = 100.0                                            
228600              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
228700           ELSE                                                           
228800              MOVE WS-SERVG TO PSUM-SPLIT(PSUM-IX)                        
228900           END-IF                                                         
229000        END-IF                                                            
229100                                                                          
229200*********  OMSHASTIGHET/PRISKLASS                                         
229300        COMPUTE WS-SUMMA = WS-PSUM-KVDISP-PR-AKT(PSUM-IX) +               
229400                           WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                 
229500        IF WS-SUMMA = ZERO                                                
229600           CONTINUE                                                       
229700        ELSE                                                              
229800           COMPUTE WS-OMSHAST ROUNDED =                                   
229900               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
230000           MOVE WS-OMSHAST TO PSUM-OMSHAST-DISP(PSUM-IX)                  
230100        END-IF                                                            
230200                                                                          
230300        COMPUTE WS-SUMMA = WS-PSUM-LS-PR-AKT(PSUM-IX) +                   
230400                           WS-PSUM-AK-PR-AKT(PSUM-IX) +                   
230500                           WS-PSUM-LS-PR-PAS(PSUM-IX) +                   
230600                           WS-PSUM-AK-PR-PAS(PSUM-IX)                     
230700        IF WS-SUMMA = ZERO                                                
230800           CONTINUE                                                       
230900        ELSE                                                              
231000           COMPUTE WS-OMSHAST ROUNDED =                                   
231100               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
231200           MOVE WS-OMSHAST TO PSUM-OMSHAST-LS(PSUM-IX)                    
231300        END-IF                                                            
231400                                                                          
231500        ADD +1 TO PSUM-IX                                                 
231600     END-PERFORM                                                          
231700     .                                                                    
231800     EJECT                                                                
231900 BCC-SUMMERA-FREKVENSKLASS SECTION.                                       
232000******************************************************************        
232100* SUMMERING PER FREKVENSKLASS                                    *        
232200******************************************************************        
232300                                                                          
232400     MOVE +1 TO FSUM-IX                                                   
232500                ART-IX                                                    
232600     MOVE +65 TO ART-IX-MAX                                               
232700     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
232800                                                                          
232900        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
233000          ADD ART-KVANT-AKT(ART-IX) TO FSUM-KVANT-AKT(FSUM-IX)            
233100          ADD ART-KVANT-PAS(ART-IX) TO FSUM-KVANT-PAS(FSUM-IX)            
233200          ADD ART-KVOT(ART-IX)      TO FSUM-KVOT(FSUM-IX)                 
233300          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-FSUM-KVOI-AKT(FSUM-IX)        
233400          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-FSUM-KVOI-PAS(FSUM-IX)        
233500          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-FSUM-KVOI-TEO(FSUM-IX)        
233600          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-FSUM-KVOI-SAK(FSUM-IX)        
233700          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
233800                                  TO WS-FSUM-KVOI-CDC-AKT(FSUM-IX)        
233900          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
234000                                  TO WS-FSUM-KVOI-CDC-PAS(FSUM-IX)        
234100          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
234200                                  TO WS-FSUM-KVOI-CDC-TEO(FSUM-IX)        
234300          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
234400                                  TO WS-FSUM-KVOI-CDC-SAK(FSUM-IX)        
234500          ADD WS-ART-KVOI(ART-IX) TO WS-FSUM-KVOI(FSUM-IX)                
234600          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
234700                             TO WS-FSUM-KVDISP-PR-AKT (FSUM-IX)           
234800          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
234900                             TO WS-FSUM-OK-PR-AKT(FSUM-IX)                
235000          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
235100                             TO WS-FSUM-LS-PR-AKT(FSUM-IX)                
235200          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
235300                             TO WS-FSUM-AK-PR-AKT(FSUM-IX)                
235400          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
235500                             TO WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
235600          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
235700                             TO WS-FSUM-OK-PR-PAS(FSUM-IX)                
235800          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
235900                             TO WS-FSUM-LS-PR-PAS(FSUM-IX)                
236000          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
236100                             TO WS-FSUM-AK-PR-PAS(FSUM-IX)                
236200          ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)            
236300          ADD WS-ART-MLAGER(ART-IX) TO WS-FSUM-MLAGER(FSUM-IX)            
236400          ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)            
236500          ADD WS-ART-SUINKORD(ART-IX) TO WS-FSUM-SUINKORD(FSUM-IX)        
236600          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-FSUM-SUFYSAVP(FSUM-IX)        
236700          ADD WS-ART-SUAVBRP (ART-IX) TO WS-FSUM-SUAVBRP (FSUM-IX)        
236800          ADD WS-ART-SULAGERB(ART-IX) TO WS-FSUM-SULAGERB(FSUM-IX)        
236900          ADD WS-ART-SUSORTB (ART-IX) TO WS-FSUM-SUSORTB (FSUM-IX)        
237000                                                                          
237100          ADD +8 TO ART-IX                                                
237200        END-PERFORM                                                       
237300        ADD +1 TO FSUM-IX                                                 
237400                                                                          
237500        EVALUATE TRUE                                                     
237600           WHEN  FSUM-IX = 1                                              
237700                 MOVE +1 TO ART-IX                                        
237800           WHEN  FSUM-IX = 2                                              
237900                 MOVE +2 TO ART-IX                                        
238000                 MOVE +66 TO ART-IX-MAX                                   
238100           WHEN  FSUM-IX = 3                                              
238200                 MOVE +3 TO ART-IX                                        
238300                 MOVE +67 TO ART-IX-MAX                                   
238400           WHEN  FSUM-IX = 4                                              
238500                 MOVE +4 TO ART-IX                                        
238600                 MOVE +68 TO ART-IX-MAX                                   
238700           WHEN  FSUM-IX = 5                                              
238800                 MOVE +5 TO ART-IX                                        
238900                 MOVE +69 TO ART-IX-MAX                                   
239000           WHEN  FSUM-IX = 6                                              
239100                 MOVE +6 TO ART-IX                                        
239200                 MOVE +70 TO ART-IX-MAX                                   
239300           WHEN  FSUM-IX = 7                                              
239400                 MOVE +7 TO ART-IX                                        
239500                 MOVE +71 TO ART-IX-MAX                                   
239600           WHEN  FSUM-IX = 8                                              
239700                 MOVE +8 TO ART-IX                                        
239800                 MOVE +72 TO ART-IX-MAX                                   
239900           WHEN OTHER                                                     
240000                CONTINUE                                                  
240100        END-EVALUATE                                                      
240200                                                                          
240300     END-PERFORM                                                          
240400                                                                          
240500     MOVE +1 TO FSUM-IX                                                   
240600     MOVE +8 TO FSUM-IX-MAX                                               
240700     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
240800                                                                          
240900*********  DISP LAGER VÄRDE/FREKVENSKLASS                                 
241000        IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) = ZERO                          
241100           CONTINUE                                                       
241200        ELSE                                                              
241300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
241400                     WS-FSUM-KVDISP-PR-AKT(FSUM-IX) / 1000                
241500           MOVE WS-SUMMA-KR TO FSUM-KVDISP-AKT(FSUM-IX)                   
241600        END-IF                                                            
241700                                                                          
241800        IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) = ZERO                          
241900           CONTINUE                                                       
242000        ELSE                                                              
242100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
242200                     WS-FSUM-KVDISP-PR-PAS(FSUM-IX) / 1000                
242300           MOVE WS-SUMMA-KR TO FSUM-KVDISP-PAS(FSUM-IX)                   
242400        END-IF                                                            
242500                                                                          
242600*********  LAGERVÄRDE/FREKVENSKLASS                                       
242700        IF WS-FSUM-LS-PR-AKT(FSUM-IX) = ZERO                              
242800           CONTINUE                                                       
242900        ELSE                                                              
243000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
243100                     WS-FSUM-LS-PR-AKT(FSUM-IX) / 1000                    
243200           MOVE WS-SUMMA-KR TO FSUM-LS-AKT(FSUM-IX)                       
243300        END-IF                                                            
243400                                                                          
243500        IF WS-FSUM-LS-PR-PAS(FSUM-IX) = ZERO                              
243600           CONTINUE                                                       
243700        ELSE                                                              
243800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
243900                     WS-FSUM-LS-PR-PAS(FSUM-IX) / 1000                    
244000           MOVE WS-SUMMA-KR TO FSUM-LS-PAS(FSUM-IX)                       
244100        END-IF                                                            
244200                                                                          
244300*********  AK-VÄRDE/FREKVENSKLASS                                         
244400        IF WS-FSUM-AK-PR-AKT(FSUM-IX) = ZERO                              
244500           CONTINUE                                                       
244600        ELSE                                                              
244700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
244800                     WS-FSUM-AK-PR-AKT(FSUM-IX) / 1000                    
244900           MOVE WS-SUMMA-KR TO FSUM-AK-AKT(FSUM-IX)                       
245000        END-IF                                                            
245100                                                                          
245200        IF WS-FSUM-AK-PR-PAS(FSUM-IX) = ZERO                              
245300           CONTINUE                                                       
245400        ELSE                                                              
245500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
245600                     WS-FSUM-AK-PR-PAS(FSUM-IX) / 1000                    
245700           MOVE WS-SUMMA-KR TO FSUM-AK-PAS(FSUM-IX)                       
245800        END-IF                                                            
245900                                                                          
246000*********  SÄKERHETSLAGER/FREKVENSKLASS                                   
246100        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
246200           CONTINUE                                                       
246300        ELSE                                                              
246400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
246500                          WS-FSUM-SLAGER(FSUM-IX) / 1000                  
246600           MOVE WS-SUMMA-KR TO FSUM-SLAGER(FSUM-IX)                       
246700        END-IF                                                            
246800                                                                          
246900*********  ÖVERLAGER/FREKVENSKLASS                                        
247000        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
247100           CONTINUE                                                       
247200        ELSE                                                              
247300           COMPUTE WS-SUMMA-KR ROUNDED                                    
247400                     = WS-FSUM-OLAGER(FSUM-IX) / 1000                     
247500           MOVE WS-SUMMA-KR TO FSUM-OLAGER(FSUM-IX)                       
247600        END-IF                                                            
247700                                                                          
247800*********  MEDELLAGER/FREKVENSKLASS                                       
247900        IF WS-FSUM-MLAGER(FSUM-IX) = ZERO                                 
248000           CONTINUE                                                       
248100        ELSE                                                              
248200           COMPUTE WS-SUMMA-KR =                                          
248300                       WS-FSUM-MLAGER(FSUM-IX) / 1000                     
248400           ADD WS-SUMMA-KR TO FSUM-MLAGER(FSUM-IX)                        
248500        END-IF                                                            
248600                                                                          
248700*********  SERVICEGRAD BRUTTO/FREKVENSKLASS                               
248800        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
248900           MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                           
249000        ELSE                                                              
249100           COMPUTE WS-SERVG ROUNDED =                                     
249200             WS-FSUM-SUAVBRP(FSUM-IX) * 100 /                             
249300                          WS-FSUM-SUINKORD(FSUM-IX)                       
249400           IF WS-SERVG = 100.0                                            
249500              MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                        
249600           ELSE                                                           
249700              MOVE WS-SERVG TO FSUM-SERVG-BTO(FSUM-IX)                    
249800           END-IF                                                         
249900        END-IF                                                            
250000                                                                          
250100*********  SERVICEGRAD NETTO/PRISKLASS                                    
250200        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
250300           MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                           
250400        ELSE                                                              
250500           COMPUTE WS-SERVG ROUNDED =                                     
250600            (WS-FSUM-SUAVBRP(FSUM-IX) - WS-FSUM-SUFYSAVP(FSUM-IX))        
250700                           * 100 /                                        
250800                          WS-FSUM-SUINKORD(FSUM-IX)                       
250900           IF WS-SERVG = 100.0                                            
251000              MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                        
251100           ELSE                                                           
251200              MOVE WS-SERVG TO FSUM-SERVG-NTO(FSUM-IX)                    
251300           END-IF                                                         
251400        END-IF                                                            
251500                                                                          
251600*********  SPLITFAKTOR/FREKVENSKLASS                                      
251700        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
251800           MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                               
251900        ELSE                                                              
252000           COMPUTE WS-SERVG ROUNDED = (WS-FSUM-SUINKORD(FSUM-IX)          
252100                       + WS-FSUM-SULAGERB (FSUM-IX)) * 100                
252200                        / (WS-FSUM-SUINKORD(FSUM-IX) +                    
252300                           WS-FSUM-SULAGERB(FSUM-IX) +                    
252400                           WS-FSUM-SUSORTB(FSUM-IX))                      
252500           IF WS-SERVG = 100.0                                            
252600              MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                            
252700           ELSE                                                           
252800              MOVE WS-SERVG TO FSUM-SPLIT(FSUM-IX)                        
252900           END-IF                                                         
253000        END-IF                                                            
253100                                                                          
253200*********  OMSHASTIGHET/FREKVENSKLASS                                     
253300        COMPUTE WS-SUMMA = WS-FSUM-KVDISP-PR-AKT(FSUM-IX) +               
253400                           WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                 
253500        IF WS-SUMMA = ZERO                                                
253600           CONTINUE                                                       
253700        ELSE                                                              
253800           COMPUTE WS-OMSHAST ROUNDED =                                   
253900               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
254000           MOVE WS-OMSHAST TO FSUM-OMSHAST-DISP(FSUM-IX)                  
254100        END-IF                                                            
254200                                                                          
254300        COMPUTE WS-SUMMA = WS-FSUM-LS-PR-AKT(FSUM-IX) +                   
254400                           WS-FSUM-AK-PR-AKT(FSUM-IX) +                   
254500                           WS-FSUM-LS-PR-PAS(FSUM-IX) +                   
254600                           WS-FSUM-AK-PR-PAS(FSUM-IX)                     
254700        IF WS-SUMMA = ZERO                                                
254800           CONTINUE                                                       
254900        ELSE                                                              
255000           COMPUTE WS-OMSHAST ROUNDED =                                   
255100               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
255200           MOVE WS-OMSHAST TO FSUM-OMSHAST-LS(FSUM-IX)                    
255300        END-IF                                                            
255400                                                                          
255500        ADD +1 TO FSUM-IX                                                 
255600                                                                          
255700     END-PERFORM                                                          
255800     .                                                                    
255900     EJECT                                                                
256000 BCD-SUMMERA-TOTAL SECTION.                                               
256100******************************************************************        
256200* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
256300******************************************************************        
256400                                                                          
256500     MOVE +1 TO PSUM-IX                                                   
256600     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
256700        ADD PSUM-KVANT-AKT(PSUM-IX) TO TOT-KVANT-AKT                      
256800        ADD PSUM-KVANT-PAS(PSUM-IX) TO TOT-KVANT-PAS                      
256900        ADD PSUM-KVOT(PSUM-IX) TO TOT-KVOT                                
257000        ADD WS-PSUM-KVOI-AKT(PSUM-IX) TO WS-TOT-KVOI-AKT                  
257100        ADD WS-PSUM-KVOI-PAS(PSUM-IX) TO WS-TOT-KVOI-PAS                  
257200        ADD WS-PSUM-KVOI-TEO(PSUM-IX) TO WS-TOT-KVOI-TEO                  
257300        ADD WS-PSUM-KVOI-SAK(PSUM-IX) TO WS-TOT-KVOI-SAK                  
257400        ADD WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                                 
257500                                TO WS-TOT-KVOI-CDC-AKT                    
257600        ADD WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                                 
257700                                TO WS-TOT-KVOI-CDC-PAS                    
257800        ADD WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                                 
257900                                TO WS-TOT-KVOI-CDC-TEO                    
258000        ADD WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                                 
258100                                TO WS-TOT-KVOI-CDC-SAK                    
258200        ADD WS-PSUM-KVOI(PSUM-IX) TO WS-TOT-KVOI                          
258300        ADD WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                                
258400                           TO WS-TOT-KVDISP-PR-AKT                        
258500        ADD WS-PSUM-OK-PR-AKT(PSUM-IX)                                    
258600                           TO WS-TOT-OK-PR-AKT                            
258700        ADD WS-PSUM-LS-PR-AKT(PSUM-IX)                                    
258800                           TO WS-TOT-LS-PR-AKT                            
258900        ADD WS-PSUM-AK-PR-AKT(PSUM-IX)                                    
259000                           TO WS-TOT-AK-PR-AKT                            
259100        ADD WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                                
259200                           TO WS-TOT-KVDISP-PR-PAS                        
259300        ADD WS-PSUM-OK-PR-PAS(PSUM-IX)                                    
259400                           TO WS-TOT-OK-PR-PAS                            
259500        ADD WS-PSUM-LS-PR-PAS(PSUM-IX)                                    
259600                           TO WS-TOT-LS-PR-PAS                            
259700        ADD WS-PSUM-AK-PR-PAS(PSUM-IX)                                    
259800                           TO WS-TOT-AK-PR-PAS                            
259900        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
260000        ADD WS-PSUM-MLAGER(PSUM-IX) TO WS-TOT-MLAGER                      
260100        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
260200        ADD WS-PSUM-SUINKORD(PSUM-IX) TO WS-TOT-SUINKORD                  
260300        ADD WS-PSUM-SUFYSAVP(PSUM-IX) TO WS-TOT-SUFYSAVP                  
260400        ADD WS-PSUM-SUAVBRP (PSUM-IX) TO WS-TOT-SUAVBRP                   
260500        ADD WS-PSUM-SULAGERB(PSUM-IX) TO WS-TOT-SULAGERB                  
260600        ADD WS-PSUM-SUSORTB (PSUM-IX) TO WS-TOT-SUSORTB                   
260700                                                                          
260800        ADD +1 TO PSUM-IX                                                 
260900     END-PERFORM                                                          
261000                                                                          
261100*********  DISP LAGER VÄRDE TOTALT                                        
261200        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
261300           CONTINUE                                                       
261400        ELSE                                                              
261500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
261600                     WS-TOT-KVDISP-PR-AKT / 1000                          
261700           MOVE WS-SUMMA-KR TO TOT-KVDISP-AKT                             
261800        END-IF                                                            
261900                                                                          
262000        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
262100           CONTINUE                                                       
262200        ELSE                                                              
262300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
262400                     WS-TOT-KVDISP-PR-PAS / 1000                          
262500           MOVE WS-SUMMA-KR TO TOT-KVDISP-PAS                             
262600        END-IF                                                            
262700                                                                          
262800*********  LAGERVÄRDE TOTALT                                              
262900        IF WS-TOT-LS-PR-AKT = ZERO                                        
263000           CONTINUE                                                       
263100        ELSE                                                              
263200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
263300                     WS-TOT-LS-PR-AKT / 1000                              
263400           MOVE WS-SUMMA-KR TO TOT-LS-AKT                                 
263500        END-IF                                                            
263600                                                                          
263700        IF WS-TOT-LS-PR-PAS = ZERO                                        
263800           CONTINUE                                                       
263900        ELSE                                                              
264000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
264100                     WS-TOT-LS-PR-PAS / 1000                              
264200           MOVE WS-SUMMA-KR TO TOT-LS-PAS                                 
264300        END-IF                                                            
264400                                                                          
264500*********  AK-VÄRDE TOTALT                                                
264600        IF WS-TOT-AK-PR-AKT = ZERO                                        
264700           CONTINUE                                                       
264800        ELSE                                                              
264900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
265000                     WS-TOT-AK-PR-AKT / 1000                              
265100           MOVE WS-SUMMA-KR TO TOT-AK-AKT                                 
265200        END-IF                                                            
265300                                                                          
265400        IF WS-TOT-AK-PR-PAS = ZERO                                        
265500           CONTINUE                                                       
265600        ELSE                                                              
265700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
265800                     WS-TOT-AK-PR-PAS / 1000                              
265900           MOVE WS-SUMMA-KR TO TOT-AK-PAS                                 
266000        END-IF                                                            
266100                                                                          
266200*********  SÄKERHETSLAGER TOTALT                                          
266300        IF WS-TOT-SLAGER = ZERO                                           
266400           CONTINUE                                                       
266500        ELSE                                                              
266600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
266700                          WS-TOT-SLAGER / 1000                            
266800           MOVE WS-SUMMA-KR TO TOT-SLAGER                                 
266900        END-IF                                                            
267000                                                                          
267100*********  ÖVERLAGER TOTALT                                               
267200        IF WS-TOT-OLAGER = ZERO                                           
267300           CONTINUE                                                       
267400        ELSE                                                              
267500           COMPUTE WS-SUMMA-KR ROUNDED                                    
267600                     = WS-TOT-OLAGER / 1000                               
267700           MOVE WS-SUMMA-KR TO TOT-OLAGER                                 
267800        END-IF                                                            
267900                                                                          
268000*********  MEDELLAGER TOTALT                                              
268100        IF WS-TOT-MLAGER = ZERO                                           
268200           CONTINUE                                                       
268300        ELSE                                                              
268400           COMPUTE WS-SUMMA-KR =                                          
268500                       WS-TOT-MLAGER / 1000                               
268600           ADD WS-SUMMA-KR TO TOT-MLAGER                                  
268700        END-IF                                                            
268800                                                                          
268900*********  SERVICEGRAD BRUTTO TOTALT                                      
269000        IF WS-TOT-SUINKORD = ZERO                                         
269100           MOVE 99.9 TO TOT-SERVG-BTO                                     
269200        ELSE                                                              
269300           COMPUTE WS-SERVG ROUNDED =                                     
269400             WS-TOT-SUAVBRP * 100 /                                       
269500                          WS-TOT-SUINKORD                                 
269600           IF WS-SERVG = 100.0                                            
269700              MOVE 99.9 TO TOT-SERVG-BTO                                  
269800           ELSE                                                           
269900              MOVE WS-SERVG TO TOT-SERVG-BTO                              
270000           END-IF                                                         
270100        END-IF                                                            
270200                                                                          
270300*********  SERVICEGRAD NETTO TOTALT                                       
270400        IF WS-TOT-SUINKORD = ZERO                                         
270500           MOVE 99.9 TO TOT-SERVG-BTO                                     
270600        ELSE                                                              
270700           COMPUTE WS-SERVG ROUNDED =                                     
270800            (WS-TOT-SUAVBRP - WS-TOT-SUFYSAVP)                            
270900                           * 100 /                                        
271000                          WS-TOT-SUINKORD                                 
271100           IF WS-SERVG = 100.0                                            
271200              MOVE 99.9 TO TOT-SERVG-NTO                                  
271300           ELSE                                                           
271400              MOVE WS-SERVG TO TOT-SERVG-NTO                              
271500           END-IF                                                         
271600        END-IF                                                            
271700                                                                          
271800*********  SPLITFAKTOR TOTALT                                             
271900        IF WS-TOT-SUINKORD = ZERO                                         
272000           MOVE 99.9 TO TOT-SPLIT                                         
272100        ELSE                                                              
272200           COMPUTE WS-SERVG ROUNDED = (WS-TOT-SUINKORD                    
272300                       + WS-TOT-SULAGERB) * 100                           
272400                        / (WS-TOT-SUINKORD +                              
272500                           WS-TOT-SULAGERB +                              
272600                           WS-TOT-SUSORTB)                                
272700           IF WS-SERVG = 100.0                                            
272800              MOVE 99.9 TO TOT-SPLIT                                      
272900           ELSE                                                           
273000              MOVE WS-SERVG TO TOT-SPLIT                                  
273100           END-IF                                                         
273200        END-IF                                                            
273300                                                                          
273400*********  OMSHASTIGHET TOTALT                                            
273500        COMPUTE WS-SUMMA = WS-TOT-KVDISP-PR-AKT +                         
273600                           WS-TOT-KVDISP-PR-PAS                           
273700        IF WS-SUMMA = ZERO                                                
273800           CONTINUE                                                       
273900        ELSE                                                              
274000           COMPUTE WS-OMSHAST ROUNDED =                                   
274100               WS-TOT-KVOI / WS-SUMMA                                     
274200           MOVE WS-OMSHAST TO TOT-OMSHAST-DISP                            
274300        END-IF                                                            
274400                                                                          
274500        COMPUTE WS-SUMMA = WS-TOT-LS-PR-AKT +                             
274600                           WS-TOT-AK-PR-AKT +                             
274700                           WS-TOT-LS-PR-PAS +                             
274800                           WS-TOT-AK-PR-PAS                               
274900        IF WS-SUMMA = ZERO                                                
275000           CONTINUE                                                       
275100        ELSE                                                              
275200           COMPUTE WS-OMSHAST ROUNDED =                                   
275300               WS-TOT-KVOI / WS-SUMMA                                     
275400           MOVE WS-OMSHAST TO TOT-OMSHAST-LS                              
275500                                                                          
275600        END-IF                                                            
275700*********  ORDERTRÄFFAR TOTALT                                            
275800                                                                          
275900        ADD WS-KVOT-SAKNAS-WDK7 TO TOT-KVOT                               
276000     .                                                                    
276100     EJECT                                                                
276200 BCE-BERAKNINGAR-AV-TOTAL SECTION.                                        
276300******************************************************************        
276400* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
276500******************************************************************        
276600                                                                          
276700     MOVE +1 TO ART-IX                                                    
276800     MOVE +72 TO ART-IX-MAX                                               
276900     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
277000                                                                          
277100******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
277200        IF TOT-KVANT-AKT = ZERO                                           
277300           CONTINUE                                                       
277400        ELSE                                                              
277500           COMPUTE WS-PROC = ART-KVANT-AKT(ART-IX)                        
277600                                    * 100 / TOT-KVANT-AKT                 
277700           MOVE WS-PROC TO ART-PROC-KVANT-A(ART-IX)                       
277800        END-IF                                                            
277900                                                                          
278000******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
278100        IF TOT-KVANT-PAS = ZERO                                           
278200           CONTINUE                                                       
278300        ELSE                                                              
278400           COMPUTE WS-PROC = ART-KVANT-PAS(ART-IX)                        
278500                                    * 100 / TOT-KVANT-PAS                 
278600           MOVE WS-PROC TO ART-PROC-KVANT-P(ART-IX)                       
278700        END-IF                                                            
278800                                                                          
278900******** % ANTAL ORDERTRÄFFAR AV TOTAL                                    
279000        IF TOT-KVOT = ZERO                                                
279100           CONTINUE                                                       
279200        ELSE                                                              
279300           COMPUTE WS-PROC = ART-KVOT(ART-IX)                             
279400                                    * 100 / TOT-KVOT                      
279500           MOVE WS-PROC TO ART-PROC-KVOT(ART-IX)                          
279600        END-IF                                                            
279700                                                                          
279800*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  AKTIVA                      
279900                                                                          
280000        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
280100           CONTINUE                                                       
280200        ELSE                                                              
280300           IF WS-ART-KVDISP-PR-AKT(ART-IX) > ZERO                         
280400              COMPUTE WS-PROC = WS-ART-KVDISP-PR-AKT(ART-IX)              
280500                              * 100 / WS-TOT-KVDISP-PR-AKT                
280600              MOVE WS-PROC TO ART-PROC-KVDISP-A(ART-IX)                   
280700           END-IF                                                         
280800        END-IF                                                            
280900                                                                          
281000*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  PASSIVA                     
281100                                                                          
281200        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
281300           CONTINUE                                                       
281400        ELSE                                                              
281500           IF WS-ART-KVDISP-PR-PAS(ART-IX) > ZERO                         
281600              COMPUTE WS-PROC = WS-ART-KVDISP-PR-PAS(ART-IX)              
281700                              * 100 / WS-TOT-KVDISP-PR-PAS                
281800              MOVE WS-PROC TO ART-PROC-KVDISP-P(ART-IX)                   
281900           END-IF                                                         
282000        END-IF                                                            
282100                                                                          
282200*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                      
282300                                                                          
282400        IF WS-TOT-LS-PR-AKT = ZERO                                        
282500           CONTINUE                                                       
282600        ELSE                                                              
282700           IF WS-ART-LS-PR-AKT(ART-IX) > ZERO                             
282800              COMPUTE WS-PROC = WS-ART-LS-PR-AKT(ART-IX)                  
282900                              * 100 / WS-TOT-LS-PR-AKT                    
283000              MOVE WS-PROC TO ART-PROC-LS-A(ART-IX)                       
283100           END-IF                                                         
283200        END-IF                                                            
283300                                                                          
283400*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA                     
283500                                                                          
283600        IF WS-TOT-LS-PR-PAS = ZERO                                        
283700           CONTINUE                                                       
283800        ELSE                                                              
283900           IF WS-ART-LS-PR-PAS(ART-IX) > ZERO                             
284000              COMPUTE WS-PROC = WS-ART-LS-PR-PAS(ART-IX)                  
284100                              * 100 / WS-TOT-LS-PR-PAS                    
284200              MOVE WS-PROC TO ART-PROC-LS-P(ART-IX)                       
284300           END-IF                                                         
284400        END-IF                                                            
284500                                                                          
284600*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                          
284700                                                                          
284800        IF WS-TOT-AK-PR-AKT = ZERO                                        
284900           CONTINUE                                                       
285000        ELSE                                                              
285100           IF WS-ART-AK-PR-AKT(ART-IX) > ZERO                             
285200              COMPUTE WS-PROC = WS-ART-AK-PR-AKT(ART-IX)                  
285300                              * 100 / WS-TOT-AK-PR-AKT                    
285400              MOVE WS-PROC TO ART-PROC-AK-A(ART-IX)                       
285500           END-IF                                                         
285600        END-IF                                                            
285700                                                                          
285800*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                         
285900                                                                          
286000        IF WS-TOT-AK-PR-PAS = ZERO                                        
286100           CONTINUE                                                       
286200        ELSE                                                              
286300           IF WS-ART-AK-PR-PAS(ART-IX) > ZERO                             
286400              COMPUTE WS-PROC = WS-ART-AK-PR-PAS(ART-IX)                  
286500                              * 100 / WS-TOT-AK-PR-PAS                    
286600              MOVE WS-PROC TO ART-PROC-AK-P(ART-IX)                       
286700           END-IF                                                         
286800        END-IF                                                            
286900                                                                          
287000        ADD +1 TO ART-IX                                                  
287100     END-PERFORM                                                          
287200                                                                          
287300****************************                                              
287400                                                                          
287500     MOVE +1 TO PSUM-IX                                                   
287600     MOVE +9 TO PSUM-IX-MAX                                               
287700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
287800                                                                          
287900******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
288000        IF TOT-KVANT-AKT = ZERO                                           
288100           CONTINUE                                                       
288200        ELSE                                                              
288300           COMPUTE WS-PROC = PSUM-KVANT-AKT(PSUM-IX) * 100 /              
288400                           TOT-KVANT-AKT                                  
288500           MOVE WS-PROC TO PSUM-PROC-KVANT-A(PSUM-IX)                     
288600        END-IF                                                            
288700                                                                          
288800******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
288900        IF TOT-KVANT-PAS = ZERO                                           
289000           CONTINUE                                                       
289100        ELSE                                                              
289200           COMPUTE WS-PROC = PSUM-KVANT-PAS(PSUM-IX) * 100 /              
289300                           TOT-KVANT-PAS                                  
289400           MOVE WS-PROC TO PSUM-PROC-KVANT-P(PSUM-IX)                     
289500        END-IF                                                            
289600                                                                          
289700******** % ANTAL ORDERTRÄFFAR AV TOTALA                                   
289800        IF TOT-KVOT = ZERO                                                
289900           CONTINUE                                                       
290000        ELSE                                                              
290100           COMPUTE WS-PROC = PSUM-KVOT(PSUM-IX) * 100 /                   
290200                           TOT-KVOT                                       
290300           MOVE WS-PROC TO PSUM-PROC-KVOT(PSUM-IX)                        
290400        END-IF                                                            
290500                                                                          
290600*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA                
290700                                                                          
290800        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
290900           CONTINUE                                                       
291000        ELSE                                                              
291100           IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) > ZERO                       
291200              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-AKT(PSUM-IX)            
291300                              * 100 / WS-TOT-KVDISP-PR-AKT                
291400              MOVE WS-PROC TO PSUM-PROC-KVDISP-A(PSUM-IX)                 
291500           END-IF                                                         
291600        END-IF                                                            
291700                                                                          
291800*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA               
291900                                                                          
292000        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
292100           CONTINUE                                                       
292200        ELSE                                                              
292300           IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) > ZERO                       
292400              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
292500                              * 100 / WS-TOT-KVDISP-PR-PAS                
292600              MOVE WS-PROC TO PSUM-PROC-KVDISP-P(PSUM-IX)                 
292700           END-IF                                                         
292800        END-IF                                                            
292900                                                                          
293000*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                
293100                                                                          
293200        IF WS-TOT-LS-PR-AKT = ZERO                                        
293300           CONTINUE                                                       
293400        ELSE                                                              
293500           IF WS-PSUM-LS-PR-AKT(PSUM-IX) > ZERO                           
293600              COMPUTE WS-PROC = WS-PSUM-LS-PR-AKT(PSUM-IX)                
293700                              * 100 / WS-TOT-LS-PR-AKT                    
293800              MOVE WS-PROC TO PSUM-PROC-LS-A(PSUM-IX)                     
293900           END-IF                                                         
294000        END-IF                                                            
294100                                                                          
294200*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA               
294300                                                                          
294400        IF WS-TOT-LS-PR-PAS = ZERO                                        
294500           CONTINUE                                                       
294600        ELSE                                                              
294700           IF WS-PSUM-LS-PR-PAS(PSUM-IX) > ZERO                           
294800              COMPUTE WS-PROC = WS-PSUM-LS-PR-PAS(PSUM-IX)                
294900                              * 100 / WS-TOT-LS-PR-PAS                    
295000              MOVE WS-PROC TO PSUM-PROC-LS-P(PSUM-IX)                     
295100           END-IF                                                         
295200        END-IF                                                            
295300                                                                          
295400*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                    
295500                                                                          
295600        IF WS-TOT-AK-PR-AKT = ZERO                                        
295700           CONTINUE                                                       
295800        ELSE                                                              
295900           IF WS-PSUM-AK-PR-AKT(PSUM-IX) > ZERO                           
296000              COMPUTE WS-PROC = WS-PSUM-AK-PR-AKT(PSUM-IX)                
296100                              * 100 / WS-TOT-AK-PR-AKT                    
296200              MOVE WS-PROC TO PSUM-PROC-AK-A(PSUM-IX)                     
296300           END-IF                                                         
296400        END-IF                                                            
296500                                                                          
296600*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                   
296700                                                                          
296800        IF WS-TOT-AK-PR-PAS = ZERO                                        
296900           CONTINUE                                                       
297000        ELSE                                                              
297100           IF WS-PSUM-AK-PR-PAS(PSUM-IX) > ZERO                           
297200              COMPUTE WS-PROC = WS-PSUM-AK-PR-PAS(PSUM-IX)                
297300                              * 100 / WS-TOT-AK-PR-PAS                    
297400              MOVE WS-PROC TO PSUM-PROC-AK-P(PSUM-IX)                     
297500           END-IF                                                         
297600        END-IF                                                            
297700                                                                          
297800        ADD +1 TO PSUM-IX                                                 
297900     END-PERFORM                                                          
298000                                                                          
298100****************************************                                  
298200                                                                          
298300     MOVE +1 TO FSUM-IX                                                   
298400     MOVE +8 TO FSUM-IX-MAX                                               
298500     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
298600                                                                          
298700******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
298800        IF TOT-KVANT-AKT = ZERO                                           
298900           CONTINUE                                                       
299000        ELSE                                                              
299100           COMPUTE WS-PROC = FSUM-KVANT-AKT(FSUM-IX) * 100 /              
299200                           TOT-KVANT-AKT                                  
299300           MOVE WS-PROC TO FSUM-PROC-KVANT-A(FSUM-IX)                     
299400        END-IF                                                            
299500                                                                          
299600******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
299700        IF TOT-KVANT-PAS = ZERO                                           
299800           CONTINUE                                                       
299900        ELSE                                                              
300000           COMPUTE WS-PROC = FSUM-KVANT-PAS(FSUM-IX) * 100 /              
300100                           TOT-KVANT-PAS                                  
300200           MOVE WS-PROC TO FSUM-PROC-KVANT-P(FSUM-IX)                     
300300        END-IF                                                            
300400                                                                          
300500******** % ANTAL ORDERTRÄFFAR TOTALA                                      
300600        IF TOT-KVOT = ZERO                                                
300700           CONTINUE                                                       
300800        ELSE                                                              
300900           COMPUTE WS-PROC = FSUM-KVOT(FSUM-IX) * 100 /                   
301000                           TOT-KVOT                                       
301100           MOVE WS-PROC TO FSUM-PROC-KVOT(FSUM-IX)                        
301200        END-IF                                                            
301300                                                                          
301400*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA            
301500                                                                          
301600        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
301700           CONTINUE                                                       
301800        ELSE                                                              
301900           IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) > ZERO                       
302000              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-AKT(FSUM-IX)            
302100                              * 100 / WS-TOT-KVDISP-PR-AKT                
302200              MOVE WS-PROC TO FSUM-PROC-KVDISP-A(FSUM-IX)                 
302300           END-IF                                                         
302400        END-IF                                                            
302500                                                                          
302600*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA           
302700                                                                          
302800        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
302900           CONTINUE                                                       
303000        ELSE                                                              
303100           IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) > ZERO                       
303200              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
303300                              * 100 / WS-TOT-KVDISP-PR-PAS                
303400              MOVE WS-PROC TO FSUM-PROC-KVDISP-P(FSUM-IX)                 
303500           END-IF                                                         
303600        END-IF                                                            
303700                                                                          
303800*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA            
303900                                                                          
304000        IF WS-TOT-LS-PR-AKT = ZERO                                        
304100           CONTINUE                                                       
304200        ELSE                                                              
304300           IF WS-FSUM-LS-PR-AKT(FSUM-IX) > ZERO                           
304400              COMPUTE WS-PROC = WS-FSUM-LS-PR-AKT(FSUM-IX)                
304500                              * 100 / WS-TOT-LS-PR-AKT                    
304600              MOVE WS-PROC TO FSUM-PROC-LS-A(FSUM-IX)                     
304700           END-IF                                                         
304800        END-IF                                                            
304900                                                                          
305000*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA           
305100                                                                          
305200        IF WS-TOT-LS-PR-PAS = ZERO                                        
305300           CONTINUE                                                       
305400        ELSE                                                              
305500           IF WS-FSUM-LS-PR-PAS(FSUM-IX) > ZERO                           
305600              COMPUTE WS-PROC = WS-FSUM-LS-PR-PAS(FSUM-IX)                
305700                              * 100 / WS-TOT-LS-PR-PAS                    
305800              MOVE WS-PROC TO FSUM-PROC-LS-P(FSUM-IX)                     
305900           END-IF                                                         
306000        END-IF                                                            
306100                                                                          
306200*******  %  FREKVENSSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA               
306300                                                                          
306400        IF WS-TOT-AK-PR-AKT = ZERO                                        
306500           CONTINUE                                                       
306600        ELSE                                                              
306700           IF WS-FSUM-AK-PR-AKT(FSUM-IX) > ZERO                           
306800              COMPUTE WS-PROC = WS-FSUM-AK-PR-AKT(FSUM-IX)                
306900                              * 100 / WS-TOT-AK-PR-AKT                    
307000              MOVE WS-PROC TO FSUM-PROC-AK-A(FSUM-IX)                     
307100           END-IF                                                         
307200        END-IF                                                            
307300                                                                          
307400*******  %  FREKVENSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA               
307500                                                                          
307600        IF WS-TOT-AK-PR-PAS = ZERO                                        
307700           CONTINUE                                                       
307800        ELSE                                                              
307900           IF WS-FSUM-AK-PR-PAS(FSUM-IX) > ZERO                           
308000              COMPUTE WS-PROC = WS-FSUM-AK-PR-PAS(FSUM-IX)                
308100                              * 100 / WS-TOT-AK-PR-PAS                    
308200              MOVE WS-PROC TO FSUM-PROC-AK-P(FSUM-IX)                     
308300           END-IF                                                         
308400        END-IF                                                            
308500                                                                          
308600        ADD +1 TO FSUM-IX                                                 
308700     END-PERFORM                                                          
308800     .                                                                    
308900     EJECT                                                                
309000 C-SKRIV-LISTA SECTION.                                                   
309100******************************************************************        
309200*  SID 1 BESTÅR AV 3 RUTRADER INKL PRISKLASS-TOTAL               *        
309300*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
309400*      3           3 RUTRADER INKL PRISKLASS-TOTAL               *        
309500*      4           1 RUTRAD   FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
309600******************************************************************        
309700                                                                          
309800     MOVE +1 TO IX1                                                       
309900     MOVE +2 TO IX2                                                       
310000     MOVE +3 TO IX3                                                       
310100     MOVE +4 TO IX4                                                       
310200     MOVE +5 TO IX5                                                       
310300     MOVE +6 TO IX6                                                       
310400     MOVE +7 TO IX7                                                       
310500     MOVE +8 TO IX8                                                       
310600     MOVE +1 TO PSUM-IX                                                   
310700                                                                          
310800*********** SKRIVER SID-1                                                 
310900                                                                          
311000     PERFORM S21A-SKRIV-RUBRIKER                                          
311100     MOVE '1' TO W001-DET1-PRISKLASS                                      
311200     PERFORM CA-FLYTTA-SKRIV-RAD                                          
311300     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
311400     ADD +1 TO PSUM-IX                                                    
311500     MOVE '2' TO W001-DET1-PRISKLASS                                      
311600     PERFORM CA-FLYTTA-SKRIV-RAD                                          
311700                                                                          
311800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
311900     ADD +1 TO PSUM-IX                                                    
312000     MOVE '3' TO W001-DET1-PRISKLASS                                      
312100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
312200                                                                          
312300*********** SKRIVER SID-2                                                 
312400     PERFORM S21A-SKRIV-RUBRIKER                                          
312500     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
312600     ADD +1 TO PSUM-IX                                                    
312700     MOVE '4' TO W001-DET1-PRISKLASS                                      
312800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
312900                                                                          
313000     ADD +1 TO PSUM-IX                                                    
313100     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
313200     MOVE '5' TO W001-DET1-PRISKLASS                                      
313300     PERFORM CA-FLYTTA-SKRIV-RAD                                          
313400                                                                          
313500     ADD +1 TO PSUM-IX                                                    
313600     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
313700     MOVE '6' TO W001-DET1-PRISKLASS                                      
313800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
313900                                                                          
314000*********** SKRIVER SID-3                                                 
314100     PERFORM S21A-SKRIV-RUBRIKER                                          
314200     ADD +1 TO PSUM-IX                                                    
314300     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
314400     MOVE '7' TO W001-DET1-PRISKLASS                                      
314500     PERFORM CA-FLYTTA-SKRIV-RAD                                          
314600                                                                          
314700     ADD +1 TO PSUM-IX                                                    
314800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
314900     MOVE '8' TO W001-DET1-PRISKLASS                                      
315000     PERFORM CA-FLYTTA-SKRIV-RAD                                          
315100                                                                          
315200     ADD +1 TO PSUM-IX                                                    
315300     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
315400     MOVE '9' TO W001-DET1-PRISKLASS                                      
315500     PERFORM CA-FLYTTA-SKRIV-RAD                                          
315600                                                                          
315700*********** SKRIVER SID-4                                                 
315800     PERFORM S21A-SKRIV-RUBRIKER                                          
315900     MOVE +1 TO IX1                                                       
316000     MOVE +2 TO IX2                                                       
316100     MOVE +3 TO IX3                                                       
316200     MOVE +4 TO IX4                                                       
316300     MOVE +5 TO IX5                                                       
316400     MOVE +6 TO IX6                                                       
316500     MOVE +7 TO IX7                                                       
316600     MOVE +8 TO IX8                                                       
316700     MOVE SPACE TO W001-DET1-PRISKLASS                                    
316800     PERFORM CB-FLYTTA-SKRIV-TOT                                          
316900     .                                                                    
317000     EJECT                                                                
317100 CA-FLYTTA-SKRIV-RAD SECTION.                                             
317200                                                                          
317300     MOVE ART-KVANT-AKT(IX1)       TO  W001-DET1-KVANTA                   
317400     MOVE ART-PROC-KVANT-A(IX1)    TO  W001-DET1-P-KVANTA                 
317500     MOVE ART-KVANT-AKT(IX2)       TO  W001-DET1-KVANTB                   
317600     MOVE ART-PROC-KVANT-A(IX2)    TO  W001-DET1-P-KVANTB                 
317700     MOVE ART-KVANT-AKT(IX3)       TO  W001-DET1-KVANTC                   
317800     MOVE ART-PROC-KVANT-A(IX3)    TO  W001-DET1-P-KVANTC                 
317900     MOVE ART-KVANT-AKT(IX4)       TO  W001-DET1-KVANTD                   
318000     MOVE ART-PROC-KVANT-A(IX4)    TO  W001-DET1-P-KVANTD                 
318100     MOVE ART-KVANT-AKT(IX5)       TO  W001-DET1-KVANTE                   
318200     MOVE ART-PROC-KVANT-A(IX5)    TO  W001-DET1-P-KVANTE                 
318300     MOVE ART-KVANT-AKT(IX6)       TO  W001-DET1-KVANTF                   
318400     MOVE ART-PROC-KVANT-A(IX6)    TO  W001-DET1-P-KVANTF                 
318500     MOVE ART-KVANT-AKT(IX7)       TO  W001-DET1-KVANTG                   
318600     MOVE ART-PROC-KVANT-A(IX7)    TO  W001-DET1-P-KVANTG                 
318700     MOVE ART-KVANT-AKT(IX8)       TO  W001-DET1-KVANTH                   
318800     MOVE ART-PROC-KVANT-A(IX8)    TO  W001-DET1-P-KVANTH                 
318900     MOVE PSUM-KVANT-AKT(PSUM-IX)  TO  W001-DET1-TOT                      
319000     MOVE PSUM-PROC-KVANT-A(PSUM-IX) TO W001-DET1-P-TOT                   
319100     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
319200     MOVE +2 TO W001-SKIP                                                 
319300     PERFORM S21-SKRIV-LISTA                                              
319400                                                                          
319500     MOVE ART-KVANT-PAS(IX1)       TO  W001-DET2-KVANTA                   
319600     MOVE ART-PROC-KVANT-P(IX1)    TO  W001-DET2-P-KVANTA                 
319700     MOVE ART-KVANT-PAS(IX2)       TO  W001-DET2-KVANTB                   
319800     MOVE ART-PROC-KVANT-P(IX2)    TO  W001-DET2-P-KVANTB                 
319900     MOVE ART-KVANT-PAS(IX3)       TO  W001-DET2-KVANTC                   
320000     MOVE ART-PROC-KVANT-P(IX3)    TO  W001-DET2-P-KVANTC                 
320100     MOVE ART-KVANT-PAS(IX4)       TO  W001-DET2-KVANTD                   
320200     MOVE ART-PROC-KVANT-P(IX4)    TO  W001-DET2-P-KVANTD                 
320300     MOVE ART-KVANT-PAS(IX5)       TO  W001-DET2-KVANTE                   
320400     MOVE ART-PROC-KVANT-P(IX5)    TO  W001-DET2-P-KVANTE                 
320500     MOVE ART-KVANT-PAS(IX6)       TO  W001-DET2-KVANTF                   
320600     MOVE ART-PROC-KVANT-P(IX6)    TO  W001-DET2-P-KVANTF                 
320700     MOVE ART-KVANT-PAS(IX7)       TO  W001-DET2-KVANTG                   
320800     MOVE ART-PROC-KVANT-P(IX7)    TO  W001-DET2-P-KVANTG                 
320900     MOVE ART-KVANT-PAS(IX8)       TO  W001-DET2-KVANTH                   
321000     MOVE ART-PROC-KVANT-P(IX8)    TO  W001-DET2-P-KVANTH                 
321100     MOVE PSUM-KVANT-PAS(PSUM-IX)  TO  W001-DET2-TOT                      
321200     MOVE PSUM-PROC-KVANT-P(PSUM-IX) TO W001-DET2-P-TOT                   
321300     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
321400     MOVE +1 TO W001-SKIP                                                 
321500     PERFORM S21-SKRIV-LISTA                                              
321600                                                                          
321700     MOVE ART-KVDISP-AKT(IX1)      TO  W001-DET3-DLAGERA                  
321800     MOVE ART-PROC-KVDISP-A(IX1)   TO  W001-DET3-P-DLAGERA                
321900     MOVE ART-KVDISP-AKT(IX2)      TO  W001-DET3-DLAGERB                  
322000     MOVE ART-PROC-KVDISP-A(IX2)   TO  W001-DET3-P-DLAGERB                
322100     MOVE ART-KVDISP-AKT(IX3)      TO  W001-DET3-DLAGERC                  
322200     MOVE ART-PROC-KVDISP-A(IX3)   TO  W001-DET3-P-DLAGERC                
322300     MOVE ART-KVDISP-AKT(IX4)      TO  W001-DET3-DLAGERD                  
322400     MOVE ART-PROC-KVDISP-A(IX4)   TO  W001-DET3-P-DLAGERD                
322500     MOVE ART-KVDISP-AKT(IX5)      TO  W001-DET3-DLAGERE                  
322600     MOVE ART-PROC-KVDISP-A(IX5)   TO  W001-DET3-P-DLAGERE                
322700     MOVE ART-KVDISP-AKT(IX6)      TO  W001-DET3-DLAGERF                  
322800     MOVE ART-PROC-KVDISP-A(IX6)   TO  W001-DET3-P-DLAGERF                
322900     MOVE ART-KVDISP-AKT(IX7)      TO  W001-DET3-DLAGERG                  
323000     MOVE ART-PROC-KVDISP-A(IX7)   TO  W001-DET3-P-DLAGERG                
323100     MOVE ART-KVDISP-AKT(IX8)      TO  W001-DET3-DLAGERH                  
323200     MOVE ART-PROC-KVDISP-A(IX8)   TO  W001-DET3-P-DLAGERH                
323300     MOVE PSUM-KVDISP-AKT(PSUM-IX) TO  W001-DET3-TOT                      
323400     MOVE PSUM-PROC-KVDISP-A(PSUM-IX)                                     
323500                                   TO  W001-DET3-P-TOT                    
323600     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
323700     MOVE +1 TO W001-SKIP                                                 
323800     PERFORM S21-SKRIV-LISTA                                              
323900                                                                          
324000     MOVE ART-KVDISP-PAS(IX1)      TO  W001-DET4-DLAGERA                  
324100     MOVE ART-PROC-KVDISP-P(IX1)   TO  W001-DET4-P-DLAGERA                
324200     MOVE ART-KVDISP-PAS(IX2)      TO  W001-DET4-DLAGERB                  
324300     MOVE ART-PROC-KVDISP-P(IX2)   TO  W001-DET4-P-DLAGERB                
324400     MOVE ART-KVDISP-PAS(IX3)      TO  W001-DET4-DLAGERC                  
324500     MOVE ART-PROC-KVDISP-P(IX3)   TO  W001-DET4-P-DLAGERC                
324600     MOVE ART-KVDISP-PAS(IX4)      TO  W001-DET4-DLAGERD                  
324700     MOVE ART-PROC-KVDISP-P(IX4)   TO  W001-DET4-P-DLAGERD                
324800     MOVE ART-KVDISP-PAS(IX5)      TO  W001-DET4-DLAGERE                  
324900     MOVE ART-PROC-KVDISP-P(IX5)   TO  W001-DET4-P-DLAGERE                
325000     MOVE ART-KVDISP-PAS(IX6)      TO  W001-DET4-DLAGERF                  
325100     MOVE ART-PROC-KVDISP-P(IX6)   TO  W001-DET4-P-DLAGERF                
325200     MOVE ART-KVDISP-PAS(IX7)      TO  W001-DET4-DLAGERG                  
325300     MOVE ART-PROC-KVDISP-P(IX7)   TO  W001-DET4-P-DLAGERG                
325400     MOVE ART-KVDISP-PAS(IX8)      TO  W001-DET4-DLAGERH                  
325500     MOVE ART-PROC-KVDISP-P(IX8)   TO  W001-DET4-P-DLAGERH                
325600     MOVE PSUM-KVDISP-PAS(PSUM-IX) TO  W001-DET4-TOT                      
325700     MOVE PSUM-PROC-KVDISP-P(PSUM-IX)                                     
325800                                   TO  W001-DET4-P-TOT                    
325900     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
326000     MOVE +1 TO W001-SKIP                                                 
326100     PERFORM S21-SKRIV-LISTA                                              
326200                                                                          
326300     MOVE ART-LS-AKT(IX1)          TO  W001-DET5-LLAGERA                  
326400     MOVE ART-PROC-LS-A(IX1)       TO  W001-DET5-P-LLAGERA                
326500     MOVE ART-LS-AKT(IX2)          TO  W001-DET5-LLAGERB                  
326600     MOVE ART-PROC-LS-A(IX2)       TO  W001-DET5-P-LLAGERB                
326700     MOVE ART-LS-AKT(IX3)          TO  W001-DET5-LLAGERC                  
326800     MOVE ART-PROC-LS-A(IX3)       TO  W001-DET5-P-LLAGERC                
326900     MOVE ART-LS-AKT(IX4)          TO  W001-DET5-LLAGERD                  
327000     MOVE ART-PROC-LS-A(IX4)       TO  W001-DET5-P-LLAGERD                
327100     MOVE ART-LS-AKT(IX5)          TO  W001-DET5-LLAGERE                  
327200     MOVE ART-PROC-LS-A(IX5)       TO  W001-DET5-P-LLAGERE                
327300     MOVE ART-LS-AKT(IX6)          TO  W001-DET5-LLAGERF                  
327400     MOVE ART-PROC-LS-A(IX6)       TO  W001-DET5-P-LLAGERF                
327500     MOVE ART-LS-AKT(IX7)          TO  W001-DET5-LLAGERG                  
327600     MOVE ART-PROC-LS-A(IX7)       TO  W001-DET5-P-LLAGERG                
327700     MOVE ART-LS-AKT(IX8)          TO  W001-DET5-LLAGERH                  
327800     MOVE ART-PROC-LS-A(IX8)       TO  W001-DET5-P-LLAGERH                
327900     MOVE PSUM-LS-AKT(PSUM-IX)     TO  W001-DET5-TOT                      
328000     MOVE PSUM-PROC-LS-A(PSUM-IX)  TO  W001-DET5-P-TOT                    
328100     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
328200     MOVE +1 TO W001-SKIP                                                 
328300     PERFORM S21-SKRIV-LISTA                                              
328400                                                                          
328500     MOVE ART-LS-PAS(IX1)          TO  W001-DET6-LLAGERA                  
328600     MOVE ART-PROC-LS-P(IX1)       TO  W001-DET6-P-LLAGERA                
328700     MOVE ART-LS-PAS(IX2)          TO  W001-DET6-LLAGERB                  
328800     MOVE ART-PROC-LS-P(IX2)       TO  W001-DET6-P-LLAGERB                
328900     MOVE ART-LS-PAS(IX3)          TO  W001-DET6-LLAGERC                  
329000     MOVE ART-PROC-LS-P(IX3)       TO  W001-DET6-P-LLAGERC                
329100     MOVE ART-LS-PAS(IX4)          TO  W001-DET6-LLAGERD                  
329200     MOVE ART-PROC-LS-P(IX4)       TO  W001-DET6-P-LLAGERD                
329300     MOVE ART-LS-PAS(IX5)          TO  W001-DET6-LLAGERE                  
329400     MOVE ART-PROC-LS-P(IX5)       TO  W001-DET6-P-LLAGERE                
329500     MOVE ART-LS-PAS(IX6)          TO  W001-DET6-LLAGERF                  
329600     MOVE ART-PROC-LS-P(IX6)       TO  W001-DET6-P-LLAGERF                
329700     MOVE ART-LS-PAS(IX7)          TO  W001-DET6-LLAGERG                  
329800     MOVE ART-PROC-LS-P(IX7)       TO  W001-DET6-P-LLAGERG                
329900     MOVE ART-LS-PAS(IX8)          TO  W001-DET6-LLAGERH                  
330000     MOVE ART-PROC-LS-P(IX8)       TO  W001-DET6-P-LLAGERH                
330100     MOVE PSUM-LS-PAS(PSUM-IX)     TO  W001-DET6-TOT                      
330200     MOVE PSUM-PROC-LS-P(PSUM-IX)  TO  W001-DET6-P-TOT                    
330300     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
330400     MOVE +1 TO W001-SKIP                                                 
330500     PERFORM S21-SKRIV-LISTA                                              
330600                                                                          
330700     MOVE ART-AK-AKT(IX1)          TO  W001-DET7-ALAGERA                  
330800     MOVE ART-PROC-AK-A(IX1)       TO  W001-DET7-P-ALAGERA                
330900     MOVE ART-AK-AKT(IX2)          TO  W001-DET7-ALAGERB                  
331000     MOVE ART-PROC-AK-A(IX2)       TO  W001-DET7-P-ALAGERB                
331100     MOVE ART-AK-AKT(IX3)          TO  W001-DET7-ALAGERC                  
331200     MOVE ART-PROC-AK-A(IX3)       TO  W001-DET7-P-ALAGERC                
331300     MOVE ART-AK-AKT(IX4)          TO  W001-DET7-ALAGERD                  
331400     MOVE ART-PROC-AK-A(IX4)       TO  W001-DET7-P-ALAGERD                
331500     MOVE ART-AK-AKT(IX5)          TO  W001-DET7-ALAGERE                  
331600     MOVE ART-PROC-AK-A(IX5)       TO  W001-DET7-P-ALAGERE                
331700     MOVE ART-AK-AKT(IX6)          TO  W001-DET7-ALAGERF                  
331800     MOVE ART-PROC-AK-A(IX6)       TO  W001-DET7-P-ALAGERF                
331900     MOVE ART-AK-AKT(IX7)          TO  W001-DET7-ALAGERG                  
332000     MOVE ART-PROC-AK-A(IX7)       TO  W001-DET7-P-ALAGERG                
332100     MOVE ART-AK-AKT(IX8)          TO  W001-DET7-ALAGERH                  
332200     MOVE ART-PROC-AK-A(IX8)       TO  W001-DET7-P-ALAGERH                
332300     MOVE PSUM-AK-AKT(PSUM-IX)     TO  W001-DET7-TOT                      
332400     MOVE PSUM-PROC-AK-A(PSUM-IX)  TO  W001-DET7-P-TOT                    
332500     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
332600     MOVE +1 TO W001-SKIP                                                 
332700     PERFORM S21-SKRIV-LISTA                                              
332800                                                                          
332900     MOVE ART-AK-PAS(IX1)          TO  W001-DET8-ALAGERA                  
333000     MOVE ART-PROC-AK-P(IX1)       TO  W001-DET8-P-ALAGERA                
333100     MOVE ART-AK-PAS(IX2)          TO  W001-DET8-ALAGERB                  
333200     MOVE ART-PROC-AK-P(IX2)       TO  W001-DET8-P-ALAGERB                
333300     MOVE ART-AK-PAS(IX3)          TO  W001-DET8-ALAGERC                  
333400     MOVE ART-PROC-AK-P(IX3)       TO  W001-DET8-P-ALAGERC                
333500     MOVE ART-AK-PAS(IX4)          TO  W001-DET8-ALAGERD                  
333600     MOVE ART-PROC-AK-P(IX4)       TO  W001-DET8-P-ALAGERD                
333700     MOVE ART-AK-PAS(IX5)          TO  W001-DET8-ALAGERE                  
333800     MOVE ART-PROC-AK-P(IX5)       TO  W001-DET8-P-ALAGERE                
333900     MOVE ART-AK-PAS(IX6)          TO  W001-DET8-ALAGERF                  
334000     MOVE ART-PROC-AK-P(IX6)       TO  W001-DET8-P-ALAGERF                
334100     MOVE ART-AK-PAS(IX7)          TO  W001-DET8-ALAGERG                  
334200     MOVE ART-PROC-AK-P(IX7)       TO  W001-DET8-P-ALAGERG                
334300     MOVE ART-AK-PAS(IX8)          TO  W001-DET8-ALAGERH                  
334400     MOVE ART-PROC-AK-P(IX8)       TO  W001-DET8-P-ALAGERH                
334500     MOVE PSUM-AK-PAS(PSUM-IX)     TO  W001-DET8-TOT                      
334600     MOVE PSUM-PROC-AK-P(PSUM-IX)  TO  W001-DET8-P-TOT                    
334700     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
334800     MOVE +1 TO W001-SKIP                                                 
334900     PERFORM S21-SKRIV-LISTA                                              
335000                                                                          
335100     MOVE ART-OLAGER(IX1)          TO  W001-DET9-OLAGERA                  
335200     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET9-P-OLAGERA                
335300     MOVE ART-OLAGER(IX2)          TO  W001-DET9-OLAGERB                  
335400     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET9-P-OLAGERB                
335500     MOVE ART-OLAGER(IX3)          TO  W001-DET9-OLAGERC                  
335600     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET9-P-OLAGERC                
335700     MOVE ART-OLAGER(IX4)          TO  W001-DET9-OLAGERD                  
335800     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET9-P-OLAGERD                
335900     MOVE ART-OLAGER(IX5)          TO  W001-DET9-OLAGERE                  
336000     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET9-P-OLAGERE                
336100     MOVE ART-OLAGER(IX6)          TO  W001-DET9-OLAGERF                  
336200     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET9-P-OLAGERF                
336300     MOVE ART-OLAGER(IX7)          TO  W001-DET9-OLAGERG                  
336400     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET9-P-OLAGERG                
336500     MOVE ART-OLAGER(IX8)          TO  W001-DET9-OLAGERH                  
336600     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET9-P-OLAGERH                
336700     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET9-TOT                      
336800     MOVE PSUM-PROC-OLAGER(PSUM-IX)                                       
336900                                   TO  W001-DET9-P-TOT                    
337000     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
337100     MOVE +1 TO W001-SKIP                                                 
337200     PERFORM S21-SKRIV-LISTA                                              
337300                                                                          
337400     MOVE ART-SLAGER(IX1)          TO  W001-DET10-SLAGERA                 
337500     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET10-P-SLAGERA               
337600     MOVE ART-SLAGER(IX2)          TO  W001-DET10-SLAGERB                 
337700     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET10-P-SLAGERB               
337800     MOVE ART-SLAGER(IX3)          TO  W001-DET10-SLAGERC                 
337900     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET10-P-SLAGERC               
338000     MOVE ART-SLAGER(IX4)          TO  W001-DET10-SLAGERD                 
338100     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET10-P-SLAGERD               
338200     MOVE ART-SLAGER(IX5)          TO  W001-DET10-SLAGERE                 
338300     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET10-P-SLAGERE               
338400     MOVE ART-SLAGER(IX6)          TO  W001-DET10-SLAGERF                 
338500     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET10-P-SLAGERF               
338600     MOVE ART-SLAGER(IX7)          TO  W001-DET10-SLAGERG                 
338700     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET10-P-SLAGERG               
338800     MOVE ART-SLAGER(IX8)          TO  W001-DET10-SLAGERH                 
338900     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET10-P-SLAGERH               
339000     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET10-TOT                     
339100     MOVE PSUM-PROC-SLAGER(PSUM-IX)                                       
339200                                   TO  W001-DET10-P-TOT                   
339300     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
339400     MOVE +1 TO W001-SKIP                                                 
339500     PERFORM S21-SKRIV-LISTA                                              
339600                                                                          
339700     MOVE ART-MLAGER(IX1)          TO  W001-DET11-MLAGERA                 
339800     MOVE ART-PROC-MLAGER(IX1)     TO  W001-DET11-P-MLAGERA               
339900     MOVE ART-MLAGER(IX2)          TO  W001-DET11-MLAGERB                 
340000     MOVE ART-PROC-MLAGER(IX2)     TO  W001-DET11-P-MLAGERB               
340100     MOVE ART-MLAGER(IX3)          TO  W001-DET11-MLAGERC                 
340200     MOVE ART-PROC-MLAGER(IX3)     TO  W001-DET11-P-MLAGERC               
340300     MOVE ART-MLAGER(IX4)          TO  W001-DET11-MLAGERD                 
340400     MOVE ART-PROC-MLAGER(IX4)     TO  W001-DET11-P-MLAGERD               
340500     MOVE ART-MLAGER(IX5)          TO  W001-DET11-MLAGERE                 
340600     MOVE ART-PROC-MLAGER(IX5)     TO  W001-DET11-P-MLAGERE               
340700     MOVE ART-MLAGER(IX6)          TO  W001-DET11-MLAGERF                 
340800     MOVE ART-PROC-MLAGER(IX6)     TO  W001-DET11-P-MLAGERF               
340900     MOVE ART-MLAGER(IX7)          TO  W001-DET11-MLAGERG                 
341000     MOVE ART-PROC-MLAGER(IX7)     TO  W001-DET11-P-MLAGERG               
341100     MOVE ART-MLAGER(IX8)          TO  W001-DET11-MLAGERH                 
341200     MOVE ART-PROC-MLAGER(IX8)     TO  W001-DET11-P-MLAGERH               
341300     MOVE PSUM-MLAGER(PSUM-IX)     TO  W001-DET11-TOT                     
341400     MOVE PSUM-PROC-MLAGER(PSUM-IX)                                       
341500                                   TO  W001-DET11-P-TOT                   
341600     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
341700     MOVE +1 TO W001-SKIP                                                 
341800     PERFORM S21-SKRIV-LISTA                                              
341900                                                                          
342000     MOVE ART-KVOT(IX1)            TO  W001-DET12-KVOTA                   
342100     MOVE ART-PROC-KVOT(IX1)       TO  W001-DET12-P-KVOTA                 
342200     MOVE ART-KVOT(IX2)            TO  W001-DET12-KVOTB                   
342300     MOVE ART-PROC-KVOT(IX2)       TO  W001-DET12-P-KVOTB                 
342400     MOVE ART-KVOT(IX3)            TO  W001-DET12-KVOTC                   
342500     MOVE ART-PROC-KVOT(IX3)       TO  W001-DET12-P-KVOTC                 
342600     MOVE ART-KVOT(IX4)            TO  W001-DET12-KVOTD                   
342700     MOVE ART-PROC-KVOT(IX4)       TO  W001-DET12-P-KVOTD                 
342800     MOVE ART-KVOT(IX5)            TO  W001-DET12-KVOTE                   
342900     MOVE ART-PROC-KVOT(IX5)       TO  W001-DET12-P-KVOTE                 
343000     MOVE ART-KVOT(IX6)            TO  W001-DET12-KVOTF                   
343100     MOVE ART-PROC-KVOT(IX6)       TO  W001-DET12-P-KVOTF                 
343200     MOVE ART-KVOT(IX7)            TO  W001-DET12-KVOTG                   
343300     MOVE ART-PROC-KVOT(IX7)       TO  W001-DET12-P-KVOTG                 
343400     MOVE ART-KVOT(IX8)            TO  W001-DET12-KVOTH                   
343500     MOVE ART-PROC-KVOT(IX8)       TO  W001-DET12-P-KVOTH                 
343600     MOVE PSUM-KVOT(PSUM-IX)       TO  W001-DET12-TOT                     
343700     MOVE PSUM-PROC-KVOT(PSUM-IX)  TO  W001-DET12-P-TOT                   
343800     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
343900     MOVE +1 TO W001-SKIP                                                 
344000     PERFORM S21-SKRIV-LISTA                                              
344100                                                                          
344200     MOVE ART-SPLIT(IX1)           TO  W001-DET13-SPLITA                  
344300     MOVE ART-SPLIT(IX2)           TO  W001-DET13-SPLITB                  
344400     MOVE ART-SPLIT(IX3)           TO  W001-DET13-SPLITC                  
344500     MOVE ART-SPLIT(IX4)           TO  W001-DET13-SPLITD                  
344600     MOVE ART-SPLIT(IX5)           TO  W001-DET13-SPLITE                  
344700     MOVE ART-SPLIT(IX6)           TO  W001-DET13-SPLITF                  
344800     MOVE ART-SPLIT(IX7)           TO  W001-DET13-SPLITG                  
344900     MOVE ART-SPLIT(IX8)           TO  W001-DET13-SPLITH                  
345000     MOVE PSUM-SPLIT(PSUM-IX)      TO  W001-DET13-TOT                     
345100     MOVE ZERO                     TO  W001-DET13-P-TOT                   
345200     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
345300     MOVE +1 TO W001-SKIP                                                 
345400     PERFORM S21-SKRIV-LISTA                                              
345500                                                                          
345600     MOVE ART-OMSHAST-DISP(IX1)    TO  W001-DET14-OMSHASTA                
345700     MOVE ART-OMSHAST-PROC-D(IX1)  TO  W001-DET14-P-OMSHASTA              
345800     MOVE ART-OMSHAST-DISP(IX2)    TO  W001-DET14-OMSHASTB                
345900     MOVE ART-OMSHAST-PROC-D(IX2)  TO  W001-DET14-P-OMSHASTB              
346000     MOVE ART-OMSHAST-DISP(IX3)    TO  W001-DET14-OMSHASTC                
346100     MOVE ART-OMSHAST-PROC-D(IX3)  TO  W001-DET14-P-OMSHASTC              
346200     MOVE ART-OMSHAST-DISP(IX4)    TO  W001-DET14-OMSHASTD                
346300     MOVE ART-OMSHAST-PROC-D(IX4)  TO  W001-DET14-P-OMSHASTD              
346400     MOVE ART-OMSHAST-DISP(IX5)    TO  W001-DET14-OMSHASTE                
346500     MOVE ART-OMSHAST-PROC-D(IX5)  TO  W001-DET14-P-OMSHASTE              
346600     MOVE ART-OMSHAST-DISP(IX6)    TO  W001-DET14-OMSHASTF                
346700     MOVE ART-OMSHAST-PROC-D(IX6)  TO  W001-DET14-P-OMSHASTF              
346800     MOVE ART-OMSHAST-DISP(IX7)    TO  W001-DET14-OMSHASTG                
346900     MOVE ART-OMSHAST-PROC-D(IX7)  TO  W001-DET14-P-OMSHASTG              
347000     MOVE ART-OMSHAST-DISP(IX8)    TO  W001-DET14-OMSHASTH                
347100     MOVE ART-OMSHAST-PROC-D(IX8)  TO  W001-DET14-P-OMSHASTH              
347200     MOVE PSUM-OMSHAST-DISP(PSUM-IX) TO W001-DET14-TOT                    
347300     MOVE PSUM-OMSHAST-PROC-D(PSUM-IX)                                    
347400                                     TO W001-DET14-P-TOT                  
347500     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
347600     MOVE +1 TO W001-SKIP                                                 
347700     PERFORM S21-SKRIV-LISTA                                              
347800                                                                          
347900     MOVE ART-OMSHAST-LS(IX1)      TO  W001-DET15-OMSHASTA                
348000     MOVE ART-OMSHAST-PROC-LS(IX1) TO  W001-DET15-P-OMSHASTA              
348100     MOVE ART-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB                
348200     MOVE ART-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB              
348300     MOVE ART-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC                
348400     MOVE ART-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC              
348500     MOVE ART-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD                
348600     MOVE ART-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD              
348700     MOVE ART-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE                
348800     MOVE ART-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE              
348900     MOVE ART-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF                
349000     MOVE ART-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF              
349100     MOVE ART-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG                
349200     MOVE ART-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG              
349300     MOVE ART-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH                
349400     MOVE ART-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH              
349500     MOVE PSUM-OMSHAST-LS(PSUM-IX) TO  W001-DET15-TOT                     
349600     MOVE PSUM-OMSHAST-PROC-LS(PSUM-IX)                                   
349700                                     TO W001-DET15-P-TOT                  
349800     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
349900     MOVE +1 TO W001-SKIP                                                 
350000     PERFORM S21-SKRIV-LISTA                                              
350100                                                                          
350200     MOVE ART-SERVG-BTO(IX1)       TO  W001-DET16-SERVG-BTOA              
350300     MOVE ART-SERVG-BTO(IX2)       TO  W001-DET16-SERVG-BTOB              
350400     MOVE ART-SERVG-BTO(IX3)       TO  W001-DET16-SERVG-BTOC              
350500     MOVE ART-SERVG-BTO(IX4)       TO  W001-DET16-SERVG-BTOD              
350600     MOVE ART-SERVG-BTO(IX5)       TO  W001-DET16-SERVG-BTOE              
350700     MOVE ART-SERVG-BTO(IX6)       TO  W001-DET16-SERVG-BTOF              
350800     MOVE ART-SERVG-BTO(IX7)       TO  W001-DET16-SERVG-BTOG              
350900     MOVE ART-SERVG-BTO(IX8)       TO  W001-DET16-SERVG-BTOH              
351000     MOVE PSUM-SERVG-BTO (PSUM-IX) TO  W001-DET16-TOT                     
351100     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
351200     MOVE +1 TO W001-SKIP                                                 
351300     PERFORM S21-SKRIV-LISTA                                              
351400                                                                          
351500     MOVE ART-SERVG-NTO(IX1)       TO  W001-DET17-SERVG-NTOA              
351600     MOVE ART-SERVG-NTO(IX2)       TO  W001-DET17-SERVG-NTOB              
351700     MOVE ART-SERVG-NTO(IX3)       TO  W001-DET17-SERVG-NTOC              
351800     MOVE ART-SERVG-NTO(IX4)       TO  W001-DET17-SERVG-NTOD              
351900     MOVE ART-SERVG-NTO(IX5)       TO  W001-DET17-SERVG-NTOE              
352000     MOVE ART-SERVG-NTO(IX6)       TO  W001-DET17-SERVG-NTOF              
352100     MOVE ART-SERVG-NTO(IX7)       TO  W001-DET17-SERVG-NTOG              
352200     MOVE ART-SERVG-NTO(IX8)       TO  W001-DET17-SERVG-NTOH              
352300     MOVE PSUM-SERVG-NTO(PSUM-IX)  TO  W001-DET17-TOT                     
352400     MOVE ZERO                     TO  W001-DET17-P-TOT                   
352500     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
352600     MOVE +1 TO W001-SKIP                                                 
352700     PERFORM S21-SKRIV-LISTA                                              
352800     .                                                                    
352900     EJECT                                                                
353000 CB-FLYTTA-SKRIV-TOT SECTION.                                             
353100                                                                          
353200     MOVE FSUM-KVANT-AKT(IX1)      TO  W001-DET1-KVANTA                   
353300     MOVE FSUM-PROC-KVANT-A(IX1)   TO  W001-DET1-P-KVANTA                 
353400     MOVE FSUM-KVANT-AKT(IX2)      TO  W001-DET1-KVANTB                   
353500     MOVE FSUM-PROC-KVANT-A(IX2)   TO  W001-DET1-P-KVANTB                 
353600     MOVE FSUM-KVANT-AKT(IX3)      TO  W001-DET1-KVANTC                   
353700     MOVE FSUM-PROC-KVANT-A(IX3)   TO  W001-DET1-P-KVANTC                 
353800     MOVE FSUM-KVANT-AKT(IX4)      TO  W001-DET1-KVANTD                   
353900     MOVE FSUM-PROC-KVANT-A(IX4)   TO  W001-DET1-P-KVANTD                 
354000     MOVE FSUM-KVANT-AKT(IX5)      TO  W001-DET1-KVANTE                   
354100     MOVE FSUM-PROC-KVANT-A(IX5)   TO  W001-DET1-P-KVANTE                 
354200     MOVE FSUM-KVANT-AKT(IX6)      TO  W001-DET1-KVANTF                   
354300     MOVE FSUM-PROC-KVANT-A(IX6)   TO  W001-DET1-P-KVANTF                 
354400     MOVE FSUM-KVANT-AKT(IX7)      TO  W001-DET1-KVANTG                   
354500     MOVE FSUM-PROC-KVANT-A(IX7)   TO  W001-DET1-P-KVANTG                 
354600     MOVE FSUM-KVANT-AKT(IX8)      TO  W001-DET1-KVANTH                   
354700     MOVE FSUM-PROC-KVANT-A(IX8)   TO  W001-DET1-P-KVANTH                 
354800     MOVE TOT-KVANT-AKT            TO  W001-DET1-TOT                      
354900     MOVE ZERO                     TO  W001-DET1-P-TOT                    
355000     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
355100     MOVE +2 TO W001-SKIP                                                 
355200     PERFORM S21-SKRIV-LISTA                                              
355300                                                                          
355400     MOVE FSUM-KVANT-PAS(IX1)      TO  W001-DET2-KVANTA                   
355500     MOVE FSUM-PROC-KVANT-P(IX1)   TO  W001-DET2-P-KVANTA                 
355600     MOVE FSUM-KVANT-PAS(IX2)      TO  W001-DET2-KVANTB                   
355700     MOVE FSUM-PROC-KVANT-P(IX2)   TO  W001-DET2-P-KVANTB                 
355800     MOVE FSUM-KVANT-PAS(IX3)      TO  W001-DET2-KVANTC                   
355900     MOVE FSUM-PROC-KVANT-P(IX3)   TO  W001-DET2-P-KVANTC                 
356000     MOVE FSUM-KVANT-PAS(IX4)      TO  W001-DET2-KVANTD                   
356100     MOVE FSUM-PROC-KVANT-P(IX4)   TO  W001-DET2-P-KVANTD                 
356200     MOVE FSUM-KVANT-PAS(IX5)      TO  W001-DET2-KVANTE                   
356300     MOVE FSUM-PROC-KVANT-P(IX5)   TO  W001-DET2-P-KVANTE                 
356400     MOVE FSUM-KVANT-PAS(IX6)      TO  W001-DET2-KVANTF                   
356500     MOVE FSUM-PROC-KVANT-P(IX6)   TO  W001-DET2-P-KVANTF                 
356600     MOVE FSUM-KVANT-PAS(IX7)      TO  W001-DET2-KVANTG                   
356700     MOVE FSUM-PROC-KVANT-P(IX7)   TO  W001-DET2-P-KVANTG                 
356800     MOVE FSUM-KVANT-PAS(IX8)      TO  W001-DET2-KVANTH                   
356900     MOVE FSUM-PROC-KVANT-P(IX8)   TO  W001-DET2-P-KVANTH                 
357000     MOVE TOT-KVANT-PAS            TO  W001-DET2-TOT                      
357100     MOVE ZERO                     TO  W001-DET2-P-TOT                    
357200     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
357300     MOVE +1 TO W001-SKIP                                                 
357400     PERFORM S21-SKRIV-LISTA                                              
357500                                                                          
357600     MOVE FSUM-KVDISP-AKT(IX1)     TO  W001-DET3-DLAGERA                  
357700     MOVE FSUM-PROC-KVDISP-A(IX1)  TO  W001-DET3-P-DLAGERA                
357800     MOVE FSUM-KVDISP-AKT(IX2)     TO  W001-DET3-DLAGERB                  
357900     MOVE FSUM-PROC-KVDISP-A(IX2)  TO  W001-DET3-P-DLAGERB                
358000     MOVE FSUM-KVDISP-AKT(IX3)     TO  W001-DET3-DLAGERC                  
358100     MOVE FSUM-PROC-KVDISP-A(IX3)  TO  W001-DET3-P-DLAGERC                
358200     MOVE FSUM-KVDISP-AKT(IX4)     TO  W001-DET3-DLAGERD                  
358300     MOVE FSUM-PROC-KVDISP-A(IX4)  TO  W001-DET3-P-DLAGERD                
358400     MOVE FSUM-KVDISP-AKT(IX5)     TO  W001-DET3-DLAGERE                  
358500     MOVE FSUM-PROC-KVDISP-A(IX5)  TO  W001-DET3-P-DLAGERE                
358600     MOVE FSUM-KVDISP-AKT(IX6)     TO  W001-DET3-DLAGERF                  
358700     MOVE FSUM-PROC-KVDISP-A(IX6)  TO  W001-DET3-P-DLAGERF                
358800     MOVE FSUM-KVDISP-AKT(IX7)     TO  W001-DET3-DLAGERG                  
358900     MOVE FSUM-PROC-KVDISP-A(IX7)  TO  W001-DET3-P-DLAGERG                
359000     MOVE FSUM-KVDISP-AKT(IX8)     TO  W001-DET3-DLAGERH                  
359100     MOVE FSUM-PROC-KVDISP-A(IX8)  TO  W001-DET3-P-DLAGERH                
359200     MOVE TOT-KVDISP-AKT           TO  W001-DET3-TOT                      
359300     MOVE ZERO                     TO  W001-DET3-P-TOT                    
359400     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
359500     MOVE +1 TO W001-SKIP                                                 
359600     PERFORM S21-SKRIV-LISTA                                              
359700                                                                          
359800     MOVE FSUM-KVDISP-PAS(IX1)     TO  W001-DET4-DLAGERA                  
359900     MOVE FSUM-PROC-KVDISP-P(IX1)  TO  W001-DET4-P-DLAGERA                
360000     MOVE FSUM-KVDISP-PAS(IX2)     TO  W001-DET4-DLAGERB                  
360100     MOVE FSUM-PROC-KVDISP-P(IX2)  TO  W001-DET4-P-DLAGERB                
360200     MOVE FSUM-KVDISP-PAS(IX3)     TO  W001-DET4-DLAGERC                  
360300     MOVE FSUM-PROC-KVDISP-P(IX3)  TO  W001-DET4-P-DLAGERC                
360400     MOVE FSUM-KVDISP-PAS(IX4)     TO  W001-DET4-DLAGERD                  
360500     MOVE FSUM-PROC-KVDISP-P(IX4)  TO  W001-DET4-P-DLAGERD                
360600     MOVE FSUM-KVDISP-PAS(IX5)     TO  W001-DET4-DLAGERE                  
360700     MOVE FSUM-PROC-KVDISP-P(IX5)  TO  W001-DET4-P-DLAGERE                
360800     MOVE FSUM-KVDISP-PAS(IX6)     TO  W001-DET4-DLAGERF                  
360900     MOVE FSUM-PROC-KVDISP-P(IX6)  TO  W001-DET4-P-DLAGERF                
361000     MOVE FSUM-KVDISP-PAS(IX7)     TO  W001-DET4-DLAGERG                  
361100     MOVE FSUM-PROC-KVDISP-P(IX7)  TO  W001-DET4-P-DLAGERG                
361200     MOVE FSUM-KVDISP-PAS(IX8)     TO  W001-DET4-DLAGERH                  
361300     MOVE FSUM-PROC-KVDISP-P(IX8)  TO  W001-DET4-P-DLAGERH                
361400     MOVE TOT-KVDISP-PAS           TO  W001-DET4-TOT                      
361500     MOVE ZERO                     TO  W001-DET4-P-TOT                    
361600     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
361700     MOVE +1 TO W001-SKIP                                                 
361800     PERFORM S21-SKRIV-LISTA                                              
361900                                                                          
362000     MOVE FSUM-LS-AKT(IX1)         TO  W001-DET5-LLAGERA                  
362100     MOVE FSUM-PROC-LS-A(IX1)      TO  W001-DET5-P-LLAGERA                
362200     MOVE FSUM-LS-AKT(IX2)         TO  W001-DET5-LLAGERB                  
362300     MOVE FSUM-PROC-LS-A(IX2)      TO  W001-DET5-P-LLAGERB                
362400     MOVE FSUM-LS-AKT(IX3)         TO  W001-DET5-LLAGERC                  
362500     MOVE FSUM-PROC-LS-A(IX3)      TO  W001-DET5-P-LLAGERC                
362600     MOVE FSUM-LS-AKT(IX4)         TO  W001-DET5-LLAGERD                  
362700     MOVE FSUM-PROC-LS-A(IX4)      TO  W001-DET5-P-LLAGERD                
362800     MOVE FSUM-LS-AKT(IX5)         TO  W001-DET5-LLAGERE                  
362900     MOVE FSUM-PROC-LS-A(IX5)      TO  W001-DET5-P-LLAGERE                
363000     MOVE FSUM-LS-AKT(IX6)         TO  W001-DET5-LLAGERF                  
363100     MOVE FSUM-PROC-LS-A(IX6)      TO  W001-DET5-P-LLAGERF                
363200     MOVE FSUM-LS-AKT(IX7)         TO  W001-DET5-LLAGERG                  
363300     MOVE FSUM-PROC-LS-A(IX7)      TO  W001-DET5-P-LLAGERG                
363400     MOVE FSUM-LS-AKT(IX8)         TO  W001-DET5-LLAGERH                  
363500     MOVE FSUM-PROC-LS-A(IX8)      TO  W001-DET5-P-LLAGERH                
363600     MOVE TOT-LS-AKT               TO  W001-DET5-TOT                      
363700     MOVE ZERO                     TO  W001-DET5-P-TOT                    
363800     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
363900     MOVE +1 TO W001-SKIP                                                 
364000     PERFORM S21-SKRIV-LISTA                                              
364100                                                                          
364200     MOVE FSUM-LS-PAS(IX1)         TO  W001-DET6-LLAGERA                  
364300     MOVE FSUM-PROC-LS-P(IX1)      TO  W001-DET6-P-LLAGERA                
364400     MOVE FSUM-LS-PAS(IX2)         TO  W001-DET6-LLAGERB                  
364500     MOVE FSUM-PROC-LS-P(IX2)      TO  W001-DET6-P-LLAGERB                
364600     MOVE FSUM-LS-PAS(IX3)         TO  W001-DET6-LLAGERC                  
364700     MOVE FSUM-PROC-LS-P(IX3)      TO  W001-DET6-P-LLAGERC                
364800     MOVE FSUM-LS-PAS(IX4)         TO  W001-DET6-LLAGERD                  
364900     MOVE FSUM-PROC-LS-P(IX4)      TO  W001-DET6-P-LLAGERD                
365000     MOVE FSUM-LS-PAS(IX5)         TO  W001-DET6-LLAGERE                  
365100     MOVE FSUM-PROC-LS-P(IX5)      TO  W001-DET6-P-LLAGERE                
365200     MOVE FSUM-LS-PAS(IX6)         TO  W001-DET6-LLAGERF                  
365300     MOVE FSUM-PROC-LS-P(IX6)      TO  W001-DET6-P-LLAGERF                
365400     MOVE FSUM-LS-PAS(IX7)         TO  W001-DET6-LLAGERG                  
365500     MOVE FSUM-PROC-LS-P(IX7)      TO  W001-DET6-P-LLAGERG                
365600     MOVE FSUM-LS-PAS(IX8)         TO  W001-DET6-LLAGERH                  
365700     MOVE FSUM-PROC-LS-P(IX8)      TO  W001-DET6-P-LLAGERH                
365800     MOVE TOT-LS-PAS               TO  W001-DET6-TOT                      
365900     MOVE ZERO                     TO  W001-DET6-P-TOT                    
366000     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
366100     MOVE +1 TO W001-SKIP                                                 
366200     PERFORM S21-SKRIV-LISTA                                              
366300                                                                          
366400     MOVE FSUM-AK-AKT(IX1)         TO  W001-DET7-ALAGERA                  
366500     MOVE FSUM-PROC-AK-A(IX1)      TO  W001-DET7-P-ALAGERA                
366600     MOVE FSUM-AK-AKT(IX2)         TO  W001-DET7-ALAGERB                  
366700     MOVE FSUM-PROC-AK-A(IX2)      TO  W001-DET7-P-ALAGERB                
366800     MOVE FSUM-AK-AKT(IX3)         TO  W001-DET7-ALAGERC                  
366900     MOVE FSUM-PROC-AK-A(IX3)      TO  W001-DET7-P-ALAGERC                
367000     MOVE FSUM-AK-AKT(IX4)         TO  W001-DET7-ALAGERD                  
367100     MOVE FSUM-PROC-AK-A(IX4)      TO  W001-DET7-P-ALAGERD                
367200     MOVE FSUM-AK-AKT(IX5)         TO  W001-DET7-ALAGERE                  
367300     MOVE FSUM-PROC-AK-A(IX5)      TO  W001-DET7-P-ALAGERE                
367400     MOVE FSUM-AK-AKT(IX6)         TO  W001-DET7-ALAGERF                  
367500     MOVE FSUM-PROC-AK-A(IX6)      TO  W001-DET7-P-ALAGERF                
367600     MOVE FSUM-AK-AKT(IX7)         TO  W001-DET7-ALAGERG                  
367700     MOVE FSUM-PROC-AK-A(IX7)      TO  W001-DET7-P-ALAGERG                
367800     MOVE FSUM-AK-AKT(IX8)         TO  W001-DET7-ALAGERH                  
367900     MOVE FSUM-PROC-AK-A(IX8)      TO  W001-DET7-P-ALAGERH                
368000     MOVE TOT-AK-AKT               TO  W001-DET7-TOT                      
368100     MOVE ZERO                     TO  W001-DET7-P-TOT                    
368200     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
368300     MOVE +1 TO W001-SKIP                                                 
368400     PERFORM S21-SKRIV-LISTA                                              
368500                                                                          
368600     MOVE FSUM-AK-PAS(IX1)         TO  W001-DET8-ALAGERA                  
368700     MOVE FSUM-PROC-AK-P(IX1)      TO  W001-DET8-P-ALAGERA                
368800     MOVE FSUM-AK-PAS(IX2)         TO  W001-DET8-ALAGERB                  
368900     MOVE FSUM-PROC-AK-P(IX2)      TO  W001-DET8-P-ALAGERB                
369000     MOVE FSUM-AK-PAS(IX3)         TO  W001-DET8-ALAGERC                  
369100     MOVE FSUM-PROC-AK-P(IX3)      TO  W001-DET8-P-ALAGERC                
369200     MOVE FSUM-AK-PAS(IX4)         TO  W001-DET8-ALAGERD                  
369300     MOVE FSUM-PROC-AK-P(IX4)      TO  W001-DET8-P-ALAGERD                
369400     MOVE FSUM-AK-PAS(IX5)         TO  W001-DET8-ALAGERE                  
369500     MOVE FSUM-PROC-AK-P(IX5)      TO  W001-DET8-P-ALAGERE                
369600     MOVE FSUM-AK-PAS(IX6)         TO  W001-DET8-ALAGERF                  
369700     MOVE FSUM-PROC-AK-P(IX6)      TO  W001-DET8-P-ALAGERF                
369800     MOVE FSUM-AK-PAS(IX7)         TO  W001-DET8-ALAGERG                  
369900     MOVE FSUM-PROC-AK-P(IX7)      TO  W001-DET8-P-ALAGERG                
370000     MOVE FSUM-AK-PAS(IX8)         TO  W001-DET8-ALAGERH                  
370100     MOVE FSUM-PROC-AK-P(IX8)      TO  W001-DET8-P-ALAGERH                
370200     MOVE TOT-AK-PAS               TO  W001-DET8-TOT                      
370300     MOVE ZERO                     TO  W001-DET8-P-TOT                    
370400     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
370500     MOVE +1 TO W001-SKIP                                                 
370600     PERFORM S21-SKRIV-LISTA                                              
370700                                                                          
370800     MOVE FSUM-OLAGER(IX1)         TO  W001-DET9-OLAGERA                  
370900     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET9-P-OLAGERA                
371000     MOVE FSUM-OLAGER(IX2)         TO  W001-DET9-OLAGERB                  
371100     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET9-P-OLAGERB                
371200     MOVE FSUM-OLAGER(IX3)         TO  W001-DET9-OLAGERC                  
371300     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET9-P-OLAGERC                
371400     MOVE FSUM-OLAGER(IX4)         TO  W001-DET9-OLAGERD                  
371500     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET9-P-OLAGERD                
371600     MOVE FSUM-OLAGER(IX5)         TO  W001-DET9-OLAGERE                  
371700     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET9-P-OLAGERE                
371800     MOVE FSUM-OLAGER(IX6)         TO  W001-DET9-OLAGERF                  
371900     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET9-P-OLAGERF                
372000     MOVE FSUM-OLAGER(IX7)         TO  W001-DET9-OLAGERG                  
372100     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET9-P-OLAGERG                
372200     MOVE FSUM-OLAGER(IX8)         TO  W001-DET9-OLAGERH                  
372300     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET9-P-OLAGERH                
372400     MOVE TOT-OLAGER               TO  W001-DET9-TOT                      
372500     MOVE TOT-PROC-OLAGER          TO  W001-DET9-P-TOT                    
372600     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
372700     MOVE +1 TO W001-SKIP                                                 
372800     PERFORM S21-SKRIV-LISTA                                              
372900                                                                          
373000     MOVE FSUM-SLAGER(IX1)         TO  W001-DET10-SLAGERA                 
373100     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET10-P-SLAGERA               
373200     MOVE FSUM-SLAGER(IX2)         TO  W001-DET10-SLAGERB                 
373300     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET10-P-SLAGERB               
373400     MOVE FSUM-SLAGER(IX3)         TO  W001-DET10-SLAGERC                 
373500     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET10-P-SLAGERC               
373600     MOVE FSUM-SLAGER(IX4)         TO  W001-DET10-SLAGERD                 
373700     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET10-P-SLAGERD               
373800     MOVE FSUM-SLAGER(IX5)         TO  W001-DET10-SLAGERE                 
373900     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET10-P-SLAGERE               
374000     MOVE FSUM-SLAGER(IX6)         TO  W001-DET10-SLAGERF                 
374100     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET10-P-SLAGERF               
374200     MOVE FSUM-SLAGER(IX7)         TO  W001-DET10-SLAGERG                 
374300     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET10-P-SLAGERG               
374400     MOVE FSUM-SLAGER(IX8)         TO  W001-DET10-SLAGERH                 
374500     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET10-P-SLAGERH               
374600     MOVE TOT-SLAGER               TO  W001-DET10-TOT                     
374700     MOVE TOT-PROC-SLAGER          TO  W001-DET10-P-TOT                   
374800     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
374900     MOVE +1 TO W001-SKIP                                                 
375000     PERFORM S21-SKRIV-LISTA                                              
375100                                                                          
375200     MOVE FSUM-MLAGER(IX1)         TO  W001-DET11-MLAGERA                 
375300     MOVE FSUM-PROC-MLAGER(IX1)    TO  W001-DET11-P-MLAGERA               
375400     MOVE FSUM-MLAGER(IX2)         TO  W001-DET11-MLAGERB                 
375500     MOVE FSUM-PROC-MLAGER(IX2)    TO  W001-DET11-P-MLAGERB               
375600     MOVE FSUM-MLAGER(IX3)         TO  W001-DET11-MLAGERC                 
375700     MOVE FSUM-PROC-MLAGER(IX3)    TO  W001-DET11-P-MLAGERC               
375800     MOVE FSUM-MLAGER(IX4)         TO  W001-DET11-MLAGERD                 
375900     MOVE FSUM-PROC-MLAGER(IX4)    TO  W001-DET11-P-MLAGERD               
376000     MOVE FSUM-MLAGER(IX5)         TO  W001-DET11-MLAGERE                 
376100     MOVE FSUM-PROC-MLAGER(IX5)    TO  W001-DET11-P-MLAGERE               
376200     MOVE FSUM-MLAGER(IX6)         TO  W001-DET11-MLAGERF                 
376300     MOVE FSUM-PROC-MLAGER(IX6)    TO  W001-DET11-P-MLAGERF               
376400     MOVE FSUM-MLAGER(IX7)         TO  W001-DET11-MLAGERG                 
376500     MOVE FSUM-PROC-MLAGER(IX7)    TO  W001-DET11-P-MLAGERG               
376600     MOVE FSUM-MLAGER(IX8)         TO  W001-DET11-MLAGERH                 
376700     MOVE FSUM-PROC-MLAGER(IX8)    TO  W001-DET11-P-MLAGERH               
376800     MOVE TOT-MLAGER               TO  W001-DET11-TOT                     
376900     MOVE TOT-PROC-MLAGER          TO  W001-DET11-P-TOT                   
377000     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
377100     MOVE +1 TO W001-SKIP                                                 
377200     PERFORM S21-SKRIV-LISTA                                              
377300                                                                          
377400     MOVE FSUM-KVOT(IX1)           TO  W001-DET12-KVOTA                   
377500     MOVE FSUM-PROC-KVOT(IX1)      TO  W001-DET12-P-KVOTA                 
377600     MOVE FSUM-KVOT(IX2)           TO  W001-DET12-KVOTB                   
377700     MOVE FSUM-PROC-KVOT(IX2)      TO  W001-DET12-P-KVOTB                 
377800     MOVE FSUM-KVOT(IX3)           TO  W001-DET12-KVOTC                   
377900     MOVE FSUM-PROC-KVOT(IX3)      TO  W001-DET12-P-KVOTC                 
378000     MOVE FSUM-KVOT(IX4)           TO  W001-DET12-KVOTD                   
378100     MOVE FSUM-PROC-KVOT(IX4)      TO  W001-DET12-P-KVOTD                 
378200     MOVE FSUM-KVOT(IX5)           TO  W001-DET12-KVOTE                   
378300     MOVE FSUM-PROC-KVOT(IX5)      TO  W001-DET12-P-KVOTE                 
378400     MOVE FSUM-KVOT(IX6)           TO  W001-DET12-KVOTF                   
378500     MOVE FSUM-PROC-KVOT(IX6)      TO  W001-DET12-P-KVOTF                 
378600     MOVE FSUM-KVOT(IX7)           TO  W001-DET12-KVOTG                   
378700     MOVE FSUM-PROC-KVOT(IX7)      TO  W001-DET12-P-KVOTG                 
378800     MOVE FSUM-KVOT(IX8)           TO  W001-DET12-KVOTH                   
378900     MOVE FSUM-PROC-KVOT(IX8)      TO  W001-DET12-P-KVOTH                 
379000     MOVE TOT-KVOT                 TO  W001-DET12-TOT                     
379100     MOVE TOT-PROC-MLAGER          TO  W001-DET12-P-TOT                   
379200     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
379300     MOVE +1 TO W001-SKIP                                                 
379400     PERFORM S21-SKRIV-LISTA                                              
379500                                                                          
379600     MOVE FSUM-SPLIT(IX1)          TO  W001-DET13-SPLITA                  
379700     MOVE FSUM-SPLIT(IX2)          TO  W001-DET13-SPLITB                  
379800     MOVE FSUM-SPLIT(IX3)          TO  W001-DET13-SPLITC                  
379900     MOVE FSUM-SPLIT(IX4)          TO  W001-DET13-SPLITD                  
380000     MOVE FSUM-SPLIT(IX5)          TO  W001-DET13-SPLITE                  
380100     MOVE FSUM-SPLIT(IX6)          TO  W001-DET13-SPLITF                  
380200     MOVE FSUM-SPLIT(IX7)          TO  W001-DET13-SPLITG                  
380300     MOVE FSUM-SPLIT(IX8)          TO  W001-DET13-SPLITH                  
380400     MOVE TOT-SPLIT                TO  W001-DET13-TOT                     
380500     MOVE ZERO                     TO  W001-DET13-P-TOT                   
380600     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
380700     MOVE +1 TO W001-SKIP                                                 
380800     PERFORM S21-SKRIV-LISTA                                              
380900                                                                          
381000     MOVE FSUM-OMSHAST-DISP(IX1)   TO  W001-DET14-OMSHASTA                
381100     MOVE FSUM-OMSHAST-PROC-D(IX1) TO  W001-DET14-P-OMSHASTA              
381200     MOVE FSUM-OMSHAST-DISP(IX2)   TO  W001-DET14-OMSHASTB                
381300     MOVE FSUM-OMSHAST-PROC-D(IX2) TO  W001-DET14-P-OMSHASTB              
381400     MOVE FSUM-OMSHAST-DISP(IX3)   TO  W001-DET14-OMSHASTC                
381500     MOVE FSUM-OMSHAST-PROC-D(IX3) TO  W001-DET14-P-OMSHASTC              
381600     MOVE FSUM-OMSHAST-DISP(IX4)   TO  W001-DET14-OMSHASTD                
381700     MOVE FSUM-OMSHAST-PROC-D(IX4) TO  W001-DET14-P-OMSHASTD              
381800     MOVE FSUM-OMSHAST-DISP(IX5)   TO  W001-DET14-OMSHASTE                
381900     MOVE FSUM-OMSHAST-PROC-D(IX5) TO  W001-DET14-P-OMSHASTE              
382000     MOVE FSUM-OMSHAST-DISP(IX6)   TO  W001-DET14-OMSHASTF                
382100     MOVE FSUM-OMSHAST-PROC-D(IX6) TO  W001-DET14-P-OMSHASTF              
382200     MOVE FSUM-OMSHAST-DISP(IX7)   TO  W001-DET14-OMSHASTG                
382300     MOVE FSUM-OMSHAST-PROC-D(IX7) TO  W001-DET14-P-OMSHASTG              
382400     MOVE FSUM-OMSHAST-DISP(IX8)   TO  W001-DET14-OMSHASTH                
382500     MOVE FSUM-OMSHAST-PROC-D(IX8) TO  W001-DET14-P-OMSHASTH              
382600     MOVE TOT-OMSHAST-DISP         TO  W001-DET14-TOT                     
382700     MOVE ZERO                     TO  W001-DET14-P-TOT                   
382800     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
382900     MOVE +1 TO W001-SKIP                                                 
383000     PERFORM S21-SKRIV-LISTA                                              
383100                                                                          
383200     MOVE FSUM-OMSHAST-LS(IX1)      TO W001-DET15-OMSHASTA                
383300     MOVE FSUM-OMSHAST-PROC-LS(IX1) TO W001-DET15-P-OMSHASTA              
383400     MOVE FSUM-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB               
383500     MOVE FSUM-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB             
383600     MOVE FSUM-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC               
383700     MOVE FSUM-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC             
383800     MOVE FSUM-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD               
383900     MOVE FSUM-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD             
384000     MOVE FSUM-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE               
384100     MOVE FSUM-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE             
384200     MOVE FSUM-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF               
384300     MOVE FSUM-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF             
384400     MOVE FSUM-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG               
384500     MOVE FSUM-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG             
384600     MOVE FSUM-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH               
384700     MOVE FSUM-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH             
384800     MOVE TOT-OMSHAST-LS            TO  W001-DET15-TOT                    
384900     MOVE ZERO                      TO  W001-DET15-P-TOT                  
385000     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
385100     MOVE +1 TO W001-SKIP                                                 
385200     PERFORM S21-SKRIV-LISTA                                              
385300                                                                          
385400     MOVE FSUM-SERVG-BTO(IX1)     TO  W001-DET16-SERVG-BTOA               
385500     MOVE FSUM-SERVG-BTO(IX2)     TO  W001-DET16-SERVG-BTOB               
385600     MOVE FSUM-SERVG-BTO(IX3)     TO  W001-DET16-SERVG-BTOC               
385700     MOVE FSUM-SERVG-BTO(IX4)     TO  W001-DET16-SERVG-BTOD               
385800     MOVE FSUM-SERVG-BTO(IX5)     TO  W001-DET16-SERVG-BTOE               
385900     MOVE FSUM-SERVG-BTO(IX6)     TO  W001-DET16-SERVG-BTOF               
386000     MOVE FSUM-SERVG-BTO(IX7)     TO  W001-DET16-SERVG-BTOG               
386100     MOVE FSUM-SERVG-BTO(IX8)     TO  W001-DET16-SERVG-BTOH               
386200     MOVE TOT-SERVG-BTO           TO  W001-DET16-TOT                      
386300     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
386400     MOVE +1 TO W001-SKIP                                                 
386500     PERFORM S21-SKRIV-LISTA                                              
386600                                                                          
386700     MOVE FSUM-SERVG-NTO(IX1)     TO  W001-DET17-SERVG-NTOA               
386800     MOVE FSUM-SERVG-NTO(IX2)     TO  W001-DET17-SERVG-NTOB               
386900     MOVE FSUM-SERVG-NTO(IX3)     TO  W001-DET17-SERVG-NTOC               
387000     MOVE FSUM-SERVG-NTO(IX4)     TO  W001-DET17-SERVG-NTOD               
387100     MOVE FSUM-SERVG-NTO(IX5)     TO  W001-DET17-SERVG-NTOE               
387200     MOVE FSUM-SERVG-NTO(IX6)     TO  W001-DET17-SERVG-NTOF               
387300     MOVE FSUM-SERVG-NTO(IX7)     TO  W001-DET17-SERVG-NTOG               
387400     MOVE FSUM-SERVG-NTO(IX8)     TO  W001-DET17-SERVG-NTOH               
387500     MOVE TOT-SERVG-NTO           TO  W001-DET17-TOT                      
387600     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
387700     MOVE +1 TO W001-SKIP                                                 
387800     PERFORM S21-SKRIV-LISTA                                              
387900     .                                                                    
388000     EJECT                                                                
388100 Z-FINIT SECTION.                                                         
388200                                                                          
388300     CLOSE W23195                                                         
388400           W23197-001                                                     
388500                                                                          
388600     MOVE 'S' TO POSTSUM-OPKOD                                            
388700     CALL POSTSUM USING POSTSUM-PARM                                      
388800     .                                                                    
388900     EJECT                                                                
389000 S01-LAS-W23195      SECTION.                                             
389100                                                                          
389200     READ W23195 INTO IN-AREA                                             
389300     AT END                                                               
389400         SET END-OF-W23195 TO TRUE                                        
389500                                                                          
389600     NOT AT END                                                           
389700        MOVE 'W23197'      TO POSTSUM-FDNAMN                              
389800        MOVE 'W23197D1'    TO POSTSUM-DDNAMN2                             
389900        MOVE SPACE         TO POSTSUM-TRANSTYP                            
390000        CALL POSTSUM USING POSTSUM-PARM                                   
390100     .                                                                    
390200     EJECT                                                                
390300 S10-HITTA-KVDLTID SECTION.                                               
390400                                                                          
390500                                                                          
390600     MOVE +1 TO IDDC-IX                                                   
390700     PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                               
390800                  (W-IDDC-REC  = WS-IDDC-B601 (IDDC-IX) AND               
390900                   W-IDDC-SEND = WS-IDDC-B616 (IDDC-IX))                  
391000                                                                          
391100        ADD +1 TO IDDC-IX                                                 
391200     END-PERFORM                                                          
391300     .                                                                    
391400     EJECT                                                                
391500 S21-SKRIV-LISTA SECTION.                                                 
391600                                                                          
391700     WRITE W23197-001-RAD FROM W001-RAD AFTER W001-SKIP                   
391800                                                                          
391900     MOVE SPACE TO W001-RAD                                               
392000     ADD  +1 TO W001-ANTAL-RADER                                          
392100     .                                                                    
392200     EJECT                                                                
392300 S21A-SKRIV-RUBRIKER SECTION.                                             
392400                                                                          
392500     ADD +1 TO W001-SIDRAKNARE                                            
392600     MOVE W001-SIDRAKNARE TO W001-SID                                     
392700     WRITE W23197-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
392800     WRITE W23197-001-RAD FROM W001-RUBRIK3 AFTER 2                       
392900     MOVE +2 TO W001-ANTAL-RADER                                          
393000     MOVE +2 TO W001-SKIP                                                 
393100     .                                                                    
393200* --- IMS SEKTIONER ---                                                   
393300                                                                          
393400 IMS-GET-WDB6      SECTION.                                               
393500                                                                          
393600     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B6                        
393700     MOVE '  GAGKGB'          TO GODK-STATUSKODER                         
393800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
393900     PERFORM IMS-STATUSKONTROLL                                           
394000     .                                                                    
394100     EJECT                                                                
394200 IMS-STATUSKONTROLL SECTION.                                              
394300                                                                          
394400     SET STATUS-IX TO 1                                                   
394500     SEARCH GODK-STATUS                                                   
394600       AT END                                                             
394700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
394800           DELIMITED BY SIZE INTO FELTEXT                                 
394900         DISPLAY FELTEXT                                                  
395000         CALL FELLOG                                                      
395100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
395200         CONTINUE                                                         
395300     END-SEARCH                                                           
395400     .                                                                    
