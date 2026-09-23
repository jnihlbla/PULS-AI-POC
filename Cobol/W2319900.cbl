000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2319900.                                                
000400*AUTHOR.         RAHUL JAIN.                                              
000500*DATE-WRITTEN.   11/11/11.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    THE PROGRAM IS A COPY OF W23197                                      
001100*               W23197 HANDLES US NDC                                     
001200*               W23199 HANDLES CHINA NDC AND CHINA LDC                    
001300*                                                                         
001400*    FUNCTION:                                                            
001500*      - READS FILE W23195/NDC AND LDC                                    
001600*      - CREATES REPORT FOR FOLLOW-UP REFILL TOTAL CHINA                  
001700*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200     EJECT                                                                
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INPUT FILE -                                               
002600     SELECT W23195                     ASSIGN TO W23199D1.                
002700*          --- REPORT                                                     
002800     SELECT W23199-001                 ASSIGN TO W23199D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W23195                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  -COPY W23195        -L.                                              
003900     SKIP3                                                                
004000 FD  W23199-001                                                           
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300     SKIP2                                                                
004400 01  W23199-001-RAD              PIC X(169).                              
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W2319900'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  SW-KVOI-TRAFF               PIC X       VALUE 'N'.                   
005400 77  SW-ARTIKEL-SAKNAS-WDK7      PIC X       VALUE 'N'.                   
005500 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
005700 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
005800 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
005900 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
006000 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
006200 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
006300 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  NDC-IX                      PIC 9(2)    VALUE ZERO COMP-3.           
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
009900 77  WS-FIXAD-SUMMA              PIC S9(16)      VALUE ZERO.              
010000                                                                          
010100 77  FELTEXT                     PIC X(80)       VALUE SPACE.             
010200                                                                          
010300 77  W23195-EOF-SW                    PIC X       VALUE 'N'.              
010400     88  END-OF-W23195                VALUE 'J'.                          
010500                                                                          
010600 01  DAGENS-DATUM                PIC 9(6).                                
010700 01  FILLER REDEFINES DAGENS-DATUM.                                       
010800     03  DAGENS-AAR              PIC 9(2).                                
010900     03  DAGENS-MAANAD           PIC 9(2).                                
011000     03  DAGENS-DAG              PIC 9(2).                                
011100                                                                          
011200 01  DAGENS-VECKA                PIC 9(4).                                
011300 01  FILLER REDEFINES DAGENS-VECKA.                                       
011400     03  D-VECKA-AAR             PIC 9(2).                                
011500     03  D-VECKA-VECKA           PIC 9(2).                                
011600                                                                          
011700 01  VECKOR.                                                              
011800     03  AAVVD                   PIC 9(5).                                
011900     03  FILLER REDEFINES AAVVD.                                          
012000         05  AAVV                PIC 9(4).                                
012100         05  D                   PIC 9(1).                                
012200                                                                          
012300     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
012400     EJECT                                                                
012500 01  ART-TABELL.                                                          
012600     03 ART-RAD OCCURS 72.                                                
012700        05  ART-KVANT-AKT        PIC S9(9)      VALUE ZERO COMP-3.        
012800        05  ART-KVANT-PAS        PIC S9(9)      VALUE ZERO COMP-3.        
012900        05  ART-PROC-KVANT-A     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013000        05  ART-PROC-KVANT-P     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013100        05  ART-KVDISP-AKT       PIC S9(11)     VALUE ZERO COMP-3.        
013200        05  ART-PROC-KVDISP-A    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013300        05  ART-KVDISP-PAS       PIC S9(11)     VALUE ZERO COMP-3.        
013400        05  ART-PROC-KVDISP-P    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013500        05  ART-LS-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
013600        05  ART-PROC-LS-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013700        05  ART-LS-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
013800        05  ART-PROC-LS-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013900        05  ART-AK-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
014000        05  ART-PROC-AK-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014100        05  ART-AK-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
014200        05  ART-PROC-AK-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014300        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
014400        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014500        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
014600        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014700        05  ART-MLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
014800        05  ART-PROC-MLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014900        05  ART-KVOT             PIC S9(9)      VALUE ZERO COMP-3.        
015000        05  ART-PROC-KVOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015100        05  ART-SPLIT            PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015200        05  ART-OMSHAST-DISP     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015300        05  ART-OMSHAST-PROC-D   PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015400        05  ART-OMSHAST-LS       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015500        05  ART-OMSHAST-PROC-LS  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015600        05  ART-SERVG-BTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015700        05  ART-SERVG-NTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015800*******  ARBETSFÄLT                                                       
015900        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
016000        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
016100        05  WS-ART-MLAGER        PIC S9(11)V9(2) VALUE ZERO.              
016200        05  WS-ART-LS-AKT        PIC S9(11)      VALUE ZERO.              
016300        05  WS-ART-LS-PAS        PIC S9(11)      VALUE ZERO.              
016400        05  WS-ART-LS-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
016500        05  WS-ART-LS-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
016600        05  WS-ART-KVLS-AKT      PIC S9(11)      VALUE ZERO.              
016700        05  WS-ART-KVLS-PAS      PIC S9(11)      VALUE ZERO.              
016800        05  WS-ART-KVDISP-AKT    PIC S9(11)      VALUE ZERO.              
016900        05  WS-ART-KVDISP-PAS    PIC S9(11)      VALUE ZERO.              
017000        05  WS-ART-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.              
017100        05  WS-ART-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.              
017200        05  WS-ART-KVOKS-AKT     PIC S9(11)      VALUE ZERO.              
017300        05  WS-ART-KVOKS-PAS     PIC S9(11)      VALUE ZERO.              
017400        05  WS-ART-OK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
017500        05  WS-ART-OK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
017600        05  WS-ART-KVAKS-AKT     PIC S9(11)      VALUE ZERO.              
017700        05  WS-ART-KVAKS-PAS     PIC S9(11)      VALUE ZERO.              
017800        05  WS-ART-AK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
017900        05  WS-ART-AK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
018000        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
018100        05  WS-ART-KVOI-AKT      PIC S9(9)      VALUE ZERO COMP-3.        
018200        05  WS-ART-KVOI-PAS      PIC S9(9)      VALUE ZERO COMP-3.        
018300        05  WS-ART-KVOI-TEO      PIC S9(9)      VALUE ZERO COMP-3.        
018400        05  WS-ART-KVOI-SAK      PIC S9(9)      VALUE ZERO COMP-3.        
018500        05  WS-ART-KVOI-CDC-AKT  PIC S9(9)      VALUE ZERO COMP-3.        
018600        05  WS-ART-KVOI-CDC-PAS  PIC S9(9)      VALUE ZERO COMP-3.        
018700        05  WS-ART-KVOI-CDC-TEO  PIC S9(9)      VALUE ZERO COMP-3.        
018800        05  WS-ART-KVOI-CDC-SAK  PIC S9(9)      VALUE ZERO COMP-3.        
018900        05  WS-ART-SUINKORD      PIC S9(16)V9(2)                          
019000                                                VALUE ZERO COMP-3.        
019100        05  WS-ART-SUFYSAVP      PIC S9(16)V9(2)                          
019200                                                VALUE ZERO COMP-3.        
019300        05  WS-ART-SUAVBRP       PIC S9(16)V9(2)                          
019400                                                VALUE ZERO COMP-3.        
019500        05  WS-ART-SULAGERB      PIC S9(16)V9(2)                          
019600                                                VALUE ZERO COMP-3.        
019700        05  WS-ART-SUSORTB       PIC S9(16)V9(2)                          
019800                                                VALUE ZERO COMP-3.        
019900     EJECT                                                                
020000 01  PSUM-TABELL.                                                         
020100     03 PSUM-RAD OCCURS 9.                                                
020200        05  PSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
020300        05  PSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
020400        05  PSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020500        05  PSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020600        05  PSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
020700        05  PSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
020800        05  PSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
020900        05  PSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021000        05  PSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
021100        05  PSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021200        05  PSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
021300        05  PSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021400        05  PSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
021500        05  PSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021600        05  PSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
021700        05  PSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021800        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
021900        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022000        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
022100        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022200        05  PSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
022300        05  PSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022400        05  PSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
022500        05  PSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022600        05  PSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022700        05  PSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022800        05  PSUM-OMSHAST-PROC-D  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
022900        05  PSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023000        05  PSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
023100        05  PSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023200        05  PSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023300*******  ARBETSFÄLT                                                       
023400        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
023500        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
023600        05  WS-PSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
023700        05  WS-PSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
023800        05  WS-PSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
023900        05  WS-PSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
024000        05  WS-PSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
024100        05  WS-PSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
024200        05  WS-PSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
024300        05  WS-PSUM-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.             
024400        05  WS-PSUM-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.             
024500        05  WS-PSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
024600        05  WS-PSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
024700        05  WS-PSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
024800        05  WS-PSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
024900        05  WS-PSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
025000        05  WS-PSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
025100        05  WS-PSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
025200        05  WS-PSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
025300        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
025400        05  WS-PSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
025500        05  WS-PSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
025600        05  WS-PSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
025700        05  WS-PSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
025800        05  WS-PSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
025900        05  WS-PSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
026000        05  WS-PSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
026100        05  WS-PSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
026200        05  WS-PSUM-SUINKORD     PIC S9(16)V9(2)                          
026300                                                VALUE ZERO COMP-3.        
026400        05  WS-PSUM-SUFYSAVP     PIC S9(16)V9(2)                          
026500                                                VALUE ZERO COMP-3.        
026600        05  WS-PSUM-SUAVBRP      PIC S9(16)V9(2)                          
026700                                                VALUE ZERO COMP-3.        
026800        05  WS-PSUM-SULAGERB     PIC S9(16)V9(2)                          
026900                                                VALUE ZERO COMP-3.        
027000        05  WS-PSUM-SUSORTB      PIC S9(16)V9(2)                          
027100                                                VALUE ZERO COMP-3.        
027200     EJECT                                                                
027300 01  FSUM-TABELL.                                                         
027400     03 FSUM-RAD OCCURS 8.                                                
027500        05  FSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
027600        05  FSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
027700        05  FSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027800        05  FSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027900        05  FSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
028000        05  FSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028100        05  FSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
028200        05  FSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028300        05  FSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
028400        05  FSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028500        05  FSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
028600        05  FSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028700        05  FSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
028800        05  FSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028900        05  FSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
029000        05  FSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029100        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029200        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029300        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029400        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029500        05  FSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029600        05  FSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029700        05  FSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
029800        05  FSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029900        05  FSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030000        05  FSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030100        05  FSUM-OMSHAST-PROC-D PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030200        05  FSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030300        05  FSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
030400        05  FSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030500        05  FSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
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
033500        05  WS-FSUM-SUINKORD     PIC S9(16)V9(2)                          
033600                                                VALUE ZERO COMP-3.        
033700        05  WS-FSUM-SUFYSAVP     PIC S9(16)V9(2)                          
033800                                                VALUE ZERO COMP-3.        
033900        05  WS-FSUM-SUAVBRP      PIC S9(16)V9(2)                          
034000                                                VALUE ZERO COMP-3.        
034100        05  WS-FSUM-SULAGERB     PIC S9(16)V9(2)                          
034200                                                VALUE ZERO COMP-3.        
034300        05  WS-FSUM-SUSORTB      PIC S9(16)V9(2)                          
034400                                                VALUE ZERO COMP-3.        
034500     EJECT                                                                
034600 01  TOTAL-RUTA.                                                          
034700     03  TOT-KVANT-AKT          PIC S9(9)      VALUE ZERO COMP-3.         
034800     03  TOT-KVANT-PAS          PIC S9(9)      VALUE ZERO COMP-3.         
034900     03  TOT-KVDISP-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
035000     03  TOT-KVDISP-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
035100     03  TOT-LS-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
035200     03  TOT-LS-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
035300     03  TOT-AK-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
035400     03  TOT-AK-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
035500     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035600     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035700     03  TOT-MLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035800     03  TOT-PROC-MLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035900     03  TOT-KVOT               PIC S9(9)      VALUE ZERO COMP-3.         
036000     03  TOT-PROC-KVOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036100     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
036200     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036300     03  TOT-SPLIT              PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036400     03  TOT-OMSHAST-DISP       PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036500     03  TOT-OMSHAST-LS         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036600     03  TOT-SERVG-BTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036700     03  TOT-SERVG-NTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036800*******  ARBETSFÄLT                                                       
036900     03  WS-TOT-SLAGER          PIC S9(11)V9(2) VALUE ZERO.               
037000     03  WS-TOT-OLAGER          PIC S9(11)V9(2) VALUE ZERO.               
037100     03  WS-TOT-MLAGER          PIC S9(11)V9(2) VALUE ZERO.               
037200     03  WS-TOT-LS-AKT          PIC S9(11)      VALUE ZERO.               
037300     03  WS-TOT-LS-PAS          PIC S9(11)      VALUE ZERO.               
037400     03  WS-TOT-LS-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
037500     03  WS-TOT-LS-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
037600     03  WS-TOT-KVDISP-AKT      PIC S9(11)      VALUE ZERO.               
037700     03  WS-TOT-KVDISP-PAS      PIC S9(11)      VALUE ZERO.               
037800     03  WS-TOT-KVDISP-PR-AKT   PIC S9(11)V9(2) VALUE ZERO.               
037900     03  WS-TOT-KVDISP-PR-PAS   PIC S9(11)V9(2) VALUE ZERO.               
038000     03  WS-TOT-KVOKS-AKT       PIC S9(11)      VALUE ZERO.               
038100     03  WS-TOT-KVOKS-PAS       PIC S9(11)      VALUE ZERO.               
038200     03  WS-TOT-OK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
038300     03  WS-TOT-OK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
038400     03  WS-TOT-KVAKS-AKT       PIC S9(11)      VALUE ZERO.               
038500     03  WS-TOT-KVAKS-PAS       PIC S9(11)      VALUE ZERO.               
038600     03  WS-TOT-AK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
038700     03  WS-TOT-AK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
038800     03  WS-TOT-KVOI            PIC S9(11)V9(2) VALUE ZERO.               
038900     03  WS-TOT-KVOI-AKT        PIC S9(9)       VALUE ZERO.               
039000     03  WS-TOT-KVOI-PAS        PIC S9(9)       VALUE ZERO.               
039100     03  WS-TOT-KVOI-TEO        PIC S9(9)       VALUE ZERO.               
039200     03  WS-TOT-KVOI-SAK        PIC S9(9)       VALUE ZERO.               
039300     03  WS-TOT-KVOI-CDC-AKT    PIC S9(9)       VALUE ZERO.               
039400     03  WS-TOT-KVOI-CDC-PAS    PIC S9(9)       VALUE ZERO.               
039500     03  WS-TOT-KVOI-CDC-TEO    PIC S9(9)       VALUE ZERO.               
039600     03  WS-TOT-KVOI-CDC-SAK    PIC S9(9)       VALUE ZERO.               
039700     03  WS-TOT-SUINKORD        PIC S9(16)V9(2)                           
039800                                                VALUE ZERO COMP-3.        
039900     03  WS-TOT-SUFYSAVP        PIC S9(16)V9(2)                           
040000                                                VALUE ZERO COMP-3.        
040100     03  WS-TOT-SUAVBRP         PIC S9(16)V9(2)                           
040200                                                VALUE ZERO COMP-3.        
040300     03  WS-TOT-SULAGERB        PIC S9(16)V9(2)                           
040400                                                VALUE ZERO COMP-3.        
040500     03  WS-TOT-SUSORTB         PIC S9(16)V9(2)                           
040600                                                VALUE ZERO COMP-3.        
040700     EJECT                                                                
040800*      --- VALID IDDC CODES                                               
040900*                                                                         
041000*01    -COPY WWDC99                                                       
041100                                                                          
041200 01  W-IDDC-SEND                 PIC X(2) VALUE SPACE.                    
041300 01  W-IDDC-REC                  PIC X(2) VALUE SPACE.                    
041400 01  W-IDDC                      PIC X(2) VALUE SPACE.                    
041500 01  W-IDLEVNR                   PIC X(5) VALUE SPACE.                    
041600 01  WS-IDDC-CDC                 PIC X(2) VALUE '11'.                     
041700                                                                          
041800 01  WS-DC-TABELL.                                                        
041900     03 DC-TABELL OCCURS 500.                                             
042000        05  WS-IDDC-B601         PIC X(2) VALUE SPACE.                    
042100        05  WS-IDDC-B616         PIC X(2) VALUE SPACE.                    
042200        05  WS-KVDLTID-TOT       PIC 9(3) VALUE ZERO.                     
042300 01  IDDC-IX                     PIC 9(3).                                
042400 01  IDDC-IX-MAX                 PIC 9(3) VALUE 500.                      
042500*                                                                         
042600 01  WS-IDLEV-TABELL.                                                     
042700     03 IDLEV-ROOT-TABELL OCCURS 500.                                     
042800        05  WS-IDDC-R            PIC X(2) VALUE SPACE.                    
042900        05  WS-IDLEVNR-R         PIC X(5) VALUE SPACE.                    
043000 01  IDLEV-IX                    PIC 9(3).                                
043100 01  IDLEV-IX-MAX                PIC 9(3) VALUE 500.                      
043200                                                                          
043300       EJECT                                                              
043400 01  DYNAMISKA-SUBPROGRAM.                                                
043500*                                                                         
043600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
043700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
043800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
043900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
044000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
044100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
044200                                                                          
044300     EJECT                                                                
044400 01  PARAM-TILL-DATKORT.                                                  
044500     03  PROG-ID                 PIC X(8)    VALUE 'W2319900'.            
044600     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
044700*03  -COPY WDATKORT                                                       
044800     EJECT                                                                
044900*03  -COPY WDATAREA                                                       
045000     EJECT                                                                
045100*    --- PARAMETRAR TILL POSTSUM                                          
045200*                                                                         
045300*01  -COPY W0005   -PRE  POSTSUM-                                         
045400     EJECT                                                                
045500 01  NDC-POST.                                                            
045600     03  FILLER             PIC X(2).                                     
045700     03  AKTUELLT-NDC       PIC X(2).                                     
045800     03  FILLER             PIC X(76).                                    
045900     EJECT                                                                
046000 01  IN-AREA-START               PIC X(24)   VALUE                        
046100                                 'IN-AREA-START    '.                     
046200*01  AREA  -COPY W23195     -PRE IN-                                      
046300     EJECT                                                                
046400 01  W001-AREA-START             PIC X(24)   VALUE                        
046500                                 'W001-AREA-START  '.                     
046600     SKIP2                                                                
046700 01  W001-HJALPAREOR.                                                     
046800*                                                                         
046900     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
047000     03  W001-ANTAL-RADER                                                 
047100                                 PIC 9(3)    VALUE 999.                   
047200     03  W001-MAX-RADER-PER-SIDA                                          
047300                                 PIC 9(3)    VALUE 63.                    
047400     03  W001-MAX-POSITIONER-PER-RAD                                      
047500                                 PIC 9(3)    VALUE 165.                   
047600     03  W001-LISTNR             PIC X(11)   VALUE SPACE.                 
047700     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
047800     SKIP2                                                                
047900 01  W001-RAD.                                                            
048000     03  FILLER                  PIC X(165)  VALUE SPACE.                 
048100     EJECT                                                                
048200 01  W001-RUBRIK1.                                                        
048300*                                                                         
048400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
048500     03  FILLER                  PIC X(18)                                
048600                              VALUE 'VOLVO CAR PARTS   '.                 
048700     03  W001-LISTID             PIC X(12)                                
048800                                 VALUE SPACE.                             
048900     03  FILLER                  PIC X(20)                                
049000             VALUE 'FOLLOW-UP REFILL    '.                                
049100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
049200     03  FILLER                  PIC X(4)    VALUE 'NDC '.                
049300     03  FILLER                  PIC X(2)    VALUE SPACE.                 
049400     03  W001-AKTUELLT-IDDC      PIC X(11)   VALUE SPACE.                 
049500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
049600     03  FILLER                  PIC X(6)    VALUE 'WEEK  '.              
049700     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
049800     03  FILLER                  PIC X(37)   VALUE SPACE.                 
049900     03  W001-DATUM              PIC XXBXXBXX.                            
050000     03  FILLER                  PIC X(6)    VALUE SPACE.                 
050100     03  FILLER                  PIC X(5)    VALUE 'PAGE '.               
050200     03  W001-SID                PIC Z(4)9.                               
050300     EJECT                                                                
050400 01  W001-RUBRIK3.                                                        
050500*                                                                         
050600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050700     03  FILLER                  PIC X(11)                                
050800                             VALUE 'PRICE CLASS'.                         
050900     03  FILLER                  PIC X(11)   VALUE SPACE.                 
051000     03  FILLER                  PIC X(7)    VALUE 'A      '.             
051100     03  FILLER                  PIC X(7)    VALUE SPACE.                 
051200     03  FILLER                  PIC X(8)    VALUE 'B       '.            
051300     03  FILLER                  PIC X(6)    VALUE SPACE.                 
051400     03  FILLER                  PIC X(8)    VALUE 'C       '.            
051500     03  FILLER                  PIC X(6)    VALUE SPACE.                 
051600     03  FILLER                  PIC X(9)    VALUE 'D        '.           
051700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
051800     03  FILLER                  PIC X(9)    VALUE 'E        '.           
051900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
052000     03  FILLER                  PIC X(10)   VALUE 'F         '.          
052100     03  FILLER                  PIC X(4)    VALUE SPACE.                 
052200     03  FILLER                  PIC X(10)   VALUE 'G         '.          
052300     03  FILLER                  PIC X(4)    VALUE SPACE.                 
052400     03  FILLER                  PIC X(10)   VALUE 'H         '.          
052500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
052600     03  FILLER                  PIC X(6)    VALUE 'TOTAL '.              
052700     EJECT                                                                
052800 01  W001-DETALJRAD-1.                                                    
052900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053000     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
053100     03  FILLER                  PIC X       VALUE SPACE.                 
053200     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  A'.        
053300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053400     03  W001-DET1-KVANTA        PIC Z(7)9.                               
053500     03  FILLER                  PIC X       VALUE SPACE.                 
053600     03  W001-DET1-P-KVANTA      PIC Z9.9.                                
053700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053800     03  W001-DET1-KVANTB        PIC Z(7)9.                               
053900     03  FILLER                  PIC X       VALUE SPACE.                 
054000     03  W001-DET1-P-KVANTB      PIC Z9.9.                                
054100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054200     03  W001-DET1-KVANTC        PIC Z(7)9.                               
054300     03  FILLER                  PIC X       VALUE SPACE.                 
054400     03  W001-DET1-P-KVANTC      PIC Z9.9.                                
054500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054600     03  W001-DET1-KVANTD        PIC Z(7)9.                               
054700     03  FILLER                  PIC X       VALUE SPACE.                 
054800     03  W001-DET1-P-KVANTD      PIC Z9.9.                                
054900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055000     03  W001-DET1-KVANTE        PIC Z(7)9.                               
055100     03  FILLER                  PIC X       VALUE SPACE.                 
055200     03  W001-DET1-P-KVANTE      PIC Z9.9.                                
055300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055400     03  W001-DET1-KVANTF        PIC Z(7)9.                               
055500     03  FILLER                  PIC X       VALUE SPACE.                 
055600     03  W001-DET1-P-KVANTF      PIC Z9.9.                                
055700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055800     03  W001-DET1-KVANTG        PIC Z(7)9.                               
055900     03  FILLER                  PIC X       VALUE SPACE.                 
056000     03  W001-DET1-P-KVANTG      PIC Z9.9.                                
056100     03  FILLER                  PIC X       VALUE SPACE.                 
056200     03  W001-DET1-KVANTH        PIC Z(7)9.                               
056300     03  FILLER                  PIC X       VALUE SPACE.                 
056400     03  W001-DET1-P-KVANTH      PIC Z9.9.                                
056500     03  FILLER                  PIC X       VALUE SPACE.                 
056600     03  W001-DET1-TOT           PIC Z(13)9.                              
056700     03  FILLER                  PIC X       VALUE SPACE.                 
056800     03  W001-DET1-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
056900     EJECT                                                                
057000 01  W001-DETALJRAD-2.                                                    
057100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057200     03  W001-DET2-PRISKLASS     PIC X       VALUE SPACE.                 
057300     03  FILLER                  PIC X       VALUE SPACE.                 
057400     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  P'.        
057500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057600     03  W001-DET2-KVANTA        PIC Z(7)9.                               
057700     03  FILLER                  PIC X       VALUE SPACE.                 
057800     03  W001-DET2-P-KVANTA      PIC Z9.9.                                
057900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058000     03  W001-DET2-KVANTB        PIC Z(7)9.                               
058100     03  FILLER                  PIC X       VALUE SPACE.                 
058200     03  W001-DET2-P-KVANTB      PIC Z9.9.                                
058300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058400     03  W001-DET2-KVANTC        PIC Z(7)9.                               
058500     03  FILLER                  PIC X       VALUE SPACE.                 
058600     03  W001-DET2-P-KVANTC      PIC Z9.9.                                
058700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058800     03  W001-DET2-KVANTD        PIC Z(7)9.                               
058900     03  FILLER                  PIC X       VALUE SPACE.                 
059000     03  W001-DET2-P-KVANTD      PIC Z9.9.                                
059100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059200     03  W001-DET2-KVANTE        PIC Z(7)9.                               
059300     03  FILLER                  PIC X       VALUE SPACE.                 
059400     03  W001-DET2-P-KVANTE      PIC Z9.9.                                
059500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059600     03  W001-DET2-KVANTF        PIC Z(7)9.                               
059700     03  FILLER                  PIC X       VALUE SPACE.                 
059800     03  W001-DET2-P-KVANTF      PIC Z9.9.                                
059900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060000     03  W001-DET2-KVANTG        PIC Z(7)9.                               
060100     03  FILLER                  PIC X       VALUE SPACE.                 
060200     03  W001-DET2-P-KVANTG      PIC Z9.9.                                
060300     03  FILLER                  PIC X       VALUE SPACE.                 
060400     03  W001-DET2-KVANTH        PIC Z(7)9.                               
060500     03  FILLER                  PIC X       VALUE SPACE.                 
060600     03  W001-DET2-P-KVANTH      PIC Z9.9.                                
060700     03  FILLER                  PIC X       VALUE SPACE.                 
060800     03  W001-DET2-TOT           PIC Z(13)9.                              
060900     03  FILLER                  PIC X       VALUE SPACE.                 
061000     03  W001-DET2-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
061100     EJECT                                                                
061200 01  W001-DETALJRAD-3.                                                    
061300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
061400     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND A'.        
061500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061600     03  W001-DET3-DLAGERA       PIC Z(7)9.                               
061700     03  FILLER                  PIC X       VALUE SPACE.                 
061800     03  W001-DET3-P-DLAGERA     PIC Z9.9.                                
061900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062000     03  W001-DET3-DLAGERB       PIC Z(7)9.                               
062100     03  FILLER                  PIC X       VALUE SPACE.                 
062200     03  W001-DET3-P-DLAGERB     PIC Z9.9.                                
062300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062400     03  W001-DET3-DLAGERC       PIC Z(7)9.                               
062500     03  FILLER                  PIC X       VALUE SPACE.                 
062600     03  W001-DET3-P-DLAGERC     PIC Z9.9.                                
062700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062800     03  W001-DET3-DLAGERD       PIC Z(7)9.                               
062900     03  FILLER                  PIC X       VALUE SPACE.                 
063000     03  W001-DET3-P-DLAGERD     PIC Z9.9.                                
063100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063200     03  W001-DET3-DLAGERE       PIC Z(7)9.                               
063300     03  FILLER                  PIC X       VALUE SPACE.                 
063400     03  W001-DET3-P-DLAGERE     PIC Z9.9.                                
063500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063600     03  W001-DET3-DLAGERF       PIC Z(7)9.                               
063700     03  FILLER                  PIC X       VALUE SPACE.                 
063800     03  W001-DET3-P-DLAGERF     PIC Z9.9.                                
063900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064000     03  W001-DET3-DLAGERG       PIC Z(7)9.                               
064100     03  FILLER                  PIC X       VALUE SPACE.                 
064200     03  W001-DET3-P-DLAGERG     PIC Z9.9.                                
064300     03  FILLER                  PIC X       VALUE SPACE.                 
064400     03  W001-DET3-DLAGERH       PIC Z(7)9.                               
064500     03  FILLER                  PIC X       VALUE SPACE.                 
064600     03  W001-DET3-P-DLAGERH     PIC Z9.9.                                
064700     03  FILLER                  PIC X       VALUE SPACE.                 
064800     03  W001-DET3-TOT           PIC Z(13)9.                              
064900     03  FILLER                  PIC X       VALUE SPACE.                 
065000     03  W001-DET3-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
065100 01  W001-DETALJRAD-4.                                                    
065200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
065300     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND P'.        
065400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065500     03  W001-DET4-DLAGERA       PIC Z(7)9.                               
065600     03  FILLER                  PIC X       VALUE SPACE.                 
065700     03  W001-DET4-P-DLAGERA     PIC Z9.9.                                
065800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065900     03  W001-DET4-DLAGERB       PIC Z(7)9.                               
066000     03  FILLER                  PIC X       VALUE SPACE.                 
066100     03  W001-DET4-P-DLAGERB     PIC Z9.9.                                
066200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066300     03  W001-DET4-DLAGERC       PIC Z(7)9.                               
066400     03  FILLER                  PIC X       VALUE SPACE.                 
066500     03  W001-DET4-P-DLAGERC     PIC Z9.9.                                
066600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066700     03  W001-DET4-DLAGERD       PIC Z(7)9.                               
066800     03  FILLER                  PIC X       VALUE SPACE.                 
066900     03  W001-DET4-P-DLAGERD     PIC Z9.9.                                
067000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067100     03  W001-DET4-DLAGERE       PIC Z(7)9.                               
067200     03  FILLER                  PIC X       VALUE SPACE.                 
067300     03  W001-DET4-P-DLAGERE     PIC Z9.9.                                
067400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067500     03  W001-DET4-DLAGERF       PIC Z(7)9.                               
067600     03  FILLER                  PIC X       VALUE SPACE.                 
067700     03  W001-DET4-P-DLAGERF     PIC Z9.9.                                
067800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067900     03  W001-DET4-DLAGERG       PIC Z(7)9.                               
068000     03  FILLER                  PIC X       VALUE SPACE.                 
068100     03  W001-DET4-P-DLAGERG     PIC Z9.9.                                
068200     03  FILLER                  PIC X       VALUE SPACE.                 
068300     03  W001-DET4-DLAGERH       PIC Z(7)9.                               
068400     03  FILLER                  PIC X       VALUE SPACE.                 
068500     03  W001-DET4-P-DLAGERH     PIC Z9.9.                                
068600     03  FILLER                  PIC X       VALUE SPACE.                 
068700     03  W001-DET4-TOT           PIC Z(13)9.                              
068800     03  FILLER                  PIC X       VALUE SPACE.                 
068900     03  W001-DET4-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
069000     EJECT                                                                
069100 01  W001-DETALJRAD-5.                                                    
069200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
069300     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. A'.        
069400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069500     03  W001-DET5-LLAGERA       PIC Z(7)9.                               
069600     03  FILLER                  PIC X       VALUE SPACE.                 
069700     03  W001-DET5-P-LLAGERA     PIC Z9.9.                                
069800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069900     03  W001-DET5-LLAGERB       PIC Z(7)9.                               
070000     03  FILLER                  PIC X       VALUE SPACE.                 
070100     03  W001-DET5-P-LLAGERB     PIC Z9.9.                                
070200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070300     03  W001-DET5-LLAGERC       PIC Z(7)9.                               
070400     03  FILLER                  PIC X       VALUE SPACE.                 
070500     03  W001-DET5-P-LLAGERC     PIC Z9.9.                                
070600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070700     03  W001-DET5-LLAGERD       PIC Z(7)9.                               
070800     03  FILLER                  PIC X       VALUE SPACE.                 
070900     03  W001-DET5-P-LLAGERD     PIC Z9.9.                                
071000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071100     03  W001-DET5-LLAGERE       PIC Z(7)9.                               
071200     03  FILLER                  PIC X       VALUE SPACE.                 
071300     03  W001-DET5-P-LLAGERE     PIC Z9.9.                                
071400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071500     03  W001-DET5-LLAGERF       PIC Z(7)9.                               
071600     03  FILLER                  PIC X       VALUE SPACE.                 
071700     03  W001-DET5-P-LLAGERF     PIC Z9.9.                                
071800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071900     03  W001-DET5-LLAGERG       PIC Z(7)9.                               
072000     03  FILLER                  PIC X       VALUE SPACE.                 
072100     03  W001-DET5-P-LLAGERG     PIC Z9.9.                                
072200     03  FILLER                  PIC X       VALUE SPACE.                 
072300     03  W001-DET5-LLAGERH       PIC Z(7)9.                               
072400     03  FILLER                  PIC X       VALUE SPACE.                 
072500     03  W001-DET5-P-LLAGERH     PIC Z9.9.                                
072600     03  FILLER                  PIC X       VALUE SPACE.                 
072700     03  W001-DET5-TOT           PIC Z(13)9.                              
072800     03  FILLER                  PIC X       VALUE SPACE.                 
072900     03  W001-DET5-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
073000     EJECT                                                                
073100 01  W001-DETALJRAD-6.                                                    
073200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
073300     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. P'.        
073400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073500     03  W001-DET6-LLAGERA       PIC Z(7)9.                               
073600     03  FILLER                  PIC X       VALUE SPACE.                 
073700     03  W001-DET6-P-LLAGERA     PIC Z9.9.                                
073800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073900     03  W001-DET6-LLAGERB       PIC Z(7)9.                               
074000     03  FILLER                  PIC X       VALUE SPACE.                 
074100     03  W001-DET6-P-LLAGERB     PIC Z9.9.                                
074200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074300     03  W001-DET6-LLAGERC       PIC Z(7)9.                               
074400     03  FILLER                  PIC X       VALUE SPACE.                 
074500     03  W001-DET6-P-LLAGERC     PIC Z9.9.                                
074600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074700     03  W001-DET6-LLAGERD       PIC Z(7)9.                               
074800     03  FILLER                  PIC X       VALUE SPACE.                 
074900     03  W001-DET6-P-LLAGERD     PIC Z9.9.                                
075000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075100     03  W001-DET6-LLAGERE       PIC Z(7)9.                               
075200     03  FILLER                  PIC X       VALUE SPACE.                 
075300     03  W001-DET6-P-LLAGERE     PIC Z9.9.                                
075400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075500     03  W001-DET6-LLAGERF       PIC Z(7)9.                               
075600     03  FILLER                  PIC X       VALUE SPACE.                 
075700     03  W001-DET6-P-LLAGERF     PIC Z9.9.                                
075800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075900     03  W001-DET6-LLAGERG       PIC Z(7)9.                               
076000     03  FILLER                  PIC X       VALUE SPACE.                 
076100     03  W001-DET6-P-LLAGERG     PIC Z9.9.                                
076200     03  FILLER                  PIC X       VALUE SPACE.                 
076300     03  W001-DET6-LLAGERH       PIC Z(7)9.                               
076400     03  FILLER                  PIC X       VALUE SPACE.                 
076500     03  W001-DET6-P-LLAGERH     PIC Z9.9.                                
076600     03  FILLER                  PIC X       VALUE SPACE.                 
076700     03  W001-DET6-TOT           PIC Z(13)9.                              
076800     03  FILLER                  PIC X       VALUE SPACE.                 
076900     03  W001-DET6-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
077000     EJECT                                                                
077100 01  W001-DETALJRAD-7.                                                    
077200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
077300     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   A'.        
077400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077500     03  W001-DET7-ALAGERA       PIC Z(7)9.                               
077600     03  FILLER                  PIC X       VALUE SPACE.                 
077700     03  W001-DET7-P-ALAGERA     PIC Z9.9.                                
077800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077900     03  W001-DET7-ALAGERB       PIC Z(7)9.                               
078000     03  FILLER                  PIC X       VALUE SPACE.                 
078100     03  W001-DET7-P-ALAGERB     PIC Z9.9.                                
078200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078300     03  W001-DET7-ALAGERC       PIC Z(7)9.                               
078400     03  FILLER                  PIC X       VALUE SPACE.                 
078500     03  W001-DET7-P-ALAGERC     PIC Z9.9.                                
078600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078700     03  W001-DET7-ALAGERD       PIC Z(7)9.                               
078800     03  FILLER                  PIC X       VALUE SPACE.                 
078900     03  W001-DET7-P-ALAGERD     PIC Z9.9.                                
079000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079100     03  W001-DET7-ALAGERE       PIC Z(7)9.                               
079200     03  FILLER                  PIC X       VALUE SPACE.                 
079300     03  W001-DET7-P-ALAGERE     PIC Z9.9.                                
079400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079500     03  W001-DET7-ALAGERF       PIC Z(7)9.                               
079600     03  FILLER                  PIC X       VALUE SPACE.                 
079700     03  W001-DET7-P-ALAGERF     PIC Z9.9.                                
079800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079900     03  W001-DET7-ALAGERG       PIC Z(7)9.                               
080000     03  FILLER                  PIC X       VALUE SPACE.                 
080100     03  W001-DET7-P-ALAGERG     PIC Z9.9.                                
080200     03  FILLER                  PIC X       VALUE SPACE.                 
080300     03  W001-DET7-ALAGERH       PIC Z(7)9.                               
080400     03  FILLER                  PIC X       VALUE SPACE.                 
080500     03  W001-DET7-P-ALAGERH     PIC Z9.9.                                
080600     03  FILLER                  PIC X       VALUE SPACE.                 
080700     03  W001-DET7-TOT           PIC Z(13)9.                              
080800     03  FILLER                  PIC X       VALUE SPACE.                 
080900     03  W001-DET7-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
081000     EJECT                                                                
081100 01  W001-DETALJRAD-8.                                                    
081200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
081300     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   P'.        
081400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081500     03  W001-DET8-ALAGERA       PIC Z(7)9.                               
081600     03  FILLER                  PIC X       VALUE SPACE.                 
081700     03  W001-DET8-P-ALAGERA     PIC Z9.9.                                
081800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081900     03  W001-DET8-ALAGERB       PIC Z(7)9.                               
082000     03  FILLER                  PIC X       VALUE SPACE.                 
082100     03  W001-DET8-P-ALAGERB     PIC Z9.9.                                
082200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082300     03  W001-DET8-ALAGERC       PIC Z(7)9.                               
082400     03  FILLER                  PIC X       VALUE SPACE.                 
082500     03  W001-DET8-P-ALAGERC     PIC Z9.9.                                
082600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082700     03  W001-DET8-ALAGERD       PIC Z(7)9.                               
082800     03  FILLER                  PIC X       VALUE SPACE.                 
082900     03  W001-DET8-P-ALAGERD     PIC Z9.9.                                
083000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083100     03  W001-DET8-ALAGERE       PIC Z(7)9.                               
083200     03  FILLER                  PIC X       VALUE SPACE.                 
083300     03  W001-DET8-P-ALAGERE     PIC Z9.9.                                
083400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083500     03  W001-DET8-ALAGERF       PIC Z(7)9.                               
083600     03  FILLER                  PIC X       VALUE SPACE.                 
083700     03  W001-DET8-P-ALAGERF     PIC Z9.9.                                
083800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083900     03  W001-DET8-ALAGERG       PIC Z(7)9.                               
084000     03  FILLER                  PIC X       VALUE SPACE.                 
084100     03  W001-DET8-P-ALAGERG     PIC Z9.9.                                
084200     03  FILLER                  PIC X       VALUE SPACE.                 
084300     03  W001-DET8-ALAGERH       PIC Z(7)9.                               
084400     03  FILLER                  PIC X       VALUE SPACE.                 
084500     03  W001-DET8-P-ALAGERH     PIC Z9.9.                                
084600     03  FILLER                  PIC X       VALUE SPACE.                 
084700     03  W001-DET8-TOT           PIC Z(13)9.                              
084800     03  FILLER                  PIC X       VALUE SPACE.                 
084900     03  W001-DET8-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
085000     EJECT                                                                
085100 01  W001-DETALJRAD-9.                                                    
085200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
085300     03  FILLER                  PIC X(12)   VALUE 'OVERSTOCK   '.        
085400     03  FILLER                  PIC X       VALUE SPACE.                 
085500     03  W001-DET9-OLAGERA       PIC Z(7)9.                               
085600     03  FILLER                  PIC X       VALUE SPACE.                 
085700     03  W001-DET9-P-OLAGERA     PIC Z9.9    BLANK WHEN ZERO.             
085800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085900     03  W001-DET9-OLAGERB       PIC Z(7)9.                               
086000     03  FILLER                  PIC X       VALUE SPACE.                 
086100     03  W001-DET9-P-OLAGERB     PIC Z9.9    BLANK WHEN ZERO.             
086200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086300     03  W001-DET9-OLAGERC       PIC Z(7)9.                               
086400     03  FILLER                  PIC X       VALUE SPACE.                 
086500     03  W001-DET9-P-OLAGERC     PIC Z9.9    BLANK WHEN ZERO.             
086600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086700     03  W001-DET9-OLAGERD       PIC Z(7)9.                               
086800     03  FILLER                  PIC X       VALUE SPACE.                 
086900     03  W001-DET9-P-OLAGERD     PIC Z9.9    BLANK WHEN ZERO.             
087000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087100     03  W001-DET9-OLAGERE       PIC Z(7)9.                               
087200     03  FILLER                  PIC X       VALUE SPACE.                 
087300     03  W001-DET9-P-OLAGERE     PIC Z9.9    BLANK WHEN ZERO.             
087400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087500     03  W001-DET9-OLAGERF       PIC Z(7)9.                               
087600     03  FILLER                  PIC X       VALUE SPACE.                 
087700     03  W001-DET9-P-OLAGERF     PIC Z9.9    BLANK WHEN ZERO.             
087800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087900     03  W001-DET9-OLAGERG       PIC Z(7)9.                               
088000     03  FILLER                  PIC X       VALUE SPACE.                 
088100     03  W001-DET9-P-OLAGERG     PIC Z9.9    BLANK WHEN ZERO.             
088200     03  FILLER                  PIC X       VALUE SPACE.                 
088300     03  W001-DET9-OLAGERH       PIC Z(7)9.                               
088400     03  FILLER                  PIC X       VALUE SPACE.                 
088500     03  W001-DET9-P-OLAGERH     PIC Z9.9    BLANK WHEN ZERO.             
088600     03  FILLER                  PIC X       VALUE SPACE.                 
088700     03  W001-DET9-TOT           PIC Z(13)9.                              
088800     03  FILLER                  PIC X       VALUE SPACE.                 
088900     03  W001-DET9-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
089000     EJECT                                                                
089100 01  W001-DETALJRAD-10.                                                   
089200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
089300     03  FILLER                  PIC X(12)   VALUE 'SAF. STOCK  '.        
089400     03  FILLER                  PIC X       VALUE SPACE.                 
089500     03  W001-DET10-SLAGERA      PIC Z(7)9.                               
089600     03  FILLER                  PIC X       VALUE SPACE.                 
089700     03  W001-DET10-P-SLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
089800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089900     03  W001-DET10-SLAGERB      PIC Z(7)9.                               
090000     03  FILLER                  PIC X       VALUE SPACE.                 
090100     03  W001-DET10-P-SLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
090200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090300     03  W001-DET10-SLAGERC      PIC Z(7)9.                               
090400     03  FILLER                  PIC X       VALUE SPACE.                 
090500     03  W001-DET10-P-SLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
090600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090700     03  W001-DET10-SLAGERD      PIC Z(7)9.                               
090800     03  FILLER                  PIC X       VALUE SPACE.                 
090900     03  W001-DET10-P-SLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
091000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091100     03  W001-DET10-SLAGERE      PIC Z(7)9.                               
091200     03  FILLER                  PIC X       VALUE SPACE.                 
091300     03  W001-DET10-P-SLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
091400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091500     03  W001-DET10-SLAGERF      PIC Z(7)9.                               
091600     03  FILLER                  PIC X       VALUE SPACE.                 
091700     03  W001-DET10-P-SLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
091800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091900     03  W001-DET10-SLAGERG      PIC Z(7)9.                               
092000     03  FILLER                  PIC X       VALUE SPACE.                 
092100     03  W001-DET10-P-SLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
092200     03  FILLER                  PIC X       VALUE SPACE.                 
092300     03  W001-DET10-SLAGERH      PIC Z(7)9.                               
092400     03  FILLER                  PIC X       VALUE SPACE.                 
092500     03  W001-DET10-P-SLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
092600     03  FILLER                  PIC X       VALUE SPACE.                 
092700     03  W001-DET10-TOT          PIC Z(13)9.                              
092800     03  FILLER                  PIC X       VALUE SPACE.                 
092900     03  W001-DET10-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
093000     EJECT                                                                
093100 01  W001-DETALJRAD-11.                                                   
093200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
093300     03  FILLER                  PIC X(12)   VALUE 'AVERAGE ST. '.        
093400     03  FILLER                  PIC X       VALUE SPACE.                 
093500     03  W001-DET11-MLAGERA      PIC Z(7)9.                               
093600     03  FILLER                  PIC X       VALUE SPACE.                 
093700     03  W001-DET11-P-MLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
093800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093900     03  W001-DET11-MLAGERB      PIC Z(7)9.                               
094000     03  FILLER                  PIC X       VALUE SPACE.                 
094100     03  W001-DET11-P-MLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
094200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094300     03  W001-DET11-MLAGERC      PIC Z(7)9.                               
094400     03  FILLER                  PIC X       VALUE SPACE.                 
094500     03  W001-DET11-P-MLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
094600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094700     03  W001-DET11-MLAGERD      PIC Z(7)9.                               
094800     03  FILLER                  PIC X       VALUE SPACE.                 
094900     03  W001-DET11-P-MLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
095000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095100     03  W001-DET11-MLAGERE      PIC Z(7)9.                               
095200     03  FILLER                  PIC X       VALUE SPACE.                 
095300     03  W001-DET11-P-MLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
095400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095500     03  W001-DET11-MLAGERF      PIC Z(7)9.                               
095600     03  FILLER                  PIC X       VALUE SPACE.                 
095700     03  W001-DET11-P-MLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
095800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095900     03  W001-DET11-MLAGERG      PIC Z(7)9.                               
096000     03  FILLER                  PIC X       VALUE SPACE.                 
096100     03  W001-DET11-P-MLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
096200     03  FILLER                  PIC X       VALUE SPACE.                 
096300     03  W001-DET11-MLAGERH      PIC Z(7)9.                               
096400     03  FILLER                  PIC X       VALUE SPACE.                 
096500     03  W001-DET11-P-MLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
096600     03  FILLER                  PIC X       VALUE SPACE.                 
096700     03  W001-DET11-TOT          PIC Z(13)9.                              
096800     03  FILLER                  PIC X       VALUE SPACE.                 
096900     03  W001-DET11-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
097000     EJECT                                                                
097100 01  W001-DETALJRAD-12.                                                   
097200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097300     03  W001-DET12-PRISKLASS    PIC X       VALUE SPACE.                 
097400     03  FILLER                  PIC X       VALUE SPACE.                 
097500     03  FILLER                  PIC X(12)   VALUE 'NO INCOM ORD'.        
097600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097700     03  W001-DET12-KVOTA        PIC Z(7)9.                               
097800     03  FILLER                  PIC X       VALUE SPACE.                 
097900     03  W001-DET12-P-KVOTA      PIC Z9.9.                                
098000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098100     03  W001-DET12-KVOTB        PIC Z(7)9.                               
098200     03  FILLER                  PIC X       VALUE SPACE.                 
098300     03  W001-DET12-P-KVOTB      PIC Z9.9.                                
098400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098500     03  W001-DET12-KVOTC        PIC Z(7)9.                               
098600     03  FILLER                  PIC X       VALUE SPACE.                 
098700     03  W001-DET12-P-KVOTC      PIC Z9.9.                                
098800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098900     03  W001-DET12-KVOTD        PIC Z(7)9.                               
099000     03  FILLER                  PIC X       VALUE SPACE.                 
099100     03  W001-DET12-P-KVOTD      PIC Z9.9.                                
099200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
099300     03  W001-DET12-KVOTE        PIC Z(7)9.                               
099400     03  FILLER                  PIC X       VALUE SPACE.                 
099500     03  W001-DET12-P-KVOTE      PIC Z9.9.                                
099600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
099700     03  W001-DET12-KVOTF        PIC Z(7)9.                               
099800     03  FILLER                  PIC X       VALUE SPACE.                 
099900     03  W001-DET12-P-KVOTF      PIC Z9.9.                                
100000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
100100     03  W001-DET12-KVOTG        PIC Z(7)9.                               
100200     03  FILLER                  PIC X       VALUE SPACE.                 
100300     03  W001-DET12-P-KVOTG      PIC Z9.9.                                
100400     03  FILLER                  PIC X       VALUE SPACE.                 
100500     03  W001-DET12-KVOTH        PIC Z(7)9.                               
100600     03  FILLER                  PIC X       VALUE SPACE.                 
100700     03  W001-DET12-P-KVOTH      PIC Z9.9.                                
100800     03  FILLER                  PIC X       VALUE SPACE.                 
100900     03  W001-DET12-TOT          PIC Z(13)9.                              
101000     03  FILLER                  PIC X       VALUE SPACE.                 
101100     03  W001-DET12-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
101200     EJECT                                                                
101300 01  W001-DETALJRAD-13.                                                   
101400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
101500     03  FILLER                  PIC X(13) VALUE 'SPLIT FACTOR '.         
101600     03  FILLER                  PIC X(4)    VALUE SPACE.                 
101700     03  W001-DET13-SPLITA       PIC Z9.9.                                
101800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101900     03  W001-DET13-SPLITB       PIC Z9.9.                                
102000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
102100     03  W001-DET13-SPLITC       PIC Z9.9.                                
102200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
102300     03  W001-DET13-SPLITD       PIC Z9.9.                                
102400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
102500     03  W001-DET13-SPLITE       PIC Z9.9.                                
102600     03  FILLER                  PIC X(10)   VALUE SPACE.                 
102700     03  W001-DET13-SPLITF       PIC Z9.9.                                
102800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
102900     03  W001-DET13-SPLITG       PIC Z9.9.                                
103000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
103100     03  W001-DET13-SPLITH       PIC Z9.9.                                
103200     03  FILLER                  PIC X(16)   VALUE SPACE.                 
103300     03  W001-DET13-TOT          PIC Z9.9.                                
103400     03  FILLER                  PIC X       VALUE SPACE.                 
103500     03  W001-DET13-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
103600     EJECT                                                                
103700 01  W001-DETALJRAD-14.                                                   
103800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
103900     03  FILLER                  PIC X(14) VALUE 'TOR        SOH'.        
104000     03  W001-DET14-OMSHASTA     PIC Z(4)9.9.                             
104100     03  FILLER                  PIC X       VALUE SPACE.                 
104200     03  W001-DET14-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
104300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104400     03  W001-DET14-OMSHASTB     PIC Z(5)9.9.                             
104500     03  FILLER                  PIC X       VALUE SPACE.                 
104600     03  W001-DET14-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
104700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104800     03  W001-DET14-OMSHASTC     PIC Z(5)9.9.                             
104900     03  FILLER                  PIC X       VALUE SPACE.                 
105000     03  W001-DET14-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
105100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105200     03  W001-DET14-OMSHASTD     PIC Z(5)9.9.                             
105300     03  FILLER                  PIC X       VALUE SPACE.                 
105400     03  W001-DET14-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
105500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105600     03  W001-DET14-OMSHASTE     PIC Z(5)9.9.                             
105700     03  FILLER                  PIC X       VALUE SPACE.                 
105800     03  W001-DET14-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
105900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106000     03  W001-DET14-OMSHASTF     PIC Z(5)9.9.                             
106100     03  FILLER                  PIC X       VALUE SPACE.                 
106200     03  W001-DET14-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
106300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106400     03  W001-DET14-OMSHASTG     PIC Z(5)9.9.                             
106500     03  FILLER                  PIC X       VALUE SPACE.                 
106600     03  W001-DET14-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
106700     03  FILLER                  PIC X       VALUE SPACE.                 
106800     03  W001-DET14-OMSHASTH     PIC Z(5)9.9.                             
106900     03  FILLER                  PIC X       VALUE SPACE.                 
107000     03  W001-DET14-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
107100     03  FILLER                  PIC X       VALUE SPACE.                 
107200     03  W001-DET14-TOT          PIC Z(11)9.9.                            
107300     03  FILLER                  PIC X       VALUE SPACE.                 
107400     03  W001-DET14-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
107500     EJECT                                                                
107600 01  W001-DETALJRAD-15.                                                   
107700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
107800     03  FILLER                  PIC X(14) VALUE 'TOR BAL+AK+GIT'.        
107900     03  W001-DET15-OMSHASTA     PIC Z(4)9.9.                             
108000     03  FILLER                  PIC X       VALUE SPACE.                 
108100     03  W001-DET15-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
108200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108300     03  W001-DET15-OMSHASTB     PIC Z(5)9.9.                             
108400     03  FILLER                  PIC X       VALUE SPACE.                 
108500     03  W001-DET15-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
108600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108700     03  W001-DET15-OMSHASTC     PIC Z(5)9.9.                             
108800     03  FILLER                  PIC X       VALUE SPACE.                 
108900     03  W001-DET15-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
109000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109100     03  W001-DET15-OMSHASTD     PIC Z(5)9.9.                             
109200     03  FILLER                  PIC X       VALUE SPACE.                 
109300     03  W001-DET15-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
109400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109500     03  W001-DET15-OMSHASTE     PIC Z(5)9.9.                             
109600     03  FILLER                  PIC X       VALUE SPACE.                 
109700     03  W001-DET15-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
109800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109900     03  W001-DET15-OMSHASTF     PIC Z(5)9.9.                             
110000     03  FILLER                  PIC X       VALUE SPACE.                 
110100     03  W001-DET15-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
110200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
110300     03  W001-DET15-OMSHASTG     PIC Z(5)9.9.                             
110400     03  FILLER                  PIC X       VALUE SPACE.                 
110500     03  W001-DET15-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
110600     03  FILLER                  PIC X       VALUE SPACE.                 
110700     03  W001-DET15-OMSHASTH     PIC Z(5)9.9.                             
110800     03  FILLER                  PIC X       VALUE SPACE.                 
110900     03  W001-DET15-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
111000     03  FILLER                  PIC X       VALUE SPACE.                 
111100     03  W001-DET15-TOT          PIC Z(11)9.9.                            
111200     03  FILLER                  PIC X       VALUE SPACE.                 
111300     03  W001-DET15-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
111400     EJECT                                                                
111500 01  W001-DETALJRAD-16.                                                   
111600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
111700     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE G'.        
111800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
111900     03  W001-DET16-SERVG-BTOA   PIC Z9.9.                                
112000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112100     03  W001-DET16-SERVG-BTOB   PIC Z9.9.                                
112200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112300     03  W001-DET16-SERVG-BTOC   PIC Z9.9.                                
112400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112500     03  W001-DET16-SERVG-BTOD   PIC Z9.9.                                
112600     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112700     03  W001-DET16-SERVG-BTOE   PIC Z9.9.                                
112800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112900     03  W001-DET16-SERVG-BTOF   PIC Z9.9.                                
113000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113100     03  W001-DET16-SERVG-BTOG   PIC Z9.9.                                
113200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113300     03  W001-DET16-SERVG-BTOH   PIC Z9.9.                                
113400     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
113600     03  W001-DET16-TOT          PIC Z9.9.                                
113700     03  FILLER                  PIC X       VALUE SPACE.                 
113800     03  W001-DET16-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
113900     EJECT                                                                
114000 01  W001-DETALJRAD-17.                                                   
114100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
114200     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE N'.        
114300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
114400     03  W001-DET17-SERVG-NTOA   PIC Z9.9.                                
114500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114600     03  W001-DET17-SERVG-NTOB   PIC Z9.9.                                
114700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114800     03  W001-DET17-SERVG-NTOC   PIC Z9.9.                                
114900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
115000     03  W001-DET17-SERVG-NTOD   PIC Z9.9.                                
115100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
115200     03  W001-DET17-SERVG-NTOE   PIC Z9.9.                                
115300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
115400     03  W001-DET17-SERVG-NTOF   PIC Z9.9.                                
115500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
115600     03  W001-DET17-SERVG-NTOG   PIC Z9.9.                                
115700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
115800     03  W001-DET17-SERVG-NTOH   PIC Z9.9.                                
115900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
116000     03  FILLER                  PIC X(11)   VALUE SPACE.                 
116100     03  W001-DET17-TOT          PIC Z9.9.                                
116200     03  FILLER                  PIC X       VALUE SPACE.                 
116300     03  W001-DET17-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
116400     EJECT                                                                
116500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
116600*                                                                         
116700     EJECT                                                                
116800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
116900     SKIP3                                                                
117000 01  NYCKLAR-TILL-DLI.                                                    
117100     03  W-IDDC-B6-X.                                                     
117200         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
117300     SKIP2                                                                
117400*    --- STATUS-KOD FRÅN IMS                                              
117500 01  STATUS-WS                   PIC XX.                                  
117600     88  SEGMENT-FINNS                       VALUE '  ' 'GA' 'GK'.        
117700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
117800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
117900     SKIP2                                                                
118000 01  GODK-STATUSKODER.                                                    
118100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
118200     SKIP3                                                                
118300 01  SSA1                        PIC X(64).                               
118400 01  SSA2                        PIC X(64).                               
118500     EJECT                                                                
118600*    --- IMS FUNKTIONSKODER                                               
118700*01  -COPY W0003                                                          
118800     EJECT                                                                
118900*    ---  DLI INPUT-OUTPUT AREA                                           
119000 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
119100 01   DLI-IO-AREA-B6      PIC X(900).                                     
119200 01   DLI-IO-AREA-B601    REDEFINES DLI-IO-AREA-B6.                       
119300*     03  -COPY WDB601                                                    
119400     EJECT                                                                
119500 01   DLI-IO-AREA-B616    REDEFINES DLI-IO-AREA-B6.                       
119600*     03  -COPY WDB616                                                    
119700     EJECT                                                                
119800 LINKAGE SECTION.                                                         
119900                                                                          
120000*01  -COPY W0008      -PRE WDB6-                                          
120100     05  FILLER                  PIC X.                                   
120200 PROCEDURE DIVISION USING WDB6-PCB.                                       
120300                                                                          
120400     PERFORM A-INIT                                                       
120500     PERFORM B-SKAPA-LISTA                                                
120600     PERFORM C-SKRIV-LISTA                                                
120700     PERFORM Z-FINIT                                                      
120800                                                                          
120900     MOVE ZERO TO RETURN-CODE                                             
121000     GOBACK                                                               
121100     .                                                                    
121200     EJECT                                                                
121300 A-INIT SECTION.                                                          
121400                                                                          
121500     OPEN INPUT  W23195                                                   
121600     OPEN OUTPUT W23199-001                                               
121700                                                                          
121800     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
121900     MOVE D-AAR    TO DAGENS-AAR                                          
122000                      D-VECKA-AAR                                         
122100     MOVE D-MAANAD TO DAGENS-MAANAD                                       
122200     MOVE D-VECKA  TO D-VECKA-VECKA                                       
122300     MOVE D-DAG    TO DAGENS-DAG                                          
122400     MOVE DAGENS-DATUM TO W001-DATUM                                      
122500     MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                              
122600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
122700                                                                          
122800     MOVE 'CN NDC/LDC ' TO W001-AKTUELLT-IDDC                             
122900     MOVE 'W23199-001'  TO W001-LISTNR                                    
123000                           W001-LISTID                                    
123100                                                                          
123200     PERFORM AA-LADDA-DC-TABELL                                           
123300     .                                                                    
123400     EJECT                                                                
123500 AA-LADDA-DC-TABELL SECTION.                                              
123600                                                                          
123700     INITIALIZE WS-DC-TABELL                                              
123800                WS-IDLEV-TABELL                                           
123900     MOVE 1 TO IDDC-IX                                                    
124000               IDLEV-IX                                                   
124100     PERFORM IMS-GET-WDB6                                                 
124200                                                                          
124300     PERFORM UNTIL SEGMENT-SLUT                                           
124400                OR IDDC-IX  > IDDC-IX-MAX                                 
124500                                                                          
124600        IF WDB6-SEG-NAME-FB = 'WDB601  '                                  
124700           MOVE DCS-IDDC        TO W-IDDC                                 
124800                                   WS-IDDC-R      (IDLEV-IX)              
124900           MOVE DCS-IDLEVNR-DC  TO WS-IDLEVNR-R   (IDLEV-IX)              
125000           ADD 1 TO IDLEV-IX                                              
125100        END-IF                                                            
125200                                                                          
125300        IF WDB6-SEG-NAME-FB = 'WDB616  '                                  
125400           MOVE W-IDDC          TO WS-IDDC-B601   (IDDC-IX)               
125500           MOVE REF-IDDC-REF    TO WS-IDDC-B616   (IDDC-IX)               
125600           MOVE REF-KVDLTID-TOT TO WS-KVDLTID-TOT (IDDC-IX)               
125700           ADD 1 TO IDDC-IX                                               
125800        END-IF                                                            
125900                                                                          
126000        PERFORM IMS-GET-WDB6                                              
126100     END-PERFORM                                                          
126200                                                                          
126300     .                                                                    
126400     EJECT                                                                
126500 B-SKAPA-LISTA SECTION.                                                   
126600                                                                          
126700     PERFORM BA-NOLLSTALL                                                 
126800                                                                          
126900     PERFORM S01-LAS-W23195                                               
127000     PERFORM UNTIL END-OF-W23195                                          
127100        MOVE IN-IDDC TO WS-IDDC                                           
127200        IF NDC-CN                                                         
127300           IF IN-IDDC NOT = W-IDDC-REC                                    
127400              PERFORM S12-HITTA-KVDLTID                                   
127500           END-IF                                                         
127600           PERFORM BB-SKAPA-TABELLER                                      
127700        END-IF                                                            
127800        PERFORM S01-LAS-W23195                                            
127900     END-PERFORM                                                          
128000                                                                          
128100     PERFORM BC-SUMMERA                                                   
128200     .                                                                    
128300     EJECT                                                                
128400 BA-NOLLSTALL SECTION.                                                    
128500******************************************************************        
128600*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
128700*  FREKVENSKLASS                                                 *        
128800*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
128900*     FREKV-KLASSER  = A B C D E F G                             *        
129000*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
129100*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
129200*                 TOTAL-SUMMERING                                *        
129300* ****************************************************************        
129400                                                                          
129500******* NOLLSTÄLLNING AV 72 'RUTOR' PER PRISKLASS/FREKVKLASS              
129600                                                                          
129700     MOVE +1  TO ART-IX                                                   
129800     MOVE +72 TO ART-IX-MAX                                               
129900     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
130000        MOVE ZERO TO     ART-KVANT-AKT(ART-IX)                            
130100                         ART-KVANT-PAS(ART-IX)                            
130200                         ART-PROC-KVANT-A(ART-IX)                         
130300                         ART-PROC-KVANT-P(ART-IX)                         
130400                         ART-KVDISP-AKT(ART-IX)                           
130500                         ART-PROC-KVDISP-A(ART-IX)                        
130600                         ART-KVDISP-PAS(ART-IX)                           
130700                         ART-PROC-KVDISP-P(ART-IX)                        
130800                         ART-LS-AKT(ART-IX)                               
130900                         ART-PROC-LS-A(ART-IX)                            
131000                         ART-LS-PAS(ART-IX)                               
131100                         ART-PROC-LS-P(ART-IX)                            
131200                         ART-AK-AKT(ART-IX)                               
131300                         ART-PROC-AK-A(ART-IX)                            
131400                         ART-AK-PAS(ART-IX)                               
131500                         ART-PROC-AK-P(ART-IX)                            
131600                         ART-OLAGER(ART-IX)                               
131700                         ART-PROC-OLAGER(ART-IX)                          
131800                         ART-SLAGER(ART-IX)                               
131900                         ART-PROC-SLAGER(ART-IX)                          
132000                         ART-MLAGER(ART-IX)                               
132100                         ART-PROC-MLAGER(ART-IX)                          
132200                         ART-KVOT(ART-IX)                                 
132300                         ART-PROC-KVOT(ART-IX)                            
132400                         ART-SPLIT(ART-IX)                                
132500                         ART-OMSHAST-DISP(ART-IX)                         
132600                         ART-OMSHAST-PROC-D(ART-IX)                       
132700                         ART-OMSHAST-LS(ART-IX)                           
132800                         ART-OMSHAST-PROC-LS(ART-IX)                      
132900                         ART-SERVG-BTO(ART-IX)                            
133000                         ART-SERVG-NTO(ART-IX)                            
133100****************                                                          
133200                         WS-ART-SLAGER(ART-IX)                            
133300                         WS-ART-OLAGER(ART-IX)                            
133400                         WS-ART-MLAGER(ART-IX)                            
133500                         WS-ART-KVLS-AKT(ART-IX)                          
133600                         WS-ART-KVLS-PAS(ART-IX)                          
133700                         WS-ART-LS-AKT(ART-IX)                            
133800                         WS-ART-LS-PAS(ART-IX)                            
133900                         WS-ART-LS-PR-AKT(ART-IX)                         
134000                         WS-ART-LS-PR-PAS(ART-IX)                         
134100                         WS-ART-KVDISP-AKT(ART-IX)                        
134200                         WS-ART-KVDISP-PAS(ART-IX)                        
134300                         WS-ART-KVDISP-PR-AKT(ART-IX)                     
134400                         WS-ART-KVDISP-PR-PAS(ART-IX)                     
134500                         WS-ART-KVOKS-AKT(ART-IX)                         
134600                         WS-ART-KVOKS-PAS(ART-IX)                         
134700                         WS-ART-OK-PR-AKT(ART-IX)                         
134800                         WS-ART-OK-PR-PAS(ART-IX)                         
134900                         WS-ART-KVAKS-AKT(ART-IX)                         
135000                         WS-ART-KVAKS-PAS(ART-IX)                         
135100                         WS-ART-AK-PR-AKT(ART-IX)                         
135200                         WS-ART-AK-PR-PAS(ART-IX)                         
135300                         WS-ART-KVOI(ART-IX)                              
135400                         WS-ART-KVOI-AKT(ART-IX)                          
135500                         WS-ART-KVOI-PAS(ART-IX)                          
135600                         WS-ART-KVOI-TEO(ART-IX)                          
135700                         WS-ART-KVOI-SAK(ART-IX)                          
135800                         WS-ART-KVOI-CDC-AKT(ART-IX)                      
135900                         WS-ART-KVOI-CDC-PAS(ART-IX)                      
136000                         WS-ART-KVOI-CDC-TEO(ART-IX)                      
136100                         WS-ART-KVOI-CDC-SAK(ART-IX)                      
136200                         WS-ART-SUINKORD(ART-IX)                          
136300                         WS-ART-SUFYSAVP(ART-IX)                          
136400                         WS-ART-SUAVBRP(ART-IX)                           
136500                         WS-ART-SULAGERB(ART-IX)                          
136600                         WS-ART-SUSORTB(ART-IX)                           
136700                                                                          
136800        ADD +1 TO ART-IX                                                  
136900     END-PERFORM                                                          
137000                                                                          
137100******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
137200*******                          OBEROENDE AV FREKVENSKLASS               
137300                                                                          
137400     MOVE +1 TO PSUM-IX                                                   
137500     MOVE +9 TO PSUM-IX-MAX                                               
137600     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
137700        MOVE ZERO TO     PSUM-KVANT-AKT(PSUM-IX)                          
137800                         PSUM-KVANT-PAS(PSUM-IX)                          
137900                         PSUM-PROC-KVANT-A(PSUM-IX)                       
138000                         PSUM-PROC-KVANT-P(PSUM-IX)                       
138100                         PSUM-KVDISP-AKT(PSUM-IX)                         
138200                         PSUM-PROC-KVDISP-A(PSUM-IX)                      
138300                         PSUM-KVDISP-PAS(PSUM-IX)                         
138400                         PSUM-PROC-KVDISP-P(PSUM-IX)                      
138500                         PSUM-LS-AKT(PSUM-IX)                             
138600                         PSUM-PROC-LS-A(PSUM-IX)                          
138700                         PSUM-LS-PAS(PSUM-IX)                             
138800                         PSUM-PROC-LS-P(PSUM-IX)                          
138900                         PSUM-AK-AKT(PSUM-IX)                             
139000                         PSUM-PROC-AK-A(PSUM-IX)                          
139100                         PSUM-AK-PAS(PSUM-IX)                             
139200                         PSUM-PROC-AK-P(PSUM-IX)                          
139300                         PSUM-OLAGER(PSUM-IX)                             
139400                         PSUM-PROC-OLAGER(PSUM-IX)                        
139500                         PSUM-SLAGER(PSUM-IX)                             
139600                         PSUM-PROC-SLAGER(PSUM-IX)                        
139700                         PSUM-MLAGER(PSUM-IX)                             
139800                         PSUM-PROC-MLAGER(PSUM-IX)                        
139900                         PSUM-KVOT(PSUM-IX)                               
140000                         PSUM-PROC-KVOT(PSUM-IX)                          
140100                         PSUM-SPLIT(PSUM-IX)                              
140200                         PSUM-OMSHAST-DISP(PSUM-IX)                       
140300                         PSUM-OMSHAST-PROC-D(PSUM-IX)                     
140400                         PSUM-OMSHAST-LS(PSUM-IX)                         
140500                         PSUM-OMSHAST-PROC-LS(PSUM-IX)                    
140600                         PSUM-SERVG-BTO(PSUM-IX)                          
140700                         PSUM-SERVG-NTO(PSUM-IX)                          
140800*************                                                             
140900                         WS-PSUM-SLAGER(PSUM-IX)                          
141000                         WS-PSUM-OLAGER(PSUM-IX)                          
141100                         WS-PSUM-MLAGER(PSUM-IX)                          
141200                         WS-PSUM-LS-AKT(PSUM-IX)                          
141300                         WS-PSUM-LS-PAS(PSUM-IX)                          
141400                         WS-PSUM-LS-PR-AKT(PSUM-IX)                       
141500                         WS-PSUM-LS-PR-PAS(PSUM-IX)                       
141600                         WS-PSUM-KVDISP-AKT(PSUM-IX)                      
141700                         WS-PSUM-KVDISP-PAS(PSUM-IX)                      
141800                         WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                   
141900                         WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                   
142000                         WS-PSUM-KVOKS-AKT(PSUM-IX)                       
142100                         WS-PSUM-KVOKS-PAS(PSUM-IX)                       
142200                         WS-PSUM-OK-PR-AKT(PSUM-IX)                       
142300                         WS-PSUM-OK-PR-PAS(PSUM-IX)                       
142400                         WS-PSUM-KVAKS-AKT(PSUM-IX)                       
142500                         WS-PSUM-KVAKS-PAS(PSUM-IX)                       
142600                         WS-PSUM-AK-PR-AKT(PSUM-IX)                       
142700                         WS-PSUM-AK-PR-PAS(PSUM-IX)                       
142800                         WS-PSUM-KVOI(PSUM-IX)                            
142900                         WS-PSUM-KVOI-AKT(PSUM-IX)                        
143000                         WS-PSUM-KVOI-PAS(PSUM-IX)                        
143100                         WS-PSUM-KVOI-TEO(PSUM-IX)                        
143200                         WS-PSUM-KVOI-SAK(PSUM-IX)                        
143300                         WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                    
143400                         WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                    
143500                         WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                    
143600                         WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                    
143700                         WS-PSUM-SUINKORD(PSUM-IX)                        
143800                         WS-PSUM-SUFYSAVP(PSUM-IX)                        
143900                         WS-PSUM-SUAVBRP(PSUM-IX)                         
144000                         WS-PSUM-SULAGERB(PSUM-IX)                        
144100                         WS-PSUM-SUSORTB(PSUM-IX)                         
144200                                                                          
144300        ADD +1 TO PSUM-IX                                                 
144400     END-PERFORM                                                          
144500                                                                          
144600******* NOLLSTÄLLNING AV 7 'RUTOR' TOTALSUMMA PER FREKVENSKLASS           
144700*******                            OBEROENDE AV PRISKLASS                 
144800                                                                          
144900     MOVE +1 TO FSUM-IX                                                   
145000     MOVE +8 TO FSUM-IX-MAX                                               
145100     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
145200        MOVE ZERO TO     FSUM-KVANT-AKT(FSUM-IX)                          
145300                         FSUM-KVANT-PAS(FSUM-IX)                          
145400                         FSUM-PROC-KVANT-A(FSUM-IX)                       
145500                         FSUM-PROC-KVANT-P(FSUM-IX)                       
145600                         FSUM-KVDISP-AKT(FSUM-IX)                         
145700                         FSUM-PROC-KVDISP-A(FSUM-IX)                      
145800                         FSUM-KVDISP-PAS(FSUM-IX)                         
145900                         FSUM-PROC-KVDISP-P(FSUM-IX)                      
146000                         FSUM-LS-AKT(FSUM-IX)                             
146100                         FSUM-PROC-LS-A(FSUM-IX)                          
146200                         FSUM-LS-PAS(FSUM-IX)                             
146300                         FSUM-PROC-LS-P(FSUM-IX)                          
146400                         FSUM-AK-AKT(FSUM-IX)                             
146500                         FSUM-PROC-AK-A(FSUM-IX)                          
146600                         FSUM-AK-PAS(FSUM-IX)                             
146700                         FSUM-PROC-AK-P(FSUM-IX)                          
146800                         FSUM-OLAGER(FSUM-IX)                             
146900                         FSUM-PROC-OLAGER(FSUM-IX)                        
147000                         FSUM-SLAGER(FSUM-IX)                             
147100                         FSUM-PROC-SLAGER(FSUM-IX)                        
147200                         FSUM-MLAGER(FSUM-IX)                             
147300                         FSUM-PROC-MLAGER(FSUM-IX)                        
147400                         FSUM-KVOT(FSUM-IX)                               
147500                         FSUM-PROC-KVOT(FSUM-IX)                          
147600                         FSUM-SPLIT(FSUM-IX)                              
147700                         FSUM-OMSHAST-DISP(FSUM-IX)                       
147800                         FSUM-OMSHAST-PROC-D(FSUM-IX)                     
147900                         FSUM-OMSHAST-LS(FSUM-IX)                         
148000                         FSUM-OMSHAST-PROC-LS(FSUM-IX)                    
148100                         FSUM-SERVG-BTO(FSUM-IX)                          
148200                         FSUM-SERVG-NTO(FSUM-IX)                          
148300*****************                                                         
148400                         WS-FSUM-SLAGER(FSUM-IX)                          
148500                         WS-FSUM-OLAGER(FSUM-IX)                          
148600                         WS-FSUM-MLAGER(FSUM-IX)                          
148700                         WS-FSUM-LS-AKT(FSUM-IX)                          
148800                         WS-FSUM-LS-PAS(FSUM-IX)                          
148900                         WS-FSUM-LS-PR-AKT(FSUM-IX)                       
149000                         WS-FSUM-LS-PR-PAS(FSUM-IX)                       
149100                         WS-FSUM-KVDISP-AKT(FSUM-IX)                      
149200                         WS-FSUM-KVDISP-PAS(FSUM-IX)                      
149300                         WS-FSUM-KVDISP-PR-AKT(FSUM-IX)                   
149400                         WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                   
149500                         WS-FSUM-KVOKS-AKT(FSUM-IX)                       
149600                         WS-FSUM-KVOKS-PAS(FSUM-IX)                       
149700                         WS-FSUM-OK-PR-AKT(FSUM-IX)                       
149800                         WS-FSUM-OK-PR-PAS(FSUM-IX)                       
149900                         WS-FSUM-KVAKS-AKT(FSUM-IX)                       
150000                         WS-FSUM-KVAKS-PAS(FSUM-IX)                       
150100                         WS-FSUM-AK-PR-AKT(FSUM-IX)                       
150200                         WS-FSUM-AK-PR-PAS(FSUM-IX)                       
150300                         WS-FSUM-KVOI(FSUM-IX)                            
150400                         WS-FSUM-KVOI-AKT(FSUM-IX)                        
150500                         WS-FSUM-KVOI-PAS(FSUM-IX)                        
150600                         WS-FSUM-KVOI-TEO(FSUM-IX)                        
150700                         WS-FSUM-KVOI-SAK(FSUM-IX)                        
150800                         WS-FSUM-KVOI-CDC-AKT(FSUM-IX)                    
150900                         WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                    
151000                         WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                    
151100                         WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                    
151200                         WS-FSUM-SUINKORD(FSUM-IX)                        
151300                         WS-FSUM-SUFYSAVP(FSUM-IX)                        
151400                         WS-FSUM-SUAVBRP(FSUM-IX)                         
151500                         WS-FSUM-SULAGERB(FSUM-IX)                        
151600                         WS-FSUM-SUSORTB(FSUM-IX)                         
151700                                                                          
151800        ADD +1 TO FSUM-IX                                                 
151900     END-PERFORM                                                          
152000                                                                          
152100******* NOLLSTÄLLNING AV TOTALRUTA                                        
152200                                                                          
152300     MOVE ZERO TO     TOT-KVANT-AKT                                       
152400                      TOT-KVANT-PAS                                       
152500                      TOT-KVDISP-AKT                                      
152600                      TOT-KVDISP-PAS                                      
152700                      TOT-LS-AKT                                          
152800                      TOT-LS-PAS                                          
152900                      TOT-AK-AKT                                          
153000                      TOT-AK-PAS                                          
153100                      TOT-SLAGER                                          
153200                      TOT-MLAGER                                          
153300                      TOT-OLAGER                                          
153400                      TOT-KVOT                                            
153500                      TOT-PROC-OLAGER                                     
153600                      TOT-PROC-MLAGER                                     
153700                      TOT-PROC-SLAGER                                     
153800                      TOT-SPLIT                                           
153900                      TOT-OMSHAST-DISP                                    
154000                      TOT-OMSHAST-LS                                      
154100                      TOT-SERVG-BTO                                       
154200                      TOT-SERVG-NTO                                       
154300************                                                              
154400                      WS-TOT-SLAGER                                       
154500                      WS-TOT-OLAGER                                       
154600                      WS-TOT-MLAGER                                       
154700                      WS-TOT-LS-AKT                                       
154800                      WS-TOT-LS-PAS                                       
154900                      WS-TOT-LS-PR-AKT                                    
155000                      WS-TOT-LS-PR-PAS                                    
155100                      WS-TOT-KVDISP-AKT                                   
155200                      WS-TOT-KVDISP-PAS                                   
155300                      WS-TOT-KVDISP-PR-AKT                                
155400                      WS-TOT-KVDISP-PR-PAS                                
155500                      WS-TOT-KVOKS-AKT                                    
155600                      WS-TOT-KVOKS-PAS                                    
155700                      WS-TOT-OK-PR-AKT                                    
155800                      WS-TOT-OK-PR-PAS                                    
155900                      WS-TOT-KVAKS-AKT                                    
156000                      WS-TOT-KVAKS-PAS                                    
156100                      WS-TOT-AK-PR-AKT                                    
156200                      WS-TOT-AK-PR-PAS                                    
156300                      WS-TOT-KVOI                                         
156400                      WS-TOT-KVOI-AKT                                     
156500                      WS-TOT-KVOI-PAS                                     
156600                      WS-TOT-KVOI-TEO                                     
156700                      WS-TOT-KVOI-SAK                                     
156800                      WS-TOT-KVOI-CDC-AKT                                 
156900                      WS-TOT-KVOI-CDC-PAS                                 
157000                      WS-TOT-KVOI-CDC-TEO                                 
157100                      WS-TOT-KVOI-CDC-SAK                                 
157200                      WS-TOT-SUINKORD                                     
157300                      WS-TOT-SUFYSAVP                                     
157400                      WS-TOT-SUAVBRP                                      
157500                      WS-TOT-SULAGERB                                     
157600                      WS-TOT-SUSORTB                                      
157700     .                                                                    
157800     EJECT                                                                
157900 BB-SKAPA-TABELLER SECTION.                                               
158000                                                                          
158100     PERFORM BBA-SAETT-ART-IX                                             
158200     IF SW-ARTIKEL-SAKNAS-WDK7 = JA                                       
158300        PERFORM BBC-UPPDAT-SAKN-ART                                       
158400     END-IF                                                               
158500     IF ART-IX > ZERO                                                     
158600        PERFORM BBB-UPPDATERA-TABELLER                                    
158700     END-IF                                                               
158800     .                                                                    
158900     EJECT                                                                
159000 BBA-SAETT-ART-IX SECTION.                                                
159100******************************************************************        
159200* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
159300******************************************************************        
159400                                                                          
159500     MOVE NEJ TO SW-ARTIKEL-SAKNAS-WDK7                                   
159600     EVALUATE TRUE                                                        
159700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
159800          MOVE +1 TO ART-IX                                               
159900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
160000          MOVE +2 TO ART-IX                                               
160100     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
160200          MOVE +3 TO ART-IX                                               
160300     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
160400          MOVE +4 TO ART-IX                                               
160500     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
160600          MOVE +5 TO ART-IX                                               
160700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
160800          MOVE +6 TO ART-IX                                               
160900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
161000          MOVE +7 TO ART-IX                                               
161100     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
161200          MOVE +8 TO ART-IX                                               
161300                                                                          
161400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
161500          MOVE +9 TO ART-IX                                               
161600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
161700          MOVE +10 TO ART-IX                                              
161800     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
161900          MOVE +11 TO ART-IX                                              
162000     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
162100          MOVE +12 TO ART-IX                                              
162200     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
162300          MOVE +13 TO ART-IX                                              
162400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
162500          MOVE +14 TO ART-IX                                              
162600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
162700          MOVE +15 TO ART-IX                                              
162800     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
162900          MOVE +16 TO ART-IX                                              
163000                                                                          
163100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
163200          MOVE +17 TO ART-IX                                              
163300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
163400          MOVE +18 TO ART-IX                                              
163500     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
163600          MOVE +19 TO ART-IX                                              
163700     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
163800          MOVE +20 TO ART-IX                                              
163900     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
164000          MOVE +21 TO ART-IX                                              
164100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
164200          MOVE +22 TO ART-IX                                              
164300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
164400          MOVE +23 TO ART-IX                                              
164500     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
164600          MOVE +24 TO ART-IX                                              
164700                                                                          
164800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
164900          MOVE +25 TO ART-IX                                              
165000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
165100          MOVE +26 TO ART-IX                                              
165200     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
165300          MOVE +27 TO ART-IX                                              
165400     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
165500          MOVE +28 TO ART-IX                                              
165600     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
165700          MOVE +29 TO ART-IX                                              
165800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
165900          MOVE +30 TO ART-IX                                              
166000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
166100          MOVE +31 TO ART-IX                                              
166200     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
166300          MOVE +32 TO ART-IX                                              
166400                                                                          
166500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
166600          MOVE +33 TO ART-IX                                              
166700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
166800          MOVE +34 TO ART-IX                                              
166900     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
167000          MOVE +35 TO ART-IX                                              
167100     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
167200          MOVE +36 TO ART-IX                                              
167300     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
167400          MOVE +37 TO ART-IX                                              
167500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
167600          MOVE +38 TO ART-IX                                              
167700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
167800          MOVE +39 TO ART-IX                                              
167900     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
168000          MOVE +40 TO ART-IX                                              
168100                                                                          
168200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
168300          MOVE +41 TO ART-IX                                              
168400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
168500          MOVE +42 TO ART-IX                                              
168600     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
168700          MOVE +43 TO ART-IX                                              
168800     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
168900          MOVE +44 TO ART-IX                                              
169000     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
169100          MOVE +45 TO ART-IX                                              
169200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
169300          MOVE +46 TO ART-IX                                              
169400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
169500          MOVE +47 TO ART-IX                                              
169600     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
169700          MOVE +48 TO ART-IX                                              
169800                                                                          
169900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
170000          MOVE +49 TO ART-IX                                              
170100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
170200          MOVE +50 TO ART-IX                                              
170300     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
170400          MOVE +51 TO ART-IX                                              
170500     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
170600          MOVE +52 TO ART-IX                                              
170700     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
170800          MOVE +53 TO ART-IX                                              
170900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
171000          MOVE +54 TO ART-IX                                              
171100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
171200          MOVE +55 TO ART-IX                                              
171300     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
171400          MOVE +56 TO ART-IX                                              
171500                                                                          
171600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
171700          MOVE +57 TO ART-IX                                              
171800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
171900          MOVE +58 TO ART-IX                                              
172000     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
172100          MOVE +59 TO ART-IX                                              
172200     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
172300          MOVE +60 TO ART-IX                                              
172400     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
172500          MOVE +61 TO ART-IX                                              
172600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
172700          MOVE +62 TO ART-IX                                              
172800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
172900          MOVE +63 TO ART-IX                                              
173000     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
173100          MOVE +64 TO ART-IX                                              
173200                                                                          
173300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
173400          MOVE +65 TO ART-IX                                              
173500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
173600          MOVE +66 TO ART-IX                                              
173700     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
173800          MOVE +67 TO ART-IX                                              
173900     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
174000          MOVE +68 TO ART-IX                                              
174100     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
174200          MOVE +69 TO ART-IX                                              
174300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
174400          MOVE +70 TO ART-IX                                              
174500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
174600          MOVE +71 TO ART-IX                                              
174700     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
174800          MOVE +72 TO ART-IX                                              
174900     WHEN OTHER                                                           
175000          MOVE ZERO TO ART-IX                                             
175100          IF IN-KDPRISKL = SPACE AND IN-KDFREKKL = SPACE                  
175200             MOVE JA TO SW-ARTIKEL-SAKNAS-WDK7                            
175300          END-IF                                                          
175400     END-EVALUATE                                                         
175500     .                                                                    
175600     EJECT                                                                
175700 BBB-UPPDATERA-TABELLER SECTION.                                          
175800                                                                          
175900*********  ANTAL ARTIKLAR                                                 
176000     IF IN-KDREFSTA = 'A'                                                 
176100        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
176200     ELSE                                                                 
176300        IF IN-KDREFSTA = 'P'                                              
176400           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
176500        END-IF                                                            
176600     END-IF                                                               
176700                                                                          
176800*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
176900     IF IN-KDREFSTA = 'A'                                                 
177000        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
177100        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
177200        COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                           
177300        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRMATRL                         
177400        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
177500                                                                          
177600        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRMATRL                          
177700        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
177800                                                                          
177900        COMPUTE WS-SUMMA = IN-KVLS * IN-PRMATRL                           
178000        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
178100                                                                          
178200        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
178300        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
178400        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRMATRL                          
178500        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
178600     ELSE                                                                 
178700        IF IN-KDREFSTA = 'P'                                              
178800           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
178900           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
179000           COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                        
179100           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRMATRL                      
179200           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
179300                                                                          
179400           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRMATRL                       
179500           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
179600                                                                          
179700           COMPUTE WS-SUMMA = IN-KVLS * IN-PRMATRL                        
179800           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
179900                                                                          
180000           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
180100           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
180200           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRMATRL                       
180300           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
180400        END-IF                                                            
180500     END-IF                                                               
180600                                                                          
180700*********  OMSÄTTNINGSHASTIGHET                                           
180800     MOVE +1 TO KVOI-IX                                                   
180900     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
181000     PERFORM UNTIL KVOI-IX > 53                                           
181100        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
181200        ADD +1 TO KVOI-IX                                                 
181300     END-PERFORM                                                          
181400                                                                          
181500     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRMATRL                       
181600     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
181700                                                                          
181800*********  SÄKERHETSLAGER/ARTIKEL                                         
181900     IF IN-KDREFSTA = 'A'                                                 
182000        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRMATRL                       
182100        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
182200     END-IF                                                               
182300                                                                          
182400*********  ÖVERLAGER/ARTIKEL                                              
182500     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
182600     IF WS-KVDISP > IN-KVREFOVL                                           
182700        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
182800        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRMATRL                         
182900        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
183000     END-IF                                                               
183100                                                                          
183200*********  MEDELLAGER/ARTIKEL                                             
183300     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
183400     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
183500     COMPUTE WS-LT-BEHOV-SDC-NORM = WS-KVDLTID-TOT (IDDC-IX)              
183600                                      * WS-KVPB-DAG-SDC-NORM              
183700     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
183800                        + (IN-KVREFBER / 2)                               
183900     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRMATRL                            
184000     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
184100                                                                          
184200*********  SERVICEGRAD OCH SPLITFAKTOR ORDERRADER/ARTIKEL                 
184300                                                                          
184400     MOVE +1 TO KVOI-IX                                                   
184500     MOVE NEJ TO SW-KVOI-TRAFF                                            
184600     PERFORM UNTIL KVOI-IX > 5                                            
184700        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
184800           MOVE JA TO SW-KVOI-TRAFF                                       
184900           IF IN-KDREFSTA = 'A'                                           
185000              ADD IN-KVOT-INNEV(KVOI-IX)                                  
185100                                TO WS-ART-KVOI-AKT(ART-IX)                
185200              ADD IN-KVOT-CDC-INNEV(KVOI-IX)                              
185300                                TO WS-ART-KVOI-CDC-AKT(ART-IX)            
185400           ELSE                                                           
185500              IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                     
185600                 ADD IN-KVOT-INNEV(KVOI-IX)                               
185700                                TO WS-ART-KVOI-TEO(ART-IX)                
185800                 ADD IN-KVOT-CDC-INNEV(KVOI-IX)                           
185900                                TO WS-ART-KVOI-CDC-TEO(ART-IX)            
186000              ELSE                                                        
186100                 IF IN-KDREFSTA = 'P'                                     
186200                    ADD IN-KVOT-INNEV(KVOI-IX)                            
186300                                   TO WS-ART-KVOI-PAS(ART-IX)             
186400                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
186500                                   TO WS-ART-KVOI-CDC-PAS(ART-IX)         
186600                 ELSE                                                     
186700                    ADD IN-KVOT-INNEV(KVOI-IX)                            
186800                                   TO WS-ART-KVOI-SAK(ART-IX)             
186900                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
187000                                   TO WS-ART-KVOI-CDC-SAK(ART-IX)         
187100                 END-IF                                                   
187200              END-IF                                                      
187300           END-IF                                                         
187400        END-IF                                                            
187500        ADD +1 TO KVOI-IX                                                 
187600     END-PERFORM                                                          
187700                                                                          
187800     IF SW-KVOI-TRAFF = NEJ                                               
187900        MOVE D-VECKA-VECKA TO KVOI-IX                                     
188000        IF IN-KDREFSTA = 'A'                                              
188100           ADD IN-KVOT-RULL(KVOI-IX)                                      
188200                              TO WS-ART-KVOI-AKT(ART-IX)                  
188300           ADD IN-KVOT-CDC-RULL(KVOI-IX)                                  
188400                              TO WS-ART-KVOI-CDC-AKT(ART-IX)              
188500        ELSE                                                              
188600           IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                        
188700              ADD IN-KVOT-RULL(KVOI-IX)                                   
188800                              TO WS-ART-KVOI-TEO(ART-IX)                  
188900              ADD IN-KVOT-CDC-RULL(KVOI-IX)                               
189000                              TO WS-ART-KVOI-CDC-TEO(ART-IX)              
189100           ELSE                                                           
189200              IF IN-KDREFSTA = 'P'                                        
189300                 ADD IN-KVOT-RULL(KVOI-IX)                                
189400                                 TO WS-ART-KVOI-PAS(ART-IX)               
189500                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
189600                                 TO WS-ART-KVOI-CDC-PAS(ART-IX)           
189700              ELSE                                                        
189800                 ADD IN-KVOT-RULL(KVOI-IX)                                
189900                                 TO WS-ART-KVOI-SAK(ART-IX)               
190000                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
190100                                 TO WS-ART-KVOI-CDC-SAK(ART-IX)           
190200              END-IF                                                      
190300           END-IF                                                         
190400        END-IF                                                            
190500     END-IF                                                               
190600                                                                          
190700*********  SERVICEGRAD OCH SPLITFAKTOR FRÅN SRS                           
190800                                                                          
190900     MOVE IN-SUINKORD    TO WS-FIXAD-SUMMA                                
191000     ADD WS-FIXAD-SUMMA  TO WS-ART-SUINKORD(ART-IX)                       
191100     ADD IN-SUFYSAVP     TO WS-ART-SUFYSAVP(ART-IX)                       
191200     ADD IN-SUAVBRP      TO WS-ART-SUAVBRP (ART-IX)                       
191300     MOVE IN-SULAGERB    TO WS-FIXAD-SUMMA                                
191400     ADD WS-FIXAD-SUMMA  TO WS-ART-SULAGERB(ART-IX)                       
191500     MOVE IN-SUSORTB     TO WS-FIXAD-SUMMA                                
191600     ADD WS-FIXAD-SUMMA  TO WS-ART-SUSORTB (ART-IX)                       
191700     .                                                                    
191800     EJECT                                                                
191900 BBC-UPPDAT-SAKN-ART SECTION.                                             
192000                                                                          
192100     MOVE +1 TO KVOI-IX                                                   
192200     MOVE NEJ TO SW-KVOI-TRAFF                                            
192300     PERFORM UNTIL KVOI-IX > 5                                            
192400        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
192500           MOVE JA TO SW-KVOI-TRAFF                                       
192600           ADD IN-KVOT-INNEV(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7              
192700           ADD IN-KVOT-CDC-INNEV(KVOI-IX)                                 
192800                                    TO WS-KVOT-CDC-SAKNAS-WDK7            
192900        END-IF                                                            
193000        ADD +1 TO KVOI-IX                                                 
193100     END-PERFORM                                                          
193200                                                                          
193300     IF SW-KVOI-TRAFF = NEJ                                               
193400        MOVE D-VECKA-VECKA TO KVOI-IX                                     
193500        ADD IN-KVOT-RULL(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7                  
193600        ADD IN-KVOT-CDC-RULL(KVOI-IX) TO WS-KVOT-CDC-SAKNAS-WDK7          
193700     END-IF                                                               
193800     .                                                                    
193900     EJECT                                                                
194000 BC-SUMMERA SECTION.                                                      
194100                                                                          
194200     PERFORM BCA-SUMMERA-RUTA                                             
194300     PERFORM BCB-SUMMERA-PRISKLASS                                        
194400     PERFORM BCC-SUMMERA-FREKVENSKLASS                                    
194500     PERFORM BCD-SUMMERA-TOTAL                                            
194600     PERFORM BCE-BERAKNINGAR-AV-TOTAL                                     
194700     .                                                                    
194800     EJECT                                                                
194900 BCA-SUMMERA-RUTA SECTION.                                                
195000                                                                          
195100     MOVE +1  TO ART-IX                                                   
195200     MOVE +72 TO ART-IX-MAX                                               
195300     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
195400                                                                          
195500*********  DISP LAGER VÄRDE/RUTA                                          
195600        IF WS-ART-KVDISP-PR-AKT(ART-IX) = ZERO                            
195700           CONTINUE                                                       
195800        ELSE                                                              
195900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
196000                          WS-ART-KVDISP-PR-AKT(ART-IX) / 1000             
196100           MOVE WS-SUMMA-KR TO ART-KVDISP-AKT(ART-IX)                     
196200        END-IF                                                            
196300                                                                          
196400        IF WS-ART-KVDISP-PR-PAS(ART-IX) = ZERO                            
196500           CONTINUE                                                       
196600        ELSE                                                              
196700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
196800                          WS-ART-KVDISP-PR-PAS(ART-IX) / 1000             
196900           MOVE WS-SUMMA-KR TO ART-KVDISP-PAS(ART-IX)                     
197000        END-IF                                                            
197100                                                                          
197200*********  LAGERVÄRDE/RUTA                                                
197300        IF WS-ART-LS-PR-AKT(ART-IX) = ZERO                                
197400           CONTINUE                                                       
197500        ELSE                                                              
197600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
197700                          WS-ART-LS-PR-AKT(ART-IX) / 1000                 
197800           MOVE WS-SUMMA-KR TO ART-LS-AKT(ART-IX)                         
197900        END-IF                                                            
198000                                                                          
198100        IF WS-ART-LS-PR-PAS(ART-IX) = ZERO                                
198200           CONTINUE                                                       
198300        ELSE                                                              
198400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
198500                          WS-ART-LS-PR-PAS(ART-IX) / 1000                 
198600           MOVE WS-SUMMA-KR TO ART-LS-PAS(ART-IX)                         
198700        END-IF                                                            
198800                                                                          
198900*********  AK-VÄRDE/RUTA                                                  
199000        IF WS-ART-AK-PR-AKT(ART-IX) = ZERO                                
199100           CONTINUE                                                       
199200        ELSE                                                              
199300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
199400                          WS-ART-AK-PR-AKT(ART-IX) / 1000                 
199500           MOVE WS-SUMMA-KR TO ART-AK-AKT(ART-IX)                         
199600        END-IF                                                            
199700                                                                          
199800        IF WS-ART-AK-PR-PAS(ART-IX) = ZERO                                
199900           CONTINUE                                                       
200000        ELSE                                                              
200100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
200200                          WS-ART-AK-PR-PAS(ART-IX) / 1000                 
200300           MOVE WS-SUMMA-KR TO ART-AK-PAS(ART-IX)                         
200400        END-IF                                                            
200500                                                                          
200600*********  SÄKERHETSLAGER/RUTA                                            
200700        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
200800           CONTINUE                                                       
200900        ELSE                                                              
201000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
201100                          WS-ART-SLAGER(ART-IX) / 1000                    
201200           MOVE WS-SUMMA-KR TO ART-SLAGER(ART-IX)                         
201300        END-IF                                                            
201400                                                                          
201500*********  ÖVERLAGER/RUTA                                                 
201600        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
201700           CONTINUE                                                       
201800        ELSE                                                              
201900           COMPUTE WS-SUMMA-KR ROUNDED                                    
202000                              = WS-ART-OLAGER(ART-IX) / 1000              
202100           MOVE WS-SUMMA-KR TO ART-OLAGER(ART-IX)                         
202200        END-IF                                                            
202300                                                                          
202400*********  MEDELLAGER/RUTA                                                
202500        IF WS-ART-MLAGER(ART-IX) = ZERO                                   
202600           CONTINUE                                                       
202700        ELSE                                                              
202800           COMPUTE WS-SUMMA-KR =                                          
202900                                WS-ART-MLAGER(ART-IX) / 1000              
203000           ADD WS-SUMMA-KR TO ART-MLAGER(ART-IX)                          
203100        END-IF                                                            
203200                                                                          
203300*********  OMSHASTIGHET/RUTA                                              
203400        COMPUTE WS-SUMMA = WS-ART-KVDISP-PR-AKT(ART-IX) +                 
203500                           WS-ART-KVDISP-PR-PAS(ART-IX)                   
203600        IF WS-SUMMA = ZERO                                                
203700           CONTINUE                                                       
203800        ELSE                                                              
203900           COMPUTE WS-OMSHAST ROUNDED =                                   
204000               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
204100           MOVE WS-OMSHAST TO ART-OMSHAST-DISP(ART-IX)                    
204200        END-IF                                                            
204300                                                                          
204400        COMPUTE WS-SUMMA = WS-ART-LS-PR-AKT(ART-IX) +                     
204500                           WS-ART-AK-PR-AKT(ART-IX) +                     
204600                           WS-ART-LS-PR-PAS(ART-IX) +                     
204700                           WS-ART-AK-PR-PAS(ART-IX)                       
204800        IF WS-SUMMA = ZERO                                                
204900           CONTINUE                                                       
205000        ELSE                                                              
205100           COMPUTE WS-OMSHAST ROUNDED =                                   
205200               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
205300           MOVE WS-OMSHAST TO ART-OMSHAST-LS(ART-IX)                      
205400        END-IF                                                            
205500                                                                          
205600*********  SERVICEGRAD BRUTTO/RUTA                                        
205700        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
205800           MOVE 99.9   TO ART-SERVG-BTO(ART-IX)                           
205900        ELSE                                                              
206000           COMPUTE WS-SERVG ROUNDED =                                     
206100             WS-ART-SUAVBRP(ART-IX) * 100 /                               
206200                          WS-ART-SUINKORD(ART-IX)                         
206300           IF WS-SERVG = 100.0                                            
206400              MOVE 99.9 TO ART-SERVG-BTO(ART-IX)                          
206500           ELSE                                                           
206600              MOVE WS-SERVG TO ART-SERVG-BTO(ART-IX)                      
206700           END-IF                                                         
206800        END-IF                                                            
206900                                                                          
207000*********  SERVICEGRAD NETTO/RUTA                                         
207100        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
207200           MOVE 99.9   TO ART-SERVG-NTO(ART-IX)                           
207300        ELSE                                                              
207400           COMPUTE WS-SERVG ROUNDED =                                     
207500             (WS-ART-SUAVBRP(ART-IX) - WS-ART-SUFYSAVP(ART-IX))           
207600                           * 100 /                                        
207700                          WS-ART-SUINKORD(ART-IX)                         
207800           IF WS-SERVG = 100.0                                            
207900              MOVE 99.9 TO ART-SERVG-NTO(ART-IX)                          
208000           ELSE                                                           
208100              MOVE WS-SERVG TO ART-SERVG-NTO(ART-IX)                      
208200           END-IF                                                         
208300        END-IF                                                            
208400                                                                          
208500*********  SPLITFAKTOR/RUTA                                               
208600        IF (WS-ART-SUINKORD(ART-IX) +                                     
208700            WS-ART-SULAGERB(ART-IX) +                                     
208800            WS-ART-SUSORTB (ART-IX))  = ZERO                              
208900           MOVE 99.9 TO ART-SPLIT(ART-IX)                                 
209000        ELSE                                                              
209100           COMPUTE WS-SERVG ROUNDED = (WS-ART-SUINKORD(ART-IX) +          
209200                         WS-ART-SULAGERB (ART-IX)) * 100                  
209300                        / (WS-ART-SUINKORD(ART-IX) +                      
209400                           WS-ART-SULAGERB(ART-IX) +                      
209500                           WS-ART-SUSORTB(ART-IX))                        
209600           IF WS-SERVG = 100.0                                            
209700              MOVE 99.9 TO ART-SPLIT(ART-IX)                              
209800           ELSE                                                           
209900              MOVE WS-SERVG TO ART-SPLIT(ART-IX)                          
210000           END-IF                                                         
210100        END-IF                                                            
210200*********  ORDERTRÄFFAR/RUTA                                              
210300*       COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
210400*                                   WS-ART-KVOI-PAS(ART-IX) +             
210500*                                   WS-ART-KVOI-TEO(ART-IX) +             
210600*                                   WS-ART-KVOI-SAK(ART-IX)               
210700*       ADD WS-KVOI-TOT-VECKA TO ART-KVOT(ART-IX)                         
210800                                                                          
210900        MOVE WS-ART-SUINKORD(ART-IX) TO ART-KVOT(ART-IX)                  
211000                                                                          
211100        ADD +1  TO ART-IX                                                 
211200     END-PERFORM                                                          
211300     .                                                                    
211400     EJECT                                                                
211500 BCB-SUMMERA-PRISKLASS SECTION.                                           
211600******************************************************************        
211700* SUMMERING PER PRISKLASS                                        *        
211800******************************************************************        
211900                                                                          
212000     MOVE +1 TO PSUM-IX                                                   
212100                ART-IX                                                    
212200     MOVE +8 TO ART-IX-MAX                                                
212300                                                                          
212400     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
212500        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
212600                                                                          
212700          ADD ART-KVANT-AKT(ART-IX) TO PSUM-KVANT-AKT(PSUM-IX)            
212800          ADD ART-KVANT-PAS(ART-IX) TO PSUM-KVANT-PAS(PSUM-IX)            
212900          ADD ART-KVOT(ART-IX)      TO PSUM-KVOT(PSUM-IX)                 
213000          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-PSUM-KVOI-AKT(PSUM-IX)        
213100          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-PSUM-KVOI-PAS(PSUM-IX)        
213200          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-PSUM-KVOI-TEO(PSUM-IX)        
213300          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-PSUM-KVOI-SAK(PSUM-IX)        
213400          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
213500                                  TO WS-PSUM-KVOI-CDC-AKT(PSUM-IX)        
213600          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
213700                                  TO WS-PSUM-KVOI-CDC-PAS(PSUM-IX)        
213800          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
213900                                  TO WS-PSUM-KVOI-CDC-TEO(PSUM-IX)        
214000          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
214100                                  TO WS-PSUM-KVOI-CDC-SAK(PSUM-IX)        
214200          ADD WS-ART-KVOI(ART-IX) TO WS-PSUM-KVOI(PSUM-IX)                
214300          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
214400                             TO WS-PSUM-KVDISP-PR-AKT (PSUM-IX)           
214500          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
214600                             TO WS-PSUM-OK-PR-AKT(PSUM-IX)                
214700          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
214800                             TO WS-PSUM-LS-PR-AKT(PSUM-IX)                
214900          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
215000                             TO WS-PSUM-AK-PR-AKT(PSUM-IX)                
215100          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
215200                             TO WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
215300          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
215400                             TO WS-PSUM-OK-PR-PAS(PSUM-IX)                
215500          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
215600                             TO WS-PSUM-LS-PR-PAS(PSUM-IX)                
215700          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
215800                             TO WS-PSUM-AK-PR-PAS(PSUM-IX)                
215900          ADD WS-ART-OLAGER(ART-IX)   TO WS-PSUM-OLAGER(PSUM-IX)          
216000          ADD WS-ART-MLAGER(ART-IX)   TO WS-PSUM-MLAGER(PSUM-IX)          
216100          ADD WS-ART-SLAGER(ART-IX)   TO WS-PSUM-SLAGER(PSUM-IX)          
216200          ADD WS-ART-SUINKORD(ART-IX) TO WS-PSUM-SUINKORD(PSUM-IX)        
216300          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-PSUM-SUFYSAVP(PSUM-IX)        
216400          ADD WS-ART-SUAVBRP (ART-IX) TO WS-PSUM-SUAVBRP (PSUM-IX)        
216500          ADD WS-ART-SULAGERB(ART-IX) TO WS-PSUM-SULAGERB(PSUM-IX)        
216600          ADD WS-ART-SUSORTB (ART-IX) TO WS-PSUM-SUSORTB (PSUM-IX)        
216700                                                                          
216800          ADD +1 TO ART-IX                                                
216900        END-PERFORM                                                       
217000                                                                          
217100        ADD +1 TO PSUM-IX                                                 
217200        ADD +8 TO ART-IX-MAX                                              
217300     END-PERFORM                                                          
217400                                                                          
217500     MOVE +1 TO PSUM-IX                                                   
217600     MOVE +9 TO PSUM-IX-MAX                                               
217700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
217800                                                                          
217900*********  DISP LAGER VÄRDE/PRISKLASS                                     
218000        IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) = ZERO                          
218100           CONTINUE                                                       
218200        ELSE                                                              
218300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
218400                     WS-PSUM-KVDISP-PR-AKT(PSUM-IX) / 1000                
218500           MOVE WS-SUMMA-KR TO PSUM-KVDISP-AKT(PSUM-IX)                   
218600        END-IF                                                            
218700                                                                          
218800        IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) = ZERO                          
218900           CONTINUE                                                       
219000        ELSE                                                              
219100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
219200                     WS-PSUM-KVDISP-PR-PAS(PSUM-IX) / 1000                
219300           MOVE WS-SUMMA-KR TO PSUM-KVDISP-PAS(PSUM-IX)                   
219400        END-IF                                                            
219500                                                                          
219600*********  LAGERVÄRDE/PRISKLASS                                           
219700        IF WS-PSUM-LS-PR-AKT(PSUM-IX) = ZERO                              
219800           CONTINUE                                                       
219900        ELSE                                                              
220000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
220100                     WS-PSUM-LS-PR-AKT(PSUM-IX) / 1000                    
220200           MOVE WS-SUMMA-KR TO PSUM-LS-AKT(PSUM-IX)                       
220300        END-IF                                                            
220400                                                                          
220500        IF WS-PSUM-LS-PR-PAS(PSUM-IX) = ZERO                              
220600           CONTINUE                                                       
220700        ELSE                                                              
220800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
220900                     WS-PSUM-LS-PR-PAS(PSUM-IX) / 1000                    
221000           MOVE WS-SUMMA-KR TO PSUM-LS-PAS(PSUM-IX)                       
221100        END-IF                                                            
221200                                                                          
221300*********  AK-VÄRDE/PRISKLASS                                             
221400        IF WS-PSUM-AK-PR-AKT(PSUM-IX) = ZERO                              
221500           CONTINUE                                                       
221600        ELSE                                                              
221700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
221800                     WS-PSUM-AK-PR-AKT(PSUM-IX) / 1000                    
221900           MOVE WS-SUMMA-KR TO PSUM-AK-AKT(PSUM-IX)                       
222000        END-IF                                                            
222100                                                                          
222200        IF WS-PSUM-AK-PR-PAS(PSUM-IX) = ZERO                              
222300           CONTINUE                                                       
222400        ELSE                                                              
222500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
222600                     WS-PSUM-AK-PR-PAS(PSUM-IX) / 1000                    
222700           MOVE WS-SUMMA-KR TO PSUM-AK-PAS(PSUM-IX)                       
222800        END-IF                                                            
222900                                                                          
223000*********  SÄKERHETSLAGER/PRISKLASS                                       
223100        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
223200           CONTINUE                                                       
223300        ELSE                                                              
223400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
223500                          WS-PSUM-SLAGER(PSUM-IX) / 1000                  
223600           MOVE WS-SUMMA-KR TO PSUM-SLAGER(PSUM-IX)                       
223700        END-IF                                                            
223800                                                                          
223900*********  ÖVERLAGER/PRISKLASS                                            
224000        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
224100           CONTINUE                                                       
224200        ELSE                                                              
224300           COMPUTE WS-SUMMA-KR ROUNDED                                    
224400                     = WS-PSUM-OLAGER(PSUM-IX) / 1000                     
224500           MOVE WS-SUMMA-KR TO PSUM-OLAGER(PSUM-IX)                       
224600        END-IF                                                            
224700                                                                          
224800*********  MEDELLAGER/PRISKLASS                                           
224900        IF WS-PSUM-MLAGER(PSUM-IX) = ZERO                                 
225000           CONTINUE                                                       
225100        ELSE                                                              
225200           COMPUTE WS-SUMMA-KR =                                          
225300                       WS-PSUM-MLAGER(PSUM-IX) / 1000                     
225400           ADD WS-SUMMA-KR TO PSUM-MLAGER(PSUM-IX)                        
225500        END-IF                                                            
225600                                                                          
225700*********  SERVICEGRAD BRUTTO/PRISKLASS                                   
225800        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
225900           MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                           
226000        ELSE                                                              
226100           COMPUTE WS-SERVG ROUNDED =                                     
226200             WS-PSUM-SUAVBRP(PSUM-IX) * 100 /                             
226300                          WS-PSUM-SUINKORD(PSUM-IX)                       
226400           IF WS-SERVG = 100.0                                            
226500              MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                        
226600           ELSE                                                           
226700              MOVE WS-SERVG TO PSUM-SERVG-BTO(PSUM-IX)                    
226800           END-IF                                                         
226900        END-IF                                                            
227000                                                                          
227100*********  SERVICEGRAD NETTO/PRISKLASS                                    
227200        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
227300           MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                           
227400        ELSE                                                              
227500           COMPUTE WS-SERVG ROUNDED =                                     
227600            (WS-PSUM-SUAVBRP(PSUM-IX) - WS-PSUM-SUFYSAVP(PSUM-IX))        
227700                           * 100 /                                        
227800                          WS-PSUM-SUINKORD(PSUM-IX)                       
227900           IF WS-SERVG = 100.0                                            
228000              MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                        
228100           ELSE                                                           
228200              MOVE WS-SERVG TO PSUM-SERVG-NTO(PSUM-IX)                    
228300           END-IF                                                         
228400        END-IF                                                            
228500                                                                          
228600*********  SPLITFAKTOR/PRISKLASS                                          
228700        IF (WS-PSUM-SUINKORD(PSUM-IX) +                                   
228800           WS-PSUM-SULAGERB(PSUM-IX) +                                    
228900           WS-PSUM-SUSORTB(PSUM-IX)) = ZERO                               
229000              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
229100        ELSE                                                              
229200           COMPUTE WS-SERVG ROUNDED = (WS-PSUM-SUINKORD(PSUM-IX)          
229300                       + WS-PSUM-SULAGERB (PSUM-IX)) * 100                
229400                        / (WS-PSUM-SUINKORD(PSUM-IX) +                    
229500                           WS-PSUM-SULAGERB(PSUM-IX) +                    
229600                           WS-PSUM-SUSORTB(PSUM-IX))                      
229700           IF WS-SERVG = 100.0                                            
229800              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
229900           ELSE                                                           
230000              MOVE WS-SERVG TO PSUM-SPLIT(PSUM-IX)                        
230100           END-IF                                                         
230200        END-IF                                                            
230300                                                                          
230400*********  OMSHASTIGHET/PRISKLASS                                         
230500        COMPUTE WS-SUMMA = WS-PSUM-KVDISP-PR-AKT(PSUM-IX) +               
230600                           WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                 
230700        IF WS-SUMMA = ZERO                                                
230800           CONTINUE                                                       
230900        ELSE                                                              
231000           COMPUTE WS-OMSHAST ROUNDED =                                   
231100               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
231200           MOVE WS-OMSHAST TO PSUM-OMSHAST-DISP(PSUM-IX)                  
231300        END-IF                                                            
231400                                                                          
231500        COMPUTE WS-SUMMA = WS-PSUM-LS-PR-AKT(PSUM-IX) +                   
231600                           WS-PSUM-AK-PR-AKT(PSUM-IX) +                   
231700                           WS-PSUM-LS-PR-PAS(PSUM-IX) +                   
231800                           WS-PSUM-AK-PR-PAS(PSUM-IX)                     
231900        IF WS-SUMMA = ZERO                                                
232000           CONTINUE                                                       
232100        ELSE                                                              
232200           COMPUTE WS-OMSHAST ROUNDED =                                   
232300               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
232400           MOVE WS-OMSHAST TO PSUM-OMSHAST-LS(PSUM-IX)                    
232500        END-IF                                                            
232600                                                                          
232700        ADD +1 TO PSUM-IX                                                 
232800     END-PERFORM                                                          
232900     .                                                                    
233000     EJECT                                                                
233100 BCC-SUMMERA-FREKVENSKLASS SECTION.                                       
233200******************************************************************        
233300* SUMMERING PER FREKVENSKLASS                                    *        
233400******************************************************************        
233500                                                                          
233600     MOVE +1 TO FSUM-IX                                                   
233700                ART-IX                                                    
233800     MOVE +65 TO ART-IX-MAX                                               
233900     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
234000                                                                          
234100        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
234200          ADD ART-KVANT-AKT(ART-IX) TO FSUM-KVANT-AKT(FSUM-IX)            
234300          ADD ART-KVANT-PAS(ART-IX) TO FSUM-KVANT-PAS(FSUM-IX)            
234400          ADD ART-KVOT(ART-IX)      TO FSUM-KVOT(FSUM-IX)                 
234500          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-FSUM-KVOI-AKT(FSUM-IX)        
234600          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-FSUM-KVOI-PAS(FSUM-IX)        
234700          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-FSUM-KVOI-TEO(FSUM-IX)        
234800          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-FSUM-KVOI-SAK(FSUM-IX)        
234900          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
235000                                  TO WS-FSUM-KVOI-CDC-AKT(FSUM-IX)        
235100          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
235200                                  TO WS-FSUM-KVOI-CDC-PAS(FSUM-IX)        
235300          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
235400                                  TO WS-FSUM-KVOI-CDC-TEO(FSUM-IX)        
235500          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
235600                                  TO WS-FSUM-KVOI-CDC-SAK(FSUM-IX)        
235700          ADD WS-ART-KVOI(ART-IX) TO WS-FSUM-KVOI(FSUM-IX)                
235800          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
235900                             TO WS-FSUM-KVDISP-PR-AKT (FSUM-IX)           
236000          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
236100                             TO WS-FSUM-OK-PR-AKT(FSUM-IX)                
236200          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
236300                             TO WS-FSUM-LS-PR-AKT(FSUM-IX)                
236400          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
236500                             TO WS-FSUM-AK-PR-AKT(FSUM-IX)                
236600          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
236700                             TO WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
236800          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
236900                             TO WS-FSUM-OK-PR-PAS(FSUM-IX)                
237000          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
237100                             TO WS-FSUM-LS-PR-PAS(FSUM-IX)                
237200          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
237300                             TO WS-FSUM-AK-PR-PAS(FSUM-IX)                
237400          ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)            
237500          ADD WS-ART-MLAGER(ART-IX) TO WS-FSUM-MLAGER(FSUM-IX)            
237600          ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)            
237700          ADD WS-ART-SUINKORD(ART-IX) TO WS-FSUM-SUINKORD(FSUM-IX)        
237800          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-FSUM-SUFYSAVP(FSUM-IX)        
237900          ADD WS-ART-SUAVBRP (ART-IX) TO WS-FSUM-SUAVBRP (FSUM-IX)        
238000          ADD WS-ART-SULAGERB(ART-IX) TO WS-FSUM-SULAGERB(FSUM-IX)        
238100          ADD WS-ART-SUSORTB (ART-IX) TO WS-FSUM-SUSORTB (FSUM-IX)        
238200                                                                          
238300          ADD +8 TO ART-IX                                                
238400        END-PERFORM                                                       
238500        ADD +1 TO FSUM-IX                                                 
238600                                                                          
238700        EVALUATE TRUE                                                     
238800           WHEN  FSUM-IX = 1                                              
238900                 MOVE +1 TO ART-IX                                        
239000           WHEN  FSUM-IX = 2                                              
239100                 MOVE +2 TO ART-IX                                        
239200                 MOVE +66 TO ART-IX-MAX                                   
239300           WHEN  FSUM-IX = 3                                              
239400                 MOVE +3 TO ART-IX                                        
239500                 MOVE +67 TO ART-IX-MAX                                   
239600           WHEN  FSUM-IX = 4                                              
239700                 MOVE +4 TO ART-IX                                        
239800                 MOVE +68 TO ART-IX-MAX                                   
239900           WHEN  FSUM-IX = 5                                              
240000                 MOVE +5 TO ART-IX                                        
240100                 MOVE +69 TO ART-IX-MAX                                   
240200           WHEN  FSUM-IX = 6                                              
240300                 MOVE +6 TO ART-IX                                        
240400                 MOVE +70 TO ART-IX-MAX                                   
240500           WHEN  FSUM-IX = 7                                              
240600                 MOVE +7 TO ART-IX                                        
240700                 MOVE +71 TO ART-IX-MAX                                   
240800           WHEN  FSUM-IX = 8                                              
240900                 MOVE +8 TO ART-IX                                        
241000                 MOVE +72 TO ART-IX-MAX                                   
241100           WHEN OTHER                                                     
241200                CONTINUE                                                  
241300        END-EVALUATE                                                      
241400                                                                          
241500     END-PERFORM                                                          
241600                                                                          
241700     MOVE +1 TO FSUM-IX                                                   
241800     MOVE +8 TO FSUM-IX-MAX                                               
241900     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
242000                                                                          
242100*********  DISP LAGER VÄRDE/FREKVENSKLASS                                 
242200        IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) = ZERO                          
242300           CONTINUE                                                       
242400        ELSE                                                              
242500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
242600                     WS-FSUM-KVDISP-PR-AKT(FSUM-IX) / 1000                
242700           MOVE WS-SUMMA-KR TO FSUM-KVDISP-AKT(FSUM-IX)                   
242800        END-IF                                                            
242900                                                                          
243000        IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) = ZERO                          
243100           CONTINUE                                                       
243200        ELSE                                                              
243300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
243400                     WS-FSUM-KVDISP-PR-PAS(FSUM-IX) / 1000                
243500           MOVE WS-SUMMA-KR TO FSUM-KVDISP-PAS(FSUM-IX)                   
243600        END-IF                                                            
243700                                                                          
243800*********  LAGERVÄRDE/FREKVENSKLASS                                       
243900        IF WS-FSUM-LS-PR-AKT(FSUM-IX) = ZERO                              
244000           CONTINUE                                                       
244100        ELSE                                                              
244200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
244300                     WS-FSUM-LS-PR-AKT(FSUM-IX) / 1000                    
244400           MOVE WS-SUMMA-KR TO FSUM-LS-AKT(FSUM-IX)                       
244500        END-IF                                                            
244600                                                                          
244700        IF WS-FSUM-LS-PR-PAS(FSUM-IX) = ZERO                              
244800           CONTINUE                                                       
244900        ELSE                                                              
245000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
245100                     WS-FSUM-LS-PR-PAS(FSUM-IX) / 1000                    
245200           MOVE WS-SUMMA-KR TO FSUM-LS-PAS(FSUM-IX)                       
245300        END-IF                                                            
245400                                                                          
245500*********  AK-VÄRDE/FREKVENSKLASS                                         
245600        IF WS-FSUM-AK-PR-AKT(FSUM-IX) = ZERO                              
245700           CONTINUE                                                       
245800        ELSE                                                              
245900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
246000                     WS-FSUM-AK-PR-AKT(FSUM-IX) / 1000                    
246100           MOVE WS-SUMMA-KR TO FSUM-AK-AKT(FSUM-IX)                       
246200        END-IF                                                            
246300                                                                          
246400        IF WS-FSUM-AK-PR-PAS(FSUM-IX) = ZERO                              
246500           CONTINUE                                                       
246600        ELSE                                                              
246700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
246800                     WS-FSUM-AK-PR-PAS(FSUM-IX) / 1000                    
246900           MOVE WS-SUMMA-KR TO FSUM-AK-PAS(FSUM-IX)                       
247000        END-IF                                                            
247100                                                                          
247200*********  SÄKERHETSLAGER/FREKVENSKLASS                                   
247300        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
247400           CONTINUE                                                       
247500        ELSE                                                              
247600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
247700                          WS-FSUM-SLAGER(FSUM-IX) / 1000                  
247800           MOVE WS-SUMMA-KR TO FSUM-SLAGER(FSUM-IX)                       
247900        END-IF                                                            
248000                                                                          
248100*********  ÖVERLAGER/FREKVENSKLASS                                        
248200        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
248300           CONTINUE                                                       
248400        ELSE                                                              
248500           COMPUTE WS-SUMMA-KR ROUNDED                                    
248600                     = WS-FSUM-OLAGER(FSUM-IX) / 1000                     
248700           MOVE WS-SUMMA-KR TO FSUM-OLAGER(FSUM-IX)                       
248800        END-IF                                                            
248900                                                                          
249000*********  MEDELLAGER/FREKVENSKLASS                                       
249100        IF WS-FSUM-MLAGER(FSUM-IX) = ZERO                                 
249200           CONTINUE                                                       
249300        ELSE                                                              
249400           COMPUTE WS-SUMMA-KR =                                          
249500                       WS-FSUM-MLAGER(FSUM-IX) / 1000                     
249600           ADD WS-SUMMA-KR TO FSUM-MLAGER(FSUM-IX)                        
249700        END-IF                                                            
249800                                                                          
249900*********  SERVICEGRAD BRUTTO/FREKVENSKLASS                               
250000        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
250100           MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                           
250200        ELSE                                                              
250300           COMPUTE WS-SERVG ROUNDED =                                     
250400             WS-FSUM-SUAVBRP(FSUM-IX) * 100 /                             
250500                          WS-FSUM-SUINKORD(FSUM-IX)                       
250600           IF WS-SERVG = 100.0                                            
250700              MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                        
250800           ELSE                                                           
250900              MOVE WS-SERVG TO FSUM-SERVG-BTO(FSUM-IX)                    
251000           END-IF                                                         
251100        END-IF                                                            
251200                                                                          
251300*********  SERVICEGRAD NETTO/PRISKLASS                                    
251400        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
251500           MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                           
251600        ELSE                                                              
251700           COMPUTE WS-SERVG ROUNDED =                                     
251800            (WS-FSUM-SUAVBRP(FSUM-IX) - WS-FSUM-SUFYSAVP(FSUM-IX))        
251900                           * 100 /                                        
252000                          WS-FSUM-SUINKORD(FSUM-IX)                       
252100           IF WS-SERVG = 100.0                                            
252200              MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                        
252300           ELSE                                                           
252400              MOVE WS-SERVG TO FSUM-SERVG-NTO(FSUM-IX)                    
252500           END-IF                                                         
252600        END-IF                                                            
252700                                                                          
252800*********  SPLITFAKTOR/FREKVENSKLASS                                      
252900        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
253000           MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                               
253100        ELSE                                                              
253200           COMPUTE WS-SERVG ROUNDED = (WS-FSUM-SUINKORD(FSUM-IX)          
253300                       + WS-FSUM-SULAGERB (FSUM-IX)) * 100                
253400                        / (WS-FSUM-SUINKORD(FSUM-IX) +                    
253500                           WS-FSUM-SULAGERB(FSUM-IX) +                    
253600                           WS-FSUM-SUSORTB(FSUM-IX))                      
253700           IF WS-SERVG = 100.0                                            
253800              MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                            
253900           ELSE                                                           
254000              MOVE WS-SERVG TO FSUM-SPLIT(FSUM-IX)                        
254100           END-IF                                                         
254200        END-IF                                                            
254300                                                                          
254400*********  OMSHASTIGHET/FREKVENSKLASS                                     
254500        COMPUTE WS-SUMMA = WS-FSUM-KVDISP-PR-AKT(FSUM-IX) +               
254600                           WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                 
254700        IF WS-SUMMA = ZERO                                                
254800           CONTINUE                                                       
254900        ELSE                                                              
255000           COMPUTE WS-OMSHAST ROUNDED =                                   
255100               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
255200           MOVE WS-OMSHAST TO FSUM-OMSHAST-DISP(FSUM-IX)                  
255300        END-IF                                                            
255400                                                                          
255500        COMPUTE WS-SUMMA = WS-FSUM-LS-PR-AKT(FSUM-IX) +                   
255600                           WS-FSUM-AK-PR-AKT(FSUM-IX) +                   
255700                           WS-FSUM-LS-PR-PAS(FSUM-IX) +                   
255800                           WS-FSUM-AK-PR-PAS(FSUM-IX)                     
255900        IF WS-SUMMA = ZERO                                                
256000           CONTINUE                                                       
256100        ELSE                                                              
256200           COMPUTE WS-OMSHAST ROUNDED =                                   
256300               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
256400           MOVE WS-OMSHAST TO FSUM-OMSHAST-LS(FSUM-IX)                    
256500        END-IF                                                            
256600                                                                          
256700        ADD +1 TO FSUM-IX                                                 
256800                                                                          
256900     END-PERFORM                                                          
257000     .                                                                    
257100     EJECT                                                                
257200 BCD-SUMMERA-TOTAL SECTION.                                               
257300******************************************************************        
257400* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
257500******************************************************************        
257600                                                                          
257700     MOVE +1 TO PSUM-IX                                                   
257800     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
257900        ADD PSUM-KVANT-AKT(PSUM-IX) TO TOT-KVANT-AKT                      
258000        ADD PSUM-KVANT-PAS(PSUM-IX) TO TOT-KVANT-PAS                      
258100        ADD PSUM-KVOT(PSUM-IX) TO TOT-KVOT                                
258200        ADD WS-PSUM-KVOI-AKT(PSUM-IX) TO WS-TOT-KVOI-AKT                  
258300        ADD WS-PSUM-KVOI-PAS(PSUM-IX) TO WS-TOT-KVOI-PAS                  
258400        ADD WS-PSUM-KVOI-TEO(PSUM-IX) TO WS-TOT-KVOI-TEO                  
258500        ADD WS-PSUM-KVOI-SAK(PSUM-IX) TO WS-TOT-KVOI-SAK                  
258600        ADD WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                                 
258700                                TO WS-TOT-KVOI-CDC-AKT                    
258800        ADD WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                                 
258900                                TO WS-TOT-KVOI-CDC-PAS                    
259000        ADD WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                                 
259100                                TO WS-TOT-KVOI-CDC-TEO                    
259200        ADD WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                                 
259300                                TO WS-TOT-KVOI-CDC-SAK                    
259400        ADD WS-PSUM-KVOI(PSUM-IX) TO WS-TOT-KVOI                          
259500        ADD WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                                
259600                           TO WS-TOT-KVDISP-PR-AKT                        
259700        ADD WS-PSUM-OK-PR-AKT(PSUM-IX)                                    
259800                           TO WS-TOT-OK-PR-AKT                            
259900        ADD WS-PSUM-LS-PR-AKT(PSUM-IX)                                    
260000                           TO WS-TOT-LS-PR-AKT                            
260100        ADD WS-PSUM-AK-PR-AKT(PSUM-IX)                                    
260200                           TO WS-TOT-AK-PR-AKT                            
260300        ADD WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                                
260400                           TO WS-TOT-KVDISP-PR-PAS                        
260500        ADD WS-PSUM-OK-PR-PAS(PSUM-IX)                                    
260600                           TO WS-TOT-OK-PR-PAS                            
260700        ADD WS-PSUM-LS-PR-PAS(PSUM-IX)                                    
260800                           TO WS-TOT-LS-PR-PAS                            
260900        ADD WS-PSUM-AK-PR-PAS(PSUM-IX)                                    
261000                           TO WS-TOT-AK-PR-PAS                            
261100        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
261200        ADD WS-PSUM-MLAGER(PSUM-IX) TO WS-TOT-MLAGER                      
261300        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
261400        ADD WS-PSUM-SUINKORD(PSUM-IX) TO WS-TOT-SUINKORD                  
261500        ADD WS-PSUM-SUFYSAVP(PSUM-IX) TO WS-TOT-SUFYSAVP                  
261600        ADD WS-PSUM-SUAVBRP (PSUM-IX) TO WS-TOT-SUAVBRP                   
261700        ADD WS-PSUM-SULAGERB(PSUM-IX) TO WS-TOT-SULAGERB                  
261800        ADD WS-PSUM-SUSORTB (PSUM-IX) TO WS-TOT-SUSORTB                   
261900                                                                          
262000        ADD +1 TO PSUM-IX                                                 
262100     END-PERFORM                                                          
262200                                                                          
262300*********  DISP LAGER VÄRDE TOTALT                                        
262400        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
262500           CONTINUE                                                       
262600        ELSE                                                              
262700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
262800                     WS-TOT-KVDISP-PR-AKT / 1000                          
262900           MOVE WS-SUMMA-KR TO TOT-KVDISP-AKT                             
263000        END-IF                                                            
263100                                                                          
263200        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
263300           CONTINUE                                                       
263400        ELSE                                                              
263500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
263600                     WS-TOT-KVDISP-PR-PAS / 1000                          
263700           MOVE WS-SUMMA-KR TO TOT-KVDISP-PAS                             
263800        END-IF                                                            
263900                                                                          
264000*********  LAGERVÄRDE TOTALT                                              
264100        IF WS-TOT-LS-PR-AKT = ZERO                                        
264200           CONTINUE                                                       
264300        ELSE                                                              
264400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
264500                     WS-TOT-LS-PR-AKT / 1000                              
264600           MOVE WS-SUMMA-KR TO TOT-LS-AKT                                 
264700        END-IF                                                            
264800                                                                          
264900        IF WS-TOT-LS-PR-PAS = ZERO                                        
265000           CONTINUE                                                       
265100        ELSE                                                              
265200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
265300                     WS-TOT-LS-PR-PAS / 1000                              
265400           MOVE WS-SUMMA-KR TO TOT-LS-PAS                                 
265500        END-IF                                                            
265600                                                                          
265700*********  AK-VÄRDE TOTALT                                                
265800        IF WS-TOT-AK-PR-AKT = ZERO                                        
265900           CONTINUE                                                       
266000        ELSE                                                              
266100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
266200                     WS-TOT-AK-PR-AKT / 1000                              
266300           MOVE WS-SUMMA-KR TO TOT-AK-AKT                                 
266400        END-IF                                                            
266500                                                                          
266600        IF WS-TOT-AK-PR-PAS = ZERO                                        
266700           CONTINUE                                                       
266800        ELSE                                                              
266900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
267000                     WS-TOT-AK-PR-PAS / 1000                              
267100           MOVE WS-SUMMA-KR TO TOT-AK-PAS                                 
267200        END-IF                                                            
267300                                                                          
267400*********  SÄKERHETSLAGER TOTALT                                          
267500        IF WS-TOT-SLAGER = ZERO                                           
267600           CONTINUE                                                       
267700        ELSE                                                              
267800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
267900                          WS-TOT-SLAGER / 1000                            
268000           MOVE WS-SUMMA-KR TO TOT-SLAGER                                 
268100        END-IF                                                            
268200                                                                          
268300*********  ÖVERLAGER TOTALT                                               
268400        IF WS-TOT-OLAGER = ZERO                                           
268500           CONTINUE                                                       
268600        ELSE                                                              
268700           COMPUTE WS-SUMMA-KR ROUNDED                                    
268800                     = WS-TOT-OLAGER / 1000                               
268900           MOVE WS-SUMMA-KR TO TOT-OLAGER                                 
269000        END-IF                                                            
269100                                                                          
269200*********  MEDELLAGER TOTALT                                              
269300        IF WS-TOT-MLAGER = ZERO                                           
269400           CONTINUE                                                       
269500        ELSE                                                              
269600           COMPUTE WS-SUMMA-KR =                                          
269700                       WS-TOT-MLAGER / 1000                               
269800           ADD WS-SUMMA-KR TO TOT-MLAGER                                  
269900        END-IF                                                            
270000                                                                          
270100*********  SERVICEGRAD BRUTTO TOTALT                                      
270200        IF WS-TOT-SUINKORD = ZERO                                         
270300           MOVE 99.9 TO TOT-SERVG-BTO                                     
270400        ELSE                                                              
270500           COMPUTE WS-SERVG ROUNDED =                                     
270600             WS-TOT-SUAVBRP * 100 /                                       
270700                          WS-TOT-SUINKORD                                 
270800           IF WS-SERVG = 100.0                                            
270900              MOVE 99.9 TO TOT-SERVG-BTO                                  
271000           ELSE                                                           
271100              MOVE WS-SERVG TO TOT-SERVG-BTO                              
271200           END-IF                                                         
271300        END-IF                                                            
271400                                                                          
271500*********  SERVICEGRAD NETTO TOTALT                                       
271600        IF WS-TOT-SUINKORD = ZERO                                         
271700           MOVE 99.9 TO TOT-SERVG-BTO                                     
271800        ELSE                                                              
271900           COMPUTE WS-SERVG ROUNDED =                                     
272000            (WS-TOT-SUAVBRP - WS-TOT-SUFYSAVP)                            
272100                           * 100 /                                        
272200                          WS-TOT-SUINKORD                                 
272300           IF WS-SERVG = 100.0                                            
272400              MOVE 99.9 TO TOT-SERVG-NTO                                  
272500           ELSE                                                           
272600              MOVE WS-SERVG TO TOT-SERVG-NTO                              
272700           END-IF                                                         
272800        END-IF                                                            
272900                                                                          
273000*********  SPLITFAKTOR TOTALT                                             
273100        IF WS-TOT-SUINKORD = ZERO                                         
273200           MOVE 99.9 TO TOT-SPLIT                                         
273300        ELSE                                                              
273400           COMPUTE WS-SERVG ROUNDED = (WS-TOT-SUINKORD                    
273500                       + WS-TOT-SULAGERB) * 100                           
273600                        / (WS-TOT-SUINKORD +                              
273700                           WS-TOT-SULAGERB +                              
273800                           WS-TOT-SUSORTB)                                
273900           IF WS-SERVG = 100.0                                            
274000              MOVE 99.9 TO TOT-SPLIT                                      
274100           ELSE                                                           
274200              MOVE WS-SERVG TO TOT-SPLIT                                  
274300           END-IF                                                         
274400        END-IF                                                            
274500                                                                          
274600*********  OMSHASTIGHET TOTALT                                            
274700        COMPUTE WS-SUMMA = WS-TOT-KVDISP-PR-AKT +                         
274800                           WS-TOT-KVDISP-PR-PAS                           
274900        IF WS-SUMMA = ZERO                                                
275000           CONTINUE                                                       
275100        ELSE                                                              
275200           COMPUTE WS-OMSHAST ROUNDED =                                   
275300               WS-TOT-KVOI / WS-SUMMA                                     
275400           MOVE WS-OMSHAST TO TOT-OMSHAST-DISP                            
275500        END-IF                                                            
275600                                                                          
275700        COMPUTE WS-SUMMA = WS-TOT-LS-PR-AKT +                             
275800                           WS-TOT-AK-PR-AKT +                             
275900                           WS-TOT-LS-PR-PAS +                             
276000                           WS-TOT-AK-PR-PAS                               
276100        IF WS-SUMMA = ZERO                                                
276200           CONTINUE                                                       
276300        ELSE                                                              
276400           COMPUTE WS-OMSHAST ROUNDED =                                   
276500               WS-TOT-KVOI / WS-SUMMA                                     
276600           MOVE WS-OMSHAST TO TOT-OMSHAST-LS                              
276700                                                                          
276800        END-IF                                                            
276900*********  ORDERTRÄFFAR TOTALT                                            
277000                                                                          
277100        ADD WS-KVOT-SAKNAS-WDK7 TO TOT-KVOT                               
277200     .                                                                    
277300     EJECT                                                                
277400 BCE-BERAKNINGAR-AV-TOTAL SECTION.                                        
277500******************************************************************        
277600* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
277700******************************************************************        
277800                                                                          
277900     MOVE +1 TO ART-IX                                                    
278000     MOVE +72 TO ART-IX-MAX                                               
278100     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
278200                                                                          
278300******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
278400        IF TOT-KVANT-AKT = ZERO                                           
278500           CONTINUE                                                       
278600        ELSE                                                              
278700           COMPUTE WS-PROC = ART-KVANT-AKT(ART-IX)                        
278800                                    * 100 / TOT-KVANT-AKT                 
278900           MOVE WS-PROC TO ART-PROC-KVANT-A(ART-IX)                       
279000        END-IF                                                            
279100                                                                          
279200******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
279300        IF TOT-KVANT-PAS = ZERO                                           
279400           CONTINUE                                                       
279500        ELSE                                                              
279600           COMPUTE WS-PROC = ART-KVANT-PAS(ART-IX)                        
279700                                    * 100 / TOT-KVANT-PAS                 
279800           MOVE WS-PROC TO ART-PROC-KVANT-P(ART-IX)                       
279900        END-IF                                                            
280000                                                                          
280100******** % ANTAL ORDERTRÄFFAR AV TOTAL                                    
280200        IF TOT-KVOT = ZERO                                                
280300           CONTINUE                                                       
280400        ELSE                                                              
280500           COMPUTE WS-PROC = ART-KVOT(ART-IX)                             
280600                                    * 100 / TOT-KVOT                      
280700           MOVE WS-PROC TO ART-PROC-KVOT(ART-IX)                          
280800        END-IF                                                            
280900                                                                          
281000*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  AKTIVA                      
281100                                                                          
281200        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
281300           CONTINUE                                                       
281400        ELSE                                                              
281500           IF WS-ART-KVDISP-PR-AKT(ART-IX) > ZERO                         
281600              COMPUTE WS-PROC = WS-ART-KVDISP-PR-AKT(ART-IX)              
281700                              * 100 / WS-TOT-KVDISP-PR-AKT                
281800              MOVE WS-PROC TO ART-PROC-KVDISP-A(ART-IX)                   
281900           END-IF                                                         
282000        END-IF                                                            
282100                                                                          
282200*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  PASSIVA                     
282300                                                                          
282400        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
282500           CONTINUE                                                       
282600        ELSE                                                              
282700           IF WS-ART-KVDISP-PR-PAS(ART-IX) > ZERO                         
282800              COMPUTE WS-PROC = WS-ART-KVDISP-PR-PAS(ART-IX)              
282900                              * 100 / WS-TOT-KVDISP-PR-PAS                
283000              MOVE WS-PROC TO ART-PROC-KVDISP-P(ART-IX)                   
283100           END-IF                                                         
283200        END-IF                                                            
283300                                                                          
283400*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                      
283500                                                                          
283600        IF WS-TOT-LS-PR-AKT = ZERO                                        
283700           CONTINUE                                                       
283800        ELSE                                                              
283900           IF WS-ART-LS-PR-AKT(ART-IX) > ZERO                             
284000              COMPUTE WS-PROC = WS-ART-LS-PR-AKT(ART-IX)                  
284100                              * 100 / WS-TOT-LS-PR-AKT                    
284200              MOVE WS-PROC TO ART-PROC-LS-A(ART-IX)                       
284300           END-IF                                                         
284400        END-IF                                                            
284500                                                                          
284600*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA                     
284700                                                                          
284800        IF WS-TOT-LS-PR-PAS = ZERO                                        
284900           CONTINUE                                                       
285000        ELSE                                                              
285100           IF WS-ART-LS-PR-PAS(ART-IX) > ZERO                             
285200              COMPUTE WS-PROC = WS-ART-LS-PR-PAS(ART-IX)                  
285300                              * 100 / WS-TOT-LS-PR-PAS                    
285400              MOVE WS-PROC TO ART-PROC-LS-P(ART-IX)                       
285500           END-IF                                                         
285600        END-IF                                                            
285700                                                                          
285800*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                          
285900                                                                          
286000        IF WS-TOT-AK-PR-AKT = ZERO                                        
286100           CONTINUE                                                       
286200        ELSE                                                              
286300           IF WS-ART-AK-PR-AKT(ART-IX) > ZERO                             
286400              COMPUTE WS-PROC = WS-ART-AK-PR-AKT(ART-IX)                  
286500                              * 100 / WS-TOT-AK-PR-AKT                    
286600              MOVE WS-PROC TO ART-PROC-AK-A(ART-IX)                       
286700           END-IF                                                         
286800        END-IF                                                            
286900                                                                          
287000*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                         
287100                                                                          
287200        IF WS-TOT-AK-PR-PAS = ZERO                                        
287300           CONTINUE                                                       
287400        ELSE                                                              
287500           IF WS-ART-AK-PR-PAS(ART-IX) > ZERO                             
287600              COMPUTE WS-PROC = WS-ART-AK-PR-PAS(ART-IX)                  
287700                              * 100 / WS-TOT-AK-PR-PAS                    
287800              MOVE WS-PROC TO ART-PROC-AK-P(ART-IX)                       
287900           END-IF                                                         
288000        END-IF                                                            
288100                                                                          
288200        ADD +1 TO ART-IX                                                  
288300     END-PERFORM                                                          
288400                                                                          
288500****************************                                              
288600                                                                          
288700     MOVE +1 TO PSUM-IX                                                   
288800     MOVE +9 TO PSUM-IX-MAX                                               
288900     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
289000                                                                          
289100******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
289200        IF TOT-KVANT-AKT = ZERO                                           
289300           CONTINUE                                                       
289400        ELSE                                                              
289500           COMPUTE WS-PROC = PSUM-KVANT-AKT(PSUM-IX) * 100 /              
289600                           TOT-KVANT-AKT                                  
289700           MOVE WS-PROC TO PSUM-PROC-KVANT-A(PSUM-IX)                     
289800        END-IF                                                            
289900                                                                          
290000******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
290100        IF TOT-KVANT-PAS = ZERO                                           
290200           CONTINUE                                                       
290300        ELSE                                                              
290400           COMPUTE WS-PROC = PSUM-KVANT-PAS(PSUM-IX) * 100 /              
290500                           TOT-KVANT-PAS                                  
290600           MOVE WS-PROC TO PSUM-PROC-KVANT-P(PSUM-IX)                     
290700        END-IF                                                            
290800                                                                          
290900******** % ANTAL ORDERTRÄFFAR AV TOTALA                                   
291000        IF TOT-KVOT = ZERO                                                
291100           CONTINUE                                                       
291200        ELSE                                                              
291300           COMPUTE WS-PROC = PSUM-KVOT(PSUM-IX) * 100 /                   
291400                           TOT-KVOT                                       
291500           MOVE WS-PROC TO PSUM-PROC-KVOT(PSUM-IX)                        
291600        END-IF                                                            
291700                                                                          
291800*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA                
291900                                                                          
292000        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
292100           CONTINUE                                                       
292200        ELSE                                                              
292300           IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) > ZERO                       
292400              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-AKT(PSUM-IX)            
292500                              * 100 / WS-TOT-KVDISP-PR-AKT                
292600              MOVE WS-PROC TO PSUM-PROC-KVDISP-A(PSUM-IX)                 
292700           END-IF                                                         
292800        END-IF                                                            
292900                                                                          
293000*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA               
293100                                                                          
293200        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
293300           CONTINUE                                                       
293400        ELSE                                                              
293500           IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) > ZERO                       
293600              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
293700                              * 100 / WS-TOT-KVDISP-PR-PAS                
293800              MOVE WS-PROC TO PSUM-PROC-KVDISP-P(PSUM-IX)                 
293900           END-IF                                                         
294000        END-IF                                                            
294100                                                                          
294200*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                
294300                                                                          
294400        IF WS-TOT-LS-PR-AKT = ZERO                                        
294500           CONTINUE                                                       
294600        ELSE                                                              
294700           IF WS-PSUM-LS-PR-AKT(PSUM-IX) > ZERO                           
294800              COMPUTE WS-PROC = WS-PSUM-LS-PR-AKT(PSUM-IX)                
294900                              * 100 / WS-TOT-LS-PR-AKT                    
295000              MOVE WS-PROC TO PSUM-PROC-LS-A(PSUM-IX)                     
295100           END-IF                                                         
295200        END-IF                                                            
295300                                                                          
295400*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA               
295500                                                                          
295600        IF WS-TOT-LS-PR-PAS = ZERO                                        
295700           CONTINUE                                                       
295800        ELSE                                                              
295900           IF WS-PSUM-LS-PR-PAS(PSUM-IX) > ZERO                           
296000              COMPUTE WS-PROC = WS-PSUM-LS-PR-PAS(PSUM-IX)                
296100                              * 100 / WS-TOT-LS-PR-PAS                    
296200              MOVE WS-PROC TO PSUM-PROC-LS-P(PSUM-IX)                     
296300           END-IF                                                         
296400        END-IF                                                            
296500                                                                          
296600*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                    
296700                                                                          
296800        IF WS-TOT-AK-PR-AKT = ZERO                                        
296900           CONTINUE                                                       
297000        ELSE                                                              
297100           IF WS-PSUM-AK-PR-AKT(PSUM-IX) > ZERO                           
297200              COMPUTE WS-PROC = WS-PSUM-AK-PR-AKT(PSUM-IX)                
297300                              * 100 / WS-TOT-AK-PR-AKT                    
297400              MOVE WS-PROC TO PSUM-PROC-AK-A(PSUM-IX)                     
297500           END-IF                                                         
297600        END-IF                                                            
297700                                                                          
297800*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                   
297900                                                                          
298000        IF WS-TOT-AK-PR-PAS = ZERO                                        
298100           CONTINUE                                                       
298200        ELSE                                                              
298300           IF WS-PSUM-AK-PR-PAS(PSUM-IX) > ZERO                           
298400              COMPUTE WS-PROC = WS-PSUM-AK-PR-PAS(PSUM-IX)                
298500                              * 100 / WS-TOT-AK-PR-PAS                    
298600              MOVE WS-PROC TO PSUM-PROC-AK-P(PSUM-IX)                     
298700           END-IF                                                         
298800        END-IF                                                            
298900                                                                          
299000        ADD +1 TO PSUM-IX                                                 
299100     END-PERFORM                                                          
299200                                                                          
299300****************************************                                  
299400                                                                          
299500     MOVE +1 TO FSUM-IX                                                   
299600     MOVE +8 TO FSUM-IX-MAX                                               
299700     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
299800                                                                          
299900******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
300000        IF TOT-KVANT-AKT = ZERO                                           
300100           CONTINUE                                                       
300200        ELSE                                                              
300300           COMPUTE WS-PROC = FSUM-KVANT-AKT(FSUM-IX) * 100 /              
300400                           TOT-KVANT-AKT                                  
300500           MOVE WS-PROC TO FSUM-PROC-KVANT-A(FSUM-IX)                     
300600        END-IF                                                            
300700                                                                          
300800******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
300900        IF TOT-KVANT-PAS = ZERO                                           
301000           CONTINUE                                                       
301100        ELSE                                                              
301200           COMPUTE WS-PROC = FSUM-KVANT-PAS(FSUM-IX) * 100 /              
301300                           TOT-KVANT-PAS                                  
301400           MOVE WS-PROC TO FSUM-PROC-KVANT-P(FSUM-IX)                     
301500        END-IF                                                            
301600                                                                          
301700******** % ANTAL ORDERTRÄFFAR TOTALA                                      
301800        IF TOT-KVOT = ZERO                                                
301900           CONTINUE                                                       
302000        ELSE                                                              
302100           COMPUTE WS-PROC = FSUM-KVOT(FSUM-IX) * 100 /                   
302200                           TOT-KVOT                                       
302300           MOVE WS-PROC TO FSUM-PROC-KVOT(FSUM-IX)                        
302400        END-IF                                                            
302500                                                                          
302600*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA            
302700                                                                          
302800        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
302900           CONTINUE                                                       
303000        ELSE                                                              
303100           IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) > ZERO                       
303200              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-AKT(FSUM-IX)            
303300                              * 100 / WS-TOT-KVDISP-PR-AKT                
303400              MOVE WS-PROC TO FSUM-PROC-KVDISP-A(FSUM-IX)                 
303500           END-IF                                                         
303600        END-IF                                                            
303700                                                                          
303800*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA           
303900                                                                          
304000        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
304100           CONTINUE                                                       
304200        ELSE                                                              
304300           IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) > ZERO                       
304400              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
304500                              * 100 / WS-TOT-KVDISP-PR-PAS                
304600              MOVE WS-PROC TO FSUM-PROC-KVDISP-P(FSUM-IX)                 
304700           END-IF                                                         
304800        END-IF                                                            
304900                                                                          
305000*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA            
305100                                                                          
305200        IF WS-TOT-LS-PR-AKT = ZERO                                        
305300           CONTINUE                                                       
305400        ELSE                                                              
305500           IF WS-FSUM-LS-PR-AKT(FSUM-IX) > ZERO                           
305600              COMPUTE WS-PROC = WS-FSUM-LS-PR-AKT(FSUM-IX)                
305700                              * 100 / WS-TOT-LS-PR-AKT                    
305800              MOVE WS-PROC TO FSUM-PROC-LS-A(FSUM-IX)                     
305900           END-IF                                                         
306000        END-IF                                                            
306100                                                                          
306200*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA           
306300                                                                          
306400        IF WS-TOT-LS-PR-PAS = ZERO                                        
306500           CONTINUE                                                       
306600        ELSE                                                              
306700           IF WS-FSUM-LS-PR-PAS(FSUM-IX) > ZERO                           
306800              COMPUTE WS-PROC = WS-FSUM-LS-PR-PAS(FSUM-IX)                
306900                              * 100 / WS-TOT-LS-PR-PAS                    
307000              MOVE WS-PROC TO FSUM-PROC-LS-P(FSUM-IX)                     
307100           END-IF                                                         
307200        END-IF                                                            
307300                                                                          
307400*******  %  FREKVENSSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA               
307500                                                                          
307600        IF WS-TOT-AK-PR-AKT = ZERO                                        
307700           CONTINUE                                                       
307800        ELSE                                                              
307900           IF WS-FSUM-AK-PR-AKT(FSUM-IX) > ZERO                           
308000              COMPUTE WS-PROC = WS-FSUM-AK-PR-AKT(FSUM-IX)                
308100                              * 100 / WS-TOT-AK-PR-AKT                    
308200              MOVE WS-PROC TO FSUM-PROC-AK-A(FSUM-IX)                     
308300           END-IF                                                         
308400        END-IF                                                            
308500                                                                          
308600*******  %  FREKVENSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA               
308700                                                                          
308800        IF WS-TOT-AK-PR-PAS = ZERO                                        
308900           CONTINUE                                                       
309000        ELSE                                                              
309100           IF WS-FSUM-AK-PR-PAS(FSUM-IX) > ZERO                           
309200              COMPUTE WS-PROC = WS-FSUM-AK-PR-PAS(FSUM-IX)                
309300                              * 100 / WS-TOT-AK-PR-PAS                    
309400              MOVE WS-PROC TO FSUM-PROC-AK-P(FSUM-IX)                     
309500           END-IF                                                         
309600        END-IF                                                            
309700                                                                          
309800        ADD +1 TO FSUM-IX                                                 
309900     END-PERFORM                                                          
310000     .                                                                    
310100     EJECT                                                                
310200 C-SKRIV-LISTA SECTION.                                                   
310300******************************************************************        
310400*  SID 1 BESTÅR AV 3 RUTRADER INKL PRISKLASS-TOTAL               *        
310500*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
310600*      3           3 RUTRADER INKL PRISKLASS-TOTAL               *        
310700*      4           1 RUTRAD   FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
310800******************************************************************        
310900                                                                          
311000     MOVE +1 TO IX1                                                       
311100     MOVE +2 TO IX2                                                       
311200     MOVE +3 TO IX3                                                       
311300     MOVE +4 TO IX4                                                       
311400     MOVE +5 TO IX5                                                       
311500     MOVE +6 TO IX6                                                       
311600     MOVE +7 TO IX7                                                       
311700     MOVE +8 TO IX8                                                       
311800     MOVE +1 TO PSUM-IX                                                   
311900                                                                          
312000*********** SKRIVER SID-1                                                 
312100                                                                          
312200     PERFORM S21A-SKRIV-RUBRIKER                                          
312300     MOVE '1' TO W001-DET1-PRISKLASS                                      
312400     PERFORM CA-FLYTTA-SKRIV-RAD                                          
312500     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
312600     ADD +1 TO PSUM-IX                                                    
312700     MOVE '2' TO W001-DET1-PRISKLASS                                      
312800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
312900                                                                          
313000     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
313100     ADD +1 TO PSUM-IX                                                    
313200     MOVE '3' TO W001-DET1-PRISKLASS                                      
313300     PERFORM CA-FLYTTA-SKRIV-RAD                                          
313400                                                                          
313500*********** SKRIVER SID-2                                                 
313600     PERFORM S21A-SKRIV-RUBRIKER                                          
313700     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
313800     ADD +1 TO PSUM-IX                                                    
313900     MOVE '4' TO W001-DET1-PRISKLASS                                      
314000     PERFORM CA-FLYTTA-SKRIV-RAD                                          
314100                                                                          
314200     ADD +1 TO PSUM-IX                                                    
314300     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
314400     MOVE '5' TO W001-DET1-PRISKLASS                                      
314500     PERFORM CA-FLYTTA-SKRIV-RAD                                          
314600                                                                          
314700     ADD +1 TO PSUM-IX                                                    
314800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
314900     MOVE '6' TO W001-DET1-PRISKLASS                                      
315000     PERFORM CA-FLYTTA-SKRIV-RAD                                          
315100                                                                          
315200*********** SKRIVER SID-3                                                 
315300     PERFORM S21A-SKRIV-RUBRIKER                                          
315400     ADD +1 TO PSUM-IX                                                    
315500     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
315600     MOVE '7' TO W001-DET1-PRISKLASS                                      
315700     PERFORM CA-FLYTTA-SKRIV-RAD                                          
315800                                                                          
315900     ADD +1 TO PSUM-IX                                                    
316000     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
316100     MOVE '8' TO W001-DET1-PRISKLASS                                      
316200     PERFORM CA-FLYTTA-SKRIV-RAD                                          
316300                                                                          
316400     ADD +1 TO PSUM-IX                                                    
316500     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
316600     MOVE '9' TO W001-DET1-PRISKLASS                                      
316700     PERFORM CA-FLYTTA-SKRIV-RAD                                          
316800                                                                          
316900*********** SKRIVER SID-4                                                 
317000     PERFORM S21A-SKRIV-RUBRIKER                                          
317100     MOVE +1 TO IX1                                                       
317200     MOVE +2 TO IX2                                                       
317300     MOVE +3 TO IX3                                                       
317400     MOVE +4 TO IX4                                                       
317500     MOVE +5 TO IX5                                                       
317600     MOVE +6 TO IX6                                                       
317700     MOVE +7 TO IX7                                                       
317800     MOVE +8 TO IX8                                                       
317900     MOVE SPACE TO W001-DET1-PRISKLASS                                    
318000     PERFORM CB-FLYTTA-SKRIV-TOT                                          
318100     .                                                                    
318200     EJECT                                                                
318300 CA-FLYTTA-SKRIV-RAD SECTION.                                             
318400                                                                          
318500     MOVE ART-KVANT-AKT(IX1)       TO  W001-DET1-KVANTA                   
318600     MOVE ART-PROC-KVANT-A(IX1)    TO  W001-DET1-P-KVANTA                 
318700     MOVE ART-KVANT-AKT(IX2)       TO  W001-DET1-KVANTB                   
318800     MOVE ART-PROC-KVANT-A(IX2)    TO  W001-DET1-P-KVANTB                 
318900     MOVE ART-KVANT-AKT(IX3)       TO  W001-DET1-KVANTC                   
319000     MOVE ART-PROC-KVANT-A(IX3)    TO  W001-DET1-P-KVANTC                 
319100     MOVE ART-KVANT-AKT(IX4)       TO  W001-DET1-KVANTD                   
319200     MOVE ART-PROC-KVANT-A(IX4)    TO  W001-DET1-P-KVANTD                 
319300     MOVE ART-KVANT-AKT(IX5)       TO  W001-DET1-KVANTE                   
319400     MOVE ART-PROC-KVANT-A(IX5)    TO  W001-DET1-P-KVANTE                 
319500     MOVE ART-KVANT-AKT(IX6)       TO  W001-DET1-KVANTF                   
319600     MOVE ART-PROC-KVANT-A(IX6)    TO  W001-DET1-P-KVANTF                 
319700     MOVE ART-KVANT-AKT(IX7)       TO  W001-DET1-KVANTG                   
319800     MOVE ART-PROC-KVANT-A(IX7)    TO  W001-DET1-P-KVANTG                 
319900     MOVE ART-KVANT-AKT(IX8)       TO  W001-DET1-KVANTH                   
320000     MOVE ART-PROC-KVANT-A(IX8)    TO  W001-DET1-P-KVANTH                 
320100     MOVE PSUM-KVANT-AKT(PSUM-IX)  TO  W001-DET1-TOT                      
320200     MOVE PSUM-PROC-KVANT-A(PSUM-IX) TO W001-DET1-P-TOT                   
320300     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
320400     MOVE +2 TO W001-SKIP                                                 
320500     PERFORM S21-SKRIV-LISTA                                              
320600                                                                          
320700     MOVE ART-KVANT-PAS(IX1)       TO  W001-DET2-KVANTA                   
320800     MOVE ART-PROC-KVANT-P(IX1)    TO  W001-DET2-P-KVANTA                 
320900     MOVE ART-KVANT-PAS(IX2)       TO  W001-DET2-KVANTB                   
321000     MOVE ART-PROC-KVANT-P(IX2)    TO  W001-DET2-P-KVANTB                 
321100     MOVE ART-KVANT-PAS(IX3)       TO  W001-DET2-KVANTC                   
321200     MOVE ART-PROC-KVANT-P(IX3)    TO  W001-DET2-P-KVANTC                 
321300     MOVE ART-KVANT-PAS(IX4)       TO  W001-DET2-KVANTD                   
321400     MOVE ART-PROC-KVANT-P(IX4)    TO  W001-DET2-P-KVANTD                 
321500     MOVE ART-KVANT-PAS(IX5)       TO  W001-DET2-KVANTE                   
321600     MOVE ART-PROC-KVANT-P(IX5)    TO  W001-DET2-P-KVANTE                 
321700     MOVE ART-KVANT-PAS(IX6)       TO  W001-DET2-KVANTF                   
321800     MOVE ART-PROC-KVANT-P(IX6)    TO  W001-DET2-P-KVANTF                 
321900     MOVE ART-KVANT-PAS(IX7)       TO  W001-DET2-KVANTG                   
322000     MOVE ART-PROC-KVANT-P(IX7)    TO  W001-DET2-P-KVANTG                 
322100     MOVE ART-KVANT-PAS(IX8)       TO  W001-DET2-KVANTH                   
322200     MOVE ART-PROC-KVANT-P(IX8)    TO  W001-DET2-P-KVANTH                 
322300     MOVE PSUM-KVANT-PAS(PSUM-IX)  TO  W001-DET2-TOT                      
322400     MOVE PSUM-PROC-KVANT-P(PSUM-IX) TO W001-DET2-P-TOT                   
322500     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
322600     MOVE +1 TO W001-SKIP                                                 
322700     PERFORM S21-SKRIV-LISTA                                              
322800                                                                          
322900     MOVE ART-KVDISP-AKT(IX1)      TO  W001-DET3-DLAGERA                  
323000     MOVE ART-PROC-KVDISP-A(IX1)   TO  W001-DET3-P-DLAGERA                
323100     MOVE ART-KVDISP-AKT(IX2)      TO  W001-DET3-DLAGERB                  
323200     MOVE ART-PROC-KVDISP-A(IX2)   TO  W001-DET3-P-DLAGERB                
323300     MOVE ART-KVDISP-AKT(IX3)      TO  W001-DET3-DLAGERC                  
323400     MOVE ART-PROC-KVDISP-A(IX3)   TO  W001-DET3-P-DLAGERC                
323500     MOVE ART-KVDISP-AKT(IX4)      TO  W001-DET3-DLAGERD                  
323600     MOVE ART-PROC-KVDISP-A(IX4)   TO  W001-DET3-P-DLAGERD                
323700     MOVE ART-KVDISP-AKT(IX5)      TO  W001-DET3-DLAGERE                  
323800     MOVE ART-PROC-KVDISP-A(IX5)   TO  W001-DET3-P-DLAGERE                
323900     MOVE ART-KVDISP-AKT(IX6)      TO  W001-DET3-DLAGERF                  
324000     MOVE ART-PROC-KVDISP-A(IX6)   TO  W001-DET3-P-DLAGERF                
324100     MOVE ART-KVDISP-AKT(IX7)      TO  W001-DET3-DLAGERG                  
324200     MOVE ART-PROC-KVDISP-A(IX7)   TO  W001-DET3-P-DLAGERG                
324300     MOVE ART-KVDISP-AKT(IX8)      TO  W001-DET3-DLAGERH                  
324400     MOVE ART-PROC-KVDISP-A(IX8)   TO  W001-DET3-P-DLAGERH                
324500     MOVE PSUM-KVDISP-AKT(PSUM-IX) TO  W001-DET3-TOT                      
324600     MOVE PSUM-PROC-KVDISP-A(PSUM-IX)                                     
324700                                   TO  W001-DET3-P-TOT                    
324800     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
324900     MOVE +1 TO W001-SKIP                                                 
325000     PERFORM S21-SKRIV-LISTA                                              
325100                                                                          
325200     MOVE ART-KVDISP-PAS(IX1)      TO  W001-DET4-DLAGERA                  
325300     MOVE ART-PROC-KVDISP-P(IX1)   TO  W001-DET4-P-DLAGERA                
325400     MOVE ART-KVDISP-PAS(IX2)      TO  W001-DET4-DLAGERB                  
325500     MOVE ART-PROC-KVDISP-P(IX2)   TO  W001-DET4-P-DLAGERB                
325600     MOVE ART-KVDISP-PAS(IX3)      TO  W001-DET4-DLAGERC                  
325700     MOVE ART-PROC-KVDISP-P(IX3)   TO  W001-DET4-P-DLAGERC                
325800     MOVE ART-KVDISP-PAS(IX4)      TO  W001-DET4-DLAGERD                  
325900     MOVE ART-PROC-KVDISP-P(IX4)   TO  W001-DET4-P-DLAGERD                
326000     MOVE ART-KVDISP-PAS(IX5)      TO  W001-DET4-DLAGERE                  
326100     MOVE ART-PROC-KVDISP-P(IX5)   TO  W001-DET4-P-DLAGERE                
326200     MOVE ART-KVDISP-PAS(IX6)      TO  W001-DET4-DLAGERF                  
326300     MOVE ART-PROC-KVDISP-P(IX6)   TO  W001-DET4-P-DLAGERF                
326400     MOVE ART-KVDISP-PAS(IX7)      TO  W001-DET4-DLAGERG                  
326500     MOVE ART-PROC-KVDISP-P(IX7)   TO  W001-DET4-P-DLAGERG                
326600     MOVE ART-KVDISP-PAS(IX8)      TO  W001-DET4-DLAGERH                  
326700     MOVE ART-PROC-KVDISP-P(IX8)   TO  W001-DET4-P-DLAGERH                
326800     MOVE PSUM-KVDISP-PAS(PSUM-IX) TO  W001-DET4-TOT                      
326900     MOVE PSUM-PROC-KVDISP-P(PSUM-IX)                                     
327000                                   TO  W001-DET4-P-TOT                    
327100     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
327200     MOVE +1 TO W001-SKIP                                                 
327300     PERFORM S21-SKRIV-LISTA                                              
327400                                                                          
327500     MOVE ART-LS-AKT(IX1)          TO  W001-DET5-LLAGERA                  
327600     MOVE ART-PROC-LS-A(IX1)       TO  W001-DET5-P-LLAGERA                
327700     MOVE ART-LS-AKT(IX2)          TO  W001-DET5-LLAGERB                  
327800     MOVE ART-PROC-LS-A(IX2)       TO  W001-DET5-P-LLAGERB                
327900     MOVE ART-LS-AKT(IX3)          TO  W001-DET5-LLAGERC                  
328000     MOVE ART-PROC-LS-A(IX3)       TO  W001-DET5-P-LLAGERC                
328100     MOVE ART-LS-AKT(IX4)          TO  W001-DET5-LLAGERD                  
328200     MOVE ART-PROC-LS-A(IX4)       TO  W001-DET5-P-LLAGERD                
328300     MOVE ART-LS-AKT(IX5)          TO  W001-DET5-LLAGERE                  
328400     MOVE ART-PROC-LS-A(IX5)       TO  W001-DET5-P-LLAGERE                
328500     MOVE ART-LS-AKT(IX6)          TO  W001-DET5-LLAGERF                  
328600     MOVE ART-PROC-LS-A(IX6)       TO  W001-DET5-P-LLAGERF                
328700     MOVE ART-LS-AKT(IX7)          TO  W001-DET5-LLAGERG                  
328800     MOVE ART-PROC-LS-A(IX7)       TO  W001-DET5-P-LLAGERG                
328900     MOVE ART-LS-AKT(IX8)          TO  W001-DET5-LLAGERH                  
329000     MOVE ART-PROC-LS-A(IX8)       TO  W001-DET5-P-LLAGERH                
329100     MOVE PSUM-LS-AKT(PSUM-IX)     TO  W001-DET5-TOT                      
329200     MOVE PSUM-PROC-LS-A(PSUM-IX)  TO  W001-DET5-P-TOT                    
329300     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
329400     MOVE +1 TO W001-SKIP                                                 
329500     PERFORM S21-SKRIV-LISTA                                              
329600                                                                          
329700     MOVE ART-LS-PAS(IX1)          TO  W001-DET6-LLAGERA                  
329800     MOVE ART-PROC-LS-P(IX1)       TO  W001-DET6-P-LLAGERA                
329900     MOVE ART-LS-PAS(IX2)          TO  W001-DET6-LLAGERB                  
330000     MOVE ART-PROC-LS-P(IX2)       TO  W001-DET6-P-LLAGERB                
330100     MOVE ART-LS-PAS(IX3)          TO  W001-DET6-LLAGERC                  
330200     MOVE ART-PROC-LS-P(IX3)       TO  W001-DET6-P-LLAGERC                
330300     MOVE ART-LS-PAS(IX4)          TO  W001-DET6-LLAGERD                  
330400     MOVE ART-PROC-LS-P(IX4)       TO  W001-DET6-P-LLAGERD                
330500     MOVE ART-LS-PAS(IX5)          TO  W001-DET6-LLAGERE                  
330600     MOVE ART-PROC-LS-P(IX5)       TO  W001-DET6-P-LLAGERE                
330700     MOVE ART-LS-PAS(IX6)          TO  W001-DET6-LLAGERF                  
330800     MOVE ART-PROC-LS-P(IX6)       TO  W001-DET6-P-LLAGERF                
330900     MOVE ART-LS-PAS(IX7)          TO  W001-DET6-LLAGERG                  
331000     MOVE ART-PROC-LS-P(IX7)       TO  W001-DET6-P-LLAGERG                
331100     MOVE ART-LS-PAS(IX8)          TO  W001-DET6-LLAGERH                  
331200     MOVE ART-PROC-LS-P(IX8)       TO  W001-DET6-P-LLAGERH                
331300     MOVE PSUM-LS-PAS(PSUM-IX)     TO  W001-DET6-TOT                      
331400     MOVE PSUM-PROC-LS-P(PSUM-IX)  TO  W001-DET6-P-TOT                    
331500     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
331600     MOVE +1 TO W001-SKIP                                                 
331700     PERFORM S21-SKRIV-LISTA                                              
331800                                                                          
331900     MOVE ART-AK-AKT(IX1)          TO  W001-DET7-ALAGERA                  
332000     MOVE ART-PROC-AK-A(IX1)       TO  W001-DET7-P-ALAGERA                
332100     MOVE ART-AK-AKT(IX2)          TO  W001-DET7-ALAGERB                  
332200     MOVE ART-PROC-AK-A(IX2)       TO  W001-DET7-P-ALAGERB                
332300     MOVE ART-AK-AKT(IX3)          TO  W001-DET7-ALAGERC                  
332400     MOVE ART-PROC-AK-A(IX3)       TO  W001-DET7-P-ALAGERC                
332500     MOVE ART-AK-AKT(IX4)          TO  W001-DET7-ALAGERD                  
332600     MOVE ART-PROC-AK-A(IX4)       TO  W001-DET7-P-ALAGERD                
332700     MOVE ART-AK-AKT(IX5)          TO  W001-DET7-ALAGERE                  
332800     MOVE ART-PROC-AK-A(IX5)       TO  W001-DET7-P-ALAGERE                
332900     MOVE ART-AK-AKT(IX6)          TO  W001-DET7-ALAGERF                  
333000     MOVE ART-PROC-AK-A(IX6)       TO  W001-DET7-P-ALAGERF                
333100     MOVE ART-AK-AKT(IX7)          TO  W001-DET7-ALAGERG                  
333200     MOVE ART-PROC-AK-A(IX7)       TO  W001-DET7-P-ALAGERG                
333300     MOVE ART-AK-AKT(IX8)          TO  W001-DET7-ALAGERH                  
333400     MOVE ART-PROC-AK-A(IX8)       TO  W001-DET7-P-ALAGERH                
333500     MOVE PSUM-AK-AKT(PSUM-IX)     TO  W001-DET7-TOT                      
333600     MOVE PSUM-PROC-AK-A(PSUM-IX)  TO  W001-DET7-P-TOT                    
333700     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
333800     MOVE +1 TO W001-SKIP                                                 
333900     PERFORM S21-SKRIV-LISTA                                              
334000                                                                          
334100     MOVE ART-AK-PAS(IX1)          TO  W001-DET8-ALAGERA                  
334200     MOVE ART-PROC-AK-P(IX1)       TO  W001-DET8-P-ALAGERA                
334300     MOVE ART-AK-PAS(IX2)          TO  W001-DET8-ALAGERB                  
334400     MOVE ART-PROC-AK-P(IX2)       TO  W001-DET8-P-ALAGERB                
334500     MOVE ART-AK-PAS(IX3)          TO  W001-DET8-ALAGERC                  
334600     MOVE ART-PROC-AK-P(IX3)       TO  W001-DET8-P-ALAGERC                
334700     MOVE ART-AK-PAS(IX4)          TO  W001-DET8-ALAGERD                  
334800     MOVE ART-PROC-AK-P(IX4)       TO  W001-DET8-P-ALAGERD                
334900     MOVE ART-AK-PAS(IX5)          TO  W001-DET8-ALAGERE                  
335000     MOVE ART-PROC-AK-P(IX5)       TO  W001-DET8-P-ALAGERE                
335100     MOVE ART-AK-PAS(IX6)          TO  W001-DET8-ALAGERF                  
335200     MOVE ART-PROC-AK-P(IX6)       TO  W001-DET8-P-ALAGERF                
335300     MOVE ART-AK-PAS(IX7)          TO  W001-DET8-ALAGERG                  
335400     MOVE ART-PROC-AK-P(IX7)       TO  W001-DET8-P-ALAGERG                
335500     MOVE ART-AK-PAS(IX8)          TO  W001-DET8-ALAGERH                  
335600     MOVE ART-PROC-AK-P(IX8)       TO  W001-DET8-P-ALAGERH                
335700     MOVE PSUM-AK-PAS(PSUM-IX)     TO  W001-DET8-TOT                      
335800     MOVE PSUM-PROC-AK-P(PSUM-IX)  TO  W001-DET8-P-TOT                    
335900     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
336000     MOVE +1 TO W001-SKIP                                                 
336100     PERFORM S21-SKRIV-LISTA                                              
336200                                                                          
336300     MOVE ART-OLAGER(IX1)          TO  W001-DET9-OLAGERA                  
336400     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET9-P-OLAGERA                
336500     MOVE ART-OLAGER(IX2)          TO  W001-DET9-OLAGERB                  
336600     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET9-P-OLAGERB                
336700     MOVE ART-OLAGER(IX3)          TO  W001-DET9-OLAGERC                  
336800     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET9-P-OLAGERC                
336900     MOVE ART-OLAGER(IX4)          TO  W001-DET9-OLAGERD                  
337000     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET9-P-OLAGERD                
337100     MOVE ART-OLAGER(IX5)          TO  W001-DET9-OLAGERE                  
337200     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET9-P-OLAGERE                
337300     MOVE ART-OLAGER(IX6)          TO  W001-DET9-OLAGERF                  
337400     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET9-P-OLAGERF                
337500     MOVE ART-OLAGER(IX7)          TO  W001-DET9-OLAGERG                  
337600     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET9-P-OLAGERG                
337700     MOVE ART-OLAGER(IX8)          TO  W001-DET9-OLAGERH                  
337800     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET9-P-OLAGERH                
337900     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET9-TOT                      
338000     MOVE PSUM-PROC-OLAGER(PSUM-IX)                                       
338100                                   TO  W001-DET9-P-TOT                    
338200     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
338300     MOVE +1 TO W001-SKIP                                                 
338400     PERFORM S21-SKRIV-LISTA                                              
338500                                                                          
338600     MOVE ART-SLAGER(IX1)          TO  W001-DET10-SLAGERA                 
338700     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET10-P-SLAGERA               
338800     MOVE ART-SLAGER(IX2)          TO  W001-DET10-SLAGERB                 
338900     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET10-P-SLAGERB               
339000     MOVE ART-SLAGER(IX3)          TO  W001-DET10-SLAGERC                 
339100     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET10-P-SLAGERC               
339200     MOVE ART-SLAGER(IX4)          TO  W001-DET10-SLAGERD                 
339300     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET10-P-SLAGERD               
339400     MOVE ART-SLAGER(IX5)          TO  W001-DET10-SLAGERE                 
339500     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET10-P-SLAGERE               
339600     MOVE ART-SLAGER(IX6)          TO  W001-DET10-SLAGERF                 
339700     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET10-P-SLAGERF               
339800     MOVE ART-SLAGER(IX7)          TO  W001-DET10-SLAGERG                 
339900     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET10-P-SLAGERG               
340000     MOVE ART-SLAGER(IX8)          TO  W001-DET10-SLAGERH                 
340100     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET10-P-SLAGERH               
340200     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET10-TOT                     
340300     MOVE PSUM-PROC-SLAGER(PSUM-IX)                                       
340400                                   TO  W001-DET10-P-TOT                   
340500     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
340600     MOVE +1 TO W001-SKIP                                                 
340700     PERFORM S21-SKRIV-LISTA                                              
340800                                                                          
340900     MOVE ART-MLAGER(IX1)          TO  W001-DET11-MLAGERA                 
341000     MOVE ART-PROC-MLAGER(IX1)     TO  W001-DET11-P-MLAGERA               
341100     MOVE ART-MLAGER(IX2)          TO  W001-DET11-MLAGERB                 
341200     MOVE ART-PROC-MLAGER(IX2)     TO  W001-DET11-P-MLAGERB               
341300     MOVE ART-MLAGER(IX3)          TO  W001-DET11-MLAGERC                 
341400     MOVE ART-PROC-MLAGER(IX3)     TO  W001-DET11-P-MLAGERC               
341500     MOVE ART-MLAGER(IX4)          TO  W001-DET11-MLAGERD                 
341600     MOVE ART-PROC-MLAGER(IX4)     TO  W001-DET11-P-MLAGERD               
341700     MOVE ART-MLAGER(IX5)          TO  W001-DET11-MLAGERE                 
341800     MOVE ART-PROC-MLAGER(IX5)     TO  W001-DET11-P-MLAGERE               
341900     MOVE ART-MLAGER(IX6)          TO  W001-DET11-MLAGERF                 
342000     MOVE ART-PROC-MLAGER(IX6)     TO  W001-DET11-P-MLAGERF               
342100     MOVE ART-MLAGER(IX7)          TO  W001-DET11-MLAGERG                 
342200     MOVE ART-PROC-MLAGER(IX7)     TO  W001-DET11-P-MLAGERG               
342300     MOVE ART-MLAGER(IX8)          TO  W001-DET11-MLAGERH                 
342400     MOVE ART-PROC-MLAGER(IX8)     TO  W001-DET11-P-MLAGERH               
342500     MOVE PSUM-MLAGER(PSUM-IX)     TO  W001-DET11-TOT                     
342600     MOVE PSUM-PROC-MLAGER(PSUM-IX)                                       
342700                                   TO  W001-DET11-P-TOT                   
342800     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
342900     MOVE +1 TO W001-SKIP                                                 
343000     PERFORM S21-SKRIV-LISTA                                              
343100                                                                          
343200     MOVE ART-KVOT(IX1)            TO  W001-DET12-KVOTA                   
343300     MOVE ART-PROC-KVOT(IX1)       TO  W001-DET12-P-KVOTA                 
343400     MOVE ART-KVOT(IX2)            TO  W001-DET12-KVOTB                   
343500     MOVE ART-PROC-KVOT(IX2)       TO  W001-DET12-P-KVOTB                 
343600     MOVE ART-KVOT(IX3)            TO  W001-DET12-KVOTC                   
343700     MOVE ART-PROC-KVOT(IX3)       TO  W001-DET12-P-KVOTC                 
343800     MOVE ART-KVOT(IX4)            TO  W001-DET12-KVOTD                   
343900     MOVE ART-PROC-KVOT(IX4)       TO  W001-DET12-P-KVOTD                 
344000     MOVE ART-KVOT(IX5)            TO  W001-DET12-KVOTE                   
344100     MOVE ART-PROC-KVOT(IX5)       TO  W001-DET12-P-KVOTE                 
344200     MOVE ART-KVOT(IX6)            TO  W001-DET12-KVOTF                   
344300     MOVE ART-PROC-KVOT(IX6)       TO  W001-DET12-P-KVOTF                 
344400     MOVE ART-KVOT(IX7)            TO  W001-DET12-KVOTG                   
344500     MOVE ART-PROC-KVOT(IX7)       TO  W001-DET12-P-KVOTG                 
344600     MOVE ART-KVOT(IX8)            TO  W001-DET12-KVOTH                   
344700     MOVE ART-PROC-KVOT(IX8)       TO  W001-DET12-P-KVOTH                 
344800     MOVE PSUM-KVOT(PSUM-IX)       TO  W001-DET12-TOT                     
344900     MOVE PSUM-PROC-KVOT(PSUM-IX)  TO  W001-DET12-P-TOT                   
345000     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
345100     MOVE +1 TO W001-SKIP                                                 
345200     PERFORM S21-SKRIV-LISTA                                              
345300                                                                          
345400     MOVE ART-SPLIT(IX1)           TO  W001-DET13-SPLITA                  
345500     MOVE ART-SPLIT(IX2)           TO  W001-DET13-SPLITB                  
345600     MOVE ART-SPLIT(IX3)           TO  W001-DET13-SPLITC                  
345700     MOVE ART-SPLIT(IX4)           TO  W001-DET13-SPLITD                  
345800     MOVE ART-SPLIT(IX5)           TO  W001-DET13-SPLITE                  
345900     MOVE ART-SPLIT(IX6)           TO  W001-DET13-SPLITF                  
346000     MOVE ART-SPLIT(IX7)           TO  W001-DET13-SPLITG                  
346100     MOVE ART-SPLIT(IX8)           TO  W001-DET13-SPLITH                  
346200     MOVE PSUM-SPLIT(PSUM-IX)      TO  W001-DET13-TOT                     
346300     MOVE ZERO                     TO  W001-DET13-P-TOT                   
346400     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
346500     MOVE +1 TO W001-SKIP                                                 
346600     PERFORM S21-SKRIV-LISTA                                              
346700                                                                          
346800     MOVE ART-OMSHAST-DISP(IX1)    TO  W001-DET14-OMSHASTA                
346900     MOVE ART-OMSHAST-PROC-D(IX1)  TO  W001-DET14-P-OMSHASTA              
347000     MOVE ART-OMSHAST-DISP(IX2)    TO  W001-DET14-OMSHASTB                
347100     MOVE ART-OMSHAST-PROC-D(IX2)  TO  W001-DET14-P-OMSHASTB              
347200     MOVE ART-OMSHAST-DISP(IX3)    TO  W001-DET14-OMSHASTC                
347300     MOVE ART-OMSHAST-PROC-D(IX3)  TO  W001-DET14-P-OMSHASTC              
347400     MOVE ART-OMSHAST-DISP(IX4)    TO  W001-DET14-OMSHASTD                
347500     MOVE ART-OMSHAST-PROC-D(IX4)  TO  W001-DET14-P-OMSHASTD              
347600     MOVE ART-OMSHAST-DISP(IX5)    TO  W001-DET14-OMSHASTE                
347700     MOVE ART-OMSHAST-PROC-D(IX5)  TO  W001-DET14-P-OMSHASTE              
347800     MOVE ART-OMSHAST-DISP(IX6)    TO  W001-DET14-OMSHASTF                
347900     MOVE ART-OMSHAST-PROC-D(IX6)  TO  W001-DET14-P-OMSHASTF              
348000     MOVE ART-OMSHAST-DISP(IX7)    TO  W001-DET14-OMSHASTG                
348100     MOVE ART-OMSHAST-PROC-D(IX7)  TO  W001-DET14-P-OMSHASTG              
348200     MOVE ART-OMSHAST-DISP(IX8)    TO  W001-DET14-OMSHASTH                
348300     MOVE ART-OMSHAST-PROC-D(IX8)  TO  W001-DET14-P-OMSHASTH              
348400     MOVE PSUM-OMSHAST-DISP(PSUM-IX) TO W001-DET14-TOT                    
348500     MOVE PSUM-OMSHAST-PROC-D(PSUM-IX)                                    
348600                                     TO W001-DET14-P-TOT                  
348700     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
348800     MOVE +1 TO W001-SKIP                                                 
348900     PERFORM S21-SKRIV-LISTA                                              
349000                                                                          
349100     MOVE ART-OMSHAST-LS(IX1)      TO  W001-DET15-OMSHASTA                
349200     MOVE ART-OMSHAST-PROC-LS(IX1) TO  W001-DET15-P-OMSHASTA              
349300     MOVE ART-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB                
349400     MOVE ART-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB              
349500     MOVE ART-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC                
349600     MOVE ART-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC              
349700     MOVE ART-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD                
349800     MOVE ART-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD              
349900     MOVE ART-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE                
350000     MOVE ART-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE              
350100     MOVE ART-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF                
350200     MOVE ART-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF              
350300     MOVE ART-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG                
350400     MOVE ART-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG              
350500     MOVE ART-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH                
350600     MOVE ART-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH              
350700     MOVE PSUM-OMSHAST-LS(PSUM-IX) TO  W001-DET15-TOT                     
350800     MOVE PSUM-OMSHAST-PROC-LS(PSUM-IX)                                   
350900                                     TO W001-DET15-P-TOT                  
351000     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
351100     MOVE +1 TO W001-SKIP                                                 
351200     PERFORM S21-SKRIV-LISTA                                              
351300                                                                          
351400     MOVE ART-SERVG-BTO(IX1)       TO  W001-DET16-SERVG-BTOA              
351500     MOVE ART-SERVG-BTO(IX2)       TO  W001-DET16-SERVG-BTOB              
351600     MOVE ART-SERVG-BTO(IX3)       TO  W001-DET16-SERVG-BTOC              
351700     MOVE ART-SERVG-BTO(IX4)       TO  W001-DET16-SERVG-BTOD              
351800     MOVE ART-SERVG-BTO(IX5)       TO  W001-DET16-SERVG-BTOE              
351900     MOVE ART-SERVG-BTO(IX6)       TO  W001-DET16-SERVG-BTOF              
352000     MOVE ART-SERVG-BTO(IX7)       TO  W001-DET16-SERVG-BTOG              
352100     MOVE ART-SERVG-BTO(IX8)       TO  W001-DET16-SERVG-BTOH              
352200     MOVE PSUM-SERVG-BTO (PSUM-IX) TO  W001-DET16-TOT                     
352300     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
352400     MOVE +1 TO W001-SKIP                                                 
352500     PERFORM S21-SKRIV-LISTA                                              
352600                                                                          
352700     MOVE ART-SERVG-NTO(IX1)       TO  W001-DET17-SERVG-NTOA              
352800     MOVE ART-SERVG-NTO(IX2)       TO  W001-DET17-SERVG-NTOB              
352900     MOVE ART-SERVG-NTO(IX3)       TO  W001-DET17-SERVG-NTOC              
353000     MOVE ART-SERVG-NTO(IX4)       TO  W001-DET17-SERVG-NTOD              
353100     MOVE ART-SERVG-NTO(IX5)       TO  W001-DET17-SERVG-NTOE              
353200     MOVE ART-SERVG-NTO(IX6)       TO  W001-DET17-SERVG-NTOF              
353300     MOVE ART-SERVG-NTO(IX7)       TO  W001-DET17-SERVG-NTOG              
353400     MOVE ART-SERVG-NTO(IX8)       TO  W001-DET17-SERVG-NTOH              
353500     MOVE PSUM-SERVG-NTO(PSUM-IX)  TO  W001-DET17-TOT                     
353600     MOVE ZERO                     TO  W001-DET17-P-TOT                   
353700     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
353800     MOVE +1 TO W001-SKIP                                                 
353900     PERFORM S21-SKRIV-LISTA                                              
354000     .                                                                    
354100     EJECT                                                                
354200 CB-FLYTTA-SKRIV-TOT SECTION.                                             
354300                                                                          
354400     MOVE FSUM-KVANT-AKT(IX1)      TO  W001-DET1-KVANTA                   
354500     MOVE FSUM-PROC-KVANT-A(IX1)   TO  W001-DET1-P-KVANTA                 
354600     MOVE FSUM-KVANT-AKT(IX2)      TO  W001-DET1-KVANTB                   
354700     MOVE FSUM-PROC-KVANT-A(IX2)   TO  W001-DET1-P-KVANTB                 
354800     MOVE FSUM-KVANT-AKT(IX3)      TO  W001-DET1-KVANTC                   
354900     MOVE FSUM-PROC-KVANT-A(IX3)   TO  W001-DET1-P-KVANTC                 
355000     MOVE FSUM-KVANT-AKT(IX4)      TO  W001-DET1-KVANTD                   
355100     MOVE FSUM-PROC-KVANT-A(IX4)   TO  W001-DET1-P-KVANTD                 
355200     MOVE FSUM-KVANT-AKT(IX5)      TO  W001-DET1-KVANTE                   
355300     MOVE FSUM-PROC-KVANT-A(IX5)   TO  W001-DET1-P-KVANTE                 
355400     MOVE FSUM-KVANT-AKT(IX6)      TO  W001-DET1-KVANTF                   
355500     MOVE FSUM-PROC-KVANT-A(IX6)   TO  W001-DET1-P-KVANTF                 
355600     MOVE FSUM-KVANT-AKT(IX7)      TO  W001-DET1-KVANTG                   
355700     MOVE FSUM-PROC-KVANT-A(IX7)   TO  W001-DET1-P-KVANTG                 
355800     MOVE FSUM-KVANT-AKT(IX8)      TO  W001-DET1-KVANTH                   
355900     MOVE FSUM-PROC-KVANT-A(IX8)   TO  W001-DET1-P-KVANTH                 
356000     MOVE TOT-KVANT-AKT            TO  W001-DET1-TOT                      
356100     MOVE ZERO                     TO  W001-DET1-P-TOT                    
356200     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
356300     MOVE +2 TO W001-SKIP                                                 
356400     PERFORM S21-SKRIV-LISTA                                              
356500                                                                          
356600     MOVE FSUM-KVANT-PAS(IX1)      TO  W001-DET2-KVANTA                   
356700     MOVE FSUM-PROC-KVANT-P(IX1)   TO  W001-DET2-P-KVANTA                 
356800     MOVE FSUM-KVANT-PAS(IX2)      TO  W001-DET2-KVANTB                   
356900     MOVE FSUM-PROC-KVANT-P(IX2)   TO  W001-DET2-P-KVANTB                 
357000     MOVE FSUM-KVANT-PAS(IX3)      TO  W001-DET2-KVANTC                   
357100     MOVE FSUM-PROC-KVANT-P(IX3)   TO  W001-DET2-P-KVANTC                 
357200     MOVE FSUM-KVANT-PAS(IX4)      TO  W001-DET2-KVANTD                   
357300     MOVE FSUM-PROC-KVANT-P(IX4)   TO  W001-DET2-P-KVANTD                 
357400     MOVE FSUM-KVANT-PAS(IX5)      TO  W001-DET2-KVANTE                   
357500     MOVE FSUM-PROC-KVANT-P(IX5)   TO  W001-DET2-P-KVANTE                 
357600     MOVE FSUM-KVANT-PAS(IX6)      TO  W001-DET2-KVANTF                   
357700     MOVE FSUM-PROC-KVANT-P(IX6)   TO  W001-DET2-P-KVANTF                 
357800     MOVE FSUM-KVANT-PAS(IX7)      TO  W001-DET2-KVANTG                   
357900     MOVE FSUM-PROC-KVANT-P(IX7)   TO  W001-DET2-P-KVANTG                 
358000     MOVE FSUM-KVANT-PAS(IX8)      TO  W001-DET2-KVANTH                   
358100     MOVE FSUM-PROC-KVANT-P(IX8)   TO  W001-DET2-P-KVANTH                 
358200     MOVE TOT-KVANT-PAS            TO  W001-DET2-TOT                      
358300     MOVE ZERO                     TO  W001-DET2-P-TOT                    
358400     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
358500     MOVE +1 TO W001-SKIP                                                 
358600     PERFORM S21-SKRIV-LISTA                                              
358700                                                                          
358800     MOVE FSUM-KVDISP-AKT(IX1)     TO  W001-DET3-DLAGERA                  
358900     MOVE FSUM-PROC-KVDISP-A(IX1)  TO  W001-DET3-P-DLAGERA                
359000     MOVE FSUM-KVDISP-AKT(IX2)     TO  W001-DET3-DLAGERB                  
359100     MOVE FSUM-PROC-KVDISP-A(IX2)  TO  W001-DET3-P-DLAGERB                
359200     MOVE FSUM-KVDISP-AKT(IX3)     TO  W001-DET3-DLAGERC                  
359300     MOVE FSUM-PROC-KVDISP-A(IX3)  TO  W001-DET3-P-DLAGERC                
359400     MOVE FSUM-KVDISP-AKT(IX4)     TO  W001-DET3-DLAGERD                  
359500     MOVE FSUM-PROC-KVDISP-A(IX4)  TO  W001-DET3-P-DLAGERD                
359600     MOVE FSUM-KVDISP-AKT(IX5)     TO  W001-DET3-DLAGERE                  
359700     MOVE FSUM-PROC-KVDISP-A(IX5)  TO  W001-DET3-P-DLAGERE                
359800     MOVE FSUM-KVDISP-AKT(IX6)     TO  W001-DET3-DLAGERF                  
359900     MOVE FSUM-PROC-KVDISP-A(IX6)  TO  W001-DET3-P-DLAGERF                
360000     MOVE FSUM-KVDISP-AKT(IX7)     TO  W001-DET3-DLAGERG                  
360100     MOVE FSUM-PROC-KVDISP-A(IX7)  TO  W001-DET3-P-DLAGERG                
360200     MOVE FSUM-KVDISP-AKT(IX8)     TO  W001-DET3-DLAGERH                  
360300     MOVE FSUM-PROC-KVDISP-A(IX8)  TO  W001-DET3-P-DLAGERH                
360400     MOVE TOT-KVDISP-AKT           TO  W001-DET3-TOT                      
360500     MOVE ZERO                     TO  W001-DET3-P-TOT                    
360600     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
360700     MOVE +1 TO W001-SKIP                                                 
360800     PERFORM S21-SKRIV-LISTA                                              
360900                                                                          
361000     MOVE FSUM-KVDISP-PAS(IX1)     TO  W001-DET4-DLAGERA                  
361100     MOVE FSUM-PROC-KVDISP-P(IX1)  TO  W001-DET4-P-DLAGERA                
361200     MOVE FSUM-KVDISP-PAS(IX2)     TO  W001-DET4-DLAGERB                  
361300     MOVE FSUM-PROC-KVDISP-P(IX2)  TO  W001-DET4-P-DLAGERB                
361400     MOVE FSUM-KVDISP-PAS(IX3)     TO  W001-DET4-DLAGERC                  
361500     MOVE FSUM-PROC-KVDISP-P(IX3)  TO  W001-DET4-P-DLAGERC                
361600     MOVE FSUM-KVDISP-PAS(IX4)     TO  W001-DET4-DLAGERD                  
361700     MOVE FSUM-PROC-KVDISP-P(IX4)  TO  W001-DET4-P-DLAGERD                
361800     MOVE FSUM-KVDISP-PAS(IX5)     TO  W001-DET4-DLAGERE                  
361900     MOVE FSUM-PROC-KVDISP-P(IX5)  TO  W001-DET4-P-DLAGERE                
362000     MOVE FSUM-KVDISP-PAS(IX6)     TO  W001-DET4-DLAGERF                  
362100     MOVE FSUM-PROC-KVDISP-P(IX6)  TO  W001-DET4-P-DLAGERF                
362200     MOVE FSUM-KVDISP-PAS(IX7)     TO  W001-DET4-DLAGERG                  
362300     MOVE FSUM-PROC-KVDISP-P(IX7)  TO  W001-DET4-P-DLAGERG                
362400     MOVE FSUM-KVDISP-PAS(IX8)     TO  W001-DET4-DLAGERH                  
362500     MOVE FSUM-PROC-KVDISP-P(IX8)  TO  W001-DET4-P-DLAGERH                
362600     MOVE TOT-KVDISP-PAS           TO  W001-DET4-TOT                      
362700     MOVE ZERO                     TO  W001-DET4-P-TOT                    
362800     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
362900     MOVE +1 TO W001-SKIP                                                 
363000     PERFORM S21-SKRIV-LISTA                                              
363100                                                                          
363200     MOVE FSUM-LS-AKT(IX1)         TO  W001-DET5-LLAGERA                  
363300     MOVE FSUM-PROC-LS-A(IX1)      TO  W001-DET5-P-LLAGERA                
363400     MOVE FSUM-LS-AKT(IX2)         TO  W001-DET5-LLAGERB                  
363500     MOVE FSUM-PROC-LS-A(IX2)      TO  W001-DET5-P-LLAGERB                
363600     MOVE FSUM-LS-AKT(IX3)         TO  W001-DET5-LLAGERC                  
363700     MOVE FSUM-PROC-LS-A(IX3)      TO  W001-DET5-P-LLAGERC                
363800     MOVE FSUM-LS-AKT(IX4)         TO  W001-DET5-LLAGERD                  
363900     MOVE FSUM-PROC-LS-A(IX4)      TO  W001-DET5-P-LLAGERD                
364000     MOVE FSUM-LS-AKT(IX5)         TO  W001-DET5-LLAGERE                  
364100     MOVE FSUM-PROC-LS-A(IX5)      TO  W001-DET5-P-LLAGERE                
364200     MOVE FSUM-LS-AKT(IX6)         TO  W001-DET5-LLAGERF                  
364300     MOVE FSUM-PROC-LS-A(IX6)      TO  W001-DET5-P-LLAGERF                
364400     MOVE FSUM-LS-AKT(IX7)         TO  W001-DET5-LLAGERG                  
364500     MOVE FSUM-PROC-LS-A(IX7)      TO  W001-DET5-P-LLAGERG                
364600     MOVE FSUM-LS-AKT(IX8)         TO  W001-DET5-LLAGERH                  
364700     MOVE FSUM-PROC-LS-A(IX8)      TO  W001-DET5-P-LLAGERH                
364800     MOVE TOT-LS-AKT               TO  W001-DET5-TOT                      
364900     MOVE ZERO                     TO  W001-DET5-P-TOT                    
365000     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
365100     MOVE +1 TO W001-SKIP                                                 
365200     PERFORM S21-SKRIV-LISTA                                              
365300                                                                          
365400     MOVE FSUM-LS-PAS(IX1)         TO  W001-DET6-LLAGERA                  
365500     MOVE FSUM-PROC-LS-P(IX1)      TO  W001-DET6-P-LLAGERA                
365600     MOVE FSUM-LS-PAS(IX2)         TO  W001-DET6-LLAGERB                  
365700     MOVE FSUM-PROC-LS-P(IX2)      TO  W001-DET6-P-LLAGERB                
365800     MOVE FSUM-LS-PAS(IX3)         TO  W001-DET6-LLAGERC                  
365900     MOVE FSUM-PROC-LS-P(IX3)      TO  W001-DET6-P-LLAGERC                
366000     MOVE FSUM-LS-PAS(IX4)         TO  W001-DET6-LLAGERD                  
366100     MOVE FSUM-PROC-LS-P(IX4)      TO  W001-DET6-P-LLAGERD                
366200     MOVE FSUM-LS-PAS(IX5)         TO  W001-DET6-LLAGERE                  
366300     MOVE FSUM-PROC-LS-P(IX5)      TO  W001-DET6-P-LLAGERE                
366400     MOVE FSUM-LS-PAS(IX6)         TO  W001-DET6-LLAGERF                  
366500     MOVE FSUM-PROC-LS-P(IX6)      TO  W001-DET6-P-LLAGERF                
366600     MOVE FSUM-LS-PAS(IX7)         TO  W001-DET6-LLAGERG                  
366700     MOVE FSUM-PROC-LS-P(IX7)      TO  W001-DET6-P-LLAGERG                
366800     MOVE FSUM-LS-PAS(IX8)         TO  W001-DET6-LLAGERH                  
366900     MOVE FSUM-PROC-LS-P(IX8)      TO  W001-DET6-P-LLAGERH                
367000     MOVE TOT-LS-PAS               TO  W001-DET6-TOT                      
367100     MOVE ZERO                     TO  W001-DET6-P-TOT                    
367200     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
367300     MOVE +1 TO W001-SKIP                                                 
367400     PERFORM S21-SKRIV-LISTA                                              
367500                                                                          
367600     MOVE FSUM-AK-AKT(IX1)         TO  W001-DET7-ALAGERA                  
367700     MOVE FSUM-PROC-AK-A(IX1)      TO  W001-DET7-P-ALAGERA                
367800     MOVE FSUM-AK-AKT(IX2)         TO  W001-DET7-ALAGERB                  
367900     MOVE FSUM-PROC-AK-A(IX2)      TO  W001-DET7-P-ALAGERB                
368000     MOVE FSUM-AK-AKT(IX3)         TO  W001-DET7-ALAGERC                  
368100     MOVE FSUM-PROC-AK-A(IX3)      TO  W001-DET7-P-ALAGERC                
368200     MOVE FSUM-AK-AKT(IX4)         TO  W001-DET7-ALAGERD                  
368300     MOVE FSUM-PROC-AK-A(IX4)      TO  W001-DET7-P-ALAGERD                
368400     MOVE FSUM-AK-AKT(IX5)         TO  W001-DET7-ALAGERE                  
368500     MOVE FSUM-PROC-AK-A(IX5)      TO  W001-DET7-P-ALAGERE                
368600     MOVE FSUM-AK-AKT(IX6)         TO  W001-DET7-ALAGERF                  
368700     MOVE FSUM-PROC-AK-A(IX6)      TO  W001-DET7-P-ALAGERF                
368800     MOVE FSUM-AK-AKT(IX7)         TO  W001-DET7-ALAGERG                  
368900     MOVE FSUM-PROC-AK-A(IX7)      TO  W001-DET7-P-ALAGERG                
369000     MOVE FSUM-AK-AKT(IX8)         TO  W001-DET7-ALAGERH                  
369100     MOVE FSUM-PROC-AK-A(IX8)      TO  W001-DET7-P-ALAGERH                
369200     MOVE TOT-AK-AKT               TO  W001-DET7-TOT                      
369300     MOVE ZERO                     TO  W001-DET7-P-TOT                    
369400     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
369500     MOVE +1 TO W001-SKIP                                                 
369600     PERFORM S21-SKRIV-LISTA                                              
369700                                                                          
369800     MOVE FSUM-AK-PAS(IX1)         TO  W001-DET8-ALAGERA                  
369900     MOVE FSUM-PROC-AK-P(IX1)      TO  W001-DET8-P-ALAGERA                
370000     MOVE FSUM-AK-PAS(IX2)         TO  W001-DET8-ALAGERB                  
370100     MOVE FSUM-PROC-AK-P(IX2)      TO  W001-DET8-P-ALAGERB                
370200     MOVE FSUM-AK-PAS(IX3)         TO  W001-DET8-ALAGERC                  
370300     MOVE FSUM-PROC-AK-P(IX3)      TO  W001-DET8-P-ALAGERC                
370400     MOVE FSUM-AK-PAS(IX4)         TO  W001-DET8-ALAGERD                  
370500     MOVE FSUM-PROC-AK-P(IX4)      TO  W001-DET8-P-ALAGERD                
370600     MOVE FSUM-AK-PAS(IX5)         TO  W001-DET8-ALAGERE                  
370700     MOVE FSUM-PROC-AK-P(IX5)      TO  W001-DET8-P-ALAGERE                
370800     MOVE FSUM-AK-PAS(IX6)         TO  W001-DET8-ALAGERF                  
370900     MOVE FSUM-PROC-AK-P(IX6)      TO  W001-DET8-P-ALAGERF                
371000     MOVE FSUM-AK-PAS(IX7)         TO  W001-DET8-ALAGERG                  
371100     MOVE FSUM-PROC-AK-P(IX7)      TO  W001-DET8-P-ALAGERG                
371200     MOVE FSUM-AK-PAS(IX8)         TO  W001-DET8-ALAGERH                  
371300     MOVE FSUM-PROC-AK-P(IX8)      TO  W001-DET8-P-ALAGERH                
371400     MOVE TOT-AK-PAS               TO  W001-DET8-TOT                      
371500     MOVE ZERO                     TO  W001-DET8-P-TOT                    
371600     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
371700     MOVE +1 TO W001-SKIP                                                 
371800     PERFORM S21-SKRIV-LISTA                                              
371900                                                                          
372000     MOVE FSUM-OLAGER(IX1)         TO  W001-DET9-OLAGERA                  
372100     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET9-P-OLAGERA                
372200     MOVE FSUM-OLAGER(IX2)         TO  W001-DET9-OLAGERB                  
372300     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET9-P-OLAGERB                
372400     MOVE FSUM-OLAGER(IX3)         TO  W001-DET9-OLAGERC                  
372500     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET9-P-OLAGERC                
372600     MOVE FSUM-OLAGER(IX4)         TO  W001-DET9-OLAGERD                  
372700     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET9-P-OLAGERD                
372800     MOVE FSUM-OLAGER(IX5)         TO  W001-DET9-OLAGERE                  
372900     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET9-P-OLAGERE                
373000     MOVE FSUM-OLAGER(IX6)         TO  W001-DET9-OLAGERF                  
373100     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET9-P-OLAGERF                
373200     MOVE FSUM-OLAGER(IX7)         TO  W001-DET9-OLAGERG                  
373300     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET9-P-OLAGERG                
373400     MOVE FSUM-OLAGER(IX8)         TO  W001-DET9-OLAGERH                  
373500     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET9-P-OLAGERH                
373600     MOVE TOT-OLAGER               TO  W001-DET9-TOT                      
373700     MOVE TOT-PROC-OLAGER          TO  W001-DET9-P-TOT                    
373800     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
373900     MOVE +1 TO W001-SKIP                                                 
374000     PERFORM S21-SKRIV-LISTA                                              
374100                                                                          
374200     MOVE FSUM-SLAGER(IX1)         TO  W001-DET10-SLAGERA                 
374300     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET10-P-SLAGERA               
374400     MOVE FSUM-SLAGER(IX2)         TO  W001-DET10-SLAGERB                 
374500     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET10-P-SLAGERB               
374600     MOVE FSUM-SLAGER(IX3)         TO  W001-DET10-SLAGERC                 
374700     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET10-P-SLAGERC               
374800     MOVE FSUM-SLAGER(IX4)         TO  W001-DET10-SLAGERD                 
374900     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET10-P-SLAGERD               
375000     MOVE FSUM-SLAGER(IX5)         TO  W001-DET10-SLAGERE                 
375100     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET10-P-SLAGERE               
375200     MOVE FSUM-SLAGER(IX6)         TO  W001-DET10-SLAGERF                 
375300     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET10-P-SLAGERF               
375400     MOVE FSUM-SLAGER(IX7)         TO  W001-DET10-SLAGERG                 
375500     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET10-P-SLAGERG               
375600     MOVE FSUM-SLAGER(IX8)         TO  W001-DET10-SLAGERH                 
375700     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET10-P-SLAGERH               
375800     MOVE TOT-SLAGER               TO  W001-DET10-TOT                     
375900     MOVE TOT-PROC-SLAGER          TO  W001-DET10-P-TOT                   
376000     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
376100     MOVE +1 TO W001-SKIP                                                 
376200     PERFORM S21-SKRIV-LISTA                                              
376300                                                                          
376400     MOVE FSUM-MLAGER(IX1)         TO  W001-DET11-MLAGERA                 
376500     MOVE FSUM-PROC-MLAGER(IX1)    TO  W001-DET11-P-MLAGERA               
376600     MOVE FSUM-MLAGER(IX2)         TO  W001-DET11-MLAGERB                 
376700     MOVE FSUM-PROC-MLAGER(IX2)    TO  W001-DET11-P-MLAGERB               
376800     MOVE FSUM-MLAGER(IX3)         TO  W001-DET11-MLAGERC                 
376900     MOVE FSUM-PROC-MLAGER(IX3)    TO  W001-DET11-P-MLAGERC               
377000     MOVE FSUM-MLAGER(IX4)         TO  W001-DET11-MLAGERD                 
377100     MOVE FSUM-PROC-MLAGER(IX4)    TO  W001-DET11-P-MLAGERD               
377200     MOVE FSUM-MLAGER(IX5)         TO  W001-DET11-MLAGERE                 
377300     MOVE FSUM-PROC-MLAGER(IX5)    TO  W001-DET11-P-MLAGERE               
377400     MOVE FSUM-MLAGER(IX6)         TO  W001-DET11-MLAGERF                 
377500     MOVE FSUM-PROC-MLAGER(IX6)    TO  W001-DET11-P-MLAGERF               
377600     MOVE FSUM-MLAGER(IX7)         TO  W001-DET11-MLAGERG                 
377700     MOVE FSUM-PROC-MLAGER(IX7)    TO  W001-DET11-P-MLAGERG               
377800     MOVE FSUM-MLAGER(IX8)         TO  W001-DET11-MLAGERH                 
377900     MOVE FSUM-PROC-MLAGER(IX8)    TO  W001-DET11-P-MLAGERH               
378000     MOVE TOT-MLAGER               TO  W001-DET11-TOT                     
378100     MOVE TOT-PROC-MLAGER          TO  W001-DET11-P-TOT                   
378200     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
378300     MOVE +1 TO W001-SKIP                                                 
378400     PERFORM S21-SKRIV-LISTA                                              
378500                                                                          
378600     MOVE FSUM-KVOT(IX1)           TO  W001-DET12-KVOTA                   
378700     MOVE FSUM-PROC-KVOT(IX1)      TO  W001-DET12-P-KVOTA                 
378800     MOVE FSUM-KVOT(IX2)           TO  W001-DET12-KVOTB                   
378900     MOVE FSUM-PROC-KVOT(IX2)      TO  W001-DET12-P-KVOTB                 
379000     MOVE FSUM-KVOT(IX3)           TO  W001-DET12-KVOTC                   
379100     MOVE FSUM-PROC-KVOT(IX3)      TO  W001-DET12-P-KVOTC                 
379200     MOVE FSUM-KVOT(IX4)           TO  W001-DET12-KVOTD                   
379300     MOVE FSUM-PROC-KVOT(IX4)      TO  W001-DET12-P-KVOTD                 
379400     MOVE FSUM-KVOT(IX5)           TO  W001-DET12-KVOTE                   
379500     MOVE FSUM-PROC-KVOT(IX5)      TO  W001-DET12-P-KVOTE                 
379600     MOVE FSUM-KVOT(IX6)           TO  W001-DET12-KVOTF                   
379700     MOVE FSUM-PROC-KVOT(IX6)      TO  W001-DET12-P-KVOTF                 
379800     MOVE FSUM-KVOT(IX7)           TO  W001-DET12-KVOTG                   
379900     MOVE FSUM-PROC-KVOT(IX7)      TO  W001-DET12-P-KVOTG                 
380000     MOVE FSUM-KVOT(IX8)           TO  W001-DET12-KVOTH                   
380100     MOVE FSUM-PROC-KVOT(IX8)      TO  W001-DET12-P-KVOTH                 
380200     MOVE TOT-KVOT                 TO  W001-DET12-TOT                     
380300     MOVE TOT-PROC-MLAGER          TO  W001-DET12-P-TOT                   
380400     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
380500     MOVE +1 TO W001-SKIP                                                 
380600     PERFORM S21-SKRIV-LISTA                                              
380700                                                                          
380800     MOVE FSUM-SPLIT(IX1)          TO  W001-DET13-SPLITA                  
380900     MOVE FSUM-SPLIT(IX2)          TO  W001-DET13-SPLITB                  
381000     MOVE FSUM-SPLIT(IX3)          TO  W001-DET13-SPLITC                  
381100     MOVE FSUM-SPLIT(IX4)          TO  W001-DET13-SPLITD                  
381200     MOVE FSUM-SPLIT(IX5)          TO  W001-DET13-SPLITE                  
381300     MOVE FSUM-SPLIT(IX6)          TO  W001-DET13-SPLITF                  
381400     MOVE FSUM-SPLIT(IX7)          TO  W001-DET13-SPLITG                  
381500     MOVE FSUM-SPLIT(IX8)          TO  W001-DET13-SPLITH                  
381600     MOVE TOT-SPLIT                TO  W001-DET13-TOT                     
381700     MOVE ZERO                     TO  W001-DET13-P-TOT                   
381800     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
381900     MOVE +1 TO W001-SKIP                                                 
382000     PERFORM S21-SKRIV-LISTA                                              
382100                                                                          
382200     MOVE FSUM-OMSHAST-DISP(IX1)   TO  W001-DET14-OMSHASTA                
382300     MOVE FSUM-OMSHAST-PROC-D(IX1) TO  W001-DET14-P-OMSHASTA              
382400     MOVE FSUM-OMSHAST-DISP(IX2)   TO  W001-DET14-OMSHASTB                
382500     MOVE FSUM-OMSHAST-PROC-D(IX2) TO  W001-DET14-P-OMSHASTB              
382600     MOVE FSUM-OMSHAST-DISP(IX3)   TO  W001-DET14-OMSHASTC                
382700     MOVE FSUM-OMSHAST-PROC-D(IX3) TO  W001-DET14-P-OMSHASTC              
382800     MOVE FSUM-OMSHAST-DISP(IX4)   TO  W001-DET14-OMSHASTD                
382900     MOVE FSUM-OMSHAST-PROC-D(IX4) TO  W001-DET14-P-OMSHASTD              
383000     MOVE FSUM-OMSHAST-DISP(IX5)   TO  W001-DET14-OMSHASTE                
383100     MOVE FSUM-OMSHAST-PROC-D(IX5) TO  W001-DET14-P-OMSHASTE              
383200     MOVE FSUM-OMSHAST-DISP(IX6)   TO  W001-DET14-OMSHASTF                
383300     MOVE FSUM-OMSHAST-PROC-D(IX6) TO  W001-DET14-P-OMSHASTF              
383400     MOVE FSUM-OMSHAST-DISP(IX7)   TO  W001-DET14-OMSHASTG                
383500     MOVE FSUM-OMSHAST-PROC-D(IX7) TO  W001-DET14-P-OMSHASTG              
383600     MOVE FSUM-OMSHAST-DISP(IX8)   TO  W001-DET14-OMSHASTH                
383700     MOVE FSUM-OMSHAST-PROC-D(IX8) TO  W001-DET14-P-OMSHASTH              
383800     MOVE TOT-OMSHAST-DISP         TO  W001-DET14-TOT                     
383900     MOVE ZERO                     TO  W001-DET14-P-TOT                   
384000     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
384100     MOVE +1 TO W001-SKIP                                                 
384200     PERFORM S21-SKRIV-LISTA                                              
384300                                                                          
384400     MOVE FSUM-OMSHAST-LS(IX1)      TO W001-DET15-OMSHASTA                
384500     MOVE FSUM-OMSHAST-PROC-LS(IX1) TO W001-DET15-P-OMSHASTA              
384600     MOVE FSUM-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB               
384700     MOVE FSUM-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB             
384800     MOVE FSUM-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC               
384900     MOVE FSUM-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC             
385000     MOVE FSUM-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD               
385100     MOVE FSUM-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD             
385200     MOVE FSUM-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE               
385300     MOVE FSUM-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE             
385400     MOVE FSUM-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF               
385500     MOVE FSUM-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF             
385600     MOVE FSUM-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG               
385700     MOVE FSUM-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG             
385800     MOVE FSUM-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH               
385900     MOVE FSUM-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH             
386000     MOVE TOT-OMSHAST-LS            TO  W001-DET15-TOT                    
386100     MOVE ZERO                      TO  W001-DET15-P-TOT                  
386200     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
386300     MOVE +1 TO W001-SKIP                                                 
386400     PERFORM S21-SKRIV-LISTA                                              
386500                                                                          
386600     MOVE FSUM-SERVG-BTO(IX1)     TO  W001-DET16-SERVG-BTOA               
386700     MOVE FSUM-SERVG-BTO(IX2)     TO  W001-DET16-SERVG-BTOB               
386800     MOVE FSUM-SERVG-BTO(IX3)     TO  W001-DET16-SERVG-BTOC               
386900     MOVE FSUM-SERVG-BTO(IX4)     TO  W001-DET16-SERVG-BTOD               
387000     MOVE FSUM-SERVG-BTO(IX5)     TO  W001-DET16-SERVG-BTOE               
387100     MOVE FSUM-SERVG-BTO(IX6)     TO  W001-DET16-SERVG-BTOF               
387200     MOVE FSUM-SERVG-BTO(IX7)     TO  W001-DET16-SERVG-BTOG               
387300     MOVE FSUM-SERVG-BTO(IX8)     TO  W001-DET16-SERVG-BTOH               
387400     MOVE TOT-SERVG-BTO           TO  W001-DET16-TOT                      
387500     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
387600     MOVE +1 TO W001-SKIP                                                 
387700     PERFORM S21-SKRIV-LISTA                                              
387800                                                                          
387900     MOVE FSUM-SERVG-NTO(IX1)     TO  W001-DET17-SERVG-NTOA               
388000     MOVE FSUM-SERVG-NTO(IX2)     TO  W001-DET17-SERVG-NTOB               
388100     MOVE FSUM-SERVG-NTO(IX3)     TO  W001-DET17-SERVG-NTOC               
388200     MOVE FSUM-SERVG-NTO(IX4)     TO  W001-DET17-SERVG-NTOD               
388300     MOVE FSUM-SERVG-NTO(IX5)     TO  W001-DET17-SERVG-NTOE               
388400     MOVE FSUM-SERVG-NTO(IX6)     TO  W001-DET17-SERVG-NTOF               
388500     MOVE FSUM-SERVG-NTO(IX7)     TO  W001-DET17-SERVG-NTOG               
388600     MOVE FSUM-SERVG-NTO(IX8)     TO  W001-DET17-SERVG-NTOH               
388700     MOVE TOT-SERVG-NTO           TO  W001-DET17-TOT                      
388800     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
388900     MOVE +1 TO W001-SKIP                                                 
389000     PERFORM S21-SKRIV-LISTA                                              
389100     .                                                                    
389200     EJECT                                                                
389300 Z-FINIT SECTION.                                                         
389400                                                                          
389500     CLOSE W23195                                                         
389600           W23199-001                                                     
389700                                                                          
389800     MOVE 'S' TO POSTSUM-OPKOD                                            
389900     CALL POSTSUM USING POSTSUM-PARM                                      
390000     .                                                                    
390100     EJECT                                                                
390200 S01-LAS-W23195      SECTION.                                             
390300                                                                          
390400     READ W23195 INTO IN-AREA                                             
390500     AT END                                                               
390600         SET END-OF-W23195 TO TRUE                                        
390700                                                                          
390800     NOT AT END                                                           
390900        MOVE 'W23199'      TO POSTSUM-FDNAMN                              
391000        MOVE 'W23199D1'    TO POSTSUM-DDNAMN2                             
391100        MOVE SPACE         TO POSTSUM-TRANSTYP                            
391200        CALL POSTSUM USING POSTSUM-PARM                                   
391300     .                                                                    
391400     EJECT                                                                
391500 S12-HITTA-KVDLTID SECTION.                                               
391600                                                                          
391700                                                                          
391800     MOVE +1        TO IDDC-IX                                            
391900                       IDLEV-IX                                           
392000     MOVE SPACES    TO W-IDDC-SEND                                        
392100     MOVE IN-IDDC   TO W-IDDC-REC                                         
392200*                                                                         
392300     PERFORM UNTIL IDLEV-IX    > IDLEV-IX-MAX            OR               
392400                   IN-IDLEVNR  = WS-IDLEVNR-R (IDLEV-IX)                  
392500                                                                          
392600        ADD +1 TO IDLEV-IX                                                
392700     END-PERFORM                                                          
392800     IF IDLEV-IX < IDLEV-IX-MAX                                           
392900        MOVE WS-IDDC-R (IDLEV-IX)  TO W-IDDC-SEND                         
393000     ELSE                                                                 
393100        MOVE WS-IDDC-CDC           TO W-IDDC-SEND                         
393200     END-IF                                                               
393300*                                                                         
393400     PERFORM UNTIL IDDC-IX     > IDDC-IX-MAX             OR               
393500                  (W-IDDC-REC  = WS-IDDC-B601 (IDDC-IX)  AND              
393600                   W-IDDC-SEND = WS-IDDC-B616 (IDDC-IX))                  
393700                                                                          
393800        ADD +1 TO IDDC-IX                                                 
393900     END-PERFORM                                                          
394000*                                                                         
394100     IF IDDC-IX     > IDDC-IX-MAX                                         
394200        DISPLAY 'FOR WORKING TABLE WITH WDB6 DATA'                        
394300        DISPLAY 'INDEX OUT OF BOUNDS'                                     
394400        DISPLAY 'DC SEND   :' W-IDDC-SEND                                 
394500        DISPLAY 'DC REC    :' W-IDDC-REC                                  
394600        DISPLAY 'SUPPLIER  :' IN-IDLEVNR                                  
394700        DISPLAY 'PART      :' IN-IDARTNR                                  
394800     END-IF                                                               
394900     .                                                                    
395000     EJECT                                                                
395100 S21-SKRIV-LISTA SECTION.                                                 
395200                                                                          
395300     WRITE W23199-001-RAD FROM W001-RAD AFTER W001-SKIP                   
395400                                                                          
395500     MOVE SPACE TO W001-RAD                                               
395600     ADD  +1 TO W001-ANTAL-RADER                                          
395700     .                                                                    
395800     EJECT                                                                
395900 S21A-SKRIV-RUBRIKER SECTION.                                             
396000                                                                          
396100     ADD +1 TO W001-SIDRAKNARE                                            
396200     MOVE W001-SIDRAKNARE TO W001-SID                                     
396300     WRITE W23199-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
396400     WRITE W23199-001-RAD FROM W001-RUBRIK3 AFTER 2                       
396500     MOVE +2 TO W001-ANTAL-RADER                                          
396600     MOVE +2 TO W001-SKIP                                                 
396700     .                                                                    
396800* --- IMS SEKTIONER ---                                                   
396900                                                                          
397000 IMS-GET-WDB6      SECTION.                                               
397100                                                                          
397200     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B6                        
397300     MOVE '  GAGKGB'          TO GODK-STATUSKODER                         
397400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
397500     PERFORM IMS-STATUSKONTROLL                                           
397600     .                                                                    
397700     EJECT                                                                
397800 IMS-STATUSKONTROLL SECTION.                                              
397900                                                                          
398000     SET STATUS-IX TO 1                                                   
398100     SEARCH GODK-STATUS                                                   
398200       AT END                                                             
398300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
398400           DELIMITED BY SIZE INTO FELTEXT                                 
398500         DISPLAY FELTEXT                                                  
398600         CALL FELLOG                                                      
398700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
398800         CONTINUE                                                         
398900     END-SEARCH                                                           
399000     .                                                                    
