000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2319800.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   97/05/21.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      - LÄSER FIL W23195                                                 
001100*      - SKAPAR LISTA REFILL UPPFÖLJNING NDC                              
001200*    PROGRAMMET ÄR EN KOPIA AV W2319600                                   
001300*                                                                         
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800     EJECT                                                                
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- GRUNDFIL ANALYS-LISTOR                                     
002200     SELECT W23195                     ASSIGN TO W23198D1.                
002300*          --- AKTUELLT NDC-LAGER                                         
002400*    D2-FIL TAS BORT OCH ERSÄTTS MED SYMBOLISK PARAMETER                  
002500*    SELECT W271NDC                    ASSIGN TO W23198D2.                
002600*          --- LISTA                                                      
002700     SELECT W23198-001                 ASSIGN TO W23198D3.                
002800*          --- URVAL                                                      
002900     SELECT W231PP                     ASSIGN TO W23198D4.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W23195                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800*01  -COPY W23195        -L.                                              
003900     SKIP3                                                                
004000 FD  W23198-001                                                           
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300 01  W23198-001-RAD              PIC X(165).                              
004400     EJECT                                                                
004500 FD  W231PP                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800 01  PARM                        PIC X(80).                               
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W2319800'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
005700 77  SW-KVOI-TRAFF               PIC X       VALUE 'N'.                   
005800 77  SW-ARTIKEL-SAKNAS-WDK7      PIC X       VALUE 'N'.                   
005900 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
006000 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
006200 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
006300 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
006500 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
006700 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
006800 77  KVOI-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006900 77  ART-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
007000 77  ART-IX-MAX                  PIC S9(3)   VALUE +72  COMP-3.           
007100 77  PSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007200 77  PSUM-IX-MAX                 PIC S9(3)   VALUE +9   COMP-3.           
007300 77  FSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007400 77  FSUM-IX-MAX                 PIC S9(3)   VALUE +8   COMP-3.           
007500 77  WS-NDC-NUM                  PIC 9(2)    VALUE ZERO.                  
007600 77  WS-KVAKS                    PIC S9(11)  VALUE ZERO COMP-3.           
007700 77  WS-KVLS-TOT                 PIC S9(11)  VALUE ZERO COMP-3.           
007800 77  WS-KVOKS-TOT                PIC S9(11)  VALUE ZERO COMP-3.           
007900 77  WS-KVOI                     PIC S9(11)V9(2)                          
008000                                   VALUE ZERO COMP-3.                     
008100 77  WS-KVOI-TOT-AAR             PIC S9(11)     VALUE ZERO COMP-3.        
008200 77  WS-KVOI-TOT-VECKA           PIC S9(11)     VALUE ZERO COMP-3.        
008300 77  WS-KVOI-TOT-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
008400 77  WS-KVOI-TEO-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
008500 77  WS-KVOT-SAKNAS-WDK7         PIC 9(9)       VALUE ZERO.               
008600 77  WS-KVOT-CDC-SAKNAS-WDK7     PIC 9(9)       VALUE ZERO.               
008700 77  WS-KVDISP                   PIC S9(11)     VALUE ZERO COMP-3.        
008800 77  WS-KVDISP-PR                PIC S9(11)V9(2)                          
008900                                   VALUE ZERO COMP-3.                     
009000 77  WS-SUMMA                    PIC S9(11)V9(2)                          
009100                                   VALUE ZERO COMP-3.                     
009200 77  WS-SUMMA-TOT                PIC S9(11)V9(2)                          
009300                                   VALUE ZERO COMP-3.                     
009400 77  WS-SUMMA-KR                 PIC S9(11)      VALUE ZERO.              
009500 77  WS-SERVG                    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
009600 77  WS-OLAGER                   PIC S9(11)      VALUE ZERO.              
009700 77  WS-MLAGER                   PIC S9(11)      VALUE ZERO.              
009800 77  WS-OMSHAST                  PIC 9(11)V9(1)  VALUE ZERO.              
009900 77  WS-PROC                     PIC S9(3)V9(1)  VALUE ZERO.              
010000 77  WS-KVPB-VECKA-SDC           PIC S9(6)V9(2)  VALUE ZERO.              
010100 77  WS-KVPB-DAG-SDC-NORM        PIC S9(6)V9(2)  VALUE ZERO.              
010200 77  WS-LT-BEHOV-SDC-NORM        PIC S9(7)V9(2)  VALUE ZERO.              
010300 77  WS-FIXAD-SUMMA              PIC S9(16)      VALUE ZERO.              
010400                                                                          
010500 77  FELTEXT                     PIC X(80)       VALUE SPACE.             
010600                                                                          
010700 77  W23195-EOF-SW               PIC X       VALUE 'N'.                   
010800     88  END-OF-W23195                       VALUE 'J'.                   
010900                                                                          
011000 77  W231PP-EOF-SW               PIC X       VALUE 'N'.                   
011100     88  END-OF-W231PP                       VALUE 'J'.                   
011200                                                                          
011300 01  DAGENS-DATUM                PIC 9(6).                                
011400 01  FILLER REDEFINES DAGENS-DATUM.                                       
011500     03  DAGENS-AAR              PIC 9(2).                                
011600     03  DAGENS-MAANAD           PIC 9(2).                                
011700     03  DAGENS-DAG              PIC 9(2).                                
011800                                                                          
011900 01  DAGENS-VECKA                PIC 9(4).                                
012000 01  FILLER REDEFINES DAGENS-VECKA.                                       
012100     03  D-VECKA-AAR             PIC 9(2).                                
012200     03  D-VECKA-VECKA           PIC 9(2).                                
012300                                                                          
012400 01  VECKOR.                                                              
012500     03  AAVVD                   PIC 9(5).                                
012600     03  FILLER REDEFINES AAVVD.                                          
012700         05  AAVV                PIC 9(4).                                
012800         05  D                   PIC 9(1).                                
012900                                                                          
013000     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
013100                                                                          
013200 01  PARAM-TILL-W009VADD.                                                 
013300     03  W009VADD-DATUM-2        PIC S9(5) COMP-3.                        
013400     03  W009VADD-ANTAL-2        PIC S9(3) COMP-3.                        
013500     EJECT                                                                
013600*      --- VALID IDDC CODES                                               
013700*                                                                         
013800*01    -COPY WWDCKONS                                                     
013900                                                                          
014000 01  W-IDDC-SEND                 PIC X(2).                                
014100 01  W-IDDC-REC                  PIC X(2).                                
014200 01  W-IDDC                      PIC X(2).                                
014300 01  W-IDLEVNR-DC                PIC X(5).                                
014400 01  WS-DC-TABELL.                                                        
014500     03 DC-TABELL OCCURS 500.                                             
014600        05  WS-IDDC-B601         PIC X(2) VALUE SPACE.                    
014700        05  WS-IDLEVNR-DC        PIC X(5) VALUE ZERO.                     
014800        05  WS-IDDC-B616         PIC X(2) VALUE SPACE.                    
014900        05  WS-KVDLTID-TOT       PIC 9(3) VALUE ZERO.                     
015000 01  IDDC-IX                     PIC 9(3).                                
015100 01  IDDC-IX-MAX                 PIC 9(3) VALUE 500.                      
015200       EJECT                                                              
015300 01  ART-TABELL.                                                          
015400     03 ART-RAD OCCURS 72.                                                
015500        05  ART-KVANT-AKT        PIC S9(9)      VALUE ZERO COMP-3.        
015600        05  ART-KVANT-PAS        PIC S9(9)      VALUE ZERO COMP-3.        
015700        05  ART-PROC-KVANT-A     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015800        05  ART-PROC-KVANT-P     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015900        05  ART-KVDISP-AKT       PIC S9(11)     VALUE ZERO COMP-3.        
016000        05  ART-PROC-KVDISP-A    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016100        05  ART-KVDISP-PAS       PIC S9(11)     VALUE ZERO COMP-3.        
016200        05  ART-PROC-KVDISP-P    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016300        05  ART-LS-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
016400        05  ART-PROC-LS-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016500        05  ART-LS-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
016600        05  ART-PROC-LS-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016700        05  ART-AK-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
016800        05  ART-PROC-AK-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016900        05  ART-AK-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
017000        05  ART-PROC-AK-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017100        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
017200        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017300        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
017400        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017500        05  ART-MLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
017600        05  ART-PROC-MLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017700        05  ART-KVOT             PIC S9(9)      VALUE ZERO COMP-3.        
017800        05  ART-PROC-KVOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017900        05  ART-SPLIT            PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018000        05  ART-OMSHAST-DISP     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018100        05  ART-OMSHAST-PROC-D   PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018200        05  ART-OMSHAST-LS       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018300        05  ART-OMSHAST-PROC-LS  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018400        05  ART-SERVG-BTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018500        05  ART-SERVG-NTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018600*******  ARBETSFÄLT                                                       
018700        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
018800        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
018900        05  WS-ART-MLAGER        PIC S9(11)V9(2) VALUE ZERO.              
019000        05  WS-ART-LS-AKT        PIC S9(11)      VALUE ZERO.              
019100        05  WS-ART-LS-PAS        PIC S9(11)      VALUE ZERO.              
019200        05  WS-ART-LS-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
019300        05  WS-ART-LS-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
019400        05  WS-ART-KVLS-AKT      PIC S9(11)      VALUE ZERO.              
019500        05  WS-ART-KVLS-PAS      PIC S9(11)      VALUE ZERO.              
019600        05  WS-ART-KVDISP-AKT    PIC S9(11)      VALUE ZERO.              
019700        05  WS-ART-KVDISP-PAS    PIC S9(11)      VALUE ZERO.              
019800        05  WS-ART-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.              
019900        05  WS-ART-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.              
020000        05  WS-ART-KVOKS-AKT     PIC S9(11)      VALUE ZERO.              
020100        05  WS-ART-KVOKS-PAS     PIC S9(11)      VALUE ZERO.              
020200        05  WS-ART-OK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
020300        05  WS-ART-OK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
020400        05  WS-ART-KVAKS-AKT     PIC S9(11)      VALUE ZERO.              
020500        05  WS-ART-KVAKS-PAS     PIC S9(11)      VALUE ZERO.              
020600        05  WS-ART-AK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
020700        05  WS-ART-AK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
020800        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
020900        05  WS-ART-KVOI-AKT      PIC S9(9)      VALUE ZERO COMP-3.        
021000        05  WS-ART-KVOI-PAS      PIC S9(9)      VALUE ZERO COMP-3.        
021100        05  WS-ART-KVOI-TEO      PIC S9(9)      VALUE ZERO COMP-3.        
021200        05  WS-ART-KVOI-SAK      PIC S9(9)      VALUE ZERO COMP-3.        
021300        05  WS-ART-KVOI-CDC-AKT  PIC S9(9)      VALUE ZERO COMP-3.        
021400        05  WS-ART-KVOI-CDC-PAS  PIC S9(9)      VALUE ZERO COMP-3.        
021500        05  WS-ART-KVOI-CDC-TEO  PIC S9(9)      VALUE ZERO COMP-3.        
021600        05  WS-ART-KVOI-CDC-SAK  PIC S9(9)      VALUE ZERO COMP-3.        
021700        05  WS-ART-SUINKORD      PIC S9(16)V9(2)                          
021800                                                VALUE ZERO COMP-3.        
021900        05  WS-ART-SUFYSAVP      PIC S9(16)V9(2)                          
022000                                                VALUE ZERO COMP-3.        
022100        05  WS-ART-SUAVBRP       PIC S9(16)V9(2)                          
022200                                                VALUE ZERO COMP-3.        
022300        05  WS-ART-SULAGERB      PIC S9(16)V9(2)                          
022400                                                VALUE ZERO COMP-3.        
022500        05  WS-ART-SUSORTB       PIC S9(16)V9(2)                          
022600                                                VALUE ZERO COMP-3.        
022700     EJECT                                                                
022800 01  PSUM-TABELL.                                                         
022900     03 PSUM-RAD OCCURS 9.                                                
023000        05  PSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
023100        05  PSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
023200        05  PSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023300        05  PSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023400        05  PSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
023500        05  PSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023600        05  PSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
023700        05  PSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023800        05  PSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
023900        05  PSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024000        05  PSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
024100        05  PSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024200        05  PSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
024300        05  PSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024400        05  PSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
024500        05  PSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024600        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
024700        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024800        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
024900        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
025000        05  PSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
025100        05  PSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
025200        05  PSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
025300        05  PSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
025400        05  PSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
025500        05  PSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
025600        05  PSUM-OMSHAST-PROC-D  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
025700        05  PSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
025800        05  PSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
025900        05  PSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
026000        05  PSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
026100*******  ARBETSFÄLT                                                       
026200        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
026300        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
026400        05  WS-PSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
026500        05  WS-PSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
026600        05  WS-PSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
026700        05  WS-PSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
026800        05  WS-PSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
026900        05  WS-PSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
027000        05  WS-PSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
027100        05  WS-PSUM-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.             
027200        05  WS-PSUM-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.             
027300        05  WS-PSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
027400        05  WS-PSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
027500        05  WS-PSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
027600        05  WS-PSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
027700        05  WS-PSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
027800        05  WS-PSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
027900        05  WS-PSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
028000        05  WS-PSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
028100        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
028200        05  WS-PSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
028300        05  WS-PSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
028400        05  WS-PSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
028500        05  WS-PSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
028600        05  WS-PSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
028700        05  WS-PSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
028800        05  WS-PSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
028900        05  WS-PSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
029000        05  WS-PSUM-SUINKORD     PIC S9(16)V9(2)                          
029100                                                VALUE ZERO COMP-3.        
029200        05  WS-PSUM-SUFYSAVP     PIC S9(16)V9(2)                          
029300                                                VALUE ZERO COMP-3.        
029400        05  WS-PSUM-SUAVBRP      PIC S9(16)V9(2)                          
029500                                                VALUE ZERO COMP-3.        
029600        05  WS-PSUM-SULAGERB     PIC S9(16)V9(2)                          
029700                                                VALUE ZERO COMP-3.        
029800        05  WS-PSUM-SUSORTB      PIC S9(16)V9(2)                          
029900                                                VALUE ZERO COMP-3.        
030000     EJECT                                                                
030100 01  FSUM-TABELL.                                                         
030200     03 FSUM-RAD OCCURS 8.                                                
030300        05  FSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
030400        05  FSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
030500        05  FSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030600        05  FSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030700        05  FSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
030800        05  FSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030900        05  FSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
031000        05  FSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031100        05  FSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
031200        05  FSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031300        05  FSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
031400        05  FSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031500        05  FSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
031600        05  FSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031700        05  FSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
031800        05  FSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031900        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
032000        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032100        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
032200        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032300        05  FSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
032400        05  FSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032500        05  FSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
032600        05  FSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032700        05  FSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032800        05  FSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032900        05  FSUM-OMSHAST-PROC-D PIC S9(7)V9(1) VALUE ZERO COMP-3.         
033000        05  FSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
033100        05  FSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
033200        05  FSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
033300        05  FSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
033400*******  ARBETSFÄLT                                                       
033500        05  WS-FSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
033600        05  WS-FSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
033700        05  WS-FSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
033800        05  WS-FSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
033900        05  WS-FSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
034000        05  WS-FSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
034100        05  WS-FSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
034200        05  WS-FSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
034300        05  WS-FSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
034400        05  WS-FSUM-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.            
034500        05  WS-FSUM-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.            
034600        05  WS-FSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
034700        05  WS-FSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
034800        05  WS-FSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
034900        05  WS-FSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
035000        05  WS-FSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
035100        05  WS-FSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
035200        05  WS-FSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
035300        05  WS-FSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
035400        05  WS-FSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
035500        05  WS-FSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
035600        05  WS-FSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
035700        05  WS-FSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
035800        05  WS-FSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
035900        05  WS-FSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
036000        05  WS-FSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
036100        05  WS-FSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
036200        05  WS-FSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
036300        05  WS-FSUM-SUINKORD     PIC S9(16)V9(2)                          
036400                                                VALUE ZERO COMP-3.        
036500        05  WS-FSUM-SUFYSAVP     PIC S9(16)V9(2)                          
036600                                                VALUE ZERO COMP-3.        
036700        05  WS-FSUM-SUAVBRP      PIC S9(16)V9(2)                          
036800                                                VALUE ZERO COMP-3.        
036900        05  WS-FSUM-SULAGERB     PIC S9(16)V9(2)                          
037000                                                VALUE ZERO COMP-3.        
037100        05  WS-FSUM-SUSORTB      PIC S9(16)V9(2)                          
037200                                                VALUE ZERO COMP-3.        
037300     EJECT                                                                
037400 01  TOTAL-RUTA.                                                          
037500     03  TOT-KVANT-AKT          PIC S9(9)      VALUE ZERO COMP-3.         
037600     03  TOT-KVANT-PAS          PIC S9(9)      VALUE ZERO COMP-3.         
037700     03  TOT-KVDISP-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
037800     03  TOT-KVDISP-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
037900     03  TOT-LS-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
038000     03  TOT-LS-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
038100     03  TOT-AK-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
038200     03  TOT-AK-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
038300     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
038400     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038500     03  TOT-MLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
038600     03  TOT-PROC-MLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038700     03  TOT-KVOT               PIC S9(9)      VALUE ZERO COMP-3.         
038800     03  TOT-PROC-KVOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038900     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
039000     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
039100     03  TOT-SPLIT              PIC S9(7)V9(1) VALUE ZERO COMP-3.         
039200     03  TOT-OMSHAST-DISP       PIC S9(7)V9(1) VALUE ZERO COMP-3.         
039300     03  TOT-OMSHAST-LS         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
039400     03  TOT-SERVG-BTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
039500     03  TOT-SERVG-NTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
039600*******  ARBETSFÄLT                                                       
039700     03  WS-TOT-SLAGER          PIC S9(11)V9(2) VALUE ZERO.               
039800     03  WS-TOT-OLAGER          PIC S9(11)V9(2) VALUE ZERO.               
039900     03  WS-TOT-MLAGER          PIC S9(11)V9(2) VALUE ZERO.               
040000     03  WS-TOT-LS-AKT          PIC S9(11)      VALUE ZERO.               
040100     03  WS-TOT-LS-PAS          PIC S9(11)      VALUE ZERO.               
040200     03  WS-TOT-LS-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
040300     03  WS-TOT-LS-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
040400     03  WS-TOT-KVDISP-AKT      PIC S9(11)      VALUE ZERO.               
040500     03  WS-TOT-KVDISP-PAS      PIC S9(11)      VALUE ZERO.               
040600     03  WS-TOT-KVDISP-PR-AKT   PIC S9(11)V9(2) VALUE ZERO.               
040700     03  WS-TOT-KVDISP-PR-PAS   PIC S9(11)V9(2) VALUE ZERO.               
040800     03  WS-TOT-KVOKS-AKT       PIC S9(11)      VALUE ZERO.               
040900     03  WS-TOT-KVOKS-PAS       PIC S9(11)      VALUE ZERO.               
041000     03  WS-TOT-OK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
041100     03  WS-TOT-OK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
041200     03  WS-TOT-KVAKS-AKT       PIC S9(11)      VALUE ZERO.               
041300     03  WS-TOT-KVAKS-PAS       PIC S9(11)      VALUE ZERO.               
041400     03  WS-TOT-AK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
041500     03  WS-TOT-AK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
041600     03  WS-TOT-KVOI            PIC S9(11)V9(2) VALUE ZERO.               
041700     03  WS-TOT-KVOI-AKT        PIC S9(9)       VALUE ZERO.               
041800     03  WS-TOT-KVOI-PAS        PIC S9(9)       VALUE ZERO.               
041900     03  WS-TOT-KVOI-TEO        PIC S9(9)       VALUE ZERO.               
042000     03  WS-TOT-KVOI-SAK        PIC S9(9)       VALUE ZERO.               
042100     03  WS-TOT-KVOI-CDC-AKT    PIC S9(9)       VALUE ZERO.               
042200     03  WS-TOT-KVOI-CDC-PAS    PIC S9(9)       VALUE ZERO.               
042300     03  WS-TOT-KVOI-CDC-TEO    PIC S9(9)       VALUE ZERO.               
042400     03  WS-TOT-KVOI-CDC-SAK    PIC S9(9)       VALUE ZERO.               
042500     03  WS-TOT-SUINKORD        PIC S9(16)V9(2)                           
042600                                                VALUE ZERO COMP-3.        
042700     03  WS-TOT-SUFYSAVP        PIC S9(16)V9(2)                           
042800                                                VALUE ZERO COMP-3.        
042900     03  WS-TOT-SUAVBRP         PIC S9(16)V9(2)                           
043000                                                VALUE ZERO COMP-3.        
043100     03  WS-TOT-SULAGERB        PIC S9(16)V9(2)                           
043200                                                VALUE ZERO COMP-3.        
043300     03  WS-TOT-SUSORTB         PIC S9(16)V9(2)                           
043400                                                VALUE ZERO COMP-3.        
043500     EJECT                                                                
043600 01  DYNAMISKA-SUBPROGRAM.                                                
043700*                                                                         
043800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
043900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
044000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
044100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
044200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
044300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
044400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
044500     EJECT                                                                
044600 01  PARAM-TILL-DATKORT.                                                  
044700     03  PROG-ID                 PIC X(8)    VALUE 'W2319800'.            
044800     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
044900*03  -COPY WDATKORT                                                       
045000     EJECT                                                                
045100*03  -COPY WDATAREA                                                       
045200     EJECT                                                                
045300*    --- PARAMETRAR TILL POSTSUM                                          
045400*                                                                         
045500*01  -COPY W0005   -PRE  POSTSUM-                                         
045600     EJECT                                                                
045700 01  PARM-AREA-START             PIC X(24)   VALUE                        
045800                                 'PARM-AREA-START  '.                     
045900 01  PARM-AREA                   PIC X(13).                               
046000 01  FILLER REDEFINES PARM-AREA.                                          
046100     03  PARM-IDUSER             PIC X(8).                                
046200     03  PARM-IDREFTAB           PIC X.                                   
046300     03  FILLER                  PIC XX.                                  
046400     03  PARM-IDDC               PIC XX.                                  
046500     EJECT                                                                
046600 01  IN-AREA-START               PIC X(24)   VALUE                        
046700                                 'IN-AREA-START    '.                     
046800*01  AREA  -COPY W23195     -PRE IN-                                      
046900     EJECT                                                                
047000 01  W001-AREA-START             PIC X(24)   VALUE                        
047100                                 'W001-AREA-START  '.                     
047200     SKIP2                                                                
047300 01  W001-HJALPAREOR.                                                     
047400*                                                                         
047500     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
047600     03  W001-ANTAL-RADER                                                 
047700                                 PIC 9(3)    VALUE 999.                   
047800     03  W001-MAX-RADER-PER-SIDA                                          
047900                                 PIC 9(3)    VALUE 63.                    
048000     03  W001-MAX-POSITIONER-PER-RAD                                      
048100                                 PIC 9(3)    VALUE 165.                   
048200     03  W001-LISTNR             PIC X(11)   VALUE SPACE.                 
048300     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
048400     SKIP2                                                                
048500 01  W001-RAD.                                                            
048600     03  FILLER                  PIC X(165)  VALUE SPACE.                 
048700     EJECT                                                                
048800 01  W001-RUBRIK1.                                                        
048900*                                                                         
049000     03  FILLER                  PIC X(3)    VALUE SPACE.                 
049100     03  FILLER                  PIC X(18)                                
049200                              VALUE 'VOLVO CAR PARTS   '.                 
049300     03  W001-LISTID             PIC X(12)                                
049400                                 VALUE SPACE.                             
049500     03  FILLER                  PIC X(20)                                
049600             VALUE 'FOLLOW-UP REFILL    '.                                
049700     03  FILLER                  PIC X(8)    VALUE SPACE.                 
049800     03  FILLER                  PIC X(4)    VALUE 'NDC '.                
049900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050000     03  W001-AKTUELLT-IDDC      PIC X(2)    VALUE SPACE.                 
050100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
050200     03  FILLER                  PIC X(10)   VALUE 'TABLE NO. '.          
050300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050400     03  W001-IDREFTAB           PIC X(1)    VALUE SPACE.                 
050500     03  FILLER                  PIC X(6)    VALUE SPACE.                 
050600     03  FILLER                  PIC X(6)    VALUE 'WEEK  '.              
050700     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
050800     03  FILLER                  PIC X(5)    VALUE SPACE.                 
050900     03  FILLER                  PIC X(7)    VALUE 'IDUSER '.             
051000     03  W001-IDUSER             PIC X(8)    VALUE SPACE.                 
051100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
051200     03  W001-DATUM              PIC XXBXXBXX.                            
051300     03  FILLER                  PIC X(6)    VALUE SPACE.                 
051400     03  FILLER                  PIC X(5)    VALUE 'PAGE '.               
051500     03  W001-SID                PIC Z(4)9.                               
051600     EJECT                                                                
051700 01  W001-RUBRIK3.                                                        
051800*                                                                         
051900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052000     03  FILLER                  PIC X(11)                                
052100                             VALUE 'PRICE CLASS'.                         
052200     03  FILLER                  PIC X(11)   VALUE SPACE.                 
052300     03  FILLER                  PIC X(7)    VALUE 'A      '.             
052400     03  FILLER                  PIC X(7)    VALUE SPACE.                 
052500     03  FILLER                  PIC X(8)    VALUE 'B       '.            
052600     03  FILLER                  PIC X(6)    VALUE SPACE.                 
052700     03  FILLER                  PIC X(8)    VALUE 'C       '.            
052800     03  FILLER                  PIC X(6)    VALUE SPACE.                 
052900     03  FILLER                  PIC X(9)    VALUE 'D        '.           
053000     03  FILLER                  PIC X(5)    VALUE SPACE.                 
053100     03  FILLER                  PIC X(9)    VALUE 'E        '.           
053200     03  FILLER                  PIC X(5)    VALUE SPACE.                 
053300     03  FILLER                  PIC X(10)   VALUE 'F         '.          
053400     03  FILLER                  PIC X(4)    VALUE SPACE.                 
053500     03  FILLER                  PIC X(10)   VALUE 'G         '.          
053600     03  FILLER                  PIC X(4)    VALUE SPACE.                 
053700     03  FILLER                  PIC X(10)   VALUE 'H         '.          
053800     03  FILLER                  PIC X(11)   VALUE SPACE.                 
053900     03  FILLER                  PIC X(6)    VALUE 'TOTAL '.              
054000     EJECT                                                                
054100 01  W001-DETALJRAD-1.                                                    
054200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054300     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
054400     03  FILLER                  PIC X       VALUE SPACE.                 
054500     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  A'.        
054600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054700     03  W001-DET1-KVANTA        PIC Z(7)9.                               
054800     03  FILLER                  PIC X       VALUE SPACE.                 
054900     03  W001-DET1-P-KVANTA      PIC Z9.9.                                
055000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055100     03  W001-DET1-KVANTB        PIC Z(7)9.                               
055200     03  FILLER                  PIC X       VALUE SPACE.                 
055300     03  W001-DET1-P-KVANTB      PIC Z9.9.                                
055400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055500     03  W001-DET1-KVANTC        PIC Z(7)9.                               
055600     03  FILLER                  PIC X       VALUE SPACE.                 
055700     03  W001-DET1-P-KVANTC      PIC Z9.9.                                
055800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055900     03  W001-DET1-KVANTD        PIC Z(7)9.                               
056000     03  FILLER                  PIC X       VALUE SPACE.                 
056100     03  W001-DET1-P-KVANTD      PIC Z9.9.                                
056200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056300     03  W001-DET1-KVANTE        PIC Z(7)9.                               
056400     03  FILLER                  PIC X       VALUE SPACE.                 
056500     03  W001-DET1-P-KVANTE      PIC Z9.9.                                
056600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056700     03  W001-DET1-KVANTF        PIC Z(7)9.                               
056800     03  FILLER                  PIC X       VALUE SPACE.                 
056900     03  W001-DET1-P-KVANTF      PIC Z9.9.                                
057000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057100     03  W001-DET1-KVANTG        PIC Z(7)9.                               
057200     03  FILLER                  PIC X       VALUE SPACE.                 
057300     03  W001-DET1-P-KVANTG      PIC Z9.9.                                
057400     03  FILLER                  PIC X       VALUE SPACE.                 
057500     03  W001-DET1-KVANTH        PIC Z(7)9.                               
057600     03  FILLER                  PIC X       VALUE SPACE.                 
057700     03  W001-DET1-P-KVANTH      PIC Z9.9.                                
057800     03  FILLER                  PIC X       VALUE SPACE.                 
057900     03  W001-DET1-TOT           PIC Z(13)9.                              
058000     03  FILLER                  PIC X       VALUE SPACE.                 
058100     03  W001-DET1-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
058200     EJECT                                                                
058300 01  W001-DETALJRAD-2.                                                    
058400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058500     03  W001-DET2-PRISKLASS     PIC X       VALUE SPACE.                 
058600     03  FILLER                  PIC X       VALUE SPACE.                 
058700     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  P'.        
058800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058900     03  W001-DET2-KVANTA        PIC Z(7)9.                               
059000     03  FILLER                  PIC X       VALUE SPACE.                 
059100     03  W001-DET2-P-KVANTA      PIC Z9.9.                                
059200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059300     03  W001-DET2-KVANTB        PIC Z(7)9.                               
059400     03  FILLER                  PIC X       VALUE SPACE.                 
059500     03  W001-DET2-P-KVANTB      PIC Z9.9.                                
059600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059700     03  W001-DET2-KVANTC        PIC Z(7)9.                               
059800     03  FILLER                  PIC X       VALUE SPACE.                 
059900     03  W001-DET2-P-KVANTC      PIC Z9.9.                                
060000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060100     03  W001-DET2-KVANTD        PIC Z(7)9.                               
060200     03  FILLER                  PIC X       VALUE SPACE.                 
060300     03  W001-DET2-P-KVANTD      PIC Z9.9.                                
060400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060500     03  W001-DET2-KVANTE        PIC Z(7)9.                               
060600     03  FILLER                  PIC X       VALUE SPACE.                 
060700     03  W001-DET2-P-KVANTE      PIC Z9.9.                                
060800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060900     03  W001-DET2-KVANTF        PIC Z(7)9.                               
061000     03  FILLER                  PIC X       VALUE SPACE.                 
061100     03  W001-DET2-P-KVANTF      PIC Z9.9.                                
061200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061300     03  W001-DET2-KVANTG        PIC Z(7)9.                               
061400     03  FILLER                  PIC X       VALUE SPACE.                 
061500     03  W001-DET2-P-KVANTG      PIC Z9.9.                                
061600     03  FILLER                  PIC X       VALUE SPACE.                 
061700     03  W001-DET2-KVANTH        PIC Z(7)9.                               
061800     03  FILLER                  PIC X       VALUE SPACE.                 
061900     03  W001-DET2-P-KVANTH      PIC Z9.9.                                
062000     03  FILLER                  PIC X       VALUE SPACE.                 
062100     03  W001-DET2-TOT           PIC Z(13)9.                              
062200     03  FILLER                  PIC X       VALUE SPACE.                 
062300     03  W001-DET2-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
062400     EJECT                                                                
062500 01  W001-DETALJRAD-3.                                                    
062600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
062700     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND A'.        
062800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062900     03  W001-DET3-DLAGERA       PIC Z(7)9.                               
063000     03  FILLER                  PIC X       VALUE SPACE.                 
063100     03  W001-DET3-P-DLAGERA     PIC Z9.9.                                
063200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063300     03  W001-DET3-DLAGERB       PIC Z(7)9.                               
063400     03  FILLER                  PIC X       VALUE SPACE.                 
063500     03  W001-DET3-P-DLAGERB     PIC Z9.9.                                
063600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063700     03  W001-DET3-DLAGERC       PIC Z(7)9.                               
063800     03  FILLER                  PIC X       VALUE SPACE.                 
063900     03  W001-DET3-P-DLAGERC     PIC Z9.9.                                
064000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064100     03  W001-DET3-DLAGERD       PIC Z(7)9.                               
064200     03  FILLER                  PIC X       VALUE SPACE.                 
064300     03  W001-DET3-P-DLAGERD     PIC Z9.9.                                
064400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064500     03  W001-DET3-DLAGERE       PIC Z(7)9.                               
064600     03  FILLER                  PIC X       VALUE SPACE.                 
064700     03  W001-DET3-P-DLAGERE     PIC Z9.9.                                
064800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064900     03  W001-DET3-DLAGERF       PIC Z(7)9.                               
065000     03  FILLER                  PIC X       VALUE SPACE.                 
065100     03  W001-DET3-P-DLAGERF     PIC Z9.9.                                
065200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065300     03  W001-DET3-DLAGERG       PIC Z(7)9.                               
065400     03  FILLER                  PIC X       VALUE SPACE.                 
065500     03  W001-DET3-P-DLAGERG     PIC Z9.9.                                
065600     03  FILLER                  PIC X       VALUE SPACE.                 
065700     03  W001-DET3-DLAGERH       PIC Z(7)9.                               
065800     03  FILLER                  PIC X       VALUE SPACE.                 
065900     03  W001-DET3-P-DLAGERH     PIC Z9.9.                                
066000     03  FILLER                  PIC X       VALUE SPACE.                 
066100     03  W001-DET3-TOT           PIC Z(13)9.                              
066200     03  FILLER                  PIC X       VALUE SPACE.                 
066300     03  W001-DET3-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
066400 01  W001-DETALJRAD-4.                                                    
066500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
066600     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND P'.        
066700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066800     03  W001-DET4-DLAGERA       PIC Z(7)9.                               
066900     03  FILLER                  PIC X       VALUE SPACE.                 
067000     03  W001-DET4-P-DLAGERA     PIC Z9.9.                                
067100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067200     03  W001-DET4-DLAGERB       PIC Z(7)9.                               
067300     03  FILLER                  PIC X       VALUE SPACE.                 
067400     03  W001-DET4-P-DLAGERB     PIC Z9.9.                                
067500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067600     03  W001-DET4-DLAGERC       PIC Z(7)9.                               
067700     03  FILLER                  PIC X       VALUE SPACE.                 
067800     03  W001-DET4-P-DLAGERC     PIC Z9.9.                                
067900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068000     03  W001-DET4-DLAGERD       PIC Z(7)9.                               
068100     03  FILLER                  PIC X       VALUE SPACE.                 
068200     03  W001-DET4-P-DLAGERD     PIC Z9.9.                                
068300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068400     03  W001-DET4-DLAGERE       PIC Z(7)9.                               
068500     03  FILLER                  PIC X       VALUE SPACE.                 
068600     03  W001-DET4-P-DLAGERE     PIC Z9.9.                                
068700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068800     03  W001-DET4-DLAGERF       PIC Z(7)9.                               
068900     03  FILLER                  PIC X       VALUE SPACE.                 
069000     03  W001-DET4-P-DLAGERF     PIC Z9.9.                                
069100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069200     03  W001-DET4-DLAGERG       PIC Z(7)9.                               
069300     03  FILLER                  PIC X       VALUE SPACE.                 
069400     03  W001-DET4-P-DLAGERG     PIC Z9.9.                                
069500     03  FILLER                  PIC X       VALUE SPACE.                 
069600     03  W001-DET4-DLAGERH       PIC Z(7)9.                               
069700     03  FILLER                  PIC X       VALUE SPACE.                 
069800     03  W001-DET4-P-DLAGERH     PIC Z9.9.                                
069900     03  FILLER                  PIC X       VALUE SPACE.                 
070000     03  W001-DET4-TOT           PIC Z(13)9.                              
070100     03  FILLER                  PIC X       VALUE SPACE.                 
070200     03  W001-DET4-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
070300     EJECT                                                                
070400 01  W001-DETALJRAD-5.                                                    
070500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
070600     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. A'.        
070700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070800     03  W001-DET5-LLAGERA       PIC Z(7)9.                               
070900     03  FILLER                  PIC X       VALUE SPACE.                 
071000     03  W001-DET5-P-LLAGERA     PIC Z9.9.                                
071100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071200     03  W001-DET5-LLAGERB       PIC Z(7)9.                               
071300     03  FILLER                  PIC X       VALUE SPACE.                 
071400     03  W001-DET5-P-LLAGERB     PIC Z9.9.                                
071500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071600     03  W001-DET5-LLAGERC       PIC Z(7)9.                               
071700     03  FILLER                  PIC X       VALUE SPACE.                 
071800     03  W001-DET5-P-LLAGERC     PIC Z9.9.                                
071900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072000     03  W001-DET5-LLAGERD       PIC Z(7)9.                               
072100     03  FILLER                  PIC X       VALUE SPACE.                 
072200     03  W001-DET5-P-LLAGERD     PIC Z9.9.                                
072300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072400     03  W001-DET5-LLAGERE       PIC Z(7)9.                               
072500     03  FILLER                  PIC X       VALUE SPACE.                 
072600     03  W001-DET5-P-LLAGERE     PIC Z9.9.                                
072700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072800     03  W001-DET5-LLAGERF       PIC Z(7)9.                               
072900     03  FILLER                  PIC X       VALUE SPACE.                 
073000     03  W001-DET5-P-LLAGERF     PIC Z9.9.                                
073100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073200     03  W001-DET5-LLAGERG       PIC Z(7)9.                               
073300     03  FILLER                  PIC X       VALUE SPACE.                 
073400     03  W001-DET5-P-LLAGERG     PIC Z9.9.                                
073500     03  FILLER                  PIC X       VALUE SPACE.                 
073600     03  W001-DET5-LLAGERH       PIC Z(7)9.                               
073700     03  FILLER                  PIC X       VALUE SPACE.                 
073800     03  W001-DET5-P-LLAGERH     PIC Z9.9.                                
073900     03  FILLER                  PIC X       VALUE SPACE.                 
074000     03  W001-DET5-TOT           PIC Z(13)9.                              
074100     03  FILLER                  PIC X       VALUE SPACE.                 
074200     03  W001-DET5-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
074300     EJECT                                                                
074400 01  W001-DETALJRAD-6.                                                    
074500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
074600     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. P'.        
074700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074800     03  W001-DET6-LLAGERA       PIC Z(7)9.                               
074900     03  FILLER                  PIC X       VALUE SPACE.                 
075000     03  W001-DET6-P-LLAGERA     PIC Z9.9.                                
075100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075200     03  W001-DET6-LLAGERB       PIC Z(7)9.                               
075300     03  FILLER                  PIC X       VALUE SPACE.                 
075400     03  W001-DET6-P-LLAGERB     PIC Z9.9.                                
075500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075600     03  W001-DET6-LLAGERC       PIC Z(7)9.                               
075700     03  FILLER                  PIC X       VALUE SPACE.                 
075800     03  W001-DET6-P-LLAGERC     PIC Z9.9.                                
075900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076000     03  W001-DET6-LLAGERD       PIC Z(7)9.                               
076100     03  FILLER                  PIC X       VALUE SPACE.                 
076200     03  W001-DET6-P-LLAGERD     PIC Z9.9.                                
076300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076400     03  W001-DET6-LLAGERE       PIC Z(7)9.                               
076500     03  FILLER                  PIC X       VALUE SPACE.                 
076600     03  W001-DET6-P-LLAGERE     PIC Z9.9.                                
076700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076800     03  W001-DET6-LLAGERF       PIC Z(7)9.                               
076900     03  FILLER                  PIC X       VALUE SPACE.                 
077000     03  W001-DET6-P-LLAGERF     PIC Z9.9.                                
077100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077200     03  W001-DET6-LLAGERG       PIC Z(7)9.                               
077300     03  FILLER                  PIC X       VALUE SPACE.                 
077400     03  W001-DET6-P-LLAGERG     PIC Z9.9.                                
077500     03  FILLER                  PIC X       VALUE SPACE.                 
077600     03  W001-DET6-LLAGERH       PIC Z(7)9.                               
077700     03  FILLER                  PIC X       VALUE SPACE.                 
077800     03  W001-DET6-P-LLAGERH     PIC Z9.9.                                
077900     03  FILLER                  PIC X       VALUE SPACE.                 
078000     03  W001-DET6-TOT           PIC Z(13)9.                              
078100     03  FILLER                  PIC X       VALUE SPACE.                 
078200     03  W001-DET6-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
078300     EJECT                                                                
078400 01  W001-DETALJRAD-7.                                                    
078500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
078600     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   A'.        
078700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078800     03  W001-DET7-ALAGERA       PIC Z(7)9.                               
078900     03  FILLER                  PIC X       VALUE SPACE.                 
079000     03  W001-DET7-P-ALAGERA     PIC Z9.9.                                
079100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079200     03  W001-DET7-ALAGERB       PIC Z(7)9.                               
079300     03  FILLER                  PIC X       VALUE SPACE.                 
079400     03  W001-DET7-P-ALAGERB     PIC Z9.9.                                
079500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079600     03  W001-DET7-ALAGERC       PIC Z(7)9.                               
079700     03  FILLER                  PIC X       VALUE SPACE.                 
079800     03  W001-DET7-P-ALAGERC     PIC Z9.9.                                
079900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080000     03  W001-DET7-ALAGERD       PIC Z(7)9.                               
080100     03  FILLER                  PIC X       VALUE SPACE.                 
080200     03  W001-DET7-P-ALAGERD     PIC Z9.9.                                
080300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080400     03  W001-DET7-ALAGERE       PIC Z(7)9.                               
080500     03  FILLER                  PIC X       VALUE SPACE.                 
080600     03  W001-DET7-P-ALAGERE     PIC Z9.9.                                
080700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080800     03  W001-DET7-ALAGERF       PIC Z(7)9.                               
080900     03  FILLER                  PIC X       VALUE SPACE.                 
081000     03  W001-DET7-P-ALAGERF     PIC Z9.9.                                
081100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081200     03  W001-DET7-ALAGERG       PIC Z(7)9.                               
081300     03  FILLER                  PIC X       VALUE SPACE.                 
081400     03  W001-DET7-P-ALAGERG     PIC Z9.9.                                
081500     03  FILLER                  PIC X       VALUE SPACE.                 
081600     03  W001-DET7-ALAGERH       PIC Z(7)9.                               
081700     03  FILLER                  PIC X       VALUE SPACE.                 
081800     03  W001-DET7-P-ALAGERH     PIC Z9.9.                                
081900     03  FILLER                  PIC X       VALUE SPACE.                 
082000     03  W001-DET7-TOT           PIC Z(13)9.                              
082100     03  FILLER                  PIC X       VALUE SPACE.                 
082200     03  W001-DET7-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
082300     EJECT                                                                
082400 01  W001-DETALJRAD-8.                                                    
082500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
082600     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   P'.        
082700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082800     03  W001-DET8-ALAGERA       PIC Z(7)9.                               
082900     03  FILLER                  PIC X       VALUE SPACE.                 
083000     03  W001-DET8-P-ALAGERA     PIC Z9.9.                                
083100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083200     03  W001-DET8-ALAGERB       PIC Z(7)9.                               
083300     03  FILLER                  PIC X       VALUE SPACE.                 
083400     03  W001-DET8-P-ALAGERB     PIC Z9.9.                                
083500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083600     03  W001-DET8-ALAGERC       PIC Z(7)9.                               
083700     03  FILLER                  PIC X       VALUE SPACE.                 
083800     03  W001-DET8-P-ALAGERC     PIC Z9.9.                                
083900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084000     03  W001-DET8-ALAGERD       PIC Z(7)9.                               
084100     03  FILLER                  PIC X       VALUE SPACE.                 
084200     03  W001-DET8-P-ALAGERD     PIC Z9.9.                                
084300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084400     03  W001-DET8-ALAGERE       PIC Z(7)9.                               
084500     03  FILLER                  PIC X       VALUE SPACE.                 
084600     03  W001-DET8-P-ALAGERE     PIC Z9.9.                                
084700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084800     03  W001-DET8-ALAGERF       PIC Z(7)9.                               
084900     03  FILLER                  PIC X       VALUE SPACE.                 
085000     03  W001-DET8-P-ALAGERF     PIC Z9.9.                                
085100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085200     03  W001-DET8-ALAGERG       PIC Z(7)9.                               
085300     03  FILLER                  PIC X       VALUE SPACE.                 
085400     03  W001-DET8-P-ALAGERG     PIC Z9.9.                                
085500     03  FILLER                  PIC X       VALUE SPACE.                 
085600     03  W001-DET8-ALAGERH       PIC Z(7)9.                               
085700     03  FILLER                  PIC X       VALUE SPACE.                 
085800     03  W001-DET8-P-ALAGERH     PIC Z9.9.                                
085900     03  FILLER                  PIC X       VALUE SPACE.                 
086000     03  W001-DET8-TOT           PIC Z(13)9.                              
086100     03  FILLER                  PIC X       VALUE SPACE.                 
086200     03  W001-DET8-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
086300     EJECT                                                                
086400 01  W001-DETALJRAD-9.                                                    
086500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
086600     03  FILLER                  PIC X(12)   VALUE 'OVERSTOCK   '.        
086700     03  FILLER                  PIC X       VALUE SPACE.                 
086800     03  W001-DET9-OLAGERA       PIC Z(7)9.                               
086900     03  FILLER                  PIC X       VALUE SPACE.                 
087000     03  W001-DET9-P-OLAGERA     PIC Z9.9    BLANK WHEN ZERO.             
087100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087200     03  W001-DET9-OLAGERB       PIC Z(7)9.                               
087300     03  FILLER                  PIC X       VALUE SPACE.                 
087400     03  W001-DET9-P-OLAGERB     PIC Z9.9    BLANK WHEN ZERO.             
087500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087600     03  W001-DET9-OLAGERC       PIC Z(7)9.                               
087700     03  FILLER                  PIC X       VALUE SPACE.                 
087800     03  W001-DET9-P-OLAGERC     PIC Z9.9    BLANK WHEN ZERO.             
087900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088000     03  W001-DET9-OLAGERD       PIC Z(7)9.                               
088100     03  FILLER                  PIC X       VALUE SPACE.                 
088200     03  W001-DET9-P-OLAGERD     PIC Z9.9    BLANK WHEN ZERO.             
088300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088400     03  W001-DET9-OLAGERE       PIC Z(7)9.                               
088500     03  FILLER                  PIC X       VALUE SPACE.                 
088600     03  W001-DET9-P-OLAGERE     PIC Z9.9    BLANK WHEN ZERO.             
088700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088800     03  W001-DET9-OLAGERF       PIC Z(7)9.                               
088900     03  FILLER                  PIC X       VALUE SPACE.                 
089000     03  W001-DET9-P-OLAGERF     PIC Z9.9    BLANK WHEN ZERO.             
089100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089200     03  W001-DET9-OLAGERG       PIC Z(7)9.                               
089300     03  FILLER                  PIC X       VALUE SPACE.                 
089400     03  W001-DET9-P-OLAGERG     PIC Z9.9    BLANK WHEN ZERO.             
089500     03  FILLER                  PIC X       VALUE SPACE.                 
089600     03  W001-DET9-OLAGERH       PIC Z(7)9.                               
089700     03  FILLER                  PIC X       VALUE SPACE.                 
089800     03  W001-DET9-P-OLAGERH     PIC Z9.9    BLANK WHEN ZERO.             
089900     03  FILLER                  PIC X       VALUE SPACE.                 
090000     03  W001-DET9-TOT           PIC Z(13)9.                              
090100     03  FILLER                  PIC X       VALUE SPACE.                 
090200     03  W001-DET9-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
090300     EJECT                                                                
090400 01  W001-DETALJRAD-10.                                                   
090500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
090600     03  FILLER                  PIC X(12)   VALUE 'SAF. STOCK  '.        
090700     03  FILLER                  PIC X       VALUE SPACE.                 
090800     03  W001-DET10-SLAGERA      PIC Z(7)9.                               
090900     03  FILLER                  PIC X       VALUE SPACE.                 
091000     03  W001-DET10-P-SLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
091100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091200     03  W001-DET10-SLAGERB      PIC Z(7)9.                               
091300     03  FILLER                  PIC X       VALUE SPACE.                 
091400     03  W001-DET10-P-SLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
091500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091600     03  W001-DET10-SLAGERC      PIC Z(7)9.                               
091700     03  FILLER                  PIC X       VALUE SPACE.                 
091800     03  W001-DET10-P-SLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
091900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092000     03  W001-DET10-SLAGERD      PIC Z(7)9.                               
092100     03  FILLER                  PIC X       VALUE SPACE.                 
092200     03  W001-DET10-P-SLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
092300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092400     03  W001-DET10-SLAGERE      PIC Z(7)9.                               
092500     03  FILLER                  PIC X       VALUE SPACE.                 
092600     03  W001-DET10-P-SLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
092700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092800     03  W001-DET10-SLAGERF      PIC Z(7)9.                               
092900     03  FILLER                  PIC X       VALUE SPACE.                 
093000     03  W001-DET10-P-SLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
093100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093200     03  W001-DET10-SLAGERG      PIC Z(7)9.                               
093300     03  FILLER                  PIC X       VALUE SPACE.                 
093400     03  W001-DET10-P-SLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
093500     03  FILLER                  PIC X       VALUE SPACE.                 
093600     03  W001-DET10-SLAGERH      PIC Z(7)9.                               
093700     03  FILLER                  PIC X       VALUE SPACE.                 
093800     03  W001-DET10-P-SLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
093900     03  FILLER                  PIC X       VALUE SPACE.                 
094000     03  W001-DET10-TOT          PIC Z(13)9.                              
094100     03  FILLER                  PIC X       VALUE SPACE.                 
094200     03  W001-DET10-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
094300     EJECT                                                                
094400 01  W001-DETALJRAD-11.                                                   
094500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
094600     03  FILLER                  PIC X(12)   VALUE 'AVERAGE ST. '.        
094700     03  FILLER                  PIC X       VALUE SPACE.                 
094800     03  W001-DET11-MLAGERA      PIC Z(7)9.                               
094900     03  FILLER                  PIC X       VALUE SPACE.                 
095000     03  W001-DET11-P-MLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
095100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095200     03  W001-DET11-MLAGERB      PIC Z(7)9.                               
095300     03  FILLER                  PIC X       VALUE SPACE.                 
095400     03  W001-DET11-P-MLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
095500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095600     03  W001-DET11-MLAGERC      PIC Z(7)9.                               
095700     03  FILLER                  PIC X       VALUE SPACE.                 
095800     03  W001-DET11-P-MLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
095900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096000     03  W001-DET11-MLAGERD      PIC Z(7)9.                               
096100     03  FILLER                  PIC X       VALUE SPACE.                 
096200     03  W001-DET11-P-MLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
096300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096400     03  W001-DET11-MLAGERE      PIC Z(7)9.                               
096500     03  FILLER                  PIC X       VALUE SPACE.                 
096600     03  W001-DET11-P-MLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
096700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096800     03  W001-DET11-MLAGERF      PIC Z(7)9.                               
096900     03  FILLER                  PIC X       VALUE SPACE.                 
097000     03  W001-DET11-P-MLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
097100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097200     03  W001-DET11-MLAGERG      PIC Z(7)9.                               
097300     03  FILLER                  PIC X       VALUE SPACE.                 
097400     03  W001-DET11-P-MLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
097500     03  FILLER                  PIC X       VALUE SPACE.                 
097600     03  W001-DET11-MLAGERH      PIC Z(7)9.                               
097700     03  FILLER                  PIC X       VALUE SPACE.                 
097800     03  W001-DET11-P-MLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
097900     03  FILLER                  PIC X       VALUE SPACE.                 
098000     03  W001-DET11-TOT          PIC Z(13)9.                              
098100     03  FILLER                  PIC X       VALUE SPACE.                 
098200     03  W001-DET11-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
098300     EJECT                                                                
098400 01  W001-DETALJRAD-12.                                                   
098500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098600     03  W001-DET12-PRISKLASS    PIC X       VALUE SPACE.                 
098700     03  FILLER                  PIC X       VALUE SPACE.                 
098800     03  FILLER                  PIC X(12)   VALUE 'NO INCOM ORD'.        
098900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
099000     03  W001-DET12-KVOTA        PIC Z(7)9.                               
099100     03  FILLER                  PIC X       VALUE SPACE.                 
099200     03  W001-DET12-P-KVOTA      PIC Z9.9.                                
099300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
099400     03  W001-DET12-KVOTB        PIC Z(7)9.                               
099500     03  FILLER                  PIC X       VALUE SPACE.                 
099600     03  W001-DET12-P-KVOTB      PIC Z9.9.                                
099700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
099800     03  W001-DET12-KVOTC        PIC Z(7)9.                               
099900     03  FILLER                  PIC X       VALUE SPACE.                 
100000     03  W001-DET12-P-KVOTC      PIC Z9.9.                                
100100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
100200     03  W001-DET12-KVOTD        PIC Z(7)9.                               
100300     03  FILLER                  PIC X       VALUE SPACE.                 
100400     03  W001-DET12-P-KVOTD      PIC Z9.9.                                
100500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
100600     03  W001-DET12-KVOTE        PIC Z(7)9.                               
100700     03  FILLER                  PIC X       VALUE SPACE.                 
100800     03  W001-DET12-P-KVOTE      PIC Z9.9.                                
100900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
101000     03  W001-DET12-KVOTF        PIC Z(7)9.                               
101100     03  FILLER                  PIC X       VALUE SPACE.                 
101200     03  W001-DET12-P-KVOTF      PIC Z9.9.                                
101300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
101400     03  W001-DET12-KVOTG        PIC Z(7)9.                               
101500     03  FILLER                  PIC X       VALUE SPACE.                 
101600     03  W001-DET12-P-KVOTG      PIC Z9.9.                                
101700     03  FILLER                  PIC X       VALUE SPACE.                 
101800     03  W001-DET12-KVOTH        PIC Z(7)9.                               
101900     03  FILLER                  PIC X       VALUE SPACE.                 
102000     03  W001-DET12-P-KVOTH      PIC Z9.9.                                
102100     03  FILLER                  PIC X       VALUE SPACE.                 
102200     03  W001-DET12-TOT          PIC Z(13)9.                              
102300     03  FILLER                  PIC X       VALUE SPACE.                 
102400     03  W001-DET12-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
102500     EJECT                                                                
102600 01  W001-DETALJRAD-13.                                                   
102700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
102800     03  FILLER                  PIC X(13) VALUE 'SPLIT FACTOR '.         
102900     03  FILLER                  PIC X(4)    VALUE SPACE.                 
103000     03  W001-DET13-SPLITA       PIC Z9.9.                                
103100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
103200     03  W001-DET13-SPLITB       PIC Z9.9.                                
103300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
103400     03  W001-DET13-SPLITC       PIC Z9.9.                                
103500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
103600     03  W001-DET13-SPLITD       PIC Z9.9.                                
103700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
103800     03  W001-DET13-SPLITE       PIC Z9.9.                                
103900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
104000     03  W001-DET13-SPLITF       PIC Z9.9.                                
104100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
104200     03  W001-DET13-SPLITG       PIC Z9.9.                                
104300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
104400     03  W001-DET13-SPLITH       PIC Z9.9.                                
104500     03  FILLER                  PIC X(16)   VALUE SPACE.                 
104600     03  W001-DET13-TOT          PIC Z9.9.                                
104700     03  FILLER                  PIC X       VALUE SPACE.                 
104800     03  W001-DET13-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
104900     EJECT                                                                
105000 01  W001-DETALJRAD-14.                                                   
105100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
105200     03  FILLER                  PIC X(14) VALUE 'TOR        SOH'.        
105300     03  W001-DET14-OMSHASTA     PIC Z(4)9.9.                             
105400     03  FILLER                  PIC X       VALUE SPACE.                 
105500     03  W001-DET14-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
105600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105700     03  W001-DET14-OMSHASTB     PIC Z(5)9.9.                             
105800     03  FILLER                  PIC X       VALUE SPACE.                 
105900     03  W001-DET14-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
106000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106100     03  W001-DET14-OMSHASTC     PIC Z(5)9.9.                             
106200     03  FILLER                  PIC X       VALUE SPACE.                 
106300     03  W001-DET14-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
106400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106500     03  W001-DET14-OMSHASTD     PIC Z(5)9.9.                             
106600     03  FILLER                  PIC X       VALUE SPACE.                 
106700     03  W001-DET14-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
106800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106900     03  W001-DET14-OMSHASTE     PIC Z(5)9.9.                             
107000     03  FILLER                  PIC X       VALUE SPACE.                 
107100     03  W001-DET14-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
107200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107300     03  W001-DET14-OMSHASTF     PIC Z(5)9.9.                             
107400     03  FILLER                  PIC X       VALUE SPACE.                 
107500     03  W001-DET14-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
107600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107700     03  W001-DET14-OMSHASTG     PIC Z(5)9.9.                             
107800     03  FILLER                  PIC X       VALUE SPACE.                 
107900     03  W001-DET14-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
108000     03  FILLER                  PIC X       VALUE SPACE.                 
108100     03  W001-DET14-OMSHASTH     PIC Z(5)9.9.                             
108200     03  FILLER                  PIC X       VALUE SPACE.                 
108300     03  W001-DET14-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
108400     03  FILLER                  PIC X       VALUE SPACE.                 
108500     03  W001-DET14-TOT          PIC Z(11)9.9.                            
108600     03  FILLER                  PIC X       VALUE SPACE.                 
108700     03  W001-DET14-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
108800     EJECT                                                                
108900 01  W001-DETALJRAD-15.                                                   
109000     03  FILLER                  PIC X(3)    VALUE SPACE.                 
109100     03  FILLER                  PIC X(14) VALUE 'TOR BAL+AK+GIT'.        
109200     03  W001-DET15-OMSHASTA     PIC Z(4)9.9.                             
109300     03  FILLER                  PIC X       VALUE SPACE.                 
109400     03  W001-DET15-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
109500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109600     03  W001-DET15-OMSHASTB     PIC Z(5)9.9.                             
109700     03  FILLER                  PIC X       VALUE SPACE.                 
109800     03  W001-DET15-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
109900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
110000     03  W001-DET15-OMSHASTC     PIC Z(5)9.9.                             
110100     03  FILLER                  PIC X       VALUE SPACE.                 
110200     03  W001-DET15-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
110300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
110400     03  W001-DET15-OMSHASTD     PIC Z(5)9.9.                             
110500     03  FILLER                  PIC X       VALUE SPACE.                 
110600     03  W001-DET15-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
110700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
110800     03  W001-DET15-OMSHASTE     PIC Z(5)9.9.                             
110900     03  FILLER                  PIC X       VALUE SPACE.                 
111000     03  W001-DET15-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
111100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
111200     03  W001-DET15-OMSHASTF     PIC Z(5)9.9.                             
111300     03  FILLER                  PIC X       VALUE SPACE.                 
111400     03  W001-DET15-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
111500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
111600     03  W001-DET15-OMSHASTG     PIC Z(5)9.9.                             
111700     03  FILLER                  PIC X       VALUE SPACE.                 
111800     03  W001-DET15-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
111900     03  FILLER                  PIC X       VALUE SPACE.                 
112000     03  W001-DET15-OMSHASTH     PIC Z(5)9.9.                             
112100     03  FILLER                  PIC X       VALUE SPACE.                 
112200     03  W001-DET15-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
112300     03  FILLER                  PIC X       VALUE SPACE.                 
112400     03  W001-DET15-TOT          PIC Z(11)9.9.                            
112500     03  FILLER                  PIC X       VALUE SPACE.                 
112600     03  W001-DET15-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
112700     EJECT                                                                
112800 01  W001-DETALJRAD-16.                                                   
112900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
113000     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE G'.        
113100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
113200     03  W001-DET16-SERVG-BTOA   PIC Z9.9.                                
113300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113400     03  W001-DET16-SERVG-BTOB   PIC Z9.9.                                
113500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113600     03  W001-DET16-SERVG-BTOC   PIC Z9.9.                                
113700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113800     03  W001-DET16-SERVG-BTOD   PIC Z9.9.                                
113900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114000     03  W001-DET16-SERVG-BTOE   PIC Z9.9.                                
114100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114200     03  W001-DET16-SERVG-BTOF   PIC Z9.9.                                
114300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114400     03  W001-DET16-SERVG-BTOG   PIC Z9.9.                                
114500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114600     03  W001-DET16-SERVG-BTOH   PIC Z9.9.                                
114700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
114800     03  FILLER                  PIC X(11)   VALUE SPACE.                 
114900     03  W001-DET16-TOT          PIC Z9.9.                                
115000     03  FILLER                  PIC X       VALUE SPACE.                 
115100     03  W001-DET16-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
115200     EJECT                                                                
115300 01  W001-DETALJRAD-17.                                                   
115400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
115500     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE N'.        
115600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
115700     03  W001-DET17-SERVG-NTOA   PIC Z9.9.                                
115800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
115900     03  W001-DET17-SERVG-NTOB   PIC Z9.9.                                
116000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
116100     03  W001-DET17-SERVG-NTOC   PIC Z9.9.                                
116200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
116300     03  W001-DET17-SERVG-NTOD   PIC Z9.9.                                
116400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
116500     03  W001-DET17-SERVG-NTOE   PIC Z9.9.                                
116600     03  FILLER                  PIC X(10)   VALUE SPACE.                 
116700     03  W001-DET17-SERVG-NTOF   PIC Z9.9.                                
116800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
116900     03  W001-DET17-SERVG-NTOG   PIC Z9.9.                                
117000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
117100     03  W001-DET17-SERVG-NTOH   PIC Z9.9.                                
117200     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117300     03  FILLER                  PIC X(11)   VALUE SPACE.                 
117400     03  W001-DET17-TOT          PIC Z9.9.                                
117500     03  FILLER                  PIC X       VALUE SPACE.                 
117600     03  W001-DET17-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
117700     EJECT                                                                
117800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
117900*                                                                         
118000     EJECT                                                                
118100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
118200     SKIP3                                                                
118300 01  NYCKLAR-TILL-DLI.                                                    
118400     03  W-IDDC-B6-X.                                                     
118500         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
118600     SKIP2                                                                
118700*    --- STATUS-KOD FRÅN IMS                                              
118800 01  STATUS-WS                   PIC XX.                                  
118900     88  SEGMENT-FINNS                       VALUE '  '.                  
119000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
119100     SKIP2                                                                
119200 01  GODK-STATUSKODER.                                                    
119300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
119400     SKIP3                                                                
119500 01  SSA1                        PIC X(64).                               
119600 01  SSA2                        PIC X(64).                               
119700     EJECT                                                                
119800*    --- IMS FUNKTIONSKODER                                               
119900*01  -COPY W0003                                                          
120000     EJECT                                                                
120100*    ---  DLI INPUT-OUTPUT AREA                                           
120200 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
120300 01   DLI-IO-AREA-B6      PIC X(900).                                     
120400 01   DLI-IO-AREA-B601    REDEFINES DLI-IO-AREA-B6.                       
120500*     03  -COPY WDB601                                                    
120600     EJECT                                                                
120700 01   DLI-IO-AREA-B616    REDEFINES DLI-IO-AREA-B6.                       
120800*     03  -COPY WDB616                                                    
120900     EJECT                                                                
121000 LINKAGE SECTION.                                                         
121100                                                                          
121200*01  -COPY W0008      -PRE WDB6-                                          
121300     05  FILLER                  PIC X.                                   
121400                                                                          
121500 PROCEDURE DIVISION USING WDB6-PCB.                                       
121600                                                                          
121700     PERFORM A-INIT                                                       
121800     PERFORM B-SKAPA-LISTA                                                
121900     IF SW-TRAEFF = JA                                                    
122000        PERFORM C-SKRIV-LISTA                                             
122100     END-IF                                                               
122200     PERFORM Z-FINIT                                                      
122300                                                                          
122400     MOVE ZERO TO RETURN-CODE                                             
122500     GOBACK                                                               
122600     .                                                                    
122700     EJECT                                                                
122800 A-INIT SECTION.                                                          
122900                                                                          
123000     OPEN INPUT  W23195                                                   
123100                 W231PP                                                   
123200     OPEN OUTPUT W23198-001                                               
123300                                                                          
123400     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
123500     MOVE D-AAR    TO DAGENS-AAR                                          
123600                      D-VECKA-AAR                                         
123700     MOVE D-MAANAD TO DAGENS-MAANAD                                       
123800     MOVE D-VECKA  TO D-VECKA-VECKA                                       
123900     MOVE D-DAG    TO DAGENS-DAG                                          
124000     MOVE DAGENS-DATUM TO W001-DATUM                                      
124100     MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                              
124200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
124300                                                                          
124400     MOVE NEJ TO SW-TRAEFF                                                
124500                                                                          
124600     MOVE 'W23198-001' TO W001-LISTNR                                     
124700                          W001-LISTID                                     
124800                                                                          
124900     PERFORM S02-LAS-W231PP                                               
125000     DISPLAY '******** PARM-IDREFTAB ** ' PARM-IDREFTAB                   
125100     DISPLAY '********** PARM-IDUSER ** ' PARM-IDUSER                     
125200     DISPLAY '********** PARM-IDDC   ** ' PARM-IDDC                       
125300     MOVE PARM-IDREFTAB TO W001-IDREFTAB                                  
125400     MOVE PARM-IDUSER   TO W001-IDUSER                                    
125500     MOVE PARM-IDDC     TO W001-AKTUELLT-IDDC                             
125600                                                                          
125700     MOVE 'IDAG' TO DAT-KDDATFORM                                         
125800     CALL WDATKONV USING DAT-KDDATFORM                                    
125900                         DAT-I-TIDATUM                                    
126000                         DAT-O-TIDATUM                                    
126100                         DAT-KDSVAR                                       
126200     IF DAT-KDSVAR-OK                                                     
126300        MOVE DAT-TIAAVVD TO AAVVD                                         
126400        MOVE AAVV        TO W009VADD-DATUM-2                              
126500        MOVE -1          TO W009VADD-ANTAL-2                              
126600     ELSE                                                                 
126700        MOVE ZERO        TO W009VADD-DATUM-2                              
126800     END-IF                                                               
126900     CALL W009VADD USING W009VADD-DATUM-2 W009VADD-ANTAL-2                
127000     MOVE W009VADD-DATUM-2 TO W001-AKTUELL-VECKA                          
127100                                                                          
127200     PERFORM AB-LADDA-DC-TABELL                                           
127300     .                                                                    
127400     EJECT                                                                
127500 AB-LADDA-DC-TABELL SECTION.                                              
127600                                                                          
127700                                                                          
127800     INITIALIZE WS-DC-TABELL                                              
127900     MOVE 1 TO IDDC-IX                                                    
128000     PERFORM IMS-GET-WDB6                                                 
128100                                                                          
128200     PERFORM UNTIL SEGMENT-SAKNAS                                         
128300                OR IDDC-IX > IDDC-IX-MAX                                  
128400                                                                          
128500        IF WDB6-SEG-NAME-FB = 'WDB601  '                                  
128600           MOVE DCS-IDDC       TO W-IDDC                                  
128700           MOVE DCS-IDLEVNR-DC TO W-IDLEVNR-DC                            
128800        END-IF                                                            
128900                                                                          
129000        IF WDB6-SEG-NAME-FB = 'WDB616  '                                  
129100           MOVE W-IDDC           TO WS-IDDC-B601   (IDDC-IX)              
129200           MOVE W-IDLEVNR-DC     TO WS-IDLEVNR-DC  (IDDC-IX)              
129300           MOVE REF-IDDC-REF     TO WS-IDDC-B616   (IDDC-IX)              
129400           MOVE REF-KVDLTID-TOT  TO WS-KVDLTID-TOT (IDDC-IX)              
129500           ADD 1 TO IDDC-IX                                               
129600        END-IF                                                            
129700                                                                          
129800        PERFORM IMS-GET-WDB6                                              
129900     END-PERFORM                                                          
130000                                                                          
130100     .                                                                    
130200     EJECT                                                                
130300 B-SKAPA-LISTA SECTION.                                                   
130400                                                                          
130500                                                                          
130600     PERFORM BB-NOLLSTALL                                                 
130700                                                                          
130800     PERFORM S01-LAS-W23195                                               
130900     PERFORM UNTIL END-OF-W23195                                          
131000        PERFORM BA-HITTA-KVDLTID                                          
131100        IF IN-IDREFTAB = PARM-IDREFTAB                                    
131200           MOVE JA TO SW-TRAEFF                                           
131300           PERFORM BC-SKAPA-TABELLER                                      
131400        END-IF                                                            
131500        PERFORM S01-LAS-W23195                                            
131600     END-PERFORM                                                          
131700                                                                          
131800     IF SW-TRAEFF = JA                                                    
131900        PERFORM BD-SUMMERA                                                
132000     END-IF                                                               
132100     .                                                                    
132200     EJECT                                                                
132300 BA-HITTA-KVDLTID SECTION.                                                
132400                                                                          
132500*    HITTA SÄNDANDE DC MHA LEVERANTÖRSNUMMER                              
132600     MOVE +1 TO IDDC-IX                                                   
132700     PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                               
132800                   IN-IDLEVNR = WS-IDLEVNR-DC (IDDC-IX)                   
132900                                                                          
133000        ADD +1 TO IDDC-IX                                                 
133100     END-PERFORM                                                          
133200                                                                          
133300     IF IDDC-IX NOT > IDDC-IX-MAX                                         
133400*    VI HAR HITTAT LEVERANTÖREN WS-IDDC-B601 ÄR DÅ SÄNDANDE DC            
133500*    MOTTAGANDE DC FINNS I IN-IDLEVNR                                     
133600*    GÖR NY SÖKNING I TABELLEN MED DESSA VÄRDEN FÖR ATT HITTA             
133700*    RÄTT KVDAGAR                                                         
133800                                                                          
133900        MOVE WS-IDDC-B601(IDDC-IX) TO W-IDDC-SEND                         
134000        MOVE IN-IDDC               TO W-IDDC-REC                          
134100                                                                          
134200        MOVE +1 TO IDDC-IX                                                
134300        PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                            
134400                     (W-IDDC-REC  = WS-IDDC-B601 (IDDC-IX) AND            
134500                      W-IDDC-SEND = WS-IDDC-B616 (IDDC-IX))               
134600                                                                          
134700           ADD +1 TO IDDC-IX                                              
134800        END-PERFORM                                                       
134900        IF IDDC-IX > IDDC-IX-MAX                                          
135000           MOVE SPACE     TO W-IDDC-SEND                                  
135100           MOVE SPACE     TO W-IDDC-REC                                   
135200        END-IF                                                            
135300     ELSE                                                                 
135400           MOVE SPACE     TO W-IDDC-SEND                                  
135500           MOVE SPACE     TO W-IDDC-REC                                   
135600     END-IF                                                               
135700*    LÄS OM MED DC11 SOM SÄNDANDE........                                 
135800*    OM INGEN TRÄFF NU HELLER ABENDAR VI SKITEN...                        
135900     IF W-IDDC-SEND = SPACE                                               
136000        MOVE WC-CDC-SE    TO W-IDDC-SEND                                  
136100        MOVE IN-IDDC      TO W-IDDC-REC                                   
136200        MOVE +1 TO IDDC-IX                                                
136300        PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                            
136400                     (W-IDDC-REC  = WS-IDDC-B601 (IDDC-IX) AND            
136500                      W-IDDC-SEND = WS-IDDC-B616 (IDDC-IX))               
136600                                                                          
136700           ADD +1 TO IDDC-IX                                              
136800        END-PERFORM                                                       
136900        IF IDDC-IX > IDDC-IX-MAX                                          
137000           CALL FELLOG                                                    
137100        END-IF                                                            
137200     END-IF                                                               
137300     .                                                                    
137400     EJECT                                                                
137500 BB-NOLLSTALL SECTION.                                                    
137600******************************************************************        
137700*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
137800*  FREKVENSKLASS                                                 *        
137900*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
138000*     FREKV-KLASSER  = A B C D E F G                             *        
138100*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
138200*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
138300*                 TOTAL-SUMMERING                                *        
138400* ****************************************************************        
138500                                                                          
138600******* NOLLSTÄLLNING AV 72 'RUTOR' PER PRISKLASS/FREKVKLASS              
138700                                                                          
138800     MOVE +1  TO ART-IX                                                   
138900     MOVE +72 TO ART-IX-MAX                                               
139000     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
139100        MOVE ZERO TO     ART-KVANT-AKT(ART-IX)                            
139200                         ART-KVANT-PAS(ART-IX)                            
139300                         ART-PROC-KVANT-A(ART-IX)                         
139400                         ART-PROC-KVANT-P(ART-IX)                         
139500                         ART-KVDISP-AKT(ART-IX)                           
139600                         ART-PROC-KVDISP-A(ART-IX)                        
139700                         ART-KVDISP-PAS(ART-IX)                           
139800                         ART-PROC-KVDISP-P(ART-IX)                        
139900                         ART-LS-AKT(ART-IX)                               
140000                         ART-PROC-LS-A(ART-IX)                            
140100                         ART-LS-PAS(ART-IX)                               
140200                         ART-PROC-LS-P(ART-IX)                            
140300                         ART-AK-AKT(ART-IX)                               
140400                         ART-PROC-AK-A(ART-IX)                            
140500                         ART-AK-PAS(ART-IX)                               
140600                         ART-PROC-AK-P(ART-IX)                            
140700                         ART-OLAGER(ART-IX)                               
140800                         ART-PROC-OLAGER(ART-IX)                          
140900                         ART-SLAGER(ART-IX)                               
141000                         ART-PROC-SLAGER(ART-IX)                          
141100                         ART-MLAGER(ART-IX)                               
141200                         ART-PROC-MLAGER(ART-IX)                          
141300                         ART-KVOT(ART-IX)                                 
141400                         ART-PROC-KVOT(ART-IX)                            
141500                         ART-SPLIT(ART-IX)                                
141600                         ART-OMSHAST-DISP(ART-IX)                         
141700                         ART-OMSHAST-PROC-D(ART-IX)                       
141800                         ART-OMSHAST-LS(ART-IX)                           
141900                         ART-OMSHAST-PROC-LS(ART-IX)                      
142000                         ART-SERVG-BTO(ART-IX)                            
142100                         ART-SERVG-NTO(ART-IX)                            
142200****************                                                          
142300                         WS-ART-SLAGER(ART-IX)                            
142400                         WS-ART-OLAGER(ART-IX)                            
142500                         WS-ART-MLAGER(ART-IX)                            
142600                         WS-ART-KVLS-AKT(ART-IX)                          
142700                         WS-ART-KVLS-PAS(ART-IX)                          
142800                         WS-ART-LS-AKT(ART-IX)                            
142900                         WS-ART-LS-PAS(ART-IX)                            
143000                         WS-ART-LS-PR-AKT(ART-IX)                         
143100                         WS-ART-LS-PR-PAS(ART-IX)                         
143200                         WS-ART-KVDISP-AKT(ART-IX)                        
143300                         WS-ART-KVDISP-PAS(ART-IX)                        
143400                         WS-ART-KVDISP-PR-AKT(ART-IX)                     
143500                         WS-ART-KVDISP-PR-PAS(ART-IX)                     
143600                         WS-ART-KVOKS-AKT(ART-IX)                         
143700                         WS-ART-KVOKS-PAS(ART-IX)                         
143800                         WS-ART-OK-PR-AKT(ART-IX)                         
143900                         WS-ART-OK-PR-PAS(ART-IX)                         
144000                         WS-ART-KVAKS-AKT(ART-IX)                         
144100                         WS-ART-KVAKS-PAS(ART-IX)                         
144200                         WS-ART-AK-PR-AKT(ART-IX)                         
144300                         WS-ART-AK-PR-PAS(ART-IX)                         
144400                         WS-ART-KVOI(ART-IX)                              
144500                         WS-ART-KVOI-AKT(ART-IX)                          
144600                         WS-ART-KVOI-PAS(ART-IX)                          
144700                         WS-ART-KVOI-TEO(ART-IX)                          
144800                         WS-ART-KVOI-SAK(ART-IX)                          
144900                         WS-ART-KVOI-CDC-AKT(ART-IX)                      
145000                         WS-ART-KVOI-CDC-PAS(ART-IX)                      
145100                         WS-ART-KVOI-CDC-TEO(ART-IX)                      
145200                         WS-ART-KVOI-CDC-SAK(ART-IX)                      
145300                         WS-ART-SUINKORD(ART-IX)                          
145400                         WS-ART-SUFYSAVP(ART-IX)                          
145500                         WS-ART-SUAVBRP(ART-IX)                           
145600                         WS-ART-SULAGERB(ART-IX)                          
145700                         WS-ART-SUSORTB(ART-IX)                           
145800                                                                          
145900        ADD +1 TO ART-IX                                                  
146000     END-PERFORM                                                          
146100                                                                          
146200******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
146300*******                          OBEROENDE AV FREKVENSKLASS               
146400                                                                          
146500     MOVE +1 TO PSUM-IX                                                   
146600     MOVE +9 TO PSUM-IX-MAX                                               
146700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
146800        MOVE ZERO TO     PSUM-KVANT-AKT(PSUM-IX)                          
146900                         PSUM-KVANT-PAS(PSUM-IX)                          
147000                         PSUM-PROC-KVANT-A(PSUM-IX)                       
147100                         PSUM-PROC-KVANT-P(PSUM-IX)                       
147200                         PSUM-KVDISP-AKT(PSUM-IX)                         
147300                         PSUM-PROC-KVDISP-A(PSUM-IX)                      
147400                         PSUM-KVDISP-PAS(PSUM-IX)                         
147500                         PSUM-PROC-KVDISP-P(PSUM-IX)                      
147600                         PSUM-LS-AKT(PSUM-IX)                             
147700                         PSUM-PROC-LS-A(PSUM-IX)                          
147800                         PSUM-LS-PAS(PSUM-IX)                             
147900                         PSUM-PROC-LS-P(PSUM-IX)                          
148000                         PSUM-AK-AKT(PSUM-IX)                             
148100                         PSUM-PROC-AK-A(PSUM-IX)                          
148200                         PSUM-AK-PAS(PSUM-IX)                             
148300                         PSUM-PROC-AK-P(PSUM-IX)                          
148400                         PSUM-OLAGER(PSUM-IX)                             
148500                         PSUM-PROC-OLAGER(PSUM-IX)                        
148600                         PSUM-SLAGER(PSUM-IX)                             
148700                         PSUM-PROC-SLAGER(PSUM-IX)                        
148800                         PSUM-MLAGER(PSUM-IX)                             
148900                         PSUM-PROC-MLAGER(PSUM-IX)                        
149000                         PSUM-KVOT(PSUM-IX)                               
149100                         PSUM-PROC-KVOT(PSUM-IX)                          
149200                         PSUM-SPLIT(PSUM-IX)                              
149300                         PSUM-OMSHAST-DISP(PSUM-IX)                       
149400                         PSUM-OMSHAST-PROC-D(PSUM-IX)                     
149500                         PSUM-OMSHAST-LS(PSUM-IX)                         
149600                         PSUM-OMSHAST-PROC-LS(PSUM-IX)                    
149700                         PSUM-SERVG-BTO(PSUM-IX)                          
149800                         PSUM-SERVG-NTO(PSUM-IX)                          
149900*************                                                             
150000                         WS-PSUM-SLAGER(PSUM-IX)                          
150100                         WS-PSUM-OLAGER(PSUM-IX)                          
150200                         WS-PSUM-MLAGER(PSUM-IX)                          
150300                         WS-PSUM-LS-AKT(PSUM-IX)                          
150400                         WS-PSUM-LS-PAS(PSUM-IX)                          
150500                         WS-PSUM-LS-PR-AKT(PSUM-IX)                       
150600                         WS-PSUM-LS-PR-PAS(PSUM-IX)                       
150700                         WS-PSUM-KVDISP-AKT(PSUM-IX)                      
150800                         WS-PSUM-KVDISP-PAS(PSUM-IX)                      
150900                         WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                   
151000                         WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                   
151100                         WS-PSUM-KVOKS-AKT(PSUM-IX)                       
151200                         WS-PSUM-KVOKS-PAS(PSUM-IX)                       
151300                         WS-PSUM-OK-PR-AKT(PSUM-IX)                       
151400                         WS-PSUM-OK-PR-PAS(PSUM-IX)                       
151500                         WS-PSUM-KVAKS-AKT(PSUM-IX)                       
151600                         WS-PSUM-KVAKS-PAS(PSUM-IX)                       
151700                         WS-PSUM-AK-PR-AKT(PSUM-IX)                       
151800                         WS-PSUM-AK-PR-PAS(PSUM-IX)                       
151900                         WS-PSUM-KVOI(PSUM-IX)                            
152000                         WS-PSUM-KVOI-AKT(PSUM-IX)                        
152100                         WS-PSUM-KVOI-PAS(PSUM-IX)                        
152200                         WS-PSUM-KVOI-TEO(PSUM-IX)                        
152300                         WS-PSUM-KVOI-SAK(PSUM-IX)                        
152400                         WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                    
152500                         WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                    
152600                         WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                    
152700                         WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                    
152800                         WS-PSUM-SUINKORD(PSUM-IX)                        
152900                         WS-PSUM-SUFYSAVP(PSUM-IX)                        
153000                         WS-PSUM-SUAVBRP(PSUM-IX)                         
153100                         WS-PSUM-SULAGERB(PSUM-IX)                        
153200                         WS-PSUM-SUSORTB(PSUM-IX)                         
153300                                                                          
153400        ADD +1 TO PSUM-IX                                                 
153500     END-PERFORM                                                          
153600                                                                          
153700******* NOLLSTÄLLNING AV 7 'RUTOR' TOTALSUMMA PER FREKVENSKLASS           
153800*******                            OBEROENDE AV PRISKLASS                 
153900                                                                          
154000     MOVE +1 TO FSUM-IX                                                   
154100     MOVE +8 TO FSUM-IX-MAX                                               
154200     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
154300        MOVE ZERO TO     FSUM-KVANT-AKT(FSUM-IX)                          
154400                         FSUM-KVANT-PAS(FSUM-IX)                          
154500                         FSUM-PROC-KVANT-A(FSUM-IX)                       
154600                         FSUM-PROC-KVANT-P(FSUM-IX)                       
154700                         FSUM-KVDISP-AKT(FSUM-IX)                         
154800                         FSUM-PROC-KVDISP-A(FSUM-IX)                      
154900                         FSUM-KVDISP-PAS(FSUM-IX)                         
155000                         FSUM-PROC-KVDISP-P(FSUM-IX)                      
155100                         FSUM-LS-AKT(FSUM-IX)                             
155200                         FSUM-PROC-LS-A(FSUM-IX)                          
155300                         FSUM-LS-PAS(FSUM-IX)                             
155400                         FSUM-PROC-LS-P(FSUM-IX)                          
155500                         FSUM-AK-AKT(FSUM-IX)                             
155600                         FSUM-PROC-AK-A(FSUM-IX)                          
155700                         FSUM-AK-PAS(FSUM-IX)                             
155800                         FSUM-PROC-AK-P(FSUM-IX)                          
155900                         FSUM-OLAGER(FSUM-IX)                             
156000                         FSUM-PROC-OLAGER(FSUM-IX)                        
156100                         FSUM-SLAGER(FSUM-IX)                             
156200                         FSUM-PROC-SLAGER(FSUM-IX)                        
156300                         FSUM-MLAGER(FSUM-IX)                             
156400                         FSUM-PROC-MLAGER(FSUM-IX)                        
156500                         FSUM-KVOT(FSUM-IX)                               
156600                         FSUM-PROC-KVOT(FSUM-IX)                          
156700                         FSUM-SPLIT(FSUM-IX)                              
156800                         FSUM-OMSHAST-DISP(FSUM-IX)                       
156900                         FSUM-OMSHAST-PROC-D(FSUM-IX)                     
157000                         FSUM-OMSHAST-LS(FSUM-IX)                         
157100                         FSUM-OMSHAST-PROC-LS(FSUM-IX)                    
157200                         FSUM-SERVG-BTO(FSUM-IX)                          
157300                         FSUM-SERVG-NTO(FSUM-IX)                          
157400*****************                                                         
157500                         WS-FSUM-SLAGER(FSUM-IX)                          
157600                         WS-FSUM-OLAGER(FSUM-IX)                          
157700                         WS-FSUM-MLAGER(FSUM-IX)                          
157800                         WS-FSUM-LS-AKT(FSUM-IX)                          
157900                         WS-FSUM-LS-PAS(FSUM-IX)                          
158000                         WS-FSUM-LS-PR-AKT(FSUM-IX)                       
158100                         WS-FSUM-LS-PR-PAS(FSUM-IX)                       
158200                         WS-FSUM-KVDISP-AKT(FSUM-IX)                      
158300                         WS-FSUM-KVDISP-PAS(FSUM-IX)                      
158400                         WS-FSUM-KVDISP-PR-AKT(FSUM-IX)                   
158500                         WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                   
158600                         WS-FSUM-KVOKS-AKT(FSUM-IX)                       
158700                         WS-FSUM-KVOKS-PAS(FSUM-IX)                       
158800                         WS-FSUM-OK-PR-AKT(FSUM-IX)                       
158900                         WS-FSUM-OK-PR-PAS(FSUM-IX)                       
159000                         WS-FSUM-KVAKS-AKT(FSUM-IX)                       
159100                         WS-FSUM-KVAKS-PAS(FSUM-IX)                       
159200                         WS-FSUM-AK-PR-AKT(FSUM-IX)                       
159300                         WS-FSUM-AK-PR-PAS(FSUM-IX)                       
159400                         WS-FSUM-KVOI(FSUM-IX)                            
159500                         WS-FSUM-KVOI-AKT(FSUM-IX)                        
159600                         WS-FSUM-KVOI-PAS(FSUM-IX)                        
159700                         WS-FSUM-KVOI-TEO(FSUM-IX)                        
159800                         WS-FSUM-KVOI-SAK(FSUM-IX)                        
159900                         WS-FSUM-KVOI-CDC-AKT(FSUM-IX)                    
160000                         WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                    
160100                         WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                    
160200                         WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                    
160300                         WS-FSUM-SUINKORD(FSUM-IX)                        
160400                         WS-FSUM-SUFYSAVP(FSUM-IX)                        
160500                         WS-FSUM-SUAVBRP(FSUM-IX)                         
160600                         WS-FSUM-SULAGERB(FSUM-IX)                        
160700                         WS-FSUM-SUSORTB(FSUM-IX)                         
160800                                                                          
160900        ADD +1 TO FSUM-IX                                                 
161000     END-PERFORM                                                          
161100                                                                          
161200******* NOLLSTÄLLNING AV TOTALRUTA                                        
161300                                                                          
161400     MOVE ZERO TO     TOT-KVANT-AKT                                       
161500                      TOT-KVANT-PAS                                       
161600                      TOT-KVDISP-AKT                                      
161700                      TOT-KVDISP-PAS                                      
161800                      TOT-LS-AKT                                          
161900                      TOT-LS-PAS                                          
162000                      TOT-AK-AKT                                          
162100                      TOT-AK-PAS                                          
162200                      TOT-SLAGER                                          
162300                      TOT-MLAGER                                          
162400                      TOT-OLAGER                                          
162500                      TOT-KVOT                                            
162600                      TOT-PROC-OLAGER                                     
162700                      TOT-PROC-MLAGER                                     
162800                      TOT-PROC-SLAGER                                     
162900                      TOT-SPLIT                                           
163000                      TOT-OMSHAST-DISP                                    
163100                      TOT-OMSHAST-LS                                      
163200                      TOT-SERVG-BTO                                       
163300                      TOT-SERVG-NTO                                       
163400************                                                              
163500                      WS-TOT-SLAGER                                       
163600                      WS-TOT-OLAGER                                       
163700                      WS-TOT-MLAGER                                       
163800                      WS-TOT-LS-AKT                                       
163900                      WS-TOT-LS-PAS                                       
164000                      WS-TOT-LS-PR-AKT                                    
164100                      WS-TOT-LS-PR-PAS                                    
164200                      WS-TOT-KVDISP-AKT                                   
164300                      WS-TOT-KVDISP-PAS                                   
164400                      WS-TOT-KVDISP-PR-AKT                                
164500                      WS-TOT-KVDISP-PR-PAS                                
164600                      WS-TOT-KVOKS-AKT                                    
164700                      WS-TOT-KVOKS-PAS                                    
164800                      WS-TOT-OK-PR-AKT                                    
164900                      WS-TOT-OK-PR-PAS                                    
165000                      WS-TOT-KVAKS-AKT                                    
165100                      WS-TOT-KVAKS-PAS                                    
165200                      WS-TOT-AK-PR-AKT                                    
165300                      WS-TOT-AK-PR-PAS                                    
165400                      WS-TOT-KVOI                                         
165500                      WS-TOT-KVOI-AKT                                     
165600                      WS-TOT-KVOI-PAS                                     
165700                      WS-TOT-KVOI-TEO                                     
165800                      WS-TOT-KVOI-SAK                                     
165900                      WS-TOT-KVOI-CDC-AKT                                 
166000                      WS-TOT-KVOI-CDC-PAS                                 
166100                      WS-TOT-KVOI-CDC-TEO                                 
166200                      WS-TOT-KVOI-CDC-SAK                                 
166300                      WS-TOT-SUINKORD                                     
166400                      WS-TOT-SUFYSAVP                                     
166500                      WS-TOT-SUAVBRP                                      
166600                      WS-TOT-SULAGERB                                     
166700                      WS-TOT-SUSORTB                                      
166800     .                                                                    
166900     EJECT                                                                
167000 BC-SKAPA-TABELLER SECTION.                                               
167100                                                                          
167200     PERFORM BCA-SAETT-ART-IX                                             
167300     IF SW-ARTIKEL-SAKNAS-WDK7 = JA                                       
167400        PERFORM BCC-UPPDAT-SAKN-ART                                       
167500     END-IF                                                               
167600     IF ART-IX > ZERO                                                     
167700        PERFORM BCB-UPPDATERA-TABELLER                                    
167800     END-IF                                                               
167900     .                                                                    
168000     EJECT                                                                
168100 BCA-SAETT-ART-IX SECTION.                                                
168200******************************************************************        
168300* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
168400******************************************************************        
168500                                                                          
168600     MOVE NEJ TO SW-ARTIKEL-SAKNAS-WDK7                                   
168700     EVALUATE TRUE                                                        
168800     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
168900          MOVE +1 TO ART-IX                                               
169000     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
169100          MOVE +2 TO ART-IX                                               
169200     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
169300          MOVE +3 TO ART-IX                                               
169400     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
169500          MOVE +4 TO ART-IX                                               
169600     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
169700          MOVE +5 TO ART-IX                                               
169800     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
169900          MOVE +6 TO ART-IX                                               
170000     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
170100          MOVE +7 TO ART-IX                                               
170200     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
170300          MOVE +8 TO ART-IX                                               
170400                                                                          
170500     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
170600          MOVE +9 TO ART-IX                                               
170700     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
170800          MOVE +10 TO ART-IX                                              
170900     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
171000          MOVE +11 TO ART-IX                                              
171100     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
171200          MOVE +12 TO ART-IX                                              
171300     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
171400          MOVE +13 TO ART-IX                                              
171500     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
171600          MOVE +14 TO ART-IX                                              
171700     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
171800          MOVE +15 TO ART-IX                                              
171900     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
172000          MOVE +16 TO ART-IX                                              
172100                                                                          
172200     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
172300          MOVE +17 TO ART-IX                                              
172400     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
172500          MOVE +18 TO ART-IX                                              
172600     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
172700          MOVE +19 TO ART-IX                                              
172800     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
172900          MOVE +20 TO ART-IX                                              
173000     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
173100          MOVE +21 TO ART-IX                                              
173200     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
173300          MOVE +22 TO ART-IX                                              
173400     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
173500          MOVE +23 TO ART-IX                                              
173600     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
173700          MOVE +24 TO ART-IX                                              
173800                                                                          
173900     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
174000          MOVE +25 TO ART-IX                                              
174100     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
174200          MOVE +26 TO ART-IX                                              
174300     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
174400          MOVE +27 TO ART-IX                                              
174500     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
174600          MOVE +28 TO ART-IX                                              
174700     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
174800          MOVE +29 TO ART-IX                                              
174900     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
175000          MOVE +30 TO ART-IX                                              
175100     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
175200          MOVE +31 TO ART-IX                                              
175300     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
175400          MOVE +32 TO ART-IX                                              
175500                                                                          
175600     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
175700          MOVE +33 TO ART-IX                                              
175800     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
175900          MOVE +34 TO ART-IX                                              
176000     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
176100          MOVE +35 TO ART-IX                                              
176200     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
176300          MOVE +36 TO ART-IX                                              
176400     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
176500          MOVE +37 TO ART-IX                                              
176600     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
176700          MOVE +38 TO ART-IX                                              
176800     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
176900          MOVE +39 TO ART-IX                                              
177000     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
177100          MOVE +40 TO ART-IX                                              
177200                                                                          
177300     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
177400          MOVE +41 TO ART-IX                                              
177500     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
177600          MOVE +42 TO ART-IX                                              
177700     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
177800          MOVE +43 TO ART-IX                                              
177900     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
178000          MOVE +44 TO ART-IX                                              
178100     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
178200          MOVE +45 TO ART-IX                                              
178300     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
178400          MOVE +46 TO ART-IX                                              
178500     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
178600          MOVE +47 TO ART-IX                                              
178700     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
178800          MOVE +48 TO ART-IX                                              
178900                                                                          
179000     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
179100          MOVE +49 TO ART-IX                                              
179200     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
179300          MOVE +50 TO ART-IX                                              
179400     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
179500          MOVE +51 TO ART-IX                                              
179600     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
179700          MOVE +52 TO ART-IX                                              
179800     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
179900          MOVE +53 TO ART-IX                                              
180000     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
180100          MOVE +54 TO ART-IX                                              
180200     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
180300          MOVE +55 TO ART-IX                                              
180400     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
180500          MOVE +56 TO ART-IX                                              
180600                                                                          
180700     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
180800          MOVE +57 TO ART-IX                                              
180900     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
181000          MOVE +58 TO ART-IX                                              
181100     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
181200          MOVE +59 TO ART-IX                                              
181300     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
181400          MOVE +60 TO ART-IX                                              
181500     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
181600          MOVE +61 TO ART-IX                                              
181700     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
181800          MOVE +62 TO ART-IX                                              
181900     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
182000          MOVE +63 TO ART-IX                                              
182100     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
182200          MOVE +64 TO ART-IX                                              
182300                                                                          
182400     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
182500          MOVE +65 TO ART-IX                                              
182600     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
182700          MOVE +66 TO ART-IX                                              
182800     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
182900          MOVE +67 TO ART-IX                                              
183000     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
183100          MOVE +68 TO ART-IX                                              
183200     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
183300          MOVE +69 TO ART-IX                                              
183400     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
183500          MOVE +70 TO ART-IX                                              
183600     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
183700          MOVE +71 TO ART-IX                                              
183800     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
183900          MOVE +72 TO ART-IX                                              
184000     WHEN OTHER                                                           
184100          MOVE ZERO TO ART-IX                                             
184200          IF IN-KDPRISKL = SPACE AND IN-KDFREKKL = SPACE                  
184300             MOVE JA TO SW-ARTIKEL-SAKNAS-WDK7                            
184400          END-IF                                                          
184500     END-EVALUATE                                                         
184600     .                                                                    
184700     EJECT                                                                
184800 BCB-UPPDATERA-TABELLER SECTION.                                          
184900                                                                          
185000*********  ANTAL ARTIKLAR                                                 
185100     IF IN-KDREFSTA = 'A'                                                 
185200        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
185300     ELSE                                                                 
185400        IF IN-KDREFSTA = 'P'                                              
185500           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
185600        END-IF                                                            
185700     END-IF                                                               
185800                                                                          
185900*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
186000     IF IN-KDREFSTA = 'A'                                                 
186100        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
186200        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
186300        COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                           
186400        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                        
186500        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
186600                                                                          
186700        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                         
186800        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
186900                                                                          
187000        COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                          
187100        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
187200                                                                          
187300        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
187400        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
187500        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                         
187600        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
187700     ELSE                                                                 
187800        IF IN-KDREFSTA = 'P'                                              
187900           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
188000           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
188100           COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                        
188200           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                     
188300           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
188400                                                                          
188500           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                      
188600           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
188700                                                                          
188800           COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                       
188900           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
189000                                                                          
189100           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
189200           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
189300           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                      
189400           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
189500        END-IF                                                            
189600     END-IF                                                               
189700                                                                          
189800*********  OMSÄTTNINGSHASTIGHET                                           
189900     MOVE +1 TO KVOI-IX                                                   
190000     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
190100     PERFORM UNTIL KVOI-IX > 53                                           
190200        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
190300        ADD +1 TO KVOI-IX                                                 
190400     END-PERFORM                                                          
190500                                                                          
190600     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRARTSTD                      
190700     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
190800                                                                          
190900*********  SÄKERHETSLAGER/ARTIKEL                                         
191000     IF IN-KDREFSTA = 'A'                                                 
191100        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRARTSTD                      
191200        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
191300     END-IF                                                               
191400                                                                          
191500*********  ÖVERLAGER/ARTIKEL                                              
191600     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
191700     IF WS-KVDISP > IN-KVREFOVL                                           
191800        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
191900        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRARTSTD                        
192000        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
192100     END-IF                                                               
192200                                                                          
192300*********  MEDELLAGER/ARTIKEL                                             
192400     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
192500     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
192600     COMPUTE WS-LT-BEHOV-SDC-NORM = WS-KVDLTID-TOT (IDDC-IX)              
192700                                  * WS-KVPB-DAG-SDC-NORM                  
192800     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
192900                        + (IN-KVREFBER / 2)                               
193000     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRARTSTD                           
193100     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
193200                                                                          
193300*********  SERVICEGRAD OCH SPLITFAKTOR ORDERRADER/ARTIKEL                 
193400                                                                          
193500     MOVE +1 TO KVOI-IX                                                   
193600     MOVE NEJ TO SW-KVOI-TRAFF                                            
193700     PERFORM UNTIL KVOI-IX > 5                                            
193800        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
193900           MOVE JA TO SW-KVOI-TRAFF                                       
194000           IF IN-KDREFSTA = 'A'                                           
194100              ADD IN-KVOT-INNEV(KVOI-IX)                                  
194200                                TO WS-ART-KVOI-AKT(ART-IX)                
194300              ADD IN-KVOT-CDC-INNEV(KVOI-IX)                              
194400                                TO WS-ART-KVOI-CDC-AKT(ART-IX)            
194500           ELSE                                                           
194600              IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                     
194700                 ADD IN-KVOT-INNEV(KVOI-IX)                               
194800                                TO WS-ART-KVOI-TEO(ART-IX)                
194900                 ADD IN-KVOT-CDC-INNEV(KVOI-IX)                           
195000                                TO WS-ART-KVOI-CDC-TEO(ART-IX)            
195100              ELSE                                                        
195200                 IF IN-KDREFSTA = 'P'                                     
195300                    ADD IN-KVOT-INNEV(KVOI-IX)                            
195400                                   TO WS-ART-KVOI-PAS(ART-IX)             
195500                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
195600                                   TO WS-ART-KVOI-CDC-PAS(ART-IX)         
195700                 ELSE                                                     
195800                    ADD IN-KVOT-INNEV(KVOI-IX)                            
195900                                   TO WS-ART-KVOI-SAK(ART-IX)             
196000                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
196100                                   TO WS-ART-KVOI-CDC-SAK(ART-IX)         
196200                 END-IF                                                   
196300              END-IF                                                      
196400           END-IF                                                         
196500        END-IF                                                            
196600        ADD +1 TO KVOI-IX                                                 
196700     END-PERFORM                                                          
196800                                                                          
196900     IF SW-KVOI-TRAFF = NEJ                                               
197000        MOVE D-VECKA-VECKA TO KVOI-IX                                     
197100        IF IN-KDREFSTA = 'A'                                              
197200           ADD IN-KVOT-RULL(KVOI-IX)                                      
197300                              TO WS-ART-KVOI-AKT(ART-IX)                  
197400           ADD IN-KVOT-CDC-RULL(KVOI-IX)                                  
197500                              TO WS-ART-KVOI-CDC-AKT(ART-IX)              
197600        ELSE                                                              
197700           IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                        
197800              ADD IN-KVOT-RULL(KVOI-IX)                                   
197900                              TO WS-ART-KVOI-TEO(ART-IX)                  
198000              ADD IN-KVOT-CDC-RULL(KVOI-IX)                               
198100                              TO WS-ART-KVOI-CDC-TEO(ART-IX)              
198200           ELSE                                                           
198300              IF IN-KDREFSTA = 'P'                                        
198400                 ADD IN-KVOT-RULL(KVOI-IX)                                
198500                                 TO WS-ART-KVOI-PAS(ART-IX)               
198600                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
198700                                 TO WS-ART-KVOI-CDC-PAS(ART-IX)           
198800              ELSE                                                        
198900                 ADD IN-KVOT-RULL(KVOI-IX)                                
199000                                 TO WS-ART-KVOI-SAK(ART-IX)               
199100                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
199200                                 TO WS-ART-KVOI-CDC-SAK(ART-IX)           
199300              END-IF                                                      
199400           END-IF                                                         
199500        END-IF                                                            
199600     END-IF                                                               
199700                                                                          
199800*********  SERVICEGRAD OCH SPLITFAKTOR FRÅN SRS                           
199900                                                                          
200000     MOVE IN-SUINKORD    TO WS-FIXAD-SUMMA                                
200100     ADD WS-FIXAD-SUMMA  TO WS-ART-SUINKORD(ART-IX)                       
200200     ADD IN-SUFYSAVP     TO WS-ART-SUFYSAVP(ART-IX)                       
200300     ADD IN-SUAVBRP      TO WS-ART-SUAVBRP (ART-IX)                       
200400     MOVE IN-SULAGERB    TO WS-FIXAD-SUMMA                                
200500     ADD WS-FIXAD-SUMMA  TO WS-ART-SULAGERB(ART-IX)                       
200600     MOVE IN-SUSORTB     TO WS-FIXAD-SUMMA                                
200700     ADD WS-FIXAD-SUMMA  TO WS-ART-SUSORTB (ART-IX)                       
200800     .                                                                    
200900     EJECT                                                                
201000 BCC-UPPDAT-SAKN-ART SECTION.                                             
201100                                                                          
201200     MOVE +1 TO KVOI-IX                                                   
201300     MOVE NEJ TO SW-KVOI-TRAFF                                            
201400     PERFORM UNTIL KVOI-IX > 5                                            
201500        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
201600           MOVE JA TO SW-KVOI-TRAFF                                       
201700           ADD IN-KVOT-INNEV(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7              
201800           ADD IN-KVOT-CDC-INNEV(KVOI-IX)                                 
201900                                    TO WS-KVOT-CDC-SAKNAS-WDK7            
202000        END-IF                                                            
202100        ADD +1 TO KVOI-IX                                                 
202200     END-PERFORM                                                          
202300                                                                          
202400     IF SW-KVOI-TRAFF = NEJ                                               
202500        MOVE D-VECKA-VECKA TO KVOI-IX                                     
202600        ADD IN-KVOT-RULL(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7                  
202700        ADD IN-KVOT-CDC-RULL(KVOI-IX) TO WS-KVOT-CDC-SAKNAS-WDK7          
202800     END-IF                                                               
202900     .                                                                    
203000     EJECT                                                                
203100 BD-SUMMERA SECTION.                                                      
203200                                                                          
203300     PERFORM BDA-SUMMERA-RUTA                                             
203400     PERFORM BDB-SUMMERA-PRISKLASS                                        
203500     PERFORM BDC-SUMMERA-FREKVENSKLASS                                    
203600     PERFORM BDD-SUMMERA-TOTAL                                            
203700     PERFORM BDE-BERAKNINGAR-AV-TOTAL                                     
203800     .                                                                    
203900     EJECT                                                                
204000 BDA-SUMMERA-RUTA SECTION.                                                
204100                                                                          
204200     MOVE +1  TO ART-IX                                                   
204300     MOVE +72 TO ART-IX-MAX                                               
204400     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
204500                                                                          
204600*********  DISP LAGER VÄRDE/RUTA                                          
204700        IF WS-ART-KVDISP-PR-AKT(ART-IX) = ZERO                            
204800           CONTINUE                                                       
204900        ELSE                                                              
205000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
205100                          WS-ART-KVDISP-PR-AKT(ART-IX) / 1000             
205200           MOVE WS-SUMMA-KR TO ART-KVDISP-AKT(ART-IX)                     
205300        END-IF                                                            
205400                                                                          
205500        IF WS-ART-KVDISP-PR-PAS(ART-IX) = ZERO                            
205600           CONTINUE                                                       
205700        ELSE                                                              
205800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
205900                          WS-ART-KVDISP-PR-PAS(ART-IX) / 1000             
206000           MOVE WS-SUMMA-KR TO ART-KVDISP-PAS(ART-IX)                     
206100        END-IF                                                            
206200                                                                          
206300*********  LAGERVÄRDE/RUTA                                                
206400        IF WS-ART-LS-PR-AKT(ART-IX) = ZERO                                
206500           CONTINUE                                                       
206600        ELSE                                                              
206700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
206800                          WS-ART-LS-PR-AKT(ART-IX) / 1000                 
206900           MOVE WS-SUMMA-KR TO ART-LS-AKT(ART-IX)                         
207000        END-IF                                                            
207100                                                                          
207200        IF WS-ART-LS-PR-PAS(ART-IX) = ZERO                                
207300           CONTINUE                                                       
207400        ELSE                                                              
207500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
207600                          WS-ART-LS-PR-PAS(ART-IX) / 1000                 
207700           MOVE WS-SUMMA-KR TO ART-LS-PAS(ART-IX)                         
207800        END-IF                                                            
207900                                                                          
208000*********  AK-VÄRDE/RUTA                                                  
208100        IF WS-ART-AK-PR-AKT(ART-IX) = ZERO                                
208200           CONTINUE                                                       
208300        ELSE                                                              
208400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
208500                          WS-ART-AK-PR-AKT(ART-IX) / 1000                 
208600           MOVE WS-SUMMA-KR TO ART-AK-AKT(ART-IX)                         
208700        END-IF                                                            
208800                                                                          
208900        IF WS-ART-AK-PR-PAS(ART-IX) = ZERO                                
209000           CONTINUE                                                       
209100        ELSE                                                              
209200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
209300                          WS-ART-AK-PR-PAS(ART-IX) / 1000                 
209400           MOVE WS-SUMMA-KR TO ART-AK-PAS(ART-IX)                         
209500        END-IF                                                            
209600                                                                          
209700*********  SÄKERHETSLAGER/RUTA                                            
209800        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
209900           CONTINUE                                                       
210000        ELSE                                                              
210100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
210200                          WS-ART-SLAGER(ART-IX) / 1000                    
210300           MOVE WS-SUMMA-KR TO ART-SLAGER(ART-IX)                         
210400        END-IF                                                            
210500                                                                          
210600*********  ÖVERLAGER/RUTA                                                 
210700        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
210800           CONTINUE                                                       
210900        ELSE                                                              
211000           COMPUTE WS-SUMMA-KR ROUNDED                                    
211100                              = WS-ART-OLAGER(ART-IX) / 1000              
211200           MOVE WS-SUMMA-KR TO ART-OLAGER(ART-IX)                         
211300        END-IF                                                            
211400                                                                          
211500*********  MEDELLAGER/RUTA                                                
211600        IF WS-ART-MLAGER(ART-IX) = ZERO                                   
211700           CONTINUE                                                       
211800        ELSE                                                              
211900           COMPUTE WS-SUMMA-KR =                                          
212000                                WS-ART-MLAGER(ART-IX) / 1000              
212100           ADD WS-SUMMA-KR TO ART-MLAGER(ART-IX)                          
212200        END-IF                                                            
212300                                                                          
212400*********  OMSHASTIGHET/RUTA                                              
212500        COMPUTE WS-SUMMA = WS-ART-KVDISP-PR-AKT(ART-IX) +                 
212600                           WS-ART-KVDISP-PR-PAS(ART-IX)                   
212700        IF WS-SUMMA = ZERO                                                
212800           CONTINUE                                                       
212900        ELSE                                                              
213000           COMPUTE WS-OMSHAST ROUNDED =                                   
213100               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
213200           MOVE WS-OMSHAST TO ART-OMSHAST-DISP(ART-IX)                    
213300        END-IF                                                            
213400                                                                          
213500        COMPUTE WS-SUMMA = WS-ART-LS-PR-AKT(ART-IX) +                     
213600                           WS-ART-AK-PR-AKT(ART-IX) +                     
213700                           WS-ART-LS-PR-PAS(ART-IX) +                     
213800                           WS-ART-AK-PR-PAS(ART-IX)                       
213900        IF WS-SUMMA = ZERO                                                
214000           CONTINUE                                                       
214100        ELSE                                                              
214200           COMPUTE WS-OMSHAST ROUNDED =                                   
214300               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
214400           MOVE WS-OMSHAST TO ART-OMSHAST-LS(ART-IX)                      
214500        END-IF                                                            
214600                                                                          
214700*********  SERVICEGRAD BRUTTO/RUTA                                        
214800        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
214900           MOVE 99.9   TO ART-SERVG-BTO(ART-IX)                           
215000        ELSE                                                              
215100           COMPUTE WS-SERVG ROUNDED =                                     
215200             WS-ART-SUAVBRP(ART-IX) * 100 /                               
215300                          WS-ART-SUINKORD(ART-IX)                         
215400           IF WS-SERVG = 100.0                                            
215500              MOVE 99.9 TO ART-SERVG-BTO(ART-IX)                          
215600           ELSE                                                           
215700              MOVE WS-SERVG TO ART-SERVG-BTO(ART-IX)                      
215800           END-IF                                                         
215900        END-IF                                                            
216000                                                                          
216100*********  SERVICEGRAD NETTO/RUTA                                         
216200        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
216300           MOVE 99.9   TO ART-SERVG-NTO(ART-IX)                           
216400        ELSE                                                              
216500           COMPUTE WS-SERVG ROUNDED =                                     
216600             (WS-ART-SUAVBRP(ART-IX) - WS-ART-SUFYSAVP(ART-IX))           
216700                           * 100 /                                        
216800                          WS-ART-SUINKORD(ART-IX)                         
216900           IF WS-SERVG = 100.0                                            
217000              MOVE 99.9 TO ART-SERVG-NTO(ART-IX)                          
217100           ELSE                                                           
217200              MOVE WS-SERVG TO ART-SERVG-NTO(ART-IX)                      
217300           END-IF                                                         
217400        END-IF                                                            
217500                                                                          
217600*********  SPLITFAKTOR/RUTA                                               
217700        IF (WS-ART-SUINKORD(ART-IX) + WS-ART-SULAGERB (ART-IX) +          
217800            WS-ART-SUSORTB (ART-IX)) = ZERO                               
217900           MOVE 99.9 TO ART-SPLIT(ART-IX)                                 
218000        ELSE                                                              
218100           COMPUTE WS-SERVG ROUNDED = (WS-ART-SUINKORD(ART-IX) +          
218200                         WS-ART-SULAGERB (ART-IX)) * 100                  
218300                        / (WS-ART-SUINKORD(ART-IX) +                      
218400                           WS-ART-SULAGERB(ART-IX) +                      
218500                           WS-ART-SUSORTB(ART-IX))                        
218600           IF WS-SERVG = 100.0                                            
218700              MOVE 99.9 TO ART-SPLIT(ART-IX)                              
218800           ELSE                                                           
218900              MOVE WS-SERVG TO ART-SPLIT(ART-IX)                          
219000           END-IF                                                         
219100        END-IF                                                            
219200*********  ORDERTRÄFFAR/RUTA                                              
219300*       COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
219400*                                   WS-ART-KVOI-PAS(ART-IX) +             
219500*                                   WS-ART-KVOI-TEO(ART-IX) +             
219600*                                   WS-ART-KVOI-SAK(ART-IX)               
219700*       ADD WS-KVOI-TOT-VECKA TO ART-KVOT(ART-IX)                         
219800                                                                          
219900        MOVE WS-ART-SUINKORD(ART-IX) TO ART-KVOT(ART-IX)                  
220000                                                                          
220100        ADD +1  TO ART-IX                                                 
220200     END-PERFORM                                                          
220300     .                                                                    
220400     EJECT                                                                
220500 BDB-SUMMERA-PRISKLASS SECTION.                                           
220600******************************************************************        
220700* SUMMERING PER PRISKLASS                                        *        
220800******************************************************************        
220900                                                                          
221000     MOVE +1 TO PSUM-IX                                                   
221100                ART-IX                                                    
221200     MOVE +8 TO ART-IX-MAX                                                
221300                                                                          
221400     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
221500        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
221600                                                                          
221700          ADD ART-KVANT-AKT(ART-IX) TO PSUM-KVANT-AKT(PSUM-IX)            
221800          ADD ART-KVANT-PAS(ART-IX) TO PSUM-KVANT-PAS(PSUM-IX)            
221900          ADD ART-KVOT(ART-IX)      TO PSUM-KVOT(PSUM-IX)                 
222000          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-PSUM-KVOI-AKT(PSUM-IX)        
222100          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-PSUM-KVOI-PAS(PSUM-IX)        
222200          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-PSUM-KVOI-TEO(PSUM-IX)        
222300          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-PSUM-KVOI-SAK(PSUM-IX)        
222400          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
222500                                  TO WS-PSUM-KVOI-CDC-AKT(PSUM-IX)        
222600          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
222700                                  TO WS-PSUM-KVOI-CDC-PAS(PSUM-IX)        
222800          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
222900                                  TO WS-PSUM-KVOI-CDC-TEO(PSUM-IX)        
223000          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
223100                                  TO WS-PSUM-KVOI-CDC-SAK(PSUM-IX)        
223200          ADD WS-ART-KVOI(ART-IX) TO WS-PSUM-KVOI(PSUM-IX)                
223300          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
223400                             TO WS-PSUM-KVDISP-PR-AKT (PSUM-IX)           
223500          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
223600                             TO WS-PSUM-OK-PR-AKT(PSUM-IX)                
223700          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
223800                             TO WS-PSUM-LS-PR-AKT(PSUM-IX)                
223900          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
224000                             TO WS-PSUM-AK-PR-AKT(PSUM-IX)                
224100          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
224200                             TO WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
224300          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
224400                             TO WS-PSUM-OK-PR-PAS(PSUM-IX)                
224500          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
224600                             TO WS-PSUM-LS-PR-PAS(PSUM-IX)                
224700          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
224800                             TO WS-PSUM-AK-PR-PAS(PSUM-IX)                
224900          ADD WS-ART-OLAGER(ART-IX)   TO WS-PSUM-OLAGER(PSUM-IX)          
225000          ADD WS-ART-MLAGER(ART-IX)   TO WS-PSUM-MLAGER(PSUM-IX)          
225100          ADD WS-ART-SLAGER(ART-IX)   TO WS-PSUM-SLAGER(PSUM-IX)          
225200          ADD WS-ART-SUINKORD(ART-IX) TO WS-PSUM-SUINKORD(PSUM-IX)        
225300          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-PSUM-SUFYSAVP(PSUM-IX)        
225400          ADD WS-ART-SUAVBRP (ART-IX) TO WS-PSUM-SUAVBRP (PSUM-IX)        
225500          ADD WS-ART-SULAGERB(ART-IX) TO WS-PSUM-SULAGERB(PSUM-IX)        
225600          ADD WS-ART-SUSORTB (ART-IX) TO WS-PSUM-SUSORTB (PSUM-IX)        
225700                                                                          
225800          ADD +1 TO ART-IX                                                
225900        END-PERFORM                                                       
226000                                                                          
226100        ADD +1 TO PSUM-IX                                                 
226200        ADD +8 TO ART-IX-MAX                                              
226300     END-PERFORM                                                          
226400                                                                          
226500     MOVE +1 TO PSUM-IX                                                   
226600     MOVE +9 TO PSUM-IX-MAX                                               
226700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
226800                                                                          
226900*********  DISP LAGER VÄRDE/PRISKLASS                                     
227000        IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) = ZERO                          
227100           CONTINUE                                                       
227200        ELSE                                                              
227300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
227400                     WS-PSUM-KVDISP-PR-AKT(PSUM-IX) / 1000                
227500           MOVE WS-SUMMA-KR TO PSUM-KVDISP-AKT(PSUM-IX)                   
227600        END-IF                                                            
227700                                                                          
227800        IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) = ZERO                          
227900           CONTINUE                                                       
228000        ELSE                                                              
228100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
228200                     WS-PSUM-KVDISP-PR-PAS(PSUM-IX) / 1000                
228300           MOVE WS-SUMMA-KR TO PSUM-KVDISP-PAS(PSUM-IX)                   
228400        END-IF                                                            
228500                                                                          
228600*********  LAGERVÄRDE/PRISKLASS                                           
228700        IF WS-PSUM-LS-PR-AKT(PSUM-IX) = ZERO                              
228800           CONTINUE                                                       
228900        ELSE                                                              
229000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
229100                     WS-PSUM-LS-PR-AKT(PSUM-IX) / 1000                    
229200           MOVE WS-SUMMA-KR TO PSUM-LS-AKT(PSUM-IX)                       
229300        END-IF                                                            
229400                                                                          
229500        IF WS-PSUM-LS-PR-PAS(PSUM-IX) = ZERO                              
229600           CONTINUE                                                       
229700        ELSE                                                              
229800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
229900                     WS-PSUM-LS-PR-PAS(PSUM-IX) / 1000                    
230000           MOVE WS-SUMMA-KR TO PSUM-LS-PAS(PSUM-IX)                       
230100        END-IF                                                            
230200                                                                          
230300*********  AK-VÄRDE/PRISKLASS                                             
230400        IF WS-PSUM-AK-PR-AKT(PSUM-IX) = ZERO                              
230500           CONTINUE                                                       
230600        ELSE                                                              
230700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
230800                     WS-PSUM-AK-PR-AKT(PSUM-IX) / 1000                    
230900           MOVE WS-SUMMA-KR TO PSUM-AK-AKT(PSUM-IX)                       
231000        END-IF                                                            
231100                                                                          
231200        IF WS-PSUM-AK-PR-PAS(PSUM-IX) = ZERO                              
231300           CONTINUE                                                       
231400        ELSE                                                              
231500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
231600                     WS-PSUM-AK-PR-PAS(PSUM-IX) / 1000                    
231700           MOVE WS-SUMMA-KR TO PSUM-AK-PAS(PSUM-IX)                       
231800        END-IF                                                            
231900                                                                          
232000*********  SÄKERHETSLAGER/PRISKLASS                                       
232100        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
232200           CONTINUE                                                       
232300        ELSE                                                              
232400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
232500                          WS-PSUM-SLAGER(PSUM-IX) / 1000                  
232600           MOVE WS-SUMMA-KR TO PSUM-SLAGER(PSUM-IX)                       
232700        END-IF                                                            
232800                                                                          
232900*********  ÖVERLAGER/PRISKLASS                                            
233000        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
233100           CONTINUE                                                       
233200        ELSE                                                              
233300           COMPUTE WS-SUMMA-KR ROUNDED                                    
233400                     = WS-PSUM-OLAGER(PSUM-IX) / 1000                     
233500           MOVE WS-SUMMA-KR TO PSUM-OLAGER(PSUM-IX)                       
233600        END-IF                                                            
233700                                                                          
233800*********  MEDELLAGER/PRISKLASS                                           
233900        IF WS-PSUM-MLAGER(PSUM-IX) = ZERO                                 
234000           CONTINUE                                                       
234100        ELSE                                                              
234200           COMPUTE WS-SUMMA-KR =                                          
234300                       WS-PSUM-MLAGER(PSUM-IX) / 1000                     
234400           ADD WS-SUMMA-KR TO PSUM-MLAGER(PSUM-IX)                        
234500        END-IF                                                            
234600                                                                          
234700*********  SERVICEGRAD BRUTTO/PRISKLASS                                   
234800        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
234900           MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                           
235000        ELSE                                                              
235100           COMPUTE WS-SERVG ROUNDED =                                     
235200             WS-PSUM-SUAVBRP(PSUM-IX) * 100 /                             
235300                          WS-PSUM-SUINKORD(PSUM-IX)                       
235400           IF WS-SERVG = 100.0                                            
235500              MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                        
235600           ELSE                                                           
235700              MOVE WS-SERVG TO PSUM-SERVG-BTO(PSUM-IX)                    
235800           END-IF                                                         
235900        END-IF                                                            
236000                                                                          
236100*********  SERVICEGRAD NETTO/PRISKLASS                                    
236200        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
236300           MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                           
236400        ELSE                                                              
236500           COMPUTE WS-SERVG ROUNDED =                                     
236600            (WS-PSUM-SUAVBRP(PSUM-IX) - WS-PSUM-SUFYSAVP(PSUM-IX))        
236700                           * 100 /                                        
236800                          WS-PSUM-SUINKORD(PSUM-IX)                       
236900           IF WS-SERVG = 100.0                                            
237000              MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                        
237100           ELSE                                                           
237200              MOVE WS-SERVG TO PSUM-SERVG-NTO(PSUM-IX)                    
237300           END-IF                                                         
237400        END-IF                                                            
237500                                                                          
237600*********  SPLITFAKTOR/PRISKLASS                                          
237700        IF (WS-PSUM-SUINKORD(PSUM-IX) +                                   
237800            WS-PSUM-SULAGERB (PSUM-IX) +                                  
237900            WS-PSUM-SUSORTB (PSUM-IX)) = ZERO                             
238000              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
238100        ELSE                                                              
238200           COMPUTE WS-SERVG ROUNDED = (WS-PSUM-SUINKORD(PSUM-IX)          
238300                       + WS-PSUM-SULAGERB (PSUM-IX)) * 100                
238400                        / (WS-PSUM-SUINKORD(PSUM-IX) +                    
238500                           WS-PSUM-SULAGERB(PSUM-IX) +                    
238600                           WS-PSUM-SUSORTB(PSUM-IX))                      
238700           IF WS-SERVG = 100.0                                            
238800              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
238900           ELSE                                                           
239000              MOVE WS-SERVG TO PSUM-SPLIT(PSUM-IX)                        
239100           END-IF                                                         
239200        END-IF                                                            
239300                                                                          
239400*********  OMSHASTIGHET/PRISKLASS                                         
239500        COMPUTE WS-SUMMA = WS-PSUM-KVDISP-PR-AKT(PSUM-IX) +               
239600                           WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                 
239700        IF WS-SUMMA = ZERO                                                
239800           CONTINUE                                                       
239900        ELSE                                                              
240000           COMPUTE WS-OMSHAST ROUNDED =                                   
240100               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
240200           MOVE WS-OMSHAST TO PSUM-OMSHAST-DISP(PSUM-IX)                  
240300        END-IF                                                            
240400                                                                          
240500        COMPUTE WS-SUMMA = WS-PSUM-LS-PR-AKT(PSUM-IX) +                   
240600                           WS-PSUM-AK-PR-AKT(PSUM-IX) +                   
240700                           WS-PSUM-LS-PR-PAS(PSUM-IX) +                   
240800                           WS-PSUM-AK-PR-PAS(PSUM-IX)                     
240900        IF WS-SUMMA = ZERO                                                
241000           CONTINUE                                                       
241100        ELSE                                                              
241200           COMPUTE WS-OMSHAST ROUNDED =                                   
241300               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
241400           MOVE WS-OMSHAST TO PSUM-OMSHAST-LS(PSUM-IX)                    
241500        END-IF                                                            
241600                                                                          
241700        ADD +1 TO PSUM-IX                                                 
241800     END-PERFORM                                                          
241900     .                                                                    
242000     EJECT                                                                
242100 BDC-SUMMERA-FREKVENSKLASS SECTION.                                       
242200******************************************************************        
242300* SUMMERING PER FREKVENSKLASS                                    *        
242400******************************************************************        
242500                                                                          
242600     MOVE +1 TO FSUM-IX                                                   
242700                ART-IX                                                    
242800     MOVE +65 TO ART-IX-MAX                                               
242900     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
243000                                                                          
243100        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
243200          ADD ART-KVANT-AKT(ART-IX) TO FSUM-KVANT-AKT(FSUM-IX)            
243300          ADD ART-KVANT-PAS(ART-IX) TO FSUM-KVANT-PAS(FSUM-IX)            
243400          ADD ART-KVOT(ART-IX)      TO FSUM-KVOT(FSUM-IX)                 
243500          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-FSUM-KVOI-AKT(FSUM-IX)        
243600          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-FSUM-KVOI-PAS(FSUM-IX)        
243700          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-FSUM-KVOI-TEO(FSUM-IX)        
243800          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-FSUM-KVOI-SAK(FSUM-IX)        
243900          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
244000                                  TO WS-FSUM-KVOI-CDC-AKT(FSUM-IX)        
244100          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
244200                                  TO WS-FSUM-KVOI-CDC-PAS(FSUM-IX)        
244300          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
244400                                  TO WS-FSUM-KVOI-CDC-TEO(FSUM-IX)        
244500          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
244600                                  TO WS-FSUM-KVOI-CDC-SAK(FSUM-IX)        
244700          ADD WS-ART-KVOI(ART-IX) TO WS-FSUM-KVOI(FSUM-IX)                
244800          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
244900                             TO WS-FSUM-KVDISP-PR-AKT (FSUM-IX)           
245000          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
245100                             TO WS-FSUM-OK-PR-AKT(FSUM-IX)                
245200          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
245300                             TO WS-FSUM-LS-PR-AKT(FSUM-IX)                
245400          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
245500                             TO WS-FSUM-AK-PR-AKT(FSUM-IX)                
245600          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
245700                             TO WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
245800          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
245900                             TO WS-FSUM-OK-PR-PAS(FSUM-IX)                
246000          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
246100                             TO WS-FSUM-LS-PR-PAS(FSUM-IX)                
246200          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
246300                             TO WS-FSUM-AK-PR-PAS(FSUM-IX)                
246400          ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)            
246500          ADD WS-ART-MLAGER(ART-IX) TO WS-FSUM-MLAGER(FSUM-IX)            
246600          ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)            
246700          ADD WS-ART-SUINKORD(ART-IX) TO WS-FSUM-SUINKORD(FSUM-IX)        
246800          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-FSUM-SUFYSAVP(FSUM-IX)        
246900          ADD WS-ART-SUAVBRP (ART-IX) TO WS-FSUM-SUAVBRP (FSUM-IX)        
247000          ADD WS-ART-SULAGERB(ART-IX) TO WS-FSUM-SULAGERB(FSUM-IX)        
247100          ADD WS-ART-SUSORTB (ART-IX) TO WS-FSUM-SUSORTB (FSUM-IX)        
247200                                                                          
247300          ADD +8 TO ART-IX                                                
247400        END-PERFORM                                                       
247500        ADD +1 TO FSUM-IX                                                 
247600                                                                          
247700        EVALUATE TRUE                                                     
247800           WHEN  FSUM-IX = 1                                              
247900                 MOVE +1 TO ART-IX                                        
248000           WHEN  FSUM-IX = 2                                              
248100                 MOVE +2 TO ART-IX                                        
248200                 MOVE +66 TO ART-IX-MAX                                   
248300           WHEN  FSUM-IX = 3                                              
248400                 MOVE +3 TO ART-IX                                        
248500                 MOVE +67 TO ART-IX-MAX                                   
248600           WHEN  FSUM-IX = 4                                              
248700                 MOVE +4 TO ART-IX                                        
248800                 MOVE +68 TO ART-IX-MAX                                   
248900           WHEN  FSUM-IX = 5                                              
249000                 MOVE +5 TO ART-IX                                        
249100                 MOVE +69 TO ART-IX-MAX                                   
249200           WHEN  FSUM-IX = 6                                              
249300                 MOVE +6 TO ART-IX                                        
249400                 MOVE +70 TO ART-IX-MAX                                   
249500           WHEN  FSUM-IX = 7                                              
249600                 MOVE +7 TO ART-IX                                        
249700                 MOVE +71 TO ART-IX-MAX                                   
249800           WHEN  FSUM-IX = 8                                              
249900                 MOVE +8 TO ART-IX                                        
250000                 MOVE +72 TO ART-IX-MAX                                   
250100           WHEN OTHER                                                     
250200                CONTINUE                                                  
250300        END-EVALUATE                                                      
250400                                                                          
250500     END-PERFORM                                                          
250600                                                                          
250700     MOVE +1 TO FSUM-IX                                                   
250800     MOVE +8 TO FSUM-IX-MAX                                               
250900     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
251000                                                                          
251100*********  DISP LAGER VÄRDE/FREKVENSKLASS                                 
251200        IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) = ZERO                          
251300           CONTINUE                                                       
251400        ELSE                                                              
251500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
251600                     WS-FSUM-KVDISP-PR-AKT(FSUM-IX) / 1000                
251700           MOVE WS-SUMMA-KR TO FSUM-KVDISP-AKT(FSUM-IX)                   
251800        END-IF                                                            
251900                                                                          
252000        IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) = ZERO                          
252100           CONTINUE                                                       
252200        ELSE                                                              
252300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
252400                     WS-FSUM-KVDISP-PR-PAS(FSUM-IX) / 1000                
252500           MOVE WS-SUMMA-KR TO FSUM-KVDISP-PAS(FSUM-IX)                   
252600        END-IF                                                            
252700                                                                          
252800*********  LAGERVÄRDE/FREKVENSKLASS                                       
252900        IF WS-FSUM-LS-PR-AKT(FSUM-IX) = ZERO                              
253000           CONTINUE                                                       
253100        ELSE                                                              
253200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
253300                     WS-FSUM-LS-PR-AKT(FSUM-IX) / 1000                    
253400           MOVE WS-SUMMA-KR TO FSUM-LS-AKT(FSUM-IX)                       
253500        END-IF                                                            
253600                                                                          
253700        IF WS-FSUM-LS-PR-PAS(FSUM-IX) = ZERO                              
253800           CONTINUE                                                       
253900        ELSE                                                              
254000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
254100                     WS-FSUM-LS-PR-PAS(FSUM-IX) / 1000                    
254200           MOVE WS-SUMMA-KR TO FSUM-LS-PAS(FSUM-IX)                       
254300        END-IF                                                            
254400                                                                          
254500*********  AK-VÄRDE/FREKVENSKLASS                                         
254600        IF WS-FSUM-AK-PR-AKT(FSUM-IX) = ZERO                              
254700           CONTINUE                                                       
254800        ELSE                                                              
254900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
255000                     WS-FSUM-AK-PR-AKT(FSUM-IX) / 1000                    
255100           MOVE WS-SUMMA-KR TO FSUM-AK-AKT(FSUM-IX)                       
255200        END-IF                                                            
255300                                                                          
255400        IF WS-FSUM-AK-PR-PAS(FSUM-IX) = ZERO                              
255500           CONTINUE                                                       
255600        ELSE                                                              
255700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
255800                     WS-FSUM-AK-PR-PAS(FSUM-IX) / 1000                    
255900           MOVE WS-SUMMA-KR TO FSUM-AK-PAS(FSUM-IX)                       
256000        END-IF                                                            
256100                                                                          
256200*********  SÄKERHETSLAGER/FREKVENSKLASS                                   
256300        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
256400           CONTINUE                                                       
256500        ELSE                                                              
256600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
256700                          WS-FSUM-SLAGER(FSUM-IX) / 1000                  
256800           MOVE WS-SUMMA-KR TO FSUM-SLAGER(FSUM-IX)                       
256900        END-IF                                                            
257000                                                                          
257100*********  ÖVERLAGER/FREKVENSKLASS                                        
257200        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
257300           CONTINUE                                                       
257400        ELSE                                                              
257500           COMPUTE WS-SUMMA-KR ROUNDED                                    
257600                     = WS-FSUM-OLAGER(FSUM-IX) / 1000                     
257700           MOVE WS-SUMMA-KR TO FSUM-OLAGER(FSUM-IX)                       
257800        END-IF                                                            
257900                                                                          
258000*********  MEDELLAGER/FREKVENSKLASS                                       
258100        IF WS-FSUM-MLAGER(FSUM-IX) = ZERO                                 
258200           CONTINUE                                                       
258300        ELSE                                                              
258400           COMPUTE WS-SUMMA-KR =                                          
258500                       WS-FSUM-MLAGER(FSUM-IX) / 1000                     
258600           ADD WS-SUMMA-KR TO FSUM-MLAGER(FSUM-IX)                        
258700        END-IF                                                            
258800                                                                          
258900*********  SERVICEGRAD BRUTTO/FREKVENSKLASS                               
259000        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
259100           MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                           
259200        ELSE                                                              
259300           COMPUTE WS-SERVG ROUNDED =                                     
259400             WS-FSUM-SUAVBRP(FSUM-IX) * 100 /                             
259500                          WS-FSUM-SUINKORD(FSUM-IX)                       
259600           IF WS-SERVG = 100.0                                            
259700              MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                        
259800           ELSE                                                           
259900              MOVE WS-SERVG TO FSUM-SERVG-BTO(FSUM-IX)                    
260000           END-IF                                                         
260100        END-IF                                                            
260200                                                                          
260300*********  SERVICEGRAD NETTO/PRISKLASS                                    
260400        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
260500           MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                           
260600        ELSE                                                              
260700           COMPUTE WS-SERVG ROUNDED =                                     
260800            (WS-FSUM-SUAVBRP(FSUM-IX) - WS-FSUM-SUFYSAVP(FSUM-IX))        
260900                           * 100 /                                        
261000                          WS-FSUM-SUINKORD(FSUM-IX)                       
261100           IF WS-SERVG = 100.0                                            
261200              MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                        
261300           ELSE                                                           
261400              MOVE WS-SERVG TO FSUM-SERVG-NTO(FSUM-IX)                    
261500           END-IF                                                         
261600        END-IF                                                            
261700                                                                          
261800*********  SPLITFAKTOR/FREKVENSKLASS                                      
261900        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
262000           MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                               
262100        ELSE                                                              
262200           COMPUTE WS-SERVG ROUNDED = (WS-FSUM-SUINKORD(FSUM-IX)          
262300                       + WS-FSUM-SULAGERB (FSUM-IX)) * 100                
262400                        / (WS-FSUM-SUINKORD(FSUM-IX) +                    
262500                           WS-FSUM-SULAGERB(FSUM-IX) +                    
262600                           WS-FSUM-SUSORTB(FSUM-IX))                      
262700           IF WS-SERVG = 100.0                                            
262800              MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                            
262900           ELSE                                                           
263000              MOVE WS-SERVG TO FSUM-SPLIT(FSUM-IX)                        
263100           END-IF                                                         
263200        END-IF                                                            
263300                                                                          
263400*********  OMSHASTIGHET/FREKVENSKLASS                                     
263500        COMPUTE WS-SUMMA = WS-FSUM-KVDISP-PR-AKT(FSUM-IX) +               
263600                           WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                 
263700        IF WS-SUMMA = ZERO                                                
263800           CONTINUE                                                       
263900        ELSE                                                              
264000           COMPUTE WS-OMSHAST ROUNDED =                                   
264100               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
264200           MOVE WS-OMSHAST TO FSUM-OMSHAST-DISP(FSUM-IX)                  
264300        END-IF                                                            
264400                                                                          
264500        COMPUTE WS-SUMMA = WS-FSUM-LS-PR-AKT(FSUM-IX) +                   
264600                           WS-FSUM-AK-PR-AKT(FSUM-IX) +                   
264700                           WS-FSUM-LS-PR-PAS(FSUM-IX) +                   
264800                           WS-FSUM-AK-PR-PAS(FSUM-IX)                     
264900        IF WS-SUMMA = ZERO                                                
265000           CONTINUE                                                       
265100        ELSE                                                              
265200           COMPUTE WS-OMSHAST ROUNDED =                                   
265300               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
265400           MOVE WS-OMSHAST TO FSUM-OMSHAST-LS(FSUM-IX)                    
265500        END-IF                                                            
265600                                                                          
265700        ADD +1 TO FSUM-IX                                                 
265800                                                                          
265900     END-PERFORM                                                          
266000     .                                                                    
266100     EJECT                                                                
266200 BDD-SUMMERA-TOTAL SECTION.                                               
266300******************************************************************        
266400* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
266500******************************************************************        
266600                                                                          
266700     MOVE +1 TO PSUM-IX                                                   
266800     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
266900        ADD PSUM-KVANT-AKT(PSUM-IX) TO TOT-KVANT-AKT                      
267000        ADD PSUM-KVANT-PAS(PSUM-IX) TO TOT-KVANT-PAS                      
267100        ADD PSUM-KVOT(PSUM-IX) TO TOT-KVOT                                
267200        ADD WS-PSUM-KVOI-AKT(PSUM-IX) TO WS-TOT-KVOI-AKT                  
267300        ADD WS-PSUM-KVOI-PAS(PSUM-IX) TO WS-TOT-KVOI-PAS                  
267400        ADD WS-PSUM-KVOI-TEO(PSUM-IX) TO WS-TOT-KVOI-TEO                  
267500        ADD WS-PSUM-KVOI-SAK(PSUM-IX) TO WS-TOT-KVOI-SAK                  
267600        ADD WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                                 
267700                                TO WS-TOT-KVOI-CDC-AKT                    
267800        ADD WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                                 
267900                                TO WS-TOT-KVOI-CDC-PAS                    
268000        ADD WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                                 
268100                                TO WS-TOT-KVOI-CDC-TEO                    
268200        ADD WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                                 
268300                                TO WS-TOT-KVOI-CDC-SAK                    
268400        ADD WS-PSUM-KVOI(PSUM-IX) TO WS-TOT-KVOI                          
268500        ADD WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                                
268600                           TO WS-TOT-KVDISP-PR-AKT                        
268700        ADD WS-PSUM-OK-PR-AKT(PSUM-IX)                                    
268800                           TO WS-TOT-OK-PR-AKT                            
268900        ADD WS-PSUM-LS-PR-AKT(PSUM-IX)                                    
269000                           TO WS-TOT-LS-PR-AKT                            
269100        ADD WS-PSUM-AK-PR-AKT(PSUM-IX)                                    
269200                           TO WS-TOT-AK-PR-AKT                            
269300        ADD WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                                
269400                           TO WS-TOT-KVDISP-PR-PAS                        
269500        ADD WS-PSUM-OK-PR-PAS(PSUM-IX)                                    
269600                           TO WS-TOT-OK-PR-PAS                            
269700        ADD WS-PSUM-LS-PR-PAS(PSUM-IX)                                    
269800                           TO WS-TOT-LS-PR-PAS                            
269900        ADD WS-PSUM-AK-PR-PAS(PSUM-IX)                                    
270000                           TO WS-TOT-AK-PR-PAS                            
270100        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
270200        ADD WS-PSUM-MLAGER(PSUM-IX) TO WS-TOT-MLAGER                      
270300        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
270400        ADD WS-PSUM-SUINKORD(PSUM-IX) TO WS-TOT-SUINKORD                  
270500        ADD WS-PSUM-SUFYSAVP(PSUM-IX) TO WS-TOT-SUFYSAVP                  
270600        ADD WS-PSUM-SUAVBRP (PSUM-IX) TO WS-TOT-SUAVBRP                   
270700        ADD WS-PSUM-SULAGERB(PSUM-IX) TO WS-TOT-SULAGERB                  
270800        ADD WS-PSUM-SUSORTB (PSUM-IX) TO WS-TOT-SUSORTB                   
270900                                                                          
271000        ADD +1 TO PSUM-IX                                                 
271100     END-PERFORM                                                          
271200                                                                          
271300*********  DISP LAGER VÄRDE TOTALT                                        
271400        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
271500           CONTINUE                                                       
271600        ELSE                                                              
271700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
271800                     WS-TOT-KVDISP-PR-AKT / 1000                          
271900           MOVE WS-SUMMA-KR TO TOT-KVDISP-AKT                             
272000        END-IF                                                            
272100                                                                          
272200        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
272300           CONTINUE                                                       
272400        ELSE                                                              
272500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
272600                     WS-TOT-KVDISP-PR-PAS / 1000                          
272700           MOVE WS-SUMMA-KR TO TOT-KVDISP-PAS                             
272800        END-IF                                                            
272900                                                                          
273000*********  LAGERVÄRDE TOTALT                                              
273100        IF WS-TOT-LS-PR-AKT = ZERO                                        
273200           CONTINUE                                                       
273300        ELSE                                                              
273400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
273500                     WS-TOT-LS-PR-AKT / 1000                              
273600           MOVE WS-SUMMA-KR TO TOT-LS-AKT                                 
273700        END-IF                                                            
273800                                                                          
273900        IF WS-TOT-LS-PR-PAS = ZERO                                        
274000           CONTINUE                                                       
274100        ELSE                                                              
274200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
274300                     WS-TOT-LS-PR-PAS / 1000                              
274400           MOVE WS-SUMMA-KR TO TOT-LS-PAS                                 
274500        END-IF                                                            
274600                                                                          
274700*********  AK-VÄRDE TOTALT                                                
274800        IF WS-TOT-AK-PR-AKT = ZERO                                        
274900           CONTINUE                                                       
275000        ELSE                                                              
275100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
275200                     WS-TOT-AK-PR-AKT / 1000                              
275300           MOVE WS-SUMMA-KR TO TOT-AK-AKT                                 
275400        END-IF                                                            
275500                                                                          
275600        IF WS-TOT-AK-PR-PAS = ZERO                                        
275700           CONTINUE                                                       
275800        ELSE                                                              
275900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
276000                     WS-TOT-AK-PR-PAS / 1000                              
276100           MOVE WS-SUMMA-KR TO TOT-AK-PAS                                 
276200        END-IF                                                            
276300                                                                          
276400*********  SÄKERHETSLAGER TOTALT                                          
276500        IF WS-TOT-SLAGER = ZERO                                           
276600           CONTINUE                                                       
276700        ELSE                                                              
276800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
276900                          WS-TOT-SLAGER / 1000                            
277000           MOVE WS-SUMMA-KR TO TOT-SLAGER                                 
277100        END-IF                                                            
277200                                                                          
277300*********  ÖVERLAGER TOTALT                                               
277400        IF WS-TOT-OLAGER = ZERO                                           
277500           CONTINUE                                                       
277600        ELSE                                                              
277700           COMPUTE WS-SUMMA-KR ROUNDED                                    
277800                     = WS-TOT-OLAGER / 1000                               
277900           MOVE WS-SUMMA-KR TO TOT-OLAGER                                 
278000        END-IF                                                            
278100                                                                          
278200*********  MEDELLAGER TOTALT                                              
278300        IF WS-TOT-MLAGER = ZERO                                           
278400           CONTINUE                                                       
278500        ELSE                                                              
278600           COMPUTE WS-SUMMA-KR =                                          
278700                       WS-TOT-MLAGER / 1000                               
278800           ADD WS-SUMMA-KR TO TOT-MLAGER                                  
278900        END-IF                                                            
279000                                                                          
279100*********  SERVICEGRAD BRUTTO TOTALT                                      
279200        IF WS-TOT-SUINKORD = ZERO                                         
279300           MOVE 99.9 TO TOT-SERVG-BTO                                     
279400        ELSE                                                              
279500           COMPUTE WS-SERVG ROUNDED =                                     
279600             WS-TOT-SUAVBRP * 100 /                                       
279700                          WS-TOT-SUINKORD                                 
279800           IF WS-SERVG = 100.0                                            
279900              MOVE 99.9 TO TOT-SERVG-BTO                                  
280000           ELSE                                                           
280100              MOVE WS-SERVG TO TOT-SERVG-BTO                              
280200           END-IF                                                         
280300        END-IF                                                            
280400                                                                          
280500*********  SERVICEGRAD NETTO TOTALT                                       
280600        IF WS-TOT-SUINKORD = ZERO                                         
280700           MOVE 99.9 TO TOT-SERVG-BTO                                     
280800        ELSE                                                              
280900           COMPUTE WS-SERVG ROUNDED =                                     
281000            (WS-TOT-SUAVBRP - WS-TOT-SUFYSAVP)                            
281100                           * 100 /                                        
281200                          WS-TOT-SUINKORD                                 
281300           IF WS-SERVG = 100.0                                            
281400              MOVE 99.9 TO TOT-SERVG-NTO                                  
281500           ELSE                                                           
281600              MOVE WS-SERVG TO TOT-SERVG-NTO                              
281700           END-IF                                                         
281800        END-IF                                                            
281900                                                                          
282000*********  SPLITFAKTOR TOTALT                                             
282100        IF WS-TOT-SUINKORD = ZERO                                         
282200           MOVE 99.9 TO TOT-SPLIT                                         
282300        ELSE                                                              
282400           COMPUTE WS-SERVG ROUNDED = (WS-TOT-SUINKORD                    
282500                       + WS-TOT-SULAGERB) * 100                           
282600                        / (WS-TOT-SUINKORD +                              
282700                           WS-TOT-SULAGERB +                              
282800                           WS-TOT-SUSORTB)                                
282900           IF WS-SERVG = 100.0                                            
283000              MOVE 99.9 TO TOT-SPLIT                                      
283100           ELSE                                                           
283200              MOVE WS-SERVG TO TOT-SPLIT                                  
283300           END-IF                                                         
283400        END-IF                                                            
283500                                                                          
283600*********  OMSHASTIGHET TOTALT                                            
283700        COMPUTE WS-SUMMA = WS-TOT-KVDISP-PR-AKT +                         
283800                           WS-TOT-KVDISP-PR-PAS                           
283900        IF WS-SUMMA = ZERO                                                
284000           CONTINUE                                                       
284100        ELSE                                                              
284200           COMPUTE WS-OMSHAST ROUNDED =                                   
284300               WS-TOT-KVOI / WS-SUMMA                                     
284400           MOVE WS-OMSHAST TO TOT-OMSHAST-DISP                            
284500        END-IF                                                            
284600                                                                          
284700        COMPUTE WS-SUMMA = WS-TOT-LS-PR-AKT +                             
284800                           WS-TOT-AK-PR-AKT +                             
284900                           WS-TOT-LS-PR-PAS +                             
285000                           WS-TOT-AK-PR-PAS                               
285100        IF WS-SUMMA = ZERO                                                
285200           CONTINUE                                                       
285300        ELSE                                                              
285400           COMPUTE WS-OMSHAST ROUNDED =                                   
285500               WS-TOT-KVOI / WS-SUMMA                                     
285600           MOVE WS-OMSHAST TO TOT-OMSHAST-LS                              
285700                                                                          
285800        END-IF                                                            
285900*********  ORDERTRÄFFAR TOTALT                                            
286000                                                                          
286100        ADD WS-KVOT-SAKNAS-WDK7 TO TOT-KVOT                               
286200     .                                                                    
286300     EJECT                                                                
286400 BDE-BERAKNINGAR-AV-TOTAL SECTION.                                        
286500******************************************************************        
286600* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
286700******************************************************************        
286800                                                                          
286900     MOVE +1 TO ART-IX                                                    
287000     MOVE +72 TO ART-IX-MAX                                               
287100     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
287200                                                                          
287300******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
287400        IF TOT-KVANT-AKT = ZERO                                           
287500           CONTINUE                                                       
287600        ELSE                                                              
287700           COMPUTE WS-PROC = ART-KVANT-AKT(ART-IX)                        
287800                                    * 100 / TOT-KVANT-AKT                 
287900           MOVE WS-PROC TO ART-PROC-KVANT-A(ART-IX)                       
288000        END-IF                                                            
288100                                                                          
288200******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
288300        IF TOT-KVANT-PAS = ZERO                                           
288400           CONTINUE                                                       
288500        ELSE                                                              
288600           COMPUTE WS-PROC = ART-KVANT-PAS(ART-IX)                        
288700                                    * 100 / TOT-KVANT-PAS                 
288800           MOVE WS-PROC TO ART-PROC-KVANT-P(ART-IX)                       
288900        END-IF                                                            
289000                                                                          
289100******** % ANTAL ORDERTRÄFFAR AV TOTAL                                    
289200        IF TOT-KVOT = ZERO                                                
289300           CONTINUE                                                       
289400        ELSE                                                              
289500           COMPUTE WS-PROC = ART-KVOT(ART-IX)                             
289600                                    * 100 / TOT-KVOT                      
289700           MOVE WS-PROC TO ART-PROC-KVOT(ART-IX)                          
289800        END-IF                                                            
289900                                                                          
290000*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  AKTIVA                      
290100                                                                          
290200        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
290300           CONTINUE                                                       
290400        ELSE                                                              
290500           IF WS-ART-KVDISP-PR-AKT(ART-IX) > ZERO                         
290600              COMPUTE WS-PROC = WS-ART-KVDISP-PR-AKT(ART-IX)              
290700                              * 100 / WS-TOT-KVDISP-PR-AKT                
290800              MOVE WS-PROC TO ART-PROC-KVDISP-A(ART-IX)                   
290900           END-IF                                                         
291000        END-IF                                                            
291100                                                                          
291200*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  PASSIVA                     
291300                                                                          
291400        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
291500           CONTINUE                                                       
291600        ELSE                                                              
291700           IF WS-ART-KVDISP-PR-PAS(ART-IX) > ZERO                         
291800              COMPUTE WS-PROC = WS-ART-KVDISP-PR-PAS(ART-IX)              
291900                              * 100 / WS-TOT-KVDISP-PR-PAS                
292000              MOVE WS-PROC TO ART-PROC-KVDISP-P(ART-IX)                   
292100           END-IF                                                         
292200        END-IF                                                            
292300                                                                          
292400*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                      
292500                                                                          
292600        IF WS-TOT-LS-PR-AKT = ZERO                                        
292700           CONTINUE                                                       
292800        ELSE                                                              
292900           IF WS-ART-LS-PR-AKT(ART-IX) > ZERO                             
293000              COMPUTE WS-PROC = WS-ART-LS-PR-AKT(ART-IX)                  
293100                              * 100 / WS-TOT-LS-PR-AKT                    
293200              MOVE WS-PROC TO ART-PROC-LS-A(ART-IX)                       
293300           END-IF                                                         
293400        END-IF                                                            
293500                                                                          
293600*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA                     
293700                                                                          
293800        IF WS-TOT-LS-PR-PAS = ZERO                                        
293900           CONTINUE                                                       
294000        ELSE                                                              
294100           IF WS-ART-LS-PR-PAS(ART-IX) > ZERO                             
294200              COMPUTE WS-PROC = WS-ART-LS-PR-PAS(ART-IX)                  
294300                              * 100 / WS-TOT-LS-PR-PAS                    
294400              MOVE WS-PROC TO ART-PROC-LS-P(ART-IX)                       
294500           END-IF                                                         
294600        END-IF                                                            
294700                                                                          
294800*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                          
294900                                                                          
295000        IF WS-TOT-AK-PR-AKT = ZERO                                        
295100           CONTINUE                                                       
295200        ELSE                                                              
295300           IF WS-ART-AK-PR-AKT(ART-IX) > ZERO                             
295400              COMPUTE WS-PROC = WS-ART-AK-PR-AKT(ART-IX)                  
295500                              * 100 / WS-TOT-AK-PR-AKT                    
295600              MOVE WS-PROC TO ART-PROC-AK-A(ART-IX)                       
295700           END-IF                                                         
295800        END-IF                                                            
295900                                                                          
296000*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                         
296100                                                                          
296200        IF WS-TOT-AK-PR-PAS = ZERO                                        
296300           CONTINUE                                                       
296400        ELSE                                                              
296500           IF WS-ART-AK-PR-PAS(ART-IX) > ZERO                             
296600              COMPUTE WS-PROC = WS-ART-AK-PR-PAS(ART-IX)                  
296700                              * 100 / WS-TOT-AK-PR-PAS                    
296800              MOVE WS-PROC TO ART-PROC-AK-P(ART-IX)                       
296900           END-IF                                                         
297000        END-IF                                                            
297100                                                                          
297200        ADD +1 TO ART-IX                                                  
297300     END-PERFORM                                                          
297400                                                                          
297500****************************                                              
297600                                                                          
297700     MOVE +1 TO PSUM-IX                                                   
297800     MOVE +9 TO PSUM-IX-MAX                                               
297900     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
298000                                                                          
298100******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
298200        IF TOT-KVANT-AKT = ZERO                                           
298300           CONTINUE                                                       
298400        ELSE                                                              
298500           COMPUTE WS-PROC = PSUM-KVANT-AKT(PSUM-IX) * 100 /              
298600                           TOT-KVANT-AKT                                  
298700           MOVE WS-PROC TO PSUM-PROC-KVANT-A(PSUM-IX)                     
298800        END-IF                                                            
298900                                                                          
299000******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
299100        IF TOT-KVANT-PAS = ZERO                                           
299200           CONTINUE                                                       
299300        ELSE                                                              
299400           COMPUTE WS-PROC = PSUM-KVANT-PAS(PSUM-IX) * 100 /              
299500                           TOT-KVANT-PAS                                  
299600           MOVE WS-PROC TO PSUM-PROC-KVANT-P(PSUM-IX)                     
299700        END-IF                                                            
299800                                                                          
299900******** % ANTAL ORDERTRÄFFAR AV TOTALA                                   
300000        IF TOT-KVOT = ZERO                                                
300100           CONTINUE                                                       
300200        ELSE                                                              
300300           COMPUTE WS-PROC = PSUM-KVOT(PSUM-IX) * 100 /                   
300400                           TOT-KVOT                                       
300500           MOVE WS-PROC TO PSUM-PROC-KVOT(PSUM-IX)                        
300600        END-IF                                                            
300700                                                                          
300800*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA                
300900                                                                          
301000        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
301100           CONTINUE                                                       
301200        ELSE                                                              
301300           IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) > ZERO                       
301400              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-AKT(PSUM-IX)            
301500                              * 100 / WS-TOT-KVDISP-PR-AKT                
301600              MOVE WS-PROC TO PSUM-PROC-KVDISP-A(PSUM-IX)                 
301700           END-IF                                                         
301800        END-IF                                                            
301900                                                                          
302000*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA               
302100                                                                          
302200        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
302300           CONTINUE                                                       
302400        ELSE                                                              
302500           IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) > ZERO                       
302600              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
302700                              * 100 / WS-TOT-KVDISP-PR-PAS                
302800              MOVE WS-PROC TO PSUM-PROC-KVDISP-P(PSUM-IX)                 
302900           END-IF                                                         
303000        END-IF                                                            
303100                                                                          
303200*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                
303300                                                                          
303400        IF WS-TOT-LS-PR-AKT = ZERO                                        
303500           CONTINUE                                                       
303600        ELSE                                                              
303700           IF WS-PSUM-LS-PR-AKT(PSUM-IX) > ZERO                           
303800              COMPUTE WS-PROC = WS-PSUM-LS-PR-AKT(PSUM-IX)                
303900                              * 100 / WS-TOT-LS-PR-AKT                    
304000              MOVE WS-PROC TO PSUM-PROC-LS-A(PSUM-IX)                     
304100           END-IF                                                         
304200        END-IF                                                            
304300                                                                          
304400*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA               
304500                                                                          
304600        IF WS-TOT-LS-PR-PAS = ZERO                                        
304700           CONTINUE                                                       
304800        ELSE                                                              
304900           IF WS-PSUM-LS-PR-PAS(PSUM-IX) > ZERO                           
305000              COMPUTE WS-PROC = WS-PSUM-LS-PR-PAS(PSUM-IX)                
305100                              * 100 / WS-TOT-LS-PR-PAS                    
305200              MOVE WS-PROC TO PSUM-PROC-LS-P(PSUM-IX)                     
305300           END-IF                                                         
305400        END-IF                                                            
305500                                                                          
305600*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                    
305700                                                                          
305800        IF WS-TOT-AK-PR-AKT = ZERO                                        
305900           CONTINUE                                                       
306000        ELSE                                                              
306100           IF WS-PSUM-AK-PR-AKT(PSUM-IX) > ZERO                           
306200              COMPUTE WS-PROC = WS-PSUM-AK-PR-AKT(PSUM-IX)                
306300                              * 100 / WS-TOT-AK-PR-AKT                    
306400              MOVE WS-PROC TO PSUM-PROC-AK-A(PSUM-IX)                     
306500           END-IF                                                         
306600        END-IF                                                            
306700                                                                          
306800*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                   
306900                                                                          
307000        IF WS-TOT-AK-PR-PAS = ZERO                                        
307100           CONTINUE                                                       
307200        ELSE                                                              
307300           IF WS-PSUM-AK-PR-PAS(PSUM-IX) > ZERO                           
307400              COMPUTE WS-PROC = WS-PSUM-AK-PR-PAS(PSUM-IX)                
307500                              * 100 / WS-TOT-AK-PR-PAS                    
307600              MOVE WS-PROC TO PSUM-PROC-AK-P(PSUM-IX)                     
307700           END-IF                                                         
307800        END-IF                                                            
307900                                                                          
308000        ADD +1 TO PSUM-IX                                                 
308100     END-PERFORM                                                          
308200                                                                          
308300****************************************                                  
308400                                                                          
308500     MOVE +1 TO FSUM-IX                                                   
308600     MOVE +8 TO FSUM-IX-MAX                                               
308700     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
308800                                                                          
308900******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
309000        IF TOT-KVANT-AKT = ZERO                                           
309100           CONTINUE                                                       
309200        ELSE                                                              
309300           COMPUTE WS-PROC = FSUM-KVANT-AKT(FSUM-IX) * 100 /              
309400                           TOT-KVANT-AKT                                  
309500           MOVE WS-PROC TO FSUM-PROC-KVANT-A(FSUM-IX)                     
309600        END-IF                                                            
309700                                                                          
309800******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
309900        IF TOT-KVANT-PAS = ZERO                                           
310000           CONTINUE                                                       
310100        ELSE                                                              
310200           COMPUTE WS-PROC = FSUM-KVANT-PAS(FSUM-IX) * 100 /              
310300                           TOT-KVANT-PAS                                  
310400           MOVE WS-PROC TO FSUM-PROC-KVANT-P(FSUM-IX)                     
310500        END-IF                                                            
310600                                                                          
310700******** % ANTAL ORDERTRÄFFAR TOTALA                                      
310800        IF TOT-KVOT = ZERO                                                
310900           CONTINUE                                                       
311000        ELSE                                                              
311100           COMPUTE WS-PROC = FSUM-KVOT(FSUM-IX) * 100 /                   
311200                           TOT-KVOT                                       
311300           MOVE WS-PROC TO FSUM-PROC-KVOT(FSUM-IX)                        
311400        END-IF                                                            
311500                                                                          
311600*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA            
311700                                                                          
311800        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
311900           CONTINUE                                                       
312000        ELSE                                                              
312100           IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) > ZERO                       
312200              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-AKT(FSUM-IX)            
312300                              * 100 / WS-TOT-KVDISP-PR-AKT                
312400              MOVE WS-PROC TO FSUM-PROC-KVDISP-A(FSUM-IX)                 
312500           END-IF                                                         
312600        END-IF                                                            
312700                                                                          
312800*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA           
312900                                                                          
313000        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
313100           CONTINUE                                                       
313200        ELSE                                                              
313300           IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) > ZERO                       
313400              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
313500                              * 100 / WS-TOT-KVDISP-PR-PAS                
313600              MOVE WS-PROC TO FSUM-PROC-KVDISP-P(FSUM-IX)                 
313700           END-IF                                                         
313800        END-IF                                                            
313900                                                                          
314000*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA            
314100                                                                          
314200        IF WS-TOT-LS-PR-AKT = ZERO                                        
314300           CONTINUE                                                       
314400        ELSE                                                              
314500           IF WS-FSUM-LS-PR-AKT(FSUM-IX) > ZERO                           
314600              COMPUTE WS-PROC = WS-FSUM-LS-PR-AKT(FSUM-IX)                
314700                              * 100 / WS-TOT-LS-PR-AKT                    
314800              MOVE WS-PROC TO FSUM-PROC-LS-A(FSUM-IX)                     
314900           END-IF                                                         
315000        END-IF                                                            
315100                                                                          
315200*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA           
315300                                                                          
315400        IF WS-TOT-LS-PR-PAS = ZERO                                        
315500           CONTINUE                                                       
315600        ELSE                                                              
315700           IF WS-FSUM-LS-PR-PAS(FSUM-IX) > ZERO                           
315800              COMPUTE WS-PROC = WS-FSUM-LS-PR-PAS(FSUM-IX)                
315900                              * 100 / WS-TOT-LS-PR-PAS                    
316000              MOVE WS-PROC TO FSUM-PROC-LS-P(FSUM-IX)                     
316100           END-IF                                                         
316200        END-IF                                                            
316300                                                                          
316400*******  %  FREKVENSSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA               
316500                                                                          
316600        IF WS-TOT-AK-PR-AKT = ZERO                                        
316700           CONTINUE                                                       
316800        ELSE                                                              
316900           IF WS-FSUM-AK-PR-AKT(FSUM-IX) > ZERO                           
317000              COMPUTE WS-PROC = WS-FSUM-AK-PR-AKT(FSUM-IX)                
317100                              * 100 / WS-TOT-AK-PR-AKT                    
317200              MOVE WS-PROC TO FSUM-PROC-AK-A(FSUM-IX)                     
317300           END-IF                                                         
317400        END-IF                                                            
317500                                                                          
317600*******  %  FREKVENSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA               
317700                                                                          
317800        IF WS-TOT-AK-PR-PAS = ZERO                                        
317900           CONTINUE                                                       
318000        ELSE                                                              
318100           IF WS-FSUM-AK-PR-PAS(FSUM-IX) > ZERO                           
318200              COMPUTE WS-PROC = WS-FSUM-AK-PR-PAS(FSUM-IX)                
318300                              * 100 / WS-TOT-AK-PR-PAS                    
318400              MOVE WS-PROC TO FSUM-PROC-AK-P(FSUM-IX)                     
318500           END-IF                                                         
318600        END-IF                                                            
318700                                                                          
318800        ADD +1 TO FSUM-IX                                                 
318900     END-PERFORM                                                          
319000     .                                                                    
319100     EJECT                                                                
319200 C-SKRIV-LISTA SECTION.                                                   
319300******************************************************************        
319400*  SID 1 BESTÅR AV 3 RUTRADER INKL PRISKLASS-TOTAL               *        
319500*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
319600*      3           3 RUTRADER INKL PRISKLASS-TOTAL               *        
319700*      4           1 RUTRAD   FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
319800******************************************************************        
319900                                                                          
320000     MOVE +1 TO IX1                                                       
320100     MOVE +2 TO IX2                                                       
320200     MOVE +3 TO IX3                                                       
320300     MOVE +4 TO IX4                                                       
320400     MOVE +5 TO IX5                                                       
320500     MOVE +6 TO IX6                                                       
320600     MOVE +7 TO IX7                                                       
320700     MOVE +8 TO IX8                                                       
320800     MOVE +1 TO PSUM-IX                                                   
320900                                                                          
321000*********** SKRIVER SID-1                                                 
321100                                                                          
321200     DISPLAY '********* W001-RUBRIK1 ** ' W001-RUBRIK1                    
321300     PERFORM S21A-SKRIV-RUBRIKER                                          
321400     MOVE '1' TO W001-DET1-PRISKLASS                                      
321500     PERFORM CA-FLYTTA-SKRIV-RAD                                          
321600     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
321700     ADD +1 TO PSUM-IX                                                    
321800     MOVE '2' TO W001-DET1-PRISKLASS                                      
321900     PERFORM CA-FLYTTA-SKRIV-RAD                                          
322000                                                                          
322100     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
322200     ADD +1 TO PSUM-IX                                                    
322300     MOVE '3' TO W001-DET1-PRISKLASS                                      
322400     PERFORM CA-FLYTTA-SKRIV-RAD                                          
322500                                                                          
322600*********** SKRIVER SID-2                                                 
322700     PERFORM S21A-SKRIV-RUBRIKER                                          
322800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
322900     ADD +1 TO PSUM-IX                                                    
323000     MOVE '4' TO W001-DET1-PRISKLASS                                      
323100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
323200                                                                          
323300     ADD +1 TO PSUM-IX                                                    
323400     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
323500     MOVE '5' TO W001-DET1-PRISKLASS                                      
323600     PERFORM CA-FLYTTA-SKRIV-RAD                                          
323700                                                                          
323800     ADD +1 TO PSUM-IX                                                    
323900     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
324000     MOVE '6' TO W001-DET1-PRISKLASS                                      
324100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
324200                                                                          
324300*********** SKRIVER SID-3                                                 
324400     PERFORM S21A-SKRIV-RUBRIKER                                          
324500     ADD +1 TO PSUM-IX                                                    
324600     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
324700     MOVE '7' TO W001-DET1-PRISKLASS                                      
324800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
324900                                                                          
325000     ADD +1 TO PSUM-IX                                                    
325100     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
325200     MOVE '8' TO W001-DET1-PRISKLASS                                      
325300     PERFORM CA-FLYTTA-SKRIV-RAD                                          
325400                                                                          
325500     ADD +1 TO PSUM-IX                                                    
325600     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
325700     MOVE '9' TO W001-DET1-PRISKLASS                                      
325800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
325900                                                                          
326000*********** SKRIVER SID-4                                                 
326100     PERFORM S21A-SKRIV-RUBRIKER                                          
326200     MOVE +1 TO IX1                                                       
326300     MOVE +2 TO IX2                                                       
326400     MOVE +3 TO IX3                                                       
326500     MOVE +4 TO IX4                                                       
326600     MOVE +5 TO IX5                                                       
326700     MOVE +6 TO IX6                                                       
326800     MOVE +7 TO IX7                                                       
326900     MOVE +8 TO IX8                                                       
327000     MOVE SPACE TO W001-DET1-PRISKLASS                                    
327100     PERFORM CB-FLYTTA-SKRIV-TOT                                          
327200     .                                                                    
327300     EJECT                                                                
327400 CA-FLYTTA-SKRIV-RAD SECTION.                                             
327500                                                                          
327600     MOVE ART-KVANT-AKT(IX1)       TO  W001-DET1-KVANTA                   
327700     MOVE ART-PROC-KVANT-A(IX1)    TO  W001-DET1-P-KVANTA                 
327800     MOVE ART-KVANT-AKT(IX2)       TO  W001-DET1-KVANTB                   
327900     MOVE ART-PROC-KVANT-A(IX2)    TO  W001-DET1-P-KVANTB                 
328000     MOVE ART-KVANT-AKT(IX3)       TO  W001-DET1-KVANTC                   
328100     MOVE ART-PROC-KVANT-A(IX3)    TO  W001-DET1-P-KVANTC                 
328200     MOVE ART-KVANT-AKT(IX4)       TO  W001-DET1-KVANTD                   
328300     MOVE ART-PROC-KVANT-A(IX4)    TO  W001-DET1-P-KVANTD                 
328400     MOVE ART-KVANT-AKT(IX5)       TO  W001-DET1-KVANTE                   
328500     MOVE ART-PROC-KVANT-A(IX5)    TO  W001-DET1-P-KVANTE                 
328600     MOVE ART-KVANT-AKT(IX6)       TO  W001-DET1-KVANTF                   
328700     MOVE ART-PROC-KVANT-A(IX6)    TO  W001-DET1-P-KVANTF                 
328800     MOVE ART-KVANT-AKT(IX7)       TO  W001-DET1-KVANTG                   
328900     MOVE ART-PROC-KVANT-A(IX7)    TO  W001-DET1-P-KVANTG                 
329000     MOVE ART-KVANT-AKT(IX8)       TO  W001-DET1-KVANTH                   
329100     MOVE ART-PROC-KVANT-A(IX8)    TO  W001-DET1-P-KVANTH                 
329200     MOVE PSUM-KVANT-AKT(PSUM-IX)  TO  W001-DET1-TOT                      
329300     MOVE PSUM-PROC-KVANT-A(PSUM-IX) TO W001-DET1-P-TOT                   
329400     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
329500     MOVE +2 TO W001-SKIP                                                 
329600     PERFORM S21-SKRIV-LISTA                                              
329700                                                                          
329800     MOVE ART-KVANT-PAS(IX1)       TO  W001-DET2-KVANTA                   
329900     MOVE ART-PROC-KVANT-P(IX1)    TO  W001-DET2-P-KVANTA                 
330000     MOVE ART-KVANT-PAS(IX2)       TO  W001-DET2-KVANTB                   
330100     MOVE ART-PROC-KVANT-P(IX2)    TO  W001-DET2-P-KVANTB                 
330200     MOVE ART-KVANT-PAS(IX3)       TO  W001-DET2-KVANTC                   
330300     MOVE ART-PROC-KVANT-P(IX3)    TO  W001-DET2-P-KVANTC                 
330400     MOVE ART-KVANT-PAS(IX4)       TO  W001-DET2-KVANTD                   
330500     MOVE ART-PROC-KVANT-P(IX4)    TO  W001-DET2-P-KVANTD                 
330600     MOVE ART-KVANT-PAS(IX5)       TO  W001-DET2-KVANTE                   
330700     MOVE ART-PROC-KVANT-P(IX5)    TO  W001-DET2-P-KVANTE                 
330800     MOVE ART-KVANT-PAS(IX6)       TO  W001-DET2-KVANTF                   
330900     MOVE ART-PROC-KVANT-P(IX6)    TO  W001-DET2-P-KVANTF                 
331000     MOVE ART-KVANT-PAS(IX7)       TO  W001-DET2-KVANTG                   
331100     MOVE ART-PROC-KVANT-P(IX7)    TO  W001-DET2-P-KVANTG                 
331200     MOVE ART-KVANT-PAS(IX8)       TO  W001-DET2-KVANTH                   
331300     MOVE ART-PROC-KVANT-P(IX8)    TO  W001-DET2-P-KVANTH                 
331400     MOVE PSUM-KVANT-PAS(PSUM-IX)  TO  W001-DET2-TOT                      
331500     MOVE PSUM-PROC-KVANT-P(PSUM-IX) TO W001-DET2-P-TOT                   
331600     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
331700     MOVE +1 TO W001-SKIP                                                 
331800     PERFORM S21-SKRIV-LISTA                                              
331900                                                                          
332000     MOVE ART-KVDISP-AKT(IX1)      TO  W001-DET3-DLAGERA                  
332100     MOVE ART-PROC-KVDISP-A(IX1)   TO  W001-DET3-P-DLAGERA                
332200     MOVE ART-KVDISP-AKT(IX2)      TO  W001-DET3-DLAGERB                  
332300     MOVE ART-PROC-KVDISP-A(IX2)   TO  W001-DET3-P-DLAGERB                
332400     MOVE ART-KVDISP-AKT(IX3)      TO  W001-DET3-DLAGERC                  
332500     MOVE ART-PROC-KVDISP-A(IX3)   TO  W001-DET3-P-DLAGERC                
332600     MOVE ART-KVDISP-AKT(IX4)      TO  W001-DET3-DLAGERD                  
332700     MOVE ART-PROC-KVDISP-A(IX4)   TO  W001-DET3-P-DLAGERD                
332800     MOVE ART-KVDISP-AKT(IX5)      TO  W001-DET3-DLAGERE                  
332900     MOVE ART-PROC-KVDISP-A(IX5)   TO  W001-DET3-P-DLAGERE                
333000     MOVE ART-KVDISP-AKT(IX6)      TO  W001-DET3-DLAGERF                  
333100     MOVE ART-PROC-KVDISP-A(IX6)   TO  W001-DET3-P-DLAGERF                
333200     MOVE ART-KVDISP-AKT(IX7)      TO  W001-DET3-DLAGERG                  
333300     MOVE ART-PROC-KVDISP-A(IX7)   TO  W001-DET3-P-DLAGERG                
333400     MOVE ART-KVDISP-AKT(IX8)      TO  W001-DET3-DLAGERH                  
333500     MOVE ART-PROC-KVDISP-A(IX8)   TO  W001-DET3-P-DLAGERH                
333600     MOVE PSUM-KVDISP-AKT(PSUM-IX) TO  W001-DET3-TOT                      
333700     MOVE PSUM-PROC-KVDISP-A(PSUM-IX)                                     
333800                                   TO  W001-DET3-P-TOT                    
333900     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
334000     MOVE +1 TO W001-SKIP                                                 
334100     PERFORM S21-SKRIV-LISTA                                              
334200                                                                          
334300     MOVE ART-KVDISP-PAS(IX1)      TO  W001-DET4-DLAGERA                  
334400     MOVE ART-PROC-KVDISP-P(IX1)   TO  W001-DET4-P-DLAGERA                
334500     MOVE ART-KVDISP-PAS(IX2)      TO  W001-DET4-DLAGERB                  
334600     MOVE ART-PROC-KVDISP-P(IX2)   TO  W001-DET4-P-DLAGERB                
334700     MOVE ART-KVDISP-PAS(IX3)      TO  W001-DET4-DLAGERC                  
334800     MOVE ART-PROC-KVDISP-P(IX3)   TO  W001-DET4-P-DLAGERC                
334900     MOVE ART-KVDISP-PAS(IX4)      TO  W001-DET4-DLAGERD                  
335000     MOVE ART-PROC-KVDISP-P(IX4)   TO  W001-DET4-P-DLAGERD                
335100     MOVE ART-KVDISP-PAS(IX5)      TO  W001-DET4-DLAGERE                  
335200     MOVE ART-PROC-KVDISP-P(IX5)   TO  W001-DET4-P-DLAGERE                
335300     MOVE ART-KVDISP-PAS(IX6)      TO  W001-DET4-DLAGERF                  
335400     MOVE ART-PROC-KVDISP-P(IX6)   TO  W001-DET4-P-DLAGERF                
335500     MOVE ART-KVDISP-PAS(IX7)      TO  W001-DET4-DLAGERG                  
335600     MOVE ART-PROC-KVDISP-P(IX7)   TO  W001-DET4-P-DLAGERG                
335700     MOVE ART-KVDISP-PAS(IX8)      TO  W001-DET4-DLAGERH                  
335800     MOVE ART-PROC-KVDISP-P(IX8)   TO  W001-DET4-P-DLAGERH                
335900     MOVE PSUM-KVDISP-PAS(PSUM-IX) TO  W001-DET4-TOT                      
336000     MOVE PSUM-PROC-KVDISP-P(PSUM-IX)                                     
336100                                   TO  W001-DET4-P-TOT                    
336200     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
336300     MOVE +1 TO W001-SKIP                                                 
336400     PERFORM S21-SKRIV-LISTA                                              
336500                                                                          
336600     MOVE ART-LS-AKT(IX1)          TO  W001-DET5-LLAGERA                  
336700     MOVE ART-PROC-LS-A(IX1)       TO  W001-DET5-P-LLAGERA                
336800     MOVE ART-LS-AKT(IX2)          TO  W001-DET5-LLAGERB                  
336900     MOVE ART-PROC-LS-A(IX2)       TO  W001-DET5-P-LLAGERB                
337000     MOVE ART-LS-AKT(IX3)          TO  W001-DET5-LLAGERC                  
337100     MOVE ART-PROC-LS-A(IX3)       TO  W001-DET5-P-LLAGERC                
337200     MOVE ART-LS-AKT(IX4)          TO  W001-DET5-LLAGERD                  
337300     MOVE ART-PROC-LS-A(IX4)       TO  W001-DET5-P-LLAGERD                
337400     MOVE ART-LS-AKT(IX5)          TO  W001-DET5-LLAGERE                  
337500     MOVE ART-PROC-LS-A(IX5)       TO  W001-DET5-P-LLAGERE                
337600     MOVE ART-LS-AKT(IX6)          TO  W001-DET5-LLAGERF                  
337700     MOVE ART-PROC-LS-A(IX6)       TO  W001-DET5-P-LLAGERF                
337800     MOVE ART-LS-AKT(IX7)          TO  W001-DET5-LLAGERG                  
337900     MOVE ART-PROC-LS-A(IX7)       TO  W001-DET5-P-LLAGERG                
338000     MOVE ART-LS-AKT(IX8)          TO  W001-DET5-LLAGERH                  
338100     MOVE ART-PROC-LS-A(IX8)       TO  W001-DET5-P-LLAGERH                
338200     MOVE PSUM-LS-AKT(PSUM-IX)     TO  W001-DET5-TOT                      
338300     MOVE PSUM-PROC-LS-A(PSUM-IX)  TO  W001-DET5-P-TOT                    
338400     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
338500     MOVE +1 TO W001-SKIP                                                 
338600     PERFORM S21-SKRIV-LISTA                                              
338700                                                                          
338800     MOVE ART-LS-PAS(IX1)          TO  W001-DET6-LLAGERA                  
338900     MOVE ART-PROC-LS-P(IX1)       TO  W001-DET6-P-LLAGERA                
339000     MOVE ART-LS-PAS(IX2)          TO  W001-DET6-LLAGERB                  
339100     MOVE ART-PROC-LS-P(IX2)       TO  W001-DET6-P-LLAGERB                
339200     MOVE ART-LS-PAS(IX3)          TO  W001-DET6-LLAGERC                  
339300     MOVE ART-PROC-LS-P(IX3)       TO  W001-DET6-P-LLAGERC                
339400     MOVE ART-LS-PAS(IX4)          TO  W001-DET6-LLAGERD                  
339500     MOVE ART-PROC-LS-P(IX4)       TO  W001-DET6-P-LLAGERD                
339600     MOVE ART-LS-PAS(IX5)          TO  W001-DET6-LLAGERE                  
339700     MOVE ART-PROC-LS-P(IX5)       TO  W001-DET6-P-LLAGERE                
339800     MOVE ART-LS-PAS(IX6)          TO  W001-DET6-LLAGERF                  
339900     MOVE ART-PROC-LS-P(IX6)       TO  W001-DET6-P-LLAGERF                
340000     MOVE ART-LS-PAS(IX7)          TO  W001-DET6-LLAGERG                  
340100     MOVE ART-PROC-LS-P(IX7)       TO  W001-DET6-P-LLAGERG                
340200     MOVE ART-LS-PAS(IX8)          TO  W001-DET6-LLAGERH                  
340300     MOVE ART-PROC-LS-P(IX8)       TO  W001-DET6-P-LLAGERH                
340400     MOVE PSUM-LS-PAS(PSUM-IX)     TO  W001-DET6-TOT                      
340500     MOVE PSUM-PROC-LS-P(PSUM-IX)  TO  W001-DET6-P-TOT                    
340600     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
340700     MOVE +1 TO W001-SKIP                                                 
340800     PERFORM S21-SKRIV-LISTA                                              
340900                                                                          
341000     MOVE ART-AK-AKT(IX1)          TO  W001-DET7-ALAGERA                  
341100     MOVE ART-PROC-AK-A(IX1)       TO  W001-DET7-P-ALAGERA                
341200     MOVE ART-AK-AKT(IX2)          TO  W001-DET7-ALAGERB                  
341300     MOVE ART-PROC-AK-A(IX2)       TO  W001-DET7-P-ALAGERB                
341400     MOVE ART-AK-AKT(IX3)          TO  W001-DET7-ALAGERC                  
341500     MOVE ART-PROC-AK-A(IX3)       TO  W001-DET7-P-ALAGERC                
341600     MOVE ART-AK-AKT(IX4)          TO  W001-DET7-ALAGERD                  
341700     MOVE ART-PROC-AK-A(IX4)       TO  W001-DET7-P-ALAGERD                
341800     MOVE ART-AK-AKT(IX5)          TO  W001-DET7-ALAGERE                  
341900     MOVE ART-PROC-AK-A(IX5)       TO  W001-DET7-P-ALAGERE                
342000     MOVE ART-AK-AKT(IX6)          TO  W001-DET7-ALAGERF                  
342100     MOVE ART-PROC-AK-A(IX6)       TO  W001-DET7-P-ALAGERF                
342200     MOVE ART-AK-AKT(IX7)          TO  W001-DET7-ALAGERG                  
342300     MOVE ART-PROC-AK-A(IX7)       TO  W001-DET7-P-ALAGERG                
342400     MOVE ART-AK-AKT(IX8)          TO  W001-DET7-ALAGERH                  
342500     MOVE ART-PROC-AK-A(IX8)       TO  W001-DET7-P-ALAGERH                
342600     MOVE PSUM-AK-AKT(PSUM-IX)     TO  W001-DET7-TOT                      
342700     MOVE PSUM-PROC-AK-A(PSUM-IX)  TO  W001-DET7-P-TOT                    
342800     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
342900     MOVE +1 TO W001-SKIP                                                 
343000     PERFORM S21-SKRIV-LISTA                                              
343100                                                                          
343200     MOVE ART-AK-PAS(IX1)          TO  W001-DET8-ALAGERA                  
343300     MOVE ART-PROC-AK-P(IX1)       TO  W001-DET8-P-ALAGERA                
343400     MOVE ART-AK-PAS(IX2)          TO  W001-DET8-ALAGERB                  
343500     MOVE ART-PROC-AK-P(IX2)       TO  W001-DET8-P-ALAGERB                
343600     MOVE ART-AK-PAS(IX3)          TO  W001-DET8-ALAGERC                  
343700     MOVE ART-PROC-AK-P(IX3)       TO  W001-DET8-P-ALAGERC                
343800     MOVE ART-AK-PAS(IX4)          TO  W001-DET8-ALAGERD                  
343900     MOVE ART-PROC-AK-P(IX4)       TO  W001-DET8-P-ALAGERD                
344000     MOVE ART-AK-PAS(IX5)          TO  W001-DET8-ALAGERE                  
344100     MOVE ART-PROC-AK-P(IX5)       TO  W001-DET8-P-ALAGERE                
344200     MOVE ART-AK-PAS(IX6)          TO  W001-DET8-ALAGERF                  
344300     MOVE ART-PROC-AK-P(IX6)       TO  W001-DET8-P-ALAGERF                
344400     MOVE ART-AK-PAS(IX7)          TO  W001-DET8-ALAGERG                  
344500     MOVE ART-PROC-AK-P(IX7)       TO  W001-DET8-P-ALAGERG                
344600     MOVE ART-AK-PAS(IX8)          TO  W001-DET8-ALAGERH                  
344700     MOVE ART-PROC-AK-P(IX8)       TO  W001-DET8-P-ALAGERH                
344800     MOVE PSUM-AK-PAS(PSUM-IX)     TO  W001-DET8-TOT                      
344900     MOVE PSUM-PROC-AK-P(PSUM-IX)  TO  W001-DET8-P-TOT                    
345000     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
345100     MOVE +1 TO W001-SKIP                                                 
345200     PERFORM S21-SKRIV-LISTA                                              
345300                                                                          
345400     MOVE ART-OLAGER(IX1)          TO  W001-DET9-OLAGERA                  
345500     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET9-P-OLAGERA                
345600     MOVE ART-OLAGER(IX2)          TO  W001-DET9-OLAGERB                  
345700     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET9-P-OLAGERB                
345800     MOVE ART-OLAGER(IX3)          TO  W001-DET9-OLAGERC                  
345900     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET9-P-OLAGERC                
346000     MOVE ART-OLAGER(IX4)          TO  W001-DET9-OLAGERD                  
346100     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET9-P-OLAGERD                
346200     MOVE ART-OLAGER(IX5)          TO  W001-DET9-OLAGERE                  
346300     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET9-P-OLAGERE                
346400     MOVE ART-OLAGER(IX6)          TO  W001-DET9-OLAGERF                  
346500     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET9-P-OLAGERF                
346600     MOVE ART-OLAGER(IX7)          TO  W001-DET9-OLAGERG                  
346700     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET9-P-OLAGERG                
346800     MOVE ART-OLAGER(IX8)          TO  W001-DET9-OLAGERH                  
346900     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET9-P-OLAGERH                
347000     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET9-TOT                      
347100     MOVE PSUM-PROC-OLAGER(PSUM-IX)                                       
347200                                   TO  W001-DET9-P-TOT                    
347300     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
347400     MOVE +1 TO W001-SKIP                                                 
347500     PERFORM S21-SKRIV-LISTA                                              
347600                                                                          
347700     MOVE ART-SLAGER(IX1)          TO  W001-DET10-SLAGERA                 
347800     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET10-P-SLAGERA               
347900     MOVE ART-SLAGER(IX2)          TO  W001-DET10-SLAGERB                 
348000     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET10-P-SLAGERB               
348100     MOVE ART-SLAGER(IX3)          TO  W001-DET10-SLAGERC                 
348200     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET10-P-SLAGERC               
348300     MOVE ART-SLAGER(IX4)          TO  W001-DET10-SLAGERD                 
348400     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET10-P-SLAGERD               
348500     MOVE ART-SLAGER(IX5)          TO  W001-DET10-SLAGERE                 
348600     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET10-P-SLAGERE               
348700     MOVE ART-SLAGER(IX6)          TO  W001-DET10-SLAGERF                 
348800     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET10-P-SLAGERF               
348900     MOVE ART-SLAGER(IX7)          TO  W001-DET10-SLAGERG                 
349000     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET10-P-SLAGERG               
349100     MOVE ART-SLAGER(IX8)          TO  W001-DET10-SLAGERH                 
349200     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET10-P-SLAGERH               
349300     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET10-TOT                     
349400     MOVE PSUM-PROC-SLAGER(PSUM-IX)                                       
349500                                   TO  W001-DET10-P-TOT                   
349600     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
349700     MOVE +1 TO W001-SKIP                                                 
349800     PERFORM S21-SKRIV-LISTA                                              
349900                                                                          
350000     MOVE ART-MLAGER(IX1)          TO  W001-DET11-MLAGERA                 
350100     MOVE ART-PROC-MLAGER(IX1)     TO  W001-DET11-P-MLAGERA               
350200     MOVE ART-MLAGER(IX2)          TO  W001-DET11-MLAGERB                 
350300     MOVE ART-PROC-MLAGER(IX2)     TO  W001-DET11-P-MLAGERB               
350400     MOVE ART-MLAGER(IX3)          TO  W001-DET11-MLAGERC                 
350500     MOVE ART-PROC-MLAGER(IX3)     TO  W001-DET11-P-MLAGERC               
350600     MOVE ART-MLAGER(IX4)          TO  W001-DET11-MLAGERD                 
350700     MOVE ART-PROC-MLAGER(IX4)     TO  W001-DET11-P-MLAGERD               
350800     MOVE ART-MLAGER(IX5)          TO  W001-DET11-MLAGERE                 
350900     MOVE ART-PROC-MLAGER(IX5)     TO  W001-DET11-P-MLAGERE               
351000     MOVE ART-MLAGER(IX6)          TO  W001-DET11-MLAGERF                 
351100     MOVE ART-PROC-MLAGER(IX6)     TO  W001-DET11-P-MLAGERF               
351200     MOVE ART-MLAGER(IX7)          TO  W001-DET11-MLAGERG                 
351300     MOVE ART-PROC-MLAGER(IX7)     TO  W001-DET11-P-MLAGERG               
351400     MOVE ART-MLAGER(IX8)          TO  W001-DET11-MLAGERH                 
351500     MOVE ART-PROC-MLAGER(IX8)     TO  W001-DET11-P-MLAGERH               
351600     MOVE PSUM-MLAGER(PSUM-IX)     TO  W001-DET11-TOT                     
351700     MOVE PSUM-PROC-MLAGER(PSUM-IX)                                       
351800                                   TO  W001-DET11-P-TOT                   
351900     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
352000     MOVE +1 TO W001-SKIP                                                 
352100     PERFORM S21-SKRIV-LISTA                                              
352200                                                                          
352300     MOVE ART-KVOT(IX1)            TO  W001-DET12-KVOTA                   
352400     MOVE ART-PROC-KVOT(IX1)       TO  W001-DET12-P-KVOTA                 
352500     MOVE ART-KVOT(IX2)            TO  W001-DET12-KVOTB                   
352600     MOVE ART-PROC-KVOT(IX2)       TO  W001-DET12-P-KVOTB                 
352700     MOVE ART-KVOT(IX3)            TO  W001-DET12-KVOTC                   
352800     MOVE ART-PROC-KVOT(IX3)       TO  W001-DET12-P-KVOTC                 
352900     MOVE ART-KVOT(IX4)            TO  W001-DET12-KVOTD                   
353000     MOVE ART-PROC-KVOT(IX4)       TO  W001-DET12-P-KVOTD                 
353100     MOVE ART-KVOT(IX5)            TO  W001-DET12-KVOTE                   
353200     MOVE ART-PROC-KVOT(IX5)       TO  W001-DET12-P-KVOTE                 
353300     MOVE ART-KVOT(IX6)            TO  W001-DET12-KVOTF                   
353400     MOVE ART-PROC-KVOT(IX6)       TO  W001-DET12-P-KVOTF                 
353500     MOVE ART-KVOT(IX7)            TO  W001-DET12-KVOTG                   
353600     MOVE ART-PROC-KVOT(IX7)       TO  W001-DET12-P-KVOTG                 
353700     MOVE ART-KVOT(IX8)            TO  W001-DET12-KVOTH                   
353800     MOVE ART-PROC-KVOT(IX8)       TO  W001-DET12-P-KVOTH                 
353900     MOVE PSUM-KVOT(PSUM-IX)       TO  W001-DET12-TOT                     
354000     MOVE PSUM-PROC-KVOT(PSUM-IX)  TO  W001-DET12-P-TOT                   
354100     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
354200     MOVE +1 TO W001-SKIP                                                 
354300     PERFORM S21-SKRIV-LISTA                                              
354400                                                                          
354500     MOVE ART-SPLIT(IX1)           TO  W001-DET13-SPLITA                  
354600     MOVE ART-SPLIT(IX2)           TO  W001-DET13-SPLITB                  
354700     MOVE ART-SPLIT(IX3)           TO  W001-DET13-SPLITC                  
354800     MOVE ART-SPLIT(IX4)           TO  W001-DET13-SPLITD                  
354900     MOVE ART-SPLIT(IX5)           TO  W001-DET13-SPLITE                  
355000     MOVE ART-SPLIT(IX6)           TO  W001-DET13-SPLITF                  
355100     MOVE ART-SPLIT(IX7)           TO  W001-DET13-SPLITG                  
355200     MOVE ART-SPLIT(IX8)           TO  W001-DET13-SPLITH                  
355300     MOVE PSUM-SPLIT(PSUM-IX)      TO  W001-DET13-TOT                     
355400     MOVE ZERO                     TO  W001-DET13-P-TOT                   
355500     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
355600     MOVE +1 TO W001-SKIP                                                 
355700     PERFORM S21-SKRIV-LISTA                                              
355800                                                                          
355900     MOVE ART-OMSHAST-DISP(IX1)    TO  W001-DET14-OMSHASTA                
356000     MOVE ART-OMSHAST-PROC-D(IX1)  TO  W001-DET14-P-OMSHASTA              
356100     MOVE ART-OMSHAST-DISP(IX2)    TO  W001-DET14-OMSHASTB                
356200     MOVE ART-OMSHAST-PROC-D(IX2)  TO  W001-DET14-P-OMSHASTB              
356300     MOVE ART-OMSHAST-DISP(IX3)    TO  W001-DET14-OMSHASTC                
356400     MOVE ART-OMSHAST-PROC-D(IX3)  TO  W001-DET14-P-OMSHASTC              
356500     MOVE ART-OMSHAST-DISP(IX4)    TO  W001-DET14-OMSHASTD                
356600     MOVE ART-OMSHAST-PROC-D(IX4)  TO  W001-DET14-P-OMSHASTD              
356700     MOVE ART-OMSHAST-DISP(IX5)    TO  W001-DET14-OMSHASTE                
356800     MOVE ART-OMSHAST-PROC-D(IX5)  TO  W001-DET14-P-OMSHASTE              
356900     MOVE ART-OMSHAST-DISP(IX6)    TO  W001-DET14-OMSHASTF                
357000     MOVE ART-OMSHAST-PROC-D(IX6)  TO  W001-DET14-P-OMSHASTF              
357100     MOVE ART-OMSHAST-DISP(IX7)    TO  W001-DET14-OMSHASTG                
357200     MOVE ART-OMSHAST-PROC-D(IX7)  TO  W001-DET14-P-OMSHASTG              
357300     MOVE ART-OMSHAST-DISP(IX8)    TO  W001-DET14-OMSHASTH                
357400     MOVE ART-OMSHAST-PROC-D(IX8)  TO  W001-DET14-P-OMSHASTH              
357500     MOVE PSUM-OMSHAST-DISP(PSUM-IX) TO W001-DET14-TOT                    
357600     MOVE PSUM-OMSHAST-PROC-D(PSUM-IX)                                    
357700                                     TO W001-DET14-P-TOT                  
357800     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
357900     MOVE +1 TO W001-SKIP                                                 
358000     PERFORM S21-SKRIV-LISTA                                              
358100                                                                          
358200     MOVE ART-OMSHAST-LS(IX1)      TO  W001-DET15-OMSHASTA                
358300     MOVE ART-OMSHAST-PROC-LS(IX1) TO  W001-DET15-P-OMSHASTA              
358400     MOVE ART-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB                
358500     MOVE ART-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB              
358600     MOVE ART-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC                
358700     MOVE ART-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC              
358800     MOVE ART-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD                
358900     MOVE ART-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD              
359000     MOVE ART-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE                
359100     MOVE ART-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE              
359200     MOVE ART-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF                
359300     MOVE ART-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF              
359400     MOVE ART-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG                
359500     MOVE ART-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG              
359600     MOVE ART-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH                
359700     MOVE ART-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH              
359800     MOVE PSUM-OMSHAST-LS(PSUM-IX) TO  W001-DET15-TOT                     
359900     MOVE PSUM-OMSHAST-PROC-LS(PSUM-IX)                                   
360000                                     TO W001-DET15-P-TOT                  
360100     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
360200     MOVE +1 TO W001-SKIP                                                 
360300     PERFORM S21-SKRIV-LISTA                                              
360400                                                                          
360500     MOVE ART-SERVG-BTO(IX1)       TO  W001-DET16-SERVG-BTOA              
360600     MOVE ART-SERVG-BTO(IX2)       TO  W001-DET16-SERVG-BTOB              
360700     MOVE ART-SERVG-BTO(IX3)       TO  W001-DET16-SERVG-BTOC              
360800     MOVE ART-SERVG-BTO(IX4)       TO  W001-DET16-SERVG-BTOD              
360900     MOVE ART-SERVG-BTO(IX5)       TO  W001-DET16-SERVG-BTOE              
361000     MOVE ART-SERVG-BTO(IX6)       TO  W001-DET16-SERVG-BTOF              
361100     MOVE ART-SERVG-BTO(IX7)       TO  W001-DET16-SERVG-BTOG              
361200     MOVE ART-SERVG-BTO(IX8)       TO  W001-DET16-SERVG-BTOH              
361300     MOVE PSUM-SERVG-BTO (PSUM-IX) TO  W001-DET16-TOT                     
361400     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
361500     MOVE +1 TO W001-SKIP                                                 
361600     PERFORM S21-SKRIV-LISTA                                              
361700                                                                          
361800     MOVE ART-SERVG-NTO(IX1)       TO  W001-DET17-SERVG-NTOA              
361900     MOVE ART-SERVG-NTO(IX2)       TO  W001-DET17-SERVG-NTOB              
362000     MOVE ART-SERVG-NTO(IX3)       TO  W001-DET17-SERVG-NTOC              
362100     MOVE ART-SERVG-NTO(IX4)       TO  W001-DET17-SERVG-NTOD              
362200     MOVE ART-SERVG-NTO(IX5)       TO  W001-DET17-SERVG-NTOE              
362300     MOVE ART-SERVG-NTO(IX6)       TO  W001-DET17-SERVG-NTOF              
362400     MOVE ART-SERVG-NTO(IX7)       TO  W001-DET17-SERVG-NTOG              
362500     MOVE ART-SERVG-NTO(IX8)       TO  W001-DET17-SERVG-NTOH              
362600     MOVE PSUM-SERVG-NTO(PSUM-IX)  TO  W001-DET17-TOT                     
362700     MOVE ZERO                     TO  W001-DET17-P-TOT                   
362800     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
362900     MOVE +1 TO W001-SKIP                                                 
363000     PERFORM S21-SKRIV-LISTA                                              
363100     .                                                                    
363200     EJECT                                                                
363300 CB-FLYTTA-SKRIV-TOT SECTION.                                             
363400                                                                          
363500     MOVE FSUM-KVANT-AKT(IX1)      TO  W001-DET1-KVANTA                   
363600     MOVE FSUM-PROC-KVANT-A(IX1)   TO  W001-DET1-P-KVANTA                 
363700     MOVE FSUM-KVANT-AKT(IX2)      TO  W001-DET1-KVANTB                   
363800     MOVE FSUM-PROC-KVANT-A(IX2)   TO  W001-DET1-P-KVANTB                 
363900     MOVE FSUM-KVANT-AKT(IX3)      TO  W001-DET1-KVANTC                   
364000     MOVE FSUM-PROC-KVANT-A(IX3)   TO  W001-DET1-P-KVANTC                 
364100     MOVE FSUM-KVANT-AKT(IX4)      TO  W001-DET1-KVANTD                   
364200     MOVE FSUM-PROC-KVANT-A(IX4)   TO  W001-DET1-P-KVANTD                 
364300     MOVE FSUM-KVANT-AKT(IX5)      TO  W001-DET1-KVANTE                   
364400     MOVE FSUM-PROC-KVANT-A(IX5)   TO  W001-DET1-P-KVANTE                 
364500     MOVE FSUM-KVANT-AKT(IX6)      TO  W001-DET1-KVANTF                   
364600     MOVE FSUM-PROC-KVANT-A(IX6)   TO  W001-DET1-P-KVANTF                 
364700     MOVE FSUM-KVANT-AKT(IX7)      TO  W001-DET1-KVANTG                   
364800     MOVE FSUM-PROC-KVANT-A(IX7)   TO  W001-DET1-P-KVANTG                 
364900     MOVE FSUM-KVANT-AKT(IX8)      TO  W001-DET1-KVANTH                   
365000     MOVE FSUM-PROC-KVANT-A(IX8)   TO  W001-DET1-P-KVANTH                 
365100     MOVE TOT-KVANT-AKT            TO  W001-DET1-TOT                      
365200     MOVE ZERO                     TO  W001-DET1-P-TOT                    
365300     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
365400     MOVE +2 TO W001-SKIP                                                 
365500     PERFORM S21-SKRIV-LISTA                                              
365600                                                                          
365700     MOVE FSUM-KVANT-PAS(IX1)      TO  W001-DET2-KVANTA                   
365800     MOVE FSUM-PROC-KVANT-P(IX1)   TO  W001-DET2-P-KVANTA                 
365900     MOVE FSUM-KVANT-PAS(IX2)      TO  W001-DET2-KVANTB                   
366000     MOVE FSUM-PROC-KVANT-P(IX2)   TO  W001-DET2-P-KVANTB                 
366100     MOVE FSUM-KVANT-PAS(IX3)      TO  W001-DET2-KVANTC                   
366200     MOVE FSUM-PROC-KVANT-P(IX3)   TO  W001-DET2-P-KVANTC                 
366300     MOVE FSUM-KVANT-PAS(IX4)      TO  W001-DET2-KVANTD                   
366400     MOVE FSUM-PROC-KVANT-P(IX4)   TO  W001-DET2-P-KVANTD                 
366500     MOVE FSUM-KVANT-PAS(IX5)      TO  W001-DET2-KVANTE                   
366600     MOVE FSUM-PROC-KVANT-P(IX5)   TO  W001-DET2-P-KVANTE                 
366700     MOVE FSUM-KVANT-PAS(IX6)      TO  W001-DET2-KVANTF                   
366800     MOVE FSUM-PROC-KVANT-P(IX6)   TO  W001-DET2-P-KVANTF                 
366900     MOVE FSUM-KVANT-PAS(IX7)      TO  W001-DET2-KVANTG                   
367000     MOVE FSUM-PROC-KVANT-P(IX7)   TO  W001-DET2-P-KVANTG                 
367100     MOVE FSUM-KVANT-PAS(IX8)      TO  W001-DET2-KVANTH                   
367200     MOVE FSUM-PROC-KVANT-P(IX8)   TO  W001-DET2-P-KVANTH                 
367300     MOVE TOT-KVANT-PAS            TO  W001-DET2-TOT                      
367400     MOVE ZERO                     TO  W001-DET2-P-TOT                    
367500     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
367600     MOVE +1 TO W001-SKIP                                                 
367700     PERFORM S21-SKRIV-LISTA                                              
367800                                                                          
367900     MOVE FSUM-KVDISP-AKT(IX1)     TO  W001-DET3-DLAGERA                  
368000     MOVE FSUM-PROC-KVDISP-A(IX1)  TO  W001-DET3-P-DLAGERA                
368100     MOVE FSUM-KVDISP-AKT(IX2)     TO  W001-DET3-DLAGERB                  
368200     MOVE FSUM-PROC-KVDISP-A(IX2)  TO  W001-DET3-P-DLAGERB                
368300     MOVE FSUM-KVDISP-AKT(IX3)     TO  W001-DET3-DLAGERC                  
368400     MOVE FSUM-PROC-KVDISP-A(IX3)  TO  W001-DET3-P-DLAGERC                
368500     MOVE FSUM-KVDISP-AKT(IX4)     TO  W001-DET3-DLAGERD                  
368600     MOVE FSUM-PROC-KVDISP-A(IX4)  TO  W001-DET3-P-DLAGERD                
368700     MOVE FSUM-KVDISP-AKT(IX5)     TO  W001-DET3-DLAGERE                  
368800     MOVE FSUM-PROC-KVDISP-A(IX5)  TO  W001-DET3-P-DLAGERE                
368900     MOVE FSUM-KVDISP-AKT(IX6)     TO  W001-DET3-DLAGERF                  
369000     MOVE FSUM-PROC-KVDISP-A(IX6)  TO  W001-DET3-P-DLAGERF                
369100     MOVE FSUM-KVDISP-AKT(IX7)     TO  W001-DET3-DLAGERG                  
369200     MOVE FSUM-PROC-KVDISP-A(IX7)  TO  W001-DET3-P-DLAGERG                
369300     MOVE FSUM-KVDISP-AKT(IX8)     TO  W001-DET3-DLAGERH                  
369400     MOVE FSUM-PROC-KVDISP-A(IX8)  TO  W001-DET3-P-DLAGERH                
369500     MOVE TOT-KVDISP-AKT           TO  W001-DET3-TOT                      
369600     MOVE ZERO                     TO  W001-DET3-P-TOT                    
369700     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
369800     MOVE +1 TO W001-SKIP                                                 
369900     PERFORM S21-SKRIV-LISTA                                              
370000                                                                          
370100     MOVE FSUM-KVDISP-PAS(IX1)     TO  W001-DET4-DLAGERA                  
370200     MOVE FSUM-PROC-KVDISP-P(IX1)  TO  W001-DET4-P-DLAGERA                
370300     MOVE FSUM-KVDISP-PAS(IX2)     TO  W001-DET4-DLAGERB                  
370400     MOVE FSUM-PROC-KVDISP-P(IX2)  TO  W001-DET4-P-DLAGERB                
370500     MOVE FSUM-KVDISP-PAS(IX3)     TO  W001-DET4-DLAGERC                  
370600     MOVE FSUM-PROC-KVDISP-P(IX3)  TO  W001-DET4-P-DLAGERC                
370700     MOVE FSUM-KVDISP-PAS(IX4)     TO  W001-DET4-DLAGERD                  
370800     MOVE FSUM-PROC-KVDISP-P(IX4)  TO  W001-DET4-P-DLAGERD                
370900     MOVE FSUM-KVDISP-PAS(IX5)     TO  W001-DET4-DLAGERE                  
371000     MOVE FSUM-PROC-KVDISP-P(IX5)  TO  W001-DET4-P-DLAGERE                
371100     MOVE FSUM-KVDISP-PAS(IX6)     TO  W001-DET4-DLAGERF                  
371200     MOVE FSUM-PROC-KVDISP-P(IX6)  TO  W001-DET4-P-DLAGERF                
371300     MOVE FSUM-KVDISP-PAS(IX7)     TO  W001-DET4-DLAGERG                  
371400     MOVE FSUM-PROC-KVDISP-P(IX7)  TO  W001-DET4-P-DLAGERG                
371500     MOVE FSUM-KVDISP-PAS(IX8)     TO  W001-DET4-DLAGERH                  
371600     MOVE FSUM-PROC-KVDISP-P(IX8)  TO  W001-DET4-P-DLAGERH                
371700     MOVE TOT-KVDISP-PAS           TO  W001-DET4-TOT                      
371800     MOVE ZERO                     TO  W001-DET4-P-TOT                    
371900     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
372000     MOVE +1 TO W001-SKIP                                                 
372100     PERFORM S21-SKRIV-LISTA                                              
372200                                                                          
372300     MOVE FSUM-LS-AKT(IX1)         TO  W001-DET5-LLAGERA                  
372400     MOVE FSUM-PROC-LS-A(IX1)      TO  W001-DET5-P-LLAGERA                
372500     MOVE FSUM-LS-AKT(IX2)         TO  W001-DET5-LLAGERB                  
372600     MOVE FSUM-PROC-LS-A(IX2)      TO  W001-DET5-P-LLAGERB                
372700     MOVE FSUM-LS-AKT(IX3)         TO  W001-DET5-LLAGERC                  
372800     MOVE FSUM-PROC-LS-A(IX3)      TO  W001-DET5-P-LLAGERC                
372900     MOVE FSUM-LS-AKT(IX4)         TO  W001-DET5-LLAGERD                  
373000     MOVE FSUM-PROC-LS-A(IX4)      TO  W001-DET5-P-LLAGERD                
373100     MOVE FSUM-LS-AKT(IX5)         TO  W001-DET5-LLAGERE                  
373200     MOVE FSUM-PROC-LS-A(IX5)      TO  W001-DET5-P-LLAGERE                
373300     MOVE FSUM-LS-AKT(IX6)         TO  W001-DET5-LLAGERF                  
373400     MOVE FSUM-PROC-LS-A(IX6)      TO  W001-DET5-P-LLAGERF                
373500     MOVE FSUM-LS-AKT(IX7)         TO  W001-DET5-LLAGERG                  
373600     MOVE FSUM-PROC-LS-A(IX7)      TO  W001-DET5-P-LLAGERG                
373700     MOVE FSUM-LS-AKT(IX8)         TO  W001-DET5-LLAGERH                  
373800     MOVE FSUM-PROC-LS-A(IX8)      TO  W001-DET5-P-LLAGERH                
373900     MOVE TOT-LS-AKT               TO  W001-DET5-TOT                      
374000     MOVE ZERO                     TO  W001-DET5-P-TOT                    
374100     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
374200     MOVE +1 TO W001-SKIP                                                 
374300     PERFORM S21-SKRIV-LISTA                                              
374400                                                                          
374500     MOVE FSUM-LS-PAS(IX1)         TO  W001-DET6-LLAGERA                  
374600     MOVE FSUM-PROC-LS-P(IX1)      TO  W001-DET6-P-LLAGERA                
374700     MOVE FSUM-LS-PAS(IX2)         TO  W001-DET6-LLAGERB                  
374800     MOVE FSUM-PROC-LS-P(IX2)      TO  W001-DET6-P-LLAGERB                
374900     MOVE FSUM-LS-PAS(IX3)         TO  W001-DET6-LLAGERC                  
375000     MOVE FSUM-PROC-LS-P(IX3)      TO  W001-DET6-P-LLAGERC                
375100     MOVE FSUM-LS-PAS(IX4)         TO  W001-DET6-LLAGERD                  
375200     MOVE FSUM-PROC-LS-P(IX4)      TO  W001-DET6-P-LLAGERD                
375300     MOVE FSUM-LS-PAS(IX5)         TO  W001-DET6-LLAGERE                  
375400     MOVE FSUM-PROC-LS-P(IX5)      TO  W001-DET6-P-LLAGERE                
375500     MOVE FSUM-LS-PAS(IX6)         TO  W001-DET6-LLAGERF                  
375600     MOVE FSUM-PROC-LS-P(IX6)      TO  W001-DET6-P-LLAGERF                
375700     MOVE FSUM-LS-PAS(IX7)         TO  W001-DET6-LLAGERG                  
375800     MOVE FSUM-PROC-LS-P(IX7)      TO  W001-DET6-P-LLAGERG                
375900     MOVE FSUM-LS-PAS(IX8)         TO  W001-DET6-LLAGERH                  
376000     MOVE FSUM-PROC-LS-P(IX8)      TO  W001-DET6-P-LLAGERH                
376100     MOVE TOT-LS-PAS               TO  W001-DET6-TOT                      
376200     MOVE ZERO                     TO  W001-DET6-P-TOT                    
376300     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
376400     MOVE +1 TO W001-SKIP                                                 
376500     PERFORM S21-SKRIV-LISTA                                              
376600                                                                          
376700     MOVE FSUM-AK-AKT(IX1)         TO  W001-DET7-ALAGERA                  
376800     MOVE FSUM-PROC-AK-A(IX1)      TO  W001-DET7-P-ALAGERA                
376900     MOVE FSUM-AK-AKT(IX2)         TO  W001-DET7-ALAGERB                  
377000     MOVE FSUM-PROC-AK-A(IX2)      TO  W001-DET7-P-ALAGERB                
377100     MOVE FSUM-AK-AKT(IX3)         TO  W001-DET7-ALAGERC                  
377200     MOVE FSUM-PROC-AK-A(IX3)      TO  W001-DET7-P-ALAGERC                
377300     MOVE FSUM-AK-AKT(IX4)         TO  W001-DET7-ALAGERD                  
377400     MOVE FSUM-PROC-AK-A(IX4)      TO  W001-DET7-P-ALAGERD                
377500     MOVE FSUM-AK-AKT(IX5)         TO  W001-DET7-ALAGERE                  
377600     MOVE FSUM-PROC-AK-A(IX5)      TO  W001-DET7-P-ALAGERE                
377700     MOVE FSUM-AK-AKT(IX6)         TO  W001-DET7-ALAGERF                  
377800     MOVE FSUM-PROC-AK-A(IX6)      TO  W001-DET7-P-ALAGERF                
377900     MOVE FSUM-AK-AKT(IX7)         TO  W001-DET7-ALAGERG                  
378000     MOVE FSUM-PROC-AK-A(IX7)      TO  W001-DET7-P-ALAGERG                
378100     MOVE FSUM-AK-AKT(IX8)         TO  W001-DET7-ALAGERH                  
378200     MOVE FSUM-PROC-AK-A(IX8)      TO  W001-DET7-P-ALAGERH                
378300     MOVE TOT-AK-AKT               TO  W001-DET7-TOT                      
378400     MOVE ZERO                     TO  W001-DET7-P-TOT                    
378500     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
378600     MOVE +1 TO W001-SKIP                                                 
378700     PERFORM S21-SKRIV-LISTA                                              
378800                                                                          
378900     MOVE FSUM-AK-PAS(IX1)         TO  W001-DET8-ALAGERA                  
379000     MOVE FSUM-PROC-AK-P(IX1)      TO  W001-DET8-P-ALAGERA                
379100     MOVE FSUM-AK-PAS(IX2)         TO  W001-DET8-ALAGERB                  
379200     MOVE FSUM-PROC-AK-P(IX2)      TO  W001-DET8-P-ALAGERB                
379300     MOVE FSUM-AK-PAS(IX3)         TO  W001-DET8-ALAGERC                  
379400     MOVE FSUM-PROC-AK-P(IX3)      TO  W001-DET8-P-ALAGERC                
379500     MOVE FSUM-AK-PAS(IX4)         TO  W001-DET8-ALAGERD                  
379600     MOVE FSUM-PROC-AK-P(IX4)      TO  W001-DET8-P-ALAGERD                
379700     MOVE FSUM-AK-PAS(IX5)         TO  W001-DET8-ALAGERE                  
379800     MOVE FSUM-PROC-AK-P(IX5)      TO  W001-DET8-P-ALAGERE                
379900     MOVE FSUM-AK-PAS(IX6)         TO  W001-DET8-ALAGERF                  
380000     MOVE FSUM-PROC-AK-P(IX6)      TO  W001-DET8-P-ALAGERF                
380100     MOVE FSUM-AK-PAS(IX7)         TO  W001-DET8-ALAGERG                  
380200     MOVE FSUM-PROC-AK-P(IX7)      TO  W001-DET8-P-ALAGERG                
380300     MOVE FSUM-AK-PAS(IX8)         TO  W001-DET8-ALAGERH                  
380400     MOVE FSUM-PROC-AK-P(IX8)      TO  W001-DET8-P-ALAGERH                
380500     MOVE TOT-AK-PAS               TO  W001-DET8-TOT                      
380600     MOVE ZERO                     TO  W001-DET8-P-TOT                    
380700     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
380800     MOVE +1 TO W001-SKIP                                                 
380900     PERFORM S21-SKRIV-LISTA                                              
381000                                                                          
381100     MOVE FSUM-OLAGER(IX1)         TO  W001-DET9-OLAGERA                  
381200     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET9-P-OLAGERA                
381300     MOVE FSUM-OLAGER(IX2)         TO  W001-DET9-OLAGERB                  
381400     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET9-P-OLAGERB                
381500     MOVE FSUM-OLAGER(IX3)         TO  W001-DET9-OLAGERC                  
381600     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET9-P-OLAGERC                
381700     MOVE FSUM-OLAGER(IX4)         TO  W001-DET9-OLAGERD                  
381800     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET9-P-OLAGERD                
381900     MOVE FSUM-OLAGER(IX5)         TO  W001-DET9-OLAGERE                  
382000     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET9-P-OLAGERE                
382100     MOVE FSUM-OLAGER(IX6)         TO  W001-DET9-OLAGERF                  
382200     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET9-P-OLAGERF                
382300     MOVE FSUM-OLAGER(IX7)         TO  W001-DET9-OLAGERG                  
382400     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET9-P-OLAGERG                
382500     MOVE FSUM-OLAGER(IX8)         TO  W001-DET9-OLAGERH                  
382600     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET9-P-OLAGERH                
382700     MOVE TOT-OLAGER               TO  W001-DET9-TOT                      
382800     MOVE TOT-PROC-OLAGER          TO  W001-DET9-P-TOT                    
382900     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
383000     MOVE +1 TO W001-SKIP                                                 
383100     PERFORM S21-SKRIV-LISTA                                              
383200                                                                          
383300     MOVE FSUM-SLAGER(IX1)         TO  W001-DET10-SLAGERA                 
383400     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET10-P-SLAGERA               
383500     MOVE FSUM-SLAGER(IX2)         TO  W001-DET10-SLAGERB                 
383600     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET10-P-SLAGERB               
383700     MOVE FSUM-SLAGER(IX3)         TO  W001-DET10-SLAGERC                 
383800     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET10-P-SLAGERC               
383900     MOVE FSUM-SLAGER(IX4)         TO  W001-DET10-SLAGERD                 
384000     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET10-P-SLAGERD               
384100     MOVE FSUM-SLAGER(IX5)         TO  W001-DET10-SLAGERE                 
384200     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET10-P-SLAGERE               
384300     MOVE FSUM-SLAGER(IX6)         TO  W001-DET10-SLAGERF                 
384400     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET10-P-SLAGERF               
384500     MOVE FSUM-SLAGER(IX7)         TO  W001-DET10-SLAGERG                 
384600     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET10-P-SLAGERG               
384700     MOVE FSUM-SLAGER(IX8)         TO  W001-DET10-SLAGERH                 
384800     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET10-P-SLAGERH               
384900     MOVE TOT-SLAGER               TO  W001-DET10-TOT                     
385000     MOVE TOT-PROC-SLAGER          TO  W001-DET10-P-TOT                   
385100     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
385200     MOVE +1 TO W001-SKIP                                                 
385300     PERFORM S21-SKRIV-LISTA                                              
385400                                                                          
385500     MOVE FSUM-MLAGER(IX1)         TO  W001-DET11-MLAGERA                 
385600     MOVE FSUM-PROC-MLAGER(IX1)    TO  W001-DET11-P-MLAGERA               
385700     MOVE FSUM-MLAGER(IX2)         TO  W001-DET11-MLAGERB                 
385800     MOVE FSUM-PROC-MLAGER(IX2)    TO  W001-DET11-P-MLAGERB               
385900     MOVE FSUM-MLAGER(IX3)         TO  W001-DET11-MLAGERC                 
386000     MOVE FSUM-PROC-MLAGER(IX3)    TO  W001-DET11-P-MLAGERC               
386100     MOVE FSUM-MLAGER(IX4)         TO  W001-DET11-MLAGERD                 
386200     MOVE FSUM-PROC-MLAGER(IX4)    TO  W001-DET11-P-MLAGERD               
386300     MOVE FSUM-MLAGER(IX5)         TO  W001-DET11-MLAGERE                 
386400     MOVE FSUM-PROC-MLAGER(IX5)    TO  W001-DET11-P-MLAGERE               
386500     MOVE FSUM-MLAGER(IX6)         TO  W001-DET11-MLAGERF                 
386600     MOVE FSUM-PROC-MLAGER(IX6)    TO  W001-DET11-P-MLAGERF               
386700     MOVE FSUM-MLAGER(IX7)         TO  W001-DET11-MLAGERG                 
386800     MOVE FSUM-PROC-MLAGER(IX7)    TO  W001-DET11-P-MLAGERG               
386900     MOVE FSUM-MLAGER(IX8)         TO  W001-DET11-MLAGERH                 
387000     MOVE FSUM-PROC-MLAGER(IX8)    TO  W001-DET11-P-MLAGERH               
387100     MOVE TOT-MLAGER               TO  W001-DET11-TOT                     
387200     MOVE TOT-PROC-MLAGER          TO  W001-DET11-P-TOT                   
387300     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
387400     MOVE +1 TO W001-SKIP                                                 
387500     PERFORM S21-SKRIV-LISTA                                              
387600                                                                          
387700     MOVE FSUM-KVOT(IX1)           TO  W001-DET12-KVOTA                   
387800     MOVE FSUM-PROC-KVOT(IX1)      TO  W001-DET12-P-KVOTA                 
387900     MOVE FSUM-KVOT(IX2)           TO  W001-DET12-KVOTB                   
388000     MOVE FSUM-PROC-KVOT(IX2)      TO  W001-DET12-P-KVOTB                 
388100     MOVE FSUM-KVOT(IX3)           TO  W001-DET12-KVOTC                   
388200     MOVE FSUM-PROC-KVOT(IX3)      TO  W001-DET12-P-KVOTC                 
388300     MOVE FSUM-KVOT(IX4)           TO  W001-DET12-KVOTD                   
388400     MOVE FSUM-PROC-KVOT(IX4)      TO  W001-DET12-P-KVOTD                 
388500     MOVE FSUM-KVOT(IX5)           TO  W001-DET12-KVOTE                   
388600     MOVE FSUM-PROC-KVOT(IX5)      TO  W001-DET12-P-KVOTE                 
388700     MOVE FSUM-KVOT(IX6)           TO  W001-DET12-KVOTF                   
388800     MOVE FSUM-PROC-KVOT(IX6)      TO  W001-DET12-P-KVOTF                 
388900     MOVE FSUM-KVOT(IX7)           TO  W001-DET12-KVOTG                   
389000     MOVE FSUM-PROC-KVOT(IX7)      TO  W001-DET12-P-KVOTG                 
389100     MOVE FSUM-KVOT(IX8)           TO  W001-DET12-KVOTH                   
389200     MOVE FSUM-PROC-KVOT(IX8)      TO  W001-DET12-P-KVOTH                 
389300     MOVE TOT-KVOT                 TO  W001-DET12-TOT                     
389400     MOVE TOT-PROC-MLAGER          TO  W001-DET12-P-TOT                   
389500     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
389600     MOVE +1 TO W001-SKIP                                                 
389700     PERFORM S21-SKRIV-LISTA                                              
389800                                                                          
389900     MOVE FSUM-SPLIT(IX1)          TO  W001-DET13-SPLITA                  
390000     MOVE FSUM-SPLIT(IX2)          TO  W001-DET13-SPLITB                  
390100     MOVE FSUM-SPLIT(IX3)          TO  W001-DET13-SPLITC                  
390200     MOVE FSUM-SPLIT(IX4)          TO  W001-DET13-SPLITD                  
390300     MOVE FSUM-SPLIT(IX5)          TO  W001-DET13-SPLITE                  
390400     MOVE FSUM-SPLIT(IX6)          TO  W001-DET13-SPLITF                  
390500     MOVE FSUM-SPLIT(IX7)          TO  W001-DET13-SPLITG                  
390600     MOVE FSUM-SPLIT(IX8)          TO  W001-DET13-SPLITH                  
390700     MOVE TOT-SPLIT                TO  W001-DET13-TOT                     
390800     MOVE ZERO                     TO  W001-DET13-P-TOT                   
390900     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
391000     MOVE +1 TO W001-SKIP                                                 
391100     PERFORM S21-SKRIV-LISTA                                              
391200                                                                          
391300     MOVE FSUM-OMSHAST-DISP(IX1)   TO  W001-DET14-OMSHASTA                
391400     MOVE FSUM-OMSHAST-PROC-D(IX1) TO  W001-DET14-P-OMSHASTA              
391500     MOVE FSUM-OMSHAST-DISP(IX2)   TO  W001-DET14-OMSHASTB                
391600     MOVE FSUM-OMSHAST-PROC-D(IX2) TO  W001-DET14-P-OMSHASTB              
391700     MOVE FSUM-OMSHAST-DISP(IX3)   TO  W001-DET14-OMSHASTC                
391800     MOVE FSUM-OMSHAST-PROC-D(IX3) TO  W001-DET14-P-OMSHASTC              
391900     MOVE FSUM-OMSHAST-DISP(IX4)   TO  W001-DET14-OMSHASTD                
392000     MOVE FSUM-OMSHAST-PROC-D(IX4) TO  W001-DET14-P-OMSHASTD              
392100     MOVE FSUM-OMSHAST-DISP(IX5)   TO  W001-DET14-OMSHASTE                
392200     MOVE FSUM-OMSHAST-PROC-D(IX5) TO  W001-DET14-P-OMSHASTE              
392300     MOVE FSUM-OMSHAST-DISP(IX6)   TO  W001-DET14-OMSHASTF                
392400     MOVE FSUM-OMSHAST-PROC-D(IX6) TO  W001-DET14-P-OMSHASTF              
392500     MOVE FSUM-OMSHAST-DISP(IX7)   TO  W001-DET14-OMSHASTG                
392600     MOVE FSUM-OMSHAST-PROC-D(IX7) TO  W001-DET14-P-OMSHASTG              
392700     MOVE FSUM-OMSHAST-DISP(IX8)   TO  W001-DET14-OMSHASTH                
392800     MOVE FSUM-OMSHAST-PROC-D(IX8) TO  W001-DET14-P-OMSHASTH              
392900     MOVE TOT-OMSHAST-DISP         TO  W001-DET14-TOT                     
393000     MOVE ZERO                     TO  W001-DET14-P-TOT                   
393100     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
393200     MOVE +1 TO W001-SKIP                                                 
393300     PERFORM S21-SKRIV-LISTA                                              
393400                                                                          
393500     MOVE FSUM-OMSHAST-LS(IX1)      TO W001-DET15-OMSHASTA                
393600     MOVE FSUM-OMSHAST-PROC-LS(IX1) TO W001-DET15-P-OMSHASTA              
393700     MOVE FSUM-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB               
393800     MOVE FSUM-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB             
393900     MOVE FSUM-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC               
394000     MOVE FSUM-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC             
394100     MOVE FSUM-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD               
394200     MOVE FSUM-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD             
394300     MOVE FSUM-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE               
394400     MOVE FSUM-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE             
394500     MOVE FSUM-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF               
394600     MOVE FSUM-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF             
394700     MOVE FSUM-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG               
394800     MOVE FSUM-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG             
394900     MOVE FSUM-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH               
395000     MOVE FSUM-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH             
395100     MOVE TOT-OMSHAST-LS            TO  W001-DET15-TOT                    
395200     MOVE ZERO                      TO  W001-DET15-P-TOT                  
395300     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
395400     MOVE +1 TO W001-SKIP                                                 
395500     PERFORM S21-SKRIV-LISTA                                              
395600                                                                          
395700     MOVE FSUM-SERVG-BTO(IX1)     TO  W001-DET16-SERVG-BTOA               
395800     MOVE FSUM-SERVG-BTO(IX2)     TO  W001-DET16-SERVG-BTOB               
395900     MOVE FSUM-SERVG-BTO(IX3)     TO  W001-DET16-SERVG-BTOC               
396000     MOVE FSUM-SERVG-BTO(IX4)     TO  W001-DET16-SERVG-BTOD               
396100     MOVE FSUM-SERVG-BTO(IX5)     TO  W001-DET16-SERVG-BTOE               
396200     MOVE FSUM-SERVG-BTO(IX6)     TO  W001-DET16-SERVG-BTOF               
396300     MOVE FSUM-SERVG-BTO(IX7)     TO  W001-DET16-SERVG-BTOG               
396400     MOVE FSUM-SERVG-BTO(IX8)     TO  W001-DET16-SERVG-BTOH               
396500     MOVE TOT-SERVG-BTO           TO  W001-DET16-TOT                      
396600     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
396700     MOVE +1 TO W001-SKIP                                                 
396800     PERFORM S21-SKRIV-LISTA                                              
396900                                                                          
397000     MOVE FSUM-SERVG-NTO(IX1)     TO  W001-DET17-SERVG-NTOA               
397100     MOVE FSUM-SERVG-NTO(IX2)     TO  W001-DET17-SERVG-NTOB               
397200     MOVE FSUM-SERVG-NTO(IX3)     TO  W001-DET17-SERVG-NTOC               
397300     MOVE FSUM-SERVG-NTO(IX4)     TO  W001-DET17-SERVG-NTOD               
397400     MOVE FSUM-SERVG-NTO(IX5)     TO  W001-DET17-SERVG-NTOE               
397500     MOVE FSUM-SERVG-NTO(IX6)     TO  W001-DET17-SERVG-NTOF               
397600     MOVE FSUM-SERVG-NTO(IX7)     TO  W001-DET17-SERVG-NTOG               
397700     MOVE FSUM-SERVG-NTO(IX8)     TO  W001-DET17-SERVG-NTOH               
397800     MOVE TOT-SERVG-NTO           TO  W001-DET17-TOT                      
397900     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
398000     MOVE +1 TO W001-SKIP                                                 
398100     PERFORM S21-SKRIV-LISTA                                              
398200     .                                                                    
398300     EJECT                                                                
398400 Z-FINIT SECTION.                                                         
398500                                                                          
398600     CLOSE W23195                                                         
398700           W231PP                                                         
398800           W23198-001                                                     
398900                                                                          
399000     MOVE 'S' TO POSTSUM-OPKOD                                            
399100     CALL POSTSUM USING POSTSUM-PARM                                      
399200     .                                                                    
399300     EJECT                                                                
399400 S01-LAS-W23195 SECTION.                                                  
399500                                                                          
399600     READ W23195 INTO IN-AREA                                             
399700     AT END                                                               
399800         SET END-OF-W23195 TO TRUE                                        
399900                                                                          
400000     NOT AT END                                                           
400100        MOVE 'W23198'      TO POSTSUM-FDNAMN                              
400200        MOVE 'W23198D1'    TO POSTSUM-DDNAMN2                             
400300        MOVE SPACE         TO POSTSUM-TRANSTYP                            
400400        CALL POSTSUM USING POSTSUM-PARM                                   
400500     .                                                                    
400600     EJECT                                                                
400700 S02-LAS-W231PP SECTION.                                                  
400800                                                                          
400900     READ W231PP INTO PARM-AREA                                           
401000     AT END                                                               
401100         SET END-OF-W231PP TO TRUE                                        
401200                                                                          
401300     NOT AT END                                                           
401400        MOVE 'PARM  '      TO POSTSUM-FDNAMN                              
401500        MOVE 'W23198D4'    TO POSTSUM-DDNAMN2                             
401600        MOVE SPACE         TO POSTSUM-TRANSTYP                            
401700        CALL POSTSUM USING POSTSUM-PARM                                   
401800     .                                                                    
401900     EJECT                                                                
402000 S21-SKRIV-LISTA SECTION.                                                 
402100                                                                          
402200     WRITE W23198-001-RAD FROM W001-RAD AFTER W001-SKIP                   
402300                                                                          
402400     MOVE SPACE TO W001-RAD                                               
402500     ADD  +1 TO W001-ANTAL-RADER                                          
402600     .                                                                    
402700     EJECT                                                                
402800 S21A-SKRIV-RUBRIKER SECTION.                                             
402900                                                                          
403000     ADD +1 TO W001-SIDRAKNARE                                            
403100     MOVE W001-SIDRAKNARE TO W001-SID                                     
403200     WRITE W23198-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
403300     WRITE W23198-001-RAD FROM W001-RUBRIK3 AFTER 2                       
403400     MOVE +2 TO W001-ANTAL-RADER                                          
403500     MOVE +2 TO W001-SKIP                                                 
403600     .                                                                    
403700* --- IMS SEKTIONER ---                                                   
403800     SKIP3                                                                
403900                                                                          
404000 IMS-GET-WDB6      SECTION.                                               
404100                                                                          
404200     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B6                        
404300     MOVE '  GAGKGB'          TO GODK-STATUSKODER                         
404400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
404500     PERFORM IMS-STATUSKONTROLL                                           
404600     .                                                                    
404700     EJECT                                                                
404800 IMS-STATUSKONTROLL SECTION.                                              
404900                                                                          
405000     SET STATUS-IX TO 1                                                   
405100     SEARCH GODK-STATUS                                                   
405200       AT END                                                             
405300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
405400           DELIMITED BY SIZE INTO FELTEXT                                 
405500         DISPLAY FELTEXT                                                  
405600         CALL FELLOG                                                      
405700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
405800         CONTINUE                                                         
405900     END-SEARCH                                                           
406000     .                                                                    
