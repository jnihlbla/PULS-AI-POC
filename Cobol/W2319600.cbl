000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2319600.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   97/05/21.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      - LÄSER FIL W23195                                                 
001100*      - SKAPAR LISTA REFILL UPPFÖLJNING NDC                              
001200*                                                                         
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700     EJECT                                                                
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- GRUNDFIL ANALYS-LISTOR                                     
002100     SELECT W23195                     ASSIGN TO W23196D1.                
002200*          --- LISTA SOM FIL TILL DAP                                     
002300     SELECT W2319A                     ASSIGN TO W23196D3.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W23195                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200     SKIP2                                                                
003300*01  -COPY W23195        -L.                                              
003400     SKIP3                                                                
003500 FD  W2319A                                                               
003600     RECORDING       V                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900 01  W2319A-RAD                  PIC X(169).                              
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2317100'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  SW-KVOI-TRAFF               PIC X       VALUE 'N'.                   
004900 77  SW-ARTIKEL-SAKNAS-WDK7      PIC X       VALUE 'N'.                   
005000 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
005100 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
005200 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
005300 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
005400 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
005500 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
005700 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
005800 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
005900 77  NDC-IX                      PIC 9(2)    VALUE ZERO COMP-3.           
006000 77  KVOI-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  ART-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
006200 77  ART-IX-MAX                  PIC S9(3)   VALUE +72  COMP-3.           
006300 77  PSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  PSUM-IX-MAX                 PIC S9(3)   VALUE +9   COMP-3.           
006500 77  FSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  FSUM-IX-MAX                 PIC S9(3)   VALUE +8   COMP-3.           
006700 77  WS-KVAKS                    PIC S9(11)  VALUE ZERO COMP-3.           
006800 77  WS-KVLS-TOT                 PIC S9(11)  VALUE ZERO COMP-3.           
006900 77  WS-KVOKS-TOT                PIC S9(11)  VALUE ZERO COMP-3.           
007000 77  WS-KVOI                     PIC S9(11)V9(2)                          
007100                                   VALUE ZERO COMP-3.                     
007200 77  WS-KVOI-TOT-AAR             PIC S9(11)     VALUE ZERO COMP-3.        
007300 77  WS-KVOI-TOT-VECKA           PIC S9(11)     VALUE ZERO COMP-3.        
007400 77  WS-KVOI-TOT-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
007500 77  WS-KVOI-TEO-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
007600 77  WS-KVOT-SAKNAS-WDK7         PIC 9(9)       VALUE ZERO.               
007700 77  WS-KVOT-CDC-SAKNAS-WDK7     PIC 9(9)       VALUE ZERO.               
007800 77  WS-KVDISP                   PIC S9(11)     VALUE ZERO COMP-3.        
007900 77  WS-KVDISP-PR                PIC S9(11)V9(2)                          
008000                                   VALUE ZERO COMP-3.                     
008100 77  WS-SUMMA                    PIC S9(11)V9(2)                          
008200                                   VALUE ZERO COMP-3.                     
008300 77  WS-SUMMA-TOT                PIC S9(11)V9(2)                          
008400                                   VALUE ZERO COMP-3.                     
008500 77  WS-SUMMA-KR                 PIC S9(11)      VALUE ZERO.              
008600 77  WS-SERVG                    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
008700 77  WS-OLAGER                   PIC S9(11)      VALUE ZERO.              
008800 77  WS-MLAGER                   PIC S9(11)      VALUE ZERO.              
008900 77  WS-OMSHAST                  PIC 9(11)V9(1)  VALUE ZERO.              
009000 77  WS-PROC                     PIC S9(3)V9(1)  VALUE ZERO.              
009100 77  WS-KVPB-VECKA-SDC           PIC S9(6)V9(2)  VALUE ZERO.              
009200 77  WS-KVPB-DAG-SDC-NORM        PIC S9(6)V9(2)  VALUE ZERO.              
009300 77  WS-LT-BEHOV-SDC-NORM        PIC S9(7)V9(2)  VALUE ZERO.              
009400 77  WS-FIXAD-SUMMA              PIC S9(16)      VALUE ZERO.              
009500 77  WS-PREV-IDDC                PIC X(2)        VALUE SPACE.             
009600 77  FELTEXT                     PIC X(80)       VALUE SPACE.             
009700                                                                          
009800 77  W23195-EOF-SW               PIC X       VALUE 'N'.                   
009900     88  END-OF-W23195                       VALUE 'J'.                   
010000                                                                          
010100 01  DAGENS-DATUM                PIC 9(6).                                
010200 01  FILLER REDEFINES DAGENS-DATUM.                                       
010300     03  DAGENS-AAR              PIC 9(2).                                
010400     03  DAGENS-MAANAD           PIC 9(2).                                
010500     03  DAGENS-DAG              PIC 9(2).                                
010600                                                                          
010700 01  DAGENS-VECKA                PIC 9(4).                                
010800 01  FILLER REDEFINES DAGENS-VECKA.                                       
010900     03  D-VECKA-AAR             PIC 9(2).                                
011000     03  D-VECKA-VECKA           PIC 9(2).                                
011100                                                                          
011200 01  VECKOR.                                                              
011300     03  AAVVD                   PIC 9(5).                                
011400     03  FILLER REDEFINES AAVVD.                                          
011500         05  AAVV                PIC 9(4).                                
011600         05  D                   PIC 9(1).                                
011700                                                                          
011800     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
011900     EJECT                                                                
012000*      --- VALID IDDC CODES                                               
012100*                                                                         
012200*01    -COPY WWDC99                                                       
012300*                                                                         
012400*01    -COPY WWDCKONS                                                     
012500                                                                          
012600 01  W-IDDC-SEND                 PIC X(2).                                
012700 01  W-IDDC-REC                  PIC X(2).                                
012800 01  W-IDDC                      PIC X(2).                                
012900 01  W-IDLEVNR-DC                PIC X(5).                                
013000 01  WS-DC-TABELL.                                                        
013100     03 DC-TABELL OCCURS 500.                                             
013200        05  WS-IDDC-B601         PIC X(2) VALUE SPACE.                    
013300        05  WS-IDLEVNR-DC        PIC X(5) VALUE ZERO.                     
013400        05  WS-IDDC-B616         PIC X(2) VALUE SPACE.                    
013500        05  WS-KVDLTID-TOT       PIC 9(3) VALUE ZERO.                     
013600 01  IDDC-IX                     PIC 9(3).                                
013700 01  IDDC-IX-MAX                 PIC 9(3) VALUE 500.                      
013800                                                                          
013900       EJECT                                                              
014000 01  ART-TABELL.                                                          
014100     03 ART-RAD OCCURS 72.                                                
014200        05  ART-KVANT-AKT        PIC S9(9)      VALUE ZERO COMP-3.        
014300        05  ART-KVANT-PAS        PIC S9(9)      VALUE ZERO COMP-3.        
014400        05  ART-PROC-KVANT-A     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014500        05  ART-PROC-KVANT-P     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014600        05  ART-KVDISP-AKT       PIC S9(11)     VALUE ZERO COMP-3.        
014700        05  ART-PROC-KVDISP-A    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014800        05  ART-KVDISP-PAS       PIC S9(11)     VALUE ZERO COMP-3.        
014900        05  ART-PROC-KVDISP-P    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015000        05  ART-LS-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
015100        05  ART-PROC-LS-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015200        05  ART-LS-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
015300        05  ART-PROC-LS-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015400        05  ART-AK-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
015500        05  ART-PROC-AK-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015600        05  ART-AK-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
015700        05  ART-PROC-AK-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015800        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
015900        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016000        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
016100        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016200        05  ART-MLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
016300        05  ART-PROC-MLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016400        05  ART-KVOT             PIC S9(9)      VALUE ZERO COMP-3.        
016500        05  ART-PROC-KVOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016600        05  ART-SPLIT            PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016700        05  ART-OMSHAST-DISP     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016800        05  ART-OMSHAST-PROC-D   PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016900        05  ART-OMSHAST-LS       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017000        05  ART-OMSHAST-PROC-LS  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017100        05  ART-SERVG-BTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017200        05  ART-SERVG-NTO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017300*******  ARBETSFÄLT                                                       
017400        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
017500        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
017600        05  WS-ART-MLAGER        PIC S9(11)V9(2) VALUE ZERO.              
017700        05  WS-ART-LS-AKT        PIC S9(11)      VALUE ZERO.              
017800        05  WS-ART-LS-PAS        PIC S9(11)      VALUE ZERO.              
017900        05  WS-ART-LS-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
018000        05  WS-ART-LS-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
018100        05  WS-ART-KVLS-AKT      PIC S9(11)      VALUE ZERO.              
018200        05  WS-ART-KVLS-PAS      PIC S9(11)      VALUE ZERO.              
018300        05  WS-ART-KVDISP-AKT    PIC S9(11)      VALUE ZERO.              
018400        05  WS-ART-KVDISP-PAS    PIC S9(11)      VALUE ZERO.              
018500        05  WS-ART-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.              
018600        05  WS-ART-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.              
018700        05  WS-ART-KVOKS-AKT     PIC S9(11)      VALUE ZERO.              
018800        05  WS-ART-KVOKS-PAS     PIC S9(11)      VALUE ZERO.              
018900        05  WS-ART-OK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
019000        05  WS-ART-OK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
019100        05  WS-ART-KVAKS-AKT     PIC S9(11)      VALUE ZERO.              
019200        05  WS-ART-KVAKS-PAS     PIC S9(11)      VALUE ZERO.              
019300        05  WS-ART-AK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
019400        05  WS-ART-AK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
019500        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
019600        05  WS-ART-KVOI-AKT      PIC S9(9)      VALUE ZERO COMP-3.        
019700        05  WS-ART-KVOI-PAS      PIC S9(9)      VALUE ZERO COMP-3.        
019800        05  WS-ART-KVOI-TEO      PIC S9(9)      VALUE ZERO COMP-3.        
019900        05  WS-ART-KVOI-SAK      PIC S9(9)      VALUE ZERO COMP-3.        
020000        05  WS-ART-KVOI-CDC-AKT  PIC S9(9)      VALUE ZERO COMP-3.        
020100        05  WS-ART-KVOI-CDC-PAS  PIC S9(9)      VALUE ZERO COMP-3.        
020200        05  WS-ART-KVOI-CDC-TEO  PIC S9(9)      VALUE ZERO COMP-3.        
020300        05  WS-ART-KVOI-CDC-SAK  PIC S9(9)      VALUE ZERO COMP-3.        
020400        05  WS-ART-SUINKORD      PIC S9(16)V9(2)                          
020500                                                VALUE ZERO COMP-3.        
020600        05  WS-ART-SUFYSAVP      PIC S9(16)V9(2)                          
020700                                                VALUE ZERO COMP-3.        
020800        05  WS-ART-SUAVBRP       PIC S9(16)V9(2)                          
020900                                                VALUE ZERO COMP-3.        
021000        05  WS-ART-SULAGERB      PIC S9(16)V9(2)                          
021100                                                VALUE ZERO COMP-3.        
021200        05  WS-ART-SUSORTB       PIC S9(16)V9(2)                          
021300                                                VALUE ZERO COMP-3.        
021400     EJECT                                                                
021500 01  PSUM-TABELL.                                                         
021600     03 PSUM-RAD OCCURS 9.                                                
021700        05  PSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
021800        05  PSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
021900        05  PSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022000        05  PSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022100        05  PSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
022200        05  PSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022300        05  PSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
022400        05  PSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022500        05  PSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
022600        05  PSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022700        05  PSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
022800        05  PSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022900        05  PSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
023000        05  PSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023100        05  PSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
023200        05  PSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023300        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
023400        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023500        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
023600        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023700        05  PSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
023800        05  PSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023900        05  PSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
024000        05  PSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024100        05  PSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024200        05  PSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024300        05  PSUM-OMSHAST-PROC-D  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
024400        05  PSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024500        05  PSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
024600        05  PSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024700        05  PSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024800*******  ARBETSFÄLT                                                       
024900        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
025000        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
025100        05  WS-PSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
025200        05  WS-PSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
025300        05  WS-PSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
025400        05  WS-PSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
025500        05  WS-PSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
025600        05  WS-PSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
025700        05  WS-PSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
025800        05  WS-PSUM-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.             
025900        05  WS-PSUM-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.             
026000        05  WS-PSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
026100        05  WS-PSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
026200        05  WS-PSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
026300        05  WS-PSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
026400        05  WS-PSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
026500        05  WS-PSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
026600        05  WS-PSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
026700        05  WS-PSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
026800        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
026900        05  WS-PSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
027000        05  WS-PSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
027100        05  WS-PSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
027200        05  WS-PSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
027300        05  WS-PSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
027400        05  WS-PSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
027500        05  WS-PSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
027600        05  WS-PSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
027700        05  WS-PSUM-SUINKORD     PIC S9(16)V9(2)                          
027800                                                VALUE ZERO COMP-3.        
027900        05  WS-PSUM-SUFYSAVP     PIC S9(16)V9(2)                          
028000                                                VALUE ZERO COMP-3.        
028100        05  WS-PSUM-SUAVBRP      PIC S9(16)V9(2)                          
028200                                                VALUE ZERO COMP-3.        
028300        05  WS-PSUM-SULAGERB     PIC S9(16)V9(2)                          
028400                                                VALUE ZERO COMP-3.        
028500        05  WS-PSUM-SUSORTB      PIC S9(16)V9(2)                          
028600                                                VALUE ZERO COMP-3.        
028700     EJECT                                                                
028800 01  FSUM-TABELL.                                                         
028900     03 FSUM-RAD OCCURS 8.                                                
029000        05  FSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
029100        05  FSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
029200        05  FSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029300        05  FSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029400        05  FSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
029500        05  FSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029600        05  FSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
029700        05  FSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029800        05  FSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
029900        05  FSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030000        05  FSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
030100        05  FSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030200        05  FSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
030300        05  FSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030400        05  FSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
030500        05  FSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030600        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
030700        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030800        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
030900        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031000        05  FSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
031100        05  FSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031200        05  FSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
031300        05  FSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031400        05  FSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031500        05  FSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031600        05  FSUM-OMSHAST-PROC-D PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031700        05  FSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031800        05  FSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
031900        05  FSUM-SERVG-BTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032000        05  FSUM-SERVG-NTO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
032100*******  ARBETSFÄLT                                                       
032200        05  WS-FSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
032300        05  WS-FSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
032400        05  WS-FSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
032500        05  WS-FSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
032600        05  WS-FSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
032700        05  WS-FSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032800        05  WS-FSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
032900        05  WS-FSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
033000        05  WS-FSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
033100        05  WS-FSUM-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.            
033200        05  WS-FSUM-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.            
033300        05  WS-FSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
033400        05  WS-FSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
033500        05  WS-FSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
033600        05  WS-FSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
033700        05  WS-FSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
033800        05  WS-FSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
033900        05  WS-FSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
034000        05  WS-FSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
034100        05  WS-FSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
034200        05  WS-FSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
034300        05  WS-FSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
034400        05  WS-FSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
034500        05  WS-FSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
034600        05  WS-FSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
034700        05  WS-FSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
034800        05  WS-FSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
034900        05  WS-FSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
035000        05  WS-FSUM-SUINKORD     PIC S9(16)V9(2)                          
035100                                                VALUE ZERO COMP-3.        
035200        05  WS-FSUM-SUFYSAVP     PIC S9(16)V9(2)                          
035300                                                VALUE ZERO COMP-3.        
035400        05  WS-FSUM-SUAVBRP      PIC S9(16)V9(2)                          
035500                                                VALUE ZERO COMP-3.        
035600        05  WS-FSUM-SULAGERB     PIC S9(16)V9(2)                          
035700                                                VALUE ZERO COMP-3.        
035800        05  WS-FSUM-SUSORTB      PIC S9(16)V9(2)                          
035900                                                VALUE ZERO COMP-3.        
036000     EJECT                                                                
036100 01  TOTAL-RUTA.                                                          
036200     03  TOT-KVANT-AKT          PIC S9(9)      VALUE ZERO COMP-3.         
036300     03  TOT-KVANT-PAS          PIC S9(9)      VALUE ZERO COMP-3.         
036400     03  TOT-KVDISP-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
036500     03  TOT-KVDISP-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
036600     03  TOT-LS-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
036700     03  TOT-LS-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
036800     03  TOT-AK-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
036900     03  TOT-AK-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
037000     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
037100     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
037200     03  TOT-MLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
037300     03  TOT-PROC-MLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
037400     03  TOT-KVOT               PIC S9(9)      VALUE ZERO COMP-3.         
037500     03  TOT-PROC-KVOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
037600     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
037700     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
037800     03  TOT-SPLIT              PIC S9(7)V9(1) VALUE ZERO COMP-3.         
037900     03  TOT-OMSHAST-DISP       PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038000     03  TOT-OMSHAST-LS         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038100     03  TOT-SERVG-BTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038200     03  TOT-SERVG-NTO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
038300*******  ARBETSFÄLT                                                       
038400     03  WS-TOT-SLAGER          PIC S9(11)V9(2) VALUE ZERO.               
038500     03  WS-TOT-OLAGER          PIC S9(11)V9(2) VALUE ZERO.               
038600     03  WS-TOT-MLAGER          PIC S9(11)V9(2) VALUE ZERO.               
038700     03  WS-TOT-LS-AKT          PIC S9(11)      VALUE ZERO.               
038800     03  WS-TOT-LS-PAS          PIC S9(11)      VALUE ZERO.               
038900     03  WS-TOT-LS-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
039000     03  WS-TOT-LS-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
039100     03  WS-TOT-KVDISP-AKT      PIC S9(11)      VALUE ZERO.               
039200     03  WS-TOT-KVDISP-PAS      PIC S9(11)      VALUE ZERO.               
039300     03  WS-TOT-KVDISP-PR-AKT   PIC S9(11)V9(2) VALUE ZERO.               
039400     03  WS-TOT-KVDISP-PR-PAS   PIC S9(11)V9(2) VALUE ZERO.               
039500     03  WS-TOT-KVOKS-AKT       PIC S9(11)      VALUE ZERO.               
039600     03  WS-TOT-KVOKS-PAS       PIC S9(11)      VALUE ZERO.               
039700     03  WS-TOT-OK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
039800     03  WS-TOT-OK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
039900     03  WS-TOT-KVAKS-AKT       PIC S9(11)      VALUE ZERO.               
040000     03  WS-TOT-KVAKS-PAS       PIC S9(11)      VALUE ZERO.               
040100     03  WS-TOT-AK-PR-AKT       PIC S9(11)V9(2) VALUE ZERO.               
040200     03  WS-TOT-AK-PR-PAS       PIC S9(11)V9(2) VALUE ZERO.               
040300     03  WS-TOT-KVOI            PIC S9(11)V9(2) VALUE ZERO.               
040400     03  WS-TOT-KVOI-AKT        PIC S9(9)       VALUE ZERO.               
040500     03  WS-TOT-KVOI-PAS        PIC S9(9)       VALUE ZERO.               
040600     03  WS-TOT-KVOI-TEO        PIC S9(9)       VALUE ZERO.               
040700     03  WS-TOT-KVOI-SAK        PIC S9(9)       VALUE ZERO.               
040800     03  WS-TOT-KVOI-CDC-AKT    PIC S9(9)       VALUE ZERO.               
040900     03  WS-TOT-KVOI-CDC-PAS    PIC S9(9)       VALUE ZERO.               
041000     03  WS-TOT-KVOI-CDC-TEO    PIC S9(9)       VALUE ZERO.               
041100     03  WS-TOT-KVOI-CDC-SAK    PIC S9(9)       VALUE ZERO.               
041200     03  WS-TOT-SUINKORD        PIC S9(16)V9(2)                           
041300                                                VALUE ZERO COMP-3.        
041400     03  WS-TOT-SUFYSAVP        PIC S9(16)V9(2)                           
041500                                                VALUE ZERO COMP-3.        
041600     03  WS-TOT-SUAVBRP         PIC S9(16)V9(2)                           
041700                                                VALUE ZERO COMP-3.        
041800     03  WS-TOT-SULAGERB        PIC S9(16)V9(2)                           
041900                                                VALUE ZERO COMP-3.        
042000     03  WS-TOT-SUSORTB         PIC S9(16)V9(2)                           
042100                                                VALUE ZERO COMP-3.        
042200     EJECT                                                                
042300 01  DYNAMISKA-SUBPROGRAM.                                                
042400*                                                                         
042500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
042600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
042700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
042800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
042900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
043000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
043100                                                                          
043200     EJECT                                                                
043300 01  PARAM-TILL-DATKORT.                                                  
043400     03  PROG-ID                 PIC X(8)    VALUE 'W2319600'.            
043500     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
043600*03  -COPY WDATKORT                                                       
043700     EJECT                                                                
043800*03  -COPY WDATAREA                                                       
043900     EJECT                                                                
044000*    --- PARAMETRAR TILL POSTSUM                                          
044100*                                                                         
044200*01  -COPY W0005   -PRE  POSTSUM-                                         
044300     EJECT                                                                
044400 01  IN-AREA-START               PIC X(24)   VALUE                        
044500                                 'IN-AREA-START    '.                     
044600*01  AREA  -COPY W23195     -PRE IN-                                      
044700     EJECT                                                                
044800 01  W001-AREA-START             PIC X(24)   VALUE                        
044900                                 'W001-AREA-START  '.                     
045000     SKIP2                                                                
045100 01  W001-HJALPAREOR.                                                     
045200*                                                                         
045300     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
045400     03  W001-ANTAL-RADER                                                 
045500                                 PIC 9(3)    VALUE 999.                   
045600     03  W001-MAX-RADER-PER-SIDA                                          
045700                                 PIC 9(3)    VALUE 63.                    
045800     03  W001-MAX-POSITIONER-PER-RAD                                      
045900                                 PIC 9(3)    VALUE 165.                   
046000     03  W001-LISTNR             PIC X(11)   VALUE SPACE.                 
046100     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
046200     SKIP2                                                                
046300 01  W001-RAD.                                                            
046400     03  FILLER                  PIC X(165)  VALUE SPACE.                 
046500     EJECT                                                                
046600 01  W001-DAP.                                                            
046700     03  FILLER                  PIC X(165)  VALUE SPACE.                 
046800     EJECT                                                                
046900 01  W001-RUBRIK1.                                                        
047000*                                                                         
047100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
047200     03  FILLER                  PIC X(18)                                
047300                              VALUE 'VOLVO CAR PARTS   '.                 
047400     03  W001-LISTID             PIC X(12)                                
047500                                 VALUE SPACE.                             
047600     03  FILLER                  PIC X(20)                                
047700             VALUE 'FOLLOW-UP REFILL    '.                                
047800     03  FILLER                  PIC X(11)   VALUE SPACE.                 
047900     03  FILLER                  PIC X(4)    VALUE 'NDC '.                
048000     03  FILLER                  PIC X(2)    VALUE SPACE.                 
048100     03  W001-AKTUELLT-IDDC      PIC X(2)    VALUE SPACE.                 
048200     03  FILLER                  PIC X(6)    VALUE SPACE.                 
048300     03  FILLER                  PIC X(6)    VALUE 'WEEK  '.              
048400     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
048500     03  FILLER                  PIC X(37)   VALUE SPACE.                 
048600     03  W001-DATUM              PIC XXBXXBXX.                            
048700     03  FILLER                  PIC X(6)    VALUE SPACE.                 
048800     03  FILLER                  PIC X(5)    VALUE 'PAGE '.               
048900     03  W001-SID                PIC Z(4)9.                               
049000     EJECT                                                                
049100 01  W001-RUBRIK3.                                                        
049200*                                                                         
049300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
049400     03  FILLER                  PIC X(11)                                
049500                             VALUE 'PRICE CLASS'.                         
049600     03  FILLER                  PIC X(11)   VALUE SPACE.                 
049700     03  FILLER                  PIC X(7)    VALUE 'A      '.             
049800     03  FILLER                  PIC X(7)    VALUE SPACE.                 
049900     03  FILLER                  PIC X(8)    VALUE 'B       '.            
050000     03  FILLER                  PIC X(6)    VALUE SPACE.                 
050100     03  FILLER                  PIC X(8)    VALUE 'C       '.            
050200     03  FILLER                  PIC X(6)    VALUE SPACE.                 
050300     03  FILLER                  PIC X(9)    VALUE 'D        '.           
050400     03  FILLER                  PIC X(5)    VALUE SPACE.                 
050500     03  FILLER                  PIC X(9)    VALUE 'E        '.           
050600     03  FILLER                  PIC X(5)    VALUE SPACE.                 
050700     03  FILLER                  PIC X(10)   VALUE 'F         '.          
050800     03  FILLER                  PIC X(4)    VALUE SPACE.                 
050900     03  FILLER                  PIC X(10)   VALUE 'G         '.          
051000     03  FILLER                  PIC X(4)    VALUE SPACE.                 
051100     03  FILLER                  PIC X(10)   VALUE 'H         '.          
051200     03  FILLER                  PIC X(11)   VALUE SPACE.                 
051300     03  FILLER                  PIC X(6)    VALUE 'TOTAL '.              
051400     EJECT                                                                
051500 01  W001-DETALJRAD-1.                                                    
051600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
051700     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
051800     03  FILLER                  PIC X       VALUE SPACE.                 
051900     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  A'.        
052000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052100     03  W001-DET1-KVANTA        PIC Z(7)9.                               
052200     03  FILLER                  PIC X       VALUE SPACE.                 
052300     03  W001-DET1-P-KVANTA      PIC Z9.9.                                
052400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052500     03  W001-DET1-KVANTB        PIC Z(7)9.                               
052600     03  FILLER                  PIC X       VALUE SPACE.                 
052700     03  W001-DET1-P-KVANTB      PIC Z9.9.                                
052800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052900     03  W001-DET1-KVANTC        PIC Z(7)9.                               
053000     03  FILLER                  PIC X       VALUE SPACE.                 
053100     03  W001-DET1-P-KVANTC      PIC Z9.9.                                
053200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053300     03  W001-DET1-KVANTD        PIC Z(7)9.                               
053400     03  FILLER                  PIC X       VALUE SPACE.                 
053500     03  W001-DET1-P-KVANTD      PIC Z9.9.                                
053600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053700     03  W001-DET1-KVANTE        PIC Z(7)9.                               
053800     03  FILLER                  PIC X       VALUE SPACE.                 
053900     03  W001-DET1-P-KVANTE      PIC Z9.9.                                
054000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054100     03  W001-DET1-KVANTF        PIC Z(7)9.                               
054200     03  FILLER                  PIC X       VALUE SPACE.                 
054300     03  W001-DET1-P-KVANTF      PIC Z9.9.                                
054400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054500     03  W001-DET1-KVANTG        PIC Z(7)9.                               
054600     03  FILLER                  PIC X       VALUE SPACE.                 
054700     03  W001-DET1-P-KVANTG      PIC Z9.9.                                
054800     03  FILLER                  PIC X       VALUE SPACE.                 
054900     03  W001-DET1-KVANTH        PIC Z(7)9.                               
055000     03  FILLER                  PIC X       VALUE SPACE.                 
055100     03  W001-DET1-P-KVANTH      PIC Z9.9.                                
055200     03  FILLER                  PIC X       VALUE SPACE.                 
055300     03  W001-DET1-TOT           PIC Z(13)9.                              
055400     03  FILLER                  PIC X       VALUE SPACE.                 
055500     03  W001-DET1-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
055600     EJECT                                                                
055700 01  W001-DETALJRAD-2.                                                    
055800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055900     03  W001-DET2-PRISKLASS     PIC X       VALUE SPACE.                 
056000     03  FILLER                  PIC X       VALUE SPACE.                 
056100     03  FILLER                  PIC X(12)   VALUE 'QTY PARTS  P'.        
056200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056300     03  W001-DET2-KVANTA        PIC Z(7)9.                               
056400     03  FILLER                  PIC X       VALUE SPACE.                 
056500     03  W001-DET2-P-KVANTA      PIC Z9.9.                                
056600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056700     03  W001-DET2-KVANTB        PIC Z(7)9.                               
056800     03  FILLER                  PIC X       VALUE SPACE.                 
056900     03  W001-DET2-P-KVANTB      PIC Z9.9.                                
057000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057100     03  W001-DET2-KVANTC        PIC Z(7)9.                               
057200     03  FILLER                  PIC X       VALUE SPACE.                 
057300     03  W001-DET2-P-KVANTC      PIC Z9.9.                                
057400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057500     03  W001-DET2-KVANTD        PIC Z(7)9.                               
057600     03  FILLER                  PIC X       VALUE SPACE.                 
057700     03  W001-DET2-P-KVANTD      PIC Z9.9.                                
057800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057900     03  W001-DET2-KVANTE        PIC Z(7)9.                               
058000     03  FILLER                  PIC X       VALUE SPACE.                 
058100     03  W001-DET2-P-KVANTE      PIC Z9.9.                                
058200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058300     03  W001-DET2-KVANTF        PIC Z(7)9.                               
058400     03  FILLER                  PIC X       VALUE SPACE.                 
058500     03  W001-DET2-P-KVANTF      PIC Z9.9.                                
058600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058700     03  W001-DET2-KVANTG        PIC Z(7)9.                               
058800     03  FILLER                  PIC X       VALUE SPACE.                 
058900     03  W001-DET2-P-KVANTG      PIC Z9.9.                                
059000     03  FILLER                  PIC X       VALUE SPACE.                 
059100     03  W001-DET2-KVANTH        PIC Z(7)9.                               
059200     03  FILLER                  PIC X       VALUE SPACE.                 
059300     03  W001-DET2-P-KVANTH      PIC Z9.9.                                
059400     03  FILLER                  PIC X       VALUE SPACE.                 
059500     03  W001-DET2-TOT           PIC Z(13)9.                              
059600     03  FILLER                  PIC X       VALUE SPACE.                 
059700     03  W001-DET2-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
059800     EJECT                                                                
059900 01  W001-DETALJRAD-3.                                                    
060000     03  FILLER                  PIC X(3)    VALUE SPACE.                 
060100     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND A'.        
060200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060300     03  W001-DET3-DLAGERA       PIC Z(7)9.                               
060400     03  FILLER                  PIC X       VALUE SPACE.                 
060500     03  W001-DET3-P-DLAGERA     PIC Z9.9.                                
060600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060700     03  W001-DET3-DLAGERB       PIC Z(7)9.                               
060800     03  FILLER                  PIC X       VALUE SPACE.                 
060900     03  W001-DET3-P-DLAGERB     PIC Z9.9.                                
061000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061100     03  W001-DET3-DLAGERC       PIC Z(7)9.                               
061200     03  FILLER                  PIC X       VALUE SPACE.                 
061300     03  W001-DET3-P-DLAGERC     PIC Z9.9.                                
061400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061500     03  W001-DET3-DLAGERD       PIC Z(7)9.                               
061600     03  FILLER                  PIC X       VALUE SPACE.                 
061700     03  W001-DET3-P-DLAGERD     PIC Z9.9.                                
061800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061900     03  W001-DET3-DLAGERE       PIC Z(7)9.                               
062000     03  FILLER                  PIC X       VALUE SPACE.                 
062100     03  W001-DET3-P-DLAGERE     PIC Z9.9.                                
062200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062300     03  W001-DET3-DLAGERF       PIC Z(7)9.                               
062400     03  FILLER                  PIC X       VALUE SPACE.                 
062500     03  W001-DET3-P-DLAGERF     PIC Z9.9.                                
062600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062700     03  W001-DET3-DLAGERG       PIC Z(7)9.                               
062800     03  FILLER                  PIC X       VALUE SPACE.                 
062900     03  W001-DET3-P-DLAGERG     PIC Z9.9.                                
063000     03  FILLER                  PIC X       VALUE SPACE.                 
063100     03  W001-DET3-DLAGERH       PIC Z(7)9.                               
063200     03  FILLER                  PIC X       VALUE SPACE.                 
063300     03  W001-DET3-P-DLAGERH     PIC Z9.9.                                
063400     03  FILLER                  PIC X       VALUE SPACE.                 
063500     03  W001-DET3-TOT           PIC Z(13)9.                              
063600     03  FILLER                  PIC X       VALUE SPACE.                 
063700     03  W001-DET3-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
063800 01  W001-DETALJRAD-4.                                                    
063900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
064000     03  FILLER                  PIC X(12)   VALUE 'ST ON HAND P'.        
064100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064200     03  W001-DET4-DLAGERA       PIC Z(7)9.                               
064300     03  FILLER                  PIC X       VALUE SPACE.                 
064400     03  W001-DET4-P-DLAGERA     PIC Z9.9.                                
064500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064600     03  W001-DET4-DLAGERB       PIC Z(7)9.                               
064700     03  FILLER                  PIC X       VALUE SPACE.                 
064800     03  W001-DET4-P-DLAGERB     PIC Z9.9.                                
064900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065000     03  W001-DET4-DLAGERC       PIC Z(7)9.                               
065100     03  FILLER                  PIC X       VALUE SPACE.                 
065200     03  W001-DET4-P-DLAGERC     PIC Z9.9.                                
065300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065400     03  W001-DET4-DLAGERD       PIC Z(7)9.                               
065500     03  FILLER                  PIC X       VALUE SPACE.                 
065600     03  W001-DET4-P-DLAGERD     PIC Z9.9.                                
065700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065800     03  W001-DET4-DLAGERE       PIC Z(7)9.                               
065900     03  FILLER                  PIC X       VALUE SPACE.                 
066000     03  W001-DET4-P-DLAGERE     PIC Z9.9.                                
066100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066200     03  W001-DET4-DLAGERF       PIC Z(7)9.                               
066300     03  FILLER                  PIC X       VALUE SPACE.                 
066400     03  W001-DET4-P-DLAGERF     PIC Z9.9.                                
066500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066600     03  W001-DET4-DLAGERG       PIC Z(7)9.                               
066700     03  FILLER                  PIC X       VALUE SPACE.                 
066800     03  W001-DET4-P-DLAGERG     PIC Z9.9.                                
066900     03  FILLER                  PIC X       VALUE SPACE.                 
067000     03  W001-DET4-DLAGERH       PIC Z(7)9.                               
067100     03  FILLER                  PIC X       VALUE SPACE.                 
067200     03  W001-DET4-P-DLAGERH     PIC Z9.9.                                
067300     03  FILLER                  PIC X       VALUE SPACE.                 
067400     03  W001-DET4-TOT           PIC Z(13)9.                              
067500     03  FILLER                  PIC X       VALUE SPACE.                 
067600     03  W001-DET4-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
067700     EJECT                                                                
067800 01  W001-DETALJRAD-5.                                                    
067900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
068000     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. A'.        
068100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068200     03  W001-DET5-LLAGERA       PIC Z(7)9.                               
068300     03  FILLER                  PIC X       VALUE SPACE.                 
068400     03  W001-DET5-P-LLAGERA     PIC Z9.9.                                
068500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068600     03  W001-DET5-LLAGERB       PIC Z(7)9.                               
068700     03  FILLER                  PIC X       VALUE SPACE.                 
068800     03  W001-DET5-P-LLAGERB     PIC Z9.9.                                
068900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069000     03  W001-DET5-LLAGERC       PIC Z(7)9.                               
069100     03  FILLER                  PIC X       VALUE SPACE.                 
069200     03  W001-DET5-P-LLAGERC     PIC Z9.9.                                
069300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069400     03  W001-DET5-LLAGERD       PIC Z(7)9.                               
069500     03  FILLER                  PIC X       VALUE SPACE.                 
069600     03  W001-DET5-P-LLAGERD     PIC Z9.9.                                
069700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069800     03  W001-DET5-LLAGERE       PIC Z(7)9.                               
069900     03  FILLER                  PIC X       VALUE SPACE.                 
070000     03  W001-DET5-P-LLAGERE     PIC Z9.9.                                
070100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070200     03  W001-DET5-LLAGERF       PIC Z(7)9.                               
070300     03  FILLER                  PIC X       VALUE SPACE.                 
070400     03  W001-DET5-P-LLAGERF     PIC Z9.9.                                
070500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070600     03  W001-DET5-LLAGERG       PIC Z(7)9.                               
070700     03  FILLER                  PIC X       VALUE SPACE.                 
070800     03  W001-DET5-P-LLAGERG     PIC Z9.9.                                
070900     03  FILLER                  PIC X       VALUE SPACE.                 
071000     03  W001-DET5-LLAGERH       PIC Z(7)9.                               
071100     03  FILLER                  PIC X       VALUE SPACE.                 
071200     03  W001-DET5-P-LLAGERH     PIC Z9.9.                                
071300     03  FILLER                  PIC X       VALUE SPACE.                 
071400     03  W001-DET5-TOT           PIC Z(13)9.                              
071500     03  FILLER                  PIC X       VALUE SPACE.                 
071600     03  W001-DET5-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
071700     EJECT                                                                
071800 01  W001-DETALJRAD-6.                                                    
071900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
072000     03  FILLER                  PIC X(12)   VALUE 'STOCK BAL. P'.        
072100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072200     03  W001-DET6-LLAGERA       PIC Z(7)9.                               
072300     03  FILLER                  PIC X       VALUE SPACE.                 
072400     03  W001-DET6-P-LLAGERA     PIC Z9.9.                                
072500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072600     03  W001-DET6-LLAGERB       PIC Z(7)9.                               
072700     03  FILLER                  PIC X       VALUE SPACE.                 
072800     03  W001-DET6-P-LLAGERB     PIC Z9.9.                                
072900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073000     03  W001-DET6-LLAGERC       PIC Z(7)9.                               
073100     03  FILLER                  PIC X       VALUE SPACE.                 
073200     03  W001-DET6-P-LLAGERC     PIC Z9.9.                                
073300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073400     03  W001-DET6-LLAGERD       PIC Z(7)9.                               
073500     03  FILLER                  PIC X       VALUE SPACE.                 
073600     03  W001-DET6-P-LLAGERD     PIC Z9.9.                                
073700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073800     03  W001-DET6-LLAGERE       PIC Z(7)9.                               
073900     03  FILLER                  PIC X       VALUE SPACE.                 
074000     03  W001-DET6-P-LLAGERE     PIC Z9.9.                                
074100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074200     03  W001-DET6-LLAGERF       PIC Z(7)9.                               
074300     03  FILLER                  PIC X       VALUE SPACE.                 
074400     03  W001-DET6-P-LLAGERF     PIC Z9.9.                                
074500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074600     03  W001-DET6-LLAGERG       PIC Z(7)9.                               
074700     03  FILLER                  PIC X       VALUE SPACE.                 
074800     03  W001-DET6-P-LLAGERG     PIC Z9.9.                                
074900     03  FILLER                  PIC X       VALUE SPACE.                 
075000     03  W001-DET6-LLAGERH       PIC Z(7)9.                               
075100     03  FILLER                  PIC X       VALUE SPACE.                 
075200     03  W001-DET6-P-LLAGERH     PIC Z9.9.                                
075300     03  FILLER                  PIC X       VALUE SPACE.                 
075400     03  W001-DET6-TOT           PIC Z(13)9.                              
075500     03  FILLER                  PIC X       VALUE SPACE.                 
075600     03  W001-DET6-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
075700     EJECT                                                                
075800 01  W001-DETALJRAD-7.                                                    
075900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
076000     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   A'.        
076100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076200     03  W001-DET7-ALAGERA       PIC Z(7)9.                               
076300     03  FILLER                  PIC X       VALUE SPACE.                 
076400     03  W001-DET7-P-ALAGERA     PIC Z9.9.                                
076500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076600     03  W001-DET7-ALAGERB       PIC Z(7)9.                               
076700     03  FILLER                  PIC X       VALUE SPACE.                 
076800     03  W001-DET7-P-ALAGERB     PIC Z9.9.                                
076900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077000     03  W001-DET7-ALAGERC       PIC Z(7)9.                               
077100     03  FILLER                  PIC X       VALUE SPACE.                 
077200     03  W001-DET7-P-ALAGERC     PIC Z9.9.                                
077300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077400     03  W001-DET7-ALAGERD       PIC Z(7)9.                               
077500     03  FILLER                  PIC X       VALUE SPACE.                 
077600     03  W001-DET7-P-ALAGERD     PIC Z9.9.                                
077700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077800     03  W001-DET7-ALAGERE       PIC Z(7)9.                               
077900     03  FILLER                  PIC X       VALUE SPACE.                 
078000     03  W001-DET7-P-ALAGERE     PIC Z9.9.                                
078100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078200     03  W001-DET7-ALAGERF       PIC Z(7)9.                               
078300     03  FILLER                  PIC X       VALUE SPACE.                 
078400     03  W001-DET7-P-ALAGERF     PIC Z9.9.                                
078500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078600     03  W001-DET7-ALAGERG       PIC Z(7)9.                               
078700     03  FILLER                  PIC X       VALUE SPACE.                 
078800     03  W001-DET7-P-ALAGERG     PIC Z9.9.                                
078900     03  FILLER                  PIC X       VALUE SPACE.                 
079000     03  W001-DET7-ALAGERH       PIC Z(7)9.                               
079100     03  FILLER                  PIC X       VALUE SPACE.                 
079200     03  W001-DET7-P-ALAGERH     PIC Z9.9.                                
079300     03  FILLER                  PIC X       VALUE SPACE.                 
079400     03  W001-DET7-TOT           PIC Z(13)9.                              
079500     03  FILLER                  PIC X       VALUE SPACE.                 
079600     03  W001-DET7-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
079700     EJECT                                                                
079800 01  W001-DETALJRAD-8.                                                    
079900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
080000     03  FILLER                  PIC X(12)   VALUE 'QTY ADV.   P'.        
080100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080200     03  W001-DET8-ALAGERA       PIC Z(7)9.                               
080300     03  FILLER                  PIC X       VALUE SPACE.                 
080400     03  W001-DET8-P-ALAGERA     PIC Z9.9.                                
080500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080600     03  W001-DET8-ALAGERB       PIC Z(7)9.                               
080700     03  FILLER                  PIC X       VALUE SPACE.                 
080800     03  W001-DET8-P-ALAGERB     PIC Z9.9.                                
080900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081000     03  W001-DET8-ALAGERC       PIC Z(7)9.                               
081100     03  FILLER                  PIC X       VALUE SPACE.                 
081200     03  W001-DET8-P-ALAGERC     PIC Z9.9.                                
081300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081400     03  W001-DET8-ALAGERD       PIC Z(7)9.                               
081500     03  FILLER                  PIC X       VALUE SPACE.                 
081600     03  W001-DET8-P-ALAGERD     PIC Z9.9.                                
081700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081800     03  W001-DET8-ALAGERE       PIC Z(7)9.                               
081900     03  FILLER                  PIC X       VALUE SPACE.                 
082000     03  W001-DET8-P-ALAGERE     PIC Z9.9.                                
082100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082200     03  W001-DET8-ALAGERF       PIC Z(7)9.                               
082300     03  FILLER                  PIC X       VALUE SPACE.                 
082400     03  W001-DET8-P-ALAGERF     PIC Z9.9.                                
082500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082600     03  W001-DET8-ALAGERG       PIC Z(7)9.                               
082700     03  FILLER                  PIC X       VALUE SPACE.                 
082800     03  W001-DET8-P-ALAGERG     PIC Z9.9.                                
082900     03  FILLER                  PIC X       VALUE SPACE.                 
083000     03  W001-DET8-ALAGERH       PIC Z(7)9.                               
083100     03  FILLER                  PIC X       VALUE SPACE.                 
083200     03  W001-DET8-P-ALAGERH     PIC Z9.9.                                
083300     03  FILLER                  PIC X       VALUE SPACE.                 
083400     03  W001-DET8-TOT           PIC Z(13)9.                              
083500     03  FILLER                  PIC X       VALUE SPACE.                 
083600     03  W001-DET8-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
083700     EJECT                                                                
083800 01  W001-DETALJRAD-9.                                                    
083900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
084000     03  FILLER                  PIC X(12)   VALUE 'OVERSTOCK   '.        
084100     03  FILLER                  PIC X       VALUE SPACE.                 
084200     03  W001-DET9-OLAGERA       PIC Z(7)9.                               
084300     03  FILLER                  PIC X       VALUE SPACE.                 
084400     03  W001-DET9-P-OLAGERA     PIC Z9.9    BLANK WHEN ZERO.             
084500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084600     03  W001-DET9-OLAGERB       PIC Z(7)9.                               
084700     03  FILLER                  PIC X       VALUE SPACE.                 
084800     03  W001-DET9-P-OLAGERB     PIC Z9.9    BLANK WHEN ZERO.             
084900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085000     03  W001-DET9-OLAGERC       PIC Z(7)9.                               
085100     03  FILLER                  PIC X       VALUE SPACE.                 
085200     03  W001-DET9-P-OLAGERC     PIC Z9.9    BLANK WHEN ZERO.             
085300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085400     03  W001-DET9-OLAGERD       PIC Z(7)9.                               
085500     03  FILLER                  PIC X       VALUE SPACE.                 
085600     03  W001-DET9-P-OLAGERD     PIC Z9.9    BLANK WHEN ZERO.             
085700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085800     03  W001-DET9-OLAGERE       PIC Z(7)9.                               
085900     03  FILLER                  PIC X       VALUE SPACE.                 
086000     03  W001-DET9-P-OLAGERE     PIC Z9.9    BLANK WHEN ZERO.             
086100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086200     03  W001-DET9-OLAGERF       PIC Z(7)9.                               
086300     03  FILLER                  PIC X       VALUE SPACE.                 
086400     03  W001-DET9-P-OLAGERF     PIC Z9.9    BLANK WHEN ZERO.             
086500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086600     03  W001-DET9-OLAGERG       PIC Z(7)9.                               
086700     03  FILLER                  PIC X       VALUE SPACE.                 
086800     03  W001-DET9-P-OLAGERG     PIC Z9.9    BLANK WHEN ZERO.             
086900     03  FILLER                  PIC X       VALUE SPACE.                 
087000     03  W001-DET9-OLAGERH       PIC Z(7)9.                               
087100     03  FILLER                  PIC X       VALUE SPACE.                 
087200     03  W001-DET9-P-OLAGERH     PIC Z9.9    BLANK WHEN ZERO.             
087300     03  FILLER                  PIC X       VALUE SPACE.                 
087400     03  W001-DET9-TOT           PIC Z(13)9.                              
087500     03  FILLER                  PIC X       VALUE SPACE.                 
087600     03  W001-DET9-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
087700     EJECT                                                                
087800 01  W001-DETALJRAD-10.                                                   
087900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
088000     03  FILLER                  PIC X(12)   VALUE 'SAF. STOCK  '.        
088100     03  FILLER                  PIC X       VALUE SPACE.                 
088200     03  W001-DET10-SLAGERA      PIC Z(7)9.                               
088300     03  FILLER                  PIC X       VALUE SPACE.                 
088400     03  W001-DET10-P-SLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
088500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088600     03  W001-DET10-SLAGERB      PIC Z(7)9.                               
088700     03  FILLER                  PIC X       VALUE SPACE.                 
088800     03  W001-DET10-P-SLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
088900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089000     03  W001-DET10-SLAGERC      PIC Z(7)9.                               
089100     03  FILLER                  PIC X       VALUE SPACE.                 
089200     03  W001-DET10-P-SLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
089300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089400     03  W001-DET10-SLAGERD      PIC Z(7)9.                               
089500     03  FILLER                  PIC X       VALUE SPACE.                 
089600     03  W001-DET10-P-SLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
089700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089800     03  W001-DET10-SLAGERE      PIC Z(7)9.                               
089900     03  FILLER                  PIC X       VALUE SPACE.                 
090000     03  W001-DET10-P-SLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
090100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090200     03  W001-DET10-SLAGERF      PIC Z(7)9.                               
090300     03  FILLER                  PIC X       VALUE SPACE.                 
090400     03  W001-DET10-P-SLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
090500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090600     03  W001-DET10-SLAGERG      PIC Z(7)9.                               
090700     03  FILLER                  PIC X       VALUE SPACE.                 
090800     03  W001-DET10-P-SLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
090900     03  FILLER                  PIC X       VALUE SPACE.                 
091000     03  W001-DET10-SLAGERH      PIC Z(7)9.                               
091100     03  FILLER                  PIC X       VALUE SPACE.                 
091200     03  W001-DET10-P-SLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
091300     03  FILLER                  PIC X       VALUE SPACE.                 
091400     03  W001-DET10-TOT          PIC Z(13)9.                              
091500     03  FILLER                  PIC X       VALUE SPACE.                 
091600     03  W001-DET10-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
091700     EJECT                                                                
091800 01  W001-DETALJRAD-11.                                                   
091900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
092000     03  FILLER                  PIC X(12)   VALUE 'AVERAGE ST. '.        
092100     03  FILLER                  PIC X       VALUE SPACE.                 
092200     03  W001-DET11-MLAGERA      PIC Z(7)9.                               
092300     03  FILLER                  PIC X       VALUE SPACE.                 
092400     03  W001-DET11-P-MLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
092500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092600     03  W001-DET11-MLAGERB      PIC Z(7)9.                               
092700     03  FILLER                  PIC X       VALUE SPACE.                 
092800     03  W001-DET11-P-MLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
092900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093000     03  W001-DET11-MLAGERC      PIC Z(7)9.                               
093100     03  FILLER                  PIC X       VALUE SPACE.                 
093200     03  W001-DET11-P-MLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
093300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093400     03  W001-DET11-MLAGERD      PIC Z(7)9.                               
093500     03  FILLER                  PIC X       VALUE SPACE.                 
093600     03  W001-DET11-P-MLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
093700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093800     03  W001-DET11-MLAGERE      PIC Z(7)9.                               
093900     03  FILLER                  PIC X       VALUE SPACE.                 
094000     03  W001-DET11-P-MLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
094100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094200     03  W001-DET11-MLAGERF      PIC Z(7)9.                               
094300     03  FILLER                  PIC X       VALUE SPACE.                 
094400     03  W001-DET11-P-MLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
094500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094600     03  W001-DET11-MLAGERG      PIC Z(7)9.                               
094700     03  FILLER                  PIC X       VALUE SPACE.                 
094800     03  W001-DET11-P-MLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
094900     03  FILLER                  PIC X       VALUE SPACE.                 
095000     03  W001-DET11-MLAGERH      PIC Z(7)9.                               
095100     03  FILLER                  PIC X       VALUE SPACE.                 
095200     03  W001-DET11-P-MLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
095300     03  FILLER                  PIC X       VALUE SPACE.                 
095400     03  W001-DET11-TOT          PIC Z(13)9.                              
095500     03  FILLER                  PIC X       VALUE SPACE.                 
095600     03  W001-DET11-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
095700     EJECT                                                                
095800 01  W001-DETALJRAD-12.                                                   
095900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096000     03  W001-DET12-PRISKLASS    PIC X       VALUE SPACE.                 
096100     03  FILLER                  PIC X       VALUE SPACE.                 
096200     03  FILLER                  PIC X(12)   VALUE 'NO INCOM ORD'.        
096300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096400     03  W001-DET12-KVOTA        PIC Z(7)9.                               
096500     03  FILLER                  PIC X       VALUE SPACE.                 
096600     03  W001-DET12-P-KVOTA      PIC Z9.9.                                
096700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096800     03  W001-DET12-KVOTB        PIC Z(7)9.                               
096900     03  FILLER                  PIC X       VALUE SPACE.                 
097000     03  W001-DET12-P-KVOTB      PIC Z9.9.                                
097100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097200     03  W001-DET12-KVOTC        PIC Z(7)9.                               
097300     03  FILLER                  PIC X       VALUE SPACE.                 
097400     03  W001-DET12-P-KVOTC      PIC Z9.9.                                
097500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097600     03  W001-DET12-KVOTD        PIC Z(7)9.                               
097700     03  FILLER                  PIC X       VALUE SPACE.                 
097800     03  W001-DET12-P-KVOTD      PIC Z9.9.                                
097900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098000     03  W001-DET12-KVOTE        PIC Z(7)9.                               
098100     03  FILLER                  PIC X       VALUE SPACE.                 
098200     03  W001-DET12-P-KVOTE      PIC Z9.9.                                
098300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098400     03  W001-DET12-KVOTF        PIC Z(7)9.                               
098500     03  FILLER                  PIC X       VALUE SPACE.                 
098600     03  W001-DET12-P-KVOTF      PIC Z9.9.                                
098700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098800     03  W001-DET12-KVOTG        PIC Z(7)9.                               
098900     03  FILLER                  PIC X       VALUE SPACE.                 
099000     03  W001-DET12-P-KVOTG      PIC Z9.9.                                
099100     03  FILLER                  PIC X       VALUE SPACE.                 
099200     03  W001-DET12-KVOTH        PIC Z(7)9.                               
099300     03  FILLER                  PIC X       VALUE SPACE.                 
099400     03  W001-DET12-P-KVOTH      PIC Z9.9.                                
099500     03  FILLER                  PIC X       VALUE SPACE.                 
099600     03  W001-DET12-TOT          PIC Z(13)9.                              
099700     03  FILLER                  PIC X       VALUE SPACE.                 
099800     03  W001-DET12-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
099900     EJECT                                                                
100000 01  W001-DETALJRAD-13.                                                   
100100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
100200     03  FILLER                  PIC X(13) VALUE 'SPLIT FACTOR '.         
100300     03  FILLER                  PIC X(4)    VALUE SPACE.                 
100400     03  W001-DET13-SPLITA       PIC Z9.9.                                
100500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
100600     03  W001-DET13-SPLITB       PIC Z9.9.                                
100700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
100800     03  W001-DET13-SPLITC       PIC Z9.9.                                
100900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101000     03  W001-DET13-SPLITD       PIC Z9.9.                                
101100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101200     03  W001-DET13-SPLITE       PIC Z9.9.                                
101300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101400     03  W001-DET13-SPLITF       PIC Z9.9.                                
101500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101600     03  W001-DET13-SPLITG       PIC Z9.9.                                
101700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
101800     03  W001-DET13-SPLITH       PIC Z9.9.                                
101900     03  FILLER                  PIC X(16)   VALUE SPACE.                 
102000     03  W001-DET13-TOT          PIC Z9.9.                                
102100     03  FILLER                  PIC X       VALUE SPACE.                 
102200     03  W001-DET13-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
102300     EJECT                                                                
102400 01  W001-DETALJRAD-14.                                                   
102500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
102600     03  FILLER                  PIC X(14) VALUE 'TOR        SOH'.        
102700     03  W001-DET14-OMSHASTA     PIC Z(4)9.9.                             
102800     03  FILLER                  PIC X       VALUE SPACE.                 
102900     03  W001-DET14-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
103000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103100     03  W001-DET14-OMSHASTB     PIC Z(5)9.9.                             
103200     03  FILLER                  PIC X       VALUE SPACE.                 
103300     03  W001-DET14-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
103400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103500     03  W001-DET14-OMSHASTC     PIC Z(5)9.9.                             
103600     03  FILLER                  PIC X       VALUE SPACE.                 
103700     03  W001-DET14-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
103800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103900     03  W001-DET14-OMSHASTD     PIC Z(5)9.9.                             
104000     03  FILLER                  PIC X       VALUE SPACE.                 
104100     03  W001-DET14-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
104200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104300     03  W001-DET14-OMSHASTE     PIC Z(5)9.9.                             
104400     03  FILLER                  PIC X       VALUE SPACE.                 
104500     03  W001-DET14-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
104600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104700     03  W001-DET14-OMSHASTF     PIC Z(5)9.9.                             
104800     03  FILLER                  PIC X       VALUE SPACE.                 
104900     03  W001-DET14-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
105000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105100     03  W001-DET14-OMSHASTG     PIC Z(5)9.9.                             
105200     03  FILLER                  PIC X       VALUE SPACE.                 
105300     03  W001-DET14-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
105400     03  FILLER                  PIC X       VALUE SPACE.                 
105500     03  W001-DET14-OMSHASTH     PIC Z(5)9.9.                             
105600     03  FILLER                  PIC X       VALUE SPACE.                 
105700     03  W001-DET14-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
105800     03  FILLER                  PIC X       VALUE SPACE.                 
105900     03  W001-DET14-TOT          PIC Z(11)9.9.                            
106000     03  FILLER                  PIC X       VALUE SPACE.                 
106100     03  W001-DET14-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
106200     EJECT                                                                
106300 01  W001-DETALJRAD-15.                                                   
106400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
106500     03  FILLER                  PIC X(14) VALUE 'TOR BAL+AK+GIT'.        
106600     03  W001-DET15-OMSHASTA     PIC Z(4)9.9.                             
106700     03  FILLER                  PIC X       VALUE SPACE.                 
106800     03  W001-DET15-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
106900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107000     03  W001-DET15-OMSHASTB     PIC Z(5)9.9.                             
107100     03  FILLER                  PIC X       VALUE SPACE.                 
107200     03  W001-DET15-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
107300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107400     03  W001-DET15-OMSHASTC     PIC Z(5)9.9.                             
107500     03  FILLER                  PIC X       VALUE SPACE.                 
107600     03  W001-DET15-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
107700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107800     03  W001-DET15-OMSHASTD     PIC Z(5)9.9.                             
107900     03  FILLER                  PIC X       VALUE SPACE.                 
108000     03  W001-DET15-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
108100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108200     03  W001-DET15-OMSHASTE     PIC Z(5)9.9.                             
108300     03  FILLER                  PIC X       VALUE SPACE.                 
108400     03  W001-DET15-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
108500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108600     03  W001-DET15-OMSHASTF     PIC Z(5)9.9.                             
108700     03  FILLER                  PIC X       VALUE SPACE.                 
108800     03  W001-DET15-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
108900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109000     03  W001-DET15-OMSHASTG     PIC Z(5)9.9.                             
109100     03  FILLER                  PIC X       VALUE SPACE.                 
109200     03  W001-DET15-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
109300     03  FILLER                  PIC X       VALUE SPACE.                 
109400     03  W001-DET15-OMSHASTH     PIC Z(5)9.9.                             
109500     03  FILLER                  PIC X       VALUE SPACE.                 
109600     03  W001-DET15-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
109700     03  FILLER                  PIC X       VALUE SPACE.                 
109800     03  W001-DET15-TOT          PIC Z(11)9.9.                            
109900     03  FILLER                  PIC X       VALUE SPACE.                 
110000     03  W001-DET15-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
110100     EJECT                                                                
110200 01  W001-DETALJRAD-16.                                                   
110300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
110400     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE G'.        
110500     03  FILLER                  PIC X(3)    VALUE SPACE.                 
110600     03  W001-DET16-SERVG-BTOA   PIC Z9.9.                                
110700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
110800     03  W001-DET16-SERVG-BTOB   PIC Z9.9.                                
110900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111000     03  W001-DET16-SERVG-BTOC   PIC Z9.9.                                
111100     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111200     03  W001-DET16-SERVG-BTOD   PIC Z9.9.                                
111300     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111400     03  W001-DET16-SERVG-BTOE   PIC Z9.9.                                
111500     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111600     03  W001-DET16-SERVG-BTOF   PIC Z9.9.                                
111700     03  FILLER                  PIC X(10)   VALUE SPACE.                 
111800     03  W001-DET16-SERVG-BTOG   PIC Z9.9.                                
111900     03  FILLER                  PIC X(10)   VALUE SPACE.                 
112000     03  W001-DET16-SERVG-BTOH   PIC Z9.9.                                
112100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
112200     03  FILLER                  PIC X(11)   VALUE SPACE.                 
112300     03  W001-DET16-TOT          PIC Z9.9.                                
112400     03  FILLER                  PIC X       VALUE SPACE.                 
112500     03  W001-DET16-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
112600     EJECT                                                                
112700 01  W001-DETALJRAD-17.                                                   
112800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
112900     03  FILLER                  PIC X(14) VALUE 'SERV. DEGREE N'.        
113000     03  FILLER                  PIC X(3)    VALUE SPACE.                 
113100     03  W001-DET17-SERVG-NTOA   PIC Z9.9.                                
113200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113300     03  W001-DET17-SERVG-NTOB   PIC Z9.9.                                
113400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113500     03  W001-DET17-SERVG-NTOC   PIC Z9.9.                                
113600     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113700     03  W001-DET17-SERVG-NTOD   PIC Z9.9.                                
113800     03  FILLER                  PIC X(10)   VALUE SPACE.                 
113900     03  W001-DET17-SERVG-NTOE   PIC Z9.9.                                
114000     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114100     03  W001-DET17-SERVG-NTOF   PIC Z9.9.                                
114200     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114300     03  W001-DET17-SERVG-NTOG   PIC Z9.9.                                
114400     03  FILLER                  PIC X(10)   VALUE SPACE.                 
114500     03  W001-DET17-SERVG-NTOH   PIC Z9.9.                                
114600     03  FILLER                  PIC X(5)    VALUE SPACE.                 
114700     03  FILLER                  PIC X(11)   VALUE SPACE.                 
114800     03  W001-DET17-TOT          PIC Z9.9.                                
114900     03  FILLER                  PIC X       VALUE SPACE.                 
115000     03  W001-DET17-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
115100     EJECT                                                                
115200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
115300*                                                                         
115400     EJECT                                                                
115500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
115600     SKIP3                                                                
115700 01  NYCKLAR-TILL-DLI.                                                    
115800     03  W-IDDC-B6-X.                                                     
115900         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
116000     SKIP2                                                                
116100*    --- STATUS-KOD FRÅN IMS                                              
116200 01  STATUS-WS                   PIC XX.                                  
116300     88  SEGMENT-FINNS                       VALUE '  '.                  
116400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
116500     SKIP2                                                                
116600 01  GODK-STATUSKODER.                                                    
116700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
116800     SKIP3                                                                
116900 01  SSA1                        PIC X(64).                               
117000 01  SSA2                        PIC X(64).                               
117100     EJECT                                                                
117200*    --- IMS FUNKTIONSKODER                                               
117300*01  -COPY W0003                                                          
117400     EJECT                                                                
117500*    ---  DLI INPUT-OUTPUT AREA                                           
117600 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
117700 01   DLI-IO-AREA-B6      PIC X(900).                                     
117800 01   DLI-IO-AREA-B601    REDEFINES DLI-IO-AREA-B6.                       
117900*     03  -COPY WDB601                                                    
118000     EJECT                                                                
118100 01   DLI-IO-AREA-B616    REDEFINES DLI-IO-AREA-B6.                       
118200*     03  -COPY WDB616                                                    
118300     EJECT                                                                
118400 LINKAGE SECTION.                                                         
118500                                                                          
118600*01  -COPY W0008      -PRE WDB6-                                          
118700     05  FILLER                  PIC X.                                   
118800 PROCEDURE DIVISION USING WDB6-PCB.                                       
118900                                                                          
119000     PERFORM A-INIT                                                       
119100                                                                          
119200     PERFORM S01-LAS-W23195                                               
119300                                                                          
119400     PERFORM UNTIL END-OF-W23195                                          
119500       PERFORM B-SKAPA-LISTA                                              
119600       PERFORM S30-SKRIV-DAP1                                             
119700       PERFORM S31-SKRIV-DAP2                                             
119800       PERFORM C-SKRIV-LISTA                                              
119900     END-PERFORM                                                          
120000     PERFORM Z-FINIT                                                      
120100                                                                          
120200     MOVE ZERO TO RETURN-CODE                                             
120300     GOBACK                                                               
120400     .                                                                    
120500     EJECT                                                                
120600 A-INIT SECTION.                                                          
120700                                                                          
120800     OPEN INPUT  W23195                                                   
120900     OPEN OUTPUT W2319A                                                   
121000                                                                          
121100     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
121200     MOVE D-AAR    TO DAGENS-AAR                                          
121300                      D-VECKA-AAR                                         
121400     MOVE D-MAANAD TO DAGENS-MAANAD                                       
121500     MOVE D-VECKA  TO D-VECKA-VECKA                                       
121600     MOVE D-DAG    TO DAGENS-DAG                                          
121700     MOVE DAGENS-DATUM TO W001-DATUM                                      
121800     MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                              
121900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
122000                                                                          
122100     PERFORM AA-LADDA-DC-TABELL                                           
122200     .                                                                    
122300     EJECT                                                                
122400 AA-LADDA-DC-TABELL SECTION.                                              
122500                                                                          
122600                                                                          
122700     INITIALIZE WS-DC-TABELL                                              
122800     MOVE 1 TO IDDC-IX                                                    
122900     PERFORM IMS-GET-WDB6                                                 
123000                                                                          
123100     PERFORM UNTIL SEGMENT-SAKNAS                                         
123200                OR IDDC-IX > IDDC-IX-MAX                                  
123300                                                                          
123400        IF WDB6-SEG-NAME-FB = 'WDB601  '                                  
123500           MOVE DCS-IDDC       TO W-IDDC                                  
123600           MOVE DCS-IDLEVNR-DC TO W-IDLEVNR-DC                            
123700        END-IF                                                            
123800                                                                          
123900        IF WDB6-SEG-NAME-FB = 'WDB616  '                                  
124000           MOVE W-IDDC           TO WS-IDDC-B601   (IDDC-IX)              
124100           MOVE W-IDLEVNR-DC     TO WS-IDLEVNR-DC  (IDDC-IX)              
124200           MOVE REF-IDDC-REF     TO WS-IDDC-B616   (IDDC-IX)              
124300           MOVE REF-KVDLTID-TOT  TO WS-KVDLTID-TOT (IDDC-IX)              
124400           ADD 1 TO IDDC-IX                                               
124500        END-IF                                                            
124600                                                                          
124700        PERFORM IMS-GET-WDB6                                              
124800     END-PERFORM                                                          
124900                                                                          
125000     .                                                                    
125100     EJECT                                                                
125200     EJECT                                                                
125300 B-SKAPA-LISTA SECTION.                                                   
125400                                                                          
125500     MOVE IN-IDDC      TO W001-AKTUELLT-IDDC                              
125600                          WS-IDDC                                         
125700     STRING 'W23196 ' IN-IDDC                                             
125800            DELIMITED BY SIZE INTO W001-LISTNR                            
125900     MOVE W001-LISTNR  TO W001-LISTID                                     
126000                                                                          
126100     PERFORM BA-HITTA-KVDLTID                                             
126200                                                                          
126300     PERFORM BB-NOLLSTALL                                                 
126400                                                                          
126500     MOVE IN-IDDC TO WS-PREV-IDDC                                         
126600     PERFORM UNTIL END-OF-W23195 OR IN-IDDC NOT = WS-PREV-IDDC            
126700        PERFORM BC-SKAPA-TABELLER                                         
126800        PERFORM S01-LAS-W23195                                            
126900     END-PERFORM                                                          
127000                                                                          
127100     PERFORM BD-SUMMERA                                                   
127200     .                                                                    
127300     EJECT                                                                
127400 BA-HITTA-KVDLTID SECTION.                                                
127500                                                                          
127600*    HITTA SÄNDANDE DC MHA LEVERANTÖRSNUMMER                              
127700     MOVE +1 TO IDDC-IX                                                   
127800     PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                               
127900                   IN-IDLEVNR = WS-IDLEVNR-DC (IDDC-IX)                   
128000                                                                          
128100        ADD +1 TO IDDC-IX                                                 
128200     END-PERFORM                                                          
128300                                                                          
128400     IF IDDC-IX NOT > IDDC-IX-MAX                                         
128500*    VI HAR HITTAT LEVERANTÖREN WS-IDDC-B601 ÄR DÅ SÄNDANDE DC            
128600*    MOTTAGANDE DC FINNS I IN-IDLEVNR                                     
128700*    GÖR NY SÖKNING I TABELLEN MED DESSA VÄRDEN FÖR ATT HITTA             
128800*    RÄTT KVDAGAR                                                         
128900                                                                          
129000        MOVE WS-IDDC-B601(IDDC-IX) TO W-IDDC-SEND                         
129100        MOVE IN-IDDC               TO W-IDDC-REC                          
129200                                                                          
129300        MOVE +1 TO IDDC-IX                                                
129400        PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                            
129500                     (W-IDDC-REC  = WS-IDDC-B601 (IDDC-IX) AND            
129600                      W-IDDC-SEND = WS-IDDC-B616 (IDDC-IX))               
129700                                                                          
129800           ADD +1 TO IDDC-IX                                              
129900        END-PERFORM                                                       
130000        IF IDDC-IX > IDDC-IX-MAX                                          
130100           MOVE SPACE     TO W-IDDC-SEND                                  
130200           MOVE SPACE     TO W-IDDC-REC                                   
130300        END-IF                                                            
130400     ELSE                                                                 
130500           MOVE SPACE     TO W-IDDC-SEND                                  
130600           MOVE SPACE     TO W-IDDC-REC                                   
130700     END-IF                                                               
130800*    LÄS OM MED DC11 SOM SÄNDANDE........                                 
130900*    OM INGEN TRÄFF NU HELLER ABENDAR VI SKITEN...                        
131000     IF W-IDDC-SEND = SPACE                                               
131100        MOVE WC-CDC-SE    TO W-IDDC-SEND                                  
131200        MOVE IN-IDDC      TO W-IDDC-REC                                   
131300        MOVE +1 TO IDDC-IX                                                
131400        PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                            
131500                     (W-IDDC-REC  = WS-IDDC-B601 (IDDC-IX) AND            
131600                      W-IDDC-SEND = WS-IDDC-B616 (IDDC-IX))               
131700                                                                          
131800           ADD +1 TO IDDC-IX                                              
131900        END-PERFORM                                                       
132000        IF IDDC-IX > IDDC-IX-MAX                                          
132100           CALL FELLOG                                                    
132200        END-IF                                                            
132300     END-IF                                                               
132400     .                                                                    
132500     EJECT                                                                
132600 BB-NOLLSTALL SECTION.                                                    
132700******************************************************************        
132800*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
132900*  FREKVENSKLASS                                                 *        
133000*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
133100*     FREKV-KLASSER  = A B C D E F G                             *        
133200*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
133300*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
133400*                 TOTAL-SUMMERING                                *        
133500* ****************************************************************        
133600                                                                          
133700******* NOLLSTÄLLNING AV 72 'RUTOR' PER PRISKLASS/FREKVKLASS              
133800                                                                          
133900     MOVE +1  TO ART-IX                                                   
134000     MOVE +72 TO ART-IX-MAX                                               
134100     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
134200        MOVE ZERO TO     ART-KVANT-AKT(ART-IX)                            
134300                         ART-KVANT-PAS(ART-IX)                            
134400                         ART-PROC-KVANT-A(ART-IX)                         
134500                         ART-PROC-KVANT-P(ART-IX)                         
134600                         ART-KVDISP-AKT(ART-IX)                           
134700                         ART-PROC-KVDISP-A(ART-IX)                        
134800                         ART-KVDISP-PAS(ART-IX)                           
134900                         ART-PROC-KVDISP-P(ART-IX)                        
135000                         ART-LS-AKT(ART-IX)                               
135100                         ART-PROC-LS-A(ART-IX)                            
135200                         ART-LS-PAS(ART-IX)                               
135300                         ART-PROC-LS-P(ART-IX)                            
135400                         ART-AK-AKT(ART-IX)                               
135500                         ART-PROC-AK-A(ART-IX)                            
135600                         ART-AK-PAS(ART-IX)                               
135700                         ART-PROC-AK-P(ART-IX)                            
135800                         ART-OLAGER(ART-IX)                               
135900                         ART-PROC-OLAGER(ART-IX)                          
136000                         ART-SLAGER(ART-IX)                               
136100                         ART-PROC-SLAGER(ART-IX)                          
136200                         ART-MLAGER(ART-IX)                               
136300                         ART-PROC-MLAGER(ART-IX)                          
136400                         ART-KVOT(ART-IX)                                 
136500                         ART-PROC-KVOT(ART-IX)                            
136600                         ART-SPLIT(ART-IX)                                
136700                         ART-OMSHAST-DISP(ART-IX)                         
136800                         ART-OMSHAST-PROC-D(ART-IX)                       
136900                         ART-OMSHAST-LS(ART-IX)                           
137000                         ART-OMSHAST-PROC-LS(ART-IX)                      
137100                         ART-SERVG-BTO(ART-IX)                            
137200                         ART-SERVG-NTO(ART-IX)                            
137300****************                                                          
137400                         WS-ART-SLAGER(ART-IX)                            
137500                         WS-ART-OLAGER(ART-IX)                            
137600                         WS-ART-MLAGER(ART-IX)                            
137700                         WS-ART-KVLS-AKT(ART-IX)                          
137800                         WS-ART-KVLS-PAS(ART-IX)                          
137900                         WS-ART-LS-AKT(ART-IX)                            
138000                         WS-ART-LS-PAS(ART-IX)                            
138100                         WS-ART-LS-PR-AKT(ART-IX)                         
138200                         WS-ART-LS-PR-PAS(ART-IX)                         
138300                         WS-ART-KVDISP-AKT(ART-IX)                        
138400                         WS-ART-KVDISP-PAS(ART-IX)                        
138500                         WS-ART-KVDISP-PR-AKT(ART-IX)                     
138600                         WS-ART-KVDISP-PR-PAS(ART-IX)                     
138700                         WS-ART-KVOKS-AKT(ART-IX)                         
138800                         WS-ART-KVOKS-PAS(ART-IX)                         
138900                         WS-ART-OK-PR-AKT(ART-IX)                         
139000                         WS-ART-OK-PR-PAS(ART-IX)                         
139100                         WS-ART-KVAKS-AKT(ART-IX)                         
139200                         WS-ART-KVAKS-PAS(ART-IX)                         
139300                         WS-ART-AK-PR-AKT(ART-IX)                         
139400                         WS-ART-AK-PR-PAS(ART-IX)                         
139500                         WS-ART-KVOI(ART-IX)                              
139600                         WS-ART-KVOI-AKT(ART-IX)                          
139700                         WS-ART-KVOI-PAS(ART-IX)                          
139800                         WS-ART-KVOI-TEO(ART-IX)                          
139900                         WS-ART-KVOI-SAK(ART-IX)                          
140000                         WS-ART-KVOI-CDC-AKT(ART-IX)                      
140100                         WS-ART-KVOI-CDC-PAS(ART-IX)                      
140200                         WS-ART-KVOI-CDC-TEO(ART-IX)                      
140300                         WS-ART-KVOI-CDC-SAK(ART-IX)                      
140400                         WS-ART-SUINKORD(ART-IX)                          
140500                         WS-ART-SUFYSAVP(ART-IX)                          
140600                         WS-ART-SUAVBRP(ART-IX)                           
140700                         WS-ART-SULAGERB(ART-IX)                          
140800                         WS-ART-SUSORTB(ART-IX)                           
140900                                                                          
141000        ADD +1 TO ART-IX                                                  
141100     END-PERFORM                                                          
141200                                                                          
141300******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
141400*******                          OBEROENDE AV FREKVENSKLASS               
141500                                                                          
141600     MOVE +1 TO PSUM-IX                                                   
141700     MOVE +9 TO PSUM-IX-MAX                                               
141800     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
141900        MOVE ZERO TO     PSUM-KVANT-AKT(PSUM-IX)                          
142000                         PSUM-KVANT-PAS(PSUM-IX)                          
142100                         PSUM-PROC-KVANT-A(PSUM-IX)                       
142200                         PSUM-PROC-KVANT-P(PSUM-IX)                       
142300                         PSUM-KVDISP-AKT(PSUM-IX)                         
142400                         PSUM-PROC-KVDISP-A(PSUM-IX)                      
142500                         PSUM-KVDISP-PAS(PSUM-IX)                         
142600                         PSUM-PROC-KVDISP-P(PSUM-IX)                      
142700                         PSUM-LS-AKT(PSUM-IX)                             
142800                         PSUM-PROC-LS-A(PSUM-IX)                          
142900                         PSUM-LS-PAS(PSUM-IX)                             
143000                         PSUM-PROC-LS-P(PSUM-IX)                          
143100                         PSUM-AK-AKT(PSUM-IX)                             
143200                         PSUM-PROC-AK-A(PSUM-IX)                          
143300                         PSUM-AK-PAS(PSUM-IX)                             
143400                         PSUM-PROC-AK-P(PSUM-IX)                          
143500                         PSUM-OLAGER(PSUM-IX)                             
143600                         PSUM-PROC-OLAGER(PSUM-IX)                        
143700                         PSUM-SLAGER(PSUM-IX)                             
143800                         PSUM-PROC-SLAGER(PSUM-IX)                        
143900                         PSUM-MLAGER(PSUM-IX)                             
144000                         PSUM-PROC-MLAGER(PSUM-IX)                        
144100                         PSUM-KVOT(PSUM-IX)                               
144200                         PSUM-PROC-KVOT(PSUM-IX)                          
144300                         PSUM-SPLIT(PSUM-IX)                              
144400                         PSUM-OMSHAST-DISP(PSUM-IX)                       
144500                         PSUM-OMSHAST-PROC-D(PSUM-IX)                     
144600                         PSUM-OMSHAST-LS(PSUM-IX)                         
144700                         PSUM-OMSHAST-PROC-LS(PSUM-IX)                    
144800                         PSUM-SERVG-BTO(PSUM-IX)                          
144900                         PSUM-SERVG-NTO(PSUM-IX)                          
145000*************                                                             
145100                         WS-PSUM-SLAGER(PSUM-IX)                          
145200                         WS-PSUM-OLAGER(PSUM-IX)                          
145300                         WS-PSUM-MLAGER(PSUM-IX)                          
145400                         WS-PSUM-LS-AKT(PSUM-IX)                          
145500                         WS-PSUM-LS-PAS(PSUM-IX)                          
145600                         WS-PSUM-LS-PR-AKT(PSUM-IX)                       
145700                         WS-PSUM-LS-PR-PAS(PSUM-IX)                       
145800                         WS-PSUM-KVDISP-AKT(PSUM-IX)                      
145900                         WS-PSUM-KVDISP-PAS(PSUM-IX)                      
146000                         WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                   
146100                         WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                   
146200                         WS-PSUM-KVOKS-AKT(PSUM-IX)                       
146300                         WS-PSUM-KVOKS-PAS(PSUM-IX)                       
146400                         WS-PSUM-OK-PR-AKT(PSUM-IX)                       
146500                         WS-PSUM-OK-PR-PAS(PSUM-IX)                       
146600                         WS-PSUM-KVAKS-AKT(PSUM-IX)                       
146700                         WS-PSUM-KVAKS-PAS(PSUM-IX)                       
146800                         WS-PSUM-AK-PR-AKT(PSUM-IX)                       
146900                         WS-PSUM-AK-PR-PAS(PSUM-IX)                       
147000                         WS-PSUM-KVOI(PSUM-IX)                            
147100                         WS-PSUM-KVOI-AKT(PSUM-IX)                        
147200                         WS-PSUM-KVOI-PAS(PSUM-IX)                        
147300                         WS-PSUM-KVOI-TEO(PSUM-IX)                        
147400                         WS-PSUM-KVOI-SAK(PSUM-IX)                        
147500                         WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                    
147600                         WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                    
147700                         WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                    
147800                         WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                    
147900                         WS-PSUM-SUINKORD(PSUM-IX)                        
148000                         WS-PSUM-SUFYSAVP(PSUM-IX)                        
148100                         WS-PSUM-SUAVBRP(PSUM-IX)                         
148200                         WS-PSUM-SULAGERB(PSUM-IX)                        
148300                         WS-PSUM-SUSORTB(PSUM-IX)                         
148400                                                                          
148500        ADD +1 TO PSUM-IX                                                 
148600     END-PERFORM                                                          
148700                                                                          
148800******* NOLLSTÄLLNING AV 7 'RUTOR' TOTALSUMMA PER FREKVENSKLASS           
148900*******                            OBEROENDE AV PRISKLASS                 
149000                                                                          
149100     MOVE +1 TO FSUM-IX                                                   
149200     MOVE +8 TO FSUM-IX-MAX                                               
149300     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
149400        MOVE ZERO TO     FSUM-KVANT-AKT(FSUM-IX)                          
149500                         FSUM-KVANT-PAS(FSUM-IX)                          
149600                         FSUM-PROC-KVANT-A(FSUM-IX)                       
149700                         FSUM-PROC-KVANT-P(FSUM-IX)                       
149800                         FSUM-KVDISP-AKT(FSUM-IX)                         
149900                         FSUM-PROC-KVDISP-A(FSUM-IX)                      
150000                         FSUM-KVDISP-PAS(FSUM-IX)                         
150100                         FSUM-PROC-KVDISP-P(FSUM-IX)                      
150200                         FSUM-LS-AKT(FSUM-IX)                             
150300                         FSUM-PROC-LS-A(FSUM-IX)                          
150400                         FSUM-LS-PAS(FSUM-IX)                             
150500                         FSUM-PROC-LS-P(FSUM-IX)                          
150600                         FSUM-AK-AKT(FSUM-IX)                             
150700                         FSUM-PROC-AK-A(FSUM-IX)                          
150800                         FSUM-AK-PAS(FSUM-IX)                             
150900                         FSUM-PROC-AK-P(FSUM-IX)                          
151000                         FSUM-OLAGER(FSUM-IX)                             
151100                         FSUM-PROC-OLAGER(FSUM-IX)                        
151200                         FSUM-SLAGER(FSUM-IX)                             
151300                         FSUM-PROC-SLAGER(FSUM-IX)                        
151400                         FSUM-MLAGER(FSUM-IX)                             
151500                         FSUM-PROC-MLAGER(FSUM-IX)                        
151600                         FSUM-KVOT(FSUM-IX)                               
151700                         FSUM-PROC-KVOT(FSUM-IX)                          
151800                         FSUM-SPLIT(FSUM-IX)                              
151900                         FSUM-OMSHAST-DISP(FSUM-IX)                       
152000                         FSUM-OMSHAST-PROC-D(FSUM-IX)                     
152100                         FSUM-OMSHAST-LS(FSUM-IX)                         
152200                         FSUM-OMSHAST-PROC-LS(FSUM-IX)                    
152300                         FSUM-SERVG-BTO(FSUM-IX)                          
152400                         FSUM-SERVG-NTO(FSUM-IX)                          
152500*****************                                                         
152600                         WS-FSUM-SLAGER(FSUM-IX)                          
152700                         WS-FSUM-OLAGER(FSUM-IX)                          
152800                         WS-FSUM-MLAGER(FSUM-IX)                          
152900                         WS-FSUM-LS-AKT(FSUM-IX)                          
153000                         WS-FSUM-LS-PAS(FSUM-IX)                          
153100                         WS-FSUM-LS-PR-AKT(FSUM-IX)                       
153200                         WS-FSUM-LS-PR-PAS(FSUM-IX)                       
153300                         WS-FSUM-KVDISP-AKT(FSUM-IX)                      
153400                         WS-FSUM-KVDISP-PAS(FSUM-IX)                      
153500                         WS-FSUM-KVDISP-PR-AKT(FSUM-IX)                   
153600                         WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                   
153700                         WS-FSUM-KVOKS-AKT(FSUM-IX)                       
153800                         WS-FSUM-KVOKS-PAS(FSUM-IX)                       
153900                         WS-FSUM-OK-PR-AKT(FSUM-IX)                       
154000                         WS-FSUM-OK-PR-PAS(FSUM-IX)                       
154100                         WS-FSUM-KVAKS-AKT(FSUM-IX)                       
154200                         WS-FSUM-KVAKS-PAS(FSUM-IX)                       
154300                         WS-FSUM-AK-PR-AKT(FSUM-IX)                       
154400                         WS-FSUM-AK-PR-PAS(FSUM-IX)                       
154500                         WS-FSUM-KVOI(FSUM-IX)                            
154600                         WS-FSUM-KVOI-AKT(FSUM-IX)                        
154700                         WS-FSUM-KVOI-PAS(FSUM-IX)                        
154800                         WS-FSUM-KVOI-TEO(FSUM-IX)                        
154900                         WS-FSUM-KVOI-SAK(FSUM-IX)                        
155000                         WS-FSUM-KVOI-CDC-AKT(FSUM-IX)                    
155100                         WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                    
155200                         WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                    
155300                         WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                    
155400                         WS-FSUM-SUINKORD(FSUM-IX)                        
155500                         WS-FSUM-SUFYSAVP(FSUM-IX)                        
155600                         WS-FSUM-SUAVBRP(FSUM-IX)                         
155700                         WS-FSUM-SULAGERB(FSUM-IX)                        
155800                         WS-FSUM-SUSORTB(FSUM-IX)                         
155900                                                                          
156000        ADD +1 TO FSUM-IX                                                 
156100     END-PERFORM                                                          
156200                                                                          
156300******* NOLLSTÄLLNING AV TOTALRUTA                                        
156400                                                                          
156500     MOVE ZERO TO     TOT-KVANT-AKT                                       
156600                      TOT-KVANT-PAS                                       
156700                      TOT-KVDISP-AKT                                      
156800                      TOT-KVDISP-PAS                                      
156900                      TOT-LS-AKT                                          
157000                      TOT-LS-PAS                                          
157100                      TOT-AK-AKT                                          
157200                      TOT-AK-PAS                                          
157300                      TOT-SLAGER                                          
157400                      TOT-MLAGER                                          
157500                      TOT-OLAGER                                          
157600                      TOT-KVOT                                            
157700                      TOT-PROC-OLAGER                                     
157800                      TOT-PROC-MLAGER                                     
157900                      TOT-PROC-SLAGER                                     
158000                      TOT-SPLIT                                           
158100                      TOT-OMSHAST-DISP                                    
158200                      TOT-OMSHAST-LS                                      
158300                      TOT-SERVG-BTO                                       
158400                      TOT-SERVG-NTO                                       
158500************                                                              
158600                      WS-TOT-SLAGER                                       
158700                      WS-TOT-OLAGER                                       
158800                      WS-TOT-MLAGER                                       
158900                      WS-TOT-LS-AKT                                       
159000                      WS-TOT-LS-PAS                                       
159100                      WS-TOT-LS-PR-AKT                                    
159200                      WS-TOT-LS-PR-PAS                                    
159300                      WS-TOT-KVDISP-AKT                                   
159400                      WS-TOT-KVDISP-PAS                                   
159500                      WS-TOT-KVDISP-PR-AKT                                
159600                      WS-TOT-KVDISP-PR-PAS                                
159700                      WS-TOT-KVOKS-AKT                                    
159800                      WS-TOT-KVOKS-PAS                                    
159900                      WS-TOT-OK-PR-AKT                                    
160000                      WS-TOT-OK-PR-PAS                                    
160100                      WS-TOT-KVAKS-AKT                                    
160200                      WS-TOT-KVAKS-PAS                                    
160300                      WS-TOT-AK-PR-AKT                                    
160400                      WS-TOT-AK-PR-PAS                                    
160500                      WS-TOT-KVOI                                         
160600                      WS-TOT-KVOI-AKT                                     
160700                      WS-TOT-KVOI-PAS                                     
160800                      WS-TOT-KVOI-TEO                                     
160900                      WS-TOT-KVOI-SAK                                     
161000                      WS-TOT-KVOI-CDC-AKT                                 
161100                      WS-TOT-KVOI-CDC-PAS                                 
161200                      WS-TOT-KVOI-CDC-TEO                                 
161300                      WS-TOT-KVOI-CDC-SAK                                 
161400                      WS-TOT-SUINKORD                                     
161500                      WS-TOT-SUFYSAVP                                     
161600                      WS-TOT-SUAVBRP                                      
161700                      WS-TOT-SULAGERB                                     
161800                      WS-TOT-SUSORTB                                      
161900     .                                                                    
162000     EJECT                                                                
162100 BC-SKAPA-TABELLER SECTION.                                               
162200                                                                          
162300     PERFORM BCA-SAETT-ART-IX                                             
162400     IF SW-ARTIKEL-SAKNAS-WDK7 = JA                                       
162500        PERFORM BCC-UPPDAT-SAKN-ART                                       
162600     END-IF                                                               
162700     IF ART-IX > ZERO                                                     
162800        PERFORM BCB-UPPDATERA-TABELLER                                    
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200 BCA-SAETT-ART-IX SECTION.                                                
163300******************************************************************        
163400* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
163500******************************************************************        
163600                                                                          
163700     MOVE NEJ TO SW-ARTIKEL-SAKNAS-WDK7                                   
163800     EVALUATE TRUE                                                        
163900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
164000          MOVE +1 TO ART-IX                                               
164100     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
164200          MOVE +2 TO ART-IX                                               
164300     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
164400          MOVE +3 TO ART-IX                                               
164500     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
164600          MOVE +4 TO ART-IX                                               
164700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
164800          MOVE +5 TO ART-IX                                               
164900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
165000          MOVE +6 TO ART-IX                                               
165100     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
165200          MOVE +7 TO ART-IX                                               
165300     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
165400          MOVE +8 TO ART-IX                                               
165500                                                                          
165600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
165700          MOVE +9 TO ART-IX                                               
165800     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
165900          MOVE +10 TO ART-IX                                              
166000     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
166100          MOVE +11 TO ART-IX                                              
166200     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
166300          MOVE +12 TO ART-IX                                              
166400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
166500          MOVE +13 TO ART-IX                                              
166600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
166700          MOVE +14 TO ART-IX                                              
166800     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
166900          MOVE +15 TO ART-IX                                              
167000     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
167100          MOVE +16 TO ART-IX                                              
167200                                                                          
167300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
167400          MOVE +17 TO ART-IX                                              
167500     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
167600          MOVE +18 TO ART-IX                                              
167700     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
167800          MOVE +19 TO ART-IX                                              
167900     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
168000          MOVE +20 TO ART-IX                                              
168100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
168200          MOVE +21 TO ART-IX                                              
168300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
168400          MOVE +22 TO ART-IX                                              
168500     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
168600          MOVE +23 TO ART-IX                                              
168700     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
168800          MOVE +24 TO ART-IX                                              
168900                                                                          
169000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
169100          MOVE +25 TO ART-IX                                              
169200     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
169300          MOVE +26 TO ART-IX                                              
169400     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
169500          MOVE +27 TO ART-IX                                              
169600     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
169700          MOVE +28 TO ART-IX                                              
169800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
169900          MOVE +29 TO ART-IX                                              
170000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
170100          MOVE +30 TO ART-IX                                              
170200     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
170300          MOVE +31 TO ART-IX                                              
170400     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
170500          MOVE +32 TO ART-IX                                              
170600                                                                          
170700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
170800          MOVE +33 TO ART-IX                                              
170900     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
171000          MOVE +34 TO ART-IX                                              
171100     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
171200          MOVE +35 TO ART-IX                                              
171300     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
171400          MOVE +36 TO ART-IX                                              
171500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
171600          MOVE +37 TO ART-IX                                              
171700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
171800          MOVE +38 TO ART-IX                                              
171900     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
172000          MOVE +39 TO ART-IX                                              
172100     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
172200          MOVE +40 TO ART-IX                                              
172300                                                                          
172400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
172500          MOVE +41 TO ART-IX                                              
172600     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
172700          MOVE +42 TO ART-IX                                              
172800     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
172900          MOVE +43 TO ART-IX                                              
173000     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
173100          MOVE +44 TO ART-IX                                              
173200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
173300          MOVE +45 TO ART-IX                                              
173400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
173500          MOVE +46 TO ART-IX                                              
173600     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
173700          MOVE +47 TO ART-IX                                              
173800     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
173900          MOVE +48 TO ART-IX                                              
174000                                                                          
174100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
174200          MOVE +49 TO ART-IX                                              
174300     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
174400          MOVE +50 TO ART-IX                                              
174500     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
174600          MOVE +51 TO ART-IX                                              
174700     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
174800          MOVE +52 TO ART-IX                                              
174900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
175000          MOVE +53 TO ART-IX                                              
175100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
175200          MOVE +54 TO ART-IX                                              
175300     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
175400          MOVE +55 TO ART-IX                                              
175500     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
175600          MOVE +56 TO ART-IX                                              
175700                                                                          
175800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
175900          MOVE +57 TO ART-IX                                              
176000     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
176100          MOVE +58 TO ART-IX                                              
176200     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
176300          MOVE +59 TO ART-IX                                              
176400     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
176500          MOVE +60 TO ART-IX                                              
176600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
176700          MOVE +61 TO ART-IX                                              
176800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
176900          MOVE +62 TO ART-IX                                              
177000     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
177100          MOVE +63 TO ART-IX                                              
177200     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
177300          MOVE +64 TO ART-IX                                              
177400                                                                          
177500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
177600          MOVE +65 TO ART-IX                                              
177700     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
177800          MOVE +66 TO ART-IX                                              
177900     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
178000          MOVE +67 TO ART-IX                                              
178100     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
178200          MOVE +68 TO ART-IX                                              
178300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
178400          MOVE +69 TO ART-IX                                              
178500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
178600          MOVE +70 TO ART-IX                                              
178700     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
178800          MOVE +71 TO ART-IX                                              
178900     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
179000          MOVE +72 TO ART-IX                                              
179100     WHEN OTHER                                                           
179200          MOVE ZERO TO ART-IX                                             
179300          IF IN-KDPRISKL = SPACE AND IN-KDFREKKL = SPACE                  
179400             MOVE JA TO SW-ARTIKEL-SAKNAS-WDK7                            
179500          END-IF                                                          
179600     END-EVALUATE                                                         
179700     .                                                                    
179800     EJECT                                                                
179900 BCB-UPPDATERA-TABELLER SECTION.                                          
180000                                                                          
180100     IF NDC-CN                                                            
180200        PERFORM BCBA-UPDATE-TABLE-CHINA                                   
180300     ELSE                                                                 
180400        PERFORM BCBB-UPDATE-TABLE-REST                                    
180500     END-IF                                                               
180600     PERFORM BCBC-UPDATE-TABLE-FINAL                                      
180700                                                                          
180800     .                                                                    
180900     EJECT                                                                
181000 BCBA-UPDATE-TABLE-CHINA SECTION.                                         
181100                                                                          
181200*********  ANTAL ARTIKLAR                                                 
181300     IF IN-KDREFSTA = 'A'                                                 
181400        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
181500     ELSE                                                                 
181600        IF IN-KDREFSTA = 'P'                                              
181700           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
181800        END-IF                                                            
181900     END-IF                                                               
182000                                                                          
182100*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
182200     IF IN-KDREFSTA = 'A'                                                 
182300        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
182400        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
182500        COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                           
182600        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRMATRL                         
182700        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
182800                                                                          
182900        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRMATRL                          
183000        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
183100                                                                          
183200        COMPUTE WS-SUMMA = IN-KVLS * IN-PRMATRL                           
183300        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
183400                                                                          
183500        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
183600        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
183700        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRMATRL                          
183800        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
183900     ELSE                                                                 
184000        IF IN-KDREFSTA = 'P'                                              
184100           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
184200           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
184300           COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                        
184400           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRMATRL                      
184500           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
184600                                                                          
184700           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRMATRL                       
184800           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
184900                                                                          
185000           COMPUTE WS-SUMMA = IN-KVLS * IN-PRMATRL                        
185100           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
185200                                                                          
185300           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
185400           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
185500           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRMATRL                       
185600           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
185700        END-IF                                                            
185800     END-IF                                                               
185900                                                                          
186000*********  OMSÄTTNINGSHASTIGHET                                           
186100     MOVE +1 TO KVOI-IX                                                   
186200     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
186300     PERFORM UNTIL KVOI-IX > 53                                           
186400        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
186500        ADD +1 TO KVOI-IX                                                 
186600     END-PERFORM                                                          
186700                                                                          
186800     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRMATRL                       
186900     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
187000                                                                          
187100*********  SÄKERHETSLAGER/ARTIKEL                                         
187200     IF IN-KDREFSTA = 'A'                                                 
187300        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRMATRL                       
187400        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
187500     END-IF                                                               
187600                                                                          
187700*********  ÖVERLAGER/ARTIKEL                                              
187800     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
187900     IF WS-KVDISP > IN-KVREFOVL                                           
188000        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
188100        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRMATRL                         
188200        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
188300     END-IF                                                               
188400                                                                          
188500*********  MEDELLAGER/ARTIKEL                                             
188600     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
188700     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
188800     COMPUTE WS-LT-BEHOV-SDC-NORM = WS-KVDLTID-TOT (IDDC-IX)              
188900                                      * WS-KVPB-DAG-SDC-NORM              
189000     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
189100                        + (IN-KVREFBER / 2)                               
189200     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRMATRL                            
189300     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
189400                                                                          
189500     .                                                                    
189600     EJECT                                                                
189700 BCBB-UPDATE-TABLE-REST SECTION.                                          
189800                                                                          
189900*********  ANTAL ARTIKLAR                                                 
190000     IF IN-KDREFSTA = 'A'                                                 
190100        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
190200     ELSE                                                                 
190300        IF IN-KDREFSTA = 'P'                                              
190400           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
190500        END-IF                                                            
190600     END-IF                                                               
190700                                                                          
190800*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
190900     IF IN-KDREFSTA = 'A'                                                 
191000        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
191100        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
191200        COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                           
191300        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                        
191400        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
191500                                                                          
191600        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                         
191700        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
191800                                                                          
191900        COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                          
192000        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
192100                                                                          
192200        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
192300        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
192400        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                         
192500        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
192600     ELSE                                                                 
192700        IF IN-KDREFSTA = 'P'                                              
192800           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
192900           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
193000           COMPUTE WS-KVDISP = IN-KVLS - IN-KVRESS                        
193100           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                     
193200           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
193300                                                                          
193400           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                      
193500           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
193600                                                                          
193700           COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                       
193800           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
193900                                                                          
194000           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
194100           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
194200           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                      
194300           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
194400        END-IF                                                            
194500     END-IF                                                               
194600                                                                          
194700*********  OMSÄTTNINGSHASTIGHET                                           
194800     MOVE +1 TO KVOI-IX                                                   
194900     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
195000     PERFORM UNTIL KVOI-IX > 53                                           
195100        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
195200        ADD +1 TO KVOI-IX                                                 
195300     END-PERFORM                                                          
195400                                                                          
195500     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRARTSTD                      
195600     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
195700                                                                          
195800*********  SÄKERHETSLAGER/ARTIKEL                                         
195900     IF IN-KDREFSTA = 'A'                                                 
196000        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRARTSTD                      
196100        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
196200     END-IF                                                               
196300                                                                          
196400*********  ÖVERLAGER/ARTIKEL                                              
196500     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
196600     IF WS-KVDISP > IN-KVREFOVL                                           
196700        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
196800        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRARTSTD                        
196900        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
197000     END-IF                                                               
197100                                                                          
197200*********  MEDELLAGER/ARTIKEL                                             
197300     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
197400     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
197500     COMPUTE WS-LT-BEHOV-SDC-NORM = WS-KVDLTID-TOT (IDDC-IX)              
197600                                      * WS-KVPB-DAG-SDC-NORM              
197700     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
197800                        + (IN-KVREFBER / 2)                               
197900     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRARTSTD                           
198000     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
198100                                                                          
198200     .                                                                    
198300     EJECT                                                                
198400 BCBC-UPDATE-TABLE-FINAL SECTION.                                         
198500                                                                          
198600*********  SERVICEGRAD OCH SPLITFAKTOR ORDERRADER/ARTIKEL                 
198700                                                                          
198800     MOVE +1 TO KVOI-IX                                                   
198900     MOVE NEJ TO SW-KVOI-TRAFF                                            
199000     PERFORM UNTIL KVOI-IX > 5                                            
199100        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
199200           MOVE JA TO SW-KVOI-TRAFF                                       
199300           IF IN-KDREFSTA = 'A'                                           
199400              ADD IN-KVOT-INNEV(KVOI-IX)                                  
199500                                TO WS-ART-KVOI-AKT(ART-IX)                
199600              ADD IN-KVOT-CDC-INNEV(KVOI-IX)                              
199700                                TO WS-ART-KVOI-CDC-AKT(ART-IX)            
199800           ELSE                                                           
199900              IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                     
200000                 ADD IN-KVOT-INNEV(KVOI-IX)                               
200100                                TO WS-ART-KVOI-TEO(ART-IX)                
200200                 ADD IN-KVOT-CDC-INNEV(KVOI-IX)                           
200300                                TO WS-ART-KVOI-CDC-TEO(ART-IX)            
200400              ELSE                                                        
200500                 IF IN-KDREFSTA = 'P'                                     
200600                    ADD IN-KVOT-INNEV(KVOI-IX)                            
200700                                   TO WS-ART-KVOI-PAS(ART-IX)             
200800                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
200900                                   TO WS-ART-KVOI-CDC-PAS(ART-IX)         
201000                 ELSE                                                     
201100                    ADD IN-KVOT-INNEV(KVOI-IX)                            
201200                                   TO WS-ART-KVOI-SAK(ART-IX)             
201300                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
201400                                   TO WS-ART-KVOI-CDC-SAK(ART-IX)         
201500                 END-IF                                                   
201600              END-IF                                                      
201700           END-IF                                                         
201800        END-IF                                                            
201900        ADD +1 TO KVOI-IX                                                 
202000     END-PERFORM                                                          
202100                                                                          
202200     IF SW-KVOI-TRAFF = NEJ                                               
202300        MOVE D-VECKA-VECKA TO KVOI-IX                                     
202400        IF IN-KDREFSTA = 'A'                                              
202500           ADD IN-KVOT-RULL(KVOI-IX)                                      
202600                              TO WS-ART-KVOI-AKT(ART-IX)                  
202700           ADD IN-KVOT-CDC-RULL(KVOI-IX)                                  
202800                              TO WS-ART-KVOI-CDC-AKT(ART-IX)              
202900        ELSE                                                              
203000           IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                        
203100              ADD IN-KVOT-RULL(KVOI-IX)                                   
203200                              TO WS-ART-KVOI-TEO(ART-IX)                  
203300              ADD IN-KVOT-CDC-RULL(KVOI-IX)                               
203400                              TO WS-ART-KVOI-CDC-TEO(ART-IX)              
203500           ELSE                                                           
203600              IF IN-KDREFSTA = 'P'                                        
203700                 ADD IN-KVOT-RULL(KVOI-IX)                                
203800                                 TO WS-ART-KVOI-PAS(ART-IX)               
203900                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
204000                                 TO WS-ART-KVOI-CDC-PAS(ART-IX)           
204100              ELSE                                                        
204200                 ADD IN-KVOT-RULL(KVOI-IX)                                
204300                                 TO WS-ART-KVOI-SAK(ART-IX)               
204400                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
204500                                 TO WS-ART-KVOI-CDC-SAK(ART-IX)           
204600              END-IF                                                      
204700           END-IF                                                         
204800        END-IF                                                            
204900     END-IF                                                               
205000                                                                          
205100*********  SERVICEGRAD OCH SPLITFAKTOR FRÅN SRS                           
205200                                                                          
205300     MOVE IN-SUINKORD    TO WS-FIXAD-SUMMA                                
205400     ADD WS-FIXAD-SUMMA  TO WS-ART-SUINKORD(ART-IX)                       
205500     ADD IN-SUFYSAVP     TO WS-ART-SUFYSAVP(ART-IX)                       
205600     ADD IN-SUAVBRP      TO WS-ART-SUAVBRP (ART-IX)                       
205700     MOVE IN-SULAGERB    TO WS-FIXAD-SUMMA                                
205800     ADD WS-FIXAD-SUMMA  TO WS-ART-SULAGERB(ART-IX)                       
205900     MOVE IN-SUSORTB     TO WS-FIXAD-SUMMA                                
206000     ADD WS-FIXAD-SUMMA  TO WS-ART-SUSORTB (ART-IX)                       
206100     .                                                                    
206200     EJECT                                                                
206300 BCC-UPPDAT-SAKN-ART SECTION.                                             
206400                                                                          
206500     MOVE +1 TO KVOI-IX                                                   
206600     MOVE NEJ TO SW-KVOI-TRAFF                                            
206700     PERFORM UNTIL KVOI-IX > 5                                            
206800        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
206900           MOVE JA TO SW-KVOI-TRAFF                                       
207000           ADD IN-KVOT-INNEV(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7              
207100           ADD IN-KVOT-CDC-INNEV(KVOI-IX)                                 
207200                                    TO WS-KVOT-CDC-SAKNAS-WDK7            
207300        END-IF                                                            
207400        ADD +1 TO KVOI-IX                                                 
207500     END-PERFORM                                                          
207600                                                                          
207700     IF SW-KVOI-TRAFF = NEJ                                               
207800        MOVE D-VECKA-VECKA TO KVOI-IX                                     
207900        ADD IN-KVOT-RULL(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7                  
208000        ADD IN-KVOT-CDC-RULL(KVOI-IX) TO WS-KVOT-CDC-SAKNAS-WDK7          
208100     END-IF                                                               
208200     .                                                                    
208300     EJECT                                                                
208400 BD-SUMMERA SECTION.                                                      
208500                                                                          
208600     PERFORM BDA-SUMMERA-RUTA                                             
208700     PERFORM BDB-SUMMERA-PRISKLASS                                        
208800     PERFORM BDC-SUMMERA-FREKVENSKLASS                                    
208900     PERFORM BDD-SUMMERA-TOTAL                                            
209000     PERFORM BDE-BERAKNINGAR-AV-TOTAL                                     
209100     .                                                                    
209200     EJECT                                                                
209300 BDA-SUMMERA-RUTA SECTION.                                                
209400                                                                          
209500     MOVE +1  TO ART-IX                                                   
209600     MOVE +72 TO ART-IX-MAX                                               
209700     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
209800                                                                          
209900*********  DISP LAGER VÄRDE/RUTA                                          
210000        IF WS-ART-KVDISP-PR-AKT(ART-IX) = ZERO                            
210100           CONTINUE                                                       
210200        ELSE                                                              
210300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
210400                          WS-ART-KVDISP-PR-AKT(ART-IX) / 1000             
210500           MOVE WS-SUMMA-KR TO ART-KVDISP-AKT(ART-IX)                     
210600        END-IF                                                            
210700                                                                          
210800        IF WS-ART-KVDISP-PR-PAS(ART-IX) = ZERO                            
210900           CONTINUE                                                       
211000        ELSE                                                              
211100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
211200                          WS-ART-KVDISP-PR-PAS(ART-IX) / 1000             
211300           MOVE WS-SUMMA-KR TO ART-KVDISP-PAS(ART-IX)                     
211400        END-IF                                                            
211500                                                                          
211600*********  LAGERVÄRDE/RUTA                                                
211700        IF WS-ART-LS-PR-AKT(ART-IX) = ZERO                                
211800           CONTINUE                                                       
211900        ELSE                                                              
212000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
212100                          WS-ART-LS-PR-AKT(ART-IX) / 1000                 
212200           MOVE WS-SUMMA-KR TO ART-LS-AKT(ART-IX)                         
212300        END-IF                                                            
212400                                                                          
212500        IF WS-ART-LS-PR-PAS(ART-IX) = ZERO                                
212600           CONTINUE                                                       
212700        ELSE                                                              
212800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
212900                          WS-ART-LS-PR-PAS(ART-IX) / 1000                 
213000           MOVE WS-SUMMA-KR TO ART-LS-PAS(ART-IX)                         
213100        END-IF                                                            
213200                                                                          
213300*********  AK-VÄRDE/RUTA                                                  
213400        IF WS-ART-AK-PR-AKT(ART-IX) = ZERO                                
213500           CONTINUE                                                       
213600        ELSE                                                              
213700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
213800                          WS-ART-AK-PR-AKT(ART-IX) / 1000                 
213900           MOVE WS-SUMMA-KR TO ART-AK-AKT(ART-IX)                         
214000        END-IF                                                            
214100                                                                          
214200        IF WS-ART-AK-PR-PAS(ART-IX) = ZERO                                
214300           CONTINUE                                                       
214400        ELSE                                                              
214500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
214600                          WS-ART-AK-PR-PAS(ART-IX) / 1000                 
214700           MOVE WS-SUMMA-KR TO ART-AK-PAS(ART-IX)                         
214800        END-IF                                                            
214900                                                                          
215000*********  SÄKERHETSLAGER/RUTA                                            
215100        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
215200           CONTINUE                                                       
215300        ELSE                                                              
215400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
215500                          WS-ART-SLAGER(ART-IX) / 1000                    
215600           MOVE WS-SUMMA-KR TO ART-SLAGER(ART-IX)                         
215700        END-IF                                                            
215800                                                                          
215900*********  ÖVERLAGER/RUTA                                                 
216000        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
216100           CONTINUE                                                       
216200        ELSE                                                              
216300           COMPUTE WS-SUMMA-KR ROUNDED                                    
216400                              = WS-ART-OLAGER(ART-IX) / 1000              
216500           MOVE WS-SUMMA-KR TO ART-OLAGER(ART-IX)                         
216600        END-IF                                                            
216700                                                                          
216800*********  MEDELLAGER/RUTA                                                
216900        IF WS-ART-MLAGER(ART-IX) = ZERO                                   
217000           CONTINUE                                                       
217100        ELSE                                                              
217200           COMPUTE WS-SUMMA-KR =                                          
217300                                WS-ART-MLAGER(ART-IX) / 1000              
217400           ADD WS-SUMMA-KR TO ART-MLAGER(ART-IX)                          
217500        END-IF                                                            
217600                                                                          
217700*********  OMSHASTIGHET/RUTA                                              
217800        COMPUTE WS-SUMMA = WS-ART-KVDISP-PR-AKT(ART-IX) +                 
217900                           WS-ART-KVDISP-PR-PAS(ART-IX)                   
218000        IF WS-SUMMA = ZERO                                                
218100           CONTINUE                                                       
218200        ELSE                                                              
218300           COMPUTE WS-OMSHAST ROUNDED =                                   
218400               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
218500           MOVE WS-OMSHAST TO ART-OMSHAST-DISP(ART-IX)                    
218600        END-IF                                                            
218700                                                                          
218800        COMPUTE WS-SUMMA = WS-ART-LS-PR-AKT(ART-IX) +                     
218900                           WS-ART-AK-PR-AKT(ART-IX) +                     
219000                           WS-ART-LS-PR-PAS(ART-IX) +                     
219100                           WS-ART-AK-PR-PAS(ART-IX)                       
219200        IF WS-SUMMA = ZERO                                                
219300           CONTINUE                                                       
219400        ELSE                                                              
219500           COMPUTE WS-OMSHAST ROUNDED =                                   
219600               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
219700           MOVE WS-OMSHAST TO ART-OMSHAST-LS(ART-IX)                      
219800        END-IF                                                            
219900                                                                          
220000*********  SERVICEGRAD BRUTTO/RUTA                                        
220100        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
220200           MOVE 99.9   TO ART-SERVG-BTO(ART-IX)                           
220300        ELSE                                                              
220400           COMPUTE WS-SERVG ROUNDED =                                     
220500             WS-ART-SUAVBRP(ART-IX) * 100 /                               
220600                          WS-ART-SUINKORD(ART-IX)                         
220700           IF WS-SERVG = 100.0                                            
220800              MOVE 99.9 TO ART-SERVG-BTO(ART-IX)                          
220900           ELSE                                                           
221000              MOVE WS-SERVG TO ART-SERVG-BTO(ART-IX)                      
221100           END-IF                                                         
221200        END-IF                                                            
221300                                                                          
221400*********  SERVICEGRAD NETTO/RUTA                                         
221500        IF WS-ART-SUINKORD(ART-IX) = ZERO                                 
221600           MOVE 99.9   TO ART-SERVG-NTO(ART-IX)                           
221700        ELSE                                                              
221800           COMPUTE WS-SERVG ROUNDED =                                     
221900             (WS-ART-SUAVBRP(ART-IX) - WS-ART-SUFYSAVP(ART-IX))           
222000                           * 100 /                                        
222100                          WS-ART-SUINKORD(ART-IX)                         
222200           IF WS-SERVG = 100.0                                            
222300              MOVE 99.9 TO ART-SERVG-NTO(ART-IX)                          
222400           ELSE                                                           
222500              MOVE WS-SERVG TO ART-SERVG-NTO(ART-IX)                      
222600           END-IF                                                         
222700        END-IF                                                            
222800                                                                          
222900*********  SPLITFAKTOR/RUTA                                               
223000        IF (WS-ART-SUINKORD(ART-IX) + WS-ART-SULAGERB (ART-IX) +          
223100            WS-ART-SUSORTB (ART-IX)) = ZERO                               
223200           MOVE 99.9 TO ART-SPLIT(ART-IX)                                 
223300        ELSE                                                              
223400           COMPUTE WS-SERVG ROUNDED = (WS-ART-SUINKORD(ART-IX) +          
223500                         WS-ART-SULAGERB (ART-IX)) * 100                  
223600                        / (WS-ART-SUINKORD(ART-IX) +                      
223700                           WS-ART-SULAGERB(ART-IX) +                      
223800                           WS-ART-SUSORTB(ART-IX))                        
223900           IF WS-SERVG = 100.0                                            
224000              MOVE 99.9 TO ART-SPLIT(ART-IX)                              
224100           ELSE                                                           
224200              MOVE WS-SERVG TO ART-SPLIT(ART-IX)                          
224300           END-IF                                                         
224400        END-IF                                                            
224500*********  ORDERTRÄFFAR/RUTA                                              
224600*       COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
224700*                                   WS-ART-KVOI-PAS(ART-IX) +             
224800*                                   WS-ART-KVOI-TEO(ART-IX) +             
224900*                                   WS-ART-KVOI-SAK(ART-IX)               
225000*       ADD WS-KVOI-TOT-VECKA TO ART-KVOT(ART-IX)                         
225100                                                                          
225200        MOVE WS-ART-SUINKORD(ART-IX) TO ART-KVOT(ART-IX)                  
225300                                                                          
225400        ADD +1  TO ART-IX                                                 
225500     END-PERFORM                                                          
225600     .                                                                    
225700     EJECT                                                                
225800 BDB-SUMMERA-PRISKLASS SECTION.                                           
225900******************************************************************        
226000* SUMMERING PER PRISKLASS                                        *        
226100******************************************************************        
226200                                                                          
226300     MOVE +1 TO PSUM-IX                                                   
226400                ART-IX                                                    
226500     MOVE +8 TO ART-IX-MAX                                                
226600                                                                          
226700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
226800        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
226900                                                                          
227000          ADD ART-KVANT-AKT(ART-IX) TO PSUM-KVANT-AKT(PSUM-IX)            
227100          ADD ART-KVANT-PAS(ART-IX) TO PSUM-KVANT-PAS(PSUM-IX)            
227200          ADD ART-KVOT(ART-IX)      TO PSUM-KVOT(PSUM-IX)                 
227300          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-PSUM-KVOI-AKT(PSUM-IX)        
227400          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-PSUM-KVOI-PAS(PSUM-IX)        
227500          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-PSUM-KVOI-TEO(PSUM-IX)        
227600          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-PSUM-KVOI-SAK(PSUM-IX)        
227700          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
227800                                  TO WS-PSUM-KVOI-CDC-AKT(PSUM-IX)        
227900          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
228000                                  TO WS-PSUM-KVOI-CDC-PAS(PSUM-IX)        
228100          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
228200                                  TO WS-PSUM-KVOI-CDC-TEO(PSUM-IX)        
228300          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
228400                                  TO WS-PSUM-KVOI-CDC-SAK(PSUM-IX)        
228500          ADD WS-ART-KVOI(ART-IX) TO WS-PSUM-KVOI(PSUM-IX)                
228600          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
228700                             TO WS-PSUM-KVDISP-PR-AKT (PSUM-IX)           
228800          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
228900                             TO WS-PSUM-OK-PR-AKT(PSUM-IX)                
229000          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
229100                             TO WS-PSUM-LS-PR-AKT(PSUM-IX)                
229200          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
229300                             TO WS-PSUM-AK-PR-AKT(PSUM-IX)                
229400          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
229500                             TO WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
229600          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
229700                             TO WS-PSUM-OK-PR-PAS(PSUM-IX)                
229800          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
229900                             TO WS-PSUM-LS-PR-PAS(PSUM-IX)                
230000          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
230100                             TO WS-PSUM-AK-PR-PAS(PSUM-IX)                
230200          ADD WS-ART-OLAGER(ART-IX)   TO WS-PSUM-OLAGER(PSUM-IX)          
230300          ADD WS-ART-MLAGER(ART-IX)   TO WS-PSUM-MLAGER(PSUM-IX)          
230400          ADD WS-ART-SLAGER(ART-IX)   TO WS-PSUM-SLAGER(PSUM-IX)          
230500          ADD WS-ART-SUINKORD(ART-IX) TO WS-PSUM-SUINKORD(PSUM-IX)        
230600          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-PSUM-SUFYSAVP(PSUM-IX)        
230700          ADD WS-ART-SUAVBRP (ART-IX) TO WS-PSUM-SUAVBRP (PSUM-IX)        
230800          ADD WS-ART-SULAGERB(ART-IX) TO WS-PSUM-SULAGERB(PSUM-IX)        
230900          ADD WS-ART-SUSORTB (ART-IX) TO WS-PSUM-SUSORTB (PSUM-IX)        
231000                                                                          
231100          ADD +1 TO ART-IX                                                
231200        END-PERFORM                                                       
231300                                                                          
231400        ADD +1 TO PSUM-IX                                                 
231500        ADD +8 TO ART-IX-MAX                                              
231600     END-PERFORM                                                          
231700                                                                          
231800     MOVE +1 TO PSUM-IX                                                   
231900     MOVE +9 TO PSUM-IX-MAX                                               
232000     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
232100                                                                          
232200*********  DISP LAGER VÄRDE/PRISKLASS                                     
232300        IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) = ZERO                          
232400           CONTINUE                                                       
232500        ELSE                                                              
232600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
232700                     WS-PSUM-KVDISP-PR-AKT(PSUM-IX) / 1000                
232800           MOVE WS-SUMMA-KR TO PSUM-KVDISP-AKT(PSUM-IX)                   
232900        END-IF                                                            
233000                                                                          
233100        IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) = ZERO                          
233200           CONTINUE                                                       
233300        ELSE                                                              
233400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
233500                     WS-PSUM-KVDISP-PR-PAS(PSUM-IX) / 1000                
233600           MOVE WS-SUMMA-KR TO PSUM-KVDISP-PAS(PSUM-IX)                   
233700        END-IF                                                            
233800                                                                          
233900*********  LAGERVÄRDE/PRISKLASS                                           
234000        IF WS-PSUM-LS-PR-AKT(PSUM-IX) = ZERO                              
234100           CONTINUE                                                       
234200        ELSE                                                              
234300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
234400                     WS-PSUM-LS-PR-AKT(PSUM-IX) / 1000                    
234500           MOVE WS-SUMMA-KR TO PSUM-LS-AKT(PSUM-IX)                       
234600        END-IF                                                            
234700                                                                          
234800        IF WS-PSUM-LS-PR-PAS(PSUM-IX) = ZERO                              
234900           CONTINUE                                                       
235000        ELSE                                                              
235100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
235200                     WS-PSUM-LS-PR-PAS(PSUM-IX) / 1000                    
235300           MOVE WS-SUMMA-KR TO PSUM-LS-PAS(PSUM-IX)                       
235400        END-IF                                                            
235500                                                                          
235600*********  AK-VÄRDE/PRISKLASS                                             
235700        IF WS-PSUM-AK-PR-AKT(PSUM-IX) = ZERO                              
235800           CONTINUE                                                       
235900        ELSE                                                              
236000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
236100                     WS-PSUM-AK-PR-AKT(PSUM-IX) / 1000                    
236200           MOVE WS-SUMMA-KR TO PSUM-AK-AKT(PSUM-IX)                       
236300        END-IF                                                            
236400                                                                          
236500        IF WS-PSUM-AK-PR-PAS(PSUM-IX) = ZERO                              
236600           CONTINUE                                                       
236700        ELSE                                                              
236800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
236900                     WS-PSUM-AK-PR-PAS(PSUM-IX) / 1000                    
237000           MOVE WS-SUMMA-KR TO PSUM-AK-PAS(PSUM-IX)                       
237100        END-IF                                                            
237200                                                                          
237300*********  SÄKERHETSLAGER/PRISKLASS                                       
237400        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
237500           CONTINUE                                                       
237600        ELSE                                                              
237700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
237800                          WS-PSUM-SLAGER(PSUM-IX) / 1000                  
237900           MOVE WS-SUMMA-KR TO PSUM-SLAGER(PSUM-IX)                       
238000        END-IF                                                            
238100                                                                          
238200*********  ÖVERLAGER/PRISKLASS                                            
238300        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
238400           CONTINUE                                                       
238500        ELSE                                                              
238600           COMPUTE WS-SUMMA-KR ROUNDED                                    
238700                     = WS-PSUM-OLAGER(PSUM-IX) / 1000                     
238800           MOVE WS-SUMMA-KR TO PSUM-OLAGER(PSUM-IX)                       
238900        END-IF                                                            
239000                                                                          
239100*********  MEDELLAGER/PRISKLASS                                           
239200        IF WS-PSUM-MLAGER(PSUM-IX) = ZERO                                 
239300           CONTINUE                                                       
239400        ELSE                                                              
239500           COMPUTE WS-SUMMA-KR =                                          
239600                       WS-PSUM-MLAGER(PSUM-IX) / 1000                     
239700           ADD WS-SUMMA-KR TO PSUM-MLAGER(PSUM-IX)                        
239800        END-IF                                                            
239900                                                                          
240000*********  SERVICEGRAD BRUTTO/PRISKLASS                                   
240100        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
240200           MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                           
240300        ELSE                                                              
240400           COMPUTE WS-SERVG ROUNDED =                                     
240500             WS-PSUM-SUAVBRP(PSUM-IX) * 100 /                             
240600                          WS-PSUM-SUINKORD(PSUM-IX)                       
240700           IF WS-SERVG = 100.0                                            
240800              MOVE 99.9 TO PSUM-SERVG-BTO(PSUM-IX)                        
240900           ELSE                                                           
241000              MOVE WS-SERVG TO PSUM-SERVG-BTO(PSUM-IX)                    
241100           END-IF                                                         
241200        END-IF                                                            
241300                                                                          
241400*********  SERVICEGRAD NETTO/PRISKLASS                                    
241500        IF WS-PSUM-SUINKORD(PSUM-IX) = ZERO                               
241600           MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                           
241700        ELSE                                                              
241800           COMPUTE WS-SERVG ROUNDED =                                     
241900            (WS-PSUM-SUAVBRP(PSUM-IX) - WS-PSUM-SUFYSAVP(PSUM-IX))        
242000                           * 100 /                                        
242100                          WS-PSUM-SUINKORD(PSUM-IX)                       
242200           IF WS-SERVG = 100.0                                            
242300              MOVE 99.9 TO PSUM-SERVG-NTO(PSUM-IX)                        
242400           ELSE                                                           
242500              MOVE WS-SERVG TO PSUM-SERVG-NTO(PSUM-IX)                    
242600           END-IF                                                         
242700        END-IF                                                            
242800                                                                          
242900*********  SPLITFAKTOR/PRISKLASS                                          
243000        IF (WS-PSUM-SUINKORD(PSUM-IX) +                                   
243100            WS-PSUM-SULAGERB (PSUM-IX) +                                  
243200            WS-PSUM-SUSORTB (PSUM-IX)) = ZERO                             
243300              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
243400        ELSE                                                              
243500           COMPUTE WS-SERVG ROUNDED = (WS-PSUM-SUINKORD(PSUM-IX)          
243600                       + WS-PSUM-SULAGERB (PSUM-IX)) * 100                
243700                        / (WS-PSUM-SUINKORD(PSUM-IX) +                    
243800                           WS-PSUM-SULAGERB(PSUM-IX) +                    
243900                           WS-PSUM-SUSORTB(PSUM-IX))                      
244000           IF WS-SERVG = 100.0                                            
244100              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
244200           ELSE                                                           
244300              MOVE WS-SERVG TO PSUM-SPLIT(PSUM-IX)                        
244400           END-IF                                                         
244500        END-IF                                                            
244600                                                                          
244700*********  OMSHASTIGHET/PRISKLASS                                         
244800        COMPUTE WS-SUMMA = WS-PSUM-KVDISP-PR-AKT(PSUM-IX) +               
244900                           WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                 
245000        IF WS-SUMMA = ZERO                                                
245100           CONTINUE                                                       
245200        ELSE                                                              
245300           COMPUTE WS-OMSHAST ROUNDED =                                   
245400               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
245500           MOVE WS-OMSHAST TO PSUM-OMSHAST-DISP(PSUM-IX)                  
245600        END-IF                                                            
245700                                                                          
245800        COMPUTE WS-SUMMA = WS-PSUM-LS-PR-AKT(PSUM-IX) +                   
245900                           WS-PSUM-AK-PR-AKT(PSUM-IX) +                   
246000                           WS-PSUM-LS-PR-PAS(PSUM-IX) +                   
246100                           WS-PSUM-AK-PR-PAS(PSUM-IX)                     
246200        IF WS-SUMMA = ZERO                                                
246300           CONTINUE                                                       
246400        ELSE                                                              
246500           COMPUTE WS-OMSHAST ROUNDED =                                   
246600               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
246700           MOVE WS-OMSHAST TO PSUM-OMSHAST-LS(PSUM-IX)                    
246800        END-IF                                                            
246900                                                                          
247000        ADD +1 TO PSUM-IX                                                 
247100     END-PERFORM                                                          
247200     .                                                                    
247300     EJECT                                                                
247400 BDC-SUMMERA-FREKVENSKLASS SECTION.                                       
247500******************************************************************        
247600* SUMMERING PER FREKVENSKLASS                                    *        
247700******************************************************************        
247800                                                                          
247900     MOVE +1 TO FSUM-IX                                                   
248000                ART-IX                                                    
248100     MOVE +65 TO ART-IX-MAX                                               
248200     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
248300                                                                          
248400        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
248500          ADD ART-KVANT-AKT(ART-IX) TO FSUM-KVANT-AKT(FSUM-IX)            
248600          ADD ART-KVANT-PAS(ART-IX) TO FSUM-KVANT-PAS(FSUM-IX)            
248700          ADD ART-KVOT(ART-IX)      TO FSUM-KVOT(FSUM-IX)                 
248800          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-FSUM-KVOI-AKT(FSUM-IX)        
248900          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-FSUM-KVOI-PAS(FSUM-IX)        
249000          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-FSUM-KVOI-TEO(FSUM-IX)        
249100          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-FSUM-KVOI-SAK(FSUM-IX)        
249200          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
249300                                  TO WS-FSUM-KVOI-CDC-AKT(FSUM-IX)        
249400          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
249500                                  TO WS-FSUM-KVOI-CDC-PAS(FSUM-IX)        
249600          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
249700                                  TO WS-FSUM-KVOI-CDC-TEO(FSUM-IX)        
249800          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
249900                                  TO WS-FSUM-KVOI-CDC-SAK(FSUM-IX)        
250000          ADD WS-ART-KVOI(ART-IX) TO WS-FSUM-KVOI(FSUM-IX)                
250100          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
250200                             TO WS-FSUM-KVDISP-PR-AKT (FSUM-IX)           
250300          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
250400                             TO WS-FSUM-OK-PR-AKT(FSUM-IX)                
250500          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
250600                             TO WS-FSUM-LS-PR-AKT(FSUM-IX)                
250700          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
250800                             TO WS-FSUM-AK-PR-AKT(FSUM-IX)                
250900          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
251000                             TO WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
251100          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
251200                             TO WS-FSUM-OK-PR-PAS(FSUM-IX)                
251300          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
251400                             TO WS-FSUM-LS-PR-PAS(FSUM-IX)                
251500          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
251600                             TO WS-FSUM-AK-PR-PAS(FSUM-IX)                
251700          ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)            
251800          ADD WS-ART-MLAGER(ART-IX) TO WS-FSUM-MLAGER(FSUM-IX)            
251900          ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)            
252000          ADD WS-ART-SUINKORD(ART-IX) TO WS-FSUM-SUINKORD(FSUM-IX)        
252100          ADD WS-ART-SUFYSAVP(ART-IX) TO WS-FSUM-SUFYSAVP(FSUM-IX)        
252200          ADD WS-ART-SUAVBRP (ART-IX) TO WS-FSUM-SUAVBRP (FSUM-IX)        
252300          ADD WS-ART-SULAGERB(ART-IX) TO WS-FSUM-SULAGERB(FSUM-IX)        
252400          ADD WS-ART-SUSORTB (ART-IX) TO WS-FSUM-SUSORTB (FSUM-IX)        
252500                                                                          
252600          ADD +8 TO ART-IX                                                
252700        END-PERFORM                                                       
252800        ADD +1 TO FSUM-IX                                                 
252900                                                                          
253000        EVALUATE TRUE                                                     
253100           WHEN  FSUM-IX = 1                                              
253200                 MOVE +1 TO ART-IX                                        
253300           WHEN  FSUM-IX = 2                                              
253400                 MOVE +2 TO ART-IX                                        
253500                 MOVE +66 TO ART-IX-MAX                                   
253600           WHEN  FSUM-IX = 3                                              
253700                 MOVE +3 TO ART-IX                                        
253800                 MOVE +67 TO ART-IX-MAX                                   
253900           WHEN  FSUM-IX = 4                                              
254000                 MOVE +4 TO ART-IX                                        
254100                 MOVE +68 TO ART-IX-MAX                                   
254200           WHEN  FSUM-IX = 5                                              
254300                 MOVE +5 TO ART-IX                                        
254400                 MOVE +69 TO ART-IX-MAX                                   
254500           WHEN  FSUM-IX = 6                                              
254600                 MOVE +6 TO ART-IX                                        
254700                 MOVE +70 TO ART-IX-MAX                                   
254800           WHEN  FSUM-IX = 7                                              
254900                 MOVE +7 TO ART-IX                                        
255000                 MOVE +71 TO ART-IX-MAX                                   
255100           WHEN  FSUM-IX = 8                                              
255200                 MOVE +8 TO ART-IX                                        
255300                 MOVE +72 TO ART-IX-MAX                                   
255400           WHEN OTHER                                                     
255500                CONTINUE                                                  
255600        END-EVALUATE                                                      
255700                                                                          
255800     END-PERFORM                                                          
255900                                                                          
256000     MOVE +1 TO FSUM-IX                                                   
256100     MOVE +8 TO FSUM-IX-MAX                                               
256200     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
256300                                                                          
256400*********  DISP LAGER VÄRDE/FREKVENSKLASS                                 
256500        IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) = ZERO                          
256600           CONTINUE                                                       
256700        ELSE                                                              
256800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
256900                     WS-FSUM-KVDISP-PR-AKT(FSUM-IX) / 1000                
257000           MOVE WS-SUMMA-KR TO FSUM-KVDISP-AKT(FSUM-IX)                   
257100        END-IF                                                            
257200                                                                          
257300        IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) = ZERO                          
257400           CONTINUE                                                       
257500        ELSE                                                              
257600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
257700                     WS-FSUM-KVDISP-PR-PAS(FSUM-IX) / 1000                
257800           MOVE WS-SUMMA-KR TO FSUM-KVDISP-PAS(FSUM-IX)                   
257900        END-IF                                                            
258000                                                                          
258100*********  LAGERVÄRDE/FREKVENSKLASS                                       
258200        IF WS-FSUM-LS-PR-AKT(FSUM-IX) = ZERO                              
258300           CONTINUE                                                       
258400        ELSE                                                              
258500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
258600                     WS-FSUM-LS-PR-AKT(FSUM-IX) / 1000                    
258700           MOVE WS-SUMMA-KR TO FSUM-LS-AKT(FSUM-IX)                       
258800        END-IF                                                            
258900                                                                          
259000        IF WS-FSUM-LS-PR-PAS(FSUM-IX) = ZERO                              
259100           CONTINUE                                                       
259200        ELSE                                                              
259300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
259400                     WS-FSUM-LS-PR-PAS(FSUM-IX) / 1000                    
259500           MOVE WS-SUMMA-KR TO FSUM-LS-PAS(FSUM-IX)                       
259600        END-IF                                                            
259700                                                                          
259800*********  AK-VÄRDE/FREKVENSKLASS                                         
259900        IF WS-FSUM-AK-PR-AKT(FSUM-IX) = ZERO                              
260000           CONTINUE                                                       
260100        ELSE                                                              
260200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
260300                     WS-FSUM-AK-PR-AKT(FSUM-IX) / 1000                    
260400           MOVE WS-SUMMA-KR TO FSUM-AK-AKT(FSUM-IX)                       
260500        END-IF                                                            
260600                                                                          
260700        IF WS-FSUM-AK-PR-PAS(FSUM-IX) = ZERO                              
260800           CONTINUE                                                       
260900        ELSE                                                              
261000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
261100                     WS-FSUM-AK-PR-PAS(FSUM-IX) / 1000                    
261200           MOVE WS-SUMMA-KR TO FSUM-AK-PAS(FSUM-IX)                       
261300        END-IF                                                            
261400                                                                          
261500*********  SÄKERHETSLAGER/FREKVENSKLASS                                   
261600        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
261700           CONTINUE                                                       
261800        ELSE                                                              
261900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
262000                          WS-FSUM-SLAGER(FSUM-IX) / 1000                  
262100           MOVE WS-SUMMA-KR TO FSUM-SLAGER(FSUM-IX)                       
262200        END-IF                                                            
262300                                                                          
262400*********  ÖVERLAGER/FREKVENSKLASS                                        
262500        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
262600           CONTINUE                                                       
262700        ELSE                                                              
262800           COMPUTE WS-SUMMA-KR ROUNDED                                    
262900                     = WS-FSUM-OLAGER(FSUM-IX) / 1000                     
263000           MOVE WS-SUMMA-KR TO FSUM-OLAGER(FSUM-IX)                       
263100        END-IF                                                            
263200                                                                          
263300*********  MEDELLAGER/FREKVENSKLASS                                       
263400        IF WS-FSUM-MLAGER(FSUM-IX) = ZERO                                 
263500           CONTINUE                                                       
263600        ELSE                                                              
263700           COMPUTE WS-SUMMA-KR =                                          
263800                       WS-FSUM-MLAGER(FSUM-IX) / 1000                     
263900           ADD WS-SUMMA-KR TO FSUM-MLAGER(FSUM-IX)                        
264000        END-IF                                                            
264100                                                                          
264200*********  SERVICEGRAD BRUTTO/FREKVENSKLASS                               
264300        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
264400           MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                           
264500        ELSE                                                              
264600           COMPUTE WS-SERVG ROUNDED =                                     
264700             WS-FSUM-SUAVBRP(FSUM-IX) * 100 /                             
264800                          WS-FSUM-SUINKORD(FSUM-IX)                       
264900           IF WS-SERVG = 100.0                                            
265000              MOVE 99.9 TO FSUM-SERVG-BTO(FSUM-IX)                        
265100           ELSE                                                           
265200              MOVE WS-SERVG TO FSUM-SERVG-BTO(FSUM-IX)                    
265300           END-IF                                                         
265400        END-IF                                                            
265500                                                                          
265600*********  SERVICEGRAD NETTO/PRISKLASS                                    
265700        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
265800           MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                           
265900        ELSE                                                              
266000           COMPUTE WS-SERVG ROUNDED =                                     
266100            (WS-FSUM-SUAVBRP(FSUM-IX) - WS-FSUM-SUFYSAVP(FSUM-IX))        
266200                           * 100 /                                        
266300                          WS-FSUM-SUINKORD(FSUM-IX)                       
266400           IF WS-SERVG = 100.0                                            
266500              MOVE 99.9 TO FSUM-SERVG-NTO(FSUM-IX)                        
266600           ELSE                                                           
266700              MOVE WS-SERVG TO FSUM-SERVG-NTO(FSUM-IX)                    
266800           END-IF                                                         
266900        END-IF                                                            
267000                                                                          
267100*********  SPLITFAKTOR/FREKVENSKLASS                                      
267200        IF WS-FSUM-SUINKORD(FSUM-IX) = ZERO                               
267300           MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                               
267400        ELSE                                                              
267500           COMPUTE WS-SERVG ROUNDED = (WS-FSUM-SUINKORD(FSUM-IX)          
267600                       + WS-FSUM-SULAGERB (FSUM-IX)) * 100                
267700                        / (WS-FSUM-SUINKORD(FSUM-IX) +                    
267800                           WS-FSUM-SULAGERB(FSUM-IX) +                    
267900                           WS-FSUM-SUSORTB(FSUM-IX))                      
268000           IF WS-SERVG = 100.0                                            
268100              MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                            
268200           ELSE                                                           
268300              MOVE WS-SERVG TO FSUM-SPLIT(FSUM-IX)                        
268400           END-IF                                                         
268500        END-IF                                                            
268600                                                                          
268700*********  OMSHASTIGHET/FREKVENSKLASS                                     
268800        COMPUTE WS-SUMMA = WS-FSUM-KVDISP-PR-AKT(FSUM-IX) +               
268900                           WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                 
269000        IF WS-SUMMA = ZERO                                                
269100           CONTINUE                                                       
269200        ELSE                                                              
269300           COMPUTE WS-OMSHAST ROUNDED =                                   
269400               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
269500           MOVE WS-OMSHAST TO FSUM-OMSHAST-DISP(FSUM-IX)                  
269600        END-IF                                                            
269700                                                                          
269800        COMPUTE WS-SUMMA = WS-FSUM-LS-PR-AKT(FSUM-IX) +                   
269900                           WS-FSUM-AK-PR-AKT(FSUM-IX) +                   
270000                           WS-FSUM-LS-PR-PAS(FSUM-IX) +                   
270100                           WS-FSUM-AK-PR-PAS(FSUM-IX)                     
270200        IF WS-SUMMA = ZERO                                                
270300           CONTINUE                                                       
270400        ELSE                                                              
270500           COMPUTE WS-OMSHAST ROUNDED =                                   
270600               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
270700           MOVE WS-OMSHAST TO FSUM-OMSHAST-LS(FSUM-IX)                    
270800        END-IF                                                            
270900                                                                          
271000        ADD +1 TO FSUM-IX                                                 
271100                                                                          
271200     END-PERFORM                                                          
271300     .                                                                    
271400     EJECT                                                                
271500 BDD-SUMMERA-TOTAL SECTION.                                               
271600******************************************************************        
271700* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
271800******************************************************************        
271900                                                                          
272000     MOVE +1 TO PSUM-IX                                                   
272100     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
272200        ADD PSUM-KVANT-AKT(PSUM-IX) TO TOT-KVANT-AKT                      
272300        ADD PSUM-KVANT-PAS(PSUM-IX) TO TOT-KVANT-PAS                      
272400        ADD PSUM-KVOT(PSUM-IX) TO TOT-KVOT                                
272500        ADD WS-PSUM-KVOI-AKT(PSUM-IX) TO WS-TOT-KVOI-AKT                  
272600        ADD WS-PSUM-KVOI-PAS(PSUM-IX) TO WS-TOT-KVOI-PAS                  
272700        ADD WS-PSUM-KVOI-TEO(PSUM-IX) TO WS-TOT-KVOI-TEO                  
272800        ADD WS-PSUM-KVOI-SAK(PSUM-IX) TO WS-TOT-KVOI-SAK                  
272900        ADD WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                                 
273000                                TO WS-TOT-KVOI-CDC-AKT                    
273100        ADD WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                                 
273200                                TO WS-TOT-KVOI-CDC-PAS                    
273300        ADD WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                                 
273400                                TO WS-TOT-KVOI-CDC-TEO                    
273500        ADD WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                                 
273600                                TO WS-TOT-KVOI-CDC-SAK                    
273700        ADD WS-PSUM-KVOI(PSUM-IX) TO WS-TOT-KVOI                          
273800        ADD WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                                
273900                           TO WS-TOT-KVDISP-PR-AKT                        
274000        ADD WS-PSUM-OK-PR-AKT(PSUM-IX)                                    
274100                           TO WS-TOT-OK-PR-AKT                            
274200        ADD WS-PSUM-LS-PR-AKT(PSUM-IX)                                    
274300                           TO WS-TOT-LS-PR-AKT                            
274400        ADD WS-PSUM-AK-PR-AKT(PSUM-IX)                                    
274500                           TO WS-TOT-AK-PR-AKT                            
274600        ADD WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                                
274700                           TO WS-TOT-KVDISP-PR-PAS                        
274800        ADD WS-PSUM-OK-PR-PAS(PSUM-IX)                                    
274900                           TO WS-TOT-OK-PR-PAS                            
275000        ADD WS-PSUM-LS-PR-PAS(PSUM-IX)                                    
275100                           TO WS-TOT-LS-PR-PAS                            
275200        ADD WS-PSUM-AK-PR-PAS(PSUM-IX)                                    
275300                           TO WS-TOT-AK-PR-PAS                            
275400        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
275500        ADD WS-PSUM-MLAGER(PSUM-IX) TO WS-TOT-MLAGER                      
275600        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
275700        ADD WS-PSUM-SUINKORD(PSUM-IX) TO WS-TOT-SUINKORD                  
275800        ADD WS-PSUM-SUFYSAVP(PSUM-IX) TO WS-TOT-SUFYSAVP                  
275900        ADD WS-PSUM-SUAVBRP (PSUM-IX) TO WS-TOT-SUAVBRP                   
276000        ADD WS-PSUM-SULAGERB(PSUM-IX) TO WS-TOT-SULAGERB                  
276100        ADD WS-PSUM-SUSORTB (PSUM-IX) TO WS-TOT-SUSORTB                   
276200                                                                          
276300        ADD +1 TO PSUM-IX                                                 
276400     END-PERFORM                                                          
276500                                                                          
276600*********  DISP LAGER VÄRDE TOTALT                                        
276700        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
276800           CONTINUE                                                       
276900        ELSE                                                              
277000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
277100                     WS-TOT-KVDISP-PR-AKT / 1000                          
277200           MOVE WS-SUMMA-KR TO TOT-KVDISP-AKT                             
277300        END-IF                                                            
277400                                                                          
277500        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
277600           CONTINUE                                                       
277700        ELSE                                                              
277800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
277900                     WS-TOT-KVDISP-PR-PAS / 1000                          
278000           MOVE WS-SUMMA-KR TO TOT-KVDISP-PAS                             
278100        END-IF                                                            
278200                                                                          
278300*********  LAGERVÄRDE TOTALT                                              
278400        IF WS-TOT-LS-PR-AKT = ZERO                                        
278500           CONTINUE                                                       
278600        ELSE                                                              
278700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
278800                     WS-TOT-LS-PR-AKT / 1000                              
278900           MOVE WS-SUMMA-KR TO TOT-LS-AKT                                 
279000        END-IF                                                            
279100                                                                          
279200        IF WS-TOT-LS-PR-PAS = ZERO                                        
279300           CONTINUE                                                       
279400        ELSE                                                              
279500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
279600                     WS-TOT-LS-PR-PAS / 1000                              
279700           MOVE WS-SUMMA-KR TO TOT-LS-PAS                                 
279800        END-IF                                                            
279900                                                                          
280000*********  AK-VÄRDE TOTALT                                                
280100        IF WS-TOT-AK-PR-AKT = ZERO                                        
280200           CONTINUE                                                       
280300        ELSE                                                              
280400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
280500                     WS-TOT-AK-PR-AKT / 1000                              
280600           MOVE WS-SUMMA-KR TO TOT-AK-AKT                                 
280700        END-IF                                                            
280800                                                                          
280900        IF WS-TOT-AK-PR-PAS = ZERO                                        
281000           CONTINUE                                                       
281100        ELSE                                                              
281200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
281300                     WS-TOT-AK-PR-PAS / 1000                              
281400           MOVE WS-SUMMA-KR TO TOT-AK-PAS                                 
281500        END-IF                                                            
281600                                                                          
281700*********  SÄKERHETSLAGER TOTALT                                          
281800        IF WS-TOT-SLAGER = ZERO                                           
281900           CONTINUE                                                       
282000        ELSE                                                              
282100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
282200                          WS-TOT-SLAGER / 1000                            
282300           MOVE WS-SUMMA-KR TO TOT-SLAGER                                 
282400        END-IF                                                            
282500                                                                          
282600*********  ÖVERLAGER TOTALT                                               
282700        IF WS-TOT-OLAGER = ZERO                                           
282800           CONTINUE                                                       
282900        ELSE                                                              
283000           COMPUTE WS-SUMMA-KR ROUNDED                                    
283100                     = WS-TOT-OLAGER / 1000                               
283200           MOVE WS-SUMMA-KR TO TOT-OLAGER                                 
283300        END-IF                                                            
283400                                                                          
283500*********  MEDELLAGER TOTALT                                              
283600        IF WS-TOT-MLAGER = ZERO                                           
283700           CONTINUE                                                       
283800        ELSE                                                              
283900           COMPUTE WS-SUMMA-KR =                                          
284000                       WS-TOT-MLAGER / 1000                               
284100           ADD WS-SUMMA-KR TO TOT-MLAGER                                  
284200        END-IF                                                            
284300                                                                          
284400*********  SERVICEGRAD BRUTTO TOTALT                                      
284500        IF WS-TOT-SUINKORD = ZERO                                         
284600           MOVE 99.9 TO TOT-SERVG-BTO                                     
284700        ELSE                                                              
284800           COMPUTE WS-SERVG ROUNDED =                                     
284900             WS-TOT-SUAVBRP * 100 /                                       
285000                          WS-TOT-SUINKORD                                 
285100           IF WS-SERVG = 100.0                                            
285200              MOVE 99.9 TO TOT-SERVG-BTO                                  
285300           ELSE                                                           
285400              MOVE WS-SERVG TO TOT-SERVG-BTO                              
285500           END-IF                                                         
285600        END-IF                                                            
285700                                                                          
285800*********  SERVICEGRAD NETTO TOTALT                                       
285900        IF WS-TOT-SUINKORD = ZERO                                         
286000           MOVE 99.9 TO TOT-SERVG-BTO                                     
286100        ELSE                                                              
286200           COMPUTE WS-SERVG ROUNDED =                                     
286300            (WS-TOT-SUAVBRP - WS-TOT-SUFYSAVP)                            
286400                           * 100 /                                        
286500                          WS-TOT-SUINKORD                                 
286600           IF WS-SERVG = 100.0                                            
286700              MOVE 99.9 TO TOT-SERVG-NTO                                  
286800           ELSE                                                           
286900              MOVE WS-SERVG TO TOT-SERVG-NTO                              
287000           END-IF                                                         
287100        END-IF                                                            
287200                                                                          
287300*********  SPLITFAKTOR TOTALT                                             
287400        IF WS-TOT-SUINKORD = ZERO                                         
287500           MOVE 99.9 TO TOT-SPLIT                                         
287600        ELSE                                                              
287700           COMPUTE WS-SERVG ROUNDED = (WS-TOT-SUINKORD                    
287800                       + WS-TOT-SULAGERB) * 100                           
287900                        / (WS-TOT-SUINKORD +                              
288000                           WS-TOT-SULAGERB +                              
288100                           WS-TOT-SUSORTB)                                
288200           IF WS-SERVG = 100.0                                            
288300              MOVE 99.9 TO TOT-SPLIT                                      
288400           ELSE                                                           
288500              MOVE WS-SERVG TO TOT-SPLIT                                  
288600           END-IF                                                         
288700        END-IF                                                            
288800                                                                          
288900*********  OMSHASTIGHET TOTALT                                            
289000        COMPUTE WS-SUMMA = WS-TOT-KVDISP-PR-AKT +                         
289100                           WS-TOT-KVDISP-PR-PAS                           
289200        IF WS-SUMMA = ZERO                                                
289300           CONTINUE                                                       
289400        ELSE                                                              
289500           COMPUTE WS-OMSHAST ROUNDED =                                   
289600               WS-TOT-KVOI / WS-SUMMA                                     
289700           MOVE WS-OMSHAST TO TOT-OMSHAST-DISP                            
289800        END-IF                                                            
289900                                                                          
290000        COMPUTE WS-SUMMA = WS-TOT-LS-PR-AKT +                             
290100                           WS-TOT-AK-PR-AKT +                             
290200                           WS-TOT-LS-PR-PAS +                             
290300                           WS-TOT-AK-PR-PAS                               
290400        IF WS-SUMMA = ZERO                                                
290500           CONTINUE                                                       
290600        ELSE                                                              
290700           COMPUTE WS-OMSHAST ROUNDED =                                   
290800               WS-TOT-KVOI / WS-SUMMA                                     
290900           MOVE WS-OMSHAST TO TOT-OMSHAST-LS                              
291000                                                                          
291100        END-IF                                                            
291200*********  ORDERTRÄFFAR TOTALT                                            
291300                                                                          
291400        ADD WS-KVOT-SAKNAS-WDK7 TO TOT-KVOT                               
291500     .                                                                    
291600     EJECT                                                                
291700 BDE-BERAKNINGAR-AV-TOTAL SECTION.                                        
291800******************************************************************        
291900* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
292000******************************************************************        
292100                                                                          
292200     MOVE +1 TO ART-IX                                                    
292300     MOVE +72 TO ART-IX-MAX                                               
292400     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
292500                                                                          
292600******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
292700        IF TOT-KVANT-AKT = ZERO                                           
292800           CONTINUE                                                       
292900        ELSE                                                              
293000           COMPUTE WS-PROC = ART-KVANT-AKT(ART-IX)                        
293100                                    * 100 / TOT-KVANT-AKT                 
293200           MOVE WS-PROC TO ART-PROC-KVANT-A(ART-IX)                       
293300        END-IF                                                            
293400                                                                          
293500******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
293600        IF TOT-KVANT-PAS = ZERO                                           
293700           CONTINUE                                                       
293800        ELSE                                                              
293900           COMPUTE WS-PROC = ART-KVANT-PAS(ART-IX)                        
294000                                    * 100 / TOT-KVANT-PAS                 
294100           MOVE WS-PROC TO ART-PROC-KVANT-P(ART-IX)                       
294200        END-IF                                                            
294300                                                                          
294400******** % ANTAL ORDERTRÄFFAR AV TOTAL                                    
294500        IF TOT-KVOT = ZERO                                                
294600           CONTINUE                                                       
294700        ELSE                                                              
294800           COMPUTE WS-PROC = ART-KVOT(ART-IX)                             
294900                                    * 100 / TOT-KVOT                      
295000           MOVE WS-PROC TO ART-PROC-KVOT(ART-IX)                          
295100        END-IF                                                            
295200                                                                          
295300*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  AKTIVA                      
295400                                                                          
295500        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
295600           CONTINUE                                                       
295700        ELSE                                                              
295800           IF WS-ART-KVDISP-PR-AKT(ART-IX) > ZERO                         
295900              COMPUTE WS-PROC = WS-ART-KVDISP-PR-AKT(ART-IX)              
296000                              * 100 / WS-TOT-KVDISP-PR-AKT                
296100              MOVE WS-PROC TO ART-PROC-KVDISP-A(ART-IX)                   
296200           END-IF                                                         
296300        END-IF                                                            
296400                                                                          
296500*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  PASSIVA                     
296600                                                                          
296700        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
296800           CONTINUE                                                       
296900        ELSE                                                              
297000           IF WS-ART-KVDISP-PR-PAS(ART-IX) > ZERO                         
297100              COMPUTE WS-PROC = WS-ART-KVDISP-PR-PAS(ART-IX)              
297200                              * 100 / WS-TOT-KVDISP-PR-PAS                
297300              MOVE WS-PROC TO ART-PROC-KVDISP-P(ART-IX)                   
297400           END-IF                                                         
297500        END-IF                                                            
297600                                                                          
297700*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                      
297800                                                                          
297900        IF WS-TOT-LS-PR-AKT = ZERO                                        
298000           CONTINUE                                                       
298100        ELSE                                                              
298200           IF WS-ART-LS-PR-AKT(ART-IX) > ZERO                             
298300              COMPUTE WS-PROC = WS-ART-LS-PR-AKT(ART-IX)                  
298400                              * 100 / WS-TOT-LS-PR-AKT                    
298500              MOVE WS-PROC TO ART-PROC-LS-A(ART-IX)                       
298600           END-IF                                                         
298700        END-IF                                                            
298800                                                                          
298900*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA                     
299000                                                                          
299100        IF WS-TOT-LS-PR-PAS = ZERO                                        
299200           CONTINUE                                                       
299300        ELSE                                                              
299400           IF WS-ART-LS-PR-PAS(ART-IX) > ZERO                             
299500              COMPUTE WS-PROC = WS-ART-LS-PR-PAS(ART-IX)                  
299600                              * 100 / WS-TOT-LS-PR-PAS                    
299700              MOVE WS-PROC TO ART-PROC-LS-P(ART-IX)                       
299800           END-IF                                                         
299900        END-IF                                                            
300000                                                                          
300100*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                          
300200                                                                          
300300        IF WS-TOT-AK-PR-AKT = ZERO                                        
300400           CONTINUE                                                       
300500        ELSE                                                              
300600           IF WS-ART-AK-PR-AKT(ART-IX) > ZERO                             
300700              COMPUTE WS-PROC = WS-ART-AK-PR-AKT(ART-IX)                  
300800                              * 100 / WS-TOT-AK-PR-AKT                    
300900              MOVE WS-PROC TO ART-PROC-AK-A(ART-IX)                       
301000           END-IF                                                         
301100        END-IF                                                            
301200                                                                          
301300*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                         
301400                                                                          
301500        IF WS-TOT-AK-PR-PAS = ZERO                                        
301600           CONTINUE                                                       
301700        ELSE                                                              
301800           IF WS-ART-AK-PR-PAS(ART-IX) > ZERO                             
301900              COMPUTE WS-PROC = WS-ART-AK-PR-PAS(ART-IX)                  
302000                              * 100 / WS-TOT-AK-PR-PAS                    
302100              MOVE WS-PROC TO ART-PROC-AK-P(ART-IX)                       
302200           END-IF                                                         
302300        END-IF                                                            
302400                                                                          
302500        ADD +1 TO ART-IX                                                  
302600     END-PERFORM                                                          
302700                                                                          
302800****************************                                              
302900                                                                          
303000     MOVE +1 TO PSUM-IX                                                   
303100     MOVE +9 TO PSUM-IX-MAX                                               
303200     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
303300                                                                          
303400******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
303500        IF TOT-KVANT-AKT = ZERO                                           
303600           CONTINUE                                                       
303700        ELSE                                                              
303800           COMPUTE WS-PROC = PSUM-KVANT-AKT(PSUM-IX) * 100 /              
303900                           TOT-KVANT-AKT                                  
304000           MOVE WS-PROC TO PSUM-PROC-KVANT-A(PSUM-IX)                     
304100        END-IF                                                            
304200                                                                          
304300******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
304400        IF TOT-KVANT-PAS = ZERO                                           
304500           CONTINUE                                                       
304600        ELSE                                                              
304700           COMPUTE WS-PROC = PSUM-KVANT-PAS(PSUM-IX) * 100 /              
304800                           TOT-KVANT-PAS                                  
304900           MOVE WS-PROC TO PSUM-PROC-KVANT-P(PSUM-IX)                     
305000        END-IF                                                            
305100                                                                          
305200******** % ANTAL ORDERTRÄFFAR AV TOTALA                                   
305300        IF TOT-KVOT = ZERO                                                
305400           CONTINUE                                                       
305500        ELSE                                                              
305600           COMPUTE WS-PROC = PSUM-KVOT(PSUM-IX) * 100 /                   
305700                           TOT-KVOT                                       
305800           MOVE WS-PROC TO PSUM-PROC-KVOT(PSUM-IX)                        
305900        END-IF                                                            
306000                                                                          
306100*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA                
306200                                                                          
306300        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
306400           CONTINUE                                                       
306500        ELSE                                                              
306600           IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) > ZERO                       
306700              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-AKT(PSUM-IX)            
306800                              * 100 / WS-TOT-KVDISP-PR-AKT                
306900              MOVE WS-PROC TO PSUM-PROC-KVDISP-A(PSUM-IX)                 
307000           END-IF                                                         
307100        END-IF                                                            
307200                                                                          
307300*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA               
307400                                                                          
307500        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
307600           CONTINUE                                                       
307700        ELSE                                                              
307800           IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) > ZERO                       
307900              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
308000                              * 100 / WS-TOT-KVDISP-PR-PAS                
308100              MOVE WS-PROC TO PSUM-PROC-KVDISP-P(PSUM-IX)                 
308200           END-IF                                                         
308300        END-IF                                                            
308400                                                                          
308500*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                
308600                                                                          
308700        IF WS-TOT-LS-PR-AKT = ZERO                                        
308800           CONTINUE                                                       
308900        ELSE                                                              
309000           IF WS-PSUM-LS-PR-AKT(PSUM-IX) > ZERO                           
309100              COMPUTE WS-PROC = WS-PSUM-LS-PR-AKT(PSUM-IX)                
309200                              * 100 / WS-TOT-LS-PR-AKT                    
309300              MOVE WS-PROC TO PSUM-PROC-LS-A(PSUM-IX)                     
309400           END-IF                                                         
309500        END-IF                                                            
309600                                                                          
309700*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA               
309800                                                                          
309900        IF WS-TOT-LS-PR-PAS = ZERO                                        
310000           CONTINUE                                                       
310100        ELSE                                                              
310200           IF WS-PSUM-LS-PR-PAS(PSUM-IX) > ZERO                           
310300              COMPUTE WS-PROC = WS-PSUM-LS-PR-PAS(PSUM-IX)                
310400                              * 100 / WS-TOT-LS-PR-PAS                    
310500              MOVE WS-PROC TO PSUM-PROC-LS-P(PSUM-IX)                     
310600           END-IF                                                         
310700        END-IF                                                            
310800                                                                          
310900*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                    
311000                                                                          
311100        IF WS-TOT-AK-PR-AKT = ZERO                                        
311200           CONTINUE                                                       
311300        ELSE                                                              
311400           IF WS-PSUM-AK-PR-AKT(PSUM-IX) > ZERO                           
311500              COMPUTE WS-PROC = WS-PSUM-AK-PR-AKT(PSUM-IX)                
311600                              * 100 / WS-TOT-AK-PR-AKT                    
311700              MOVE WS-PROC TO PSUM-PROC-AK-A(PSUM-IX)                     
311800           END-IF                                                         
311900        END-IF                                                            
312000                                                                          
312100*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                   
312200                                                                          
312300        IF WS-TOT-AK-PR-PAS = ZERO                                        
312400           CONTINUE                                                       
312500        ELSE                                                              
312600           IF WS-PSUM-AK-PR-PAS(PSUM-IX) > ZERO                           
312700              COMPUTE WS-PROC = WS-PSUM-AK-PR-PAS(PSUM-IX)                
312800                              * 100 / WS-TOT-AK-PR-PAS                    
312900              MOVE WS-PROC TO PSUM-PROC-AK-P(PSUM-IX)                     
313000           END-IF                                                         
313100        END-IF                                                            
313200                                                                          
313300        ADD +1 TO PSUM-IX                                                 
313400     END-PERFORM                                                          
313500                                                                          
313600****************************************                                  
313700                                                                          
313800     MOVE +1 TO FSUM-IX                                                   
313900     MOVE +8 TO FSUM-IX-MAX                                               
314000     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
314100                                                                          
314200******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
314300        IF TOT-KVANT-AKT = ZERO                                           
314400           CONTINUE                                                       
314500        ELSE                                                              
314600           COMPUTE WS-PROC = FSUM-KVANT-AKT(FSUM-IX) * 100 /              
314700                           TOT-KVANT-AKT                                  
314800           MOVE WS-PROC TO FSUM-PROC-KVANT-A(FSUM-IX)                     
314900        END-IF                                                            
315000                                                                          
315100******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
315200        IF TOT-KVANT-PAS = ZERO                                           
315300           CONTINUE                                                       
315400        ELSE                                                              
315500           COMPUTE WS-PROC = FSUM-KVANT-PAS(FSUM-IX) * 100 /              
315600                           TOT-KVANT-PAS                                  
315700           MOVE WS-PROC TO FSUM-PROC-KVANT-P(FSUM-IX)                     
315800        END-IF                                                            
315900                                                                          
316000******** % ANTAL ORDERTRÄFFAR TOTALA                                      
316100        IF TOT-KVOT = ZERO                                                
316200           CONTINUE                                                       
316300        ELSE                                                              
316400           COMPUTE WS-PROC = FSUM-KVOT(FSUM-IX) * 100 /                   
316500                           TOT-KVOT                                       
316600           MOVE WS-PROC TO FSUM-PROC-KVOT(FSUM-IX)                        
316700        END-IF                                                            
316800                                                                          
316900*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA            
317000                                                                          
317100        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
317200           CONTINUE                                                       
317300        ELSE                                                              
317400           IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) > ZERO                       
317500              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-AKT(FSUM-IX)            
317600                              * 100 / WS-TOT-KVDISP-PR-AKT                
317700              MOVE WS-PROC TO FSUM-PROC-KVDISP-A(FSUM-IX)                 
317800           END-IF                                                         
317900        END-IF                                                            
318000                                                                          
318100*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA           
318200                                                                          
318300        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
318400           CONTINUE                                                       
318500        ELSE                                                              
318600           IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) > ZERO                       
318700              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
318800                              * 100 / WS-TOT-KVDISP-PR-PAS                
318900              MOVE WS-PROC TO FSUM-PROC-KVDISP-P(FSUM-IX)                 
319000           END-IF                                                         
319100        END-IF                                                            
319200                                                                          
319300*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA            
319400                                                                          
319500        IF WS-TOT-LS-PR-AKT = ZERO                                        
319600           CONTINUE                                                       
319700        ELSE                                                              
319800           IF WS-FSUM-LS-PR-AKT(FSUM-IX) > ZERO                           
319900              COMPUTE WS-PROC = WS-FSUM-LS-PR-AKT(FSUM-IX)                
320000                              * 100 / WS-TOT-LS-PR-AKT                    
320100              MOVE WS-PROC TO FSUM-PROC-LS-A(FSUM-IX)                     
320200           END-IF                                                         
320300        END-IF                                                            
320400                                                                          
320500*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA           
320600                                                                          
320700        IF WS-TOT-LS-PR-PAS = ZERO                                        
320800           CONTINUE                                                       
320900        ELSE                                                              
321000           IF WS-FSUM-LS-PR-PAS(FSUM-IX) > ZERO                           
321100              COMPUTE WS-PROC = WS-FSUM-LS-PR-PAS(FSUM-IX)                
321200                              * 100 / WS-TOT-LS-PR-PAS                    
321300              MOVE WS-PROC TO FSUM-PROC-LS-P(FSUM-IX)                     
321400           END-IF                                                         
321500        END-IF                                                            
321600                                                                          
321700*******  %  FREKVENSSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA               
321800                                                                          
321900        IF WS-TOT-AK-PR-AKT = ZERO                                        
322000           CONTINUE                                                       
322100        ELSE                                                              
322200           IF WS-FSUM-AK-PR-AKT(FSUM-IX) > ZERO                           
322300              COMPUTE WS-PROC = WS-FSUM-AK-PR-AKT(FSUM-IX)                
322400                              * 100 / WS-TOT-AK-PR-AKT                    
322500              MOVE WS-PROC TO FSUM-PROC-AK-A(FSUM-IX)                     
322600           END-IF                                                         
322700        END-IF                                                            
322800                                                                          
322900*******  %  FREKVENSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA               
323000                                                                          
323100        IF WS-TOT-AK-PR-PAS = ZERO                                        
323200           CONTINUE                                                       
323300        ELSE                                                              
323400           IF WS-FSUM-AK-PR-PAS(FSUM-IX) > ZERO                           
323500              COMPUTE WS-PROC = WS-FSUM-AK-PR-PAS(FSUM-IX)                
323600                              * 100 / WS-TOT-AK-PR-PAS                    
323700              MOVE WS-PROC TO FSUM-PROC-AK-P(FSUM-IX)                     
323800           END-IF                                                         
323900        END-IF                                                            
324000                                                                          
324100        ADD +1 TO FSUM-IX                                                 
324200     END-PERFORM                                                          
324300     .                                                                    
324400     EJECT                                                                
324500 C-SKRIV-LISTA SECTION.                                                   
324600******************************************************************        
324700*  SID 1 BESTÅR AV 3 RUTRADER INKL PRISKLASS-TOTAL               *        
324800*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
324900*      3           3 RUTRADER INKL PRISKLASS-TOTAL               *        
325000*      4           1 RUTRAD   FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
325100******************************************************************        
325200                                                                          
325300     MOVE +1 TO IX1                                                       
325400     MOVE +2 TO IX2                                                       
325500     MOVE +3 TO IX3                                                       
325600     MOVE +4 TO IX4                                                       
325700     MOVE +5 TO IX5                                                       
325800     MOVE +6 TO IX6                                                       
325900     MOVE +7 TO IX7                                                       
326000     MOVE +8 TO IX8                                                       
326100     MOVE +1 TO PSUM-IX                                                   
326200                                                                          
326300*********** SKRIVER SID-1                                                 
326400                                                                          
326500     PERFORM S21A-SKRIV-RUBRIKER                                          
326600     MOVE '1' TO W001-DET1-PRISKLASS                                      
326700     PERFORM CA-FLYTTA-SKRIV-RAD                                          
326800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
326900     ADD +1 TO PSUM-IX                                                    
327000     MOVE '2' TO W001-DET1-PRISKLASS                                      
327100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
327200                                                                          
327300     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
327400     ADD +1 TO PSUM-IX                                                    
327500     MOVE '3' TO W001-DET1-PRISKLASS                                      
327600     PERFORM CA-FLYTTA-SKRIV-RAD                                          
327700                                                                          
327800*********** SKRIVER SID-2                                                 
327900     PERFORM S21A-SKRIV-RUBRIKER                                          
328000     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
328100     ADD +1 TO PSUM-IX                                                    
328200     MOVE '4' TO W001-DET1-PRISKLASS                                      
328300     PERFORM CA-FLYTTA-SKRIV-RAD                                          
328400                                                                          
328500     ADD +1 TO PSUM-IX                                                    
328600     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
328700     MOVE '5' TO W001-DET1-PRISKLASS                                      
328800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
328900                                                                          
329000     ADD +1 TO PSUM-IX                                                    
329100     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
329200     MOVE '6' TO W001-DET1-PRISKLASS                                      
329300     PERFORM CA-FLYTTA-SKRIV-RAD                                          
329400                                                                          
329500*********** SKRIVER SID-3                                                 
329600     PERFORM S21A-SKRIV-RUBRIKER                                          
329700     ADD +1 TO PSUM-IX                                                    
329800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
329900     MOVE '7' TO W001-DET1-PRISKLASS                                      
330000     PERFORM CA-FLYTTA-SKRIV-RAD                                          
330100                                                                          
330200     ADD +1 TO PSUM-IX                                                    
330300     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
330400     MOVE '8' TO W001-DET1-PRISKLASS                                      
330500     PERFORM CA-FLYTTA-SKRIV-RAD                                          
330600                                                                          
330700     ADD +1 TO PSUM-IX                                                    
330800     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
330900     MOVE '9' TO W001-DET1-PRISKLASS                                      
331000     PERFORM CA-FLYTTA-SKRIV-RAD                                          
331100                                                                          
331200*********** SKRIVER SID-4                                                 
331300     PERFORM S21A-SKRIV-RUBRIKER                                          
331400     MOVE +1 TO IX1                                                       
331500     MOVE +2 TO IX2                                                       
331600     MOVE +3 TO IX3                                                       
331700     MOVE +4 TO IX4                                                       
331800     MOVE +5 TO IX5                                                       
331900     MOVE +6 TO IX6                                                       
332000     MOVE +7 TO IX7                                                       
332100     MOVE +8 TO IX8                                                       
332200     MOVE SPACE TO W001-DET1-PRISKLASS                                    
332300     PERFORM CB-FLYTTA-SKRIV-TOT                                          
332400     .                                                                    
332500     EJECT                                                                
332600 CA-FLYTTA-SKRIV-RAD SECTION.                                             
332700                                                                          
332800     MOVE ART-KVANT-AKT(IX1)       TO  W001-DET1-KVANTA                   
332900     MOVE ART-PROC-KVANT-A(IX1)    TO  W001-DET1-P-KVANTA                 
333000     MOVE ART-KVANT-AKT(IX2)       TO  W001-DET1-KVANTB                   
333100     MOVE ART-PROC-KVANT-A(IX2)    TO  W001-DET1-P-KVANTB                 
333200     MOVE ART-KVANT-AKT(IX3)       TO  W001-DET1-KVANTC                   
333300     MOVE ART-PROC-KVANT-A(IX3)    TO  W001-DET1-P-KVANTC                 
333400     MOVE ART-KVANT-AKT(IX4)       TO  W001-DET1-KVANTD                   
333500     MOVE ART-PROC-KVANT-A(IX4)    TO  W001-DET1-P-KVANTD                 
333600     MOVE ART-KVANT-AKT(IX5)       TO  W001-DET1-KVANTE                   
333700     MOVE ART-PROC-KVANT-A(IX5)    TO  W001-DET1-P-KVANTE                 
333800     MOVE ART-KVANT-AKT(IX6)       TO  W001-DET1-KVANTF                   
333900     MOVE ART-PROC-KVANT-A(IX6)    TO  W001-DET1-P-KVANTF                 
334000     MOVE ART-KVANT-AKT(IX7)       TO  W001-DET1-KVANTG                   
334100     MOVE ART-PROC-KVANT-A(IX7)    TO  W001-DET1-P-KVANTG                 
334200     MOVE ART-KVANT-AKT(IX8)       TO  W001-DET1-KVANTH                   
334300     MOVE ART-PROC-KVANT-A(IX8)    TO  W001-DET1-P-KVANTH                 
334400     MOVE PSUM-KVANT-AKT(PSUM-IX)  TO  W001-DET1-TOT                      
334500     MOVE PSUM-PROC-KVANT-A(PSUM-IX) TO W001-DET1-P-TOT                   
334600     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
334700     MOVE +2 TO W001-SKIP                                                 
334800     PERFORM S21-SKRIV-LISTA                                              
334900                                                                          
335000     MOVE ART-KVANT-PAS(IX1)       TO  W001-DET2-KVANTA                   
335100     MOVE ART-PROC-KVANT-P(IX1)    TO  W001-DET2-P-KVANTA                 
335200     MOVE ART-KVANT-PAS(IX2)       TO  W001-DET2-KVANTB                   
335300     MOVE ART-PROC-KVANT-P(IX2)    TO  W001-DET2-P-KVANTB                 
335400     MOVE ART-KVANT-PAS(IX3)       TO  W001-DET2-KVANTC                   
335500     MOVE ART-PROC-KVANT-P(IX3)    TO  W001-DET2-P-KVANTC                 
335600     MOVE ART-KVANT-PAS(IX4)       TO  W001-DET2-KVANTD                   
335700     MOVE ART-PROC-KVANT-P(IX4)    TO  W001-DET2-P-KVANTD                 
335800     MOVE ART-KVANT-PAS(IX5)       TO  W001-DET2-KVANTE                   
335900     MOVE ART-PROC-KVANT-P(IX5)    TO  W001-DET2-P-KVANTE                 
336000     MOVE ART-KVANT-PAS(IX6)       TO  W001-DET2-KVANTF                   
336100     MOVE ART-PROC-KVANT-P(IX6)    TO  W001-DET2-P-KVANTF                 
336200     MOVE ART-KVANT-PAS(IX7)       TO  W001-DET2-KVANTG                   
336300     MOVE ART-PROC-KVANT-P(IX7)    TO  W001-DET2-P-KVANTG                 
336400     MOVE ART-KVANT-PAS(IX8)       TO  W001-DET2-KVANTH                   
336500     MOVE ART-PROC-KVANT-P(IX8)    TO  W001-DET2-P-KVANTH                 
336600     MOVE PSUM-KVANT-PAS(PSUM-IX)  TO  W001-DET2-TOT                      
336700     MOVE PSUM-PROC-KVANT-P(PSUM-IX) TO W001-DET2-P-TOT                   
336800     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
336900     MOVE +1 TO W001-SKIP                                                 
337000     PERFORM S21-SKRIV-LISTA                                              
337100                                                                          
337200     MOVE ART-KVDISP-AKT(IX1)      TO  W001-DET3-DLAGERA                  
337300     MOVE ART-PROC-KVDISP-A(IX1)   TO  W001-DET3-P-DLAGERA                
337400     MOVE ART-KVDISP-AKT(IX2)      TO  W001-DET3-DLAGERB                  
337500     MOVE ART-PROC-KVDISP-A(IX2)   TO  W001-DET3-P-DLAGERB                
337600     MOVE ART-KVDISP-AKT(IX3)      TO  W001-DET3-DLAGERC                  
337700     MOVE ART-PROC-KVDISP-A(IX3)   TO  W001-DET3-P-DLAGERC                
337800     MOVE ART-KVDISP-AKT(IX4)      TO  W001-DET3-DLAGERD                  
337900     MOVE ART-PROC-KVDISP-A(IX4)   TO  W001-DET3-P-DLAGERD                
338000     MOVE ART-KVDISP-AKT(IX5)      TO  W001-DET3-DLAGERE                  
338100     MOVE ART-PROC-KVDISP-A(IX5)   TO  W001-DET3-P-DLAGERE                
338200     MOVE ART-KVDISP-AKT(IX6)      TO  W001-DET3-DLAGERF                  
338300     MOVE ART-PROC-KVDISP-A(IX6)   TO  W001-DET3-P-DLAGERF                
338400     MOVE ART-KVDISP-AKT(IX7)      TO  W001-DET3-DLAGERG                  
338500     MOVE ART-PROC-KVDISP-A(IX7)   TO  W001-DET3-P-DLAGERG                
338600     MOVE ART-KVDISP-AKT(IX8)      TO  W001-DET3-DLAGERH                  
338700     MOVE ART-PROC-KVDISP-A(IX8)   TO  W001-DET3-P-DLAGERH                
338800     MOVE PSUM-KVDISP-AKT(PSUM-IX) TO  W001-DET3-TOT                      
338900     MOVE PSUM-PROC-KVDISP-A(PSUM-IX)                                     
339000                                   TO  W001-DET3-P-TOT                    
339100     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
339200     MOVE +1 TO W001-SKIP                                                 
339300     PERFORM S21-SKRIV-LISTA                                              
339400                                                                          
339500     MOVE ART-KVDISP-PAS(IX1)      TO  W001-DET4-DLAGERA                  
339600     MOVE ART-PROC-KVDISP-P(IX1)   TO  W001-DET4-P-DLAGERA                
339700     MOVE ART-KVDISP-PAS(IX2)      TO  W001-DET4-DLAGERB                  
339800     MOVE ART-PROC-KVDISP-P(IX2)   TO  W001-DET4-P-DLAGERB                
339900     MOVE ART-KVDISP-PAS(IX3)      TO  W001-DET4-DLAGERC                  
340000     MOVE ART-PROC-KVDISP-P(IX3)   TO  W001-DET4-P-DLAGERC                
340100     MOVE ART-KVDISP-PAS(IX4)      TO  W001-DET4-DLAGERD                  
340200     MOVE ART-PROC-KVDISP-P(IX4)   TO  W001-DET4-P-DLAGERD                
340300     MOVE ART-KVDISP-PAS(IX5)      TO  W001-DET4-DLAGERE                  
340400     MOVE ART-PROC-KVDISP-P(IX5)   TO  W001-DET4-P-DLAGERE                
340500     MOVE ART-KVDISP-PAS(IX6)      TO  W001-DET4-DLAGERF                  
340600     MOVE ART-PROC-KVDISP-P(IX6)   TO  W001-DET4-P-DLAGERF                
340700     MOVE ART-KVDISP-PAS(IX7)      TO  W001-DET4-DLAGERG                  
340800     MOVE ART-PROC-KVDISP-P(IX7)   TO  W001-DET4-P-DLAGERG                
340900     MOVE ART-KVDISP-PAS(IX8)      TO  W001-DET4-DLAGERH                  
341000     MOVE ART-PROC-KVDISP-P(IX8)   TO  W001-DET4-P-DLAGERH                
341100     MOVE PSUM-KVDISP-PAS(PSUM-IX) TO  W001-DET4-TOT                      
341200     MOVE PSUM-PROC-KVDISP-P(PSUM-IX)                                     
341300                                   TO  W001-DET4-P-TOT                    
341400     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
341500     MOVE +1 TO W001-SKIP                                                 
341600     PERFORM S21-SKRIV-LISTA                                              
341700                                                                          
341800     MOVE ART-LS-AKT(IX1)          TO  W001-DET5-LLAGERA                  
341900     MOVE ART-PROC-LS-A(IX1)       TO  W001-DET5-P-LLAGERA                
342000     MOVE ART-LS-AKT(IX2)          TO  W001-DET5-LLAGERB                  
342100     MOVE ART-PROC-LS-A(IX2)       TO  W001-DET5-P-LLAGERB                
342200     MOVE ART-LS-AKT(IX3)          TO  W001-DET5-LLAGERC                  
342300     MOVE ART-PROC-LS-A(IX3)       TO  W001-DET5-P-LLAGERC                
342400     MOVE ART-LS-AKT(IX4)          TO  W001-DET5-LLAGERD                  
342500     MOVE ART-PROC-LS-A(IX4)       TO  W001-DET5-P-LLAGERD                
342600     MOVE ART-LS-AKT(IX5)          TO  W001-DET5-LLAGERE                  
342700     MOVE ART-PROC-LS-A(IX5)       TO  W001-DET5-P-LLAGERE                
342800     MOVE ART-LS-AKT(IX6)          TO  W001-DET5-LLAGERF                  
342900     MOVE ART-PROC-LS-A(IX6)       TO  W001-DET5-P-LLAGERF                
343000     MOVE ART-LS-AKT(IX7)          TO  W001-DET5-LLAGERG                  
343100     MOVE ART-PROC-LS-A(IX7)       TO  W001-DET5-P-LLAGERG                
343200     MOVE ART-LS-AKT(IX8)          TO  W001-DET5-LLAGERH                  
343300     MOVE ART-PROC-LS-A(IX8)       TO  W001-DET5-P-LLAGERH                
343400     MOVE PSUM-LS-AKT(PSUM-IX)     TO  W001-DET5-TOT                      
343500     MOVE PSUM-PROC-LS-A(PSUM-IX)  TO  W001-DET5-P-TOT                    
343600     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
343700     MOVE +1 TO W001-SKIP                                                 
343800     PERFORM S21-SKRIV-LISTA                                              
343900                                                                          
344000     MOVE ART-LS-PAS(IX1)          TO  W001-DET6-LLAGERA                  
344100     MOVE ART-PROC-LS-P(IX1)       TO  W001-DET6-P-LLAGERA                
344200     MOVE ART-LS-PAS(IX2)          TO  W001-DET6-LLAGERB                  
344300     MOVE ART-PROC-LS-P(IX2)       TO  W001-DET6-P-LLAGERB                
344400     MOVE ART-LS-PAS(IX3)          TO  W001-DET6-LLAGERC                  
344500     MOVE ART-PROC-LS-P(IX3)       TO  W001-DET6-P-LLAGERC                
344600     MOVE ART-LS-PAS(IX4)          TO  W001-DET6-LLAGERD                  
344700     MOVE ART-PROC-LS-P(IX4)       TO  W001-DET6-P-LLAGERD                
344800     MOVE ART-LS-PAS(IX5)          TO  W001-DET6-LLAGERE                  
344900     MOVE ART-PROC-LS-P(IX5)       TO  W001-DET6-P-LLAGERE                
345000     MOVE ART-LS-PAS(IX6)          TO  W001-DET6-LLAGERF                  
345100     MOVE ART-PROC-LS-P(IX6)       TO  W001-DET6-P-LLAGERF                
345200     MOVE ART-LS-PAS(IX7)          TO  W001-DET6-LLAGERG                  
345300     MOVE ART-PROC-LS-P(IX7)       TO  W001-DET6-P-LLAGERG                
345400     MOVE ART-LS-PAS(IX8)          TO  W001-DET6-LLAGERH                  
345500     MOVE ART-PROC-LS-P(IX8)       TO  W001-DET6-P-LLAGERH                
345600     MOVE PSUM-LS-PAS(PSUM-IX)     TO  W001-DET6-TOT                      
345700     MOVE PSUM-PROC-LS-P(PSUM-IX)  TO  W001-DET6-P-TOT                    
345800     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
345900     MOVE +1 TO W001-SKIP                                                 
346000     PERFORM S21-SKRIV-LISTA                                              
346100                                                                          
346200     MOVE ART-AK-AKT(IX1)          TO  W001-DET7-ALAGERA                  
346300     MOVE ART-PROC-AK-A(IX1)       TO  W001-DET7-P-ALAGERA                
346400     MOVE ART-AK-AKT(IX2)          TO  W001-DET7-ALAGERB                  
346500     MOVE ART-PROC-AK-A(IX2)       TO  W001-DET7-P-ALAGERB                
346600     MOVE ART-AK-AKT(IX3)          TO  W001-DET7-ALAGERC                  
346700     MOVE ART-PROC-AK-A(IX3)       TO  W001-DET7-P-ALAGERC                
346800     MOVE ART-AK-AKT(IX4)          TO  W001-DET7-ALAGERD                  
346900     MOVE ART-PROC-AK-A(IX4)       TO  W001-DET7-P-ALAGERD                
347000     MOVE ART-AK-AKT(IX5)          TO  W001-DET7-ALAGERE                  
347100     MOVE ART-PROC-AK-A(IX5)       TO  W001-DET7-P-ALAGERE                
347200     MOVE ART-AK-AKT(IX6)          TO  W001-DET7-ALAGERF                  
347300     MOVE ART-PROC-AK-A(IX6)       TO  W001-DET7-P-ALAGERF                
347400     MOVE ART-AK-AKT(IX7)          TO  W001-DET7-ALAGERG                  
347500     MOVE ART-PROC-AK-A(IX7)       TO  W001-DET7-P-ALAGERG                
347600     MOVE ART-AK-AKT(IX8)          TO  W001-DET7-ALAGERH                  
347700     MOVE ART-PROC-AK-A(IX8)       TO  W001-DET7-P-ALAGERH                
347800     MOVE PSUM-AK-AKT(PSUM-IX)     TO  W001-DET7-TOT                      
347900     MOVE PSUM-PROC-AK-A(PSUM-IX)  TO  W001-DET7-P-TOT                    
348000     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
348100     MOVE +1 TO W001-SKIP                                                 
348200     PERFORM S21-SKRIV-LISTA                                              
348300                                                                          
348400     MOVE ART-AK-PAS(IX1)          TO  W001-DET8-ALAGERA                  
348500     MOVE ART-PROC-AK-P(IX1)       TO  W001-DET8-P-ALAGERA                
348600     MOVE ART-AK-PAS(IX2)          TO  W001-DET8-ALAGERB                  
348700     MOVE ART-PROC-AK-P(IX2)       TO  W001-DET8-P-ALAGERB                
348800     MOVE ART-AK-PAS(IX3)          TO  W001-DET8-ALAGERC                  
348900     MOVE ART-PROC-AK-P(IX3)       TO  W001-DET8-P-ALAGERC                
349000     MOVE ART-AK-PAS(IX4)          TO  W001-DET8-ALAGERD                  
349100     MOVE ART-PROC-AK-P(IX4)       TO  W001-DET8-P-ALAGERD                
349200     MOVE ART-AK-PAS(IX5)          TO  W001-DET8-ALAGERE                  
349300     MOVE ART-PROC-AK-P(IX5)       TO  W001-DET8-P-ALAGERE                
349400     MOVE ART-AK-PAS(IX6)          TO  W001-DET8-ALAGERF                  
349500     MOVE ART-PROC-AK-P(IX6)       TO  W001-DET8-P-ALAGERF                
349600     MOVE ART-AK-PAS(IX7)          TO  W001-DET8-ALAGERG                  
349700     MOVE ART-PROC-AK-P(IX7)       TO  W001-DET8-P-ALAGERG                
349800     MOVE ART-AK-PAS(IX8)          TO  W001-DET8-ALAGERH                  
349900     MOVE ART-PROC-AK-P(IX8)       TO  W001-DET8-P-ALAGERH                
350000     MOVE PSUM-AK-PAS(PSUM-IX)     TO  W001-DET8-TOT                      
350100     MOVE PSUM-PROC-AK-P(PSUM-IX)  TO  W001-DET8-P-TOT                    
350200     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
350300     MOVE +1 TO W001-SKIP                                                 
350400     PERFORM S21-SKRIV-LISTA                                              
350500                                                                          
350600     MOVE ART-OLAGER(IX1)          TO  W001-DET9-OLAGERA                  
350700     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET9-P-OLAGERA                
350800     MOVE ART-OLAGER(IX2)          TO  W001-DET9-OLAGERB                  
350900     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET9-P-OLAGERB                
351000     MOVE ART-OLAGER(IX3)          TO  W001-DET9-OLAGERC                  
351100     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET9-P-OLAGERC                
351200     MOVE ART-OLAGER(IX4)          TO  W001-DET9-OLAGERD                  
351300     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET9-P-OLAGERD                
351400     MOVE ART-OLAGER(IX5)          TO  W001-DET9-OLAGERE                  
351500     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET9-P-OLAGERE                
351600     MOVE ART-OLAGER(IX6)          TO  W001-DET9-OLAGERF                  
351700     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET9-P-OLAGERF                
351800     MOVE ART-OLAGER(IX7)          TO  W001-DET9-OLAGERG                  
351900     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET9-P-OLAGERG                
352000     MOVE ART-OLAGER(IX8)          TO  W001-DET9-OLAGERH                  
352100     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET9-P-OLAGERH                
352200     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET9-TOT                      
352300     MOVE PSUM-PROC-OLAGER(PSUM-IX)                                       
352400                                   TO  W001-DET9-P-TOT                    
352500     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
352600     MOVE +1 TO W001-SKIP                                                 
352700     PERFORM S21-SKRIV-LISTA                                              
352800                                                                          
352900     MOVE ART-SLAGER(IX1)          TO  W001-DET10-SLAGERA                 
353000     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET10-P-SLAGERA               
353100     MOVE ART-SLAGER(IX2)          TO  W001-DET10-SLAGERB                 
353200     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET10-P-SLAGERB               
353300     MOVE ART-SLAGER(IX3)          TO  W001-DET10-SLAGERC                 
353400     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET10-P-SLAGERC               
353500     MOVE ART-SLAGER(IX4)          TO  W001-DET10-SLAGERD                 
353600     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET10-P-SLAGERD               
353700     MOVE ART-SLAGER(IX5)          TO  W001-DET10-SLAGERE                 
353800     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET10-P-SLAGERE               
353900     MOVE ART-SLAGER(IX6)          TO  W001-DET10-SLAGERF                 
354000     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET10-P-SLAGERF               
354100     MOVE ART-SLAGER(IX7)          TO  W001-DET10-SLAGERG                 
354200     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET10-P-SLAGERG               
354300     MOVE ART-SLAGER(IX8)          TO  W001-DET10-SLAGERH                 
354400     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET10-P-SLAGERH               
354500     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET10-TOT                     
354600     MOVE PSUM-PROC-SLAGER(PSUM-IX)                                       
354700                                   TO  W001-DET10-P-TOT                   
354800     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
354900     MOVE +1 TO W001-SKIP                                                 
355000     PERFORM S21-SKRIV-LISTA                                              
355100                                                                          
355200     MOVE ART-MLAGER(IX1)          TO  W001-DET11-MLAGERA                 
355300     MOVE ART-PROC-MLAGER(IX1)     TO  W001-DET11-P-MLAGERA               
355400     MOVE ART-MLAGER(IX2)          TO  W001-DET11-MLAGERB                 
355500     MOVE ART-PROC-MLAGER(IX2)     TO  W001-DET11-P-MLAGERB               
355600     MOVE ART-MLAGER(IX3)          TO  W001-DET11-MLAGERC                 
355700     MOVE ART-PROC-MLAGER(IX3)     TO  W001-DET11-P-MLAGERC               
355800     MOVE ART-MLAGER(IX4)          TO  W001-DET11-MLAGERD                 
355900     MOVE ART-PROC-MLAGER(IX4)     TO  W001-DET11-P-MLAGERD               
356000     MOVE ART-MLAGER(IX5)          TO  W001-DET11-MLAGERE                 
356100     MOVE ART-PROC-MLAGER(IX5)     TO  W001-DET11-P-MLAGERE               
356200     MOVE ART-MLAGER(IX6)          TO  W001-DET11-MLAGERF                 
356300     MOVE ART-PROC-MLAGER(IX6)     TO  W001-DET11-P-MLAGERF               
356400     MOVE ART-MLAGER(IX7)          TO  W001-DET11-MLAGERG                 
356500     MOVE ART-PROC-MLAGER(IX7)     TO  W001-DET11-P-MLAGERG               
356600     MOVE ART-MLAGER(IX8)          TO  W001-DET11-MLAGERH                 
356700     MOVE ART-PROC-MLAGER(IX8)     TO  W001-DET11-P-MLAGERH               
356800     MOVE PSUM-MLAGER(PSUM-IX)     TO  W001-DET11-TOT                     
356900     MOVE PSUM-PROC-MLAGER(PSUM-IX)                                       
357000                                   TO  W001-DET11-P-TOT                   
357100     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
357200     MOVE +1 TO W001-SKIP                                                 
357300     PERFORM S21-SKRIV-LISTA                                              
357400                                                                          
357500     MOVE ART-KVOT(IX1)            TO  W001-DET12-KVOTA                   
357600     MOVE ART-PROC-KVOT(IX1)       TO  W001-DET12-P-KVOTA                 
357700     MOVE ART-KVOT(IX2)            TO  W001-DET12-KVOTB                   
357800     MOVE ART-PROC-KVOT(IX2)       TO  W001-DET12-P-KVOTB                 
357900     MOVE ART-KVOT(IX3)            TO  W001-DET12-KVOTC                   
358000     MOVE ART-PROC-KVOT(IX3)       TO  W001-DET12-P-KVOTC                 
358100     MOVE ART-KVOT(IX4)            TO  W001-DET12-KVOTD                   
358200     MOVE ART-PROC-KVOT(IX4)       TO  W001-DET12-P-KVOTD                 
358300     MOVE ART-KVOT(IX5)            TO  W001-DET12-KVOTE                   
358400     MOVE ART-PROC-KVOT(IX5)       TO  W001-DET12-P-KVOTE                 
358500     MOVE ART-KVOT(IX6)            TO  W001-DET12-KVOTF                   
358600     MOVE ART-PROC-KVOT(IX6)       TO  W001-DET12-P-KVOTF                 
358700     MOVE ART-KVOT(IX7)            TO  W001-DET12-KVOTG                   
358800     MOVE ART-PROC-KVOT(IX7)       TO  W001-DET12-P-KVOTG                 
358900     MOVE ART-KVOT(IX8)            TO  W001-DET12-KVOTH                   
359000     MOVE ART-PROC-KVOT(IX8)       TO  W001-DET12-P-KVOTH                 
359100     MOVE PSUM-KVOT(PSUM-IX)       TO  W001-DET12-TOT                     
359200     MOVE PSUM-PROC-KVOT(PSUM-IX)  TO  W001-DET12-P-TOT                   
359300     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
359400     MOVE +1 TO W001-SKIP                                                 
359500     PERFORM S21-SKRIV-LISTA                                              
359600                                                                          
359700     MOVE ART-SPLIT(IX1)           TO  W001-DET13-SPLITA                  
359800     MOVE ART-SPLIT(IX2)           TO  W001-DET13-SPLITB                  
359900     MOVE ART-SPLIT(IX3)           TO  W001-DET13-SPLITC                  
360000     MOVE ART-SPLIT(IX4)           TO  W001-DET13-SPLITD                  
360100     MOVE ART-SPLIT(IX5)           TO  W001-DET13-SPLITE                  
360200     MOVE ART-SPLIT(IX6)           TO  W001-DET13-SPLITF                  
360300     MOVE ART-SPLIT(IX7)           TO  W001-DET13-SPLITG                  
360400     MOVE ART-SPLIT(IX8)           TO  W001-DET13-SPLITH                  
360500     MOVE PSUM-SPLIT(PSUM-IX)      TO  W001-DET13-TOT                     
360600     MOVE ZERO                     TO  W001-DET13-P-TOT                   
360700     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
360800     MOVE +1 TO W001-SKIP                                                 
360900     PERFORM S21-SKRIV-LISTA                                              
361000                                                                          
361100     MOVE ART-OMSHAST-DISP(IX1)    TO  W001-DET14-OMSHASTA                
361200     MOVE ART-OMSHAST-PROC-D(IX1)  TO  W001-DET14-P-OMSHASTA              
361300     MOVE ART-OMSHAST-DISP(IX2)    TO  W001-DET14-OMSHASTB                
361400     MOVE ART-OMSHAST-PROC-D(IX2)  TO  W001-DET14-P-OMSHASTB              
361500     MOVE ART-OMSHAST-DISP(IX3)    TO  W001-DET14-OMSHASTC                
361600     MOVE ART-OMSHAST-PROC-D(IX3)  TO  W001-DET14-P-OMSHASTC              
361700     MOVE ART-OMSHAST-DISP(IX4)    TO  W001-DET14-OMSHASTD                
361800     MOVE ART-OMSHAST-PROC-D(IX4)  TO  W001-DET14-P-OMSHASTD              
361900     MOVE ART-OMSHAST-DISP(IX5)    TO  W001-DET14-OMSHASTE                
362000     MOVE ART-OMSHAST-PROC-D(IX5)  TO  W001-DET14-P-OMSHASTE              
362100     MOVE ART-OMSHAST-DISP(IX6)    TO  W001-DET14-OMSHASTF                
362200     MOVE ART-OMSHAST-PROC-D(IX6)  TO  W001-DET14-P-OMSHASTF              
362300     MOVE ART-OMSHAST-DISP(IX7)    TO  W001-DET14-OMSHASTG                
362400     MOVE ART-OMSHAST-PROC-D(IX7)  TO  W001-DET14-P-OMSHASTG              
362500     MOVE ART-OMSHAST-DISP(IX8)    TO  W001-DET14-OMSHASTH                
362600     MOVE ART-OMSHAST-PROC-D(IX8)  TO  W001-DET14-P-OMSHASTH              
362700     MOVE PSUM-OMSHAST-DISP(PSUM-IX) TO W001-DET14-TOT                    
362800     MOVE PSUM-OMSHAST-PROC-D(PSUM-IX)                                    
362900                                     TO W001-DET14-P-TOT                  
363000     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
363100     MOVE +1 TO W001-SKIP                                                 
363200     PERFORM S21-SKRIV-LISTA                                              
363300                                                                          
363400     MOVE ART-OMSHAST-LS(IX1)      TO  W001-DET15-OMSHASTA                
363500     MOVE ART-OMSHAST-PROC-LS(IX1) TO  W001-DET15-P-OMSHASTA              
363600     MOVE ART-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB                
363700     MOVE ART-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB              
363800     MOVE ART-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC                
363900     MOVE ART-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC              
364000     MOVE ART-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD                
364100     MOVE ART-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD              
364200     MOVE ART-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE                
364300     MOVE ART-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE              
364400     MOVE ART-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF                
364500     MOVE ART-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF              
364600     MOVE ART-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG                
364700     MOVE ART-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG              
364800     MOVE ART-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH                
364900     MOVE ART-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH              
365000     MOVE PSUM-OMSHAST-LS(PSUM-IX) TO  W001-DET15-TOT                     
365100     MOVE PSUM-OMSHAST-PROC-LS(PSUM-IX)                                   
365200                                     TO W001-DET15-P-TOT                  
365300     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
365400     MOVE +1 TO W001-SKIP                                                 
365500     PERFORM S21-SKRIV-LISTA                                              
365600                                                                          
365700     MOVE ART-SERVG-BTO(IX1)       TO  W001-DET16-SERVG-BTOA              
365800     MOVE ART-SERVG-BTO(IX2)       TO  W001-DET16-SERVG-BTOB              
365900     MOVE ART-SERVG-BTO(IX3)       TO  W001-DET16-SERVG-BTOC              
366000     MOVE ART-SERVG-BTO(IX4)       TO  W001-DET16-SERVG-BTOD              
366100     MOVE ART-SERVG-BTO(IX5)       TO  W001-DET16-SERVG-BTOE              
366200     MOVE ART-SERVG-BTO(IX6)       TO  W001-DET16-SERVG-BTOF              
366300     MOVE ART-SERVG-BTO(IX7)       TO  W001-DET16-SERVG-BTOG              
366400     MOVE ART-SERVG-BTO(IX8)       TO  W001-DET16-SERVG-BTOH              
366500     MOVE PSUM-SERVG-BTO (PSUM-IX) TO  W001-DET16-TOT                     
366600     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
366700     MOVE +1 TO W001-SKIP                                                 
366800     PERFORM S21-SKRIV-LISTA                                              
366900                                                                          
367000     MOVE ART-SERVG-NTO(IX1)       TO  W001-DET17-SERVG-NTOA              
367100     MOVE ART-SERVG-NTO(IX2)       TO  W001-DET17-SERVG-NTOB              
367200     MOVE ART-SERVG-NTO(IX3)       TO  W001-DET17-SERVG-NTOC              
367300     MOVE ART-SERVG-NTO(IX4)       TO  W001-DET17-SERVG-NTOD              
367400     MOVE ART-SERVG-NTO(IX5)       TO  W001-DET17-SERVG-NTOE              
367500     MOVE ART-SERVG-NTO(IX6)       TO  W001-DET17-SERVG-NTOF              
367600     MOVE ART-SERVG-NTO(IX7)       TO  W001-DET17-SERVG-NTOG              
367700     MOVE ART-SERVG-NTO(IX8)       TO  W001-DET17-SERVG-NTOH              
367800     MOVE PSUM-SERVG-NTO(PSUM-IX)  TO  W001-DET17-TOT                     
367900     MOVE ZERO                     TO  W001-DET17-P-TOT                   
368000     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
368100     MOVE +1 TO W001-SKIP                                                 
368200     PERFORM S21-SKRIV-LISTA                                              
368300     .                                                                    
368400     EJECT                                                                
368500 CB-FLYTTA-SKRIV-TOT SECTION.                                             
368600                                                                          
368700     MOVE FSUM-KVANT-AKT(IX1)      TO  W001-DET1-KVANTA                   
368800     MOVE FSUM-PROC-KVANT-A(IX1)   TO  W001-DET1-P-KVANTA                 
368900     MOVE FSUM-KVANT-AKT(IX2)      TO  W001-DET1-KVANTB                   
369000     MOVE FSUM-PROC-KVANT-A(IX2)   TO  W001-DET1-P-KVANTB                 
369100     MOVE FSUM-KVANT-AKT(IX3)      TO  W001-DET1-KVANTC                   
369200     MOVE FSUM-PROC-KVANT-A(IX3)   TO  W001-DET1-P-KVANTC                 
369300     MOVE FSUM-KVANT-AKT(IX4)      TO  W001-DET1-KVANTD                   
369400     MOVE FSUM-PROC-KVANT-A(IX4)   TO  W001-DET1-P-KVANTD                 
369500     MOVE FSUM-KVANT-AKT(IX5)      TO  W001-DET1-KVANTE                   
369600     MOVE FSUM-PROC-KVANT-A(IX5)   TO  W001-DET1-P-KVANTE                 
369700     MOVE FSUM-KVANT-AKT(IX6)      TO  W001-DET1-KVANTF                   
369800     MOVE FSUM-PROC-KVANT-A(IX6)   TO  W001-DET1-P-KVANTF                 
369900     MOVE FSUM-KVANT-AKT(IX7)      TO  W001-DET1-KVANTG                   
370000     MOVE FSUM-PROC-KVANT-A(IX7)   TO  W001-DET1-P-KVANTG                 
370100     MOVE FSUM-KVANT-AKT(IX8)      TO  W001-DET1-KVANTH                   
370200     MOVE FSUM-PROC-KVANT-A(IX8)   TO  W001-DET1-P-KVANTH                 
370300     MOVE TOT-KVANT-AKT            TO  W001-DET1-TOT                      
370400     MOVE ZERO                     TO  W001-DET1-P-TOT                    
370500     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
370600     MOVE +2 TO W001-SKIP                                                 
370700     PERFORM S21-SKRIV-LISTA                                              
370800                                                                          
370900     MOVE FSUM-KVANT-PAS(IX1)      TO  W001-DET2-KVANTA                   
371000     MOVE FSUM-PROC-KVANT-P(IX1)   TO  W001-DET2-P-KVANTA                 
371100     MOVE FSUM-KVANT-PAS(IX2)      TO  W001-DET2-KVANTB                   
371200     MOVE FSUM-PROC-KVANT-P(IX2)   TO  W001-DET2-P-KVANTB                 
371300     MOVE FSUM-KVANT-PAS(IX3)      TO  W001-DET2-KVANTC                   
371400     MOVE FSUM-PROC-KVANT-P(IX3)   TO  W001-DET2-P-KVANTC                 
371500     MOVE FSUM-KVANT-PAS(IX4)      TO  W001-DET2-KVANTD                   
371600     MOVE FSUM-PROC-KVANT-P(IX4)   TO  W001-DET2-P-KVANTD                 
371700     MOVE FSUM-KVANT-PAS(IX5)      TO  W001-DET2-KVANTE                   
371800     MOVE FSUM-PROC-KVANT-P(IX5)   TO  W001-DET2-P-KVANTE                 
371900     MOVE FSUM-KVANT-PAS(IX6)      TO  W001-DET2-KVANTF                   
372000     MOVE FSUM-PROC-KVANT-P(IX6)   TO  W001-DET2-P-KVANTF                 
372100     MOVE FSUM-KVANT-PAS(IX7)      TO  W001-DET2-KVANTG                   
372200     MOVE FSUM-PROC-KVANT-P(IX7)   TO  W001-DET2-P-KVANTG                 
372300     MOVE FSUM-KVANT-PAS(IX8)      TO  W001-DET2-KVANTH                   
372400     MOVE FSUM-PROC-KVANT-P(IX8)   TO  W001-DET2-P-KVANTH                 
372500     MOVE TOT-KVANT-PAS            TO  W001-DET2-TOT                      
372600     MOVE ZERO                     TO  W001-DET2-P-TOT                    
372700     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
372800     MOVE +1 TO W001-SKIP                                                 
372900     PERFORM S21-SKRIV-LISTA                                              
373000                                                                          
373100     MOVE FSUM-KVDISP-AKT(IX1)     TO  W001-DET3-DLAGERA                  
373200     MOVE FSUM-PROC-KVDISP-A(IX1)  TO  W001-DET3-P-DLAGERA                
373300     MOVE FSUM-KVDISP-AKT(IX2)     TO  W001-DET3-DLAGERB                  
373400     MOVE FSUM-PROC-KVDISP-A(IX2)  TO  W001-DET3-P-DLAGERB                
373500     MOVE FSUM-KVDISP-AKT(IX3)     TO  W001-DET3-DLAGERC                  
373600     MOVE FSUM-PROC-KVDISP-A(IX3)  TO  W001-DET3-P-DLAGERC                
373700     MOVE FSUM-KVDISP-AKT(IX4)     TO  W001-DET3-DLAGERD                  
373800     MOVE FSUM-PROC-KVDISP-A(IX4)  TO  W001-DET3-P-DLAGERD                
373900     MOVE FSUM-KVDISP-AKT(IX5)     TO  W001-DET3-DLAGERE                  
374000     MOVE FSUM-PROC-KVDISP-A(IX5)  TO  W001-DET3-P-DLAGERE                
374100     MOVE FSUM-KVDISP-AKT(IX6)     TO  W001-DET3-DLAGERF                  
374200     MOVE FSUM-PROC-KVDISP-A(IX6)  TO  W001-DET3-P-DLAGERF                
374300     MOVE FSUM-KVDISP-AKT(IX7)     TO  W001-DET3-DLAGERG                  
374400     MOVE FSUM-PROC-KVDISP-A(IX7)  TO  W001-DET3-P-DLAGERG                
374500     MOVE FSUM-KVDISP-AKT(IX8)     TO  W001-DET3-DLAGERH                  
374600     MOVE FSUM-PROC-KVDISP-A(IX8)  TO  W001-DET3-P-DLAGERH                
374700     MOVE TOT-KVDISP-AKT           TO  W001-DET3-TOT                      
374800     MOVE ZERO                     TO  W001-DET3-P-TOT                    
374900     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
375000     MOVE +1 TO W001-SKIP                                                 
375100     PERFORM S21-SKRIV-LISTA                                              
375200                                                                          
375300     MOVE FSUM-KVDISP-PAS(IX1)     TO  W001-DET4-DLAGERA                  
375400     MOVE FSUM-PROC-KVDISP-P(IX1)  TO  W001-DET4-P-DLAGERA                
375500     MOVE FSUM-KVDISP-PAS(IX2)     TO  W001-DET4-DLAGERB                  
375600     MOVE FSUM-PROC-KVDISP-P(IX2)  TO  W001-DET4-P-DLAGERB                
375700     MOVE FSUM-KVDISP-PAS(IX3)     TO  W001-DET4-DLAGERC                  
375800     MOVE FSUM-PROC-KVDISP-P(IX3)  TO  W001-DET4-P-DLAGERC                
375900     MOVE FSUM-KVDISP-PAS(IX4)     TO  W001-DET4-DLAGERD                  
376000     MOVE FSUM-PROC-KVDISP-P(IX4)  TO  W001-DET4-P-DLAGERD                
376100     MOVE FSUM-KVDISP-PAS(IX5)     TO  W001-DET4-DLAGERE                  
376200     MOVE FSUM-PROC-KVDISP-P(IX5)  TO  W001-DET4-P-DLAGERE                
376300     MOVE FSUM-KVDISP-PAS(IX6)     TO  W001-DET4-DLAGERF                  
376400     MOVE FSUM-PROC-KVDISP-P(IX6)  TO  W001-DET4-P-DLAGERF                
376500     MOVE FSUM-KVDISP-PAS(IX7)     TO  W001-DET4-DLAGERG                  
376600     MOVE FSUM-PROC-KVDISP-P(IX7)  TO  W001-DET4-P-DLAGERG                
376700     MOVE FSUM-KVDISP-PAS(IX8)     TO  W001-DET4-DLAGERH                  
376800     MOVE FSUM-PROC-KVDISP-P(IX8)  TO  W001-DET4-P-DLAGERH                
376900     MOVE TOT-KVDISP-PAS           TO  W001-DET4-TOT                      
377000     MOVE ZERO                     TO  W001-DET4-P-TOT                    
377100     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
377200     MOVE +1 TO W001-SKIP                                                 
377300     PERFORM S21-SKRIV-LISTA                                              
377400                                                                          
377500     MOVE FSUM-LS-AKT(IX1)         TO  W001-DET5-LLAGERA                  
377600     MOVE FSUM-PROC-LS-A(IX1)      TO  W001-DET5-P-LLAGERA                
377700     MOVE FSUM-LS-AKT(IX2)         TO  W001-DET5-LLAGERB                  
377800     MOVE FSUM-PROC-LS-A(IX2)      TO  W001-DET5-P-LLAGERB                
377900     MOVE FSUM-LS-AKT(IX3)         TO  W001-DET5-LLAGERC                  
378000     MOVE FSUM-PROC-LS-A(IX3)      TO  W001-DET5-P-LLAGERC                
378100     MOVE FSUM-LS-AKT(IX4)         TO  W001-DET5-LLAGERD                  
378200     MOVE FSUM-PROC-LS-A(IX4)      TO  W001-DET5-P-LLAGERD                
378300     MOVE FSUM-LS-AKT(IX5)         TO  W001-DET5-LLAGERE                  
378400     MOVE FSUM-PROC-LS-A(IX5)      TO  W001-DET5-P-LLAGERE                
378500     MOVE FSUM-LS-AKT(IX6)         TO  W001-DET5-LLAGERF                  
378600     MOVE FSUM-PROC-LS-A(IX6)      TO  W001-DET5-P-LLAGERF                
378700     MOVE FSUM-LS-AKT(IX7)         TO  W001-DET5-LLAGERG                  
378800     MOVE FSUM-PROC-LS-A(IX7)      TO  W001-DET5-P-LLAGERG                
378900     MOVE FSUM-LS-AKT(IX8)         TO  W001-DET5-LLAGERH                  
379000     MOVE FSUM-PROC-LS-A(IX8)      TO  W001-DET5-P-LLAGERH                
379100     MOVE TOT-LS-AKT               TO  W001-DET5-TOT                      
379200     MOVE ZERO                     TO  W001-DET5-P-TOT                    
379300     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
379400     MOVE +1 TO W001-SKIP                                                 
379500     PERFORM S21-SKRIV-LISTA                                              
379600                                                                          
379700     MOVE FSUM-LS-PAS(IX1)         TO  W001-DET6-LLAGERA                  
379800     MOVE FSUM-PROC-LS-P(IX1)      TO  W001-DET6-P-LLAGERA                
379900     MOVE FSUM-LS-PAS(IX2)         TO  W001-DET6-LLAGERB                  
380000     MOVE FSUM-PROC-LS-P(IX2)      TO  W001-DET6-P-LLAGERB                
380100     MOVE FSUM-LS-PAS(IX3)         TO  W001-DET6-LLAGERC                  
380200     MOVE FSUM-PROC-LS-P(IX3)      TO  W001-DET6-P-LLAGERC                
380300     MOVE FSUM-LS-PAS(IX4)         TO  W001-DET6-LLAGERD                  
380400     MOVE FSUM-PROC-LS-P(IX4)      TO  W001-DET6-P-LLAGERD                
380500     MOVE FSUM-LS-PAS(IX5)         TO  W001-DET6-LLAGERE                  
380600     MOVE FSUM-PROC-LS-P(IX5)      TO  W001-DET6-P-LLAGERE                
380700     MOVE FSUM-LS-PAS(IX6)         TO  W001-DET6-LLAGERF                  
380800     MOVE FSUM-PROC-LS-P(IX6)      TO  W001-DET6-P-LLAGERF                
380900     MOVE FSUM-LS-PAS(IX7)         TO  W001-DET6-LLAGERG                  
381000     MOVE FSUM-PROC-LS-P(IX7)      TO  W001-DET6-P-LLAGERG                
381100     MOVE FSUM-LS-PAS(IX8)         TO  W001-DET6-LLAGERH                  
381200     MOVE FSUM-PROC-LS-P(IX8)      TO  W001-DET6-P-LLAGERH                
381300     MOVE TOT-LS-PAS               TO  W001-DET6-TOT                      
381400     MOVE ZERO                     TO  W001-DET6-P-TOT                    
381500     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
381600     MOVE +1 TO W001-SKIP                                                 
381700     PERFORM S21-SKRIV-LISTA                                              
381800                                                                          
381900     MOVE FSUM-AK-AKT(IX1)         TO  W001-DET7-ALAGERA                  
382000     MOVE FSUM-PROC-AK-A(IX1)      TO  W001-DET7-P-ALAGERA                
382100     MOVE FSUM-AK-AKT(IX2)         TO  W001-DET7-ALAGERB                  
382200     MOVE FSUM-PROC-AK-A(IX2)      TO  W001-DET7-P-ALAGERB                
382300     MOVE FSUM-AK-AKT(IX3)         TO  W001-DET7-ALAGERC                  
382400     MOVE FSUM-PROC-AK-A(IX3)      TO  W001-DET7-P-ALAGERC                
382500     MOVE FSUM-AK-AKT(IX4)         TO  W001-DET7-ALAGERD                  
382600     MOVE FSUM-PROC-AK-A(IX4)      TO  W001-DET7-P-ALAGERD                
382700     MOVE FSUM-AK-AKT(IX5)         TO  W001-DET7-ALAGERE                  
382800     MOVE FSUM-PROC-AK-A(IX5)      TO  W001-DET7-P-ALAGERE                
382900     MOVE FSUM-AK-AKT(IX6)         TO  W001-DET7-ALAGERF                  
383000     MOVE FSUM-PROC-AK-A(IX6)      TO  W001-DET7-P-ALAGERF                
383100     MOVE FSUM-AK-AKT(IX7)         TO  W001-DET7-ALAGERG                  
383200     MOVE FSUM-PROC-AK-A(IX7)      TO  W001-DET7-P-ALAGERG                
383300     MOVE FSUM-AK-AKT(IX8)         TO  W001-DET7-ALAGERH                  
383400     MOVE FSUM-PROC-AK-A(IX8)      TO  W001-DET7-P-ALAGERH                
383500     MOVE TOT-AK-AKT               TO  W001-DET7-TOT                      
383600     MOVE ZERO                     TO  W001-DET7-P-TOT                    
383700     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
383800     MOVE +1 TO W001-SKIP                                                 
383900     PERFORM S21-SKRIV-LISTA                                              
384000                                                                          
384100     MOVE FSUM-AK-PAS(IX1)         TO  W001-DET8-ALAGERA                  
384200     MOVE FSUM-PROC-AK-P(IX1)      TO  W001-DET8-P-ALAGERA                
384300     MOVE FSUM-AK-PAS(IX2)         TO  W001-DET8-ALAGERB                  
384400     MOVE FSUM-PROC-AK-P(IX2)      TO  W001-DET8-P-ALAGERB                
384500     MOVE FSUM-AK-PAS(IX3)         TO  W001-DET8-ALAGERC                  
384600     MOVE FSUM-PROC-AK-P(IX3)      TO  W001-DET8-P-ALAGERC                
384700     MOVE FSUM-AK-PAS(IX4)         TO  W001-DET8-ALAGERD                  
384800     MOVE FSUM-PROC-AK-P(IX4)      TO  W001-DET8-P-ALAGERD                
384900     MOVE FSUM-AK-PAS(IX5)         TO  W001-DET8-ALAGERE                  
385000     MOVE FSUM-PROC-AK-P(IX5)      TO  W001-DET8-P-ALAGERE                
385100     MOVE FSUM-AK-PAS(IX6)         TO  W001-DET8-ALAGERF                  
385200     MOVE FSUM-PROC-AK-P(IX6)      TO  W001-DET8-P-ALAGERF                
385300     MOVE FSUM-AK-PAS(IX7)         TO  W001-DET8-ALAGERG                  
385400     MOVE FSUM-PROC-AK-P(IX7)      TO  W001-DET8-P-ALAGERG                
385500     MOVE FSUM-AK-PAS(IX8)         TO  W001-DET8-ALAGERH                  
385600     MOVE FSUM-PROC-AK-P(IX8)      TO  W001-DET8-P-ALAGERH                
385700     MOVE TOT-AK-PAS               TO  W001-DET8-TOT                      
385800     MOVE ZERO                     TO  W001-DET8-P-TOT                    
385900     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
386000     MOVE +1 TO W001-SKIP                                                 
386100     PERFORM S21-SKRIV-LISTA                                              
386200                                                                          
386300     MOVE FSUM-OLAGER(IX1)         TO  W001-DET9-OLAGERA                  
386400     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET9-P-OLAGERA                
386500     MOVE FSUM-OLAGER(IX2)         TO  W001-DET9-OLAGERB                  
386600     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET9-P-OLAGERB                
386700     MOVE FSUM-OLAGER(IX3)         TO  W001-DET9-OLAGERC                  
386800     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET9-P-OLAGERC                
386900     MOVE FSUM-OLAGER(IX4)         TO  W001-DET9-OLAGERD                  
387000     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET9-P-OLAGERD                
387100     MOVE FSUM-OLAGER(IX5)         TO  W001-DET9-OLAGERE                  
387200     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET9-P-OLAGERE                
387300     MOVE FSUM-OLAGER(IX6)         TO  W001-DET9-OLAGERF                  
387400     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET9-P-OLAGERF                
387500     MOVE FSUM-OLAGER(IX7)         TO  W001-DET9-OLAGERG                  
387600     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET9-P-OLAGERG                
387700     MOVE FSUM-OLAGER(IX8)         TO  W001-DET9-OLAGERH                  
387800     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET9-P-OLAGERH                
387900     MOVE TOT-OLAGER               TO  W001-DET9-TOT                      
388000     MOVE TOT-PROC-OLAGER          TO  W001-DET9-P-TOT                    
388100     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
388200     MOVE +1 TO W001-SKIP                                                 
388300     PERFORM S21-SKRIV-LISTA                                              
388400                                                                          
388500     MOVE FSUM-SLAGER(IX1)         TO  W001-DET10-SLAGERA                 
388600     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET10-P-SLAGERA               
388700     MOVE FSUM-SLAGER(IX2)         TO  W001-DET10-SLAGERB                 
388800     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET10-P-SLAGERB               
388900     MOVE FSUM-SLAGER(IX3)         TO  W001-DET10-SLAGERC                 
389000     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET10-P-SLAGERC               
389100     MOVE FSUM-SLAGER(IX4)         TO  W001-DET10-SLAGERD                 
389200     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET10-P-SLAGERD               
389300     MOVE FSUM-SLAGER(IX5)         TO  W001-DET10-SLAGERE                 
389400     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET10-P-SLAGERE               
389500     MOVE FSUM-SLAGER(IX6)         TO  W001-DET10-SLAGERF                 
389600     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET10-P-SLAGERF               
389700     MOVE FSUM-SLAGER(IX7)         TO  W001-DET10-SLAGERG                 
389800     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET10-P-SLAGERG               
389900     MOVE FSUM-SLAGER(IX8)         TO  W001-DET10-SLAGERH                 
390000     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET10-P-SLAGERH               
390100     MOVE TOT-SLAGER               TO  W001-DET10-TOT                     
390200     MOVE TOT-PROC-SLAGER          TO  W001-DET10-P-TOT                   
390300     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
390400     MOVE +1 TO W001-SKIP                                                 
390500     PERFORM S21-SKRIV-LISTA                                              
390600                                                                          
390700     MOVE FSUM-MLAGER(IX1)         TO  W001-DET11-MLAGERA                 
390800     MOVE FSUM-PROC-MLAGER(IX1)    TO  W001-DET11-P-MLAGERA               
390900     MOVE FSUM-MLAGER(IX2)         TO  W001-DET11-MLAGERB                 
391000     MOVE FSUM-PROC-MLAGER(IX2)    TO  W001-DET11-P-MLAGERB               
391100     MOVE FSUM-MLAGER(IX3)         TO  W001-DET11-MLAGERC                 
391200     MOVE FSUM-PROC-MLAGER(IX3)    TO  W001-DET11-P-MLAGERC               
391300     MOVE FSUM-MLAGER(IX4)         TO  W001-DET11-MLAGERD                 
391400     MOVE FSUM-PROC-MLAGER(IX4)    TO  W001-DET11-P-MLAGERD               
391500     MOVE FSUM-MLAGER(IX5)         TO  W001-DET11-MLAGERE                 
391600     MOVE FSUM-PROC-MLAGER(IX5)    TO  W001-DET11-P-MLAGERE               
391700     MOVE FSUM-MLAGER(IX6)         TO  W001-DET11-MLAGERF                 
391800     MOVE FSUM-PROC-MLAGER(IX6)    TO  W001-DET11-P-MLAGERF               
391900     MOVE FSUM-MLAGER(IX7)         TO  W001-DET11-MLAGERG                 
392000     MOVE FSUM-PROC-MLAGER(IX7)    TO  W001-DET11-P-MLAGERG               
392100     MOVE FSUM-MLAGER(IX8)         TO  W001-DET11-MLAGERH                 
392200     MOVE FSUM-PROC-MLAGER(IX8)    TO  W001-DET11-P-MLAGERH               
392300     MOVE TOT-MLAGER               TO  W001-DET11-TOT                     
392400     MOVE TOT-PROC-MLAGER          TO  W001-DET11-P-TOT                   
392500     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
392600     MOVE +1 TO W001-SKIP                                                 
392700     PERFORM S21-SKRIV-LISTA                                              
392800                                                                          
392900     MOVE FSUM-KVOT(IX1)           TO  W001-DET12-KVOTA                   
393000     MOVE FSUM-PROC-KVOT(IX1)      TO  W001-DET12-P-KVOTA                 
393100     MOVE FSUM-KVOT(IX2)           TO  W001-DET12-KVOTB                   
393200     MOVE FSUM-PROC-KVOT(IX2)      TO  W001-DET12-P-KVOTB                 
393300     MOVE FSUM-KVOT(IX3)           TO  W001-DET12-KVOTC                   
393400     MOVE FSUM-PROC-KVOT(IX3)      TO  W001-DET12-P-KVOTC                 
393500     MOVE FSUM-KVOT(IX4)           TO  W001-DET12-KVOTD                   
393600     MOVE FSUM-PROC-KVOT(IX4)      TO  W001-DET12-P-KVOTD                 
393700     MOVE FSUM-KVOT(IX5)           TO  W001-DET12-KVOTE                   
393800     MOVE FSUM-PROC-KVOT(IX5)      TO  W001-DET12-P-KVOTE                 
393900     MOVE FSUM-KVOT(IX6)           TO  W001-DET12-KVOTF                   
394000     MOVE FSUM-PROC-KVOT(IX6)      TO  W001-DET12-P-KVOTF                 
394100     MOVE FSUM-KVOT(IX7)           TO  W001-DET12-KVOTG                   
394200     MOVE FSUM-PROC-KVOT(IX7)      TO  W001-DET12-P-KVOTG                 
394300     MOVE FSUM-KVOT(IX8)           TO  W001-DET12-KVOTH                   
394400     MOVE FSUM-PROC-KVOT(IX8)      TO  W001-DET12-P-KVOTH                 
394500     MOVE TOT-KVOT                 TO  W001-DET12-TOT                     
394600     MOVE TOT-PROC-MLAGER          TO  W001-DET12-P-TOT                   
394700     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
394800     MOVE +1 TO W001-SKIP                                                 
394900     PERFORM S21-SKRIV-LISTA                                              
395000                                                                          
395100     MOVE FSUM-SPLIT(IX1)          TO  W001-DET13-SPLITA                  
395200     MOVE FSUM-SPLIT(IX2)          TO  W001-DET13-SPLITB                  
395300     MOVE FSUM-SPLIT(IX3)          TO  W001-DET13-SPLITC                  
395400     MOVE FSUM-SPLIT(IX4)          TO  W001-DET13-SPLITD                  
395500     MOVE FSUM-SPLIT(IX5)          TO  W001-DET13-SPLITE                  
395600     MOVE FSUM-SPLIT(IX6)          TO  W001-DET13-SPLITF                  
395700     MOVE FSUM-SPLIT(IX7)          TO  W001-DET13-SPLITG                  
395800     MOVE FSUM-SPLIT(IX8)          TO  W001-DET13-SPLITH                  
395900     MOVE TOT-SPLIT                TO  W001-DET13-TOT                     
396000     MOVE ZERO                     TO  W001-DET13-P-TOT                   
396100     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
396200     MOVE +1 TO W001-SKIP                                                 
396300     PERFORM S21-SKRIV-LISTA                                              
396400                                                                          
396500     MOVE FSUM-OMSHAST-DISP(IX1)   TO  W001-DET14-OMSHASTA                
396600     MOVE FSUM-OMSHAST-PROC-D(IX1) TO  W001-DET14-P-OMSHASTA              
396700     MOVE FSUM-OMSHAST-DISP(IX2)   TO  W001-DET14-OMSHASTB                
396800     MOVE FSUM-OMSHAST-PROC-D(IX2) TO  W001-DET14-P-OMSHASTB              
396900     MOVE FSUM-OMSHAST-DISP(IX3)   TO  W001-DET14-OMSHASTC                
397000     MOVE FSUM-OMSHAST-PROC-D(IX3) TO  W001-DET14-P-OMSHASTC              
397100     MOVE FSUM-OMSHAST-DISP(IX4)   TO  W001-DET14-OMSHASTD                
397200     MOVE FSUM-OMSHAST-PROC-D(IX4) TO  W001-DET14-P-OMSHASTD              
397300     MOVE FSUM-OMSHAST-DISP(IX5)   TO  W001-DET14-OMSHASTE                
397400     MOVE FSUM-OMSHAST-PROC-D(IX5) TO  W001-DET14-P-OMSHASTE              
397500     MOVE FSUM-OMSHAST-DISP(IX6)   TO  W001-DET14-OMSHASTF                
397600     MOVE FSUM-OMSHAST-PROC-D(IX6) TO  W001-DET14-P-OMSHASTF              
397700     MOVE FSUM-OMSHAST-DISP(IX7)   TO  W001-DET14-OMSHASTG                
397800     MOVE FSUM-OMSHAST-PROC-D(IX7) TO  W001-DET14-P-OMSHASTG              
397900     MOVE FSUM-OMSHAST-DISP(IX8)   TO  W001-DET14-OMSHASTH                
398000     MOVE FSUM-OMSHAST-PROC-D(IX8) TO  W001-DET14-P-OMSHASTH              
398100     MOVE TOT-OMSHAST-DISP         TO  W001-DET14-TOT                     
398200     MOVE ZERO                     TO  W001-DET14-P-TOT                   
398300     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
398400     MOVE +1 TO W001-SKIP                                                 
398500     PERFORM S21-SKRIV-LISTA                                              
398600                                                                          
398700     MOVE FSUM-OMSHAST-LS(IX1)      TO W001-DET15-OMSHASTA                
398800     MOVE FSUM-OMSHAST-PROC-LS(IX1) TO W001-DET15-P-OMSHASTA              
398900     MOVE FSUM-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB               
399000     MOVE FSUM-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB             
399100     MOVE FSUM-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC               
399200     MOVE FSUM-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC             
399300     MOVE FSUM-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD               
399400     MOVE FSUM-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD             
399500     MOVE FSUM-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE               
399600     MOVE FSUM-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE             
399700     MOVE FSUM-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF               
399800     MOVE FSUM-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF             
399900     MOVE FSUM-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG               
400000     MOVE FSUM-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG             
400100     MOVE FSUM-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH               
400200     MOVE FSUM-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH             
400300     MOVE TOT-OMSHAST-LS            TO  W001-DET15-TOT                    
400400     MOVE ZERO                      TO  W001-DET15-P-TOT                  
400500     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
400600     MOVE +1 TO W001-SKIP                                                 
400700     PERFORM S21-SKRIV-LISTA                                              
400800                                                                          
400900     MOVE FSUM-SERVG-BTO(IX1)     TO  W001-DET16-SERVG-BTOA               
401000     MOVE FSUM-SERVG-BTO(IX2)     TO  W001-DET16-SERVG-BTOB               
401100     MOVE FSUM-SERVG-BTO(IX3)     TO  W001-DET16-SERVG-BTOC               
401200     MOVE FSUM-SERVG-BTO(IX4)     TO  W001-DET16-SERVG-BTOD               
401300     MOVE FSUM-SERVG-BTO(IX5)     TO  W001-DET16-SERVG-BTOE               
401400     MOVE FSUM-SERVG-BTO(IX6)     TO  W001-DET16-SERVG-BTOF               
401500     MOVE FSUM-SERVG-BTO(IX7)     TO  W001-DET16-SERVG-BTOG               
401600     MOVE FSUM-SERVG-BTO(IX8)     TO  W001-DET16-SERVG-BTOH               
401700     MOVE TOT-SERVG-BTO           TO  W001-DET16-TOT                      
401800     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
401900     MOVE +1 TO W001-SKIP                                                 
402000     PERFORM S21-SKRIV-LISTA                                              
402100                                                                          
402200     MOVE FSUM-SERVG-NTO(IX1)     TO  W001-DET17-SERVG-NTOA               
402300     MOVE FSUM-SERVG-NTO(IX2)     TO  W001-DET17-SERVG-NTOB               
402400     MOVE FSUM-SERVG-NTO(IX3)     TO  W001-DET17-SERVG-NTOC               
402500     MOVE FSUM-SERVG-NTO(IX4)     TO  W001-DET17-SERVG-NTOD               
402600     MOVE FSUM-SERVG-NTO(IX5)     TO  W001-DET17-SERVG-NTOE               
402700     MOVE FSUM-SERVG-NTO(IX6)     TO  W001-DET17-SERVG-NTOF               
402800     MOVE FSUM-SERVG-NTO(IX7)     TO  W001-DET17-SERVG-NTOG               
402900     MOVE FSUM-SERVG-NTO(IX8)     TO  W001-DET17-SERVG-NTOH               
403000     MOVE TOT-SERVG-NTO           TO  W001-DET17-TOT                      
403100     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
403200     MOVE +1 TO W001-SKIP                                                 
403300     PERFORM S21-SKRIV-LISTA                                              
403400     .                                                                    
403500     EJECT                                                                
403600 Z-FINIT SECTION.                                                         
403700                                                                          
403800     CLOSE W23195                                                         
403900           W2319A                                                         
404000                                                                          
404100     MOVE 'S' TO POSTSUM-OPKOD                                            
404200     CALL POSTSUM USING POSTSUM-PARM                                      
404300     .                                                                    
404400     EJECT                                                                
404500 S01-LAS-W23195 SECTION.                                                  
404600                                                                          
404700     READ W23195 INTO IN-AREA                                             
404800     AT END                                                               
404900         SET END-OF-W23195 TO TRUE                                        
405000                                                                          
405100     NOT AT END                                                           
405200        MOVE 'W23196'      TO POSTSUM-FDNAMN                              
405300        MOVE 'W23196D1'    TO POSTSUM-DDNAMN2                             
405400        MOVE SPACE         TO POSTSUM-TRANSTYP                            
405500        CALL POSTSUM USING POSTSUM-PARM                                   
405600     .                                                                    
405700     EJECT                                                                
405800 S21-SKRIV-LISTA SECTION.                                                 
405900                                                                          
406000     WRITE W2319A-RAD FROM W001-RAD AFTER W001-SKIP                       
406100                                                                          
406200     MOVE SPACE TO W001-RAD                                               
406300     ADD  +1 TO W001-ANTAL-RADER                                          
406400     .                                                                    
406500     EJECT                                                                
406600 S21A-SKRIV-RUBRIKER SECTION.                                             
406700                                                                          
406800     ADD +1 TO W001-SIDRAKNARE                                            
406900     MOVE W001-SIDRAKNARE TO W001-SID                                     
407000     WRITE W2319A-RAD FROM W001-RUBRIK1 AFTER PAGE                        
407100     WRITE W2319A-RAD FROM W001-RUBRIK3 AFTER 2                           
407200     MOVE +2 TO W001-ANTAL-RADER                                          
407300     MOVE +2 TO W001-SKIP                                                 
407400     .                                                                    
407500 S30-SKRIV-DAP1 SECTION.                                                  
407600                                                                          
407700     MOVE ' ¤DAPW23196' TO W001-DAP                                       
407800     WRITE W2319A-RAD FROM W001-DAP                                       
407900                                                                          
408000     MOVE SPACE TO W001-DAP                                               
408100     .                                                                    
408200 S31-SKRIV-DAP2 SECTION.                                                  
408300                                                                          
408400     STRING ' ¤DAP' W001-AKTUELLT-IDDC                                    
408500            DELIMITED BY SIZE INTO W001-DAP                               
408600     WRITE W2319A-RAD FROM W001-DAP                                       
408700                                                                          
408800     MOVE SPACE TO W001-DAP                                               
408900     .                                                                    
409000     EJECT                                                                
409100* --- IMS SEKTIONER ---                                                   
409200     SKIP3                                                                
409300                                                                          
409400 IMS-GET-WDB6      SECTION.                                               
409500                                                                          
409600     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B6                        
409700     MOVE '  GAGKGB'          TO GODK-STATUSKODER                         
409800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
409900     PERFORM IMS-STATUSKONTROLL                                           
410000     .                                                                    
410100     EJECT                                                                
410200 IMS-STATUSKONTROLL SECTION.                                              
410300                                                                          
410400     SET STATUS-IX TO 1                                                   
410500     SEARCH GODK-STATUS                                                   
410600       AT END                                                             
410700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
410800           DELIMITED BY SIZE INTO FELTEXT                                 
410900         DISPLAY FELTEXT                                                  
411000         CALL FELLOG                                                      
411100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
411200         CONTINUE                                                         
411300     END-SEARCH                                                           
411400     .                                                                    
