000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2317200.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   95/08/16.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      - LÄSER FIL W23170                                                 
001010*      - LÄSER     WDB6                                                   
001100*      - SKAPAR LISTA REFILL UPPFÖLJNING                                  
001200*      PROGRAMMET ÄR EN KOPIA AV W23171                                   
001300*                                                                         
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800     EJECT                                                                
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- GRUNDFIL ANALYS-LISTOR                                     
002200     SELECT W23170                     ASSIGN TO W23172D1.                
002300*          --- AKTUELLT SDC-LAGER                                         
002310*    INFIL TAS BORT,FÅS VIA SYMBOLISK PARAMETER ISTÄLLET                  
002400*    SELECT W271SDC                    ASSIGN TO W23172D2.                
002500*          --- LISTA                                                      
002600     SELECT W23172-001                 ASSIGN TO W23172D3.                
002700*          --- URVAL                                                      
002800     SELECT W231PP                     ASSIGN TO W23172D4.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W23170                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700*01  -COPY W231701A      -L.                                              
003800     SKIP3                                                                
004400 FD  W231PP                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700 01  PARM                PIC X(80).                                       
004800     SKIP3                                                                
004900 FD  W23172-001                                                           
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200**1  W23172-001-RAD              PIC X(135).                              
005300 01  W23172-001-RAD              PIC X(165).                              
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W2317200'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
006300 77  SW-KVOI-TRAFF               PIC X       VALUE 'N'.                   
006400 77  SW-ARTIKEL-SAKNAS-WDK7      PIC X       VALUE 'N'.                   
006500 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
006700 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
006800 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
006900 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
007000 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
007100 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
007200 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
007300 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
007400 77  SDC-IX                      PIC 9(2)    VALUE ZERO COMP-3.           
007500 77  KVOI-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007600 77  ART-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
007700 77  ART-IX-MAX                  PIC S9(3)   VALUE +72  COMP-3.           
007800 77  PSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007900 77  PSUM-IX-MAX                 PIC S9(3)   VALUE +9   COMP-3.           
008000 77  FSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
008100 77  FSUM-IX-MAX                 PIC S9(3)   VALUE +8   COMP-3.           
008200 77  WS-SDC-NUM                  PIC 9(2)    VALUE ZERO.                  
008300 77  WS-KVAKS                    PIC S9(11)  VALUE ZERO COMP-3.           
008400 77  WS-KVLS-TOT                 PIC S9(11)  VALUE ZERO COMP-3.           
008500 77  WS-KVOKS-TOT                PIC S9(11)  VALUE ZERO COMP-3.           
008600 77  WS-KVOI                     PIC S9(11)V9(2)                          
008700                                   VALUE ZERO COMP-3.                     
008800 77  WS-KVOI-TOT-AAR             PIC S9(11)     VALUE ZERO COMP-3.        
008900 77  WS-KVOI-TOT-VECKA           PIC S9(11)     VALUE ZERO COMP-3.        
009000 77  WS-KVOI-TOT-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
009100 77  WS-KVOI-TEO-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
009200 77  WS-KVOT-SAKNAS-WDK7         PIC 9(9)       VALUE ZERO.               
009300 77  WS-KVOT-CDC-SAKNAS-WDK7     PIC 9(9)       VALUE ZERO.               
009400 77  WS-KVDISP                   PIC S9(11)     VALUE ZERO COMP-3.        
009500 77  WS-KVDISP-PR                PIC S9(11)V9(2)                          
009600                                   VALUE ZERO COMP-3.                     
009700 77  WS-SUMMA                    PIC S9(11)V9(2)                          
009800                                   VALUE ZERO COMP-3.                     
009900 77  WS-SUMMA-TOT                PIC S9(11)V9(2)                          
010000                                   VALUE ZERO COMP-3.                     
010100 77  WS-SUMMA-KR                 PIC S9(11)      VALUE ZERO.              
010200 77  WS-SERVG                    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
010300 77  WS-OLAGER                   PIC S9(11)      VALUE ZERO.              
010400 77  WS-MLAGER                   PIC S9(11)      VALUE ZERO.              
010500 77  WS-OMSHAST                  PIC 9(11)V9(1)  VALUE ZERO.              
010600 77  WS-PROC                     PIC S9(3)V9(1)  VALUE ZERO.              
010700 77  WS-KVPB-VECKA-SDC           PIC S9(6)V9(2)  VALUE ZERO.              
010800 77  WS-KVPB-DAG-SDC-NORM        PIC S9(6)V9(2)  VALUE ZERO.              
010900 77  WS-LT-BEHOV-SDC-NORM        PIC S9(7)V9(2)  VALUE ZERO.              
011000                                                                          
011100 77  W23170-EOF-SW               PIC X       VALUE 'N'.                   
011200     88  END-OF-W23170                       VALUE 'J'.                   
011300 77  W231PP-EOF-SW               PIC X       VALUE 'N'.                   
011400     88  END-OF-W231PP                       VALUE 'J'.                   
011500                                                                          
011600*      --- VALID IDDC CODES                                               
011700*                                                                         
011800*01    -COPY WWDC99                                                       
011801                                                                          
011900       EJECT                                                              
012000 01  DAGENS-DATUM                PIC 9(6).                                
012100 01  FILLER REDEFINES DAGENS-DATUM.                                       
012200     03  DAGENS-AAR              PIC 9(2).                                
012300     03  DAGENS-MAANAD           PIC 9(2).                                
012400     03  DAGENS-DAG              PIC 9(2).                                
012500                                                                          
012600 01  DAGENS-VECKA                PIC 9(4).                                
012700 01  FILLER REDEFINES DAGENS-VECKA.                                       
012800     03  D-VECKA-AAR             PIC 9(2).                                
012900     03  D-VECKA-VECKA           PIC 9(2).                                
013000                                                                          
013100 01  TEST-VECKA                  PIC 9(4).                                
013200 01  FILLER REDEFINES TEST-VECKA.                                         
013300     03  TEST-AA                 PIC 9(2).                                
013400     03  TEST-VV                 PIC 9(2).                                
013500                                                                          
013600 01  VECKOR.                                                              
013700     03  AAVVD                   PIC 9(5).                                
013800     03  FILLER REDEFINES AAVVD.                                          
013900         05  AAVV                PIC 9(4).                                
014000         05  D                   PIC 9(1).                                
014100     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
014200                                                                          
014300 01  PARAM-TILL-W009VADD.                                                 
014400     03  W009VADD-DATUM-2        PIC S9(5) COMP-3.                        
014500     03  W009VADD-ANTAL-2        PIC S9(3) COMP-3.                        
014600     EJECT                                                                
014700 01  ART-TABELL.                                                          
014800     03 ART-RAD OCCURS 72.                                                
014900        05  ART-KVANT-AKT        PIC S9(9)      VALUE ZERO COMP-3.        
015000        05  ART-KVANT-PAS        PIC S9(9)      VALUE ZERO COMP-3.        
015100        05  ART-PROC-KVANT-A     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015200        05  ART-PROC-KVANT-P     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015300        05  ART-KVDISP-AKT       PIC S9(11)     VALUE ZERO COMP-3.        
015400        05  ART-PROC-KVDISP-A    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015500        05  ART-KVDISP-PAS       PIC S9(11)     VALUE ZERO COMP-3.        
015600        05  ART-PROC-KVDISP-P    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015700        05  ART-LS-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
015800        05  ART-PROC-LS-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015900        05  ART-LS-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
016000        05  ART-PROC-LS-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016100        05  ART-AK-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
016200        05  ART-PROC-AK-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016300        05  ART-AK-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
016400        05  ART-PROC-AK-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016500        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
016600        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016700        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
016800        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016900        05  ART-MLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
017000        05  ART-PROC-MLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017100        05  ART-KVOT             PIC S9(9)      VALUE ZERO COMP-3.        
017200        05  ART-PROC-KVOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017300        05  ART-SPLIT            PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017400        05  ART-OMSHAST-DISP     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017500        05  ART-OMSHAST-PROC-D   PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017600        05  ART-OMSHAST-LS       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017700        05  ART-OMSHAST-PROC-LS  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017800        05  ART-SERVG-TOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017900        05  ART-SERVG-AKT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018000        05  ART-SERVG-PAS        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018100        05  ART-SERVG-TEO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
018200*******  ARBETSFÄLT                                                       
018300        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
018400        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
018500        05  WS-ART-MLAGER        PIC S9(11)V9(2) VALUE ZERO.              
018600        05  WS-ART-LS-AKT        PIC S9(11)      VALUE ZERO.              
018700        05  WS-ART-LS-PAS        PIC S9(11)      VALUE ZERO.              
018800        05  WS-ART-LS-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
018900        05  WS-ART-LS-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
019000        05  WS-ART-KVLS-AKT      PIC S9(11)      VALUE ZERO.              
019100        05  WS-ART-KVLS-PAS      PIC S9(11)      VALUE ZERO.              
019200        05  WS-ART-KVDISP-AKT    PIC S9(11)      VALUE ZERO.              
019300        05  WS-ART-KVDISP-PAS    PIC S9(11)      VALUE ZERO.              
019400        05  WS-ART-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.              
019500        05  WS-ART-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.              
019600        05  WS-ART-KVOKS-AKT     PIC S9(11)      VALUE ZERO.              
019700        05  WS-ART-KVOKS-PAS     PIC S9(11)      VALUE ZERO.              
019800        05  WS-ART-OK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
019900        05  WS-ART-OK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
020000        05  WS-ART-KVAKS-AKT     PIC S9(11)      VALUE ZERO.              
020100        05  WS-ART-KVAKS-PAS     PIC S9(11)      VALUE ZERO.              
020200        05  WS-ART-AK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
020300        05  WS-ART-AK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
020400        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
020500        05  WS-ART-KVOI-AKT      PIC S9(9)      VALUE ZERO COMP-3.        
020600        05  WS-ART-KVOI-PAS      PIC S9(9)      VALUE ZERO COMP-3.        
020700        05  WS-ART-KVOI-TEO      PIC S9(9)      VALUE ZERO COMP-3.        
020800        05  WS-ART-KVOI-SAK      PIC S9(9)      VALUE ZERO COMP-3.        
020900        05  WS-ART-KVOI-CDC-AKT  PIC S9(9)      VALUE ZERO COMP-3.        
021000        05  WS-ART-KVOI-CDC-PAS  PIC S9(9)      VALUE ZERO COMP-3.        
021100        05  WS-ART-KVOI-CDC-TEO  PIC S9(9)      VALUE ZERO COMP-3.        
021200        05  WS-ART-KVOI-CDC-SAK  PIC S9(9)      VALUE ZERO COMP-3.        
021300     EJECT                                                                
021400 01  PSUM-TABELL.                                                         
021500     03 PSUM-RAD OCCURS 9.                                                
021600        05  PSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
021700        05  PSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
021800        05  PSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021900        05  PSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022000        05  PSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
022100        05  PSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022200        05  PSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
022300        05  PSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022400        05  PSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
022500        05  PSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022600        05  PSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
022700        05  PSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022800        05  PSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
022900        05  PSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023000        05  PSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
023100        05  PSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023200        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
023300        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023400        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
023500        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023600        05  PSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
023700        05  PSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023800        05  PSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
023900        05  PSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024000        05  PSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024100        05  PSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024200        05  PSUM-OMSHAST-PROC-D  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
024300        05  PSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024400        05  PSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
024500        05  PSUM-SERVG-TOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024600        05  PSUM-SERVG-AKT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024700        05  PSUM-SERVG-PAS      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024800        05  PSUM-SERVG-TEO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024900*******  ARBETSFÄLT                                                       
025000        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
025100        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
025200        05  WS-PSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
025300        05  WS-PSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
025400        05  WS-PSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
025500        05  WS-PSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
025600        05  WS-PSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
025700        05  WS-PSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
025800        05  WS-PSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
025900        05  WS-PSUM-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.             
026000        05  WS-PSUM-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.             
026100        05  WS-PSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
026200        05  WS-PSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
026300        05  WS-PSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
026400        05  WS-PSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
026500        05  WS-PSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
026600        05  WS-PSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
026700        05  WS-PSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
026800        05  WS-PSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
026900        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
027000        05  WS-PSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
027100        05  WS-PSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
027200        05  WS-PSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
027300        05  WS-PSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
027400        05  WS-PSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
027500        05  WS-PSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
027600        05  WS-PSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
027700        05  WS-PSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
027800     EJECT                                                                
027900 01  FSUM-TABELL.                                                         
028000     03 FSUM-RAD OCCURS 8.                                                
028100        05  FSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
028200        05  FSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
028300        05  FSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028400        05  FSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028500        05  FSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
028600        05  FSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028700        05  FSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
028800        05  FSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028900        05  FSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
029000        05  FSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029100        05  FSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
029200        05  FSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029300        05  FSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
029400        05  FSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029500        05  FSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
029600        05  FSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029700        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029800        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029900        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
030000        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030100        05  FSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
030200        05  FSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030300        05  FSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
030400        05  FSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030500        05  FSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030600        05  FSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030700        05  FSUM-OMSHAST-PROC-D PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030800        05  FSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030900        05  FSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
031000        05  FSUM-SERVG-TOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031100        05  FSUM-SERVG-AKT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031200        05  FSUM-SERVG-PAS      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031300        05  FSUM-SERVG-TEO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
031400*******  ARBETSFÄLT                                                       
031500        05  WS-FSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
031600        05  WS-FSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
031700        05  WS-FSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
031800        05  WS-FSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
031900        05  WS-FSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
032000        05  WS-FSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032100        05  WS-FSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
032200        05  WS-FSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
032300        05  WS-FSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
032400        05  WS-FSUM-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.            
032500        05  WS-FSUM-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.            
032600        05  WS-FSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
032700        05  WS-FSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
032800        05  WS-FSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032900        05  WS-FSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
033000        05  WS-FSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
033100        05  WS-FSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
033200        05  WS-FSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
033300        05  WS-FSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
033400        05  WS-FSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
033500        05  WS-FSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
033600        05  WS-FSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
033700        05  WS-FSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
033800        05  WS-FSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
033900        05  WS-FSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
034000        05  WS-FSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
034100        05  WS-FSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
034200        05  WS-FSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
034300     EJECT                                                                
034400 01  TOTAL-RUTA.                                                          
034500     03  TOT-KVANT-AKT          PIC S9(9)      VALUE ZERO COMP-3.         
034600     03  TOT-KVANT-PAS          PIC S9(9)      VALUE ZERO COMP-3.         
034700     03  TOT-KVDISP-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
034800     03  TOT-KVDISP-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
034900     03  TOT-LS-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
035000     03  TOT-LS-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
035100     03  TOT-AK-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
035200     03  TOT-AK-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
035300     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035400     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035500     03  TOT-MLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035600     03  TOT-PROC-MLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035700     03  TOT-KVOT               PIC S9(9)      VALUE ZERO COMP-3.         
035800     03  TOT-PROC-KVOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035900     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
036000     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036100     03  TOT-SPLIT              PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036200     03  TOT-OMSHAST-DISP       PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036300     03  TOT-OMSHAST-LS         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036400     03  TOT-SERVG-TOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036500     03  TOT-SERVG-AKT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036600     03  TOT-SERVG-PAS          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036700     03  TOT-SERVG-TEO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036800*******  ARBETSFÄLT                                                       
036900     03  WS-TOT-SLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
037000     03  WS-TOT-OLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
037100     03  WS-TOT-MLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
037200     03  WS-TOT-LS-AKT       PIC S9(11)      VALUE ZERO.                  
037300     03  WS-TOT-LS-PAS       PIC S9(11)      VALUE ZERO.                  
037400     03  WS-TOT-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.                  
037500     03  WS-TOT-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.                  
037600     03  WS-TOT-KVDISP-AKT   PIC S9(11)      VALUE ZERO.                  
037700     03  WS-TOT-KVDISP-PAS   PIC S9(11)      VALUE ZERO.                  
037800     03  WS-TOT-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.                
037900     03  WS-TOT-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.                
038000     03  WS-TOT-KVOKS-AKT    PIC S9(11)      VALUE ZERO.                  
038100     03  WS-TOT-KVOKS-PAS    PIC S9(11)      VALUE ZERO.                  
038200     03  WS-TOT-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.                  
038300     03  WS-TOT-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.                  
038400     03  WS-TOT-KVAKS-AKT    PIC S9(11)      VALUE ZERO.                  
038500     03  WS-TOT-KVAKS-PAS    PIC S9(11)      VALUE ZERO.                  
038600     03  WS-TOT-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.                  
038700     03  WS-TOT-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.                  
038800     03  WS-TOT-KVOI         PIC S9(11)V9(2) VALUE ZERO.                  
038900     03  WS-TOT-KVOI-AKT     PIC S9(9)       VALUE ZERO.                  
039000     03  WS-TOT-KVOI-PAS     PIC S9(9)       VALUE ZERO.                  
039100     03  WS-TOT-KVOI-TEO     PIC S9(9)       VALUE ZERO.                  
039200     03  WS-TOT-KVOI-SAK     PIC S9(9)       VALUE ZERO.                  
039300     03  WS-TOT-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.                  
039400     03  WS-TOT-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.                  
039500     03  WS-TOT-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.                  
039600     03  WS-TOT-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.                  
039700     EJECT                                                                
039800 01  DYNAMISKA-SUBPROGRAM.                                                
039900*                                                                         
040000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
040100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
040200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
040300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
040400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
040410     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
040420     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
040500                                                                          
040600     EJECT                                                                
040700 01  PARAM-TILL-DATKORT.                                                  
040800     03  PROG-ID                 PIC X(8)    VALUE 'W2317200'.            
040900     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
041000*03  -COPY WDATKORT                                                       
041100     EJECT                                                                
041200*03  -COPY WDATAREA                                                       
041300     EJECT                                                                
041400*    --- PARAMETRAR TILL POSTSUM                                          
041500*                                                                         
041800*01  -COPY W0005   -PRE  POSTSUM-                                         
042400     EJECT                                                                
042500 01  PARM-AREA-START             PIC X(24)   VALUE                        
042600                                 'PARM-AREA-START  '.                     
042700 01  PARM-AREA                   PIC X(13).                               
042800 01  FILLER REDEFINES PARM-AREA.                                          
042900     03  PARM-IDUSER             PIC X(8).                                
043000     03  PARM-IDREFTAB           PIC X.                                   
043010     03  FILLER                  PIC XX.                                  
043020     03  PARM-IDDC               PIC XX.                                  
043100     EJECT                                                                
043200 01  IN-AREA-START               PIC X(24)   VALUE                        
043300                                 'IN-AREA-START    '.                     
043400*01  AREA  -COPY W231701A   -PRE IN-                                      
043500     EJECT                                                                
043600 01  W001-AREA-START             PIC X(24)   VALUE                        
043700                                 'W001-AREA-START  '.                     
043800     SKIP2                                                                
043900 01  W001-HJALPAREOR.                                                     
044000*                                                                         
044100     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
044200     03  W001-ANTAL-RADER                                                 
044300                                 PIC 9(3)    VALUE 999.                   
044400     03  W001-MAX-RADER-PER-SIDA                                          
044500                                 PIC 9(3)    VALUE 63.                    
044600     03  W001-MAX-POSITIONER-PER-RAD                                      
044700                                 PIC 9(3)    VALUE 165.                   
044800     03  W001-LISTNR             PIC X(11)   VALUE SPACE.                 
044900     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
045000     SKIP2                                                                
045100 01  W001-RAD.                                                            
045200     03  FILLER                  PIC X(165)  VALUE SPACE.                 
045300     EJECT                                                                
045400 01  W001-RUBRIK1.                                                        
045500*                                                                         
045600     03  FILLER                  PIC X(3)    VALUE SPACE.                 
045700     03  FILLER                  PIC X(18)                                
045800                              VALUE 'VOLVO CAR PARTS   '.                 
045900     03  W001-LISTID             PIC X(12)                                
046000                                 VALUE SPACE.                             
046100     03  FILLER                  PIC X(20)                                
046200             VALUE 'REFILL UPPFÖLJNING  '.                                
046300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
046400     03  FILLER                  PIC X(9)    VALUE 'SDC-LAGER'.           
046500     03  FILLER                  PIC X(2)    VALUE SPACE.                 
046600     03  W001-AKTUELLT-IDDC      PIC X(2)    VALUE SPACE.                 
046700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
046800     03  FILLER                  PIC X(11)   VALUE 'REFILL TAB.'.         
046900     03  FILLER                  PIC X(2)    VALUE SPACE.                 
047000     03  W001-IDREFTAB           PIC X(1)    VALUE SPACE.                 
047100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
047200     03  FILLER                  PIC X(6)    VALUE 'VECKA '.              
047300     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
047400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
047500     03  FILLER                  PIC X(5)    VALUE 'USER '.               
047600     03  W001-IDUSER             PIC X(8)    VALUE SPACE.                 
047700     03  FILLER                  PIC X(7)    VALUE SPACE.                 
047800     03  W001-DATUM              PIC XXBXXBXX.                            
047900     03  FILLER                  PIC X(7)    VALUE SPACE.                 
048000     03  FILLER                  PIC X(4)    VALUE 'SID '.                
048100     03  W001-SID                PIC Z(4)9.                               
048200     EJECT                                                                
048300 01  W001-RUBRIK3.                                                        
048400*                                                                         
048500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
048600     03  FILLER                  PIC X(9)                                 
048700                             VALUE 'PRISKLASS'.                           
048800     03  FILLER                  PIC X(13)   VALUE SPACE.                 
048900     03  FILLER                  PIC X(7)    VALUE 'A      '.             
049000     03  FILLER                  PIC X(7)    VALUE SPACE.                 
049100     03  FILLER                  PIC X(8)    VALUE 'B       '.            
049200     03  FILLER                  PIC X(6)    VALUE SPACE.                 
049300     03  FILLER                  PIC X(8)    VALUE 'C       '.            
049400     03  FILLER                  PIC X(6)    VALUE SPACE.                 
049500     03  FILLER                  PIC X(9)    VALUE 'D        '.           
049600     03  FILLER                  PIC X(5)    VALUE SPACE.                 
049700     03  FILLER                  PIC X(9)    VALUE 'E        '.           
049800     03  FILLER                  PIC X(5)    VALUE SPACE.                 
049900     03  FILLER                  PIC X(10)   VALUE 'F         '.          
050000     03  FILLER                  PIC X(4)    VALUE SPACE.                 
050100     03  FILLER                  PIC X(10)   VALUE 'G         '.          
050200     03  FILLER                  PIC X(4)    VALUE SPACE.                 
050300     03  FILLER                  PIC X(10)   VALUE 'H         '.          
050400     03  FILLER                  PIC X(11)   VALUE SPACE.                 
050500     03  FILLER                  PIC X(6)    VALUE 'TOTALT'.              
050600     EJECT                                                                
050700 01  W001-DETALJRAD-1.                                                    
050800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050900     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
051000     03  FILLER                  PIC X       VALUE SPACE.                 
051100     03  FILLER                  PIC X(12)   VALUE 'ANT ART  AKT'.        
051200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
051300     03  W001-DET1-KVANTA        PIC Z(7)9.                               
051400     03  FILLER                  PIC X       VALUE SPACE.                 
051500     03  W001-DET1-P-KVANTA      PIC Z9.9.                                
051600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
051700     03  W001-DET1-KVANTB        PIC Z(7)9.                               
051800     03  FILLER                  PIC X       VALUE SPACE.                 
051900     03  W001-DET1-P-KVANTB      PIC Z9.9.                                
052000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052100     03  W001-DET1-KVANTC        PIC Z(7)9.                               
052200     03  FILLER                  PIC X       VALUE SPACE.                 
052300     03  W001-DET1-P-KVANTC      PIC Z9.9.                                
052400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052500     03  W001-DET1-KVANTD        PIC Z(7)9.                               
052600     03  FILLER                  PIC X       VALUE SPACE.                 
052700     03  W001-DET1-P-KVANTD      PIC Z9.9.                                
052800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052900     03  W001-DET1-KVANTE        PIC Z(7)9.                               
053000     03  FILLER                  PIC X       VALUE SPACE.                 
053100     03  W001-DET1-P-KVANTE      PIC Z9.9.                                
053200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053300     03  W001-DET1-KVANTF        PIC Z(7)9.                               
053400     03  FILLER                  PIC X       VALUE SPACE.                 
053500     03  W001-DET1-P-KVANTF      PIC Z9.9.                                
053600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053700     03  W001-DET1-KVANTG        PIC Z(7)9.                               
053800     03  FILLER                  PIC X       VALUE SPACE.                 
053900     03  W001-DET1-P-KVANTG      PIC Z9.9.                                
054000     03  FILLER                  PIC X       VALUE SPACE.                 
054100     03  W001-DET1-KVANTH        PIC Z(7)9.                               
054200     03  FILLER                  PIC X       VALUE SPACE.                 
054300     03  W001-DET1-P-KVANTH      PIC Z9.9.                                
054400     03  FILLER                  PIC X       VALUE SPACE.                 
054500     03  W001-DET1-TOT           PIC Z(13)9.                              
054600     03  FILLER                  PIC X       VALUE SPACE.                 
054700     03  W001-DET1-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
054800     EJECT                                                                
054900 01  W001-DETALJRAD-2.                                                    
055000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055100     03  W001-DET2-PRISKLASS     PIC X       VALUE SPACE.                 
055200     03  FILLER                  PIC X       VALUE SPACE.                 
055300     03  FILLER                  PIC X(12)   VALUE 'ANT ART  PAS'.        
055400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055500     03  W001-DET2-KVANTA        PIC Z(7)9.                               
055600     03  FILLER                  PIC X       VALUE SPACE.                 
055700     03  W001-DET2-P-KVANTA      PIC Z9.9.                                
055800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055900     03  W001-DET2-KVANTB        PIC Z(7)9.                               
056000     03  FILLER                  PIC X       VALUE SPACE.                 
056100     03  W001-DET2-P-KVANTB      PIC Z9.9.                                
056200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056300     03  W001-DET2-KVANTC        PIC Z(7)9.                               
056400     03  FILLER                  PIC X       VALUE SPACE.                 
056500     03  W001-DET2-P-KVANTC      PIC Z9.9.                                
056600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
056700     03  W001-DET2-KVANTD        PIC Z(7)9.                               
056800     03  FILLER                  PIC X       VALUE SPACE.                 
056900     03  W001-DET2-P-KVANTD      PIC Z9.9.                                
057000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057100     03  W001-DET2-KVANTE        PIC Z(7)9.                               
057200     03  FILLER                  PIC X       VALUE SPACE.                 
057300     03  W001-DET2-P-KVANTE      PIC Z9.9.                                
057400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057500     03  W001-DET2-KVANTF        PIC Z(7)9.                               
057600     03  FILLER                  PIC X       VALUE SPACE.                 
057700     03  W001-DET2-P-KVANTF      PIC Z9.9.                                
057800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057900     03  W001-DET2-KVANTG        PIC Z(7)9.                               
058000     03  FILLER                  PIC X       VALUE SPACE.                 
058100     03  W001-DET2-P-KVANTG      PIC Z9.9.                                
058200     03  FILLER                  PIC X       VALUE SPACE.                 
058300     03  W001-DET2-KVANTH        PIC Z(7)9.                               
058400     03  FILLER                  PIC X       VALUE SPACE.                 
058500     03  W001-DET2-P-KVANTH      PIC Z9.9.                                
058600     03  FILLER                  PIC X       VALUE SPACE.                 
058700     03  W001-DET2-TOT           PIC Z(13)9.                              
058800     03  FILLER                  PIC X       VALUE SPACE.                 
058900     03  W001-DET2-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
059000     EJECT                                                                
059100 01  W001-DETALJRAD-3.                                                    
059200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
059300     03  FILLER                  PIC X(12)   VALUE 'DISP LAGER A'.        
059400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059500     03  W001-DET3-DLAGERA       PIC Z(7)9.                               
059600     03  FILLER                  PIC X       VALUE SPACE.                 
059700     03  W001-DET3-P-DLAGERA     PIC Z9.9.                                
059800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059900     03  W001-DET3-DLAGERB       PIC Z(7)9.                               
060000     03  FILLER                  PIC X       VALUE SPACE.                 
060100     03  W001-DET3-P-DLAGERB     PIC Z9.9.                                
060200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060300     03  W001-DET3-DLAGERC       PIC Z(7)9.                               
060400     03  FILLER                  PIC X       VALUE SPACE.                 
060500     03  W001-DET3-P-DLAGERC     PIC Z9.9.                                
060600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
060700     03  W001-DET3-DLAGERD       PIC Z(7)9.                               
060800     03  FILLER                  PIC X       VALUE SPACE.                 
060900     03  W001-DET3-P-DLAGERD     PIC Z9.9.                                
061000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061100     03  W001-DET3-DLAGERE       PIC Z(7)9.                               
061200     03  FILLER                  PIC X       VALUE SPACE.                 
061300     03  W001-DET3-P-DLAGERE     PIC Z9.9.                                
061400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061500     03  W001-DET3-DLAGERF       PIC Z(7)9.                               
061600     03  FILLER                  PIC X       VALUE SPACE.                 
061700     03  W001-DET3-P-DLAGERF     PIC Z9.9.                                
061800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061900     03  W001-DET3-DLAGERG       PIC Z(7)9.                               
062000     03  FILLER                  PIC X       VALUE SPACE.                 
062100     03  W001-DET3-P-DLAGERG     PIC Z9.9.                                
062200     03  FILLER                  PIC X       VALUE SPACE.                 
062300     03  W001-DET3-DLAGERH       PIC Z(7)9.                               
062400     03  FILLER                  PIC X       VALUE SPACE.                 
062500     03  W001-DET3-P-DLAGERH     PIC Z9.9.                                
062600     03  FILLER                  PIC X       VALUE SPACE.                 
062700     03  W001-DET3-TOT           PIC Z(13)9.                              
062800     03  FILLER                  PIC X       VALUE SPACE.                 
062900     03  W001-DET3-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
063000 01  W001-DETALJRAD-4.                                                    
063100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
063200     03  FILLER                  PIC X(12)   VALUE 'DISP LAGER P'.        
063300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063400     03  W001-DET4-DLAGERA       PIC Z(7)9.                               
063500     03  FILLER                  PIC X       VALUE SPACE.                 
063600     03  W001-DET4-P-DLAGERA     PIC Z9.9.                                
063700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063800     03  W001-DET4-DLAGERB       PIC Z(7)9.                               
063900     03  FILLER                  PIC X       VALUE SPACE.                 
064000     03  W001-DET4-P-DLAGERB     PIC Z9.9.                                
064100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064200     03  W001-DET4-DLAGERC       PIC Z(7)9.                               
064300     03  FILLER                  PIC X       VALUE SPACE.                 
064400     03  W001-DET4-P-DLAGERC     PIC Z9.9.                                
064500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
064600     03  W001-DET4-DLAGERD       PIC Z(7)9.                               
064700     03  FILLER                  PIC X       VALUE SPACE.                 
064800     03  W001-DET4-P-DLAGERD     PIC Z9.9.                                
064900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065000     03  W001-DET4-DLAGERE       PIC Z(7)9.                               
065100     03  FILLER                  PIC X       VALUE SPACE.                 
065200     03  W001-DET4-P-DLAGERE     PIC Z9.9.                                
065300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065400     03  W001-DET4-DLAGERF       PIC Z(7)9.                               
065500     03  FILLER                  PIC X       VALUE SPACE.                 
065600     03  W001-DET4-P-DLAGERF     PIC Z9.9.                                
065700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065800     03  W001-DET4-DLAGERG       PIC Z(7)9.                               
065900     03  FILLER                  PIC X       VALUE SPACE.                 
066000     03  W001-DET4-P-DLAGERG     PIC Z9.9.                                
066100     03  FILLER                  PIC X       VALUE SPACE.                 
066200     03  W001-DET4-DLAGERH       PIC Z(7)9.                               
066300     03  FILLER                  PIC X       VALUE SPACE.                 
066400     03  W001-DET4-P-DLAGERH     PIC Z9.9.                                
066500     03  FILLER                  PIC X       VALUE SPACE.                 
066600     03  W001-DET4-TOT           PIC Z(13)9.                              
066700     03  FILLER                  PIC X       VALUE SPACE.                 
066800     03  W001-DET4-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
066900     EJECT                                                                
067000 01  W001-DETALJRAD-5.                                                    
067100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
067200     03  FILLER                  PIC X(12)   VALUE 'LAGERVÄRDE A'.        
067300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067400     03  W001-DET5-LLAGERA       PIC Z(7)9.                               
067500     03  FILLER                  PIC X       VALUE SPACE.                 
067600     03  W001-DET5-P-LLAGERA     PIC Z9.9.                                
067700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067800     03  W001-DET5-LLAGERB       PIC Z(7)9.                               
067900     03  FILLER                  PIC X       VALUE SPACE.                 
068000     03  W001-DET5-P-LLAGERB     PIC Z9.9.                                
068100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068200     03  W001-DET5-LLAGERC       PIC Z(7)9.                               
068300     03  FILLER                  PIC X       VALUE SPACE.                 
068400     03  W001-DET5-P-LLAGERC     PIC Z9.9.                                
068500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
068600     03  W001-DET5-LLAGERD       PIC Z(7)9.                               
068700     03  FILLER                  PIC X       VALUE SPACE.                 
068800     03  W001-DET5-P-LLAGERD     PIC Z9.9.                                
068900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069000     03  W001-DET5-LLAGERE       PIC Z(7)9.                               
069100     03  FILLER                  PIC X       VALUE SPACE.                 
069200     03  W001-DET5-P-LLAGERE     PIC Z9.9.                                
069300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069400     03  W001-DET5-LLAGERF       PIC Z(7)9.                               
069500     03  FILLER                  PIC X       VALUE SPACE.                 
069600     03  W001-DET5-P-LLAGERF     PIC Z9.9.                                
069700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069800     03  W001-DET5-LLAGERG       PIC Z(7)9.                               
069900     03  FILLER                  PIC X       VALUE SPACE.                 
070000     03  W001-DET5-P-LLAGERG     PIC Z9.9.                                
070100     03  FILLER                  PIC X       VALUE SPACE.                 
070200     03  W001-DET5-LLAGERH       PIC Z(7)9.                               
070300     03  FILLER                  PIC X       VALUE SPACE.                 
070400     03  W001-DET5-P-LLAGERH     PIC Z9.9.                                
070500     03  FILLER                  PIC X       VALUE SPACE.                 
070600     03  W001-DET5-TOT           PIC Z(13)9.                              
070700     03  FILLER                  PIC X       VALUE SPACE.                 
070800     03  W001-DET5-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
070900     EJECT                                                                
071000 01  W001-DETALJRAD-6.                                                    
071100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
071200     03  FILLER                  PIC X(12)   VALUE 'LAGERVÄRDE P'.        
071300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071400     03  W001-DET6-LLAGERA       PIC Z(7)9.                               
071500     03  FILLER                  PIC X       VALUE SPACE.                 
071600     03  W001-DET6-P-LLAGERA     PIC Z9.9.                                
071700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071800     03  W001-DET6-LLAGERB       PIC Z(7)9.                               
071900     03  FILLER                  PIC X       VALUE SPACE.                 
072000     03  W001-DET6-P-LLAGERB     PIC Z9.9.                                
072100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072200     03  W001-DET6-LLAGERC       PIC Z(7)9.                               
072300     03  FILLER                  PIC X       VALUE SPACE.                 
072400     03  W001-DET6-P-LLAGERC     PIC Z9.9.                                
072500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
072600     03  W001-DET6-LLAGERD       PIC Z(7)9.                               
072700     03  FILLER                  PIC X       VALUE SPACE.                 
072800     03  W001-DET6-P-LLAGERD     PIC Z9.9.                                
072900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073000     03  W001-DET6-LLAGERE       PIC Z(7)9.                               
073100     03  FILLER                  PIC X       VALUE SPACE.                 
073200     03  W001-DET6-P-LLAGERE     PIC Z9.9.                                
073300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073400     03  W001-DET6-LLAGERF       PIC Z(7)9.                               
073500     03  FILLER                  PIC X       VALUE SPACE.                 
073600     03  W001-DET6-P-LLAGERF     PIC Z9.9.                                
073700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073800     03  W001-DET6-LLAGERG       PIC Z(7)9.                               
073900     03  FILLER                  PIC X       VALUE SPACE.                 
074000     03  W001-DET6-P-LLAGERG     PIC Z9.9.                                
074100     03  FILLER                  PIC X       VALUE SPACE.                 
074200     03  W001-DET6-LLAGERH       PIC Z(7)9.                               
074300     03  FILLER                  PIC X       VALUE SPACE.                 
074400     03  W001-DET6-P-LLAGERH     PIC Z9.9.                                
074500     03  FILLER                  PIC X       VALUE SPACE.                 
074600     03  W001-DET6-TOT           PIC Z(13)9.                              
074700     03  FILLER                  PIC X       VALUE SPACE.                 
074800     03  W001-DET6-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
074900     EJECT                                                                
075000 01  W001-DETALJRAD-7.                                                    
075100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
075200     03  FILLER                  PIC X(12)   VALUE 'AK VÄRDE   A'.        
075300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075400     03  W001-DET7-ALAGERA       PIC Z(7)9.                               
075500     03  FILLER                  PIC X       VALUE SPACE.                 
075600     03  W001-DET7-P-ALAGERA     PIC Z9.9.                                
075700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075800     03  W001-DET7-ALAGERB       PIC Z(7)9.                               
075900     03  FILLER                  PIC X       VALUE SPACE.                 
076000     03  W001-DET7-P-ALAGERB     PIC Z9.9.                                
076100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076200     03  W001-DET7-ALAGERC       PIC Z(7)9.                               
076300     03  FILLER                  PIC X       VALUE SPACE.                 
076400     03  W001-DET7-P-ALAGERC     PIC Z9.9.                                
076500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
076600     03  W001-DET7-ALAGERD       PIC Z(7)9.                               
076700     03  FILLER                  PIC X       VALUE SPACE.                 
076800     03  W001-DET7-P-ALAGERD     PIC Z9.9.                                
076900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077000     03  W001-DET7-ALAGERE       PIC Z(7)9.                               
077100     03  FILLER                  PIC X       VALUE SPACE.                 
077200     03  W001-DET7-P-ALAGERE     PIC Z9.9.                                
077300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077400     03  W001-DET7-ALAGERF       PIC Z(7)9.                               
077500     03  FILLER                  PIC X       VALUE SPACE.                 
077600     03  W001-DET7-P-ALAGERF     PIC Z9.9.                                
077700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077800     03  W001-DET7-ALAGERG       PIC Z(7)9.                               
077900     03  FILLER                  PIC X       VALUE SPACE.                 
078000     03  W001-DET7-P-ALAGERG     PIC Z9.9.                                
078100     03  FILLER                  PIC X       VALUE SPACE.                 
078200     03  W001-DET7-ALAGERH       PIC Z(7)9.                               
078300     03  FILLER                  PIC X       VALUE SPACE.                 
078400     03  W001-DET7-P-ALAGERH     PIC Z9.9.                                
078500     03  FILLER                  PIC X       VALUE SPACE.                 
078600     03  W001-DET7-TOT           PIC Z(13)9.                              
078700     03  FILLER                  PIC X       VALUE SPACE.                 
078800     03  W001-DET7-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
078900     EJECT                                                                
079000 01  W001-DETALJRAD-8.                                                    
079100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
079200     03  FILLER                  PIC X(12)   VALUE 'AK VÄRDE   P'.        
079300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079400     03  W001-DET8-ALAGERA       PIC Z(7)9.                               
079500     03  FILLER                  PIC X       VALUE SPACE.                 
079600     03  W001-DET8-P-ALAGERA     PIC Z9.9.                                
079700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079800     03  W001-DET8-ALAGERB       PIC Z(7)9.                               
079900     03  FILLER                  PIC X       VALUE SPACE.                 
080000     03  W001-DET8-P-ALAGERB     PIC Z9.9.                                
080100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080200     03  W001-DET8-ALAGERC       PIC Z(7)9.                               
080300     03  FILLER                  PIC X       VALUE SPACE.                 
080400     03  W001-DET8-P-ALAGERC     PIC Z9.9.                                
080500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
080600     03  W001-DET8-ALAGERD       PIC Z(7)9.                               
080700     03  FILLER                  PIC X       VALUE SPACE.                 
080800     03  W001-DET8-P-ALAGERD     PIC Z9.9.                                
080900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081000     03  W001-DET8-ALAGERE       PIC Z(7)9.                               
081100     03  FILLER                  PIC X       VALUE SPACE.                 
081200     03  W001-DET8-P-ALAGERE     PIC Z9.9.                                
081300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081400     03  W001-DET8-ALAGERF       PIC Z(7)9.                               
081500     03  FILLER                  PIC X       VALUE SPACE.                 
081600     03  W001-DET8-P-ALAGERF     PIC Z9.9.                                
081700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081800     03  W001-DET8-ALAGERG       PIC Z(7)9.                               
081900     03  FILLER                  PIC X       VALUE SPACE.                 
082000     03  W001-DET8-P-ALAGERG     PIC Z9.9.                                
082100     03  FILLER                  PIC X       VALUE SPACE.                 
082200     03  W001-DET8-ALAGERH       PIC Z(7)9.                               
082300     03  FILLER                  PIC X       VALUE SPACE.                 
082400     03  W001-DET8-P-ALAGERH     PIC Z9.9.                                
082500     03  FILLER                  PIC X       VALUE SPACE.                 
082600     03  W001-DET8-TOT           PIC Z(13)9.                              
082700     03  FILLER                  PIC X       VALUE SPACE.                 
082800     03  W001-DET8-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
082900     EJECT                                                                
083000 01  W001-DETALJRAD-9.                                                    
083100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
083200     03  FILLER                  PIC X(12)   VALUE 'ÖVERLAGER   '.        
083300     03  FILLER                  PIC X       VALUE SPACE.                 
083400     03  W001-DET9-OLAGERA       PIC Z(7)9.                               
083500     03  FILLER                  PIC X       VALUE SPACE.                 
083600     03  W001-DET9-P-OLAGERA     PIC Z9.9    BLANK WHEN ZERO.             
083700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083800     03  W001-DET9-OLAGERB       PIC Z(7)9.                               
083900     03  FILLER                  PIC X       VALUE SPACE.                 
084000     03  W001-DET9-P-OLAGERB     PIC Z9.9    BLANK WHEN ZERO.             
084100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084200     03  W001-DET9-OLAGERC       PIC Z(7)9.                               
084300     03  FILLER                  PIC X       VALUE SPACE.                 
084400     03  W001-DET9-P-OLAGERC     PIC Z9.9    BLANK WHEN ZERO.             
084500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
084600     03  W001-DET9-OLAGERD       PIC Z(7)9.                               
084700     03  FILLER                  PIC X       VALUE SPACE.                 
084800     03  W001-DET9-P-OLAGERD     PIC Z9.9    BLANK WHEN ZERO.             
084900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085000     03  W001-DET9-OLAGERE       PIC Z(7)9.                               
085100     03  FILLER                  PIC X       VALUE SPACE.                 
085200     03  W001-DET9-P-OLAGERE     PIC Z9.9    BLANK WHEN ZERO.             
085300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085400     03  W001-DET9-OLAGERF       PIC Z(7)9.                               
085500     03  FILLER                  PIC X       VALUE SPACE.                 
085600     03  W001-DET9-P-OLAGERF     PIC Z9.9    BLANK WHEN ZERO.             
085700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085800     03  W001-DET9-OLAGERG       PIC Z(7)9.                               
085900     03  FILLER                  PIC X       VALUE SPACE.                 
086000     03  W001-DET9-P-OLAGERG     PIC Z9.9    BLANK WHEN ZERO.             
086100     03  FILLER                  PIC X       VALUE SPACE.                 
086200     03  W001-DET9-OLAGERH       PIC Z(7)9.                               
086300     03  FILLER                  PIC X       VALUE SPACE.                 
086400     03  W001-DET9-P-OLAGERH     PIC Z9.9    BLANK WHEN ZERO.             
086500     03  FILLER                  PIC X       VALUE SPACE.                 
086600     03  W001-DET9-TOT           PIC Z(13)9.                              
086700     03  FILLER                  PIC X       VALUE SPACE.                 
086800     03  W001-DET9-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
086900     EJECT                                                                
087000 01  W001-DETALJRAD-10.                                                   
087100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
087200     03  FILLER                  PIC X(12)   VALUE 'SÄK-LAGER   '.        
087300     03  FILLER                  PIC X       VALUE SPACE.                 
087400     03  W001-DET10-SLAGERA      PIC Z(7)9.                               
087500     03  FILLER                  PIC X       VALUE SPACE.                 
087600     03  W001-DET10-P-SLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
087700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087800     03  W001-DET10-SLAGERB      PIC Z(7)9.                               
087900     03  FILLER                  PIC X       VALUE SPACE.                 
088000     03  W001-DET10-P-SLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
088100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088200     03  W001-DET10-SLAGERC      PIC Z(7)9.                               
088300     03  FILLER                  PIC X       VALUE SPACE.                 
088400     03  W001-DET10-P-SLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
088500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
088600     03  W001-DET10-SLAGERD      PIC Z(7)9.                               
088700     03  FILLER                  PIC X       VALUE SPACE.                 
088800     03  W001-DET10-P-SLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
088900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089000     03  W001-DET10-SLAGERE      PIC Z(7)9.                               
089100     03  FILLER                  PIC X       VALUE SPACE.                 
089200     03  W001-DET10-P-SLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
089300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089400     03  W001-DET10-SLAGERF      PIC Z(7)9.                               
089500     03  FILLER                  PIC X       VALUE SPACE.                 
089600     03  W001-DET10-P-SLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
089700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089800     03  W001-DET10-SLAGERG      PIC Z(7)9.                               
089900     03  FILLER                  PIC X       VALUE SPACE.                 
090000     03  W001-DET10-P-SLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
090100     03  FILLER                  PIC X       VALUE SPACE.                 
090200     03  W001-DET10-SLAGERH      PIC Z(7)9.                               
090300     03  FILLER                  PIC X       VALUE SPACE.                 
090400     03  W001-DET10-P-SLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
090500     03  FILLER                  PIC X       VALUE SPACE.                 
090600     03  W001-DET10-TOT          PIC Z(13)9.                              
090700     03  FILLER                  PIC X       VALUE SPACE.                 
090800     03  W001-DET10-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
090900     EJECT                                                                
091000 01  W001-DETALJRAD-11.                                                   
091100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
091200     03  FILLER                  PIC X(12)   VALUE 'MEDELLAGER  '.        
091300     03  FILLER                  PIC X       VALUE SPACE.                 
091400     03  W001-DET11-MLAGERA      PIC Z(7)9.                               
091500     03  FILLER                  PIC X       VALUE SPACE.                 
091600     03  W001-DET11-P-MLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
091700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091800     03  W001-DET11-MLAGERB      PIC Z(7)9.                               
091900     03  FILLER                  PIC X       VALUE SPACE.                 
092000     03  W001-DET11-P-MLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
092100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092200     03  W001-DET11-MLAGERC      PIC Z(7)9.                               
092300     03  FILLER                  PIC X       VALUE SPACE.                 
092400     03  W001-DET11-P-MLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
092500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092600     03  W001-DET11-MLAGERD      PIC Z(7)9.                               
092700     03  FILLER                  PIC X       VALUE SPACE.                 
092800     03  W001-DET11-P-MLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
092900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093000     03  W001-DET11-MLAGERE      PIC Z(7)9.                               
093100     03  FILLER                  PIC X       VALUE SPACE.                 
093200     03  W001-DET11-P-MLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
093300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093400     03  W001-DET11-MLAGERF      PIC Z(7)9.                               
093500     03  FILLER                  PIC X       VALUE SPACE.                 
093600     03  W001-DET11-P-MLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
093700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093800     03  W001-DET11-MLAGERG      PIC Z(7)9.                               
093900     03  FILLER                  PIC X       VALUE SPACE.                 
094000     03  W001-DET11-P-MLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
094100     03  FILLER                  PIC X       VALUE SPACE.                 
094200     03  W001-DET11-MLAGERH      PIC Z(7)9.                               
094300     03  FILLER                  PIC X       VALUE SPACE.                 
094400     03  W001-DET11-P-MLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
094500     03  FILLER                  PIC X       VALUE SPACE.                 
094600     03  W001-DET11-TOT          PIC Z(13)9.                              
094700     03  FILLER                  PIC X       VALUE SPACE.                 
094800     03  W001-DET11-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
094900     EJECT                                                                
095000 01  W001-DETALJRAD-12.                                                   
095100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095200     03  W001-DET12-PRISKLASS    PIC X       VALUE SPACE.                 
095300     03  FILLER                  PIC X       VALUE SPACE.                 
095400     03  FILLER                  PIC X(12)   VALUE 'ORDERTRÄFFAR'.        
095500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095600     03  W001-DET12-KVOTA        PIC Z(7)9.                               
095700     03  FILLER                  PIC X       VALUE SPACE.                 
095800     03  W001-DET12-P-KVOTA      PIC Z9.9.                                
095900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096000     03  W001-DET12-KVOTB        PIC Z(7)9.                               
096100     03  FILLER                  PIC X       VALUE SPACE.                 
096200     03  W001-DET12-P-KVOTB      PIC Z9.9.                                
096300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096400     03  W001-DET12-KVOTC        PIC Z(7)9.                               
096500     03  FILLER                  PIC X       VALUE SPACE.                 
096600     03  W001-DET12-P-KVOTC      PIC Z9.9.                                
096700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
096800     03  W001-DET12-KVOTD        PIC Z(7)9.                               
096900     03  FILLER                  PIC X       VALUE SPACE.                 
097000     03  W001-DET12-P-KVOTD      PIC Z9.9.                                
097100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097200     03  W001-DET12-KVOTE        PIC Z(7)9.                               
097300     03  FILLER                  PIC X       VALUE SPACE.                 
097400     03  W001-DET12-P-KVOTE      PIC Z9.9.                                
097500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
097600     03  W001-DET12-KVOTF        PIC Z(7)9.                               
097700     03  FILLER                  PIC X       VALUE SPACE.                 
097800     03  W001-DET12-P-KVOTF      PIC Z9.9.                                
097900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
098000     03  W001-DET12-KVOTG        PIC Z(7)9.                               
098100     03  FILLER                  PIC X       VALUE SPACE.                 
098200     03  W001-DET12-P-KVOTG      PIC Z9.9.                                
098300     03  FILLER                  PIC X       VALUE SPACE.                 
098400     03  W001-DET12-KVOTH        PIC Z(7)9.                               
098500     03  FILLER                  PIC X       VALUE SPACE.                 
098600     03  W001-DET12-P-KVOTH      PIC Z9.9.                                
098700     03  FILLER                  PIC X       VALUE SPACE.                 
098800     03  W001-DET12-TOT          PIC Z(13)9.                              
098900     03  FILLER                  PIC X       VALUE SPACE.                 
099000     03  W001-DET12-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
099100     EJECT                                                                
099200 01  W001-DETALJRAD-13.                                                   
099300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
099400     03  FILLER                  PIC X(13) VALUE 'SPLITFAKTOR  '.         
099500     03  FILLER                  PIC X(4)    VALUE SPACE.                 
099600     03  W001-DET13-SPLITA       PIC Z9.9.                                
099700     03  FILLER                  PIC X       VALUE SPACE.                 
099800     03  W001-DET13-FILLERA      PIC X(4)    VALUE SPACE.                 
099900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
100000     03  W001-DET13-SPLITB       PIC Z9.9.                                
100100     03  FILLER                  PIC X       VALUE SPACE.                 
100200     03  W001-DET13-FILLERB      PIC X(4)    VALUE SPACE.                 
100300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
100400     03  W001-DET13-SPLITC       PIC Z9.9.                                
100500     03  FILLER                  PIC X       VALUE SPACE.                 
100600     03  W001-DET13-FILLERC      PIC X(4)    VALUE SPACE.                 
100700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
100800     03  W001-DET13-SPLITD       PIC Z9.9.                                
100900     03  FILLER                  PIC X       VALUE SPACE.                 
101000     03  W001-DET13-FILLERD      PIC X(4)    VALUE SPACE.                 
101100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
101200     03  W001-DET13-SPLITE       PIC Z9.9.                                
101300     03  FILLER                  PIC X       VALUE SPACE.                 
101400     03  W001-DET13-FILLERE      PIC X(4)    VALUE SPACE.                 
101500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
101600     03  W001-DET13-SPLITF       PIC Z9.9.                                
101700     03  FILLER                  PIC X       VALUE SPACE.                 
101800     03  W001-DET13-FILLERF      PIC X(4)    VALUE SPACE.                 
101900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
102000     03  W001-DET13-SPLITG       PIC Z9.9.                                
102100     03  FILLER                  PIC X       VALUE SPACE.                 
102200     03  W001-DET13-FILLERG      PIC X(4)    VALUE SPACE.                 
102300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
102400     03  W001-DET13-SPLITH       PIC Z9.9.                                
102500     03  FILLER                  PIC X       VALUE SPACE.                 
102600     03  W001-DET13-FILLERH      PIC X(4)    VALUE SPACE.                 
102700     03  FILLER                  PIC X(11)   VALUE SPACE.                 
102800     03  W001-DET13-TOT          PIC Z9.9.                                
102900     03  FILLER                  PIC X       VALUE SPACE.                 
103000     03  W001-DET13-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
103100     EJECT                                                                
103200 01  W001-DETALJRAD-14.                                                   
103300     03  FILLER                  PIC X(3)    VALUE SPACE.                 
103400     03  FILLER                  PIC X(13) VALUE 'TOR      DISP'.         
103500     03  W001-DET14-OMSHASTA     PIC Z(5)9.9.                             
103600     03  FILLER                  PIC X       VALUE SPACE.                 
103700     03  W001-DET14-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
103800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103900     03  W001-DET14-OMSHASTB     PIC Z(5)9.9.                             
104000     03  FILLER                  PIC X       VALUE SPACE.                 
104100     03  W001-DET14-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
104200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104300     03  W001-DET14-OMSHASTC     PIC Z(5)9.9.                             
104400     03  FILLER                  PIC X       VALUE SPACE.                 
104500     03  W001-DET14-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
104600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
104700     03  W001-DET14-OMSHASTD     PIC Z(5)9.9.                             
104800     03  FILLER                  PIC X       VALUE SPACE.                 
104900     03  W001-DET14-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
105000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105100     03  W001-DET14-OMSHASTE     PIC Z(5)9.9.                             
105200     03  FILLER                  PIC X       VALUE SPACE.                 
105300     03  W001-DET14-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
105400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105500     03  W001-DET14-OMSHASTF     PIC Z(5)9.9.                             
105600     03  FILLER                  PIC X       VALUE SPACE.                 
105700     03  W001-DET14-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
105800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105900     03  W001-DET14-OMSHASTG     PIC Z(5)9.9.                             
106000     03  FILLER                  PIC X       VALUE SPACE.                 
106100     03  W001-DET14-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
106200     03  FILLER                  PIC X       VALUE SPACE.                 
106300     03  W001-DET14-OMSHASTH     PIC Z(5)9.9.                             
106400     03  FILLER                  PIC X       VALUE SPACE.                 
106500     03  W001-DET14-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
106600     03  FILLER                  PIC X       VALUE SPACE.                 
106700     03  W001-DET14-TOT          PIC Z(11)9.9.                            
106800     03  FILLER                  PIC X       VALUE SPACE.                 
106900     03  W001-DET14-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
107000     EJECT                                                                
107100 01  W001-DETALJRAD-15.                                                   
107200     03  FILLER                  PIC X(3)    VALUE SPACE.                 
107300     03  FILLER                  PIC X(13)  VALUE 'TOR LS+AK+GIT'.        
107400     03  W001-DET15-OMSHASTA     PIC Z(5)9.9.                             
107500     03  FILLER                  PIC X       VALUE SPACE.                 
107600     03  W001-DET15-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
107700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107800     03  W001-DET15-OMSHASTB     PIC Z(5)9.9.                             
107900     03  FILLER                  PIC X       VALUE SPACE.                 
108000     03  W001-DET15-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
108100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108200     03  W001-DET15-OMSHASTC     PIC Z(5)9.9.                             
108300     03  FILLER                  PIC X       VALUE SPACE.                 
108400     03  W001-DET15-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
108500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
108600     03  W001-DET15-OMSHASTD     PIC Z(5)9.9.                             
108700     03  FILLER                  PIC X       VALUE SPACE.                 
108800     03  W001-DET15-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
108900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109000     03  W001-DET15-OMSHASTE     PIC Z(5)9.9.                             
109100     03  FILLER                  PIC X       VALUE SPACE.                 
109200     03  W001-DET15-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
109300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109400     03  W001-DET15-OMSHASTF     PIC Z(5)9.9.                             
109500     03  FILLER                  PIC X       VALUE SPACE.                 
109600     03  W001-DET15-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
109700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
109800     03  W001-DET15-OMSHASTG     PIC Z(5)9.9.                             
109900     03  FILLER                  PIC X       VALUE SPACE.                 
110000     03  W001-DET15-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
110100     03  FILLER                  PIC X       VALUE SPACE.                 
110200     03  W001-DET15-OMSHASTH     PIC Z(5)9.9.                             
110300     03  FILLER                  PIC X       VALUE SPACE.                 
110400     03  W001-DET15-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
110500     03  FILLER                  PIC X       VALUE SPACE.                 
110600     03  W001-DET15-TOT          PIC Z(11)9.9.                            
110700     03  FILLER                  PIC X       VALUE SPACE.                 
110800     03  W001-DET15-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
110900     EJECT                                                                
111000 01  W001-DETALJRAD-16.                                                   
111100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
111200     03  FILLER                  PIC X(12) VALUE 'SERVGRAD A/P'.          
111300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
111400     03  W001-DET16-SERVGA-A     PIC Z9.9.                                
111500     03  FILLER                  PIC X       VALUE SPACE.                 
111600     03  W001-DET16-SERVGA-P     PIC Z9.9.                                
111700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
111800     03  W001-DET16-SERVGB-A     PIC Z9.9.                                
111900     03  FILLER                  PIC X       VALUE SPACE.                 
112000     03  W001-DET16-SERVGB-P     PIC Z9.9.                                
112100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
112200     03  W001-DET16-SERVGC-A     PIC Z9.9.                                
112300     03  FILLER                  PIC X       VALUE SPACE.                 
112400     03  W001-DET16-SERVGC-P     PIC Z9.9.                                
112500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
112600     03  W001-DET16-SERVGD-A     PIC Z9.9.                                
112700     03  FILLER                  PIC X       VALUE SPACE.                 
112800     03  W001-DET16-SERVGD-P     PIC Z9.9.                                
112900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113000     03  W001-DET16-SERVGE-A     PIC Z9.9.                                
113100     03  FILLER                  PIC X       VALUE SPACE.                 
113200     03  W001-DET16-SERVGE-P     PIC Z9.9.                                
113300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113400     03  W001-DET16-SERVGF-A     PIC Z9.9.                                
113500     03  FILLER                  PIC X       VALUE SPACE.                 
113600     03  W001-DET16-SERVGF-P     PIC Z9.9.                                
113700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113800     03  W001-DET16-SERVGG-A     PIC Z9.9.                                
113900     03  FILLER                  PIC X       VALUE SPACE.                 
114000     03  W001-DET16-SERVGG-P     PIC Z9.9.                                
114100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
114200     03  W001-DET16-SERVGH-A     PIC Z9.9.                                
114300     03  FILLER                  PIC X       VALUE SPACE.                 
114400     03  W001-DET16-SERVGH-P     PIC Z9.9.                                
114500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
114600     03  W001-DET16-TOT          PIC Z9.9.                                
114700     03  FILLER                  PIC X       VALUE SPACE.                 
114800     03  W001-DET16-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
114900     EJECT                                                                
115000 01  W001-DETALJRAD-17.                                                   
115100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
115200     03  FILLER                  PIC X(12) VALUE 'SERVGRAD TOT'.          
115300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
115400     03  W001-DET17-SERVGA-TOT   PIC Z9.9.                                
115500     03  FILLER                  PIC X       VALUE SPACE.                 
115600     03  W001-DET17-SERVGA-BTO   PIC X(4)    VALUE SPACE.                 
115700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
115800     03  W001-DET17-SERVGB-TOT   PIC Z9.9.                                
115900     03  FILLER                  PIC X       VALUE SPACE.                 
116000     03  W001-DET17-SERVGB-BTO   PIC X(4)    VALUE SPACE.                 
116100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
116200     03  W001-DET17-SERVGC-TOT   PIC Z9.9.                                
116300     03  FILLER                  PIC X       VALUE SPACE.                 
116400     03  W001-DET17-SERVGC-BTO   PIC X(4)    VALUE SPACE.                 
116500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
116600     03  W001-DET17-SERVGD-TOT   PIC Z9.9.                                
116700     03  FILLER                  PIC X       VALUE SPACE.                 
116800     03  W001-DET17-SERVGD-BTO   PIC X(4)    VALUE SPACE.                 
116900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117000     03  W001-DET17-SERVGE-TOT   PIC Z9.9.                                
117100     03  FILLER                  PIC X       VALUE SPACE.                 
117200     03  W001-DET17-SERVGE-BTO   PIC X(4)    VALUE SPACE.                 
117300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117400     03  W001-DET17-SERVGF-TOT   PIC Z9.9.                                
117500     03  FILLER                  PIC X       VALUE SPACE.                 
117600     03  W001-DET17-SERVGF-BTO   PIC X(4)    VALUE SPACE.                 
117700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117800     03  W001-DET17-SERVGG-TOT   PIC Z9.9.                                
117900     03  FILLER                  PIC X       VALUE SPACE.                 
118000     03  W001-DET17-SERVGG-BTO   PIC X(4)    VALUE SPACE.                 
118100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
118200     03  W001-DET17-SERVGH-TOT   PIC Z9.9.                                
118300     03  FILLER                  PIC X       VALUE SPACE.                 
118400     03  W001-DET17-SERVGH-BTO   PIC X(4)    VALUE SPACE.                 
118500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
118600     03  W001-DET17-TOT          PIC Z9.9.                                
118700     03  FILLER                  PIC X       VALUE SPACE.                 
118800     03  W001-DET17-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
118900     EJECT                                                                
119000 01  W001-DETALJRAD-18.                                                   
119100     03  FILLER                  PIC X(3)    VALUE SPACE.                 
119200     03  FILLER                  PIC X(12) VALUE 'SERVGRAD TEO'.          
119300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
119400     03  W001-DET18-SERVGA-TEO   PIC Z9.9.                                
119500     03  FILLER                  PIC X       VALUE SPACE.                 
119600     03  W001-DET18-SERVGA-BTO   PIC X(4)    VALUE SPACE.                 
119700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
119800     03  W001-DET18-SERVGB-TEO   PIC Z9.9.                                
119900     03  FILLER                  PIC X       VALUE SPACE.                 
120000     03  W001-DET18-SERVGB-BTO   PIC X(4)    VALUE SPACE.                 
120100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
120200     03  W001-DET18-SERVGC-TEO   PIC Z9.9.                                
120300     03  FILLER                  PIC X       VALUE SPACE.                 
120400     03  W001-DET18-SERVGC-BTO   PIC X(4)    VALUE SPACE.                 
120500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
120600     03  W001-DET18-SERVGD-TEO   PIC Z9.9.                                
120700     03  FILLER                  PIC X       VALUE SPACE.                 
120800     03  W001-DET18-SERVGD-BTO   PIC X(4)    VALUE SPACE.                 
120900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
121000     03  W001-DET18-SERVGE-TEO   PIC Z9.9.                                
121100     03  FILLER                  PIC X       VALUE SPACE.                 
121200     03  W001-DET18-SERVGE-BTO   PIC X(4)    VALUE SPACE.                 
121300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
121400     03  W001-DET18-SERVGF-TEO   PIC Z9.9.                                
121500     03  FILLER                  PIC X       VALUE SPACE.                 
121600     03  W001-DET18-SERVGF-BTO   PIC X(4)    VALUE SPACE.                 
121700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
121800     03  W001-DET18-SERVGG-TEO   PIC Z9.9.                                
121900     03  FILLER                  PIC X       VALUE SPACE.                 
122000     03  W001-DET18-SERVGG-BTO   PIC X(4)    VALUE SPACE.                 
122100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
122200     03  W001-DET18-SERVGH-TEO   PIC Z9.9.                                
122300     03  FILLER                  PIC X       VALUE SPACE.                 
122400     03  W001-DET18-SERVGH-BTO   PIC X(4)    VALUE SPACE.                 
122500     03  FILLER                  PIC X(11)   VALUE SPACE.                 
122600     03  W001-DET18-TOT          PIC Z9.9.                                
122700     03  FILLER                  PIC X       VALUE SPACE.                 
122800     03  W001-DET18-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
122801                                                                          
122810*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
122820*                                                                         
122830 01      IMS-WS.                                                          
122840   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
122850     SKIP3                                                                
122860*                            *** STATUSKOD FRÅN IMS                       
122870   03    STATUS-WS       PIC XX.                                          
122880     88  SEGMENT-FINNS               VALUE '  '.                          
122890     88  SEGMENT-SAKNAS              VALUE 'GE'                           
122891                                           'GB'.                          
122892     03    GODK-STATUSKODER.                                              
122893         05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.           
122894     SKIP3                                                                
122895   03    SSA1            PIC X(128).                                      
122896   03    SSA2            PIC X(128).                                      
122897                                                                          
122898 01  NYCKLAR-TILL-DLI.                                                    
122899     03  W-IDDC-B6-X.                                                     
122900         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
122901     03  W-IDDC-B616-X.                                                   
122902         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
122903     SKIP3                                                                
122904*01      -COPY W0003                                                      
122905*    ---  DLI INPUT-OUTPUT AREA                                           
122906 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
122907 01   DLI-IO-AREA-B6      PIC X(900).                                     
122908 01   DLI-IO-AREA-B601    REDEFINES DLI-IO-AREA-B6.                       
122909*     03  -COPY WDB601                                                    
122910     EJECT                                                                
122911 01   DLI-IO-AREA-B616    REDEFINES DLI-IO-AREA-B6.                       
122912*     03  -COPY WDB616                                                    
122918                                                                          
122919     EJECT                                                                
122920 LINKAGE SECTION.                                                         
122921     SKIP3                                                                
122922*01  AREA  -COPY W61159      -PRE LINK-.                                  
122923     EJECT                                                                
122924*01  -COPY W0008  -PRE WDB6-.                                             
122925     05  FILLER              PIC X.                                       
122926     EJECT                                                                
122930     EJECT                                                                
123000 PROCEDURE DIVISION USING WDB6-PCB.                                       
123100                                                                          
123200     PERFORM A-INIT                                                       
123300     PERFORM B-SKAPA-LISTA                                                
123400     IF SW-TRAEFF = JA                                                    
123500        PERFORM C-SKRIV-LISTA                                             
123600     END-IF                                                               
123700     PERFORM Z-FINIT                                                      
123800                                                                          
123900     MOVE ZERO TO RETURN-CODE                                             
124000     GOBACK                                                               
124100     .                                                                    
124200     EJECT                                                                
124300 A-INIT SECTION.                                                          
124400                                                                          
124500     OPEN INPUT  W23170                                                   
124700                 W231PP                                                   
124800     OPEN OUTPUT W23172-001                                               
124900                                                                          
125000     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
125100     MOVE D-AAR    TO DAGENS-AAR                                          
125200                      D-VECKA-AAR                                         
125300     MOVE D-MAANAD TO DAGENS-MAANAD                                       
125400     MOVE D-VECKA  TO D-VECKA-VECKA                                       
125500     MOVE D-DAG    TO DAGENS-DAG                                          
125600     MOVE DAGENS-DATUM TO W001-DATUM                                      
125700     MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                              
125800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
125900     MOVE NEJ TO SW-TRAEFF                                                
126000                                                                          
127400     MOVE 'W23172-001' TO W001-LISTNR                                     
127500                          W001-LISTID                                     
127600                                                                          
127700     PERFORM S02-LAS-W231PP                                               
127710     DISPLAY '******** PARM-IDREFTAB ** ' PARM-IDREFTAB                   
127720     DISPLAY '********** PARM-IDUSER ** ' PARM-IDUSER                     
127730     DISPLAY '********** PARM-IDDC   ** ' PARM-IDDC                       
127800     MOVE PARM-IDREFTAB TO W001-IDREFTAB                                  
127900     MOVE PARM-IDUSER   TO W001-IDUSER                                    
127910     MOVE PARM-IDDC     TO WS-IDDC                                        
128000                           WS-SDC-NUM                                     
128010                           W001-AKTUELLT-IDDC                             
128020                                                                          
128100     MOVE 'IDAG' TO DAT-KDDATFORM                                         
128200     CALL WDATKONV USING DAT-KDDATFORM                                    
128300                         DAT-I-TIDATUM                                    
128400                         DAT-O-TIDATUM                                    
128500                         DAT-KDSVAR                                       
128600     IF DAT-KDSVAR-OK                                                     
128700        MOVE DAT-TIAAVVD TO AAVVD                                         
128800        MOVE AAVV        TO W009VADD-DATUM-2                              
128900        MOVE -1          TO W009VADD-ANTAL-2                              
129000     ELSE                                                                 
129100        MOVE ZERO        TO W009VADD-DATUM-2                              
129200     END-IF                                                               
129300     CALL W009VADD USING W009VADD-DATUM-2 W009VADD-ANTAL-2                
129400     MOVE W009VADD-DATUM-2 TO W001-AKTUELL-VECKA                          
129500*******                                                                   
129600     MOVE W001-AKTUELL-VECKA TO TEST-VECKA                                
129700     MOVE TEST-VV TO D-VECKA-VECKA                                        
129800*******                                                                   
129900     .                                                                    
130500     EJECT                                                                
130610 B-SKAPA-LISTA SECTION.                                                   
130700                                                                          
130800     PERFORM BA-NOLLSTALL                                                 
130900                                                                          
131000     PERFORM S01-LAS-W23170                                               
131100     PERFORM UNTIL END-OF-W23170                                          
131200       IF IN-IDDC = PARM-IDDC                                             
131300         IF PARM-IDREFTAB = IN-IDREFTAB                                   
131400            PERFORM BB-SKAPA-TABELLER                                     
131500            MOVE JA TO SW-TRAEFF                                          
131600         END-IF                                                           
131610       END-IF                                                             
131700       PERFORM S01-LAS-W23170                                             
131800     END-PERFORM                                                          
131900                                                                          
132000     IF SW-TRAEFF = JA                                                    
132100        PERFORM BC-SUMMERA                                                
132200     END-IF                                                               
132300     .                                                                    
132400     EJECT                                                                
132500 BA-NOLLSTALL SECTION.                                                    
132600******************************************************************        
132700*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
132800*  FREKVENSKLASS                                                 *        
132900*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
133000*     FREKV-KLASSER  = A B C D E F G                             *        
133100*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
133200*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
133300*                 TOTAL-SUMMERING                                *        
133400* ****************************************************************        
133500                                                                          
133600******* NOLLSTÄLLNING AV 63 'RUTOR' PER PRISKLASS/FREKVKLASS              
133700                                                                          
133800     MOVE +1  TO ART-IX                                                   
133900*    MOVE +63 TO ART-IX-MAX                                               
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
137100                         ART-SERVG-TOT(ART-IX)                            
137200                         ART-SERVG-AKT(ART-IX)                            
137300                         ART-SERVG-PAS(ART-IX)                            
137400                         ART-SERVG-TEO(ART-IX)                            
137500                         WS-ART-SLAGER(ART-IX)                            
137600                         WS-ART-OLAGER(ART-IX)                            
137700                         WS-ART-MLAGER(ART-IX)                            
137800                         WS-ART-KVLS-AKT(ART-IX)                          
137900                         WS-ART-KVLS-PAS(ART-IX)                          
138000                         WS-ART-LS-AKT(ART-IX)                            
138100                         WS-ART-LS-PAS(ART-IX)                            
138200                         WS-ART-LS-PR-AKT(ART-IX)                         
138300                         WS-ART-LS-PR-PAS(ART-IX)                         
138400                         WS-ART-KVDISP-AKT(ART-IX)                        
138500                         WS-ART-KVDISP-PAS(ART-IX)                        
138600                         WS-ART-KVDISP-PR-AKT(ART-IX)                     
138700                         WS-ART-KVDISP-PR-PAS(ART-IX)                     
138800                         WS-ART-KVOKS-AKT(ART-IX)                         
138900                         WS-ART-KVOKS-PAS(ART-IX)                         
139000                         WS-ART-OK-PR-AKT(ART-IX)                         
139100                         WS-ART-OK-PR-PAS(ART-IX)                         
139200                         WS-ART-KVAKS-AKT(ART-IX)                         
139300                         WS-ART-KVAKS-PAS(ART-IX)                         
139400                         WS-ART-AK-PR-AKT(ART-IX)                         
139500                         WS-ART-AK-PR-PAS(ART-IX)                         
139600                         WS-ART-KVOI(ART-IX)                              
139700                         WS-ART-KVOI-AKT(ART-IX)                          
139800                         WS-ART-KVOI-PAS(ART-IX)                          
139900                         WS-ART-KVOI-TEO(ART-IX)                          
140000                         WS-ART-KVOI-SAK(ART-IX)                          
140100                         WS-ART-KVOI-CDC-AKT(ART-IX)                      
140200                         WS-ART-KVOI-CDC-PAS(ART-IX)                      
140300                         WS-ART-KVOI-CDC-TEO(ART-IX)                      
140400                         WS-ART-KVOI-CDC-SAK(ART-IX)                      
140500                                                                          
140600        ADD +1 TO ART-IX                                                  
140700     END-PERFORM                                                          
140800                                                                          
140900******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
141000*******                          OBEROENDE AV FREKVENSKLASS               
141100                                                                          
141200     MOVE +1 TO PSUM-IX                                                   
141300     MOVE +9 TO PSUM-IX-MAX                                               
141400     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
141500        MOVE ZERO TO     PSUM-KVANT-AKT(PSUM-IX)                          
141600                         PSUM-KVANT-PAS(PSUM-IX)                          
141700                         PSUM-PROC-KVANT-A(PSUM-IX)                       
141800                         PSUM-PROC-KVANT-P(PSUM-IX)                       
141900                         PSUM-KVDISP-AKT(PSUM-IX)                         
142000                         PSUM-PROC-KVDISP-A(PSUM-IX)                      
142100                         PSUM-KVDISP-PAS(PSUM-IX)                         
142200                         PSUM-PROC-KVDISP-P(PSUM-IX)                      
142300                         PSUM-LS-AKT(PSUM-IX)                             
142400                         PSUM-PROC-LS-A(PSUM-IX)                          
142500                         PSUM-LS-PAS(PSUM-IX)                             
142600                         PSUM-PROC-LS-P(PSUM-IX)                          
142700                         PSUM-AK-AKT(PSUM-IX)                             
142800                         PSUM-PROC-AK-A(PSUM-IX)                          
142900                         PSUM-AK-PAS(PSUM-IX)                             
143000                         PSUM-PROC-AK-P(PSUM-IX)                          
143100                         PSUM-OLAGER(PSUM-IX)                             
143200                         PSUM-PROC-OLAGER(PSUM-IX)                        
143300                         PSUM-SLAGER(PSUM-IX)                             
143400                         PSUM-PROC-SLAGER(PSUM-IX)                        
143500                         PSUM-MLAGER(PSUM-IX)                             
143600                         PSUM-PROC-MLAGER(PSUM-IX)                        
143700                         PSUM-KVOT(PSUM-IX)                               
143800                         PSUM-PROC-KVOT(PSUM-IX)                          
143900                         PSUM-SPLIT(PSUM-IX)                              
144000                         PSUM-OMSHAST-DISP(PSUM-IX)                       
144100                         PSUM-OMSHAST-PROC-D(PSUM-IX)                     
144200                         PSUM-OMSHAST-LS(PSUM-IX)                         
144300                         PSUM-OMSHAST-PROC-LS(PSUM-IX)                    
144400                         PSUM-SERVG-TOT(PSUM-IX)                          
144500                         PSUM-SERVG-AKT(PSUM-IX)                          
144600                         PSUM-SERVG-PAS(PSUM-IX)                          
144700                         PSUM-SERVG-TEO(PSUM-IX)                          
144800                         WS-PSUM-SLAGER(PSUM-IX)                          
144900                         WS-PSUM-OLAGER(PSUM-IX)                          
145000                         WS-PSUM-MLAGER(PSUM-IX)                          
145100                         WS-PSUM-LS-AKT(PSUM-IX)                          
145200                         WS-PSUM-LS-PAS(PSUM-IX)                          
145300                         WS-PSUM-LS-PR-AKT(PSUM-IX)                       
145400                         WS-PSUM-LS-PR-PAS(PSUM-IX)                       
145500                         WS-PSUM-KVDISP-AKT(PSUM-IX)                      
145600                         WS-PSUM-KVDISP-PAS(PSUM-IX)                      
145700                         WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                   
145800                         WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                   
145900                         WS-PSUM-KVOKS-AKT(PSUM-IX)                       
146000                         WS-PSUM-KVOKS-PAS(PSUM-IX)                       
146100                         WS-PSUM-OK-PR-AKT(PSUM-IX)                       
146200                         WS-PSUM-OK-PR-PAS(PSUM-IX)                       
146300                         WS-PSUM-KVAKS-AKT(PSUM-IX)                       
146400                         WS-PSUM-KVAKS-PAS(PSUM-IX)                       
146500                         WS-PSUM-AK-PR-AKT(PSUM-IX)                       
146600                         WS-PSUM-AK-PR-PAS(PSUM-IX)                       
146700                         WS-PSUM-KVOI(PSUM-IX)                            
146800                         WS-PSUM-KVOI-AKT(PSUM-IX)                        
146900                         WS-PSUM-KVOI-PAS(PSUM-IX)                        
147000                         WS-PSUM-KVOI-TEO(PSUM-IX)                        
147100                         WS-PSUM-KVOI-SAK(PSUM-IX)                        
147200                         WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                    
147300                         WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                    
147400                         WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                    
147500                         WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                    
147600                                                                          
147700        ADD +1 TO PSUM-IX                                                 
147800     END-PERFORM                                                          
147900                                                                          
148000******* NOLLSTÄLLNING AV 7 'RUTOR' TOTALSUMMA PER FREKVENSKLASS           
148100*******                            OBEROENDE AV PRISKLASS                 
148200                                                                          
148300     MOVE +1 TO FSUM-IX                                                   
148400     MOVE +8 TO FSUM-IX-MAX                                               
148500     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
148600        MOVE ZERO TO     FSUM-KVANT-AKT(FSUM-IX)                          
148700                         FSUM-KVANT-PAS(FSUM-IX)                          
148800                         FSUM-PROC-KVANT-A(FSUM-IX)                       
148900                         FSUM-PROC-KVANT-P(FSUM-IX)                       
149000                         FSUM-KVDISP-AKT(FSUM-IX)                         
149100                         FSUM-PROC-KVDISP-A(FSUM-IX)                      
149200                         FSUM-KVDISP-PAS(FSUM-IX)                         
149300                         FSUM-PROC-KVDISP-P(FSUM-IX)                      
149400                         FSUM-LS-AKT(FSUM-IX)                             
149500                         FSUM-PROC-LS-A(FSUM-IX)                          
149600                         FSUM-LS-PAS(FSUM-IX)                             
149700                         FSUM-PROC-LS-P(FSUM-IX)                          
149800                         FSUM-AK-AKT(FSUM-IX)                             
149900                         FSUM-PROC-AK-A(FSUM-IX)                          
150000                         FSUM-AK-PAS(FSUM-IX)                             
150100                         FSUM-PROC-AK-P(FSUM-IX)                          
150200                         FSUM-OLAGER(FSUM-IX)                             
150300                         FSUM-PROC-OLAGER(FSUM-IX)                        
150400                         FSUM-SLAGER(FSUM-IX)                             
150500                         FSUM-PROC-SLAGER(FSUM-IX)                        
150600                         FSUM-MLAGER(FSUM-IX)                             
150700                         FSUM-PROC-MLAGER(FSUM-IX)                        
150800                         FSUM-KVOT(FSUM-IX)                               
150900                         FSUM-PROC-KVOT(FSUM-IX)                          
151000                         FSUM-SPLIT(FSUM-IX)                              
151100                         FSUM-OMSHAST-DISP(FSUM-IX)                       
151200                         FSUM-OMSHAST-PROC-D(FSUM-IX)                     
151300                         FSUM-OMSHAST-LS(FSUM-IX)                         
151400                         FSUM-OMSHAST-PROC-LS(FSUM-IX)                    
151500                         FSUM-SERVG-TOT(FSUM-IX)                          
151600                         FSUM-SERVG-AKT(FSUM-IX)                          
151700                         FSUM-SERVG-PAS(FSUM-IX)                          
151800                         FSUM-SERVG-TEO(FSUM-IX)                          
151900                         WS-FSUM-SLAGER(FSUM-IX)                          
152000                         WS-FSUM-OLAGER(FSUM-IX)                          
152100                         WS-FSUM-MLAGER(FSUM-IX)                          
152200                         WS-FSUM-LS-AKT(FSUM-IX)                          
152300                         WS-FSUM-LS-PAS(FSUM-IX)                          
152400                         WS-FSUM-LS-PR-AKT(FSUM-IX)                       
152500                         WS-FSUM-LS-PR-PAS(FSUM-IX)                       
152600                         WS-FSUM-KVDISP-AKT(FSUM-IX)                      
152700                         WS-FSUM-KVDISP-PAS(FSUM-IX)                      
152800                         WS-FSUM-KVDISP-PR-AKT(FSUM-IX)                   
152900                         WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                   
153000                         WS-FSUM-KVOKS-AKT(FSUM-IX)                       
153100                         WS-FSUM-KVOKS-PAS(FSUM-IX)                       
153200                         WS-FSUM-OK-PR-AKT(FSUM-IX)                       
153300                         WS-FSUM-OK-PR-PAS(FSUM-IX)                       
153400                         WS-FSUM-KVAKS-AKT(FSUM-IX)                       
153500                         WS-FSUM-KVAKS-PAS(FSUM-IX)                       
153600                         WS-FSUM-AK-PR-AKT(FSUM-IX)                       
153700                         WS-FSUM-AK-PR-PAS(FSUM-IX)                       
153800                         WS-FSUM-KVOI(FSUM-IX)                            
153900                         WS-FSUM-KVOI-AKT(FSUM-IX)                        
154000                         WS-FSUM-KVOI-PAS(FSUM-IX)                        
154100                         WS-FSUM-KVOI-TEO(FSUM-IX)                        
154200                         WS-FSUM-KVOI-SAK(FSUM-IX)                        
154300                         WS-FSUM-KVOI-CDC-AKT(FSUM-IX)                    
154400                         WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                    
154500                         WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                    
154600                         WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                    
154700                                                                          
154800        ADD +1 TO FSUM-IX                                                 
154900     END-PERFORM                                                          
155000                                                                          
155100******* NOLLSTÄLLNING AV TOTALRUTA                                        
155200                                                                          
155300     MOVE ZERO TO     TOT-KVANT-AKT                                       
155400                      TOT-KVANT-PAS                                       
155500                      TOT-KVDISP-AKT                                      
155600                      TOT-KVDISP-PAS                                      
155700                      TOT-LS-AKT                                          
155800                      TOT-LS-PAS                                          
155900                      TOT-AK-AKT                                          
156000                      TOT-AK-PAS                                          
156100                      TOT-SLAGER                                          
156200                      TOT-MLAGER                                          
156300                      TOT-OLAGER                                          
156400                      TOT-KVOT                                            
156500                      TOT-PROC-OLAGER                                     
156600                      TOT-PROC-MLAGER                                     
156700                      TOT-PROC-SLAGER                                     
156800                      TOT-SPLIT                                           
156900                      TOT-OMSHAST-DISP                                    
157000                      TOT-OMSHAST-LS                                      
157100                      TOT-SERVG-TOT                                       
157200                      TOT-SERVG-AKT                                       
157300                      TOT-SERVG-PAS                                       
157400                      TOT-SERVG-TEO                                       
157500                      WS-TOT-SLAGER                                       
157600                      WS-TOT-OLAGER                                       
157700                      WS-TOT-MLAGER                                       
157800                      WS-TOT-LS-AKT                                       
157900                      WS-TOT-LS-PAS                                       
158000                      WS-TOT-LS-PR-AKT                                    
158100                      WS-TOT-LS-PR-PAS                                    
158200                      WS-TOT-KVDISP-AKT                                   
158300                      WS-TOT-KVDISP-PAS                                   
158400                      WS-TOT-KVDISP-PR-AKT                                
158500                      WS-TOT-KVDISP-PR-PAS                                
158600                      WS-TOT-KVOKS-AKT                                    
158700                      WS-TOT-KVOKS-PAS                                    
158800                      WS-TOT-OK-PR-AKT                                    
158900                      WS-TOT-OK-PR-PAS                                    
159000                      WS-TOT-KVAKS-AKT                                    
159100                      WS-TOT-KVAKS-PAS                                    
159200                      WS-TOT-AK-PR-AKT                                    
159300                      WS-TOT-AK-PR-PAS                                    
159400                      WS-TOT-KVOI                                         
159500                      WS-TOT-KVOI-AKT                                     
159600                      WS-TOT-KVOI-PAS                                     
159700                      WS-TOT-KVOI-TEO                                     
159800                      WS-TOT-KVOI-SAK                                     
159900                      WS-TOT-KVOI-CDC-AKT                                 
160000                      WS-TOT-KVOI-CDC-PAS                                 
160100                      WS-TOT-KVOI-CDC-TEO                                 
160200                      WS-TOT-KVOI-CDC-SAK                                 
160300     .                                                                    
160400     EJECT                                                                
160500 BB-SKAPA-TABELLER SECTION.                                               
160600                                                                          
160610     IF W-IDDC-B6 = SPACE                                                 
160620        MOVE IN-IDDC     TO W-IDDC-B6                                     
160630        MOVE IN-IDDC-REF TO W-IDDC-B616                                   
160640        PERFORM IMS-GU-WDB616                                             
160650     END-IF                                                               
160700     PERFORM BBA-SAETT-ART-IX                                             
160800     IF SW-ARTIKEL-SAKNAS-WDK7 = JA                                       
160900        PERFORM BBC-UPPDAT-SAKN-ART                                       
161000     END-IF                                                               
161100     IF ART-IX > ZERO                                                     
161200        PERFORM BBB-UPPDATERA-TABELLER                                    
161300     END-IF                                                               
161400     .                                                                    
161500     EJECT                                                                
161600 BBA-SAETT-ART-IX SECTION.                                                
161700******************************************************************        
161800* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
161900******************************************************************        
162000                                                                          
162100     MOVE NEJ TO SW-ARTIKEL-SAKNAS-WDK7                                   
162200     EVALUATE TRUE                                                        
162300     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
162400          MOVE +1 TO ART-IX                                               
162500     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
162600          MOVE +2 TO ART-IX                                               
162700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
162800          MOVE +3 TO ART-IX                                               
162900     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
163000          MOVE +4 TO ART-IX                                               
163100     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
163200          MOVE +5 TO ART-IX                                               
163300     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
163400          MOVE +6 TO ART-IX                                               
163500     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
163600          MOVE +7 TO ART-IX                                               
163700     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
163800          MOVE +8 TO ART-IX                                               
163900                                                                          
164000     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
164100          MOVE +9 TO ART-IX                                               
164200     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
164300          MOVE +10 TO ART-IX                                              
164400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
164500          MOVE +11 TO ART-IX                                              
164600     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
164700          MOVE +12 TO ART-IX                                              
164800     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
164900          MOVE +13 TO ART-IX                                              
165000     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
165100          MOVE +14 TO ART-IX                                              
165200     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
165300          MOVE +15 TO ART-IX                                              
165400     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
165500          MOVE +16 TO ART-IX                                              
165600                                                                          
165700     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
165800          MOVE +17 TO ART-IX                                              
165900     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
166000          MOVE +18 TO ART-IX                                              
166100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
166200          MOVE +19 TO ART-IX                                              
166300     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
166400          MOVE +20 TO ART-IX                                              
166500     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
166600          MOVE +21 TO ART-IX                                              
166700     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
166800          MOVE +22 TO ART-IX                                              
166900     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
167000          MOVE +23 TO ART-IX                                              
167100     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
167200          MOVE +24 TO ART-IX                                              
167300                                                                          
167400     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
167500          MOVE +25 TO ART-IX                                              
167600     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
167700          MOVE +26 TO ART-IX                                              
167800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
167900          MOVE +27 TO ART-IX                                              
168000     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
168100          MOVE +28 TO ART-IX                                              
168200     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
168300          MOVE +29 TO ART-IX                                              
168400     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
168500          MOVE +30 TO ART-IX                                              
168600     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
168700          MOVE +31 TO ART-IX                                              
168800     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
168900          MOVE +32 TO ART-IX                                              
169000                                                                          
169100     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
169200          MOVE +33 TO ART-IX                                              
169300     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
169400          MOVE +34 TO ART-IX                                              
169500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
169600          MOVE +35 TO ART-IX                                              
169700     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
169800          MOVE +36 TO ART-IX                                              
169900     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
170000          MOVE +37 TO ART-IX                                              
170100     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
170200          MOVE +38 TO ART-IX                                              
170300     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
170400          MOVE +39 TO ART-IX                                              
170500     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
170600          MOVE +40 TO ART-IX                                              
170700                                                                          
170800     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
170900          MOVE +41 TO ART-IX                                              
171000     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
171100          MOVE +42 TO ART-IX                                              
171200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
171300          MOVE +43 TO ART-IX                                              
171400     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
171500          MOVE +44 TO ART-IX                                              
171600     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
171700          MOVE +45 TO ART-IX                                              
171800     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
171900          MOVE +46 TO ART-IX                                              
172000     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
172100          MOVE +47 TO ART-IX                                              
172200     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
172300          MOVE +48 TO ART-IX                                              
172400                                                                          
172500     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
172600          MOVE +49 TO ART-IX                                              
172700     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
172800          MOVE +50 TO ART-IX                                              
172900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
173000          MOVE +51 TO ART-IX                                              
173100     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
173200          MOVE +52 TO ART-IX                                              
173300     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
173400          MOVE +53 TO ART-IX                                              
173500     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
173600          MOVE +54 TO ART-IX                                              
173700     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
173800          MOVE +55 TO ART-IX                                              
173900     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
174000          MOVE +56 TO ART-IX                                              
174100                                                                          
174200     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
174300          MOVE +57 TO ART-IX                                              
174400     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
174500          MOVE +58 TO ART-IX                                              
174600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
174700          MOVE +59 TO ART-IX                                              
174800     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
174900          MOVE +60 TO ART-IX                                              
175000     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
175100          MOVE +61 TO ART-IX                                              
175200     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
175300          MOVE +62 TO ART-IX                                              
175400     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
175500          MOVE +63 TO ART-IX                                              
175600     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
175700          MOVE +64 TO ART-IX                                              
175800                                                                          
175900     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
176000          MOVE +65 TO ART-IX                                              
176100     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
176200          MOVE +66 TO ART-IX                                              
176300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
176400          MOVE +67 TO ART-IX                                              
176500     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
176600          MOVE +68 TO ART-IX                                              
176700     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
176800          MOVE +69 TO ART-IX                                              
176900     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
177000          MOVE +70 TO ART-IX                                              
177100     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
177200          MOVE +71 TO ART-IX                                              
177300     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
177400          MOVE +72 TO ART-IX                                              
177500     WHEN OTHER                                                           
177600          MOVE ZERO TO ART-IX                                             
177700          IF IN-KDPRISKL = SPACE AND IN-KDFREKKL = SPACE                  
177800             MOVE JA TO SW-ARTIKEL-SAKNAS-WDK7                            
177900          END-IF                                                          
178000     END-EVALUATE                                                         
178100     .                                                                    
178200     EJECT                                                                
178300 BBB-UPPDATERA-TABELLER SECTION.                                          
178400                                                                          
178500*********  ANTAL ARTIKLAR                                                 
178600     IF IN-KDREFSTA = 'A'                                                 
178700        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
178800     ELSE                                                                 
178900        IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                           
179000           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
179100        END-IF                                                            
179200     END-IF                                                               
179300                                                                          
179400*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
179500     IF IN-KDREFSTA = 'A'                                                 
179600        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
179700        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
179800        COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                            
179900        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                        
180000        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
180100                                                                          
180200        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                         
180300        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
180400                                                                          
180500        COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                          
180600        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
180700                                                                          
180800        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
180900        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
181000        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                         
181100        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
181200     ELSE                                                                 
181300        IF IN-KDREFSTA = 'P'                                              
181400           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
181500           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
181600           COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                         
181700           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                     
181800           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
181900                                                                          
182000           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                      
182100           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
182200                                                                          
182300           COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                       
182400           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
182500                                                                          
182600           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
182700           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
182800           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                      
182900           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
183000        END-IF                                                            
183100     END-IF                                                               
183200                                                                          
183300*********  OMSÄTTNINGSHASTIGHET                                           
183400     MOVE +1 TO KVOI-IX                                                   
183500     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
183600     PERFORM UNTIL KVOI-IX > 53                                           
183700        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
183800        ADD +1 TO KVOI-IX                                                 
183900     END-PERFORM                                                          
184000                                                                          
184100     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRARTSTD                      
184200     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
184300                                                                          
184400*********  SÄKERHETSLAGER/ARTIKEL                                         
184500     IF IN-KDREFSTA = 'A'                                                 
184600        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRARTSTD                      
184700        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
184800     END-IF                                                               
184900                                                                          
185000*********  ÖVERLAGER/ARTIKEL                                              
185100     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
185200     IF WS-KVDISP > IN-KVREFOVL                                           
185300        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
185400        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRARTSTD                        
185500        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
185600     END-IF                                                               
185700                                                                          
185800*********  MEDELLAGER/ARTIKEL                                             
185900     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
186000     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
186100     COMPUTE WS-LT-BEHOV-SDC-NORM = REF-KVDLTID-TOT                       
186200                                  * WS-KVPB-DAG-SDC-NORM                  
186300     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
186400                        + (IN-KVREFBER / 2)                               
186500     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRARTSTD                           
186600     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
186700                                                                          
186800*********  SERVICEGRAD OCH SPLITFAKTOR ORDERRADER/ARTIKEL                 
186900                                                                          
187000     MOVE +1 TO KVOI-IX                                                   
187100     MOVE NEJ TO SW-KVOI-TRAFF                                            
187200     PERFORM UNTIL KVOI-IX > 5                                            
187300        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
187400           MOVE JA TO SW-KVOI-TRAFF                                       
187500           IF IN-KDREFSTA = 'A'                                           
187600              ADD IN-KVOT-INNEV(KVOI-IX)                                  
187700                                TO WS-ART-KVOI-AKT(ART-IX)                
187800              ADD IN-KVOT-CDC-INNEV(KVOI-IX)                              
187900                                TO WS-ART-KVOI-CDC-AKT(ART-IX)            
188000           ELSE                                                           
188100              IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                     
188200                 ADD IN-KVOT-INNEV(KVOI-IX)                               
188300                                TO WS-ART-KVOI-TEO(ART-IX)                
188400                 ADD IN-KVOT-CDC-INNEV(KVOI-IX)                           
188500                                TO WS-ART-KVOI-CDC-TEO(ART-IX)            
188600              ELSE                                                        
188700                 IF IN-KDREFSTA = 'P'                                     
188800                    ADD IN-KVOT-INNEV(KVOI-IX)                            
188900                                   TO WS-ART-KVOI-PAS(ART-IX)             
189000                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
189100                                   TO WS-ART-KVOI-CDC-PAS(ART-IX)         
189200                 ELSE                                                     
189300                    ADD IN-KVOT-INNEV(KVOI-IX)                            
189400                                   TO WS-ART-KVOI-SAK(ART-IX)             
189500                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
189600                                   TO WS-ART-KVOI-CDC-SAK(ART-IX)         
189700                 END-IF                                                   
189800              END-IF                                                      
189900           END-IF                                                         
190000        END-IF                                                            
190100        ADD +1 TO KVOI-IX                                                 
190200     END-PERFORM                                                          
190300                                                                          
190400     IF SW-KVOI-TRAFF = NEJ                                               
190500        MOVE D-VECKA-VECKA TO KVOI-IX                                     
190600        IF IN-KDREFSTA = 'A'                                              
190700           ADD IN-KVOT-RULL(KVOI-IX)                                      
190800                              TO WS-ART-KVOI-AKT(ART-IX)                  
190900           ADD IN-KVOT-CDC-RULL(KVOI-IX)                                  
191000                              TO WS-ART-KVOI-CDC-AKT(ART-IX)              
191100        ELSE                                                              
191200           IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                        
191300              ADD IN-KVOT-RULL(KVOI-IX)                                   
191400                              TO WS-ART-KVOI-TEO(ART-IX)                  
191500              ADD IN-KVOT-CDC-RULL(KVOI-IX)                               
191600                              TO WS-ART-KVOI-CDC-TEO(ART-IX)              
191700           ELSE                                                           
191800              IF IN-KDREFSTA = 'P'                                        
191900                 ADD IN-KVOT-RULL(KVOI-IX)                                
192000                                 TO WS-ART-KVOI-PAS(ART-IX)               
192100                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
192200                                 TO WS-ART-KVOI-CDC-PAS(ART-IX)           
192300              ELSE                                                        
192400                 ADD IN-KVOT-RULL(KVOI-IX)                                
192500                                 TO WS-ART-KVOI-SAK(ART-IX)               
192600                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
192700                                 TO WS-ART-KVOI-CDC-SAK(ART-IX)           
192800              END-IF                                                      
192900           END-IF                                                         
193000        END-IF                                                            
193100     END-IF                                                               
193200     .                                                                    
193300     EJECT                                                                
193400 BBC-UPPDAT-SAKN-ART SECTION.                                             
193500                                                                          
193600     MOVE +1 TO KVOI-IX                                                   
193700     MOVE NEJ TO SW-KVOI-TRAFF                                            
193800     PERFORM UNTIL KVOI-IX > 5                                            
193900        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
194000           MOVE JA TO SW-KVOI-TRAFF                                       
194100           ADD IN-KVOT-INNEV(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7              
194200           ADD IN-KVOT-CDC-INNEV(KVOI-IX)                                 
194300                                    TO WS-KVOT-CDC-SAKNAS-WDK7            
194400        END-IF                                                            
194500        ADD +1 TO KVOI-IX                                                 
194600     END-PERFORM                                                          
194700                                                                          
194800     IF SW-KVOI-TRAFF = NEJ                                               
194900        MOVE D-VECKA-VECKA TO KVOI-IX                                     
195000        ADD IN-KVOT-RULL(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7                  
195100        ADD IN-KVOT-CDC-RULL(KVOI-IX) TO WS-KVOT-CDC-SAKNAS-WDK7          
195200     END-IF                                                               
195300     .                                                                    
195400     EJECT                                                                
195500 BC-SUMMERA SECTION.                                                      
195600                                                                          
195700     PERFORM BCA-SUMMERA-RUTA                                             
195800     PERFORM BCB-SUMMERA-PRISKLASS                                        
195900     PERFORM BCC-SUMMERA-FREKVENSKLASS                                    
196000     PERFORM BCD-SUMMERA-TOTAL                                            
196100     PERFORM BCE-BERAKNINGAR-AV-TOTAL                                     
196200     .                                                                    
196300     EJECT                                                                
196400 BCA-SUMMERA-RUTA SECTION.                                                
196500                                                                          
196600     MOVE +1  TO ART-IX                                                   
196700     MOVE +72 TO ART-IX-MAX                                               
196800     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
196900                                                                          
197000*********  DISP LAGER VÄRDE/RUTA                                          
197100        IF WS-ART-KVDISP-PR-AKT(ART-IX) = ZERO                            
197200           CONTINUE                                                       
197300        ELSE                                                              
197400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
197500                          WS-ART-KVDISP-PR-AKT(ART-IX) / 1000             
197600           MOVE WS-SUMMA-KR TO ART-KVDISP-AKT(ART-IX)                     
197700        END-IF                                                            
197800                                                                          
197900        IF WS-ART-KVDISP-PR-PAS(ART-IX) = ZERO                            
198000           CONTINUE                                                       
198100        ELSE                                                              
198200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
198300                          WS-ART-KVDISP-PR-PAS(ART-IX) / 1000             
198400           MOVE WS-SUMMA-KR TO ART-KVDISP-PAS(ART-IX)                     
198500        END-IF                                                            
198600                                                                          
198700*********  LAGERVÄRDE/RUTA                                                
198800        IF WS-ART-LS-PR-AKT(ART-IX) = ZERO                                
198900           CONTINUE                                                       
199000        ELSE                                                              
199100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
199200                          WS-ART-LS-PR-AKT(ART-IX) / 1000                 
199300           MOVE WS-SUMMA-KR TO ART-LS-AKT(ART-IX)                         
199400        END-IF                                                            
199500                                                                          
199600        IF WS-ART-LS-PR-PAS(ART-IX) = ZERO                                
199700           CONTINUE                                                       
199800        ELSE                                                              
199900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
200000                          WS-ART-LS-PR-PAS(ART-IX) / 1000                 
200100           MOVE WS-SUMMA-KR TO ART-LS-PAS(ART-IX)                         
200200        END-IF                                                            
200300                                                                          
200400*********  AK-VÄRDE/RUTA                                                  
200500        IF WS-ART-AK-PR-AKT(ART-IX) = ZERO                                
200600           CONTINUE                                                       
200700        ELSE                                                              
200800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
200900                          WS-ART-AK-PR-AKT(ART-IX) / 1000                 
201000           MOVE WS-SUMMA-KR TO ART-AK-AKT(ART-IX)                         
201100        END-IF                                                            
201200                                                                          
201300        IF WS-ART-AK-PR-PAS(ART-IX) = ZERO                                
201400           CONTINUE                                                       
201500        ELSE                                                              
201600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
201700                          WS-ART-AK-PR-PAS(ART-IX) / 1000                 
201800           MOVE WS-SUMMA-KR TO ART-AK-PAS(ART-IX)                         
201900        END-IF                                                            
202000                                                                          
202100*********  SÄKERHETSLAGER/RUTA                                            
202200        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
202300           CONTINUE                                                       
202400        ELSE                                                              
202500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
202600                          WS-ART-SLAGER(ART-IX) / 1000                    
202700           MOVE WS-SUMMA-KR TO ART-SLAGER(ART-IX)                         
202800        END-IF                                                            
202900                                                                          
203000*********  ÖVERLAGER/RUTA                                                 
203100        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
203200           CONTINUE                                                       
203300        ELSE                                                              
203400           COMPUTE WS-SUMMA-KR ROUNDED                                    
203500                              = WS-ART-OLAGER(ART-IX) / 1000              
203600           MOVE WS-SUMMA-KR TO ART-OLAGER(ART-IX)                         
203700        END-IF                                                            
203800                                                                          
203900*********  MEDELLAGER/RUTA                                                
204000        IF WS-ART-MLAGER(ART-IX) = ZERO                                   
204100           CONTINUE                                                       
204200        ELSE                                                              
204300           COMPUTE WS-SUMMA-KR =                                          
204400                                WS-ART-MLAGER(ART-IX) / 1000              
204500           ADD WS-SUMMA-KR TO ART-MLAGER(ART-IX)                          
204600        END-IF                                                            
204700                                                                          
204800*********  OMSHASTIGHET/RUTA                                              
204900        COMPUTE WS-SUMMA = WS-ART-KVDISP-PR-AKT(ART-IX) +                 
205000                           WS-ART-KVDISP-PR-PAS(ART-IX)                   
205100        IF WS-SUMMA = ZERO                                                
205200           CONTINUE                                                       
205300        ELSE                                                              
205400           COMPUTE WS-OMSHAST ROUNDED =                                   
205500               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
205600           MOVE WS-OMSHAST TO ART-OMSHAST-DISP(ART-IX)                    
205700        END-IF                                                            
205800                                                                          
205900        COMPUTE WS-SUMMA = WS-ART-LS-PR-AKT(ART-IX) +                     
206000                           WS-ART-AK-PR-AKT(ART-IX) +                     
206100                           WS-ART-LS-PR-PAS(ART-IX) +                     
206200                           WS-ART-AK-PR-PAS(ART-IX)                       
206300        IF WS-SUMMA = ZERO                                                
206400           CONTINUE                                                       
206500        ELSE                                                              
206600           COMPUTE WS-OMSHAST ROUNDED =                                   
206700               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
206800           MOVE WS-OMSHAST TO ART-OMSHAST-LS(ART-IX)                      
206900        END-IF                                                            
207000                                                                          
207100*********  TOTAL SERVICEGRAD/RUTA                                         
207200        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
207300                                    WS-ART-KVOI-PAS(ART-IX) +             
207400                                    WS-ART-KVOI-TEO(ART-IX) +             
207500                                    WS-ART-KVOI-SAK(ART-IX)               
207600        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
207700                        =   WS-ART-KVOI-CDC-AKT(ART-IX) +                 
207800                            WS-ART-KVOI-CDC-PAS(ART-IX) +                 
207900                            WS-ART-KVOI-CDC-TEO(ART-IX) +                 
208000                            WS-ART-KVOI-CDC-SAK(ART-IX)                   
208100        IF WS-KVOI-TOT-VECKA = ZERO                                       
208200           MOVE 99.9   TO ART-SERVG-TOT(ART-IX)                           
208300        ELSE                                                              
208400           COMPUTE WS-SERVG ROUNDED =                                     
208500             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
208600                              WS-KVOI-TOT-VECKA                           
208700           IF WS-SERVG = 100.0                                            
208800              MOVE 99.9 TO ART-SERVG-TOT(ART-IX)                          
208900           ELSE                                                           
209000              MOVE WS-SERVG TO ART-SERVG-TOT(ART-IX)                      
209100           END-IF                                                         
209200        END-IF                                                            
209300                                                                          
209400*********  SERVICEGRAD/RUTA AKTIVA ARTIKLAR                               
209500        IF WS-ART-KVOI-AKT(ART-IX) = ZERO                                 
209600           MOVE 99.9   TO ART-SERVG-AKT(ART-IX)                           
209700        ELSE                                                              
209800           COMPUTE WS-SERVG ROUNDED = (WS-ART-KVOI-AKT(ART-IX) -          
209900                         WS-ART-KVOI-CDC-AKT(ART-IX)) * 100 /             
210000                         WS-ART-KVOI-AKT(ART-IX)                          
210100           IF WS-SERVG = 100.0                                            
210200              MOVE 99.9 TO ART-SERVG-AKT(ART-IX)                          
210300           ELSE                                                           
210400              MOVE WS-SERVG TO ART-SERVG-AKT(ART-IX)                      
210500           END-IF                                                         
210600        END-IF                                                            
210700                                                                          
210800*********  SERVICEGRAD/RUTA PASSIVA ARTIKLAR                              
210900        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-PAS(ART-IX) +             
211000                                    WS-ART-KVOI-TEO(ART-IX)               
211100        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
211200                        =   WS-ART-KVOI-CDC-PAS(ART-IX) +                 
211300                            WS-ART-KVOI-CDC-TEO(ART-IX)                   
211400        IF WS-KVOI-TOT-VECKA = ZERO                                       
211500           MOVE 99.9 TO ART-SERVG-PAS(ART-IX)                             
211600        ELSE                                                              
211700           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
211800                         WS-KVOI-TOT-CDC-VECKA) * 100                     
211900                        / WS-KVOI-TOT-VECKA                               
212000           IF WS-SERVG = 100.0                                            
212100              MOVE 99.9 TO ART-SERVG-PAS(ART-IX)                          
212200           ELSE                                                           
212300              MOVE WS-SERVG TO ART-SERVG-PAS(ART-IX)                      
212400           END-IF                                                         
212500        END-IF                                                            
212600                                                                          
212700*********  TEORETISK SERVICEGRAD/RUTA                                     
212800        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
212900                                    WS-ART-KVOI-TEO(ART-IX)               
213000        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
213100                        =   WS-ART-KVOI-CDC-AKT(ART-IX) +                 
213200                            WS-ART-KVOI-CDC-TEO(ART-IX)                   
213300        IF WS-KVOI-TOT-VECKA = ZERO                                       
213400           MOVE 99.9   TO ART-SERVG-TEO(ART-IX)                           
213500        ELSE                                                              
213600           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
213700                         WS-KVOI-TOT-CDC-VECKA) * 100                     
213800                        / WS-KVOI-TOT-VECKA                               
213900           IF WS-SERVG = 100.0                                            
214000              MOVE 99.9 TO ART-SERVG-TEO(ART-IX)                          
214100           ELSE                                                           
214200              MOVE WS-SERVG TO ART-SERVG-TEO(ART-IX)                      
214300           END-IF                                                         
214400        END-IF                                                            
214500                                                                          
214600*********  SPLITFAKTOR/RUTA                                               
214700        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
214800                                    WS-ART-KVOI-PAS(ART-IX) +             
214900                                    WS-ART-KVOI-TEO(ART-IX) +             
215000                                    WS-ART-KVOI-SAK(ART-IX)               
215100        COMPUTE WS-KVOI-TOT-CDC-VECKA =                                   
215200                            WS-ART-KVOI-CDC-SAK(ART-IX) +                 
215300                            WS-ART-KVOI-CDC-PAS(ART-IX)                   
215400        IF WS-KVOI-TOT-VECKA = ZERO                                       
215500           MOVE 99.9 TO ART-SPLIT(ART-IX)                                 
215600        ELSE                                                              
215700           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
215800                         WS-KVOI-TOT-CDC-VECKA) * 100                     
215900                        / WS-KVOI-TOT-VECKA                               
216000           IF WS-SERVG = 100.0                                            
216100              MOVE 99.9 TO ART-SPLIT(ART-IX)                              
216200           ELSE                                                           
216300              MOVE WS-SERVG TO ART-SPLIT(ART-IX)                          
216400           END-IF                                                         
216500        END-IF                                                            
216600*********  ORDERTRÄFFAR/RUTA                                              
216700        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
216800                                    WS-ART-KVOI-PAS(ART-IX) +             
216900                                    WS-ART-KVOI-TEO(ART-IX) +             
217000                                    WS-ART-KVOI-SAK(ART-IX)               
217100        ADD WS-KVOI-TOT-VECKA TO ART-KVOT(ART-IX)                         
217200                                                                          
217300        ADD +1  TO ART-IX                                                 
217400     END-PERFORM                                                          
217500     .                                                                    
217600     EJECT                                                                
217700 BCB-SUMMERA-PRISKLASS SECTION.                                           
217800******************************************************************        
217900* SUMMERING PER PRISKLASS                                        *        
218000******************************************************************        
218100                                                                          
218200     MOVE +1 TO PSUM-IX                                                   
218300                ART-IX                                                    
218400     MOVE +8 TO ART-IX-MAX                                                
218500                                                                          
218600     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
218700        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
218800                                                                          
218900          ADD ART-KVANT-AKT(ART-IX) TO PSUM-KVANT-AKT(PSUM-IX)            
219000          ADD ART-KVANT-PAS(ART-IX) TO PSUM-KVANT-PAS(PSUM-IX)            
219100          ADD ART-KVOT(ART-IX)      TO PSUM-KVOT(PSUM-IX)                 
219200          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-PSUM-KVOI-AKT(PSUM-IX)        
219300          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-PSUM-KVOI-PAS(PSUM-IX)        
219400          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-PSUM-KVOI-TEO(PSUM-IX)        
219500          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-PSUM-KVOI-SAK(PSUM-IX)        
219600          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
219700                                  TO WS-PSUM-KVOI-CDC-AKT(PSUM-IX)        
219800          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
219900                                  TO WS-PSUM-KVOI-CDC-PAS(PSUM-IX)        
220000          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
220100                                  TO WS-PSUM-KVOI-CDC-TEO(PSUM-IX)        
220200          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
220300                                  TO WS-PSUM-KVOI-CDC-SAK(PSUM-IX)        
220400          ADD WS-ART-KVOI(ART-IX) TO WS-PSUM-KVOI(PSUM-IX)                
220500          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
220600                             TO WS-PSUM-KVDISP-PR-AKT (PSUM-IX)           
220700          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
220800                             TO WS-PSUM-OK-PR-AKT(PSUM-IX)                
220900          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
221000                             TO WS-PSUM-LS-PR-AKT(PSUM-IX)                
221100          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
221200                             TO WS-PSUM-AK-PR-AKT(PSUM-IX)                
221300          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
221400                             TO WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
221500          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
221600                             TO WS-PSUM-OK-PR-PAS(PSUM-IX)                
221700          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
221800                             TO WS-PSUM-LS-PR-PAS(PSUM-IX)                
221900          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
222000                             TO WS-PSUM-AK-PR-PAS(PSUM-IX)                
222100          ADD WS-ART-OLAGER(ART-IX) TO WS-PSUM-OLAGER(PSUM-IX)            
222200          ADD WS-ART-MLAGER(ART-IX) TO WS-PSUM-MLAGER(PSUM-IX)            
222300          ADD WS-ART-SLAGER(ART-IX) TO WS-PSUM-SLAGER(PSUM-IX)            
222400                                                                          
222500          ADD +1 TO ART-IX                                                
222600        END-PERFORM                                                       
222700                                                                          
222800        ADD +1 TO PSUM-IX                                                 
222900        ADD +8 TO ART-IX-MAX                                              
223000     END-PERFORM                                                          
223100                                                                          
223200     MOVE +1 TO PSUM-IX                                                   
223300     MOVE +9 TO PSUM-IX-MAX                                               
223400     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
223500                                                                          
223600*********  DISP LAGER VÄRDE/PRISKLASS                                     
223700        IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) = ZERO                          
223800           CONTINUE                                                       
223900        ELSE                                                              
224000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
224100                     WS-PSUM-KVDISP-PR-AKT(PSUM-IX) / 1000                
224200           MOVE WS-SUMMA-KR TO PSUM-KVDISP-AKT(PSUM-IX)                   
224300        END-IF                                                            
224400                                                                          
224500        IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) = ZERO                          
224600           CONTINUE                                                       
224700        ELSE                                                              
224800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
224900                     WS-PSUM-KVDISP-PR-PAS(PSUM-IX) / 1000                
225000           MOVE WS-SUMMA-KR TO PSUM-KVDISP-PAS(PSUM-IX)                   
225100        END-IF                                                            
225200                                                                          
225300*********  LAGERVÄRDE/PRISKLASS                                           
225400        IF WS-PSUM-LS-PR-AKT(PSUM-IX) = ZERO                              
225500           CONTINUE                                                       
225600        ELSE                                                              
225700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
225800                     WS-PSUM-LS-PR-AKT(PSUM-IX) / 1000                    
225900           MOVE WS-SUMMA-KR TO PSUM-LS-AKT(PSUM-IX)                       
226000        END-IF                                                            
226100                                                                          
226200        IF WS-PSUM-LS-PR-PAS(PSUM-IX) = ZERO                              
226300           CONTINUE                                                       
226400        ELSE                                                              
226500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
226600                     WS-PSUM-LS-PR-PAS(PSUM-IX) / 1000                    
226700           MOVE WS-SUMMA-KR TO PSUM-LS-PAS(PSUM-IX)                       
226800        END-IF                                                            
226900                                                                          
227000*********  AK-VÄRDE/PRISKLASS                                             
227100        IF WS-PSUM-AK-PR-AKT(PSUM-IX) = ZERO                              
227200           CONTINUE                                                       
227300        ELSE                                                              
227400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
227500                     WS-PSUM-AK-PR-AKT(PSUM-IX) / 1000                    
227600           MOVE WS-SUMMA-KR TO PSUM-AK-AKT(PSUM-IX)                       
227700        END-IF                                                            
227800                                                                          
227900        IF WS-PSUM-AK-PR-PAS(PSUM-IX) = ZERO                              
228000           CONTINUE                                                       
228100        ELSE                                                              
228200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
228300                     WS-PSUM-AK-PR-PAS(PSUM-IX) / 1000                    
228400           MOVE WS-SUMMA-KR TO PSUM-AK-PAS(PSUM-IX)                       
228500        END-IF                                                            
228600                                                                          
228700*********  SÄKERHETSLAGER/PRISKLASS                                       
228800        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
228900           CONTINUE                                                       
229000        ELSE                                                              
229100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
229200                          WS-PSUM-SLAGER(PSUM-IX) / 1000                  
229300           MOVE WS-SUMMA-KR TO PSUM-SLAGER(PSUM-IX)                       
229400        END-IF                                                            
229500                                                                          
229600*********  ÖVERLAGER/PRISKLASS                                            
229700        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
229800           CONTINUE                                                       
229900        ELSE                                                              
230000           COMPUTE WS-SUMMA-KR ROUNDED                                    
230100                     = WS-PSUM-OLAGER(PSUM-IX) / 1000                     
230200           MOVE WS-SUMMA-KR TO PSUM-OLAGER(PSUM-IX)                       
230300        END-IF                                                            
230400                                                                          
230500*********  MEDELLAGER/PRISKLASS                                           
230600        IF WS-PSUM-MLAGER(PSUM-IX) = ZERO                                 
230700           CONTINUE                                                       
230800        ELSE                                                              
230900           COMPUTE WS-SUMMA-KR =                                          
231000                       WS-PSUM-MLAGER(PSUM-IX) / 1000                     
231100           ADD WS-SUMMA-KR TO PSUM-MLAGER(PSUM-IX)                        
231200        END-IF                                                            
231300                                                                          
231400*********  TOTAL SERVICEGRAD/PRISKLASS                                    
231500        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-AKT(PSUM-IX) +           
231600                                    WS-PSUM-KVOI-PAS(PSUM-IX) +           
231700                                    WS-PSUM-KVOI-TEO(PSUM-IX) +           
231800                                    WS-PSUM-KVOI-SAK(PSUM-IX)             
231900        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
232000                       =   WS-PSUM-KVOI-CDC-AKT(PSUM-IX) +                
232100                           WS-PSUM-KVOI-CDC-PAS(PSUM-IX) +                
232200                           WS-PSUM-KVOI-CDC-TEO(PSUM-IX) +                
232300                           WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                  
232400                                                                          
232500        IF WS-KVOI-TOT-VECKA = ZERO                                       
232600           MOVE 99.9 TO PSUM-SERVG-TOT(PSUM-IX)                           
232700        ELSE                                                              
232800           COMPUTE WS-SERVG ROUNDED =                                     
232900             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
233000                              WS-KVOI-TOT-VECKA                           
233100           IF WS-SERVG = 100.0                                            
233200              MOVE 99.9 TO PSUM-SERVG-TOT(PSUM-IX)                        
233300           ELSE                                                           
233400              MOVE WS-SERVG TO PSUM-SERVG-TOT(PSUM-IX)                    
233500           END-IF                                                         
233600        END-IF                                                            
233700                                                                          
233800*********  SERVICEGRAD/PRISKLASS  AKTIVA ARTIKLAR                         
233900        IF WS-PSUM-KVOI-AKT(PSUM-IX) = ZERO                               
234000           MOVE 99.9 TO PSUM-SERVG-AKT(PSUM-IX)                           
234100        ELSE                                                              
234200           COMPUTE WS-SERVG ROUNDED = (WS-PSUM-KVOI-AKT(PSUM-IX)          
234300                  - WS-PSUM-KVOI-CDC-AKT(PSUM-IX)) * 100                  
234400                        / WS-PSUM-KVOI-AKT(PSUM-IX)                       
234500           IF WS-SERVG = 100.0                                            
234600              MOVE 99.9 TO PSUM-SERVG-AKT(PSUM-IX)                        
234700           ELSE                                                           
234800              MOVE WS-SERVG TO PSUM-SERVG-AKT(PSUM-IX)                    
234900           END-IF                                                         
235000        END-IF                                                            
235100                                                                          
235200*********  SERVICEGRAD/RUTA PASSIVA ARTIKLAR                              
235300        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-PAS(PSUM-IX) +           
235400                                    WS-PSUM-KVOI-TEO(PSUM-IX)             
235500        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
235600                        =   WS-PSUM-KVOI-CDC-PAS(PSUM-IX) +               
235700                            WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                 
235800        IF WS-KVOI-TOT-VECKA = ZERO                                       
235900           MOVE 99.9 TO PSUM-SERVG-PAS(PSUM-IX)                           
236000        ELSE                                                              
236100           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
236200                         WS-KVOI-TOT-CDC-VECKA) * 100                     
236300                        / WS-KVOI-TOT-VECKA                               
236400           IF WS-SERVG = 100.0                                            
236500              MOVE 99.9 TO PSUM-SERVG-PAS(PSUM-IX)                        
236600           ELSE                                                           
236700              MOVE WS-SERVG TO PSUM-SERVG-PAS(PSUM-IX)                    
236800           END-IF                                                         
236900        END-IF                                                            
237000                                                                          
237100*********  TEORETISK SERVICEGRAD/RUTA                                     
237200        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-AKT(PSUM-IX) +           
237300                                    WS-PSUM-KVOI-TEO(PSUM-IX)             
237400        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
237500                        =   WS-PSUM-KVOI-CDC-AKT(PSUM-IX) +               
237600                            WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                 
237700        IF WS-KVOI-TOT-VECKA = ZERO                                       
237800           MOVE 99.9 TO PSUM-SERVG-TEO(PSUM-IX)                           
237900        ELSE                                                              
238000           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
238100                         WS-KVOI-TOT-CDC-VECKA) * 100                     
238200                        / WS-KVOI-TOT-VECKA                               
238300           IF WS-SERVG = 100.0                                            
238400              MOVE 99.9 TO PSUM-SERVG-TEO(PSUM-IX)                        
238500           ELSE                                                           
238600              MOVE WS-SERVG TO PSUM-SERVG-TEO(PSUM-IX)                    
238700           END-IF                                                         
238800        END-IF                                                            
238900                                                                          
239000*********  SPLITFAKTOR/PRISKLASS                                          
239100        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-AKT(PSUM-IX) +           
239200                                    WS-PSUM-KVOI-PAS(PSUM-IX) +           
239300                                    WS-PSUM-KVOI-TEO(PSUM-IX) +           
239400                                    WS-PSUM-KVOI-SAK(PSUM-IX)             
239500        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
239600                        =   WS-PSUM-KVOI-CDC-SAK(PSUM-IX) +               
239700                            WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                 
239800        IF WS-KVOI-TOT-VECKA = ZERO                                       
239900           MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                               
240000        ELSE                                                              
240100           COMPUTE WS-SERVG ROUNDED =                                     
240200             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
240300                              WS-KVOI-TOT-VECKA                           
240400           IF WS-SERVG = 100.0                                            
240500              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
240600           ELSE                                                           
240700              MOVE WS-SERVG TO PSUM-SPLIT(PSUM-IX)                        
240800           END-IF                                                         
240900        END-IF                                                            
241000                                                                          
241100*********  OMSHASTIGHET/PRISKLASS                                         
241200        COMPUTE WS-SUMMA = WS-PSUM-KVDISP-PR-AKT(PSUM-IX) +               
241300                           WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                 
241400        IF WS-SUMMA = ZERO                                                
241500           CONTINUE                                                       
241600        ELSE                                                              
241700           COMPUTE WS-OMSHAST ROUNDED =                                   
241800               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
241900           MOVE WS-OMSHAST TO PSUM-OMSHAST-DISP(PSUM-IX)                  
242000        END-IF                                                            
242100                                                                          
242200        COMPUTE WS-SUMMA = WS-PSUM-LS-PR-AKT(PSUM-IX) +                   
242300                           WS-PSUM-AK-PR-AKT(PSUM-IX) +                   
242400                           WS-PSUM-LS-PR-PAS(PSUM-IX) +                   
242500                           WS-PSUM-AK-PR-PAS(PSUM-IX)                     
242600        IF WS-SUMMA = ZERO                                                
242700           CONTINUE                                                       
242800        ELSE                                                              
242900           COMPUTE WS-OMSHAST ROUNDED =                                   
243000               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
243100           MOVE WS-OMSHAST TO PSUM-OMSHAST-LS(PSUM-IX)                    
243200        END-IF                                                            
243300                                                                          
243400        ADD +1 TO PSUM-IX                                                 
243500     END-PERFORM                                                          
243600     .                                                                    
243700     EJECT                                                                
243800 BCC-SUMMERA-FREKVENSKLASS SECTION.                                       
243900******************************************************************        
244000* SUMMERING PER FREKVENSKLASS                                    *        
244100******************************************************************        
244200                                                                          
244300     MOVE +1 TO FSUM-IX                                                   
244400                ART-IX                                                    
244500     MOVE +65 TO ART-IX-MAX                                               
244600     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
244700                                                                          
244800        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
244900          ADD ART-KVANT-AKT(ART-IX) TO FSUM-KVANT-AKT(FSUM-IX)            
245000          ADD ART-KVANT-PAS(ART-IX) TO FSUM-KVANT-PAS(FSUM-IX)            
245100          ADD ART-KVOT(ART-IX)      TO FSUM-KVOT(FSUM-IX)                 
245200          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-FSUM-KVOI-AKT(FSUM-IX)        
245300          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-FSUM-KVOI-PAS(FSUM-IX)        
245400          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-FSUM-KVOI-TEO(FSUM-IX)        
245500          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-FSUM-KVOI-SAK(FSUM-IX)        
245600          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
245700                                  TO WS-FSUM-KVOI-CDC-AKT(FSUM-IX)        
245800          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
245900                                  TO WS-FSUM-KVOI-CDC-PAS(FSUM-IX)        
246000          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
246100                                  TO WS-FSUM-KVOI-CDC-TEO(FSUM-IX)        
246200          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
246300                                  TO WS-FSUM-KVOI-CDC-SAK(FSUM-IX)        
246400          ADD WS-ART-KVOI(ART-IX) TO WS-FSUM-KVOI(FSUM-IX)                
246500          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
246600                             TO WS-FSUM-KVDISP-PR-AKT (FSUM-IX)           
246700          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
246800                             TO WS-FSUM-OK-PR-AKT(FSUM-IX)                
246900          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
247000                             TO WS-FSUM-LS-PR-AKT(FSUM-IX)                
247100          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
247200                             TO WS-FSUM-AK-PR-AKT(FSUM-IX)                
247300          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
247400                             TO WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
247500          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
247600                             TO WS-FSUM-OK-PR-PAS(FSUM-IX)                
247700          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
247800                             TO WS-FSUM-LS-PR-PAS(FSUM-IX)                
247900          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
248000                             TO WS-FSUM-AK-PR-PAS(FSUM-IX)                
248100          ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)            
248200          ADD WS-ART-MLAGER(ART-IX) TO WS-FSUM-MLAGER(FSUM-IX)            
248300          ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)            
248400                                                                          
248500          ADD +8 TO ART-IX                                                
248600        END-PERFORM                                                       
248700        ADD +1 TO FSUM-IX                                                 
248800                                                                          
248900        EVALUATE TRUE                                                     
249000           WHEN  FSUM-IX = 1                                              
249100                 MOVE +1 TO ART-IX                                        
249200           WHEN  FSUM-IX = 2                                              
249300                 MOVE +2 TO ART-IX                                        
249400                 MOVE +66 TO ART-IX-MAX                                   
249500           WHEN  FSUM-IX = 3                                              
249600                 MOVE +3 TO ART-IX                                        
249700                 MOVE +67 TO ART-IX-MAX                                   
249800           WHEN  FSUM-IX = 4                                              
249900                 MOVE +4 TO ART-IX                                        
250000                 MOVE +68 TO ART-IX-MAX                                   
250100           WHEN  FSUM-IX = 5                                              
250200                 MOVE +5 TO ART-IX                                        
250300                 MOVE +69 TO ART-IX-MAX                                   
250400           WHEN  FSUM-IX = 6                                              
250500                 MOVE +6 TO ART-IX                                        
250600                 MOVE +70 TO ART-IX-MAX                                   
250700           WHEN  FSUM-IX = 7                                              
250800                 MOVE +7 TO ART-IX                                        
250900                 MOVE +71 TO ART-IX-MAX                                   
251000           WHEN  FSUM-IX = 8                                              
251100                 MOVE +8 TO ART-IX                                        
251200                 MOVE +72 TO ART-IX-MAX                                   
251300           WHEN OTHER                                                     
251400                CONTINUE                                                  
251500        END-EVALUATE                                                      
251600                                                                          
251700     END-PERFORM                                                          
251800                                                                          
251900     MOVE +1 TO FSUM-IX                                                   
252000     MOVE +8 TO FSUM-IX-MAX                                               
252100     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
252200                                                                          
252300*********  DISP LAGER VÄRDE/FREKVENSKLASS                                 
252400        IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) = ZERO                          
252500           CONTINUE                                                       
252600        ELSE                                                              
252700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
252800                     WS-FSUM-KVDISP-PR-AKT(FSUM-IX) / 1000                
252900           MOVE WS-SUMMA-KR TO FSUM-KVDISP-AKT(FSUM-IX)                   
253000        END-IF                                                            
253100                                                                          
253200        IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) = ZERO                          
253300           CONTINUE                                                       
253400        ELSE                                                              
253500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
253600                     WS-FSUM-KVDISP-PR-PAS(FSUM-IX) / 1000                
253700           MOVE WS-SUMMA-KR TO FSUM-KVDISP-PAS(FSUM-IX)                   
253800        END-IF                                                            
253900                                                                          
254000*********  LAGERVÄRDE/FREKVENSKLASS                                       
254100        IF WS-FSUM-LS-PR-AKT(FSUM-IX) = ZERO                              
254200           CONTINUE                                                       
254300        ELSE                                                              
254400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
254500                     WS-FSUM-LS-PR-AKT(FSUM-IX) / 1000                    
254600           MOVE WS-SUMMA-KR TO FSUM-LS-AKT(FSUM-IX)                       
254700        END-IF                                                            
254800                                                                          
254900        IF WS-FSUM-LS-PR-PAS(FSUM-IX) = ZERO                              
255000           CONTINUE                                                       
255100        ELSE                                                              
255200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
255300                     WS-FSUM-LS-PR-PAS(FSUM-IX) / 1000                    
255400           MOVE WS-SUMMA-KR TO FSUM-LS-PAS(FSUM-IX)                       
255500        END-IF                                                            
255600                                                                          
255700*********  AK-VÄRDE/FREKVENSKLASS                                         
255800        IF WS-FSUM-AK-PR-AKT(FSUM-IX) = ZERO                              
255900           CONTINUE                                                       
256000        ELSE                                                              
256100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
256200                     WS-FSUM-AK-PR-AKT(FSUM-IX) / 1000                    
256300           MOVE WS-SUMMA-KR TO FSUM-AK-AKT(FSUM-IX)                       
256400        END-IF                                                            
256500                                                                          
256600        IF WS-FSUM-AK-PR-PAS(FSUM-IX) = ZERO                              
256700           CONTINUE                                                       
256800        ELSE                                                              
256900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
257000                     WS-FSUM-AK-PR-PAS(FSUM-IX) / 1000                    
257100           MOVE WS-SUMMA-KR TO FSUM-AK-PAS(FSUM-IX)                       
257200        END-IF                                                            
257300                                                                          
257400*********  SÄKERHETSLAGER/FREKVENSKLASS                                   
257500        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
257600           CONTINUE                                                       
257700        ELSE                                                              
257800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
257900                          WS-FSUM-SLAGER(FSUM-IX) / 1000                  
258000           MOVE WS-SUMMA-KR TO FSUM-SLAGER(FSUM-IX)                       
258100        END-IF                                                            
258200                                                                          
258300*********  ÖVERLAGER/FREKVENSKLASS                                        
258400        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
258500           CONTINUE                                                       
258600        ELSE                                                              
258700           COMPUTE WS-SUMMA-KR ROUNDED                                    
258800                     = WS-FSUM-OLAGER(FSUM-IX) / 1000                     
258900           MOVE WS-SUMMA-KR TO FSUM-OLAGER(FSUM-IX)                       
259000        END-IF                                                            
259100                                                                          
259200*********  MEDELLAGER/FREKVENSKLASS                                       
259300        IF WS-FSUM-MLAGER(FSUM-IX) = ZERO                                 
259400           CONTINUE                                                       
259500        ELSE                                                              
259600           COMPUTE WS-SUMMA-KR =                                          
259700                       WS-FSUM-MLAGER(FSUM-IX) / 1000                     
259800           ADD WS-SUMMA-KR TO FSUM-MLAGER(FSUM-IX)                        
259900        END-IF                                                            
260000                                                                          
260100*********  TOTAL SERVICEGRAD/FREKVENSKLASS                                
260200        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-AKT(FSUM-IX) +           
260300                                    WS-FSUM-KVOI-PAS(FSUM-IX) +           
260400                                    WS-FSUM-KVOI-TEO(FSUM-IX) +           
260500                                    WS-FSUM-KVOI-SAK(FSUM-IX)             
260600        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
260700                       =   WS-FSUM-KVOI-CDC-AKT(FSUM-IX) +                
260800                           WS-FSUM-KVOI-CDC-PAS(FSUM-IX) +                
260900                           WS-FSUM-KVOI-CDC-TEO(FSUM-IX) +                
261000                           WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                  
261100                                                                          
261200        IF WS-KVOI-TOT-VECKA = ZERO                                       
261300           MOVE 99.9 TO FSUM-SERVG-TOT(FSUM-IX)                           
261400        ELSE                                                              
261500           COMPUTE WS-SERVG ROUNDED =                                     
261600             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
261700                              WS-KVOI-TOT-VECKA                           
261800           IF WS-SERVG = 100.0                                            
261900              MOVE 99.9 TO FSUM-SERVG-TOT(FSUM-IX)                        
262000           ELSE                                                           
262100              MOVE WS-SERVG TO FSUM-SERVG-TOT(FSUM-IX)                    
262200           END-IF                                                         
262300        END-IF                                                            
262400                                                                          
262500*********  SERVICEGRAD/FREKVENSKLASS AKTIVA ARTIKLAR                      
262600        IF WS-FSUM-KVOI-AKT(FSUM-IX) = ZERO                               
262700           MOVE 99.9 TO FSUM-SERVG-AKT(FSUM-IX)                           
262800        ELSE                                                              
262900           COMPUTE WS-SERVG ROUNDED = (WS-FSUM-KVOI-AKT(FSUM-IX)          
263000                - WS-FSUM-KVOI-CDC-AKT(FSUM-IX)) * 100                    
263100                        / WS-FSUM-KVOI-AKT(FSUM-IX)                       
263200           IF WS-SERVG = 100.0                                            
263300              MOVE 99.9 TO FSUM-SERVG-AKT(FSUM-IX)                        
263400           ELSE                                                           
263500              MOVE WS-SERVG TO FSUM-SERVG-AKT(FSUM-IX)                    
263600           END-IF                                                         
263700        END-IF                                                            
263800                                                                          
263900*********  SERVICEGRAD/FREKVENSKLASS PASSIVA ARTIKLAR                     
264000        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-PAS(FSUM-IX) +           
264100                                    WS-FSUM-KVOI-TEO(FSUM-IX)             
264200        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
264300                        =   WS-FSUM-KVOI-CDC-PAS(FSUM-IX) +               
264400                            WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                 
264500        IF WS-KVOI-TOT-VECKA = ZERO                                       
264600           MOVE 99.9 TO FSUM-SERVG-PAS(FSUM-IX)                           
264700        ELSE                                                              
264800           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
264900                         WS-KVOI-TOT-CDC-VECKA) * 100                     
265000                        / WS-KVOI-TOT-VECKA                               
265100           IF WS-SERVG = 100.0                                            
265200              MOVE 99.9 TO FSUM-SERVG-PAS(FSUM-IX)                        
265300           ELSE                                                           
265400              MOVE WS-SERVG TO FSUM-SERVG-PAS(FSUM-IX)                    
265500           END-IF                                                         
265600        END-IF                                                            
265700                                                                          
265800*********  TEORETISK SERVICEGRAD/FREKVENSKLASS                            
265900        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-AKT(FSUM-IX) +           
266000                                    WS-FSUM-KVOI-TEO(FSUM-IX)             
266100        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
266200                        =   WS-FSUM-KVOI-CDC-AKT(FSUM-IX) +               
266300                            WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                 
266400        IF WS-KVOI-TOT-VECKA = ZERO                                       
266500           MOVE 99.9 TO FSUM-SERVG-TEO(FSUM-IX)                           
266600        ELSE                                                              
266700           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
266800                         WS-KVOI-TOT-CDC-VECKA) * 100                     
266900                        / WS-KVOI-TOT-VECKA                               
267000           IF WS-SERVG = 100.0                                            
267100              MOVE 99.9 TO FSUM-SERVG-TEO(FSUM-IX)                        
267200           ELSE                                                           
267300              MOVE WS-SERVG TO FSUM-SERVG-TEO(FSUM-IX)                    
267400           END-IF                                                         
267500        END-IF                                                            
267600                                                                          
267700*********  SPLITFAKTOR/FREKVENSKLASS                                      
267800        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-AKT(FSUM-IX) +           
267900                                    WS-FSUM-KVOI-PAS(FSUM-IX) +           
268000                                    WS-FSUM-KVOI-TEO(FSUM-IX) +           
268100                                    WS-FSUM-KVOI-SAK(FSUM-IX)             
268200        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
268300                        =   WS-FSUM-KVOI-CDC-SAK(FSUM-IX) +               
268400                            WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                 
268500        IF WS-KVOI-TOT-VECKA = ZERO                                       
268600           MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                               
268700        ELSE                                                              
268800           COMPUTE WS-SERVG ROUNDED =                                     
268900             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
269000                              WS-KVOI-TOT-VECKA                           
269100           IF WS-SERVG = 100.0                                            
269200              MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                            
269300           ELSE                                                           
269400              MOVE WS-SERVG TO FSUM-SPLIT(FSUM-IX)                        
269500           END-IF                                                         
269600        END-IF                                                            
269700                                                                          
269800*********  OMSHASTIGHET/FREKVENSKLASS                                     
269900        COMPUTE WS-SUMMA = WS-FSUM-KVDISP-PR-AKT(FSUM-IX) +               
270000                           WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                 
270100        IF WS-SUMMA = ZERO                                                
270200           CONTINUE                                                       
270300        ELSE                                                              
270400           COMPUTE WS-OMSHAST ROUNDED =                                   
270500               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
270600           MOVE WS-OMSHAST TO FSUM-OMSHAST-DISP(FSUM-IX)                  
270700        END-IF                                                            
270800                                                                          
270900        COMPUTE WS-SUMMA = WS-FSUM-LS-PR-AKT(FSUM-IX) +                   
271000                           WS-FSUM-AK-PR-AKT(FSUM-IX) +                   
271100                           WS-FSUM-LS-PR-PAS(FSUM-IX) +                   
271200                           WS-FSUM-AK-PR-PAS(FSUM-IX)                     
271300        IF WS-SUMMA = ZERO                                                
271400           CONTINUE                                                       
271500        ELSE                                                              
271600           COMPUTE WS-OMSHAST ROUNDED =                                   
271700               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
271800           MOVE WS-OMSHAST TO FSUM-OMSHAST-LS(FSUM-IX)                    
271900        END-IF                                                            
272000                                                                          
272100        ADD +1 TO FSUM-IX                                                 
272200                                                                          
272300     END-PERFORM                                                          
272400     .                                                                    
272500     EJECT                                                                
272600 BCD-SUMMERA-TOTAL SECTION.                                               
272700******************************************************************        
272800* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
272900******************************************************************        
273000                                                                          
273100     MOVE +1 TO PSUM-IX                                                   
273200     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
273300        ADD PSUM-KVANT-AKT(PSUM-IX) TO TOT-KVANT-AKT                      
273400        ADD PSUM-KVANT-PAS(PSUM-IX) TO TOT-KVANT-PAS                      
273500        ADD PSUM-KVOT(PSUM-IX) TO TOT-KVOT                                
273600        ADD WS-PSUM-KVOI-AKT(PSUM-IX) TO WS-TOT-KVOI-AKT                  
273700        ADD WS-PSUM-KVOI-PAS(PSUM-IX) TO WS-TOT-KVOI-PAS                  
273800        ADD WS-PSUM-KVOI-TEO(PSUM-IX) TO WS-TOT-KVOI-TEO                  
273900        ADD WS-PSUM-KVOI-SAK(PSUM-IX) TO WS-TOT-KVOI-SAK                  
274000        ADD WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                                 
274100                                TO WS-TOT-KVOI-CDC-AKT                    
274200        ADD WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                                 
274300                                TO WS-TOT-KVOI-CDC-PAS                    
274400        ADD WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                                 
274500                                TO WS-TOT-KVOI-CDC-TEO                    
274600        ADD WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                                 
274700                                TO WS-TOT-KVOI-CDC-SAK                    
274800        ADD WS-PSUM-KVOI(PSUM-IX) TO WS-TOT-KVOI                          
274900        ADD WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                                
275000                           TO WS-TOT-KVDISP-PR-AKT                        
275100        ADD WS-PSUM-OK-PR-AKT(PSUM-IX)                                    
275200                           TO WS-TOT-OK-PR-AKT                            
275300        ADD WS-PSUM-LS-PR-AKT(PSUM-IX)                                    
275400                           TO WS-TOT-LS-PR-AKT                            
275500        ADD WS-PSUM-AK-PR-AKT(PSUM-IX)                                    
275600                           TO WS-TOT-AK-PR-AKT                            
275700        ADD WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                                
275800                           TO WS-TOT-KVDISP-PR-PAS                        
275900        ADD WS-PSUM-OK-PR-PAS(PSUM-IX)                                    
276000                           TO WS-TOT-OK-PR-PAS                            
276100        ADD WS-PSUM-LS-PR-PAS(PSUM-IX)                                    
276200                           TO WS-TOT-LS-PR-PAS                            
276300        ADD WS-PSUM-AK-PR-PAS(PSUM-IX)                                    
276400                           TO WS-TOT-AK-PR-PAS                            
276500        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
276600        ADD WS-PSUM-MLAGER(PSUM-IX) TO WS-TOT-MLAGER                      
276700        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
276800                                                                          
276900        ADD +1 TO PSUM-IX                                                 
277000     END-PERFORM                                                          
277100                                                                          
277200*********  DISP LAGER VÄRDE TOTALT                                        
277300        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
277400           CONTINUE                                                       
277500        ELSE                                                              
277600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
277700                     WS-TOT-KVDISP-PR-AKT / 1000                          
277800           MOVE WS-SUMMA-KR TO TOT-KVDISP-AKT                             
277900        END-IF                                                            
278000                                                                          
278100        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
278200           CONTINUE                                                       
278300        ELSE                                                              
278400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
278500                     WS-TOT-KVDISP-PR-PAS / 1000                          
278600           MOVE WS-SUMMA-KR TO TOT-KVDISP-PAS                             
278700        END-IF                                                            
278800                                                                          
278900*********  LAGERVÄRDE TOTALT                                              
279000        IF WS-TOT-LS-PR-AKT = ZERO                                        
279100           CONTINUE                                                       
279200        ELSE                                                              
279300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
279400                     WS-TOT-LS-PR-AKT / 1000                              
279500           MOVE WS-SUMMA-KR TO TOT-LS-AKT                                 
279600        END-IF                                                            
279700                                                                          
279800        IF WS-TOT-LS-PR-PAS = ZERO                                        
279900           CONTINUE                                                       
280000        ELSE                                                              
280100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
280200                     WS-TOT-LS-PR-PAS / 1000                              
280300           MOVE WS-SUMMA-KR TO TOT-LS-PAS                                 
280400        END-IF                                                            
280500                                                                          
280600*********  AK-VÄRDE TOTALT                                                
280700        IF WS-TOT-AK-PR-AKT = ZERO                                        
280800           CONTINUE                                                       
280900        ELSE                                                              
281000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
281100                     WS-TOT-AK-PR-AKT / 1000                              
281200           MOVE WS-SUMMA-KR TO TOT-AK-AKT                                 
281300        END-IF                                                            
281400                                                                          
281500        IF WS-TOT-AK-PR-PAS = ZERO                                        
281600           CONTINUE                                                       
281700        ELSE                                                              
281800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
281900                     WS-TOT-AK-PR-PAS / 1000                              
282000           MOVE WS-SUMMA-KR TO TOT-AK-PAS                                 
282100        END-IF                                                            
282200                                                                          
282300*********  SÄKERHETSLAGER TOTALT                                          
282400        IF WS-TOT-SLAGER = ZERO                                           
282500           CONTINUE                                                       
282600        ELSE                                                              
282700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
282800                          WS-TOT-SLAGER / 1000                            
282900           MOVE WS-SUMMA-KR TO TOT-SLAGER                                 
283000        END-IF                                                            
283100                                                                          
283200*********  ÖVERLAGER TOTALT                                               
283300        IF WS-TOT-OLAGER = ZERO                                           
283400           CONTINUE                                                       
283500        ELSE                                                              
283600           COMPUTE WS-SUMMA-KR ROUNDED                                    
283700                     = WS-TOT-OLAGER / 1000                               
283800           MOVE WS-SUMMA-KR TO TOT-OLAGER                                 
283900        END-IF                                                            
284000                                                                          
284100*********  MEDELLAGER TOTALT                                              
284200        IF WS-TOT-MLAGER = ZERO                                           
284300           CONTINUE                                                       
284400        ELSE                                                              
284500           COMPUTE WS-SUMMA-KR =                                          
284600                       WS-TOT-MLAGER / 1000                               
284700           ADD WS-SUMMA-KR TO TOT-MLAGER                                  
284800        END-IF                                                            
284900                                                                          
285000*********  TOTAL SERVICEGRAD TOTALT                                       
285100        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-AKT +                     
285200                                    WS-TOT-KVOI-PAS +                     
285300                                    WS-TOT-KVOI-TEO +                     
285400                                    WS-TOT-KVOI-SAK                       
285500        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
285600                       =   WS-TOT-KVOI-CDC-AKT +                          
285700                           WS-TOT-KVOI-CDC-PAS +                          
285800                           WS-TOT-KVOI-CDC-TEO +                          
285900                           WS-TOT-KVOI-CDC-SAK                            
286000                                                                          
286100        IF WS-KVOI-TOT-VECKA = ZERO                                       
286200           MOVE 99.9 TO TOT-SERVG-TOT                                     
286300        ELSE                                                              
286400           COMPUTE WS-SERVG ROUNDED =                                     
286500             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
286600                              WS-KVOI-TOT-VECKA                           
286700           IF WS-SERVG = 100.0                                            
286800              MOVE 99.9 TO TOT-SERVG-TOT                                  
286900           ELSE                                                           
287000              MOVE WS-SERVG TO TOT-SERVG-TOT                              
287100           END-IF                                                         
287200        END-IF                                                            
287300                                                                          
287400*********  SERVICEGRAD TOTALT AKTIVA ARTIKLAR                             
287500                                                                          
287600        IF WS-TOT-KVOI-AKT = ZERO                                         
287700           MOVE 99.9 TO TOT-SERVG-AKT                                     
287800        ELSE                                                              
287900           COMPUTE WS-SERVG ROUNDED = (WS-TOT-KVOI-AKT                    
288000                      - WS-TOT-KVOI-CDC-AKT) * 100                        
288100                        / WS-TOT-KVOI-AKT                                 
288200           IF WS-SERVG = 100.0                                            
288300              MOVE 99.9 TO TOT-SERVG-AKT                                  
288400           ELSE                                                           
288500              MOVE WS-SERVG TO TOT-SERVG-AKT                              
288600           END-IF                                                         
288700        END-IF                                                            
288800                                                                          
288900*********  SERVICEGRAD TOTALT PASSIVA ARTIKLAR                            
289000        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-PAS +                     
289100                                    WS-TOT-KVOI-TEO                       
289200        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
289300                        =   WS-TOT-KVOI-CDC-PAS +                         
289400                            WS-TOT-KVOI-CDC-TEO                           
289500        IF WS-KVOI-TOT-VECKA = ZERO                                       
289600           MOVE 99.9 TO TOT-SERVG-PAS                                     
289700        ELSE                                                              
289800           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
289900                         WS-KVOI-TOT-CDC-VECKA) * 100                     
290000                        / WS-KVOI-TOT-VECKA                               
290100           IF WS-SERVG = 100.0                                            
290200              MOVE 99.9 TO TOT-SERVG-PAS                                  
290300           ELSE                                                           
290400              MOVE WS-SERVG TO TOT-SERVG-PAS                              
290500           END-IF                                                         
290600        END-IF                                                            
290700                                                                          
290800*********  TEORETISK SERVICEGRAD TOTALT                                   
290900        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-AKT +                     
291000                                    WS-TOT-KVOI-TEO                       
291100        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
291200                        =   WS-TOT-KVOI-CDC-AKT +                         
291300                            WS-TOT-KVOI-CDC-TEO                           
291400        IF WS-KVOI-TOT-VECKA = ZERO                                       
291500           MOVE 99.9 TO TOT-SERVG-TEO                                     
291600        ELSE                                                              
291700           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
291800                         WS-KVOI-TOT-CDC-VECKA) * 100                     
291900                        / WS-KVOI-TOT-VECKA                               
292000           IF WS-SERVG = 100.0                                            
292100              MOVE 99.9 TO TOT-SERVG-TEO                                  
292200           ELSE                                                           
292300              MOVE WS-SERVG TO TOT-SERVG-TEO                              
292400           END-IF                                                         
292500        END-IF                                                            
292600                                                                          
292700*********  SPLITFAKTOR TOTALT                                             
292800        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-AKT +                     
292900                                    WS-TOT-KVOI-PAS +                     
293000                                    WS-TOT-KVOI-TEO +                     
293100                                    WS-TOT-KVOI-SAK +                     
293200                                    WS-KVOT-SAKNAS-WDK7                   
293300        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
293400                        =   WS-TOT-KVOI-CDC-SAK +                         
293500                            WS-TOT-KVOI-CDC-PAS +                         
293600                            WS-KVOT-CDC-SAKNAS-WDK7                       
293700        IF WS-KVOI-TOT-VECKA = ZERO                                       
293800           MOVE 99.9 TO TOT-SPLIT                                         
293900        ELSE                                                              
294000           COMPUTE WS-SERVG ROUNDED =                                     
294100             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
294200                              WS-KVOI-TOT-VECKA                           
294300           IF WS-SERVG = 100.0                                            
294400              MOVE 99.9 TO TOT-SPLIT                                      
294500           ELSE                                                           
294600              MOVE WS-SERVG TO TOT-SPLIT                                  
294700           END-IF                                                         
294800        END-IF                                                            
294900                                                                          
295000*********  OMSHASTIGHET TOTALT                                            
295100        COMPUTE WS-SUMMA = WS-TOT-KVDISP-PR-AKT +                         
295200                           WS-TOT-KVDISP-PR-PAS                           
295300        IF WS-SUMMA = ZERO                                                
295400           CONTINUE                                                       
295500        ELSE                                                              
295600           COMPUTE WS-OMSHAST ROUNDED =                                   
295700               WS-TOT-KVOI / WS-SUMMA                                     
295800           MOVE WS-OMSHAST TO TOT-OMSHAST-DISP                            
295900        END-IF                                                            
296000                                                                          
296100        COMPUTE WS-SUMMA = WS-TOT-LS-PR-AKT +                             
296200                           WS-TOT-AK-PR-AKT +                             
296300                           WS-TOT-LS-PR-PAS +                             
296400                           WS-TOT-AK-PR-PAS                               
296500        IF WS-SUMMA = ZERO                                                
296600           CONTINUE                                                       
296700        ELSE                                                              
296800           COMPUTE WS-OMSHAST ROUNDED =                                   
296900               WS-TOT-KVOI / WS-SUMMA                                     
297000           MOVE WS-OMSHAST TO TOT-OMSHAST-LS                              
297100                                                                          
297200        END-IF                                                            
297300*********  ORDERTRÄFFAR TOTALT                                            
297400                                                                          
297500        ADD WS-KVOT-SAKNAS-WDK7 TO TOT-KVOT                               
297600     .                                                                    
297700     EJECT                                                                
297800 BCE-BERAKNINGAR-AV-TOTAL SECTION.                                        
297900******************************************************************        
298000* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
298100******************************************************************        
298200                                                                          
298300     MOVE +1 TO ART-IX                                                    
298400     MOVE +72 TO ART-IX-MAX                                               
298500     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
298600                                                                          
298700******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
298800        IF TOT-KVANT-AKT = ZERO                                           
298900           CONTINUE                                                       
299000        ELSE                                                              
299100           COMPUTE WS-PROC = ART-KVANT-AKT(ART-IX)                        
299200                                    * 100 / TOT-KVANT-AKT                 
299300           MOVE WS-PROC TO ART-PROC-KVANT-A(ART-IX)                       
299400        END-IF                                                            
299500                                                                          
299600******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
299700        IF TOT-KVANT-PAS = ZERO                                           
299800           CONTINUE                                                       
299900        ELSE                                                              
300000           COMPUTE WS-PROC = ART-KVANT-PAS(ART-IX)                        
300100                                    * 100 / TOT-KVANT-PAS                 
300200           MOVE WS-PROC TO ART-PROC-KVANT-P(ART-IX)                       
300300        END-IF                                                            
300400                                                                          
300500******** % ANTAL ORDERTRÄFFAR AV TOTAL                                    
300600        IF TOT-KVOT = ZERO                                                
300700           CONTINUE                                                       
300800        ELSE                                                              
300900           COMPUTE WS-PROC = ART-KVOT(ART-IX)                             
301000                                    * 100 / TOT-KVOT                      
301100           MOVE WS-PROC TO ART-PROC-KVOT(ART-IX)                          
301200        END-IF                                                            
301300                                                                          
301400*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  AKTIVA                      
301500                                                                          
301600        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
301700           CONTINUE                                                       
301800        ELSE                                                              
301900           IF WS-ART-KVDISP-PR-AKT(ART-IX) > ZERO                         
302000              COMPUTE WS-PROC = WS-ART-KVDISP-PR-AKT(ART-IX)              
302100                              * 100 / WS-TOT-KVDISP-PR-AKT                
302200              MOVE WS-PROC TO ART-PROC-KVDISP-A(ART-IX)                   
302300           END-IF                                                         
302400        END-IF                                                            
302500                                                                          
302600*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  PASSIVA                     
302700                                                                          
302800        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
302900           CONTINUE                                                       
303000        ELSE                                                              
303100           IF WS-ART-KVDISP-PR-PAS(ART-IX) > ZERO                         
303200              COMPUTE WS-PROC = WS-ART-KVDISP-PR-PAS(ART-IX)              
303300                              * 100 / WS-TOT-KVDISP-PR-PAS                
303400              MOVE WS-PROC TO ART-PROC-KVDISP-P(ART-IX)                   
303500           END-IF                                                         
303600        END-IF                                                            
303700                                                                          
303800*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                      
303900                                                                          
304000        IF WS-TOT-LS-PR-AKT = ZERO                                        
304100           CONTINUE                                                       
304200        ELSE                                                              
304300           IF WS-ART-LS-PR-AKT(ART-IX) > ZERO                             
304400              COMPUTE WS-PROC = WS-ART-LS-PR-AKT(ART-IX)                  
304500                              * 100 / WS-TOT-LS-PR-AKT                    
304600              MOVE WS-PROC TO ART-PROC-LS-A(ART-IX)                       
304700           END-IF                                                         
304800        END-IF                                                            
304900                                                                          
305000*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA                     
305100                                                                          
305200        IF WS-TOT-LS-PR-PAS = ZERO                                        
305300           CONTINUE                                                       
305400        ELSE                                                              
305500           IF WS-ART-LS-PR-PAS(ART-IX) > ZERO                             
305600              COMPUTE WS-PROC = WS-ART-LS-PR-PAS(ART-IX)                  
305700                              * 100 / WS-TOT-LS-PR-PAS                    
305800              MOVE WS-PROC TO ART-PROC-LS-P(ART-IX)                       
305900           END-IF                                                         
306000        END-IF                                                            
306100                                                                          
306200*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                          
306300                                                                          
306400        IF WS-TOT-AK-PR-AKT = ZERO                                        
306500           CONTINUE                                                       
306600        ELSE                                                              
306700           IF WS-ART-AK-PR-AKT(ART-IX) > ZERO                             
306800              COMPUTE WS-PROC = WS-ART-AK-PR-AKT(ART-IX)                  
306900                              * 100 / WS-TOT-AK-PR-AKT                    
307000              MOVE WS-PROC TO ART-PROC-AK-A(ART-IX)                       
307100           END-IF                                                         
307200        END-IF                                                            
307300                                                                          
307400*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                         
307500                                                                          
307600        IF WS-TOT-AK-PR-PAS = ZERO                                        
307700           CONTINUE                                                       
307800        ELSE                                                              
307900           IF WS-ART-AK-PR-PAS(ART-IX) > ZERO                             
308000              COMPUTE WS-PROC = WS-ART-AK-PR-PAS(ART-IX)                  
308100                              * 100 / WS-TOT-AK-PR-PAS                    
308200              MOVE WS-PROC TO ART-PROC-AK-P(ART-IX)                       
308300           END-IF                                                         
308400        END-IF                                                            
308500                                                                          
308600        ADD +1 TO ART-IX                                                  
308700     END-PERFORM                                                          
308800                                                                          
308900****************************                                              
309000                                                                          
309100     MOVE +1 TO PSUM-IX                                                   
309200     MOVE +9 TO PSUM-IX-MAX                                               
309300     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
309400                                                                          
309500******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
309600        IF TOT-KVANT-AKT = ZERO                                           
309700           CONTINUE                                                       
309800        ELSE                                                              
309900           COMPUTE WS-PROC = PSUM-KVANT-AKT(PSUM-IX) * 100 /              
310000                           TOT-KVANT-AKT                                  
310100           MOVE WS-PROC TO PSUM-PROC-KVANT-A(PSUM-IX)                     
310200        END-IF                                                            
310300                                                                          
310400******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
310500        IF TOT-KVANT-PAS = ZERO                                           
310600           CONTINUE                                                       
310700        ELSE                                                              
310800           COMPUTE WS-PROC = PSUM-KVANT-PAS(PSUM-IX) * 100 /              
310900                           TOT-KVANT-PAS                                  
311000           MOVE WS-PROC TO PSUM-PROC-KVANT-P(PSUM-IX)                     
311100        END-IF                                                            
311200                                                                          
311300******** % ANTAL ORDERTRÄFFAR AV TOTALA                                   
311400        IF TOT-KVOT = ZERO                                                
311500           CONTINUE                                                       
311600        ELSE                                                              
311700           COMPUTE WS-PROC = PSUM-KVOT(PSUM-IX) * 100 /                   
311800                           TOT-KVOT                                       
311900           MOVE WS-PROC TO PSUM-PROC-KVOT(PSUM-IX)                        
312000        END-IF                                                            
312100                                                                          
312200*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA                
312300                                                                          
312400        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
312500           CONTINUE                                                       
312600        ELSE                                                              
312700           IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) > ZERO                       
312800              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-AKT(PSUM-IX)            
312900                              * 100 / WS-TOT-KVDISP-PR-AKT                
313000              MOVE WS-PROC TO PSUM-PROC-KVDISP-A(PSUM-IX)                 
313100           END-IF                                                         
313200        END-IF                                                            
313300                                                                          
313400*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA               
313500                                                                          
313600        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
313700           CONTINUE                                                       
313800        ELSE                                                              
313900           IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) > ZERO                       
314000              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
314100                              * 100 / WS-TOT-KVDISP-PR-PAS                
314200              MOVE WS-PROC TO PSUM-PROC-KVDISP-P(PSUM-IX)                 
314300           END-IF                                                         
314400        END-IF                                                            
314500                                                                          
314600*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                
314700                                                                          
314800        IF WS-TOT-LS-PR-AKT = ZERO                                        
314900           CONTINUE                                                       
315000        ELSE                                                              
315100           IF WS-PSUM-LS-PR-AKT(PSUM-IX) > ZERO                           
315200              COMPUTE WS-PROC = WS-PSUM-LS-PR-AKT(PSUM-IX)                
315300                              * 100 / WS-TOT-LS-PR-AKT                    
315400              MOVE WS-PROC TO PSUM-PROC-LS-A(PSUM-IX)                     
315500           END-IF                                                         
315600        END-IF                                                            
315700                                                                          
315800*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA               
315900                                                                          
316000        IF WS-TOT-LS-PR-PAS = ZERO                                        
316100           CONTINUE                                                       
316200        ELSE                                                              
316300           IF WS-PSUM-LS-PR-PAS(PSUM-IX) > ZERO                           
316400              COMPUTE WS-PROC = WS-PSUM-LS-PR-PAS(PSUM-IX)                
316500                              * 100 / WS-TOT-LS-PR-PAS                    
316600              MOVE WS-PROC TO PSUM-PROC-LS-P(PSUM-IX)                     
316700           END-IF                                                         
316800        END-IF                                                            
316900                                                                          
317000*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                    
317100                                                                          
317200        IF WS-TOT-AK-PR-AKT = ZERO                                        
317300           CONTINUE                                                       
317400        ELSE                                                              
317500           IF WS-PSUM-AK-PR-AKT(PSUM-IX) > ZERO                           
317600              COMPUTE WS-PROC = WS-PSUM-AK-PR-AKT(PSUM-IX)                
317700                              * 100 / WS-TOT-AK-PR-AKT                    
317800              MOVE WS-PROC TO PSUM-PROC-AK-A(PSUM-IX)                     
317900           END-IF                                                         
318000        END-IF                                                            
318100                                                                          
318200*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                   
318300                                                                          
318400        IF WS-TOT-AK-PR-PAS = ZERO                                        
318500           CONTINUE                                                       
318600        ELSE                                                              
318700           IF WS-PSUM-AK-PR-PAS(PSUM-IX) > ZERO                           
318800              COMPUTE WS-PROC = WS-PSUM-AK-PR-PAS(PSUM-IX)                
318900                              * 100 / WS-TOT-AK-PR-PAS                    
319000              MOVE WS-PROC TO PSUM-PROC-AK-P(PSUM-IX)                     
319100           END-IF                                                         
319200        END-IF                                                            
319300                                                                          
319400        ADD +1 TO PSUM-IX                                                 
319500     END-PERFORM                                                          
319600                                                                          
319700****************************************                                  
319800                                                                          
319900     MOVE +1 TO FSUM-IX                                                   
320000     MOVE +8 TO FSUM-IX-MAX                                               
320100     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
320200                                                                          
320300******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
320400        IF TOT-KVANT-AKT = ZERO                                           
320500           CONTINUE                                                       
320600        ELSE                                                              
320700           COMPUTE WS-PROC = FSUM-KVANT-AKT(FSUM-IX) * 100 /              
320800                           TOT-KVANT-AKT                                  
320900           MOVE WS-PROC TO FSUM-PROC-KVANT-A(FSUM-IX)                     
321000        END-IF                                                            
321100                                                                          
321200******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
321300        IF TOT-KVANT-PAS = ZERO                                           
321400           CONTINUE                                                       
321500        ELSE                                                              
321600           COMPUTE WS-PROC = FSUM-KVANT-PAS(FSUM-IX) * 100 /              
321700                           TOT-KVANT-PAS                                  
321800           MOVE WS-PROC TO FSUM-PROC-KVANT-P(FSUM-IX)                     
321900        END-IF                                                            
322000                                                                          
322100******** % ANTAL ORDERTRÄFFAR TOTALA                                      
322200        IF TOT-KVOT = ZERO                                                
322300           CONTINUE                                                       
322400        ELSE                                                              
322500           COMPUTE WS-PROC = FSUM-KVOT(FSUM-IX) * 100 /                   
322600                           TOT-KVOT                                       
322700           MOVE WS-PROC TO FSUM-PROC-KVOT(FSUM-IX)                        
322800        END-IF                                                            
322900                                                                          
323000*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA            
323100                                                                          
323200        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
323300           CONTINUE                                                       
323400        ELSE                                                              
323500           IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) > ZERO                       
323600              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-AKT(FSUM-IX)            
323700                              * 100 / WS-TOT-KVDISP-PR-AKT                
323800              MOVE WS-PROC TO FSUM-PROC-KVDISP-A(FSUM-IX)                 
323900           END-IF                                                         
324000        END-IF                                                            
324100                                                                          
324200*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA           
324300                                                                          
324400        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
324500           CONTINUE                                                       
324600        ELSE                                                              
324700           IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) > ZERO                       
324800              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
324900                              * 100 / WS-TOT-KVDISP-PR-PAS                
325000              MOVE WS-PROC TO FSUM-PROC-KVDISP-P(FSUM-IX)                 
325100           END-IF                                                         
325200        END-IF                                                            
325300                                                                          
325400*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA            
325500                                                                          
325600        IF WS-TOT-LS-PR-AKT = ZERO                                        
325700           CONTINUE                                                       
325800        ELSE                                                              
325900           IF WS-FSUM-LS-PR-AKT(FSUM-IX) > ZERO                           
326000              COMPUTE WS-PROC = WS-FSUM-LS-PR-AKT(FSUM-IX)                
326100                              * 100 / WS-TOT-LS-PR-AKT                    
326200              MOVE WS-PROC TO FSUM-PROC-LS-A(FSUM-IX)                     
326300           END-IF                                                         
326400        END-IF                                                            
326500                                                                          
326600*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA           
326700                                                                          
326800        IF WS-TOT-LS-PR-PAS = ZERO                                        
326900           CONTINUE                                                       
327000        ELSE                                                              
327100           IF WS-FSUM-LS-PR-PAS(FSUM-IX) > ZERO                           
327200              COMPUTE WS-PROC = WS-FSUM-LS-PR-PAS(FSUM-IX)                
327300                              * 100 / WS-TOT-LS-PR-PAS                    
327400              MOVE WS-PROC TO FSUM-PROC-LS-P(FSUM-IX)                     
327500           END-IF                                                         
327600        END-IF                                                            
327700                                                                          
327800*******  %  FREKVENSSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA               
327900                                                                          
328000        IF WS-TOT-AK-PR-AKT = ZERO                                        
328100           CONTINUE                                                       
328200        ELSE                                                              
328300           IF WS-FSUM-AK-PR-AKT(FSUM-IX) > ZERO                           
328400              COMPUTE WS-PROC = WS-FSUM-AK-PR-AKT(FSUM-IX)                
328500                              * 100 / WS-TOT-AK-PR-AKT                    
328600              MOVE WS-PROC TO FSUM-PROC-AK-A(FSUM-IX)                     
328700           END-IF                                                         
328800        END-IF                                                            
328900                                                                          
329000*******  %  FREKVENSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA               
329100                                                                          
329200        IF WS-TOT-AK-PR-PAS = ZERO                                        
329300           CONTINUE                                                       
329400        ELSE                                                              
329500           IF WS-FSUM-AK-PR-PAS(FSUM-IX) > ZERO                           
329600              COMPUTE WS-PROC = WS-FSUM-AK-PR-PAS(FSUM-IX)                
329700                              * 100 / WS-TOT-AK-PR-PAS                    
329800              MOVE WS-PROC TO FSUM-PROC-AK-P(FSUM-IX)                     
329900           END-IF                                                         
330000        END-IF                                                            
330100                                                                          
330200        ADD +1 TO FSUM-IX                                                 
330300     END-PERFORM                                                          
330400     .                                                                    
330500     EJECT                                                                
330600 C-SKRIV-LISTA SECTION.                                                   
330700******************************************************************        
330800*  SID 1 BESTÅR AV 3 RUTRADER INKL PRISKLASS-TOTAL               *        
330900*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
331000*      3           3 RUTRADER INKL PRISKLASS-TOTAL               *        
331100*      4           1 RUTRAD   FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
331200******************************************************************        
331300                                                                          
331400     MOVE +1 TO IX1                                                       
331500     MOVE +2 TO IX2                                                       
331600     MOVE +3 TO IX3                                                       
331700     MOVE +4 TO IX4                                                       
331800     MOVE +5 TO IX5                                                       
331900     MOVE +6 TO IX6                                                       
332000     MOVE +7 TO IX7                                                       
332100     MOVE +8 TO IX8                                                       
332200     MOVE +1 TO PSUM-IX                                                   
332300                                                                          
332400*********** SKRIVER SID-1                                                 
332500                                                                          
332600     PERFORM S21A-SKRIV-RUBRIKER                                          
332700     MOVE '1' TO W001-DET1-PRISKLASS                                      
332800     PERFORM CA-FLYTTA-SKRIV-RAD                                          
332900     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
333000     ADD +1 TO PSUM-IX                                                    
333100     MOVE '2' TO W001-DET1-PRISKLASS                                      
333200     PERFORM CA-FLYTTA-SKRIV-RAD                                          
333300                                                                          
333400     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
333500     ADD +1 TO PSUM-IX                                                    
333600     MOVE '3' TO W001-DET1-PRISKLASS                                      
333700     PERFORM CA-FLYTTA-SKRIV-RAD                                          
333800                                                                          
333900*********** SKRIVER SID-2                                                 
334000     PERFORM S21A-SKRIV-RUBRIKER                                          
334100     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
334200     ADD +1 TO PSUM-IX                                                    
334300     MOVE '4' TO W001-DET1-PRISKLASS                                      
334400     PERFORM CA-FLYTTA-SKRIV-RAD                                          
334500                                                                          
334600     ADD +1 TO PSUM-IX                                                    
334700     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
334800     MOVE '5' TO W001-DET1-PRISKLASS                                      
334900     PERFORM CA-FLYTTA-SKRIV-RAD                                          
335000                                                                          
335100     ADD +1 TO PSUM-IX                                                    
335200     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
335300     MOVE '6' TO W001-DET1-PRISKLASS                                      
335400     PERFORM CA-FLYTTA-SKRIV-RAD                                          
335500                                                                          
335600*********** SKRIVER SID-3                                                 
335700     PERFORM S21A-SKRIV-RUBRIKER                                          
335800     ADD +1 TO PSUM-IX                                                    
335900     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
336000     MOVE '7' TO W001-DET1-PRISKLASS                                      
336100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
336200                                                                          
336300     ADD +1 TO PSUM-IX                                                    
336400     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
336500     MOVE '8' TO W001-DET1-PRISKLASS                                      
336600     PERFORM CA-FLYTTA-SKRIV-RAD                                          
336700                                                                          
336800     ADD +1 TO PSUM-IX                                                    
336900     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
337000     MOVE '9' TO W001-DET1-PRISKLASS                                      
337100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
337200                                                                          
337300*********** SKRIVER SID-4                                                 
337400     PERFORM S21A-SKRIV-RUBRIKER                                          
337500     MOVE +1 TO IX1                                                       
337600     MOVE +2 TO IX2                                                       
337700     MOVE +3 TO IX3                                                       
337800     MOVE +4 TO IX4                                                       
337900     MOVE +5 TO IX5                                                       
338000     MOVE +6 TO IX6                                                       
338100     MOVE +7 TO IX7                                                       
338200     MOVE +8 TO IX8                                                       
338300     MOVE SPACE TO W001-DET1-PRISKLASS                                    
338400     PERFORM CB-FLYTTA-SKRIV-TOT                                          
338500     .                                                                    
338600     EJECT                                                                
338700 CA-FLYTTA-SKRIV-RAD SECTION.                                             
338800                                                                          
338900     MOVE ART-KVANT-AKT(IX1)       TO  W001-DET1-KVANTA                   
339000     MOVE ART-PROC-KVANT-A(IX1)    TO  W001-DET1-P-KVANTA                 
339100     MOVE ART-KVANT-AKT(IX2)       TO  W001-DET1-KVANTB                   
339200     MOVE ART-PROC-KVANT-A(IX2)    TO  W001-DET1-P-KVANTB                 
339300     MOVE ART-KVANT-AKT(IX3)       TO  W001-DET1-KVANTC                   
339400     MOVE ART-PROC-KVANT-A(IX3)    TO  W001-DET1-P-KVANTC                 
339500     MOVE ART-KVANT-AKT(IX4)       TO  W001-DET1-KVANTD                   
339600     MOVE ART-PROC-KVANT-A(IX4)    TO  W001-DET1-P-KVANTD                 
339700     MOVE ART-KVANT-AKT(IX5)       TO  W001-DET1-KVANTE                   
339800     MOVE ART-PROC-KVANT-A(IX5)    TO  W001-DET1-P-KVANTE                 
339900     MOVE ART-KVANT-AKT(IX6)       TO  W001-DET1-KVANTF                   
340000     MOVE ART-PROC-KVANT-A(IX6)    TO  W001-DET1-P-KVANTF                 
340100     MOVE ART-KVANT-AKT(IX7)       TO  W001-DET1-KVANTG                   
340200     MOVE ART-PROC-KVANT-A(IX7)    TO  W001-DET1-P-KVANTG                 
340300     MOVE ART-KVANT-AKT(IX8)       TO  W001-DET1-KVANTH                   
340400     MOVE ART-PROC-KVANT-A(IX8)    TO  W001-DET1-P-KVANTH                 
340500     MOVE PSUM-KVANT-AKT(PSUM-IX)  TO  W001-DET1-TOT                      
340600     MOVE PSUM-PROC-KVANT-A(PSUM-IX) TO W001-DET1-P-TOT                   
340700     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
340800     MOVE +2 TO W001-SKIP                                                 
340900     PERFORM S21-SKRIV-LISTA                                              
341000                                                                          
341100     MOVE ART-KVANT-PAS(IX1)       TO  W001-DET2-KVANTA                   
341200     MOVE ART-PROC-KVANT-P(IX1)    TO  W001-DET2-P-KVANTA                 
341300     MOVE ART-KVANT-PAS(IX2)       TO  W001-DET2-KVANTB                   
341400     MOVE ART-PROC-KVANT-P(IX2)    TO  W001-DET2-P-KVANTB                 
341500     MOVE ART-KVANT-PAS(IX3)       TO  W001-DET2-KVANTC                   
341600     MOVE ART-PROC-KVANT-P(IX3)    TO  W001-DET2-P-KVANTC                 
341700     MOVE ART-KVANT-PAS(IX4)       TO  W001-DET2-KVANTD                   
341800     MOVE ART-PROC-KVANT-P(IX4)    TO  W001-DET2-P-KVANTD                 
341900     MOVE ART-KVANT-PAS(IX5)       TO  W001-DET2-KVANTE                   
342000     MOVE ART-PROC-KVANT-P(IX5)    TO  W001-DET2-P-KVANTE                 
342100     MOVE ART-KVANT-PAS(IX6)       TO  W001-DET2-KVANTF                   
342200     MOVE ART-PROC-KVANT-P(IX6)    TO  W001-DET2-P-KVANTF                 
342300     MOVE ART-KVANT-PAS(IX7)       TO  W001-DET2-KVANTG                   
342400     MOVE ART-PROC-KVANT-P(IX7)    TO  W001-DET2-P-KVANTG                 
342500     MOVE ART-KVANT-PAS(IX8)       TO  W001-DET2-KVANTH                   
342600     MOVE ART-PROC-KVANT-P(IX8)    TO  W001-DET2-P-KVANTH                 
342700     MOVE PSUM-KVANT-PAS(PSUM-IX)  TO  W001-DET2-TOT                      
342800     MOVE PSUM-PROC-KVANT-P(PSUM-IX) TO W001-DET2-P-TOT                   
342900     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
343000     MOVE +1 TO W001-SKIP                                                 
343100     PERFORM S21-SKRIV-LISTA                                              
343200                                                                          
343300     MOVE ART-KVDISP-AKT(IX1)      TO  W001-DET3-DLAGERA                  
343400     MOVE ART-PROC-KVDISP-A(IX1)   TO  W001-DET3-P-DLAGERA                
343500     MOVE ART-KVDISP-AKT(IX2)      TO  W001-DET3-DLAGERB                  
343600     MOVE ART-PROC-KVDISP-A(IX2)   TO  W001-DET3-P-DLAGERB                
343700     MOVE ART-KVDISP-AKT(IX3)      TO  W001-DET3-DLAGERC                  
343800     MOVE ART-PROC-KVDISP-A(IX3)   TO  W001-DET3-P-DLAGERC                
343900     MOVE ART-KVDISP-AKT(IX4)      TO  W001-DET3-DLAGERD                  
344000     MOVE ART-PROC-KVDISP-A(IX4)   TO  W001-DET3-P-DLAGERD                
344100     MOVE ART-KVDISP-AKT(IX5)      TO  W001-DET3-DLAGERE                  
344200     MOVE ART-PROC-KVDISP-A(IX5)   TO  W001-DET3-P-DLAGERE                
344300     MOVE ART-KVDISP-AKT(IX6)      TO  W001-DET3-DLAGERF                  
344400     MOVE ART-PROC-KVDISP-A(IX6)   TO  W001-DET3-P-DLAGERF                
344500     MOVE ART-KVDISP-AKT(IX7)      TO  W001-DET3-DLAGERG                  
344600     MOVE ART-PROC-KVDISP-A(IX7)   TO  W001-DET3-P-DLAGERG                
344700     MOVE ART-KVDISP-AKT(IX8)      TO  W001-DET3-DLAGERH                  
344800     MOVE ART-PROC-KVDISP-A(IX8)   TO  W001-DET3-P-DLAGERH                
344900     MOVE PSUM-KVDISP-AKT(PSUM-IX) TO  W001-DET3-TOT                      
345000     MOVE PSUM-PROC-KVDISP-A(PSUM-IX)                                     
345100                                   TO  W001-DET3-P-TOT                    
345200     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
345300     MOVE +1 TO W001-SKIP                                                 
345400     PERFORM S21-SKRIV-LISTA                                              
345500                                                                          
345600     MOVE ART-KVDISP-PAS(IX1)      TO  W001-DET4-DLAGERA                  
345700     MOVE ART-PROC-KVDISP-P(IX1)   TO  W001-DET4-P-DLAGERA                
345800     MOVE ART-KVDISP-PAS(IX2)      TO  W001-DET4-DLAGERB                  
345900     MOVE ART-PROC-KVDISP-P(IX2)   TO  W001-DET4-P-DLAGERB                
346000     MOVE ART-KVDISP-PAS(IX3)      TO  W001-DET4-DLAGERC                  
346100     MOVE ART-PROC-KVDISP-P(IX3)   TO  W001-DET4-P-DLAGERC                
346200     MOVE ART-KVDISP-PAS(IX4)      TO  W001-DET4-DLAGERD                  
346300     MOVE ART-PROC-KVDISP-P(IX4)   TO  W001-DET4-P-DLAGERD                
346400     MOVE ART-KVDISP-PAS(IX5)      TO  W001-DET4-DLAGERE                  
346500     MOVE ART-PROC-KVDISP-P(IX5)   TO  W001-DET4-P-DLAGERE                
346600     MOVE ART-KVDISP-PAS(IX6)      TO  W001-DET4-DLAGERF                  
346700     MOVE ART-PROC-KVDISP-P(IX6)   TO  W001-DET4-P-DLAGERF                
346800     MOVE ART-KVDISP-PAS(IX7)      TO  W001-DET4-DLAGERG                  
346900     MOVE ART-PROC-KVDISP-P(IX7)   TO  W001-DET4-P-DLAGERG                
347000     MOVE ART-KVDISP-PAS(IX8)      TO  W001-DET4-DLAGERH                  
347100     MOVE ART-PROC-KVDISP-P(IX8)   TO  W001-DET4-P-DLAGERH                
347200     MOVE PSUM-KVDISP-PAS(PSUM-IX) TO  W001-DET4-TOT                      
347300     MOVE PSUM-PROC-KVDISP-P(PSUM-IX)                                     
347400                                   TO  W001-DET4-P-TOT                    
347500     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
347600     MOVE +1 TO W001-SKIP                                                 
347700     PERFORM S21-SKRIV-LISTA                                              
347800                                                                          
347900     MOVE ART-LS-AKT(IX1)          TO  W001-DET5-LLAGERA                  
348000     MOVE ART-PROC-LS-A(IX1)       TO  W001-DET5-P-LLAGERA                
348100     MOVE ART-LS-AKT(IX2)          TO  W001-DET5-LLAGERB                  
348200     MOVE ART-PROC-LS-A(IX2)       TO  W001-DET5-P-LLAGERB                
348300     MOVE ART-LS-AKT(IX3)          TO  W001-DET5-LLAGERC                  
348400     MOVE ART-PROC-LS-A(IX3)       TO  W001-DET5-P-LLAGERC                
348500     MOVE ART-LS-AKT(IX4)          TO  W001-DET5-LLAGERD                  
348600     MOVE ART-PROC-LS-A(IX4)       TO  W001-DET5-P-LLAGERD                
348700     MOVE ART-LS-AKT(IX5)          TO  W001-DET5-LLAGERE                  
348800     MOVE ART-PROC-LS-A(IX5)       TO  W001-DET5-P-LLAGERE                
348900     MOVE ART-LS-AKT(IX6)          TO  W001-DET5-LLAGERF                  
349000     MOVE ART-PROC-LS-A(IX6)       TO  W001-DET5-P-LLAGERF                
349100     MOVE ART-LS-AKT(IX7)          TO  W001-DET5-LLAGERG                  
349200     MOVE ART-PROC-LS-A(IX7)       TO  W001-DET5-P-LLAGERG                
349300     MOVE ART-LS-AKT(IX8)          TO  W001-DET5-LLAGERH                  
349400     MOVE ART-PROC-LS-A(IX8)       TO  W001-DET5-P-LLAGERH                
349500     MOVE PSUM-LS-AKT(PSUM-IX)     TO  W001-DET5-TOT                      
349600     MOVE PSUM-PROC-LS-A(PSUM-IX)  TO  W001-DET5-P-TOT                    
349700     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
349800     MOVE +1 TO W001-SKIP                                                 
349900     PERFORM S21-SKRIV-LISTA                                              
350000                                                                          
350100     MOVE ART-LS-PAS(IX1)          TO  W001-DET6-LLAGERA                  
350200     MOVE ART-PROC-LS-P(IX1)       TO  W001-DET6-P-LLAGERA                
350300     MOVE ART-LS-PAS(IX2)          TO  W001-DET6-LLAGERB                  
350400     MOVE ART-PROC-LS-P(IX2)       TO  W001-DET6-P-LLAGERB                
350500     MOVE ART-LS-PAS(IX3)          TO  W001-DET6-LLAGERC                  
350600     MOVE ART-PROC-LS-P(IX3)       TO  W001-DET6-P-LLAGERC                
350700     MOVE ART-LS-PAS(IX4)          TO  W001-DET6-LLAGERD                  
350800     MOVE ART-PROC-LS-P(IX4)       TO  W001-DET6-P-LLAGERD                
350900     MOVE ART-LS-PAS(IX5)          TO  W001-DET6-LLAGERE                  
351000     MOVE ART-PROC-LS-P(IX5)       TO  W001-DET6-P-LLAGERE                
351100     MOVE ART-LS-PAS(IX6)          TO  W001-DET6-LLAGERF                  
351200     MOVE ART-PROC-LS-P(IX6)       TO  W001-DET6-P-LLAGERF                
351300     MOVE ART-LS-PAS(IX7)          TO  W001-DET6-LLAGERG                  
351400     MOVE ART-PROC-LS-P(IX7)       TO  W001-DET6-P-LLAGERG                
351500     MOVE ART-LS-PAS(IX8)          TO  W001-DET6-LLAGERH                  
351600     MOVE ART-PROC-LS-P(IX8)       TO  W001-DET6-P-LLAGERH                
351700     MOVE PSUM-LS-PAS(PSUM-IX)     TO  W001-DET6-TOT                      
351800     MOVE PSUM-PROC-LS-P(PSUM-IX)  TO  W001-DET6-P-TOT                    
351900     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
352000     MOVE +1 TO W001-SKIP                                                 
352100     PERFORM S21-SKRIV-LISTA                                              
352200                                                                          
352300     MOVE ART-AK-AKT(IX1)          TO  W001-DET7-ALAGERA                  
352400     MOVE ART-PROC-AK-A(IX1)       TO  W001-DET7-P-ALAGERA                
352500     MOVE ART-AK-AKT(IX2)          TO  W001-DET7-ALAGERB                  
352600     MOVE ART-PROC-AK-A(IX2)       TO  W001-DET7-P-ALAGERB                
352700     MOVE ART-AK-AKT(IX3)          TO  W001-DET7-ALAGERC                  
352800     MOVE ART-PROC-AK-A(IX3)       TO  W001-DET7-P-ALAGERC                
352900     MOVE ART-AK-AKT(IX4)          TO  W001-DET7-ALAGERD                  
353000     MOVE ART-PROC-AK-A(IX4)       TO  W001-DET7-P-ALAGERD                
353100     MOVE ART-AK-AKT(IX5)          TO  W001-DET7-ALAGERE                  
353200     MOVE ART-PROC-AK-A(IX5)       TO  W001-DET7-P-ALAGERE                
353300     MOVE ART-AK-AKT(IX6)          TO  W001-DET7-ALAGERF                  
353400     MOVE ART-PROC-AK-A(IX6)       TO  W001-DET7-P-ALAGERF                
353500     MOVE ART-AK-AKT(IX7)          TO  W001-DET7-ALAGERG                  
353600     MOVE ART-PROC-AK-A(IX7)       TO  W001-DET7-P-ALAGERG                
353700     MOVE ART-AK-AKT(IX8)          TO  W001-DET7-ALAGERH                  
353800     MOVE ART-PROC-AK-A(IX8)       TO  W001-DET7-P-ALAGERH                
353900     MOVE PSUM-AK-AKT(PSUM-IX)     TO  W001-DET7-TOT                      
354000     MOVE PSUM-PROC-AK-A(PSUM-IX)  TO  W001-DET7-P-TOT                    
354100     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
354200     MOVE +1 TO W001-SKIP                                                 
354300     PERFORM S21-SKRIV-LISTA                                              
354400                                                                          
354500     MOVE ART-AK-PAS(IX1)          TO  W001-DET8-ALAGERA                  
354600     MOVE ART-PROC-AK-P(IX1)       TO  W001-DET8-P-ALAGERA                
354700     MOVE ART-AK-PAS(IX2)          TO  W001-DET8-ALAGERB                  
354800     MOVE ART-PROC-AK-P(IX2)       TO  W001-DET8-P-ALAGERB                
354900     MOVE ART-AK-PAS(IX3)          TO  W001-DET8-ALAGERC                  
355000     MOVE ART-PROC-AK-P(IX3)       TO  W001-DET8-P-ALAGERC                
355100     MOVE ART-AK-PAS(IX4)          TO  W001-DET8-ALAGERD                  
355200     MOVE ART-PROC-AK-P(IX4)       TO  W001-DET8-P-ALAGERD                
355300     MOVE ART-AK-PAS(IX5)          TO  W001-DET8-ALAGERE                  
355400     MOVE ART-PROC-AK-P(IX5)       TO  W001-DET8-P-ALAGERE                
355500     MOVE ART-AK-PAS(IX6)          TO  W001-DET8-ALAGERF                  
355600     MOVE ART-PROC-AK-P(IX6)       TO  W001-DET8-P-ALAGERF                
355700     MOVE ART-AK-PAS(IX7)          TO  W001-DET8-ALAGERG                  
355800     MOVE ART-PROC-AK-P(IX7)       TO  W001-DET8-P-ALAGERG                
355900     MOVE ART-AK-PAS(IX8)          TO  W001-DET8-ALAGERH                  
356000     MOVE ART-PROC-AK-P(IX8)       TO  W001-DET8-P-ALAGERH                
356100     MOVE PSUM-AK-PAS(PSUM-IX)     TO  W001-DET8-TOT                      
356200     MOVE PSUM-PROC-AK-P(PSUM-IX)  TO  W001-DET8-P-TOT                    
356300     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
356400     MOVE +1 TO W001-SKIP                                                 
356500     PERFORM S21-SKRIV-LISTA                                              
356600                                                                          
356700     MOVE ART-OLAGER(IX1)          TO  W001-DET9-OLAGERA                  
356800     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET9-P-OLAGERA                
356900     MOVE ART-OLAGER(IX2)          TO  W001-DET9-OLAGERB                  
357000     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET9-P-OLAGERB                
357100     MOVE ART-OLAGER(IX3)          TO  W001-DET9-OLAGERC                  
357200     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET9-P-OLAGERC                
357300     MOVE ART-OLAGER(IX4)          TO  W001-DET9-OLAGERD                  
357400     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET9-P-OLAGERD                
357500     MOVE ART-OLAGER(IX5)          TO  W001-DET9-OLAGERE                  
357600     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET9-P-OLAGERE                
357700     MOVE ART-OLAGER(IX6)          TO  W001-DET9-OLAGERF                  
357800     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET9-P-OLAGERF                
357900     MOVE ART-OLAGER(IX7)          TO  W001-DET9-OLAGERG                  
358000     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET9-P-OLAGERG                
358100     MOVE ART-OLAGER(IX8)          TO  W001-DET9-OLAGERH                  
358200     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET9-P-OLAGERH                
358300     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET9-TOT                      
358400     MOVE PSUM-PROC-OLAGER(PSUM-IX)                                       
358500                                   TO  W001-DET9-P-TOT                    
358600     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
358700     MOVE +1 TO W001-SKIP                                                 
358800     PERFORM S21-SKRIV-LISTA                                              
358900                                                                          
359000     MOVE ART-SLAGER(IX1)          TO  W001-DET10-SLAGERA                 
359100     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET10-P-SLAGERA               
359200     MOVE ART-SLAGER(IX2)          TO  W001-DET10-SLAGERB                 
359300     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET10-P-SLAGERB               
359400     MOVE ART-SLAGER(IX3)          TO  W001-DET10-SLAGERC                 
359500     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET10-P-SLAGERC               
359600     MOVE ART-SLAGER(IX4)          TO  W001-DET10-SLAGERD                 
359700     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET10-P-SLAGERD               
359800     MOVE ART-SLAGER(IX5)          TO  W001-DET10-SLAGERE                 
359900     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET10-P-SLAGERE               
360000     MOVE ART-SLAGER(IX6)          TO  W001-DET10-SLAGERF                 
360100     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET10-P-SLAGERF               
360200     MOVE ART-SLAGER(IX7)          TO  W001-DET10-SLAGERG                 
360300     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET10-P-SLAGERG               
360400     MOVE ART-SLAGER(IX8)          TO  W001-DET10-SLAGERH                 
360500     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET10-P-SLAGERH               
360600     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET10-TOT                     
360700     MOVE PSUM-PROC-SLAGER(PSUM-IX)                                       
360800                                   TO  W001-DET10-P-TOT                   
360900     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
361000     MOVE +1 TO W001-SKIP                                                 
361100     PERFORM S21-SKRIV-LISTA                                              
361200                                                                          
361300     MOVE ART-MLAGER(IX1)          TO  W001-DET11-MLAGERA                 
361400     MOVE ART-PROC-MLAGER(IX1)     TO  W001-DET11-P-MLAGERA               
361500     MOVE ART-MLAGER(IX2)          TO  W001-DET11-MLAGERB                 
361600     MOVE ART-PROC-MLAGER(IX2)     TO  W001-DET11-P-MLAGERB               
361700     MOVE ART-MLAGER(IX3)          TO  W001-DET11-MLAGERC                 
361800     MOVE ART-PROC-MLAGER(IX3)     TO  W001-DET11-P-MLAGERC               
361900     MOVE ART-MLAGER(IX4)          TO  W001-DET11-MLAGERD                 
362000     MOVE ART-PROC-MLAGER(IX4)     TO  W001-DET11-P-MLAGERD               
362100     MOVE ART-MLAGER(IX5)          TO  W001-DET11-MLAGERE                 
362200     MOVE ART-PROC-MLAGER(IX5)     TO  W001-DET11-P-MLAGERE               
362300     MOVE ART-MLAGER(IX6)          TO  W001-DET11-MLAGERF                 
362400     MOVE ART-PROC-MLAGER(IX6)     TO  W001-DET11-P-MLAGERF               
362500     MOVE ART-MLAGER(IX7)          TO  W001-DET11-MLAGERG                 
362600     MOVE ART-PROC-MLAGER(IX7)     TO  W001-DET11-P-MLAGERG               
362700     MOVE ART-MLAGER(IX8)          TO  W001-DET11-MLAGERH                 
362800     MOVE ART-PROC-MLAGER(IX8)     TO  W001-DET11-P-MLAGERH               
362900     MOVE PSUM-MLAGER(PSUM-IX)     TO  W001-DET11-TOT                     
363000     MOVE PSUM-PROC-MLAGER(PSUM-IX)                                       
363100                                   TO  W001-DET11-P-TOT                   
363200     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
363300     MOVE +1 TO W001-SKIP                                                 
363400     PERFORM S21-SKRIV-LISTA                                              
363500                                                                          
363600     MOVE ART-KVOT(IX1)            TO  W001-DET12-KVOTA                   
363700     MOVE ART-PROC-KVOT(IX1)       TO  W001-DET12-P-KVOTA                 
363800     MOVE ART-KVOT(IX2)            TO  W001-DET12-KVOTB                   
363900     MOVE ART-PROC-KVOT(IX2)       TO  W001-DET12-P-KVOTB                 
364000     MOVE ART-KVOT(IX3)            TO  W001-DET12-KVOTC                   
364100     MOVE ART-PROC-KVOT(IX3)       TO  W001-DET12-P-KVOTC                 
364200     MOVE ART-KVOT(IX4)            TO  W001-DET12-KVOTD                   
364300     MOVE ART-PROC-KVOT(IX4)       TO  W001-DET12-P-KVOTD                 
364400     MOVE ART-KVOT(IX5)            TO  W001-DET12-KVOTE                   
364500     MOVE ART-PROC-KVOT(IX5)       TO  W001-DET12-P-KVOTE                 
364600     MOVE ART-KVOT(IX6)            TO  W001-DET12-KVOTF                   
364700     MOVE ART-PROC-KVOT(IX6)       TO  W001-DET12-P-KVOTF                 
364800     MOVE ART-KVOT(IX7)            TO  W001-DET12-KVOTG                   
364900     MOVE ART-PROC-KVOT(IX7)       TO  W001-DET12-P-KVOTG                 
365000     MOVE ART-KVOT(IX8)            TO  W001-DET12-KVOTH                   
365100     MOVE ART-PROC-KVOT(IX8)       TO  W001-DET12-P-KVOTH                 
365200     MOVE PSUM-KVOT(PSUM-IX)       TO  W001-DET12-TOT                     
365300     MOVE PSUM-PROC-KVOT(PSUM-IX)  TO  W001-DET12-P-TOT                   
365400     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
365500     MOVE +1 TO W001-SKIP                                                 
365600     PERFORM S21-SKRIV-LISTA                                              
365700                                                                          
365800     MOVE ART-SPLIT(IX1)           TO  W001-DET13-SPLITA                  
365900     MOVE ART-SPLIT(IX2)           TO  W001-DET13-SPLITB                  
366000     MOVE ART-SPLIT(IX3)           TO  W001-DET13-SPLITC                  
366100     MOVE ART-SPLIT(IX4)           TO  W001-DET13-SPLITD                  
366200     MOVE ART-SPLIT(IX5)           TO  W001-DET13-SPLITE                  
366300     MOVE ART-SPLIT(IX6)           TO  W001-DET13-SPLITF                  
366400     MOVE ART-SPLIT(IX7)           TO  W001-DET13-SPLITG                  
366500     MOVE ART-SPLIT(IX8)           TO  W001-DET13-SPLITH                  
366600     MOVE PSUM-SPLIT(PSUM-IX)      TO  W001-DET13-TOT                     
366700     MOVE ZERO                     TO  W001-DET13-P-TOT                   
366800     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
366900     MOVE +1 TO W001-SKIP                                                 
367000     PERFORM S21-SKRIV-LISTA                                              
367100                                                                          
367200     MOVE ART-OMSHAST-DISP(IX1)    TO  W001-DET14-OMSHASTA                
367300     MOVE ART-OMSHAST-PROC-D(IX1)  TO  W001-DET14-P-OMSHASTA              
367400     MOVE ART-OMSHAST-DISP(IX2)    TO  W001-DET14-OMSHASTB                
367500     MOVE ART-OMSHAST-PROC-D(IX2)  TO  W001-DET14-P-OMSHASTB              
367600     MOVE ART-OMSHAST-DISP(IX3)    TO  W001-DET14-OMSHASTC                
367700     MOVE ART-OMSHAST-PROC-D(IX3)  TO  W001-DET14-P-OMSHASTC              
367800     MOVE ART-OMSHAST-DISP(IX4)    TO  W001-DET14-OMSHASTD                
367900     MOVE ART-OMSHAST-PROC-D(IX4)  TO  W001-DET14-P-OMSHASTD              
368000     MOVE ART-OMSHAST-DISP(IX5)    TO  W001-DET14-OMSHASTE                
368100     MOVE ART-OMSHAST-PROC-D(IX5)  TO  W001-DET14-P-OMSHASTE              
368200     MOVE ART-OMSHAST-DISP(IX6)    TO  W001-DET14-OMSHASTF                
368300     MOVE ART-OMSHAST-PROC-D(IX6)  TO  W001-DET14-P-OMSHASTF              
368400     MOVE ART-OMSHAST-DISP(IX7)    TO  W001-DET14-OMSHASTG                
368500     MOVE ART-OMSHAST-PROC-D(IX7)  TO  W001-DET14-P-OMSHASTG              
368600     MOVE ART-OMSHAST-DISP(IX8)    TO  W001-DET14-OMSHASTH                
368700     MOVE ART-OMSHAST-PROC-D(IX8)  TO  W001-DET14-P-OMSHASTH              
368800     MOVE PSUM-OMSHAST-DISP(PSUM-IX) TO W001-DET14-TOT                    
368900     MOVE PSUM-OMSHAST-PROC-D(PSUM-IX)                                    
369000                                     TO W001-DET14-P-TOT                  
369100     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
369200     MOVE +1 TO W001-SKIP                                                 
369300     PERFORM S21-SKRIV-LISTA                                              
369400                                                                          
369500     MOVE ART-OMSHAST-LS(IX1)      TO  W001-DET15-OMSHASTA                
369600     MOVE ART-OMSHAST-PROC-LS(IX1) TO  W001-DET15-P-OMSHASTA              
369700     MOVE ART-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB                
369800     MOVE ART-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB              
369900     MOVE ART-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC                
370000     MOVE ART-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC              
370100     MOVE ART-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD                
370200     MOVE ART-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD              
370300     MOVE ART-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE                
370400     MOVE ART-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE              
370500     MOVE ART-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF                
370600     MOVE ART-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF              
370700     MOVE ART-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG                
370800     MOVE ART-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG              
370900     MOVE ART-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH                
371000     MOVE ART-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH              
371100     MOVE PSUM-OMSHAST-LS(PSUM-IX) TO  W001-DET15-TOT                     
371200     MOVE PSUM-OMSHAST-PROC-LS(PSUM-IX)                                   
371300                                     TO W001-DET15-P-TOT                  
371400     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
371500     MOVE +1 TO W001-SKIP                                                 
371600     PERFORM S21-SKRIV-LISTA                                              
371700                                                                          
371800     MOVE ART-SERVG-AKT(IX1)       TO  W001-DET16-SERVGA-A                
371900     MOVE ART-SERVG-PAS(IX1)       TO  W001-DET16-SERVGA-P                
372000     MOVE ART-SERVG-AKT(IX2)       TO  W001-DET16-SERVGB-A                
372100     MOVE ART-SERVG-PAS(IX2)       TO  W001-DET16-SERVGB-P                
372200     MOVE ART-SERVG-AKT(IX3)       TO  W001-DET16-SERVGC-A                
372300     MOVE ART-SERVG-PAS(IX3)       TO  W001-DET16-SERVGC-P                
372400     MOVE ART-SERVG-AKT(IX4)       TO  W001-DET16-SERVGD-A                
372500     MOVE ART-SERVG-PAS(IX4)       TO  W001-DET16-SERVGD-P                
372600     MOVE ART-SERVG-AKT(IX5)       TO  W001-DET16-SERVGE-A                
372700     MOVE ART-SERVG-PAS(IX5)       TO  W001-DET16-SERVGE-P                
372800     MOVE ART-SERVG-AKT(IX6)       TO  W001-DET16-SERVGF-A                
372900     MOVE ART-SERVG-PAS(IX6)       TO  W001-DET16-SERVGF-P                
373000     MOVE ART-SERVG-AKT(IX7)       TO  W001-DET16-SERVGG-A                
373100     MOVE ART-SERVG-PAS(IX7)       TO  W001-DET16-SERVGG-P                
373200     MOVE ART-SERVG-AKT(IX8)       TO  W001-DET16-SERVGH-A                
373300     MOVE ART-SERVG-PAS(IX8)       TO  W001-DET16-SERVGH-P                
373400     MOVE PSUM-SERVG-AKT (PSUM-IX) TO  W001-DET16-TOT                     
373500     MOVE PSUM-SERVG-PAS                                                  
373600          (PSUM-IX)                TO  W001-DET16-P-TOT                   
373700     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
373800     MOVE +1 TO W001-SKIP                                                 
373900     PERFORM S21-SKRIV-LISTA                                              
374000                                                                          
374100     MOVE ART-SERVG-TOT(IX1)       TO  W001-DET17-SERVGA-TOT              
374200     MOVE ART-SERVG-TOT(IX2)       TO  W001-DET17-SERVGB-TOT              
374300     MOVE ART-SERVG-TOT(IX3)       TO  W001-DET17-SERVGC-TOT              
374400     MOVE ART-SERVG-TOT(IX4)       TO  W001-DET17-SERVGD-TOT              
374500     MOVE ART-SERVG-TOT(IX5)       TO  W001-DET17-SERVGE-TOT              
374600     MOVE ART-SERVG-TOT(IX6)       TO  W001-DET17-SERVGF-TOT              
374700     MOVE ART-SERVG-TOT(IX7)       TO  W001-DET17-SERVGG-TOT              
374800     MOVE ART-SERVG-TOT(IX8)       TO  W001-DET17-SERVGH-TOT              
374900     MOVE PSUM-SERVG-TOT(PSUM-IX)  TO  W001-DET17-TOT                     
375000     MOVE ZERO                     TO  W001-DET17-P-TOT                   
375100     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
375200     MOVE +1 TO W001-SKIP                                                 
375300     PERFORM S21-SKRIV-LISTA                                              
375400                                                                          
375500     MOVE ART-SERVG-TEO(IX1)       TO  W001-DET18-SERVGA-TEO              
375600     MOVE ART-SERVG-TEO(IX2)       TO  W001-DET18-SERVGB-TEO              
375700     MOVE ART-SERVG-TEO(IX3)       TO  W001-DET18-SERVGC-TEO              
375800     MOVE ART-SERVG-TEO(IX4)       TO  W001-DET18-SERVGD-TEO              
375900     MOVE ART-SERVG-TEO(IX5)       TO  W001-DET18-SERVGE-TEO              
376000     MOVE ART-SERVG-TEO(IX6)       TO  W001-DET18-SERVGF-TEO              
376100     MOVE ART-SERVG-TEO(IX7)       TO  W001-DET18-SERVGG-TEO              
376200     MOVE ART-SERVG-TEO(IX8)       TO  W001-DET18-SERVGH-TEO              
376300     MOVE PSUM-SERVG-TEO(PSUM-IX)  TO  W001-DET18-TOT                     
376400     MOVE ZERO                     TO  W001-DET18-P-TOT                   
376500     MOVE W001-DETALJRAD-18 TO W001-RAD                                   
376600     MOVE +1 TO W001-SKIP                                                 
376700     PERFORM S21-SKRIV-LISTA                                              
376800     .                                                                    
376900     EJECT                                                                
377000 CB-FLYTTA-SKRIV-TOT SECTION.                                             
377100                                                                          
377200     MOVE FSUM-KVANT-AKT(IX1)      TO  W001-DET1-KVANTA                   
377300     MOVE FSUM-PROC-KVANT-A(IX1)   TO  W001-DET1-P-KVANTA                 
377400     MOVE FSUM-KVANT-AKT(IX2)      TO  W001-DET1-KVANTB                   
377500     MOVE FSUM-PROC-KVANT-A(IX2)   TO  W001-DET1-P-KVANTB                 
377600     MOVE FSUM-KVANT-AKT(IX3)      TO  W001-DET1-KVANTC                   
377700     MOVE FSUM-PROC-KVANT-A(IX3)   TO  W001-DET1-P-KVANTC                 
377800     MOVE FSUM-KVANT-AKT(IX4)      TO  W001-DET1-KVANTD                   
377900     MOVE FSUM-PROC-KVANT-A(IX4)   TO  W001-DET1-P-KVANTD                 
378000     MOVE FSUM-KVANT-AKT(IX5)      TO  W001-DET1-KVANTE                   
378100     MOVE FSUM-PROC-KVANT-A(IX5)   TO  W001-DET1-P-KVANTE                 
378200     MOVE FSUM-KVANT-AKT(IX6)      TO  W001-DET1-KVANTF                   
378300     MOVE FSUM-PROC-KVANT-A(IX6)   TO  W001-DET1-P-KVANTF                 
378400     MOVE FSUM-KVANT-AKT(IX7)      TO  W001-DET1-KVANTG                   
378500     MOVE FSUM-PROC-KVANT-A(IX7)   TO  W001-DET1-P-KVANTG                 
378600     MOVE FSUM-KVANT-AKT(IX8)      TO  W001-DET1-KVANTH                   
378700     MOVE FSUM-PROC-KVANT-A(IX8)   TO  W001-DET1-P-KVANTH                 
378800     MOVE TOT-KVANT-AKT            TO  W001-DET1-TOT                      
378900     MOVE ZERO                     TO  W001-DET1-P-TOT                    
379000     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
379100     MOVE +2 TO W001-SKIP                                                 
379200     PERFORM S21-SKRIV-LISTA                                              
379300                                                                          
379400     MOVE FSUM-KVANT-PAS(IX1)      TO  W001-DET2-KVANTA                   
379500     MOVE FSUM-PROC-KVANT-P(IX1)   TO  W001-DET2-P-KVANTA                 
379600     MOVE FSUM-KVANT-PAS(IX2)      TO  W001-DET2-KVANTB                   
379700     MOVE FSUM-PROC-KVANT-P(IX2)   TO  W001-DET2-P-KVANTB                 
379800     MOVE FSUM-KVANT-PAS(IX3)      TO  W001-DET2-KVANTC                   
379900     MOVE FSUM-PROC-KVANT-P(IX3)   TO  W001-DET2-P-KVANTC                 
380000     MOVE FSUM-KVANT-PAS(IX4)      TO  W001-DET2-KVANTD                   
380100     MOVE FSUM-PROC-KVANT-P(IX4)   TO  W001-DET2-P-KVANTD                 
380200     MOVE FSUM-KVANT-PAS(IX5)      TO  W001-DET2-KVANTE                   
380300     MOVE FSUM-PROC-KVANT-P(IX5)   TO  W001-DET2-P-KVANTE                 
380400     MOVE FSUM-KVANT-PAS(IX6)      TO  W001-DET2-KVANTF                   
380500     MOVE FSUM-PROC-KVANT-P(IX6)   TO  W001-DET2-P-KVANTF                 
380600     MOVE FSUM-KVANT-PAS(IX7)      TO  W001-DET2-KVANTG                   
380700     MOVE FSUM-PROC-KVANT-P(IX7)   TO  W001-DET2-P-KVANTG                 
380800     MOVE FSUM-KVANT-PAS(IX8)      TO  W001-DET2-KVANTH                   
380900     MOVE FSUM-PROC-KVANT-P(IX8)   TO  W001-DET2-P-KVANTH                 
381000     MOVE TOT-KVANT-PAS            TO  W001-DET2-TOT                      
381100     MOVE ZERO                     TO  W001-DET2-P-TOT                    
381200     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
381300     MOVE +1 TO W001-SKIP                                                 
381400     PERFORM S21-SKRIV-LISTA                                              
381500                                                                          
381600     MOVE FSUM-KVDISP-AKT(IX1)     TO  W001-DET3-DLAGERA                  
381700     MOVE FSUM-PROC-KVDISP-A(IX1)  TO  W001-DET3-P-DLAGERA                
381800     MOVE FSUM-KVDISP-AKT(IX2)     TO  W001-DET3-DLAGERB                  
381900     MOVE FSUM-PROC-KVDISP-A(IX2)  TO  W001-DET3-P-DLAGERB                
382000     MOVE FSUM-KVDISP-AKT(IX3)     TO  W001-DET3-DLAGERC                  
382100     MOVE FSUM-PROC-KVDISP-A(IX3)  TO  W001-DET3-P-DLAGERC                
382200     MOVE FSUM-KVDISP-AKT(IX4)     TO  W001-DET3-DLAGERD                  
382300     MOVE FSUM-PROC-KVDISP-A(IX4)  TO  W001-DET3-P-DLAGERD                
382400     MOVE FSUM-KVDISP-AKT(IX5)     TO  W001-DET3-DLAGERE                  
382500     MOVE FSUM-PROC-KVDISP-A(IX5)  TO  W001-DET3-P-DLAGERE                
382600     MOVE FSUM-KVDISP-AKT(IX6)     TO  W001-DET3-DLAGERF                  
382700     MOVE FSUM-PROC-KVDISP-A(IX6)  TO  W001-DET3-P-DLAGERF                
382800     MOVE FSUM-KVDISP-AKT(IX7)     TO  W001-DET3-DLAGERG                  
382900     MOVE FSUM-PROC-KVDISP-A(IX7)  TO  W001-DET3-P-DLAGERG                
383000     MOVE FSUM-KVDISP-AKT(IX8)     TO  W001-DET3-DLAGERH                  
383100     MOVE FSUM-PROC-KVDISP-A(IX8)  TO  W001-DET3-P-DLAGERH                
383200     MOVE TOT-KVDISP-AKT           TO  W001-DET3-TOT                      
383300     MOVE ZERO                     TO  W001-DET3-P-TOT                    
383400     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
383500     MOVE +1 TO W001-SKIP                                                 
383600     PERFORM S21-SKRIV-LISTA                                              
383700                                                                          
383800     MOVE FSUM-KVDISP-PAS(IX1)     TO  W001-DET4-DLAGERA                  
383900     MOVE FSUM-PROC-KVDISP-P(IX1)  TO  W001-DET4-P-DLAGERA                
384000     MOVE FSUM-KVDISP-PAS(IX2)     TO  W001-DET4-DLAGERB                  
384100     MOVE FSUM-PROC-KVDISP-P(IX2)  TO  W001-DET4-P-DLAGERB                
384200     MOVE FSUM-KVDISP-PAS(IX3)     TO  W001-DET4-DLAGERC                  
384300     MOVE FSUM-PROC-KVDISP-P(IX3)  TO  W001-DET4-P-DLAGERC                
384400     MOVE FSUM-KVDISP-PAS(IX4)     TO  W001-DET4-DLAGERD                  
384500     MOVE FSUM-PROC-KVDISP-P(IX4)  TO  W001-DET4-P-DLAGERD                
384600     MOVE FSUM-KVDISP-PAS(IX5)     TO  W001-DET4-DLAGERE                  
384700     MOVE FSUM-PROC-KVDISP-P(IX5)  TO  W001-DET4-P-DLAGERE                
384800     MOVE FSUM-KVDISP-PAS(IX6)     TO  W001-DET4-DLAGERF                  
384900     MOVE FSUM-PROC-KVDISP-P(IX6)  TO  W001-DET4-P-DLAGERF                
385000     MOVE FSUM-KVDISP-PAS(IX7)     TO  W001-DET4-DLAGERG                  
385100     MOVE FSUM-PROC-KVDISP-P(IX7)  TO  W001-DET4-P-DLAGERG                
385200     MOVE FSUM-KVDISP-PAS(IX8)     TO  W001-DET4-DLAGERH                  
385300     MOVE FSUM-PROC-KVDISP-P(IX8)  TO  W001-DET4-P-DLAGERH                
385400     MOVE TOT-KVDISP-PAS           TO  W001-DET4-TOT                      
385500     MOVE ZERO                     TO  W001-DET4-P-TOT                    
385600     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
385700     MOVE +1 TO W001-SKIP                                                 
385800     PERFORM S21-SKRIV-LISTA                                              
385900                                                                          
386000     MOVE FSUM-LS-AKT(IX1)         TO  W001-DET5-LLAGERA                  
386100     MOVE FSUM-PROC-LS-A(IX1)      TO  W001-DET5-P-LLAGERA                
386200     MOVE FSUM-LS-AKT(IX2)         TO  W001-DET5-LLAGERB                  
386300     MOVE FSUM-PROC-LS-A(IX2)      TO  W001-DET5-P-LLAGERB                
386400     MOVE FSUM-LS-AKT(IX3)         TO  W001-DET5-LLAGERC                  
386500     MOVE FSUM-PROC-LS-A(IX3)      TO  W001-DET5-P-LLAGERC                
386600     MOVE FSUM-LS-AKT(IX4)         TO  W001-DET5-LLAGERD                  
386700     MOVE FSUM-PROC-LS-A(IX4)      TO  W001-DET5-P-LLAGERD                
386800     MOVE FSUM-LS-AKT(IX5)         TO  W001-DET5-LLAGERE                  
386900     MOVE FSUM-PROC-LS-A(IX5)      TO  W001-DET5-P-LLAGERE                
387000     MOVE FSUM-LS-AKT(IX6)         TO  W001-DET5-LLAGERF                  
387100     MOVE FSUM-PROC-LS-A(IX6)      TO  W001-DET5-P-LLAGERF                
387200     MOVE FSUM-LS-AKT(IX7)         TO  W001-DET5-LLAGERG                  
387300     MOVE FSUM-PROC-LS-A(IX7)      TO  W001-DET5-P-LLAGERG                
387400     MOVE FSUM-LS-AKT(IX8)         TO  W001-DET5-LLAGERH                  
387500     MOVE FSUM-PROC-LS-A(IX8)      TO  W001-DET5-P-LLAGERH                
387600     MOVE TOT-LS-AKT               TO  W001-DET5-TOT                      
387700     MOVE ZERO                     TO  W001-DET5-P-TOT                    
387800     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
387900     MOVE +1 TO W001-SKIP                                                 
388000     PERFORM S21-SKRIV-LISTA                                              
388100                                                                          
388200     MOVE FSUM-LS-PAS(IX1)         TO  W001-DET6-LLAGERA                  
388300     MOVE FSUM-PROC-LS-P(IX1)      TO  W001-DET6-P-LLAGERA                
388400     MOVE FSUM-LS-PAS(IX2)         TO  W001-DET6-LLAGERB                  
388500     MOVE FSUM-PROC-LS-P(IX2)      TO  W001-DET6-P-LLAGERB                
388600     MOVE FSUM-LS-PAS(IX3)         TO  W001-DET6-LLAGERC                  
388700     MOVE FSUM-PROC-LS-P(IX3)      TO  W001-DET6-P-LLAGERC                
388800     MOVE FSUM-LS-PAS(IX4)         TO  W001-DET6-LLAGERD                  
388900     MOVE FSUM-PROC-LS-P(IX4)      TO  W001-DET6-P-LLAGERD                
389000     MOVE FSUM-LS-PAS(IX5)         TO  W001-DET6-LLAGERE                  
389100     MOVE FSUM-PROC-LS-P(IX5)      TO  W001-DET6-P-LLAGERE                
389200     MOVE FSUM-LS-PAS(IX6)         TO  W001-DET6-LLAGERF                  
389300     MOVE FSUM-PROC-LS-P(IX6)      TO  W001-DET6-P-LLAGERF                
389400     MOVE FSUM-LS-PAS(IX7)         TO  W001-DET6-LLAGERG                  
389500     MOVE FSUM-PROC-LS-P(IX7)      TO  W001-DET6-P-LLAGERG                
389600     MOVE FSUM-LS-PAS(IX8)         TO  W001-DET6-LLAGERH                  
389700     MOVE FSUM-PROC-LS-P(IX8)      TO  W001-DET6-P-LLAGERH                
389800     MOVE TOT-LS-PAS               TO  W001-DET6-TOT                      
389900     MOVE ZERO                     TO  W001-DET6-P-TOT                    
390000     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
390100     MOVE +1 TO W001-SKIP                                                 
390200     PERFORM S21-SKRIV-LISTA                                              
390300                                                                          
390400     MOVE FSUM-AK-AKT(IX1)         TO  W001-DET7-ALAGERA                  
390500     MOVE FSUM-PROC-AK-A(IX1)      TO  W001-DET7-P-ALAGERA                
390600     MOVE FSUM-AK-AKT(IX2)         TO  W001-DET7-ALAGERB                  
390700     MOVE FSUM-PROC-AK-A(IX2)      TO  W001-DET7-P-ALAGERB                
390800     MOVE FSUM-AK-AKT(IX3)         TO  W001-DET7-ALAGERC                  
390900     MOVE FSUM-PROC-AK-A(IX3)      TO  W001-DET7-P-ALAGERC                
391000     MOVE FSUM-AK-AKT(IX4)         TO  W001-DET7-ALAGERD                  
391100     MOVE FSUM-PROC-AK-A(IX4)      TO  W001-DET7-P-ALAGERD                
391200     MOVE FSUM-AK-AKT(IX5)         TO  W001-DET7-ALAGERE                  
391300     MOVE FSUM-PROC-AK-A(IX5)      TO  W001-DET7-P-ALAGERE                
391400     MOVE FSUM-AK-AKT(IX6)         TO  W001-DET7-ALAGERF                  
391500     MOVE FSUM-PROC-AK-A(IX6)      TO  W001-DET7-P-ALAGERF                
391600     MOVE FSUM-AK-AKT(IX7)         TO  W001-DET7-ALAGERG                  
391700     MOVE FSUM-PROC-AK-A(IX7)      TO  W001-DET7-P-ALAGERG                
391800     MOVE FSUM-AK-AKT(IX8)         TO  W001-DET7-ALAGERH                  
391900     MOVE FSUM-PROC-AK-A(IX8)      TO  W001-DET7-P-ALAGERH                
392000     MOVE TOT-AK-AKT               TO  W001-DET7-TOT                      
392100     MOVE ZERO                     TO  W001-DET7-P-TOT                    
392200     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
392300     MOVE +1 TO W001-SKIP                                                 
392400     PERFORM S21-SKRIV-LISTA                                              
392500                                                                          
392600     MOVE FSUM-AK-PAS(IX1)         TO  W001-DET8-ALAGERA                  
392700     MOVE FSUM-PROC-AK-P(IX1)      TO  W001-DET8-P-ALAGERA                
392800     MOVE FSUM-AK-PAS(IX2)         TO  W001-DET8-ALAGERB                  
392900     MOVE FSUM-PROC-AK-P(IX2)      TO  W001-DET8-P-ALAGERB                
393000     MOVE FSUM-AK-PAS(IX3)         TO  W001-DET8-ALAGERC                  
393100     MOVE FSUM-PROC-AK-P(IX3)      TO  W001-DET8-P-ALAGERC                
393200     MOVE FSUM-AK-PAS(IX4)         TO  W001-DET8-ALAGERD                  
393300     MOVE FSUM-PROC-AK-P(IX4)      TO  W001-DET8-P-ALAGERD                
393400     MOVE FSUM-AK-PAS(IX5)         TO  W001-DET8-ALAGERE                  
393500     MOVE FSUM-PROC-AK-P(IX5)      TO  W001-DET8-P-ALAGERE                
393600     MOVE FSUM-AK-PAS(IX6)         TO  W001-DET8-ALAGERF                  
393700     MOVE FSUM-PROC-AK-P(IX6)      TO  W001-DET8-P-ALAGERF                
393800     MOVE FSUM-AK-PAS(IX7)         TO  W001-DET8-ALAGERG                  
393900     MOVE FSUM-PROC-AK-P(IX7)      TO  W001-DET8-P-ALAGERG                
394000     MOVE FSUM-AK-PAS(IX8)         TO  W001-DET8-ALAGERH                  
394100     MOVE FSUM-PROC-AK-P(IX8)      TO  W001-DET8-P-ALAGERH                
394200     MOVE TOT-AK-PAS               TO  W001-DET8-TOT                      
394300     MOVE ZERO                     TO  W001-DET8-P-TOT                    
394400     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
394500     MOVE +1 TO W001-SKIP                                                 
394600     PERFORM S21-SKRIV-LISTA                                              
394700                                                                          
394800     MOVE FSUM-OLAGER(IX1)         TO  W001-DET9-OLAGERA                  
394900     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET9-P-OLAGERA                
395000     MOVE FSUM-OLAGER(IX2)         TO  W001-DET9-OLAGERB                  
395100     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET9-P-OLAGERB                
395200     MOVE FSUM-OLAGER(IX3)         TO  W001-DET9-OLAGERC                  
395300     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET9-P-OLAGERC                
395400     MOVE FSUM-OLAGER(IX4)         TO  W001-DET9-OLAGERD                  
395500     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET9-P-OLAGERD                
395600     MOVE FSUM-OLAGER(IX5)         TO  W001-DET9-OLAGERE                  
395700     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET9-P-OLAGERE                
395800     MOVE FSUM-OLAGER(IX6)         TO  W001-DET9-OLAGERF                  
395900     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET9-P-OLAGERF                
396000     MOVE FSUM-OLAGER(IX7)         TO  W001-DET9-OLAGERG                  
396100     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET9-P-OLAGERG                
396200     MOVE FSUM-OLAGER(IX8)         TO  W001-DET9-OLAGERH                  
396300     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET9-P-OLAGERH                
396400     MOVE TOT-OLAGER               TO  W001-DET9-TOT                      
396500     MOVE TOT-PROC-OLAGER          TO  W001-DET9-P-TOT                    
396600     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
396700     MOVE +1 TO W001-SKIP                                                 
396800     PERFORM S21-SKRIV-LISTA                                              
396900                                                                          
397000     MOVE FSUM-SLAGER(IX1)         TO  W001-DET10-SLAGERA                 
397100     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET10-P-SLAGERA               
397200     MOVE FSUM-SLAGER(IX2)         TO  W001-DET10-SLAGERB                 
397300     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET10-P-SLAGERB               
397400     MOVE FSUM-SLAGER(IX3)         TO  W001-DET10-SLAGERC                 
397500     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET10-P-SLAGERC               
397600     MOVE FSUM-SLAGER(IX4)         TO  W001-DET10-SLAGERD                 
397700     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET10-P-SLAGERD               
397800     MOVE FSUM-SLAGER(IX5)         TO  W001-DET10-SLAGERE                 
397900     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET10-P-SLAGERE               
398000     MOVE FSUM-SLAGER(IX6)         TO  W001-DET10-SLAGERF                 
398100     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET10-P-SLAGERF               
398200     MOVE FSUM-SLAGER(IX7)         TO  W001-DET10-SLAGERG                 
398300     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET10-P-SLAGERG               
398400     MOVE FSUM-SLAGER(IX8)         TO  W001-DET10-SLAGERH                 
398500     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET10-P-SLAGERH               
398600     MOVE TOT-SLAGER               TO  W001-DET10-TOT                     
398700     MOVE TOT-PROC-SLAGER          TO  W001-DET10-P-TOT                   
398800     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
398900     MOVE +1 TO W001-SKIP                                                 
399000     PERFORM S21-SKRIV-LISTA                                              
399100                                                                          
399200     MOVE FSUM-MLAGER(IX1)         TO  W001-DET11-MLAGERA                 
399300     MOVE FSUM-PROC-MLAGER(IX1)    TO  W001-DET11-P-MLAGERA               
399400     MOVE FSUM-MLAGER(IX2)         TO  W001-DET11-MLAGERB                 
399500     MOVE FSUM-PROC-MLAGER(IX2)    TO  W001-DET11-P-MLAGERB               
399600     MOVE FSUM-MLAGER(IX3)         TO  W001-DET11-MLAGERC                 
399700     MOVE FSUM-PROC-MLAGER(IX3)    TO  W001-DET11-P-MLAGERC               
399800     MOVE FSUM-MLAGER(IX4)         TO  W001-DET11-MLAGERD                 
399900     MOVE FSUM-PROC-MLAGER(IX4)    TO  W001-DET11-P-MLAGERD               
400000     MOVE FSUM-MLAGER(IX5)         TO  W001-DET11-MLAGERE                 
400100     MOVE FSUM-PROC-MLAGER(IX5)    TO  W001-DET11-P-MLAGERE               
400200     MOVE FSUM-MLAGER(IX6)         TO  W001-DET11-MLAGERF                 
400300     MOVE FSUM-PROC-MLAGER(IX6)    TO  W001-DET11-P-MLAGERF               
400400     MOVE FSUM-MLAGER(IX7)         TO  W001-DET11-MLAGERG                 
400500     MOVE FSUM-PROC-MLAGER(IX7)    TO  W001-DET11-P-MLAGERG               
400600     MOVE FSUM-MLAGER(IX8)         TO  W001-DET11-MLAGERH                 
400700     MOVE FSUM-PROC-MLAGER(IX8)    TO  W001-DET11-P-MLAGERH               
400800     MOVE TOT-MLAGER               TO  W001-DET11-TOT                     
400900     MOVE TOT-PROC-MLAGER          TO  W001-DET11-P-TOT                   
401000     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
401100     MOVE +1 TO W001-SKIP                                                 
401200     PERFORM S21-SKRIV-LISTA                                              
401300                                                                          
401400     MOVE FSUM-KVOT(IX1)           TO  W001-DET12-KVOTA                   
401500     MOVE FSUM-PROC-KVOT(IX1)      TO  W001-DET12-P-KVOTA                 
401600     MOVE FSUM-KVOT(IX2)           TO  W001-DET12-KVOTB                   
401700     MOVE FSUM-PROC-KVOT(IX2)      TO  W001-DET12-P-KVOTB                 
401800     MOVE FSUM-KVOT(IX3)           TO  W001-DET12-KVOTC                   
401900     MOVE FSUM-PROC-KVOT(IX3)      TO  W001-DET12-P-KVOTC                 
402000     MOVE FSUM-KVOT(IX4)           TO  W001-DET12-KVOTD                   
402100     MOVE FSUM-PROC-KVOT(IX4)      TO  W001-DET12-P-KVOTD                 
402200     MOVE FSUM-KVOT(IX5)           TO  W001-DET12-KVOTE                   
402300     MOVE FSUM-PROC-KVOT(IX5)      TO  W001-DET12-P-KVOTE                 
402400     MOVE FSUM-KVOT(IX6)           TO  W001-DET12-KVOTF                   
402500     MOVE FSUM-PROC-KVOT(IX6)      TO  W001-DET12-P-KVOTF                 
402600     MOVE FSUM-KVOT(IX7)           TO  W001-DET12-KVOTG                   
402700     MOVE FSUM-PROC-KVOT(IX7)      TO  W001-DET12-P-KVOTG                 
402800     MOVE FSUM-KVOT(IX8)           TO  W001-DET12-KVOTH                   
402900     MOVE FSUM-PROC-KVOT(IX8)      TO  W001-DET12-P-KVOTH                 
403000     MOVE TOT-KVOT                 TO  W001-DET12-TOT                     
403100     MOVE TOT-PROC-MLAGER          TO  W001-DET12-P-TOT                   
403200     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
403300     MOVE +1 TO W001-SKIP                                                 
403400     PERFORM S21-SKRIV-LISTA                                              
403500                                                                          
403600     MOVE FSUM-SPLIT(IX1)          TO  W001-DET13-SPLITA                  
403700     MOVE FSUM-SPLIT(IX2)          TO  W001-DET13-SPLITB                  
403800     MOVE FSUM-SPLIT(IX3)          TO  W001-DET13-SPLITC                  
403900     MOVE FSUM-SPLIT(IX4)          TO  W001-DET13-SPLITD                  
404000     MOVE FSUM-SPLIT(IX5)          TO  W001-DET13-SPLITE                  
404100     MOVE FSUM-SPLIT(IX6)          TO  W001-DET13-SPLITF                  
404200     MOVE FSUM-SPLIT(IX7)          TO  W001-DET13-SPLITG                  
404300     MOVE FSUM-SPLIT(IX8)          TO  W001-DET13-SPLITH                  
404400     MOVE TOT-SPLIT                TO  W001-DET13-TOT                     
404500     MOVE ZERO                     TO  W001-DET13-P-TOT                   
404600     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
404700     MOVE +1 TO W001-SKIP                                                 
404800     PERFORM S21-SKRIV-LISTA                                              
404900                                                                          
405000     MOVE FSUM-OMSHAST-DISP(IX1)   TO  W001-DET14-OMSHASTA                
405100     MOVE FSUM-OMSHAST-PROC-D(IX1) TO  W001-DET14-P-OMSHASTA              
405200     MOVE FSUM-OMSHAST-DISP(IX2)   TO  W001-DET14-OMSHASTB                
405300     MOVE FSUM-OMSHAST-PROC-D(IX2) TO  W001-DET14-P-OMSHASTB              
405400     MOVE FSUM-OMSHAST-DISP(IX3)   TO  W001-DET14-OMSHASTC                
405500     MOVE FSUM-OMSHAST-PROC-D(IX3) TO  W001-DET14-P-OMSHASTC              
405600     MOVE FSUM-OMSHAST-DISP(IX4)   TO  W001-DET14-OMSHASTD                
405700     MOVE FSUM-OMSHAST-PROC-D(IX4) TO  W001-DET14-P-OMSHASTD              
405800     MOVE FSUM-OMSHAST-DISP(IX5)   TO  W001-DET14-OMSHASTE                
405900     MOVE FSUM-OMSHAST-PROC-D(IX5) TO  W001-DET14-P-OMSHASTE              
406000     MOVE FSUM-OMSHAST-DISP(IX6)   TO  W001-DET14-OMSHASTF                
406100     MOVE FSUM-OMSHAST-PROC-D(IX6) TO  W001-DET14-P-OMSHASTF              
406200     MOVE FSUM-OMSHAST-DISP(IX7)   TO  W001-DET14-OMSHASTG                
406300     MOVE FSUM-OMSHAST-PROC-D(IX7) TO  W001-DET14-P-OMSHASTG              
406400     MOVE FSUM-OMSHAST-DISP(IX8)   TO  W001-DET14-OMSHASTH                
406500     MOVE FSUM-OMSHAST-PROC-D(IX8) TO  W001-DET14-P-OMSHASTH              
406600     MOVE TOT-OMSHAST-DISP         TO  W001-DET14-TOT                     
406700     MOVE ZERO                     TO  W001-DET14-P-TOT                   
406800     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
406900     MOVE +1 TO W001-SKIP                                                 
407000     PERFORM S21-SKRIV-LISTA                                              
407100                                                                          
407200     MOVE FSUM-OMSHAST-LS(IX1)      TO W001-DET15-OMSHASTA                
407300     MOVE FSUM-OMSHAST-PROC-LS(IX1) TO W001-DET15-P-OMSHASTA              
407400     MOVE FSUM-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB               
407500     MOVE FSUM-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB             
407600     MOVE FSUM-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC               
407700     MOVE FSUM-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC             
407800     MOVE FSUM-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD               
407900     MOVE FSUM-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD             
408000     MOVE FSUM-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE               
408100     MOVE FSUM-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE             
408200     MOVE FSUM-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF               
408300     MOVE FSUM-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF             
408400     MOVE FSUM-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG               
408500     MOVE FSUM-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG             
408600     MOVE FSUM-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH               
408700     MOVE FSUM-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH             
408800     MOVE TOT-OMSHAST-LS            TO  W001-DET15-TOT                    
408900     MOVE ZERO                      TO  W001-DET15-P-TOT                  
409000     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
409100     MOVE +1 TO W001-SKIP                                                 
409200     PERFORM S21-SKRIV-LISTA                                              
409300                                                                          
409400     MOVE FSUM-SERVG-AKT(IX1)      TO  W001-DET16-SERVGA-A                
409500     MOVE FSUM-SERVG-PAS(IX1)      TO  W001-DET16-SERVGA-P                
409600     MOVE FSUM-SERVG-AKT(IX2)      TO  W001-DET16-SERVGB-A                
409700     MOVE FSUM-SERVG-PAS(IX2)      TO  W001-DET16-SERVGB-P                
409800     MOVE FSUM-SERVG-AKT(IX3)      TO  W001-DET16-SERVGC-A                
409900     MOVE FSUM-SERVG-PAS(IX3)      TO  W001-DET16-SERVGC-P                
410000     MOVE FSUM-SERVG-AKT(IX4)      TO  W001-DET16-SERVGD-A                
410100     MOVE FSUM-SERVG-PAS(IX4)      TO  W001-DET16-SERVGD-P                
410200     MOVE FSUM-SERVG-AKT(IX5)      TO  W001-DET16-SERVGE-A                
410300     MOVE FSUM-SERVG-PAS(IX5)      TO  W001-DET16-SERVGE-P                
410400     MOVE FSUM-SERVG-AKT(IX6)      TO  W001-DET16-SERVGF-A                
410500     MOVE FSUM-SERVG-PAS(IX6)      TO  W001-DET16-SERVGF-P                
410600     MOVE FSUM-SERVG-AKT(IX7)      TO  W001-DET16-SERVGG-A                
410700     MOVE FSUM-SERVG-PAS(IX7)      TO  W001-DET16-SERVGG-P                
410800     MOVE FSUM-SERVG-AKT(IX8)      TO  W001-DET16-SERVGH-A                
410900     MOVE FSUM-SERVG-PAS(IX8)      TO  W001-DET16-SERVGH-P                
411000     MOVE TOT-SERVG-AKT            TO  W001-DET16-TOT                     
411100     MOVE TOT-SERVG-PAS            TO  W001-DET16-P-TOT                   
411200     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
411300     MOVE +1 TO W001-SKIP                                                 
411400     PERFORM S21-SKRIV-LISTA                                              
411500                                                                          
411600     MOVE FSUM-SERVG-TOT(IX1)      TO  W001-DET17-SERVGA-TOT              
411700     MOVE FSUM-SERVG-TOT(IX2)      TO  W001-DET17-SERVGB-TOT              
411800     MOVE FSUM-SERVG-TOT(IX3)      TO  W001-DET17-SERVGC-TOT              
411900     MOVE FSUM-SERVG-TOT(IX4)      TO  W001-DET17-SERVGD-TOT              
412000     MOVE FSUM-SERVG-TOT(IX5)      TO  W001-DET17-SERVGE-TOT              
412100     MOVE FSUM-SERVG-TOT(IX6)      TO  W001-DET17-SERVGF-TOT              
412200     MOVE FSUM-SERVG-TOT(IX7)      TO  W001-DET17-SERVGG-TOT              
412300     MOVE FSUM-SERVG-TOT(IX8)      TO  W001-DET17-SERVGH-TOT              
412400     MOVE TOT-SERVG-TOT            TO  W001-DET17-TOT                     
412500     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
412600     MOVE +1 TO W001-SKIP                                                 
412700     PERFORM S21-SKRIV-LISTA                                              
412800                                                                          
412900     MOVE FSUM-SERVG-TEO(IX1)      TO  W001-DET18-SERVGA-TEO              
413000     MOVE FSUM-SERVG-TEO(IX2)      TO  W001-DET18-SERVGB-TEO              
413100     MOVE FSUM-SERVG-TEO(IX3)      TO  W001-DET18-SERVGC-TEO              
413200     MOVE FSUM-SERVG-TEO(IX4)      TO  W001-DET18-SERVGD-TEO              
413300     MOVE FSUM-SERVG-TEO(IX5)      TO  W001-DET18-SERVGE-TEO              
413400     MOVE FSUM-SERVG-TEO(IX6)      TO  W001-DET18-SERVGF-TEO              
413500     MOVE FSUM-SERVG-TEO(IX7)      TO  W001-DET18-SERVGG-TEO              
413600     MOVE FSUM-SERVG-TEO(IX8)      TO  W001-DET18-SERVGH-TEO              
413700     MOVE TOT-SERVG-TEO            TO  W001-DET18-TOT                     
413800     MOVE W001-DETALJRAD-18 TO W001-RAD                                   
413900     MOVE +1 TO W001-SKIP                                                 
414000     PERFORM S21-SKRIV-LISTA                                              
414100     .                                                                    
414200     EJECT                                                                
414300 Z-FINIT SECTION.                                                         
414400                                                                          
414500     CLOSE W23170                                                         
414700           W231PP                                                         
414800           W23172-001                                                     
414900                                                                          
415000     MOVE 'S' TO POSTSUM-OPKOD                                            
415100     CALL POSTSUM USING POSTSUM-PARM                                      
415200     .                                                                    
415300     EJECT                                                                
415400 S01-LAS-W23170 SECTION.                                                  
415500                                                                          
415600     READ W23170 INTO IN-AREA                                             
415700     AT END                                                               
415800         SET END-OF-W23170 TO TRUE                                        
415900                                                                          
416000     NOT AT END                                                           
416100        MOVE 'W23172'      TO POSTSUM-FDNAMN                              
416200        MOVE 'W23172D1'    TO POSTSUM-DDNAMN2                             
416300        MOVE SPACE         TO POSTSUM-TRANSTYP                            
416400        CALL POSTSUM USING POSTSUM-PARM                                   
416500     .                                                                    
416600     EJECT                                                                
416700 S02-LAS-W231PP SECTION.                                                  
416800                                                                          
416900     READ W231PP INTO PARM-AREA                                           
417000     AT END                                                               
417100         SET END-OF-W231PP TO TRUE                                        
417200                                                                          
417300     NOT AT END                                                           
417400        MOVE 'PARM  '      TO POSTSUM-FDNAMN                              
417500        MOVE 'W23172D4'    TO POSTSUM-DDNAMN2                             
417600        MOVE SPACE         TO POSTSUM-TRANSTYP                            
417700        CALL POSTSUM USING POSTSUM-PARM                                   
417800     .                                                                    
417900     EJECT                                                                
418000 S21-SKRIV-LISTA SECTION.                                                 
418100                                                                          
418200     WRITE W23172-001-RAD FROM W001-RAD AFTER W001-SKIP                   
418300                                                                          
418400     MOVE SPACE TO W001-RAD                                               
418500     ADD  +1 TO W001-ANTAL-RADER                                          
418600     .                                                                    
418700     EJECT                                                                
418800 S21A-SKRIV-RUBRIKER SECTION.                                             
418900                                                                          
419000     ADD +1 TO W001-SIDRAKNARE                                            
419100     MOVE W001-SIDRAKNARE TO W001-SID                                     
419200     WRITE W23172-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
419300     WRITE W23172-001-RAD FROM W001-RUBRIK3 AFTER 2                       
419400     MOVE +2 TO W001-ANTAL-RADER                                          
419500     MOVE +2 TO W001-SKIP                                                 
419600     .                                                                    
419700                                                                          
419710 IMS-GU-WDB616    SECTION.                                                
419720     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
419730          DELIMITED BY SIZE INTO SSA1                                     
419740     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
419750          DELIMITED BY SIZE INTO SSA2                                     
419760     MOVE '  ' TO GODK-STATUSKODER                                        
419770     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
419780     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
419790     PERFORM IMS-STATUSKONTROLL                                           
419791     .                                                                    
419792                                                                          
421700 IMS-STATUSKONTROLL SECTION.                                              
421800     SET STATUS-IX TO 1                                                   
421900     SEARCH GODK-STATUS  AT END CALL FELLOG                               
422000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
422100          CONTINUE                                                        
422200     END-SEARCH                                                           
422300     .                                                                    
