000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5107400.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   SEPT 98.                                                 
000500*    REMARKS.                                                             
000600*                SKAPAR HÄNDELSELISTOR TILL ON-DEMAND                     
000700*                                                                         
000800*        INDATA  W51075                                                   
000900*                                                                         
001000*                                                                         
001100     EJECT                                                                
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SELECT W51075              ASSIGN TO UT-S-W51074D1.                  
001900     SELECT LISTA1              ASSIGN TO UT-S-W51074D2.                  
001910     SELECT LISTA2              ASSIGN TO UT-S-W51074D3.                  
002000     SELECT LISTA3              ASSIGN TO UT-S-W51074D4.                  
002010     SELECT LISTA4              ASSIGN TO UT-S-W51074D5.                  
002020     SELECT LISTA5              ASSIGN TO UT-S-W51074D6.                  
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W51075                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900*01  -COPY W51074     -L.                                                 
003000                                                                          
003100 FD  LISTA1                                                               
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400 01  LISTPOST1                    PIC X(121).                             
003500                                                                          
003600 FD  LISTA2                                                               
003700     RECORDING F                                                          
003800     BLOCK CONTAINS 0.                                                    
003900 01  LISTPOST2                    PIC X(121).                             
004000     EJECT                                                                
004100                                                                          
004110 FD  LISTA3                                                               
004120     RECORDING V                                                          
004130     BLOCK CONTAINS 0.                                                    
004140 01  LISTPOST3                    PIC X(165).                             
004150                                                                          
004160 FD  LISTA4                                                               
004170     RECORDING V                                                          
004180     BLOCK CONTAINS 0.                                                    
004190 01  LISTPOST4                    PIC X(165).                             
004191                                                                          
004192 FD  LISTA5                                                               
004193     RECORDING F                                                          
004194     BLOCK CONTAINS 0.                                                    
004195 01  LISTPOST5                    PIC X(229).                             
004196     EJECT                                                                
004197                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM               PIC X(8)        VALUE 'W5107400'.                
004500 77  JA                  PIC X           VALUE 'J'.                       
004600 77  NEJ                 PIC X           VALUE 'N'.                       
004700 77  IN-EOF              PIC X           VALUE 'N'.                       
004800                                                                          
004900 01  W-IDARTNR           PIC 9(9)        VALUE ZERO.                      
005000 01  W-ANTAL             PIC S9(7)       VALUE ZERO.                      
005100 01  W-IDVERGL           PIC X(10)       VALUE SPACE.                     
005200 01  W-IDVERGL-1         PIC X(10)       VALUE SPACE.                     
005300 01  W-IDVERGL-2         PIC X(10)       VALUE SPACE.                     
005310 01  WS-SUBEL            PIC S9(9)V99    VALUE +0    COMP-3.              
005400                                                                          
005500 01  RAD-ANT             PIC S9          VALUE +0    COMP-3.              
005600 01  SIDNR               PIC S9(5)       VALUE +0    COMP-3.              
005700 01  RADNR               PIC S9(3)       VALUE +99   COMP-3.              
005800     EJECT                                                                
005900                                                                          
006000 01  INAREA.                                                              
006100*    03  -COPY W51074  -PRE IN-.                                          
006200     EJECT                                                                
006300                                                                          
006400 01  TITEL1.                                                              
006500     03  FILLER              PIC X(29)   VALUE                            
006600         ' W51074-001   VCCS           '.                                 
006700     03  FILLER              PIC X(22)   VALUE                            
006800         'TRANSACTION LIST DATE:'.                                        
006900     03  W-DATUM-T1          PIC X(8).                                    
007000     03  FILLER              PIC X(7)   VALUE                             
007100         ' TIME: '.                                                       
007200     03  W-TID-T1            PIC X(4).                                    
007300 01  TITEL2.                                                              
007400     03  FILLER              PIC X(29)   VALUE                            
007500         ' W51074-002   VCCS           '.                                 
007600     03  FILLER              PIC X(22)   VALUE                            
007700         'TRANSACTION LIST DATE:'.                                        
007800     03  W-DATUM-T2          PIC X(8).                                    
007900     03  FILLER              PIC X(7)   VALUE                             
008000         ' TIME: '.                                                       
008100     03  W-TID-T2            PIC X(4).                                    
008200                                                                          
008300 01  RUBRIK1A.                                                            
008400     03  FILLER              PIC X(12) VALUE                              
008500         ' INV/CR:    '.                                                  
008600     03  W-IDVERGL-RUBR1A    PIC X(10).                                   
008700     03  FILLER              PIC X(12) VALUE                              
008800         ' VER.DATE:  '.                                                  
008900     03  W-DAVERDAT-RUBR1A   PIC X(10).                                   
009000     03  FILLER              PIC X(12) VALUE                              
009100         ' EVENT :    '.                                                  
009200     03  W-KDEKHHT-RUBR1A    PIC X(3).                                    
009300     03  FILLER REDEFINES W-KDEKHHT-RUBR1A.                               
009400         05 W-KDEKHHT-DEL1   PIC X(2).                                    
009500         05 W-KDEKHHT-DEL2   PIC X.                                       
009600 01  RUBRIK1B.                                                            
009700     03  FILLER              PIC X(12) VALUE                              
009800         ' REF.NR:    '.                                                  
009900     03  W-IDVERGL-RUBR1B    PIC X(10).                                   
010000     03  FILLER              PIC X(12) VALUE                              
010100         ' VER.DATE:  '.                                                  
010200     03  W-DAVERDAT-RUBR1B   PIC X(10).                                   
010300     03  FILLER              PIC X(12) VALUE                              
010400         ' EVENT :    '.                                                  
010500     03  W-KDEKHHT-RUBR1B    PIC X(3).                                    
010600 01  RUBRIK2.                                                             
010700     03  FILLER              PIC X(30) VALUE                              
010800         ' EVENT    LEVELDOC DC     QTY '.                                
010900     03  FILLER              PIC X(52) VALUE                              
011000         '    PARTNO  PG ACCT/PARMA PK COSTCENTER INT ORDER NO'.          
011100     03  FILLER              PIC X(29)   VALUE                            
011200         '     AMOUNT     PROFIT CENTER'.                                 
011300     EJECT                                                                
011400                                                                          
011500 01  BLANK-RAD              PIC X    VALUE SPACE.                         
011600 01  DETALJRAD-1.                                                         
011700     03  FILLER                 PIC X.                                    
011800     03  RAD-HAENDELSE          PIC X(7).                                 
011900     03  FILLER REDEFINES RAD-HAENDELSE.                                  
012000         05  RAD-KDEKHHT        PIC X(3).                                 
012100         05  RAD-STRECK         PIC X.                                    
012200         05  RAD-KDEKSHT        PIC X(3).                                 
012300     03  FILLER                 PIC X(2)  VALUE SPACE.                    
012400     03  RAD-KDEKNIVA           PIC X(4).                                 
012500     03  FILLER                 PIC X(1)  VALUE SPACE.                    
012600     03  RAD-KDDOKTYP           PIC X(2).                                 
012700     03  FILLER                 PIC X(2)  VALUE SPACE.                    
012800     03  RAD-IDDC               PIC X(2).                                 
012900     03  FILLER                 PIC X(1)  VALUE SPACE.                    
013000     03  RAD-KVANTAL            PIC X(7).                                 
013100     03  FILLER REDEFINES RAD-KVANTAL.                                    
013200         05  RAD-KVANTAL-1      PIC X(6).                                 
013300         05  RAD-KVANTAL-2      PIC X(1).                                 
013400     03  RAD-KVANTAL-TKN        PIC X(1).                                 
013500     03  FILLER                 PIC X(1)  VALUE SPACE.                    
013600     03  RAD-IDARTNR            PIC Z(8)9.                                
013700     03  FILLER                 PIC X(1)  VALUE SPACE.                    
013800     03  RAD-KDPRODSL           PIC Z(3).                                 
013900     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014000     03  RAD-IDKONTO            PIC 9(10).                                
014100     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014200     03  RAD-KDPOST             PIC X(2).                                 
014300     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014400     03  RAD-IDKST              PIC X(10).                                
014500     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014600     03  RAD-IDANALYS           PIC X(12).                                
014700     03  RAD-SUBEL              PIC Z(8)9.99.                             
014800     03  RAD-IDTECKEN           PIC X.                                    
014900     03  FILLER                 PIC X(3)  VALUE SPACE.                    
015000     03  RAD-IDPRCTR            PIC X(10).                                
015100     EJECT                                                                
015200                                                                          
015310 01  UT-HEADER-SF.                                                        
015320     03  UT-H-IDVERGL-SF     PIC X(15)    VALUE                           
015330                                          'DOCUMENT NUMBER'.              
015340     03  FILLER              PIC X(01)    VALUE ';'.                      
015350     03  UT-H-DATUM-T1-SF    PIC X(09)    VALUE 'LIST DATE'.              
015351     03  FILLER              PIC X(01)    VALUE ';'.                      
015352     03  UT-H-DAVERDAT-SF    PIC X(13)    VALUE 'DOCUMENT DATE'.          
015353     03  FILLER              PIC X(01)    VALUE ';'.                      
015354     03  UT-H-KDEKHHT-SF     PIC X(10)    VALUE 'MAIN EVENT'.             
015355     03  FILLER              PIC X(01)    VALUE ';'.                      
015356     03  UT-H-KDEKSHT-SF     PIC X(09)    VALUE 'SUB EVENT'.              
015357     03  FILLER              PIC X(01)    VALUE ';'.                      
015358     03  UT-H-KDEKNIVA-SF    PIC X(11)    VALUE 'EVENT LEVEL'.            
015359     03  FILLER              PIC X(01)    VALUE ';'.                      
015360     03  UT-H-KDDOKTYP-SF    PIC X(13)    VALUE 'DOCUMENT TYPE'.          
015361     03  FILLER              PIC X(01)    VALUE ';'.                      
015362     03  UT-H-IDDC-SF        PIC X(02)    VALUE 'DC'.                     
015363     03  FILLER              PIC X(01)    VALUE ';'.                      
015364     03  UT-H-KVANTAL-SF     PIC X(08)    VALUE 'QUANTITY'.               
015365     03  FILLER              PIC X(01)    VALUE ';'.                      
015366     03  UT-H-IDARTNR-SF     PIC X(07)    VALUE 'PART NO'.                
015367     03  FILLER              PIC X(01)    VALUE ';'.                      
015368     03  UT-H-KDPRODSL-SF    PIC X(13)    VALUE 'PRODUCT GROUP'.          
015369     03  FILLER              PIC X(01)    VALUE ';'.                      
015370     03  UT-H-IDKONTO-SF     PIC X(07)    VALUE 'ACCOUNT'.                
015371     03  FILLER              PIC X(01)    VALUE ';'.                      
015372     03  UT-H-KDPOST-SF      PIC X(11)    VALUE 'POSTING KEY'.            
015373     03  FILLER              PIC X(01)    VALUE ';'.                      
015374     03  UT-H-IDKST-SF       PIC X(10)    VALUE 'COSTCENTER'.             
015375     03  FILLER              PIC X(01)    VALUE ';'.                      
015376     03  UT-H-IDTECKEN-SF    PIC X(12)    VALUE 'ORDER NUMBER'.           
015377     03  FILLER              PIC X(01)    VALUE ';'.                      
015378     03  UT-H-SUBEL-SF       PIC X(06)    VALUE 'AMOUNT'.                 
015379     03  FILLER              PIC X(01)    VALUE ';'.                      
015380     03  UT-H-IDPRCTR-SF     PIC X(13)    VALUE 'PROFIT CENTER'.          
015381     03  FILLER              PIC X(01)    VALUE ';'.                      
015382     03  UT-H-PRARTSTD-SF    PIC X(05)    VALUE 'PRICE'.                  
015383     03  FILLER              PIC X(01)    VALUE ';'.                      
015384     03  UT-H-KDTRADP-SF     PIC X(15)    VALUE 'TRADING PARTNER'.        
015385     03  FILLER              PIC X(01)    VALUE ';'.                      
015386     03  UT-H-FLLSBOK-SF     PIC X(21)    VALUE                           
015387                                          'FLAG FOR STOCK UPDATE'.        
015388                                                                          
015389 01  UT-AREA.                                                             
015390     03  FILLER                 PIC X(02)    VALUE SPACE.                 
015391     03  UT-IDVERGL             PIC X(10)    VALUE SPACE.                 
015392     03  FILLER                 PIC X(01)    VALUE ';'.                   
015393     03  UT-DATUM-T1             PIC X(8)     VALUE SPACE.                
015394     03  FILLER                 PIC X(01)    VALUE ';'.                   
015395     03  UT-DAVERDAT            PIC X(10)    VALUE SPACE.                 
015396     03  FILLER                 PIC X(01)    VALUE ';'.                   
015397     03  UT-HAENDELSE           PIC X(07).                                
015398     03  FILLER REDEFINES UT-HAENDELSE.                                   
015399         05  UT-KDEKHHT         PIC X(03).                                
015400         05  UT-STRECK          PIC X(01).                                
015401         05  UT-KDEKSHT         PIC X(03).                                
015402     03  FILLER                 PIC X(01)    VALUE ';'.                   
015403     03  UT-KDEKNIVA            PIC X(04)    VALUE SPACE.                 
015404     03  FILLER                 PIC X(01)    VALUE ';'.                   
015405     03  UT-KDDOKTYP            PIC X(02)    VALUE SPACE.                 
015406     03  FILLER                 PIC X(01)    VALUE ';'.                   
015407     03  UT-IDDC                PIC X(02)    VALUE SPACE.                 
015408     03  FILLER                 PIC X(01)    VALUE ';'.                   
015409     03  UT-KVANTAL             PIC X(07)    VALUE SPACE.                 
015410     03  FILLER                 PIC X(01)    VALUE ';'.                   
015411     03  UT-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                 
015412     03  FILLER                 PIC X(01)    VALUE ';'.                   
015413     03  UT-IDARTNR             PIC Z(8)9    VALUE ZERO.                  
015414     03  FILLER                 PIC X(01)    VALUE ';'.                   
015415     03  UT-KDPRODSL            PIC Z(3)     VALUE ZERO.                  
015416     03  FILLER                 PIC X(01)    VALUE ';'.                   
015417     03  UT-IDKONTO             PIC 9(10)    VALUE ZERO.                  
015418     03  FILLER                 PIC X(01)    VALUE ';'.                   
015419     03  UT-KDPOST              PIC X(02)    VALUE SPACE.                 
015420     03  FILLER                 PIC X(01)    VALUE ';'.                   
015421     03  UT-IDKST               PIC X(10)    VALUE SPACE.                 
015422     03  FILLER                 PIC X(01)    VALUE ';'.                   
015423     03  UT-IDANALYS            PIC X(12)    VALUE SPACE.                 
015424     03  FILLER                 PIC X(01)    VALUE ';'.                   
015425     03  UT-SUBEL               PIC Z(8)9.99 VALUE ZERO.                  
015426     03  FILLER                 PIC X(01)    VALUE ';'.                   
015427     03  UT-TECKEN              PIC X(01)    VALUE SPACE.                 
015428     03  FILLER                 PIC X(01)    VALUE ';'.                   
015429     03  UT-IDPRCTR             PIC X(10)    VALUE SPACE.                 
015430     03  FILLER                 PIC X(01)    VALUE ';'.                   
015431                                                                          
015432 01  UT-AREA-SF.                                                          
015433     03  UT-IDVERGL-SF          PIC X(10)    VALUE SPACE.                 
015434     03  FILLER                 PIC X(01)    VALUE ';'.                   
015435     03  UT-DATUM-T1-SF         PIC X(8)     VALUE SPACE.                 
015436     03  FILLER                 PIC X(01)    VALUE ';'.                   
015437     03  UT-DAVERDAT-SF         PIC X(10)    VALUE SPACE.                 
015438     03  FILLER                 PIC X(01)    VALUE ';'.                   
015439     03  UT-KDEKHHT-SF          PIC X(03)    VALUE SPACE.                 
015440     03  FILLER                 PIC X(01)    VALUE ';'.                   
015441     03  UT-KDEKSHT-SF          PIC X(03)    VALUE SPACE.                 
015442     03  FILLER                 PIC X(01)    VALUE ';'.                   
015443     03  UT-KDEKNIVA-SF         PIC X(04)    VALUE SPACE.                 
015444     03  FILLER                 PIC X(01)    VALUE ';'.                   
015445     03  UT-KDDOKTYP-SF         PIC X(02)    VALUE SPACE.                 
015446     03  FILLER                 PIC X(01)    VALUE ';'.                   
015447     03  UT-IDDC-SF             PIC X(02)    VALUE SPACE.                 
015448     03  FILLER                 PIC X(01)    VALUE ';'.                   
015449     03  UT-KVANTAL-SF          PIC -(7)9    VALUE SPACE.                 
015450     03  FILLER                 PIC X(01)    VALUE ';'.                   
015451     03  UT-IDARTNR-SF          PIC Z(8)9    VALUE ZERO.                  
015452     03  FILLER                 PIC X(01)    VALUE ';'.                   
015453     03  UT-KDPRODSL-SF         PIC Z(2)9    VALUE ZERO.                  
015454     03  FILLER                 PIC X(01)    VALUE ';'.                   
015455     03  UT-IDKONTO-SF          PIC Z(9)9    VALUE ZERO.                  
015456     03  FILLER                 PIC X(01)    VALUE ';'.                   
015457     03  UT-KDPOST-SF           PIC X(02)    VALUE SPACE.                 
015458     03  FILLER                 PIC X(01)    VALUE ';'.                   
015459     03  UT-IDKST-SF            PIC X(10)    VALUE SPACE.                 
015460     03  FILLER                 PIC X(01)    VALUE ';'.                   
015461     03  UT-IDANALYS-SF         PIC X(12)    VALUE SPACE.                 
015462     03  FILLER                 PIC X(01)    VALUE ';'.                   
015463     03  UT-SUBEL-SF            PIC -(9)9.99 VALUE ZERO.                  
015464     03  FILLER                 PIC X(01)    VALUE ';'.                   
015465     03  UT-IDPRCTR-SF          PIC X(10)    VALUE SPACE.                 
015466     03  FILLER                 PIC X(01)    VALUE ';'.                   
015467     03  UT-PRARTSTD-SF         PIC -(7)9.99 VALUE ZERO.                  
015468     03  FILLER                 PIC X(01)    VALUE ';'.                   
015469     03  UT-KDTRADP-SF          PIC X(04)    VALUE SPACE.                 
015470     03  FILLER                 PIC X(01)    VALUE ';'.                   
015471     03  UT-FLLSBOK-SF          PIC X(01)    VALUE SPACE.                 
015472     EJECT                                                                
015473                                                                          
015474 PROCEDURE DIVISION.                                                      
015480     PERFORM A-INITIERA                                                   
015500                                                                          
015600     PERFORM S01-LAS-INFIL                                                
015700     PERFORM UNTIL IN-EOF = JA                                            
015800       PERFORM B-REDIGERA-DETALJRAD                                       
015900                                                                          
016000       PERFORM S01-LAS-INFIL                                              
016100     END-PERFORM                                                          
016200                                                                          
016300     PERFORM Z-AVSLUTA                                                    
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     .                                                                    
016700                                                                          
016800 A-INITIERA SECTION.                                                      
016900     OPEN INPUT  W51075                                                   
017000          OUTPUT LISTA1                                                   
017100                 LISTA2                                                   
017110                 LISTA3                                                   
017120                 LISTA4                                                   
017130                 LISTA5                                                   
017200                                                                          
017300     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATUM-T1                        
017400                                        W-DATUM-T2                        
017500     MOVE FUNCTION CURRENT-DATE(9:4) TO W-TID-T1                          
017600                                        W-TID-T2                          
017610                                                                          
017620     PERFORM AA-WRITE-HEADER                                              
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000 B-REDIGERA-DETALJRAD SECTION.                                            
018100                                                                          
018110     INITIALIZE WS-SUBEL                                                  
018200     IF (IN-KVANTAL NOT = W-ANTAL) OR                                     
018300        (IN-IDARTNR NOT = W-IDARTNR) OR                                   
018400        (IN-IDVERGL NOT = W-IDVERGL)                                      
018500        MOVE IN-KVANTAL     TO RAD-KVANTAL                                
018600        INSPECT RAD-KVANTAL-1 REPLACING LEADING ZERO BY SPACE             
018700        IF IN-KVANTAL < 0                                                 
018800           MOVE '-'   TO RAD-KVANTAL-TKN                                  
018900        ELSE                                                              
018910           MOVE SPACE TO RAD-KVANTAL-TKN                                  
019000        END-IF                                                            
019010        MOVE IN-KVANTAL     TO UT-KVANTAL                                 
019011                               UT-KVANTAL-SF                              
019020        INSPECT UT-KVANTAL   REPLACING LEADING ZERO BY SPACE              
019030        IF IN-KVANTAL < 0                                                 
019040           MOVE '-'   TO UT-KVANTAL-TKN                                   
019050        ELSE                                                              
019060           MOVE SPACE TO UT-KVANTAL-TKN                                   
019070        END-IF                                                            
019100     END-IF                                                               
019200     MOVE IN-KDEKHHT        TO RAD-KDEKHHT                                
019300     MOVE '-'               TO RAD-STRECK                                 
019400     MOVE IN-KDEKSHT        TO RAD-KDEKSHT                                
019500     MOVE IN-KDEKNIVA       TO RAD-KDEKNIVA                               
019600     MOVE IN-KDDOKTYP       TO RAD-KDDOKTYP                               
019700     MOVE IN-IDDC           TO RAD-IDDC                                   
019800     MOVE IN-IDARTNR        TO RAD-IDARTNR                                
019900     MOVE IN-KDPRODSL       TO RAD-KDPRODSL                               
020000     MOVE IN-IDKONTO        TO RAD-IDKONTO                                
020100     INSPECT RAD-IDKONTO REPLACING LEADING ZERO BY SPACE                  
020200     MOVE IN-IDKST          TO RAD-IDKST                                  
020300     MOVE IN-IDANALYS       TO RAD-IDANALYS                               
020400     MOVE IN-SUBEL          TO RAD-SUBEL                                  
020500     MOVE IN-IDTECKEN       TO RAD-IDTECKEN                               
020600     MOVE IN-IDPRCTR        TO RAD-IDPRCTR                                
020700     MOVE IN-KDPOST         TO RAD-KDPOST                                 
020710****FOR REPORT                                                            
020720     MOVE IN-IDVERGL(1:9)   TO UT-IDVERGL                                 
020721                               UT-IDVERGL-SF                              
020730     MOVE W-DATUM-T1        TO UT-DATUM-T1                                
020731                               UT-DATUM-T1-SF                             
020740     MOVE IN-DAVERDAT       TO UT-DAVERDAT                                
020741                               UT-DAVERDAT-SF                             
020750     MOVE IN-KDEKHHT        TO UT-KDEKHHT                                 
020751                               UT-KDEKHHT-SF                              
020760     MOVE '-'               TO UT-STRECK                                  
020770     MOVE IN-KDEKSHT        TO UT-KDEKSHT                                 
020771                               UT-KDEKSHT-SF                              
020780     MOVE IN-KDEKNIVA       TO UT-KDEKNIVA                                
020781                               UT-KDEKNIVA-SF                             
020790     MOVE IN-KDDOKTYP       TO UT-KDDOKTYP                                
020791                               UT-KDDOKTYP-SF                             
020792     MOVE IN-IDDC           TO UT-IDDC                                    
020793                               UT-IDDC-SF                                 
020794     MOVE IN-IDARTNR        TO UT-IDARTNR                                 
020795                               UT-IDARTNR-SF                              
020796     MOVE IN-KDPRODSL       TO UT-KDPRODSL                                
020797                               UT-KDPRODSL-SF                             
020798     MOVE IN-IDKONTO        TO UT-IDKONTO                                 
020799                               UT-IDKONTO-SF                              
020800     INSPECT UT-IDKONTO REPLACING LEADING ZERO BY SPACE                   
020801     INSPECT UT-IDKONTO-SF REPLACING LEADING ZERO BY SPACE                
020802     MOVE IN-IDKST          TO UT-IDKST                                   
020803                               UT-IDKST-SF                                
020804     MOVE IN-IDANALYS       TO UT-IDANALYS                                
020805                               UT-IDANALYS-SF                             
020806     MOVE IN-SUBEL          TO UT-SUBEL                                   
020807                               WS-SUBEL                                   
020808     MOVE IN-IDTECKEN       TO UT-TECKEN                                  
020809     IF IN-IDTECKEN = '-'                                                 
020810        COMPUTE WS-SUBEL = 0 - WS-SUBEL                                   
020811        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
020812     ELSE                                                                 
020813        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
020814     END-IF                                                               
020815     MOVE IN-IDPRCTR        TO UT-IDPRCTR                                 
020816                               UT-IDPRCTR-SF                              
020817     MOVE IN-KDPOST         TO UT-KDPOST                                  
020818                               UT-KDPOST-SF                               
020820     MOVE IN-PRARTSTD       TO UT-PRARTSTD-SF                             
020830     MOVE IN-FLLSBOK        TO UT-FLLSBOK-SF                              
020840     MOVE 'SEPV'            TO UT-KDTRADP-SF                              
020900     IF IN-KDEKHHT = '303'                                                
021000                  OR '304'                                                
021100                  OR '201'                                                
021200                  OR '202'                                                
021300                  OR '203'                                                
021400                  OR '204'                                                
021500                  OR '205'                                                
021600                  OR '501'                                                
021700        PERFORM S02-SKRIV-LISTA1                                          
021800     ELSE                                                                 
021900        PERFORM S03-SKRIV-LISTA2                                          
022000     END-IF                                                               
022100                                                                          
022110     PERFORM S04-SKRIV-LIST-SF                                            
022120                                                                          
022200     MOVE IN-IDVERGL        TO W-IDVERGL                                  
022300     MOVE IN-IDARTNR        TO W-IDARTNR                                  
022400     MOVE IN-KVANTAL        TO W-ANTAL                                    
022500     MOVE SPACE             TO DETALJRAD-1                                
022600     .                                                                    
022700     EJECT                                                                
022800                                                                          
022900 Z-AVSLUTA SECTION.                                                       
023000     CLOSE LISTA1 W51075 LISTA3 LISTA5                                    
023100     .                                                                    
023200     EJECT                                                                
023300                                                                          
023400 S01-LAS-INFIL SECTION.                                                   
023500     READ W51075 INTO INAREA                                              
023600        AT END MOVE JA TO IN-EOF                                          
023700     END-READ                                                             
023800     .                                                                    
023900                                                                          
023970                                                                          
024000 S02-SKRIV-LISTA1 SECTION.                                                
024100                                                                          
024200     IF IN-IDVERGL NOT = W-IDVERGL-1                                      
024300       WRITE LISTPOST1 FROM TITEL1 AFTER PAGE                             
024400       MOVE IN-IDVERGL        TO W-IDVERGL-RUBR1A                         
024500       INSPECT W-IDVERGL-RUBR1A REPLACING LEADING ZERO BY SPACE           
024600       MOVE IN-DAVERDAT       TO W-DAVERDAT-RUBR1A                        
024700       MOVE IN-KDEKHHT (1:2)  TO W-KDEKHHT-DEL1                           
024800       MOVE 'X' TO W-KDEKHHT-DEL2                                         
024900                                                                          
025000       WRITE LISTPOST1        FROM RUBRIK1A AFTER 1                       
025100       WRITE LISTPOST1        FROM RUBRIK2 AFTER 1                        
025200       MOVE IN-IDVERGL        TO W-IDVERGL-1                              
025300     END-IF                                                               
025400                                                                          
025500     WRITE LISTPOST1          FROM DETALJRAD-1 AFTER 1                    
025510     WRITE LISTPOST3          FROM UT-AREA AFTER 1                        
025600     .                                                                    
025700                                                                          
025800 S03-SKRIV-LISTA2 SECTION.                                                
025900                                                                          
026000     IF IN-IDVERGL NOT = W-IDVERGL-2                                      
026100       WRITE LISTPOST2     FROM TITEL2 AFTER PAGE                         
026200       MOVE IN-IDVERGL     TO W-IDVERGL-RUBR1B                            
026300       INSPECT W-IDVERGL-RUBR1B REPLACING LEADING ZERO BY SPACE           
026400       MOVE IN-DAVERDAT    TO W-DAVERDAT-RUBR1B                           
026500       MOVE IN-KDEKHHT     TO W-KDEKHHT-RUBR1B                            
026600                                                                          
026700       WRITE LISTPOST2     FROM RUBRIK1B AFTER 1                          
026800       WRITE LISTPOST2     FROM RUBRIK2 AFTER 1                           
026900       MOVE IN-IDVERGL     TO W-IDVERGL-2                                 
027000     END-IF                                                               
027100                                                                          
027200     WRITE LISTPOST2       FROM DETALJRAD-1 AFTER 1                       
027210     WRITE LISTPOST4       FROM UT-AREA AFTER 1                           
027300     .                                                                    
027400                                                                          
027500 S04-SKRIV-LIST-SF SECTION.                                               
027600                                                                          
027700     WRITE LISTPOST5       FROM UT-AREA-SF                                
027800     .                                                                    
027900     EJECT                                                                
028000 AA-WRITE-HEADER SECTION.                                                 
028100                                                                          
028200     WRITE LISTPOST5       FROM UT-HEADER-SF                              
028300     .                                                                    
028400     EJECT                                                                
