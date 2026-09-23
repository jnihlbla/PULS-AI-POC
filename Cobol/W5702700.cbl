000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5702700.                                                
000300 AUTHOR.         MAMATHA SHETTY.                                          
000400 DATE-WRITTEN.   AUG 2022.                                                
000500*    REMARKS.                                                             
000600*                SKAPAR HÄNDELSELISTOR TILL ON-DEMAND - DUBAI,            
000700*                 KOREA, TURKEY , CHINA , MALAYSIA                        
000800*        INDATA  W57027                                                   
000900*                                                                         
001000*                                                                         
001100     EJECT                                                                
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SELECT W57027              ASSIGN TO UT-S-W57027D1.                  
001900     SELECT LISTA1              ASSIGN TO UT-S-W57027D2.                  
002000     SELECT LISTA2              ASSIGN TO UT-S-W57027D3.                  
002100     SELECT LISTA3              ASSIGN TO UT-S-W57027D4.                  
002200     SELECT LISTA4              ASSIGN TO UT-S-W57027D5.                  
002210     SELECT LISTA5              ASSIGN TO UT-S-W57027D6.                  
002300     EJECT                                                                
002400                                                                          
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700                                                                          
002800                                                                          
002900 FD  W57027                                                               
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0.                                                    
003200*01  -COPY W57073     -L.                                                 
003300                                                                          
003400 FD  LISTA1                                                               
003500     RECORDING F                                                          
003600     BLOCK CONTAINS 0.                                                    
003700 01  LISTPOST1                    PIC X(121).                             
003800                                                                          
003900 FD  LISTA2                                                               
004000     RECORDING F                                                          
004100     BLOCK CONTAINS 0.                                                    
004200 01  LISTPOST2                    PIC X(121).                             
004300                                                                          
004400 FD  LISTA3                                                               
004500     RECORDING V                                                          
004600     BLOCK CONTAINS 0.                                                    
004700 01  LISTPOST3                    PIC X(165).                             
004800                                                                          
004900 FD  LISTA4                                                               
005000     RECORDING V                                                          
005100     BLOCK CONTAINS 0.                                                    
005200 01  LISTPOST4                    PIC X(165).                             
005210                                                                          
005220 FD  LISTA5                                                               
005230     RECORDING F                                                          
005240     BLOCK CONTAINS 0.                                                    
005250 01  LISTPOST5                    PIC X(229).                             
005300     EJECT                                                                
005400                                                                          
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM               PIC X(8)        VALUE 'W5702700'.                
005800 77  JA                  PIC X           VALUE 'J'.                       
005900 77  NEJ                 PIC X           VALUE 'N'.                       
006000 77  IN-EOF              PIC X           VALUE 'N'.                       
006100                                                                          
006200 01  W-IDARTNR           PIC 9(9)        VALUE ZERO.                      
006300 01  W-ANTAL             PIC S9(7)       VALUE ZERO.                      
006400 01  W-IDVERGL           PIC X(10)       VALUE SPACE.                     
006500 01  W-KDTRADP           PIC X(04).                                       
006600 01  WS-KDTRADP          PIC X(4).                                        
006700 01  WS-PREV-KDTRADP-1   PIC X(4)        VALUE SPACE.                     
006710 01  WS-PREV-KDTRADP-2   PIC X(4)        VALUE SPACE.                     
006800 01  W-IDVERGL-1         PIC X(10)       VALUE SPACE.                     
006900 01  W-IDVERGL-2         PIC X(10)       VALUE SPACE.                     
007000                                                                          
007100 01  RAD-ANT             PIC S9          VALUE +0    COMP-3.              
007200 01  SIDNR               PIC S9(5)       VALUE +0    COMP-3.              
007300 01  RADNR               PIC S9(3)       VALUE +99   COMP-3.              
007320 01  WS-SUBEL            PIC S9(9)V99    VALUE +0    COMP-3.              
007400     EJECT                                                                
007500 01  W001-DAP.                                                            
007600     03  FILLER                  PIC X(165)  VALUE SPACE.                 
007700     EJECT                                                                
007800                                                                          
007900 01  INAREA.                                                              
008000*    03  -COPY W57073  -PRE IN-.                                          
008100     EJECT                                                                
008200                                                                          
008300 01  UT-CONTROL.                                                          
008400     03  FILLER                  PIC X(15)   VALUE                        
008500                                 ' ¤DAPW51027-001'.                       
008600     EJECT                                                                
008700 01  UT-CONTROL-KDTRADP.                                                  
008800     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
008900     03  UT-CTL-KDTRADP          PIC X(4)    VALUE SPACE.                 
009000     EJECT                                                                
009100 01  TITEL1.                                                              
009200     03  FILLER              PIC X(14)   VALUE                            
009300         ' W57027-001   '.                                                
009400     03  W-HEADER-T1         PIC X(15).                                   
009600     03  FILLER              PIC X(22)   VALUE                            
009700         'TRANSACTION LIST DATE:'.                                        
009800     03  W-DATUM-T1          PIC X(8).                                    
009900     03  FILLER              PIC X(7)   VALUE                             
010000         ' TIME: '.                                                       
010100     03  W-TID-T1            PIC X(4).                                    
010200 01  TITEL2.                                                              
010300     03  FILLER              PIC X(14)   VALUE                            
010400         ' W57027-001   '.                                                
010500     03  W-HEADER-T2         PIC X(15).                                   
010800     03  FILLER              PIC X(22)   VALUE                            
010900         'TRANSACTION LIST DATE:'.                                        
011000     03  W-DATUM-T2          PIC X(8).                                    
011100     03  FILLER              PIC X(7)   VALUE                             
011200         ' TIME: '.                                                       
011300     03  W-TID-T2            PIC X(4).                                    
011400                                                                          
011500 01  RUBRIK1A.                                                            
011600     03  FILLER              PIC X(12) VALUE                              
011700         ' INV/CR:    '.                                                  
011800     03  W-IDVERGL-RUBR1A    PIC X(10).                                   
011900     03  FILLER              PIC X(12) VALUE                              
012000         ' VER.DATE:  '.                                                  
012100     03  W-DAVERDAT-RUBR1A   PIC X(10).                                   
012200     03  FILLER              PIC X(12) VALUE                              
012300         ' EVENT :    '.                                                  
012400     03  W-KDEKHHT-RUBR1A    PIC X(3).                                    
012500     03  FILLER REDEFINES W-KDEKHHT-RUBR1A.                               
012600         05 W-KDEKHHT-DEL1   PIC X(2).                                    
012700         05 W-KDEKHHT-DEL2   PIC X.                                       
012800 01  RUBRIK1B.                                                            
012900     03  FILLER              PIC X(12) VALUE                              
013000         ' REF.NR:    '.                                                  
013100     03  W-IDVERGL-RUBR1B    PIC X(10).                                   
013200     03  FILLER              PIC X(12) VALUE                              
013300         ' VER.DATE:  '.                                                  
013400     03  W-DAVERDAT-RUBR1B   PIC X(10).                                   
013500     03  FILLER              PIC X(12) VALUE                              
013600         ' EVENT :    '.                                                  
013700     03  W-KDEKHHT-RUBR1B    PIC X(3).                                    
013800 01  RUBRIK2.                                                             
013900     03  FILLER              PIC X(30) VALUE                              
014000         ' EVENT    LEVELDOC DC     QTY '.                                
014100     03  FILLER              PIC X(52) VALUE                              
014200         '    PARTNO  PG ACCT/PARMA PK COSTCENTER INT ORDER NO'.          
014300     03  FILLER              PIC X(29)   VALUE                            
014400         '     AMOUNT     PROFIT CENTER'.                                 
014500     EJECT                                                                
014600 01  BLANK-RAD              PIC X    VALUE SPACE.                         
014700 01  DETALJRAD-1.                                                         
014800     03  FILLER                 PIC X.                                    
014900     03  RAD-HAENDELSE          PIC X(7).                                 
015000     03  FILLER REDEFINES RAD-HAENDELSE.                                  
015100         05  RAD-KDEKHHT        PIC X(3).                                 
015200         05  RAD-STRECK         PIC X.                                    
015300         05  RAD-KDEKSHT        PIC X(3).                                 
015400     03  FILLER                 PIC X(2)  VALUE SPACE.                    
015500     03  RAD-KDEKNIVA           PIC X(4).                                 
015600     03  FILLER                 PIC X(1)  VALUE SPACE.                    
015700     03  RAD-KDDOKTYP           PIC X(2).                                 
015800     03  FILLER                 PIC X(2)  VALUE SPACE.                    
015900     03  RAD-IDDC               PIC X(2).                                 
016000     03  FILLER                 PIC X(1)  VALUE SPACE.                    
016100     03  RAD-KVANTAL            PIC X(7).                                 
016200     03  FILLER REDEFINES RAD-KVANTAL.                                    
016300         05  RAD-KVANTAL-1      PIC X(6).                                 
016400         05  RAD-KVANTAL-2      PIC X(1).                                 
016500     03  RAD-KVANTAL-TKN        PIC X(1).                                 
016600     03  FILLER                 PIC X(1)  VALUE SPACE.                    
016700     03  RAD-IDARTNR            PIC Z(8)9.                                
016800     03  FILLER                 PIC X(1)  VALUE SPACE.                    
016900     03  RAD-KDPRODSL           PIC Z(3).                                 
017000     03  FILLER                 PIC X(1)  VALUE SPACE.                    
017100     03  RAD-IDKONTO            PIC X(10).                                
017200     03  FILLER                 PIC X(1)  VALUE SPACE.                    
017300     03  RAD-KDPOST             PIC X(2).                                 
017400     03  FILLER                 PIC X(1)  VALUE SPACE.                    
017500     03  RAD-IDKST              PIC X(10).                                
017600     03  FILLER                 PIC X(1)  VALUE SPACE.                    
017700     03  RAD-IDANALYS           PIC X(12).                                
017800     03  RAD-SUBEL              PIC Z(8)9.99.                             
017900     03  RAD-TECKEN             PIC X.                                    
018000     03  FILLER                 PIC X(3)  VALUE SPACE.                    
018100     03  RAD-IDPRCTR            PIC X(10).                                
018200     EJECT                                                                
018300 01  UT-HEADER1.                                                          
018400     03  UT-H-IDVERGL1       PIC X(15)    VALUE ' INVOICE CREDIT'.        
018500     03  FILLER              PIC X(01)    VALUE ';'.                      
018600     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
018700     03  FILLER              PIC X(01)    VALUE ';'.                      
018800     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
018900     03  FILLER              PIC X(01)    VALUE ';'.                      
019000     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
019100     03  FILLER              PIC X(01)    VALUE ';'.                      
019200     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
019300     03  FILLER              PIC X(01)    VALUE ';'.                      
019400     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
019500     03  FILLER              PIC X(01)    VALUE ';'.                      
019600     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
019700     03  FILLER              PIC X(01)    VALUE ';'.                      
019800     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
019900     03  FILLER              PIC X(01)    VALUE ';'.                      
020000     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
020100     03  FILLER              PIC X(01)    VALUE ';'.                      
020200     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
020300     03  FILLER              PIC X(01)    VALUE ';'.                      
020400     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
020500     03  FILLER              PIC X(01)    VALUE ';'.                      
020600     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
020700     03  FILLER              PIC X(01)    VALUE ';'.                      
020800     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
020900     03  FILLER              PIC X(01)    VALUE ';'.                      
021000     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
021100     03  FILLER              PIC X(01)    VALUE ';'.                      
021200     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
021300     03  FILLER              PIC X(01)    VALUE ';'.                      
021400     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
021500     03  FILLER              PIC X(01)    VALUE ';'.                      
021600     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
021700     03  FILLER              PIC X(01)    VALUE ';'.                      
021800     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
021900     03  FILLER              PIC X(01)    VALUE ';'.                      
022000                                                                          
022100 01  UT-HEADER2.                                                          
022200     03  UT-H-IDVERGL1       PIC X(11)    VALUE ' REF NUMBER'.            
022300     03  FILLER              PIC X(01)    VALUE ';'.                      
022400     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
022500     03  FILLER              PIC X(01)    VALUE ';'.                      
022600     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
022700     03  FILLER              PIC X(01)    VALUE ';'.                      
022800     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
022900     03  FILLER              PIC X(01)    VALUE ';'.                      
023000     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
023100     03  FILLER              PIC X(01)    VALUE ';'.                      
023200     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
023300     03  FILLER              PIC X(01)    VALUE ';'.                      
023400     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
023500     03  FILLER              PIC X(01)    VALUE ';'.                      
023600     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
023700     03  FILLER              PIC X(01)    VALUE ';'.                      
023800     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
023900     03  FILLER              PIC X(01)    VALUE ';'.                      
024000     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
024100     03  FILLER              PIC X(01)    VALUE ';'.                      
024200     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
024300     03  FILLER              PIC X(01)    VALUE ';'.                      
024400     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
024500     03  FILLER              PIC X(01)    VALUE ';'.                      
024600     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
024700     03  FILLER              PIC X(01)    VALUE ';'.                      
024800     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
024900     03  FILLER              PIC X(01)    VALUE ';'.                      
025000     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
025100     03  FILLER              PIC X(01)    VALUE ';'.                      
025200     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
025300     03  FILLER              PIC X(01)    VALUE ';'.                      
025400     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
025500     03  FILLER              PIC X(01)    VALUE ';'.                      
025600     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
025700     03  FILLER              PIC X(01)    VALUE ';'.                      
025800                                                                          
025810 01  UT-HEADER-SF.                                                        
025820     03  UT-H-IDVERGL-SF     PIC X(15)    VALUE                           
025821                                          'DOCUMENT NUMBER'.              
025830     03  FILLER              PIC X(01)    VALUE ';'.                      
025840     03  UT-H-DATUM-T1-SF    PIC X(09)    VALUE 'LIST DATE'.              
025850     03  FILLER              PIC X(01)    VALUE ';'.                      
025860     03  UT-H-DAVERDAT-SF    PIC X(13)    VALUE 'DOCUMENT DATE'.          
025870     03  FILLER              PIC X(01)    VALUE ';'.                      
025880     03  UT-H-KDEKHHT-SF     PIC X(10)    VALUE 'MAIN EVENT'.             
025890     03  FILLER              PIC X(01)    VALUE ';'.                      
025891     03  UT-H-KDEKSHT-SF     PIC X(09)    VALUE 'SUB EVENT'.              
025892     03  FILLER              PIC X(01)    VALUE ';'.                      
025893     03  UT-H-KDEKNIVA-SF    PIC X(11)    VALUE 'EVENT LEVEL'.            
025894     03  FILLER              PIC X(01)    VALUE ';'.                      
025895     03  UT-H-KDDOKTYP-SF    PIC X(13)    VALUE 'DOCUMENT TYPE'.          
025896     03  FILLER              PIC X(01)    VALUE ';'.                      
025899     03  UT-H-IDDC-SF        PIC X(02)    VALUE 'DC'.                     
025900     03  FILLER              PIC X(01)    VALUE ';'.                      
025903     03  UT-H-KVANTAL-SF     PIC X(08)    VALUE 'QUANTITY'.               
025904     03  FILLER              PIC X(01)    VALUE ';'.                      
025907     03  UT-H-IDARTNR-SF     PIC X(07)    VALUE 'PART NO'.                
025908     03  FILLER              PIC X(01)    VALUE ';'.                      
025909     03  UT-H-KDPRODSL-SF    PIC X(13)    VALUE 'PRODUCT GROUP'.          
025910     03  FILLER              PIC X(01)    VALUE ';'.                      
025911     03  UT-H-IDKONTO-SF     PIC X(07)    VALUE 'ACCOUNT'.                
025912     03  FILLER              PIC X(01)    VALUE ';'.                      
025913     03  UT-H-KDPOST-SF      PIC X(11)    VALUE 'POSTING KEY'.            
025914     03  FILLER              PIC X(01)    VALUE ';'.                      
025915     03  UT-H-IDKST-SF       PIC X(10)    VALUE 'COSTCENTER'.             
025916     03  FILLER              PIC X(01)    VALUE ';'.                      
025917     03  UT-H-IDTECKEN-SF    PIC X(12)    VALUE 'ORDER NUMBER'.           
025918     03  FILLER              PIC X(01)    VALUE ';'.                      
025921     03  UT-H-SUBEL-SF       PIC X(06)    VALUE 'AMOUNT'.                 
025922     03  FILLER              PIC X(01)    VALUE ';'.                      
025923     03  UT-H-IDPRCTR-SF     PIC X(13)    VALUE 'PROFIT CENTER'.          
025924     03  FILLER              PIC X(01)    VALUE ';'.                      
025925     03  UT-H-PRARTSTD-SF    PIC X(05)    VALUE 'PRICE'.                  
025926     03  FILLER              PIC X(01)    VALUE ';'.                      
025927     03  UT-H-KDTRADP-SF     PIC X(15)    VALUE 'TRADING PARTNER'.        
025928     03  FILLER              PIC X(01)    VALUE ';'.                      
025929     03  UT-H-FLLSBOK-SF     PIC X(21)    VALUE                           
025930                                          'FLAG FOR STOCK UPDATE'.        
025932                                                                          
025940 01  UT-AREA.                                                             
026000     03  FILLER                 PIC X(02)    VALUE SPACE.                 
026100     03  UT-IDVERGL             PIC X(10)    VALUE SPACE.                 
026200     03  FILLER                 PIC X(01)    VALUE ';'.                   
026300     03  UT-DATUM-T1             PIC X(8)     VALUE SPACE.                
026400     03  FILLER                 PIC X(01)    VALUE ';'.                   
026500     03  UT-DAVERDAT            PIC X(10)    VALUE SPACE.                 
026600     03  FILLER                 PIC X(01)    VALUE ';'.                   
026700     03  UT-HAENDELSE           PIC X(07).                                
026800     03  FILLER REDEFINES UT-HAENDELSE.                                   
026900         05  UT-KDEKHHT         PIC X(03).                                
027000         05  UT-STRECK          PIC X(01).                                
027100         05  UT-KDEKSHT         PIC X(03).                                
027200     03  FILLER                 PIC X(01).                                
027300     03  FILLER                 PIC X(01)    VALUE ';'.                   
027400     03  UT-KDEKNIVA            PIC X(04)    VALUE SPACE.                 
027500     03  FILLER                 PIC X(01)    VALUE ';'.                   
027600     03  UT-KDDOKTYP            PIC X(02)    VALUE SPACE.                 
027700     03  FILLER                 PIC X(01)    VALUE ';'.                   
027800     03  UT-IDDC                PIC X(02)    VALUE SPACE.                 
027900     03  FILLER                 PIC X(01)    VALUE ';'.                   
028000     03  UT-KVANTAL             PIC X(07)    VALUE SPACE.                 
028100     03  FILLER REDEFINES  UT-KVANTAL.                                    
028200         05  UT-KVANTAL-1       PIC X(6).                                 
028300         05  UT-KVANTAL-2       PIC X(1).                                 
028400     03  FILLER                 PIC X(01)    VALUE ';'.                   
028500     03  UT-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                 
028600     03  FILLER                 PIC X(01)    VALUE ';'.                   
028700     03  UT-IDARTNR             PIC Z(8)9    VALUE ZERO.                  
028800     03  FILLER                 PIC X(01)    VALUE ';'.                   
028900     03  UT-KDPRODSL            PIC Z(3)     VALUE ZERO.                  
029000     03  FILLER                 PIC X(01)    VALUE ';'.                   
029100     03  UT-IDKONTO             PIC X(10)    VALUE SPACE.                 
029200     03  FILLER                 PIC X(01)    VALUE ';'.                   
029300     03  UT-KDPOST              PIC X(02)    VALUE SPACE.                 
029400     03  FILLER                 PIC X(01)    VALUE ';'.                   
029500     03  UT-IDKST               PIC X(10)    VALUE SPACE.                 
029600     03  FILLER                 PIC X(01)    VALUE ';'.                   
029700     03  UT-IDANALYS            PIC X(12)    VALUE SPACE.                 
029800     03  FILLER                 PIC X(01)    VALUE ';'.                   
029900     03  UT-SUBEL               PIC Z(8)9.99 VALUE ZERO.                  
030000     03  FILLER                 PIC X(01)    VALUE ';'.                   
030100     03  UT-TECKEN              PIC X(01)    VALUE SPACE.                 
030200     03  FILLER                 PIC X(01)    VALUE ';'.                   
030300     03  UT-IDPRCTR             PIC X(10)    VALUE SPACE.                 
030400     03  FILLER                 PIC X(01)    VALUE ';'.                   
030401                                                                          
030410 01  UT-AREA-SF.                                                          
030430     03  UT-IDVERGL-SF          PIC X(10)    VALUE SPACE.                 
030440     03  FILLER                 PIC X(01)    VALUE ';'.                   
030450     03  UT-DATUM-T1-SF         PIC X(8)     VALUE SPACE.                 
030460     03  FILLER                 PIC X(01)    VALUE ';'.                   
030470     03  UT-DAVERDAT-SF         PIC X(10)    VALUE SPACE.                 
030480     03  FILLER                 PIC X(01)    VALUE ';'.                   
030492     03  UT-KDEKHHT-SF          PIC X(03)    VALUE SPACE.                 
030493     03  FILLER                 PIC X(01)    VALUE ';'.                   
030494     03  UT-KDEKSHT-SF          PIC X(03)    VALUE SPACE.                 
030497     03  FILLER                 PIC X(01)    VALUE ';'.                   
030498     03  UT-KDEKNIVA-SF         PIC X(04)    VALUE SPACE.                 
030499     03  FILLER                 PIC X(01)    VALUE ';'.                   
030500     03  UT-KDDOKTYP-SF         PIC X(02)    VALUE SPACE.                 
030501     03  FILLER                 PIC X(01)    VALUE ';'.                   
030502     03  UT-IDDC-SF             PIC X(02)    VALUE SPACE.                 
030503     03  FILLER                 PIC X(01)    VALUE ';'.                   
030504     03  UT-KVANTAL-SF          PIC -(7)9    VALUE SPACE.                 
030508     03  FILLER                 PIC X(01)    VALUE ';'.                   
030511     03  UT-IDARTNR-SF          PIC Z(8)9    VALUE ZERO.                  
030512     03  FILLER                 PIC X(01)    VALUE ';'.                   
030513     03  UT-KDPRODSL-SF         PIC Z(2)9    VALUE ZERO.                  
030514     03  FILLER                 PIC X(01)    VALUE ';'.                   
030515     03  UT-IDKONTO-SF          PIC Z(9)9    VALUE ZERO.                  
030516     03  FILLER                 PIC X(01)    VALUE ';'.                   
030517     03  UT-KDPOST-SF           PIC X(02)    VALUE SPACE.                 
030518     03  FILLER                 PIC X(01)    VALUE ';'.                   
030519     03  UT-IDKST-SF            PIC X(10)    VALUE SPACE.                 
030520     03  FILLER                 PIC X(01)    VALUE ';'.                   
030521     03  UT-IDANALYS-SF         PIC X(12)    VALUE SPACE.                 
030522     03  FILLER                 PIC X(01)    VALUE ';'.                   
030525     03  UT-SUBEL-SF            PIC -(9)9.99 VALUE ZERO.                  
030526     03  FILLER                 PIC X(01)    VALUE ';'.                   
030528     03  UT-IDPRCTR-SF          PIC X(10)    VALUE SPACE.                 
030529     03  FILLER                 PIC X(01)    VALUE ';'.                   
030530     03  UT-PRARTSTD-SF         PIC -(7)9.99 VALUE ZERO.                  
030531     03  FILLER                 PIC X(01)    VALUE ';'.                   
030534     03  UT-KDTRADP-SF          PIC X(04)    VALUE SPACE.                 
030535     03  FILLER                 PIC X(01)    VALUE ';'.                   
030536     03  UT-FLLSBOK-SF          PIC X(01)    VALUE SPACE.                 
030540     EJECT                                                                
030600                                                                          
030700 PROCEDURE DIVISION.                                                      
030800     PERFORM A-INITIERA                                                   
030900                                                                          
031000     PERFORM S01-LAS-W57027                                               
031100     PERFORM UNTIL IN-EOF = JA                                            
031400       PERFORM B-REDIGERA-DETALJRAD                                       
031500                                                                          
031600       PERFORM S01-LAS-W57027                                             
031700     END-PERFORM                                                          
031800                                                                          
031900     PERFORM Z-AVSLUTA                                                    
032000     MOVE ZERO TO RETURN-CODE                                             
032100     GOBACK                                                               
032200     .                                                                    
032300                                                                          
032400 A-INITIERA SECTION.                                                      
032500     OPEN INPUT  W57027                                                   
032600          OUTPUT LISTA1                                                   
032700                 LISTA2                                                   
032800                 LISTA3                                                   
032900                 LISTA4                                                   
032910                 LISTA5                                                   
033000                                                                          
033100     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATUM-T1                        
033200                                        W-DATUM-T2                        
033300     MOVE FUNCTION CURRENT-DATE(9:4) TO W-TID-T1                          
033400                                        W-TID-T2                          
033500     MOVE SPACES TO WS-PREV-KDTRADP-1                                     
033501     MOVE SPACES TO WS-PREV-KDTRADP-2                                     
033510     PERFORM AA-WRITE-HEADER                                              
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 B-REDIGERA-DETALJRAD SECTION.                                            
034000                                                                          
034010     INITIALIZE WS-SUBEL                                                  
034100     IF (IN-KVANTAL NOT = W-ANTAL) OR                                     
034200        (IN-IDARTNR NOT = W-IDARTNR) OR                                   
034300        (IN-IDVERGL NOT = W-IDVERGL)                                      
034400        MOVE IN-KVANTAL     TO RAD-KVANTAL                                
034500        INSPECT RAD-KVANTAL-1 REPLACING LEADING ZERO BY SPACE             
034600        IF IN-KVANTAL < 0                                                 
034700           MOVE '-'   TO RAD-KVANTAL-TKN                                  
034800        ELSE                                                              
034900           MOVE SPACE TO RAD-KVANTAL-TKN                                  
035000        END-IF                                                            
035100        MOVE IN-KVANTAL     TO UT-KVANTAL                                 
035110                               UT-KVANTAL-SF                              
035200        INSPECT UT-KVANTAL-1 REPLACING LEADING ZERO BY SPACE              
035300        IF IN-KVANTAL < 0                                                 
035400           MOVE '-'   TO UT-KVANTAL-TKN                                   
035500        ELSE                                                              
035600           MOVE SPACE TO UT-KVANTAL-TKN                                   
035700        END-IF                                                            
035800     END-IF                                                               
036900     MOVE IN-KDEKHHT        TO RAD-KDEKHHT                                
037000     MOVE '-'               TO RAD-STRECK                                 
037100     MOVE IN-KDEKSHT        TO RAD-KDEKSHT                                
037200     MOVE IN-KDEKNIVA       TO RAD-KDEKNIVA                               
037300     MOVE IN-KDDOKTYP       TO RAD-KDDOKTYP                               
037400     MOVE IN-IDDC           TO RAD-IDDC                                   
037500     MOVE IN-IDARTNR        TO RAD-IDARTNR                                
037600     MOVE IN-KDPRODSL       TO RAD-KDPRODSL                               
037700     MOVE IN-IDKONTO        TO RAD-IDKONTO                                
037800     INSPECT RAD-IDKONTO REPLACING LEADING ZERO BY SPACE                  
037900     MOVE IN-IDKST          TO RAD-IDKST                                  
038000     MOVE IN-IDANALYS       TO RAD-IDANALYS                               
038100     MOVE IN-SUBEL          TO RAD-SUBEL                                  
038200     MOVE IN-IDTECKEN       TO RAD-TECKEN                                 
038300     MOVE IN-IDPRCTR        TO RAD-IDPRCTR                                
038400     MOVE IN-KDPOST         TO RAD-KDPOST                                 
038500     MOVE IN-KDTRADP        TO W-KDTRADP                                  
038700****FOR REPORT                                                            
038800     MOVE IN-IDVERGL        TO UT-IDVERGL                                 
038810                               UT-IDVERGL-SF                              
038900     MOVE W-DATUM-T1        TO UT-DATUM-T1                                
038910                               UT-DATUM-T1-SF                             
039000     MOVE IN-DAVERDAT       TO UT-DAVERDAT                                
039010                               UT-DAVERDAT-SF                             
039100     MOVE IN-KDEKHHT        TO UT-KDEKHHT                                 
039110                               UT-KDEKHHT-SF                              
039200     MOVE '-'               TO UT-STRECK                                  
039300     MOVE IN-KDEKSHT        TO UT-KDEKSHT                                 
039310                               UT-KDEKSHT-SF                              
039400     MOVE IN-KDEKNIVA       TO UT-KDEKNIVA                                
039410                               UT-KDEKNIVA-SF                             
039500     MOVE IN-KDDOKTYP       TO UT-KDDOKTYP                                
039510                               UT-KDDOKTYP-SF                             
039600     MOVE IN-IDDC           TO UT-IDDC                                    
039610                               UT-IDDC-SF                                 
039700     MOVE IN-IDARTNR        TO UT-IDARTNR                                 
039710                               UT-IDARTNR-SF                              
039800     MOVE IN-KDPRODSL       TO UT-KDPRODSL                                
039810                               UT-KDPRODSL-SF                             
039900     MOVE IN-IDKONTO        TO UT-IDKONTO                                 
039910                               UT-IDKONTO-SF                              
040000     INSPECT UT-IDKONTO REPLACING LEADING ZERO BY SPACE                   
040010     INSPECT UT-IDKONTO-SF REPLACING LEADING ZERO BY SPACE                
040100     MOVE IN-IDKST          TO UT-IDKST                                   
040110                               UT-IDKST-SF                                
040200     MOVE IN-IDANALYS       TO UT-IDANALYS                                
040210                               UT-IDANALYS-SF                             
040300     MOVE IN-SUBEL          TO UT-SUBEL                                   
040310                               WS-SUBEL                                   
040400     MOVE IN-IDTECKEN       TO UT-TECKEN                                  
040410     IF IN-IDTECKEN = '-'                                                 
040420        COMPUTE WS-SUBEL = 0 - WS-SUBEL                                   
040421        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
040422     ELSE                                                                 
040423        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
040430     END-IF                                                               
040500     MOVE IN-IDPRCTR        TO UT-IDPRCTR                                 
040510                               UT-IDPRCTR-SF                              
040600     MOVE IN-KDPOST         TO UT-KDPOST                                  
040610                               UT-KDPOST-SF                               
040620     MOVE IN-PRARTSTD       TO UT-PRARTSTD-SF                             
040630     MOVE IN-FLLSBOK        TO UT-FLLSBOK-SF                              
040640     MOVE IN-KDTRADP        TO UT-KDTRADP-SF                              
040700     IF IN-KDEKHHT = '303'                                                
040800                  OR '304'                                                
040900                  OR '201'                                                
041000                  OR '202'                                                
041100                  OR '203'                                                
041200                  OR '204'                                                
041300                  OR '205'                                                
041400                  OR '501'                                                
041410     PERFORM BA1-WRITE-HEADER                                             
041500        PERFORM S02-SKRIV-LISTA1                                          
041600     ELSE                                                                 
041610     PERFORM BA2-WRITE-HEADER                                             
041700        PERFORM S03-SKRIV-LISTA2                                          
041800     END-IF                                                               
041900                                                                          
041920     PERFORM S04-SKRIV-LIST-SF                                            
041930                                                                          
042000     MOVE IN-IDVERGL        TO W-IDVERGL                                  
042100     MOVE IN-IDARTNR        TO W-IDARTNR                                  
042200     MOVE IN-KVANTAL        TO W-ANTAL                                    
042300     MOVE SPACE             TO DETALJRAD-1                                
042600     .                                                                    
042700     EJECT                                                                
042800                                                                          
042900 Z-AVSLUTA SECTION.                                                       
043000     CLOSE LISTA1  W57027 LISTA3 LISTA5                                   
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400 S01-LAS-W57027 SECTION.                                                  
043500     READ W57027 INTO INAREA                                              
043600        AT END MOVE JA TO IN-EOF                                          
043700     END-READ                                                             
043800     .                                                                    
043900 S11-WRITE-HEADERS SECTION.                                               
044000                                                                          
044100     .                                                                    
044200                                                                          
044300 S02-SKRIV-LISTA1 SECTION.                                                
044400                                                                          
044500     IF IN-IDVERGL NOT = W-IDVERGL-1                                      
044600       MOVE W-KDTRADP TO W-HEADER-T1                                      
044700       WRITE LISTPOST1 FROM TITEL1  AFTER PAGE                            
044800       MOVE IN-IDVERGL        TO W-IDVERGL-RUBR1A                         
044900       INSPECT W-IDVERGL-RUBR1A REPLACING LEADING ZERO BY SPACE           
045000       MOVE IN-DAVERDAT       TO W-DAVERDAT-RUBR1A                        
045100       MOVE IN-KDEKHHT (1:2)  TO W-KDEKHHT-DEL1                           
045200       MOVE 'X' TO W-KDEKHHT-DEL2                                         
045300                                                                          
045400       WRITE LISTPOST1        FROM RUBRIK1A AFTER 1                       
045500       WRITE LISTPOST1        FROM RUBRIK2 AFTER 1                        
045600       MOVE IN-IDVERGL        TO W-IDVERGL-1                              
045700     END-IF                                                               
045800                                                                          
045900     WRITE LISTPOST1          FROM DETALJRAD-1 AFTER 1                    
046000     WRITE LISTPOST3          FROM UT-AREA AFTER 1                        
046100     .                                                                    
046200                                                                          
046300 S03-SKRIV-LISTA2 SECTION.                                                
046400                                                                          
046500     IF IN-IDVERGL NOT = W-IDVERGL-2                                      
046600       MOVE W-KDTRADP TO W-HEADER-T2                                      
046700       WRITE LISTPOST2     FROM TITEL2 AFTER PAGE                         
046800       MOVE IN-IDVERGL     TO W-IDVERGL-RUBR1B                            
046900       INSPECT W-IDVERGL-RUBR1B REPLACING LEADING ZERO BY SPACE           
047000       MOVE IN-DAVERDAT    TO W-DAVERDAT-RUBR1B                           
047100       MOVE IN-KDEKHHT     TO W-KDEKHHT-RUBR1B                            
047200                                                                          
047300       WRITE LISTPOST2     FROM RUBRIK1B AFTER 1                          
047400       WRITE LISTPOST2     FROM RUBRIK2 AFTER 1                           
047500       MOVE IN-IDVERGL     TO W-IDVERGL-2                                 
047600     END-IF                                                               
047700                                                                          
047800     WRITE LISTPOST2       FROM DETALJRAD-1 AFTER 1                       
047900     WRITE LISTPOST4       FROM UT-AREA AFTER 1                           
048000     .                                                                    
048010 S04-SKRIV-LIST-SF SECTION.                                               
048020                                                                          
048098     WRITE LISTPOST5       FROM UT-AREA-SF                                
048099     .                                                                    
048100     EJECT                                                                
048110 BA1-WRITE-HEADER SECTION.                                                
048200                                                                          
048310     IF IN-KDTRADP NOT = WS-PREV-KDTRADP-1                                
048400     AND IN-KDTRADP(1:2) NOT = WS-PREV-KDTRADP-1(1:2)                     
048500       MOVE IN-KDTRADP   TO WS-PREV-KDTRADP-1                             
048800       PERFORM S10-WRITE-DAP-001                                          
048900     WRITE LISTPOST3           FROM UT-HEADER1                            
049110     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049301 BA2-WRITE-HEADER SECTION.                                                
049302                                                                          
049303     IF IN-KDTRADP NOT = WS-PREV-KDTRADP-2                                
049304     AND IN-KDTRADP(1:2) NOT = WS-PREV-KDTRADP-2(1:2)                     
049305       MOVE IN-KDTRADP   TO WS-PREV-KDTRADP-2                             
049306       PERFORM S10-WRITE-DAP-002                                          
049308     WRITE LISTPOST4           FROM UT-HEADER2                            
049309     END-IF                                                               
049310     .                                                                    
049311     EJECT                                                                
049312 AA-WRITE-HEADER SECTION.                                                 
049320                                                                          
049380     WRITE LISTPOST5           FROM UT-HEADER-SF                          
049391     .                                                                    
049392     EJECT                                                                
049400 S10-WRITE-DAP-001 SECTION.                                               
049500                                                                          
049700     MOVE ' ¤DAPW57027-001' TO W001-DAP                                   
049800     WRITE LISTPOST3  FROM W001-DAP                                       
049900     MOVE SPACES            TO W001-DAP                                   
050000     STRING ' ¤DAP' WS-PREV-KDTRADP-1                                     
050100            DELIMITED BY SIZE INTO W001-DAP                               
050200     WRITE LISTPOST3  FROM W001-DAP                                       
051000     MOVE SPACES            TO W001-DAP                                   
051200     .                                                                    
051300     EJECT                                                                
051400 S10-WRITE-DAP-002 SECTION.                                               
051500                                                                          
052300     MOVE ' ¤DAPW57027-002' TO W001-DAP                                   
052400     WRITE LISTPOST4  FROM W001-DAP                                       
052500     MOVE SPACES            TO W001-DAP                                   
052600     STRING ' ¤DAP' WS-PREV-KDTRADP-2                                     
052700            DELIMITED BY SIZE INTO W001-DAP                               
052800     WRITE LISTPOST4  FROM W001-DAP                                       
052900     MOVE SPACES            TO W001-DAP                                   
053000     .                                                                    
053100     EJECT                                                                
