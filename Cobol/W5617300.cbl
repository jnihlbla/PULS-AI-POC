000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5617300.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   NOV 2017.                                                
000500*    REMARKS.                                                             
000600*                SKAPAR HÄNDELSELISTOR TILL ON-DEMAND                     
000700*                                                                         
000800*        INDATA  W56175                                                   
000900*                                                                         
001000*                                                                         
001100     EJECT                                                                
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SELECT W56175              ASSIGN TO UT-S-W56173D1.                  
001900     SELECT LISTA1              ASSIGN TO UT-S-W56173D2.                  
002000     SELECT LISTA2              ASSIGN TO UT-S-W56173D3.                  
002010     SELECT LISTA3              ASSIGN TO UT-S-W56173D4.                  
002020     SELECT LISTA4              ASSIGN TO UT-S-W56173D5.                  
002030     SELECT LISTA5              ASSIGN TO UT-S-W56173D6.                  
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W56175                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900*01  -COPY W56173     -L.                                                 
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
003901                                                                          
003910 FD  LISTA3                                                               
003920     RECORDING V                                                          
003930     BLOCK CONTAINS 0.                                                    
003940 01  LISTPOST3                    PIC X(165).                             
003950                                                                          
003960 FD  LISTA4                                                               
003970     RECORDING V                                                          
003980     BLOCK CONTAINS 0.                                                    
003990 01  LISTPOST4                    PIC X(165).                             
003991                                                                          
003992 FD  LISTA5                                                               
003993     RECORDING F                                                          
003994     BLOCK CONTAINS 0.                                                    
003995 01  LISTPOST5                    PIC X(229).                             
004000     EJECT                                                                
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM               PIC X(8)        VALUE 'W5617300'.                
004500 77  JA                  PIC X           VALUE 'J'.                       
004600 77  NEJ                 PIC X           VALUE 'N'.                       
004700 77  IN-EOF              PIC X           VALUE 'N'.                       
004701 77  W56173D3-WRITE-HDR-SW       PIC X       VALUE 'N'.                   
004702     88  WRITED3-YES                         VALUE 'J'.                   
004703 77  W56173D4-WRITE-HDR-SW       PIC X       VALUE 'N'.                   
004704     88  WRITED4-YES                         VALUE 'J'.                   
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
006100*    03  -COPY W56173  -PRE IN-.                                          
006200     EJECT                                                                
006300                                                                          
006400 01  TITEL1.                                                              
006500     03  FILLER              PIC X(29)   VALUE                            
006600         ' W56173-001   VCUS           '.                                 
006700     03  FILLER              PIC X(22)   VALUE                            
006800         'TRANSACTION LIST DATE:'.                                        
006900     03  W-DATUM-T1          PIC X(8).                                    
007000     03  FILLER              PIC X(7)   VALUE                             
007100         ' TIME: '.                                                       
007200     03  W-TID-T1            PIC X(4).                                    
007300 01  TITEL2.                                                              
007400     03  FILLER              PIC X(29)   VALUE                            
007500         ' W56173-002   VCUS           '.                                 
007600     03  FILLER              PIC X(22)   VALUE                            
007700         'TRANSACTION LISTDATE:'.                                         
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
014800     03  RAD-TECKEN             PIC X.                                    
014900     03  FILLER                 PIC X(3)  VALUE SPACE.                    
015000     03  RAD-IDPRCTR            PIC X(10).                                
015100     EJECT                                                                
015200                                                                          
015210 01  UT-HEADER1.                                                          
015220     03  UT-H-IDVERGL1       PIC X(15)    VALUE ' INVOICE CREDIT'.        
015230     03  FILLER              PIC X(01)    VALUE ';'.                      
015240     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
015250     03  FILLER              PIC X(01)    VALUE ';'.                      
015260     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
015270     03  FILLER              PIC X(01)    VALUE ';'.                      
015280     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
015290     03  FILLER              PIC X(01)    VALUE ';'.                      
015291     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
015292     03  FILLER              PIC X(01)    VALUE ';'.                      
015293     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
015294     03  FILLER              PIC X(01)    VALUE ';'.                      
015295     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
015296     03  FILLER              PIC X(01)    VALUE ';'.                      
015297     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
015298     03  FILLER              PIC X(01)    VALUE ';'.                      
015299     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
015300     03  FILLER              PIC X(01)    VALUE ';'.                      
015301     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
015302     03  FILLER              PIC X(01)    VALUE ';'.                      
015303     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
015304     03  FILLER              PIC X(01)    VALUE ';'.                      
015305     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
015306     03  FILLER              PIC X(01)    VALUE ';'.                      
015307     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
015308     03  FILLER              PIC X(01)    VALUE ';'.                      
015309     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
015310     03  FILLER              PIC X(01)    VALUE ';'.                      
015311     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
015312     03  FILLER              PIC X(01)    VALUE ';'.                      
015313     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
015314     03  FILLER              PIC X(01)    VALUE ';'.                      
015315     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
015316     03  FILLER              PIC X(01)    VALUE ';'.                      
015317     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
015318     03  FILLER              PIC X(01)    VALUE ';'.                      
015319                                                                          
015320 01  UT-HEADER2.                                                          
015321     03  UT-H-IDVERGL1       PIC X(11)    VALUE ' REF NUMBER'.            
015322     03  FILLER              PIC X(01)    VALUE ';'.                      
015323     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
015324     03  FILLER              PIC X(01)    VALUE ';'.                      
015325     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
015326     03  FILLER              PIC X(01)    VALUE ';'.                      
015327     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
015328     03  FILLER              PIC X(01)    VALUE ';'.                      
015329     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
015330     03  FILLER              PIC X(01)    VALUE ';'.                      
015331     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
015332     03  FILLER              PIC X(01)    VALUE ';'.                      
015333     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
015334     03  FILLER              PIC X(01)    VALUE ';'.                      
015335     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
015336     03  FILLER              PIC X(01)    VALUE ';'.                      
015337     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
015338     03  FILLER              PIC X(01)    VALUE ';'.                      
015339     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
015340     03  FILLER              PIC X(01)    VALUE ';'.                      
015341     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
015342     03  FILLER              PIC X(01)    VALUE ';'.                      
015343     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
015344     03  FILLER              PIC X(01)    VALUE ';'.                      
015345     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
015346     03  FILLER              PIC X(01)    VALUE ';'.                      
015347     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
015348     03  FILLER              PIC X(01)    VALUE ';'.                      
015349     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
015350     03  FILLER              PIC X(01)    VALUE ';'.                      
015351     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
015352     03  FILLER              PIC X(01)    VALUE ';'.                      
015353     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
015354     03  FILLER              PIC X(01)    VALUE ';'.                      
015355     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
015356     03  FILLER              PIC X(01)    VALUE ';'.                      
015357                                                                          
015358 01  UT-HEADER-SF.                                                        
015359     03  UT-H-IDVERGL-SF     PIC X(15)    VALUE                           
015360                                          'DOCUMENT NUMBER'.              
015361     03  FILLER              PIC X(01)    VALUE ';'.                      
015362     03  UT-H-DATUM-T1-SF    PIC X(09)    VALUE 'LIST DATE'.              
015363     03  FILLER              PIC X(01)    VALUE ';'.                      
015364     03  UT-H-DAVERDAT-SF    PIC X(13)    VALUE 'DOCUMENT DATE'.          
015365     03  FILLER              PIC X(01)    VALUE ';'.                      
015366     03  UT-H-KDEKHHT-SF     PIC X(10)    VALUE 'MAIN EVENT'.             
015367     03  FILLER              PIC X(01)    VALUE ';'.                      
015368     03  UT-H-KDEKSHT-SF     PIC X(09)    VALUE 'SUB EVENT'.              
015369     03  FILLER              PIC X(01)    VALUE ';'.                      
015370     03  UT-H-KDEKNIVA-SF    PIC X(11)    VALUE 'EVENT LEVEL'.            
015371     03  FILLER              PIC X(01)    VALUE ';'.                      
015372     03  UT-H-KDDOKTYP-SF    PIC X(13)    VALUE 'DOCUMENT TYPE'.          
015373     03  FILLER              PIC X(01)    VALUE ';'.                      
015374     03  UT-H-IDDC-SF        PIC X(02)    VALUE 'DC'.                     
015375     03  FILLER              PIC X(01)    VALUE ';'.                      
015376     03  UT-H-KVANTAL-SF     PIC X(08)    VALUE 'QUANTITY'.               
015377     03  FILLER              PIC X(01)    VALUE ';'.                      
015378     03  UT-H-IDARTNR-SF     PIC X(07)    VALUE 'PART NO'.                
015379     03  FILLER              PIC X(01)    VALUE ';'.                      
015380     03  UT-H-KDPRODSL-SF    PIC X(13)    VALUE 'PRODUCT GROUP'.          
015381     03  FILLER              PIC X(01)    VALUE ';'.                      
015382     03  UT-H-IDKONTO-SF     PIC X(07)    VALUE 'ACCOUNT'.                
015383     03  FILLER              PIC X(01)    VALUE ';'.                      
015384     03  UT-H-KDPOST-SF      PIC X(11)    VALUE 'POSTING KEY'.            
015385     03  FILLER              PIC X(01)    VALUE ';'.                      
015386     03  UT-H-IDKST-SF       PIC X(10)    VALUE 'COSTCENTER'.             
015387     03  FILLER              PIC X(01)    VALUE ';'.                      
015388     03  UT-H-IDTECKEN-SF    PIC X(12)    VALUE 'ORDER NUMBER'.           
015389     03  FILLER              PIC X(01)    VALUE ';'.                      
015390     03  UT-H-SUBEL-SF       PIC X(06)    VALUE 'AMOUNT'.                 
015391     03  FILLER              PIC X(01)    VALUE ';'.                      
015392     03  UT-H-IDPRCTR-SF     PIC X(13)    VALUE 'PROFIT CENTER'.          
015393     03  FILLER              PIC X(01)    VALUE ';'.                      
015394     03  UT-H-PRARTSTD-SF    PIC X(05)    VALUE 'PRICE'.                  
015395     03  FILLER              PIC X(01)    VALUE ';'.                      
015396     03  UT-H-KDTRADP-SF     PIC X(15)    VALUE 'TRADING PARTNER'.        
015397     03  FILLER              PIC X(01)    VALUE ';'.                      
015398     03  UT-H-FLLSBOK-SF     PIC X(21)    VALUE                           
015399                                          'FLAG FOR STOCK UPDATE'.        
015400                                                                          
015401 01  UT-AREA.                                                             
015402     03  FILLER                 PIC X(02)    VALUE SPACE.                 
015403     03  UT-IDVERGL             PIC X(10)    VALUE SPACE.                 
015404     03  FILLER                 PIC X(01)    VALUE ';'.                   
015405     03  UT-DATUM-T1             PIC X(8)     VALUE SPACE.                
015406     03  FILLER                 PIC X(01)    VALUE ';'.                   
015407     03  UT-DAVERDAT            PIC X(10)    VALUE SPACE.                 
015408     03  FILLER                 PIC X(01)    VALUE ';'.                   
015409     03  UT-HAENDELSE           PIC X(07).                                
015410     03  FILLER REDEFINES UT-HAENDELSE.                                   
015411         05  UT-KDEKHHT         PIC X(03).                                
015412         05  UT-STRECK          PIC X(01).                                
015413         05  UT-KDEKSHT         PIC X(03).                                
015414     03  FILLER                 PIC X(01).                                
015415     03  FILLER                 PIC X(01)    VALUE ';'.                   
015416     03  UT-KDEKNIVA            PIC X(04)    VALUE SPACE.                 
015417     03  FILLER                 PIC X(01)    VALUE ';'.                   
015418     03  UT-KDDOKTYP            PIC X(02)    VALUE SPACE.                 
015419     03  FILLER                 PIC X(01)    VALUE ';'.                   
015420     03  UT-IDDC                PIC X(02)    VALUE SPACE.                 
015421     03  FILLER                 PIC X(01)    VALUE ';'.                   
015422     03  UT-KVANTAL             PIC X(07)    VALUE SPACE.                 
015423     03  FILLER REDEFINES  UT-KVANTAL.                                    
015424         05  UT-KVANTAL-1       PIC X(6).                                 
015425         05  UT-KVANTAL-2       PIC X(1).                                 
015426     03  FILLER                 PIC X(01)    VALUE ';'.                   
015427     03  UT-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                 
015428     03  FILLER                 PIC X(01)    VALUE ';'.                   
015429     03  UT-IDARTNR             PIC Z(8)9    VALUE ZERO.                  
015430     03  FILLER                 PIC X(01)    VALUE ';'.                   
015431     03  UT-KDPRODSL            PIC Z(3)     VALUE ZERO.                  
015432     03  FILLER                 PIC X(01)    VALUE ';'.                   
015433     03  UT-IDKONTO             PIC 9(10)    VALUE ZERO.                  
015434     03  FILLER                 PIC X(01)    VALUE ';'.                   
015435     03  UT-KDPOST              PIC X(02)    VALUE SPACE.                 
015436     03  FILLER                 PIC X(01)    VALUE ';'.                   
015437     03  UT-IDKST               PIC X(10)    VALUE SPACE.                 
015438     03  FILLER                 PIC X(01)    VALUE ';'.                   
015439     03  UT-IDANALYS            PIC X(12)    VALUE SPACE.                 
015440     03  FILLER                 PIC X(01)    VALUE ';'.                   
015441     03  UT-SUBEL               PIC Z(8)9.99 VALUE ZERO.                  
015442     03  FILLER                 PIC X(01)    VALUE ';'.                   
015443     03  UT-TECKEN              PIC X(01)    VALUE SPACE.                 
015444     03  FILLER                 PIC X(01)    VALUE ';'.                   
015445     03  UT-IDPRCTR             PIC X(10)    VALUE SPACE.                 
015446     03  FILLER                 PIC X(01)    VALUE ';'.                   
015447                                                                          
015448 01  UT-AREA-SF.                                                          
015449     03  UT-IDVERGL-SF          PIC X(10)    VALUE SPACE.                 
015450     03  FILLER                 PIC X(01)    VALUE ';'.                   
015451     03  UT-DATUM-T1-SF         PIC X(8)     VALUE SPACE.                 
015452     03  FILLER                 PIC X(01)    VALUE ';'.                   
015453     03  UT-DAVERDAT-SF         PIC X(10)    VALUE SPACE.                 
015454     03  FILLER                 PIC X(01)    VALUE ';'.                   
015455     03  UT-KDEKHHT-SF          PIC X(03)    VALUE SPACE.                 
015456     03  FILLER                 PIC X(01)    VALUE ';'.                   
015457     03  UT-KDEKSHT-SF          PIC X(03)    VALUE SPACE.                 
015458     03  FILLER                 PIC X(01)    VALUE ';'.                   
015459     03  UT-KDEKNIVA-SF         PIC X(04)    VALUE SPACE.                 
015460     03  FILLER                 PIC X(01)    VALUE ';'.                   
015461     03  UT-KDDOKTYP-SF         PIC X(02)    VALUE SPACE.                 
015462     03  FILLER                 PIC X(01)    VALUE ';'.                   
015463     03  UT-IDDC-SF             PIC X(02)    VALUE SPACE.                 
015464     03  FILLER                 PIC X(01)    VALUE ';'.                   
015465     03  UT-KVANTAL-SF          PIC -(7)9    VALUE SPACE.                 
015466     03  FILLER                 PIC X(01)    VALUE ';'.                   
015467     03  UT-IDARTNR-SF          PIC Z(8)9    VALUE ZERO.                  
015468     03  FILLER                 PIC X(01)    VALUE ';'.                   
015469     03  UT-KDPRODSL-SF         PIC Z(2)9    VALUE ZERO.                  
015470     03  FILLER                 PIC X(01)    VALUE ';'.                   
015471     03  UT-IDKONTO-SF          PIC Z(9)9    VALUE ZERO.                  
015472     03  FILLER                 PIC X(01)    VALUE ';'.                   
015473     03  UT-KDPOST-SF           PIC X(02)    VALUE SPACE.                 
015474     03  FILLER                 PIC X(01)    VALUE ';'.                   
015475     03  UT-IDKST-SF            PIC X(10)    VALUE SPACE.                 
015476     03  FILLER                 PIC X(01)    VALUE ';'.                   
015477     03  UT-IDANALYS-SF         PIC X(12)    VALUE SPACE.                 
015478     03  FILLER                 PIC X(01)    VALUE ';'.                   
015479     03  UT-SUBEL-SF            PIC -(9)9.99 VALUE ZERO.                  
015480     03  FILLER                 PIC X(01)    VALUE ';'.                   
015481     03  UT-IDPRCTR-SF          PIC X(10)    VALUE SPACE.                 
015482     03  FILLER                 PIC X(01)    VALUE ';'.                   
015483     03  UT-PRARTSTD-SF         PIC -(7)9.99 VALUE ZERO.                  
015484     03  FILLER                 PIC X(01)    VALUE ';'.                   
015485     03  UT-KDTRADP-SF          PIC X(04)    VALUE SPACE.                 
015486     03  FILLER                 PIC X(01)    VALUE ';'.                   
015487     03  UT-FLLSBOK-SF          PIC X(01)    VALUE SPACE.                 
015488     EJECT                                                                
015489                                                                          
015490 PROCEDURE DIVISION.                                                      
015491     PERFORM A-INITIERA                                                   
015500                                                                          
015600     PERFORM S01-LAS-INFIL                                                
015800     PERFORM UNTIL IN-EOF = JA                                            
015900       PERFORM B-REDIGERA-DETALJRAD                                       
016000                                                                          
016100       PERFORM S01-LAS-INFIL                                              
016200     END-PERFORM                                                          
016300                                                                          
016400     PERFORM Z-AVSLUTA                                                    
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800                                                                          
016900 A-INITIERA SECTION.                                                      
017000     OPEN INPUT  W56175                                                   
017100          OUTPUT LISTA1                                                   
017200                 LISTA2                                                   
017210                 LISTA3                                                   
017220                 LISTA4                                                   
017230                 LISTA5                                                   
017300                                                                          
017400     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATUM-T1                        
017500                                        W-DATUM-T2                        
017600     MOVE FUNCTION CURRENT-DATE(9:4) TO W-TID-T1                          
017700                                        W-TID-T2                          
017710     PERFORM AA-WRITE-HEADER                                              
017800     .                                                                    
017900     EJECT                                                                
018000                                                                          
018100 B-REDIGERA-DETALJRAD SECTION.                                            
018200                                                                          
018210     INITIALIZE WS-SUBEL                                                  
018300     IF (IN-KVANTAL NOT = W-ANTAL) OR                                     
018400        (IN-IDARTNR NOT = W-IDARTNR) OR                                   
018500        (IN-IDVERGL NOT = W-IDVERGL)                                      
018600        MOVE IN-KVANTAL     TO RAD-KVANTAL                                
018700        INSPECT RAD-KVANTAL-1 REPLACING LEADING ZERO BY SPACE             
018800        IF IN-KVANTAL < 0                                                 
018900           MOVE '-'   TO RAD-KVANTAL-TKN                                  
018910        ELSE                                                              
018920           MOVE SPACE TO RAD-KVANTAL-TKN                                  
019000        END-IF                                                            
019010        MOVE IN-KVANTAL     TO UT-KVANTAL                                 
019011                               UT-KVANTAL-SF                              
019020        INSPECT UT-KVANTAL-1 REPLACING LEADING ZERO BY SPACE              
019030        IF IN-KVANTAL < 0                                                 
019040           MOVE '-'   TO UT-KVANTAL-TKN                                   
019041        ELSE                                                              
019042           MOVE SPACE TO UT-KVANTAL-TKN                                   
019050        END-IF                                                            
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
020400     MOVE IN-IDANALYS       TO RAD-IDANALYS                               
020500     MOVE IN-SUBEL          TO RAD-SUBEL                                  
020600     MOVE IN-IDTECKEN       TO RAD-TECKEN                                 
020700     MOVE IN-IDPRCTR        TO RAD-IDPRCTR                                
020800     MOVE IN-KDPOST         TO RAD-KDPOST                                 
020900****FOR REPORT                                                            
020910     MOVE IN-IDVERGL        TO UT-IDVERGL                                 
020911                               UT-IDVERGL-SF                              
020920     MOVE W-DATUM-T1        TO UT-DATUM-T1                                
020921                               UT-DATUM-T1-SF                             
020930     MOVE IN-DAVERDAT       TO UT-DAVERDAT                                
020931                               UT-DAVERDAT-SF                             
020940     MOVE IN-KDEKHHT        TO UT-KDEKHHT                                 
020941                               UT-KDEKHHT-SF                              
020950     MOVE '-'               TO UT-STRECK                                  
020960     MOVE IN-KDEKSHT        TO UT-KDEKSHT                                 
020961                               UT-KDEKSHT-SF                              
020970     MOVE IN-KDEKNIVA       TO UT-KDEKNIVA                                
020971                               UT-KDEKNIVA-SF                             
020980     MOVE IN-KDDOKTYP       TO UT-KDDOKTYP                                
020981                               UT-KDDOKTYP-SF                             
020990     MOVE IN-IDDC           TO UT-IDDC                                    
020991                               UT-IDDC-SF                                 
020992     MOVE IN-IDARTNR        TO UT-IDARTNR                                 
020993                               UT-IDARTNR-SF                              
020994     MOVE IN-KDPRODSL       TO UT-KDPRODSL                                
020995                               UT-KDPRODSL-SF                             
020996     MOVE IN-IDKONTO        TO UT-IDKONTO                                 
020997                               UT-IDKONTO-SF                              
020998     INSPECT UT-IDKONTO REPLACING LEADING ZERO BY SPACE                   
020999     INSPECT UT-IDKONTO-SF REPLACING LEADING ZERO BY SPACE                
021000     MOVE IN-IDKST          TO UT-IDKST                                   
021001                               UT-IDKST-SF                                
021002     MOVE IN-IDANALYS       TO UT-IDANALYS                                
021003                               UT-IDANALYS-SF                             
021004     MOVE IN-SUBEL          TO UT-SUBEL                                   
021005                               WS-SUBEL                                   
021006     MOVE IN-IDTECKEN       TO UT-TECKEN                                  
021007     IF IN-IDTECKEN = '-'                                                 
021008        COMPUTE WS-SUBEL = 0 - WS-SUBEL                                   
021009        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
021010     ELSE                                                                 
021011        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
021012     END-IF                                                               
021013     MOVE IN-IDPRCTR        TO UT-IDPRCTR                                 
021014                               UT-IDPRCTR-SF                              
021015     MOVE IN-KDPOST         TO UT-KDPOST                                  
021016                               UT-KDPOST-SF                               
021017     MOVE IN-PRARTSTD       TO UT-PRARTSTD-SF                             
021018     MOVE IN-FLLSBOK        TO UT-FLLSBOK-SF                              
021019     MOVE 'US01'            TO UT-KDTRADP-SF                              
021020     IF IN-KDEKHHT = '303'                                                
021100                  OR '304'                                                
021200                  OR '201'                                                
021300                  OR '202'                                                
021400                  OR '203'                                                
021500                  OR '204'                                                
021600                  OR '205'                                                
021700                  OR '501'                                                
021800        IF W56173D3-WRITE-HDR-SW = 'N'                                    
021801          WRITE LISTPOST3           FROM UT-HEADER1                       
021802        END-IF                                                            
021803        PERFORM S02-SKRIV-LISTA1                                          
021804        SET WRITED3-YES     TO TRUE                                       
021900     ELSE                                                                 
021901        IF W56173D4-WRITE-HDR-SW = 'N'                                    
021902          WRITE LISTPOST4           FROM UT-HEADER2                       
021903        END-IF                                                            
022000        PERFORM S03-SKRIV-LISTA2                                          
022010        SET WRITED4-YES     TO TRUE                                       
022100     END-IF                                                               
022200                                                                          
022210     PERFORM S04-SKRIV-LIST-SF                                            
022220                                                                          
022300     MOVE IN-IDVERGL        TO W-IDVERGL                                  
022400     MOVE IN-IDARTNR        TO W-IDARTNR                                  
022500     MOVE IN-KVANTAL        TO W-ANTAL                                    
022600     MOVE SPACE             TO DETALJRAD-1                                
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 Z-AVSLUTA SECTION.                                                       
023100     CLOSE LISTA1 W56175 LISTA2 LISTA3 LISTA4 LISTA5                      
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 S01-LAS-INFIL SECTION.                                                   
023600     READ W56175 INTO INAREA                                              
023700        AT END MOVE JA TO IN-EOF                                          
023800     END-READ                                                             
023900     .                                                                    
024000                                                                          
024100 S02-SKRIV-LISTA1 SECTION.                                                
024200                                                                          
024300     IF IN-IDVERGL NOT = W-IDVERGL-1                                      
024400       WRITE LISTPOST1 FROM TITEL1 AFTER PAGE                             
024500       MOVE IN-IDVERGL        TO W-IDVERGL-RUBR1A                         
024600       INSPECT W-IDVERGL-RUBR1A REPLACING LEADING ZERO BY SPACE           
024700       MOVE IN-DAVERDAT       TO W-DAVERDAT-RUBR1A                        
024800       MOVE IN-KDEKHHT (1:2)  TO W-KDEKHHT-DEL1                           
024900       MOVE 'X' TO W-KDEKHHT-DEL2                                         
025000                                                                          
025100       WRITE LISTPOST1        FROM RUBRIK1A AFTER 1                       
025200       WRITE LISTPOST1        FROM RUBRIK2 AFTER 1                        
025300       MOVE IN-IDVERGL        TO W-IDVERGL-1                              
025400     END-IF                                                               
025500                                                                          
025600     WRITE LISTPOST1          FROM DETALJRAD-1 AFTER 1                    
025610     WRITE LISTPOST3          FROM UT-AREA AFTER 1                        
025700     .                                                                    
025800                                                                          
025900 S03-SKRIV-LISTA2 SECTION.                                                
026000                                                                          
026100     IF IN-IDVERGL NOT = W-IDVERGL-2                                      
026200       WRITE LISTPOST2     FROM TITEL2 AFTER PAGE                         
026300       MOVE IN-IDVERGL     TO W-IDVERGL-RUBR1B                            
026400       INSPECT W-IDVERGL-RUBR1B REPLACING LEADING ZERO BY SPACE           
026500       MOVE IN-DAVERDAT    TO W-DAVERDAT-RUBR1B                           
026600       MOVE IN-KDEKHHT     TO W-KDEKHHT-RUBR1B                            
026700                                                                          
026800       WRITE LISTPOST2     FROM RUBRIK1B AFTER 1                          
026900       WRITE LISTPOST2     FROM RUBRIK2 AFTER 1                           
027000       MOVE IN-IDVERGL     TO W-IDVERGL-2                                 
027100     END-IF                                                               
027200                                                                          
027300     WRITE LISTPOST2       FROM DETALJRAD-1 AFTER 1                       
027310     WRITE LISTPOST4       FROM UT-AREA AFTER 1                           
027400     .                                                                    
027500                                                                          
027600 S04-SKRIV-LIST-SF SECTION.                                               
027700                                                                          
027800     WRITE LISTPOST5       FROM UT-AREA-SF                                
027900     .                                                                    
028000     EJECT                                                                
028100                                                                          
028200 AA-WRITE-HEADER SECTION.                                                 
028300                                                                          
028400     WRITE LISTPOST5           FROM UT-HEADER-SF                          
028500     .                                                                    
028600     EJECT                                                                
