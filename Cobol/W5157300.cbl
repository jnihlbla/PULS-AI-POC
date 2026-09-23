000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5157300.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   AUG 2017.                                                
000500*    REMARKS.                                                             
000600*                SKAPAR HÄNDELSELISTOR TILL ON-DEMAND                     
000700*                                                                         
000800*        INDATA  W51575                                                   
000900*                                                                         
001000*                                                                         
001100     EJECT                                                                
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SELECT W51575              ASSIGN TO UT-S-W51573D1.                  
001900     SELECT LISTA1              ASSIGN TO UT-S-W51573D2.                  
002000     SELECT LISTA2              ASSIGN TO UT-S-W51573D3.                  
002010     SELECT LISTA3              ASSIGN TO UT-S-W51573D4.                  
002020     SELECT LISTA4              ASSIGN TO UT-S-W51573D5.                  
002030     SELECT LISTA5              ASSIGN TO UT-S-W51573D6.                  
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W51575                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900*01  -COPY W51573     -L.                                                 
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
003910                                                                          
003920 FD  LISTA3                                                               
003930     RECORDING V                                                          
003940     BLOCK CONTAINS 0.                                                    
003950 01  LISTPOST3                    PIC X(165).                             
003960                                                                          
003970 FD  LISTA4                                                               
003980     RECORDING V                                                          
003990     BLOCK CONTAINS 0.                                                    
003991 01  LISTPOST4                    PIC X(165).                             
003992                                                                          
003993 FD  LISTA5                                                               
003994     RECORDING F                                                          
003995     BLOCK CONTAINS 0.                                                    
003996 01  LISTPOST5                    PIC X(229).                             
004000     EJECT                                                                
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM               PIC X(8)        VALUE 'W5157300'.                
004500 77  JA                  PIC X           VALUE 'J'.                       
004600 77  NEJ                 PIC X           VALUE 'N'.                       
004700 77  IN-EOF              PIC X           VALUE 'N'.                       
004710 77  W51573D3-WRITE-HDR-SW       PIC X       VALUE 'N'.                   
004720     88  WRITED3-YES                         VALUE 'J'.                   
004730 77  W51573D4-WRITE-HDR-SW       PIC X       VALUE 'N'.                   
004740     88  WRITED4-YES                         VALUE 'J'.                   
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
006100*    03  -COPY W51573  -PRE IN-.                                          
006200     EJECT                                                                
006300                                                                          
006400 01  TITEL1.                                                              
006500     03  FILLER              PIC X(29)   VALUE                            
006600         ' W51573-001   VCIN           '.                                 
006700     03  FILLER              PIC X(22)   VALUE                            
006800         'TRANSACTION LIST DATE:'.                                        
006900     03  W-DATUM-T1          PIC X(8).                                    
007000     03  FILLER              PIC X(7)   VALUE                             
007100         ' TIME: '.                                                       
007200     03  W-TID-T1            PIC X(4).                                    
007300 01  TITEL2.                                                              
007400     03  FILLER              PIC X(29)   VALUE                            
007500         ' W51573-002   VCIN           '.                                 
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
014401     03  RAD-IDKST              PIC X(10).                                
014500     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014600     03  RAD-IDANALYS           PIC X(12).                                
014700     03  RAD-SUBEL              PIC Z(8)9.99.                             
014800     03  RAD-TECKEN             PIC X.                                    
014900     03  FILLER                 PIC X(3)  VALUE SPACE.                    
015000     03  RAD-IDPRCTR            PIC X(10).                                
015100     EJECT                                                                
015200                                                                          
015201 01  UT-HEADER1.                                                          
015202     03  UT-H-IDVERGL1       PIC X(15)    VALUE ' INVOICE CREDIT'.        
015203     03  FILLER              PIC X(01)    VALUE ';'.                      
015204     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
015205     03  FILLER              PIC X(01)    VALUE ';'.                      
015206     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
015207     03  FILLER              PIC X(01)    VALUE ';'.                      
015208     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
015209     03  FILLER              PIC X(01)    VALUE ';'.                      
015210     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
015211     03  FILLER              PIC X(01)    VALUE ';'.                      
015212     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
015213     03  FILLER              PIC X(01)    VALUE ';'.                      
015214     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
015215     03  FILLER              PIC X(01)    VALUE ';'.                      
015216     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
015217     03  FILLER              PIC X(01)    VALUE ';'.                      
015218     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
015219     03  FILLER              PIC X(01)    VALUE ';'.                      
015220     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
015221     03  FILLER              PIC X(01)    VALUE ';'.                      
015222     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
015223     03  FILLER              PIC X(01)    VALUE ';'.                      
015224     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
015225     03  FILLER              PIC X(01)    VALUE ';'.                      
015226     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
015227     03  FILLER              PIC X(01)    VALUE ';'.                      
015228     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
015229     03  FILLER              PIC X(01)    VALUE ';'.                      
015230     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
015231     03  FILLER              PIC X(01)    VALUE ';'.                      
015232     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
015233     03  FILLER              PIC X(01)    VALUE ';'.                      
015234     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
015235     03  FILLER              PIC X(01)    VALUE ';'.                      
015236     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
015237     03  FILLER              PIC X(01)    VALUE ';'.                      
015238                                                                          
015239 01  UT-HEADER2.                                                          
015240     03  UT-H-IDVERGL1       PIC X(11)    VALUE ' REF NUMBER'.            
015241     03  FILLER              PIC X(01)    VALUE ';'.                      
015242     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
015243     03  FILLER              PIC X(01)    VALUE ';'.                      
015244     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
015245     03  FILLER              PIC X(01)    VALUE ';'.                      
015246     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
015247     03  FILLER              PIC X(01)    VALUE ';'.                      
015248     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
015249     03  FILLER              PIC X(01)    VALUE ';'.                      
015250     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
015251     03  FILLER              PIC X(01)    VALUE ';'.                      
015252     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
015253     03  FILLER              PIC X(01)    VALUE ';'.                      
015254     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
015255     03  FILLER              PIC X(01)    VALUE ';'.                      
015256     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
015257     03  FILLER              PIC X(01)    VALUE ';'.                      
015258     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
015259     03  FILLER              PIC X(01)    VALUE ';'.                      
015260     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
015261     03  FILLER              PIC X(01)    VALUE ';'.                      
015262     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
015263     03  FILLER              PIC X(01)    VALUE ';'.                      
015264     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
015265     03  FILLER              PIC X(01)    VALUE ';'.                      
015266     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
015267     03  FILLER              PIC X(01)    VALUE ';'.                      
015268     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
015269     03  FILLER              PIC X(01)    VALUE ';'.                      
015270     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
015271     03  FILLER              PIC X(01)    VALUE ';'.                      
015272     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
015273     03  FILLER              PIC X(01)    VALUE ';'.                      
015274     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
015275     03  FILLER              PIC X(01)    VALUE ';'.                      
015276                                                                          
015277 01  UT-HEADER-SF.                                                        
015278     03  UT-H-IDVERGL-SF     PIC X(15)    VALUE                           
015279                                          'DOCUMENT NUMBER'.              
015280     03  FILLER              PIC X(01)    VALUE ';'.                      
015281     03  UT-H-DATUM-T1-SF    PIC X(09)    VALUE 'LIST DATE'.              
015282     03  FILLER              PIC X(01)    VALUE ';'.                      
015283     03  UT-H-DAVERDAT-SF    PIC X(13)    VALUE 'DOCUMENT DATE'.          
015284     03  FILLER              PIC X(01)    VALUE ';'.                      
015285     03  UT-H-KDEKHHT-SF     PIC X(10)    VALUE 'MAIN EVENT'.             
015286     03  FILLER              PIC X(01)    VALUE ';'.                      
015287     03  UT-H-KDEKSHT-SF     PIC X(09)    VALUE 'SUB EVENT'.              
015288     03  FILLER              PIC X(01)    VALUE ';'.                      
015289     03  UT-H-KDEKNIVA-SF    PIC X(11)    VALUE 'EVENT LEVEL'.            
015290     03  FILLER              PIC X(01)    VALUE ';'.                      
015291     03  UT-H-KDDOKTYP-SF    PIC X(13)    VALUE 'DOCUMENT TYPE'.          
015292     03  FILLER              PIC X(01)    VALUE ';'.                      
015293     03  UT-H-IDDC-SF        PIC X(02)    VALUE 'DC'.                     
015294     03  FILLER              PIC X(01)    VALUE ';'.                      
015295     03  UT-H-KVANTAL-SF     PIC X(08)    VALUE 'QUANTITY'.               
015296     03  FILLER              PIC X(01)    VALUE ';'.                      
015297     03  UT-H-IDARTNR-SF     PIC X(07)    VALUE 'PART NO'.                
015298     03  FILLER              PIC X(01)    VALUE ';'.                      
015299     03  UT-H-KDPRODSL-SF    PIC X(13)    VALUE 'PRODUCT GROUP'.          
015300     03  FILLER              PIC X(01)    VALUE ';'.                      
015301     03  UT-H-IDKONTO-SF     PIC X(07)    VALUE 'ACCOUNT'.                
015302     03  FILLER              PIC X(01)    VALUE ';'.                      
015303     03  UT-H-KDPOST-SF      PIC X(11)    VALUE 'POSTING KEY'.            
015304     03  FILLER              PIC X(01)    VALUE ';'.                      
015305     03  UT-H-IDKST-SF       PIC X(10)    VALUE 'COSTCENTER'.             
015306     03  FILLER              PIC X(01)    VALUE ';'.                      
015307     03  UT-H-IDTECKEN-SF    PIC X(12)    VALUE 'ORDER NUMBER'.           
015308     03  FILLER              PIC X(01)    VALUE ';'.                      
015309     03  UT-H-SUBEL-SF       PIC X(06)    VALUE 'AMOUNT'.                 
015310     03  FILLER              PIC X(01)    VALUE ';'.                      
015311     03  UT-H-IDPRCTR-SF     PIC X(13)    VALUE 'PROFIT CENTER'.          
015312     03  FILLER              PIC X(01)    VALUE ';'.                      
015313     03  UT-H-PRARTSTD-SF    PIC X(05)    VALUE 'PRICE'.                  
015314     03  FILLER              PIC X(01)    VALUE ';'.                      
015316     03  UT-H-KDTRADP-SF     PIC X(15)    VALUE 'TRADING PARTNER'.        
015317     03  FILLER              PIC X(01)    VALUE ';'.                      
015318     03  UT-H-FLLSBOK-SF     PIC X(21)    VALUE                           
015319                                          'FLAG FOR STOCK UPDATE'.        
015320                                                                          
015321 01  UT-AREA.                                                             
015322     03  FILLER                 PIC X(02)    VALUE SPACE.                 
015323     03  UT-IDVERGL             PIC X(10)    VALUE SPACE.                 
015324     03  FILLER                 PIC X(01)    VALUE ';'.                   
015325     03  UT-DATUM-T1             PIC X(8)     VALUE SPACE.                
015326     03  FILLER                 PIC X(01)    VALUE ';'.                   
015327     03  UT-DAVERDAT            PIC X(10)    VALUE SPACE.                 
015328     03  FILLER                 PIC X(01)    VALUE ';'.                   
015329     03  UT-HAENDELSE           PIC X(07).                                
015330     03  FILLER REDEFINES UT-HAENDELSE.                                   
015331         05  UT-KDEKHHT         PIC X(03).                                
015332         05  UT-STRECK          PIC X(01).                                
015333         05  UT-KDEKSHT         PIC X(03).                                
015334     03  FILLER                 PIC X(01).                                
015335     03  FILLER                 PIC X(01)    VALUE ';'.                   
015336     03  UT-KDEKNIVA            PIC X(04)    VALUE SPACE.                 
015337     03  FILLER                 PIC X(01)    VALUE ';'.                   
015338     03  UT-KDDOKTYP            PIC X(02)    VALUE SPACE.                 
015339     03  FILLER                 PIC X(01)    VALUE ';'.                   
015340     03  UT-IDDC                PIC X(02)    VALUE SPACE.                 
015341     03  FILLER                 PIC X(01)    VALUE ';'.                   
015342     03  UT-KVANTAL             PIC X(07)    VALUE SPACE.                 
015343     03  FILLER REDEFINES  UT-KVANTAL.                                    
015344         05  UT-KVANTAL-1       PIC X(6).                                 
015345         05  UT-KVANTAL-2       PIC X(1).                                 
015346     03  FILLER                 PIC X(01)    VALUE ';'.                   
015347     03  UT-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                 
015348     03  FILLER                 PIC X(01)    VALUE ';'.                   
015349     03  UT-IDARTNR             PIC Z(8)9    VALUE ZERO.                  
015350     03  FILLER                 PIC X(01)    VALUE ';'.                   
015351     03  UT-KDPRODSL            PIC Z(3)     VALUE ZERO.                  
015352     03  FILLER                 PIC X(01)    VALUE ';'.                   
015353     03  UT-IDKONTO             PIC 9(10)    VALUE ZERO.                  
015354     03  FILLER                 PIC X(01)    VALUE ';'.                   
015355     03  UT-KDPOST              PIC X(02)    VALUE SPACE.                 
015356     03  FILLER                 PIC X(01)    VALUE ';'.                   
015357     03  UT-IDKST               PIC X(10)    VALUE SPACE.                 
015358     03  FILLER                 PIC X(01)    VALUE ';'.                   
015359     03  UT-IDANALYS            PIC X(12)    VALUE SPACE.                 
015360     03  FILLER                 PIC X(01)    VALUE ';'.                   
015361     03  UT-SUBEL               PIC Z(8)9.99 VALUE ZERO.                  
015362     03  FILLER                 PIC X(01)    VALUE ';'.                   
015363     03  UT-TECKEN              PIC X(01)    VALUE SPACE.                 
015364     03  FILLER                 PIC X(01)    VALUE ';'.                   
015365     03  UT-IDPRCTR             PIC X(10)    VALUE SPACE.                 
015366     03  FILLER                 PIC X(01)    VALUE ';'.                   
015367                                                                          
015368 01  UT-AREA-SF.                                                          
015369     03  UT-IDVERGL-SF          PIC X(10)    VALUE SPACE.                 
015370     03  FILLER                 PIC X(01)    VALUE ';'.                   
015371     03  UT-DATUM-T1-SF         PIC X(8)     VALUE SPACE.                 
015372     03  FILLER                 PIC X(01)    VALUE ';'.                   
015373     03  UT-DAVERDAT-SF         PIC X(10)    VALUE SPACE.                 
015374     03  FILLER                 PIC X(01)    VALUE ';'.                   
015375     03  UT-KDEKHHT-SF          PIC X(03)    VALUE SPACE.                 
015376     03  FILLER                 PIC X(01)    VALUE ';'.                   
015377     03  UT-KDEKSHT-SF          PIC X(03)    VALUE SPACE.                 
015378     03  FILLER                 PIC X(01)    VALUE ';'.                   
015379     03  UT-KDEKNIVA-SF         PIC X(04)    VALUE SPACE.                 
015380     03  FILLER                 PIC X(01)    VALUE ';'.                   
015381     03  UT-KDDOKTYP-SF         PIC X(02)    VALUE SPACE.                 
015382     03  FILLER                 PIC X(01)    VALUE ';'.                   
015383     03  UT-IDDC-SF             PIC X(02)    VALUE SPACE.                 
015384     03  FILLER                 PIC X(01)    VALUE ';'.                   
015385     03  UT-KVANTAL-SF          PIC -(7)9    VALUE SPACE.                 
015386     03  FILLER                 PIC X(01)    VALUE ';'.                   
015387     03  UT-IDARTNR-SF          PIC Z(8)9    VALUE ZERO.                  
015388     03  FILLER                 PIC X(01)    VALUE ';'.                   
015389     03  UT-KDPRODSL-SF         PIC Z(2)9    VALUE ZERO.                  
015390     03  FILLER                 PIC X(01)    VALUE ';'.                   
015391     03  UT-IDKONTO-SF          PIC Z(9)9    VALUE ZERO.                  
015392     03  FILLER                 PIC X(01)    VALUE ';'.                   
015393     03  UT-KDPOST-SF           PIC X(02)    VALUE SPACE.                 
015394     03  FILLER                 PIC X(01)    VALUE ';'.                   
015395     03  UT-IDKST-SF            PIC X(10)    VALUE SPACE.                 
015396     03  FILLER                 PIC X(01)    VALUE ';'.                   
015397     03  UT-IDANALYS-SF         PIC X(12)    VALUE SPACE.                 
015398     03  FILLER                 PIC X(01)    VALUE ';'.                   
015399     03  UT-SUBEL-SF            PIC -(9)9.99 VALUE ZERO.                  
015400     03  FILLER                 PIC X(01)    VALUE ';'.                   
015401     03  UT-IDPRCTR-SF          PIC X(10)    VALUE SPACE.                 
015402     03  FILLER                 PIC X(01)    VALUE ';'.                   
015403     03  UT-PRARTSTD-SF         PIC -(7)9.99 VALUE ZERO.                  
015404     03  FILLER                 PIC X(01)    VALUE ';'.                   
015405     03  UT-KDTRADP-SF          PIC X(04)    VALUE SPACE.                 
015406     03  FILLER                 PIC X(01)    VALUE ';'.                   
015407     03  UT-FLLSBOK-SF          PIC X(01)    VALUE SPACE.                 
015408     EJECT                                                                
015409                                                                          
015410 PROCEDURE DIVISION.                                                      
015420     PERFORM A-INITIERA                                                   
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
016900     OPEN INPUT  W51575                                                   
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
017610     PERFORM AA-WRITE-HEADER                                              
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
018810        ELSE                                                              
018820           MOVE SPACE TO RAD-KVANTAL-TKN                                  
018900        END-IF                                                            
018910        MOVE IN-KVANTAL     TO UT-KVANTAL                                 
018911                               UT-KVANTAL-SF                              
018920        INSPECT UT-KVANTAL-1 REPLACING LEADING ZERO BY SPACE              
018930        IF IN-KVANTAL < 0                                                 
018940           MOVE '-'   TO UT-KVANTAL-TKN                                   
018941        ELSE                                                              
018942           MOVE SPACE TO UT-KVANTAL-TKN                                   
018950        END-IF                                                            
019000     END-IF                                                               
019100     MOVE IN-KDEKHHT        TO RAD-KDEKHHT                                
019200     MOVE '-'               TO RAD-STRECK                                 
019300     MOVE IN-KDEKSHT        TO RAD-KDEKSHT                                
019400     MOVE IN-KDEKNIVA       TO RAD-KDEKNIVA                               
019500     MOVE IN-KDDOKTYP       TO RAD-KDDOKTYP                               
019600     MOVE IN-IDDC           TO RAD-IDDC                                   
019700     MOVE IN-IDARTNR        TO RAD-IDARTNR                                
019800     MOVE IN-KDPRODSL       TO RAD-KDPRODSL                               
019900     MOVE IN-IDKONTO        TO RAD-IDKONTO                                
020000     INSPECT RAD-IDKONTO REPLACING LEADING ZERO BY SPACE                  
020100     MOVE IN-IDKST          TO RAD-IDKST                                  
020300     MOVE IN-IDANALYS       TO RAD-IDANALYS                               
020400     MOVE IN-SUBEL          TO RAD-SUBEL                                  
020500     MOVE IN-IDTECKEN       TO RAD-TECKEN                                 
020600     MOVE IN-IDPRCTR        TO RAD-IDPRCTR                                
020700     MOVE IN-KDPOST         TO RAD-KDPOST                                 
020800****FOR REPORT                                                            
020801     MOVE IN-IDVERGL        TO UT-IDVERGL                                 
020802                               UT-IDVERGL-SF                              
020803     MOVE W-DATUM-T1        TO UT-DATUM-T1                                
020804                               UT-DATUM-T1-SF                             
020805     MOVE IN-DAVERDAT       TO UT-DAVERDAT                                
020806                               UT-DAVERDAT-SF                             
020807     MOVE IN-KDEKHHT        TO UT-KDEKHHT                                 
020808                               UT-KDEKHHT-SF                              
020809     MOVE '-'               TO UT-STRECK                                  
020810     MOVE IN-KDEKSHT        TO UT-KDEKSHT                                 
020811                               UT-KDEKSHT-SF                              
020812     MOVE IN-KDEKNIVA       TO UT-KDEKNIVA                                
020813                               UT-KDEKNIVA-SF                             
020814     MOVE IN-KDDOKTYP       TO UT-KDDOKTYP                                
020815                               UT-KDDOKTYP-SF                             
020816     MOVE IN-IDDC           TO UT-IDDC                                    
020817                               UT-IDDC-SF                                 
020818     MOVE IN-IDARTNR        TO UT-IDARTNR                                 
020819                               UT-IDARTNR-SF                              
020820     MOVE IN-KDPRODSL       TO UT-KDPRODSL                                
020821                               UT-KDPRODSL-SF                             
020822     MOVE IN-IDKONTO        TO UT-IDKONTO                                 
020823                               UT-IDKONTO-SF                              
020824     INSPECT UT-IDKONTO REPLACING LEADING ZERO BY SPACE                   
020825     INSPECT UT-IDKONTO-SF REPLACING LEADING ZERO BY SPACE                
020826     MOVE IN-IDKST          TO UT-IDKST                                   
020827                               UT-IDKST-SF                                
020828     MOVE IN-IDANALYS       TO UT-IDANALYS                                
020829                               UT-IDANALYS-SF                             
020830     MOVE IN-SUBEL          TO UT-SUBEL                                   
020831                               WS-SUBEL                                   
020832     MOVE IN-IDTECKEN       TO UT-TECKEN                                  
020833     IF IN-IDTECKEN = '-'                                                 
020834        COMPUTE WS-SUBEL = 0 - WS-SUBEL                                   
020835        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
020836     ELSE                                                                 
020837        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
020838     END-IF                                                               
020839     MOVE IN-IDPRCTR        TO UT-IDPRCTR                                 
020840                               UT-IDPRCTR-SF                              
020841     MOVE IN-KDPOST         TO UT-KDPOST                                  
020850                               UT-KDPOST-SF                               
020860     MOVE IN-PRARTSTD       TO UT-PRARTSTD-SF                             
020870     MOVE IN-FLLSBOK        TO UT-FLLSBOK-SF                              
020880     MOVE 'IN07'            TO UT-KDTRADP-SF                              
020900     IF IN-KDEKHHT = '303'                                                
021100                  OR '201'                                                
021300                  OR '203'                                                
021400                  OR '204'                                                
021500        IF W51573D3-WRITE-HDR-SW = 'N'                                    
021600          WRITE LISTPOST3           FROM UT-HEADER1                       
021610        END-IF                                                            
021710        PERFORM S02-SKRIV-LISTA1                                          
021720        SET WRITED3-YES     TO TRUE                                       
021800     ELSE                                                                 
021810        IF W51573D4-WRITE-HDR-SW = 'N'                                    
021820          WRITE LISTPOST4           FROM UT-HEADER2                       
021830        END-IF                                                            
021900        PERFORM S03-SKRIV-LISTA2                                          
021910        SET WRITED4-YES     TO TRUE                                       
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
023000     CLOSE LISTA1 W51575 LISTA2 LISTA3 LISTA4 LISTA5                      
023100     .                                                                    
023200     EJECT                                                                
023300                                                                          
023400 S01-LAS-INFIL SECTION.                                                   
023500     READ W51575 INTO INAREA                                              
023600        AT END MOVE JA TO IN-EOF                                          
023700     END-READ                                                             
023800     .                                                                    
023900                                                                          
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
028000                                                                          
028100 AA-WRITE-HEADER SECTION.                                                 
028200                                                                          
028300     WRITE LISTPOST5           FROM UT-HEADER-SF                          
028400     .                                                                    
028500     EJECT                                                                
