000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5707300.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   SEPT 98.                                                 
000500*    REMARKS.                                                             
000600*                SKAPAR HÄNDELSELISTOR TILL ON-DEMAND                     
000700*                                                                         
000800*        INDATA  W57075                                                   
000900*                                                                         
001000*                                                                         
001100     EJECT                                                                
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SELECT W57075              ASSIGN TO UT-S-W57073D1.                  
001900     SELECT LISTA1              ASSIGN TO UT-S-W57073D2.                  
002000     SELECT LISTA2              ASSIGN TO UT-S-W57073D3.                  
002010     SELECT LISTA3              ASSIGN TO UT-S-W57073D4.                  
002020     SELECT LISTA4              ASSIGN TO UT-S-W57073D5.                  
002030     SELECT LISTA5              ASSIGN TO UT-S-W57073D6.                  
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W57075                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900*01  -COPY W57073     -L.                                                 
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
003941                                                                          
003950 FD  LISTA4                                                               
003960     RECORDING V                                                          
003970     BLOCK CONTAINS 0.                                                    
003980 01  LISTPOST4                    PIC X(165).                             
003990                                                                          
003991 FD  LISTA5                                                               
003992     RECORDING F                                                          
003993     BLOCK CONTAINS 0.                                                    
003994 01  LISTPOST5                    PIC X(229).                             
004000     EJECT                                                                
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM               PIC X(8)        VALUE 'W5707300'.                
004500 77  JA                  PIC X           VALUE 'J'.                       
004600 77  NEJ                 PIC X           VALUE 'N'.                       
004700 77  IN-EOF              PIC X           VALUE 'N'.                       
004800                                                                          
004900 01  W-IDARTNR           PIC 9(9)        VALUE ZERO.                      
004910 01  WS-IDARTNR          PIC 9(9)        VALUE ZERO.                      
005000 01  W-ANTAL             PIC S9(7)       VALUE ZERO.                      
005100 01  W-IDVERGL           PIC X(10)       VALUE SPACE.                     
005110 01  WS-IDVERGL          PIC X(10)       VALUE SPACE.                     
005120 01  WS-SUBEL            PIC S9(9)V99    VALUE +0    COMP-3.              
005200 01  W-IDVERGL-1         PIC X(10)       VALUE SPACE.                     
005300 01  W-IDVERGL-2         PIC X(10)       VALUE SPACE.                     
005400                                                                          
005500 01  RAD-ANT             PIC S9          VALUE +0    COMP-3.              
005600 01  SIDNR               PIC S9(5)       VALUE +0    COMP-3.              
005700 01  RADNR               PIC S9(3)       VALUE +99   COMP-3.              
005800     EJECT                                                                
005900                                                                          
006000 01  INAREA.                                                              
006100*    03  -COPY W57073  -PRE IN-.                                          
006200     EJECT                                                                
006300                                                                          
006400 01  TITEL1.                                                              
006500     03  FILLER              PIC X(29)   VALUE                            
006600         ' W57073-001   VCCN           '.                                 
006700     03  FILLER              PIC X(22)   VALUE                            
006800         'TRANSACTION LIST DATE:'.                                        
006900     03  W-DATUM-T1          PIC X(8).                                    
007000     03  FILLER              PIC X(7)   VALUE                             
007100         ' TIME: '.                                                       
007200     03  W-TID-T1            PIC X(4).                                    
007300 01  TITEL2.                                                              
007400     03  FILLER              PIC X(29)   VALUE                            
007500         ' W57073-002   VCCN           '.                                 
007600     03  FILLER              PIC X(22)   VALUE                            
007700         'TRANSACTION LIST DATE:'.                                        
007800     03  W-DATUM-T2          PIC X(8).                                    
007900     03  FILLER              PIC X(7)   VALUE                             
008000         ' TIME: '.                                                       
008100     03  W-TID-T2            PIC X(4).                                    
008200                                                                          
008380 01  RUBRIK1A.                                                            
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
011301 01  BLANK-RAD              PIC X    VALUE SPACE.                         
011302 01  DETALJRAD-1.                                                         
011303     03  FILLER                 PIC X.                                    
011304     03  RAD-HAENDELSE          PIC X(7).                                 
011305     03  FILLER REDEFINES RAD-HAENDELSE.                                  
011306         05  RAD-KDEKHHT        PIC X(3).                                 
011307         05  RAD-STRECK         PIC X.                                    
011308         05  RAD-KDEKSHT        PIC X(3).                                 
011309     03  FILLER                 PIC X(2)  VALUE SPACE.                    
011310     03  RAD-KDEKNIVA           PIC X(4).                                 
011311     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011312     03  RAD-KDDOKTYP           PIC X(2).                                 
011313     03  FILLER                 PIC X(2)  VALUE SPACE.                    
011314     03  RAD-IDDC               PIC X(2).                                 
011315     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011316     03  RAD-KVANTAL            PIC X(7).                                 
011317     03  FILLER REDEFINES RAD-KVANTAL.                                    
011318         05  RAD-KVANTAL-1      PIC X(6).                                 
011319         05  RAD-KVANTAL-2      PIC X(1).                                 
011320     03  RAD-KVANTAL-TKN        PIC X(1).                                 
011321     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011322     03  RAD-IDARTNR            PIC Z(8)9.                                
011323     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011324     03  RAD-KDPRODSL           PIC Z(3).                                 
011325     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011326     03  RAD-IDKONTO            PIC 9(10).                                
011327     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011328     03  RAD-KDPOST             PIC X(2).                                 
011329     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011330     03  RAD-IDKST              PIC X(10).                                
011331     03  FILLER                 PIC X(1)  VALUE SPACE.                    
011332     03  RAD-IDANALYS           PIC X(12).                                
011333     03  RAD-SUBEL              PIC Z(8)9.99.                             
011334     03  RAD-TECKEN             PIC X.                                    
011335     03  FILLER                 PIC X(3)  VALUE SPACE.                    
011336     03  RAD-IDPRCTR            PIC X(10).                                
011337     EJECT                                                                
011338 01  UT-HEADER1.                                                          
011339     03  UT-H-IDVERGL1       PIC X(15)    VALUE ' INVOICE CREDIT'.        
011340     03  FILLER              PIC X(01)    VALUE ';'.                      
011341     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
011342     03  FILLER              PIC X(01)    VALUE ';'.                      
011343     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
011350     03  FILLER              PIC X(01)    VALUE ';'.                      
011380     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
011390     03  FILLER              PIC X(01)    VALUE ';'.                      
011391     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
011392     03  FILLER              PIC X(01)    VALUE ';'.                      
011393     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
011394     03  FILLER              PIC X(01)    VALUE ';'.                      
011395     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
011396     03  FILLER              PIC X(01)    VALUE ';'.                      
011397     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
011398     03  FILLER              PIC X(01)    VALUE ';'.                      
011399     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
011400     03  FILLER              PIC X(01)    VALUE ';'.                      
011401     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
011402     03  FILLER              PIC X(01)    VALUE ';'.                      
011403     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
011404     03  FILLER              PIC X(01)    VALUE ';'.                      
011405     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
011406     03  FILLER              PIC X(01)    VALUE ';'.                      
011407     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
011408     03  FILLER              PIC X(01)    VALUE ';'.                      
011409     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
011410     03  FILLER              PIC X(01)    VALUE ';'.                      
011411     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
011412     03  FILLER              PIC X(01)    VALUE ';'.                      
011413     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
011414     03  FILLER              PIC X(01)    VALUE ';'.                      
011415     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
011416     03  FILLER              PIC X(01)    VALUE ';'.                      
011417     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
011418     03  FILLER              PIC X(01)    VALUE ';'.                      
011419                                                                          
011420 01  UT-HEADER2.                                                          
011430     03  UT-H-IDVERGL1       PIC X(11)    VALUE ' REF NUMBER'.            
011440     03  FILLER              PIC X(01)    VALUE ';'.                      
011450     03  UT-H-DATUM-T1       PIC X(09)    VALUE 'LIST DATE'.              
011451     03  FILLER              PIC X(01)    VALUE ';'.                      
011452     03  UT-H-DAVERDAT       PIC X(09)    VALUE 'VER.DATE:'.              
011453     03  FILLER              PIC X(01)    VALUE ';'.                      
011454     03  UT-H-KDEKNIVA       PIC X(05)    VALUE 'EVENT'.                  
011455     03  FILLER              PIC X(01)    VALUE ';'.                      
011456     03  UT-H-KDDOKTYP       PIC X(08)    VALUE 'LEVEL'.                  
011457     03  FILLER              PIC X(01)    VALUE ';'.                      
011458     03  UT-H-DOC            PIC X(03)    VALUE 'DOC'.                    
011459     03  FILLER              PIC X(01)    VALUE ';'.                      
011460     03  UT-H-IDDC           PIC X(02)    VALUE 'DC'.                     
011461     03  FILLER              PIC X(01)    VALUE ';'.                      
011462     03  UT-H-KVANTAL        PIC X(03)    VALUE 'QTY'.                    
011463     03  FILLER              PIC X(01)    VALUE ';'.                      
011464     03  UT-H-SIGN-QTY       PIC X(08)    VALUE 'SIGN-QTY'.               
011465     03  FILLER              PIC X(01)    VALUE ';'.                      
011466     03  UT-H-IDARTNR        PIC X(07)    VALUE 'PART NO'.                
011467     03  FILLER              PIC X(01)    VALUE ';'.                      
011468     03  UT-H-KDPRODSL       PIC X(13)    VALUE 'PRODUCT GROUP'.          
011469     03  FILLER              PIC X(01)    VALUE ';'.                      
011470     03  UT-H-IDKONTO        PIC X(10)    VALUE 'ACCOUNT'.                
011471     03  FILLER              PIC X(01)    VALUE ';'.                      
011472     03  UT-H-KDPOST         PIC X(03)    VALUE 'PK'.                     
011473     03  FILLER              PIC X(01)    VALUE ';'.                      
011474     03  UT-H-IDKST          PIC X(10)    VALUE 'COSTCENTER'.             
011475     03  FILLER              PIC X(01)    VALUE ';'.                      
011476     03  UT-H-IDTECKEN       PIC X(12)    VALUE 'INT ORDER NO'.           
011477     03  FILLER              PIC X(01)    VALUE ';'.                      
011478     03  UT-H-SUBEL          PIC X(06)    VALUE 'AMOUNT'.                 
011479     03  FILLER              PIC X(01)    VALUE ';'.                      
011480     03  UT-H-UT-TECKEN      PIC X(11)    VALUE 'SIGN-AMOUNT'.            
011481     03  FILLER              PIC X(01)    VALUE ';'.                      
011482     03  UT-H-IDPRCTR        PIC X(13)    VALUE 'PROFIT CENTER'.          
011483     03  FILLER              PIC X(01)    VALUE ';'.                      
011484                                                                          
011485 01  UT-HEADER-SF.                                                        
011486     03  UT-H-IDVERGL-SF     PIC X(15)    VALUE                           
011487                                          'DOCUMENT NUMBER'.              
011488     03  FILLER              PIC X(01)    VALUE ';'.                      
011489     03  UT-H-DATUM-T1-SF    PIC X(09)    VALUE 'LIST DATE'.              
011490     03  FILLER              PIC X(01)    VALUE ';'.                      
011491     03  UT-H-DAVERDAT-SF    PIC X(13)    VALUE 'DOCUMENT DATE'.          
011492     03  FILLER              PIC X(01)    VALUE ';'.                      
011493     03  UT-H-KDEKHHT-SF     PIC X(10)    VALUE 'MAIN EVENT'.             
011494     03  FILLER              PIC X(01)    VALUE ';'.                      
011495     03  UT-H-KDEKSHT-SF     PIC X(09)    VALUE 'SUB EVENT'.              
011496     03  FILLER              PIC X(01)    VALUE ';'.                      
011497     03  UT-H-KDEKNIVA-SF    PIC X(11)    VALUE 'EVENT LEVEL'.            
011498     03  FILLER              PIC X(01)    VALUE ';'.                      
011499     03  UT-H-KDDOKTYP-SF    PIC X(13)    VALUE 'DOCUMENT TYPE'.          
011500     03  FILLER              PIC X(01)    VALUE ';'.                      
011501     03  UT-H-IDDC-SF        PIC X(02)    VALUE 'DC'.                     
011502     03  FILLER              PIC X(01)    VALUE ';'.                      
011503     03  UT-H-KVANTAL-SF     PIC X(08)    VALUE 'QUANTITY'.               
011504     03  FILLER              PIC X(01)    VALUE ';'.                      
011505     03  UT-H-IDARTNR-SF     PIC X(07)    VALUE 'PART NO'.                
011506     03  FILLER              PIC X(01)    VALUE ';'.                      
011507     03  UT-H-KDPRODSL-SF    PIC X(13)    VALUE 'PRODUCT GROUP'.          
011508     03  FILLER              PIC X(01)    VALUE ';'.                      
011509     03  UT-H-IDKONTO-SF     PIC X(07)    VALUE 'ACCOUNT'.                
011510     03  FILLER              PIC X(01)    VALUE ';'.                      
011511     03  UT-H-KDPOST-SF      PIC X(11)    VALUE 'POSTING KEY'.            
011512     03  FILLER              PIC X(01)    VALUE ';'.                      
011513     03  UT-H-IDKST-SF       PIC X(10)    VALUE 'COSTCENTER'.             
011514     03  FILLER              PIC X(01)    VALUE ';'.                      
011515     03  UT-H-IDTECKEN-SF    PIC X(12)    VALUE 'ORDER NUMBER'.           
011516     03  FILLER              PIC X(01)    VALUE ';'.                      
011517     03  UT-H-SUBEL-SF       PIC X(06)    VALUE 'AMOUNT'.                 
011518     03  FILLER              PIC X(01)    VALUE ';'.                      
011519     03  UT-H-IDPRCTR-SF     PIC X(13)    VALUE 'PROFIT CENTER'.          
011520     03  FILLER              PIC X(01)    VALUE ';'.                      
011521     03  UT-H-PRARTSTD-SF    PIC X(05)    VALUE 'PRICE'.                  
011522     03  FILLER              PIC X(01)    VALUE ';'.                      
011523     03  UT-H-KDTRADP-SF     PIC X(15)    VALUE 'TRADING PARTNER'.        
011524     03  FILLER              PIC X(01)    VALUE ';'.                      
011525     03  UT-H-FLLSBOK-SF     PIC X(21)    VALUE                           
011526                                          'FLAG FOR STOCK UPDATE'.        
011527                                                                          
011528 01  UT-AREA.                                                             
011529     03  FILLER                 PIC X(02)    VALUE SPACE.                 
011530     03  UT-IDVERGL             PIC X(10)    VALUE SPACE.                 
011540     03  FILLER                 PIC X(01)    VALUE ';'.                   
011600     03  UT-DATUM-T1             PIC X(8)     VALUE SPACE.                
015201     03  FILLER                 PIC X(01)    VALUE ';'.                   
015210     03  UT-DAVERDAT            PIC X(10)    VALUE SPACE.                 
015220     03  FILLER                 PIC X(01)    VALUE ';'.                   
015230*    03  UT-KDEKHHT             PIC X(03)    VALUE SPACE.                 
015233*    03  FILLER REDEFINES UT-KDEKHHT.                                     
015234*        05 W-KDEKHHT-DEL3      PIC X(02).                                
015235*        05 W-KDEKHHT-DEL4      PIC X(01).                                
015236     03  UT-HAENDELSE           PIC X(07).                                
015237     03  FILLER REDEFINES UT-HAENDELSE.                                   
015238         05  UT-KDEKHHT         PIC X(03).                                
015239         05  UT-STRECK          PIC X(01).                                
015240         05  UT-KDEKSHT         PIC X(03).                                
015241     03  FILLER                 PIC X(01).                                
015242     03  FILLER                 PIC X(01)    VALUE ';'.                   
015250     03  UT-KDEKNIVA            PIC X(04)    VALUE SPACE.                 
015260     03  FILLER                 PIC X(01)    VALUE ';'.                   
015270     03  UT-KDDOKTYP            PIC X(02)    VALUE SPACE.                 
015280     03  FILLER                 PIC X(01)    VALUE ';'.                   
015290     03  UT-IDDC                PIC X(02)    VALUE SPACE.                 
015291     03  FILLER                 PIC X(01)    VALUE ';'.                   
015292     03  UT-KVANTAL             PIC X(07)    VALUE SPACE.                 
015293     03  FILLER REDEFINES  UT-KVANTAL.                                    
015294         05  UT-KVANTAL-1       PIC X(6).                                 
015295         05  UT-KVANTAL-2       PIC X(1).                                 
015296     03  FILLER                 PIC X(01)    VALUE ';'.                   
015297     03  UT-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                 
015298     03  FILLER                 PIC X(01)    VALUE ';'.                   
015299     03  UT-IDARTNR             PIC Z(8)9    VALUE ZERO.                  
015300     03  FILLER                 PIC X(01)    VALUE ';'.                   
015301     03  UT-KDPRODSL            PIC Z(3)     VALUE ZERO.                  
015302     03  FILLER                 PIC X(01)    VALUE ';'.                   
015303     03  UT-IDKONTO             PIC 9(10)    VALUE ZERO.                  
015304     03  FILLER                 PIC X(01)    VALUE ';'.                   
015305     03  UT-KDPOST              PIC X(02)    VALUE SPACE.                 
015306     03  FILLER                 PIC X(01)    VALUE ';'.                   
015307     03  UT-IDKST               PIC X(10)    VALUE SPACE.                 
015308     03  FILLER                 PIC X(01)    VALUE ';'.                   
015310     03  UT-IDANALYS            PIC X(12)    VALUE SPACE.                 
015311     03  FILLER                 PIC X(01)    VALUE ';'.                   
015312     03  UT-SUBEL               PIC Z(8)9.99 VALUE ZERO.                  
015313     03  FILLER                 PIC X(01)    VALUE ';'.                   
015314     03  UT-TECKEN              PIC X(01)    VALUE SPACE.                 
015315     03  FILLER                 PIC X(01)    VALUE ';'.                   
015316     03  UT-IDPRCTR             PIC X(10)    VALUE SPACE.                 
015317     03  FILLER                 PIC X(01)    VALUE ';'.                   
015318                                                                          
015319 01  UT-AREA-SF.                                                          
015320     03  UT-IDVERGL-SF          PIC X(10)    VALUE SPACE.                 
015321     03  FILLER                 PIC X(01)    VALUE ';'.                   
015322     03  UT-DATUM-T1-SF         PIC X(8)     VALUE SPACE.                 
015323     03  FILLER                 PIC X(01)    VALUE ';'.                   
015324     03  UT-DAVERDAT-SF         PIC X(10)    VALUE SPACE.                 
015325     03  FILLER                 PIC X(01)    VALUE ';'.                   
015326     03  UT-KDEKHHT-SF          PIC X(03)    VALUE SPACE.                 
015327     03  FILLER                 PIC X(01)    VALUE ';'.                   
015328     03  UT-KDEKSHT-SF          PIC X(03)    VALUE SPACE.                 
015329     03  FILLER                 PIC X(01)    VALUE ';'.                   
015330     03  UT-KDEKNIVA-SF         PIC X(04)    VALUE SPACE.                 
015331     03  FILLER                 PIC X(01)    VALUE ';'.                   
015332     03  UT-KDDOKTYP-SF         PIC X(02)    VALUE SPACE.                 
015333     03  FILLER                 PIC X(01)    VALUE ';'.                   
015334     03  UT-IDDC-SF             PIC X(02)    VALUE SPACE.                 
015335     03  FILLER                 PIC X(01)    VALUE ';'.                   
015336     03  UT-KVANTAL-SF          PIC -(7)9    VALUE SPACE.                 
015337     03  FILLER                 PIC X(01)    VALUE ';'.                   
015338     03  UT-IDARTNR-SF          PIC Z(8)9    VALUE ZERO.                  
015339     03  FILLER                 PIC X(01)    VALUE ';'.                   
015340     03  UT-KDPRODSL-SF         PIC Z(2)9    VALUE ZERO.                  
015341     03  FILLER                 PIC X(01)    VALUE ';'.                   
015342     03  UT-IDKONTO-SF          PIC Z(9)9    VALUE ZERO.                  
015343     03  FILLER                 PIC X(01)    VALUE ';'.                   
015344     03  UT-KDPOST-SF           PIC X(02)    VALUE SPACE.                 
015345     03  FILLER                 PIC X(01)    VALUE ';'.                   
015346     03  UT-IDKST-SF            PIC X(10)    VALUE SPACE.                 
015347     03  FILLER                 PIC X(01)    VALUE ';'.                   
015348     03  UT-IDANALYS-SF         PIC X(12)    VALUE SPACE.                 
015349     03  FILLER                 PIC X(01)    VALUE ';'.                   
015350     03  UT-SUBEL-SF            PIC -(9)9.99 VALUE ZERO.                  
015351     03  FILLER                 PIC X(01)    VALUE ';'.                   
015352     03  UT-IDPRCTR-SF          PIC X(10)    VALUE SPACE.                 
015353     03  FILLER                 PIC X(01)    VALUE ';'.                   
015354     03  UT-PRARTSTD-SF         PIC -(7)9.99 VALUE ZERO.                  
015355     03  FILLER                 PIC X(01)    VALUE ';'.                   
015356     03  UT-KDTRADP-SF          PIC X(04)    VALUE SPACE.                 
015357     03  FILLER                 PIC X(01)    VALUE ';'.                   
015358     03  UT-FLLSBOK-SF          PIC X(01)    VALUE SPACE.                 
015359     EJECT                                                                
015360                                                                          
015370 PROCEDURE DIVISION.                                                      
015400     PERFORM A-INITIERA                                                   
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
017000     OPEN INPUT  W57075                                                   
017100          OUTPUT LISTA1                                                   
017200                 LISTA2                                                   
017210                 LISTA3                                                   
017220                 LISTA4                                                   
017230                 LISTA5                                                   
017300                                                                          
017310     PERFORM S11-WRITE-HEADERS                                            
017400     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATUM-T1                        
017500                                        W-DATUM-T2                        
017600     MOVE FUNCTION CURRENT-DATE(9:4) TO W-TID-T1                          
017700                                        W-TID-T2                          
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
019120     IF  IN-IDVERGL = W-IDVERGL                                           
019130     AND IN-KDEKHHT = '102'                                               
019140     AND IN-KDEKSHT = '107'                                               
019150     AND IN-KDEKNIVA = 'HEMT'                                             
019160         MOVE W-IDARTNR     TO RAD-IDARTNR                                
019161         MOVE W-IDARTNR     TO UT-IDARTNR                                 
019162                               UT-IDARTNR-SF                              
019170     ELSE                                                                 
019180         MOVE IN-IDARTNR    TO RAD-IDARTNR                                
019181         MOVE IN-IDARTNR    TO UT-IDARTNR                                 
019182                               UT-IDARTNR-SF                              
019190     END-IF                                                               
019200     MOVE IN-KDEKHHT        TO RAD-KDEKHHT                                
019300     MOVE '-'               TO RAD-STRECK                                 
019400     MOVE IN-KDEKSHT        TO RAD-KDEKSHT                                
019500     MOVE IN-KDEKNIVA       TO RAD-KDEKNIVA                               
019600     MOVE IN-KDDOKTYP       TO RAD-KDDOKTYP                               
019700     MOVE IN-IDDC           TO RAD-IDDC                                   
019800*    MOVE IN-IDARTNR        TO RAD-IDARTNR                                
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
020901     MOVE IN-IDVERGL        TO UT-IDVERGL                                 
020902                               UT-IDVERGL-SF                              
020903     MOVE W-DATUM-T1        TO UT-DATUM-T1                                
020904                               UT-DATUM-T1-SF                             
020905     MOVE IN-DAVERDAT       TO UT-DAVERDAT                                
020906                               UT-DAVERDAT-SF                             
020910     MOVE IN-KDEKHHT        TO UT-KDEKHHT                                 
020911                               UT-KDEKHHT-SF                              
020920     MOVE '-'               TO UT-STRECK                                  
020930     MOVE IN-KDEKSHT        TO UT-KDEKSHT                                 
020931                               UT-KDEKSHT-SF                              
020940     MOVE IN-KDEKNIVA       TO UT-KDEKNIVA                                
020941                               UT-KDEKNIVA-SF                             
020950     MOVE IN-KDDOKTYP       TO UT-KDDOKTYP                                
020951                               UT-KDDOKTYP-SF                             
020960     MOVE IN-IDDC           TO UT-IDDC                                    
020961                               UT-IDDC-SF                                 
020970*    MOVE IN-IDARTNR        TO UT-IDARTNR                                 
020971                                                                          
020980     MOVE IN-KDPRODSL       TO UT-KDPRODSL                                
020981                               UT-KDPRODSL-SF                             
020990     MOVE IN-IDKONTO        TO UT-IDKONTO                                 
020991                               UT-IDKONTO-SF                              
020992     INSPECT UT-IDKONTO REPLACING LEADING ZERO BY SPACE                   
020993     INSPECT UT-IDKONTO-SF REPLACING LEADING ZERO BY SPACE                
020994     MOVE IN-IDKST          TO UT-IDKST                                   
020995                               UT-IDKST-SF                                
020996     MOVE IN-IDANALYS       TO UT-IDANALYS                                
020997                               UT-IDANALYS-SF                             
020998     MOVE IN-SUBEL          TO UT-SUBEL                                   
020999                               WS-SUBEL                                   
021000     MOVE IN-IDTECKEN       TO UT-TECKEN                                  
021001     IF IN-IDTECKEN = '-'                                                 
021002        COMPUTE WS-SUBEL = 0 - WS-SUBEL                                   
021003        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
021004     ELSE                                                                 
021005        MOVE WS-SUBEL       TO UT-SUBEL-SF                                
021006     END-IF                                                               
021007                                                                          
021008     MOVE IN-IDPRCTR        TO UT-IDPRCTR                                 
021009                               UT-IDPRCTR-SF                              
021010     MOVE IN-KDPOST         TO UT-KDPOST                                  
021011                               UT-KDPOST-SF                               
021012     MOVE IN-PRARTSTD       TO UT-PRARTSTD-SF                             
021013     MOVE IN-FLLSBOK        TO UT-FLLSBOK-SF                              
021014     MOVE IN-KDTRADP        TO UT-KDTRADP-SF                              
021020     IF IN-KDEKHHT = '303'                                                
021100                  OR '304'                                                
021200                  OR '201'                                                
021300                  OR '202'                                                
021400                  OR '203'                                                
021500                  OR '204'                                                
021600                  OR '205'                                                
021700                  OR '501'                                                
021800        PERFORM S02-SKRIV-LISTA1                                          
021900     ELSE                                                                 
022000        PERFORM S03-SKRIV-LISTA2                                          
022100     END-IF                                                               
022101                                                                          
022110     PERFORM S04-SKRIV-LIST-SF                                            
022200                                                                          
022300     MOVE IN-IDVERGL        TO W-IDVERGL                                  
022310     IF IN-IDARTNR NOT = 0                                                
022400       MOVE IN-IDARTNR        TO W-IDARTNR                                
022410     END-IF                                                               
022500     MOVE IN-KVANTAL        TO W-ANTAL                                    
022600     MOVE SPACE             TO DETALJRAD-1                                
022610     MOVE SPACE             TO UT-HEADER1                                 
022620     MOVE SPACE             TO UT-HEADER2                                 
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 Z-AVSLUTA SECTION.                                                       
023100     CLOSE LISTA1 W57075 LISTA3                                           
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 S01-LAS-INFIL SECTION.                                                   
023600     READ W57075 INTO INAREA                                              
023700        AT END MOVE JA TO IN-EOF                                          
023800     END-READ                                                             
023900     .                                                                    
023910 S11-WRITE-HEADERS SECTION.                                               
023920                                                                          
023960     WRITE LISTPOST3           FROM UT-HEADER1                            
023970                                                                          
023990     WRITE LISTPOST4           FROM UT-HEADER2                            
023991                                                                          
023992     WRITE LISTPOST5           FROM UT-HEADER-SF                          
023993     .                                                                    
024000                                                                          
024100 S02-SKRIV-LISTA1 SECTION.                                                
024200                                                                          
024300     IF IN-IDVERGL NOT = W-IDVERGL-1                                      
024400       WRITE LISTPOST1 FROM TITEL1  AFTER PAGE                            
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
025910 S03-SKRIV-LISTA2 SECTION.                                                
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
027500 S04-SKRIV-LIST-SF SECTION.                                               
027600                                                                          
027700     WRITE LISTPOST5       FROM UT-AREA-SF                                
027800     .                                                                    
027900     EJECT                                                                
