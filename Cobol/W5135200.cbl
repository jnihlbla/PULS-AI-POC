001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5135200.                                                
001300 AUTHOR.         ASPFJÄLL MARKUS.                                         
001400 DATE-WRITTEN.   05/07/13.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER PARAMETRAR IFRÅN SOP O SKAPAR SEDAN FIL SOM                
001900*        SKICKAS SOM MAIL                                                 
002000*                                                                         
002101*        PROGRAMMET LÄSER      WDH1                                       
002110*        PROGRAMMET LÄSER      WDH7                                       
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- INFIL SOP PARAMETERAR                                      
003403     SELECT W513S1                     ASSIGN TO W51352D1.                
003404     SKIP2                                                                
003405*          --- UTFIL MED INVENTERINGSPOSTER                               
003410     SELECT W51352-001                 ASSIGN TO W51352D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W513S1                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W51352      -L.                                                
004007     SKIP3                                                                
004008 FD  W51352-001                                                           
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004020 01  W51352-001-RAD              PIC X(200).                              
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W5135200'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  INDX                        PIC S9(4)   COMP SYNC.                   
004801                                                                          
004802 01  WS-IDUSER-CRE               PIC X(8).                                
004803 01  WS-IDUSER-CLO               PIC X(8).                                
004804 01  WS-IDUSER-PR1               PIC X(8).                                
004805 01  WS-IDUSER-PR2               PIC X(8).                                
004806 01  WS-IDUSER-PR3               PIC X(8).                                
004808 01  WS-DAREGDAT-CRE             PIC 9(8).                                
004809 01  WS-DAREGDAT-CLO             PIC 9(8).                                
004810 01  WS-DAREGDAT-PR1             PIC 9(8).                                
004811 01  WS-DAREGDAT-PR2             PIC 9(8).                                
004812 01  WS-DAREGDAT-PR3             PIC 9(8).                                
004813 01  WS-KDINVKAT                 PIC 9(2).                                
004814 01  WS-KDJUSTYP                 PIC 9(2).                                
004816 01  WS-IDARTNR                  PIC 9(8).                                
004817 01  WS-ANTAL-POST               PIC S9(3) COMP-3.                        
004818 01  WS-ANTALDAGAR               PIC 9(3).                                
004819 01  WS-KDSEGKEY                 PIC 9(3).                                
004820 01  WS-TAL                      PIC 9(5).                                
004821 01  W-DAREGDAT-SORT             PIC 9(8)    VALUE ZERO.                  
004822                                                                          
004823 77  W513S1-EOF-SW               PIC X       VALUE 'N'.                   
004824     88  END-OF-W513S1                       VALUE 'J'.                   
004825                                                                          
004830 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004840     88  NYCKLAR-OK                          VALUE 'J'.                   
004850     88  NYCKLAR-FEL                         VALUE 'N'.                   
004860                                                                          
004870 77  WS-SOK-TYP                    PIC X(10) VALUE SPACE.                 
004880     88  LAES-ARTIKEL                  VALUE 'ARTIKEL   '.                
004890     88  LAES-KDINVKAT                 VALUE 'KDINVKAT  '.                
004891     88  LAES-USERF                    VALUE 'USERF     '.                
004892     88  LAES-USERD                    VALUE 'USERD     '.                
004893     88  LAES-DATUM                    VALUE 'DATUM     '.                
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005410                                                                          
005411 01  WS-DATUM-SSAAMMDD             PIC 9(8).                              
005412 01  WS-DATUM-AAAAMMDD REDEFINES WS-DATUM-SSAAMMDD.                       
005413     03 WS-DATUM-SS                PIC 9(2).                              
005414     03 WS-DATUM-AAMMDD            PIC 9(6).                              
005415                                                                          
005420                                                                          
005430                                                                          
005440 01  WS-DATUM-FOM                  PIC 9(8).                              
005500 01  WS-DATUM-TOM                  PIC 9(8).                              
005510                                                                          
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006120     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*01  -COPY W0005   -PRE  POSTSUM-                                         
007204     EJECT                                                                
007205*01  -COPY WDAGAREA                                                       
007206     EJECT                                                                
007207*    ----  DATUM AREA                                                     
007401     EJECT                                                                
007402 01  FILLER                        PIC X(16) VALUE ' W-DATUM'.            
007403 01  WS-DATUM               PIC 9(8).                                     
007404 01  WS-DATUM1-9 REDEFINES WS-DATUM.                                      
007405     03 WS-DATUM-SEKEL      PIC 9(2).                                     
007406     03 WS-DATUM4-9         PIC 9(6).                                     
007407                                                                          
007408 01  WS-DATUM1                     PIC 9(9).                              
007409 01  WS-DATUM2                     PIC 9(9).                              
007410                                                                          
007411 01  WS-TID                        PIC 9(8).                              
007412 01  WS-TID1-9 REDEFINES WS-TID.                                          
007413     03 WS-HHMM                    PIC 9(4).                              
007414     03 WS-SSTTT                   PIC 9(4).                              
007415                                                                          
007416******* MAIL INFO IFRÅN WDH1 *************                                
007417 01  MAIL-LISTAN                 PIC X(24)   VALUE 'MAIL-INFO'.           
007418                                                                          
007419 01  FILLER                      PIC X(16)   VALUE 'MAIL-RAD'.            
007420 01  MAIL-RUB1.                                                           
007421     03 FILLER                   PIC X(29)   VALUE                        
007422       'INVENTORY ADJUSTMENTS WORKING'.                                   
007423     03 FILLER                   PIC X(20) VALUE SPACE.                   
007424     03 FILLER                   PIC X(6)  VALUE                          
007425       'DATE: '.                                                          
007426     03 MAIL-DAGENS-DATUM        PIC X(8).                                
007427     03 FILLER                   PIC X(7)  VALUE                          
007428       ' TIME: '.                                                         
007429     03 MAIL-TID-HHMM            PIC X(4).                                
007430                                                                          
007431 01  FILLER                      PIC X(16)   VALUE 'MAIL-RAD'.            
007432 01  MAIL-RUB2.                                                           
007433     03 FILLER                   PIC X(43)  VALUE SPACE.                  
007434     03 FILLER                   PIC X(4)   VALUE 'TIME'.                 
007435     03 FILLER                   PIC X(11)  VALUE SPACE.                  
007436     03 FILLER                   PIC X(10)  VALUE 'TOTAL TIME'.           
007437                                                                          
007438 01  MAIL-RUB3.                                                           
007439     03 FILLER                   PIC X(35)  VALUE                         
007440       'PARTNO    TYPE                   ID'.                             
007441     03 FILLER                   PIC X(27)  VALUE                         
007442       '        FROM TOM DAYS  FROM'.                                     
007443     03 FILLER                   PIC X(14) VALUE                          
007444       '  TOM     DAYS'.                                                  
007445                                                                          
007446 01  MAIL-RAD.                                                            
007447     03  CR1-IDARTNR             PIC Z(9)    VALUE ZERO.                  
007448     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007449     03  CR1-TYPE                PIC X(3)    VALUE SPACE.                 
007450     03  FILLER                  PIC X(1)    VALUE SPACE.                 
007451     03  FILLER                  PIC X(9)    VALUE                        
007452         'CREATED  '.                                                     
007453     03  CR1-DATE                PIC X(8)    VALUE SPACE.                 
007454     03  FILLER                  PIC X(1)    VALUE SPACE.                 
007455     03  CR1-IDUSER-CRE          PIC X(8)    VALUE SPACE.                 
007456                                                                          
007457 01  MAIL-RAD1.                                                           
007458     03  FILLER                  PIC X(11)   VALUE SPACE.                 
007459     03  RAD1-TYPE               PIC X(2)    VALUE SPACE.                 
007460     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007461     03  FILLER                  PIC X(9)    VALUE                        
007462         'PRINTED1 '.                                                     
007463     03  RAD1-DATE               PIC X(8)    VALUE SPACE.                 
007464     03  FILLER                  PIC X       VALUE SPACE.                 
007465     03  RAD1-USER               PIC X(8)    VALUE SPACE.                 
007466     03  FILLER                  PIC X(12)   VALUE                        
007467         '  CR   PR1  '.                                                  
007468     03  RAD1-WS-DATE            PIC Z(3)   VALUE ZERO.                   
007469     03  FILLER                  PIC X(16)   VALUE                        
007470         '  CR    PR1     '.                                              
007471     03  RAD1-WS-DATETOT         PIC Z(3)    VALUE ZERO.                  
007472                                                                          
007473 01  FILLER                      PIC X(16)   VALUE 'MAIL-RAD2'.           
007474 01  MAIL-RAD2.                                                           
007475     03  FILLER                  PIC X(11)   VALUE SPACE.                 
007476     03  RAD2-TYPE               PIC X(2)    VALUE SPACE.                 
007477     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007478     03  FILLER                  PIC X(9)    VALUE                        
007479         'PRINTED2 '.                                                     
007480     03  RAD2-DATE               PIC X(8)    VALUE SPACE.                 
007481     03  FILLER                  PIC X       VALUE SPACE.                 
007482     03  RAD2-USER               PIC X(8)    VALUE SPACE.                 
007483     03  FILLER                  PIC X(12)   VALUE                        
007484         '  PR1  PR2  '.                                                  
007485     03  RAD2-WS-DATE            PIC Z(3)    VALUE ZERO.                  
007486     03  FILLER                  PIC X(16)   VALUE                        
007487         '  CR    PR2     '.                                              
007488     03  RAD2-WS-DATETOT         PIC Z(3)    VALUE ZERO.                  
007489                                                                          
007490 01  FILLER                      PIC X(16)   VALUE 'MAIL-RAD3'.           
007491 01  MAIL-RAD3.                                                           
007492     03  FILLER                  PIC X(11)   VALUE SPACE.                 
007493     03  RAD3-TYPE               PIC X(2)    VALUE SPACE.                 
007494     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007495     03  FILLER                  PIC X(9)    VALUE                        
007496         'PRINTED3 '.                                                     
007497     03  RAD3-DATE               PIC X(8)    VALUE SPACE.                 
007498     03  FILLER                  PIC X       VALUE SPACE.                 
007499     03  RAD3-USER               PIC X(8)    VALUE SPACE.                 
007500     03  FILLER                  PIC X(12)   VALUE                        
007501         '  PR2  PR3  '.                                                  
007502     03  RAD3-WS-DATE            PIC Z(3)    VALUE ZERO.                  
007503     03  FILLER                  PIC X(16)   VALUE                        
007504         '  CR    PR3     '.                                              
007505     03  RAD3-WS-DATETOT         PIC Z(3)    VALUE ZERO.                  
007506 01  MAIL-TOM.                                                            
007507     03 FILLER                   PIC X(1)   VALUE SPACE.                  
007508                                                                          
007509******* MAIL INFO IFRÅN WDH7 *************                                
007510 01  FILLER                  PIC X(16)   VALUE 'CREATED-H7-RAD1'.         
007511 01  H7-MAIL-RUB1.                                                        
007512     03 FILLER                   PIC X(29)   VALUE                        
007513       'INVENTORY ADJUSTMENTS HISTORY'.                                   
007514     03 FILLER                   PIC X(20) VALUE SPACE.                   
007515     03 FILLER                   PIC X(6)  VALUE                          
007516       'DATE: '.                                                          
007517     03 H7-MAIL-DAGENS-DATUM     PIC X(8).                                
007518     03 FILLER                   PIC X(7)  VALUE                          
007519       ' TIME: '.                                                         
007520     03 H7-MAIL-TID-HHMM         PIC X(4).                                
007521                                                                          
007522 01  H7-MAIL-RUB2.                                                        
007523     03 FILLER                   PIC X(9)   VALUE 'PART NO. '.            
007524     03 H7-IDARTNR               PIC Z(7)9   VALUE ZERO.                  
007526     03 FILLER                   PIC X(14)  VALUE SPACE.                  
007527     03 FILLER                   PIC X(4)   VALUE 'TIME'.                 
007528     03 FILLER                   PIC X(11)  VALUE SPACE.                  
007529     03 FILLER                   PIC X(10)  VALUE 'TOTAL TIME'.           
007530                                                                          
007531 01  H7-MAIL-RUB3.                                                        
007532     03 FILLER                   PIC X(4)  VALUE 'TYPE'.                  
007533     03 FILLER                   PIC X(19) VALUE SPACE.                   
007534     03 FILLER                   PIC X(2)  VALUE 'ID'.                    
007535     03 FILLER                   PIC X(6)  VALUE SPACE.                   
007536     03 FILLER                   PIC X(32)                                
007537        VALUE 'FROM TOM DAYS  FROM  TOM    DAYS'.                         
007538                                                                          
007539 01  H7-CREATED-RAD1.                                                     
007540     03  FILLER                  PIC X(1)    VALUE SPACE.                 
007541     03  H7-CRE-TYPE             PIC Z(2)    VALUE SPACE.                 
007542     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007543     03  FILLER                  PIC X(9)    VALUE                        
007544         'CREATED  '.                                                     
007545     03  H7-CRE-DATE             PIC X(8)    VALUE SPACE.                 
007546     03  FILLER                  PIC X(1)    VALUE SPACE.                 
007547     03  H7-CRE-IDUSER           PIC X(8)    VALUE SPACE.                 
007548                                                                          
007549 01  FILLER                  PIC X(16)   VALUE 'MAIL2-RAD1'.              
007550 01  H7-MAIL-RAD1.                                                        
007551     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007552     03  H7-RAD1-TYPE            PIC Z(2)    VALUE SPACE.                 
007553     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007554     03  FILLER                  PIC X(9)    VALUE                        
007555         'PRINTED1 '.                                                     
007556     03  H7-RAD1-DATE            PIC X(8)    VALUE SPACE.                 
007557     03  FILLER                  PIC X       VALUE SPACE.                 
007558     03  H7-RAD1-USER            PIC X(8)    VALUE SPACE.                 
007559     03  FILLER                  PIC X(10)   VALUE                        
007560         'CR   PR1  '.                                                    
007561     03  H7-RAD1-WS-DATE         PIC Z(3)    VALUE ZERO.                  
007562     03  FILLER                  PIC X(16)   VALUE                        
007563         '  CR    PR1     '.                                              
007564     03  H7-RAD1-WS-DATETOT      PIC Z(3)    VALUE ZERO.                  
007565                                                                          
007566 01  FILLER                 PIC X(16)   VALUE 'MAIL2-H7-RAD2'.            
007567 01  H7-MAIL-RAD2.                                                        
007568     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007569     03  H7-RAD2-TYPE            PIC Z(2)    VALUE SPACE.                 
007570     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007571     03  FILLER                  PIC X(9)    VALUE                        
007572         'PRINTED2 '.                                                     
007573     03  H7-RAD2-DATE            PIC X(8)    VALUE SPACE.                 
007574     03  FILLER                  PIC X       VALUE SPACE.                 
007575     03  H7-RAD2-USER            PIC X(8)    VALUE SPACE.                 
007576     03  FILLER                  PIC X(10)   VALUE                        
007577         'PR1  PR2  '.                                                    
007578     03  H7-RAD2-WS-DATE         PIC Z(3)    VALUE ZERO.                  
007579     03  FILLER                  PIC X(16)   VALUE                        
007580         '  CR    PR2     '.                                              
007581     03  H7-RAD2-WS-DATETOT      PIC Z(3)    VALUE ZERO.                  
007582                                                                          
007583 01  FILLER                  PIC X(16)   VALUE 'MAIL2-H7-RAD3'.           
007584 01  H7-MAIL-RAD3.                                                        
007585     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007586     03  H7-RAD3-TYPE            PIC Z(2)    VALUE SPACE.                 
007587     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007588     03  FILLER                  PIC X(9)    VALUE                        
007589         'PRINTED3 '.                                                     
007590     03  H7-RAD3-DATE            PIC X(8)    VALUE SPACE.                 
007591     03  FILLER                  PIC X       VALUE SPACE.                 
007592     03  H7-RAD3-USER            PIC X(8)    VALUE SPACE.                 
007593     03  FILLER                  PIC X(10)   VALUE                        
007594         'PR2  PR3  '.                                                    
007595     03  H7-RAD3-WS-DATE         PIC Z(3)    VALUE ZERO.                  
007596     03  FILLER                  PIC X(16)   VALUE                        
007597         '  CR    PR3     '.                                              
007598     03  H7-RAD3-WS-DATETOT      PIC Z(3)    VALUE ZERO.                  
007599                                                                          
007600 01  FILLER                      PIC X(16)   VALUE 'MAIL2-RAD4'.          
007601 01  H7-MAIL-RAD4.                                                        
007602     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007603     03  H7-RAD4-TYPE            PIC Z(2)    VALUE SPACE.                 
007604     03  FILLER                  PIC X(2)    VALUE SPACE.                 
007605     03  FILLER                  PIC X(9)    VALUE                        
007606         'CLOSED   '.                                                     
007607     03  H7-RAD4-DATE            PIC X(8)    VALUE SPACE.                 
007608     03  FILLER                  PIC X       VALUE SPACE.                 
007609     03  H7-RAD4-USER            PIC X(8)    VALUE SPACE.                 
007610     03  FILLER                  PIC X(10)   VALUE                        
007611         'PR3  CL   '.                                                    
007612     03  H7-RAD4-WS-DATE         PIC Z(3)    VALUE ZERO.                  
007613     03  FILLER                  PIC X(16)   VALUE                        
007614         '  CR    CL      '.                                              
007615     03  H7-RAD4-WS-DATETOT      PIC Z(3)    VALUE ZERO.                  
007616                                                                          
007617 01  PARM-AREA-START             PIC X(24)   VALUE                        
007618                                 'PARM-AREA-START '.                      
007619 01  PARM-AREA                   PIC X(27).                               
007620 01  FILLER REDEFINES PARM-AREA.                                          
007621     03 PARM-BAS                 PIC 9(4).                                
007622     03 PARM-IDDC                PIC X(2).                                
007623     03 PARM-SOK-TYP             PIC X(10).                               
007624     03 PARM-IDARTNR             PIC S9(9) COMP-3.                        
007625     03 PARM-KDINVKAT            PIC S9(3) COMP-3.                        
007626     03 PARM-DATUM               PIC 9(6).                                
007627     03 PARM-IDUSER              PIC X(8).                                
007628     EJECT                                                                
007629                                                                          
007630 01  IN-AREA-START               PIC X(24)   VALUE                        
007631                                 'IN-AREA-START  '.                       
007632     SKIP2                                                                
007633                                                                          
007634*01  AREA -COPY W51352     -PRE IN-                                       
007635     EJECT                                                                
007640*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008203     03  W-IDARTNR-X.                                                     
008204         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008205     03  W-IDDC-X.                                                        
008206         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008209     03  W-TISEGKEY-X.                                                    
008210         05  W-TISEGKEY     PIC S9(9)   VALUE +999999999 COMP-3.          
008211     03  W-KDJUSTYP-X.                                                    
008212         05  W-KDJUSTYP                  PIC 9(2).                        
008213                                                                          
008214     03 W-WDH111KY-X.                                                     
008215       05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.               
008216       05    W-KDINVKAT-UNIK       PIC S9(3)   VALUE ZERO COMP-3.         
008217       05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.         
008218       05    W-DAREGDAT-SORT-UNIK PIC 9(8)      VALUE ZERO.               
008219                                                                          
008220     03 W-WDH111-KEY-MIN-X.                                               
008221       05    W-IDDC-MIN        PIC X(2)  VALUE SPACE.                     
008222       05    W-KDINVKAT-MIN    PIC S9(3) VALUE ZERO COMP-3.               
008230       05    W-TISEGKEY-MIN    PIC S9(9) VALUE ZERO COMP-3.               
008240       05    W-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                     
008250                                                                          
008260     03 W-WDH111-KEY-MAX-X.                                               
008270       05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                     
008280       05    W-KDINVKAT-MAX    PIC S9(3) VALUE +049       COMP-3.         
008290       05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
008291       05    W-DAREGDAT-SORT-MIN PIC 9(8) VALUE 99999999.                 
008292                                                                          
008293                                                                          
008294   03  W-WDH1C1KY-X.                                                      
008295     05 W-SEQC-IDDC                 PIC X(2)  VALUE SPACE.                
008296     05 W-SEQC-KDINVKAT             PIC S9(3) VALUE ZERO COMP-3.          
008297     05 W-SEQC-DAREGDAT-SORT        PIC 9(8)  VALUE ZERO.                 
008298     05 W-SEQC-IDARTNR              PIC S9(9) VALUE ZERO COMP-3.          
008299     05 W-SEQC-TISEGKEY             PIC S9(9) VALUE ZERO COMP-3.          
008300                                                                          
008301   03  W-WDH1C1KY-MIN-X.                                                  
008302     05 W-SEQC-IDDC-MIN             PIC X(2)  VALUE SPACE.                
008303     05 W-SEQC-KDINVKAT-MIN         PIC S9(3) VALUE ZERO COMP-3.          
008304     05 W-SEQC-DAREGDAT-SORT-MIN    PIC 9(8)  VALUE ZERO.                 
008305     05 W-SEQC-IDARTNR-MIN          PIC S9(9) VALUE ZERO COMP-3.          
008306     05 W-SEQC-TISEGKEY-MIN         PIC S9(9) VALUE ZERO COMP-3.          
008307                                                                          
008308   03  W-WDH1C1KY-MAX-X.                                                  
008309     05 W-SEQC-IDDC-MAX             PIC X(2)  VALUE SPACE.                
008310     05 W-SEQC-KDINVKAT-MAX         PIC S9(3) VALUE +999  COMP-3.         
008311     05 W-SEQC-DAREGDAT-SORT-MAX    PIC 9(8)  VALUE 99999999.             
008312     05 W-SEQC-IDARTNR-MAX     PIC S9(9) VALUE +999999999 COMP-3.         
008313     05 W-SEQC-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
008314                                                                          
008315   03  W-WDH1E1KY-X.                                                      
008316     05 W-SEQE-IDDC            PIC X(2).                                  
008317     05 W-SEQE-DAREGDAT-SORT PIC 9(8).                                    
008318     05 W-SEQE-KDINVKAT        PIC S9(3)           COMP-3.                
008319     05 W-SEQE-IDARTNR         PIC S9(9)           COMP-3.                
008320     05 W-SEQE-TISEGKEY        PIC S9(9)           COMP-3.                
008321                                                                          
008322   03  W-WDH1E1KY-MIN-X.                                                  
008323     05 W-SEQE-IDDC-MIN            PIC X(2).                              
008324     05 W-SEQE-DAREGDAT-SORT-MIN PIC 9(8).                                
008325     05 W-SEQE-KDINVKAT-MIN        PIC S9(3) COMP-3 VALUE ZERO.           
008326     05 W-SEQE-IDARTNR-MIN         PIC S9(9) COMP-3 VALUE ZERO.           
008327     05 W-SEQE-TISEGKEY-MIN        PIC S9(9) COMP-3 VALUE ZERO.           
008328                                                                          
008329   03  W-WDH1E1KY-MAX-X.                                                  
008330     05 W-SEQE-IDDC-MAX            PIC X(2).                              
008331     05 W-SEQE-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                 
008332     05 W-SEQE-KDINVKAT-MAX        PIC S9(3) COMP-3 VALUE +999.           
008333     05 W-SEQE-IDARTNR-MAX     PIC S9(9) COMP-3 VALUE +999999999.         
008334     05 W-SEQE-TISEGKEY-MAX    PIC S9(9) COMP-3 VALUE +999999999.         
008335                                                                          
008336   03  W-WDH1F1KY-X.                                                      
008337     05 W-SEQF-IDUSER          PIC X(8).                                  
008338     05 W-SEQF-IDDC            PIC X(2).                                  
008339     05 W-SEQF-KDINVKAT        PIC S9(3) COMP-3.                          
008340     05 W-SEQF-DAREGDAT-SORT PIC 9(8).                                    
008341     05 W-SEQF-TISEGKEY        PIC S9(9) COMP-3.                          
008342     05 W-SEQF-IDARTNR         PIC S9(9) COMP-3.                          
008343     05 W-SEQF-KDSEGKEY        PIC X.                                     
008344                                                                          
008345   03  W-WDH1F1KY-MIN-X.                                                  
008346     05 W-SEQF-IDUSER-MIN          PIC X(8).                              
008347     05 W-SEQF-IDDC-MIN            PIC X(2).                              
008348     05 W-SEQF-KDINVKAT-MIN        PIC S9(3) VALUE ZERO COMP-3.           
008349     05 W-SEQF-DAREGDAT-SORT-MIN PIC 9(8)    VALUE ZERO.                  
008350     05 W-SEQF-TISEGKEY-MIN        PIC S9(9) VALUE ZERO COMP-3.           
008351     05 W-SEQF-IDARTNR-MIN         PIC S9(9) VALUE ZERO COMP-3.           
008352     05 W-SEQF-KDSEGKEY-MIN        PIC X     VALUE LOW-VALUE.             
008353                                                                          
008354   03  W-WDH1F1KY-MAX-X.                                                  
008355     05 W-SEQF-IDUSER-MAX          PIC X(8) VALUE HIGH-VALUE.             
008356     05 W-SEQF-IDDC-MAX            PIC X(2).                              
008357     05 W-SEQF-KDINVKAT-MAX        PIC S9(3) VALUE +999 COMP-3.           
008358     05 W-SEQF-DAREGDAT-SORT-MAX PIC 9(8)    VALUE 99999999.              
008359     05 W-SEQF-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
008360     05 W-SEQF-IDARTNR-MAX     PIC S9(9) VALUE +999999999 COMP-3.         
008361     05 W-SEQF-KDSEGKEY-MAX        PIC X     VALUE HIGH-VALUE.            
008362                                                                          
008363   03  W-WDH1D1KY-X.                                                      
008364     05 W-SEQD-IDUSER          PIC X(8).                                  
008365     05 W-SEQD-IDDC            PIC X(2).                                  
008366     05 W-SEQD-DAREGDAT-SORT PIC 9(8).                                    
008367     05 W-SEQD-KDINVKAT        PIC S9(3) COMP-3.                          
008368     05 W-SEQD-TISEGKEY        PIC S9(9) COMP-3.                          
008369     05 W-SEQD-IDARTNR         PIC S9(9) COMP-3.                          
008370     05 W-SEQD-KDSEGKEY        PIC X.                                     
008371                                                                          
008372   03  W-WDH1D1KY-MIN-X.                                                  
008373     05 W-SEQD-IDUSER-MIN          PIC X(8) VALUE LOW-VALUE.              
008374     05 W-SEQD-IDDC-MIN            PIC X(2) VALUE LOW-VALUE.              
008375     05 W-SEQD-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                     
008376     05 W-SEQD-KDINVKAT-MIN        PIC S9(3) COMP-3 VALUE ZERO.           
008377     05 W-SEQD-TISEGKEY-MIN        PIC S9(9) COMP-3 VALUE ZERO.           
008378     05 W-SEQD-IDARTNR-MIN         PIC S9(9) COMP-3 VALUE ZERO.           
008379     05 W-SEQD-KDSEGKEY-MIN        PIC X    VALUE LOW-VALUE.              
008380                                                                          
008381   03  W-WDH1D1KY-MAX-X.                                                  
008382     05 W-SEQD-IDUSER-MAX          PIC X(8) VALUE HIGH-VALUE.             
008383     05 W-SEQD-IDDC-MAX            PIC X(2) VALUE HIGH-VALUE.             
008384     05 W-SEQD-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                 
008385     05 W-SEQD-KDINVKAT-MAX        PIC S9(3) VALUE +999 COMP-3.           
008386     05 W-SEQD-TISEGKEY-MAX     PIC S9(9) VALUE +999999999 COMP-3.        
008387     05 W-SEQD-IDARTNR-MAX      PIC S9(9) VALUE +999999999 COMP-3.        
008388     05 W-SEQD-KDSEGKEY-MAX        PIC X    VALUE HIGH-VALUE.             
008389                                                                          
008390     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(128).                              
009400 01  SSA2                        PIC X(128).                              
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH101'.                      
010102 01  DLI-IO-WDH101.                                                       
010103*    03  -COPY WDH101                                                     
010104     EJECT                                                                
010105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH111'.                      
010106 01  DLI-IO-WDH111.                                                       
010107*    03  -COPY WDH111                                                     
010108 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH121'.                      
010109 01  DLI-IO-WDH121.                                                       
010110*    03  -COPY WDH121                                                     
010111 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1C1'.                      
010112 01  DLI-IO-WDH1C1.                                                       
010113*    03  -COPY WDH1C1                                                     
010114 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1D1'.                      
010115 01  DLI-IO-WDH1D1.                                                       
010116*    03  -COPY WDH1D1                                                     
010117 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1E1'.                      
010118 01  DLI-IO-WDH1E1.                                                       
010119*    03  -COPY WDH1E1                                                     
010120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1F1'.                      
010121 01  DLI-IO-WDH1F1.                                                       
010122*    03  -COPY WDH1F1                                                     
010123 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH701'.                      
010124 01  DLI-IO-WDH701.                                                       
010125*    03  -COPY WDH701                                                     
010126     EJECT                                                                
010127 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH711'.                      
010128 01  DLI-IO-WDH711.                                                       
010130*    03  -COPY WDH711                                                     
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010701                                                                          
010702*01  -COPY W0009   -PRE MSG-                                              
010703     EJECT                                                                
010704*01  -COPY W0008  -PRE WDH1-                                              
010705     05  FILLER                  PIC X.                                   
010706*01  -COPY W0008  -PRE WDH1C-                                             
010707     05  FILLER                  PIC X.                                   
010708*01  -COPY W0008  -PRE WDH1D-                                             
010709     05  FILLER                  PIC X.                                   
010710*01  -COPY W0008  -PRE WDH1E-                                             
010711     05  FILLER                  PIC X.                                   
010712*01  -COPY W0008  -PRE WDH1F-                                             
010713     05  FILLER                  PIC X.                                   
010714                                                                          
010715*01  -COPY W0008  -PRE WDH7-                                              
010720     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010901 PROCEDURE DIVISION  USING MSG-PCB WDH1-PCB WDH1C-PCB WDH1D-PCB           
010902                           WDH1E-PCB WDH1F-PCB WDH7-PCB.                  
010903                                                                          
010904 MAIN SECTION.                                                            
010905     ENTRY 'DLITCBL' USING MSG-PCB WDH1-PCB WDH1C-PCB WDH1D-PCB           
010910                           WDH1E-PCB WDH1F-PCB WDH7-PCB.                  
011000                                                                          
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-LAES-W513S1                                              
011520     PERFORM B-KOLLA-NYCKLAR                                              
011530**   DISPLAY 'IN-AREA ' IN-AREA                                           
012200     IF IN-BAS = 'WDH1'                                                   
012500         PERFORM C-LAES-WDH1                                              
012600     ELSE                                                                 
012610       IF IN-BAS = 'WDH7'                                                 
012620         PERFORM D-LAES-WDH7                                              
012630       END-IF                                                             
012640     END-IF                                                               
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
012910**** CALL ABEND                                                           
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN INPUT  W513S1                                                   
013501                                                                          
013510     OPEN OUTPUT W51352-001                                               
013600                                                                          
013700     ACCEPT DAGENS-DATUM      FROM DATE                                   
013710     ACCEPT WS-DATUM-SSAAMMDD FROM DATE                                   
013800     ACCEPT WS-TID            FROM TIME                                   
013801     MOVE WS-DATUM-AAMMDD  TO MAIL-DAGENS-DATUM                           
013802                              H7-MAIL-DAGENS-DATUM                        
013803     MOVE WS-HHMM          TO MAIL-TID-HHMM                               
013804                              H7-MAIL-TID-HHMM                            
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014101 B-KOLLA-NYCKLAR SECTION.                                                 
014102     IF IN-IDARTNR NUMERIC                                                
014103       MOVE IN-IDARTNR      TO W-IDARTNR                                  
014104     ELSE                                                                 
014105       MOVE NEJ TO NYCKLAR-SW                                             
014106     END-IF                                                               
014107     IF IN-IDDC NOT = SPACE                                               
014108       MOVE IN-IDDC         TO W-IDDC                                     
014109                               W-IDDC-MIN                                 
014110                               W-IDDC-MAX                                 
014111     ELSE                                                                 
014112       MOVE NEJ TO NYCKLAR-SW                                             
014113     END-IF                                                               
014114     .                                                                    
014120     EJECT                                                                
014130 C-LAES-WDH1 SECTION.                                                     
014131     MOVE IN-SOK-TYP TO WS-SOK-TYP                                        
014132     IF LAES-ARTIKEL                                                      
014133       MOVE IN-IDARTNR TO W-IDARTNR                                       
014140       PERFORM IMS-GU-WDH101                                              
014141       MOVE IN-IDDC TO W-IDDC-MIN                                         
014142                       W-IDDC-MAX                                         
014150       IF SEGMENT-FINNS                                                   
014160         PERFORM IMS-GNP-WDH111                                           
014161         IF SEGMENT-FINNS                                                 
014163           MOVE INV-DAREGDAT-CRE  TO WS-DAREGDAT-CRE                      
014164           MOVE INV-DAREGDAT-PR1  TO WS-DAREGDAT-PR1                      
014165           MOVE INV-DAREGDAT-PR2  TO WS-DAREGDAT-PR2                      
014166           MOVE INV-DAREGDAT-PR3  TO WS-DAREGDAT-PR3                      
014167           MOVE INV-KDINVKAT      TO WS-KDINVKAT                          
014168           PERFORM IMS-GNP-WDH121                                         
014169           MOVE  +1   TO WS-ANTAL-POST                                    
014170           MOVE  ZERO TO WS-KDSEGKEY                                      
014173           IF SEGMENT-FINNS                                               
014174*****        PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4            
014175             PERFORM UNTIL SEGMENT-SAKNAS                                 
014176               IF INVL-KDSEGKEY = '0'                                     
014177                 MOVE INVL-IDUSER TO WS-IDUSER-CRE                        
014178               END-IF                                                     
014179               IF INVL-KDSEGKEY = '1'                                     
014180                 MOVE INVL-IDUSER TO WS-IDUSER-PR1                        
014181               END-IF                                                     
014182               IF INVL-KDSEGKEY = '2'                                     
014183                 MOVE INVL-IDUSER TO WS-IDUSER-PR2                        
014184               END-IF                                                     
014185               IF INVL-KDSEGKEY = '3'                                     
014186                 MOVE INVL-IDUSER TO WS-IDUSER-PR3                        
014187                 MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                     
014188****             ADD +4 TO WS-ANTAL-POST                                  
014189               END-IF                                                     
014190***            IF WS-KDSEGKEY < 3                                         
014191                 PERFORM IMS-GNP-WDH121                                   
014192****           END-IF                                                     
014193               ADD +1 TO WS-ANTAL-POST                                    
014194             END-PERFORM                                                  
014195             MOVE '  ' TO STATUS-WS                                       
014196             PERFORM CA-FYLL-UTAREA                                       
014197           END-IF                                                         
014198         END-IF                                                           
014199       END-IF                                                             
014200*                                                                         
014201     ELSE                                                                 
014202       IF LAES-KDINVKAT                                                   
014203         MOVE IN-KDINVKAT TO W-SEQC-KDINVKAT-MIN                          
014204                             W-SEQC-KDINVKAT-MAX                          
014205                             W-KDINVKAT-UNIK                              
014206         MOVE IN-IDDC     TO W-SEQC-IDDC-MIN                              
014207         MOVE IN-IDDC     TO W-SEQC-IDDC-MAX                              
014208                             W-IDDC-UNIK                                  
014209         PERFORM IMS-GN-WDH1C1                                            
014210         IF SEGMENT-FINNS                                                 
014211           MOVE SEQC-IDARTNR          TO W-IDARTNR                        
014212                                                                          
014213           MOVE SEQC-TISEGKEY         TO W-TISEGKEY-UNIK                  
014214                                                                          
014215           MOVE SEQC-DAREGDAT-SORT    TO W-DAREGDAT-SORT-UNIK             
014216                                                                          
014217           MOVE SEQC-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                  
014218           MOVE SEQC-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                  
014219           MOVE SEQC-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                  
014220           MOVE SEQC-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                  
014221                                                                          
014222           PERFORM IMS-GU-WDH111                                          
014223           IF SEGMENT-FINNS                                               
014224             MOVE INV-KDINVKAT     TO WS-KDINVKAT                         
014225             PERFORM IMS-GNP-WDH121                                       
014226             MOVE  +1   TO WS-ANTAL-POST                                  
014227             MOVE  ZERO TO WS-KDSEGKEY                                    
014228             PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4            
014229               IF INVL-KDSEGKEY = '0'                                     
014230                 MOVE INVL-IDUSER TO WS-IDUSER-CRE                        
014231               END-IF                                                     
014232               IF INVL-KDSEGKEY = '1'                                     
014233                 MOVE INVL-IDUSER TO WS-IDUSER-PR1                        
014234               END-IF                                                     
014235               IF INVL-KDSEGKEY = '2'                                     
014236                 MOVE INVL-IDUSER TO WS-IDUSER-PR2                        
014237               END-IF                                                     
014238               IF INVL-KDSEGKEY = '3'                                     
014239                 MOVE INVL-IDUSER      TO WS-IDUSER-PR3                   
014240                 MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                     
014241                 ADD +4 TO WS-ANTAL-POST                                  
014242               END-IF                                                     
014243               IF WS-KDSEGKEY < 3                                         
014244                 PERFORM IMS-GNP-WDH121                                   
014245               END-IF                                                     
014246               ADD +1 TO WS-ANTAL-POST                                    
014247             END-PERFORM                                                  
014248             PERFORM CA-FYLL-UTAREA                                       
014249           END-IF                                                         
014250         END-IF                                                           
014251       ELSE                                                               
014252         IF LAES-USERF                                                    
014253           MOVE IN-IDDC                    TO W-SEQF-IDDC-MIN             
014254                                              W-SEQF-IDDC-MAX             
014255                                              W-IDDC-UNIK                 
014256           MOVE IN-IDUSER                  TO W-SEQF-IDUSER-MIN           
014257                                              W-SEQF-IDUSER-MAX           
014258           IF IN-KDINVKAT > 0                                             
014259             MOVE IN-KDINVKAT              TO W-SEQF-KDINVKAT-MIN         
014260                                              W-SEQF-KDINVKAT-MAX         
014261           END-IF                                                         
014262                                                                          
014263           IF IN-DATUM > 0                                                
014264             MOVE IN-DATUM                 TO WS-DATUM-AAMMDD             
014265             MOVE 20                       TO WS-DATUM-SS                 
014266             MOVE WS-DATUM-SSAAMMDD        TO W-DAREGDAT-SORT             
014269             MOVE  W-DAREGDAT-SORT     TO                                 
014270                   W-SEQF-DAREGDAT-SORT-MIN                               
014271           END-IF                                                         
014272           PERFORM IMS-GN-WDH1F1                                          
014273           IF SEGMENT-FINNS                                               
014274             MOVE SEQF-IDARTNR         TO W-IDARTNR                       
014275                                                                          
014276             MOVE SEQF-KDINVKAT        TO W-KDINVKAT-UNIK                 
014277                                          WS-KDINVKAT                     
014278             MOVE SEQF-TISEGKEY        TO W-TISEGKEY-UNIK                 
014279             MOVE SEQF-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK              
014280             PERFORM IMS-GU-WDH111                                        
014281             MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                 
014282             MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                 
014283             MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                 
014284             MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                 
014287             IF SEGMENT-FINNS                                             
014288               MOVE INV-KDINVKAT       TO WS-KDINVKAT                     
014289               PERFORM IMS-GNP-WDH121                                     
014290               MOVE  +1   TO WS-ANTAL-POST                                
014291               MOVE  ZERO TO WS-KDSEGKEY                                  
014292               PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4          
014293                 IF INVL-KDSEGKEY = '0'                                   
014294                   MOVE INVL-IDUSER TO WS-IDUSER-CRE                      
014295                 END-IF                                                   
014296                 IF INVL-KDSEGKEY = '1'                                   
014297                   MOVE INVL-IDUSER TO WS-IDUSER-PR1                      
014298                 END-IF                                                   
014299                 IF INVL-KDSEGKEY = '2'                                   
014300                   MOVE INVL-IDUSER TO WS-IDUSER-PR2                      
014301                 END-IF                                                   
014302                 IF INVL-KDSEGKEY = '3'                                   
014303                   MOVE INVL-IDUSER      TO WS-IDUSER-PR3                 
014304                   MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                   
014305                   ADD +4 TO WS-ANTAL-POST                                
014306                 END-IF                                                   
014307                 IF WS-KDSEGKEY < 3                                       
014308                   PERFORM IMS-GNP-WDH121                                 
014309                 END-IF                                                   
014310                 ADD +1 TO WS-ANTAL-POST                                  
014311               END-PERFORM                                                
014312               PERFORM CA-FYLL-UTAREA                                     
014313             END-IF                                                       
014314           END-IF                                                         
014315         ELSE                                                             
014316           IF LAES-USERD                                                  
014317           MOVE IN-IDDC                    TO W-SEQD-IDDC-MIN             
014318                                              W-SEQD-IDDC-MAX             
014319                                              W-IDDC-UNIK                 
014320           MOVE IN-IDUSER                  TO W-SEQD-IDUSER-MIN           
014321                                              W-SEQD-IDUSER-MAX           
014326                                                                          
014327           IF IN-DATUM > 0                                                
014328             MOVE IN-DATUM                 TO WS-DATUM-AAMMDD             
014329             MOVE 20                       TO WS-DATUM-SS                 
014330             MOVE WS-DATUM-SSAAMMDD        TO W-DAREGDAT-SORT             
014333             MOVE  W-DAREGDAT-SORT     TO                                 
014334                   W-SEQD-DAREGDAT-SORT-MIN                               
014335           END-IF                                                         
014336           PERFORM IMS-GN-WDH1D1                                          
014337             IF SEGMENT-FINNS                                             
014338               MOVE SEQD-IDARTNR         TO W-IDARTNR                     
014339                                                                          
014340               MOVE SEQD-KDINVKAT        TO W-KDINVKAT-UNIK               
014341                                            WS-KDINVKAT                   
014342               MOVE SEQD-TISEGKEY        TO W-TISEGKEY-UNIK               
014343               MOVE SEQD-DAREGDAT-SORT TO   W-DAREGDAT-SORT-UNIK          
014344               PERFORM IMS-GU-WDH111                                      
014348               IF SEGMENT-FINNS                                           
014349                 MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE             
014350                 MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1             
014351                 MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2             
014352                 MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3             
014353                 MOVE INV-KDINVKAT         TO WS-KDINVKAT                 
014354                 PERFORM IMS-GNP-WDH121                                   
014355                 MOVE  +1   TO WS-ANTAL-POST                              
014356                 MOVE  ZERO TO WS-KDSEGKEY                                
014357                 PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4        
014358                   IF INVL-KDSEGKEY = '0'                                 
014359                     MOVE INVL-IDUSER TO WS-IDUSER-CRE                    
014360                   END-IF                                                 
014361                   IF INVL-KDSEGKEY = '1'                                 
014362                     MOVE INVL-IDUSER TO WS-IDUSER-PR1                    
014363                   END-IF                                                 
014364                   IF INVL-KDSEGKEY = '2'                                 
014365                     MOVE INVL-IDUSER TO WS-IDUSER-PR2                    
014366                   END-IF                                                 
014367                   IF INVL-KDSEGKEY = '3'                                 
014368                     MOVE INVL-IDUSER      TO WS-IDUSER-PR3               
014369                     MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                 
014370                     ADD +4 TO WS-ANTAL-POST                              
014371                   END-IF                                                 
014372                   IF WS-KDSEGKEY < 3                                     
014373                     PERFORM IMS-GNP-WDH121                               
014374                   END-IF                                                 
014375                   ADD +1 TO WS-ANTAL-POST                                
014376                 END-PERFORM                                              
014377                 PERFORM CA-FYLL-UTAREA                                   
014378               END-IF                                                     
014379             END-IF                                                       
014380           ELSE                                                           
014381             IF LAES-DATUM                                                
014382               MOVE IN-IDDC                  TO W-SEQE-IDDC-MIN           
014383                                                W-SEQE-IDDC-MAX           
014384                                                W-IDDC-UNIK               
014385                                                                          
014386               MOVE IN-DATUM                 TO WS-DATUM-AAMMDD           
014387               MOVE 20                       TO WS-DATUM-SS               
014388               MOVE WS-DATUM-SSAAMMDD        TO W-DAREGDAT-SORT           
014391               MOVE  W-DAREGDAT-SORT     TO                               
014392                     W-SEQE-DAREGDAT-SORT-MIN                             
014393                                                                          
014394               PERFORM IMS-GN-WDH1E1                                      
014395               IF SEGMENT-FINNS                                           
014396                 MOVE SEQE-IDARTNR         TO W-IDARTNR                   
014397                                                                          
014398                 MOVE SEQE-KDINVKAT        TO W-KDINVKAT-UNIK             
014399                                           WS-KDINVKAT                    
014400                 MOVE SEQE-TISEGKEY        TO W-TISEGKEY-UNIK             
014401                 MOVE SEQE-DAREGDAT-SORT TO                               
014402                                  W-DAREGDAT-SORT-UNIK                    
014403                 PERFORM IMS-GU-WDH111                                    
014404                 IF SEGMENT-FINNS                                         
014405                   MOVE INV-DAREGDAT-CRE   TO WS-DAREGDAT-CRE             
014406                   MOVE INV-DAREGDAT-PR1   TO WS-DAREGDAT-PR1             
014407                   MOVE INV-DAREGDAT-PR2   TO WS-DAREGDAT-PR2             
014408                   MOVE INV-DAREGDAT-PR3   TO WS-DAREGDAT-PR3             
014409                   PERFORM IMS-GNP-WDH121                                 
014410                   MOVE  +1   TO WS-ANTAL-POST                            
014411                   MOVE  ZERO TO WS-KDSEGKEY                              
014412                   PERFORM UNTIL SEGMENT-SAKNAS OR                        
014413                                     WS-ANTAL-POST > 4                    
014414                   IF INVL-KDSEGKEY = '0'                                 
014415                     MOVE INVL-IDUSER TO WS-IDUSER-CRE                    
014416                   END-IF                                                 
014417                   IF INVL-KDSEGKEY = '1'                                 
014418                     MOVE INVL-IDUSER TO WS-IDUSER-PR1                    
014419                   END-IF                                                 
014420                   IF INVL-KDSEGKEY = '2'                                 
014421                     MOVE INVL-IDUSER TO WS-IDUSER-PR2                    
014422                   END-IF                                                 
014423                   IF INVL-KDSEGKEY = '3'                                 
014424                     MOVE INVL-IDUSER      TO WS-IDUSER-PR3               
014425                     MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                 
014426                     ADD +4 TO WS-ANTAL-POST                              
014427                   END-IF                                                 
014428                   IF WS-KDSEGKEY < 3                                     
014429                     PERFORM IMS-GNP-WDH121                               
014430                   END-IF                                                 
014431                   ADD +1 TO WS-ANTAL-POST                                
014432                   END-PERFORM                                            
014433                   PERFORM CA-FYLL-UTAREA                                 
014439                 END-IF                                                   
014440               END-IF                                                     
014441             END-IF                                                       
014442           END-IF                                                         
014443         END-IF                                                           
014444       END-IF                                                             
014445     END-IF                                                               
014446                                                                          
014447     .                                                                    
014448     EJECT                                                                
014449 CA-FYLL-UTAREA SECTION.                                                  
014450     WRITE W51352-001-RAD FROM MAIL-RUB1                                  
014451***  WRITE W51352-001-RAD FROM MAIL-TOM                                   
014452     WRITE W51352-001-RAD FROM MAIL-RUB2                                  
014453     WRITE W51352-001-RAD FROM MAIL-RUB3                                  
014454                                                                          
014455                                                                          
014456     MOVE +1 TO INDX                                                      
014457     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 100                           
014458       MOVE W-IDARTNR            TO CR1-IDARTNR                           
014459       MOVE WS-KDINVKAT          TO CR1-TYPE                              
014460       MOVE WS-DAREGDAT-CRE      TO WS-DATUM                              
014461       MOVE WS-DATUM4-9          TO CR1-DATE                              
014462       MOVE WS-IDUSER-CRE        TO CR1-IDUSER-CRE                        
014463       WRITE W51352-001-RAD FROM MAIL-RAD                                 
014464                                                                          
014465       MOVE WS-KDINVKAT          TO RAD1-TYPE                             
014466       MOVE WS-DAREGDAT-PR1      TO WS-DATUM                              
014467       MOVE WS-DATUM4-9          TO RAD1-DATE                             
014468       MOVE WS-IDUSER-PR1        TO RAD1-USER                             
014469       MOVE WS-DAREGDAT-PR1(3:6) TO WS-DATUM-TOM                          
014470       MOVE WS-DAREGDAT-CRE(3:6) TO WS-DATUM-FOM                          
014471                                                                          
014472       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014473         PERFORM S02-DATUM                                                
014474         MOVE WS-ANTALDAGAR      TO RAD1-WS-DATE                          
014475                                    RAD1-WS-DATETOT                       
014476       ELSE                                                               
014477         MOVE ZERO               TO RAD1-WS-DATE                          
014478                                    RAD1-WS-DATETOT                       
014479       END-IF                                                             
014480       WRITE W51352-001-RAD      FROM  MAIL-RAD1                          
014481** RAD 2 ***                                                              
014482       MOVE WS-KDINVKAT          TO RAD2-TYPE                             
014483       MOVE WS-DAREGDAT-PR2      TO WS-DATUM                              
014484       MOVE WS-DATUM4-9          TO RAD2-DATE                             
014485       MOVE WS-IDUSER-PR2        TO RAD2-USER                             
014486                                                                          
014487       MOVE WS-DAREGDAT-PR2(3:6) TO WS-DATUM-TOM                          
014488       MOVE WS-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                          
014489                                                                          
014490       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014491         PERFORM S02-DATUM                                                
014492         MOVE WS-ANTALDAGAR      TO RAD2-WS-DATE                          
014493       ELSE                                                               
014494         MOVE ZERO               TO RAD2-WS-DATE                          
014495       END-IF                                                             
014496                                                                          
014497       MOVE WS-DAREGDAT-PR2(3:6) TO WS-DATUM-TOM                          
014498       MOVE WS-DAREGDAT-CRE(3:6) TO WS-DATUM-FOM                          
014499                                                                          
014500       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014501         PERFORM S02-DATUM                                                
014502         MOVE WS-ANTALDAGAR      TO RAD2-WS-DATETOT                       
014503       ELSE                                                               
014504         MOVE ZERO               TO RAD2-WS-DATETOT                       
014505       END-IF                                                             
014506       WRITE W51352-001-RAD      FROM  MAIL-RAD2                          
014507** RAD 3                                                                  
014508       MOVE WS-KDINVKAT          TO RAD3-TYPE                             
014509       MOVE WS-DAREGDAT-PR3      TO WS-DATUM                              
014510       MOVE WS-DATUM4-9          TO RAD3-DATE                             
014511       MOVE WS-IDUSER-PR3        TO RAD3-USER                             
014512                                                                          
014513       MOVE WS-DAREGDAT-PR3(3:6) TO WS-DATUM-TOM                          
014514       MOVE WS-DAREGDAT-PR2(3:6) TO WS-DATUM-FOM                          
014515                                                                          
014516       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014517         PERFORM S02-DATUM                                                
014518         MOVE WS-ANTALDAGAR      TO RAD3-WS-DATE                          
014519       ELSE                                                               
014520         MOVE ZERO               TO RAD3-WS-DATE                          
014521       END-IF                                                             
014522                                                                          
014523       MOVE WS-DAREGDAT-PR3(3:6) TO WS-DATUM-TOM                          
014524       MOVE WS-DAREGDAT-CRE(3:6) TO WS-DATUM-FOM                          
014525                                                                          
014526       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014527         PERFORM S02-DATUM                                                
014528         MOVE WS-ANTALDAGAR      TO RAD3-WS-DATETOT                       
014529       ELSE                                                               
014530         MOVE ZERO               TO RAD3-WS-DATETOT                       
014531       END-IF                                                             
014532       WRITE W51352-001-RAD      FROM  MAIL-RAD3                          
014533***    WRITE W51352-001-RAD      FROM  MAIL-TOM                           
014534       ADD  +1 TO INDX                                                    
014535       MOVE INDX TO WS-TAL                                                
014537       PERFORM CB-LAES-NY-RAD                                             
014539****   END-IF                                                             
014540     END-PERFORM                                                          
014541     .                                                                    
014542     EJECT                                                                
014543 CB-LAES-NY-RAD SECTION.                                                  
014545     IF LAES-ARTIKEL                                                      
014546       PERFORM IMS-GNP-WDH111                                             
014547         IF SEGMENT-FINNS                                                 
014548           MOVE INV-DAREGDAT-CRE  TO WS-DAREGDAT-CRE                      
014549           MOVE INV-DAREGDAT-PR1  TO WS-DAREGDAT-PR1                      
014550           MOVE INV-DAREGDAT-PR2  TO WS-DAREGDAT-PR2                      
014551           MOVE INV-DAREGDAT-PR3  TO WS-DAREGDAT-PR3                      
014552           MOVE INV-KDINVKAT      TO WS-KDINVKAT                          
014553           PERFORM IMS-GNP-WDH121                                         
014554           MOVE  +1   TO WS-ANTAL-POST                                    
014555           MOVE  ZERO TO WS-KDSEGKEY                                      
014556           IF SEGMENT-FINNS                                               
014557             PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4            
014558               IF INVL-KDSEGKEY = '0'                                     
014559                 MOVE INVL-IDUSER TO WS-IDUSER-CRE                        
014560               END-IF                                                     
014561               IF INVL-KDSEGKEY = '1'                                     
014562                 MOVE INVL-IDUSER TO WS-IDUSER-PR1                        
014563               END-IF                                                     
014564               IF INVL-KDSEGKEY = '2'                                     
014565                 MOVE INVL-IDUSER TO WS-IDUSER-PR2                        
014566               END-IF                                                     
014567               IF INVL-KDSEGKEY = '3'                                     
014568                 MOVE INVL-IDUSER TO WS-IDUSER-PR3                        
014569                 MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                     
014570                 ADD +4 TO WS-ANTAL-POST                                  
014571               END-IF                                                     
014572               IF WS-KDSEGKEY < 3                                         
014573                 PERFORM IMS-GNP-WDH121                                   
014574               END-IF                                                     
014575               ADD +1 TO WS-ANTAL-POST                                    
014576             END-PERFORM                                                  
014577           END-IF                                                         
014578         END-IF                                                           
014579     ELSE                                                                 
014580       IF LAES-KDINVKAT                                                   
014581         PERFORM IMS-GN-WDH1C1                                            
014582         IF SEGMENT-FINNS                                                 
014583           MOVE SEQC-IDARTNR          TO W-IDARTNR                        
014584                                                                          
014585           MOVE SEQC-TISEGKEY         TO W-TISEGKEY-UNIK                  
014586                                                                          
014587           MOVE SEQC-DAREGDAT-SORT    TO W-DAREGDAT-SORT-UNIK             
014588                                                                          
014589           MOVE SEQC-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                  
014590           MOVE SEQC-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                  
014591           MOVE SEQC-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                  
014592           MOVE SEQC-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                  
014593                                                                          
014594           PERFORM IMS-GU-WDH111                                          
014595           MOVE INV-KDINVKAT TO WS-KDINVKAT                               
014596           IF SEGMENT-FINNS                                               
014597             PERFORM IMS-GNP-WDH121                                       
014598             MOVE  +1   TO WS-ANTAL-POST                                  
014599             MOVE  ZERO TO WS-KDSEGKEY                                    
014600             PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4            
014601               IF INVL-KDSEGKEY = '0'                                     
014602                 MOVE INVL-IDUSER TO WS-IDUSER-CRE                        
014603               END-IF                                                     
014604               IF INVL-KDSEGKEY = '1'                                     
014605                 MOVE INVL-IDUSER TO WS-IDUSER-PR1                        
014606               END-IF                                                     
014607               IF INVL-KDSEGKEY = '2'                                     
014608                 MOVE INVL-IDUSER TO WS-IDUSER-PR2                        
014609               END-IF                                                     
014610               IF INVL-KDSEGKEY = '3'                                     
014611                 MOVE INVL-IDUSER      TO WS-IDUSER-PR3                   
014612                 MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                     
014613                 ADD +4 TO WS-ANTAL-POST                                  
014614               END-IF                                                     
014615               IF WS-KDSEGKEY < 3                                         
014616                 PERFORM IMS-GNP-WDH121                                   
014617               END-IF                                                     
014618               ADD +1 TO WS-ANTAL-POST                                    
014619             END-PERFORM                                                  
014620           END-IF                                                         
014621         END-IF                                                           
014622       ELSE                                                               
014623         IF LAES-USERF                                                    
014624           PERFORM UNTIL SEGMENT-SAKNAS OR                                
014625                         W-IDARTNR NOT = SEQF-IDARTNR                     
014626             PERFORM IMS-GN-WDH1F1                                        
014627           END-PERFORM                                                    
014628                                                                          
014629***        PERFORM IMS-GN-WDH1F1                                          
014630           IF SEGMENT-FINNS                                               
014631             MOVE SEQF-IDARTNR         TO W-IDARTNR                       
014632                                                                          
014633             MOVE SEQF-KDINVKAT        TO W-KDINVKAT-UNIK                 
014634                                          WS-KDINVKAT                     
014635             MOVE SEQF-TISEGKEY        TO W-TISEGKEY-UNIK                 
014636             MOVE SEQF-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK              
014637             PERFORM IMS-GU-WDH111                                        
014638             MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                 
014639             MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                 
014640             MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                 
014641             MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                 
014642             IF SEGMENT-FINNS                                             
014643               MOVE INV-KDINVKAT       TO WS-KDINVKAT                     
014644               PERFORM IMS-GNP-WDH121                                     
014645               MOVE  +1   TO WS-ANTAL-POST                                
014646               MOVE  ZERO TO WS-KDSEGKEY                                  
014647               PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4          
014648                 IF INVL-KDSEGKEY = '0'                                   
014649                   MOVE INVL-IDUSER TO WS-IDUSER-CRE                      
014650                 END-IF                                                   
014651                 IF INVL-KDSEGKEY = '1'                                   
014652                   MOVE INVL-IDUSER TO WS-IDUSER-PR1                      
014653                 END-IF                                                   
014654                 IF INVL-KDSEGKEY = '2'                                   
014655                   MOVE INVL-IDUSER TO WS-IDUSER-PR2                      
014656                 END-IF                                                   
014657                 IF INVL-KDSEGKEY = '3'                                   
014658                   MOVE INVL-IDUSER      TO WS-IDUSER-PR3                 
014659                   MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                   
014660                   ADD +4 TO WS-ANTAL-POST                                
014661                 END-IF                                                   
014662                 IF WS-KDSEGKEY < 3                                       
014663                   PERFORM IMS-GNP-WDH121                                 
014664                 END-IF                                                   
014665                 ADD +1 TO WS-ANTAL-POST                                  
014666               END-PERFORM                                                
014667             END-IF                                                       
014668           END-IF                                                         
014669         ELSE                                                             
014670           IF LAES-USERD                                                  
014671           PERFORM UNTIL SEGMENT-SAKNAS OR                                
014672                         W-IDARTNR NOT = SEQD-IDARTNR                     
014673             PERFORM IMS-GN-WDH1D1                                        
014674           END-PERFORM                                                    
014675                                                                          
014676***        PERFORM IMS-GN-WDH1D1                                          
014677           IF SEGMENT-FINNS                                               
014678             MOVE SEQD-IDARTNR         TO W-IDARTNR                       
014679                                                                          
014680             MOVE SEQD-KDINVKAT        TO W-KDINVKAT-UNIK                 
014681                                          WS-KDINVKAT                     
014682             MOVE SEQD-TISEGKEY        TO W-TISEGKEY-UNIK                 
014683             MOVE SEQD-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK              
014684             PERFORM IMS-GU-WDH111                                        
014685             MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                 
014686             MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                 
014687             MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                 
014688             MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                 
014689             IF SEGMENT-FINNS                                             
014690               MOVE INV-KDINVKAT       TO WS-KDINVKAT                     
014691               PERFORM IMS-GNP-WDH121                                     
014692               MOVE  +1   TO WS-ANTAL-POST                                
014693               MOVE  ZERO TO WS-KDSEGKEY                                  
014694               PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4          
014695                 IF INVL-KDSEGKEY = '0'                                   
014696                   MOVE INVL-IDUSER TO WS-IDUSER-CRE                      
014697                 END-IF                                                   
014698                 IF INVL-KDSEGKEY = '1'                                   
014699                   MOVE INVL-IDUSER TO WS-IDUSER-PR1                      
014700                 END-IF                                                   
014701                 IF INVL-KDSEGKEY = '2'                                   
014702                   MOVE INVL-IDUSER TO WS-IDUSER-PR2                      
014703                 END-IF                                                   
014704                 IF INVL-KDSEGKEY = '3'                                   
014705                   MOVE INVL-IDUSER      TO WS-IDUSER-PR3                 
014706                   MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                   
014707                   ADD +4 TO WS-ANTAL-POST                                
014708                 END-IF                                                   
014709                 IF WS-KDSEGKEY < 3                                       
014710                   PERFORM IMS-GNP-WDH121                                 
014711                 END-IF                                                   
014712                 ADD +1 TO WS-ANTAL-POST                                  
014713               END-PERFORM                                                
014714             END-IF                                                       
014715           END-IF                                                         
014716           ELSE                                                           
014717             IF LAES-DATUM                                                
014718               PERFORM IMS-GN-WDH1E1                                      
014719               IF SEGMENT-FINNS                                           
014720                 MOVE SEQE-IDARTNR         TO W-IDARTNR                   
014721                                                                          
014722                 MOVE SEQE-KDINVKAT        TO W-KDINVKAT-UNIK             
014723                                           WS-KDINVKAT                    
014724                 MOVE SEQE-TISEGKEY        TO W-TISEGKEY-UNIK             
014725                 MOVE SEQE-DAREGDAT-SORT TO                               
014726                                  W-DAREGDAT-SORT-UNIK                    
014727                 PERFORM IMS-GU-WDH111                                    
014728                 IF SEGMENT-FINNS                                         
014729                   MOVE INV-DAREGDAT-CRE   TO WS-DAREGDAT-CRE             
014730                   MOVE INV-DAREGDAT-PR1   TO WS-DAREGDAT-PR1             
014731                   MOVE INV-DAREGDAT-PR2   TO WS-DAREGDAT-PR2             
014732                   MOVE INV-DAREGDAT-PR3   TO WS-DAREGDAT-PR3             
014733                   PERFORM IMS-GNP-WDH121                                 
014734                   MOVE  +1   TO WS-ANTAL-POST                            
014735                   MOVE  ZERO TO WS-KDSEGKEY                              
014736                   PERFORM UNTIL SEGMENT-SAKNAS OR                        
014737                                     WS-ANTAL-POST > 4                    
014738                   IF INVL-KDSEGKEY = '0'                                 
014739                     MOVE INVL-IDUSER TO WS-IDUSER-CRE                    
014740                   END-IF                                                 
014741                   IF INVL-KDSEGKEY = '1'                                 
014742                     MOVE INVL-IDUSER TO WS-IDUSER-PR1                    
014743                   END-IF                                                 
014744                   IF INVL-KDSEGKEY = '2'                                 
014745                     MOVE INVL-IDUSER TO WS-IDUSER-PR2                    
014746                   END-IF                                                 
014747                   IF INVL-KDSEGKEY = '3'                                 
014748                     MOVE INVL-IDUSER      TO WS-IDUSER-PR3               
014749                     MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                 
014750                     ADD +4 TO WS-ANTAL-POST                              
014751                   END-IF                                                 
014752                   IF WS-KDSEGKEY < 3                                     
014753                     PERFORM IMS-GNP-WDH121                               
014754                   END-IF                                                 
014755                   ADD +1 TO WS-ANTAL-POST                                
014756                   END-PERFORM                                            
014757                 END-IF                                                   
014758               END-IF                                                     
014759             END-IF                                                       
014760           END-IF                                                         
014761         END-IF                                                           
014762       END-IF                                                             
014763     END-IF                                                               
014764     .                                                                    
014765     EJECT                                                                
014766 D-LAES-WDH7 SECTION.                                                     
014767     MOVE IN-IDARTNR TO W-IDARTNR                                         
014768     MOVE IN-IDARTNR TO H7-IDARTNR                                        
014769     MOVE IN-IDDC    TO W-IDDC                                            
014771     IF IN-KDINVKAT > ZERO                                                
014772       MOVE IN-KDINVKAT TO W-KDJUSTYP                                     
014774     ELSE                                                                 
014775       MOVE ZERO TO W-KDJUSTYP                                            
014776     END-IF                                                               
014778                                                                          
014779     PERFORM IMS-GET-WDH701                                               
014780                                                                          
014781     IF SEGMENT-FINNS                                                     
014782       PERFORM IMS-GET-WDH711                                             
014783       IF W-KDJUSTYP > ZERO                                               
014784        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
014785                      INVH-KDJUSTYP = W-KDJUSTYP                          
014786          PERFORM IMS-GET-WDH711                                          
014787        END-PERFORM                                                       
014788       END-IF                                                             
014789       IF SEGMENT-FINNS                                                   
014790         MOVE INVH-KDJUSTYP             TO WS-KDJUSTYP                    
014791         MOVE INVH-DAREGDAT-CRE         TO WS-DAREGDAT-CRE                
014792         MOVE INVH-DAREGDAT-PR1         TO WS-DAREGDAT-PR1                
014793         MOVE INVH-DAREGDAT-PR2         TO WS-DAREGDAT-PR2                
014794         MOVE INVH-DAREGDAT-PR3         TO WS-DAREGDAT-PR3                
014795         MOVE INVH-DAREGDAT-CLO         TO WS-DAREGDAT-CLO                
014796         MOVE INVH-IDUSER-CRE           TO WS-IDUSER-CRE                  
014797         MOVE INVH-IDUSER-PR1           TO WS-IDUSER-PR1                  
014798         MOVE INVH-IDUSER-PR2           TO WS-IDUSER-PR2                  
014799         MOVE INVH-IDUSER-PR3           TO WS-IDUSER-PR3                  
014800         MOVE INVH-IDUSER-CLO           TO WS-IDUSER-CLO                  
014802         PERFORM DA-FYLL-UTAREA                                           
014803       END-IF                                                             
014804     END-IF                                                               
014805                                                                          
014806     .                                                                    
014807 DA-FYLL-UTAREA SECTION.                                                  
014808     WRITE W51352-001-RAD FROM H7-MAIL-RUB1                               
014809***  WRITE W51352-001-RAD FROM MAIL-TOM                                   
014810     WRITE W51352-001-RAD FROM H7-MAIL-RUB2                               
014811     WRITE W51352-001-RAD FROM H7-MAIL-RUB3                               
014812     MOVE +1 TO INDX                                                      
014813     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 500                           
014814       MOVE WS-KDJUSTYP             TO H7-CRE-TYPE                        
014815       MOVE WS-DAREGDAT-CRE         TO H7-CRE-DATE                        
014816       MOVE WS-IDUSER-CRE           TO H7-CRE-IDUSER                      
014817       WRITE W51352-001-RAD FROM H7-CREATED-RAD1                          
014818       ADD +1 TO INDX                                                     
014819                                                                          
014820       MOVE WS-KDJUSTYP             TO H7-RAD1-TYPE                       
014821       MOVE WS-DAREGDAT-PR1         TO H7-RAD1-DATE                       
014822       MOVE WS-IDUSER-PR1           TO H7-RAD1-USER                       
014823       MOVE WS-DAREGDAT-CRE(3:6)    TO WS-DATUM-FOM                       
014824       MOVE WS-DAREGDAT-PR1(3:6)    TO WS-DATUM-TOM                       
014825                                                                          
014826       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014827         PERFORM S02-DATUM                                                
014828         MOVE WS-ANTALDAGAR         TO H7-RAD1-WS-DATE                    
014829                                       H7-RAD1-WS-DATETOT                 
014830       ELSE                                                               
014831         MOVE ZERO                  TO H7-RAD1-WS-DATE                    
014832                                       H7-RAD1-WS-DATETOT                 
014833       END-IF                                                             
014834       WRITE W51352-001-RAD FROM H7-MAIL-RAD1                             
014835       ADD +1 TO INDX                                                     
014836                                                                          
014837       MOVE WS-KDJUSTYP             TO H7-RAD2-TYPE                       
014838       MOVE WS-DAREGDAT-PR2         TO H7-RAD2-DATE                       
014839       MOVE WS-IDUSER-PR2           TO H7-RAD2-USER                       
014840       MOVE WS-DAREGDAT-PR1(3:6)    TO WS-DATUM-FOM                       
014841       MOVE WS-DAREGDAT-PR2(3:6)    TO WS-DATUM-TOM                       
014842       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014843         PERFORM S02-DATUM                                                
014844         MOVE WS-ANTALDAGAR         TO H7-RAD2-WS-DATE                    
014845       ELSE                                                               
014846         MOVE ZERO                  TO H7-RAD2-WS-DATE                    
014847       END-IF                                                             
014848       MOVE WS-DAREGDAT-CRE(3:6)    TO WS-DATUM-FOM                       
014849       MOVE WS-DAREGDAT-PR2(3:6)    TO WS-DATUM-TOM                       
014850       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014851         PERFORM S02-DATUM                                                
014852         MOVE WS-ANTALDAGAR         TO H7-RAD2-WS-DATETOT                 
014853       ELSE                                                               
014854         MOVE ZERO                  TO H7-RAD2-WS-DATETOT                 
014855       END-IF                                                             
014856       WRITE W51352-001-RAD FROM H7-MAIL-RAD2                             
014857                                                                          
014858       ADD +1 TO INDX                                                     
014859                                                                          
014860       MOVE WS-KDJUSTYP             TO H7-RAD3-TYPE                       
014861       MOVE WS-DAREGDAT-PR3         TO H7-RAD3-DATE                       
014862       MOVE WS-IDUSER-PR3           TO H7-RAD3-USER                       
014863       MOVE WS-DAREGDAT-PR2(3:6)    TO WS-DATUM-FOM                       
014864       MOVE WS-DAREGDAT-PR3(3:6)    TO WS-DATUM-TOM                       
014865       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014866         PERFORM S02-DATUM                                                
014867         MOVE WS-ANTALDAGAR         TO H7-RAD3-WS-DATE                    
014868       ELSE                                                               
014869         MOVE ZERO                  TO H7-RAD3-WS-DATE                    
014870       END-IF                                                             
014871       MOVE WS-DAREGDAT-CRE(3:6)    TO WS-DATUM-FOM                       
014872       MOVE WS-DAREGDAT-PR3(3:6)    TO WS-DATUM-TOM                       
014873       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014874         PERFORM S02-DATUM                                                
014875         MOVE WS-ANTALDAGAR         TO H7-RAD3-WS-DATETOT                 
014876       ELSE                                                               
014877         MOVE ZERO                  TO H7-RAD3-WS-DATETOT                 
014878       END-IF                                                             
014879       WRITE W51352-001-RAD FROM H7-MAIL-RAD3                             
014880                                                                          
014881       ADD +1 TO INDX                                                     
014882                                                                          
014883       MOVE WS-KDJUSTYP             TO H7-RAD4-TYPE                       
014884       MOVE WS-DAREGDAT-CLO         TO H7-RAD4-DATE                       
014885       MOVE WS-IDUSER-CLO           TO H7-RAD4-USER                       
014886       MOVE WS-DAREGDAT-PR3(3:6)    TO WS-DATUM-FOM                       
014887       MOVE WS-DAREGDAT-CLO(3:6)    TO WS-DATUM-TOM                       
014888       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014889         PERFORM S02-DATUM                                                
014890         MOVE WS-ANTALDAGAR         TO H7-RAD4-WS-DATE                    
014891       ELSE                                                               
014892         MOVE ZERO                  TO H7-RAD4-WS-DATE                    
014893       END-IF                                                             
014894       MOVE WS-DAREGDAT-CRE(3:6)    TO WS-DATUM-FOM                       
014895       MOVE WS-DAREGDAT-CLO(3:6)    TO WS-DATUM-TOM                       
014896       IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                           
014897         PERFORM S02-DATUM                                                
014898         MOVE WS-ANTALDAGAR         TO H7-RAD4-WS-DATETOT                 
014899       ELSE                                                               
014900         MOVE ZERO                  TO H7-RAD4-WS-DATETOT                 
014901       END-IF                                                             
014902       WRITE W51352-001-RAD FROM H7-MAIL-RAD4                             
014903***    WRITE W51352-001-RAD FROM MAIL-TOM                                 
014904     PERFORM DB-LAES-RAD                                                  
014906     END-PERFORM                                                          
014907     EJECT                                                                
014908     .                                                                    
014909 DB-LAES-RAD SECTION.                                                     
014910     PERFORM IMS-GET-WDH711                                               
014911     IF SEGMENT-FINNS                                                     
014912       IF W-KDJUSTYP > 0                                                  
014913         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
014914                       W-KDJUSTYP = INVH-KDJUSTYP                         
014915           PERFORM IMS-GET-WDH711                                         
014916         END-PERFORM                                                      
014917       END-IF                                                             
014918       IF SEGMENT-FINNS                                                   
014919         MOVE INVH-KDJUSTYP             TO WS-KDJUSTYP                    
014920         MOVE INVH-DAREGDAT-CRE         TO WS-DAREGDAT-CRE                
014921         MOVE INVH-DAREGDAT-PR1         TO WS-DAREGDAT-PR1                
014922         MOVE INVH-DAREGDAT-PR2         TO WS-DAREGDAT-PR2                
014923         MOVE INVH-DAREGDAT-PR3         TO WS-DAREGDAT-PR3                
014924         MOVE INVH-DAREGDAT-CLO         TO WS-DAREGDAT-CLO                
014925         MOVE INVH-IDUSER-CRE           TO WS-IDUSER-CRE                  
014926         MOVE INVH-IDUSER-PR1           TO WS-IDUSER-PR1                  
014927         MOVE INVH-IDUSER-PR2           TO WS-IDUSER-PR2                  
014928         MOVE INVH-IDUSER-PR3           TO WS-IDUSER-PR3                  
014929         MOVE INVH-IDUSER-CLO           TO WS-IDUSER-CLO                  
014931       END-IF                                                             
014932     END-IF                                                               
014933     .                                                                    
014934 Z-FINIT SECTION.                                                         
014935     CLOSE W513S1                                                         
014936           W51352-001                                                     
014937     SKIP2                                                                
014938     MOVE 'S' TO POSTSUM-OPKOD                                            
014939     CALL POSTSUM USING POSTSUM-PARM                                      
014940     .                                                                    
014941     EJECT                                                                
014942 S01-LAES-W513S1  SECTION.                                                
014943     READ W513S1 INTO IN-AREA                                             
014944     AT END                                                               
014945        MOVE HIGH-VALUE   TO IN-AREA                                      
014946        SET END-OF-W513S1 TO TRUE                                         
014947                                                                          
014948     NOT AT END                                                           
014949        MOVE 'W513S1'     TO POSTSUM-FDNAMN                               
014950        MOVE 'W51352D1'   TO POSTSUM-DDNAMN2                              
014951        MOVE 'PARM'       TO POSTSUM-TRANSTYP                             
014952        CALL POSTSUM USING POSTSUM-PARM                                   
014953     END-READ                                                             
014954     .                                                                    
014955     EJECT                                                                
014956 S02-DATUM SECTION.                                                       
014957     MOVE ZERO               TO WS-ANTALDAGAR                             
014958     MOVE WS-DATUM-FOM       TO DAG-TIAAMMDD-FOM                          
014959     MOVE WS-DATUM-TOM       TO DAG-TIAAMMDD-TOM                          
014960                                                                          
014961     MOVE 001               TO DAG-KDCALL                                 
014962     MOVE 20                TO DAG-TISEKEL-FOM                            
014963                               DAG-TISEKEL-TOM                            
014964                                                                          
014965     CALL WDAGKONV USING  DAG-KDCALL  DAG-DATUM-AREA                      
014966                          DAG-KDSVAR                                      
014967     IF DAG-KDSVAR = SPACE                                                
014968       MOVE DAG-KVKALDAG   TO WS-ANTALDAGAR                               
014969     END-IF                                                               
014970                                                                          
014971     .                                                                    
014972     EJECT                                                                
014973*                                                                         
014974*S11-SKRIV-W51337 SECTION.                                                
014975*                                                                         
014976*    WRITE UT-POST FROM UT-AREA                                           
014977*                                                                         
014978*    MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
014979*    MOVE 'W51337' TO POSTSUM-FDNAMN                                      
014980*    MOVE 'W51352D2' TO POSTSUM-DDNAMN2                                   
014981*    CALL POSTSUM USING POSTSUM-PARM                                      
014982*    .                                                                    
014990*    EJECT                                                                
015000 S99-ABEND SECTION.                                                       
015100                                                                          
015201     SKIP2                                                                
015202     MOVE 'S' TO POSTSUM-OPKOD                                            
015210     CALL POSTSUM USING POSTSUM-PARM                                      
015300     CALL ABEND USING RKOD-ABEND                                          
015400     .                                                                    
015500     EJECT                                                                
015600* --- IMS SEKTIONER ---                                                   
015700                                                                          
015801     EJECT                                                                
015802 IMS-GU-WDH101 SECTION.                                                   
015803                                                                          
015804     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
015805          DELIMITED BY SIZE INTO SSA1                                     
015806     MOVE '  GE' TO GODK-STATUSKODER                                      
015807     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH101 SSA1                    
015808     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
015809     PERFORM IMS-STATUSKONTROLL                                           
015810     .                                                                    
015811     EJECT                                                                
015812 IMS-GU-WDH111 SECTION.                                                   
015813**** MOVE 'IMS-GU-WDH111'    TO WS-IMS                                    
015815     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
015816          DELIMITED BY SIZE INTO SSA1                                     
015817     STRING 'WDH111  (WDH111KY =' W-WDH111KY-X ')'                        
015818          DELIMITED BY SIZE INTO SSA2                                     
015819     MOVE '  GE' TO GODK-STATUSKODER                                      
015820     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH111 SSA1 SSA2               
015821     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
015822     PERFORM IMS-STATUSKONTROLL                                           
015823     .                                                                    
015824     EJECT                                                                
015825 IMS-GNP-WDH111 SECTION.                                                  
015826                                                                          
015827***  STRING 'WDH111  (WDH111KY =' W-WDH111-KY-X ')'                       
015828                                                                          
015829     STRING 'WDH111  (WDH111KY>=' W-WDH111-KEY-MIN-X                      
015830                    '&WDH111KY<=' W-WDH111-KEY-MAX-X ')'                  
015831          DELIMITED BY SIZE INTO SSA1                                     
015832     MOVE '  GE' TO GODK-STATUSKODER                                      
015833     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH111 SSA1                   
015834     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
015835     PERFORM IMS-STATUSKONTROLL                                           
015836     .                                                                    
015837     EJECT                                                                
015838 IMS-GNP-WDH121 SECTION.                                                  
015840*    MOVE 'IMS-GNP-WDH121'   TO WS-IMS                                    
015841     MOVE 'WDH121 ' TO SSA1                                               
015842     MOVE '  GE' TO GODK-STATUSKODER                                      
015843     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH121 SSA1                   
015844     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
015845     PERFORM IMS-STATUSKONTROLL                                           
015846     .                                                                    
015847     EJECT                                                                
015848 IMS-GN-WDH1C1  SECTION.                                                  
015850     STRING 'WDH1C1  (WDH1C1KY>=' W-WDH1C1KY-MIN-X                        
015851                    '&WDH1C1KY<=' W-WDH1C1KY-MAX-X ')'                    
015852              DELIMITED BY SIZE INTO SSA1                                 
015853     MOVE '  GE' TO GODK-STATUSKODER                                      
015854     CALL CBLTDLI USING GN WDH1C-PCB DLI-IO-WDH1C1 SSA1                   
015855     MOVE WDH1C-STATUS-CODE TO STATUS-WS                                  
015856     PERFORM IMS-STATUSKONTROLL                                           
015857     .                                                                    
015858     SKIP2                                                                
015859 IMS-GN-WDH1F1  SECTION.                                                  
015860***  MOVE 'IMS-GN-WDH1F1'    TO WS-IMS                                    
015862     STRING 'WDH1F1  (WDH1F1KY>=' W-WDH1F1KY-MIN-X                        
015863                    '&WDH1F1KY<=' W-WDH1F1KY-MAX-X ')'                    
015864              DELIMITED BY SIZE INTO SSA1                                 
015865     MOVE '  GEGB' TO GODK-STATUSKODER                                    
015866     CALL CBLTDLI USING GN WDH1F-PCB DLI-IO-WDH1F1 SSA1                   
015867     MOVE WDH1F-STATUS-CODE TO STATUS-WS                                  
015868     PERFORM IMS-STATUSKONTROLL                                           
015869     .                                                                    
015870     SKIP2                                                                
015871 IMS-GN-WDH1D1  SECTION.                                                  
015873**   MOVE 'IMS-GN-WDH1D1'    TO WS-IMS                                    
015874     STRING 'WDH1D1  (WDH1D1KY>=' W-WDH1D1KY-MIN-X                        
015875                    '&WDH1D1KY<=' W-WDH1D1KY-MAX-X ')'                    
015876              DELIMITED BY SIZE INTO SSA1                                 
015877     MOVE '  GEGB' TO GODK-STATUSKODER                                    
015878     CALL CBLTDLI USING GN WDH1D-PCB DLI-IO-WDH1D1 SSA1                   
015879     MOVE WDH1D-STATUS-CODE TO STATUS-WS                                  
015880     PERFORM IMS-STATUSKONTROLL                                           
015881     .                                                                    
015882     SKIP2                                                                
015883 IMS-GN-WDH1E1  SECTION.                                                  
015885***  MOVE 'IMS-GN-WDH1E1'    TO WS-IMS                                    
015886     STRING 'WDH1E1  (WDH1E1KY>=' W-WDH1E1KY-MIN-X                        
015887                    '&WDH1E1KY<=' W-WDH1E1KY-MAX-X ')'                    
015888              DELIMITED BY SIZE INTO SSA1                                 
015889     MOVE '  GEGB' TO GODK-STATUSKODER                                    
015890     CALL CBLTDLI USING GN WDH1E-PCB DLI-IO-WDH1E1 SSA1                   
015891     MOVE WDH1E-STATUS-CODE TO STATUS-WS                                  
015892     PERFORM IMS-STATUSKONTROLL                                           
015893     .                                                                    
015894     SKIP2                                                                
015895 IMS-GET-WDH701 SECTION.                                                  
015896                                                                          
015897     STRING 'WDH701  (IDARTNR  =' W-IDARTNR-X ')'                         
015898          DELIMITED BY SIZE INTO SSA1                                     
015899     MOVE '  GE' TO GODK-STATUSKODER                                      
015900     CALL CBLTDLI USING GU WDH7-PCB DLI-IO-WDH701 SSA1                    
015901     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
015902     PERFORM IMS-STATUSKONTROLL                                           
015903     .                                                                    
015904     EJECT                                                                
015905 IMS-GET-WDH711 SECTION.                                                  
015906                                                                          
015907     STRING 'WDH711  (TISEGKEY<=' W-TISEGKEY-X                            
015908                    '&IDDC     =' W-IDDC-X ')'                            
015909            DELIMITED BY SIZE INTO SSA1                                   
015911                                                                          
015912     MOVE '  GE' TO GODK-STATUSKODER                                      
015913     CALL CBLTDLI USING GNP WDH7-PCB DLI-IO-WDH711 SSA1                   
015914     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
015915     PERFORM IMS-STATUSKONTROLL                                           
015916     .                                                                    
015920     EJECT                                                                
016000 IMS-STATUSKONTROLL SECTION.                                              
016100                                                                          
016200     SET STATUS-IX TO 1                                                   
016300     SEARCH GODK-STATUS                                                   
016400       AT END                                                             
016500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016600           DELIMITED BY SIZE INTO FELTEXT                                 
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
