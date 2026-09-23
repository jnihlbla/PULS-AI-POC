001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5031100.                                                
001600 AUTHOR.         ASPFJÄLL MARKUS.                                         
001700 DATE-WRITTEN.   05/06/29.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        LÄSER WDH1 OCH VISAR POSTER                                      
002200*                                                                         
002301*        PROGRAMMET LÄSER      WDP3                                       
002310*        PROGRAMMET LÄSER      WDH1                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T311                                              
002700*        MID:         W5I31101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O31101                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5031100'.            
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-LAS-IDARTNR              PIC X       VALUE 'N'.                   
004600 77  WS-LAS-KDINVKAT             PIC X       VALUE 'N'.                   
004601 01  WS-IDUSER-CRE               PIC X(8).                                
004602 01  WS-IDUSER-PR1               PIC X(8).                                
004603 01  WS-IDUSER-PR2               PIC X(8).                                
004604 01  WS-IDUSER-PR3               PIC X(8).                                
004605 01  WS-DAREGDAT-CRE             PIC 9(8).                                
004606 01  WS-DAREGDAT-PR1             PIC 9(8).                                
004607 01  WS-DAREGDAT-PR2             PIC 9(8).                                
004608 01  WS-DAREGDAT-PR3             PIC 9(8).                                
004609 01  WS-KDINVKAT                 PIC 9(2).                                
004610 01  WS-IDARTNR                  PIC 9(8).                                
004611 01  WS-ANTAL-POST               PIC S9(3) COMP-3.                        
004612 01  WS-ANTALDAGAR               PIC 9(3).                                
004613 01  WS-KDSEGKEY                 PIC 9(3).                                
004614 01  WS-SECTION                  PIC X(32).                               
004615 01  WS-IMS                      PIC X(32).                               
004616*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004617 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004620 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005100                                                                          
005200 77  POST-SW                     PIC X       VALUE 'N'.                   
005300     88  POST-FINNS                          VALUE 'J'.                   
005400     88  POST-SAKNAS                         VALUE 'N'.                   
005500                                                                          
005510 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005520     88  NYCKLAR-OK                          VALUE 'J'.                   
005530     88  NYCKLAR-FEL                         VALUE 'N'.                   
005540                                                                          
005550 77  INDATA-SW                  PIC X       VALUE 'J'.                    
005560     88  INDATA-OK                          VALUE 'J'.                    
005570     88  INDATA-FEL                         VALUE 'N'.                    
005580                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5311'.                
005800     88  GODK-MID                            VALUE '5311' '5312'.         
006300     88  HELP-MID                            VALUE '0551'.                
006310 77  WS-SOK-TYP                    PIC X(10) VALUE SPACE.                 
006320     88  LAES-ARTIKEL                  VALUE 'ARTIKEL   '.                
006330     88  LAES-KDINVKAT                 VALUE 'KDINVKAT  '.                
006340     88  LAES-USERF                    VALUE 'USERF     '.                
006341     88  LAES-USERD                    VALUE 'USERD     '.                
006350     88  LAES-DATUM                    VALUE 'DATUM     '.                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007110     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007510*01  -COPY WDAGAREA                                                       
007520     SKIP3                                                                
007530  01  FILLER                      PIC X(8)    VALUE 'WORKAREA'.           
007531**01   -COPY  WORKAREA                                                    
007540     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007801     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
007802     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007803     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
007810     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008100     EJECT                                                                
008110 01  FILLER.                                                              
008120     03  MAILSEND.                                                        
008130       05  FILLER              PIC X(13)   VALUE                          
008140                                     'SENDING MAIL'.                      
008150                                                                          
008160                                                                          
008170                                                                          
008180                                                                          
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008710 01  PROG-TO-PROG-SW.                                                     
008720*    03  -COPY WMSGSOP                                                    
008730     EJECT                                                                
008800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008900*                                                                         
009000 01  SPAR-AREA.                                                           
009100     03  SPAR-IDTRANS               PIC X(4)    VALUE '5311'.             
009200     03  SPAR-IDDC                  PIC X(2).                             
009201     03  SPAR-IDARTNR-ENTER         PIC S9(9) COMP-3.                     
009202     03  SPAR-IDARTNR-NEXT          PIC S9(9) COMP-3.                     
009205     03  SPAR-KDINVKAT-ENTER        PIC S9(3) COMP-3.                     
009206     03  SPAR-KDINVKAT-NEXT         PIC S9(3) COMP-3.                     
009207     03  SPAR-SEQF-KDINVKAT         PIC S9(3) COMP-3.                     
009208     03  SPAR-DATUM-ENTER           PIC  9(6).                            
009209     03  SPAR-DATUM-NEXT            PIC  9(6).                            
009210     03  SPAR-IDUSER-ENTER          PIC X(8).                             
009211     03  SPAR-IDUSER-NEXT           PIC X(8).                             
009220     03  SPAR-TISEGKEY-ENTER        PIC S9(9) COMP-3.                     
009221     03  SPAR-TISEGKEY-NEXT         PIC S9(9) COMP-3.                     
009222     03  SPAR-DAREGDAT-SORT-ENTER PIC 9(8).                               
009223     03  SPAR-DAREGDAT-SORT-NEXT    PIC 9(8).                             
009224     03  SPAR-SEQF-DAREGDAT-SORT    PIC 9(8).                             
009225     03  SPAR-SEQD-DAREGDAT-SORT    PIC 9(8).                             
009226     03  SPAR-IDARTNR-SEQ-ENTER    PIC S9(9) COMP-3.                      
009227     03  SPAR-IDARTNR-SEQ-NEXT     PIC S9(9) COMP-3.                      
009228     03  SPAR-KDSEGKEY-ENTER       PIC X.                                 
009229     03  SPAR-KDSEGKEY-NEXT        PIC X.                                 
009230     03  SPAR-IDARTNR-MID          PIC 9(9).                              
009231     03  SPAR-KDINVKAT-MID         PIC 9(2).                              
009232     03  SPAR-DATUM-MID            PIC 9(8).                              
009233     03  SPAR-IDUSER-MID             PIC X(8).                            
009301     03  SPAR-SOK-TYP             PIC X(10)   VALUE SPACE.                
009302     EJECT                                                                
009303*    --- AREOR FÖR SÖK-TYP                                                
009308*    --- AREOR FÖR DATUM                                                  
009310 01  WS-DATUM                      PIC 9(9).                              
009311 01  WS-DATUM1-9 REDEFINES WS-DATUM.                                      
009312     03 WS-DATUM1-3                PIC 9(3).                              
009313     03 WS-DATUM4-9                PIC 9(6).                              
009314                                                                          
009320 01  WS-DATUM1                     PIC 9(9).                              
009330 01  WS-DATUM2                     PIC 9(9).                              
009331 01  WS-DATUM-FOM                  PIC 9(8).                              
009332 01  WS-DATUM-TOM                  PIC 9(8).                              
009333                                                                          
009340 01  WS-DATUM-SSAAMMDD             PIC 9(8).                              
009350 01  WS-DATUM-AAAAMMDD REDEFINES WS-DATUM-SSAAMMDD.                       
009360     03 WS-DATUM-SS                PIC 9(2).                              
009370     03 WS-DATUM-AAMMDD            PIC 9(6).                              
009380                                                                          
009400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W5I31101                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W5O31101                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011601*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011602     03  W-IDARTNR-MIN-X.                                                 
011603         05  W-IDARTNR-MIN       PIC S9(9)        COMP-3.                 
011604                                                                          
011608     03  W-IDUSER-MIN-X.                                                  
011609         05  W-IDUSER-MIN        PIC X(8).                                
011610                                                                          
011611     03  W-IDUSER-X.                                                      
011612         05  W-IDUSER            PIC X(8).                                
011613                                                                          
011614     03  W-KDINVKAT-X.                                                    
011615         05  W-KDINVKAT          PIC 9(2).                                
011616                                                                          
011617     03  W-DATUM-X.                                                       
011618         05  W-DATUM             PIC 9(6).                                
011619                                                                          
011620     03  W-KDARBTYP-X.                                                    
011621         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
011622                                                                          
011623     03  W-IDPERSON-X.                                                    
011624         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
011625                                                                          
011626     03  W-IDARTNR-X.                                                     
011627         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011628                                                                          
011629     03  W-IDDC-X.                                                        
011630         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011631                                                                          
011632     03  W-TISEGKEY-X.                                                    
011633         05  W-TISEGKEY          PIC S9(9)   VALUE ZERO  COMP-3.          
011634     03  W-DAREGDAT-SORT-X.                                               
011635         05  W-DAREGDAT-SORT       PIC 9(8)    VALUE ZERO.                
011636                                                                          
011640     03 W-WDH111KY-X.                                                     
011641       05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.               
011643       05    W-KDINVKAT-UNIK       PIC S9(3)   VALUE ZERO COMP-3.         
011650       05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.         
011651       05    W-DAREGDAT-SORT-UNIK PIC 9(8)      VALUE ZERO.               
011660                                                                          
011690     03 W-WDH111-KEY-MIN-X.                                               
011692       05    W-IDDC-MIN        PIC X(2)  VALUE SPACE.                     
011694       05    W-KDINVKAT-MIN    PIC S9(3) VALUE ZERO COMP-3.               
011696       05    W-TISEGKEY-MIN    PIC S9(9) VALUE ZERO COMP-3.               
011697       05    W-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                     
011698                                                                          
011700     03 W-WDH111-KEY-MAX-X.                                               
011702       05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                     
011704       05    W-KDINVKAT-MAX    PIC S9(3) VALUE +049       COMP-3.         
011705       05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
011706       05    W-DAREGDAT-SORT-MIN PIC 9(8) VALUE 99999999.                 
011710                                                                          
011732                                                                          
011733   03  W-WDH1C1KY-X.                                                      
011734     05 W-SEQC-IDDC                 PIC X(2)  VALUE SPACE.                
011735     05 W-SEQC-KDINVKAT             PIC S9(3) VALUE ZERO COMP-3.          
011736     05 W-SEQC-DAREGDAT-SORT        PIC 9(8)  VALUE ZERO.                 
011737     05 W-SEQC-IDARTNR              PIC S9(9) VALUE ZERO COMP-3.          
011738     05 W-SEQC-TISEGKEY             PIC S9(9) VALUE ZERO COMP-3.          
011739                                                                          
011740   03  W-WDH1C1KY-MIN-X.                                                  
011741     05 W-SEQC-IDDC-MIN             PIC X(2)  VALUE SPACE.                
011742     05 W-SEQC-KDINVKAT-MIN         PIC S9(3) VALUE ZERO COMP-3.          
011743     05 W-SEQC-DAREGDAT-SORT-MIN    PIC 9(8)  VALUE ZERO.                 
011744     05 W-SEQC-IDARTNR-MIN          PIC S9(9) VALUE ZERO COMP-3.          
011745     05 W-SEQC-TISEGKEY-MIN         PIC S9(9) VALUE ZERO COMP-3.          
011746                                                                          
011747   03  W-WDH1C1KY-MAX-X.                                                  
011748     05 W-SEQC-IDDC-MAX             PIC X(2)  VALUE SPACE.                
011749     05 W-SEQC-KDINVKAT-MAX         PIC S9(3) VALUE +999  COMP-3.         
011750     05 W-SEQC-DAREGDAT-SORT-MAX    PIC 9(8)  VALUE 99999999.             
011751     05 W-SEQC-IDARTNR-MAX     PIC S9(9) VALUE +999999999 COMP-3.         
011752     05 W-SEQC-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
011753                                                                          
011754   03  W-WDH1E1KY-X.                                                      
011755     05 W-SEQE-IDDC            PIC X(2).                                  
011757     05 W-SEQE-DAREGDAT-SORT PIC 9(8).                                    
011760     05 W-SEQE-KDINVKAT        PIC S9(3)           COMP-3.                
011763     05 W-SEQE-IDARTNR         PIC S9(9)           COMP-3.                
011766     05 W-SEQE-TISEGKEY        PIC S9(9)           COMP-3.                
011780                                                                          
011781   03  W-WDH1E1KY-MIN-X.                                                  
011782     05 W-SEQE-IDDC-MIN            PIC X(2).                              
011783     05 W-SEQE-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                     
011784     05 W-SEQE-KDINVKAT-MIN        PIC S9(3) COMP-3 VALUE ZERO.           
011785     05 W-SEQE-IDARTNR-MIN         PIC S9(9) COMP-3 VALUE ZERO.           
011786     05 W-SEQE-TISEGKEY-MIN        PIC S9(9) COMP-3 VALUE ZERO.           
011787                                                                          
011788   03  W-WDH1E1KY-MAX-X.                                                  
011789     05 W-SEQE-IDDC-MAX            PIC X(2).                              
011790     05 W-SEQE-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                 
011791     05 W-SEQE-KDINVKAT-MAX        PIC S9(3) COMP-3 VALUE +999.           
011792     05 W-SEQE-IDARTNR-MAX     PIC S9(9) COMP-3 VALUE +999999999.         
011793     05 W-SEQE-TISEGKEY-MAX    PIC S9(9) COMP-3 VALUE +999999999.         
011794                                                                          
011795   03  W-WDH1F1KY-X.                                                      
011796     05 W-SEQF-IDUSER          PIC X(8).                                  
011799     05 W-SEQF-IDDC            PIC X(2).                                  
011802     05 W-SEQF-KDINVKAT        PIC S9(3) COMP-3.                          
011805     05 W-SEQF-DAREGDAT-SORT PIC 9(8).                                    
011808     05 W-SEQF-TISEGKEY        PIC S9(9) COMP-3.                          
011811     05 W-SEQF-IDARTNR         PIC S9(9) COMP-3.                          
011814     05 W-SEQF-KDSEGKEY        PIC X.                                     
011824                                                                          
011825   03  W-WDH1F1KY-MIN-X.                                                  
011826     05 W-SEQF-IDUSER-MIN          PIC X(8).                              
011827     05 W-SEQF-IDDC-MIN            PIC X(2).                              
011828     05 W-SEQF-KDINVKAT-MIN        PIC S9(3) VALUE ZERO COMP-3.           
011829     05 W-SEQF-DAREGDAT-SORT-MIN PIC 9(8)    VALUE ZERO.                  
011830     05 W-SEQF-TISEGKEY-MIN        PIC S9(9) VALUE ZERO COMP-3.           
011831     05 W-SEQF-IDARTNR-MIN         PIC S9(9) VALUE ZERO COMP-3.           
011832     05 W-SEQF-KDSEGKEY-MIN        PIC X     VALUE LOW-VALUE.             
011833                                                                          
011834   03  W-WDH1F1KY-MAX-X.                                                  
011835     05 W-SEQF-IDUSER-MAX          PIC X(8) VALUE HIGH-VALUE.             
011836     05 W-SEQF-IDDC-MAX            PIC X(2).                              
011837     05 W-SEQF-KDINVKAT-MAX        PIC S9(3) VALUE +999 COMP-3.           
011838     05 W-SEQF-DAREGDAT-SORT-MAX PIC 9(8)    VALUE 99999999.              
011839     05 W-SEQF-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
011840     05 W-SEQF-IDARTNR-MAX     PIC S9(9) VALUE +999999999 COMP-3.         
011841     05 W-SEQF-KDSEGKEY-MAX        PIC X     VALUE HIGH-VALUE.            
011842                                                                          
011843   03  W-WDH1D1KY-X.                                                      
011844     05 W-SEQD-IDUSER          PIC X(8).                                  
011845     05 W-SEQD-IDDC            PIC X(2).                                  
011846     05 W-SEQD-DAREGDAT-SORT PIC 9(8).                                    
011847     05 W-SEQD-KDINVKAT        PIC S9(3) COMP-3.                          
011849     05 W-SEQD-TISEGKEY        PIC S9(9) COMP-3.                          
011850     05 W-SEQD-IDARTNR         PIC S9(9) COMP-3.                          
011851     05 W-SEQD-KDSEGKEY        PIC X.                                     
011852                                                                          
011853   03  W-WDH1D1KY-MIN-X.                                                  
011854     05 W-SEQD-IDUSER-MIN          PIC X(8) VALUE LOW-VALUE.              
011855     05 W-SEQD-IDDC-MIN            PIC X(2) VALUE LOW-VALUE.              
011856     05 W-SEQD-DAREGDAT-SORT-MIN PIC 9(8) VALUE ZERO.                     
011857     05 W-SEQD-KDINVKAT-MIN        PIC S9(3) COMP-3 VALUE ZERO.           
011858     05 W-SEQD-TISEGKEY-MIN        PIC S9(9) COMP-3 VALUE ZERO.           
011859     05 W-SEQD-IDARTNR-MIN         PIC S9(9) COMP-3 VALUE ZERO.           
011860     05 W-SEQD-KDSEGKEY-MIN        PIC X    VALUE LOW-VALUE.              
011861                                                                          
011862   03  W-WDH1D1KY-MAX-X.                                                  
011863     05 W-SEQD-IDUSER-MAX          PIC X(8) VALUE HIGH-VALUE.             
011864     05 W-SEQD-IDDC-MAX            PIC X(2) VALUE HIGH-VALUE.             
011865     05 W-SEQD-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                 
011866     05 W-SEQD-KDINVKAT-MAX        PIC S9(3) VALUE +999 COMP-3.           
011867     05 W-SEQD-TISEGKEY-MAX     PIC S9(9) VALUE +999999999 COMP-3.        
011868     05 W-SEQD-IDARTNR-MAX      PIC S9(9) VALUE +999999999 COMP-3.        
011869     05 W-SEQD-KDSEGKEY-MAX        PIC X    VALUE HIGH-VALUE.             
011870                                                                          
011871     03  WS-IDMAIL.                                                       
011872         05  IDMAIL              PIC X(58) VALUE SPACE.                   
011873                                                                          
011874      03  WS-URVAL1.                                                      
011875          05  URV-BAS             PIC X(4) VALUE 'WDH1'.                  
011876          05  URV-IDDC            PIC X(2) VALUE SPACE.                   
011877          05  URV-SOK-TYP         PIC X(10) VALUE SPACE.                  
011878          05  URV-IDARTNR         PIC S9(9) VALUE ZERO COMP-3.            
011879          05  URV-KDINVKAT        PIC S9(3) VALUE ZERO COMP-3.            
011881          05  URV-DATUM           PIC 9(6) VALUE ZERO.                    
011882          05  URV-IDUSER          PIC X(8) VALUE SPACE.                   
011883                                                                          
011884     SKIP2                                                                
011885**** --- BILD RUBRIKER ----                                               
011886                                                                          
011887 01  FILLER                      PIC X(16)   VALUE 'CREATED-RAD1'.        
011888 01  CREATED-RAD1.                                                        
011889     03  CR1-IDARTNR             PIC Z(8)9   VALUE SPACE.                 
011890     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011891     03  CR1-TYPE                PIC 9(2).                                
011892     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011893     03  FILLER                  PIC X(9)    VALUE                        
011894         'CREATED  '.                                                     
011895     03  CR1-DATE                PIC X(8)    VALUE SPACE.                 
011896     03  FILLER                  PIC X(1)    VALUE SPACE.                 
011897     03  CR1-IDUSER              PIC X(8)    VALUE SPACE.                 
011898                                                                          
011899 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD1'.          
011900 01  PRINT-RAD1.                                                          
011901     03  FILLER                  PIC X(11)   VALUE SPACE.                 
011902     03  RAD1-TYPE               PIC 9(2).                                
011903     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011904     03  FILLER                  PIC X(9)    VALUE                        
011905         'PRINTED1 '.                                                     
011906     03  RAD1-DATE               PIC X(8)    VALUE SPACE.                 
011907     03  FILLER                  PIC X       VALUE SPACE.                 
011908     03  RAD1-USER               PIC X(8)    VALUE SPACE.                 
011909     03  FILLER                  PIC X(9)   VALUE                         
011910         'CR   PR1 '.                                                     
011911     03  RAD1-WS-DATE            PIC Z(2)9   VALUE ZERO.                  
011912     03  FILLER                  PIC X(14)   VALUE                        
011913         '  CR   PR1    '.                                                
011914     03  RAD1-WS-DATETOT         PIC Z(2)9    VALUE ZERO.                 
011915                                                                          
011916 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD2'.          
011917 01  PRINT-RAD2.                                                          
011918     03  FILLER                  PIC X(11)   VALUE SPACE.                 
011919     03  RAD2-TYPE               PIC 9(2).                                
011920     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011921     03  FILLER                  PIC X(9)    VALUE                        
011922         'PRINTED2 '.                                                     
011923     03  RAD2-DATE               PIC X(8)    VALUE SPACE.                 
011924     03  FILLER                  PIC X       VALUE SPACE.                 
011925     03  RAD2-USER               PIC X(8)    VALUE SPACE.                 
011926     03  FILLER                  PIC X(9)   VALUE                         
011927         'PR1  PR2 '.                                                     
011928     03  RAD2-WS-DATE            PIC Z(2)9   VALUE ZERO.                  
011929     03  FILLER                  PIC X(14)   VALUE                        
011930         '  PR1  PR2    '.                                                
011931     03  RAD2-WS-DATETOT         PIC Z(2)9   VALUE ZERO.                  
011932                                                                          
011933 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD3'.          
011934 01  PRINT-RAD3.                                                          
011935     03  FILLER                  PIC X(11)   VALUE SPACE.                 
011936     03  RAD3-TYPE               PIC 9(2).                                
011937     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011938     03  FILLER                  PIC X(9)    VALUE                        
011939         'PRINTED3 '.                                                     
011940     03  RAD3-DATE               PIC X(8)    VALUE SPACE.                 
011941     03  FILLER                  PIC X       VALUE SPACE.                 
011942     03  RAD3-USER               PIC X(8)    VALUE SPACE.                 
011943     03  FILLER                  PIC X(9)   VALUE                         
011944         'PR2  PR3 '.                                                     
011945     03  RAD3-WS-DATE            PIC Z(2)9   VALUE ZERO.                  
011946     03  FILLER                  PIC X(14)   VALUE                        
011947         '  PR1  PR3    '.                                                
011948     03  RAD3-WS-DATETOT         PIC Z(2)9   VALUE ZERO.                  
011949                                                                          
011950*    --- STATUS-KOD FRÅN IMS                                              
011960 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(128).                              
012800 01  SSA2                        PIC X(128).                              
012810 01  SSA3                        PIC X(128).                              
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
013602 01  DLI-IO-WDP301.                                                       
013603*    03  -COPY WDP301                                                     
013604     EJECT                                                                
013605 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
013606 01  DLI-IO-WDP311.                                                       
013607*    03  -COPY WDP311                                                     
013608 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH101'.                      
013609 01  DLI-IO-WDH101.                                                       
013610*    03  -COPY WDH101                                                     
013611     EJECT                                                                
013612 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH111'.                      
013613 01  DLI-IO-WDH111.                                                       
013620*    03  -COPY WDH111                                                     
013900     EJECT                                                                
013901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH121'.                      
013902 01  DLI-IO-WDH121.                                                       
013903*    03  -COPY WDH121                                                     
013904     EJECT                                                                
013905 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1A1'.                      
013910 01  DLI-IO-WDH1A1.                                                       
013940*    03   -COPY WDH1A1                                                    
013950     EJECT                                                                
013960 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1C1'.                      
013970 01  DLI-IO-WDH1C1.                                                       
013980*    03   -COPY WDH1C1                                                    
013990     EJECT                                                                
013995 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1D1'.                      
013996 01  DLI-IO-WDH1D1.                                                       
013997*    03   -COPY WDH1D1                                                    
013998 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1E1'.                      
013999 01  DLI-IO-WDH1E1.                                                       
014000*    03   -COPY WDH1E1                                                    
014001     EJECT                                                                
014002 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH1F1'.                      
014003 01  DLI-IO-WDH1F1.                                                       
014004*    03   -COPY WDH1F1                                                    
014005     EJECT                                                                
014010 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014110*01  -COPY W0009   -PRE ALT-                                              
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014401                                                                          
014402*01  -COPY W0008  -PRE WDP3-                                              
014403     05  FILLER                  PIC X.                                   
014404                                                                          
014405*01  -COPY W0008  -PRE WDH1-                                              
014410     05  FILLER                  PIC X.                                   
014420                                                                          
014430*01  -COPY W0008  -PRE WDH1A-                                             
014440     05  FILLER                  PIC X.                                   
014450*01  -COPY W0008  -PRE WDH1C-                                             
014460     05  FILLER                  PIC X.                                   
014470*01  -COPY W0008  -PRE WDH1D-                                             
014480     05  FILLER                  PIC X.                                   
014490*01  -COPY W0008  -PRE WDH1E-                                             
014491     05  FILLER                  PIC X.                                   
014492*01  -COPY W0008  -PRE WDH1F-                                             
014493     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB WDP7-PCB WDP3-PCB             
014602                           WDH1-PCB WDH1A-PCB WDH1C-PCB WDH1D-PCB         
014603                                    WDH1E-PCB WDH1F-PCB.                  
014604 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB WDP7-PCB WDP3-PCB             
014620                           WDH1-PCB WDH1A-PCB WDH1C-PCB WDH1D-PCB         
014630                                    WDH1E-PCB WDH1F-PCB.                  
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-KOLLA-NYCKLAR                                            
015300       IF NYCKLAR-OK                                                      
015501           IF MFS-FIRST                                                   
015502             PERFORM C-FOERSTA-SIDA                                       
015503           ELSE                                                           
015504             IF MFS-NEXT                                                  
015505               PERFORM D-NAESTA-SIDA                                      
015506             ELSE                                                         
015507               IF MFS-UPDATE                                              
015508                 PERFORM G-UPDATE                                         
015509***              IF INDATA-FEL                                            
015510                   PERFORM E-SAMMA-SIDA                                   
015511***              END-IF                                                   
015512               ELSE                                                       
015513                 PERFORM E-SAMMA-SIDA                                     
015514               END-IF                                                     
015515             END-IF                                                       
015520           END-IF                                                         
015800         PERFORM F-LAES-VISA-INFO                                         
015900       END-IF                                                             
016000*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016100*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O31101 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600***  CALL FELLOG                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I31101                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I31101                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W5O31101' TO MFS-IDMOD                                         
018900     MOVE '5311' TO MOD-IDTRANS                                           
019000     MOVE SPACE           TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
019910     MOVE 'GB ' TO MED-IDSKYLT                                            
019920                                                                          
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020300                                                                          
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '5311'            TO MSGI-IDTRANS                               
020900     IF GODK-MID                                                          
021000       IF MID-IDARTNR-IN NUMERIC AND MID-IDARTNR-IN > ZERO                
021010         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
021020       END-IF                                                             
021030     END-IF                                                               
021200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021210     IF GODK-MID                                                          
021300       MOVE MSGI-SPAR-AREA TO SPAR-AREA                                   
021301       MOVE SPAR-SOK-TYP   TO WS-SOK-TYP                                  
021310     ELSE                                                                 
021311       MOVE ALL '+'           TO MID-W5I31101                             
021321       PERFORM S02-NOLLA-SPAR-AREA                                        
021330     END-IF                                                               
021500*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
021600     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021720                                                                          
021800     MOVE JA TO NYCKLAR-SW                                                
022001                                                                          
022002*    -- KONTROLL AV IDDC                                                  
022003     MOVE MFS-RENSA-FAELT         TO MOD-IDDC-IN                          
022004                                                                          
022005     IF GODK-MID                                                          
022006       IF SPAR-IDTRANS = '5312'                                           
022009         MOVE ALL '+'             TO MID-W5I31101                         
022011                                                                          
022012         MOVE SPAR-IDDC           TO MID-IDDC-IN                          
022013         MOVE SPAR-IDARTNR-ENTER  TO MID-IDARTNR-IN                       
022014       ELSE                                                               
022015         IF MID-IDDC-IN NOT = SPAR-IDDC                                   
022016           MOVE '7'         TO MFS-IDPFK                                  
022017           MOVE SPACE       TO MFS-KDTRTYP                                
022018         END-IF                                                           
022019       END-IF                                                             
022020     ELSE                                                                 
022021       MOVE MSGI-IDDC       TO MID-IDDC-IN                                
022025     END-IF                                                               
022026     INSPECT MID-IDDC-IN REPLACING LEADING SPACE BY ZERO                  
022027     IF MID-IDDC-IN = '00'                                                
022028       MOVE MSGI-IDDC  TO MID-IDDC-IN                                     
022029     END-IF                                                               
022030     IF MID-IDDC-IN NOT = ALL '+'                                         
022031       MOVE MID-IDDC-IN    TO W-IDDC-MIN                                  
022032                              SPAR-IDDC                                   
022033                              W-IDDC-MAX                                  
022034                              W-IDDC                                      
022035                              MOD-IDDC-IN                                 
022036                              MOD-IDDC-UT                                 
022037                              W-IDDC-UNIK                                 
022038                              URV-IDDC                                    
022039                              W-SEQC-IDDC-MIN                             
022040                              W-SEQC-IDDC-MAX                             
022041                              W-SEQD-IDDC-MIN                             
022042                              W-SEQD-IDDC-MAX                             
022043                              W-SEQE-IDDC-MIN                             
022044                              W-SEQE-IDDC-MAX                             
022045     ELSE                                                                 
022046       MOVE MSGI-IDDC      TO W-IDDC-MIN                                  
022047                              SPAR-IDDC                                   
022048                              W-IDDC-MAX                                  
022049                              MOD-IDDC-IN                                 
022050                              MOD-IDDC-UT                                 
022051                              W-IDDC                                      
022052                              W-IDDC-UNIK                                 
022053                              URV-IDDC                                    
022054                              W-SEQC-IDDC-MIN                             
022055                              W-SEQC-IDDC-MAX                             
022056                              W-SEQE-IDDC-MIN                             
022057                              W-SEQE-IDDC-MAX                             
022060     END-IF                                                               
022101                                                                          
022102*    -- KONTROLL AV IDARTNR                                               
022103     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
022104                                                                          
022105     IF MID-IDARTNR-IN NOT = ALL '+'                                      
022106       MOVE '7'         TO MFS-IDPFK                                      
022107       MOVE SPACE       TO MFS-KDTRTYP                                    
022108                                                                          
022109       INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO             
022110       IF MID-IDARTNR-IN NUMERIC                                          
022111         MOVE MID-IDARTNR-IN TO W-IDARTNR                                 
022112                                URV-IDARTNR                               
022113                                MOD-IDARTNR-UT                            
022114                                SPAR-IDARTNR-ENTER                        
022115                                MSGI-IDARTNR                              
022116         MOVE ZERO           TO SPAR-KDINVKAT-ENTER                       
022117                                SPAR-KDINVKAT-NEXT                        
022118                                SPAR-DATUM-NEXT                           
022119                                SPAR-DATUM-ENTER                          
022120         MOVE 'ARTIKEL   '   TO WS-SOK-TYP                                
022121                                SPAR-SOK-TYP                              
022122       ELSE                                                               
022123         MOVE NEJ TO NYCKLAR-SW                                           
022124         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
022125       END-IF                                                             
022126     ELSE                                                                 
022127       MOVE ZERO             TO W-IDARTNR                                 
022128                                                                          
022129       MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-UT                            
022130     END-IF                                                               
022140                                                                          
022141***  -- KONTROLL AV KDINVKAT-IN                                           
022142**** IF EGEN-MID                                                          
022143     MOVE MFS-RENSA-FAELT TO MOD-KDINVKAT-IN                              
022144                                                                          
022145     IF MID-KDINVKAT-IN NOT = ALL '+'                                     
022146       MOVE '7'         TO MFS-IDPFK                                      
022147       MOVE SPACE       TO MFS-KDTRTYP                                    
022148                                                                          
022149       INSPECT MID-KDINVKAT-IN  REPLACING LEADING SPACE BY ZERO           
022150       IF MID-KDINVKAT-IN  NUMERIC                                        
022151         IF MID-KDINVKAT-IN >= ZERO                                       
022152           MOVE MID-KDINVKAT-IN  TO W-KDINVKAT-UNIK                       
022153                                    W-KDINVKAT-MIN                        
022154                                    W-KDINVKAT-MAX                        
022155                                    WS-KDINVKAT                           
022156                                    MOD-KDINVKAT-UT                       
022157                                    URV-KDINVKAT                          
022158                                    W-SEQC-KDINVKAT-MIN                   
022159                                    W-SEQC-KDINVKAT-MAX                   
022160                                    W-SEQF-KDINVKAT-MIN                   
022161                                    W-SEQF-KDINVKAT-MAX                   
022162                                    SPAR-SEQF-KDINVKAT                    
022163                                    SPAR-KDINVKAT-ENTER                   
022164                                                                          
022165           MOVE ZERO TO SPAR-IDARTNR-ENTER                                
022166                        SPAR-IDARTNR-NEXT                                 
022167                        SPAR-DATUM-ENTER                                  
022168                        SPAR-DATUM-NEXT                                   
022169           MOVE 'KDINVKAT  '      TO WS-SOK-TYP                           
022170                                     SPAR-SOK-TYP                         
022171         ELSE                                                             
022172           MOVE ZERO               TO SPAR-KDINVKAT-ENTER                 
022173         END-IF                                                           
022174       ELSE                                                               
022175         MOVE NEJ TO NYCKLAR-SW                                           
022176         MOVE MFS-RENSA-FAELT TO MOD-KDINVKAT-UT                          
022177       END-IF                                                             
022178     END-IF                                                               
022180*   -- KONTROLL AV DATUM                                                  
022181     MOVE MFS-RENSA-FAELT TO MOD-DATUM-IN                                 
022182     IF MID-DATUM-IN NOT = ALL '+'                                        
022183       MOVE '7'         TO MFS-IDPFK                                      
022184       MOVE SPACE       TO MFS-KDTRTYP                                    
022185       IF MID-DATUM-IN NUMERIC AND MID-DATUM-IN > 0                       
022186         MOVE MID-DATUM-IN             TO MOD-DATUM-UT                    
022187                                          W-DATUM                         
022188                                          WS-DATUM-AAMMDD                 
022189                                          SPAR-DATUM-ENTER                
022190                                          SPAR-DATUM-NEXT                 
022191         MOVE 20                       TO WS-DATUM-SS                     
022192         MOVE WS-DATUM-SSAAMMDD        TO W-DAREGDAT-SORT                 
022193         IF MID-DATUM-IN = 999999                                         
022194           MOVE 99999999 TO W-DAREGDAT-SORT                               
022195         END-IF                                                           
022196*        COMPUTE W-DAREGDAT-SORT = WS-DATUM-SSAAMMDD                      
022197*         99999999 - WS-DATUM-SSAAMMDD                                    
022198         MOVE  W-DAREGDAT-SORT     TO  W-SEQC-DAREGDAT-SORT-MAX           
022199                                       W-SEQD-DAREGDAT-SORT-MAX           
022200                                       W-SEQE-DAREGDAT-SORT-MAX           
022201                                       W-SEQF-DAREGDAT-SORT-MAX           
022202                                       SPAR-SEQF-DAREGDAT-SORT            
022203                                       SPAR-SEQD-DAREGDAT-SORT            
022204                                                                          
022205         IF MID-KDINVKAT-IN NOT = ALL '+' AND                             
022206            MID-DATUM-IN    NOT = ALL '+' AND                             
022207            MID-IDUSER-IN       = ALL '+'                                 
022208           MOVE 'KDINVKAT  '           TO WS-SOK-TYP                      
022209                                          SPAR-SOK-TYP                    
022210         END-IF                                                           
022211         IF MID-KDINVKAT-IN  = ALL '+' AND                                
022212            MID-IDUSER-IN    = ALL '+' AND                                
022213            MID-DATUM-IN NOT = ALL '+'                                    
022214           MOVE 'DATUM     '           TO WS-SOK-TYP                      
022215                                          SPAR-SOK-TYP                    
022216         END-IF                                                           
022217       ELSE                                                               
022218         MOVE NEJ TO NYCKLAR-SW                                           
022219         MOVE ZERO                     TO SPAR-DATUM-ENTER                
022220         MOVE ZERO                     TO SPAR-DATUM-NEXT                 
022221       END-IF                                                             
022222                                                                          
022223     END-IF                                                               
022224                                                                          
022225*    -- KONTROLL AV IDUSER-IN                                             
022226     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
022227                                                                          
022228     IF MID-IDUSER-IN   NOT = ALL '+'                                     
022229       MOVE '7'         TO MFS-IDPFK                                      
022230       MOVE SPACE       TO MFS-KDTRTYP                                    
022231     END-IF                                                               
022233     IF MID-IDUSER-IN NOT = ALL '+' AND INDATA-OK                         
022234       INSPECT MID-IDUSER-IN    REPLACING LEADING SPACE BY ZERO           
022235       MOVE MID-IDUSER-IN    TO W-IDUSER                                  
022236                                MOD-IDUSER-UT                             
022237                                SPAR-IDUSER-ENTER                         
022238                                SPAR-IDUSER-NEXT                          
022239                                W-SEQF-IDUSER-MIN                         
022240                                W-SEQF-IDUSER-MAX                         
022241                                W-SEQD-IDUSER-MIN                         
022242                                W-SEQD-IDUSER-MAX                         
022243       MOVE 'USERF     '     TO SPAR-SOK-TYP                              
022244                                WS-SOK-TYP                                
022245                                                                          
022246     ELSE                                                                 
022247       MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                              
022248     END-IF                                                               
022249                                                                          
022250     IF MID-IDUSER-IN NOT = ALL '+' AND INDATA-OK                         
022251       IF MID-KDINVKAT-IN  = ALL '+'                                      
022252         MOVE ZERO            TO SPAR-SEQF-KDINVKAT                       
022253       END-IF                                                             
022254       IF MID-DATUM-IN  = ALL '+'                                         
022255         MOVE ZERO            TO SPAR-SEQF-DAREGDAT-SORT                  
022256                                 SPAR-SEQD-DAREGDAT-SORT                  
022257       END-IF                                                             
022258       IF MID-DATUM-IN NOT = ALL '+' AND                                  
022259          MID-KDINVKAT-IN = ALL '+'                                       
022260         MOVE 'USERD     ' TO WS-SOK-TYP                                  
022261                              SPAR-SOK-TYP                                
022262                                                                          
022263       ELSE                                                               
022264         IF MID-DATUM-IN    = ALL '+' AND                                 
022265            MID-KDINVKAT-IN = ALL '+'                                     
022266           MOVE 'USERD     ' TO WS-SOK-TYP                                
022267                              SPAR-SOK-TYP                                
022268                                                                          
022269         ELSE                                                             
022270           MOVE 'USERF     ' TO WS-SOK-TYP                                
022271                              SPAR-SOK-TYP                                
022272                                                                          
022273         END-IF                                                           
022274       END-IF                                                             
022275     END-IF                                                               
022276**************************************************************            
022277     IF MFS-FIRST AND GODK-MID AND INDATA-OK                              
022278       IF MID-IDARTNR-IN  = ALL '+' AND                                   
022279          MID-KDINVKAT-IN = ALL '+' AND                                   
022280          MID-DATUM-IN =    ALL '+' AND                                   
022281          MID-IDUSER-IN =   ALL '+'                                       
022282         IF SPAR-SOK-TYP = 'ARTIKEL   '                                   
022283           MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR                          
022284                                       MOD-IDARTNR-UT                     
022285           MOVE JA                  TO WS-LAS-IDARTNR                     
022286         END-IF                                                           
022287         IF SPAR-SOK-TYP = 'KDINVKAT  '                                   
022288           MOVE SPAR-KDINVKAT-ENTER         TO W-SEQC-KDINVKAT-MIN        
022289                                               W-SEQC-KDINVKAT            
022290                                               W-KDINVKAT-UNIK            
022291                                               WS-KDINVKAT                
022292                                               W-KDINVKAT                 
022293                                               W-KDINVKAT-MIN             
022294                                               W-KDINVKAT-MAX             
022295                                            MOD-KDINVKAT-UT               
022296            MOVE 'J' TO WS-LAS-KDINVKAT                                   
022297         END-IF                                                           
022298         IF SPAR-SOK-TYP = 'KDINVKAT  ' AND                               
022299            SPAR-DATUM-ENTER > 0                                          
022300           MOVE SPAR-DATUM-ENTER         TO MOD-DATUM-UT                  
022301                                            W-DATUM                       
022302                                            WS-DATUM-AAMMDD               
022303                                                                          
022304           MOVE 20 TO WS-DATUM-SS                                         
022305           MOVE WS-DATUM-SSAAMMDD        TO W-DAREGDAT-SORT               
022306           IF SPAR-DATUM-ENTER = 999999                                   
022307             MOVE 99999999 TO W-DAREGDAT-SORT                             
022308           END-IF                                                         
022309*          COMPUTE W-DAREGDAT-SORT = WS-DATUM-SSAAMMDD                    
022310*           99999999 - WS-DATUM-SSAAMMDD                                  
022311            MOVE  W-DAREGDAT-SORT TO    W-SEQC-DAREGDAT-SORT-MAX          
022312         END-IF                                                           
022313         IF SPAR-SOK-TYP = 'DATUM     ' AND                               
022314            SPAR-DATUM-ENTER > 0                                          
022315           MOVE SPAR-DATUM-ENTER         TO MOD-DATUM-UT                  
022316                                            W-DATUM                       
022317                                            WS-DATUM-AAMMDD               
022318                                                                          
022319           MOVE 20 TO WS-DATUM-SS                                         
022320           MOVE WS-DATUM-SSAAMMDD        TO W-DAREGDAT-SORT               
022321           IF SPAR-DATUM-ENTER = 999999                                   
022322             MOVE 99999999 TO W-DAREGDAT-SORT                             
022323           END-IF                                                         
022324*          COMPUTE W-DAREGDAT-SORT = WS-DATUM-SSAAMMDD                    
022325*          99999999 - WS-DATUM-SSAAMMDD                                   
022326           MOVE  W-DAREGDAT-SORT    TO  W-SEQE-DAREGDAT-SORT-MAX          
022327                                                                          
022328         END-IF                                                           
022329         IF LAES-USERF OR LAES-USERD                                      
022330                                                                          
022331           MOVE SPAR-IDUSER-ENTER        TO MOD-IDUSER-UT                 
022332                                            W-IDUSER                      
022333                                            W-SEQF-IDUSER-MIN             
022334                                            W-SEQF-IDUSER-MAX             
022335                                            W-SEQD-IDUSER-MIN             
022336                                            W-SEQD-IDUSER-MAX             
022337           IF SPAR-SEQF-KDINVKAT > ZERO                                   
022338             MOVE SPAR-SEQF-KDINVKAT TO W-SEQF-KDINVKAT-MIN               
022339                                        W-SEQF-KDINVKAT-MAX               
022340                                        MOD-KDINVKAT-UT                   
022341           END-IF                                                         
022342           IF SPAR-SEQF-DAREGDAT-SORT > ZERO                              
022343             MOVE SPAR-SEQF-DAREGDAT-SORT TO                              
022344                               W-SEQF-DAREGDAT-SORT-MAX                   
022345                                                                          
022346             MOVE SPAR-DATUM-ENTER TO MOD-DATUM-UT                        
022347           END-IF                                                         
022348           IF SPAR-SEQD-DAREGDAT-SORT > ZERO                              
022349             MOVE SPAR-SEQD-DAREGDAT-SORT TO                              
022350                               W-SEQD-DAREGDAT-SORT-MAX                   
022351                                                                          
022352             MOVE SPAR-DATUM-ENTER TO MOD-DATUM-UT                        
022353           END-IF                                                         
022354         END-IF                                                           
022355       END-IF                                                             
022356     END-IF                                                               
022357**************************************************************            
022358     IF MID-IDARTNR-IN NOT = ALL '+'                                      
022359       MOVE MID-IDARTNR-IN      TO SPAR-IDARTNR-MID                       
022360       MOVE ZERO                TO SPAR-KDINVKAT-MID                      
022361                                   SPAR-DATUM-MID                         
022362       MOVE SPACE               TO SPAR-IDUSER-MID                        
022363     END-IF                                                               
022364     IF MID-KDINVKAT-IN NOT = ALL '+' OR                                  
022365        MID-DATUM-IN NOT    = ALL '+' OR                                  
022366        MID-IDUSER-IN NOT   = ALL '+'                                     
022367       MOVE ZERO                  TO SPAR-IDARTNR-MID                     
022368       IF MID-KDINVKAT-IN  = ALL '+'                                      
022369         MOVE ZERO                TO SPAR-KDINVKAT-MID                    
022370       ELSE                                                               
022371         MOVE MID-KDINVKAT-IN     TO SPAR-KDINVKAT-MID                    
022372       END-IF                                                             
022373       IF MID-IDUSER-IN  = ALL '+'                                        
022374         MOVE SPACE               TO SPAR-IDUSER-MID                      
022375       ELSE                                                               
022376         MOVE MOD-IDUSER-IN       TO SPAR-IDUSER-MID                      
022377       END-IF                                                             
022378       IF MID-DATUM-IN = ALL '+'                                          
022379         MOVE ZERO                TO SPAR-DATUM-MID                       
022380       ELSE                                                               
022381         MOVE MID-DATUM-IN        TO SPAR-DATUM-MID                       
022382       END-IF                                                             
022383     END-IF                                                               
022384*** SLUT PÅ EGEN-MID                                                      
022385***  END-IF                                                               
022386     IF GODK-MID AND  NYCKLAR-OK                                          
022387       CONTINUE                                                           
022388     ELSE                                                                 
022389       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
022390       MOVE NEJ TO NYCKLAR-SW                                             
022391     END-IF                                                               
022392                                                                          
022393     IF NYCKLAR-FEL                                                       
022400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022500       CALL WMEDKONV USING MED-WMEDAREA                                   
022600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022700       PERFORM MFS-RENSA-FAELT-IN                                         
022800       PERFORM MFS-RENSA-FAELT-UT                                         
022801       IF MID-IDDC-IN NOT = ALL '+'                                       
022802         MOVE MID-IDDC-IN TO MOD-IDDC-IN                                  
022803         MOVE MID-IDDC-IN TO MOD-IDDC-UT                                  
022804       ELSE                                                               
022810         MOVE MSGI-IDDC TO MOD-IDDC-IN                                    
022820         MOVE MSGI-IDDC TO MOD-IDDC-UT                                    
022830       END-IF                                                             
022840                                                                          
022841       MOVE '002'      TO MSGI-KDCALL                                     
022842       MOVE '5311'     TO SPAR-IDTRANS                                    
022843       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
022844       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
022900     END-IF                                                               
023000     .                                                                    
023101     EJECT                                                                
023102 C-FOERSTA-SIDA SECTION.                                                  
023103                                                                          
023104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023105     CALL WMEDKONV USING MED-WMEDAREA                                     
023106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023107                                                                          
023108     PERFORM MFS-RENSA-FAELT-IN                                           
023109     .                                                                    
023110     EJECT                                                                
023111 D-NAESTA-SIDA SECTION.                                                   
023112                                                                          
023113     IF SPAR-IDTRANS = '5311'                                             
023114       IF SPAR-SOK-TYP = 'ARTIKEL   '                                     
023115         MOVE SPAR-IDARTNR-NEXT           TO W-IDARTNR                    
023116                                           MOD-IDARTNR-UT                 
023117         MOVE SPAR-KDINVKAT-NEXT          TO W-KDINVKAT                   
023118                                             W-KDINVKAT-MIN               
023119                                             W-KDINVKAT-MAX               
023122       ELSE                                                               
023123         IF SPAR-SOK-TYP = 'KDINVKAT  '                                   
023124         MOVE W-IDDC                      TO W-SEQC-IDDC-MIN              
023125                                             W-SEQC-IDDC                  
023126                                             W-IDDC-UNIK                  
023129         MOVE SPAR-IDARTNR-SEQ-NEXT      TO W-IDARTNR                     
023130                                             W-SEQC-IDARTNR               
023131                                                                          
023132         IF SPAR-DATUM-ENTER > 0                                          
023133           MOVE SPAR-DATUM-NEXT           TO MOD-DATUM-UT                 
023134         END-IF                                                           
023135         MOVE SPAR-KDINVKAT-NEXT          TO W-SEQC-KDINVKAT-MIN          
023136                                             W-SEQC-KDINVKAT              
023137                                             W-KDINVKAT-UNIK              
023138                                             WS-KDINVKAT                  
023139                                             W-KDINVKAT                   
023140                                             W-KDINVKAT-MIN               
023141                                             W-KDINVKAT-MAX               
023142                                             MOD-KDINVKAT-UT              
023143         MOVE SPAR-TISEGKEY-NEXT          TO W-SEQC-TISEGKEY              
023144                                             W-TISEGKEY                   
023145                                             W-TISEGKEY-UNIK              
023146                                                                          
023147         MOVE SPAR-DAREGDAT-SORT-NEXT    TO W-SEQC-DAREGDAT-SORT          
023148                                        W-SEQC-DAREGDAT-SORT-MAX          
023149                                        W-DAREGDAT-SORT-UNIK              
023150         ELSE                                                             
023151           IF SPAR-SOK-TYP = 'DATUM     '                                 
023152                                                                          
023153             MOVE W-IDDC                  TO W-SEQE-IDDC-MIN              
023154                                             W-SEQE-IDDC                  
023155                                                                          
023156             MOVE SPAR-IDARTNR-SEQ-NEXT   TO W-IDARTNR                    
023157                                             W-SEQE-IDARTNR               
023158                                                                          
023159             IF SPAR-DATUM-ENTER > 0                                      
023160               MOVE SPAR-DATUM-NEXT       TO MOD-DATUM-UT                 
023162                                             W-DATUM                      
023163                                             WS-DATUM-AAMMDD              
023164                                                                          
023165               MOVE 20 TO WS-DATUM-SS                                     
023166               MOVE WS-DATUM-SSAAMMDD     TO W-DAREGDAT-SORT              
023167               IF SPAR-DATUM-ENTER = 999999                               
023168                  MOVE 99999999 TO W-DAREGDAT-SORT                        
023169               END-IF                                                     
023170*              COMPUTE W-DAREGDAT-SORT =                                  
023171*              99999999 - WS-DATUM-SSAAMMDD                               
023172               MOVE W-DAREGDAT-SORT        TO                             
023173                    W-SEQE-DAREGDAT-SORT-MAX                              
023174                                                                          
023175             END-IF                                                       
023176             MOVE SPAR-KDINVKAT-NEXT      TO W-SEQE-KDINVKAT              
023178                                             WS-KDINVKAT                  
023179                                             W-KDINVKAT                   
023180                                             W-KDINVKAT-MIN               
023181                                             W-KDINVKAT-MAX               
023182                                                                          
023183             MOVE SPAR-TISEGKEY-NEXT      TO W-SEQE-TISEGKEY              
023184                                             W-TISEGKEY                   
023186                                                                          
023187             MOVE SPAR-DAREGDAT-SORT-NEXT    TO                           
023188                                        W-SEQE-DAREGDAT-SORT              
023189           ELSE                                                           
023190             IF LAES-USERF OR LAES-USERD                                  
023192               MOVE SPAR-KDINVKAT-NEXT       TO W-SEQF-KDINVKAT           
023193                                                W-SEQD-KDINVKAT           
023194               MOVE SPAR-IDUSER-NEXT         TO W-SEQF-IDUSER             
023196                                                MOD-IDUSER-UT             
023197                                                W-IDUSER                  
023198                                                W-SEQF-IDUSER-MIN         
023199                                                W-SEQF-IDUSER-MAX         
023200                                                W-SEQD-IDUSER             
023201                                                W-SEQD-IDUSER-MIN         
023202                                                W-SEQD-IDUSER-MAX         
023203               MOVE SPAR-DAREGDAT-SORT-NEXT    TO                         
023204                                        W-SEQF-DAREGDAT-SORT              
023205                                        W-SEQD-DAREGDAT-SORT              
023206               MOVE SPAR-TISEGKEY-NEXT      TO W-SEQF-TISEGKEY            
023207                                               W-SEQD-TISEGKEY            
023208               MOVE SPAR-IDARTNR-SEQ-NEXT   TO W-SEQF-IDARTNR             
023209                                               W-SEQD-IDARTNR             
023210               MOVE SPAR-KDSEGKEY-NEXT      TO W-SEQF-KDSEGKEY            
023211                                               W-SEQD-KDSEGKEY            
023212               IF SPAR-SEQF-KDINVKAT > ZERO                               
023213                 MOVE SPAR-SEQF-KDINVKAT TO W-SEQF-KDINVKAT-MIN           
023214                                            W-SEQF-KDINVKAT-MAX           
023215                                            MOD-KDINVKAT-UT               
023216               END-IF                                                     
023217               IF SPAR-SEQF-DAREGDAT-SORT > ZERO                          
023218                 MOVE SPAR-SEQF-DAREGDAT-SORT TO                          
023219                                   W-SEQF-DAREGDAT-SORT-MAX               
023220                                                                          
023221                 MOVE SPAR-DATUM-ENTER          TO MOD-DATUM-UT           
023222               END-IF                                                     
023223               IF SPAR-SEQD-DAREGDAT-SORT > ZERO                          
023224                 MOVE SPAR-SEQD-DAREGDAT-SORT TO                          
023225                                   W-SEQD-DAREGDAT-SORT-MAX               
023226                                                                          
023227                 MOVE SPAR-DATUM-ENTER          TO MOD-DATUM-UT           
023228               END-IF                                                     
023229             END-IF                                                       
023230           END-IF                                                         
023231         END-IF                                                           
023232       END-IF                                                             
023233     ELSE                                                                 
023234       PERFORM MFS-RENSA-FAELT-IN                                         
023235     END-IF                                                               
023236     .                                                                    
023237     EJECT                                                                
023238 E-SAMMA-SIDA SECTION.                                                    
023239                                                                          
023240     IF SPAR-IDTRANS = '5311' OR '0551'                                   
023241*                                                                         
023242       IF SPAR-SOK-TYP = 'ARTIKEL   '                                     
023243         MOVE W-IDDC              TO W-IDDC-MIN                           
023244                                                                          
023245         MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR                            
023246                                     MOD-IDARTNR-UT                       
023247       ELSE                                                               
023248         IF SPAR-SOK-TYP = 'KDINVKAT  '                                   
023249                                                                          
023250         MOVE W-IDDC                      TO W-SEQC-IDDC-MIN              
023251                                             W-SEQC-IDDC                  
023252                                             W-IDDC-UNIK                  
023253         MOVE SPAR-IDARTNR-SEQ-ENTER     TO W-IDARTNR                     
023254                                             W-SEQC-IDARTNR               
023255                                                                          
023256                                                                          
023257         MOVE SPAR-KDINVKAT-ENTER         TO W-SEQC-KDINVKAT-MIN          
023258                                             W-SEQC-KDINVKAT              
023259                                             W-KDINVKAT-UNIK              
023260                                             W-KDINVKAT                   
023261                                             W-KDINVKAT-MIN               
023262                                             W-KDINVKAT-MAX               
023263                                             WS-KDINVKAT                  
023264                                           MOD-KDINVKAT-UT                
023265         MOVE SPAR-TISEGKEY-ENTER         TO W-SEQC-TISEGKEY              
023266                                             W-TISEGKEY                   
023267                                             W-TISEGKEY-UNIK              
023268                                                                          
023269         MOVE SPAR-DAREGDAT-SORT-ENTER TO   W-SEQC-DAREGDAT-SORT          
023270                                        W-SEQC-DAREGDAT-SORT-MAX          
023271                                        W-DAREGDAT-SORT-UNIK              
023272                                                                          
023273         ELSE                                                             
023274           IF SPAR-SOK-TYP = 'DATUM     '                                 
023275             MOVE W-IDDC                   TO W-SEQE-IDDC                 
023276             MOVE SPAR-DATUM-ENTER         TO MOD-DATUM-UT                
023277                                                                          
023278             MOVE SPAR-DAREGDAT-SORT-ENTER TO                             
023279                        W-SEQE-DAREGDAT-SORT                              
023280                        W-SEQE-DAREGDAT-SORT-MAX                          
023281             MOVE SPAR-KDINVKAT-ENTER      TO W-SEQE-KDINVKAT             
023282             MOVE SPAR-IDARTNR-SEQ-ENTER   TO W-SEQE-IDARTNR              
023283             MOVE SPAR-TISEGKEY-ENTER      TO W-SEQE-TISEGKEY             
023284                                                                          
023285           ELSE                                                           
023286             IF LAES-USERF OR LAES-USERD                                  
023287               MOVE W-IDDC                 TO W-SEQF-IDDC                 
023288                                              W-SEQD-IDDC                 
023289               MOVE SPAR-IDUSER-ENTER      TO W-SEQF-IDUSER               
023290                                              W-SEQF-IDUSER-MIN           
023291                                              W-SEQF-IDUSER-MAX           
023292                                              W-SEQD-IDUSER               
023293                                              W-SEQD-IDUSER-MIN           
023294                                              W-SEQD-IDUSER-MAX           
023295                                              W-IDUSER                    
023296                                              MOD-IDUSER-UT               
023297               MOVE SPAR-DAREGDAT-SORT-ENTER TO                           
023298                        W-SEQF-DAREGDAT-SORT                              
023299                        W-SEQD-DAREGDAT-SORT                              
023300                                                                          
023301               MOVE SPAR-KDINVKAT-ENTER      TO W-SEQF-KDINVKAT           
023302                                                W-SEQD-KDINVKAT           
023303               MOVE SPAR-IDARTNR-SEQ-ENTER   TO W-SEQF-IDARTNR            
023304                                                W-SEQD-IDARTNR            
023305               MOVE SPAR-TISEGKEY-ENTER      TO W-SEQF-TISEGKEY           
023306                                                W-SEQD-TISEGKEY           
023307               MOVE SPAR-KDSEGKEY-ENTER      TO W-SEQF-KDSEGKEY           
023308                                                W-SEQD-KDSEGKEY           
023309                                                                          
023310               IF SPAR-SEQF-KDINVKAT > ZERO                               
023311                 MOVE SPAR-SEQF-KDINVKAT TO W-SEQF-KDINVKAT-MIN           
023312                                            W-SEQF-KDINVKAT-MAX           
023313                                            MOD-KDINVKAT-UT               
023314               END-IF                                                     
023315               IF SPAR-SEQF-DAREGDAT-SORT > ZERO                          
023316                 MOVE SPAR-SEQF-DAREGDAT-SORT TO                          
023317                                   W-SEQF-DAREGDAT-SORT-MAX               
023318                                                                          
023319                 MOVE SPAR-DATUM-ENTER TO MOD-DATUM-UT                    
023320               END-IF                                                     
023321               IF SPAR-SEQD-DAREGDAT-SORT > ZERO                          
023322                 MOVE SPAR-SEQD-DAREGDAT-SORT TO                          
023323                                   W-SEQD-DAREGDAT-SORT-MAX               
023324                                                                          
023325                 MOVE SPAR-DATUM-ENTER TO MOD-DATUM-UT                    
023326               END-IF                                                     
023328             END-IF                                                       
023329           END-IF                                                         
023330         END-IF                                                           
023331       END-IF                                                             
023332       IF MID-IDARTNR-IN  = ALL '+' AND                                   
023333          MID-KDINVKAT-IN = ALL '+' AND                                   
023334          MID-DATUM-IN    = ALL '+' AND                                   
023335          MID-IDUSER-IN   = ALL '+' AND                                   
023336          MID-KDARBTYP-IN = ALL '+' AND                                   
023337          MID-IDPERSON-IN = ALL '+'                                       
023338         PERFORM MFS-RENSA-FAELT-IN                                       
023339       ELSE                                                               
023340         IF INDATA-OK AND MFS-ENTER                                       
023341           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
023342           CALL WMEDKONV USING MED-WMEDAREA                               
023343           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
023344           PERFORM EA-MID-INDATA-TILL-MOD                                 
023345         ELSE                                                             
023346           IF INDATA-FEL                                                  
023347             MOVE ERR-WRONG-KEY TO MED-IDMFSINF                           
023348             CALL WMEDKONV USING MED-WMEDAREA                             
023349             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
023350           END-IF                                                         
023351         END-IF                                                           
023352       END-IF                                                             
023353     ELSE                                                                 
023354       PERFORM MFS-RENSA-FAELT-IN                                         
023355     END-IF                                                               
023356     .                                                                    
023357     EJECT                                                                
023358 EA-MID-INDATA-TILL-MOD SECTION.                                          
023359                                                                          
023360       IF MID-IDARTNR-IN = ALL '+'                                        
023361         MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                         
023362       ELSE                                                               
023363         MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                         
023364       END-IF                                                             
023365       IF MID-KDINVKAT-IN = ALL '+'                                       
023366         MOVE MFS-RENSA-FAELT   TO MOD-KDINVKAT-UT                        
023367       ELSE                                                               
023368         MOVE MID-KDINVKAT-IN   TO MOD-KDINVKAT-UT                        
023369       END-IF                                                             
023370       IF MID-DATUM-IN = ALL '+'                                          
023371         MOVE MFS-RENSA-FAELT   TO MOD-DATUM-UT                           
023372       ELSE                                                               
023373         MOVE MID-DATUM-IN      TO MOD-DATUM-UT                           
023374       END-IF                                                             
023375       IF MID-IDUSER-IN = ALL '+'                                         
023376         MOVE MFS-RENSA-FAELT   TO MOD-IDUSER-UT                          
023377       ELSE                                                               
023378         MOVE MID-IDUSER-IN     TO MOD-IDUSER-UT                          
023379       END-IF                                                             
023380       IF MID-IDDC-IN = ALL '+'                                           
023381         MOVE MFS-RENSA-FAELT   TO MOD-IDDC-UT                            
023382       ELSE                                                               
023383         MOVE MID-IDDC-IN       TO MOD-IDDC-IN                            
023384         MOVE MID-IDDC-IN       TO MOD-IDDC-UT                            
023385       END-IF                                                             
023386       IF MID-KDARBTYP-IN = ALL '+'                                       
023387         MOVE MFS-RENSA-FAELT   TO MOD-KDARBTYP-UT                        
023388       ELSE                                                               
023389         MOVE MID-KDARBTYP-IN   TO MOD-KDARBTYP-UT                        
023390         IF INDATA-OK                                                     
023391           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDARBTYP-ATTR                
023392         END-IF                                                           
023393       END-IF                                                             
023394       IF MID-IDPERSON-IN = ALL '+'                                       
023395         MOVE MFS-RENSA-FAELT   TO MOD-IDPERSON-UT                        
023396       ELSE                                                               
023397         MOVE MID-IDPERSON-IN   TO MOD-IDPERSON-UT                        
023398         IF INDATA-OK                                                     
023399           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-ATTR                
023400         END-IF                                                           
023401       END-IF                                                             
023402                                                                          
023403* * * * * FÖR VARJE MID-FÄLT                                              
023404* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
023405* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
023406* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
023407     .                                                                    
023408     EJECT                                                                
023410 F-LAES-VISA-INFO SECTION.                                                
023500                                                                          
023600     PERFORM FA-LAES-GRUNDDATA                                            
023700                                                                          
023800     IF SEGMENT-SAKNAS                                                    
023900        MOVE KEYS-ARE-MISSING  TO MED-IDMFSINF                            
024000        CALL WMEDKONV USING MED-WMEDAREA                                  
024100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024200        PERFORM MFS-RENSA-FAELT-UT                                        
024210        IF MID-IDARTNR-IN NUMERIC                                         
024220          MOVE MID-IDARTNR-IN        TO MOD-IDARTNR-UT                    
024221        ELSE                                                              
024222          IF LAES-ARTIKEL                                                 
024223            MOVE SPAR-IDARTNR-ENTER  TO MOD-IDARTNR-UT                    
024224          END-IF                                                          
024230        END-IF                                                            
024231                                                                          
024240        IF MID-KDINVKAT-IN NUMERIC                                        
024250          MOVE MID-KDINVKAT-IN       TO MOD-KDINVKAT-UT                   
024251        ELSE                                                              
024252          IF LAES-KDINVKAT                                                
024253            MOVE SPAR-KDINVKAT-ENTER TO MOD-KDINVKAT-UT                   
024254          END-IF                                                          
024260        END-IF                                                            
024261                                                                          
024262        IF MID-IDUSER-IN NOT = ALL '+'                                    
024263          MOVE MID-IDUSER-IN         TO MOD-IDUSER-UT                     
024264        ELSE                                                              
024265          IF LAES-USERF                                                   
024266            MOVE SPAR-IDUSER-ENTER   TO MOD-IDUSER-UT                     
024267          END-IF                                                          
024268        END-IF                                                            
024269        IF MID-DATUM-IN NOT = ALL '+'                                     
024270          MOVE MID-DATUM-IN          TO MOD-DATUM-UT                      
024271        ELSE                                                              
024272          IF LAES-DATUM                                                   
024273            MOVE SPAR-DATUM-ENTER    TO MOD-DATUM-UT                      
024274          END-IF                                                          
024275        END-IF                                                            
024276                                                                          
024277        IF MID-IDDC-IN NOT = ALL '+'                                      
024280          MOVE MID-IDDC-IN           TO MOD-IDDC-IN                       
024281                                        MOD-IDDC-UT                       
024282        ELSE                                                              
024283          MOVE MSGI-IDDC             TO MOD-IDDC-IN                       
024284                                        MOD-IDDC-UT                       
024290        END-IF                                                            
024291        MOVE '002'      TO MSGI-KDCALL                                    
024292        MOVE '5311'     TO SPAR-IDTRANS                                   
024293        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
024294        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
024300     ELSE                                                                 
024401*      -- POSITIONERA FÖR LÄSNING AV DATA TILL ÖVERSTA RADEN              
024402*      -- (EJ NÖDVÄNDIGT OM -MIN NYCKLAR ANVÄNDS DIREKT I SSA)            
024403       IF SPAR-SOK-TYP = 'ARTIKEL   '                                     
024404         MOVE W-IDARTNR                 TO SPAR-IDARTNR-ENTER             
024405       END-IF                                                             
024418                                                                          
024419       MOVE INV-KDINVKAT         TO SPAR-KDINVKAT-ENTER                   
024420       MOVE INV-TISEGKEY         TO SPAR-TISEGKEY-ENTER                   
024421       MOVE INV-DAREGDAT-SORT    TO SPAR-DAREGDAT-SORT-ENTER              
024422       MOVE W-IDARTNR            TO SPAR-IDARTNR-ENTER                    
024423                                    SPAR-IDARTNR-SEQ-ENTER                
024424       MOVE W-IDUSER             TO SPAR-IDUSER-ENTER                     
024425                                                                          
024427       MOVE +1 TO INDX                                                    
024428       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 14                          
024429       IF WS-KDINVKAT NOT = 12 AND                                        
024430          INV-FLINVBEH = 'N'                                              
024431       MOVE W-IDARTNR            TO CR1-IDARTNR                           
024432       MOVE WS-KDINVKAT          TO CR1-TYPE                              
024433       MOVE WS-DAREGDAT-CRE      TO WS-DATUM                              
024434       MOVE WS-DATUM4-9          TO CR1-DATE                              
024435       MOVE WS-IDUSER-CRE        TO CR1-IDUSER                            
024436       MOVE CREATED-RAD1         TO MOD-RAD-INFO(INDX)                    
024437                                                                          
024438       ADD  +1 TO INDX                                                    
024439       IF INDX < 15                                                       
024440         IF  WS-DAREGDAT-PR1 NUMERIC                                      
024441           MOVE WS-KDINVKAT          TO RAD1-TYPE                         
024442           MOVE WS-DAREGDAT-PR1      TO WS-DATUM                          
024443           MOVE WS-DATUM4-9          TO RAD1-DATE                         
024444           MOVE WS-IDUSER-PR1        TO RAD1-USER                         
024445           MOVE WS-DAREGDAT-PR1(3:6) TO WS-DATUM-TOM                      
024446           MOVE WS-DAREGDAT-CRE(3:6) TO WS-DATUM-FOM                      
024447                                                                          
024448           IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                       
024449             PERFORM S01-DATUM                                            
024450             MOVE WS-ANTALDAGAR      TO RAD1-WS-DATE                      
024451                                        RAD1-WS-DATETOT                   
024452           ELSE                                                           
024453             MOVE ZERO               TO RAD1-WS-DATE                      
024454                                        RAD1-WS-DATETOT                   
024455           END-IF                                                         
024456                                                                          
024459           MOVE PRINT-RAD1           TO MOD-RAD-INFO(INDX)                
024460           ADD  +1 TO INDX                                                
024461         END-IF                                                           
024462       END-IF                                                             
024463                                                                          
024464       IF INDX < 15                                                       
024465         IF  WS-DAREGDAT-PR2 NUMERIC                                      
024466           MOVE WS-KDINVKAT          TO RAD2-TYPE                         
024467           MOVE WS-DAREGDAT-PR2      TO WS-DATUM                          
024468           MOVE WS-DATUM4-9          TO RAD2-DATE                         
024469           MOVE WS-IDUSER-PR2        TO RAD2-USER                         
024470                                                                          
024471           MOVE WS-DAREGDAT-PR2(3:6) TO WS-DATUM-TOM                      
024472           MOVE WS-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                      
024473                                                                          
024474           IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                       
024475             PERFORM S01-DATUM                                            
024476             MOVE WS-ANTALDAGAR      TO RAD2-WS-DATE                      
024478           ELSE                                                           
024479             MOVE ZERO               TO RAD2-WS-DATE                      
024480           END-IF                                                         
024481                                                                          
024485           MOVE WS-DAREGDAT-PR2(3:6) TO WS-DATUM-TOM                      
024486           MOVE WS-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                      
024487                                                                          
024488           IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                       
024489             PERFORM S01-DATUM                                            
024490             MOVE WS-ANTALDAGAR      TO RAD2-WS-DATETOT                   
024491           ELSE                                                           
024492             MOVE ZERO               TO RAD2-WS-DATETOT                   
024493           END-IF                                                         
024497                                                                          
024498           MOVE PRINT-RAD2           TO MOD-RAD-INFO(INDX)                
024499           ADD  +1 TO INDX                                                
024500         END-IF                                                           
024501       END-IF                                                             
024502                                                                          
024503       IF INDX < 15                                                       
024504         IF WS-DAREGDAT-PR3 NUMERIC                                       
024505           MOVE WS-KDINVKAT          TO RAD3-TYPE                         
024506           MOVE WS-DAREGDAT-PR3      TO WS-DATUM                          
024507           MOVE WS-DATUM4-9          TO RAD3-DATE                         
024508           MOVE WS-IDUSER-PR3        TO RAD3-USER                         
024509                                                                          
024510           MOVE WS-DAREGDAT-PR3(3:6) TO WS-DATUM-TOM                      
024511           MOVE WS-DAREGDAT-PR2(3:6) TO WS-DATUM-FOM                      
024512                                                                          
024513           IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                       
024514             PERFORM S01-DATUM                                            
024515             MOVE WS-ANTALDAGAR      TO RAD3-WS-DATE                      
024516           ELSE                                                           
024517             MOVE ZERO               TO RAD3-WS-DATE                      
024518           END-IF                                                         
024520                                                                          
024521           MOVE WS-DAREGDAT-PR3(3:6) TO WS-DATUM-TOM                      
024522           MOVE WS-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                      
024523                                                                          
024524           IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                       
024525             PERFORM S01-DATUM                                            
024526             MOVE WS-ANTALDAGAR      TO RAD3-WS-DATETOT                   
024527           ELSE                                                           
024528             MOVE ZERO               TO RAD3-WS-DATETOT                   
024529           END-IF                                                         
024532                                                                          
024533           MOVE PRINT-RAD3           TO MOD-RAD-INFO(INDX)                
024534           ADD  +1 TO INDX                                                
024535         END-IF                                                           
024538       END-IF                                                             
024539       END-IF                                                             
024540       IF INDX < 15                                                       
024541         PERFORM FB-LAES-RADDATA                                          
024542       END-IF                                                             
024544       END-PERFORM                                                        
024545**     CALL FELLOG                                                        
024546       IF SEGMENT-FINNS                                                   
024547         IF SPAR-SOK-TYP = 'ARTIKEL   '                                   
024548           MOVE ZERO                 TO SPAR-KDINVKAT-NEXT                
024549           MOVE ZERO                 TO SPAR-TISEGKEY-NEXT                
024550           MOVE ZERO                 TO SPAR-DAREGDAT-SORT-NEXT           
024551           MOVE W-IDARTNR            TO SPAR-IDARTNR-NEXT                 
024552         ELSE                                                             
024553           IF SPAR-SOK-TYP = 'KDINVKAT  '                                 
024554             MOVE INV-KDINVKAT         TO SPAR-KDINVKAT-NEXT              
024555             MOVE INV-TISEGKEY         TO SPAR-TISEGKEY-NEXT              
024556             MOVE INV-DAREGDAT-SORT    TO SPAR-DAREGDAT-SORT-NEXT         
024557             MOVE ZERO                 TO SPAR-IDARTNR-NEXT               
024558           END-IF                                                         
024559           IF SPAR-SOK-TYP = 'DATUM     '                                 
024560             MOVE INV-KDINVKAT         TO SPAR-KDINVKAT-NEXT              
024561             MOVE INV-TISEGKEY         TO SPAR-TISEGKEY-NEXT              
024562             MOVE INV-DAREGDAT-SORT    TO SPAR-DAREGDAT-SORT-NEXT         
024563             MOVE ZERO                 TO SPAR-IDARTNR-NEXT               
024564           END-IF                                                         
024565           IF LAES-USERD OR LAES-USERF                                    
024566             MOVE INV-KDINVKAT         TO SPAR-KDINVKAT-NEXT              
024567             MOVE INV-TISEGKEY         TO SPAR-TISEGKEY-NEXT              
024568             MOVE INV-DAREGDAT-SORT    TO SPAR-DAREGDAT-SORT-NEXT         
024569             MOVE W-IDUSER             TO SPAR-IDUSER-NEXT                
024570           END-IF                                                         
024571         END-IF                                                           
024572         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
024573         CALL WMEDKONV USING MED-WMEDAREA                                 
024574         IF MOD-TEMFSINF  = ' '                                           
024575           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
024576         END-IF                                                           
024577       ELSE                                                               
024578         MOVE SPAR-KDINVKAT-ENTER        TO SPAR-KDINVKAT-NEXT            
024579         MOVE SPAR-TISEGKEY-ENTER        TO SPAR-TISEGKEY-NEXT            
024580         MOVE SPAR-DAREGDAT-SORT-ENTER TO                                 
024581                         SPAR-DAREGDAT-SORT-NEXT                          
024582         MOVE SPAR-IDARTNR-ENTER         TO SPAR-IDARTNR-NEXT             
024583                                            SPAR-IDARTNR-SEQ-NEXT         
024584         MOVE SPAR-IDUSER-ENTER          TO SPAR-IDUSER-NEXT              
024585         MOVE SPAR-KDSEGKEY-ENTER        TO SPAR-KDSEGKEY-NEXT            
024586         MOVE INF-LAST-PAGE        TO MED-IDMFSINF                        
024587         CALL WMEDKONV USING MED-WMEDAREA                                 
024588         IF MOD-TEMFSINF  = ' '                                           
024589           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
024590         END-IF                                                           
024591       END-IF                                                             
024592                                                                          
024593       MOVE '002'      TO MSGI-KDCALL                                     
024594       MOVE '5311'     TO SPAR-IDTRANS                                    
024595       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
024596       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
024597     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 FA-LAES-GRUNDDATA SECTION.                                               
024801     MOVE 'FA-LAES-GRUND'    TO WS-SECTION                                
024802     IF SPAR-SOK-TYP = 'ARTIKEL   '                                       
024803       PERFORM FAA-LAES-ARTIKEL                                           
024804     ELSE                                                                 
024805       IF SPAR-SOK-TYP = 'KDINVKAT  '                                     
024806         PERFORM FAB-LAES-KDINVKAT                                        
024807       ELSE                                                               
024808         IF SPAR-SOK-TYP = 'DATUM     '                                   
024809           PERFORM FAC-LAES-DATUM                                         
024810         ELSE                                                             
024811           IF LAES-USERF                                                  
024812             PERFORM FAD-LAES-USERF                                       
024813           ELSE                                                           
024814             IF LAES-USERD                                                
024815               PERFORM FAE-LAES-USERD                                     
024816             ELSE                                                         
024817               MOVE 'GE' TO STATUS-WS                                     
024818             END-IF                                                       
024819           END-IF                                                         
024820         END-IF                                                           
024821       END-IF                                                             
024822     END-IF                                                               
024823     .                                                                    
024824     EJECT                                                                
024825 FAA-LAES-ARTIKEL   SECTION.                                              
024826     MOVE 'FAA-LAES-ART '    TO WS-SECTION                                
024830     MOVE NEJ TO POST-SW                                                  
024900       PERFORM IMS-GET-WDH101                                             
025000       IF SEGMENT-FINNS                                                   
025010         MOVE ART-IDARTNR         TO W-IDARTNR                            
025100         PERFORM IMS-GNP-WDH111                                           
025101         IF SEGMENT-FINNS                                                 
025102           MOVE INV-DAREGDAT-CRE  TO WS-DAREGDAT-CRE                      
025103           MOVE INV-DAREGDAT-PR1  TO WS-DAREGDAT-PR1                      
025104           MOVE INV-DAREGDAT-PR2  TO WS-DAREGDAT-PR2                      
025105           MOVE INV-DAREGDAT-PR3  TO WS-DAREGDAT-PR3                      
025106           MOVE INV-KDINVKAT      TO WS-KDINVKAT                          
025107                                     W-KDINVKAT-UNIK                      
025108           MOVE INV-TISEGKEY        TO W-TISEGKEY-UNIK                    
025109           MOVE INV-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                 
025110                                                                          
025112           PERFORM IMS-GNP-WDH121                                         
025113           MOVE  +1   TO WS-ANTAL-POST                                    
025114           MOVE  ZERO TO WS-KDSEGKEY                                      
025115           IF SEGMENT-FINNS                                               
025116             PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4            
025117               IF INVL-KDSEGKEY = '0'                                     
025118                 MOVE INVL-IDUSER TO WS-IDUSER-CRE                        
025119               END-IF                                                     
025120               IF INVL-KDSEGKEY = '1'                                     
025121                 MOVE INVL-IDUSER TO WS-IDUSER-PR1                        
025122               END-IF                                                     
025123               IF INVL-KDSEGKEY = '2'                                     
025124                 MOVE INVL-IDUSER TO WS-IDUSER-PR2                        
025125               END-IF                                                     
025126               IF INVL-KDSEGKEY = '3'                                     
025127                 MOVE INVL-IDUSER TO WS-IDUSER-PR3                        
025128                 MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                     
025129                 ADD +4 TO WS-ANTAL-POST                                  
025130               END-IF                                                     
025131               IF WS-KDSEGKEY < 3                                         
025132                 PERFORM IMS-GNP-WDH121                                   
025133               END-IF                                                     
025134               ADD +1 TO WS-ANTAL-POST                                    
025135             END-PERFORM                                                  
025136           ELSE                                                           
025137             MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                        
025138             MOVE SPACE           TO WS-IDUSER-CRE                        
025139             MOVE SPACE           TO WS-IDUSER-PR1                        
025140             MOVE SPACE           TO WS-IDUSER-PR2                        
025141             MOVE SPACE           TO WS-IDUSER-PR3                        
025142           END-IF                                                         
025143         ELSE                                                             
025144           MOVE KEYS-ARE-MISSING  TO MED-IDMFSFEL                         
025145         END-IF                                                           
025146       ELSE                                                               
025147         MOVE KEYS-ARE-MISSING    TO MED-IDMFSFEL                         
025148       END-IF                                                             
025149     .                                                                    
025150     EJECT                                                                
025151 FAB-LAES-KDINVKAT  SECTION.                                              
025152       MOVE 'FAB-LAES-KDINV'   TO WS-SECTION                              
025153       IF MFS-NEXT OR MFS-ENTER                                           
025154         PERFORM IMS-GU-WDH1C1                                            
025155       ELSE                                                               
025156         PERFORM IMS-GN-WDH1C1                                            
025157       END-IF                                                             
025158       IF SEGMENT-FINNS                                                   
025159         MOVE SEQC-IDARTNR          TO W-IDARTNR                          
025160                                       W-IDARTNR                          
025161                                       SPAR-IDARTNR-SEQ-ENTER             
025162         MOVE SEQC-TISEGKEY         TO W-TISEGKEY                         
025163                                       W-TISEGKEY-UNIK                    
025164         MOVE SEQC-DAREGDAT-SORT    TO W-DAREGDAT-SORT-UNIK               
025165                                                                          
025166         MOVE SEQC-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                    
025167         MOVE SEQC-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                    
025168         MOVE SEQC-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                    
025169         MOVE SEQC-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                    
025170                                                                          
025171         PERFORM IMS-GU-WDH111                                            
025172         IF SEGMENT-FINNS                                                 
025173           PERFORM IMS-GNP-WDH121-UNIK                                    
025174           MOVE  +1   TO WS-ANTAL-POST                                    
025175           MOVE  ZERO TO WS-KDSEGKEY                                      
025176           PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4              
025177             IF INVL-KDSEGKEY = '0'                                       
025178               MOVE INVL-IDUSER TO WS-IDUSER-CRE                          
025179             END-IF                                                       
025180             IF INVL-KDSEGKEY = '1'                                       
025181               MOVE INVL-IDUSER TO WS-IDUSER-PR1                          
025182             END-IF                                                       
025183             IF INVL-KDSEGKEY = '2'                                       
025184               MOVE INVL-IDUSER TO WS-IDUSER-PR2                          
025185             END-IF                                                       
025186             IF INVL-KDSEGKEY = '3'                                       
025187               MOVE INVL-IDUSER      TO WS-IDUSER-PR3                     
025188               MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                       
025189               ADD +4 TO WS-ANTAL-POST                                    
025190             END-IF                                                       
025191             IF WS-KDSEGKEY < 3                                           
025192               PERFORM IMS-GNP-WDH121-UNIK                                
025193             END-IF                                                       
025194             ADD +1 TO WS-ANTAL-POST                                      
025195           END-PERFORM                                                    
025196         ELSE                                                             
025197           MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                          
025198         END-IF                                                           
025199       ELSE                                                               
025200         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
025201       END-IF                                                             
025202     IF SEGMENT-SAKNAS                                                    
025210       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
025300     END-IF                                                               
025400     .                                                                    
025501     EJECT                                                                
025514 FAC-LAES-DATUM SECTION.                                                  
025515     MOVE 'FAC-LAES-DATUM'   TO WS-SECTION                                
025516     IF MFS-NEXT  OR MFS-ENTER                                            
025517       PERFORM IMS-GU-WDH1E1                                              
025518     ELSE                                                                 
025519       PERFORM IMS-GN-WDH1E1                                              
025520     END-IF                                                               
025521                                                                          
025522     IF SEGMENT-FINNS                                                     
025523       MOVE SEQE-IDARTNR          TO W-IDARTNR                            
025524                                     W-IDARTNR                            
025525                                     SPAR-IDARTNR-SEQ-ENTER               
025526       MOVE SEQE-TISEGKEY         TO W-TISEGKEY                           
025527                                     W-TISEGKEY-UNIK                      
025528       MOVE SEQE-DAREGDAT-SORT    TO W-DAREGDAT-SORT-UNIK                 
025529       MOVE SEQE-KDINVKAT         TO W-KDINVKAT-UNIK                      
025530                                     WS-KDINVKAT                          
025531                                                                          
025532       MOVE SEQE-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                      
025533       MOVE SEQE-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                      
025534       MOVE SEQE-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                      
025535       MOVE SEQE-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                      
025536                                                                          
025537       PERFORM IMS-GU-WDH111                                              
025538       IF SEGMENT-FINNS                                                   
025539         PERFORM IMS-GNP-WDH121-UNIK                                      
025540         MOVE  +1   TO WS-ANTAL-POST                                      
025541         MOVE  ZERO TO WS-KDSEGKEY                                        
025542         PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4                
025543           IF INVL-KDSEGKEY = '0'                                         
025544             MOVE INVL-IDUSER TO WS-IDUSER-CRE                            
025545           END-IF                                                         
025546           IF INVL-KDSEGKEY = '1'                                         
025547             MOVE INVL-IDUSER TO WS-IDUSER-PR1                            
025548           END-IF                                                         
025549           IF INVL-KDSEGKEY = '2'                                         
025550             MOVE INVL-IDUSER TO WS-IDUSER-PR2                            
025551           END-IF                                                         
025552           IF INVL-KDSEGKEY = '3'                                         
025553             MOVE INVL-IDUSER      TO WS-IDUSER-PR3                       
025554             MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                         
025555             ADD +4 TO WS-ANTAL-POST                                      
025556           END-IF                                                         
025557           IF WS-KDSEGKEY < 3                                             
025558             PERFORM IMS-GNP-WDH121-UNIK                                  
025559           END-IF                                                         
025560           ADD +1 TO WS-ANTAL-POST                                        
025561         END-PERFORM                                                      
025562       ELSE                                                               
025563         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
025564       END-IF                                                             
025565     END-IF                                                               
025566     .                                                                    
025567     EJECT                                                                
025568 FAD-LAES-USERF SECTION.                                                  
025569     MOVE 'FAD-LAES-USERF SECTION' TO WS-SECTION                          
025570     MOVE W-IDDC TO W-SEQF-IDDC                                           
025571     MOVE W-IDDC TO W-SEQF-IDDC-MIN                                       
025572     MOVE W-IDDC TO W-SEQF-IDDC-MAX                                       
025573                                                                          
025574     IF MFS-NEXT OR MFS-ENTER                                             
025575       PERFORM IMS-GU-WDH1F1                                              
025576     ELSE                                                                 
025577       PERFORM IMS-GN-WDH1F1                                              
025578     END-IF                                                               
025579     MOVE 'FAD-GN-WDH1F1'    TO WS-SECTION                                
025580     IF SEGMENT-FINNS                                                     
025581**   CALL FELLOG                                                          
025582       MOVE SEQF-IDARTNR         TO W-IDARTNR                             
025583                                                                          
025584                                    SPAR-IDARTNR-SEQ-ENTER                
025585       MOVE SEQF-KDINVKAT        TO W-KDINVKAT-UNIK                       
025586                                    WS-KDINVKAT                           
025587       MOVE SEQF-TISEGKEY        TO W-TISEGKEY-UNIK                       
025588       MOVE SEQF-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                    
025589       MOVE SEQF-KDSEGKEY        TO SPAR-KDSEGKEY-ENTER                   
025590       PERFORM IMS-GU-WDH111                                              
025592       MOVE 'FAD-GU-WDH111'    TO WS-SECTION                              
025593       IF SEGMENT-FINNS                                                   
025594         MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                     
025595         MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                     
025596         MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                     
025597         MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                     
025598         PERFORM IMS-GNP-WDH121-UNIK                                      
025599         MOVE 'FAD-GNP-WDH121'   TO WS-SECTION                            
025600         MOVE  +1   TO WS-ANTAL-POST                                      
025601         MOVE  ZERO TO WS-KDSEGKEY                                        
025602         PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4                
025603           IF INVL-KDSEGKEY = '0'                                         
025604             MOVE INVL-IDUSER TO WS-IDUSER-CRE                            
025605           END-IF                                                         
025606           IF INVL-KDSEGKEY = '1'                                         
025607             MOVE INVL-IDUSER TO WS-IDUSER-PR1                            
025608           END-IF                                                         
025609           IF INVL-KDSEGKEY = '2'                                         
025610             MOVE INVL-IDUSER TO WS-IDUSER-PR2                            
025611           END-IF                                                         
025612           IF INVL-KDSEGKEY = '3'                                         
025613             MOVE INVL-IDUSER      TO WS-IDUSER-PR3                       
025614             MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                         
025615             ADD +4 TO WS-ANTAL-POST                                      
025616           END-IF                                                         
025617           IF WS-KDSEGKEY < 3                                             
025618             PERFORM IMS-GNP-WDH121-UNIK                                  
025619           END-IF                                                         
025620           ADD +1 TO WS-ANTAL-POST                                        
025621         END-PERFORM                                                      
025622       ELSE                                                               
025623         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
025624       END-IF                                                             
025625     ELSE                                                                 
025626       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
025627     END-IF                                                               
025628     .                                                                    
025629     EJECT                                                                
025630 FAE-LAES-USERD SECTION.                                                  
025631     MOVE 'FAE-LAES-USERD SECTION' TO WS-SECTION                          
025632     MOVE W-IDDC TO W-SEQD-IDDC                                           
025633     MOVE W-IDDC TO W-SEQD-IDDC-MIN                                       
025634     MOVE W-IDDC TO W-SEQD-IDDC-MAX                                       
025635                                                                          
025636     IF MFS-NEXT OR MFS-ENTER                                             
025637       PERFORM IMS-GU-WDH1D1                                              
025638     ELSE                                                                 
025639       PERFORM IMS-GN-WDH1D1                                              
025640     END-IF                                                               
025641     MOVE 'FAD-GN-WDH1D1'    TO WS-SECTION                                
025642     IF SEGMENT-FINNS                                                     
025643       MOVE SEQD-IDARTNR         TO W-IDARTNR                             
025644                                                                          
025645                                    SPAR-IDARTNR-SEQ-ENTER                
025646       MOVE SEQD-KDINVKAT        TO W-KDINVKAT-UNIK                       
025647                                    WS-KDINVKAT                           
025648       MOVE SEQD-TISEGKEY        TO W-TISEGKEY-UNIK                       
025649       MOVE SEQD-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                    
025650       MOVE SEQD-KDSEGKEY        TO SPAR-KDSEGKEY-ENTER                   
025651       PERFORM IMS-GU-WDH111                                              
025652       MOVE 'FAD-GU-WDH111'    TO WS-SECTION                              
025653       IF SEGMENT-FINNS                                                   
025654         MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                     
025655         MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                     
025656         MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                     
025657         MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                     
025658         PERFORM IMS-GNP-WDH121-UNIK                                      
025659         MOVE 'FAD-GNP-WDH121'   TO WS-SECTION                            
025660         MOVE  +1   TO WS-ANTAL-POST                                      
025661         MOVE  ZERO TO WS-KDSEGKEY                                        
025662         PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4                
025663           IF INVL-KDSEGKEY = '0'                                         
025664             MOVE INVL-IDUSER TO WS-IDUSER-CRE                            
025665           END-IF                                                         
025666           IF INVL-KDSEGKEY = '1'                                         
025667             MOVE INVL-IDUSER TO WS-IDUSER-PR1                            
025668           END-IF                                                         
025669           IF INVL-KDSEGKEY = '2'                                         
025670             MOVE INVL-IDUSER TO WS-IDUSER-PR2                            
025671           END-IF                                                         
025672           IF INVL-KDSEGKEY = '3'                                         
025673             MOVE INVL-IDUSER      TO WS-IDUSER-PR3                       
025674             MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                         
025675             ADD +4 TO WS-ANTAL-POST                                      
025676           END-IF                                                         
025677           IF WS-KDSEGKEY < 3                                             
025678             PERFORM IMS-GNP-WDH121-UNIK                                  
025679           END-IF                                                         
025680           ADD +1 TO WS-ANTAL-POST                                        
025681         END-PERFORM                                                      
025682       ELSE                                                               
025683         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
025684       END-IF                                                             
025685     ELSE                                                                 
025686       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
025687     END-IF                                                               
025688     .                                                                    
025689     EJECT                                                                
025690 FB-LAES-RADDATA SECTION.                                                 
025691     MOVE 'FB-LAES-RADDA'    TO WS-SECTION                                
025692     IF SPAR-SOK-TYP = 'ARTIKEL   '                                       
025693       PERFORM FBA-LAES-ARTIKEL                                           
025694     ELSE                                                                 
025695       IF SPAR-SOK-TYP = 'KDINVKAT  '                                     
025696         PERFORM FBB-LAES-KDINVKAT                                        
025697       ELSE                                                               
025698         IF SPAR-SOK-TYP = 'DATUM     '                                   
025699           PERFORM FBC-LAES-DATUM                                         
025700         ELSE                                                             
025701           IF LAES-USERF                                                  
025702             PERFORM FBD-LAES-USERF                                       
025703           ELSE                                                           
025704             IF LAES-USERD                                                
025705               PERFORM FBE-LAES-USERD                                     
025706             END-IF                                                       
025707           END-IF                                                         
025708         END-IF                                                           
025709       END-IF                                                             
025710     END-IF                                                               
025711     .                                                                    
025712     EJECT                                                                
025713 FBA-LAES-ARTIKEL SECTION.                                                
025714     MOVE 'FBA-LAES-ART'    TO WS-SECTION                                 
025715     MOVE NEJ TO POST-SW                                                  
025716       PERFORM IMS-GNP-WDH111                                             
025717         MOVE INV-KDINVKAT      TO WS-KDINVKAT                            
025718         IF SEGMENT-FINNS                                                 
025719           PERFORM IMS-GNP-WDH121                                         
025720           IF SEGMENT-FINNS                                               
025721             PERFORM UNTIL SEGMENT-SAKNAS                                 
025722               IF INVL-KDSEGKEY = '0'                                     
025723                 MOVE INVL-IDUSER TO WS-IDUSER-CRE                        
025724               END-IF                                                     
025725               IF INVL-KDSEGKEY = '1'                                     
025726                 MOVE INVL-IDUSER TO WS-IDUSER-PR1                        
025727               END-IF                                                     
025728               IF INVL-KDSEGKEY = '2'                                     
025729                 MOVE INVL-IDUSER TO WS-IDUSER-PR2                        
025730               END-IF                                                     
025731               IF INVL-KDSEGKEY = '3'                                     
025732                 MOVE INVL-IDUSER TO WS-IDUSER-PR3                        
025733               END-IF                                                     
025734               PERFORM IMS-GNP-WDH121                                     
025735             END-PERFORM                                                  
025736             MOVE '  ' TO STATUS-WS                                       
025737           ELSE                                                           
025738             MOVE SPACE           TO WS-IDUSER-CRE                        
025739             MOVE SPACE           TO WS-IDUSER-PR1                        
025740             MOVE SPACE           TO WS-IDUSER-PR2                        
025741             MOVE SPACE           TO WS-IDUSER-PR3                        
025742           END-IF                                                         
025743         END-IF                                                           
025744     .                                                                    
025745     EJECT                                                                
025746 FBB-LAES-KDINVKAT SECTION.                                               
025747     MOVE 'FBB-LAES-KDINV'    TO WS-SECTION                               
025748       PERFORM IMS-GN-WDH1C1                                              
025749       IF SEGMENT-FINNS                                                   
025750         MOVE SEQC-IDARTNR         TO W-IDARTNR                           
025751                                      W-IDARTNR                           
025752                                      SPAR-IDARTNR-SEQ-NEXT               
025753         MOVE SEQC-TISEGKEY        TO W-TISEGKEY-UNIK                     
025754         MOVE SEQC-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                  
025755                                                                          
025756         MOVE SEQC-DAREGDAT-CRE    TO WS-DAREGDAT-CRE                     
025757         MOVE SEQC-DAREGDAT-PR1    TO WS-DAREGDAT-PR1                     
025758         MOVE SEQC-DAREGDAT-PR2    TO WS-DAREGDAT-PR2                     
025759         MOVE SEQC-DAREGDAT-PR3    TO WS-DAREGDAT-PR3                     
025760                                                                          
025761         PERFORM IMS-GU-WDH111                                            
025762         IF SEGMENT-FINNS                                                 
025763         MOVE INV-KDINVKAT      TO WS-KDINVKAT                            
025764         PERFORM IMS-GNP-WDH121-UNIK                                      
025765         MOVE +1 TO WS-ANTAL-POST                                         
025766         MOVE ZERO TO WS-KDSEGKEY                                         
025767           PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4              
025768             IF INVL-KDSEGKEY = '0'                                       
025769               MOVE INVL-IDUSER     TO WS-IDUSER-CRE                      
025770             END-IF                                                       
025771             IF INVL-KDSEGKEY = '1'                                       
025772               MOVE INVL-IDUSER     TO WS-IDUSER-PR1                      
025773             END-IF                                                       
025774             IF INVL-KDSEGKEY = '2'                                       
025775               MOVE INVL-IDUSER     TO WS-IDUSER-PR2                      
025776             END-IF                                                       
025777             IF INVL-KDSEGKEY = '3'                                       
025778               MOVE INVL-IDUSER     TO WS-IDUSER-PR3                      
025779               MOVE INVL-KDSEGKEY   TO WS-KDSEGKEY                        
025780               ADD +4 TO WS-ANTAL-POST                                    
025781             END-IF                                                       
025782             IF WS-KDSEGKEY < 3                                           
025783               PERFORM IMS-GNP-WDH121-UNIK                                
025784             END-IF                                                       
025785             ADD +1 TO WS-ANTAL-POST                                      
025786           END-PERFORM                                                    
025787         ELSE                                                             
025788           MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                          
025789         END-IF                                                           
025790       ELSE                                                               
025791         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
025792       END-IF                                                             
025793     .                                                                    
025794     EJECT                                                                
025795 FBC-LAES-DATUM    SECTION.                                               
025796       MOVE 'FBC-LAES-DATUM'    TO WS-SECTION                             
025797       PERFORM IMS-GN-WDH1E1                                              
025798       IF SEGMENT-FINNS                                                   
025799         MOVE SEQE-IDARTNR         TO W-IDARTNR                           
025800                                      W-IDARTNR                           
025801                                      SPAR-IDARTNR-SEQ-NEXT               
025802         MOVE SEQE-TISEGKEY        TO W-TISEGKEY-UNIK                     
025803         MOVE SEQE-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                  
025804         MOVE SEQE-KDINVKAT        TO WS-KDINVKAT                         
025805                                      W-KDINVKAT-UNIK                     
025806                                                                          
025807         MOVE SEQE-DAREGDAT-CRE    TO WS-DAREGDAT-CRE                     
025808         MOVE SEQE-DAREGDAT-PR1    TO WS-DAREGDAT-PR1                     
025809         MOVE SEQE-DAREGDAT-PR2    TO WS-DAREGDAT-PR2                     
025810         MOVE SEQE-DAREGDAT-PR3    TO WS-DAREGDAT-PR3                     
025811                                                                          
025812         PERFORM IMS-GU-WDH111                                            
025813         IF SEGMENT-FINNS                                                 
025814         MOVE INV-KDINVKAT      TO WS-KDINVKAT                            
025815         PERFORM IMS-GNP-WDH121-UNIK                                      
025816         MOVE +1 TO WS-ANTAL-POST                                         
025817         MOVE ZERO TO WS-KDSEGKEY                                         
025818           PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4              
025819             IF INVL-KDSEGKEY = '0'                                       
025820               MOVE INVL-IDUSER     TO WS-IDUSER-CRE                      
025821             END-IF                                                       
025822             IF INVL-KDSEGKEY = '1'                                       
025823               MOVE INVL-IDUSER     TO WS-IDUSER-PR1                      
025824             END-IF                                                       
025825             IF INVL-KDSEGKEY = '2'                                       
025826               MOVE INVL-IDUSER     TO WS-IDUSER-PR2                      
025827             END-IF                                                       
025828             IF INVL-KDSEGKEY = '3'                                       
025829               MOVE INVL-IDUSER     TO WS-IDUSER-PR3                      
025830               MOVE INVL-KDSEGKEY   TO WS-KDSEGKEY                        
025831               ADD +4 TO WS-ANTAL-POST                                    
025832             END-IF                                                       
025833             IF WS-KDSEGKEY < 3                                           
025834               PERFORM IMS-GNP-WDH121-UNIK                                
025835             END-IF                                                       
025836             ADD +1 TO WS-ANTAL-POST                                      
025837           END-PERFORM                                                    
025838         ELSE                                                             
025839           MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                          
025840         END-IF                                                           
025841       ELSE                                                               
025842         MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                            
025843       END-IF                                                             
025844     .                                                                    
025845     EJECT                                                                
025846 FBD-LAES-USERF SECTION.                                                  
025847     MOVE 'FBD-LAES-USERF'   TO WS-SECTION                                
025848     PERFORM IMS-GN-WDH1F1                                                
025849                                                                          
025850     PERFORM UNTIL SEGMENT-SAKNAS OR W-IDARTNR NOT = SEQF-IDARTNR         
025851       PERFORM IMS-GN-WDH1F1                                              
025852     END-PERFORM                                                          
025853                                                                          
025854     IF SEGMENT-FINNS AND W-IDARTNR NOT = SEQF-IDARTNR                    
025856       MOVE SEQF-IDARTNR         TO W-IDARTNR                             
025857                                    W-IDARTNR                             
025858                                                                          
025859                                    SPAR-IDARTNR-SEQ-NEXT                 
025860       MOVE SEQF-KDINVKAT        TO W-KDINVKAT-UNIK                       
025861                                    WS-KDINVKAT                           
025862       MOVE SEQF-TISEGKEY        TO W-TISEGKEY-UNIK                       
025863       MOVE SEQF-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                    
025864       MOVE SEQF-KDSEGKEY        TO SPAR-KDSEGKEY-NEXT                    
025865       PERFORM IMS-GU-WDH111                                              
025866       MOVE 'FBD-GU-WDH111'    TO WS-SECTION                              
025867*    CALL FELLOG                                                          
025868       IF SEGMENT-FINNS                                                   
025869         MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                     
025870         MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                     
025871         MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                     
025872         MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                     
025873         MOVE INV-KDINVKAT         TO WS-KDINVKAT                         
025874         PERFORM IMS-GNP-WDH121-UNIK                                      
025875         MOVE 'FBD-GNP-WDH121'    TO WS-SECTION                           
025876         MOVE  +1   TO WS-ANTAL-POST                                      
025877         MOVE  ZERO TO WS-KDSEGKEY                                        
025878         PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4                
025879           IF INVL-KDSEGKEY = '0'                                         
025880             MOVE INVL-IDUSER TO WS-IDUSER-CRE                            
025881           END-IF                                                         
025882           IF INVL-KDSEGKEY = '1'                                         
025883             MOVE INVL-IDUSER TO WS-IDUSER-PR1                            
025884           END-IF                                                         
025885           IF INVL-KDSEGKEY = '2'                                         
025886             MOVE INVL-IDUSER TO WS-IDUSER-PR2                            
025887           END-IF                                                         
025888           IF INVL-KDSEGKEY = '3'                                         
025889             MOVE INVL-IDUSER      TO WS-IDUSER-PR3                       
025890             MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                         
025891             ADD +4 TO WS-ANTAL-POST                                      
025892           END-IF                                                         
025893           IF WS-KDSEGKEY < 3                                             
025894             PERFORM IMS-GNP-WDH121-UNIK                                  
025895           END-IF                                                         
025896           ADD +1 TO WS-ANTAL-POST                                        
025897         END-PERFORM                                                      
025898       END-IF                                                             
025899     ELSE                                                                 
025900       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
025901     END-IF                                                               
025902     .                                                                    
025903     EJECT                                                                
025904 FBE-LAES-USERD SECTION.                                                  
025905     MOVE 'FBE-LAES-USERD'   TO WS-SECTION                                
025906     PERFORM IMS-GN-WDH1D1                                                
025907                                                                          
025908     PERFORM UNTIL SEGMENT-SAKNAS OR W-IDARTNR NOT = SEQD-IDARTNR         
025909       PERFORM IMS-GN-WDH1D1                                              
025910     END-PERFORM                                                          
025911                                                                          
025912     IF SEGMENT-FINNS AND W-IDARTNR NOT = SEQD-IDARTNR                    
025913       MOVE SEQD-IDARTNR         TO W-IDARTNR                             
025914                                    W-IDARTNR                             
025915                                                                          
025916                                    SPAR-IDARTNR-SEQ-NEXT                 
025917       MOVE SEQD-KDINVKAT        TO W-KDINVKAT-UNIK                       
025918                                    WS-KDINVKAT                           
025919       MOVE SEQD-TISEGKEY        TO W-TISEGKEY-UNIK                       
025920       MOVE SEQD-DAREGDAT-SORT TO W-DAREGDAT-SORT-UNIK                    
025921       MOVE SEQD-KDSEGKEY        TO SPAR-KDSEGKEY-NEXT                    
025922       PERFORM IMS-GU-WDH111                                              
025923       MOVE 'FBD-GU-WDH111'    TO WS-SECTION                              
025924       IF SEGMENT-FINNS                                                   
025925         MOVE INV-DAREGDAT-CRE     TO WS-DAREGDAT-CRE                     
025926         MOVE INV-DAREGDAT-PR1     TO WS-DAREGDAT-PR1                     
025927         MOVE INV-DAREGDAT-PR2     TO WS-DAREGDAT-PR2                     
025928         MOVE INV-DAREGDAT-PR3     TO WS-DAREGDAT-PR3                     
025929         MOVE INV-KDINVKAT         TO WS-KDINVKAT                         
025930         PERFORM IMS-GNP-WDH121-UNIK                                      
025931         MOVE 'FBD-GNP-WDH121'    TO WS-SECTION                           
025932         MOVE  +1   TO WS-ANTAL-POST                                      
025933         MOVE  ZERO TO WS-KDSEGKEY                                        
025934         PERFORM UNTIL SEGMENT-SAKNAS OR WS-ANTAL-POST > 4                
025935           IF INVL-KDSEGKEY = '0'                                         
025936             MOVE INVL-IDUSER TO WS-IDUSER-CRE                            
025937           END-IF                                                         
025938           IF INVL-KDSEGKEY = '1'                                         
025939             MOVE INVL-IDUSER TO WS-IDUSER-PR1                            
025940           END-IF                                                         
025941           IF INVL-KDSEGKEY = '2'                                         
025942             MOVE INVL-IDUSER TO WS-IDUSER-PR2                            
025943           END-IF                                                         
025944           IF INVL-KDSEGKEY = '3'                                         
025945             MOVE INVL-IDUSER      TO WS-IDUSER-PR3                       
025946             MOVE INVL-KDSEGKEY    TO WS-KDSEGKEY                         
025947             ADD +4 TO WS-ANTAL-POST                                      
025948           END-IF                                                         
025949           IF WS-KDSEGKEY < 3                                             
025950             PERFORM IMS-GNP-WDH121-UNIK                                  
025951           END-IF                                                         
025952           ADD +1 TO WS-ANTAL-POST                                        
025953         END-PERFORM                                                      
025954       END-IF                                                             
025955     ELSE                                                                 
025956       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
025957     END-IF                                                               
025958     .                                                                    
025959     EJECT                                                                
025960 G-UPDATE SECTION.                                                        
025961     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
025962       MOVE MID-KDARBTYP-IN   TO MOD-KDARBTYP-UT                          
025963                                 W-KDARBTYP                               
025964     ELSE                                                                 
025965       MOVE NEJ               TO INDATA-SW                                
025966       MOVE MFS-RENSA-FAELT   TO MOD-KDARBTYP-UT                          
025967     END-IF                                                               
025968     IF MID-IDPERSON-IN NOT = ALL '+'                                     
025969       MOVE MID-IDPERSON-IN   TO MOD-IDPERSON-UT                          
025970                                 W-IDPERSON                               
025971     ELSE                                                                 
025972       MOVE NEJ               TO INDATA-SW                                
025973       MOVE MFS-RENSA-FAELT   TO MOD-IDPERSON-UT                          
025974     END-IF                                                               
025975                                                                          
025976     IF INDATA-OK                                                         
025977       PERFORM IMS-GET-WDP301                                             
025978       IF SEGMENT-FINNS                                                   
025979         MOVE MFS-ALFA-FAELT-RAETT     TO MOD-KDARBTYP-ATTR               
025980         PERFORM IMS-GET-WDP311                                           
025981         IF SEGMENT-FINNS                                                 
025982           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDPERSON-ATTR               
025983           MOVE PERS-IDMAIL            TO IDMAIL                          
025984           MOVE WS-SOK-TYP             TO URV-SOK-TYP                     
025985           IF LAES-ARTIKEL                                                
025986             MOVE SPAR-IDARTNR-MID     TO URV-IDARTNR                     
025987           END-IF                                                         
025988                                                                          
025989           IF LAES-KDINVKAT                                               
025990             MOVE SPAR-KDINVKAT-MID    TO URV-KDINVKAT                    
025991             IF SPAR-DATUM-MID   > 0 AND SPAR-DATUM-MID   NUMERIC         
025992               MOVE SPAR-DATUM-MID     TO URV-DATUM                       
025993             END-IF                                                       
025994           END-IF                                                         
025995           IF LAES-DATUM                                                  
025996             MOVE SPAR-DATUM-MID       TO URV-DATUM                       
025997           END-IF                                                         
025998           IF LAES-USERF OR LAES-USERD                                    
025999             MOVE SPAR-IDUSER-MID      TO URV-IDUSER                      
026000            IF SPAR-KDINVKAT-MID > 0                                      
026001              MOVE SPAR-KDINVKAT-MID   TO URV-KDINVKAT                    
026002            END-IF                                                        
026003            IF SPAR-DATUM-MID > 0                                         
026004              MOVE SPAR-DATUM-MID       TO URV-DATUM                      
026005            END-IF                                                        
026006           END-IF                                                         
026007                                                                          
026008           MOVE W-IDDC                 TO URV-IDDC                        
026009**         MOVE SPAR-SEQA-KDINVKAT-MIN TO URV-KDINVKAT                    
026010           MOVE SPAR-IDUSER-ENTER      TO URV-IDUSER                      
026011           MOVE 'WDH1'                 TO URV-BAS                         
026012********   STARTA SOP ***********                                         
026013                                                                          
026014           MOVE '5311'   TO MSGSOP-IDTRANS                                
026015           MOVE '1'      TO MSGSOP-KDMFSFOR                               
026016           MOVE 'O'      TO MSGSOP-KDSOPFUNK                              
026017           MOVE 'W513S1' TO MSGSOP-IDPROCESS                              
026018                                                                          
026019           STRING 'IDUSER(' MSG-SIGNON-USERID ') URVAL1('                 
026020                            WS-URVAL1 ') MAIL('                           
026021                            WS-IDMAIL ')'                                 
026022                   DELIMITED BY SIZE INTO MSGSOP-TESYMBV                  
026023                                                                          
026024                   MOVE  MAILSEND     TO MOD-TEMFSINF                     
026025           PERFORM IMS-INSERT-ALTMSG                                      
026026           MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                        
026027           MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-UT                        
026028           MOVE ALL '+'         TO MID-KDARBTYP-IN                        
026029                                   MID-IDPERSON-IN                        
026030         ELSE                                                             
026031           MOVE NEJ TO INDATA-SW                                          
026032           MOVE MID-IDPERSON-IN           TO MOD-IDPERSON-UT              
026033           MOVE ERR-WRONG-KEY             TO MED-IDMFSINF                 
026034           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDPERSON-ATTR            
026035           MOVE MFS-ALFA-FAELT-RAETT      TO MOD-KDARBTYP-ATTR            
026036         END-IF                                                           
026037       ELSE                                                               
026038         MOVE NEJ TO INDATA-SW                                            
026039         MOVE MID-KDARBTYP-IN             TO MOD-KDARBTYP-UT              
026040         MOVE ERR-WRONG-KEY               TO MED-IDMFSINF                 
026041         MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDARBTYP-ATTR            
026042         MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDPERSON-ATTR            
026043       END-IF                                                             
026044     ELSE                                                                 
026045       MOVE ERR-WRONG-KEY                 TO MED-IDMFSINF                 
026046                                                                          
026047     END-IF                                                               
026048                                                                          
026049     .                                                                    
026050     EJECT                                                                
026051 S01-DATUM SECTION.                                                       
026052     MOVE ZERO               TO WS-ANTALDAGAR                             
026053     MOVE WS-DATUM-FOM       TO WORK-TIAAMMDD-FOM                         
026054     MOVE WS-DATUM-TOM       TO WORK-TIAAMMDD-TOM                         
026055     MOVE W-IDDC             TO WORK-IDDC                                 
026056                                                                          
026057     MOVE 001                TO WORK-KDCALL                               
026058                                                                          
026059     CALL WORKDAY  USING  WORK-KDCALL  WORK-DATE-AREA                     
026060                          WORK-KDSVAR                                     
026061     IF WORK-KDSVAR = SPACE                                               
026062       MOVE WORK-KVWORKD     TO WS-ANTALDAGAR                             
026063     END-IF                                                               
026064                                                                          
026065     .                                                                    
026066     EJECT                                                                
026067 S02-NOLLA-SPAR-AREA SECTION.                                             
026068     MOVE ZERO  TO SPAR-IDARTNR-ENTER                                     
026069     MOVE ZERO  TO SPAR-IDARTNR-NEXT                                      
026070     MOVE ZERO  TO SPAR-KDINVKAT-ENTER                                    
026071     MOVE ZERO  TO SPAR-KDINVKAT-NEXT                                     
026072     MOVE ZERO  TO SPAR-SEQF-KDINVKAT                                     
026073     MOVE ZERO  TO SPAR-DATUM-ENTER                                       
026074     MOVE ZERO  TO SPAR-DATUM-NEXT                                        
026075     MOVE SPACE TO SPAR-IDUSER-ENTER                                      
026076     MOVE SPACE TO SPAR-IDUSER-NEXT                                       
026077     MOVE ZERO  TO SPAR-TISEGKEY-ENTER                                    
026078     MOVE ZERO  TO SPAR-TISEGKEY-NEXT                                     
026079     MOVE ZERO  TO SPAR-DAREGDAT-SORT-ENTER                               
026080     MOVE ZERO  TO SPAR-DAREGDAT-SORT-NEXT                                
026081     MOVE ZERO  TO SPAR-SEQF-DAREGDAT-SORT                                
026082     MOVE ZERO  TO SPAR-SEQD-DAREGDAT-SORT                                
026083     MOVE ZERO  TO SPAR-IDARTNR-SEQ-ENTER                                 
026084     MOVE ZERO  TO SPAR-IDARTNR-SEQ-NEXT                                  
026085     MOVE ZERO  TO SPAR-KDSEGKEY-ENTER                                    
026086     MOVE ZERO  TO SPAR-KDSEGKEY-NEXT                                     
026087     MOVE ZERO  TO SPAR-IDARTNR-MID                                       
026088     MOVE ZERO  TO SPAR-KDINVKAT-MID                                      
026089     MOVE ZERO  TO SPAR-DATUM-MID                                         
026090     MOVE SPACE TO SPAR-IDUSER-MID                                        
026091     MOVE SPACE TO SPAR-SOK-TYP                                           
026092     .                                                                    
026093     EJECT                                                                
026094 MFS-RENSA-FAELT-UT SECTION.                                              
026095                                                                          
026100*    --- ALLA UTDATA-FÄLT                                                 
026110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
026200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
026300                             MOD-KDINVKAT-UT                              
026310                             MOD-IDUSER-UT                                
026320                             MOD-IDDC-UT                                  
026321                             MOD-DATUM-UT                                 
026322     MOVE +1 TO INDX                                                      
026323     PERFORM UNTIL INDX > 14                                              
026330      MOVE MFS-RENSA-FAELT TO MOD-RAD-INFO (INDX)                         
026340      ADD +1 TO INDX                                                      
026350     END-PERFORM                                                          
026400     .                                                                    
026501     SKIP3                                                                
026502*MFS-RENSA-RAD-FAELT-UT SECTION.                                          
026503*                                                                         
026504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
026505*    MOVE MFS-RENSA-FAELT TO MOD-RAD-INFO (INDX)                          
026510*    .                                                                    
026600*    SKIP3                                                                
026700 MFS-RENSA-FAELT-IN SECTION.                                              
026800                                                                          
026900*    --- ALLA INDATA-FÄLT                                                 
027000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
027100                             MOD-KDINVKAT-IN                              
027110                             MOD-IDUSER-IN                                
027130                             MOD-KDARBTYP-UT                              
027140                             MOD-IDPERSON-UT                              
027141                             MOD-DATUM-IN                                 
027150     MOVE +1 TO INDX                                                      
027160     PERFORM UNTIL INDX > 14                                              
027170      MOVE MFS-RENSA-FAELT TO MOD-RAD-INFO (INDX)                         
027180      ADD +1 TO INDX                                                      
027190     END-PERFORM                                                          
027200     .                                                                    
027300     EJECT                                                                
027310*                                                                         
027400*MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027500*                                                                         
027600*    --- ALLA UTDATA-FÄLT                                                 
027710*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027800*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
027900*                              MOD-KDINVKAT-UT                            
028000*                              MOD-DATUM-UT                               
028001*    MOVE +1 TO INDX                                                      
028002*    PERFORM UNTIL INDX > MAX-INDX                                        
028003*      PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
028004*      ADD +1 TO INDX                                                     
028005*    END-PERFORM                                                          
028006*    .                                                                    
028007*    SKIP2                                                                
028008*                                                                         
028009*MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
028010*                                                                         
028011*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
028012*    MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-INFO (INDX)                        
028013*                                                                         
028014*                                                                         
028020*    .                                                                    
028200*    SKIP3                                                                
028210*                                                                         
028300*MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028400*                                                                         
028500*    --- ALLA INDATA-FÄLT                                                 
028600*    MOVE MFS-ROER-EJ-FAELT TO   MOD-IDARTNR-IN                           
028700*                                MOD-KDINVKAT-IN                          
028710*                                MOD-IDUSER-IN                            
028720*                                MOD-DATUM-IN                             
028800*    .                                                                    
028900*    EJECT                                                                
029000 MFS-FORM-ATTR SECTION.                                                   
029100                                                                          
029200*    --- ALLA INDATA-FÄLT                                                 
029300     MOVE MFS-FORMATETS-ATTR TO MOD-KDARBTYP-ATTR                         
029400                                MOD-IDPERSON-ATTR                         
029500     .                                                                    
029600     SKIP2                                                                
029700 MFS-LAES-IN-IGEN SECTION.                                                
029800                                                                          
029900*    --- ALLA INDATA-FÄLT                                                 
030000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDARBTYP-ATTR                      
030100                                   MOD-IDPERSON-ATTR                      
030200     .                                                                    
030300     EJECT                                                                
030400* --- IMS SEKTIONER ---                                                   
030500     SKIP3                                                                
030600 IMS-GET-MSG SECTION.                                                     
030700                                                                          
030800     MOVE '  QC' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-INSERT-MSG SECTION.                                                  
031500                                                                          
031600***  IF MSGI-IDLAND-SPR = 'SE'                                            
031700       MOVE 'N' TO MFS-KDHUVOMR                                           
031800***  END-IF                                                               
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE SPACE TO GODK-STATUSKODER                                       
032100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032501     EJECT                                                                
032502 IMS-INSERT-ALTMSG SECTION.                                               
032503                                                                          
032504     MOVE SPACE TO GODK-STATUSKODER                                       
032505     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
032506     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032507     PERFORM IMS-STATUSKONTROLL                                           
032508     .                                                                    
032509     EJECT                                                                
032514                                                                          
032515 IMS-GET-WDP301 SECTION.                                                  
032516                                                                          
032517     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
032518          DELIMITED BY SIZE INTO SSA1                                     
032519     MOVE '  GE' TO GODK-STATUSKODER                                      
032520     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP301 SSA1                    
032521     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
032522     PERFORM IMS-STATUSKONTROLL                                           
032523     .                                                                    
032524     EJECT                                                                
032525 IMS-GET-WDP311 SECTION.                                                  
032526                                                                          
032527     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
032528          DELIMITED BY SIZE INTO SSA1                                     
032529     MOVE '  GE' TO GODK-STATUSKODER                                      
032530     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP311 SSA1                   
032531     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
032532     PERFORM IMS-STATUSKONTROLL                                           
032533     .                                                                    
032534     EJECT                                                                
032535 IMS-GET-WDH101 SECTION.                                                  
032536     MOVE 'IMS-GET-WDH101'    TO WS-IMS                                   
032537     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
032538          DELIMITED BY SIZE INTO SSA1                                     
032539     MOVE '  GE' TO GODK-STATUSKODER                                      
032540     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH101 SSA1                    
032541     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
032542     PERFORM IMS-STATUSKONTROLL                                           
032543     .                                                                    
032544     EJECT                                                                
032545 IMS-GNP-WDH111 SECTION.                                                  
032546     MOVE 'IMS-GNP-WDH111'   TO WS-IMS                                    
032547**** STRING 'WDH111  (WDH111KY =' W-WDH111KY-X ')'                        
032548     STRING 'WDH111  (WDH111KY>=' W-WDH111-KEY-MIN-X                      
032549                    '&WDH111KY<=' W-WDH111-KEY-MAX-X ')'                  
032552          DELIMITED BY SIZE INTO SSA1                                     
032553     MOVE '  GE' TO GODK-STATUSKODER                                      
032554     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH111 SSA1                   
032555     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
032556     PERFORM IMS-STATUSKONTROLL                                           
032560     .                                                                    
032600     EJECT                                                                
032610 IMS-GU-WDH111 SECTION.                                                   
032620     MOVE 'IMS-GU-WDH111'    TO WS-IMS                                    
032621     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
032622          DELIMITED BY SIZE INTO SSA1                                     
032630     STRING 'WDH111  (WDH111KY =' W-WDH111KY-X ')'                        
032640          DELIMITED BY SIZE INTO SSA2                                     
032650     MOVE '  GE' TO GODK-STATUSKODER                                      
032660     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH111 SSA1 SSA2               
032670     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
032680     PERFORM IMS-STATUSKONTROLL                                           
032690     .                                                                    
032691     EJECT                                                                
032692 IMS-GNP-WDH121-UNIK SECTION.                                             
032696     MOVE 'IMS-GN-WDH121'    TO WS-IMS                                    
032697***  STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
032698***       DELIMITED BY SIZE INTO SSA1                                     
032699     STRING 'WDH111  (WDH111KY =' W-WDH111KY-X ')'                        
032700          DELIMITED BY SIZE INTO SSA1                                     
032701     MOVE 'WDH121 ' TO SSA2                                               
032702     MOVE '  GE' TO GODK-STATUSKODER                                      
032703     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH121 SSA1 SSA2              
032704     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
032705     PERFORM IMS-STATUSKONTROLL                                           
032706     .                                                                    
032707     EJECT                                                                
032708 IMS-GNP-WDH121 SECTION.                                                  
032709     MOVE 'IMS-GNP-WDH121'   TO WS-IMS                                    
032710     STRING 'WDH111  (WDH111KY>=' W-WDH111-KEY-MIN-X                      
032720                    '&WDH111KY<=' W-WDH111-KEY-MAX-X ')'                  
032730          DELIMITED BY SIZE INTO SSA1                                     
032732     MOVE 'WDH121 ' TO SSA2                                               
032733     MOVE '  GE' TO GODK-STATUSKODER                                      
032734     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH121 SSA1 SSA2              
032735     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
032736     PERFORM IMS-STATUSKONTROLL                                           
032737     .                                                                    
032738     EJECT                                                                
032739 IMS-GU-WDH1C1  SECTION.                                                  
032740     STRING 'WDH1C1  (WDH1C1KY =' W-WDH1C1KY-X ')'                        
032741              DELIMITED BY SIZE INTO SSA1                                 
032742     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032743     CALL CBLTDLI USING GU WDH1C-PCB DLI-IO-WDH1C1 SSA1                   
032744     MOVE WDH1C-STATUS-CODE TO STATUS-WS                                  
032745     PERFORM IMS-STATUSKONTROLL                                           
032746     .                                                                    
032747     SKIP2                                                                
032748 IMS-GN-WDH1C1  SECTION.                                                  
032749     STRING 'WDH1C1  (WDH1C1KY>=' W-WDH1C1KY-MIN-X                        
032750                    '&WDH1C1KY<=' W-WDH1C1KY-MAX-X ')'                    
032751              DELIMITED BY SIZE INTO SSA1                                 
032752     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032753     CALL CBLTDLI USING GN WDH1C-PCB DLI-IO-WDH1C1 SSA1                   
032754     MOVE WDH1C-STATUS-CODE TO STATUS-WS                                  
032755     PERFORM IMS-STATUSKONTROLL                                           
032756     .                                                                    
032757     SKIP2                                                                
032758*                                                                         
032759*IMS-GNP-WDH1C1    SECTION.                                               
032760*    MOVE 'WDH101 ' TO SSA1                                               
032761*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
032762*    CALL CBLTDLI USING GNP WDH1C-PCB DLI-IO-WDH101 SSA1                  
032763*    MOVE WDH1C-STATUS-CODE TO STATUS-WS                                  
032764*    PERFORM IMS-STATUSKONTROLL                                           
032765*    .                                                                    
032766*    SKIP2                                                                
032767*IMS-GNP-WDH1C1-21 SECTION.                                               
032768*    MOVE 'WDH121 ' TO SSA1                                               
032769*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
032770*    CALL CBLTDLI USING GNP WDH1C-PCB DLI-IO-WDH121 SSA1                  
032771*    MOVE WDH1C-STATUS-CODE TO STATUS-WS                                  
032772*    PERFORM IMS-STATUSKONTROLL                                           
032773*    .                                                                    
032774*    SKIP2                                                                
032775 IMS-GU-WDH1E1  SECTION.                                                  
032776     MOVE 'IMS-GU-WDH1E1'    TO WS-IMS                                    
032777     STRING 'WDH1E1  (WDH1E1KY =' W-WDH1E1KY-X ')'                        
032778              DELIMITED BY SIZE INTO SSA1                                 
032779     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032780     CALL CBLTDLI USING GU WDH1E-PCB DLI-IO-WDH1E1 SSA1                   
032781     MOVE WDH1E-STATUS-CODE TO STATUS-WS                                  
032782     PERFORM IMS-STATUSKONTROLL                                           
032783     .                                                                    
032784     SKIP2                                                                
032785 IMS-GN-WDH1E1  SECTION.                                                  
032786     MOVE 'IMS-GN-WDH1E1'    TO WS-IMS                                    
032787     STRING 'WDH1E1  (WDH1E1KY>=' W-WDH1E1KY-MIN-X                        
032788                    '&WDH1E1KY<=' W-WDH1E1KY-MAX-X ')'                    
032789              DELIMITED BY SIZE INTO SSA1                                 
032790     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032791     CALL CBLTDLI USING GN WDH1E-PCB DLI-IO-WDH1E1 SSA1                   
032792     MOVE WDH1E-STATUS-CODE TO STATUS-WS                                  
032793     PERFORM IMS-STATUSKONTROLL                                           
032794     .                                                                    
032795     SKIP2                                                                
032796 IMS-GU-WDH1F1  SECTION.                                                  
032797     MOVE 'IMS-GU-WDH1F1'    TO WS-IMS                                    
032798                                                                          
032799     STRING 'WDH1F1  (WDH1F1KY =' W-WDH1F1KY-X ')'                        
032800              DELIMITED BY SIZE INTO SSA1                                 
032801     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032802     CALL CBLTDLI USING GU WDH1F-PCB DLI-IO-WDH1F1 SSA1                   
032803     MOVE WDH1F-STATUS-CODE TO STATUS-WS                                  
032804     PERFORM IMS-STATUSKONTROLL                                           
032805     .                                                                    
032806     SKIP2                                                                
032807 IMS-GN-WDH1F1  SECTION.                                                  
032808     MOVE 'IMS-GN-WDH1F1'    TO WS-IMS                                    
032809     STRING 'WDH1F1  (WDH1F1KY>=' W-WDH1F1KY-MIN-X                        
032810                    '&WDH1F1KY<=' W-WDH1F1KY-MAX-X ')'                    
032811              DELIMITED BY SIZE INTO SSA1                                 
032812     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032813     CALL CBLTDLI USING GN WDH1F-PCB DLI-IO-WDH1F1 SSA1                   
032814     MOVE WDH1F-STATUS-CODE TO STATUS-WS                                  
032815     PERFORM IMS-STATUSKONTROLL                                           
032816     .                                                                    
032817     SKIP2                                                                
032818 IMS-GU-WDH1D1  SECTION.                                                  
032819     MOVE 'IMS-GU-WDH1D1'    TO WS-IMS                                    
032820                                                                          
032821     STRING 'WDH1D1  (WDH1D1KY =' W-WDH1D1KY-X ')'                        
032822              DELIMITED BY SIZE INTO SSA1                                 
032823     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032824     CALL CBLTDLI USING GU WDH1D-PCB DLI-IO-WDH1D1 SSA1                   
032825     MOVE WDH1D-STATUS-CODE TO STATUS-WS                                  
032826     PERFORM IMS-STATUSKONTROLL                                           
032827     .                                                                    
032828     SKIP2                                                                
032829 IMS-GN-WDH1D1  SECTION.                                                  
032830     MOVE 'IMS-GN-WDH1D1'    TO WS-IMS                                    
032831     STRING 'WDH1D1  (WDH1D1KY>=' W-WDH1D1KY-MIN-X                        
032832                    '&WDH1D1KY<=' W-WDH1D1KY-MAX-X ')'                    
032833              DELIMITED BY SIZE INTO SSA1                                 
032834     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032835     CALL CBLTDLI USING GN WDH1D-PCB DLI-IO-WDH1D1 SSA1                   
032836     MOVE WDH1D-STATUS-CODE TO STATUS-WS                                  
032837     PERFORM IMS-STATUSKONTROLL                                           
032838     .                                                                    
032839     SKIP2                                                                
032840*                                                                         
032841*IMS-GNP-WDH1E1    SECTION.                                               
032842*    MOVE 'WDH101 ' TO SSA1                                               
032843*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
032844*    CALL CBLTDLI USING GNP WDH1E-PCB DLI-IO-WDH101 SSA1                  
032845*    MOVE WDH1E-STATUS-CODE TO STATUS-WS                                  
032846*    PERFORM IMS-STATUSKONTROLL                                           
032847*    .                                                                    
032848*    SKIP2                                                                
032849 IMS-STATUSKONTROLL SECTION.                                              
032850                                                                          
032900     SET STATUS-IX TO 1                                                   
033000     SEARCH GODK-STATUS                                                   
033100       AT END                                                             
033200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033300         DELIMITED BY SIZE INTO FELTEXT                                   
033400         CALL FELLOG                                                      
033500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
