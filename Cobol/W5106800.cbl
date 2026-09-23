000010 ID DIVISION.                                                             
000020 PROGRAM-ID.     W5106800.                                                
000030 AUTHOR.         BO HAMMARIN.                                             
000040 DATE-WRITTEN.   JULI-1998.                                               
000050 DATE-COMPILED.                                                           
000060                                                                          
000070*                                                                         
000080*    FUNKTION:                                                            
000090*       -PGM LÄSER KONTROLLERADE/KOMPLETTERADE EKONOMISKA                 
000100*        HÄNDELSETRANSAKTIONER OCH MATCHAR DESSA MOT                      
000110*        EKONOMISKA STYRPARAMETRAR FÖR ATT I SLUTÄNDEN                    
000120*        PRODUCERA POSTER TILL R3 I FORM AV                               
000130*        1 "LINE RECORD" HUVUDBOK               (PTYP 610)                
000140*        2 "LINE RECORD" KUNDRESKONTRA          (PTYP 310)                
000150*        3 "LINE RECORD" LEVERANTÖRSRESKONTRA   (PTYP 210)                
000160*                                                                         
000170*       -PGM SKAPAR/SKRIVER ÄVEN FÖLJANDE POSTER TILL R3                  
000180*        1 "HEADER RECORD" HUVUDBOK             (PTYP 600)                
000190*        2 "HEADER RECORD" KUNDRESKONTRA        (PTYP 300)                
000200*        3 "HEADER RECORD" LEVERANTÖRSRESKONTRA (PTYP 200)                
000210*                                                                         
000220*       -PGM PLOCKAR UNDAN NY MÅNADS POSTER VID MÅNADSSKIFTE              
000230*        FÖR ATT TA IN DESSA VID NÄSTA KÖRNING.                           
000240*        (NY MÅNADS POSTER = DATUMKORTS MÅNAD + 1, OM DENNA ÄR            
000250*         LIKA MED IN-POSTENS DAVERDAT'S MÅNAD,                           
000260*         SKRIVS POSTEN PÅ UTFIL FÖR AT TAS IN NÄSTA KÖRNING).            
000270*                                                                         
000280*                                                                         
000290*       -PROGRAMMET LÄSER      WDH5                                       
000300*                              WLBETC (WDB1)                              
000310*                              WLGMTA (WDB2)                              
000320*                              WL5121 (WDR1)                              
000330*                         MÅNADSKURSER WDG2                               
000340*                           DCREGISTER WDB6                               
000350*                                                                         
000360* -OBS - OBS - OBS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                       
000370* 1 KOMPLETTERANDE TRANSAKTIONSPLAN FINNS LAGRAD                          
000380*   I ETT EXCEL-FORMULÄR PÅ T-DISKEN (VSEG0441) MED NAMNET                
000390*  .PRODUCT AND SERVICES/DOCUMENTATION/CAR/SYSDOC/W5ECON/TPLAN-R3         
000400* 2 KOMPLETTERANDE DOKUMENTATION OM HÄNDELSETYPER FINNS                   
000410*   LAGRAD I ETT EXCEL-FORMULÄR PÅ T-DISKEN (VSEG0441) MED NAMNET         
000420*  .PRODUCT AND SERVICES/DOCUMENTATION/CAR/SYSDOC/W5ECON/PULSEVENT        
000430* -OBS - OBS - OBS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                       
000440*                                                                         
000450*    ABENDKODER:                                                          
000460*        U0016 -  . . . .                                                 
000470*        U1000 -  . . . .                                                 
000480*                                                                         
000490 ENVIRONMENT DIVISION.                                                    
000500                                                                          
000510 INPUT-OUTPUT SECTION.                                                    
000520                                                                          
000530 FILE-CONTROL.                                                            
000540*          --- KONTR./KOMPL. HÄNDELSETRANSAKTIONER                        
000550     SELECT W51066                     ASSIGN TO W51068D1.                
000560                                                                          
000570*          --- SAP/R3 - EJ FAKTUROR/KREDITNOTOR                           
000580     SELECT W51071A                    ASSIGN TO W51068D2.                
000590                                                                          
000600*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ACKUM.                  
000610     SELECT W51072A                    ASSIGN TO W51068D3.                
000620                                                                          
000630*          --- SAP/R3 - FAKTURA-/KREDITNOTAPOSTER ÖVRIGA                  
000640     SELECT W51073A                    ASSIGN TO W51068D4.                
000650                                                                          
000660*          --- LOGG TILL ON-DEMAND                                        
000670     SELECT W51075                     ASSIGN TO W51068D5.                
000680                                                                          
000690*          --- LOGG TILL ARTIKEL-DIFF-ANALYSEN                            
000700     SELECT W51068                     ASSIGN TO W51068D6.                
000710                                                                          
000720*          --- FIL TILL VECKORAPPORTEN                                    
000730     SELECT W51714                     ASSIGN TO W51068D7.                
000740                                                                          
000750*          --- FIL TILL MÅNADSAVSTÄMNINGEN                                
000760     SELECT W51713                     ASSIGN TO W51068D8.                
000770                                                                          
000780*      - - - SAP-POSTER TILL NÄSTA DAGKÖRNING                             
000790     SELECT W5106N                     ASSIGN TO W51068D9.                
000800                                                                          
000810*      - - - INV-POSTER TILL NÄSTA VECKOKÖRNING                           
000820     SELECT W51310                     ASSIGN TO W51068DA.                
000830     EJECT                                                                
000840                                                                          
000850 DATA DIVISION.                                                           
000860                                                                          
000870 FILE SECTION.                                                            
000880 FD  W51066                                                               
000890     RECORDING       F                                                    
000900     BLOCK CONTAINS  0.                                                   
000910 01  SAP-POST.                                                            
000920*    03  -COPY WDR901        -L.                                          
000930     03 FILLER                   PIC X(6).                                
000940                                                                          
000950 FD  W51071A                                                              
000960     RECORDING       V                                                    
000970     BLOCK CONTAINS  0.                                                   
000980*01  71INIT-POST -COPY R3INIT20               -L.                         
000990*01  71HEAD-POST -COPY R3HEAD20               -L.                         
001000*01  71LINE-POST -COPY R3LINE20               -L.                         
001010                                                                          
001020 FD  W51072A                                                              
001030     RECORDING       F                                                    
001040     BLOCK CONTAINS  0.                                                   
001050*01  72LINE-POST -COPY R3LINE20               -L.                         
001060                                                                          
001070 FD  W51073A                                                              
001080     RECORDING       V                                                    
001090     BLOCK CONTAINS  0.                                                   
001100*01  73HEAD-POST -COPY R3HEAD20               -L.                         
001110*01  73LINE-POST -COPY R3LINE20               -L.                         
001120                                                                          
001130 FD  W51075                                                               
001140     RECORDING       F                                                    
001150     BLOCK CONTAINS  0.                                                   
001160*01  LOGG-POST   -COPY W51074                 -L.                         
001170                                                                          
001180 FD  W51068                                                               
001190     RECORDING       F                                                    
001200     BLOCK CONTAINS  0.                                                   
001210*01  AVST-POST   -COPY W51068                 -L.                         
001220                                                                          
001230 FD  W51714                                                               
001240     RECORDING       V                                                    
001250     BLOCK CONTAINS  0.                                                   
001260*01  POST -COPY W517RW1 -PRE  RW1-   -L.                                  
001270*01  POST -COPY W517RW2 -PRE  RW2-   -L.                                  
001280                                                                          
001290 FD  W51713                                                               
001300     RECORDING       F                                                    
001310     BLOCK CONTAINS  0.                                                   
001320*01  POST -COPY W51713  -PRE  MON-   -L.                                  
001330                                                                          
001340 FD  W5106N                                                               
001350     RECORDING       F                                                    
001360     BLOCK CONTAINS  0.                                                   
001370                                                                          
001380 01  SAPUT-POST.                                                          
001390*    03  -COPY WDR901        -L.                                          
001400     03 FILLER                   PIC X(6).                                
001410                                                                          
001420 FD  W51310                                                               
001430     RECORDING       F                                                    
001440     BLOCK CONTAINS  0.                                                   
001450*01  POST -COPY W51310  -PRE  INV-   -L.                                  
001460                                                                          
001470     EJECT                                                                
001480 WORKING-STORAGE SECTION.                                                 
001490*    -- CHECKED BY WY2000                                                 
001500 77  IDPGM                        PIC X(8)    VALUE 'W5106800'.           
001510 77  JA                           PIC X       VALUE 'J'.                  
001520 77  NEJ                          PIC X       VALUE 'N'.                  
001530 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
001540 77  W51066-EOF-SW                PIC X       VALUE 'N'.                  
001550     88  END-OF-W51066                        VALUE 'J'.                  
001560 77  WS-HEADER-SW                 PIC X       VALUE 'N'.                  
001570 77  WS-LINE-SW                   PIC X       VALUE 'N'.                  
001580 77  WS-LINE-AMOUNT             PIC S9(13)V99 COMP-3.                     
001590 77  WS-LOP                     PIC 9                VALUE ZERO.          
001600 77  WS-SPAR-KDEKHHT            PIC X(3)             VALUE SPACE.         
001610 77  WS-SPAR-KDEKSHT            PIC X(3)             VALUE SPACE.         
001620 77  WS-SPAR-IDVERGL            PIC X(10)            VALUE SPACE.         
001630 77  WS-BELOPP                  PIC S9(7)V99  COMP-3.                     
001640 77  WS-BELOPP-135              PIC S9(7)V99  COMP-3.                     
001650 77  WS-BELOPP-145              PIC S9(7)V99  COMP-3.                     
001660 77  WS-SUMMA-135               PIC S9(7)V99  COMP-3 VALUE ZERO.          
001670 77  WS-SUMMA-145               PIC S9(7)V99  COMP-3 VALUE ZERO.          
001680 77  WS-SUMMA-204-204           PIC S9(7)V99  COMP-3 VALUE ZERO.          
001690 77  WS-BELOPP-ALL1             PIC S9(7)V99  COMP-3.                     
001700 77  WS-BELOPP-ALL              PIC S9(7)V99  COMP-3.                     
001710 77  WS-BONUS                 PIC S9(1)V9999 COMP-3 VALUE 0.0465.         
001720 77  WS-DISCOUNT              PIC S9(1)V9999 COMP-3 VALUE 0.0030.         
001730 77  WS-MARGIN                PIC S9(1)V9999 COMP-3 VALUE 0.0375.         
001740 77  SPAR-LINE-ACCOUNT            PIC X(10).                              
001750 77  SPAR-LINE-ORDER              PIC X(12).                              
001760 77  SPAR-LINE-COST-CENTER        PIC X(10).                              
001770 77  SPAR-SUMMA                   PIC S9(9)V99  COMP-3 VALUE ZERO.        
001780 77  SPAR-SUMMA-202-204           PIC S9(9)V99  COMP-3 VALUE ZERO.        
001790 77  SPAR-DIFF                    PIC S9(9)V99  COMP-3 VALUE ZERO.        
001800 77  SPAR-PRDMTRL                 PIC S9(9)V99  COMP-3 VALUE ZERO.        
001810 77  SPAR-PROVRPAL                PIC S9(9)V99  COMP-3 VALUE ZERO.        
001820 77  SPAR-PRDIRLON                PIC S9(9)V99  COMP-3 VALUE ZERO.        
001830 77  WS-RED-IDKST                 PIC X(10).                              
001840 77  WS-IDPTYP                    PIC X(3).                               
001850 77  WS-IDLEVNR                   PIC S9(5)   VALUE ZERO.                 
001860 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
001870 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
001880 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
001890 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
001900 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
001910 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
001920 77  WS-IDDC-SAVE                 PIC X(2)    VALUE SPACE.                
001930                                                                          
001940 77    WDB6-A-SW                  PIC X       VALUE 'J'.                  
001950       88  WDB6-A-FINNS                       VALUE 'J'.                  
001960       88  WDB6-A-SAKNAS                      VALUE 'N'.                  
001970                                                                          
001980*01  -COPY WWDCKONS                                                       
001990                                                                          
002000*01  -COPY WWPRODSL                                                       
002010                                                                          
002020*01  -COPY WWDC99                                                         
002030                                                                          
002040     EJECT                                                                
002050                                                                          
002060 01  FILLER                       PIC X(16)   VALUE 'WWIDFTG '.           
002070*01  -COPY WWIDFTG                                                        
002080     EJECT                                                                
002090                                                                          
002100 01  FELTEXT                      PIC X(80).                              
002110 01  TEST-IDDISTR                 PIC 9(5)    COMP-3.                     
002120*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
002130     EJECT                                                                
002140                                                                          
002150*01  FILLER  -COPY WWDIST28   -RED TEST-IDDISTR.                          
002160     EJECT                                                                
002170                                                                          
002180*01  FILLER  -COPY WWDIST34   -RED TEST-IDDISTR.                          
002190     EJECT                                                                
002200                                                                          
002210*01  FILLER  -COPY WWDIST35   -RED TEST-IDDISTR.                          
002220     EJECT                                                                
002230                                                                          
002240*01  FILLER  -COPY WWDIST42   -RED TEST-IDDISTR.                          
002250     EJECT                                                                
002260                                                                          
002270 01  TEST-IDARTNR                 PIC 9(9) COMP-3.                        
002280*01  FILLER  -COPY WWBYT24    -RED TEST-IDARTNR                           
002290     EJECT                                                                
002300                                                                          
002310 01  TEST-IDHTYP                  PIC X(6).                               
002320     EJECT                                                                
002330 01  W-BET-IDPARTNR-NUM          PIC 9(10).                               
002340 01  W-BET-IDPARTNR-ALFA         PIC X(10).                               
002350     EJECT                                                                
002360 01  WS-IDDISTR-IDKUNDNR.                                                 
002370     03  FILLER                   PIC X(2)    VALUE SPACE.                
002380     03  WS-IDDISTR               PIC 9(4).                               
002390     03  WS-IDKUNDNR              PIC 9(6).                               
002400                                                                          
002410 01  WS-KDBETVIL                  PIC X(4).                               
002420 01  WS-KDVALISO-WDB6             PIC X(3).                               
002430 01  WS-KDVALISO-WDB1             PIC X(3).                               
002440 01  WS-KDVALISO                  PIC X(3).                               
002450 01  WS-KDVALISO-CN               PIC X(3) VALUE 'CNY'.                   
002460 01  WS-KDVALISO-US               PIC X(3) VALUE 'USD'.                   
002470 01  WS-KDVALISO-IN               PIC X(3) VALUE 'INR'.                   
002480 01  WS-KDVALISO-KR               PIC X(3) VALUE 'KRW'.                   
002490 01  WS-KDVALISO-TR               PIC X(3) VALUE 'TRY'.                   
002500 01  WS-KDVALISO-MY               PIC X(3) VALUE 'MYR'.                   
002510 01  WS-KDVALISO-TH               PIC X(3) VALUE 'THB'.                   
002520 01  WS-KDVALISO-TW               PIC X(3) VALUE 'TWD'.                   
002530 01  WS-KDVALISO-MX               PIC X(3) VALUE 'MXN'.                   
002540 01  WS-KDVALISO-BR               PIC X(3) VALUE 'BRL'.                   
002550 01  WS-KDVALISO-ZA               PIC X(3) VALUE 'ZAR'.                   
002560 01  WS-PRKURS-ALL                PIC S9(6)V9(5) COMP-3.                  
002570 01  WS-PRKURS                    PIC S9(6)V9(5) COMP-3.                  
002580 01  WS-PRKURS-CN                 PIC S9(6)V9(5) COMP-3.                  
002590 01  WS-PRKURS-CN3                PIC S9(6)V9(5) COMP-3.                  
002600 01  WS-PRKURS-US                 PIC S9(6)V9(5) COMP-3.                  
002610 01  WS-PRKURS-US3                PIC S9(6)V9(5) COMP-3.                  
002620 01  WS-PRKURS-MY                 PIC S9(6)V9(5) COMP-3.                  
002630 01  WS-PRKURS-MY3                PIC S9(6)V9(5) COMP-3.                  
002640 01  WS-PRKURS-ALL3               PIC S9(6)V9(5) COMP-3.                  
002650 01  WS-KDANMORS-HDR              PIC X(2).                               
002660     88 WS-KDANMORS-HDR-YES       VALUE '12' '22' '27' '13'               
002670                                        '23' '28'.                        
002680                                                                          
002690 01  WS-LINE-AMOUNT-LC            PIC S9(9)V9(5) COMP-3.                  
002700 01  WS-SUNTO-JPY                 PIC S9(9) COMP-3 VALUE ZERO.            
002710 01  WS-SUBTO-JPY                 PIC S9(9) COMP-3 VALUE ZERO.            
002720 01  WS-SUVAT-JPY                 PIC S9(9) COMP-3 VALUE ZERO.            
002730                                                                          
002740 01  WS-ALLOCATE.                                                         
002750     03  WS-ALLOCATE-DC           PIC X(2).                               
002760     03  WS-ALLOCATE-DISTR        PIC X(5).                               
002770     03  WS-ALLOCATE-REF          PIC X(7)    VALUE SPACE.                
002780     03  FILLER                   PIC X(4)    VALUE SPACE.                
002790                                                                          
002800 01  WS-ALLOCATE-TIS.                                                     
002810     03  WS-ALLOCATE-ORDER        PIC X(10).                              
002820     03  FILLER                   PIC X(8)    VALUE SPACE.                
002830                                                                          
002840 01  WS-TEXT.                                                             
002850     03  WS-TEXT-FEEDER-SYSTEM    PIC X(10).                              
002860     03  WS-TEXT-KDEKHHT          PIC X(3).                               
002870     03  WS-TEXT-KDEKSHT          PIC X(3).                               
002880     03  WS-HEAD-TEXT-SOFT        PIC X(2).                               
002890     03  FILLER                   PIC X(7)    VALUE SPACE.                
002900                                                                          
002910 01  WS-LINE-TEXT.                                                        
002920     03  WS-LINE-TEXT-KDEKHHT     PIC X(3).                               
002930     03  WS-LINE-TEXT-KDEKSHT     PIC X(3).                               
002940     03  WS-LINE-TEXT-SOFT        PIC X(2).                               
002950     03  FILLER                   PIC X(42)   VALUE SPACE.                
002960                                                                          
002970 01  WS-PRCTR-PRODSL-DISP         PIC 9(2) VALUE ZERO.                    
002980 01  WS-PRCTR-PRODSL-DISP-L       PIC 9(2) VALUE ZERO.                    
002990 01  WS-PRCTR.                                                            
003000     03  FILLER                   PIC X(1) VALUE SPACE.                   
003010     03  WS-PRCTR-PRODSL          PIC X(2) VALUE SPACE.                   
003020     03  FILLER                   PIC X(7) VALUE SPACE.                   
003030 01  WS-PRCTR-LYNK.                                                       
003040     03  FILLER                   PIC X(4) VALUE SPACE.                   
003050     03  WS-PRCTR-PRODSL-L        PIC X(2) VALUE SPACE.                   
003060     03  FILLER                   PIC X(4) VALUE SPACE.                   
003070                                                                          
003080 01  WS-ORDER-STAT.                                                       
003090     03  WS-ANALYS-STAT  OCCURS 3 PIC X(12).                              
003100                                                                          
003110 01  WS-R3-ACCOUNT.                                                       
003120     03  WS-R3-ACCOUNT-ALFA.                                              
003130         05 FILLER                PIC X(4).                               
003140         05 WS-R3-ACCOUNT-6       PIC X(6).                               
003150     03  WS-R3-ACCOUNT-DISP REDEFINES WS-R3-ACCOUNT-ALFA.                 
003160         05 WS-R3-ACCOUNT-10      PIC 9(10).                              
003170                                                                          
003180 01  WS-R3-IDPARTNR               PIC 9(10).                              
003190                                                                          
003200 01  WS-ACCOUNT.                                                          
003210     03  FILLER                   PIC X(7).                               
003220     03  WS-ACCOUNT-4             PIC X(1).                               
003230     03  WS-ACCOUNT-5             PIC X(1).                               
003240     03  WS-ACCOUNT-5A            PIC X(1).                               
003250                                                                          
003260 01  WS-ACCOUNT-VERS2.                                                    
003270     03  FILLER                   PIC X(9).                               
003280     03  WS-ACCOUNT-6             PIC X(1).                               
003290                                                                          
003300 01  WS-ACCOUNT-RETURER.                                                  
003310     03  FILLER                   PIC X(8).                               
003320     03  WS-ACCOUNT-RETURER-5-6   PIC X(2).                               
003330                                                                          
003340 01  SPAR-AREA.                                                           
003350     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
003360     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
003370     03  SPAR-DAVERDAT            PIC 9(8)    VALUE ZERO.                 
003380     03  SPAR-IDVERGL             PIC X(10)   VALUE SPACE.                
003390                                                                          
003400 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
003410 01  FILLER REDEFINES DAGENS-DATUM.                                       
003420     03  DAGENS-DATUM-AAR         PIC 9(2).                               
003430     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
003440     03  DAGENS-DATUM-DAG         PIC 9(2).                               
003450                                                                          
003460 01  WS-NEW-MONTH                 PIC 9(2).                               
003470                                                                          
003480 01  WS-DAREGDAT.                                                         
003490     03  WS-DAREGDAT-SEKEL        PIC 9(2)    VALUE 20.                   
003500     03  WS-DAREGDAT-AAMMDD       PIC 9(6).                               
003510                                                                          
003520 01  DAGENS-KLOCKA                PIC 9(8)    VALUE ZERO.                 
003530 01  WS-KLOCKA                    PIC 9(6)    VALUE ZERO.                 
003540     EJECT                                                                
003550                                                                          
003560 01  DYNAMISKA-SUBPROGRAM.                                                
003570     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
003580     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
003590     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
003600     03  DATKORT                  PIC X(8)    VALUE 'DATKORT'.            
003610     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
003620     03  W510KONT                 PIC X(8)    VALUE 'W510KONT'.           
003630     03  W009CIA                  PIC X(8)    VALUE 'W009CIA'.            
003640     03  W510MARK                 PIC X(8)    VALUE 'W510MARK'.           
003650     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
003660                                                                          
003670*    --- PARAMETRAR TILL ABEND                                            
003680                                                                          
003690 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
003700 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
003710 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
003720     EJECT                                                                
003730                                                                          
003740*    --- PARAMETRAR TILL DATKORT                                          
003750*                                                                         
003760 01  PROGRAM-NAMN                 PIC X(6)    VALUE 'W51068'.             
003770                                                                          
003780 01  DATUMKORT-ID                 PIC X(6)    VALUE 'WDATUM'.             
003790                                                                          
003800*01  -COPY WDATKORT                                                       
003810     EJECT                                                                
003820                                                                          
003830*    --- PARAMETRAR TILL POSTSUM                                          
003840*                                                                         
003850*01  -COPY W0005   -PRE  POSTSUM-                                         
003860     EJECT                                                                
003870 01  FILLER                          PIC X(16) VALUE 'W510MARK '.         
003880*                                                                         
003890*01  -COPY W510MARK                                                       
003900*    --- PARAMETRAR TILL ABEND                                            
003910     EJECT                                                                
003920                                                                          
003930 01  FILLER                       PIC X(16) VALUE 'W510KONT-AREA'.        
003940*01  -COPY W510KONT                                                       
003950     EJECT                                                                
003960                                                                          
003970 01  FILLER                       PIC X(16) VALUE 'W510CURR-AREA'.        
003980*01  -COPY W510CURR                                                       
003990     EJECT                                                                
004000                                                                          
004010*    --- PARAMETRAR TILL W009CIA                                          
004020*01  -COPY W009CIA                                                        
004030     EJECT                                                                
004040                                                                          
004050 01  IN-AREA-START                PIC X(24) VALUE 'IN-AREA-START'.        
004060                                                                          
004070*01  AREA -COPY WDR901           -PRE IN-                                 
004080*        05   -COPY W510EKHA     -PRE IN- -RED IN-FIL-WDR901-DATA         
004090         05   IN-EKH-IDSYSMOT     PIC X(6).                               
004100                                                                          
004110     EJECT                                                                
004120 01  UT-AREA-START                PIC X(24) VALUE 'R3-AREA.START'.        
004130                                                                          
004140*01  -COPY R3LINE20              -PRE R3-                                 
004150*01  -COPY R3HEAD20              -PRE R3-                                 
004160*01  -COPY R3INIT20              -PRE R3-                                 
004170*01  -COPY W51074                -PRE LOGG-                               
004180*01  -COPY W51068                -PRE AVST-                               
004190*01  -COPY W517RW1               -PRE RW1-                                
004200*01  -COPY W517RW2               -PRE RW2-                                
004210*01  -COPY W51713                -PRE MON-                                
004220*01  -COPY W51310                -PRE INV-                                
004230     EJECT                                                                
004240                                                                          
004250*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
004260*                                                                         
004270 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
004280                                                                          
004290 01  NYCKLAR-TILL-DLI.                                                    
004300     03  W-WDH501KY-X.                                                    
004310         05  W-IDFTG              PIC 9(2)    VALUE ZERO.                 
004320         05  W-KDEKHHT            PIC X(3)    VALUE SPACE.                
004330     03  W-KDEKSHT-X.                                                     
004340         05  W-KDEKSHT            PIC X(3)    VALUE SPACE.                
004350     03  W-KDEKNIVA-X.                                                    
004360         05  W-KDEKNIVA           PIC X(5)    VALUE SPACE.                
004370     03  W-WDH531KY-X.                                                    
004380         05  W-IDSYSMOT           PIC X(6)    VALUE SPACE.                
004390         05  W-IDPTYP             PIC X(3)    VALUE SPACE.                
004400     03  W-IDRADNR-X.                                                     
004410         05  W-IDRADNR            PIC S9(5)   VALUE ZERO COMP-3.          
004420                                                                          
004430     03  W-IDGMT-KEY.                                                     
004440         05  W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                     
004450         05  W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                     
004460                                                                          
004470     03  W-WDB101KY-X.                                                    
004480         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
004490         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
004500                                                                          
004510     03  W-WDGXKEY-5121-X.                                                
004520         05  FILLER               PIC X(4)    VALUE '5121'.               
004530         05  FILLER               PIC X(2)    VALUE '57'.                 
004540         05  FILLER               PIC X(24)   VALUE LOW-VALUE.            
004550     03  W-WDGXKEY-5122-X.                                                
004560         05  W-IDKONTO-5122       PIC S9(11)  VALUE ZERO COMP-3.          
004570         05  W-IDPRCTR-5122       PIC X(10)   VALUE LOW-VALUE.            
004580     03  W-WDGXKEY-5122-MIN-X.                                            
004590         05  W-IDKONTO-5122-MIN   PIC S9(11)  VALUE ZERO COMP-3.          
004600         05  W-IDPRCTR-5122-MIN   PIC X(10)   VALUE LOW-VALUE.            
004610     03  W-WDGXKEY-5122-MAX-X.                                            
004620         05  W-IDKONTO-5122-MAX   PIC S9(11)  VALUE ZERO COMP-3.          
004630         05  W-IDPRCTR-5122-MAX   PIC X(10)   VALUE HIGH-VALUE.           
004640                                                                          
004650     03  W-IDDC-B6-X.                                                     
004660         05 W-IDDC-B6             PIC X(2)    VALUE SPACE.                
004670                                                                          
004680     EJECT                                                                
004690                                                                          
004700 01  TABELL-IDKST.                                                        
004710     03 IDKST-57510          PIC X(10) VALUE '0000057510'.                
004720     EJECT                                                                
004730                                                                          
004740*    --- STATUS-KOD FRÅN IMS                                              
004750 01  STATUS-WS                    PIC XX.                                 
004760     88  SEGMENT-FINNS                        VALUE '  '.                 
004770     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
004780                                                                          
004790 01  GODK-STATUSKODER.                                                    
004800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
004810                                                                          
004820 01  SSA1                         PIC X(64).                              
004830 01  SSA2                         PIC X(64).                              
004840 01  SSA3                         PIC X(64).                              
004850     EJECT                                                                
004860                                                                          
004870*    --- IMS FUNKTIONSKODER                                               
004880*01  -COPY W0003                                                          
004890     EJECT                                                                
004900                                                                          
004910*    ---  DLI INPUT-OUTPUT AREA                                           
004920 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
004930 01  DLI-IO-WDH501.                                                       
004940*    03  -COPY WDH501                                                     
004950     EJECT                                                                
004960                                                                          
004970 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
004980 01  DLI-IO-WDH511.                                                       
004990*    03  -COPY WDH511                                                     
005000     EJECT                                                                
005010                                                                          
005020 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
005030 01  DLI-IO-WDH521.                                                       
005040*    03  -COPY WDH521                                                     
005050     EJECT                                                                
005060                                                                          
005070 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
005080 01  DLI-IO-WDH531.                                                       
005090*    03  -COPY WDH531                                                     
005100     EJECT                                                                
005110                                                                          
005120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBETC01'.                    
005130 01  DLI-IO-WLBETC01.                                                     
005140*    03  -COPY WDB101                                                     
005150     EJECT                                                                
005160                                                                          
005170 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLGMTA01'.                    
005180 01  DLI-IO-WLGMTA01.                                                     
005190*    03  -COPY WDB201                                                     
005200     EJECT                                                                
005210                                                                          
005220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
005230 01  DLI-IO-WDGX5121.                                                     
005240*    03  -COPY WDGX5121                                                   
005250     EJECT                                                                
005260                                                                          
005270 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
005280 01  DLI-IO-WDGX5122.                                                     
005290*    03  -COPY WDGX5122                                                   
005300     EJECT                                                                
005310                                                                          
005320 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
005330 01   DLI-IO-AREA-B601.                                                   
005340*     03  -COPY WDB601                                                    
005350     EJECT                                                                
005360                                                                          
005370 LINKAGE SECTION.                                                         
005380*01  -COPY W0008  -PRE WDH5-                                              
005390     05  FILLER                  PIC X.                                   
005400                                                                          
005410*01  -COPY W0008  -PRE GMTA-                                              
005420     05  FILLER                  PIC X.                                   
005430                                                                          
005440*01  -COPY W0008  -PRE BETC-                                              
005450     05  FILLER                  PIC X.                                   
005460                                                                          
005470*01  -COPY W0008  -PRE 5121-                                              
005480     05  FILLER                  PIC X.                                   
005490                                                                          
005500*01  -COPY W0008  -PRE WDG2-                                              
005510     05  FILLER                  PIC X.                                   
005520                                                                          
005530*01  -COPY W0008  -PRE WDB6-                                              
005540     05  FILLER                  PIC X.                                   
005550                                                                          
005560     EJECT                                                                
005570                                                                          
005580 PROCEDURE DIVISION  USING WDH5-PCB GMTA-PCB BETC-PCB                     
005590                           5121-PCB WDG2-PCB WDB6-PCB.                    
005600 MAIN SECTION.                                                            
005610     ENTRY 'DLITCBL' USING WDH5-PCB GMTA-PCB BETC-PCB                     
005620                           5121-PCB WDG2-PCB WDB6-PCB.                    
005630                                                                          
005640     PERFORM A-INIT                                                       
005650                                                                          
005660     PERFORM S01-READ-W51066                                              
005670     PERFORM UNTIL END-OF-W51066                                          
005680*** POSTER SKAPADE EFTER MIDNATT NY MÅNAD SPARAS TILL NÄSTA KÖRN.         
005690       IF  IN-EKH-DAVERDAT(5:2) = WS-NEW-MONTH                            
005700       AND IN-EKH-DAVERDAT(3:2) = DAGENS-DATUM-AAR                        
005710       AND WS-NEW-MONTH > 01                                              
005720         PERFORM S60-WRITE-W5106N                                         
005730       ELSE                                                               
005740*** THE RUN ON NEW YEARS DAY SHOULD NOT CONTAIN POSTS                     
005750*** THAT IS CREATED ON THE FIRST                                          
005760         IF FUNCTION CURRENT-DATE(5:4) = 0101                             
005770         AND IN-EKH-DAVERDAT(5:4) = FUNCTION CURRENT-DATE(5:4)            
005780           PERFORM S60-WRITE-W5106N                                       
005790         ELSE                                                             
005800           PERFORM S40-SKAPA-W517-OCH-MON-POSTER                          
005810           PERFORM S30-READ-DATABASE-B2-B1                                
005820           IF IN-EKH-IDSYSMOT = 'SAP' OR 'SAPEXT'                         
005830             PERFORM C-EXECUTE                                            
005840           END-IF                                                         
005850         END-IF                                                           
005860       END-IF                                                             
005870       PERFORM S01-READ-W51066                                            
005880     END-PERFORM                                                          
005890                                                                          
005900     PERFORM Z-FINI                                                       
005910                                                                          
005920     MOVE ZERO TO RETURN-CODE                                             
005930     GOBACK                                                               
005940     .                                                                    
005950     EJECT                                                                
005960                                                                          
005970 A-INIT SECTION.                                                          
005980     OPEN INPUT  W51066                                                   
005990                                                                          
006000     OPEN OUTPUT W51068                                                   
006010                 W51071A                                                  
006020                 W51072A                                                  
006030                 W51073A                                                  
006040                 W51075                                                   
006050                 W51713                                                   
006060                 W51714                                                   
006070                 W5106N                                                   
006080                 W51310                                                   
006090                                                                          
006100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
006110     MOVE 20               TO RW1-DAVVREG(1:2)                            
006120     MOVE D-AAR            TO DAGENS-DATUM-AAR                            
006130                              RW1-DAVVREG(3:2)                            
006140                              MON-TIAAVV(1:2)                             
006150                              W-DATE-AAMM(1:2)                            
006160                              WS-TIAA                                     
006170     MOVE D-MAANAD         TO DAGENS-DATUM-MAANAD                         
006180                              MON-TIMM                                    
006190                              W-DATE-AAMM(3:2)                            
006200                              WS-TIMM                                     
006210                              WS-NEW-MONTH                                
006220     MOVE D-DAG            TO DAGENS-DATUM-DAG                            
006230     MOVE D-VECKA          TO RW1-DAVVREG(5:2)                            
006240                              MON-TIAAVV(3:2)                             
006250     MOVE RW1-DAVVREG      TO RW2-DAVVREG                                 
006260                                                                          
006270*** WS-NEW-MONTH ÄR NÄSTA MÅNAD, ANV. VID MÅNADSSKIFE FÖR ATT             
006280*** SPARA BOKF.TRANSAR TILL NÄSTA KÖRNIG AV DETTA PGM                     
006290     IF WS-NEW-MONTH = 12                                                 
006300       MOVE 1              TO WS-NEW-MONTH                                
006310     ELSE                                                                 
006320       ADD 1               TO WS-NEW-MONTH                                
006330*** FIX FÖR ATT KLARA LÖRDAGNATT EFTER FREDAGNATT-MÅNADSSKIFTE            
006340***   DÅ DATUMKORT PÅ LÖRDAG ÄR SAMMA SOM PÅ FREDAG:                      
006350***   DELS ATT MED W-TIMM HÄMTA NYA MÅNADENS KURS OCH                     
006360***   DELS ATT MHA WS-NEW-MONTH BOKFÖRA ALLA SPARADE BOKF.TRANSAR         
006370***   'FUNCTION CURRENT-DATE(7:2) = 02' => DAG 2 I NY MÅNAD               
006380       IF FUNCTION CURRENT-DATE(7:2) = 02                                 
006390       AND WS-NEW-MONTH < FUNCTION CURRENT-DATE(5:2)                      
006400         ADD 1             TO WS-NEW-MONTH                                
006410         ADD 1             TO WS-TIMM                                     
006420         MOVE WS-TIMM      TO W-DATE-AAMM(3:2)                            
006430       END-IF                                                             
006440     END-IF                                                               
006450                                                                          
006460     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
006470                                                                          
006480     MOVE DAGENS-DATUM TO WS-DAREGDAT-AAMMDD                              
006490                                                                          
006500     ACCEPT DAGENS-KLOCKA FROM TIME                                       
006510     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
006520                                                                          
006530     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
006540     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
006550     MOVE 'M'                   TO CURR-KDVALTYP                          
006560                                                                          
006570     MOVE WS-KDVALISO-CN        TO CURR-KDVALISO-ROW                      
006580     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
006590     IF CURR-KDSVAR = ' '                                                 
006600       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-CN                           
006610     ELSE                                                                 
006620       MOVE 1                   TO WS-PRKURS-CN                           
006630     END-IF                                                               
006640     MOVE WS-PRKURS-CN          TO WS-PRKURS-CN3                          
006650                                                                          
006660     MOVE WS-KDVALISO-US        TO CURR-KDVALISO-ROW                      
006670     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
006680     IF CURR-KDSVAR = ' '                                                 
006690       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-US                           
006700     ELSE                                                                 
006710       MOVE 1                   TO WS-PRKURS-US                           
006720     END-IF                                                               
006730     MOVE WS-PRKURS-US          TO WS-PRKURS-US3                          
006740                                                                          
006750     MOVE WS-KDVALISO-MY        TO CURR-KDVALISO-ROW                      
006760     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
006770     IF CURR-KDSVAR = ' '                                                 
006780       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-MY                           
006790     ELSE                                                                 
006800       MOVE 1                   TO WS-PRKURS-MY                           
006810     END-IF                                                               
006820     MOVE WS-PRKURS-MY          TO WS-PRKURS-MY3                          
006830     .                                                                    
006840     EJECT                                                                
006850                                                                          
006860 C-EXECUTE SECTION.                                                       
006870     MOVE WC-IDFTG-PV           TO W-IDFTG                                
006880     MOVE IN-EKH-KDEKHHT        TO W-KDEKHHT                              
006890     MOVE IN-EKH-KDEKSHT        TO W-KDEKSHT                              
006900     IF IN-EKH-KDEKNIVA = 'TDDI'                                          
006910       MOVE 'DDI'               TO IN-EKH-KDEKNIVA                        
006920     END-IF                                                               
006930     MOVE IN-EKH-KDEKNIVA       TO W-KDEKNIVA                             
006940     PERFORM IMS-GU-WDH521                                                
006950     PERFORM IMS-GNP-WDH531                                               
006960                                                                          
006970     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
006980     MOVE IN-EKH-KDEKHHT        TO TEST-IDHTYP(1:3)                       
006990     MOVE IN-EKH-KDEKSHT        TO TEST-IDHTYP(4:3)                       
007000     IF IN-EKH-IDDISTR > ZERO                                             
007010       MOVE WS-KDVALISO-WDB1    TO WS-KDVALISO                            
007020       IF TEST-IDHTYP  = '302301'                                         
007030       OR TEST-IDHTYP  = '102120'                                         
007040       OR TEST-IDHTYP  = '102121'                                         
007050       OR TEST-IDHTYP  = '102122'                                         
007060       OR TEST-IDHTYP  = '102135'                                         
007070       OR TEST-IDHTYP  = '102145'                                         
007080         MOVE IN-EKH-KDVALISO   TO WS-KDVALISO                            
007090       END-IF                                                             
007100     ELSE                                                                 
007110       IF IN-EKH-KDEKHHT = '205'                                          
007120         MOVE WS-KDVALISO-WDB1  TO WS-KDVALISO                            
007130       ELSE                                                               
007140         MOVE IN-EKH-KDVALISO   TO WS-KDVALISO                            
007150       END-IF                                                             
007160     END-IF                                                               
007170     MOVE IN-EKH-PRKURS         TO WS-PRKURS                              
007180                                                                          
007190* HÄNDELSE 103-102 HAR RADPRISETS KDVALISO KVAR I FILEN FÖR               
007200* ATT KUNNA FÖLJA UPP OCH JÄMFÖRA DESSA TRANSAR MED LEVA1-FILER           
007210* BOKFÖRINGEN I SAP SKER DOCK ALLTID I SEK, DÄRFÖR BYTET HÄR:             
007220     IF IN-EKH-KDEKHHT = '103' AND IN-EKH-KDEKSHT = '102'                 
007230       MOVE 'SEK'               TO WS-KDVALISO                            
007240     END-IF                                                               
007250                                                                          
007260     IF IN-EKH-IDVERGL = WS-SPAR-IDVERGL                                  
007270       IF  ((IN-EKH-KDEKHHT = WS-SPAR-KDEKHHT                             
007280       AND IN-EKH-KDEKSHT = WS-SPAR-KDEKSHT)                              
007290       OR (IN-EKH-KDEKHHT = '303'))                                       
007300         MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                             
007310       ELSE                                                               
007320         IF WS-SPAR-KDEKHHT = '303'                                       
007330           MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                           
007340         ELSE                                                             
007350           MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                         
007360           MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                         
007370           IF WS-LOP = 9                                                  
007380             MOVE ZERO  TO WS-LOP                                         
007390           ELSE                                                           
007400             ADD +1     TO WS-LOP                                         
007410           END-IF                                                         
007420           MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                           
007430         END-IF                                                           
007440       END-IF                                                             
007450     ELSE                                                                 
007460       MOVE IN-EKH-IDVERGL TO WS-SPAR-IDVERGL                             
007470       MOVE IN-EKH-KDEKHHT TO WS-SPAR-KDEKHHT                             
007480       MOVE IN-EKH-KDEKSHT TO WS-SPAR-KDEKSHT                             
007490       IF WS-LOP = 9                                                      
007500         MOVE ZERO  TO WS-LOP                                             
007510       ELSE                                                               
007520         ADD +1     TO WS-LOP                                             
007530       END-IF                                                             
007540       MOVE WS-LOP  TO IN-EKH-IDVERGL(10:1)                               
007550     END-IF                                                               
007560                                                                          
007570* TEST OM HEADERPOST - GL/AR/AP SKALL SKRIVAS                             
007580     IF SYST-IDPTYP = '310'                                               
007590       IF IN-FIL-IDPGM = 'W4183000'                                       
007600         PERFORM CA-CREATE-WRITE-HEADER-GL                                
007610       ELSE                                                               
007620         PERFORM CB-CREATE-WRITE-HEADER-AR                                
007630       END-IF                                                             
007640     ELSE                                                                 
007650       IF SYST-IDPTYP = '210'                                             
007660         PERFORM CC-CREATE-WRITE-HEADER-AP                                
007670       ELSE                                                               
007680        IF (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '311'             
007690           AND IN-EKH-FLLSBOK = 'N')                                      
007700          PERFORM CA-CREATE-WRITE-HEADER-GL                               
007710        ELSE                                                              
007720                                                                          
007730* TEST OM BRYTNING PÅ VERIFIKATION                                        
007740         IF IN-EKH-DAVERDAT NOT = SPAR-DAVERDAT                           
007750         OR IN-EKH-IDVERGL  NOT = SPAR-IDVERGL                            
007760         OR IN-EKH-KDEKHHT  NOT = SPAR-KDEKHHT                            
007770         OR IN-EKH-KDEKSHT  NOT = SPAR-KDEKSHT                            
007780           MOVE IN-EKH-DAVERDAT   TO SPAR-DAVERDAT                        
007790           MOVE IN-EKH-IDVERGL    TO SPAR-IDVERGL                         
007800           MOVE IN-EKH-KDEKHHT    TO SPAR-KDEKHHT                         
007810           MOVE IN-EKH-KDEKSHT    TO SPAR-KDEKSHT                         
007820           MOVE IN-EKH-KDANMORS   TO WS-KDANMORS-HDR                      
007830                                                                          
007840* NEDANSTÅENDE HUVUDHÄNDELSETYPER SKALL INTE GENERERA                     
007850* HEADER-POST TILL HUVUDBOKEN                                             
007860           IF (IN-EKH-KDEKHHT = '102'                                     
007870           AND IN-EKH-KDEKSHT = '107')                                    
007880           OR (IN-EKH-KDEKHHT = '102'                                     
007890           AND IN-EKH-KDEKSHT = '120')                                    
007900           OR (IN-EKH-KDEKHHT = '102'                                     
007910           AND IN-EKH-KDEKSHT = '135')                                    
007920           OR (IN-EKH-KDEKHHT = '102'                                     
007930           AND IN-EKH-KDEKSHT = '145')                                    
007940           OR (IN-EKH-KDEKHHT = '202'                                     
007950           AND IN-EKH-KDEKSHT = '204')                                    
007960           OR (IN-EKH-KDEKHHT = '204'                                     
007970           AND NOT IN-EKH-KDEKSHT = '203')                                
007980           OR (IN-EKH-KDEKHHT = '205')                                    
007990           OR (IN-EKH-KDEKHHT = '303')                                    
008000           OR (IN-EKH-KDEKHHT = '304')                                    
008010             CONTINUE                                                     
008020           ELSE                                                           
008030             PERFORM CA-CREATE-WRITE-HEADER-GL                            
008040           END-IF                                                         
008050         END-IF                                                           
008060        END-IF                                                            
008070       END-IF                                                             
008080     END-IF                                                               
008090                                                                          
008100     PERFORM UNTIL SEGMENT-SAKNAS                                         
008110       PERFORM CJ-BUILD-COMMON-LOG-PART                                   
008120                                                                          
008130* TEST FÖR HOPP TILL RÄTT STYRMODUL - RADPOSTER                           
008140       IF SYST-IDPTYP = '610'                                             
008150         PERFORM CD-BUILD-COMMON-610-PART                                 
008160         PERFORM CE-SCHEDULE-LINE-GL                                      
008170       ELSE                                                               
008180         IF SYST-IDPTYP = '310'                                           
008190           PERFORM CF-BUILD-COMMON-310-PART                               
008200           PERFORM CG-SCHEDULE-LINE-AR                                    
008210         ELSE                                                             
008220           PERFORM CH-BUILD-COMMON-210-PART                               
008230           PERFORM CI-SCHEDULE-LINE-AP                                    
008240         END-IF                                                           
008250       END-IF                                                             
008260       PERFORM IMS-GNP-WDH531                                             
008270     END-PERFORM                                                          
008280     .                                                                    
008290     EJECT                                                                
008300                                                                          
008310 CA-CREATE-WRITE-HEADER-GL SECTION.                                       
008320     MOVE SPACE                   TO R3-HEAD-R3                           
008330     MOVE '600'                   TO R3-HEAD-RECORD-TYPE                  
008340     MOVE 'SEPV'                  TO R3-HEAD-COMPANY-CODE                 
008350     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
008360     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
008370     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
008380     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
008390     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
008400       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
008410     ELSE                                                                 
008420       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
008430     END-IF                                                               
008440     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
008450     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
008460     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
008470     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
008480     IF IN-EKH-IDDISTR = 9111                                             
008490     OR IN-EKH-IDDISTR = 9161                                             
008500     OR IN-EKH-IDDISTR = 9162                                             
008510       MOVE 'CNY'         TO WS-KDVALISO                                  
008520     END-IF                                                               
008530     IF IN-EKH-IDDISTR = 9211                                             
008540     OR IN-EKH-IDDISTR = 9261                                             
008550     OR IN-EKH-IDDISTR = 9262                                             
008560       MOVE 'USD'         TO WS-KDVALISO                                  
008570     END-IF                                                               
008580     IF (IN-EKH-IDDISTR = 8661                                            
008590     OR IN-EKH-IDDISTR = 8662)                                            
008600     AND IN-EKH-IDKUNDNR = 66                                             
008610       MOVE 'MYR'         TO WS-KDVALISO                                  
008620     END-IF                                                               
008630     IF (IN-EKH-KDEKHHT = '102'                                           
008640     AND IN-EKH-KDEKSHT = '121')                                          
008650     OR (IN-EKH-KDEKHHT = '102'                                           
008660     AND IN-EKH-KDEKSHT = '122')                                          
008670       MOVE 'SEK' TO WS-KDVALISO                                          
008680     END-IF                                                               
008690     IF WS-KDVALISO = 'SEK'                                               
008700       MOVE WS-PRKURS           TO R3-HEAD-EXCHANGE-RATE                  
008710     ELSE                                                                 
008720       MOVE WS-KDVALISO         TO CURR-KDVALISO-ROW                      
008730       MOVE WS-TIMM             TO W-DATE-AAMM(3:2)                       
008740       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
008750       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
008760       IF CURR-KDSVAR = ' '                                               
008770         IF IN-EKH-IDDISTR > ZERO                                         
008780           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
008790         ELSE                                                             
008800           IF WS-PRKURS   = ZERO                                          
008810             MOVE 1             TO WS-PRKURS                              
008820           END-IF                                                         
008830         END-IF                                                           
008840       ELSE                                                               
008850         MOVE 1                 TO WS-PRKURS                              
008860       END-IF                                                             
008870       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
008880                                       CURR-REVALUTA-TO                   
008890       END-COMPUTE                                                        
008900       IF CURR-REVALUTA-TO = +1                                           
008910         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
008920       END-IF                                                             
008930       IF CURR-REVALUTA-TO = +10                                          
008940         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
008950       END-IF                                                             
008960       IF CURR-REVALUTA-TO = +100                                         
008970         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
008980       END-IF                                                             
008990     END-IF                                                               
009000     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
009010     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
009020     IF IN-EKH-KDEKHHT = '301'                                            
009030       MOVE '3XX'                 TO WS-TEXT-KDEKSHT                      
009040     ELSE                                                                 
009050       MOVE IN-EKH-KDEKSHT        TO WS-TEXT-KDEKSHT                      
009060     END-IF                                                               
009070     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
009080     MOVE ZERO                    TO R3-HEAD-TRANSLATE-DATE               
009090     MOVE JA                      TO WS-HEADER-SW                         
009100     MOVE NEJ                     TO WS-LINE-SW                           
009110                                                                          
009120* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYPER TILL AGGREGERINGSFIL             
009130     IF IN-EKH-KDEKHHT = '204' OR '203' OR '201'                          
009140     OR(IN-EKH-KDEKHHT = '303'                                            
009150     AND (IN-FIL-IDPGM = 'W4183300' OR 'W4184500'))                       
009160     OR (IN-EKH-KDEKHHT = '102'                                           
009170     AND IN-EKH-KDEKSHT = '121')                                          
009180     OR IN-EKH-KDEKHHT = '501'                                            
009190       PERFORM S004-WRITE-W51073A-HEAD                                    
009200     ELSE                                                                 
009210*** DO NOT WRITE HEADER FOR 403-408 IF 303-3XX LEVEL IS PRESENT           
009220       IF (IN-FIL-IDPGM = 'W4183000'                                      
009230       AND IN-EKH-KDEKHHT = '403'                                         
009240       AND WS-KDANMORS-HDR-YES)                                           
009250         CONTINUE                                                         
009260       ELSE                                                               
009270         PERFORM S002-WRITE-W51071A-HEAD                                  
009280       END-IF                                                             
009290     END-IF                                                               
009300     .                                                                    
009310     EJECT                                                                
009320                                                                          
009330 CB-CREATE-WRITE-HEADER-AR SECTION.                                       
009340     MOVE SPACE                   TO R3-HEAD-R3                           
009350     MOVE '300'                   TO R3-HEAD-RECORD-TYPE                  
009360     MOVE 'SEPV'                  TO R3-HEAD-COMPANY-CODE                 
009370                                     R3-HEAD-CONTROL-AREA                 
009380     MOVE SYST-KDDOKTYP           TO R3-HEAD-DOCUMENT-TYPE                
009390     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
009400     MOVE WS-LOP                  TO IN-EKH-IDVERGL(10:1)                 
009410     MOVE IN-EKH-IDVERGL          TO R3-HEAD-DOCUMENT-NO-REF              
009420     MOVE IN-EKH-DAVERDAT         TO R3-HEAD-DOCUMENT-DATE                
009430     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
009440       MOVE IN-EKH-DAVERDAT       TO R3-HEAD-POSTING-DATE                 
009450     ELSE                                                                 
009460       MOVE WS-DAREGDAT           TO R3-HEAD-POSTING-DATE                 
009470     END-IF                                                               
009480     MOVE '1    '                 TO R3-HEAD-EXCHANGE-TOFACT              
009490     MOVE '1    '                 TO R3-HEAD-EXCHANGE-FRFACT              
009500     MOVE WS-KDVALISO             TO R3-HEAD-CURRENCY                     
009510     IF WS-KDVALISO = 'SEK'                                               
009520       MOVE WS-PRKURS             TO R3-HEAD-EXCHANGE-RATE                
009530     ELSE                                                                 
009540       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
009550       IF IN-FIL-IDPGM = 'W4183300'                                       
009560         IF IN-EKH-DAAVIDAT > ZERO                                        
009570           MOVE IN-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                        
009580           MOVE IN-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                        
009590         ELSE                                                             
009600           MOVE WS-TIAA              TO WS-TIAA-CR                        
009610           MOVE WS-TIMM              TO WS-TIMM-CR                        
009620         END-IF                                                           
009630       ELSE                                                               
009640         MOVE WS-TIAA                TO WS-TIAA-CR                        
009650         MOVE WS-TIMM                TO WS-TIMM-CR                        
009660       END-IF                                                             
009670       MOVE WS-TIAA-CR          TO W-DATE-AAMM(1:2)                       
009680       MOVE WS-TIMM-CR          TO W-DATE-AAMM(3:2)                       
009690       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
009700       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
009710       IF CURR-KDSVAR = ' '                                               
009720         IF IN-EKH-IDDISTR > ZERO                                         
009730           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
009740         ELSE                                                             
009750           IF WS-PRKURS = ZERO                                            
009760             MOVE 1             TO WS-PRKURS                              
009770           END-IF                                                         
009780         END-IF                                                           
009790       ELSE                                                               
009800         MOVE 1                 TO WS-PRKURS                              
009810       END-IF                                                             
009820       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
009830                                       CURR-REVALUTA-TO                   
009840       END-COMPUTE                                                        
009850       IF CURR-REVALUTA-TO = +1                                           
009860         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
009870       END-IF                                                             
009880       IF CURR-REVALUTA-TO = +10                                          
009890         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
009900       END-IF                                                             
009910       IF CURR-REVALUTA-TO = +100                                         
009920         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
009930       END-IF                                                             
009940     END-IF                                                               
009950     MOVE 'PULS'                  TO WS-TEXT-FEEDER-SYSTEM                
009960     MOVE IN-EKH-KDEKHHT          TO WS-TEXT-KDEKHHT                      
009970     MOVE IN-EKH-KDEKSHT          TO WS-TEXT-KDEKSHT                      
009980     MOVE WS-TEXT                 TO R3-HEAD-TEXT                         
009990     MOVE JA                      TO WS-HEADER-SW                         
010000     MOVE NEJ                     TO WS-LINE-SW                           
010010                                                                          
010020* ENBART NEDANSTÅENDE HUVUDHÄNDELSETYP TILL W51073A                       
010030     IF IN-EKH-KDEKHHT = '204'                                            
010040     OR(IN-EKH-KDEKHHT = '303'                                            
010050     AND (IN-FIL-IDPGM = 'W4184500' OR 'W4183300'))                       
010060     OR IN-EKH-KDEKHHT = '205'                                            
010070     OR IN-EKH-KDEKHHT = '304'                                            
010080     OR IN-EKH-KDEKHHT = '501'                                            
010090       PERFORM S004-WRITE-W51073A-HEAD                                    
010100     ELSE                                                                 
010110       PERFORM S002-WRITE-W51071A-HEAD                                    
010120     END-IF                                                               
010130     .                                                                    
010140     EJECT                                                                
010150                                                                          
010160 CC-CREATE-WRITE-HEADER-AP SECTION.                                       
010170     MOVE SPACE                 TO R3-HEAD-R3                             
010180     MOVE '200'                 TO R3-HEAD-RECORD-TYPE                    
010190     MOVE 'SEPV'                TO R3-HEAD-COMPANY-CODE                   
010200                                     R3-HEAD-CONTROL-AREA                 
010210     MOVE SYST-KDDOKTYP         TO R3-HEAD-DOCUMENT-TYPE                  
010220     MOVE WS-LOP                TO IN-EKH-IDVERGL(10:1)                   
010230     MOVE IN-EKH-IDVERGL        TO R3-HEAD-DOCUMENT-NO-REF                
010240     MOVE IN-EKH-DAVERDAT       TO R3-HEAD-DOCUMENT-DATE                  
010250     IF WS-DAREGDAT < IN-EKH-DAVERDAT                                     
010260       MOVE IN-EKH-DAVERDAT     TO R3-HEAD-POSTING-DATE                   
010270     ELSE                                                                 
010280       MOVE WS-DAREGDAT         TO R3-HEAD-POSTING-DATE                   
010290     END-IF                                                               
010300     MOVE '1    '               TO R3-HEAD-EXCHANGE-TOFACT                
010310     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
010320     MOVE WS-KDVALISO           TO R3-HEAD-CURRENCY                       
010330     IF WS-KDVALISO = 'SEK'                                               
010340       MOVE WS-PRKURS           TO R3-HEAD-EXCHANGE-RATE                  
010350       MOVE '1    '             TO R3-HEAD-EXCHANGE-FRFACT                
010360     ELSE                                                                 
010370       MOVE WS-KDVALISO         TO CURR-KDVALISO-ROW                      
010380       MOVE WS-TIAA             TO W-DATE-AAMM(1:2)                       
010390       MOVE WS-TIMM             TO W-DATE-AAMM(3:2)                       
010400       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
010410       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
010420       IF CURR-KDSVAR = ' '                                               
010430         IF IN-EKH-IDDISTR > ZERO                                         
010440           MOVE CURR-PRKURS-NEW TO WS-PRKURS                              
010450         ELSE                                                             
010460           IF WS-PRKURS   = ZERO                                          
010470             MOVE 1             TO WS-PRKURS                              
010480           END-IF                                                         
010490         END-IF                                                           
010500       ELSE                                                               
010510         MOVE 1                 TO WS-PRKURS                              
010520       END-IF                                                             
010530       COMPUTE R3-HEAD-EXCHANGE-RATE = WS-PRKURS *                        
010540                                       CURR-REVALUTA-TO                   
010550       END-COMPUTE                                                        
010560       IF CURR-REVALUTA-TO = +1                                           
010570         MOVE '1    '           TO R3-HEAD-EXCHANGE-FRFACT                
010580       END-IF                                                             
010590       IF CURR-REVALUTA-TO = +10                                          
010600         MOVE '10   '           TO R3-HEAD-EXCHANGE-FRFACT                
010610       END-IF                                                             
010620       IF CURR-REVALUTA-TO = +100                                         
010630         MOVE '100  '           TO R3-HEAD-EXCHANGE-FRFACT                
010640       END-IF                                                             
010650     END-IF                                                               
010660**** IF THIS IS CHANGED TO READ WDB6 TO FETCH CURRENCY, IT WILL           
010670**** IMPACT THE WDH5 CALL STATUS IN C-EXECUTE SECTION.                    
010680     IF (IN-EKH-KDEKHHT = '102'                                           
010690     AND IN-EKH-KDEKSHT = '120')                                          
010700       MOVE IN-EKH-KDVALISO     TO R3-HEAD-CURRENCY                       
010710       IF IN-EKH-KDVALISO = 'CNY'                                         
010720         MOVE WS-PRKURS-CN3     TO R3-HEAD-EXCHANGE-RATE                  
010730       END-IF                                                             
010740       IF IN-EKH-KDVALISO = 'USD'                                         
010750         MOVE WS-PRKURS-US3     TO R3-HEAD-EXCHANGE-RATE                  
010760       END-IF                                                             
010770       IF IN-EKH-KDVALISO = 'MYR'                                         
010780         MOVE WS-PRKURS-MY3     TO R3-HEAD-EXCHANGE-RATE                  
010790       END-IF                                                             
010800     END-IF                                                               
010810**** !!!!                                                                 
010820     MOVE 'PULS'                TO WS-TEXT-FEEDER-SYSTEM                  
010830     MOVE IN-EKH-KDEKHHT        TO WS-TEXT-KDEKHHT                        
010840     MOVE IN-EKH-KDEKSHT        TO WS-TEXT-KDEKSHT                        
010850     MOVE WS-TEXT               TO R3-HEAD-TEXT                           
010860     MOVE JA                    TO WS-HEADER-SW                           
010870     MOVE NEJ                   TO WS-LINE-SW                             
010880     PERFORM S004-WRITE-W51073A-HEAD                                      
010890     .                                                                    
010900     EJECT                                                                
010910                                                                          
010920 CD-BUILD-COMMON-610-PART SECTION.                                        
010930     MOVE SPACE               TO R3-LINE-R3                               
010940     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
010950                                 R3-LINE-DUE-DATE                         
010960                                 R3-LINE-AMOUNT                           
010970                                 R3-LINE-AMOUNT-LC                        
010980                                 R3-LINE-TAX-AMOUNT                       
010990                                 R3-LINE-TAX-AMOUNT-LC                    
011000                                 R3-LINE-NUMBER-OF-DAYS                   
011010                                 R3-LINE-QUANTITY                         
011020                                 R3-LINE-SAMNR                            
011030     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
011040     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
011050     MOVE 'SEPV'              TO R3-LINE-COMPANY-CODE                     
011060     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
011070     MOVE IN-EKH-IDFAKT-EXP   TO R3-LINE-REFKEY3                          
011080     IF SYST-KDPOST = '50'                                                
011090       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
011100     ELSE                                                                 
011110       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
011120     END-IF                                                               
011130     IF SYST-IDPRCTR NOT = SPACE                                          
011140       MOVE SYST-IDPRCTR             TO WS-PRCTR                          
011150       MOVE WS-PRCTR                 TO R3-LINE-PROFIT-CENTER             
011160       IF WS-PRCTR-PRODSL = '??'                                          
011170         MOVE IN-EKH-KDPRODSL        TO TEST-KDPRODSL                     
011180         IF KDPRODSL-LOCAL                                                
011190           MOVE '9X'                 TO WS-PRCTR-PRODSL                   
011200           MOVE  WS-PRCTR             TO R3-LINE-PROFIT-CENTER            
011210         ELSE                                                             
011220          IF KDPRODSL-LYNK                                                
011230            MOVE 'LYNK'            TO WS-PRCTR-LYNK(1:4)                  
011240            MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP-L              
011250            MOVE WS-PRCTR-PRODSL-DISP-L TO WS-PRCTR-PRODSL-L              
011260            MOVE WS-PRCTR-LYNK     TO R3-LINE-PROFIT-CENTER               
011270          ELSE                                                            
011280            MOVE IN-EKH-KDPRODSL      TO WS-PRCTR-PRODSL-DISP             
011290            MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR-PRODSL                  
011300            MOVE WS-PRCTR         TO R3-LINE-PROFIT-CENTER                
011310          END-IF                                                          
011320         END-IF                                                           
011330       END-IF                                                             
011340     END-IF                                                               
011350     .                                                                    
011360     EJECT                                                                
011370                                                                          
011380 CE-SCHEDULE-LINE-GL SECTION.                                             
011390     MOVE NEJ                     TO WS-HEADER-SW                         
011400     MOVE JA                      TO WS-LINE-SW                           
011410     MOVE IN-EKH-KDPRODSL         TO TEST-KDPRODSL                        
011420     EVALUATE IN-EKH-KDEKHHT                                              
011430     WHEN '101'                                                           
011440          PERFORM CEA-MAIN-EVENT-101                                      
011450     WHEN '102'                                                           
011460          PERFORM CEB-MAIN-EVENT-102                                      
011470     WHEN '103'                                                           
011480          PERFORM CEC-MAIN-EVENT-103                                      
011490     WHEN '201'                                                           
011500          PERFORM CED-MAIN-EVENT-201                                      
011510     WHEN '202'                                                           
011520          PERFORM CEE-MAIN-EVENT-202                                      
011530     WHEN '203'                                                           
011540          PERFORM CEF-MAIN-EVENT-203                                      
011550     WHEN '204'                                                           
011560          PERFORM CEG-MAIN-EVENT-204                                      
011570     WHEN '205'                                                           
011580          PERFORM CET-MAIN-EVENT-205                                      
011590     WHEN '301'                                                           
011600          PERFORM CEH-MAIN-EVENT-301                                      
011610     WHEN '302'                                                           
011620          PERFORM CEI-MAIN-EVENT-302                                      
011630     WHEN '303'                                                           
011640          PERFORM CEJ-MAIN-EVENT-303                                      
011650     WHEN '304'                                                           
011660          PERFORM CEU-MAIN-EVENT-304                                      
011670     WHEN '401'                                                           
011680          PERFORM CEK-MAIN-EVENT-401                                      
011690     WHEN '402'                                                           
011700          PERFORM CEL-MAIN-EVENT-402                                      
011710     WHEN '403'                                                           
011720          PERFORM CEM-MAIN-EVENT-403                                      
011730     WHEN '404'                                                           
011740          PERFORM CEN-MAIN-EVENT-404                                      
011750     WHEN '405'                                                           
011760          PERFORM CEO-MAIN-EVENT-405                                      
011770     WHEN '406'                                                           
011780          PERFORM CEP-MAIN-EVENT-406                                      
011790     WHEN '501'                                                           
011800          PERFORM CEQ-MAIN-EVENT-501                                      
011810     WHEN '502'                                                           
011820          PERFORM CER-MAIN-EVENT-502                                      
011830     WHEN '503'                                                           
011840          PERFORM CES-MAIN-EVENT-503                                      
011850     WHEN '505'                                                           
011860          PERFORM CEU-MAIN-EVENT-505                                      
011870     END-EVALUATE                                                         
011880     .                                                                    
011890     EJECT                                                                
011900                                                                          
011910 CEA-MAIN-EVENT-101 SECTION.                                              
011920     EVALUATE IN-EKH-KDEKNIVA                                             
011930     WHEN 'DET'                                                           
011940       IF SYST-IDSEKVNR = 1                                               
011950         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
011960         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
011970         COMPUTE R3-LINE-AMOUNT-LC =                                      
011980                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
011990         END-COMPUTE                                                      
012000         PERFORM S02-WRITE-W51071A                                        
012010       END-IF                                                             
012020                                                                          
012030       IF SYST-IDSEKVNR = 2                                               
012040         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
012050         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
012060         IF KDPRODSL-LYNK                                                 
012070           MOVE '1'               TO WS-ACCOUNT-5                         
012080           MOVE '0'               TO WS-ACCOUNT-5A                        
012090           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
012100           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
012110           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
012120           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
012130           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
012140         END-IF                                                           
012150         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
012160         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
012170                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
012180         END-COMPUTE                                                      
012190         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
012200         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
012210         MOVE SPACE               TO WS-ALLOCATE-REF                      
012220         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
012230         PERFORM S02-WRITE-W51071A                                        
012240       END-IF                                                             
012250     END-EVALUATE                                                         
012260     .                                                                    
012270     EJECT                                                                
012280                                                                          
012290 CEB-MAIN-EVENT-102 SECTION.                                              
012300     EVALUATE IN-EKH-KDEKSHT                                              
012310     WHEN '101'                                                           
012320          PERFORM CEBA-SUB-EVENT-102-101                                  
012330     WHEN '102'                                                           
012340          PERFORM CEBB-SUB-EVENT-102-102                                  
012350     WHEN '105'                                                           
012360          PERFORM CEBC-SUB-EVENT-102-105                                  
012370     WHEN '107'                                                           
012380          PERFORM CEBD-SUB-EVENT-102-107                                  
012390     WHEN '109'                                                           
012400          PERFORM CEBE-SUB-EVENT-102-109                                  
012410     WHEN '110'                                                           
012420          PERFORM CEBE-SUB-EVENT-102-110                                  
012430     WHEN '120'                                                           
012440          PERFORM CEBF-SUB-EVENT-102-120                                  
012450     WHEN '121'                                                           
012460          PERFORM CEBG-SUB-EVENT-102-121                                  
012470     WHEN '122'                                                           
012480          PERFORM CEBH-SUB-EVENT-102-122                                  
012490     WHEN '135'                                                           
012500          PERFORM CEBI-SUB-EVENT-102-135                                  
012510     WHEN '145'                                                           
012520          PERFORM CEBI-SUB-EVENT-102-145                                  
012530     END-EVALUATE                                                         
012540     .                                                                    
012550     EJECT                                                                
012560                                                                          
012570 CEBA-SUB-EVENT-102-101 SECTION.                                          
012580     EVALUATE IN-EKH-KDEKNIVA                                             
012590     WHEN 'DET'                                                           
012600       IF SYST-IDSEKVNR = 1                                               
012610         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
012620         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
012630         COMPUTE R3-LINE-AMOUNT-LC =                                      
012640                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
012650         END-COMPUTE                                                      
012660         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
012670         MOVE SYST-IDKST          TO WS-RED-IDKST                         
012680         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
012690         PERFORM S02-WRITE-W51071A                                        
012700       END-IF                                                             
012710                                                                          
012720       IF SYST-IDSEKVNR = 2                                               
012730         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
012740         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
012750         IF KDPRODSL-LYNK                                                 
012760           MOVE '1'               TO WS-ACCOUNT-5                         
012770           MOVE '0'               TO WS-ACCOUNT-5A                        
012780           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
012790           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
012800           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
012810           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
012820           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
012830         END-IF                                                           
012840         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
012850         COMPUTE R3-LINE-AMOUNT-LC =                                      
012860                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
012870         END-COMPUTE                                                      
012880         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
012890         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
012900         MOVE SPACE               TO WS-ALLOCATE-REF                      
012910         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
012920         PERFORM S02-WRITE-W51071A                                        
012930       END-IF                                                             
012940     END-EVALUATE                                                         
012950     .                                                                    
012960                                                                          
012970 CEBB-SUB-EVENT-102-102 SECTION.                                          
012980     EVALUATE IN-EKH-KDEKNIVA                                             
012990     WHEN 'DET'                                                           
013000       IF SYST-IDSEKVNR = 1                                               
013010         IF IN-EKH-KVANTAL > 0                                            
013020           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
013030           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
013040           IF KDPRODSL-LYNK                                               
013050             MOVE '1'               TO WS-ACCOUNT-5                       
013060             MOVE '0'               TO WS-ACCOUNT-5A                      
013070             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
013080             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
013090             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
013100             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
013110             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
013120           END-IF                                                         
013130           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
013140           COMPUTE R3-LINE-AMOUNT-LC =                                    
013150                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
013160           END-COMPUTE                                                    
013170           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
013180           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
013190           MOVE SPACE               TO WS-ALLOCATE-REF                    
013200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
013210           PERFORM S02-WRITE-W51071A                                      
013220         END-IF                                                           
013230       END-IF                                                             
013240                                                                          
013250       IF SYST-IDSEKVNR = 2                                               
013260         IF IN-EKH-KVANTAL > 0                                            
013270           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
013280           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
013290           COMPUTE R3-LINE-AMOUNT-LC =                                    
013300                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
013310           END-COMPUTE                                                    
013320           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
013330           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
013340           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
013350           PERFORM S02-WRITE-W51071A                                      
013360         END-IF                                                           
013370       END-IF                                                             
013380                                                                          
013390       IF SYST-IDSEKVNR = 3                                               
013400         IF IN-EKH-KVANTAL < 0                                            
013410           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
013420           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
013430           IF KDPRODSL-LYNK                                               
013440             MOVE '1'               TO WS-ACCOUNT-5                       
013450             MOVE '0'               TO WS-ACCOUNT-5A                      
013460             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
013470             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
013480             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
013490             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
013500             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
013510           END-IF                                                         
013520           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
013530           COMPUTE R3-LINE-AMOUNT-LC =                                    
013540                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
013550           END-COMPUTE                                                    
013560           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
013570           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
013580           MOVE SPACE               TO WS-ALLOCATE-REF                    
013590           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
013600           PERFORM S02-WRITE-W51071A                                      
013610         END-IF                                                           
013620       END-IF                                                             
013630                                                                          
013640       IF SYST-IDSEKVNR = 4                                               
013650         IF IN-EKH-KVANTAL < 0                                            
013660           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
013670           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
013680           COMPUTE R3-LINE-AMOUNT-LC =                                    
013690                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
013700           END-COMPUTE                                                    
013710           MOVE IN-EKH-IDKST        TO WS-RED-IDKST                       
013720           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
013730           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
013740           PERFORM S02-WRITE-W51071A                                      
013750         END-IF                                                           
013760       END-IF                                                             
013770     END-EVALUATE                                                         
013780     .                                                                    
013790     EJECT                                                                
013800                                                                          
013810 CEBC-SUB-EVENT-102-105 SECTION.                                          
013820     EVALUATE IN-EKH-KDEKNIVA                                             
013830     WHEN 'DET'                                                           
013840       IF IN-EKH-KVANTAL > 0                                              
013850         IF SYST-IDSEKVNR = 1                                             
013860           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
013870           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
013880           COMPUTE R3-LINE-AMOUNT-LC =                                    
013890                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
013900           END-COMPUTE                                                    
013910           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
013920           PERFORM S02-WRITE-W51071A                                      
013930         END-IF                                                           
013940                                                                          
013950         IF SYST-IDSEKVNR = 2                                             
013960           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
013970           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
013980           IF KDPRODSL-LYNK                                               
013990             MOVE '1'               TO WS-ACCOUNT-5                       
014000             MOVE '0'               TO WS-ACCOUNT-5A                      
014010             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
014020             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
014030             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
014040             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
014050             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
014060           END-IF                                                         
014070           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
014080           COMPUTE R3-LINE-AMOUNT-LC =                                    
014090                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
014100           END-COMPUTE                                                    
014110           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
014120           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
014130           MOVE SPACE               TO WS-ALLOCATE-REF                    
014140           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
014150           PERFORM S02-WRITE-W51071A                                      
014160         END-IF                                                           
014170       END-IF                                                             
014180                                                                          
014190       IF IN-EKH-KVANTAL < 0                                              
014200         IF SYST-IDSEKVNR = 3                                             
014210           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
014220           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
014230           COMPUTE R3-LINE-AMOUNT-LC =                                    
014240                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
014250           END-COMPUTE                                                    
014260           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
014270           PERFORM S02-WRITE-W51071A                                      
014280         END-IF                                                           
014290                                                                          
014300         IF SYST-IDSEKVNR = 4                                             
014310           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
014320           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
014330           IF KDPRODSL-LYNK                                               
014340             MOVE '1'               TO WS-ACCOUNT-5                       
014350             MOVE '0'               TO WS-ACCOUNT-5A                      
014360             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
014370             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
014380             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
014390             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
014400             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
014410           END-IF                                                         
014420           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
014430           COMPUTE R3-LINE-AMOUNT-LC =                                    
014440                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
014450           END-COMPUTE                                                    
014460           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
014470           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
014480           MOVE SPACE               TO WS-ALLOCATE-REF                    
014490           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
014500           PERFORM S02-WRITE-W51071A                                      
014510         END-IF                                                           
014520       END-IF                                                             
014530     END-EVALUATE                                                         
014540     .                                                                    
014550     EJECT                                                                
014560                                                                          
014570 CEBD-SUB-EVENT-102-107 SECTION.                                          
014580     EVALUATE IN-EKH-KDEKNIVA                                             
014590     WHEN 'DET'                                                           
014600       IF SYST-IDSEKVNR = 1                                               
014610         IF IN-EKH-KVANTAL < 0                                            
014620           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
014630           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
014640           IF KDPRODSL-LYNK                                               
014650             MOVE '1'               TO WS-ACCOUNT-5                       
014660             MOVE '0'               TO WS-ACCOUNT-5A                      
014670             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
014680             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
014690             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
014700             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
014710             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
014720           END-IF                                                         
014730           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
014740           COMPUTE R3-LINE-AMOUNT-LC =                                    
014750                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
014760           END-COMPUTE                                                    
014770           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
014780           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
014790           MOVE SPACE               TO WS-ALLOCATE-REF                    
014800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
014810           PERFORM S03-WRITE-W51072                                       
014820         END-IF                                                           
014830       END-IF                                                             
014840                                                                          
014850       IF SYST-IDSEKVNR = 2                                               
014860         IF IN-EKH-KVANTAL > 0                                            
014870           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
014880           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
014890           IF KDPRODSL-LYNK                                               
014900             MOVE '1'               TO WS-ACCOUNT-5                       
014910             MOVE '0'               TO WS-ACCOUNT-5A                      
014920             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
014930             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
014940             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
014950             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
014960             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
014970           END-IF                                                         
014980           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
014990           COMPUTE R3-LINE-AMOUNT-LC =                                    
015000                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
015010           END-COMPUTE                                                    
015020           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
015030           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
015040           MOVE SPACE               TO WS-ALLOCATE-REF                    
015050           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
015060           PERFORM S03-WRITE-W51072                                       
015070         END-IF                                                           
015080       END-IF                                                             
015090                                                                          
015100     WHEN 'KALK'                                                          
015110       IF SYST-IDSEKVNR = 1                                               
015120         IF IN-EKH-SUBEL > 0                                              
015130           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015140           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015150           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015160           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
015170           PERFORM S04-WRITE-W51073A                                      
015180         END-IF                                                           
015190       END-IF                                                             
015200                                                                          
015210       IF SYST-IDSEKVNR = 2                                               
015220         IF IN-EKH-SUBEL < 0                                              
015230           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015250           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015260           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
015270           PERFORM S04-WRITE-W51073A                                      
015280         END-IF                                                           
015290       END-IF                                                             
015300                                                                          
015310     WHEN 'HEMT'                                                          
015320       IF SYST-IDSEKVNR = 1                                               
015330         IF IN-EKH-SUBEL > 0                                              
015340           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015350           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015360           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015370           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
015380           PERFORM S04-WRITE-W51073A                                      
015390         END-IF                                                           
015400       END-IF                                                             
015410                                                                          
015420       IF SYST-IDSEKVNR = 2                                               
015430         IF IN-EKH-SUBEL < 0                                              
015440           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015450           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015460           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015470           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
015480           PERFORM S04-WRITE-W51073A                                      
015490         END-IF                                                           
015500       END-IF                                                             
015510                                                                          
015520     WHEN 'DIFF'                                                          
015530       IF SYST-IDSEKVNR = 1                                               
015540         IF IN-EKH-SUBEL < 0                                              
015550           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015560           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015570           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015580           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
015590           PERFORM S04-WRITE-W51073A                                      
015600         END-IF                                                           
015610       END-IF                                                             
015620                                                                          
015630       IF SYST-IDSEKVNR = 2                                               
015640         IF IN-EKH-SUBEL > 0                                              
015650           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015660           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015670           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015680           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
015690           PERFORM S04-WRITE-W51073A                                      
015700         END-IF                                                           
015710       END-IF                                                             
015720                                                                          
015730     WHEN 'ARB'                                                           
015740       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
015750         MOVE IN-EKH-IDDC-SEND      TO W-IDDC-B6                          
015760         PERFORM IMS-GU-WDB601                                            
015770       END-IF                                                             
015780                                                                          
015790       IF SYST-IDSEKVNR = 1                                               
015800         IF IN-EKH-SUBEL < 0                                              
015810           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
015820           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
015830           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
015840                                                                          
015850* HÄR STYR DC-TILLHÖRIGHET VILKET ANALYSNR SOM SKALL ANVÄNDAS             
015860           IF DCS-CDC                                                     
015870             MOVE '158600010143'    TO R3-LINE-ORDER                      
015880           ELSE                                                           
015890             MOVE '158600010144'    TO R3-LINE-ORDER                      
015900           END-IF                                                         
015910           MOVE SYST-IDKST          TO WS-RED-IDKST                       
015920           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
015930           PERFORM S04-WRITE-W51073A                                      
015940         END-IF                                                           
015950       END-IF                                                             
015960                                                                          
015970       IF SYST-IDSEKVNR = 2                                               
015980         IF IN-EKH-SUBEL > 0                                              
015990           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
016000           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
016010           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
016020                                                                          
016030* HÄR STYR DC-TILLHÖRIGHET VILKET ANALYSNR SOM SKALL ANVÄNDAS             
016040           IF DCS-CDC                                                     
016050             MOVE '158600010143'    TO R3-LINE-ORDER                      
016060           ELSE                                                           
016070             MOVE '158600010144'    TO R3-LINE-ORDER                      
016080           END-IF                                                         
016090           MOVE SYST-IDKST          TO WS-RED-IDKST                       
016100           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
016110           PERFORM S04-WRITE-W51073A                                      
016120         END-IF                                                           
016130       END-IF                                                             
016140                                                                          
016150     WHEN 'TRP'                                                           
016160       IF SYST-IDSEKVNR = 1                                               
016170         IF IN-EKH-SUBEL < 0                                              
016180           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
016190           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
016200           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
016210           MOVE SYST-IDKST          TO WS-RED-IDKST                       
016220           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
016230           PERFORM S04-WRITE-W51073A                                      
016240         END-IF                                                           
016250       END-IF                                                             
016260                                                                          
016270       IF SYST-IDSEKVNR = 2                                               
016280         IF IN-EKH-SUBEL > 0                                              
016290           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
016300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
016310           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
016320           MOVE SYST-IDKST          TO WS-RED-IDKST                       
016330           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
016340           PERFORM S04-WRITE-W51073A                                      
016350         END-IF                                                           
016360       END-IF                                                             
016370                                                                          
016380     WHEN 'MATR'                                                          
016390       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
016400         MOVE IN-EKH-IDDC-SEND      TO W-IDDC-B6                          
016410         PERFORM IMS-GU-WDB601                                            
016420       END-IF                                                             
016430                                                                          
016440       IF SYST-IDSEKVNR = 1                                               
016450         IF IN-EKH-SUBEL < 0                                              
016460           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
016470           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
016480           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
016490                                                                          
016500* HÄR STYR DC-TILLHÖRIGHET VILKET ANALYSNR SOM SKALL ANVÄNDAS             
016510           IF DCS-CDC                                                     
016520             MOVE '158600010146'    TO R3-LINE-ORDER                      
016530           ELSE                                                           
016540             MOVE '158600010145'    TO R3-LINE-ORDER                      
016550           END-IF                                                         
016560           MOVE SYST-IDKST          TO WS-RED-IDKST                       
016570           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
016580           PERFORM S04-WRITE-W51073A                                      
016590         END-IF                                                           
016600       END-IF                                                             
016610                                                                          
016620       IF SYST-IDSEKVNR = 2                                               
016630         IF IN-EKH-SUBEL > 0                                              
016640           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
016650           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
016660           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
016670                                                                          
016680* HÄR STYR DC-TILLHÖRIGHET VILKET ANALYSNR SOM SKALL ANVÄNDAS             
016690           IF DCS-CDC                                                     
016700             MOVE '158600010146'    TO R3-LINE-ORDER                      
016710           ELSE                                                           
016720             MOVE '158600010145'    TO R3-LINE-ORDER                      
016730           END-IF                                                         
016740           MOVE SYST-IDKST          TO WS-RED-IDKST                       
016750           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
016760           PERFORM S04-WRITE-W51073A                                      
016770         END-IF                                                           
016780       END-IF                                                             
016790     END-EVALUATE                                                         
016800     .                                                                    
016810     EJECT                                                                
016820                                                                          
016830 CEBE-SUB-EVENT-102-109 SECTION.                                          
016840     EVALUATE IN-EKH-KDEKNIVA                                             
016850     WHEN 'DET'                                                           
016860       IF IN-EKH-KVANTAL > 0                                              
016870         IF SYST-IDSEKVNR = 1                                             
016880           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
016890           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
016900           IF KDPRODSL-LYNK                                               
016910             MOVE '1'               TO WS-ACCOUNT-5                       
016920             MOVE '0'               TO WS-ACCOUNT-5A                      
016930             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
016940             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
016950             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
016960             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
016970             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
016980           END-IF                                                         
016990           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
017000           COMPUTE R3-LINE-AMOUNT-LC =                                    
017010                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
017020           END-COMPUTE                                                    
017030           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
017040           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
017050           MOVE SPACE               TO WS-ALLOCATE-REF                    
017060           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
017070           PERFORM S02-WRITE-W51071A                                      
017080         END-IF                                                           
017090                                                                          
017100         IF SYST-IDSEKVNR = 2                                             
017110           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
017120           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
017130           COMPUTE R3-LINE-AMOUNT-LC =                                    
017140                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
017150           END-COMPUTE                                                    
017160           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
017170           PERFORM S02-WRITE-W51071A                                      
017180         END-IF                                                           
017190       END-IF                                                             
017200                                                                          
017210       IF IN-EKH-KVANTAL < 0                                              
017220         IF SYST-IDSEKVNR = 3                                             
017230           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
017240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
017250           COMPUTE R3-LINE-AMOUNT-LC =                                    
017260                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
017270           END-COMPUTE                                                    
017280           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
017290           PERFORM S02-WRITE-W51071A                                      
017300         END-IF                                                           
017310                                                                          
017320         IF SYST-IDSEKVNR = 4                                             
017330           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
017340           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
017350           IF KDPRODSL-LYNK                                               
017360             MOVE '1'               TO WS-ACCOUNT-5                       
017370             MOVE '0'               TO WS-ACCOUNT-5A                      
017380             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
017390             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
017400             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
017410             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
017420             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
017430           END-IF                                                         
017440           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
017450           COMPUTE R3-LINE-AMOUNT-LC =                                    
017460                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
017470           END-COMPUTE                                                    
017480           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
017490           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
017500           MOVE SPACE               TO WS-ALLOCATE-REF                    
017510           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
017520           PERFORM S02-WRITE-W51071A                                      
017530         END-IF                                                           
017540       END-IF                                                             
017550     END-EVALUATE                                                         
017560     .                                                                    
017570     EJECT                                                                
017580                                                                          
017590 CEBE-SUB-EVENT-102-110 SECTION.                                          
017600     EVALUATE IN-EKH-KDEKNIVA                                             
017610     WHEN 'DET'                                                           
017620       IF IN-EKH-KVANTAL > 0                                              
017630         IF SYST-IDSEKVNR = 1                                             
017640           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
017650           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
017660           COMPUTE R3-LINE-AMOUNT-LC =                                    
017670                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
017680           END-COMPUTE                                                    
017690           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
017700           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
017710           MOVE SPACE               TO WS-ALLOCATE-REF                    
017720           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
017730           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
017740           PERFORM S02-WRITE-W51071A                                      
017750         END-IF                                                           
017760                                                                          
017770         IF SYST-IDSEKVNR = 2                                             
017780           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
017790           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
017800           COMPUTE R3-LINE-AMOUNT-LC =                                    
017810                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
017820           END-COMPUTE                                                    
017830           PERFORM S02-WRITE-W51071A                                      
017840         END-IF                                                           
017850       END-IF                                                             
017860     END-EVALUATE                                                         
017870     .                                                                    
017880     EJECT                                                                
017890                                                                          
017900 CEBF-SUB-EVENT-102-120 SECTION.                                          
017910     EVALUATE IN-EKH-KDEKNIVA                                             
017920     WHEN 'DET'                                                           
017930       IF SYST-IDSEKVNR = 1                                               
017940         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
017950         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
017960         COMPUTE R3-LINE-AMOUNT    ROUNDED =                              
017970              IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                       
017980         PERFORM S03-WRITE-W51072                                         
017990       END-IF                                                             
018000                                                                          
018010     WHEN 'AVDR'                                                          
018020     WHEN 'FÖRS'                                                          
018030     WHEN 'LEG'                                                           
018040     WHEN 'FRAKT'                                                         
018050       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
018060       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
018070       MOVE SYST-IDKST          TO WS-RED-IDKST                           
018080       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
018090       COMPUTE R3-LINE-AMOUNT    ROUNDED =                                
018100               IN-EKH-SUBEL * -1                                          
018110       PERFORM S04-WRITE-W51073A                                          
018120                                                                          
018130     WHEN 'EMB'                                                           
018140       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
018150       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
018160       MOVE SYST-IDKST          TO WS-RED-IDKST                           
018170       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
018180       COMPUTE R3-LINE-AMOUNT    ROUNDED =                                
018190               IN-EKH-SUBEL * -1                                          
018200       PERFORM S04-WRITE-W51073A                                          
018210                                                                          
018220     END-EVALUATE                                                         
018230     .                                                                    
018240     EJECT                                                                
018250                                                                          
018260 CEBG-SUB-EVENT-102-121 SECTION.                                          
018270     EVALUATE IN-EKH-KDEKNIVA                                             
018280     WHEN 'DET'                                                           
018290       IF IN-EKH-IDDISTR = 9111                                           
018300       OR IN-EKH-IDDISTR = 9161                                           
018310       OR IN-EKH-IDDISTR = 9162                                           
018320         MOVE WS-PRKURS-CN3 TO WS-PRKURS-ALL3                             
018330         MOVE 'CN05'        TO BET-KDTRADP                                
018340       END-IF                                                             
018350       IF IN-EKH-IDDISTR = 9211                                           
018360       OR IN-EKH-IDDISTR = 9261                                           
018370       OR IN-EKH-IDDISTR = 9262                                           
018380         MOVE WS-PRKURS-US3 TO WS-PRKURS-ALL3                             
018390         MOVE 'US01'        TO BET-KDTRADP                                
018400       END-IF                                                             
018410       IF (IN-EKH-IDDISTR = 8661                                          
018420       OR IN-EKH-IDDISTR = 8662)                                          
018430       AND IN-EKH-IDKUNDNR = 66                                           
018440         MOVE WS-PRKURS-MY3 TO WS-PRKURS-ALL3                             
018450         MOVE 'MY04'        TO BET-KDTRADP                                
018460       END-IF                                                             
018470       IF SYST-IDSEKVNR = 1                                               
018480         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
018490         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
018500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
018510              IN-EKH-KVANTAL * IN-EKH-PRARTNTO * WS-PRKURS-ALL3           
018520         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
018530         PERFORM S03-WRITE-W51072                                         
018540       END-IF                                                             
018550                                                                          
018560       IF SYST-IDSEKVNR = 2                                               
018570         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
018580         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
018590         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
018600            IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                         
018610         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
018620         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
018630         MOVE SPACE               TO WS-ALLOCATE-REF                      
018640         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
018650         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
018660         PERFORM S03-WRITE-W51072                                         
018670       END-IF                                                             
018680                                                                          
018690       COMPUTE WS-BELOPP         ROUNDED =                                
018700           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO * WS-PRKURS-ALL3) -          
018710           (IN-EKH-KVANTAL * IN-EKH-PRARTSTD)                             
018720       IF WS-BELOPP      > 0                                              
018730         IF SYST-IDSEKVNR = 3                                             
018740           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
018750           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
018760           COMPUTE R3-LINE-AMOUNT-LC  ROUNDED = WS-BELOPP                 
018770           PERFORM S11-ANALYSIS                                           
018780           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
018790           PERFORM S04-WRITE-W51073A                                      
018800         END-IF                                                           
018810       ELSE                                                               
018820         IF WS-BELOPP = 0                                                 
018830           CONTINUE                                                       
018840         ELSE                                                             
018850           IF SYST-IDSEKVNR = 4                                           
018860             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
018870             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
018880             COMPUTE R3-LINE-AMOUNT-LC ROUNDED = WS-BELOPP                
018890             PERFORM S11-ANALYSIS                                         
018900             MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER          
018910             PERFORM S04-WRITE-W51073A                                    
018920           END-IF                                                         
018930         END-IF                                                           
018940       END-IF                                                             
018950                                                                          
018960                                                                          
018970     WHEN 'AVDR'                                                          
018980     WHEN 'FÖRS'                                                          
018990     WHEN 'LEG'                                                           
019000     WHEN 'FRAKT'                                                         
019010       IF SYST-IDSEKVNR = 1                                               
019020         IF IN-EKH-IDDISTR = 9111                                         
019030         OR IN-EKH-IDDISTR = 9161                                         
019040         OR IN-EKH-IDDISTR = 9162                                         
019050           MOVE 'CN05'      TO BET-KDTRADP                                
019060         END-IF                                                           
019070         IF IN-EKH-IDDISTR = 9211                                         
019080         OR IN-EKH-IDDISTR = 9261                                         
019090         OR IN-EKH-IDDISTR = 9262                                         
019100           MOVE 'US01'      TO BET-KDTRADP                                
019110         END-IF                                                           
019120         IF (IN-EKH-IDDISTR = 8661                                        
019130         OR IN-EKH-IDDISTR = 8662)                                        
019140         AND IN-EKH-IDKUNDNR = 66                                         
019150           MOVE 'MYR'     TO WS-KDVALISO                                  
019160         END-IF                                                           
019170         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
019180         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
019190         MOVE SYST-IDKST          TO WS-RED-IDKST                         
019200         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
019210         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
019220                 IN-EKH-SUBEL                                             
019230         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
019240         PERFORM S04-WRITE-W51073A                                        
019250       END-IF                                                             
019260                                                                          
019270       IF SYST-IDSEKVNR = 2                                               
019280         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
019290         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
019300         MOVE SYST-IDKST          TO WS-RED-IDKST                         
019310         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
019320         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
019330         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
019340         MOVE SPACE               TO WS-ALLOCATE-REF                      
019350         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
019360         COMPUTE R3-LINE-AMOUNT-LC  ROUNDED =                             
019370                 IN-EKH-SUBEL * -1                                        
019380         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
019390         PERFORM S04-WRITE-W51073A                                        
019400       END-IF                                                             
019410                                                                          
019420                                                                          
019430     WHEN 'EMB'                                                           
019440       IF SYST-IDSEKVNR = 1                                               
019450         IF IN-EKH-IDDISTR = 9111                                         
019460         OR IN-EKH-IDDISTR = 9161                                         
019470         OR IN-EKH-IDDISTR = 9162                                         
019480           MOVE 'CN05'      TO BET-KDTRADP                                
019490         END-IF                                                           
019500         IF IN-EKH-IDDISTR = 9211                                         
019510         OR IN-EKH-IDDISTR = 9261                                         
019520         OR IN-EKH-IDDISTR = 9262                                         
019530           MOVE 'US01'      TO BET-KDTRADP                                
019540         END-IF                                                           
019550         IF (IN-EKH-IDDISTR = 8661                                        
019560         OR IN-EKH-IDDISTR = 8662)                                        
019570         AND IN-EKH-IDKUNDNR = 66                                         
019580           MOVE 'MY04'      TO BET-KDTRADP                                
019590         END-IF                                                           
019600         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
019610         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
019620         MOVE SYST-IDKST          TO WS-RED-IDKST                         
019630         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
019640         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
019650                 IN-EKH-SUBEL                                             
019660         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
019670         PERFORM S04-WRITE-W51073A                                        
019680       END-IF                                                             
019690                                                                          
019700       IF SYST-IDSEKVNR = 2                                               
019710         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
019720         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
019730         MOVE SYST-IDKST          TO WS-RED-IDKST                         
019740         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
019750         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
019760         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
019770         MOVE SPACE               TO WS-ALLOCATE-REF                      
019780         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
019790         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
019800                 IN-EKH-SUBEL * -1                                        
019810         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
019820         PERFORM S04-WRITE-W51073A                                        
019830       END-IF                                                             
019840                                                                          
019850     END-EVALUATE                                                         
019860     .                                                                    
019870     EJECT                                                                
019880                                                                          
019890 CEBH-SUB-EVENT-102-122 SECTION.                                          
019900     EVALUATE IN-EKH-KDEKNIVA                                             
019910     WHEN 'DET'                                                           
019920       IF IN-EKH-IDDISTR = 9111                                           
019930       OR IN-EKH-IDDISTR = 9161                                           
019940       OR IN-EKH-IDDISTR = 9162                                           
019950         MOVE 'CN05'      TO BET-KDTRADP                                  
019960       END-IF                                                             
019970       IF IN-EKH-IDDISTR = 9211                                           
019980       OR IN-EKH-IDDISTR = 9261                                           
019990       OR IN-EKH-IDDISTR = 9262                                           
020000         MOVE 'US01'      TO BET-KDTRADP                                  
020010       END-IF                                                             
020020       IF (IN-EKH-IDDISTR = 8661                                          
020030       OR IN-EKH-IDDISTR = 8662)                                          
020040       AND IN-EKH-IDKUNDNR = 66                                           
020050         MOVE 'MY04'      TO BET-KDTRADP                                  
020060       END-IF                                                             
020070       IF SYST-IDSEKVNR = 1                                               
020080         IF IN-EKH-KVANTAL > 0                                            
020090           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
020100           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
020110           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
020120           IF IN-EKH-IDDISTR = 70                                         
020130             MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                   
020140             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020150               IN-EKH-KVANTAL *  IN-EKH-PRARTNTO                          
020160           ELSE                                                           
020170             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020180               IN-EKH-KVANTAL *  IN-EKH-PRARTSTD                          
020190           END-IF                                                         
020200           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
020210           MOVE SPACE               TO WS-ALLOCATE-REF                    
020220           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
020230           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
020240           PERFORM S02-WRITE-W51071A                                      
020250         END-IF                                                           
020260       END-IF                                                             
020270                                                                          
020280       IF SYST-IDSEKVNR = 2                                               
020290         IF IN-EKH-KVANTAL < 0                                            
020300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
020310           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
020320           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
020330           IF IN-EKH-IDDISTR = 70                                         
020340             MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                   
020350             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020360               IN-EKH-KVANTAL *  IN-EKH-PRARTNTO                          
020370           ELSE                                                           
020380             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020390               IN-EKH-KVANTAL *  IN-EKH-PRARTSTD                          
020400           END-IF                                                         
020410           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
020420           MOVE SPACE               TO WS-ALLOCATE-REF                    
020430           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
020440           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
020450           PERFORM S02-WRITE-W51071A                                      
020460         END-IF                                                           
020470       END-IF                                                             
020480                                                                          
020490       IF SYST-IDSEKVNR = 3                                               
020500         IF IN-EKH-KVANTAL > 0                                            
020510           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
020520           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
020530           IF IN-EKH-IDDISTR = 70                                         
020540             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020550               IN-EKH-KVANTAL *  IN-EKH-PRARTNTO                          
020560           ELSE                                                           
020570             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020580               IN-EKH-KVANTAL *  IN-EKH-PRARTSTD                          
020590           END-IF                                                         
020600           MOVE SYST-IDKST          TO WS-RED-IDKST                       
020610           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
020620           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
020630           PERFORM S02-WRITE-W51071A                                      
020640         END-IF                                                           
020650       END-IF                                                             
020660                                                                          
020670       IF SYST-IDSEKVNR = 4                                               
020680         IF IN-EKH-KVANTAL < 0                                            
020690           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
020700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
020710           IF IN-EKH-IDDISTR = 70                                         
020720             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020730               IN-EKH-KVANTAL *  IN-EKH-PRARTNTO                          
020740           ELSE                                                           
020750             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
020760               IN-EKH-KVANTAL *  IN-EKH-PRARTSTD                          
020770           END-IF                                                         
020780           MOVE SYST-IDKST          TO WS-RED-IDKST                       
020790           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
020800           MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER            
020810           PERFORM S02-WRITE-W51071A                                      
020820         END-IF                                                           
020830       END-IF                                                             
020840     END-EVALUATE                                                         
020850     .                                                                    
020860     EJECT                                                                
020870                                                                          
020880 CEBI-SUB-EVENT-102-135 SECTION.                                          
020890     MOVE IN-EKH-IDDC-SEND      TO WS-IDDC-SAVE                           
020900                                   W-IDDC-B6                              
020910     PERFORM S81-GET-CURR-RATE                                            
020920                                                                          
020930     EVALUATE IN-EKH-KDEKNIVA                                             
020940     WHEN 'DET'                                                           
020950       IF SYST-IDSEKVNR = 1                                               
020960         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
020970         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
020980         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
020990              IN-EKH-KVANTAL * IN-EKH-PRARTNTO                            
021000         COMPUTE WS-SUMMA-135 = WS-SUMMA-135 +                            
021010                                R3-LINE-AMOUNT-LC                         
021020         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
021030         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
021040         MOVE SPACE               TO WS-ALLOCATE-DC                       
021050         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
021060         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
021070         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
021080         PERFORM S03-WRITE-W51072                                         
021090       END-IF                                                             
021100                                                                          
021110       IF IN-EKH-KDVALISO = WS-KDVALISO-WDB6                              
021120         COMPUTE WS-BELOPP-ALL   ROUNDED =                                
021130           (IN-EKH-KVANTAL * IN-EKH-PRARTSTD * WS-PRKURS-ALL) -           
021140           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO)                             
021150         MOVE WS-BELOPP-ALL TO WS-BELOPP-135                              
021160       END-IF                                                             
021170                                                                          
021180       IF WS-BELOPP-135 > 0                                               
021190         IF SYST-IDSEKVNR = 2                                             
021200           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
021210           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
021220           COMPUTE R3-LINE-AMOUNT-LC ROUNDED = WS-BELOPP-135              
021230           COMPUTE WS-SUMMA-135 = WS-SUMMA-135 +                          
021240                                  R3-LINE-AMOUNT-LC                       
021250           MOVE SYST-IDANALYS   TO R3-LINE-ORDER                          
021260           PERFORM S04-WRITE-W51073A                                      
021270         END-IF                                                           
021280       END-IF                                                             
021290                                                                          
021300       IF WS-BELOPP-135 < 0                                               
021310         IF SYST-IDSEKVNR = 3                                             
021320           MOVE SYST-IDKONTO  TO WS-R3-ACCOUNT-10                         
021330           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
021340           COMPUTE R3-LINE-AMOUNT-LC ROUNDED = WS-BELOPP-135              
021350           COMPUTE WS-SUMMA-135 = WS-SUMMA-135 -                          
021360                                  R3-LINE-AMOUNT-LC                       
021370           MOVE SYST-IDANALYS TO R3-LINE-ORDER                            
021380           PERFORM S04-WRITE-W51073A                                      
021390         END-IF                                                           
021400       END-IF                                                             
021410                                                                          
021420     WHEN 'EMB'                                                           
021430     WHEN 'FÖRS'                                                          
021440     WHEN 'FRAKT'                                                         
021450       IF SYST-IDSEKVNR = 1                                               
021460         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
021470         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
021480         MOVE SPACE               TO R3-LINE-ORDER                        
021490         MOVE SYST-IDPRCTR      TO R3-LINE-PROFIT-CENTER                  
021500         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
021510                 IN-EKH-SUBEL * WS-PRKURS-ALL                             
021520         COMPUTE WS-SUMMA-135 = WS-SUMMA-135 +                            
021530                                R3-LINE-AMOUNT-LC                         
021540         MOVE SYST-IDKST          TO WS-RED-IDKST                         
021550         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
021560         MOVE SPACE               TO WS-ALLOCATE-DC                       
021570         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
021580         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
021590         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
021600         PERFORM S03-WRITE-W51072                                         
021610       END-IF                                                             
021620                                                                          
021630     WHEN 'DDI'                                                           
021640       IF WS-SUMMA-135 < ZERO                                             
021650         IF SYST-IDSEKVNR = 1                                             
021660           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
021670           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
021680           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
021690                   WS-SUMMA-135 * -1                                      
021700           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
021710           PERFORM S03-WRITE-W51072                                       
021720           MOVE ZERO                TO WS-SUMMA-135                       
021730                                       WS-BELOPP-ALL                      
021740                                       WS-BELOPP-135                      
021750         END-IF                                                           
021760       END-IF                                                             
021770       IF WS-SUMMA-135 > ZERO                                             
021780         IF SYST-IDSEKVNR = 2                                             
021790           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
021800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
021810           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
021820                   WS-SUMMA-135                                           
021830           MOVE IN-EKH-IDKST      TO R3-LINE-COST-CENTER                  
021840           PERFORM S03-WRITE-W51072                                       
021850           MOVE ZERO                TO WS-SUMMA-135                       
021860                                       WS-BELOPP-ALL                      
021870                                       WS-BELOPP-135                      
021880         END-IF                                                           
021890       END-IF                                                             
021900     END-EVALUATE                                                         
021910     .                                                                    
021920     EJECT                                                                
021930                                                                          
021940 CEBI-SUB-EVENT-102-145 SECTION.                                          
021950     EVALUATE IN-EKH-KDEKNIVA                                             
021960     WHEN 'DET'                                                           
021970       IF SYST-IDSEKVNR = 1                                               
021980         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
021990         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
022000         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
022010              IN-EKH-KVANTAL * IN-EKH-PRARTNTO                            
022020         COMPUTE WS-SUMMA-145 = WS-SUMMA-145 +                            
022030                                R3-LINE-AMOUNT-LC                         
022040         MOVE IN-EKH-IDDC-SEND    TO W-IDDC-B6                            
022050         PERFORM IMS-GU-WDB601                                            
022060         MOVE DCS-KDTRADP         TO R3-LINE-TRADING-PARTNER              
022070         MOVE DCS-IDPARTNR        TO R3-LINE-PA-CUSTOMER                  
022080         MOVE SPACE               TO WS-ALLOCATE-DC                       
022090         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
022100         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
022110         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
022120         PERFORM S03-WRITE-W51072                                         
022130       END-IF                                                             
022140                                                                          
022150       MOVE IN-EKH-IDDC-SEND    TO WS-IDDC-SAVE                           
022160                                   W-IDDC-B6                              
022170       PERFORM S81-GET-CURR-RATE                                          
022180                                                                          
022190       IF IN-EKH-KDVALISO = WS-KDVALISO-WDB6                              
022200         COMPUTE WS-BELOPP-ALL1   ROUNDED =                               
022210           (IN-EKH-KVANTAL * IN-EKH-PRARTSTD * WS-PRKURS-ALL) -           
022220           (IN-EKH-KVANTAL * IN-EKH-PRARTNTO)                             
022230         MOVE WS-BELOPP-ALL1 TO WS-BELOPP-145                             
022240       END-IF                                                             
022250                                                                          
022260       IF WS-BELOPP-145 > 0                                               
022270         IF SYST-IDSEKVNR = 2                                             
022280           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
022290           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
022300           COMPUTE R3-LINE-AMOUNT-LC ROUNDED = WS-BELOPP-145              
022310           COMPUTE WS-SUMMA-145 = WS-SUMMA-145 +                          
022320                                  R3-LINE-AMOUNT-LC                       
022330           MOVE SYST-IDANALYS   TO R3-LINE-ORDER                          
022340           PERFORM S03-WRITE-W51072                                       
022350         END-IF                                                           
022360       END-IF                                                             
022370                                                                          
022380       IF WS-BELOPP-145 < 0                                               
022390         IF SYST-IDSEKVNR = 3                                             
022400           MOVE SYST-IDKONTO  TO WS-R3-ACCOUNT-10                         
022410           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
022420           COMPUTE R3-LINE-AMOUNT-LC ROUNDED = WS-BELOPP-145              
022430           COMPUTE WS-SUMMA-145 = WS-SUMMA-145 -                          
022440                                  R3-LINE-AMOUNT-LC                       
022450           MOVE SYST-IDANALYS TO R3-LINE-ORDER                            
022460           PERFORM S03-WRITE-W51072                                       
022470         END-IF                                                           
022480       END-IF                                                             
022490                                                                          
022500     WHEN 'EMB'                                                           
022510     WHEN 'LEG'                                                           
022520     WHEN 'FÖRS'                                                          
022530     WHEN 'FRAKT'                                                         
022540       IF SYST-IDSEKVNR = 1                                               
022550         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
022560         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
022570         MOVE SPACE               TO R3-LINE-ORDER                        
022580           MOVE SYST-IDPRCTR      TO R3-LINE-PROFIT-CENTER                
022590           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
022600                 IN-EKH-SUBEL * WS-PRKURS-ALL                             
022610         END-IF                                                           
022620                                                                          
022630         COMPUTE WS-SUMMA-145 = WS-SUMMA-145 +                            
022640                                R3-LINE-AMOUNT-LC                         
022650         MOVE SYST-IDKST          TO WS-RED-IDKST                         
022660         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
022670         MOVE SPACE               TO WS-ALLOCATE-DC                       
022680         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
022690         MOVE IN-EKH-IDVERGL      TO WS-ALLOCATE-REF                      
022700         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
022710         PERFORM S03-WRITE-W51072                                         
022720     WHEN 'DDI'                                                           
022730       IF WS-SUMMA-145 < ZERO                                             
022740         IF SYST-IDSEKVNR = 1                                             
022750           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
022760           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
022770           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
022780                   WS-SUMMA-145                                           
022790           MOVE IN-EKH-IDKST        TO R3-LINE-COST-CENTER                
022800           PERFORM S03-WRITE-W51072                                       
022810           MOVE ZERO                TO WS-SUMMA-145                       
022820                                       WS-BELOPP-ALL1                     
022830                                       WS-BELOPP-145                      
022840         END-IF                                                           
022850       END-IF                                                             
022860       IF WS-SUMMA-145 > ZERO                                             
022870         IF SYST-IDSEKVNR = 2                                             
022880           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
022890           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
022900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
022910                   WS-SUMMA-145                                           
022920           MOVE IN-EKH-IDKST      TO R3-LINE-COST-CENTER                  
022930           PERFORM S03-WRITE-W51072                                       
022940           MOVE ZERO                TO WS-SUMMA-145                       
022950                                       WS-BELOPP-ALL1                     
022960                                       WS-BELOPP-145                      
022970         END-IF                                                           
022980       END-IF                                                             
022990     END-EVALUATE                                                         
023000     .                                                                    
023010     EJECT                                                                
023020                                                                          
023030 CEC-MAIN-EVENT-103 SECTION.                                              
023040     EVALUATE IN-EKH-KDEKSHT                                              
023050     WHEN '101'                                                           
023060          PERFORM CECA-SUB-EVENT-103-101                                  
023070     WHEN '102'                                                           
023080          PERFORM CECB-SUB-EVENT-103-102                                  
023090     END-EVALUATE                                                         
023100     .                                                                    
023110     EJECT                                                                
023120                                                                          
023130 CECA-SUB-EVENT-103-101 SECTION.                                          
023140                                                                          
023150     EVALUATE IN-EKH-KDEKNIVA                                             
023160     WHEN 'DET'                                                           
023170       IF SYST-IDSEKVNR = 1                                               
023180                                                                          
023190* HÄR GENERERAS TRE R3-POSTER MED OLIKA ANALYSNR                          
023200         IF IN-EKH-KVANTAL > 0                                            
023210           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
023220           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
023230           COMPUTE R3-LINE-AMOUNT-LC =                                    
023240                   IN-EKH-KVANTAL * IN-EKH-PRDIRLON                       
023250           END-COMPUTE                                                    
023260           MOVE R3-LINE-AMOUNT-LC   TO SPAR-PRDIRLON                      
023270* POST NR 1                                                               
023280           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
023290           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
023300             PERFORM S02-WRITE-W51071A                                    
023310           END-IF                                                         
023320           COMPUTE R3-LINE-AMOUNT-LC =                                    
023330                   IN-EKH-KVANTAL * IN-EKH-PRDMTRL                        
023340           END-COMPUTE                                                    
023350           MOVE R3-LINE-AMOUNT-LC   TO SPAR-PRDMTRL                       
023360* POST NR 2                                                               
023370           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
023380           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
023390             PERFORM S02-WRITE-W51071A                                    
023400           END-IF                                                         
023410           COMPUTE R3-LINE-AMOUNT-LC =                                    
023420                   IN-EKH-KVANTAL * IN-EKH-PROVRPAL                       
023430           END-COMPUTE                                                    
023440           MOVE R3-LINE-AMOUNT-LC   TO SPAR-PROVRPAL                      
023450* POST NR 3                                                               
023460           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
023470           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
023480             PERFORM S02-WRITE-W51071A                                    
023490           END-IF                                                         
023500         END-IF                                                           
023510       END-IF                                                             
023520                                                                          
023530       IF SYST-IDSEKVNR = 2                                               
023540         IF IN-EKH-KVANTAL > 0                                            
023550           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
023560           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
023570           IF KDPRODSL-LYNK                                               
023580             MOVE '1'               TO WS-ACCOUNT-5                       
023590             MOVE '0'               TO WS-ACCOUNT-5A                      
023600             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
023610             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
023620             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
023630             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
023640             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
023650           END-IF                                                         
023660           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
023670           COMPUTE R3-LINE-AMOUNT-LC =                                    
023680                   IN-EKH-KVANTAL * (IN-EKH-PRDIRLON +                    
023690                                     IN-EKH-PRDMTRL  +                    
023700                                     IN-EKH-PROVRPAL)                     
023710           END-COMPUTE                                                    
023720           MOVE R3-LINE-AMOUNT-LC   TO SPAR-SUMMA                         
023730           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
023740           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
023750           MOVE SPACE               TO WS-ALLOCATE-REF                    
023760           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
023770                                                                          
023780           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
023790             PERFORM S02-WRITE-W51071A                                    
023800           END-IF                                                         
023810         END-IF                                                           
023820       END-IF                                                             
023830                                                                          
023840       IF SYST-IDSEKVNR = 3                                               
023850                                                                          
023860* HÄR GENERERAS TRE R3-POSTER MED OLIKA ANALYSNR                          
023870         IF IN-EKH-KVANTAL < 0                                            
023880           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
023890           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
023900           COMPUTE R3-LINE-AMOUNT-LC =                                    
023910                   IN-EKH-KVANTAL * IN-EKH-PRDIRLON                       
023920           END-COMPUTE                                                    
023930           MOVE R3-LINE-AMOUNT-LC   TO SPAR-PRDIRLON                      
023940* POST NR 1                                                               
023950           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
023960           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
023970             PERFORM S02-WRITE-W51071A                                    
023980           END-IF                                                         
023990           COMPUTE R3-LINE-AMOUNT-LC =                                    
024000                   IN-EKH-KVANTAL * IN-EKH-PRDMTRL                        
024010           END-COMPUTE                                                    
024020           MOVE R3-LINE-AMOUNT-LC   TO SPAR-PRDMTRL                       
024030* POST NR 2                                                               
024040           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
024050           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
024060             PERFORM S02-WRITE-W51071A                                    
024070           END-IF                                                         
024080           COMPUTE R3-LINE-AMOUNT-LC =                                    
024090                   IN-EKH-KVANTAL * IN-EKH-PROVRPAL                       
024100           END-COMPUTE                                                    
024110           MOVE R3-LINE-AMOUNT-LC   TO SPAR-PROVRPAL                      
024120* POST NR 3                                                               
024130           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
024140           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
024150             PERFORM S02-WRITE-W51071A                                    
024160           END-IF                                                         
024170         END-IF                                                           
024180       END-IF                                                             
024190                                                                          
024200       IF SYST-IDSEKVNR = 4                                               
024210         IF IN-EKH-KVANTAL < 0                                            
024220           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
024230           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
024240           IF KDPRODSL-LYNK                                               
024250             MOVE '1'               TO WS-ACCOUNT-5                       
024260             MOVE '0'               TO WS-ACCOUNT-5A                      
024270             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
024280             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
024290             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
024300             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
024310             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
024320           END-IF                                                         
024330           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
024340           COMPUTE R3-LINE-AMOUNT-LC =                                    
024350                   IN-EKH-KVANTAL * (IN-EKH-PRDIRLON +                    
024360                                     IN-EKH-PRDMTRL  +                    
024370                                     IN-EKH-PROVRPAL)                     
024380           END-COMPUTE                                                    
024390           MOVE R3-LINE-AMOUNT-LC   TO SPAR-SUMMA                         
024400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
024410           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
024420           MOVE SPACE               TO WS-ALLOCATE-REF                    
024430           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
024440                                                                          
024450           IF R3-LINE-AMOUNT-LC NOT = ZERO                                
024460             PERFORM S02-WRITE-W51071A                                    
024470           END-IF                                                         
024480         END-IF                                                           
024490       END-IF                                                             
024500                                                                          
024510       IF SYST-IDSEKVNR = 5                                               
024520         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
024530         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
024540         COMPUTE WS-LINE-AMOUNT-LC =                                      
024550                                     IN-EKH-KVANTAL *                     
024560                                    (IN-EKH-PRARTSTD -                    
024570                                     IN-EKH-PRINK    -                    
024580                                     IN-EKH-PRDIRLON -                    
024590                                     IN-EKH-PRDMTRL  -                    
024600                                     IN-EKH-PROVRPAL)                     
024610         END-COMPUTE                                                      
024620         IF WS-LINE-AMOUNT-LC < ZERO                                      
024630           MOVE '40'              TO R3-LINE-POSTING-KEY                  
024640           MOVE '+'               TO R3-LINE-AMOUNT-SIGN                  
024650           COMPUTE R3-LINE-AMOUNT-LC =                                    
024660                   WS-LINE-AMOUNT-LC * -1                                 
024670           END-COMPUTE                                                    
024680         ELSE                                                             
024690           MOVE '50'              TO R3-LINE-POSTING-KEY                  
024700           MOVE '-'               TO R3-LINE-AMOUNT-SIGN                  
024710           COMPUTE R3-LINE-AMOUNT-LC =                                    
024720                   WS-LINE-AMOUNT-LC *  1                                 
024730           END-COMPUTE                                                    
024740         END-IF                                                           
024750         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
024760         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
024770           PERFORM S02-WRITE-W51071A                                      
024780         END-IF                                                           
024790         COMPUTE WS-LINE-AMOUNT-LC = SPAR-SUMMA    -                      
024800                                     SPAR-PRDIRLON -                      
024810                                     SPAR-PRDMTRL  -                      
024820                                     SPAR-PROVRPAL                        
024830         END-COMPUTE                                                      
024840         IF WS-LINE-AMOUNT-LC NOT = ZERO                                  
024850           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
024860           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
024870           MOVE SYST-IDANALYS     TO R3-LINE-ORDER                        
024880           IF WS-LINE-AMOUNT-LC < ZERO                                    
024890             MOVE '40'            TO R3-LINE-POSTING-KEY                  
024900             MOVE '+'             TO R3-LINE-AMOUNT-SIGN                  
024910             COMPUTE R3-LINE-AMOUNT-LC =                                  
024920                     WS-LINE-AMOUNT-LC * -1                               
024930             END-COMPUTE                                                  
024940           ELSE                                                           
024950             MOVE '50'            TO R3-LINE-POSTING-KEY                  
024960             MOVE '-'             TO R3-LINE-AMOUNT-SIGN                  
024970             COMPUTE R3-LINE-AMOUNT-LC =                                  
024980                     WS-LINE-AMOUNT-LC *  1                               
024990             END-COMPUTE                                                  
025000           END-IF                                                         
025010           PERFORM S02-WRITE-W51071A                                      
025020         END-IF                                                           
025030       END-IF                                                             
025040                                                                          
025050       IF SYST-IDSEKVNR = 6                                               
025060         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
025070         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
025080         IF KDPRODSL-LYNK                                                 
025090           MOVE '1'               TO WS-ACCOUNT-5                         
025100           MOVE '0'               TO WS-ACCOUNT-5A                        
025110           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
025120           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
025130           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
025140           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
025150           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
025160         END-IF                                                           
025170         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
025180         COMPUTE WS-LINE-AMOUNT-LC =                                      
025190                                     IN-EKH-KVANTAL *                     
025200                                    (IN-EKH-PRARTSTD -                    
025210                                     IN-EKH-PRINK    -                    
025220                                     IN-EKH-PRDIRLON -                    
025230                                     IN-EKH-PRDMTRL  -                    
025240                                     IN-EKH-PROVRPAL)                     
025250         END-COMPUTE                                                      
025260         IF WS-LINE-AMOUNT-LC < ZERO                                      
025270           MOVE '50'              TO R3-LINE-POSTING-KEY                  
025280           MOVE '-'               TO R3-LINE-AMOUNT-SIGN                  
025290           COMPUTE R3-LINE-AMOUNT-LC =                                    
025300                   WS-LINE-AMOUNT-LC * -1                                 
025310           END-COMPUTE                                                    
025320         ELSE                                                             
025330           MOVE '40'              TO R3-LINE-POSTING-KEY                  
025340           MOVE '+'               TO R3-LINE-AMOUNT-SIGN                  
025350           COMPUTE R3-LINE-AMOUNT-LC =                                    
025360                   WS-LINE-AMOUNT-LC *  1                                 
025370           END-COMPUTE                                                    
025380         END-IF                                                           
025390         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
025400         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
025410         MOVE SPACE               TO WS-ALLOCATE-REF                      
025420         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
025430                                                                          
025440         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
025450           PERFORM S02-WRITE-W51071A                                      
025460         END-IF                                                           
025470       END-IF                                                             
025480     END-EVALUATE                                                         
025490     .                                                                    
025500     EJECT                                                                
025510                                                                          
025520 CECB-SUB-EVENT-103-102 SECTION.                                          
025530                                                                          
025540     EVALUATE IN-EKH-KDEKNIVA                                             
025550     WHEN 'DET'                                                           
025560***  TRIPPEL FÖR NORMAL INLEVERANS                                        
025570       IF SYST-IDSEKVNR = 1                                               
025580         IF IN-EKH-KVANTAL > 0                                            
025590           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
025600           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
025610           IF KDPRODSL-LYNK                                               
025620             MOVE '1'               TO WS-ACCOUNT-5                       
025630             MOVE '0'               TO WS-ACCOUNT-5A                      
025640             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
025650             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
025660             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
025670             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
025680             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
025690           END-IF                                                         
025700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
025710           COMPUTE R3-LINE-AMOUNT-LC =                                    
025720                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
025730           END-COMPUTE                                                    
025740           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
025750           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
025760           MOVE SPACE               TO WS-ALLOCATE-REF                    
025770           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
025780           PERFORM S02-WRITE-W51071A                                      
025790         END-IF                                                           
025800       END-IF                                                             
025810                                                                          
025820       IF SYST-IDSEKVNR = 2                                               
025830         IF IN-EKH-PRHEMTAG > ZERO AND IN-EKH-KVANTAL > 0                 
025840           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
025850           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
025860           COMPUTE R3-LINE-AMOUNT-LC =                                    
025870                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
025880           END-COMPUTE                                                    
025890           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
025900           PERFORM S02-WRITE-W51071A                                      
025910         END-IF                                                           
025920       END-IF                                                             
025930                                                                          
025940       IF SYST-IDSEKVNR = 3                                               
025950         IF IN-EKH-KVANTAL > 0                                            
025960           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
025970           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
025980           COMPUTE R3-LINE-AMOUNT-LC =                                    
025990                  (IN-EKH-KVANTAL * IN-EKH-PRINK)    -                    
026000                  (IN-EKH-KVANTAL * IN-EKH-PRHEMTAG)                      
026010           END-COMPUTE                                                    
026020           PERFORM S02-WRITE-W51071A                                      
026030         END-IF                                                           
026040       END-IF                                                             
026050                                                                          
026060***  TRIPPEL FÖR AVVIKANDE (NEGATIV) INLEVERANS                           
026070       IF SYST-IDSEKVNR = 4                                               
026080         IF IN-EKH-KVANTAL < 0                                            
026090           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
026100           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
026110           IF KDPRODSL-LYNK                                               
026120             MOVE '1'               TO WS-ACCOUNT-5                       
026130             MOVE '0'               TO WS-ACCOUNT-5A                      
026140             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
026150             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
026160             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
026170             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
026180             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
026190           END-IF                                                         
026200           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
026210           COMPUTE R3-LINE-AMOUNT-LC =                                    
026220                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
026230           END-COMPUTE                                                    
026240           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
026250           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
026260           MOVE SPACE               TO WS-ALLOCATE-REF                    
026270           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
026280           PERFORM S02-WRITE-W51071A                                      
026290         END-IF                                                           
026300       END-IF                                                             
026310                                                                          
026320       IF SYST-IDSEKVNR = 5                                               
026330         IF IN-EKH-PRHEMTAG > ZERO AND IN-EKH-KVANTAL < 0                 
026340           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
026350           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
026360           COMPUTE R3-LINE-AMOUNT-LC =                                    
026370                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
026380           END-COMPUTE                                                    
026390           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
026400           PERFORM S02-WRITE-W51071A                                      
026410         END-IF                                                           
026420       END-IF                                                             
026430                                                                          
026440       IF SYST-IDSEKVNR = 6                                               
026450         IF IN-EKH-KVANTAL < 0                                            
026460           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
026470           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
026480           COMPUTE R3-LINE-AMOUNT-LC =                                    
026490                  (IN-EKH-KVANTAL * IN-EKH-PRINK)    -                    
026500                  (IN-EKH-KVANTAL * IN-EKH-PRHEMTAG)                      
026510           END-COMPUTE                                                    
026520           PERFORM S02-WRITE-W51071A                                      
026530         END-IF                                                           
026540       END-IF                                                             
026550     END-EVALUATE                                                         
026560     .                                                                    
026570     EJECT                                                                
026580                                                                          
026590 CED-MAIN-EVENT-201 SECTION.                                              
026600     EVALUATE IN-EKH-KDEKSHT                                              
026610     WHEN '201'                                                           
026620          PERFORM CEDA-SUB-EVENT-201-201                                  
026630     WHEN '202'                                                           
026640          PERFORM CEDB-SUB-EVENT-201-202                                  
026650     END-EVALUATE                                                         
026660     .                                                                    
026670     EJECT                                                                
026680                                                                          
026690 CEDA-SUB-EVENT-201-201 SECTION.                                          
026700     EVALUATE IN-EKH-KDEKNIVA                                             
026710     WHEN 'DET'                                                           
026720       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
026730       MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                             
026740       IF KDPRODSL-LYNK                                                   
026750         MOVE '1'               TO WS-ACCOUNT-5                           
026760         MOVE '0'               TO WS-ACCOUNT-5A                          
026770         MOVE 'LYNK'            TO WS-PRCTR(1:4)                          
026780         MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                   
026790         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                       
026800         MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                  
026810         MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                       
026820       END-IF                                                             
026830       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
026840       COMPUTE R3-LINE-AMOUNT-LC =                                        
026850               IN-EKH-KVANTAL * IN-EKH-PRARTSTD                           
026860       END-COMPUTE                                                        
026870       MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                         
026880       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
026890       MOVE SPACE               TO WS-ALLOCATE-REF                        
026900       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
026910       PERFORM S03-WRITE-W51072                                           
026920                                                                          
026930* HÄR GENERERAS EN R3-POST/DEBET DÅ SUMMAPOSTEN EJ GÅR ATT                
026940* ANVÄNDA PGA ATT FELAKTIGT SUMMABELOPP KOMMER FRÅN FAKTURERINGEN         
026950       MOVE '40'                TO R3-LINE-POSTING-KEY                    
026960       MOVE '+'                 TO R3-LINE-AMOUNT-SIGN                    
026970       MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                       
026980       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
026990                                   SPAR-LINE-ACCOUNT                      
027000       COMPUTE R3-LINE-AMOUNT-LC =                                        
027010               IN-EKH-KVANTAL * IN-EKH-PRARTSTD                           
027020       END-COMPUTE                                                        
027030       MOVE SPACE               TO R3-LINE-PROFIT-CENTER                  
027040       MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                          
027050                                   SPAR-LINE-ORDER                        
027060       MOVE IN-EKH-IDKST        TO WS-RED-IDKST                           
027070       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
027080                                   SPAR-LINE-COST-CENTER                  
027090       PERFORM S03-WRITE-W51072                                           
027100                                                                          
027110     WHEN 'EMB'                                                           
027120       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
027130       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
027140       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
027150       PERFORM S04-WRITE-W51073A                                          
027160                                                                          
027170* HÄR GENERERAS EN R3-POST/DEBET DÅ SUMMAPOSTEN EJ GÅR ATT                
027180* ANVÄNDA PGA ATT FELAKTIGT SUMMABELOPP KOMMER FRÅN FAKTURERINGEN         
027190       MOVE '40'                  TO R3-LINE-POSTING-KEY                  
027200       MOVE '+'                   TO R3-LINE-AMOUNT-SIGN                  
027210       MOVE SPAR-LINE-ACCOUNT     TO R3-LINE-ACCOUNT                      
027220       MOVE IN-EKH-SUBEL          TO R3-LINE-AMOUNT-LC                    
027230       MOVE SPACE                 TO R3-LINE-PROFIT-CENTER                
027240       MOVE SPAR-LINE-ORDER       TO R3-LINE-ORDER                        
027250       MOVE SPAR-LINE-COST-CENTER TO R3-LINE-COST-CENTER                  
027260       PERFORM S04-WRITE-W51073A                                          
027270                                                                          
027280     WHEN 'FÖRS'                                                          
027290     WHEN 'LEG'                                                           
027300     WHEN 'FRAKT'                                                         
027310       IF IN-EKH-KDEKNIVA = 'LEG'                                         
027320         MOVE +3                 TO KONT-KDCALL                           
027330         MOVE ZERO               TO KONT-KDFRAKT                          
027340       ELSE                                                               
027350         IF IN-EKH-KDEKNIVA = 'FRAKT'                                     
027360           MOVE +4               TO KONT-KDCALL                           
027370           MOVE IN-EKH-KDFRAKT   TO KONT-KDFRAKT                          
027380         ELSE                                                             
027390           IF IN-EKH-KDEKNIVA = 'FÖRS'                                    
027400             MOVE +5             TO KONT-KDCALL                           
027410             MOVE ZERO           TO KONT-KDFRAKT                          
027420           END-IF                                                         
027430         END-IF                                                           
027440       END-IF                                                             
027450       MOVE IN-EKH-IDDISTR      TO KONT-IDDISTR                           
027460       CALL W510KONT USING KONT-W510KONT                                  
027470       MOVE KONT-IDKONTO        TO WS-R3-ACCOUNT-10                       
027480       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
027490       MOVE KONT-IDKST          TO WS-RED-IDKST                           
027500       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
027510       MOVE KONT-IDANALYS       TO R3-LINE-ORDER                          
027520       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
027530       PERFORM S04-WRITE-W51073A                                          
027540                                                                          
027550* HÄR GENERERAS EN R3-POST/DEBET DÅ SUMMAPOSTEN EJ GÅR ATT                
027560* ANVÄNDA PGA ATT FELAKTIGT SUMMABELOPP KOMMER FRÅN FAKTURERINGEN         
027570       MOVE '40'                  TO R3-LINE-POSTING-KEY                  
027580       MOVE '+'                   TO R3-LINE-AMOUNT-SIGN                  
027590       MOVE SPAR-LINE-ACCOUNT     TO R3-LINE-ACCOUNT                      
027600       MOVE IN-EKH-SUBEL          TO R3-LINE-AMOUNT-LC                    
027610       MOVE SPAR-LINE-ORDER       TO R3-LINE-ORDER                        
027620       MOVE SPAR-LINE-COST-CENTER TO R3-LINE-COST-CENTER                  
027630       PERFORM S04-WRITE-W51073A                                          
027640                                                                          
027650     WHEN 'DDI'                                                           
027660       IF SYST-IDSEKVNR = 1                                               
027670         IF IN-EKH-SUBEL > 0                                              
027680           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
027690           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
027700           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
027710           PERFORM S04-WRITE-W51073A                                      
027720         END-IF                                                           
027730       END-IF                                                             
027740                                                                          
027750       IF SYST-IDSEKVNR = 2                                               
027760         IF IN-EKH-SUBEL < 0                                              
027770           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
027780           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
027790           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
027800           PERFORM S04-WRITE-W51073A                                      
027810         END-IF                                                           
027820       END-IF                                                             
027830     END-EVALUATE                                                         
027840     .                                                                    
027850     EJECT                                                                
027860                                                                          
027870 CEDB-SUB-EVENT-201-202 SECTION.                                          
027880     EVALUATE IN-EKH-KDEKNIVA                                             
027890     WHEN 'SUM'                                                           
027900       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
027910       MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT-VERS2                       
027920       IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                                 
027930          MOVE IN-EKH-IDDC-SEND  TO W-IDDC-B6                             
027940          PERFORM IMS-GU-WDB601                                           
027950       END-IF                                                             
027960       IF DCS-CDC                                                         
027970         MOVE '5'               TO WS-ACCOUNT-6                           
027980         MOVE '158600000112'    TO R3-LINE-ORDER                          
027990       ELSE                                                               
028000         MOVE '6'               TO WS-ACCOUNT-6                           
028010         MOVE '158600000127'    TO R3-LINE-ORDER                          
028020       END-IF                                                             
028030       MOVE WS-ACCOUNT-VERS2    TO WS-R3-ACCOUNT-10                       
028040       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
028050       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
028060       PERFORM S04-WRITE-W51073A                                          
028070                                                                          
028080     WHEN 'DET'                                                           
028090       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
028100       MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                             
028110       IF KDPRODSL-LYNK                                                   
028120         MOVE '1'               TO WS-ACCOUNT-5                           
028130         MOVE '0'               TO WS-ACCOUNT-5A                          
028140         MOVE 'LYNK'            TO WS-PRCTR(1:4)                          
028150         MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                   
028160         MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                       
028170         MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                  
028180         MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                       
028190       END-IF                                                             
028200       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
028210       COMPUTE R3-LINE-AMOUNT-LC =                                        
028220               IN-EKH-KVANTAL * IN-EKH-PRARTSTD                           
028230       END-COMPUTE                                                        
028240       MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                         
028250       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
028260       MOVE SPACE               TO WS-ALLOCATE-REF                        
028270       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
028280       PERFORM S03-WRITE-W51072                                           
028290     END-EVALUATE                                                         
028300     .                                                                    
028310     EJECT                                                                
028320                                                                          
028330 CEE-MAIN-EVENT-202 SECTION.                                              
028340     EVALUATE IN-EKH-KDEKSHT                                              
028350     WHEN '202'                                                           
028360          PERFORM CEEA-SUB-EVENT-202-202                                  
028370     WHEN '203'                                                           
028380          PERFORM CEEB-SUB-EVENT-202-203                                  
028390     WHEN '204'                                                           
028400          PERFORM CEEC-SUB-EVENT-202-204                                  
028410     END-EVALUATE                                                         
028420     .                                                                    
028430     EJECT                                                                
028440                                                                          
028450 CEEA-SUB-EVENT-202-202 SECTION.                                          
028460     EVALUATE IN-EKH-KDEKNIVA                                             
028470     WHEN 'DET'                                                           
028480       IF SYST-IDSEKVNR = 1                                               
028490         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
028500         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
028510         IF KDPRODSL-LYNK                                                 
028520           MOVE '1'               TO WS-ACCOUNT-5                         
028530           MOVE '0'               TO WS-ACCOUNT-5A                        
028540           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
028550           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
028560           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
028570           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
028580           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
028590         END-IF                                                           
028600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
028610         COMPUTE R3-LINE-AMOUNT-LC =                                      
028620                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
028630         END-COMPUTE                                                      
028640         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
028650         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
028660         MOVE SPACE               TO WS-ALLOCATE-REF                      
028670         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
028680         PERFORM S02-WRITE-W51071A                                        
028690       END-IF                                                             
028700                                                                          
028710       IF SYST-IDSEKVNR = 2                                               
028720         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
028730         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
028740         IF KDPRODSL-LYNK                                                 
028750           MOVE '1'               TO WS-ACCOUNT-5                         
028760           MOVE '0'               TO WS-ACCOUNT-5A                        
028770           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
028780           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
028790           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
028800           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
028810           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
028820         END-IF                                                           
028830         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
028840         COMPUTE R3-LINE-AMOUNT-LC =                                      
028850                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
028860         END-COMPUTE                                                      
028870         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
028880         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
028890         MOVE SPACE               TO WS-ALLOCATE-REF                      
028900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
028910         PERFORM S02-WRITE-W51071A                                        
028920       END-IF                                                             
028930     END-EVALUATE                                                         
028940     .                                                                    
028950     EJECT                                                                
028960                                                                          
028970 CEEB-SUB-EVENT-202-203 SECTION.                                          
028980     EVALUATE IN-EKH-KDEKNIVA                                             
028990     WHEN 'DET'                                                           
029000       IF SYST-IDSEKVNR = 1                                               
029010         IF IN-EKH-KVANTAL < 0                                            
029020           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
029030           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
029040           IF KDPRODSL-LYNK                                               
029050             MOVE '1'               TO WS-ACCOUNT-5                       
029060             MOVE '0'               TO WS-ACCOUNT-5A                      
029070             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
029080             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
029090             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
029100             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
029110             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
029120           END-IF                                                         
029130           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
029140           COMPUTE R3-LINE-AMOUNT-LC =                                    
029150                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
029160           END-COMPUTE                                                    
029170           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
029180           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
029190           MOVE SPACE               TO WS-ALLOCATE-REF                    
029200           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
029210           PERFORM S02-WRITE-W51071A                                      
029220         END-IF                                                           
029230       END-IF                                                             
029240                                                                          
029250       IF SYST-IDSEKVNR = 2                                               
029260         IF IN-EKH-KVANTAL < 0                                            
029270           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
029280           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
029290           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
029300           COMPUTE R3-LINE-AMOUNT-LC =                                    
029310                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
029320           END-COMPUTE                                                    
029330           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
029340           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
029350           MOVE SPACE               TO WS-ALLOCATE-REF                    
029360           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
029370           PERFORM S02-WRITE-W51071A                                      
029380         END-IF                                                           
029390       END-IF                                                             
029400                                                                          
029410       IF SYST-IDSEKVNR = 3                                               
029420         IF IN-EKH-KVANTAL > 0                                            
029430           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
029440           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
029450           IF KDPRODSL-LYNK                                               
029460             MOVE '1'               TO WS-ACCOUNT-5                       
029470             MOVE '0'               TO WS-ACCOUNT-5A                      
029480             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
029490             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
029500             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
029510             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
029520             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
029530           END-IF                                                         
029540           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
029550           COMPUTE R3-LINE-AMOUNT-LC =                                    
029560                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
029570           END-COMPUTE                                                    
029580           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
029590           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
029600           MOVE SPACE               TO WS-ALLOCATE-REF                    
029610           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
029620           PERFORM S02-WRITE-W51071A                                      
029630         END-IF                                                           
029640       END-IF                                                             
029650                                                                          
029660       IF SYST-IDSEKVNR = 4                                               
029670         IF IN-EKH-KVANTAL > 0                                            
029680           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
029690           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
029700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
029710           COMPUTE R3-LINE-AMOUNT-LC =                                    
029720                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
029730           END-COMPUTE                                                    
029740           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
029750           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
029760           MOVE SPACE               TO WS-ALLOCATE-REF                    
029770           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
029780           PERFORM S02-WRITE-W51071A                                      
029790         END-IF                                                           
029800       END-IF                                                             
029810     END-EVALUATE                                                         
029820     .                                                                    
029830     EJECT                                                                
029840                                                                          
029850 CEEC-SUB-EVENT-202-204 SECTION.                                          
029860     EVALUATE IN-EKH-KDEKNIVA                                             
029870     WHEN 'DET'                                                           
029880       IF SYST-IDSEKVNR = 1                                               
029890         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
029900         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
029910         IF KDPRODSL-LYNK                                                 
029920           MOVE '1'               TO WS-ACCOUNT-5                         
029930           MOVE '0'               TO WS-ACCOUNT-5A                        
029940           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
029950           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
029960           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
029970           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
029980           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
029990         END-IF                                                           
030000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
030010         COMPUTE R3-LINE-AMOUNT-LC =                                      
030020                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
030030         END-COMPUTE                                                      
030040         ADD  R3-LINE-AMOUNT-LC   TO SPAR-SUMMA-202-204                   
030050         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
030060         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
030070         MOVE SPACE               TO WS-ALLOCATE-REF                      
030080         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
030090         PERFORM S03-WRITE-W51072                                         
030100       END-IF                                                             
030110                                                                          
030120     WHEN 'SUM'                                                           
030130       IF SYST-IDSEKVNR = 1                                               
030140         IF SPAR-SUMMA-202-204 < 0                                        
030150           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
030160           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
030170           MOVE SPAR-SUMMA-202-204  TO R3-LINE-AMOUNT-LC                  
030180           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
030190           PERFORM S04-WRITE-W51073A                                      
030200           MOVE ZERO                TO SPAR-SUMMA-202-204                 
030210         END-IF                                                           
030220       END-IF                                                             
030230                                                                          
030240       IF SYST-IDSEKVNR = 2                                               
030250         IF SPAR-SUMMA-202-204 > 0                                        
030260           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
030270           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
030280           MOVE SPAR-SUMMA-202-204  TO R3-LINE-AMOUNT-LC                  
030290           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
030300           PERFORM S04-WRITE-W51073A                                      
030310           MOVE ZERO                TO SPAR-SUMMA-202-204                 
030320         END-IF                                                           
030330       END-IF                                                             
030340                                                                          
030350     WHEN 'AVDR'                                                          
030360     WHEN 'FÖRS'                                                          
030370     WHEN 'LEG'                                                           
030380     WHEN 'FRAKT'                                                         
030390       IF IN-EKH-KDEKNIVA = 'LEG'                                         
030400         MOVE +3                 TO KONT-KDCALL                           
030410         MOVE ZERO               TO KONT-KDFRAKT                          
030420       ELSE                                                               
030430         IF IN-EKH-KDEKNIVA = 'FRAKT'                                     
030440           MOVE +4               TO KONT-KDCALL                           
030450           MOVE IN-EKH-KDFRAKT   TO KONT-KDFRAKT                          
030460         ELSE                                                             
030470           IF IN-EKH-KDEKNIVA = 'FÖRS'                                    
030480             MOVE +5             TO KONT-KDCALL                           
030490             MOVE ZERO           TO KONT-KDFRAKT                          
030500           ELSE                                                           
030510             IF IN-EKH-KDEKNIVA = 'AVDR'                                  
030520               MOVE +6           TO KONT-KDCALL                           
030530               MOVE ZERO         TO KONT-KDFRAKT                          
030540             END-IF                                                       
030550           END-IF                                                         
030560         END-IF                                                           
030570       END-IF                                                             
030580       MOVE IN-EKH-IDDISTR      TO KONT-IDDISTR                           
030590       CALL W510KONT USING KONT-W510KONT                                  
030600       MOVE KONT-IDKONTO        TO WS-R3-ACCOUNT-10                       
030610       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
030620       MOVE KONT-IDKST          TO WS-RED-IDKST                           
030630       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
030640       MOVE KONT-IDANALYS       TO R3-LINE-ORDER                          
030650       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
030660       ADD  R3-LINE-AMOUNT-LC   TO SPAR-SUMMA-202-204                     
030670       PERFORM S04-WRITE-W51073A                                          
030680                                                                          
030690     WHEN 'EMB'                                                           
030700       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
030710       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
030720       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
030730       ADD  R3-LINE-AMOUNT-LC   TO SPAR-SUMMA-202-204                     
030740       PERFORM S04-WRITE-W51073A                                          
030750                                                                          
030760     END-EVALUATE                                                         
030770     .                                                                    
030780     EJECT                                                                
030790                                                                          
030800 CEF-MAIN-EVENT-203 SECTION.                                              
030810     EVALUATE IN-EKH-KDEKSHT                                              
030820     WHEN '201'                                                           
030830          PERFORM CEFA-SUB-EVENT-203-201                                  
030840     WHEN '202'                                                           
030850          PERFORM CEFB-SUB-EVENT-203-202                                  
030860     END-EVALUATE                                                         
030870     .                                                                    
030880     EJECT                                                                
030890                                                                          
030900 CEFA-SUB-EVENT-203-201 SECTION.                                          
030910     EVALUATE IN-EKH-KDEKNIVA                                             
030920     WHEN 'DET'                                                           
030930       IF SYST-IDSEKVNR = 1                                               
030940* KONTO EJ MANUELLT REGISTRERAT                                           
030950         IF IN-EKH-IDKONTO > 0                                            
030960           MOVE IN-EKH-IDKONTO  TO WS-R3-ACCOUNT-10                       
030970           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
030980           IF IN-EKH-PRARTNTO > 0                                         
030990             COMPUTE R3-LINE-AMOUNT-LC =                                  
031000                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
031010             END-COMPUTE                                                  
031020           ELSE                                                           
031030             COMPUTE R3-LINE-AMOUNT-LC =                                  
031040                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
031050             END-COMPUTE                                                  
031060           END-IF                                                         
031070           MOVE IN-EKH-IDANALYS   TO R3-LINE-ORDER                        
031080           MOVE IN-EKH-IDKST      TO WS-RED-IDKST                         
031090           MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                  
031100           PERFORM S03-WRITE-W51072                                       
031110         END-IF                                                           
031120       END-IF                                                             
031130                                                                          
031140       IF SYST-IDSEKVNR = 2                                               
031150         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
031160         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
031170         IF KDPRODSL-LYNK                                                 
031180           MOVE '1'               TO WS-ACCOUNT-5                         
031190           MOVE '0'               TO WS-ACCOUNT-5A                        
031200           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
031210           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
031220           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
031230           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
031240           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
031250         END-IF                                                           
031260         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
031270         COMPUTE R3-LINE-AMOUNT-LC =                                      
031280                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
031290         END-COMPUTE                                                      
031300         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
031310         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
031320         MOVE SPACE             TO WS-ALLOCATE-REF                        
031330         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
031340         PERFORM S03-WRITE-W51072                                         
031350       END-IF                                                             
031360                                                                          
031370* NETTOPRIS ÄR STÖRRE ÄN NOLL OM DET RÖR SIG OM EN EXTERN KUND            
031380       IF IN-EKH-PRARTNTO > 0                                             
031390         COMPUTE R3-LINE-AMOUNT-LC =                                      
031400                 IN-EKH-KVANTAL *                                         
031410                (IN-EKH-PRARTNTO - IN-EKH-PRARTSTD)                       
031420         END-COMPUTE                                                      
031430                                                                          
031440         IF SYST-IDSEKVNR = 3                                             
031450           IF IN-EKH-PRARTNTO > IN-EKH-PRARTSTD                           
031460             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
031470             MOVE SYST-IDKST          TO WS-RED-IDKST                     
031480             MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER              
031490             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
031500             PERFORM S04-WRITE-W51073A                                    
031510           END-IF                                                         
031520         END-IF                                                           
031530                                                                          
031540         IF SYST-IDSEKVNR = 4                                             
031550           IF IN-EKH-PRARTNTO < IN-EKH-PRARTSTD                           
031560             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
031570             MOVE SYST-IDKST          TO WS-RED-IDKST                     
031580             MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER              
031590             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
031600             PERFORM S04-WRITE-W51073A                                    
031610           END-IF                                                         
031620         END-IF                                                           
031630       END-IF                                                             
031640                                                                          
031650       IF SYST-IDSEKVNR = 5                                               
031660* KONTO EJ MANUELLT REGISTRERAT                                           
031670         IF IN-EKH-IDKONTO = 0                                            
031680           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
031690           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
031700           IF IN-EKH-PRARTNTO > 0                                         
031710             COMPUTE R3-LINE-AMOUNT-LC =                                  
031720                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
031730             END-COMPUTE                                                  
031740           ELSE                                                           
031750             COMPUTE R3-LINE-AMOUNT-LC =                                  
031760                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
031770             END-COMPUTE                                                  
031780           END-IF                                                         
031790           MOVE SYST-IDANALYS     TO R3-LINE-ORDER                        
031800           MOVE SPACES            TO R3-LINE-COST-CENTER                  
031810           PERFORM S03-WRITE-W51072                                       
031820         END-IF                                                           
031830       END-IF                                                             
031840                                                                          
031850     END-EVALUATE                                                         
031860     .                                                                    
031870     EJECT                                                                
031880                                                                          
031890 CEFB-SUB-EVENT-203-202 SECTION.                                          
031900     EVALUATE IN-EKH-KDEKNIVA                                             
031910     WHEN 'DET'                                                           
031920       IF SYST-IDSEKVNR = 1                                               
031930         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
031940         MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                             
031950         IF KDPRODSL-LYNK                                                 
031960           MOVE '1'             TO WS-ACCOUNT-5                           
031970           MOVE '0'             TO WS-ACCOUNT-5A                          
031980           MOVE 'LYNK'          TO WS-PRCTR(1:4)                          
031990           MOVE IN-EKH-KDPRODSL TO WS-PRCTR-PRODSL-DISP                   
032000           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
032010           MOVE WS-PRCTR        TO R3-LINE-PROFIT-CENTER                  
032020           MOVE WS-ACCOUNT      TO WS-R3-ACCOUNT-10                       
032030         END-IF                                                           
032040         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
032050         COMPUTE R3-LINE-AMOUNT-LC =                                      
032060                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
032070         END-COMPUTE                                                      
032080         MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                         
032090         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
032100         MOVE SPACE             TO WS-ALLOCATE-REF                        
032110         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
032120         MOVE 'SEPV'            TO R3-LINE-NEW-COMPANY-CODE               
032130         PERFORM S03-WRITE-W51072                                         
032140       END-IF                                                             
032150       COMPUTE R3-LINE-AMOUNT-LC =                                        
032160               IN-EKH-KVANTAL *                                           
032170              (IN-EKH-PRARTNTO - IN-EKH-PRARTSTD)                         
032180       END-COMPUTE                                                        
032190                                                                          
032200       IF SYST-IDSEKVNR = 2                                               
032210         IF IN-EKH-PRARTNTO > IN-EKH-PRARTSTD                             
032220           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
032230           MOVE SYST-IDKST      TO WS-RED-IDKST                           
032240           MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                    
032250           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
032260           MOVE 'SEPV'          TO R3-LINE-NEW-COMPANY-CODE               
032270           PERFORM S03-WRITE-W51072                                       
032280         END-IF                                                           
032290       END-IF                                                             
032300                                                                          
032310       IF SYST-IDSEKVNR = 3                                               
032320         IF IN-EKH-PRARTNTO < IN-EKH-PRARTSTD                             
032330           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
032340           MOVE SYST-IDKST      TO WS-RED-IDKST                           
032350           MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                    
032360           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
032370           MOVE 'SEPV'          TO R3-LINE-NEW-COMPANY-CODE               
032380           PERFORM S04-WRITE-W51073A                                      
032390         END-IF                                                           
032400       END-IF                                                             
032410                                                                          
032420       IF SYST-IDSEKVNR = 4                                               
032430         MOVE IN-EKH-IDKONTO    TO WS-R3-ACCOUNT-10                       
032440         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
032450         COMPUTE R3-LINE-AMOUNT-LC =                                      
032460                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
032470         END-COMPUTE                                                      
032480         IF IN-EKH-IDKST    = SPACE AND                                   
032490            IN-EKH-IDANALYS = SPACE                                       
032500           PERFORM S12-PROFITCENTER                                       
032510         END-IF                                                           
032520         MOVE IN-EKH-IDANALYS   TO R3-LINE-ORDER                          
032530         MOVE IN-EKH-IDKST      TO WS-RED-IDKST                           
032540         MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                    
032550         MOVE 'SE09'            TO R3-LINE-NEW-COMPANY-CODE               
032560         PERFORM S04-WRITE-W51073A                                        
032570       END-IF                                                             
032580     END-EVALUATE                                                         
032590     .                                                                    
032600     EJECT                                                                
032610                                                                          
032620 CEG-MAIN-EVENT-204 SECTION.                                              
032630     EVALUATE IN-EKH-KDEKSHT                                              
032640     WHEN '201'                                                           
032650          PERFORM CEGA-SUB-EVENT-204-201                                  
032660     WHEN '203'                                                           
032670          PERFORM CEGB-SUB-EVENT-204-203                                  
032680     WHEN '204'                                                           
032690          PERFORM CEGB-SUB-EVENT-204-204                                  
032700     WHEN '205'                                                           
032710          PERFORM CEGB-SUB-EVENT-204-205                                  
032720     WHEN '206'                                                           
032730          PERFORM CEGB-SUB-EVENT-204-206                                  
032740     WHEN '208'                                                           
032750          PERFORM CEGB-SUB-EVENT-204-208                                  
032760     WHEN '302'                                                           
032770          PERFORM CEGE-SUB-EVENT-204-302                                  
032780     WHEN '303'                                                           
032790          PERFORM CEGE-SUB-EVENT-204-303                                  
032800     END-EVALUATE                                                         
032810     .                                                                    
032820     EJECT                                                                
032830                                                                          
032840 CEGA-SUB-EVENT-204-201 SECTION.                                          
032850     EVALUATE IN-EKH-KDEKNIVA                                             
032860     WHEN 'DET'                                                           
032870         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
032880         MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
032890                                                                          
032900* R-FAKTURA                                                               
032910       IF SYST-IDSEKVNR = 1                                               
032920         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
032930         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
032940         IF BET-KDTRADP(3:2) NOT = SPACE                                  
032950           MOVE '1'               TO WS-ACCOUNT-4                         
032960         ELSE                                                             
032970           MOVE '3'               TO WS-ACCOUNT-4                         
032980         END-IF                                                           
032990         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
033000         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
033010         COMPUTE R3-LINE-AMOUNT-LC =                                      
033020                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
033030         END-COMPUTE                                                      
033040         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
033050         MOVE SPACE               TO WS-ALLOCATE-DC                       
033060         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
033070         MOVE SPACE               TO WS-ALLOCATE-REF                      
033080         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
033090         IF KDPRODSL-LYNK                                                 
033100           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
033110           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
033120           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
033130           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
033140         END-IF                                                           
033150         IF IN-EKH-IDKST = 'VOCEXT'                                       
033160           MOVE IN-EKH-IDKST      TO R3-LINE-PROFIT-CENTER                
033170         END-IF                                                           
033180         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
033190         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
033200         PERFORM S03-WRITE-W51072                                         
033210       END-IF                                                             
033220                                                                          
033230       IF SYST-IDSEKVNR = 2                                               
033240* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
033250*        PERFORM S32-CHECK-POS-NEG                                        
033260*        IF WS-LINE-AMOUNT > 0                                            
033270           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
033280           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
033290           IF BET-KDTRADP(3:2) NOT = SPACE                                
033300             MOVE '1'               TO WS-ACCOUNT-4                       
033310           ELSE                                                           
033320             MOVE '3'               TO WS-ACCOUNT-4                       
033330           END-IF                                                         
033340           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
033350           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
033360           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
033370           COMPUTE R3-LINE-AMOUNT-LC =                                    
033380                 IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)        
033390           IF KDPRODSL-LYNK                                               
033400             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
033410             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
033420             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
033430             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
033440           END-IF                                                         
033450           IF IN-EKH-IDKST = 'VOCEXT'                                     
033460             MOVE IN-EKH-IDKST      TO R3-LINE-PROFIT-CENTER              
033470           END-IF                                                         
033480           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
033490           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
033500           PERFORM S03-WRITE-W51072                                       
033510*        END-IF                                                           
033520       END-IF                                                             
033530                                                                          
033540       IF SYST-IDSEKVNR = 3                                               
033550         IF IN-EKH-PRHEMTAG > ZERO                                        
033560           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
033570           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
033580           COMPUTE R3-LINE-AMOUNT-LC =                                    
033590                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
033600           END-COMPUTE                                                    
033610           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
033620           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
033630           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
033640           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
033650           PERFORM S03-WRITE-W51072                                       
033660         END-IF                                                           
033670       END-IF                                                             
033680                                                                          
033690       IF SYST-IDSEKVNR = 4                                               
033700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
033710         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
033720         IF BET-KDTRADP(3:2) NOT = SPACE                                  
033730           MOVE '1'               TO WS-ACCOUNT-4                         
033740         ELSE                                                             
033750           MOVE '3'               TO WS-ACCOUNT-4                         
033760         END-IF                                                           
033770         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
033780         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
033790         COMPUTE R3-LINE-AMOUNT-LC =                                      
033800                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
033810                                   IN-EKH-PRINK)                          
033820         END-COMPUTE                                                      
033830         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
033840         IF IN-EKH-IDKST = 'VOCEXT'                                       
033850           MOVE IN-EKH-IDKST      TO R3-LINE-PROFIT-CENTER                
033860         END-IF                                                           
033870         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
033880         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
033890         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
033900           PERFORM S03-WRITE-W51072                                       
033910         END-IF                                                           
033920       END-IF                                                             
033930                                                                          
033940       IF SYST-IDSEKVNR = 5                                               
033950         IF IN-EKH-PRHEMTAG > ZERO                                        
033960           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
033970           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
033980           IF BET-KDTRADP(3:2) NOT = SPACE                                
033990             MOVE '1'               TO WS-ACCOUNT-4                       
034000           ELSE                                                           
034010             MOVE '3'               TO WS-ACCOUNT-4                       
034020           END-IF                                                         
034030           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
034040           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
034050           COMPUTE R3-LINE-AMOUNT-LC =                                    
034060                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
034070           END-COMPUTE                                                    
034080           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
034090           IF IN-EKH-IDKST = 'VOCEXT'                                     
034100             MOVE IN-EKH-IDKST      TO R3-LINE-PROFIT-CENTER              
034110           END-IF                                                         
034120           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
034130           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
034140           PERFORM S03-WRITE-W51072                                       
034150         END-IF                                                           
034160       END-IF                                                             
034170                                                                          
034180       IF SYST-IDSEKVNR = 6                                               
034190         IF IN-EKH-PRHEMTAG > ZERO                                        
034200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
034210           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
034220           COMPUTE R3-LINE-AMOUNT-LC =                                    
034230                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
034240           END-COMPUTE                                                    
034250           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
034260           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
034270           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
034280           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
034290           PERFORM S03-WRITE-W51072                                       
034300         END-IF                                                           
034310       END-IF                                                             
034320                                                                          
034330                                                                          
034340       IF SYST-IDSEKVNR = 9                                               
034350         IF NOT BYT24-FIKTIV                                              
034360           IF IN-EKH-KDSORT NOT = 'SW'                                    
034370                                                                          
034380* HÄR BOKAS EJ FIKTIVA BYTESARTIKLAR/                                     
034390*           EJ SOFTWARE                                                   
034400*           EJ TILLÄGGSDEBITERING AV RETURER                              
034410               MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                 
034420               MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                       
034430               IF KDPRODSL-LYNK                                           
034440                 MOVE '1'             TO WS-ACCOUNT-5                     
034450                 MOVE '0'             TO WS-ACCOUNT-5A                    
034460                 MOVE 'LYNK'          TO WS-PRCTR(1:4)                    
034470                 MOVE IN-EKH-KDPRODSL TO WS-PRCTR-PRODSL-DISP             
034480                 MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)               
034490                 MOVE WS-PRCTR        TO R3-LINE-PROFIT-CENTER            
034500                 MOVE WS-ACCOUNT      TO WS-R3-ACCOUNT-10                 
034510               END-IF                                                     
034520               MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                  
034530               COMPUTE R3-LINE-AMOUNT-LC =                                
034540                       IN-EKH-KVANTAL * IN-EKH-PRARTSTD                   
034550               END-COMPUTE                                                
034560               MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                   
034570               MOVE SPACE             TO WS-ALLOCATE-DISTR                
034580               MOVE SPACE             TO WS-ALLOCATE-REF                  
034590               MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                 
034600               IF IN-EKH-IDKST = 'VOCEXT'                                 
034610                 MOVE IN-EKH-IDKST      TO R3-LINE-PROFIT-CENTER          
034620               END-IF                                                     
034630               MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                
034640               MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE               
034650               PERFORM S03-WRITE-W51072                                   
034660           END-IF                                                         
034670         END-IF                                                           
034680       END-IF                                                             
034690                                                                          
034700       IF SYST-IDSEKVNR = 10                                              
034710* HÄR BOKAS SOFTWARE                                                      
034720         IF IN-EKH-KDSORT = 'SW'                                          
034730           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
034740           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
034750           COMPUTE R3-LINE-AMOUNT-LC =                                    
034760                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
034770           END-COMPUTE                                                    
034780           PERFORM S11-ANALYSIS                                           
034790           IF IN-EKH-IDKST = 'VOCEXT'                                     
034800             MOVE IN-EKH-IDKST      TO R3-LINE-PROFIT-CENTER              
034810             MOVE '158600002785'    TO R3-LINE-ORDER                      
034820           END-IF                                                         
034830           PERFORM S03-WRITE-W51072                                       
034840         END-IF                                                           
034850       END-IF                                                             
034860                                                                          
034870                                                                          
034880     WHEN 'AVDR'                                                          
034890     WHEN 'FÖRS'                                                          
034900     WHEN 'LEG'                                                           
034910     WHEN 'FRAKT'                                                         
034920       IF IN-EKH-KDEKNIVA = 'LEG'                                         
034930         MOVE +3                 TO KONT-KDCALL                           
034940         MOVE ZERO               TO KONT-KDFRAKT                          
034950       ELSE                                                               
034960         IF IN-EKH-KDEKNIVA = 'FRAKT'                                     
034970           MOVE +4               TO KONT-KDCALL                           
034980           MOVE IN-EKH-KDFRAKT   TO KONT-KDFRAKT                          
034990         ELSE                                                             
035000           IF IN-EKH-KDEKNIVA = 'FÖRS'                                    
035010             MOVE +5             TO KONT-KDCALL                           
035020             MOVE ZERO           TO KONT-KDFRAKT                          
035030           ELSE                                                           
035040             IF IN-EKH-KDEKNIVA = 'AVDR'                                  
035050               MOVE +6           TO KONT-KDCALL                           
035060               MOVE ZERO         TO KONT-KDFRAKT                          
035070             END-IF                                                       
035080           END-IF                                                         
035090         END-IF                                                           
035100       END-IF                                                             
035110       MOVE IN-EKH-IDDISTR      TO KONT-IDDISTR                           
035120       CALL W510KONT USING KONT-W510KONT                                  
035130       MOVE KONT-IDKONTO        TO WS-R3-ACCOUNT-10                       
035140       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
035150       MOVE KONT-IDKST          TO WS-RED-IDKST                           
035160       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
035170       MOVE KONT-IDANALYS       TO R3-LINE-ORDER                          
035180       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
035190       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
035200       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
035210       PERFORM S04-WRITE-W51073A                                          
035220                                                                          
035230     WHEN 'EMB'                                                           
035240       IF SYST-IDSEKVNR = 1                                               
035250         IF NOT KDPRODSL-LYNK                                             
035260           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
035270           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
035280           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
035290           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
035300           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
035310           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
035320           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
035330           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
035340           PERFORM S04-WRITE-W51073A                                      
035350         END-IF                                                           
035360       END-IF                                                             
035370                                                                          
035380*** LYNK                                                                  
035390       IF SYST-IDSEKVNR = 2                                               
035400         IF KDPRODSL-LYNK                                                 
035410           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
035420           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
035430           MOVE 'LYNK'              TO WS-PRCTR(1:4)                      
035440           MOVE IN-EKH-KDPRODSL     TO WS-PRCTR-PRODSL-DISP               
035450           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
035460           MOVE WS-PRCTR            TO R3-LINE-PROFIT-CENTER              
035470           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
035480           PERFORM S04-WRITE-W51073A                                      
035490         END-IF                                                           
035500       END-IF                                                             
035510                                                                          
035520     WHEN 'DDI'                                                           
035530* KURSDIFFERENSER                                                         
035540       IF SYST-IDSEKVNR = 1                                               
035550         IF IN-EKH-SUBEL > 0                                              
035560           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
035570           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
035580           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
035590           PERFORM S04-WRITE-W51073A                                      
035600         END-IF                                                           
035610       END-IF                                                             
035620                                                                          
035630       IF SYST-IDSEKVNR = 2                                               
035640         IF IN-EKH-SUBEL < 0                                              
035650           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
035660           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
035670           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
035680           PERFORM S04-WRITE-W51073A                                      
035690         END-IF                                                           
035700       END-IF                                                             
035710     END-EVALUATE                                                         
035720     .                                                                    
035730     EJECT                                                                
035740 CEGB-SUB-EVENT-204-203 SECTION.                                          
035750     EVALUATE IN-EKH-KDEKNIVA                                             
035760     WHEN 'DET'                                                           
035770       MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                         
035780       MOVE IN-EKH-IDARTNR        TO TEST-IDARTNR                         
035790       IF SYST-IDSEKVNR = 1                                               
035800         IF NOT BYT24-FIKTIV                                              
035810           IF IN-EKH-KDSORT NOT = 'SW'                                    
035820             IF IN-EKH-FLLSBOK  = 'N'                                     
035830             AND (IN-EKH-FLOVRLEV = 'Y' OR 'J')                           
035840               NEXT SENTENCE                                              
035850             ELSE                                                         
035860* HÄR BOKAS EJ FIKTIVA BYTESARTIKLAR/                                     
035870*           EJ SOFTWARE                                                   
035880*           EJ TILLÄGGSDEBITERING AV RETURER                              
035890               MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                 
035900               MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                       
035910               IF BET-KDTRADP(3:2) NOT = SPACE                            
035920                 MOVE '1'             TO WS-ACCOUNT-4                     
035930               ELSE                                                       
035940                 MOVE '3'             TO WS-ACCOUNT-4                     
035950               END-IF                                                     
035960               MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                 
035970               MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                  
035980               COMPUTE R3-LINE-AMOUNT-LC =                                
035990                       IN-EKH-KVANTAL * IN-EKH-PRARTSTD                   
036000               END-COMPUTE                                                
036010               PERFORM S11-ANALYSIS                                       
036020               PERFORM S03-WRITE-W51072                                   
036030             END-IF                                                       
036040           END-IF                                                         
036050         END-IF                                                           
036060       END-IF                                                             
036070                                                                          
036080       IF SYST-IDSEKVNR = 2                                               
036090         IF NOT BYT24-FIKTIV                                              
036100           IF IN-EKH-KDSORT NOT = 'SW'                                    
036110             IF IN-EKH-FLLSBOK  = 'N'                                     
036120             AND (IN-EKH-FLOVRLEV = 'Y' OR 'J')                           
036130               NEXT SENTENCE                                              
036140             ELSE                                                         
036150                                                                          
036160* HÄR BOKAS EJ FIKTIVA BYTESARTIKLAR/                                     
036170*           EJ SOFTWARE                                                   
036180*           EJ TILLÄGGSDEBITERING AV RETURER                              
036190               MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                 
036200               MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                  
036210               COMPUTE R3-LINE-AMOUNT-LC =                                
036220                       IN-EKH-KVANTAL * IN-EKH-PRARTSTD                   
036230               END-COMPUTE                                                
036240               PERFORM S11-ANALYSIS                                       
036250               MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                   
036260               MOVE SPACE             TO WS-ALLOCATE-DISTR                
036270               MOVE SPACE             TO WS-ALLOCATE-REF                  
036280               MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                 
036290               PERFORM S03-WRITE-W51072                                   
036300             END-IF                                                       
036310           END-IF                                                         
036320         END-IF                                                           
036330       END-IF                                                             
036340     END-EVALUATE                                                         
036350     .                                                                    
036360     EJECT                                                                
036370 CEGB-SUB-EVENT-204-204 SECTION.                                          
036380     EVALUATE IN-EKH-KDEKNIVA                                             
036390     WHEN 'EXP'                                                           
036400                                                                          
036410       IF SYST-IDSEKVNR = 1                                               
036420         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
036430         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
036440         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
036450         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
036460         IF IN-EKH-KDVALISO = 'SEK'                                       
036470           COMPUTE R3-LINE-AMOUNT-LC =                                    
036480                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
036490           COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 +                  
036500                   R3-LINE-AMOUNT-LC                                      
036510         ELSE                                                             
036520           COMPUTE R3-LINE-AMOUNT-LC =                                    
036530                  IN-EKH-KVANTAL * IN-EKH-PRARTNTO * IN-EKH-PRKURS        
036540           COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 +                  
036550                  R3-LINE-AMOUNT-LC                                       
036560         END-IF                                                           
036570         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
036580         MOVE SPACE               TO WS-ALLOCATE-DC                       
036590         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
036600         MOVE SPACE               TO WS-ALLOCATE-REF                      
036610         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
036620         MOVE SYST-IDKST          TO WS-RED-IDKST                         
036630         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
036640         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
036650         PERFORM S03-WRITE-W51072                                         
036660       END-IF                                                             
036670                                                                          
036680     WHEN 'DDI'                                                           
036690* KURSDIFFERENSER                                                         
036700       IF WS-SUMMA-204-204 < ZERO                                         
036710         IF SYST-IDSEKVNR = 1                                             
036720           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
036730           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
036740           COMPUTE R3-LINE-AMOUNT-LC = WS-SUMMA-204-204                   
036750           PERFORM S03-WRITE-W51072                                       
036760           MOVE ZERO                TO WS-SUMMA-204-204                   
036770         END-IF                                                           
036780       ELSE                                                               
036790         IF WS-SUMMA-204-204 > ZERO                                       
036800           IF SYST-IDSEKVNR = 2                                           
036810             MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                   
036820             MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                    
036830             COMPUTE R3-LINE-AMOUNT-LC = WS-SUMMA-204-204                 
036840             PERFORM S03-WRITE-W51072                                     
036850             MOVE ZERO              TO WS-SUMMA-204-204                   
036860           END-IF                                                         
036870         END-IF                                                           
036880       END-IF                                                             
036890     END-EVALUATE                                                         
036900     .                                                                    
036910     EJECT                                                                
036920                                                                          
036930 CEGB-SUB-EVENT-204-205 SECTION.                                          
036940     EVALUATE IN-EKH-KDEKNIVA                                             
036950     WHEN 'DET'                                                           
036960                                                                          
036970* R-FAKTURA                                                               
036980       IF BET-KDTRADP(3:2) NOT = SPACE                                    
036990         IF SYST-IDSEKVNR = 1                                             
037000           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
037010           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
037020           COMPUTE R3-LINE-AMOUNT-LC =                                    
037030                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
037040           END-COMPUTE                                                    
037050           PERFORM S03-WRITE-W51072                                       
037060         END-IF                                                           
037070       END-IF                                                             
037080                                                                          
037090       IF SYST-IDSEKVNR = 2                                               
037100         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
037110         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
037120         COMPUTE R3-LINE-AMOUNT-LC =                                      
037130                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
037140         END-COMPUTE                                                      
037150         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
037160         PERFORM S03-WRITE-W51072                                         
037170       END-IF                                                             
037180                                                                          
037190       IF BET-KDTRADP(3:2) NOT = SPACE                                    
037200         IF SYST-IDSEKVNR = 3                                             
037210           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
037220           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
037230           COMPUTE R3-LINE-AMOUNT-LC =                                    
037240                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
037250           END-COMPUTE                                                    
037260           PERFORM S03-WRITE-W51072                                       
037270         END-IF                                                           
037280       END-IF                                                             
037290                                                                          
037300       IF BET-KDTRADP(3:2) = SPACE                                        
037310         IF SYST-IDSEKVNR = 4                                             
037320           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
037330           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
037340           COMPUTE R3-LINE-AMOUNT-LC =                                    
037350                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
037360           END-COMPUTE                                                    
037370           PERFORM S03-WRITE-W51072                                       
037380         END-IF                                                           
037390       END-IF                                                             
037400                                                                          
037410       IF BET-KDTRADP(3:2) = SPACE                                        
037420         IF SYST-IDSEKVNR = 5                                             
037430           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
037440           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
037450           COMPUTE R3-LINE-AMOUNT-LC =                                    
037460                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
037470           END-COMPUTE                                                    
037480           PERFORM S03-WRITE-W51072                                       
037490         END-IF                                                           
037500       END-IF                                                             
037510     END-EVALUATE                                                         
037520     .                                                                    
037530     EJECT                                                                
037540                                                                          
037550 CEGB-SUB-EVENT-204-206 SECTION.                                          
037560     EVALUATE IN-EKH-KDEKNIVA                                             
037570     WHEN 'DET'                                                           
037580                                                                          
037590* R-FAKTURA                                                               
037600       IF SYST-IDSEKVNR = 1                                               
037610         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
037620         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
037630         COMPUTE R3-LINE-AMOUNT-LC =                                      
037640                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
037650         END-COMPUTE                                                      
037660         PERFORM S03-WRITE-W51072                                         
037670       END-IF                                                             
037680                                                                          
037690     END-EVALUATE                                                         
037700     .                                                                    
037710     EJECT                                                                
037720                                                                          
037730 CEGB-SUB-EVENT-204-208 SECTION.                                          
037740     EVALUATE IN-EKH-KDEKNIVA                                             
037750     WHEN 'DET'                                                           
037760         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
037770         MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
037780                                                                          
037790* R-FAKTURA                                                               
037800       IF SYST-IDSEKVNR = 1                                               
037810         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
037820         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
037830         IF BET-KDTRADP(3:2) NOT = SPACE                                  
037840           MOVE '1'               TO WS-ACCOUNT-4                         
037850         ELSE                                                             
037860           MOVE '3'               TO WS-ACCOUNT-4                         
037870         END-IF                                                           
037880         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
037890         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
037900         COMPUTE R3-LINE-AMOUNT-LC =                                      
037910                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
037920         END-COMPUTE                                                      
037930         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
037940         MOVE SPACE               TO WS-ALLOCATE-DC                       
037950         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
037960         MOVE SPACE               TO WS-ALLOCATE-REF                      
037970         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
037980         PERFORM S03-WRITE-W51072                                         
037990       END-IF                                                             
038000                                                                          
038010       IF SYST-IDSEKVNR = 2                                               
038020* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
038030*        PERFORM S32-CHECK-POS-NEG                                        
038040*        IF WS-LINE-AMOUNT > 0                                            
038050           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
038060           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
038070           IF BET-KDTRADP(3:2) NOT = SPACE                                
038080             MOVE '1'               TO WS-ACCOUNT-4                       
038090           ELSE                                                           
038100             MOVE '3'               TO WS-ACCOUNT-4                       
038110           END-IF                                                         
038120           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
038130           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
038140           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
038150           COMPUTE R3-LINE-AMOUNT-LC =                                    
038160                 IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)        
038170           PERFORM S03-WRITE-W51072                                       
038180*        END-IF                                                           
038190       END-IF                                                             
038200                                                                          
038210       IF SYST-IDSEKVNR = 3                                               
038220         IF IN-EKH-PRHEMTAG > ZERO                                        
038230           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
038240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
038250           COMPUTE R3-LINE-AMOUNT-LC =                                    
038260                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
038270           END-COMPUTE                                                    
038280           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
038290           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
038300           PERFORM S03-WRITE-W51072                                       
038310         END-IF                                                           
038320       END-IF                                                             
038330                                                                          
038340       IF SYST-IDSEKVNR = 4                                               
038350         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
038360         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
038370         IF BET-KDTRADP(3:2) NOT = SPACE                                  
038380           MOVE '1'               TO WS-ACCOUNT-4                         
038390         ELSE                                                             
038400           MOVE '3'               TO WS-ACCOUNT-4                         
038410         END-IF                                                           
038420         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
038430         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
038440         COMPUTE R3-LINE-AMOUNT-LC =                                      
038450                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
038460                                   IN-EKH-PRINK)                          
038470         END-COMPUTE                                                      
038480         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
038490         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
038500           PERFORM S03-WRITE-W51072                                       
038510         END-IF                                                           
038520       END-IF                                                             
038530                                                                          
038540       IF SYST-IDSEKVNR = 5                                               
038550         IF IN-EKH-PRHEMTAG > ZERO                                        
038560           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
038570           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
038580           IF BET-KDTRADP(3:2) NOT = SPACE                                
038590             MOVE '1'               TO WS-ACCOUNT-4                       
038600           ELSE                                                           
038610             MOVE '3'               TO WS-ACCOUNT-4                       
038620           END-IF                                                         
038630           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
038640           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
038650           COMPUTE R3-LINE-AMOUNT-LC =                                    
038660                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
038670           END-COMPUTE                                                    
038680           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
038690           PERFORM S03-WRITE-W51072                                       
038700         END-IF                                                           
038710       END-IF                                                             
038720                                                                          
038730       IF SYST-IDSEKVNR = 6                                               
038740         IF IN-EKH-PRHEMTAG > ZERO                                        
038750           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
038760           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
038770           COMPUTE R3-LINE-AMOUNT-LC =                                    
038780                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
038790           END-COMPUTE                                                    
038800           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
038810           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
038820           PERFORM S03-WRITE-W51072                                       
038830         END-IF                                                           
038840       END-IF                                                             
038850                                                                          
038860                                                                          
038870       IF SYST-IDSEKVNR = 9                                               
038880         IF NOT BYT24-FIKTIV                                              
038890           IF IN-EKH-KDSORT NOT = 'SW'                                    
038900                                                                          
038910* HÄR BOKAS EJ FIKTIVA BYTESARTIKLAR/                                     
038920*           EJ SOFTWARE                                                   
038930*           EJ TILLÄGGSDEBITERING AV RETURER                              
038940               MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                 
038950               MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                       
038960               MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                  
038970               COMPUTE R3-LINE-AMOUNT-LC =                                
038980                       IN-EKH-KVANTAL * IN-EKH-PRARTSTD                   
038990               END-COMPUTE                                                
039000               MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                   
039010               MOVE SPACE             TO WS-ALLOCATE-DISTR                
039020               MOVE SPACE             TO WS-ALLOCATE-REF                  
039030               MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                 
039040               PERFORM S03-WRITE-W51072                                   
039050           END-IF                                                         
039060         END-IF                                                           
039070       END-IF                                                             
039080                                                                          
039090       IF SYST-IDSEKVNR = 10                                              
039100* HÄR BOKAS SOFTWARE                                                      
039110         IF IN-EKH-KDSORT = 'SW'                                          
039120           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
039130           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
039140           COMPUTE R3-LINE-AMOUNT-LC =                                    
039150                   IN-EKH-KVANTAL * IN-EKH-PRINK                          
039160           END-COMPUTE                                                    
039170           PERFORM S11-ANALYSIS                                           
039180           PERFORM S03-WRITE-W51072                                       
039190         END-IF                                                           
039200       END-IF                                                             
039210                                                                          
039220                                                                          
039230     WHEN 'AVDR'                                                          
039240     WHEN 'FÖRS'                                                          
039250     WHEN 'LEG'                                                           
039260     WHEN 'FRAKT'                                                         
039270       IF IN-EKH-KDEKNIVA = 'LEG'                                         
039280         MOVE +3                 TO KONT-KDCALL                           
039290         MOVE ZERO               TO KONT-KDFRAKT                          
039300       ELSE                                                               
039310         IF IN-EKH-KDEKNIVA = 'FRAKT'                                     
039320           MOVE +4               TO KONT-KDCALL                           
039330           MOVE IN-EKH-KDFRAKT   TO KONT-KDFRAKT                          
039340         ELSE                                                             
039350           IF IN-EKH-KDEKNIVA = 'FÖRS'                                    
039360             MOVE +5             TO KONT-KDCALL                           
039370             MOVE ZERO           TO KONT-KDFRAKT                          
039380           ELSE                                                           
039390             IF IN-EKH-KDEKNIVA = 'AVDR'                                  
039400               MOVE +6           TO KONT-KDCALL                           
039410               MOVE ZERO         TO KONT-KDFRAKT                          
039420             END-IF                                                       
039430           END-IF                                                         
039440         END-IF                                                           
039450       END-IF                                                             
039460       MOVE IN-EKH-IDDISTR      TO KONT-IDDISTR                           
039470       CALL W510KONT USING KONT-W510KONT                                  
039480       MOVE KONT-IDKONTO        TO WS-R3-ACCOUNT-10                       
039490       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
039500       MOVE KONT-IDKST          TO WS-RED-IDKST                           
039510       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
039520       MOVE KONT-IDANALYS       TO R3-LINE-ORDER                          
039530       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
039540       PERFORM S04-WRITE-W51073A                                          
039550                                                                          
039560     WHEN 'EMB'                                                           
039570       IF SYST-IDSEKVNR = 1                                               
039580         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
039590         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
039600         IF BET-KDTRADP(3:2) NOT = SPACE                                  
039610           MOVE '1'               TO WS-ACCOUNT-4                         
039620         ELSE                                                             
039630           MOVE '3'               TO WS-ACCOUNT-4                         
039640         END-IF                                                           
039650         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
039660         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
039670         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
039680         PERFORM S04-WRITE-W51073A                                        
039690       END-IF                                                             
039700                                                                          
039710                                                                          
039720     WHEN 'DDI'                                                           
039730* KURSDIFFERENSER                                                         
039740       IF SYST-IDSEKVNR = 1                                               
039750         IF IN-EKH-SUBEL > 0                                              
039760           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
039770           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
039780           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
039790           PERFORM S04-WRITE-W51073A                                      
039800         END-IF                                                           
039810       END-IF                                                             
039820                                                                          
039830       IF SYST-IDSEKVNR = 2                                               
039840         IF IN-EKH-SUBEL < 0                                              
039850           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
039860           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
039870           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
039880           PERFORM S04-WRITE-W51073A                                      
039890         END-IF                                                           
039900       END-IF                                                             
039910     END-EVALUATE                                                         
039920     .                                                                    
039930     EJECT                                                                
039940 CEGE-SUB-EVENT-204-302 SECTION.                                          
039950     EVALUATE IN-EKH-KDEKNIVA                                             
039960     WHEN 'DET'                                                           
039970         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
039980         MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
039990* R-FAKTURA                                                               
040000       IF SYST-IDSEKVNR = 1                                               
040010         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
040020         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
040030         COMPUTE R3-LINE-AMOUNT-LC =                                      
040040                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
040050         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
040060         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
040070         MOVE SPACE               TO WS-ALLOCATE-DC                       
040080         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
040090         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
040100         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
040110         PERFORM S03-WRITE-W51072                                         
040120       END-IF                                                             
040130                                                                          
040140       IF SYST-IDSEKVNR = 2                                               
040150         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
040160         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
040170         COMPUTE R3-LINE-AMOUNT-LC =                                      
040180                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
040190         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
040200         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
040210         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
040220         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
040230         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
040240         PERFORM S04-WRITE-W51073A                                        
040250       END-IF                                                             
040260                                                                          
040270       IF SYST-IDSEKVNR = 3                                               
040280         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
040290         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
040300         COMPUTE R3-LINE-AMOUNT-LC =                                      
040310                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
040320         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
040330         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
040340         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
040350         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
040360         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
040370         PERFORM S04-WRITE-W51073A                                        
040380       END-IF                                                             
040390                                                                          
040400     WHEN 'FRAKT'                                                         
040410     WHEN 'EMB'                                                           
040420     WHEN 'FÖRS'                                                          
040430       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
040440       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
040450       COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                                
040460               IN-EKH-SUBEL * -1                                          
040470       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
040480       MOVE SPACE               TO WS-ALLOCATE-DC                         
040490       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
040500       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
040510       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
040520                                                                          
040530       PERFORM S03-WRITE-W51072                                           
040540                                                                          
040550     END-EVALUATE                                                         
040560     .                                                                    
040570     EJECT                                                                
040580 CEGE-SUB-EVENT-204-303 SECTION.                                          
040590     EVALUATE IN-EKH-KDEKNIVA                                             
040600     WHEN 'DET'                                                           
040610         MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                         
040620         MOVE IN-EKH-IDARTNR      TO TEST-IDARTNR                         
040630* R-FAKTURA                                                               
040640       IF SYST-IDSEKVNR = 1                                               
040650         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
040660         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
040670         COMPUTE R3-LINE-AMOUNT-LC =                                      
040680                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
040690         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
040700         MOVE BET-KDTRADP         TO R3-LINE-TRADING-PARTNER              
040710         MOVE SPACE               TO WS-ALLOCATE-DC                       
040720         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
040730         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
040740         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
040750         PERFORM S04-WRITE-W51073A                                        
040760       END-IF                                                             
040770                                                                          
040780       IF SYST-IDSEKVNR = 2                                               
040790         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
040800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
040810         COMPUTE R3-LINE-AMOUNT-LC =                                      
040820                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
040830         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
040840         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
040850         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
040860         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
040870         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
040880         PERFORM S03-WRITE-W51072                                         
040890       END-IF                                                             
040900                                                                          
040910       IF SYST-IDSEKVNR = 3                                               
040920         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
040930         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
040940         COMPUTE R3-LINE-AMOUNT-LC =                                      
040950                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
040960         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
040970         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
040980         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
040990         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
041000         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
041010         PERFORM S04-WRITE-W51073A                                        
041020       END-IF                                                             
041030                                                                          
041040     WHEN 'FRAKT'                                                         
041050     WHEN 'EMB'                                                           
041060     WHEN 'LEG'                                                           
041070     WHEN 'FÖRS'                                                          
041080                                                                          
041090       IF SYST-IDSEKVNR = 1                                               
041100         MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                       
041110         MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                        
041120         COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                              
041130                 IN-EKH-SUBEL * -1                                        
041140         MOVE SYST-IDANALYS     TO R3-LINE-ORDER                          
041150         MOVE SPACE             TO WS-ALLOCATE-DC                         
041160         MOVE SPACE             TO WS-ALLOCATE-DISTR                      
041170         MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                        
041180         MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                       
041190                                                                          
041200         PERFORM S03-WRITE-W51072                                         
041210       END-IF                                                             
041220                                                                          
041230                                                                          
041240     END-EVALUATE                                                         
041250     .                                                                    
041260     EJECT                                                                
041270 CEH-MAIN-EVENT-301 SECTION.                                              
041280     EVALUATE IN-EKH-KDEKNIVA                                             
041290     WHEN 'SUM'                                                           
041300       IF SYST-IDSEKVNR = 1                                               
041310         IF IN-EKH-SUBEL > 0                                              
041320           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
041330           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
041340           IF IN-EKH-KDTRADP(3:2) NOT = SPACE                             
041350             MOVE '1'               TO WS-ACCOUNT-4                       
041360           ELSE                                                           
041370             MOVE '3'               TO WS-ACCOUNT-4                       
041380           END-IF                                                         
041390           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
041400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
041410           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
041420                   0.65 * IN-EKH-SUBEL                                    
041430           END-COMPUTE                                                    
041440           MOVE R3-LINE-AMOUNT-LC   TO SPAR-SUMMA                         
041450           MOVE IN-EKH-KDTRADP      TO R3-LINE-TRADING-PARTNER            
041460           MOVE IN-EKH-IDKUNDNR     TO WS-R3-IDPARTNR                     
041470           MOVE WS-R3-IDPARTNR      TO R3-LINE-PA-CUSTOMER                
041480           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
041490           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
041500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
041510           PERFORM S02-WRITE-W51071A                                      
041520         END-IF                                                           
041530       END-IF                                                             
041540                                                                          
041550       IF SYST-IDSEKVNR = 2                                               
041560         IF IN-EKH-SUBEL > 0                                              
041570           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
041580           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
041590           IF IN-EKH-KDTRADP(3:2) NOT = SPACE                             
041600             MOVE '1'               TO WS-ACCOUNT-4                       
041610           ELSE                                                           
041620             MOVE '3'               TO WS-ACCOUNT-4                       
041630           END-IF                                                         
041640           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
041650           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
041660           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
041670           MOVE IN-EKH-KDTRADP      TO R3-LINE-TRADING-PARTNER            
041680           MOVE IN-EKH-IDKUNDNR     TO WS-R3-IDPARTNR                     
041690           MOVE WS-R3-IDPARTNR      TO R3-LINE-PA-CUSTOMER                
041700           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
041710           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
041720           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
041730           PERFORM S02-WRITE-W51071A                                      
041740         END-IF                                                           
041750       END-IF                                                             
041760                                                                          
041770       IF SYST-IDSEKVNR = 3                                               
041780         IF IN-EKH-SUBEL > 0                                              
041790           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
041800           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
041810           IF IN-EKH-KDTRADP(3:2) NOT = SPACE                             
041820             MOVE '6'               TO WS-ACCOUNT-4                       
041830           ELSE                                                           
041840             MOVE '7'               TO WS-ACCOUNT-4                       
041850           END-IF                                                         
041860           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
041870           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
041880* COSTCENTER HÅRDKODAT PGA ATT FÄLTET ÄR NUMERISKT I REGELVERKET          
041890           MOVE 'VCPOCSD'           TO R3-LINE-COST-CENTER                
041900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
041910                   0.35 * IN-EKH-SUBEL                                    
041920           END-COMPUTE                                                    
041930           ADD  R3-LINE-AMOUNT-LC   TO SPAR-SUMMA                         
041940           COMPUTE SPAR-DIFF = IN-EKH-SUBEL -                             
041950                               SPAR-SUMMA                                 
041960           END-COMPUTE                                                    
041970           COMPUTE R3-LINE-AMOUNT-LC = R3-LINE-AMOUNT-LC +                
041980                                       SPAR-DIFF                          
041990           END-COMPUTE                                                    
042000           MOVE IN-EKH-KDTRADP      TO R3-LINE-TRADING-PARTNER            
042010           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
042020           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
042030           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
042040           PERFORM S02-WRITE-W51071A                                      
042050         END-IF                                                           
042060       END-IF                                                             
042070                                                                          
042080       IF SYST-IDSEKVNR = 4                                               
042090         IF IN-EKH-SUBEL < 0                                              
042100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
042110           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
042120           IF IN-EKH-KDTRADP(3:2) NOT = SPACE                             
042130             MOVE '1'               TO WS-ACCOUNT-4                       
042140           ELSE                                                           
042150             MOVE '3'               TO WS-ACCOUNT-4                       
042160           END-IF                                                         
042170           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
042180           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
042190           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
042200                   0.65 * IN-EKH-SUBEL * -1                               
042210           END-COMPUTE                                                    
042220           MOVE R3-LINE-AMOUNT-LC   TO SPAR-SUMMA                         
042230           MOVE IN-EKH-KDTRADP      TO R3-LINE-TRADING-PARTNER            
042240           MOVE IN-EKH-IDKUNDNR     TO WS-R3-IDPARTNR                     
042250           MOVE WS-R3-IDPARTNR      TO R3-LINE-PA-CUSTOMER                
042260           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
042270           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
042280           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
042290           PERFORM S02-WRITE-W51071A                                      
042300         END-IF                                                           
042310       END-IF                                                             
042320                                                                          
042330       IF SYST-IDSEKVNR = 5                                               
042340         IF IN-EKH-SUBEL < 0                                              
042350           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
042360           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
042370           IF IN-EKH-KDTRADP(3:2) NOT = SPACE                             
042380             MOVE '1'               TO WS-ACCOUNT-4                       
042390           ELSE                                                           
042400             MOVE '3'               TO WS-ACCOUNT-4                       
042410           END-IF                                                         
042420           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
042430           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
042440           COMPUTE R3-LINE-AMOUNT-LC = IN-EKH-SUBEL * -1                  
042450           MOVE IN-EKH-KDTRADP      TO R3-LINE-TRADING-PARTNER            
042460           MOVE IN-EKH-IDKUNDNR     TO WS-R3-IDPARTNR                     
042470           MOVE WS-R3-IDPARTNR      TO R3-LINE-PA-CUSTOMER                
042480           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
042490           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
042500           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
042510           PERFORM S02-WRITE-W51071A                                      
042520         END-IF                                                           
042530       END-IF                                                             
042540                                                                          
042550       IF SYST-IDSEKVNR = 6                                               
042560         IF IN-EKH-SUBEL < 0                                              
042570           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
042580           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
042590           IF IN-EKH-KDTRADP(3:2) NOT = SPACE                             
042600             MOVE '6'               TO WS-ACCOUNT-4                       
042610           ELSE                                                           
042620             MOVE '7'               TO WS-ACCOUNT-4                       
042630           END-IF                                                         
042640           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
042650           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
042660* COSTCENTER HÅRDKODAT PGA ATT FÄLTET ÄR NUMERISKT I REGELVERKET          
042670           MOVE 'VCPOCSD'           TO R3-LINE-COST-CENTER                
042680           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
042690                   0.35 * IN-EKH-SUBEL * -1                               
042700           END-COMPUTE                                                    
042710           ADD  R3-LINE-AMOUNT-LC   TO SPAR-SUMMA                         
042720           COMPUTE SPAR-DIFF = (IN-EKH-SUBEL * -1) -                      
042730                                SPAR-SUMMA                                
042740           END-COMPUTE                                                    
042750           COMPUTE R3-LINE-AMOUNT-LC = R3-LINE-AMOUNT-LC +                
042760                                       SPAR-DIFF                          
042770           END-COMPUTE                                                    
042780           MOVE IN-EKH-KDTRADP      TO R3-LINE-TRADING-PARTNER            
042790           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
042800           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
042810           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
042820           PERFORM S02-WRITE-W51071A                                      
042830         END-IF                                                           
042840       END-IF                                                             
042850     END-EVALUATE                                                         
042860     .                                                                    
042870     EJECT                                                                
042880                                                                          
042890 CEI-MAIN-EVENT-302 SECTION.                                              
042900     EVALUATE IN-EKH-KDEKSHT                                              
042910     WHEN '301'                                                           
042920          PERFORM CEIA-SUB-EVENT-302-301                                  
042930     WHEN '302'                                                           
042940          PERFORM CEIB-SUB-EVENT-302-302                                  
042950     WHEN '303'                                                           
042960          PERFORM CEIC-SUB-EVENT-302-303                                  
042970     WHEN '316'                                                           
042980          PERFORM CEID-SUB-EVENT-302-316                                  
042990     WHEN '367'                                                           
043000          PERFORM CEIE-SUB-EVENT-302-367                                  
043010     END-EVALUATE                                                         
043020     .                                                                    
043030     EJECT                                                                
043040                                                                          
043050 CEIA-SUB-EVENT-302-301 SECTION.                                          
043060     EVALUATE IN-EKH-KDEKNIVA                                             
043070     WHEN 'DET'                                                           
043080       IF SYST-IDSEKVNR = 1                                               
043090         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
043100         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
043110         IF KDPRODSL-LYNK                                                 
043120           MOVE '1'               TO WS-ACCOUNT-5                         
043130           MOVE '0'               TO WS-ACCOUNT-5A                        
043140           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
043150           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
043160           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
043170           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
043180           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
043190         END-IF                                                           
043200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
043210         COMPUTE R3-LINE-AMOUNT-LC =                                      
043220                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
043230         END-COMPUTE                                                      
043240         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
043250         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
043260         MOVE SPACE               TO WS-ALLOCATE-REF                      
043270         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
043280         PERFORM S02-WRITE-W51071A                                        
043290       END-IF                                                             
043300                                                                          
043310       IF SYST-IDSEKVNR = 2                                               
043320         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
043330         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT-VERS2                     
043340         IF DCS-IDDC NOT = IN-EKH-IDDC-SEND                               
043350            MOVE IN-EKH-IDDC-SEND  TO W-IDDC-B6                           
043360            PERFORM IMS-GU-WDB601                                         
043370         END-IF                                                           
043380         IF DCS-CDC                                                       
043390           MOVE '5'               TO WS-ACCOUNT-6                         
043400           MOVE '158600000112'    TO R3-LINE-ORDER                        
043410         ELSE                                                             
043420           MOVE '6'               TO WS-ACCOUNT-6                         
043430           MOVE '158600000127'    TO R3-LINE-ORDER                        
043440         END-IF                                                           
043450         MOVE WS-ACCOUNT-VERS2    TO WS-R3-ACCOUNT-10                     
043460         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
043470         COMPUTE R3-LINE-AMOUNT-LC =                                      
043480                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
043490         END-COMPUTE                                                      
043500         PERFORM S02-WRITE-W51071A                                        
043510       END-IF                                                             
043520     END-EVALUATE                                                         
043530     .                                                                    
043540     EJECT                                                                
043550                                                                          
043560 CEIB-SUB-EVENT-302-302 SECTION.                                          
043570     EVALUATE IN-EKH-KDEKNIVA                                             
043580     WHEN 'DET'                                                           
043590*      R31:OR                                                             
043600       IF IN-FIL-IDPGM NOT = 'W4079700'                                   
043610         IF SYST-IDSEKVNR = 1                                             
043620           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
043630           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
043640           IF KDPRODSL-LYNK                                               
043650             MOVE '1'               TO WS-ACCOUNT-5                       
043660             MOVE '0'               TO WS-ACCOUNT-5A                      
043670             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
043680             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
043690             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
043700             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
043710             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
043720           END-IF                                                         
043730           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
043740           COMPUTE R3-LINE-AMOUNT-LC =                                    
043750                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
043760           END-COMPUTE                                                    
043770           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
043780           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
043790           MOVE SPACE               TO WS-ALLOCATE-REF                    
043800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
043810           PERFORM S02-WRITE-W51071A                                      
043820         END-IF                                                           
043830                                                                          
043840         IF SYST-IDSEKVNR = 2                                             
043850           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
043860           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
043870           IF BET-KDTRADP(3:2) NOT = SPACE                                
043880             MOVE '1'               TO WS-ACCOUNT-4                       
043890           ELSE                                                           
043900             MOVE '3'               TO WS-ACCOUNT-4                       
043910           END-IF                                                         
043920           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
043930           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
043940           COMPUTE R3-LINE-AMOUNT-LC =                                    
043950                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
043960           END-COMPUTE                                                    
043970           PERFORM S11-ANALYSIS                                           
043980           PERFORM S02-WRITE-W51071A                                      
043990         END-IF                                                           
044000       ELSE                                                               
044010         IF IN-EKH-KDANMORS = '74' AND IN-EKH-FLLSBOK = 'N'               
044020           IF IN-EKH-IDKONTO > 0                                          
044030             IF SYST-IDSEKVNR = 5                                         
044040               MOVE IN-EKH-IDKONTO    TO WS-R3-ACCOUNT-10                 
044050               MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                       
044060               IF KDPRODSL-LYNK                                           
044070                 MOVE '1'             TO WS-ACCOUNT-5                     
044080                 MOVE '0'             TO WS-ACCOUNT-5A                    
044090                 MOVE 'LYNK'          TO WS-PRCTR(1:4)                    
044100                 MOVE IN-EKH-KDPRODSL TO WS-PRCTR-PRODSL-DISP             
044110                 MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)               
044120                 MOVE WS-PRCTR        TO R3-LINE-PROFIT-CENTER            
044130                 MOVE WS-ACCOUNT      TO WS-R3-ACCOUNT-10                 
044140               END-IF                                                     
044150               MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                  
044160               MOVE IN-EKH-IDANALYS   TO R3-LINE-ORDER                    
044170               MOVE IN-EKH-IDKST      TO WS-RED-IDKST                     
044180               MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER              
044190               COMPUTE R3-LINE-AMOUNT-LC =                                
044200                       IN-EKH-KVANTAL * IN-EKH-PRARTNTO * -1              
044210               END-COMPUTE                                                
044220               MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                   
044230               MOVE SPACE             TO WS-ALLOCATE-DISTR                
044240               MOVE SPACE             TO WS-ALLOCATE-REF                  
044250               MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                 
044260               PERFORM S02-WRITE-W51071A                                  
044270             END-IF                                                       
044280           END-IF                                                         
044290                                                                          
044300           IF SYST-IDSEKVNR = 6                                           
044310             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
044320             MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                       
044330             IF BET-KDTRADP(3:2) NOT = SPACE                              
044340               MOVE '1'               TO WS-ACCOUNT-4                     
044350             ELSE                                                         
044360               MOVE '3'               TO WS-ACCOUNT-4                     
044370             END-IF                                                       
044380             MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                 
044390             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
044400             COMPUTE R3-LINE-AMOUNT-LC =                                  
044410                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
044420             END-COMPUTE                                                  
044430             PERFORM S11-ANALYSIS                                         
044440             PERFORM S02-WRITE-W51071A                                    
044450           END-IF                                                         
044460                                                                          
044470           IF IN-EKH-IDKONTO = 0                                          
044480             IF SYST-IDSEKVNR = 7                                         
044490               MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                 
044500               MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT                       
044510               IF KDPRODSL-LYNK                                           
044520                 MOVE '1'             TO WS-ACCOUNT-5                     
044530                 MOVE '0'             TO WS-ACCOUNT-5A                    
044540                 MOVE 'LYNK'          TO WS-PRCTR(1:4)                    
044550                 MOVE IN-EKH-KDPRODSL TO WS-PRCTR-PRODSL-DISP             
044560                 MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)               
044570                 MOVE WS-PRCTR        TO R3-LINE-PROFIT-CENTER            
044580                 MOVE WS-ACCOUNT      TO WS-R3-ACCOUNT-10                 
044590               END-IF                                                     
044600               MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                  
044610               MOVE IN-EKH-IDANALYS   TO R3-LINE-ORDER                    
044620               MOVE IN-EKH-IDKST      TO WS-RED-IDKST                     
044630               MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER              
044640               COMPUTE R3-LINE-AMOUNT-LC =                                
044650                       IN-EKH-KVANTAL * IN-EKH-PRARTNTO * -1              
044660               END-COMPUTE                                                
044670               MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                   
044680               MOVE SPACE             TO WS-ALLOCATE-DISTR                
044690               MOVE SPACE             TO WS-ALLOCATE-REF                  
044700               MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                 
044710               PERFORM S02-WRITE-W51071A                                  
044720             END-IF                                                       
044730           END-IF                                                         
044740         ELSE                                                             
044750***    R32:OR      ******                                                 
044760           IF SYST-IDSEKVNR = 3                                           
044770             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
044780             MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                       
044790             IF KDPRODSL-LYNK                                             
044800               MOVE '1'               TO WS-ACCOUNT-5                     
044810               MOVE '0'               TO WS-ACCOUNT-5A                    
044820               MOVE 'LYNK'            TO WS-PRCTR(1:4)                    
044830               MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP             
044840               MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                 
044850               MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER            
044860               MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                 
044870             END-IF                                                       
044880             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
044890             COMPUTE R3-LINE-AMOUNT-LC =                                  
044900                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
044910             END-COMPUTE                                                  
044920             MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                   
044930             MOVE SPACE               TO WS-ALLOCATE-DISTR                
044940             MOVE SPACE               TO WS-ALLOCATE-REF                  
044950             MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                 
044960             PERFORM S02-WRITE-W51071A                                    
044970           END-IF                                                         
044980                                                                          
044990           IF SYST-IDSEKVNR = 4                                           
045000             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
045010             MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                       
045020             IF BET-KDTRADP(3:2) NOT = SPACE                              
045030               MOVE '1'               TO WS-ACCOUNT-4                     
045040             ELSE                                                         
045050               MOVE '3'               TO WS-ACCOUNT-4                     
045060             END-IF                                                       
045070             MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                 
045080             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
045090             COMPUTE R3-LINE-AMOUNT-LC =                                  
045100                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
045110             END-COMPUTE                                                  
045120             PERFORM S11-ANALYSIS                                         
045130             PERFORM S02-WRITE-W51071A                                    
045140           END-IF                                                         
045150         END-IF                                                           
045160       END-IF                                                             
045170     END-EVALUATE                                                         
045180     .                                                                    
045190     EJECT                                                                
045200                                                                          
045210 CEIC-SUB-EVENT-302-303 SECTION.                                          
045220     EVALUATE IN-EKH-KDEKNIVA                                             
045230     WHEN 'DET'                                                           
045240       IF SYST-IDSEKVNR = 1                                               
045250         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
045260         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
045270         IF KDPRODSL-LYNK                                                 
045280           MOVE '1'               TO WS-ACCOUNT-5                         
045290           MOVE '0'               TO WS-ACCOUNT-5A                        
045300           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
045310           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
045320           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
045330           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
045340           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
045350         END-IF                                                           
045360         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
045370         COMPUTE R3-LINE-AMOUNT-LC =                                      
045380                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
045390         END-COMPUTE                                                      
045400         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
045410         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
045420         MOVE SPACE               TO WS-ALLOCATE-REF                      
045430         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
045440         PERFORM S02-WRITE-W51071A                                        
045450       END-IF                                                             
045460                                                                          
045470       IF SYST-IDSEKVNR = 2                                               
045480         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
045490         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
045500         IF BET-KDTRADP(3:2) NOT = SPACE                                  
045510           MOVE '1'               TO WS-ACCOUNT-4                         
045520         ELSE                                                             
045530           MOVE '3'               TO WS-ACCOUNT-4                         
045540         END-IF                                                           
045550         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
045560         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
045570         COMPUTE R3-LINE-AMOUNT-LC =                                      
045580                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
045590         END-COMPUTE                                                      
045600         PERFORM S11-ANALYSIS                                             
045610         PERFORM S02-WRITE-W51071A                                        
045620       END-IF                                                             
045630     END-EVALUATE                                                         
045640     .                                                                    
045650     EJECT                                                                
045660                                                                          
045670 CEID-SUB-EVENT-302-316 SECTION.                                          
045680     EVALUATE IN-EKH-KDEKNIVA                                             
045690     WHEN 'DET'                                                           
045700       IF SYST-IDSEKVNR = 1                                               
045710         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
045720         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
045730         COMPUTE R3-LINE-AMOUNT-LC =                                      
045740                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
045750         END-COMPUTE                                                      
045760         PERFORM S11-ANALYSIS                                             
045770         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
045780         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
045790         MOVE SPACE               TO WS-ALLOCATE-REF                      
045800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
045810         PERFORM S02-WRITE-W51071A                                        
045820       END-IF                                                             
045830                                                                          
045840       IF SYST-IDSEKVNR = 2                                               
045850         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
045860         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
045870         IF BET-KDTRADP(3:2) NOT = SPACE                                  
045880           MOVE '1'               TO WS-ACCOUNT-4                         
045890         ELSE                                                             
045900           MOVE '3'               TO WS-ACCOUNT-4                         
045910         END-IF                                                           
045920         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
045930         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
045940         COMPUTE R3-LINE-AMOUNT-LC =                                      
045950                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
045960         END-COMPUTE                                                      
045970         PERFORM S11-ANALYSIS                                             
045980         PERFORM S02-WRITE-W51071A                                        
045990       END-IF                                                             
046000     END-EVALUATE                                                         
046010     .                                                                    
046020     EJECT                                                                
046030                                                                          
046040 CEIE-SUB-EVENT-302-367 SECTION.                                          
046050     EVALUATE IN-EKH-KDEKNIVA                                             
046060     WHEN 'DET'                                                           
046070       IF SYST-IDSEKVNR = 1                                               
046080         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
046090         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
046100         COMPUTE R3-LINE-AMOUNT-LC =                                      
046110                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
046120         END-COMPUTE                                                      
046130         PERFORM S11-ANALYSIS                                             
046140         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
046150         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
046160         MOVE SPACE               TO WS-ALLOCATE-REF                      
046170         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
046180         PERFORM S02-WRITE-W51071A                                        
046190       END-IF                                                             
046200                                                                          
046210       IF SYST-IDSEKVNR = 2                                               
046220         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
046230         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
046240         IF BET-KDTRADP(3:2) NOT = SPACE                                  
046250           MOVE '1'               TO WS-ACCOUNT-4                         
046260         ELSE                                                             
046270           MOVE '3'               TO WS-ACCOUNT-4                         
046280         END-IF                                                           
046290         PERFORM S11-ANALYSIS                                             
046300         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
046310         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
046320         COMPUTE R3-LINE-AMOUNT-LC =                                      
046330                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
046340         END-COMPUTE                                                      
046350         PERFORM S11-ANALYSIS                                             
046360         PERFORM S02-WRITE-W51071A                                        
046370       END-IF                                                             
046380     END-EVALUATE                                                         
046390     .                                                                    
046400     EJECT                                                                
046410                                                                          
046420 CEJ-MAIN-EVENT-303 SECTION.                                              
046430     EVALUATE IN-EKH-KDEKSHT                                              
046440     WHEN '3XX'                                                           
046450          PERFORM CEJ3XX-SUB-EVENT-303-3XX                                
046460     WHEN '301'                                                           
046470          PERFORM CEJ301-SUB-EVENT-303-301                                
046480     WHEN '302'                                                           
046490          PERFORM CEJ302-SUB-EVENT-303-302                                
046500     WHEN '303'                                                           
046510          PERFORM CEJ303-SUB-EVENT-303-303                                
046520     WHEN '304'                                                           
046530          PERFORM CEJ304-SUB-EVENT-303-304                                
046540     WHEN '305'                                                           
046550          PERFORM CEJ305-SUB-EVENT-303-305                                
046560     WHEN '306'                                                           
046570          PERFORM CEJ306-SUB-EVENT-303-306                                
046580     WHEN '307'                                                           
046590          PERFORM CEJ307-SUB-EVENT-303-307                                
046600     WHEN '308'                                                           
046610          PERFORM CEJ308-SUB-EVENT-303-308                                
046620     WHEN '309'                                                           
046630          PERFORM CEJ309-SUB-EVENT-303-309                                
046640     WHEN '310'                                                           
046650          PERFORM CEJ310-SUB-EVENT-303-310                                
046660     WHEN '311'                                                           
046670          PERFORM CEJ311-SUB-EVENT-303-311                                
046680     WHEN '312'                                                           
046690          PERFORM CEJ312-SUB-EVENT-303-312                                
046700     WHEN '314'                                                           
046710          PERFORM CEJ314-SUB-EVENT-303-314                                
046720     WHEN '315'                                                           
046730          PERFORM CEJ315-SUB-EVENT-303-315                                
046740     WHEN '316'                                                           
046750          PERFORM CEJ316-SUB-EVENT-303-316                                
046760     WHEN '318'                                                           
046770          PERFORM CEJ318-SUB-EVENT-303-318                                
046780     WHEN '381'                                                           
046790          PERFORM CEJ381-SUB-EVENT-303-381                                
046800     WHEN '386'                                                           
046810          PERFORM CEJ386-SUB-EVENT-303-386                                
046820     END-EVALUATE                                                         
046830     .                                                                    
046840     EJECT                                                                
046850                                                                          
046860 CEJ3XX-SUB-EVENT-303-3XX SECTION.                                        
046870     EVALUATE IN-EKH-KDEKNIVA                                             
046880     WHEN 'FRAKT'                                                         
046890     WHEN 'FÖRS'                                                          
046900     WHEN 'LEG'                                                           
046910       IF IN-EKH-KDEKNIVA = 'LEG'                                         
046920         MOVE +3                TO KONT-KDCALL                            
046930         MOVE ZERO              TO KONT-KDFRAKT                           
046940       ELSE                                                               
046950         IF IN-EKH-KDEKNIVA = 'FRAKT'                                     
046960           MOVE +4              TO KONT-KDCALL                            
046970           MOVE IN-EKH-KDFRAKT  TO KONT-KDFRAKT                           
046980         ELSE                                                             
046990           IF IN-EKH-KDEKNIVA = 'FÖRS'                                    
047000             MOVE +5            TO KONT-KDCALL                            
047010             MOVE ZERO          TO KONT-KDFRAKT                           
047020           END-IF                                                         
047030         END-IF                                                           
047040       END-IF                                                             
047050       MOVE IN-EKH-IDDISTR      TO KONT-IDDISTR                           
047060       CALL W510KONT USING KONT-W510KONT                                  
047070       MOVE KONT-IDKONTO        TO WS-R3-ACCOUNT-10                       
047080       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
047090       MOVE KONT-IDKST          TO WS-RED-IDKST                           
047100       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
047110       MOVE KONT-IDANALYS       TO R3-LINE-ORDER                          
047120       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
047130       MOVE SPACE               TO WS-ALLOCATE-DC                         
047140       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
047150       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
047160       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
047170       PERFORM S04-WRITE-W51073A                                          
047180                                                                          
047190     WHEN 'DDI'                                                           
047200* KURSDIFFERENSER                                                         
047210       IF SYST-IDSEKVNR = 1                                               
047220         IF IN-EKH-SUBEL < 0                                              
047230           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
047240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
047250           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
047260           MOVE SPACE               TO WS-ALLOCATE-DC                     
047270           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
047280           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
047290           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
047300           PERFORM S04-WRITE-W51073A                                      
047310         END-IF                                                           
047320       END-IF                                                             
047330                                                                          
047340       IF SYST-IDSEKVNR = 2                                               
047350         IF IN-EKH-SUBEL > 0                                              
047360           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
047370           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
047380           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
047390           MOVE SPACE               TO WS-ALLOCATE-DC                     
047400           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
047410           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
047420           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
047430           PERFORM S04-WRITE-W51073A                                      
047440         END-IF                                                           
047450       END-IF                                                             
047460     END-EVALUATE                                                         
047470     .                                                                    
047480     EJECT                                                                
047490                                                                          
047500 CEJ301-SUB-EVENT-303-301 SECTION.                                        
047510     EVALUATE IN-EKH-KDEKNIVA                                             
047520     WHEN 'DET'                                                           
047530       IF SYST-IDSEKVNR = 1                                               
047540         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
047550         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
047560         IF BET-KDTRADP(3:2) NOT = SPACE                                  
047570           MOVE '1'               TO WS-ACCOUNT-4                         
047580         ELSE                                                             
047590           MOVE '3'               TO WS-ACCOUNT-4                         
047600         END-IF                                                           
047610         MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                   
047620*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
047630         IF GMT-KDKUNDKAT = 18                                            
047640           MOVE '13'              TO WS-ACCOUNT-RETURER-5-6               
047650         ELSE                                                             
047660           MOVE '02'              TO WS-ACCOUNT-RETURER-5-6               
047670         END-IF                                                           
047680         MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                     
047690         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
047700         COMPUTE R3-LINE-AMOUNT-LC =                                      
047710                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
047720         END-COMPUTE                                                      
047730         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
047740         MOVE SPACE               TO WS-ALLOCATE-DC                       
047750         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
047760         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
047770         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
047780         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
047790         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
047800         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
047810         IF KDPRODSL-LYNK                                                 
047820           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
047830           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
047840           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
047850           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
047860         END-IF                                                           
047870         PERFORM S03-WRITE-W51072                                         
047880       END-IF                                                             
047890                                                                          
047900       IF SYST-IDSEKVNR = 2                                               
047910* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
047920*        PERFORM S32-CHECK-POS-NEG                                        
047930*        IF WS-LINE-AMOUNT > 0                                            
047940           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
047950           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
047960           IF BET-KDTRADP(3:2) NOT = SPACE                                
047970             MOVE '1'               TO WS-ACCOUNT-4                       
047980           ELSE                                                           
047990             MOVE '3'               TO WS-ACCOUNT-4                       
048000           END-IF                                                         
048010           MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                 
048020*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
048030           IF GMT-KDKUNDKAT = 18                                          
048040             MOVE '30'              TO WS-ACCOUNT-RETURER-5-6             
048050           ELSE                                                           
048060             MOVE '22'              TO WS-ACCOUNT-RETURER-5-6             
048070           END-IF                                                         
048080           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
048090           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
048100           COMPUTE R3-LINE-AMOUNT-LC =                                    
048110             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
048120           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
048130           MOVE SPACE               TO WS-ALLOCATE-DC                     
048140           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
048150           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
048160           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
048170           IF KDPRODSL-LYNK                                               
048180             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
048190             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
048200             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
048210             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
048220           END-IF                                                         
048230           PERFORM S03-WRITE-W51072                                       
048240*        END-IF                                                           
048250       END-IF                                                             
048260                                                                          
048270       IF SYST-IDSEKVNR = 3                                               
048280         IF IN-EKH-PRHEMTAG > ZERO                                        
048290           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
048300           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
048310           COMPUTE R3-LINE-AMOUNT-LC =                                    
048320                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
048330           END-COMPUTE                                                    
048340           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
048350           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
048360           MOVE SPACE               TO WS-ALLOCATE-DC                     
048370           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
048380           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
048390           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
048400           PERFORM S03-WRITE-W51072                                       
048410         END-IF                                                           
048420       END-IF                                                             
048430                                                                          
048440       IF SYST-IDSEKVNR = 4                                               
048450         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
048460         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
048470         IF BET-KDTRADP(3:2) NOT = SPACE                                  
048480           MOVE '1'               TO WS-ACCOUNT-4                         
048490         ELSE                                                             
048500           MOVE '3'               TO WS-ACCOUNT-4                         
048510         END-IF                                                           
048520         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
048530***** ECOM CREDIT                                                         
048540         IF GMT-KDKUNDKAT = 18                                            
048550           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT-VERS2                     
048560           MOVE '2'               TO WS-ACCOUNT-6                         
048570           MOVE WS-ACCOUNT-VERS2  TO WS-R3-ACCOUNT-10                     
048580         END-IF                                                           
048590*****                                                                     
048600         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
048610         COMPUTE R3-LINE-AMOUNT-LC =                                      
048620                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
048630                                   IN-EKH-PRINK)                          
048640         END-COMPUTE                                                      
048650         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
048660         MOVE SPACE               TO WS-ALLOCATE-DC                       
048670         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
048680         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
048690         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
048700         IF KDPRODSL-LYNK                                                 
048710           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
048720           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
048730           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
048740           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
048750         END-IF                                                           
048760         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
048770           PERFORM S03-WRITE-W51072                                       
048780         END-IF                                                           
048790       END-IF                                                             
048800                                                                          
048810       IF SYST-IDSEKVNR = 5                                               
048820         IF IN-EKH-PRHEMTAG > ZERO                                        
048830           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
048840           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
048850           IF BET-KDTRADP(3:2) NOT = SPACE                                
048860             MOVE '1'               TO WS-ACCOUNT-4                       
048870           ELSE                                                           
048880             MOVE '3'               TO WS-ACCOUNT-4                       
048890           END-IF                                                         
048900           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
048910           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
048920           COMPUTE R3-LINE-AMOUNT-LC =                                    
048930                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
048940           END-COMPUTE                                                    
048950           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
048960           MOVE SPACE               TO WS-ALLOCATE-DC                     
048970           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
048980           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
048990           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
049000           PERFORM S03-WRITE-W51072                                       
049010         END-IF                                                           
049020       END-IF                                                             
049030                                                                          
049040       IF SYST-IDSEKVNR = 6                                               
049050         IF IN-EKH-PRHEMTAG > ZERO                                        
049060           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
049070           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
049080           COMPUTE R3-LINE-AMOUNT-LC =                                    
049090                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
049100           END-COMPUTE                                                    
049110           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
049120           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
049130           MOVE SPACE               TO WS-ALLOCATE-DC                     
049140           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
049150           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
049160           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
049170           PERFORM S03-WRITE-W51072                                       
049180         END-IF                                                           
049190       END-IF                                                             
049200                                                                          
049210                                                                          
049220       IF SYST-IDSEKVNR = 9                                               
049230* HÄR BOKAS EJ SOFTWARE                                                   
049240         IF IN-EKH-KDSORT NOT = 'SW'                                      
049250           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
049260           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
049270           IF KDPRODSL-LYNK                                               
049280             MOVE '1'               TO WS-ACCOUNT-5                       
049290             MOVE '0'               TO WS-ACCOUNT-5A                      
049300             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
049310             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
049320             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
049330             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
049340             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
049350           END-IF                                                         
049360           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
049370           COMPUTE R3-LINE-AMOUNT-LC =                                    
049380                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
049390           END-COMPUTE                                                    
049400           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
049410           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
049420           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
049430           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
049440           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
049450           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
049460           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
049470           PERFORM S03-WRITE-W51072                                       
049480         END-IF                                                           
049490       END-IF                                                             
049500                                                                          
049510       IF SYST-IDSEKVNR = 10                                              
049520* HÄR BOKAS SOFTWARE                                                      
049530         IF IN-EKH-KDSORT = 'SW'                                          
049540           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
049550           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
049560           COMPUTE R3-LINE-AMOUNT-LC =                                    
049570                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
049580           END-COMPUTE                                                    
049590           PERFORM S11-ANALYSIS                                           
049600           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
049610           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
049620           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
049630           MOVE SPACE               TO WS-ALLOCATE-DC                     
049640           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
049650           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
049660           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
049670           PERFORM S03-WRITE-W51072                                       
049680         END-IF                                                           
049690       END-IF                                                             
049700                                                                          
049710       IF SYST-IDSEKVNR = 11                                              
049720         IF IN-EKH-PRLANDCO NOT = ZERO                                    
049730           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
049740           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
049750           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
049760           IF IN-EKH-KDANMORS NOT = '30'                                  
049770              MOVE IDKST-57510      TO WS-RED-IDKST                       
049780           ELSE                                                           
049790              MOVE SPACES           TO WS-RED-IDKST                       
049800           END-IF                                                         
049810           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
049820           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
049830           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
049840           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
049850           MOVE SPACE               TO WS-ALLOCATE-DC                     
049860           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
049870           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
049880           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
049890           PERFORM S03-WRITE-W51072                                       
049900         END-IF                                                           
049910       END-IF                                                             
049920                                                                          
049930                                                                          
049940     END-EVALUATE                                                         
049950     .                                                                    
049960     EJECT                                                                
049970                                                                          
049980 CEJ302-SUB-EVENT-303-302 SECTION.                                        
049990     EVALUATE IN-EKH-KDEKNIVA                                             
050000     WHEN 'DET'                                                           
050010       IF SYST-IDSEKVNR = 1                                               
050020         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
050030         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
050040         IF BET-KDTRADP(3:2) NOT = SPACE                                  
050050           MOVE '1'               TO WS-ACCOUNT-4                         
050060         ELSE                                                             
050070           MOVE '3'               TO WS-ACCOUNT-4                         
050080         END-IF                                                           
050090         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
050100*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
050110         IF GMT-KDKUNDKAT = 18                                            
050120           MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                   
050130           MOVE '13'              TO WS-ACCOUNT-RETURER-5-6               
050140           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
050150         END-IF                                                           
050160***                                                                       
050170         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
050180         COMPUTE R3-LINE-AMOUNT-LC =                                      
050190                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
050200         END-COMPUTE                                                      
050210         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
050220         MOVE SPACE               TO WS-ALLOCATE-DC                       
050230         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
050240         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
050250         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
050260         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
050270         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
050280         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
050290         PERFORM S03-WRITE-W51072                                         
050300       END-IF                                                             
050310                                                                          
050320       IF SYST-IDSEKVNR = 2                                               
050330         IF IN-EKH-PRLANDCO NOT = ZERO                                    
050340           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
050350           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
050360           MOVE IN-EKH-PRLANDCO   TO R3-LINE-AMOUNT-LC                    
050370           IF IN-EKH-KDANMORS NOT = '30'                                  
050380              MOVE IDKST-57510      TO WS-RED-IDKST                       
050390           ELSE                                                           
050400              MOVE SPACES           TO WS-RED-IDKST                       
050410           END-IF                                                         
050420           MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                  
050430           MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT                 
050440           MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT                 
050450           MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                         
050460           MOVE SPACE               TO WS-ALLOCATE-DC                     
050470           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
050480           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
050490           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
050500           PERFORM S03-WRITE-W51072                                       
050510         END-IF                                                           
050520       END-IF                                                             
050530     END-EVALUATE                                                         
050540     .                                                                    
050550     EJECT                                                                
050560                                                                          
050570 CEJ303-SUB-EVENT-303-303 SECTION.                                        
050580     EVALUATE IN-EKH-KDEKNIVA                                             
050590     WHEN 'DET'                                                           
050600       IF SYST-IDSEKVNR = 1                                               
050610         MOVE SYST-IDKONTO          TO WS-R3-ACCOUNT-10                   
050620         MOVE WS-R3-ACCOUNT-10      TO WS-ACCOUNT                         
050630         IF BET-KDTRADP(3:2) NOT = SPACE                                  
050640           MOVE '1'                 TO WS-ACCOUNT-4                       
050650         ELSE                                                             
050660           MOVE '3'                 TO WS-ACCOUNT-4                       
050670         END-IF                                                           
050680         MOVE WS-ACCOUNT            TO WS-R3-ACCOUNT-10                   
050690         MOVE WS-R3-ACCOUNT-6       TO R3-LINE-ACCOUNT                    
050700         COMPUTE R3-LINE-AMOUNT-LC =                                      
050710                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
050720         END-COMPUTE                                                      
050730         MOVE W-BET-IDPARTNR-NUM    TO R3-LINE-PA-CUSTOMER                
050740         MOVE SPACE                 TO WS-ALLOCATE-DC                     
050750         MOVE IN-EKH-IDDISTR        TO WS-ALLOCATE-DISTR                  
050760         MOVE IN-EKH-IDFAKT-EXP     TO WS-ALLOCATE-REF                    
050770         MOVE WS-ALLOCATE           TO R3-LINE-ALLOCATE                   
050780         MOVE IN-EKH-KDEKHHT        TO WS-LINE-TEXT-KDEKHHT               
050790         MOVE IN-EKH-KDEKSHT        TO WS-LINE-TEXT-KDEKSHT               
050800         MOVE WS-LINE-TEXT          TO R3-LINE-TEXT                       
050810         PERFORM S03-WRITE-W51072                                         
050820       END-IF                                                             
050830                                                                          
050840       IF SYST-IDSEKVNR = 2                                               
050850         IF IN-EKH-PRLANDCO NOT = ZERO                                    
050860           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
050870           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
050880           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
050890           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
050900           IF IN-EKH-KDANMORS NOT = '30'                                  
050910              MOVE IDKST-57510      TO WS-RED-IDKST                       
050920           ELSE                                                           
050930              MOVE SPACES           TO WS-RED-IDKST                       
050940           END-IF                                                         
050950           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
050960           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
050970           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
050980           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
050990           MOVE SPACE               TO WS-ALLOCATE-DC                     
051000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
051010           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
051020           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
051030           PERFORM S03-WRITE-W51072                                       
051040         END-IF                                                           
051050       END-IF                                                             
051060     END-EVALUATE                                                         
051070     .                                                                    
051080     EJECT                                                                
051090                                                                          
051100 CEJ304-SUB-EVENT-303-304 SECTION.                                        
051110     EVALUATE IN-EKH-KDEKNIVA                                             
051120     WHEN 'DET'                                                           
051130       MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                       
051140       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
051150       COMPUTE R3-LINE-AMOUNT-LC =                                        
051160               IN-EKH-KVANTAL * IN-EKH-PRARTNTO                           
051170       END-COMPUTE                                                        
051180       MOVE SYST-IDANALYS       TO R3-LINE-ORDER                          
051190       MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                   
051200       MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                   
051210       MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                           
051220       MOVE SPACE               TO WS-ALLOCATE-DC                         
051230       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
051240       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
051250       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
051260       PERFORM S03-WRITE-W51072                                           
051270     END-EVALUATE                                                         
051280     .                                                                    
051290     EJECT                                                                
051300                                                                          
051310 CEJ305-SUB-EVENT-303-305 SECTION.                                        
051320     EVALUATE IN-EKH-KDEKNIVA                                             
051330     WHEN 'DET'                                                           
051340       IF SYST-IDSEKVNR = 1                                               
051350         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
051360         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
051370         IF BET-KDTRADP(3:2) NOT = SPACE                                  
051380           MOVE '1'               TO WS-ACCOUNT-4                         
051390         ELSE                                                             
051400           MOVE '3'               TO WS-ACCOUNT-4                         
051410         END-IF                                                           
051420         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
051430*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
051440         IF GMT-KDKUNDKAT = 18                                            
051450           MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                   
051460           MOVE '13'              TO WS-ACCOUNT-RETURER-5-6               
051470           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
051480         END-IF                                                           
051490***                                                                       
051500         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
051510         COMPUTE R3-LINE-AMOUNT-LC =                                      
051520                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
051530         END-COMPUTE                                                      
051540         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
051550         MOVE SPACE               TO WS-ALLOCATE-DC                       
051560         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
051570         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
051580         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
051590         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
051600         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
051610         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
051620         PERFORM S03-WRITE-W51072                                         
051630       END-IF                                                             
051640                                                                          
051650       IF SYST-IDSEKVNR = 2                                               
051660* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
051670*        PERFORM S32-CHECK-POS-NEG                                        
051680*        IF WS-LINE-AMOUNT > 0                                            
051690           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
051700           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
051710           IF BET-KDTRADP(3:2) NOT = SPACE                                
051720             MOVE '1'               TO WS-ACCOUNT-4                       
051730           ELSE                                                           
051740             MOVE '3'               TO WS-ACCOUNT-4                       
051750           END-IF                                                         
051760           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
051770*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
051780           IF GMT-KDKUNDKAT = 18                                          
051790             MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                 
051800             MOVE '30'              TO WS-ACCOUNT-RETURER-5-6             
051810             MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                 
051820           END-IF                                                         
051830***                                                                       
051840           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
051850           COMPUTE R3-LINE-AMOUNT-LC =                                    
051860             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
051870           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
051880           MOVE SPACE               TO WS-ALLOCATE-DC                     
051890           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
051900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
051910           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
051920           PERFORM S03-WRITE-W51072                                       
051930*        END-IF                                                           
051940       END-IF                                                             
051950                                                                          
051960       IF SYST-IDSEKVNR = 3                                               
051970         IF IN-EKH-PRHEMTAG > ZERO                                        
051980           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
051990           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
052000           COMPUTE R3-LINE-AMOUNT-LC =                                    
052010                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
052020           END-COMPUTE                                                    
052030           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
052040           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
052050           MOVE SPACE               TO WS-ALLOCATE-DC                     
052060           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
052070           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
052080           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
052090           PERFORM S03-WRITE-W51072                                       
052100         END-IF                                                           
052110       END-IF                                                             
052120                                                                          
052130       IF SYST-IDSEKVNR = 4                                               
052140         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
052150         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
052160         IF BET-KDTRADP(3:2) NOT = SPACE                                  
052170           MOVE '1'               TO WS-ACCOUNT-4                         
052180         ELSE                                                             
052190           MOVE '3'               TO WS-ACCOUNT-4                         
052200         END-IF                                                           
052210         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
052220***** ECOM CREDIT                                                         
052230         IF GMT-KDKUNDKAT = 18                                            
052240           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT-VERS2                     
052250           MOVE '2'               TO WS-ACCOUNT-6                         
052260           MOVE WS-ACCOUNT-VERS2  TO WS-R3-ACCOUNT-10                     
052270         END-IF                                                           
052280*****                                                                     
052290         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
052300         COMPUTE R3-LINE-AMOUNT-LC =                                      
052310                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
052320                                   IN-EKH-PRINK)                          
052330         END-COMPUTE                                                      
052340         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
052350         MOVE SPACE               TO WS-ALLOCATE-DC                       
052360         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
052370         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
052380         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
052390         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
052400           PERFORM S03-WRITE-W51072                                       
052410         END-IF                                                           
052420       END-IF                                                             
052430                                                                          
052440       IF SYST-IDSEKVNR = 5                                               
052450         IF IN-EKH-PRHEMTAG > ZERO                                        
052460           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
052470           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
052480           IF BET-KDTRADP(3:2) NOT = SPACE                                
052490             MOVE '1'               TO WS-ACCOUNT-4                       
052500           ELSE                                                           
052510             MOVE '3'               TO WS-ACCOUNT-4                       
052520           END-IF                                                         
052530           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
052540           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
052550           COMPUTE R3-LINE-AMOUNT-LC =                                    
052560                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
052570           END-COMPUTE                                                    
052580           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
052590           MOVE SPACE               TO WS-ALLOCATE-DC                     
052600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
052610           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
052620           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
052630           PERFORM S03-WRITE-W51072                                       
052640         END-IF                                                           
052650       END-IF                                                             
052660                                                                          
052670       IF SYST-IDSEKVNR = 6                                               
052680         IF IN-EKH-PRHEMTAG > ZERO                                        
052690           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
052700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
052710           COMPUTE R3-LINE-AMOUNT-LC =                                    
052720                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
052730           END-COMPUTE                                                    
052740           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
052750           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
052760           MOVE SPACE               TO WS-ALLOCATE-DC                     
052770           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
052780           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
052790           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
052800           PERFORM S03-WRITE-W51072                                       
052810         END-IF                                                           
052820       END-IF                                                             
052830                                                                          
052840       IF SYST-IDSEKVNR = 7                                               
052850         IF IN-EKH-PRLANDCO NOT = ZERO                                    
052860           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
052870           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
052880           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
052890           IF IN-EKH-KDANMORS NOT = '30'                                  
052900              MOVE IDKST-57510      TO WS-RED-IDKST                       
052910           ELSE                                                           
052920              MOVE SPACES           TO WS-RED-IDKST                       
052930           END-IF                                                         
052940           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
052950                                                                          
052960           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
052970           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
052980           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
052990           MOVE SPACE               TO WS-ALLOCATE-DC                     
053000           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
053010           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
053020           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
053030           PERFORM S03-WRITE-W51072                                       
053040         END-IF                                                           
053050       END-IF                                                             
053060                                                                          
053070                                                                          
053080       IF SYST-IDSEKVNR = 10                                              
053090         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
053100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
053110         COMPUTE R3-LINE-AMOUNT-LC =                                      
053120                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
053130         END-COMPUTE                                                      
053140***** ECOM CREDIT                                                         
053150         IF GMT-KDKUNDKAT = 18                                            
053160           MOVE '158600002791'    TO R3-LINE-ORDER                        
053170         ELSE                                                             
053180           MOVE SYST-IDANALYS     TO R3-LINE-ORDER                        
053190         END-IF                                                           
053200         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
053210         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
053220         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
053230         MOVE SPACE               TO WS-ALLOCATE-DC                       
053240         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
053250         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
053260         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
053270         PERFORM S03-WRITE-W51072                                         
053280       END-IF                                                             
053290                                                                          
053300     END-EVALUATE                                                         
053310     .                                                                    
053320     EJECT                                                                
053330                                                                          
053340 CEJ306-SUB-EVENT-303-306 SECTION.                                        
053350     EVALUATE IN-EKH-KDEKNIVA                                             
053360     WHEN 'DET'                                                           
053370       IF SYST-IDSEKVNR = 1                                               
053380         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
053390         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
053400         IF BET-KDTRADP(3:2) NOT = SPACE                                  
053410           MOVE '1'               TO WS-ACCOUNT-4                         
053420         ELSE                                                             
053430           MOVE '3'               TO WS-ACCOUNT-4                         
053440         END-IF                                                           
053450         MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                   
053460         IF IN-EKH-KDANMORS = '98'                                        
053470           MOVE '07'              TO WS-ACCOUNT-RETURER-5-6               
053480         ELSE                                                             
053490           MOVE '02'              TO WS-ACCOUNT-RETURER-5-6               
053500         END-IF                                                           
053510         MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                     
053520         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
053530         COMPUTE R3-LINE-AMOUNT-LC =                                      
053540                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
053550         END-COMPUTE                                                      
053560         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
053570         MOVE SPACE               TO WS-ALLOCATE-DC                       
053580         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
053590         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
053600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
053610         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
053620         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
053630         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
053640         PERFORM S03-WRITE-W51072                                         
053650       END-IF                                                             
053660                                                                          
053670       IF SYST-IDSEKVNR = 2                                               
053680* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
053690*        PERFORM S32-CHECK-POS-NEG                                        
053700*        IF WS-LINE-AMOUNT > 0                                            
053710           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
053720           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
053730           IF BET-KDTRADP(3:2) NOT = SPACE                                
053740             MOVE '1'               TO WS-ACCOUNT-4                       
053750           ELSE                                                           
053760             MOVE '3'               TO WS-ACCOUNT-4                       
053770           END-IF                                                         
053780           MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                 
053790           IF IN-EKH-KDANMORS = '98'                                      
053800             MOVE '27'              TO WS-ACCOUNT-RETURER-5-6             
053810           ELSE                                                           
053820             MOVE '22'              TO WS-ACCOUNT-RETURER-5-6             
053830           END-IF                                                         
053840           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
053850           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
053860*          MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
053870           COMPUTE R3-LINE-AMOUNT-LC =                                    
053880             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
053890           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
053900           MOVE SPACE               TO WS-ALLOCATE-DC                     
053910           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
053920           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
053930           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
053940           PERFORM S03-WRITE-W51072                                       
053950*        END-IF                                                           
053960       END-IF                                                             
053970                                                                          
053980       IF SYST-IDSEKVNR = 3                                               
053990         IF IN-EKH-PRHEMTAG > ZERO                                        
054000           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
054010           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
054020           COMPUTE R3-LINE-AMOUNT-LC =                                    
054030                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
054040           END-COMPUTE                                                    
054050           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
054060           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
054070           MOVE SPACE               TO WS-ALLOCATE-DC                     
054080           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
054090           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
054100           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
054110           PERFORM S03-WRITE-W51072                                       
054120         END-IF                                                           
054130       END-IF                                                             
054140                                                                          
054150       IF SYST-IDSEKVNR = 4                                               
054160         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
054170         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
054180         IF BET-KDTRADP(3:2) NOT = SPACE                                  
054190           MOVE '1'               TO WS-ACCOUNT-4                         
054200         ELSE                                                             
054210           MOVE '3'               TO WS-ACCOUNT-4                         
054220         END-IF                                                           
054230         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
054240         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
054250         COMPUTE R3-LINE-AMOUNT-LC =                                      
054260                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
054270                                   IN-EKH-PRINK)                          
054280         END-COMPUTE                                                      
054290         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
054300         MOVE SPACE               TO WS-ALLOCATE-DC                       
054310         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
054320         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
054330         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
054340         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
054350           PERFORM S03-WRITE-W51072                                       
054360         END-IF                                                           
054370       END-IF                                                             
054380                                                                          
054390       IF SYST-IDSEKVNR = 5                                               
054400         IF IN-EKH-PRHEMTAG > ZERO                                        
054410           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
054420           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
054430           IF BET-KDTRADP(3:2) NOT = SPACE                                
054440             MOVE '1'               TO WS-ACCOUNT-4                       
054450           ELSE                                                           
054460             MOVE '3'               TO WS-ACCOUNT-4                       
054470           END-IF                                                         
054480           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
054490           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
054500           COMPUTE R3-LINE-AMOUNT-LC =                                    
054510                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
054520           END-COMPUTE                                                    
054530           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
054540           MOVE SPACE               TO WS-ALLOCATE-DC                     
054550           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
054560           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
054570           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
054580           PERFORM S03-WRITE-W51072                                       
054590         END-IF                                                           
054600       END-IF                                                             
054610                                                                          
054620       IF SYST-IDSEKVNR = 6                                               
054630         IF IN-EKH-PRHEMTAG > ZERO                                        
054640           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
054650           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
054660           COMPUTE R3-LINE-AMOUNT-LC =                                    
054670                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
054680           END-COMPUTE                                                    
054690           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
054700           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
054710           MOVE SPACE               TO WS-ALLOCATE-DC                     
054720           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
054730           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
054740           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
054750           PERFORM S03-WRITE-W51072                                       
054760         END-IF                                                           
054770       END-IF                                                             
054780                                                                          
054790       IF SYST-IDSEKVNR = 7                                               
054800         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
054810         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
054820         COMPUTE R3-LINE-AMOUNT-LC =                                      
054830*                IN-EKH-KVANTAL * IN-EKH-PRARTSJK                         
054840                 IN-EKH-KVANTAL * IN-EKH-PRINK                            
054850         END-COMPUTE                                                      
054860         PERFORM S11-ANALYSIS                                             
054870         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
054880         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
054890         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
054900         MOVE SPACE               TO WS-ALLOCATE-DC                       
054910         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
054920         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
054930         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
054940         PERFORM S03-WRITE-W51072                                         
054950       END-IF                                                             
054960                                                                          
054970     END-EVALUATE                                                         
054980     .                                                                    
054990     EJECT                                                                
055000                                                                          
055010 CEJ307-SUB-EVENT-303-307 SECTION.                                        
055020     EVALUATE IN-EKH-KDEKNIVA                                             
055030     WHEN 'DET'                                                           
055040       IF SYST-IDSEKVNR = 1                                               
055050         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
055060         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
055070         IF BET-KDTRADP(3:2) NOT = SPACE                                  
055080           MOVE '1'               TO WS-ACCOUNT-4                         
055090         ELSE                                                             
055100           MOVE '3'               TO WS-ACCOUNT-4                         
055110         END-IF                                                           
055120         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
055130*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
055140         IF GMT-KDKUNDKAT = 18                                            
055150           MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                   
055160           MOVE '13'              TO WS-ACCOUNT-RETURER-5-6               
055170           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
055180         END-IF                                                           
055190***                                                                       
055200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
055210         COMPUTE R3-LINE-AMOUNT-LC =                                      
055220                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
055230         END-COMPUTE                                                      
055240         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
055250         MOVE SPACE               TO WS-ALLOCATE-DC                       
055260         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
055270         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
055280         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
055290         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
055300         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
055310         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
055320         IF KDPRODSL-LYNK                                                 
055330           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
055340           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
055350           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
055360           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
055370         END-IF                                                           
055380         PERFORM S03-WRITE-W51072                                         
055390       END-IF                                                             
055400                                                                          
055410       IF SYST-IDSEKVNR = 2                                               
055420* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
055430*        PERFORM S32-CHECK-POS-NEG                                        
055440*        IF WS-LINE-AMOUNT > 0                                            
055450           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
055460           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
055470           IF BET-KDTRADP(3:2) NOT = SPACE                                
055480             MOVE '1'               TO WS-ACCOUNT-4                       
055490           ELSE                                                           
055500             MOVE '3'               TO WS-ACCOUNT-4                       
055510           END-IF                                                         
055520           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
055530*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
055540           IF GMT-KDKUNDKAT = 18                                          
055550             MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                 
055560             MOVE '30'              TO WS-ACCOUNT-RETURER-5-6             
055570             MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                 
055580           END-IF                                                         
055590***                                                                       
055600           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
055610*          MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
055620           COMPUTE R3-LINE-AMOUNT-LC =                                    
055630             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
055640           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
055650           MOVE SPACE               TO WS-ALLOCATE-DC                     
055660           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
055670           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
055680           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
055690           IF KDPRODSL-LYNK                                               
055700             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
055710             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
055720             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
055730             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
055740           END-IF                                                         
055750           PERFORM S03-WRITE-W51072                                       
055760*        END-IF                                                           
055770       END-IF                                                             
055780                                                                          
055790       IF SYST-IDSEKVNR = 3                                               
055800         IF IN-EKH-PRHEMTAG > ZERO                                        
055810           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
055820           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
055830           COMPUTE R3-LINE-AMOUNT-LC =                                    
055840                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
055850           END-COMPUTE                                                    
055860           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
055870           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
055880           MOVE SPACE               TO WS-ALLOCATE-DC                     
055890           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
055900           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
055910           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
055920           PERFORM S03-WRITE-W51072                                       
055930         END-IF                                                           
055940       END-IF                                                             
055950                                                                          
055960       IF SYST-IDSEKVNR = 4                                               
055970         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
055980         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
055990         IF BET-KDTRADP(3:2) NOT = SPACE                                  
056000           MOVE '1'               TO WS-ACCOUNT-4                         
056010         ELSE                                                             
056020           MOVE '3'               TO WS-ACCOUNT-4                         
056030         END-IF                                                           
056040         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
056050***** ECOM CREDIT                                                         
056060         IF GMT-KDKUNDKAT = 18                                            
056070           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT-VERS2                     
056080           MOVE '2'               TO WS-ACCOUNT-6                         
056090           MOVE WS-ACCOUNT-VERS2  TO WS-R3-ACCOUNT-10                     
056100         END-IF                                                           
056110*****                                                                     
056120         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
056130         COMPUTE R3-LINE-AMOUNT-LC =                                      
056140                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
056150                                   IN-EKH-PRINK)                          
056160         END-COMPUTE                                                      
056170         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
056180         MOVE SPACE               TO WS-ALLOCATE-DC                       
056190         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
056200         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
056210         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
056220         IF KDPRODSL-LYNK                                                 
056230           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
056240           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
056250           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
056260           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
056270         END-IF                                                           
056280         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
056290           PERFORM S03-WRITE-W51072                                       
056300         END-IF                                                           
056310       END-IF                                                             
056320                                                                          
056330       IF SYST-IDSEKVNR = 5                                               
056340         IF IN-EKH-PRHEMTAG > ZERO                                        
056350           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
056360           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
056370           IF BET-KDTRADP(3:2) NOT = SPACE                                
056380             MOVE '1'               TO WS-ACCOUNT-4                       
056390           ELSE                                                           
056400             MOVE '3'               TO WS-ACCOUNT-4                       
056410           END-IF                                                         
056420           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
056430           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
056440           COMPUTE R3-LINE-AMOUNT-LC =                                    
056450                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
056460           END-COMPUTE                                                    
056470           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
056480           MOVE SPACE               TO WS-ALLOCATE-DC                     
056490           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
056500           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
056510           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
056520           PERFORM S03-WRITE-W51072                                       
056530         END-IF                                                           
056540       END-IF                                                             
056550                                                                          
056560       IF SYST-IDSEKVNR = 6                                               
056570         IF IN-EKH-PRHEMTAG > ZERO                                        
056580           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
056590           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
056600           COMPUTE R3-LINE-AMOUNT-LC =                                    
056610                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
056620           END-COMPUTE                                                    
056630           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
056640           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
056650           MOVE SPACE               TO WS-ALLOCATE-DC                     
056660           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
056670           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
056680           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
056690           PERFORM S03-WRITE-W51072                                       
056700         END-IF                                                           
056710       END-IF                                                             
056720                                                                          
056730       IF SYST-IDSEKVNR = 9                                               
056740         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
056750         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
056760         COMPUTE R3-LINE-AMOUNT-LC =                                      
056770                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
056780         END-COMPUTE                                                      
056790         PERFORM S11-ANALYSIS                                             
056800***** ECOM CREDIT                                                         
056810         IF GMT-KDKUNDKAT = 18                                            
056820           MOVE '158600002791'    TO R3-LINE-ORDER                        
056830         END-IF                                                           
056840         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
056850         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
056860         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
056870         MOVE SPACE               TO WS-ALLOCATE-DC                       
056880         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
056890         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
056900         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
056910         IF KDPRODSL-LYNK                                                 
056920           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
056930           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
056940           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
056950           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
056960         END-IF                                                           
056970         PERFORM S03-WRITE-W51072                                         
056980       END-IF                                                             
056990                                                                          
057000       IF SYST-IDSEKVNR = 10                                              
057010         IF IN-EKH-PRLANDCO NOT = ZERO                                    
057020           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
057030           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
057040           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
057050           IF IN-EKH-KDANMORS NOT = '30'                                  
057060              MOVE IDKST-57510      TO WS-RED-IDKST                       
057070           ELSE                                                           
057080              MOVE SPACES           TO WS-RED-IDKST                       
057090           END-IF                                                         
057100           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
057110           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
057120           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
057130           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
057140           MOVE SPACE               TO WS-ALLOCATE-DC                     
057150           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
057160           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
057170           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
057180***** ECOM CREDIT                                                         
057190           IF GMT-KDKUNDKAT = 18                                          
057200             MOVE '158600002791'    TO R3-LINE-ORDER                      
057210           END-IF                                                         
057220           PERFORM S03-WRITE-W51072                                       
057230         END-IF                                                           
057240       END-IF                                                             
057250                                                                          
057260     END-EVALUATE                                                         
057270     .                                                                    
057280     EJECT                                                                
057290                                                                          
057300 CEJ308-SUB-EVENT-303-308 SECTION.                                        
057310     EVALUATE IN-EKH-KDEKNIVA                                             
057320     WHEN 'DET'                                                           
057330       IF SYST-IDSEKVNR = 1                                               
057340         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
057350         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
057360         IF BET-KDTRADP(3:2) NOT = SPACE                                  
057370           MOVE '1'               TO WS-ACCOUNT-4                         
057380         ELSE                                                             
057390           MOVE '3'               TO WS-ACCOUNT-4                         
057400         END-IF                                                           
057410         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
057420         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
057430         COMPUTE R3-LINE-AMOUNT-LC =                                      
057440                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
057450         END-COMPUTE                                                      
057460         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
057470         MOVE SPACE               TO WS-ALLOCATE-DC                       
057480         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
057490         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
057500         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
057510         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
057520         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
057530         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
057540         PERFORM S03-WRITE-W51072                                         
057550       END-IF                                                             
057560                                                                          
057570       IF SYST-IDSEKVNR = 2                                               
057580* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
057590*        PERFORM S32-CHECK-POS-NEG                                        
057600*        IF WS-LINE-AMOUNT > 0                                            
057610           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
057620           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
057630           IF BET-KDTRADP(3:2) NOT = SPACE                                
057640             MOVE '1'               TO WS-ACCOUNT-4                       
057650           ELSE                                                           
057660             MOVE '3'               TO WS-ACCOUNT-4                       
057670           END-IF                                                         
057680           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
057690           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
057700*          MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
057710           COMPUTE R3-LINE-AMOUNT-LC =                                    
057720             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
057730           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
057740           MOVE SPACE               TO WS-ALLOCATE-DC                     
057750           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
057760           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
057770           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
057780           PERFORM S03-WRITE-W51072                                       
057790*        END-IF                                                           
057800       END-IF                                                             
057810                                                                          
057820       IF SYST-IDSEKVNR = 3                                               
057830         IF IN-EKH-PRHEMTAG > ZERO                                        
057840           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
057850           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
057860           COMPUTE R3-LINE-AMOUNT-LC =                                    
057870                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
057880           END-COMPUTE                                                    
057890           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
057900           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
057910           MOVE SPACE               TO WS-ALLOCATE-DC                     
057920           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
057930           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
057940           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
057950           PERFORM S03-WRITE-W51072                                       
057960         END-IF                                                           
057970       END-IF                                                             
057980                                                                          
057990       IF SYST-IDSEKVNR = 4                                               
058000         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
058010         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
058020         IF BET-KDTRADP(3:2) NOT = SPACE                                  
058030           MOVE '1'               TO WS-ACCOUNT-4                         
058040         ELSE                                                             
058050           MOVE '3'               TO WS-ACCOUNT-4                         
058060         END-IF                                                           
058070         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
058080         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
058090         COMPUTE R3-LINE-AMOUNT-LC =                                      
058100                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
058110                                   IN-EKH-PRINK)                          
058120         END-COMPUTE                                                      
058130         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
058140         MOVE SPACE               TO WS-ALLOCATE-DC                       
058150         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
058160         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
058170         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
058180         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
058190           PERFORM S03-WRITE-W51072                                       
058200         END-IF                                                           
058210       END-IF                                                             
058220                                                                          
058230       IF SYST-IDSEKVNR = 5                                               
058240         IF IN-EKH-PRHEMTAG > ZERO                                        
058250           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
058260           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
058270           IF BET-KDTRADP(3:2) NOT = SPACE                                
058280             MOVE '1'               TO WS-ACCOUNT-4                       
058290           ELSE                                                           
058300             MOVE '3'               TO WS-ACCOUNT-4                       
058310           END-IF                                                         
058320           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
058330           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
058340           COMPUTE R3-LINE-AMOUNT-LC =                                    
058350                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
058360           END-COMPUTE                                                    
058370           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
058380           MOVE SPACE               TO WS-ALLOCATE-DC                     
058390           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
058400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
058410           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
058420           PERFORM S03-WRITE-W51072                                       
058430         END-IF                                                           
058440       END-IF                                                             
058450                                                                          
058460       IF SYST-IDSEKVNR = 6                                               
058470         IF IN-EKH-PRHEMTAG > ZERO                                        
058480           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
058490           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
058500           COMPUTE R3-LINE-AMOUNT-LC =                                    
058510                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
058520           END-COMPUTE                                                    
058530           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
058540           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
058550           MOVE SPACE               TO WS-ALLOCATE-DC                     
058560           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
058570           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
058580           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
058590           PERFORM S03-WRITE-W51072                                       
058600         END-IF                                                           
058610       END-IF                                                             
058620                                                                          
058630                                                                          
058640       IF SYST-IDSEKVNR = 9                                               
058650         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
058660         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
058670         COMPUTE R3-LINE-AMOUNT-LC =                                      
058680                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
058690         END-COMPUTE                                                      
058700         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
058710         MOVE IN-EKH-IDKST        TO WS-RED-IDKST                         
058720         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
058730         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
058740         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
058750         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
058760         MOVE SPACE               TO WS-ALLOCATE-DC                       
058770         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
058780         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
058790         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
058800         PERFORM S03-WRITE-W51072                                         
058810       END-IF                                                             
058820                                                                          
058830       IF SYST-IDSEKVNR = 10                                              
058840         IF IN-EKH-PRLANDCO NOT = ZERO                                    
058850           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
058860           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
058870           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
058880           IF IN-EKH-KDANMORS NOT = '30'                                  
058890              MOVE IDKST-57510      TO WS-RED-IDKST                       
058900           ELSE                                                           
058910              MOVE SPACES           TO WS-RED-IDKST                       
058920           END-IF                                                         
058930           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
058940           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
058950           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
058960           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
058970           MOVE SPACE               TO WS-ALLOCATE-DC                     
058980           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
058990           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
059000           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
059010           PERFORM S03-WRITE-W51072                                       
059020         END-IF                                                           
059030       END-IF                                                             
059040                                                                          
059050     END-EVALUATE                                                         
059060     .                                                                    
059070     EJECT                                                                
059080                                                                          
059090 CEJ309-SUB-EVENT-303-309 SECTION.                                        
059100     EVALUATE IN-EKH-KDEKNIVA                                             
059110     WHEN 'DET'                                                           
059120       IF SYST-IDSEKVNR = 1                                               
059130         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
059140         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
059150         IF BET-KDTRADP(3:2) NOT = SPACE                                  
059160           MOVE '1'               TO WS-ACCOUNT-4                         
059170         ELSE                                                             
059180           MOVE '3'               TO WS-ACCOUNT-4                         
059190         END-IF                                                           
059200         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
059210*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
059220         IF GMT-KDKUNDKAT = 18                                            
059230           MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                   
059240           MOVE '13'              TO WS-ACCOUNT-RETURER-5-6               
059250           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
059260         END-IF                                                           
059270***                                                                       
059280         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
059290         COMPUTE R3-LINE-AMOUNT-LC =                                      
059300                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
059310         END-COMPUTE                                                      
059320         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
059330         MOVE SPACE               TO WS-ALLOCATE-DC                       
059340         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
059350         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
059360         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
059370         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
059380         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
059390         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
059400         IF KDPRODSL-LYNK                                                 
059410           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
059420           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
059430           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
059440           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
059450         END-IF                                                           
059460         PERFORM S03-WRITE-W51072                                         
059470       END-IF                                                             
059480                                                                          
059490       IF SYST-IDSEKVNR = 2                                               
059500* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
059510*        PERFORM S32-CHECK-POS-NEG                                        
059520*        IF WS-LINE-AMOUNT > 0                                            
059530           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
059540           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
059550           IF BET-KDTRADP(3:2) NOT = SPACE                                
059560             MOVE '1'               TO WS-ACCOUNT-4                       
059570           ELSE                                                           
059580             MOVE '3'               TO WS-ACCOUNT-4                       
059590           END-IF                                                         
059600           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
059610*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
059620           IF GMT-KDKUNDKAT = 18                                          
059630             MOVE WS-ACCOUNT        TO WS-ACCOUNT-RETURER                 
059640             MOVE '30'              TO WS-ACCOUNT-RETURER-5-6             
059650             MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                 
059660           END-IF                                                         
059670***                                                                       
059680           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
059690*          MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
059700           COMPUTE R3-LINE-AMOUNT-LC =                                    
059710             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
059720           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
059730           MOVE SPACE               TO WS-ALLOCATE-DC                     
059740           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
059750           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
059760           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
059770           IF KDPRODSL-LYNK                                               
059780             MOVE 'LYNK'          TO WS-PRCTR(1:4)                        
059790             MOVE IN-EKH-KDPRODSL TO WS-PRCTR-PRODSL-DISP                 
059800             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
059810             MOVE WS-PRCTR        TO R3-LINE-PROFIT-CENTER                
059820           END-IF                                                         
059830           PERFORM S03-WRITE-W51072                                       
059840*        END-IF                                                           
059850       END-IF                                                             
059860                                                                          
059870       IF SYST-IDSEKVNR = 3                                               
059880         IF IN-EKH-PRHEMTAG > ZERO                                        
059890           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
059900           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
059910           COMPUTE R3-LINE-AMOUNT-LC =                                    
059920                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
059930           END-COMPUTE                                                    
059940           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
059950           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
059960           MOVE SPACE               TO WS-ALLOCATE-DC                     
059970           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
059980           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
059990           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
060000           PERFORM S03-WRITE-W51072                                       
060010         END-IF                                                           
060020       END-IF                                                             
060030                                                                          
060040       IF SYST-IDSEKVNR = 4                                               
060050         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
060060         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
060070         IF BET-KDTRADP(3:2) NOT = SPACE                                  
060080           MOVE '1'               TO WS-ACCOUNT-4                         
060090         ELSE                                                             
060100           MOVE '3'               TO WS-ACCOUNT-4                         
060110         END-IF                                                           
060120         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
060130***** ECOM CREDIT                                                         
060140         IF GMT-KDKUNDKAT = 18                                            
060150           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT-VERS2                     
060160           MOVE '2'               TO WS-ACCOUNT-6                         
060170           MOVE WS-ACCOUNT-VERS2  TO WS-R3-ACCOUNT-10                     
060180         END-IF                                                           
060190*****                                                                     
060200         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
060210         COMPUTE R3-LINE-AMOUNT-LC =                                      
060220                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
060230                                   IN-EKH-PRINK)                          
060240         END-COMPUTE                                                      
060250         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
060260         MOVE SPACE               TO WS-ALLOCATE-DC                       
060270         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
060280         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
060290         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
060300         IF KDPRODSL-LYNK                                                 
060310           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
060320           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
060330           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
060340           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
060350         END-IF                                                           
060360         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
060370           PERFORM S03-WRITE-W51072                                       
060380         END-IF                                                           
060390       END-IF                                                             
060400                                                                          
060410       IF SYST-IDSEKVNR = 5                                               
060420         IF IN-EKH-PRHEMTAG > ZERO                                        
060430           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
060440           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
060450           IF BET-KDTRADP(3:2) NOT = SPACE                                
060460             MOVE '1'               TO WS-ACCOUNT-4                       
060470           ELSE                                                           
060480             MOVE '3'               TO WS-ACCOUNT-4                       
060490           END-IF                                                         
060500           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
060510           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
060520           COMPUTE R3-LINE-AMOUNT-LC =                                    
060530                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
060540           END-COMPUTE                                                    
060550           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
060560           MOVE SPACE               TO WS-ALLOCATE-DC                     
060570           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
060580           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
060590           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
060600           PERFORM S03-WRITE-W51072                                       
060610         END-IF                                                           
060620       END-IF                                                             
060630                                                                          
060640       IF SYST-IDSEKVNR = 6                                               
060650         IF IN-EKH-PRHEMTAG > ZERO                                        
060660           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
060670           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
060680           COMPUTE R3-LINE-AMOUNT-LC =                                    
060690                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
060700           END-COMPUTE                                                    
060710           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
060720           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
060730           MOVE SPACE               TO WS-ALLOCATE-DC                     
060740           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
060750           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
060760           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
060770           PERFORM S03-WRITE-W51072                                       
060780         END-IF                                                           
060790       END-IF                                                             
060800                                                                          
060810       IF SYST-IDSEKVNR = 9                                               
060820         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
060830         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
060840         COMPUTE R3-LINE-AMOUNT-LC =                                      
060850                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
060860         END-COMPUTE                                                      
060870         PERFORM S11-ANALYSIS                                             
060880***** ECOM CREDIT                                                         
060890         IF GMT-KDKUNDKAT = 18                                            
060900           MOVE '158600002791'    TO R3-LINE-ORDER                        
060910         END-IF                                                           
060920         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
060930         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
060940         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
060950         MOVE SPACE               TO WS-ALLOCATE-DC                       
060960         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
060970         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
060980         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
060990         IF KDPRODSL-LYNK                                                 
061000           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
061010           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
061020           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
061030           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
061040         END-IF                                                           
061050         PERFORM S03-WRITE-W51072                                         
061060       END-IF                                                             
061070                                                                          
061080       IF SYST-IDSEKVNR = 10                                              
061090         IF IN-EKH-PRLANDCO NOT = ZERO                                    
061100           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
061110           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
061120           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
061130           IF IN-EKH-KDANMORS NOT = '30'                                  
061140              MOVE IDKST-57510      TO WS-RED-IDKST                       
061150           ELSE                                                           
061160              MOVE SPACES           TO WS-RED-IDKST                       
061170           END-IF                                                         
061180           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
061190           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
061200           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
061210           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
061220           MOVE SPACE               TO WS-ALLOCATE-DC                     
061230           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
061240           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
061250           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
061260           PERFORM S03-WRITE-W51072                                       
061270         END-IF                                                           
061280       END-IF                                                             
061290                                                                          
061300     END-EVALUATE                                                         
061310     .                                                                    
061320     EJECT                                                                
061330                                                                          
061340 CEJ310-SUB-EVENT-303-310 SECTION.                                        
061350     EVALUATE IN-EKH-KDEKNIVA                                             
061360     WHEN 'DET'                                                           
061370       IF SYST-IDSEKVNR = 1                                               
061380         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
061390         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
061400         IF KDPRODSL-LYNK                                                 
061410           MOVE '1'               TO WS-ACCOUNT-5                         
061420           MOVE '0'               TO WS-ACCOUNT-5A                        
061430           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
061440           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
061450           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
061460           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
061470           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
061480         END-IF                                                           
061490         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
061500         COMPUTE R3-LINE-AMOUNT-LC =                                      
061510                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
061520         END-COMPUTE                                                      
061530         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
061540         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
061550         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
061560         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
061570         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
061580         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
061590         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
061600         PERFORM S02-WRITE-W51071A                                        
061610       END-IF                                                             
061620                                                                          
061630       IF SYST-IDSEKVNR = 2                                               
061640         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
061650         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
061660         COMPUTE R3-LINE-AMOUNT-LC =                                      
061670                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
061680         END-COMPUTE                                                      
061690         PERFORM S11-ANALYSIS                                             
061700***** ECOM CREDIT                                                         
061710         IF GMT-KDKUNDKAT = 18                                            
061720           MOVE '158600002791'    TO R3-LINE-ORDER                        
061730         END-IF                                                           
061740         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
061750         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
061760         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
061770         MOVE SPACE               TO WS-ALLOCATE-DC                       
061780         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
061790         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
061800         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
061810         PERFORM S02-WRITE-W51071A                                        
061820       END-IF                                                             
061830     END-EVALUATE                                                         
061840     .                                                                    
061850     EJECT                                                                
061860                                                                          
061870 CEJ311-SUB-EVENT-303-311 SECTION.                                        
061880     EVALUATE IN-EKH-KDEKNIVA                                             
061890     WHEN 'DET'                                                           
061900      IF IN-EKH-FLLSBOK  = 'Y' OR 'J'                                     
061910        IF SYST-IDSEKVNR = 1                                              
061920          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
061930          MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                          
061940          IF KDPRODSL-LYNK                                                
061950            MOVE '1'               TO WS-ACCOUNT-5                        
061960            MOVE '0'               TO WS-ACCOUNT-5A                       
061970            MOVE 'LYNK'            TO WS-PRCTR(1:4)                       
061980            MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                
061990            MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                    
062000            MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER               
062010            MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                    
062020          END-IF                                                          
062030          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
062040          COMPUTE R3-LINE-AMOUNT-LC =                                     
062050                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD                        
062060          END-COMPUTE                                                     
062070          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
062080          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
062090          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
062100          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
062110          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
062120          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
062130          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
062140          PERFORM S02-WRITE-W51071A                                       
062150        END-IF                                                            
062160                                                                          
062170        IF SYST-IDSEKVNR = 2                                              
062180          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
062190          MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                          
062200          IF BET-KDTRADP(3:2) NOT = SPACE                                 
062210            MOVE '1'               TO WS-ACCOUNT-4                        
062220          ELSE                                                            
062230            MOVE '3'               TO WS-ACCOUNT-4                        
062240          END-IF                                                          
062250          MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                    
062260          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
062270          COMPUTE R3-LINE-AMOUNT-LC =                                     
062280                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD                        
062290          END-COMPUTE                                                     
062300          PERFORM S11-ANALYSIS                                            
062310          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
062320          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
062330          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
062340          MOVE SPACE               TO WS-ALLOCATE-DC                      
062350          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
062360          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
062370          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
062380          IF KDPRODSL-LYNK                                                
062390            MOVE 'LYNK'            TO WS-PRCTR(1:4)                       
062400            MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                
062410            MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                    
062420            MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER               
062430          END-IF                                                          
062440          PERFORM S02-WRITE-W51071A                                       
062450        END-IF                                                            
062460                                                                          
062470      ELSE                                                                
062480**** VID SAKNAD RETUR, KOD 12 ELLER 22 SKER DENNA BOKNING                 
062490                                                                          
062500        IF SYST-IDSEKVNR = 3                                              
062510          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
062520          MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                          
062530          IF KDPRODSL-LYNK                                                
062540            MOVE '1'               TO WS-ACCOUNT-5                        
062550            MOVE '0'               TO WS-ACCOUNT-5A                       
062560            MOVE 'LYNK'            TO WS-PRCTR(1:4)                       
062570            MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                
062580            MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                    
062590            MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER               
062600            MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                    
062610          END-IF                                                          
062620          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
062630          COMPUTE R3-LINE-AMOUNT-LC =                                     
062640                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
062650          END-COMPUTE                                                     
062660          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
062670          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
062680          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
062690          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
062700          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
062710          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
062720          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
062730          PERFORM S02-WRITE-W51071A                                       
062740        END-IF                                                            
062750                                                                          
062760        IF SYST-IDSEKVNR = 4                                              
062770          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
062780          MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                          
062790          IF BET-KDTRADP(3:2) NOT = SPACE                                 
062800            MOVE '1'               TO WS-ACCOUNT-4                        
062810          ELSE                                                            
062820            MOVE '3'               TO WS-ACCOUNT-4                        
062830          END-IF                                                          
062840          MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                    
062850          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
062860          COMPUTE R3-LINE-AMOUNT-LC =                                     
062870                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
062880          END-COMPUTE                                                     
062890          PERFORM S11-ANALYSIS                                            
062900          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
062910          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
062920          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
062930          MOVE SPACE               TO WS-ALLOCATE-DC                      
062940          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
062950          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
062960          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
062970          IF KDPRODSL-LYNK                                                
062980            MOVE 'LYNK'            TO WS-PRCTR(1:4)                       
062990            MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                
063000            MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                    
063010            MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER               
063020          END-IF                                                          
063030          PERFORM S02-WRITE-W51071A                                       
063040        END-IF                                                            
063050       END-IF                                                             
063060     END-EVALUATE                                                         
063070     .                                                                    
063080     EJECT                                                                
063090                                                                          
063100 CEJ312-SUB-EVENT-303-312 SECTION.                                        
063110     EVALUATE IN-EKH-KDEKNIVA                                             
063120     WHEN 'DET'                                                           
063130       IF SYST-IDSEKVNR = 1                                               
063140         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
063150         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
063160         IF BET-KDTRADP(3:2) NOT = SPACE                                  
063170           MOVE '1'               TO WS-ACCOUNT-4                         
063180         ELSE                                                             
063190           MOVE '3'               TO WS-ACCOUNT-4                         
063200         END-IF                                                           
063210         MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                   
063220*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
063230         IF GMT-KDKUNDKAT = 18                                            
063240           MOVE '13'              TO WS-ACCOUNT-RETURER-5-6               
063250         ELSE                                                             
063260           IF IN-EKH-KDANMORS = '98'                                      
063270             MOVE '07'            TO WS-ACCOUNT-RETURER-5-6               
063280           ELSE                                                           
063290             MOVE '02'            TO WS-ACCOUNT-RETURER-5-6               
063300           END-IF                                                         
063310         END-IF                                                           
063320         MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                     
063330         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
063340         COMPUTE R3-LINE-AMOUNT-LC =                                      
063350                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
063360         END-COMPUTE                                                      
063370         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
063380         MOVE SPACE               TO WS-ALLOCATE-DC                       
063390         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
063400         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
063410         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
063420         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
063430         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
063440         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
063450         IF KDPRODSL-LYNK                                                 
063460           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
063470           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
063480           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
063490           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
063500         END-IF                                                           
063510         PERFORM S03-WRITE-W51072                                         
063520       END-IF                                                             
063530                                                                          
063540       IF SYST-IDSEKVNR = 2                                               
063550* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
063560         PERFORM S32-CHECK-POS-NEG                                        
063570         IF WS-LINE-AMOUNT > 0                                            
063580           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
063590           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
063600           IF BET-KDTRADP(3:2) NOT = SPACE                                
063610             MOVE '1'               TO WS-ACCOUNT-4                       
063620           ELSE                                                           
063630             MOVE '3'               TO WS-ACCOUNT-4                       
063640           END-IF                                                         
063650           MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                 
063660*** ECOM CREDIT IN A SEPARATE ACCOUNT                                     
063670           IF GMT-KDKUNDKAT = 18                                          
063680             MOVE '30'              TO WS-ACCOUNT-RETURER-5-6             
063690           ELSE                                                           
063700             IF IN-EKH-KDANMORS = '98'                                    
063710               MOVE '27'            TO WS-ACCOUNT-RETURER-5-6             
063720             ELSE                                                         
063730               MOVE '22'            TO WS-ACCOUNT-RETURER-5-6             
063740             END-IF                                                       
063750           END-IF                                                         
063760           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
063770           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
063780           MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
063790           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
063800           MOVE SPACE               TO WS-ALLOCATE-DC                     
063810           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
063820           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
063830           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
063840           IF KDPRODSL-LYNK                                               
063850             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
063860             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
063870             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
063880             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
063890           END-IF                                                         
063900           PERFORM S03-WRITE-W51072                                       
063910         END-IF                                                           
063920       END-IF                                                             
063930                                                                          
063940       IF SYST-IDSEKVNR = 3                                               
063950         IF IN-EKH-PRHEMTAG > ZERO                                        
063960           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
063970           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
063980           COMPUTE R3-LINE-AMOUNT-LC =                                    
063990                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
064000           END-COMPUTE                                                    
064010           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
064020           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
064030           MOVE SPACE               TO WS-ALLOCATE-DC                     
064040           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
064050           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
064060           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
064070           PERFORM S03-WRITE-W51072                                       
064080         END-IF                                                           
064090       END-IF                                                             
064100                                                                          
064110       IF SYST-IDSEKVNR = 4                                               
064120         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
064130         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
064140         IF BET-KDTRADP(3:2) NOT = SPACE                                  
064150           MOVE '1'               TO WS-ACCOUNT-4                         
064160         ELSE                                                             
064170           MOVE '3'               TO WS-ACCOUNT-4                         
064180         END-IF                                                           
064190         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
064200***** ECOM CREDIT                                                         
064210         IF GMT-KDKUNDKAT = 18                                            
064220           MOVE WS-R3-ACCOUNT-10  TO WS-ACCOUNT-VERS2                     
064230           MOVE '2'               TO WS-ACCOUNT-6                         
064240           MOVE WS-ACCOUNT-VERS2  TO WS-R3-ACCOUNT-10                     
064250         END-IF                                                           
064260*****                                                                     
064270         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
064280         COMPUTE R3-LINE-AMOUNT-LC =                                      
064290                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
064300                                   IN-EKH-PRINK)                          
064310         END-COMPUTE                                                      
064320         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
064330         MOVE SPACE               TO WS-ALLOCATE-DC                       
064340         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
064350         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
064360         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
064370         IF KDPRODSL-LYNK                                                 
064380           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
064390           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
064400           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
064410           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
064420         END-IF                                                           
064430         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
064440           PERFORM S03-WRITE-W51072                                       
064450         END-IF                                                           
064460       END-IF                                                             
064470                                                                          
064480       IF SYST-IDSEKVNR = 5                                               
064490         IF IN-EKH-PRHEMTAG > ZERO                                        
064500           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
064510           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
064520           IF BET-KDTRADP(3:2) NOT = SPACE                                
064530             MOVE '1'               TO WS-ACCOUNT-4                       
064540           ELSE                                                           
064550             MOVE '3'               TO WS-ACCOUNT-4                       
064560           END-IF                                                         
064570           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
064580           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
064590           COMPUTE R3-LINE-AMOUNT-LC =                                    
064600                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
064610           END-COMPUTE                                                    
064620           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
064630           MOVE SPACE               TO WS-ALLOCATE-DC                     
064640           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
064650           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
064660           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
064670           PERFORM S03-WRITE-W51072                                       
064680         END-IF                                                           
064690       END-IF                                                             
064700                                                                          
064710       IF SYST-IDSEKVNR = 6                                               
064720         IF IN-EKH-PRHEMTAG > ZERO                                        
064730           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
064740           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
064750           COMPUTE R3-LINE-AMOUNT-LC =                                    
064760                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
064770           END-COMPUTE                                                    
064780           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
064790           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
064800           MOVE SPACE               TO WS-ALLOCATE-DC                     
064810           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
064820           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
064830           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
064840           PERFORM S03-WRITE-W51072                                       
064850         END-IF                                                           
064860       END-IF                                                             
064870                                                                          
064880       IF SYST-IDSEKVNR = 7                                               
064890         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
064900         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
064910         IF BET-KDTRADP(3:2) NOT = SPACE                                  
064920           MOVE '1'               TO WS-ACCOUNT-4                         
064930         ELSE                                                             
064940           MOVE '3'               TO WS-ACCOUNT-4                         
064950         END-IF                                                           
064960         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
064970         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
064980         COMPUTE R3-LINE-AMOUNT-LC =                                      
064990                 IN-EKH-KVANTAL * IN-EKH-PRARTSJK                         
065000*                IN-EKH-KVANTAL * IN-EKH-PRINK                            
065010         END-COMPUTE                                                      
065020         PERFORM S11-ANALYSIS                                             
065030         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
065040         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
065050         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
065060         MOVE SPACE               TO WS-ALLOCATE-DC                       
065070         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
065080         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
065090         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
065100         IF KDPRODSL-LYNK                                                 
065110           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
065120           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
065130           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
065140           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
065150         END-IF                                                           
065160         PERFORM S03-WRITE-W51072                                         
065170       END-IF                                                             
065180                                                                          
065190       IF SYST-IDSEKVNR = 8                                               
065200         IF IN-EKH-PRLANDCO NOT = ZERO                                    
065210           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
065220           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
065230           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
065240           IF IN-EKH-KDANMORS NOT = '30'                                  
065250              MOVE IDKST-57510      TO WS-RED-IDKST                       
065260           ELSE                                                           
065270              MOVE SPACES           TO WS-RED-IDKST                       
065280           END-IF                                                         
065290           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
065300           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
065310           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
065320           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
065330           MOVE SPACE               TO WS-ALLOCATE-DC                     
065340           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
065350           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
065360           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
065370           PERFORM S03-WRITE-W51072                                       
065380         END-IF                                                           
065390       END-IF                                                             
065400                                                                          
065410       IF SYST-IDSEKVNR = 9                                               
065420* OM SJÄLVKOST-MATERIEL BLIR NEGATIVT SKA BOKNING SKE                     
065430         PERFORM S32-CHECK-POS-NEG                                        
065440         IF WS-LINE-AMOUNT < 0                                            
065450           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
065460           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
065470           IF BET-KDTRADP(3:2) NOT = SPACE                                
065480             MOVE '1'               TO WS-ACCOUNT-4                       
065490           ELSE                                                           
065500             MOVE '3'               TO WS-ACCOUNT-4                       
065510           END-IF                                                         
065520           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
065530           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
065540           MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
065550           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
065560           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
065570           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
065580           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
065590           MOVE SPACE               TO WS-ALLOCATE-DC                     
065600           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
065610           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
065620           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
065630           IF KDPRODSL-LYNK                                               
065640           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
065650             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
065660             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
065670             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
065680           END-IF                                                         
065690           PERFORM S03-WRITE-W51072                                       
065700         END-IF                                                           
065710       END-IF                                                             
065720                                                                          
065730     END-EVALUATE                                                         
065740     .                                                                    
065750     EJECT                                                                
065760                                                                          
065770 CEJ314-SUB-EVENT-303-314 SECTION.                                        
065780     EVALUATE IN-EKH-KDEKNIVA                                             
065790     WHEN 'DET'                                                           
065800       IF SYST-IDSEKVNR = 1                                               
065810         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
065820         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
065830         IF BET-KDTRADP(3:2) NOT = SPACE                                  
065840           MOVE '1'               TO WS-ACCOUNT-4                         
065850         ELSE                                                             
065860           MOVE '3'               TO WS-ACCOUNT-4                         
065870         END-IF                                                           
065880         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
065890         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
065900         COMPUTE R3-LINE-AMOUNT-LC =                                      
065910                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
065920         END-COMPUTE                                                      
065930         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
065940         MOVE SPACE               TO WS-ALLOCATE-DC                       
065950         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
065960         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
065970         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
065980         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
065990         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
066000         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
066010         PERFORM S03-WRITE-W51072                                         
066020       END-IF                                                             
066030                                                                          
066040       IF SYST-IDSEKVNR = 2                                               
066050* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
066060*        PERFORM S32-CHECK-POS-NEG                                        
066070*        IF WS-LINE-AMOUNT > 0                                            
066080           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
066090           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
066100           IF BET-KDTRADP(3:2) NOT = SPACE                                
066110             MOVE '1'               TO WS-ACCOUNT-4                       
066120           ELSE                                                           
066130             MOVE '3'               TO WS-ACCOUNT-4                       
066140           END-IF                                                         
066150           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
066160           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
066170*          MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
066180           COMPUTE R3-LINE-AMOUNT-LC =                                    
066190             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
066200           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
066210           MOVE SPACE               TO WS-ALLOCATE-DC                     
066220           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
066230           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
066240           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
066250           PERFORM S03-WRITE-W51072                                       
066260*        END-IF                                                           
066270       END-IF                                                             
066280                                                                          
066290       IF SYST-IDSEKVNR = 3                                               
066300         IF IN-EKH-PRHEMTAG > ZERO                                        
066310           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
066320           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
066330           COMPUTE R3-LINE-AMOUNT-LC =                                    
066340                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
066350           END-COMPUTE                                                    
066360           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
066370           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
066380           MOVE SPACE               TO WS-ALLOCATE-DC                     
066390           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
066400           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
066410           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
066420           PERFORM S03-WRITE-W51072                                       
066430         END-IF                                                           
066440       END-IF                                                             
066450                                                                          
066460       IF SYST-IDSEKVNR = 4                                               
066470         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
066480         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
066490         IF BET-KDTRADP(3:2) NOT = SPACE                                  
066500           MOVE '1'               TO WS-ACCOUNT-4                         
066510         ELSE                                                             
066520           MOVE '3'               TO WS-ACCOUNT-4                         
066530         END-IF                                                           
066540         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
066550         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
066560         COMPUTE R3-LINE-AMOUNT-LC =                                      
066570                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
066580                                   IN-EKH-PRINK)                          
066590         END-COMPUTE                                                      
066600         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
066610         MOVE SPACE               TO WS-ALLOCATE-DC                       
066620         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
066630         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
066640         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
066650         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
066660           PERFORM S03-WRITE-W51072                                       
066670         END-IF                                                           
066680       END-IF                                                             
066690                                                                          
066700       IF SYST-IDSEKVNR = 5                                               
066710         IF IN-EKH-PRHEMTAG > ZERO                                        
066720           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
066730           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
066740           IF BET-KDTRADP(3:2) NOT = SPACE                                
066750             MOVE '1'               TO WS-ACCOUNT-4                       
066760           ELSE                                                           
066770             MOVE '3'               TO WS-ACCOUNT-4                       
066780           END-IF                                                         
066790           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
066800           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
066810           COMPUTE R3-LINE-AMOUNT-LC =                                    
066820                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
066830           END-COMPUTE                                                    
066840           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
066850           MOVE SPACE               TO WS-ALLOCATE-DC                     
066860           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
066870           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
066880           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
066890           PERFORM S03-WRITE-W51072                                       
066900         END-IF                                                           
066910       END-IF                                                             
066920                                                                          
066930       IF SYST-IDSEKVNR = 6                                               
066940         IF IN-EKH-PRHEMTAG > ZERO                                        
066950           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
066960           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
066970           COMPUTE R3-LINE-AMOUNT-LC =                                    
066980                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
066990           END-COMPUTE                                                    
067000           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
067010           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
067020           MOVE SPACE               TO WS-ALLOCATE-DC                     
067030           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
067040           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
067050           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
067060           PERFORM S03-WRITE-W51072                                       
067070         END-IF                                                           
067080       END-IF                                                             
067090                                                                          
067100       IF SYST-IDSEKVNR = 7                                               
067110         IF IN-EKH-PRLANDCO NOT = ZERO                                    
067120           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
067130           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
067140           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
067150                                                                          
067160           IF IN-EKH-KDANMORS NOT = '30'                                  
067170              MOVE IDKST-57510      TO WS-RED-IDKST                       
067180           ELSE                                                           
067190              MOVE SPACES           TO WS-RED-IDKST                       
067200           END-IF                                                         
067210           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
067220                                                                          
067230           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
067240           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
067250           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
067260           MOVE SPACE               TO WS-ALLOCATE-DC                     
067270           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
067280           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
067290           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
067300           PERFORM S03-WRITE-W51072                                       
067310         END-IF                                                           
067320       END-IF                                                             
067330                                                                          
067340                                                                          
067350       IF SYST-IDSEKVNR = 10                                              
067360         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
067370         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
067380         IF KDPRODSL-LYNK                                                 
067390           MOVE '1'               TO WS-ACCOUNT-5                         
067400           MOVE '0'               TO WS-ACCOUNT-5A                        
067410           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
067420           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
067430           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
067440           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
067450           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
067460         END-IF                                                           
067470         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
067480         COMPUTE R3-LINE-AMOUNT-LC =                                      
067490                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
067500         END-COMPUTE                                                      
067510         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
067520         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
067530         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
067540         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
067550         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
067560         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
067570         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
067580         PERFORM S03-WRITE-W51072                                         
067590       END-IF                                                             
067600                                                                          
067610     END-EVALUATE                                                         
067620     .                                                                    
067630     EJECT                                                                
067640                                                                          
067650 CEJ315-SUB-EVENT-303-315 SECTION.                                        
067660     EVALUATE IN-EKH-KDEKNIVA                                             
067670     WHEN 'DET'                                                           
067680       IF SYST-IDSEKVNR = 1                                               
067690         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
067700         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
067710         IF BET-KDTRADP(3:2) NOT = SPACE                                  
067720           MOVE '1'               TO WS-ACCOUNT-4                         
067730         ELSE                                                             
067740           MOVE '3'               TO WS-ACCOUNT-4                         
067750         END-IF                                                           
067760         MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                   
067770         MOVE '02'                TO WS-ACCOUNT-RETURER-5-6               
067780         MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                     
067790         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
067800         COMPUTE R3-LINE-AMOUNT-LC =                                      
067810                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
067820         END-COMPUTE                                                      
067830         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
067840         MOVE SPACE               TO WS-ALLOCATE-DC                       
067850         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
067860         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
067870         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
067880         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
067890         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
067900         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
067910         PERFORM S03-WRITE-W51072                                         
067920       END-IF                                                             
067930                                                                          
067940       IF SYST-IDSEKVNR = 2                                               
067950* OM SJÄLVKOST-MATERIEL BLIR POSITIVT SKA BOKNING SKE                     
067960*        PERFORM S32-CHECK-POS-NEG                                        
067970*        IF WS-LINE-AMOUNT > 0                                            
067980           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
067990           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
068000           IF BET-KDTRADP(3:2) NOT = SPACE                                
068010             MOVE '1'               TO WS-ACCOUNT-4                       
068020           ELSE                                                           
068030             MOVE '3'               TO WS-ACCOUNT-4                       
068040           END-IF                                                         
068050           MOVE WS-ACCOUNT          TO WS-ACCOUNT-RETURER                 
068060           MOVE '22'                TO WS-ACCOUNT-RETURER-5-6             
068070           MOVE WS-ACCOUNT-RETURER  TO WS-R3-ACCOUNT-10                   
068080           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
068090*          MOVE WS-LINE-AMOUNT      TO R3-LINE-AMOUNT-LC                  
068100           COMPUTE R3-LINE-AMOUNT-LC =                                    
068110             IN-EKH-KVANTAL * (IN-EKH-PRINK - IN-EKH-PRHEMTAG)            
068120           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
068130           MOVE SPACE               TO WS-ALLOCATE-DC                     
068140           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
068150           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
068160           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
068170           PERFORM S03-WRITE-W51072                                       
068180*        END-IF                                                           
068190       END-IF                                                             
068200                                                                          
068210       IF SYST-IDSEKVNR = 3                                               
068220         IF IN-EKH-PRHEMTAG > ZERO                                        
068230           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
068240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
068250           COMPUTE R3-LINE-AMOUNT-LC =                                    
068260                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
068270           END-COMPUTE                                                    
068280           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
068290           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
068300           MOVE SPACE               TO WS-ALLOCATE-DC                     
068310           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
068320           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
068330           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
068340           PERFORM S03-WRITE-W51072                                       
068350         END-IF                                                           
068360       END-IF                                                             
068370                                                                          
068380       IF SYST-IDSEKVNR = 4                                               
068390         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
068400         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
068410         IF BET-KDTRADP(3:2) NOT = SPACE                                  
068420           MOVE '1'               TO WS-ACCOUNT-4                         
068430         ELSE                                                             
068440           MOVE '3'               TO WS-ACCOUNT-4                         
068450         END-IF                                                           
068460         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
068470         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
068480         COMPUTE R3-LINE-AMOUNT-LC =                                      
068490                 IN-EKH-KVANTAL * (IN-EKH-PRARTSTD -                      
068500                                   IN-EKH-PRINK)                          
068510         END-COMPUTE                                                      
068520         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
068530         MOVE SPACE               TO WS-ALLOCATE-DC                       
068540         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
068550         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
068560         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
068570         IF R3-LINE-AMOUNT-LC NOT = ZERO                                  
068580           PERFORM S03-WRITE-W51072                                       
068590         END-IF                                                           
068600       END-IF                                                             
068610                                                                          
068620       IF SYST-IDSEKVNR = 5                                               
068630         IF IN-EKH-PRHEMTAG > ZERO                                        
068640           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
068650           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
068660           IF BET-KDTRADP(3:2) NOT = SPACE                                
068670             MOVE '1'               TO WS-ACCOUNT-4                       
068680           ELSE                                                           
068690             MOVE '3'               TO WS-ACCOUNT-4                       
068700           END-IF                                                         
068710           MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                   
068720           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
068730           COMPUTE R3-LINE-AMOUNT-LC =                                    
068740                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
068750           END-COMPUTE                                                    
068760           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
068770           MOVE SPACE               TO WS-ALLOCATE-DC                     
068780           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
068790           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
068800           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
068810           PERFORM S03-WRITE-W51072                                       
068820         END-IF                                                           
068830       END-IF                                                             
068840                                                                          
068850       IF SYST-IDSEKVNR = 6                                               
068860         IF IN-EKH-PRHEMTAG > ZERO                                        
068870           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
068880           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
068890           COMPUTE R3-LINE-AMOUNT-LC =                                    
068900                   IN-EKH-KVANTAL * IN-EKH-PRHEMTAG                       
068910           END-COMPUTE                                                    
068920           MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                
068930           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
068940           MOVE SPACE               TO WS-ALLOCATE-DC                     
068950           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
068960           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
068970           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
068980           PERFORM S03-WRITE-W51072                                       
068990         END-IF                                                           
069000       END-IF                                                             
069010                                                                          
069020                                                                          
069030       IF SYST-IDSEKVNR = 9                                               
069040* HÄR BOKAS EJ SOFTWARE                                                   
069050         IF IN-EKH-KDSORT NOT = 'SW'                                      
069060           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
069070           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
069080           COMPUTE R3-LINE-AMOUNT-LC =                                    
069090                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
069100           END-COMPUTE                                                    
069110           PERFORM S11-ANALYSIS                                           
069120           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
069130           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
069140           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
069150           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
069160           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
069170           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
069180           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
069190           PERFORM S03-WRITE-W51072                                       
069200         END-IF                                                           
069210       END-IF                                                             
069220                                                                          
069230       IF SYST-IDSEKVNR = 10                                              
069240         IF IN-EKH-PRLANDCO NOT = ZERO                                    
069250           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
069260           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
069270           MOVE IN-EKH-PRLANDCO     TO R3-LINE-AMOUNT-LC                  
069280           IF IN-EKH-KDANMORS NOT = '30'                                  
069290              MOVE IDKST-57510      TO WS-RED-IDKST                       
069300           ELSE                                                           
069310              MOVE SPACES           TO WS-RED-IDKST                       
069320           END-IF                                                         
069330           MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                
069340           MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT               
069350           MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT               
069360           MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                       
069370           MOVE SPACE               TO WS-ALLOCATE-DC                     
069380           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
069390           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
069400           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
069410           PERFORM S03-WRITE-W51072                                       
069420         END-IF                                                           
069430       END-IF                                                             
069440                                                                          
069450                                                                          
069460     WHEN 'AVDR'                                                          
069470     WHEN 'FÖRS'                                                          
069480     WHEN 'LEG'                                                           
069490     WHEN 'FRAKT'                                                         
069500       IF IN-EKH-KDEKNIVA = 'LEG'                                         
069510         MOVE +3                 TO KONT-KDCALL                           
069520         MOVE ZERO               TO KONT-KDFRAKT                          
069530       ELSE                                                               
069540         IF IN-EKH-KDEKNIVA = 'FRAKT'                                     
069550           MOVE +4               TO KONT-KDCALL                           
069560           MOVE IN-EKH-KDFRAKT   TO KONT-KDFRAKT                          
069570         ELSE                                                             
069580           IF IN-EKH-KDEKNIVA = 'FÖRS'                                    
069590             MOVE +5             TO KONT-KDCALL                           
069600             MOVE ZERO           TO KONT-KDFRAKT                          
069610           ELSE                                                           
069620             IF IN-EKH-KDEKNIVA = 'AVDR'                                  
069630               MOVE +6           TO KONT-KDCALL                           
069640               MOVE ZERO         TO KONT-KDFRAKT                          
069650             END-IF                                                       
069660           END-IF                                                         
069670         END-IF                                                           
069680       END-IF                                                             
069690       MOVE IN-EKH-IDDISTR      TO KONT-IDDISTR                           
069700       CALL W510KONT USING KONT-W510KONT                                  
069710       MOVE KONT-IDKONTO        TO WS-R3-ACCOUNT-10                       
069720       MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                        
069730       MOVE KONT-IDKST          TO WS-RED-IDKST                           
069740       MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                    
069750       MOVE KONT-IDANALYS       TO R3-LINE-ORDER                          
069760       MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                      
069770       ADD  R3-LINE-AMOUNT-LC   TO SPAR-SUMMA-202-204                     
069780       MOVE SPACE               TO WS-ALLOCATE-DC                         
069790       MOVE SPACE               TO WS-ALLOCATE-DISTR                      
069800       MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                        
069810       MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                       
069820       PERFORM S04-WRITE-W51073A                                          
069830                                                                          
069840     END-EVALUATE                                                         
069850     .                                                                    
069860     EJECT                                                                
069870                                                                          
069880 CEJ316-SUB-EVENT-303-316 SECTION.                                        
069890     EVALUATE IN-EKH-KDEKNIVA                                             
069900     WHEN 'DET'                                                           
069910      IF IN-EKH-FLLSBOK  = 'Y' OR 'J'                                     
069920        IF SYST-IDSEKVNR = 1                                              
069930          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
069940          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
069950          COMPUTE R3-LINE-AMOUNT-LC =                                     
069960                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD                        
069970          END-COMPUTE                                                     
069980          PERFORM S11-ANALYSIS                                            
069990          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
070000          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
070010          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
070020          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
070030          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
070040          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
070050          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
070060          PERFORM S02-WRITE-W51071A                                       
070070        END-IF                                                            
070080                                                                          
070090        IF SYST-IDSEKVNR = 2                                              
070100          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
070110          MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                          
070120          IF BET-KDTRADP(3:2) NOT = SPACE                                 
070130            MOVE '1'               TO WS-ACCOUNT-4                        
070140          ELSE                                                            
070150            MOVE '3'               TO WS-ACCOUNT-4                        
070160          END-IF                                                          
070170          MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                    
070180          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
070190          COMPUTE R3-LINE-AMOUNT-LC =                                     
070200                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD                        
070210          END-COMPUTE                                                     
070220          PERFORM S11-ANALYSIS                                            
070230          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
070240          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
070250          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
070260          MOVE SPACE               TO WS-ALLOCATE-DC                      
070270          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
070280          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
070290          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
070300          PERFORM S02-WRITE-W51071A                                       
070310        END-IF                                                            
070320                                                                          
070330      ELSE                                                                
070340**** VID SAKNAD RETUR, KOD 27 SKER DENNA BOKNING                          
070350                                                                          
070360        IF SYST-IDSEKVNR = 3                                              
070370          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
070380          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
070390          COMPUTE R3-LINE-AMOUNT-LC =                                     
070400                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
070410          END-COMPUTE                                                     
070420          PERFORM S11-ANALYSIS                                            
070430          MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                      
070440          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
070450          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
070460          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
070470          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
070480          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
070490          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
070500          PERFORM S02-WRITE-W51071A                                       
070510        END-IF                                                            
070520                                                                          
070530        IF SYST-IDSEKVNR = 4                                              
070540          MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                    
070550          MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                          
070560          IF BET-KDTRADP(3:2) NOT = SPACE                                 
070570            MOVE '1'               TO WS-ACCOUNT-4                        
070580          ELSE                                                            
070590            MOVE '3'               TO WS-ACCOUNT-4                        
070600          END-IF                                                          
070610          MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                    
070620          MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                     
070630          COMPUTE R3-LINE-AMOUNT-LC =                                     
070640                  IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                   
070650          END-COMPUTE                                                     
070660          PERFORM S11-ANALYSIS                                            
070670          MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                
070680          MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                
070690          MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                        
070700          MOVE SPACE               TO WS-ALLOCATE-DC                      
070710          MOVE SPACE               TO WS-ALLOCATE-DISTR                   
070720          MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                     
070730          MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                    
070740          PERFORM S02-WRITE-W51071A                                       
070750        END-IF                                                            
070760       END-IF                                                             
070770     END-EVALUATE                                                         
070780     EJECT                                                                
070790     .                                                                    
070800 CEJ318-SUB-EVENT-303-318 SECTION.                                        
070810     EVALUATE IN-EKH-KDEKNIVA                                             
070820     WHEN 'DET'                                                           
070830       IF SYST-IDSEKVNR = 1                                               
070840         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
070850         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
070860         COMPUTE R3-LINE-AMOUNT-LC =                                      
070870                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
070880         END-COMPUTE                                                      
070890         PERFORM S11-ANALYSIS                                             
070900         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
070910         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
070920         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
070930         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
070940         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
070950         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
070960         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
070970         PERFORM S02-WRITE-W51071A                                        
070980       END-IF                                                             
070990                                                                          
071000       IF SYST-IDSEKVNR = 2                                               
071010         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
071020         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
071030         COMPUTE R3-LINE-AMOUNT-LC =                                      
071040                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
071050         END-COMPUTE                                                      
071060         PERFORM S11-ANALYSIS                                             
071070         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
071080         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
071090         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
071100         MOVE SPACE               TO WS-ALLOCATE-DC                       
071110         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
071120         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
071130         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
071140         PERFORM S02-WRITE-W51071A                                        
071150       END-IF                                                             
071160     END-EVALUATE                                                         
071170     .                                                                    
071180 CEJ381-SUB-EVENT-303-381 SECTION.                                        
071190     EVALUATE IN-EKH-KDEKNIVA                                             
071200     WHEN 'DET'                                                           
071210       IF SYST-IDSEKVNR = 1                                               
071220         IF IN-EKH-KVANTAL > 0                                            
071230           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
071240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
071250           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
071260            IN-EKH-KVANTAL * IN-EKH-PRARTNTO                              
071270           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
071280           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
071290           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
071300           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
071310           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
071320           PERFORM S04-WRITE-W51073A                                      
071330         END-IF                                                           
071340       END-IF                                                             
071350                                                                          
071360       IF SYST-IDSEKVNR = 2                                               
071370         IF IN-EKH-KVANTAL < 0                                            
071380           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
071390           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
071400           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
071410            IN-EKH-KVANTAL * IN-EKH-PRARTNTO                              
071420           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
071430           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
071440           MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                    
071450           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
071460           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
071470           PERFORM S04-WRITE-W51073A                                      
071480         END-IF                                                           
071490       END-IF                                                             
071500                                                                          
071510     WHEN 'LAND'                                                          
071520       MOVE SYST-IDKONTO            TO WS-R3-ACCOUNT-10                   
071530       MOVE WS-R3-ACCOUNT-6         TO R3-LINE-ACCOUNT                    
071540       MOVE IN-EKH-SUBEL            TO R3-LINE-AMOUNT-LC                  
071550       MOVE IN-EKH-IDDC-REC         TO WS-ALLOCATE-DC                     
071560       MOVE SPACE                   TO WS-ALLOCATE-DISTR                  
071570       MOVE IN-EKH-IDFAKT-EXP       TO WS-ALLOCATE-REF                    
071580       MOVE WS-ALLOCATE             TO R3-LINE-ALLOCATE                   
071590       MOVE SYST-IDKST              TO WS-RED-IDKST                       
071600       MOVE WS-RED-IDKST            TO R3-LINE-COST-CENTER                
071610       PERFORM S04-WRITE-W51073A                                          
071620                                                                          
071630     END-EVALUATE                                                         
071640     .                                                                    
071650     EJECT                                                                
071660                                                                          
071670 CEJ386-SUB-EVENT-303-386 SECTION.                                        
071680     EVALUATE IN-EKH-KDEKNIVA                                             
071690     WHEN 'DET'                                                           
071700                                                                          
071710       IF SYST-IDSEKVNR = 1                                               
071720         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
071730         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
071740         IF BET-KDTRADP(3:2) NOT = SPACE                                  
071750           MOVE '1'               TO WS-ACCOUNT-4                         
071760         ELSE                                                             
071770           MOVE '3'               TO WS-ACCOUNT-4                         
071780         END-IF                                                           
071790         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
071800         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
071810         COMPUTE R3-LINE-AMOUNT-LC =                                      
071820                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
071830         END-COMPUTE                                                      
071840         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
071850         MOVE SPACE               TO WS-ALLOCATE-DC                       
071860         MOVE IN-EKH-IDDISTR      TO WS-ALLOCATE-DISTR                    
071870         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
071880         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
071890         MOVE IN-EKH-KDEKHHT      TO WS-LINE-TEXT-KDEKHHT                 
071900         MOVE IN-EKH-KDEKSHT      TO WS-LINE-TEXT-KDEKSHT                 
071910         MOVE WS-LINE-TEXT        TO R3-LINE-TEXT                         
071920         PERFORM S03-WRITE-W51072                                         
071930       END-IF                                                             
071940                                                                          
071950       IF SYST-IDSEKVNR = 2                                               
071960         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
071970         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
071980         COMPUTE R3-LINE-AMOUNT-LC =                                      
071990                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
072000         END-COMPUTE                                                      
072010         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
072020         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
072030         MOVE SPACE               TO WS-ALLOCATE-DC                       
072040         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
072050         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
072060         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
072070         PERFORM S03-WRITE-W51072                                         
072080       END-IF                                                             
072090                                                                          
072100       IF SYST-IDSEKVNR = 3                                               
072110         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
072120         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
072130         IF BET-KDTRADP(3:2) NOT = SPACE                                  
072140           MOVE '1'               TO WS-ACCOUNT-4                         
072150         ELSE                                                             
072160           MOVE '3'               TO WS-ACCOUNT-4                         
072170         END-IF                                                           
072180         MOVE WS-ACCOUNT          TO WS-R3-ACCOUNT-10                     
072190         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
072200         COMPUTE R3-LINE-AMOUNT-LC =                                      
072210           IN-EKH-KVANTAL * IN-EKH-PRARTSTD                               
072220         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-PA-CUSTOMER                  
072230         MOVE SPACE               TO WS-ALLOCATE-DC                       
072240         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
072250         MOVE IN-EKH-IDFAKT-EXP   TO WS-ALLOCATE-REF                      
072260         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
072270         PERFORM S03-WRITE-W51072                                         
072280       END-IF                                                             
072290                                                                          
072300     END-EVALUATE                                                         
072310     .                                                                    
072320     EJECT                                                                
072330                                                                          
072340 CEK-MAIN-EVENT-401 SECTION.                                              
072350     EVALUATE IN-EKH-KDEKNIVA                                             
072360                                                                          
072370* PRISÄNDRING LÖPANDE                                                     
072380     WHEN 'DET'                                                           
072390       COMPUTE WS-BELOPP = IN-EKH-KVANTAL *                               
072400                           IN-EKH-PRARTSTD                                
072410       END-COMPUTE                                                        
072420       IF SYST-IDSEKVNR = 1                                               
072430                                                                          
072440* PRISHÖJNING                                                             
072450         IF WS-BELOPP > 0                                                 
072460           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
072470           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
072480           COMPUTE R3-LINE-AMOUNT-LC =                                    
072490                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
072500           END-COMPUTE                                                    
072510           PERFORM S11-ANALYSIS                                           
072520           PERFORM S02-WRITE-W51071A                                      
072530         END-IF                                                           
072540       END-IF                                                             
072550                                                                          
072560       IF SYST-IDSEKVNR = 2                                               
072570                                                                          
072580* PRISHÖJNING                                                             
072590         IF WS-BELOPP > 0                                                 
072600           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
072610           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
072620           IF KDPRODSL-LYNK                                               
072630             MOVE '1'               TO WS-ACCOUNT-5                       
072640             MOVE '0'               TO WS-ACCOUNT-5A                      
072650             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
072660             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
072670             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
072680             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
072690             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
072700           END-IF                                                         
072710           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
072720           COMPUTE R3-LINE-AMOUNT-LC =                                    
072730                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
072740           END-COMPUTE                                                    
072750           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
072760           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
072770           MOVE SPACE               TO WS-ALLOCATE-REF                    
072780           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
072790           PERFORM S02-WRITE-W51071A                                      
072800         END-IF                                                           
072810       END-IF                                                             
072820                                                                          
072830       IF SYST-IDSEKVNR = 3                                               
072840                                                                          
072850* PRISSÄNKNING                                                            
072860         IF WS-BELOPP < 0                                                 
072870           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
072880           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
072890           COMPUTE R3-LINE-AMOUNT-LC =                                    
072900                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
072910           END-COMPUTE                                                    
072920           PERFORM S11-ANALYSIS                                           
072930           PERFORM S02-WRITE-W51071A                                      
072940         END-IF                                                           
072950       END-IF                                                             
072960                                                                          
072970       IF SYST-IDSEKVNR = 4                                               
072980                                                                          
072990* PRISSÄNKNING                                                            
073000         IF WS-BELOPP < 0                                                 
073010           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
073020           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
073030           IF KDPRODSL-LYNK                                               
073040             MOVE '1'               TO WS-ACCOUNT-5                       
073050             MOVE '0'               TO WS-ACCOUNT-5A                      
073060             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
073070             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
073080             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
073090             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
073100             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
073110           END-IF                                                         
073120           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
073130           COMPUTE R3-LINE-AMOUNT-LC =                                    
073140                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
073150           END-COMPUTE                                                    
073160           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
073170           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
073180           MOVE SPACE               TO WS-ALLOCATE-REF                    
073190           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
073200           PERFORM S02-WRITE-W51071A                                      
073210         END-IF                                                           
073220       END-IF                                                             
073230                                                                          
073240* PRISÄNDRING ÅRLIG                                                       
073250     WHEN 'SUM'                                                           
073260       IF SYST-IDSEKVNR = 1                                               
073270                                                                          
073280* PRISHÖJNING                                                             
073290         IF IN-EKH-SUBEL > 0                                              
073300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
073310           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
073320           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
073330           PERFORM S11-ANALYSIS                                           
073340           PERFORM S02-WRITE-W51071A                                      
073350         END-IF                                                           
073360       END-IF                                                             
073370                                                                          
073380       IF SYST-IDSEKVNR = 2                                               
073390                                                                          
073400* PRISHÖJNING                                                             
073410         IF IN-EKH-SUBEL > 0                                              
073420           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
073430           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
073440           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
073450           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
073460           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
073470           MOVE SPACE               TO WS-ALLOCATE-REF                    
073480           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
073490           PERFORM S02-WRITE-W51071A                                      
073500         END-IF                                                           
073510       END-IF                                                             
073520                                                                          
073530       IF SYST-IDSEKVNR = 3                                               
073540                                                                          
073550* PRISSÄNKNING                                                            
073560         IF IN-EKH-SUBEL < 0                                              
073570           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
073580           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
073590           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
073600           PERFORM S11-ANALYSIS                                           
073610           PERFORM S02-WRITE-W51071A                                      
073620         END-IF                                                           
073630       END-IF                                                             
073640                                                                          
073650       IF SYST-IDSEKVNR = 4                                               
073660                                                                          
073670* PRISSÄNKNING                                                            
073680         IF IN-EKH-SUBEL < 0                                              
073690           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
073700           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
073710           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
073720           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
073730           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
073740           MOVE SPACE               TO WS-ALLOCATE-REF                    
073750           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
073760           PERFORM S02-WRITE-W51071A                                      
073770         END-IF                                                           
073780       END-IF                                                             
073790     END-EVALUATE                                                         
073800     .                                                                    
073810     EJECT                                                                
073820                                                                          
073830 CEL-MAIN-EVENT-402 SECTION.                                              
073840     EVALUATE IN-EKH-KDEKNIVA                                             
073850     WHEN 'DET'                                                           
073860       IF SYST-IDSEKVNR = 1                                               
073870         IF IN-EKH-KVANTAL > 0                                            
073880           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
073890           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
073900           IF KDPRODSL-LYNK                                               
073910             MOVE '1'               TO WS-ACCOUNT-5                       
073920             MOVE '0'               TO WS-ACCOUNT-5A                      
073930             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
073940             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
073950             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
073960             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
073970             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
073980           END-IF                                                         
073990           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
074000           COMPUTE R3-LINE-AMOUNT-LC =                                    
074010                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
074020           END-COMPUTE                                                    
074030           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
074040           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
074050           MOVE SPACE               TO WS-ALLOCATE-REF                    
074060           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
074070           PERFORM S02-WRITE-W51071A                                      
074080         END-IF                                                           
074090       END-IF                                                             
074100                                                                          
074110       IF SYST-IDSEKVNR = 2                                               
074120         IF IN-EKH-KVANTAL < 0                                            
074130           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
074140           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
074150           IF KDPRODSL-LYNK                                               
074160             MOVE '1'               TO WS-ACCOUNT-5                       
074170             MOVE '0'               TO WS-ACCOUNT-5A                      
074180             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
074190             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
074200             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
074210             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
074220             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
074230           END-IF                                                         
074240           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
074250           COMPUTE R3-LINE-AMOUNT-LC =                                    
074260                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
074270           END-COMPUTE                                                    
074280           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
074290           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
074300           MOVE SPACE               TO WS-ALLOCATE-REF                    
074310           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
074320           PERFORM S02-WRITE-W51071A                                      
074330         END-IF                                                           
074340       END-IF                                                             
074350     END-EVALUATE                                                         
074360     .                                                                    
074370     EJECT                                                                
074380                                                                          
074390 CEM-MAIN-EVENT-403 SECTION.                                              
074400     EVALUATE IN-EKH-KDEKSHT                                              
074410     WHEN '401'                                                           
074420     WHEN '402'                                                           
074430     WHEN '403'                                                           
074440     WHEN '404'                                                           
074450     WHEN '405'                                                           
074460     WHEN '407'                                                           
074470     WHEN '408'                                                           
074480     WHEN '409'                                                           
074490          PERFORM CEMA-SUB-EVENT-403-401-MFL                              
074500     WHEN '410'                                                           
074510          PERFORM CEMB-SUB-EVENT-403-410                                  
074520     END-EVALUATE                                                         
074530     .                                                                    
074540     EJECT                                                                
074550                                                                          
074560 CEMA-SUB-EVENT-403-401-MFL SECTION.                                      
074570     EVALUATE IN-EKH-KDEKNIVA                                             
074580     WHEN 'DET'                                                           
074590       IF SYST-IDSEKVNR = 1                                               
074600         IF IN-EKH-KVANTAL > 0                                            
074610           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
074620           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
074630           COMPUTE R3-LINE-AMOUNT-LC =                                    
074640                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
074650           END-COMPUTE                                                    
074660           PERFORM S11-ANALYSIS                                           
074670           IF IN-EKH-KDEKHHT = '403'                                      
074680           AND IN-EKH-KDEKSHT = '408'                                     
074690             MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT               
074700             MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT               
074710             MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                       
074720             MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                     
074730             MOVE SPACE             TO WS-ALLOCATE-DISTR                  
074740             MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                    
074750             MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                   
074760           END-IF                                                         
074770           PERFORM S02-WRITE-W51071A                                      
074780         END-IF                                                           
074790       END-IF                                                             
074800                                                                          
074810       IF SYST-IDSEKVNR = 2                                               
074820         IF IN-EKH-KVANTAL > 0                                            
074830           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
074840           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
074850           IF KDPRODSL-LYNK                                               
074860             MOVE '1'               TO WS-ACCOUNT-5                       
074870             MOVE '0'               TO WS-ACCOUNT-5A                      
074880             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
074890             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
074900             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
074910             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
074920             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
074930           END-IF                                                         
074940           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
074950           COMPUTE R3-LINE-AMOUNT-LC =                                    
074960                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
074970           END-COMPUTE                                                    
074980           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
074990           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
075000           MOVE SPACE               TO WS-ALLOCATE-REF                    
075010           IF IN-EKH-KDEKHHT = '403'                                      
075020           AND IN-EKH-KDEKSHT = '408'                                     
075030             MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT               
075040             MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT               
075050             MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                       
075060             MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                    
075070           END-IF                                                         
075080           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
075090           PERFORM S02-WRITE-W51071A                                      
075100         END-IF                                                           
075110       END-IF                                                             
075120                                                                          
075130       IF SYST-IDSEKVNR = 3                                               
075140         IF IN-EKH-KVANTAL < 0                                            
075150           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
075160           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
075170           COMPUTE R3-LINE-AMOUNT-LC =                                    
075180                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
075190           END-COMPUTE                                                    
075200           PERFORM S11-ANALYSIS                                           
075210           IF IN-EKH-KDEKHHT = '403'                                      
075220           AND IN-EKH-KDEKSHT = '408'                                     
075230             MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT               
075240             MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT               
075250             MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                       
075260             MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                     
075270             MOVE SPACE             TO WS-ALLOCATE-DISTR                  
075280             MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                    
075290             MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                   
075300           END-IF                                                         
075310           PERFORM S02-WRITE-W51071A                                      
075320         END-IF                                                           
075330       END-IF                                                             
075340                                                                          
075350       IF SYST-IDSEKVNR = 4                                               
075360         IF IN-EKH-KVANTAL < 0                                            
075370           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
075380           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
075390           IF KDPRODSL-LYNK                                               
075400             MOVE '1'               TO WS-ACCOUNT-5                       
075410             MOVE '0'               TO WS-ACCOUNT-5A                      
075420             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
075430             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
075440             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
075450             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
075460             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
075470           END-IF                                                         
075480           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
075490           COMPUTE R3-LINE-AMOUNT-LC =                                    
075500                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
075510           END-COMPUTE                                                    
075520           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
075530           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
075540           MOVE SPACE               TO WS-ALLOCATE-REF                    
075550           IF IN-EKH-KDEKHHT = '403'                                      
075560           AND IN-EKH-KDEKSHT = '408'                                     
075570             MOVE IN-EKH-KDEKHHT    TO WS-LINE-TEXT-KDEKHHT               
075580             MOVE IN-EKH-KDEKSHT    TO WS-LINE-TEXT-KDEKSHT               
075590             MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                       
075600             MOVE IN-EKH-IDFAKT-EXP TO WS-ALLOCATE-REF                    
075610           END-IF                                                         
075620           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
075630           PERFORM S02-WRITE-W51071A                                      
075640         END-IF                                                           
075650       END-IF                                                             
075660     END-EVALUATE                                                         
075670     .                                                                    
075680     EJECT                                                                
075690                                                                          
075700 CEMB-SUB-EVENT-403-410 SECTION.                                          
075710     EVALUATE IN-EKH-KDEKNIVA                                             
075720     WHEN 'DET'                                                           
075730       IF SYST-IDSEKVNR = 1                                               
075740         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
075750         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
075760         IF KDPRODSL-LYNK                                                 
075770           MOVE '1'               TO WS-ACCOUNT-5                         
075780           MOVE '0'               TO WS-ACCOUNT-5A                        
075790           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
075800           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
075810           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
075820           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
075830           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
075840         END-IF                                                           
075850         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
075860         COMPUTE R3-LINE-AMOUNT-LC =                                      
075870                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
075880         END-COMPUTE                                                      
075890         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
075900         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
075910         MOVE SPACE               TO WS-ALLOCATE-REF                      
075920         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
075930         PERFORM S02-WRITE-W51071A                                        
075940       END-IF                                                             
075950                                                                          
075960       IF SYST-IDSEKVNR = 2                                               
075970         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
075980         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
075990         IF KDPRODSL-LYNK                                                 
076000           MOVE '1'               TO WS-ACCOUNT-5                         
076010           MOVE '0'               TO WS-ACCOUNT-5A                        
076020           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
076030           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
076040           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
076050           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
076060           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
076070         END-IF                                                           
076080         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
076090         COMPUTE R3-LINE-AMOUNT-LC =                                      
076100                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
076110         END-COMPUTE                                                      
076120         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
076130         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
076140         MOVE SPACE               TO WS-ALLOCATE-REF                      
076150         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
076160         PERFORM S02-WRITE-W51071A                                        
076170       END-IF                                                             
076180     END-EVALUATE                                                         
076190     .                                                                    
076200     EJECT                                                                
076210                                                                          
076220 CEN-MAIN-EVENT-404 SECTION.                                              
076230     EVALUATE IN-EKH-KDEKNIVA                                             
076240     WHEN 'DET'                                                           
076250       IF SYST-IDSEKVNR = 1                                               
076260                                                                          
076270* KONTO EJ MANUELLT REGISTRERAT                                           
076280         IF IN-EKH-IDKONTO = 0                                            
076290           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
076300           IF DIST18-SKROT-SDC                                            
076310             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
076320             MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                       
076330             PERFORM S11-ANALYSIS                                         
076340             IF KDPRODSL-LYNK                                             
076350               MOVE '3'               TO WS-ACCOUNT-5                     
076360               MOVE 'LYNK'            TO WS-PRCTR(1:4)                    
076370               MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP             
076380               MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                 
076390               MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER            
076400               MOVE '158600002782'    TO R3-LINE-ORDER                    
076410               MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                 
076420             END-IF                                                       
076430             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
076440             COMPUTE R3-LINE-AMOUNT-LC =                                  
076450                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
076460             END-COMPUTE                                                  
076470             PERFORM S02-WRITE-W51071A                                    
076480           END-IF                                                         
076490         END-IF                                                           
076500       END-IF                                                             
076510                                                                          
076520       IF SYST-IDSEKVNR = 2                                               
076530                                                                          
076540* KONTO EJ MANUELLT REGISTRERAT                                           
076550         IF IN-EKH-IDKONTO = 0                                            
076560           MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                     
076570           IF NOT DIST18-SKROT-SDC                                        
076580             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
076590             MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                       
076600             PERFORM S11-ANALYSIS                                         
076610             IF KDPRODSL-LYNK                                             
076620               MOVE '2331'            TO WS-ACCOUNT(7:4)                  
076630               MOVE 'LYNK'            TO WS-PRCTR(1:4)                    
076640               MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP             
076650               MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                 
076660               MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER            
076670               MOVE '158600002782'    TO R3-LINE-ORDER                    
076680               MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                 
076690             END-IF                                                       
076700             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
076710             COMPUTE R3-LINE-AMOUNT-LC =                                  
076720                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
076730             END-COMPUTE                                                  
076740             PERFORM S02-WRITE-W51071A                                    
076750           END-IF                                                         
076760         END-IF                                                           
076770       END-IF                                                             
076780                                                                          
076790       IF SYST-IDSEKVNR = 3                                               
076800                                                                          
076810* KONTO MANUELLT REGISTRERAT                                              
076820         IF IN-EKH-IDKONTO > 0                                            
076830           MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                   
076840           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
076850           MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                      
076860           IF KDPRODSL-LYNK                                               
076870             MOVE '482331'          TO WS-R3-ACCOUNT-6                    
076880             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
076890             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
076900             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
076910             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
076920             MOVE '158600002782'    TO R3-LINE-ORDER                      
076930           END-IF                                                         
076940           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
076950           COMPUTE R3-LINE-AMOUNT-LC =                                    
076960                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
076970           END-COMPUTE                                                    
076980           PERFORM S02-WRITE-W51071A                                      
076990         END-IF                                                           
077000       END-IF                                                             
077010                                                                          
077020       IF SYST-IDSEKVNR = 4                                               
077030         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
077040         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
077050         IF KDPRODSL-LYNK                                                 
077060           MOVE '1'               TO WS-ACCOUNT-5                         
077070           MOVE '0'               TO WS-ACCOUNT-5A                        
077080           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
077090           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
077100           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
077110           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
077120           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
077130         END-IF                                                           
077140         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
077150         COMPUTE R3-LINE-AMOUNT-LC =                                      
077160                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
077170         END-COMPUTE                                                      
077180         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
077190         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
077200         MOVE SPACE               TO WS-ALLOCATE-REF                      
077210         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
077220         PERFORM S02-WRITE-W51071A                                        
077230       END-IF                                                             
077240     END-EVALUATE                                                         
077250     .                                                                    
077260     EJECT                                                                
077270                                                                          
077280 CEO-MAIN-EVENT-405 SECTION.                                              
077290     EVALUATE IN-EKH-KDEKSHT                                              
077300     WHEN '402'                                                           
077310          PERFORM CEOB-SUB-EVENT-405-402                                  
077320     WHEN '403'                                                           
077330          PERFORM CEOC-SUB-EVENT-405-403                                  
077340     END-EVALUATE                                                         
077350     .                                                                    
077360     EJECT                                                                
077370                                                                          
077380 CEOB-SUB-EVENT-405-402 SECTION.                                          
077390     EVALUATE IN-EKH-KDEKNIVA                                             
077400     WHEN 'DET'                                                           
077410       IF SYST-IDSEKVNR = 1                                               
077420         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
077430         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
077440         IF KDPRODSL-LYNK                                                 
077450           MOVE '1'               TO WS-ACCOUNT-5                         
077460           MOVE '0'               TO WS-ACCOUNT-5A                        
077470           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
077480           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
077490           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
077500           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
077510           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
077520         END-IF                                                           
077530         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
077540         COMPUTE R3-LINE-AMOUNT-LC =                                      
077550                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
077560         END-COMPUTE                                                      
077570         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
077580         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
077590         MOVE SPACE               TO WS-ALLOCATE-REF                      
077600         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
077610         PERFORM S02-WRITE-W51071A                                        
077620       END-IF                                                             
077630                                                                          
077640       IF SYST-IDSEKVNR = 2                                               
077650         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
077660         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
077670         COMPUTE R3-LINE-AMOUNT-LC =                                      
077680                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
077690         END-COMPUTE                                                      
077700         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
077710         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
077720         MOVE SPACE               TO WS-ALLOCATE-REF                      
077730         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
077740         MOVE WS-PRCTR            TO R3-LINE-PROFIT-CENTER                
077750         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
077760         PERFORM S02-WRITE-W51071A                                        
077770       END-IF                                                             
077780     END-EVALUATE                                                         
077790     .                                                                    
077800     EJECT                                                                
077810                                                                          
077820 CEOC-SUB-EVENT-405-403 SECTION.                                          
077830     EVALUATE IN-EKH-KDEKNIVA                                             
077840     WHEN 'DET'                                                           
077850       IF SYST-IDSEKVNR = 1                                               
077860         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
077870         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
077880         IF KDPRODSL-LYNK                                                 
077890           MOVE '1'               TO WS-ACCOUNT-5                         
077900           MOVE '0'               TO WS-ACCOUNT-5A                        
077910           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
077920           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
077930           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
077940           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
077950           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
077960         END-IF                                                           
077970         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
077980         COMPUTE R3-LINE-AMOUNT-LC =                                      
077990                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
078000         END-COMPUTE                                                      
078010         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
078020         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
078030         MOVE SPACE               TO WS-ALLOCATE-REF                      
078040         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
078050         PERFORM S02-WRITE-W51071A                                        
078060       END-IF                                                             
078070                                                                          
078080       IF SYST-IDSEKVNR = 2                                               
078090         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
078100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
078110         COMPUTE R3-LINE-AMOUNT-LC =                                      
078120                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
078130         END-COMPUTE                                                      
078140         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
078150         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
078160         MOVE SPACE               TO WS-ALLOCATE-REF                      
078170         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
078180         MOVE WS-PRCTR            TO R3-LINE-PROFIT-CENTER                
078190         MOVE IN-EKH-IDANALYS     TO R3-LINE-ORDER                        
078200         PERFORM S02-WRITE-W51071A                                        
078210       END-IF                                                             
078220     END-EVALUATE                                                         
078230     .                                                                    
078240     EJECT                                                                
078250                                                                          
078260 CEP-MAIN-EVENT-406 SECTION.                                              
078270     EVALUATE IN-EKH-KDEKNIVA                                             
078280     WHEN 'DET'                                                           
078290       IF SYST-IDSEKVNR = 1                                               
078300         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
078310         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
078320         IF KDPRODSL-LYNK                                                 
078330           MOVE '1'               TO WS-ACCOUNT-5                         
078340           MOVE '0'               TO WS-ACCOUNT-5A                        
078350           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
078360           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
078370           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
078380           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
078390           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
078400         END-IF                                                           
078410         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
078420         COMPUTE R3-LINE-AMOUNT-LC =                                      
078430                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
078440         END-COMPUTE                                                      
078450         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
078460         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
078470         MOVE SPACE               TO WS-ALLOCATE-REF                      
078480         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
078490         PERFORM S02-WRITE-W51071A                                        
078500       END-IF                                                             
078510                                                                          
078520       IF SYST-IDSEKVNR = 2                                               
078530         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
078540         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
078550         COMPUTE R3-LINE-AMOUNT-LC =                                      
078560                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
078570         END-COMPUTE                                                      
078580         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
078590         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
078600         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
078610         MOVE SPACE               TO WS-ALLOCATE-REF                      
078620         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
078630         PERFORM S02-WRITE-W51071A                                        
078640       END-IF                                                             
078650     END-EVALUATE                                                         
078660     .                                                                    
078670     EJECT                                                                
078680                                                                          
078690 CEQ-MAIN-EVENT-501 SECTION.                                              
078700                                                                          
078710     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
078720     EVALUATE IN-EKH-KDEKSHT                                              
078730     WHEN '501'                                                           
078740          PERFORM CEQA-SUB-EVENT-501-501                                  
078750     WHEN '502'                                                           
078760          PERFORM CEQB-SUB-EVENT-501-502                                  
078770     END-EVALUATE                                                         
078780     .                                                                    
078790     EJECT                                                                
078800                                                                          
078810 CEQA-SUB-EVENT-501-501 SECTION.                                          
078820                                                                          
078830     EVALUATE IN-EKH-KDEKNIVA                                             
078840     WHEN 'DET'                                                           
078850       IF SYST-IDSEKVNR = 1                                               
078860         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
078870         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
078880         IF KDPRODSL-LYNK                                                 
078890           MOVE '1'               TO WS-ACCOUNT-5                         
078900           MOVE '0'               TO WS-ACCOUNT-5A                        
078910           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
078920           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
078930           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
078940           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
078950           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
078960         END-IF                                                           
078970         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
078980         COMPUTE R3-LINE-AMOUNT-LC =                                      
078990                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
079000         END-COMPUTE                                                      
079010         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
079020         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
079030         MOVE SPACE               TO WS-ALLOCATE-REF                      
079040         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
079050         IF DIST35-REFILL-VAT-EU                                          
079060           PERFORM S80-CHECK-TAX-CODE-3                                   
079070         END-IF                                                           
079080         PERFORM S03-WRITE-W51072                                         
079090       END-IF                                                             
079100                                                                          
079110       IF SYST-IDSEKVNR = 2                                               
079120         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
079130         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
079140         IF KDPRODSL-LYNK                                                 
079150           MOVE '1'               TO WS-ACCOUNT-5                         
079160           MOVE '0'               TO WS-ACCOUNT-5A                        
079170           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
079180           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
079190           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
079200           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
079210           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
079220         END-IF                                                           
079230         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
079240         COMPUTE R3-LINE-AMOUNT-LC =                                      
079250                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
079260         END-COMPUTE                                                      
079270         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
079280         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
079290         MOVE SPACE               TO WS-ALLOCATE-REF                      
079300         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
079310         IF DIST35-REFILL-VAT-EU                                          
079320           PERFORM S80-CHECK-TAX-CODE-4                                   
079330         END-IF                                                           
079340         PERFORM S03-WRITE-W51072                                         
079350       END-IF                                                             
079360                                                                          
079370       IF DIST35-REFILL-VAT-NON-EU                                        
079380         IF SYST-IDSEKVNR = 3                                             
079390           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
079400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
079410           COMPUTE R3-LINE-AMOUNT-LC =                                    
079420                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
079430           END-COMPUTE                                                    
079440           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
079450           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
079460           MOVE SPACE             TO WS-ALLOCATE-REF                      
079470           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
079480***        MOVE 'Y3'              TO R3-LINE-TAX-CODE                     
079490           PERFORM S80-CHECK-TAX-CODE-3                                   
079500           PERFORM S03-WRITE-W51072                                       
079510         END-IF                                                           
079520                                                                          
079530         IF SYST-IDSEKVNR = 4                                             
079540           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
079550           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
079560           COMPUTE R3-LINE-AMOUNT-LC =                                    
079570                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
079580           END-COMPUTE                                                    
079590           MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                       
079600           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
079610           MOVE SPACE             TO WS-ALLOCATE-REF                      
079620           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
079630***        MOVE '90'              TO R3-LINE-TAX-CODE                     
079640           PERFORM S80-CHECK-TAX-CODE-4                                   
079650           PERFORM S03-WRITE-W51072                                       
079660         END-IF                                                           
079670       END-IF                                                             
079680     END-EVALUATE                                                         
079690     .                                                                    
079700     EJECT                                                                
079710                                                                          
079720 CEQB-SUB-EVENT-501-502   SECTION.                                        
079730     EVALUATE IN-EKH-KDEKNIVA                                             
079740     WHEN 'DET'                                                           
079750       IF SYST-IDSEKVNR = 1                                               
079760         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
079770         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
079780         IF KDPRODSL-LYNK                                                 
079790           MOVE '1'               TO WS-ACCOUNT-5                         
079800           MOVE '0'               TO WS-ACCOUNT-5A                        
079810           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
079820           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
079830           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
079840           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
079850           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
079860         END-IF                                                           
079870         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
079880         COMPUTE R3-LINE-AMOUNT-LC =                                      
079890                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
079900         END-COMPUTE                                                      
079910         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
079920         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
079930         MOVE SPACE               TO WS-ALLOCATE-REF                      
079940         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
079950         IF DIST35-RETUR-VAT-EU                                           
079960         OR DIST35-RETURQ-VAT-EU                                          
079970           PERFORM S80-CHECK-TAX-CODE-3                                   
079980         END-IF                                                           
079990         PERFORM S03-WRITE-W51072                                         
080000       END-IF                                                             
080010                                                                          
080020       IF SYST-IDSEKVNR = 2                                               
080030         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
080040         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
080050         IF KDPRODSL-LYNK                                                 
080060           MOVE '1'               TO WS-ACCOUNT-5                         
080070           MOVE '0'               TO WS-ACCOUNT-5A                        
080080           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
080090           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
080100           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
080110           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
080120           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
080130         END-IF                                                           
080140         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
080150         COMPUTE R3-LINE-AMOUNT-LC =                                      
080160                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
080170         END-COMPUTE                                                      
080180         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
080190         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
080200         MOVE SPACE               TO WS-ALLOCATE-REF                      
080210         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
080220         IF DIST35-RETUR-VAT-EU                                           
080230         OR DIST35-RETURQ-VAT-EU                                          
080240           PERFORM S80-CHECK-TAX-CODE-4                                   
080250         END-IF                                                           
080260         PERFORM S03-WRITE-W51072                                         
080270       END-IF                                                             
080280                                                                          
080290       IF DIST35-RETUR-VAT-NON-EU                                         
080300       OR DIST35-RETURQ-VAT-NON-EU                                        
080310         IF SYST-IDSEKVNR = 3                                             
080320           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
080330           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
080340           COMPUTE R3-LINE-AMOUNT-LC =                                    
080350                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
080360           END-COMPUTE                                                    
080370           MOVE IN-EKH-IDDC-REC   TO WS-ALLOCATE-DC                       
080380           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
080390           MOVE SPACE             TO WS-ALLOCATE-REF                      
080400           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
080410***        MOVE '58'              TO R3-LINE-TAX-CODE                     
080420           PERFORM S80-CHECK-TAX-CODE-3                                   
080430           PERFORM S03-WRITE-W51072                                       
080440         END-IF                                                           
080450                                                                          
080460         IF SYST-IDSEKVNR = 4                                             
080470           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
080480           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
080490           COMPUTE R3-LINE-AMOUNT-LC =                                    
080500                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
080510           END-COMPUTE                                                    
080520           MOVE IN-EKH-IDDC-SEND  TO WS-ALLOCATE-DC                       
080530           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
080540           MOVE SPACE             TO WS-ALLOCATE-REF                      
080550           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
080560***        MOVE 'Y4'              TO R3-LINE-TAX-CODE                     
080570           PERFORM S80-CHECK-TAX-CODE-4                                   
080580           PERFORM S03-WRITE-W51072                                       
080590         END-IF                                                           
080600       END-IF                                                             
080610     END-EVALUATE                                                         
080620     .                                                                    
080630     EJECT                                                                
080640                                                                          
080650 CER-MAIN-EVENT-502 SECTION.                                              
080660                                                                          
080670     MOVE IN-EKH-IDDISTR          TO TEST-IDDISTR                         
080680     EVALUATE IN-EKH-KDEKSHT                                              
080690     WHEN '501'                                                           
080700          PERFORM CERA-SUB-EVENT-502-501                                  
080710     WHEN '502'                                                           
080720          PERFORM CERB-SUB-EVENT-502-502                                  
080730     WHEN '503'                                                           
080740          PERFORM CERB-SUB-EVENT-502-503                                  
080750     WHEN '504'                                                           
080760          PERFORM CERC-SUB-EVENT-502-504                                  
080770     END-EVALUATE                                                         
080780     .                                                                    
080790     EJECT                                                                
080800                                                                          
080810 CERA-SUB-EVENT-502-501 SECTION.                                          
080820     EVALUATE IN-EKH-KDEKNIVA                                             
080830     WHEN 'DET'                                                           
080840       IF SYST-IDSEKVNR = 1                                               
080850                                                                          
080860* HÄR BOKAS FÖRLORAT KOLLI                                                
080870         IF IN-EKH-KVANTAL < 0                                            
080880           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
080890           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
080900           IF KDPRODSL-LYNK                                               
080910             MOVE '1'               TO WS-ACCOUNT-5                       
080920             MOVE '0'               TO WS-ACCOUNT-5A                      
080930             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
080940             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
080950             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
080960             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
080970             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
080980           END-IF                                                         
080990           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
081000           COMPUTE R3-LINE-AMOUNT-LC =                                    
081010                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
081020           END-COMPUTE                                                    
081030           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
081040           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
081050           MOVE SPACE               TO WS-ALLOCATE-REF                    
081060           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
081070           PERFORM S02-WRITE-W51071A                                      
081080         END-IF                                                           
081090       END-IF                                                             
081100                                                                          
081110       IF SYST-IDSEKVNR = 2                                               
081120                                                                          
081130* HÄR BOKAS FÖRLORAT KOLLI                                                
081140         IF IN-EKH-KVANTAL < 0                                            
081150           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
081160           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
081170           COMPUTE R3-LINE-AMOUNT-LC =                                    
081180                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
081190           END-COMPUTE                                                    
081200           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
081210           PERFORM S02-WRITE-W51071A                                      
081220         END-IF                                                           
081230       END-IF                                                             
081240                                                                          
081250       IF SYST-IDSEKVNR = 3                                               
081260                                                                          
081270* HÄR BOKAS ÅTERFUNNET KOLLI                                              
081280         IF IN-EKH-KVANTAL > 0                                            
081290           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
081300           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
081310           IF KDPRODSL-LYNK                                               
081320             MOVE '1'               TO WS-ACCOUNT-5                       
081330             MOVE '0'               TO WS-ACCOUNT-5A                      
081340             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
081350             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
081360             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
081370             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
081380             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
081390           END-IF                                                         
081400           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
081410           COMPUTE R3-LINE-AMOUNT-LC =                                    
081420                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
081430           END-COMPUTE                                                    
081440           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
081450           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
081460           MOVE SPACE               TO WS-ALLOCATE-REF                    
081470           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
081480           PERFORM S02-WRITE-W51071A                                      
081490         END-IF                                                           
081500       END-IF                                                             
081510                                                                          
081520       IF SYST-IDSEKVNR = 4                                               
081530                                                                          
081540* HÄR BOKAS ÅTERFUNNET KOLLI                                              
081550         IF IN-EKH-KVANTAL > 0                                            
081560           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
081570           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
081580           COMPUTE R3-LINE-AMOUNT-LC =                                    
081590                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
081600           END-COMPUTE                                                    
081610           MOVE SYST-IDANALYS       TO R3-LINE-ORDER                      
081620           PERFORM S02-WRITE-W51071A                                      
081630         END-IF                                                           
081640       END-IF                                                             
081650     END-EVALUATE                                                         
081660     .                                                                    
081670     EJECT                                                                
081680                                                                          
081690 CERB-SUB-EVENT-502-502 SECTION.                                          
081700     EVALUATE IN-EKH-KDEKNIVA                                             
081710     WHEN 'DET'                                                           
081720       IF SYST-IDSEKVNR = 1                                               
081730* HÄR BOKAS ÖVERLEVERNAS                                                  
081740         IF IN-EKH-KVANTAL > 0                                            
081750           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
081760           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
081770           IF KDPRODSL-LYNK                                               
081780             MOVE '1'               TO WS-ACCOUNT-5                       
081790             MOVE '0'               TO WS-ACCOUNT-5A                      
081800             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
081810             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
081820             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
081830             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
081840             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
081850           END-IF                                                         
081860           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
081870           COMPUTE R3-LINE-AMOUNT-LC =                                    
081880                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
081890           END-COMPUTE                                                    
081900           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
081910           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
081920           MOVE SPACE               TO WS-ALLOCATE-REF                    
081930           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
081940           IF DIST35-RETUR-VAT-EU                                         
081950           OR DIST35-RETURQ-VAT-EU                                        
081960             PERFORM S80-CHECK-TAX-CODE-3                                 
081970           END-IF                                                         
081980           PERFORM S02-WRITE-W51071A                                      
081990         END-IF                                                           
082000       END-IF                                                             
082010                                                                          
082020       IF SYST-IDSEKVNR = 2                                               
082030* HÄR BOKAS ÖVERLEVERNAS                                                  
082040         IF IN-EKH-KVANTAL > 0                                            
082050           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
082060           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
082070           IF KDPRODSL-LYNK                                               
082080             MOVE '1'               TO WS-ACCOUNT-5                       
082090             MOVE '0'               TO WS-ACCOUNT-5A                      
082100             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
082110             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
082120             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
082130             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
082140             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
082150           END-IF                                                         
082160           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
082170           COMPUTE R3-LINE-AMOUNT-LC =                                    
082180                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
082190           END-COMPUTE                                                    
082200           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
082210           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
082220           MOVE SPACE               TO WS-ALLOCATE-REF                    
082230           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
082240           IF DIST35-RETUR-VAT-EU                                         
082250           OR DIST35-RETURQ-VAT-EU                                        
082260             PERFORM S80-CHECK-TAX-CODE-4                                 
082270           END-IF                                                         
082280           PERFORM S02-WRITE-W51071A                                      
082290         END-IF                                                           
082300       END-IF                                                             
082310                                                                          
082320       IF DIST35-RETUR-VAT-NON-EU                                         
082330       OR DIST35-RETURQ-VAT-NON-EU                                        
082340         IF SYST-IDSEKVNR = 3                                             
082350           IF IN-EKH-KVANTAL > 0                                          
082360             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
082370             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
082380             COMPUTE R3-LINE-AMOUNT-LC =                                  
082390                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
082400             END-COMPUTE                                                  
082410             MOVE IN-EKH-IDDC-REC TO WS-ALLOCATE-DC                       
082420             MOVE SPACE           TO WS-ALLOCATE-DISTR                    
082430             MOVE SPACE           TO WS-ALLOCATE-REF                      
082440             MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                     
082450***          MOVE 'Y3'            TO R3-LINE-TAX-CODE                     
082460             PERFORM S80-CHECK-TAX-CODE-3                                 
082470             PERFORM S02-WRITE-W51071A                                    
082480           END-IF                                                         
082490         END-IF                                                           
082500                                                                          
082510         IF SYST-IDSEKVNR = 4                                             
082520           IF IN-EKH-KVANTAL > 0                                          
082530             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
082540             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
082550             COMPUTE R3-LINE-AMOUNT-LC =                                  
082560                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
082570             END-COMPUTE                                                  
082580             MOVE IN-EKH-IDDC-SEND TO WS-ALLOCATE-DC                      
082590             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
082600             MOVE SPACE            TO WS-ALLOCATE-REF                     
082610             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
082620***          MOVE '90'             TO R3-LINE-TAX-CODE                    
082630             PERFORM S80-CHECK-TAX-CODE-4                                 
082640             PERFORM S02-WRITE-W51071A                                    
082650           END-IF                                                         
082660         END-IF                                                           
082670       END-IF                                                             
082680     END-EVALUATE                                                         
082690     .                                                                    
082700     EJECT                                                                
082710                                                                          
082720 CERB-SUB-EVENT-502-503 SECTION.                                          
082730     EVALUATE IN-EKH-KDEKNIVA                                             
082740     WHEN 'DET'                                                           
082750       IF SYST-IDSEKVNR = 1                                               
082760* HÄR BOKAS UNDERLEVERNAS                                                 
082770         IF IN-EKH-KVANTAL < 0                                            
082780           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
082790           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
082800           IF KDPRODSL-LYNK                                               
082810             MOVE '1'               TO WS-ACCOUNT-5                       
082820             MOVE '0'               TO WS-ACCOUNT-5A                      
082830             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
082840             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
082850             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
082860             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
082870             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
082880           END-IF                                                         
082890           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
082900           COMPUTE R3-LINE-AMOUNT-LC =                                    
082910                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
082920           END-COMPUTE                                                    
082930           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
082940           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
082950           MOVE SPACE               TO WS-ALLOCATE-REF                    
082960           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
082970           IF DIST35-RETUR-VAT-EU                                         
082980           OR DIST35-RETURQ-VAT-EU                                        
082990             PERFORM S80-CHECK-TAX-CODE-3                                 
083000           END-IF                                                         
083010           PERFORM S02-WRITE-W51071A                                      
083020         END-IF                                                           
083030       END-IF                                                             
083040                                                                          
083050       IF SYST-IDSEKVNR = 2                                               
083060* HÄR BOKAS UNDERLEVERNAS                                                 
083070         IF IN-EKH-KVANTAL < 0                                            
083080           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
083090           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
083100           IF KDPRODSL-LYNK                                               
083110             MOVE '1'               TO WS-ACCOUNT-5                       
083120             MOVE '0'               TO WS-ACCOUNT-5A                      
083130             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
083140             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
083150             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
083160             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
083170             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
083180           END-IF                                                         
083190           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
083200           COMPUTE R3-LINE-AMOUNT-LC =                                    
083210                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
083220           END-COMPUTE                                                    
083230           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
083240           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
083250           MOVE SPACE               TO WS-ALLOCATE-REF                    
083260           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
083270           IF DIST35-RETUR-VAT-EU                                         
083280           OR DIST35-RETURQ-VAT-EU                                        
083290             PERFORM S80-CHECK-TAX-CODE-4                                 
083300           END-IF                                                         
083310           PERFORM S02-WRITE-W51071A                                      
083320         END-IF                                                           
083330       END-IF                                                             
083340                                                                          
083350       IF DIST35-RETUR-VAT-NON-EU                                         
083360       OR DIST35-RETURQ-VAT-NON-EU                                        
083370         IF SYST-IDSEKVNR = 3                                             
083380           IF IN-EKH-KVANTAL < 0                                          
083390             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
083400             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
083410             COMPUTE R3-LINE-AMOUNT-LC =                                  
083420                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
083430             END-COMPUTE                                                  
083440             MOVE IN-EKH-IDDC-SEND TO WS-ALLOCATE-DC                      
083450             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
083460             MOVE SPACE            TO WS-ALLOCATE-REF                     
083470             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
083480***          MOVE '90'             TO R3-LINE-TAX-CODE                    
083490             PERFORM S80-CHECK-TAX-CODE-4                                 
083500             PERFORM S02-WRITE-W51071A                                    
083510           END-IF                                                         
083520         END-IF                                                           
083530                                                                          
083540         IF SYST-IDSEKVNR = 4                                             
083550           IF IN-EKH-KVANTAL < 0                                          
083560             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
083570             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
083580             COMPUTE R3-LINE-AMOUNT-LC =                                  
083590                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
083600             END-COMPUTE                                                  
083610             MOVE IN-EKH-IDDC-REC  TO WS-ALLOCATE-DC                      
083620             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
083630             MOVE SPACE            TO WS-ALLOCATE-REF                     
083640             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
083650***          MOVE 'Y3'             TO R3-LINE-TAX-CODE                    
083660             PERFORM S80-CHECK-TAX-CODE-3                                 
083670             PERFORM S02-WRITE-W51071A                                    
083680           END-IF                                                         
083690         END-IF                                                           
083700       END-IF                                                             
083710     END-EVALUATE                                                         
083720     .                                                                    
083730     EJECT                                                                
083740                                                                          
083750 CERC-SUB-EVENT-502-504 SECTION.                                          
083760     EVALUATE IN-EKH-KDEKNIVA                                             
083770     WHEN 'DET'                                                           
083780       IF SYST-IDSEKVNR = 1                                               
083790         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
083800         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
083810         IF KDPRODSL-LYNK                                                 
083820           MOVE '1'               TO WS-ACCOUNT-5                         
083830           MOVE '0'               TO WS-ACCOUNT-5A                        
083840           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
083850           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
083860           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
083870           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
083880           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
083890         END-IF                                                           
083900         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
083910         COMPUTE R3-LINE-AMOUNT-LC =                                      
083920                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
083930         END-COMPUTE                                                      
083940         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
083950         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
083960         MOVE SPACE               TO WS-ALLOCATE-REF                      
083970         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
083980         PERFORM S02-WRITE-W51071A                                        
083990       END-IF                                                             
084000                                                                          
084010       IF SYST-IDSEKVNR = 2                                               
084020         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
084030         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
084040         IF KDPRODSL-LYNK                                                 
084050           MOVE '1'               TO WS-ACCOUNT-5                         
084060           MOVE '0'               TO WS-ACCOUNT-5A                        
084070           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
084080           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
084090           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
084100           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
084110           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
084120         END-IF                                                           
084130         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
084140         COMPUTE R3-LINE-AMOUNT-LC =                                      
084150                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
084160         END-COMPUTE                                                      
084170         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
084180         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
084190         MOVE SPACE               TO WS-ALLOCATE-REF                      
084200         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
084210         PERFORM S02-WRITE-W51071A                                        
084220       END-IF                                                             
084230     END-EVALUATE                                                         
084240     .                                                                    
084250     EJECT                                                                
084260                                                                          
084270 CES-MAIN-EVENT-503 SECTION.                                              
084280                                                                          
084290     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
084300     EVALUATE IN-EKH-KDEKSHT                                              
084310     WHEN '501'                                                           
084320          PERFORM CESA-SUB-EVENT-503-501                                  
084330     WHEN '502'                                                           
084340          PERFORM CESB-SUB-EVENT-503-502                                  
084350     WHEN '503'                                                           
084360          PERFORM CESB-SUB-EVENT-503-503                                  
084370     WHEN '504'                                                           
084380          PERFORM CESC-SUB-EVENT-503-504                                  
084390     END-EVALUATE                                                         
084400     .                                                                    
084410     EJECT                                                                
084420                                                                          
084430 CESA-SUB-EVENT-503-501 SECTION.                                          
084440     EVALUATE IN-EKH-KDEKNIVA                                             
084450     WHEN 'DET'                                                           
084460       IF SYST-IDSEKVNR = 1                                               
084470         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
084480         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
084490         IF KDPRODSL-LYNK                                                 
084500           MOVE '1'               TO WS-ACCOUNT-5                         
084510           MOVE '0'               TO WS-ACCOUNT-5A                        
084520           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
084530           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
084540           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
084550           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
084560           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
084570         END-IF                                                           
084580         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
084590         COMPUTE R3-LINE-AMOUNT-LC =                                      
084600                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
084610         END-COMPUTE                                                      
084620         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
084630         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
084640         MOVE SPACE               TO WS-ALLOCATE-REF                      
084650         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
084660         PERFORM S02-WRITE-W51071A                                        
084670       END-IF                                                             
084680                                                                          
084690       IF SYST-IDSEKVNR = 2                                               
084700         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
084710         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
084720         COMPUTE R3-LINE-AMOUNT-LC =                                      
084730                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
084740         END-COMPUTE                                                      
084750         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
084760         PERFORM S02-WRITE-W51071A                                        
084770       END-IF                                                             
084780     END-EVALUATE                                                         
084790     .                                                                    
084800     EJECT                                                                
084810                                                                          
084820 CESB-SUB-EVENT-503-502     SECTION.                                      
084830     EVALUATE IN-EKH-KDEKNIVA                                             
084840     WHEN 'DET'                                                           
084850       IF SYST-IDSEKVNR = 1                                               
084860* HÄR BOKAS ÖVERLEVERANS                                                  
084870         IF IN-EKH-KVANTAL > 0                                            
084880           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
084890           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
084900           IF KDPRODSL-LYNK                                               
084910             MOVE '1'               TO WS-ACCOUNT-5                       
084920             MOVE '0'               TO WS-ACCOUNT-5A                      
084930             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
084940             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
084950             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
084960             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
084970             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
084980           END-IF                                                         
084990           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
085000           COMPUTE R3-LINE-AMOUNT-LC =                                    
085010                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
085020           END-COMPUTE                                                    
085030           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
085040           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
085050           MOVE SPACE               TO WS-ALLOCATE-REF                    
085060           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
085070           IF DIST35-REFILL-VAT-EU                                        
085080             PERFORM S80-CHECK-TAX-CODE-3                                 
085090           END-IF                                                         
085100           PERFORM S02-WRITE-W51071A                                      
085110         END-IF                                                           
085120       END-IF                                                             
085130                                                                          
085140       IF SYST-IDSEKVNR = 2                                               
085150* HÄR BOKAS ÖVERLEVERANS                                                  
085160         IF IN-EKH-KVANTAL > 0                                            
085170           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
085180           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
085190           IF KDPRODSL-LYNK                                               
085200             MOVE '1'               TO WS-ACCOUNT-5                       
085210             MOVE '0'               TO WS-ACCOUNT-5A                      
085220             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
085230             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
085240             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
085250             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
085260             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
085270           END-IF                                                         
085280           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
085290           COMPUTE R3-LINE-AMOUNT-LC =                                    
085300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
085310           END-COMPUTE                                                    
085320           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
085330           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
085340           MOVE SPACE               TO WS-ALLOCATE-REF                    
085350           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
085360           IF DIST35-REFILL-VAT-EU                                        
085370             PERFORM S80-CHECK-TAX-CODE-4                                 
085380           END-IF                                                         
085390           PERFORM S02-WRITE-W51071A                                      
085400         END-IF                                                           
085410       END-IF                                                             
085420                                                                          
085430       IF DIST35-REFILL-VAT-NON-EU                                        
085440         IF SYST-IDSEKVNR = 3                                             
085450           IF IN-EKH-KVANTAL > 0                                          
085460             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
085470             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
085480             COMPUTE R3-LINE-AMOUNT-LC =                                  
085490                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
085500             END-COMPUTE                                                  
085510             MOVE IN-EKH-IDDC-REC  TO WS-ALLOCATE-DC                      
085520             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
085530             MOVE SPACE            TO WS-ALLOCATE-REF                     
085540             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
085550***          MOVE 'Y3'             TO R3-LINE-TAX-CODE                    
085560             PERFORM S80-CHECK-TAX-CODE-3                                 
085570             PERFORM S02-WRITE-W51071A                                    
085580           END-IF                                                         
085590         END-IF                                                           
085600                                                                          
085610         IF SYST-IDSEKVNR = 4                                             
085620           IF IN-EKH-KVANTAL > 0                                          
085630             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
085640             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
085650             COMPUTE R3-LINE-AMOUNT-LC =                                  
085660                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
085670             END-COMPUTE                                                  
085680             MOVE IN-EKH-IDDC-SEND TO WS-ALLOCATE-DC                      
085690             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
085700             MOVE SPACE            TO WS-ALLOCATE-REF                     
085710             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
085720***          MOVE '90'             TO R3-LINE-TAX-CODE                    
085730             PERFORM S80-CHECK-TAX-CODE-4                                 
085740             PERFORM S02-WRITE-W51071A                                    
085750           END-IF                                                         
085760         END-IF                                                           
085770       END-IF                                                             
085780     END-EVALUATE                                                         
085790     .                                                                    
085800     EJECT                                                                
085810                                                                          
085820 CESB-SUB-EVENT-503-503 SECTION.                                          
085830     EVALUATE IN-EKH-KDEKNIVA                                             
085840     WHEN 'DET'                                                           
085850       IF SYST-IDSEKVNR = 1                                               
085860* HÄR BOKAS UNDERLEVERANS                                                 
085870         IF IN-EKH-KVANTAL < 0                                            
085880           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
085890           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
085900           IF KDPRODSL-LYNK                                               
085910             MOVE '1'               TO WS-ACCOUNT-5                       
085920             MOVE '0'               TO WS-ACCOUNT-5A                      
085930             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
085940             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
085950             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
085960             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
085970             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
085980           END-IF                                                         
085990           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
086000           COMPUTE R3-LINE-AMOUNT-LC =                                    
086010                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
086020           END-COMPUTE                                                    
086030           MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                     
086040           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
086050           MOVE SPACE               TO WS-ALLOCATE-REF                    
086060           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
086070           IF DIST35-REFILL-VAT-EU                                        
086080             PERFORM S80-CHECK-TAX-CODE-3                                 
086090           END-IF                                                         
086100           PERFORM S02-WRITE-W51071A                                      
086110         END-IF                                                           
086120       END-IF                                                             
086130                                                                          
086140       IF SYST-IDSEKVNR = 2                                               
086150* HÄR BOKAS UNDERLEVERANS                                                 
086160         IF IN-EKH-KVANTAL < 0                                            
086170           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
086180           MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                         
086190           IF KDPRODSL-LYNK                                               
086200             MOVE '1'               TO WS-ACCOUNT-5                       
086210             MOVE '0'               TO WS-ACCOUNT-5A                      
086220             MOVE 'LYNK'            TO WS-PRCTR(1:4)                      
086230             MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP               
086240             MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                   
086250             MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER              
086260             MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                   
086270           END-IF                                                         
086280           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
086290           COMPUTE R3-LINE-AMOUNT-LC =                                    
086300                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD                       
086310           END-COMPUTE                                                    
086320           MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                     
086330           MOVE SPACE               TO WS-ALLOCATE-DISTR                  
086340           MOVE SPACE               TO WS-ALLOCATE-REF                    
086350           MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                   
086360           IF DIST35-REFILL-VAT-EU                                        
086370             PERFORM S80-CHECK-TAX-CODE-4                                 
086380           END-IF                                                         
086390           PERFORM S02-WRITE-W51071A                                      
086400         END-IF                                                           
086410       END-IF                                                             
086420                                                                          
086430       IF DIST35-REFILL-VAT-NON-EU                                        
086440         IF SYST-IDSEKVNR = 3                                             
086450           IF IN-EKH-KVANTAL < 0                                          
086460             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
086470             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
086480             COMPUTE R3-LINE-AMOUNT-LC =                                  
086490                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
086500             END-COMPUTE                                                  
086510             MOVE IN-EKH-IDDC-SEND TO WS-ALLOCATE-DC                      
086520             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
086530             MOVE SPACE            TO WS-ALLOCATE-REF                     
086540             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
086550***          MOVE '90'             TO R3-LINE-TAX-CODE                    
086560             PERFORM S80-CHECK-TAX-CODE-4                                 
086570             PERFORM S02-WRITE-W51071A                                    
086580           END-IF                                                         
086590         END-IF                                                           
086600                                                                          
086610         IF SYST-IDSEKVNR = 4                                             
086620           IF IN-EKH-KVANTAL < 0                                          
086630             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
086640             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
086650             COMPUTE R3-LINE-AMOUNT-LC =                                  
086660                     IN-EKH-KVANTAL * IN-EKH-PRARTSTD                     
086670             END-COMPUTE                                                  
086680             MOVE IN-EKH-IDDC-REC  TO WS-ALLOCATE-DC                      
086690             MOVE SPACE            TO WS-ALLOCATE-DISTR                   
086700             MOVE SPACE            TO WS-ALLOCATE-REF                     
086710             MOVE WS-ALLOCATE      TO R3-LINE-ALLOCATE                    
086720***          MOVE 'Y3'             TO R3-LINE-TAX-CODE                    
086730             PERFORM S80-CHECK-TAX-CODE-3                                 
086740             PERFORM S02-WRITE-W51071A                                    
086750           END-IF                                                         
086760         END-IF                                                           
086770       END-IF                                                             
086780     END-EVALUATE                                                         
086790     .                                                                    
086800     EJECT                                                                
086810                                                                          
086820 CESC-SUB-EVENT-503-504 SECTION.                                          
086830     EVALUATE IN-EKH-KDEKNIVA                                             
086840     WHEN 'DET'                                                           
086850       IF SYST-IDSEKVNR = 1                                               
086860         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
086870         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
086880         IF KDPRODSL-LYNK                                                 
086890           MOVE '1'               TO WS-ACCOUNT-5                         
086900           MOVE '0'               TO WS-ACCOUNT-5A                        
086910           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
086920           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
086930           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
086940           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
086950           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
086960         END-IF                                                           
086970         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
086980         COMPUTE R3-LINE-AMOUNT-LC =                                      
086990                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
087000         END-COMPUTE                                                      
087010         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
087020         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
087030         MOVE SPACE               TO WS-ALLOCATE-REF                      
087040         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
087050         PERFORM S02-WRITE-W51071A                                        
087060       END-IF                                                             
087070                                                                          
087080       IF SYST-IDSEKVNR = 2                                               
087090         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
087100         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
087110         COMPUTE R3-LINE-AMOUNT-LC =                                      
087120                 IN-EKH-KVANTAL * IN-EKH-PRARTSTD                         
087130         END-COMPUTE                                                      
087140         MOVE SYST-IDANALYS       TO R3-LINE-ORDER                        
087150         PERFORM S02-WRITE-W51071A                                        
087160       END-IF                                                             
087170     END-EVALUATE                                                         
087180     .                                                                    
087190     EJECT                                                                
087200                                                                          
087210 CEU-MAIN-EVENT-505 SECTION.                                              
087220                                                                          
087230     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
087240     EVALUATE IN-EKH-KDEKSHT                                              
087250     WHEN '501'                                                           
087260          PERFORM CEUA-SUB-EVENT-505-501                                  
087270     END-EVALUATE                                                         
087280     .                                                                    
087290     EJECT                                                                
087300                                                                          
087310 CEUA-SUB-EVENT-505-501 SECTION.                                          
087320                                                                          
087330     EVALUATE IN-EKH-KDEKNIVA                                             
087340     WHEN 'DET'                                                           
087350       IF SYST-IDSEKVNR = 1                                               
087360         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
087370         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
087380         IF KDPRODSL-LYNK                                                 
087390           MOVE '1'               TO WS-ACCOUNT-5                         
087400           MOVE '0'               TO WS-ACCOUNT-5A                        
087410           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
087420           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
087430           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
087440           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
087450           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
087460         END-IF                                                           
087470         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
087480         COMPUTE R3-LINE-AMOUNT-LC =                                      
087490                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
087500         END-COMPUTE                                                      
087510         MOVE IN-EKH-IDDC-REC     TO WS-ALLOCATE-DC                       
087520         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
087530         MOVE SPACE               TO WS-ALLOCATE-REF                      
087540         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
087550         PERFORM S02-WRITE-W51071A                                        
087560       END-IF                                                             
087570                                                                          
087580       IF SYST-IDSEKVNR = 2                                               
087590         MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                     
087600         MOVE WS-R3-ACCOUNT-10    TO WS-ACCOUNT                           
087610         IF KDPRODSL-LYNK                                                 
087620           MOVE '1'               TO WS-ACCOUNT-5                         
087630           MOVE '0'               TO WS-ACCOUNT-5A                        
087640           MOVE 'LYNK'            TO WS-PRCTR(1:4)                        
087650           MOVE IN-EKH-KDPRODSL   TO WS-PRCTR-PRODSL-DISP                 
087660           MOVE WS-PRCTR-PRODSL-DISP TO WS-PRCTR(5:2)                     
087670           MOVE WS-PRCTR          TO R3-LINE-PROFIT-CENTER                
087680           MOVE WS-ACCOUNT        TO WS-R3-ACCOUNT-10                     
087690         END-IF                                                           
087700         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
087710         COMPUTE R3-LINE-AMOUNT-LC =                                      
087720                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
087730         END-COMPUTE                                                      
087740         MOVE IN-EKH-IDDC-SEND    TO WS-ALLOCATE-DC                       
087750         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
087760         MOVE SPACE               TO WS-ALLOCATE-REF                      
087770         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
087780         IF DIST28-CDC-BE25                                               
087790         OR DIST28-CDC-ES25                                               
087800           MOVE '70'              TO R3-LINE-TAX-CODE                     
087810         END-IF                                                           
087820         IF DIST28-CDC-GB25                                               
087830         OR DIST28-CDC-NO25                                               
087840           MOVE '90'              TO R3-LINE-TAX-CODE                     
087850         END-IF                                                           
087860         PERFORM S02-WRITE-W51071A                                        
087870       END-IF                                                             
087880                                                                          
087890     END-EVALUATE                                                         
087900     .                                                                    
087910     EJECT                                                                
087920                                                                          
087930 CET-MAIN-EVENT-205 SECTION.                                              
087940     EVALUATE IN-EKH-KDEKSHT                                              
087950     WHEN '201'                                                           
087960          PERFORM CETA-SUB-EVENT-205-201                                  
087970     WHEN '202'                                                           
087980          PERFORM CETB-SUB-EVENT-205-202                                  
087990     END-EVALUATE                                                         
088000     .                                                                    
088010     EJECT                                                                
088020                                                                          
088030 CETA-SUB-EVENT-205-201 SECTION.                                          
088040     EVALUATE IN-EKH-KDEKNIVA                                             
088050     WHEN 'FRAKT'                                                         
088060       IF BET-KDTRADP(3:2) NOT = SPACE                                    
088070         IF SYST-IDSEKVNR = 1                                             
088080           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
088090           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
088100           MOVE SYST-IDKST      TO WS-RED-IDKST                           
088110           MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                    
088120           MOVE IN-EKH-SUBEL    TO R3-LINE-AMOUNT-LC                      
088130           MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                       
088140           MOVE SPACE           TO WS-ALLOCATE-DC                         
088150           MOVE SPACE           TO WS-ALLOCATE-DISTR                      
088160           MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                        
088170           MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                       
088180           PERFORM S04-WRITE-W51073A                                      
088190         END-IF                                                           
088200       ELSE                                                               
088210         IF SYST-IDSEKVNR = 2                                             
088220           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
088230           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
088240           MOVE SYST-IDKST      TO WS-RED-IDKST                           
088250           MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                    
088260           MOVE IN-EKH-SUBEL    TO R3-LINE-AMOUNT-LC                      
088270           MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                       
088280           MOVE SPACE           TO WS-ALLOCATE-DC                         
088290           MOVE SPACE           TO WS-ALLOCATE-DISTR                      
088300           MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                        
088310           MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                       
088320           PERFORM S04-WRITE-W51073A                                      
088330         END-IF                                                           
088340       END-IF                                                             
088350                                                                          
088360     WHEN 'DET'                                                           
088370       IF BET-KDTRADP(3:2) NOT = SPACE                                    
088380         IF SYST-IDSEKVNR = 1                                             
088390           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
088400           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
088410           MOVE SYST-IDKST        TO WS-RED-IDKST                         
088420           MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                  
088430           COMPUTE R3-LINE-AMOUNT-LC =                                    
088440                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
088450           END-COMPUTE                                                    
088460           MOVE IN-EKH-BEVAT      TO R3-LINE-TAX-CODE                     
088470           MOVE SPACE             TO WS-ALLOCATE-DC                       
088480           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
088490           MOVE IN-EKH-IDREF      TO WS-ALLOCATE-REF                      
088500           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
088510           PERFORM S04-WRITE-W51073A                                      
088520         END-IF                                                           
088530       ELSE                                                               
088540         IF SYST-IDSEKVNR = 2                                             
088550           MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                     
088560           MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
088570           MOVE SYST-IDKST        TO WS-RED-IDKST                         
088580           MOVE WS-RED-IDKST      TO R3-LINE-COST-CENTER                  
088590           COMPUTE R3-LINE-AMOUNT-LC =                                    
088600                   IN-EKH-KVANTAL * IN-EKH-PRARTNTO                       
088610           END-COMPUTE                                                    
088620           MOVE IN-EKH-BEVAT      TO R3-LINE-TAX-CODE                     
088630           MOVE SPACE             TO WS-ALLOCATE-DC                       
088640           MOVE SPACE             TO WS-ALLOCATE-DISTR                    
088650           MOVE IN-EKH-IDREF      TO WS-ALLOCATE-REF                      
088660           MOVE WS-ALLOCATE       TO R3-LINE-ALLOCATE                     
088670           PERFORM S04-WRITE-W51073A                                      
088680         END-IF                                                           
088690       END-IF                                                             
088700     END-EVALUATE                                                         
088710     .                                                                    
088720     EJECT                                                                
088730                                                                          
088740 CETB-SUB-EVENT-205-202 SECTION.                                          
088750     EVALUATE IN-EKH-KDEKNIVA                                             
088760     WHEN 'FRAKT'                                                         
088770       IF BET-KDTRADP(3:2) NOT = SPACE                                    
088780         IF SYST-IDSEKVNR = 1                                             
088790           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
088800           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
088810           MOVE SYST-IDKST      TO WS-RED-IDKST                           
088820           MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                    
088830           MOVE IN-EKH-SUBEL    TO R3-LINE-AMOUNT-LC                      
088840           MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                       
088850           MOVE SPACE           TO WS-ALLOCATE-DC                         
088860           MOVE SPACE           TO WS-ALLOCATE-DISTR                      
088870           MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                        
088880           MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                       
088890           PERFORM S04-WRITE-W51073A                                      
088900         END-IF                                                           
088910       ELSE                                                               
088920         IF SYST-IDSEKVNR = 2                                             
088930           MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                       
088940           MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                        
088950           MOVE SYST-IDKST      TO WS-RED-IDKST                           
088960           MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                    
088970           MOVE IN-EKH-SUBEL    TO R3-LINE-AMOUNT-LC                      
088980           MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                       
088990           MOVE SPACE           TO WS-ALLOCATE-DC                         
089000           MOVE SPACE           TO WS-ALLOCATE-DISTR                      
089010           MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                        
089020           MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                       
089030           PERFORM S04-WRITE-W51073A                                      
089040         END-IF                                                           
089050       END-IF                                                             
089060                                                                          
089070     WHEN 'DET'                                                           
089080       IF IN-EKH-IDTRANS = 'WIFI'                                         
089090         IF BET-KDTRADP(3:2) NOT = SPACE                                  
089100           IF SYST-IDSEKVNR = 1                                           
089110             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
089120             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
089130             MOVE SYST-IDKST      TO WS-RED-IDKST                         
089140             MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                  
089150             COMPUTE R3-LINE-AMOUNT-LC =                                  
089160                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
089170             END-COMPUTE                                                  
089180             MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                     
089190             MOVE SPACE           TO WS-ALLOCATE-DC                       
089200             MOVE SPACE           TO WS-ALLOCATE-DISTR                    
089210             MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                      
089220             MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                     
089230             PERFORM S04-WRITE-W51073A                                    
089240           END-IF                                                         
089250         ELSE                                                             
089260           IF SYST-IDSEKVNR = 2                                           
089270             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
089280             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
089290             MOVE SYST-IDKST      TO WS-RED-IDKST                         
089300             MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                  
089310             COMPUTE R3-LINE-AMOUNT-LC =                                  
089320                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
089330             END-COMPUTE                                                  
089340             MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                     
089350             MOVE SPACE           TO WS-ALLOCATE-DC                       
089360             MOVE SPACE           TO WS-ALLOCATE-DISTR                    
089370             MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                      
089380             MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                     
089390             PERFORM S04-WRITE-W51073A                                    
089400           END-IF                                                         
089410         END-IF                                                           
089420       END-IF                                                             
089430                                                                          
089440       IF IN-EKH-IDTRANS = 'VIDA'                                         
089450         IF BET-KDTRADP(3:2) NOT = SPACE                                  
089460           IF SYST-IDSEKVNR = 3                                           
089470             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
089480             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
089490             MOVE SYST-IDKST      TO WS-RED-IDKST                         
089500             MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                  
089510             COMPUTE R3-LINE-AMOUNT-LC =                                  
089520                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
089530             END-COMPUTE                                                  
089540             MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                     
089550             MOVE SPACE           TO WS-ALLOCATE-DC                       
089560             MOVE SPACE           TO WS-ALLOCATE-DISTR                    
089570             MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                      
089580             MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                     
089590             PERFORM S04-WRITE-W51073A                                    
089600           END-IF                                                         
089610         ELSE                                                             
089620           IF SYST-IDSEKVNR = 4                                           
089630             MOVE SYST-IDKONTO    TO WS-R3-ACCOUNT-10                     
089640             MOVE WS-R3-ACCOUNT-6 TO R3-LINE-ACCOUNT                      
089650             MOVE SYST-IDKST      TO WS-RED-IDKST                         
089660             MOVE WS-RED-IDKST    TO R3-LINE-COST-CENTER                  
089670             COMPUTE R3-LINE-AMOUNT-LC =                                  
089680                     IN-EKH-KVANTAL * IN-EKH-PRARTNTO                     
089690             END-COMPUTE                                                  
089700             MOVE IN-EKH-BEVAT    TO R3-LINE-TAX-CODE                     
089710             MOVE SPACE           TO WS-ALLOCATE-DC                       
089720             MOVE SPACE           TO WS-ALLOCATE-DISTR                    
089730             MOVE IN-EKH-IDREF    TO WS-ALLOCATE-REF                      
089740             MOVE WS-ALLOCATE     TO R3-LINE-ALLOCATE                     
089750             PERFORM S04-WRITE-W51073A                                    
089760           END-IF                                                         
089770         END-IF                                                           
089780       END-IF                                                             
089790     END-EVALUATE                                                         
089800     .                                                                    
089810     EJECT                                                                
089820                                                                          
089830 CEU-MAIN-EVENT-304 SECTION.                                              
089840     EVALUATE IN-EKH-KDEKSHT                                              
089850     WHEN '301'                                                           
089860          PERFORM CEUA-SUB-EVENT-304-301                                  
089870     END-EVALUATE                                                         
089880     .                                                                    
089890     EJECT                                                                
089900                                                                          
089910 CEUA-SUB-EVENT-304-301 SECTION.                                          
089920     EVALUATE IN-EKH-KDEKNIVA                                             
089930     WHEN 'FRAKT'                                                         
089940       MOVE SYST-IDKONTO          TO WS-R3-ACCOUNT-10                     
089950       MOVE WS-R3-ACCOUNT-6       TO R3-LINE-ACCOUNT                      
089960       MOVE SYST-IDKST            TO WS-RED-IDKST                         
089970       MOVE WS-RED-IDKST          TO R3-LINE-COST-CENTER                  
089980       MOVE IN-EKH-SUBEL          TO R3-LINE-AMOUNT-LC                    
089990       MOVE IN-EKH-BEVAT          TO R3-LINE-TAX-CODE                     
090000       MOVE SPACE                 TO WS-ALLOCATE-DC                       
090010       MOVE SPACE                 TO WS-ALLOCATE-DISTR                    
090020       MOVE IN-EKH-IDREF          TO WS-ALLOCATE-REF                      
090030       MOVE WS-ALLOCATE           TO R3-LINE-ALLOCATE                     
090040       PERFORM S04-WRITE-W51073A                                          
090050                                                                          
090060     WHEN 'DET'                                                           
090070       IF SYST-IDSEKVNR = 1                                               
090080         MOVE IN-EKH-IDKONTO      TO WS-R3-ACCOUNT-10                     
090090         MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                      
090100         MOVE SYST-IDKST          TO WS-RED-IDKST                         
090110         MOVE WS-RED-IDKST        TO R3-LINE-COST-CENTER                  
090120         COMPUTE R3-LINE-AMOUNT-LC =                                      
090130                 IN-EKH-KVANTAL * IN-EKH-PRARTNTO                         
090140         END-COMPUTE                                                      
090150         MOVE IN-EKH-BEVAT        TO R3-LINE-TAX-CODE                     
090160         MOVE SPACE               TO WS-ALLOCATE-DC                       
090170         MOVE SPACE               TO WS-ALLOCATE-DISTR                    
090180         MOVE IN-EKH-IDREF        TO WS-ALLOCATE-REF                      
090190         MOVE WS-ALLOCATE         TO R3-LINE-ALLOCATE                     
090200         PERFORM S04-WRITE-W51073A                                        
090210       END-IF                                                             
090220     END-EVALUATE                                                         
090230     .                                                                    
090240     EJECT                                                                
090250                                                                          
090260 CF-BUILD-COMMON-310-PART SECTION.                                        
090270     MOVE SPACE              TO R3-LINE-R3                                
090280     MOVE ZERO               TO R3-LINE-VALUE-DATE                        
090290                                R3-LINE-DUE-DATE                          
090300                                R3-LINE-AMOUNT                            
090310                                R3-LINE-AMOUNT-LC                         
090320                                R3-LINE-TAX-AMOUNT                        
090330                                R3-LINE-TAX-AMOUNT-LC                     
090340                                R3-LINE-NUMBER-OF-DAYS                    
090350                                R3-LINE-QUANTITY                          
090360                                R3-LINE-SAMNR                             
090370     MOVE SYST-IDPTYP        TO R3-LINE-RECORD-TYPE                       
090380     MOVE SYST-KDPOST        TO R3-LINE-POSTING-KEY                       
090390     MOVE 'SEPV'             TO R3-LINE-COMPANY-CODE                      
090400     MOVE IN-EKH-IDVERGL     TO R3-LINE-DOCUMENT-NO-REF                   
090410     MOVE IN-EKH-IDFAKT-EXP  TO R3-LINE-REFKEY3                           
090420     IF SYST-KDPOST = '11'                                                
090430       MOVE '-'              TO R3-LINE-AMOUNT-SIGN                       
090440     ELSE                                                                 
090450       MOVE '+'              TO R3-LINE-AMOUNT-SIGN                       
090460     END-IF                                                               
090470     .                                                                    
090480     EJECT                                                                
090490                                                                          
090500 CG-SCHEDULE-LINE-AR SECTION.                                             
090510     MOVE NEJ                     TO WS-HEADER-SW                         
090520     MOVE JA                      TO WS-LINE-SW                           
090530     EVALUATE IN-EKH-KDEKHHT                                              
090540     WHEN '102'                                                           
090550     WHEN '204'                                                           
090560* HANDLING FEE KOMMER IN MED LOKAL VALUTA, RÄKNA OM TILL SEK              
090570        IF IN-EKH-KDEKSHT = '204'                                         
090580          PERFORM CGA-MAIN-EVENT-204-204                                  
090590        ELSE                                                              
090600          PERFORM CGA-MAIN-EVENT-204-303                                  
090610        END-IF                                                            
090620     WHEN '205'                                                           
090630          PERFORM CGB-MAIN-EVENT-205-304                                  
090640     WHEN '303'                                                           
090650                                                                          
090660* KUNDRESKONTRAPOST SKALL BARA SKAPAS FRÅN PGM W4183300/W4184500          
090670       IF IN-FIL-IDPGM = 'W4183300' OR 'W4184500'                         
090680         PERFORM CGA-MAIN-EVENT-204-303                                   
090690       END-IF                                                             
090700     WHEN '304'                                                           
090710          PERFORM CGB-MAIN-EVENT-205-304                                  
090720     END-EVALUATE                                                         
090730     .                                                                    
090740     EJECT                                                                
090750                                                                          
090760 CGA-MAIN-EVENT-204-303 SECTION.                                          
090770     EVALUATE IN-EKH-KDEKNIVA                                             
090780     WHEN 'SUM'                                                           
090790       IF SYST-IDSEKVNR = 1                                               
090800         MOVE W-BET-IDPARTNR-NUM   TO R3-LINE-ACCOUNT                     
090810         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
090820         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
090830                 R3-LINE-AMOUNT-LC / WS-PRKURS                            
090840         END-COMPUTE                                                      
090850         PERFORM S10-VATCODE                                              
090860         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
090870         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
090880                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                        
090890         END-COMPUTE                                                      
090900         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
090910                                                                          
090920**** FOR EXTENDED WARRANTY GERMANY SHOULD HAVE IPT AND NOT VAT            
090930*        IF  IN-EKH-IDDISTR = 2278                                        
090940*        AND IN-EKH-KDEKHHT = '204'                                       
090950*        AND IN-EKH-KDEKSHT = '205'                                       
090960*          MOVE ZERO TO R3-LINE-TAX-AMOUNT-LC                             
090970*          MOVE ZERO TO R3-LINE-TAX-AMOUNT                                
090980*        END-IF                                                           
090990                                                                          
091000         PERFORM S04-WRITE-W51073A                                        
091010                                                                          
091020**** FOR EXTENDED WARRANTY GERMANY SHOULD HAVE IPT AND NOT VAT            
091030*        IF  IN-EKH-IDDISTR = 2278                                        
091040*        AND IN-EKH-KDEKHHT = '204'                                       
091050*        AND IN-EKH-KDEKSHT = '205'                                       
091060*          PERFORM CD-BUILD-COMMON-610-PART                               
091070*          MOVE '244023'          TO WS-R3-ACCOUNT-10                     
091080*          MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                      
091090*          MOVE IN-EKH-SUVAT      TO R3-LINE-AMOUNT-LC                    
091100*          MOVE '50'              TO R3-LINE-POSTING-KEY                  
091110*          MOVE '610'             TO R3-LINE-RECORD-TYPE                  
091120*          MOVE '-'               TO R3-LINE-AMOUNT-SIGN                  
091130*          PERFORM S03-WRITE-W51072                                       
091140*        ELSE                                                             
091150*          CONTINUE                                                       
091160*        END-IF                                                           
091170       END-IF                                                             
091180     END-EVALUATE                                                         
091190     .                                                                    
091200     EJECT                                                                
091210                                                                          
091220 CGA-MAIN-EVENT-204-204 SECTION.                                          
091230     EVALUATE IN-EKH-KDEKNIVA                                             
091240     WHEN 'SUM'                                                           
091250       IF SYST-IDSEKVNR = 1                                               
091260         IF WS-KDVALISO = 'SEK'                                           
091270           MOVE 1 TO WS-PRKURS                                            
091280         END-IF                                                           
091290         MOVE W-BET-IDPARTNR-NUM   TO R3-LINE-ACCOUNT                     
091300**** HANDLING FEE ADJUSTMENT FOR CURRENCY NOT SEK                         
091310**** DEALER NET MARKET                                                    
091320         IF WS-KDVALISO-HUV NOT = IN-EKH-KDVALISO                         
091330           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
091340           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
091350                   R3-LINE-AMOUNT    * WS-PRKURS                          
091360           COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 -                  
091370                   R3-LINE-AMOUNT-LC                                      
091380           PERFORM S10-VATCODE                                            
091390           IF IN-EKH-SUVAT > ZERO                                         
091400             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
091410             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
091420                     R3-LINE-TAX-AMOUNT  * WS-PRKURS                      
091430             COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 +                
091440                     R3-LINE-TAX-AMOUNT-LC                                
091450             MOVE WS-KDBETVIL      TO R3-LINE-PAYTERMS                    
091460           END-IF                                                         
091470         ELSE                                                             
091480**** HANDLING FEE ADJUSTMENTS FOR PARTNERS NOT SEK                        
091490**** BUT IS PRICED IN SEK                                                 
091500           IF WS-KDVALISO-HUV NOT = WS-KDVALISO                           
091510             MOVE IN-EKH-SUBEL     TO R3-LINE-AMOUNT-LC                   
091520             COMPUTE R3-LINE-AMOUNT    ROUNDED =                          
091530                     R3-LINE-AMOUNT-LC / WS-PRKURS                        
091540             COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 -                
091550                     R3-LINE-AMOUNT-LC                                    
091560             PERFORM S10-VATCODE                                          
091570             IF IN-EKH-SUVAT > ZERO                                       
091580               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT-LC               
091590               COMPUTE R3-LINE-TAX-AMOUNT   ROUNDED =                     
091600                       R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                  
091610               COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 +              
091620                       R3-LINE-TAX-AMOUNT-LC                              
091630               MOVE WS-KDBETVIL    TO R3-LINE-PAYTERMS                    
091640             END-IF                                                       
091650           ELSE                                                           
091660**** HANDLING FEE ADJUSTMENTS FOR PARTNERS IN SEK                         
091670             MOVE IN-EKH-SUBEL     TO R3-LINE-AMOUNT                      
091680             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
091690                     R3-LINE-AMOUNT * WS-PRKURS                           
091700             COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 -                
091710                     R3-LINE-AMOUNT-LC                                    
091720             PERFORM S10-VATCODE                                          
091730             IF IN-EKH-SUVAT > ZERO                                       
091740               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
091750               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
091760                       R3-LINE-TAX-AMOUNT * WS-PRKURS                     
091770               COMPUTE WS-SUMMA-204-204 = WS-SUMMA-204-204 +              
091780                       R3-LINE-TAX-AMOUNT-LC                              
091790               MOVE WS-KDBETVIL    TO R3-LINE-PAYTERMS                    
091800             END-IF                                                       
091810           END-IF                                                         
091820         END-IF                                                           
091830                                                                          
091840         PERFORM S04-WRITE-W51073A                                        
091850       END-IF                                                             
091860     END-EVALUATE                                                         
091870     .                                                                    
091880     EJECT                                                                
091890                                                                          
091900 CGB-MAIN-EVENT-205-304 SECTION.                                          
091910     EVALUATE IN-EKH-KDEKNIVA                                             
091920     WHEN 'SUM'                                                           
091930       IF SYST-IDSEKVNR = 1                                               
091940         MOVE W-BET-IDPARTNR-NUM   TO R3-LINE-ACCOUNT                     
091950         MOVE IN-EKH-SUBEL         TO R3-LINE-AMOUNT-LC                   
091960         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
091970                 R3-LINE-AMOUNT-LC / WS-PRKURS                            
091980         END-COMPUTE                                                      
091990         MOVE IN-EKH-BEVAT         TO R3-LINE-TAX-CODE                    
092000         MOVE IN-EKH-SUVAT         TO R3-LINE-TAX-AMOUNT-LC               
092010         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
092020                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                        
092030         END-COMPUTE                                                      
092040         MOVE WS-KDBETVIL          TO R3-LINE-PAYTERMS                    
092050         MOVE IN-EKH-IDREF         TO WS-ALLOCATE-ORDER                   
092060         MOVE WS-ALLOCATE-TIS      TO R3-LINE-ALLOCATE                    
092070                                                                          
092080         IF WS-KDVALISO = 'JPY'                                           
092090           COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-AMOUNT * 1              
092100           MOVE WS-SUNTO-JPY TO R3-LINE-AMOUNT                            
092110           COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-TAX-AMOUNT * 1          
092120           MOVE WS-SUNTO-JPY TO R3-LINE-TAX-AMOUNT                        
092130         END-IF                                                           
092140         PERFORM S04-WRITE-W51073A                                        
092150       END-IF                                                             
092160     END-EVALUATE                                                         
092170     .                                                                    
092180     EJECT                                                                
092190                                                                          
092200 CH-BUILD-COMMON-210-PART SECTION.                                        
092210     MOVE SPACE               TO R3-LINE-R3                               
092220                                                                          
092230     MOVE ZERO                TO R3-LINE-VALUE-DATE                       
092240                                 R3-LINE-DUE-DATE                         
092250                                 R3-LINE-AMOUNT                           
092260                                 R3-LINE-AMOUNT-LC                        
092270                                 R3-LINE-TAX-AMOUNT                       
092280                                 R3-LINE-TAX-AMOUNT-LC                    
092290                                 R3-LINE-NUMBER-OF-DAYS                   
092300                                 R3-LINE-QUANTITY                         
092310                                 R3-LINE-NUMBER-OF-DAYS                   
092320                                 R3-LINE-SAMNR                            
092330     MOVE SYST-KDPOST         TO R3-LINE-POSTING-KEY                      
092340     MOVE SYST-IDPTYP         TO R3-LINE-RECORD-TYPE                      
092350     MOVE 'SEPV'              TO R3-LINE-COMPANY-CODE                     
092360     MOVE IN-EKH-IDVERGL      TO R3-LINE-DOCUMENT-NO-REF                  
092370     MOVE IN-EKH-IDFAKT-EXP   TO R3-LINE-REFKEY3                          
092380     IF SYST-KDPOST = '31'                                                
092390       MOVE '-'               TO R3-LINE-AMOUNT-SIGN                      
092400     ELSE                                                                 
092410       MOVE '+'               TO R3-LINE-AMOUNT-SIGN                      
092420     END-IF                                                               
092430     .                                                                    
092440     EJECT                                                                
092450                                                                          
092460 CI-SCHEDULE-LINE-AP SECTION.                                             
092470     MOVE NEJ                TO WS-HEADER-SW                              
092480     MOVE JA                 TO WS-LINE-SW                                
092490     EVALUATE IN-EKH-KDEKHHT                                              
092500     WHEN '102'                                                           
092510        IF IN-EKH-KDEKSHT = '120'                                         
092520          PERFORM CIC-MAIN-EVENT-102-120                                  
092530        ELSE                                                              
092540          IF IN-EKH-KDEKSHT = '135'                                       
092550            PERFORM CID-MAIN-EVENT-102-135                                
092560          ELSE                                                            
092570            IF IN-EKH-KDEKSHT = '145'                                     
092580              PERFORM CIE-MAIN-EVENT-102-145                              
092590            ELSE                                                          
092600              PERFORM CIA-MAIN-EVENT-102                                  
092610            END-IF                                                        
092620          END-IF                                                          
092630        END-IF                                                            
092640     WHEN '202'                                                           
092650          PERFORM CIB-MAIN-EVENT-202                                      
092660     END-EVALUATE                                                         
092670     .                                                                    
092680     EJECT                                                                
092690                                                                          
092700 CIA-MAIN-EVENT-102 SECTION.                                              
092710     EVALUATE IN-EKH-KDEKNIVA                                             
092720     WHEN 'SUM'                                                           
092730       IF SYST-IDSEKVNR = 1                                               
092740         IF IN-EKH-SUBEL > 0                                              
092750           MOVE IN-EKH-IDLEVNR      TO R3-LINE-ACCOUNT                    
092760           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
092770           COMPUTE R3-LINE-AMOUNT ROUNDED =                               
092780                   R3-LINE-AMOUNT-LC / WS-PRKURS                          
092790           END-COMPUTE                                                    
092800           PERFORM S10-VATCODE                                            
092810           MOVE IN-EKH-SUVAT        TO R3-LINE-TAX-AMOUNT-LC              
092820           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
092830                   R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                      
092840           END-COMPUTE                                                    
092850           IF WS-KDVALISO = 'JPY'                                         
092860             COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-AMOUNT * 1            
092870             MOVE WS-SUNTO-JPY TO R3-LINE-AMOUNT                          
092880             COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-TAX-AMOUNT * 1        
092890             MOVE WS-SUNTO-JPY TO R3-LINE-TAX-AMOUNT                      
092900           END-IF                                                         
092910           PERFORM S04-WRITE-W51073A                                      
092920         END-IF                                                           
092930       END-IF                                                             
092940                                                                          
092950       IF SYST-IDSEKVNR = 2                                               
092960         IF IN-EKH-SUBEL < 0                                              
092970           MOVE IN-EKH-IDLEVNR      TO R3-LINE-ACCOUNT                    
092980           MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                  
092990           COMPUTE R3-LINE-AMOUNT ROUNDED =                               
093000                   R3-LINE-AMOUNT-LC / WS-PRKURS                          
093010           END-COMPUTE                                                    
093020           PERFORM S10-VATCODE                                            
093030           MOVE IN-EKH-SUVAT        TO R3-LINE-TAX-AMOUNT-LC              
093040           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
093050                   R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                      
093060           END-COMPUTE                                                    
093070           IF WS-KDVALISO = 'JPY'                                         
093080             COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-AMOUNT * 1            
093090             MOVE WS-SUNTO-JPY TO R3-LINE-AMOUNT                          
093100             COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-TAX-AMOUNT * 1        
093110             MOVE WS-SUNTO-JPY TO R3-LINE-TAX-AMOUNT                      
093120           END-IF                                                         
093130           PERFORM S04-WRITE-W51073A                                      
093140         END-IF                                                           
093150       END-IF                                                             
093160                                                                          
093170     END-EVALUATE                                                         
093180     .                                                                    
093190     EJECT                                                                
093200                                                                          
093210 CIB-MAIN-EVENT-202 SECTION.                                              
093220     EVALUATE IN-EKH-KDEKNIVA                                             
093230     WHEN 'SUM'                                                           
093240       IF SYST-IDSEKVNR = 1                                               
093250         MOVE W-BET-IDPARTNR-NUM  TO R3-LINE-ACCOUNT                      
093260         MOVE IN-EKH-SUBEL        TO R3-LINE-AMOUNT-LC                    
093270         COMPUTE R3-LINE-AMOUNT ROUNDED =                                 
093280                 R3-LINE-AMOUNT-LC / WS-PRKURS                            
093290         END-COMPUTE                                                      
093300         PERFORM S10-VATCODE                                              
093310         MOVE IN-EKH-SUVAT        TO R3-LINE-TAX-AMOUNT-LC                
093320         COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                             
093330                 R3-LINE-TAX-AMOUNT-LC / WS-PRKURS                        
093340         END-COMPUTE                                                      
093350         IF WS-KDVALISO = 'JPY'                                           
093360           COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-AMOUNT * 1              
093370           MOVE WS-SUNTO-JPY TO R3-LINE-AMOUNT                            
093380           COMPUTE WS-SUNTO-JPY ROUNDED = R3-LINE-TAX-AMOUNT * 1          
093390           MOVE WS-SUNTO-JPY TO R3-LINE-TAX-AMOUNT                        
093400         END-IF                                                           
093410         PERFORM S04-WRITE-W51073A                                        
093420         COMPUTE SPAR-SUMMA-202-204 = SPAR-SUMMA-202-204 -                
093430                                      R3-LINE-AMOUNT-LC +                 
093440                                      R3-LINE-TAX-AMOUNT-LC               
093450         END-COMPUTE                                                      
093460       END-IF                                                             
093470                                                                          
093480     END-EVALUATE                                                         
093490     .                                                                    
093500     EJECT                                                                
093510                                                                          
093520 CIC-MAIN-EVENT-102-120 SECTION.                                          
093530     EVALUATE IN-EKH-KDEKNIVA                                             
093540     WHEN 'SUM'                                                           
093550       IF SYST-IDSEKVNR = 1                                               
093560         IF IN-EKH-KDVALISO = 'USD'                                       
093570           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
093580           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
093590           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
093600           PERFORM S31-READ-DATABASE-B1                                   
093610           IF BET-IDLEVNR-FIN > ' '                                       
093620             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
093630           END-IF                                                         
093640           PERFORM S10-VATCODE                                            
093650           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
093660           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
093670                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
093680           END-COMPUTE                                                    
093690           PERFORM S04-WRITE-W51073A                                      
093700         END-IF                                                           
093710       END-IF                                                             
093720                                                                          
093730       IF SYST-IDSEKVNR = 2                                               
093740         IF IN-EKH-KDVALISO = 'CNY'                                       
093750           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
093760           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
093770           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
093780           PERFORM S31-READ-DATABASE-B1                                   
093790           IF BET-IDLEVNR-FIN > ' '                                       
093800             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
093810           END-IF                                                         
093820           PERFORM S10-VATCODE                                            
093830           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
093840           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
093850                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
093860           END-COMPUTE                                                    
093870           PERFORM S04-WRITE-W51073A                                      
093880         END-IF                                                           
093890       END-IF                                                             
093900                                                                          
093910       IF SYST-IDSEKVNR = 3                                               
093920         IF IN-EKH-KDVALISO = 'THB'                                       
093930           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
093940           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
093950           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
093960           PERFORM S31-READ-DATABASE-B1                                   
093970           IF BET-IDLEVNR-FIN > ' '                                       
093980             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
093990           END-IF                                                         
094000           PERFORM S10-VATCODE                                            
094010           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
094020           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
094030                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
094040           END-COMPUTE                                                    
094050           PERFORM S04-WRITE-W51073A                                      
094060         END-IF                                                           
094070       END-IF                                                             
094080                                                                          
094090       IF SYST-IDSEKVNR = 4                                               
094100         IF IN-EKH-KDVALISO = 'TWD'                                       
094110           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
094120           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
094130           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
094140           PERFORM S31-READ-DATABASE-B1                                   
094150           IF BET-IDLEVNR-FIN > ' '                                       
094160             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
094170           END-IF                                                         
094180           PERFORM S10-VATCODE                                            
094190           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
094200           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
094210                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
094220           END-COMPUTE                                                    
094230           PERFORM S04-WRITE-W51073A                                      
094240         END-IF                                                           
094250       END-IF                                                             
094260                                                                          
094270       IF SYST-IDSEKVNR = 5                                               
094280         IF IN-EKH-KDVALISO = 'KRW'                                       
094290           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
094300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
094310           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
094320           PERFORM S31-READ-DATABASE-B1                                   
094330           IF BET-IDLEVNR-FIN > ' '                                       
094340             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
094350           END-IF                                                         
094360           PERFORM S10-VATCODE                                            
094370           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
094380           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
094390                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
094400           END-COMPUTE                                                    
094410           PERFORM S04-WRITE-W51073A                                      
094420         END-IF                                                           
094430       END-IF                                                             
094440                                                                          
094450       IF SYST-IDSEKVNR = 6                                               
094460         IF IN-EKH-KDVALISO = 'MYR'                                       
094470           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
094480           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
094490           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
094500           PERFORM S31-READ-DATABASE-B1                                   
094510           IF BET-IDLEVNR-FIN > ' '                                       
094520             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
094530           END-IF                                                         
094540           PERFORM S10-VATCODE                                            
094550           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
094560           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
094570                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
094580           END-COMPUTE                                                    
094590           PERFORM S04-WRITE-W51073A                                      
094600         END-IF                                                           
094610       END-IF                                                             
094620                                                                          
094630       IF SYST-IDSEKVNR = 7                                               
094640         IF IN-EKH-KDVALISO = 'INR'                                       
094650           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
094660           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
094670           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
094680           PERFORM S31-READ-DATABASE-B1                                   
094690           IF BET-IDLEVNR-FIN > ' '                                       
094700             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
094710           END-IF                                                         
094720           PERFORM S10-VATCODE                                            
094730           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
094740           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
094750                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
094760           END-COMPUTE                                                    
094770           PERFORM S04-WRITE-W51073A                                      
094780         END-IF                                                           
094790       END-IF                                                             
094800                                                                          
094810       IF SYST-IDSEKVNR = 8                                               
094820         IF IN-EKH-KDVALISO = 'MXN'                                       
094830           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
094840           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
094850           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
094860           PERFORM S31-READ-DATABASE-B1                                   
094870           IF BET-IDLEVNR-FIN > ' '                                       
094880             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
094890           END-IF                                                         
094900           PERFORM S10-VATCODE                                            
094910           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
094920           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
094930                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
094940           END-COMPUTE                                                    
094950           PERFORM S04-WRITE-W51073A                                      
094960         END-IF                                                           
094970       END-IF                                                             
094980                                                                          
094990       IF SYST-IDSEKVNR = 9                                               
095000         IF IN-EKH-KDVALISO = 'BRL'                                       
095010           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
095020           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
095030           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
095040           PERFORM S31-READ-DATABASE-B1                                   
095050           IF BET-IDLEVNR-FIN > ' '                                       
095060             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
095070           END-IF                                                         
095080           PERFORM S10-VATCODE                                            
095090           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
095100           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
095110                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
095120           END-COMPUTE                                                    
095130           PERFORM S04-WRITE-W51073A                                      
095140         END-IF                                                           
095150       END-IF                                                             
095160                                                                          
095170       IF SYST-IDSEKVNR = 10                                              
095180         IF IN-EKH-KDVALISO = 'ZAR'                                       
095190           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
095200           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
095210           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
095220           PERFORM S31-READ-DATABASE-B1                                   
095230           IF BET-IDLEVNR-FIN > ' '                                       
095240             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
095250           END-IF                                                         
095260           PERFORM S10-VATCODE                                            
095270           MOVE IN-EKH-SUVAT       TO R3-LINE-TAX-AMOUNT                  
095280           COMPUTE R3-LINE-TAX-AMOUNT ROUNDED =                           
095290                   R3-LINE-TAX-AMOUNT  / WS-PRKURS                        
095300           END-COMPUTE                                                    
095310           PERFORM S04-WRITE-W51073A                                      
095320         END-IF                                                           
095330       END-IF                                                             
095340                                                                          
095350     END-EVALUATE                                                         
095360     .                                                                    
095370     EJECT                                                                
095380                                                                          
095390 CID-MAIN-EVENT-102-135 SECTION.                                          
095400     EVALUATE IN-EKH-KDEKNIVA                                             
095410     WHEN 'SUM'                                                           
095420                                                                          
095430       IF SYST-IDSEKVNR = 1                                               
095440         IF IN-EKH-KDVALISO = 'USD'                                       
095450           IF IN-EKH-IDDC-SEND = '87'                                     
095460             CONTINUE                                                     
095470           ELSE                                                           
095480             MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                    
095490             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
095500             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
095510             PERFORM S31-READ-DATABASE-B1                                 
095520             IF BET-IDLEVNR-FIN > ' '                                     
095530               MOVE BET-IDLEVNR-FIN TO R3-LINE-ACCOUNT                    
095540             END-IF                                                       
095550             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
095560                   (R3-LINE-AMOUNT  * WS-PRKURS-US)                       
095570             COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                   
095580                                   WS-SUMMA-135                           
095590             PERFORM S10-VATCODE                                          
095600             IF IN-EKH-SUVAT = ZERO                                       
095610               MOVE ZERO             TO R3-LINE-TAX-AMOUNT                
095620               MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC             
095630             ELSE                                                         
095640               MOVE ZERO             TO R3-LINE-TAX-AMOUNT                
095650               MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC             
095660             END-IF                                                       
095670             MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                  
095680             PERFORM S04-WRITE-W51073A                                    
095690           END-IF                                                         
095700         END-IF                                                           
095710       END-IF                                                             
095720                                                                          
095730       IF SYST-IDSEKVNR = 2                                               
095740         IF IN-EKH-KDVALISO = 'CNY'                                       
095750           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
095760           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
095770           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
095780           PERFORM S31-READ-DATABASE-B1                                   
095790           IF BET-IDLEVNR-FIN > ' '                                       
095800             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
095810           END-IF                                                         
095820           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
095830                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
095840           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
095850                                 WS-SUMMA-135                             
095860           PERFORM S10-VATCODE                                            
095870           IF IN-EKH-SUVAT = ZERO                                         
095880             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
095890             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
095900           ELSE                                                           
095910             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
095920             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
095930                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
095940           END-IF                                                         
095950           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
095960           PERFORM S04-WRITE-W51073A                                      
095970         END-IF                                                           
095980       END-IF                                                             
095990                                                                          
096000       IF SYST-IDSEKVNR = 3                                               
096010         IF IN-EKH-KDVALISO = 'THB'                                       
096020           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
096030           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
096040           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
096050           PERFORM S31-READ-DATABASE-B1                                   
096060           IF BET-IDLEVNR-FIN > ' '                                       
096070             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
096080           END-IF                                                         
096090           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
096100                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
096110           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
096120                                 WS-SUMMA-135                             
096130           PERFORM S10-VATCODE                                            
096140           IF IN-EKH-SUVAT = ZERO                                         
096150             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
096160             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
096170           ELSE                                                           
096180             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
096190             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
096200                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
096210           END-IF                                                         
096220           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
096230           PERFORM S04-WRITE-W51073A                                      
096240         END-IF                                                           
096250       END-IF                                                             
096260                                                                          
096270       IF SYST-IDSEKVNR = 4                                               
096280         IF IN-EKH-KDVALISO = 'TWD'                                       
096290           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
096300           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
096310           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
096320           PERFORM S31-READ-DATABASE-B1                                   
096330           IF BET-IDLEVNR-FIN > ' '                                       
096340             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
096350           END-IF                                                         
096360           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
096370                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
096380           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
096390                                 WS-SUMMA-135                             
096400           PERFORM S10-VATCODE                                            
096410           IF IN-EKH-SUVAT = ZERO                                         
096420             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
096430             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
096440           ELSE                                                           
096450             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
096460             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
096470                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
096480           END-IF                                                         
096490           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
096500           PERFORM S04-WRITE-W51073A                                      
096510         END-IF                                                           
096520       END-IF                                                             
096530                                                                          
096540       IF SYST-IDSEKVNR = 5                                               
096550         IF IN-EKH-KDVALISO = 'KRW'                                       
096560           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
096570           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
096580           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
096590           PERFORM S31-READ-DATABASE-B1                                   
096600           IF BET-IDLEVNR-FIN > ' '                                       
096610             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
096620           END-IF                                                         
096630           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
096640                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
096650           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
096660                                 WS-SUMMA-135                             
096670           PERFORM S10-VATCODE                                            
096680           IF IN-EKH-SUVAT = ZERO                                         
096690             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
096700             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
096710           ELSE                                                           
096720             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
096730             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
096740                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
096750           END-IF                                                         
096760           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
096770           PERFORM S04-WRITE-W51073A                                      
096780         END-IF                                                           
096790       END-IF                                                             
096800                                                                          
096810       IF SYST-IDSEKVNR = 6                                               
096820         IF IN-EKH-KDVALISO = 'MYR'                                       
096830           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
096840           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
096850           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
096860           PERFORM S31-READ-DATABASE-B1                                   
096870           IF BET-IDLEVNR-FIN > ' '                                       
096880             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
096890           END-IF                                                         
096900           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
096910                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
096920           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
096930                                 WS-SUMMA-135                             
096940           PERFORM S10-VATCODE                                            
096950           IF IN-EKH-SUVAT = ZERO                                         
096960             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
096970             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
096980           ELSE                                                           
096990             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
097000             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
097010                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
097020           END-IF                                                         
097030           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
097040           PERFORM S04-WRITE-W51073A                                      
097050         END-IF                                                           
097060       END-IF                                                             
097070                                                                          
097080       IF SYST-IDSEKVNR = 7                                               
097090         IF IN-EKH-KDVALISO = 'INR'                                       
097100           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
097110           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
097120           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
097130           PERFORM S31-READ-DATABASE-B1                                   
097140           IF BET-IDLEVNR-FIN > ' '                                       
097150             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
097160           END-IF                                                         
097170           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
097180                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
097190           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
097200                                 WS-SUMMA-135                             
097210           PERFORM S10-VATCODE                                            
097220           IF IN-EKH-SUVAT = ZERO                                         
097230             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
097240             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
097250           ELSE                                                           
097260             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
097270             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
097280                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
097290           END-IF                                                         
097300           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
097310           PERFORM S04-WRITE-W51073A                                      
097320         END-IF                                                           
097330       END-IF                                                             
097340                                                                          
097350       IF SYST-IDSEKVNR = 8                                               
097360         IF IN-EKH-KDVALISO = 'MXN'                                       
097370           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
097380           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
097390           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
097400           PERFORM S31-READ-DATABASE-B1                                   
097410           IF BET-IDLEVNR-FIN > ' '                                       
097420             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
097430           END-IF                                                         
097440           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
097450                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
097460           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
097470                                 WS-SUMMA-135                             
097480           PERFORM S10-VATCODE                                            
097490           IF IN-EKH-SUVAT = ZERO                                         
097500             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
097510             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
097520           ELSE                                                           
097530             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
097540             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
097550                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
097560           END-IF                                                         
097570           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
097580           PERFORM S04-WRITE-W51073A                                      
097590         END-IF                                                           
097600       END-IF                                                             
097610                                                                          
097620       IF SYST-IDSEKVNR = 9                                               
097630         IF IN-EKH-KDVALISO = 'BRL'                                       
097640           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
097650           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
097660           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
097670           PERFORM S31-READ-DATABASE-B1                                   
097680           IF BET-IDLEVNR-FIN > ' '                                       
097690             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
097700           END-IF                                                         
097710           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
097720                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
097730           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
097740                                 WS-SUMMA-135                             
097750           PERFORM S10-VATCODE                                            
097760           IF IN-EKH-SUVAT = ZERO                                         
097770             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
097780             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
097790           ELSE                                                           
097800             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
097810             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
097820                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
097830           END-IF                                                         
097840           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
097850           PERFORM S04-WRITE-W51073A                                      
097860         END-IF                                                           
097870       END-IF                                                             
097880                                                                          
097890       IF SYST-IDSEKVNR = 10                                              
097900         IF IN-EKH-KDVALISO = 'TRY'                                       
097910           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
097920           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
097930           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
097940           PERFORM S31-READ-DATABASE-B1                                   
097950           IF BET-IDLEVNR-FIN > ' '                                       
097960             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
097970           END-IF                                                         
097980           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
097990                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
098000           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
098010                                 WS-SUMMA-135                             
098020           PERFORM S10-VATCODE                                            
098030           IF IN-EKH-SUVAT = ZERO                                         
098040             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
098050             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
098060           ELSE                                                           
098070             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
098080             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
098090                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
098100           END-IF                                                         
098110           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
098120           PERFORM S04-WRITE-W51073A                                      
098130         END-IF                                                           
098140       END-IF                                                             
098150                                                                          
098160       IF SYST-IDSEKVNR = 11                                              
098170         IF IN-EKH-KDVALISO = 'USD'                                       
098180           IF IN-EKH-IDDC-SEND = '87'                                     
098190             MOVE IN-EKH-SUBEL     TO R3-LINE-AMOUNT                      
098200             MOVE SYST-IDKONTO      TO WS-R3-ACCOUNT-10                   
098210             MOVE WS-R3-ACCOUNT-6   TO R3-LINE-ACCOUNT                    
098220             PERFORM S31-READ-DATABASE-B1                                 
098230             IF BET-IDLEVNR-FIN > ' '                                     
098240               MOVE BET-IDLEVNR-FIN TO R3-LINE-ACCOUNT                    
098250             END-IF                                                       
098260             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
098270                   (R3-LINE-AMOUNT * WS-PRKURS-ALL)                       
098280             COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                   
098290                                   WS-SUMMA-135                           
098300             PERFORM S10-VATCODE                                          
098310             IF IN-EKH-SUVAT = ZERO                                       
098320               MOVE ZERO           TO R3-LINE-TAX-AMOUNT                  
098330               MOVE ZERO           TO R3-LINE-TAX-AMOUNT-LC               
098340             ELSE                                                         
098350               MOVE IN-EKH-SUVAT   TO R3-LINE-TAX-AMOUNT                  
098360               COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                    
098370                   (R3-LINE-TAX-AMOUNT * WS-PRKURS-ALL)                   
098380             END-IF                                                       
098390             MOVE WS-KDBETVIL      TO R3-LINE-PAYTERMS                    
098400             PERFORM S04-WRITE-W51073A                                    
098410           END-IF                                                         
098420         END-IF                                                           
098430       END-IF                                                             
098440                                                                          
098450       IF SYST-IDSEKVNR = 12                                              
098460         IF IN-EKH-KDVALISO = 'ZAR'                                       
098470           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
098480           MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                   
098490           MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                    
098500           PERFORM S31-READ-DATABASE-B1                                   
098510           IF BET-IDLEVNR-FIN > ' '                                       
098520             MOVE BET-IDLEVNR-FIN   TO R3-LINE-ACCOUNT                    
098530           END-IF                                                         
098540           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
098550                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
098560           COMPUTE WS-SUMMA-135 = R3-LINE-AMOUNT-LC -                     
098570                                 WS-SUMMA-135                             
098580           PERFORM S10-VATCODE                                            
098590           IF IN-EKH-SUVAT = ZERO                                         
098600             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
098610             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
098620           ELSE                                                           
098630             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
098640             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
098650                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
098660           END-IF                                                         
098670           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
098680           PERFORM S04-WRITE-W51073A                                      
098690         END-IF                                                           
098700       END-IF                                                             
098710                                                                          
098720     END-EVALUATE                                                         
098730     .                                                                    
098740     EJECT                                                                
098750                                                                          
098760 CIE-MAIN-EVENT-102-145 SECTION.                                          
098770     EVALUATE IN-EKH-KDEKNIVA                                             
098780     WHEN 'SUM'                                                           
098790                                                                          
098800       IF SYST-IDSEKVNR = 1                                               
098810         IF IN-EKH-KDVALISO = 'USD'                                       
098820           IF IN-EKH-IDDC-SEND = '87'                                     
098830             CONTINUE                                                     
098840           ELSE                                                           
098850             MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                    
098860             MOVE SYST-IDKONTO        TO WS-R3-ACCOUNT-10                 
098870             MOVE WS-R3-ACCOUNT-6     TO R3-LINE-ACCOUNT                  
098880             COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                          
098890                   (R3-LINE-AMOUNT  * WS-PRKURS-US)                       
098900             COMPUTE WS-SUMMA-145 = R3-LINE-AMOUNT-LC -                   
098910                                   WS-SUMMA-145                           
098920             PERFORM S10-VATCODE                                          
098930             IF IN-EKH-SUVAT = ZERO                                       
098940               MOVE ZERO             TO R3-LINE-TAX-AMOUNT                
098950               MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC             
098960             ELSE                                                         
098970               MOVE ZERO             TO R3-LINE-TAX-AMOUNT                
098980               MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC             
098990             END-IF                                                       
099000             MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                  
099010             PERFORM S04-WRITE-W51073A                                    
099020           END-IF                                                         
099030         END-IF                                                           
099040       END-IF                                                             
099050                                                                          
099060       IF SYST-IDSEKVNR = 2                                               
099070         IF IN-EKH-KDVALISO = 'USD'                                       
099080         AND IN-EKH-IDDC-SEND NOT = '87'                                  
099090           CONTINUE                                                       
099100         ELSE                                                             
099110           MOVE IN-EKH-SUBEL       TO R3-LINE-AMOUNT                      
099120           MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                           
099130           PERFORM IMS-GU-WDB601                                          
099140           MOVE DCS-IDPARTNR       TO R3-LINE-ACCOUNT                     
099150           COMPUTE R3-LINE-AMOUNT-LC ROUNDED =                            
099160                 (R3-LINE-AMOUNT  * WS-PRKURS-ALL)                        
099170           COMPUTE WS-SUMMA-145 = R3-LINE-AMOUNT-LC -                     
099180                                 WS-SUMMA-145                             
099190           PERFORM S10-VATCODE                                            
099200           IF IN-EKH-SUVAT = ZERO                                         
099210             MOVE ZERO             TO R3-LINE-TAX-AMOUNT                  
099220             MOVE ZERO             TO R3-LINE-TAX-AMOUNT-LC               
099230           ELSE                                                           
099240             MOVE IN-EKH-SUVAT     TO R3-LINE-TAX-AMOUNT                  
099250             COMPUTE R3-LINE-TAX-AMOUNT-LC ROUNDED =                      
099260                 (R3-LINE-TAX-AMOUNT  * WS-PRKURS-ALL)                    
099270           END-IF                                                         
099280           MOVE WS-KDBETVIL        TO R3-LINE-PAYTERMS                    
099290           PERFORM S04-WRITE-W51073A                                      
099300         END-IF                                                           
099310       END-IF                                                             
099320                                                                          
099330     END-EVALUATE                                                         
099340     .                                                                    
099350     EJECT                                                                
099360                                                                          
099370 CJ-BUILD-COMMON-LOG-PART SECTION.                                        
099380     MOVE ZERO             TO LOGG-W51074                                 
099390     MOVE IN-EKH-DAVERDAT  TO LOGG-DAVERDAT                               
099400     MOVE IN-EKH-KDEKHHT   TO LOGG-KDEKHHT                                
099410     MOVE IN-EKH-KDEKSHT   TO LOGG-KDEKSHT                                
099420     MOVE IN-EKH-KDEKNIVA  TO LOGG-KDEKNIVA                               
099430     MOVE IN-EKH-IDVERGL   TO LOGG-IDVERGL                                
099440     MOVE IN-EKH-IDARTNR   TO LOGG-IDARTNR                                
099450     MOVE IN-EKH-KDPRODSL  TO LOGG-KDPRODSL                               
099460     MOVE IN-EKH-FLLSBOK   TO LOGG-FLLSBOK                                
099470     MOVE IN-EKH-KVANTAL   TO LOGG-KVANTAL                                
099480     MOVE IN-EKH-PRARTSTD  TO LOGG-PRARTSTD                               
099490                                                                          
099500****  SKAPA AVSTÄMNINGS- OCH ANALYSPOST   *******                         
099510     MOVE IN-FIL-TIKLOCK   TO AVST-TIKLOCK                                
099520     MOVE IN-FIL-DAREGDAT  TO AVST-DAREGDAT                               
099530     MOVE IN-EKH-KDEKHHT   TO AVST-KDEKHHT                                
099540     MOVE IN-EKH-KDEKSHT   TO AVST-KDEKSHT                                
099550     MOVE IN-EKH-KDEKNIVA  TO AVST-KDEKNIVA                               
099560     MOVE IN-EKH-IDVERGL   TO AVST-IDVERGL                                
099570     MOVE IN-EKH-IDARTNR   TO AVST-IDARTNR                                
099580     MOVE IN-EKH-KDPRODSL  TO AVST-KDPRODSL                               
099590     MOVE IN-EKH-FLLSBOK   TO AVST-FLLSBOK                                
099600     MOVE IN-EKH-KVANTAL   TO AVST-KVANTAL                                
099610     MOVE IN-EKH-PRARTSTD  TO AVST-PRARTSTD                               
099620     MOVE IN-EKH-DAVERDAT  TO AVST-DAVERDAT                               
099630     MOVE ZERO             TO AVST-PRINK                                  
099640     .                                                                    
099650     EJECT                                                                
099660                                                                          
099670 Z-FINI SECTION.                                                          
099680     CLOSE W51066                                                         
099690           W51068                                                         
099700           W51071A                                                        
099710           W51072A                                                        
099720           W51073A                                                        
099730           W51075                                                         
099740           W51713                                                         
099750           W51714                                                         
099760           W5106N                                                         
099770           W51310                                                         
099780                                                                          
099790     MOVE 'S' TO POSTSUM-OPKOD                                            
099800     CALL POSTSUM USING POSTSUM-PARM                                      
099810     .                                                                    
099820     EJECT                                                                
099830                                                                          
099840 S01-READ-W51066  SECTION.                                                
099850     READ W51066 INTO IN-AREA                                             
099860     AT END                                                               
099870        MOVE HIGH-VALUE   TO IN-EKH-W510EKHA                              
099880        SET END-OF-W51066 TO TRUE                                         
099890                                                                          
099900     NOT AT END                                                           
099910        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
099920        MOVE 'W51066'     TO POSTSUM-FDNAMN                               
099930        MOVE 'W51068D1'   TO POSTSUM-DDNAMN2                              
099940        CALL POSTSUM USING POSTSUM-PARM                                   
099950     END-READ                                                             
099960     .                                                                    
099970                                                                          
099980 S02-WRITE-W51071A SECTION.                                               
099990     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
100000     MOVE SPACE                 TO 71LINE-POST                            
100100     IF WS-LINE-SW = JA                                                   
100200       IF WS-KDVALISO = 'SEK'                                             
100300         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
100400       END-IF                                                             
100500       IF IN-EKH-KDSORT = 'SW'                                            
100600         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
100700         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
100800         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
100900       ELSE                                                               
101000         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
101100         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
101200         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
101300       END-IF                                                             
101400       WRITE 71LINE-POST        FROM R3-LINE-R3                           
101500       PERFORM S20-CREATE-WRITE-LOG                                       
101600     ELSE                                                                 
101700       WRITE 71HEAD-POST        FROM R3-HEAD-R3                           
101800     END-IF                                                               
101900                                                                          
102000     IF WS-LINE-SW = JA                                                   
102100       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
102200     ELSE                                                                 
102300       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
102400     END-IF                                                               
102500     MOVE 'W51071A'             TO POSTSUM-FDNAMN                         
102600     MOVE 'W51068D2'            TO POSTSUM-DDNAMN2                        
102700     CALL POSTSUM USING POSTSUM-PARM                                      
102800     .                                                                    
102900                                                                          
103000 S002-WRITE-W51071A-HEAD SECTION.                                         
103100     MOVE SPACE                 TO 71LINE-POST                            
103200     IF IN-EKH-KDSORT = 'SW'                                              
103300       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
103400       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
103500       MOVE WS-TEXT           TO R3-LINE-TEXT                             
103600     ELSE                                                                 
103700       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
103800       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
103900       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
104000     END-IF                                                               
104100     WRITE 71HEAD-POST          FROM R3-HEAD-R3                           
104200                                                                          
104300     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
104400     MOVE 'W51071A'             TO POSTSUM-FDNAMN                         
104500     MOVE 'W51068D2'            TO POSTSUM-DDNAMN2                        
104600     CALL POSTSUM USING POSTSUM-PARM                                      
104700     .                                                                    
104800                                                                          
104900 S03-WRITE-W51072 SECTION.                                                
105000     MOVE IN-EKH-IDDISTR      TO TEST-IDDISTR                             
105100     IF WS-KDVALISO = 'SEK'                                               
105200       MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                           
105300     END-IF                                                               
105400     IF IN-EKH-KDSORT = 'SW'                                              
105500       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
105600       MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                        
105700       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
105800     ELSE                                                                 
105900       MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                             
106000       MOVE SPACE             TO WS-LINE-TEXT-SOFT                        
106100       MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                             
106200     END-IF                                                               
106300     WRITE 72LINE-POST        FROM R3-LINE-R3                             
106400                                                                          
106500     MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                         
106600     MOVE 'W51072A'           TO POSTSUM-FDNAMN                           
106700     MOVE 'W51068D3'          TO POSTSUM-DDNAMN2                          
106800     CALL POSTSUM USING POSTSUM-PARM                                      
106900                                                                          
107000     PERFORM S20-CREATE-WRITE-LOG                                         
107100     .                                                                    
107200                                                                          
107300 S04-WRITE-W51073A SECTION.                                               
107400     MOVE IN-EKH-IDDISTR        TO TEST-IDDISTR                           
107500     MOVE SPACE                 TO 73LINE-POST                            
107600     IF WS-LINE-SW = JA                                                   
107700       IF WS-KDVALISO = 'SEK'                                             
107800         MOVE R3-LINE-AMOUNT-LC TO R3-LINE-AMOUNT                         
107900       END-IF                                                             
108000       IF IN-EKH-KDSORT = 'SW'                                            
108100         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
108200         MOVE IN-EKH-KDSORT     TO WS-LINE-TEXT-SOFT                      
108300         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
108400       ELSE                                                               
108500         MOVE R3-LINE-TEXT      TO WS-LINE-TEXT                           
108600         MOVE SPACE             TO WS-LINE-TEXT-SOFT                      
108700         MOVE WS-LINE-TEXT      TO R3-LINE-TEXT                           
108800       END-IF                                                             
108900       WRITE 73LINE-POST        FROM R3-LINE-R3                           
109000       MOVE R3-LINE-RECORD-TYPE TO POSTSUM-TRANSTYP                       
109100     ELSE                                                                 
109200       WRITE 73HEAD-POST        FROM R3-HEAD-R3                           
109300       MOVE R3-HEAD-RECORD-TYPE TO POSTSUM-TRANSTYP                       
109400     END-IF                                                               
109500                                                                          
109600     MOVE 'W51073A'             TO POSTSUM-FDNAMN                         
109700     MOVE 'W51068D4'            TO POSTSUM-DDNAMN2                        
109800     CALL POSTSUM USING POSTSUM-PARM                                      
109900                                                                          
110000     IF WS-LINE-SW = JA                                                   
110100       PERFORM S20-CREATE-WRITE-LOG                                       
110200     END-IF                                                               
110300     .                                                                    
110400                                                                          
110500 S004-WRITE-W51073A-HEAD SECTION.                                         
110600     MOVE SPACE                 TO 73LINE-POST                            
110700     IF IN-EKH-KDSORT = 'SW'                                              
110800       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
110900       MOVE IN-EKH-KDSORT     TO WS-HEAD-TEXT-SOFT                        
111000       MOVE WS-TEXT           TO R3-LINE-TEXT                             
111100     ELSE                                                                 
111200       MOVE R3-HEAD-TEXT      TO WS-TEXT                                  
111300       MOVE SPACE             TO WS-HEAD-TEXT-SOFT                        
111400       MOVE WS-TEXT           TO R3-HEAD-TEXT                             
111500     END-IF                                                               
111600     WRITE 73HEAD-POST          FROM R3-HEAD-R3                           
111700                                                                          
111800     MOVE R3-HEAD-RECORD-TYPE   TO POSTSUM-TRANSTYP                       
111900     MOVE 'W51073A'             TO POSTSUM-FDNAMN                         
112000     MOVE 'W51068D4'            TO POSTSUM-DDNAMN2                        
112100     CALL POSTSUM USING POSTSUM-PARM                                      
112200     .                                                                    
112300                                                                          
112400 S10-VATCODE SECTION.                                                     
112500*                                                                         
112600* GÖRS ÄNDRING I DENNA SECTION SKA MAN ÄVEN SE IGENOM                     
112700* FÖLJANDE PROGRAM OCH EV. ÄNDRA ÄVEN DÄR                                 
112800* - W41830 !!!!!!!!!!!!!!!!!!!!!!                                         
112900* - W40636 !!!!!!!!!!!!!!!!!!!!!!                                         
113000* - W40638 !!!!!!!!!!!!!!!!!!!!!!                                         
113100* - WF0202 !!!!!!!!!!!!!!!!!!!!!!                                         
113200*                                                                         
113300     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
113400     PERFORM IMS-GU-WDB601                                                
113500     IF DCS-KDDC = SPACE                                                  
113600       MOVE NEJ              TO WDB6-A-SW                                 
113700     ELSE                                                                 
113800       MOVE JA               TO WDB6-A-SW                                 
113900     END-IF                                                               
114000                                                                          
114100     MOVE IN-EKH-IDDISTR     TO TEST-IDDISTR                              
114200     MOVE IN-EKH-BEVAT       TO R3-LINE-TAX-CODE                          
114300     IF IN-EKH-SUVAT = ZERO                                               
114400       IF  WDB6-A-FINNS                                                   
114500       AND DCS-SDC                                                        
114600       AND DCS-IDLANDX2 = 'IT'                                            
114700         IF IN-EKH-IDDISTR = 1558                                         
114800         OR IN-EKH-IDDISTR = 1578                                         
114900         OR IN-EKH-IDDISTR = 3020                                         
115000           MOVE 'ID'         TO R3-LINE-TAX-CODE                          
115100         ELSE                                                             
115200           MOVE 'IC'         TO R3-LINE-TAX-CODE                          
115300         END-IF                                                           
115400       ELSE                                                               
115500         IF  WDB6-A-FINNS                                                 
115600         AND DCS-SDC                                                      
115700         AND DCS-IDLANDX2 = 'NL'                                          
115800           IF IN-EKH-IDDISTR = 1619 OR 1620 OR 1622 OR 1628               
115900           OR                  1678                                       
116000             MOVE 'NZ'         TO R3-LINE-TAX-CODE                        
116100           ELSE                                                           
116200             IF DIST42-EU                                                 
116300               MOVE 'NI'       TO R3-LINE-TAX-CODE                        
116400             ELSE                                                         
116500               MOVE 'NJ'       TO R3-LINE-TAX-CODE                        
116600             END-IF                                                       
116700           END-IF                                                         
116800         ELSE                                                             
116900           IF  WDB6-A-FINNS                                               
117000           AND DCS-DDC                                                    
117100           AND DCS-IDLANDX2 = 'DE'                                        
117200             IF DIST42-EU                                                 
117300               MOVE 'VI'       TO R3-LINE-TAX-CODE                        
117400             ELSE                                                         
117500               MOVE 'VJ'       TO R3-LINE-TAX-CODE                        
117600             END-IF                                                       
117700           ELSE                                                           
117800             IF DCS-IDLANDX2 = 'BE'                                       
117900               IF DIST34-BELGIEN-DDC                                      
118000                 MOVE 'BD'     TO R3-LINE-TAX-CODE                        
118100               ELSE                                                       
118200                 IF DIST42-EU                                             
118300                   MOVE 'BQ'   TO R3-LINE-TAX-CODE                        
118400                 ELSE                                                     
118500                   MOVE 'BX'   TO R3-LINE-TAX-CODE                        
118600                 END-IF                                                   
118700               END-IF                                                     
118800             ELSE                                                         
118900               IF DCS-IDLANDX2 = 'FR'                                     
119000                 IF DIST34-FRANKRIKE-DDC                                  
119100                   MOVE 'F3'   TO R3-LINE-TAX-CODE                        
119200                 ELSE                                                     
119300                   IF DIST42-EU                                           
119400                     MOVE 'F4' TO R3-LINE-TAX-CODE                        
119500                   ELSE                                                   
119600                     MOVE 'F5' TO R3-LINE-TAX-CODE                        
119700                   END-IF                                                 
119800                 END-IF                                                   
119900               ELSE                                                       
120000                IF DCS-IDLANDX2 = 'FI'                                    
120100                  IF DIST34-FINLAND-DDC                                   
120200                    MOVE '48'   TO R3-LINE-TAX-CODE                       
120300                  ELSE                                                    
120400                    IF DIST42-EU                                          
120500                      MOVE '78' TO R3-LINE-TAX-CODE                       
120600                    ELSE                                                  
120700                      MOVE '90' TO R3-LINE-TAX-CODE                       
120800                    END-IF                                                
120900                  END-IF                                                  
121000                ELSE                                                      
121100                IF DCS-IDLANDX2 = 'PL'                                    
121200                  IF DIST34-POLAND-DDC                                    
121300                    MOVE 'P2'   TO R3-LINE-TAX-CODE                       
121400                  ELSE                                                    
121500                    IF DIST42-EU                                          
121600                      MOVE 'PC' TO R3-LINE-TAX-CODE                       
121700                    ELSE                                                  
121800                      MOVE 'PD' TO R3-LINE-TAX-CODE                       
121900                    END-IF                                                
122000                  END-IF                                                  
122100                ELSE                                                      
122200                 IF DCS-IDLANDX2 = 'AU'                                   
122300                   IF DIST34-AUSTRALIA-DDC                                
122400                     MOVE '90'   TO R3-LINE-TAX-CODE                      
122500                   ELSE                                                   
122600                     IF DIST42-EU                                         
122700                       MOVE '90' TO R3-LINE-TAX-CODE                      
122800                     ELSE                                                 
122900                       MOVE '90' TO R3-LINE-TAX-CODE                      
123000                     END-IF                                               
123100                   END-IF                                                 
123200                 ELSE                                                     
123300                  IF DCS-IDLANDX2 = 'ES'                                  
123400                   IF DIST34-SPANIEN-SDC                                  
123500                     MOVE 'S4' TO R3-LINE-TAX-CODE                        
123600                   ELSE                                                   
123700                    IF DIST42-EU                                          
123800                      MOVE '78' TO R3-LINE-TAX-CODE                       
123900                    ELSE                                                  
124000                      MOVE '90' TO R3-LINE-TAX-CODE                       
124100                    END-IF                                                
124200                   END-IF                                                 
124300                  ELSE                                                    
124400                   IF DCS-IDLANDX2 = 'CH'                                 
124500                    IF DIST34-SCHWEIZ-LDC                                 
124600                      MOVE '90' TO R3-LINE-TAX-CODE                       
124700                    ELSE                                                  
124800                     IF DIST42-EU                                         
124900                       MOVE '78' TO R3-LINE-TAX-CODE                      
125000                     ELSE                                                 
125100                       MOVE '90' TO R3-LINE-TAX-CODE                      
125200                     END-IF                                               
125300                    END-IF                                                
125400                   ELSE                                                   
125500                   IF DCS-IDLANDX2 = 'GB'                                 
125600*** SENDING FROM GB TO EU                                                 
125700                     IF DIST34-ENGLAND-DDC                                
125800                     OR DIST34-ENGLAND-LDC                                
125900                     OR DIST34-ENGLAND-SDC                                
126000                       MOVE 'G7' TO R3-LINE-TAX-CODE                      
126100                     ELSE                                                 
126200                       IF DIST42-EU                                       
126300                         MOVE 'G9' TO R3-LINE-TAX-CODE                    
126400                       ELSE                                               
126500                         MOVE '90' TO R3-LINE-TAX-CODE                    
126600                       END-IF                                             
126700                     END-IF                                               
126800                   ELSE                                                   
126900                     IF DCS-IDLANDX2 = 'SE'                               
127000                       IF DIST42-EU                                       
127100**** SERVICE IS 60 OR 80                                                  
127200                         IF IN-EKH-BEVAT = '60'                           
127300                           MOVE '60' TO R3-LINE-TAX-CODE                  
127400                         ELSE                                             
127500                           MOVE '70' TO R3-LINE-TAX-CODE                  
127600                         END-IF                                           
127700                       ELSE                                               
127800                         IF IN-EKH-BEVAT = '80'                           
127900                         OR IN-EKH-BEVAT = 'RU'                           
128000                           IF IN-EKH-BEVAT = '80'                         
128100                             MOVE '80' TO R3-LINE-TAX-CODE                
128200                           ELSE                                           
128300                             MOVE 'RU' TO R3-LINE-TAX-CODE                
128400                           END-IF                                         
128500                         ELSE                                             
128600**** NORTHERN IRELAND (CUST 22004) IS PART OF EU, BUT THE                 
128700**** DISTRICT 1378 (FOR GB) IS NOT EU                                     
128800                           IF IN-EKH-IDKUNDNR = 22004                     
128900                             MOVE '70' TO R3-LINE-TAX-CODE                
129000                           ELSE                                           
129100                             MOVE '90' TO R3-LINE-TAX-CODE                
129200                           END-IF                                         
129300                         END-IF                                           
129400                       END-IF                                             
129500                     ELSE                                                 
129600                       IF DIST42-EU                                       
129700                         MOVE '78'   TO R3-LINE-TAX-CODE                  
129800                       ELSE                                               
129900                         MOVE '90'   TO R3-LINE-TAX-CODE                  
130000                       END-IF                                             
130100                     END-IF                                               
130200                   END-IF                                                 
130300                   END-IF                                                 
130400                  END-IF                                                  
130500                 END-IF                                                   
130600                 END-IF                                                   
130700                END-IF                                                    
130800               END-IF                                                     
130900             END-IF                                                       
131000           END-IF                                                         
131100         END-IF                                                           
131200       END-IF                                                             
131300       IF BET-FLDIRVAT = 'J'                                              
131400       AND IN-EKH-BEVAT > ' '                                             
131500         MOVE IN-EKH-BEVAT   TO R3-LINE-TAX-CODE                          
131600       END-IF                                                             
131700     ELSE                                                                 
131800       EVALUATE TRUE                                                      
131900       WHEN WDB6-A-FINNS                                                  
132000        AND DCS-IDLANDX2 = 'SE'                                           
132100        AND NOT DCS-DDC                                                   
132200            IF IN-EKH-BEVAT = 'RU'                                        
132300              MOVE 'RU'      TO R3-LINE-TAX-CODE                          
132400            ELSE                                                          
132500              MOVE '21'      TO R3-LINE-TAX-CODE                          
132600            END-IF                                                        
132700       WHEN WDB6-A-FINNS                                                  
132800        AND DCS-SDC                                                       
132900        AND DCS-IDLANDX2 = 'NL'                                           
133000            MOVE 'NZ'        TO R3-LINE-TAX-CODE                          
133100       WHEN WDB6-A-FINNS                                                  
133200        AND DCS-SDC                                                       
133300        AND DCS-IDLANDX2 = 'GB'                                           
133400              MOVE 'G7'      TO R3-LINE-TAX-CODE                          
133500       WHEN WDB6-A-FINNS                                                  
133600        AND DCS-DDC                                                       
133700        AND DCS-IDLANDX2 = 'GB'                                           
133800              MOVE 'G7'      TO R3-LINE-TAX-CODE                          
133900       WHEN WDB6-A-FINNS                                                  
134000        AND DCS-SDC                                                       
134100        AND DCS-IDLANDX2 = 'ES'                                           
134200              MOVE 'S4'      TO R3-LINE-TAX-CODE                          
134300       WHEN WDB6-A-FINNS                                                  
134400        AND DCS-SDC                                                       
134500        AND DCS-IDLANDX2 = 'IT'                                           
134600              MOVE 'IC'      TO R3-LINE-TAX-CODE                          
134700       WHEN WDB6-A-FINNS                                                  
134800        AND DCS-DDC                                                       
134900        AND DCS-IDLANDX2 = 'IT'                                           
135000              MOVE 'IC'      TO R3-LINE-TAX-CODE                          
135100       WHEN WDB6-A-FINNS                                                  
135200        AND DCS-SDC                                                       
135300        AND DCS-IDLANDX2 = 'AT'                                           
135400              MOVE 'A2'      TO R3-LINE-TAX-CODE                          
135500       WHEN WDB6-A-FINNS                                                  
135600        AND DCS-SDC                                                       
135700        AND DCS-IDLANDX2 = 'PL'                                           
135800              MOVE 'P2'      TO R3-LINE-TAX-CODE                          
135900       WHEN WDB6-A-FINNS                                                  
136000        AND DCS-DDC                                                       
136100        AND DCS-IDLANDX2 = 'PL'                                           
136200              MOVE 'P2'      TO R3-LINE-TAX-CODE                          
136300       WHEN WDB6-A-FINNS                                                  
136400        AND DCS-DDC                                                       
136500        AND DCS-IDLANDX2 = 'FR'                                           
136600              MOVE 'F3'      TO R3-LINE-TAX-CODE                          
136700       WHEN WDB6-A-FINNS                                                  
136800        AND DCS-NDC-PF                                                    
136900        AND DCS-IDLANDX2 = 'JP'                                           
137000            MOVE 'J4'        TO R3-LINE-TAX-CODE                          
137100       WHEN WDB6-A-FINNS                                                  
137200        AND DCS-NDC-PF                                                    
137300        AND DCS-IDLANDX2 = 'AU'                                           
137400            MOVE '90'        TO R3-LINE-TAX-CODE                          
137500       WHEN WDB6-A-FINNS                                                  
137600        AND DCS-DDC                                                       
137700        AND DCS-IDLANDX2 = 'AU'                                           
137800            MOVE '90'        TO R3-LINE-TAX-CODE                          
137900       WHEN WDB6-A-FINNS                                                  
138000        AND DCS-DDC                                                       
138100        AND DCS-IDLANDX2 = 'SE'                                           
138200              MOVE '21'      TO R3-LINE-TAX-CODE                          
138300       WHEN WDB6-A-FINNS                                                  
138400        AND DCS-DDC                                                       
138500        AND DCS-IDLANDX2 = 'NO'                                           
138600              MOVE 'Y1'      TO R3-LINE-TAX-CODE                          
138700       WHEN WDB6-A-FINNS                                                  
138800        AND DCS-DDC                                                       
138900        AND DCS-IDLANDX2 = 'BE'                                           
139000              MOVE 'B7'      TO R3-LINE-TAX-CODE                          
139100       WHEN WDB6-A-FINNS                                                  
139200        AND DCS-DDC                                                       
139300        AND DCS-IDLANDX2 = 'DE'                                           
139400          MOVE 'VE'          TO R3-LINE-TAX-CODE                          
139500       END-EVALUATE                                                       
139600       IF W-IDDC-B6 = SPACE                                               
139700         MOVE IN-EKH-BEVAT   TO R3-LINE-TAX-CODE                          
139800       END-IF                                                             
139900       IF BET-FLDIRVAT = 'J'                                              
140000       AND IN-EKH-BEVAT > ' '                                             
140100         MOVE IN-EKH-BEVAT   TO R3-LINE-TAX-CODE                          
140200       END-IF                                                             
140300**** FOR EXTENDED WARRANTY GERMANY SHOULD HAVE IPT AND NOT VAT            
140400*      IF  IN-EKH-IDDISTR = 2278                                          
140500*      AND IN-EKH-KDEKHHT = '204'                                         
140600*      AND IN-EKH-KDEKSHT = '205'                                         
140700*        MOVE '80'           TO R3-LINE-TAX-CODE                          
140800*      END-IF                                                             
140900     END-IF                                                               
141000     .                                                                    
141100     EJECT                                                                
141200                                                                          
141300 S11-ANALYSIS SECTION.                                                    
141400     MOVE WS-R3-ACCOUNT-10        TO W-IDKONTO-5122                       
141500       MOVE R3-LINE-PROFIT-CENTER TO W-IDPRCTR-5122                       
141600     PERFORM IMS-GU-5122                                                  
141700     IF SEGMENT-SAKNAS                                                    
141800       MOVE '1586????????'        TO R3-LINE-ORDER                        
141900     ELSE                                                                 
142000       MOVE 5122-IDANALYS         TO R3-LINE-ORDER                        
142100     END-IF                                                               
142200     .                                                                    
142300     EJECT                                                                
142400                                                                          
142500 S12-PROFITCENTER SECTION.                                                
142600     MOVE IN-EKH-IDDISTR          TO WS-IDDISTR                           
142700     MOVE IN-EKH-IDKUNDNR         TO WS-IDKUNDNR                          
142800                                                                          
142900     PERFORM IMS-GU-5121                                                  
143000     MOVE +999999                 TO W-IDKONTO-5122-MIN                   
143100                                     W-IDKONTO-5122-MAX                   
143200     PERFORM IMS-GNP-5122                                                 
143300     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
143400             5122-IDANALYS = WS-IDDISTR-IDKUNDNR                          
143500       PERFORM IMS-GNP-5122                                               
143600     END-PERFORM                                                          
143700                                                                          
143800     IF SEGMENT-SAKNAS                                                    
143900       MOVE '??????????'          TO R3-LINE-PROFIT-CENTER                
144000     ELSE                                                                 
144100       MOVE 5122-IDPRCTR          TO R3-LINE-PROFIT-CENTER                
144200     END-IF                                                               
144300     .                                                                    
144400     EJECT                                                                
144500                                                                          
144600 S20-CREATE-WRITE-LOG SECTION.                                            
144700     MOVE SYST-KDDOKTYP               TO LOGG-KDDOKTYP                    
144800     MOVE R3-LINE-ALLOCATE(1:2)       TO LOGG-IDDC                        
144900     IF SYST-IDPTYP = '610'                                               
145000       MOVE R3-LINE-ACCOUNT(1:6)      TO LOGG-IDKONTO                     
145100     ELSE                                                                 
145200       MOVE ZERO                      TO WS-IDLEVNR                       
145300       INSPECT R3-LINE-ACCOUNT  TALLYING WS-IDLEVNR                       
145400                          FOR CHARACTERS BEFORE INITIAL SPACE             
145500       IF WS-IDLEVNR   > ZERO                                             
145600          MOVE R3-LINE-ACCOUNT(1:WS-IDLEVNR)                              
145700                                      TO LOGG-IDKONTO                     
145800       END-IF                                                             
145900     END-IF                                                               
146000     IF R3-LINE-COST-CENTER NOT = SPACE                                   
146100       MOVE R3-LINE-COST-CENTER(1:5)  TO LOGG-IDKST                       
146200     END-IF                                                               
146300     MOVE R3-LINE-ORDER               TO LOGG-IDANALYS                    
146400     MOVE R3-LINE-PROFIT-CENTER       TO LOGG-IDPRCTR                     
146500     IF (IN-EKH-KDEKHHT = '102'                                           
146600     AND IN-EKH-KDEKSHT = '120')                                          
146700     OR (IN-EKH-KDEKHHT = '102'                                           
146800     AND IN-EKH-KDEKSHT = '130')                                          
146900       MOVE R3-LINE-AMOUNT            TO LOGG-SUBEL                       
147000     ELSE                                                                 
147100       MOVE R3-LINE-AMOUNT-LC         TO LOGG-SUBEL                       
147200     END-IF                                                               
147300     MOVE R3-LINE-AMOUNT-SIGN         TO LOGG-IDTECKEN                    
147400     MOVE R3-LINE-POSTING-KEY         TO LOGG-KDPOST                      
147500                                                                          
147600     PERFORM S21-WRITE-W51075                                             
147700     PERFORM S22-WRITE-W51068                                             
147800                                                                          
147900     IF R3-LINE-TAX-AMOUNT-LC NOT = ZERO                                  
148000       MOVE R3-LINE-TAX-AMOUNT-LC     TO LOGG-SUBEL                       
148100       MOVE 'MOMS'                    TO LOGG-KDEKNIVA                    
148200       MOVE R3-LINE-AMOUNT-SIGN       TO LOGG-IDTECKEN                    
148300                                                                          
148400       PERFORM S21-WRITE-W51075                                           
148500     END-IF                                                               
148600     .                                                                    
148700     EJECT                                                                
148800 S21-WRITE-W51075 SECTION.                                                
148900                                                                          
149000     IF DCS-IDDC NOT = LOGG-IDDC                                          
149100        MOVE LOGG-IDDC TO W-IDDC-B6                                       
149200        PERFORM IMS-GU-WDB601                                             
149300     END-IF                                                               
149400     IF DCS-KDDC = SPACE                                                  
149500       MOVE NEJ              TO WDB6-A-SW                                 
149600     ELSE                                                                 
149700       MOVE JA               TO WDB6-A-SW                                 
149800     END-IF                                                               
149900                                                                          
150000     IF LOGG-KDEKHHT = '501' AND LOGG-KDEKSHT = '501'                     
150100       IF  WDB6-A-FINNS                                                   
150200       AND (DCS-NDC-PF                                                    
150300       OR   DCS-SDC)                                                      
150400         MOVE 'Y'     TO LOGG-FLLSBOK                                     
150500       END-IF                                                             
150600     END-IF                                                               
150700     IF  WDB6-A-FINNS                                                     
150800     AND DCS-DDC                                                          
150900       MOVE 'N'       TO LOGG-FLLSBOK                                     
151000     END-IF                                                               
151100     WRITE LOGG-POST FROM LOGG-W51074                                     
151200                                                                          
151300     MOVE 'ONDM'      TO POSTSUM-TRANSTYP                                 
151400     MOVE 'W51075'    TO POSTSUM-FDNAMN                                   
151500     MOVE 'W51068D5'  TO POSTSUM-DDNAMN2                                  
151600     CALL POSTSUM USING POSTSUM-PARM                                      
151700     .                                                                    
151800                                                                          
151900 S22-WRITE-W51068 SECTION.                                                
152000                                                                          
152100     MOVE R3-LINE-ALLOCATE(1:2) TO AVST-IDDC                              
152200     MOVE R3-LINE-ACCOUNT       TO AVST-IDKONTO                           
152300     MOVE R3-LINE-AMOUNT-LC     TO AVST-SUBEL                             
152400                                                                          
152500     IF R3-LINE-AMOUNT-SIGN = '+'                                         
152600       IF AVST-SUBEL < +0                                                 
152700         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
152800       END-IF                                                             
152900       IF AVST-KVANTAL < +0                                               
153000         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
153100       END-IF                                                             
153200     ELSE                                                                 
153300       IF AVST-SUBEL > +0                                                 
153400         COMPUTE AVST-SUBEL = AVST-SUBEL * -1                             
153500       END-IF                                                             
153600       IF AVST-KVANTAL > +0                                               
153700         COMPUTE AVST-KVANTAL = AVST-KVANTAL * -1                         
153800       END-IF                                                             
153900     END-IF                                                               
154000                                                                          
154100     IF DCS-IDDC NOT = AVST-IDDC                                          
154200        MOVE AVST-IDDC  TO W-IDDC-B6                                      
154300        PERFORM IMS-GU-WDB601                                             
154400     END-IF                                                               
154500     IF DCS-KDDC = SPACE                                                  
154600       MOVE NEJ              TO WDB6-A-SW                                 
154700     ELSE                                                                 
154800       MOVE JA               TO WDB6-A-SW                                 
154900     END-IF                                                               
155000                                                                          
155100     IF AVST-KDEKHHT = '501' AND AVST-KDEKSHT = '501'                     
155200       IF  WDB6-A-FINNS                                                   
155300       AND (DCS-NDC-PF OR DCS-SDC)                                        
155400         MOVE 'Y'               TO AVST-FLLSBOK                           
155500       END-IF                                                             
155600     END-IF                                                               
155700     IF  WDB6-A-FINNS                                                     
155800     AND DCS-DDC                                                          
155900       MOVE 'N'                 TO AVST-FLLSBOK                           
156000     END-IF                                                               
156100                                                                          
156200     IF AVST-IDKONTO(1:4) = '1454'                                        
156300       MOVE '0000'              TO AVST-IDKONTO(7:4)                      
156400       WRITE AVST-POST FROM AVST-W51068                                   
156500                                                                          
156600       MOVE 'AVST'              TO POSTSUM-TRANSTYP                       
156700       MOVE 'W51068'            TO POSTSUM-FDNAMN                         
156800       MOVE 'W51068D6'          TO POSTSUM-DDNAMN2                        
156900       CALL POSTSUM USING POSTSUM-PARM                                    
157000     END-IF                                                               
157100     .                                                                    
157200     EJECT                                                                
157300                                                                          
157400 S30-READ-DATABASE-B2-B1 SECTION.                                         
157500     IF IN-EKH-KDEKHHT = '205'                                            
157600     OR                  '304'                                            
157700     OR (IN-EKH-KDEKHHT = '204'                                           
157800     AND IN-EKH-KDEKSHT = '253')                                          
157900       MOVE 'VO'              TO CIA-IDARTPRE-IN                          
158000       MOVE IN-EKH-IDKUNDNR   TO CIA-IDARTBET-IN                          
158100       CALL W009CIA USING        CIA-W009CIA                              
158200       MOVE CIA-IDARTBET-UT   TO W-WDB1-IDPARTNR                          
158300       MOVE WC-IDFTG-PV       TO W-WDB1-IDFTG                             
158400     ELSE                                                                 
158500       MOVE IN-EKH-IDDISTR    TO W-IDDISTR-WDB2                           
158600       MOVE IN-EKH-IDKUNDNR   TO W-IDKUNDNR-WDB2                          
158700       PERFORM IMS-GU-WDB201                                              
158800                                                                          
158900       MOVE GMT-IDPARTNR      TO W-WDB1-IDPARTNR                          
159000       MOVE WC-IDFTG-PV       TO W-WDB1-IDFTG                             
159100     END-IF                                                               
159200     PERFORM IMS-GU-WDB101                                                
159300     IF SEGMENT-SAKNAS                                                    
159400                                                                          
159500       DISPLAY 'BETALARUPPG. SAKNAS '                                     
159600       DISPLAY IN-EKH-IDVERGL                                             
159700       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
159800       DISPLAY W-WDB1-IDPARTNR                                            
159900       DISPLAY W-WDB1-IDFTG                                               
160000                                                                          
160100       MOVE SPACE           TO BET-KDTRADP                                
160200       MOVE ZERO            TO BET-IDPARTNR                               
160300       MOVE '????'          TO WS-KDBETVIL                                
160400       MOVE '???'           TO WS-KDVALISO-WDB1                           
160500     ELSE                                                                 
160600       MOVE BET-KDBETVIL    TO WS-KDBETVIL                                
160700       MOVE BET-KDVALISO    TO WS-KDVALISO-WDB1                           
160800     END-IF                                                               
160900                                                                          
161000     MOVE BET-IDPARTNR          TO W-BET-IDPARTNR-ALFA                    
161100     MOVE ZERO TO TALLY                                                   
161200     INSPECT W-BET-IDPARTNR-ALFA TALLYING TALLY                           
161300                 FOR CHARACTERS BEFORE INITIAL SPACE                      
161400     IF TALLY = ZERO                                                      
161500       MOVE ZERO                TO W-BET-IDPARTNR-NUM                     
161600     ELSE                                                                 
161700       MOVE W-BET-IDPARTNR-ALFA(1:TALLY)                                  
161800                                TO W-BET-IDPARTNR-NUM                     
161900     END-IF                                                               
162000     .                                                                    
162100     EJECT                                                                
162200                                                                          
162300 S31-READ-DATABASE-B1 SECTION.                                            
162400     IF R3-LINE-ACCOUNT(1:1) = '0'                                        
162500       IF R3-LINE-ACCOUNT(2:1) = '0'                                      
162600         MOVE R3-LINE-ACCOUNT(3:7) TO W-WDB1-IDPARTNR                     
162700       ELSE                                                               
162800         MOVE R3-LINE-ACCOUNT(2:8) TO W-WDB1-IDPARTNR                     
162900       END-IF                                                             
163000     ELSE                                                                 
163100       MOVE R3-LINE-ACCOUNT TO W-WDB1-IDPARTNR                            
163200     END-IF                                                               
163300     MOVE WC-IDFTG-PV       TO W-WDB1-IDFTG                               
163400     PERFORM IMS-GU-WDB101                                                
163500     IF SEGMENT-SAKNAS                                                    
163600       DISPLAY 'BETALARUPPG. SAKNAS '                                     
163700       DISPLAY IN-EKH-IDVERGL                                             
163800       DISPLAY IN-EKH-IDDISTR ' ' IN-EKH-IDKUNDNR                         
163900       DISPLAY W-WDB1-IDPARTNR                                            
164000       DISPLAY W-WDB1-IDFTG                                               
164100     ELSE                                                                 
164200       CONTINUE                                                           
164300     END-IF                                                               
164400     .                                                                    
164500     EJECT                                                                
164600                                                                          
164700 S32-CHECK-POS-NEG SECTION.                                               
164800     COMPUTE WS-LINE-AMOUNT =                                             
164900             IN-EKH-KVANTAL * (IN-EKH-PRARTSJK -                          
165000                              (IN-EKH-PRARTSTD -                          
165100                               IN-EKH-PRINK)   -                          
165200                               IN-EKH-PRHEMTAG)                           
165300     END-COMPUTE                                                          
165400     .                                                                    
165500     EJECT                                                                
165600 S40-SKAPA-W517-OCH-MON-POSTER SECTION.                                   
165700                                                                          
165800     MOVE IN-EKH-IDDC-SEND   TO W-IDDC-B6                                 
165900     PERFORM IMS-GU-WDB601                                                
166000     IF DCS-KDDC = SPACE                                                  
166100       MOVE NEJ              TO WDB6-A-SW                                 
166200     ELSE                                                                 
166300       MOVE JA               TO WDB6-A-SW                                 
166400     END-IF                                                               
166500                                                                          
166600     IF IN-EKH-KDEKHHT = '403' AND IN-EKH-KDEKSHT(1:2) = '40'             
166700       IF IN-EKH-KDEKSHT NOT = '406'                                      
166800         IF IN-EKH-FLDCET = NEJ                                           
166900           PERFORM S42-SKAPA-RW2-INV-POSTER                               
167000         END-IF                                                           
167100       END-IF                                                             
167200     END-IF                                                               
167300                                                                          
167400     IF IN-EKH-KDEKNIVA = 'DET'                                           
167500       IF  IN-EKH-KDEKHHT = '204'                                         
167600       AND (IN-EKH-KDEKSHT = '201' OR '202' OR '251')                     
167700         PERFORM S43-SKAPA-RW1-FAKT-POSTER                                
167800       END-IF                                                             
167900                                                                          
168000       IF (IN-FIL-IDPGM = 'W4183300' OR 'W4184500')                       
168100       AND (WDB6-A-FINNS                                                  
168200       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
168300       OR   DCS-DDC OR DCS-NDC-PF))                                       
168400         IF IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '304'             
168500           CONTINUE                                                       
168600         ELSE                                                             
168700           PERFORM S44-SKAPA-RW1-KRE-RADPOSTER                            
168800         END-IF                                                           
168900       END-IF                                                             
169000                                                                          
169100       IF IN-FIL-IDPGM = 'W4183000'                                       
169200       AND (WDB6-A-FINNS                                                  
169300       AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC                              
169400       OR   DCS-DDC OR DCS-NDC-PF))                                       
169500         PERFORM S45-SKAPA-RW1-KRE-POSTER                                 
169600       END-IF                                                             
169700     END-IF                                                               
169800     .                                                                    
169900     EJECT                                                                
170000 S42-SKAPA-RW2-INV-POSTER SECTION.                                        
170100                                                                          
170200     MOVE 'RW2'              TO RW2-IDPTYP                                
170300     MOVE 'RW2'              TO WS-IDPTYP                                 
170400     MOVE ZERO               TO RW2-IDDISTR                               
170500     IF DCS-KDDC = SPACE OR DCS-DDC                                       
170600       MOVE WC-CDC-SE        TO RW2-IDDC                                  
170700     ELSE                                                                 
170800       MOVE IN-EKH-IDDC-SEND TO RW2-IDDC                                  
170900     END-IF                                                               
171000     IF IN-EKH-KVANTAL < +0                                               
171100       MOVE '0422'           TO RW2-KDWRTYP                               
171200     COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1         
171300     ELSE                                                                 
171400       MOVE '0421'           TO RW2-KDWRTYP                               
171500       COMPUTE RW2-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
171600     END-IF                                                               
171700                                                                          
171800     IF RW2-SUARTSTD NOT = +0                                             
171900       PERFORM S50-SKRIV-RW2-POST                                         
172000       PERFORM S70-WRITE-W51310                                           
172100     END-IF                                                               
172200     .                                                                    
172300     EJECT                                                                
172400 S43-SKAPA-RW1-FAKT-POSTER SECTION.                                       
172500                                                                          
172600     MOVE '0110'             TO RW1-KDWRTYP                               
172700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
172800       MOVE WC-CDC-SE        TO RW1-IDDC                                  
172900     ELSE                                                                 
173000       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
173100     END-IF                                                               
173200     COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD              
173300     COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSJK              
173400     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTNTO              
173500                                                                          
173600     IF RW1-SUARTFSG NOT = +0 OR RW1-SUARTSTD NOT = +0                    
173700     OR RW1-SUARTSJK NOT = +0                                             
173800       PERFORM S48-SKRIV-RW1-POST                                         
173900       PERFORM S49-SKAPA-SKRIV-MON-POST                                   
174000     END-IF                                                               
174100     IF RW1-SUARTSTD NOT = +0                                             
174200       MOVE 'RW1' TO WS-IDPTYP                                            
174300       PERFORM S70-WRITE-W51310                                           
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700 S44-SKAPA-RW1-KRE-RADPOSTER SECTION.                                     
174800                                                                          
174900     MOVE '0110'             TO RW1-KDWRTYP                               
175000     IF DCS-KDDC = SPACE OR DCS-DDC                                       
175100       MOVE WC-CDC-SE        TO RW1-IDDC                                  
175200     ELSE                                                                 
175300       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
175400     END-IF                                                               
175500     IF IN-EKH-KDANMORS = '30'                                            
175600       MOVE ZERO             TO RW1-SUARTSTD                              
175700     ELSE                                                                 
175800      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
175900     END-IF                                                               
176000     IF IN-EKH-KDANMORS = '30' OR '80'                                    
176100       MOVE ZERO             TO RW1-SUARTSJK                              
176200     ELSE                                                                 
176300      COMPUTE RW1-SUARTSJK = IN-EKH-KVANTAL * IN-EKH-PRARTSJK * -1        
176400     END-IF                                                               
176500     COMPUTE RW1-SUARTFSG = IN-EKH-KVANTAL * IN-EKH-PRARTNTO * -1         
176600                                                                          
176700     IF RW1-SUARTFSG NOT = +0 OR RW1-SUARTSTD NOT = +0                    
176800     OR RW1-SUARTSJK NOT = +0                                             
176900       PERFORM S48-SKRIV-RW1-POST                                         
177000       PERFORM S49-SKAPA-SKRIV-MON-POST                                   
177100     END-IF                                                               
177200     IF RW1-SUARTSTD NOT = +0                                             
177300       MOVE 'RW1' TO WS-IDPTYP                                            
177400       PERFORM S70-WRITE-W51310                                           
177500     END-IF                                                               
177600     .                                                                    
177700     EJECT                                                                
177800 S45-SKAPA-RW1-KRE-POSTER SECTION.                                        
177900                                                                          
178000     MOVE '0110'             TO RW1-KDWRTYP                               
178100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
178200       MOVE WC-CDC-SE        TO RW1-IDDC                                  
178300     ELSE                                                                 
178400       MOVE IN-EKH-IDDC-SEND TO RW1-IDDC                                  
178500     END-IF                                                               
178600     IF IN-EKH-KDEKSHT = '310'                                            
178700*** SKROTNING KDANMORS  13 O 23                                           
178800       COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD            
178900     ELSE                                                                 
179000      COMPUTE RW1-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1        
179100     END-IF                                                               
179200                                                                          
179300     MOVE ZERO               TO RW1-SUARTSJK                              
179400                                RW1-SUARTFSG                              
179500     IF RW1-SUARTSTD NOT = +0                                             
179600       PERFORM S48-SKRIV-RW1-POST                                         
179700     END-IF                                                               
179800     IF RW1-SUARTSTD NOT = +0                                             
179900       MOVE 'RW1' TO WS-IDPTYP                                            
180000       PERFORM S70-WRITE-W51310                                           
180100     END-IF                                                               
180200     .                                                                    
180300     EJECT                                                                
180400 S46-CREATE-W517-OCH-MON-POST SECTION.                                    
180500                                                                          
180600     MOVE ZERO                TO RW1-SUARTSTD                             
180700     MOVE ZERO                TO RW1-SUARTSJK                             
180800     MOVE R3-LINE-AMOUNT-LC   TO RW1-SUARTFSG                             
180900     IF IN-EKH-KDEKHHT = '204' AND IN-EKH-KDEKSHT = '251'                 
181000       COMPUTE RW1-SUARTFSG = RW1-SUARTFSG * -1                           
181100     END-IF                                                               
181200     MOVE '0110'              TO RW1-KDWRTYP                              
181300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
181400       MOVE WC-CDC-SE         TO RW1-IDDC                                 
181500     ELSE                                                                 
181600       MOVE IN-EKH-IDDC-SEND  TO RW1-IDDC                                 
181700     END-IF                                                               
181800     IF RW1-SUARTFSG NOT = +0 OR RW1-SUARTSTD NOT = +0                    
181900     OR RW1-SUARTSJK NOT = +0                                             
182000       PERFORM S48-SKRIV-RW1-POST                                         
182100       PERFORM S49-SKAPA-SKRIV-MON-POST                                   
182200     END-IF                                                               
182300     .                                                                    
182400     EJECT                                                                
182500 S48-SKRIV-RW1-POST SECTION.                                              
182600                                                                          
182700     MOVE IN-EKH-KDPRODSL     TO RW1-KDPRODSL                             
182800                                 TEST-KDPRODSL                            
182900     IF KDPRODSL-LOCAL                                                    
183000       SUBTRACT 80 FROM RW1-KDPRODSL                                      
183100     END-IF                                                               
183200                                                                          
183300     IF KDPRODSL-VOLVO-BIMA                                               
183400       CONTINUE                                                           
183500     ELSE                                                                 
183600       MOVE '11'              TO RW1-KDPRODSL                             
183700     END-IF                                                               
183800                                                                          
183900     MOVE 'RW1'               TO RW1-IDPTYP                               
184000     MOVE ZERO                TO RW1-KDMARK                               
184100     MOVE IN-EKH-IDDISTR      TO RW1-IDDISTR                              
184200     WRITE RW1-POST FROM RW1-W517RW1                                      
184300                                                                          
184400     MOVE 'RW1'               TO POSTSUM-TRANSTYP                         
184500     MOVE 'W51714'            TO POSTSUM-FDNAMN                           
184600     MOVE 'W51068D7'          TO POSTSUM-DDNAMN2                          
184700     CALL POSTSUM USING POSTSUM-PARM                                      
184800     .                                                                    
184900     EJECT                                                                
185000 S49-SKAPA-SKRIV-MON-POST SECTION.                                        
185100                                                                          
185200     MOVE RW1-KDPRODSL        TO MON-KDPRODSL                             
185300                                                                          
185400     MOVE +0                  TO MARK-KDCALL                              
185500     MOVE RW1-IDDISTR         TO MARK-IDDISTR                             
185600     CALL W510MARK USING MARK-W510MARK                                    
185700     MOVE MARK-KDMARK-BUDG    TO MON-KDMARK                               
185800     MOVE RW1-SUARTSJK        TO MON-SUARTSJK                             
185900     MOVE RW1-SUARTFSG        TO MON-SUARTFSG                             
186000                                                                          
186100     WRITE MON-POST FROM MON-W51713                                       
186200                                                                          
186300     MOVE 'MON'               TO POSTSUM-TRANSTYP                         
186400     MOVE 'W51713'            TO POSTSUM-FDNAMN                           
186500     MOVE 'W51068D8'          TO POSTSUM-DDNAMN2                          
186600     CALL POSTSUM USING POSTSUM-PARM                                      
186700     .                                                                    
186800     EJECT                                                                
186900 S50-SKRIV-RW2-POST SECTION.                                              
187000                                                                          
187100     MOVE IN-EKH-KDPRODSL     TO RW2-KDPRODSL                             
187200                                 TEST-KDPRODSL                            
187300     IF KDPRODSL-LOCAL                                                    
187400       SUBTRACT 80 FROM RW2-KDPRODSL                                      
187500     END-IF                                                               
187600                                                                          
187700     IF KDPRODSL-VOLVO-BIMA                                               
187800       CONTINUE                                                           
187900     ELSE                                                                 
188000       MOVE '11'              TO RW2-KDPRODSL                             
188100     END-IF                                                               
188200                                                                          
188300     WRITE RW2-POST FROM RW2-W517RW2                                      
188400                                                                          
188500     MOVE 'RW2'               TO POSTSUM-TRANSTYP                         
188600     MOVE 'W51714'            TO POSTSUM-FDNAMN                           
188700     MOVE 'W51068D7'          TO POSTSUM-DDNAMN2                          
188800     CALL POSTSUM USING POSTSUM-PARM                                      
188900     .                                                                    
189000     EJECT                                                                
189100 S60-WRITE-W5106N SECTION.                                                
189200                                                                          
189300     WRITE SAPUT-POST  FROM IN-AREA                                       
189400                                                                          
189500     MOVE 'SPAR'              TO POSTSUM-TRANSTYP                         
189600     MOVE 'W5106N'            TO POSTSUM-FDNAMN                           
189700     MOVE 'W51068D9'          TO POSTSUM-DDNAMN2                          
189800     CALL POSTSUM USING POSTSUM-PARM                                      
189900     .                                                                    
190000     EJECT                                                                
190100 S70-WRITE-W51310 SECTION.                                                
190200     IF WS-IDPTYP  = 'RW2'                                                
190300       IF DCS-KDDC = SPACE OR DCS-DDC                                     
190400         MOVE WC-CDC-SE        TO INV-IDDC                                
190500       ELSE                                                               
190600         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
190700       END-IF                                                             
190800       IF IN-EKH-KVANTAL < +0                                             
190900*        MOVE '0422'           TO RW2-KDWRTYP                             
191000         MOVE '003'            TO INV-IDPTYP                              
191100       COMPUTE INV-SUARTSTD =                                             
191200                   IN-EKH-KVANTAL * IN-EKH-PRARTSTD * -1                  
191300       ELSE                                                               
191400*        MOVE '0421'           TO RW2-KDWRTYP                             
191500         MOVE '002'            TO INV-IDPTYP                              
191600         COMPUTE INV-SUARTSTD = IN-EKH-KVANTAL * IN-EKH-PRARTSTD          
191700       END-IF                                                             
191800       MOVE SPACE TO WS-IDPTYP                                            
191900       MOVE 0                  TO INV-ADLAGOMR                            
192000       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
192100       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
192200     END-IF                                                               
192300     IF WS-IDPTYP  = 'RW1'                                                
192400       IF DCS-KDDC = SPACE OR DCS-DDC                                     
192500         MOVE WC-CDC-SE        TO INV-IDDC                                
192600       ELSE                                                               
192700         MOVE IN-EKH-IDDC-SEND TO INV-IDDC                                
192800       END-IF                                                             
192900       MOVE RW1-SUARTSTD       TO INV-SUARTSTD                            
193000       MOVE RW1-DAVVREG        TO INV-DAVVREG                             
193100       MOVE 0                  TO INV-ADLAGOMR                            
193200       MOVE IN-EKH-IDARTNR     TO INV-IDARTNR                             
193300       MOVE '001'              TO INV-IDPTYP                              
193400       MOVE SPACE              TO WS-IDPTYP                               
193500     END-IF                                                               
193600     WRITE INV-POST  FROM INV-W51310                                      
193700                                                                          
193800     MOVE 'INV'               TO POSTSUM-TRANSTYP                         
193900     MOVE 'W51310'            TO POSTSUM-FDNAMN                           
194000     MOVE 'W51068DA'          TO POSTSUM-DDNAMN2                          
194100     CALL POSTSUM USING POSTSUM-PARM                                      
194200     .                                                                    
194300     EJECT                                                                
194400 S80-CHECK-TAX-CODE-3 SECTION.                                            
194500                                                                          
194600     IF DIST35-REFILL-VAT-EU                                              
194700     OR DIST35-REFILL-VAT-NON-EU                                          
194800       MOVE IN-EKH-IDDC-REC TO WS-IDDC                                    
194900       EVALUATE TRUE                                                      
195000         WHEN SDC-AT                                                      
195100           MOVE 'S9'     TO R3-LINE-TAX-CODE                              
195200         WHEN LDC-BE                                                      
195300           MOVE 'H4'     TO R3-LINE-TAX-CODE                              
195400         WHEN SDC-ES                                                      
195500           MOVE 'E4'     TO R3-LINE-TAX-CODE                              
195600         WHEN LDC-FR                                                      
195700           MOVE 'F9'     TO R3-LINE-TAX-CODE                              
195800         WHEN LDC-FI                                                      
195900           MOVE 'FK'     TO R3-LINE-TAX-CODE                              
196000         WHEN SDC-IT                                                      
196100           MOVE 'I4'     TO R3-LINE-TAX-CODE                              
196200         WHEN LDC-IT                                                      
196300           MOVE 'I4'     TO R3-LINE-TAX-CODE                              
196400         WHEN SDC-NL                                                      
196500           MOVE 'NE'     TO R3-LINE-TAX-CODE                              
196600         WHEN LDC-NL                                                      
196700           MOVE 'NE'     TO R3-LINE-TAX-CODE                              
196800         WHEN LDC-PL                                                      
196900           MOVE 'PA'     TO R3-LINE-TAX-CODE                              
197000         WHEN LDC-DE                                                      
197100           MOVE 'V5'     TO R3-LINE-TAX-CODE                              
197200         WHEN LDC-NO                                                      
197300           MOVE 'Y3'     TO R3-LINE-TAX-CODE                              
197400         WHEN LDC-GB                                                      
197500           MOVE 'G4'     TO R3-LINE-TAX-CODE                              
197600         WHEN OTHER                                                       
197700           MOVE SPACES   TO R3-LINE-TAX-CODE                              
197800       END-EVALUATE                                                       
197900     END-IF                                                               
198000                                                                          
198100     IF DIST35-RETUR-VAT-EU                                               
198200     OR DIST35-RETUR-VAT-NON-EU                                           
198300     OR DIST35-RETURQ-VAT-EU                                              
198400     OR DIST35-RETURQ-VAT-NON-EU                                          
198500       MOVE IN-EKH-IDDC-SEND TO WS-IDDC                                   
198600       EVALUATE TRUE                                                      
198700         WHEN SDC-AT                                                      
198800         WHEN LDC-BE                                                      
198900         WHEN SDC-ES                                                      
199000         WHEN LDC-FR                                                      
199100         WHEN LDC-FI                                                      
199200         WHEN SDC-IT                                                      
199300         WHEN LDC-IT                                                      
199400         WHEN SDC-NL                                                      
199500         WHEN LDC-NL                                                      
199600         WHEN LDC-PL                                                      
199700         WHEN LDC-DE                                                      
199800           MOVE '41'     TO R3-LINE-TAX-CODE                              
199900         WHEN LDC-NO                                                      
200000         WHEN LDC-GB                                                      
200100           MOVE '58'     TO R3-LINE-TAX-CODE                              
200200         WHEN OTHER                                                       
200300           MOVE SPACES   TO R3-LINE-TAX-CODE                              
200400       END-EVALUATE                                                       
200500     END-IF                                                               
200600     .                                                                    
200700     EJECT                                                                
200800 S80-CHECK-TAX-CODE-4 SECTION.                                            
200900                                                                          
201000     IF DIST35-REFILL-VAT-EU                                              
201100     OR DIST35-REFILL-VAT-NON-EU                                          
201200       MOVE IN-EKH-IDDC-REC  TO WS-IDDC                                   
201300       EVALUATE TRUE                                                      
201400         WHEN SDC-AT                                                      
201500         WHEN LDC-BE                                                      
201600         WHEN SDC-ES                                                      
201700         WHEN LDC-FR                                                      
201800         WHEN LDC-FI                                                      
201900         WHEN SDC-IT                                                      
202000         WHEN LDC-IT                                                      
202100         WHEN SDC-NL                                                      
202200         WHEN LDC-NL                                                      
202300         WHEN LDC-PL                                                      
202400         WHEN LDC-DE                                                      
202500           MOVE '70'     TO R3-LINE-TAX-CODE                              
202600         WHEN LDC-NO                                                      
202700         WHEN LDC-GB                                                      
202800           MOVE '90'     TO R3-LINE-TAX-CODE                              
202900         WHEN OTHER                                                       
203000           MOVE SPACES   TO R3-LINE-TAX-CODE                              
203100       END-EVALUATE                                                       
203200     END-IF                                                               
203300                                                                          
203400     IF DIST35-RETUR-VAT-EU                                               
203500     OR DIST35-RETUR-VAT-NON-EU                                           
203600     OR DIST35-RETURQ-VAT-EU                                              
203700     OR DIST35-RETURQ-VAT-NON-EU                                          
203800       MOVE IN-EKH-IDDC-SEND TO WS-IDDC                                   
203900       EVALUATE TRUE                                                      
204000         WHEN SDC-AT                                                      
204100           MOVE 'A4'     TO R3-LINE-TAX-CODE                              
204200         WHEN LDC-BE                                                      
204300           MOVE 'BQ'     TO R3-LINE-TAX-CODE                              
204400         WHEN SDC-ES                                                      
204500           MOVE 'E7'     TO R3-LINE-TAX-CODE                              
204600         WHEN LDC-FR                                                      
204700           MOVE 'F4'     TO R3-LINE-TAX-CODE                              
204800         WHEN LDC-FI                                                      
204900           MOVE 'FN'     TO R3-LINE-TAX-CODE                              
205000         WHEN SDC-IT                                                      
205100           MOVE 'ID'     TO R3-LINE-TAX-CODE                              
205200         WHEN LDC-IT                                                      
205300           MOVE 'ID'     TO R3-LINE-TAX-CODE                              
205400         WHEN SDC-NL                                                      
205500           MOVE 'NI'     TO R3-LINE-TAX-CODE                              
205600         WHEN LDC-NL                                                      
205700           MOVE 'NI'     TO R3-LINE-TAX-CODE                              
205800         WHEN LDC-PL                                                      
205900           MOVE 'PC'     TO R3-LINE-TAX-CODE                              
206000         WHEN LDC-DE                                                      
206100           MOVE 'VI'     TO R3-LINE-TAX-CODE                              
206200         WHEN LDC-NO                                                      
206300           MOVE 'Y4'     TO R3-LINE-TAX-CODE                              
206400         WHEN LDC-GB                                                      
206500           MOVE 'G9'     TO R3-LINE-TAX-CODE                              
206600         WHEN OTHER                                                       
206700           MOVE SPACES   TO R3-LINE-TAX-CODE                              
206800       END-EVALUATE                                                       
206900     END-IF                                                               
207000     .                                                                    
207100     EJECT                                                                
207200 S81-GET-CURR-RATE SECTION.                                               
207300                                                                          
207400     PERFORM IMS-GU-WDB601                                                
207500     MOVE DCS-KDVALISO          TO WS-KDVALISO-WDB6                       
207600     MOVE WS-TIAA               TO W-DATE-AAMM(1:2)                       
207700     MOVE WS-TIMM               TO W-DATE-AAMM(3:2)                       
207800     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
207900     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
208000     MOVE 'M'                   TO CURR-KDVALTYP                          
208100     MOVE WS-KDVALISO-WDB6      TO CURR-KDVALISO-ROW                      
208200     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
208300     IF CURR-KDSVAR = ' '                                                 
208400       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-ALL                          
208500     ELSE                                                                 
208600       MOVE 1                   TO WS-PRKURS-ALL                          
208700     END-IF                                                               
208800     .                                                                    
208900* --- IMS SECTIONS ---                                                    
209000                                                                          
209100 IMS-GU-WDH521 SECTION.                                                   
209200     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
209300          DELIMITED BY SIZE INTO SSA1                                     
209400     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
209500          DELIMITED BY SIZE INTO SSA2                                     
209600     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
209700          DELIMITED BY SIZE INTO SSA3                                     
209800     MOVE '  '              TO GODK-STATUSKODER                           
209900     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH521 SSA1                   
210000                                                   SSA2                   
210100                                                   SSA3                   
210200     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
210300                                                                          
210400     PERFORM IMS-STATUS-CONTROL                                           
210500     .                                                                    
210600                                                                          
210700 IMS-GNP-WDH531 SECTION.                                                  
210800     MOVE 'WDH531  '        TO SSA1                                       
210900     MOVE '  GE'            TO GODK-STATUSKODER                           
211000     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
211100     MOVE WDH5-STATUS-CODE  TO STATUS-WS                                  
211200                                                                          
211300     PERFORM IMS-STATUS-CONTROL                                           
211400     .                                                                    
211500     EJECT                                                                
211600                                                                          
211700 IMS-GU-WDB201 SECTION.                                                   
211800                                                                          
211900     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-KEY ')'                         
212000          DELIMITED BY SIZE INTO SSA1                                     
212100     MOVE '  '                  TO GODK-STATUSKODER                       
212200     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
212300      MOVE GMTA-STATUS-CODE      TO STATUS-WS                             
212400     PERFORM IMS-STATUS-CONTROL                                           
212500     .                                                                    
212600     EJECT                                                                
212700                                                                          
212800 IMS-GU-WDB101 SECTION.                                                   
212900     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
213000          DELIMITED BY SIZE INTO SSA1                                     
213100     MOVE '  GE'               TO GODK-STATUSKODER                        
213200     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
213300     MOVE BETC-STATUS-CODE     TO STATUS-WS                               
213400     PERFORM IMS-STATUS-CONTROL                                           
213500     .                                                                    
213600     EJECT                                                                
213700                                                                          
213800 IMS-GU-5122 SECTION.                                                     
213900     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
214000            DELIMITED BY SIZE INTO SSA1                                   
214100     STRING 'WDGX5122(KEY5122  =' W-WDGXKEY-5122-X ')'                    
214200            DELIMITED BY SIZE INTO SSA2                                   
214300     MOVE '  GE'           TO GODK-STATUSKODER                            
214400     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5122 SSA1 SSA2            
214500     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
214600     PERFORM IMS-STATUS-CONTROL                                           
214700     .                                                                    
214800                                                                          
214900 IMS-GU-5121 SECTION.                                                     
215000     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-5121-X ')'                    
215100            DELIMITED BY SIZE INTO SSA1                                   
215200     MOVE '    '           TO GODK-STATUSKODER                            
215300     CALL CBLTDLI USING GU  5121-PCB DLI-IO-WDGX5121 SSA1                 
215400     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
215500     PERFORM IMS-STATUS-CONTROL                                           
215600     .                                                                    
215700                                                                          
215800 IMS-GNP-5122 SECTION.                                                    
215900     STRING 'WDGX5122(KEY5122 >=' W-WDGXKEY-5122-MIN-X                    
216000                    '&KEY5122 <=' W-WDGXKEY-5122-MAX-X ')'                
216100            DELIMITED BY SIZE INTO SSA1                                   
216200     MOVE '  GE'           TO GODK-STATUSKODER                            
216300     CALL CBLTDLI USING GNP 5121-PCB DLI-IO-WDGX5122 SSA1                 
216400     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
216500     PERFORM IMS-STATUS-CONTROL                                           
216600     .                                                                    
216700     EJECT                                                                
216800                                                                          
216900 IMS-GU-WDB601    SECTION.                                                
217000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
217100          DELIMITED BY SIZE INTO SSA1                                     
217200     MOVE '  GE' TO GODK-STATUSKODER                                      
217300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
217400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
217500     PERFORM IMS-STATUS-CONTROL                                           
217600     IF SEGMENT-SAKNAS                                                    
217700        MOVE SPACE TO DCS-KDDC                                            
217800     END-IF                                                               
217900     .                                                                    
218000     EJECT                                                                
218100                                                                          
218200 IMS-STATUS-CONTROL SECTION.                                              
218300     SET STATUS-IX TO 1                                                   
218400     SEARCH GODK-STATUS                                                   
218500       AT END                                                             
218600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
218700           DELIMITED BY SIZE INTO FELTEXT                                 
218800         DISPLAY FELTEXT                                                  
218900         CALL FELLOG                                                      
219000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
219100         CONTINUE                                                         
219200     END-SEARCH                                                           
219300     .                                                                    
