000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2215000.                                            
000300 AUTHOR.             IDK, GÖTEBORG.                                       
000400 DATE-WRITTEN.       FEBR 1978./ OMSKRIVEN 2019 AUGUSTI                   
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION.                                                            
000800*        PROGRAMMET UTFÖR OMSPECIFIKATION AV                              
000900*        LEVERANSPLAN.TIDIGARE PGM W2215000/W2215010                      
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDK9                                       
001400*        PROGRAMMET LÄSER      WDD9                                       
001500*        PROGRAMMET LÄSER      WDL2                                       
001600*        PROGRAMMET LÄSER      WDR2                                       
001700*        PROGRAMMET LÄSER      WDF1                                       
001800*        PROGRAMMET LÄSER      WDF3                                       
001900*        PROGRAMMET LÄSER      WDB6                                       
002000*        PROGRAMMET LÄSER      WDD3                                       
002100*        PROGRAMMET LÄSER      WDD7                                       
002200*        PROGRAMMET LÄSER      WDG3                                       
002300*                                                                         
002400*                                                                         
002500*    SUBPROGRAM.                                                          
002600**-OBS!      W2215010    IMS-SUBMODUL - OLD PGM W2215000                  
002700*            W009VADD    ADD AV VECKOR TILL DATUM                         
002800*            W221LPAD    UPPDATERING AV TABELL KDLPORS-TAB                
002900*            W2222200    BERÄKNING AV BEHOVSTABELL                        
003000**-OBS!      W221BLOC    FLYTTA BLOCKADE AVROPSVECKOR,                    
003100**-                      FLYTTAD TILL BMP W2215A00                        
003200*            DATAKORT                                                     
003300*            POSTSUM                                                      
003400*            ABEND                                                        
003500*                                                                         
003600*    ABENDKODER:                                                          
003700*        U0016 -  . . . .                                                 
003800*        U1000 -  . . . .                                                 
003900*                                                                         
004000**-------------------------------------------------------------           
004100*    ÄNDRINGAR: (OLD PGM W2215000)                                        
004200*            2006 JAN-FEB.                                                
004300*            STOPPA AUTOMATPLAN FÖR DEN NYA ORSAKSKODEN 09. /CE           
004400*            2007 JAN                                                     
004500*            Tillägg av upplägg av planer på nya LPF-basen WDD6           
004600*                                                           /CE           
004700*            2007 APRIL 19                                                
004800*            Stoppa utskrivning av planer för vissa lev.nr.               
004900*            uppdatering till WDD6                                        
005000*            eTracker = 4904907 --> 4596935             /CE               
005100*            (CCID = 4904907)                                             
005200*                                                                         
005300*        2007 DEC.  (inst.08.1)                                           
005400*        ETRACKER 4820410. DO NOT INCLUDE OVERSTOCK AT MICRO-LDC          
005500*                          SDC w. WDB6-FLOVRLAGBER='N'                    
005600*                                                                         
005700* 2012-08-07  E'TRACKER 10143273 LOUCAL SOURCING CHINA                    
005800*                                                                         
005900* 2012-11-07  E-TRACKER 10185414 FLYTTA AVROP JUL/NYÅR 2012 - FIX         
006000*                                                                         
006100* 2013-11-13  E-TRACKER 10217534 FLYTTA AVROP JUL/NYÅR 2013 - FIX         
006200*                                                                         
006300* 2014-03-20  E-TRACKER 8403120  BLOCKADE AVROP, BILD 2149                
006400*                                                                         
006500* 2014-04-17  E-TRACKER 10230472 RÄTTA FEL VID HELGDAGSJUSTERING          
006600*                                AV DAGLIGA AVROP.SE SECTION              
006700*                                GEBD-KOLL-HELGDAG                        
006800*                                                                         
006900* 2014-06-26  E-TRACKER 8616110  WRONG INFO FROM REFILL   GK              
007000*                                                                         
007100* 2014-10-10  E-TRACKER 10240126 FLYTTA AVROP JUL/NYÅR 2014 - FIX         
007200*                                                                         
007300* 2014-11-11  E-TRACKER 10244811 DATKONVJUST JUL/NYÅR 2014 - FIX          
007400*                                                                         
007500* 2015-03-12  E-TRACKER 10130993 TA BORT ONÖDIGA ORSAKSKODER              
007600*                                KDLPORS W221W005/W221LPAD.               
007700*                                                                         
007800* 2018-11-08  JIRA PULS-2923 FLYTTA AVROP JUL/NYÅR 2018 - FIX             
007900*                                                                         
008000* 2019-08-06  PBI 1465788 W200V1 - REWRITE BATCH PROGRAMS TO BMP          
008100*             OLD W2215000/10 MOVED INTO ONE PGM W2215000.                
008200*             ALL UPDATES MOVED TO NEW BMP W2215A00.                      
008300*                                                                         
008400**-------------------------------------------------------------           
008500     EJECT                                                                
008600 ENVIRONMENT DIVISION.                                                    
008700     SKIP2                                                                
008800 INPUT-OUTPUT SECTION.                                                    
008900                                                                          
009000 FILE-CONTROL.                                                            
009100     SKIP2                                                                
009200*          --- BEGÄRAN OMSPEC/KONCEPT                                     
009300*          --- INPUT FRÅN PGM W2214000                                    
009400     SELECT W22142                     ASSIGN TO W22150D1.                
009500     SKIP2                                                                
009600*          --- FÖR FRAMST. AV LEVPLANKONCEPT                              
009700*          --- OUTPUT                                                     
009800     SELECT W22151                     ASSIGN TO W22150D2.                
009900     SKIP2                                                                
010000*          --- FÖR FRAMST. AV LEVPLANSTATISTIK                            
010100*          --- OUTPUT                                                     
010200     SELECT W22152                     ASSIGN TO W22150D3.                
010300     SKIP2                                                                
010400*          --- FÖR FRAMST. AV AUT LEVPLANER                               
010500*          --- OUTPUT                                                     
010600     SELECT W22154                     ASSIGN TO W22150D4.                
010700     SKIP2                                                                
010800*          --- FÖR UPPDATERING AV WDD6                                    
010900*          --- LEV.PLAN.FÖRSLAGS-KÖ-BASEN                                 
011000     SELECT W22159                     ASSIGN TO W22150D5.                
011100     SKIP2                                                                
011200*          --- FÖR UPPDATERING I BMP W2215A00                             
011300*          --- OMSPEC AV LEVERANSPLAN.                                    
011400     SELECT W2215A                     ASSIGN TO W22150D6.                
011500                                                                          
011600     EJECT                                                                
011700 DATA DIVISION.                                                           
011800     SKIP2                                                                
011900 FILE SECTION.                                                            
012000     SKIP3                                                                
012100 FD  W22142                                                               
012200     RECORDING       F                                                    
012300     BLOCK CONTAINS  0.                                                   
012400                                                                          
012500*01  POST -COPY W221LI42    -L.                                           
012600     SKIP3                                                                
012700 FD  W22151                                                               
012800     RECORDING       F                                                    
012900     BLOCK CONTAINS  0.                                                   
013000                                                                          
013100*01  POST -COPY W221LI51   -L  -PRE U51KONC-.                             
013200     SKIP3                                                                
013300 FD  W22152                                                               
013400     RECORDING       F                                                    
013500     BLOCK CONTAINS  0.                                                   
013600                                                                          
013700*01  POST  -COPY W221LI52   -L  -PRE U52STAT-.                            
013800     SKIP3                                                                
013900 FD  W22154                                                               
014000     RECORDING       F                                                    
014100     BLOCK CONTAINS  0.                                                   
014200                                                                          
014300*01  POST  -COPY W22154     -L  -PRE U54AUT-.                             
014400     EJECT                                                                
014500 FD  W22159                                                               
014600     RECORDING       F                                                    
014700     BLOCK CONTAINS  0.                                                   
014800                                                                          
014900*01  POST  -COPY WDD601     -L  -PRE U59-.                                
015000     EJECT                                                                
015100 FD  W2215A                                                               
015200     RECORDING       F                                                    
015300     BLOCK CONTAINS  0.                                                   
015400                                                                          
015500*01  POST  -COPY W2215A     -L  -PRE U5A-.                                
015600     EJECT                                                                
015700 WORKING-STORAGE SECTION.                                                 
015800     SKIP2                                                                
015900*    -COPY WY2000W3                                                       
016000     SKIP3                                                                
016100*    -COPY WY2000W1                                                       
016200     SKIP3                                                                
016300*    -COPY WY2000W9                                                       
016400     SKIP3                                                                
016500 77  IDPGM                       PIC X(8)  VALUE 'W2215000'.              
016600 77  CURRENT-SECTION             PIC X(30) VALUE SPACE.                   
016700 77  DBS-SECTION                 PIC X(30) VALUE SPACE.                   
016800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
016900 01  RKOD                        PIC S9(4) VALUE +0 COMP SYNC.            
017000                                                                          
017100 77  SW-FRYS                     PIC X       VALUE 'N'.                   
017200 77  SW-OK                       PIC X       VALUE 'N'.                   
017300 77  SW-HELG                     PIC X       VALUE 'N'.                   
017400 77  SW-FLORS                    PIC X       VALUE 'N'.                   
017500 77  SW-FLORS-06                 PIC X       VALUE 'N'.                   
017600 77  SW-BLOCKAD-VECKA            PIC X       VALUE 'N'.                   
017700 77  SW-WDD902-NYUPPL            PIC X       VALUE 'N'.                   
017800*                                                                         
017900 77  WS-SKIP-IDANSK-2024         PIC 9(3)    VALUE ZERO.                  
018000     88 SKIP-IDANSK-2024                     VALUE 510 THRU 569           
018100                                                   610 THRU 619           
018200                                                   518 528 538 558        
018300                                                   568 608 618 628        
018400                                                   638 648 658 678        
018500                                                   688 698 728 758        
018600                                                   788 878 898.           
018700*                                                                         
018800 77  WS-SKIP-IDANSK-2025         PIC 9(3)    VALUE ZERO.                  
018900     88 SKIP-IDANSK-2025                     VALUE 428 458 518 528        
019000                                                   538 548 558 568        
019100                                                   578 628 648 658        
019200                                                   678 688 698 728        
019300                                                   788 798 818 888        
019400                                                   898.                   
019500*                                                                         
019600 77  WS-SKIP-IDLEVNR-2024        PIC X(5)    VALUE SPACES.                
019700     88 SKIP-IDLEVNR-2024                    VALUE 'AEX15'                
019800                                                   'BL3ZA'                
019900                                                   'BWKSA'.               
020000                                                                          
020100     SKIP3                                                                
020200 01  KONSTANTER.                                                          
020300     03  JA                  PIC X       VALUE 'J'.                       
020400     03  NEJ                 PIC X       VALUE 'N'.                       
020500     03  LEVSEGMENT-SAKNAS   PIC X       VALUE 'S'.                       
020600                                                                          
020700     SKIP1                                                                
020800     03  LAES-ARTIKEL-DATA   PIC S9(3)   VALUE +501  COMP-3.              
020900     03  LAES-LEVERANTOER-DATA                                            
021000                             PIC S9(3)   VALUE +502  COMP-3.              
021100     03  NYUPPL-LEVERANTOER  PIC S9(3)   VALUE +503  COMP-3.              
021200     03  UPPDAT-MATINFO      PIC S9(3)   VALUE +504  COMP-3.              
021300     03  UPPDAT-OMSPEC       PIC S9(3)   VALUE +505  COMP-3.              
021400     03  BORTTAG-OMSPEC      PIC S9(3)   VALUE +506  COMP-3.              
021500     03  NYUPPL-OMSPEC       PIC S9(3)   VALUE +507  COMP-3.              
021600     SKIP1                                                                
021700     03  LAES-AVROP-FIRST    PIC S9(3)   VALUE +521  COMP-3.              
021800     03  LAES-AVROP-NEXT     PIC S9(3)   VALUE +522  COMP-3.              
021900     03  BORTTAG-ALLA-FOERSLAG                                            
022000                             PIC S9(3)   VALUE +523  COMP-3.              
022100     03  NYUPPL-AVROP        PIC S9(3)   VALUE +524  COMP-3.              
022200     03  NYUPPL-DAG-AVROP    PIC S9(3)   VALUE +525  COMP-3.              
022300     03  LAES-WDB6-DC-INFO   PIC S9(3)   VALUE +526  COMP-3.              
022400                                                                          
022500     SKIP1                                                                
022600*--------------------------------------------------------------           
022700*--- TYP AV UPPDATERINGAR TILL BMP W2215A00 UT-FIL W2215A.                
022800*--------------------------------------------------------------           
022900     03  BORTTAG-OMSPEC-5A       PIC X(3)    VALUE '001'.                 
023000     03  BORTTAG-FORSLAG-5A      PIC X(3)    VALUE '002'.                 
023100     03  UPDATE-WDK611-5A        PIC X(3)    VALUE '003'.                 
023200     03  NYUPPL-OMSPEC-5A        PIC X(3)    VALUE '004'.                 
023300     03  UPPDAT-OMSPEC-5A        PIC X(3)    VALUE '005'.                 
023400     03  NYUPPL-LEV-5A           PIC X(3)    VALUE '006'.                 
023500     03  NYUPPL-DAG-AVROP-5A     PIC X(3)    VALUE '007'.                 
023600     03  NYUPPL-AVROP-5A         PIC X(3)    VALUE '008'.                 
023700     03  FLYTTA-BLOC-AVROP       PIC X(3)    VALUE '009'.                 
023800                                                                          
023900*--------------------------------------------------------------           
024000     SKIP2                                                                
024100     03  MAX-BEHOVSVECKOR-I-TAB                                           
024200                             PIC S9(9)   VALUE +156  COMP SYNC.           
024300     03  SEP-SATS-TPO-LEV-SDC-NDC                                         
024400                             PIC X(2)    VALUE '19'.                      
024500     03  SEMESTER-VECKA-START                                             
024600                             PIC S9(3)   VALUE +999  COMP-3.              
024700     03  SEMESTER-VECKA-SLUT                                              
024800                             PIC S9(3)   VALUE +0    COMP-3.              
024900     03  OLIKA-LEV           PIC X(5).                                    
025000       88 RENAULT-LEVNR                 VALUE '3868'                      
025100                                              'K8M6A'.                    
025200       88 VOLKSWAGEN-LEVNR              VALUE '6453'                      
025300                                              'Q09EB'.                    
025400       88 ALLISON-LEVNR                 VALUE '4175'.                     
025500       88 EATON-LEVNR                   VALUE '5333'.                     
025600       88 SOMA-LEVNR                    VALUE '3680'.                     
025700       88 TRW-LEVNR                     VALUE '5362'                      
025800                                              'R9K2A'.                    
025900       88 SATS-LEVNR                    VALUE '1002'.                     
026000       88 GEMEN-LEVNR                   VALUE '14489'                     
026100                                              'DL7YA'.                    
026200       88 SKOVDE-LEVNR                  VALUE '1621'                      
026300                                              'C7CUL'.                    
026400     03  WS-KDLPORS          PIC S9(3)  VALUE ZERO COMP-3.                
026500       88  WS-SPARA-FORSLAG             VALUE 01                          
026600                                              04                          
026700                                              06                          
026800                                              08                          
026900                                              11                          
027000                                              14                          
027100                                              15                          
027200                                              16                          
027300                                              17                          
027400                                              18                          
027500                                              19                          
027600                                              20                          
027700                                              21                          
027800                                              23                          
027900                                              26                          
028000                                              27                          
028100                                              28                          
028200                                              29.                         
028300 SKIP3                                                                    
028400*                            *************************************        
028500*                            **  KOPIA AV WDD9-AREA FÖR KONTROLL**        
028600*                            **  AV GJORDA UPPDATERINGAR        **        
028700*                            *************************************        
028800 01  SPAR-FAELT.                                                          
028900* FRÅN WDD904                                                             
029000     03  WSPAR-KDLPORS-GRP.                                               
029100         05  WSPAR-D904-KDLPORS-TAB OCCURS 3 PIC S9(3) COMP-3.            
029200     03  WSPAR-D904-TISPECST   PIC S9(5)  VALUE ZERO    COMP-3.           
029300     03  WSPAR-D904-KVBEST-PL  PIC S9(7)  VALUE ZERO    COMP-3.           
029400     03  WSPAR-D904-KDPLKOEP   PIC S9     VALUE ZERO    COMP-3.           
029500                                                                          
029600 01  TAB-WDD905.                                                          
029700     03 TAB-D905-REC OCCURS 1000.                                         
029800        05 TAB-D905-IDARTNR     PIC S9(9) COMP-3.                         
029900        05 TAB-D905-IDDC        PIC X(2).                                 
030000        05 TAB-D905-IDLEVNR     PIC X(5).                                 
030100        05 -COPY WDD905 -PRE TAB-D905-                                    
030200 01  WS-TAB-WDD905.                                                       
030300     03 WS-TAB-D905-REC.                                                  
030400        05 WS-TAB-D905-IDARTNR   PIC S9(9) COMP-3.                        
030500        05 WS-TAB-D905-IDDC      PIC X(2).                                
030600        05 WS-TAB-D905-IDLEVNR   PIC X(5).                                
030700        05 -COPY WDD905 -PRE WS-TAB-D905-                                 
030800 EJECT                                                                    
030900 01  ARBETS-FAELT.                                                        
031000     SKIP1                                                                
031100     03  INDX                PIC S9(4)  VALUE +0   COMP SYNC.             
031200     03  MAX-INDX            PIC S9(4)  VALUE +156 COMP SYNC.             
031300     03  IX                  PIC S9(9)               COMP-3.              
031400     03  IX-L                PIC S9(9)  VALUE ZERO   COMP-3.              
031500     03  IX-SUM              PIC S9(9)               COMP-3.              
031600     03  IX-ORS              PIC S9(3)               COMP-3.              
031700     03  IX-DAG              PIC S9(3)               COMP-3.              
031800     03  IX-DG               PIC S9(3)               COMP-3.              
031900                                                                          
032000     03  IX1-D9              PIC S9(5)  VALUE +0     COMP-3.              
032100     03  IX2-D9              PIC S9(5)  VALUE +0     COMP-3.              
032200     03  IX2-D9-MAX          PIC S9(5)  VALUE +1000  COMP-3.              
032300                                                                          
032400     03  BLOC-TAB-IX         PIC S9(4)  VALUE +0     COMP-3.              
032500     03  BLOC-MAX-IX         PIC S9(4)  VALUE +60    COMP-3.              
032600     SKIP1                                                                
032700*--  INFLYTTADE FÄLT FRÅN W2215010                                        
032800     03  W-TIAAVV            PIC 9(04)  VALUE ZERO.                       
032900     03  W-KVPB-SDC          PIC S9(6)V9(1) VALUE ZERO  COMP-3.           
033000     03  W-TILLG-SDC         PIC S9(7)  VALUE ZERO   COMP-3.              
033100     03  W-OVERLAGER-SDC     PIC S9(7)  VALUE ZERO   COMP-3.              
033200     03  WS-PRARTBES         PIC X      VALUE 'N'.                        
033300     03  DAGENS-AAAAMMDD     PIC 9(8)   VALUE ZERO.                       
033400                                                                          
033500     03  DAGENS-SSAAVV       PIC 9(6)   VALUE ZERO.                       
033600     03  FILLER  REDEFINES DAGENS-SSAAVV.                                 
033700         05  DAGENS-SS       PIC 9(2).                                    
033800         05  DAGENS-AAVV     PIC 9(4).                                    
033900         05  FILLER REDEFINES DAGENS-AAVV.                                
034000             07  DAGENS-AA   PIC 9(2).                                    
034100             07  DAGENS-VV   PIC 9(2).                                    
034200     03  FILLER  REDEFINES DAGENS-SSAAVV.                                 
034300         05  DAGENS-SSAA     PIC 9(4).                                    
034400         05  FILLER          PIC 9(2).                                    
034500                                                                          
034600     03  TIFINLV-SSAAVVD     PIC 9(7)     VALUE ZERO.                     
034700     03  FILLER  REDEFINES TIFINLV-SSAAVVD.                               
034800         05  TIFINLV-SS      PIC 9(2).                                    
034900         05  TIFINLV-AAVVD   PIC 9(5).                                    
035000         05  FILLER REDEFINES TIFINLV-AAVVD.                              
035100             07  TIFINLV-AA  PIC 9(2).                                    
035200             07  TIFINLV-VV  PIC 9(2).                                    
035300             07  FILLER      PIC 9(1).                                    
035400     03  FILLER  REDEFINES TIFINLV-SSAAVVD.                               
035500         05  TIFINLV-SSAA    PIC 9(4).                                    
035600         05  FILLER          PIC 9(3).                                    
035700                                                                          
035800     03  DAGENS-ABS-VV       PIC 9(8)     VALUE ZERO.                     
035900     03  TIFINLV-ABS-VV      PIC 9(8)     VALUE ZERO.                     
036000                                                                          
036100     03  SUM-RETUR           PIC S9(7)    VALUE ZERO COMP-3.              
036200                                                                          
036300     03  WS-DASPECST         PIC 9(6).                                    
036400     03  FILLER  REDEFINES WS-DASPECST.                                   
036500         05  WS-DASPECST-SS    PIC 9(2).                                  
036600         05  WS-DASPECST-AAVV  PIC 9(4).                                  
036700*                                                                         
036800     03 WS-DAYS-TIAVRDAT-INL   PIC 9(6)     VALUE ZERO.                   
036900     03 WS-DAYS-TIAVRDAT-DISP  PIC 9(6)     VALUE ZERO.                   
037000     SKIP1                                                                
037100*------                                                                   
037200                                                                          
037300     SKIP1                                                                
037400     03  ANT-LEVDAG          PIC 9(3).                                    
037500     03  W-IDANSK            PIC 9(3).                                    
037600     03  W-KDLPORS           PIC 9(3).                                    
037700     03  W-TILLG             PIC S9(7)V9(2)          COMP-3.              
037800     03  W-TILLG-SPAR        PIC S9(7)V9(2)          COMP-3.              
037900     03  W-TILLG-BER         PIC S9(7)V9(2)          COMP-3.              
038000     03  W-BUFF              PIC S9(7)               COMP-3.              
038100     03  W-BUFF-VV           PIC S9(7)V99            COMP-3.              
038200     03  W-BUFF-VV-1         PIC S9(7)V99            COMP-3.              
038300     03  W-AVROPSKVANTITET   PIC S9(7)               COMP-3.              
038400     03  W-KVAVROP-VVKL12-ACC PIC S9(7)              COMP-3.              
038500     03  W-KVANTITET         PIC S9(7)               COMP-3.              
038600     03  WS-SUMMA-BEHOV      PIC S9(7)               COMP-3.              
038700     03  WS-SUM-START        PIC S9(7)               COMP-3.              
038800     03  W-ANTAL             PIC S9(9)               COMP-3.              
038900     03  W-ANTAL-DEC         PIC S9(9)               COMP-3.              
039000     03  W-KDERS             PIC 9(2).                                    
039100     03  W-KVBEST-REST       PIC S9(7)               COMP-3.              
039200     03  W-DUMMY             PIC S9(7)               COMP-3.              
039300     03  W-DATUM-AAVV-HELP   PIC S9(5)               COMP-3.              
039400     03  WS-DAYS-TIAAVVD     PIC 9(5).                                    
039500     03  WS-DAYS-TIDATE-AA   PIC 9(2).                                    
039600     03  WS-DAYS-TIDATE-VV   PIC 9(2).                                    
039700     03  WS-DAYS-TISPECST-AAVV PIC 9(4)  VALUE ZERO.                      
039800     SKIP1                                                                
039900     03  W-DATUM-AAVV        PIC S9(4).                                   
040000     03  W-DAT REDEFINES W-DATUM-AAVV.                                    
040100         05  W-DATUM-AA      PIC 9(2).                                    
040200         05  W-DATUM-VV      PIC 9(2).                                    
040300     03  AKT-DATUM-AAVV      PIC S9(4).                                   
040400     03  FILLER REDEFINES AKT-DATUM-AAVV.                                 
040500         05  AKT-DATUM-AA    PIC 9(2).                                    
040600         05  AKT-DATUM-VV    PIC 9(2).                                    
040700     03  AKT-DATUM-AAMMDD    PIC S9(6).                                   
040800     03  FILLER REDEFINES AKT-DATUM-AAMMDD.                               
040900         05  AKT-DATUM-AR    PIC 9(2).                                    
041000         05  AKT-DATUM-MM    PIC 9(2).                                    
041100         05  AKT-DATUM-DD    PIC 9(2).                                    
041200     SKIP1                                                                
041300     03  W-DATUM-FROM        PIC S9(5)               COMP-3.              
041400     03  W-DATUM-TOM         PIC S9(5)               COMP-3.              
041500     03  W-DIFF-AA           PIC S9(3)               COMP-3.              
041600     03  W-VECKO-DIFFERENS   PIC S9(3)               COMP-3.              
041700     03  W-VECKO-DIFFERENS-VV PIC S9(3)              COMP-3.              
041800     03  WS-VVBEHOV          PIC S9(3)  VALUE ZERO   COMP-3.              
041900     03  WS-SATS-VVBEHOV     PIC S9(3)  VALUE ZERO   COMP-3.              
042000     03  WS1-YY              PIC 9(2).                                    
042100     03  WS2-YY              PIC 9(2).                                    
042200     SKIP1                                                                
042300     03  W-DATUM-AAVV-AKT    PIC S9(5)               COMP-3.              
042400     03  W-TISPECST-DISP     PIC S9(5)               COMP-3.              
042500     03  W-TISPECST-AVS      PIC S9(5)               COMP-3.              
042600     03  W-GRAENS-AVROP      PIC S9(5)               COMP-3.              
042700     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
042800     03  W-KVDAGAR-FFH       PIC S9(3)               COMP-3.              
042900     03  W-KVVECKOR-SPEC     PIC S9(3)               COMP-3.              
043000     03  W-KVVECKOR-INLEV    PIC S9(3)               COMP-3.              
043100     03  W-KVVECKOR-TT       PIC S9(3)               COMP-3.              
043200     03  W-KVVECKOR-FFH      PIC S9(3)               COMP-3.              
043300     03  W-KVVECKOR-TEMP1    PIC S9(3)               COMP-3.              
043400     03  W-KVVECKOR-TEMP2    PIC S9(3)               COMP-3.              
043500     03  W-SEMESTER-VV-START PIC S9(3)               COMP-3.              
043600     03  W-SEMESTER-VV-SLUT  PIC S9(3)               COMP-3.              
043700     03  W-TIFINLV           PIC 9(5).                                    
043800     03  FILLER REDEFINES W-TIFINLV.                                      
043900         05  W-TIFINLV-1-4   PIC 9(4).                                    
044000         05  FILLER          PIC 9.                                       
044100     03  FILLER REDEFINES W-TIFINLV.                                      
044200         05  W-TIFINLV-AA    PIC 9(2).                                    
044300         05  W-TIFINLV-VV    PIC 9(2).                                    
044400         05  FILLER          PIC 9.                                       
044500     03  W-TISPECST-ADJ      PIC 9(4).                                    
044600     03  W-AAVV-ADD          PIC S9(5)   COMP-3  VALUE ZERO.              
044700     03  ANTAL-VV            PIC S9(5)   COMP-3  VALUE ZERO.              
044800     03  W-KOP               PIC S9(7)   COMP-3  VALUE ZERO.              
044900     03  W-KDLEVPLF          PIC X               VALUE SPACE.             
045000     03  WS-KVBR             PIC S9(7)   COMP-3  VALUE ZERO.              
045100     03  WS-KVBEST-PL        PIC S9(7)   COMP-3  VALUE ZERO.              
045200     03  WS-KVAVROP          PIC S9(7)   COMP-3  VALUE ZERO.              
045300     SKIP3                                                                
045400     03  W-M                 PIC S9(7)               COMP-3.              
045500     03  IX-M                PIC S9(9)   VALUE +0    COMP SYNC.           
045600     03  IX-KOM              PIC S9(9)   VALUE +0    COMP SYNC.           
045700     03  IX-W-Q-FREKV        PIC S9(9)   VALUE +0    COMP SYNC.           
045800     03  W-Q-FREKV-MAX       PIC S9(3)   VALUE +0    COMP-3.              
045900     03  W-ARBKVANT          PIC S9(7)   VALUE +0    COMP-3.              
046000     03  W-ARBKVANT2         PIC S9(7)V99 VALUE +0   COMP-3.              
046100     03  W-KVANTAL           PIC S9(6)               COMP-3.              
046200     03  W-ARSBEH            PIC S9(9)      VALUE ZERO  COMP-3.           
046300     03  W-ARSOMS            PIC S9(9)V9(2) VALUE ZERO  COMP-3.           
046400     03  W-ARSOMS-80000      PIC S9(9)V9(2) VALUE 80000 COMP-3.           
046500     03  W-TIAAMMDD-AVS      PIC  9(6).                                   
046600     03  W-DAAVROP-AVS       PIC  9(6).                                   
046700     03  FILLER REDEFINES W-DAAVROP-AVS.                                  
046800         05  W-TIAVROP-AVS-SEKEL PIC 99.                                  
046900         05  W-TIAVROP-AVS   PIC 9(4).                                    
047000     03  W-TILEVDAG  OCCURS 5    PIC 9.                                   
047100     03  W-KVAVROP   OCCURS 5    PIC S9(7) COMP-3.                        
047200     03  ARB-TILEVDAG  OCCURS 5  PIC 9.                                   
047300     03  WOL-TILEVDAG            PIC 9.                                   
047400     03  WOL-TIAAVV-AVS          PIC 9(4).                                
047500     03  ARB-TIAAMMDD-AVS        PIC 9(6).                                
047600     03  GAM-TIAAMMDD-AVS        PIC 9(6).                                
047700     03  WS-TIAAMMDD-SPECST      PIC 9(6)     VALUE ZERO.                 
047800     03  WS-TISSAAVV             PIC 9(6)     VALUE ZERO.                 
047900     03  FILLER   REDEFINES WS-TISSAAVV.                                  
048000         05  WS-TISS             PIC 9(2).                                
048100         05  WS-TIAAVV           PIC 9(4).                                
048200     03  WS-AAVVD-TIFINLV        PIC 9(5)     VALUE ZERO.                 
048300     03  FILLER   REDEFINES WS-AAVVD-TIFINLV.                             
048400         05  WS-AAVV-TIFINLV     PIC 9(4).                                
048500         05  FILLER              PIC 9(1).                                
048600     03  WS-KVPALL           PIC S9(7) VALUE ZERO    COMP-3.              
048700     03  WS-KVULOAD          PIC S9(7) VALUE ZERO    COMP-3.              
048800     03  WART-TILEVDAG  OCCURS 5 PIC 9.                                   
048900                                                                          
049000     03  WS-IDLANDX2-SHIP        PIC X(2)     VALUE SPACE.                
049100     03  WS-FRYSTID8             PIC 9(8).                                
049200     03  FILLER  REDEFINES WS-FRYSTID8.                                   
049300         05  WS-FRYSTID-SEKEL    PIC 9(2).                                
049400         05  WS-FRYSTID          PIC 9(6).                                
049500     03  VADD-DATUM-AAVV         PIC S9(5)  COMP-3.                       
049600     03  SPAR-TIAVROP-AVS        PIC S9(5)  COMP-3.                       
049700     03  FIX-AAVVD               PIC 9(5)   VALUE ZERO.                   
049800     03  WS-FLAGGA-WLC           PIC X      VALUE 'N'.                    
049900     03  WS-FLAGGA-2AAR          PIC X      VALUE 'N'.                    
050000     03  WS-FLAGGA-REDUC         PIC X      VALUE 'N'.                    
050100     03  W-GALL-AVROP            PIC 9(6)   VALUE ZERO.                   
050200     03  FILLER REDEFINES W-GALL-AVROP.                                   
050300         05  FILLER              PIC 9(2).                                
050400         05  W-GALL-AVROP-AAVV   PIC 9(4).                                
050500     03  W-PREL-AVROP            PIC 9(6)   VALUE ZERO.                   
050600     03  FILLER REDEFINES W-PREL-AVROP.                                   
050700         05  FILLER              PIC 9(2).                                
050800         05  W-PREL-AVROP-AAVV   PIC 9(4).                                
050900     03  W-DIFF-Z                PIC S9(3)  VALUE ZERO.                   
051000***  NEDANSTÅENDE VÄRDEN KAN JUSTERAS                                     
051100***  X1 OCH X2 ÄR FÖR KDVVKL 1 OCH 2                                      
051200***  Y1 OCH Y2 ÄR FÖR KDVVKL 3 OCH 4 OCH 5                                
051300***  1 ÄR NÄR AVROP SENARELAGTS   I PRELIMINÄRT FÖRSLAG                   
051400***  2 ÄR NÄR AVROP TIDIGARELAGTS I PRELIMINÄRT FÖRSLAG                   
051500     03  W-DIFF-X1               PIC S9(3)  VALUE +2.                     
051600     03  W-DIFF-X2               PIC S9(3)  VALUE +2.                     
051700     03  W-DIFF-Y1               PIC S9(3)  VALUE +2.                     
051800     03  W-DIFF-Y2               PIC S9(3)  VALUE +2.                     
051900***                                                                       
052000     03  WS-SUMMA-KVPB           PIC S9(6)V9(1)   COMP-3.                 
052100                                                                          
052200                                                                          
052300*----                                                                     
052400                                                                          
052500 01  WS-IDANSK-HELP              PIC 9(3)    VALUE ZERO.                  
052600 01  FILLER REDEFINES WS-IDANSK-HELP.                                     
052700     03  WS-IDANSK-2             PIC 9(2).                                
052800     03  WS-IDANSK-3             PIC 9(1).                                
052900                                                                          
053000 01  WS-DAAVROP-AVS      PIC 9(6).                                        
053100 01  FILLER  REDEFINES WS-DAAVROP-AVS.                                    
053200     03  WS-DAAVROP-SS   PIC 9(2).                                        
053300     03  WS-DAAVROP-AAVV PIC 9(4).                                        
053400                                                                          
053500*-----------------------------------------------------------------        
053600*--- LEVERANTÖRER SOM EJ SKALL FLYTTA AVROP VID JUL/NYÅR ---------        
053700 77  W-TEST-IDLEVNR-SHIP         PIC X(5)    VALUE SPACE.                 
053800     88  IDLEVNR-ALLTID-SKIP                 VALUE 'BSBZA'.               
053900     EJECT                                                                
054000*-----------------------------------------------------------------        
054100                                                                          
054200 01  FIX-VECKOR.                                                          
054300     03  FIX-TIAVROP-AVS     PIC S9(5)   COMP-3.                          
054400     03  FIX-TIAVROP-INL     PIC S9(5)   COMP-3.                          
054500                                                                          
054600 01  SWITCHAR.                                                            
054700     03  SW-OMSPEC-UTFOERD   PIC X       VALUE 'N'.                       
054800     03  SW-OPTIMAL-OMSPEC   PIC X       VALUE 'N'.                       
054900     03  SW-X-OPT            PIC X       VALUE 'N'.                       
055000     03  SW-BESTREST-TAEKT   PIC X       VALUE 'N'.                       
055100     03  SW-FOERSTA-SEMESTER-LAEST                                        
055200                             PIC X       VALUE 'N'.                       
055300                                                                          
055400     03  FOERST-SW           PIC X        VALUE 'J'.                      
055500         88 FOERST                        VALUE 'J'.                      
055600     03  SW-TRAEFF           PIC X        VALUE 'N'.                      
055700         88 TRAEFF                        VALUE 'J'.                      
055800     03  SW-AUT-PLAN         PIC X        VALUE 'J'.                      
055900     03  SW-PERSLUT          PIC X        VALUE 'N'.                      
056000         88 PERSLUT-JAMN                  VALUE 'J'.                      
056100     03  SW-SKIP-FORSLAG     PIC X        VALUE 'N'.                      
056200     SKIP3                                                                
056300****************************************** TILLGÅNGSTABELL                
056400 01  TILLGANGSTABELL.                                                     
056500     03  TILLGTAB-MAX        PIC S9(9)   VALUE +156  COMP SYNC.           
056600     03  TILLGTAB-IX         PIC S9(9)   VALUE +0    COMP SYNC.           
056700     SKIP1                                                                
056800     03  TILLGTAB.                                                        
056900         05  TILLGTAB-INGANG OCCURS 156.                                  
057000             10  TILLGTAB-ANTAL                                           
057100                             PIC S9(7)V99            COMP-3.              
057200     SKIP3                                                                
057300 01  EOF-SWITCHAR.                                                        
057400     03  W22142-EOF          PIC X   VALUE 'N'.                           
057500     EJECT                                                                
057600 01  TABELL-HELG.                                                         
057700     03  TAB-RAD   OCCURS 2000.                                           
057800         05  TAB-IDLANDX2      PIC X(2).                                  
057900         05  TAB-DADATUM-HELG  PIC 9(8).                                  
058000         05  TAB-FLHELG        PIC X.                                     
058100                                                                          
058200 01  IX-HELG                   PIC S9(5)  COMP-3  VALUE ZERO.             
058300 01  MAX-HELG                  PIC S9(5)  COMP-3  VALUE 2000.             
058400 01  ANT-HELG                  PIC S9(5)  COMP-3  VALUE ZERO.             
058500     EJECT                                                                
058600*01  FILLER     -COPY W221W012 -PRE W012-                                 
058700     EJECT                                                                
058800*    -COPY W221SLEV                                                       
058900     EJECT                                                                
059000*    -COPY W200EMAB                                                       
059100     EJECT                                                                
059200*    -COPY WWPRODSL                                                       
059300     EJECT                                                                
059400                                                                          
059500 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
059600*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
059700*- - - - - - - - - - - - - DC-MAX OCCURS SÄTTS TILL VERKLIGT ANTAL        
059800*- - - - - - - - - - - - - I M- SEKTIONEN.                                
059900 01  IDDC-INDEX-WS.                                                       
060000     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
060100                                                                          
060200     03 WDCIX            PIC S9(3)   VALUE +0  COMP SYNC.                 
060300     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
060400                                                                          
060500 01  IDDC-TABELL.                                                         
060600     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
060700                INDEXED BY DCIX.                                          
060800        05 T-DCS.                                                         
060900          07 T-DCS-IDDC           PIC X(2).                               
061000          07 T-DCS-KDDC           PIC X(2).                               
061100          07 T-DCS-FLOVRLAGBER    PIC X.                                  
061200                                                                          
061300                                                                          
061400 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
061500*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
061600 01  TABENTRY-PARM.                                                       
061700     03  STEGLANGD               PIC S9(9) COMP.                          
061800     03  ANTAL                   PIC S9(9) COMP.                          
061900     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
062000                                                                          
062100*------------------------------------------------------------             
062200                                                                          
062300     EJECT                                                                
062400 01  DYNAMISKA-SUBPROGRAM.                                                
062500     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
062600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
062700     03  ABEND               PIC X(8)    VALUE 'ABEND  '.                 
062800     03  W221LPAD            PIC X(8)    VALUE 'W221LPAD'.                
062900     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
063000     03  W22222              PIC X(8)    VALUE 'W22222'.                  
063100     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
063200     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
063300     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
063400     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY'.                 
063500     03  WDAGKONV            PIC X(8)    VALUE 'WDAGKONV'.                
063600     03  WZ20DAYS            PIC X(8)    VALUE 'WZ20DAYS'.                
063700     03  WINTSOR             PIC X(8)    VALUE 'WINTSOR '.                
063800     EJECT                                                                
063900 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22150'.                  
064000 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
064100*                            *** PARAMETRAR TILL DATKORT                  
064200*01  -COPY WDATKORT                                                       
064300     EJECT                                                                
064400*01  -COPY WORKAREA                                                       
064500*                            *** PARAMETRAR TILL WDAGKONV '               
064600*01  -COPY WDAGAREA.                                                      
064700     EJECT                                                                
064800*    --- PARAMETRAR TILL POSTSUM                                          
064900*                                                                         
065000*01  -COPY W0005   -PRE  POSTSUM-                                         
065100     EJECT                                                                
065200*    --- PARAMETRAR TILL WDATKONV                                         
065300*                                                                         
065400*01  -COPY WDATAREA.                                                      
065500     EJECT                                                                
065600*    --- PARAMETRAR TILL ABEND                                            
065700                                                                          
065800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
065900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
066000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
066100     SKIP2                                                                
066200 01  FELTEXT2.                                                            
066300     03  FILLER                  PIC X(9)   VALUE 'FELTEXT2'.             
066400     03  FELTEXT2-STR            PIC X(71)  VALUE SPACE.                  
066500     EJECT                                                                
066600                                                                          
066700 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
066800*   -COPY WZ20DAYS                                                        
066900     EJECT                                                                
067000                                                                          
067100*    --- LÄNKAREA TILL SUBPROGRAM W221BLOC                                
067200*01  -COPY W221BLOC                                                       
067300     EJECT                                                                
067400*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
067500 01  W-W221LP-CTX                PIC X(08) VALUE 'W221LP01'.              
067600 01  W-KDLPORS-GRP.                                                       
067700     03 W-KDLPORS-TAB OCCURS 4   PIC 9(3).                                
067800                                                                          
067900     EJECT                                                                
068000*      --- VALID IDDC CODES                                               
068100*                                                                         
068200*01  -COPY WWDC99                                                         
068300*01  -COPY WWDCKONS                                                       
068400                                                                          
068500     EJECT                                                                
068600 01  IN-AREA-START               PIC X(24)   VALUE                        
068700                                 'IN-AREA-START  '.                       
068800     SKIP2                                                                
068900*                            *************************************        
069000*                            ** AREA FÖR W22142                 **        
069100*                            ** BEGÄRAN AV OMSPEC/KONCEPT       **        
069200*                            *************************************        
069300*01  AREA  -COPY W221LI42   -PRE             I42BEG-.                     
069400     EJECT                                                                
069500                                                                          
069600 01  UT-AREA-START               PIC X(24)   VALUE                        
069700                                 'UT-AREA-START  '.                       
069800     SKIP2                                                                
069900*                            *************************************        
070000*                            **  AREA FÖR W22151                **        
070100*                            **  FRAMST. AV. LEVPLANKONCEPT     **        
070200*                            *************************************        
070300*01  AREA  -COPY W221LI51   -PRE         U51KONC-.                        
070400     EJECT                                                                
070500*                            *************************************        
070600*                            **  AREA FÖR W22152                **        
070700*                            **  FRAMST.  AV LEVPLANSTATISTIK   **        
070800*                            *************************************        
070900*01  AREA  -COPY W221LI52   -PRE         U52STAT-.                        
071000     EJECT                                                                
071100*                            *************************************        
071200*                            **  AREA FÖR W22154                **        
071300*                            **  FRAMST.  AV AUT LEVPLANER      **        
071400*                            *************************************        
071500*01  AREA  -COPY W22154     -PRE         U54AUT-.                         
071600     EJECT                                                                
071700*                            *************************************        
071800*                            **  AREA FÖR W22159                **        
071900*                            **  Uppd.fil till WDD6 (Lev.pl.f.kö)*        
072000*                            *************************************        
072100*01  AREA  -COPY WDD601     -PRE         U59-.                            
072200     EJECT                                                                
072300                                                                          
072400*                            *************************************        
072500*                            **  AREA FÖR W2215A                **        
072600*                            **  UPPD.FIL TILL BMP W2215A00      *        
072700*                            *************************************        
072800 01  U5A-AREA-START             PIC X(24)   VALUE                         
072900                                'U5A-AREA-START  '.                       
073000     SKIP2                                                                
073100                                                                          
073200*01  AREA -COPY W2215A      -PRE U5A-                                     
073300     EJECT                                                                
073400*                            *************************************        
073500*                            ** LINK-AREA  (ARBETS-AREA)        **        
073600*                            ** ARTIKELDATA                     **        
073700*                            *************************************        
073800*01  AREA  -COPY W221L501   -PRE LINK-                                    
073900     EJECT                                                                
074000*                            *************************************        
074100*                            **  LNK2-AREA                      **        
074200*                            **  BEHOVSTABELL                   **        
074300*                            *************************************        
074400*01  AREA  -COPY W222L222   -PRE LNK2-.                                   
074500     EJECT                                                                
074600*                            *************************************        
074700*                            ** LINK3-AREA (ARBETS-AREA)        **        
074800*                            ** LÄSNING AV AVROP  WDD905        **        
074900*                            *************************************        
075000*01  AREA  -COPY W221L502   -PRE LINK3-.                                  
075100     EJECT                                                                
075200*                            *************************************        
075300*                            **  TAB2-AREA                      **        
075400*                            **  BEHOVSTABELL                   **        
075500*                            *************************************        
075600*01  AREA  -COPY W222L222   -PRE TAB2-.                                   
075700     EJECT                                                                
075800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
075900*                                                                         
076000     EJECT                                                                
076100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
076200     SKIP3                                                                
076300 01  NYCKLAR-TILL-DLI.                                                    
076400                                                                          
076500     03  W-IDARTNR-X.                                                     
076600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
076700     03  W-WDD901KY-X.                                                    
076800         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO  COMP-3.          
076900         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
077000     03  W-IDLEVNR-X.                                                     
077100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
077200     03  W-IDLEVNR-SHIP-X.                                                
077300         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
077400     03  W-KDAVROP-X.                                                     
077500         05  W-KDAVROP           PIC S9(1)   VALUE ZERO  COMP-3.          
077600*--                                                                       
077700                                                                          
077800     03  W-WDD905KY-X.                                                    
077900         05  W-DAAVROP-KY        PIC 9(6)    VALUE ZERO.                  
078000         05  W-TILEVDAG-KY       PIC S9      VALUE ZERO COMP-3.           
078100     03  W-WDF301KY-X.                                                    
078200         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
078300         05  W-DADATUM-HELG      PIC 9(8)    VALUE ZERO.                  
078400         05  FILLER  REDEFINES W-DADATUM-HELG.                            
078500             07  W-DADATUM-HELG-SS      PIC 9(2).                         
078600             07  W-DADATUM-HELG-AAMMDD  PIC 9(6).                         
078700     03  W-WDD905KY-MIN-X.                                                
078800         05  W-DAAVROP-MIN       PIC 9(6)    VALUE ZERO.                  
078900         05  W-TILEVDAG-MIN      PIC S9      VALUE ZERO COMP-3.           
079000     03  W-WDD905KY-MAX-X.                                                
079100         05  W-DAAVROP-MAX       PIC 9(6)    VALUE ZERO.                  
079200         05  W-TILEVDAG-MAX      PIC S9      VALUE 6    COMP-3.           
079300                                                                          
079400     03  W-IDDC-REF-X.                                                    
079500         05  W-IDDC-REF          PIC X(2)   VALUE '11'.                   
079600                                                                          
079700*----                                                                     
079800     03  W-WDGXKEY-2257-X.                                                
079900         05 W-IDHTYP-2257        PIC X(4)    VALUE '2257'.                
080000         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
080100                                                                          
080200     03  W-WDGXKEY-2258-X.                                                
080300         05 W-IDLEVNR-SHIP-2258  PIC X(5)    VALUE SPACE.                 
080400                                                                          
080500     03  W-DAAVROP-2260-X.                                                
080600         05 W-DAAVROP-2260       PIC 9(6)    VALUE ZERO.                  
080700                                                                          
080800     03  W-IDANSK-2260-X.                                                 
080900         05 W-IDANSK-2260        PIC S9(3)   VALUE ZERO COMP-3.           
081000                                                                          
081100*---                                                                      
081200     03  W-DAPRLIST-X.                                                    
081300         05  W-DAPRLIST          PIC  9(8)   VALUE ZERO.                  
081400                                                                          
081500     03  W-IDPTYP-X.                                                      
081600         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
081700                                                                          
081800*---                                                                      
081900     03  W-WDD601KY-MIN-X.                                                
082000         05 W-IDDC-MIN           PIC X(2)  VALUE SPACE.                   
082100         05 W-IDLEVNR-MIN        PIC X(5)  VALUE SPACE.                   
082200         05 W-IDARTNR-MIN        PIC S9(9) VALUE ZERO COMP-3.             
082300         05 W-IDANSK-MIN         PIC S9(3) VALUE +000 COMP-3.             
082400                                                                          
082500     03  W-WDD601KY-MAX-X.                                                
082600         05 W-IDDC-MAX           PIC X(2)  VALUE SPACE.                   
082700         05 W-IDLEVNR-MAX        PIC X(5)  VALUE SPACE.                   
082800         05 W-IDARTNR-MAX        PIC S9(9) VALUE ZERO COMP-3.             
082900         05 W-IDANSK-MAX         PIC S9(3) VALUE +999 COMP-3.             
083000                                                                          
083100     SKIP2                                                                
083200*    --- STATUS-KOD FRÅN IMS                                              
083300 01  STATUS-WS           PIC XX.                                          
083400     88  SEGMENT-FINNS                       VALUE '  '.                  
083500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
083600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
083700     SKIP2                                                                
083800 01  GODK-STATUSKODER.                                                    
083900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
084000     SKIP3                                                                
084100 01  SSA1                        PIC X(128).                              
084200 01  SSA2                        PIC X(64).                               
084300 01  SSA3                        PIC X(64).                               
084400     EJECT                                                                
084500*    --- IMS FUNKTIONSKODER                                               
084600*    -COPY W0003.                                                         
084700     EJECT                                                                
084800*    ---  DLI INPUT-OUTPUT AREA                                           
084900                                                                          
085000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK601'.              
085100 01  DLI-IO-WDK601.                                                       
085200*    03  -COPY WDK601  -PRE K601-                                         
085300     EJECT                                                                
085400 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK611'.              
085500 01  DLI-IO-WDK611.                                                       
085600*    03  -COPY WDK611                                                     
085700     EJECT                                                                
085800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK621'.              
085900 01  DLI-IO-WDK621.                                                       
086000*    03  -COPY WDK621                                                     
086100     EJECT                                                                
086200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK626'.              
086300 01  DLI-IO-WDK626.                                                       
086400*    03  -COPY WDK626                                                     
086500     EJECT                                                                
086600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK701'.              
086700 01  DLI-IO-WDK701.                                                       
086800*    03  -COPY WDK701                                                     
086900     EJECT                                                                
087000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK711'.              
087100 01  DLI-IO-WDK711.                                                       
087200*    03  -COPY WDK711                                                     
087300     EJECT                                                                
087400 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDK901'.              
087500 01  DLI-IO-WDK901.                                                       
087600*    03  -COPY WDK901  -PRE ARTM-                                         
087700     EJECT                                                                
087800 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD901'.              
087900 01  DLI-IO-WDD901.                                                       
088000*    03 WLINLB01 -COPY WDD901    -PRE D9ART-                              
088100     EJECT                                                                
088200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD902'.              
088300 01  DLI-IO-WDD902.                                                       
088400*    03 WLINLB11 -COPY WDD902    -PRE D9LEV-                              
088500     EJECT                                                                
088600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD904'.              
088700 01  DLI-IO-WDD904.                                                       
088800*    03 WLINLB22 -COPY WDD904    -PRE OMSPEC-                             
088900     EJECT                                                                
089000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD905'.              
089100 01  DLI-IO-WDD905.                                                       
089200*    03 WLINLB23 -COPY WDD905    -PRE D9AVROP-                            
089300     EJECT                                                                
089400 01  DLI-IO-AREA             PIC X(200).                                  
089500     SKIP2                                                                
089600 01  WLINLB01 -COPY WDD901     -PRE ART-      -RED DLI-IO-AREA.           
089700     SKIP3                                                                
089800*01  WLINLB11 -COPY WDD902     -PRE LEV-      -RED DLI-IO-AREA.           
089900     EJECT                                                                
090000*01  WLINLB23 -COPY WDD905     -PRE AVROP-    -RED DLI-IO-AREA.           
090100     EJECT                                                                
090200                                                                          
090300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL201'.            
090400 01  DLI-IO-WDL201.                                                       
090500*    03 WLINLE01  -COPY WDL201   -PRE INLE-                               
090600     SKIP3                                                                
090700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL221'.            
090800 01  DLI-IO-WDL221.                                                       
090900*    03 WLINLE21  -COPY WDL221                                            
091000     EJECT                                                                
091100 01  FILLER                     PIC X(16)  VALUE 'DLI-IO-AREA-F1'.        
091200     SKIP3                                                                
091300 01  DLI-IO-AREA-F1          PIC X(100).                                  
091400     SKIP2                                                                
091500*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
091600     EJECT                                                                
091700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F106'.         
091800     SKIP3                                                                
091900 01  DLI-IO-AREA-F106.                                                    
092000     SKIP2                                                                
092100*    03  WLLEVA14 -COPY WDF106                                            
092200     EJECT                                                                
092300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F3A1'.         
092400     SKIP3                                                                
092500 01  DLI-IO-AREA-F3A1.                                                    
092600     SKIP2                                                                
092700*    03  -COPY WDF3A1                                                     
092800     EJECT                                                                
092900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F311'.         
093000     SKIP3                                                                
093100 01  DLI-IO-AREA-F311.                                                    
093200     SKIP2                                                                
093300*    03  -COPY WDF311                                                     
093400     EJECT                                                                
093500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDB601'.           
093600 01  DLI-IO-WDB601.                                                       
093700*    03  -COPY WDB601                                                     
093800     EJECT                                                                
093900 01  FILLER                  PIC X(16)  VALUE 'WDD3-IO-AREA'.             
094000     SKIP3                                                                
094100 01  WDD3-IO-AREA.                                                        
094200     SKIP2                                                                
094300*    03  -COPY WDD311                                                     
094400     EJECT                                                                
094500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDD601'.           
094600 01  DLI-IO-WDD601.                                                       
094700*    03  -COPY WDD601                                                     
094800     EJECT                                                                
094900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2258'.          
095000 01  DLI-IO-WDGX2258.                                                     
095100*    03 -COPY WDGX2258                                                    
095200     EJECT                                                                
095300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2260'.          
095400 01  DLI-IO-WDGX2260.                                                     
095500*    03 -COPY WDGX2260                                                    
095600     EJECT                                                                
095700                                                                          
095800                                                                          
095900 LINKAGE SECTION.                                                         
096000     SKIP3                                                                
096100*01  -COPY W0008  -PRE WDK6-                                              
096200     05  FILLER                  PIC X.                                   
096300     EJECT                                                                
096400*01  -COPY W0008  -PRE WDD91-                                             
096500     05  FILLER                  PIC X(7).                                
096600     05  WDD91-KEY-02-IDLEVNR    PIC X(5).                                
096700     EJECT                                                                
096800 01  W22X-AA-PCB                 PIC X.                                   
096900 01  W22X-ARTM-PCB               PIC X.                                   
097000     EJECT                                                                
097100 01  W22X-WDK7-PCB               PIC X.                                   
097200     EJECT                                                                
097300*01  -COPY W0008  -PRE WDD93-                                             
097400     05  FILLER                  PIC X.                                   
097500     EJECT                                                                
097600*01  -COPY W0008  -PRE WDL2-                                              
097700     05  FILLER                  PIC X.                                   
097800     EJECT                                                                
097900 01  W22X-2501-PCB               PIC X.                                   
098000     EJECT                                                                
098100 01  W22X-WDB6R-PCB              PIC X.                                   
098200     EJECT                                                                
098300 01  W22X-WDK7R-PCB              PIC X.                                   
098400     EJECT                                                                
098500*    -COPY W0008 -PRE WDF1-                                               
098600     05  FILLER                  PIC X.                                   
098700     EJECT                                                                
098800*01  -COPY W0008  -PRE WDF3-                                              
098900     05  FILLER                  PIC X.                                   
099000     EJECT                                                                
099100 01  W22X-WDB6-PCB               PIC X.                                   
099200     EJECT                                                                
099300*01  -COPY W0008  -PRE WDD3-                                              
099400     05  FILLER                  PIC X.                                   
099500     EJECT                                                                
099600 01  W22X-WDD7-PCB               PIC X.                                   
099700 01  W22X-WDK7E-PCB              PIC X.                                   
099800 01  W22X-UTIL-WDK6-PCB          PIC X.                                   
099900 01  W22X-UTIL-WDK7-PCB          PIC X.                                   
100000 01  W22X-UTIL-WDB6-PCB          PIC X.                                   
100100 01  W22X-UTUP-WDK7-PCB          PIC X.                                   
100200 01  W22X-UTUP-WDB6-PCB          PIC X.                                   
100300 01  W22X-UTUP-UTIL-WDK6-PCB     PIC X.                                   
100400 01  W22X-UTUP-UTIL-WDK7-PCB     PIC X.                                   
100500 01  W22X-UTUP-UTIL-WDB6-PCB     PIC X.                                   
100600     EJECT                                                                
100700*01  -COPY W0008  -PRE 2257-                                              
100800     05  FILLER                  PIC X.                                   
100900     EJECT                                                                
101000*01  -COPY W0008  -PRE WDB6-                                              
101100     05  FILLER                  PIC X.                                   
101200     EJECT                                                                
101300*01  -COPY W0008  -PRE WDK7-                                              
101400     05  FILLER                  PIC X.                                   
101500     EJECT                                                                
101600*01  -COPY W0008  -PRE WDK9-                                              
101700     05  FILLER                  PIC X.                                   
101800     EJECT                                                                
101900 PROCEDURE DIVISION USING WDK6-PCB  WDD91-PCB W22X-AA-PCB                 
102000                          W22X-ARTM-PCB W22X-WDK7-PCB                     
102100                          WDD93-PCB WDL2-PCB W22X-2501-PCB                
102200                          W22X-WDB6R-PCB W22X-WDK7R-PCB                   
102300                          WDF1-PCB  WDF3-PCB W22X-WDB6-PCB                
102400                          WDD3-PCB  W22X-WDD7-PCB W22X-WDK7E-PCB          
102500                          W22X-UTIL-WDK6-PCB W22X-UTIL-WDK7-PCB           
102600                          W22X-UTIL-WDB6-PCB W22X-UTUP-WDK7-PCB           
102700                          W22X-UTUP-WDB6-PCB                              
102800                          W22X-UTUP-UTIL-WDK6-PCB                         
102900                          W22X-UTUP-UTIL-WDK7-PCB                         
103000                          W22X-UTUP-UTIL-WDB6-PCB                         
103100                          2257-PCB WDB6-PCB WDK7-PCB WDK9-PCB.            
103200                                                                          
103300     ENTRY 'DLITCBL' USING WDK6-PCB  WDD91-PCB W22X-AA-PCB                
103400                           W22X-ARTM-PCB W22X-WDK7-PCB                    
103500                           WDD93-PCB WDL2-PCB W22X-2501-PCB               
103600                           W22X-WDB6R-PCB W22X-WDK7R-PCB                  
103700                           WDF1-PCB  WDF3-PCB W22X-WDB6-PCB               
103800                           WDD3-PCB  W22X-WDD7-PCB W22X-WDK7E-PCB         
103900                           W22X-UTIL-WDK6-PCB W22X-UTIL-WDK7-PCB          
104000                           W22X-UTIL-WDB6-PCB W22X-UTUP-WDK7-PCB          
104100                           W22X-UTUP-WDB6-PCB                             
104200                           W22X-UTUP-UTIL-WDK6-PCB                        
104300                           W22X-UTUP-UTIL-WDK7-PCB                        
104400                           W22X-UTUP-UTIL-WDB6-PCB                        
104500                           2257-PCB WDB6-PCB WDK7-PCB WDK9-PCB.           
104600                                                                          
104700                                                                          
104800     PERFORM A-INITIERA                                                   
104900     PERFORM B-LAES-BEGAERAN                                              
105000     SKIP1                                                                
105100     PERFORM UNTIL NOT(                                                   
105200        W22142-EOF       = NEJ)                                           
105300                                                                          
105400         INITIALIZE TAB-WDD905                                            
105500         MOVE +0    TO IX1-D9                                             
105600                                                                          
105700         PERFORM C-LAES-ARTIKELDATA                                       
105800     SKIP1                                                                
105900         IF  LINK-ANROP-OK                                                
106000     SKIP1                                                                
106100             MOVE LINK-KDLEVPLF TO W-KDLEVPLF                             
106200                                                                          
106300             PERFORM K-KONTROLL-AUT-PLAN                                  
106400                                                                          
106500             MOVE LAES-LEVERANTOER-DATA TO LINK-KDCALL                    
106600             PERFORM I-LAES-LEVERANTOER-DATA                              
106700                                                                          
106800             IF LINK-SEGMENT-SAKNAS                                       
106900                 MOVE NYUPPL-LEVERANTOER TO LINK-KDCALL                   
107000                 PERFORM J-NYUPPL-LEVERANTOER                             
107100             END-IF                                                       
107200             MOVE W-KDLEVPLF   TO LINK-KDLEVPLF                           
107300             PERFORM D-TYP-AV-OMSPEC                                      
107400     SKIP1                                                                
107500             IF  LINK-KDLPSP = 5                                          
107600                 PERFORM F-BORTTAG-OMSPEC-AVROP                           
107700             END-IF                                                       
107800     SKIP1                                                                
107900             IF  LINK-KDLPSP = 3                                          
108000             OR  (LINK-KDHF > ZERO  AND LINK-FLAVRART = JA)               
108100             OR   LINK-KDERS (1) > 9                                      
108200     SKIP1                                                                
108300                 MOVE I42BEG-KDLPORS-TAB (1)                              
108400                              TO  LINK-KDLPORS-TAB (1)                    
108500                 MOVE 26 TO  LINK-KDLPORS-TAB (2)                         
108600                 MOVE 27 TO  LINK-KDLPORS-TAB (3)                         
108700                 IF LINK-KDLPSP = 5                                       
108800                    MOVE ZERO TO LINK-KDLPSP                              
108900                 END-IF                                                   
109000                 MOVE NEJ TO SW-OMSPEC-UTFOERD                            
109100                 MOVE NEJ TO SW-SKIP-FORSLAG                              
109200             ELSE                                                         
109300*FIX-START JUL 2019 INGA PLANER 2147                                      
109400               IF W-DATUM-AAVV-AKT = 1951 OR 1952                         
109500                 IF  LINK-KDLPSP = 5                                      
109600                     MOVE ZERO TO LINK-KDLPSP                             
109700                 END-IF                                                   
109800                 MOVE NEJ TO SW-OMSPEC-UTFOERD                            
109900                 MOVE JA  TO SW-SKIP-FORSLAG                              
110000               ELSE                                                       
110100                 PERFORM G-OMSPEC-AV-LEVERANSPLAN                         
110200                 MOVE JA TO SW-OMSPEC-UTFOERD                             
110300***              * SKALL DET PRELIMINÄRA FÖRSLAGET BEHÅLLAS ?             
110400***              * (MEN ÄR FLJIT = JA   SÅ BEHÅLLS FÖRSLAGET)             
110500***              * (så även vid kdlpors = spara-forslag)                  
110600                 MOVE NEJ                  TO SW-FLORS                    
110700                 MOVE LINK-KDLPORS-TAB (1) TO WS-KDLPORS                  
110800                 IF WS-SPARA-FORSLAG                                      
110900                    MOVE JA                TO SW-FLORS                    
111000                 END-IF                                                   
111100                 MOVE LINK-KDLPORS-TAB (2) TO WS-KDLPORS                  
111200                 IF WS-SPARA-FORSLAG                                      
111300                    MOVE JA                TO SW-FLORS                    
111400                 END-IF                                                   
111500                 MOVE LINK-KDLPORS-TAB (3) TO WS-KDLPORS                  
111600                 IF WS-SPARA-FORSLAG                                      
111700                    MOVE JA                TO SW-FLORS                    
111800                 END-IF                                                   
111900                 IF LINK-FLJIT = JA OR SW-FLORS = JA                      
112000                   MOVE NEJ          TO SW-SKIP-FORSLAG                   
112100                 ELSE                                                     
112200                   MOVE LINK-IDARTNR TO W-IDARTNR-D9                      
112300                   MOVE WC-CDC-SE    TO W-IDDC-D9                         
112400                   MOVE LINK-IDLEVNR TO W-IDLEVNR                         
112500                   PERFORM IMS-GU-LEVERANTOER-WDD902                      
112600                   IF SEGMENT-FINNS OR (SW-WDD902-NYUPPL = JA)            
112700                      IF SW-FLORS-06  = NEJ                               
112800                         PERFORM M-KOLL-SKIP-PREL-PLAN                    
112900                      END-IF                                              
113000                   ELSE                                                   
113100                      MOVE NEJ    TO SW-SKIP-FORSLAG                      
113200                   END-IF                                                 
113300                 END-IF                                                   
113400*JUL-FIX JUL 2018 SLUT                                                    
113500               END-IF                                                     
113600             END-IF                                                       
113700                                                                          
113800             IF SW-SKIP-FORSLAG = NEJ                                     
113900               IF SW-OMSPEC-UTFOERD = JA                                  
114000                 PERFORM L1-KONTROLL-ASTERISK-W22146-MM                   
114100               ELSE                                                       
114200                 PERFORM L-KONTROLL-ASTERISK-W22146-MM                    
114300               END-IF                                                     
114400               PERFORM E-SKAPA-UTFILER                                    
114500             END-IF                                                       
114600             PERFORM H-KONTROLL-UPPD-AV-REGISTER                          
114700         ELSE                                                             
114800             DISPLAY 'IDARTNR ' LINK-IDARTNR ' SAKNAS'                    
114900         END-IF                                                           
115000         PERFORM B-LAES-BEGAERAN                                          
115100     END-PERFORM                                                          
115200                                                                          
115300     SKIP1                                                                
115400     PERFORM Z-AVSLUTA                                                    
115500     MOVE ZERO TO RETURN-CODE                                             
115600     GOBACK                                                               
115700     .                                                                    
115800     EJECT                                                                
115900 A-INITIERA SECTION.                                                      
116000******************************************************************        
116100*                                                                *        
116200*                                                                *        
116300*                                                                *        
116400******************************************************************        
116500     SKIP1                                                                
116600     OPEN INPUT W22142                                                    
116700                                                                          
116800     OPEN OUTPUT W22151                                                   
116900                 W22152                                                   
117000                 W22154                                                   
117100                 W22159                                                   
117200                 W2215A                                                   
117300     SKIP1                                                                
117400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
117500     SKIP1                                                                
117600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
117700                                                                          
117800     MOVE D-AAR      TO W-DATUM-AA AKT-DATUM-AR                           
117900     MOVE D-VECKA    TO W-DATUM-VV                                        
118000     MOVE W-DATUM-AAVV TO W-DATUM-AAVV-AKT AKT-DATUM-AAVV                 
118100                        LINK-TIAAVV-AKT                                   
118200     MOVE D-MAANAD   TO AKT-DATUM-MM                                      
118300     MOVE D-DAG      TO AKT-DATUM-DD                                      
118400***                                                                       
118500     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
118600     MOVE W-DATUM-AAVV   TO DAT-I-TIDATUM                                 
118700                                                                          
118800     CALL WDATKONV USING DAT-KDDATFORM                                    
118900                         DAT-I-TIDATUM                                    
119000                         DAT-O-TIDATUM                                    
119100                         DAT-KDSVAR                                       
119200                                                                          
119300     MOVE 'AARP  '       TO DAT-KDDATFORM                                 
119400     MOVE DAT-TIRP       TO DAT-I-TIDATUM                                 
119500                                                                          
119600     CALL WDATKONV USING DAT-KDDATFORM                                    
119700                         DAT-I-TIDATUM                                    
119800                         DAT-O-TIDATUM                                    
119900                         DAT-KDSVAR                                       
120000*    STARTVECKA FÖR PERIODEN + ANTAL VECKOR I PERIODEN = AKT VV ?         
120100     IF W-DATUM-VV = ( DAT-TIVV + DAT-KVVIPER - 1)                        
120200        IF DAT-TIRP = 2 OR 4 OR 6 OR 8 OR 10 OR 12                        
120300           MOVE JA TO SW-PERSLUT                                          
120400        END-IF                                                            
120500     END-IF                                                               
120600                                                                          
120700**    HÄR LÄSER VI IN HELGDAGSTAB LAND+DATUM                              
120800     PERFORM AA-FYLL-HELG-TAB                                             
120900                                                                          
121000**    HÄR LÄSER VI IN ALLA WDB601 TILL IDDC-TABELLEN I WS                 
121100**    FÖR ATT SLIPPA IMS-CALL FÖR VARJE ARTIKEL PÅ INFILEN.               
121200     PERFORM AB-LAES-WDB6-DC-INFO                                         
121300                                                                          
121400                                                                          
121500     MOVE WC-CDC-SE  TO W-IDDC-MIN                                        
121600                        W-IDDC-MAX                                        
121700     .                                                                    
121800     EJECT                                                                
121900 AA-FYLL-HELG-TAB  SECTION.                                               
122000     SKIP3                                                                
122100     PERFORM IMS-GU-WDF3A1                                                
122200     MOVE +1                   TO IX-HELG                                 
122300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
122400        IF IX-HELG > MAX-HELG                                             
122500           MOVE '** FEL - TABELL HELGDAGAR FULL ** MAX-HELG '             
122600                              TO FELTEXT                                  
122700           CALL FELLOG                                                    
122800        END-IF                                                            
122900        MOVE HLGA-IDLANDX2     TO TAB-IDLANDX2     (IX-HELG)              
123000        MOVE HLGA-DADATUM-HELG TO TAB-DADATUM-HELG (IX-HELG)              
123100        MOVE HLGA-FLHELG       TO TAB-FLHELG       (IX-HELG)              
123200        ADD +1                 TO IX-HELG                                 
123300        PERFORM IMS-GN-WDF3A1                                             
123400     END-PERFORM                                                          
123500                                                                          
123600     MOVE IX-HELG              TO   ANT-HELG                              
123700     SUBTRACT 1                FROM ANT-HELG                              
123800     .                                                                    
123900     EJECT                                                                
124000 AB-LAES-WDB6-DC-INFO  SECTION.                                           
124100     MOVE 'AB-LAES-WDB6-DC-INFO '  TO CURRENT-SECTION                     
124200*--  M-LAES-WDB6-DC-INFO I PGM W2215010                                   
124300     SKIP2                                                                
124400******************************************************************        
124500*                                                                *        
124600*    LÄS IDDC-BASEN WDB6 OCH SKAPA EN TABELL MED ALLA SEGMENT    *        
124700*    FÖR ATT SLIPPA LÄSA WDB6 FÖR VARJE ARTIKEL PÅ INFILEN.      *        
124800*    ( LÖNAR SIG OM FILEN HAR FLER POSTER ÄN WDB601 )            *        
124900*                                                                *        
125000******************************************************************        
125100     SET DCIX TO +1                                                       
125200     PERFORM IMS-GN-WDB601                                                
125300                                                                          
125400     PERFORM UNTIL SEGMENT-SLUT                                           
125500       IF DCIX <= DC-MAX                                                  
125600         MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                                
125700         MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                                
125800         MOVE DCS-FLOVRLAGBER TO T-DCS-FLOVRLAGBER(DCIX)                  
125900*                                                                         
126000         SET DCIX UP BY +1                                                
126100                                                                          
126200         PERFORM IMS-GN-WDB601                                            
126300       ELSE                                                               
126400                                                                          
126500           MOVE +35 TO RKOD                                               
126600           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT2-STR              
126700           DISPLAY FELTEXT2                                               
126800           CALL ABEND USING RKOD                                          
126900       END-IF                                                             
127000     END-PERFORM                                                          
127100                                                                          
127200*    --- SÄTTER TAKET PÅ TABELLEN                                         
127300     SET DCIX   DOWN BY +1                                                
127400     SET DC-MAX TO DCIX                                                   
127500                                                                          
127600*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
127700     MOVE DC-MAX                  TO ANTAL                                
127800     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
127900     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
128000                                                                          
128100     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
128200                  T-DCS-IDDC(1) NYCKELLANGD                               
128300     .                                                                    
128400     EJECT                                                                
128500 B-LAES-BEGAERAN SECTION.                                                 
128600     SKIP3                                                                
128700     READ W22142 INTO I42BEG-AREA                                         
128800       AT END                                                             
128900          MOVE HIGH-VALUE TO I42BEG-AREA                                  
129000          MOVE JA TO  W22142-EOF                                          
129100     END-READ                                                             
129200     SKIP1                                                                
129300     IF W22142-EOF = NEJ                                                  
129400         MOVE 'W22142'       TO POSTSUM-FDNAMN                            
129500         MOVE 'W22150D1'     TO POSTSUM-DDNAMN2                           
129600         MOVE SPACE          TO POSTSUM-TRANSTYP                          
129700         CALL POSTSUM USING POSTSUM-PARM                                  
129800     END-IF                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 C-LAES-ARTIKELDATA SECTION.                                              
130200******************************************************************        
130300*                                                                *        
130400*    LÄSNING AV ARTIKELDATA FRÅN ARTIKELREG OCH                  *        
130500*    LEVERANSPLAN                                                *        
130600*                                                                *        
130700******************************************************************        
130800     SKIP3                                                                
130900     MOVE I42BEG-IDARTNR TO LINK-IDARTNR                                  
131000     MOVE LAES-ARTIKEL-DATA TO LINK-KDCALL                                
131100     PERFORM CA-LAES-ARTIKEL-DATA                                         
131200                                                                          
131300     MOVE LINK-TILEVDAG (1) TO WART-TILEVDAG (1)                          
131400     MOVE LINK-TILEVDAG (2) TO WART-TILEVDAG (2)                          
131500     MOVE LINK-TILEVDAG (3) TO WART-TILEVDAG (3)                          
131600     MOVE LINK-TILEVDAG (4) TO WART-TILEVDAG (4)                          
131700     MOVE LINK-TILEVDAG (5) TO WART-TILEVDAG (5)                          
131800     MOVE LINK-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                             
131900     SKIP3                                                                
132000*    *******  SEMESTERJUSTERING  **************                           
132100       MOVE 28 TO SEMESTER-VECKA-START                                    
132200       MOVE 31 TO SEMESTER-VECKA-SLUT                                     
132300       IF LINK-IDARTNR = 271794 OR 271788                                 
132400          MOVE 29 TO SEMESTER-VECKA-START                                 
132500          MOVE 30 TO SEMESTER-VECKA-SLUT                                  
132600       END-IF                                                             
132700     .                                                                    
132800     EJECT                                                                
132900 CA-LAES-ARTIKEL-DATA SECTION.                                            
133000     MOVE 'CA-LAES-ARTIKEL-DATA'  TO CURRENT-SECTION                      
133100*--- SE A-LAES-ARTIKEL-DATA I PGM W2215010                                
133200     SKIP3                                                                
133300     MOVE NEJ TO LINK-FLJANEJ-ANROP                                       
133400     MOVE LINK-IDARTNR TO W-IDARTNR                                       
133500     SKIP1                                                                
133600     PERFORM IMS-GU-ARTIKEL-WDK601                                        
133700     IF SEGMENT-FINNS                                                     
133800     AND K601-ART-KDERS-UTG = ZERO                                        
133900         MOVE JA           TO LINK-FLJANEJ-ANROP                          
134000         MOVE K601-ART-IDARTNR TO LINK-IDARTNR                            
134100         MOVE K601-ART-KDPRODSL TO LINK-KDPRODSL                          
134200         MOVE K601-ART-TIFINLV TO LINK-TIFINLV                            
134300         MOVE K601-ART-IDLEVNR TO LINK-IDLEVNR                            
134400     SKIP1                                                                
134500         PERFORM IMS-GNP-CLAG-WDK611                                      
134600         IF SEGMENT-FINNS                                                 
134700            MOVE CLAG-IDLEVNR-SHIP     TO LINK-IDLEVNR-SHIP               
134800            MOVE CLAG-FLAVRART         TO LINK-FLAVRART                   
134900            MOVE CLAG-FLJIT            TO LINK-FLJIT                      
135000            MOVE CLAG-FLMANQ           TO LINK-FLMANQ                     
135100            MOVE CLAG-IDANSK           TO LINK-IDANSK                     
135200            MOVE CLAG-KDAVT            TO LINK-KDAVT                      
135300            MOVE CLAG-KDHF             TO LINK-KDHF                       
135400            MOVE CLAG-KDEFFMAN         TO LINK-KDEFFMAN                   
135500            MOVE CLAG-ADLAGOMR         TO LINK-ADLAGOMR                   
135600            MOVE CLAG-KDLPSP           TO LINK-KDLPSP                     
135700            MOVE CLAG-KVBK             TO LINK-KVBK                       
135800            MOVE CLAG-KVDAGAR-INLEV    TO LINK-KVDAGAR-INLEV              
135900            MOVE CLAG-KVDAGAR-TT       TO LINK-KVDAGAR-TT                 
136000            MOVE CLAG-KVLAAN           TO LINK-KVLAAN                     
136100            MOVE CLAG-KVVECKOR-BT      TO LINK-KVVECKOR-BT                
136200            MOVE CLAG-KVVECKOR-FT      TO LINK-KVVECKOR-FT                
136300            MOVE CLAG-KVVECKOR-LT      TO LINK-KVVECKOR-LT                
136400            MOVE CLAG-TIOMSPEC         TO LINK-TIOMSPEC                   
136500            MOVE CLAG-TILPSP           TO LINK-TILPSP                     
136600            MOVE CLAG-KDVVKL           TO LINK-KDVVKL                     
136700            MOVE CLAG-KVQ              TO LINK-KVQ                        
136800            MOVE CLAG-KVQ-JUST         TO LINK-KVQ-JUST                   
136900            MOVE CLAG-TIQJUST          TO LINK-TIQJUST                    
137000            MOVE CLAG-KDLEVPLF         TO LINK-KDLEVPLF                   
137100            MOVE CLAG-KVSLUTKP         TO LINK-KVSLUTKP                   
137200            MOVE CLAG-KVUTRS           TO LINK-KVUTRS                     
137300            MOVE CLAG-TIAVIDAT-SEN     TO LINK-TIAVIDAT-SEN               
137400                                                                          
137500            MOVE CLAG-FLOREGPB     TO LINK-FLOREGPB (1)                   
137600            MOVE CLAG-RVPROURS     TO LINK-RVPROURS (1)                   
137700            MOVE CLAG-RVPROFEL     TO LINK-RVPROFEL (1)                   
137800            MOVE CLAG-KVPB-SEP     TO LINK-KVPB-TOT (1)                   
137900            ADD  CLAG-KVPB-SATS    TO LINK-KVPB-TOT (1)                   
138000                                                                          
138100            MOVE CLAG-IDLKTO     TO LINK-IDLKTO                           
138200                                                                          
138300            MOVE CLAG-KDUART     TO LINK-KDUART                           
138400            MOVE CLAG-KDOPPLAN   TO LINK-KDOPPLAN                         
138500                                                                          
138600            MOVE CLAG-KDERS      TO LINK-KDERS (1)                        
138700            MOVE CLAG-KVAKS-PAV  TO LINK-KVAKS-PAV                        
138800            MOVE CLAG-KVAKS-CDC  TO LINK-KVAKS (1)                        
138900            ADD  CLAG-KVAKS-PAV  TO LINK-KVAKS (1)                        
139000            ADD  CLAG-KVAKS-T    TO LINK-KVAKS (1)                        
139100                                                                          
139200            IF LINK-KVAKS (1) > ZERO                                      
139300               PERFORM CAA-JUSTERA-MED-RETURER                            
139400            END-IF                                                        
139500                                                                          
139600            MOVE CLAG-KVLS       TO LINK-KVLS (1)                         
139700            MOVE CLAG-KVRESS     TO LINK-KVRESS (1)                       
139800            MOVE CLAG-KVROS      TO LINK-KVROS (1)                        
139900            MOVE CLAG-KVSLAGER   TO LINK-KVSLAGER (1)                     
140000                                                                          
140100*** HÄMTA BEST.PRIS FRÅN WDK621                                           
140200            MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD            
140300            COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD               
140400            PERFORM IMS-GNP-PRIS-WDK621                                   
140500            IF SEGMENT-SAKNAS                                             
140600              MOVE CLAG-PRARTSTD       TO LINK-PRARTBES                   
140700            ELSE                                                          
140800              MOVE NEJ                 TO WS-PRARTBES                     
140900              PERFORM UNTIL  SEGMENT-SAKNAS                               
141000                IF PRL-SUINLEV-PR > ZERO                                  
141100                  MOVE PRL-PRARTBES-PR  TO LINK-PRARTBES                  
141200                  SET SEGMENT-SAKNAS TO TRUE                              
141300                ELSE                                                      
141400                  IF WS-PRARTBES = NEJ                                    
141500                    MOVE PRL-PRARTBES-PR TO LINK-PRARTBES                 
141600                    MOVE JA              TO WS-PRARTBES                   
141700                  END-IF                                                  
141800                  PERFORM IMS-GNP-PRIS-WDK621                             
141900                END-IF                                                    
142000              END-PERFORM                                                 
142100            END-IF                                                        
142200*** HÄMTA BEST.PRIS FRÅN WDK621                                           
142300                                                                          
142400            MOVE CLAG-KVPALL        TO LINK-KVPALL                        
142500            MOVE CLAG-KVQPACK-1     TO LINK-KVQPACK-1                     
142600            MOVE CLAG-KVPB-SEP      TO LINK-KVPB-SEP                      
142700            MOVE CLAG-KVPB-SATS     TO LINK-KVPB-SATS                     
142800            MOVE CLAG-KVPB-TPO      TO LINK-KVPB-TPO                      
142900            MOVE CLAG-FLNYBER       TO LINK-FLNYBER                       
143000            MOVE CLAG-KVEOQ         TO LINK-KVEOQ                         
143100            MOVE CLAG-KVULOAD       TO LINK-KVULOAD                       
143200                                                                          
143300            MOVE CLAG-FLSKROT-BEV   TO LINK-FLSKROT-BEV                   
143400            MOVE CLAG-FLSKROT-WLC   TO LINK-FLSKROT-WLC                   
143500            MOVE CLAG-TISKPREL      TO LINK-TISKPREL                      
143600                                                                          
143700            MOVE CLAG-TILEVDAG (1)  TO LINK-TILEVDAG (1)                  
143800            MOVE CLAG-TILEVDAG (2)  TO LINK-TILEVDAG (2)                  
143900            MOVE CLAG-TILEVDAG (3)  TO LINK-TILEVDAG (3)                  
144000            MOVE CLAG-TILEVDAG (4)  TO LINK-TILEVDAG (4)                  
144100            MOVE CLAG-TILEVDAG (5)  TO LINK-TILEVDAG (5)                  
144200         END-IF                                                           
144300                                                                          
144400         MOVE SPACE          TO LINK-FLOREGPB (2)                         
144500         MOVE ZERO           TO LINK-RVPROURS (2)                         
144600         MOVE ZERO           TO LINK-RVPROFEL (2)                         
144700         MOVE ZERO           TO LINK-KVPB-TOT (2)                         
144800                                                                          
144900         MOVE ZERO           TO LINK-KDERS (2)                            
145000                                LINK-KVAKS (2)                            
145100                                LINK-KVLS (2)                             
145200                                LINK-KVRESS (2)                           
145300                                LINK-KVROS (2)                            
145400                                LINK-KVSLAGER (2)                         
145500                                                                          
145600         MOVE ZERO           TO  W-KVPB-SDC                               
145700                                 W-TILLG-SDC                              
145800                                 W-OVERLAGER-SDC                          
145900                                                                          
146000         MOVE NEJ     TO LINK-FLRESEASON                                  
146100         PERFORM IMS-GNP-JUST-WDK626                                      
146200         IF  SEGMENT-FINNS                                                
146300           MOVE 1 TO IX                                                   
146400           PERFORM UNTIL                                                  
146500            ( IX > 12 )                                                   
146600             IF JUST-RESEASON (IX) NOT = 1                                
146700                MOVE JA TO LINK-FLRESEASON                                
146800             END-IF                                                       
146900             ADD +1 TO IX                                                 
147000           END-PERFORM                                                    
147100         END-IF                                                           
147200                                                                          
147300                                                                          
147400         MOVE NEJ     TO LINK-FLASTERISK                                  
147500                                                                          
147600         PERFORM IMS-GU-WDK701-SDC                                        
147700                                                                          
147800*--  SKALL BARA LÄSA DE MED IDDC-REF = 11                                 
147900         IF SEGMENT-FINNS                                                 
148000            PERFORM IMS-GNP-WDK711-SDC-REF                                
148100            PERFORM UNTIL SEGMENT-SAKNAS                                  
148200               ADD SLAG-KVPB-REF   TO   W-KVPB-SDC                        
148300               MOVE ZERO           TO   W-TILLG-SDC                       
148400               ADD SLAG-KVLS       TO   W-TILLG-SDC                       
148500               ADD SLAG-KVBEART    TO   W-TILLG-SDC                       
148600               ADD SLAG-KVAKS-SDC  TO   W-TILLG-SDC                       
148700               ADD SLAG-KVAKS-PAV  TO   W-TILLG-SDC                       
148800**** FIX FÖR NEGATIVA KVOKS                                               
148900               IF SLAG-KVOKS-BULK > 0                                     
149000                 SUBTRACT SLAG-KVOKS-BULK FROM W-TILLG-SDC                
149100               END-IF                                                     
149200               IF SLAG-KVOKS-DAG > 0                                      
149300                 SUBTRACT SLAG-KVOKS-DAG FROM W-TILLG-SDC                 
149400               END-IF                                                     
149500                                                                          
149600               MOVE SLAG-IDDC TO WS-IDDC                                  
149700               IF NDC                                                     
149800                 CONTINUE                                                 
149900               ELSE                                                       
150000                 SET DCIX TO +1                                           
150100                 SEARCH DC-TAB                                            
150200                    AT END                                                
150300                      MOVE NEJ TO DCS-TRAEFF                              
150400                    WHEN T-DCS-IDDC(DCIX) = SLAG-IDDC                     
150500                      MOVE JA  TO DCS-TRAEFF                              
150600                      CONTINUE                                            
150700                 END-SEARCH                                               
150800                                                                          
150900*                -- KOLLA SDC-LAGER (POS 1 = S)                           
151000                 IF DCS-TRAEFF = JA                                       
151100                   IF T-DCS-KDDC(DCIX)(1:1) = 'S'                         
151200*                 -- KOLLA OM ÖVERLAGERBERÄKNING PÅ SDC SKA GÖRAS         
151300                     IF T-DCS-FLOVRLAGBER(DCIX) = NEJ                     
151400                       CONTINUE                                           
151500                     ELSE                                                 
151600                       IF SLAG-KVREFOVL < W-TILLG-SDC                     
151700                         COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC        
151800                                              + W-TILLG-SDC               
151900                                              - SLAG-KVREFOVL             
152000                       END-IF                                             
152100                     END-IF                                               
152200                   END-IF                                                 
152300                 ELSE                                                     
152400*                  -- SDC ÄR EJ REGISTRERAT PÅ WDB6. HOPPA !              
152500*                  -- BORDE EGENTLIGEN ALDRIG INTRÄFFA'                   
152600                   DISPLAY 'IDDC ' SLAG-IDDC ' EJ REG PÅ WDB6'            
152700                   CONTINUE                                               
152800                 END-IF                                                   
152900               END-IF                                                     
153000               MOVE LINK-TIAAVV-AKT TO DAGENS-AAVV                        
153100               IF DAGENS-AA > 50                                          
153200                  MOVE 19             TO DAGENS-SS                        
153300               ELSE                                                       
153400                  MOVE 20             TO DAGENS-SS                        
153500               END-IF                                                     
153600               COMPUTE DAGENS-ABS-VV = DAGENS-SSAA * 52                   
153700                                     + DAGENS-VV                          
153800               MOVE K601-ART-TIFINLV  TO TIFINLV-AAVVD                    
153900               IF TIFINLV-AA > 50                                         
154000                  MOVE 19             TO TIFINLV-SS                       
154100               ELSE                                                       
154200                  MOVE 20             TO TIFINLV-SS                       
154300               END-IF                                                     
154400               COMPUTE TIFINLV-ABS-VV = TIFINLV-SSAA * 52                 
154500                                      + TIFINLV-VV                        
154600***            MOVE TIFINLV-ABS-VV    TO TMP1-YYWWD                       
154700***            MOVE 52                TO TMP2-YYWWD                       
154800***            PERFORM WY2000P2                                           
154900***************IF (DAGENS-ABS-VV - TMP1-YYWWD < TMP2-YYWWD                
155000               IF (DAGENS-ABS-VV - TIFINLV-ABS-VV) < 52                   
155100                  MOVE ZERO TO W-OVERLAGER-SDC                            
155200               END-IF                                                     
155300               PERFORM IMS-GNP-WDK711-SDC-REF                             
155400            END-PERFORM                                                   
155500         END-IF                                                           
155600                                                                          
155700         MOVE W-OVERLAGER-SDC TO LINK-KVLS-SDC-OVER                       
155800                                                                          
155900         COMPUTE W-KVPB-SDC ROUNDED =  W-KVPB-SDC                         
156000                                                                          
156100         MOVE W-KVPB-SDC      TO LINK-KVPB-SDC                            
156200                                                                          
156300         PERFORM IMS-GET-ARTM-ART-SEG                                     
156400         IF SEGMENT-FINNS                                                 
156500*** FIX FÖR NEGATIVA KVOKS                                                
156600           IF ARTM-ART-KVOKS-BULK > 0                                     
156700             MOVE ARTM-ART-KVOKS-BULK    TO LINK-KVOKS-BULK (1)           
156800           ELSE                                                           
156900             MOVE +0                     TO LINK-KVOKS-BULK (1)           
157000           END-IF                                                         
157100           IF ARTM-ART-KVOKS-DAG > 0                                      
157200             MOVE ARTM-ART-KVOKS-DAG     TO LINK-KVOKS-DAG  (1)           
157300           ELSE                                                           
157400             MOVE +0                     TO LINK-KVOKS-DAG (1)            
157500           END-IF                                                         
157600           IF ARTM-ART-KVOKS-VOR > 0                                      
157700             MOVE ARTM-ART-KVOKS-VOR     TO LINK-KVOKS-VOR  (1)           
157800           ELSE                                                           
157900             MOVE +0                     TO LINK-KVOKS-VOR (1)            
158000           END-IF                                                         
158100           MOVE ZERO                     TO LINK-KVOKS-BULK (2)           
158200                                            LINK-KVOKS-DAG  (2)           
158300                                            LINK-KVOKS-VOR  (2)           
158400         ELSE                                                             
158500            MOVE ZERO                    TO LINK-KVOKS-BULK (1)           
158600                                            LINK-KVOKS-BULK (2)           
158700                                            LINK-KVOKS-DAG  (1)           
158800                                            LINK-KVOKS-DAG  (2)           
158900                                            LINK-KVOKS-VOR  (1)           
159000                                            LINK-KVOKS-VOR  (2)           
159100         END-IF                                                           
159200     END-IF                                                               
159300     .                                                                    
159400     EJECT                                                                
159500 CAA-JUSTERA-MED-RETURER SECTION.                                         
159600     MOVE 'CAA-JUSTERA-MED-RETURER '  TO CURRENT-SECTION                  
159700                                                                          
159800     PERFORM IMS-GET-INLE01-WDL201                                        
159900     IF SEGMENT-FINNS                                                     
160000        MOVE ZERO TO SUM-RETUR                                            
160100        PERFORM IMS-GET-INLE21-WDL221                                     
160200        PERFORM UNTIL SEGMENT-SAKNAS                                      
160300           IF MOT-KDRT = 7 OR 77                                          
160400              COMPUTE SUM-RETUR = SUM-RETUR +                             
160500                      MOT-KVAVIS - MOT-KVANTMOT                           
160600           END-IF                                                         
160700           PERFORM IMS-GET-INLE21-WDL221                                  
160800        END-PERFORM                                                       
160900        IF SUM-RETUR < ZERO                                               
161000           MOVE ZERO TO SUM-RETUR                                         
161100        END-IF                                                            
161200        SUBTRACT SUM-RETUR      FROM LINK-KVAKS (1)                       
161300        IF LINK-KVAKS (1) < ZERO                                          
161400           MOVE ZERO TO LINK-KVAKS (1)                                    
161500        END-IF                                                            
161600     END-IF                                                               
161700     .                                                                    
161800     EJECT                                                                
161900                                                                          
162000     EJECT                                                                
162100 D-TYP-AV-OMSPEC SECTION.                                                 
162200******************************************************************        
162300*                                                                *        
162400*                                                                *        
162500*                                                                *        
162600******************************************************************        
162700     SKIP1                                                                
162800     MOVE NEJ TO SW-OPTIMAL-OMSPEC   SW-X-OPT                             
162900     SKIP1                                                                
163000     IF  I42BEG-KDLPORS-TAB (1) = 18                                      
163100     OR  I42BEG-KDLPORS-TAB (2) = 18                                      
163200     OR  I42BEG-KDLPORS-TAB (3) = 18                                      
163300         MOVE JA TO SW-OPTIMAL-OMSPEC                                     
163400     END-IF                                                               
163500                                                                          
163600     IF  I42BEG-KDLPORS-TAB (1) = 20                                      
163700     OR  I42BEG-KDLPORS-TAB (2) = 20                                      
163800     OR  I42BEG-KDLPORS-TAB (3) = 20                                      
163900         MOVE JA TO SW-X-OPT                                              
164000     END-IF                                                               
164100                                                                          
164200     IF  LINK-KDOPPLAN = JA  OR                                           
164300        (LINK-KDOPPLAN = 'X' AND                                          
164400         SW-X-OPT = JA)                                                   
164500         MOVE LINK-IDLEVNR    TO OLIKA-LEV                                
164600         IF LINK-KDHF > 0                                                 
164700         OR SATS-LEVNR                                                    
164800         OR GEMEN-LEVNR                                                   
164900            MOVE NEJ TO SW-OPTIMAL-OMSPEC                                 
165000         ELSE                                                             
165100            MOVE JA TO SW-OPTIMAL-OMSPEC                                  
165200         END-IF                                                           
165300     END-IF                                                               
165400                                                                          
165500     .                                                                    
165600     EJECT                                                                
165700 E-SKAPA-UTFILER SECTION.                                                 
165800******************************************************************        
165900*                                                                *        
166000*    UTFILER REDIGERAS OCH SKRIVS                                *        
166100*       - W22151  FÖR FRAMST. AV LEVERANSPLANEKONCEPT            *        
166200*       - W22152  FÖR FRAMST. AV LEVERANSPLANESTATISTIK          *        
166300*       - W22154  FÖR FRAMST. AV AUTOMATISKA LEVERANSPLANER      *        
166400*       - W22159  FÖR Uppdateringsposter till WDD6 (Lev.planekön)*        
166500*                                                                *        
166600******************************************************************        
166700     SKIP1                                                                
166800     IF SW-AUT-PLAN = NEJ                                                 
166900        MOVE WC-CDC-SE      TO U59-LPF-IDDC                               
167000        MOVE LINK-IDANSK    TO U51KONC-IDANSK                             
167100                               U59-LPF-IDANSK                             
167200        MOVE LINK-IDLEVNR   TO U51KONC-IDLEVNR                            
167300                               U59-LPF-IDLEVNR                            
167400        MOVE LINK-IDARTNR   TO U51KONC-IDARTNR                            
167500                               U59-LPF-IDARTNR                            
167600        MOVE SW-AUT-PLAN    TO U51KONC-FLLEVPLA                           
167700        MOVE LINK-TIOMSPEC  TO U51KONC-TIOMSPEC-FOREG                     
167800                               U59-LPF-TIOMSPEC                           
167900        MOVE  LINK-KDLPORS-TAB (1) TO U51KONC-KDLPORS-TAB (1)             
168000                                      U59-LPF-KDLPORS (1)                 
168100        MOVE  LINK-KDLPORS-TAB (2) TO U51KONC-KDLPORS-TAB (2)             
168200                                      U59-LPF-KDLPORS (2)                 
168300        MOVE  LINK-KDLPORS-TAB (3) TO U51KONC-KDLPORS-TAB (3)             
168400                                      U59-LPF-KDLPORS (3)                 
168500       PERFORM EA-SKRIV-W22151                                            
168600                                                                          
168700       Move W-KDLEVPLF       To U59-LPF-KDLEVPLF                          
168800       Move AKT-DATUM-AAMMDD To U59-LPF-TIUPPDAT                          
168900       Move LINK-IDARTNR     To W-IDARTNR                                 
169000       Perform IMS-GET-WDD311-SVENSKA                                     
169100       Move Space            To U59-LPF-BEART                             
169200       If SEGMENT-FINNS                                                   
169300          Move TEXT-BEART    To U59-LPF-BEART                             
169400       End-If                                                             
169500**     -- Filter out some supplier ID's for update                        
169600       If U59-LPF-IDLEVNR = '8265 '                                       
169700                         Or '9996 '                                       
169800                         Or '9997 '                                       
169900                         Or '9998 '                                       
170000                         Or 'BQ8VA'                                       
170100                         Or 'BW5JA'                                       
170200                         Or '2589A'                                       
170300         Continue                                                         
170400       Else                                                               
170500         Perform ED-SKRIV-W22159                                          
170600       End-if                                                             
170700     ELSE                                                                 
170800        MOVE LINK-IDARTNR   TO U54AUT-IDARTNR                             
170900        MOVE LINK-IDLEVNR   TO U54AUT-IDLEVNR                             
171000        MOVE LINK-IDANSK    TO U54AUT-IDANSK                              
171100        MOVE 'F'            TO U54AUT-KDBEHX-PLA                          
171200        MOVE '2'            TO U54AUT-KOMKOD                              
171300        PERFORM EC-SKRIV-W22154                                           
171400        IF SW-X-OPT = JA OR (SW-FLORS-06 = JA)                            
171500**         OM KDOPPLAN = X OCH DET ÄR LÄGE FÖR EX-LEV                     
171600**         BLIR DET OPTIMAL PLAN OCH ALLTID KONCEPT (ÄVEN FÖR AUT)        
171700           MOVE LINK-IDANSK    TO U51KONC-IDANSK                          
171800                                  U59-LPF-IDANSK                          
171900           MOVE LINK-IDLEVNR   TO U51KONC-IDLEVNR                         
172000                                  U59-LPF-IDLEVNR                         
172100           MOVE LINK-IDARTNR   TO U51KONC-IDARTNR                         
172200                                  U59-LPF-IDARTNR                         
172300           MOVE SW-AUT-PLAN    TO U51KONC-FLLEVPLA                        
172400           MOVE LINK-TIOMSPEC  TO U51KONC-TIOMSPEC-FOREG                  
172500                                  U59-LPF-TIOMSPEC                        
172600           MOVE  LINK-KDLPORS-TAB (1) TO U51KONC-KDLPORS-TAB (1)          
172700                                         U59-LPF-KDLPORS (1)              
172800           MOVE  LINK-KDLPORS-TAB (2) TO U51KONC-KDLPORS-TAB (2)          
172900                                         U59-LPF-KDLPORS (2)              
173000           MOVE  LINK-KDLPORS-TAB (3) TO U51KONC-KDLPORS-TAB (3)          
173100                                         U59-LPF-KDLPORS (3)              
173200           PERFORM EA-SKRIV-W22151                                        
173300                                                                          
173400           Move W-KDLEVPLF       To U59-LPF-KDLEVPLF                      
173500           Move AKT-DATUM-AAMMDD To U59-LPF-TIUPPDAT                      
173600           Move LINK-IDARTNR     To W-IDARTNR                             
173700           Perform IMS-GET-WDD311-SVENSKA                                 
173800           Move Space            To U59-LPF-BEART                         
173900           If SEGMENT-FINNS                                               
174000              Move TEXT-BEART    To U59-LPF-BEART                         
174100           End-If                                                         
174200**         -- Filter out some supplier ID's for update                    
174300           If U59-LPF-IDLEVNR = '8265 '                                   
174400                             Or '9996 '                                   
174500                             Or '9997 '                                   
174600                             Or '9998 '                                   
174700                             Or 'BQ8VA'                                   
174800                             Or 'BW5JA'                                   
174900                             Or '2589A'                                   
175000             Continue                                                     
175100           Else                                                           
175200             Perform ED-SKRIV-W22159                                      
175300           End-if                                                         
175400        END-IF                                                            
175500     END-IF                                                               
175600                                                                          
175700     MOVE LINK-IDARTNR   TO U52STAT-IDARTNR                               
175800     MOVE LINK-IDANSK    TO U52STAT-IDANSK                                
175900     MOVE LINK-KDVVKL    TO U52STAT-KDVVKL                                
176000     MOVE I42BEG-KDLPORS-TAB (1) TO U52STAT-KDLPORS                       
176100     PERFORM EB-SKRIV-W22152                                              
176200     EJECT                                                                
176300     .                                                                    
176400 EA-SKRIV-W22151 SECTION.                                                 
176500     SKIP1                                                                
176600     WRITE U51KONC-POST FROM U51KONC-AREA                                 
176700     SKIP1                                                                
176800     MOVE 'W22151'           TO POSTSUM-FDNAMN                            
176900     MOVE 'W22150D2'         TO POSTSUM-DDNAMN2                           
177000     MOVE 'KONC'             TO POSTSUM-TRANSTYP                          
177100     CALL POSTSUM USING POSTSUM-PARM                                      
177200     SKIP3                                                                
177300     .                                                                    
177400 EB-SKRIV-W22152 SECTION.                                                 
177500     SKIP1                                                                
177600     WRITE U52STAT-POST FROM U52STAT-AREA                                 
177700     SKIP1                                                                
177800     MOVE 'W22152'           TO POSTSUM-FDNAMN                            
177900     MOVE 'W22150D3'         TO POSTSUM-DDNAMN2                           
178000     MOVE 'STAT'             TO POSTSUM-TRANSTYP                          
178100     CALL POSTSUM USING POSTSUM-PARM                                      
178200     SKIP3                                                                
178300     .                                                                    
178400 EC-SKRIV-W22154 SECTION.                                                 
178500     SKIP1                                                                
178600     WRITE U54AUT-POST FROM U54AUT-AREA                                   
178700     SKIP1                                                                
178800     MOVE 'W22154'           TO POSTSUM-FDNAMN                            
178900     MOVE 'W22150D4'         TO POSTSUM-DDNAMN2                           
179000     MOVE 'AUT '             TO POSTSUM-TRANSTYP                          
179100     CALL POSTSUM USING POSTSUM-PARM                                      
179200     .                                                                    
179300     EJECT                                                                
179400 ED-SKRIV-W22159 SECTION.                                                 
179500     SKIP1                                                                
179600     WRITE U59-POST FROM U59-AREA                                         
179700     SKIP1                                                                
179800     MOVE 'W22159'           TO POSTSUM-FDNAMN                            
179900     MOVE 'W22150D5'         TO POSTSUM-DDNAMN2                           
180000     MOVE 'U59 '             TO POSTSUM-TRANSTYP                          
180100     CALL POSTSUM USING POSTSUM-PARM                                      
180200     .                                                                    
180300     EJECT                                                                
180400 F-BORTTAG-OMSPEC-AVROP SECTION.                                          
180500     SKIP3                                                                
180600*--  MOVE BORTTAG-OMSPEC TO LINK-KDCALL                                   
180700     PERFORM S20-BORTTAG-OMSPEC                                           
180800     SKIP1                                                                
180900     MOVE ZERO                   TO WSPAR-D904-KDLPORS-TAB (1)            
181000                                    WSPAR-D904-KDLPORS-TAB (2)            
181100                                    WSPAR-D904-KDLPORS-TAB (3)            
181200                                    WSPAR-D904-KVBEST-PL                  
181300                                    WSPAR-D904-KDPLKOEP                   
181400     SKIP1                                                                
181500     MOVE I42BEG-KDLPORS-TAB (1)  TO LINK-KDLPORS-TAB (1)                 
181600     MOVE I42BEG-KDLPORS-TAB (2)  TO LINK-KDLPORS-TAB (2)                 
181700     MOVE I42BEG-KDLPORS-TAB (3)  TO LINK-KDLPORS-TAB (3)                 
181800     ADD  I42BEG-KVBEST-PL        TO LINK-KVBEST-PL                       
181900     MOVE I42BEG-KDPLKOEP         TO LINK-KDPLKOEP                        
182000     SKIP1                                                                
182100     PERFORM S21-BORTTAG-ALLA-FOERSLAG                                    
182200     .                                                                    
182300     EJECT                                                                
182400 G-OMSPEC-AV-LEVERANSPLAN SECTION.                                        
182500******************************************************************        
182600*    AVROP SOM HAMNAR INOM ETT BLOCKAT VECKO-INTERVALL PÅ        *        
182700*    IDLEVNR-SHIP, SKALL FLYTTAS VIA SUB-PGM W221BLOC.           *        
182800*    FÖR BLOCKADE VECKOR, SE BILD 2149.                          *        
182900*                                                                *        
183000******************************************************************        
183100     SKIP1                                                                
183200     PERFORM G1-SKAPA-BEHOVSTABELL                                        
183300     PERFORM GA-INITIERA-OMSPEC                                           
183400     PERFORM GB-SKAPA-TILLGANGSTABELL                                     
183500     PERFORM GC-PROCESS-BEHOV-VW-RENAULT                                  
183600     PERFORM GD-BERAKNA-TILLG-I-SPECVECKA                                 
183700     SKIP1                                                                
183800     MOVE LINK-IDARTNR TO LINK3-IDARTNR                                   
183900     MOVE LINK-IDLEVNR TO LINK3-IDLEVNR                                   
184000     MOVE 1 TO LINK3-KDAVROP                                              
184100                                                                          
184200     INITIALIZE  BLOC-W221BLOC                                            
184300     MOVE IDPGM TO BLOC-IDPGM                                             
184400     MOVE +0    TO BLOC-TAB-IX                                            
184500     MOVE NEJ   TO SW-FLORS-06                                            
184600                                                                          
184700     PERFORM GE-SPEC-NYTT-FOERSLAG                                        
184800                                                                          
184900     IF BLOC-TAB-IX > 0                                                   
185000       PERFORM GF-FLYTTA-BLOCKADE-AVROP                                   
185100                                                                          
185200       MOVE JA      TO SW-FLORS-06                                        
185300     END-IF                                                               
185400     SKIP1                                                                
185500     MOVE 5 TO LINK-KDLPSP                                                
185600     MOVE W-DATUM-AAVV-AKT TO LINK-TIOMSPEC                               
185700     MOVE LINK-TIOMSPEC TO LINK-TILPSP                                    
185800     IF LINK-KDVVKL = 5                                                   
185900        MOVE 2 TO W-ANTAL-VECKOR                                          
186000     ELSE                                                                 
186100        MOVE 3 TO W-ANTAL-VECKOR                                          
186200     END-IF                                                               
186300     CALL W009VADD USING LINK-TILPSP W-ANTAL-VECKOR                       
186400     .                                                                    
186500     EJECT                                                                
186600 G1-SKAPA-BEHOVSTABELL SECTION.                                           
186700     SKIP3                                                                
186800     MOVE SEP-SATS-TPO-LEV-SDC-NDC  TO LNK2-KDBEHOV                       
186900     MOVE SPACE                     TO LNK2-IDDC                          
187000     MOVE W-DATUM-AAVV-AKT          TO LNK2-TIAAVV-AKTUELL                
187100                                       LNK2-TIBEHOV-START                 
187200     MOVE 1                         TO W-ANTAL-VECKOR                     
187300     CALL W009VADD USING LNK2-TIBEHOV-START W-ANTAL-VECKOR                
187400     MOVE LINK-IDARTNR              TO LNK2-IDARTNR                       
187500     MOVE 156                       TO LNK2-KVVECKOR-BEHOV                
187600     MOVE +6                        TO LNK2-TID-AKTUELL                   
187700     MOVE NEJ                       TO LNK2-FLINKLDIRLEV                  
187800     SKIP1                                                                
187900     CALL W22222 USING LNK2-AREA W22X-AA-PCB    W22X-WDK7-PCB             
188000                                 W22X-ARTM-PCB  W22X-2501-PCB             
188100                                 W22X-WDB6R-PCB W22X-WDK7R-PCB            
188200                                 W22X-WDB6-PCB  W22X-WDD7-PCB             
188300                                 W22X-WDK7E-PCB                           
188400                                 W22X-UTIL-WDK6-PCB                       
188500                                 W22X-UTIL-WDK7-PCB                       
188600                                 W22X-UTIL-WDB6-PCB                       
188700                                 W22X-UTUP-WDK7-PCB                       
188800                                 W22X-UTUP-WDB6-PCB                       
188900                                 W22X-UTUP-UTIL-WDK6-PCB                  
189000                                 W22X-UTUP-UTIL-WDK7-PCB                  
189100                                 W22X-UTUP-UTIL-WDB6-PCB                  
189200                                                                          
189300     IF LINK-ANROP-FEL                                                    
189400        PERFORM S04-NOLLA-W22222                                          
189500     END-IF                                                               
189600     .                                                                    
189700     EJECT                                                                
189800 GA-INITIERA-OMSPEC SECTION.                                              
189900     SKIP3                                                                
190000     MOVE ZERO                    TO W-TILLG-SPAR                         
190100     COMPUTE W-TILLG-SPAR =                                               
190200                     LINK-KVLS   (1)                                      
190300                 -   LINK-KVRESS (1)                                      
190400                 -   LINK-KVROS  (1)                                      
190500                 -  (LINK-KVOKS-BULK (1) + LINK-KVOKS-DAG (1) +           
190600                     LINK-KVOKS-VOR  (1))                                 
190700                 +   LINK-KVAKS  (1)                                      
190800                 +   LINK-KVLAAN                                          
190900                 +   LINK-KVLS-SDC-OVER                                   
191000*                                                                         
191100     MOVE I42BEG-KDLPORS-TAB (1)  TO LINK-KDLPORS-TAB (1)                 
191200     MOVE I42BEG-KDLPORS-TAB (2)  TO LINK-KDLPORS-TAB (2)                 
191300     MOVE I42BEG-KDLPORS-TAB (3)  TO LINK-KDLPORS-TAB (3)                 
191400     MOVE I42BEG-KVBEST-PL        TO LINK-KVBEST-PL                       
191500     MOVE I42BEG-KDPLKOEP         TO LINK-KDPLKOEP                        
191600     MOVE LINK-TIFINLV            TO W-TIFINLV                            
191700     SKIP1                                                                
191800*                                                                         
191900***  PUBWEEK IS LEADTIME ADJUSTED IN DEMAND MODULE.                       
192000***  IF PUBWEEK IS IN FUTURE, CHECK THE WEEK FOR FIRST DEMAND             
192100***  SO THE CALL-OFFS SHOULD BE FROM LEADTIME + CURRENT WEEK.             
192200*                                                                         
192300     MOVE W-DATUM-AAVV-AKT           TO TMP1-YYWW                         
192400     MOVE W-TIFINLV-1-4              TO TMP2-YYWW                         
192500     PERFORM WY2000P3                                                     
192600     IF TMP2-YYWW     > TMP1-YYWW                                         
192700        MOVE 1                    TO IX-L                                 
192800        PERFORM UNTIL IX-L > 156                                          
192900        OR LNK2-KVBEHOV-VECKA (IX-L) > ZERO                               
193000          ADD 1                   TO IX-L                                 
193100        END-PERFORM                                                       
193200*                                                                         
193300*      CHECK IF DEMAND EXISTS I N THE FIRST WEEK OR                       
193400*      IF WE HAVE NEGATIVE ASSETS LIKE IN CASE OF BACKORDER               
193500*      BASED ON IX-L VALUE WE ADJUST THE DELIVERY PLAN WEEK               
193600*                                                                         
193700        IF LNK2-KVBEHOV-DESSUTOM     > ZERO                               
193800        OR W-TILLG-SPAR  < ZERO                                           
193900           MOVE 1                 TO IX-L                                 
194000        END-IF                                                            
194100        IF IX-L > 156                                                     
194200           CONTINUE                                                       
194300        ELSE                                                              
194400           MOVE IX-L                 TO W-ANTAL-VECKOR                    
194500           MOVE W-DATUM-AAVV-AKT     TO W-AAVV-ADD                        
194600           CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                  
194700           MOVE W-AAVV-ADD           TO W-TISPECST-ADJ                    
194800        END-IF                                                            
194900     END-IF                                                               
195000     IF SW-OPTIMAL-OMSPEC = NEJ                                           
195100        MOVE W-DATUM-AAVV-AKT     TO W-TISPECST-DISP                      
195200        MOVE LINK-KVVECKOR-FT     TO W-ANTAL-VECKOR                       
195300        ADD 1                     TO W-ANTAL-VECKOR                       
195400        CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR                
195500        MOVE W-DATUM-AAVV-AKT     TO LINK-TISPECST                        
195600        MOVE LINK-KVVECKOR-LT     TO W-ANTAL-VECKOR                       
195700        ADD 1                     TO W-ANTAL-VECKOR                       
195800        CALL W009VADD USING LINK-TISPECST W-ANTAL-VECKOR                  
195900     ELSE                                                                 
196000        MOVE W-DATUM-AAVV-AKT     TO W-TISPECST-DISP                      
196100        MOVE LINK-KVVECKOR-FT     TO W-ANTAL-VECKOR                       
196200        ADD  2                    TO W-ANTAL-VECKOR                       
196300        SUBTRACT LINK-KVVECKOR-LT FROM W-ANTAL-VECKOR                     
196400        CALL W009VADD USING W-TISPECST-DISP  W-ANTAL-VECKOR               
196500        MOVE W-DATUM-AAVV-AKT     TO LINK-TISPECST                        
196600        MOVE 2                    TO W-ANTAL-VECKOR                       
196700        CALL W009VADD USING LINK-TISPECST  W-ANTAL-VECKOR                 
196800     END-IF                                                               
196900*                                                                         
197000*    IF THE DELIERY PLAN WEEK CALCULATES IS LESS THAN                     
197100*    ADJUSTED DELIVERY PLAN WEEK CALUCATED ABOVE (BASED ON                
197200*    DEMAND FROM THE DEMAND DEMAND MODULE),                               
197300*    USE THE ADJUSTED DELIVERY PLAN START WEEK                            
197400*                                                                         
197500     MOVE W-DATUM-AAVV-AKT           TO TMP1-YYWW                         
197600     MOVE W-TIFINLV-1-4              TO TMP2-YYWW                         
197700     PERFORM WY2000P3                                                     
197800     MOVE LINK-KDPRODSL              TO TEST-KDPRODSL                     
197900     IF TMP1-YYWW + 1 <  TMP2-YYWW                                        
198000        MOVE W-TISPECST-DISP         TO TMP1-YYWW                         
198100        MOVE W-TISPECST-ADJ          TO TMP2-YYWW                         
198200        PERFORM WY2000P3                                                  
198300        IF TMP1-YYWW <  TMP2-YYWW                                         
198400           MOVE W-TISPECST-ADJ       TO W-TISPECST-DISP                   
198500        END-IF                                                            
198600     END-IF                                                               
198700     SKIP1                                                                
198800     MOVE LINK-IDLEVNR    TO OLIKA-LEV                                    
198900     IF  ALLISON-LEVNR OR EATON-LEVNR OR SOMA-LEVNR                       
199000        OR TRW-LEVNR                                                      
199100         PERFORM GAA-JUST-SPECST-VW-RENAULT                               
199200     END-IF                                                               
199300     IF  LINK-KDHF > ZERO                                                 
199400         OR LINK-IDARTNR = 271794 OR 271788                               
199500        MOVE LINK-IDLEVNR    TO OLIKA-LEV                                 
199600        IF NOT SATS-LEVNR                                                 
199700           PERFORM GAB-JUST-FOR-SEMESTER                                  
199800        END-IF                                                            
199900     END-IF                                                               
200000     SKIP1                                                                
200100**  SEMESTERJUSTERING SKALL EJ GÖRAS FÖR KONCERN **                       
200200*    IF  LINK-KDAVT = 3                                                   
200300*       PERFORM GAB-JUST-FOR-SEMESTER                                     
200400*    END-IF                                                               
200500     SKIP1                                                                
200600     MOVE LINK-IDLEVNR    TO OLIKA-LEV                                    
200700     IF SKOVDE-LEVNR                                                      
200800        MOVE LINK-KVVECKOR-BT TO W-KVVECKOR-SPEC                          
200900        ADD 10 TO W-KVVECKOR-SPEC                                         
201000           IF  W-KVVECKOR-SPEC < 52                                       
201100            MOVE 52 TO W-KVVECKOR-SPEC                                    
201200           END-IF                                                         
201300     ELSE                                                                 
201400        MOVE W-DATUM-AAVV-AKT    TO W-DATUM-FROM                          
201500        MOVE W-TISPECST-DISP     TO W-DATUM-TOM                           
201600        PERFORM S02-BERAKNA-VECKODIFFERENS                                
201700                                                                          
201800        MOVE LINK-IDLEVNR TO OLIKA-LEV                                    
201900        IF LINK-KDHF    > ZERO       OR                                   
202000           SATS-LEVNR                                                     
202100           MOVE ZERO TO W-KVDAGAR-FFH                                     
202200        ELSE                                                              
202300           COMPUTE W-KVDAGAR-FFH =                                        
202400           LINK-KVDAGAR-INLEV + LINK-KVDAGAR-TT                           
202500        END-IF                                                            
202600                                                                          
202700        COMPUTE W-KVVECKOR-SPEC ROUNDED =                                 
202800                                      52 - LINK-KVVECKOR-LT               
202900                                                                          
203000        IF W-KVVECKOR-SPEC < 10                                           
203100           MOVE 10 TO W-KVVECKOR-SPEC                                     
203200        END-IF                                                            
203300     END-IF                                                               
203400                                                                          
203500     MOVE ZERO TO W-KVAVROP-VVKL12-ACC                                    
203600                  W-TILLG                                                 
203700     SKIP1                                                                
203800     MOVE 1 TO IX                                                         
203900     PERFORM UNTIL                                                        
204000       (IX     > TILLGTAB-MAX)                                            
204100         MOVE ZERO TO TILLGTAB-ANTAL (IX)                                 
204200         ADD 1 TO IX                                                      
204300     END-PERFORM                                                          
204400                                                                          
204500*WZ20DAYS                                                                 
204600     MOVE LINK-TISPECST           TO WS-DAYS-TISPECST-AAVV                
204700     MOVE WS-DAYS-TISPECST-AAVV   TO DAYS-TIDATE1                         
204800     MOVE 'YYWW'                  TO DAYS-KDDATFMT1                       
204900     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
205000     MOVE 0                       TO DAYS-KVDAYS                          
205100     MOVE SPACE                   TO DAYS-TIDATE2                         
205200                               DAYS-IDCALEND                              
205300     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
205400*                                                                         
205500     IF DAYS-KDRC = 8                                                     
205600       MOVE 'FEL VID ANROP TILL WZ20DAYS 5' TO FELTEXT                    
205700       CALL ABEND USING RKOD                                              
205800     ELSE                                                                 
205900       MOVE DAYS-TIDATE2(1:6) TO WS-TIAAMMDD-SPECST                       
206000     END-IF                                                               
206100     .                                                                    
206200     EJECT                                                                
206300 GAA-JUST-SPECST-VW-RENAULT SECTION.                                      
206400******************************************************************        
206500*                                                                *        
206600*    TISPECST JUSTERAS FÖR VW/RENAULT/ALLISON/EATON/SOMA/TRW     *        
206700*                                             LEVERANTÖR         *        
206800*    TISPECST ÖKAS MED RESTEN AV TISPECST/4                      *        
206900*                                                                *        
207000******************************************************************        
207100     SKIP1                                                                
207200     DIVIDE LINK-TISPECST BY 4 GIVING W-DUMMY                             
207300            REMAINDER W-ANTAL-VECKOR                                      
207400     COMPUTE W-ANTAL-VECKOR = 4 - W-ANTAL-VECKOR                          
207500     SKIP1                                                                
207600     CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR                   
207700     CALL W009VADD USING LINK-TISPECST W-ANTAL-VECKOR                     
207800     .                                                                    
207900     EJECT                                                                
208000 GAB-JUST-FOR-SEMESTER SECTION.                                           
208100******************************************************************        
208200*                                                                *        
208300*    JUSTERING AV TISPECST FÖR SEMESTER                          *        
208400*                                                                *        
208500******************************************************************        
208600     SKIP1                                                                
208700*    DIVIDE LINK-KVDAGAR-TT BY 5 GIVING W-KVVECKOR-TT ROUNDED             
208800     MOVE LINK-TISPECST TO W-DATUM-AAVV                                   
208900*    ADD W-KVVECKOR-TT TO W-DATUM-VV                                      
209000     SKIP1                                                                
209100     IF  W-DATUM-VV NOT < SEMESTER-VECKA-START                            
209200     AND W-DATUM-VV NOT > SEMESTER-VECKA-SLUT                             
209300         COMPUTE W-ANTAL-VECKOR =                                         
209400                            SEMESTER-VECKA-SLUT + 1                       
209500                         -  W-DATUM-VV                                    
209600         CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR               
209700         CALL W009VADD USING LINK-TISPECST W-ANTAL-VECKOR                 
209800     END-IF                                                               
209900     .                                                                    
210000     EJECT                                                                
210100 GB-SKAPA-TILLGANGSTABELL SECTION.                                        
210200******************************************************************        
210300*                                                                *        
210400*    TABELL MED INLEVERANSER PLACERADE I RESPECTIVE VECKA        *        
210500*                                                                *        
210600******************************************************************        
210700     SKIP1                                                                
210800     MOVE LINK-IDARTNR     TO LINK3-IDARTNR                               
210900     MOVE 2                TO LINK3-KDAVROP                               
211000     MOVE LAES-AVROP-FIRST TO LINK3-KDCALL                                
211100     PERFORM GBA-LAES-AVROP-FIRST                                         
211200     SKIP1                                                                
211300     MOVE LINK-TISPECST    TO W-GRAENS-AVROP                              
211400     MOVE W-KVVECKOR-SPEC  TO W-ANTAL-VECKOR                              
211500     CALL W009VADD USING W-GRAENS-AVROP W-ANTAL-VECKOR                    
211600     SKIP1                                                                
211700     PERFORM UNTIL NOT(                                                   
211800        LINK3-ANROP-OK)                                                   
211900         MOVE LINK3-TIAVROP-AVS    TO TMP1-YYWW                           
212000         MOVE LINK-TISPECST        TO TMP2-YYWW                           
212100         MOVE W-GRAENS-AVROP       TO TMP3-YYWW                           
212200         PERFORM WY2000Q3                                                 
212300         IF  (LINK3-IDLEVNR = LINK-IDLEVNR                                
212400          AND TMP1-YYWW < TMP2-YYWW)                                      
212500         OR  (LINK3-IDLEVNR NOT = LINK-IDLEVNR                            
212600          AND TMP1-YYWW < TMP3-YYWW)                                      
212700     SKIP1                                                                
212800              MOVE LINK3-TIAVROP-DISP   TO TMP1-YYWW                      
212900              MOVE W-TISPECST-DISP      TO TMP2-YYWW                      
213000              PERFORM WY2000P3                                            
213100              IF  TMP1-YYWW < TMP2-YYWW                                   
213200                  ADD LINK3-KVAVROP TO W-TILLG                            
213300              ELSE                                                        
213400                  MOVE LINK3-TIAVROP-DISP TO W-DATUM-TOM                  
213500                  MOVE W-DATUM-AAVV-AKT    TO W-DATUM-FROM                
213600                  PERFORM S02-BERAKNA-VECKODIFFERENS                      
213700                  IF W-VECKO-DIFFERENS > ZERO                             
213800                    MOVE W-VECKO-DIFFERENS  TO TILLGTAB-IX                
213900                  ELSE                                                    
214000                    MOVE +1                 TO TILLGTAB-IX                
214100                    DISPLAY ' ARTNR ' LINK-IDARTNR                        
214200                  END-IF                                                  
214300                  ADD LINK3-KVAVROP                                       
214400                             TO TILLGTAB-ANTAL (TILLGTAB-IX)              
214500              END-IF                                                      
214600              IF  LINK3-IDLEVNR = LINK-IDLEVNR                            
214700                  ADD LINK3-KVAVROP TO W-KVAVROP-VVKL12-ACC               
214800              END-IF                                                      
214900         END-IF                                                           
215000         MOVE LAES-AVROP-NEXT    TO LINK3-KDCALL                          
215100         PERFORM GBB-LAES-AVROP-NEXT                                      
215200     END-PERFORM                                                          
215300     .                                                                    
215400     EJECT                                                                
215500 GBA-LAES-AVROP-FIRST SECTION.                                            
215600     MOVE 'GBA-LAES-AVROP-FIRST '  TO CURRENT-SECTION                     
215700                                                                          
215800*--- H-LAES-AVROP-FIRST SECTION I PGM W2215010                            
215900     SKIP1                                                                
216000     MOVE LINK3-IDARTNR TO W-IDARTNR-D9                                   
216100     MOVE WC-CDC-SE     TO W-IDDC-D9                                      
216200     PERFORM IMS-GU-LEV-ROT-WDD901                                        
216300     IF SEGMENT-FINNS                                                     
216400        PERFORM GBAA-LAES-AVROP-FIRST-D905                                
216500     ELSE                                                                 
216600        MOVE NEJ TO LINK3-FLJANEJ-ANROP                                   
216700        MOVE ZERO      TO LINK3-TIAVROP-AVS                               
216800                          LINK3-TIAVROP-INL                               
216900                          LINK3-TIAVROP-DISP                              
217000                          LINK3-KVAVROP                                   
217100                          LINK3-TIAVRDAT-INL                              
217200                          LINK3-TIAVRDAT-DISP                             
217300                          LINK3-TILEVDAG                                  
217400        MOVE SPACE     TO LINK3-IDLEVNR                                   
217500     END-IF                                                               
217600     .                                                                    
217700     EJECT                                                                
217800 GBAA-LAES-AVROP-FIRST-D905 SECTION.                                      
217900     MOVE 'GBAA-LAES-AVROP-FIRST-D905 '  TO CURRENT-SECTION               
218000                                                                          
218100     MOVE LINK3-KDAVROP TO W-KDAVROP                                      
218200     PERFORM IMS-GNP-AVROP-WDD905-NEXT                                    
218300                                                                          
218400     IF SEGMENT-FINNS                                                     
218500         MOVE JA TO LINK3-FLJANEJ-ANROP                                   
218600         MOVE D9AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                       
218700         MOVE WS-DAAVROP-AAVV     TO LINK3-TIAVROP-AVS                    
218800         MOVE D9AVROP-TILEVDAG    TO LINK3-TILEVDAG                       
218900***   IF D9AVROP-TIAVRDAT-INL > ZERO                                      
219000*WZ20DAYS                                                                 
219100         MOVE D9AVROP-TIAVRDAT-INL TO WS-DAYS-TIAVRDAT-INL                
219200         MOVE WS-DAYS-TIAVRDAT-INL TO DAYS-TIDATE1                        
219300         MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                      
219400         MOVE 'YYWW'               TO DAYS-KDDATFMT2                      
219500         MOVE 0                    TO DAYS-KVDAYS                         
219600         MOVE SPACE                TO DAYS-TIDATE2                        
219700                                       DAYS-IDCALEND                      
219800         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
219900*                                                                         
220000         IF DAYS-KDRC = 8                                                 
220100           MOVE 'FEL VID ANROP TILL WZ20DAYS 1' TO FELTEXT                
220200           CALL ABEND USING RKOD                                          
220300         ELSE                                                             
220400           MOVE DAYS-TIDATE2(1:4)  TO W-TIAAVV                            
220500           MOVE W-TIAAVV          TO LINK3-TIAVROP-INL                    
220600         END-IF                                                           
220700***   ELSE                                                                
220800***        MOVE ZERO              TO LINK3-TIAVROP-INL                    
220900***   END-IF                                                              
221000***   IF D9AVROP-TIAVRDAT-DISP > ZERO                                     
221100*WZ20DAYS                                                                 
221200         MOVE D9AVROP-TIAVRDAT-DISP  TO WS-DAYS-TIAVRDAT-DISP             
221300         MOVE WS-DAYS-TIAVRDAT-DISP  TO DAYS-TIDATE1                      
221400         MOVE 'YYMMDD'               TO DAYS-KDDATFMT1                    
221500         MOVE 'YYWW'                 TO DAYS-KDDATFMT2                    
221600         MOVE 0                      TO DAYS-KVDAYS                       
221700         MOVE SPACE                  TO DAYS-TIDATE2                      
221800                                         DAYS-IDCALEND                    
221900         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
222000*                                                                         
222100         IF DAYS-KDRC = 8                                                 
222200           MOVE 'FEL VID ANROP TILL WZ20DAYS 2' TO FELTEXT                
222300           CALL ABEND USING RKOD                                          
222400         ELSE                                                             
222500           MOVE DAYS-TIDATE2(1:4) TO W-TIAAVV                             
222600           MOVE W-TIAAVV          TO LINK3-TIAVROP-DISP                   
222700         END-IF                                                           
222800***   ELSE                                                                
222900***       MOVE ZERO               TO LINK3-TIAVROP-DISP                   
223000***   END-IF                                                              
223100         MOVE D9AVROP-KVAVROP      TO LINK3-KVAVROP                       
223200         MOVE WDD91-KEY-02-IDLEVNR TO LINK3-IDLEVNR                       
223300     ELSE                                                                 
223400         MOVE NEJ TO LINK3-FLJANEJ-ANROP                                  
223500         MOVE ZERO                 TO LINK3-TIAVROP-AVS                   
223600                                      LINK3-TIAVROP-INL                   
223700                                      LINK3-TIAVROP-DISP                  
223800                                      LINK3-KVAVROP                       
223900                                      LINK3-TIAVRDAT-INL                  
224000                                      LINK3-TIAVRDAT-DISP                 
224100                                      LINK3-TILEVDAG                      
224200         MOVE SPACE                TO LINK3-IDLEVNR                       
224300     END-IF                                                               
224400     .                                                                    
224500     EJECT                                                                
224600 GBB-LAES-AVROP-NEXT SECTION.                                             
224700     MOVE 'GBB-LAES-AVROP-NEXT '  TO CURRENT-SECTION                      
224800                                                                          
224900***  I-LAES-AVROP-NEXT  SECTION. PGM W2215010                             
225000     SKIP1                                                                
225100     MOVE LINK3-IDARTNR TO W-IDARTNR                                      
225200     MOVE LINK3-KDAVROP TO W-KDAVROP                                      
225300     PERFORM IMS-GNP-AVROP-WDD905-NEXT                                    
225400     SKIP1                                                                
225500     IF SEGMENT-FINNS                                                     
225600         MOVE JA TO LINK3-FLJANEJ-ANROP                                   
225700         MOVE D9AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                       
225800         MOVE WS-DAAVROP-AAVV     TO LINK3-TIAVROP-AVS                    
225900         MOVE D9AVROP-TILEVDAG    TO LINK3-TILEVDAG                       
226000***   IF D9AVROP-TIAVRDAT-INL > ZERO                                      
226100*WZ20DAYS                                                                 
226200          MOVE D9AVROP-TIAVRDAT-INL   TO WS-DAYS-TIAVRDAT-INL             
226300          MOVE WS-DAYS-TIAVRDAT-INL   TO DAYS-TIDATE1                     
226400          MOVE 'YYMMDD'               TO DAYS-KDDATFMT1                   
226500          MOVE 'YYWW'                 TO DAYS-KDDATFMT2                   
226600          MOVE 0                      TO DAYS-KVDAYS                      
226700          MOVE SPACE                  TO DAYS-TIDATE2                     
226800                                         DAYS-IDCALEND                    
226900          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
227000*                                                                         
227100          IF DAYS-KDRC = 8                                                
227200            MOVE 'FEL VID ANROP TILL WZ20DAYS 3' TO FELTEXT               
227300            CALL ABEND USING RKOD                                         
227400          ELSE                                                            
227500            MOVE DAYS-TIDATE2(1:4) TO WS-TIAAVV                           
227600            MOVE WS-TIAAVV         TO LINK3-TIAVROP-INL                   
227700          END-IF                                                          
227800***   ELSE                                                                
227900***        MOVE ZERO               TO LINK3-TIAVROP-INL                   
228000***   END-IF                                                              
228100***   IF D9AVROP-TIAVRDAT-DISP > ZERO                                     
228200*WZ20DAYS                                                                 
228300          MOVE D9AVROP-TIAVRDAT-DISP  TO WS-DAYS-TIAVRDAT-DISP            
228400          MOVE WS-DAYS-TIAVRDAT-DISP  TO DAYS-TIDATE1                     
228500          MOVE 'YYMMDD'               TO DAYS-KDDATFMT1                   
228600          MOVE 'YYWW'                 TO DAYS-KDDATFMT2                   
228700          MOVE 0                      TO DAYS-KVDAYS                      
228800          MOVE SPACE                  TO DAYS-TIDATE2                     
228900                                         DAYS-IDCALEND                    
229000          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
229100*                                                                         
229200          IF DAYS-KDRC = 8                                                
229300            MOVE 'FEL VID ANROP TILL WZ20DAYS 4' TO FELTEXT               
229400            CALL ABEND USING RKOD                                         
229500          ELSE                                                            
229600            MOVE DAYS-TIDATE2(1:4) TO WS-TIAAVV                           
229700            MOVE WS-TIAAVV         TO LINK3-TIAVROP-DISP                  
229800          END-IF                                                          
229900***   ELSE                                                                
230000***        MOVE ZERO               TO LINK3-TIAVROP-DISP                  
230100***   END-IF                                                              
230200         MOVE D9AVROP-KVAVROP      TO LINK3-KVAVROP                       
230300         MOVE WDD91-KEY-02-IDLEVNR TO LINK3-IDLEVNR                       
230400     ELSE                                                                 
230500         MOVE NEJ TO LINK3-FLJANEJ-ANROP                                  
230600         MOVE ZERO                TO LINK3-TIAVROP-AVS                    
230700                                     LINK3-TIAVROP-INL                    
230800                                     LINK3-TIAVROP-DISP                   
230900                                     LINK3-KVAVROP                        
231000                                     LINK3-TIAVRDAT-INL                   
231100                                     LINK3-TIAVRDAT-DISP                  
231200                                     LINK3-TILEVDAG                       
231300         MOVE SPACE               TO LINK3-IDLEVNR                        
231400     END-IF                                                               
231500     .                                                                    
231600     EJECT                                                                
231700 GC-PROCESS-BEHOV-VW-RENAULT SECTION.                                     
231800     SKIP3                                                                
231900*                                                                         
232000     IF LINK-KDHF > ZERO                                                  
232100       OR LINK-IDARTNR = 271794 OR 271788                                 
232200         MOVE LINK-IDLEVNR    TO OLIKA-LEV                                
232300         IF NOT SATS-LEVNR                                                
232400             PERFORM GCA-JUSTERA-FOR-SEMESTER                             
232500         END-IF                                                           
232600     END-IF                                                               
232700     MOVE LINK-IDLEVNR     TO OLIKA-LEV                                   
232800     IF  ALLISON-LEVNR OR EATON-LEVNR OR SOMA-LEVNR                       
232900         OR TRW-LEVNR                                                     
233000         PERFORM GCB-JUST-BEHOV-VW-RENAULT                                
233100     END-IF                                                               
233200     .                                                                    
233300     EJECT                                                                
233400 GCA-JUSTERA-FOR-SEMESTER SECTION.                                        
233500******************************************************************        
233600*                                                                *        
233700*    BEHOV UNDER SEMESTERN FLYTTAS FÖRE SEMESTERN                *        
233800*  OBS                                                           *        
233900*  OBS     KOPIA AV SECTIONEN FINNS I PROGRAM W2214000           *        
234000*  OBS     SKALL JUSTERAS PARALLELLT                             *        
234100*  OBS                                                           *        
234200*                                                                *        
234300******************************************************************        
234400     SKIP1                                                                
234500     IF LINK-KDHF = ZERO                                                  
234600        COMPUTE W-KVDAGAR-FFH =                                           
234700                LINK-KVDAGAR-TT + LINK-KVDAGAR-INLEV                      
234800     ELSE                                                                 
234900        MOVE +0 TO W-KVDAGAR-FFH                                          
235000     END-IF                                                               
235100                                                                          
235200     COMPUTE W-SEMESTER-VV-START ROUNDED =                                
235300             SEMESTER-VECKA-START + (W-KVDAGAR-FFH / 5)                   
235400     COMPUTE W-SEMESTER-VV-SLUT ROUNDED =                                 
235500             SEMESTER-VECKA-SLUT + (W-KVDAGAR-FFH / 5)                    
235600                                                                          
235700     MOVE LNK2-TIBEHOV-START TO W-DATUM-AAVV                              
235800     SUBTRACT W-DATUM-VV FROM W-SEMESTER-VV-START                         
235900     SUBTRACT W-DATUM-VV FROM W-SEMESTER-VV-SLUT                          
236000     ADD 1 TO W-SEMESTER-VV-START                                         
236100              W-SEMESTER-VV-SLUT                                          
236200     SKIP1                                                                
236300     PERFORM UNTIL NOT(                                                   
236400        W-SEMESTER-VV-START < LNK2-KVVECKOR-BEHOV)                        
236500         MOVE W-SEMESTER-VV-START TO IX                                   
236600                                     IX-SUM                               
236700         SUBTRACT 1 FROM IX-SUM                                           
236800         PERFORM UNTIL    (                                               
236900            IX     > W-SEMESTER-VV-SLUT                                   
237000         OR  IX     > LNK2-KVVECKOR-BEHOV)                                
237100           IF IX-SUM > ZERO                                               
237200             ADD LNK2-KVBEHOV-VECKA (IX)                                  
237300                             TO LNK2-KVBEHOV-VECKA (IX-SUM)               
237400           END-IF                                                         
237500           IF IX > ZERO                                                   
237600             MOVE ZERO TO LNK2-KVBEHOV-VECKA (IX)                         
237700           END-IF                                                         
237800           ADD 1 TO IX                                                    
237900         END-PERFORM                                                      
238000         ADD 52 TO W-SEMESTER-VV-START                                    
238100                   W-SEMESTER-VV-SLUT                                     
238200     END-PERFORM                                                          
238300     .                                                                    
238400     EJECT                                                                
238500 GCB-JUST-BEHOV-VW-RENAULT SECTION.                                       
238600******************************************************************        
238700*                                                                *        
238800*    OM VW/RENAULT/ALLISON/EATON/SOMA/TRW SKALL ALLA BEHOV       *        
238900*    LIGGA I BEHOVSVECKOR                                        *        
239000*    SÅ ATT RESTEN (BEHOVSVECKA/4) = FRAMFÖRHÅLLNING             *        
239100*  OBS                                                           *        
239200*  OBS     KOPIA AV SECTIONEN FINNS I PROGRAM W2214000           *        
239300*  OBS     SKALL JUSTERAS PARALLELLT                             *        
239400*  OBS                                                           *        
239500*                                                                *        
239600******************************************************************        
239700     SKIP1                                                                
239800     COMPUTE W-KVDAGAR-FFH =                                              
239900             LINK-KVDAGAR-TT + LINK-KVDAGAR-INLEV                         
240000     COMPUTE W-KVVECKOR-FFH ROUNDED =                                     
240100             (W-KVDAGAR-FFH / 5)                                          
240200                                                                          
240300                                                                          
240400     PERFORM UNTIL W-KVVECKOR-FFH < 4                                     
240500         SUBTRACT 4 FROM W-KVVECKOR-FFH                                   
240600     END-PERFORM                                                          
240700     SKIP1                                                                
240800     MOVE LNK2-TIBEHOV-START TO W-DATUM-AAVV                              
240900     DIVIDE W-DATUM-VV BY 4 GIVING W-ANTAL                                
241000     MULTIPLY 4 BY W-ANTAL                                                
241100     SUBTRACT W-ANTAL FROM W-DATUM-VV                                     
241200     SKIP1                                                                
241300     MOVE 1 TO IX-SUM                                                     
241400     SUBTRACT W-DATUM-VV FROM IX-SUM                                      
241500     ADD W-KVVECKOR-FFH TO IX-SUM                                         
241600     PERFORM UNTIL NOT(                                                   
241700        IX-SUM < 1)                                                       
241800        ADD 4 TO IX-SUM                                                   
241900     END-PERFORM                                                          
242000     SKIP1                                                                
242100     MOVE 1 TO IX                                                         
242200     PERFORM UNTIL NOT(                                                   
242300        IX < IX-SUM)                                                      
242400        ADD LNK2-KVBEHOV-VECKA (IX) TO LNK2-KVBEHOV-DESSUTOM              
242500        SUBTRACT LNK2-KVBEHOV-VECKA (IX)                                  
242600                                     FROM LNK2-KVBEHOV-SUMMA              
242700        MOVE ZERO TO LNK2-KVBEHOV-VECKA (IX)                              
242800        ADD 1 TO IX                                                       
242900     END-PERFORM                                                          
243000     SKIP1                                                                
243100     PERFORM UNTIL NOT(                                                   
243200        IX-SUM < LNK2-KVVECKOR-BEHOV)                                     
243300         MOVE IX-SUM TO IX                                                
243400         ADD 1 TO IX                                                      
243500         PERFORM UNTIL NOT(                                               
243600            IX < IX-SUM + 4)                                              
243700             ADD LNK2-KVBEHOV-VECKA (IX)                                  
243800                             TO LNK2-KVBEHOV-VECKA (IX-SUM)               
243900             MOVE ZERO TO LNK2-KVBEHOV-VECKA (IX)                         
244000             ADD 1 TO IX                                                  
244100         END-PERFORM                                                      
244200         ADD 4 TO IX-SUM                                                  
244300     END-PERFORM                                                          
244400     .                                                                    
244500     EJECT                                                                
244600 GD-BERAKNA-TILLG-I-SPECVECKA SECTION.                                    
244700******************************************************************        
244800*                                                                *        
244900*    BERAKNING AV TILLGÅNG I FÖRSTA SPEC-VECKA                   *        
245000*    AVROP HAR TIDIGARE ADDERATS TILL W-TILLG                    *        
245100*                                                                *        
245200******************************************************************        
245300     SKIP1                                                                
245400     COMPUTE W-TILLG-BER =                                                
245500                     LINK-KVLS   (1)                                      
245600                 -   LINK-KVRESS (1)                                      
245700                 -   LINK-KVROS  (1)                                      
245800                 -  (LINK-KVOKS-BULK (1) + LINK-KVOKS-DAG (1) +           
245900                     LINK-KVOKS-VOR  (1))                                 
246000                 +   LINK-KVAKS  (1)                                      
246100                 +   LINK-KVLAAN                                          
246200                 +   LINK-KVLS-SDC-OVER                                   
246300                                                                          
246400                                                                          
246500     ADD W-TILLG-BER TO W-TILLG                                           
246600     SUBTRACT LNK2-KVBEHOV-DESSUTOM                                       
246700                             FROM W-TILLG                                 
246800     MOVE 1 TO IX                                                         
246900     MOVE LNK2-TIBEHOV-START TO W-DATUM-FROM                              
247000     MOVE W-TISPECST-DISP    TO W-DATUM-TOM                               
247100     PERFORM S02-BERAKNA-VECKODIFFERENS                                   
247200*                                                                         
247300     IF W-VECKO-DIFFERENS > MAX-BEHOVSVECKOR-I-TAB                        
247400       MOVE MAX-BEHOVSVECKOR-I-TAB TO W-VECKO-DIFFERENS                   
247500     END-IF                                                               
247600*                                                                         
247700     MOVE 1 TO IX                                                         
247800     PERFORM UNTIL                                                        
247900       (IX     > W-VECKO-DIFFERENS)                                       
248000         SUBTRACT LNK2-KVBEHOV-VECKA (IX)                                 
248100                         FROM W-TILLG                                     
248200         ADD 1 TO IX                                                      
248300     END-PERFORM                                                          
248400                                                                          
248500     MOVE W-TILLG TO WS-SUM-START                                         
248600     .                                                                    
248700     EJECT                                                                
248800 GE-SPEC-NYTT-FOERSLAG SECTION.                                           
248900******************************************************************        
249000*                                                                *        
249100*    SPEC AV NYTT FÖRSLAG FRÅN LINK-TISPECST DATUM UNDER         *        
249200*    W-KVVECKOR-SPEC VECKOR                                      *        
249300*    BESTÄLLNINGSREST TÄCKS FÖR VVKL 1 OCH 2                     *        
249400*                                                                *        
249500******************************************************************        
249600     SKIP1                                                                
249700     COMPUTE W-KVBEST-REST = LINK-KVBR                                    
249800                        + LINK-KVBEST-PL                                  
249900     SKIP1                                                                
250000     COMPUTE W-BUFF = LINK-KVSLAGER (1)                                   
250100     SKIP1                                                                
250200     MOVE NEJ TO SW-BESTREST-TAEKT                                        
250300     MOVE LINK-IDLEVNR      TO OLIKA-LEV                                  
250400     IF (LINK-KDVVKL < 3 AND NOT SATS-LEVNR)                              
250500     OR  (LINK-KDERS (1) > ZERO AND < 10)                                 
250600     SKIP1                                                                
250700         IF  W-KVAVROP-VVKL12-ACC NOT < W-KVBEST-REST                     
250800             MOVE JA TO SW-BESTREST-TAEKT                                 
250900         END-IF                                                           
251000     END-IF                                                               
251100                                                                          
251200     PERFORM GEE-KOLL-WLC-2AAR                                            
251300                                                                          
251400     MOVE W-DATUM-AAVV-AKT TO W-DATUM-FROM                                
251500     MOVE W-TISPECST-DISP TO W-DATUM-TOM                                  
251600     PERFORM S02-BERAKNA-VECKODIFFERENS                                   
251700     IF W-VECKO-DIFFERENS < ZERO                                          
251800       MOVE +1 TO W-VECKO-DIFFERENS                                       
251900       DISPLAY I42BEG-IDARTNR ' vdiff < 0'                                
252000     END-IF                                                               
252100     MOVE W-VECKO-DIFFERENS TO IX                                         
252200     ADD W-KVVECKOR-SPEC TO W-VECKO-DIFFERENS                             
252300*                                                                         
252400     IF W-VECKO-DIFFERENS > MAX-BEHOVSVECKOR-I-TAB                        
252500       MOVE MAX-BEHOVSVECKOR-I-TAB TO W-VECKO-DIFFERENS                   
252600     END-IF                                                               
252700*                                                                         
252800*                                                                         
252900     MOVE JA  TO FOERST-SW                                                
253000*                                                                         
253100     MOVE LINK-IDLEVNR    TO WS-IDLEVNR-EMIL                              
253200     IF (LINK-KDVVKL > 2 AND LINK-FLMANQ = NEJ) OR                        
253300        (LINK-KDEFFMAN = 'E' OR 'B') OR                                   
253400        (NOT EJ-GODK-EMIL-LEVNR)                                          
253500        PERFORM GECA-BER-ARSOMS                                           
253600     END-IF                                                               
253700     IF LINK-KDVVKL > 2 AND LINK-FLMANQ = NEJ                             
253800        IF W-ARSOMS > W-ARSOMS-80000                                      
253900           MOVE 1  TO W-Q-FREKV-MAX                                       
254000        ELSE                                                              
254100           MOVE 3  TO W-Q-FREKV-MAX                                       
254200        END-IF                                                            
254300     END-IF                                                               
254400                                                                          
254500**   LÄGGES UTANFÖR ITERATION                                             
254600**   LÄS WDF106 GU OKVAL  ADR-IDLANDX2                                    
254700**   BERÄKNA FRYSGRÄNS  DAGENS + LINK-KVVECKOR-FT                         
254800                                                                          
254900     PERFORM GED-LANDKOD-FRYSTID                                          
255000*                                                                         
255100     MOVE LINK-IDARTNR       TO BLOC-IDARTNR                              
255200     MOVE WC-CDC-SE          TO BLOC-IDDC                                 
255300     MOVE LINK-IDLEVNR       TO BLOC-IDLEVNR                              
255400     MOVE LINK-IDLEVNR-SHIP  TO BLOC-IDLEVNR-SHIP                         
255500     MOVE WS-IDLANDX2-SHIP   TO BLOC-IDLANDX2-SHIP                        
255600     MOVE LINK-IDANSK        TO BLOC-IDANSK                               
255700     MOVE LINK3-KDAVROP      TO BLOC-KDAVROP                              
255800     MOVE WS-TIAAMMDD-SPECST TO BLOC-TIAAMMDD-SPECST                      
255900     MOVE WS-FRYSTID         TO BLOC-TIAAMMDD-FT                          
256000     MOVE LINK-KVDAGAR-TT    TO BLOC-KVDAGAR-TT                           
256100     MOVE LINK-KVDAGAR-INLEV TO BLOC-KVDAGAR-INLEV                        
256200     MOVE LINK-KVQ           TO BLOC-KVQ                                  
256300     MOVE LINK-KVPALL        TO BLOC-KVPALL                               
256400     MOVE LINK-KVULOAD       TO BLOC-KVULOAD                              
256500*                                                                         
256600                                                                          
256700     PERFORM UNTIL                                                        
256800      ( IX NOT < W-VECKO-DIFFERENS  OR SW-BESTREST-TAEKT =                
256900        JA  ) OR WS-FLAGGA-REDUC = JA                                     
257000       IF IX NOT > ZERO                                                   
257100         MOVE 1 TO IX                                                     
257200         DISPLAY ' ARTNR ' I42BEG-IDARTNR ' ix<=0'                        
257300       END-IF                                                             
257400       ADD TILLGTAB-ANTAL (IX)                                            
257500                               TO W-TILLG                                 
257600       SUBTRACT LNK2-KVBEHOV-VECKA   (IX)                                 
257700                               FROM W-TILLG                               
257800                                                                          
257900      MOVE LINK-KDPRODSL         TO TEST-KDPRODSL                         
258000      IF LINK-KDVVKL > 2 AND LINK-FLMANQ = NEJ                            
258100         AND KDPRODSL-VOLVO-BIMA                                          
258200         AND LINK-FLNYBER NOT = JA                                        
258300                                                                          
258400*    ÅRSOMSÄTTNINGEN BESTÄMMER HUR OFTA AVROP SKALL SKE(W-Q-FREKV)        
258500                                                                          
258600         MOVE W-BUFF TO W-BUFF-VV                                         
258700         MOVE +1 TO IX-W-Q-FREKV                                          
258800         MOVE IX TO IX-KOM                                                
258900         PERFORM UNTIL IX-W-Q-FREKV > W-Q-FREKV-MAX                       
259000            ADD +1 TO IX-KOM                                              
259100            COMPUTE W-VECKO-DIFFERENS-VV = W-VECKO-DIFFERENS + 3          
259200            IF IX-KOM < W-VECKO-DIFFERENS-VV AND                          
259300               IX-KOM < MAX-BEHOVSVECKOR-I-TAB                            
259400              SUBTRACT TILLGTAB-ANTAL     (IX-KOM) FROM W-BUFF-VV         
259500              ADD      LNK2-KVBEHOV-VECKA (IX-KOM) TO   W-BUFF-VV         
259600            END-IF                                                        
259700            IF IX-W-Q-FREKV = 1                                           
259800               MOVE W-BUFF-VV TO W-BUFF-VV-1                              
259900            END-IF                                                        
260000            ADD +1 TO IX-W-Q-FREKV                                        
260100         END-PERFORM                                                      
260200                                                                          
260300         IF  W-TILLG < W-BUFF-VV                                          
260400           IF FOERST AND W-Q-FREKV-MAX = 3 AND                            
260500             W-TILLG NOT < W-BUFF-VV-1                                    
260600***         *   FÖR ARTIKLAR MED HEMTAGNING VAR 3:E VECKA                 
260700***         *VI SKALL INTE BÖRJA TA HEM 1:A GÅNGEN FÖRRÄN VI              
260800***         *KOMMER UNDER SÄKERHETSLAGRET REDAN VECKA IX+1                
260900             CONTINUE                                                     
261000           ELSE                                                           
261100              PERFORM GEC-BERAEKNA-AVROPSKV-NYTT                          
261200              ADD W-AVROPSKVANTITET TO W-TILLG                            
261300              MOVE W-AVROPSKVANTITET TO LINK3-KVAVROP                     
261400              IF W-AVROPSKVANTITET > ZERO                                 
261500                PERFORM GEB-SKAPA-AVROP                                   
261600*********       JUSTERA TILLG DE VECKOR VI EV HOPPAR ÖVER                 
261700                MOVE +2 TO IX-W-Q-FREKV                                   
261800                PERFORM UNTIL IX-W-Q-FREKV > W-Q-FREKV-MAX                
261900                   ADD +1 TO IX                                           
262000                   ADD TILLGTAB-ANTAL (IX)                                
262100                                           TO W-TILLG                     
262200                   SUBTRACT LNK2-KVBEHOV-VECKA  (IX)                      
262300                                           FROM W-TILLG                   
262400                   ADD +1 TO IX-W-Q-FREKV                                 
262500                END-PERFORM                                               
262600                MOVE NEJ TO FOERST-SW                                     
262700              END-IF                                                      
262800           END-IF                                                         
262900         END-IF                                                           
263000       ELSE                                                               
263100         IF  W-TILLG < W-BUFF                                             
263200           PERFORM GEA-BERAEKNA-AVROPSKVANTITET                           
263300           ADD W-AVROPSKVANTITET TO W-TILLG                               
263400           MOVE W-AVROPSKVANTITET TO LINK3-KVAVROP                        
263500           IF W-AVROPSKVANTITET > ZERO                                    
263600             PERFORM GEB-SKAPA-AVROP                                      
263700           END-IF                                                         
263800         END-IF                                                           
263900       END-IF                                                             
264000       ADD 1 TO IX                                                        
264100     END-PERFORM                                                          
264200     .                                                                    
264300     EJECT                                                                
264400 GEE-KOLL-WLC-2AAR SECTION.                                               
264500                                                                          
264600     MOVE ZERO                    TO WS-SUMMA-BEHOV                       
264700     MOVE ZERO                    TO WS-SATS-VVBEHOV                      
264800     ADD  WS-SUM-START            TO WS-SUMMA-BEHOV                       
264900     MOVE NEJ                     TO WS-FLAGGA-WLC                        
265000                                     WS-FLAGGA-2AAR                       
265100                                     WS-FLAGGA-REDUC                      
265200***  *HAR ARTIKEL MINDRE ÄN 2 ÅR TILL PRELIMINÄR SKROT ?                  
265300     MOVE LINK-TISKPREL           TO TMP1-YYWW                            
265400     MOVE LINK-TISPECST           TO TMP2-YYWW                            
265500     PERFORM WY2000P3                                                     
265600     IF LINK-FLSKROT-BEV = JA                                             
265700        IF (TMP1-YYWW NOT < TMP2-YYWW)                                    
265800***        *BERÄKNA ANTAL VECKOR TILL SKROT                               
265900           MOVE LINK-TISPECST     TO W-DATUM-FROM                         
266000           MOVE LINK-TISKPREL     TO W-DATUM-TOM                          
266100           PERFORM S02-BERAKNA-VECKODIFFERENS                             
266200           MOVE W-VECKO-DIFFERENS TO WS-VVBEHOV                           
266300           MOVE JA                TO WS-FLAGGA-WLC                        
266400***WLC2                                                                   
266500           IF LINK-KVPB-SATS > ZERO                                       
266600              IF WS-VVBEHOV > 52                                          
266700                 MOVE WS-VVBEHOV  TO WS-SATS-VVBEHOV                      
266800                 MOVE 52          TO WS-VVBEHOV                           
266900              END-IF                                                      
267000           END-IF                                                         
267100        ELSE                                                              
267200***        *TISKPREL REDAN PASSERAD                                       
267300           MOVE ZERO              TO WS-VVBEHOV                           
267400           MOVE JA                TO WS-FLAGGA-WLC                        
267500        END-IF                                                            
267600     ELSE                                                                 
267700        MOVE LINK-TIFINLV         TO WS-AAVVD-TIFINLV                     
267800        MOVE WS-AAVV-TIFINLV      TO WS-TIAAVV                            
267900        IF  WS-TIAAVV > 5000                                              
268000            MOVE 19               TO WS-TISS                              
268100        ELSE                                                              
268200            MOVE 20               TO WS-TISS                              
268300        END-IF                                                            
268400        ADD 200                   TO WS-TISSAAVV                          
268500        MOVE WS-TIAAVV            TO TMP1-YYWW                            
268600        MOVE AKT-DATUM-AAVV       TO TMP2-YYWW                            
268700        PERFORM WY2000P3                                                  
268800***     *ÄR ATIKEL ÄLDRE ÄN 2 ÅR ?                                        
268900        IF LINK-TIFINLV > ZERO AND TMP2-YYWW > TMP1-YYWW                  
269000***        *KOLL MANUELL EJ PASSERAD LEVERANSPLANESPÄRR                   
269100***        *ANNARS BERÄKNA ANTAL VECKOR 2 ÅR                              
269200           MOVE LINK-TILPSP       TO TMP1-YYWW                            
269300           MOVE AKT-DATUM-AAVV    TO TMP2-YYWW                            
269400           PERFORM WY2000P3                                               
269500           IF LINK-KDLPSP = 3 AND TMP1-YYWW > TMP2-YYWW                   
269600              MOVE NEJ            TO WS-FLAGGA-2AAR                       
269700           ELSE                                                           
269800              MOVE 104            TO WS-VVBEHOV                           
269900              MOVE JA             TO WS-FLAGGA-2AAR                       
270000***WLC2                                                                   
270100              IF LINK-KVPB-SATS > ZERO                                    
270200                 MOVE 52          TO WS-VVBEHOV                           
270300              END-IF                                                      
270400           END-IF                                                         
270500        ELSE                                                              
270600           MOVE NEJ               TO WS-FLAGGA-WLC                        
270700           MOVE NEJ               TO WS-FLAGGA-2AAR                       
270800        END-IF                                                            
270900     END-IF                                                               
271000                                                                          
271100     IF WS-FLAGGA-WLC = JA OR WS-FLAGGA-2AAR = JA                         
271200***     *KOPIERA LNK2-AREA                                                
271300***     *CALL W22222 MED WS-VVBEHOV                                       
271400***     *RÄKNA UT SUMMA BEHOV TILL SKROT/2 ÅR                             
271500        MOVE LNK2-AREA            TO TAB2-AREA                            
271600        MOVE WS-VVBEHOV           TO TAB2-KVVECKOR-BEHOV                  
271700                                                                          
271800        CALL W22222 USING TAB2-AREA W22X-AA-PCB   W22X-WDK7-PCB           
271900                                    W22X-ARTM-PCB W22X-2501-PCB           
272000                                    W22X-WDB6R-PCB                        
272100                                    W22X-WDK7R-PCB                        
272200                                    W22X-WDB6-PCB W22X-WDD7-PCB           
272300                                    W22X-WDK7E-PCB                        
272400                                    W22X-UTIL-WDK6-PCB                    
272500                                    W22X-UTIL-WDK7-PCB                    
272600                                    W22X-UTIL-WDB6-PCB                    
272700                                    W22X-UTUP-WDK7-PCB                    
272800                                    W22X-UTUP-WDB6-PCB                    
272900                                    W22X-UTUP-UTIL-WDK6-PCB               
273000                                    W22X-UTUP-UTIL-WDK7-PCB               
273100                                    W22X-UTUP-UTIL-WDB6-PCB               
273200                                                                          
273300***     *SUMMERING BEHOV FINNS I TAB2-KVBEHOV-SUMMA                       
273400***WLC2                                                                   
273500        IF WS-FLAGGA-WLC = JA AND LINK-KVPB-SATS > ZERO                   
273600           IF WS-SATS-VVBEHOV > 52                                        
273700              COMPUTE TAB2-KVBEHOV-SUMMA =                                
273800                      TAB2-KVBEHOV-SUMMA * (WS-SATS-VVBEHOV / 52)         
273900           END-IF                                                         
274000        END-IF                                                            
274100                                                                          
274200        IF WS-FLAGGA-2AAR = JA                                            
274300***WLC2                                                                   
274400           IF LINK-KVPB-SATS > ZERO                                       
274500              COMPUTE TAB2-KVBEHOV-SUMMA =                                
274600                      TAB2-KVBEHOV-SUMMA * 2                              
274700           END-IF                                                         
274800***********vi adderar start-tillg för att kunna göra                      
274900***********rätt jämförelse senare                                         
275000           COMPUTE TAB2-KVBEHOV-SUMMA = TAB2-KVBEHOV-SUMMA +              
275100                                        WS-SUM-START                      
275200        END-IF                                                            
275300     END-IF                                                               
275400     .                                                                    
275500     EJECT                                                                
275600 GEA-BERAEKNA-AVROPSKVANTITET SECTION.                                    
275700     SKIP1                                                                
275800*                                                                         
275900     MOVE LINK-KVQ TO W-KVANTITET                                         
276000     IF  LINK-TIQJUST > ZERO                                              
276100        MOVE W-DATUM-AAVV-AKT TO W-DATUM-AAVV-HELP                        
276200                                                                          
276300        COMPUTE W-KVDAGAR-FFH =                                           
276400                LINK-KVDAGAR-INLEV + LINK-KVDAGAR-TT                      
276500        COMPUTE W-ANTAL-VECKOR ROUNDED =                                  
276600                IX - (W-KVDAGAR-FFH / 5)                                  
276700        CALL W009VADD USING W-DATUM-AAVV-HELP W-ANTAL-VECKOR              
276800        MOVE LINK-TIQJUST        TO TMP1-YYWW                             
276900        MOVE W-DATUM-AAVV-HELP   TO TMP2-YYWW                             
277000        PERFORM WY2000P3                                                  
277100        IF TMP1-YYWW <= TMP2-YYWW                                         
277200           MOVE LINK-KVQ-JUST TO W-KVANTITET                              
277300        ELSE                                                              
277400           MOVE LINK-KVQ TO W-KVANTITET                                   
277500        END-IF                                                            
277600     END-IF                                                               
277700                                                                          
277800     IF LINK-KVULOAD > ZERO                                               
277900        MOVE LINK-KVULOAD TO WS-KVULOAD                                   
278000     ELSE                                                                 
278100        MOVE LINK-KVPALL  TO WS-KVULOAD                                   
278200     END-IF                                                               
278300     IF LINK-FLNYBER = JA AND WS-KVULOAD > ZERO                           
278400        PERFORM UNTIL (W-TILLG + W-KVANTITET) >=                          
278500                       W-BUFF                                             
278600**********************(W-BUFF + LINK-KVEOQ)                               
278700           ADD WS-KVULOAD TO W-KVANTITET                                  
278800        END-PERFORM                                                       
278900        MOVE W-KVANTITET TO W-AVROPSKVANTITET                             
279000     ELSE                                                                 
279100        IF  W-KVANTITET = ZERO                                            
279200            MOVE 1 TO W-KVANTITET                                         
279300        END-IF                                                            
279400        COMPUTE W-ANTAL ROUNDED = ((W-KVANTITET / 2                       
279500                                  - W-TILLG                               
279600                                  + W-BUFF)                               
279700                                /   W-KVANTITET)                          
279800                                +   0.49                                  
279900        COMPUTE W-AVROPSKVANTITET ROUNDED =                               
280000                                        W-KVANTITET * W-ANTAL             
280100     END-IF                                                               
280200*                                                                         
280300     SKIP1                                                                
280400     IF  W-AVROPSKVANTITET < 1                                            
280500         MOVE 1 TO W-AVROPSKVANTITET                                      
280600     END-IF                                                               
280700     .                                                                    
280800     EJECT                                                                
280900 GEB-SKAPA-AVROP SECTION.                                                 
281000******************************************************************        
281100*                                                                *        
281200*    AVROP SKAPAS OCH SKRIVES PÅ REGISTRET                       *        
281300*                                                                *        
281400*    SATSER HAR ALLTID AVROP PÅ EN MÅNDAG.INLB23-TILEVDAG = 1    *        
281500*    (man behöver inte flytta avrop jul/nyår om ej tilevdag=1)   *        
281600*                                                                *        
281700******************************************************************        
281800     SKIP1                                                                
281900     IF  (LINK-KDERS (1) > ZERO AND < 10)                                 
282000                                                                          
282100         IF  W-KVAVROP-VVKL12-ACC + W-AVROPSKVANTITET NOT <               
282200                                    W-KVBEST-REST                         
282300             MOVE JA TO SW-BESTREST-TAEKT                                 
282400             COMPUTE LINK3-KVAVROP = W-KVBEST-REST                        
282500                               - W-KVAVROP-VVKL12-ACC                     
282600         ELSE                                                             
282700             ADD W-AVROPSKVANTITET TO W-KVAVROP-VVKL12-ACC                
282800         END-IF                                                           
282900     END-IF                                                               
283000                                                                          
283100     MOVE IX TO W-ANTAL-VECKOR                                            
283200                                                                          
283300     MOVE W-DATUM-AAVV-AKT TO LINK3-TIAVROP-DISP                          
283400     CALL W009VADD USING LINK3-TIAVROP-DISP W-ANTAL-VECKOR                
283500                                                                          
283600*TILEVDAG                                                                 
283700     MOVE ZERO         TO LINK3-TILEVDAG                                  
283800     MOVE +1           TO IX-DAG                                          
283900     PERFORM UNTIL IX-DAG > 5                                             
284000        IF WART-TILEVDAG (IX-DAG) > ZERO                                  
284100           MOVE WART-TILEVDAG (IX-DAG) TO LINK3-TILEVDAG                  
284200           ADD +5      TO IX-DAG                                          
284300        END-IF                                                            
284400        ADD +1         TO IX-DAG                                          
284500     END-PERFORM                                                          
284600     IF LINK3-TILEVDAG = ZERO                                             
284700        MOVE LINK-IDLEVNR          TO W-IDLEVNR                           
284800        MOVE LINK-IDLEVNR-SHIP     TO W-IDLEVNR-SHIP                      
284900        PERFORM IMS-GET-LEVA01-WDF101-SHIP                                
285000        IF SEGMENT-FINNS                                                  
285100          MOVE +1                  TO IX-DAG                              
285200          PERFORM UNTIL IX-DAG > 5                                        
285300             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
285400                MOVE F1-LEV-TILEVDAG (IX-DAG)                             
285500                                   TO LINK3-TILEVDAG                      
285600                ADD +5             TO IX-DAG                              
285700             END-IF                                                       
285800             ADD +1                TO IX-DAG                              
285900          END-PERFORM                                                     
286000          IF LINK3-TILEVDAG = ZERO                                        
286100             MOVE +1               TO LINK3-TILEVDAG                      
286200          END-IF                                                          
286300        ELSE                                                              
286400          MOVE +1                  TO LINK3-TILEVDAG                      
286500        END-IF                                                            
286600     END-IF                                                               
286700                                                                          
286800     COMPUTE WS-SUMMA-KVPB = LINK-KVPB-SEP  +                             
286900                             LINK-KVPB-SATS +                             
287000                             LINK-KVPB-TPO  +                             
287100                             LINK-KVPB-SDC                                
287200                                                                          
287300     MOVE LINK-IDLEVNR    TO OLIKA-LEV                                    
287400                                                                          
287500     IF  SATS-LEVNR                                                       
287600     OR  LINK-KDHF > ZERO                                                 
287700        MOVE LINK3-TIAVROP-DISP TO LINK3-TIAVROP-AVS                      
287800                                   W-TIAVROP-AVS                          
287900*WZ20DAYS                                                                 
288000        MOVE 0                    TO WS-DAYS-TIAAVVD                      
288100        COMPUTE WS-DAYS-TIAAVVD = 10 * LINK3-TIAVROP-AVS +                
288200                                  LINK3-TILEVDAG                          
288300        MOVE WS-DAYS-TIAAVVD      TO DAYS-TIDATE1                         
288400        MOVE 'YYWWD'              TO DAYS-KDDATFMT1                       
288500        MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                       
288600        MOVE 0                    TO DAYS-KVDAYS                          
288700        MOVE SPACE                TO DAYS-TIDATE2                         
288800                                  DAYS-IDCALEND                           
288900        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
289000*                                                                         
289100        IF DAYS-KDRC = 8                                                  
289200          MOVE 'FEL VID ANROP TILL WZ20DAYS X' TO FELTEXT                 
289300          CALL ABEND USING RKOD                                           
289400        ELSE                                                              
289500          MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                        
289600          MOVE W-TIAAMMDD-AVS TO LINK3-TIAVRDAT-INL                       
289700                                 LINK3-TIAVRDAT-DISP                      
289800        END-IF                                                            
289900*FIX-START nyår DISP-09  sats                                             
290000        IF (LINK-IDLEVNR NOT = 'DL7EA')            AND                    
290100           (WS-SUMMA-KVPB  < 5.0)                  AND                    
290200           (LINK3-TIAVROP-DISP = 0951 OR 0952 OR 0953) AND                
290300           (LINK3-TILEVDAG = 1)                                           
290400                 MOVE 100111   TO  W-TIAAMMDD-AVS                         
290500                 MOVE 100111   TO  LINK3-TIAVRDAT-DISP                    
290600                 MOVE 100111   TO  LINK3-TIAVRDAT-INL                     
290700                 MOVE 1002     TO  LINK3-TIAVROP-AVS                      
290800                                   W-TIAVROP-AVS                          
290900        END-IF                                                            
291000*FIX-END                                                                  
291100     ELSE                                                                 
291200*AVS                                                                      
291300        COMPUTE W-KVDAGAR-FFH =                                           
291400                LINK-KVDAGAR-INLEV + LINK-KVDAGAR-TT                      
291500        COMPUTE W-ANTAL-VECKOR ROUNDED =                                  
291600                W-KVDAGAR-FFH / -5                                        
291700        MOVE LINK3-TIAVROP-DISP TO LINK3-TIAVROP-AVS                      
291800        CALL W009VADD USING LINK3-TIAVROP-AVS W-ANTAL-VECKOR              
291900        MOVE LINK3-TIAVROP-AVS  TO W-TIAVROP-AVS                          
292000*                                                                         
292100*FIX-START NYÅR 2025                                                      
292200        MOVE LINK-IDANSK              TO WS-SKIP-IDANSK-2025              
292300        IF SKIP-IDANSK-2025                                               
292400           IF LINK3-TIAVROP-AVS = 2548 OR 2549                            
292500              MOVE 2603               TO  FIX-TIAVROP-AVS                 
292600              MOVE FIX-TIAVROP-AVS    TO  LINK3-TIAVROP-AVS               
292700              MOVE LINK3-TIAVROP-AVS  TO  W-TIAVROP-AVS                   
292800           ELSE                                                           
292900              IF LINK3-TIAVROP-AVS = 2550 OR 2551                         
293000                 MOVE 2604               TO  FIX-TIAVROP-AVS              
293100                 MOVE FIX-TIAVROP-AVS    TO  LINK3-TIAVROP-AVS            
293200                 MOVE LINK3-TIAVROP-AVS  TO  W-TIAVROP-AVS                
293300              END-IF                                                      
293400           END-IF                                                         
293500        END-IF                                                            
293600*                                                                         
293700*FIX-START NYÅR 2024                                                      
293800        MOVE LINK-IDANSK              TO WS-SKIP-IDANSK-2024              
293900        MOVE LINK-IDLEVNR             TO WS-SKIP-IDLEVNR-2024             
294000        IF SKIP-IDANSK-2024                                               
294100        OR SKIP-IDLEVNR-2024                                              
294200           IF LINK3-TIAVROP-AVS = 2451 OR 2452                            
294300              MOVE 2502               TO  FIX-TIAVROP-AVS                 
294400              MOVE FIX-TIAVROP-AVS    TO  LINK3-TIAVROP-AVS               
294500              MOVE LINK3-TIAVROP-AVS  TO  W-TIAVROP-AVS                   
294600           END-IF                                                         
294700        END-IF                                                            
294800*                                                                         
294900*COMMENTING BELOW OLD CODE - RETAINED FOR REFERENCE - 202410              
295000*FIX-START NYÅR 2018                                                      
295100*       IF LINK-IDLEVNR = 'BP3EA'                                         
295200*         IF LINK-IDANSK = 430 OR 680 OR 685 OR 718 OR 730                
295300*           IF LINK3-TIAVROP-AVS = 1851                                   
295400*              MOVE 1902              TO  FIX-TIAVROP-AVS                 
295500*              MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
295600*              MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
295700*           END-IF                                                        
295800*         END-IF                                                          
295900*       END-IF                                                            
296000*                                                                         
296100*       IF LINK-IDLEVNR = 'BSBZA' AND (LINK-IDANSK = 500)                 
296200*         IF LINK3-TIAVROP-AVS = 1851                                     
296300*            MOVE 1902              TO  FIX-TIAVROP-AVS                   
296400*            MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS                 
296500*            MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                     
296600*         END-IF                                                          
296700*       END-IF                                                            
296800*                                                                         
296900*       IF LINK-IDLEVNR = 'BP7YA' AND (LINK-IDANSK = 695)                 
297000*         IF LINK3-TIAVROP-AVS = 1851                                     
297100*            MOVE 1902              TO  FIX-TIAVROP-AVS                   
297200*            MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS                 
297300*            MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                     
297400*         END-IF                                                          
297500*                                                                         
297600*         IF LINK3-TIAVROP-AVS = 1852 AND                                 
297700*            (LINK3-TILEVDAG = 2 OR 3)                                    
297800*                                                                         
297900*            MOVE 1902              TO  FIX-TIAVROP-AVS                   
298000*            MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS                 
298100*            MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                     
298200*         END-IF                                                          
298300*                                                                         
298400*         IF LINK3-TIAVROP-AVS = 1901 AND                                 
298500*            (LINK3-TILEVDAG = 2)                                         
298600*                                                                         
298700*            MOVE 1902              TO  FIX-TIAVROP-AVS                   
298800*            MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS                 
298900*            MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                     
299000*         END-IF                                                          
299100*       END-IF                                                            
299200*                                                                         
299300*       IF LINK-IDLEVNR = 'BP8BA' AND (LINK-IDANSK = 680)                 
299400*         IF LINK3-TIAVROP-AVS = 1851                                     
299500*            MOVE 1902              TO  FIX-TIAVROP-AVS                   
299600*            MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS                 
299700*            MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                     
299800*         END-IF                                                          
299900*       END-IF                                                            
300000*FIX-END NYÅR 2018                                                        
300100                                                                          
300200*AVS-AAMMDD                                                               
300300*WZ20DAYS                                                                 
300400        MOVE 0                    TO WS-DAYS-TIAAVVD                      
300500        COMPUTE WS-DAYS-TIAAVVD = 10 * LINK3-TIAVROP-AVS +                
300600                                  LINK3-TILEVDAG                          
300700        MOVE WS-DAYS-TIAAVVD      TO DAYS-TIDATE1                         
300800        MOVE 'YYWWD'              TO DAYS-KDDATFMT1                       
300900        MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                       
301000        MOVE 0                    TO DAYS-KVDAYS                          
301100        MOVE SPACE                TO DAYS-TIDATE2                         
301200                                  DAYS-IDCALEND                           
301300        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
301400*                                                                         
301500        IF DAYS-KDRC = 8                                                  
301600          MOVE 'FEL VID ANROP TILL WZ20DAYSXX' TO FELTEXT                 
301700          CALL ABEND USING RKOD                                           
301800        ELSE                                                              
301900          MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                        
302000                                    ARB-TIAAMMDD-AVS                      
302100                                    GAM-TIAAMMDD-AVS                      
302200          MOVE LINK3-TILEVDAG TO WOL-TILEVDAG                             
302300*                                                                         
302400          MOVE W-TIAAMMDD-AVS     TO DAYS-TIDATE1                         
302500          MOVE 'YYMMDD'           TO DAYS-KDDATFMT1                       
302600          MOVE 'YYWW'             TO DAYS-KDDATFMT2                       
302700          MOVE 0                  TO DAYS-KVDAYS                          
302800          MOVE SPACE              TO DAYS-TIDATE2                         
302900                                    DAYS-IDCALEND                         
303000          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
303100*                                                                         
303200          IF DAYS-KDRC = 8                                                
303300            MOVE 'FEL VID ANROP TILL WZ20DAYS Y' TO FELTEXT               
303400            CALL ABEND USING RKOD                                         
303500          ELSE                                                            
303600            MOVE DAYS-TIDATE2(1:4) TO WOL-TIAAVV-AVS                      
303700*WDATKONV MOVE DAT-TIAAVV-GRP TO WOL-TIAAVV-AVS                           
303800          END-IF                                                          
303900        END-IF                                                            
304000                                                                          
304100*     AVS TVÅ VECKOR BAKÅT OM ARTIKELN SKALL VARA DISPONIBEL UNDER        
304200*     PUBLICERINGSVECKA                                                   
304300        MOVE W-DATUM-AAVV-AKT     TO TMP1-YYWW                            
304400        MOVE W-TIFINLV-1-4        TO TMP2-YYWW                            
304500        PERFORM WY2000P3                                                  
304600        IF TMP1-YYWW + 1 <= TMP2-YYWW                                     
304700        AND LINK3-TIAVROP-DISP = W-TIFINLV-1-4                            
304800                                                                          
304900*          UNDERSÖKNING OM AVS BLEV FÖR NÄRA I TID                        
305000           MOVE W-DATUM-AAVV-AKT TO W-DATUM-AAVV-HELP                     
305100           MOVE +1 TO W-ANTAL-VECKOR                                      
305200           CALL W009VADD USING W-DATUM-AAVV-HELP W-ANTAL-VECKOR           
305300           MOVE LINK3-TIAVROP-AVS   TO TMP1-YYWW                          
305400           MOVE W-DATUM-AAVV-HELP   TO TMP2-YYWW                          
305500           PERFORM WY2000P3                                               
305600           IF TMP1-YYWW < TMP2-YYWW                                       
305700              MOVE W-DATUM-AAVV-HELP TO LINK3-TIAVROP-AVS                 
305800           END-IF                                                         
305900*WZ20DAYS                                                                 
306000           MOVE LINK3-TIAVROP-AVS  TO W-TIAVROP-AVS                       
306100           MOVE 0                 TO WS-DAYS-TIAAVVD                      
306200           COMPUTE WS-DAYS-TIAAVVD = 10 * LINK3-TIAVROP-AVS +             
306300                                     LINK3-TILEVDAG                       
306400           MOVE WS-DAYS-TIAAVVD   TO DAYS-TIDATE1                         
306500           MOVE 'YYWWD'           TO DAYS-KDDATFMT1                       
306600           MOVE 'YYMMDD'          TO DAYS-KDDATFMT2                       
306700           MOVE 0                 TO DAYS-KVDAYS                          
306800           MOVE SPACE             TO DAYS-TIDATE2                         
306900                                     DAYS-IDCALEND                        
307000           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
307100                                                                          
307200           IF DAYS-KDRC = 8                                               
307300             MOVE 'FEL VID ANROP TILL WZ20DAYS Z' TO FELTEXT              
307400             CALL ABEND USING RKOD                                        
307500           ELSE                                                           
307600             MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                     
307700                                       ARB-TIAAMMDD-AVS                   
307800                                       GAM-TIAAMMDD-AVS                   
307900             MOVE LINK3-TILEVDAG    TO WOL-TILEVDAG                       
308000*                                                                         
308100             MOVE W-TIAAMMDD-AVS  TO DAYS-TIDATE1                         
308200             MOVE 'YYMMDD'        TO DAYS-KDDATFMT1                       
308300             MOVE 'YYWW'          TO DAYS-KDDATFMT2                       
308400             MOVE 0               TO DAYS-KVDAYS                          
308500             MOVE SPACE           TO DAYS-TIDATE2                         
308600                                       DAYS-IDCALEND                      
308700             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
308800*                                                                         
308900             IF DAYS-KDRC = 8                                             
309000               MOVE 'FEL VID ANROP TILL WZ20DAYS U' TO FELTEXT            
309100               CALL ABEND USING RKOD                                      
309200             ELSE                                                         
309300               MOVE DAYS-TIDATE2(1:4) TO WOL-TIAAVV-AVS                   
309400*WDATKONV      MOVE DAT-TIAAVV-GRP TO WOL-TIAAVV-AVS                      
309500             END-IF                                                       
309600           END-IF                                                         
309700        END-IF                                                            
309800*FIX NYÅR START  DATKONVJUST 2014                                         
309900        COMPUTE FIX-AAVVD = 10 * LINK3-TIAVROP-AVS +                      
310000                                 LINK3-TILEVDAG                           
310100        IF FIX-AAVVD = 15011                                              
310200           MOVE 141229    TO W-TIAAMMDD-AVS                               
310300                             ARB-TIAAMMDD-AVS                             
310400                             GAM-TIAAMMDD-AVS                             
310500           MOVE 1         TO WOL-TILEVDAG                                 
310600           MOVE 1501      TO WOL-TIAAVV-AVS                               
310700        END-IF                                                            
310800        IF FIX-AAVVD = 15012                                              
310900           MOVE 141230    TO W-TIAAMMDD-AVS                               
311000                             ARB-TIAAMMDD-AVS                             
311100                             GAM-TIAAMMDD-AVS                             
311200           MOVE 2         TO WOL-TILEVDAG                                 
311300           MOVE 1501      TO WOL-TIAAVV-AVS                               
311400        END-IF                                                            
311500        IF FIX-AAVVD = 15013                                              
311600           MOVE 141231    TO W-TIAAMMDD-AVS                               
311700                             ARB-TIAAMMDD-AVS                             
311800                             GAM-TIAAMMDD-AVS                             
311900           MOVE 3         TO WOL-TILEVDAG                                 
312000           MOVE 1501      TO WOL-TIAAVV-AVS                               
312100        END-IF                                                            
312200*FIX NYÅR END DATKONVJUST 2014                                            
312300                                                                          
312400        MOVE LINK-IDLEVNR        TO WS-IDLEVNR-EMIL                       
312500        IF (NOT EJ-GODK-EMIL-LEVNR) AND                                   
312600           (LINK-KVQ > ZERO)        AND                                   
312700           ((W-ARSBEH / LINK-KVQ) > 35)                                   
312800                                                                          
312900*---    DAGLIGA AVROP BEHANDLAS SENARE                                    
313000                                                                          
313100           CONTINUE                                                       
313200        ELSE                                                              
313300**        LÄS HELG-TAB  IDLANDX2+DATUM                                    
313400**        OM TRÄFF JUSTERA DATUM (OBS LINK3-TILEVDAG ?)                   
313500                                                                          
313600          PERFORM GEBD-KOLL-HELGDAG                                       
313700        END-IF                                                            
313800                                                                          
313900*INL                                                                      
314000        MOVE 2                   TO WORK-KDCALL                           
314100        MOVE WC-CDC-SE           TO WORK-IDDC                             
314200        MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                     
314300        MOVE LINK-KVDAGAR-TT     TO WORK-KVWORKD                          
314400        ADD +1                   TO WORK-KVWORKD                          
314500        CALL WORKDAY USING  WORK-KDCALL                                   
314600             WORK-DATE-AREA WORK-KDSVAR                                   
314700        MOVE WORK-TIAAMMDD-TOM   TO LINK3-TIAVRDAT-INL                    
314800*DISP                                                                     
314900        MOVE 2                   TO WORK-KDCALL                           
315000        MOVE WC-CDC-SE           TO WORK-IDDC                             
315100        MOVE LINK3-TIAVRDAT-INL  TO WORK-TIAAMMDD-FOM                     
315200        MOVE LINK-KVDAGAR-INLEV  TO WORK-KVWORKD                          
315300        ADD +1                   TO WORK-KVWORKD                          
315400        CALL WORKDAY USING  WORK-KDCALL                                   
315500             WORK-DATE-AREA WORK-KDSVAR                                   
315600        MOVE WORK-TIAAMMDD-TOM   TO LINK3-TIAVRDAT-DISP                   
315700     END-IF                                                               
315800                                                                          
315900     IF WS-FLAGGA-WLC = JA OR WS-FLAGGA-2AAR = JA                         
316000***     *TEST OM SUMMA AVROP > SUMMA-BEHOV                                
316100***     *SÄTT FLAGG-REDUC  ELLER FLAGG-EXCEED                             
316200        ADD W-AVROPSKVANTITET       TO WS-SUMMA-BEHOV                     
316300        IF WS-SUMMA-BEHOV > TAB2-KVBEHOV-SUMMA                            
316400           IF WS-FLAGGA-WLC = JA                                          
316500              COMPUTE W-AVROPSKVANTITET = W-AVROPSKVANTITET -             
316600                      (WS-SUMMA-BEHOV - TAB2-KVBEHOV-SUMMA)               
316700              IF W-AVROPSKVANTITET < ZERO OR                              
316800                 WS-VVBEHOV        = ZERO                                 
316900                 MOVE ZERO          TO W-AVROPSKVANTITET                  
317000              END-IF                                                      
317100              MOVE JA               TO WS-FLAGGA-REDUC                    
317200              MOVE W-AVROPSKVANTITET TO LINK3-KVAVROP                     
317300              IF LINK-FLSKROT-WLC = NEJ                                   
317400                 MOVE JA            TO LINK-FLSKROT-WLC                   
317500                 MOVE NEJ           TO SW-AUT-PLAN                        
317600***              * + KDLPORS                                              
317700                 MOVE 28            TO W-KDLPORS                          
317800                 PERFORM S01-ADD-TILL-ORSAKSTABELL                        
317900              END-IF                                                      
318000           ELSE                                                           
318100*----         *WS-FLAGGA-2AAR = JA                                        
318200*----         * + KDLPORS                                                 
318300              MOVE 4                TO W-ANTAL-VECKOR                     
318400              MOVE LINK-TISPECST    TO VADD-DATUM-AAVV                    
318500              CALL W009VADD USING  VADD-DATUM-AAVV W-ANTAL-VECKOR         
318600                                                                          
318700              IF W-TIAVROP-AVS  >= LINK-TISPECST AND                      
318800                 W-TIAVROP-AVS  <  VADD-DATUM-AAVV                        
318900                   MOVE NEJ         TO SW-AUT-PLAN                        
319000                   MOVE 29          TO W-KDLPORS                          
319100                   PERFORM S01-ADD-TILL-ORSAKSTABELL                      
319200              END-IF                                                      
319300           END-IF                                                         
319400        END-IF                                                            
319500     END-IF                                                               
319600                                                                          
319700*----HÄR KOLLAR MAN OM AVROPSVECKAN ÄR BLOCKAD FÖR IDLEVNR-SHIP           
319800*----OCH SKALL FLYTTAS VIA SUB-PGM W221BLOC.                              
319900*----                                                                     
320000     IF LINK3-KVAVROP > ZERO                                              
320100       PERFORM GEBB-KOLL-BLOCKAD-VECKA                                    
320200     END-IF                                                               
320300                                                                          
320400*    HÄR GÖRS NÅGON TEST SOM ANGER VILKA AVROP SOM SKALL                  
320500*    SMETAS UT PÅ LEVERANTÖRENS LEVDAGAR AVROPS-VECKAN                    
320600     MOVE LINK-IDLEVNR        TO WS-IDLEVNR-EMIL                          
320700     IF (NOT EJ-GODK-EMIL-LEVNR) AND                                      
320800        (LINK-KVQ > ZERO)        AND                                      
320900        ((W-ARSBEH / LINK-KVQ) > 35)                                      
321000                                                                          
321100        IF SW-BLOCKAD-VECKA = JA                                          
321200          PERFORM S03-TILEVDAG-DAGL-AVROP                                 
321300          MOVE JA   TO BLOC-FLAGGA-DAGL-AVROP                             
321400        ELSE                                                              
321500          PERFORM S03-TILEVDAG-DAGL-AVROP                                 
321600          PERFORM GEBA-SKAPA-DAGL-AVROP                                   
321700        END-IF                                                            
321800     ELSE                                                                 
321900                                                                          
322000        IF SW-BLOCKAD-VECKA = JA                                          
322100          MOVE NEJ  TO BLOC-FLAGGA-DAGL-AVROP                             
322200        ELSE                                                              
322300          IF LINK3-KVAVROP > ZERO                                         
322400              MOVE NYUPPL-AVROP    TO LINK3-KDCALL                        
322500              PERFORM S22B-NYUPPL-AVROP                                   
322600                                                                          
322700***         * I DENNA CALL GÖR VI REPL I STÄLLET FÖR ISRT                 
322800***         * OM SEGMENTET REDAN FINNS (OCH ADDERAR KVAVROP)              
322900          END-IF                                                          
323000        END-IF                                                            
323100     END-IF                                                               
323200     .                                                                    
323300     EJECT                                                                
323400 GEBA-SKAPA-DAGL-AVROP         SECTION.                                   
323500     MOVE 'GEBA-SKAPA-DAGL-AVROP '  TO CURRENT-SECTION                    
323600                                                                          
323700*------                                                                   
323800*ANTAL/TILEVDAG -  FLYTTAD TILL S03-TILEVDAG-DAGL-AVROP                   
323900*------                                                                   
324000                                                                          
324100*KVAVROP/DAG                                                              
324200     MOVE ZERO                  TO W-KVAVROP (1)                          
324300                                   W-KVAVROP (2)                          
324400                                   W-KVAVROP (3)                          
324500                                   W-KVAVROP (4)                          
324600                                   W-KVAVROP (5)                          
324700     IF LINK-KVPALL < +1                                                  
324800        MOVE +1                 TO WS-KVPALL                              
324900     ELSE                                                                 
325000        MOVE LINK-KVPALL        TO WS-KVPALL                              
325100     END-IF                                                               
325200     IF LINK-FLNYBER = JA AND LINK-KVQ > ZERO                             
325300        MOVE LINK-KVQ           TO WS-KVPALL                              
325400     END-IF                                                               
325500                                                                          
325600     IF LINK3-KVAVROP > ZERO                                              
325700        PERFORM UNTIL (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP(3)         
325800                     + W-KVAVROP(4) + W-KVAVROP(5))                       
325900                     NOT < LINK3-KVAVROP                                  
326000          MOVE +1               TO IX-DAG                                 
326100          PERFORM UNTIL    IX-DAG  > 5                                    
326200            IF W-TILEVDAG (IX-DAG) > ZERO                                 
326300               IF (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP (3) +          
326400                   W-KVAVROP(4) + W-KVAVROP(5)) < LINK3-KVAVROP           
326500                   ADD WS-KVPALL TO W-KVAVROP (IX-DAG)                    
326600               END-IF                                                     
326700            END-IF                                                        
326800            ADD +1               TO IX-DAG                                
326900            IF IX-DAG = 6                                                 
327000               IF LINK-KVULOAD > ZERO                                     
327100                  MOVE LINK-KVULOAD TO WS-KVULOAD                         
327200               ELSE                                                       
327300                  MOVE LINK-KVPALL  TO WS-KVULOAD                         
327400               END-IF                                                     
327500               IF LINK-FLNYBER = JA AND WS-KVULOAD > ZERO                 
327600                  MOVE WS-KVULOAD   TO WS-KVPALL                          
327700               END-IF                                                     
327800            END-IF                                                        
327900          END-PERFORM                                                     
328000        END-PERFORM                                                       
328100     END-IF                                                               
328200                                                                          
328300*AVS-AAVV                                                                 
328400     MOVE W-TIAVROP-AVS         TO LINK3-TIAVROP-AVS                      
328500                                    SPAR-TIAVROP-AVS                      
328600     MOVE +1                    TO IX-DAG                                 
328700     PERFORM UNTIL   (IX-DAG) > 5                                         
328800       IF W-TILEVDAG (IX-DAG) > ZERO AND W-KVAVROP (IX-DAG) > ZERO        
328900*                                                                         
329000*FIX-START NYÅR 2025                                                      
329100          MOVE LINK-IDANSK            TO WS-SKIP-IDANSK-2025              
329200          IF SKIP-IDANSK-2025                                             
329300            IF LINK3-TIAVROP-AVS = 2548 OR 2549                           
329400               MOVE 2603               TO  FIX-TIAVROP-AVS                
329500               MOVE FIX-TIAVROP-AVS    TO  LINK3-TIAVROP-AVS              
329600               MOVE LINK3-TIAVROP-AVS  TO  W-TIAVROP-AVS                  
329700            ELSE                                                          
329800               IF LINK3-TIAVROP-AVS = 2550 OR 2551                        
329900                  MOVE 2604               TO  FIX-TIAVROP-AVS             
330000                  MOVE FIX-TIAVROP-AVS    TO  LINK3-TIAVROP-AVS           
330100                  MOVE LINK3-TIAVROP-AVS  TO  W-TIAVROP-AVS               
330200               END-IF                                                     
330300            END-IF                                                        
330400          END-IF                                                          
330500*                                                                         
330600*FIX-START NYÅR 2024                                                      
330700          MOVE LINK-IDANSK            TO WS-SKIP-IDANSK-2024              
330800          MOVE LINK-IDLEVNR           TO WS-SKIP-IDLEVNR-2024             
330900          IF SKIP-IDANSK-2024                                             
331000          OR SKIP-IDLEVNR-2024                                            
331100            IF LINK3-TIAVROP-AVS = 2451 OR 2452                           
331200               MOVE 2502              TO  FIX-TIAVROP-AVS                 
331300               MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
331400               MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
331500            END-IF                                                        
331600          END-IF                                                          
331700*                                                                         
331800*COMMENTING BELOW OLD CODE - RETAINED FOR REFERENCE - 202410              
331900*FIX-START NYÅR 2018                                                      
332000*         IF LINK-IDLEVNR = 'BP3EA'                                       
332100*           IF LINK-IDANSK = 430 OR 680 OR 685 OR 718 OR 730              
332200*              IF LINK3-TIAVROP-AVS = 1851                                
332300*                                                                         
332400*                MOVE 1902              TO  FIX-TIAVROP-AVS               
332500*                MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS             
332600*                MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                 
332700*              END-IF                                                     
332800*           END-IF                                                        
332900*         END-IF                                                          
333000*                                                                         
333100*         IF LINK-IDLEVNR = 'BSBZA' AND (LINK-IDANSK = 500)               
333200*            IF LINK3-TIAVROP-AVS = 1851                                  
333300*                                                                         
333400*              MOVE 1902              TO  FIX-TIAVROP-AVS                 
333500*              MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
333600*              MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
333700*            END-IF                                                       
333800*         END-IF                                                          
333900*                                                                         
334000*         IF LINK-IDLEVNR = 'BP7YA' AND (LINK-IDANSK = 695)               
334100*            IF LINK3-TIAVROP-AVS = 1851                                  
334200*                                                                         
334300*              MOVE 1902              TO  FIX-TIAVROP-AVS                 
334400*              MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
334500*              MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
334600*            END-IF                                                       
334700*                                                                         
334800*            IF LINK3-TIAVROP-AVS = 1852 AND                              
334900*              (W-TILEVDAG (IX-DAG) = 2 OR 3)                             
335000*                                                                         
335100*              MOVE 1902              TO  FIX-TIAVROP-AVS                 
335200*              MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
335300*              MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
335400*            END-IF                                                       
335500*                                                                         
335600*            IF LINK3-TIAVROP-AVS = 1901 AND                              
335700*              (W-TILEVDAG (IX-DAG) = 2)                                  
335800*                                                                         
335900*              MOVE 1902              TO  FIX-TIAVROP-AVS                 
336000*              MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
336100*              MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
336200*            END-IF                                                       
336300*         END-IF                                                          
336400*                                                                         
336500*         IF LINK-IDLEVNR = 'BP8BA' AND (LINK-IDANSK = 680)               
336600*            IF LINK3-TIAVROP-AVS = 1851                                  
336700*                                                                         
336800*              MOVE 1902              TO  FIX-TIAVROP-AVS                 
336900*              MOVE FIX-TIAVROP-AVS   TO  LINK3-TIAVROP-AVS               
337000*              MOVE LINK3-TIAVROP-AVS TO  W-TIAVROP-AVS                   
337100*            END-IF                                                       
337200*         END-IF                                                          
337300*FIX-END NYÅR 2018                                                        
337400*                                                                         
337500*AVS-AAMMDD                                                               
337600*WZ20DAYS                                                                 
337700          MOVE 0                  TO WS-DAYS-TIAAVVD                      
337800          COMPUTE WS-DAYS-TIAAVVD = 10 * LINK3-TIAVROP-AVS +              
337900                                    W-TILEVDAG (IX-DAG)                   
338000          MOVE WS-DAYS-TIAAVVD    TO DAYS-TIDATE1                         
338100          MOVE 'YYWWD'            TO DAYS-KDDATFMT1                       
338200          MOVE 'YYMMDD'           TO DAYS-KDDATFMT2                       
338300          MOVE 0                  TO DAYS-KVDAYS                          
338400          MOVE SPACE              TO DAYS-TIDATE2                         
338500                                    DAYS-IDCALEND                         
338600          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
338700*                                                                         
338800          IF DAYS-KDRC = 8                                                
338900            MOVE 'FEL VID ANROP TILL WZ20DAYS W' TO FELTEXT               
339000            CALL ABEND USING RKOD                                         
339100          ELSE                                                            
339200            MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                      
339300                                      ARB-TIAAMMDD-AVS                    
339400            MOVE W-TILEVDAG (IX-DAG) TO LINK3-TILEVDAG                    
339500                                        WOL-TILEVDAG                      
339600                                                                          
339700            MOVE W-TIAAMMDD-AVS   TO DAYS-TIDATE1                         
339800            MOVE 'YYMMDD'         TO DAYS-KDDATFMT1                       
339900            MOVE 'YYWW'           TO DAYS-KDDATFMT2                       
340000            MOVE 0                TO DAYS-KVDAYS                          
340100            MOVE SPACE            TO DAYS-TIDATE2                         
340200                                      DAYS-IDCALEND                       
340300            CALL WZ20DAYS USING DAYS-WZ20DAYS                             
340400*                                                                         
340500            IF DAYS-KDRC = 8                                              
340600              MOVE 'FEL VID ANROP TILL WZ20DAYS L' TO FELTEXT             
340700              CALL ABEND USING RKOD                                       
340800            ELSE                                                          
340900              MOVE DAYS-TIDATE2(1:4) TO WOL-TIAAVV-AVS                    
341000*WDATKONV     MOVE DAT-TIAAVV-GRP TO WOL-TIAAVV-AVS                       
341100            END-IF                                                        
341200          END-IF                                                          
341300*FIX NYÅR START   DATKONVJUST 2014                                        
341400          COMPUTE FIX-AAVVD = 10 * LINK3-TIAVROP-AVS +                    
341500                                   W-TILEVDAG (IX-DAG)                    
341600          IF FIX-AAVVD = 15011                                            
341700             MOVE 141229   TO W-TIAAMMDD-AVS                              
341800                               ARB-TIAAMMDD-AVS                           
341900             MOVE 1         TO WOL-TILEVDAG                               
342000                               LINK3-TILEVDAG                             
342100             MOVE 1501      TO WOL-TIAAVV-AVS                             
342200          END-IF                                                          
342300          IF FIX-AAVVD = 15012                                            
342400             MOVE 141230   TO W-TIAAMMDD-AVS                              
342500                               ARB-TIAAMMDD-AVS                           
342600             MOVE 2         TO WOL-TILEVDAG                               
342700                               LINK3-TILEVDAG                             
342800             MOVE 1501      TO WOL-TIAAVV-AVS                             
342900          END-IF                                                          
343000          IF FIX-AAVVD = 15013                                            
343100             MOVE 141231   TO W-TIAAMMDD-AVS                              
343200                               ARB-TIAAMMDD-AVS                           
343300             MOVE 3         TO WOL-TILEVDAG                               
343400                               LINK3-TILEVDAG                             
343500             MOVE 1501      TO WOL-TIAAVV-AVS                             
343600          END-IF                                                          
343700*FIX NYÅR END  DATKONVJUST 2014                                           
343800                                                                          
343900**    LÄS HELG-TAB IDLANDX2 DATUM                                         
344000**      OM TRÄFF JUSTERA DATUM                                            
344100                                                                          
344200        PERFORM GEBD-KOLL-HELGDAG                                         
344300*                                                                         
344400*INL                                                                      
344500          MOVE 2                   TO WORK-KDCALL                         
344600          MOVE WC-CDC-SE           TO WORK-IDDC                           
344700          MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                   
344800          MOVE LINK-KVDAGAR-TT     TO WORK-KVWORKD                        
344900          ADD +1                   TO WORK-KVWORKD                        
345000          CALL WORKDAY USING  WORK-KDCALL                                 
345100               WORK-DATE-AREA WORK-KDSVAR                                 
345200          MOVE WORK-TIAAMMDD-TOM   TO LINK3-TIAVRDAT-INL                  
345300*DISP                                                                     
345400          MOVE 2                   TO WORK-KDCALL                         
345500          MOVE WC-CDC-SE           TO WORK-IDDC                           
345600          MOVE LINK3-TIAVRDAT-INL  TO WORK-TIAAMMDD-FOM                   
345700          MOVE LINK-KVDAGAR-INLEV  TO WORK-KVWORKD                        
345800          ADD +1                   TO WORK-KVWORKD                        
345900          CALL WORKDAY USING  WORK-KDCALL                                 
346000               WORK-DATE-AREA WORK-KDSVAR                                 
346100          MOVE WORK-TIAAMMDD-TOM   TO LINK3-TIAVRDAT-DISP                 
346200                                                                          
346300          MOVE LINK3-TIAVROP-AVS TO W-TIAVROP-AVS                         
346400          IF W-TIAVROP-AVS > 5000                                         
346500             MOVE 19             TO W-TIAVROP-AVS-SEKEL                   
346600          ELSE                                                            
346700             MOVE 20             TO W-TIAVROP-AVS-SEKEL                   
346800          END-IF                                                          
346900***       MOVE W-DAAVROP-AVS     TO LINK3-DAAVROP-AVS                     
347000                                                                          
347100         IF WS-FLAGGA-REDUC = JA                                          
347200***         LÄGG DET REDUCERADE KVAVROP PÅ 1:A DAGEN                      
347300            MOVE 5 TO IX-DAG                                              
347400         ELSE                                                             
347500            MOVE W-KVAVROP (IX-DAG) TO LINK3-KVAVROP                      
347600         END-IF                                                           
347700                                                                          
347800**-HÄR KOLLAR MAN OM AVROPSVECKAN ÄR BLOCKAD FÖR IDLEVNR-SHIP             
347900**-OCH SKALL FLYTTAS VIA SUB-PGM W221BLOC.                                
348000                                                                          
348100         IF LINK3-KVAVROP > ZERO                                          
348200           PERFORM GEBB-KOLL-BLOCKAD-VECKA                                
348300         END-IF                                                           
348400                                                                          
348500                                                                          
348600         IF SW-BLOCKAD-VECKA = JA                                         
348700           MOVE JA   TO BLOC-FLAGGA-DAGL-AVROP                            
348800         ELSE                                                             
348900           MOVE NYUPPL-DAG-AVROP    TO LINK3-KDCALL                       
349000           PERFORM S22A-NYUPPL-DAG-AVROP                                  
349100         END-IF                                                           
349200                                                                          
349300         MOVE SPAR-TIAVROP-AVS   TO LINK3-TIAVROP-AVS                     
349400       END-IF                                                             
349500                                                                          
349600       ADD +1 TO IX-DAG                                                   
349700     END-PERFORM                                                          
349800     .                                                                    
349900     EJECT                                                                
350000 GEBB-KOLL-BLOCKAD-VECKA  SECTION.                                        
350100     MOVE 'GEBB-KOLL-BLOCKAD-VECKA '  TO CURRENT-SECTION                  
350200                                                                          
350300     MOVE NEJ    TO SW-BLOCKAD-VECKA                                      
350400                                                                          
350500     MOVE LINK-IDLEVNR-SHIP    TO W-IDLEVNR-SHIP-2258                     
350600     MOVE LINK-IDANSK          TO W-IDANSK-2260                           
350700     MOVE LINK3-TIAVROP-AVS    TO WS-DAAVROP-AAVV                         
350800     MOVE 20                   TO WS-DAAVROP-SS                           
350900     MOVE WS-DAAVROP-AVS       TO W-DAAVROP-2260                          
351000                                                                          
351100     PERFORM IMS-GU-WDGX2258                                              
351200     IF SEGMENT-FINNS                                                     
351300                                                                          
351400       PERFORM IMS-GNP-WDGX2260                                           
351500       IF SEGMENT-FINNS                                                   
351600         MOVE JA                   TO SW-BLOCKAD-VECKA                    
351700                                                                          
351800         ADD +1                    TO BLOC-TAB-IX                         
351900         IF BLOC-TAB-IX < BLOC-MAX-IX                                     
352000           MOVE WS-DAAVROP-AVS     TO                                     
352100                                    BLOC-DAAVROP-AVS(BLOC-TAB-IX)         
352200           MOVE LINK3-TILEVDAG     TO                                     
352300                                    BLOC-TILEVDAG(BLOC-TAB-IX)            
352400           MOVE LINK3-KVAVROP      TO                                     
352500                                    BLOC-KVAVROP(BLOC-TAB-IX)             
352600           MOVE 2260-DAAVROP-FOM   TO                                     
352700                                    BLOC-DAAVROP-FOM(BLOC-TAB-IX)         
352800           MOVE 2260-DAAVROP-TOM   TO                                     
352900                                    BLOC-DAAVROP-TOM(BLOC-TAB-IX)         
353000           MOVE 2260-DAAVROP-TFOM  TO                                     
353100                                    BLOC-DAAVROP-TFOM(BLOC-TAB-IX)        
353200         END-IF                                                           
353300                                                                          
353400***      * + KDLPORS                                                      
353500         MOVE 06                   TO W-KDLPORS                           
353600         PERFORM S01-ADD-TILL-ORSAKSTABELL                                
353700       END-IF                                                             
353800     END-IF                                                               
353900     .                                                                    
354000     EJECT                                                                
354100 GEBD-KOLL-HELGDAG   SECTION.                                             
354200                                                                          
354300**    ( FINNS UNDERLIGGANDE SEGMENT MED IDLEVNR ? )                       
354400**    ( - KOMMER I SENARE RELEASE                 )                       
354500     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
354600     MOVE 20               TO W-DADATUM-HELG-SS                           
354700     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
354800                                                                          
354900     MOVE NEJ TO SW-HELG                                                  
355000     MOVE +1  TO IX-HELG                                                  
355100     PERFORM UNTIL IX-HELG > ANT-HELG                                     
355200        IF W-IDLANDX2 = TAB-IDLANDX2 (IX-HELG) AND                        
355300           W-DADATUM-HELG = TAB-DADATUM-HELG (IX-HELG)                    
355400           MOVE JA TO SW-HELG                                             
355500           ADD ANT-HELG TO IX-HELG                                        
355600        ELSE                                                              
355700           IF W-IDLANDX2 < TAB-IDLANDX2 (IX-HELG)                         
355800              MOVE ANT-HELG TO IX-HELG                                    
355900           END-IF                                                         
356000        END-IF                                                            
356100        ADD +1 TO IX-HELG                                                 
356200     END-PERFORM                                                          
356300                                                                          
356400     IF SW-HELG = JA                                                      
356500                                                                          
356600        PERFORM GEBDA-SOEK-NY-AVS-DAG                                     
356700*WZ20DAYS                                                                 
356800        MOVE ARB-TIAAMMDD-AVS        TO DAYS-TIDATE1                      
356900                                        W-TIAAMMDD-AVS                    
357000        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
357100        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
357200        MOVE 0                       TO DAYS-KVDAYS                       
357300        MOVE SPACE                   TO DAYS-TIDATE2                      
357400                                           DAYS-IDCALEND                  
357500        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
357600*                                                                         
357700        IF DAYS-KDRC = 8                                                  
357800          MOVE 'FEL VID ANROP TILL WZ20DAYS K' TO FELTEXT                 
357900          CALL ABEND USING RKOD                                           
358000        ELSE                                                              
358100          MOVE DAYS-TIDATE2(1:2)   TO WS-DAYS-TIDATE-AA                   
358200          MOVE DAYS-TIDATE2(3:2)   TO WS-DAYS-TIDATE-VV                   
358300          COMPUTE LINK3-TIAVROP-AVS =                                     
358400                  WS-DAYS-TIDATE-AA * 100 +                               
358500                  WS-DAYS-TIDATE-VV                                       
358600          MOVE DAYS-TIDATE2(5:1)   TO LINK3-TILEVDAG                      
358700        END-IF                                                            
358800*FIX NYÅR START DATKONVJUST 2014                                          
358900        IF ARB-TIAAMMDD-AVS = 141229                                      
359000           MOVE 1501   TO LINK3-TIAVROP-AVS                               
359100           MOVE 1      TO LINK3-TILEVDAG                                  
359200        END-IF                                                            
359300        IF ARB-TIAAMMDD-AVS = 141230                                      
359400           MOVE 1501   TO LINK3-TIAVROP-AVS                               
359500           MOVE 2      TO LINK3-TILEVDAG                                  
359600        END-IF                                                            
359700        IF ARB-TIAAMMDD-AVS = 141231                                      
359800           MOVE 1501   TO LINK3-TIAVROP-AVS                               
359900           MOVE 3      TO LINK3-TILEVDAG                                  
360000        END-IF                                                            
360100*FIX NYÅR END DATKONVJUST 2014                                            
360200     END-IF                                                               
360300                                                                          
360400     .                                                                    
360500     EJECT                                                                
360600 GEBDA-SOEK-NY-AVS-DAG   SECTION.                                         
360700                                                                          
360800*    ITERERA                                                              
360900*       -7 DAGAR                                                          
361000*       UTANFÖR FRYSTID ?                                                 
361100*       HELGDAG ?                                                         
361200     MOVE NEJ                 TO SW-OK SW-FRYS                            
361300     PERFORM UNTIL SW-OK = JA OR SW-FRYS = JA                             
361400        MOVE 003              TO DAG-KDCALL                               
361500        MOVE 20               TO DAG-TISEKEL-TOM                          
361600        MOVE ARB-TIAAMMDD-AVS TO DAG-TIAAMMDD-TOM                         
361700        MOVE 8                TO DAG-KVKALDAG                             
361800        CALL WDAGKONV   USING DAG-KDCALL,                                 
361900                              DAG-DATUM-AREA,                             
362000                              DAG-KDSVAR                                  
362100        IF DAG-KDSVAR = SPACE                                             
362200           MOVE DAG-TIAAMMDD-FOM TO ARB-TIAAMMDD-AVS                      
362300           IF ARB-TIAAMMDD-AVS >  WS-FRYSTID  AND                         
362400              ARB-TIAAMMDD-AVS >= WS-TIAAMMDD-SPECST                      
362500              MOVE JA         TO SW-OK                                    
362600              PERFORM GEBDAA-KOLL-HELG                                    
362700           ELSE                                                           
362800              MOVE JA         TO SW-FRYS                                  
362900           END-IF                                                         
363000        ELSE                                                              
363100           MOVE 'FEL VID ANROP TILL DAGKONV 1'                            
363200                              TO FELTEXT                                  
363300           CALL FELLOG                                                    
363400        END-IF                                                            
363500     END-PERFORM                                                          
363600                                                                          
363700     IF SW-FRYS = JA                                                      
363800*       OM EJ OK: FINNS ANNAN DAG URSPRUNGLIG VECKA ?                     
363900                                                                          
364000        PERFORM GEBDB-SOEK-ANNAN-DAG                                      
364100                                                                          
364200*       OM EJ OK: NÄSTA MÖJLIGA TILLFÄLLE                                 
364300     END-IF                                                               
364400                                                                          
364500     MOVE ARB-TIAAMMDD-AVS  TO W-TIAAMMDD-AVS                             
364600     .                                                                    
364700     EJECT                                                                
364800 GEBDAA-KOLL-HELG   SECTION.                                              
364900                                                                          
365000*                                                                         
365100     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
365200     MOVE 20               TO W-DADATUM-HELG-SS                           
365300     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
365400     MOVE +1  TO IX-HELG                                                  
365500     PERFORM UNTIL IX-HELG > ANT-HELG                                     
365600        IF W-IDLANDX2 = TAB-IDLANDX2 (IX-HELG) AND                        
365700           W-DADATUM-HELG = TAB-DADATUM-HELG (IX-HELG)                    
365800**            SÖK NY AVS-DAG                                              
365900           MOVE NEJ     TO SW-OK                                          
366000           ADD ANT-HELG TO IX-HELG                                        
366100        ELSE                                                              
366200           IF W-IDLANDX2 < TAB-IDLANDX2 (IX-HELG)                         
366300              MOVE ANT-HELG TO IX-HELG                                    
366400           END-IF                                                         
366500        END-IF                                                            
366600        ADD +1 TO IX-HELG                                                 
366700     END-PERFORM                                                          
366800     .                                                                    
366900     EJECT                                                                
367000 GEBDB-SOEK-ANNAN-DAG SECTION.                                            
367100                                                                          
367200*                                                                         
367300*    FINNS WDK611-TILEVDAG NOT = WOL-TILEVDAG                             
367400*ALTERNATIVT                                                              
367500*    FINNS WDF1-TILEVDAG   NOT = WOL-TILEVDAG                             
367600                                                                          
367700     MOVE ZERO TO ARB-TILEVDAG (1)                                        
367800                  ARB-TILEVDAG (2)                                        
367900                  ARB-TILEVDAG (3)                                        
368000                  ARB-TILEVDAG (4)                                        
368100                  ARB-TILEVDAG (5)                                        
368200     IF LINK-TILEVDAG (1) > ZERO OR                                       
368300        LINK-TILEVDAG (2) > ZERO OR                                       
368400        LINK-TILEVDAG (3) > ZERO OR                                       
368500        LINK-TILEVDAG (4) > ZERO OR                                       
368600        LINK-TILEVDAG (5) > ZERO                                          
368700        MOVE +1              TO IX-DG                                     
368800        PERFORM UNTIL IX-DG > 5                                           
368900           IF LINK-TILEVDAG (IX-DG) > ZERO AND                            
369000              LINK-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                    
369100              MOVE LINK-TILEVDAG (IX-DG)                                  
369200                             TO ARB-TILEVDAG (IX-DG)                      
369300           END-IF                                                         
369400           ADD +1            TO IX-DG                                     
369500        END-PERFORM                                                       
369600     ELSE                                                                 
369700        MOVE +1              TO IX-DG                                     
369800        PERFORM UNTIL IX-DG > 5                                           
369900           IF F1-LEV-TILEVDAG (IX-DG) > ZERO AND                          
370000              F1-LEV-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                  
370100              MOVE F1-LEV-TILEVDAG (IX-DG)                                
370200                             TO ARB-TILEVDAG (IX-DG)                      
370300           END-IF                                                         
370400           ADD +1            TO IX-DG                                     
370500        END-PERFORM                                                       
370600     END-IF                                                               
370700                                                                          
370800     MOVE +1                 TO IX-DG                                     
370900     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
371000        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
371100           PERFORM GEBDBA-KOLL-NYTT-DATUM                                 
371200        END-IF                                                            
371300        ADD +1               TO IX-DG                                     
371400     END-PERFORM                                                          
371500                                                                          
371600*    ANNARS FÖRSTA LEVDAG NÄSTA VECKA OSV                                 
371700     MOVE WOL-TILEVDAG TO ARB-TILEVDAG (WOL-TILEVDAG)                     
371800     PERFORM UNTIL SW-OK = JA                                             
371900*      STEGA FRAMÅT                                                       
372000       PERFORM GEBDBB-KOLL-NASTA-DATUM                                    
372100     END-PERFORM                                                          
372200     .                                                                    
372300     EJECT                                                                
372400 GEBDBA-KOLL-NYTT-DATUM SECTION.                                          
372500                                                                          
372600*    OM OK => SW-OK = JA                                                  
372700*    KOLL AV ANNAN DAG I URSPRUNGLIG AVSÄNDNINGSVECKA                     
372800                                                                          
372900*WZ20DAYS                                                                 
373000        MOVE 0                    TO WS-DAYS-TIAAVVD                      
373100        COMPUTE WS-DAYS-TIAAVVD = 10 *  WOL-TIAAVV-AVS +                  
373200                                  ARB-TILEVDAG (IX-DG)                    
373300        MOVE WS-DAYS-TIAAVVD      TO DAYS-TIDATE1                         
373400        MOVE 'YYWWD'              TO DAYS-KDDATFMT1                       
373500        MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                       
373600        MOVE 0                    TO DAYS-KVDAYS                          
373700        MOVE SPACE                TO DAYS-TIDATE2                         
373800                                  DAYS-IDCALEND                           
373900        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
374000*                                                                         
374100        IF DAYS-KDRC = 8                                                  
374200          MOVE 'FEL VID ANROP TILL WZ20DAYS M' TO FELTEXT                 
374300          CALL ABEND USING RKOD                                           
374400        ELSE                                                              
374500          MOVE DAYS-TIDATE2(1:6) TO ARB-TIAAMMDD-AVS                      
374600        END-IF                                                            
374700*FIX NYÅR START  DATKONVJUST 2014                                         
374800     COMPUTE FIX-AAVVD = 10 * WOL-TIAAVV-AVS +                            
374900                              ARB-TILEVDAG (IX-DG)                        
375000     IF FIX-AAVVD = 15011                                                 
375100        MOVE 141229    TO ARB-TIAAMMDD-AVS                                
375200     END-IF                                                               
375300     IF FIX-AAVVD = 15012                                                 
375400        MOVE 141230    TO ARB-TIAAMMDD-AVS                                
375500     END-IF                                                               
375600     IF FIX-AAVVD = 15013                                                 
375700        MOVE 141231    TO ARB-TIAAMMDD-AVS                                
375800     END-IF                                                               
375900*FIX NYÅR END  DATKONVJUST 2014                                           
376000                                                                          
376100     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
376200     MOVE 20               TO W-DADATUM-HELG-SS                           
376300     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
376400     MOVE +1  TO IX-HELG                                                  
376500     PERFORM UNTIL IX-HELG > ANT-HELG                                     
376600        IF W-IDLANDX2 = TAB-IDLANDX2 (IX-HELG) AND                        
376700           W-DADATUM-HELG = TAB-DADATUM-HELG (IX-HELG)                    
376800**            SÖK NY AVS-DAG                                              
376900           MOVE NEJ     TO SW-OK                                          
377000           ADD ANT-HELG TO IX-HELG                                        
377100        ELSE                                                              
377200           IF W-IDLANDX2 < TAB-IDLANDX2 (IX-HELG) OR                      
377300              IX-HELG = ANT-HELG                                          
377400              MOVE JA       TO SW-OK                                      
377500              MOVE ANT-HELG TO IX-HELG                                    
377600           END-IF                                                         
377700        END-IF                                                            
377800        ADD +1 TO IX-HELG                                                 
377900     END-PERFORM                                                          
378000     .                                                                    
378100     EJECT                                                                
378200 GEBDBB-KOLL-NASTA-DATUM SECTION.                                         
378300                                                                          
378400**      OM OK => SW-OK = JA                                               
378500**   ÖKA VECKA MED 1                                                      
378600**   SÖK AVS-DAGAR I VECKAN                                               
378700                                                                          
378800     MOVE WOL-TIAAVV-AVS  TO VADD-DATUM-AAVV                              
378900     MOVE 1               TO W-ANTAL-VECKOR                               
379000     CALL W009VADD USING VADD-DATUM-AAVV W-ANTAL-VECKOR                   
379100     MOVE VADD-DATUM-AAVV TO WOL-TIAAVV-AVS                               
379200                                                                          
379300     MOVE +1                 TO IX-DG                                     
379400     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
379500        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
379600           PERFORM GEBDBA-KOLL-NYTT-DATUM                                 
379700        END-IF                                                            
379800        ADD +1               TO IX-DG                                     
379900     END-PERFORM                                                          
380000     .                                                                    
380100     EJECT                                                                
380200 GEC-BERAEKNA-AVROPSKV-NYTT    SECTION.                                   
380300                                                                          
380400                                                                          
380500*    AVROPSKVANTITETEN SKALL VARA SKILLNADEN MELLAN TILLG                 
380600*    OCH (SÄKERHETSLAGER + -  DE BEHOV OCH TILLG                          
380700*    SOM SKER DE KOMMANDE W-Q-FREKV VECKORNA)                             
380800*    DETTA SKALL AVRUNDAS UPPÅT                                           
380900                                                                          
381000     COMPUTE W-ARBKVANT2 = W-BUFF-VV - W-TILLG                            
381100                                                                          
381200                                                                          
381300     PERFORM GECB-AVRUNDA                                                 
381400                                                                          
381500     MOVE W-ARBKVANT      TO W-AVROPSKVANTITET                            
381600                                                                          
381700     IF  W-AVROPSKVANTITET < 1                                            
381800         MOVE 1 TO W-AVROPSKVANTITET                                      
381900     END-IF                                                               
382000     .                                                                    
382100     EJECT                                                                
382200 GECA-BER-ARSOMS               SECTION.                                   
382300                                                                          
382400        COMPUTE W-ARSOMS = 12                                             
382500                * ( LINK-KVPB-SEP                                         
382600                  + LINK-KVPB-SATS                                        
382700                  + LINK-KVPB-TPO                                         
382800                  + LINK-KVPB-SDC    )                                    
382900                *   LINK-PRARTBES                                         
383000                                                                          
383100        COMPUTE W-ARSBEH = 12                                             
383200                * ( LINK-KVPB-SEP                                         
383300                  + LINK-KVPB-SATS                                        
383400                  + LINK-KVPB-TPO                                         
383500                  + LINK-KVPB-SDC    )                                    
383600     .                                                                    
383700     EJECT                                                                
383800 GECB-AVRUNDA                  SECTION.                                   
383900     MOVE 'GECB-AVRUNDA  '  TO CURRENT-SECTION                            
384000                                                                          
384100     IF  LINK-KVPALL > ZERO                                               
384200        COMPUTE W-ARBKVANT =                                              
384300                ( W-ARBKVANT2 / LINK-KVPALL ) + 0.99                      
384400        COMPUTE W-ARBKVANT = LINK-KVPALL * W-ARBKVANT                     
384500        IF W-ARBKVANT = ZERO                                              
384600           MOVE LINK-KVPALL TO W-ARBKVANT                                 
384700        END-IF                                                            
384800     ELSE                                                                 
384900        IF  LINK-KVQPACK-1 > ZERO                                         
385000            COMPUTE W-ARBKVANT =                                          
385100                    ( W-ARBKVANT2 / LINK-KVQPACK-1 ) + 0.9                
385200            COMPUTE W-ARBKVANT = LINK-KVQPACK-1 * W-ARBKVANT              
385300            IF W-ARBKVANT = ZERO                                          
385400               MOVE LINK-KVQPACK-1 TO W-ARBKVANT                          
385500            END-IF                                                        
385600        ELSE                                                              
385700            MOVE 1 TO W-M                                                 
385800            MOVE 1  TO IX-M                                               
385900            PERFORM UNTIL                                                 
386000               IX-M  > W012-MAXINDEX-1                                    
386100               OR  W-ARBKVANT2 NOT > W012-KVQ-BER-TOM (IX-M)              
386200               ADD 1 TO IX-M                                              
386300            END-PERFORM                                                   
386400            MOVE W012-KVANTAL-MULTIPEL (IX-M) TO W-M                      
386500                                                                          
386600            COMPUTE W-KVANTAL = (W-ARBKVANT2 / W-M) + 0.9                 
386700            IF  W-KVANTAL = ZERO                                          
386800               MOVE W-M TO W-ARBKVANT                                     
386900            ELSE                                                          
387000               MULTIPLY W-M BY W-KVANTAL GIVING W-ARBKVANT                
387100            END-IF                                                        
387200        END-IF                                                            
387300     END-IF                                                               
387400     .                                                                    
387500     EJECT                                                                
387600 GED-LANDKOD-FRYSTID SECTION.                                             
387700                                                                          
387800**   LÄS WDF106 GU OKVAL  ADR-IDLANDX2                                    
387900**   BERÄKNA FRYSGRÄNS  DAGENS + LINK-KVVECKOR-FT                         
388000**                                                                        
388100     MOVE SPACE               TO WS-IDLANDX2-SHIP                         
388200     MOVE LINK-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                           
388300     PERFORM IMS-GET-LEVA14-WDF106-SHIP                                   
388400     IF SEGMENT-FINNS                                                     
388500        MOVE ADR-IDLANDX2     TO WS-IDLANDX2-SHIP                         
388600     ELSE                                                                 
388700        DISPLAY 'LANDKOD SAKNAS ' W-IDLEVNR                               
388800        MOVE SPACE            TO WS-IDLANDX2-SHIP                         
388900     END-IF                                                               
389000                                                                          
389100**   LÄGG DAGENS DATUM I FRYSTID. ADDERA MED FT*7                         
389200     MOVE AKT-DATUM-AAMMDD    TO WS-FRYSTID                               
389300**   ANTAL DGR FRYSTID*7 + 1                                              
389400**   DAGKONV DD + FT                                                      
389500     MOVE 002                 TO DAG-KDCALL                               
389600     MOVE 20                  TO DAG-TISEKEL-FOM                          
389700     MOVE AKT-DATUM-AAMMDD    TO DAG-TIAAMMDD-FOM                         
389800     COMPUTE DAG-KVKALDAG = (LINK-KVVECKOR-FT * 7) + 2                    
389900     CALL WDAGKONV USING DAG-KDCALL,                                      
390000                         DAG-DATUM-AREA,                                  
390100                         DAG-KDSVAR                                       
390200     IF DAG-KDSVAR = SPACE                                                
390300        MOVE DAG-TISEKEL-TOM  TO WS-FRYSTID-SEKEL                         
390400        MOVE DAG-TIAAMMDD-TOM TO WS-FRYSTID                               
390500     ELSE                                                                 
390600        MOVE 'FEL VID ANROP TILL DAGKONV 2'                               
390700                              TO FELTEXT                                  
390800        CALL FELLOG                                                       
390900     END-IF                                                               
391000     .                                                                    
391100     EJECT                                                                
391200 GF-FLYTTA-BLOCKADE-AVROP SECTION.                                        
391300     MOVE 'GF-FLYTTA-BLOCKADE-AVROP ' TO CURRENT-SECTION                  
391400                                                                          
391500     PERFORM S100-NOLLA-W2215A-AREA                                       
391600                                                                          
391700     MOVE FLYTTA-BLOC-AVROP     TO U5A-UPLP-IDPTYP                        
391800     MOVE LINK-IDARTNR          TO U5A-UPLP-IDARTNR                       
391900     MOVE WC-CDC-SE             TO U5A-UPLP-IDDC                          
392000     MOVE LINK-IDLEVNR          TO U5A-UPLP-IDLEVNR                       
392100                                                                          
392200     MOVE BLOC-IDLEVNR-SHIP     TO U5A-UPLP-IDLEVNR-SHIP                  
392300     MOVE BLOC-IDLANDX2-SHIP    TO U5A-UPLP-IDLANDX2-SHIP                 
392400     MOVE BLOC-IDANSK           TO U5A-UPLP-IDANSK                        
392500     MOVE BLOC-KDAVROP          TO U5A-UPLP-KDAVROP                       
392600     MOVE BLOC-KVQ              TO U5A-UPLP-KVQ                           
392700     MOVE BLOC-KVPALL           TO U5A-UPLP-KVPALL                        
392800     MOVE BLOC-KVULOAD          TO U5A-UPLP-KVULOAD                       
392900     MOVE BLOC-TIAAMMDD-SPECST  TO U5A-UPLP-TIAAMMDD-SPECST               
393000     MOVE BLOC-TIAAMMDD-FT      TO U5A-UPLP-TIAAMMDD-FT                   
393100     MOVE BLOC-KVDAGAR-TT       TO U5A-UPLP-KVDAGAR-TT                    
393200     MOVE BLOC-KVDAGAR-INLEV    TO U5A-UPLP-KVDAGAR-INLEV                 
393300     MOVE BLOC-FLAGGA-DAGL-AVROP TO U5A-UPLP-FLAGGA-DAGL-AVROP            
393400                                                                          
393500     MOVE BLOC-TILEVDAG-DAGL(1)  TO U5A-UPLP-TILEVDAG-DAGL(1)             
393600     MOVE BLOC-TILEVDAG-DAGL(2)  TO U5A-UPLP-TILEVDAG-DAGL(2)             
393700     MOVE BLOC-TILEVDAG-DAGL(3)  TO U5A-UPLP-TILEVDAG-DAGL(3)             
393800     MOVE BLOC-TILEVDAG-DAGL(4)  TO U5A-UPLP-TILEVDAG-DAGL(4)             
393900     MOVE BLOC-TILEVDAG-DAGL(5)  TO U5A-UPLP-TILEVDAG-DAGL(5)             
394000                                                                          
394100     MOVE +1 TO IX                                                        
394200     PERFORM UNTIL IX > BLOC-TAB-IX                                       
394300       IF BLOC-DAAVROP-AVS(IX) NOT = ZERO                                 
394400                                                                          
394500          MOVE BLOC-DAAVROP-AVS(IX) TO                                    
394600                                    U5A-UPLP-DAAVROP-AVS                  
394700          MOVE BLOC-TILEVDAG(IX)    TO                                    
394800                                    U5A-UPLP-TILEVDAG                     
394900          MOVE BLOC-KVAVROP(IX)     TO                                    
395000                                    U5A-UPLP-KVAVROP                      
395100          MOVE BLOC-DAAVROP-FOM(IX) TO                                    
395200                                    U5A-UPLP-DAAVROP-FOM                  
395300          MOVE BLOC-DAAVROP-TOM(IX) TO                                    
395400                                    U5A-UPLP-DAAVROP-TOM                  
395500          MOVE BLOC-DAAVROP-TFOM(IX) TO                                   
395600                                    U5A-UPLP-DAAVROP-TFOM                 
395700                                                                          
395800          PERFORM S10-SKRIV-W2215A                                        
395900       END-IF                                                             
396000       ADD +1 TO IX                                                       
396100     END-PERFORM                                                          
396200     .                                                                    
396300     EJECT                                                                
396400 H-KONTROLL-UPPD-AV-REGISTER SECTION.                                     
396500******************************************************************        
396600*                                                                *        
396700*                                                                *        
396800*                                                                *        
396900*                                                                *        
397000******************************************************************        
397100     SKIP1                                                                
397200     IF  CLAG-KDLPSP NOT = LINK-KDLPSP                                    
397300     OR  CLAG-TIOMSPEC NOT = LINK-TIOMSPEC                                
397400     OR  CLAG-TILPSP NOT = LINK-TILPSP                                    
397500     OR  CLAG-KDLEVPLF NOT = LINK-KDLEVPLF                                
397600     OR  CLAG-FLSKROT-WLC  NOT = LINK-FLSKROT-WLC                         
397700*---     MOVE UPPDAT-MATINFO TO LINK-KDCALL                               
397800         PERFORM HA-UPPDAT-MATINFO                                        
397900     END-IF                                                               
398000     SKIP1                                                                
398100     IF SW-OMSPEC-UTFOERD = JA                                            
398200       IF  WSPAR-D904-TISPECST NOT = LINK-TISPECST                        
398300*---       MOVE BORTTAG-OMSPEC TO LINK-KDCALL                             
398400           PERFORM S20-BORTTAG-OMSPEC                                     
398500                                                                          
398600           MOVE NYUPPL-OMSPEC TO LINK-KDCALL                              
398700           PERFORM S23-NYUPPL-OMSPEC                                      
398800                                                                          
398900       ELSE                                                               
399000         IF  WSPAR-D904-KDLPORS-TAB (1) NOT = LINK-KDLPORS-TAB (1)        
399100         OR  WSPAR-D904-KDLPORS-TAB (2) NOT = LINK-KDLPORS-TAB (2)        
399200         OR  WSPAR-D904-KDLPORS-TAB (3) NOT = LINK-KDLPORS-TAB (3)        
399300         OR  WSPAR-D904-KDPLKOEP NOT = LINK-KDPLKOEP                      
399400         OR  WSPAR-D904-KVBEST-PL NOT = LINK-KVBEST-PL                    
399500             MOVE UPPDAT-OMSPEC TO LINK-KDCALL                            
399600             PERFORM HB-UPPDAT-OMSPEC                                     
399700                                                                          
399800            IF  LINK-ANROP-FEL                                            
399900                MOVE NYUPPL-OMSPEC TO LINK-KDCALL                         
400000                PERFORM S23-NYUPPL-OMSPEC                                 
400100            END-IF                                                        
400200         END-IF                                                           
400300       END-IF                                                             
400400     ELSE                                                                 
400500         MOVE BORTTAG-OMSPEC TO LINK-KDCALL                               
400600         PERFORM S20-BORTTAG-OMSPEC                                       
400700                                                                          
400800     END-IF                                                               
400900     .                                                                    
401000     EJECT                                                                
401100 HA-UPPDAT-MATINFO  SECTION.                                              
401200     MOVE 'HA-UPPDAT-MATINFO-WDK611 ' TO CURRENT-SECTION                  
401300                                                                          
401400*--- D-UPPDAT-MATINFO SECTION I PGM W2215010                              
401500                                                                          
401600     PERFORM S100-NOLLA-W2215A-AREA                                       
401700                                                                          
401800     MOVE UPDATE-WDK611-5A TO U5A-UPLP-IDPTYP                             
401900     MOVE LINK-IDARTNR     TO U5A-UPLP-IDARTNR                            
402000     MOVE WC-CDC-SE        TO U5A-UPLP-IDDC                               
402100                                                                          
402200     MOVE LINK-KDLPSP      TO U5A-UPLP-KDLPSP                             
402300     MOVE LINK-TIOMSPEC    TO U5A-UPLP-TIOMSPEC                           
402400     MOVE LINK-TILPSP      TO U5A-UPLP-TILPSP                             
402500     MOVE LINK-KDLEVPLF    TO U5A-UPLP-KDLEVPLF                           
402600     MOVE LINK-FLSKROT-WLC TO U5A-UPLP-FLSKROT-WLC                        
402700                                                                          
402800     PERFORM S10-SKRIV-W2215A                                             
402900     .                                                                    
403000     EJECT                                                                
403100 HB-UPPDAT-OMSPEC  SECTION.                                               
403200     MOVE 'HB-UPPDAT-OMSPEC '  TO CURRENT-SECTION                         
403300                                                                          
403400*--- E-UPPDAT-OMSPEC SECTION I PGM W2215010                               
403500                                                                          
403600     MOVE LINK-IDARTNR TO W-IDARTNR-D9                                    
403700     MOVE WC-CDC-SE    TO W-IDDC-D9                                       
403800     MOVE LINK-IDLEVNR TO W-IDLEVNR                                       
403900                                                                          
404000     PERFORM IMS-GU-LEVERANTOER-D902                                      
404100     IF SEGMENT-FINNS                                                     
404200        PERFORM S100-NOLLA-W2215A-AREA                                    
404300                                                                          
404400        MOVE UPPDAT-OMSPEC-5A  TO U5A-UPLP-IDPTYP                         
404500        MOVE LINK-IDARTNR      TO U5A-UPLP-IDARTNR                        
404600        MOVE WC-CDC-SE         TO U5A-UPLP-IDDC                           
404700        MOVE LINK-IDLEVNR      TO U5A-UPLP-IDLEVNR                        
404800                                                                          
404900        MOVE LINK-TISPECST TO WS-DASPECST-AAVV                            
405000        IF WS-DASPECST-AAVV > 5000                                        
405100           MOVE 19         TO WS-DASPECST-SS                              
405200        ELSE                                                              
405300           MOVE 20         TO WS-DASPECST-SS                              
405400        END-IF                                                            
405500        MOVE WS-DASPECST   TO U5A-UPLP-DASPECST                           
405600                                                                          
405700        MOVE LINK-KDLPORS-TAB (1) TO U5A-UPLP-KDLPORS-TAB (1)             
405800        MOVE LINK-KDLPORS-TAB (2) TO U5A-UPLP-KDLPORS-TAB (2)             
405900        MOVE LINK-KDLPORS-TAB (3) TO U5A-UPLP-KDLPORS-TAB (3)             
406000        MOVE LINK-KDPLKOEP        TO U5A-UPLP-KDPLKOEP                    
406100        MOVE LINK-KVBEST-PL       TO U5A-UPLP-KVBEST-PL                   
406200                                                                          
406300        PERFORM S10-SKRIV-W2215A                                          
406400        MOVE JA TO LINK-FLJANEJ-ANROP                                     
406500     ELSE                                                                 
406600        MOVE NEJ TO LINK-FLJANEJ-ANROP                                    
406700     END-IF                                                               
406800     .                                                                    
406900     EJECT                                                                
407000 I-LAES-LEVERANTOER-DATA SECTION.                                         
407100     MOVE 'I-LAES-LEVERANTOER-DATA '  TO CURRENT-SECTION                  
407200                                                                          
407300*--- B-LAES-LEVERANTOER-DATA SECTION I PGM W2215010                       
407400                                                                          
407500     MOVE NEJ TO SW-WDD902-NYUPPL                                         
407600                                                                          
407700     MOVE ZERO TO WSPAR-D904-TISPECST                                     
407800                  WSPAR-D904-KDLPORS-TAB (1)                              
407900                  WSPAR-D904-KDLPORS-TAB (2)                              
408000                  WSPAR-D904-KDLPORS-TAB (3)                              
408100                  WSPAR-D904-KVBEST-PL                                    
408200                  WSPAR-D904-KDPLKOEP                                     
408300                                                                          
408400     MOVE LINK-IDARTNR TO W-IDARTNR-D9                                    
408500     MOVE WC-CDC-SE    TO W-IDDC-D9                                       
408600     MOVE LINK-IDLEVNR TO W-IDLEVNR                                       
408700     PERFORM IMS-GU-LEV-ROT-WDD901                                        
408800     IF SEGMENT-SAKNAS                                                    
408900         MOVE LEVSEGMENT-SAKNAS TO LINK-FLJANEJ-ANROP                     
409000         MOVE ZERO        TO LINK-KVBR                                    
409100                             LINK-TISPECST                                
409200                             LINK-KDLPORS-TAB (1)                         
409300                             LINK-KDLPORS-TAB (2)                         
409400                             LINK-KDLPORS-TAB (3)                         
409500                             LINK-KVBEST-PL                               
409600                             LINK-KDPLKOEP                                
409700     ELSE                                                                 
409800         PERFORM IMS-GNP-LEVERANTOER-WDD902                               
409900         IF SEGMENT-SAKNAS                                                
410000             MOVE LEVSEGMENT-SAKNAS TO LINK-FLJANEJ-ANROP                 
410100             MOVE ZERO    TO LINK-KVBR                                    
410200                                 LINK-TISPECST                            
410300                                 LINK-KDLPORS-TAB (1)                     
410400                                 LINK-KDLPORS-TAB (2)                     
410500                                 LINK-KDLPORS-TAB (3)                     
410600                                 LINK-KVBEST-PL                           
410700                                 LINK-KDPLKOEP                            
410800         ELSE                                                             
410900             MOVE JA TO LINK-FLJANEJ-ANROP                                
411000             MOVE D9LEV-KVBR TO LINK-KVBR                                 
411100         SKIP1                                                            
411200             PERFORM IMS-GNP-OMSPEC-WDD904                                
411300             IF SEGMENT-FINNS                                             
411400               MOVE OMSPEC-DASPECST  TO WS-DASPECST                       
411500               MOVE WS-DASPECST-AAVV TO LINK-TISPECST                     
411600                                        WSPAR-D904-TISPECST               
411700               MOVE OMSPEC-KDLPORS-TAB (1)                                
411800                                         TO LINK-KDLPORS-TAB (1)          
411900                                      WSPAR-D904-KDLPORS-TAB (1)          
412000               MOVE OMSPEC-KDLPORS-TAB (2)                                
412100                                         TO LINK-KDLPORS-TAB (2)          
412200                                      WSPAR-D904-KDLPORS-TAB (2)          
412300               MOVE OMSPEC-KDLPORS-TAB (3)                                
412400                                         TO LINK-KDLPORS-TAB (3)          
412500                                      WSPAR-D904-KDLPORS-TAB (3)          
412600               MOVE OMSPEC-KVBEST-PL TO LINK-KVBEST-PL                    
412700                                        WSPAR-D904-KVBEST-PL              
412800               MOVE OMSPEC-KDPLKOEP  TO LINK-KDPLKOEP                     
412900                                        WSPAR-D904-KDPLKOEP               
413000             ELSE                                                         
413100               MOVE ZERO TO LINK-TISPECST                                 
413200                            LINK-KDLPORS-TAB (1)                          
413300                            LINK-KDLPORS-TAB (2)                          
413400                            LINK-KDLPORS-TAB (3)                          
413500                            LINK-KVBEST-PL                                
413600                            LINK-KDPLKOEP                                 
413700             END-IF                                                       
413800         END-IF                                                           
413900     END-IF                                                               
414000     .                                                                    
414100     EJECT                                                                
414200 J-NYUPPL-LEVERANTOER SECTION.                                            
414300     MOVE 'J-NYUPPL-LEVERANTOER  '  TO CURRENT-SECTION                    
414400                                                                          
414500*** C-NYUPPL-LEVERANTOER SECTION I PGM W2215010.                          
414600                                                                          
414700     PERFORM S100-NOLLA-W2215A-AREA                                       
414800                                                                          
414900     MOVE NYUPPL-LEV-5A  TO U5A-UPLP-IDPTYP                               
415000     MOVE LINK-IDARTNR   TO U5A-UPLP-IDARTNR                              
415100     MOVE WC-CDC-SE      TO U5A-UPLP-IDDC                                 
415200     MOVE LINK-IDLEVNR   TO U5A-UPLP-IDLEVNR                              
415300                                                                          
415400*-- FLYTTAS VID UPPDAT I W2215A00                                         
415500*--  MOVE ZERO TO LEV-KVBR                                                
415600*--               LEV-TILEVPL                                             
415700                                                                          
415800     PERFORM S10-SKRIV-W2215A                                             
415900                                                                          
416000     IF SW-WDD902-NYUPPL = NEJ                                            
416100       MOVE JA TO SW-WDD902-NYUPPL                                        
416200     END-IF                                                               
416300     .                                                                    
416400     EJECT                                                                
416500 K-KONTROLL-AUT-PLAN SECTION.                                             
416600     SKIP3                                                                
416700     MOVE JA              TO SW-AUT-PLAN                                  
416800     MOVE SPACE           TO U51KONC-TELPORS                              
416900     MOVE SPACE           TO U59-LPF-TELPORSX                             
417000                                                                          
417100     MOVE +3              TO IX-ORS                                       
417200     PERFORM UNTIL IX-ORS < 1                                             
417300        IF I42BEG-KDLPORS-TAB (IX-ORS)                                    
417400        = 09 OR 11 OR 17 OR 18                                            
417500                                                                          
417600          MOVE 'EJ AUT'        TO POSTSUM-FDNAMN                          
417700          MOVE 'ORS.KOD '      TO POSTSUM-DDNAMN2                         
417800                                 U51KONC-TELPORS                          
417900          IF I42BEG-KDLPORS-TAB (IX-ORS) = 09                             
418000             MOVE 'INLÄ'       TO POSTSUM-TRANSTYP                        
418100                                  U51KONC-TELPORS(10:4)                   
418200             MOVE 'NEW PUBW'   TO U59-LPF-TELPORSX                        
418300          ELSE                                                            
418400             IF I42BEG-KDLPORS-TAB (IX-ORS) = 11                          
418500                MOVE 'LEVB'    TO POSTSUM-TRANSTYP                        
418600                                  U51KONC-TELPORS(10:4)                   
418700                MOVE 'NEW SUPPL' TO U59-LPF-TELPORSX                      
418800             ELSE                                                         
418900                IF I42BEG-KDLPORS-TAB (IX-ORS) = 17                       
419000                   MOVE 'OMSP' TO POSTSUM-TRANSTYP                        
419100                                  U51KONC-TELPORS(10:4)                   
419200                   MOVE 'REQUESTED' TO U59-LPF-TELPORSX                   
419300                ELSE                                                      
419400*                                                = 18                     
419500                   MOVE 'OPTI' TO POSTSUM-TRANSTYP                        
419600                                  U51KONC-TELPORS(10:4)                   
419700                  MOVE 'OPTIMAL' TO U59-LPF-TELPORSX                      
419800                END-IF                                                    
419900             END-IF                                                       
420000          END-IF                                                          
420100          MOVE NEJ TO SW-AUT-PLAN                                         
420200        END-IF                                                            
420300        SUBTRACT 1 FROM IX-ORS                                            
420400     END-PERFORM                                                          
420500     IF SW-AUT-PLAN = NEJ                                                 
420600        CALL POSTSUM USING POSTSUM-PARM                                   
420700     END-IF                                                               
420800                                                                          
420900     MOVE +3              TO IX-ORS                                       
421000     PERFORM UNTIL IX-ORS < 1                                             
421100        IF I42BEG-KDLPORS-TAB (IX-ORS) = 14                               
421200           IF SW-AUT-PLAN = JA                                            
421300              MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                       
421400              MOVE 'ORS.KOD '     TO POSTSUM-DDNAMN2                      
421500                                     U51KONC-TELPORS                      
421600              MOVE 'NYSÄ'         TO POSTSUM-TRANSTYP                     
421700                                     U51KONC-TELPORS(10:4)                
421800              MOVE 'NEW SEASON' TO U59-LPF-TELPORSX                       
421900              CALL POSTSUM USING POSTSUM-PARM                             
422000           END-IF                                                         
422100           MOVE NEJ TO SW-AUT-PLAN                                        
422200        END-IF                                                            
422300        SUBTRACT 1 FROM IX-ORS                                            
422400     END-PERFORM                                                          
422500                                                                          
422600     MOVE +3              TO IX-ORS                                       
422700     PERFORM UNTIL IX-ORS < 1                                             
422800        IF I42BEG-KDLPORS-TAB (IX-ORS) = 15                               
422900           IF SW-AUT-PLAN = JA                                            
423000              MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                       
423100              MOVE 'ORS.KOD '     TO POSTSUM-DDNAMN2                      
423200                                     U51KONC-TELPORS                      
423300              MOVE 'ANBY'         TO POSTSUM-TRANSTYP                     
423400                                     U51KONC-TELPORS(10:4)                
423500              MOVE 'NEW PROC'   TO U59-LPF-TELPORSX                       
423600              CALL POSTSUM USING POSTSUM-PARM                             
423700           END-IF                                                         
423800           MOVE NEJ TO SW-AUT-PLAN                                        
423900        END-IF                                                            
424000        SUBTRACT 1 FROM IX-ORS                                            
424100     END-PERFORM                                                          
424200                                                                          
424300     IF LINK-KDERS (1) > ZERO                                             
424400     Or LINK-KDUART    = 'S'                                              
424500     Or LINK-KDUART    = 'P'                                              
424600     Or LINK-KDUART    = 'A'                                              
424700     Or LINK-KDUART    = 'M'                                              
424800        IF SW-AUT-PLAN = JA                                               
424900           MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                          
425000           MOVE 'ORS.KOD '     TO POSTSUM-DDNAMN2                         
425100                                  U51KONC-TELPORS                         
425200           MOVE SPACE          TO POSTSUM-TRANSTYP                        
425300                                  U51KONC-TELPORS(10:4)                   
425400           MOVE 'REASONCODE' TO U59-LPF-TELPORSX                          
425500           IF LINK-KDUART = 'S' OR LINK-KDUART = 'P' OR                   
425600              LINK-KDUART = 'M' OR LINK-KDUART = 'A'                      
425700              MOVE LINK-KDUART TO POSTSUM-TRANSTYP                        
425800                                  U51KONC-TELPORS(10:4)                   
425900              MOVE LINK-KDUART  TO U59-LPF-TELPORSX(1:1)                  
426000              MOVE '-EXC.PART'  TO U59-LPF-TELPORSX(2:9)                  
426100           END-IF                                                         
426200           IF LINK-KDERS (1) > ZERO                                       
426300              MOVE 'ERS.KOD '     TO POSTSUM-DDNAMN2                      
426400                                     U51KONC-TELPORS                      
426500              MOVE LINK-KDERS (1) TO W-KDERS                              
426600              MOVE '> 0'          TO POSTSUM-TRANSTYP                     
426700                                     U51KONC-TELPORS(10:4)                
426800              MOVE 'SS CDC > 0'   TO U59-LPF-TELPORSX                     
426900           END-IF                                                         
427000           CALL POSTSUM USING POSTSUM-PARM                                
427100        END-IF                                                            
427200        MOVE NEJ TO SW-AUT-PLAN                                           
427300     END-IF                                                               
427400                                                                          
427500     COMPUTE W-KOP ROUNDED =                                              
427600             LINK-KVBK + LINK-KVPB-TOT (1)                                
427700     MOVE LINK-KDPRODSL          TO TEST-KDPRODSL                         
427800     IF (I42BEG-KVBEST-PL > W-KOP) AND                                    
427900         GOOD-KDPRODSL                                                    
428000                 IF SW-AUT-PLAN = JA                                      
428100                    MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                 
428200                    MOVE 'STORT   '     TO POSTSUM-DDNAMN2                
428300                                           U51KONC-TELPORS                
428400                    MOVE 'KÖP '         TO POSTSUM-TRANSTYP               
428500                                           U51KONC-TELPORS(10:4)          
428600                    MOVE 'LARGE PURC'   TO U59-LPF-TELPORSX               
428700                    CALL POSTSUM USING POSTSUM-PARM                       
428800                 END-IF                                                   
428900                 MOVE NEJ TO SW-AUT-PLAN                                  
429000     END-IF                                                               
429100                                                                          
429200     IF LINK-FLASTERISK = JA                                              
429300        IF SW-AUT-PLAN = JA                                               
429400           MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                          
429500           MOVE 'ENDAST  '     TO POSTSUM-DDNAMN2                         
429600                                  U51KONC-TELPORS                         
429700           MOVE 'PROG'         TO POSTSUM-TRANSTYP                        
429800                                  U51KONC-TELPORS(10:4)                   
429900           MOVE 'CALLOFF>OB'   TO U59-LPF-TELPORSX                        
430000           CALL POSTSUM USING POSTSUM-PARM                                
430100        END-IF                                                            
430200        MOVE NEJ TO SW-AUT-PLAN                                           
430300     END-IF                                                               
430400                                                                          
430500     IF LINK-KVSLUTKP > ZERO                                              
430600        IF SW-AUT-PLAN = JA                                               
430700           MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                          
430800           MOVE 'SLUTKÖP '     TO POSTSUM-DDNAMN2                         
430900                                  U51KONC-TELPORS                         
431000           MOVE 'UPPD'         TO POSTSUM-TRANSTYP                        
431100                                  U51KONC-TELPORS(10:4)                   
431200           MOVE 'FINALPURCH'   TO U59-LPF-TELPORSX                        
431300           CALL POSTSUM USING POSTSUM-PARM                                
431400        END-IF                                                            
431500        MOVE NEJ TO SW-AUT-PLAN                                           
431600     END-IF                                                               
431700                                                                          
431800     IF LINK-KVUTRS   > ZERO                                              
431900        IF SW-AUT-PLAN = JA                                               
432000           MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                          
432100           MOVE 'UTREDN  '     TO POSTSUM-DDNAMN2                         
432200                                  U51KONC-TELPORS                         
432300           MOVE 'SALD'         TO POSTSUM-TRANSTYP                        
432400                                  U51KONC-TELPORS(10:4)                   
432500           MOVE 'INVEST BAL'   TO U59-LPF-TELPORSX                        
432600           CALL POSTSUM USING POSTSUM-PARM                                
432700        END-IF                                                            
432800        MOVE NEJ TO SW-AUT-PLAN                                           
432900     END-IF                                                               
433000                                                                          
433100     IF LINK-KDLPSP   = 3                                                 
433200        IF SW-AUT-PLAN = JA                                               
433300           MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                          
433400           MOVE 'KDLPSP  '     TO POSTSUM-DDNAMN2                         
433500                                  U51KONC-TELPORS                         
433600           MOVE '= 3 '         TO POSTSUM-TRANSTYP                        
433700                                  U51KONC-TELPORS(10:4)                   
433800           MOVE 'CODE DS=3'    TO U59-LPF-TELPORSX                        
433900           CALL POSTSUM USING POSTSUM-PARM                                
434000        END-IF                                                            
434100        MOVE NEJ TO SW-AUT-PLAN                                           
434200     END-IF                                                               
434300                                                                          
434400     IF LINK-KDLEVPLF = 'N' OR 'S' OR 'G' OR 'P'                          
434500        IF SW-AUT-PLAN = JA                                               
434600           MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                          
434700           MOVE 'STOPPAD '     TO POSTSUM-DDNAMN2                         
434800                                  U51KONC-TELPORS                         
434900           MOVE LINK-KDLEVPLF  TO POSTSUM-TRANSTYP                        
435000                                  U51KONC-TELPORS(10:4)                   
435100           MOVE 'AUTO.SCH'     TO U59-LPF-TELPORSX(1:8)                   
435200           MOVE LINK-KDLEVPLF  TO U59-LPF-TELPORSX(10:1)                  
435300           CALL POSTSUM USING POSTSUM-PARM                                
435400        END-IF                                                            
435500        MOVE NEJ TO SW-AUT-PLAN                                           
435600        IF LINK-KDLEVPLF = 'N'                                            
435700           MOVE JA       TO W-KDLEVPLF                                    
435800        END-IF                                                            
435900     END-IF                                                               
436000     .                                                                    
436100     EJECT                                                                
436200 L-KONTROLL-ASTERISK-W22146-MM SECTION.                                   
436300                                                                          
436400*****************************************************************         
436500*    OM LEVERANSPLANFÖRSLAGET HAR '*' FRAMFÖR FÖRSTA AVROP                
436600*    (GÖRS I PGM W22146)  SÅ SKALL DET INTE KUNNA GODKÄNNAS               
436700*    AUTOMATISKT     (UNDANTAG SATSER)                                    
436800*****************************************************************         
436900                                                                          
437000     IF SW-AUT-PLAN = JA AND LINK-IDLEVNR NOT = '1002 '                   
437100        MOVE LINK-IDARTNR   TO W-IDARTNR-D9                               
437200        MOVE WC-CDC-SE      TO W-IDDC-D9                                  
437300        MOVE LINK-IDLEVNR   TO W-IDLEVNR                                  
437400        MOVE +1             TO W-KDAVROP                                  
437500                                                                          
437600        PERFORM IMS-GU-LEVERANTOER-WDD902                                 
437700        IF SEGMENT-FINNS                                                  
437800           MOVE LEV-KVBR       TO WS-KVBR                                 
437900                                                                          
438000           MOVE I42BEG-KVBEST-PL TO WS-KVBEST-PL                          
438100                                                                          
438200           PERFORM IMS-GNP-AVROP-FORSL-WDD905                             
438300           IF SEGMENT-FINNS                                               
438400             MOVE AVROP-KVAVROP TO WS-KVAVROP                             
438500                                                                          
438600             IF WS-KVAVROP > WS-KVBR + WS-KVBEST-PL                       
438700               MOVE LINK-KDPRODSL       TO TEST-KDPRODSL                  
438800               IF GOOD-KDPRODSL                                           
438900                 IF SW-AUT-PLAN = JA                                      
439000                    MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                 
439100                    MOVE 'ENDAST  '     TO POSTSUM-DDNAMN2                
439200                                           U51KONC-TELPORS                
439300                    MOVE '****'         TO POSTSUM-TRANSTYP               
439400                                           U51KONC-TELPORS(10:4)          
439500                    MOVE 'ONLY ***'     TO U59-LPF-TELPORSX               
439600                    CALL POSTSUM USING POSTSUM-PARM                       
439700                 END-IF                                                   
439800                 MOVE NEJ TO SW-AUT-PLAN                                  
439900               END-IF                                                     
440000             END-IF                                                       
440100           ELSE                                                           
440200             MOVE LINK-KDPRODSL         TO TEST-KDPRODSL                  
440300             IF GOOD-KDPRODSL                                             
440400              IF SW-AUT-PLAN = JA                                         
440500                 MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                    
440600                 MOVE 'INGET   '     TO POSTSUM-DDNAMN2                   
440700                                        U51KONC-TELPORS                   
440800                 MOVE 'FÖRS'         TO POSTSUM-TRANSTYP                  
440900                                        U51KONC-TELPORS(10:4)             
441000                 MOVE 'NO DEMAND'    TO U59-LPF-TELPORSX                  
441100                 CALL POSTSUM USING POSTSUM-PARM                          
441200              END-IF                                                      
441300              MOVE NEJ TO SW-AUT-PLAN                                     
441400             END-IF                                                       
441500           END-IF                                                         
441600        ELSE                                                              
441700           IF SW-AUT-PLAN = JA                                            
441800              MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                       
441900              MOVE 'FEL     '     TO POSTSUM-DDNAMN2                      
442000                                     U51KONC-TELPORS                      
442100              MOVE '****'         TO POSTSUM-TRANSTYP                     
442200                                     U51KONC-TELPORS(10:4)                
442300              MOVE 'ERROR'        TO U59-LPF-TELPORSX                     
442400              CALL POSTSUM USING POSTSUM-PARM                             
442500           END-IF                                                         
442600           MOVE NEJ TO SW-AUT-PLAN                                        
442700        END-IF                                                            
442800     END-IF                                                               
442900                                                                          
443000     IF SW-AUT-PLAN = JA                                                  
443100        MOVE LINK-IDARTNR   TO W-IDARTNR-D9                               
443200        MOVE WC-CDC-SE      TO W-IDDC-D9                                  
443300        MOVE LINK-IDLEVNR   TO W-IDLEVNR                                  
443400     END-IF                                                               
443500                                                                          
443600     MOVE 'ANSK  '       TO POSTSUM-FDNAMN                                
443700     MOVE LINK-IDANSK    TO W-IDANSK                                      
443800     MOVE W-IDANSK       TO POSTSUM-DDNAMN2                               
443900     IF SW-AUT-PLAN = JA                                                  
444000        MOVE 'AUT '      TO POSTSUM-TRANSTYP                              
444100     ELSE                                                                 
444200        MOVE 'PAPP'      TO POSTSUM-TRANSTYP                              
444300     END-IF                                                               
444400     CALL POSTSUM USING POSTSUM-PARM                                      
444500     .                                                                    
444600     EJECT                                                                
444700 L1-KONTROLL-ASTERISK-W22146-MM SECTION.                                  
444800                                                                          
444900*****************************************************************         
445000*    OM LEVERANSPLANFÖRSLAGET HAR '*' FRAMFÖR FÖRSTA AVROP                
445100*    (GÖRS I PGM W22146)  SÅ SKALL DET INTE KUNNA GODKÄNNAS               
445200*    AUTOMATISKT     (UNDANTAG SATSER)                                    
445300*****************************************************************         
445400                                                                          
445500     IF SW-AUT-PLAN = JA AND LINK-IDLEVNR NOT = '1002 '                   
445600        MOVE LINK-IDARTNR   TO W-IDARTNR-D9                               
445700        MOVE WC-CDC-SE      TO W-IDDC-D9                                  
445800        MOVE LINK-IDLEVNR   TO W-IDLEVNR                                  
445900        MOVE +1             TO W-KDAVROP                                  
446000                                                                          
446100        PERFORM IMS-GU-LEVERANTOER-WDD902                                 
446200                                                                          
446300        IF SEGMENT-FINNS OR (SW-WDD902-NYUPPL = JA)                       
446400                                                                          
446500           IF SEGMENT-FINNS                                               
446600             MOVE LEV-KVBR       TO WS-KVBR                               
446700           ELSE                                                           
446800             MOVE ZERO           TO WS-KVBR                               
446900           END-IF                                                         
447000                                                                          
447100           MOVE I42BEG-KVBEST-PL TO WS-KVBEST-PL                          
447200                                                                          
447300           PERFORM L1A-SEARCH-WDD905-TAB                                  
447400           IF WS-TAB-D905-IDARTNR > +0                                    
447500              MOVE WS-TAB-D905-KVAVROP TO WS-KVAVROP                      
447600                                                                          
447700              IF WS-KVAVROP > WS-KVBR + WS-KVBEST-PL                      
447800                MOVE LINK-KDPRODSL       TO TEST-KDPRODSL                 
447900                IF GOOD-KDPRODSL                                          
448000                  IF SW-AUT-PLAN = JA                                     
448100                    MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                 
448200                    MOVE 'ENDAST  '     TO POSTSUM-DDNAMN2                
448300                                           U51KONC-TELPORS                
448400                    MOVE '****'         TO POSTSUM-TRANSTYP               
448500                                           U51KONC-TELPORS(10:4)          
448600                    MOVE 'ONLY ***'     TO U59-LPF-TELPORSX               
448700                    CALL POSTSUM USING POSTSUM-PARM                       
448800                  END-IF                                                  
448900                  MOVE NEJ TO SW-AUT-PLAN                                 
449000                END-IF                                                    
449100              END-IF                                                      
449200           ELSE                                                           
449300             MOVE LINK-KDPRODSL         TO TEST-KDPRODSL                  
449400             IF GOOD-KDPRODSL                                             
449500                IF SW-AUT-PLAN = JA                                       
449600                   MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                  
449700                   MOVE 'INGET   '     TO POSTSUM-DDNAMN2                 
449800                                          U51KONC-TELPORS                 
449900                   MOVE 'FÖRS'         TO POSTSUM-TRANSTYP                
450000                                          U51KONC-TELPORS(10:4)           
450100                   MOVE 'NO DEMAND'    TO U59-LPF-TELPORSX                
450200                   CALL POSTSUM USING POSTSUM-PARM                        
450300               END-IF                                                     
450400               MOVE NEJ TO SW-AUT-PLAN                                    
450500             END-IF                                                       
450600           END-IF                                                         
450700        ELSE                                                              
450800           IF SW-AUT-PLAN = JA                                            
450900              MOVE 'EJ AUT'       TO POSTSUM-FDNAMN                       
451000              MOVE 'FEL     '     TO POSTSUM-DDNAMN2                      
451100                                     U51KONC-TELPORS                      
451200              MOVE '****'         TO POSTSUM-TRANSTYP                     
451300                                     U51KONC-TELPORS(10:4)                
451400              MOVE 'ERROR'        TO U59-LPF-TELPORSX                     
451500              CALL POSTSUM USING POSTSUM-PARM                             
451600           END-IF                                                         
451700           MOVE NEJ TO SW-AUT-PLAN                                        
451800        END-IF                                                            
451900     END-IF                                                               
452000                                                                          
452100     IF SW-AUT-PLAN = JA                                                  
452200        MOVE LINK-IDARTNR   TO W-IDARTNR-D9                               
452300        MOVE WC-CDC-SE      TO W-IDDC-D9                                  
452400        MOVE LINK-IDLEVNR   TO W-IDLEVNR                                  
452500     END-IF                                                               
452600                                                                          
452700     MOVE 'ANSK  '       TO POSTSUM-FDNAMN                                
452800     MOVE LINK-IDANSK    TO W-IDANSK                                      
452900     MOVE W-IDANSK       TO POSTSUM-DDNAMN2                               
453000     IF SW-AUT-PLAN = JA                                                  
453100        MOVE 'AUT '      TO POSTSUM-TRANSTYP                              
453200     ELSE                                                                 
453300        MOVE 'PAPP'      TO POSTSUM-TRANSTYP                              
453400     END-IF                                                               
453500     CALL POSTSUM USING POSTSUM-PARM                                      
453600     .                                                                    
453700     EJECT                                                                
453800 L1A-SEARCH-WDD905-TAB SECTION.                                           
453900     MOVE 'L1A-SEARCH-WDD905-TAB' TO CURRENT-SECTION                      
454000                                                                          
454100     INITIALIZE WS-TAB-D905-REC                                           
454200                                                                          
454300     MOVE +0                           TO IX2-D9                          
454400     PERFORM UNTIL IX2-D9 = IX2-D9-MAX                                    
454500       ADD +1                          TO IX2-D9                          
454600       IF TAB-D905-IDARTNR (IX2-D9) = +0                                  
454700         MOVE IX2-D9-MAX               TO IX2-D9                          
454800       ELSE                                                               
454900         IF TAB-D905-IDARTNR (IX2-D9) = W-IDARTNR-D9 AND                  
455000            TAB-D905-IDDC    (IX2-D9) = W-IDDC-D9    AND                  
455100            TAB-D905-IDLEVNR (IX2-D9) = W-IDLEVNR    AND                  
455200            TAB-D905-KDAVROP (IX2-D9) = W-KDAVROP                         
455300                                                                          
455400             MOVE TAB-D905-REC(IX2-D9) TO WS-TAB-D905-REC                 
455500             MOVE IX2-D9-MAX           TO IX2-D9                          
455600         END-IF                                                           
455700       END-IF                                                             
455800     END-PERFORM                                                          
455900     .                                                                    
456000     EJECT                                                                
456100 M-KOLL-SKIP-PREL-PLAN SECTION.                                           
456200     SKIP2                                                                
456300*    * KOLL MELLAN GÄLLANDE PLAN OCH PRELIMINÄRT FÖRSLAG                  
456400*    * AVSEENDE DIFF MELLAN FÖRSTA AVROP                                  
456500*    * (SOM INFALLER UNDER DE 20 FÖRSTA VECKORNA)                         
456600                                                                          
456700     MOVE LINK-IDARTNR    TO W-IDARTNR-D9                                 
456800     MOVE WC-CDC-SE       TO W-IDDC-D9                                    
456900     MOVE LINK-TISPECST   TO W-DAAVROP-MIN                                
457000                             VADD-DATUM-AAVV                              
457100     ADD  200000          TO W-DAAVROP-MIN                                
457200     MOVE 20              TO W-ANTAL-VECKOR                               
457300     CALL W009VADD USING  VADD-DATUM-AAVV W-ANTAL-VECKOR                  
457400     MOVE VADD-DATUM-AAVV TO W-DAAVROP-MAX                                
457500     ADD  200000          TO W-DAAVROP-MAX                                
457600     MOVE 2 TO W-KDAVROP                                                  
457700     PERFORM IMS-GU-AVROP-KVAL-WDD905                                     
457800     IF SEGMENT-FINNS                                                     
457900        MOVE AVROP-DAAVROP-AVS         TO W-GALL-AVROP                    
458000        MOVE 1                         TO W-KDAVROP                       
458100        PERFORM MB-SEARCH-WDD905-TAB                                      
458200        IF WS-TAB-D905-IDARTNR > +0                                       
458300          MOVE WS-TAB-D905-DAAVROP-AVS TO W-PREL-AVROP                    
458400*        *DET FINNS BÅDE GÄLLANDE OCH PREL                                
458500*        *KOLLA DIFF,EV MOVE JA TO SW-SKIP-FORSLAG                        
458600          PERFORM MA-BER-DIFF                                             
458700        ELSE                                                              
458800*        *DET FINNS GÄLLANDE MEN EJ PREL INOM 20 V                        
458900          ADD 100                      TO W-DAAVROP-MAX                   
459000          MOVE 1                       TO W-KDAVROP                       
459100          PERFORM MB-SEARCH-WDD905-TAB                                    
459200          IF WS-TAB-D905-IDARTNR > +0                                     
459300            MOVE WS-TAB-D905-DAAVROP-AVS TO W-PREL-AVROP                  
459400            PERFORM MA-BER-DIFF                                           
459500          ELSE                                                            
459600            MOVE NEJ                   TO SW-SKIP-FORSLAG                 
459700          END-IF                                                          
459800        END-IF                                                            
459900     ELSE                                                                 
460000        MOVE 1                         TO W-KDAVROP                       
460100        PERFORM MB-SEARCH-WDD905-TAB                                      
460200        IF WS-TAB-D905-IDARTNR > +0                                       
460300*         *DET FINNS PREL MEN EJ GÄLLANDE                                 
460400*         *BEHÅLL PREL FÖRSLAG                                            
460500           MOVE NEJ                    TO SW-SKIP-FORSLAG                 
460600        ELSE                                                              
460700*         *DET FINNS VARKEN PREL ELLER GÄLLANDE                           
460800           MOVE JA                     TO SW-SKIP-FORSLAG                 
460900        END-IF                                                            
461000     END-IF                                                               
461100                                                                          
461200     IF SW-SKIP-FORSLAG = JA                                              
461300*      *DELETE PRELIMINÄRT FÖRSLAG                                        
461400        PERFORM S21-BORTTAG-ALLA-FOERSLAG                                 
461500                                                                          
461600        MOVE NEJ          TO SW-OMSPEC-UTFOERD                            
461700        MOVE ZERO         TO LINK-KDLPSP                                  
461800                                                                          
461900*--     TAG ÄVEN BORT ARTIKELN FRÅN FÖRSLAGS-KÖN                          
462000*--     OBS! DELETE WDD601 GÖRS I PGM W2215A00,SE S21-SECTION             
462100                                                                          
462200     END-IF                                                               
462300     .                                                                    
462400     EJECT                                                                
462500 MA-BER-DIFF SECTION.                                                     
462600     SKIP3                                                                
462700     IF W-GALL-AVROP >= W-PREL-AVROP                                      
462800        MOVE W-PREL-AVROP-AAVV TO W-DATUM-FROM                            
462900        MOVE W-GALL-AVROP-AAVV TO W-DATUM-TOM                             
463000        IF LINK-KDVVKL < 3                                                
463100           MOVE W-DIFF-X2      TO W-DIFF-Z                                
463200        ELSE                                                              
463300*         *LINK-KDVVKL >= 3                                               
463400           MOVE W-DIFF-Y2      TO W-DIFF-Z                                
463500        END-IF                                                            
463600     ELSE                                                                 
463700*      *W-GALL-AVROP <  W-PREL-AVROP                                      
463800        MOVE W-GALL-AVROP-AAVV TO W-DATUM-FROM                            
463900        MOVE W-PREL-AVROP-AAVV TO W-DATUM-TOM                             
464000        IF LINK-KDVVKL < 3                                                
464100           MOVE W-DIFF-X1      TO W-DIFF-Z                                
464200        ELSE                                                              
464300*         *LINK-KDVVKL >= 3                                               
464400           MOVE W-DIFF-Y1      TO W-DIFF-Z                                
464500        END-IF                                                            
464600     END-IF                                                               
464700                                                                          
464800     PERFORM S02-BERAKNA-VECKODIFFERENS                                   
464900                                                                          
465000     IF W-VECKO-DIFFERENS > W-DIFF-Z                                      
465100        MOVE NEJ               TO SW-SKIP-FORSLAG                         
465200     ELSE                                                                 
465300        MOVE JA                TO SW-SKIP-FORSLAG                         
465400     END-IF                                                               
465500     .                                                                    
465600     EJECT                                                                
465700 MB-SEARCH-WDD905-TAB SECTION.                                            
465800                                                                          
465900     INITIALIZE WS-TAB-D905-REC                                           
466000                                                                          
466100     MOVE +0                           TO IX2-D9                          
466200     PERFORM UNTIL IX2-D9 = IX2-D9-MAX                                    
466300       ADD +1                          TO IX2-D9                          
466400       IF TAB-D905-IDARTNR (IX2-D9) = +0                                  
466500          MOVE IX2-D9-MAX              TO IX2-D9                          
466600       ELSE                                                               
466700          IF TAB-D905-IDARTNR (IX2-D9) = W-IDARTNR-D9                     
466800         AND TAB-D905-IDDC    (IX2-D9) = W-IDDC-D9                        
466900         AND TAB-D905-IDLEVNR (IX2-D9) = W-IDLEVNR                        
467000         AND TAB-D905-KDAVROP (IX2-D9) = W-KDAVROP                        
467100                                                                          
467200           IF (TAB-D905-DAAVROP-AVS (IX2-D9) >= W-DAAVROP-MIN AND         
467300               TAB-D905-TILEVDAG    (IX2-D9) >= W-TILEVDAG-MIN)           
467400          AND (TAB-D905-DAAVROP-AVS (IX2-D9) <= W-DAAVROP-MAX AND         
467500               TAB-D905-TILEVDAG    (IX2-D9) <= W-TILEVDAG-MAX)           
467600             MOVE TAB-D905-REC(IX2-D9) TO WS-TAB-D905-REC                 
467700             MOVE IX2-D9-MAX           TO IX2-D9                          
467800           END-IF                                                         
467900         END-IF                                                           
468000       END-IF                                                             
468100     END-PERFORM                                                          
468200     .                                                                    
468300     EJECT                                                                
468400 Z-AVSLUTA SECTION.                                                       
468500                                                                          
468600     CLOSE                                                                
468700           W22142                                                         
468800           W22151                                                         
468900           W22152                                                         
469000           W22154                                                         
469100           W22159                                                         
469200           W2215A                                                         
469300     SKIP3                                                                
469400     MOVE 'S' TO POSTSUM-OPKOD                                            
469500     CALL POSTSUM USING POSTSUM-PARM                                      
469600     .                                                                    
469700     EJECT                                                                
469800 S01-ADD-TILL-ORSAKSTABELL SECTION.                                       
469900     SKIP3                                                                
470000     MOVE LINK-KDLPORS-TAB(01)  TO W-KDLPORS-TAB(01)                      
470100     MOVE LINK-KDLPORS-TAB(02)  TO W-KDLPORS-TAB(02)                      
470200     MOVE LINK-KDLPORS-TAB(03)  TO W-KDLPORS-TAB(03)                      
470300     MOVE W-KDLPORS             TO W-KDLPORS-TAB(04)                      
470400                                                                          
470500     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
470600                                                                          
470700     MOVE W-KDLPORS-TAB(01)     TO LINK-KDLPORS-TAB(01)                   
470800     MOVE W-KDLPORS-TAB(02)     TO LINK-KDLPORS-TAB(02)                   
470900     MOVE W-KDLPORS-TAB(03)     TO LINK-KDLPORS-TAB(03)                   
471000     .                                                                    
471100     EJECT                                                                
471200 S02-BERAKNA-VECKODIFFERENS SECTION.                                      
471300     MOVE 'S02-BERAKNA-VECKODIFFERENS '  TO CURRENT-SECTION               
471400     SKIP3                                                                
471500     MOVE W-DATUM-TOM TO W-DATUM-AAVV                                     
471600     MOVE W-DATUM-AA  TO TMP1-YY WS1-YY                                   
471700     MOVE W-DATUM-VV  TO W-VECKO-DIFFERENS                                
471800     SKIP1                                                                
471900     MOVE W-DATUM-FROM   TO W-DATUM-AAVV                                  
472000     MOVE W-DATUM-AA     TO TMP2-YY WS2-YY                                
472100     SUBTRACT W-DATUM-VV FROM W-VECKO-DIFFERENS                           
472200     PERFORM WY2000P9                                                     
472300     COMPUTE W-DIFF-AA = (TMP1-YY - TMP2-YY) * 52                         
472400     ADD W-DIFF-AA TO W-VECKO-DIFFERENS                                   
472500     IF WS2-YY = 9 AND WS1-YY > 9                                         
472600        ADD +1 TO W-VECKO-DIFFERENS                                       
472700     END-IF                                                               
472800     .                                                                    
472900     EJECT                                                                
473000 S03-TILEVDAG-DAGL-AVROP  SECTION.                                        
473100     MOVE 'S03-TILEVDAG-DAGL-AVROP '  TO CURRENT-SECTION                  
473200                                                                          
473300     MOVE ZERO                TO W-TILEVDAG (1)                           
473400                                 W-TILEVDAG (2)                           
473500                                 W-TILEVDAG (3)                           
473600                                 W-TILEVDAG (4)                           
473700                                 W-TILEVDAG (5)                           
473800     MOVE ZERO                TO ANT-LEVDAG                               
473900                                                                          
474000     MOVE +1                  TO IX-DAG                                   
474100     PERFORM UNTIL IX-DAG > 5                                             
474200        IF WART-TILEVDAG (IX-DAG) > ZERO                                  
474300           MOVE WART-TILEVDAG (IX-DAG) TO W-TILEVDAG (IX-DAG)             
474400           ADD +1             TO ANT-LEVDAG                               
474500        END-IF                                                            
474600        ADD +1                TO IX-DAG                                   
474700     END-PERFORM                                                          
474800                                                                          
474900     IF ANT-LEVDAG = ZERO                                                 
475000        MOVE LINK-IDLEVNR          TO W-IDLEVNR                           
475100        MOVE LINK-IDLEVNR-SHIP     TO W-IDLEVNR-SHIP                      
475200        PERFORM IMS-GET-LEVA01-WDF101-SHIP                                
475300        IF SEGMENT-FINNS                                                  
475400          MOVE +1                  TO IX-DAG                              
475500          PERFORM UNTIL IX-DAG > 5                                        
475600             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
475700                MOVE F1-LEV-TILEVDAG (IX-DAG)                             
475800                                   TO W-TILEVDAG (IX-DAG)                 
475900                ADD +1             TO ANT-LEVDAG                          
476000             END-IF                                                       
476100             ADD +1                TO IX-DAG                              
476200          END-PERFORM                                                     
476300                                                                          
476400          IF ANT-LEVDAG = ZERO                                            
476500             MOVE +1               TO W-TILEVDAG (1)                      
476600                                      ANT-LEVDAG                          
476700          END-IF                                                          
476800        ELSE                                                              
476900          MOVE +1                  TO W-TILEVDAG (1)                      
477000                                      ANT-LEVDAG                          
477100        END-IF                                                            
477200     END-IF                                                               
477300                                                                          
477400*--- FLYTTA TILL W221BLOC                                                 
477500     MOVE W-TILEVDAG (1)  TO BLOC-TILEVDAG-DAGL(1)                        
477600     MOVE W-TILEVDAG (2)  TO BLOC-TILEVDAG-DAGL(2)                        
477700     MOVE W-TILEVDAG (3)  TO BLOC-TILEVDAG-DAGL(3)                        
477800     MOVE W-TILEVDAG (4)  TO BLOC-TILEVDAG-DAGL(4)                        
477900     MOVE W-TILEVDAG (5)  TO BLOC-TILEVDAG-DAGL(5)                        
478000                                                                          
478100     .                                                                    
478200     EJECT                                                                
478300 S04-NOLLA-W22222 SECTION.                                                
478400                                                                          
478500     MOVE +0                        TO LNK2-KVBEHOV-SUMMA                 
478600                                       LNK2-KVBEHOV-DESSUTOM              
478700                                       LNK2-TIBEHOV-FIRST                 
478800                                                                          
478900     MOVE +1                        TO INDX                               
479000     PERFORM UNTIL INDX >  MAX-INDX                                       
479100       MOVE +0                      TO LNK2-KVBEHOV-VECKA(INDX)           
479200       ADD +1                       TO INDX                               
479300     END-PERFORM                                                          
479400     .                                                                    
479500     EJECT                                                                
479600 S10-SKRIV-W2215A SECTION.                                                
479700     MOVE 'S10-SKRIV-W2215A '  TO CURRENT-SECTION                         
479800                                                                          
479900     WRITE U5A-POST FROM U5A-AREA                                         
480000                                                                          
480100     MOVE SPACE               TO POSTSUM-TRANSTYP                         
480200     MOVE U5A-UPLP-IDPTYP     TO POSTSUM-TRANSTYP                         
480300     MOVE 'W2215A'            TO POSTSUM-FDNAMN                           
480400     MOVE 'W22150D6'          TO POSTSUM-DDNAMN2                          
480500     CALL POSTSUM USING       POSTSUM-PARM                                
480600     .                                                                    
480700     EJECT                                                                
480800 S20-BORTTAG-OMSPEC SECTION.                                              
480900     MOVE 'S20-BORTTAG-OMSPEC '  TO CURRENT-SECTION                       
481000                                                                          
481100*--- F-BORTTAG-OMSPEC SECTION I PGM W2215010                              
481200                                                                          
481300     PERFORM S100-NOLLA-W2215A-AREA                                       
481400                                                                          
481500     MOVE BORTTAG-OMSPEC-5A  TO U5A-UPLP-IDPTYP                           
481600     MOVE LINK-IDARTNR       TO U5A-UPLP-IDARTNR                          
481700     MOVE WC-CDC-SE          TO U5A-UPLP-IDDC                             
481800     MOVE LINK-IDLEVNR       TO U5A-UPLP-IDLEVNR                          
481900                                                                          
482000     PERFORM S10-SKRIV-W2215A                                             
482100                                                                          
482200     .                                                                    
482300     EJECT                                                                
482400 S21-BORTTAG-ALLA-FOERSLAG SECTION.                                       
482500     MOVE 'S21-BORTTAG-ALLA-FOERSLAG '  TO CURRENT-SECTION                
482600                                                                          
482700*--- J-BORTTAG-ALLA-FOERSLAG SECTION I PGM W2215010                       
482800                                                                          
482900*--- TAR ÄVEN BORT FRÅN FÖRSLAGSKÖN WDD6.                                 
483000                                                                          
483100     PERFORM S100-NOLLA-W2215A-AREA                                       
483200                                                                          
483300     MOVE BORTTAG-FORSLAG-5A  TO U5A-UPLP-IDPTYP                          
483400     MOVE LINK-IDARTNR        TO U5A-UPLP-IDARTNR                         
483500     MOVE WC-CDC-SE           TO U5A-UPLP-IDDC                            
483600     MOVE LINK-IDLEVNR        TO U5A-UPLP-IDLEVNR                         
483700     MOVE +1                  TO U5A-UPLP-KDAVROP                         
483800                                                                          
483900     PERFORM S10-SKRIV-W2215A                                             
484000                                                                          
484100     .                                                                    
484200     EJECT                                                                
484300 S22A-NYUPPL-DAG-AVROP  SECTION.                                          
484400     MOVE 'S22A-NYUPPL-DAG-AVROP '  TO CURRENT-SECTION                    
484500                                                                          
484600     PERFORM S100-NOLLA-W2215A-AREA                                       
484700     MOVE NYUPPL-DAG-AVROP-5A TO U5A-UPLP-IDPTYP                          
484800                                                                          
484900     PERFORM S22-NYUPPL-AVROP                                             
485000                                                                          
485100     .                                                                    
485200     EJECT                                                                
485300 S22B-NYUPPL-AVROP  SECTION.                                              
485400     MOVE 'S22B-NYUPPL-AVROP '  TO CURRENT-SECTION                        
485500                                                                          
485600     PERFORM S100-NOLLA-W2215A-AREA                                       
485700     MOVE NYUPPL-AVROP-5A TO U5A-UPLP-IDPTYP                              
485800                                                                          
485900     PERFORM S22-NYUPPL-AVROP                                             
486000                                                                          
486100     .                                                                    
486200     EJECT                                                                
486300 S22-NYUPPL-AVROP  SECTION.                                               
486400     MOVE 'S22-NYUPPL-AVROP '  TO CURRENT-SECTION                         
486500                                                                          
486600*------------------------------------------------                         
486700*-   I-NYUPPL-DAG-AVROP SECTION I PGM W2215010                            
486800*-   K-NYUPPL-AVROP SECTION I PGM W2215010                                
486900*------------------------------------------------                         
487000                                                                          
487100     MOVE LINK3-IDARTNR     TO U5A-UPLP-IDARTNR                           
487200     MOVE WC-CDC-SE         TO U5A-UPLP-IDDC                              
487300     MOVE LINK3-IDLEVNR     TO U5A-UPLP-IDLEVNR                           
487400     MOVE +1                TO U5A-UPLP-KDAVROP                           
487500                                                                          
487600     MOVE LINK3-TIAVROP-AVS    TO WS-DAAVROP-AAVV                         
487700     IF WS-DAAVROP-AAVV > 5000                                            
487800        MOVE 19                TO WS-DAAVROP-SS                           
487900     ELSE                                                                 
488000        MOVE 20                TO WS-DAAVROP-SS                           
488100     END-IF                                                               
488200     MOVE WS-DAAVROP-AVS       TO U5A-UPLP-DAAVROP-AVS                    
488300                                                                          
488400     MOVE LINK3-TILEVDAG       TO U5A-UPLP-TILEVDAG                       
488500     MOVE LINK3-TIAVRDAT-INL   TO U5A-UPLP-TIAVRDAT-INL                   
488600     MOVE LINK3-TIAVRDAT-DISP  TO U5A-UPLP-TIAVRDAT-DISP                  
488700     MOVE LINK3-KVAVROP        TO U5A-UPLP-KVAVROP                        
488800                                                                          
488900     PERFORM S10-SKRIV-W2215A                                             
489000                                                                          
489100*******************************************************                   
489200*    SAVE FOR LATER SEARCH FOR UPDATED WDD905         *                   
489300*******************************************************                   
489400     ADD +1                      TO IX1-D9                                
489500     IF IX1-D9 > IX2-D9-MAX                                               
489600        MOVE '** IX2-D9 > IX2-D9-MAX IN TAB-WDD905 ***'                   
489700                              TO FELTEXT                                  
489800        CALL FELLOG                                                       
489900     END-IF                                                               
490000     MOVE U5A-UPLP-IDARTNR       TO TAB-D905-IDARTNR      (IX1-D9)        
490100     MOVE U5A-UPLP-IDDC          TO TAB-D905-IDDC         (IX1-D9)        
490200     MOVE U5A-UPLP-IDLEVNR       TO TAB-D905-IDLEVNR      (IX1-D9)        
490300     MOVE U5A-UPLP-KDAVROP       TO TAB-D905-KDAVROP      (IX1-D9)        
490400     MOVE U5A-UPLP-DAAVROP-AVS   TO TAB-D905-DAAVROP-AVS  (IX1-D9)        
490500     MOVE U5A-UPLP-TILEVDAG      TO TAB-D905-TILEVDAG     (IX1-D9)        
490600     MOVE U5A-UPLP-TIAVRDAT-INL  TO TAB-D905-TIAVRDAT-INL (IX1-D9)        
490700     MOVE U5A-UPLP-TIAVRDAT-DISP TO TAB-D905-TIAVRDAT-DISP(IX1-D9)        
490800     MOVE U5A-UPLP-KVAVROP       TO TAB-D905-KVAVROP      (IX1-D9)        
490900                                                                          
491000     .                                                                    
491100     EJECT                                                                
491200 S23-NYUPPL-OMSPEC SECTION.                                               
491300     MOVE 'S23-NYUPPL-OMSPEC '  TO CURRENT-SECTION                        
491400                                                                          
491500*--- G-NYUPPL-OMSPEC SECTION I PGM W2215010                               
491600                                                                          
491700     PERFORM S100-NOLLA-W2215A-AREA                                       
491800                                                                          
491900     MOVE NYUPPL-OMSPEC-5A  TO U5A-UPLP-IDPTYP                            
492000     MOVE LINK-IDARTNR      TO U5A-UPLP-IDARTNR                           
492100     MOVE WC-CDC-SE         TO U5A-UPLP-IDDC                              
492200     MOVE LINK-IDLEVNR      TO U5A-UPLP-IDLEVNR                           
492300     MOVE LINK-TISPECST TO WS-DASPECST-AAVV                               
492400     IF WS-DASPECST-AAVV > 5000                                           
492500        MOVE 19         TO WS-DASPECST-SS                                 
492600     ELSE                                                                 
492700        MOVE 20         TO WS-DASPECST-SS                                 
492800     END-IF                                                               
492900     MOVE WS-DASPECST   TO U5A-UPLP-DASPECST                              
493000                                                                          
493100     MOVE LINK-KDLPORS-TAB (1) TO U5A-UPLP-KDLPORS-TAB (1)                
493200     MOVE LINK-KDLPORS-TAB (2) TO U5A-UPLP-KDLPORS-TAB (2)                
493300     MOVE LINK-KDLPORS-TAB (3) TO U5A-UPLP-KDLPORS-TAB (3)                
493400     MOVE LINK-KVBEST-PL TO U5A-UPLP-KVBEST-PL                            
493500     MOVE LINK-KDPLKOEP  TO U5A-UPLP-KDPLKOEP                             
493600                                                                          
493700*--- SAKNAS WDD901 ELLER WDD902 SÅ INSERTAS DESSA I W2215A00.             
493800                                                                          
493900     PERFORM S10-SKRIV-W2215A                                             
494000     .                                                                    
494100     EJECT                                                                
494200 S100-NOLLA-W2215A-AREA SECTION.                                          
494300     MOVE 'S100-NOLLA-W2215A-AREA'  TO CURRENT-SECTION                    
494400                                                                          
494500     MOVE SPACE      TO U5A-UPLP-IDPTYP                                   
494600                        U5A-UPLP-IDDC                                     
494700                        U5A-UPLP-IDLEVNR                                  
494800                        U5A-UPLP-KDLEVPLF                                 
494900     MOVE ZERO       TO U5A-UPLP-IDARTNR                                  
495000                        U5A-UPLP-DAAVROP-AVS                              
495100                        U5A-UPLP-DASPECST                                 
495200                        U5A-UPLP-KDAVROP                                  
495300                        U5A-UPLP-KDPLKOEP                                 
495400                        U5A-UPLP-KDLPSP                                   
495500                        U5A-UPLP-KVAVROP                                  
495600                        U5A-UPLP-KVBEST-PL                                
495700                        U5A-UPLP-TIAVRDAT-DISP                            
495800                        U5A-UPLP-TIAVRDAT-INL                             
495900                        U5A-UPLP-TILEVDAG                                 
496000                        U5A-UPLP-TILPSP                                   
496100                        U5A-UPLP-TIOMSPEC                                 
496200     MOVE SPACE      TO U5A-UPLP-FLSKROT-WLC                              
496300                                                                          
496400     MOVE 1 TO IX-ORS                                                     
496500     PERFORM UNTIL IX-ORS > 3                                             
496600        MOVE ZERO    TO U5A-UPLP-KDLPORS-TAB(IX-ORS)                      
496700        ADD 1        TO IX-ORS                                            
496800     END-PERFORM                                                          
496900                                                                          
497000     MOVE SPACE      TO U5A-UPLP-IDLEVNR-SHIP                             
497100                        U5A-UPLP-IDLANDX2-SHIP                            
497200     MOVE ZERO       TO U5A-UPLP-IDANSK                                   
497300                        U5A-UPLP-KVQ                                      
497400                        U5A-UPLP-KVPALL                                   
497500                        U5A-UPLP-KVULOAD                                  
497600                        U5A-UPLP-TIAAMMDD-SPECST                          
497700                        U5A-UPLP-TIAAMMDD-FT                              
497800                        U5A-UPLP-KVDAGAR-TT                               
497900                        U5A-UPLP-KVDAGAR-INLEV                            
498000     MOVE SPACE      TO U5A-UPLP-FLAGGA-DAGL-AVROP                        
498100                                                                          
498200     MOVE ZERO       TO U5A-UPLP-TILEVDAG-DAGL(1)                         
498300                        U5A-UPLP-TILEVDAG-DAGL(2)                         
498400                        U5A-UPLP-TILEVDAG-DAGL(3)                         
498500                        U5A-UPLP-TILEVDAG-DAGL(4)                         
498600                        U5A-UPLP-TILEVDAG-DAGL(5)                         
498700                                                                          
498800     MOVE ZERO       TO U5A-UPLP-DAAVROP-FOM                              
498900                        U5A-UPLP-DAAVROP-TOM                              
499000                        U5A-UPLP-DAAVROP-TFOM                             
499100                                                                          
499200     .                                                                    
499300     EJECT                                                                
499400                                                                          
499500                                                                          
499600******************************************************************        
499700*                                                                *        
499800*    I M S   S E K T I O N E R                                   *        
499900*                                                                *        
500000******************************************************************        
500100                                                                          
500200 IMS-GN-WDB601    SECTION.                                                
500300     MOVE 'IMS-GN-WDB601 '  TO DBS-SECTION                                
500400                                                                          
500500     MOVE 'WDB601 '   TO SSA1                                             
500600     MOVE '  GB' TO GODK-STATUSKODER                                      
500700     CALL CBLTDLI USING GN WDB6-PCB  DLI-IO-WDB601 SSA1                   
500800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
500900     PERFORM IMS-STATUSKONTROLL                                           
501000     .                                                                    
501100     EJECT                                                                
501200 IMS-GU-ARTIKEL-WDK601 SECTION.                                           
501300     MOVE 'IMS-GU-ARTIKEL-WDK601'  TO DBS-SECTION                         
501400     SKIP2                                                                
501500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
501600            DELIMITED BY SIZE INTO SSA1                                   
501700     MOVE '  GE' TO GODK-STATUSKODER                                      
501800     CALL CBLTDLI USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                  
501900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
502000     PERFORM IMS-STATUSKONTROLL                                           
502100     .                                                                    
502200     EJECT                                                                
502300 IMS-GNP-CLAG-WDK611 SECTION.                                             
502400     MOVE 'IMS-GNP-CLAG-WDK611 '  TO DBS-SECTION                          
502500     SKIP1                                                                
502600     MOVE 'WDK611 ' TO SSA1                                               
502700     MOVE '  ' TO GODK-STATUSKODER                                        
502800     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK611 SSA1                  
502900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
503000     PERFORM IMS-STATUSKONTROLL                                           
503100     .                                                                    
503200     EJECT                                                                
503300 IMS-GNP-PRIS-WDK621 SECTION.                                             
503400     MOVE 'IMS-GNP-PRIS-WDK621 '  TO DBS-SECTION                          
503500                                                                          
503600     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                        
503700            DELIMITED BY SIZE INTO SSA1                                   
503800     MOVE '  GE' TO GODK-STATUSKODER                                      
503900     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK621 SSA1                  
504000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
504100     PERFORM IMS-STATUSKONTROLL                                           
504200     .                                                                    
504300     EJECT                                                                
504400 IMS-GNP-JUST-WDK626  SECTION.                                            
504500     MOVE 'IMS-GNP-JUST-WDK626'  TO DBS-SECTION                           
504600                                                                          
504700     MOVE 'WDK611 ' TO SSA1                                               
504800     MOVE 'WDK626 ' TO SSA2                                               
504900     MOVE '  GE' TO GODK-STATUSKODER                                      
505000     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK626 SSA1 SSA2             
505100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
505200     PERFORM IMS-STATUSKONTROLL                                           
505300     .                                                                    
505400     EJECT                                                                
505500 IMS-GU-WDK701-SDC    SECTION.                                            
505600     MOVE 'IMS-GU-WDK701-SDC '  TO DBS-SECTION                            
505700                                                                          
505800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
505900          DELIMITED BY SIZE INTO SSA1                                     
506000     MOVE '  GE' TO GODK-STATUSKODER                                      
506100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
506200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
506300     PERFORM IMS-STATUSKONTROLL                                           
506400     .                                                                    
506500     SKIP2                                                                
506600 IMS-GNP-WDK711-SDC-REF  SECTION.                                         
506700     MOVE 'IMS-GNP-WDK711-SDC-REF '  TO DBS-SECTION                       
506800                                                                          
506900     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
507000          DELIMITED BY SIZE INTO SSA1                                     
507100     MOVE '  GE' TO GODK-STATUSKODER                                      
507200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
507300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
507400     PERFORM IMS-STATUSKONTROLL                                           
507500     .                                                                    
507600     EJECT                                                                
507700 IMS-GET-ARTM-ART-SEG SECTION.                                            
507800     MOVE 'IMS-GU-ARTM-ART-SEG-K9'  TO DBS-SECTION                        
507900                                                                          
508000     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
508100            DELIMITED BY SIZE INTO SSA1                                   
508200     MOVE '  GE' TO GODK-STATUSKODER                                      
508300     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
508400     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
508500     PERFORM IMS-STATUSKONTROLL                                           
508600     .                                                                    
508700     EJECT                                                                
508800 IMS-GET-INLE01-WDL201 SECTION.                                           
508900     MOVE 'IMS-GU-INLE01-WDL201 ' TO DBS-SECTION                          
509000                                                                          
509100     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
509200          DELIMITED BY SIZE INTO SSA1                                     
509300     MOVE '  GE' TO GODK-STATUSKODER                                      
509400     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201  SSA1                   
509500     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
509600     PERFORM IMS-STATUSKONTROLL                                           
509700     .                                                                    
509800     EJECT                                                                
509900 IMS-GET-INLE21-WDL221 SECTION.                                           
510000     MOVE 'IMS-GNP-INLE21-WDL221 '  TO DBS-SECTION                        
510100                                                                          
510200     MOVE 'WDL211   ' TO SSA1                                             
510300     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
510400          DELIMITED BY SIZE INTO SSA2                                     
510500     MOVE '  GE' TO GODK-STATUSKODER                                      
510600     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221  SSA1 SSA2             
510700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
510800     PERFORM IMS-STATUSKONTROLL                                           
510900     .                                                                    
511000     EJECT                                                                
511100 IMS-GU-LEV-ROT-WDD901 SECTION.                                           
511200     MOVE 'IMS-GU-LEV-ROT-WDD901 '  TO DBS-SECTION                        
511300                                                                          
511400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
511500            DELIMITED BY SIZE INTO SSA1                                   
511600     MOVE '  GE' TO GODK-STATUSKODER                                      
511700     CALL CBLTDLI USING GU  WDD91-PCB DLI-IO-WDD901 SSA1                  
511800     MOVE WDD91-STATUS-CODE TO STATUS-WS                                  
511900     PERFORM IMS-STATUSKONTROLL                                           
512000     .                                                                    
512100     EJECT                                                                
512200 IMS-GNP-LEVERANTOER-WDD902 SECTION.                                      
512300     MOVE 'IMS-GNP-LEVERANTOER-WDD902'  TO DBS-SECTION                    
512400                                                                          
512500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
512600            DELIMITED BY SIZE INTO SSA1                                   
512700     MOVE '  GE' TO GODK-STATUSKODER                                      
512800     CALL CBLTDLI USING GNP  WDD91-PCB DLI-IO-WDD902 SSA1                 
512900     MOVE WDD91-STATUS-CODE TO STATUS-WS                                  
513000     PERFORM IMS-STATUSKONTROLL                                           
513100     .                                                                    
513200     EJECT                                                                
513300 IMS-GU-LEVERANTOER-D902 SECTION.                                         
513400     MOVE 'IMS-GU-LEVERANTOER-D902'  TO DBS-SECTION                       
513500                                                                          
513600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
513700            DELIMITED BY SIZE INTO SSA1                                   
513800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
513900            DELIMITED BY SIZE INTO SSA2                                   
514000     MOVE '  GE' TO GODK-STATUSKODER                                      
514100     CALL CBLTDLI USING GU  WDD91-PCB DLI-IO-WDD902 SSA1 SSA2             
514200     MOVE WDD91-STATUS-CODE TO STATUS-WS                                  
514300     PERFORM IMS-STATUSKONTROLL                                           
514400     .                                                                    
514500     EJECT                                                                
514600 IMS-GNP-OMSPEC-WDD904  SECTION.                                          
514700     MOVE 'IMS-GNP-OMSPEC-SEG-D904 '  TO DBS-SECTION                      
514800                                                                          
514900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
515000            DELIMITED BY SIZE INTO SSA1                                   
515100     MOVE 'WDD904 ' TO SSA2                                               
515200     MOVE '  GE' TO GODK-STATUSKODER                                      
515300     CALL CBLTDLI USING GNP  WDD91-PCB DLI-IO-WDD904 SSA1 SSA2            
515400     MOVE WDD91-STATUS-CODE TO STATUS-WS                                  
515500     PERFORM IMS-STATUSKONTROLL                                           
515600     .                                                                    
515700     EJECT                                                                
515800 IMS-GNP-AVROP-WDD905-NEXT  SECTION.                                      
515900     MOVE 'IMS-GNP-AVROP-WDD905-NEXT' TO DBS-SECTION                      
516000                                                                          
516100     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
516200            DELIMITED BY SIZE INTO SSA1                                   
516300     MOVE '  GE' TO GODK-STATUSKODER                                      
516400     CALL CBLTDLI USING GNP WDD91-PCB DLI-IO-WDD905 SSA1                  
516500     MOVE WDD91-STATUS-CODE TO STATUS-WS                                  
516600     PERFORM IMS-STATUSKONTROLL                                           
516700     .                                                                    
516800     EJECT                                                                
516900 IMS-GU-LEVERANTOER-WDD902 SECTION.                                       
517000     MOVE 'IMS-GU-LEVERANTOER-WDD902 '  TO DBS-SECTION                    
517100     SKIP1                                                                
517200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
517300            DELIMITED BY SIZE INTO SSA1                                   
517400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
517500            DELIMITED BY SIZE INTO SSA2                                   
517600     MOVE '  GE' TO GODK-STATUSKODER                                      
517700     CALL CBLTDLI USING GU WDD93-PCB DLI-IO-AREA SSA1 SSA2                
517800     MOVE WDD93-STATUS-CODE TO STATUS-WS                                  
517900     PERFORM IMS-STATUSKONTROLL                                           
518000     .                                                                    
518100     SKIP2                                                                
518200 IMS-GNP-AVROP-FORSL-WDD905 SECTION.                                      
518300     MOVE 'IMS-GNP-AVROP-FORSL-WDD905 '  TO DBS-SECTION                   
518400     SKIP1                                                                
518500     STRING 'WDD905  *F(KDAVROP  =' W-KDAVROP-X ')'                       
518600            DELIMITED BY SIZE INTO SSA1                                   
518700     MOVE '  GE' TO GODK-STATUSKODER                                      
518800     CALL CBLTDLI USING GNP WDD93-PCB DLI-IO-AREA SSA1                    
518900     MOVE WDD93-STATUS-CODE TO STATUS-WS                                  
519000     PERFORM IMS-STATUSKONTROLL                                           
519100     .                                                                    
519200     EJECT                                                                
519300 IMS-GU-AVROP-KVAL-WDD905 SECTION.                                        
519400     MOVE 'IMS-GU-AVROP-KVAL-WDD905 '  TO DBS-SECTION                     
519500     SKIP1                                                                
519600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
519700            DELIMITED BY SIZE INTO SSA1                                   
519800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
519900            DELIMITED BY SIZE INTO SSA2                                   
520000     STRING 'WDD905  (WDD905KY>=' W-WDD905KY-MIN-X '&'                    
520100                     'WDD905KY<=' W-WDD905KY-MAX-X '&'                    
520200                     'KDAVROP  =' W-KDAVROP-X ')'                         
520300            DELIMITED BY SIZE INTO SSA3                                   
520400     MOVE '  GE' TO GODK-STATUSKODER                                      
520500     CALL CBLTDLI USING GU WDD93-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
520600     MOVE WDD93-STATUS-CODE TO STATUS-WS                                  
520700     PERFORM IMS-STATUSKONTROLL                                           
520800     .                                                                    
520900     EJECT                                                                
521000 IMS-GET-LEVA01-WDF101-SHIP SECTION.                                      
521100     MOVE 'IMS-GET-LEVA01-WDF101-SHIP ' TO DBS-SECTION                    
521200                                                                          
521300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
521400          DELIMITED BY SIZE INTO SSA1                                     
521500     MOVE '  GE' TO GODK-STATUSKODER                                      
521600     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
521700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
521800     PERFORM IMS-STATUSKONTROLL                                           
521900     .                                                                    
522000     SKIP3                                                                
522100 IMS-GET-LEVA14-WDF106-SHIP SECTION.                                      
522200     MOVE 'IMS-GET-LEVA14-WDF106-SHIP '  TO DBS-SECTION                   
522300                                                                          
522400     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
522500          DELIMITED BY SIZE INTO SSA1                                     
522600     STRING 'WDF106     '                                                 
522700          DELIMITED BY SIZE INTO SSA2                                     
522800     MOVE '  GE' TO GODK-STATUSKODER                                      
522900     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F106 SSA1 SSA2            
523000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
523100     PERFORM IMS-STATUSKONTROLL                                           
523200     .                                                                    
523300     EJECT                                                                
523400 IMS-GU-WDF3A1 SECTION.                                                   
523500     MOVE 'IMS-GU-WDF3A1 '  TO DBS-SECTION                                
523600                                                                          
523700     STRING 'WDF3A1      '                                                
523800          DELIMITED BY SIZE INTO SSA1                                     
523900     MOVE '  GE' TO GODK-STATUSKODER                                      
524000     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-AREA-F3A1 SSA1                 
524100     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
524200     PERFORM IMS-STATUSKONTROLL                                           
524300     .                                                                    
524400     SKIP3                                                                
524500 IMS-GN-WDF3A1 SECTION.                                                   
524600     MOVE 'IMS-GN-WDF3A1 '  TO DBS-SECTION                                
524700                                                                          
524800     STRING 'WDF3A1      '                                                
524900          DELIMITED BY SIZE INTO SSA1                                     
525000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
525100     CALL CBLTDLI USING GN WDF3-PCB DLI-IO-AREA-F3A1 SSA1                 
525200     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
525300     PERFORM IMS-STATUSKONTROLL                                           
525400     .                                                                    
525500     EJECT                                                                
525600 IMS-GET-WDD311-SVENSKA SECTION.                                          
525700     MOVE 'IMS-GET-WDD311-SVENSKA '  TO DBS-SECTION                       
525800     SKIP2                                                                
525900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
526000            DELIMITED BY SIZE INTO SSA1                                   
526100     MOVE   'WDD311  (IDSKYLT  =S  )'                                     
526200                                TO SSA2                                   
526300     MOVE '  GE' TO GODK-STATUSKODER                                      
526400     CALL CBLTDLI USING GU   WDD3-PCB WDD3-IO-AREA SSA1 SSA2              
526500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
526600     PERFORM IMS-STATUSKONTROLL                                           
526700     .                                                                    
526800     EJECT                                                                
526900 IMS-GU-WDGX2258  SECTION.                                                
527000     MOVE 'IMS-GU-WDGX2258 '  TO DBS-SECTION                              
527100                                                                          
527200     MOVE SPACES              TO SSA1 SSA2                                
527300     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
527400          DELIMITED BY SIZE INTO SSA1                                     
527500     STRING 'WDGX2258(IDLEVNRS =' W-WDGXKEY-2258-X ')'                    
527600          DELIMITED BY SIZE INTO SSA2                                     
527700     MOVE '  GE'              TO GODK-STATUSKODER                         
527800     CALL CBLTDLI USING GU 2257-PCB DLI-IO-WDGX2258 SSA1 SSA2             
527900     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
528000     PERFORM IMS-STATUSKONTROLL                                           
528100     .                                                                    
528200     SKIP2                                                                
528300 IMS-GNP-WDGX2260  SECTION.                                               
528400     MOVE 'IMS-GNP-WDGX2260 '  TO DBS-SECTION                             
528500                                                                          
528600     MOVE SPACE               TO SSA1                                     
528700     STRING 'WDGX2260(DAAVROPF<=' W-DAAVROP-2260-X                        
528800                    '&DAAVROPT>=' W-DAAVROP-2260-X                        
528900                    '&IDANSKF <=' W-IDANSK-2260-X                         
529000                    '&IDANSKT >=' W-IDANSK-2260-X ')'                     
529100          DELIMITED BY SIZE INTO SSA1                                     
529200     MOVE '  GE'              TO GODK-STATUSKODER                         
529300     CALL CBLTDLI USING GNP 2257-PCB DLI-IO-WDGX2260 SSA1                 
529400     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
529500     PERFORM IMS-STATUSKONTROLL                                           
529600     .                                                                    
529700     EJECT                                                                
529800 IMS-STATUSKONTROLL SECTION.                                              
529900                                                                          
530000     SET STATUS-IX TO 1                                                   
530100     SEARCH GODK-STATUS                                                   
530200       AT END                                                             
530300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
530400           DELIMITED BY SIZE INTO FELTEXT                                 
530500         DISPLAY FELTEXT                                                  
530600         CALL FELLOG                                                      
530700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
530800         CONTINUE                                                         
530900     END-SEARCH                                                           
531000     .                                                                    
531100     EJECT                                                                
531200*    -COPY WY2000P9                                                       
531300     EJECT                                                                
531400*    -COPY WY2000P1                                                       
531500     EJECT                                                                
531600*    -COPY WY2000P3                                                       
531700     EJECT                                                                
531800*    -COPY WY2000Q3                                                       
