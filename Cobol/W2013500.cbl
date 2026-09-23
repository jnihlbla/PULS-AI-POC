000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2013500.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   94/08/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        GÖR OMSPECIFIKATION AV LEVERANSPLAN FÖR ANGIVEN ARTIKEL.         
001000*        PROGRAMMET KAN ÄVEN ANROPAS FRÅN W20103.                         
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
001400*        PROGRAMMET UPPDATERAR        (WDD6)                              
001500*        PROGRAMMET LÄSER             (WDB6)                              
001600*                                     (WDG3) H-TYP 2257                   
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W2T135                                              
002000*        MID:         W2I13501                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W2O13501                                            
002400*                                                                         
002500*    ÄNDRAD FUNKTION:                                                     
002600*        UPPDATERAR LEV.PLANE.FÖRSLAGSBASEN WDD6 VID OMSPEC ELLER         
002700*        EGET FÖRSLAG FRÅN 2103. "MID-KDOMSPEC".                          
002800*        NY SEKTION FÖR DETTA INLAGD "HG-. /C.E. FEBR.-07                 
002900*                                                                         
003000*        ETRACKER 4820410. DO NOT INCLUDE OVERSTOCK AT MICRO-LDC          
003100*        TILLKOMMER WDB601-LÄSNING.                                       
003200*                                                                         
003300*        2015-04-22  ETRACKER 10130993                                    
003400*        REDUCE NUMBER OF DELIVERY SCHEDULES                              
003500*                                                                         
003600***-------------------------------------------------------------          
003700*** PROGRAMÄNDRINGAR                                                      
003800***-------------------------------------------------------------          
003900* 2012-01-03  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                
004000*                                                                         
004100* 2012-11-06  E-TRACKER 10185414 FLYTTA AVROP JUL/NYÅR 2012 - FIX         
004200*                                                                         
004300* 2013-11-12  E-TRACKER 10217534 FLYTTA AVROP JUL/NYÅR 2013 - FIX         
004400*                                                                         
004500* 2014-03-13  E'TRACKER 8403120  BLOCKADE AVROP, BILD 2149                
004600*                                                                         
004700* 2014-06-26  E'TRACKER 8616110  GK WRONG INFO FROM REFILL                
004800*                                                                         
004900* 2014-10-13  E-TRACKER 10240126 FLYTTA AVROP JUL/NYÅR 2014 - FIX         
005000*                                                                         
005100* 2014-11-10  E-TRACKER 10244811 DATKONVJUST JUL/NYÅR 2014 - FIX          
005200*                                                                         
005300* 2018-11-08  JIRA PULS-2923 FLYTTA AVROP JUL/NYÅR 2018 - FIX             
005400*                                                                         
005500* 2019-11-06  AZURE PBI 1362575 RETURNS SHOULD BE EXCLUDED AS             
005600*                               AN ASSET. RE-REFILL NA AND CN.            
005700*                                                                         
005800                                                                          
005900     SKIP3                                                                
006000 ENVIRONMENT DIVISION.                                                    
006100     EJECT                                                                
006200 DATA DIVISION.                                                           
006300 WORKING-STORAGE SECTION.                                                 
006400*    -COPY WY2000W3                                                       
006500     SKIP3                                                                
006600*    -COPY WY2000W2                                                       
006700     SKIP3                                                                
006800*    -COPY WY2000W9                                                       
006900     SKIP3                                                                
007000 77  IDPGM                       PIC X(08)   VALUE 'W2013500'.            
007100                                                                          
007200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007300 77  FILLER                      PIC X(08) VALUE 'FELTEXT:'.              
007400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007500 77  FILLER                      PIC X(08) VALUE 'CURRENT:'.              
007600 77  CURRENT-SECTION             PIC X(30) VALUE SPACE.                   
007700 77  FILLER                      PIC X(08) VALUE 'DBS-SEC:'.              
007800 77  DBS-SECTION                 PIC X(30) VALUE SPACE.                   
007900                                                                          
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  OCH                         PIC X       VALUE '&'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008300 77  FL-PRARTBES                 PIC X       VALUE 'N'.                   
008400 77  DEFINITIV                   PIC S9  COMP-3 VALUE +1.                 
008500                                                                          
008600*01  -COPY WWDCKONS                                                       
008700                                                                          
008800*01  -COPY WWPRODSL                                                       
008900                                                                          
009000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
009100 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
009200 77  MAX-INDX                    PIC S9(9)  VALUE +156  COMP SYNC.        
009300 77  W-INDX                      PIC S9(9)  VALUE +0    COMP SYNC.        
009400 77  W-CL-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
009500 77  BLOC-TAB-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
009600 77  BLOC-MAX-IX                 PIC S9(4)  VALUE +60   COMP SYNC.        
009700                                                                          
009800*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
009900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +121  COMP SYNC.        
010000 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
010100                                                                          
010200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010300 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
010400*                                                                         
010500 77  WS-SKIP-IDANSK-2024         PIC 9(3)    VALUE ZERO.                  
010600     88 SKIP-IDANSK-2024                     VALUE 510 THRU 569           
010700                                                   610 THRU 619           
010800                                                   518 528 538 558        
010900                                                   568 608 618 628        
011000                                                   638 648 658 678        
011100                                                   688 698 728 758        
011200                                                   788 878 898.           
011300*                                                                         
011400 77  WS-SKIP-IDANSK-2025         PIC 9(3)    VALUE ZERO.                  
011500     88 SKIP-IDANSK-2025                     VALUE 428 458 518 528        
011600                                                   538 548 558 568        
011700                                                   578 628 648 658        
011800                                                   678 688 698 728        
011900                                                   788 798 818 888        
012000                                                   898.                   
012100*                                                                         
012200 77  WS-SKIP-IDLEVNR-2024        PIC X(5)    VALUE SPACES.                
012300     88 SKIP-IDLEVNR-2024                    VALUE 'AEX15'                
012400                                                   'BL3ZA'                
012500                                                   'BWKSA'.               
012600*                                                                         
012700 01  WS-IDANSK-HELP              PIC 9(3)    VALUE ZERO.                  
012800 01  FILLER REDEFINES WS-IDANSK-HELP.                                     
012900     03 WS-IDANSK-2              PIC 9(2).                                
013000     03 WS-IDANSK-3              PIC 9(1).                                
013100                                                                          
013200*-----------------------------------------------------------------        
013300*--- LEVERANTÖRER SOM EJ SKALL FLYTTA AVROP VID JUL/NYÅR ---------        
013400                                                                          
013500 77  W-TEST-IDLEVNR-SHIP         PIC X(5)    VALUE SPACE.                 
013600     88  IDLEVNR-ALLTID-SKIP                 VALUE 'BSBZA'.               
013700     EJECT                                                                
013800*-----------------------------------------------------------------        
013900                                                                          
014000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
014100     88  INDATA-OK                           VALUE 'J'.                   
014200     88  INDATA-FEL                          VALUE 'N'.                   
014300                                                                          
014400 77  FOERST-SW                   PIC X       VALUE 'J'.                   
014500     88  FOERST                              VALUE 'J'.                   
014600     88  EJ-FOERST                           VALUE 'N'.                   
014700                                                                          
014800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014900     88  NYCKLAR-OK                          VALUE 'J'.                   
015000     88  NYCKLAR-FEL                         VALUE 'N'.                   
015100                                                                          
015200 77  SW-FRYS                     PIC X       VALUE 'N'.                   
015300 77  SW-OK                       PIC X       VALUE 'N'.                   
015400 77  SW-BLOCKAD-VECKA            PIC X       VALUE 'N'.                   
015500                                                                          
015600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015700     88  EGEN-MID                            VALUE '2135'.                
015800     88  GODK-MID                            VALUE '2131' '2132'          
015900                                                   '2133' '2134'          
016000                                                   '2135' '2136'          
016100                                                   '2137' '2138'          
016200                                                   '2139'.                
016300     88  HELP-MID                            VALUE '0551'.                
016400     EJECT                                                                
016500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016600 01  GENERELLA-SUBPROGRAM.                                                
016700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017100     03  W221PUNK                PIC X(8)    VALUE 'W221PUNK'.            
017200     03  W221LPAD                PIC X(8)    VALUE 'W221LPAD'.            
017300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017400     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
017500     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
017600     03  W22222                  PIC X(8)    VALUE 'W22222'.              
017700     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
017800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
017900     03  W221BLOC                PIC X(8)    VALUE 'W221BLOC'.            
018000     EJECT                                                                
018100*01  -COPY WORKAREA                                                       
018200     EJECT                                                                
018300*    --- LÄNKAREA TILL SUBPROGRAM W222PBTO  (KVPB-PLAN)                   
018400*01  -COPY W222PBTO                                                       
018500     EJECT                                                                
018600*    --- LÄNKAREA TILL SUBPROGRAM W221PUNK                                
018700*01 -COPY W221PUNK    -PRE PUNK-                                          
018800     EJECT                                                                
018900*    --- LÄNKAREA TILL SUBPROGRAM W221BLOC                                
019000*01 -COPY W221BLOC                                                        
019100     EJECT                                                                
019200*    *************************************                                
019300*    **  LINK-AREA                      **                                
019400*    **  BEHOVSTABELL                   **                                
019500*    *************************************                                
019600*01  AREA  -COPY W222L222   -PRE LINK-.                                   
019700     EJECT                                                                
019800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019900*01 -COPY WMEDAREA                                                        
020000     SKIP3                                                                
020100                                                                          
020200 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
020300*   -COPY WZ20DAYS                                                        
020400     EJECT                                                                
020500                                                                          
020600 01  MESSAGE-CODES.                                                       
020700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
020800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
021000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021200     EJECT                                                                
021300*                            *** PARAMETRAR TILL WDATKONV '               
021400*01  -COPY WDATAREA.                                                      
021500     EJECT                                                                
021600*                            *** PARAMETRAR TILL WDAGKONV '               
021700*01  -COPY WDAGAREA.                                                      
021800     EJECT                                                                
021900* VARIABLER TILL SUBPROGRAM W009VADD                                      
022000 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
022100 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
022200     EJECT                                                                
022300*----------------------------------------- BYTES-ARTIKEL.                 
022400 01  FILLER                  PIC X(16)   VALUE 'BYTES-ARTIKEL'.           
022500 01  TEST-IDARTNR            PIC 9(9)    COMP-3.                          
022600*01  FILLER     -COPY WWBYT02    -RED  TEST-IDARTNR.                      
022700     EJECT                                                                
022800*01  FILLER     -COPY W221W012 -PRE W012-                                 
022900     EJECT                                                                
023000*    -COPY W200EMAB                                                       
023100     EJECT                                                                
023200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023300*                                                                         
023400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023500     SKIP3                                                                
023600*01  MID -COPY W2I13501    -PRE MID-                                      
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023900     SKIP3                                                                
024000*01  -COPY WMSGAREA                                                       
024100     EJECT                                                                
024200     03  MOD REDEFINES MSG-AREA.                                          
024300*      05  -COPY W2O13501                                                 
024400     EJECT                                                                
024500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024600     SKIP3                                                                
024700*01  -COPY WMFSAREA                                                       
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
025000 01      P-TO-P-SW.                                                       
025100                                                                          
025200  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
025300  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
025400  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
025500  02     P-TO-P-KDTRANS          PIC X(8).                                
025600  02     P-TO-P-IDTRANS          PIC X(4).                                
025700  02     P-TO-P-KDMFSFOR         PIC X(1).                                
025800  02     P-TO-P-DATA             PIC X(1000).                             
025900     EJECT                                                                
026000*01  -COPY W2I10301   -PRE MOD2103-MID-                                   
026100     EJECT                                                                
026200 01 SPARAREOR.                                                            
026300     03  W-SPAR-ARTC23.                                                   
026400         05 W-SPAR-REDIRLEV  OCCURS 2                                     
026500                             PIC S9(1)V9(2) VALUE ZERO COMP-3.            
026600         05 W-SPAR-KVPB-SATS OCCURS 2                                     
026700                             PIC S9(6)V9(1) VALUE ZERO COMP-3.            
026800         05 W-SPAR-KVPB-SEP  OCCURS 2                                     
026900                             PIC S9(6)V9(1) VALUE ZERO COMP-3.            
027000         05 W-SPAR-TOT-KVPB-SEP                                           
027100                             PIC S9(6)V9(1) VALUE ZERO COMP-3.            
027200     03  W-SPAR-ARTC31.                                                   
027300         05 W-SPAR-RESEASON  OCCURS 2                                     
027400                             PIC S9(1)V9(2)   VALUE ZERO COMP-3.          
027500     03  W-SPAR-ARTC91.                                                   
027600         05 W-SPAR-KDERS   OCCURS 2 PIC S9(3) VALUE ZERO COMP-3.          
027700         05 W-SPAR-KVLS    OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
027800         05 W-SPAR-KVRESS  OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
027900         05 W-SPAR-KVAKS   OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
028000         05 W-SPAR-KVROS   OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
028100         05 W-SPAR-KVRETUR OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
028200         05 W-SPAR-KVSLAGER OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.         
028300     03  W-SPAR-INLB11.                                                   
028400         05 W-SPAR-KVBR          PIC S9(7)    VALUE ZERO COMP-3.          
028500         05 W-SPAR-KVBR-TOT      PIC S9(7)    VALUE ZERO COMP-3.          
028600 01 ARBETSAREOR.                                                          
028700     03 W-VECKO-KVPB-SEP  OCCURS 2                                        
028800                             PIC S9(6)V9(1) VALUE ZERO COMP-3.            
028900     03  W-KDPRODSL          PIC  9(2).                                   
029000     03  FILLER REDEFINES W-KDPRODSL.                                     
029100         05  FILLER          PIC 9.                                       
029200         05  W-IDPROD        PIC 9.                                       
029300     03  W-W221LP-CTX        PIC X(08) VALUE 'W221LP01'.                  
029400     03  W-KDLPORS-GRP.                                                   
029500         05 W-KDLPORS-TAB    OCCURS 4  PIC 9(03).                         
029600     03  W-TMP1-DATUM-FROM   PIC 9(4).                                    
029700     03  W-TMP1-DAT-FROM     REDEFINES W-TMP1-DATUM-FROM.                 
029800       05 W-TMP1-DAT-FROM-AA PIC 9(2).                                    
029900       05 W-TMP1-DAT-FROM-VV PIC 9(2).                                    
030000     03  W-TMP2-DATUM-TO     PIC 9(4).                                    
030100     03  W-DATUM-AAVVD       PIC S9(5) COMP-3 VALUE ZERO.                 
030200     03  W-DATUM-AAVV-S      PIC 9(4).                                    
030300     03  W-DATUM-AAVV        PIC 9(4).                                    
030400     03  W-DAT-AAVV          REDEFINES W-DATUM-AAVV.                      
030500         05  W-DATUM-AA      PIC 9(2).                                    
030600         05  W-DATUM-VV      PIC 9(2).                                    
030700     03  W-DATUM-AAMMDD      PIC 9(6).                                    
030800     03  W-DAT2-AAMMDD       REDEFINES W-DATUM-AAMMDD.                    
030900         05  W-DATUM2-AA     PIC 9(2).                                    
031000         05  W-DATUM2-MM     PIC 9(2).                                    
031100         05  W-DATUM2-DD     PIC 9(2).                                    
031200                                                                          
031300     03  AKT-DATUM-AAVV      PIC 9(4) VALUE ZERO.                         
031400     03  FILLER REDEFINES AKT-DATUM-AAVV.                                 
031500         05  AKT-DATUM-AA    PIC 9(2).                                    
031600         05  AKT-DATUM-VV    PIC 9(2).                                    
031700                                                                          
031800     03  W-DAT-TID-AKT       PIC 9(1) VALUE ZERO.                         
031900     03  W-TIFINLV-AAVV-S    PIC 9(4) VALUE ZERO.                         
032000     03  TIFINLV-AAVVD       PIC 9(5) VALUE ZERO.                         
032100     03  FILLER REDEFINES TIFINLV-AAVVD.                                  
032200         05  TIFINLV-AA      PIC 9(2).                                    
032300         05  TIFINLV-VV      PIC 9(2).                                    
032400         05  FILLER          PIC 9(1).                                    
032500     03  DAGENS-ABS-VV       PIC 9(5)     VALUE ZERO.                     
032600     03  TIFINLV-ABS-VV      PIC 9(5)     VALUE ZERO.                     
032700     03  VECKO-DIFF          PIC S9(3)    COMP-3.                         
032800     03  MAX-BEHOVSVECKOR-I-TAB                                           
032900                             PIC S9(9)   VALUE +156  COMP SYNC.           
033000     03  SEP-SATS-TPO-LEV-SDC-NDC                                         
033100                             PIC  X(2)   VALUE '19'.                      
033200     03  SATS-TPO-LEV-SDC-NDC                                             
033300                             PIC  X(2)   VALUE '18'.                      
033400     03  SEMESTER-VECKA-START                                             
033500                             PIC S9(3)   VALUE +999  COMP-3.              
033600     03  SEMESTER-VECKA-SLUT                                              
033700                             PIC S9(3)   VALUE +0    COMP-3.              
033800     03  OLIKA-LEV           PIC X(5).                                    
033900       88 RENAULT-LEVNR                 VALUE '3868'                      
034000                                              'K8M6A'.                    
034100       88 VOLKSWAGEN-LEVNR              VALUE '6453'                      
034200                                              'Q09EB'.                    
034300       88 ALLISON-LEVNR                 VALUE '4175'.                     
034400       88 EATON-LEVNR                   VALUE '5333'.                     
034500       88 SOMA-LEVNR                    VALUE '3680'.                     
034600       88 TRW-LEVNR                     VALUE '5362'                      
034700                                              'R9K2A'.                    
034800       88 SATS-LEVNR                    VALUE '1002'.                     
034900       88 GEMEN-LEVNR                   VALUE '14489'                     
035000                                              'DL7YA'.                    
035100       88 SKOVDE-LEVNR                  VALUE '1621'                      
035200                                              'C7CUL'.                    
035300     03  IX                  PIC S9(9)               COMP-3.              
035400     03  IX-L                PIC S9(9)               COMP-3.              
035500     03  IX-SUM              PIC S9(9)               COMP-3.              
035600     03  IX-ORS              PIC S9(3)               COMP-3.              
035700     03  IX-DAG              PIC S9(3)               COMP-3.              
035800     03  IX-DG               PIC S9(3)               COMP-3.              
035900     03  IX-MED              PIC S9(3) COMP-3 VALUE +1.                   
036000     SKIP1                                                                
036100     03  W-IDANSK            PIC S9(3)               COMP-3.              
036200     03  W-TILLG             PIC S9(7)V9(2)          COMP-3.              
036300     03  W-TILLG-SPAR        PIC S9(7)V9(2)          COMP-3.              
036400     03  W-TILLG-BER         PIC S9(7)V9(2)          COMP-3.              
036500     03  W-VECKO-ATGANG      PIC S9(7)               COMP-3.              
036600     03  W-VECKO-PB          PIC S9(6)V9(1)          COMP-3.              
036700     03  W-BUFF              PIC S9(7)               COMP-3.              
036800     03  W-BUFF-VV           PIC S9(7)V99            COMP-3.              
036900     03  W-BUFF-VV-1         PIC S9(7)V99            COMP-3.              
037000     03  W-AVROPSKVANTITET   PIC S9(7)               COMP-3.              
037100     03  W-KVAVROP-VVKL12-ACC PIC S9(7)              COMP-3.              
037200     03  W-KVANTITET         PIC S9(7)               COMP-3.              
037300     03  W-ANTAL             PIC S9(9)               COMP-3.              
037400     03  W-ANTAL-DEC         PIC S9(9)V99            COMP-3.              
037500     03  W-KVPB              PIC S9(6)V9(3)          COMP-3.              
037600     03  W-KVPB-SDC-TOT      PIC S9(6)V9(1)          COMP-3.              
037700     03  W-KVPB-SDC-EJ-DIR   PIC S9(6)V9(1)          COMP-3.              
037800     03  W-TILLG-SDC         PIC S9(7)               COMP-3.              
037900     03  W-OVERLAGER-SDC     PIC S9(7)               COMP-3.              
038000     03  W-KDERS             PIC 9(2).                                    
038100     03  W-KVBEST-REST       PIC S9(7)               COMP-3.              
038200     03  W-DUMMY             PIC S9(7)               COMP-3.              
038300     03  W-DATUM-AAVV-HELP   PIC S9(5)               COMP-3.              
038400     SKIP1                                                                
038500     SKIP1                                                                
038600     03  W-DATUM-FROM        PIC S9(5)               COMP-3.              
038700     03  W-DATUM-TOM         PIC S9(5)               COMP-3.              
038800     03  W-DIFF-AA           PIC S9(3)               COMP-3.              
038900     03  W-VECKO-DIFFERENS   PIC S9(3)               COMP-3.              
039000     03  W-VECKO-DIFFERENS-VV PIC S9(3)              COMP-3.              
039100     SKIP1                                                                
039200     03  DAGENS-AAAAMMDD     PIC 9(8)     VALUE ZERO.                     
039300     03  W-DATUM-AAVV-AKT    PIC S9(5)               COMP-3.              
039400     03  W-TISPECST-DISP     PIC S9(5)               COMP-3.              
039500     03  W-TISPECST-AVS      PIC S9(5)               COMP-3.              
039600     03  W-GRAENS-AVROP      PIC S9(5)               COMP-3.              
039700     03  W-KVDAGAR-FFH       PIC S9(3)               COMP-3.              
039800     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
039900     03  W-KVVECKOR-SPEC     PIC S9(3)               COMP-3.              
040000     03  W-KVVECKOR-INLEV    PIC S9(3)               COMP-3.              
040100     03  W-KVVECKOR-TT       PIC S9(3)               COMP-3.              
040200     03  W-KVVECKOR-FFH      PIC S9(3)               COMP-3.              
040300     03  W-KVVECKOR-TEMP1    PIC S9(3)               COMP-3.              
040400     03  W-KVVECKOR-TEMP2    PIC S9(3)               COMP-3.              
040500     03  W-SEMESTER-VV-START PIC S9(3)               COMP-3.              
040600     03  W-SEMESTER-VV-SLUT  PIC S9(3)               COMP-3.              
040700     03  W-TISPECST-ADJ      PIC 9(4).                                    
040800     03  W-TIFINLV           PIC 9(5).                                    
040900     03  FILLER REDEFINES W-TIFINLV.                                      
041000         05  W-TIFINLV-1-4   PIC 9(4).                                    
041100         05  FILLER          PIC 9.                                       
041200     03  FILLER REDEFINES W-TIFINLV.                                      
041300         05  W-TIFINLV-AA    PIC 9(2).                                    
041400         05  W-TIFINLV-VV    PIC 9(2).                                    
041500         05  FILLER          PIC 9.                                       
041600     03  ANTAL-VV            PIC S9(5)   COMP-3  VALUE ZERO.              
041700     03  W-AAVV-ADD          PIC S9(5)   COMP-3  VALUE ZERO.              
041800     03  W-DASPECST          PIC 9(6).                                    
041900     03  FILLER REDEFINES W-DASPECST.                                     
042000         05  W-DASPECST-SS   PIC 9(2).                                    
042100         05  W-DASPECST-AAVV PIC 9(4).                                    
042200     03  W-DAAVROP-AVS       PIC 9(6).                                    
042300     03  FILLER REDEFINES W-DAAVROP-AVS.                                  
042400         05  W-DAAVROP-SS    PIC 9(2).                                    
042500         05  W-DAAVROP-AAVV  PIC 9(4).                                    
042600     SKIP2                                                                
042700     03  W-KVANTAL-TILLG     PIC S9(7)               COMP-3.              
042800     03  W-KVKP-EXTRA        PIC S9(7)   VALUE ZERO  COMP-3.              
042900     03  W-N                 PIC S9(7)   VALUE ZERO  COMP-3.              
043000     03  W-KVANTAL-KOEP      PIC S9(7)   VALUE ZERO  COMP-3.              
043100     03  SUM-RETUR           PIC S9(7)   VALUE ZERO  COMP-3.              
043200                                                                          
043300     03  W-M                 PIC S9(7)               COMP-3.              
043400     03  IX-M                PIC S9(9)   VALUE +0    COMP SYNC.           
043500     03  IX-KOM              PIC S9(9)   VALUE +0    COMP SYNC.           
043600     03  IX-W-Q-FREKV        PIC S9(9)   VALUE +0    COMP SYNC.           
043700     03  W-Q-FREKV-MAX       PIC S9(3)   VALUE +0    COMP-3.              
043800     03  W-ARBKVANT          PIC S9(7)   VALUE +0    COMP-3.              
043900     03  W-ARBKVANT2         PIC S9(7)V99  VALUE +0    COMP-3.            
044000     03  W-ARBKVANT-SPAR     PIC S9(7)   VALUE +0    COMP-3.              
044100     03  W-KVANTAL           PIC S9(6)               COMP-3.              
044200     03  W-ARSBEH            PIC S9(9)      VALUE ZERO COMP-3.            
044300     03  W-ARSOMS            PIC S9(9)V9(2) VALUE ZERO COMP-3.            
044400     03  W-ARSOMS-80000      PIC S9(9)V9(2) VALUE +80000.00               
044500                                                     COMP-3.              
044600     03  W-TILEVDAG  OCCURS 5    PIC 9.                                   
044700     03  ARB-TILEVDAG  OCCURS 5  PIC 9.                                   
044800     03  W-KVAVROP   OCCURS 5    PIC S9(7) COMP-3.                        
044900     03  WOL-TILEVDAG            PIC 9.                                   
045000     03  WOL-TIAAVV-AVS          PIC 9(4).                                
045100     03  ANT-LEVDAG              PIC 9(3).                                
045200     03  W-TIAAMMDD-AVS          PIC 9(6).                                
045300     03  ARB-TIAAMMDD-AVS        PIC 9(6).                                
045400     03  GAM-TIAAMMDD-AVS        PIC 9(6).                                
045500     03  WS-TIAAMMDD-SPECST      PIC 9(6)     VALUE ZERO.                 
045600     03  W-TIAVROP-AVS           PIC 9(4).                                
045700     03  SPAR-TIAVROP-AVS        PIC 9(4).                                
045800     03  FIX-AAVVD               PIC 9(5)     VALUE ZERO.                 
045900     03  WS-KVPALL               PIC S9(7)    VALUE ZERO COMP-3.          
046000     03  WS-KVULOAD              PIC S9(7)    VALUE ZERO COMP-3.          
046100     03  WS-DAYS-TIDATE1-AAVVD   PIC 9(5)     VALUE ZERO.                 
046200     03  WS-DAYS-TIAAVV          PIC 9(4)     VALUE ZERO.                 
046300                                                                          
046400     03  WS-SUMMA-KVPB           PIC S9(6)V9(1)   COMP-3.                 
046500     03  WS-IDLANDX2-SHIP        PIC X(2)     VALUE SPACE.                
046600     03  WS-FRYSTID8             PIC 9(8).                                
046700     03  FILLER  REDEFINES WS-FRYSTID8.                                   
046800         05  WS-FRYSTID-SEKEL    PIC 9(2).                                
046900         05  WS-FRYSTID          PIC 9(6).                                
047000                                                                          
047100 01  WS-TIAAVV               PIC 9(04)   VALUE ZERO.                      
047200 01  WS-TIAVROP-DISP         PIC 9(04)   VALUE ZERO.                      
047300                                                                          
047400 01  SWITCHAR.                                                            
047500     03  SW-OMSPEC-UTFOERD   PIC X       VALUE 'N'.                       
047600     03  SW-BESTREST-TAEKT   PIC X       VALUE 'N'.                       
047700     03  SW-FOERSTA-SEMESTER-LAEST                                        
047800                             PIC X       VALUE 'N'.                       
047900                                                                          
048000     03  SW-TRAEFF           PIC X        VALUE 'N'.                      
048100         88 TRAEFF                        VALUE 'J'.                      
048200     03  SW-AUT-PLAN         PIC X        VALUE 'J'.                      
048300     SKIP3                                                                
048400****************************************** TILLGÅNGSTABELL                
048500 01  FILLER                 PIC X(11) VALUE 'TILLGTABELL'.                
048600 01  TILLGANGSTABELL.                                                     
048700     03  TILLGTAB-MAX        PIC S9(9)   VALUE +156  COMP SYNC.           
048800     03  TILLGTAB-IX         PIC S9(9)   VALUE +0    COMP SYNC.           
048900     SKIP1                                                                
049000     03  TILLGTAB.                                                        
049100         05  TILLGTAB-INGANG OCCURS 156.                                  
049200             10  TILLGTAB-ANTAL                                           
049300                             PIC S9(7)V99            COMP-3.              
049400     EJECT                                                                
049500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
049600*                                                                         
049700                                                                          
049800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
049900     SKIP3                                                                
050000 01  NYCKLAR-TILL-DLI.                                                    
050100     03  W-DAPRLIST-X.                                                    
050200         05  W-DAPRLIST          PIC 9(8)  VALUE ZERO.                    
050300     03  W-WDD901KY-X.                                                    
050400         05  W-IDARTNR-INLB      PIC S9(9)   VALUE ZERO COMP-3.           
050500         05  W-IDDC-INLB         PIC X(2)    VALUE SPACE.                 
050600     03  W-IDARTNR-X.                                                     
050700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
050800     03  W-IDLEVNR-X.                                                     
050900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
051000     03  W-IDLEVNR-SHIP-X.                                                
051100         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
051200     03  W-KDSEGKEY-X.                                                    
051300         05  W-KDSEGKEY          PIC X(1).                                
051400*    03  W-DASPECST-X.                                                    
051500*        05  W-DASPECST          PIC  9(6)   VALUE ZERO.                  
051600     03  W-KDAVROP-X.                                                     
051700         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
051800     03  W-IDPTYP-X.                                                      
051900         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
052000     03  W-WDD905KY-X.                                                    
052100         05  W-DAAVROP-KY        PIC 9(6)    VALUE ZERO.                  
052200         05  W-TILEVDAG-KY       PIC S9      VALUE ZERO COMP-3.           
052300     03  W-WDF301KY-X.                                                    
052400         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
052500         05  W-DADATUM-HELG      PIC 9(8)    VALUE ZERO.                  
052600         05  FILLER  REDEFINES W-DADATUM-HELG.                            
052700             07  W-DADATUM-HELG-SS      PIC 9(2).                         
052800             07  W-DADATUM-HELG-AAMMDD  PIC 9(6).                         
052900     03  W-WDD601KY-MIN-X.                                                
053000            05 W-IDDC-MIN        PIC X(2)  VALUE SPACE.                   
053100            05 W-IDLEVNR-MIN     PIC X(5)  VALUE SPACE.                   
053200            05 W-IDARTNR-MIN     PIC S9(9) VALUE ZERO COMP-3.             
053300            05 W-IDANSK-MIN      PIC S9(3) VALUE ZERO COMP-3.             
053400     03  W-WDD601KY-MAX-X.                                                
053500            05 W-IDDC-MAX        PIC X(2)  VALUE '99'.                    
053600            05 W-IDLEVNR-MAX     PIC X(5)  VALUE SPACE.                   
053700            05 W-IDARTNR-MAX     PIC S9(9) VALUE ZERO COMP-3.             
053800            05 W-IDANSK-MAX      PIC S9(3) VALUE 999  COMP-3.             
053900                                                                          
054000     03  W-IDANSK-NON-X.                                                  
054100         05  W-IDANSK-NON        PIC S9(3) VALUE ZERO COMP-3.             
054200                                                                          
054300     03  W-IDDC-B6-X.                                                     
054400         05  W-IDDC-B6           PIC X(2)  VALUE SPACE.                   
054500                                                                          
054600                                                                          
054700     03  W-IDDC-REF-X.                                                    
054800         05  W-IDDC-REF          PIC X(2)   VALUE '11'.                   
054900                                                                          
055000                                                                          
055100*------                                                                   
055200                                                                          
055300     03  W-WDGXKEY-2257-X.                                                
055400         05 W-IDHTYP-2257        PIC X(4)    VALUE '2257'.                
055500         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
055600                                                                          
055700     03  W-WDGXKEY-2258-X.                                                
055800         05 W-IDLEVNR-SHIP-2258  PIC X(5)    VALUE SPACE.                 
055900                                                                          
056000     03  W-DAAVROP-2260-X.                                                
056100         05 W-DAAVROP-2260       PIC 9(6)    VALUE ZERO.                  
056200                                                                          
056300     03  W-IDANSK-2260-X.                                                 
056400         05 W-IDANSK-2260        PIC S9(3)   VALUE ZERO COMP-3.           
056500                                                                          
056600     EJECT                                                                
056700*      --- VALID IDDC CODES                                               
056800*                                                                         
056900*01    -COPY WWDC99                                                       
057000       EJECT                                                              
057100*    --- STATUS-KOD FRÅN IMS                                              
057200 01  STATUS-WS                   PIC XX.                                  
057300     88  SEGMENT-FINNS                       VALUE '  '.                  
057400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
057500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
057600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
057700     SKIP2                                                                
057800 01  GODK-STATUSKODER.                                                    
057900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
058000     SKIP3                                                                
058100 01  SSA1                        PIC X(128).                              
058200 01  SSA2                        PIC X(64).                               
058300 01  SSA3                        PIC X(64).                               
058400     EJECT                                                                
058500*    --- IMS FUNKTIONSKODER                                               
058600*01  -COPY W0003                                                          
058700     EJECT                                                                
058800*    ---  DLI INPUT-OUTPUT AREA                                           
058900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
059000     SKIP3                                                                
059100 01  DLI-IO-AREA1.                                                        
059200     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
059300     SKIP3                                                                
059400     03  WLARTC01 REDEFINES IO-AREA1.                                     
059500*        05  -COPY WDK601                                                 
059600     EJECT                                                                
059700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
059800     SKIP3                                                                
059900 01  DLI-IO-AREA2.                                                        
060000     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
060100     SKIP3                                                                
060200     03  WLARTC11 REDEFINES IO-AREA2.                                     
060300*        05  -COPY WDK611                                                 
060400     EJECT                                                                
060500 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WLARTC21'.          
060600     SKIP3                                                                
060700 01  DLI-IO-WLARTC21.                                                     
060800*    03  -COPY WDK621                                                     
060900     EJECT                                                                
061000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
061100     SKIP3                                                                
061200 01  DLI-IO-AREA7.                                                        
061300     03  IO-AREA7                PIC X(150)  VALUE SPACE.                 
061400     SKIP3                                                                
061500     03  WLINLB01 REDEFINES IO-AREA7.                                     
061600*        05  -COPY WDD901  -PRE INLB01-                                   
061700     EJECT                                                                
061800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
061900     SKIP3                                                                
062000 01  DLI-IO-AREA8.                                                        
062100     03  IO-AREA8                PIC X(150)  VALUE SPACE.                 
062200     SKIP3                                                                
062300     03  WLINLB11 REDEFINES IO-AREA8.                                     
062400*        05  -COPY WDD902  -PRE INLB11-                                   
062500     EJECT                                                                
062600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA9'.        
062700     SKIP3                                                                
062800 01  DLI-IO-AREA9.                                                        
062900     03  IO-AREA9                PIC X(150)  VALUE SPACE.                 
063000     03  WLINLB22 REDEFINES IO-AREA9.                                     
063100*        05  -COPY WDD904  -PRE INLB22-                                   
063200     EJECT                                                                
063300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA10'.        
063400     SKIP3                                                                
063500 01  DLI-IO-AREA10.                                                       
063600     03  IO-AREA10                PIC X(150)  VALUE SPACE.                
063700     03  WLINLB23 REDEFINES IO-AREA10.                                    
063800*        05  -COPY WDD905  -PRE INLB23-                                   
063900     EJECT                                                                
064000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA1A'.        
064100     SKIP3                                                                
064200 01  DLI-IO-AREA1A.                                                       
064300     03  IO-AREA1A                PIC X(150)  VALUE SPACE.                
064400     03  WLINLB2A REDEFINES IO-AREA1A.                                    
064500*        05  -COPY WDD905  -PRE INLB2A-                                   
064600     EJECT                                                                
064700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA11'.        
064800     SKIP3                                                                
064900 01  DLI-IO-AREA11.                                                       
065000     03  IO-AREA11                PIC X(150)  VALUE SPACE.                
065100     03  WLARTM01 REDEFINES IO-AREA11.                                    
065200*        05  -COPY WDK901  -PRE ARTM01-                                   
065300     EJECT                                                                
065400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA13'.        
065500     SKIP3                                                                
065600 01  DLI-IO-AREA13.                                                       
065700     03  IO-AREA13                PIC X(150)  VALUE SPACE.                
065800     03  WLARTC26 REDEFINES IO-AREA13.                                    
065900*        05  -COPY WDK626                                                 
066000     EJECT                                                                
066100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA14'.        
066200     SKIP3                                                                
066300 01  DLI-IO-AREA14.                                                       
066400     03  IO-AREA14                PIC X(300)  VALUE SPACE.                
066500     03  WDK701 REDEFINES IO-AREA14.                                      
066600*        05  -COPY WDK701                                                 
066700     EJECT                                                                
066800     03  WDK711 REDEFINES IO-AREA14.                                      
066900*        05  -COPY WDK711                                                 
067000     EJECT                                                                
067100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA21'.        
067200     SKIP3                                                                
067300 01  DLI-IO-AREA21.                                                       
067400     03  IO-AREA21                PIC X(150)  VALUE SPACE.                
067500     03  WLARTS01 REDEFINES IO-AREA21.                                    
067600*        05  -COPY WDL221                                                 
067700     EJECT                                                                
067800 01  FILLER                     PIC X(16)  VALUE 'DLI-IO-AREA-F1'.        
067900     SKIP3                                                                
068000 01  DLI-IO-AREA-F1          PIC X(100).                                  
068100     SKIP2                                                                
068200*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
068300     EJECT                                                                
068400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F106'.         
068500     SKIP3                                                                
068600 01  DLI-IO-AREA-F106.                                                    
068700     SKIP2                                                                
068800*    03  WLLEVA14 -COPY WDF106                                            
068900     EJECT                                                                
069000 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F301'.         
069100     SKIP3                                                                
069200 01  DLI-IO-AREA-F301.                                                    
069300     SKIP2                                                                
069400*    03  -COPY WDF301                                                     
069500     EJECT                                                                
069600                                                                          
069700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F311'.         
069800 01  DLI-IO-AREA-F311.                                                    
069900     SKIP2                                                                
070000*    03  -COPY WDF311                                                     
070100     EJECT                                                                
070200                                                                          
070300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-WDD3'.         
070400 01  DLI-IO-AREA-WDD3.                                                    
070500     SKIP2                                                                
070600*    03  -COPY WDD311                                                     
070700     EJECT                                                                
070800                                                                          
070900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-WDD6'.         
071000 01  DLI-IO-AREA-WDD6.                                                    
071100     SKIP2                                                                
071200*    03  -COPY WDD601                                                     
071300     EJECT                                                                
071400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-WDB6'.         
071500 01  DLI-IO-AREA-WDB6.                                                    
071600     SKIP2                                                                
071700*    03  -COPY WDB601                                                     
071800     EJECT                                                                
071900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-2-B6'.         
072000 01  DLI-IO-AREA-2-B6.                                                    
072100     SKIP2                                                                
072200*    03  -COPY WDB601 -PRE 2-                                             
072300     EJECT                                                                
072400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2258'.          
072500 01  DLI-IO-WDGX2258.                                                     
072600*    03 -COPY WDGX2258                                                    
072700     EJECT                                                                
072800 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2260'.          
072900 01  DLI-IO-WDGX2260.                                                     
073000*    03 -COPY WDGX2260                                                    
073100     EJECT                                                                
073200                                                                          
073300  LINKAGE SECTION.                                                        
073400                                                                          
073500*01  -COPY W0009   -PRE MSG-                                              
073600     EJECT                                                                
073700*01  -COPY W0009   -PRE ALT-                                              
073800     EJECT                                                                
073900*01  -COPY W0008  -PRE ARTC-                                              
074000     05  FILLER                  PIC X.                                   
074100     EJECT                                                                
074200*01  -COPY W0008  -PRE INLB-                                              
074300      05  FILLER                 PIC X(7).                                
074400      05  INLB-KEY-02-IDLEVNR    PIC X(5).                                
074500     EJECT                                                                
074600 01  W222-WDK6-PCB               PIC X.                                   
074700     EJECT                                                                
074800*01  -COPY W0008  -PRE ARTM-                                              
074900     05  FILLER                  PIC X.                                   
075000     EJECT                                                                
075100*01  -COPY W0008  -PRE WDK7-                                              
075200     05  FILLER                  PIC X.                                   
075300     EJECT                                                                
075400*01  -COPY W0008  -PRE INLE-                                              
075500     05  FILLER                  PIC X.                                   
075600     EJECT                                                                
075700 01  W222-2501-PCB               PIC X.                                   
075800     EJECT                                                                
075900 01  W222-WDB6R-PCB              PIC X.                                   
076000     EJECT                                                                
076100 01  W222-WDK7R-PCB              PIC X.                                   
076200     EJECT                                                                
076300*01  -COPY W0008  -PRE WDF1-.                                             
076400     05  FILLER                  PIC X.                                   
076500     EJECT                                                                
076600*01  -COPY W0008  -PRE WDP6-.                                             
076700     05  FILLER                  PIC X.                                   
076800     EJECT                                                                
076900*01  -COPY W0008  -PRE WDF3-.                                             
077000     05  FILLER                  PIC X.                                   
077100     EJECT                                                                
077200 01  W222-WDB6-PCB               PIC X.                                   
077300     EJECT                                                                
077400*01  -COPY W0008  -PRE WDD3-.                                             
077500     05  FILLER                  PIC X.                                   
077600     EJECT                                                                
077700*01  -COPY W0008  -PRE WDD6-.                                             
077800     05  FILLER                  PIC X.                                   
077900     EJECT                                                                
078000*01  -COPY W0008  -PRE WDB6-2-.                                           
078100     05  FILLER                  PIC X.                                   
078200     EJECT                                                                
078300 01  W222-WDD7-PCB               PIC X.                                   
078400     EJECT                                                                
078500 01  W222-WDK7E-PCB              PIC X.                                   
078600     EJECT                                                                
078700 01  W222-UTIL-WDK6-PCB          PIC X.                                   
078800     EJECT                                                                
078900 01  W222-UTIL-WDK7-PCB          PIC X.                                   
079000     EJECT                                                                
079100 01  W222-UTIL-WDB6-PCB          PIC X.                                   
079200     EJECT                                                                
079300 01  W222-UTUP-WDK7-PCB          PIC X.                                   
079400     EJECT                                                                
079500 01  W222-UTUP-WDB6-PCB          PIC X.                                   
079600     EJECT                                                                
079700 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
079800     EJECT                                                                
079900 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
080000     EJECT                                                                
080100 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
080200     EJECT                                                                
080300*01  -COPY W0008  -PRE 2257-.                                             
080400     05  FILLER                  PIC X.                                   
080500     EJECT                                                                
080600 01  BLOC-WDD9-PCB               PIC X.                                   
080700 01  BLOC-WDF3-PCB               PIC X.                                   
080800 01  BLOC-WDR2-PCB               PIC X.                                   
080900 01  BLOC-WDR5-PCB               PIC X.                                   
081000     EJECT                                                                
081100 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB ARTC-PCB  INLB-PCB            
081200                     W222-WDK6-PCB  ARTM-PCB WDK7-PCB INLE-PCB            
081300                     W222-2501-PCB  W222-WDB6R-PCB W222-WDK7R-PCB         
081400                     WDF1-PCB       WDP6-PCB WDF3-PCB                     
081500                     W222-WDB6-PCB  WDD3-PCB WDD6-PCB WDB6-2-PCB          
081600                     W222-WDD7-PCB  W222-WDK7E-PCB                        
081700                     W222-UTIL-WDK6-PCB                                   
081800                     W222-UTIL-WDK7-PCB                                   
081900                     W222-UTIL-WDB6-PCB                                   
082000                     W222-UTUP-WDK7-PCB                                   
082100                     W222-UTUP-WDB6-PCB                                   
082200                     W222-UTUP-UTIL-WDK6-PCB                              
082300                     W222-UTUP-UTIL-WDK7-PCB                              
082400                     W222-UTUP-UTIL-WDB6-PCB                              
082500                     2257-PCB                                             
082600                     BLOC-WDD9-PCB                                        
082700                     BLOC-WDF3-PCB                                        
082800                     BLOC-WDR2-PCB                                        
082900                     BLOC-WDR5-PCB.                                       
083000 MAIN SECTION.                                                            
083100     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB ARTC-PCB  INLB-PCB            
083200                     W222-WDK6-PCB  ARTM-PCB WDK7-PCB INLE-PCB            
083300                     W222-2501-PCB  W222-WDB6R-PCB W222-WDK7R-PCB         
083400                     WDF1-PCB       WDP6-PCB WDF3-PCB                     
083500                     W222-WDB6-PCB  WDD3-PCB WDD6-PCB WDB6-2-PCB          
083600                     W222-WDD7-PCB  W222-WDK7E-PCB                        
083700                     W222-UTIL-WDK6-PCB                                   
083800                     W222-UTIL-WDK7-PCB                                   
083900                     W222-UTIL-WDB6-PCB                                   
084000                     W222-UTUP-WDK7-PCB                                   
084100                     W222-UTUP-WDB6-PCB                                   
084200                     W222-UTUP-UTIL-WDK6-PCB                              
084300                     W222-UTUP-UTIL-WDK7-PCB                              
084400                     W222-UTUP-UTIL-WDB6-PCB                              
084500                     2257-PCB                                             
084600                     BLOC-WDD9-PCB                                        
084700                     BLOC-WDF3-PCB                                        
084800                     BLOC-WDR2-PCB                                        
084900                     BLOC-WDR5-PCB.                                       
085000                                                                          
085100     PERFORM IMS-GET-MSG                                                  
085200     IF SEGMENT-FINNS                                                     
085300       PERFORM A-INIT                                                     
085400       PERFORM B-KOLLA-NYCKLAR                                            
085500       IF NYCKLAR-OK                                                      
085600         IF MFS-UPDATE OR MFS-UPD-X                                       
085700           PERFORM G-KOLLA-INPUT                                          
085800           IF INDATA-OK                                                   
085900             PERFORM H-UPPDATERA                                          
086000           END-IF                                                         
086100         ELSE                                                             
086200           IF MFS-FIRST                                                   
086300             PERFORM C-FOERSTA-SIDA                                       
086400           ELSE                                                           
086500             PERFORM E-SAMMA-SIDA                                         
086600           END-IF                                                         
086700           PERFORM F-LAES-VISA-INFO                                       
086800         END-IF                                                           
086900       END-IF                                                             
087000       IF MFS-UPD-X OR MFS-UPDATE                                         
087100          PERFORM S02-STARTA-2103-TRANS                                   
087200       ELSE                                                               
087300          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
087400          PERFORM IMS-INSERT-MSG                                          
087500       END-IF                                                             
087600     END-IF                                                               
087700                                                                          
087800     MOVE ZERO TO RETURN-CODE                                             
087900     GOBACK                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 A-INIT SECTION.                                                          
088300                                                                          
088400     IF MSG-DUBBLA-TRANSKODER                                             
088500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13501                 
088600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
088700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
088800     ELSE                                                                 
088900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13501                  
089000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
089100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
089200     END-IF                                                               
089300                                                                          
089400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
089500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
089600                                                                          
089700     MOVE LOW-VALUE TO MSG-AREA                                           
089800     MOVE 'W2O13501' TO MFS-IDMOD                                         
089900     MOVE '2135' TO MOD-IDTRANS                                           
090000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
090100                                                                          
090200*    --- OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4               
090300*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17              
090400     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W2O13501 + 4                  
090500                                                                          
090600     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
090700       CONTINUE                                                           
090800     ELSE                                                                 
090900       MOVE SPACE TO MFS-KDTRTYP                                          
091000       MOVE '7' TO MFS-IDPFK                                              
091100     END-IF                                                               
091200                                                                          
091300     IF ENGLISH-TEXT                                                      
091400       MOVE +2 TO SPRAK-IX                                                
091500       MOVE 'GB ' TO MED-IDSKYLT                                          
091600     ELSE                                                                 
091700       MOVE +1 TO SPRAK-IX                                                
091800       MOVE 'S  ' TO MED-IDSKYLT                                          
091900     END-IF                                                               
092000                                                                          
092100     MOVE WC-CDC-SE  TO W-IDDC-MIN                                        
092200                        W-IDDC-MAX                                        
092300                                                                          
092400                                                                          
092500     .                                                                    
092600     EJECT                                                                
092700 B-KOLLA-NYCKLAR SECTION.                                                 
092800                                                                          
092900     MOVE JA TO NYCKLAR-SW                                                
093000                                                                          
093100*    -- KONTROLL AV IDARTNR                                               
093200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
093300                                                                          
093400     IF MID-IDARTNR-IN = ALL '+'                                          
093500       MOVE MID-IDARTNR-UT TO WS-IDARTNR                                  
093600       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
093700     ELSE                                                                 
093800       MOVE MID-IDARTNR-IN TO WS-IDARTNR                                  
093900       IF NOT MFS-UPD-X                                                   
094000          MOVE '7'      TO MFS-IDPFK                                      
094100          MOVE SPACE    TO MFS-KDTRTYP                                    
094200        END-IF                                                            
094300     END-IF                                                               
094400                                                                          
094500     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
094600       MOVE WS-IDARTNR TO W-IDARTNR                                       
094700     ELSE                                                                 
094800       MOVE NEJ TO NYCKLAR-SW                                             
094900     END-IF                                                               
095000                                                                          
095100     IF GODK-MID OR NYCKLAR-OK                                            
095200       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
095300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
095400     ELSE                                                                 
095500       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
095600     END-IF                                                               
095700                                                                          
095800     IF NYCKLAR-FEL                                                       
095900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
096000       CALL WMEDKONV USING MED-WMEDAREA                                   
096100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
096200     END-IF                                                               
096300     .                                                                    
096400     EJECT                                                                
096500 C-FOERSTA-SIDA SECTION.                                                  
096600                                                                          
096700     CONTINUE                                                             
096800     .                                                                    
096900     EJECT                                                                
097000 E-SAMMA-SIDA SECTION.                                                    
097100                                                                          
097200     IF EGEN-MID OR HELP-MID                                              
097300         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
097400         CALL WMEDKONV USING MED-WMEDAREA                                 
097500         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
097600     END-IF                                                               
097700     .                                                                    
097800     EJECT                                                                
097900 F-LAES-VISA-INFO SECTION.                                                
098000                                                                          
098100     PERFORM FA-LAES-GRUNDDATA                                            
098200                                                                          
098300     IF SEGMENT-SAKNAS                                                    
098400        CALL WMEDKONV USING MED-WMEDAREA                                  
098500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 FA-LAES-GRUNDDATA SECTION.                                               
099000                                                                          
099100     MOVE WS-IDARTNR    TO W-IDARTNR-INLB                                 
099200     MOVE WC-CDC-SE     TO W-IDDC-INLB                                    
099300     PERFORM IMS-GET-INLB-WLINLB01                                        
099400     IF SEGMENT-SAKNAS                                                    
099500        MOVE '005'      TO MED-IDMFSFEL                                   
099600        CALL WMEDKONV USING MED-WMEDAREA                                  
099700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100 G-KOLLA-INPUT SECTION.                                                   
100200                                                                          
100300     MOVE JA  TO INDATA-SW                                                
100400                                                                          
100500     MOVE WS-IDARTNR    TO W-IDARTNR-INLB                                 
100600     MOVE WC-CDC-SE     TO W-IDDC-INLB                                    
100700     PERFORM IMS-GET-INLB-WLINLB01                                        
100800     IF SEGMENT-FINNS                                                     
100900        PERFORM IMS-GET-ARTC-WLARTC01                                     
101000        IF SEGMENT-SAKNAS                                                 
101100           MOVE '303'      TO MED-IDMFSFEL                                
101200           CALL WMEDKONV USING MED-WMEDAREA                               
101300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
101400           MOVE NEJ TO INDATA-SW                                          
101500        ELSE                                                              
101600           MOVE ART-IDLEVNR  TO W-IDLEVNR                                 
101700           MOVE ART-KDPRODSL TO W-KDPRODSL                                
101800        END-IF                                                            
101900     ELSE                                                                 
102000        MOVE '005'      TO MED-IDMFSFEL                                   
102100        CALL WMEDKONV USING MED-WMEDAREA                                  
102200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
102300        MOVE NEJ TO INDATA-SW                                             
102400     END-IF                                                               
102500     .                                                                    
102600     EJECT                                                                
102700 H-UPPDATERA SECTION.                                                     
102800                                                                          
102900     PERFORM HA-TAG-BORT-OMSPEC-AVROP                                     
103000                                                                          
103100     PERFORM HB-BERAKNA-NYA-PUNKTER                                       
103200                                                                          
103300     PERFORM HC-KOLLA-OM-KOEP                                             
103400                                                                          
103500     PERFORM HD-OMSPEC                                                    
103600                                                                          
103700     PERFORM HE-UPPDATERA-ARTC                                            
103800                                                                          
103900     PERFORM HF-SKAPA-OMSPEC-SEGMENT                                      
104000                                                                          
104100     PERFORM HG-SKAPA-FOERSLAGSPOST-PA-KOE                                
104200                                                                          
104300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
104400     CALL WMEDKONV USING MED-WMEDAREA                                     
104500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
104600     .                                                                    
104700     EJECT                                                                
104800 HA-TAG-BORT-OMSPEC-AVROP SECTION.                                        
104900                                                                          
105000     PERFORM IMS-GET-INLB-WLINLB22                                        
105100     IF SEGMENT-FINNS                                                     
105200        PERFORM IMS-DLET-INLB-WLINLB22                                    
105300     END-IF                                                               
105400                                                                          
105500     MOVE +1  TO W-KDAVROP                                                
105600     PERFORM IMS-GET-INLB-WLINLB23-FIRST                                  
105700     PERFORM UNTIL SEGMENT-SAKNAS                                         
105800       PERFORM IMS-DLET-INLB-WLINLB23                                     
105900       PERFORM IMS-GET-INLB-WLINLB23-NEXT                                 
106000     END-PERFORM                                                          
106100     .                                                                    
106200     EJECT                                                                
106300 HB-BERAKNA-NYA-PUNKTER SECTION.                                          
106400                                                                          
106500     PERFORM HBA-SKAPA-LANKAREA                                           
106600                                                                          
106700     PERFORM HBB-SKAPA-BEHOVSTABELL                                       
106800                                                                          
106900     IF PUNK-KDERS (1) > 20                                               
107000         MOVE +0 TO PUNK-KVPB-SEP (1)                                     
107100         MOVE +0 TO PUNK-KVPB-SEP (2)                                     
107200     END-IF                                                               
107300                                                                          
107400     CALL W221PUNK USING PUNK-W221PUNK LINK-AREA WDP6-PCB                 
107500                                                 WDF1-PCB                 
107600     PERFORM HBC-UPPDATERA-ARTC                                           
107700     .                                                                    
107800     EJECT                                                                
107900 HBA-SKAPA-LANKAREA      SECTION.                                         
108000                                                                          
108100     MOVE 'IDAG  '               TO DAT-KDDATFORM                         
108200                                                                          
108300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
108400                         DAT-O-TIDATUM DAT-KDSVAR                         
108500                                                                          
108600     MOVE DAT-TIAA               TO W-DATUM-AA W-DATUM2-AA                
108700     MOVE DAT-TIMM               TO W-DATUM2-MM                           
108800     MOVE DAT-TIDD               TO W-DATUM2-DD                           
108900     MOVE DAT-TIVV               TO W-DATUM-VV                            
109000     MOVE DAT-TID                TO W-DAT-TID-AKT                         
109100     MOVE W-DATUM-AAVV           TO PUNK-TIAAVV-AKT                       
109200                                    PUNK-TIAAVVD-AKT                      
109300                                    W-DATUM-AAVV-S                        
109400                                    W-DATUM-AAVVD                         
109500                                    W-DATUM-AAVV-AKT                      
109600                                    AKT-DATUM-AAVV                        
109700     MULTIPLY 10 BY PUNK-TIAAVVD-AKT                                      
109800     MULTIPLY 10 BY W-DATUM-AAVVD                                         
109900                                                                          
110000     MOVE ART-IDARTNR            TO PUNK-IDARTNR                          
110100     MOVE ART-TIFINLV            TO PUNK-TIFINLV                          
110200     MOVE ART-KDPRODSL           TO PUNK-KDPRODSL                         
110300     MOVE ART-IDLEVNR            TO PUNK-IDLEVNR                          
110400     MOVE ZERO                   TO PUNK-KVOEKORR                         
110500     MOVE +0                     TO PUNK-KDLPORS-TAB (1)                  
110600                                    PUNK-KDLPORS-TAB (2)                  
110700                                    PUNK-KDLPORS-TAB (3)                  
110800                                                                          
110900     PERFORM IMS-GET-ARTC-WLARTC11                                        
111000                                                                          
111100     PERFORM HBAA-BEHANDLA-ARTC11                                         
111200                                                                          
111300     PERFORM HBAB-BEHANDLA-ARTC21                                         
111400                                                                          
111500     PERFORM HBAC-BEHANDLA-ARTC26                                         
111600                                                                          
111700     PERFORM HBAF-BEHANDLA-ARTM01                                         
111800                                                                          
111900     PERFORM HBAG-BEHANDLA-INLB11                                         
112000                                                                          
112100     PERFORM HBAH-BEHANDLA-SDC                                            
112200     .                                                                    
112300     EJECT                                                                
112400 HBAA-BEHANDLA-ARTC11 SECTION.                                            
112500                                                                          
112600     MOVE ART-KDPRODSL           TO W-KDPRODSL                            
112700     MOVE W-IDPROD               TO PUNK-IDPROD                           
112800     MOVE CLAG-FLAVRART          TO PUNK-FLAVRART                         
112900     MOVE CLAG-FLMANBK           TO PUNK-FLMANBK                          
113000     MOVE CLAG-FLMANKP           TO PUNK-FLMANKP                          
113100     MOVE CLAG-KDHF              TO PUNK-KDHF                             
113200     MOVE CLAG-KVPALL            TO PUNK-KVPALL                           
113300                                                                          
113400     MOVE CLAG-FLMANQ            TO PUNK-FLMANQ                           
113500     MOVE CLAG-KDAVT             TO PUNK-KDAVT                            
113600     MOVE CLAG-KDFREKKL          TO PUNK-KDFREKKL                         
113700     MOVE CLAG-KDPRISKL          TO PUNK-KDPRISKL                         
113800     MOVE CLAG-KDVVKL            TO PUNK-KDVVKL                           
113900     MOVE CLAG-KVAP              TO PUNK-KVAP                             
114000     MOVE CLAG-KVBK              TO PUNK-KVBK                             
114100     MOVE CLAG-KVKP              TO PUNK-KVKP                             
114200     MOVE CLAG-KVQ               TO PUNK-KVQ                              
114300     MOVE CLAG-KVQ-JUST          TO PUNK-KVQ-JUST                         
114400     MOVE CLAG-KVVECKOR-FT       TO PUNK-KVVECKOR-FT                      
114500     MOVE CLAG-KVVECKOR-BT       TO PUNK-KVVECKOR-BT                      
114600     MOVE CLAG-TIQJUST           TO PUNK-TIQJUST                          
114700                                                                          
114800     MOVE +1 TO W-CL-IX                                                   
114900        MOVE SPACE                     TO PUNK-FLFSP(W-CL-IX)             
115000        MOVE ZERO                      TO PUNK-REDIRLEV(W-CL-IX)          
115100                                          PUNK-KVPB-SATS(W-CL-IX)         
115200                                          PUNK-KVPB-TPO(W-CL-IX)          
115300                                          PUNK-KVPB-VESL(W-CL-IX)         
115400                                         W-SPAR-REDIRLEV(W-CL-IX)         
115500                                                                          
115600        MOVE SPACE                     TO PUNK-FLMPB(W-CL-IX)             
115700        MOVE ZERO                      TO PUNK-KVMAD-SEP(W-CL-IX)         
115800                                          PUNK-KVMAD-TOT(W-CL-IX)         
115900                                          PUNK-KVMP(W-CL-IX)              
116000                                          PUNK-KVPB-SEP(W-CL-IX)          
116100                                          PUNK-RESLJUST(W-CL-IX)          
116200                                          PUNK-TISLJUST(W-CL-IX)          
116300                                          W-SPAR-RESEASON(W-CL-IX)        
116400                                                                          
116500        MOVE +1                        TO W-CL-IX                         
116600                                          PUNK-KVANTAL-CLAGER             
116700        MOVE SPACE                     TO PUNK-FLFSP(W-CL-IX)             
116800        MOVE CLAG-REDIRLEV             TO PUNK-REDIRLEV(W-CL-IX)          
116900                                         W-SPAR-REDIRLEV(W-CL-IX)         
117000        MOVE CLAG-KVPB-SATS            TO PUNK-KVPB-SATS(W-CL-IX)         
117100        MOVE CLAG-KVPB-TPO             TO PUNK-KVPB-TPO(W-CL-IX)          
117200        MOVE CLAG-KVPB-VESL            TO PUNK-KVPB-VESL(W-CL-IX)         
117300                                                                          
117400        MOVE CLAG-FLMPB                TO PUNK-FLMPB(W-CL-IX)             
117500        MOVE CLAG-KVMAD-SEP            TO PUNK-KVMAD-SEP(W-CL-IX)         
117600        MOVE CLAG-KVMAD-TOT            TO PUNK-KVMAD-TOT(W-CL-IX)         
117700        MOVE CLAG-KVMP                 TO PUNK-KVMP(W-CL-IX)              
117800        MOVE CLAG-KVPB-SEP             TO PUNK-KVPB-SEP(W-CL-IX)          
117900        MOVE CLAG-RESLJUST             TO PUNK-RESLJUST(W-CL-IX)          
118000        MOVE CLAG-TISLJUST             TO PUNK-TISLJUST(W-CL-IX)          
118100                                                                          
118200     MOVE CLAG-PRARTSTD          TO PUNK-PRARTSTD                         
118300                                                                          
118400     MOVE CLAG-KDGK              TO PUNK-KDGK                             
118500     MOVE CLAG-KVQPACK-1         TO PUNK-KVQPACK-1                        
118600     MOVE CLAG-KDUART            TO PUNK-KDUART                           
118700                                                                          
118800     MOVE CLAG-KDLTK             TO PUNK-KDLTK                            
118900                                                                          
119000     MOVE +1 TO W-CL-IX                                                   
119100        MOVE ZERO                      TO PUNK-KDERS(W-CL-IX)             
119200                                          PUNK-KVAKS(W-CL-IX)             
119300                                          PUNK-KVLS(W-CL-IX)              
119400                                          PUNK-KVRESS(W-CL-IX)            
119500                                          PUNK-KVROS(W-CL-IX)             
119600                                          PUNK-KVSLAGER(W-CL-IX)          
119700                                                                          
119800        MOVE CLAG-KDERS                TO PUNK-KDERS(W-CL-IX)             
119900        MOVE CLAG-KVAKS-CDC            TO PUNK-KVAKS(W-CL-IX)             
120000                                          W-SPAR-KVAKS(W-CL-IX)           
120100        ADD  CLAG-KVAKS-PAV            TO PUNK-KVAKS(W-CL-IX)             
120200                                          W-SPAR-KVAKS(W-CL-IX)           
120300        ADD  CLAG-KVAKS-T              TO PUNK-KVAKS(W-CL-IX)             
120400                                          W-SPAR-KVAKS(W-CL-IX)           
120500                                                                          
120600        IF W-SPAR-KVAKS(W-CL-IX)  > ZERO                                  
120700           PERFORM HBAAA-JUSTERA-MED-RETURER                              
120800        END-IF                                                            
120900                                                                          
121000        MOVE CLAG-KVLS                 TO PUNK-KVLS(W-CL-IX)              
121100                                          W-SPAR-KVLS(W-CL-IX)            
121200        MOVE CLAG-KVRESS               TO PUNK-KVRESS(W-CL-IX)            
121300                                          W-SPAR-KVRESS(W-CL-IX)          
121400        MOVE CLAG-KVROS                TO PUNK-KVROS(W-CL-IX)             
121500                                          W-SPAR-KVROS(W-CL-IX)           
121600        MOVE CLAG-KVSLAGER             TO PUNK-KVSLAGER(W-CL-IX)          
121700        MOVE CLAG-KVRETUR              TO W-SPAR-KVRETUR(W-CL-IX)         
121800*EOQ                                                                      
121900        MOVE CLAG-ADLAGOMR             TO PUNK-ADLAGOMR                   
122000        MOVE CLAG-BEFT                 TO PUNK-BEFT                       
122100        MOVE CLAG-FLNYBER              TO PUNK-FLNYBER                    
122200        MOVE CLAG-KVULOAD              TO PUNK-KVULOAD                    
122300        MOVE CLAG-PRORDSK              TO PUNK-PRORDSK                    
122400        MOVE CLAG-VLARTNTO             TO PUNK-VLARTNTO                   
122500        MOVE CLAG-KVEOQ                TO PUNK-KVEOQ                      
122600        MOVE CLAG-KVSLAGER-OPT         TO PUNK-KVSLAGER-OPT               
122700        IF CLAG-KVVECKOR-LVAR NUMERIC                                     
122800           MOVE CLAG-KVVECKOR-LVAR     TO PUNK-KVVECKOR-LVAR              
122900        ELSE                                                              
123000           MOVE ZERO                   TO PUNK-KVVECKOR-LVAR              
123100        END-IF                                                            
123200*EOQ                                                                      
123300        IF CLAG-DAPBPLAN > ZERO                                           
123400*                                              MANUELL KVPB-PLAN          
123500           MOVE CLAG-KVPB-PLAN         TO PUNK-KVPB-PLAN                  
123600        ELSE                                                              
123700*                                            MASKINELL KVPB-PLAN          
123800           MOVE ART-IDARTNR TO PBTO-IDARTNR                               
123900           CALL W222PBTO USING PBTO-W222PBTO                              
124000                               W222-WDK6-PCB                              
124100                               WDK7-PCB                                   
124200                               ARTM-PCB                                   
124300                               W222-2501-PCB                              
124400                               W222-WDB6R-PCB                             
124500                               W222-WDK7R-PCB                             
124600                               W222-WDB6-PCB                              
124700                               W222-WDD7-PCB                              
124800                               W222-WDK7E-PCB                             
124900                               W222-UTIL-WDK6-PCB                         
125000                               W222-UTIL-WDK7-PCB                         
125100                               W222-UTIL-WDB6-PCB                         
125200                               W222-UTUP-WDK7-PCB                         
125300                               W222-UTUP-WDB6-PCB                         
125400                               W222-UTUP-UTIL-WDK6-PCB                    
125500                               W222-UTUP-UTIL-WDK7-PCB                    
125600                               W222-UTUP-UTIL-WDB6-PCB                    
125700                                                                          
125800           IF PBTO-KDSVAR = JA                                            
125900              MOVE PBTO-KVPB-PLAN      TO PUNK-KVPB-PLAN                  
126000           ELSE                                                           
126100              MOVE ZERO                TO PUNK-KVPB-PLAN                  
126200           END-IF                                                         
126300        END-IF                                                            
126400*                                      SPARA IDANSK FÖR WDD601            
126500     MOVE CLAG-IDANSK                  TO W-IDANSK                        
126600     .                                                                    
126700     EJECT                                                                
126800 HBAAA-JUSTERA-MED-RETURER SECTION.                                       
126900                                                                          
127000     PERFORM IMS-GET-INLE01-WDL201                                        
127100     IF SEGMENT-FINNS                                                     
127200        MOVE ZERO TO SUM-RETUR                                            
127300        PERFORM IMS-GET-INLE21-WDL221                                     
127400        PERFORM UNTIL SEGMENT-SAKNAS                                      
127500           IF MOT-KDRT = 7 OR 77                                          
127600              COMPUTE SUM-RETUR = SUM-RETUR +                             
127700                      MOT-KVAVIS - MOT-KVANTMOT                           
127800           END-IF                                                         
127900           PERFORM IMS-GET-INLE21-WDL221                                  
128000        END-PERFORM                                                       
128100        IF SUM-RETUR < ZERO                                               
128200           MOVE ZERO TO SUM-RETUR                                         
128300        END-IF                                                            
128400        SUBTRACT SUM-RETUR           FROM PUNK-KVAKS(W-CL-IX)             
128500                                          W-SPAR-KVAKS(W-CL-IX)           
128600        IF PUNK-KVAKS(W-CL-IX) < ZERO                                     
128700           MOVE ZERO TO PUNK-KVAKS(W-CL-IX)                               
128800        END-IF                                                            
128900        IF W-SPAR-KVAKS(W-CL-IX) < ZERO                                   
129000           MOVE ZERO TO W-SPAR-KVAKS(W-CL-IX)                             
129100        END-IF                                                            
129200     END-IF                                                               
129300     .                                                                    
129400     EJECT                                                                
129500                                                                          
129600 HBAB-BEHANDLA-ARTC21 SECTION.                                            
129700     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
129800     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
129900     PERFORM IMS-GET-ARTC-WLARTC21                                        
130000     IF SEGMENT-SAKNAS                                                    
130100       MOVE CLAG-PRARTSTD       TO PUNK-PRARTBES                          
130200     ELSE                                                                 
130300       MOVE NEJ                 TO FL-PRARTBES                            
130400       PERFORM UNTIL  SEGMENT-SAKNAS                                      
130500         IF PRL-SUINLEV-PR > ZERO                                         
130600           MOVE PRL-PRARTBES-PR  TO PUNK-PRARTBES                         
130700           SET SEGMENT-SAKNAS TO TRUE                                     
130800         ELSE                                                             
130900           IF FL-PRARTBES = NEJ                                           
131000             MOVE PRL-PRARTBES-PR TO PUNK-PRARTBES                        
131100             MOVE JA              TO FL-PRARTBES                          
131200           END-IF                                                         
131300           PERFORM IMS-GET-ARTC-WLARTC21                                  
131400         END-IF                                                           
131500       END-PERFORM                                                        
131600     END-IF                                                               
131700     .                                                                    
131800     EJECT                                                                
131900 HBAC-BEHANDLA-ARTC26 SECTION.                                            
132000                                                                          
132100     PERFORM IMS-GET-ARTC-WLARTC26                                        
132200     IF SEGMENT-FINNS                                                     
132300        MOVE JUST-RESEASON(DAT-TIPP) TO W-SPAR-RESEASON(W-CL-IX)          
132400     ELSE                                                                 
132500        MOVE 1                       TO W-SPAR-RESEASON(W-CL-IX)          
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900 HBAF-BEHANDLA-ARTM01 SECTION.                                            
133000                                                                          
133100     PERFORM IMS-GET-ARTM-WLARTM01                                        
133200     IF SEGMENT-FINNS                                                     
133300        MOVE ARTM01-ART-KVOKS-BULK    TO PUNK-KVOKS-BULK(1)               
133400        MOVE ARTM01-ART-KVOKS-DAG     TO PUNK-KVOKS-DAG(1)                
133500        MOVE ARTM01-ART-KVOKS-VOR     TO PUNK-KVOKS-VOR(1)                
133600     ELSE                                                                 
133700        MOVE ZERO                     TO ARTM01-ART-KVOKS-BULK            
133800                                         PUNK-KVOKS-BULK(1)               
133900                                         ARTM01-ART-KVOKS-DAG             
134000                                         PUNK-KVOKS-DAG(1)                
134100                                         ARTM01-ART-KVOKS-VOR             
134200                                         PUNK-KVOKS-VOR(1)                
134300     END-IF                                                               
134400     .                                                                    
134500     EJECT                                                                
134600                                                                          
134700                                                                          
134800 HBAG-BEHANDLA-INLB11 SECTION.                                            
134900                                                                          
135000     MOVE ZERO                         TO PUNK-KVBR-TOT                   
135100                                          W-SPAR-KVBR-TOT                 
135200                                          W-SPAR-KVBR                     
135300     PERFORM IMS-GET-INLB-WLINLB11-FIRST                                  
135400     PERFORM UNTIL SEGMENT-SAKNAS                                         
135500        IF INLB11-IDLEVNR = ART-IDLEVNR                                   
135600           MOVE INLB11-KVBR            TO W-SPAR-KVBR                     
135700        END-IF                                                            
135800        ADD INLB11-KVBR                TO PUNK-KVBR-TOT                   
135900                                          W-SPAR-KVBR-TOT                 
136000        PERFORM IMS-GET-INLB-WLINLB11-NEXT                                
136100     END-PERFORM                                                          
136200     .                                                                    
136300     EJECT                                                                
136400                                                                          
136500 HBAH-BEHANDLA-SDC    SECTION.                                            
136600                                                                          
136700     MOVE ZERO              TO   W-KVPB-SDC-TOT                           
136800                                 W-KVPB-SDC-EJ-DIR                        
136900                                 W-TILLG-SDC                              
137000                                 W-OVERLAGER-SDC                          
137100                                                                          
137200     PERFORM IMS-GET-WDK701-SDC                                           
137300                                                                          
137400**** SKALL BARA LÄSA DE MED IDDC-REF = 11                                 
137500     IF SEGMENT-FINNS                                                     
137600        PERFORM IMS-GNP-WDK711-SDC-REF                                    
137700        PERFORM UNTIL SEGMENT-SAKNAS                                      
137800                                                                          
137900           ADD SLAG-KVPB-REF     TO W-KVPB-SDC-TOT                        
138000           IF SLAG-FLCDCBEH = JA                                          
138100               ADD SLAG-KVPB-REF TO W-KVPB-SDC-EJ-DIR                     
138200               ADD SLAG-KVPBREOI TO W-KVPB-SDC-EJ-DIR                     
138300           END-IF                                                         
138400           MOVE ZERO          TO W-TILLG-SDC                              
138500           ADD SLAG-KVLS      TO W-TILLG-SDC                              
138600           ADD SLAG-KVBEART   TO W-TILLG-SDC                              
138700           ADD SLAG-KVAKS-SDC TO W-TILLG-SDC                              
138800           ADD SLAG-KVAKS-PAV TO W-TILLG-SDC                              
138900                                                                          
139000**** FIX FÖR NEGATIVA KVOKS,SE W2215000                                   
139100           IF SLAG-KVOKS-BULK > 0                                         
139200             SUBTRACT SLAG-KVOKS-BULK FROM W-TILLG-SDC                    
139300           END-IF                                                         
139400           IF SLAG-KVOKS-DAG > 0                                          
139500             SUBTRACT SLAG-KVOKS-DAG  FROM W-TILLG-SDC                    
139600           END-IF                                                         
139700                                                                          
139800           MOVE SLAG-IDDC     TO WS-IDDC                                  
139900           IF NDC                                                         
140000              CONTINUE                                                    
140100           ELSE                                                           
140200             MOVE SLAG-IDDC  TO W-IDDC-B6                                 
140300             PERFORM IMS-GU-WDB601                                        
140400             IF 2-DCS-SDC                                                 
140500               IF 2-DCS-FLOVRLAGBER = NEJ                                 
140600                 CONTINUE                                                 
140700               ELSE                                                       
140800                 IF SLAG-KVREFOVL < W-TILLG-SDC                           
140900                   COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC              
141000                                           + W-TILLG-SDC                  
141100                                           - SLAG-KVREFOVL                
141200                 END-IF                                                   
141300               END-IF                                                     
141400             END-IF                                                       
141500           END-IF                                                         
141600           MOVE ART-TIFINLV TO TIFINLV-AAVVD                              
141700           MOVE AKT-DATUM-AA   TO TMP1-YY                                 
141800           MOVE TIFINLV-AA     TO TMP2-YY                                 
141900           PERFORM WY2000P9                                               
142000           COMPUTE VECKO-DIFF = (TMP1-YY - TMP2-YY) * 52                  
142100                              + AKT-DATUM-VV - TIFINLV-VV                 
142200           IF VECKO-DIFF < 52                                             
142300              MOVE ZERO TO W-OVERLAGER-SDC                                
142400           END-IF                                                         
142500                                                                          
142600           PERFORM IMS-GNP-WDK711-SDC-REF                                 
142700        END-PERFORM                                                       
142800     END-IF                                                               
142900                                                                          
143000     MOVE W-OVERLAGER-SDC TO PUNK-KVLS-SDC-OVER                           
143100                                                                          
143200     MOVE W-KVPB-SDC-TOT  TO PUNK-KVPB-SDC-TOT                            
143300     MOVE W-KVPB-SDC-EJ-DIR                                               
143400                          TO PUNK-KVPB-SDC-EJ-DIR                         
143500     .                                                                    
143600     EJECT                                                                
143700 HBB-SKAPA-BEHOVSTABELL  SECTION.                                         
143800                                                                          
143900     MOVE ART-IDARTNR           TO LINK-IDARTNR                           
144000     MOVE DAT-TID               TO LINK-TID-AKTUELL                       
144100     MOVE SPACE                 TO LINK-IDDC                              
144200     MOVE SEP-SATS-TPO-LEV-SDC-NDC                                        
144300                                TO LINK-KDBEHOV                           
144400     MOVE CLAG-KVVECKOR-BT      TO LINK-KVVECKOR-BEHOV                    
144500     ADD 62                     TO LINK-KVVECKOR-BEHOV                    
144600     IF  LINK-KVVECKOR-BEHOV   > 156                                      
144700         MOVE 156               TO LINK-KVVECKOR-BEHOV                    
144800     END-IF                                                               
144900     MOVE W-DATUM-AAVV          TO LINK-TIAAVV-AKTUELL                    
145000                                   LINK-TIBEHOV-START                     
145100     MOVE 1                     TO W-ANTAL-VECKOR                         
145200     CALL W009VADD USING LINK-TIBEHOV-START  W-ANTAL-VECKOR               
145300     MOVE NEJ                   TO LINK-FLINKLDIRLEV                      
145400                                                                          
145500     IF ART-KDERS-UTG = +0                                                
145600        CALL W22222 USING LINK-AREA W222-WDK6-PCB WDK7-PCB                
145700                          ARTM-PCB  W222-2501-PCB W222-WDB6R-PCB          
145800                          W222-WDK7R-PCB W222-WDB6-PCB                    
145900                          W222-WDD7-PCB W222-WDK7E-PCB                    
146000                          W222-UTIL-WDK6-PCB                              
146100                          W222-UTIL-WDK7-PCB                              
146200                          W222-UTIL-WDB6-PCB                              
146300                          W222-UTUP-WDK7-PCB                              
146400                          W222-UTUP-WDB6-PCB                              
146500                          W222-UTUP-UTIL-WDK6-PCB                         
146600                          W222-UTUP-UTIL-WDK7-PCB                         
146700                          W222-UTUP-UTIL-WDB6-PCB                         
146800                                                                          
146900        IF LINK-ANROP-FEL                                                 
147000           PERFORM S04-NOLLA-W22222                                       
147100        END-IF                                                            
147200     ELSE                                                                 
147300        PERFORM S04-NOLLA-W22222                                          
147400     END-IF                                                               
147500     .                                                                    
147600     EJECT                                                                
147700 HBC-UPPDATERA-ARTC SECTION.                                              
147800                                                                          
147900     PERFORM IMS-GET-ARTC-WLARTC11                                        
148000                                                                          
148100     PERFORM HBCA-UPPDATERA-ARTC11                                        
148200                                                                          
148300     PERFORM IMS-REPL-ARTC-WLARTC11                                       
148400     .                                                                    
148500     EJECT                                                                
148600 HBCA-UPPDATERA-ARTC11 SECTION.                                           
148700                                                                          
148800     MOVE PUNK-FLMANQ        TO CLAG-FLMANQ                               
148900     MOVE PUNK-KDAVT         TO CLAG-KDAVT                                
149000     MOVE PUNK-KDFREKKL      TO CLAG-KDFREKKL                             
149100     MOVE PUNK-KDPRISKL      TO CLAG-KDPRISKL                             
149200     MOVE PUNK-KDVVKL        TO CLAG-KDVVKL                               
149300     MOVE PUNK-KVAP          TO CLAG-KVAP                                 
149400     MOVE PUNK-KVBK          TO CLAG-KVBK                                 
149500     MOVE PUNK-KVKP          TO CLAG-KVKP                                 
149600     MOVE PUNK-KVQ           TO CLAG-KVQ                                  
149700     MOVE PUNK-KVQ-JUST      TO CLAG-KVQ-JUST                             
149800     MOVE PUNK-KVVECKOR-FT   TO CLAG-KVVECKOR-FT                          
149900     MOVE PUNK-KVVECKOR-BT   TO CLAG-KVVECKOR-BT                          
150000     MOVE PUNK-TIQJUST       TO CLAG-TIQJUST                              
150100                                                                          
150200     MOVE ZERO               TO  W-SPAR-TOT-KVPB-SEP                      
150300     MOVE +1 TO W-CL-IX                                                   
150400        MOVE ZERO                    TO W-SPAR-KVPB-SEP(W-CL-IX)          
150500                                        W-SPAR-KVPB-SATS(W-CL-IX)         
150600                                                                          
150700        MOVE PUNK-FLMPB(W-CL-IX)     TO CLAG-FLMPB                        
150800        MOVE PUNK-KVMAD-SEP(W-CL-IX) TO CLAG-KVMAD-SEP                    
150900        MOVE PUNK-KVMAD-TOT(W-CL-IX) TO CLAG-KVMAD-TOT                    
151000        MOVE PUNK-KVMP(W-CL-IX)      TO CLAG-KVMP                         
151100        MOVE PUNK-KVPB-SEP(W-CL-IX)  TO CLAG-KVPB-SEP                     
151200                                        W-SPAR-KVPB-SEP(W-CL-IX)          
151300                                                                          
151400        MOVE PUNK-RESLJUST(W-CL-IX)  TO CLAG-RESLJUST                     
151500        MOVE PUNK-TISLJUST(W-CL-IX)  TO CLAG-TISLJUST                     
151600*EOQ                                                                      
151700********MOVE PUNK-KVULOAD            TO CLAG-KVULOAD                      
151800        MOVE PUNK-KVEOQ              TO CLAG-KVEOQ                        
151900        MOVE PUNK-KVSLAGER-OPT       TO CLAG-KVSLAGER-OPT                 
152000*EOQ                                                                      
152100        MOVE CLAG-KVPB-SATS          TO W-SPAR-KVPB-SATS(W-CL-IX)         
152200                                                                          
152300     MOVE PUNK-KDLTK         TO CLAG-KDLTK                                
152400                                                                          
152500     MOVE +1 TO W-CL-IX                                                   
152600        MOVE ZERO                      TO W-SPAR-KVAKS(W-CL-IX)           
152700                                          W-SPAR-KVLS(W-CL-IX)            
152800                                          W-SPAR-KVRESS(W-CL-IX)          
152900                                          W-SPAR-KVROS(W-CL-IX)           
153000                                          W-SPAR-KVRETUR(W-CL-IX)         
153100                                                                          
153200        MOVE PUNK-KVSLAGER(W-CL-IX) TO CLAG-KVSLAGER                      
153300                                       W-SPAR-KVSLAGER(W-CL-IX)           
153400                                                                          
153500        MOVE CLAG-KVAKS-CDC            TO W-SPAR-KVAKS(W-CL-IX)           
153600        ADD  CLAG-KVAKS-PAV            TO W-SPAR-KVAKS(W-CL-IX)           
153700        ADD  CLAG-KVAKS-T              TO W-SPAR-KVAKS(W-CL-IX)           
153800                                                                          
153900        IF W-SPAR-KVAKS(W-CL-IX) > ZERO                                   
154000           PERFORM HBCAA-JUSTERA-MED-RETURER                              
154100        END-IF                                                            
154200                                                                          
154300        MOVE CLAG-KVLS                 TO W-SPAR-KVLS(W-CL-IX)            
154400        MOVE CLAG-KVRESS               TO W-SPAR-KVRESS(W-CL-IX)          
154500        MOVE CLAG-KVROS                TO W-SPAR-KVROS(W-CL-IX)           
154600        MOVE CLAG-KVRETUR              TO W-SPAR-KVRETUR(W-CL-IX)         
154700                                                                          
154800     .                                                                    
154900     EJECT                                                                
155000 HBCAA-JUSTERA-MED-RETURER SECTION.                                       
155100                                                                          
155200     PERFORM IMS-GET-INLE01-WDL201                                        
155300     IF SEGMENT-FINNS                                                     
155400        MOVE ZERO TO SUM-RETUR                                            
155500        PERFORM IMS-GET-INLE21-WDL221                                     
155600        PERFORM UNTIL SEGMENT-SAKNAS                                      
155700           IF MOT-KDRT = 7 OR 77                                          
155800              COMPUTE SUM-RETUR = SUM-RETUR +                             
155900                      MOT-KVAVIS - MOT-KVANTMOT                           
156000           END-IF                                                         
156100           PERFORM IMS-GET-INLE21-WDL221                                  
156200        END-PERFORM                                                       
156300        IF SUM-RETUR < ZERO                                               
156400           MOVE ZERO TO SUM-RETUR                                         
156500        END-IF                                                            
156600        SUBTRACT SUM-RETUR           FROM W-SPAR-KVAKS(W-CL-IX)           
156700        IF W-SPAR-KVAKS(W-CL-IX) < ZERO                                   
156800           MOVE ZERO TO W-SPAR-KVAKS(W-CL-IX)                             
156900        END-IF                                                            
157000     END-IF                                                               
157100     .                                                                    
157200     EJECT                                                                
157300                                                                          
157400 HC-KOLLA-OM-KOEP SECTION.                                                
157500                                                                          
157600     IF CLAG-KDAVT NOT = 0                                                
157700       PERFORM HCA-BERAKNA-TILLG                                          
157800       IF W-KVANTAL-TILLG < CLAG-KVKP                                     
157900          PERFORM HCB-BERAKN-FORSLAG-TILL-KOEP                            
158000       END-IF                                                             
158100     END-IF                                                               
158200     .                                                                    
158300     EJECT                                                                
158400                                                                          
158500 HCA-BERAKNA-TILLG SECTION.                                               
158600     SKIP3                                                                
158700     COMPUTE W-KVANTAL-TILLG = W-SPAR-KVLS (1)                            
158800     -   W-SPAR-KVRESS (1)                                                
158900     +   W-SPAR-KVAKS  (1)                                                
159000     -   W-SPAR-KVROS  (1)                                                
159100     -   (ARTM01-ART-KVOKS-BULK     + ARTM01-ART-KVOKS-DAG                
159200                                    + ARTM01-ART-KVOKS-VOR )              
159300     +   CLAG-KVLAAN                                                      
159400     +   W-SPAR-KVBR-TOT                                                  
159500     +   W-OVERLAGER-SDC                                                  
159600                                                                          
159700     MOVE ART-IDARTNR TO TEST-IDARTNR                                     
159800     .                                                                    
159900     EJECT                                                                
160000                                                                          
160100 HCB-BERAKN-FORSLAG-TILL-KOEP SECTION.                                    
160200     SKIP1                                                                
160300     COMPUTE W-KVPB = W-SPAR-KVPB-SEP  (1)                                
160400                   +  W-SPAR-KVPB-SATS (1)                                
160500     SKIP1                                                                
160600     IF  W-KVPB = ZERO                                                    
160700         COMPUTE W-KVANTAL-KOEP = CLAG-KVKP - W-KVANTAL-TILLG             
160800     ELSE                                                                 
160900         IF  CLAG-KVQ  > ZERO                                             
161000             COMPUTE W-N = 0.99 + (CLAG-KVKP - CLAG-KVBK / 2              
161100                                  -  W-KVANTAL-TILLG)                     
161200                                  /  CLAG-KVQ                             
161300             COMPUTE W-KVANTAL-KOEP = CLAG-KVBK +                         
161400                                      W-N * CLAG-KVQ                      
161500         ELSE                                                             
161600             COMPUTE W-KVANTAL-KOEP = CLAG-KVBK / 2                       
161700                                     + CLAG-KVKP                          
161800                                     - W-KVANTAL-TILLG                    
161900         END-IF                                                           
162000     END-IF                                                               
162100     IF W-KVANTAL-KOEP < CLAG-KVBK                                        
162200        MOVE CLAG-KVBK TO W-KVANTAL-KOEP                                  
162300     END-IF                                                               
162400                                                                          
162500     .                                                                    
162600     EJECT                                                                
162700                                                                          
162800 HD-OMSPEC SECTION.                                                       
162900     MOVE 'HD-OMSPEC '   TO CURRENT-SECTION                               
163000                                                                          
163100     PERFORM HDA-INITIERA-OMSPEC                                          
163200     PERFORM HDB-SKAPA-TILLGANGSTABELL                                    
163300     PERFORM HDC-SKAPA-BEHOVSTABELL                                       
163400     PERFORM HDD-BERAKNA-TILLG-I-SPECVECKA                                
163500     SKIP1                                                                
163600     MOVE 1 TO INLB23-KDAVROP                                             
163700                                                                          
163800     INITIALIZE  BLOC-W221BLOC                                            
163900     MOVE IDPGM TO BLOC-IDPGM                                             
164000     MOVE +0    TO BLOC-TAB-IX                                            
164100                                                                          
164200     PERFORM HDE-SPEC-NYTT-FOERSLAG                                       
164300                                                                          
164400     IF BLOC-TAB-IX > 0                                                   
164500       CALL W221BLOC USING BLOC-W221BLOC BLOC-WDD9-PCB                    
164600                                         BLOC-WDF3-PCB                    
164700                                         BLOC-WDR2-PCB                    
164800                                         BLOC-WDR5-PCB                    
164900     END-IF                                                               
165000     SKIP1                                                                
165100     .                                                                    
165200     EJECT                                                                
165300 HDA-INITIERA-OMSPEC SECTION.                                             
165400     SKIP3                                                                
165500                                                                          
165600     MOVE ZERO                     TO W-TILLG-SPAR                        
165700                                                                          
165800     MOVE W-KVANTAL-KOEP           TO INLB22-KVBEST-PL                    
165900     MOVE ART-TIFINLV              TO W-TIFINLV                           
166000     COMPUTE W-TILLG-SPAR =                                               
166100                     W-SPAR-KVLS   (1)                                    
166200                 -   W-SPAR-KVRESS (1)                                    
166300                 -   W-SPAR-KVROS  (1)                                    
166400                 -  (ARTM01-ART-KVOKS-BULK    +                           
166500                     ARTM01-ART-KVOKS-DAG     +                           
166600                     ARTM01-ART-KVOKS-VOR     )                           
166700                 +   W-SPAR-KVAKS  (1)                                    
166800                 +   CLAG-KVLAAN                                          
166900                 +   W-OVERLAGER-SDC                                      
167000*                                                                         
167100***  PUBWEEK IS LEADTIME ADJUSTED IN DEMAND MODULE.                       
167200***  IF PUBWEEK IS IN FUTURE, CHECK THE WEEK FOR FIRST DEMAND             
167300***  SO THE CALL-OFFS SHOULD BE FROM LEADTIME + CURRENT WEEK.             
167400*                                                                         
167500                                                                          
167600     MOVE W-DATUM-AAVV-AKT         TO TMP1-YYWW                           
167700     MOVE W-TIFINLV-1-4            TO TMP2-YYWW                           
167800     PERFORM WY2000P3                                                     
167900     IF TMP2-YYWW     > TMP1-YYWW                                         
168000       MOVE 1                      TO IX-L                                
168100       PERFORM UNTIL IX-L > 156                                           
168200       OR LINK-KVBEHOV-VECKA (IX-L) > ZERO                                
168300         ADD 1                     TO IX-L                                
168400       END-PERFORM                                                        
168500*      CHECK IF DEMAND EXISTS I N THE FIRST WEEK OR                       
168600*      IF WE HAVE NEGATIVE ASSETS LIKE IN CASE OF BACKORDER               
168700*      BASED ON IX-L VALUE WE ADJUST THE DELIVERY PLAN WEEK               
168800*                                                                         
168900       IF LINK-KVBEHOV-DESSUTOM  > ZERO                                   
169000       OR W-TILLG-SPAR  < ZERO                                            
169100          MOVE 1                   TO IX-L                                
169200       END-IF                                                             
169300       IF IX-L > 156                                                      
169400          CONTINUE                                                        
169500       ELSE                                                               
169600          MOVE IX-L                TO ANTAL-VECKOR                        
169700          MOVE W-DATUM-AAVV-AKT    TO W-AAVV-ADD                          
169800          CALL W009VADD USING W-AAVV-ADD ANTAL-VECKOR                     
169900          MOVE W-AAVV-ADD          TO W-TISPECST-ADJ                      
170000       END-IF                                                             
170100     END-IF                                                               
170200*                                                                         
170300     MOVE W-DATUM-AAVV-AKT         TO W-TISPECST-DISP                     
170400     MOVE CLAG-KVVECKOR-FT         TO W-ANTAL-VECKOR                      
170500     ADD 1                         TO W-ANTAL-VECKOR                      
170600     CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR                   
170700     MOVE W-DATUM-AAVV-AKT         TO W-DASPECST-AAVV                     
170800*                                                                         
170900     MOVE CLAG-KVVECKOR-LT         TO W-ANTAL-VECKOR                      
171000     ADD 1 TO W-ANTAL-VECKOR                                              
171100     MOVE W-DASPECST-AAVV          TO W-AAVV-ADD                          
171200     CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                        
171300     MOVE W-AAVV-ADD               TO W-DASPECST-AAVV                     
171400     IF W-DASPECST-AAVV > 5000                                            
171500        MOVE 19                    TO W-DASPECST-SS                       
171600     ELSE                                                                 
171700        MOVE 20                    TO W-DASPECST-SS                       
171800     END-IF                                                               
171900     MOVE W-DASPECST               TO INLB22-DASPECST                     
172000*                                                                         
172100*    IF THE DELIERY PLAN WEEK CALCULATES IS LESS THAN                     
172200*    ADJUSTED DELIVERY PLAN WEEK CALUCATED ABOVE (BASED ON                
172300*    DEMAND FROM THE DEMAND DEMAND MODULE),                               
172400*    USE THE ADJUSTED DELIVERY PLAN START WEEK                            
172500*                                                                         
172600     MOVE W-DATUM-AAVV-AKT         TO TMP1-YYWW                           
172700     MOVE W-TIFINLV-1-4            TO TMP2-YYWW                           
172800     PERFORM WY2000P3                                                     
172900     MOVE ART-KDPRODSL             TO TEST-KDPRODSL                       
173000     IF TMP1-YYWW <  TMP2-YYWW                                            
173100        MOVE W-TISPECST-DISP       TO TMP1-YYWW                           
173200        MOVE W-TISPECST-ADJ        TO TMP2-YYWW                           
173300        PERFORM WY2000P3                                                  
173400        IF TMP1-YYWW <  TMP2-YYWW                                         
173500           MOVE W-TISPECST-ADJ     TO W-TISPECST-DISP                     
173600        END-IF                                                            
173700     END-IF                                                               
173800*                                                                         
173900     SKIP1                                                                
174000     MOVE ART-IDLEVNR              TO OLIKA-LEV                           
174100     IF ALLISON-LEVNR OR EATON-LEVNR OR SOMA-LEVNR                        
174200     OR TRW-LEVNR                                                         
174300        PERFORM HDAA-JUST-SPECST-VW-RENAULT                               
174400     END-IF                                                               
174500     IF  CLAG-KDHF > ZERO                                                 
174600     MOVE ART-IDLEVNR              TO OLIKA-LEV                           
174700     IF NOT SATS-LEVNR                                                    
174800        PERFORM HDAB-JUST-FOR-SEMESTER                                    
174900     END-IF                                                               
175000     END-IF                                                               
175100     SKIP1                                                                
175200***  IF  CLAG-KDAVT = 3                                                   
175300***     PERFORM HDAB-JUST-FOR-SEMESTER                                    
175400***  END-IF                                                               
175500     SKIP1                                                                
175600     MOVE ART-IDLEVNR              TO OLIKA-LEV                           
175700     IF SKOVDE-LEVNR                                                      
175800        MOVE CLAG-KVVECKOR-BT      TO W-KVVECKOR-SPEC                     
175900        ADD 10                     TO W-KVVECKOR-SPEC                     
176000           IF W-KVVECKOR-SPEC < 52                                        
176100            MOVE 52                TO W-KVVECKOR-SPEC                     
176200           END-IF                                                         
176300     ELSE                                                                 
176400        MOVE W-DATUM-AAVV-AKT      TO W-DATUM-FROM                        
176500        MOVE W-TISPECST-DISP       TO W-DATUM-TOM                         
176600        PERFORM S01-BERAKNA-VECKODIFFERENS                                
176700                                                                          
176800        MOVE ART-IDLEVNR           TO OLIKA-LEV                           
176900        IF CLAG-KDHF    > ZERO  OR                                        
177000           SATS-LEVNR                                                     
177100           MOVE ZERO               TO W-KVVECKOR-FFH                      
177200        ELSE                                                              
177300           COMPUTE W-KVDAGAR-FFH =                                        
177400                   CLAG-KVDAGAR-TT + CLAG-KVDAGAR-INLEV                   
177500           COMPUTE W-KVVECKOR-FFH ROUNDED =                               
177600                   W-KVDAGAR-FFH / 5                                      
177700                                                                          
177800        END-IF                                                            
177900                                                                          
178000        COMPUTE W-KVVECKOR-SPEC = 52 - CLAG-KVVECKOR-LT                   
178100        IF W-KVVECKOR-SPEC < 10                                           
178200           MOVE 10 TO W-KVVECKOR-SPEC                                     
178300        END-IF                                                            
178400     END-IF                                                               
178500                                                                          
178600     MOVE ZERO TO W-KVAVROP-VVKL12-ACC                                    
178700                  W-TILLG                                                 
178800     SKIP1                                                                
178900     MOVE 1 TO IX                                                         
179000     PERFORM UNTIL NOT(                                                   
179100        IX NOT > TILLGTAB-MAX)                                            
179200         MOVE ZERO TO TILLGTAB-ANTAL (IX)                                 
179300         ADD 1 TO IX                                                      
179400     END-PERFORM                                                          
179500                                                                          
179600                                                                          
179700*ETRACKER 10256698                                                        
179800     MOVE W-DASPECST-AAVV     TO DAYS-TIDATE1                             
179900                                                                          
180000     MOVE 'YYWW'              TO DAYS-KDDATFMT1                           
180100     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
180200     MOVE 0                   TO DAYS-KVDAYS                              
180300     MOVE SPACE               TO DAYS-TIDATE2                             
180400                                 DAYS-IDCALEND                            
180500     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
180600                                                                          
180700*                                                                         
180800     IF DAYS-KDRC = 8                                                     
180900       MOVE 'FEL VID ANROP TILL WZ20DAYS 5'                               
181000                                TO FELTEXT                                
181100       CALL FELLOG                                                        
181200     ELSE                                                                 
181300       MOVE DAYS-TIDATE2(1:6)     TO WS-TIAAMMDD-SPECST                   
181400     END-IF                                                               
181500     .                                                                    
181600     EJECT                                                                
181700 HDAA-JUST-SPECST-VW-RENAULT SECTION.                                     
181800******************************************************************        
181900*                                                                *        
182000*    TISPECST JUSTERAS FÖR VW/RENAULT/ALLISON/EATON/SOMA/TRW     *        
182100*                                             LEVERANTÖR         *        
182200*    TISPECST ÖKAS MED RESTEN AV TISPECST/4                      *        
182300*                                                                *        
182400******************************************************************        
182500     SKIP1                                                                
182600     MOVE INLB22-DASPECST TO W-DASPECST                                   
182700     DIVIDE W-DASPECST-AAVV BY 4 GIVING W-DUMMY                           
182800            REMAINDER W-ANTAL-VECKOR                                      
182900     COMPUTE W-ANTAL-VECKOR = 4 - W-ANTAL-VECKOR                          
183000     SKIP1                                                                
183100     CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR                   
183200     MOVE W-DASPECST-AAVV  TO W-AAVV-ADD                                  
183300     CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                        
183400     MOVE W-AAVV-ADD       TO W-DASPECST-AAVV                             
183500     IF W-DASPECST-AAVV > 5000                                            
183600        MOVE 19             TO W-DASPECST-SS                              
183700     ELSE                                                                 
183800        MOVE 20             TO W-DASPECST-SS                              
183900     END-IF                                                               
184000     MOVE W-DASPECST        TO INLB22-DASPECST                            
184100     .                                                                    
184200     EJECT                                                                
184300 HDAB-JUST-FOR-SEMESTER SECTION.                                          
184400******************************************************************        
184500*                                                                *        
184600*    JUSTERING AV TISPECST FÖR SEMESTER                          *        
184700*                                                                *        
184800******************************************************************        
184900     SKIP1                                                                
185000*    DIVIDE CLAG-KVDAGAR-TT BY 5 GIVING W-KVVECKOR-TT ROUNDED             
185100     MOVE INLB22-DASPECST TO W-DASPECST                                   
185200     MOVE W-DASPECST-AAVV TO W-DATUM-AAVV                                 
185300*    ADD W-KVVECKOR-TT TO W-DATUM-VV                                      
185400     SKIP1                                                                
185500     IF  W-DATUM-VV NOT < SEMESTER-VECKA-START                            
185600     AND W-DATUM-VV NOT > SEMESTER-VECKA-SLUT                             
185700         COMPUTE W-ANTAL-VECKOR =                                         
185800                            SEMESTER-VECKA-SLUT + 1                       
185900                         -  W-DATUM-VV                                    
186000         CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR               
186100         MOVE W-DASPECST-AAVV  TO W-AAVV-ADD                              
186200         CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                    
186300         MOVE W-AAVV-ADD       TO W-DASPECST-AAVV                         
186400     END-IF                                                               
186500     IF W-DASPECST-AAVV > 5000                                            
186600        MOVE 19             TO W-DASPECST-SS                              
186700     ELSE                                                                 
186800        MOVE 20             TO W-DASPECST-SS                              
186900     END-IF                                                               
187000     MOVE W-DASPECST        TO INLB22-DASPECST                            
187100     .                                                                    
187200     EJECT                                                                
187300 HDB-SKAPA-TILLGANGSTABELL SECTION.                                       
187400******************************************************************        
187500*                                                                *        
187600*    TABELL MED INLEVERANSER PLACERADE I RESPECTIVE VECKA        *        
187700*                                                                *        
187800******************************************************************        
187900     SKIP1                                                                
188000     MOVE 2             TO W-KDAVROP                                      
188100     PERFORM IMS-GET-INLB-WLINLB23-FIRST                                  
188200                                                                          
188300     MOVE INLB22-DASPECST TO W-DASPECST                                   
188400     MOVE W-DASPECST-AAVV TO W-GRAENS-AVROP                               
188500     MOVE W-KVVECKOR-SPEC TO W-ANTAL-VECKOR                               
188600     CALL W009VADD USING W-GRAENS-AVROP W-ANTAL-VECKOR                    
188700     SKIP1                                                                
188800     PERFORM UNTIL SEGMENT-SAKNAS                                         
188900         MOVE INLB22-DASPECST      TO W-DASPECST                          
189000         MOVE INLB23-DAAVROP-AVS   TO W-DAAVROP-AVS                       
189100         MOVE W-DAAVROP-AAVV       TO TMP1-YYWW                           
189200         MOVE W-GRAENS-AVROP       TO TMP2-YYWW                           
189300         PERFORM WY2000P3                                                 
189400         IF  INLB-KEY-02-IDLEVNR = ART-IDLEVNR                            
189500         AND INLB23-DAAVROP-AVS < INLB22-DASPECST                         
189600         OR  (INLB-KEY-02-IDLEVNR NOT = ART-IDLEVNR                       
189700          AND TMP1-YYWW < TMP2-YYWW)                                      
189800     SKIP1                                                                
189900              MOVE INLB23-TIAVRDAT-DISP  TO DAT-I-TIDATUM                 
190000              MOVE 'AAMMDD'              TO DAT-KDDATFORM                 
190100              CALL WDATKONV USING           DAT-KDDATFORM                 
190200                                            DAT-I-TIDATUM                 
190300                                            DAT-O-TIDATUM                 
190400                                            DAT-KDSVAR                    
190500              IF DAT-KDSVAR-FEL                                           
190600                MOVE 'FEL VID ANROP TILL DATKONV 1'                       
190700                                         TO FELTEXT                       
190800                CALL FELLOG                                               
190900              ELSE                                                        
191000                MOVE DAT-TIAAVV-GRP      TO WS-TIAAVV                     
191100                MOVE WS-TIAAVV           TO TMP1-YYWW                     
191200              END-IF                                                      
191300              MOVE W-TISPECST-DISP       TO TMP2-YYWW                     
191400              PERFORM WY2000P3                                            
191500              IF  TMP1-YYWW < TMP2-YYWW                                   
191600                  ADD INLB23-KVAVROP TO W-TILLG                           
191700              ELSE                                                        
191800                  MOVE WS-TIAAVV           TO W-DATUM-TOM                 
191900                  MOVE W-DATUM-AAVV-AKT    TO W-DATUM-FROM                
192000                  PERFORM S01-BERAKNA-VECKODIFFERENS                      
192100                  IF W-VECKO-DIFFERENS > ZERO                             
192200                    MOVE W-VECKO-DIFFERENS  TO TILLGTAB-IX                
192300                  ELSE                                                    
192400                    MOVE +1                 TO TILLGTAB-IX                
192500                  END-IF                                                  
192600                  ADD INLB23-KVAVROP                                      
192700                             TO TILLGTAB-ANTAL (TILLGTAB-IX)              
192800              END-IF                                                      
192900              IF  INLB-KEY-02-IDLEVNR = ART-IDLEVNR                       
193000                  ADD INLB23-KVAVROP TO W-KVAVROP-VVKL12-ACC              
193100              END-IF                                                      
193200         END-IF                                                           
193300         PERFORM IMS-GET-INLB-WLINLB23-NEXT                               
193400     END-PERFORM                                                          
193500     .                                                                    
193600     EJECT                                                                
193700 HDC-SKAPA-BEHOVSTABELL SECTION.                                          
193800     SKIP3                                                                
193900     MOVE SEP-SATS-TPO-LEV-SDC-NDC                                        
194000                                TO LINK-KDBEHOV                           
194100     MOVE SPACE                 TO LINK-IDDC                              
194200     MOVE W-DATUM-AAVV-AKT      TO LINK-TIAAVV-AKTUELL                    
194300                                   LINK-TIBEHOV-START                     
194400     MOVE 1                     TO W-ANTAL-VECKOR                         
194500     CALL W009VADD USING LINK-TIBEHOV-START W-ANTAL-VECKOR                
194600     MOVE ART-IDARTNR           TO LINK-IDARTNR                           
194700     MOVE 156                   TO LINK-KVVECKOR-BEHOV                    
194800     MOVE NEJ                   TO LINK-FLINKLDIRLEV                      
194900     SKIP1                                                                
195000     IF ART-KDERS-UTG = +0                                                
195100        CALL W22222 USING LINK-AREA W222-WDK6-PCB WDK7-PCB                
195200                          ARTM-PCB  W222-2501-PCB W222-WDB6R-PCB          
195300                          W222-WDK7R-PCB W222-WDB6-PCB                    
195400                          W222-WDD7-PCB W222-WDK7E-PCB                    
195500                          W222-UTIL-WDK6-PCB                              
195600                          W222-UTIL-WDK7-PCB                              
195700                          W222-UTIL-WDB6-PCB                              
195800                          W222-UTUP-WDK7-PCB                              
195900                          W222-UTUP-WDB6-PCB                              
196000                          W222-UTUP-UTIL-WDK6-PCB                         
196100                          W222-UTUP-UTIL-WDK7-PCB                         
196200                          W222-UTUP-UTIL-WDB6-PCB                         
196300                                                                          
196400        IF LINK-ANROP-FEL                                                 
196500           PERFORM S04-NOLLA-W22222                                       
196600        END-IF                                                            
196700     ELSE                                                                 
196800        PERFORM S04-NOLLA-W22222                                          
196900     END-IF                                                               
197000*                                                                         
197100     MOVE ART-IDLEVNR     TO OLIKA-LEV                                    
197200     IF CLAG-KDHF > ZERO                                                  
197300         IF NOT SATS-LEVNR                                                
197400             PERFORM HDCA-JUSTERA-FOR-SEMESTER                            
197500         END-IF                                                           
197600     END-IF                                                               
197700*                                                                         
197800     MOVE ART-IDLEVNR    TO OLIKA-LEV                                     
197900     IF     ALLISON-LEVNR OR EATON-LEVNR OR SOMA-LEVNR                    
198000         OR TRW-LEVNR                                                     
198100         PERFORM HDCB-JUST-BEHOV-VW-RENAULT                               
198200     END-IF                                                               
198300     .                                                                    
198400     EJECT                                                                
198500 HDCA-JUSTERA-FOR-SEMESTER SECTION.                                       
198600******************************************************************        
198700*                                                                *        
198800*    BEHOV UNDER SEMESTERN FLYTTAS FÖRE SEMESTERN                *        
198900*  OBS                                                           *        
199000*  OBS     KOPIA AV SECTIONEN FINNS I PROGRAM W2214000           *        
199100*  OBS     SKALL JUSTERAS PARALLELLT                             *        
199200*  OBS                                                           *        
199300*                                                                *        
199400******************************************************************        
199500     SKIP1                                                                
199600     IF CLAG-KDHF = ZERO                                                  
199700        COMPUTE W-KVDAGAR-FFH =                                           
199800                CLAG-KVDAGAR-TT + CLAG-KVDAGAR-INLEV                      
199900     ELSE                                                                 
200000        MOVE +0 TO W-KVDAGAR-FFH                                          
200100     END-IF                                                               
200200                                                                          
200300     COMPUTE W-SEMESTER-VV-START ROUNDED =                                
200400             SEMESTER-VECKA-START + (W-KVDAGAR-FFH / 5)                   
200500     COMPUTE W-SEMESTER-VV-SLUT ROUNDED =                                 
200600             SEMESTER-VECKA-SLUT + (W-KVDAGAR-FFH / 5)                    
200700                                                                          
200800     SKIP1                                                                
200900     MOVE LINK-TIBEHOV-START TO W-DATUM-AAVV                              
201000     SUBTRACT W-DATUM-VV FROM W-SEMESTER-VV-START                         
201100     SUBTRACT W-DATUM-VV FROM W-SEMESTER-VV-SLUT                          
201200     ADD 1 TO W-SEMESTER-VV-START                                         
201300              W-SEMESTER-VV-SLUT                                          
201400                                                                          
201500     PERFORM UNTIL NOT(                                                   
201600        W-SEMESTER-VV-START < LINK-KVVECKOR-BEHOV)                        
201700         MOVE W-SEMESTER-VV-START TO IX                                   
201800                                     IX-SUM                               
201900         SUBTRACT 1 FROM IX-SUM                                           
202000         PERFORM UNTIL NOT(                                               
202100            IX NOT > W-SEMESTER-VV-SLUT                                   
202200         AND IX NOT > LINK-KVVECKOR-BEHOV)                                
202300           IF IX-SUM > ZERO                                               
202400             ADD LINK-KVBEHOV-VECKA (IX)                                  
202500                             TO LINK-KVBEHOV-VECKA (IX-SUM)               
202600           END-IF                                                         
202700           IF IX > ZERO                                                   
202800             MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                         
202900           END-IF                                                         
203000           ADD 1 TO IX                                                    
203100         END-PERFORM                                                      
203200         ADD 52 TO W-SEMESTER-VV-START                                    
203300                   W-SEMESTER-VV-SLUT                                     
203400     END-PERFORM                                                          
203500     .                                                                    
203600     EJECT                                                                
203700 HDCB-JUST-BEHOV-VW-RENAULT SECTION.                                      
203800******************************************************************        
203900*                                                                *        
204000*    OM VW/RENAULT/ALLISON/EATON/SOMA/TRW SKALL ALLA BEHOV       *        
204100*    LIGGA I BEHOVSVECKOR                                        *        
204200*    SÅ ATT RESTEN (BEHOVSVECKA/4) = FRAMFÖRHÅLLNING             *        
204300*  OBS                                                           *        
204400*  OBS     KOPIA AV SECTIONEN FINNS I PROGRAM W2214000           *        
204500*  OBS     SKALL JUSTERAS PARALLELLT                             *        
204600*  OBS                                                           *        
204700*                                                                *        
204800******************************************************************        
204900     SKIP1                                                                
205000     COMPUTE W-KVDAGAR-FFH =                                              
205100             CLAG-KVDAGAR-TT + CLAG-KVDAGAR-INLEV                         
205200     COMPUTE W-KVVECKOR-FFH ROUNDED =                                     
205300             W-KVDAGAR-FFH / 5                                            
205400                                                                          
205500     PERFORM UNTIL W-KVVECKOR-FFH < 4                                     
205600         SUBTRACT 4 FROM W-KVVECKOR-FFH                                   
205700     END-PERFORM                                                          
205800                                                                          
205900     MOVE LINK-TIBEHOV-START TO W-DATUM-AAVV                              
206000     DIVIDE W-DATUM-VV BY 4 GIVING W-ANTAL                                
206100     MULTIPLY 4 BY W-ANTAL                                                
206200     SUBTRACT W-ANTAL FROM W-DATUM-VV                                     
206300     SKIP1                                                                
206400     MOVE 1 TO IX-SUM                                                     
206500     SUBTRACT W-DATUM-VV FROM IX-SUM                                      
206600     ADD W-KVVECKOR-FFH TO IX-SUM                                         
206700     PERFORM UNTIL NOT(                                                   
206800        IX-SUM < 1)                                                       
206900        ADD 4 TO IX-SUM                                                   
207000     END-PERFORM                                                          
207100     SKIP1                                                                
207200     MOVE 1 TO IX                                                         
207300     PERFORM UNTIL NOT(                                                   
207400        IX < IX-SUM)                                                      
207500        ADD LINK-KVBEHOV-VECKA (IX) TO LINK-KVBEHOV-DESSUTOM              
207600        SUBTRACT LINK-KVBEHOV-VECKA (IX)                                  
207700                                     FROM LINK-KVBEHOV-SUMMA              
207800        MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                              
207900        ADD 1 TO IX                                                       
208000     END-PERFORM                                                          
208100     SKIP1                                                                
208200     PERFORM UNTIL NOT(                                                   
208300        IX-SUM < LINK-KVVECKOR-BEHOV)                                     
208400         MOVE IX-SUM TO IX                                                
208500         ADD 1 TO IX                                                      
208600         PERFORM UNTIL NOT(                                               
208700            IX < IX-SUM + 4)                                              
208800             ADD LINK-KVBEHOV-VECKA (IX)                                  
208900                             TO LINK-KVBEHOV-VECKA (IX-SUM)               
209000             MOVE ZERO TO LINK-KVBEHOV-VECKA (IX)                         
209100             ADD 1 TO IX                                                  
209200         END-PERFORM                                                      
209300         ADD 4 TO IX-SUM                                                  
209400     END-PERFORM                                                          
209500     .                                                                    
209600     EJECT                                                                
209700 HDD-BERAKNA-TILLG-I-SPECVECKA SECTION.                                   
209800******************************************************************        
209900*                                                                *        
210000*    BERAKNING AV TILLGÅNG I FÖRSTA SPEC-VECKA                   *        
210100*    AVROP HAR TIDIGARE ADDERATS TILL W-TILLG                    *        
210200*                                                                *        
210300******************************************************************        
210400     SKIP1                                                                
210500                                                                          
210600                                                                          
210700     PERFORM HDDA-BERAKNA-ATGANG-I-VECKAN                                 
210800                                                                          
210900     COMPUTE W-TILLG-BER =                                                
211000                     W-SPAR-KVLS   (1)                                    
211100                 -   W-SPAR-KVRESS (1)                                    
211200                 -   W-SPAR-KVROS  (1)                                    
211300                 -  (ARTM01-ART-KVOKS-BULK    +                           
211400                     ARTM01-ART-KVOKS-DAG     +                           
211500                     ARTM01-ART-KVOKS-VOR     )                           
211600                 +   W-SPAR-KVAKS  (1)                                    
211700                 +   CLAG-KVLAAN                                          
211800                 -   W-VECKO-ATGANG                                       
211900                 +   W-OVERLAGER-SDC                                      
212000                                                                          
212100     ADD W-TILLG-BER TO W-TILLG                                           
212200     SUBTRACT LINK-KVBEHOV-DESSUTOM                                       
212300                             FROM W-TILLG                                 
212400     MOVE 1 TO IX                                                         
212500     MOVE LINK-TIBEHOV-START TO W-DATUM-FROM                              
212600     MOVE W-TISPECST-DISP    TO W-DATUM-TOM                               
212700     PERFORM S01-BERAKNA-VECKODIFFERENS                                   
212800*                                                                         
212900     IF W-VECKO-DIFFERENS > MAX-BEHOVSVECKOR-I-TAB                        
213000       MOVE MAX-BEHOVSVECKOR-I-TAB TO W-VECKO-DIFFERENS                   
213100     END-IF                                                               
213200*                                                                         
213300     MOVE 1 TO IX                                                         
213400     PERFORM UNTIL NOT(                                                   
213500        IX NOT > W-VECKO-DIFFERENS)                                       
213600         SUBTRACT LINK-KVBEHOV-VECKA (IX)                                 
213700                         FROM W-TILLG                                     
213800         ADD 1 TO IX                                                      
213900     END-PERFORM                                                          
214000     .                                                                    
214100     EJECT                                                                
214200 HDDA-BERAKNA-ATGANG-I-VECKAN SECTION.                                    
214300******************************************************************        
214400*                                                                *        
214500*    BERAKNING AV ÅTGÅNG I VECKAN DÅ OMSPECEN                    *        
214600*    GÖRS.                                                       *        
214700*                                                                *        
214800******************************************************************        
214900                                                                          
215000     MOVE ZERO                    TO W-TIFINLV-AAVV-S                     
215100     COMPUTE W-VECKO-ATGANG = ZERO                                        
215200                                                                          
215300     COMPUTE W-VECKO-KVPB-SEP(1)   =                                      
215400             W-SPAR-KVPB-SEP(1)    * (1 - W-SPAR-REDIRLEV(1))             
215500                                                                          
215600       COMPUTE W-SPAR-TOT-KVPB-SEP = (W-VECKO-KVPB-SEP(1) *               
215700                                      W-SPAR-RESEASON(1))                 
215800                                                                          
215900       COMPUTE W-VECKO-PB = W-SPAR-TOT-KVPB-SEP / 4.33                    
216000                                                                          
216100     EVALUATE W-DAT-TID-AKT                                               
216200       WHEN 1                                                             
216300         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 1                  
216400       WHEN 2                                                             
216500         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.8                
216600       WHEN 3                                                             
216700         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.6                
216800       WHEN 4                                                             
216900         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.4                
217000       WHEN 5                                                             
217100         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.2                
217200       WHEN 6                                                             
217300         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0                  
217400     END-EVALUATE                                                         
217500     DIVIDE ART-TIFINLV BY 10 GIVING W-TIFINLV-AAVV-S                     
217600     IF W-DATUM-AAVV-S < W-TIFINLV-AAVV-S                                 
217700        COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0                   
217800     END-IF                                                               
217900     .                                                                    
218000     EJECT                                                                
218100 HDE-SPEC-NYTT-FOERSLAG SECTION.                                          
218200     MOVE 'HDE-SPEC-NYTT-FOERSLAG '  TO CURRENT-SECTION                   
218300******************************************************************        
218400*                                                                *        
218500*    SPEC AV NYTT FÖRSLAG FRÅN LINK-TISPECST DATUM UNDER         *        
218600*    W-KVVECKOR-SPEC VECKOR                                      *        
218700*    BESTÄLLNINGSREST TÄCKS FÖR VVKL 1 OCH 2                     *        
218800*                                                                *        
218900******************************************************************        
219000     SKIP1                                                                
219100     COMPUTE W-KVBEST-REST = W-SPAR-KVBR                                  
219200                        + INLB22-KVBEST-PL                                
219300     SKIP1                                                                
219400     COMPUTE W-BUFF = W-SPAR-KVSLAGER (1)                                 
219500     SKIP1                                                                
219600     MOVE NEJ TO SW-BESTREST-TAEKT                                        
219700     MOVE ART-IDLEVNR      TO OLIKA-LEV                                   
219800     IF (CLAG-KDVVKL < 3 AND NOT SATS-LEVNR)                              
219900     OR  (W-SPAR-KDERS (1) > ZERO AND < 10)                               
220000     SKIP1                                                                
220100         IF  W-KVAVROP-VVKL12-ACC NOT < W-KVBEST-REST                     
220200             MOVE JA TO SW-BESTREST-TAEKT                                 
220300         END-IF                                                           
220400     END-IF                                                               
220500     SKIP1                                                                
220600     MOVE W-DATUM-AAVV-AKT TO W-DATUM-FROM                                
220700     MOVE W-TISPECST-DISP TO W-DATUM-TOM                                  
220800     PERFORM S01-BERAKNA-VECKODIFFERENS                                   
220900     IF W-VECKO-DIFFERENS < ZERO                                          
221000       MOVE +1 TO W-VECKO-DIFFERENS                                       
221100     END-IF                                                               
221200     MOVE W-VECKO-DIFFERENS TO IX                                         
221300     ADD W-KVVECKOR-SPEC TO W-VECKO-DIFFERENS                             
221400     MOVE JA             TO FOERST-SW                                     
221500*                                                                         
221600     IF W-VECKO-DIFFERENS > MAX-BEHOVSVECKOR-I-TAB                        
221700       MOVE MAX-BEHOVSVECKOR-I-TAB TO W-VECKO-DIFFERENS                   
221800     END-IF                                                               
221900                                                                          
222000     PERFORM HDECA-BER-ARSOMS                                             
222100     IF CLAG-KDVVKL > 2 AND CLAG-FLMANQ = NEJ                             
222200        IF W-ARSOMS > W-ARSOMS-80000                                      
222300           MOVE 1  TO W-Q-FREKV-MAX                                       
222400        ELSE                                                              
222500           MOVE 3  TO W-Q-FREKV-MAX                                       
222600        END-IF                                                            
222700     END-IF                                                               
222800                                                                          
222900**   LÄGGES I HDE UTANFÖR ITERATION                                       
223000**   LÄS WDF106 GU OKVAL  ADR-IDLANDX2                                    
223100**   BERÄKNA FRYSGRÄNS  DAGENS + CLAG-KVVECKOR-FT                         
223200                                                                          
223300     PERFORM HDED-LANDKOD-FRYSTID                                         
223400                                                                          
223500     MOVE ART-IDARTNR        TO BLOC-IDARTNR                              
223600     MOVE WC-CDC-SE          TO BLOC-IDDC                                 
223700     MOVE ART-IDLEVNR        TO BLOC-IDLEVNR                              
223800     MOVE CLAG-IDLEVNR-SHIP  TO BLOC-IDLEVNR-SHIP                         
223900     MOVE WS-IDLANDX2-SHIP   TO BLOC-IDLANDX2-SHIP                        
224000     MOVE CLAG-IDANSK        TO BLOC-IDANSK                               
224100     MOVE INLB23-KDAVROP     TO BLOC-KDAVROP                              
224200     MOVE WS-TIAAMMDD-SPECST TO BLOC-TIAAMMDD-SPECST                      
224300     MOVE WS-FRYSTID         TO BLOC-TIAAMMDD-FT                          
224400     MOVE CLAG-KVDAGAR-TT    TO BLOC-KVDAGAR-TT                           
224500     MOVE CLAG-KVDAGAR-INLEV TO BLOC-KVDAGAR-INLEV                        
224600     MOVE CLAG-KVQ           TO BLOC-KVQ                                  
224700     MOVE CLAG-KVPALL        TO BLOC-KVPALL                               
224800     MOVE CLAG-KVULOAD       TO BLOC-KVULOAD                              
224900                                                                          
225000     PERFORM UNTIL                                                        
225100       ( IX NOT < W-VECKO-DIFFERENS OR  SW-BESTREST-TAEKT =               
225200        JA  )                                                             
225300       IF IX NOT > ZERO                                                   
225400         MOVE 1 TO IX                                                     
225500       END-IF                                                             
225600       ADD TILLGTAB-ANTAL (IX)                                            
225700                               TO W-TILLG                                 
225800       SUBTRACT LINK-KVBEHOV-VECKA  (IX)                                  
225900                               FROM W-TILLG                               
226000       MOVE ART-KDPRODSL         TO TEST-KDPRODSL                         
226100       IF CLAG-KDVVKL > 2 AND CLAG-FLMANQ = NEJ                           
226200          AND CLAG-FLNYBER NOT = JA                                       
226300          AND KDPRODSL-VOLVO-BIMA                                         
226400                                                                          
226500*  ÅRSOMSÄTTNINGEN BESTÄMMER HUR OFTA AVROP SKALL SKE (W-Q-FREKV)         
226600                                                                          
226700         MOVE W-BUFF TO W-BUFF-VV                                         
226800         MOVE +1 TO IX-W-Q-FREKV                                          
226900         MOVE IX TO IX-KOM                                                
227000         PERFORM UNTIL IX-W-Q-FREKV > W-Q-FREKV-MAX                       
227100            ADD +1 TO IX-KOM                                              
227200*OBS    ATT IX-KOM EJ FÅR GÅ ÖVER TABELLEN                                
227300            COMPUTE W-VECKO-DIFFERENS-VV = W-VECKO-DIFFERENS + 3          
227400            IF IX-KOM < W-VECKO-DIFFERENS-VV AND                          
227500               IX-KOM NOT > TILLGTAB-MAX                                  
227600              SUBTRACT TILLGTAB-ANTAL     (IX-KOM) FROM W-BUFF-VV         
227700              ADD      LINK-KVBEHOV-VECKA (IX-KOM) TO   W-BUFF-VV         
227800            END-IF                                                        
227900            IF IX-W-Q-FREKV = 1                                           
228000               MOVE W-BUFF-VV TO W-BUFF-VV-1                              
228100            END-IF                                                        
228200            ADD +1 TO IX-W-Q-FREKV                                        
228300         END-PERFORM                                                      
228400                                                                          
228500         IF  W-TILLG < W-BUFF-VV                                          
228600           IF FOERST AND W-Q-FREKV-MAX = 3 AND                            
228700             W-TILLG NOT < W-BUFF-VV-1                                    
228800***         *   FÖR ARTIKLAR MED HEMTAGNING VAR 3:E VECKA                 
228900***         *VI SKALL INTE BÖRJA TA HEM 1:A GÅNGEN FÖRRÄN VI              
229000***         *KOMMER UNDER SÄKERHETSLAGRET REDAN VECKA IX+1                
229100             CONTINUE                                                     
229200           ELSE                                                           
229300              PERFORM HDEC-BERAEKNA-AVROPSKV-NYTT                         
229400              ADD W-AVROPSKVANTITET TO W-TILLG                            
229500              MOVE W-AVROPSKVANTITET TO INLB23-KVAVROP                    
229600              IF W-AVROPSKVANTITET > ZERO                                 
229700                PERFORM HDEB-SKAPA-AVROP                                  
229800******             JUSTERA TILLG DE VECKOR VI EV HOPPAR ÖVER              
229900                MOVE +2 TO IX-W-Q-FREKV                                   
230000                PERFORM UNTIL IX-W-Q-FREKV > W-Q-FREKV-MAX                
230100                   ADD +1 TO IX                                           
230200                   ADD TILLGTAB-ANTAL (IX)                                
230300                                           TO W-TILLG                     
230400                   SUBTRACT LINK-KVBEHOV-VECKA  (IX)                      
230500                                           FROM W-TILLG                   
230600                   ADD +1 TO IX-W-Q-FREKV                                 
230700                END-PERFORM                                               
230800                MOVE NEJ TO FOERST-SW                                     
230900              END-IF                                                      
231000           END-IF                                                         
231100         END-IF                                                           
231200       ELSE                                                               
231300         IF  W-TILLG < W-BUFF                                             
231400           PERFORM HDEA-BERAEKNA-AVROPSKVANTITET                          
231500           ADD W-AVROPSKVANTITET TO W-TILLG                               
231600           MOVE W-AVROPSKVANTITET TO INLB23-KVAVROP                       
231700           IF W-AVROPSKVANTITET > ZERO                                    
231800             PERFORM HDEB-SKAPA-AVROP                                     
231900           END-IF                                                         
232000         END-IF                                                           
232100       END-IF                                                             
232200       ADD 1 TO IX                                                        
232300     END-PERFORM                                                          
232400     .                                                                    
232500     EJECT                                                                
232600 HDEA-BERAEKNA-AVROPSKVANTITET SECTION.                                   
232700     MOVE 'HDEA-BERAEKNA-AVROPSKVANTITET' TO CURRENT-SECTION              
232800     SKIP1                                                                
232900*                                                                         
233000     MOVE CLAG-KVQ TO W-KVANTITET                                         
233100     IF  CLAG-TIQJUST > ZERO                                              
233200        MOVE W-DATUM-AAVV-AKT TO W-DATUM-AAVV-HELP                        
233300        COMPUTE W-ANTAL-VECKOR ROUNDED =                                  
233400           IX - (CLAG-KVDAGAR-TT + CLAG-KVDAGAR-INLEV) / 5                
233500        CALL W009VADD USING W-DATUM-AAVV-HELP W-ANTAL-VECKOR              
233600        MOVE CLAG-TIQJUST        TO TMP1-YYWW                             
233700        MOVE W-DATUM-AAVV-HELP   TO TMP2-YYWW                             
233800        PERFORM WY2000P3                                                  
233900        IF TMP1-YYWW <= TMP2-YYWW                                         
234000           MOVE CLAG-KVQ-JUST TO W-KVANTITET                              
234100        ELSE                                                              
234200           MOVE CLAG-KVQ      TO W-KVANTITET                              
234300        END-IF                                                            
234400     END-IF                                                               
234500                                                                          
234600     IF CLAG-KVULOAD > ZERO                                               
234700        MOVE CLAG-KVULOAD TO WS-KVULOAD                                   
234800     ELSE                                                                 
234900        MOVE CLAG-KVPALL  TO WS-KVULOAD                                   
235000     END-IF                                                               
235100     IF CLAG-FLNYBER = JA AND WS-KVULOAD > ZERO                           
235200        PERFORM UNTIL (W-TILLG + W-KVANTITET) >=                          
235300                       W-BUFF                                             
235400**********************(W-BUFF + CLAG-KVEOQ)                               
235500           ADD WS-KVULOAD TO W-KVANTITET                                  
235600        END-PERFORM                                                       
235700        MOVE W-KVANTITET  TO W-AVROPSKVANTITET                            
235800     ELSE                                                                 
235900        IF  W-KVANTITET = ZERO                                            
236000            MOVE 1 TO W-KVANTITET                                         
236100        END-IF                                                            
236200        COMPUTE W-ANTAL     ROUNDED = (((W-KVANTITET / 2)                 
236300                                  - W-TILLG                               
236400                                  + W-BUFF)                               
236500                                  / W-KVANTITET)                          
236600                                  + 0.49                                  
236700        COMPUTE W-AVROPSKVANTITET ROUNDED =                               
236800                                        W-KVANTITET * W-ANTAL             
236900     END-IF                                                               
237000*                                                                         
237100     IF  W-AVROPSKVANTITET < 1                                            
237200         MOVE 1 TO W-AVROPSKVANTITET                                      
237300     END-IF                                                               
237400     .                                                                    
237500     EJECT                                                                
237600 HDEB-SKAPA-AVROP SECTION.                                                
237700     MOVE 'HDEB-SKAPA-AVROP '  TO CURRENT-SECTION                         
237800******************************************************************        
237900*                                                                *        
238000*    AVROP SKAPAS OCH SKRIVES PÅ REGISTRET                       *        
238100*                                                                *        
238200*    SATSER HAR ALLTID AVROP PÅ EN MÅNDAG.INLB23-TILEVDAG = 1    *        
238300*                                                                *        
238400******************************************************************        
238500     SKIP1                                                                
238600     IF  (W-SPAR-KDERS (1) > ZERO AND < 10)                               
238700     SKIP1                                                                
238800         IF  W-KVAVROP-VVKL12-ACC + W-AVROPSKVANTITET NOT <               
238900                                    W-KVBEST-REST                         
239000             MOVE JA TO SW-BESTREST-TAEKT                                 
239100             COMPUTE INLB23-KVAVROP = W-KVBEST-REST                       
239200                               - W-KVAVROP-VVKL12-ACC                     
239300         ELSE                                                             
239400             ADD W-AVROPSKVANTITET TO W-KVAVROP-VVKL12-ACC                
239500         END-IF                                                           
239600     END-IF                                                               
239700     MOVE IX TO W-ANTAL-VECKOR                                            
239800     SKIP1                                                                
239900     MOVE W-DATUM-AAVV-AKT TO DATUM-AAVV                                  
240000     CALL W009VADD USING DATUM-AAVV W-ANTAL-VECKOR                        
240100                                                                          
240200     MOVE DATUM-AAVV           TO WS-DAYS-TIAAVV                          
240300                                  WS-TIAVROP-DISP                         
240400     MOVE WS-DAYS-TIAAVV       TO DAYS-TIDATE1                            
240500     MOVE 'YYWW'               TO DAYS-KDDATFMT1                          
240600     MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                          
240700     MOVE 0                    TO DAYS-KVDAYS                             
240800     MOVE SPACE                TO DAYS-TIDATE2                            
240900                                  DAYS-IDCALEND                           
241000     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
241100                                                                          
241200     IF DAYS-KDRC = 8                                                     
241300       MOVE 'FEL VID ANROP TILL WZ20DAYS 6'                               
241400                               TO FELTEXT                                 
241500       CALL FELLOG                                                        
241600     ELSE                                                                 
241700       MOVE DAYS-TIDATE2(1:6)  TO INLB23-TIAVRDAT-DISP                    
241800     END-IF                                                               
241900                                                                          
242000     COMPUTE WS-SUMMA-KVPB = CLAG-KVPB-SEP  +                             
242100                             CLAG-KVPB-SATS +                             
242200                             CLAG-KVPB-TPO  +                             
242300                             PUNK-KVPB-SDC-TOT                            
242400                                                                          
242500     MOVE ART-IDLEVNR      TO OLIKA-LEV                                   
242600     IF  SATS-LEVNR                                                       
242700     OR  CLAG-KDHF > ZERO                                                 
242800         MOVE INLB23-TIAVRDAT-DISP TO INLB23-TIAVRDAT-INL                 
242900         MOVE WS-TIAVROP-DISP      TO W-DAAVROP-AAVV                      
243000         PERFORM HDEBA-TILEVDAG                                           
243100*FIX-START NYÅR  DISP-09   SATS                                           
243200         IF (ART-IDLEVNR NOT = 'DL7EA')             AND                   
243300            (WS-SUMMA-KVPB  < 5.0)                  AND                   
243400            (W-DAAVROP-AAVV = 0951 OR 0952 OR 0953) AND                   
243500            INLB23-TILEVDAG = 1                                           
243600                  MOVE 1002     TO  W-DAAVROP-AAVV                        
243700                  MOVE 1002     TO  WS-TIAVROP-DISP                       
243800                  MOVE 100111   TO  INLB23-TIAVRDAT-DISP                  
243900                  MOVE 100111   TO  INLB23-TIAVRDAT-INL                   
244000         END-IF                                                           
244100*FIX-END                                                                  
244200     ELSE                                                                 
244300*AVS                                                                      
244400        COMPUTE W-KVDAGAR-FFH =                                           
244500                CLAG-KVDAGAR-INLEV + CLAG-KVDAGAR-TT                      
244600        COMPUTE W-ANTAL-VECKOR ROUNDED =                                  
244700                W-KVDAGAR-FFH / -5                                        
244800                                                                          
244900        MOVE WS-TIAVROP-DISP  TO W-DAAVROP-AAVV                           
245000        MOVE W-DAAVROP-AAVV   TO W-AAVV-ADD                               
245100        CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                     
245200        MOVE W-AAVV-ADD       TO W-DAAVROP-AAVV                           
245300        MOVE W-DAAVROP-AAVV   TO W-TIAVROP-AVS                            
245400                                                                          
245500        PERFORM HDEBA-TILEVDAG                                            
245600                                                                          
245700*FIX-START NYÅR 2025                                                      
245800        MOVE CLAG-IDANSK     TO WS-SKIP-IDANSK-2025                       
245900        IF SKIP-IDANSK-2025                                               
246000           IF W-TIAVROP-AVS   = 2548 OR 2549                              
246100              MOVE 2603      TO  W-DAAVROP-AAVV                           
246200              MOVE 2603      TO  W-TIAVROP-AVS                            
246300           ELSE                                                           
246400              IF W-TIAVROP-AVS   = 2550 OR 2551                           
246500                 MOVE 2604      TO  W-DAAVROP-AAVV                        
246600                 MOVE 2604      TO  W-TIAVROP-AVS                         
246700              END-IF                                                      
246800           END-IF                                                         
246900        END-IF                                                            
247000                                                                          
247100*FIX-START NYÅR 2024                                                      
247200        MOVE CLAG-IDANSK     TO WS-SKIP-IDANSK-2024                       
247300        MOVE ART-IDLEVNR     TO WS-SKIP-IDLEVNR-2024                      
247400        IF SKIP-IDANSK-2024                                               
247500        OR SKIP-IDLEVNR-2024                                              
247600           IF W-TIAVROP-AVS   = 2451 OR 2452                              
247700              MOVE 2502      TO  W-DAAVROP-AAVV                           
247800              MOVE 2502      TO  W-TIAVROP-AVS                            
247900           END-IF                                                         
248000        END-IF                                                            
248100                                                                          
248200*COMMENTING BELOW OLD CODE - RETAINED FOR REFERENCE - 202410              
248300*FIX-START NYÅR 2018                                                      
248400*       IF ART-IDLEVNR = 'BP3EA'                                          
248500*         IF CLAG-IDANSK = 430 OR 680 OR 685 OR 718 OR 730                
248600*                                                                         
248700*           IF W-TIAVROP-AVS  = 1851                                      
248800*              MOVE 1902     TO  W-DAAVROP-AAVV                           
248900*              MOVE 1902     TO  W-TIAVROP-AVS                            
249000*           END-IF                                                        
249100*         END-IF                                                          
249200*       END-IF                                                            
249300*                                                                         
249400*       IF ART-IDLEVNR = 'BSBZA' AND (CLAG-IDANSK = 500)                  
249500*         IF W-TIAVROP-AVS  = 1851                                        
249600*            MOVE 1902     TO  W-DAAVROP-AAVV                             
249700*            MOVE 1902     TO  W-TIAVROP-AVS                              
249800*         END-IF                                                          
249900*       END-IF                                                            
250000*                                                                         
250100*       IF ART-IDLEVNR = 'BP7YA' AND (CLAG-IDANSK = 695)                  
250200*         IF W-TIAVROP-AVS  = 1851                                        
250300*            MOVE 1902     TO  W-DAAVROP-AAVV                             
250400*            MOVE 1902     TO  W-TIAVROP-AVS                              
250500*         END-IF                                                          
250600*                                                                         
250700*         IF W-TIAVROP-AVS  = 1852  AND                                   
250800*           (INLB23-TILEVDAG = 2 OR 3)                                    
250900*                                                                         
251000*            MOVE 1902     TO  W-DAAVROP-AAVV                             
251100*            MOVE 1902     TO  W-TIAVROP-AVS                              
251200*         END-IF                                                          
251300*                                                                         
251400*         IF W-TIAVROP-AVS  = 1901  AND                                   
251500*           (INLB23-TILEVDAG = 2)                                         
251600*                                                                         
251700*            MOVE 1902     TO  W-DAAVROP-AAVV                             
251800*            MOVE 1902     TO  W-TIAVROP-AVS                              
251900*         END-IF                                                          
252000*       END-IF                                                            
252100*                                                                         
252200*       IF ART-IDLEVNR = 'BP8BA' AND (CLAG-IDANSK = 680)                  
252300*         IF W-TIAVROP-AVS  = 1851                                        
252400*            MOVE 1902     TO  W-DAAVROP-AAVV                             
252500*            MOVE 1902     TO  W-TIAVROP-AVS                              
252600*         END-IF                                                          
252700*       END-IF                                                            
252800*FIX-END NYÅR 2018                                                        
252900                                                                          
253000*AVS-AAMMDD                                                               
253100*ETRACKER 10255129                                                        
253200*       W-TIAVROP-AVS IS SET IN THE LINES JUST BEFORE THIS TEXT.          
253300*                                                                         
253400*ETRACKER 10255129                                                        
253500        MOVE 'YYWWD'             TO DAYS-KDDATFMT1                        
253600        MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                        
253700        COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-TIAVROP-AVS +              
253800                                        INLB23-TILEVDAG                   
253900        MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1                      
254000        MOVE 0                   TO DAYS-KVDAYS                           
254100        MOVE SPACE               TO DAYS-TIDATE2                          
254200                                    DAYS-IDCALEND                         
254300        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
254400                                                                          
254500*                                                                         
254600        IF DAYS-KDRC = 8                                                  
254700          MOVE 'FEL VID ANROP TILL WZ20DAYS 2'                            
254800                                   TO FELTEXT                             
254900          CALL FELLOG                                                     
255000        ELSE                                                              
255100           MOVE DAYS-TIDATE2(1:6)  TO W-TIAAMMDD-AVS                      
255200                                      ARB-TIAAMMDD-AVS                    
255300                                      GAM-TIAAMMDD-AVS                    
255400        END-IF                                                            
255500                                                                          
255600        MOVE W-TIAVROP-AVS       TO WOL-TIAAVV-AVS                        
255700        MOVE INLB23-TILEVDAG     TO WOL-TILEVDAG                          
255800*                                                                         
255900*--     AVS TVÅ VECKOR BAKÅT TIFINLEV ÄR NÄRA                             
256000        MOVE W-DATUM-AAVV-AKT   TO TMP1-YYWW                              
256100        MOVE W-TIFINLV-1-4      TO TMP2-YYWW                              
256200        PERFORM WY2000P3                                                  
256300        IF  TMP1-YYWW <= TMP2-YYWW                                        
256400        AND WS-TIAVROP-DISP = W-TIFINLV-1-4                               
256500                                                                          
256600*--        UNDERSÖKNING OM AVS BLEV FÖR NÄRA I TID                        
256700           MOVE W-DATUM-AAVV-AKT     TO W-DATUM-AAVV-HELP                 
256800           MOVE +1 TO W-ANTAL-VECKOR                                      
256900           CALL W009VADD USING W-DATUM-AAVV-HELP W-ANTAL-VECKOR           
257000           MOVE W-DAAVROP-AAVV       TO TMP1-YYWW                         
257100           MOVE W-DATUM-AAVV-HELP    TO TMP2-YYWW                         
257200           PERFORM WY2000P3                                               
257300           IF TMP1-YYWW < TMP2-YYWW                                       
257400              MOVE W-DATUM-AAVV-HELP TO W-DAAVROP-AAVV                    
257500           END-IF                                                         
257600                                                                          
257700           MOVE W-DAAVROP-AAVV       TO W-TIAVROP-AVS                     
257800                                                                          
257900*ETRACKER 10255129                                                        
258000           MOVE 'YYWWD'              TO DAYS-KDDATFMT1                    
258100           MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                    
258200           COMPUTE WS-DAYS-TIDATE1-AAVVD                                  
258300                                      = 10 * W-TIAVROP-AVS +              
258400                                        INLB23-TILEVDAG                   
258500           MOVE WS-DAYS-TIDATE1-AAVVD                                     
258600                                     TO DAYS-TIDATE1                      
258700           MOVE 0                    TO DAYS-KVDAYS                       
258800           MOVE SPACE                TO DAYS-TIDATE2                      
258900                                        DAYS-IDCALEND                     
259000           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
259100                                                                          
259200           IF DAYS-KDRC = 8                                               
259300              MOVE 'FEL VID ANROP TILL WZ20DAYS 4'                        
259400                                     TO FELTEXT                           
259500              CALL FELLOG                                                 
259600           ELSE                                                           
259700              MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                    
259800                                        ARB-TIAAMMDD-AVS                  
259900                                        GAM-TIAAMMDD-AVS                  
260000           END-IF                                                         
260100                                                                          
260200           MOVE W-TIAVROP-AVS        TO WOL-TIAAVV-AVS                    
260300           MOVE INLB23-TILEVDAG      TO WOL-TILEVDAG                      
260400        END-IF                                                            
260500*                                                                         
260600*SKALL DENNA FIX TAS BORT? 150518                                         
260700*FIX NYÅR START  DATKONVJUST 2014                                         
260800        COMPUTE FIX-AAVVD  = 10 * W-TIAVROP-AVS +                         
260900                             INLB23-TILEVDAG                              
261000                                                                          
261100        IF FIX-AAVVD = 15011                                              
261200           MOVE 141229  TO W-TIAAMMDD-AVS                                 
261300                           ARB-TIAAMMDD-AVS                               
261400                           GAM-TIAAMMDD-AVS                               
261500           MOVE 1       TO WOL-TILEVDAG                                   
261600           MOVE 1501    TO WOL-TIAAVV-AVS                                 
261700        END-IF                                                            
261800        IF FIX-AAVVD = 15012                                              
261900           MOVE 141230  TO W-TIAAMMDD-AVS                                 
262000                           ARB-TIAAMMDD-AVS                               
262100                           GAM-TIAAMMDD-AVS                               
262200           MOVE 2       TO WOL-TILEVDAG                                   
262300           MOVE 1501    TO WOL-TIAAVV-AVS                                 
262400        END-IF                                                            
262500        IF FIX-AAVVD = 15013                                              
262600           MOVE 141231  TO W-TIAAMMDD-AVS                                 
262700                           ARB-TIAAMMDD-AVS                               
262800                           GAM-TIAAMMDD-AVS                               
262900           MOVE 3       TO WOL-TILEVDAG                                   
263000           MOVE 1501    TO WOL-TIAAVV-AVS                                 
263100        END-IF                                                            
263200*FIX NYÅR END DATKONVJUST 2014                                            
263300                                                                          
263400                                                                          
263500        MOVE ART-IDLEVNR        TO WS-IDLEVNR-EMIL                        
263600        IF (NOT EJ-GODK-EMIL-LEVNR) AND                                   
263700           (CLAG-KVQ > ZERO)        AND                                   
263800           ((W-ARSBEH / CLAG-KVQ) > 35)                                   
263900**         DAGLIGA AVROP BEHANDLAS SENARE                                 
264000           CONTINUE                                                       
264100        ELSE                                                              
264200**         LÄS WDF301 GU LANDKOD IDLANDX2 DATUM                           
264300**         FINNS UNDERLIGGANDE SEGMENT MED IDLEVNR ?                      
264400**         OM TRÄFF JUSTERA DATUM (OBS INLB23-TILEVDAG ?)                 
264500                                                                          
264600           PERFORM HDEBD-KOLL-HELGDAG                                     
264700        END-IF                                                            
264800                                                                          
264900*INL + DISP                                                               
265000        PERFORM HDEBC-BERAEKNA-INL-DISP-AAMMDD                            
265100                                                                          
265200     END-IF                                                               
265300     IF W-DAAVROP-AAVV > 5000                                             
265400        MOVE 19         TO W-DAAVROP-SS                                   
265500     ELSE                                                                 
265600        MOVE 20         TO W-DAAVROP-SS                                   
265700     END-IF                                                               
265800     MOVE W-DAAVROP-AVS TO INLB23-DAAVROP-AVS                             
265900                                                                          
266000*----HÄR KOLLAR MAN OM AVROPSVECKAN ÄR BLOCKAD FÖR IDLEVNR-SHIP           
266100*----OCH SKALL FLYTTAS VIA SUB-PGM W221BLOC.                              
266200*----                                                                     
266300     IF INLB23-KVAVROP > ZERO                                             
266400       PERFORM HDEBE-KOLL-BLOCKAD-VECKA                                   
266500     END-IF                                                               
266600                                                                          
266700*    HÄR GÖRS NÅGON TEST SOM ANGER VILKA AVROP SOM SKALL                  
266800*    SMETAS UT PÅ LEVERANTÖRENS LEVDAGAR AVROPS-VECKAN                    
266900     MOVE ART-IDLEVNR     TO WS-IDLEVNR-EMIL                              
267000     IF (NOT EJ-GODK-EMIL-LEVNR) AND                                      
267100        (CLAG-KVQ > ZERO)        AND                                      
267200        ((W-ARSBEH / CLAG-KVQ) > 35)                                      
267300*       SATS? KDHF?                                                       
267400                                                                          
267500        IF SW-BLOCKAD-VECKA = JA                                          
267600          PERFORM S03-TILEVDAG-DAGL-AVROP                                 
267700          MOVE JA   TO BLOC-FLAGGA-DAGL-AVROP                             
267800        ELSE                                                              
267900          PERFORM S03-TILEVDAG-DAGL-AVROP                                 
268000          PERFORM HDEBB-SKAPA-DAGL-AVROP                                  
268100        END-IF                                                            
268200                                                                          
268300     ELSE                                                                 
268400                                                                          
268500        IF SW-BLOCKAD-VECKA = JA                                          
268600          MOVE NEJ  TO BLOC-FLAGGA-DAGL-AVROP                             
268700        ELSE                                                              
268800          IF INLB23-KVAVROP > ZERO                                        
268900             MOVE INLB23-DAAVROP-AVS TO W-DAAVROP-KY                      
269000             MOVE INLB23-TILEVDAG    TO W-TILEVDAG-KY                     
269100             MOVE INLB23-KDAVROP     TO W-KDAVROP                         
269200             MOVE WS-IDARTNR         TO W-IDARTNR-INLB                    
269300             MOVE WC-CDC-SE          TO W-IDDC-INLB                       
269400             PERFORM IMS-GHU-INLB-WLINLB23                                
269500             IF SEGMENT-FINNS                                             
269600                ADD INLB2A-KVAVROP   TO INLB23-KVAVROP                    
269700                PERFORM IMS-REPL-INLB-WLINLB23                            
269800             ELSE                                                         
269900                PERFORM IMS-ISRT-INLB-WLINLB23                            
270000             END-IF                                                       
270100          END-IF                                                          
270200        END-IF                                                            
270300     END-IF                                                               
270400     .                                                                    
270500     EJECT                                                                
270600*                                                                *        
270700 HDEBA-TILEVDAG            SECTION.                                       
270800     MOVE 'HDEBA-TILEVDAG     '  TO CURRENT-SECTION                       
270900                                                                          
271000     MOVE ZERO                     TO INLB23-TILEVDAG                     
271100                                                                          
271200     MOVE +1                       TO IX-DAG                              
271300     PERFORM UNTIL IX-DAG > 5                                             
271400        IF CLAG-TILEVDAG (IX-DAG) > ZERO                                  
271500           MOVE CLAG-TILEVDAG (IX-DAG) TO INLB23-TILEVDAG                 
271600           MOVE +5                 TO IX-DAG                              
271700        END-IF                                                            
271800        ADD +1                     TO IX-DAG                              
271900     END-PERFORM                                                          
272000                                                                          
272100     IF INLB23-TILEVDAG = ZERO                                            
272200        MOVE CLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                        
272300        PERFORM IMS-GET-LEVA01-WDF101-SHIP                                
272400        IF SEGMENT-FINNS                                                  
272500          MOVE +1                  TO IX-DAG                              
272600          PERFORM UNTIL IX-DAG > 5                                        
272700             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
272800                MOVE F1-LEV-TILEVDAG (IX-DAG) TO INLB23-TILEVDAG          
272900                MOVE +5            TO IX-DAG                              
273000             END-IF                                                       
273100             ADD +1                TO IX-DAG                              
273200          END-PERFORM                                                     
273300          IF INLB23-TILEVDAG = ZERO                                       
273400             MOVE +1               TO INLB23-TILEVDAG                     
273500          END-IF                                                          
273600        ELSE                                                              
273700          MOVE +1                  TO INLB23-TILEVDAG                     
273800        END-IF                                                            
273900     END-IF                                                               
274000                                                                          
274100     .                                                                    
274200     EJECT                                                                
274300 HDEBB-SKAPA-DAGL-AVROP         SECTION.                                  
274400     MOVE 'HDEBB-SKAPA-DAGL-AVROP '  TO CURRENT-SECTION                   
274500                                                                          
274600*ANT-LEVDAGAR - FLYTTAT TILL S03-TILEVDAG.....                            
274700*                                                                         
274800                                                                          
274900*KVAVROP/DAG                                                              
275000     MOVE ZERO                  TO W-KVAVROP (1)                          
275100                                   W-KVAVROP (2)                          
275200                                   W-KVAVROP (3)                          
275300                                   W-KVAVROP (4)                          
275400                                   W-KVAVROP (5)                          
275500     IF CLAG-KVPALL < +1                                                  
275600        MOVE +1                 TO WS-KVPALL                              
275700     ELSE                                                                 
275800        MOVE CLAG-KVPALL        TO WS-KVPALL                              
275900     END-IF                                                               
276000     IF CLAG-FLNYBER = JA AND CLAG-KVQ > ZERO                             
276100        MOVE CLAG-KVQ           TO WS-KVPALL                              
276200     END-IF                                                               
276300                                                                          
276400     IF INLB23-KVAVROP > ZERO                                             
276500        PERFORM UNTIL (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP(3)         
276600                     + W-KVAVROP(4) + W-KVAVROP(5))                       
276700                     NOT < INLB23-KVAVROP                                 
276800          MOVE +1               TO IX-DAG                                 
276900          PERFORM UNTIL    IX-DAG  > 5                                    
277000            IF W-TILEVDAG (IX-DAG) > ZERO                                 
277100               IF (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP (3) +          
277200                   W-KVAVROP(4) + W-KVAVROP(5)) < INLB23-KVAVROP          
277300                   ADD WS-KVPALL TO W-KVAVROP (IX-DAG)                    
277400               END-IF                                                     
277500            END-IF                                                        
277600            ADD +1               TO IX-DAG                                
277700            IF IX-DAG = 6                                                 
277800               IF CLAG-KVULOAD > ZERO                                     
277900                  MOVE CLAG-KVULOAD TO WS-KVULOAD                         
278000               ELSE                                                       
278100                  MOVE CLAG-KVPALL  TO WS-KVULOAD                         
278200               END-IF                                                     
278300               IF CLAG-FLNYBER = JA AND WS-KVULOAD > ZERO                 
278400                  MOVE WS-KVULOAD   TO WS-KVPALL                          
278500               END-IF                                                     
278600            END-IF                                                        
278700          END-PERFORM                                                     
278800        END-PERFORM                                                       
278900     END-IF                                                               
279000                                                                          
279100*AVS-AAVV                                                                 
279200     MOVE +1                    TO IX-DAG                                 
279300     PERFORM UNTIL   (IX-DAG) > 5                                         
279400       MOVE W-TIAVROP-AVS     TO SPAR-TIAVROP-AVS                         
279500       IF W-TILEVDAG (IX-DAG) > ZERO AND W-KVAVROP (IX-DAG) > ZERO        
279600*                                                                         
279700*FIX-START NYÅR 2025                                                      
279800          MOVE CLAG-IDANSK   TO WS-SKIP-IDANSK-2025                       
279900          IF SKIP-IDANSK-2025                                             
280000            IF W-TIAVROP-AVS   = 2548 OR 2549                             
280100               MOVE 2603      TO  W-TIAVROP-AVS                           
280200            ELSE                                                          
280300               IF W-TIAVROP-AVS   = 2550 OR 2551                          
280400                  MOVE 2604      TO  W-TIAVROP-AVS                        
280500               END-IF                                                     
280600            END-IF                                                        
280700          END-IF                                                          
280800*                                                                         
280900*FIX-START NYÅR 2024                                                      
281000          MOVE CLAG-IDANSK   TO WS-SKIP-IDANSK-2024                       
281100          MOVE ART-IDLEVNR   TO WS-SKIP-IDLEVNR-2024                      
281200          IF SKIP-IDANSK-2024                                             
281300          OR SKIP-IDLEVNR-2024                                            
281400            IF W-TIAVROP-AVS  = 2451 OR 2452                              
281500               MOVE 2502     TO  W-TIAVROP-AVS                            
281600            END-IF                                                        
281700          END-IF                                                          
281800*                                                                         
281900**COMMENTING BELOW OLD CODE - RETAINED FOR REFERENCE - 202410             
282000*FIX-START NYÅR 2018                                                      
282100*       IF ART-IDLEVNR = 'BP3EA'                                          
282200*         IF CLAG-IDANSK = 430 OR 680 OR 685 OR 718 OR 730                
282300*                                                                         
282400*           IF W-TIAVROP-AVS  = 1851                                      
282500*              MOVE 1902     TO  W-TIAVROP-AVS                            
282600*           END-IF                                                        
282700*         END-IF                                                          
282800*       END-IF                                                            
282900*                                                                         
283000*       IF ART-IDLEVNR = 'BSBZA' AND (CLAG-IDANSK = 500)                  
283100*         IF W-TIAVROP-AVS  = 1851                                        
283200*            MOVE 1902     TO  W-TIAVROP-AVS                              
283300*         END-IF                                                          
283400*       END-IF                                                            
283500*                                                                         
283600*       IF ART-IDLEVNR = 'BP7YA' AND (CLAG-IDANSK = 695)                  
283700*         IF W-TIAVROP-AVS  = 1851                                        
283800*            MOVE 1902     TO  W-TIAVROP-AVS                              
283900*         END-IF                                                          
284000*                                                                         
284100*         IF W-TIAVROP-AVS  = 1852 AND                                    
284200*           (W-TILEVDAG (IX-DAG) = 2 OR 3)                                
284300*            MOVE 1902     TO  W-TIAVROP-AVS                              
284400*         END-IF                                                          
284500*                                                                         
284600*         IF W-TIAVROP-AVS  = 1901 AND                                    
284700*           (W-TILEVDAG (IX-DAG) = 2)                                     
284800*            MOVE 1902     TO  W-TIAVROP-AVS                              
284900*         END-IF                                                          
285000*       END-IF                                                            
285100*                                                                         
285200*       IF ART-IDLEVNR = 'BP8BA' AND (CLAG-IDANSK = 680)                  
285300*         IF W-TIAVROP-AVS  = 1851                                        
285400*            MOVE 1902     TO  W-TIAVROP-AVS                              
285500*         END-IF                                                          
285600*       END-IF                                                            
285700*FIX-END NYÅR 2018                                                        
285800*                                                                         
285900*AVS-YYMMD                                                                
286000        MOVE 'YYWWD'              TO DAYS-KDDATFMT1                       
286100        MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                       
286200        COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-TIAVROP-AVS +              
286300                                        W-TILEVDAG (IX-DAG)               
286400        MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1                      
286500        MOVE 0                    TO DAYS-KVDAYS                          
286600        MOVE SPACE                TO DAYS-TIDATE2                         
286700                                     DAYS-IDCALEND                        
286800        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
286900                                                                          
287000        IF DAYS-KDRC = 8                                                  
287100          MOVE 'FEL VID ANROP TILL WZ20DAYS 1'                            
287200                                  TO FELTEXT                              
287300          CALL FELLOG                                                     
287400        ELSE                                                              
287500          MOVE DAYS-TIDATE2(1:6)  TO W-TIAAMMDD-AVS                       
287600                                     ARB-TIAAMMDD-AVS                     
287700        END-IF                                                            
287800                                                                          
287900        MOVE W-TILEVDAG (IX-DAG)  TO WOL-TILEVDAG                         
288000        MOVE W-TIAVROP-AVS        TO WOL-TIAAVV-AVS                       
288100        MOVE W-TIAVROP-AVS        TO W-DAAVROP-AVS                        
288200                                                                          
288300*FIX NYÅR START   DATKONVJUST 2014                                        
288400        COMPUTE FIX-AAVVD  = 10 * W-TIAVROP-AVS +                         
288500                             W-TILEVDAG (IX-DAG)                          
288600        IF FIX-AAVVD = 15011                                              
288700           MOVE 141229  TO W-TIAAMMDD-AVS                                 
288800                           ARB-TIAAMMDD-AVS                               
288900           MOVE 1       TO WOL-TILEVDAG                                   
289000           MOVE 1501    TO WOL-TIAAVV-AVS                                 
289100        END-IF                                                            
289200        IF FIX-AAVVD = 15012                                              
289300           MOVE 141230  TO W-TIAAMMDD-AVS                                 
289400                           ARB-TIAAMMDD-AVS                               
289500           MOVE 2       TO WOL-TILEVDAG                                   
289600           MOVE 1501    TO WOL-TIAAVV-AVS                                 
289700        END-IF                                                            
289800        IF FIX-AAVVD = 15013                                              
289900           MOVE 141231  TO W-TIAAMMDD-AVS                                 
290000                           ARB-TIAAMMDD-AVS                               
290100           MOVE 3       TO WOL-TILEVDAG                                   
290200           MOVE 1501    TO WOL-TIAAVV-AVS                                 
290300        END-IF                                                            
290400*FIX NYÅR END  DATKONVJUST 2014                                           
290500*                                                                         
290600                                                                          
290700**    LÄS WDF301 GU LANDKOD IDLANDX2 DATUM                                
290800**      FINNS UNDERLIGGANDE SEGMENT MED IDLEVNR ?                         
290900**      OM TRÄFF JUSTERA DATUM                                            
291000                                                                          
291100        PERFORM HDEBD-KOLL-HELGDAG                                        
291200                                                                          
291300*INL + DISP                                                               
291400        PERFORM HDEBC-BERAEKNA-INL-DISP-AAMMDD                            
291500                                                                          
291600        IF W-DAAVROP-AAVV > 5000                                          
291700           MOVE 19         TO W-DAAVROP-SS                                
291800        ELSE                                                              
291900           MOVE 20         TO W-DAAVROP-SS                                
292000        END-IF                                                            
292100        MOVE W-DAAVROP-AVS       TO INLB23-DAAVROP-AVS                    
292200                                    W-DAAVROP-KY                          
292300        MOVE WOL-TILEVDAG        TO INLB23-TILEVDAG                       
292400                                    W-TILEVDAG-KY                         
292500        MOVE W-KVAVROP (IX-DAG)  TO INLB23-KVAVROP                        
292600        MOVE INLB23-KDAVROP      TO W-KDAVROP                             
292700                                                                          
292800**-HÄR KOLLAR MAN OM AVROPSVECKAN ÄR BLOCKAD FÖR IDLEVNR-SHIP             
292900**-OCH SKALL FLYTTAS VIA SUB-PGM W221BLOC.                                
293000        IF INLB23-KVAVROP > ZERO                                          
293100          PERFORM HDEBE-KOLL-BLOCKAD-VECKA                                
293200        END-IF                                                            
293300                                                                          
293400        IF SW-BLOCKAD-VECKA = JA                                          
293500          MOVE JA   TO BLOC-FLAGGA-DAGL-AVROP                             
293600        ELSE                                                              
293700          MOVE WS-IDARTNR          TO W-IDARTNR-INLB                      
293800          MOVE WC-CDC-SE           TO W-IDDC-INLB                         
293900          PERFORM IMS-GHU-INLB-WLINLB23                                   
294000          IF SEGMENT-FINNS                                                
294100            ADD INLB2A-KVAVROP    TO INLB23-KVAVROP                       
294200            PERFORM IMS-REPL-INLB-WLINLB23                                
294300          ELSE                                                            
294400            PERFORM IMS-ISRT-INLB-WLINLB23                                
294500          END-IF                                                          
294600        END-IF                                                            
294700        MOVE SPAR-TIAVROP-AVS    TO W-TIAVROP-AVS                         
294800       END-IF                                                             
294900                                                                          
295000       ADD +1 TO IX-DAG                                                   
295100     END-PERFORM                                                          
295200     .                                                                    
295300     EJECT                                                                
295400 HDEBC-BERAEKNA-INL-DISP-AAMMDD SECTION.                                  
295500     MOVE 'HDEBC-BERAEKNA-INL-DISP-AAMMDD' TO CURRENT-SECTION             
295600                                                                          
295700*INL                                                                      
295800          MOVE 2                   TO WORK-KDCALL                         
295900          MOVE '11'                TO WORK-IDDC                           
296000          MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                   
296100          MOVE CLAG-KVDAGAR-TT     TO WORK-KVWORKD                        
296200          ADD +1                   TO WORK-KVWORKD                        
296300          CALL WORKDAY USING  WORK-KDCALL                                 
296400               WORK-DATE-AREA WORK-KDSVAR                                 
296500          MOVE WORK-TIAAMMDD-TOM   TO INLB23-TIAVRDAT-INL                 
296600*DISP                                                                     
296700          MOVE 2                   TO WORK-KDCALL                         
296800          MOVE '11'                TO WORK-IDDC                           
296900          MOVE INLB23-TIAVRDAT-INL TO WORK-TIAAMMDD-FOM                   
297000          MOVE CLAG-KVDAGAR-INLEV  TO WORK-KVWORKD                        
297100          ADD +1                   TO WORK-KVWORKD                        
297200          CALL WORKDAY USING  WORK-KDCALL                                 
297300               WORK-DATE-AREA WORK-KDSVAR                                 
297400          MOVE WORK-TIAAMMDD-TOM   TO INLB23-TIAVRDAT-DISP                
297500     .                                                                    
297600     EJECT                                                                
297700 HDEBD-KOLL-HELGDAG  SECTION.                                             
297800     MOVE 'HDEBD-KOLL-HELGDAG '  TO CURRENT-SECTION                       
297900                                                                          
298000     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
298100     MOVE 20               TO W-DADATUM-HELG-SS                           
298200     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
298300*                                                                         
298400     PERFORM IMS-GET-WDF301                                               
298500                                                                          
298600     IF SEGMENT-FINNS                                                     
298700                                                                          
298800        PERFORM HDEBDA-SOEK-NY-AVS-DAG                                    
298900                                                                          
299000        MOVE ARB-TIAAMMDD-AVS     TO W-TIAAMMDD-AVS                       
299100        MOVE ARB-TIAAMMDD-AVS     TO DAYS-TIDATE1                         
299200        MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                       
299300        MOVE 'YYWWD'              TO DAYS-KDDATFMT2                       
299400        MOVE 0                    TO DAYS-KVDAYS                          
299500        MOVE SPACE                TO DAYS-TIDATE2                         
299600                                     DAYS-IDCALEND                        
299700        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
299800                                                                          
299900*                                                                         
300000        IF DAYS-KDRC = 8                                                  
300100          MOVE 'FEL VID ANROP TILL WZ20DAYS 3'                            
300200                                   TO FELTEXT                             
300300          CALL FELLOG                                                     
300400        ELSE                                                              
300500          MOVE DAYS-TIDATE2(1:4)  TO W-DAAVROP-AAVV                       
300600          MOVE DAYS-TIDATE2(5:1)  TO INLB23-TILEVDAG                      
300700          MOVE DAYS-TIDATE2(5:1)  TO WOL-TILEVDAG                         
300800        END-IF                                                            
300900*                                                                         
301000*FIX NYÅR   DATKONVJUST 2014                                              
301100        IF ARB-TIAAMMDD-AVS = 141229                                      
301200           MOVE 1501   TO W-DAAVROP-AAVV                                  
301300           MOVE 1      TO INLB23-TILEVDAG                                 
301400                          WOL-TILEVDAG                                    
301500        END-IF                                                            
301600        IF ARB-TIAAMMDD-AVS = 141230                                      
301700           MOVE 1501   TO W-DAAVROP-AAVV                                  
301800           MOVE 2      TO INLB23-TILEVDAG                                 
301900                          WOL-TILEVDAG                                    
302000        END-IF                                                            
302100        IF ARB-TIAAMMDD-AVS = 141231                                      
302200           MOVE 1501   TO W-DAAVROP-AAVV                                  
302300           MOVE 3      TO INLB23-TILEVDAG                                 
302400                          WOL-TILEVDAG                                    
302500        END-IF                                                            
302600*FIX NYÅR END  DATKONVJUST 2014                                           
302700     END-IF                                                               
302800     .                                                                    
302900     EJECT                                                                
303000 HDEBDA-SOEK-NY-AVS-DAG  SECTION.                                         
303100     MOVE 'HDEBDA-SOEK-NY-AVS-DAG '  TO CURRENT-SECTION                   
303200                                                                          
303300*    ITERERA                                                              
303400*       -7 DAGAR                                                          
303500*       UTANFÖR FRYSTID ?                                                 
303600*       HELGDAG ?                                                         
303700     MOVE NEJ                 TO SW-OK SW-FRYS                            
303800     PERFORM UNTIL SW-OK = JA OR SW-FRYS = JA                             
303900        MOVE 003              TO DAG-KDCALL                               
304000        MOVE 20               TO DAG-TISEKEL-TOM                          
304100        MOVE ARB-TIAAMMDD-AVS TO DAG-TIAAMMDD-TOM                         
304200        MOVE 8                TO DAG-KVKALDAG                             
304300        CALL WDAGKONV   USING DAG-KDCALL,                                 
304400                              DAG-DATUM-AREA,                             
304500                              DAG-KDSVAR                                  
304600        IF DAG-KDSVAR = SPACE                                             
304700           MOVE DAG-TIAAMMDD-FOM TO ARB-TIAAMMDD-AVS                      
304800           IF ARB-TIAAMMDD-AVS > WS-FRYSTID AND                           
304900              ARB-TIAAMMDD-AVS >= WS-TIAAMMDD-SPECST                      
305000              MOVE JA         TO SW-OK                                    
305100              PERFORM HDEBDAA-KOLL-HELG                                   
305200           ELSE                                                           
305300              MOVE JA         TO SW-FRYS                                  
305400           END-IF                                                         
305500        ELSE                                                              
305600           MOVE 'FEL VID ANROP TILL DAGKONV 1'                            
305700                              TO FELTEXT                                  
305800           CALL FELLOG                                                    
305900        END-IF                                                            
306000     END-PERFORM                                                          
306100                                                                          
306200     IF SW-FRYS = JA                                                      
306300*       OM EJ OK: FINNS ANNAN DAG URSPRUNGLIG VECKA ?                     
306400                                                                          
306500        PERFORM HDEBDB-SOEK-ANNAN-DAG                                     
306600                                                                          
306700*       OM EJ OK: NÄSTA MÖJLIGA TILLFÄLLE                                 
306800     END-IF                                                               
306900     .                                                                    
307000     EJECT                                                                
307100 HDEBDAA-KOLL-HELG  SECTION.                                              
307200     MOVE 'HDEBDAA-KOLL-HELG  '  TO CURRENT-SECTION                       
307300                                                                          
307400*                                                                         
307500     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
307600     MOVE 20               TO W-DADATUM-HELG-SS                           
307700     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
307800*                                                                         
307900     PERFORM IMS-GET-WDF301                                               
308000                                                                          
308100     IF SEGMENT-FINNS                                                     
308200        MOVE NEJ TO SW-OK                                                 
308300     END-IF                                                               
308400     .                                                                    
308500     EJECT                                                                
308600 HDEBDB-SOEK-ANNAN-DAG SECTION.                                           
308700     MOVE 'HDEBDB-SOEK-ANNAN-DAG '  TO CURRENT-SECTION                    
308800                                                                          
308900*                                                                         
309000*    FINNS WDK611-TILEVDAG NOT = WOL-TILEVDAG                             
309100*ALTERNATIVT                                                              
309200*    FINNS WDF1-TILEVDAG   NOT = WOL-TILEVDAG                             
309300                                                                          
309400     MOVE ZERO TO ARB-TILEVDAG (1)                                        
309500                  ARB-TILEVDAG (2)                                        
309600                  ARB-TILEVDAG (3)                                        
309700                  ARB-TILEVDAG (4)                                        
309800                  ARB-TILEVDAG (5)                                        
309900     IF CLAG-TILEVDAG (1) > ZERO OR                                       
310000        CLAG-TILEVDAG (2) > ZERO OR                                       
310100        CLAG-TILEVDAG (3) > ZERO OR                                       
310200        CLAG-TILEVDAG (4) > ZERO OR                                       
310300        CLAG-TILEVDAG (5) > ZERO                                          
310400        MOVE +1              TO IX-DG                                     
310500        PERFORM UNTIL IX-DG > 5                                           
310600           IF CLAG-TILEVDAG (IX-DG) > ZERO AND                            
310700              CLAG-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                    
310800              MOVE CLAG-TILEVDAG (IX-DG)                                  
310900                             TO ARB-TILEVDAG (IX-DG)                      
311000           END-IF                                                         
311100           ADD +1            TO IX-DG                                     
311200        END-PERFORM                                                       
311300     ELSE                                                                 
311400        MOVE +1              TO IX-DG                                     
311500        PERFORM UNTIL IX-DG > 5                                           
311600           IF F1-LEV-TILEVDAG (IX-DG) > ZERO AND                          
311700              F1-LEV-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                  
311800              MOVE F1-LEV-TILEVDAG (IX-DG)                                
311900                             TO ARB-TILEVDAG (IX-DG)                      
312000           END-IF                                                         
312100           ADD +1            TO IX-DG                                     
312200        END-PERFORM                                                       
312300     END-IF                                                               
312400                                                                          
312500     MOVE +1                 TO IX-DG                                     
312600     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
312700        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
312800           PERFORM HDEBDBA-KOLL-NYTT-DATUM                                
312900        END-IF                                                            
313000        ADD +1               TO IX-DG                                     
313100     END-PERFORM                                                          
313200                                                                          
313300*    ANNARS FÖRSTA LEVDAG NÄSTA VECKA OSV                                 
313400     MOVE WOL-TILEVDAG TO ARB-TILEVDAG (WOL-TILEVDAG)                     
313500     PERFORM UNTIL SW-OK = JA                                             
313600*      STEGA FRAMÅT                                                       
313700       PERFORM HDEBDBB-KOLL-NASTA-DATUM                                   
313800     END-PERFORM                                                          
313900     .                                                                    
314000     EJECT                                                                
314100 HDEBDBA-KOLL-NYTT-DATUM SECTION.                                         
314200     MOVE 'HDEBDBA-KOLL-NYTT-DATUM '  TO CURRENT-SECTION                  
314300                                                                          
314400*    OM OK => SW-OK = JA                                                  
314500*    KOLL AV ANNAN DAG I URSPRUNGLIG AVSÄNDNINGSVECKA                     
314600                                                                          
314700     MOVE 'YYWWD'             TO DAYS-KDDATFMT1                           
314800     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
314900     COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * WOL-TIAAVV-AVS +                
315000                                    ARB-TILEVDAG (IX-DG)                  
315100     MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1                         
315200     MOVE 0                   TO DAYS-KVDAYS                              
315300     MOVE SPACE               TO DAYS-TIDATE2                             
315400                                 DAYS-IDCALEND                            
315500     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
315600                                                                          
315700     IF DAYS-KDRC = 8                                                     
315800       MOVE 'FEL VID ANROP TILL WZ20DAYS 7'                               
315900                                TO FELTEXT                                
316000       CALL FELLOG                                                        
316100     ELSE                                                                 
316200        MOVE DAYS-TIDATE2(1:6)  TO ARB-TIAAMMDD-AVS                       
316300     END-IF                                                               
316400                                                                          
316500                                                                          
316600*FIX NYÅR START   DATKONVJUST 2014                                        
316700     COMPUTE FIX-AAVVD  = 10 * WOL-TIAAVV-AVS +                           
316800                          ARB-TILEVDAG (IX-DG)                            
316900     IF FIX-AAVVD = 15011                                                 
317000        MOVE 141229  TO ARB-TIAAMMDD-AVS                                  
317100     END-IF                                                               
317200     IF FIX-AAVVD = 15012                                                 
317300        MOVE 141230  TO ARB-TIAAMMDD-AVS                                  
317400     END-IF                                                               
317500     IF FIX-AAVVD = 15013                                                 
317600        MOVE 141231  TO ARB-TIAAMMDD-AVS                                  
317700     END-IF                                                               
317800*FIX NYÅR END DATKONVJUST 2014                                            
317900                                                                          
318000     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
318100     MOVE 20               TO W-DADATUM-HELG-SS                           
318200     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
318300     PERFORM IMS-GET-WDF301                                               
318400                                                                          
318500     IF SEGMENT-FINNS                                                     
318600**         SÖK VIDARE                                                     
318700       CONTINUE                                                           
318800     ELSE                                                                 
318900        MOVE JA TO SW-OK                                                  
319000     END-IF                                                               
319100     .                                                                    
319200     EJECT                                                                
319300 HDEBDBB-KOLL-NASTA-DATUM SECTION.                                        
319400     MOVE 'HDEBDBB-KOLL-NASTA-DATUM '  TO CURRENT-SECTION                 
319500                                                                          
319600*       OM OK => SW-OK = JA                                               
319700**   ÖKA VECKA MED 1                                                      
319800**   SÖK AVS-DAGAR I VECKAN                                               
319900                                                                          
320000     MOVE WOL-TIAAVV-AVS  TO DATUM-AAVV                                   
320100     MOVE 1               TO W-ANTAL-VECKOR                               
320200     CALL W009VADD USING DATUM-AAVV W-ANTAL-VECKOR                        
320300     MOVE DATUM-AAVV      TO WOL-TIAAVV-AVS                               
320400                                                                          
320500     MOVE +1                 TO IX-DG                                     
320600     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
320700        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
320800           PERFORM HDEBDBA-KOLL-NYTT-DATUM                                
320900        END-IF                                                            
321000        ADD +1               TO IX-DG                                     
321100     END-PERFORM                                                          
321200     .                                                                    
321300     EJECT                                                                
321400 HDEBE-KOLL-BLOCKAD-VECKA  SECTION.                                       
321500     MOVE 'HDEBE-KOLL-BLOCKAD-VECKA '  TO CURRENT-SECTION                 
321600                                                                          
321700     MOVE NEJ    TO SW-BLOCKAD-VECKA                                      
321800                                                                          
321900     MOVE CLAG-IDLEVNR-SHIP    TO W-IDLEVNR-SHIP-2258                     
322000     MOVE CLAG-IDANSK          TO W-IDANSK-2260                           
322100     MOVE INLB23-DAAVROP-AVS   TO W-DAAVROP-2260                          
322200                                                                          
322300     PERFORM IMS-GU-WDGX2258                                              
322400     IF SEGMENT-FINNS                                                     
322500                                                                          
322600       PERFORM IMS-GNP-WDGX2260                                           
322700       IF SEGMENT-FINNS                                                   
322800         MOVE JA               TO SW-BLOCKAD-VECKA                        
322900                                                                          
323000         ADD +1  TO BLOC-TAB-IX                                           
323100         IF BLOC-TAB-IX < BLOC-MAX-IX                                     
323200           MOVE INLB23-DAAVROP-AVS  TO                                    
323300                                    BLOC-DAAVROP-AVS(BLOC-TAB-IX)         
323400           MOVE INLB23-TILEVDAG     TO                                    
323500                                    BLOC-TILEVDAG(BLOC-TAB-IX)            
323600           MOVE INLB23-KVAVROP      TO                                    
323700                                    BLOC-KVAVROP(BLOC-TAB-IX)             
323800           MOVE 2260-DAAVROP-FOM   TO                                     
323900                                    BLOC-DAAVROP-FOM(BLOC-TAB-IX)         
324000           MOVE 2260-DAAVROP-TOM   TO                                     
324100                                    BLOC-DAAVROP-TOM(BLOC-TAB-IX)         
324200           MOVE 2260-DAAVROP-TFOM  TO                                     
324300                                    BLOC-DAAVROP-TFOM(BLOC-TAB-IX)        
324400         END-IF                                                           
324500       END-IF                                                             
324600     END-IF                                                               
324700                                                                          
324800     .                                                                    
324900     EJECT                                                                
325000 HDEC-BERAEKNA-AVROPSKV-NYTT   SECTION.                                   
325100     MOVE 'HDEC-BERAEKNA-AVROPSKV-NYTT '  TO CURRENT-SECTION              
325200                                                                          
325300*    AVROPSKVANTITETEN SKALL VARA SKILLNADEN MELLAN TILLG                 
325400*    OCH (SÄKERHETSLAGER + - DE BEHOV OCH TILLG                           
325500*    SOM SKER DE KOMMANDE W-Q-FREKV VECKORNA)                             
325600*    DETTA SKALL AVRUNDAS UPPÅT                                           
325700                                                                          
325800     COMPUTE W-ARBKVANT2 = W-BUFF-VV - W-TILLG                            
325900                                                                          
326000     PERFORM HDECB-AVRUNDA                                                
326100                                                                          
326200     MOVE W-ARBKVANT      TO W-AVROPSKVANTITET                            
326300                                                                          
326400     IF  W-AVROPSKVANTITET < 1                                            
326500         MOVE 1 TO W-AVROPSKVANTITET                                      
326600     END-IF                                                               
326700     .                                                                    
326800     EJECT                                                                
326900 HDECA-BER-ARSOMS              SECTION.                                   
327000     MOVE 'HDECA-BER-ARSOMS    '  TO CURRENT-SECTION                      
327100                                                                          
327200        COMPUTE W-ARSOMS = 12                                             
327300                * ( CLAG-KVPB-SEP                                         
327400                  + CLAG-KVPB-SATS                                        
327500                  + CLAG-KVPB-TPO                                         
327600                  + PUNK-KVPB-SDC-TOT )                                   
327700                *   PUNK-PRARTBES                                         
327800                                                                          
327900        COMPUTE W-ARSBEH = 12                                             
328000                * ( CLAG-KVPB-SEP                                         
328100                  + CLAG-KVPB-SATS                                        
328200                  + CLAG-KVPB-TPO                                         
328300                  + PUNK-KVPB-SDC-TOT )                                   
328400     .                                                                    
328500     EJECT                                                                
328600 HDECB-AVRUNDA                 SECTION.                                   
328700     MOVE 'HDECB-AVRUNDA      '  TO CURRENT-SECTION                       
328800                                                                          
328900     IF  CLAG-KVPALL > ZERO                                               
329000        COMPUTE W-ARBKVANT =                                              
329100                ( W-ARBKVANT2 / CLAG-KVPALL ) + 0.99                      
329200        COMPUTE W-ARBKVANT = CLAG-KVPALL * W-ARBKVANT                     
329300        IF W-ARBKVANT = ZERO                                              
329400              MOVE CLAG-KVPALL TO W-ARBKVANT                              
329500        END-IF                                                            
329600     ELSE                                                                 
329700        IF  CLAG-KVQPACK-1 > ZERO                                         
329800            COMPUTE W-ARBKVANT =                                          
329900                    ( W-ARBKVANT2 / CLAG-KVQPACK-1 ) + 0.99               
330000            COMPUTE W-ARBKVANT = CLAG-KVQPACK-1 * W-ARBKVANT              
330100            IF W-ARBKVANT = ZERO                                          
330200               MOVE CLAG-KVQPACK-1 TO W-ARBKVANT                          
330300            END-IF                                                        
330400        ELSE                                                              
330500            MOVE 1 TO W-M                                                 
330600            MOVE 1  TO IX-M                                               
330700            PERFORM UNTIL                                                 
330800               IX-M  > W012-MAXINDEX-1                                    
330900               OR  W-ARBKVANT2 NOT > W012-KVQ-BER-TOM (IX-M)              
331000               ADD 1 TO IX-M                                              
331100            END-PERFORM                                                   
331200            MOVE W012-KVANTAL-MULTIPEL (IX-M) TO W-M                      
331300                                                                          
331400            COMPUTE W-KVANTAL =                                           
331500                    ( W-ARBKVANT2 / W-M ) + 0.99                          
331600            IF  W-KVANTAL = ZERO                                          
331700               MOVE W-M TO W-ARBKVANT                                     
331800            ELSE                                                          
331900               MULTIPLY W-M BY W-KVANTAL GIVING W-ARBKVANT                
332000            END-IF                                                        
332100        END-IF                                                            
332200     END-IF                                                               
332300     .                                                                    
332400     EJECT                                                                
332500 HDED-LANDKOD-FRYSTID SECTION.                                            
332600     MOVE 'HDED-LANDKOD-FRYSTID '  TO CURRENT-SECTION                     
332700                                                                          
332800**   LÄS WDF106 GU OKVAL  ADR-IDLANDX2                                    
332900**   BERÄKNA FRYSGRÄNS  DAGENS + CLAG-KVVECKOR-FT                         
333000**                                                                        
333100     MOVE CLAG-IDLEVNR-SHIP  TO W-IDLEVNR-SHIP                            
333200     MOVE SPACE              TO WS-IDLANDX2-SHIP                          
333300     PERFORM IMS-GET-LEVA14-WDF106-SHIP                                   
333400     IF SEGMENT-FINNS                                                     
333500        MOVE ADR-IDLANDX2    TO WS-IDLANDX2-SHIP                          
333600     END-IF                                                               
333700**   LÄGG DAGENS DATUM I FRYSTID. ADDERA MED FT*7                         
333800     MOVE W-DATUM-AAMMDD     TO WS-FRYSTID                                
333900**   ANTAL DGR FRYSTID*7 + 1                                              
334000**   DAGKONV DD + FT                                                      
334100     MOVE 002            TO DAG-KDCALL                                    
334200     MOVE 20             TO DAG-TISEKEL-FOM                               
334300     MOVE W-DATUM-AAMMDD TO DAG-TIAAMMDD-FOM                              
334400     COMPUTE DAG-KVKALDAG = (CLAG-KVVECKOR-FT * 7) + 2                    
334500     CALL WDAGKONV USING DAG-KDCALL,                                      
334600                         DAG-DATUM-AREA,                                  
334700                         DAG-KDSVAR                                       
334800     IF DAG-KDSVAR = SPACE                                                
334900        MOVE DAG-TISEKEL-TOM  TO WS-FRYSTID-SEKEL                         
335000        MOVE DAG-TIAAMMDD-TOM TO WS-FRYSTID                               
335100     ELSE                                                                 
335200        MOVE 'FEL VID ANROP TILL DAGKONV 2'                               
335300                              TO FELTEXT                                  
335400        CALL FELLOG                                                       
335500     END-IF                                                               
335600     .                                                                    
335700     EJECT                                                                
335800 HE-UPPDATERA-ARTC SECTION.                                               
335900     MOVE 'HE-UPPDATERA-ARTC '   TO CURRENT-SECTION                       
336000                                                                          
336100     PERFORM IMS-GET-ARTC-WLARTC11                                        
336200                                                                          
336300     MOVE 5 TO CLAG-KDLPSP                                                
336400     MOVE W-DATUM-AAVV-AKT TO CLAG-TIOMSPEC                               
336500                              CLAG-TILPSP                                 
336600     IF CLAG-KDVVKL = 5                                                   
336700        MOVE 2 TO W-ANTAL-VECKOR                                          
336800     ELSE                                                                 
336900        MOVE 3 TO W-ANTAL-VECKOR                                          
337000     END-IF                                                               
337100     CALL W009VADD USING CLAG-TILPSP W-ANTAL-VECKOR                       
337200                                                                          
337300     PERFORM IMS-REPL-ARTC-WLARTC11                                       
337400     .                                                                    
337500     EJECT                                                                
337600 HF-SKAPA-OMSPEC-SEGMENT SECTION.                                         
337700     MOVE 'HF-SKAPA-OMSPEC-SEGMENT '  TO CURRENT-SECTION                  
337800                                                                          
337900     MOVE ZERO                 TO W-KDLPORS-TAB(1)                        
338000                                  W-KDLPORS-TAB(2)                        
338100                                  W-KDLPORS-TAB(3)                        
338200     IF INLB22-KVBEST-PL       >  ZERO                                    
338300        MOVE 1                 TO INLB22-KDPLKOEP                         
338400        MOVE 01                TO W-KDLPORS-TAB(4)                        
338500        CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                    
338600     ELSE                                                                 
338700        MOVE ZERO              TO INLB22-KDPLKOEP                         
338800     END-IF                                                               
338900                                                                          
339000*--- BLOCKADE AVROP GICK INTE ATT FLYTTA TILL EN GODKÄND VECKA.           
339100     IF BLOC-TAB-IX > 0                                                   
339200       IF BLOC-KDSVAR = 'N'                                               
339300         MOVE 06               TO W-KDLPORS-TAB(4)                        
339400         CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                   
339500       END-IF                                                             
339600     END-IF                                                               
339700                                                                          
339800     MOVE 17                   TO W-KDLPORS-TAB(4)                        
339900     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
340000                                                                          
340100     MOVE W-KDLPORS-TAB(1)     TO INLB22-KDLPORS-TAB(1)                   
340200     MOVE W-KDLPORS-TAB(2)     TO INLB22-KDLPORS-TAB(2)                   
340300     MOVE W-KDLPORS-TAB(3)     TO INLB22-KDLPORS-TAB(3)                   
340400                                                                          
340500     PERFORM IMS-ISRT-INLB-WLINLB22                                       
340600     .                                                                    
340700     EJECT                                                                
340800 HG-SKAPA-FOERSLAGSPOST-PA-KOE SECTION.                                   
340900     MOVE 'HG-SKAPA-FOERSLAGSPOST-PA-KOE' TO CURRENT-SECTION              
341000     SKIP2                                                                
341100*    --- LÄGGER UPP NYTT LEV.PLANEFÖRSLAG PÅ WDD6-KÖN                     
341200     MOVE WC-CDC-SE            TO W-IDDC-MIN                              
341300                                  W-IDDC-MAX                              
341400     MOVE W-IDLEVNR            TO W-IDLEVNR-MIN                           
341500                                  W-IDLEVNR-MAX                           
341600     MOVE W-IDARTNR            TO W-IDARTNR-MIN                           
341700                                  W-IDARTNR-MAX                           
341800     MOVE W-IDANSK             TO W-IDANSK-MIN                            
341900                                  W-IDANSK-MAX                            
342000                                                                          
342100     PERFORM IMS-GHU-WDD601                                               
342200     IF SEGMENT-FINNS                                                     
342300       MOVE W-KDLPORS-TAB(1)     TO LPF-KDLPORS(1)                        
342400       MOVE W-KDLPORS-TAB(2)     TO LPF-KDLPORS(2)                        
342500       MOVE W-KDLPORS-TAB(3)     TO LPF-KDLPORS(3)                        
342600       MOVE CLAG-KDLEVPLF        TO LPF-KDLEVPLF                          
342700       MOVE CLAG-TIOMSPEC        TO LPF-TIOMSPEC                          
342800       MOVE W-DATUM-AAMMDD       TO LPF-TIUPPDAT                          
342900       MOVE SPACE                TO LPF-TELPORSX                          
343000*    -- HÄMTA BENÄMNING                                                   
343100       MOVE 'BENÄMNING SAKNAS'   TO LPF-BEART                             
343200       PERFORM IMS-GU-WDD311-BSEQ-SVE                                     
343300       IF SEGMENT-FINNS                                                   
343400         MOVE TEXT-BEART         TO LPF-BEART                             
343500       ELSE                                                               
343600         PERFORM IMS-GU-WDD311-BSEQ-ENG                                   
343700         IF SEGMENT-FINNS                                                 
343800           MOVE TEXT-BEART       TO LPF-BEART                             
343900         END-IF                                                           
344000       END-IF                                                             
344100                                                                          
344200       PERFORM IMS-REPL-WDD601                                            
344300     ELSE                                                                 
344400*    --- LÄGGER UPP NYTT LEV.PLANEFÖRSLAG PÅ WDD6-KÖN                     
344500       MOVE WC-CDC-SE            TO LPF-IDDC                              
344600       MOVE W-IDLEVNR            TO LPF-IDLEVNR                           
344700       MOVE W-IDARTNR            TO LPF-IDARTNR                           
344800       MOVE W-IDANSK             TO LPF-IDANSK                            
344900       MOVE W-KDLPORS-TAB(1)     TO LPF-KDLPORS(1)                        
345000       MOVE W-KDLPORS-TAB(2)     TO LPF-KDLPORS(2)                        
345100       MOVE W-KDLPORS-TAB(3)     TO LPF-KDLPORS(3)                        
345200       MOVE CLAG-KDLEVPLF        TO LPF-KDLEVPLF                          
345300       MOVE CLAG-TIOMSPEC        TO LPF-TIOMSPEC                          
345400       MOVE W-DATUM-AAMMDD       TO LPF-TIUPPDAT                          
345500       MOVE SPACE                TO LPF-TELPORSX                          
345600*    -- HÄMTA BENÄMNING                                                   
345700       MOVE 'BENÄMNING SAKNAS'   TO LPF-BEART                             
345800       PERFORM IMS-GU-WDD311-BSEQ-SVE                                     
345900       IF SEGMENT-FINNS                                                   
346000         MOVE TEXT-BEART         TO LPF-BEART                             
346100       ELSE                                                               
346200         PERFORM IMS-GU-WDD311-BSEQ-ENG                                   
346300         IF SEGMENT-FINNS                                                 
346400           MOVE TEXT-BEART       TO LPF-BEART                             
346500         END-IF                                                           
346600       END-IF                                                             
346700                                                                          
346800       PERFORM IMS-ISRT-WDD601                                            
346900     END-IF                                                               
347000                                                                          
347100* SE OM DET FINNS FLER RADER MED ANNAT IDANSK? DUBLETTER.                 
347200* DESSA MÅSTE DELETE'S ANNARS ABENDAR W2215900, W200V1.                   
347300     MOVE ZERO                 TO W-IDANSK-MIN                            
347400     MOVE 999                  TO W-IDANSK-MAX                            
347500     MOVE W-IDANSK             TO W-IDANSK-NON                            
347600     PERFORM IMS-GHU-WDD601-FLERA                                         
347700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
347800       PERFORM IMS-DLET-WDD601                                            
347900       PERFORM IMS-GHN-WDD601-FLERA                                       
348000     END-PERFORM                                                          
348100                                                                          
348200     .                                                                    
348300     EJECT                                                                
348400 S01-BERAKNA-VECKODIFFERENS SECTION.                                      
348500     MOVE 'S01-BERAKNA-VECKODIFFERENS '  TO CURRENT-SECTION               
348600                                                                          
348700     MOVE +0                      TO W-VECKO-DIFFERENS                    
348800                                                                          
348900     MOVE W-DATUM-FROM            TO W-TMP1-DATUM-FROM                    
349000     MOVE W-DATUM-TOM             TO W-TMP2-DATUM-TO                      
349100     PERFORM UNTIL W-TMP1-DATUM-FROM = W-TMP2-DATUM-TO                    
349200                                                                          
349300       MOVE 'AAVV  '              TO DAT-KDDATFORM                        
349400       MOVE W-TMP1-DATUM-FROM     TO DAT-I-TIDATUM                        
349500       CALL WDATKONV USING DAT-KDDATFORM                                  
349600            DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                        
349700       IF DAT-KDSVAR-FEL                                                  
349800          ADD  +1                 TO W-TMP1-DAT-FROM-AA                   
349900          MOVE +1                 TO W-TMP1-DAT-FROM-VV                   
350000       ELSE                                                               
350100         ADD +1                   TO W-VECKO-DIFFERENS                    
350200         IF DAT-TIVV = +53                                                
350300            ADD  +1               TO W-TMP1-DAT-FROM-AA                   
350400            MOVE +1               TO W-TMP1-DAT-FROM-VV                   
350500         ELSE                                                             
350600            ADD +1                TO W-TMP1-DAT-FROM-VV                   
350700         END-IF                                                           
350800       END-IF                                                             
350900                                                                          
351000     END-PERFORM                                                          
351100     .                                                                    
351200     EJECT                                                                
351300 S02-STARTA-2103-TRANS SECTION.                                           
351400     MOVE 'S02-STARTA-2103-TRANS '  TO CURRENT-SECTION                    
351500                                                                          
351600     MOVE WS-IDARTNR           TO  MOD2103-MID-IDARTNR-IN                 
351700     COMPUTE P-TO-P-KVLL       =   LNG-P-TO-P-PREFIX                      
351800                                   + 23 + 9                               
351900                                                                          
352000     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
352100     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
352200     MOVE 'W2T103  '           TO P-TO-P-KDTRANS                          
352300     MOVE '2135'               TO P-TO-P-IDTRANS                          
352400     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
352500                                                                          
352600     MOVE MOD2103-MID-W2I10301 TO P-TO-P-DATA                             
352700     PERFORM IMS-ISRT-ALT-MSG-2103                                        
352800                                                                          
352900     .                                                                    
353000     EJECT                                                                
353100 S03-TILEVDAG-DAGL-AVROP   SECTION.                                       
353200     MOVE 'S03-TILEVDAG-DAGL-AVROP '  TO CURRENT-SECTION                  
353300                                                                          
353400     MOVE ZERO                TO W-TILEVDAG (1)                           
353500                                 W-TILEVDAG (2)                           
353600                                 W-TILEVDAG (3)                           
353700                                 W-TILEVDAG (4)                           
353800                                 W-TILEVDAG (5)                           
353900     MOVE ZERO                TO ANT-LEVDAG                               
354000                                                                          
354100     MOVE +1                  TO IX-DAG                                   
354200     PERFORM UNTIL IX-DAG > 5                                             
354300        IF CLAG-TILEVDAG (IX-DAG) > ZERO                                  
354400           MOVE CLAG-TILEVDAG (IX-DAG) TO                                 
354500                W-TILEVDAG (IX-DAG)                                       
354600           ADD +1             TO ANT-LEVDAG                               
354700        END-IF                                                            
354800        ADD +1                TO IX-DAG                                   
354900     END-PERFORM                                                          
355000     IF ANT-LEVDAG = ZERO                                                 
355100        MOVE CLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                        
355200        PERFORM IMS-GET-LEVA01-WDF101-SHIP                                
355300        IF SEGMENT-FINNS                                                  
355400          MOVE +1                  TO IX-DAG                              
355500          PERFORM UNTIL IX-DAG > 5                                        
355600             IF F1-LEV-TILEVDAG (IX-DAG) > ZERO                           
355700                MOVE F1-LEV-TILEVDAG (IX-DAG) TO                          
355800                     W-TILEVDAG (IX-DAG)                                  
355900                ADD +1             TO ANT-LEVDAG                          
356000             END-IF                                                       
356100             ADD +1                TO IX-DAG                              
356200          END-PERFORM                                                     
356300          IF ANT-LEVDAG = ZERO                                            
356400             MOVE +1               TO W-TILEVDAG (1)                      
356500                                      ANT-LEVDAG                          
356600          END-IF                                                          
356700        ELSE                                                              
356800          MOVE +1                  TO W-TILEVDAG (1)                      
356900                                      ANT-LEVDAG                          
357000        END-IF                                                            
357100     END-IF                                                               
357200                                                                          
357300*--- FLYTTA TILL W221BLOC                                                 
357400     MOVE W-TILEVDAG (1)  TO BLOC-TILEVDAG-DAGL(1)                        
357500     MOVE W-TILEVDAG (2)  TO BLOC-TILEVDAG-DAGL(2)                        
357600     MOVE W-TILEVDAG (3)  TO BLOC-TILEVDAG-DAGL(3)                        
357700     MOVE W-TILEVDAG (4)  TO BLOC-TILEVDAG-DAGL(4)                        
357800     MOVE W-TILEVDAG (5)  TO BLOC-TILEVDAG-DAGL(5)                        
357900                                                                          
358000     .                                                                    
358100     EJECT                                                                
358200                                                                          
358300 S04-NOLLA-W22222 SECTION.                                                
358400                                                                          
358500     MOVE +0                         TO LINK-KVBEHOV-SUMMA                
358600                                        LINK-KVBEHOV-DESSUTOM             
358700                                        LINK-TIBEHOV-FIRST                
358800                                                                          
358900     MOVE +1                         TO INDX                              
359000     PERFORM UNTIL INDX >  MAX-INDX                                       
359100       MOVE +0                      TO LINK-KVBEHOV-VECKA(INDX)           
359200       ADD +1                        TO INDX                              
359300     END-PERFORM                                                          
359400     .                                                                    
359500     EJECT                                                                
359600 IMS-GET-MSG SECTION.                                                     
359700                                                                          
359800     MOVE '  QC' TO GODK-STATUSKODER                                      
359900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
360000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
360100     PERFORM IMS-STATUSKONTROLL                                           
360200     .                                                                    
360300     SKIP3                                                                
360400 IMS-INSERT-MSG SECTION.                                                  
360500                                                                          
360600     IF ENGLISH-TEXT                                                      
360700       MOVE 'N' TO MFS-KDHUVOMR                                           
360800     END-IF                                                               
360900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
361000     MOVE SPACE TO GODK-STATUSKODER                                       
361100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
361200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
361300     PERFORM IMS-STATUSKONTROLL                                           
361400     .                                                                    
361500     EJECT                                                                
361600 IMS-GET-ARTC-WLARTC01 SECTION.                                           
361700     MOVE 'IMS-GET-ARTC-WLARTC01 '  TO DBS-SECTION                        
361800                                                                          
361900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
362000          DELIMITED BY SIZE INTO SSA1                                     
362100     MOVE '  GE' TO GODK-STATUSKODER                                      
362200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
362300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
362400     PERFORM IMS-STATUSKONTROLL                                           
362500     .                                                                    
362600     EJECT                                                                
362700 IMS-ISRT-ALT-MSG-2103 SECTION.                                           
362800     MOVE 'IMS-ISRT-ALT-MSG-2103 '  TO DBS-SECTION                        
362900                                                                          
363000     MOVE SPACE TO GODK-STATUSKODER                                       
363100     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
363200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
363300     PERFORM IMS-STATUSKONTROLL                                           
363400     .                                                                    
363500     SKIP2                                                                
363600 IMS-GET-ARTC-WLARTC11 SECTION.                                           
363700     MOVE 'IMS-GET-ARTC-WLARTC11 '  TO DBS-SECTION                        
363800                                                                          
363900     MOVE 'WLARTC11*F'   TO SSA1                                          
364000     MOVE '  GE' TO GODK-STATUSKODER                                      
364100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA2 SSA1                   
364200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
364300     PERFORM IMS-STATUSKONTROLL                                           
364400     .                                                                    
364500     EJECT                                                                
364600 IMS-GET-ARTC-WLARTC21 SECTION.                                           
364700     MOVE 'IMS-GET-ARTC-WLARTC21'  TO DBS-SECTION                         
364800                                                                          
364900     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
365000          DELIMITED BY SIZE INTO SSA1                                     
365100     MOVE '  GE' TO GODK-STATUSKODER                                      
365200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC21 SSA1                 
365300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
365400     PERFORM IMS-STATUSKONTROLL                                           
365500     .                                                                    
365600     EJECT                                                                
365700 IMS-GET-ARTC-WLARTC26 SECTION.                                           
365800     MOVE 'IMS-GET-ARTC-WLARTC26 '  TO DBS-SECTION                        
365900                                                                          
366000     MOVE 'WLARTC11'   TO SSA1                                            
366100     MOVE 'WLARTC26'   TO SSA2                                            
366200     MOVE '  GE' TO GODK-STATUSKODER                                      
366300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA13 SSA1 SSA2             
366400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
366500     PERFORM IMS-STATUSKONTROLL                                           
366600     .                                                                    
366700     EJECT                                                                
366800 IMS-REPL-ARTC-WLARTC11 SECTION.                                          
366900     MOVE 'IMS-REPL-ARTC-WLARTC11'  TO DBS-SECTION                        
367000                                                                          
367100     MOVE '  ' TO GODK-STATUSKODER                                        
367200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA2                        
367300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
367400     PERFORM IMS-STATUSKONTROLL                                           
367500     .                                                                    
367600     EJECT                                                                
367700 IMS-GET-INLB-WLINLB01 SECTION.                                           
367800     MOVE 'IMS-GET-INLB-WLINLB01 '  TO DBS-SECTION                        
367900                                                                          
368000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
368100          DELIMITED BY SIZE INTO SSA1                                     
368200     MOVE '  GE' TO GODK-STATUSKODER                                      
368300     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA7 SSA1                     
368400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
368500     PERFORM IMS-STATUSKONTROLL                                           
368600     .                                                                    
368700     EJECT                                                                
368800 IMS-GET-INLB-WLINLB11-FIRST SECTION.                                     
368900     MOVE 'IMS-GET-INLB-WLINLB11-FIRST'  TO DBS-SECTION                   
369000                                                                          
369100     MOVE 'WLINLB11*F'   TO SSA1                                          
369200     MOVE '  ' TO GODK-STATUSKODER                                        
369300     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA8 SSA1                   
369400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
369500     PERFORM IMS-STATUSKONTROLL                                           
369600     .                                                                    
369700     SKIP3                                                                
369800 IMS-GET-INLB-WLINLB11-NEXT SECTION.                                      
369900     MOVE 'IMS-GET-INLB-WLINLB11-NEXT'  TO DBS-SECTION                    
370000                                                                          
370100     MOVE 'WLINLB11'   TO SSA1                                            
370200     MOVE '  GE' TO GODK-STATUSKODER                                      
370300     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA8 SSA1                   
370400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
370500     PERFORM IMS-STATUSKONTROLL                                           
370600     .                                                                    
370700     SKIP3                                                                
370800 IMS-GET-INLB-WLINLB22 SECTION.                                           
370900     MOVE 'IMS-GET-INLB-WLINLB22 '  TO DBS-SECTION                        
371000                                                                          
371100     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
371200          DELIMITED BY SIZE INTO SSA1                                     
371300     MOVE   'WLINLB22'   TO SSA2                                          
371400     MOVE '  GE' TO GODK-STATUSKODER                                      
371500     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA9 SSA1 SSA2              
371600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
371700     PERFORM IMS-STATUSKONTROLL                                           
371800     .                                                                    
371900     SKIP3                                                                
372000 IMS-ISRT-INLB-WLINLB22 SECTION.                                          
372100     MOVE 'IMS-ISRT-INLB-WLINLB22 '  TO DBS-SECTION                       
372200                                                                          
372300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
372400          DELIMITED BY SIZE INTO SSA1                                     
372500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
372600          DELIMITED BY SIZE INTO SSA2                                     
372700     MOVE 'WLINLB22 ' TO SSA3                                             
372800     MOVE '  II' TO GODK-STATUSKODER                                      
372900     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA9 SSA1 SSA2 SSA3         
373000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
373100     PERFORM IMS-STATUSKONTROLL                                           
373200     .                                                                    
373300     EJECT                                                                
373400 IMS-GET-INLB-WLINLB23-FIRST SECTION.                                     
373500     MOVE 'IMS-GET-INLB-WLINLB23-FIRST'  TO DBS-SECTION                   
373600                                                                          
373700     MOVE   'WLINLB11*F' TO SSA1                                          
373800     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
373900          DELIMITED BY SIZE INTO SSA2                                     
374000     MOVE '  GE' TO GODK-STATUSKODER                                      
374100     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA10 SSA1 SSA2             
374200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
374300     PERFORM IMS-STATUSKONTROLL                                           
374400     .                                                                    
374500     SKIP3                                                                
374600 IMS-GET-INLB-WLINLB23-NEXT SECTION.                                      
374700     MOVE 'IMS-GET-INLB-WLINLB23-NEXT'  TO DBS-SECTION                    
374800                                                                          
374900     MOVE   'WLINLB11' TO SSA1                                            
375000     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
375100          DELIMITED BY SIZE INTO SSA2                                     
375200     MOVE '  GE' TO GODK-STATUSKODER                                      
375300     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA10 SSA1 SSA2             
375400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
375500     PERFORM IMS-STATUSKONTROLL                                           
375600     .                                                                    
375700     SKIP3                                                                
375800 IMS-GHU-INLB-WLINLB23 SECTION.                                           
375900     MOVE 'IMS-GHU-INLB-WLINLB23 '  TO DBS-SECTION                        
376000                                                                          
376100     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
376200          DELIMITED BY SIZE INTO SSA1                                     
376300     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
376400          DELIMITED BY SIZE INTO SSA2                                     
376500     STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X                            
376600                    '&KDAVROP  =' W-KDAVROP-X ')'                         
376700          DELIMITED BY SIZE INTO SSA3                                     
376800     MOVE '  GE' TO GODK-STATUSKODER                                      
376900     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA1A                        
377000                                            SSA1 SSA2 SSA3                
377100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
377200     PERFORM IMS-STATUSKONTROLL                                           
377300     .                                                                    
377400     SKIP3                                                                
377500 IMS-ISRT-INLB-WLINLB23 SECTION.                                          
377600     MOVE 'IMS-ISRT-INLB-WLINLB23 '  TO DBS-SECTION                       
377700                                                                          
377800     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
377900          DELIMITED BY SIZE INTO SSA1                                     
378000     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
378100          DELIMITED BY SIZE INTO SSA2                                     
378200     MOVE 'WLINLB23 ' TO SSA3                                             
378300     MOVE '  II' TO GODK-STATUSKODER                                      
378400     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA10                       
378500                                            SSA1 SSA2 SSA3                
378600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
378700     PERFORM IMS-STATUSKONTROLL                                           
378800     .                                                                    
378900     SKIP3                                                                
379000 IMS-REPL-INLB-WLINLB23 SECTION.                                          
379100     MOVE 'IMS-REPL-INLB-WLINLB23 '  TO DBS-SECTION                       
379200                                                                          
379300     MOVE '    ' TO GODK-STATUSKODER                                      
379400     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA10                       
379500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
379600     PERFORM IMS-STATUSKONTROLL                                           
379700     .                                                                    
379800     SKIP3                                                                
379900 IMS-DLET-INLB-WLINLB22 SECTION.                                          
380000     MOVE 'IMS-DLET-INLB-WLINLB22 '  TO DBS-SECTION                       
380100                                                                          
380200     MOVE '  ' TO GODK-STATUSKODER                                        
380300     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA9                        
380400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
380500     PERFORM IMS-STATUSKONTROLL                                           
380600     .                                                                    
380700     EJECT                                                                
380800 IMS-DLET-INLB-WLINLB23 SECTION.                                          
380900     MOVE 'IMS-DLET-INLB-WLINLB23 ' TO DBS-SECTION                        
381000                                                                          
381100     MOVE '  ' TO GODK-STATUSKODER                                        
381200     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA10                       
381300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
381400     PERFORM IMS-STATUSKONTROLL                                           
381500     .                                                                    
381600     EJECT                                                                
381700 IMS-GET-ARTM-WLARTM01 SECTION.                                           
381800     MOVE 'IMS-GET-ARTM-WLARTM01 '  TO DBS-SECTION                        
381900                                                                          
382000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
382100          DELIMITED BY SIZE INTO SSA1                                     
382200     MOVE '  GE' TO GODK-STATUSKODER                                      
382300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA11 SSA1                    
382400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
382500     PERFORM IMS-STATUSKONTROLL                                           
382600     .                                                                    
382700     EJECT                                                                
382800 IMS-GET-WDK701-SDC    SECTION.                                           
382900     MOVE 'IMS-GET-WDK701-SDC '  TO DBS-SECTION                           
383000                                                                          
383100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
383200          DELIMITED BY SIZE INTO SSA1                                     
383300     MOVE '  GE' TO GODK-STATUSKODER                                      
383400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA14 SSA1                    
383500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
383600     PERFORM IMS-STATUSKONTROLL                                           
383700     .                                                                    
383800     SKIP2                                                                
383900 IMS-GNP-WDK711-SDC-REF SECTION.                                          
384000     MOVE 'IMS-GNP-WDK711-SDC-REF '  TO DBS-SECTION                       
384100                                                                          
384200     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
384300          DELIMITED BY SIZE INTO SSA1                                     
384400     MOVE '  GE' TO GODK-STATUSKODER                                      
384500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA14 SSA1                   
384600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
384700     PERFORM IMS-STATUSKONTROLL                                           
384800     .                                                                    
384900     EJECT                                                                
385000 IMS-GET-INLE01-WDL201 SECTION.                                           
385100     MOVE 'IMS-GET-INLE01-WDL201 '  TO DBS-SECTION                        
385200                                                                          
385300     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
385400          DELIMITED BY SIZE INTO SSA1                                     
385500     MOVE '  GE' TO GODK-STATUSKODER                                      
385600     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA21 SSA1                    
385700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
385800     PERFORM IMS-STATUSKONTROLL                                           
385900     .                                                                    
386000     EJECT                                                                
386100 IMS-GET-INLE21-WDL221 SECTION.                                           
386200     MOVE 'IMS-GET-INLE21-WDL221 '  TO DBS-SECTION                        
386300                                                                          
386400     STRING 'WLINLE11   '                                                 
386500          DELIMITED BY SIZE INTO SSA1                                     
386600     STRING 'WLINLE21(IDPTYP   =' W-IDPTYP-X ')'                          
386700          DELIMITED BY SIZE INTO SSA2                                     
386800     MOVE '  GE' TO GODK-STATUSKODER                                      
386900     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA21 SSA1 SSA2              
387000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
387100     PERFORM IMS-STATUSKONTROLL                                           
387200     .                                                                    
387300     EJECT                                                                
387400 IMS-GET-LEVA01-WDF101-SHIP SECTION.                                      
387500     MOVE 'IMS-GET-LEVA01-WDF101-SHIP' TO DBS-SECTION                     
387600                                                                          
387700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
387800          DELIMITED BY SIZE INTO SSA1                                     
387900     MOVE '  GE' TO GODK-STATUSKODER                                      
388000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
388100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
388200     PERFORM IMS-STATUSKONTROLL                                           
388300     .                                                                    
388400     SKIP3                                                                
388500 IMS-GET-LEVA14-WDF106-SHIP SECTION.                                      
388600     MOVE 'IMS-GET-LEVA14-WDF106-SHIP'  TO DBS-SECTION                    
388700                                                                          
388800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
388900          DELIMITED BY SIZE INTO SSA1                                     
389000     STRING 'WDF106     '                                                 
389100          DELIMITED BY SIZE INTO SSA2                                     
389200     MOVE '  GE' TO GODK-STATUSKODER                                      
389300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F106 SSA1 SSA2            
389400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
389500     PERFORM IMS-STATUSKONTROLL                                           
389600     .                                                                    
389700     EJECT                                                                
389800 IMS-GET-WDF301 SECTION.                                                  
389900     MOVE 'IMS-GET-WDF301 '  TO DBS-SECTION                               
390000                                                                          
390100     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
390200          DELIMITED BY SIZE INTO SSA1                                     
390300     MOVE '  GE' TO GODK-STATUSKODER                                      
390400     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-AREA-F301 SSA1                 
390500     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
390600     PERFORM IMS-STATUSKONTROLL                                           
390700     .                                                                    
390800     SKIP3                                                                
390900 IMS-GU-WDD311-BSEQ-SVE   SECTION.                                        
391000     MOVE 'IMS-GU-WDD311-BSEQ-SVE '  TO DBS-SECTION                       
391100                                                                          
391200     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
391300          DELIMITED BY SIZE INTO SSA1                                     
391400     MOVE   'WDD311  (IDSKYLT  =S  )' TO SSA2                             
391500     MOVE '  GE' TO GODK-STATUSKODER                                      
391600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD3 SSA1 SSA2            
391700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
391800     PERFORM IMS-STATUSKONTROLL                                           
391900     .                                                                    
392000     SKIP2                                                                
392100 IMS-GU-WDD311-BSEQ-ENG   SECTION.                                        
392200     MOVE 'IMS-GU-WDD311-BSEQ-ENG '  TO DBS-SECTION                       
392300                                                                          
392400     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
392500          DELIMITED BY SIZE INTO SSA1                                     
392600     MOVE   'WDD311  (IDSKYLT  =GB )' TO SSA2                             
392700     MOVE '  GE' TO GODK-STATUSKODER                                      
392800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD3 SSA1 SSA2            
392900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
393000     PERFORM IMS-STATUSKONTROLL                                           
393100     .                                                                    
393200     SKIP2                                                                
393300 IMS-GHU-WDD601 SECTION.                                                  
393400     MOVE 'IMS-GHU-WDD601 '  TO DBS-SECTION                               
393500                                                                          
393600     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
393700                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X  ')'                   
393800          DELIMITED BY SIZE INTO SSA1                                     
393900     MOVE '  GE' TO GODK-STATUSKODER                                      
394000     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-AREA-WDD6 SSA1                
394100     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
394200     PERFORM IMS-STATUSKONTROLL                                           
394300     .                                                                    
394400     SKIP3                                                                
394500 IMS-GHU-WDD601-FLERA SECTION.                                            
394600     MOVE 'IMS-GHU-WDD601-FLERA' TO DBS-SECTION                           
394700                                                                          
394800     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
394900                    '&WDD601KY<=' W-WDD601KY-MAX-X                        
395000                    '&IDANSK  NE' W-IDANSK-NON-X ')'                      
395100          DELIMITED BY SIZE INTO SSA1                                     
395200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
395300     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-AREA-WDD6 SSA1                
395400     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
395500     PERFORM IMS-STATUSKONTROLL                                           
395600     .                                                                    
395700     SKIP3                                                                
395800 IMS-GHN-WDD601-FLERA SECTION.                                            
395900     MOVE 'IMS-GHN-WDD601-FLERA' TO DBS-SECTION                           
396000                                                                          
396100     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
396200                    '&WDD601KY<=' W-WDD601KY-MAX-X                        
396300                    '&IDANSK  NE' W-IDANSK-NON-X ')'                      
396400          DELIMITED BY SIZE INTO SSA1                                     
396500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
396600     CALL CBLTDLI USING GHN WDD6-PCB DLI-IO-AREA-WDD6 SSA1                
396700     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
396800     PERFORM IMS-STATUSKONTROLL                                           
396900     .                                                                    
397000     SKIP3                                                                
397100 IMS-REPL-WDD601        SECTION.                                          
397200     MOVE 'IMS-REPL-WDD601 '  TO DBS-SECTION                              
397300                                                                          
397400     MOVE '  ' TO GODK-STATUSKODER                                        
397500     CALL CBLTDLI USING REPL WDD6-PCB DLI-IO-AREA-WDD6                    
397600     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
397700     PERFORM IMS-STATUSKONTROLL                                           
397800     .                                                                    
397900     EJECT                                                                
398000 IMS-ISRT-WDD601        SECTION.                                          
398100     MOVE 'IMS-ISRT-WDD601 '  TO DBS-SECTION                              
398200                                                                          
398300     MOVE 'WDD601   ' TO SSA1                                             
398400     MOVE '  ' TO GODK-STATUSKODER                                        
398500     CALL CBLTDLI USING ISRT WDD6-PCB DLI-IO-AREA-WDD6  SSA1              
398600     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
398700     PERFORM IMS-STATUSKONTROLL                                           
398800     .                                                                    
398900     EJECT                                                                
399000                                                                          
399100 IMS-DLET-WDD601        SECTION.                                          
399200     MOVE 'IMS-DLET-WDD601 '  TO DBS-SECTION                              
399300                                                                          
399400     MOVE '  ' TO GODK-STATUSKODER                                        
399500     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-AREA-WDD6                    
399600     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
399700     PERFORM IMS-STATUSKONTROLL                                           
399800     .                                                                    
399900     EJECT                                                                
400000                                                                          
400100 IMS-GU-WDB601    SECTION.                                                
400200     MOVE 'IMS-GU-WDB601  '  TO DBS-SECTION                               
400300                                                                          
400400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
400500          DELIMITED BY SIZE INTO SSA1                                     
400600     MOVE '  ' TO GODK-STATUSKODER                                        
400700     CALL CBLTDLI USING GU WDB6-2-PCB  DLI-IO-AREA-2-B6 SSA1              
400800     MOVE WDB6-2-STATUS-CODE    TO STATUS-WS                              
400900     PERFORM IMS-STATUSKONTROLL                                           
401000     .                                                                    
401100     EJECT                                                                
401200 IMS-GU-WDGX2258  SECTION.                                                
401300     MOVE 'IMS-GU-WDGX2258 '  TO DBS-SECTION                              
401400                                                                          
401500     MOVE SPACES              TO SSA1 SSA2                                
401600     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
401700          DELIMITED BY SIZE INTO SSA1                                     
401800     STRING 'WDGX2258(IDLEVNRS =' W-WDGXKEY-2258-X ')'                    
401900          DELIMITED BY SIZE INTO SSA2                                     
402000     MOVE '  GE'              TO GODK-STATUSKODER                         
402100     CALL CBLTDLI USING GU 2257-PCB DLI-IO-WDGX2258 SSA1 SSA2             
402200     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
402300     PERFORM IMS-STATUSKONTROLL                                           
402400     .                                                                    
402500     SKIP3                                                                
402600 IMS-GNP-WDGX2260  SECTION.                                               
402700     MOVE 'IMS-GNP-WDGX2260 '  TO DBS-SECTION                             
402800                                                                          
402900     MOVE SPACE               TO SSA1                                     
403000     STRING 'WDGX2260(DAAVROPF<=' W-DAAVROP-2260-X                        
403100                    '&DAAVROPT>=' W-DAAVROP-2260-X                        
403200                    '&IDANSKF <=' W-IDANSK-2260-X                         
403300                    '&IDANSKT >=' W-IDANSK-2260-X ')'                     
403400          DELIMITED BY SIZE INTO SSA1                                     
403500     MOVE '  GE'              TO GODK-STATUSKODER                         
403600     CALL CBLTDLI USING GNP 2257-PCB DLI-IO-WDGX2260 SSA1                 
403700     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
403800     PERFORM IMS-STATUSKONTROLL                                           
403900     .                                                                    
404000     EJECT                                                                
404100 IMS-STATUSKONTROLL SECTION.                                              
404200                                                                          
404300     SET STATUS-IX TO 1                                                   
404400     SEARCH GODK-STATUS                                                   
404500       AT END                                                             
404600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
404700         DELIMITED BY SIZE INTO FELTEXT                                   
404800         CALL FELLOG                                                      
404900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
405000         CONTINUE                                                         
405100     END-SEARCH                                                           
405200     .                                                                    
405300     EJECT                                                                
405400*    -COPY WY2000P9                                                       
405500     EJECT                                                                
405600*    -COPY WY2000P2                                                       
405700     EJECT                                                                
405800*    -COPY WY2000P3                                                       
