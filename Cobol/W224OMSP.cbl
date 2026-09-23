000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W224OMSP.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RECALCULATION OF DELIVERY SCHEDULES                              
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDD9                                       
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDF1                                       
001400*        PROGRAMMET LÄSER      WDF3                                       
001500*        PROGRAMMET LÄSER      WDB6                                       
001600*        PROGRAMMET LÄSER      WDD3                                       
001700*        PROGRAMMET UPPDATERAR WDD6                                       
001800*                                                                         
001900*    LÄNKAREA:                                                            
002000*        W224OMSP                                                         
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600****--------------------------------------------------------------        
002700*--- PROGRAMÄNDRINGAR                                                     
002800****--------------------------------------------------------------        
002900*                                                                         
003000* 2014-06-05  E'TRACKER 10230472  RÄTTNING AV BUGG VID                    
003100*                                 HELGDAGSJUSTERING.                      
003200*                                                                         
003300* 2014-11-20  E'TRACKER 10245983  DATKONVJUST JUL/NYÅR 2014               
003400*                                                                         
003500* 2015-01-09  E'TRACKER 10248858  TAG BORT FELAKTIG BUGGFIX,RÄTTA         
003600*                                 HELGDAGSJUSTERING VECKOAVROP.           
003700*                                                                         
003800* 2015-04-22  E'TRACKER 10130993                                          
003900*             REDUCE NUMBER OF DELIVERY SCHEDULES                         
004000*                                                                         
004100* 2015-09-08  E'TRACKER 10209749                                          
004200*             TA BORT EXTRALEVERANSER (WDD903).                           
004300*                                                                         
004400* 2015-11-12  E'TRACKER 10243132  KINA EXPORT 2015                        
004500*                                                                         
004600* 2017-03-01  E'TRACKER 10298528 RÄTTA FEL LÄSNING WDF116, ABEND          
004700*                                                                         
004800* 2017-08-18  E'TRACKER 10299286  KINA LOCAL SOURCING USA.                
004900*                                 GEMENSAMT CCID:10302687 REFILL          
005000*                                                                         
005100                                                                          
005200                                                                          
005300 ENVIRONMENT DIVISION.                                                    
005400 INPUT-OUTPUT SECTION.                                                    
005500                                                                          
005600 FILE-CONTROL.                                                            
005700 DATA DIVISION.                                                           
005800 FILE SECTION.                                                            
005900                                                                          
006000 WORKING-STORAGE SECTION.                                                 
006100*    -COPY WY2000W3                                                       
006200     SKIP3                                                                
006300*    -COPY WY2000W2                                                       
006400     SKIP3                                                                
006500*    -COPY WY2000W9                                                       
006600     SKIP3                                                                
006700                                                                          
006800 77  IDPGM                       PIC X(8)    VALUE 'W224OMSP'.            
006900 77  CURRENT-SECTION             PIC X(32)      VALUE SPACE.              
007000 77  CURRENT-IMS-SECTION         PIC X(32)      VALUE SPACE.              
007100 77  FELTEXT                     PIC X(80)      VALUE SPACE.              
007200                                                                          
007300 77  JA                          PIC X       VALUE 'J'.                   
007400 77  NEJ                         PIC X       VALUE 'N'.                   
007500                                                                          
007600 01  W-DATUM-AAVV                PIC 9(4)       VALUE ZERO.               
007700 01  W-DATUM-AAVVD               PIC 9(5)       VALUE ZERO.               
007800 01  W-DAT-TID                   PIC 9(1)       VALUE ZERO.               
007900 01  W-DATUM-AAVV-HELP           PIC S9(5)                 COMP-3.        
008000 01  W-DAPUBL                    PIC 9(8).                                
008100 01  FILLER REDEFINES W-DAPUBL.                                           
008200     03  W-DAPUBL-SS             PIC 9(2).                                
008300     03  W-DAPUBL-AAMMDD         PIC 9(6).                                
008400 01  W-DATUM-FROM                PIC 9(4).                                
008500 01  FILLER REDEFINES W-DATUM-FROM.                                       
008600     03  W-DATUM-FROM-AA         PIC 9(2).                                
008700     03  W-DATUM-FROM-VV         PIC 9(2).                                
008800 01  W-DATUM-TOM                 PIC 9(4).                                
008900 01  FILLER REDEFINES W-DATUM-TOM.                                        
009000     03  W-DATUM-TOM-AA          PIC 9(2).                                
009100     03  W-DATUM-TOM-VV          PIC 9(2).                                
009200                                                                          
009300*--- DAGENS DATUM FRÅN WDATKONV.                                          
009400 01  W-DATUM-AAMMDD-AKT          PIC 9(6) VALUE ZERO.                     
009500 01  W-DATUM-SSAAMMDD-AKT        PIC 9(8) VALUE ZERO.                     
009600 01  FILLER REDEFINES W-DATUM-SSAAMMDD-AKT.                               
009700     03  W-DATUM-SS-AKT          PIC 9(2).                                
009800     03  W-DATUM-AA-AKT          PIC 9(2).                                
009900     03  W-DATUM-MM-AKT          PIC 9(2).                                
010000     03  W-DATUM-DD-AKT          PIC 9(2).                                
010100                                                                          
010200 01  W-DAT-TID-AKT               PIC 9(1) VALUE ZERO.                     
010300 01  W-DATUM-AAVV-AKT            PIC 9(4) VALUE ZERO.                     
010400 01  W-DATUM-VV-AKT              PIC 9(2) VALUE ZERO.                     
010500 01  W-DATUM-AAVVD-AKT           PIC 9(5) VALUE ZERO.                     
010600 01  W-AKT-SEASON                PIC 9(2) VALUE ZERO.                     
010700                                                                          
010800 01  W-DATUM-AAMMDD              PIC 9(6).                                
010900 01  W-DAT2-AAMMDD     REDEFINES W-DATUM-AAMMDD.                          
011000     03  W-DATUM2-AA             PIC 9(2).                                
011100     03  W-DATUM2-MM             PIC 9(2).                                
011200     03  W-DATUM2-DD             PIC 9(2).                                
011300                                                                          
011400 01  W-TIFINLV-AAVVD         PIC 9(5) VALUE ZERO.                         
011500 01  FILLER REDEFINES W-TIFINLV-AAVVD.                                    
011600     03  W-TIFINLV-AA      PIC 9(2).                                      
011700     03  W-TIFINLV-VV      PIC 9(2).                                      
011800     03  W-FILLER          PIC 9(1).                                      
011900 01  FILLER REDEFINES W-TIFINLV-AAVVD.                                    
012000     03  W-TIFINLV-AAVV    PIC 9(4).                                      
012100     03  W-FILLER          PIC 9(1).                                      
012200 01  W-TIFINLV-CCAAMMDD    PIC 9(8)   VALUE ZERO.                         
012300 01  FILLER REDEFINES W-TIFINLV-CCAAMMDD.                                 
012400     03  W-TIFINLV-CC      PIC 9(2).                                      
012500     03  W-TIFINLV-AAMMDD  PIC 9(6).                                      
012600                                                                          
012700 01  VECKO-DIFF            PIC S9(3)  COMP-3.                             
012800 01  W-TIERSDAT            PIC  9(5)  VALUE ZERO.                         
012900 01  FILLER REDEFINES W-TIERSDAT.                                         
013000     03  W-TIERSDAT-AAVV   PIC 9(4).                                      
013100     03  FILLER            PIC 9(1).                                      
013200                                                                          
013300*-----                                                                    
013400                                                                          
013500 01  W-DAAVROP-AVS               PIC 9(6).                                
013600 01  FILLER REDEFINES W-DAAVROP-AVS.                                      
013700     03  W-DAAVROP-SS            PIC 9(2).                                
013800     03  W-DAAVROP-AAVV          PIC 9(4).                                
013900 01  WS-TIAAVV                   PIC 9(04)   VALUE ZERO.                  
014000 01  W-TIAAMMDD-AVS              PIC 9(6).                                
014100 01  ARB-TIAAMMDD-AVS            PIC 9(6).                                
014200 01  WOL-TILEVDAG                PIC 9.                                   
014300 01  WOL-TIAAVV-AVS              PIC 9(4).                                
014400 01  W-TIAVROP-AVS               PIC 9(4).                                
014500 01  SPAR-TIAVROP-AVS            PIC 9(4).                                
014600 01  FIX-AAVVD                   PIC 9(5)  VALUE ZERO.                    
014700                                                                          
014800*-- FIX FÖR ATT KLARA WZ20DAYS                                            
014900 01  WS-DAYS-TIAAVVD-AVS         PIC 9(5) VALUE ZERO.                     
015000 01  WS-DAYS-TIAAVV              PIC 9(4) VALUE ZERO.                     
015100                                                                          
015200 01  W-HELP-DATUM-SSAAVVD.                                                
015300     03  W-HELP-DATUM-SSAAVV      PIC 9(6).                               
015400     03  FILLER REDEFINES W-HELP-DATUM-SSAAVV.                            
015500         05 W-HELP-DATUM-SS       PIC 9(2).                               
015600         05 W-HELP-DATUM-AAVV     PIC 9(4).                               
015700     03  W-HELP-DATUM-D           PIC 9(1).                               
015800                                                                          
015900 01  W-HELP-TIFINLV-SSAAVVD.                                              
016000     03  W-HELP-TIFINLV-SSAAVV    PIC 9(6).                               
016100     03  FILLER REDEFINES W-HELP-TIFINLV-SSAAVV.                          
016200         05 W-HELP-TIFINLV-SS     PIC 9(2).                               
016300         05 W-HELP-TIFINLV-AAVV   PIC 9(4).                               
016400     03  W-HELP-TIFINLV-D         PIC 9(1).                               
016500                                                                          
016600 01  ARB-TILEVDAG-TAB.                                                    
016700     03  ARB-TILEVDAG  OCCURS 5  PIC 9.                                   
016800 01  W-TILEVDAG-TAB.                                                      
016900     03  W-TILEVDAG   OCCURS 5   PIC 9.                                   
017000 01  ANT-LEVDAG                  PIC 9(3).                                
017100 01  W-KVAVROP-TAB.                                                       
017200     03  W-KVAVROP    OCCURS 5   PIC 9(7)                  COMP-3.        
017300 01  WS-KVPALL                   PIC S9(7)    VALUE ZERO   COMP-3.        
017400                                                                          
017500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
017600     88  NYCKLAR-OK                          VALUE 'J'.                   
017700     88  NYCKLAR-FEL                         VALUE 'N'.                   
017800                                                                          
017900 77  FOERST-SW                   PIC X       VALUE 'J'.                   
018000     88  FOERST                              VALUE 'J'.                   
018100     88  EJ-FOERST                           VALUE 'N'.                   
018200                                                                          
018300 77  SW-FRYS                     PIC X       VALUE 'N'.                   
018400 77  SW-OK                       PIC X       VALUE 'N'.                   
018500                                                                          
018600 01  W-TIFINLV-SS                PIC 9(2).                                
018700 01  W-TIFINLV                   PIC 9(5).                                
018800 01  FILLER REDEFINES W-TIFINLV.                                          
018900     03  W-TIFINLV-1-4           PIC 9(4).                                
019000     03  FILLER                  PIC 9.                                   
019100 01  W-TISPECST-ADJ              PIC 9(4).                                
019200                                                                          
019300 01  W-TISPECST-DISP             PIC S9(5)   COMP-3.                      
019400 01  W-TISPEC-TOT                PIC S9(5)  VALUE ZERO   COMP-3.          
019500 01  W-DASPECST                  PIC 9(6).                                
019600 01  FILLER REDEFINES W-DASPECST.                                         
019700     03  W-DASPECST-SS           PIC 9(2).                                
019800     03  W-DASPECST-AAVV         PIC 9(4).                                
019900                                                                          
020000 01  W-VECKO-DIFFERENS           PIC 9(3).                                
020100 01  W-ANTAL-VECKOR              PIC S9(3)   COMP-3.                      
020200 01  W-KVVECKOR-SPEC             PIC S9(4)   COMP-3.                      
020300 01  W-KVVECKOR-ADD              PIC S9(3)  VALUE ZERO   COMP-3.          
020400 01  W-VECKO-ATGANG              PIC S9(7)   COMP-3.                      
020500 01  W-VECKO-PB                  PIC S9(6)V9(1)      COMP-3.              
020600 01  W-VECKO-KVPB-REF            PIC S9(6)V9(1) VALUE ZERO COMP-3.        
020700                                                                          
020800 01  W-AAVV-ADD                  PIC S9(5)   COMP-3  VALUE ZERO.          
020900 01  W-AAVV-JUST                 PIC 9(4).                                
021000 01  FILLER REDEFINES W-AAVV-JUST.                                        
021100     03  W-AAVV-JUST-AA          PIC 9(2).                                
021200     03  W-AAVV-JUST-VV          PIC 9(2).                                
021300 01  W-KVDAGAR-FFH               PIC S9(3)   COMP-3.                      
021400 01  W-KVVECKOR-FFH              PIC S9(3)                 COMP-3.        
021500 01  W-TILLG                     PIC S9(7)V9(2)            COMP-3.        
021600 01  W-TILLG-SPAR                PIC S9(7)V9(2)            COMP-3.        
021700 01  W-TILLG-BER                 PIC S9(7)V9(2)            COMP-3.        
021800 01  WS-TIAAMMDD-SPECST          PIC 9(6)       VALUE ZERO.               
021900                                                                          
022000 01  W-GRAENS-AVROP              PIC S9(5)                 COMP-3.        
022100 01  W-TILLG-SDC                 PIC S9(7)                 COMP-3.        
022200 01  W-OVERLAGER-SDC             PIC S9(7)                 COMP-3.        
022300 01  W-ARSBEH                    PIC S9(9)      VALUE ZERO COMP-3.        
022400 01  W-ARSOMS                    PIC S9(9)V9(2) VALUE ZERO COMP-3.        
022500 01  W-ARSOMS-100000             PIC S9(9)V9(2) VALUE +100000.00          
022600                                                           COMP-3.        
022700 01  W-BUFF                      PIC S9(7)                 COMP-3.        
022800 01  W-KVANTITET                 PIC S9(7)                 COMP-3.        
022900 01  W-ANTAL                     PIC S9(9)      VALUE ZERO COMP-3.        
023000 01  W-AVROPSKVANTITET           PIC S9(7)      VALUE ZERO COMP-3.        
023100                                                                          
023200 01  W-KVOKS-SDC                 PIC S9(7)      VALUE ZERO COMP-3.        
023300                                                                          
023400 01  W-Q-FREKV-MAX               PIC S9(3)      VALUE +0   COMP-3.        
023500 01  WS-IDLANDX2-SHIP            PIC X(2)       VALUE SPACE.              
023600 01  WS-FRYSTID8                 PIC 9(8).                                
023700 01  FILLER            REDEFINES WS-FRYSTID8.                             
023800     03  WS-FRYSTID-SEKEL        PIC 9(2).                                
023900     03  WS-FRYSTID              PIC 9(6).                                
024000 01  WS-KVULOAD                  PIC S9(7)      VALUE ZERO COMP-3.        
024100 01  WS-TIAVROP-DISP             PIC 9(04)      VALUE ZERO.               
024200                                                                          
024300 01  IX                          PIC 9(03)      VALUE ZERO.               
024400 01  IX-L                        PIC S9(9)      VALUE ZERO COMP-3.        
024500 01  MAX-IX                      PIC 9(03)      VALUE 156.                
024600 01  SEASON-IX                   PIC 9(02)      VALUE ZERO.               
024700 01  SEASON-IX-MAX               PIC 9(02)      VALUE 12.                 
024800 01  IX-DAG                      PIC S9(3)      VALUE +0 COMP-3.          
024900 01  IX-DG                       PIC S9(3)      VALUE +0 COMP-3.          
025000                                                                          
025100 01  SUM-RETUR                   PIC S9(7)      VALUE ZERO COMP-3.        
025200                                                                          
025300 01 SPARAREOR.                                                            
025400     03  W-SPAR-WDK623.                                                   
025500*        05 W-SPAR-REDIRLEV      OCCURS 2                                 
025600*                                PIC S9(1)V9(2) VALUE ZERO COMP-3.        
025700*        05 W-SPAR-KVPB-SATS OCCURS 2                                     
025800*                                PIC S9(6)V9(1) VALUE ZERO COMP-3.        
025900*        05 W-SPAR-KVPB-SEP      OCCURS 2                                 
026000*                                PIC S9(6)V9(1) VALUE ZERO COMP-3.        
026100         05 W-SPAR-TOT-KVPB-REF                                           
026200                                 PIC S9(6)V9(1) VALUE ZERO COMP-3.        
026300     03  W-SPAR-RESEASON         PIC S9(1)V9(2) VALUE ZERO COMP-3.        
026400                                                                          
026500     03  W-SPAR-WDK712.                                                   
026600         05 W-SPAR-LART-DAPUBL   PIC 9(8) VALUE ZERO.                     
026700         05 W-SPAR-LART-PRMATRL  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
026800         05 W-SPAR-LART-KVDAGAR-INLEV                                     
026900                                 PIC S9(3) VALUE ZERO COMP-3.             
027000                                                                          
027100     03  W-SPAR-WDK711.                                                   
027200         05 W-SPAR-KDERS         PIC S9(3)      VALUE ZERO COMP-3.        
027300         05 W-SPAR-SLAG-KVLS     PIC S9(7)      VALUE ZERO COMP-3.        
027400         05 W-SPAR-SLAG-KVRESS   PIC S9(7)      VALUE ZERO COMP-3.        
027500*        05 W-SPAR-KVAKS   OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
027600         05 W-SPAR-SLAG-KVROS-DAG                                         
027700                                 PIC S9(7)    VALUE ZERO COMP-3.          
027800         05 W-SPAR-SLAG-KVROS-BULK                                        
027900                                 PIC S9(7)    VALUE ZERO COMP-3.          
028000         05 W-SPAR-SLAG-KVOKS-DAG                                         
028100                                 PIC S9(7)    VALUE ZERO COMP-3.          
028200         05 W-SPAR-SLAG-KVOKS-BULK                                        
028300                                 PIC S9(7)    VALUE ZERO COMP-3.          
028400         05 W-SPAR-SLAG-KVAKS-PAV                                         
028500                                 PIC S9(7)    VALUE ZERO COMP-3.          
028600         05 W-SPAR-SLAG-KVAKS-SDC                                         
028700                                 PIC S9(7)    VALUE ZERO COMP-3.          
028800                                                                          
028900                                                                          
029000*        05 W-SPAR-KVRETUR OCCURS 2 PIC S9(7) VALUE ZERO COMP-3.          
029100         05 W-SPAR-KVSLAGER      PIC S9(7)    VALUE ZERO COMP-3.          
029200*    03  W-SPAR-INLB11.                                                   
029300*        05 W-SPAR-KVBR          PIC S9(7)    VALUE ZERO COMP-3.          
029400*        05 W-SPAR-KVBR-TOT      PIC S9(7)    VALUE ZERO COMP-3.          
029500*    03  W-KVPB-SDC-TOT      PIC S9(6)V9(1)          COMP-3.              
029600*    03  W-KVPB-SDC-EJ-DIR   PIC S9(6)V9(1)          COMP-3.              
029700                                                                          
029800****************************************** TILLGÅNGSTABELL                
029900 01  FILLER                      PIC X(11) VALUE 'TILLGTABELL'.           
030000 01  TILLGANGSTABELL.                                                     
030100     03  TILLGTAB-MAX            PIC 9(3)    VALUE 156.                   
030200     03  TILLGTAB-IX             PIC 9(3)    VALUE ZERO.                  
030300                                                                          
030400     03  TILLGTAB OCCURS 156.                                             
030500         05  TILLGTAB-ANTAL      PIC S9(7)V99      COMP-3.                
030600                                                                          
030700*01  -COPY WWPRODSL                                                       
030800                                                                          
030900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
031000 01  GENERELLA-SUBPROGRAM.                                                
031100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
031200     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
031300     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
031400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
031500     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
031600     03  W222BHDC                PIC X(8)    VALUE 'W222BHDC'.            
031700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
031800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
031900     03  W224PUNK                PIC X(8)    VALUE 'W224PUNK'.            
032000     03  W221LPAD                PIC X(8)    VALUE 'W221LPAD'.            
032100     03  W222TILG                PIC X(8)    VALUE 'W222TILG'.            
032200                                                                          
032300                                                                          
032400*    --- PARAMETRAR TILL WDATKONV                                         
032500*01  -COPY WDATAREA                                                       
032600                                                                          
032700*                            *** PARAMETRAR TILL WDAGKONV '               
032800*01  -COPY WDAGAREA.                                                      
032900                                                                          
033000*                            *** PARAMETRAR TILL WORKDAY  '               
033100*01  -COPY WORKAREA.                                                      
033200                                                                          
033300 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
033400*   -COPY WZ20DAYS                                                        
033500     EJECT                                                                
033600                                                                          
033700* VARIABLER TILL SUBPROGRAM W009VADD                                      
033800 01  DATUM-AAVV                  PIC S9(5)  COMP-3.                       
033900 01  ANTAL-VECKOR                PIC S9(3)  COMP-3.                       
034000                                                                          
034100*    --- LÄNKAREA TILL W222BHDC                                           
034200*01  -COPY W222BHDC -PRE BHDC-                                            
034300 01  PB-TOTAL-SEP-LEV-XDC        PIC X(2)   VALUE '03'.                   
034400 01  XDC-CDC-BEHOV               PIC X(2)   VALUE '05'.                   
034500                                                                          
034600*    --- LÄNKAREA TILL SUBPROGRAM W224PUNK                                
034700*01 -COPY W224PUNK                                                        
034800                                                                          
034900*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
035000 01  W-W221LP-CTX                PIC X(08) VALUE 'W221LP02'.              
035100 01  W-KDLPORS-GRP.                                                       
035200     03 W-KDLPORS-TAB OCCURS 4   PIC 9(3).                                
035300                                                                          
035400                                                                          
035500*    --- PARAMETERS FOR SUB PROGRAM W222TILG                              
035600 01  FILLER                      PIC X(16)   VALUE 'W222TILG'.            
035700     SKIP3                                                                
035800*01  -COPY W222TILG                                                       
035900     EJECT                                                                
036000                                                                          
036100*    --- PARAMETRAR TILL ABEND                                            
036200                                                                          
036300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
036400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
036500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
036600                                                                          
036700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
036800*                                                                         
036900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037000                                                                          
037100 01  NYCKLAR-TILL-DLI.                                                    
037200     03  W-WDD901KY-X.                                                    
037300         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
037400         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
037500     03  W-IDARTNR-X.                                                     
037600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
037700     03  W-IDDC-X.                                                        
037800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
037900     03  W-IDDC-B6-X.                                                     
038000         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
038100     03  W-IDLAND-X.                                                      
038200         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
038300     03  W-IDLEVNR-X.                                                     
038400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
038500     03  W-IDLEVNR-SHIP-X.                                                
038600         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
038700     03  W-KDAVROP-X.                                                     
038800         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
038900     03  W-WDD905KY-X.                                                    
039000         05  W-DAAVROP-KY        PIC 9(6)    VALUE ZERO.                  
039100         05  W-TILEVDAG-KY       PIC S9      VALUE ZERO COMP-3.           
039200     03  W-WDF301KY-X.                                                    
039300         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
039400         05  W-DADATUM-HELG      PIC 9(8)    VALUE ZERO.                  
039500         05  FILLER  REDEFINES W-DADATUM-HELG.                            
039600             07  W-DADATUM-HELG-SS      PIC 9(2).                         
039700             07  W-DADATUM-HELG-AAMMDD  PIC 9(6).                         
039800                                                                          
039900     03  W-WDD601KY-X.                                                    
040000            05 W-IDDC-D6         PIC X(2)    VALUE SPACE.                 
040100            05 W-IDLEVNR-D6      PIC X(5)    VALUE SPACE.                 
040200            05 W-IDARTNR-D6      PIC S9(9)   VALUE ZERO    COMP-3.        
040300            05 W-IDANSK-D6       PIC S9(3)   VALUE ZERO    COMP-3.        
040400                                                                          
040500     03  W-WDD601KY-MIN-X.                                                
040600            05 W-IDDC-MIN        PIC X(2)  VALUE SPACE.                   
040700            05 W-IDLEVNR-MIN     PIC X(5)  VALUE SPACE.                   
040800            05 W-IDARTNR-MIN     PIC S9(9) VALUE ZERO COMP-3.             
040900            05 W-IDANSK-MIN      PIC S9(3) VALUE ZERO COMP-3.             
041000                                                                          
041100     03  W-WDD601KY-MAX-X.                                                
041200            05 W-IDDC-MAX        PIC X(2)  VALUE SPACE.                   
041300            05 W-IDLEVNR-MAX     PIC X(5)  VALUE SPACE.                   
041400            05 W-IDARTNR-MAX     PIC S9(9) VALUE ZERO COMP-3.             
041500            05 W-IDANSK-MAX      PIC S9(3) VALUE +999 COMP-3.             
041600                                                                          
041700     03  W-IDANSK-NON-X.                                                  
041800            05  W-IDANSK-NON     PIC S9(3) VALUE ZERO COMP-3.             
041900                                                                          
042000*    --- STATUS-KOD FRÅN IMS                                              
042100 01  STATUS-WS                   PIC XX.                                  
042200     88  SEGMENT-FINNS                       VALUE '  '.                  
042300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
042400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
042500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
042600                                                                          
042700 01  GODK-STATUSKODER.                                                    
042800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042900                                                                          
043000 01  SSA1                        PIC X(128).                              
043100 01  SSA2                        PIC X(64).                               
043200 01  SSA3                        PIC X(64).                               
043300                                                                          
043400*    --- IMS FUNKTIONSKODER                                               
043500*01  -COPY W0003                                                          
043600                                                                          
043700                                                                          
043800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
043900 01  DLI-IO-WDD901.                                                       
044000*    03  -COPY WDD901                                                     
044100                                                                          
044200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
044300 01  DLI-IO-WDD902.                                                       
044400*    03  -COPY WDD902 -PRE D902-                                          
044500                                                                          
044600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
044700 01  DLI-IO-WDD904.                                                       
044800*    03  -COPY WDD904 -PRE D904-                                          
044900                                                                          
045000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
045100 01  DLI-IO-WDD905.                                                       
045200*    03  -COPY WDD905 -PRE D905-                                          
045300                                                                          
045400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905-2'.                    
045500 01  DLI-IO-WDD905-2.                                                     
045600*    03  -COPY WDD905 -PRE D905-2-                                        
045700                                                                          
045800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
045900 01  DLI-IO-WDK601.                                                       
046000*    03  -COPY WDK601                                                     
046100                                                                          
046200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
046300 01  DLI-IO-WDK611.                                                       
046400*    03  -COPY WDK611                                                     
046500                                                                          
046600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
046700 01  DLI-IO-WDK701.                                                       
046800*    03  -COPY WDK701                                                     
046900                                                                          
047000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
047100 01  DLI-IO-WDK711.                                                       
047200*    03  -COPY WDK711                                                     
047300                                                                          
047400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
047500 01  DLI-IO-WDK712.                                                       
047600*    03  -COPY WDK712                                                     
047700                                                                          
047800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
047900 01  DLI-IO-WDK722.                                                       
048000*    03  -COPY WDK722                                                     
048100                                                                          
048200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
048300 01  DLI-IO-WDK724.                                                       
048400*    03  -COPY WDK724                                                     
048500                                                                          
048600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
048700 01  DLI-IO-WDF106.                                                       
048800*    03  -COPY WDF106                                                     
048900                                                                          
049000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
049100 01  DLI-IO-WDF116.                                                       
049200*    03  -COPY WDF116                                                     
049300                                                                          
049400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF301'.                      
049500 01  DLI-IO-WDF301.                                                       
049600*    03  -COPY WDF301                                                     
049700                                                                          
049800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
049900 01  DLI-IO-WDB601.                                                       
050000*    03  -COPY WDB601                                                     
050100                                                                          
050200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
050300 01  DLI-IO-WDD311.                                                       
050400*    03  -COPY WDD311                                                     
050500                                                                          
050600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
050700 01  DLI-IO-WDD601.                                                       
050800*    03  -COPY WDD601                                                     
050900                                                                          
051000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD704'.                      
051100 01  DLI-IO-WDD704.                                                       
051200*    03  -COPY WDD704   -PRE D704-                                        
051300                                                                          
051400                                                                          
051500 LINKAGE SECTION.                                                         
051600                                                                          
051700*01  -COPY W224OMSP                                                       
051800                                                                          
051900*01  -COPY W0008     -PRE WDD9-                                           
052000     05  FILLER                   PIC X(7).                               
052100     05  WDD9-KEY-02-IDLEVNR      PIC X(5).                               
052200                                                                          
052300*01  -COPY W0008     -PRE WDK6-                                           
052400     05  FILLER                   PIC X.                                  
052500                                                                          
052600*01  -COPY W0008     -PRE WDK7-                                           
052700     05  FILLER                   PIC X.                                  
052800                                                                          
052900*01  -COPY W0008     -PRE WDF1-                                           
053000     05  FILLER                   PIC X.                                  
053100                                                                          
053200*01  -COPY W0008     -PRE WDF3-                                           
053300     05  FILLER                   PIC X.                                  
053400                                                                          
053500*01  -COPY W0008     -PRE WDB6-                                           
053600     05  FILLER                   PIC X.                                  
053700                                                                          
053800*01  -COPY W0008     -PRE WDD6-                                           
053900     05  FILLER                   PIC X.                                  
054000                                                                          
054100*01  -COPY W0008     -PRE WDD3-                                           
054200     05  FILLER                   PIC X.                                  
054300                                                                          
054400*01  -COPY W0008     -PRE WDD7-                                           
054500     05  FILLER                   PIC X.                                  
054600                                                                          
054700*    PROGRAM W224PUNK                                                     
054800 01  PUNK-REFL1-2501-PCB          PIC X.                                  
054900 01  PUNK-REFL1-WDB6-PCB          PIC X.                                  
055000 01  PUNK-REFL1-WDK7-PCB          PIC X.                                  
055100 01  PUNK-REFL1-UTIL-WDK6-PCB     PIC X.                                  
055200 01  PUNK-REFL1-UTIL-WDK7-PCB     PIC X.                                  
055300 01  PUNK-REFL1-UTIL-WDB6-PCB     PIC X.                                  
055400 01  PUNK-UTUP1-WDK7-PCB          PIC X.                                  
055500 01  PUNK-UTUP1-WDB6-PCB          PIC X.                                  
055600 01  PUNK-UTUP1-UTIL-WDK6-PCB     PIC X.                                  
055700 01  PUNK-UTUP1-UTIL-WDK7-PCB     PIC X.                                  
055800 01  PUNK-UTUP1-UTIL-WDB6-PCB     PIC X.                                  
055900                                                                          
056000*    PROGRAM W222BHDC                                                     
056100 01  BHDC-WDK6-PCB                PIC X.                                  
056200 01  BHDC-WDK7-PCB                PIC X.                                  
056300 01  BHDC-WDB6-PCB                PIC X.                                  
056400 01  BHDC-WDR2-PCB                PIC X.                                  
056500 01  BHDC-WDD7-PCB                PIC X.                                  
056600 01  BHDC-WDK7E-PCB               PIC X.                                  
056700 01  BHDC-WDD7-2-PCB              PIC X.                                  
056800 01  BHDC-WDK9-PCB                PIC X.                                  
056900 01  BHDC-REFL1-2501-PCB          PIC X.                                  
057000 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
057100 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
057200 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
057300 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
057400 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
057500     EJECT                                                                
057600 01  BHDC-REFL2-2501-PCB          PIC X.                                  
057700 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
057800 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
057900 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
058000 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
058100     EJECT                                                                
058200 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
058300 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
058400 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
058500     EJECT                                                                
058600 01  BHDC-W222-WDK6-PCB           PIC X.                                  
058700 01  BHDC-W222-WDK7-PCB           PIC X.                                  
058800 01  BHDC-W222-ARTM-PCB           PIC X.                                  
058900 01  BHDC-W222-2501-PCB           PIC X.                                  
059000 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
059100 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
059200 01  BHDC-W222-WDB6-PCB           PIC X.                                  
059300 01  BHDC-W222-WDD7-PCB           PIC X.                                  
059400 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
059500 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
059600 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
059700 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
059800 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
059900 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
060000 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
060100 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
060200 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
060300     EJECT                                                                
060400 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
060500 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
060600 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
060700 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
060800 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
060900     EJECT                                                                
061000                                                                          
061100*    PROGRAM W222TILG                                                     
061200 01  TILG-WDK7-PCB                PIC X.                                  
061300 01  TILG-WDL2-PCB                PIC X.                                  
061400 01  TILG-WDB6-PCB                PIC X.                                  
061500 01  TILG-WDD9-PCB                PIC X.                                  
061600 01  TILG-WDK6-PCB                PIC X.                                  
061700 01  TILG-WDK9-PCB                PIC X.                                  
061800     EJECT                                                                
061900                                                                          
062000 PROCEDURE DIVISION  USING OMSP-W224OMSP                                  
062100                           WDD9-PCB WDK6-PCB WDK7-PCB WDF1-PCB            
062200                           WDF3-PCB WDB6-PCB WDD6-PCB WDD3-PCB            
062300                           WDD7-PCB                                       
062400                                                                          
062500                           PUNK-REFL1-2501-PCB                            
062600                           PUNK-REFL1-WDB6-PCB                            
062700                           PUNK-REFL1-WDK7-PCB                            
062800                           PUNK-REFL1-UTIL-WDK6-PCB                       
062900                           PUNK-REFL1-UTIL-WDK7-PCB                       
063000                           PUNK-REFL1-UTIL-WDB6-PCB                       
063100                           PUNK-UTUP1-WDK7-PCB                            
063200                           PUNK-UTUP1-WDB6-PCB                            
063300                           PUNK-UTUP1-UTIL-WDK6-PCB                       
063400                           PUNK-UTUP1-UTIL-WDK7-PCB                       
063500                           PUNK-UTUP1-UTIL-WDB6-PCB                       
063600                                                                          
063700                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
063800                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
063900                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
064000                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
064100                           BHDC-REFL1-2501-PCB                            
064200                           BHDC-REFL1-WDB6-PCB                            
064300                           BHDC-REFL1-WDK7-PCB                            
064400                           BHDC-REFL1-UTIL-WDK6-PCB                       
064500                           BHDC-REFL1-UTIL-WDK7-PCB                       
064600                           BHDC-REFL1-UTIL-WDB6-PCB                       
064700                           BHDC-REFL2-2501-PCB                            
064800                           BHDC-REFL2-WDB6-PCB                            
064900                           BHDC-REFL2-UTIL-WDK6-PCB                       
065000                           BHDC-REFL2-UTIL-WDK7-PCB                       
065100                           BHDC-REFL2-UTIL-WDB6-PCB                       
065200                           BHDC-UTIL-WDK6-PCB                             
065300                           BHDC-UTIL-WDK7-PCB                             
065400                           BHDC-UTIL-WDB6-PCB                             
065500                           BHDC-W222-WDK6-PCB                             
065600                           BHDC-W222-WDK7-PCB                             
065700                           BHDC-W222-ARTM-PCB                             
065800                           BHDC-W222-2501-PCB                             
065900                           BHDC-W222-WDB6R-PCB                            
066000                           BHDC-W222-WDK7R-PCB                            
066100                           BHDC-W222-WDB6-PCB                             
066200                           BHDC-W222-WDD7-PCB                             
066300                           BHDC-W222-WDK7E-PCB                            
066400                           BHDC-W222-UTIL-WDK6-PCB                        
066500                           BHDC-W222-UTIL-WDK7-PCB                        
066600                           BHDC-W222-UTIL-WDB6-PCB                        
066700                           BHDC-W222-UTUP-WDK7-PCB                        
066800                           BHDC-W222-UTUP-WDB6-PCB                        
066900                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
067000                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
067100                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
067200                           BHDC-UTUP-WDK7-PCB                             
067300                           BHDC-UTUP-WDB6-PCB                             
067400                           BHDC-UTUP-UTIL-WDK6-PCB                        
067500                           BHDC-UTUP-UTIL-WDK7-PCB                        
067600                           BHDC-UTUP-UTIL-WDB6-PCB                        
067700                                                                          
067800                           TILG-WDK7-PCB                                  
067900                           TILG-WDL2-PCB                                  
068000                           TILG-WDB6-PCB                                  
068100                           TILG-WDD9-PCB                                  
068200                           TILG-WDK6-PCB                                  
068300                           TILG-WDK9-PCB                                  
068400                           .                                              
068500 MAIN SECTION.                                                            
068600     ENTRY 'DLITCBL' USING OMSP-W224OMSP                                  
068700                           WDD9-PCB WDK6-PCB WDK7-PCB WDF1-PCB            
068800                           WDF3-PCB WDB6-PCB WDD6-PCB WDD3-PCB            
068900                           WDD7-PCB                                       
069000                                                                          
069100                           PUNK-REFL1-2501-PCB                            
069200                           PUNK-REFL1-WDB6-PCB                            
069300                           PUNK-REFL1-WDK7-PCB                            
069400                           PUNK-REFL1-UTIL-WDK6-PCB                       
069500                           PUNK-REFL1-UTIL-WDK7-PCB                       
069600                           PUNK-REFL1-UTIL-WDB6-PCB                       
069700                           PUNK-UTUP1-WDK7-PCB                            
069800                           PUNK-UTUP1-WDB6-PCB                            
069900                           PUNK-UTUP1-UTIL-WDK6-PCB                       
070000                           PUNK-UTUP1-UTIL-WDK7-PCB                       
070100                           PUNK-UTUP1-UTIL-WDB6-PCB                       
070200                                                                          
070300                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
070400                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
070500                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
070600                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
070700                           BHDC-REFL1-2501-PCB                            
070800                           BHDC-REFL1-WDB6-PCB                            
070900                           BHDC-REFL1-WDK7-PCB                            
071000                           BHDC-REFL1-UTIL-WDK6-PCB                       
071100                           BHDC-REFL1-UTIL-WDK7-PCB                       
071200                           BHDC-REFL1-UTIL-WDB6-PCB                       
071300                           BHDC-REFL2-2501-PCB                            
071400                           BHDC-REFL2-WDB6-PCB                            
071500                           BHDC-REFL2-UTIL-WDK6-PCB                       
071600                           BHDC-REFL2-UTIL-WDK7-PCB                       
071700                           BHDC-REFL2-UTIL-WDB6-PCB                       
071800                           BHDC-UTIL-WDK6-PCB                             
071900                           BHDC-UTIL-WDK7-PCB                             
072000                           BHDC-UTIL-WDB6-PCB                             
072100                           BHDC-W222-WDK6-PCB                             
072200                           BHDC-W222-WDK7-PCB                             
072300                           BHDC-W222-ARTM-PCB                             
072400                           BHDC-W222-2501-PCB                             
072500                           BHDC-W222-WDB6R-PCB                            
072600                           BHDC-W222-WDK7R-PCB                            
072700                           BHDC-W222-WDB6-PCB                             
072800                           BHDC-W222-WDD7-PCB                             
072900                           BHDC-W222-WDK7E-PCB                            
073000                           BHDC-W222-UTIL-WDK6-PCB                        
073100                           BHDC-W222-UTIL-WDK7-PCB                        
073200                           BHDC-W222-UTIL-WDB6-PCB                        
073300                           BHDC-W222-UTUP-WDK7-PCB                        
073400                           BHDC-W222-UTUP-WDB6-PCB                        
073500                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
073600                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
073700                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
073800                           BHDC-UTUP-WDK7-PCB                             
073900                           BHDC-UTUP-WDB6-PCB                             
074000                           BHDC-UTUP-UTIL-WDK6-PCB                        
074100                           BHDC-UTUP-UTIL-WDK7-PCB                        
074200                           BHDC-UTUP-UTIL-WDB6-PCB                        
074300                                                                          
074400                           TILG-WDK7-PCB                                  
074500                           TILG-WDL2-PCB                                  
074600                           TILG-WDB6-PCB                                  
074700                           TILG-WDD9-PCB                                  
074800                           TILG-WDK6-PCB                                  
074900                           TILG-WDK9-PCB                                  
075000                           .                                              
075100     PERFORM A-INIT                                                       
075200     PERFORM B-KOLLA-INPUT                                                
075300                                                                          
075400     PERFORM C-TAG-BORT-OMSPEC-AVROP                                      
075500     PERFORM D-BERAKNA-NYA-PUNKTER                                        
075600     PERFORM E-OMSPEC                                                     
075700     PERFORM F-UPPDATERA-WDK7                                             
075800     PERFORM G-SKAPA-OMSPEC-SEGMENT                                       
075900     PERFORM H-SKAPA-FORSLAGSPOST-PA-KOE                                  
076000                                                                          
076100     PERFORM Z-FINIT                                                      
076200     MOVE ZERO TO RETURN-CODE                                             
076300     GOBACK                                                               
076400     .                                                                    
076500                                                                          
076600                                                                          
076700 A-INIT SECTION.                                                          
076800                                                                          
076900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
077000                                                                          
077100     MOVE 20                 TO W-DASPECST-SS                             
077200     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
077300                                                                          
077400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
077500                         DAT-O-TIDATUM DAT-KDSVAR                         
077600                                                                          
077700     IF DAT-KDSVAR-FEL                                                    
077800       MOVE 'FEL VID ANROP TILL DATKONV 1' TO FELTEXT                     
077900       CALL FELLOG                                                        
078000     ELSE                                                                 
078100       MOVE DAT-TIAAMMDD       TO W-DATUM-AAMMDD                          
078200                                  W-DATUM-AAMMDD-AKT                      
078300                                  W-DATUM-SSAAMMDD-AKT(3:6)               
078400       MOVE DAT-TISEKEL        TO W-DATUM-SS-AKT                          
078500                                                                          
078600       MOVE DAT-TIAA           TO W-DATUM2-AA                             
078700       MOVE DAT-TIMM           TO W-DATUM2-MM                             
078800       MOVE DAT-TIDD           TO W-DATUM2-DD                             
078900       MOVE DAT-TIAAVV-GRP     TO W-DATUM-AAVV-AKT                        
079000       MOVE DAT-TID            TO W-DAT-TID-AKT                           
079100       MOVE DAT-TIVV           TO W-DATUM-VV-AKT                          
079200       MOVE DAT-TIAAVVD-GRP    TO W-DATUM-AAVVD-AKT                       
079300       MOVE DAT-TIPP           TO W-AKT-SEASON                            
079400     END-IF                                                               
079500                                                                          
079600     .                                                                    
079700     EJECT                                                                
079800                                                                          
079900 B-KOLLA-INPUT   SECTION.                                                 
080000                                                                          
080100     MOVE 'B-KOLLA-INPUT   ' TO CURRENT-SECTION                           
080200                                                                          
080300     MOVE OMSP-IDARTNR     TO W-IDARTNR-D9                                
080400                              W-IDARTNR                                   
080500     MOVE OMSP-IDDC        TO W-IDDC-D9                                   
080600                              W-IDDC                                      
080700                              W-IDDC-B6                                   
080800                                                                          
080900     PERFORM IMS-GU-WDD901                                                
081000     PERFORM IMS-GU-WDK601                                                
081100     PERFORM IMS-GU-WDK611                                                
081200     MOVE CLAG-KDERS       TO W-SPAR-KDERS                                
081300     PERFORM IMS-GU-WDK711                                                
081400     MOVE SLAG-IDLEVNR     TO W-IDLEVNR                                   
081500     MOVE SLAG-KVLS        TO W-SPAR-SLAG-KVLS                            
081600     MOVE SLAG-KVRESS      TO W-SPAR-SLAG-KVRESS                          
081700     MOVE SLAG-KVROS-DAG   TO W-SPAR-SLAG-KVROS-DAG                       
081800     MOVE SLAG-KVROS-BULK  TO W-SPAR-SLAG-KVROS-BULK                      
081900     MOVE SLAG-KVOKS-DAG   TO W-SPAR-SLAG-KVOKS-DAG                       
082000     MOVE SLAG-KVOKS-BULK  TO W-SPAR-SLAG-KVOKS-BULK                      
082100     MOVE SLAG-KVAKS-SDC   TO W-SPAR-SLAG-KVAKS-SDC                       
082200     MOVE SLAG-KVAKS-PAV   TO W-SPAR-SLAG-KVAKS-PAV                       
082300                                                                          
082400     PERFORM IMS-GU-WDB601                                                
082500     MOVE DCS-IDLANDX2 TO W-IDLAND                                        
082600     PERFORM IMS-GU-WDK712                                                
082700     IF SEGMENT-FINNS                                                     
082800        MOVE LART-DAPUBL   TO W-SPAR-LART-DAPUBL                          
082900        MOVE LART-PRMATRL  TO W-SPAR-LART-PRMATRL                         
083000        MOVE LART-KVDAGAR-INLEV TO                                        
083100                              W-SPAR-LART-KVDAGAR-INLEV                   
083200     ELSE                                                                 
083300        MOVE ZERO          TO W-SPAR-LART-DAPUBL                          
083400                              W-SPAR-LART-PRMATRL                         
083500                              W-SPAR-LART-KVDAGAR-INLEV                   
083600     END-IF                                                               
083700                                                                          
083800     MOVE 99999            TO W-TIERSDAT                                  
083900     IF CLAG-KDERS = 03 OR 06                                             
084000        PERFORM IMS-GU-WDD704                                             
084100        IF SEGMENT-FINNS                                                  
084200           MOVE D704-TIERSDAT-PREL-C1 TO W-TIERSDAT                       
084300        END-IF                                                            
084400     END-IF                                                               
084500                                                                          
084600     .                                                                    
084700     EJECT                                                                
084800                                                                          
084900 C-TAG-BORT-OMSPEC-AVROP SECTION.                                         
085000                                                                          
085100     MOVE 'C-TAG-BORT-OMSP ' TO CURRENT-SECTION                           
085200                                                                          
085300     PERFORM IMS-GHU-WDD904                                               
085400     IF SEGMENT-FINNS                                                     
085500        PERFORM IMS-DLET-WDD904                                           
085600     END-IF                                                               
085700                                                                          
085800     PERFORM IMS-GU-WDD902                                                
085900     IF SEGMENT-FINNS                                                     
086000        MOVE +1 TO W-KDAVROP                                              
086100        PERFORM IMS-GHNP-WDD905                                           
086200        PERFORM UNTIL SEGMENT-SAKNAS                                      
086300           PERFORM IMS-DLET-WDD905                                        
086400           PERFORM IMS-GHNP-WDD905                                        
086500        END-PERFORM                                                       
086600     END-IF                                                               
086700                                                                          
086800     .                                                                    
086900                                                                          
087000                                                                          
087100                                                                          
087200 D-BERAKNA-NYA-PUNKTER SECTION.                                           
087300                                                                          
087400     MOVE 'D-NYA-PUNKTER   ' TO CURRENT-SECTION                           
087500                                                                          
087600     PERFORM DA-SKAPA-LANKAREA                                            
087700                                                                          
087800     PERFORM DB-SKAPA-BEHOVSTABELL                                        
087900                                                                          
088000     CALL W224PUNK USING PUNK-W224PUNK                                    
088100                         PUNK-REFL1-2501-PCB                              
088200                         PUNK-REFL1-WDB6-PCB                              
088300                         PUNK-REFL1-WDK7-PCB                              
088400                         PUNK-REFL1-UTIL-WDK6-PCB                         
088500                         PUNK-REFL1-UTIL-WDK7-PCB                         
088600                         PUNK-REFL1-UTIL-WDB6-PCB                         
088700                         PUNK-UTUP1-WDK7-PCB                              
088800                         PUNK-UTUP1-WDB6-PCB                              
088900                         PUNK-UTUP1-UTIL-WDK6-PCB                         
089000                         PUNK-UTUP1-UTIL-WDK7-PCB                         
089100                         PUNK-UTUP1-UTIL-WDB6-PCB                         
089200                                                                          
089300     MOVE PUNK-KVSLAGER TO W-SPAR-KVSLAGER                                
089400                                                                          
089500     PERFORM DC-UPPDATERA-WDK7                                            
089600     .                                                                    
089700                                                                          
089800 DA-SKAPA-LANKAREA      SECTION.                                          
089900                                                                          
090000     MOVE 'DA-LANKAREA     ' TO CURRENT-SECTION                           
090100                                                                          
090200                                                                          
090300     MOVE W-IDARTNR              TO PUNK-IDARTNR                          
090400     MOVE W-DATUM-AAVV-AKT       TO PUNK-TIAAVV-AKT                       
090500                                    W-DATUM-AAVV                          
090600     MOVE W-DATUM-AAVV           TO PUNK-TIAAVVD-AKT                      
090700                                    W-DATUM-AAVVD                         
090800                                                                          
090900     MULTIPLY 10 BY PUNK-TIAAVVD-AKT                                      
091000     MULTIPLY 10 BY W-DATUM-AAVVD                                         
091100                                                                          
091200     PERFORM IMS-GU-WDK722                                                
091300     MOVE XLAG-KVSLAGER          TO PUNK-KVSLAGER-IN                      
091400     MOVE XLAG-TIMANSEC          TO PUNK-TIMANSEC-IN                      
091500     MOVE XLAG-KVPALL            TO PUNK-KVPALL                           
091600     MOVE XLAG-KVULOAD           TO PUNK-KVULOAD                          
091700                                                                          
091800     PERFORM IMS-GU-WDK711                                                
091900     MOVE SLAG-IDDC              TO PUNK-IDDC                             
092000     MOVE SLAG-IDDC-REF          TO PUNK-IDDC-REF                         
092100     MOVE SLAG-IDREFTAB          TO PUNK-IDREFTAB                         
092200     MOVE SLAG-FLREFBEO          TO PUNK-FLREFBEO                         
092300     MOVE SLAG-FLWILSON          TO PUNK-FLWILSON                         
092400     MOVE SLAG-KVREFPKT          TO PUNK-KVREFPKT-IN                      
092500     MOVE SLAG-TIREFPKT          TO PUNK-TIREFPKT-IN                      
092600     MOVE SLAG-KVREFBER          TO PUNK-KVREFBER-IN                      
092700     MOVE SLAG-TIREFPAF          TO PUNK-TIREFPAF-IN                      
092800     MOVE 1 TO SEASON-IX                                                  
092900     PERFORM UNTIL SEASON-IX > SEASON-IX-MAX                              
093000        MOVE SLAG-RESEASON(SEASON-IX)                                     
093100                                 TO PUNK-RESEASON(SEASON-IX)              
093200        ADD 1 TO SEASON-IX                                                
093300     END-PERFORM                                                          
093400     MOVE SLAG-FLFLYG            TO PUNK-FLFLYG                           
093500     MOVE SLAG-IDLEVNR           TO PUNK-IDLEVNR                          
093600     MOVE SLAG-KVPB-REF          TO PUNK-KVPB-REF                         
093700     MOVE SLAG-KVPBREOI          TO PUNK-KVPBREOI                         
093800     MOVE SLAG-ADLAGOMR          TO PUNK-ADLAGOMR                         
093900                                                                          
094000     PERFORM IMS-GNP-WDK724                                               
094100     PERFORM UNTIL SEGMENT-SAKNAS                                         
094200                OR SPRL-IDLEVNR-PR = SLAG-IDLEVNR                         
094300        PERFORM IMS-GNP-WDK724                                            
094400     END-PERFORM                                                          
094500     IF SEGMENT-FINNS                                                     
094600        MOVE SPRL-PRARTBES-PR    TO PUNK-PRARTBES                         
094700     ELSE                                                                 
094800        MOVE ZERO                TO PUNK-PRARTBES                         
094900     END-IF                                                               
095000                                                                          
095100     MOVE W-SPAR-LART-PRMATRL    TO PUNK-PRMATRL                          
095200     MOVE W-SPAR-LART-DAPUBL     TO W-DAPUBL                              
095300                                                                          
095400     PERFORM DAA-OVERLAGER                                                
095500                                                                          
095600     MOVE ZERO                   TO PUNK-KVREFBER                         
095700                                    PUNK-KVREFPKT                         
095800                                    PUNK-KVREFOVL                         
095900                                    PUNK-KVSLAGER                         
096000                                    PUNK-KVEOQ                            
096100                                    PUNK-TIREFPAF                         
096200                                    PUNK-TIMANSEC                         
096300                                    PUNK-TIREFPKT                         
096400     .                                                                    
096500                                                                          
096600                                                                          
096700 DAA-OVERLAGER       SECTION.                                             
096800                                                                          
096900     MOVE 'DAA-OVERLAGER   ' TO CURRENT-SECTION                           
097000                                                                          
097100     MOVE ZERO              TO   W-TILLG-SDC                              
097200                                 W-OVERLAGER-SDC                          
097300                                                                          
097400     PERFORM IMS-GU-WDK701                                                
097500                                                                          
097600     IF SEGMENT-FINNS                                                     
097700        PERFORM IMS-GNP-WDK711-REF                                        
097800        PERFORM UNTIL SEGMENT-SAKNAS                                      
097900                                                                          
098000           MOVE ZERO           TO   W-TILLG-SDC                           
098100           ADD SLAG-KVLS       TO   W-TILLG-SDC                           
098200           ADD SLAG-KVBEART    TO   W-TILLG-SDC                           
098300           ADD SLAG-KVAKS-SDC  TO   W-TILLG-SDC                           
098400           ADD SLAG-KVAKS-PAV  TO   W-TILLG-SDC                           
098500           MOVE ZERO           TO   W-KVOKS-SDC                           
098600           COMPUTE W-KVOKS-SDC = SLAG-KVOKS-BULK                          
098700                               + SLAG-KVOKS-DAG                           
098800                                                                          
098900***-- FIX FÖR NEGATIVA KVOKS                                              
099000           IF W-KVOKS-SDC > 0                                             
099100             SUBTRACT W-KVOKS-SDC FROM W-TILLG-SDC                        
099200           END-IF                                                         
099300                                                                          
099400           SUBTRACT SLAG-KVRESS   FROM W-TILLG-SDC                        
099500           IF SLAG-IDDC NOT = DCS-IDDC                                    
099600              MOVE SLAG-IDDC  TO W-IDDC-B6                                
099700              PERFORM IMS-GU-WDB601                                       
099800           END-IF                                                         
099900                                                                          
100000           IF DCS-CHINA OR DCS-USA                                        
100100              IF DCS-FLOVRLAGBER = NEJ                                    
100200                 CONTINUE                                                 
100300              ELSE                                                        
100400                 IF SLAG-KVREFOVL < W-TILLG-SDC                           
100500                    COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC             
100600                                            + W-TILLG-SDC                 
100700                                            - SLAG-KVREFOVL               
100800                 END-IF                                                   
100900              END-IF                                                      
101000           END-IF                                                         
101100                                                                          
101200           MOVE ZERO  TO VECKO-DIFF                                       
101300           IF W-DAPUBL > ZERO                                             
101400             MOVE W-DAPUBL-AAMMDD TO DAT-I-TIDATUM                        
101500             MOVE 'AAMMDD'        TO DAT-KDDATFORM                        
101600             CALL WDATKONV USING     DAT-KDDATFORM                        
101700                                     DAT-I-TIDATUM                        
101800                                     DAT-O-TIDATUM                        
101900                                     DAT-KDSVAR                           
102000             IF DAT-KDSVAR-FEL                                            
102100                MOVE 'FEL VID ANROP TILL DATKONV 12'                      
102200                                       TO FELTEXT                         
102300                CALL FELLOG                                               
102400             ELSE                                                         
102500               MOVE W-DATUM-AA-AKT    TO TMP1-YY                          
102600               MOVE DAT-TIAA          TO TMP2-YY                          
102700               PERFORM WY2000P9                                           
102800               COMPUTE VECKO-DIFF = (TMP1-YY - TMP2-YY) * 52              
102900                             + W-DATUM-VV-AKT - DAT-TIVV                  
103000             END-IF                                                       
103100           ELSE                                                           
103200             MOVE ART-TIFINLV       TO W-TIFINLV-AAVVD                    
103300             MOVE W-DATUM-AA-AKT    TO TMP1-YY                            
103400             MOVE W-TIFINLV-AA      TO TMP2-YY                            
103500             PERFORM WY2000P9                                             
103600             COMPUTE VECKO-DIFF = (TMP1-YY - TMP2-YY) * 52                
103700                                + W-DATUM-VV-AKT - W-TIFINLV-VV           
103800           END-IF                                                         
103900                                                                          
104000           IF VECKO-DIFF < 52                                             
104100              MOVE ZERO TO W-OVERLAGER-SDC                                
104200           END-IF                                                         
104300           PERFORM IMS-GNP-WDK711-REF                                     
104400        END-PERFORM                                                       
104500     END-IF                                                               
104600     .                                                                    
104700     EJECT                                                                
104800 DB-SKAPA-BEHOVSTABELL  SECTION.                                          
104900                                                                          
105000     MOVE 'DB-SKAPA-BEH-TAB'  TO CURRENT-SECTION                          
105100                                                                          
105200     MOVE ART-IDARTNR           TO BHDC-IDARTNR                           
105300     MOVE OMSP-IDDC             TO BHDC-IDDC                              
105400     MOVE W-DAT-TID-AKT         TO BHDC-TID-AKTUELL                       
105500     MOVE 156                   TO BHDC-KVVECKOR-BEHOV                    
105600     MOVE W-DATUM-AAVV          TO BHDC-TIAAVV-AKTUELL                    
105700     MOVE W-DATUM-AAVV          TO BHDC-TIBEHOV-START                     
105800     MOVE 1                     TO W-ANTAL-VECKOR                         
105900     CALL W009VADD USING BHDC-TIBEHOV-START W-ANTAL-VECKOR                
106000                                                                          
106100     MOVE ZERO                  TO BHDC-KVTILLG-TOT-CDC                   
106200     MOVE ART-IDARTNR           TO TILG-IDARTNR                           
106300     CALL W222TILG        USING TILG-W222TILG                             
106400                                TILG-WDK7-PCB TILG-WDL2-PCB               
106500                                TILG-WDB6-PCB TILG-WDD9-PCB               
106600                                TILG-WDK6-PCB TILG-WDK9-PCB               
106700                                                                          
106800     MOVE TILG-KVTILLG-TOT      TO BHDC-KVTILLG-TOT-CDC                   
106900                                                                          
107000     MOVE PB-TOTAL-SEP-LEV-XDC  TO BHDC-KDBEHOV                           
107100                                                                          
107200     IF ART-KDERS-UTG = 0                                                 
107300       CALL W222BHDC USING BHDC-W222BHDC                                  
107400                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
107500                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
107600                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
107700                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
107800                           BHDC-REFL1-2501-PCB                            
107900                           BHDC-REFL1-WDB6-PCB                            
108000                           BHDC-REFL1-WDK7-PCB                            
108100                           BHDC-REFL1-UTIL-WDK6-PCB                       
108200                           BHDC-REFL1-UTIL-WDK7-PCB                       
108300                           BHDC-REFL1-UTIL-WDB6-PCB                       
108400                           BHDC-REFL2-2501-PCB                            
108500                           BHDC-REFL2-WDB6-PCB                            
108600                           BHDC-REFL2-UTIL-WDK6-PCB                       
108700                           BHDC-REFL2-UTIL-WDK7-PCB                       
108800                           BHDC-REFL2-UTIL-WDB6-PCB                       
108900                           BHDC-UTIL-WDK6-PCB                             
109000                           BHDC-UTIL-WDK7-PCB                             
109100                           BHDC-UTIL-WDB6-PCB                             
109200                           BHDC-W222-WDK6-PCB                             
109300                           BHDC-W222-WDK7-PCB                             
109400                           BHDC-W222-ARTM-PCB                             
109500                           BHDC-W222-2501-PCB                             
109600                           BHDC-W222-WDB6R-PCB                            
109700                           BHDC-W222-WDK7R-PCB                            
109800                           BHDC-W222-WDB6-PCB                             
109900                           BHDC-W222-WDD7-PCB                             
110000                           BHDC-W222-WDK7E-PCB                            
110100                           BHDC-W222-UTIL-WDK6-PCB                        
110200                           BHDC-W222-UTIL-WDK7-PCB                        
110300                           BHDC-W222-UTIL-WDB6-PCB                        
110400                           BHDC-W222-UTUP-WDK7-PCB                        
110500                           BHDC-W222-UTUP-WDB6-PCB                        
110600                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
110700                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
110800                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
110900                           BHDC-UTUP-WDK7-PCB                             
111000                           BHDC-UTUP-WDB6-PCB                             
111100                           BHDC-UTUP-UTIL-WDK6-PCB                        
111200                           BHDC-UTUP-UTIL-WDK7-PCB                        
111300                           BHDC-UTUP-UTIL-WDB6-PCB                        
111400                                                                          
111500       IF BHDC-FLJANEJ-ANROP = 'N'                                        
111600         PERFORM S10-NOLLA-BHDC-RESULTATFLT                               
111700       END-IF                                                             
111800     ELSE                                                                 
111900       PERFORM S10-NOLLA-BHDC-RESULTATFLT                                 
112000     END-IF                                                               
112100     .                                                                    
112200     EJECT                                                                
112300 DC-UPPDATERA-WDK7 SECTION.                                               
112400                                                                          
112500     MOVE 'DC-UPPD-WDK7    '  TO CURRENT-SECTION                          
112600                                                                          
112700     PERFORM IMS-GHU-WDK711                                               
112800     MOVE PUNK-KVREFPKT      TO SLAG-KVREFPKT                             
112900     MOVE PUNK-KVREFOVL      TO SLAG-KVREFOVL                             
113000     MOVE PUNK-TIREFPKT      TO SLAG-TIREFPKT                             
113100     IF PUNK-TIREFPAF NOT > W-DATUM-AAMMDD                                
113200        MOVE PUNK-KVREFBER   TO SLAG-KVREFBER                             
113300        MOVE PUNK-TIREFPAF   TO SLAG-TIREFPAF                             
113400     END-IF                                                               
113500     PERFORM IMS-REPL-WDK711                                              
113600                                                                          
113700     PERFORM IMS-GHU-WDK722                                               
113800     MOVE PUNK-KVSLAGER      TO XLAG-KVSLAGER                             
113900     MOVE PUNK-KVEOQ         TO XLAG-KVEOQ                                
114000     MOVE PUNK-TIMANSEC      TO XLAG-TIMANSEC                             
114100     PERFORM IMS-REPL-WDK722                                              
114200     .                                                                    
114300     EJECT                                                                
114400 E-OMSPEC SECTION.                                                        
114500                                                                          
114600     MOVE 'E-OMSPEC        ' TO CURRENT-SECTION                           
114700                                                                          
114800     PERFORM EA-INITIERA-OMSPEC                                           
114900     PERFORM EB-SKAPA-TILLGANGSTABELL                                     
115000     PERFORM EC-SKAPA-BEHOVSTABELL                                        
115100     PERFORM ED-BERAKNA-TILLG-I-SPECVECKA                                 
115200                                                                          
115300     MOVE 1 TO D905-KDAVROP                                               
115400     PERFORM EE-SPEC-NYTT-FOERSLAG                                        
115500     .                                                                    
115600                                                                          
115700 EA-INITIERA-OMSPEC SECTION.                                              
115800                                                                          
115900     MOVE 'EA-INIT-OMSPEC  ' TO CURRENT-SECTION                           
116000                                                                          
116100     MOVE ZERO                  TO D904-KVBEST-PL                         
116200                                   W-TILLG-SPAR                           
116300     COMPUTE W-TILLG-SPAR =                                               
116400                     W-SPAR-SLAG-KVLS                                     
116500                 +   W-SPAR-SLAG-KVAKS-SDC                                
116600                 +   W-SPAR-SLAG-KVAKS-PAV                                
116700                                                                          
116800                 -   W-SPAR-SLAG-KVRESS                                   
116900                 -   W-SPAR-SLAG-KVROS-DAG                                
117000                 -   W-SPAR-SLAG-KVROS-BULK                               
117100                 -   W-SPAR-SLAG-KVOKS-DAG                                
117200                 -   W-SPAR-SLAG-KVOKS-BULK                               
117300                 +   W-OVERLAGER-SDC                                      
117400                                                                          
117500*                                                                         
117600***  PUBWEEK IS LEADTIME ADJUSTED IN DEMAND MODULE.                       
117700***  IF PUBWEEK IS IN FUTURE, CHECK THE WEEK FOR FIRST DEMAND             
117800***  SO THE CALL-OFFS SHOULD BE FROM LEADTIME + CURRENT WEEK.             
117900*                                                                         
118000     IF W-SPAR-LART-DAPUBL > ZERO                                         
118100                                                                          
118200         MOVE W-SPAR-LART-DAPUBL   TO W-DAPUBL                            
118300         MOVE W-DAPUBL-AAMMDD      TO DAYS-TIDATE1                        
118400         MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                      
118500         MOVE 'YYYYWWD'            TO DAYS-KDDATFMT2                      
118600         MOVE 0                    TO DAYS-KVDAYS                         
118700         MOVE SPACE                TO DAYS-TIDATE2                        
118800                                      DAYS-IDCALEND                       
118900         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
119000*                                                                         
119100         IF DAYS-KDRC = 8                                                 
119200           MOVE 'FEL VID ANROP TILL WZ20DAYS 3' TO FELTEXT                
119300           CALL FELLOG                                                    
119400         ELSE                                                             
119500           MOVE DAYS-TIDATE2(1:2) TO W-TIFINLV-SS                         
119600           MOVE DAYS-TIDATE2(3:5) TO W-TIFINLV                            
119700                                     W-TIFINLV-AAVVD                      
119800           MOVE W-DATUM-AAVV-AKT         TO TMP1-YYWW                     
119900           MOVE W-TIFINLV-AAVV           TO TMP2-YYWW                     
120000           PERFORM WY2000P3                                               
120100           IF TMP2-YYWW     > TMP1-YYWW                                   
120200              MOVE 1                     TO IX-L                          
120300              PERFORM UNTIL IX-L > 156                                    
120400              OR BHDC-KVBEHOV-VECKA (IX-L) > ZERO                         
120500                ADD 1                    TO IX-L                          
120600              END-PERFORM                                                 
120700*                                                                         
120800*      CHECK IF DEMAND EXISTS I N THE FIRST WEEK OR                       
120900*      IF WE HAVE NEGATIVE ASSETS LIKE IN CASE OF BACKORDER               
121000*      BASED ON IX-L VALUE WE ADJUST THE DELIVERY PLAN WEEK               
121100*                                                                         
121200              IF BHDC-KVBEHOV-DESSUTOM > ZERO                             
121300              OR W-TILLG-SPAR < ZERO                                      
121400                 MOVE 1                  TO IX-L                          
121500              END-IF                                                      
121600              IF IX-L > 156                                               
121700                 CONTINUE                                                 
121800              ELSE                                                        
121900                 MOVE IX-L               TO ANTAL-VECKOR                  
122000                 MOVE W-DATUM-AAVV-AKT   TO W-AAVV-ADD                    
122100                 CALL W009VADD USING W-AAVV-ADD ANTAL-VECKOR              
122200                 MOVE W-AAVV-ADD         TO W-TISPECST-ADJ                
122300              END-IF                                                      
122400           END-IF                                                         
122500         END-IF                                                           
122600*                                                                         
122700     ELSE                                                                 
122800         MOVE ART-TIFINLV     TO W-TIFINLV-AAVVD                          
122900         MOVE W-TIFINLV-AAVVD TO DAYS-TIDATE1                             
123000         MOVE 'YYWWD'         TO DAYS-KDDATFMT1                           
123100         MOVE 'YYYYWWD'       TO DAYS-KDDATFMT2                           
123200         MOVE 0               TO DAYS-KVDAYS                              
123300         MOVE SPACE           TO DAYS-TIDATE2                             
123400                                 DAYS-IDCALEND                            
123500         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
123600*                                                                         
123700         IF DAYS-KDRC = 8                                                 
123800           MOVE 'FEL VID ANROP TILL WZ20DAYS 2' TO FELTEXT                
123900           CALL FELLOG                                                    
124000         ELSE                                                             
124100           MOVE DAYS-TIDATE2(1:2) TO W-TIFINLV-SS                         
124200           MOVE DAYS-TIDATE2(3:5) TO W-TIFINLV                            
124300                                     W-TIFINLV-AAVVD                      
124400           MOVE W-DATUM-AAVV-AKT          TO TMP1-YYWW                    
124500           MOVE W-TIFINLV-AAVV            TO TMP2-YYWW                    
124600           PERFORM WY2000P3                                               
124700           IF TMP2-YYWW     > TMP1-YYWW                                   
124800              MOVE 1                      TO IX-L                         
124900              PERFORM UNTIL IX-L > 156                                    
125000              OR BHDC-KVBEHOV-VECKA (IX-L) > ZERO                         
125100                ADD 1                     TO IX-L                         
125200              END-PERFORM                                                 
125300*                                                                         
125400*      CHECK IF DEMAND EXISTS I N THE FIRST WEEK OR                       
125500*      IF WE HAVE NEGATIVE ASSETS LIKE IN CASE OF BACKORDER               
125600*      BASED ON IX-L VALUE WE ADJUST THE DELIVERY PLAN WEEK               
125700*                                                                         
125800              IF BHDC-KVBEHOV-DESSUTOM     > ZERO                         
125900              OR W-TILLG-SPAR < ZERO                                      
126000                 MOVE 1                   TO IX-L                         
126100              END-IF                                                      
126200              IF IX-L > 156                                               
126300                 CONTINUE                                                 
126400              ELSE                                                        
126500                 MOVE IX-L                TO ANTAL-VECKOR                 
126600                 MOVE W-DATUM-AAVV-AKT    TO W-AAVV-ADD                   
126700                 CALL W009VADD USING W-AAVV-ADD ANTAL-VECKOR              
126800                 MOVE W-AAVV-ADD          TO W-TISPECST-ADJ               
126900              END-IF                                                      
127000           END-IF                                                         
127100         END-IF                                                           
127200*                                                                         
127300     END-IF                                                               
127400                                                                          
127500     PERFORM IMS-GU-WDK722                                                
127600     MOVE 20                    TO W-HELP-DATUM-SS                        
127700     MOVE W-DATUM-AAVV          TO W-HELP-DATUM-AAVV                      
127800     MOVE W-TIFINLV-SS          TO W-HELP-TIFINLV-SS                      
127900     MOVE W-TIFINLV-1-4         TO W-HELP-TIFINLV-AAVV                    
128000     MOVE ART-KDPRODSL          TO TEST-KDPRODSL                          
128100     MOVE W-DATUM-AAVV             TO W-TISPECST-DISP                     
128200     MOVE XLAG-KVVECKOR-FT         TO W-ANTAL-VECKOR                      
128300     ADD  1                        TO W-ANTAL-VECKOR                      
128400     CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR                   
128500     MOVE W-DATUM-AAVV             TO W-DASPECST-AAVV                     
128600                                                                          
128700     MOVE XLAG-KVVECKOR-LT         TO W-ANTAL-VECKOR                      
128800     ADD  1                        TO W-ANTAL-VECKOR                      
128900     MOVE W-DASPECST-AAVV          TO W-AAVV-ADD                          
129000     CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                        
129100     MOVE W-AAVV-ADD               TO W-DASPECST-AAVV                     
129200     MOVE W-DASPECST               TO D904-DASPECST                       
129300*                                                                         
129400*    IF THE DELIERY PLAN WEEK CALCULATES IS LESS THAN                     
129500*    ADJUSTED DELIVERY PLAN WEEK CALUCATED ABOVE (BASED ON                
129600*    DEMAND FROM THE DEMAND DEMAND MODULE),                               
129700*    USE THE ADJUSTED DELIVERY PLAN START WEEK                            
129800*                                                                         
129900     IF W-HELP-DATUM-SSAAVV <  W-HELP-TIFINLV-SSAAVV                      
130000        MOVE W-TISPECST-DISP       TO TMP1-YYWW                           
130100        MOVE W-TISPECST-ADJ        TO TMP2-YYWW                           
130200        PERFORM WY2000P3                                                  
130300        IF TMP1-YYWW <  TMP2-YYWW                                         
130400           MOVE W-TISPECST-ADJ     TO W-TISPECST-DISP                     
130500        END-IF                                                            
130600     END-IF                                                               
130700                                                                          
130800     MOVE W-DATUM-AAVV          TO W-DATUM-FROM                           
130900     MOVE W-TISPECST-DISP       TO W-DATUM-TOM                            
131000     PERFORM S102-BERAKNA-VECKODIFFERENS                                  
131100                                                                          
131200     COMPUTE W-KVVECKOR-FFH ROUNDED =                                     
131300             XLAG-KVDAGAR-FFH / 5                                         
131400                                                                          
131500     COMPUTE W-KVVECKOR-SPEC = 52 - XLAG-KVVECKOR-LT                      
131600                                                                          
131700     IF W-KVVECKOR-SPEC < 10                                              
131800        MOVE 10 TO W-KVVECKOR-SPEC                                        
131900     END-IF                                                               
132000                                                                          
132100     MOVE ZERO TO W-TILLG                                                 
132200                                                                          
132300     MOVE 1 TO TILLGTAB-IX                                                
132400     PERFORM UNTIL TILLGTAB-IX > TILLGTAB-MAX                             
132500        MOVE ZERO TO TILLGTAB-ANTAL (TILLGTAB-IX)                         
132600        ADD 1 TO TILLGTAB-IX                                              
132700     END-PERFORM                                                          
132800                                                                          
132900     MOVE W-DASPECST-AAVV     TO DAYS-TIDATE1                             
133000     MOVE 'YYWW'              TO DAYS-KDDATFMT1                           
133100     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
133200     MOVE 0                   TO DAYS-KVDAYS                              
133300     MOVE SPACE               TO DAYS-TIDATE2                             
133400                                 DAYS-IDCALEND                            
133500     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
133600*                                                                         
133700     IF DAYS-KDRC = 8                                                     
133800       MOVE 'FEL VID ANROP TILL WZ20DAYS 4' TO FELTEXT                    
133900       CALL FELLOG                                                        
134000     ELSE                                                                 
134100       MOVE DAYS-TIDATE2(1:6) TO WS-TIAAMMDD-SPECST                       
134200     END-IF                                                               
134300*                                                                         
134400*    OM ARTIKELN BLIR ERSATT UNDER SPECPERIODEN                           
134500*    FÅR VI KORTA AV PERIODEN LITE GRANN                                  
134600                                                                          
134700     MOVE W-DASPECST          TO W-TISPEC-TOT                             
134800     MOVE W-KVVECKOR-SPEC     TO W-KVVECKOR-ADD                           
134900     CALL W009VADD USING W-TISPEC-TOT W-KVVECKOR-ADD                      
135000     IF W-TISPEC-TOT > W-TIERSDAT-AAVV                                    
135100        IF W-DASPECST-AAVV < W-TIERSDAT-AAVV                              
135200                                                                          
135300           MOVE W-TIERSDAT-AAVV  TO DAYS-TIDATE1                          
135400           MOVE 'YYWW'           TO DAYS-KDDATFMT1                        
135500           MOVE 'YYMMDD'         TO DAYS-KDDATFMT2                        
135600           MOVE 0                TO DAYS-KVDAYS                           
135700           MOVE SPACE            TO DAYS-TIDATE2                          
135800                                    DAYS-IDCALEND                         
135900           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
136000*                                                                         
136100           IF DAYS-KDRC = 8                                               
136200             MOVE 'FEL VID ANROP TILL WZ020DAYS 7' TO FELTEXT             
136300             CALL FELLOG                                                  
136400           ELSE                                                           
136500              MOVE WS-TIAAMMDD-SPECST TO DAG-TIAAMMDD-FOM                 
136600              MOVE DAYS-TIDATE2(1:6)  TO DAG-TIAAMMDD-TOM                 
136700                                                                          
136800              MOVE 001             TO DAG-KDCALL                          
136900                                                                          
137000              CALL WDAGKONV USING DAG-KDCALL,                             
137100                            DAG-DATUM-AREA,                               
137200                            DAG-KDSVAR                                    
137300              IF DAG-KDSVAR = SPACE                                       
137400                 COMPUTE W-KVVECKOR-SPEC = DAG-KVKALDAG / 7               
137500              ELSE                                                        
137600                 MOVE 'FEL VID ANROP TILL DAGKONV 3' TO FELTEXT           
137700                 CALL FELLOG                                              
137800              END-IF                                                      
137900           END-IF                                                         
138000        ELSE                                                              
138100           MOVE ZERO               TO W-KVVECKOR-SPEC                     
138200        END-IF                                                            
138300     END-IF                                                               
138400     .                                                                    
138500     EJECT                                                                
138600                                                                          
138700 EB-SKAPA-TILLGANGSTABELL SECTION.                                        
138800******************************************************************        
138900*                                                                *        
139000*    TABELL MED INLEVERANSER PLACERADE I RESPEKTIVE VECKA        *        
139100*                                                                *        
139200******************************************************************        
139300                                                                          
139400     MOVE 'EB-TILLG-TAB    ' TO CURRENT-SECTION                           
139500                                                                          
139600     MOVE D904-DASPECST      TO W-DASPECST                                
139700     MOVE W-DASPECST-AAVV    TO W-GRAENS-AVROP                            
139800     MOVE W-KVVECKOR-SPEC    TO W-ANTAL-VECKOR                            
139900     CALL W009VADD USING W-GRAENS-AVROP W-ANTAL-VECKOR                    
140000                                                                          
140100     MOVE 2  TO W-KDAVROP                                                 
140200     PERFORM IMS-GU-WDD901                                                
140300     PERFORM IMS-GNP-WDD905-FIRST                                         
140400                                                                          
140500     PERFORM UNTIL SEGMENT-SAKNAS                                         
140600        MOVE D905-DAAVROP-AVS     TO W-DAAVROP-AVS                        
140700                                                                          
140800        IF (WDD9-KEY-02-IDLEVNR = SLAG-IDLEVNR                            
140900          AND D905-DAAVROP-AVS < D904-DASPECST)                           
141000                                                                          
141100        OR                                                                
141200           (WDD9-KEY-02-IDLEVNR NOT = SLAG-IDLEVNR                        
141300          AND W-DAAVROP-AAVV < W-GRAENS-AVROP)                            
141400                                                                          
141500           MOVE D905-TIAVRDAT-DISP  TO DAT-I-TIDATUM                      
141600           MOVE 'AAMMDD'            TO DAT-KDDATFORM                      
141700           CALL WDATKONV USING         DAT-KDDATFORM                      
141800                                       DAT-I-TIDATUM                      
141900                                       DAT-O-TIDATUM                      
142000                                       DAT-KDSVAR                         
142100           IF DAT-KDSVAR-FEL                                              
142200              MOVE 'FEL VID ANROP TILL DATKONV 2'                         
142300                                     TO FELTEXT                           
142400              CALL FELLOG                                                 
142500           END-IF                                                         
142600           MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV                         
142700           IF WS-TIAAVV < W-TISPECST-DISP                                 
142800              ADD D905-KVAVROP       TO W-TILLG                           
142900           ELSE                                                           
143000              MOVE WS-TIAAVV         TO W-DATUM-TOM                       
143100              MOVE W-DATUM-AAVV      TO W-DATUM-FROM                      
143200              PERFORM S102-BERAKNA-VECKODIFFERENS                         
143300              IF W-VECKO-DIFFERENS > ZERO                                 
143400                 MOVE W-VECKO-DIFFERENS                                   
143500                                     TO TILLGTAB-IX                       
143600              ELSE                                                        
143700                 MOVE +1             TO TILLGTAB-IX                       
143800              END-IF                                                      
143900              ADD D905-KVAVROP       TO                                   
144000                                     TILLGTAB-ANTAL (TILLGTAB-IX)         
144100           END-IF                                                         
144200        END-IF                                                            
144300        PERFORM IMS-GNP-WDD905-NEXT                                       
144400     END-PERFORM                                                          
144500     .                                                                    
144600                                                                          
144700                                                                          
144800 EC-SKAPA-BEHOVSTABELL SECTION.                                           
144900                                                                          
145000     MOVE 'EC-SKAPA-TABELL ' TO CURRENT-SECTION                           
145100                                                                          
145200     MOVE ART-IDARTNR           TO BHDC-IDARTNR                           
145300     MOVE OMSP-IDDC             TO BHDC-IDDC                              
145400     MOVE W-DATUM-AAVV          TO BHDC-TIAAVV-AKTUELL                    
145500     MOVE W-DAT-TID-AKT         TO BHDC-TID-AKTUELL                       
145600     MOVE W-DATUM-AAVV          TO BHDC-TIBEHOV-START                     
145700     MOVE 1                     TO W-ANTAL-VECKOR                         
145800     CALL W009VADD USING BHDC-TIBEHOV-START W-ANTAL-VECKOR                
145900                                                                          
146000     MOVE 156                   TO BHDC-KVVECKOR-BEHOV                    
146100     MOVE PB-TOTAL-SEP-LEV-XDC  TO BHDC-KDBEHOV                           
146200                                                                          
146300     MOVE ZERO                  TO BHDC-KVTILLG-TOT-CDC                   
146400     MOVE ART-IDARTNR           TO TILG-IDARTNR                           
146500     CALL W222TILG        USING TILG-W222TILG                             
146600                                TILG-WDK7-PCB TILG-WDL2-PCB               
146700                                TILG-WDB6-PCB TILG-WDD9-PCB               
146800                                TILG-WDK6-PCB TILG-WDK9-PCB               
146900                                                                          
147000     MOVE TILG-KVTILLG-TOT      TO BHDC-KVTILLG-TOT-CDC                   
147100                                                                          
147200     IF ART-KDERS-UTG = 0                                                 
147300       CALL W222BHDC USING BHDC-W222BHDC                                  
147400                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
147500                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
147600                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
147700                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
147800                           BHDC-REFL1-2501-PCB                            
147900                           BHDC-REFL1-WDB6-PCB                            
148000                           BHDC-REFL1-WDK7-PCB                            
148100                           BHDC-REFL1-UTIL-WDK6-PCB                       
148200                           BHDC-REFL1-UTIL-WDK7-PCB                       
148300                           BHDC-REFL1-UTIL-WDB6-PCB                       
148400                           BHDC-REFL2-2501-PCB                            
148500                           BHDC-REFL2-WDB6-PCB                            
148600                           BHDC-REFL2-UTIL-WDK6-PCB                       
148700                           BHDC-REFL2-UTIL-WDK7-PCB                       
148800                           BHDC-REFL2-UTIL-WDB6-PCB                       
148900                           BHDC-UTIL-WDK6-PCB                             
149000                           BHDC-UTIL-WDK7-PCB                             
149100                           BHDC-UTIL-WDB6-PCB                             
149200                           BHDC-W222-WDK6-PCB                             
149300                           BHDC-W222-WDK7-PCB                             
149400                           BHDC-W222-ARTM-PCB                             
149500                           BHDC-W222-2501-PCB                             
149600                           BHDC-W222-WDB6R-PCB                            
149700                           BHDC-W222-WDK7R-PCB                            
149800                           BHDC-W222-WDB6-PCB                             
149900                           BHDC-W222-WDD7-PCB                             
150000                           BHDC-W222-WDK7E-PCB                            
150100                           BHDC-W222-UTIL-WDK6-PCB                        
150200                           BHDC-W222-UTIL-WDK7-PCB                        
150300                           BHDC-W222-UTIL-WDB6-PCB                        
150400                           BHDC-W222-UTUP-WDK7-PCB                        
150500                           BHDC-W222-UTUP-WDB6-PCB                        
150600                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
150700                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
150800                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
150900                           BHDC-UTUP-WDK7-PCB                             
151000                           BHDC-UTUP-WDB6-PCB                             
151100                           BHDC-UTUP-UTIL-WDK6-PCB                        
151200                           BHDC-UTUP-UTIL-WDK7-PCB                        
151300                           BHDC-UTUP-UTIL-WDB6-PCB                        
151400                                                                          
151500       IF BHDC-FLJANEJ-ANROP = 'N'                                        
151600         PERFORM S10-NOLLA-BHDC-RESULTATFLT                               
151700       END-IF                                                             
151800     ELSE                                                                 
151900       PERFORM S10-NOLLA-BHDC-RESULTATFLT                                 
152000     END-IF                                                               
152100     .                                                                    
152200     EJECT                                                                
152300                                                                          
152400                                                                          
152500 ED-BERAKNA-TILLG-I-SPECVECKA SECTION.                                    
152600******************************************************************        
152700*                                                                *        
152800*    BERÄKNING AV TILLGÅNG I FÖRSTA SPEC-VECKA                   *        
152900*    AVROP HAR TIDIGARE ADDERATS TILL W-TILLG                    *        
153000*                                                                *        
153100******************************************************************        
153200                                                                          
153300     MOVE 'ED-TILLG-I-VECKA' TO CURRENT-SECTION                           
153400                                                                          
153500     PERFORM EDA-BERAKNA-ATGANG-I-VECKAN                                  
153600                                                                          
153700     COMPUTE W-TILLG-BER =                                                
153800                     W-SPAR-SLAG-KVLS                                     
153900                 +   W-SPAR-SLAG-KVAKS-SDC                                
154000                 +   W-SPAR-SLAG-KVAKS-PAV                                
154100                                                                          
154200                 -   W-SPAR-SLAG-KVRESS                                   
154300                 -   W-SPAR-SLAG-KVROS-DAG                                
154400                 -   W-SPAR-SLAG-KVROS-BULK                               
154500                 -   W-SPAR-SLAG-KVOKS-DAG                                
154600                 -   W-SPAR-SLAG-KVOKS-BULK                               
154700                 -   W-VECKO-ATGANG                                       
154800                 +   W-OVERLAGER-SDC                                      
154900                                                                          
155000     ADD W-TILLG-BER         TO W-TILLG                                   
155100     SUBTRACT BHDC-KVBEHOV-DESSUTOM                                       
155200                           FROM W-TILLG                                   
155300     MOVE 1 TO TILLGTAB-IX                                                
155400     MOVE BHDC-TIBEHOV-START TO W-DATUM-FROM                              
155500     MOVE W-TISPECST-DISP    TO W-DATUM-TOM                               
155600     PERFORM S102-BERAKNA-VECKODIFFERENS                                  
155700                                                                          
155800     IF W-VECKO-DIFFERENS > TILLGTAB-MAX                                  
155900       MOVE TILLGTAB-MAX     TO W-VECKO-DIFFERENS                         
156000     END-IF                                                               
156100                                                                          
156200     MOVE 1 TO TILLGTAB-IX                                                
156300     PERFORM UNTIL TILLGTAB-IX > W-VECKO-DIFFERENS                        
156400                                                                          
156500         SUBTRACT BHDC-KVBEHOV-VECKA (TILLGTAB-IX)                        
156600                           FROM W-TILLG                                   
156700         ADD 1 TO TILLGTAB-IX                                             
156800     END-PERFORM                                                          
156900     .                                                                    
157000                                                                          
157100                                                                          
157200                                                                          
157300 EDA-BERAKNA-ATGANG-I-VECKAN SECTION.                                     
157400******************************************************************        
157500*                                                                *        
157600*    BERAKNING AV ÅTGÅNG I VECKAN DÅ OMSPECEN                    *        
157700*    GÖRS.                                                       *        
157800*                                                                *        
157900******************************************************************        
158000                                                                          
158100     MOVE 'EDA-ATG-I-VECKA ' TO CURRENT-SECTION                           
158200                                                                          
158300     MOVE ZERO  TO W-VECKO-ATGANG                                         
158400                   W-TIFINLV-CCAAMMDD                                     
158500                                                                          
158600     COMPUTE W-SPAR-TOT-KVPB-REF = SLAG-KVPB-REF *                        
158700                                   SLAG-RESEASON(W-AKT-SEASON)            
158800                                                                          
158900     COMPUTE W-VECKO-PB = W-SPAR-TOT-KVPB-REF / 4.33                      
159000                                                                          
159100                                                                          
159200     EVALUATE W-DAT-TID-AKT                                               
159300       WHEN 1                                                             
159400         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 1                  
159500       WHEN 2                                                             
159600         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.8                
159700       WHEN 3                                                             
159800         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.6                
159900       WHEN 4                                                             
160000         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.4                
160100       WHEN 5                                                             
160200         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0.2                
160300       WHEN 6                                                             
160400         COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0                  
160500     END-EVALUATE                                                         
160600***  IF PUBWEEK IN FUTURE THEN CURR WEEK PB SHOULD BE ZERO                
160700     IF W-SPAR-LART-DAPUBL > ZERO                                         
160800        MOVE W-SPAR-LART-DAPUBL  TO W-TIFINLV-CCAAMMDD                    
160900     ELSE                                                                 
161000        MOVE ART-TIFINLV         TO DAT-I-TIDATUM                         
161100        MOVE 'AAVVD'             TO DAT-KDDATFORM                         
161200        CALL WDATKONV         USING DAT-KDDATFORM                         
161300                                    DAT-I-TIDATUM                         
161400                                    DAT-O-TIDATUM                         
161500                                    DAT-KDSVAR                            
161600        MOVE DAT-TISEKEL         TO W-TIFINLV-CC                          
161700        MOVE DAT-TIAAMMDD        TO W-TIFINLV-AAMMDD                      
161800     END-IF                                                               
161900     IF W-DATUM-SSAAMMDD-AKT < W-TIFINLV-CCAAMMDD                         
162000        COMPUTE W-VECKO-ATGANG ROUNDED = W-VECKO-PB * 0                   
162100     END-IF                                                               
162200     .                                                                    
162300                                                                          
162400 EE-SPEC-NYTT-FOERSLAG SECTION.                                           
162500******************************************************************        
162600*                                                                *        
162700*    SPEC AV NYTT FÖRSLAG FRÅN LINK-TISPECST DATUM UNDER         *        
162800*    W-KVVECKOR-SPEC VECKOR                                      *        
162900*    BESTÄLLNINGSREST TÄCKS FÖR VVKL 1 OCH 2                     *        
163000*                                                                *        
163100******************************************************************        
163200                                                                          
163300     COMPUTE W-BUFF = W-SPAR-KVSLAGER                                     
163400                                                                          
163500     MOVE W-DATUM-AAVV       TO W-DATUM-FROM                              
163600     MOVE W-TISPECST-DISP    TO W-DATUM-TOM                               
163700     PERFORM S102-BERAKNA-VECKODIFFERENS                                  
163800     IF W-VECKO-DIFFERENS < ZERO                                          
163900        MOVE +1              TO W-VECKO-DIFFERENS                         
164000     END-IF                                                               
164100                                                                          
164200     MOVE W-VECKO-DIFFERENS  TO TILLGTAB-IX                               
164300     ADD  W-KVVECKOR-SPEC    TO W-VECKO-DIFFERENS                         
164400     MOVE JA                 TO FOERST-SW                                 
164500                                                                          
164600     IF W-VECKO-DIFFERENS > TILLGTAB-MAX                                  
164700        MOVE TILLGTAB-MAX    TO W-VECKO-DIFFERENS                         
164800     END-IF                                                               
164900                                                                          
165000     COMPUTE W-ARSOMS = 12                                                
165100         * ( SLAG-KVPB-REF                                                
165200           + SLAG-KVPBREOI)                                               
165300         *   W-SPAR-LART-PRMATRL                                          
165400                                                                          
165500     IF W-ARSOMS > W-ARSOMS-100000                                        
165600        MOVE 1  TO W-Q-FREKV-MAX                                          
165700     ELSE                                                                 
165800        MOVE 3  TO W-Q-FREKV-MAX                                          
165900     END-IF                                                               
166000                                                                          
166100     COMPUTE W-ARSBEH = 12                                                
166200         * ( SLAG-KVPB-REF                                                
166300           + SLAG-KVPBREOI)                                               
166400                                                                          
166500     PERFORM EEA-LANDKOD-FRYSTID                                          
166600                                                                          
166700     PERFORM UNTIL TILLGTAB-IX NOT < W-VECKO-DIFFERENS                    
166800                                                                          
166900        IF TILLGTAB-IX NOT > ZERO                                         
167000           MOVE 1 TO TILLGTAB-IX                                          
167100        END-IF                                                            
167200        ADD TILLGTAB-ANTAL (TILLGTAB-IX)                                  
167300                               TO W-TILLG                                 
167400        SUBTRACT BHDC-KVBEHOV-VECKA  (TILLGTAB-IX)                        
167500                             FROM W-TILLG                                 
167600                                                                          
167700        IF W-TILLG < W-BUFF                                               
167800           PERFORM EEB-BERAEKNA-AVROPSKVANTITET                           
167900           ADD W-AVROPSKVANTITET  TO W-TILLG                              
168000           MOVE W-AVROPSKVANTITET TO D905-KVAVROP                         
168100           IF W-AVROPSKVANTITET > ZERO                                    
168200              PERFORM EEC-SKAPA-AVROP                                     
168300           END-IF                                                         
168400        END-IF                                                            
168500        ADD 1 TO TILLGTAB-IX                                              
168600     END-PERFORM                                                          
168700     .                                                                    
168800     EJECT                                                                
168900                                                                          
169000 EEA-LANDKOD-FRYSTID SECTION.                                             
169100                                                                          
169200     MOVE 'EEA-LAN-FRYSTID ' TO CURRENT-SECTION                           
169300                                                                          
169400**   LÄS WDF106 GU OKVAL  ADR-IDLANDX2                                    
169500**   BERÄKNA FRYSGRÄNS  DAGENS + XLAG-KVVECKOR-FT                         
169600**                                                                        
169700                                                                          
169800***  VILKET IDLEVNR?? DUGER SLAG-IDLEVNR SOM FINNS I W-IDLEVNR ??         
169900                                                                          
170000     MOVE XLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                           
170100                                                                          
170200     PERFORM IMS-GU-WDF106                                                
170300     IF SEGMENT-FINNS                                                     
170400        MOVE ADR-IDLANDX2     TO WS-IDLANDX2-SHIP                         
170500     ELSE                                                                 
170600        MOVE SPACE            TO WS-IDLANDX2-SHIP                         
170700     END-IF                                                               
170800                                                                          
170900*--- LÄGG DAGENS DATUM I FRYSTID. ADDERA MED FT*7                         
171000     MOVE W-DATUM-AAMMDD-AKT  TO WS-FRYSTID                               
171100                                                                          
171200*--- ANTAL DGR FRYSTID*7 + 1                                              
171300*--- DAGKONV DD + FT                                                      
171400     MOVE 002                 TO DAG-KDCALL                               
171500     MOVE 20                  TO DAG-TISEKEL-FOM                          
171600     MOVE W-DATUM-AAMMDD-AKT  TO DAG-TIAAMMDD-FOM                         
171700     COMPUTE DAG-KVKALDAG = (XLAG-KVVECKOR-FT * 7) + 2                    
171800     CALL WDAGKONV USING DAG-KDCALL,                                      
171900                         DAG-DATUM-AREA,                                  
172000                         DAG-KDSVAR                                       
172100     IF DAG-KDSVAR = SPACE                                                
172200        MOVE DAG-TISEKEL-TOM  TO WS-FRYSTID-SEKEL                         
172300        MOVE DAG-TIAAMMDD-TOM TO WS-FRYSTID                               
172400     ELSE                                                                 
172500        MOVE 'FEL VID ANROP TILL DAGKONV 2'                               
172600                              TO FELTEXT                                  
172700        CALL FELLOG                                                       
172800     END-IF                                                               
172900     .                                                                    
173000                                                                          
173100                                                                          
173200                                                                          
173300                                                                          
173400 EEB-BERAEKNA-AVROPSKVANTITET SECTION.                                    
173500                                                                          
173600     MOVE 'EEB-AVROPSKVANT ' TO CURRENT-SECTION                           
173700                                                                          
173800     MOVE SLAG-KVREFBER      TO W-KVANTITET                               
173900                                                                          
174000     IF XLAG-KVULOAD > ZERO                                               
174100        MOVE XLAG-KVULOAD    TO WS-KVULOAD                                
174200     ELSE                                                                 
174300        MOVE XLAG-KVPALL     TO WS-KVULOAD                                
174400     END-IF                                                               
174500     IF WS-KVULOAD > ZERO                                                 
174600        PERFORM UNTIL (W-TILLG + W-KVANTITET) >=                          
174700                       W-BUFF                                             
174800**********************(W-BUFF  + XLAG-KVEOQ)                              
174900          ADD WS-KVULOAD    TO W-KVANTITET                                
175000        END-PERFORM                                                       
175100        MOVE W-KVANTITET     TO W-AVROPSKVANTITET                         
175200     ELSE                                                                 
175300        IF  W-KVANTITET = ZERO                                            
175400            MOVE 1 TO W-KVANTITET                                         
175500        END-IF                                                            
175600                                                                          
175700        COMPUTE W-ANTAL     ROUNDED =  ((W-KVANTITET / 2                  
175800                                  - W-TILLG                               
175900                                  + W-BUFF)                               
176000                                /   W-KVANTITET)                          
176100                                +   0.49                                  
176200        COMPUTE W-AVROPSKVANTITET ROUNDED =                               
176300                                        W-KVANTITET * W-ANTAL             
176400     END-IF                                                               
176500                                                                          
176600     IF  W-AVROPSKVANTITET < 1                                            
176700         MOVE 1 TO W-AVROPSKVANTITET                                      
176800     END-IF                                                               
176900     .                                                                    
177000                                                                          
177100                                                                          
177200                                                                          
177300 EEC-SKAPA-AVROP SECTION.                                                 
177400******************************************************************        
177500*                                                                *        
177600*    AVROP SKAPAS OCH SKRIVES PÅ REGISTRET                       *        
177700*                                                                *        
177800******************************************************************        
177900                                                                          
178000     MOVE 'EEC-SKAPA-AVROP '   TO CURRENT-SECTION                         
178100                                                                          
178200     MOVE TILLGTAB-IX          TO W-ANTAL-VECKOR                          
178300     MOVE W-DATUM-AAVV         TO DATUM-AAVV                              
178400     CALL W009VADD USING DATUM-AAVV W-ANTAL-VECKOR                        
178500                                                                          
178600     MOVE DATUM-AAVV          TO WS-DAYS-TIAAVV                           
178700     MOVE WS-DAYS-TIAAVV      TO DAYS-TIDATE1                             
178800                                 WS-TIAVROP-DISP                          
178900     MOVE 'YYWW'              TO DAYS-KDDATFMT1                           
179000     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
179100     MOVE 0                   TO DAYS-KVDAYS                              
179200     MOVE SPACE               TO DAYS-TIDATE2                             
179300                                 DAYS-IDCALEND                            
179400     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
179500*                                                                         
179600     IF DAYS-KDRC = 8                                                     
179700       MOVE 'FEL VID ANROP TILL WZ20DAYS 8' TO FELTEXT                    
179800       CALL FELLOG                                                        
179900     ELSE                                                                 
180000       MOVE DAYS-TIDATE2(1:6) TO D905-TIAVRDAT-DISP                       
180100     END-IF                                                               
180200*                                                                         
180300     COMPUTE W-ANTAL-VECKOR ROUNDED =                                     
180400             XLAG-KVDAGAR-FFH / -5                                        
180500                                                                          
180600     MOVE WS-TIAVROP-DISP     TO W-DAAVROP-AAVV                           
180700     MOVE W-DAAVROP-AAVV      TO W-AAVV-ADD                               
180800     CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                        
180900     MOVE W-AAVV-ADD          TO W-DAAVROP-AAVV                           
181000     MOVE W-DAAVROP-AAVV      TO W-TIAVROP-AVS                            
181100                                                                          
181200     PERFORM EECA-TILEVDAG                                                
181300                                                                          
181400*AVS-AAMMDD                                                               
181500     COMPUTE WS-DAYS-TIAAVVD-AVS = 10 * W-TIAVROP-AVS +                   
181600                                    D905-TILEVDAG                         
181700     MOVE WS-DAYS-TIAAVVD-AVS TO DAYS-TIDATE1                             
181800     MOVE 'YYWWD'             TO DAYS-KDDATFMT1                           
181900     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
182000     MOVE 0                   TO DAYS-KVDAYS                              
182100     MOVE SPACE               TO DAYS-TIDATE2                             
182200                                 DAYS-IDCALEND                            
182300     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
182400*                                                                         
182500     IF DAYS-KDRC = 8                                                     
182600       MOVE 'FEL VID ANROP TILL WZ20DAYS 9' TO FELTEXT                    
182700       CALL FELLOG                                                        
182800     ELSE                                                                 
182900       MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                           
183000                                 ARB-TIAAMMDD-AVS                         
183100     END-IF                                                               
183200*                                                                         
183300     MOVE D905-TILEVDAG       TO WOL-TILEVDAG                             
183400     MOVE W-TIAVROP-AVS       TO WOL-TIAAVV-AVS                           
183500                                                                          
183600*    AVS TVÅ VECKOR BAKÅT TIFINLEV ÄR NÄRA                                
183700     IF W-HELP-DATUM-SSAAVV < W-HELP-TIFINLV-SSAAVV                       
183800     AND WS-TIAVROP-DISP    = W-HELP-TIFINLV-AAVV                         
183900*    IF  W-DATUM-AAVV     < W-TIFINLV-1-4                                 
184000*    AND WS-TIAVROP-DISP  = W-TIFINLV-1-4                                 
184100                                                                          
184200*       UNDERSÖKNING OM AVS BLEV FÖR NÄRA I TID                           
184300        MOVE W-DATUM-AAVV     TO W-DATUM-AAVV-HELP                        
184400        MOVE +1 TO W-ANTAL-VECKOR                                         
184500        CALL W009VADD USING W-DATUM-AAVV-HELP W-ANTAL-VECKOR              
184600        IF W-DAAVROP-AAVV < W-DATUM-AAVV-HELP                             
184700           MOVE W-DATUM-AAVV-HELP TO W-DAAVROP-AAVV                       
184800        END-IF                                                            
184900                                                                          
185000        MOVE W-DAAVROP-AAVV   TO W-TIAVROP-AVS                            
185100        COMPUTE WS-DAYS-TIAAVVD-AVS = 10 * W-TIAVROP-AVS +                
185200                                       D905-TILEVDAG                      
185300        MOVE WS-DAYS-TIAAVVD-AVS TO DAYS-TIDATE1                          
185400        MOVE 'YYWWD'          TO DAYS-KDDATFMT1                           
185500        MOVE 'YYMMDD'         TO DAYS-KDDATFMT2                           
185600        MOVE 0                TO DAYS-KVDAYS                              
185700        MOVE SPACE            TO DAYS-TIDATE2                             
185800                                    DAYS-IDCALEND                         
185900        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
186000*                                                                         
186100        IF DAYS-KDRC = 8                                                  
186200          MOVE 'FEL VID ANROP TILL WZ20DAYS10' TO FELTEXT                 
186300          CALL FELLOG                                                     
186400        ELSE                                                              
186500          MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                        
186600                                    ARB-TIAAMMDD-AVS                      
186700          MOVE D905-TILEVDAG     TO WOL-TILEVDAG                          
186800          MOVE W-TIAVROP-AVS     TO WOL-TIAAVV-AVS                        
186900        END-IF                                                            
187000*                                                                         
187100     END-IF                                                               
187200*FIX NYÅR START DATKONVJUST 2014                                          
187300     COMPUTE FIX-AAVVD  = 10 * W-TIAVROP-AVS +                            
187400                          D905-TILEVDAG                                   
187500     IF FIX-AAVVD = 15011                                                 
187600       MOVE 141229   TO W-TIAAMMDD-AVS                                    
187700                        ARB-TIAAMMDD-AVS                                  
187800       MOVE 1        TO WOL-TILEVDAG                                      
187900       MOVE 1501     TO WOL-TIAAVV-AVS                                    
188000     END-IF                                                               
188100     IF FIX-AAVVD = 15012                                                 
188200       MOVE 141230   TO W-TIAAMMDD-AVS                                    
188300                        ARB-TIAAMMDD-AVS                                  
188400       MOVE 2        TO WOL-TILEVDAG                                      
188500       MOVE 1501     TO WOL-TIAAVV-AVS                                    
188600     END-IF                                                               
188700     IF FIX-AAVVD = 15013                                                 
188800       MOVE 141231   TO W-TIAAMMDD-AVS                                    
188900                        ARB-TIAAMMDD-AVS                                  
189000       MOVE 3        TO WOL-TILEVDAG                                      
189100       MOVE 1501     TO WOL-TIAAVV-AVS                                    
189200     END-IF                                                               
189300*FIX NYÅR END DATKONVJUST 2014                                            
189400                                                                          
189500                                                                          
189600     IF (SLAG-KVREFBER > ZERO)        AND                                 
189700        ((W-ARSBEH / SLAG-KVREFBER) > 35)                                 
189800                                                                          
189900**   DAGLIGA AVROP BEHANDLAS SENARE                                       
190000       CONTINUE                                                           
190100     ELSE                                                                 
190200**   LÄS WDF301 GU LANDKOD IDLANDX2 DATUM                                 
190300**   FINNS UNDERLIGGANDE SEGMENT MED IDLEVNR ?                            
190400**   OM TRÄFF JUSTERA DATUM (OBS D905-TILEVDAG ?)                         
190500                                                                          
190600       PERFORM S100-KOLL-HELGDAG                                          
190700     END-IF                                                               
190800                                                                          
190900*INL + DISP                                                               
191000     PERFORM S101-BERAEKNA-INL-DISP-AAMMDD                                
191100                                                                          
191200     MOVE 20            TO W-DAAVROP-SS                                   
191300     MOVE W-DAAVROP-AVS TO D905-DAAVROP-AVS                               
191400                                                                          
191500*    HÄR GÖRS EN TEST SOM ANGER VILKA AVROP SOM SKALL                     
191600*    SMETAS UT PÅ LEVERANTÖRENS LEVDAGAR AVROPS-VECKAN                    
191700                                                                          
191800     IF (SLAG-KVREFBER > ZERO)        AND                                 
191900        ((W-ARSBEH / SLAG-KVREFBER) > 35)                                 
192000                                                                          
192100        PERFORM EECB-SKAPA-DAGL-AVROP                                     
192200                                                                          
192300     ELSE                                                                 
192400                                                                          
192500        IF D905-KVAVROP > ZERO                                            
192600           MOVE D905-DAAVROP-AVS  TO W-DAAVROP-KY                         
192700           MOVE D905-TILEVDAG     TO W-TILEVDAG-KY                        
192800           MOVE D905-KDAVROP      TO W-KDAVROP                            
192900           PERFORM IMS-GHU-WDD905                                         
193000           IF SEGMENT-FINNS                                               
193100              ADD D905-2-KVAVROP  TO D905-KVAVROP                         
193200              PERFORM IMS-REPL-WDD905                                     
193300           ELSE                                                           
193400              PERFORM IMS-ISRT-WDD905                                     
193500           END-IF                                                         
193600        END-IF                                                            
193700     END-IF                                                               
193800     .                                                                    
193900                                                                          
194000                                                                          
194100                                                                          
194200 EECA-TILEVDAG            SECTION.                                        
194300                                                                          
194400     MOVE 'EECA-TILEVDAG   '   TO CURRENT-SECTION                         
194500                                                                          
194600     MOVE ZERO                     TO D905-TILEVDAG                       
194700                                                                          
194800     MOVE +1                       TO IX-DAG                              
194900     PERFORM UNTIL IX-DAG > 5                                             
195000        IF XLAG-TILEVDAG (IX-DAG) > ZERO                                  
195100           MOVE XLAG-TILEVDAG (IX-DAG) TO D905-TILEVDAG                   
195200           MOVE +5                 TO IX-DAG                              
195300        END-IF                                                            
195400        ADD +1                     TO IX-DAG                              
195500     END-PERFORM                                                          
195600                                                                          
195700     IF D905-TILEVDAG = ZERO                                              
195800        MOVE XLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                        
195900        PERFORM IMS-GU-WDF116-SHIP                                        
196000                                                                          
196100        IF SEGMENT-FINNS                                                  
196200          MOVE +1                  TO IX-DAG                              
196300          PERFORM UNTIL IX-DAG > 5                                        
196400             IF NDC-TILEVDAG (IX-DAG) > ZERO                              
196500                MOVE NDC-TILEVDAG (IX-DAG) TO D905-TILEVDAG               
196600                MOVE +5            TO IX-DAG                              
196700             END-IF                                                       
196800             ADD +1                TO IX-DAG                              
196900          END-PERFORM                                                     
197000          IF D905-TILEVDAG = ZERO                                         
197100             MOVE +1               TO D905-TILEVDAG                       
197200          END-IF                                                          
197300        ELSE                                                              
197400          MOVE +1                  TO D905-TILEVDAG                       
197500        END-IF                                                            
197600                                                                          
197700     END-IF                                                               
197800     .                                                                    
197900                                                                          
198000                                                                          
198100                                                                          
198200 EECB-SKAPA-DAGL-AVROP         SECTION.                                   
198300                                                                          
198400     MOVE 'EECB-DAGL-AVROP  '  TO CURRENT-SECTION                         
198500                                                                          
198600     MOVE ZERO                TO W-TILEVDAG (1)                           
198700                                 W-TILEVDAG (2)                           
198800                                 W-TILEVDAG (3)                           
198900                                 W-TILEVDAG (4)                           
199000                                 W-TILEVDAG (5)                           
199100     MOVE ZERO                TO ANT-LEVDAG                               
199200                                                                          
199300     MOVE +1                  TO IX-DAG                                   
199400     PERFORM UNTIL IX-DAG > 5                                             
199500        IF XLAG-TILEVDAG (IX-DAG) > ZERO                                  
199600           MOVE XLAG-TILEVDAG (IX-DAG) TO                                 
199700                W-TILEVDAG (IX-DAG)                                       
199800           ADD +1             TO ANT-LEVDAG                               
199900        END-IF                                                            
200000        ADD +1                TO IX-DAG                                   
200100     END-PERFORM                                                          
200200     IF ANT-LEVDAG = ZERO                                                 
200300        MOVE XLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                        
200400        PERFORM IMS-GU-WDF116-SHIP                                        
200500        IF SEGMENT-FINNS                                                  
200600          MOVE +1                  TO IX-DAG                              
200700          PERFORM UNTIL IX-DAG > 5                                        
200800             IF NDC-TILEVDAG (IX-DAG) > ZERO                              
200900                MOVE NDC-TILEVDAG (IX-DAG) TO                             
201000                     W-TILEVDAG (IX-DAG)                                  
201100                ADD +1             TO ANT-LEVDAG                          
201200             END-IF                                                       
201300             ADD +1                TO IX-DAG                              
201400          END-PERFORM                                                     
201500          IF ANT-LEVDAG = ZERO                                            
201600             MOVE +1               TO W-TILEVDAG (1)                      
201700                                      ANT-LEVDAG                          
201800          END-IF                                                          
201900        ELSE                                                              
202000          MOVE +1                  TO W-TILEVDAG (1)                      
202100                                      ANT-LEVDAG                          
202200        END-IF                                                            
202300     END-IF                                                               
202400                                                                          
202500*KVAVROP/DAG                                                              
202600     MOVE ZERO                  TO W-KVAVROP (1)                          
202700                                   W-KVAVROP (2)                          
202800                                   W-KVAVROP (3)                          
202900                                   W-KVAVROP (4)                          
203000                                   W-KVAVROP (5)                          
203100     IF XLAG-KVPALL < +1                                                  
203200        MOVE +1                 TO WS-KVPALL                              
203300     ELSE                                                                 
203400        MOVE XLAG-KVPALL        TO WS-KVPALL                              
203500     END-IF                                                               
203600     IF SLAG-KVREFBER > ZERO                                              
203700        MOVE SLAG-KVREFBER      TO WS-KVPALL                              
203800     END-IF                                                               
203900                                                                          
204000     IF D905-KVAVROP > ZERO                                               
204100        PERFORM UNTIL (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP(3)         
204200                     + W-KVAVROP(4) + W-KVAVROP(5))                       
204300                     NOT < D905-KVAVROP                                   
204400          MOVE +1               TO IX-DAG                                 
204500          PERFORM UNTIL    IX-DAG > 5                                     
204600            IF W-TILEVDAG (IX-DAG) > ZERO                                 
204700               IF (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP (3) +          
204800                   W-KVAVROP(4) + W-KVAVROP(5)) < D905-KVAVROP            
204900                   ADD WS-KVPALL TO W-KVAVROP (IX-DAG)                    
205000               END-IF                                                     
205100            END-IF                                                        
205200            ADD +1               TO IX-DAG                                
205300            IF IX-DAG = 6                                                 
205400               IF XLAG-KVULOAD > ZERO                                     
205500                  MOVE XLAG-KVULOAD TO WS-KVULOAD                         
205600               ELSE                                                       
205700                  MOVE XLAG-KVPALL  TO WS-KVULOAD                         
205800               END-IF                                                     
205900               IF WS-KVULOAD > ZERO                                       
206000                  MOVE WS-KVULOAD   TO WS-KVPALL                          
206100               END-IF                                                     
206200            END-IF                                                        
206300          END-PERFORM                                                     
206400        END-PERFORM                                                       
206500     END-IF                                                               
206600                                                                          
206700*AVS-AAVV                                                                 
206800     MOVE +1                    TO IX-DAG                                 
206900     PERFORM UNTIL   (IX-DAG) > 5                                         
207000       MOVE W-TIAVROP-AVS     TO SPAR-TIAVROP-AVS                         
207100       IF  W-TILEVDAG (IX-DAG) > ZERO                                     
207200       AND W-KVAVROP  (IX-DAG) > ZERO                                     
207300*AVS-AAMMDD                                                               
207400          COMPUTE WS-DAYS-TIAAVVD-AVS = 10 * W-TIAVROP-AVS +              
207500                                         W-TILEVDAG (IX-DAG)              
207600          MOVE WS-DAYS-TIAAVVD-AVS TO DAYS-TIDATE1                        
207700          MOVE 'YYWWD'          TO DAYS-KDDATFMT1                         
207800          MOVE 'YYMMDD'         TO DAYS-KDDATFMT2                         
207900          MOVE 0                TO DAYS-KVDAYS                            
208000          MOVE SPACE            TO DAYS-TIDATE2                           
208100                                   DAYS-IDCALEND                          
208200          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
208300*                                                                         
208400          IF DAYS-KDRC = 8                                                
208500            MOVE 'FEL VID ANROP TILL WZ20DAYS 11' TO FELTEXT              
208600            CALL FELLOG                                                   
208700          ELSE                                                            
208800            MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                      
208900                                      ARB-TIAAMMDD-AVS                    
209000            MOVE W-TILEVDAG (IX-DAG) TO WOL-TILEVDAG                      
209100            MOVE W-TIAVROP-AVS     TO WOL-TIAAVV-AVS                      
209200            MOVE W-TIAVROP-AVS     TO W-DAAVROP-AVS                       
209300          END-IF                                                          
209400*                                                                         
209500*FIX NYÅR START DATKONVJUST 2014                                          
209600          COMPUTE FIX-AAVVD  = 10 * W-TIAVROP-AVS +                       
209700                               W-TILEVDAG (IX-DAG)                        
209800          IF FIX-AAVVD = 15011                                            
209900            MOVE 141229   TO W-TIAAMMDD-AVS                               
210000                             ARB-TIAAMMDD-AVS                             
210100            MOVE 1        TO WOL-TILEVDAG                                 
210200            MOVE 1501     TO WOL-TIAAVV-AVS                               
210300          END-IF                                                          
210400          IF FIX-AAVVD = 15012                                            
210500            MOVE 141230   TO W-TIAAMMDD-AVS                               
210600                             ARB-TIAAMMDD-AVS                             
210700            MOVE 2        TO WOL-TILEVDAG                                 
210800            MOVE 1501     TO WOL-TIAAVV-AVS                               
210900          END-IF                                                          
211000          IF FIX-AAVVD = 15013                                            
211100            MOVE 141231   TO W-TIAAMMDD-AVS                               
211200                             ARB-TIAAMMDD-AVS                             
211300            MOVE 3        TO WOL-TILEVDAG                                 
211400            MOVE 1501     TO WOL-TIAAVV-AVS                               
211500          END-IF                                                          
211600*FIX NYÅR END DATKONVJUST 2014                                            
211700                                                                          
211800**    LÄS WDF301 GU LANDKOD IDLANDX2 DATUM                                
211900**    FINNS UNDERLIGGANDE SEGMENT MED IDLEVNR ?                           
212000**    OM TRÄFF JUSTERA DATUM                                              
212100                                                                          
212200          PERFORM S100-KOLL-HELGDAG                                       
212300                                                                          
212400*INL + DISP                                                               
212500          PERFORM S101-BERAEKNA-INL-DISP-AAMMDD                           
212600                                                                          
212700          MOVE 20                    TO W-DAAVROP-SS                      
212800          MOVE W-DAAVROP-AVS         TO D905-DAAVROP-AVS                  
212900                                        W-DAAVROP-KY                      
213000          MOVE WOL-TILEVDAG          TO D905-TILEVDAG                     
213100                                        W-TILEVDAG-KY                     
213200          MOVE W-KVAVROP (IX-DAG) TO D905-KVAVROP                         
213300          MOVE D905-KDAVROP          TO W-KDAVROP                         
213400                                                                          
213500          PERFORM IMS-GHU-WDD905                                          
213600          IF SEGMENT-FINNS                                                
213700             ADD D905-2-KVAVROP      TO D905-KVAVROP                      
213800             PERFORM IMS-REPL-WDD905                                      
213900          ELSE                                                            
214000             PERFORM IMS-ISRT-WDD905                                      
214100          END-IF                                                          
214200          MOVE SPAR-TIAVROP-AVS      TO W-TIAVROP-AVS                     
214300       END-IF                                                             
214400                                                                          
214500       ADD +1 TO IX-DAG                                                   
214600     END-PERFORM                                                          
214700     .                                                                    
214800     EJECT                                                                
214900 F-UPPDATERA-WDK7 SECTION.                                                
215000                                                                          
215100     MOVE 'F-UPPDATERA-WDK7 '  TO CURRENT-SECTION                         
215200                                                                          
215300     PERFORM IMS-GHU-WDK722                                               
215400                                                                          
215500     MOVE 5            TO XLAG-KDLPSP                                     
215600     MOVE W-DATUM-AAVV TO XLAG-TIOMSPEC                                   
215700                          XLAG-TILPSP                                     
215800     COMPUTE W-ARSOMS = 12                                                
215900          * (SLAG-KVPB-REF + SLAG-KVPBREOI)                               
216000          *  W-SPAR-LART-PRMATRL                                          
216100                                                                          
216200     IF W-ARSOMS > W-ARSOMS-100000                                        
216300        MOVE 1 TO W-ANTAL-VECKOR                                          
216400     ELSE                                                                 
216500        MOVE 3 TO W-ANTAL-VECKOR                                          
216600     END-IF                                                               
216700     CALL W009VADD USING XLAG-TILPSP W-ANTAL-VECKOR                       
216800                                                                          
216900     PERFORM IMS-REPL-WDK722                                              
217000     .                                                                    
217100                                                                          
217200                                                                          
217300                                                                          
217400 G-SKAPA-OMSPEC-SEGMENT SECTION.                                          
217500                                                                          
217600     MOVE 'G-SKAPA-OMSPEC   '  TO CURRENT-SECTION                         
217700                                                                          
217800     MOVE ZERO                 TO W-KDLPORS-TAB(1)                        
217900                                  W-KDLPORS-TAB(2)                        
218000                                  W-KDLPORS-TAB(3)                        
218100     MOVE 17                   TO W-KDLPORS-TAB(4)                        
218200                                                                          
218300     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
218400                                                                          
218500     MOVE ZERO                 TO D904-KDPLKOEP                           
218600     MOVE W-KDLPORS-TAB(1)     TO D904-KDLPORS-TAB(1)                     
218700     MOVE W-KDLPORS-TAB(2)     TO D904-KDLPORS-TAB(2)                     
218800     MOVE W-KDLPORS-TAB(3)     TO D904-KDLPORS-TAB(3)                     
218900                                                                          
219000     PERFORM IMS-ISRT-WDD904                                              
219100     .                                                                    
219200                                                                          
219300                                                                          
219400                                                                          
219500 H-SKAPA-FORSLAGSPOST-PA-KOE SECTION.                                     
219600                                                                          
219700     MOVE 'H-FÖRSLAG-PÅ-KÖ  '  TO CURRENT-SECTION                         
219800                                                                          
219900*    --- LÄGGER UPP NYTT LEV.PLANEFÖRSLAG PÅ WDD6-KÖN                     
220000     MOVE OMSP-IDDC                TO W-IDDC-D6                           
220100     MOVE SLAG-IDLEVNR             TO W-IDLEVNR-D6                        
220200     MOVE OMSP-IDARTNR             TO W-IDARTNR-D6                        
220300     MOVE XLAG-IDANSK              TO W-IDANSK-D6                         
220400                                                                          
220500     PERFORM IMS-GHU-WDD601                                               
220600     IF SEGMENT-FINNS                                                     
220700       MOVE W-KDLPORS-TAB(1)         TO LPF-KDLPORS(1)                    
220800       MOVE W-KDLPORS-TAB(2)         TO LPF-KDLPORS(2)                    
220900       MOVE W-KDLPORS-TAB(3)         TO LPF-KDLPORS(3)                    
221000       MOVE XLAG-KDLEVPLF            TO LPF-KDLEVPLF                      
221100       MOVE XLAG-TIOMSPEC            TO LPF-TIOMSPEC                      
221200       MOVE W-DATUM-AAMMDD-AKT       TO LPF-TIUPPDAT                      
221300       MOVE SPACE                    TO LPF-TELPORSX                      
221400                                                                          
221500*    -- HÄMTA BENÄMNING                                                   
221600       PERFORM IMS-GU-WDD311-BSEQ                                         
221700       IF SEGMENT-FINNS                                                   
221800          MOVE TEXT-BEART            TO LPF-BEART                         
221900       ELSE                                                               
222000*       MOVE 'BENÄMNING SAKNAS' TO LPF-BEART                              
222100          MOVE 'DESCR. MISSING  '    TO LPF-BEART                         
222200       END-IF                                                             
222300                                                                          
222400       PERFORM IMS-REPL-WDD601                                            
222500     ELSE                                                                 
222600       MOVE OMSP-IDDC                TO LPF-IDDC                          
222700       MOVE SLAG-IDLEVNR             TO LPF-IDLEVNR                       
222800       MOVE OMSP-IDARTNR             TO LPF-IDARTNR                       
222900       MOVE XLAG-IDANSK              TO LPF-IDANSK                        
223000       MOVE W-KDLPORS-TAB(1)         TO LPF-KDLPORS(1)                    
223100       MOVE W-KDLPORS-TAB(2)         TO LPF-KDLPORS(2)                    
223200       MOVE W-KDLPORS-TAB(3)         TO LPF-KDLPORS(3)                    
223300       MOVE XLAG-KDLEVPLF            TO LPF-KDLEVPLF                      
223400       MOVE XLAG-TIOMSPEC            TO LPF-TIOMSPEC                      
223500       MOVE W-DATUM-AAMMDD-AKT       TO LPF-TIUPPDAT                      
223600       MOVE SPACE                    TO LPF-TELPORSX                      
223700                                                                          
223800*    -- HÄMTA BENÄMNING                                                   
223900       PERFORM IMS-GU-WDD311-BSEQ                                         
224000       IF SEGMENT-FINNS                                                   
224100          MOVE TEXT-BEART            TO LPF-BEART                         
224200       ELSE                                                               
224300*       MOVE 'BENÄMNING SAKNAS' TO LPF-BEART                              
224400          MOVE 'DESCR. MISSING  '    TO LPF-BEART                         
224500       END-IF                                                             
224600                                                                          
224700       PERFORM IMS-ISRT-WDD601                                            
224800     END-IF                                                               
224900                                                                          
225000* SE OM DET FINNS FLER RADER MED ANNAT IDANSK? DUBLETTER.                 
225100* DESSA MÅSTE DELETE'S ANNARS ABENDAR W2215900, W200V1.                   
225200     MOVE OMSP-IDDC            TO W-IDDC-MIN                              
225300                                  W-IDDC-MAX                              
225400     MOVE SLAG-IDLEVNR         TO W-IDLEVNR-MIN                           
225500                                  W-IDLEVNR-MAX                           
225600     MOVE OMSP-IDARTNR         TO W-IDARTNR-MIN                           
225700                                  W-IDARTNR-MAX                           
225800     MOVE ZERO                 TO W-IDANSK-MIN                            
225900     MOVE 999                  TO W-IDANSK-MAX                            
226000     MOVE XLAG-IDANSK          TO W-IDANSK-NON                            
226100     PERFORM IMS-GHU-WDD601-FLERA                                         
226200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
226300       PERFORM IMS-DLET-WDD601                                            
226400       PERFORM IMS-GHN-WDD601-FLERA                                       
226500     END-PERFORM                                                          
226600     .                                                                    
226700     EJECT                                                                
226800                                                                          
226900                                                                          
227000 Z-FINIT SECTION.                                                         
227100                                                                          
227200     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
227300                                                                          
227400     .                                                                    
227500     EJECT                                                                
227600 S10-NOLLA-BHDC-RESULTATFLT  SECTION.                                     
227700     MOVE 'S10-NOLLA-BHDC-RESULTATFLT '  TO CURRENT-SECTION               
227800                                                                          
227900     MOVE ZERO                   TO BHDC-KVBEHOV-SUMMA                    
228000     MOVE ZERO                   TO BHDC-KVBEHOV-DESSUTOM                 
228100     MOVE ZERO                   TO BHDC-TIBEHOV-FIRST                    
228200                                                                          
228300     MOVE 1   TO IX                                                       
228400     PERFORM UNTIL IX > MAX-IX                                            
228500       MOVE ZERO                 TO BHDC-KVBEHOV-VECKA(IX)                
228600                                                                          
228700       ADD 1  TO IX                                                       
228800     END-PERFORM                                                          
228900     .                                                                    
229000     EJECT                                                                
229100                                                                          
229200 S100-KOLL-HELGDAG  SECTION.                                              
229300                                                                          
229400     MOVE 'S100-KOLLA-HELG '   TO CURRENT-SECTION                         
229500                                                                          
229600     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
229700     MOVE 20               TO W-DADATUM-HELG-SS                           
229800     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
229900                                                                          
230000     PERFORM IMS-GU-WDF301                                                
230100                                                                          
230200     IF SEGMENT-FINNS                                                     
230300                                                                          
230400        PERFORM S100A-SOEK-NY-AVS-DAG                                     
230500                                                                          
230600        MOVE ARB-TIAAMMDD-AVS      TO W-TIAAMMDD-AVS                      
230700*                                                                         
230800        MOVE ARB-TIAAMMDD-AVS      TO DAYS-TIDATE1                        
230900        MOVE 'YYMMDD'              TO DAYS-KDDATFMT1                      
231000        MOVE 'YYWWD'               TO DAYS-KDDATFMT2                      
231100        MOVE 0                     TO DAYS-KVDAYS                         
231200        MOVE SPACE                 TO DAYS-TIDATE2                        
231300                                      DAYS-IDCALEND                       
231400        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
231500*                                                                         
231600        IF DAYS-KDRC = 8                                                  
231700          MOVE 'FEL VID ANROP TILL WZ20DAYS 5'                            
231800                                   TO FELTEXT                             
231900          CALL FELLOG                                                     
232000        ELSE                                                              
232100          MOVE DAYS-TIDATE2(1:4)   TO W-DAAVROP-AAVV                      
232200          MOVE DAYS-TIDATE2(5:1)   TO D905-TILEVDAG                       
232300                                      WOL-TILEVDAG                        
232400        END-IF                                                            
232500*                                                                         
232600*FIX NYÅR DATKONVJUST 2014                                                
232700        IF ARB-TIAAMMDD-AVS = 141229                                      
232800           MOVE 1501   TO W-DAAVROP-AAVV                                  
232900           MOVE 1      TO D905-TILEVDAG                                   
233000                          WOL-TILEVDAG                                    
233100        END-IF                                                            
233200        IF ARB-TIAAMMDD-AVS = 141230                                      
233300           MOVE 1501   TO W-DAAVROP-AAVV                                  
233400           MOVE 2      TO D905-TILEVDAG                                   
233500                          WOL-TILEVDAG                                    
233600        END-IF                                                            
233700        IF ARB-TIAAMMDD-AVS = 141231                                      
233800           MOVE 1501   TO W-DAAVROP-AAVV                                  
233900           MOVE 3      TO D905-TILEVDAG                                   
234000                          WOL-TILEVDAG                                    
234100        END-IF                                                            
234200*FIX NYÅR END DATKONVJUST 2014                                            
234300     END-IF                                                               
234400     .                                                                    
234500                                                                          
234600                                                                          
234700                                                                          
234800 S100A-SOEK-NY-AVS-DAG  SECTION.                                          
234900                                                                          
235000     MOVE 'S100A-NY-AVS-DAG '  TO CURRENT-SECTION                         
235100                                                                          
235200*    ITERERA                                                              
235300*       -7 DAGAR                                                          
235400*       UTANFÖR FRYSTID ?                                                 
235500*       HELGDAG ?                                                         
235600     MOVE NEJ                 TO SW-OK SW-FRYS                            
235700     PERFORM UNTIL SW-OK = JA OR SW-FRYS = JA                             
235800        MOVE 003              TO DAG-KDCALL                               
235900        MOVE 20               TO DAG-TISEKEL-TOM                          
236000        MOVE ARB-TIAAMMDD-AVS TO DAG-TIAAMMDD-TOM                         
236100        MOVE 8                TO DAG-KVKALDAG                             
236200        CALL WDAGKONV   USING DAG-KDCALL,                                 
236300                              DAG-DATUM-AREA,                             
236400                              DAG-KDSVAR                                  
236500        IF DAG-KDSVAR = SPACE                                             
236600           MOVE DAG-TIAAMMDD-FOM TO ARB-TIAAMMDD-AVS                      
236700           IF ARB-TIAAMMDD-AVS > WS-FRYSTID AND                           
236800              ARB-TIAAMMDD-AVS >= WS-TIAAMMDD-SPECST                      
236900              MOVE JA         TO SW-OK                                    
237000* KOLLA HELG                                                              
237100                                                                          
237200              MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                         
237300              MOVE 20               TO W-DADATUM-HELG-SS                  
237400              MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD              
237500                                                                          
237600              PERFORM IMS-GU-WDF301                                       
237700              IF SEGMENT-FINNS                                            
237800                MOVE NEJ      TO SW-OK                                    
237900              END-IF                                                      
238000           ELSE                                                           
238100              MOVE JA         TO SW-FRYS                                  
238200           END-IF                                                         
238300        ELSE                                                              
238400           MOVE 'FEL VID ANROP TILL DAGKONV 1'                            
238500                              TO FELTEXT                                  
238600           CALL FELLOG                                                    
238700        END-IF                                                            
238800     END-PERFORM                                                          
238900                                                                          
239000     IF SW-FRYS = JA                                                      
239100        PERFORM S100AA-SOEK-ANNAN-DAG                                     
239200     END-IF                                                               
239300     .                                                                    
239400                                                                          
239500                                                                          
239600                                                                          
239700 S100AA-SOEK-ANNAN-DAG         SECTION.                                   
239800                                                                          
239900     MOVE 'S100AA-ANNAN-DAG '  TO CURRENT-SECTION                         
240000                                                                          
240100*    FINNS WDK722-TILEVDAG NOT = WOL-TILEVDAG                             
240200*ALTERNATIVT                                                              
240300*    FINNS WDF1-TILEVDAG   NOT = WOL-TILEVDAG                             
240400                                                                          
240500     MOVE ZERO TO ARB-TILEVDAG (1)                                        
240600                  ARB-TILEVDAG (2)                                        
240700                  ARB-TILEVDAG (3)                                        
240800                  ARB-TILEVDAG (4)                                        
240900                  ARB-TILEVDAG (5)                                        
241000     IF XLAG-TILEVDAG (1) > ZERO OR                                       
241100        XLAG-TILEVDAG (2) > ZERO OR                                       
241200        XLAG-TILEVDAG (3) > ZERO OR                                       
241300        XLAG-TILEVDAG (4) > ZERO OR                                       
241400        XLAG-TILEVDAG (5) > ZERO                                          
241500        MOVE +1              TO IX-DG                                     
241600        PERFORM UNTIL IX-DG > 5                                           
241700           IF XLAG-TILEVDAG (IX-DG) > ZERO AND                            
241800              XLAG-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                    
241900              MOVE XLAG-TILEVDAG (IX-DG)                                  
242000                             TO ARB-TILEVDAG (IX-DG)                      
242100           END-IF                                                         
242200           ADD +1            TO IX-DG                                     
242300        END-PERFORM                                                       
242400     ELSE                                                                 
242500        MOVE XLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                        
242600        PERFORM IMS-GU-WDF116-SHIP                                        
242700                                                                          
242800        IF SEGMENT-FINNS                                                  
242900          MOVE +1              TO IX-DG                                   
243000          PERFORM UNTIL IX-DG > 5                                         
243100             IF NDC-TILEVDAG (IX-DG) > ZERO AND                           
243200                NDC-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                   
243300                MOVE NDC-TILEVDAG (IX-DG)                                 
243400                               TO ARB-TILEVDAG (IX-DG)                    
243500             END-IF                                                       
243600             ADD +1            TO IX-DG                                   
243700          END-PERFORM                                                     
243800        ELSE                                                              
243900          IF WOL-TILEVDAG = 1                                             
244000            CONTINUE                                                      
244100          ELSE                                                            
244200            MOVE +1            TO ARB-TILEVDAG (1)                        
244300          END-IF                                                          
244400        END-IF                                                            
244500     END-IF                                                               
244600                                                                          
244700     MOVE +1                 TO IX-DG                                     
244800     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
244900        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
245000           PERFORM S100AAA-KOLL-NYTT-DATUM                                
245100        END-IF                                                            
245200        ADD +1               TO IX-DG                                     
245300     END-PERFORM                                                          
245400                                                                          
245500*    ANNARS FÖRSTA LEVDAG NÄSTA VECKA OSV                                 
245600     MOVE WOL-TILEVDAG TO ARB-TILEVDAG (WOL-TILEVDAG)                     
245700     PERFORM UNTIL SW-OK = JA                                             
245800*      STEGA FRAMÅT                                                       
245900       PERFORM S100AAB-KOLL-NASTA-DATUM                                   
246000     END-PERFORM                                                          
246100     .                                                                    
246200                                                                          
246300                                                                          
246400                                                                          
246500 S100AAA-KOLL-NYTT-DATUM       SECTION.                                   
246600                                                                          
246700     MOVE 'S100AAA-NYTT-DATUM'  TO CURRENT-SECTION                        
246800                                                                          
246900*    OM OK => SW-OK = JA                                                  
247000*    KOLL AV ANNAN DAG I URSPRUNGLIG AVSÄNDNINGSVECKA                     
247100                                                                          
247200     COMPUTE WS-DAYS-TIAAVVD-AVS = 10 * WOL-TIAAVV-AVS +                  
247300                                   ARB-TILEVDAG (IX-DG)                   
247400     MOVE WS-DAYS-TIAAVVD-AVS   TO DAYS-TIDATE1                           
247500     MOVE 'YYWWD'               TO DAYS-KDDATFMT1                         
247600     MOVE 'YYMMDD'              TO DAYS-KDDATFMT2                         
247700     MOVE 0                     TO DAYS-KVDAYS                            
247800     MOVE SPACE                 TO DAYS-TIDATE2                           
247900                                   DAYS-IDCALEND                          
248000     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
248100*                                                                         
248200     IF DAYS-KDRC = 8                                                     
248300       MOVE 'FEL VID ANROP TILL WZ20DAYS 6' TO FELTEXT                    
248400       CALL FELLOG                                                        
248500     ELSE                                                                 
248600       MOVE DAYS-TIDATE2(1:6)   TO ARB-TIAAMMDD-AVS                       
248700     END-IF                                                               
248800*                                                                         
248900*FIX NYÅR START   DATKONVJUST 2014                                        
249000     COMPUTE FIX-AAVVD  = 10 * WOL-TIAAVV-AVS +                           
249100                          ARB-TILEVDAG (IX-DG)                            
249200     IF FIX-AAVVD = 15011                                                 
249300        MOVE 141229  TO ARB-TIAAMMDD-AVS                                  
249400     END-IF                                                               
249500     IF FIX-AAVVD = 15012                                                 
249600        MOVE 141230  TO ARB-TIAAMMDD-AVS                                  
249700     END-IF                                                               
249800     IF FIX-AAVVD = 15013                                                 
249900        MOVE 141231  TO ARB-TIAAMMDD-AVS                                  
250000     END-IF                                                               
250100*FIX NYÅR END DATKONVJUST 2014                                            
250200                                                                          
250300     MOVE WS-IDLANDX2-SHIP TO W-IDLANDX2                                  
250400     MOVE 20               TO W-DADATUM-HELG-SS                           
250500     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
250600     PERFORM IMS-GU-WDF301                                                
250700                                                                          
250800     IF SEGMENT-FINNS                                                     
250900       CONTINUE                                                           
251000     ELSE                                                                 
251100       MOVE JA TO SW-OK                                                   
251200     END-IF                                                               
251300     .                                                                    
251400     EJECT                                                                
251500                                                                          
251600                                                                          
251700 S100AAB-KOLL-NASTA-DATUM      SECTION.                                   
251800                                                                          
251900     MOVE 'S100AAB-NASTA-DAT'  TO CURRENT-SECTION                         
252000                                                                          
252100*       OM OK => SW-OK = JA                                               
252200**   ÖKA VECKA MED 1                                                      
252300**   SÖK AVS-DAGAR I VECKAN                                               
252400                                                                          
252500     MOVE WOL-TIAAVV-AVS  TO DATUM-AAVV                                   
252600     MOVE 1               TO W-ANTAL-VECKOR                               
252700     CALL W009VADD USING DATUM-AAVV W-ANTAL-VECKOR                        
252800     MOVE DATUM-AAVV      TO WOL-TIAAVV-AVS                               
252900                                                                          
253000     MOVE +1                 TO IX-DG                                     
253100     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
253200        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
253300           PERFORM S100AAA-KOLL-NYTT-DATUM                                
253400        END-IF                                                            
253500        ADD +1               TO IX-DG                                     
253600     END-PERFORM                                                          
253700     .                                                                    
253800                                                                          
253900                                                                          
254000                                                                          
254100 S101-BERAEKNA-INL-DISP-AAMMDD SECTION.                                   
254200                                                                          
254300     MOVE 'S101-BER-INL-DISP'  TO CURRENT-SECTION                         
254400                                                                          
254500*INL                                                                      
254600     MOVE XLAG-IDLEVNR-SHIP        TO W-IDLEVNR-SHIP                      
254700     PERFORM IMS-GU-WDF116-SHIP                                           
254800     IF SEGMENT-SAKNAS                                                    
254900        MOVE ZERO                  TO NDC-KVDAGAR-TT                      
255000     END-IF                                                               
255100                                                                          
255200     MOVE 2                        TO WORK-KDCALL                         
255300     MOVE OMSP-IDDC                TO WORK-IDDC                           
255400     MOVE W-TIAAMMDD-AVS           TO WORK-TIAAMMDD-FOM                   
255500     MOVE NDC-KVDAGAR-TT           TO WORK-KVWORKD                        
255600     ADD +1                        TO WORK-KVWORKD                        
255700     CALL WORKDAY USING       WORK-KDCALL                                 
255800          WORK-DATE-AREA WORK-KDSVAR                                      
255900     MOVE WORK-TIAAMMDD-TOM        TO D905-TIAVRDAT-INL                   
256000*DISP                                                                     
256100     MOVE 2                        TO WORK-KDCALL                         
256200     MOVE OMSP-IDDC                TO WORK-IDDC                           
256300     MOVE D905-TIAVRDAT-INL        TO WORK-TIAAMMDD-FOM                   
256400     MOVE W-SPAR-LART-KVDAGAR-INLEV    TO WORK-KVWORKD                    
256500     ADD +1                        TO WORK-KVWORKD                        
256600     CALL WORKDAY USING       WORK-KDCALL                                 
256700          WORK-DATE-AREA WORK-KDSVAR                                      
256800     MOVE WORK-TIAAMMDD-TOM        TO D905-TIAVRDAT-DISP                  
256900     .                                                                    
257000                                                                          
257100                                                                          
257200                                                                          
257300 S102-BERAKNA-VECKODIFFERENS SECTION.                                     
257400     MOVE 'S102-BER-V-DIFF ' TO CURRENT-SECTION                           
257500                                                                          
257600     MOVE ZERO            TO W-VECKO-DIFFERENS                            
257700                                                                          
257800     PERFORM UNTIL W-DATUM-FROM = W-DATUM-TOM                             
257900        IF W-DATUM-FROM-AA = W-DATUM-TOM-AA                               
258000           COMPUTE W-VECKO-DIFFERENS =                                    
258100                   W-VECKO-DIFFERENS + W-DATUM-TOM - W-DATUM-FROM         
258200           MOVE W-DATUM-FROM TO W-DATUM-TOM                               
258300        ELSE                                                              
258400           MOVE W-DATUM-FROM TO W-AAVV-JUST                               
258500           MOVE 53           TO W-AAVV-JUST-VV                            
258600           MOVE 'AAVV'       TO DAT-KDDATFORM                             
258700           MOVE W-AAVV-JUST  TO DAT-I-TIDATUM                             
258800           CALL WDATKONV USING  DAT-KDDATFORM                             
258900                                DAT-I-TIDATUM                             
259000                                DAT-O-TIDATUM                             
259100                                DAT-KDSVAR                                
259200           IF DAT-KDSVAR-OK                                               
259300* ÅRET HAR 53 VECKOR                                                      
259400              COMPUTE W-VECKO-DIFFERENS =                                 
259500                      W-VECKO-DIFFERENS + (54 - W-DATUM-FROM-VV)          
259600              COMPUTE W-DATUM-FROM-AA = W-DATUM-FROM-AA + 1               
259700              MOVE 1         TO W-DATUM-FROM-VV                           
259800           ELSE                                                           
259900* ÅRET HAR 52 VECKOR                                                      
260000              COMPUTE W-VECKO-DIFFERENS =                                 
260100                      W-VECKO-DIFFERENS + (53 - W-DATUM-FROM-VV)          
260200              COMPUTE W-DATUM-FROM-AA = W-DATUM-FROM-AA + 1               
260300              MOVE 1         TO W-DATUM-FROM-VV                           
260400           END-IF                                                         
260500        END-IF                                                            
260600     END-PERFORM                                                          
260700     IF W-VECKO-DIFFERENS > TILLGTAB-MAX                                  
260800       MOVE TILLGTAB-MAX     TO W-VECKO-DIFFERENS                         
260900     END-IF                                                               
261000     .                                                                    
261100                                                                          
261200                                                                          
261300 IMS-GU-WDD901 SECTION.                                                   
261400                                                                          
261500     MOVE 'IMS-GU-WDD901   ' TO CURRENT-IMS-SECTION                       
261600                                                                          
261700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
261800          DELIMITED BY SIZE INTO SSA1                                     
261900     MOVE '    ' TO GODK-STATUSKODER                                      
262000     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
262100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
262200     PERFORM IMS-STATUSKONTROLL                                           
262300     .                                                                    
262400                                                                          
262500 IMS-GU-WDD902 SECTION.                                                   
262600                                                                          
262700     MOVE 'IMS-GU-WDD902   '    TO CURRENT-IMS-SECTION                    
262800                                                                          
262900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
263000          DELIMITED BY SIZE   INTO SSA1                                   
263100     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
263200          DELIMITED BY SIZE   INTO SSA2                                   
263300     MOVE '  GE'                TO GODK-STATUSKODER                       
263400     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
263500     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
263600     PERFORM IMS-STATUSKONTROLL                                           
263700     .                                                                    
263800                                                                          
263900 IMS-GHU-WDD904 SECTION.                                                  
264000                                                                          
264100     MOVE 'IMS-GHU-WDD904  '    TO CURRENT-IMS-SECTION                    
264200                                                                          
264300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
264400          DELIMITED BY SIZE   INTO SSA1                                   
264500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
264600          DELIMITED BY SIZE   INTO SSA2                                   
264700     MOVE 'WDD904 '             TO SSA3                                   
264800     MOVE '  GE'                TO GODK-STATUSKODER                       
264900     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
265000     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
265100     PERFORM IMS-STATUSKONTROLL                                           
265200     .                                                                    
265300                                                                          
265400 IMS-DLET-WDD904 SECTION.                                                 
265500                                                                          
265600     MOVE 'IMS-DLET-WDD904 '    TO CURRENT-IMS-SECTION                    
265700                                                                          
265800     MOVE '    '                TO GODK-STATUSKODER                       
265900     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
266000     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
266100     PERFORM IMS-STATUSKONTROLL                                           
266200     .                                                                    
266300                                                                          
266400 IMS-ISRT-WDD904 SECTION.                                                 
266500                                                                          
266600     MOVE 'IMS-ISRT-WDD904 '    TO CURRENT-IMS-SECTION                    
266700                                                                          
266800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
266900          DELIMITED BY SIZE INTO SSA1                                     
267000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
267100          DELIMITED BY SIZE INTO SSA2                                     
267200     MOVE 'WDD904   '         TO SSA3                                     
267300     MOVE '  II'              TO GODK-STATUSKODER                         
267400     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3        
267500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
267600     PERFORM IMS-STATUSKONTROLL                                           
267700     .                                                                    
267800     EJECT                                                                
267900                                                                          
268000 IMS-GHU-WDD905 SECTION.                                                  
268100                                                                          
268200     MOVE 'IMS-GHU-WDD905  '    TO CURRENT-IMS-SECTION                    
268300                                                                          
268400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
268500          DELIMITED BY SIZE   INTO SSA1                                   
268600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
268700          DELIMITED BY SIZE   INTO SSA2                                   
268800     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
268900                    '&KDAVROP  =' W-KDAVROP-X ')'                         
269000          DELIMITED BY SIZE   INTO SSA3                                   
269100     MOVE '  GE'                TO GODK-STATUSKODER                       
269200     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD905-2                     
269300                                      SSA1 SSA2 SSA3                      
269400     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
269500     PERFORM IMS-STATUSKONTROLL                                           
269600     .                                                                    
269700                                                                          
269800 IMS-GHNP-WDD905 SECTION.                                                 
269900                                                                          
270000     MOVE 'IMS-GHNP-WDD905 '    TO CURRENT-IMS-SECTION                    
270100                                                                          
270200     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
270300          DELIMITED BY SIZE   INTO SSA1                                   
270400     MOVE '  GE'                TO GODK-STATUSKODER                       
270500     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
270600     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
270700     PERFORM IMS-STATUSKONTROLL                                           
270800     .                                                                    
270900     EJECT                                                                
271000 IMS-GNP-WDD905-FIRST SECTION.                                            
271100                                                                          
271200     MOVE 'IMS-GNP-WDD905-FIRST '    TO CURRENT-IMS-SECTION               
271300                                                                          
271400     MOVE 'WDD902  *F'        TO SSA1                                     
271500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
271600          DELIMITED BY SIZE   INTO SSA2                                   
271700     MOVE '  GE'                TO GODK-STATUSKODER                       
271800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
271900     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
272000     PERFORM IMS-STATUSKONTROLL                                           
272100     .                                                                    
272200     EJECT                                                                
272300 IMS-GNP-WDD905-NEXT SECTION.                                             
272400                                                                          
272500     MOVE 'IMS-GNP-WDD905-NEXT '    TO CURRENT-IMS-SECTION                
272600                                                                          
272700     MOVE 'WDD902  '          TO SSA1                                     
272800     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
272900          DELIMITED BY SIZE   INTO SSA2                                   
273000     MOVE '  GE'                TO GODK-STATUSKODER                       
273100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
273200     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
273300     PERFORM IMS-STATUSKONTROLL                                           
273400     .                                                                    
273500     EJECT                                                                
273600 IMS-ISRT-WDD905 SECTION.                                                 
273700                                                                          
273800     MOVE 'IMS-ISRT-WDD905 '    TO CURRENT-IMS-SECTION                    
273900                                                                          
274000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
274100          DELIMITED BY SIZE   INTO SSA1                                   
274200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
274300          DELIMITED BY SIZE   INTO SSA2                                   
274400     MOVE   'WDD905 '           TO SSA3                                   
274500     MOVE '  GE'                TO GODK-STATUSKODER                       
274600     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
274700     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
274800     PERFORM IMS-STATUSKONTROLL                                           
274900     .                                                                    
275000                                                                          
275100 IMS-REPL-WDD905 SECTION.                                                 
275200                                                                          
275300     MOVE 'IMS-REPL-WDD905 '    TO CURRENT-IMS-SECTION                    
275400                                                                          
275500     MOVE '    '                TO GODK-STATUSKODER                       
275600     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
275700     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
275800     PERFORM IMS-STATUSKONTROLL                                           
275900     .                                                                    
276000                                                                          
276100 IMS-DLET-WDD905 SECTION.                                                 
276200                                                                          
276300     MOVE 'IMS-DLET-WDD905 '    TO CURRENT-IMS-SECTION                    
276400                                                                          
276500     MOVE '    '                TO GODK-STATUSKODER                       
276600     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
276700     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
276800     PERFORM IMS-STATUSKONTROLL                                           
276900     .                                                                    
277000                                                                          
277100 IMS-GU-WDK601   SECTION.                                                 
277200                                                                          
277300     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
277400                                                                          
277500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
277600          DELIMITED BY SIZE INTO SSA1                                     
277700     MOVE '    ' TO GODK-STATUSKODER                                      
277800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
277900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
278000     PERFORM IMS-STATUSKONTROLL                                           
278100     .                                                                    
278200     EJECT                                                                
278300                                                                          
278400 IMS-GU-WDK611   SECTION.                                                 
278500                                                                          
278600     MOVE 'IMS-GU-WDK611   ' TO CURRENT-IMS-SECTION                       
278700                                                                          
278800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
278900          DELIMITED BY SIZE INTO SSA1                                     
279000     MOVE   'WDK611 '         TO SSA2                                     
279100     MOVE '    ' TO GODK-STATUSKODER                                      
279200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
279300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
279400     PERFORM IMS-STATUSKONTROLL                                           
279500     .                                                                    
279600     EJECT                                                                
279700                                                                          
279800 IMS-GU-WDK701   SECTION.                                                 
279900                                                                          
280000     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
280100                                                                          
280200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
280300          DELIMITED BY SIZE INTO SSA1                                     
280400     MOVE '    ' TO GODK-STATUSKODER                                      
280500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
280600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
280700     PERFORM IMS-STATUSKONTROLL                                           
280800     .                                                                    
280900     EJECT                                                                
281000                                                                          
281100 IMS-GNP-WDK711-REF   SECTION.                                            
281200                                                                          
281300     MOVE 'IMS-GNP-WDK711-REF  ' TO CURRENT-IMS-SECTION                   
281400                                                                          
281500     STRING 'WDK711  (IDDCREF  =' W-IDDC-X ')'                            
281600          DELIMITED BY SIZE INTO SSA1                                     
281700     MOVE '  GE' TO GODK-STATUSKODER                                      
281800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
281900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
282000     PERFORM IMS-STATUSKONTROLL                                           
282100     .                                                                    
282200     EJECT                                                                
282300                                                                          
282400 IMS-GU-WDK711   SECTION.                                                 
282500                                                                          
282600     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
282700                                                                          
282800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
282900          DELIMITED BY SIZE INTO SSA1                                     
283000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
283100          DELIMITED BY SIZE INTO SSA2                                     
283200     MOVE '    ' TO GODK-STATUSKODER                                      
283300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
283400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
283500     PERFORM IMS-STATUSKONTROLL                                           
283600     .                                                                    
283700     EJECT                                                                
283800                                                                          
283900 IMS-GHU-WDK711   SECTION.                                                
284000                                                                          
284100     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
284200                                                                          
284300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
284400          DELIMITED BY SIZE INTO SSA1                                     
284500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
284600          DELIMITED BY SIZE INTO SSA2                                     
284700     MOVE '    ' TO GODK-STATUSKODER                                      
284800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
284900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
285000     PERFORM IMS-STATUSKONTROLL                                           
285100     .                                                                    
285200     EJECT                                                                
285300                                                                          
285400 IMS-REPL-WDK711        SECTION.                                          
285500                                                                          
285600     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
285700                                                                          
285800     MOVE '  '                TO GODK-STATUSKODER                         
285900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
286000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
286100     PERFORM IMS-STATUSKONTROLL                                           
286200     .                                                                    
286300     EJECT                                                                
286400 IMS-GU-WDK712 SECTION.                                                   
286500                                                                          
286600     MOVE 'IMS-GU-WDK712   ' TO CURRENT-IMS-SECTION                       
286700                                                                          
286800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
286900          DELIMITED BY SIZE INTO SSA1                                     
287000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
287100          DELIMITED BY SIZE INTO SSA2                                     
287200     MOVE '  GE'              TO GODK-STATUSKODER                         
287300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
287400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
287500     PERFORM IMS-STATUSKONTROLL                                           
287600     .                                                                    
287700     EJECT                                                                
287800                                                                          
287900 IMS-GU-WDK722 SECTION.                                                   
288000                                                                          
288100     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
288200                                                                          
288300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
288400          DELIMITED BY SIZE INTO SSA1                                     
288500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
288600          DELIMITED BY SIZE INTO SSA2                                     
288700     MOVE 'WDK722 '           TO SSA3                                     
288800     MOVE '    '              TO GODK-STATUSKODER                         
288900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
289000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
289100     PERFORM IMS-STATUSKONTROLL                                           
289200     .                                                                    
289300     EJECT                                                                
289400                                                                          
289500 IMS-GHU-WDK722 SECTION.                                                  
289600                                                                          
289700     MOVE 'IMS-GHU-WDK722  ' TO CURRENT-IMS-SECTION                       
289800                                                                          
289900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
290000          DELIMITED BY SIZE INTO SSA1                                     
290100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
290200          DELIMITED BY SIZE INTO SSA2                                     
290300     MOVE 'WDK722 '           TO SSA3                                     
290400     MOVE '    '              TO GODK-STATUSKODER                         
290500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
290600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
290700     PERFORM IMS-STATUSKONTROLL                                           
290800     .                                                                    
290900     EJECT                                                                
291000 IMS-REPL-WDK722 SECTION.                                                 
291100                                                                          
291200     MOVE 'IMS-REPL-WDK722 '    TO CURRENT-IMS-SECTION                    
291300                                                                          
291400     MOVE '    '                TO GODK-STATUSKODER                       
291500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
291600     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
291700     PERFORM IMS-STATUSKONTROLL                                           
291800     .                                                                    
291900     EJECT                                                                
292000 IMS-GNP-WDK724 SECTION.                                                  
292100                                                                          
292200     MOVE 'IMS-GNP-WDK724  ' TO CURRENT-IMS-SECTION                       
292300                                                                          
292400     MOVE 'WDK724 '           TO SSA1                                     
292500     MOVE '  GE'              TO GODK-STATUSKODER                         
292600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
292700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
292800     PERFORM IMS-STATUSKONTROLL                                           
292900     .                                                                    
293000     EJECT                                                                
293100                                                                          
293200 IMS-GU-WDF106  SECTION.                                                  
293300                                                                          
293400     MOVE 'IMS-GU-WDF106   '  TO CURRENT-IMS-SECTION                      
293500                                                                          
293600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
293700          DELIMITED BY SIZE INTO SSA1                                     
293800     STRING 'WDF106     '                                                 
293900          DELIMITED BY SIZE INTO SSA2                                     
294000     MOVE '  GE'              TO GODK-STATUSKODER                         
294100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF106 SSA1 SSA2               
294200     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
294300     PERFORM IMS-STATUSKONTROLL                                           
294400     .                                                                    
294500     EJECT                                                                
294600                                                                          
294700 IMS-GU-WDF116  SECTION.                                                  
294800                                                                          
294900     MOVE 'IMS-GU-WDF116   '  TO CURRENT-IMS-SECTION                      
295000                                                                          
295100     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
295200          DELIMITED BY SIZE INTO SSA1                                     
295300     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
295400          DELIMITED BY SIZE INTO SSA2                                     
295500     MOVE '  GE'              TO GODK-STATUSKODER                         
295600     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
295700     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
295800     PERFORM IMS-STATUSKONTROLL                                           
295900     .                                                                    
296000     EJECT                                                                
296100                                                                          
296200 IMS-GU-WDF116-SHIP  SECTION.                                             
296300                                                                          
296400     MOVE 'IMS-GU-WDF116-SHIP '  TO CURRENT-IMS-SECTION                   
296500                                                                          
296600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
296700          DELIMITED BY SIZE INTO SSA1                                     
296800     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
296900          DELIMITED BY SIZE INTO SSA2                                     
297000     MOVE '  GE'              TO GODK-STATUSKODER                         
297100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
297200     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
297300     PERFORM IMS-STATUSKONTROLL                                           
297400     .                                                                    
297500     EJECT                                                                
297600                                                                          
297700 IMS-GU-WDF301  SECTION.                                                  
297800                                                                          
297900     MOVE 'IMS-GU-WDF301   '  TO CURRENT-IMS-SECTION                      
298000                                                                          
298100     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
298200          DELIMITED BY SIZE INTO SSA1                                     
298300     MOVE '  GE'              TO GODK-STATUSKODER                         
298400     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-WDF301 SSA1                    
298500     MOVE WDF3-STATUS-CODE    TO STATUS-WS                                
298600     PERFORM IMS-STATUSKONTROLL                                           
298700     .                                                                    
298800     EJECT                                                                
298900                                                                          
299000 IMS-GU-WDB601 SECTION.                                                   
299100                                                                          
299200     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
299300                                                                          
299400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
299500          DELIMITED BY SIZE INTO SSA1                                     
299600     MOVE '    '              TO GODK-STATUSKODER                         
299700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
299800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
299900     PERFORM IMS-STATUSKONTROLL                                           
300000     .                                                                    
300100     EJECT                                                                
300200                                                                          
300300 IMS-GHU-WDD601 SECTION.                                                  
300400                                                                          
300500     MOVE 'IMS-GHU-WDD601  ' TO CURRENT-IMS-SECTION                       
300600                                                                          
300700     STRING 'WDD601  (WDD601KY =' W-WDD601KY-X ')'                        
300800          DELIMITED BY SIZE INTO SSA1                                     
300900     MOVE '  GE'              TO GODK-STATUSKODER                         
301000     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
301100     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
301200     PERFORM IMS-STATUSKONTROLL                                           
301300     .                                                                    
301400     EJECT                                                                
301500 IMS-GHU-WDD601-FLERA SECTION.                                            
301600                                                                          
301700     MOVE 'IMS-GHU-WDD601-FLERA  ' TO CURRENT-IMS-SECTION                 
301800                                                                          
301900      STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                       
302000                     '&WDD601KY<=' W-WDD601KY-MAX-X                       
302100                     '&IDANSK  NE' W-IDANSK-NON-X ')'                     
302200          DELIMITED BY SIZE INTO SSA1                                     
302300     MOVE '  GE'              TO GODK-STATUSKODER                         
302400     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
302500     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
302600     PERFORM IMS-STATUSKONTROLL                                           
302700     .                                                                    
302800     EJECT                                                                
302900 IMS-GHN-WDD601-FLERA SECTION.                                            
303000                                                                          
303100     MOVE 'IMS-GHN-WDD601-FLERA  ' TO CURRENT-IMS-SECTION                 
303200                                                                          
303300      STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                       
303400                     '&WDD601KY<=' W-WDD601KY-MAX-X                       
303500                     '&IDANSK  NE' W-IDANSK-NON-X ')'                     
303600          DELIMITED BY SIZE INTO SSA1                                     
303700     MOVE '  GEGB'            TO GODK-STATUSKODER                         
303800     CALL CBLTDLI USING GHN WDD6-PCB DLI-IO-WDD601 SSA1                   
303900     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
304000     PERFORM IMS-STATUSKONTROLL                                           
304100     .                                                                    
304200     EJECT                                                                
304300 IMS-REPL-WDD601        SECTION.                                          
304400                                                                          
304500     MOVE 'IMS-REPL-WDD601 ' TO CURRENT-IMS-SECTION                       
304600                                                                          
304700     MOVE '  '                TO GODK-STATUSKODER                         
304800     CALL CBLTDLI USING REPL WDD6-PCB DLI-IO-WDD601                       
304900     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
305000     PERFORM IMS-STATUSKONTROLL                                           
305100     .                                                                    
305200     EJECT                                                                
305300 IMS-ISRT-WDD601        SECTION.                                          
305400                                                                          
305500     MOVE 'IMS-ISRT-WDD601 ' TO CURRENT-IMS-SECTION                       
305600                                                                          
305700     MOVE 'WDD601   '       TO SSA1                                       
305800     MOVE '  '              TO GODK-STATUSKODER                           
305900     CALL CBLTDLI USING ISRT WDD6-PCB DLI-IO-WDD601  SSA1                 
306000     MOVE WDD6-STATUS-CODE  TO STATUS-WS                                  
306100     PERFORM IMS-STATUSKONTROLL                                           
306200     .                                                                    
306300     EJECT                                                                
306400 IMS-DLET-WDD601        SECTION.                                          
306500                                                                          
306600     MOVE 'IMS-DLET-WDD601 ' TO CURRENT-IMS-SECTION                       
306700                                                                          
306800     MOVE '  '                TO GODK-STATUSKODER                         
306900     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
307000     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
307100     PERFORM IMS-STATUSKONTROLL                                           
307200     .                                                                    
307300     EJECT                                                                
307400                                                                          
307500 IMS-GU-WDD311-BSEQ       SECTION.                                        
307600     MOVE 'GU-WDD311-BSEQ  ' TO CURRENT-IMS-SECTION                       
307700                                                                          
307800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
307900          DELIMITED BY SIZE         INTO SSA1                             
308000     MOVE   'WDD311  (IDSKYLT  =GB )' TO SSA2                             
308100     MOVE '  GE'                      TO GODK-STATUSKODER                 
308200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
308300     MOVE WDD3-STATUS-CODE            TO STATUS-WS                        
308400     PERFORM IMS-STATUSKONTROLL                                           
308500     .                                                                    
308600     EJECT                                                                
308700                                                                          
308800                                                                          
308900 IMS-GU-WDD704 SECTION.                                                   
309000     MOVE 'IMS-GU-WDD704   ' TO CURRENT-IMS-SECTION                       
309100                                                                          
309200     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
309300          DELIMITED BY SIZE INTO SSA1                                     
309400     MOVE 'WDD704 '           TO SSA2                                     
309500     MOVE '  GE'              TO GODK-STATUSKODER                         
309600     CALL CBLTDLI USING GU  WDD7-PCB DLI-IO-WDD704 SSA1 SSA2              
309700     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
309800     PERFORM IMS-STATUSKONTROLL                                           
309900     .                                                                    
310000                                                                          
310100 IMS-STATUSKONTROLL SECTION.                                              
310200     SET STATUS-IX TO 1                                                   
310300     SEARCH GODK-STATUS AT END                                            
310400     CALL FELLOG                                                          
310500     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
310600     CONTINUE                                                             
310700     END-SEARCH                                                           
310800     .                                                                    
310900     EJECT                                                                
311000*    -COPY WY2000P2                                                       
311100     EJECT                                                                
311200*    -COPY WY2000P9                                                       
311300     EJECT                                                                
311400*    -COPY WY2000P3                                                       
