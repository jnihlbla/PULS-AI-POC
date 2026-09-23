000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2241400.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/02/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        OMSPEC AV LEVERANSPLAN NDC:ER KINA LOCAL SOURCING                
000900*                               NDC:ER USA                                
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK7                                       
001200*        PROGRAMMET LÄSER      WDL6                                       
001300*        PROGRAMMET LÄSER      WDR2                                       
001400*        PROGRAMMET LÄSER      WDF1                                       
001500*        PROGRAMMET LÄSER      WDF3                                       
001600*        PROGRAMMET LÄSER      WDB6                                       
001700*        PROGRAMMET LÄSER      WDD3                                       
001800*        PROGRAMMET LÄSER      WDD6                                       
001900*        PROGRAMMET LÄSER      WDD7                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500*                                                                         
002600****-------------------------------------------------------------         
002700*--- PROGRAMÄNDRINGAR                                                     
002800****-------------------------------------------------------------         
002900*                                                                         
003000* 2014-05-05  E'TRACKER 10230472  RÄTTNING AV BUGG VID                    
003100*                                 HELGDAGSJUSTERING.                      
003200*                                                                         
003300* 2014-11-20  E'TRACKER 10245983  DATKONVJUST JUL/NYÅR 2014 CN            
003400*                                 SE PGM W2215000 FIX-CDC                 
003500*                                                                         
003600* 2014-12-09  E'TRACKER 10243650  TAG BORT TOMMA FÖRSLAG PÅ ART.          
003700*                                 SOM SAKNAR GÄLLANDE PLAN.2447           
003800*                                                                         
003900* 2015-09-11  E'TRACKER 10130993                                          
004000*             REDUCE NUMBER OF DELIVERY SCHEDULES                         
004100*                                                                         
004200* 2015-11-13  E'TRACKER 10243132  KINA EXPORT 2015                        
004300*                                                                         
004400* 2016-10-21  E'TRACKER 10287369  Rätta regler för lev.planer som         
004500*                                 sparas.                                 
004600*                                                                         
004700* 2017-04-11  E'TRACKER 10292048  Lägg till kontroll även vid             
004800*                                 tomma förslag.SW-skip-forslag           
004900*                                                                         
005000* 2017-09-11  E'TRACKER 10299286  LOCAL SOURCING USA                      
005100*                                 CCID: 10302687 (ANSK + REFILL)          
005200*                                                                         
005300                                                                          
005400 ENVIRONMENT DIVISION.                                                    
005500 INPUT-OUTPUT SECTION.                                                    
005600                                                                          
005700 FILE-CONTROL.                                                            
005800     SKIP2                                                                
005900*          --- OMSPEC LEVERANSPLAN                                        
006000     SELECT W22412                     ASSIGN TO W22414D1.                
006100                                                                          
006200*          --- AUTOMATGODKÄNDA LEVERANSPLANER                             
006300     SELECT W22414                     ASSIGN TO W22414D2.                
006400                                                                          
006500*          --- UPPDATERINGSPOSTER OMSPEC. LEV.PLAN                        
006600     SELECT W22415                     ASSIGN TO W22414D3.                
006700                                                                          
006800*          --- LEVERANSPLANEFÖRSLAG                                       
006900     SELECT W22416                     ASSIGN TO W22414D4.                
007000                                                                          
007100                                                                          
007200 DATA DIVISION.                                                           
007300 FILE SECTION.                                                            
007400                                                                          
007500 FD  W22412                                                               
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800                                                                          
007900*01  -COPY W224LI12      -L.                                              
008000                                                                          
008100                                                                          
008200 FD  W22414                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500                                                                          
008600*01  POST -COPY W22454 -PRE  AUTO-  -L.                                   
008700                                                                          
008800                                                                          
008900 FD  W22415                                                               
009000     RECORDING       F                                                    
009100     BLOCK CONTAINS  0.                                                   
009200                                                                          
009300*01  POST -COPY W22415 -PRE  UPLP-  -L.                                   
009400                                                                          
009500                                                                          
009600 FD  W22416                                                               
009700     RECORDING       F                                                    
009800     BLOCK CONTAINS  0.                                                   
009900                                                                          
010000*01  LPF-POST -COPY WDD601    -L.                                         
010100                                                                          
010200                                                                          
010300                                                                          
010400 WORKING-STORAGE SECTION.                                                 
010500     SKIP2                                                                
010600*    -COPY WY2000W3                                                       
010700     SKIP3                                                                
010800*    -COPY WY2000W1                                                       
010900     SKIP3                                                                
011000                                                                          
011100 77  IDPGM                       PIC X(8)    VALUE 'W2241400'.            
011200 77  JA                          PIC X       VALUE 'J'.                   
011300 77  NEJ                         PIC X       VALUE 'N'.                   
011400                                                                          
011500 77  W-KDLPORS          PIC S9(3)  VALUE ZERO COMP-3.                     
011600     88  W-SPARA-FORSLAG VALUE 01 02 04 08 11 14 15 16                    
011700                               19 20 21 23 26 27 29.                      
011800                                                                          
011900 01  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
012000 01  CURRENT-S-SECTION           PIC X(16)   VALUE SPACE.                 
012100 01  CURRENT-IMS-SECTION         PIC X(32)   VALUE SPACE.                 
012200                                                                          
012300 77  W22412-EOF-SW               PIC X       VALUE 'N'.                   
012400     88  END-OF-W22412                       VALUE 'J'.                   
012500                                                                          
012600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012700 01  FILLER REDEFINES DAGENS-DATUM.                                       
012800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013100                                                                          
013200                                                                          
013300 01  W-DAINLEV-DATUM2            PIC 9(16)   VALUE ZERO.                  
013400 01  FILLER REDEFINES W-DAINLEV-DATUM2.                                   
013500     03  W-DAINLEV-SS            PIC 9(2).                                
013600     03  W-DAINLEV-AAMMDD        PIC 9(6).                                
013700     03  W-DAINLEV-HHMMSSTH      PIC 9(8).                                
013800                                                                          
013900 01  W-TIERSDAT            PIC  9(5)  VALUE ZERO.                         
014000 01  FILLER REDEFINES W-TIERSDAT.                                         
014100     03  W-TIERSDAT-AAVV   PIC 9(4).                                      
014200     03  FILLER            PIC 9(1).                                      
014300                                                                          
014400                                                                          
014500 01  DYNAMISKA-SUBPROGRAM.                                                
014600*                                                                         
014700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
015100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015200     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
015300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
015400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
015500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
015600     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015700     03  W221LPAD                PIC X(8)    VALUE 'W221LPAD'.            
015800     03  W222BHDC                PIC X(8)    VALUE 'W222BHDC'.            
015900     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
016000                                                                          
016100*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
016200 01  W-W221LP-CTX                PIC X(08) VALUE 'W221LP02'.              
016300 01  W-KDLPORS-GRP.                                                       
016400     03 W-KDLPORS-TAB OCCURS 4   PIC 9(3).                                
016500                                                                          
016600*    --- PARAMETRAR TILL ABEND                                            
016700                                                                          
016800 77  RKOD                        PIC S9(4)   VALUE +0  COMP SYNC.         
016900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017200                                                                          
017300 01  FELTEXT.                                                             
017400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017600                                                                          
017700                                                                          
017800*    --- PARAMETRAR TILL DATKORT                                          
017900                                                                          
018000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22414'.              
018100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
018200                                                                          
018300*    -COPY WDATKORT                                                       
018400                                                                          
018500                                                                          
018600*    --- PARAMETRAR TILL WDATKONV                                         
018700                                                                          
018800*01      -COPY WDATAREA.                                                  
018900                                                                          
019000                                                                          
019100*                            *** PARAMETRAR TILL WDAGKONV '               
019200*01  -COPY WDAGAREA.                                                      
019300                                                                          
019400                                                                          
019500*                            *** PARAMETRAR TILL WORKDAY  '               
019600*01  -COPY WORKAREA                                                       
019700                                                                          
019800 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
019900*   -COPY WZ20DAYS                                                        
020000     EJECT                                                                
020100                                                                          
020200*    --- PARAMETRAR TILL POSTSUM                                          
020300                                                                          
020400*01  -COPY W0005   -PRE  POSTSUM-                                         
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)   VALUE 'WWDC99 '.             
020700*01  -COPY WWDC99                                                         
020800                                                                          
020900     EJECT                                                                
021000*    --- LÄNKAREA TILL W222BHDC                                           
021100*01  -COPY W222BHDC -PRE BHDC-                                            
021200 01  PB-TOTAL-SEP-LEV-XDC        PIC X(2)   VALUE '03'.                   
021300 01  XDC-CDC-BEHOV               PIC X(2)   VALUE '05'.                   
021400                                                                          
021500*    --- TAB2-AREA BEHOVSTABELL                                           
021600*01  AREA  -COPY W222BHDC   -PRE TAB2-.                                   
021700                                                                          
021800                                                                          
021900                                                                          
022000*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
022100                                                                          
022200 01  TABENTRY-PARM.                                                       
022300     03  STEGLANGD               PIC S9(9) COMP.                          
022400     03  ANTAL                   PIC S9(9) COMP.                          
022500     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
022600                                                                          
022700                                                                          
022800                                                                          
022900 01  OMSP-AREA-START             PIC X(24)   VALUE                        
023000                                 'OMSP-AREA-START  '.                     
023100*01  AREA -COPY W224LI12   -PRE OMSP-                                     
023200                                                                          
023300                                                                          
023400 01  AUTO-AREA-START             PIC X(24)   VALUE                        
023500                                 'AUTO-AREA-START  '.                     
023600*01  AREA -COPY W22454     -PRE AUTO-                                     
023700                                                                          
023800                                                                          
023900 01  UPLP-AREA-START             PIC X(24)   VALUE                        
024000                                 'UPLP-AREA-START  '.                     
024100*01  UPLP-AREA -COPY W22415                                               
024200                                                                          
024300                                                                          
024400 01  LPF-AREA-START              PIC X(24)   VALUE                        
024500                                 'LPF-AREA-START   '.                     
024600*01  LPF-AREA -COPY WDD601                                                
024700                                                                          
024800                                                                          
024900 01  IX-BEHOV                    PIC  9(3)          VALUE ZERO.           
025000 01  IX-BEHOV-MAX                PIC  9(3)          VALUE 156.            
025100 01  IX-ORS                      PIC  9(1)          VALUE ZERO.           
025200 01  IX-DAG                      PIC S9(3)  VALUE ZERO COMP-3.            
025300 01  IX-DAG-MAX                  PIC S9(3)  VALUE +5   COMP-3.            
025400 01  IX-DG                       PIC S9(3)  VALUE ZERO COMP-3.            
025500 01  IX-BHDC                     PIC 9(03)  VALUE ZERO.                   
025600 01  IX-BHDC-MAX                 PIC 9(03)  VALUE 156.                    
025700 01  IX-L                        PIC S9(3)  VALUE ZERO COMP-3.            
025800                                                                          
025900     EJECT                                                                
026000 01  IX-HELG                     PIC S9(5)  COMP-3  VALUE ZERO.           
026100 01  MAX-HELG                    PIC S9(5)  COMP-3  VALUE 2000.           
026200 01  ANT-HELG                    PIC S9(5)  COMP-3  VALUE ZERO.           
026300 01  TABELL-HELG.                                                         
026400     03  TAB-RAD   OCCURS 2000.                                           
026500         05  TAB-IDLANDX2        PIC X(2).                                
026600         05  TAB-DADATUM-HELG    PIC 9(8).                                
026700         05  TAB-FLHELG          PIC X.                                   
026800                                                                          
026900 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
027000*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
027100*- - - - - - - - - - - - - DC-MAX OCCURS SÄTTS TILL VERKLIGT ANTAL        
027200*- - - - - - - - - - - - - I AB-LAES-WDB6-INFO SEKTIONEN.                 
027300                                                                          
027400 01  IDDC-INDEX-WS.                                                       
027500     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
027600                                                                          
027700     03 WDCIX            PIC S9(3)   VALUE +0  COMP SYNC.                 
027800     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
027900                                                                          
028000 01  IDDC-TABELL.                                                         
028100     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
028200                INDEXED BY DCIX.                                          
028300        05 T-DCS.                                                         
028400          07 T-DCS-IDDC           PIC X(2).                               
028500          07 T-DCS-KDDC           PIC X(2).                               
028600          07 T-DCS-FLOVRLAGBER    PIC X.                                  
028700          07 T-DCS-IDLANDX2       PIC X(2).                               
028800                                                                          
028900 01  TILLGANGSTABELL.                                                     
029000     03  TILLGTAB-IX-MAX     PIC  9(3)   VALUE 156.                       
029100     03  TILLGTAB-IX         PIC  9(3)   VALUE 0.                         
029200     SKIP1                                                                
029300     03  TILLGTAB.                                                        
029400         05  TILLGTAB-INGANG OCCURS 156.                                  
029500             10  TILLGTAB-ANTAL                                           
029600                             PIC S9(7)V99            COMP-3.              
029700                                                                          
029800                                                                          
029900 01  SWITCHAR.                                                            
030000     03  SW-OK                  PIC X    VALUE 'N'.                       
030100     03  SW-FRYS                PIC X    VALUE 'N'.                       
030200     03  SW-HELG                PIC X    VALUE 'N'.                       
030300     03  SW-OMSPEC-UTFOERD      PIC X    VALUE 'N'.                       
030400     03  SW-OPTIMAL-OMSPEC      PIC X    VALUE 'N'.                       
030500     03  SW-X-OPT               PIC X    VALUE 'N'.                       
030600*                                                                         
030700     03  SW-AUT-PLAN            PIC X     VALUE 'J'.                      
030800     03  SW-PERSLUT             PIC X     VALUE 'N'.                      
030900         88 PERSLUT-JAMN                  VALUE 'J'.                      
031000     03  SW-GAMMAL-LEVERANS     PIC X     VALUE 'N'.                      
031100     03  SW-SKIP-FORSLAG        PIC X     VALUE 'N'.                      
031200     03  SW-FOERSTA-AVROPSVECKA PIC X     VALUE 'N'.                      
031300     03  SW-FORSLAG-AVROP-SAKNAS PIC X    VALUE 'J'.                      
031400     03  SW-FLORS               PIC X     VALUE 'N'.                      
031500     03  SW-KDERS-X9            PIC X     VALUE 'N'.                      
031600         88 KDERS-X9                      VALUE 'J'.                      
031700     03  SW-WDF1                PIC X     VALUE 'J'.                      
031800         88 WDF1-EXIST                    VALUE 'J'.                      
031900         88 WDF1-MISSING                  VALUE 'N'.                      
032000     03  SW-KDLPORS-03          PIC X     VALUE 'N'.                      
032100         88 KDLPORS-03                    VALUE 'J'.                      
032200         88 KDLPORS-NOT-03                VALUE 'N'.                      
032300                                                                          
032400                                                                          
032500                                                                          
032600 01  WC-IDPTYP.                                                           
032700     03  BORTTAG-OMSPEC          PIC X(3)    VALUE '001'.                 
032800     03  BORTTAG-FORSLAG         PIC X(3)    VALUE '002'.                 
032900     03  UPDATE-WDK722           PIC X(3)    VALUE '003'.                 
033000     03  NYUPPL-OMSPEC           PIC X(3)    VALUE '004'.                 
033100     03  UPPDAT-OMSPEC           PIC X(3)    VALUE '005'.                 
033200     03  NYUPPL-LEV              PIC X(3)    VALUE '006'.                 
033300     03  UPD-AVROP               PIC X(3)    VALUE '007'.                 
033400     03  NYUPPL-AVROP            PIC X(3)    VALUE '008'.                 
033500                                                                          
033600 01  SPAR-FAELT.                                                          
033700* FRÅN WDK722                                                             
033800     03  SPAR-XLAG-KDLEVPLF  PIC X(1)   VALUE SPACE.                      
033900     03  SPAR-XLAG-KDLPSP    PIC S9     VALUE ZERO COMP-3.                
034000     03  SPAR-XLAG-TIOMSPEC  PIC S9(5)  VALUE ZERO COMP-3.                
034100     03  SPAR-XLAG-TILPSP    PIC S9(5)  VALUE ZERO COMP-3.                
034200* FRÅN WDD904                                                             
034300     03  SPAR-KDLPORS-GRP.                                                
034400         05  SPAR-D904-KDLPORS-TAB OCCURS 3 PIC S9(3) COMP-3.             
034500     03  SPAR-D904-DASPECST  PIC 9(6)   VALUE ZERO.                       
034600     03  SPAR-KVBEST-PL      PIC S9(7)  VALUE ZERO    COMP-3.             
034700     03  SPAR-KDPLKOEP       PIC S9     VALUE ZERO    COMP-3.             
034800* ARBETSFÄLT ??                                                           
034900     03  SPAR-KVAVROP        PIC S9(7)  VALUE ZERO    COMP-3.             
035000     03  SPAR-TILEVDAG       PIC  9(1)  VALUE ZERO.                       
035100     03  SPAR-TIAVROP-AVS    PIC  9(4)  VALUE ZERO.                       
035200     03  FIX-AAVVD           PIC  9(5)  VALUE ZERO.                       
035300                                                                          
035400                                                                          
035500 01  W-DATUM-AAVV            PIC 9(4).                                    
035600 01  W-DAT REDEFINES W-DATUM-AAVV.                                        
035700     03  W-DATUM-AA          PIC 9(2).                                    
035800     03  W-DATUM-VV          PIC 9(2).                                    
035900 01  W-AAVV                  PIC 9(4)  VALUE ZERO.                        
036000                                                                          
036100 01  AKT-DATUM-AAMMDD        PIC 9(6).                                    
036200 01  FILLER REDEFINES AKT-DATUM-AAMMDD.                                   
036300     03  AKT-DATUM-AR        PIC 9(2).                                    
036400     03  AKT-DATUM-MM        PIC 9(2).                                    
036500     03  AKT-DATUM-DD        PIC 9(2).                                    
036600                                                                          
036700 01  AKT-DATUM-AAVV          PIC 9(4).                                    
036800 01  FILLER REDEFINES AKT-DATUM-AAVV.                                     
036900     03  AKT-DATUM-AA        PIC 9(2).                                    
037000     03  AKT-DATUM-VV        PIC 9(2).                                    
037100 01  AKT-DATUM-AAVV-35       PIC S9(5) COMP-3.                            
037200                                                                          
037300 01  W-TIFINLV-AAVVD         PIC 9(5).                                    
037400 01  FILLER REDEFINES W-TIFINLV-AAVVD.                                    
037500     03  W-TIFINLV-AAVV      PIC 9(4).                                    
037600     03  FILLER REDEFINES W-TIFINLV-AAVV.                                 
037700         05 W-TIFINLV-AA     PIC 9(2).                                    
037800         05 W-TIFINLV-VV     PIC 9(2).                                    
037900     03  W-TIFINLV-D         PIC 9(1).                                    
038000*                                                                         
038100 01  W-TISPECST-ADJ          PIC 9(4).                                    
038200                                                                          
038300 01  W-DATUM-FROM            PIC S9(5)               COMP-3.              
038400 01  W-DATUM-FROM-AAVV       PIC  9(4).                                   
038500 01  FILLER REDEFINES W-DATUM-FROM-AAVV.                                  
038600     03  W-DATUM-FROM-AA     PIC  9(2).                                   
038700     03  W-DATUM-FROM-VV     PIC  9(2).                                   
038800                                                                          
038900 01  W-DATUM-TOM             PIC S9(5)               COMP-3.              
039000 01  W-DATUM-TOM-AAVV        PIC  9(4).                                   
039100 01  FILLER REDEFINES W-DATUM-TOM-AAVV.                                   
039200     03  W-DATUM-TOM-AA      PIC  9(2).                                   
039300     03  W-DATUM-TOM-VV      PIC  9(2).                                   
039400                                                                          
039500 01  W-AAVV-JUST             PIC 9(4)  VALUE ZERO.                        
039600 01  FILLER REDEFINES W-AAVV-JUST.                                        
039700     03  W-AAVV-JUST-AA          PIC 9(2).                                
039800     03  W-AAVV-JUST-VV          PIC 9(2).                                
039900                                                                          
040000 01  W-DATUM-AAVV-AKT        PIC S9(5)               COMP-3.              
040100 01  W-DATUM-AAVV-HELP       PIC S9(5)               COMP-3.              
040200 01  W-KVPB-SDC-TOT          PIC S9(7)               COMP-3.              
040300 01  W-TILLG-SDC             PIC S9(6)V9(1)          COMP-3.              
040400 01  W-TILLG                 PIC S9(7)V9(2)          COMP-3.              
040500 01  W-TILLG-BER             PIC S9(7)V9(2)          COMP-3.              
040600 01  W-TILLG-SPAR            PIC S9(7)V9(2)          COMP-3.              
040700 01  W-OVERLAGER-SDC         PIC S9(7)               COMP-3.              
040800 01  W-KVOKS                 PIC S9(7)               COMP-3.              
040900 01  W-HELP-DATUM-SSAAVVD.                                                
041000     03  W-HELP-DATUM-SSAA        PIC 9(4).                               
041100     03  FILLER REDEFINES W-HELP-DATUM-SSAA.                              
041200         05  W-HELP-DATUM-SS      PIC 9(2).                               
041300         05  W-HELP-DATUM-AA      PIC 9(2).                               
041400     03  W-HELP-DATUM-VV          PIC 9(2).                               
041500     03  W-HELP-DATUM-D           PIC 9(1).                               
041600 01  W-HELP-TIFINLV-SSAAVVD.                                              
041700     03  W-HELP-TIFINLV-SSAA        PIC 9(4).                             
041800     03  FILLER REDEFINES W-HELP-TIFINLV-SSAA.                            
041900         05  W-HELP-TIFINLV-SS    PIC 9(2).                               
042000         05  W-HELP-TIFINLV-AA    PIC 9(2).                               
042100     03  W-HELP-TIFINLV-VV        PIC 9(2).                               
042200     03  W-HELP-TIFINLV-D         PIC 9(1).                               
042300 01  W-VECKO-DIFF            PIC S9(4)  VALUE ZERO   COMP-3.              
042400 01  W-VECKO-DIFF-TEST       PIC S9(4)  VALUE ZERO   COMP-3.              
042500 01  W-DAINLEV               PIC 9(6)   VALUE ZERO.                       
042600 01  W-ANTAL-VECKOR          PIC S9(3)  VALUE ZERO   COMP-3.              
042700 01  W-AAVV-ADD              PIC S9(5)  VALUE ZERO   COMP-3.              
042800 01  W-TISPECST-DISP         PIC S9(5)  VALUE ZERO   COMP-3.              
042900 01  W-TISPECST              PIC S9(5)  VALUE ZERO   COMP-3.              
043000 01  WS-TISPECST-DAYS-AAVV   PIC  9(4)  VALUE ZERO.                       
043100 01  W-TISPEC-TOT            PIC S9(5)  VALUE ZERO   COMP-3.              
043200 01  W-DASPECST-AAMMDD       PIC  9(6)  VALUE ZERO.                       
043300 01  WS-DASPECST             PIC  9(6)  VALUE ZERO.                       
043400 01  FILLER  REDEFINES WS-DASPECST.                                       
043500     03  WS-DASPECST-SS      PIC 9(2).                                    
043600     03  WS-DASPECST-AAVV    PIC 9(4).                                    
043700 01  WS-DASPECST-AAVVD       PIC  9(5)  VALUE ZERO.                       
043800 01  FILLER  REDEFINES WS-DASPECST-AAVVD.                                 
043900     03  WS-DASPECST-YYVV    PIC 9(4).                                    
044000     03  WS-DASPECST-D       PIC 9(1).                                    
044100                                                                          
044200 01  W-KVVECKOR-FFH          PIC S9(3)  VALUE ZERO   COMP-3.              
044300 01  W-KVVECKOR-SPEC         PIC S9(3)  VALUE ZERO.                       
044400 01  W-KVVECKOR-ADD          PIC S9(3)  VALUE ZERO   COMP-3.              
044500 01  W-GRAENS-AVROP          PIC S9(5)  VALUE ZERO   COMP-3.              
044600 01  W-DAAVROP-AVS           PIC  9(6)  VALUE ZERO.                       
044700 01  FILLER REDEFINES W-DAAVROP-AVS.                                      
044800     03  W-DAAVROP-AVS-SS    PIC  9(2).                                   
044900     03  W-DAAVROP-AVS-AAVV  PIC  9(4).                                   
045000     03  FILLER REDEFINES W-DAAVROP-AVS-AAVV.                             
045100         05  W-DAAVROP-AVS-AA     PIC 9(2).                               
045200         05  W-DAAVROP-AVS-VV     PIC 9(2).                               
045300 01  W-DAAVROP-AVS-AAVV-C3   PIC S9(5)   COMP-3.                          
045400 01  W-GALL-AVROP            PIC  9(6)  VALUE ZERO.                       
045500 01  FILLER REDEFINES W-GALL-AVROP.                                       
045600     03  W-GALL-AVROP-SS     PIC 9(2).                                    
045700     03  W-GALL-AVROP-AA     PIC 9(2).                                    
045800     03  W-GALL-AVROP-VV     PIC 9(2).                                    
045900 01  W-PREL-AVROP            PIC  9(6)  VALUE ZERO.                       
046000 01  FILLER REDEFINES W-PREL-AVROP.                                       
046100     03  W-PREL-AVROP-SS     PIC 9(2).                                    
046200     03  W-PREL-AVROP-AA     PIC 9(2).                                    
046300     03  W-PREL-AVROP-VV     PIC 9(2).                                    
046400 01  W-KVBEST-PL             PIC S9(7)               COMP-3.              
046500 01  W-KDPLKOEP              PIC S9                  COMP-3.              
046600 01  W-SUM-START             PIC S9(7)  VALUE ZERO   COMP-3.              
046700 01  W-BUFF                  PIC S9(7)  VALUE ZERO   COMP-3.              
046800 01  W-SUMMA-BEHOV           PIC S9(7)  VALUE ZERO   COMP-3.              
046900 01  W-FLAGGA-2AAR           PIC X      VALUE 'N'.                        
047000 01  W-VVBEHOV               PIC S9(3)  VALUE ZERO   COMP-3.              
047100 01  W-ARSBEH                PIC S9(9)      VALUE ZERO   COMP-3.          
047200 01  W-ARSOMS                PIC S9(9)V9(2) VALUE ZERO   COMP-3.          
047300 01  W-ARSOMS-100000         PIC S9(9)V9(2) VALUE 100000 COMP-3.          
047400 01  W-IDLANDX2-SHIP         PIC X(2)     VALUE SPACE.                    
047500 01  W-KVANTITET             PIC S9(7)               COMP-3.              
047600 01  W-ANTAL                 PIC S9(9)   VALUE +0    COMP-3.              
047700 01  W-KVULOAD               PIC S9(7) VALUE ZERO    COMP-3.              
047800 01  W-AVROPSKVANTITET       PIC S9(7)   VALUE +0    COMP-3.              
047900 01  W-Q-FREKV-MAX           PIC S9(3)   VALUE +0    COMP-3.              
048000 01  W-FRYSTID8              PIC 9(8).                                    
048100 01  FILLER  REDEFINES W-FRYSTID8.                                        
048200     03  W-FRYSTID-SEKEL     PIC 9(2).                                    
048300     03  W-FRYSTID           PIC 9(6).                                    
048400 01  W-ORSAKSKOD             PIC S9(3)               COMP-3.              
048500 01  W-TIAAMMDD-AVS          PIC 9(6).                                    
048600 01  ARB-TIAAMMDD-AVS        PIC 9(6).                                    
048700 01  GAM-TIAAMMDD-AVS        PIC 9(6).                                    
048800 01  WOL-TILEVDAG            PIC 9.                                       
048900 01  WOL-TIAAVV-AVS          PIC 9(4).                                    
049000 01  WOL-TIAAVV-AVS-C3       PIC S9(5)   COMP-3.                          
049100 01  W-TIAVROP-DISP          PIC S9(5)   COMP-3.                          
049200 01  W-TIAVRDAT-DISP         PIC 9(6).                                    
049300 01  W-TIAVRDAT-INL          PIC 9(6).                                    
049400 01  W-SUMMA-KVPB            PIC S9(6)V9(1)   COMP-3.                     
049500 01  ARB-TILEVDAG-GRP.                                                    
049600     03  ARB-TILEVDAG  OCCURS 5  PIC 9.                                   
049700 01  W-TILEVDAG-GRP.                                                      
049800     03  W-TILEVDAG    OCCURS 5  PIC 9.                                   
049900 01  ANT-LEVDAG              PIC 9(3).                                    
050000 01  W-KVPALL                PIC S9(7) VALUE ZERO    COMP-3.              
050100 01  W-KVAVROP-GRP.                                                       
050200     03  W-KVAVROP     OCCURS 5  PIC S9(7) COMP-3.                        
050300 01  W-DAAVROP               PIC 9(5)  VALUE ZERO    COMP-3.              
050400 01  VADD-DATUM-AAVV         PIC S9(5)  COMP-3.                           
050500 01  WS-DAYS-TIAVRDAT-DISP        PIC 9(6).                               
050600 01  WS-DAYS-TIDATE1-AAVVD   PIC 9(5)     VALUE ZERO.                     
050700 01  WS-DAYS-TIDATE-AA       PIC 9(2)     VALUE ZERO.                     
050800 01  WS-DAYS-TIDATE-VV       PIC 9(2)     VALUE ZERO.                     
050900 01  WS-DAYS-TIFINLV         PIC 9(5)     VALUE ZERO.                     
051000                                                                          
051100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
051200*                                                                         
051300                                                                          
051400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
051500                                                                          
051600 01  NYCKLAR-TILL-DLI.                                                    
051700     03  W-IDARTNR-X.                                                     
051800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
051900     03  W-WDGXKEY-X.                                                     
052000         05  W-WDGXKEY           PIC X(30)   VALUE SPACE.                 
052100     03  W-IDLEVNR-X.                                                     
052200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
052300     03  W-WDF301KY-X.                                                    
052400         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
052500         05  W-DADATUM-HELG      PIC 9(8)    VALUE ZERO.                  
052600         05  FILLER  REDEFINES W-DADATUM-HELG.                            
052700             07  W-DADATUM-HELG-SS      PIC 9(2).                         
052800             07  W-DADATUM-HELG-AAMMDD  PIC 9(6).                         
052900     03  W-IDDC-X.                                                        
053000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
053100     03  W-IDLAND-X.                                                      
053200         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
053300     03  W-IDSKYLT-X.                                                     
053400         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
053500     03  W-IDBENNR-X.                                                     
053600         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
053700     03  W-WDD601KY-X.                                                    
053800         05  W-WDD601KY          PIC X(14)   VALUE SPACE.                 
053900     03  W-WDD901KY-X.                                                    
054000         05 W-IDARTNR-D9         PIC S9(9)   VALUE ZERO COMP-3.           
054100         05 W-IDDC-D9            PIC X(2)    VALUE SPACE.                 
054200     03  W-KDAVROP-X.                                                     
054300         05 W-KDAVROP            PIC S9(1)   VALUE ZERO COMP-3.           
054400     03  W-DAAVROP-MIN-X.                                                 
054500         05 W-DAAVROP-MIN        PIC  9(6)   VALUE ZERO.                  
054600     03  W-DAAVROP-MAX-X.                                                 
054700         05 W-DAAVROP-MAX        PIC  9(6)   VALUE ZERO.                  
054800         05 FILLER REDEFINES W-DAAVROP-MAX.                               
054900            07 W-DAAVROP-MAX-SS    PIC  9(2).                             
055000            07 W-DAAVROP-MAX-AAVV  PIC  9(4).                             
055100     03  W-IDLEVNR-SHIP-X.                                                
055200         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
055300                                                                          
055400*    --- STATUS-KOD FRÅN IMS                                              
055500 01  STATUS-WS                   PIC XX.                                  
055600     88  SEGMENT-FINNS                       VALUE '  '.                  
055700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
055800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
055900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
056000                                                                          
056100                                                                          
056200 01  GODK-STATUSKODER.                                                    
056300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
056400                                                                          
056500                                                                          
056600 01  ALL-SSA.                                                             
056700     03 SSA1                     PIC X(64).                               
056800     03 SSA2                     PIC X(64).                               
056900     03 SSA3                     PIC X(64).                               
057000                                                                          
057100                                                                          
057200*    --- IMS FUNKTIONSKODER                                               
057300*01  -COPY W0003                                                          
057400                                                                          
057500*    ---  DLI INPUT-OUTPUT AREA                                           
057600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
057700 01  DLI-IO-WDK701.                                                       
057800*    03  -COPY WDK701                                                     
057900                                                                          
058000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
058100 01  DLI-IO-WDK711.                                                       
058200*    03  -COPY WDK711                                                     
058300                                                                          
058400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
058500 01  DLI-IO-WDK712.                                                       
058600*    03  -COPY WDK712                                                     
058700                                                                          
058800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
058900 01  DLI-IO-WDK722.                                                       
059000*    03  -COPY WDK722                                                     
059100                                                                          
059200                                                                          
059300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
059400 01  DLI-IO-WDL601.                                                       
059500*    03  -COPY WDL601                                                     
059600                                                                          
059700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
059800 01  DLI-IO-WDL611.                                                       
059900*    03  -COPY WDL611                                                     
060000                                                                          
060100                                                                          
060200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
060300 01  DLI-IO-WDF106.                                                       
060400*    03  -COPY WDF106                                                     
060500                                                                          
060600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
060700 01  DLI-IO-WDF116.                                                       
060800*    03  -COPY WDF116                                                     
060900                                                                          
061000                                                                          
061100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF3A1'.                      
061200 01  DLI-IO-WDF3A1.                                                       
061300*    03  -COPY WDF3A1                                                     
061400                                                                          
061500                                                                          
061600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
061700 01  DLI-IO-WDB601.                                                       
061800*    03  -COPY WDB601                                                     
061900                                                                          
062000                                                                          
062100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
062200 01  DLI-IO-WDD311.                                                       
062300*    03  -COPY WDD311                                                     
062400                                                                          
062500                                                                          
062600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
062700 01  DLI-IO-WDD901.                                                       
062800*    03  -COPY WDD901  -PRE D901-                                         
062900                                                                          
063000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
063100 01  DLI-IO-WDD902.                                                       
063200*    03  -COPY WDD902  -PRE D902-                                         
063300                                                                          
063400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
063500 01  DLI-IO-WDD904.                                                       
063600*    03  -COPY WDD904  -PRE D904-                                         
063700                                                                          
063800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
063900 01  DLI-IO-WDD905.                                                       
064000*    03  -COPY WDD905  -PRE D905-                                         
064100                                                                          
064200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD704'.                      
064300 01  DLI-IO-WDD704.                                                       
064400*    03  -COPY WDD704  -PRE D704-                                         
064500                                                                          
064600                                                                          
064700                                                                          
064800 LINKAGE SECTION.                                                         
064900                                                                          
065000                                                                          
065100*01  -COPY W0008  -PRE WDK7-                                              
065200     05  FILLER                   PIC X.                                  
065300                                                                          
065400*01  -COPY W0008  -PRE WDD9-                                              
065500     05  FILLER                   PIC X(7).                               
065600     05  WDD9-KEY-02-IDLEVNR      PIC X(5).                               
065700                                                                          
065800*01  -COPY W0008  -PRE WDF1-                                              
065900     05  FILLER                   PIC X.                                  
066000                                                                          
066100*01  -COPY W0008  -PRE WDF3-                                              
066200     05  FILLER                   PIC X.                                  
066300                                                                          
066400*01  -COPY W0008  -PRE WDB6-                                              
066500     05  FILLER                   PIC X.                                  
066600                                                                          
066700*01  -COPY W0008  -PRE WDD3-                                              
066800     05  FILLER                   PIC X.                                  
066900                                                                          
067000*01  -COPY W0008  -PRE WDD7-                                              
067100     05  FILLER                   PIC X.                                  
067200                                                                          
067300*    PROGRAM W222BHDC                                                     
067400 01  BHDC-WDK6-PCB                PIC X.                                  
067500 01  BHDC-WDK7-PCB                PIC X.                                  
067600 01  BHDC-WDB6-PCB                PIC X.                                  
067700 01  BHDC-WDR2-PCB                PIC X.                                  
067800 01  BHDC-WDD7-PCB                PIC X.                                  
067900 01  BHDC-WDK7E-PCB               PIC X.                                  
068000 01  BHDC-WDD7-2-PCB              PIC X.                                  
068100 01  BHDC-WDK9-PCB                PIC X.                                  
068200 01  BHDC-REFL1-2501-PCB          PIC X.                                  
068300 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
068400 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
068500 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
068600 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
068700 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
068800     EJECT                                                                
068900 01  BHDC-REFL2-2501-PCB          PIC X.                                  
069000 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
069100 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
069200 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
069300 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
069400     EJECT                                                                
069500 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
069600 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
069700 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
069800     EJECT                                                                
069900 01  BHDC-W222-WDK6-PCB           PIC X.                                  
070000 01  BHDC-W222-WDK7-PCB           PIC X.                                  
070100 01  BHDC-W222-ARTM-PCB           PIC X.                                  
070200 01  BHDC-W222-2501-PCB           PIC X.                                  
070300 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
070400 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
070500 01  BHDC-W222-WDB6-PCB           PIC X.                                  
070600 01  BHDC-W222-WDD7-PCB           PIC X.                                  
070700 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
070800 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
070900 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
071000 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
071100 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
071200 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
071300 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
071400 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
071500 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
071600     EJECT                                                                
071700 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
071800 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
071900 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
072000 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
072100 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
072200     EJECT                                                                
072300                                                                          
072400                                                                          
072500 PROCEDURE DIVISION  USING WDK7-PCB WDD9-PCB                              
072600                           WDF1-PCB WDF3-PCB WDB6-PCB WDD3-PCB            
072700                           WDD7-PCB                                       
072800                                                                          
072900                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
073000                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
073100                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
073200                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
073300                           BHDC-REFL1-2501-PCB                            
073400                           BHDC-REFL1-WDB6-PCB                            
073500                           BHDC-REFL1-WDK7-PCB                            
073600                           BHDC-REFL1-UTIL-WDK6-PCB                       
073700                           BHDC-REFL1-UTIL-WDK7-PCB                       
073800                           BHDC-REFL1-UTIL-WDB6-PCB                       
073900                           BHDC-REFL2-2501-PCB                            
074000                           BHDC-REFL2-WDB6-PCB                            
074100                           BHDC-REFL2-UTIL-WDK6-PCB                       
074200                           BHDC-REFL2-UTIL-WDK7-PCB                       
074300                           BHDC-REFL2-UTIL-WDB6-PCB                       
074400                           BHDC-UTIL-WDK6-PCB                             
074500                           BHDC-UTIL-WDK7-PCB                             
074600                           BHDC-UTIL-WDB6-PCB                             
074700                           BHDC-W222-WDK6-PCB                             
074800                           BHDC-W222-WDK7-PCB                             
074900                           BHDC-W222-ARTM-PCB                             
075000                           BHDC-W222-2501-PCB                             
075100                           BHDC-W222-WDB6R-PCB                            
075200                           BHDC-W222-WDK7R-PCB                            
075300                           BHDC-W222-WDB6-PCB                             
075400                           BHDC-W222-WDD7-PCB                             
075500                           BHDC-W222-WDK7E-PCB                            
075600                           BHDC-W222-UTIL-WDK6-PCB                        
075700                           BHDC-W222-UTIL-WDK7-PCB                        
075800                           BHDC-W222-UTIL-WDB6-PCB                        
075900                           BHDC-W222-UTUP-WDK7-PCB                        
076000                           BHDC-W222-UTUP-WDB6-PCB                        
076100                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
076200                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
076300                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
076400                           BHDC-UTUP-WDK7-PCB                             
076500                           BHDC-UTUP-WDB6-PCB                             
076600                           BHDC-UTUP-UTIL-WDK6-PCB                        
076700                           BHDC-UTUP-UTIL-WDK7-PCB                        
076800                           BHDC-UTUP-UTIL-WDB6-PCB                        
076900                           .                                              
077000                                                                          
077100 MAIN SECTION.                                                            
077200     ENTRY 'DLITCBL' USING WDK7-PCB WDD9-PCB                              
077300                           WDF1-PCB WDF3-PCB WDB6-PCB WDD3-PCB            
077400                           WDD7-PCB                                       
077500                                                                          
077600                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
077700                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
077800                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
077900                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
078000                           BHDC-REFL1-2501-PCB                            
078100                           BHDC-REFL1-WDB6-PCB                            
078200                           BHDC-REFL1-WDK7-PCB                            
078300                           BHDC-REFL1-UTIL-WDK6-PCB                       
078400                           BHDC-REFL1-UTIL-WDK7-PCB                       
078500                           BHDC-REFL1-UTIL-WDB6-PCB                       
078600                           BHDC-REFL2-2501-PCB                            
078700                           BHDC-REFL2-WDB6-PCB                            
078800                           BHDC-REFL2-UTIL-WDK6-PCB                       
078900                           BHDC-REFL2-UTIL-WDK7-PCB                       
079000                           BHDC-REFL2-UTIL-WDB6-PCB                       
079100                           BHDC-UTIL-WDK6-PCB                             
079200                           BHDC-UTIL-WDK7-PCB                             
079300                           BHDC-UTIL-WDB6-PCB                             
079400                           BHDC-W222-WDK6-PCB                             
079500                           BHDC-W222-WDK7-PCB                             
079600                           BHDC-W222-ARTM-PCB                             
079700                           BHDC-W222-2501-PCB                             
079800                           BHDC-W222-WDB6R-PCB                            
079900                           BHDC-W222-WDK7R-PCB                            
080000                           BHDC-W222-WDB6-PCB                             
080100                           BHDC-W222-WDD7-PCB                             
080200                           BHDC-W222-WDK7E-PCB                            
080300                           BHDC-W222-UTIL-WDK6-PCB                        
080400                           BHDC-W222-UTIL-WDK7-PCB                        
080500                           BHDC-W222-UTIL-WDB6-PCB                        
080600                           BHDC-W222-UTUP-WDK7-PCB                        
080700                           BHDC-W222-UTUP-WDB6-PCB                        
080800                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
080900                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
081000                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
081100                           BHDC-UTUP-WDK7-PCB                             
081200                           BHDC-UTUP-WDB6-PCB                             
081300                           BHDC-UTUP-UTIL-WDK6-PCB                        
081400                           BHDC-UTUP-UTIL-WDK7-PCB                        
081500                           BHDC-UTUP-UTIL-WDB6-PCB                        
081600                           .                                              
081700                                                                          
081800     PERFORM A-INIT                                                       
081900                                                                          
082000     PERFORM S01-LAES-W22412                                              
082100     PERFORM UNTIL END-OF-W22412                                          
082200                                                                          
082300        PERFORM B-HAEMTA-ARTIKELDATA                                      
082400        PERFORM C-KONTROLL-AUT-PLAN                                       
082500        PERFORM D-BEHANDLA-LEVDATA                                        
082600        PERFORM E-TYP-AV-OMSPEC                                           
082700        IF XLAG-KDLPSP = 5                                                
082800           PERFORM F-BORTTAG-OMSPEC                                       
082900        END-IF                                                            
083000                                                                          
083100        IF  XLAG-KDLPSP = 3                                               
083200        OR (LART-TIERSDAT-VIPS NOT = ZERO                                 
083300        AND KDLPORS-NOT-03)                                               
083400           PERFORM G-SPARA-LEVSPEC-FORSLAG                                
083500        ELSE                                                              
083600           PERFORM H-SKAPA-OMSPEC-LEVPLAN                                 
083700                                                                          
083800           PERFORM S22-KOLLA-ORSAKSKOD-SPARA                              
083900           IF XLAG-FLJIT = JA OR SW-FLORS = JA                            
084000             CONTINUE                                                     
084100           ELSE                                                           
084200             PERFORM K-KOLL-SKIP-PREL-PLAN                                
084300           END-IF                                                         
084400        END-IF                                                            
084500                                                                          
084600        IF SW-SKIP-FORSLAG = NEJ                                          
084700           PERFORM I-SKAPA-UTFILER                                        
084800        END-IF                                                            
084900                                                                          
085000        PERFORM J-KOLL-UPPD-AV-REGISTER                                   
085100                                                                          
085200        PERFORM S01-LAES-W22412                                           
085300     END-PERFORM                                                          
085400                                                                          
085500                                                                          
085600     PERFORM Z-FINIT                                                      
085700                                                                          
085800     MOVE ZERO TO RETURN-CODE                                             
085900     GOBACK                                                               
086000     .                                                                    
086100                                                                          
086200                                                                          
086300                                                                          
086400 A-INIT SECTION.                                                          
086500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
086600                                                                          
086700     OPEN INPUT  W22412                                                   
086800                                                                          
086900     OPEN OUTPUT W22414                                                   
087000                 W22415                                                   
087100                 W22416                                                   
087200                                                                          
087300     MOVE 20          TO W-DAAVROP-AVS-SS                                 
087400                                                                          
087500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
087600     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
087700     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
087800     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
087900     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
088000                                                                          
088100                                                                          
088200     MOVE D-AAR        TO W-DATUM-AA                                      
088300                          AKT-DATUM-AR                                    
088400     MOVE D-VECKA      TO W-DATUM-VV                                      
088500     MOVE W-DATUM-AAVV TO W-DATUM-AAVV-AKT                                
088600                          AKT-DATUM-AAVV                                  
088700                          AKT-DATUM-AAVV-35                               
088800     MOVE 35           TO W-ANTAL-VECKOR                                  
088900     CALL W009VADD USING AKT-DATUM-AAVV-35 W-ANTAL-VECKOR                 
089000                                                                          
089100     MOVE D-MAANAD     TO AKT-DATUM-MM                                    
089200     MOVE D-DAG        TO AKT-DATUM-DD                                    
089300                                                                          
089400     MOVE 'AAVV  '     TO DAT-KDDATFORM                                   
089500     MOVE W-DATUM-AAVV TO DAT-I-TIDATUM                                   
089600                                                                          
089700     CALL WDATKONV USING DAT-KDDATFORM                                    
089800                         DAT-I-TIDATUM                                    
089900                         DAT-O-TIDATUM                                    
090000                         DAT-KDSVAR                                       
090100                                                                          
090200     MOVE 'AARP  '     TO DAT-KDDATFORM                                   
090300     MOVE DAT-TIRP     TO DAT-I-TIDATUM                                   
090400                                                                          
090500     CALL WDATKONV USING DAT-KDDATFORM                                    
090600                         DAT-I-TIDATUM                                    
090700                         DAT-O-TIDATUM                                    
090800                         DAT-KDSVAR                                       
090900                                                                          
091000*    STARTVECKA FÖR PERIODEN + ANTAL VECKOR I PERIODEN = AKT VV ?         
091100     IF W-DATUM-VV = ( DAT-TIVV + DAT-KVVIPER - 1)                        
091200        IF DAT-TIRP = 2 OR 4 OR 6 OR 8 OR 10 OR 12                        
091300           MOVE JA TO SW-PERSLUT                                          
091400        END-IF                                                            
091500     END-IF                                                               
091600                                                                          
091700**    HÄR LÄSER VI IN HELGDAGSTAB LAND+DATUM                              
091800     PERFORM AA-FYLL-HELG-TAB                                             
091900                                                                          
092000**    HÄR LÄSER VI IN ALLA WDB601 (KINA) TILL IDDC-TABELLEN I WS          
092100**    FÖR ATT SLIPPA IMS-CALL FÖR VARJE ARTIKEL PÅ INFILEN.               
092200     PERFORM AB-LAES-WDB6-INFO                                            
092300     .                                                                    
092400     EJECT                                                                
092500                                                                          
092600 AA-FYLL-HELG-TAB  SECTION.                                               
092700     MOVE 'AA-FYLL-HELG-TAB' TO CURRENT-SECTION                           
092800                                                                          
092900     PERFORM IMS-GU-WDF3A1                                                
093000     MOVE +1                   TO IX-HELG                                 
093100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
093200        IF IX-HELG > MAX-HELG                                             
093300           MOVE '** FEL - TABELL HELGDAGAR FULL ** MAX-HELG '             
093400                              TO FELTEXT                                  
093500           CALL FELLOG                                                    
093600        END-IF                                                            
093700        MOVE HLGA-IDLANDX2     TO TAB-IDLANDX2     (IX-HELG)              
093800        MOVE HLGA-DADATUM-HELG TO TAB-DADATUM-HELG (IX-HELG)              
093900        MOVE HLGA-FLHELG       TO TAB-FLHELG       (IX-HELG)              
094000        ADD +1                 TO IX-HELG                                 
094100        PERFORM IMS-GN-WDF3A1                                             
094200     END-PERFORM                                                          
094300                                                                          
094400     MOVE IX-HELG              TO   ANT-HELG                              
094500     SUBTRACT 1                FROM ANT-HELG                              
094600     .                                                                    
094700     EJECT                                                                
094800                                                                          
094900 AB-LAES-WDB6-INFO SECTION.                                               
095000     MOVE 'AB-LAES-WDB6-INF' TO CURRENT-SECTION                           
095100                                                                          
095200     SET DCIX TO +1                                                       
095300     PERFORM IMS-GN-WDB601                                                
095400     PERFORM UNTIL SEGMENT-SLUT                                           
095500        IF DCIX <= DC-MAX                                                 
095600           IF DCS-CHINA OR DCS-FTG-CN                                     
095700           OR DCS-USA OR DCS-FTG-US                                       
095800              MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                           
095900              MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                           
096000              MOVE DCS-FLOVRLAGBER TO T-DCS-FLOVRLAGBER(DCIX)             
096100              MOVE DCS-IDLANDX2    TO T-DCS-IDLANDX2(DCIX)                
096200                                                                          
096300              SET DCIX UP BY +1                                           
096400           END-IF                                                         
096500           PERFORM IMS-GN-WDB601                                          
096600        ELSE                                                              
096700                                                                          
096800           MOVE +35 TO RKOD-ABEND                                         
096900           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT-STR               
097000           DISPLAY FELTEXT                                                
097100           CALL ABEND USING RKOD-ABEND                                    
097200        END-IF                                                            
097300     END-PERFORM                                                          
097400                                                                          
097500*    --- SÄTTER TAKET PÅ TABELLEN                                         
097600     SET DCIX   DOWN BY +1                                                
097700     SET DC-MAX TO DCIX                                                   
097800                                                                          
097900*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
098000     MOVE DC-MAX                  TO ANTAL                                
098100     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
098200     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
098300                                                                          
098400     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
098500                  T-DCS-IDDC(1) NYCKELLANGD                               
098600     .                                                                    
098700     EJECT                                                                
098800                                                                          
098900 B-HAEMTA-ARTIKELDATA SECTION.                                            
099000     MOVE 'B-HAEMTA-ART-DAT'  TO CURRENT-SECTION                          
099100                                                                          
099200     MOVE ZERO                TO W-KVPB-SDC-TOT                           
099300                                 W-TILLG-SDC                              
099400                                 W-OVERLAGER-SDC                          
099500                                                                          
099600     MOVE OMSP-IDARTNR        TO W-IDARTNR                                
099700     MOVE OMSP-IDDC           TO W-IDDC                                   
099800                                                                          
099900     PERFORM BB-KOLLA-IDLANDX2-WDB601                                     
100000     PERFORM IMS-GU-WDK712                                                
100100     IF SEGMENT-SAKNAS                                                    
100200        MOVE ZERO             TO LART-DAPUBL                              
100300                                 LART-PRMATRL                             
100400                                 LART-KVDAGAR-INLEV                       
100500                                 LART-TIERSDAT-VIPS                       
100600     END-IF                                                               
100700     PERFORM IMS-GU-WDK722                                                
100800     MOVE XLAG-KDLEVPLF       TO SPAR-XLAG-KDLEVPLF                       
100900     MOVE XLAG-KDLPSP         TO SPAR-XLAG-KDLPSP                         
101000     MOVE XLAG-TIOMSPEC       TO SPAR-XLAG-TIOMSPEC                       
101100     MOVE XLAG-TILPSP         TO SPAR-XLAG-TILPSP                         
101200                                                                          
101300     PERFORM IMS-GU-WDK701                                                
101400     PERFORM IMS-GNP-WDK711-REF                                           
101500     PERFORM UNTIL SEGMENT-SAKNAS                                         
101600                                                                          
101700        ADD SLAG-KVPB-REF     TO W-KVPB-SDC-TOT                           
101800                                                                          
101900        MOVE SLAG-KVLS        TO W-TILLG-SDC                              
102000        ADD  SLAG-KVBEART     TO W-TILLG-SDC                              
102100        ADD  SLAG-KVAKS-SDC   TO W-TILLG-SDC                              
102200        ADD  SLAG-KVAKS-PAV   TO W-TILLG-SDC                              
102300                                                                          
102400        COMPUTE W-KVOKS = SLAG-KVOKS-BULK + SLAG-KVOKS-DAG                
102500                                                                          
102600*   --- FIX FÖR NEGATIVA KVOKS                                            
102700        IF W-KVOKS > ZERO                                                 
102800           SUBTRACT W-KVOKS   FROM W-TILLG-SDC                            
102900        END-IF                                                            
103000                                                                          
103100        SUBTRACT SLAG-KVRESS  FROM W-TILLG-SDC                            
103200                                                                          
103300        PERFORM BA-KOLLA-OVERLAGERBERAKNING                               
103400                                                                          
103500        MOVE ZERO   TO W-VECKO-DIFF                                       
103600        IF LART-DAPUBL > ZERO                                             
103700*WZ20DAYS                                                                 
103800           MOVE LART-DAPUBL(3:6)  TO DAYS-TIDATE1                         
103900           MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                       
104000           MOVE 'YYYYWW'          TO DAYS-KDDATFMT2                       
104100           MOVE 0                 TO DAYS-KVDAYS                          
104200           MOVE SPACE             TO DAYS-TIDATE2                         
104300                                     DAYS-IDCALEND                        
104400           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
104500*                                                                         
104600           IF DAYS-KDRC = 8                                               
104700             MOVE 'FEL VID ANROP TILL WZ20DAYS 1' TO FELTEXT              
104800             MOVE 32            TO RKOD                                   
104900             CALL ABEND USING RKOD                                        
105000           ELSE                                                           
105100             MOVE DAYS-TIDATE2(1:2) TO W-HELP-TIFINLV-SS                  
105200             MOVE DAYS-TIDATE2(3:2) TO W-HELP-TIFINLV-AA                  
105300             MOVE DAYS-TIDATE2(5:2) TO W-HELP-TIFINLV-VV                  
105400           END-IF                                                         
105500*                                                                         
105600        ELSE                                                              
105700*WZ20DAYS                                                                 
105800           MOVE OMSP-TIFINLV    TO WS-DAYS-TIFINLV                        
105900           MOVE WS-DAYS-TIFINLV TO DAYS-TIDATE1                           
106000           MOVE 'YYWWD'         TO DAYS-KDDATFMT1                         
106100           MOVE 'YYYYWW'        TO DAYS-KDDATFMT2                         
106200           MOVE 0               TO DAYS-KVDAYS                            
106300           MOVE SPACE           TO DAYS-TIDATE2                           
106400                                   DAYS-IDCALEND                          
106500           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
106600*                                                                         
106700           IF DAYS-KDRC = 8                                               
106800             MOVE 'FEL VID ANROP TILL WZ20DAYS 2' TO FELTEXT              
106900             MOVE 32            TO RKOD                                   
107000             CALL ABEND USING RKOD                                        
107100           ELSE                                                           
107200             MOVE DAYS-TIDATE2(1:2) TO W-HELP-TIFINLV-SS                  
107300             MOVE DAYS-TIDATE2(3:2) TO W-HELP-TIFINLV-AA                  
107400             MOVE DAYS-TIDATE2(5:2) TO W-HELP-TIFINLV-VV                  
107500           END-IF                                                         
107600        END-IF                                                            
107700                                                                          
107800                                                                          
107900*--KAN VARA PROBLEM ATT ALLTID LÄGGA 20 SOM SEKEL I TIFINLV               
108000*                                                                         
108100        MOVE 20                 TO W-HELP-DATUM-SS                        
108200        MOVE W-DATUM-AA         TO W-HELP-DATUM-AA                        
108300        MOVE W-DATUM-VV         TO W-HELP-DATUM-VV                        
108400        COMPUTE W-VECKO-DIFF =                                            
108500               (W-HELP-DATUM-SSAA - W-HELP-TIFINLV-SSAA) * 52 +           
108600               (W-HELP-DATUM-VV   - W-HELP-TIFINLV-VV)                    
108700        IF W-VECKO-DIFF < 52                                              
108800           MOVE ZERO          TO W-OVERLAGER-SDC                          
108900           DISPLAY '    ÖVERLAGER NOLLAS. VECKODIFF < 52'                 
109000        END-IF                                                            
109100                                                                          
109200        COMPUTE W-KVPB-SDC-TOT ROUNDED = W-KVPB-SDC-TOT                   
109300                                                                          
109400        PERFORM IMS-GNP-WDK711-REF                                        
109500     END-PERFORM                                                          
109600                                                                          
109700     MOVE 999999 TO D704-TIERSDAT-PREL-C1                                 
109800     MOVE 99999  TO W-TIERSDAT                                            
109900     IF OMSP-KDERS = 03 OR 06                                             
110000        PERFORM IMS-GU-WDD704                                             
110100        IF SEGMENT-FINNS                                                  
110200           MOVE D704-TIERSDAT-PREL-C1 TO W-TIERSDAT                       
110300        END-IF                                                            
110400     END-IF                                                               
110500                                                                          
110600     .                                                                    
110700     EJECT                                                                
110800                                                                          
110900 BA-KOLLA-OVERLAGERBERAKNING SECTION.                                     
111000     MOVE 'BA-KOLL-OVERLAG ' TO CURRENT-SECTION                           
111100                                                                          
111200     SET DCIX TO +1                                                       
111300     SEARCH DC-TAB                                                        
111400        AT END                                                            
111500*     ---  SDC ÄR EJ REGISTRERAT PÅ WDB6                                  
111600           DISPLAY 'IDDC ' SLAG-IDDC ' EJ REGISTRERAT PÅ WDB6'            
111700        WHEN T-DCS-IDDC (DCIX) = SLAG-IDDC                                
111800*     ---  KOLLA OM ÖVERLAGERBERÄKNING PÅ SDC SKA GÖRAS                   
111900           IF T-DCS-FLOVRLAGBER (DCIX) = JA                               
112000              IF SLAG-KVREFOVL < W-TILLG-SDC                              
112100                 COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC                
112200                                         + W-TILLG-SDC                    
112300                                         - SLAG-KVREFOVL                  
112400              END-IF                                                      
112500           END-IF                                                         
112600     END-SEARCH                                                           
112700     .                                                                    
112800     EJECT                                                                
112900                                                                          
113000 BB-KOLLA-IDLANDX2-WDB601    SECTION.                                     
113100     MOVE 'BB-KOLLA-IDLANDX2-WDB601 '    TO CURRENT-SECTION               
113200                                                                          
113300     SET DCIX TO +1                                                       
113400     SEARCH DC-TAB                                                        
113500        AT END                                                            
113600*     ---  NDC ÄR EJ REGISTRERAT PÅ WDB6                                  
113700           DISPLAY 'NDC IDDC ' OMSP-IDDC ' EJ REGISTRERAT PÅ WDB6'        
113800        WHEN T-DCS-IDDC (DCIX) = OMSP-IDDC                                
113900*     ---  KOLLA VILKET LAND NDC'T HAR.                                   
114000           MOVE T-DCS-IDLANDX2(DCIX) TO W-IDLAND                          
114100     END-SEARCH                                                           
114200     .                                                                    
114300     EJECT                                                                
114400                                                                          
114500 C-KONTROLL-AUT-PLAN SECTION.                                             
114600     MOVE 'C-KOLL-AUT-PLAN ' TO CURRENT-SECTION                           
114700                                                                          
114800     MOVE XLAG-KDLEVPLF      TO SPAR-XLAG-KDLEVPLF                        
114900     MOVE JA                 TO SW-AUT-PLAN                               
115000     MOVE OMSP-IDARTNR       TO W-IDARTNR                                 
115100     MOVE OMSP-IDDC          TO W-IDDC                                    
115200     PERFORM IMS-GU-WDK711                                                
115300                                                                          
115400     PERFORM CA-KOLLA-KDLPORS                                             
115500                                                                          
115600     IF SW-AUT-PLAN = JA                                                  
115700        IF OMSP-KDERS > ZERO                                              
115800           MOVE 'EJ AUT'     TO POSTSUM-FDNAMN                            
115900           MOVE 'ERS KOD'    TO POSTSUM-DDNAMN2                           
116000           MOVE '> 0'        TO POSTSUM-TRANSTYP                          
116100           MOVE 'SS CDC > 0' TO LPF-TELPORSX                              
116200           CALL POSTSUM USING POSTSUM-PARM                                
116300           MOVE NEJ          TO SW-AUT-PLAN                               
116400        END-IF                                                            
116500     END-IF                                                               
116600                                                                          
116700     IF SW-AUT-PLAN = JA                                                  
116800        IF SLAG-KVUTRS > ZERO                                             
116900           MOVE 'EJ AUT'     TO POSTSUM-FDNAMN                            
117000           MOVE 'UTREDN'     TO POSTSUM-DDNAMN2                           
117100           MOVE 'SALD'       TO POSTSUM-TRANSTYP                          
117200           MOVE 'INVEST BAL' TO LPF-TELPORSX                              
117300           CALL POSTSUM USING POSTSUM-PARM                                
117400           MOVE NEJ          TO SW-AUT-PLAN                               
117500        END-IF                                                            
117600     END-IF                                                               
117700                                                                          
117800     IF SW-AUT-PLAN = JA                                                  
117900        IF XLAG-KDLPSP = 3                                                
118000           MOVE 'EJ AUT'     TO POSTSUM-FDNAMN                            
118100           MOVE 'KDLPSP'     TO POSTSUM-DDNAMN2                           
118200           MOVE '= 3 '       TO POSTSUM-TRANSTYP                          
118300           MOVE 'DSBL DATE'  TO LPF-TELPORSX                              
118400           CALL POSTSUM USING POSTSUM-PARM                                
118500           MOVE NEJ          TO SW-AUT-PLAN                               
118600        END-IF                                                            
118700     END-IF                                                               
118800                                                                          
118900     IF SW-AUT-PLAN = JA                                                  
119000        IF XLAG-KVSLUTKP > ZERO                                           
119100           MOVE 'EJ AUT'     TO POSTSUM-FDNAMN                            
119200           MOVE 'SLUTKÖP'    TO POSTSUM-DDNAMN2                           
119300           MOVE 'UPPD'       TO POSTSUM-TRANSTYP                          
119400           MOVE 'FINALPURCH' TO LPF-TELPORSX                              
119500           CALL POSTSUM USING POSTSUM-PARM                                
119600           MOVE NEJ          TO SW-AUT-PLAN                               
119700        END-IF                                                            
119800     END-IF                                                               
119900                                                                          
120000     IF SW-AUT-PLAN = JA                                                  
120100        IF XLAG-KDLEVPLF = 'N' OR 'S' OR 'G' OR 'P'                       
120200           MOVE 'EJ AUT'      TO POSTSUM-FDNAMN                           
120300           MOVE 'STOPPAD'     TO POSTSUM-DDNAMN2                          
120400           MOVE XLAG-KDLEVPLF TO POSTSUM-TRANSTYP                         
120500           MOVE SPACE         TO LPF-TELPORSX                             
120600           MOVE 'STOP'        TO LPF-TELPORSX(1:4)                        
120700           MOVE XLAG-KDLEVPLF TO LPF-TELPORSX(6:1)                        
120800           CALL POSTSUM USING POSTSUM-PARM                                
120900           MOVE NEJ           TO SW-AUT-PLAN                              
121000        END-IF                                                            
121100        IF XLAG-KDLEVPLF = 'N'                                            
121200           MOVE JA            TO SPAR-XLAG-KDLEVPLF                       
121300        END-IF                                                            
121400     END-IF                                                               
121500                                                                          
121600     .                                                                    
121700                                                                          
121800                                                                          
121900 CA-KOLLA-KDLPORS SECTION.                                                
122000     MOVE 'CA-KOLLA-KDLPORS' TO CURRENT-SECTION                           
122100                                                                          
122200     MOVE SPACE              TO LPF-TELPORSX                              
122300                                                                          
122400     MOVE 3                  TO IX-ORS                                    
122500     PERFORM UNTIL IX-ORS < 1                                             
122600        IF OMSP-KDLPORS-TAB (IX-ORS) = 09 OR 11 OR 21                     
122700           MOVE 'EJ AUT'             TO POSTSUM-FDNAMN                    
122800           MOVE 'ORS.KOD'            TO POSTSUM-DDNAMN2                   
122900                                                                          
123000           EVALUATE OMSP-KDLPORS-TAB (IX-ORS)                             
123100              WHEN 09                                                     
123200                   MOVE 'INLÄ'       TO POSTSUM-TRANSTYP                  
123300                   MOVE 'NEW PUBW'   TO LPF-TELPORSX                      
123400                                                                          
123500              WHEN 11                                                     
123600                   MOVE 'LEVB'       TO POSTSUM-TRANSTYP                  
123700                   MOVE 'NEW SUPP'   TO LPF-TELPORSX                      
123800                                                                          
123900              WHEN 21                                                     
124000                   MOVE 'REGE'       TO POSTSUM-TRANSTYP                  
124100                   MOVE 'SS CDC > 0' TO LPF-TELPORSX                      
124200           END-EVALUATE                                                   
124300           MOVE NEJ                  TO SW-AUT-PLAN                       
124400        END-IF                                                            
124500        SUBTRACT 1 FROM IX-ORS                                            
124600     END-PERFORM                                                          
124700                                                                          
124800     IF SW-AUT-PLAN = NEJ                                                 
124900        CALL POSTSUM USING POSTSUM-PARM                                   
125000     ELSE                                                                 
125100        MOVE 3 TO IX-ORS                                                  
125200        PERFORM UNTIL IX-ORS < 1                                          
125300           IF OMSP-KDLPORS-TAB (IX-ORS) = 14                              
125400              IF SW-AUT-PLAN = JA                                         
125500                 MOVE 'EJ AUT'     TO POSTSUM-FDNAMN                      
125600                 MOVE 'ORS.KOD'    TO POSTSUM-DDNAMN2                     
125700                 MOVE 'NYSÄ'       TO POSTSUM-TRANSTYP                    
125800                 MOVE 'NEW SEASON' TO LPF-TELPORSX                        
125900                 MOVE NEJ          TO SW-AUT-PLAN                         
126000              END-IF                                                      
126100           END-IF                                                         
126200           SUBTRACT 1 FROM IX-ORS                                         
126300        END-PERFORM                                                       
126400                                                                          
126500        IF SW-AUT-PLAN = NEJ                                              
126600           CALL POSTSUM USING POSTSUM-PARM                                
126700        ELSE                                                              
126800           MOVE 3 TO IX-ORS                                               
126900           PERFORM UNTIL IX-ORS < 1                                       
127000              IF OMSP-KDLPORS-TAB (IX-ORS) = 15                           
127100                 IF SW-AUT-PLAN = JA                                      
127200                    MOVE 'EJ AUT'     TO POSTSUM-FDNAMN                   
127300                    MOVE 'ORS.KOD'    TO POSTSUM-DDNAMN2                  
127400                    MOVE 'ANBY'       TO POSTSUM-TRANSTYP                 
127500                    MOVE 'NEW PROC'   TO LPF-TELPORSX                     
127600                    CALL POSTSUM USING POSTSUM-PARM                       
127700                    MOVE NEJ          TO SW-AUT-PLAN                      
127800                 END-IF                                                   
127900              END-IF                                                      
128000              SUBTRACT 1 FROM IX-ORS                                      
128100           END-PERFORM                                                    
128200        END-IF                                                            
128300     END-IF                                                               
128400     .                                                                    
128500                                                                          
128600                                                                          
128700 D-BEHANDLA-LEVDATA SECTION.                                              
128800     MOVE 'D-BEH-LEVDATA   ' TO CURRENT-SECTION                           
128900                                                                          
129000     MOVE OMSP-IDARTNR         TO W-IDARTNR-D9                            
129100     MOVE OMSP-IDDC            TO W-IDDC-D9                               
129200     MOVE OMSP-KDLPORS-TAB (1) TO LPF-KDLPORS (1)                         
129300     MOVE OMSP-KDLPORS-TAB (2) TO LPF-KDLPORS (2)                         
129400     MOVE OMSP-KDLPORS-TAB (3) TO LPF-KDLPORS (3)                         
129500                                                                          
129600     PERFORM IMS-GU-WDD901                                                
129700     IF SEGMENT-FINNS                                                     
129800        PERFORM IMS-GU-WDD902                                             
129900     END-IF                                                               
130000                                                                          
130100     IF SEGMENT-SAKNAS                                                    
130200        MOVE ZERO            TO SPAR-D904-DASPECST                        
130300                                SPAR-D904-KDLPORS-TAB (1)                 
130400                                SPAR-D904-KDLPORS-TAB (2)                 
130500                                SPAR-D904-KDLPORS-TAB (3)                 
130600     ELSE                                                                 
130700                                                                          
130800        PERFORM IMS-GU-WDD904                                             
130900        IF SEGMENT-FINNS                                                  
131000           MOVE D904-DASPECST       TO SPAR-D904-DASPECST                 
131100           MOVE D904-KDLPORS-TAB(1) TO SPAR-D904-KDLPORS-TAB (1)          
131200           MOVE D904-KDLPORS-TAB(2) TO SPAR-D904-KDLPORS-TAB (2)          
131300           MOVE D904-KDLPORS-TAB(3) TO SPAR-D904-KDLPORS-TAB (3)          
131400           MOVE D904-KVBEST-PL      TO SPAR-KVBEST-PL                     
131500           MOVE D904-KDPLKOEP       TO SPAR-KDPLKOEP                      
131600        ELSE                                                              
131700           MOVE ZERO                TO SPAR-D904-DASPECST                 
131800                                       SPAR-D904-KDLPORS-TAB (1)          
131900                                       SPAR-D904-KDLPORS-TAB (2)          
132000                                       SPAR-D904-KDLPORS-TAB (3)          
132100                                       SPAR-KVBEST-PL                     
132200                                       SPAR-KDPLKOEP                      
132300        END-IF                                                            
132400     END-IF                                                               
132500     .                                                                    
132600                                                                          
132700                                                                          
132800 E-TYP-AV-OMSPEC SECTION.                                                 
132900     MOVE 'E-TYP-AV-OMSPEC ' TO CURRENT-SECTION                           
133000                                                                          
133100     MOVE OMSP-IDARTNR       TO W-IDARTNR                                 
133200     MOVE OMSP-IDDC          TO W-IDDC                                    
133300     PERFORM IMS-GU-WDK722                                                
133400                                                                          
133500     MOVE NEJ TO SW-OPTIMAL-OMSPEC                                        
133600                 SW-X-OPT                                                 
133700                                                                          
133800     IF OMSP-KDLPORS-TAB (1) = 20                                         
133900     OR OMSP-KDLPORS-TAB (2) = 20                                         
134000     OR OMSP-KDLPORS-TAB (3) = 20                                         
134100         MOVE JA TO SW-X-OPT                                              
134200     END-IF                                                               
134300                                                                          
134400     IF XLAG-KDOPPLAN = JA  OR                                            
134500       (XLAG-KDOPPLAN = 'X' AND                                           
134600         SW-X-OPT = JA)                                                   
134700                                                                          
134800        MOVE JA TO SW-OPTIMAL-OMSPEC                                      
134900                                                                          
135000     END-IF                                                               
135100                                                                          
135200     IF OMSP-KDERS = 09 OR 19 OR 29                                       
135300        MOVE JA  TO SW-KDERS-X9                                           
135400     ELSE                                                                 
135500        MOVE NEJ TO SW-KDERS-X9                                           
135600     END-IF                                                               
135700                                                                          
135800     IF OMSP-KDLPORS-TAB (1) = 03                                         
135900     OR OMSP-KDLPORS-TAB (2) = 03                                         
136000     OR OMSP-KDLPORS-TAB (3) = 03                                         
136100        MOVE JA  TO SW-KDLPORS-03                                         
136200     ELSE                                                                 
136300        MOVE NEJ TO SW-KDLPORS-03                                         
136400     END-IF                                                               
136500     .                                                                    
136600                                                                          
136700                                                                          
136800 F-BORTTAG-OMSPEC SECTION.                                                
136900     MOVE 'F-BORTTAG-OMSPEC' TO CURRENT-SECTION                           
137000                                                                          
137100     PERFORM S100-NOLLA-W22415-AREA                                       
137200     MOVE BORTTAG-OMSPEC     TO UPLP-IDPTYP                               
137300     MOVE OMSP-IDARTNR       TO UPLP-IDARTNR                              
137400     MOVE OMSP-IDDC          TO UPLP-IDDC                                 
137500                                                                          
137600     PERFORM S12-SKRIV-W22415                                             
137700                                                                          
137800                                                                          
137900     MOVE BORTTAG-FORSLAG    TO UPLP-IDPTYP                               
138000     MOVE 1                  TO UPLP-KDAVROP                              
138100                                                                          
138200     PERFORM S12-SKRIV-W22415                                             
138300     .                                                                    
138400                                                                          
138500                                                                          
138600 G-SPARA-LEVSPEC-FORSLAG SECTION.                                         
138700     MOVE 'G-SPARA-FORSLAG ' TO CURRENT-SECTION                           
138800                                                                          
138900*    MOVE OMSP-KDLPORS-TAB (1) TO SPAR-D904-KDLPORS-TAB (1)               
139000*    MOVE 26                   TO SPAR-D904-KDLPORS-TAB (2)               
139100*    MOVE 27                   TO SPAR-D904-KDLPORS-TAB (3)               
139200     MOVE OMSP-KDLPORS-TAB (1) TO LPF-KDLPORS (1)                         
139300     MOVE 26                   TO LPF-KDLPORS (2)                         
139400     MOVE 27                   TO LPF-KDLPORS (3)                         
139500     IF XLAG-KDLPSP = 5                                                   
139600        MOVE ZERO              TO SPAR-XLAG-KDLPSP                        
139700     ELSE                                                                 
139800        MOVE XLAG-KDLPSP       TO SPAR-XLAG-KDLPSP                        
139900     END-IF                                                               
140000                                                                          
140100     MOVE NEJ                  TO SW-OMSPEC-UTFOERD                       
140200                                  SW-SKIP-FORSLAG                         
140300     .                                                                    
140400                                                                          
140500                                                                          
140600 H-SKAPA-OMSPEC-LEVPLAN SECTION.                                          
140700     MOVE 'H-SKAPA-OMSPEC  ' TO CURRENT-SECTION                           
140800                                                                          
140900     MOVE NEJ          TO SW-SKIP-FORSLAG                                 
141000     MOVE JA           TO SW-WDF1                                         
141100                                                                          
141200     PERFORM HC-SKAPA-BEHOVSTABELL                                        
141300     PERFORM HA-INITIERA-OMSPEC                                           
141400     MOVE WS-DASPECST-AAVV  TO WS-DASPECST-YYVV                           
141500     MOVE 1                 TO WS-DASPECST-D                              
141600     IF KDERS-X9 OR KDLPORS-03                                            
141700     OR D704-TIERSDAT-PREL-C1 < WS-DASPECST-AAVVD                         
141800        MOVE JA TO SW-OMSPEC-UTFOERD                                      
141900     ELSE                                                                 
142000        PERFORM HB-SKAPA-TILLGANGSTABELL                                  
142100        PERFORM HD-BERAKNA-TILLG-I-SPECVECKA                              
142200        PERFORM HE-SPEC-NYTT-FOERSLAG                                     
142300                                                                          
142400        MOVE JA TO SW-OMSPEC-UTFOERD                                      
142500     END-IF                                                               
142600                                                                          
142700     MOVE 5 TO SPAR-XLAG-KDLPSP                                           
142800     MOVE W-DATUM-AAVV-AKT TO SPAR-XLAG-TIOMSPEC                          
142900                              SPAR-XLAG-TILPSP                            
143000                                                                          
143100     COMPUTE W-ARSOMS = 12                                                
143200          * (SLAG-KVPB-REF + SLAG-KVPBREOI)                               
143300          *  LART-PRMATRL                                                 
143400                                                                          
143500     IF W-ARSOMS > W-ARSOMS-100000                                        
143600        MOVE 1 TO W-ANTAL-VECKOR                                          
143700     ELSE                                                                 
143800        MOVE 3 TO W-ANTAL-VECKOR                                          
143900     END-IF                                                               
144000                                                                          
144100     CALL W009VADD USING SPAR-XLAG-TILPSP W-ANTAL-VECKOR                  
144200     .                                                                    
144300                                                                          
144400                                                                          
144500 HA-INITIERA-OMSPEC SECTION.                                              
144600     MOVE 'HA-INIT-OMSPEC  ' TO CURRENT-SECTION                           
144700                                                                          
144800     MOVE ZERO                 TO W-KVBEST-PL                             
144900                                  W-KDPLKOEP                              
145000                                  W-TILLG-SPAR                            
145100                                                                          
145200     COMPUTE W-TILLG-SPAR = SLAG-KVLS                                     
145300                         + SLAG-KVAKS-SDC                                 
145400                         + SLAG-KVAKS-PAV                                 
145500                                                                          
145600                         - SLAG-KVRESS                                    
145700                         - SLAG-KVROS-DAG                                 
145800                         - SLAG-KVROS-BULK                                
145900                         - SLAG-KVOKS-DAG                                 
146000                         - SLAG-KVOKS-BULK                                
146100                         + W-OVERLAGER-SDC                                
146200                                                                          
146300     IF LART-DAPUBL > ZERO                                                
146400*WZ20DAYS                                                                 
146500        MOVE LART-DAPUBL(3:6)     TO DAYS-TIDATE1                         
146600        MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                       
146700        MOVE 'YYWWD'              TO DAYS-KDDATFMT2                       
146800        MOVE 0                    TO DAYS-KVDAYS                          
146900        MOVE SPACE                TO DAYS-TIDATE2                         
147000                                     DAYS-IDCALEND                        
147100        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
147200*                                                                         
147300        IF DAYS-KDRC = 8                                                  
147400          MOVE 'FEL VID ANROP TILL WZ20DAYS 3' TO FELTEXT                 
147500          MOVE 32                 TO RKOD                                 
147600          CALL ABEND USING RKOD                                           
147700        ELSE                                                              
147800          MOVE DAYS-TIDATE2(1:5)  TO W-TIFINLV-AAVVD                      
147900        END-IF                                                            
148000     ELSE                                                                 
148100        MOVE OMSP-TIFINLV         TO W-TIFINLV-AAVVD                      
148200     END-IF                                                               
148300*                                                                         
148400***  PUBWEEK IS LEADTIME ADJUSTED IN DEMAND MODULE.                       
148500***  IF PUBWEEK IS IN FUTURE, CHECK THE WEEK FOR FIRST DEMAND             
148600***  SO THE CALL-OFFS SHOULD BE FROM LEADTIME + CURRENT WEEK.             
148700*                                                                         
148800     MOVE W-DATUM-AAVV-AKT        TO TMP1-YYWW                            
148900     MOVE W-TIFINLV-AAVV          TO TMP2-YYWW                            
149000     PERFORM WY2000P3                                                     
149100     IF TMP2-YYWW     > TMP1-YYWW                                         
149200        MOVE 1                    TO IX-L                                 
149300        PERFORM UNTIL IX-L > 156                                          
149400        OR BHDC-KVBEHOV-VECKA (IX-L) > ZERO                               
149500          ADD 1                   TO IX-L                                 
149600        END-PERFORM                                                       
149700*                                                                         
149800*      CHECK IF DEMAND EXISTS IN THE FIRST WEEK OR                        
149900*      IF WE HAVE NEGATIVE ASSETS LIKE IN CASE OF BACKORDER               
150000*      BASED ON IX-L VALUE WE ADJUST THE DELIVERY PLAN WEEK               
150100*                                                                         
150200        IF BHDC-KVBEHOV-DESSUTOM > ZERO                                   
150300        OR W-TILLG-SPAR < ZERO                                            
150400           INITIALIZE                IX-L                                 
150500           MOVE 1                 TO IX-L                                 
150600        END-IF                                                            
150700        IF IX-L > 156                                                     
150800           CONTINUE                                                       
150900        ELSE                                                              
151000           MOVE IX-L                 TO W-ANTAL-VECKOR                    
151100           MOVE W-DATUM-AAVV-AKT     TO W-AAVV-ADD                        
151200           CALL W009VADD USING W-AAVV-ADD W-ANTAL-VECKOR                  
151300           MOVE W-AAVV-ADD           TO W-TISPECST-ADJ                    
151400        END-IF                                                            
151500     END-IF                                                               
151600*                                                                         
151700     IF  SW-OPTIMAL-OMSPEC = NEJ                                          
151800         MOVE W-DATUM-AAVV-AKT    TO W-TISPECST-DISP                      
151900         MOVE XLAG-KVVECKOR-FT    TO W-ANTAL-VECKOR                       
152000         ADD 1                    TO W-ANTAL-VECKOR                       
152100         CALL W009VADD USING W-TISPECST-DISP W-ANTAL-VECKOR               
152200         MOVE W-DATUM-AAVV-AKT    TO W-TISPECST                           
152300         MOVE XLAG-KVVECKOR-LT    TO W-ANTAL-VECKOR                       
152400         ADD 1 TO W-ANTAL-VECKOR                                          
152500         CALL W009VADD USING W-TISPECST W-ANTAL-VECKOR                    
152600     ELSE                                                                 
152700         MOVE W-DATUM-AAVV-AKT    TO W-TISPECST-DISP                      
152800         MOVE XLAG-KVVECKOR-FT    TO W-ANTAL-VECKOR                       
152900         ADD  2                   TO W-ANTAL-VECKOR                       
153000         SUBTRACT XLAG-KVVECKOR-LT FROM W-ANTAL-VECKOR                    
153100         CALL W009VADD USING W-TISPECST-DISP  W-ANTAL-VECKOR              
153200         MOVE W-DATUM-AAVV-AKT    TO W-TISPECST                           
153300         MOVE 2                   TO W-ANTAL-VECKOR                       
153400         CALL W009VADD USING W-TISPECST  W-ANTAL-VECKOR                   
153500     END-IF                                                               
153600*                                                                         
153700*    IF THE DELIERY PLAN WEEK CALCULATES IS LESS THAN                     
153800*    ADJUSTED DELIVERY PLAN WEEK CALUCATED ABOVE (BASED ON                
153900*    DEMAND FROM THE DEMAND DEMAND MODULE),                               
154000*    USE THE ADJUSTED DELIVERY PLAN START WEEK                            
154100*                                                                         
154200     MOVE W-DATUM-AAVV-AKT         TO TMP1-YYWW                           
154300     MOVE W-TIFINLV-AAVV           TO TMP2-YYWW                           
154400     PERFORM WY2000P3                                                     
154500     IF TMP1-YYWW + 1 <  TMP2-YYWW                                        
154600        MOVE W-TISPECST-DISP       TO TMP1-YYWW                           
154700        MOVE W-TISPECST-ADJ        TO TMP2-YYWW                           
154800        PERFORM WY2000P3                                                  
154900        IF TMP1-YYWW <  TMP2-YYWW                                         
155000           MOVE W-TISPECST-ADJ     TO W-TISPECST-DISP                     
155100        END-IF                                                            
155200     END-IF                                                               
155300                                                                          
155400     MOVE W-DATUM-AAVV-AKT    TO W-DATUM-FROM                             
155500     MOVE W-TISPECST-DISP     TO W-DATUM-TOM                              
155600     PERFORM S102-BERAKNA-VECKODIFFERENS                                  
155700                                                                          
155800     COMPUTE W-KVVECKOR-FFH ROUNDED = XLAG-KVDAGAR-FFH / 5                
155900     COMPUTE W-KVVECKOR-SPEC = 52 - XLAG-KVVECKOR-LT                      
156000                                                                          
156100                                                                          
156200     IF W-KVVECKOR-SPEC < 10                                              
156300        MOVE 10 TO W-KVVECKOR-SPEC                                        
156400     END-IF                                                               
156500                                                                          
156600*    OM ARTIKELN BLIR ERSATT UNDER SPECPERIODEN                           
156700*    FÅR VI KORTA AV PERIODEN LITE GRANN                                  
156800                                                                          
156900     MOVE W-TISPECST      TO W-TISPEC-TOT                                 
157000     MOVE W-KVVECKOR-SPEC TO W-KVVECKOR-ADD                               
157100     CALL W009VADD USING W-TISPEC-TOT W-KVVECKOR-ADD                      
157200                                                                          
157300     IF W-TISPEC-TOT  > W-TIERSDAT-AAVV                                   
157400        IF W-TISPECST < W-TIERSDAT-AAVV                                   
157500*WZ20DAYS                                                                 
157600           MOVE W-TIERSDAT-AAVV   TO DAYS-TIDATE1                         
157700           MOVE 'YYWW'            TO DAYS-KDDATFMT1                       
157800           MOVE 'YYMMDD'          TO DAYS-KDDATFMT2                       
157900           MOVE 0                 TO DAYS-KVDAYS                          
158000           MOVE SPACE             TO DAYS-TIDATE2                         
158100                                     DAYS-IDCALEND                        
158200           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
158300*                                                                         
158400           IF DAYS-KDRC = 8                                               
158500             MOVE 'FEL VID ANROP TILL WZ20DAYS 4' TO FELTEXT              
158600             CALL ABEND USING RKOD                                        
158700           ELSE                                                           
158800*WZ20DAYS                                                                 
158900             MOVE DAYS-TIDATE2(1:6) TO DAG-TIAAMMDD-TOM                   
159000                                                                          
159100*W-TISPECST                                                               
159200             MOVE ZERO               TO WS-TISPECST-DAYS-AAVV             
159300             MOVE W-TISPECST         TO WS-TISPECST-DAYS-AAVV             
159400             MOVE WS-TISPECST-DAYS-AAVV                                   
159500                                     TO DAYS-TIDATE1                      
159600             MOVE 'YYWW'             TO DAYS-KDDATFMT1                    
159700             MOVE 'YYMMDD'           TO DAYS-KDDATFMT2                    
159800             MOVE 0                  TO DAYS-KVDAYS                       
159900             MOVE SPACE              TO DAYS-TIDATE2                      
160000             MOVE SPACE              TO DAYS-IDCALEND                     
160100             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
160200*                                                                         
160300              IF DAYS-KDRC = 8                                            
160400                MOVE 'FEL VID ANROP TILL WZ20DAYS 5' TO FELTEXT           
160500                CALL ABEND USING RKOD                                     
160600              ELSE                                                        
160700                 MOVE DAYS-TIDATE2(1:6) TO DAG-TIAAMMDD-FOM               
160800                 MOVE 001          TO DAG-KDCALL                          
160900                                                                          
161000                 CALL WDAGKONV USING DAG-KDCALL,                          
161100                                     DAG-DATUM-AREA,                      
161200                                     DAG-KDSVAR                           
161300                 IF DAG-KDSVAR = SPACE                                    
161400                    COMPUTE W-KVVECKOR-SPEC = DAG-KVKALDAG / 7            
161500                 ELSE                                                     
161600                    MOVE 'FEL VID ANROP TILL DAGKONV 3'                   
161700                                   TO FELTEXT                             
161800                    CALL FELLOG                                           
161900                 END-IF                                                   
162000              END-IF                                                      
162100           END-IF                                                         
162200        ELSE                                                              
162300           MOVE ZERO             TO W-KVVECKOR-SPEC                       
162400        END-IF                                                            
162500     END-IF                                                               
162600                                                                          
162700     MOVE ZERO TO W-TILLG                                                 
162800                                                                          
162900     MOVE 1    TO TILLGTAB-IX                                             
163000     PERFORM UNTIL                                                        
163100       (TILLGTAB-IX > TILLGTAB-IX-MAX)                                    
163200         MOVE ZERO TO TILLGTAB-ANTAL (TILLGTAB-IX)                        
163300         ADD 1 TO TILLGTAB-IX                                             
163400     END-PERFORM                                                          
163500                                                                          
163600*WZ20DAYS                                                                 
163700     MOVE ZERO                    TO WS-TISPECST-DAYS-AAVV                
163800     MOVE W-TISPECST              TO WS-TISPECST-DAYS-AAVV                
163900     MOVE WS-TISPECST-DAYS-AAVV   TO DAYS-TIDATE1                         
164000     MOVE 'YYWW'                  TO DAYS-KDDATFMT1                       
164100     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
164200     MOVE 0                       TO DAYS-KVDAYS                          
164300     MOVE SPACE                   TO DAYS-TIDATE2                         
164400                               DAYS-IDCALEND                              
164500     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
164600*                                                                         
164700     IF DAYS-KDRC = 8                                                     
164800       MOVE 'FEL VID ANROP TILL WZ20DAYS 6' TO FELTEXT                    
164900       CALL ABEND USING RKOD                                              
165000     ELSE                                                                 
165100       MOVE DAYS-TIDATE2(1:6)  TO W-DASPECST-AAMMDD                       
165200     END-IF                                                               
165300     MOVE W-TISPECST   TO WS-DASPECST-AAVV                                
165400     MOVE 20           TO WS-DASPECST-SS                                  
165500     .                                                                    
165600     EJECT                                                                
165700                                                                          
165800 HB-SKAPA-TILLGANGSTABELL SECTION.                                        
165900     MOVE 'HB-SKAPA-TILLG-T' TO CURRENT-SECTION                           
166000******************************************************************        
166100*                                                                *        
166200*    TABELL MED INLEVERANSER PLACERADE I RESPEKTIVE VECKA        *        
166300*                                                                *        
166400******************************************************************        
166500                                                                          
166600     MOVE OMSP-IDARTNR    TO W-IDARTNR-D9                                 
166700     MOVE OMSP-IDDC       TO W-IDDC-D9                                    
166800     MOVE 2               TO W-KDAVROP                                    
166900                                                                          
167000     PERFORM IMS-GU-WDD901                                                
167100     IF SEGMENT-FINNS                                                     
167200        PERFORM IMS-GNP-WDD905-F                                          
167300     END-IF                                                               
167400                                                                          
167500     MOVE W-TISPECST      TO W-GRAENS-AVROP                               
167600     MOVE W-KVVECKOR-SPEC TO W-ANTAL-VECKOR                               
167700     CALL W009VADD USING W-GRAENS-AVROP W-ANTAL-VECKOR                    
167800                                                                          
167900     PERFORM UNTIL SEGMENT-SAKNAS                                         
168000                                                                          
168100         MOVE D905-DAAVROP-AVS     TO W-DAAVROP-AVS                       
168200                                                                          
168300         IF (WDD9-KEY-02-IDLEVNR = SLAG-IDLEVNR                           
168400          AND W-DAAVROP-AVS-AAVV < W-TISPECST)                            
168500         OR  (WDD9-KEY-02-IDLEVNR NOT = SLAG-IDLEVNR                      
168600          AND W-DAAVROP-AVS-AAVV < W-GRAENS-AVROP)                        
168700                                                                          
168800*WZ20DAYS                                                                 
168900             MOVE D905-TIAVRDAT-DISP TO WS-DAYS-TIAVRDAT-DISP             
169000             MOVE WS-DAYS-TIAVRDAT-DISP TO DAYS-TIDATE1                   
169100             MOVE 'YYMMDD'           TO DAYS-KDDATFMT1                    
169200             MOVE 'YYWW'             TO DAYS-KDDATFMT2                    
169300             MOVE 0                  TO DAYS-KVDAYS                       
169400             MOVE SPACE              TO DAYS-TIDATE2                      
169500                                        DAYS-IDCALEND                     
169600             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
169700*                                                                         
169800             IF DAYS-KDRC = 8                                             
169900               MOVE 'FEL VID ANROP TILL WZ20DAYS 7' TO FELTEXT            
170000               CALL ABEND USING RKOD                                      
170100             ELSE                                                         
170200               MOVE DAYS-TIDATE2(1:4) TO W-AAVV                           
170300             END-IF                                                       
170400             IF  W-AAVV < W-TISPECST-DISP                                 
170500                 ADD D905-KVAVROP      TO W-TILLG                         
170600             ELSE                                                         
170700                 MOVE W-DATUM-AAVV-AKT TO W-DATUM-FROM                    
170800                 MOVE W-AAVV           TO W-DATUM-TOM                     
170900                 PERFORM S102-BERAKNA-VECKODIFFERENS                      
171000                                                                          
171100                 IF W-VECKO-DIFF > ZERO                                   
171200                    MOVE W-VECKO-DIFF  TO TILLGTAB-IX                     
171300                 ELSE                                                     
171400                    MOVE +1            TO TILLGTAB-IX                     
171500                 END-IF                                                   
171600                 ADD D905-KVAVROP                                         
171700                             TO TILLGTAB-ANTAL (TILLGTAB-IX)              
171800              END-IF                                                      
171900         END-IF                                                           
172000                                                                          
172100         PERFORM IMS-GNP-WDD905-N                                         
172200     END-PERFORM                                                          
172300     .                                                                    
172400     EJECT                                                                
172500                                                                          
172600 HC-SKAPA-BEHOVSTABELL SECTION.                                           
172700     MOVE 'HC-SKAPA-BEHOV-T' TO CURRENT-SECTION                           
172800                                                                          
172900     MOVE PB-TOTAL-SEP-LEV-XDC  TO BHDC-KDBEHOV                           
173000     MOVE OMSP-IDARTNR          TO BHDC-IDARTNR                           
173100     MOVE OMSP-IDDC             TO BHDC-IDDC                              
173200     MOVE W-DATUM-AAVV-AKT      TO BHDC-TIAAVV-AKTUELL                    
173300                                   BHDC-TIBEHOV-START                     
173400     MOVE 1 TO W-ANTAL-VECKOR                                             
173500     CALL W009VADD USING BHDC-TIBEHOV-START W-ANTAL-VECKOR                
173600                                                                          
173700     MOVE 156                   TO BHDC-KVVECKOR-BEHOV                    
173800     MOVE +6                    TO BHDC-TID-AKTUELL                       
173900     MOVE ZERO                  TO BHDC-KVTILLG-TOT-CDC                   
174000                                                                          
174100     IF OMSP-KDERS-UTG = 0                                                
174200       CALL W222BHDC USING BHDC-W222BHDC                                  
174300                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
174400                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
174500                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
174600                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
174700                           BHDC-REFL1-2501-PCB                            
174800                           BHDC-REFL1-WDB6-PCB                            
174900                           BHDC-REFL1-WDK7-PCB                            
175000                           BHDC-REFL1-UTIL-WDK6-PCB                       
175100                           BHDC-REFL1-UTIL-WDK7-PCB                       
175200                           BHDC-REFL1-UTIL-WDB6-PCB                       
175300                           BHDC-REFL2-2501-PCB                            
175400                           BHDC-REFL2-WDB6-PCB                            
175500                           BHDC-REFL2-UTIL-WDK6-PCB                       
175600                           BHDC-REFL2-UTIL-WDK7-PCB                       
175700                           BHDC-REFL2-UTIL-WDB6-PCB                       
175800                           BHDC-UTIL-WDK6-PCB                             
175900                           BHDC-UTIL-WDK7-PCB                             
176000                           BHDC-UTIL-WDB6-PCB                             
176100                           BHDC-W222-WDK6-PCB                             
176200                           BHDC-W222-WDK7-PCB                             
176300                           BHDC-W222-ARTM-PCB                             
176400                           BHDC-W222-2501-PCB                             
176500                           BHDC-W222-WDB6R-PCB                            
176600                           BHDC-W222-WDK7R-PCB                            
176700                           BHDC-W222-WDB6-PCB                             
176800                           BHDC-W222-WDD7-PCB                             
176900                           BHDC-W222-WDK7E-PCB                            
177000                           BHDC-W222-UTIL-WDK6-PCB                        
177100                           BHDC-W222-UTIL-WDK7-PCB                        
177200                           BHDC-W222-UTIL-WDB6-PCB                        
177300                           BHDC-W222-UTUP-WDK7-PCB                        
177400                           BHDC-W222-UTUP-WDB6-PCB                        
177500                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
177600                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
177700                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
177800                           BHDC-UTUP-WDK7-PCB                             
177900                           BHDC-UTUP-WDB6-PCB                             
178000                           BHDC-UTUP-UTIL-WDK6-PCB                        
178100                           BHDC-UTUP-UTIL-WDK7-PCB                        
178200                           BHDC-UTUP-UTIL-WDB6-PCB                        
178300                                                                          
178400       IF BHDC-FLJANEJ-ANROP = 'N'                                        
178500         PERFORM S20-NOLLA-BHDC-RESULTATFLT                               
178600       END-IF                                                             
178700     ELSE                                                                 
178800       PERFORM S20-NOLLA-BHDC-RESULTATFLT                                 
178900     END-IF                                                               
179000                                                                          
179100     .                                                                    
179200                                                                          
179300                                                                          
179400 HD-BERAKNA-TILLG-I-SPECVECKA SECTION.                                    
179500     MOVE 'Z-BER-TILLG-I-SV' TO CURRENT-SECTION                           
179600                                                                          
179700     COMPUTE W-TILLG-BER = SLAG-KVLS                                      
179800                         + SLAG-KVAKS-SDC                                 
179900                         + SLAG-KVAKS-PAV                                 
180000                                                                          
180100                         - SLAG-KVRESS                                    
180200                         - SLAG-KVROS-DAG                                 
180300                         - SLAG-KVROS-BULK                                
180400                         - SLAG-KVOKS-DAG                                 
180500                         - SLAG-KVOKS-BULK                                
180600                         + W-OVERLAGER-SDC                                
180700                                                                          
180800     ADD W-TILLG-BER         TO W-TILLG                                   
180900     SUBTRACT BHDC-KVBEHOV-DESSUTOM                                       
181000                           FROM W-TILLG                                   
181100                                                                          
181200     MOVE BHDC-TIBEHOV-START TO W-DATUM-FROM                              
181300     MOVE W-TISPECST-DISP    TO W-DATUM-TOM                               
181400     PERFORM S102-BERAKNA-VECKODIFFERENS                                  
181500                                                                          
181600     IF  W-VECKO-DIFF > IX-BEHOV-MAX                                      
181700        MOVE IX-BEHOV-MAX    TO W-VECKO-DIFF                              
181800     END-IF                                                               
181900                                                                          
182000     MOVE 1 TO TILLGTAB-IX                                                
182100     PERFORM UNTIL TILLGTAB-IX > W-VECKO-DIFF                             
182200         SUBTRACT BHDC-KVBEHOV-VECKA (TILLGTAB-IX)                        
182300                         FROM W-TILLG                                     
182400         ADD 1 TO TILLGTAB-IX                                             
182500     END-PERFORM                                                          
182600                                                                          
182700     MOVE W-TILLG TO W-SUM-START                                          
182800     .                                                                    
182900     EJECT                                                                
183000                                                                          
183100 HE-SPEC-NYTT-FOERSLAG SECTION.                                           
183200     MOVE 'HE-SPEC-NYTT-FOR' TO CURRENT-SECTION                           
183300                                                                          
183400******************************************************************        
183500*                                                                *        
183600*    SPEC AV NYTT FÖRSLAG FRÅN W-TISPECST DATUM UNDER            *        
183700*    W-KVVECKOR-SPEC VECKOR                                      *        
183800*                                                                *        
183900******************************************************************        
184000     SKIP1                                                                
184100                                                                          
184200     MOVE JA  TO SW-FORSLAG-AVROP-SAKNAS                                  
184300                                                                          
184400     MOVE XLAG-KVSLAGER     TO W-BUFF                                     
184500                                                                          
184600     PERFORM HEA-KOLL-2AAR                                                
184700                                                                          
184800     MOVE W-DATUM-AAVV-AKT TO W-DATUM-FROM                                
184900     MOVE W-TISPECST-DISP  TO W-DATUM-TOM                                 
185000     PERFORM S102-BERAKNA-VECKODIFFERENS                                  
185100                                                                          
185200     IF W-VECKO-DIFF < ZERO                                               
185300       MOVE +1 TO W-VECKO-DIFF                                            
185400       DISPLAY OMSP-IDARTNR ' vdiff < 0'                                  
185500     END-IF                                                               
185600                                                                          
185700     MOVE W-VECKO-DIFF    TO TILLGTAB-IX                                  
185800     ADD  W-KVVECKOR-SPEC TO W-VECKO-DIFF                                 
185900                                                                          
186000     IF W-VECKO-DIFF > TILLGTAB-IX-MAX                                    
186100       MOVE TILLGTAB-IX-MAX  TO W-VECKO-DIFF                              
186200     END-IF                                                               
186300                                                                          
186400     MOVE JA  TO SW-FOERSTA-AVROPSVECKA                                   
186500                                                                          
186600     COMPUTE W-ARSOMS = 12                                                
186700                      * ( SLAG-KVPB-REF                                   
186800                        + SLAG-KVPBREOI)                                  
186900                      *   LART-PRMATRL                                    
187000                                                                          
187100     IF W-ARSOMS > W-ARSOMS-100000                                        
187200        MOVE 1  TO W-Q-FREKV-MAX                                          
187300     ELSE                                                                 
187400        MOVE 3  TO W-Q-FREKV-MAX                                          
187500     END-IF                                                               
187600                                                                          
187700     COMPUTE W-ARSBEH = 12                                                
187800         * ( SLAG-KVPB-REF                                                
187900           + SLAG-KVPBREOI)                                               
188000                                                                          
188100**   LÄGGES UTANFÖR ITERATION                                             
188200**   LÄS WDF106 GU OKVAL  ADR-IDLANDX2                                    
188300**   BERÄKNA FRYSGRÄNS  DAGENS + LINK-KVVECKOR-FT                         
188400                                                                          
188500     PERFORM HEB-LANDKOD-FRYSTID                                          
188600                                                                          
188700     PERFORM UNTIL TILLGTAB-IX NOT < W-VECKO-DIFF                         
188800        IF TILLGTAB-IX NOT > ZERO                                         
188900           MOVE 1 TO TILLGTAB-IX                                          
189000        END-IF                                                            
189100        ADD TILLGTAB-ANTAL (TILLGTAB-IX)                                  
189200                                 TO W-TILLG                               
189300        SUBTRACT BHDC-KVBEHOV-VECKA  (TILLGTAB-IX)                        
189400                               FROM W-TILLG                               
189500                                                                          
189600        IF W-TILLG < W-BUFF                                               
189700           PERFORM HEC-BERAEKNA-AVROPSKVANTITET                           
189800           ADD  W-AVROPSKVANTITET TO W-TILLG                              
189900           MOVE W-AVROPSKVANTITET TO SPAR-KVAVROP                         
190000           IF W-AVROPSKVANTITET > ZERO                                    
190100              PERFORM HED-SKAPA-AVROP                                     
190200                                                                          
190300              MOVE NEJ TO SW-FORSLAG-AVROP-SAKNAS                         
190400           END-IF                                                         
190500        END-IF                                                            
190600        ADD 1 TO TILLGTAB-IX                                              
190700     END-PERFORM                                                          
190800     .                                                                    
190900     EJECT                                                                
191000                                                                          
191100 HEA-KOLL-2AAR SECTION.                                                   
191200     MOVE 'HEA-KOLL-2AAR   '   TO CURRENT-SECTION                         
191300                                                                          
191400     MOVE W-SUM-START          TO W-SUMMA-BEHOV                           
191500     MOVE NEJ                  TO W-FLAGGA-2AAR                           
191600                                                                          
191700     IF LART-DAPUBL > ZERO                                                
191800*WZ20DAYS                                                                 
191900        MOVE LART-DAPUBL(3:6)        TO DAYS-TIDATE1                      
192000        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
192100        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
192200        MOVE 0                       TO DAYS-KVDAYS                       
192300        MOVE SPACE                   TO DAYS-TIDATE2                      
192400                                        DAYS-IDCALEND                     
192500        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
192600*                                                                         
192700        IF DAYS-KDRC = 8                                                  
192800          MOVE 'FEL VID ANROP TILL WZ20DAYS 8' TO FELTEXT                 
192900          CALL ABEND USING RKOD                                           
193000        ELSE                                                              
193100          MOVE DAYS-TIDATE2(1:5) TO W-TIFINLV-AAVVD                       
193200        END-IF                                                            
193300     ELSE                                                                 
193400        MOVE OMSP-TIFINLV      TO W-TIFINLV-AAVVD                         
193500     END-IF                                                               
193600     ADD 2                     TO W-TIFINLV-AA                            
193700                                                                          
193800*  ---ÄR ATIKEL ÄLDRE ÄN 2 ÅR ?                                           
193900     IF W-TIFINLV-AAVVD > ZERO AND W-DATUM-AAVV > W-TIFINLV-AAVV          
194000*  ---  KOLL MANUELL EJ PASSERAD LEVERANSPLANESPÄRR                       
194100*  ---  ANNARS BERÄKNA ANTAL VECKOR 2 ÅR                                  
194200                                                                          
194300        IF SPAR-XLAG-KDLPSP = 3 AND XLAG-TILPSP > W-DATUM-AAVV            
194400           MOVE NEJ            TO W-FLAGGA-2AAR                           
194500        ELSE                                                              
194600           MOVE 104            TO W-VVBEHOV                               
194700           MOVE JA             TO W-FLAGGA-2AAR                           
194800        END-IF                                                            
194900     ELSE                                                                 
195000        MOVE NEJ               TO W-FLAGGA-2AAR                           
195100     END-IF                                                               
195200                                                                          
195300     IF W-FLAGGA-2AAR = JA                                                
195400***     *RÄKNA UT SUMMA BEHOV UNDER 2 ÅR (KVPB-REF + KVPBREOI)            
195500                                                                          
195600        MOVE PB-TOTAL-SEP-LEV-XDC  TO TAB2-KDBEHOV                        
195700        MOVE OMSP-IDARTNR          TO TAB2-IDARTNR                        
195800        MOVE OMSP-IDDC             TO TAB2-IDDC                           
195900        MOVE W-DATUM-AAVV-AKT      TO TAB2-TIAAVV-AKTUELL                 
196000                                      TAB2-TIBEHOV-START                  
196100        MOVE 1 TO W-ANTAL-VECKOR                                          
196200        CALL W009VADD USING TAB2-TIBEHOV-START W-ANTAL-VECKOR             
196300                                                                          
196400        MOVE +6                    TO TAB2-TID-AKTUELL                    
196500        MOVE W-VVBEHOV             TO TAB2-KVVECKOR-BEHOV                 
196600        MOVE ZERO                  TO TAB2-KVTILLG-TOT-CDC                
196700                                                                          
196800        IF OMSP-KDERS-UTG = 0                                             
196900          CALL W222BHDC USING TAB2-W222BHDC                               
197000                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
197100                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
197200                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
197300                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
197400                           BHDC-REFL1-2501-PCB                            
197500                           BHDC-REFL1-WDB6-PCB                            
197600                           BHDC-REFL1-WDK7-PCB                            
197700                           BHDC-REFL1-UTIL-WDK6-PCB                       
197800                           BHDC-REFL1-UTIL-WDK7-PCB                       
197900                           BHDC-REFL1-UTIL-WDB6-PCB                       
198000                           BHDC-REFL2-2501-PCB                            
198100                           BHDC-REFL2-WDB6-PCB                            
198200                           BHDC-REFL2-UTIL-WDK6-PCB                       
198300                           BHDC-REFL2-UTIL-WDK7-PCB                       
198400                           BHDC-REFL2-UTIL-WDB6-PCB                       
198500                           BHDC-UTIL-WDK6-PCB                             
198600                           BHDC-UTIL-WDK7-PCB                             
198700                           BHDC-UTIL-WDB6-PCB                             
198800                           BHDC-W222-WDK6-PCB                             
198900                           BHDC-W222-WDK7-PCB                             
199000                           BHDC-W222-ARTM-PCB                             
199100                           BHDC-W222-2501-PCB                             
199200                           BHDC-W222-WDB6R-PCB                            
199300                           BHDC-W222-WDK7R-PCB                            
199400                           BHDC-W222-WDB6-PCB                             
199500                           BHDC-W222-WDD7-PCB                             
199600                           BHDC-W222-WDK7E-PCB                            
199700                           BHDC-W222-UTIL-WDK6-PCB                        
199800                           BHDC-W222-UTIL-WDK7-PCB                        
199900                           BHDC-W222-UTIL-WDB6-PCB                        
200000                           BHDC-W222-UTUP-WDK7-PCB                        
200100                           BHDC-W222-UTUP-WDB6-PCB                        
200200                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
200300                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
200400                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
200500                           BHDC-UTUP-WDK7-PCB                             
200600                           BHDC-UTUP-WDB6-PCB                             
200700                           BHDC-UTUP-UTIL-WDK6-PCB                        
200800                           BHDC-UTUP-UTIL-WDK7-PCB                        
200900                           BHDC-UTUP-UTIL-WDB6-PCB                        
201000                                                                          
201100          IF TAB2-FLJANEJ-ANROP = 'N'                                     
201200            PERFORM S21-NOLLA-TAB2-RESULTATFLT                            
201300          END-IF                                                          
201400        ELSE                                                              
201500          PERFORM S21-NOLLA-TAB2-RESULTATFLT                              
201600        END-IF                                                            
201700                                                                          
201800***     *SUMMERING BEHOV FINNS I TAB2-KVBEHOV-SUMMA                       
201900                                                                          
202000***********vi adderar start-tillg för att kunna göra                      
202100***********rätt jämförelse senare                                         
202200        COMPUTE TAB2-KVBEHOV-SUMMA = TAB2-KVBEHOV-SUMMA +                 
202300                                        W-SUM-START                       
202400     END-IF                                                               
202500     .                                                                    
202600                                                                          
202700                                                                          
202800 HEB-LANDKOD-FRYSTID SECTION.                                             
202900     MOVE 'HEB-LANKOD-FR-TI' TO CURRENT-SECTION                           
203000                                                                          
203100**   BERÄKNA FRYSGRÄNS  DAGENS + XLAG-KVVECKOR-FT                         
203200**                                                                        
203300     MOVE SPACE               TO W-IDLANDX2-SHIP                          
203400     MOVE XLAG-IDLEVNR-SHIP   TO W-IDLEVNR-SHIP                           
203500     PERFORM IMS-GU-WDF106-SHIP-O                                         
203600     IF SEGMENT-FINNS                                                     
203700        MOVE ADR-IDLANDX2     TO W-IDLANDX2-SHIP                          
203800     ELSE                                                                 
203900        DISPLAY 'LANDKOD SAKNAS ' W-IDLEVNR                               
204000        MOVE SPACE            TO W-IDLANDX2-SHIP                          
204100     END-IF                                                               
204200                                                                          
204300**   LÄGG DAGENS DATUM I FRYSTID. ADDERA MED FT*7                         
204400     MOVE AKT-DATUM-AAMMDD    TO W-FRYSTID                                
204500**   ANTAL DGR FRYSTID*7 + 1                                              
204600**   DAGKONV DD + FT                                                      
204700     MOVE 002                 TO DAG-KDCALL                               
204800     MOVE 20                  TO DAG-TISEKEL-FOM                          
204900     MOVE AKT-DATUM-AAMMDD    TO DAG-TIAAMMDD-FOM                         
205000     COMPUTE DAG-KVKALDAG = (XLAG-KVVECKOR-FT * 7) + 2                    
205100     CALL WDAGKONV USING DAG-KDCALL,                                      
205200                         DAG-DATUM-AREA,                                  
205300                         DAG-KDSVAR                                       
205400     IF DAG-KDSVAR = SPACE                                                
205500        MOVE DAG-TISEKEL-TOM  TO W-FRYSTID-SEKEL                          
205600        MOVE DAG-TIAAMMDD-TOM TO W-FRYSTID                                
205700     ELSE                                                                 
205800        MOVE 'FEL VID ANROP TILL DAGKONV 2'                               
205900                              TO FELTEXT                                  
206000        CALL FELLOG                                                       
206100     END-IF                                                               
206200     .                                                                    
206300                                                                          
206400                                                                          
206500 HEC-BERAEKNA-AVROPSKVANTITET SECTION.                                    
206600     MOVE 'HEC-BER-AVROPSKV' TO CURRENT-SECTION                           
206700                                                                          
206800     MOVE SLAG-KVREFBER      TO W-KVANTITET                               
206900                                                                          
207000     IF XLAG-KVULOAD > ZERO                                               
207100        MOVE XLAG-KVULOAD    TO W-KVULOAD                                 
207200     ELSE                                                                 
207300        MOVE XLAG-KVPALL     TO W-KVULOAD                                 
207400     END-IF                                                               
207500                                                                          
207600     IF W-KVULOAD > ZERO                                                  
207700        PERFORM UNTIL (W-TILLG + W-KVANTITET) >=                          
207800                       W-BUFF                                             
207900**********************(W-BUFF + XLAG-KVEOQ)                               
208000           ADD W-KVULOAD TO W-KVANTITET                                   
208100        END-PERFORM                                                       
208200        MOVE W-KVANTITET TO W-AVROPSKVANTITET                             
208300     ELSE                                                                 
208400        IF W-KVANTITET = ZERO                                             
208500          MOVE 1 TO W-KVANTITET                                           
208600        END-IF                                                            
208700                                                                          
208800        COMPUTE W-ANTAL ROUNDED = ((W-KVANTITET / 2                       
208900                                  - W-TILLG                               
209000                                  + W-BUFF)                               
209100                                /   W-KVANTITET)                          
209200                                +   0.49                                  
209300        COMPUTE W-AVROPSKVANTITET ROUNDED =                               
209400                                        W-KVANTITET * W-ANTAL             
209500     END-IF                                                               
209600                                                                          
209700     IF  W-AVROPSKVANTITET < 1                                            
209800         MOVE 1 TO W-AVROPSKVANTITET                                      
209900     END-IF                                                               
210000     .                                                                    
210100     EJECT                                                                
210200                                                                          
210300 HED-SKAPA-AVROP SECTION.                                                 
210400     MOVE 'HED-SKAPA-AVROP ' TO CURRENT-SECTION                           
210500                                                                          
210600******************************************************************        
210700*                                                                *        
210800*    AVROP SKAPAS OCH SKRIVES PÅ REGISTRET                       *        
210900*                                                                *        
211000******************************************************************        
211100                                                                          
211200                                                                          
211300     IF W-FLAGGA-2AAR = JA                                                
211400***     *TEST OM SUMMA AVROP > SUMMA-BEHOV                                
211500        ADD W-AVROPSKVANTITET       TO W-SUMMA-BEHOV                      
211600                                                                          
211700        IF W-SUMMA-BEHOV > TAB2-KVBEHOV-SUMMA                             
211800           MOVE W-TISPECST            TO VADD-DATUM-AAVV                  
211900           MOVE 4                     TO W-ANTAL-VECKOR                   
212000           CALL W009VADD USING VADD-DATUM-AAVV W-ANTAL-VECKOR             
212100                                                                          
212200           IF W-DAAVROP-AVS-AAVV >= W-TISPECST                            
212300          AND W-DAAVROP-AVS-AAVV <  VADD-DATUM-AAVV                       
212400              MOVE NEJ                       TO SW-AUT-PLAN               
212500                                                                          
212600              MOVE SPAR-D904-KDLPORS-TAB(01) TO W-KDLPORS-TAB(01)         
212700              MOVE SPAR-D904-KDLPORS-TAB(02) TO W-KDLPORS-TAB(02)         
212800              MOVE SPAR-D904-KDLPORS-TAB(03) TO W-KDLPORS-TAB(03)         
212900              MOVE 29                        TO W-KDLPORS-TAB(04)         
213000                                                                          
213100              CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP              
213200                                                                          
213300              MOVE W-KDLPORS-TAB(01)  TO SPAR-D904-KDLPORS-TAB(01)        
213400              MOVE W-KDLPORS-TAB(02)  TO SPAR-D904-KDLPORS-TAB(02)        
213500              MOVE W-KDLPORS-TAB(03)  TO SPAR-D904-KDLPORS-TAB(03)        
213600           END-IF                                                         
213700        END-IF                                                            
213800     END-IF                                                               
213900                                                                          
214000     MOVE TILLGTAB-IX      TO W-ANTAL-VECKOR                              
214100                                                                          
214200     MOVE W-DATUM-AAVV-AKT TO W-TIAVROP-DISP                              
214300     CALL W009VADD USING W-TIAVROP-DISP W-ANTAL-VECKOR                    
214400                                                                          
214500*TILEVDAG                                                                 
214600     MOVE ZERO             TO SPAR-TILEVDAG                               
214700     MOVE +1               TO IX-DAG                                      
214800     PERFORM UNTIL IX-DAG > IX-DAG-MAX                                    
214900        IF XLAG-TILEVDAG (IX-DAG) > ZERO                                  
215000           MOVE XLAG-TILEVDAG (IX-DAG) TO SPAR-TILEVDAG                   
215100           MOVE +5     TO IX-DAG                                          
215200        END-IF                                                            
215300        ADD +1         TO IX-DAG                                          
215400     END-PERFORM                                                          
215500                                                                          
215600     IF SPAR-TILEVDAG = ZERO                                              
215700        MOVE SLAG-IDLEVNR              TO W-IDLEVNR                       
215800        MOVE XLAG-IDLEVNR-SHIP         TO W-IDLEVNR-SHIP                  
215900        PERFORM IMS-GU-WDF116-SHIP                                        
216000                                                                          
216100        IF SEGMENT-FINNS                                                  
216200           MOVE +1                     TO IX-DAG                          
216300           PERFORM UNTIL IX-DAG > IX-DAG-MAX                              
216400              IF NDC-TILEVDAG (IX-DAG) > ZERO                             
216500                 MOVE NDC-TILEVDAG (IX-DAG)                               
216600                                       TO SPAR-TILEVDAG                   
216700                 MOVE +5               TO IX-DAG                          
216800              END-IF                                                      
216900              ADD +1                   TO IX-DAG                          
217000           END-PERFORM                                                    
217100           IF SPAR-TILEVDAG = ZERO                                        
217200              MOVE +1                  TO SPAR-TILEVDAG                   
217300           END-IF                                                         
217400        ELSE                                                              
217500           MOVE +1                     TO SPAR-TILEVDAG                   
217600        END-IF                                                            
217700     END-IF                                                               
217800*----  W-KVPB-SDC-TOT hämtas från B-HAEMTA-ARTIKELDATA i                  
217900                                                                          
218000     COMPUTE W-SUMMA-KVPB = SLAG-KVPB-REF  +                              
218100                            W-KVPB-SDC-TOT                                
218200                                                                          
218300*AVS                                                                      
218400*FIX-START NYÅR DISP-09 -- Se pgm W2215000 för hjälp om behov för         
218500*FIX SLUT  Kina att göra anpassningar vid årsskifte.                      
218600                                                                          
218700     COMPUTE W-ANTAL-VECKOR ROUNDED = XLAG-KVDAGAR-FFH / -5               
218800                                                                          
218900     MOVE W-TIAVROP-DISP     TO W-DAAVROP-AVS-AAVV                        
219000* --- LITE FIX MED DATUM EFTERSOM W009VADD KRÄVER COMP-3 FORMAT           
219100     MOVE W-DAAVROP-AVS-AAVV TO W-DAAVROP-AVS-AAVV-C3                     
219200     CALL W009VADD USING W-DAAVROP-AVS-AAVV-C3 W-ANTAL-VECKOR             
219300     MOVE W-DAAVROP-AVS-AAVV-C3 TO W-DAAVROP-AVS-AAVV                     
219400                                                                          
219500*                                                                         
219600*FIX-START nyår 2012 - se pgm W2215000 om behov finns för Kina.           
219700*FIX-SLUT nyår 2012                                                       
219800                                                                          
219900*AVS-AAMMDD                                                               
220000*WZ20DAYS                                                                 
220100     MOVE ZERO                       TO WS-DAYS-TIDATE1-AAVVD             
220200     COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-DAAVROP-AVS-AAVV +            
220300                                     SPAR-TILEVDAG                        
220400     MOVE WS-DAYS-TIDATE1-AAVVD      TO DAYS-TIDATE1                      
220500     MOVE 'YYWWD'                    TO DAYS-KDDATFMT1                    
220600     MOVE 'YYMMDD'                   TO DAYS-KDDATFMT2                    
220700     MOVE 0                          TO DAYS-KVDAYS                       
220800     MOVE SPACE                      TO DAYS-TIDATE2                      
220900                                DAYS-IDCALEND                             
221000     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
221100*                                                                         
221200     IF DAYS-KDRC = 8                                                     
221300       MOVE 'FEL VID ANROP TILL WZ20DAYS 9' TO FELTEXT                    
221400       CALL ABEND USING RKOD                                              
221500     ELSE                                                                 
221600       MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                           
221700                                 ARB-TIAAMMDD-AVS                         
221800                                 GAM-TIAAMMDD-AVS                         
221900     END-IF                                                               
222000*                                                                         
222100      MOVE W-TIAAMMDD-AVS         TO DAYS-TIDATE1                         
222200      MOVE 'YYMMDD'               TO DAYS-KDDATFMT1                       
222300      MOVE 'YYWW'                 TO DAYS-KDDATFMT2                       
222400      MOVE 0                      TO DAYS-KVDAYS                          
222500      MOVE SPACE                  TO DAYS-TIDATE2                         
222600                                DAYS-IDCALEND                             
222700      CALL WZ20DAYS USING DAYS-WZ20DAYS                                   
222800*                                                                         
222900      IF DAYS-KDRC = 8                                                    
223000        MOVE 'FEL VID ANROP TILL WZ20DAYS P' TO FELTEXT                   
223100        CALL ABEND USING RKOD                                             
223200      ELSE                                                                
223300        MOVE SPAR-TILEVDAG TO WOL-TILEVDAG                                
223400        MOVE DAYS-TIDATE2(1:4) TO WOL-TIAAVV-AVS                          
223500        MOVE WOL-TIAAVV-AVS TO WOL-TIAAVV-AVS-C3                          
223600      END-IF                                                              
223700*                                                                         
223800                                                                          
223900*    AVS TVÅ VECKOR BAKÅT OM ARTIKELN SKALL VARA DISPONIBEL UNDER         
224000*    PUBLICERINGSVECKA                                                    
224100                                                                          
224200     IF LART-DAPUBL > ZERO                                                
224300*WZ20DAYS                                                                 
224400        MOVE LART-DAPUBL(3:6)        TO DAYS-TIDATE1                      
224500        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
224600        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
224700        MOVE 0                       TO DAYS-KVDAYS                       
224800        MOVE SPACE                   TO DAYS-TIDATE2                      
224900                                           DAYS-IDCALEND                  
225000        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
225100*                                                                         
225200        IF DAYS-KDRC = 8                                                  
225300          MOVE 'FEL VID ANROP TILL WZ20DAYS A' TO FELTEXT                 
225400          CALL ABEND USING RKOD                                           
225500        ELSE                                                              
225600          MOVE DAYS-TIDATE2(1:5) TO W-TIFINLV-AAVVD                       
225700        END-IF                                                            
225800     ELSE                                                                 
225900        MOVE OMSP-TIFINLV      TO W-TIFINLV-AAVVD                         
226000     END-IF                                                               
226100                                                                          
226200     IF  W-DATUM-AAVV-AKT + 1 <= W-TIFINLV-AAVV                           
226300     AND W-TIAVROP-DISP       = W-TIFINLV-AAVV                            
226400                                                                          
226500*     --- UNDERSÖKNING OM AVS BLEV FÖR NÄRA I TID                         
226600                                                                          
226700         MOVE W-DATUM-AAVV-AKT TO W-DATUM-AAVV-HELP                       
226800         MOVE +1 TO W-ANTAL-VECKOR                                        
226900         CALL W009VADD USING W-DATUM-AAVV-HELP W-ANTAL-VECKOR             
227000                                                                          
227100         IF W-DAAVROP-AVS-AAVV < W-DATUM-AAVV-HELP                        
227200            MOVE W-DATUM-AAVV-HELP TO W-DAAVROP-AVS-AAVV                  
227300         END-IF                                                           
227400                                                                          
227500*WZ20DAYS                                                                 
227600         MOVE ZERO                   TO WS-DAYS-TIDATE1-AAVVD             
227700         COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-DAAVROP-AVS-AAVV +        
227800                                         SPAR-TILEVDAG                    
227900         MOVE WS-DAYS-TIDATE1-AAVVD  TO DAYS-TIDATE1                      
228000         MOVE 'YYWWD'                TO DAYS-KDDATFMT1                    
228100         MOVE 'YYMMDD'               TO DAYS-KDDATFMT2                    
228200         MOVE 0                      TO DAYS-KVDAYS                       
228300         MOVE SPACE                  TO DAYS-TIDATE2                      
228400                                            DAYS-IDCALEND                 
228500         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
228600*                                                                         
228700         IF DAYS-KDRC = 8                                                 
228800           MOVE 'FEL VID ANROP TILL WZ20DAYS B' TO FELTEXT                
228900           CALL ABEND USING RKOD                                          
229000         ELSE                                                             
229100           MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                       
229200                                     ARB-TIAAMMDD-AVS                     
229300                                     GAM-TIAAMMDD-AVS                     
229400         END-IF                                                           
229500*                                                                         
229600         MOVE W-TIAAMMDD-AVS      TO DAYS-TIDATE1                         
229700         MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                       
229800         MOVE 'YYWW'              TO DAYS-KDDATFMT2                       
229900         MOVE 0                   TO DAYS-KVDAYS                          
230000         MOVE SPACE               TO DAYS-TIDATE2                         
230100                                   DAYS-IDCALEND                          
230200         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
230300*                                                                         
230400         IF DAYS-KDRC = 8                                                 
230500           MOVE 'FEL VID ANROP TILL WZ20DAYS Q' TO FELTEXT                
230600           CALL ABEND USING RKOD                                          
230700         ELSE                                                             
230800           MOVE SPAR-TILEVDAG TO WOL-TILEVDAG                             
230900           MOVE DAYS-TIDATE2(1:4) TO WOL-TIAAVV-AVS                       
231000           MOVE WOL-TIAAVV-AVS TO WOL-TIAAVV-AVS-C3                       
231100         END-IF                                                           
231200     END-IF                                                               
231300                                                                          
231400*FIX NYÅR START  DATKONVJUST 2014                                         
231500     COMPUTE FIX-AAVVD = 10 * W-DAAVROP-AVS-AAVV +                        
231600                              SPAR-TILEVDAG                               
231700     IF FIX-AAVVD = 15011                                                 
231800        MOVE 141229          TO W-TIAAMMDD-AVS                            
231900                                ARB-TIAAMMDD-AVS                          
232000                                GAM-TIAAMMDD-AVS                          
232100        MOVE 1               TO WOL-TILEVDAG                              
232200        MOVE 1501            TO WOL-TIAAVV-AVS                            
232300        MOVE WOL-TIAAVV-AVS  TO WOL-TIAAVV-AVS-C3                         
232400     END-IF                                                               
232500     IF FIX-AAVVD = 15012                                                 
232600        MOVE 141230          TO W-TIAAMMDD-AVS                            
232700                                ARB-TIAAMMDD-AVS                          
232800                                GAM-TIAAMMDD-AVS                          
232900        MOVE 2               TO WOL-TILEVDAG                              
233000        MOVE 1501            TO WOL-TIAAVV-AVS                            
233100        MOVE WOL-TIAAVV-AVS  TO WOL-TIAAVV-AVS-C3                         
233200     END-IF                                                               
233300     IF FIX-AAVVD = 15013                                                 
233400        MOVE 141231          TO W-TIAAMMDD-AVS                            
233500                                ARB-TIAAMMDD-AVS                          
233600                                GAM-TIAAMMDD-AVS                          
233700        MOVE 3               TO WOL-TILEVDAG                              
233800        MOVE 1501            TO WOL-TIAAVV-AVS                            
233900        MOVE WOL-TIAAVV-AVS  TO WOL-TIAAVV-AVS-C3                         
234000     END-IF                                                               
234100*FIX NYÅR END  DATKONVJUST 2014                                           
234200                                                                          
234300*INL                                                                      
234400     MOVE XLAG-IDLEVNR-SHIP         TO W-IDLEVNR-SHIP                     
234500     PERFORM IMS-GU-WDF116-SHIP                                           
234600     IF SEGMENT-SAKNAS                                                    
234700        MOVE ZERO TO NDC-KVDAGAR-TT                                       
234800        MOVE 'N'  TO SW-WDF1                                              
234900     END-IF                                                               
235000                                                                          
235100     IF (SLAG-KVREFBER > ZERO)        AND                                 
235200        ((W-ARSBEH / SLAG-KVREFBER) > 35)                                 
235300                                                                          
235400******   DAGLIGA AVROP BEHANDLAS SENARE I HEDB-SKAPA-DAGL-AVROP           
235500         CONTINUE                                                         
235600     ELSE                                                                 
235700**      LÄS HELG-TAB  IDLANDX2+DATUM                                      
235800**      OM TRÄFF JUSTERA DATUM (OBS LINK3-TILEVDAG ?)                     
235900                                                                          
236000        PERFORM HEDA-KOLL-HELGDAG                                         
236100     END-IF                                                               
236200                                                                          
236300*CC**INL                                                                  
236400*CC*     MOVE XLAG-IDLEVNR-SHIP         TO W-IDLEVNR-SHIP                 
236500*CC*     PERFORM IMS-GU-WDF116-SHIP                                       
236600*CC*     IF SEGMENT-SAKNAS                                                
236700*CC*        MOVE ZERO TO NDC-KVDAGAR-TT                                   
236800*CC*        MOVE 'N'  TO SW-WDF1                                          
236900*CC*     END-IF                                                           
237000                                                                          
237100*INL                                                                      
237200     MOVE 2                   TO WORK-KDCALL                              
237300     MOVE OMSP-IDDC           TO WORK-IDDC                                
237400     MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                        
237500     MOVE NDC-KVDAGAR-TT      TO WORK-KVWORKD                             
237600     ADD +1                   TO WORK-KVWORKD                             
237700     CALL WORKDAY USING  WORK-KDCALL                                      
237800          WORK-DATE-AREA WORK-KDSVAR                                      
237900     MOVE WORK-TIAAMMDD-TOM   TO W-TIAVRDAT-INL                           
238000*DISP                                                                     
238100     MOVE 2                   TO WORK-KDCALL                              
238200     MOVE OMSP-IDDC           TO WORK-IDDC                                
238300     MOVE W-TIAVRDAT-INL      TO WORK-TIAAMMDD-FOM                        
238400     MOVE LART-KVDAGAR-INLEV  TO WORK-KVWORKD                             
238500     ADD +1                   TO WORK-KVWORKD                             
238600     CALL WORKDAY USING  WORK-KDCALL                                      
238700          WORK-DATE-AREA WORK-KDSVAR                                      
238800     MOVE WORK-TIAAMMDD-TOM   TO W-TIAVRDAT-DISP                          
238900                                                                          
239000*    HÄR GÖRS NÅGON TEST SOM ANGER VILKA AVROP SOM SKALL                  
239100*    SMETAS UT PÅ LEVERANTÖRENS LEVDAGAR AVROPS-VECKAN                    
239200                                                                          
239300     IF (SLAG-KVREFBER > ZERO)        AND                                 
239400        ((W-ARSBEH / SLAG-KVREFBER) > 35)                                 
239500                                                                          
239600        PERFORM HEDB-SKAPA-DAGL-AVROP                                     
239700     ELSE                                                                 
239800        IF SPAR-KVAVROP > ZERO                                            
239900           PERFORM S200-KOLLA-SKRIV-NYTT-FORSLAG                          
240000        END-IF                                                            
240100     END-IF                                                               
240200     .                                                                    
240300     EJECT                                                                
240400                                                                          
240500 HEDA-KOLL-HELGDAG SECTION.                                               
240600     MOVE 'HEDA-KOLL-HELG  ' TO CURRENT-SECTION                           
240700                                                                          
240800* --- FINNS UNDERLIGGANDE SEGMENT MED IDLEVNR ?                           
240900* --- KOMMER I SENARE RELEASE                                             
241000     MOVE W-IDLANDX2-SHIP  TO W-IDLANDX2                                  
241100     MOVE 20               TO W-DADATUM-HELG-SS                           
241200     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
241300                                                                          
241400     MOVE NEJ TO SW-HELG                                                  
241500     MOVE +1  TO IX-HELG                                                  
241600     PERFORM UNTIL IX-HELG > ANT-HELG                                     
241700        IF W-IDLANDX2 = TAB-IDLANDX2 (IX-HELG) AND                        
241800           W-DADATUM-HELG = TAB-DADATUM-HELG (IX-HELG)                    
241900           MOVE JA TO SW-HELG                                             
242000           ADD ANT-HELG TO IX-HELG                                        
242100        ELSE                                                              
242200           IF W-IDLANDX2 < TAB-IDLANDX2 (IX-HELG)                         
242300              MOVE ANT-HELG TO IX-HELG                                    
242400           END-IF                                                         
242500        END-IF                                                            
242600        ADD +1 TO IX-HELG                                                 
242700     END-PERFORM                                                          
242800                                                                          
242900     IF SW-HELG = JA                                                      
243000                                                                          
243100        PERFORM HEDAA-SOEK-NY-AVS-DAG                                     
243200*WZ20DAYS                                                                 
243300        MOVE ARB-TIAAMMDD-AVS        TO DAYS-TIDATE1                      
243400        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
243500        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
243600        MOVE 0                       TO DAYS-KVDAYS                       
243700        MOVE SPACE                   TO DAYS-TIDATE2                      
243800                                           DAYS-IDCALEND                  
243900        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
244000*                                                                         
244100        IF DAYS-KDRC = 8                                                  
244200          MOVE 'FEL VID ANROP TILL WZ20DAYS C' TO FELTEXT                 
244300          CALL ABEND USING RKOD                                           
244400        ELSE                                                              
244500          MOVE DAYS-TIDATE2(1:2)   TO WS-DAYS-TIDATE-AA                   
244600          MOVE DAYS-TIDATE2(3:2)   TO WS-DAYS-TIDATE-VV                   
244700          COMPUTE W-DAAVROP-AVS-AAVV =                                    
244800                  WS-DAYS-TIDATE-AA * 100 + WS-DAYS-TIDATE-VV             
244900          MOVE DAYS-TIDATE2(5:1)   TO SPAR-TILEVDAG                       
245000        END-IF                                                            
245100*FIX NYÅR START DATKONVJUST 2014                                          
245200        IF ARB-TIAAMMDD-AVS = 141229                                      
245300           MOVE 1501   TO W-DAAVROP-AVS-AAVV                              
245400           MOVE 1      TO SPAR-TILEVDAG                                   
245500        END-IF                                                            
245600        IF ARB-TIAAMMDD-AVS = 141230                                      
245700           MOVE 1501   TO W-DAAVROP-AVS-AAVV                              
245800           MOVE 2      TO SPAR-TILEVDAG                                   
245900        END-IF                                                            
246000        IF ARB-TIAAMMDD-AVS = 141231                                      
246100           MOVE 1501   TO W-DAAVROP-AVS-AAVV                              
246200           MOVE 3      TO SPAR-TILEVDAG                                   
246300        END-IF                                                            
246400*FIX NYÅR END DATKONVJUST 2014                                            
246500     END-IF                                                               
246600     .                                                                    
246700     EJECT                                                                
246800                                                                          
246900 HEDAA-SOEK-NY-AVS-DAG SECTION.                                           
247000     MOVE 'HEDAA-NY-AVS-DAG' TO CURRENT-SECTION                           
247100                                                                          
247200*    ITERERA                                                              
247300*       -7 DAGAR                                                          
247400*       UTANFÖR FRYSTID ?                                                 
247500*       HELGDAG ?                                                         
247600     MOVE NEJ                 TO SW-OK SW-FRYS                            
247700     PERFORM UNTIL SW-OK = JA OR SW-FRYS = JA                             
247800        MOVE 003              TO DAG-KDCALL                               
247900        MOVE 20               TO DAG-TISEKEL-TOM                          
248000        MOVE ARB-TIAAMMDD-AVS TO DAG-TIAAMMDD-TOM                         
248100        MOVE 8                TO DAG-KVKALDAG                             
248200        CALL WDAGKONV   USING DAG-KDCALL,                                 
248300                              DAG-DATUM-AREA,                             
248400                              DAG-KDSVAR                                  
248500        IF DAG-KDSVAR = SPACE                                             
248600           MOVE DAG-TIAAMMDD-FOM TO ARB-TIAAMMDD-AVS                      
248700           IF ARB-TIAAMMDD-AVS >  W-FRYSTID  AND                          
248800              ARB-TIAAMMDD-AVS >= W-DASPECST-AAMMDD                       
248900              MOVE JA         TO SW-OK                                    
249000              PERFORM HEDAAA-KOLL-HELG                                    
249100           ELSE                                                           
249200              MOVE JA         TO SW-FRYS                                  
249300           END-IF                                                         
249400        ELSE                                                              
249500           MOVE 'FEL VID ANROP TILL DAGKONV 1'                            
249600                              TO FELTEXT                                  
249700           CALL FELLOG                                                    
249800        END-IF                                                            
249900     END-PERFORM                                                          
250000                                                                          
250100                                                                          
250200     IF SW-FRYS = JA                                                      
250300*       OM EJ OK: FINNS ANNAN DAG URSPRUNGLIG VECKA ?                     
250400                                                                          
250500        PERFORM HEDAAB-SOEK-ANNAN-DAG                                     
250600                                                                          
250700*       OM EJ OK: NÄSTA MÖJLIGA TILLFÄLLE                                 
250800     END-IF                                                               
250900                                                                          
251000     MOVE ARB-TIAAMMDD-AVS  TO W-TIAAMMDD-AVS                             
251100     .                                                                    
251200     EJECT                                                                
251300                                                                          
251400 HEDAAA-KOLL-HELG SECTION.                                                
251500     MOVE 'HEDAAA-KOLL-HELG' TO CURRENT-SECTION                           
251600                                                                          
251700     MOVE W-IDLANDX2-SHIP  TO W-IDLANDX2                                  
251800     MOVE 20               TO W-DADATUM-HELG-SS                           
251900     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
252000     MOVE +1  TO IX-HELG                                                  
252100     PERFORM UNTIL IX-HELG > ANT-HELG                                     
252200        IF W-IDLANDX2 = TAB-IDLANDX2 (IX-HELG) AND                        
252300           W-DADATUM-HELG = TAB-DADATUM-HELG (IX-HELG)                    
252400*             SÖK NY AVS-DAG                                              
252500           MOVE NEJ     TO SW-OK                                          
252600           ADD ANT-HELG TO IX-HELG                                        
252700        ELSE                                                              
252800           IF W-IDLANDX2 < TAB-IDLANDX2 (IX-HELG)                         
252900              MOVE ANT-HELG TO IX-HELG                                    
253000           END-IF                                                         
253100        END-IF                                                            
253200        ADD +1 TO IX-HELG                                                 
253300     END-PERFORM                                                          
253400     .                                                                    
253500                                                                          
253600                                                                          
253700 HEDAAB-SOEK-ANNAN-DAG SECTION.                                           
253800     MOVE 'HEDAAB-ANNAN-DAG' TO CURRENT-SECTION                           
253900                                                                          
254000*                                                                         
254100*    FINNS XLAG-TILEVDAG   NOT = WOL-TILEVDAG                             
254200*ALTERNATIVT                                                              
254300*    FINNS WDF1-TILEVDAG   NOT = WOL-TILEVDAG                             
254400                                                                          
254500     MOVE ZERO TO ARB-TILEVDAG (1)                                        
254600                  ARB-TILEVDAG (2)                                        
254700                  ARB-TILEVDAG (3)                                        
254800                  ARB-TILEVDAG (4)                                        
254900                  ARB-TILEVDAG (5)                                        
255000     IF XLAG-TILEVDAG (1) > ZERO OR                                       
255100        XLAG-TILEVDAG (2) > ZERO OR                                       
255200        XLAG-TILEVDAG (3) > ZERO OR                                       
255300        XLAG-TILEVDAG (4) > ZERO OR                                       
255400        XLAG-TILEVDAG (5) > ZERO                                          
255500        MOVE +1              TO IX-DG                                     
255600        PERFORM UNTIL IX-DG > 5                                           
255700           IF XLAG-TILEVDAG (IX-DG) > ZERO AND                            
255800              XLAG-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                    
255900              MOVE XLAG-TILEVDAG (IX-DG)                                  
256000                             TO ARB-TILEVDAG (IX-DG)                      
256100           END-IF                                                         
256200           ADD +1            TO IX-DG                                     
256300        END-PERFORM                                                       
256400     ELSE                                                                 
256500        IF WDF1-EXIST                                                     
256600          MOVE +1            TO IX-DG                                     
256700          PERFORM UNTIL IX-DG > 5                                         
256800             IF NDC-TILEVDAG (IX-DG) > ZERO AND                           
256900                NDC-TILEVDAG (IX-DG) NOT = WOL-TILEVDAG                   
257000                MOVE NDC-TILEVDAG (IX-DG)                                 
257100                               TO ARB-TILEVDAG (IX-DG)                    
257200             END-IF                                                       
257300             ADD +1          TO IX-DG                                     
257400          END-PERFORM                                                     
257500        END-IF                                                            
257600     END-IF                                                               
257700                                                                          
257800     MOVE +1                 TO IX-DG                                     
257900     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
258000        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
258100           PERFORM HEDAABA-KOLL-NYTT-DATUM                                
258200        END-IF                                                            
258300        ADD +1               TO IX-DG                                     
258400     END-PERFORM                                                          
258500                                                                          
258600                                                                          
258700*    ANNARS FÖRSTA LEVDAG NÄSTA VECKA OSV                                 
258800     MOVE WOL-TILEVDAG TO ARB-TILEVDAG (WOL-TILEVDAG)                     
258900     PERFORM UNTIL SW-OK = JA                                             
259000*      STEGA FRAMÅT                                                       
259100       PERFORM HEDAABB-KOLL-NASTA-DATUM                                   
259200     END-PERFORM                                                          
259300     .                                                                    
259400     EJECT                                                                
259500                                                                          
259600 HEDAABA-KOLL-NYTT-DATUM SECTION.                                         
259700     MOVE 'HEDAABA-NYTT-DAT' TO CURRENT-SECTION                           
259800                                                                          
259900*    OM OK => SW-OK = JA                                                  
260000*    KOLL AV ANNAN DAG I URSPRUNGLIG AVSÄNDNINGSVECKA                     
260100*WZ20DAYS                                                                 
260200     MOVE ZERO                       TO WS-DAYS-TIDATE1-AAVVD             
260300     COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * WOL-TIAAVV-AVS +                
260400                                     ARB-TILEVDAG (IX-DG)                 
260500     MOVE WS-DAYS-TIDATE1-AAVVD      TO DAYS-TIDATE1                      
260600     MOVE 'YYWWD'                    TO DAYS-KDDATFMT1                    
260700     MOVE 'YYMMDD'                   TO DAYS-KDDATFMT2                    
260800     MOVE 0                          TO DAYS-KVDAYS                       
260900     MOVE SPACE                      TO DAYS-TIDATE2                      
261000                                        DAYS-IDCALEND                     
261100     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
261200*                                                                         
261300     IF DAYS-KDRC = 8                                                     
261400       MOVE 'FEL VID ANROP TILL WZ20DAYS 11' TO FELTEXT                   
261500       CALL ABEND USING RKOD                                              
261600     ELSE                                                                 
261700       MOVE DAYS-TIDATE2(1:6) TO ARB-TIAAMMDD-AVS                         
261800     END-IF                                                               
261900*                                                                         
262000*FIX NYÅR START  DATKONVJUST 2014                                         
262100     COMPUTE FIX-AAVVD = 10 * WOL-TIAAVV-AVS +                            
262200                              ARB-TILEVDAG (IX-DG)                        
262300     IF FIX-AAVVD = 15011                                                 
262400        MOVE 141229    TO ARB-TIAAMMDD-AVS                                
262500     END-IF                                                               
262600     IF FIX-AAVVD = 15012                                                 
262700        MOVE 141230    TO ARB-TIAAMMDD-AVS                                
262800     END-IF                                                               
262900     IF FIX-AAVVD = 15013                                                 
263000        MOVE 141231    TO ARB-TIAAMMDD-AVS                                
263100     END-IF                                                               
263200*FIX NYÅR END  DATKONVJUST 2014                                           
263300                                                                          
263400     MOVE W-IDLANDX2-SHIP  TO W-IDLANDX2                                  
263500     MOVE 20               TO W-DADATUM-HELG-SS                           
263600     MOVE ARB-TIAAMMDD-AVS TO W-DADATUM-HELG-AAMMDD                       
263700     MOVE +1  TO IX-HELG                                                  
263800     PERFORM UNTIL IX-HELG > ANT-HELG                                     
263900        IF W-IDLANDX2 = TAB-IDLANDX2 (IX-HELG) AND                        
264000           W-DADATUM-HELG = TAB-DADATUM-HELG (IX-HELG)                    
264100**            SÖK NY AVS-DAG                                              
264200           MOVE NEJ     TO SW-OK                                          
264300           ADD ANT-HELG TO IX-HELG                                        
264400        ELSE                                                              
264500           IF W-IDLANDX2 < TAB-IDLANDX2 (IX-HELG) OR                      
264600              IX-HELG = ANT-HELG                                          
264700              MOVE JA       TO SW-OK                                      
264800              MOVE ANT-HELG TO IX-HELG                                    
264900           END-IF                                                         
265000        END-IF                                                            
265100        ADD +1 TO IX-HELG                                                 
265200     END-PERFORM                                                          
265300     .                                                                    
265400     EJECT                                                                
265500                                                                          
265600 HEDAABB-KOLL-NASTA-DATUM SECTION.                                        
265700     MOVE 'HEDAABB-N-DATUM ' TO CURRENT-SECTION                           
265800                                                                          
265900**      OM OK => SW-OK = JA                                               
266000**   ÖKA VECKA MED 1                                                      
266100**   SÖK AVS-DAGAR I VECKAN                                               
266200                                                                          
266300     MOVE WOL-TIAAVV-AVS    TO WOL-TIAAVV-AVS-C3                          
266400     MOVE 1                 TO W-ANTAL-VECKOR                             
266500     CALL W009VADD USING WOL-TIAAVV-AVS-C3 W-ANTAL-VECKOR                 
266600     MOVE WOL-TIAAVV-AVS-C3 TO WOL-TIAAVV-AVS                             
266700                                                                          
266800     MOVE +1                 TO IX-DG                                     
266900     PERFORM UNTIL IX-DG > 5 OR SW-OK = JA                                
267000        IF ARB-TILEVDAG (IX-DG) > ZERO                                    
267100           PERFORM HEDAABA-KOLL-NYTT-DATUM                                
267200        END-IF                                                            
267300        ADD +1               TO IX-DG                                     
267400     END-PERFORM                                                          
267500     .                                                                    
267600     EJECT                                                                
267700                                                                          
267800 HEDB-SKAPA-DAGL-AVROP SECTION.                                           
267900     MOVE 'HEDB-DAGL-AVROP ' TO CURRENT-SECTION                           
268000                                                                          
268100     MOVE ZERO                TO W-TILEVDAG (1)                           
268200                                 W-TILEVDAG (2)                           
268300                                 W-TILEVDAG (3)                           
268400                                 W-TILEVDAG (4)                           
268500                                 W-TILEVDAG (5)                           
268600     MOVE ZERO                TO ANT-LEVDAG                               
268700                                                                          
268800     MOVE +1                  TO IX-DAG                                   
268900     PERFORM UNTIL IX-DAG > +5                                            
269000        IF XLAG-TILEVDAG (IX-DAG) > ZERO                                  
269100           MOVE XLAG-TILEVDAG (IX-DAG) TO W-TILEVDAG (IX-DAG)             
269200           ADD +1             TO ANT-LEVDAG                               
269300        END-IF                                                            
269400        ADD +1                TO IX-DAG                                   
269500     END-PERFORM                                                          
269600                                                                          
269700**---- DET FINNS INGEN TILEVDAG PÅ DC-NIVÅ.                               
269800     IF ANT-LEVDAG = ZERO                                                 
269900        MOVE SLAG-IDLEVNR          TO W-IDLEVNR                           
270000        MOVE XLAG-IDLEVNR-SHIP     TO W-IDLEVNR-SHIP                      
270100        PERFORM IMS-GU-WDF116-SHIP                                        
270200        IF SEGMENT-FINNS                                                  
270300          MOVE +1                  TO IX-DAG                              
270400          PERFORM UNTIL IX-DAG > +5                                       
270500             IF NDC-TILEVDAG (IX-DAG) > ZERO                              
270600                MOVE NDC-TILEVDAG (IX-DAG)                                
270700                                   TO W-TILEVDAG (IX-DAG)                 
270800                ADD +1             TO ANT-LEVDAG                          
270900             END-IF                                                       
271000             ADD +1                TO IX-DAG                              
271100          END-PERFORM                                                     
271200                                                                          
271300          IF ANT-LEVDAG = ZERO                                            
271400             MOVE +1               TO W-TILEVDAG (1)                      
271500                                      ANT-LEVDAG                          
271600          END-IF                                                          
271700        ELSE                                                              
271800          MOVE +1                  TO W-TILEVDAG (1)                      
271900                                      ANT-LEVDAG                          
272000        END-IF                                                            
272100     END-IF                                                               
272200                                                                          
272300*KVAVROP/DAG                                                              
272400     MOVE ZERO                  TO W-KVAVROP (1)                          
272500                                   W-KVAVROP (2)                          
272600                                   W-KVAVROP (3)                          
272700                                   W-KVAVROP (4)                          
272800                                   W-KVAVROP (5)                          
272900     IF XLAG-KVPALL < +1                                                  
273000        MOVE +1                 TO W-KVPALL                               
273100     ELSE                                                                 
273200        MOVE XLAG-KVPALL        TO W-KVPALL                               
273300     END-IF                                                               
273400     IF SLAG-KVREFBER > ZERO                                              
273500        MOVE SLAG-KVREFBER      TO W-KVPALL                               
273600     END-IF                                                               
273700                                                                          
273800     IF SPAR-KVAVROP > ZERO                                               
273900        PERFORM UNTIL (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP(3)         
274000                     + W-KVAVROP(4) + W-KVAVROP(5))                       
274100                     NOT < SPAR-KVAVROP                                   
274200          MOVE +1               TO IX-DAG                                 
274300          PERFORM UNTIL    IX-DAG  > +5                                   
274400            IF W-TILEVDAG (IX-DAG) > ZERO                                 
274500               IF (W-KVAVROP(1) + W-KVAVROP(2) + W-KVAVROP (3) +          
274600                   W-KVAVROP(4) + W-KVAVROP(5)) < SPAR-KVAVROP            
274700                   ADD W-KVPALL TO W-KVAVROP (IX-DAG)                     
274800               END-IF                                                     
274900            END-IF                                                        
275000            ADD +1               TO IX-DAG                                
275100            IF IX-DAG = +6                                                
275200               IF XLAG-KVULOAD > ZERO                                     
275300                  MOVE XLAG-KVULOAD TO W-KVULOAD                          
275400               ELSE                                                       
275500                  MOVE XLAG-KVPALL  TO W-KVULOAD                          
275600               END-IF                                                     
275700               IF W-KVULOAD > ZERO                                        
275800                  MOVE W-KVULOAD    TO W-KVPALL                           
275900               END-IF                                                     
276000            END-IF                                                        
276100          END-PERFORM                                                     
276200        END-PERFORM                                                       
276300     END-IF                                                               
276400                                                                          
276500*AVS-AAVV                                                                 
276600                                                                          
276700     MOVE W-DAAVROP-AVS-AAVV    TO SPAR-TIAVROP-AVS                       
276800                                                                          
276900     MOVE +1                    TO IX-DAG                                 
277000     PERFORM UNTIL   (IX-DAG) > +5                                        
277100       IF W-TILEVDAG (IX-DAG) > ZERO AND W-KVAVROP (IX-DAG) > ZERO        
277200                                                                          
277300*AVS-AAMMDD                                                               
277400*                                                                         
277500*WZ20DAYS                                                                 
277600*CALL OF WZ20DAYS IS SPLIT INTO TWO CALLS BECAUSE OF NEED FOR             
277700*DATE FORMS 'YYMMDD' AND 'YYWW'.                                          
277800*PREVIOUS CALL TO WDATKONV WAS JUST ONE CALL.                             
277900*                                                                         
278000          MOVE ZERO                  TO WS-DAYS-TIDATE1-AAVVD             
278100        COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * W-DAAVROP-AVS-AAVV +         
278200                                        W-TILEVDAG (IX-DAG)               
278300          MOVE WS-DAYS-TIDATE1-AAVVD TO DAYS-TIDATE1                      
278400          MOVE 'YYWWD'               TO DAYS-KDDATFMT1                    
278500          MOVE 'YYMMDD'              TO DAYS-KDDATFMT2                    
278600          MOVE 0                     TO DAYS-KVDAYS                       
278700          MOVE SPACE                 TO DAYS-TIDATE2                      
278800                                             DAYS-IDCALEND                
278900          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
279000*                                                                         
279100          IF DAYS-KDRC = 8                                                
279200            MOVE 'FEL VID ANROP TILL WZ20DAYS 11' TO FELTEXT              
279300            CALL ABEND USING RKOD                                         
279400          ELSE                                                            
279500            MOVE DAYS-TIDATE2(1:6) TO W-TIAAMMDD-AVS                      
279600            MOVE DAYS-TIDATE2(1:6) TO ARB-TIAAMMDD-AVS                    
279700          END-IF                                                          
279800*                                                                         
279900          MOVE W-TIAAMMDD-AVS     TO DAYS-TIDATE1                         
280000          MOVE 'YYMMDD'           TO DAYS-KDDATFMT1                       
280100          MOVE 'YYWW'             TO DAYS-KDDATFMT2                       
280200          MOVE 0                  TO DAYS-KVDAYS                          
280300          MOVE SPACE              TO DAYS-TIDATE2                         
280400                                    DAYS-IDCALEND                         
280500          CALL WZ20DAYS USING DAYS-WZ20DAYS                               
280600*                                                                         
280700          IF DAYS-KDRC = 8                                                
280800            MOVE 'FEL VID ANROP TILL WZ20DAYS R' TO FELTEXT               
280900            CALL ABEND USING RKOD                                         
281000          ELSE                                                            
281100            MOVE W-TILEVDAG (IX-DAG) TO SPAR-TILEVDAG                     
281200                                        WOL-TILEVDAG                      
281300            MOVE DAYS-TIDATE2(1:4) TO WOL-TIAAVV-AVS                      
281400            MOVE WOL-TIAAVV-AVS TO WOL-TIAAVV-AVS-C3                      
281500          END-IF                                                          
281600*                                                                         
281700*FIX NYÅR START   DATKONVJUST 2014                                        
281800          COMPUTE FIX-AAVVD = 10 * W-DAAVROP-AVS-AAVV +                   
281900                                  W-TILEVDAG (IX-DAG)                     
282000          IF FIX-AAVVD = 15011                                            
282100             MOVE 141229         TO W-TIAAMMDD-AVS                        
282200                                    ARB-TIAAMMDD-AVS                      
282300             MOVE 1              TO SPAR-TILEVDAG                         
282400                                    WOL-TILEVDAG                          
282500             MOVE 1501           TO WOL-TIAAVV-AVS                        
282600             MOVE WOL-TIAAVV-AVS TO WOL-TIAAVV-AVS-C3                     
282700          END-IF                                                          
282800          IF FIX-AAVVD = 15012                                            
282900             MOVE 141230         TO W-TIAAMMDD-AVS                        
283000                                    ARB-TIAAMMDD-AVS                      
283100             MOVE 2              TO SPAR-TILEVDAG                         
283200                                    WOL-TILEVDAG                          
283300             MOVE 1501           TO WOL-TIAAVV-AVS                        
283400             MOVE WOL-TIAAVV-AVS TO WOL-TIAAVV-AVS-C3                     
283500          END-IF                                                          
283600          IF FIX-AAVVD = 15013                                            
283700             MOVE 141231         TO W-TIAAMMDD-AVS                        
283800                                    ARB-TIAAMMDD-AVS                      
283900             MOVE 3              TO SPAR-TILEVDAG                         
284000                                    WOL-TILEVDAG                          
284100             MOVE 1501           TO WOL-TIAAVV-AVS                        
284200             MOVE WOL-TIAAVV-AVS TO WOL-TIAAVV-AVS-C3                     
284300          END-IF                                                          
284400*FIX NYÅR END   DATKONVJUST 2014                                          
284500                                                                          
284600**    LÄS HELG-TAB IDLANDX2 DATUM                                         
284700**      OM TRÄFF JUSTERA DATUM                                            
284800                                                                          
284900        PERFORM HEDA-KOLL-HELGDAG                                         
285000*                                                                         
285100*INL                                                                      
285200          MOVE 2                   TO WORK-KDCALL                         
285300          MOVE OMSP-IDDC           TO WORK-IDDC                           
285400          MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                   
285500          MOVE NDC-KVDAGAR-TT      TO WORK-KVWORKD                        
285600          ADD +1                   TO WORK-KVWORKD                        
285700          CALL WORKDAY USING  WORK-KDCALL                                 
285800               WORK-DATE-AREA WORK-KDSVAR                                 
285900          MOVE WORK-TIAAMMDD-TOM   TO W-TIAVRDAT-INL                      
286000*DISP                                                                     
286100          MOVE 2                     TO WORK-KDCALL                       
286200          MOVE OMSP-IDDC             TO WORK-IDDC                         
286300          MOVE WORK-TIAAMMDD-TOM     TO WORK-TIAAMMDD-FOM                 
286400          MOVE LART-KVDAGAR-INLEV    TO WORK-KVWORKD                      
286500          ADD +1                     TO WORK-KVWORKD                      
286600          CALL WORKDAY USING  WORK-KDCALL                                 
286700               WORK-DATE-AREA WORK-KDSVAR                                 
286800          MOVE WORK-TIAAMMDD-TOM     TO W-TIAVRDAT-DISP                   
286900                                                                          
287000          MOVE 20                TO W-DAAVROP-AVS-SS                      
287100***       MOVE W-DAAVROP-AVS     TO LINK3-DAAVROP-AVS                     
287200                                                                          
287300          MOVE W-KVAVROP(IX-DAG) TO SPAR-KVAVROP                          
287400                                                                          
287500          PERFORM S200-KOLLA-SKRIV-NYTT-FORSLAG                           
287600                                                                          
287700          MOVE SPAR-TIAVROP-AVS  TO W-DAAVROP-AVS-AAVV                    
287800       END-IF                                                             
287900                                                                          
288000       ADD +1 TO IX-DAG                                                   
288100     END-PERFORM                                                          
288200     .                                                                    
288300     EJECT                                                                
288400                                                                          
288500 I-SKAPA-UTFILER SECTION.                                                 
288600     MOVE 'I-SKAPA-UTFILER ' TO CURRENT-SECTION                           
288700                                                                          
288800* --- W22414 FÖR FRAMSTÄLLNING AV AUTOMATISKA LEVERANSPLANER              
288900* --- W22415 UPPDATERINGSPOSTER TILL W2241500 FÖR WDK7 OCH WDD9           
289000* --- W22416 UPPDATERINGSPOSTER TILL WDD6 (PROC W221P159)                 
289100                                                                          
289200                                                                          
289300     IF SW-AUT-PLAN = NEJ                                                 
289400     OR SW-X-OPT = JA                                                     
289500     OR KDLPORS-03                                                        
289600        PERFORM IA-SKAPA-SKRIV-W22416-POST                                
289700     ELSE                                                                 
289800        PERFORM IB-SKAPA-SKRIV-W22414-POST                                
289900     END-IF                                                               
290000                                                                          
290100     .                                                                    
290200                                                                          
290300                                                                          
290400 IA-SKAPA-SKRIV-W22416-POST SECTION.                                      
290500     MOVE 'IA-SKAPA-W22416 ' TO CURRENT-SECTION                           
290600                                                                          
290700     MOVE OMSP-IDDC                 TO LPF-IDDC                           
290800     MOVE XLAG-IDANSK               TO LPF-IDANSK                         
290900     MOVE SLAG-IDLEVNR              TO LPF-IDLEVNR                        
291000     MOVE OMSP-IDARTNR              TO LPF-IDARTNR                        
291100     MOVE W-DATUM-AAVV-AKT          TO LPF-TIOMSPEC                       
291200     MOVE SPAR-XLAG-KDLEVPLF        TO LPF-KDLEVPLF                       
291300     MOVE AKT-DATUM-AAMMDD          TO LPF-TIUPPDAT                       
291400                                                                          
291500     MOVE OMSP-IDARTNR       TO W-IDARTNR                                 
291600     PERFORM IMS-GU-WDD311                                                
291700     IF SEGMENT-FINNS                                                     
291800        MOVE TEXT-BEART      TO LPF-BEART                                 
291900     ELSE                                                                 
292000        MOVE SPACE           TO LPF-BEART                                 
292100     END-IF                                                               
292200                                                                          
292300     PERFORM S13-SKRIV-W22416                                             
292400     .                                                                    
292500                                                                          
292600                                                                          
292700 IB-SKAPA-SKRIV-W22414-POST SECTION.                                      
292800     MOVE 'IB-SKAPA-W22414 ' TO CURRENT-SECTION                           
292900                                                                          
293000     MOVE OMSP-IDARTNR       TO AUTO-IDARTNR                              
293100     MOVE OMSP-IDDC          TO AUTO-IDDC                                 
293200     MOVE 'P'                TO AUTO-KDBEHX-PLA                           
293300     MOVE SLAG-IDLEVNR       TO AUTO-IDLEVNR                              
293400     MOVE '2'                TO AUTO-KOMKOD                               
293500     MOVE XLAG-IDANSK        TO AUTO-IDANSK                               
293600                                                                          
293700     PERFORM S11-SKRIV-W22414                                             
293800     .                                                                    
293900                                                                          
294000                                                                          
294100 J-KOLL-UPPD-AV-REGISTER SECTION.                                         
294200     MOVE 'J-KOLL-UPD-REG  ' TO CURRENT-SECTION                           
294300                                                                          
294400     PERFORM JA-KOLL-UPPD-AV-WDK722                                       
294500                                                                          
294600     IF SW-OMSPEC-UTFOERD = JA                                            
294700        MOVE W-TISPECST       TO WS-DASPECST-AAVV                         
294800        MOVE 20               TO WS-DASPECST-SS                           
294900                                                                          
295000        IF SPAR-D904-DASPECST NOT = WS-DASPECST                           
295100           PERFORM JB-BORTTAG-OMSPEC                                      
295200           IF  SW-SKIP-FORSLAG = NEJ                                      
295300               PERFORM JC-NYUPPL-OMSPEC                                   
295400           END-IF                                                         
295500        ELSE                                                              
295600           PERFORM JD-KOLLA-UPPDATERA-OMSPEC                              
295700        END-IF                                                            
295800     ELSE                                                                 
295900        IF SW-SKIP-FORSLAG = NEJ                                          
296000           PERFORM JB-BORTTAG-OMSPEC                                      
296100                                                                          
296200           PERFORM S22-KOLLA-ORSAKSKOD-SPARA                              
296300           IF XLAG-FLJIT = JA OR SW-FLORS = JA                            
296400              PERFORM JE-SKAPA-OMSPEC                                     
296500           END-IF                                                         
296600        ELSE                                                              
296700*--- BÅDE GÄLLANDE PLAN OCH FÖRSLAG SAKNAR AVROP/TOMMA PLANER.            
296800          PERFORM JB-BORTTAG-OMSPEC                                       
296900        END-IF                                                            
297000     END-IF                                                               
297100     .                                                                    
297200     EJECT                                                                
297300                                                                          
297400 JA-KOLL-UPPD-AV-WDK722   SECTION.                                        
297500     MOVE 'JA-KOLL-UPD-WDK7' TO CURRENT-SECTION                           
297600                                                                          
297700     IF SPAR-XLAG-KDLEVPLF NOT = XLAG-KDLEVPLF                            
297800     OR SPAR-XLAG-KDLPSP   NOT = XLAG-KDLPSP                              
297900     OR SPAR-XLAG-TILPSP   NOT = XLAG-TILPSP                              
298000     OR SPAR-XLAG-TIOMSPEC NOT = XLAG-TIOMSPEC                            
298100     OR KDLPORS-03                                                        
298200                                                                          
298300        PERFORM S100-NOLLA-W22415-AREA                                    
298400        MOVE UPDATE-WDK722      TO UPLP-IDPTYP                            
298500        MOVE OMSP-IDARTNR       TO UPLP-IDARTNR                           
298600        MOVE OMSP-IDDC          TO UPLP-IDDC                              
298700        MOVE SPAR-XLAG-KDLEVPLF TO UPLP-KDLEVPLF                          
298800        MOVE SPAR-XLAG-KDLPSP   TO UPLP-KDLPSP                            
298900        MOVE SPAR-XLAG-TILPSP   TO UPLP-TILPSP                            
299000        MOVE SPAR-XLAG-TIOMSPEC TO UPLP-TIOMSPEC                          
299100                                                                          
299200        PERFORM S12-SKRIV-W22415                                          
299300     END-IF                                                               
299400     .                                                                    
299500                                                                          
299600                                                                          
299700 JB-BORTTAG-OMSPEC        SECTION.                                        
299800     MOVE 'JB-BORTTAG-OMSP ' TO CURRENT-SECTION                           
299900                                                                          
300000     PERFORM S100-NOLLA-W22415-AREA                                       
300100     MOVE BORTTAG-OMSPEC     TO UPLP-IDPTYP                               
300200     MOVE OMSP-IDARTNR       TO UPLP-IDARTNR                              
300300     MOVE OMSP-IDDC          TO UPLP-IDDC                                 
300400                                                                          
300500     PERFORM S12-SKRIV-W22415                                             
300600     .                                                                    
300700                                                                          
300800                                                                          
300900 JC-NYUPPL-OMSPEC         SECTION.                                        
301000     MOVE 'JC-NYUPPL-OMSP  ' TO CURRENT-SECTION                           
301100                                                                          
301200     PERFORM S100-NOLLA-W22415-AREA                                       
301300     MOVE NYUPPL-OMSPEC             TO UPLP-IDPTYP                        
301400     MOVE OMSP-IDARTNR              TO UPLP-IDARTNR                       
301500     MOVE OMSP-IDDC                 TO UPLP-IDDC                          
301600     MOVE SLAG-IDLEVNR              TO UPLP-IDLEVNR                       
301700     MOVE LPF-KDLPORS (1)           TO UPLP-KDLPORS-TAB (1)               
301800     MOVE LPF-KDLPORS (2)           TO UPLP-KDLPORS-TAB (2)               
301900     MOVE LPF-KDLPORS (3)           TO UPLP-KDLPORS-TAB (3)               
302000                                                                          
302100     MOVE W-TISPECST                TO WS-DASPECST-AAVV                   
302200     MOVE 20                        TO WS-DASPECST-SS                     
302300     MOVE WS-DASPECST               TO UPLP-DASPECST                      
302400                                                                          
302500     MOVE ZERO                      TO UPLP-KVBEST-PL                     
302600     MOVE ZERO                      TO UPLP-KDPLKOEP                      
302700                                                                          
302800     PERFORM S12-SKRIV-W22415                                             
302900     .                                                                    
303000                                                                          
303100                                                                          
303200 JD-KOLLA-UPPDATERA-OMSPEC SECTION.                                       
303300     MOVE 'JD-UPD-OMSPEC   ' TO CURRENT-SECTION                           
303400                                                                          
303500     IF SPAR-D904-KDLPORS-TAB (1) NOT = LPF-KDLPORS (1)                   
303600     OR SPAR-D904-KDLPORS-TAB (2) NOT = LPF-KDLPORS (2)                   
303700     OR SPAR-D904-KDLPORS-TAB (3) NOT = LPF-KDLPORS (3)                   
303800     OR KDLPORS-03                                                        
303900     OR KDERS-X9                                                          
304000                                                                          
304100        MOVE OMSP-IDARTNR    TO W-IDARTNR-D9                              
304200        MOVE OMSP-IDDC       TO W-IDDC-D9                                 
304300        MOVE SLAG-IDLEVNR    TO W-IDLEVNR                                 
304400                                                                          
304500        PERFORM S100-NOLLA-W22415-AREA                                    
304600        MOVE NYUPPL-OMSPEC             TO UPLP-IDPTYP                     
304700        MOVE OMSP-IDARTNR              TO UPLP-IDARTNR                    
304800        MOVE OMSP-IDDC                 TO UPLP-IDDC                       
304900        MOVE SLAG-IDLEVNR              TO UPLP-IDLEVNR                    
305000        MOVE LPF-KDLPORS (1)           TO UPLP-KDLPORS-TAB (1)            
305100        MOVE LPF-KDLPORS (2)           TO UPLP-KDLPORS-TAB (2)            
305200        MOVE LPF-KDLPORS (3)           TO UPLP-KDLPORS-TAB (3)            
305300                                                                          
305400        MOVE W-TISPECST                TO WS-DASPECST-AAVV                
305500        MOVE 20                        TO WS-DASPECST-SS                  
305600        MOVE WS-DASPECST               TO UPLP-DASPECST                   
305700                                                                          
305800        MOVE ZERO                      TO UPLP-KVBEST-PL                  
305900        MOVE ZERO                      TO UPLP-KDPLKOEP                   
306000                                                                          
306100        PERFORM IMS-GU-WDD904                                             
306200        IF SEGMENT-FINNS                                                  
306300           MOVE UPPDAT-OMSPEC          TO UPLP-IDPTYP                     
306400        ELSE                                                              
306500           MOVE NYUPPL-OMSPEC          TO UPLP-IDPTYP                     
306600        END-IF                                                            
306700                                                                          
306800        PERFORM S12-SKRIV-W22415                                          
306900                                                                          
307000     END-IF                                                               
307100     .                                                                    
307200                                                                          
307300                                                                          
307400 JE-SKAPA-OMSPEC SECTION.                                                 
307500     MOVE 'JE-SKAPA-OMSPEC ' TO CURRENT-SECTION                           
307600                                                                          
307700     MOVE OMSP-IDARTNR       TO W-IDARTNR-D9                              
307800     MOVE OMSP-IDDC          TO W-IDDC-D9                                 
307900     MOVE SLAG-IDLEVNR       TO W-IDLEVNR                                 
308000                                                                          
308100     PERFORM S100-NOLLA-W22415-AREA                                       
308200     MOVE NYUPPL-OMSPEC      TO UPLP-IDPTYP                               
308300     MOVE OMSP-IDARTNR       TO UPLP-IDARTNR                              
308400     MOVE OMSP-IDDC          TO UPLP-IDDC                                 
308500     MOVE SLAG-IDLEVNR       TO UPLP-IDLEVNR                              
308600     MOVE LPF-KDLPORS (1)    TO UPLP-KDLPORS-TAB (1)                      
308700     MOVE LPF-KDLPORS (2)    TO UPLP-KDLPORS-TAB (2)                      
308800     MOVE LPF-KDLPORS (3)    TO UPLP-KDLPORS-TAB (3)                      
308900                                                                          
309000     MOVE W-TISPECST         TO WS-DASPECST-AAVV                          
309100     MOVE 20                 TO WS-DASPECST-SS                            
309200     MOVE WS-DASPECST        TO UPLP-DASPECST                             
309300                                                                          
309400     MOVE ZERO               TO UPLP-KVBEST-PL                            
309500     MOVE ZERO               TO UPLP-KDPLKOEP                             
309600                                                                          
309700     PERFORM IMS-GU-WDD904                                                
309800     IF SEGMENT-FINNS                                                     
309900        MOVE UPPDAT-OMSPEC   TO UPLP-IDPTYP                               
310000     ELSE                                                                 
310100        MOVE NYUPPL-OMSPEC   TO UPLP-IDPTYP                               
310200     END-IF                                                               
310300                                                                          
310400     PERFORM S12-SKRIV-W22415                                             
310500     .                                                                    
310600                                                                          
310700                                                                          
310800 K-KOLL-SKIP-PREL-PLAN   SECTION.                                         
310900     MOVE 'K-KOLL-SKIP-PREL-PLAN' TO CURRENT-SECTION                      
311000*---                                                                      
311100*--- SKIPPA FÖRSLAG OM BÅDE GÄLLANDE PLAN OCH FÖRSLAGET SAKNAR            
311200*--- AVROP.                                                               
311300*---                                                                      
311400     IF SW-FORSLAG-AVROP-SAKNAS = JA                                      
311500       MOVE OMSP-IDARTNR TO W-IDARTNR-D9                                  
311600       MOVE OMSP-IDDC    TO W-IDDC-D9                                     
311700       MOVE SLAG-IDLEVNR TO W-IDLEVNR                                     
311800       MOVE +2           TO W-KDAVROP                                     
311900       PERFORM IMS-GU-WDD905-KVAL                                         
312000       IF SEGMENT-FINNS                                                   
312100         PERFORM KA-KOLL-SKIP-PREL-PLAN                                   
312200       ELSE                                                               
312300         MOVE JA         TO SW-SKIP-FORSLAG                               
312400         MOVE NEJ        TO SW-OMSPEC-UTFOERD                             
312500         MOVE ZERO       TO SPAR-XLAG-KDLPSP                              
312600       END-IF                                                             
312700     END-IF                                                               
312800                                                                          
312900     .                                                                    
313000     SKIP1                                                                
313100 KA-KOLL-SKIP-PREL-PLAN  SECTION.                                         
313200     MOVE 'KA-KOLL-SKIP-PREL-PLAN '  TO CURRENT-SECTION                   
313300                                                                          
313400*--- * KOLL OM GÄLLANDE PLAN HAR MINST ETT AVROP EFTER MFG                
313500*--- * LEDTID (SOM INFALLER UNDER DE 20 FÖRSTA VECKORNA),                 
313600*--- * FÖRSLAGET HAR FLYTTATS MER ÄN 2 VECKOR.                            
313700                                                                          
313800                                                                          
313900     MOVE W-TISPECST          TO WS-DASPECST-AAVV                         
314000     MOVE 20                  TO WS-DASPECST-SS                           
314100     MOVE WS-DASPECST         TO W-DAAVROP-MIN                            
314200                                 W-DAAVROP-MAX                            
314300                                                                          
314400     MOVE W-TISPECST          TO VADD-DATUM-AAVV                          
314500     MOVE 20                  TO W-ANTAL-VECKOR                           
314600     CALL W009VADD USING VADD-DATUM-AAVV W-ANTAL-VECKOR                   
314700     MOVE VADD-DATUM-AAVV     TO W-DAAVROP-MAX-AAVV                       
314800                                                                          
314900                                                                          
315000     MOVE 2  TO W-KDAVROP                                                 
315100     PERFORM IMS-GU-WDD905                                                
315200     IF SEGMENT-FINNS                                                     
315300       MOVE NEJ   TO SW-SKIP-FORSLAG                                      
315400     ELSE                                                                 
315500       MOVE JA    TO SW-SKIP-FORSLAG                                      
315600       MOVE NEJ   TO SW-OMSPEC-UTFOERD                                    
315700       MOVE ZERO  TO SPAR-XLAG-KDLPSP                                     
315800     END-IF                                                               
315900                                                                          
316000     .                                                                    
316100     SKIP1                                                                
316200 Z-FINIT SECTION.                                                         
316300     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
316400                                                                          
316500     CLOSE W22412                                                         
316600           W22414                                                         
316700           W22415                                                         
316800           W22416                                                         
316900                                                                          
317000     MOVE 'S' TO POSTSUM-OPKOD                                            
317100     CALL POSTSUM USING POSTSUM-PARM                                      
317200     .                                                                    
317300                                                                          
317400                                                                          
317500 S01-LAES-W22412  SECTION.                                                
317600                                                                          
317700     READ W22412 INTO OMSP-AREA                                           
317800     AT END                                                               
317900        MOVE HIGH-VALUE   TO OMSP-AREA                                    
318000        SET END-OF-W22412 TO TRUE                                         
318100                                                                          
318200     NOT AT END                                                           
318300        MOVE 'W22412'     TO POSTSUM-FDNAMN                               
318400        MOVE 'W22414D1'   TO POSTSUM-DDNAMN2                              
318500        MOVE SPACE        TO POSTSUM-TRANSTYP                             
318600        CALL POSTSUM USING   POSTSUM-PARM                                 
318700     END-READ                                                             
318800     .                                                                    
318900                                                                          
319000                                                                          
319100 S11-SKRIV-W22414 SECTION.                                                
319200                                                                          
319300     WRITE AUTO-POST FROM AUTO-AREA                                       
319400                                                                          
319500     MOVE 'W22414'        TO POSTSUM-FDNAMN                               
319600     MOVE 'W22414D2'      TO POSTSUM-DDNAMN2                              
319700     MOVE 'AUTO'          TO POSTSUM-TRANSTYP                             
319800     CALL POSTSUM USING      POSTSUM-PARM                                 
319900     .                                                                    
320000                                                                          
320100                                                                          
320200 S12-SKRIV-W22415 SECTION.                                                
320300                                                                          
320400     WRITE UPLP-POST FROM UPLP-AREA                                       
320500                                                                          
320600     MOVE UPLP-IDPTYP     TO POSTSUM-TRANSTYP                             
320700     MOVE 'W22415'        TO POSTSUM-FDNAMN                               
320800     MOVE 'W22414D3'      TO POSTSUM-DDNAMN2                              
320900     CALL POSTSUM USING      POSTSUM-PARM                                 
321000     .                                                                    
321100                                                                          
321200                                                                          
321300 S13-SKRIV-W22416 SECTION.                                                
321400                                                                          
321500     WRITE LPF-POST FROM LPF-AREA                                         
321600                                                                          
321700     MOVE 'W22416'        TO POSTSUM-FDNAMN                               
321800     MOVE 'W22414D4'      TO POSTSUM-DDNAMN2                              
321900     MOVE 'U59'           TO POSTSUM-TRANSTYP                             
322000     CALL POSTSUM USING      POSTSUM-PARM                                 
322100     .                                                                    
322200                                                                          
322300                                                                          
322400 S20-NOLLA-BHDC-RESULTATFLT  SECTION.                                     
322500     MOVE 'S20-NOLLA-BHDC-RESULTATFLT ' TO CURRENT-SECTION                
322600                                                                          
322700     MOVE ZERO                   TO BHDC-KVBEHOV-SUMMA                    
322800     MOVE ZERO                   TO BHDC-KVBEHOV-DESSUTOM                 
322900     MOVE ZERO                   TO BHDC-TIBEHOV-FIRST                    
323000                                                                          
323100     MOVE 1   TO IX-BHDC                                                  
323200     PERFORM UNTIL IX-BHDC > IX-BHDC-MAX                                  
323300       MOVE ZERO                 TO BHDC-KVBEHOV-VECKA(IX-BHDC)           
323400                                                                          
323500       ADD 1  TO IX-BHDC                                                  
323600     END-PERFORM                                                          
323700                                                                          
323800     .                                                                    
323900     EJECT                                                                
324000 S21-NOLLA-TAB2-RESULTATFLT  SECTION.                                     
324100     MOVE 'S20-NOLLA-BHDC-RESULTATFLT ' TO CURRENT-SECTION                
324200                                                                          
324300     MOVE ZERO                   TO TAB2-KVBEHOV-SUMMA                    
324400     MOVE ZERO                   TO TAB2-KVBEHOV-DESSUTOM                 
324500     MOVE ZERO                   TO TAB2-TIBEHOV-FIRST                    
324600                                                                          
324700     MOVE 1   TO IX-BHDC                                                  
324800     PERFORM UNTIL IX-BHDC > IX-BHDC-MAX                                  
324900       MOVE ZERO                 TO TAB2-KVBEHOV-VECKA(IX-BHDC)           
325000                                                                          
325100       ADD 1  TO IX-BHDC                                                  
325200     END-PERFORM                                                          
325300                                                                          
325400     .                                                                    
325500     EJECT                                                                
325600 S22-KOLLA-ORSAKSKOD-SPARA   SECTION.                                     
325700     MOVE 'S22-KOLLA-ORSAKSKOD-SPARA  ' TO CURRENT-SECTION                
325800                                                                          
325900     MOVE NEJ TO SW-FLORS                                                 
326000     MOVE 1   TO IX-ORS                                                   
326100     PERFORM UNTIL IX-ORS > 3                                             
326200                OR SW-FLORS = JA                                          
326300        MOVE LPF-KDLPORS(IX-ORS) TO W-KDLPORS                             
326400        IF W-SPARA-FORSLAG                                                
326500           MOVE JA  TO SW-FLORS                                           
326600        END-IF                                                            
326700        ADD 1       TO IX-ORS                                             
326800     END-PERFORM                                                          
326900     .                                                                    
327000     EJECT                                                                
327100 S99-ABEND SECTION.                                                       
327200                                                                          
327300     MOVE 'S'        TO POSTSUM-OPKOD                                     
327400     CALL POSTSUM USING POSTSUM-PARM                                      
327500     CALL ABEND USING   RKOD-ABEND                                        
327600     .                                                                    
327700                                                                          
327800                                                                          
327900 S100-NOLLA-W22415-AREA SECTION.                                          
328000     MOVE 'S100-NOLL-W22415'  TO CURRENT-S-SECTION                        
328100                                                                          
328200     MOVE SPACE   TO UPLP-IDPTYP                                          
328300                     UPLP-IDDC                                            
328400                     UPLP-IDLEVNR                                         
328500                     UPLP-KDLEVPLF                                        
328600     MOVE ZERO    TO UPLP-IDARTNR                                         
328700                     UPLP-DAAVROP-AVS                                     
328800                     UPLP-DASPECST                                        
328900                     UPLP-KDAVROP                                         
329000                     UPLP-KDPLKOEP                                        
329100                     UPLP-KDLPSP                                          
329200                     UPLP-KVAVROP                                         
329300                     UPLP-KVBEST-PL                                       
329400                     UPLP-TIAVRDAT-DISP                                   
329500                     UPLP-TIAVRDAT-INL                                    
329600                     UPLP-TILEVDAG                                        
329700                     UPLP-TILPSP                                          
329800                     UPLP-TIOMSPEC                                        
329900     MOVE 1 TO IX-ORS                                                     
330000     PERFORM UNTIL IX-ORS > 3                                             
330100        MOVE ZERO TO UPLP-KDLPORS-TAB(IX-ORS)                             
330200        ADD 1     TO IX-ORS                                               
330300     END-PERFORM                                                          
330400     .                                                                    
330500     EJECT                                                                
330600                                                                          
330700 S102-BERAKNA-VECKODIFFERENS SECTION.                                     
330800     MOVE 'S102-BERAKNA-VECKODIFFERENS' TO CURRENT-SECTION                
330900                                                                          
331000     MOVE ZERO            TO W-VECKO-DIFF                                 
331100     MOVE W-DATUM-TOM     TO W-DATUM-TOM-AAVV                             
331200     MOVE W-DATUM-FROM    TO W-DATUM-FROM-AAVV                            
331300                                                                          
331400     PERFORM UNTIL W-DATUM-FROM-AAVV = W-DATUM-TOM-AAVV                   
331500        IF W-DATUM-FROM-AA = W-DATUM-TOM-AA                               
331600           COMPUTE W-VECKO-DIFF =                                         
331700           W-VECKO-DIFF + W-DATUM-TOM-AAVV - W-DATUM-FROM-AAVV            
331800           MOVE W-DATUM-FROM-AAVV TO W-DATUM-TOM-AAVV                     
331900        ELSE                                                              
332000           MOVE W-DATUM-FROM-AAVV TO W-AAVV-JUST                          
332100           MOVE 53           TO W-AAVV-JUST-VV                            
332200           MOVE 'AAVV'       TO DAT-KDDATFORM                             
332300           MOVE W-AAVV-JUST  TO DAT-I-TIDATUM                             
332400           CALL WDATKONV USING  DAT-KDDATFORM                             
332500                                DAT-I-TIDATUM                             
332600                                DAT-O-TIDATUM                             
332700                                DAT-KDSVAR                                
332800           IF DAT-KDSVAR-OK                                               
332900* ÅRET HAR 53 VECKOR                                                      
333000              COMPUTE W-VECKO-DIFF =                                      
333100                      W-VECKO-DIFF + (54 - W-DATUM-FROM-VV)               
333200              COMPUTE W-DATUM-FROM-AA = W-DATUM-FROM-AA + 1               
333300              MOVE 1         TO W-DATUM-FROM-VV                           
333400           ELSE                                                           
333500* ÅRET HAR 52 VECKOR                                                      
333600              COMPUTE W-VECKO-DIFF =                                      
333700                      W-VECKO-DIFF + (53 - W-DATUM-FROM-VV)               
333800              COMPUTE W-DATUM-FROM-AA = W-DATUM-FROM-AA + 1               
333900              MOVE 1         TO W-DATUM-FROM-VV                           
334000           END-IF                                                         
334100        END-IF                                                            
334200     END-PERFORM                                                          
334300                                                                          
334400     .                                                                    
334500     EJECT                                                                
334600                                                                          
334700 S200-KOLLA-SKRIV-NYTT-FORSLAG SECTION.                                   
334800     MOVE 'S200-KOLLA-SKRIV'  TO CURRENT-S-SECTION                        
334900                                                                          
335000     IF SW-FOERSTA-AVROPSVECKA  = JA                                      
335100        IF SW-AUT-PLAN = JA                                               
335200           MOVE W-DAAVROP-AVS(3:4) TO W-AAVV                              
335300           IF W-AAVV > AKT-DATUM-AAVV-35                                  
335400              MOVE 'CALL35'     TO POSTSUM-FDNAMN                         
335500              MOVE 'ORS.KOD'    TO POSTSUM-DDNAMN2                        
335600              MOVE '>35'        TO POSTSUM-TRANSTYP                       
335700              MOVE '1STCALL>35' TO LPF-TELPORSX                           
335800              CALL POSTSUM USING POSTSUM-PARM                             
335900              MOVE NEJ          TO SW-AUT-PLAN                            
336000           END-IF                                                         
336100        END-IF                                                            
336200                                                                          
336300        PERFORM S210-KOLLA-OM-SPARA-FORSLAG                               
336400        MOVE NEJ TO SW-FOERSTA-AVROPSVECKA                                
336500     END-IF                                                               
336600                                                                          
336700     IF SW-SKIP-FORSLAG = NEJ                                             
336800        PERFORM S300-SKRIV-FORSLAG                                        
336900     END-IF                                                               
337000     .                                                                    
337100                                                                          
337200                                                                          
337300 S210-KOLLA-OM-SPARA-FORSLAG SECTION.                                     
337400     MOVE 'S210-KOLLA-SP-FO'  TO CURRENT-S-SECTION                        
337500                                                                          
337600     PERFORM S22-KOLLA-ORSAKSKOD-SPARA                                    
337700     IF XLAG-FLJIT = JA OR SW-FLORS = JA                                  
337800        MOVE NEJ    TO SW-SKIP-FORSLAG                                    
337900     ELSE                                                                 
338000        MOVE OMSP-IDARTNR TO W-IDARTNR-D9                                 
338100        MOVE OMSP-IDDC    TO W-IDDC-D9                                    
338200        MOVE SLAG-IDLEVNR TO W-IDLEVNR                                    
338300        PERFORM IMS-GU-WDD902                                             
338400        IF SEGMENT-FINNS                                                  
338500           PERFORM S210A-KOLL-SKIP-PREL-PLAN                              
338600        ELSE                                                              
338700           MOVE NEJ       TO SW-SKIP-FORSLAG                              
338800        END-IF                                                            
338900     END-IF                                                               
339000     .                                                                    
339100                                                                          
339200                                                                          
339300 S210A-KOLL-SKIP-PREL-PLAN SECTION.                                       
339400     MOVE 'S210A-KOLL-SKIP '  TO CURRENT-S-SECTION                        
339500                                                                          
339600* --- KOLL MELLAN GÄLLANDE PLAN OCH PRELIMINÄRT FÖRSLAG                   
339700* --- AVSEENDE DIFF MELLAN FÖRSTA AVROP                                   
339800* --- (SOM INFALLER UNDER DE 20 FÖRSTA VECKORNA)                          
339900                                                                          
340000     MOVE W-DAAVROP-AVS       TO W-DAAVROP-MIN                            
340100                                 W-DAAVROP-MAX                            
340200                                                                          
340300     MOVE 20 TO W-ANTAL-VECKOR                                            
340400     MOVE W-DAAVROP-MAX-AAVV  TO W-DAAVROP                                
340500     CALL W009VADD USING W-DAAVROP W-ANTAL-VECKOR                         
340600     MOVE W-DAAVROP           TO W-DAAVROP-MAX-AAVV                       
340700                                                                          
340800     MOVE 2  TO W-KDAVROP                                                 
340900     PERFORM IMS-GU-WDD905                                                
341000     IF SEGMENT-FINNS                                                     
341100        MOVE D905-DAAVROP-AVS TO W-GALL-AVROP                             
341200        MOVE W-DAAVROP-AVS    TO W-PREL-AVROP                             
341300* ---   DET FINNS BÅDE GÄLLANDE OCH PREL                                  
341400* ---   KOLLA DIFF, EV MOVE JA TILL SW-SKIP-FORSLAG                       
341500        PERFORM S210AA-BER-DIFF                                           
341600     ELSE                                                                 
341700        MOVE NEJ   TO SW-SKIP-FORSLAG                                     
341800     END-IF                                                               
341900     .                                                                    
342000                                                                          
342100                                                                          
342200 S210AA-BER-DIFF      SECTION.                                            
342300     MOVE 'S210AA-BER-DIFF '  TO CURRENT-S-SECTION                        
342400                                                                          
342500     IF W-GALL-AVROP >= W-PREL-AVROP                                      
342600        COMPUTE W-VECKO-DIFF-TEST =                                       
342700              ((W-GALL-AVROP-AA - W-PREL-AVROP-AA) * 52                   
342800            +   W-GALL-AVROP-VV - W-PREL-AVROP-VV)                        
342900     ELSE                                                                 
343000        COMPUTE W-VECKO-DIFF-TEST =                                       
343100              ((W-PREL-AVROP-AA - W-GALL-AVROP-AA) * 52                   
343200            +   W-PREL-AVROP-VV - W-GALL-AVROP-VV)                        
343300     END-IF                                                               
343400                                                                          
343500     IF W-VECKO-DIFF-TEST > 2                                             
343600        MOVE NEJ TO SW-SKIP-FORSLAG                                       
343700     ELSE                                                                 
343800        MOVE JA  TO SW-SKIP-FORSLAG                                       
343900     END-IF                                                               
344000     .                                                                    
344100     EJECT                                                                
344200                                                                          
344300 S300-SKRIV-FORSLAG   SECTION.                                            
344400     MOVE 'S300-SKRIV-FORS '  TO CURRENT-S-SECTION                        
344500                                                                          
344600     PERFORM S100-NOLLA-W22415-AREA                                       
344700                                                                          
344800     MOVE OMSP-IDARTNR       TO W-IDARTNR-D9                              
344900     MOVE OMSP-IDDC          TO W-IDDC-D9                                 
345000     MOVE SLAG-IDLEVNR       TO W-IDLEVNR                                 
345100     MOVE 1                  TO W-KDAVROP                                 
345200     MOVE W-DAAVROP-AVS      TO W-DAAVROP-MIN                             
345300                                W-DAAVROP-MAX                             
345400     PERFORM IMS-GU-WDD905                                                
345500     IF SEGMENT-FINNS                                                     
345600        MOVE UPD-AVROP       TO UPLP-IDPTYP                               
345700        MOVE OMSP-IDARTNR    TO UPLP-IDARTNR                              
345800        MOVE OMSP-IDDC       TO UPLP-IDDC                                 
345900        MOVE SLAG-IDLEVNR    TO UPLP-IDLEVNR                              
346000        MOVE W-DAAVROP-AVS   TO UPLP-DAAVROP-AVS                          
346100        MOVE 1               TO UPLP-KDAVROP                              
346200        MOVE SPAR-KVAVROP    TO UPLP-KVAVROP                              
346300*--- OM SEGMENTET ÄR BORTTAGET NÄR VI KOMMER TILL W22415                  
346400*--- BEHÖVER VI ALLA VÄRDENA FÖR ATT GÖRA EN INSERT                       
346500        MOVE W-TIAVRDAT-DISP TO UPLP-TIAVRDAT-DISP                        
346600        MOVE W-TIAVRDAT-INL  TO UPLP-TIAVRDAT-INL                         
346700        MOVE SPAR-TILEVDAG   TO UPLP-TILEVDAG                             
346800     ELSE                                                                 
346900        MOVE OMSP-IDARTNR    TO UPLP-IDARTNR                              
347000        MOVE OMSP-IDDC       TO UPLP-IDDC                                 
347100        MOVE SLAG-IDLEVNR    TO UPLP-IDLEVNR                              
347200        PERFORM IMS-GU-WDD902                                             
347300        IF SEGMENT-SAKNAS                                                 
347400           MOVE NYUPPL-LEV   TO UPLP-IDPTYP                               
347500           PERFORM S12-SKRIV-W22415                                       
347600        END-IF                                                            
347700        MOVE NYUPPL-AVROP    TO UPLP-IDPTYP                               
347800        MOVE W-DAAVROP-AVS   TO UPLP-DAAVROP-AVS                          
347900        MOVE 1               TO UPLP-KDAVROP                              
348000        MOVE SPAR-KVAVROP    TO UPLP-KVAVROP                              
348100        MOVE W-TIAVRDAT-DISP TO UPLP-TIAVRDAT-DISP                        
348200        MOVE W-TIAVRDAT-INL  TO UPLP-TIAVRDAT-INL                         
348300        MOVE SPAR-TILEVDAG   TO UPLP-TILEVDAG                             
348400     END-IF                                                               
348500                                                                          
348600     PERFORM S12-SKRIV-W22415                                             
348700     .                                                                    
348800                                                                          
348900                                                                          
349000* --- IMS SEKTIONER ---                                                   
349100                                                                          
349200 IMS-GU-WDK701 SECTION.                                                   
349300     MOVE 'IMS-GU-WDK701   '  TO CURRENT-IMS-SECTION                      
349400                                                                          
349500     MOVE SPACE               TO ALL-SSA                                  
349600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
349700          DELIMITED BY SIZE INTO SSA1                                     
349800     MOVE '    '              TO GODK-STATUSKODER                         
349900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
350000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
350100     PERFORM IMS-STATUSKONTROLL                                           
350200     .                                                                    
350300                                                                          
350400                                                                          
350500 IMS-GU-WDK711 SECTION.                                                   
350600     MOVE 'IMS-GU-WDK711   '  TO CURRENT-IMS-SECTION                      
350700                                                                          
350800     MOVE SPACE               TO ALL-SSA                                  
350900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
351000          DELIMITED BY SIZE INTO SSA1                                     
351100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
351200          DELIMITED BY SIZE INTO SSA2                                     
351300     MOVE '    '              TO GODK-STATUSKODER                         
351400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
351500     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
351600     PERFORM IMS-STATUSKONTROLL                                           
351700     .                                                                    
351800                                                                          
351900                                                                          
352000 IMS-GNP-WDK711-REF SECTION.                                              
352100     MOVE 'IMS-GNP-WDK711-REF'  TO CURRENT-IMS-SECTION                    
352200                                                                          
352300     MOVE SPACE               TO ALL-SSA                                  
352400     STRING 'WDK711  (IDDCREF  =' W-IDDC-X ')'                            
352500          DELIMITED BY SIZE INTO SSA1                                     
352600     MOVE '  GE'              TO GODK-STATUSKODER                         
352700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
352800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
352900     PERFORM IMS-STATUSKONTROLL                                           
353000     .                                                                    
353100     EJECT                                                                
353200                                                                          
353300 IMS-GU-WDK712 SECTION.                                                   
353400     MOVE 'IMS-GU-WDK712   '  TO CURRENT-IMS-SECTION                      
353500                                                                          
353600     MOVE SPACE               TO ALL-SSA                                  
353700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
353800          DELIMITED BY SIZE INTO SSA1                                     
353900     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
354000          DELIMITED BY SIZE INTO SSA2                                     
354100     MOVE '  GE'              TO GODK-STATUSKODER                         
354200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
354300     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
354400     PERFORM IMS-STATUSKONTROLL                                           
354500     .                                                                    
354600                                                                          
354700                                                                          
354800 IMS-GU-WDK722 SECTION.                                                   
354900     MOVE 'IMS-GU-WDK722   '  TO CURRENT-IMS-SECTION                      
355000                                                                          
355100     MOVE SPACE               TO ALL-SSA                                  
355200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
355300          DELIMITED BY SIZE INTO SSA1                                     
355400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
355500          DELIMITED BY SIZE INTO SSA2                                     
355600     MOVE 'WDK722 '           TO SSA3                                     
355700     MOVE '    '              TO GODK-STATUSKODER                         
355800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
355900     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
356000     PERFORM IMS-STATUSKONTROLL                                           
356100     .                                                                    
356200                                                                          
356300                                                                          
356400 IMS-GU-WDF106-SHIP-O SECTION.                                            
356500     MOVE 'IMS-GU-WDF106-SO' TO CURRENT-IMS-SECTION                       
356600                                                                          
356700     MOVE SPACE               TO ALL-SSA                                  
356800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
356900          DELIMITED BY SIZE INTO SSA1                                     
357000     MOVE 'WDF106 '           TO SSA2                                     
357100     MOVE '  GE'              TO GODK-STATUSKODER                         
357200     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF106 SSA1 SSA2               
357300     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
357400     PERFORM IMS-STATUSKONTROLL                                           
357500     .                                                                    
357600     EJECT                                                                
357700                                                                          
357800 IMS-GU-WDF116-SHIP  SECTION.                                             
357900     MOVE 'IMS-GU-WDF116-SHIP ' TO CURRENT-IMS-SECTION                    
358000                                                                          
358100     MOVE SPACE               TO ALL-SSA                                  
358200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
358300          DELIMITED BY SIZE INTO SSA1                                     
358400     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
358500          DELIMITED BY SIZE INTO SSA2                                     
358600     MOVE '  GE'              TO GODK-STATUSKODER                         
358700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
358800     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
358900     PERFORM IMS-STATUSKONTROLL                                           
359000     .                                                                    
359100                                                                          
359200                                                                          
359300 IMS-GU-WDF3A1 SECTION.                                                   
359400     MOVE 'IMS-GU-WDF3A1   ' TO CURRENT-IMS-SECTION                       
359500                                                                          
359600     MOVE SPACE               TO ALL-SSA                                  
359700     STRING 'WDF3A1      '                                                
359800          DELIMITED BY SIZE INTO SSA1                                     
359900     MOVE '  GE'              TO GODK-STATUSKODER                         
360000     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-WDF3A1 SSA1                    
360100     MOVE WDF3-STATUS-CODE    TO STATUS-WS                                
360200     PERFORM IMS-STATUSKONTROLL                                           
360300     .                                                                    
360400                                                                          
360500                                                                          
360600 IMS-GN-WDF3A1 SECTION.                                                   
360700     MOVE 'IMS-GN-WDF3A1   '  TO CURRENT-IMS-SECTION                      
360800                                                                          
360900     MOVE SPACE               TO ALL-SSA                                  
361000     STRING 'WDF3A1      '                                                
361100          DELIMITED BY SIZE INTO SSA1                                     
361200     MOVE '  GEGB'            TO GODK-STATUSKODER                         
361300     CALL CBLTDLI USING GN WDF3-PCB DLI-IO-WDF3A1 SSA1                    
361400     MOVE WDF3-STATUS-CODE    TO STATUS-WS                                
361500     PERFORM IMS-STATUSKONTROLL                                           
361600     .                                                                    
361700                                                                          
361800                                                                          
361900 IMS-GN-WDB601 SECTION.                                                   
362000     MOVE 'IMS-GN-WDB601   '  TO CURRENT-IMS-SECTION                      
362100                                                                          
362200     MOVE SPACE               TO ALL-SSA                                  
362300     MOVE 'WDB601 '           TO SSA1                                     
362400     MOVE '  GB'              TO GODK-STATUSKODER                         
362500     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
362600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
362700     PERFORM IMS-STATUSKONTROLL                                           
362800     .                                                                    
362900                                                                          
363000                                                                          
363100 IMS-GU-WDD311 SECTION.                                                   
363200     MOVE 'IMS-GU-WDD311   ' TO CURRENT-IMS-SECTION                       
363300                                                                          
363400     MOVE SPACE               TO ALL-SSA                                  
363500     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
363600          DELIMITED BY SIZE INTO SSA1                                     
363700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
363800          DELIMITED BY SIZE INTO SSA2                                     
363900     MOVE '  GE'              TO GODK-STATUSKODER                         
364000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
364100     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
364200     PERFORM IMS-STATUSKONTROLL                                           
364300     .                                                                    
364400                                                                          
364500                                                                          
364600 IMS-GU-WDD901 SECTION.                                                   
364700     MOVE 'IMS-GU-WDD901   ' TO CURRENT-IMS-SECTION                       
364800                                                                          
364900     MOVE SPACE               TO ALL-SSA                                  
365000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
365100          DELIMITED BY SIZE INTO SSA1                                     
365200     MOVE '  GE'              TO GODK-STATUSKODER                         
365300     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
365400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
365500     PERFORM IMS-STATUSKONTROLL                                           
365600     .                                                                    
365700                                                                          
365800                                                                          
365900 IMS-GU-WDD902 SECTION.                                                   
366000     MOVE 'IMS-GU-WDD902   ' TO CURRENT-IMS-SECTION                       
366100                                                                          
366200     MOVE SPACE               TO ALL-SSA                                  
366300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
366400          DELIMITED BY SIZE INTO SSA1                                     
366500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
366600          DELIMITED BY SIZE INTO SSA2                                     
366700     MOVE '  GE'              TO GODK-STATUSKODER                         
366800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
366900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
367000     PERFORM IMS-STATUSKONTROLL                                           
367100     .                                                                    
367200                                                                          
367300                                                                          
367400 IMS-GU-WDD904 SECTION.                                                   
367500     MOVE 'IMS-GU-WDD904   ' TO CURRENT-IMS-SECTION                       
367600                                                                          
367700     MOVE SPACE               TO ALL-SSA                                  
367800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
367900          DELIMITED BY SIZE INTO SSA1                                     
368000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
368100          DELIMITED BY SIZE INTO SSA2                                     
368200     MOVE 'WDD904 '           TO SSA3                                     
368300     MOVE '  GE'              TO GODK-STATUSKODER                         
368400     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
368500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
368600     PERFORM IMS-STATUSKONTROLL                                           
368700     .                                                                    
368800                                                                          
368900                                                                          
369000 IMS-GU-WDD905 SECTION.                                                   
369100     MOVE 'IMS-GU-WDD905   ' TO CURRENT-IMS-SECTION                       
369200                                                                          
369300     MOVE SPACE               TO ALL-SSA                                  
369400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
369500          DELIMITED BY SIZE INTO SSA1                                     
369600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
369700          DELIMITED BY SIZE INTO SSA2                                     
369800     STRING 'WDD905  (DAAVROP >=' W-DAAVROP-MIN-X                         
369900                    '&DAAVROP <=' W-DAAVROP-MAX-X                         
370000                    '&KDAVROP  =' W-KDAVROP-X ')'                         
370100          DELIMITED BY SIZE INTO SSA3                                     
370200     MOVE '  GE'              TO GODK-STATUSKODER                         
370300     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3         
370400     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
370500     PERFORM IMS-STATUSKONTROLL                                           
370600     .                                                                    
370700                                                                          
370800                                                                          
370900 IMS-GNP-WDD905-F SECTION.                                                
371000     MOVE 'IMS-GHNP-WDD905F' TO CURRENT-IMS-SECTION                       
371100                                                                          
371200     MOVE SPACE               TO ALL-SSA                                  
371300     MOVE 'WDD902  *F'        TO SSA1                                     
371400     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
371500          DELIMITED BY SIZE INTO SSA2                                     
371600     MOVE '  GE'              TO GODK-STATUSKODER                         
371700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
371800     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
371900     PERFORM IMS-STATUSKONTROLL                                           
372000     .                                                                    
372100                                                                          
372200                                                                          
372300 IMS-GNP-WDD905-N SECTION.                                                
372400     MOVE 'IMS-GNP-WDD905N ' TO CURRENT-IMS-SECTION                       
372500                                                                          
372600     MOVE SPACE               TO ALL-SSA                                  
372700     MOVE 'WDD902 '           TO SSA1                                     
372800     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
372900          DELIMITED BY SIZE INTO SSA2                                     
373000     MOVE '  GE'              TO GODK-STATUSKODER                         
373100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
373200     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
373300     PERFORM IMS-STATUSKONTROLL                                           
373400     .                                                                    
373500                                                                          
373600                                                                          
373700 IMS-GU-WDD905-KVAL SECTION.                                              
373800     MOVE 'IMS-GU-WDD905-KVAL ' TO CURRENT-IMS-SECTION                    
373900                                                                          
374000     MOVE SPACE               TO ALL-SSA                                  
374100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
374200          DELIMITED BY SIZE INTO SSA1                                     
374300     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
374400          DELIMITED BY SIZE INTO SSA2                                     
374500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
374600          DELIMITED BY SIZE INTO SSA3                                     
374700     MOVE '  GE'              TO GODK-STATUSKODER                         
374800     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3         
374900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
375000     PERFORM IMS-STATUSKONTROLL                                           
375100     .                                                                    
375200                                                                          
375300                                                                          
375400 IMS-GU-WDD704 SECTION.                                                   
375500     MOVE 'IMS-GU-WDD704   ' TO CURRENT-IMS-SECTION                       
375600                                                                          
375700     MOVE SPACE               TO ALL-SSA                                  
375800     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
375900          DELIMITED BY SIZE INTO SSA1                                     
376000     MOVE 'WDD704 '           TO SSA2                                     
376100     MOVE '  GE'              TO GODK-STATUSKODER                         
376200     CALL CBLTDLI USING GU  WDD7-PCB DLI-IO-WDD704 SSA1 SSA2              
376300     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
376400     PERFORM IMS-STATUSKONTROLL                                           
376500     .                                                                    
376600                                                                          
376700                                                                          
376800                                                                          
376900 IMS-STATUSKONTROLL SECTION.                                              
377000                                                                          
377100     SET STATUS-IX TO 1                                                   
377200     SEARCH GODK-STATUS                                                   
377300       AT END                                                             
377400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
377500           DELIMITED BY SIZE INTO FELTEXT                                 
377600         DISPLAY FELTEXT                                                  
377700         CALL FELLOG                                                      
377800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
377900         CONTINUE                                                         
378000     END-SEARCH                                                           
378100     .                                                                    
378200     EJECT                                                                
378300*    -COPY WY2000P3                                                       
378400     EJECT                                                                
378500*    -COPY WY2000P1                                                       
