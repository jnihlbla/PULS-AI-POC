000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0110200.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   94/11/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BEHANDLING AV ALLMÄNNA R05:OR.                                   
001000*        R05:ORNA ANGER UPPDATERING AV VISSA FÄLT I WDK611.               
001100*        VILKET FÄLT SOM SKA UPPDATERAS, OCH VILKET VÄRDE DET             
001200*        SKA FÅ ANGES I TRANSAKTIONEN.                                    
001300*        PROGRAMMET KONTROLLERAR ATT ETT TILLÅTET FÄLTNAMN ANGETTS        
001400*        OCH ATT ETT VETTIGT VÄRDE TILL AKTUELLT FÄLT ANGETTS.            
001500*        NUMERISKA VÄRDEN KONVERTERAS TILL PACKAT FORMAT.                 
001600*        FÖR VISSA FÄLT SKA ÄVEN BEFINTLIGT VÄRDE KONTROLLERAS            
001700*        MOT ETT VÄRDE SOM ANGETTS I TRANSAKTIONEN.                       
001800*        VISSA FÄLT KRÄVER YTTERLIGARE SPECIELLA KONTROLLER               
001900*        OCH SPECIELL BEHANDLING.                                         
002000*        FELLISTA ÖVER FELAKTIGA TRANSAKTIONER SKRIVS TILL UT-            
002100*        TRATTEN.                                                         
002200*        FELKODER SOM SÄTTS:                                              
002300*         011     NYTT DATA / BEF.DATA EJ NUMERISKT                       
002400*         01E     ANTAL TECKEN I NYTT-/BEF DATA > MAXLÄNGD                
002500*         020     FELAKTIGT FÄLT (W092M002 FEL)                           
002600*         022     BEFINTLIGT VÄRDE SAKNAS I ARTREG (IDPROENH)             
002700*         023     BEFINTLIGT VÄRDE EJ SAMMA SOM ARTREG                    
002800*         024     ARTIKELNR SAKNAS                                        
002900*         025     MAX ANTAL VÄRDEN REDAN UTNYTTJAT (IDPROENH)             
003000*         027     FELAKTIGT ADART-VÄRDE                                   
003100*         041-047 FELKODER FÖR KDVSOP                                     
003200*                                                                         
003300*        PROGRAMMET UPPDATERAR WDK6                                       
003400*                              WDT3                                       
003500*                                                                         
003600*        PROGRAMMET LOGGAR SALDOÄNDRINGAR PÅ DATABASEN                    
003700*                                          WDL9/WLLOGA                    
003800*                                                                         
003900*    ABENDKODER:                                                          
004000*        U0016 -  FEL RETURKOD FRÅN SORT                                  
004100*        U0999 -  FEL STATUSKOD FRÅN IMS                                  
004200*                                                                         
004300*                                                                         
004400*    ÄNDRINGAR:                                                           
004500*        2006-JAN  ETRACKER=1986420. SKAPA LEV-PLANELARM VID              
004600*                  FÖRÄNDRING AV TIFINLV OCH NÄR ARTIKELN SAM-            
004700*                  TIDIGT ÄR ERSÄTTANDE I ERS MED EK 01-09.               
004800*                  BÅDE ERSATT OCH ERSÄTTANDE ARTIKEL SKALL LARMAS        
004900*                  MED ORSAK=09.     TILLÄGG AV WLXXBJ11-ISRT FÖR         
005000*                                    LARM PÅ 2204 HTR          /CE        
005100*                                                                         
005200*    ÄNDRING:                                                             
005300*        2006-JAN. TILLÄGG AV EKONOMISK HÄNDELSE KDEKOHT='M21'            
005400*                  FÖR US OCH CA. /M.A.                                   
005500*                  --  KOD FÖR INSERT A17-KOD PÅ WDR8                     
005600*                  --  TILLFÄLLIGT KOMMENTERAD VID ÄT 06:2                
005700*                  --  P.G.A. FÖRSENADE USA-TESTER.                       
005800*                                                                         
005900*    ÄNDRING:                                                             
006000*        2013.   WDK612-SEGMENTET FLYTTAT TILL WDT311                     
006100                                                                          
006200     SKIP3                                                                
006300 ENVIRONMENT DIVISION.                                                    
006400     SKIP2                                                                
006500 INPUT-OUTPUT SECTION.                                                    
006600                                                                          
006700 FILE-CONTROL.                                                            
006800     SKIP2                                                                
006900*          --- R05:OR FRÅN TRATTEN (PTYP 042)                             
007000     SELECT W09279-IN                  ASSIGN TO W01102D1.                
007100     SKIP2                                                                
007200*          --- FEL-TRANSAKTIONER TILL UT-TRATTEN                          
007300     SELECT W01112-FEL                 ASSIGN TO W01102D2.                
007400     SKIP2                                                                
007500*          --- OBEHANDLADE R05:OR FRÅN INFILEN                            
007600     SELECT W09279-UT                  ASSIGN TO W01102D3.                
007700     SKIP2                                                                
007800*          --- SORT-FIL                                                   
007900     SELECT SORTFIL                    ASSIGN TO W01102DS.                
008000     EJECT                                                                
008100 DATA DIVISION.                                                           
008200     SKIP2                                                                
008300 FILE SECTION.                                                            
008400     SKIP3                                                                
008500 FD  W09279-IN                                                            
008600     RECORDING       V                                                    
008700     BLOCK CONTAINS  0.                                                   
008800                                                                          
008900*01  -COPY W011042      -L.                                               
009000     SKIP3                                                                
009100 FD  W01112-FEL                                                           
009200     RECORDING       V                                                    
009300     BLOCK CONTAINS  0.                                                   
009400                                                                          
009500 01  FEL-POST.                                                            
009600*    03  -COPY W092W001  -L.                                              
009700     03  FILLER                  PIC X(16).                               
009800     03  FILLER                  PIC X.                                   
009900     03  FILLER                  PIC X(25).                               
010000     SKIP3                                                                
010100 FD  W09279-UT                                                            
010200     RECORDING       V                                                    
010300     BLOCK CONTAINS  0.                                                   
010400                                                                          
010500 01  UT-POST.                                                             
010600*    03  -COPY W011042    -PRE UT-  -L.                                   
010700     SKIP3                                                                
010800 SD  SORTFIL.                                                             
010900                                                                          
011000 01  SORT-POST.                                                           
011100     03  SORT-RANDOMKEY           PIC X(4).                               
011200*    03  -COPY W011042  -PRE SORT-                                        
011300                                                                          
011400     EJECT                                                                
011500 WORKING-STORAGE SECTION.                                                 
011600                                                                          
011700*    -- CHECKED BY WY2000                                                 
011800                                                                          
011900 77  IDPGM                       PIC X(8)    VALUE 'W0110200'.            
012000 77  JA                          PIC X       VALUE 'J'.                   
012100 77  NEJ                         PIC X       VALUE 'N'.                   
012200                                                                          
012300 77  W09279-EOF-SW               PIC X       VALUE 'N'.                   
012400     88  END-OF-W09279                       VALUE 'J'.                   
012500                                                                          
012600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
012700     88  END-OF-SORTFIL                      VALUE 'J'.                   
012800                                                                          
012900 77  ALARM-EOP-UPPD-SW           PIC X       VALUE 'N'.                   
013000     88  ALARM-EOP-UPPD-JA                   VALUE 'J'.                   
013301                                                                          
013401 77  WRITE-WDT5-SW               PIC X       VALUE 'N'.                   
013501     88  WRITE-WDT5                          VALUE 'J'.                   
013600                                                                          
013700 77  SW-WDK601-UPPD              PIC X       VALUE 'N'.                   
013800 77  SW-EMB-Q0-UPPD              PIC X       VALUE 'N'.                   
013900 77  SW-EMB-Q1-UPPD              PIC X       VALUE 'N'.                   
014000 77  SW-EMB-Q2-UPPD              PIC X       VALUE 'N'.                   
014100 77  SW-BEFT-UPPD                PIC X       VALUE 'N'.                   
014200 77  SW-KDFORP-UPPD              PIC X       VALUE 'N'.                   
014300 77  SW-KDFORPPL-UPPD            PIC X       VALUE 'N'.                   
014400 77  SW-KDFORPGP-UPPD            PIC X       VALUE 'N'.                   
014500 77  SW-KDFORPUF-UPPD            PIC X       VALUE 'N'.                   
014600 77  SW-LARM-09                  PIC X       VALUE 'N'.                   
014700 77  SW-SKAPA-A17                PIC X       VALUE 'N'.                   
014800 77  WS-KDPRODSL-A17             PIC 9(2)    VALUE ZERO.                  
014900 77  WS-KDPSLLOC-NEW             PIC 9(2)    VALUE ZERO.                  
015000 77  WS-KDPSLLOC-OLD             PIC 9(2)    VALUE ZERO.                  
015100 77  W-IDSEKVNR                  PIC 9(5)    VALUE ZERO.                  
015200 77  WS-IDANSK                   PIC S9(3)   VALUE ZERO  COMP-3.          
015300 77  WS-IDDC-ALARM               PIC X(2)    VALUE SPACES.                
015401 77  DADATTID                    PIC 9(14).                               
015501 77  TODAYS-DATE                 PIC S9(8)   VALUE ZERO.                  
015600 EJECT                                                                    
015700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015800 01  FILLER REDEFINES DAGENS-DATUM.                                       
015900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016200 01  W-TID.                                                               
016300     03  WLOGG-TID               PIC S9(9)   VALUE ZERO.                  
016400     03  WLOGG-TID2              PIC 9(6)   VALUE ZERO.                   
016500     03  WLOGG-DATUM             PIC S9(8)   VALUE ZERO.                  
016600*                                                                         
016700 01  DAGENS-TIAAAAMMDD           PIC 9(8).                                
016800 01  WS-HHMMSSTH                 PIC 9(8)    VALUE ZERO.                  
016900*                                                                         
017000 01  W-RAKNARE.                                                           
017100     03  W-ANT-BEHANDLADE        PIC S9(5)   COMP VALUE ZERO.             
017200     03  W-MAX-BEHANDLADE        PIC S9(5)   COMP VALUE +500.             
017300     03  W-ANT-UPPDAT            PIC S9(5)   COMP VALUE ZERO.             
017400     03  W-MAX-UPPDAT            PIC S9(5)   COMP VALUE +500.             
017500     03  WS-ANTAL-IN             PIC S9(5)   COMP VALUE ZERO.             
017600     EJECT                                                                
017700*      --- VALID IDDC CODES                                               
017800*                                                                         
017900*01    -COPY WWDCKONS                                                     
018000*01    -COPY WWDC99                                                       
018100       EJECT                                                              
018200 01  DYNAMISKA-SUBPROGRAM.                                                
018300*                                                                         
018400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
018500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
018900     03  W015RAND                PIC X(8)    VALUE 'W015RAND'.            
019000     03  W400ARTU                PIC X(8)   VALUE 'W400ARTU'.             
019100     SKIP2                                                                
019200*    --- PARAMETRAR TILL ABEND                                            
019300                                                                          
019400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019600     SKIP2                                                                
019700 01  FELTEXT.                                                             
019800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020000     SKIP2                                                                
020100*    --- PARAMETRAR TILL W015RAND                                         
020200                                                                          
020300 77  ARTDBD                      PIC X(4)    VALUE 'WDK7'.                
020400     EJECT                                                                
020500*    --- PARAMETRAR TILL POSTSUM                                          
020600*                                                                         
020700*01  -COPY W0005   -PRE  POSTSUM-                                         
020800     EJECT                                                                
020900*01  -COPY WDECAREA                                                       
021000     EJECT                                                                
021100 01  FILLER                      PIC X(16)   VALUE 'W400ARTU'.            
021200*01     -COPY W400ARTU                                                    
021300     SKIP3                                                                
021400 01  WSORT-AREA-START            PIC X(24)   VALUE                        
021500                                 'WSORT-AREA-START'.                      
021600     SKIP2                                                                
021700 01  WSORT-AREA.                                                          
021800     03  WSORT-RANDOMKEY         PIC X(4).                                
021900*    03  -COPY W011042  -PRE WSORT-                                       
022000     EJECT                                                                
022100 01  IN-AREA-START               PIC X(24)   VALUE                        
022200                                 'IN-AREA-START  '.                       
022300     SKIP2                                                                
022400 01  IN-AREA.                                                             
022500*    03  -COPY W011042  -PRE IN-                                          
022600     EJECT                                                                
022700 01  FEL-AREA-START              PIC X(24)   VALUE                        
022800                                 'FEL-AREA-START  '.                      
022900     SKIP2                                                                
023000 77  MAX-KVHELTAL                PIC S9(4)  COMP.                         
023100 77  MAX-KVDECIMAL               PIC S9(4)  COMP.                         
023200 77  FEL-IDARTNR                 PIC 9(8).                                
023300     SKIP2                                                                
023400 01  FEL-AREA.                                                            
023500*    03  -COPY W092W001 -PRE  FEL-                                        
023600     03  FEL-IDELMT              PIC X(16).                               
023700     03  FEL-KDTECKEN            PIC X.                                   
023800     03  FEL-IDFVARDE            PIC X(25).                               
023900     EJECT                                                                
024000 01  UT-AREA-START               PIC X(24)   VALUE                        
024100                                 'UT-AREA-START  '.                       
024200     SKIP2                                                                
024300 01  UT-AREA.                                                             
024400*    03  -COPY W011042  -PRE UT-                                          
024500     EJECT                                                                
024600*    --- ARBETSFÄLT FÖR UTÖKADE KONTROLLER AV VISSA FÄLT                  
024700*                                                                         
024800 01  FILLER                      PIC X(16)   VALUE 'KNTL-AREOR'.          
024900     SKIP3                                                                
025000 01  VSOP-IDFVARDE.                                                       
025100     03  VSOP-CODE1             PIC 9.                                    
025200     03  VSOP-CODE2             PIC 9.                                    
025300     03  VSOP-CODE3             PIC 9.                                    
025400     03  VSOP-REST               PIC X(22).                               
025500     SKIP3                                                                
025600 01  ADART-IDFVARDE.                                                      
025700     03  ADART-ADLAGOMR          PIC 99.                                  
025800     03  ADART-ADGANG            PIC 99.                                  
025900     03  ADART-ADPLATS           PIC 9(5).                                
026000     03  ADART-REST              PIC X(14).                               
026100     SKIP3                                                                
026200 77  IX                          PIC S9(4)  COMP.                         
026300 77  IX2                         PIC S9(4)  COMP.                         
026400     SKIP3                                                                
026500 01  W-KDFORP-NUM                PIC 9(4).                                
026600 01  W-KDFORP-X REDEFINES W-KDFORP-NUM                                    
026700                                 PIC X(4).                                
026800                                                                          
026900     EJECT                                                                
027000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027100*                                                                         
027200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027300     SKIP3                                                                
027400 01  NYCKLAR-TILL-DLI.                                                    
027500     03  W-IDARTNR-SPAR          PIC S9(9)   VALUE ZERO COMP-3.           
027600                                                                          
027700     03  W-WDD7A1KY-MIN.                                                  
027800         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
027900         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
028000                                                                          
028100     03  W-WDD7A1KY-MAX.                                                  
028200         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3                       
028300                                  VALUE ZERO.                             
028400         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
028500                                                                          
028600     03  W-IDARTNR-PCB2-X.                                                
028700         05  W-IDARTNR-PCB2      PIC S9(9)   VALUE ZERO COMP-3.           
028800                                                                          
028900     03  W-WDGXKEY-2203-X.                                                
029000         05  FILLER              PIC X(4)    VALUE '2203'.                
029100         05  W-IDDC-2203         PIC X(2)    VALUE '  '.                  
029200         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
029300                                                                          
029400     03  W-WDGX2223-X.                                                    
029500         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
029600         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
029700         05  W-VALFRI-2225       PIC X(24)    VALUE LOW-VALUE.            
029800                                                                          
029900     03  W-WDGX2224-X.                                                    
030000         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
030100         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
030200         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
030300                                                                          
030400     03  W-WDGX2231-X.                                                    
030500         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
030600         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
030700                                                                          
030800     03  W-WDGX2232-X.                                                    
030900         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
031000         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
031100                                                                          
031200     03  W-IDARTNR-X.                                                     
031300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031400     03  W-KDSEGKEY-X.                                                    
031500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
031600     03  W-KDEMBAL-X.                                                     
031700         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
031800     03  W-IDDC-X.                                                        
031900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
032000     03  W-IDLAND-X.                                                      
032100         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
032200     SKIP2                                                                
032300*    --- STATUS-KOD FRÅN IMS                                              
032400 01  STATUS-WS                   PIC XX.                                  
032500     88  SEGMENT-FINNS                       VALUE '  '.                  
032600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
032900     SKIP2                                                                
033000 01  GODK-STATUSKODER.                                                    
033100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033200     SKIP3                                                                
033300 01  SSA1                        PIC X(128).                              
033400 01  SSA2                        PIC X(128).                              
033500     EJECT                                                                
033600*    --- IMS FUNKTIONSKODER                                               
033700*01  -COPY W0003                                                          
033800     EJECT                                                                
033900*    ---  DLI INPUT-OUTPUT AREA                                           
034000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
034100     SKIP3                                                                
034200 01  DLI-IO-AREA.                                                         
034300     SKIP3                                                                
034400     03  DLI-IO-WDK601.                                                   
034500*        05  -COPY WDK601  -PRE ARTC-                                     
034600     EJECT                                                                
034700     03  DLI-IO-WDK611.                                                   
034800*        05  -COPY WDK611  -PRE ARTC-                                     
034900     EJECT                                                                
035000 01  DLI-IO-WDK613.                                                       
035100*        03  -COPY WDK613  -PRE ARTC-                                     
035200     EJECT                                                                
035300 01  DLI-IO-WDT301.                                                       
035400*        03  -COPY WDT301                                                 
035500     EJECT                                                                
035600 01  DLI-IO-WDT311.                                                       
035700*        03  -COPY WDT311                                                 
035800     EJECT                                                                
035900 01  DLI-IO-WDJ901.                                                       
036000*        03  -COPY WDJ901                                                 
036100     EJECT                                                                
036200 01  DLI-IO-WDJ911.                                                       
036300*        03  -COPY WDJ911                                                 
036400     EJECT                                                                
036500 01  DLI-IO-WDD7A1.                                                       
036600*        03  -COPY WDD7A1                                                 
036700     EJECT                                                                
036800 01  FILLER                      PIC X(16)   VALUE 'WLLOGA01'.            
036900*01  WLLOGA01 -COPY WDL901                                                
037000     EJECT                                                                
037100 01  FILLER                      PIC X(16)   VALUE 'K601-2-AREA'.         
037200 01  DLI-IO-AREA-K601-2.                                                  
037300*     DENNA ANVÄNDS ENDAST FÖR KONTROLLÄSNING UTANFÖR UPPDAT-PCB          
037400     SKIP3                                                                
037500*    03  -COPY WDK601 -PRE PCB2-                                          
037600     EJECT                                                                
037700 01  FILLER                      PIC X(16)   VALUE 'K611-2-AREA'.         
037800 01  DLI-IO-AREA-K611-2.                                                  
037900*     DENNA ANVÄNDS ENDAST FÖR KONTROLLÄSNING UTANFÖR UPPDAT-PCB          
038000     SKIP3                                                                
038100*    03  -COPY WDK611 -PRE PCB2-                                          
038200     EJECT                                                                
038300     EJECT                                                                
038400 01  FILLER                      PIC X(16)   VALUE 'WDG3-AREA'.           
038500 01  DLI-IO-AREA-2204.                                                    
038600*    03  -COPY WDGX2204   -PRE XXBJ11-                                    
038700     03  FILLER                  PIC X(3)  VALUE SPACE.                   
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16)   VALUE 'WDK701-AREA'.         
039000 01  DLI-IO-AREA-WDK701.                                                  
039100*    03  -COPY WDK701                                                     
039200     EJECT                                                                
039300 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
039400 01  DLI-IO-AREA-WDK711.                                                  
039500*    03  -COPY WDK711                                                     
039600     EJECT                                                                
039700 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
039800 01  DLI-IO-AREA-WDK722.                                                  
039900*    03  -COPY WDK722                                                     
040000     EJECT                                                                
040100 01  FILLER                      PIC X(16)   VALUE 'WDB6-AREA'.           
040200 01  DLI-IO-AREA-WDB601.                                                  
040300*    03  -COPY WDB601                                                     
040400     EJECT                                                                
040500 01  FILLER                      PIC X(16)   VALUE 'WDR8-AREA'.           
040600 01  DLI-IO-AREA-WDR801.                                                  
040700*    03  -COPY WDR801                                                     
040800     EJECT                                                                
040900 01  FILLER                      PIC X(16)   VALUE 'A17-TRANS'.           
041000**   ---- A17-TRANS                                                       
041100 01  -COPY W510A17    -PRE A17-                                           
041200     EJECT                                                                
041300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR220'.                      
041400 01  DLI-IO-AREA-2232.                                                    
041500*    03  -COPY WDGX2232   -PRE WDR220-                                    
041600     EJECT                                                                
041700 01  FILLER                      PIC X(16)   VALUE 'WDR501-AREA'.         
041800 01  DLI-IO-AREA-2223.                                                    
041900*    03  -COPY WDGX2223   -PRE WDR501-                                    
042000     EJECT                                                                
042100 01  FILLER                      PIC X(16)   VALUE 'WDR550-AREA'.         
042200 01  DLI-IO-AREA-2224.                                                    
042300*    03  -COPY WDGX2224   -PRE WDR550-                                    
042401 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDT501'.        
042501 01  DLI-IO-WDT501.                                                       
042601*    03  -COPY WDT501     -PRE WDT501-                                    
042701     EJECT                                                                
042801 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDT511'.         
042901 01  DLI-IO-WDT511.                                                       
043001*    03  -COPY WDT511                                                     
043101     EJECT                                                                
043200*                                                                         
043300*                                                                         
043400 LINKAGE SECTION.                                                         
043500                                                                          
043600     EJECT                                                                
043700     SKIP3                                                                
043800*01  -COPY W0009   -PRE MSG-                                              
043900     SKIP3                                                                
044000*01  -COPY W0008  -PRE WDK6-                                              
044100     05  FILLER                  PIC X.                                   
044200*01  -COPY W0008  -PRE WDJ9-                                              
044300     05  FILLER                  PIC X.                                   
044400*01  -COPY W0008  -PRE LOGA-                                              
044500     05  FILLER                  PIC X.                                   
044600*01  -COPY W0008  -PRE WDK6-2-                                            
044700     05  FILLER                  PIC X.                                   
044800*01  -COPY W0008  -PRE WDD7A-                                             
044900     05  FILLER                  PIC X.                                   
045000*01  -COPY W0008  -PRE XXBJ-                                              
045100     05  FILLER                  PIC X.                                   
045200*01  -COPY W0008  -PRE WDK7-                                              
045300     05  FILLER                  PIC X.                                   
045400*01  -COPY W0008  -PRE WDB6-                                              
045500     05  FILLER                  PIC X.                                   
045600*01  -COPY W0008  -PRE WDR8-                                              
045700     05  FILLER                  PIC X.                                   
045800*01  -COPY W0008  -PRE WDT3-                                              
045900     05  FILLER                  PIC X.                                   
046000*01  -COPY W0008  -PRE WDR2-                                              
046100     05  FILLER                  PIC X.                                   
046200*01  -COPY W0008  -PRE WDR5-                                              
046300     05  FILLER                  PIC X.                                   
046401*01  -COPY W0008  -PRE WDT5-                                              
046501      05 FILLER                  PIC X.                                   
046600     EJECT                                                                
046700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDJ9-PCB LOGA-PCB             
046800                        WDK6-2-PCB WDD7A-PCB XXBJ-PCB                     
046900                        WDR8-PCB WDB6-PCB WDK7-PCB WDT3-PCB               
047001                        WDR2-PCB WDR5-PCB WDT5-PCB.                       
047100 MAIN SECTION.                                                            
047200     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDJ9-PCB LOGA-PCB             
047300                        WDK6-2-PCB WDD7A-PCB XXBJ-PCB                     
047400                        WDR8-PCB WDB6-PCB WDK7-PCB WDT3-PCB               
047501                        WDR2-PCB WDR5-PCB WDT5-PCB.                       
047600                                                                          
047700     PERFORM A-INIT                                                       
047800                                                                          
047900     SORT SORTFIL ASCENDING SORT-RANDOMKEY SORT-IDARTNR                   
048000                  INPUT PROCEDURE  B-LAES-O-LAGG-IN-RANDOMKEY             
048100                  OUTPUT PROCEDURE C-BEHANDLA-ALLA-POSTER                 
048200                                                                          
048300     IF SORT-RETURN NOT = ZERO                                            
048400       MOVE ' FEL RETURKOD FRÅN SORT' TO FELTEXT-STR                      
048500       DISPLAY FELTEXT                                                    
048600       CALL ABEND  USING RKOD-ABEND-UTAN-DUMP                             
048700     END-IF                                                               
048800                                                                          
048900     PERFORM Z-FINIT                                                      
049000                                                                          
049100     MOVE ZERO TO RETURN-CODE                                             
049200     GOBACK                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 A-INIT SECTION.                                                          
049600                                                                          
049700     OPEN INPUT  W09279-IN                                                
049800                                                                          
049900     OPEN OUTPUT W01112-FEL                                               
050000                 W09279-UT                                                
050100                                                                          
050200     ACCEPT DAGENS-DATUM  FROM DATE                                       
050300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
050400                                                                          
050500     MOVE +130000 TO SORT-FILE-SIZE                                       
050600                                                                          
050700     PERFORM S10-INITIERA-FELPOST                                         
050800     .                                                                    
050900     EJECT                                                                
051000 B-LAES-O-LAGG-IN-RANDOMKEY         SECTION.                              
051100                                                                          
051200     PERFORM S01-LAES-W09279-IN                                           
051300     PERFORM UNTIL END-OF-W09279                                          
051400                                                                          
051500       MOVE IN-W011042 TO WSORT-W011042                                   
051600       CALL W015RAND USING WSORT-IDARTNR WSORT-RANDOMKEY ARTDBD           
051700                                                                          
051800       RELEASE SORT-POST FROM WSORT-AREA                                  
051900                                                                          
052000       PERFORM S01-LAES-W09279-IN                                         
052100     END-PERFORM                                                          
052200     .                                                                    
052300     EJECT                                                                
052400 C-BEHANDLA-ALLA-POSTER     SECTION.                                      
052500                                                                          
052600     RETURN SORTFIL INTO WSORT-AREA                                       
052700       AT END  MOVE JA TO SORTFIL-EOF-SW                                  
052800     END-RETURN                                                           
052900                                                                          
053000     PERFORM UNTIL (   END-OF-SORTFIL                                     
053100                    OR W-ANT-BEHANDLADE > W-MAX-BEHANDLADE                
053200                    OR W-ANT-UPPDAT     > W-MAX-UPPDAT )                  
053300       PERFORM CA-BEHANDLA-EN-R05-POST                                    
053400                                                                          
053500       RETURN SORTFIL INTO WSORT-AREA                                     
053600         AT END  MOVE JA TO SORTFIL-EOF-SW                                
053700       END-RETURN                                                         
053800     END-PERFORM                                                          
053900                                                                          
054000     IF NOT END-OF-SORTFIL                                                
054100       PERFORM CB-SKRIV-OBEHANDLADE                                       
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500 CA-BEHANDLA-EN-R05-POST   SECTION.                                       
054600                                                                          
054700     MOVE WSORT-IDARTNR        TO FEL-IDARTNR,    W-IDARTNR               
054800     MOVE FEL-IDARTNR          TO FEL-SORTBGP-X                           
054900     MOVE WSORT-IDELMT         TO FEL-IDELMT                              
055000     MOVE WSORT-KDTECKEN-NYTT  TO FEL-KDTECKEN                            
055100     MOVE WSORT-IDFVARDE-NYTT  TO FEL-IDFVARDE                            
055200     MOVE SPACE                TO FEL-IDFELKODX                           
055300     MOVE NEJ                  TO SW-EMB-Q0-UPPD                          
055400                                  SW-EMB-Q1-UPPD                          
055500                                  SW-EMB-Q2-UPPD                          
055600                                  SW-BEFT-UPPD                            
055700                                  SW-KDFORP-UPPD                          
055800                                  SW-KDFORPPL-UPPD                        
055900                                  SW-KDFORPGP-UPPD                        
056000                                  SW-KDFORPUF-UPPD                        
056100                                                                          
056200     PERFORM IMS-GHU-WDK601-ART                                           
056300     IF SEGMENT-FINNS                                                     
056400       MOVE ARTC-ART-KDPRODSL TO WS-KDPRODSL-A17                          
056500       MOVE NEJ TO SW-WDK601-UPPD                                         
056600                                                                          
056700*      --- FÄLT I ROT-SEGMENTET                                           
056800                                                                          
056900       EVALUATE WSORT-IDELMT                                              
057000                                                                          
057100       WHEN 'IDFKNGRP'                                                    
057200         MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL               
057300         PERFORM X01-KONV-KONTR-NYTT                                      
057400         IF FEL-IDFELKODX = SPACE                                         
057500           MOVE DEC-IDEDITDATA TO ARTC-ART-IDFKNGRP                       
057600         END-IF                                                           
057700         MOVE JA               TO SW-WDK601-UPPD                          
057800                                                                          
057900       WHEN 'IDFTG'                                                       
058000         MOVE 2  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL               
058100         PERFORM X01-KONV-KONTR-NYTT                                      
058200         IF FEL-IDFELKODX = SPACE                                         
058300           MOVE DEC-IDEDITDATA TO ARTC-ART-IDFTG                          
058400         END-IF                                                           
058500         MOVE JA               TO SW-WDK601-UPPD                          
058600                                                                          
058700       WHEN 'TIFINLV'                                                     
058800         MOVE 5  TO MAX-KVHELTAL   MOVE 1  TO MAX-KVDECIMAL               
058900         PERFORM X01-KONV-KONTR-NYTT                                      
059000         IF FEL-IDFELKODX = SPACE                                         
059100           MOVE DEC-IDEDITDATA TO ARTC-ART-TIFINLV                        
059200                                                                          
059300           IF ARTC-ART-FLERS = JA                                         
059400*            --- DENNA ARTIKEL ERSÄTTER EN ELLER FLERA ANDRA              
059500*            --- SKAPA EV. LARM-POST FÖR LEV.PLANE-OMSPEC                 
059600             PERFORM CAD-SKAPA-EV-XXBJ-POST                               
059700           END-IF                                                         
059800         END-IF                                                           
059900         MOVE JA               TO SW-WDK601-UPPD                          
060000                                                                          
060100       WHEN 'TIURPROD'                                                    
060200          MOVE 4  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL              
060300          PERFORM X01-KONV-KONTR-NYTT                                     
060400          IF FEL-IDFELKODX = SPACE                                        
060500            MOVE DEC-IDEDITDATA TO ARTC-ART-TIURPROD                      
060600            MOVE JA             TO SW-WDK601-UPPD                         
060700            IF ARTC-ART-KDPRODSL = 15                                     
060800               MOVE JA          TO ALARM-EOP-UPPD-SW                      
060900            END-IF                                                        
061000          END-IF                                                          
061100                                                                          
061200                                                                          
061300       WHEN OTHER                                                         
061400                                                                          
061500       PERFORM IMS-GET-WDK611-CLAG                                        
061600       IF SEGMENT-FINNS                                                   
061700         MOVE ARTC-CLAG-KDPSLLOC TO WS-KDPSLLOC-OLD                       
061800                                                                          
061900*        --- FÄLT I CLAG-SEGMENTET                                        
062000                                                                          
062100         EVALUATE WSORT-IDELMT                                            
062200                                                                          
062300         WHEN 'ADART'                                                     
062400           PERFORM X11-KONTROLLERA-ADART                                  
062500           IF FEL-IDFELKODX = SPACE                                       
062600             MOVE ADART-ADLAGOMR  TO ARTC-CLAG-ADLAGOMR                   
062700             MOVE ADART-ADGANG    TO ARTC-CLAG-ADGANG                     
062800             MOVE ADART-ADPLATS   TO ARTC-CLAG-ADPLATS                    
062900*            PERFORM D-UPPDATERA-WDJ9                                     
063000* WDJ9-UPPDATERINGEN BLIR FELAKTIG, RÄTTAS INNAN KOMMENTARS-              
063100* MARKERINGAR TAS BORT.                                                   
063200           END-IF                                                         
063300                                                                          
063400         WHEN 'ADGANG'                                                    
063500           MOVE 3  TO MAX-KVHELTAL                                        
063600           MOVE 0 TO MAX-KVDECIMAL                                        
063700           PERFORM X01-KONV-KONTR-NYTT                                    
063800           IF FEL-IDFELKODX = SPACE                                       
063900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-ADGANG                      
064000*            PERFORM D-UPPDATERA-WDJ9                                     
064100           END-IF                                                         
064200                                                                          
064300         WHEN 'ADLAGOMR'                                                  
064400           MOVE 3  TO MAX-KVHELTAL                                        
064500           MOVE 0 TO MAX-KVDECIMAL                                        
064600           PERFORM X01-KONV-KONTR-NYTT                                    
064700           IF FEL-IDFELKODX = SPACE                                       
064800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-ADLAGOMR                    
064900*            PERFORM D-UPPDATERA-WDJ9                                     
065000           END-IF                                                         
065100                                                                          
065200         WHEN 'ADPLATS'                                                   
065300           MOVE 5  TO MAX-KVHELTAL                                        
065400           MOVE 0 TO MAX-KVDECIMAL                                        
065500           PERFORM X01-KONV-KONTR-NYTT                                    
065600           IF FEL-IDFELKODX = SPACE                                       
065700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-ADPLATS                     
065800*            PERFORM D-UPPDATERA-WDJ9                                     
065900           END-IF                                                         
066000                                                                          
066100         WHEN 'ADINPORT'                                                  
066200             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-ADINPORT               
066300                                                                          
066400         WHEN 'BEFT'                                                      
066500           MOVE 3  TO MAX-KVHELTAL                                        
066600           MOVE 0 TO MAX-KVDECIMAL                                        
066700           PERFORM X01-KONV-KONTR-NYTT                                    
066800           IF FEL-IDFELKODX = SPACE                                       
066900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-BEFT                        
067000             MOVE JA TO SW-BEFT-UPPD                                      
067100           END-IF                                                         
067200                                                                          
067300         WHEN 'FLAVRART'                                                  
067400           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
067500             MOVE '011' TO FEL-IDFELKODX                                  
067600           ELSE                                                           
067700             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLAVRART               
067800           END-IF                                                         
067900                                                                          
068000         WHEN 'FLJIT'                                                     
068100           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
068200             MOVE '011' TO FEL-IDFELKODX                                  
068300           ELSE                                                           
068400             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLJIT                  
068500           END-IF                                                         
068600                                                                          
068700         WHEN 'FLLARM-BUF'                                                
068800           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
068900             MOVE '011' TO FEL-IDFELKODX                                  
069000           ELSE                                                           
069100             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLLARM-BUF             
069200           END-IF                                                         
069300                                                                          
069400         WHEN 'FLLSRDEL'                                                  
069500           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
069600             MOVE '011' TO FEL-IDFELKODX                                  
069700           ELSE                                                           
069800             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLLSRDEL               
069900           END-IF                                                         
070000                                                                          
070100         WHEN 'FLLTKSP'                                                   
070200           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
070300             MOVE '011' TO FEL-IDFELKODX                                  
070400           ELSE                                                           
070500             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLLTKSP                
070600           END-IF                                                         
070700                                                                          
070800         WHEN 'FLMANBK'                                                   
070900           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
071000             MOVE '011' TO FEL-IDFELKODX                                  
071100           ELSE                                                           
071200             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLMANBK                
071300           END-IF                                                         
071400                                                                          
071500         WHEN 'FLMANKP'                                                   
071600           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
071700             MOVE '011' TO FEL-IDFELKODX                                  
071800           ELSE                                                           
071900             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLMANKP                
072000           END-IF                                                         
072100                                                                          
072200         WHEN 'FLMANQ'                                                    
072300           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
072400             MOVE '011' TO FEL-IDFELKODX                                  
072500           ELSE                                                           
072600             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLMANQ                 
072700           END-IF                                                         
072800                                                                          
072900         WHEN 'FLMPB'                                                     
073000           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
073100             MOVE '011' TO FEL-IDFELKODX                                  
073200           ELSE                                                           
073300             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLMPB                  
073400           END-IF                                                         
073500                                                                          
073600         WHEN 'FLNYBER'                                                   
073700           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
073800             MOVE '011' TO FEL-IDFELKODX                                  
073900           ELSE                                                           
074000             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLNYBER                
074100           END-IF                                                         
074200                                                                          
074300         WHEN 'FLOREGPB'                                                  
074400           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
074500             MOVE '011' TO FEL-IDFELKODX                                  
074600           ELSE                                                           
074700             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLOREGPB               
074800           END-IF                                                         
074900                                                                          
075000         WHEN 'FLREFILL'                                                  
075100           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
075200             MOVE '011' TO FEL-IDFELKODX                                  
075300           ELSE                                                           
075400             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLREFILL               
075500           END-IF                                                         
075600                                                                          
075700         WHEN 'FLSKROT-BEORD'                                             
075800           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
075900             MOVE '011' TO FEL-IDFELKODX                                  
076000           ELSE                                                           
076100             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLSKROT-BEORD          
076200           END-IF                                                         
076300                                                                          
076400         WHEN 'FLSPKOST'                                                  
076500           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
076600             MOVE '011' TO FEL-IDFELKODX                                  
076700           ELSE                                                           
076800             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLSPKOST               
076900           END-IF                                                         
077000                                                                          
077100         WHEN 'FLTOPP'                                                    
077200           IF WSORT-IDFVARDE-NYTT NOT = 'J' AND 'N'                       
077300             MOVE '011' TO FEL-IDFELKODX                                  
077400           ELSE                                                           
077500             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-FLTOPP                 
077600           END-IF                                                         
077700                                                                          
077800                                                                          
077900         WHEN 'IDARTNR-EMBQ0'                                             
078000           MOVE 9  TO MAX-KVHELTAL                                        
078100           MOVE 0  TO MAX-KVDECIMAL                                       
078200           PERFORM X01-KONV-KONTR-NYTT                                    
078300           IF FEL-IDFELKODX = SPACE                                       
078400             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDARTNR-EMBQ0               
078500             ACCEPT DAGENS-DATUM FROM DATE                                
078600             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
078700             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
078800             MOVE JA             TO SW-EMB-Q0-UPPD                        
078900           END-IF                                                         
079000                                                                          
079100         WHEN 'IDARTNR-EMBQ1'                                             
079200           MOVE 9  TO MAX-KVHELTAL                                        
079300           MOVE 0  TO MAX-KVDECIMAL                                       
079400           PERFORM X01-KONV-KONTR-NYTT                                    
079500           IF FEL-IDFELKODX = SPACE                                       
079600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDARTNR-EMBQ1               
079700             ACCEPT DAGENS-DATUM FROM DATE                                
079800             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
079900             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
080000             MOVE JA             TO SW-EMB-Q1-UPPD                        
080100           END-IF                                                         
080200                                                                          
080300         WHEN 'IDARTNR-EMBQ2'                                             
080400           MOVE 9  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
080500           PERFORM X01-KONV-KONTR-NYTT                                    
080600           IF FEL-IDFELKODX = SPACE                                       
080700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDARTNR-EMBQ2               
080800             ACCEPT DAGENS-DATUM FROM DATE                                
080900             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
081000             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
081100             MOVE JA             TO SW-EMB-Q2-UPPD                        
081200           END-IF                                                         
081300                                                                          
081400         WHEN 'IDARTNR-EMBQ3'                                             
081500           MOVE 9  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
081600           PERFORM X01-KONV-KONTR-NYTT                                    
081700           IF FEL-IDFELKODX = SPACE                                       
081800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDARTNR-EMBQ3               
081900           END-IF                                                         
082000                                                                          
082100         WHEN 'IDARTNR-EMBQ4'                                             
082200           MOVE 9  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
082300           PERFORM X01-KONV-KONTR-NYTT                                    
082400           IF FEL-IDFELKODX = SPACE                                       
082500             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDARTNR-EMBQ4               
082600           END-IF                                                         
082700                                                                          
082800         WHEN 'IDAVINR-SEN'                                               
082900             MOVE WSORT-IDFVARDE-NYTT (1:8)                               
083000             TO ARTC-CLAG-IDFS-SEN                                        
083100                                                                          
083200         WHEN 'IDBERED'                                                   
083300           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
083400           PERFORM X01-KONV-KONTR-NYTT                                    
083500           IF FEL-IDFELKODX = SPACE                                       
083600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDBERED                     
083700           END-IF                                                         
083800                                                                          
083900         WHEN 'IDPLANGR-LEV'                                              
084000           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
084100           PERFORM X01-KONV-KONTR-NYTT                                    
084200           IF FEL-IDFELKODX = SPACE                                       
084300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-IDPLANGR-LEV                
084400           END-IF                                                         
084500                                                                          
084600         WHEN 'IDPROENH'                                                  
084700           PERFORM X12-BEHANDLA-IDPROENH                                  
084800                                                                          
084900         WHEN 'IDPROJ'                                                    
085000           IF WSORT-IDFVARDE-NYTT (5:) = SPACE                            
085100             MOVE WSORT-IDFVARDE-NYTT (1:4) TO ARTC-CLAG-IDPROJ           
085200           ELSE                                                           
085300             MOVE '01E' TO FEL-IDFELKODX                                  
085400           END-IF                                                         
085500                                                                          
085600         WHEN 'IDPROJUP'                                                  
085700           IF WSORT-IDFVARDE-NYTT (9:) = SPACE                            
085800             MOVE WSORT-IDFVARDE-NYTT (1:9) TO ARTC-CLAG-IDPROJUP         
085900           ELSE                                                           
086000             MOVE '01E' TO FEL-IDFELKODX                                  
086100           END-IF                                                         
086200                                                                          
086300         WHEN 'IDRITN'                                                    
086400           IF WSORT-IDFVARDE-NYTT (11:) = SPACE                           
086500             MOVE WSORT-IDFVARDE-NYTT (1:10) TO ARTC-CLAG-IDRITN          
086600           ELSE                                                           
086700             MOVE '01E' TO FEL-IDFELKODX                                  
086800           END-IF                                                         
086900                                                                          
087000         WHEN 'KDAGE'                                                     
087100           MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-KDAGE                    
087200                                                                          
087300         WHEN 'KDARTHNT'                                                  
087400           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
087500           PERFORM X01-KONV-KONTR-NYTT                                    
087600           IF FEL-IDFELKODX = SPACE                                       
087700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDARTHNT                    
087800           END-IF                                                         
087900                                                                          
088000         WHEN 'KDARTURS'                                                  
088100           IF ARTC-CLAG-KDPCOO = SPACE                                    
088200             MOVE WSORT-IDFVARDE-NYTT TO ARTU-KDARTURS                    
088300             MOVE SPACE             TO ARTU-IDDC                          
088400             MOVE ZERO              TO ARTU-IDDISTR                       
088500             CALL W400ARTU          USING ARTU-W400ARTU                   
088600             IF ARTU-KDARTURS-NUM = ZERO                                  
088700               MOVE '011' TO FEL-IDFELKODX                                
088800             ELSE                                                         
088900               MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-KDARTURS             
089001                                           GLO-KDARTURS                   
089101               MOVE JA TO WRITE-WDT5-SW                                   
089200             END-IF                                                       
089300           ELSE                                                           
089400             MOVE '011' TO FEL-IDFELKODX                                  
089500           END-IF                                                         
089600                                                                          
089700         WHEN 'KDBPSR'                                                    
089800           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
089900                                                                          
090000               PERFORM X01-KONV-KONTR-NYTT                                
090100               IF FEL-IDFELKODX = SPACE                                   
090200                 MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDBPSR                  
090300               END-IF                                                     
090400                                                                          
090500         WHEN 'KDEFFMAN'                                                  
090600           IF WSORT-IDFVARDE-NYTT = SPACE OR                              
090700                                    'E' OR 'B' OR 'N' OR 'U'              
090800             MOVE WSORT-IDFVARDE-NYTT TO ARTC-CLAG-KDEFFMAN               
090900           ELSE                                                           
091000             MOVE '011' TO FEL-IDFELKODX                                  
091100           END-IF                                                         
091200                                                                          
091300                                                                          
091400         WHEN 'KDEMBKOD-0'                                                
091500           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
091600           PERFORM X01-KONV-KONTR-NYTT                                    
091700           IF FEL-IDFELKODX = SPACE                                       
091800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDEMBKOD-0                  
091900             ACCEPT DAGENS-DATUM FROM DATE                                
092000             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
092100             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
092200             MOVE JA             TO SW-EMB-Q0-UPPD                        
092300           END-IF                                                         
092400                                                                          
092500         WHEN 'KDEMBKOD-1'                                                
092600           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
092700           PERFORM X01-KONV-KONTR-NYTT                                    
092800           IF FEL-IDFELKODX = SPACE                                       
092900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDEMBKOD-1                  
093000             ACCEPT DAGENS-DATUM FROM DATE                                
093100             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
093200             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
093300             MOVE JA             TO SW-EMB-Q1-UPPD                        
093400           END-IF                                                         
093500                                                                          
093600         WHEN 'KDEMBKOD-2'                                                
093700           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
093800           PERFORM X01-KONV-KONTR-NYTT                                    
093900           IF FEL-IDFELKODX = SPACE                                       
094000             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDEMBKOD-2                  
094100             ACCEPT DAGENS-DATUM FROM DATE                                
094200             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
094300             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
094400             MOVE JA             TO SW-EMB-Q2-UPPD                        
094500           END-IF                                                         
094600                                                                          
094700         WHEN 'KDFARLIG'                                                  
094800           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
094900           PERFORM X01-KONV-KONTR-NYTT                                    
095000           IF FEL-IDFELKODX = SPACE                                       
095100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDFARLIG                    
095200           END-IF                                                         
095300                                                                          
095400         WHEN 'KDFORP'                                                    
095500           MOVE 4  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
095600           PERFORM X01-KONV-KONTR-NYTT                                    
095700           IF FEL-IDFELKODX = SPACE                                       
095800             MOVE DEC-IDEDITDATA  TO W-KDFORP-NUM                         
095900             MOVE W-KDFORP-X      TO ARTC-CLAG-KDFORP                     
096000             MOVE JA TO SW-KDFORP-UPPD                                    
096100           END-IF                                                         
096200                                                                          
096300         WHEN 'KDFORPPL'                                                  
096400           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
096500           PERFORM X01-KONV-KONTR-NYTT                                    
096600           IF FEL-IDFELKODX = SPACE                                       
096700             MOVE DEC-IDEDITDATA  TO ARTC-CLAG-KDFORPPL                   
096800             MOVE JA TO SW-KDFORPPL-UPPD                                  
096900           END-IF                                                         
097000                                                                          
097100         WHEN 'KDFORPGP'                                                  
097200           MOVE 2  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
097300           PERFORM X01-KONV-KONTR-NYTT                                    
097400           IF FEL-IDFELKODX = SPACE                                       
097500             MOVE DEC-IDEDITDATA  TO ARTC-CLAG-KDFORPGP                   
097600             MOVE JA TO SW-KDFORPGP-UPPD                                  
097700           END-IF                                                         
097800                                                                          
097900         WHEN 'KDFORPUF'                                                  
098000           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
098100           PERFORM X01-KONV-KONTR-NYTT                                    
098200           IF FEL-IDFELKODX = SPACE                                       
098300             MOVE DEC-IDEDITDATA  TO ARTC-CLAG-KDFORPUF                   
098400             MOVE JA TO SW-KDFORPUF-UPPD                                  
098500           END-IF                                                         
098600                                                                          
098700         WHEN 'KDKG'                                                      
098800           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
098900           PERFORM X01-KONV-KONTR-NYTT                                    
099000           IF FEL-IDFELKODX = SPACE                                       
099100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDKG                        
099200           END-IF                                                         
099300                                                                          
099400         WHEN 'KDKSP'                                                     
099500           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
099600           PERFORM X01-KONV-KONTR-NYTT                                    
099700           IF FEL-IDFELKODX = SPACE                                       
099800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDKSP                       
099900           END-IF                                                         
100000                                                                          
100100         WHEN 'KDLEVSP'                                                   
100200           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
100300           PERFORM X01-KONV-KONTR-NYTT                                    
100400           IF FEL-IDFELKODX = SPACE                                       
100500             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDLEVSP                     
100600           END-IF                                                         
100700                                                                          
100800         WHEN 'KDLPSP'                                                    
100900           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
101000           PERFORM X01-KONV-KONTR-NYTT                                    
101100           IF FEL-IDFELKODX = SPACE                                       
101200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDLPSP                      
101300           END-IF                                                         
101400                                                                          
101500         WHEN 'KDPSLLOC'                                                  
101600           MOVE 2  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
101700           PERFORM X01-KONV-KONTR-NYTT                                    
101800           IF FEL-IDFELKODX = SPACE                                       
101900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDPSLLOC                    
102000                                    WS-KDPSLLOC-NEW                       
102100             MOVE JA             TO SW-SKAPA-A17                          
102200           END-IF                                                         
102300                                                                          
102400         WHEN 'KDSRA'                                                     
102500           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
102600           PERFORM X01-KONV-KONTR-NYTT                                    
102700           IF FEL-IDFELKODX = SPACE                                       
102800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDSRA                       
102900           END-IF                                                         
103000                                                                          
103100         WHEN 'KDTIPPR'                                                   
103200           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
103300           PERFORM X01-KONV-KONTR-NYTT                                    
103400           IF FEL-IDFELKODX = SPACE                                       
103500             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDTIPPR                     
103600           END-IF                                                         
103700                                                                          
103800         WHEN 'KDTULLRE'                                                  
103900           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
104000           PERFORM X01-KONV-KONTR-NYTT                                    
104100           IF FEL-IDFELKODX = SPACE                                       
104200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDTULLRE                    
104300           END-IF                                                         
104400                                                                          
104500         WHEN 'KDUART'                                                    
104600           IF WSORT-IDFVARDE-NYTT (2:) = SPACE                            
104700             MOVE WSORT-IDFVARDE-NYTT (1:1) TO ARTC-CLAG-KDUART           
104800           ELSE                                                           
104900             MOVE '01E' TO FEL-IDFELKODX                                  
105000           END-IF                                                         
105100                                                                          
105200         WHEN 'KDVTH'                                                     
105300           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
105400           PERFORM X01-KONV-KONTR-NYTT                                    
105500           IF FEL-IDFELKODX = SPACE                                       
105600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDVTH                       
105700           END-IF                                                         
105800                                                                          
105900         WHEN 'KDVVKL'                                                    
106000           MOVE 1  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
106100           PERFORM X01-KONV-KONTR-NYTT                                    
106200           IF FEL-IDFELKODX = SPACE                                       
106300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDVVKL                      
106400           END-IF                                                         
106500                                                                          
106600         WHEN 'KDVSOP'                                                    
106700           PERFORM X10-KOPPL-KONTROLLER-VSOPKOD                           
106800           IF FEL-IDFELKODX = SPACE                                       
106900             MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL           
107000             PERFORM X01-KONV-KONTR-NYTT                                  
107100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KDVSOP                      
107200           END-IF                                                         
107300                                                                          
107400         WHEN 'KVAKS-CDC'                                                 
107500           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
107600           PERFORM X01-KONV-KONTR-NYTT                                    
107700           IF FEL-IDFELKODX = SPACE                                       
107800             ADD DEC-IDEDITDATA       TO ARTC-CLAG-KVAKS-CDC              
107900             MOVE DEC-IDEDITDATA      TO LOGG-KVART-SALDO                 
108000             COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                   
108100                                  ARTC-CLAG-KVAKS-T                       
108200             MOVE '+'                 TO LOGG-IDTECKEN-KVAKS              
108300             MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS             
108400                                         LOGG-IDTECKEN-KVAKS-PAV          
108500                                         LOGG-IDTECKEN-KVLS               
108600             MOVE ARTC-CLAG-KVEFRS    TO LOGG-KVEFRS                      
108700             MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                   
108800             MOVE ARTC-CLAG-KVLS      TO LOGG-KVLS                        
108900             PERFORM S30-SALDOLOGG                                        
109000           END-IF                                                         
109100                                                                          
109200         WHEN 'KVAKS-PAV'                                                 
109300           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
109400           PERFORM X01-KONV-KONTR-NYTT                                    
109500           IF FEL-IDFELKODX = SPACE                                       
109600             ADD DEC-IDEDITDATA       TO ARTC-CLAG-KVAKS-PAV              
109700             MOVE DEC-IDEDITDATA      TO LOGG-KVART-SALDO                 
109800             MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                   
109900             MOVE '+'                 TO LOGG-IDTECKEN-KVAKS-PAV          
110000             MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS             
110100                                         LOGG-IDTECKEN-KVAKS              
110200                                         LOGG-IDTECKEN-KVLS               
110300             MOVE ARTC-CLAG-KVEFRS    TO LOGG-KVEFRS                      
110400             COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                   
110500                                  ARTC-CLAG-KVAKS-T                       
110600             MOVE ARTC-CLAG-KVLS      TO LOGG-KVLS                        
110700             PERFORM S30-SALDOLOGG                                        
110800           END-IF                                                         
110900                                                                          
111000         WHEN 'KVAKS-T'                                                   
111100           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
111200           PERFORM X01-KONV-KONTR-NYTT                                    
111300           IF FEL-IDFELKODX = SPACE                                       
111400             ADD DEC-IDEDITDATA       TO ARTC-CLAG-KVAKS-T                
111500             MOVE DEC-IDEDITDATA      TO LOGG-KVART-SALDO                 
111600             COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                   
111700                                  ARTC-CLAG-KVAKS-T                       
111800             MOVE '+'                 TO LOGG-IDTECKEN-KVAKS              
111900             MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS             
112000                                         LOGG-IDTECKEN-KVAKS-PAV          
112100                                         LOGG-IDTECKEN-KVLS               
112200             MOVE ARTC-CLAG-KVEFRS    TO LOGG-KVEFRS                      
112300             MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                   
112400             MOVE ARTC-CLAG-KVLS      TO LOGG-KVLS                        
112500             PERFORM S30-SALDOLOGG                                        
112600           END-IF                                                         
112700                                                                          
112800         WHEN 'KVAP'                                                      
112900           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
113000           PERFORM X01-KONV-KONTR-NYTT                                    
113100           IF FEL-IDFELKODX = SPACE                                       
113200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVAP                        
113300           END-IF                                                         
113400                                                                          
113500         WHEN 'KVAVIS-SEN'                                                
113600           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
113700           PERFORM X01-KONV-KONTR-NYTT                                    
113800           IF FEL-IDFELKODX = SPACE                                       
113900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVAVIS-SEN                  
114000           END-IF                                                         
114100                                                                          
114200         WHEN 'KVDAGAR-FFH'                                               
114300           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
114400           PERFORM X01-KONV-KONTR-NYTT                                    
114500           IF FEL-IDFELKODX = SPACE                                       
114600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVDAGAR-FFH                 
114700           END-IF                                                         
114800                                                                          
114900         WHEN 'KVDAGAR-INLEV'                                             
115000           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
115100           PERFORM X01-KONV-KONTR-NYTT                                    
115200           IF FEL-IDFELKODX = SPACE                                       
115300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVDAGAR-INLEV               
115400           END-IF                                                         
115500                                                                          
115600         WHEN 'KVDAGAR-TT'                                                
115700           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
115800           PERFORM X01-KONV-KONTR-NYTT                                    
115900           IF FEL-IDFELKODX = SPACE                                       
116000             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVDAGAR-TT                  
116100           END-IF                                                         
116200                                                                          
116300         WHEN 'KVEFRS'                                                    
116400           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
116500           PERFORM X01-KONV-KONTR-NYTT                                    
116600           IF FEL-IDFELKODX = SPACE                                       
116700             ADD DEC-IDEDITDATA       TO ARTC-CLAG-KVEFRS                 
116800             MOVE DEC-IDEDITDATA      TO LOGG-KVART-SALDO                 
116900             MOVE ARTC-CLAG-KVEFRS    TO LOGG-KVEFRS                      
117000             MOVE '+'                 TO LOGG-IDTECKEN-KVEFRS             
117100             MOVE SPACE               TO LOGG-IDTECKEN-KVAKS              
117200                                         LOGG-IDTECKEN-KVAKS-PAV          
117300                                         LOGG-IDTECKEN-KVLS               
117400             COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                   
117500                                  ARTC-CLAG-KVAKS-T                       
117600             MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                   
117700             MOVE ARTC-CLAG-KVLS      TO LOGG-KVLS                        
117800             PERFORM S30-SALDOLOGG                                        
117900           END-IF                                                         
118000                                                                          
118100         WHEN 'KVINVS'                                                    
118200           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
118300           PERFORM X01-KONV-KONTR-NYTT                                    
118400           IF FEL-IDFELKODX = SPACE                                       
118500             ADD DEC-IDEDITDATA TO ARTC-CLAG-KVINVS                       
118600           END-IF                                                         
118700                                                                          
118800         WHEN 'KVLAAN'                                                    
118900           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
119000           PERFORM X01-KONV-KONTR-NYTT                                    
119100           IF FEL-IDFELKODX = SPACE                                       
119200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVLAAN                      
119300           END-IF                                                         
119400                                                                          
119500         WHEN 'KVLS'                                                      
119600           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
119700           PERFORM X01-KONV-KONTR-NYTT                                    
119800           IF FEL-IDFELKODX = SPACE                                       
119900             ADD  DEC-IDEDITDATA      TO ARTC-CLAG-KVLS                   
120000             MOVE DEC-IDEDITDATA      TO LOGG-KVART-SALDO                 
120100             MOVE ARTC-CLAG-KVLS      TO LOGG-KVLS                        
120200             MOVE '+'                 TO LOGG-IDTECKEN-KVLS               
120300             MOVE SPACE               TO LOGG-IDTECKEN-KVAKS              
120400                                         LOGG-IDTECKEN-KVAKS-PAV          
120500                                         LOGG-IDTECKEN-KVEFRS             
120600             COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                   
120700                                  ARTC-CLAG-KVAKS-T                       
120800             MOVE ARTC-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                   
120900             MOVE ARTC-CLAG-KVEFRS    TO LOGG-KVEFRS                      
121000             PERFORM S30-SALDOLOGG                                        
121100           END-IF                                                         
121200                                                                          
121300         WHEN 'KVMAD-SEP'                                                 
121400           MOVE 6  TO MAX-KVHELTAL   MOVE 1  TO MAX-KVDECIMAL             
121500           PERFORM X01-KONV-KONTR-NYTT                                    
121600           IF FEL-IDFELKODX = SPACE                                       
121700             IF DEC-KVDECIMAL = 0                                         
121800*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 1                 
121900               DIVIDE 10 INTO DEC-IDEDITDATA                              
122000             END-IF                                                       
122100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVMAD-SEP                   
122200           END-IF                                                         
122300                                                                          
122400         WHEN 'KVMAD-TOT'                                                 
122500           MOVE 6  TO MAX-KVHELTAL   MOVE 1  TO MAX-KVDECIMAL             
122600           PERFORM X01-KONV-KONTR-NYTT                                    
122700           IF FEL-IDFELKODX = SPACE                                       
122800             IF DEC-KVDECIMAL = 0                                         
122900*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 1                 
123000               DIVIDE 10 INTO DEC-IDEDITDATA                              
123100             END-IF                                                       
123200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVMAD-TOT                   
123300           END-IF                                                         
123400                                                                          
123500         WHEN 'KVMP'                                                      
123600           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
123700           PERFORM X01-KONV-KONTR-NYTT                                    
123800           IF FEL-IDFELKODX = SPACE                                       
123900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVMP                        
124000           END-IF                                                         
124100                                                                          
124200         WHEN 'KVOVERF'                                                   
124300           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
124400           PERFORM X01-KONV-KONTR-NYTT                                    
124500           IF FEL-IDFELKODX = SPACE                                       
124600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVOVERF                     
124700           END-IF                                                         
124800                                                                          
124900         WHEN 'KVPALL'                                                    
125000           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
125100           PERFORM X01-KONV-KONTR-NYTT                                    
125200           IF FEL-IDFELKODX = SPACE                                       
125300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVPALL                      
125400           END-IF                                                         
125500                                                                          
125600         WHEN 'KVQPACK-0'                                                 
125700           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
125800           PERFORM X01-KONV-KONTR-NYTT                                    
125900           IF FEL-IDFELKODX = SPACE                                       
126000             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVQPACK-0                   
126100             ACCEPT DAGENS-DATUM FROM DATE                                
126200             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
126300             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
126400             MOVE JA             TO SW-EMB-Q0-UPPD                        
126500           END-IF                                                         
126600                                                                          
126700         WHEN 'KVQPACK-1'                                                 
126800           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
126900           PERFORM X01-KONV-KONTR-NYTT                                    
127000           IF FEL-IDFELKODX = SPACE                                       
127100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVQPACK-1                   
127200             ACCEPT DAGENS-DATUM FROM DATE                                
127300             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
127400             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
127500             MOVE JA             TO SW-EMB-Q1-UPPD                        
127600           END-IF                                                         
127700                                                                          
127800         WHEN 'KVQPACK-2'                                                 
127900           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
128000           PERFORM X01-KONV-KONTR-NYTT                                    
128100           IF FEL-IDFELKODX = SPACE                                       
128200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVQPACK-2                   
128300             ACCEPT DAGENS-DATUM FROM DATE                                
128400             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
128500             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
128600             MOVE JA             TO SW-EMB-Q2-UPPD                        
128700           END-IF                                                         
128800                                                                          
128900         WHEN 'KVQPACK-3'                                                 
129000           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
129100           PERFORM X01-KONV-KONTR-NYTT                                    
129200           IF FEL-IDFELKODX = SPACE                                       
129300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVQPACK-3                   
129400             ACCEPT DAGENS-DATUM FROM DATE                                
129500             MOVE DAGENS-DATUM   TO ARTC-CLAG-TIUPPDAT-EMB                
129600             MOVE 'W0110200'     TO ARTC-CLAG-IDUSER-EMB                  
129700           END-IF                                                         
129800                                                                          
129900         WHEN 'KVQPACK-4'                                                 
130000           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
130100           PERFORM X01-KONV-KONTR-NYTT                                    
130200           IF FEL-IDFELKODX = SPACE                                       
130300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVQPACK-4                   
130400           END-IF                                                         
130500                                                                          
130600         WHEN 'KVRESS'                                                    
130700           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
130800           PERFORM X01-KONV-KONTR-NYTT                                    
130900           IF FEL-IDFELKODX = SPACE                                       
131000             ADD  DEC-IDEDITDATA TO ARTC-CLAG-KVRESS                      
131100           END-IF                                                         
131200                                                                          
131300         WHEN 'KVRETUR'                                                   
131400           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
131500           PERFORM X01-KONV-KONTR-NYTT                                    
131600           IF FEL-IDFELKODX = SPACE                                       
131700             ADD  DEC-IDEDITDATA TO ARTC-CLAG-KVRETUR                     
131800           END-IF                                                         
131900                                                                          
132000         WHEN 'KVROS'                                                     
132100           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
132200           PERFORM X01-KONV-KONTR-NYTT                                    
132300           IF FEL-IDFELKODX = SPACE                                       
132400             ADD  DEC-IDEDITDATA TO ARTC-CLAG-KVROS                       
132500           END-IF                                                         
132600                                                                          
132700         WHEN 'KVSLAGER'                                                  
132800           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
132900           PERFORM X01-KONV-KONTR-NYTT                                    
133000           IF FEL-IDFELKODX = SPACE                                       
133100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVSLAGER                    
133200           END-IF                                                         
133300                                                                          
133400         WHEN 'KVSLUTKP'                                                  
133500           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
133600           PERFORM X01-KONV-KONTR-NYTT                                    
133700           IF FEL-IDFELKODX = SPACE                                       
133800             ADD  DEC-IDEDITDATA TO ARTC-CLAG-KVSLUTKP                    
133900           END-IF                                                         
134000                                                                          
134100         WHEN 'KVSPANT'                                                   
134200           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
134300           PERFORM X01-KONV-KONTR-NYTT                                    
134400           IF FEL-IDFELKODX = SPACE                                       
134500             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVSPANT                     
134600           END-IF                                                         
134700                                                                          
134800         WHEN 'KVULOAD'                                                   
134900           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
135000           PERFORM X01-KONV-KONTR-NYTT                                    
135100           IF FEL-IDFELKODX = SPACE                                       
135200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVULOAD                     
135300           END-IF                                                         
135400                                                                          
135500         WHEN 'KVUTJFEL'                                                  
135600           MOVE 6  TO MAX-KVHELTAL   MOVE 1  TO MAX-KVDECIMAL             
135700           PERFORM X01-KONV-KONTR-NYTT                                    
135800           IF FEL-IDFELKODX = SPACE                                       
135900             IF DEC-KVDECIMAL = 0                                         
136000*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 1                 
136100               DIVIDE 10 INTO DEC-IDEDITDATA                              
136200             END-IF                                                       
136300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVUTJFEL                    
136400           END-IF                                                         
136500                                                                          
136600         WHEN 'KVUTRS'                                                    
136700           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
136800           PERFORM X01-KONV-KONTR-NYTT                                    
136900           IF FEL-IDFELKODX = SPACE                                       
137000             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVUTRS                      
137100           END-IF                                                         
137200                                                                          
137300         WHEN 'KVVECKOR-BT'                                               
137400           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
137500           PERFORM X01-KONV-KONTR-NYTT                                    
137600           IF FEL-IDFELKODX = SPACE                                       
137700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVVECKOR-BT                 
137800           END-IF                                                         
137900                                                                          
138000         WHEN 'KVVECKOR-FT'                                               
138100           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
138200           PERFORM X01-KONV-KONTR-NYTT                                    
138300           IF FEL-IDFELKODX = SPACE                                       
138400             MOVE DEC-IDEDITDATA TO ARTC-CLAG-KVVECKOR-FT                 
138500           END-IF                                                         
138600                                                                          
138700         WHEN 'PRARTBTO-EXP'                                              
138800           MOVE 7  TO MAX-KVHELTAL   MOVE 2  TO MAX-KVDECIMAL             
138900           PERFORM X01-KONV-KONTR-NYTT                                    
139000           IF FEL-IDFELKODX = SPACE                                       
139100             IF DEC-KVDECIMAL = 0                                         
139200*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 2                 
139300               DIVIDE 100 INTO DEC-IDEDITDATA                             
139400             END-IF                                                       
139500             MOVE DEC-IDEDITDATA TO ARTC-CLAG-PRARTBTO-EXP                
139600           END-IF                                                         
139700                                                                          
139800         WHEN 'PRARTSJK'                                                  
139900           MOVE 7  TO MAX-KVHELTAL   MOVE 2  TO MAX-KVDECIMAL             
140000           PERFORM X01-KONV-KONTR-NYTT                                    
140100           IF FEL-IDFELKODX = SPACE                                       
140200             IF DEC-KVDECIMAL = 0                                         
140300*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 2                 
140400               DIVIDE 100 INTO DEC-IDEDITDATA                             
140500             END-IF                                                       
140600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-PRARTSJK                    
140700           END-IF                                                         
140800                                                                          
140900         WHEN 'PRARTSTD'                                                  
141000           MOVE 7  TO MAX-KVHELTAL   MOVE 2  TO MAX-KVDECIMAL             
141100           PERFORM X01-KONV-KONTR-NYTT                                    
141200           IF FEL-IDFELKODX = SPACE                                       
141300             IF DEC-KVDECIMAL = 0                                         
141400*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 2                 
141500               DIVIDE 100 INTO DEC-IDEDITDATA                             
141600             END-IF                                                       
141700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-PRARTSTD                    
141800           END-IF                                                         
141900                                                                          
142000         WHEN 'PRDIRLON'                                                  
142100           MOVE 7  TO MAX-KVHELTAL   MOVE 3  TO MAX-KVDECIMAL             
142200           PERFORM X01-KONV-KONTR-NYTT                                    
142300           IF FEL-IDFELKODX = SPACE                                       
142400             IF DEC-KVDECIMAL = 0                                         
142500*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 3                 
142600               DIVIDE 1000 INTO DEC-IDEDITDATA                            
142700             END-IF                                                       
142800             IF DEC-IDEDITDATA > 9999.000                                 
142900                 MOVE 'F07' TO FEL-IDFELKODX                              
143000             ELSE                                                         
143100                 MOVE DEC-IDEDITDATA TO ARTC-CLAG-PRDIRLON                
143200             END-IF                                                       
143300           END-IF                                                         
143400                                                                          
143500         WHEN 'PRDMTRL'                                                   
143600           MOVE 6  TO MAX-KVHELTAL   MOVE 3  TO MAX-KVDECIMAL             
143700           PERFORM X01-KONV-KONTR-NYTT                                    
143800           IF FEL-IDFELKODX = SPACE                                       
143900             IF DEC-KVDECIMAL = 0                                         
144000*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 3                 
144100               DIVIDE 1000 INTO DEC-IDEDITDATA                            
144200             END-IF                                                       
144300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-PRDMTRL                     
144400           END-IF                                                         
144500                                                                          
144600         WHEN 'PRINK'                                                     
144700           MOVE 7  TO MAX-KVHELTAL   MOVE 2  TO MAX-KVDECIMAL             
144800           PERFORM X01-KONV-KONTR-NYTT                                    
144900           IF FEL-IDFELKODX = SPACE                                       
145000             IF DEC-KVDECIMAL = 0                                         
145100*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 2                 
145200               DIVIDE 100 INTO DEC-IDEDITDATA                             
145300             END-IF                                                       
145400             MOVE DEC-IDEDITDATA TO ARTC-CLAG-PRINK                       
145500           END-IF                                                         
145600                                                                          
145700         WHEN 'PROVRPAL'                                                  
145800           MOVE 7  TO MAX-KVHELTAL   MOVE 3  TO MAX-KVDECIMAL             
145900           PERFORM X01-KONV-KONTR-NYTT                                    
146000           IF FEL-IDFELKODX = SPACE                                       
146100             IF DEC-KVDECIMAL = 0                                         
146200*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 3                 
146300               DIVIDE 1000 INTO DEC-IDEDITDATA                            
146400             END-IF                                                       
146500             IF DEC-IDEDITDATA > 9999.000                                 
146600                 MOVE 'F07' TO FEL-IDFELKODX                              
146700             ELSE                                                         
146800                 MOVE DEC-IDEDITDATA TO ARTC-CLAG-PROVRPAL                
146900             END-IF                                                       
147000           END-IF                                                         
147100                                                                          
147200         WHEN 'RESLJUST'                                                  
147300           MOVE 2  TO MAX-KVHELTAL   MOVE 1  TO MAX-KVDECIMAL             
147400           PERFORM X01-KONV-KONTR-NYTT                                    
147500           IF FEL-IDFELKODX = SPACE                                       
147600             IF DEC-KVDECIMAL = 0                                         
147700*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 1                 
147800               DIVIDE 10 INTO DEC-IDEDITDATA                              
147900             END-IF                                                       
148000             MOVE DEC-IDEDITDATA TO ARTC-CLAG-RESLJUST                    
148100           END-IF                                                         
148200                                                                          
148300         WHEN 'RVPROFEL'                                                  
148400           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
148500           PERFORM X01-KONV-KONTR-NYTT                                    
148600           IF FEL-IDFELKODX = SPACE                                       
148700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-RVPROFEL                    
148800           END-IF                                                         
148900                                                                          
149000         WHEN 'RVPROURS'                                                  
149100           MOVE 3  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
149200           PERFORM X01-KONV-KONTR-NYTT                                    
149300           IF FEL-IDFELKODX = SPACE                                       
149400             MOVE DEC-IDEDITDATA TO ARTC-CLAG-RVPROURS                    
149500           END-IF                                                         
149600                                                                          
149700         WHEN 'TIAVIDAT-SEN'                                              
149800           MOVE 6  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
149900           PERFORM X01-KONV-KONTR-NYTT                                    
150000           IF FEL-IDFELKODX = SPACE                                       
150100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIAVIDAT-SEN                
150200           END-IF                                                         
150300                                                                          
150400         WHEN 'TIBESRPT'                                                  
150500           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
150600           PERFORM X01-KONV-KONTR-NYTT                                    
150700           IF FEL-IDFELKODX = SPACE                                       
150800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIBESRPT                    
150900           END-IF                                                         
151000                                                                          
151100         WHEN 'TIBESRPT-PAAM'                                             
151200           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
151300           PERFORM X01-KONV-KONTR-NYTT                                    
151400           IF FEL-IDFELKODX = SPACE                                       
151500             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIBESRPT-PAAM               
151600           END-IF                                                         
151700                                                                          
151800         WHEN 'TIINVDAT'                                                  
151900           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
152000           PERFORM X01-KONV-KONTR-NYTT                                    
152100           IF FEL-IDFELKODX = SPACE                                       
152200             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIINVDAT                    
152300           END-IF                                                         
152400                                                                          
152500         WHEN 'TILPSP'                                                    
152600           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
152700           PERFORM X01-KONV-KONTR-NYTT                                    
152800           IF FEL-IDFELKODX = SPACE                                       
152900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TILPSP                      
153000           END-IF                                                         
153100                                                                          
153200         WHEN 'TILTK'                                                     
153300           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
153400           PERFORM X01-KONV-KONTR-NYTT                                    
153500           IF FEL-IDFELKODX = SPACE                                       
153600             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TILTK                       
153700           END-IF                                                         
153800                                                                          
153900         WHEN 'TIOMSPEC'                                                  
154000           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
154100           PERFORM X01-KONV-KONTR-NYTT                                    
154200           IF FEL-IDFELKODX = SPACE                                       
154300             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIOMSPEC                    
154400           END-IF                                                         
154500                                                                          
154600         WHEN 'TIPBDAT'                                                   
154700           MOVE 5  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
154800           PERFORM X01-KONV-KONTR-NYTT                                    
154900           IF FEL-IDFELKODX = SPACE                                       
155000             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIPBDAT                     
155100           END-IF                                                         
155200                                                                          
155300         WHEN 'TIQJUST'                                                   
155400           MOVE 4  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
155500           PERFORM X01-KONV-KONTR-NYTT                                    
155600           IF FEL-IDFELKODX = SPACE                                       
155700             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIQJUST                     
155800           END-IF                                                         
155900                                                                          
156000         WHEN 'TIRODAT'                                                   
156100           MOVE 6  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
156200           PERFORM X01-KONV-KONTR-NYTT                                    
156300           IF FEL-IDFELKODX = SPACE                                       
156400             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TIRODAT                     
156500           END-IF                                                         
156600                                                                          
156700         WHEN 'TISLJUST'                                                  
156800           MOVE 4  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
156900           PERFORM X01-KONV-KONTR-NYTT                                    
157000           IF FEL-IDFELKODX = SPACE                                       
157100             MOVE DEC-IDEDITDATA TO ARTC-CLAG-TISLJUST                    
157200           END-IF                                                         
157300                                                                          
157400         WHEN 'VKART'                                                     
157500           MOVE 7  TO MAX-KVHELTAL   MOVE 0  TO MAX-KVDECIMAL             
157600           PERFORM X01-KONV-KONTR-NYTT                                    
157700           IF FEL-IDFELKODX = SPACE                                       
157800             MOVE DEC-IDEDITDATA TO ARTC-CLAG-VKART                       
157900           END-IF                                                         
158000                                                                          
158100         WHEN 'VLARTNTO'                                                  
158200           MOVE 8  TO MAX-KVHELTAL   MOVE 1  TO MAX-KVDECIMAL             
158300           PERFORM X01-KONV-KONTR-NYTT                                    
158400           IF FEL-IDFELKODX = SPACE                                       
158500             IF DEC-KVDECIMAL = 0                                         
158600*              -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 1                 
158700               DIVIDE 10 INTO DEC-IDEDITDATA                              
158800             END-IF                                                       
158900             MOVE DEC-IDEDITDATA TO ARTC-CLAG-VLARTNTO                    
159000           END-IF                                                         
159100                                                                          
159200         END-EVALUATE                                                     
159300                                                                          
159400       ELSE                                                               
159500*        -- CLAG-SEGMENT SAKNAS                                           
159600         MOVE '024' TO FEL-IDFELKODX                                      
159700       END-IF                                                             
159800                                                                          
159900       END-EVALUATE                                                       
160000                                                                          
160100     ELSE                                                                 
160200*        -- ARTIKEL SAKNAS                                                
160300         MOVE '024' TO FEL-IDFELKODX                                      
160400     END-IF                                                               
160500                                                                          
160600     IF FEL-IDFELKODX NOT = SPACE                                         
160700       PERFORM S11-SKRIV-W01112-FEL                                       
160800     ELSE                                                                 
160900        IF SW-WDK601-UPPD = JA                                            
161000           PERFORM IMS-REPL-WDK601                                        
161100        ELSE                                                              
161200           PERFORM IMS-REPL-WDK611                                        
161300* CREATE ENTRY IN WDT5 DATABASE                                           
161301                                                                          
161302           IF GLO-KDARTURS > ' '                                          
161303             IF WRITE-WDT5                                                
161401              PERFORM IMS-GU-WDT501                                       
161402              IF SEGMENT-FINNS                                            
161403               PERFORM IMS-GHNP-WDT511                                    
161404               MOVE ARTC-ART-IDLEVNR TO GLO-IDLEVNR                       
161405               PERFORM IMS-REPL-WDT511                                    
161406              END-IF                                                      
161501              IF SEGMENT-SAKNAS                                           
161601                 MOVE W-IDARTNR TO WDT501-ARTU-IDARTNR                    
161701                 PERFORM IMS-ISRT-WDT501                                  
161801              END-IF                                                      
161901                                                                          
162001              MOVE 'R05'                 TO GLO-IDUSER                    
162002              MOVE SPACES       TO GLO-IDLEVNR                            
162101              MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                
162201              MOVE FUNCTION CURRENT-DATE(1:8)  TO TODAYS-DATE             
162301              COMPUTE GLO-DADATTID-9KOMPL =                               
162401                      99999999999999 - DADATTID                           
162501                                                                          
162601              PERFORM IMS-ISRT-WDT511                                     
162701             END-IF                                                       
162702           END-IF                                                         
162800        END-IF                                                            
162900        ADD +1      TO W-ANT-BEHANDLADE                                   
163000*                                                                         
163100* CREATE ALARM WHEN EOP IS UPDATED                                        
163200        IF ALARM-EOP-UPPD-JA                                              
163300           MOVE ALL ZERO       TO WS-IDANSK                               
163400           MOVE WC-CDC-SE      TO WS-IDDC-ALARM                           
163500           PERFORM CAG-CREATE-ALARM                                       
163600* REINTIALIZING FLAG FOR ALARM                                            
163700           MOVE NEJ            TO ALARM-EOP-UPPD-SW                       
163800        END-IF                                                            
163900*                                                                         
164000        IF SW-EMB-Q0-UPPD = JA                                            
164100           PERFORM CAA-UPPDAT-Q0-EMB                                      
164200        END-IF                                                            
164300        IF SW-EMB-Q1-UPPD = JA                                            
164400           PERFORM CAB-UPPDAT-Q1-EMB                                      
164500        END-IF                                                            
164600        IF SW-EMB-Q2-UPPD = JA                                            
164700           PERFORM CAC-UPPDAT-Q2-EMB                                      
164800        END-IF                                                            
164900        IF SW-SKAPA-A17   = JA                                            
165000           PERFORM CAF-SKAPA-A17-POST                                     
165100        END-IF                                                            
165200        IF SW-BEFT-UPPD = JA       OR                                     
165300           SW-KDFORP-UPPD = JA     OR                                     
165400           SW-KDFORPPL-UPPD = JA   OR                                     
165500           SW-KDFORPGP-UPPD = JA   OR                                     
165600           SW-KDFORPUF-UPPD = JA                                          
165700           PERFORM E-UPPDAT-WDT311                                        
165800        END-IF                                                            
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200 CAA-UPPDAT-Q0-EMB     SECTION.                                           
166300                                                                          
166400     MOVE 'Q0 '                        TO W-KDEMBAL                       
166500     PERFORM IMS-GET-WDK613-EMB                                           
166600     IF SEGMENT-FINNS                                                     
166700        MOVE ARTC-CLAG-IDARTNR-EMBQ0   TO ARTC-EMB-IDARTNR-EMB            
166800        MOVE ARTC-CLAG-KDEMBKOD-0      TO ARTC-EMB-KDEMBKOD               
166900        MOVE ARTC-CLAG-KVQPACK-0       TO ARTC-EMB-KVQPACK-EMB            
167000        PERFORM IMS-REPL-WDK613-EMB                                       
167100     ELSE                                                                 
167200        MOVE 'Q0 '                     TO ARTC-EMB-KDEMBKEY               
167300        MOVE ARTC-CLAG-IDARTNR-EMBQ0   TO ARTC-EMB-IDARTNR-EMB            
167400        MOVE ARTC-CLAG-KDEMBKOD-0      TO ARTC-EMB-KDEMBKOD               
167500        MOVE ARTC-CLAG-KVQPACK-0       TO ARTC-EMB-KVQPACK-EMB            
167600        PERFORM IMS-ISRT-WDK613-EMB                                       
167700     END-IF                                                               
167800     .                                                                    
167900     EJECT                                                                
168000 CAB-UPPDAT-Q1-EMB     SECTION.                                           
168100                                                                          
168200     MOVE 'Q1 '                        TO W-KDEMBAL                       
168300     PERFORM IMS-GET-WDK613-EMB                                           
168400     IF SEGMENT-FINNS                                                     
168500        MOVE ARTC-CLAG-IDARTNR-EMBQ1   TO ARTC-EMB-IDARTNR-EMB            
168600        MOVE ARTC-CLAG-KDEMBKOD-1      TO ARTC-EMB-KDEMBKOD               
168700        MOVE ARTC-CLAG-KVQPACK-1       TO ARTC-EMB-KVQPACK-EMB            
168800        PERFORM IMS-REPL-WDK613-EMB                                       
168900     ELSE                                                                 
169000        MOVE 'Q1 '                     TO ARTC-EMB-KDEMBKEY               
169100        MOVE ARTC-CLAG-IDARTNR-EMBQ1   TO ARTC-EMB-IDARTNR-EMB            
169200        MOVE ARTC-CLAG-KDEMBKOD-1      TO ARTC-EMB-KDEMBKOD               
169300        MOVE ARTC-CLAG-KVQPACK-1       TO ARTC-EMB-KVQPACK-EMB            
169400        PERFORM IMS-ISRT-WDK613-EMB                                       
169500     END-IF                                                               
169600     .                                                                    
169700     EJECT                                                                
169800 CAC-UPPDAT-Q2-EMB     SECTION.                                           
169900                                                                          
170000     MOVE 'Q2 '                        TO W-KDEMBAL                       
170100     PERFORM IMS-GET-WDK613-EMB                                           
170200     IF SEGMENT-FINNS                                                     
170300        MOVE ARTC-CLAG-IDARTNR-EMBQ2   TO ARTC-EMB-IDARTNR-EMB            
170400        MOVE ARTC-CLAG-KDEMBKOD-2      TO ARTC-EMB-KDEMBKOD               
170500        MOVE ARTC-CLAG-KVQPACK-2       TO ARTC-EMB-KVQPACK-EMB            
170600        PERFORM IMS-REPL-WDK613-EMB                                       
170700     ELSE                                                                 
170800        MOVE 'Q2 '                     TO ARTC-EMB-KDEMBKEY               
170900        MOVE ARTC-CLAG-IDARTNR-EMBQ2   TO ARTC-EMB-IDARTNR-EMB            
171000        MOVE ARTC-CLAG-KDEMBKOD-2      TO ARTC-EMB-KDEMBKOD               
171100        MOVE ARTC-CLAG-KVQPACK-2       TO ARTC-EMB-KVQPACK-EMB            
171200        PERFORM IMS-ISRT-WDK613-EMB                                       
171300     END-IF                                                               
171400     .                                                                    
171500     EJECT                                                                
171600 CAD-SKAPA-EV-XXBJ-POST   SECTION.                                        
171700     SKIP2                                                                
171800*    SKAPAR EN HÄNDELSE 2204 IFALL EVENTUELLT ERSATTA ARTIKLAR            
171900*    TILL DEN AKTUELLA HAR KDERS 01 < 09.                                 
172000*       --- KOLLAR OM LARM-09 SKALL LÄGGAS UPP                            
172100*       --- LÄSER WDD7A MED IDARTNR-TILLK                                 
172200     MOVE W-IDARTNR    TO W-IDARTNR-SPAR                                  
172300                          W-IDARTNR-MIN7                                  
172400                          W-IDARTNR-MAX7                                  
172500     MOVE NEJ TO SW-LARM-09                                               
172600                                                                          
172700     PERFORM IMS-GN-WDD7A1                                                
172800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
172900*       --- LÄS ERSATT ARTIKEL MED WDK6-PCB2                              
173000        MOVE ERS-IDARTNR     TO W-IDARTNR                                 
173100        PERFORM IMS-GU-WDK601-PCB2                                        
173200        IF SEGMENT-FINNS AND PCB2-ART-KDERS-UTG = ZERO                    
173300           PERFORM IMS-GNP-WDK611-PCB2                                    
173400           IF PCB2-CLAG-KDERS = +01 OR +02 OR +03                         
173500                             OR +04 OR +05 OR +06                         
173600                             OR +07 OR +08                                
173700              PERFORM CADA-SKAPA-LARM-ORSAK-09                            
173800              MOVE JA TO SW-LARM-09                                       
173900           END-IF                                                         
174000        END-IF                                                            
174100        PERFORM IMS-GN-WDD7A1                                             
174200     END-PERFORM                                                          
174300                                                                          
174400*    --- ÅTERSTÄLLER NYCKELN TILL IDARTNR-TILLK                           
174500     MOVE W-IDARTNR-SPAR     TO W-IDARTNR                                 
174600                                                                          
174700     IF SW-LARM-09 = JA                                                   
174800*       --- ERSATTA ARTIKLAR ÄR LARMADE.                                  
174900*       --- LARMA DÄRFÖR NU ÄVEN DENNA ERSÄTTANDE ARTIKELN                
175000        PERFORM CADA-SKAPA-LARM-ORSAK-09                                  
175100     END-IF                                                               
175200     .                                                                    
175300     EJECT                                                                
175400 CADA-SKAPA-LARM-ORSAK-09 SECTION.                                        
175500     SKIP2                                                                
175600     MOVE WC-CDC-SE  TO W-IDDC-2203                                       
175700     MOVE W-IDARTNR  TO XXBJ11-2204-IDARTNR                               
175800     MOVE +09        TO XXBJ11-2204-KDLPORS                               
175900     PERFORM IMS-ISRT-XXBJ-2204                                           
176000     .                                                                    
176100     EJECT                                                                
176200 CAF-SKAPA-A17-POST SECTION.                                              
176300     PERFORM IMS-GU-WDK701                                                
176400     IF SEGMENT-FINNS                                                     
176500        PERFORM IMS-GN-WDK711                                             
176600        IF SEGMENT-FINNS                                                  
176700          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                    
176800            MOVE SLAG-IDDC                  TO W-IDDC                     
176900            PERFORM IMS-GU-WDB601                                         
177000            IF SEGMENT-FINNS                                              
177100              IF DCS-NDC-NA                                               
177200                IF DCS-USA                                                
177300                  MOVE SLAG-PRAVCOST      TO A17-PRAVCOST                 
177400                  MOVE SLAG-KVLS          TO A17-KVLS                     
177500                  MOVE SLAG-KVEFRS        TO A17-KVEFRS                   
177600                  MOVE '53'               TO A17-IDFTG                    
177700                  PERFORM CAFA-SKAPA-A17                                  
177800                END-IF                                                    
177900                IF DCS-CANADA                                             
178000                  MOVE SLAG-PRAVCOST      TO A17-PRAVCOST                 
178100                  MOVE SLAG-KVLS          TO A17-KVLS                     
178200                  MOVE SLAG-KVEFRS        TO A17-KVEFRS                   
178300                  MOVE '54'               TO A17-IDFTG                    
178400                  PERFORM CAFA-SKAPA-A17                                  
178500                END-IF                                                    
178600              END-IF                                                      
178700            END-IF                                                        
178800            PERFORM IMS-GN-WDK711                                         
178900          END-PERFORM                                                     
179000        END-IF                                                            
179100     END-IF                                                               
179200     .                                                                    
179300     EJECT                                                                
179400 CAFA-SKAPA-A17 SECTION.                                                  
179500                                                                          
179600     MOVE 'A17'                  TO A17-IDPTYP                            
179700     MOVE 'M21'                  TO A17-KDEKOHT                           
179800     MOVE W-IDDC                 TO A17-IDDC-SEND                         
179900                                    A17-IDDC-REC                          
180000     MOVE WS-KDPRODSL-A17        TO A17-KDPRODSL                          
180100     MOVE W-IDARTNR              TO A17-IDARTNR                           
180200     MOVE WS-KDPSLLOC-OLD        TO A17-KDPSLLOC-OLD                      
180300     MOVE WS-KDPSLLOC-NEW        TO A17-KDPSLLOC-NEW                      
180400                                                                          
180500     MOVE 'W0110200'             TO FIL-IDPGM                             
180600     MOVE FUNCTION CURRENT-DATE (1:8) TO                                  
180700     DAGENS-TIAAAAMMDD                                                    
180800     ACCEPT WS-HHMMSSTH FROM TIME                                         
180900     MOVE WS-HHMMSSTH            TO FIL-TIKLOCK                           
181000                                                                          
181100     MOVE DAGENS-TIAAAAMMDD      TO FIL-TIREGDAT                          
181200     MOVE DAGENS-TIAAAAMMDD      TO A17-DAJUSTDA                          
181300     ADD +1                      TO W-IDSEKVNR                            
181400     MOVE W-IDSEKVNR             TO FIL-IDSEKVNR                          
181500     MOVE 'W510A17 '             TO FIL-IDCPYTXT                          
181600     MOVE A17-W510A17            TO FIL-WDR801-DATA                       
181700*                                                                         
181800*  NEDAN NY KOD KOMMENTERAD VID ÄT 06:2   EJ KLARA USA-TESTER/ KJH        
181900*                                                                         
182000     PERFORM IMS-ISRT-WDR801                                              
182100                                                                          
182200     IF SEGMENT-FINNS-REDAN                                               
182300       PERFORM UNTIL SEGMENT-FINNS                                        
182400         ADD +1                      TO W-IDSEKVNR                        
182500         MOVE W-IDSEKVNR             TO FIL-IDSEKVNR                      
182600         PERFORM IMS-ISRT-WDR801                                          
182700       END-PERFORM                                                        
182800     END-IF                                                               
182900     .                                                                    
183000     EJECT                                                                
183100 CAG-CREATE-ALARM SECTION.                                                
183200                                                                          
183300     PERFORM IMS-GU-WDK611-CLAG                                           
183400     MOVE ARTC-CLAG-IDANSK    TO WS-IDANSK                                
183500     MOVE WS-IDANSK           TO W-IDANSK-2232                            
183600     PERFORM IMS-GU-R220                                                  
183700     IF SEGMENT-FINNS                                                     
183800       MOVE WDR220-2232-IDANSK-LARM                                       
183900                              TO W-IDANSK-2223                            
184000     ELSE                                                                 
184100       MOVE ZERO              TO W-IDANSK-2223                            
184200     END-IF                                                               
184300     MOVE '2223'              TO WDR501-2223-IDHTYP                       
184400     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
184500     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
184600     PERFORM IMS-ISRT-R501                                                
184700     PERFORM IMS-GHU-R501                                                 
184800     MOVE FUNCTION CURRENT-DATE(3:6)                                      
184900                              TO WDR550-2224-TISENBEK-DAG                 
185000     MOVE FUNCTION CURRENT-DATE(11:6)                                     
185100                              TO WDR550-2224-TISENBEK-KL                  
185200     MOVE 601                 TO WDR550-2224-KDLARM                       
185300     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
185400     MOVE WS-IDDC-ALARM       TO WDR550-2224-IDDC                         
185500     MOVE JA                  TO WDR550-2224-FLNYLARM                     
185600     MOVE ZERO                TO WDR550-2224-IDDISTR                      
185700                                 WDR550-2224-IDKUNDNR                     
185800     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
185900     MOVE 1                   TO WDR550-2224-IDLOPNR                      
186000     MOVE DAGENS-DATUM        TO WDR550-2224-TIREGDAT                     
186100     MOVE SPACE               TO WDR550-2224-IDTRANS                      
186200                                 WDR550-2224-KDMFSFOR                     
186300     MOVE ZERO                TO WDR550-2224-IDKR                         
186400     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
186500                                                                          
186600     PERFORM IMS-ISRT-R550                                                
186700*                                                                         
186800*    IF PART LOCALLY PROCURED IN CN OR US, CREATE ALARM                   
186900     PERFORM CAGA-CHK-PART-SOURCING                                       
187000     .                                                                    
187100     EJECT                                                                
187200 CAGA-CHK-PART-SOURCING SECTION.                                          
187300                                                                          
187400     PERFORM IMS-GU-WDK701                                                
187500     IF SEGMENT-FINNS                                                     
187600        PERFORM IMS-GN-WDK711                                             
187700        IF SEGMENT-FINNS                                                  
187800          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                    
187900*           CHECK IF PART IS PROCURED IN US OR CHINA                      
188000            IF SLAG-IDDC-REF = SPACES                                     
188100               MOVE SLAG-IDDC                TO W-IDDC                    
188200               PERFORM IMS-GU-WDB601                                      
188300               IF SEGMENT-FINNS                                           
188400                  IF DCS-NDC-NA OR DCS-NDC-CN                             
188500                     MOVE ZERO                TO WS-IDANSK                
188600                     MOVE SLAG-IDDC           TO WS-IDDC-ALARM            
188700                     PERFORM IMS-GNP-WDK722                               
188800                     IF SEGMENT-FINNS                                     
188900                        MOVE XLAG-IDANSK      TO WS-IDANSK                
189000                     END-IF                                               
189100                     PERFORM CAGAA-CREATE-EOP-ALARM-LOCAL                 
189200                  END-IF                                                  
189300               END-IF                                                     
189400            END-IF                                                        
189500            PERFORM IMS-GN-WDK711                                         
189600          END-PERFORM                                                     
189700        END-IF                                                            
189800     END-IF                                                               
189900     .                                                                    
190000     EJECT                                                                
190100 CAGAA-CREATE-EOP-ALARM-LOCAL SECTION.                                    
190200                                                                          
190300     MOVE WS-IDANSK           TO W-IDANSK-2232                            
190400     PERFORM IMS-GU-R220                                                  
190500     IF SEGMENT-FINNS                                                     
190600       MOVE WDR220-2232-IDANSK-LARM                                       
190700                              TO W-IDANSK-2223                            
190800     ELSE                                                                 
190900       MOVE ZERO              TO W-IDANSK-2223                            
191000     END-IF                                                               
191100     MOVE '2223'              TO WDR501-2223-IDHTYP                       
191200     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
191300     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
191400     PERFORM IMS-ISRT-R501                                                
191500     PERFORM IMS-GHU-R501                                                 
191600     MOVE FUNCTION CURRENT-DATE(3:6)                                      
191700                              TO WDR550-2224-TISENBEK-DAG                 
191800     MOVE FUNCTION CURRENT-DATE(11:6)                                     
191900                              TO WDR550-2224-TISENBEK-KL                  
192000     MOVE 601                 TO WDR550-2224-KDLARM                       
192100     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
192200     MOVE WS-IDDC-ALARM       TO WDR550-2224-IDDC                         
192300     MOVE JA                  TO WDR550-2224-FLNYLARM                     
192400     MOVE ZERO                TO WDR550-2224-IDDISTR                      
192500                                 WDR550-2224-IDKUNDNR                     
192600     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
192700     MOVE 1                   TO WDR550-2224-IDLOPNR                      
192800     MOVE DAGENS-DATUM        TO WDR550-2224-TIREGDAT                     
192900     MOVE SPACE               TO WDR550-2224-IDTRANS                      
193000                                 WDR550-2224-KDMFSFOR                     
193100     MOVE ZERO                TO WDR550-2224-IDKR                         
193200     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
193300                                                                          
193400     PERFORM IMS-ISRT-R550                                                
193500     .                                                                    
193600     EJECT                                                                
193700 CB-SKRIV-OBEHANDLADE SECTION.                                            
193800                                                                          
193900     PERFORM UNTIL END-OF-SORTFIL                                         
194000                                                                          
194100       PERFORM S20-SKRIV-W09279                                           
194200                                                                          
194300       RETURN SORTFIL INTO WSORT-AREA                                     
194400         AT END  MOVE JA TO SORTFIL-EOF-SW                                
194500       END-RETURN                                                         
194600     END-PERFORM                                                          
194700     .                                                                    
194800     EJECT                                                                
194900*                                                                         
195000*D-UPPDATERA-WDJ9 SECTION.                                                
195100*    MOVE W-IDARTNR               TO ART-IDARTNR                          
195200*    MOVE 11                      TO HIST-IDDC                            
195300*    MOVE FUNCTION CURRENT-DATE(1:8) TO WLOGG-DATUM                       
195400*    COMPUTE HIST-DASTADAT-9KOMPL = 99999999 - WLOGG-DATUM                
195500*    ACCEPT WLOGG-TID2 FROM TIME                                          
195600*    MOVE WLOGG-TID2 (1:6) TO ARTC-CLAG-TIUPPDAT-EMB                      
195700*    COMPUTE HIST-TISTATID-9KOMPL = 999999 - WLOGG-TID2                   
195800*    MOVE ARTC-CLAG-ADLAGOMR      TO HIST-ADLAGOMR                        
195900*    MOVE ARTC-CLAG-ADGANG        TO HIST-ADGANG                          
196000*    MOVE ARTC-CLAG-ADPLATS       TO HIST-ADPLATS                         
196100*    MOVE 'P'                     TO HIST-KDLOC                           
196200*    MOVE 'W0110200'              TO HIST-IDUSER                          
196300*    MOVE 'W0110200'              TO HIST-IDUSER-STO                      
196400*    MOVE WLOGG-DATUM             TO HIST-DASTODAT                        
196500*    PERFORM IMS-ISRT-WDJ901                                              
196600*    PERFORM IMS-ISRT-WDJ911                                              
196700*    .                                                                    
196800*    EJECT                                                                
196900 E-UPPDAT-WDT311 SECTION.                                                 
197000     PERFORM IMS-GHU-WDT301                                               
197100                                                                          
197200     MOVE 'SE' TO W-IDLAND                                                
197300     PERFORM IMS-GHNP-WDT311                                              
197400     IF SEGMENT-FINNS                                                     
197500       MOVE FUNCTION CURRENT-DATE(1:8) TO WLOGG-DATUM                     
197600       COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WLOGG-DATUM             
197700       ACCEPT WLOGG-TID FROM TIME                                         
197800       COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                
197900       MOVE 'SE'                  TO FPCK-IDLANDX2                        
198000       MOVE 'W0110200'            TO FPCK-IDUSER                          
198100       EVALUATE WSORT-IDELMT                                              
198200       WHEN 'BEFT'                                                        
198300         MOVE ARTC-CLAG-BEFT      TO FPCK-BEFT                            
198400                                                                          
198500       WHEN 'KDFORP'                                                      
198600         MOVE ARTC-CLAG-KDFORPPL  TO FPCK-KDFORPPL                        
198700         MOVE ARTC-CLAG-KDFORPGP  TO FPCK-KDFORPGP                        
198800         MOVE ARTC-CLAG-KDFORPUF  TO FPCK-KDFORPUF                        
198900                                                                          
199000       WHEN 'KDFORPPL'                                                    
199100         MOVE ARTC-CLAG-KDFORPPL  TO FPCK-KDFORPPL                        
199200                                                                          
199300       WHEN 'KDFORPGP'                                                    
199400         MOVE ARTC-CLAG-KDFORPGP  TO FPCK-KDFORPGP                        
199500                                                                          
199600       WHEN 'KDFORPUF'                                                    
199700         MOVE ARTC-CLAG-KDFORPUF  TO FPCK-KDFORPUF                        
199800                                                                          
199900       END-EVALUATE                                                       
200000     ELSE                                                                 
200100       MOVE FUNCTION CURRENT-DATE(1:8) TO WLOGG-DATUM                     
200200       COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WLOGG-DATUM             
200300       ACCEPT WLOGG-TID FROM TIME                                         
200400       COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                
200500       MOVE ARTC-CLAG-BEFT        TO FPCK-BEFT                            
200600       MOVE ARTC-CLAG-KDFORPPL    TO FPCK-KDFORPPL                        
200700       MOVE ARTC-CLAG-KDFORPGP    TO FPCK-KDFORPGP                        
200800       MOVE ARTC-CLAG-KDFORPUF    TO FPCK-KDFORPUF                        
200900       MOVE 'W0110200'            TO FPCK-IDUSER                          
201000       MOVE SPACES                TO FPCK-TEBEFT(1)                       
201100       MOVE SPACES                TO FPCK-TEBEFT(2)                       
201200       MOVE SPACES                TO FPCK-TEBEFT(3)                       
201300       MOVE SPACES                TO FPCK-TEBEFT(4)                       
201400       MOVE SPACES                TO FPCK-TEBEFT(5)                       
201500     END-IF                                                               
201600     PERFORM IMS-ISRT-WDT311                                              
201700     .                                                                    
201800     EJECT                                                                
201900 X01-KONV-KONTR-NYTT  SECTION.                                            
202000                                                                          
202100     MOVE MAX-KVHELTAL        TO DEC-KVHELTAL                             
202200     MOVE MAX-KVDECIMAL       TO DEC-KVDECIMAL                            
202300     MOVE WSORT-IDFVARDE-NYTT TO DEC-IDFRIDATA                            
202400     CALL WDECEDIT USING DEC-WDECAREA                                     
202500                                                                          
202600     IF DEC-KDSVAR-OK                                                     
202700       IF WSORT-KDTECKEN-NYTT = '-'                                       
202800          COMPUTE DEC-IDEDITDATA =  - DEC-IDEDITDATA                      
202900       END-IF                                                             
203000     ELSE                                                                 
203100       IF DEC-KVHELTAL > MAX-KVHELTAL                                     
203200       OR DEC-KVDECIMAL > MAX-KVDECIMAL                                   
203300         MOVE '01E' TO FEL-IDFELKODX                                      
203400       ELSE                                                               
203500         MOVE '011' TO FEL-IDFELKODX                                      
203600       END-IF                                                             
203700     END-IF                                                               
203800     .                                                                    
203900     EJECT                                                                
204000*                                                                         
204100*X02-KONV-KONTR-BEF   SECTION.                                            
204200*                                                                         
204300*    MOVE MAX-KVHELTAL        TO DEC-KVHELTAL                             
204400*    MOVE MAX-KVDECIMAL       TO DEC-KVDECIMAL                            
204500*    MOVE WSORT-IDFVARDE-BEF  TO DEC-IDFRIDATA                            
204600*    CALL WDECEDIT USING DEC-WDECAREA                                     
204700*                                                                         
204800*    IF DEC-KDSVAR-OK                                                     
204900*      IF WSORT-KDTECKEN-BEF  = '-'                                       
205000*         COMPUTE DEC-IDEDITDATA =  - DEC-IDEDITDATA                      
205100*      END-IF                                                             
205200*    ELSE                                                                 
205300*      IF DEC-KVHELTAL > MAX-KVHELTAL                                     
205400*      OR DEC-KVDECIMAL > MAX-KVDECIMAL                                   
205500*        MOVE '01E' TO FEL-IDFELKODX                                      
205600*      ELSE                                                               
205700*        MOVE '011' TO FEL-IDFELKODX                                      
205800*      END-IF                                                             
205900*    END-IF                                                               
206000*    .                                                                    
206100*    EJECT                                                                
206200 X10-KOPPL-KONTROLLER-VSOPKOD SECTION.                                    
206300                                                                          
206400     MOVE IN-IDFVARDE-NYTT TO VSOP-IDFVARDE                               
206500                                                                          
206600     IF VSOP-CODE1 NOT NUMERIC                                            
206700       MOVE '041' TO FEL-IDFELKODX                                        
206800     END-IF                                                               
206900                                                                          
207000     IF VSOP-CODE2 NOT NUMERIC                                            
207100       MOVE '042' TO FEL-IDFELKODX                                        
207200     END-IF                                                               
207300                                                                          
207400     IF VSOP-CODE3 NOT NUMERIC                                            
207500       MOVE '043' TO FEL-IDFELKODX                                        
207600     END-IF                                                               
207700                                                                          
207800     IF VSOP-REST  NOT = SPACE                                            
207900       MOVE '01E' TO FEL-IDFELKODX                                        
208000     END-IF                                                               
208100                                                                          
208200     IF FEL-IDFELKODX = SPACE                                             
208300       IF VSOP-CODE1 = 3 OR > 4                                           
208400         MOVE '044' TO FEL-IDFELKODX                                      
208500       END-IF                                                             
208600                                                                          
208700       IF VSOP-CODE2 > 3                                                  
208800         MOVE '045' TO FEL-IDFELKODX                                      
208900       END-IF                                                             
209000                                                                          
209100       IF VSOP-CODE1 = 0                                                  
209200         IF VSOP-CODE3 > 4                                                
209300           MOVE '046' TO FEL-IDFELKODX                                    
209400         END-IF                                                           
209500       END-IF                                                             
209600                                                                          
209700       IF VSOP-CODE1 = 1                                                  
209800         IF VSOP-CODE3 > 6                                                
209900           MOVE '047' TO FEL-IDFELKODX                                    
210000         END-IF                                                           
210100       END-IF                                                             
210200     END-IF                                                               
210300     .                                                                    
210400     EJECT                                                                
210500 X11-KONTROLLERA-ADART  SECTION.                                          
210600                                                                          
210700*    -- KONTROLLERA BEFINTLIGT VÄRDE FORMELLT                             
210800     MOVE WSORT-IDFVARDE-BEF  TO ADART-IDFVARDE                           
210900     IF ADART-ADLAGOMR NOT NUMERIC                                        
211000     OR ADART-ADGANG   NOT NUMERIC                                        
211100     OR ADART-ADPLATS  NOT NUMERIC                                        
211200     OR ADART-REST     NOT = SPACE                                        
211300       MOVE '027' TO  FEL-IDFELKODX                                       
211400     ELSE                                                                 
211500*      -- KONTROLLERA BEFINTLIGT VÄRDE = VÄRDE PÅ BASEN                   
211600       IF ADART-ADLAGOMR NOT = ARTC-CLAG-ADLAGOMR                         
211700       OR ADART-ADGANG   NOT = ARTC-CLAG-ADGANG                           
211800       OR ADART-ADPLATS  NOT = ARTC-CLAG-ADPLATS                          
211900         MOVE '023' TO  FEL-IDFELKODX                                     
212000       ELSE                                                               
212100         MOVE WSORT-IDFVARDE-NYTT  TO ADART-IDFVARDE                      
212200         IF ADART-ADLAGOMR NOT NUMERIC                                    
212300         OR ADART-ADGANG   NOT NUMERIC                                    
212400         OR ADART-ADPLATS  NOT NUMERIC                                    
212500         OR ADART-REST     NOT = SPACE                                    
212600           MOVE '027' TO  FEL-IDFELKODX                                   
212700         END-IF                                                           
212800       END-IF                                                             
212900     END-IF                                                               
213000     EJECT                                                                
213100     .                                                                    
213200     EJECT                                                                
213300 X12-BEHANDLA-IDPROENH    SECTION.                                        
213400                                                                          
213500     IF WSORT-IDFVARDE-NYTT (9:17) NOT = SPACE                            
213600     OR WSORT-IDFVARDE-BEF  (9:17) NOT = SPACE                            
213700        MOVE '01E' TO  FEL-IDFELKODX                                      
213800     ELSE                                                                 
213900                                                                          
214000        EVALUATE TRUE                                                     
214100        WHEN WSORT-IDFVARDE-BEF = SPACE                                   
214200*         -- LÄGG UPP NYTT VÄRDE                                          
214300          MOVE 1 TO IX                                                    
214400          PERFORM UNTIL IX > 3 OR ARTC-CLAG-IDPROENH (IX) = SPACE         
214500             ADD 1 TO IX                                                  
214600          END-PERFORM                                                     
214700          IF IX > 3                                                       
214800             MOVE '025' TO FEL-IDFELKODX                                  
214900          ELSE                                                            
215000             MOVE WSORT-IDFVARDE-NYTT (1:8)                               
215100             TO ARTC-CLAG-IDPROENH (IX)                                   
215200          END-IF                                                          
215300                                                                          
215400        WHEN WSORT-IDFVARDE-NYTT = SPACE                                  
215500*         -- TA BORT BEFINTLIGT VÄRDE                                     
215600          MOVE 1 TO IX                                                    
215700          PERFORM UNTIL IX > 3                                            
215800          OR ARTC-CLAG-IDPROENH (IX) = WSORT-IDFVARDE-BEF (1:8)           
215900             ADD 1 TO IX                                                  
216000          END-PERFORM                                                     
216100          IF IX > 3                                                       
216200             MOVE '022' TO FEL-IDFELKODX                                  
216300          ELSE                                                            
216400             ADD 1 TO IX                                                  
216500             PERFORM UNTIL IX > 3                                         
216600               COMPUTE IX2 = IX - 1                                       
216700               MOVE ARTC-CLAG-IDPROENH (IX)                               
216800               TO ARTC-CLAG-IDPROENH (IX2)                                
216900               ADD 1 TO IX                                                
217000             END-PERFORM                                                  
217100             MOVE SPACE TO ARTC-CLAG-IDPROENH (3)                         
217200          END-IF                                                          
217300                                                                          
217400        WHEN OTHER                                                        
217500*         -- BYT UT BEFINTLIGT VÄRDE MOT NYTT                             
217600          MOVE 1 TO IX                                                    
217700          PERFORM UNTIL IX > 3                                            
217800          OR ARTC-CLAG-IDPROENH (IX) = WSORT-IDFVARDE-BEF (1:8)           
217900             ADD 1 TO IX                                                  
218000          END-PERFORM                                                     
218100          IF IX > 3                                                       
218200             MOVE '022' TO FEL-IDFELKODX                                  
218300          ELSE                                                            
218400             MOVE WSORT-IDFVARDE-NYTT (1:8)                               
218500             TO ARTC-CLAG-IDPROENH (IX)                                   
218600          END-IF                                                          
218700                                                                          
218800        END-EVALUATE                                                      
218900                                                                          
219000     END-IF                                                               
219100     .                                                                    
219200     EJECT                                                                
219300 Z-FINIT SECTION.                                                         
219400     CLOSE W09279-IN                                                      
219500           W01112-FEL                                                     
219600           W09279-UT                                                      
219700     SKIP2                                                                
219800     MOVE 'S' TO POSTSUM-OPKOD                                            
219900     CALL POSTSUM USING POSTSUM-PARM                                      
220000     .                                                                    
220100     EJECT                                                                
220200 S01-LAES-W09279-IN  SECTION.                                             
220300                                                                          
220400     ADD 1      TO WS-ANTAL-IN                                            
220500     READ W09279-IN INTO IN-AREA                                          
220600     AT END                                                               
220700        SET END-OF-W09279 TO TRUE                                         
220800                                                                          
220900     NOT AT END                                                           
221000        MOVE 'W09279'   TO POSTSUM-FDNAMN                                 
221100        MOVE 'W01102D1' TO POSTSUM-DDNAMN2                                
221200        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
221300        CALL POSTSUM USING POSTSUM-PARM                                   
221400     END-READ                                                             
221500     .                                                                    
221600     EJECT                                                                
221700 S10-INITIERA-FELPOST SECTION.                                            
221800                                                                          
221900     MOVE 'R05'         TO  FEL-IDPTYP                                    
222000     MOVE ZERO          TO  FEL-IDDISTR                                   
222100                            FEL-IDKUNDNR                                  
222200                            FEL-KDCLAGER                                  
222300                            FEL-KDFRAKT                                   
222400                            FEL-IDORDNR                                   
222500                            FEL-KDORDKL                                   
222600                            FEL-KDFELMRK                                  
222700     .                                                                    
222800     EJECT                                                                
222900 S11-SKRIV-W01112-FEL SECTION.                                            
223000                                                                          
223100     WRITE FEL-POST FROM FEL-AREA                                         
223200                                                                          
223300     MOVE FEL-IDPTYP  TO POSTSUM-TRANSTYP                                 
223400     MOVE 'W01112'    TO POSTSUM-FDNAMN                                   
223500     MOVE 'W01102D2'  TO POSTSUM-DDNAMN2                                  
223600     CALL POSTSUM USING POSTSUM-PARM                                      
223700     .                                                                    
223800     EJECT                                                                
223900 S20-SKRIV-W09279 SECTION.                                                
224000                                                                          
224100     WRITE UT-POST FROM WSORT-W011042                                     
224200                                                                          
224300     MOVE WSORT-IDPTYP TO POSTSUM-TRANSTYP                                
224400     MOVE 'W09279'     TO POSTSUM-FDNAMN                                  
224500     MOVE 'W01102D3'   TO POSTSUM-DDNAMN2                                 
224600     CALL POSTSUM USING POSTSUM-PARM                                      
224700     .                                                                    
224800     EJECT                                                                
224900 S30-SALDOLOGG SECTION.                                                   
225000                                                                          
225100     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
225200     MOVE 9                         TO LOGG-IDSEKVNR                      
225300     MOVE WC-CDC-SE                 TO LOGG-IDDC                          
225400     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
225500     MOVE 'R05'                     TO LOGG-IDSUBTYP                      
225600     MOVE 'W0110200'                TO LOGG-IDPGM                         
225700     MOVE '0000'                    TO LOGG-IDTRANS                       
225800     MOVE 'UNKNOWN'                 TO LOGG-IDUSER                        
225900     MOVE SPACE                     TO LOGG-REF                           
226000     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
226100     MOVE FUNCTION CURRENT-DATE(1:8) TO WLOGG-DATUM                       
226200     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - WLOGG-DATUM               
226300     ACCEPT WLOGG-TID FROM TIME                                           
226400     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
226500                                                                          
226600     PERFORM IMS-ISRT-WDL9                                                
226700     IF SEGMENT-FINNS-REDAN                                               
226800       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
226900          ADD -1 TO LOGG-IDSEKVNR                                         
227000          PERFORM IMS-ISRT-WDL9                                           
227100       END-PERFORM                                                        
227200     END-IF                                                               
227300     .                                                                    
227400     EJECT                                                                
227500                                                                          
227600* --- IMS SEKTIONER ---                                                   
227700     SKIP3                                                                
227800 IMS-GHU-WDK601-ART SECTION.                                              
227900                                                                          
228000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
228100          DELIMITED BY SIZE INTO SSA1                                     
228200     MOVE '  GE' TO GODK-STATUSKODER                                      
228300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
228400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     .                                                                    
228700     SKIP3                                                                
228800 IMS-REPL-WDK601 SECTION.                                                 
228900                                                                          
229000     MOVE '  ' TO GODK-STATUSKODER                                        
229100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK601                       
229200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
229300     PERFORM IMS-STATUSKONTROLL                                           
229400                                                                          
229500     ADD +1    TO W-ANT-UPPDAT                                            
229600     .                                                                    
229700     EJECT                                                                
229800 IMS-GU-WDK611-CLAG SECTION.                                              
229900                                                                          
230000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
230100          DELIMITED BY SIZE INTO SSA1                                     
230200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
230300          DELIMITED BY SIZE INTO SSA2                                     
230400     MOVE '  GE' TO GODK-STATUSKODER                                      
230500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
230600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
230700     PERFORM IMS-STATUSKONTROLL                                           
230800     .                                                                    
230900     SKIP3                                                                
231000 IMS-GET-WDK611-CLAG SECTION.                                             
231100                                                                          
231200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
231300          DELIMITED BY SIZE INTO SSA1                                     
231400     MOVE '  GE' TO GODK-STATUSKODER                                      
231500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
231600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
231700     PERFORM IMS-STATUSKONTROLL                                           
231800     .                                                                    
231900     SKIP3                                                                
232000 IMS-REPL-WDK611 SECTION.                                                 
232100                                                                          
232200     MOVE '  ' TO GODK-STATUSKODER                                        
232300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
232400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
232500     PERFORM IMS-STATUSKONTROLL                                           
232600                                                                          
232700     ADD +1    TO W-ANT-UPPDAT                                            
232800     .                                                                    
232900     EJECT                                                                
233000 IMS-GET-WDK613-EMB  SECTION.                                             
233100                                                                          
233200     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
233300          DELIMITED BY SIZE INTO SSA1                                     
233400     MOVE '  GE' TO GODK-STATUSKODER                                      
233500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1                  
233600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
233700     PERFORM IMS-STATUSKONTROLL                                           
233800     .                                                                    
233900     SKIP3                                                                
234000 IMS-REPL-WDK613-EMB SECTION.                                             
234100                                                                          
234200     MOVE '  ' TO GODK-STATUSKODER                                        
234300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK613                       
234400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
234500     PERFORM IMS-STATUSKONTROLL                                           
234600                                                                          
234700     ADD +1    TO W-ANT-UPPDAT                                            
234800     .                                                                    
234900     SKIP3                                                                
235000 IMS-ISRT-WDK613-EMB SECTION.                                             
235100                                                                          
235200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
235300          DELIMITED BY SIZE INTO SSA1                                     
235400     MOVE 'WDK613   ' TO SSA2                                             
235500     MOVE '  II' TO GODK-STATUSKODER                                      
235600     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
235700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
235800     PERFORM IMS-STATUSKONTROLL                                           
235900                                                                          
236000     ADD +1    TO W-ANT-UPPDAT                                            
236100     .                                                                    
236200     EJECT                                                                
236300*                                                                         
236400*IMS-ISRT-WDJ901 SECTION.                                                 
236500*                                                                         
236600*    MOVE 'WDJ901   ' TO SSA1                                             
236700*    MOVE '  II' TO GODK-STATUSKODER                                      
236800*    CALL CBLTDLI USING ISRT WDJ9-PCB DLI-IO-WDJ901 SSA1                  
236900*    MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
237000*    PERFORM IMS-STATUSKONTROLL                                           
237100*                                                                         
237200*    ADD +1    TO W-ANT-UPPDAT                                            
237300*    .                                                                    
237400*    EJECT                                                                
237500*IMS-ISRT-WDJ911 SECTION.                                                 
237600*                                                                         
237700*    STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
237800*         DELIMITED BY SIZE INTO SSA1                                     
237900*    MOVE 'WDJ911   ' TO SSA2                                             
238000*    MOVE '  ' TO GODK-STATUSKODER                                        
238100*    CALL CBLTDLI USING ISRT WDJ9-PCB DLI-IO-WDJ911 SSA1 SSA2             
238200*    MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
238300*    PERFORM IMS-STATUSKONTROLL                                           
238400*                                                                         
238500*    ADD +1    TO W-ANT-UPPDAT                                            
238600*    .                                                                    
238700*    EJECT                                                                
238800 IMS-ISRT-WDL9 SECTION.                                                   
238900     MOVE 'WLLOGA01 ' TO SSA1                                             
239000     MOVE '  II' TO GODK-STATUSKODER                                      
239100     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
239200     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
239300     PERFORM IMS-STATUSKONTROLL                                           
239400                                                                          
239500     ADD +1    TO W-ANT-UPPDAT                                            
239600     .                                                                    
239700     EJECT                                                                
239800                                                                          
239900 IMS-GU-WDK601-PCB2 SECTION.                                              
240000     SKIP2                                                                
240100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
240200             DELIMITED BY SIZE INTO SSA1                                  
240300     MOVE '  GE' TO GODK-STATUSKODER                                      
240400     CALL CBLTDLI USING GU WDK6-2-PCB DLI-IO-AREA-K601-2 SSA1             
240500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
240600     PERFORM IMS-STATUSKONTROLL                                           
240700     .                                                                    
240800     SKIP3                                                                
240900 IMS-GNP-WDK611-PCB2 SECTION.                                             
241000     SKIP2                                                                
241100     MOVE 'WDK611   ' TO SSA1                                             
241200     MOVE '  ' TO GODK-STATUSKODER                                        
241300     CALL CBLTDLI USING GNP WDK6-2-PCB DLI-IO-AREA-K611-2 SSA1            
241400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
241500     PERFORM IMS-STATUSKONTROLL                                           
241600     .                                                                    
241700     EJECT                                                                
241800                                                                          
241900 IMS-GN-WDD7A1 SECTION.                                                   
242000     SKIP2                                                                
242100     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
242200                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
242300             DELIMITED BY SIZE INTO SSA1                                  
242400     MOVE '  GE' TO GODK-STATUSKODER                                      
242500     CALL CBLTDLI USING GN WDD7A-PCB DLI-IO-WDD7A1 SSA1                   
242600     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
242700     PERFORM IMS-STATUSKONTROLL.                                          
242800                                                                          
242900     SKIP2                                                                
243000                                                                          
243100 IMS-ISRT-XXBJ-2204 SECTION.                                              
243200     SKIP2                                                                
243300     STRING 'WLXXBJ01(WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
243400          DELIMITED BY SIZE INTO SSA1                                     
243500     MOVE 'WLXXBJ11 ' TO SSA2                                             
243600     MOVE '  ' TO GODK-STATUSKODER                                        
243700     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA-2204 SSA1 SSA2          
243800     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
243900     PERFORM IMS-STATUSKONTROLL                                           
244000                                                                          
244100     ADD +1    TO W-ANT-UPPDAT                                            
244200     .                                                                    
244300     SKIP2                                                                
244400 IMS-ISRT-WDR801   SECTION.                                               
244500     MOVE 'WDR801   ' TO SSA1                                             
244600     MOVE '  II' TO GODK-STATUSKODER                                      
244700     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-AREA-WDR801 SSA1             
244800     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
244900     PERFORM IMS-STATUSKONTROLL                                           
245000     .                                                                    
245100     EJECT                                                                
245200 IMS-GU-WDB601    SECTION.                                                
245300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
245400                    DELIMITED BY SIZE INTO SSA1                           
245500     MOVE '  GE' TO GODK-STATUSKODER                                      
245600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
245700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
245800     PERFORM IMS-STATUSKONTROLL                                           
245900     .                                                                    
246000     EJECT                                                                
246100 IMS-GU-WDK701 SECTION.                                                   
246200                                                                          
246300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
246400          DELIMITED BY SIZE INTO SSA1                                     
246500     MOVE '  GE' TO GODK-STATUSKODER                                      
246600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
246700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
246800     PERFORM IMS-STATUSKONTROLL                                           
246900     .                                                                    
247000     EJECT                                                                
247100 IMS-GN-WDK711 SECTION.                                                   
247200                                                                          
247300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
247400          DELIMITED BY SIZE INTO SSA1                                     
247500     MOVE   'WDK711   ' TO SSA2                                           
247600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
247700     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-WDK711 SSA1  SSA2         
247800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
247900     PERFORM IMS-STATUSKONTROLL                                           
248000     .                                                                    
248100     EJECT                                                                
248200 IMS-GNP-WDK722 SECTION.                                                  
248300                                                                          
248400     MOVE 'WDK722 '     TO SSA1                                           
248500     MOVE '  GE' TO GODK-STATUSKODER                                      
248600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK722 SSA1              
248700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
248800     PERFORM IMS-STATUSKONTROLL                                           
248900     .                                                                    
249000     EJECT                                                                
249100 IMS-GHU-WDT301     SECTION.                                              
249200                                                                          
249300     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
249400          DELIMITED BY SIZE INTO SSA1                                     
249500     MOVE '  GE' TO GODK-STATUSKODER                                      
249600     CALL CBLTDLI USING GHU WDT3-PCB DLI-IO-WDT301 SSA1                   
249700     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
249800     PERFORM IMS-STATUSKONTROLL                                           
249900     .                                                                    
250000     SKIP3                                                                
250100 IMS-GHNP-WDT311 SECTION.                                                 
250200                                                                          
250300     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
250400          DELIMITED BY SIZE INTO SSA1                                     
250500     MOVE '  GE' TO GODK-STATUSKODER                                      
250600     CALL CBLTDLI USING GHNP WDT3-PCB DLI-IO-WDT311 SSA1                  
250700     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
250800     PERFORM IMS-STATUSKONTROLL                                           
250900     .                                                                    
251000     SKIP3                                                                
251100 IMS-ISRT-WDT311 SECTION.                                                 
251200                                                                          
251300     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
251400          DELIMITED BY SIZE INTO SSA1                                     
251500     MOVE 'WDT311 ' TO SSA2                                               
251600     MOVE '  ' TO GODK-STATUSKODER                                        
251700     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
251800     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
251900     PERFORM IMS-STATUSKONTROLL                                           
252000                                                                          
252100     ADD +1    TO W-ANT-UPPDAT                                            
252200     .                                                                    
252300     EJECT                                                                
252400 IMS-GU-R220 SECTION.                                                     
252500                                                                          
252600     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
252700          DELIMITED BY SIZE INTO SSA1                                     
252800     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
252900          DELIMITED BY SIZE INTO SSA2                                     
253000     MOVE '  GE' TO GODK-STATUSKODER                                      
253100     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2232 SSA1 SSA2            
253200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
253300     PERFORM IMS-STATUSKONTROLL                                           
253400     .                                                                    
253500     SKIP2                                                                
253600 IMS-GHU-R501 SECTION.                                                    
253700     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
253800          DELIMITED BY SIZE INTO SSA1                                     
253900     MOVE '  GE'           TO GODK-STATUSKODER                            
254000     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-AREA-2223 SSA1                
254100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
254200     PERFORM IMS-STATUSKONTROLL                                           
254300     .                                                                    
254400                                                                          
254500 IMS-ISRT-R501 SECTION.                                                   
254600     MOVE 'WDR501   '      TO SSA1                                        
254700     MOVE '  II'           TO GODK-STATUSKODER                            
254800     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2223 SSA1               
254900     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
255000     PERFORM IMS-STATUSKONTROLL                                           
255100     .                                                                    
255200                                                                          
255300 IMS-ISRT-R550 SECTION.                                                   
255400     MOVE 'WDR550   '      TO SSA1                                        
255500     MOVE '  II'           TO GODK-STATUSKODER                            
255600     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2224 SSA1               
255700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
255800     PERFORM IMS-STATUSKONTROLL                                           
255900     .                                                                    
256000     EJECT                                                                
256101 IMS-GU-WDT501 SECTION.                                                   
256201     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
256301          DELIMITED BY SIZE INTO SSA1                                     
256401     MOVE '  GE' TO GODK-STATUSKODER                                      
256501     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
256601     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
256701     PERFORM IMS-STATUSKONTROLL                                           
256801     .                                                                    
256901     EJECT                                                                
257001 IMS-ISRT-WDT501 SECTION.                                                 
257101     MOVE 'WDT501 ' TO SSA1                                               
257201     MOVE '  II' TO GODK-STATUSKODER                                      
257301     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT501 SSA1                  
257401     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
257501     PERFORM IMS-STATUSKONTROLL                                           
257601     .                                                                    
257602 IMS-GHNP-WDT511 SECTION.                                                 
257603     MOVE   'WDT511  *F' TO SSA1                                          
257605     MOVE '  ' TO GODK-STATUSKODER                                        
257606     CALL CBLTDLI USING GHNP  WDT5-PCB DLI-IO-WDT511 SSA1                 
257607     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
257608     PERFORM IMS-STATUSKONTROLL                                           
257609     .                                                                    
257610     EJECT                                                                
257620 IMS-REPL-WDT511 SECTION.                                                 
257640     MOVE '  ' TO GODK-STATUSKODER                                        
257650     CALL CBLTDLI USING REPL  WDT5-PCB DLI-IO-WDT511                      
257660     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
257670     PERFORM IMS-STATUSKONTROLL                                           
257680     .                                                                    
257690     EJECT                                                                
257701 IMS-ISRT-WDT511 SECTION.                                                 
257801     STRING 'WDT501  (IDARTNR  =' W-IDARTNR-X ')'                         
257901            DELIMITED BY SIZE INTO SSA1                                   
258001     MOVE 'WDT511 ' TO SSA2                                               
258101     MOVE '  ' TO GODK-STATUSKODER                                        
258201     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT511 SSA1 SSA2             
258301     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
258401     PERFORM IMS-STATUSKONTROLL                                           
258501     .                                                                    
258600 IMS-STATUSKONTROLL SECTION.                                              
258700                                                                          
258800     SET STATUS-IX TO 1                                                   
258900     SEARCH GODK-STATUS                                                   
259000       AT END                                                             
259100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
259200           DELIMITED BY SIZE INTO FELTEXT-STR                             
259300         DISPLAY FELTEXT                                                  
259400         CALL FELLOG                                                      
259500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
259600         CONTINUE                                                         
259700     END-SEARCH                                                           
260000     .                                                                    
