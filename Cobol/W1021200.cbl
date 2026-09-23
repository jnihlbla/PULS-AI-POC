000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1021200.                                                
000400 AUTHOR.         ANN-MARIE DAGE.                                          
000500 DATE-WRITTEN.   90/05/17.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET KAN VISA, UPPDATERA, FLYTTA, KOPIERA                  
001100*        OCH NYUPPLÄGGA RADER INOM EN STRUKTUR.                           
001200*                                                                         
001300*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*        PROGRAMMET LÄSER      WLBENC (WDK6)                              
001600*        PROGRAMMET LÄSER      WDF5                                       
001700*        PROGRAMMET UPPDATERAR WLSATB (WDJ1)                              
001800*                                                                         
001900*        ÄT SPLIT 930402 BL                                               
002000*           - KONTROLL PRODUKTSLAG FÖRPACKNING ÄNDRAT                     
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W1T212, W1T212U                                     
002400*        MID:         W1I21201                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W1O21201                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W1021200'.            
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100 77  STRIND                      PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  STRIND2                     PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77  K-STRIND                    PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  RADIND                      PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  RADIND-MAX                  PIC S9(9)   VALUE +12  COMP SYNC.        
004600                                                                          
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
005100 77  WS-STRNR                    PIC X(9)    VALUE SPACE.                 
005200 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005300 77  WS-IDRADNR                  PIC X(5)  VALUE SPACE JUST RIGHT.        
005400 77  WS-IDRADNR4                 PIC X(4)    VALUE SPACE.                 
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  EJ-TID-SIGNAL                       VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006600     88  ALLT-OK                             VALUE 'J'.                   
006700                                                                          
006800 77  AO-SW                       PIC X       VALUE 'N'.                   
006900     88  AO-SKALL-FINNAS                     VALUE 'J'.                   
007000                                                                          
007100 77  IDTRANS-SW                  PIC X(4)    VALUE SPACE.                 
007200     88  EGEN-MID                            VALUE '1212'.                
007300     88  1213-MID                            VALUE '1213'.                
007400     88  GODK-MID                            VALUE '1211' '1212'          
007500                                                   '1213' '1214'          
007600                                                   '1215'.                
007700     88  GODK-MID-MED-IDSKYLT                VALUE '1211' '1212'          
007800                                                   '1213' '1214'.         
007900     88  GODK-MID-MED-IDRADNR                VALUE '1212' '1213'.         
008000                                                                          
008100 77  KDSORT-SW                   PIC X(2)    VALUE SPACE.                 
008200     88  KDSORT-OK                           VALUE 'ST' 'SA' 'KG'         
008300                                             'M ' ' M' 'L ' ' L'          
008400                                             'MM' 'G ' ' G' 'C2'          
008500                                             'M2' 'ML' 'TM'.              
008600 77  WS-BEFT-AKTUELL             PIC S9(3)   COMP-3 VALUE ZERO.           
008700     88  BEFT-AKTUELL                        VALUE 70 71 72 73            
008800                                                   74 75.                 
008900                                                                          
009000 77  KONV-SW                     PIC X       VALUE 'N'.                   
009100     88  KONV-FINNS                          VALUE 'J'.                   
009200     88  KONV-SAKNAS                         VALUE 'N'.                   
009300                                                                          
009400 77  RADNR-SW                    PIC X       VALUE 'N'.                   
009500     88  RADNR-SAKNAS                        VALUE 'N'.                   
009600     88  RADNR-FINNS                         VALUE 'J'.                   
009700                                                                          
009800 77  RAD-SW                      PIC X       VALUE 'N'.                   
009900     88  RADEN-SAKNAS                        VALUE 'N'.                   
010000     88  RADEN-FINNS                         VALUE 'J'.                   
010100                                                                          
010200 77  UPPDAT-SW                   PIC X       VALUE 'N'.                   
010300     88  UPPDATERAT                          VALUE 'J'.                   
010400                                                                          
010500 77  KLAR-SW                     PIC X       VALUE 'N'.                   
010600     88  STRUKTUR-EJ-KLAR                    VALUE 'N'.                   
010700     88  STRUKTUR-KLAR                       VALUE 'J'.                   
010800                                                                          
010900 77  STRNR-FINNS-SW              PIC X       VALUE 'N'.                   
011000     88  STRNR-FINNS-EJ                      VALUE 'N'.                   
011100     88  STRNR-FINNS                         VALUE 'J'.                   
011200                                                                          
011300 77  STRUKTURNR-TYP-SW           PIC X       VALUE SPACE.                 
011400     88  STRUKTURNR-SPAERRAT                 VALUE 'S'.                   
011500     88  STRUKTURNR-BORTTAGET                VALUE 'S'.                   
011600                                                                          
011700 77  UTSKRIV-SW                  PIC X       VALUE 'N'.                   
011800     88  EJ-UTSKRIVEN                        VALUE 'N'.                   
011900     88  UTSKRIVEN                           VALUE 'J'.                   
012000                                                                          
012100 77  NOT-SW                      PIC X       VALUE 'N'.                   
012200     88  INGA-NOT                            VALUE 'N'.                   
012300     88  NOTERINGAR                          VALUE 'J'.                   
012400                                                                          
012500 77  FORP-SW                     PIC X       VALUE 'N'.                   
012600     88  NY-FORP                             VALUE 'J'.                   
012700                                                                          
012800 77  BORT-SW                     PIC X       VALUE 'N'.                   
012900     88  BORTTAGEN-RAD                       VALUE 'J'.                   
013000                                                                          
013100 77  DATUM-KONTROLLERAT          PIC X       VALUE 'N'.                   
013200     EJECT                                                                
013300                                                                          
013400*01  -COPY WWPRODSL                                                       
013500                                                                          
013600*      --- VALID IDDC CODES                                               
013700*                                                                         
013800*01    -COPY WWDCKONS                                                     
013900*01    -COPY WWDC99                                                       
014000       EJECT                                                              
014100*    --- ARBETSAREOR                                                      
014200 01  ARBETSAREOR.                                                         
014300                                                                          
014400     03  DAGENS-DATUM              PIC 9(6).                              
014500                                                                          
014600     03  INNEVARANDE-VECKA         PIC 9(6).                              
014700                                                                          
014800     03  AKTUELLT-DATUM            PIC 9(6).                              
014900                                                                          
015000     03  WS-SPAR-TIFINLV           PIC 9(6).                              
015100                                                                          
015200     03  WS-KDBENHOM-NUM           PIC 9(1)   VALUE  0.                   
015300     03  WS-STR-IDLEVNR-ARTC       PIC X(5)   VALUE SPACE.                
015400                                                                          
015500     03  WS-RADNR                  PIC S9(5)  VALUE +0.                   
015600     03  WS-RADNR-NYTT             PIC S9(5)  VALUE +0.                   
015700     03  WS-RADNR-GAM              PIC S9(5)  VALUE +0.                   
015800                                                                          
015900     03  WS-RADNR4                 PIC S9(4)  VALUE +0.                   
016000                                                                          
016100     03  WS-STR-NOT-WDJ122         PIC X(141) VALUE SPACE.                
016200                                                                          
016300     03  WS-TIAAVV                 PIC 9(4).                              
016400     03  WS-TIAAVV-A               REDEFINES WS-TIAAVV.                   
016500       05  WS-TIAA                 PIC 99.                                
016600       05  WS-TIVV                 PIC 99.                                
016700                                                                          
016800     03  WS-STRNR-SPAR             PIC S9(9)  VALUE +0    COMP-3.         
016900     03  WS-IDARTNR-SPAR           PIC S9(9)  VALUE +0    COMP-3.         
017000     03  WS-IDARTNR-ART-SPAR       PIC S9(9)  VALUE +0    COMP-3.         
017100                                                                          
017200     03  WS-REANTPSA               PIC S9(2)V9(3) VALUE +0 COMP-3.        
017300                                                                          
017400     03  WS-TISTODAT               PIC S9(7)               COMP-3.        
017500     03  WS-TIUPPDAT-GAM           PIC S9(7)               COMP-3.        
017600                                                                          
017700     03  WS-PB-SEP-TOT             PIC S9(6)V9             COMP-3.        
017800                                                                          
017900     03  WS-IDLEVNR                PIC X(5)   VALUE SPACE.                
018000     03  WS-IDLEVNR-PLUS           PIC X(5)   VALUE '+++++'.              
018100     03  WS-BELEVART               PIC X(30)  VALUE SPACE.                
018200     03  WS-BELEVART-PLUS          PIC X(30)                              
018300                           VALUE '++++++++++++++++++++++++++++++'.        
018400     03  WS-BEART                  PIC X(25)  VALUE SPACE.                
018500     03  WS-IDSTRTYP               PIC X      VALUE SPACE.                
018600     03  WS-KDSORT                 PIC X(2)   VALUE SPACE.                
018700                                                                          
018800     03  SPAERRAT                  PIC X      VALUE 'S'.                  
018900     03  BORTTAGET                 PIC X      VALUE 'B'.                  
019000                                                                          
019100     03  WS-LAGRA-STR.                                                    
019200*        05  -COPY WDJ101     -PRE WS-                                    
019300                                                                          
019400     03 FILLER                   PIC X(12)   VALUE 'WS-LAGRA-RAD'.        
019500     SKIP3                                                                
019600     03  WS-LAGRA-RAD.                                                    
019700*        05  -COPY WDJ111     -PRE WS-                                    
019800                                                                          
019900     03  WS-LAGRA-NOT.                                                    
020000*        05  -COPY WDJ122     -PRE WS-                                    
020100                                                                          
020200     03  BLANKA-RAD.                                                      
020300       05  FILLER                  PIC X      VALUE SPACE.                
020400       05  FILLER                  PIC S9(5)  VALUE ZERO  COMP-3.         
020500       05  FILLER                  PIC X(5)   VALUE SPACE.                
020600       05  FILLER                  PIC X(30)  VALUE SPACE.                
020700       05  FILLER                  PIC S9(9)  VALUE ZERO  COMP-3.         
020800       05  FILLER                  PIC X(46)  VALUE SPACE.                
020900       05  FILLER                  PIC S9     VALUE ZERO  COMP-3.         
021000       05  FILLER                  PIC X(3)   VALUE SPACE.                
021100       05  FILLER                  PIC S9(5)  VALUE ZERO  COMP-3.         
021200       05  FILLER                  PIC S9(7)  VALUE ZERO  COMP-3.         
021300       05  FILLER                  PIC S9(7)  VALUE ZERO  COMP-3.         
021400       05  FILLER                  PIC S9(7)  VALUE ZERO  COMP-3.         
021500                                                                          
021600 01  TAB-STR-MAX                 PIC S9(3)    VALUE 100.                  
021700                                                                          
021800 01  STR-TABELL.                                                          
021900     03 TAB-STR-STRNR            OCCURS 100   PIC S9(9)    COMP-3.        
022000                                                                          
022100                                                                          
022200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
022300 01  GENERELLA-SUBPROGRAM.                                                
022400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
022500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
022600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
022900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
023000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
023100     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
023200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023300     EJECT                                                                
023400*    --- PARAMETRAR TILL SUBPROGRAM W009VADD                              
023500 01  W009VADD-AREA.                                                       
023600     03  W009VADD-AAVV           PIC S9(5)   COMP-3.                      
023700     03  ANTAL-AAVV              PIC S9(3)   COMP-3.                      
023800     SKIP3                                                                
023900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
024000*   -COPY WMEDAREA                                                        
024100     EJECT                                                                
024200*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
024300*   -COPY WORKAREA                                                        
024400     EJECT                                                                
024500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
024600*   -COPY WDATAREA                                                        
024700     EJECT                                                                
024800*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
024900*   -COPY WDECAREA                                                        
025000     EJECT                                                                
025100*    --- PARAMETRAR TILL WWLAND03                                         
025200*   -COPY WWLAND03                                                        
025300     EJECT                                                                
025400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
025500*   -COPY WMSGINIT                                                        
025600     EJECT                                                                
025700 01  MEDDELANDEN.                                                         
025800     03  FEL-1.                                                           
025900         05  FILLER              PIC X(40)  VALUE                         
026000            'MATA IN NYA NYCKLAR'.                                        
026100         05  FILLER              PIC X(40)  VALUE                         
026200            'ENTER NEW KEYS'.                                             
026300     03  FILLER REDEFINES FEL-1.                                          
026400         05  FEL1 OCCURS 2       PIC X(40).                               
026500                                                                          
026600     03  FEL-2.                                                           
026700         05  FILLER              PIC X(40)  VALUE                         
026800             'STRUKTUR ÄR UNDER BEARBETNING'.                             
026900         05  FILLER              PIC X(40)  VALUE                         
027000             'STRUCTURE IS IN USE'.                                       
027100     03  FILLER REDEFINES FEL-2.                                          
027200         05  FEL2 OCCURS 2       PIC X(40).                               
027300                                                                          
027400     03  FEL-3.                                                           
027500         05  FILLER              PIC X(40)  VALUE                         
027600             'STRUKTUR BORTTAGSMÄRKT '.                                   
027700         05  FILLER              PIC X(40)  VALUE                         
027800             'STRUCTURE IS MARKED TO BE DELETED'.                         
027900     03  FILLER REDEFINES FEL-3.                                          
028000         05  FEL3 OCCURS 2       PIC X(40).                               
028100                                                                          
028200     03  FEL-4.                                                           
028300         05  FILLER              PIC X(40)  VALUE                         
028400              'DETTA ÄR EN FÖRPACKNINGSRAD'.                              
028500         05  FILLER              PIC X(40)  VALUE                         
028600              'PACKAGE LINE'.                                             
028700     03  FILLER REDEFINES FEL-4.                                          
028800         05  FEL4 OCCURS 2       PIC X(40).                               
028900                                                                          
029000     03  FEL-5.                                                           
029100         05  FILLER              PIC X(40)  VALUE                         
029200              'DETTA ÄR SISTA RADEN I STRUKTUREN'.                        
029300         05  FILLER              PIC X(40)  VALUE                         
029400              'LAST LINE IS SHOWN'.                                       
029500     03  FILLER REDEFINES FEL-5.                                          
029600         05  FEL5 OCCURS 2       PIC X(40).                               
029700                                                                          
029800     03  FEL-6.                                                           
029900         05  FILLER              PIC X(40)  VALUE                         
030000              'STRUKTUREN HAR INGA RADER'.                                
030100         05  FILLER              PIC X(40)  VALUE                         
030200              'NO STRUCTURE LINES EXIST'.                                 
030300     03  FILLER REDEFINES FEL-6.                                          
030400         05  FEL6 OCCURS 2       PIC X(40).                               
030500                                                                          
030600     03  FEL-8.                                                           
030700         05  FILLER              PIC X(40)  VALUE                         
030800             'RADEN FINNS EJ I STRUKTUREN'.                               
030900         05  FILLER              PIC X(40)  VALUE                         
031000             'LINE IS MISSING'.                                           
031100     03  FILLER REDEFINES FEL-8.                                          
031200         05  FEL8 OCCURS 2       PIC X(40).                               
031300                                                                          
031400     03  FEL-9.                                                           
031500         05  FILLER              PIC X(40)  VALUE                         
031600             'STRUKTUR SAKNAS'.                                           
031700         05  FILLER              PIC X(40)  VALUE                         
031800             'STRUCTURE IS MISSING'.                                      
031900     03  FILLER REDEFINES FEL-9.                                          
032000         05  FEL9 OCCURS 2       PIC X(40).                               
032100                                                                          
032200     03  FEL-10.                                                          
032300         05  FILLER              PIC X(40)  VALUE                         
032400            'UPPDATERING EJ TILLÅTEN'.                                    
032500         05  FILLER              PIC X(40)  VALUE                         
032600             'UPDATE NOT ALLOWED'.                                        
032700     03  FILLER REDEFINES FEL-10.                                         
032800         05  FEL10 OCCURS 2      PIC X(40).                               
032900                                                                          
033000     03  FEL-11.                                                          
033100         05  FILLER              PIC X(40)  VALUE                         
033200            'STRUKTUR ÄR UNDER BEARBETNING'.                              
033300         05  FILLER              PIC X(40)  VALUE                         
033400            'STRUCTURE IS IN USE'.                                        
033500     03  FILLER REDEFINES FEL-11.                                         
033600         05  FEL11 OCCURS 2      PIC X(40).                               
033700                                                                          
033800     03  FEL-13.                                                          
033900         05  FILLER              PIC X(40)  VALUE                         
034000            'UPPDATERING EJ MÖJLIG'.                                      
034100         05  FILLER              PIC X(40)  VALUE                         
034200            'UPDATE NOT POSSIBLE'.                                        
034300     03  FILLER REDEFINES FEL-13.                                         
034400         05  FEL13 OCCURS 2      PIC X(40).                               
034500                                                                          
034600     03  FEL-14.                                                          
034700         05  FILLER              PIC X(40)  VALUE                         
034800             'ARTIKEL RENSAD PÅ ARTIKELREGISTRET'.                        
034900         05  FILLER              PIC X(40)  VALUE                         
035000             'PART NUMBER DELETED'.                                       
035100     03  FILLER REDEFINES FEL-14.                                         
035200         05  FEL14 OCCURS 2      PIC X(40).                               
035300                                                                          
035400     03  FEL-15.                                                          
035500         05  FILLER              PIC X(60)  VALUE                         
035600            'UPPDATERING UTFÖRD. STRUKTUR EJ KLAR'.                       
035700         05  FILLER              PIC X(60)  VALUE                         
035800            'UPDATED. STRUCTURE NOT COMPLETE'.                            
035900     03  FILLER REDEFINES FEL-15.                                         
036000         05  FEL15 OCCURS 2      PIC X(60).                               
036100                                                                          
036200     03  FEL-16.                                                          
036300         05  FILLER              PIC X(40)  VALUE                         
036400             'STRUKTUR EJ KLAR'.                                          
036500         05  FILLER              PIC X(40)  VALUE                         
036600             'STRUCTURE NOT COMPLETE'.                                    
036700     03  FILLER REDEFINES FEL-16.                                         
036800         05  FEL16 OCCURS 2      PIC X(40).                               
036900                                                                          
037000     03  FEL-17.                                                          
037100         05  FILLER              PIC X(60)  VALUE                         
037200             'DU HAR STRUKTUR UNDER BEARBETNING'.                         
037300         05  FILLER              PIC X(60)  VALUE                         
037400             'YOU HAVE STRUCTURE IN USE'.                                 
037500     03  FILLER REDEFINES FEL-17.                                         
037600         05  FEL17 OCCURS 2      PIC X(60).                               
037700                                                                          
037800     03  MED-1.                                                           
037900         05  FILLER              PIC X(60)  VALUE                         
038000            'STRUKTUR RENSAD PÅ ARTIKELREGISTRET'.                        
038100         05  FILLER              PIC X(60)  VALUE                         
038200            'PART NUMBER DELETED'.                                        
038300     03  FILLER REDEFINES MED-1.                                          
038400         05  MED1 OCCURS 2       PIC X(60).                               
038500                                                                          
038600     03  MED-3.                                                           
038700         05  FILLER              PIC X(60)  VALUE                         
038800            'BENÄMNING FINNS EJ PÅ INRAPPORTERAD SPÅKKOD'.                
038900         05  FILLER              PIC X(60)  VALUE                         
039000            'DESCRIPTION IS MISSING'.                                     
039100     03  FILLER REDEFINES MED-3.                                          
039200         05  MED3 OCCURS 2       PIC X(60).                               
039300                                                                          
039400     03  MED-4.                                                           
039500         05  FILLER              PIC X(40)  VALUE                         
039600            'ÄO KRÄVS'.                                                   
039700         05  FILLER              PIC X(40)  VALUE                         
039800            'DCN NO IS MISSING'.                                          
039900     03  FILLER REDEFINES MED-4.                                          
040000         05  MED4 OCCURS 2       PIC X(40).                               
040100                                                                          
040200     03  MED-6.                                                           
040300         05  FILLER              PIC X(40)  VALUE                         
040400           'J=JA ELLER N=NEJ MÅSTE RAPPORTERAS'.                          
040500         05  FILLER              PIC X(40)  VALUE                         
040600           'GIVE Y=YES OR N=NO'.                                          
040700     03  FILLER REDEFINES MED-6.                                          
040800         05  MED6 OCCURS 2       PIC X(40).                               
040900                                                                          
041000     03  MED-7.                                                           
041100         05  FILLER              PIC X(40)  VALUE                         
041200           'RADEN FINNS REDAN'.                                           
041300         05  FILLER              PIC X(40)  VALUE                         
041400           'LINE NO EXISTS IN STRUCTURE'.                                 
041500     03  FILLER REDEFINES MED-7.                                          
041600         05  MED7 OCCURS 2       PIC X(40).                               
041700                                                                          
041800     03  MED-8.                                                           
041900         05  FILLER              PIC X(40)  VALUE                         
042000           'RADNUMMER EJ NUMERISKT'.                                      
042100         05  FILLER              PIC X(40)  VALUE                         
042200           'LINE NO NOT NUMERIC'.                                         
042300     03  FILLER REDEFINES MED-8.                                          
042400         05  MED8 OCCURS 2       PIC X(40).                               
042500                                                                          
042600     03  MED-9.                                                           
042700         05  FILLER              PIC X(60)  VALUE                         
042800           'ARTIKELNUMMER FÅR EJ ÄNDRAS VID KOPIERING'.                   
042900         05  FILLER              PIC X(60)  VALUE                         
043000           'UPDATE NOT ALLOWED'.                                          
043100     03  FILLER REDEFINES MED-9.                                          
043200         05  MED9 OCCURS 2       PIC X(60).                               
043300                                                                          
043400     03  MED-10.                                                          
043500         05  FILLER              PIC X(60)  VALUE                         
043600           'LEVERANTÖRSNUMMER FÅR EJ ÄNDRAS VID KOPIERING'.               
043700         05  FILLER              PIC X(60)  VALUE                         
043800           'UPDATE NOT ALLOWED'.                                          
043900     03  FILLER REDEFINES MED-10.                                         
044000         05  MED10 OCCURS 2      PIC X(60).                               
044100                                                                          
044200     03  MED-11.                                                          
044300         05  FILLER              PIC X(60)  VALUE                         
044400           'ARTIKELBENÄMNING FÅR EJ ÄNDRAS VID KOPIERING'.                
044500         05  FILLER              PIC X(60)  VALUE                         
044600           'UPDATE NOT ALLOWED'.                                          
044700     03  FILLER REDEFINES MED-11.                                         
044800         05  MED11 OCCURS 2      PIC X(60).                               
044900                                                                          
045000     03  MED-12.                                                          
045100         05  FILLER              PIC X(60)  VALUE                         
045200           'HOMONYMKOD FÅR EJ ÄNDRAS VID KOPIERING'.                      
045300         05  FILLER              PIC X(60)  VALUE                         
045400           'UPDATE NOT ALLOWED'.                                          
045500     03  FILLER REDEFINES MED-12.                                         
045600         05  MED12 OCCURS 2      PIC X(60).                               
045700                                                                          
045800     03  MED-13.                                                          
045900         05  FILLER              PIC X(40)  VALUE                         
046000           'ANTAL FELAKTIGT'.                                             
046100         05  FILLER              PIC X(40)  VALUE                         
046200           'QUANTITY NOT CORRECT'.                                        
046300     03  FILLER REDEFINES MED-13.                                         
046400         05  MED13 OCCURS 2      PIC X(40).                               
046500                                                                          
046600     03  MED-14.                                                          
046700         05  FILLER              PIC X(60)  VALUE                         
046800           'STRUKTURTYP FÅR EJ ÄNDRAS VID KOPIERING'.                     
046900         05  FILLER              PIC X(60)  VALUE                         
047000           'UPDATE NOT ALLOWED'.                                          
047100     03  FILLER REDEFINES MED-14.                                         
047200         05  MED14 OCCURS 2      PIC X(60).                               
047300                                                                          
047400     03  MED-15.                                                          
047500         05  FILLER              PIC X(60)  VALUE                         
047600            'SORT FÅR EJ ÄNDRAS VID KOPIERING'.                           
047700         05  FILLER              PIC X(60)  VALUE                         
047800            'UPDATE NOT ALLOWED'.                                         
047900     03  FILLER REDEFINES MED-15.                                         
048000         05  MED15 OCCURS 2      PIC X(60).                               
048100                                                                          
048200     03  MED-16.                                                          
048300         05  FILLER              PIC X(60)  VALUE                         
048400            'FLYTTNING ELLER BORTTAG OCKSÅ RAPPORTERAT'.                  
048500         05  FILLER              PIC X(60)  VALUE                         
048600            'UPDATE NOT POSSIBLE'.                                        
048700     03  FILLER REDEFINES MED-16.                                         
048800         05  MED16 OCCURS 2      PIC X(60).                               
048900                                                                          
049000     03  MED-17.                                                          
049100         05  FILLER              PIC X(60)  VALUE                         
049200            'FÄLT FÅR EJ UPPDATERAS I SAMBAND MED FLYTTNING'.             
049300         05  FILLER              PIC X(60)  VALUE                         
049400            'UPDATE NOT ALLOWED'.                                         
049500     03  FILLER REDEFINES MED-17.                                         
049600         05  MED17 OCCURS 2      PIC X(60).                               
049700                                                                          
049800     03  MED-18.                                                          
049900         05  FILLER              PIC X(60)  VALUE                         
050000           'FÄLT FÅR EJ UPPDATERAS I SAMBAND MED BORTTAG'.                
050100         05  FILLER              PIC X(60)  VALUE                         
050200           'UPDATE NOT ALLOWED'.                                          
050300     03  FILLER REDEFINES MED-18.                                         
050400         05  MED18 OCCURS 2      PIC X(60).                               
050500                                                                          
050600     03  MED-19.                                                          
050700         05  FILLER              PIC X(60)  VALUE                         
050800           'KOPIERING ELLER BORTTAG ÄR OCKSÅ RAPPORTERAT'.                
050900         05  FILLER              PIC X(60)  VALUE                         
051000           'UPDATE NOT POSSIBLE'.                                         
051100     03  FILLER REDEFINES MED-19.                                         
051200         05  MED19 OCCURS 2      PIC X(60).                               
051300                                                                          
051400     03  MED-20.                                                          
051500         05  FILLER              PIC X(60)  VALUE                         
051600           'LEVERANTÖRSNUMMER FELAKTIGT'.                                 
051700         05  FILLER              PIC X(60)  VALUE                         
051800           'SUPPLIER NUMBER NOT CORRECT'.                                 
051900     03  FILLER REDEFINES MED-20.                                         
052000         05  MED20 OCCURS 2      PIC X(60).                               
052100                                                                          
052200     03  MED-21.                                                          
052300         05  FILLER              PIC X(60)  VALUE                         
052400           'LEVERANTÖRSNUMMER MÅSTE RAPPORTERAS'.                         
052500         05  FILLER              PIC X(60)  VALUE                         
052600           'GIVE A SUPPLIER NUMBER'.                                      
052700     03  FILLER REDEFINES MED-21.                                         
052800         05  MED21 OCCURS 2      PIC X(60).                               
052900                                                                          
053000     03  MED-22.                                                          
053100         05  FILLER              PIC X(60)  VALUE                         
053200             'LEV.BENÄMNING FÅR EJ ÄNDRAS VID KOPIERING'.                 
053300         05  FILLER              PIC X(60)  VALUE                         
053400             'UPDATE NOT ALLOWED'.                                        
053500     03  FILLER REDEFINES MED-22.                                         
053600         05  MED22 OCCURS 2      PIC X(60).                               
053700                                                                          
053800     03  MED-24.                                                          
053900         05  FILLER              PIC X(40)  VALUE                         
054000             'DATUM FELAKTIGT'.                                           
054100         05  FILLER              PIC X(40)  VALUE                         
054200             'WEEK NOT CORRECT'.                                          
054300     03  FILLER REDEFINES MED-24.                                         
054400         05  MED24 OCCURS 2      PIC X(40).                               
054500                                                                          
054600     03  MED-25.                                                          
054700         05  FILLER              PIC X(40)  VALUE                         
054800           'DATUM MÅSTE FINNAS'.                                          
054900         05  FILLER              PIC X(40)  VALUE                         
055000             'WEEK NOT CORRECT'.                                          
055100     03  FILLER REDEFINES MED-25.                                         
055200         05  MED25 OCCURS 2      PIC X(40).                               
055300                                                                          
055400     03  MED-26.                                                          
055500         05  FILLER              PIC X(60)  VALUE                         
055600           'KOPIERING ELLER FLYTT ÄR OCKSÅ RAPPORTERAT'.                  
055700         05  FILLER              PIC X(60)  VALUE                         
055800           'UPDATE NOT POSSIBLE'.                                         
055900     03  FILLER REDEFINES MED-26.                                         
056000         05  MED26 OCCURS 2      PIC X(60).                               
056100                                                                          
056200     03  MED-27.                                                          
056300         05  FILLER              PIC X(40)  VALUE                         
056400             'RADEN GÄLLER INTE LÄNGRE'.                                  
056500         05  FILLER              PIC X(40)  VALUE                         
056600             'NOT A VALID LINE'.                                          
056700     03  FILLER REDEFINES MED-27.                                         
056800         05  MED27 OCCURS 2      PIC X(40).                               
056900                                                                          
057000     03  MED-28.                                                          
057100         05  FILLER              PIC X(40)  VALUE                         
057200            'ARTIKELNR FELAKTIGT'.                                        
057300         05  FILLER              PIC X(40)  VALUE                         
057400             'PART NUMBER NOT CORRECT'.                                   
057500     03  FILLER REDEFINES MED-28.                                         
057600         05  MED28 OCCURS 2      PIC X(40).                               
057700                                                                          
057800     03  MED-29.                                                          
057900         05  FILLER              PIC X(60)  VALUE                         
058000           'ART.NR FÅR INTE VARA SAMMA SOM STRUKT.NR'.                    
058100         05  FILLER              PIC X(60)  VALUE                         
058200           'PART NUMBER NOT CORRECT'.                                     
058300     03  FILLER REDEFINES MED-29.                                         
058400         05  MED29 OCCURS 2      PIC X(60).                               
058500                                                                          
058600     03  MED-30.                                                          
058700         05  FILLER              PIC X(60)  VALUE                         
058800            'LEV. ARTIKELBENÄMNING MÅSTE RAPPORTERAS'.                    
058900         05  FILLER              PIC X(60)  VALUE                         
059000            'GIVE A REFERENCE'.                                           
059100     03  FILLER REDEFINES MED-30.                                         
059200         05  MED30 OCCURS 2      PIC X(60).                               
059300                                                                          
059400     03  MED-31.                                                          
059500         05  FILLER              PIC X(60)  VALUE                         
059600           'STRUKTURTYPEN ÄR EJ GODKÄND'.                                 
059700         05  FILLER              PIC X(60)  VALUE                         
059800           'TYPE OF STRUCTURE NOT CORRECT'.                               
059900     03  FILLER REDEFINES MED-31.                                         
060000         05  MED31 OCCURS 2      PIC X(60).                               
060100                                                                          
060200     03  MED-32.                                                          
060300         05  FILLER              PIC X(40)  VALUE                         
060400           'SORTEN ÄR EJ GODKÄND'.                                        
060500         05  FILLER              PIC X(40)  VALUE                         
060600           'SORT CODE NOT CORRECT'.                                       
060700     03  FILLER REDEFINES MED-32.                                         
060800         05  MED32 OCCURS 2      PIC X(40).                               
060900                                                                          
061000     03  MED-33.                                                          
061100         05  FILLER              PIC X(40)  VALUE                         
061200           'ÄNDRING GJORD PÅ BEFINTLIG RAD'.                              
061300         05  FILLER              PIC X(40)  VALUE                         
061400           'UPDATE NOT ALLOWED'.                                          
061500     03  FILLER REDEFINES MED-33.                                         
061600         05  MED33 OCCURS 2      PIC X(40).                               
061700                                                                          
061800     03  MED-34.                                                          
061900         05  FILLER              PIC X(40)  VALUE                         
062000           'SORT MÅSTE RAPPORTERAS'.                                      
062100         05  FILLER              PIC X(40)  VALUE                         
062200           'GIVE A SORT-CODE'.                                            
062300     03  FILLER REDEFINES MED-34.                                         
062400         05  MED34 OCCURS 2      PIC X(40).                               
062500                                                                          
062600     03  MED-35.                                                          
062700         05  FILLER              PIC X(40)  VALUE                         
062800            'SORT SKALL VARA SA'.                                         
062900         05  FILLER              PIC X(40)  VALUE                         
063000            'SORT-CODE NOT CORRECT'.                                      
063100     03  FILLER REDEFINES MED-35.                                         
063200         05  MED35 OCCURS 2      PIC X(40).                               
063300                                                                          
063400     03  MED-36.                                                          
063500         05  FILLER              PIC X(40)  VALUE                         
063600             'SORT SKALL VARA ST'.                                        
063700         05  FILLER              PIC X(40)  VALUE                         
063800            'SORT-CODE NOT CORRECT'.                                      
063900     03  FILLER REDEFINES MED-36.                                         
064000         05  MED36 OCCURS 2      PIC X(40).                               
064100                                                                          
064200     03  MED-37.                                                          
064300         05  FILLER              PIC X(40)  VALUE                         
064400           'STRUKTURTYP SKALL VARA S'.                                    
064500         05  FILLER              PIC X(40)  VALUE                         
064600           'TYPE OF STRUCTURE NOT CORRECT'.                               
064700     03  FILLER REDEFINES MED-37.                                         
064800         05  MED37 OCCURS 2      PIC X(40).                               
064900                                                                          
065000     03  MED-38.                                                          
065100         05  FILLER              PIC X(60)  VALUE                         
065200           'STRUKTUREN KOMMER ATT INGÅ I SIG SJÄLV'.                      
065300         05  FILLER              PIC X(60)  VALUE                         
065400           'PART NUMBER NOT CORRECT'.                                     
065500     03  FILLER REDEFINES MED-38.                                         
065600         05  MED38 OCCURS 2      PIC X(60).                               
065700                                                                          
065800     03  MED-39.                                                          
065900         05  FILLER              PIC X(60)  VALUE                         
066000          'STRUKTURNUMRET FINNS INTE SOM SATS PÅ RASA'.                   
066100         05  FILLER              PIC X(60)  VALUE                         
066200          'PART NUMBER NOT CORRECT'.                                      
066300     03  FILLER REDEFINES MED-39.                                         
066400         05  MED39 OCCURS 2      PIC X(60).                               
066500                                                                          
066600     03  MED-40.                                                          
066700         05  FILLER              PIC X(60)  VALUE                         
066800           'ARTIKELN FINNS EJ SOM STRUKTUR'.                              
066900         05  FILLER              PIC X(60)  VALUE                         
067000           'PART NUMBER NOT CORRECT'.                                     
067100     03  FILLER REDEFINES MED-40.                                         
067200         05  MED40 OCCURS 2      PIC X(60).                               
067300                                                                          
067400     03  MED-41.                                                          
067500         05  FILLER              PIC X(60)  VALUE                         
067600            'BENÄMNING STÄMMER EJ ÖVERENS'.                               
067700         05  FILLER              PIC X(60)  VALUE                         
067800            'DESCRIPTION NOT CORRECT'.                                    
067900     03  FILLER REDEFINES MED-41.                                         
068000         05  MED41 OCCURS 2      PIC X(60).                               
068100                                                                          
068200     03  MED-42.                                                          
068300         05  FILLER              PIC X(40)  VALUE                         
068400           'HOMONYMKOD STÄMMER EJ ÖVERENS'.                               
068500         05  FILLER              PIC X(40)  VALUE                         
068600           'HOM.CODE NOT CORRECT'.                                        
068700     03  FILLER REDEFINES MED-42.                                         
068800         05  MED42 OCCURS 2      PIC X(40).                               
068900                                                                          
069000     03  MED-43.                                                          
069100         05  FILLER              PIC X(40)  VALUE                         
069200          'STRUKTURTYP STÄMMER EJ ÖVERENS'.                               
069300         05  FILLER              PIC X(40)  VALUE                         
069400          'TYPE OF STRUCTURE NOT CORRECT'.                                
069500     03  FILLER REDEFINES MED-43.                                         
069600         05  MED43 OCCURS 2      PIC X(40).                               
069700                                                                          
069800     03  MED-44.                                                          
069900         05  FILLER              PIC X(40)  VALUE                         
070000           'ANTAL MÅSTE RAPPORTERAS'.                                     
070100         05  FILLER              PIC X(40)  VALUE                         
070200           'GIVE A QUANTITY'.                                             
070300     03  FILLER REDEFINES MED-44.                                         
070400         05  MED44 OCCURS 2      PIC X(40).                               
070500                                                                          
070600     03  MED-45.                                                          
070700         05  FILLER              PIC X(40)  VALUE                         
070800           'ARTIKEL ERSÄTTNINGSMÄRKT'.                                    
070900         05  FILLER              PIC X(40)  VALUE                         
071000           'THIS PART IS SUPERSEDED'.                                     
071100     03  FILLER REDEFINES MED-45.                                         
071200         05  MED45 OCCURS 2      PIC X(40).                               
071300                                                                          
071400     03  MED-46.                                                          
071500         05  FILLER              PIC X(60)  VALUE                         
071600           'ARTIKEL MÅSTE FINNAS PÅ ARTIKELREGISTRET'.                    
071700         05  FILLER              PIC X(60)  VALUE                         
071800           'THIS PART IS NOT IN THE DATABASE'.                            
071900     03  FILLER REDEFINES MED-46.                                         
072000         05  MED46 OCCURS 2      PIC X(60).                               
072100                                                                          
072200     03  MED-47.                                                          
072300         05  FILLER              PIC X(40)  VALUE                         
072400         'SORTEN STÄMMER EJ ÖVERENS'.                                     
072500         05  FILLER              PIC X(40)  VALUE                         
072600          'SORT-CODE NOT CORRECT'.                                        
072700     03  FILLER REDEFINES MED-47.                                         
072800         05  MED47 OCCURS 2      PIC X(40).                               
072900                                                                          
073000     03  MED-48.                                                          
073100         05  FILLER              PIC X(60)  VALUE                         
073200         'STRUKTURNUMRET FINNS INTE I RASA'.                              
073300         05  FILLER              PIC X(60)  VALUE                         
073400          'PART NUMBER NOT CORRECT'.                                      
073500     03  FILLER REDEFINES MED-48.                                         
073600         05  MED48 OCCURS 2      PIC X(60).                               
073700                                                                          
073800     03  MED-49.                                                          
073900         05  FILLER              PIC X(60)  VALUE                         
074000          'BENÄMNINGEN HAR FLERA HOMONYMKODER'.                           
074100         05  FILLER              PIC X(60)  VALUE                         
074200          'GIVE A HOM.CODE'.                                              
074300     03  FILLER REDEFINES MED-49.                                         
074400         05  MED49 OCCURS 2      PIC X(60).                               
074500                                                                          
074600     03  MED-50.                                                          
074700         05  FILLER              PIC X(60)  VALUE                         
074800          'DEN INLAGDA STRUKTUREN INNEHÅLLER INGA RADER'.                 
074900         05  FILLER              PIC X(60)  VALUE                         
075000          'NO STRUCTURE LINES EXIST'.                                     
075100     03  FILLER REDEFINES MED-50.                                         
075200         05  MED50 OCCURS 2      PIC X(60).                               
075300     EJECT                                                                
075400 01  MESSAGE-CODES.                                                       
075500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
075600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
075700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
075800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
075900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
076000     EJECT                                                                
076100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
076200*                                                                         
076300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
076400     SKIP3                                                                
076500*01  MID -COPY W1I21201                                                   
076600     EJECT                                                                
076700*01  -COPY W1I21301    -PRE 1213-                                         
076800     EJECT                                                                
076900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
077000     SKIP3                                                                
077100*01  -COPY WMSGAREA                                                       
077200     EJECT                                                                
077300     03  MOD REDEFINES MSG-AREA.                                          
077400*      05  -COPY W1O21201                                                 
077500     EJECT                                                                
077600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
077700     SKIP3                                                                
077800*01  -COPY WMFSAREA                                                       
077900     EJECT                                                                
078000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
078100*                                                                         
078200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
078300     SKIP3                                                                
078400 01  NYCKLAR-TILL-DLI.                                                    
078500     03  W-STRNR-X.                                                       
078600         05  W-STRNR             PIC S9(9)   VALUE ZERO COMP-3.           
078700     03  W-STRNR-KONV-X.                                                  
078800         05  W-STRNR-KONV        PIC S9(9)   VALUE ZERO COMP-3.           
078900     03  W-STRNR-KONV-MIN-X.                                              
079000         05  W-STRNR-KONV-MIN   PIC S9(9) VALUE +100000000 COMP-3.        
079100     03  W-STRNR-KONV-MAX-X.                                              
079200         05  W-STRNR-KONV-MAX   PIC S9(9) VALUE +999999999 COMP-3.        
079300     03  W-IDARTNR-X.                                                     
079400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
079500     03  W-IDSKYLT-X.                                                     
079600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
079700     03  W-WDJ111KY-X.                                                    
079800         05  W-KDSTRRAD          PIC X       VALUE '0'.                   
079900         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
080000     03  W-WDJ111KY2-X.                                                   
080100         05  W-KDSTRRAD2         PIC X       VALUE '9'.                   
080200         05  W-IDRADNR2          PIC S9(5)   VALUE ZERO COMP-3.           
080300     03  W-IDLEVNR-X.                                                     
080400         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
080500     03  W-IDBENR-X.                                                      
080600         05  W-IDBENR            PIC X       VALUE SPACE.                 
080700     03  W-BELEVART-X.                                                    
080800         05  W-BELEVART          PIC X(30)   VALUE SPACE.                 
080900     03  W-BEART-X.                                                       
081000         05  W-BEART             PIC X(25)   VALUE SPACE.                 
081100     03  W-TIBEHOV-X.                                                     
081200         05  W-TIBEHOV           PIC S9(5)   VALUE ZERO COMP-3.           
081300     03  W-IDSTRNOT-X.                                                    
081400         05  W-IDSTRNOT          PIC X       VALUE '1'.                   
081500     03  W-WDGXKEY-X.                                                     
081600         05  W-IDHTYPX           PIC X(4)    VALUE SPACE.                 
081700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
081800     03  W-WDG3KEY-X.                                                     
081900         05  W-IDHTYP            PIC X(4)    VALUE SPACE.                 
082000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
082100     03  W-IDUSER-X.                                                      
082200         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
082300     03  W-WDF5ASEQ-X.                                                    
082400         05  W-IDLEVNR-A         PIC X(5)   VALUE SPACE.                  
082500         05  W-IDLEVART-A        PIC X(30)   VALUE SPACE.                 
082600     03  WS-MAX-STRNR-OKONV      PIC S9(9)   VALUE +99999999.             
082700*    --- STATUS-KOD FRÅN IMS                                              
082800 01  STATUS-WS                   PIC XX.                                  
082900     88  SEGMENT-FINNS                       VALUE '  '.                  
083000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
083100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
083200     SKIP2                                                                
083300 01  GODK-STATUSKODER.                                                    
083400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
083500     SKIP3                                                                
083600 01  SSA1                        PIC X(64).                               
083700 01  SSA2                        PIC X(64).                               
083800 01  SSA3                        PIC X(64).                               
083900     EJECT                                                                
084000*    --- IMS FUNKTIONSKODER                                               
084100*01  -COPY W0003                                                          
084200     EJECT                                                                
084300*    ---  DLI INPUT-OUTPUT AREA                                           
084400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
084500     SKIP3                                                                
084600 01  DLI-IO-AREA.                                                         
084700     03  IO-AREA                 PIC X(240)  VALUE SPACE.                 
084800     SKIP3                                                                
084900     03  WLBENA01 REDEFINES IO-AREA.                                      
085000*        05  -COPY WDD301     -PRE BENA-                                  
085100     EJECT                                                                
085200     03  WLBENA11 REDEFINES IO-AREA.                                      
085300*        05  -COPY WDD311     -PRE BENA-                                  
085400     SKIP3                                                                
085500     03  WDF501   REDEFINES IO-AREA.                                      
085600*        05  -COPY WDF501                                                 
085700     EJECT                                                                
085800     03  WDF502   REDEFINES IO-AREA.                                      
085900*        05  -COPY WDF502                                                 
086000     SKIP3                                                                
086100     03  WLSATB01 REDEFINES IO-AREA.                                      
086200*        05  -COPY WDJ101     -PRE SATB-                                  
086300     EJECT                                                                
086400     03  WLSATB11 REDEFINES IO-AREA.                                      
086500*        05  -COPY WDJ111     -PRE SATB-                                  
086600     EJECT                                                                
086700     03  WLSATB22 REDEFINES IO-AREA.                                      
086800*        05  -COPY WDJ122     -PRE SATB-                                  
086900     EJECT                                                                
087000 01  DLI-IO-AREA-K.                                                       
087100     03  IO-AREA-K               PIC X(240)  VALUE SPACE.                 
087200     SKIP3                                                                
087300     03  WLSATB01 REDEFINES IO-AREA-K.                                    
087400*        05  -COPY WDJ101     -PRE KONV-                                  
087500     EJECT                                                                
087600     03  WLSATB11 REDEFINES IO-AREA-K.                                    
087700*        05  -COPY WDJ111     -PRE KONV-                                  
087800     EJECT                                                                
087900     03  WLSATB22 REDEFINES IO-AREA-K.                                    
088000*        05  -COPY WDJ122     -PRE KONV-                                  
088100     EJECT                                                                
088200 01  DLI-IO-AREA-XX.                                                      
088300     03  IO-AREA-XX              PIC X(96)   VALUE SPACE.                 
088400     SKIP3                                                                
088500     03  WLXXBY01 REDEFINES IO-AREA-XX.                                   
088600*        05  -COPY WDGX2234                                               
088700     SKIP3                                                                
088800     03  WLXXAZ11 REDEFINES IO-AREA-XX.                                   
088900*        05  -COPY WDGX1152   -PRE XXAZ-                                  
089000     EJECT                                                                
089100 01  DLI-IO-AREA-ARTC.                                                    
089200     03  IO-AREA-ARTC            PIC X(928)  VALUE SPACE.                 
089300     SKIP3                                                                
089400     03  WLARTC01 REDEFINES IO-AREA-ARTC.                                 
089500*        05  -COPY WDK601                                                 
089600     SKIP3                                                                
089700     03  WLARTC11 REDEFINES IO-AREA-ARTC.                                 
089800*        05  -COPY WDK611                                                 
089900     EJECT                                                                
090000 01  DLI-IO-AREA-CSEQ.                                                    
090100     03  IO-AREA-CSEQ            PIC X(352)  VALUE SPACE.                 
090200     SKIP3                                                                
090300     03  WLSATB-CSEQ REDEFINES IO-AREA-CSEQ.                              
090400         05  WLSATB11.                                                    
090500*          07  -COPY WDJ111     -PRE CSEQ-                                
090600     SKIP3                                                                
090700         05  WLSATB01.                                                    
090800*          07  -COPY WDJ101     -PRE CSEQ-                                
090900     EJECT                                                                
091000 LINKAGE SECTION.                                                         
091100                                                                          
091200*01  -COPY W0009      -PRE MSG-                                           
091300     EJECT                                                                
091400*01  -COPY W0008 -PRE USEA-                                               
091500     05  FILLER                  PIC X.                                   
091600     EJECT                                                                
091700*01  -COPY W0008      -PRE BENA-A-                                        
091800     05  FILLER                  PIC X.                                   
091900     EJECT                                                                
092000*01  -COPY W0008 -PRE BENA-B-                                             
092100     05  FILLER                  PIC X.                                   
092200     EJECT                                                                
092300*01  -COPY W0008 -PRE ARTC-                                               
092400     05  FILLER                  PIC X.                                   
092500     EJECT                                                                
092600*01  -COPY W0008 -PRE WDF5-                                               
092700     05  FILLER                  PIC X.                                   
092800     EJECT                                                                
092900*01  -COPY W0008 -PRE WDF5-A-                                             
093000     05  FILLER                  PIC X.                                   
093100     EJECT                                                                
093200*01  -COPY W0008 -PRE SATB-                                               
093300     05  FILLER                  PIC X.                                   
093400     EJECT                                                                
093500*01  -COPY W0008 -PRE SATB-K-                                             
093600     05  FILLER                  PIC X.                                   
093700     EJECT                                                                
093800*01  -COPY W0008 -PRE SATB-C-                                             
093900     05  FILLER                  PIC X.                                   
094000     EJECT                                                                
094100*01  -COPY W0008 -PRE SATB-D-                                             
094200     05  FILLER                  PIC X.                                   
094300     EJECT                                                                
094400*01  -COPY W0008 -PRE XXAZ-                                               
094500     05  FILLER                  PIC X.                                   
094600     EJECT                                                                
094700*01  -COPY W0008 -PRE XXBY-                                               
094800     05  FILLER                  PIC X.                                   
094900     EJECT                                                                
095000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
095100                                   BENA-A-PCB BENA-B-PCB                  
095200                           ARTC-PCB WDF5-PCB WDF5-A-PCB SATB-PCB          
095300                           SATB-K-PCB SATB-C-PCB SATB-D-PCB               
095400                           XXAZ-PCB XXBY-PCB.                             
095500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
095600                                   BENA-A-PCB BENA-B-PCB                  
095700                           ARTC-PCB WDF5-PCB WDF5-A-PCB SATB-PCB          
095800                           SATB-K-PCB SATB-C-PCB SATB-D-PCB               
095900                           XXAZ-PCB XXBY-PCB.                             
096000                                                                          
096100     PERFORM IMS-GET-MSG                                                  
096200     IF SEGMENT-FINNS                                                     
096300       PERFORM A-INIT                                                     
096400       PERFORM B-KOLLA-NYCKLAR                                            
096500       IF NYCKLAR-OK                                                      
096600         PERFORM S01-KOLLA-STRNR                                          
096700         IF MFS-UPDATE                                                    
096800           PERFORM H-KOLLA-INPUT                                          
096900           IF INDATA-OK                                                   
097000             PERFORM I-UPPDATERA                                          
097100           END-IF                                                         
097200         ELSE                                                             
097300           IF MFS-FIRST AND 1213-MID                                      
097400             PERFORM C-VISA-RAD                                           
097500           ELSE                                                           
097600             IF MFS-FIRST                                                 
097700               PERFORM D-FOERSTA-RADEN                                    
097800             ELSE                                                         
097900               IF MFS-NEXT                                                
098000                 PERFORM E-NAESTA-RAD                                     
098100               ELSE                                                       
098200                 IF MID-IDRADNR-IN = ALL '+'                              
098300                   PERFORM F-SAMMA-RAD                                    
098400                 END-IF                                                   
098500               END-IF                                                     
098600             END-IF                                                       
098700           END-IF                                                         
098800           IF ALLT-OK                                                     
098900             PERFORM G-LAES-VISA-INFO                                     
099000           END-IF                                                         
099100         END-IF                                                           
099200       END-IF                                                             
099300       MOVE LENGTH OF MOD-W1O21201 TO MSG-KVLL                            
099400       ADD  +4                     TO MSG-KVLL                            
099500       PERFORM IMS-INSERT-MSG                                             
099600     END-IF                                                               
099700                                                                          
099800     MOVE ZERO TO RETURN-CODE                                             
099900     GOBACK                                                               
100000     .                                                                    
100100     EJECT                                                                
100200 A-INIT SECTION.                                                          
100300                                                                          
100400     IF MSG-DUBBLA-TRANSKODER                                             
100500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I21201                 
100600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
100700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
100800     ELSE                                                                 
100900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I21201                  
101000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
101100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
101200     END-IF                                                               
101300                                                                          
101400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
101500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
101600     MOVE MFS-IDTRANS TO IDTRANS-SW                                       
101700                                                                          
101800     MOVE LOW-VALUE TO MSG-AREA                                           
101900     MOVE 'W1O212N1' TO MFS-IDMOD                                         
102000     MOVE '1212' TO MOD-IDTRANS                                           
102100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
102200                                                                          
102300     IF NOT EGEN-MID                                                      
102400       MOVE SPACE TO MFS-KDTRTYP                                          
102500       MOVE '7' TO MFS-IDPFK                                              
102600     END-IF                                                               
102700                                                                          
102800     IF MFS-IDTRANS = '1213'                                              
102900       PERFORM AA-BEHANDLA-1213-TRANS                                     
103000     END-IF                                                               
103100                                                                          
103200     ACCEPT DAGENS-DATUM FROM DATE                                        
103300     MOVE DAGENS-DATUM    TO DAT-I-TIDATUM                                
103400     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
103500     CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                     
103600                          DAT-O-TIDATUM DAT-KDSVAR                        
103700     MOVE DAT-TIAAVV-GRP  TO WS-TIAAVV                                    
103800     MOVE WS-TIAAVV       TO DAT-I-TIDATUM                                
103900     MOVE 'AAVV  '        TO DAT-KDDATFORM                                
104000     CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                     
104100                          DAT-O-TIDATUM DAT-KDSVAR                        
104200     MOVE DAT-TIAAMMDD    TO INNEVARANDE-VECKA                            
104300     .                                                                    
104400     EJECT                                                                
104500******************************************************************        
104600**   FLYTTA ÖVER INFO. FRÅN DEN VALDA RADEN PÅ BILD 1213 TILL MID.        
104700******************************************************************        
104800                                                                          
104900 AA-BEHANDLA-1213-TRANS SECTION.                                          
105000                                                                          
105100     MOVE MID-W1I21201 TO 1213-MID-W1I21301                               
105200                                                                          
105300     MOVE +1 TO RADIND                                                    
105400     PERFORM UNTIL RADIND > RADIND-MAX                                    
105500       IF 1213-MID-SELECT (RADIND) = ALL '+'                              
105600         ADD +1    TO RADIND                                              
105700       ELSE                                                               
105800         MOVE 1213-MID-IDRADNR (RADIND)  TO MID-IDRADNR-IN                
105900         MOVE +14  TO RADIND                                              
106000       END-IF                                                             
106100     END-PERFORM                                                          
106200                                                                          
106300     .                                                                    
106400     EJECT                                                                
106500 B-KOLLA-NYCKLAR SECTION.                                                 
106600                                                                          
106700     MOVE JA TO NYCKLAR-SW                                                
106800                                                                          
106900***  -- KONTROLL AV STRNR                                                 
107000     MOVE MFS-RENSA-FAELT TO MOD-STRNR-IN                                 
107100                                                                          
107200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
107300     MOVE '001'             TO MSGI-KDCALL                                
107400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
107500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
107600     MOVE '1212'            TO MSGI-IDTRANS                               
107700                                                                          
107800     IF MFS-IDTRANS = '1212'                                              
107900     OR (MID-STRNR-IN NUMERIC                                             
108000     AND MID-STRNR-IN > ZERO)                                             
108100        MOVE MID-STRNR-IN TO MSGI-IDARTNR                                 
108200     END-IF                                                               
108300                                                                          
108400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
108500                                                                          
108600     IF MSGI-IDLAND-SPR = 'GB'                                            
108700       MOVE +2 TO SPRAK-IX                                                
108800       MOVE 'GB ' TO MED-IDSKYLT                                          
108900     ELSE                                                                 
109000       MOVE +1 TO SPRAK-IX                                                
109100       MOVE 'S  ' TO MED-IDSKYLT                                          
109200     END-IF                                                               
109300                                                                          
109400                                                                          
109500     MOVE MSGI-IDARTNR   TO WS-STRNR                                      
109600     INSPECT WS-STRNR REPLACING LEADING SPACE BY ZERO                     
109700                                                                          
109800     IF MID-STRNR-IN = ALL '+'                                            
109900       CONTINUE                                                           
110000     ELSE                                                                 
110100       MOVE SPACE        TO MFS-KDTRTYP                                   
110200       MOVE '7'          TO MFS-IDPFK                                     
110300     END-IF                                                               
110400                                                                          
110500     IF (WS-STRNR NUMERIC) AND (WS-STRNR > ZERO) AND                      
110600         (WS-STRNR < 10000000)                                            
110700       MOVE WS-STRNR TO W-STRNR                                           
110800     ELSE                                                                 
110900       MOVE '401'          TO MED-IDMFSFEL                                
111000       CALL WMEDKONV USING MED-WMEDAREA                                   
111100       MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                
111200       MOVE NEJ TO NYCKLAR-SW                                             
111300     END-IF                                                               
111400                                                                          
111500     IF GODK-MID OR NYCKLAR-OK                                            
111600       MOVE WS-STRNR TO MOD-STRNR-UT                                      
111700       INSPECT MOD-STRNR-UT REPLACING LEADING ZERO BY SPACE               
111800     ELSE                                                                 
111900       MOVE MFS-RENSA-FAELT TO MOD-STRNR-UT                               
112000     END-IF                                                               
112100                                                                          
112200***  -- KONTROLL AV IDSKYLT                                               
112300     MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-IN                               
112400                                                                          
112500     IF GODK-MID-MED-IDSKYLT                                              
112600       IF MID-IDSKYLT-IN = ALL '+'                                        
112700         IF MID-IDSKYLT-UT = SPACE                                        
112800           IF MSGI-IDLAND-SPR = 'GB'                                      
112900             MOVE 'GB' TO WS-IDSKYLT                                      
113000           ELSE                                                           
113100             MOVE 'S'  TO WS-IDSKYLT                                      
113200           END-IF                                                         
113300         ELSE                                                             
113400           MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                              
113500         END-IF                                                           
113600       ELSE                                                               
113700         MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                
113800         MOVE SPACE          TO MFS-KDTRTYP                               
113900       END-IF                                                             
114000     ELSE                                                                 
114100       IF MSGI-IDLAND-SPR = 'GB'                                          
114200         MOVE 'GB' TO WS-IDSKYLT                                          
114300       ELSE                                                               
114400         MOVE 'S'  TO WS-IDSKYLT                                          
114500       END-IF                                                             
114600     END-IF                                                               
114700                                                                          
114800     SET WWLAND03-IX TO +1                                                
114900     SEARCH WWLAND03-IDSKYLT-RAD                                          
115000       AT END                                                             
115100         MOVE '401'        TO MED-IDMFSFEL                                
115200         CALL WMEDKONV USING MED-WMEDAREA                                 
115300         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
115400         MOVE NEJ   TO NYCKLAR-SW                                         
115500       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                    
115600         CONTINUE                                                         
115700     END-SEARCH                                                           
115800                                                                          
115900     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
116000                                                                          
116100     IF GODK-MID OR NYCKLAR-OK                                            
116200       MOVE WS-IDSKYLT TO MOD-IDSKYLT-UT                                  
116300     ELSE                                                                 
116400       MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-UT                             
116500     END-IF                                                               
116600                                                                          
116700***  -- KONTROLL AV RADNR                                                 
116800     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-IN                               
116900                                                                          
117000     IF GODK-MID-MED-IDRADNR                                              
117100       IF MID-IDRADNR-IN = ALL '+'                                        
117200         IF MID-STRNR-IN = ALL '+'                                        
117300           MOVE MID-IDRADNR-UT TO WS-IDRADNR                              
117400           INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO             
117500         ELSE                                                             
117600           MOVE '00010'      TO WS-IDRADNR                                
117700         END-IF                                                           
117800       ELSE                                                               
117900         MOVE MID-IDRADNR-IN TO WS-IDRADNR                                
118000         INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO               
118100         MOVE SPACE      TO MFS-KDTRTYP                                   
118200       END-IF                                                             
118300     ELSE                                                                 
118400       MOVE '00010' TO WS-IDRADNR                                         
118500       MOVE '++++'  TO MID-IDRADNR-IN                                     
118600     END-IF                                                               
118700                                                                          
118800     IF WS-IDRADNR NUMERIC AND WS-IDRADNR NOT < ZERO                      
118900       MOVE WS-IDRADNR     TO W-IDRADNR                                   
119000     ELSE                                                                 
119100       MOVE '401'          TO MED-IDMFSFEL                                
119200       CALL WMEDKONV USING MED-WMEDAREA                                   
119300       MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                
119400       MOVE NEJ TO NYCKLAR-SW                                             
119500     END-IF                                                               
119600                                                                          
119700     IF GODK-MID OR NYCKLAR-OK                                            
119800       MOVE W-IDRADNR   TO WS-RADNR4                                      
119900       MOVE WS-RADNR4   TO MOD-IDRADNR-UT                                 
120000                           MOD-IDRADNR-B                                  
120100       INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE             
120200     ELSE                                                                 
120300       MOVE MFS-RENSA-FAELT TO MOD-STRNR-UT                               
120400       MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-UT                             
120500       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-UT                             
120600       MOVE FEL1(SPRAK-IX)  TO MOD-TEMFSFEL                               
120700     END-IF                                                               
120800                                                                          
120900                                                                          
121000     IF NYCKLAR-FEL                                                       
121100       PERFORM MFS-RENSA-FAELT-IN                                         
121200       PERFORM MFS-RENSA-FAELT-UT                                         
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121600******************************************************************        
121700**   VISA RADEN SOM VALDES PÅ BILD 1213.                                  
121800******************************************************************        
121900                                                                          
122000 C-VISA-RAD SECTION.                                                      
122100                                                                          
122200     MOVE JA TO ALLT-SW                                                   
122300     .                                                                    
122400     EJECT                                                                
122500******************************************************************        
122600**   VISA FÖRSTA RADEN I STRUKTUREN.                                      
122700******************************************************************        
122800                                                                          
122900 D-FOERSTA-RADEN SECTION.                                                 
123000                                                                          
123100     IF MID-IDRADNR-IN = ALL '+'                                          
123200*****  OM RADNR EJ IFYLLD LÄS FRÅN BÖRJAN                                 
123300       MOVE ZERO           TO W-IDRADNR                                   
123400     END-IF                                                               
123500     MOVE '006'            TO MED-IDMFSFEL                                
123600     CALL WMEDKONV USING MED-WMEDAREA                                     
123700     MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                                
123800                                                                          
123900     MOVE JA               TO ALLT-SW                                     
124000     .                                                                    
124100     EJECT                                                                
124200******************************************************************        
124300**   VISA NÄSTA RAD I STRUKTUREN.                                         
124400******************************************************************        
124500                                                                          
124600 E-NAESTA-RAD SECTION.                                                    
124700                                                                          
124800     MOVE MID-IDRADNR-B TO W-IDRADNR                                      
124900     ADD +1             TO W-IDRADNR                                      
125000     MOVE JA            TO ALLT-SW                                        
125100     .                                                                    
125200     EJECT                                                                
125300******************************************************************        
125400**   VISA SAMMA RAD SOM MAN ÄR PÅ.                                        
125500******************************************************************        
125600                                                                          
125700 F-SAMMA-RAD SECTION.                                                     
125800     IF MID-INPUT = ALL '+'                                               
125900       MOVE JA             TO ALLT-SW                                     
126000     ELSE                                                                 
126100       MOVE NEJ            TO ALLT-SW                                     
126200       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
126300       CALL WMEDKONV USING MED-WMEDAREA                                   
126400       MOVE MED-TEMFSINF   TO MOD-TEMFSINF                                
126500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
126600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
126700       PERFORM MFS-LAS-IN-IGEN                                            
126800     END-IF                                                               
126900     .                                                                    
127000     EJECT                                                                
127100******************************************************************        
127200**   VISA VALD RAD. FINNS RADENS ARTIKELNUMMER PÅ ARTIKELREGISTRET        
127300**   HÄMTAS VÄRDENA FRÅN ARTC, WDF5, BENA OCH RASA ANNARS                 
127400**   HÄMTAS DE BARA FRÅN RASA.                                            
127500******************************************************************        
127600                                                                          
127700 G-LAES-VISA-INFO SECTION.                                                
127800                                                                          
127900     PERFORM GA-LAES-GRUNDDATA                                            
128000                                                                          
128100     IF (STRNR-FINNS OR KONV-FINNS) AND                                   
128200      RADNR-FINNS                                                         
128300       PERFORM IMS-GET-ARTC-01                                            
128400                                                                          
128500       IF SEGMENT-FINNS                                                   
128600**   RADENS ARTIKEL FINNS PÅ ARTIKELREGISTRET                             
128700                                                                          
128800           MOVE ART-IDLEVNR TO WS-IDLEVNR                                 
128900           MOVE ART-KDSORT  TO MOD-KDSORT-UT                              
129000                                                                          
129100           IF ART-KDERS-UTG > 0                                           
129200              MOVE FEL14(SPRAK-IX) TO MOD-TEMFSINF                        
129300           END-IF                                                         
129400                                                                          
129500         PERFORM IMS-GET-BENA-BSEQ                                        
129600         IF SEGMENT-FINNS                                                 
129700           MOVE BENA-BEN-KDHOMONYM TO MOD-KDBENHOM-UT                     
129800           PERFORM IMS-GET-BENA-B-TEXT                                    
129900           IF SEGMENT-FINNS                                               
130000             MOVE BENA-TEXT-BEART TO MOD-BEART-UT                         
130100           ELSE                                                           
130200             MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                         
130300           END-IF                                                         
130400         ELSE                                                             
130500           MOVE MED3(SPRAK-IX) TO MOD-TEMFSINF                            
130600           MOVE MFS-RENSA-FAELT TO MOD-KDBENHOM-UT                        
130700           MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                           
130800         END-IF                                                           
130900                                                                          
131000         MOVE WS-IDLEVNR TO W-IDLEVNR                                     
131100         PERFORM IMS-GET-WDF501                                           
131200         IF SEGMENT-FINNS                                                 
131300           IF WS-IDLEVNR NOT = SPACE                                      
131400             MOVE WS-IDLEVNR TO W-IDLEVNR                                 
131500                                MOD-IDLEVNR                               
131600             PERFORM IMS-GET-WDF502-LAST                                  
131700             IF SEGMENT-FINNS                                             
131800               MOVE XLEV-BELEVART  TO MOD-BELEVART                        
131900             ELSE                                                         
132000               MOVE MFS-RENSA-FAELT TO MOD-BELEVART                       
132100             END-IF                                                       
132200           ELSE                                                           
132300             PERFORM IMS-GET-WDF502-OKVAL                                 
132400             IF SEGMENT-FINNS                                             
132500               MOVE XLEV-IDLEVNR   TO W-IDLEVNR                           
132600                                      MOD-IDLEVNR                         
132700               MOVE XLEV-BELEVART  TO MOD-BELEVART                        
132800               PERFORM IMS-GET-WDF502-LAST                                
132900               IF SEGMENT-FINNS                                           
133000                 MOVE XLEV-BELEVART TO MOD-BELEVART                       
133100               END-IF                                                     
133200             END-IF                                                       
133300           END-IF                                                         
133400         ELSE                                                             
133500           MOVE MFS-RENSA-FAELT TO MOD-BELEVART                           
133600         END-IF                                                           
133700       ELSE                                                               
133800**   RADENS ARTIKEL FINNS BARA PÅ RASA                                    
133900                                                                          
134000         IF WS-RAD-IDARTNR NOT = ZERO                                     
134100           MOVE WS-RAD-IDARTNR TO MOD-IDARTNR-UT                          
134200           INSPECT MOD-IDARTNR-UT                                         
134300                   REPLACING LEADING ZERO BY SPACE                        
134400         ELSE                                                             
134500           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                         
134600         END-IF                                                           
134700                                                                          
134800         IF WS-RAD-IDLEVNR NOT = SPACE                                    
134900           MOVE WS-RAD-IDLEVNR  TO MOD-IDLEVNR-UT                         
135000           MOVE WS-RAD-BELEVART TO MOD-BELEVART-UT                        
135100         ELSE                                                             
135200           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                         
135300           MOVE MFS-RENSA-FAELT TO MOD-BELEVART-UT                        
135400         END-IF                                                           
135500                                                                          
135600         MOVE WS-RAD-KDBENHOM TO MOD-KDBENHOM-UT                          
135700                                 WS-KDBENHOM-NUM                          
135800                                                                          
135900         IF WS-IDSKYLT = 'S  '                                            
136000           MOVE WS-RAD-BEART-SVE   TO MOD-BEART-UT                        
136100         ELSE                                                             
136200******     HÄMTA UTLÄNDSK BENÄMNING                                       
136300           MOVE 'S  ' TO W-IDSKYLT                                        
136400           MOVE WS-RAD-BEART-SVE TO W-BEART                               
136500           PERFORM IMS-GET-BENA-ASEQ                                      
136600           PERFORM UNTIL SEGMENT-SAKNAS OR                                
136700                         BENA-BEN-KDHOMONYM = WS-RAD-KDBENHOM             
136800             IF SEGMENT-FINNS                                             
136900               IF BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM                    
137000                 CONTINUE                                                 
137100               ELSE                                                       
137200                 PERFORM IMS-GN-BENA-ASEQ                                 
137300               END-IF                                                     
137400             ELSE                                                         
137500               MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                       
137600             END-IF                                                       
137700           END-PERFORM                                                    
137800                                                                          
137900           IF SEGMENT-FINNS                                               
138000             MOVE WS-IDSKYLT TO W-IDSKYLT                                 
138100             PERFORM IMS-GET-BENA-A-TEXT                                  
138200             IF SEGMENT-FINNS                                             
138300               MOVE BENA-TEXT-BEART TO MOD-BEART-UT                       
138400             ELSE                                                         
138500               MOVE MFS-RENSA-FAELT TO MOD-BEART-UT                       
138600             END-IF                                                       
138700           END-IF                                                         
138800         END-IF                                                           
138900                                                                          
139000         MOVE WS-RAD-KDBENHOM  TO MOD-KDBENHOM-UT                         
139100         MOVE WS-RAD-KDSORT    TO MOD-KDSORT-UT                           
139200       END-IF                                                             
139300       MOVE WS-RAD-TIREGDAT TO MOD-TIREGDAT                               
139400       MOVE WS-RAD-IDSTRTYP TO MOD-IDSTRTYP-UT                            
139500       MOVE WS-RAD-REANTPSA TO MOD-REANTPSA-UT                            
139600                                                                          
139700       PERFORM IMS-GNP-SATB-NOT                                           
139800       IF SEGMENT-FINNS                                                   
139900         MOVE SATB-NOT-TESTRNOT(1) TO MOD-TESTRNOT-IN(1)                  
140000         MOVE SATB-NOT-TESTRNOT(2) TO MOD-TESTRNOT-IN(2)                  
140100       ELSE                                                               
140200         MOVE MFS-RENSA-FAELT      TO MOD-TESTRNOT-IN(1)                  
140300         MOVE MFS-RENSA-FAELT      TO MOD-TESTRNOT-IN(2)                  
140400       END-IF                                                             
140500     END-IF                                                               
140600                                                                          
140700     PERFORM MFS-RENSA-FAELT-IN                                           
140800     IF STRUKTURNR-SPAERRAT OR STRUKTURNR-BORTTAGET                       
140900       IF STRUKTURNR-SPAERRAT                                             
141000         MOVE FEL2(SPRAK-IX) TO MOD-TEMFSFEL                              
141100       ELSE                                                               
141200         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSFEL                              
141300       END-IF                                                             
141400     END-IF                                                               
141500     .                                                                    
141600     EJECT                                                                
141700******************************************************************        
141800**** HÄMTAR VÄRDEN FRÅN ROTEN OCH LÄSER SEDAN VALD RAD OCH LAGRAR         
141900**** UNDAN DEN I WS.                                                      
142000******************************************************************        
142100                                                                          
142200 GA-LAES-GRUNDDATA SECTION.                                               
142300                                                                          
142400     IF STRNR-FINNS OR KONV-FINNS                                         
142500       IF KONV-FINNS                                                      
142600         PERFORM IMS-GHU-SATB-STR-K-PCB                                   
142700       ELSE                                                               
142800         PERFORM IMS-GU-SATB-STR                                          
142900       END-IF                                                             
143000       MOVE SATB-STR-TIREGDAT    TO MOD-TIREGDAT                          
143100       MOVE SATB-STR-IDSTRTYP    TO MOD-IDSTRTYP-UT                       
143200       IF MID-IDRADNR-IN = ALL '+'                                        
143300         PERFORM IMS-GNP-SATB-RAD                                         
143400       ELSE                                                               
143500         PERFORM IMS-GU-SATB-RAD                                          
143600       END-IF                                                             
143700       IF SEGMENT-FINNS AND SATB-RAD-KDSTRRAD NOT = '9'                   
143800         MOVE JA TO RADNR-SW                                              
143900         MOVE SATB-RAD-WDJ111  TO WS-LAGRA-RAD                            
144000         MOVE SATB-RAD-IDARTNR TO W-IDARTNR                               
144100                                  MOD-IDARTNR-UT                          
144200         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
144300         MOVE SATB-RAD-IDRADNR TO W-IDRADNR                               
144400         MOVE SATB-RAD-IDRADNR TO WS-RADNR4                               
144500         MOVE WS-RADNR4        TO MOD-IDRADNR-UT                          
144600                                  MOD-IDRADNR-B                           
144700*      GÖRS VID NYCKELKOLLEN OCKSÅ MEN MÅSTE GÖRAS HÄR OM RADNR           
144800*      EJ ÄR IFYLLT ELLER VID BLÄDDRING                                   
144900         INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE           
145000       ELSE                                                               
145100         IF SATB-RAD-KDSTRRAD = '9'                                       
145200           MOVE FEL4(SPRAK-IX) TO MOD-TEMFSFEL                            
145300           PERFORM MFS-RENSA-FAELT-UT                                     
145400           MOVE SATB-RAD-IDRADNR TO WS-RADNR4                             
145500           MOVE WS-RADNR4        TO MOD-IDRADNR-UT                        
145600                                    MOD-IDRADNR-B                         
145700           INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE         
145800         ELSE                                                             
145900           IF MFS-NEXT                                                    
146000             MOVE FEL5(SPRAK-IX) TO MOD-TEMFSFEL                          
146100             PERFORM MFS-ROER-EJ-FAELT-UT                                 
146200           ELSE                                                           
146300             IF W-IDRADNR = ZERO                                          
146400               MOVE FEL6(SPRAK-IX) TO MOD-TEMFSFEL                        
146500               PERFORM MFS-RENSA-FAELT-UT                                 
146600             ELSE                                                         
146700               MOVE W-IDRADNR TO W-IDRADNR2                               
146800               PERFORM IMS-GU-SATB-FORP-RAD                               
146900               IF SEGMENT-FINNS                                           
147000                 MOVE FEL4(SPRAK-IX) TO MOD-TEMFSFEL                      
147100                 PERFORM MFS-RENSA-FAELT-UT                               
147200               ELSE                                                       
147300                 MOVE FEL8(SPRAK-IX) TO MOD-TEMFSFEL                      
147400                 PERFORM MFS-RENSA-FAELT-UT                               
147500               END-IF                                                     
147600             END-IF                                                       
147700           END-IF                                                         
147800         END-IF                                                           
147900       END-IF                                                             
148000     ELSE                                                                 
148100       MOVE FEL9(SPRAK-IX) TO MOD-TEMFSFEL                                
148200       PERFORM MFS-RENSA-FAELT-UT                                         
148300     END-IF                                                               
148400     .                                                                    
148500     EJECT                                                                
148600******************************************************************        
148700**** KONTROLLERA INRAPPORTERADE FÄLT.                                     
148800******************************************************************        
148900                                                                          
149000 H-KOLLA-INPUT SECTION.                                                   
149100                                                                          
149200     MOVE JA TO INDATA-SW                                                 
149300     IF STRUKTURNR-SPAERRAT OR STRUKTURNR-BORTTAGET                       
149400       MOVE NEJ TO INDATA-SW                                              
149500       MOVE FEL10(SPRAK-IX) TO MOD-TEMFSFEL                               
149600       IF STRUKTURNR-SPAERRAT                                             
149700         MOVE FEL11(SPRAK-IX) TO MOD-TEMFSFEL                             
149800       ELSE                                                               
149900         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSFEL                              
150000       END-IF                                                             
150100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
150200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
150300     ELSE                                                                 
150400       IF MID-INPUT = ALL '+'                                             
150500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
150600         CALL WMEDKONV USING MED-WMEDAREA                                 
150700         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
150800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
150900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
151000         MOVE NEJ TO INDATA-SW                                            
151100       ELSE                                                               
151200         PERFORM HA-KOLLA-KLAR                                            
151300         IF INDATA-OK                                                     
151400           IF STRNR-FINNS OR KONV-FINNS                                   
151500             IF KONV-FINNS                                                
151600               PERFORM IMS-GHU-SATB-STR-K-PCB                             
151700             ELSE                                                         
151800               PERFORM IMS-GU-SATB-STR                                    
151900             END-IF                                                       
152000             IF INDATA-OK                                                 
152100               PERFORM HH-KOLLA-RAD                                       
152200               IF INDATA-OK                                               
152300                 IF MID-IDRADNR-K NOT = ALL '+'                           
152400                   PERFORM HB-KOLLA-KOPIERING                             
152500                   PERFORM S07-KOLLA-AO-AAVV                              
152600                 ELSE                                                     
152700                   IF MID-IDRADNR-F NOT = ALL '+'                         
152800                     PERFORM HC-KOLLA-FLYTTNING                           
152900                   ELSE                                                   
153000                     IF MID-BORT NOT = ALL '+'                            
153100                       PERFORM HD-KOLLA-BORTTAG                           
153200                     ELSE                                                 
153300                       IF MID-UPPDAT  = ALL '+' AND                       
153400                        MID-TESTRNOT(1) = ALL '+' AND                     
153500                        MID-TESTRNOT(2) = ALL '+' AND                     
153600                        (MID-KLAR = 'J' OR 'N' OR 'Y')                    
153700                         PERFORM MFS-ROER-EJ-FAELT-UT                     
153800                         MOVE MFS-ADD-LAES-IN-FAELT                       
153900                                       TO MOD-TESTRNOT-IN-ATTR(1)         
154000                         MOVE MFS-ADD-LAES-IN-FAELT                       
154100                                       TO MOD-TESTRNOT-IN-ATTR(2)         
154200                       ELSE                                               
154300                         PERFORM HE-KOLLA-UPPDAT-FAELT                    
154400                                                                          
154500                         IF RADNR-FINNS                                   
154600                           PERFORM HF-KOLLA-UPPDAT-RAD                    
154700                         ELSE                                             
154800                           PERFORM HG-KOLLA-NY-RAD                        
154900                         END-IF                                           
155000                       END-IF                                             
155100                     END-IF                                               
155200                   END-IF                                                 
155300                 END-IF                                                   
155400                                                                          
155500******           ÄO KRÄVS VID VISSA UPPDATERINGAR                         
155600******           NÄR DET ÄR EN GAMMAL STRUKTUR.                           
155700                 IF WS-TIUPPDAT-GAM NOT = ZERO                            
155800                   IF MID-IDAO = ALL '+' OR SPACE                         
155900                     IF AO-SKALL-FINNAS                                   
156000                       IF EJ-TID-SIGNAL                                   
156100                         MOVE MED4(SPRAK-IX) TO MOD-TEMFSINF              
156200                       END-IF                                             
156300                       MOVE MFS-ALFA-FAELT-FEL TO                         
156400                                               MOD-IDAO-IN-ATTR           
156500                       MOVE MFS-NUM-FAELT-FEL TO                          
156600                                              MOD-TIAAVV-IN-ATTR          
156700                       MOVE NEJ TO INDATA-SW                              
156800                     END-IF                                               
156900                   END-IF                                                 
157000                 END-IF                                                   
157100                                                                          
157200                 IF INDATA-FEL                                            
157300                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
157400                   CALL WMEDKONV USING MED-WMEDAREA                       
157500                   MOVE MFS-ADD-LAES-IN-FAELT                             
157600                                       TO MOD-TESTRNOT-IN-ATTR(1)         
157700                   MOVE MFS-ADD-LAES-IN-FAELT                             
157800                                       TO MOD-TESTRNOT-IN-ATTR(2)         
157900                   PERFORM MFS-ROER-EJ-FAELT-UT                           
158000                   PERFORM MFS-ROER-EJ-FAELT-IN                           
158100                 ELSE                                                     
158200                   IF KONV-SAKNAS                                         
158300                     PERFORM S02-KONV-STRUKTUR                            
158400                   END-IF                                                 
158500                 END-IF                                                   
158600               END-IF                                                     
158700             ELSE                                                         
158800               MOVE MFS-ADD-LAES-IN-FAELT                                 
158900                                    TO MOD-TESTRNOT-IN-ATTR(1)            
159000               MOVE MFS-ADD-LAES-IN-FAELT                                 
159100                                    TO MOD-TESTRNOT-IN-ATTR(2)            
159200               PERFORM MFS-ROER-EJ-FAELT-UT                               
159300               PERFORM MFS-ROER-EJ-FAELT-IN                               
159400             END-IF                                                       
159500           ELSE                                                           
159600             MOVE FEL13(SPRAK-IX) TO MOD-TEMFSFEL                         
159700             MOVE FEL9(SPRAK-IX)  TO MOD-TEMFSINF                         
159800             MOVE NEJ TO INDATA-SW                                        
159900             PERFORM MFS-RENSA-FAELT-IN                                   
160000             PERFORM MFS-RENSA-FAELT-UT                                   
160100           END-IF                                                         
160200         ELSE                                                             
160300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
160400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
160500           PERFORM MFS-LAS-IN-IGEN                                        
160600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-IN-ATTR                    
160700         END-IF                                                           
160800       END-IF                                                             
160900     END-IF                                                               
161000     .                                                                    
161100     EJECT                                                                
161200******************************************************************        
161300**   KONTROLL OM STRUKTUREN ÄR KLAR.                                      
161400******************************************************************        
161500                                                                          
161600 HA-KOLLA-KLAR SECTION.                                                   
161700                                                                          
161800     IF MID-KLAR NOT = ALL '+'                                            
161900       IF MID-KLAR = 'J' OR 'N' OR 'Y'                                    
162000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KLAR-IN-ATTR                    
162100         IF MID-KLAR = 'J' OR 'Y'                                         
162200           MOVE JA TO KLAR-SW                                             
162300         ELSE                                                             
162400           PERFORM S11-HAR-USERID-KONV-STR                                
162500         END-IF                                                           
162600       ELSE                                                               
162700         IF EJ-TID-SIGNAL                                                 
162800           MOVE MED6(SPRAK-IX) TO MOD-TEMFSINF                            
162900         END-IF                                                           
163000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-IN-ATTR                      
163100         MOVE NEJ TO INDATA-SW                                            
163200       END-IF                                                             
163300     ELSE                                                                 
163400       IF EJ-TID-SIGNAL                                                   
163500         MOVE MED6(SPRAK-IX) TO MOD-TEMFSINF                              
163600       END-IF                                                             
163700       MOVE MFS-ALFA-FAELT-FEL          TO MOD-KLAR-IN-ATTR               
163800       MOVE NEJ TO INDATA-SW                                              
163900     END-IF                                                               
164000     .                                                                    
164100     EJECT                                                                
164200******************************************************************        
164300**   KONTROLL AV KOPIERING.                                               
164400******************************************************************        
164500                                                                          
164600 HB-KOLLA-KOPIERING SECTION.                                              
164700                                                                          
164800     IF RADNR-FINNS                                                       
164900       IF MID-IDRADNR-F = ALL '+' AND                                     
165000         MID-BORT = ALL '+'                                               
165100         IF MID-IDRADNR-K NUMERIC                                         
165200           MOVE MID-IDRADNR-K TO WS-RADNR                                 
165300           PERFORM S03-LETA-EFTER-RADEN                                   
165400           IF RADEN-FINNS                                                 
165500             IF EJ-TID-SIGNAL                                             
165600               MOVE MED7(SPRAK-IX) TO MOD-TEMFSINF                        
165700             END-IF                                                       
165800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-K-ATTR                 
165900             MOVE NEJ TO INDATA-SW                                        
166000           ELSE                                                           
166100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-K-ATTR               
166200             MOVE JA TO AO-SW                                             
166300           END-IF                                                         
166400         ELSE                                                             
166500           IF EJ-TID-SIGNAL                                               
166600             MOVE MED8(SPRAK-IX) TO MOD-TEMFSINF                          
166700           END-IF                                                         
166800           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDRADNR-K-ATTR                  
166900           MOVE NEJ TO INDATA-SW                                          
167000         END-IF                                                           
167100                                                                          
167200         IF MID-IDARTNR = ALL '+'                                         
167300           MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                       
167400         ELSE                                                             
167500           IF EJ-TID-SIGNAL                                               
167600             MOVE MED9(SPRAK-IX) TO MOD-TEMFSINF                          
167700           END-IF                                                         
167800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                  
167900           MOVE NEJ TO INDATA-SW                                          
168000         END-IF                                                           
168100                                                                          
168200         IF MID-IDLEVNR = ALL '+'                                         
168300           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-UT                       
168400         ELSE                                                             
168500           IF EJ-TID-SIGNAL                                               
168600             MOVE MED10(SPRAK-IX) TO MOD-TEMFSINF                         
168700           END-IF                                                         
168800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                 
168900           MOVE NEJ TO INDATA-SW                                          
169000         END-IF                                                           
169100                                                                          
169200         IF MID-BELEVART = ALL '+'                                        
169300           MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVART-UT                      
169400         ELSE                                                             
169500           IF EJ-TID-SIGNAL                                               
169600             MOVE MED22(SPRAK-IX) TO MOD-TEMFSINF                         
169700           END-IF                                                         
169800           MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-IN-ATTR                
169900           MOVE NEJ TO INDATA-SW                                          
170000         END-IF                                                           
170100                                                                          
170200         IF MID-BEART = ALL '+'                                           
170300           MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-UT                         
170400         ELSE                                                             
170500           IF EJ-TID-SIGNAL                                               
170600             MOVE MED11(SPRAK-IX) TO MOD-TEMFSINF                         
170700           END-IF                                                         
170800           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                   
170900           MOVE NEJ TO INDATA-SW                                          
171000         END-IF                                                           
171100                                                                          
171200         IF MID-KDBENHOM = ALL '+'                                        
171300           MOVE MFS-ROER-EJ-FAELT TO MOD-KDBENHOM-UT                      
171400         ELSE                                                             
171500           IF EJ-TID-SIGNAL                                               
171600             MOVE MED12(SPRAK-IX) TO MOD-TEMFSINF                         
171700           END-IF                                                         
171800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR                 
171900           MOVE NEJ TO INDATA-SW                                          
172000         END-IF                                                           
172100                                                                          
172200         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR                            
172300                                   MOD-BELEVART                           
172400                                                                          
172500         IF MID-REANTPSA = ALL '+'                                        
172600           MOVE MFS-ROER-EJ-FAELT TO MOD-REANTPSA-UT                      
172700         ELSE                                                             
172800           MOVE MID-REANTPSA TO DEC-IDFRIDATA                             
172900           MOVE +2       TO DEC-KVHELTAL                                  
173000           MOVE +3       TO DEC-KVDECIMAL                                 
173100           CALL WDECEDIT USING DEC-WDECAREA                               
173200           IF DEC-KDSVAR-FEL OR DEC-IDEDITDATA <= ZERO                    
173300             IF EJ-TID-SIGNAL                                             
173400               MOVE MED13(SPRAK-IX) TO MOD-TEMFSINF                       
173500             END-IF                                                       
173600             MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-IN-ATTR               
173700             MOVE NEJ TO INDATA-SW                                        
173800           ELSE                                                           
173900             MOVE DEC-IDEDITDATA  TO WS-REANTPSA                          
174000             MOVE MFS-NUM-FAELT-RAETT TO MOD-REANTPSA-IN-ATTR             
174100           END-IF                                                         
174200         END-IF                                                           
174300                                                                          
174400         IF MID-IDSTRTYP = ALL '+'                                        
174500           MOVE MFS-ROER-EJ-FAELT TO MOD-IDSTRTYP-UT                      
174600         ELSE                                                             
174700           IF EJ-TID-SIGNAL                                               
174800             MOVE MED14(SPRAK-IX) TO MOD-TEMFSINF                         
174900           END-IF                                                         
175000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
175100           MOVE NEJ TO INDATA-SW                                          
175200         END-IF                                                           
175300                                                                          
175400         IF MID-KDSORT = ALL '+'                                          
175500           MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-UT                        
175600         ELSE                                                             
175700           IF EJ-TID-SIGNAL                                               
175800             MOVE MED15(SPRAK-IX) TO MOD-TEMFSINF                         
175900           END-IF                                                         
176000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR                  
176100           MOVE NEJ TO INDATA-SW                                          
176200         END-IF                                                           
176300       ELSE                                                               
176400         IF EJ-TID-SIGNAL                                                 
176500           MOVE MED16(SPRAK-IX) TO MOD-TEMFSINF                           
176600         END-IF                                                           
176700         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDRADNR-K-ATTR                    
176800         MOVE NEJ TO INDATA-SW                                            
176900       END-IF                                                             
177000     ELSE                                                                 
177100       MOVE FEL8(SPRAK-IX) TO MOD-TEMFSINF                                
177200       MOVE NEJ TO INDATA-SW                                              
177300     END-IF                                                               
177400     .                                                                    
177500     EJECT                                                                
177600******************************************************************        
177700**   KONTROLL AV FLYTTNING.                                               
177800******************************************************************        
177900                                                                          
178000 HC-KOLLA-FLYTTNING SECTION.                                              
178100                                                                          
178200     IF RADNR-FINNS                                                       
178300       IF MID-IDRADNR-K = ALL '+' AND                                     
178400         MID-BORT = ALL '+'                                               
178500         IF MID-IDRADNR-F NUMERIC                                         
178600           MOVE MID-IDRADNR-F TO WS-RADNR                                 
178700           PERFORM S03-LETA-EFTER-RADEN                                   
178800           IF RADEN-FINNS                                                 
178900             IF EJ-TID-SIGNAL                                             
179000               MOVE MED7(SPRAK-IX) TO MOD-TEMFSINF                        
179100             END-IF                                                       
179200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-F-ATTR                 
179300             MOVE NEJ TO INDATA-SW                                        
179400           ELSE                                                           
179500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-F-ATTR               
179600           END-IF                                                         
179700         ELSE                                                             
179800           IF EJ-TID-SIGNAL                                               
179900             MOVE MED8(SPRAK-IX) TO MOD-TEMFSINF                          
180000           END-IF                                                         
180100           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDRADNR-F-ATTR                  
180200           MOVE NEJ TO INDATA-SW                                          
180300         END-IF                                                           
180400         IF MID-UPPDAT = ALL '+' AND                                      
180500            MID-IDAO = ALL '+'  AND                                       
180600            MID-TIAAVV = ALL '+'                                          
180700           CONTINUE                                                       
180800         ELSE                                                             
180900           IF EJ-TID-SIGNAL                                               
181000             MOVE MED17(SPRAK-IX) TO MOD-TEMFSINF                         
181100           END-IF                                                         
181200           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDRADNR-F-ATTR                  
181300           MOVE NEJ TO INDATA-SW                                          
181400         END-IF                                                           
181500                                                                          
181600       ELSE                                                               
181700         IF EJ-TID-SIGNAL                                                 
181800           MOVE MED19(SPRAK-IX) TO MOD-TEMFSINF                           
181900         END-IF                                                           
182000         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDRADNR-F-ATTR                    
182100         MOVE NEJ TO INDATA-SW                                            
182200       END-IF                                                             
182300     ELSE                                                                 
182400       MOVE FEL8(SPRAK-IX) TO MOD-TEMFSINF                                
182500       MOVE NEJ TO INDATA-SW                                              
182600     END-IF                                                               
182700     .                                                                    
182800     EJECT                                                                
182900******************************************************************        
183000**   KONTROLL AV BORTTAG.                                                 
183100******************************************************************        
183200                                                                          
183300 HD-KOLLA-BORTTAG SECTION.                                                
183400                                                                          
183500     IF RADNR-FINNS                                                       
183600       MOVE WS-RAD-TISTODAT   TO TMP1-YYMMDD                              
183700       MOVE DAGENS-DATUM      TO TMP2-YYMMDD                              
183800       PERFORM WY2000P1                                                   
183900       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
184000         IF MID-IDRADNR-K = ALL '+' AND                                   
184100           MID-IDRADNR-F = ALL '+'                                        
184200           IF MID-BORT = 'J' OR 'Y'                                       
184300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORT-IN-ATTR                
184400             IF WS-TIUPPDAT-GAM > 0                                       
184500               PERFORM IMS-GU-SATB-STR                                    
184600               IF SEGMENT-FINNS                                           
184700                 PERFORM IMS-GNP-FIRST-KVAL-RAD                           
184800                 IF SEGMENT-FINNS                                         
184900                   MOVE 'J' TO AO-SW                                      
185000                 END-IF                                                   
185100               END-IF                                                     
185200             END-IF                                                       
185300           ELSE                                                           
185400             MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-IN-ATTR                  
185500             MOVE NEJ TO INDATA-SW                                        
185600           END-IF                                                         
185700           IF MID-UPPDAT NOT = ALL '+' OR                                 
185800              MID-TESTRNOT (1) NOT = ALL '+' OR                           
185900              MID-TESTRNOT (2) NOT = ALL '+'                              
186000             IF EJ-TID-SIGNAL                                             
186100               MOVE MED18(SPRAK-IX) TO MOD-TEMFSINF                       
186200             END-IF                                                       
186300             MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-IN-ATTR                  
186400             MOVE NEJ TO INDATA-SW                                        
186500           END-IF                                                         
186600                                                                          
186700           IF MID-TIAAVV NOT = ALL '+'                                    
186800             PERFORM S13-KONV-TIAAVV                                      
186900             MOVE AKTUELLT-DATUM    TO TMP1-YYMMDD                        
187000             MOVE INNEVARANDE-VECKA TO TMP2-YYMMDD                        
187100             PERFORM WY2000P1                                             
187200             IF DAT-KDSVAR-FEL OR                                         
187300               TMP1-YYMMDD < TMP2-YYMMDD                                  
187400               IF EJ-TID-SIGNAL                                           
187500                 MOVE MED24(SPRAK-IX) TO MOD-TEMFSINF                     
187600               END-IF                                                     
187700               MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR               
187800               MOVE NEJ TO INDATA-SW                                      
187900             ELSE                                                         
188000               MOVE AKTUELLT-DATUM   TO TMP1-YYMMDD                       
188100               MOVE DAGENS-DATUM     TO TMP2-YYMMDD                       
188200               PERFORM WY2000P1                                           
188300               IF TMP1-YYMMDD < TMP2-YYMMDD                               
188400                 MOVE DAGENS-DATUM TO AKTUELLT-DATUM                      
188500               END-IF                                                     
188600               MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR             
188700             END-IF                                                       
188800           ELSE                                                           
188900             IF AO-SKALL-FINNAS                                           
189000               IF EJ-TID-SIGNAL                                           
189100                 MOVE MED25(SPRAK-IX) TO MOD-TEMFSINF                     
189200               END-IF                                                     
189300               MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR               
189400               MOVE NEJ TO INDATA-SW                                      
189500             END-IF                                                       
189600           END-IF                                                         
189700                                                                          
189800           IF MID-IDAO NOT = ALL '+'                                      
189900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                
190000             MOVE AKTUELLT-DATUM    TO TMP1-YYMMDD                        
190100             MOVE INNEVARANDE-VECKA TO TMP2-YYMMDD                        
190200             PERFORM WY2000P1                                             
190300             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
190400               IF EJ-TID-SIGNAL                                           
190500                 MOVE MED25(SPRAK-IX) TO MOD-TEMFSINF                     
190600               END-IF                                                     
190700               MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR               
190800               MOVE NEJ TO INDATA-SW                                      
190900             END-IF                                                       
191000           ELSE                                                           
191100             MOVE SPACE          TO MID-IDAO                              
191200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                
191300           END-IF                                                         
191400                                                                          
191500         ELSE                                                             
191600           IF EJ-TID-SIGNAL                                               
191700             MOVE MED26(SPRAK-IX) TO MOD-TEMFSINF                         
191800           END-IF                                                         
191900           MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-IN-ATTR                    
192000           MOVE NEJ TO INDATA-SW                                          
192100         END-IF                                                           
192200       ELSE                                                               
192300         MOVE MED27(SPRAK-IX) TO MOD-TEMFSINF                             
192400         MOVE NEJ TO INDATA-SW                                            
192500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
192600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
192700       END-IF                                                             
192800     ELSE                                                                 
192900       MOVE FEL8(SPRAK-IX) TO MOD-TEMFSINF                                
193000       MOVE NEJ TO INDATA-SW                                              
193100     END-IF                                                               
193200     .                                                                    
193300     EJECT                                                                
193400******************************************************************        
193500**   RIMLIGHETSKONTROLL AV INRAPPORTERADE FÄLT.                           
193600******************************************************************        
193700                                                                          
193800 HE-KOLLA-UPPDAT-FAELT SECTION.                                           
193900                                                                          
194000     IF MID-IDARTNR NOT = ALL '+'                                         
194100       MOVE MID-IDARTNR TO DEC-IDFRIDATA                                  
194200       MOVE +9          TO DEC-KVHELTAL                                   
194300       MOVE +0          TO DEC-KVDECIMAL                                  
194400       CALL WDECEDIT USING DEC-WDECAREA                                   
194500       IF DEC-KDSVAR-FEL OR DEC-IDEDITDATA <= ZERO OR                     
194600                            DEC-IDEDITDATA > 99999999                     
194700         IF EJ-TID-SIGNAL                                                 
194800           MOVE MED28(SPRAK-IX)   TO MOD-TEMFSINF                         
194900         END-IF                                                           
195000         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-IN-ATTR                  
195100         MOVE NEJ TO INDATA-SW                                            
195200       ELSE                                                               
195300         MOVE DEC-IDEDITDATA TO W-IDARTNR                                 
195400         MOVE W-IDARTNR      TO MID-IDARTNR                               
195500         IF W-IDARTNR = W-STRNR                                           
195600           IF EJ-TID-SIGNAL                                               
195700            MOVE MED29(SPRAK-IX) TO MOD-TEMFSINF                          
195800           END-IF                                                         
195900           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-IN-ATTR                
196000           MOVE NEJ TO INDATA-SW                                          
196100         ELSE                                                             
196200           IF INDATA-OK                                                   
196300             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-IN-ATTR              
196400             MOVE 'J' TO AO-SW                                            
196500           END-IF                                                         
196600         END-IF                                                           
196700       END-IF                                                             
196800     END-IF                                                               
196900                                                                          
197000     IF MID-IDLEVNR NOT = ALL '+'                                         
197100       IF MID-IDLEVNR(1:1) = '0' OR ' ' OR '+'                            
197200         IF EJ-TID-SIGNAL                                                 
197300           MOVE MED20(SPRAK-IX) TO MOD-TEMFSINF                           
197400         END-IF                                                           
197500         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                   
197600         MOVE NEJ TO INDATA-SW                                            
197700       ELSE                                                               
197800         MOVE MID-IDLEVNR          TO W-IDLEVNR                           
197900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                 
198000         MOVE 'J' TO AO-SW                                                
198100         IF MID-BELEVART = ALL '+'                                        
198200           IF EJ-TID-SIGNAL                                               
198300             MOVE MED30(SPRAK-IX) TO MOD-TEMFSINF                         
198400           END-IF                                                         
198500           MOVE MFS-ALFA-FAELT-FEL  TO MOD-BELEVART-IN-ATTR               
198600           MOVE NEJ TO INDATA-SW                                          
198700         END-IF                                                           
198800       END-IF                                                             
198900     END-IF                                                               
199000                                                                          
199100     IF MID-BELEVART NOT = ALL '+'                                        
199200       IF MID-IDLEVNR = ALL '+'                                           
199300         IF EJ-TID-SIGNAL                                                 
199400           MOVE MED21(SPRAK-IX) TO MOD-TEMFSINF                           
199500         END-IF                                                           
199600         MOVE MFS-ALFA-FAELT-FEL  TO MOD-BELEVART-IN-ATTR                 
199700         MOVE NEJ TO INDATA-SW                                            
199800       ELSE                                                               
199900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEVART-IN-ATTR                
200000       END-IF                                                             
200100     END-IF                                                               
200200                                                                          
200300     IF MID-REANTPSA NOT = ALL '+'                                        
200400       MOVE MID-REANTPSA TO DEC-IDFRIDATA                                 
200500       MOVE +2           TO DEC-KVHELTAL                                  
200600       MOVE +3           TO DEC-KVDECIMAL                                 
200700       CALL WDECEDIT USING DEC-WDECAREA                                   
200800       IF DEC-KDSVAR-FEL    OR DEC-IDEDITDATA = ZERO                      
200900         IF EJ-TID-SIGNAL                                                 
201000           MOVE MED13(SPRAK-IX) TO MOD-TEMFSINF                           
201100         END-IF                                                           
201200         MOVE MFS-NUM-FAELT-FEL   TO MOD-REANTPSA-IN-ATTR                 
201300         MOVE NEJ TO INDATA-SW                                            
201400       ELSE                                                               
201500         MOVE DEC-IDEDITDATA      TO WS-REANTPSA                          
201600         MOVE MFS-NUM-FAELT-RAETT TO MOD-REANTPSA-IN-ATTR                 
201700       END-IF                                                             
201800     END-IF                                                               
201900                                                                          
202000     IF MID-IDSTRTYP NOT = ALL '+'                                        
202100       IF MID-IDSTRTYP = 'S' OR 'R' OR  'K'                               
202200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSTRTYP-IN-ATTR                
202300       ELSE                                                               
202400         IF EJ-TID-SIGNAL                                                 
202500           MOVE MED31(SPRAK-IX) TO MOD-TEMFSINF                           
202600         END-IF                                                           
202700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSTRTYP-IN-ATTR                
202800         MOVE NEJ TO INDATA-SW                                            
202900       END-IF                                                             
203000     END-IF                                                               
203100                                                                          
203200     IF MID-KDSORT NOT = ALL '+'                                          
203300       MOVE MID-KDSORT TO KDSORT-SW                                       
203400       IF KDSORT-OK                                                       
203500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR                  
203600       ELSE                                                               
203700         IF EJ-TID-SIGNAL                                                 
203800           MOVE MED32(SPRAK-IX) TO MOD-TEMFSINF                           
203900         END-IF                                                           
204000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSORT-IN-ATTR                  
204100         MOVE NEJ TO INDATA-SW                                            
204200       END-IF                                                             
204300     END-IF                                                               
204400                                                                          
204500     PERFORM S07-KOLLA-AO-AAVV                                            
204600     .                                                                    
204700     EJECT                                                                
204800******************************************************************        
204900**   UPPDATERING AV REDAN EXISTERANDE RAD.                                
205000******************************************************************        
205100                                                                          
205200 HF-KOLLA-UPPDAT-RAD SECTION.                                             
205300                                                                          
205400     MOVE WS-RAD-TISTODAT   TO TMP1-YYMMDD                                
205500     MOVE DAGENS-DATUM      TO TMP2-YYMMDD                                
205600     PERFORM WY2000P1                                                     
205700     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
205800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
205900       MOVE WS-RAD-IDARTNR TO W-IDARTNR                                   
206000       PERFORM IMS-GET-ARTC-01                                            
206100       IF SEGMENT-FINNS                                                   
206200         IF MID-IDARTNR NOT = ALL '+' OR                                  
206300           MID-IDLEVNR  NOT = ALL '+' OR                                  
206400           MID-BELEVART NOT = ALL '+' OR                                  
206500           MID-BEART    NOT = ALL '+' OR                                  
206600           MID-KDBENHOM NOT = ALL '+' OR                                  
206700           MID-IDSTRTYP NOT = ALL '+' OR                                  
206800           MID-KDSORT   NOT = ALL '+'                                     
206900           IF EJ-TID-SIGNAL                                               
207000             MOVE MED33(SPRAK-IX) TO MOD-TEMFSINF                         
207100           END-IF                                                         
207200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                  
207300           MOVE NEJ TO INDATA-SW                                          
207400         ELSE                                                             
207500           IF ART-KDERS-UTG > 0                                           
207600             IF WS-REANTPSA > 0                                           
207700               MOVE FEL14(SPRAK-IX) TO MOD-TEMFSINF                       
207800               MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR              
207900               MOVE NEJ TO INDATA-SW                                      
208000             ELSE                                                         
208100               MOVE FEL14(SPRAK-IX) TO MOD-TEMFSFEL                       
208200             END-IF                                                       
208300           ELSE                                                           
208400             IF WS-REANTPSA > 0                                           
208500               MOVE 'J' TO AO-SW                                          
208600             END-IF                                                       
208700           END-IF                                                         
208800         END-IF                                                           
208900       ELSE                                                               
209000         IF MID-IDARTNR NOT = ALL '+' OR                                  
209100           MID-IDLEVNR  NOT = ALL '+' OR                                  
209200           MID-BELEVART NOT = ALL '+' OR                                  
209300           MID-IDSTRTYP NOT = ALL '+'                                     
209400           IF EJ-TID-SIGNAL                                               
209500             MOVE MED33(SPRAK-IX) TO MOD-TEMFSINF                         
209600           END-IF                                                         
209700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                  
209800           MOVE NEJ TO INDATA-SW                                          
209900         ELSE                                                             
210000           IF WS-REANTPSA > 0                                             
210100             MOVE 'J' TO AO-SW                                            
210200           END-IF                                                         
210300                                                                          
210400           PERFORM S17-KOLLA-BEART-KDHOM-AENDRING                         
210500                                                                          
210600           IF MID-KDSORT NOT = ALL '+'                                    
210700             MOVE W-STRNR        TO WS-STRNR-SPAR                         
210800             MOVE WS-RAD-IDARTNR TO W-STRNR                               
210900             PERFORM IMS-GU-SATB-STR                                      
211000             IF SEGMENT-FINNS                                             
211100               IF SATB-STR-IDSTRTYP = 'K' OR 'R'                          
211200                 IF MID-KDSORT = 'ST'                                     
211300                   MOVE MID-KDSORT TO MOD-KDSORT-UT                       
211400                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR        
211500                 ELSE                                                     
211600                   IF EJ-TID-SIGNAL                                       
211700                     MOVE MED32(SPRAK-IX) TO MOD-TEMFSINF                 
211800                   END-IF                                                 
211900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR          
212000                   MOVE NEJ TO INDATA-SW                                  
212100                 END-IF                                                   
212200               ELSE                                                       
212300                 IF SATB-STR-IDSTRTYP = 'S'                               
212400                   IF MID-KDSORT = 'SA' OR 'TM'                           
212500                     MOVE MID-KDSORT TO MOD-KDSORT-UT                     
212600                     MOVE MFS-ALFA-FAELT-RAETT TO                         
212700                                                MOD-KDSORT-IN-ATTR        
212800                   ELSE                                                   
212900                     IF EJ-TID-SIGNAL                                     
213000                       MOVE MED32(SPRAK-IX) TO MOD-TEMFSINF               
213100                     END-IF                                               
213200                     MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR        
213300                     MOVE NEJ TO INDATA-SW                                
213400                   END-IF                                                 
213500                 END-IF                                                   
213600               END-IF                                                     
213700             ELSE                                                         
213800               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR            
213900               MOVE MID-KDSORT         TO MOD-KDSORT-UT                   
214000             END-IF                                                       
214100             MOVE WS-STRNR-SPAR TO W-STRNR                                
214200           END-IF                                                         
214300         END-IF                                                           
214400       END-IF                                                             
214500     ELSE                                                                 
214600       MOVE MED27(SPRAK-IX) TO MOD-TEMFSINF                               
214700       MOVE NEJ TO INDATA-SW                                              
214800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
214900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
215000     END-IF                                                               
215100     .                                                                    
215200     EJECT                                                                
215300******************************************************************        
215400**   KONTROLL VID INLÄGGNING AV NY RAD.                                   
215500******************************************************************        
215600                                                                          
215700 HG-KOLLA-NY-RAD SECTION.                                                 
215800                                                                          
215900     IF MID-IDARTNR = ALL '+'  AND                                        
216000       MID-IDLEVNR = ALL '+'  AND                                         
216100       MID-BEART = ALL '+'                                                
216200       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-IN-ATTR                     
216300       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-IN-ATTR                    
216400                                   MOD-BEART-IN-ATTR                      
216500       MOVE NEJ TO INDATA-SW                                              
216600     ELSE                                                                 
216700       IF MID-IDARTNR NOT = ALL '+'  AND                                  
216800         MID-IDLEVNR NOT = ALL '+'                                        
216900         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-IN-ATTR                   
217000         MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-IN-ATTR                  
217100         MOVE NEJ TO INDATA-SW                                            
217200       ELSE                                                               
217300*        OM ARTIKELN FINNS PÅ CROSSINDEX TAS ARTIKELNUMRET                
217400*        SOM ID I STÄLLET FÖR LEVNR OCH LEVBET                            
217500         IF MID-IDLEVNR NOT = ALL '+'                                     
217600           PERFORM HI-KOLLA-CROSSINDEX                                    
217700         END-IF                                                           
217800         PERFORM IMS-GET-ARTC-01                                          
217900         IF SEGMENT-FINNS                                                 
218000           IF (WS-STR-IDLEVNR-ARTC = '1002 ') AND                         
218100             (ART-KDERS-UTG > 0)                                          
218200             MOVE FEL14(SPRAK-IX)      TO MOD-TEMFSINF                    
218300             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-IN-ATTR             
218400             MOVE NEJ TO INDATA-SW                                        
218500           ELSE                                                           
218600*  911017  KTR PÅ PRODUKTSLAG FÖRPACKNING TILLAGT , GE ******             
218700             MOVE ART-KDPRODSL         TO TEST-KDPRODSL                   
218800             IF KDPRODSL-VOLVO-EMB                                        
218900                MOVE MFS-NUM-FAELT-FEL                                    
219000                                   TO MOD-IDARTNR-IN-ATTR                 
219100                MOVE NEJ TO INDATA-SW                                     
219200             ELSE                                                         
219300*****************************************************************         
219400*  920117 ÄT ARTIKLAR MED EK01 TILLÅTS I SATSER DÄR LEVNR EJ    *         
219500*            SATT (IDLEVNR = SPACE )                            *         
219600*****************************************************************         
219700                PERFORM IMS-GET-ARTC11                                    
219800                IF SEGMENT-FINNS                                          
219900                  IF WS-STR-IDLEVNR-ARTC = '1002 '                        
220000                    IF CLAG-KDERS > 0                                     
220100                    MOVE MED45(SPRAK-IX) TO MOD-TEMFSINF                  
220200                    MOVE MFS-NUM-FAELT-FEL                                
220300                                       TO MOD-IDARTNR-IN-ATTR             
220400                    MOVE NEJ TO INDATA-SW                                 
220500                     ELSE                                                 
220600                       PERFORM HG-A-KOLLA-ARTA-ARTNR                      
220700                       PERFORM HG-B-KOLLA-SATB-ARTNR                      
220800                     END-IF                                               
220900                  ELSE                                                    
221000                     IF WS-STR-IDLEVNR-ARTC = SPACE                       
221100                        IF CLAG-KDERS > 01                                
221200                          MOVE MED45(SPRAK-IX) TO MOD-TEMFSINF            
221300                          MOVE MFS-NUM-FAELT-FEL                          
221400                                       TO MOD-IDARTNR-IN-ATTR             
221500                          MOVE NEJ TO INDATA-SW                           
221600                        ELSE                                              
221700                           PERFORM HG-A-KOLLA-ARTA-ARTNR                  
221800                           PERFORM HG-B-KOLLA-SATB-ARTNR                  
221900                        END-IF                                            
222000                     ELSE                                                 
222100                        PERFORM HG-A-KOLLA-ARTA-ARTNR                     
222200                        PERFORM HG-B-KOLLA-SATB-ARTNR                     
222300                     END-IF                                               
222400                  END-IF                                                  
222500               ELSE                                                       
222600                  PERFORM HG-A-KOLLA-ARTA-ARTNR                           
222700                  PERFORM HG-B-KOLLA-SATB-ARTNR                           
222800               END-IF                                                     
222900             END-IF                                                       
223000           END-IF                                                         
223100         ELSE                                                             
223200           IF WS-STR-IDLEVNR = '1002 '                                    
223300             MOVE MED46(SPRAK-IX) TO MOD-TEMFSINF                         
223400             MOVE MFS-NUM-FAELT-FEL      TO MOD-IDARTNR-IN-ATTR           
223500             MOVE NEJ TO INDATA-SW                                        
223600           ELSE                                                           
223700             PERFORM HG-C-KOLLA-SATB                                      
223800             IF STRNR-FINNS-EJ                                            
223900               PERFORM HG-D-KOLLA-NYTT-ARTNR                              
224000             END-IF                                                       
224100           END-IF                                                         
224200         END-IF                                                           
224300         IF MID-REANTPSA = ALL '+'                                        
224400           IF EJ-TID-SIGNAL                                               
224500             MOVE MED44(SPRAK-IX) TO MOD-TEMFSINF                         
224600           END-IF                                                         
224700           MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-IN-ATTR                 
224800           MOVE NEJ TO INDATA-SW                                          
224900         END-IF                                                           
225000       END-IF                                                             
225100     END-IF                                                               
225200     .                                                                    
225300     EJECT                                                                
225400******************************************************************        
225500**   DE INRAPPORTERADE FÄLTENS VÄRDEN JÄMFÖRES MED DET SOM LIGGER         
225600**   PÅ ARTC.                                                             
225700******************************************************************        
225800                                                                          
225900 HG-A-KOLLA-ARTA-ARTNR SECTION.                                           
226000                                                                          
226100     PERFORM IMS-GET-ARTC-01                                              
226200     MOVE ART-IDLEVNR    TO MOD-IDLEVNR                                   
226300                            WS-IDLEVNR                                    
226400                                                                          
226500     IF MID-KDSORT = ALL '+'  OR SPACE                                    
226600       MOVE ART-KDSORT    TO MOD-KDSORT-UT                                
226700     ELSE                                                                 
226800       IF MID-KDSORT NOT = ART-KDSORT                                     
226900         IF EJ-TID-SIGNAL                                                 
227000           MOVE MED47(SPRAK-IX) TO MOD-TEMFSINF                           
227100         END-IF                                                           
227200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSORT-IN-ATTR                  
227300         MOVE NEJ TO INDATA-SW                                            
227400       ELSE                                                               
227500         MOVE SPACE TO MID-KDSORT                                         
227600       END-IF                                                             
227700     END-IF                                                               
227800                                                                          
227900     PERFORM IMS-GET-WDF501                                               
228000     IF SEGMENT-FINNS                                                     
228100       IF WS-IDLEVNR NOT = SPACE                                          
228200         MOVE WS-IDLEVNR TO W-IDLEVNR                                     
228300         PERFORM IMS-GET-WDF502-LAST                                      
228400         IF SEGMENT-FINNS                                                 
228500           MOVE XLEV-BELEVART   TO MOD-BELEVART                           
228600         ELSE                                                             
228700           MOVE MFS-RENSA-FAELT TO MOD-BELEVART                           
228800         END-IF                                                           
228900       ELSE                                                               
229000         PERFORM IMS-GET-WDF502-OKVAL                                     
229100         IF SEGMENT-FINNS                                                 
229200           MOVE XLEV-IDLEVNR   TO W-IDLEVNR                               
229300                                  MOD-IDLEVNR                             
229400           MOVE XLEV-BELEVART      TO MOD-BELEVART                        
229500           PERFORM IMS-GET-WDF502-LAST                                    
229600           IF SEGMENT-FINNS                                               
229700             MOVE XLEV-BELEVART    TO MOD-BELEVART                        
229800           END-IF                                                         
229900         END-IF                                                           
230000       END-IF                                                             
230100     ELSE                                                                 
230200       MOVE MFS-RENSA-FAELT TO MOD-BELEVART                               
230300     END-IF                                                               
230400                                                                          
230500     PERFORM IMS-GET-BENA-BSEQ                                            
230600                                                                          
230700     IF MID-KDBENHOM = ALL '+'                                            
230800       MOVE BENA-BEN-KDHOMONYM TO MOD-KDBENHOM-UT                         
230900     ELSE                                                                 
231000       MOVE MID-KDBENHOM TO WS-KDBENHOM-NUM                               
231100       IF WS-KDBENHOM-NUM NOT = BENA-BEN-KDHOMONYM                        
231200         IF EJ-TID-SIGNAL                                                 
231300           MOVE MED42(SPRAK-IX) TO MOD-TEMFSINF                           
231400         END-IF                                                           
231500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR                
231600         MOVE NEJ TO INDATA-SW                                            
231700       ELSE                                                               
231800         MOVE ZERO  TO MID-KDBENHOM                                       
231900       END-IF                                                             
232000     END-IF                                                               
232100                                                                          
232200     PERFORM IMS-GET-BENA-B-TEXT                                          
232300                                                                          
232400     IF MID-BEART = ALL '+'  OR SPACE                                     
232500       MOVE BENA-TEXT-BEART    TO MOD-BEART-UT                            
232600     ELSE                                                                 
232700       IF MID-BEART NOT = BENA-TEXT-BEART                                 
232800         IF EJ-TID-SIGNAL                                                 
232900           MOVE MED41(SPRAK-IX) TO MOD-TEMFSINF                           
233000         END-IF                                                           
233100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEART-IN-ATTR                   
233200         MOVE NEJ TO INDATA-SW                                            
233300       ELSE                                                               
233400         MOVE SPACE TO MID-BEART                                          
233500       END-IF                                                             
233600     END-IF                                                               
233700                                                                          
233800     .                                                                    
233900     EJECT                                                                
234000******************************************************************        
234100**   KONTROLL OM ARTIKELNUMRET ÄR EN STRUKTUR.                            
234200******************************************************************        
234300                                                                          
234400 HG-B-KOLLA-SATB-ARTNR SECTION.                                           
234500                                                                          
234600     MOVE W-STRNR     TO WS-STRNR-SPAR                                    
234700     MOVE MID-IDARTNR TO W-STRNR                                          
234800     PERFORM IMS-GU-SATB-STR                                              
234900     IF SEGMENT-FINNS                                                     
235000       MOVE JA TO STRNR-FINNS-SW                                          
235100       IF SATB-STR-TIBORT > 0                                             
235200         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSINF                              
235300         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
235400         MOVE NEJ TO INDATA-SW                                            
235500       ELSE                                                               
235600         PERFORM HG-BA-KOLLA-SATB-STRNR                                   
235700         IF SATB-STR-IDSTRTYP = 'K' OR 'R' OR 'S'                         
235800           PERFORM HG-BB-KOLLA-SATS-I-SATS                                
235900         END-IF                                                           
236000       END-IF                                                             
236100     ELSE                                                                 
236200       IF ART-KDSORT = 'SA' OR 'TM'                                       
236300         MOVE MED39(SPRAK-IX) TO MOD-TEMFSINF                             
236400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
236500         MOVE NEJ TO INDATA-SW                                            
236600       ELSE                                                               
236700         IF MID-IDSTRTYP = ALL '+' OR SPACE                               
236800           CONTINUE                                                       
236900         ELSE                                                             
237000           IF EJ-TID-SIGNAL                                               
237100             MOVE MED40(SPRAK-IX) TO MOD-TEMFSINF                         
237200           END-IF                                                         
237300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSTRTYP-IN-ATTR                
237400           MOVE NEJ TO INDATA-SW                                          
237500         END-IF                                                           
237600       END-IF                                                             
237700     MOVE NEJ TO STRNR-FINNS-SW                                           
237800     MOVE WS-STRNR-SPAR TO W-STRNR                                        
237900     END-IF                                                               
238000     .                                                                    
238100     EJECT                                                                
238200******************************************************************        
238300**   DE INRAPPORTERADE FÄLTENS VÄRDEN JÄMFÖRES MED DET SOM LIGGER         
238400**   PÅ SATB.                                                             
238500******************************************************************        
238600                                                                          
238700 HG-BA-KOLLA-SATB-STRNR SECTION.                                          
238800                                                                          
238900     IF MID-BEART = ALL '+'  OR SPACE                                     
239000       CONTINUE                                                           
239100     ELSE                                                                 
239200       IF MID-BEART NOT = SATB-STR-BEART-SVE                              
239300         IF EJ-TID-SIGNAL                                                 
239400           MOVE MED41(SPRAK-IX) TO MOD-TEMFSINF                           
239500         END-IF                                                           
239600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEART-IN-ATTR                   
239700         MOVE NEJ TO INDATA-SW                                            
239800       ELSE                                                               
239900         MOVE SPACE TO MID-BEART                                          
240000       END-IF                                                             
240100     END-IF                                                               
240200                                                                          
240300     IF MID-KDBENHOM = ALL '+'                                            
240400       CONTINUE                                                           
240500     ELSE                                                                 
240600        MOVE MID-KDBENHOM TO WS-KDBENHOM-NUM                              
240700        IF WS-KDBENHOM-NUM NOT = SATB-STR-KDBENHOM                        
240800          IF EJ-TID-SIGNAL                                                
240900            MOVE MED42(SPRAK-IX) TO MOD-TEMFSINF                          
241000          END-IF                                                          
241100          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR                
241200          MOVE NEJ TO INDATA-SW                                           
241300        ELSE                                                              
241400          MOVE ZERO  TO MID-KDBENHOM                                      
241500        END-IF                                                            
241600      END-IF                                                              
241700                                                                          
241800     IF MID-IDSTRTYP = ALL '+'  OR SPACE                                  
241900       MOVE SATB-STR-IDSTRTYP  TO MOD-IDSTRTYP-UT                         
242000                                  MID-IDSTRTYP                            
242100     ELSE                                                                 
242200       IF MID-IDSTRTYP NOT = SATB-STR-IDSTRTYP                            
242300         IF EJ-TID-SIGNAL                                                 
242400           MOVE MED43(SPRAK-IX) TO MOD-TEMFSINF                           
242500         END-IF                                                           
242600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSTRTYP-IN-ATTR                
242700         MOVE NEJ TO INDATA-SW                                            
242800       ELSE                                                               
242900         MOVE SPACE TO MID-IDSTRTYP                                       
243000       END-IF                                                             
243100     END-IF                                                               
243200                                                                          
243300     IF MID-KDSORT = ALL '+'  OR SPACE                                    
243400       CONTINUE                                                           
243500     ELSE                                                                 
243600       IF MID-KDSORT NOT = 'SA' OR 'ST' OR 'TM'                           
243700         IF EJ-TID-SIGNAL                                                 
243800           MOVE MED32(SPRAK-IX) TO MOD-TEMFSINF                           
243900         END-IF                                                           
244000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSORT-IN-ATTR                  
244100         MOVE NEJ TO INDATA-SW                                            
244200       END-IF                                                             
244300     END-IF                                                               
244400     .                                                                    
244500     EJECT                                                                
244600******************************************************************        
244700**   KONTROLLERAR SATS I SATS.                                            
244800******************************************************************        
244900                                                                          
245000 HG-BB-KOLLA-SATS-I-SATS SECTION.                                         
245100                                                                          
245200     PERFORM S12-NOLLST-TAB-STR                                           
245300                                                                          
245400     MOVE 1 TO STRIND                                                     
245500                                                                          
245600     PERFORM IMS-GHU-SATB-STR                                             
245700     IF SEGMENT-FINNS                                                     
245800       PERFORM IMS-GET-SATB-RAD                                           
245900       IF SEGMENT-FINNS                                                   
246000         PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                   
246100           IF SEGMENT-FINNS                                               
246200             MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
246300             MOVE DAGENS-DATUM        TO TMP2-YYMMDD                      
246400             PERFORM WY2000P1                                             
246500             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
246600               IF SATB-RAD-IDARTNR = WS-STRNR-SPAR                        
246700                 MOVE NEJ           TO INDATA-SW                          
246800                 MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF                     
246900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR            
247000               ELSE                                                       
247100                 IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'                 
247200                   PERFORM HG-BBA-LAGRA-I-TAB-STR                         
247300                 END-IF                                                   
247400               END-IF                                                     
247500             END-IF                                                       
247600             IF INDATA-OK                                                 
247700               PERFORM IMS-GET-SATB-RAD                                   
247800             END-IF                                                       
247900           END-IF                                                         
248000         END-PERFORM                                                      
248100                                                                          
248200         IF INDATA-OK                                                     
248300           IF STRIND > 1                                                  
248400             MOVE 1 TO STRIND2                                            
248500             PERFORM UNTIL (STRIND2 > ( STRIND - 1)) OR                   
248600                            INDATA-FEL                                    
248700               MOVE TAB-STR-STRNR(STRIND2) TO W-STRNR                     
248800               PERFORM IMS-GU-SATB-STR                                    
248900               IF SEGMENT-FINNS                                           
249000                 PERFORM IMS-GET-SATB-RAD                                 
249100                 PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)           
249200                   IF SEGMENT-FINNS                                       
249300                     MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD              
249400                     MOVE DAGENS-DATUM        TO TMP2-YYMMDD              
249500                     PERFORM WY2000P1                                     
249600                     IF TMP1-YYMMDD > TMP2-YYMMDD                         
249700                       IF SATB-RAD-IDARTNR = WS-STRNR-SPAR                
249800                         MOVE NEJ         TO INDATA-SW                    
249900                         MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF             
250000                         MOVE MFS-NUM-FAELT-FEL TO                        
250100                                              MOD-IDARTNR-IN-ATTR         
250200                       ELSE                                               
250300                         IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'         
250400                           PERFORM HG-BBA-LAGRA-I-TAB-STR                 
250500                         END-IF                                           
250600                       END-IF                                             
250700                     END-IF                                               
250800                     IF INDATA-OK                                         
250900                       PERFORM IMS-GET-SATB-RAD                           
251000                     END-IF                                               
251100                   END-IF                                                 
251200                 END-PERFORM                                              
251300               END-IF                                                     
251400               ADD 1 TO STRIND2                                           
251500             END-PERFORM                                                  
251600           END-IF                                                         
251700                                                                          
251800           IF INDATA-OK                                                   
251900             PERFORM S12-NOLLST-TAB-STR                                   
252000                                                                          
252100             MOVE 1 TO STRIND                                             
252200                                                                          
252300             MOVE WS-STRNR-SPAR TO W-IDARTNR                              
252400             MOVE SPACE         TO W-IDLEVNR                              
252500                                   W-BELEVART                             
252600             MOVE MID-IDARTNR   TO WS-IDARTNR-SPAR                        
252700             PERFORM IMS-GU-SATB-CSEQ-STR                                 
252800             PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)               
252900               IF SEGMENT-FINNS                                           
253000                 IF CSEQ-STR-IDARTNR > WS-MAX-STRNR-OKONV                 
253100                   MOVE 'GE' TO STATUS-WS                                 
253200                 ELSE                                                     
253300                   IF CSEQ-STR-TIBORT > 0                                 
253400                     PERFORM IMS-GN-SATB-CSEQ-STR                         
253500                   ELSE                                                   
253600                     MOVE CSEQ-RAD-TISTODAT   TO TMP1-YYMMDD              
253700                     MOVE DAGENS-DATUM        TO TMP2-YYMMDD              
253800                     PERFORM WY2000P1                                     
253900                     IF TMP1-YYMMDD > TMP2-YYMMDD                         
254000                       IF CSEQ-STR-IDARTNR = WS-IDARTNR-SPAR              
254100                         MOVE NEJ           TO INDATA-SW                  
254200                         MOVE MED38(SPRAK-IX) TO                          
254300                                             MOD-TEMFSINF                 
254400                         MOVE MFS-NUM-FAELT-FEL TO                        
254500                                               MOD-IDARTNR-IN-ATTR        
254600                       ELSE                                               
254700                         IF CSEQ-STR-IDSTRTYP = 'K' OR 'R' OR 'S'         
254800                           PERFORM HG-BBB-LAGRA-I-TAB-STR                 
254900                         END-IF                                           
255000                         PERFORM IMS-GN-SATB-CSEQ-STR                     
255100                       END-IF                                             
255200                     ELSE                                                 
255300                       PERFORM IMS-GN-SATB-CSEQ-STR                       
255400                     END-IF                                               
255500                   END-IF                                                 
255600                 END-IF                                                   
255700                 IF INDATA-OK                                             
255800                   PERFORM IMS-GN-SATB-CSEQ-STR                           
255900                 END-IF                                                   
256000               END-IF                                                     
256100             END-PERFORM                                                  
256200                                                                          
256300             IF INDATA-OK                                                 
256400               IF STRIND > 1                                              
256500                 MOVE 1 TO STRIND2                                        
256600                 PERFORM UNTIL (STRIND2 > ( STRIND - 1)) OR               
256700                                INDATA-FEL                                
256800                   MOVE TAB-STR-STRNR(STRIND2) TO W-STRNR                 
256900                   PERFORM IMS-GU-SATB-CSEQ-STR                           
257000                   PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)         
257100                     IF SEGMENT-FINNS                                     
257200                       IF CSEQ-STR-IDARTNR > WS-MAX-STRNR-OKONV           
257300                         MOVE 'GE' TO STATUS-WS                           
257400                       ELSE                                               
257500                         IF CSEQ-STR-TIBORT > 0                           
257600                           PERFORM IMS-GN-SATB-CSEQ-STR                   
257700                         ELSE                                             
257800                           MOVE CSEQ-RAD-TISTODAT   TO TMP1-YYMMDD        
257900                           MOVE DAGENS-DATUM        TO TMP2-YYMMDD        
258000                           PERFORM WY2000P1                               
258100                           IF TMP1-YYMMDD > TMP2-YYMMDD                   
258200                             IF CSEQ-STR-IDARTNR = WS-IDARTNR-SPAR        
258300                               MOVE NEJ           TO INDATA-SW            
258400                               MOVE MED38(SPRAK-IX) TO                    
258500                                           MOD-TEMFSINF                   
258600                               MOVE MFS-NUM-FAELT-FEL TO                  
258700                                              MOD-IDARTNR-IN-ATTR         
258800                             ELSE                                         
258900                               IF CSEQ-STR-IDSTRTYP = 'K' OR 'R'          
259000                                                          OR 'S'          
259100                                 PERFORM HG-BBB-LAGRA-I-TAB-STR           
259200                               END-IF                                     
259300                               PERFORM IMS-GN-SATB-CSEQ-STR               
259400                             END-IF                                       
259500                           ELSE                                           
259600                             PERFORM IMS-GN-SATB-CSEQ-STR                 
259700                           END-IF                                         
259800                         END-IF                                           
259900                       END-IF                                             
260000                     END-IF                                               
260100                   END-PERFORM                                            
260200                   ADD 1 TO STRIND2                                       
260300                 END-PERFORM                                              
260400               END-IF                                                     
260500             END-IF                                                       
260600           END-IF                                                         
260700         END-IF                                                           
260800       ELSE                                                               
260900         MOVE NEJ   TO INDATA-SW                                          
261000         MOVE MED50(SPRAK-IX) TO MOD-TEMFSINF                             
261100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
261200       END-IF                                                             
261300     END-IF                                                               
261400     MOVE WS-STRNR-SPAR TO W-STRNR                                        
261500     MOVE MID-IDARTNR   TO W-IDARTNR                                      
261600     .                                                                    
261700     EJECT                                                                
261800******************************************************************        
261900**   STRUKTURNUMMER SOM INTE FINNS TIDIGARE I TABELLEN                    
262000**   LÄGGS IN.                                                            
262100******************************************************************        
262200                                                                          
262300 HG-BBA-LAGRA-I-TAB-STR SECTION.                                          
262400                                                                          
262500     MOVE +1 TO K-STRIND                                                  
262600     MOVE NEJ TO STRNR-FINNS-SW                                           
262700     PERFORM UNTIL K-STRIND > STRIND                                      
262800       IF TAB-STR-STRNR(K-STRIND) = SATB-RAD-IDARTNR                      
262900         MOVE JA  TO STRNR-FINNS-SW                                       
263000         MOVE 999 TO K-STRIND                                             
263100       ELSE                                                               
263200         ADD +1   TO K-STRIND                                             
263300       END-IF                                                             
263400     END-PERFORM                                                          
263500                                                                          
263600     IF STRNR-FINNS                                                       
263700       CONTINUE                                                           
263800     ELSE                                                                 
263900       MOVE SATB-RAD-IDARTNR TO TAB-STR-STRNR(STRIND)                     
264000       ADD +1                TO STRIND                                    
264100     END-IF                                                               
264200     .                                                                    
264300     EJECT                                                                
264400******************************************************************        
264500**   STRUKTURNUMMER SOM INTE FINNS TIDIGARE I TABELLEN                    
264600**   LÄGGS IN.                                                            
264700******************************************************************        
264800                                                                          
264900 HG-BBB-LAGRA-I-TAB-STR SECTION.                                          
265000                                                                          
265100     MOVE +1 TO K-STRIND                                                  
265200     MOVE NEJ TO STRNR-FINNS-SW                                           
265300     PERFORM UNTIL K-STRIND > STRIND                                      
265400       IF TAB-STR-STRNR(K-STRIND) = CSEQ-STR-IDARTNR                      
265500         MOVE JA  TO STRNR-FINNS-SW                                       
265600         MOVE 999 TO K-STRIND                                             
265700       ELSE                                                               
265800         ADD +1   TO K-STRIND                                             
265900       END-IF                                                             
266000     END-PERFORM                                                          
266100                                                                          
266200     IF STRNR-FINNS                                                       
266300       CONTINUE                                                           
266400     ELSE                                                                 
266500       MOVE CSEQ-STR-IDARTNR TO TAB-STR-STRNR(STRIND)                     
266600       ADD +1                TO STRIND                                    
266700     END-IF                                                               
266800     .                                                                    
266900     EJECT                                                                
267000******************************************************************        
267100**   KONTROLL OM ARTIKELNUMRET ÄR EN STRUKTUR.                            
267200******************************************************************        
267300                                                                          
267400 HG-C-KOLLA-SATB SECTION.                                                 
267500                                                                          
267600     MOVE W-STRNR     TO WS-STRNR-SPAR                                    
267700     MOVE MID-IDARTNR TO W-STRNR                                          
267800     PERFORM IMS-GU-SATB-STR                                              
267900     IF SEGMENT-FINNS                                                     
268000       MOVE JA TO STRNR-FINNS-SW                                          
268100       IF SATB-STR-TIBORT > 0                                             
268200         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSINF                              
268300         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
268400         MOVE NEJ TO INDATA-SW                                            
268500       ELSE                                                               
268600         PERFORM HG-CA-KOLLA-SATB-STRNR                                   
268700         IF SATB-STR-IDSTRTYP = 'K' OR 'R' OR 'S'                         
268800           PERFORM HG-BB-KOLLA-SATS-I-SATS                                
268900         END-IF                                                           
269000       END-IF                                                             
269100     ELSE                                                                 
269200       IF MID-IDSTRTYP = 'K' OR 'R' OR 'S'                                
269300         MOVE MED48(SPRAK-IX) TO MOD-TEMFSINF                             
269400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
269500         MOVE NEJ TO INDATA-SW                                            
269600       END-IF                                                             
269700       MOVE NEJ TO STRNR-FINNS-SW                                         
269800     END-IF                                                               
269900     MOVE WS-STRNR-SPAR TO W-STRNR                                        
270000     MOVE MID-IDARTNR   TO W-IDARTNR                                      
270100     .                                                                    
270200     EJECT                                                                
270300******************************************************************        
270400**   DE INRAPPORTERADE FÄLTENS VÄRDEN JÄMFÖRES MED DET SOM LIGGER         
270500**   PÅ SATB.                                                             
270600******************************************************************        
270700                                                                          
270800 HG-CA-KOLLA-SATB-STRNR SECTION.                                          
270900                                                                          
271000     MOVE SATB-STR-IDLEVNR TO MOD-IDLEVNR                                 
271100                                                                          
271200     IF MID-BEART = ALL '+'  OR SPACE                                     
271300       MOVE SATB-STR-BEART-SVE TO MOD-BEART-UT                            
271400                                  MID-BEART                               
271500     ELSE                                                                 
271600       IF MID-BEART NOT = SATB-STR-BEART-SVE                              
271700         IF EJ-TID-SIGNAL                                                 
271800           MOVE MED41(SPRAK-IX) TO MOD-TEMFSINF                           
271900         END-IF                                                           
272000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEART-IN-ATTR                   
272100         MOVE NEJ TO INDATA-SW                                            
272200       END-IF                                                             
272300     END-IF                                                               
272400                                                                          
272500     IF MID-KDBENHOM = ALL '+'                                            
272600       MOVE SATB-STR-KDBENHOM  TO MOD-KDBENHOM-UT                         
272700                                  MID-KDBENHOM                            
272800     ELSE                                                                 
272900        MOVE MID-KDBENHOM TO WS-KDBENHOM-NUM                              
273000        IF WS-KDBENHOM-NUM NOT = SATB-STR-KDBENHOM                        
273100          IF EJ-TID-SIGNAL                                                
273200            MOVE MED42(SPRAK-IX) TO MOD-TEMFSINF                          
273300          END-IF                                                          
273400          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR                
273500          MOVE NEJ TO INDATA-SW                                           
273600        END-IF                                                            
273700      END-IF                                                              
273800                                                                          
273900     IF MID-IDSTRTYP = ALL '+'  OR SPACE                                  
274000       MOVE SATB-STR-IDSTRTYP  TO MOD-IDSTRTYP-UT                         
274100                                  MID-IDSTRTYP                            
274200     ELSE                                                                 
274300       IF MID-IDSTRTYP NOT = SATB-STR-IDSTRTYP                            
274400         IF EJ-TID-SIGNAL                                                 
274500           MOVE MED43(SPRAK-IX) TO MOD-TEMFSINF                           
274600         END-IF                                                           
274700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSTRTYP-IN-ATTR                
274800         MOVE NEJ TO INDATA-SW                                            
274900       END-IF                                                             
275000     END-IF                                                               
275100                                                                          
275200     IF MID-KDSORT = ALL '+'  OR SPACE                                    
275300       IF SATB-STR-IDSTRTYP = 'K' OR 'R'                                  
275400         MOVE 'ST'   TO WS-KDSORT                                         
275500       ELSE                                                               
275600         MOVE 'SA'   TO WS-KDSORT                                         
275700       END-IF                                                             
275800       MOVE WS-KDSORT  TO MOD-KDSORT-UT                                   
275900                          MID-KDSORT                                      
276000     ELSE                                                                 
276100       IF MID-KDSORT NOT = 'SA' OR 'ST' OR 'TM'                           
276200         IF EJ-TID-SIGNAL                                                 
276300           MOVE MED32(SPRAK-IX) TO MOD-TEMFSINF                           
276400         END-IF                                                           
276500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSORT-IN-ATTR                  
276600         MOVE NEJ TO INDATA-SW                                            
276700       END-IF                                                             
276800     END-IF                                                               
276900     .                                                                    
277000     EJECT                                                                
277100******************************************************************        
277200**   NYTT ARTIKELNUMMER SKALL LÄGGAS UPP I RASA.                          
277300******************************************************************        
277400                                                                          
277500 HG-D-KOLLA-NYTT-ARTNR SECTION.                                           
277600                                                                          
277700     PERFORM S18-KOLLA-BEART-KDHOM-NYUPPL                                 
277800                                                                          
277900     IF MID-KDSORT = ALL '+'                                              
278000       IF EJ-TID-SIGNAL                                                   
278100         MOVE MED34(SPRAK-IX) TO MOD-TEMFSINF                             
278200       END-IF                                                             
278300       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSORT-IN-ATTR                    
278400       MOVE NEJ TO INDATA-SW                                              
278500     ELSE                                                                 
278600       MOVE MID-KDSORT    TO MOD-KDSORT-UT                                
278700     END-IF                                                               
278800                                                                          
278900     IF MID-IDSTRTYP = ALL '+'                                            
279000       MOVE MFS-RENSA-FAELT TO MOD-IDSTRTYP-UT                            
279100     ELSE                                                                 
279200       IF MID-IDSTRTYP = 'K' OR 'R'                                       
279300         IF MID-KDSORT NOT = 'ST'                                         
279400           IF EJ-TID-SIGNAL                                               
279500             MOVE MED36(SPRAK-IX) TO MOD-TEMFSINF                         
279600             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR                
279700             MOVE NEJ TO INDATA-SW                                        
279800           END-IF                                                         
279900         END-IF                                                           
280000       ELSE                                                               
280100         MOVE MID-IDSTRTYP TO MOD-IDSTRTYP-UT                             
280200       END-IF                                                             
280300                                                                          
280400       IF MID-IDSTRTYP = 'S'                                              
280500         IF MID-KDSORT NOT = 'SA' OR 'TM'                                 
280600           IF EJ-TID-SIGNAL                                               
280700             MOVE MED35(SPRAK-IX) TO MOD-TEMFSINF                         
280800             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR                
280900             MOVE NEJ TO INDATA-SW                                        
281000           END-IF                                                         
281100         END-IF                                                           
281200       ELSE                                                               
281300         MOVE MID-IDSTRTYP TO MOD-IDSTRTYP-UT                             
281400       END-IF                                                             
281500     END-IF                                                               
281600                                                                          
281700     IF MID-KDSORT = 'SA' OR 'TM'                                         
281800       IF MID-IDSTRTYP NOT = 'S'                                          
281900         IF EJ-TID-SIGNAL                                                 
282000           MOVE MED37(SPRAK-IX) TO MOD-TEMFSINF                           
282100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSTRTYP-IN-ATTR              
282200           MOVE NEJ TO INDATA-SW                                          
282300         END-IF                                                           
282400       END-IF                                                             
282500     END-IF                                                               
282600     .                                                                    
282700     EJECT                                                                
282800******************************************************************        
282900**   NYTT ARTIKELNUMMER SKALL LÄGGAS UPP I RASA.                          
283000******************************************************************        
283100                                                                          
283200 HH-KOLLA-RAD SECTION.                                                    
283300                                                                          
283400     PERFORM IMS-GHNP-SATB-RAD                                            
283500                                                                          
283600     IF SEGMENT-FINNS                                                     
283700       MOVE SATB-RAD-WDJ111 TO WS-RAD-WDJ111                              
283800       MOVE JA TO RADNR-SW                                                
283900     END-IF                                                               
284000     .                                                                    
284100     EJECT                                                                
284200******************************************************************        
284300**   FINNS ARTIKELN PÅ CROSSINDEX OCH LEVNR STÄMMER ÖVERENS               
284400**   MELLAN CROSS OCH ARTIKELREGISTRET, TAS ARTIKELNUMRET.                
284500**   OM LEVNR PÅ ART.REG. ÄR 'SPACE', JÄMFÖR MAN MOT STRUKTURENS          
284600**   LEVNR ÄR DET LIKA GODKÄNNS ARTIKELNUMRET.                            
284700**                                                                        
284800******************************************************************        
284900                                                                          
285000 HI-KOLLA-CROSSINDEX SECTION.                                             
285100                                                                          
285200     MOVE ZERO        TO WS-IDARTNR-SPAR                                  
285300                         WS-IDARTNR-ART-SPAR                              
285400     MOVE MID-IDLEVNR TO W-IDLEVNR-A                                      
285500     CALL W009REDU USING MID-BELEVART W-IDLEVART-A                        
285600     PERFORM IMS-GET-WDF5-ASEQ                                            
285700     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
285800                   MID-IDLEVNR = ALL '+'                                  
285900       MOVE XART-IDARTNR      TO W-IDARTNR                                
286000       IF WS-IDARTNR-SPAR = ZERO                                          
286100         MOVE W-IDARTNR TO WS-IDARTNR-SPAR                                
286200       END-IF                                                             
286300       PERFORM IMS-GET-ARTC-01                                            
286400       IF SEGMENT-FINNS                                                   
286500         IF WS-IDARTNR-ART-SPAR = ZERO                                    
286600           MOVE W-IDARTNR TO WS-IDARTNR-ART-SPAR                          
286700         END-IF                                                           
286800         IF ART-IDLEVNR = SPACE                                           
286900           IF W-IDLEVNR-A = WS-STR-IDLEVNR-ARTC                           
287000             MOVE W-IDARTNR TO WS-IDARTNR-ART-SPAR                        
287100           END-IF                                                         
287200         ELSE                                                             
287300           IF ART-IDLEVNR = WS-STR-IDLEVNR-ARTC                           
287400             IF XART-IDARTNR = W-STRNR                                    
287500               MOVE NEJ TO INDATA-SW                                      
287600               MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF                       
287700             ELSE                                                         
287800               MOVE XART-IDARTNR     TO MID-IDARTNR                       
287900               MOVE WS-IDLEVNR-PLUS  TO MID-IDLEVNR                       
288000               MOVE WS-BELEVART-PLUS TO MID-BELEVART                      
288100             END-IF                                                       
288200           END-IF                                                         
288300         END-IF                                                           
288400       END-IF                                                             
288500       PERFORM IMS-GN-WDF5-ASEQ                                           
288600     END-PERFORM                                                          
288700                                                                          
288800     IF MID-IDARTNR = ALL '+'                                             
288900       IF WS-IDARTNR-ART-SPAR NOT = ZERO                                  
289000         IF WS-IDARTNR-ART-SPAR = W-STRNR                                 
289100           MOVE NEJ TO INDATA-SW                                          
289200           MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF                           
289300         ELSE                                                             
289400           MOVE WS-IDARTNR-ART-SPAR TO W-IDARTNR                          
289500                                       MID-IDARTNR                        
289600           MOVE WS-IDLEVNR-PLUS     TO MID-IDLEVNR                        
289700           MOVE WS-BELEVART-PLUS    TO MID-BELEVART                       
289800         END-IF                                                           
289900       ELSE                                                               
290000         IF WS-IDARTNR-SPAR NOT = ZERO                                    
290100           IF WS-IDARTNR-SPAR = W-STRNR                                   
290200             MOVE NEJ TO INDATA-SW                                        
290300             MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF                         
290400           ELSE                                                           
290500             MOVE WS-IDARTNR-SPAR  TO W-IDARTNR                           
290600                                      MID-IDARTNR                         
290700             MOVE WS-IDLEVNR-PLUS  TO MID-IDLEVNR                         
290800             MOVE WS-BELEVART-PLUS TO MID-BELEVART                        
290900           END-IF                                                         
291000         ELSE                                                             
291100           MOVE ZERO TO W-IDARTNR                                         
291200         END-IF                                                           
291300       END-IF                                                             
291400     END-IF                                                               
291500     .                                                                    
291600     EJECT                                                                
291700******************************************************************        
291800**   UPPDATERA DET KONVERTERADE STRUKTURNUMRET.                           
291900******************************************************************        
292000                                                                          
292100 I-UPPDATERA SECTION.                                                     
292200                                                                          
292300***  OM RADNUMRET SAKNAS SKAPAS EN NY RAD                                 
292400     IF RADNR-SAKNAS                                                      
292500       MOVE BLANKA-RAD     TO WS-RAD-WDJ111                               
292600       MOVE W-IDRADNR      TO WS-RAD-IDRADNR                              
292700       MOVE MID-IDAO       TO WS-RAD-IDAO-STA                             
292800       IF WS-TIUPPDAT-GAM > 0                                             
292900         MOVE 'N'          TO WS-RAD-KDISATS                              
293000       END-IF                                                             
293100       MOVE DAGENS-DATUM   TO WS-RAD-TIREGDAT                             
293200                              MOD-TIREGDAT                                
293300       MOVE AKTUELLT-DATUM TO WS-RAD-TISTADAT                             
293400       MOVE +999999        TO WS-RAD-TISTODAT                             
293500     END-IF                                                               
293600                                                                          
293700     PERFORM IB-UPPDATERA-RADEN                                           
293800                                                                          
293900     IF STRUKTUR-KLAR                                                     
294000       PERFORM IE-KONV-KLAR                                               
294100     ELSE                                                                 
294200       MOVE FEL15(SPRAK-IX) TO MOD-TEMFSFEL                               
294300       IF UPPDATERAT                                                      
294400         PERFORM IMS-GHU-SATB-STR-K-PCB                                   
294500         IF KONV-STR-IDSTRTYP = 'S'                                       
294600           MOVE W-STRNR TO W-IDARTNR                                      
294700           PERFORM IMS-GET-ARTC-01                                        
294800           IF SEGMENT-FINNS                                               
294900             IF KONV-STR-TIUPPDAT = ZERO                                  
295000                CONTINUE                                                  
295100             ELSE                                                         
295200                IF KONV-STR-IDLEVNR = '1002 '                             
295300                   MOVE JA TO KONV-STR-FLFORPQ                            
295400                ELSE                                                      
295500                   PERFORM IMS-GET-ARTC11                                 
295600                   IF SEGMENT-FINNS                                       
295700                      MOVE CLAG-BEFT TO WS-BEFT-AKTUELL                   
295800                      IF BEFT-AKTUELL                                     
295900                         MOVE JA TO KONV-STR-FLFORPQ                      
296000                      END-IF                                              
296100                   END-IF                                                 
296200                 END-IF                                                   
296300             END-IF                                                       
296400           END-IF                                                         
296500         END-IF                                                           
296600         MOVE DAGENS-DATUM TO KONV-STR-TIREGDAT                           
296700         PERFORM IMS-REPL-SATB-K-PCB                                      
296800       END-IF                                                             
296900     END-IF                                                               
297000                                                                          
297100     IF INDATA-OK                                                         
297200       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
297300       CALL WMEDKONV USING MED-WMEDAREA                                   
297400       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
297500       PERFORM MFS-FORM-ATTR                                              
297600       PERFORM MFS-RENSA-FAELT-IN                                         
297700       IF INGA-NOT                                                        
297800         MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT-IN(1)                       
297900         MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT-IN(2)                       
298000       END-IF                                                             
298100     ELSE                                                                 
298200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
298300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
298400     END-IF                                                               
298500     .                                                                    
298600     EJECT                                                                
298700******************************************************************        
298800**   UPPDATERA RADVÄRDEN.                                                 
298900******************************************************************        
299000                                                                          
299100 IB-UPPDATERA-RADEN SECTION.                                              
299200                                                                          
299300     MOVE '0'       TO WS-RAD-KDSTRRAD                                    
299400                                                                          
299500     IF MID-IDRADNR-F NOT = ALL '+'                                       
299600       PERFORM IB-A-FLYTTNING                                             
299700     ELSE                                                                 
299800                                                                          
299900       IF MID-IDRADNR-K NOT = ALL '+'                                     
300000         PERFORM IB-B-KOPIERING                                           
300100       ELSE                                                               
300200                                                                          
300300         IF MID-BORT NOT = ALL '+'                                        
300400           PERFORM IB-C-BORTTAG                                           
300500         ELSE                                                             
300600                                                                          
300700           PERFORM IB-D-UPPDATERA-RADEN                                   
300800         END-IF                                                           
300900       END-IF                                                             
301000     END-IF                                                               
301100                                                                          
301200     PERFORM IB-E-UPPDATERA-NOTERING                                      
301300                                                                          
301400     IF UPPDATERAT                                                        
301500       MOVE JA TO FORP-SW                                                 
301600     END-IF                                                               
301700     .                                                                    
301800     EJECT                                                                
301900******************************************************************        
302000**   UPPDATERA RADVÄRDEN.                                                 
302100******************************************************************        
302200                                                                          
302300 IB-A-FLYTTNING SECTION.                                                  
302400                                                                          
302500     PERFORM MFS-ROER-EJ-FAELT-UT                                         
302600     MOVE MID-IDRADNR-F TO WS-RAD-IDRADNR                                 
302700                           W-IDRADNR                                      
302800                           MOD-IDRADNR-UT                                 
302900                           MOD-IDRADNR-B                                  
303000     INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE               
303100     MOVE WS-RAD-WDJ111  TO KONV-RAD-WDJ111                               
303200     PERFORM IMS-ISRT-SATB-RAD-K-PCB                                      
303300     PERFORM S09-FLYTTA-BEH-NOT                                           
303400     PERFORM IMS-GHU-SATB-KONV-STR                                        
303500     MOVE WS-IDRADNR     TO W-IDRADNR                                     
303600     PERFORM IMS-GHNP-SATB-RAD                                            
303700     PERFORM IMS-DLET-SATB                                                
303800     MOVE MID-IDRADNR-F  TO W-IDRADNR                                     
303900     PERFORM IMS-GU-SATB-STR-K-PCB                                        
304000     .                                                                    
304100     EJECT                                                                
304200******************************************************************        
304300**   UPPDATERA RADVÄRDEN.                                                 
304400******************************************************************        
304500                                                                          
304600 IB-B-KOPIERING SECTION.                                                  
304700                                                                          
304800     IF MID-REANTPSA = ALL '+'                                            
304900       CONTINUE                                                           
305000     ELSE                                                                 
305100       MOVE WS-REANTPSA  TO WS-RAD-REANTPSA                               
305200                            MOD-REANTPSA-UT                               
305300     END-IF                                                               
305400                                                                          
305500     MOVE MID-IDRADNR-K     TO WS-RAD-IDRADNR                             
305600                               W-IDRADNR                                  
305700                               MOD-IDRADNR-UT                             
305800                               MOD-IDRADNR-B                              
305900     INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE               
306000     MOVE MID-IDAO          TO WS-RAD-IDAO-STA                            
306100     MOVE SPACE             TO WS-RAD-IDAO-STO                            
306200     IF WS-TIUPPDAT-GAM > 0                                               
306300       MOVE 'N'             TO WS-RAD-KDISATS                             
306400     END-IF                                                               
306500     MOVE DAGENS-DATUM      TO WS-RAD-TIREGDAT                            
306600                             MOD-TIREGDAT                                 
306700     MOVE AKTUELLT-DATUM    TO WS-RAD-TISTADAT                            
306800     MOVE +999999           TO WS-RAD-TISTODAT                            
306900     MOVE WS-RAD-WDJ111     TO KONV-RAD-WDJ111                            
307000     PERFORM IMS-ISRT-SATB-RAD-K-PCB                                      
307100     MOVE JA TO UPPDAT-SW                                                 
307200     .                                                                    
307300     EJECT                                                                
307400******************************************************************        
307500**   UPPDATERA RADVÄRDEN.                                                 
307600******************************************************************        
307700                                                                          
307800 IB-C-BORTTAG SECTION.                                                    
307900                                                                          
308000     PERFORM MFS-ROER-EJ-FAELT-UT                                         
308100                                                                          
308200     IF WS-TIUPPDAT-GAM = ZERO                                            
308300       PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                              
308400       PERFORM IMS-DLET-SATB-K-PCB                                        
308500       MOVE JA TO BORT-SW                                                 
308600     ELSE                                                                 
308700       PERFORM IMS-GU-SATB-STR                                            
308800       PERFORM IMS-GNP-FIRST-KVAL-RAD                                     
308900       IF SEGMENT-FINNS                                                   
309000         MOVE 'U'              TO WS-RAD-KDISATS                          
309100         MOVE MID-IDAO         TO WS-RAD-IDAO-STO                         
309200         MOVE AKTUELLT-DATUM TO WS-RAD-TISTODAT                           
309300         PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                            
309400         MOVE WS-RAD-WDJ111    TO KONV-RAD-WDJ111                         
309500         PERFORM IMS-REPL-SATB-K-PCB                                      
309600         MOVE JA         TO UPPDAT-SW                                     
309700       ELSE                                                               
309800         PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                            
309900         PERFORM IMS-DLET-SATB-K-PCB                                      
310000       END-IF                                                             
310100     END-IF                                                               
310200     .                                                                    
310300     EJECT                                                                
310400******************************************************************        
310500**   UPPDATERA RADVÄRDEN.                                                 
310600******************************************************************        
310700                                                                          
310800 IB-D-UPPDATERA-RADEN SECTION.                                            
310900                                                                          
311000     IF MID-IDARTNR = ALL '+'                                             
311100       CONTINUE                                                           
311200     ELSE                                                                 
311300       MOVE MID-IDARTNR TO WS-RAD-IDARTNR                                 
311400                           MOD-IDARTNR-UT                                 
311500       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
311600       MOVE JA TO UPPDAT-SW                                               
311700     END-IF                                                               
311800                                                                          
311900     IF MID-IDLEVNR = ALL '+'                                             
312000       CONTINUE                                                           
312100     ELSE                                                                 
312200       MOVE MID-IDLEVNR TO WS-RAD-IDLEVNR                                 
312300                           MOD-IDLEVNR-UT                                 
312400       MOVE JA TO UPPDAT-SW                                               
312500     END-IF                                                               
312600                                                                          
312700     IF MID-BELEVART = ALL '+' OR SPACE                                   
312800       CONTINUE                                                           
312900     ELSE                                                                 
313000       MOVE MID-BELEVART TO WS-RAD-BELEVART                               
313100                           MOD-BELEVART-UT                                
313200       MOVE JA TO UPPDAT-SW                                               
313300     END-IF                                                               
313400                                                                          
313500     IF MID-REANTPSA = ALL '+'                                            
313600       CONTINUE                                                           
313700     ELSE                                                                 
313800       IF RADNR-FINNS                                                     
313900         MOVE MID-IDAO         TO WS-RAD-IDAO-STO                         
314000         MOVE 'U'              TO WS-RAD-KDISATS                          
314100         MOVE AKTUELLT-DATUM   TO WS-RAD-TISTODAT                         
314200         PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                            
314300         MOVE WS-RAD-WDJ111    TO KONV-RAD-WDJ111                         
314400         PERFORM IMS-REPL-SATB-K-PCB                                      
314500       END-IF                                                             
314600     END-IF                                                               
314700                                                                          
314800     IF MID-BEART = ALL '+'                                               
314900       CONTINUE                                                           
315000     ELSE                                                                 
315100       IF MID-IDSKYLT-UT = 'GB '                                          
315200         MOVE WS-BEART TO WS-RAD-BEART-SVE                                
315300       ELSE                                                               
315400         MOVE MID-BEART TO WS-RAD-BEART-SVE                               
315500       END-IF                                                             
315600     END-IF                                                               
315700                                                                          
315800     IF MID-KDBENHOM = ALL '+'                                            
315900       CONTINUE                                                           
316000     ELSE                                                                 
316100       MOVE MID-KDBENHOM TO WS-RAD-KDBENHOM                               
316200     END-IF                                                               
316300                                                                          
316400     IF MID-IDSTRTYP = ALL '+'                                            
316500       CONTINUE                                                           
316600     ELSE                                                                 
316700       MOVE MID-IDSTRTYP TO WS-RAD-IDSTRTYP                               
316800     END-IF                                                               
316900                                                                          
317000     IF MID-KDSORT = ALL '+'                                              
317100       CONTINUE                                                           
317200     ELSE                                                                 
317300       MOVE MID-KDSORT TO WS-RAD-KDSORT                                   
317400     END-IF                                                               
317500                                                                          
317600     IF MID-REANTPSA = ALL '+'                                            
317700       CONTINUE                                                           
317800     ELSE                                                                 
317900       IF RADNR-FINNS                                                     
318000         ADD +1                TO WS-RAD-IDRADNR                          
318100         MOVE WS-RAD-IDRADNR   TO W-IDRADNR                               
318200         MOVE W-IDRADNR        TO WS-RADNR4                               
318300         MOVE WS-RADNR4        TO MOD-IDRADNR-UT                          
318400                                  MOD-IDRADNR-B                           
318500         INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE           
318600         MOVE MID-IDAO         TO WS-RAD-IDAO-STA                         
318700         MOVE SPACE            TO WS-RAD-IDAO-STO                         
318800         IF WS-TIUPPDAT-GAM > 0                                           
318900           MOVE 'N'            TO WS-RAD-KDISATS                          
319000         ELSE                                                             
319100           MOVE ' '            TO WS-RAD-KDISATS                          
319200         END-IF                                                           
319300         MOVE WS-REANTPSA      TO WS-RAD-REANTPSA                         
319400                                  MOD-REANTPSA-UT                         
319500         MOVE DAGENS-DATUM     TO WS-RAD-TIREGDAT                         
319600                                  MOD-TIREGDAT                            
319700         MOVE AKTUELLT-DATUM   TO WS-RAD-TISTADAT                         
319800         MOVE +999999          TO WS-RAD-TISTODAT                         
319900         MOVE WS-RAD-WDJ111    TO KONV-RAD-WDJ111                         
320000         PERFORM IMS-ISRT-SATB-RAD-K-PCB                                  
320100         MOVE JA  TO UPPDAT-SW                                            
320200         MOVE JA  TO UTSKRIV-SW                                           
320300       ELSE                                                               
320400         MOVE WS-REANTPSA      TO WS-RAD-REANTPSA                         
320500                                  MOD-REANTPSA-UT                         
320600         MOVE JA TO UPPDAT-SW                                             
320700       END-IF                                                             
320800     END-IF                                                               
320900                                                                          
321000     IF MID-UPPDAT NOT = ALL '+'  AND                                     
321100        EJ-UTSKRIVEN                                                      
321200       PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                              
321300       MOVE WS-RAD-WDJ111 TO KONV-RAD-WDJ111                              
321400       IF SEGMENT-FINNS                                                   
321500         PERFORM IMS-REPL-SATB-K-PCB                                      
321600       ELSE                                                               
321700         PERFORM IMS-ISRT-SATB-RAD-K-PCB                                  
321800       END-IF                                                             
321900     END-IF                                                               
322000     .                                                                    
322100     EJECT                                                                
322200******************************************************************        
322300**   UPPDATERA RADVÄRDEN.                                                 
322400******************************************************************        
322500                                                                          
322600 IB-E-UPPDATERA-NOTERING SECTION.                                         
322700                                                                          
322800     IF BORTTAGEN-RAD                                                     
322900       CONTINUE                                                           
323000     ELSE                                                                 
323100       PERFORM IMS-GHNP-FIRST-SATB-NOT-K-PCB                              
323200                                                                          
323300       IF SEGMENT-FINNS                                                   
323400         MOVE KONV-NOT-WDJ122 TO WS-LAGRA-NOT                             
323500       ELSE                                                               
323600         MOVE SPACE         TO WS-LAGRA-NOT                               
323700       END-IF                                                             
323800                                                                          
323900       IF MID-TESTRNOT(1) = ALL '+'                                       
324000         CONTINUE                                                         
324100       ELSE                                                               
324200         MOVE MID-TESTRNOT(1) TO WS-NOT-TESTRNOT(1)                       
324300                              MOD-TESTRNOT-IN(1)                          
324400         MOVE JA            TO NOT-SW                                     
324500       END-IF                                                             
324600                                                                          
324700       IF MID-TESTRNOT(2) = ALL '+'                                       
324800         CONTINUE                                                         
324900       ELSE                                                               
325000         MOVE MID-TESTRNOT(2) TO WS-NOT-TESTRNOT(2)                       
325100                              MOD-TESTRNOT-IN(2)                          
325200         MOVE JA            TO NOT-SW                                     
325300       END-IF                                                             
325400                                                                          
325500       IF NOTERINGAR                                                      
325600         MOVE WS-LAGRA-NOT TO KONV-NOT-WDJ122                             
325700         IF SEGMENT-FINNS                                                 
325800           PERFORM IMS-REPL-SATB-K-PCB                                    
325900         ELSE                                                             
326000           MOVE '1' TO KONV-NOT-IDSTRNOT                                  
326100           PERFORM IMS-ISRT-SATB-NOT-K-PCB                                
326200         END-IF                                                           
326300       END-IF                                                             
326400     END-IF                                                               
326500     .                                                                    
326600     EJECT                                                                
326700******************************************************************        
326800**   KONTROLLERAR OM STRUKTUREN INTE FINNS SEDAN FÖRUT OCH DÅ             
326900**   SKAPAR 2234-TRANSAR FÖR ALL RADER ANNARS                             
327000**   JÄMFÖR KONVERTERAT STRUKTURNUMMER MED DET URSPRUNGLIGA OCH           
327100**   SKAPAR 2234-TRANSAR FÖR ÄNDRINGARNA.                                 
327200**   ALLA SEGMENT SOM FINNS I DEN KONVERTERADE STRUKTUREN FLYTTAS         
327300**   IN I DEN URSPRUNGLIGA.                                               
327400******************************************************************        
327500                                                                          
327600 IE-KONV-KLAR SECTION.                                                    
327700                                                                          
327800     PERFORM IE-A-KOLLA-SATS-I-SATS                                       
327900     IF INDATA-OK                                                         
328000       PERFORM IMS-GHU-SATB-STR                                           
328100       IF SEGMENT-SAKNAS                                                  
328200         PERFORM IMS-GHU-SATB-STR-K-PCB                                   
328300         MOVE KONV-STR-WDJ101 TO SATB-STR-WDJ101                          
328400         MOVE W-STRNR       TO SATB-STR-IDARTNR                           
328500         MOVE DAGENS-DATUM  TO SATB-STR-TIREGDAT                          
328600         MOVE DAGENS-DATUM  TO SATB-STR-TIUPPDAT                          
328700         PERFORM IMS-ISRT-SATB-STR                                        
328800         PERFORM S04-FLYTTA-OMNUM-RADER                                   
328900         PERFORM IE-B-SKAPA-2234-TRANSAR                                  
329000         MOVE JA         TO FORP-SW                                       
329100       ELSE                                                               
329200         PERFORM IMS-GET-SATB-RAD                                         
329300         IF SEGMENT-SAKNAS                                                
329400           PERFORM IMS-GHU-SATB-STR-K-PCB                                 
329500           PERFORM IMS-GHU-SATB-STR                                       
329600           MOVE DAGENS-DATUM TO SATB-STR-TIREGDAT                         
329700           MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                         
329800           PERFORM IMS-REPL-SATB                                          
329900           PERFORM S04-FLYTTA-OMNUM-RADER                                 
330000           PERFORM IE-B-SKAPA-2234-TRANSAR                                
330100           MOVE JA         TO FORP-SW                                     
330200         ELSE                                                             
330300           PERFORM IE-C-SKAPA-2234                                        
330400           PERFORM IMS-GHU-SATB-STR                                       
330500           PERFORM IMS-DLET-SATB                                          
330600           PERFORM IMS-GHU-SATB-STR-K-PCB                                 
330700           MOVE SATB-STR-IDUSER TO KONV-STR-IDUSER                        
330800           MOVE SATB-STR-TIREGDAT TO KONV-STR-TIREGDAT                    
330900           MOVE KONV-STR-WDJ101 TO SATB-STR-WDJ101                        
331000           MOVE W-STRNR         TO SATB-STR-IDARTNR                       
331100           MOVE DAGENS-DATUM    TO SATB-STR-TIUPPDAT                      
331200           PERFORM IMS-ISRT-SATB-STR                                      
331300           PERFORM S04-FLYTTA-OMNUM-RADER                                 
331400         END-IF                                                           
331500       END-IF                                                             
331600       IF NY-FORP                                                         
331700         MOVE W-STRNR TO W-IDARTNR                                        
331800         PERFORM IMS-GET-ARTC-01                                          
331900         IF SEGMENT-FINNS                                                 
332000           PERFORM IMS-GHU-SATB-STR                                       
332100           IF SATB-STR-IDSTRTYP = 'S'                                     
332200             IF SATB-STR-TIUPPDAT = ZERO                                  
332300                CONTINUE                                                  
332400             ELSE                                                         
332500                IF SATB-STR-IDLEVNR = '1002 '                             
332600                  MOVE 'J' TO SATB-STR-FLFORPQ                            
332700                  PERFORM IMS-REPL-SATB                                   
332800                ELSE                                                      
332900                  PERFORM IMS-GET-ARTC11                                  
333000                  IF SEGMENT-FINNS                                        
333100                     MOVE CLAG-BEFT TO WS-BEFT-AKTUELL                    
333200                     IF BEFT-AKTUELL                                      
333300                        MOVE 'J' TO SATB-STR-FLFORPQ                      
333400                        PERFORM IMS-REPL-SATB                             
333500                     END-IF                                               
333600                   END-IF                                                 
333700                END-IF                                                    
333800              END-IF                                                      
333900           END-IF                                                         
334000         END-IF                                                           
334100       END-IF                                                             
334200       PERFORM IMS-GHU-SATB-STR-K-PCB                                     
334300       PERFORM IMS-DLET-SATB-K-PCB                                        
334400       MOVE SPACE TO MOD-TEMFSFEL                                         
334500       MOVE NEJ TO KONV-SW                                                
334600     END-IF                                                               
334700     .                                                                    
334800     EJECT                                                                
334900******************************************************************        
335000**   KONTOLLERAR OM SATSEN FINNS I SATSEN                                 
335100******************************************************************        
335200                                                                          
335300 IE-A-KOLLA-SATS-I-SATS SECTION.                                          
335400                                                                          
335500     MOVE W-STRNR TO WS-STRNR-SPAR                                        
335600                                                                          
335700     PERFORM S12-NOLLST-TAB-STR                                           
335800                                                                          
335900     MOVE 1 TO STRIND                                                     
336000                                                                          
336100     PERFORM IMS-GHU-SATB-KONV-STR                                        
336200     IF SEGMENT-FINNS                                                     
336300       PERFORM IMS-GET-SATB-RAD                                           
336400       PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                     
336500         IF SEGMENT-FINNS                                                 
336600           MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                        
336700           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
336800           PERFORM WY2000P1                                               
336900           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
337000             IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'                     
337100               PERFORM HG-BBA-LAGRA-I-TAB-STR                             
337200             END-IF                                                       
337300           END-IF                                                         
337400           IF INDATA-OK                                                   
337500             PERFORM IMS-GET-SATB-RAD                                     
337600           END-IF                                                         
337700         END-IF                                                           
337800       END-PERFORM                                                        
337900                                                                          
338000       IF INDATA-OK                                                       
338100         IF STRIND > 1                                                    
338200           MOVE 1 TO STRIND2                                              
338300           PERFORM UNTIL (STRIND2 > ( STRIND - 1)) OR                     
338400                          INDATA-FEL                                      
338500             MOVE TAB-STR-STRNR(STRIND2) TO W-STRNR                       
338600             PERFORM IMS-GU-SATB-STR                                      
338700             IF SEGMENT-FINNS                                             
338800               PERFORM IMS-GET-SATB-RAD                                   
338900               PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)             
339000                 IF SEGMENT-FINNS                                         
339100                   MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                
339200                   MOVE DAGENS-DATUM        TO TMP2-YYMMDD                
339300                   PERFORM WY2000P1                                       
339400                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
339500                     IF SATB-RAD-IDARTNR = WS-STRNR-SPAR                  
339600                       MOVE NEJ           TO INDATA-SW                    
339700                       MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF               
339800                       MOVE MFS-NUM-FAELT-FEL TO                          
339900                                              MOD-IDARTNR-IN-ATTR         
340000                     ELSE                                                 
340100                       IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'           
340200                         PERFORM HG-BBA-LAGRA-I-TAB-STR                   
340300                       END-IF                                             
340400                     END-IF                                               
340500                   END-IF                                                 
340600                   IF INDATA-OK                                           
340700                     PERFORM IMS-GET-SATB-RAD                             
340800                   END-IF                                                 
340900                 END-IF                                                   
341000               END-PERFORM                                                
341100             END-IF                                                       
341200             ADD 1 TO STRIND2                                             
341300           END-PERFORM                                                    
341400         END-IF                                                           
341500       END-IF                                                             
341600     END-IF                                                               
341700     MOVE WS-STRNR-SPAR TO W-STRNR                                        
341800     .                                                                    
341900     EJECT                                                                
342000******************************************************************        
342100**   SKAPAR 2234-TRANSAR VID ÖVERFLYTTNING AV EN NY STUKTUR FRÅN          
342200**   KONVERTERAT TILL ORDINARIE STRUKTURNUMMER.                           
342300******************************************************************        
342400                                                                          
342500 IE-B-SKAPA-2234-TRANSAR SECTION.                                         
342600     IF WS-STR-IDLEVNR = '1002 '                                          
342700       MOVE '2233'        TO W-IDHTYP                                     
342800       MOVE W-STRNR       TO 2234-IDARTNR-SATS                            
342900       PERFORM IMS-GHU-SATB-STR                                           
343000       PERFORM IMS-GNP-FIRST-SATB-RAD                                     
343100                                                                          
343200       PERFORM UNTIL SATB-STATUS-CODE = 'GE'                              
343300         MOVE SATB-RAD-IDARTNR TO 2234-IDARTNR-ING                        
343400         PERFORM S15-BERAEKNA-PB-SEP-TOT                                  
343500         IF SATB-RAD-IDARTNR NOT = ZERO                                   
343600           MOVE SATB-RAD-IDARTNR  TO W-IDARTNR                            
343700           MOVE 'N'               TO 2234-KDISATS                         
343800           MOVE WS-PB-SEP-TOT     TO 2234-KVPB-SEP-TOT                    
343900           MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-NY                     
344000           MOVE ZERO              TO 2234-REANTPSA-GAMMAL                 
344100           MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                        
344200           PERFORM S10-KONV-TIAAMMDD                                      
344300           MOVE WS-TIAAVV         TO 2234-TIBEHDAT                        
344400           PERFORM IMS-ISRT-2234-TRANS                                    
344500           PERFORM S14-UPPDAT-FLIART                                      
344600           PERFORM IMS-GET-SATB-RAD                                       
344700         END-IF                                                           
344800       END-PERFORM                                                        
344900     END-IF                                                               
345000     .                                                                    
345100     EJECT                                                                
345200******************************************************************        
345300**   SKAPAR 2234-TRANSAR FÖR ÄNDRINGARNA.                                 
345400******************************************************************        
345500                                                                          
345600 IE-C-SKAPA-2234 SECTION.                                                 
345700                                                                          
345800     PERFORM IMS-GHU-SATB-STR-K-PCB                                       
345900                                                                          
346000     IF WS-STR-IDLEVNR = '1002 '                                          
346100       MOVE '2233'        TO W-IDHTYP                                     
346200       MOVE W-STRNR       TO 2234-IDARTNR-SATS                            
346300       MOVE ZERO          TO 2234-KVPB-SEP-TOT                            
346400       MOVE ZERO          TO W-IDRADNR                                    
346500       PERFORM IMS-GET-SATB-RAD-K-PCB                                     
346600                                                                          
346700       PERFORM UNTIL SATB-K-STATUS-CODE = 'GE'                            
346800         IF KONV-RAD-KDSTRRAD = '0'                                       
346900           PERFORM S15-BERAEKNA-PB-SEP-TOT                                
347000           MOVE WS-PB-SEP-TOT     TO 2234-KVPB-SEP-TOT                    
347100           MOVE KONV-RAD-IDARTNR  TO 2234-IDARTNR-ING                     
347200           MOVE KONV-RAD-TIREGDAT   TO TMP1-YYMMDD                        
347300           MOVE WS-TIUPPDAT-GAM     TO TMP2-YYMMDD                        
347400           PERFORM WY2000P1                                               
347500           IF TMP1-YYMMDD >= TMP2-YYMMDD   AND                            
347600              KONV-RAD-KDISATS = 'N'                                      
347700             MOVE 'N'                 TO 2234-KDISATS                     
347800             MOVE KONV-RAD-REANTPSA TO 2234-REANTPSA-NY                   
347900             MOVE ZERO                TO 2234-REANTPSA-GAMMAL             
348000             MOVE KONV-RAD-TISTADAT TO DAT-I-TIDATUM                      
348100             PERFORM S10-KONV-TIAAMMDD                                    
348200             MOVE WS-TIAAVV           TO 2234-TIBEHDAT                    
348300             PERFORM IMS-ISRT-2234-TRANS                                  
348400             MOVE JA                  TO FORP-SW                          
348500             MOVE KONV-RAD-IDARTNR    TO W-IDARTNR                        
348600             PERFORM S14-UPPDAT-FLIART                                    
348700           ELSE                                                           
348800             MOVE KONV-RAD-TISTODAT   TO TMP1-YYMMDD                      
348900             MOVE WS-TIUPPDAT-GAM     TO TMP2-YYMMDD                      
349000             PERFORM WY2000P1                                             
349100             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
349200               MOVE KONV-RAD-TISTODAT   TO TMP1-YYMMDD                    
349300               MOVE +999999             TO TMP2-YYMMDD                    
349400               PERFORM WY2000P1                                           
349500               IF TMP1-YYMMDD < TMP2-YYMMDD                               
349600                 IF KONV-RAD-KDISATS = 'U'                                
349700                   MOVE 'U'             TO 2234-KDISATS                   
349800                   MOVE ZERO            TO 2234-REANTPSA-NY               
349900                   MOVE KONV-RAD-REANTPSA TO 2234-REANTPSA-GAMMAL         
350000                   MOVE KONV-RAD-TISTODAT TO DAT-I-TIDATUM                
350100                   PERFORM S10-KONV-TIAAMMDD                              
350200                   MOVE WS-TIAAVV       TO 2234-TIBEHDAT                  
350300                   MOVE JA              TO FORP-SW                        
350400                   PERFORM IMS-ISRT-2234-TRANS                            
350500                 END-IF                                                   
350600               END-IF                                                     
350700             END-IF                                                       
350800           END-IF                                                         
350900         END-IF                                                           
351000         PERFORM IMS-GET-SATB-RAD-K-PCB                                   
351100       END-PERFORM                                                        
351200     END-IF                                                               
351300     .                                                                    
351400     EJECT                                                                
351500******************************************************************        
351600**   VID UPPDATERING KOLLAS OM STRUKTUR ÄR SPÄRRAD.                       
351700**   RÄKNA UT DET KONVERTERADE STRUKTURNUMRET OCH SE OM DET FINNS.        
351800**   FINNS DET OCH USERID STÄMMER OCH DATUM EJ ÖVERSTIGER 2 DAGAR         
351900**   TAS DETTA. STÄMMER USERID KOLLAS DET OM IDET ÄR SPÄRRAT FRÅN         
352000**   ANNAN BILD. STÄMMER INTE USERID LÄGGS EN SIGNAL UT OCH MAN           
352100**   VISAR DET URSPRUNGLIGA STRUKTURNUMRET. ÖVERSTIGER DATERINGEN         
352200**   2 DAGAR TAS DET KONV. STRUKTURNUMRET BORT.                           
352300******************************************************************        
352400                                                                          
352500 S01-KOLLA-STRNR SECTION.                                                 
352600                                                                          
352700     MOVE SPACE TO STRUKTURNR-TYP-SW                                      
352800                                                                          
352900     MOVE W-STRNR TO W-IDARTNR                                            
353000     PERFORM IMS-GET-ARTC-01                                              
353100     IF SEGMENT-FINNS                                                     
353200       MOVE ART-IDLEVNR TO WS-STR-IDLEVNR-ARTC                            
353300     ELSE                                                                 
353400       MOVE HIGH-VALUE  TO WS-STR-IDLEVNR-ARTC                            
353500     END-IF                                                               
353600     MOVE ZERO    TO W-IDARTNR                                            
353700                                                                          
353800     PERFORM IMS-GU-SATB-STR                                              
353900     IF SEGMENT-FINNS                                                     
354000       MOVE JA                TO STRNR-FINNS-SW                           
354100       MOVE SATB-STR-WDJ101   TO WS-LAGRA-STR                             
354200       MOVE SATB-STR-TIUPPDAT TO WS-TIUPPDAT-GAM                          
354300       IF SATB-STR-TIBORT > 0                                             
354400         MOVE BORTTAGET TO STRUKTURNR-TYP-SW                              
354500       END-IF                                                             
354600     ELSE                                                                 
354700       MOVE ZERO TO WS-TIUPPDAT-GAM                                       
354800     END-IF                                                               
354900                                                                          
355000     COMPUTE W-STRNR-KONV = 999999999 - W-STRNR                           
355100     PERFORM IMS-GHU-SATB-KONV-STR                                        
355200     IF SEGMENT-FINNS                                                     
355300       MOVE 001               TO WORK-KDCALL                              
355400       MOVE WC-CDC-SE         TO WORK-IDDC                                
355500       MOVE SATB-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                        
355600       MOVE DAGENS-DATUM      TO WORK-TIAAMMDD-TOM                        
355700       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                      
355800                          WORK-KDSVAR                                     
355900**     STRUKTUREN TAS BORT OM DEN ÄR ÄLDRE ÄN 2 DAGAR                     
356000**     ELLER OM KVWORKD > 999 = KDSVAR-FEL                                
356100       IF WORK-KVWORKD > 2  OR                                            
356200          WORK-KDSVAR-FEL                                                 
356300         PERFORM IMS-DLET-SATB                                            
356400       ELSE                                                               
356500         IF MSG-SIGNON-USERID = SATB-STR-IDUSER                           
356600           PERFORM S16-KOLLA-OM-USER-HAR-LAASNING                         
356700           IF INDATA-OK                                                   
356800             MOVE SATB-STR-WDJ101 TO WS-LAGRA-STR                         
356900             MOVE FEL16(SPRAK-IX) TO MOD-TEMFSFEL                         
357000             MOVE JA TO KONV-SW                                           
357100           ELSE                                                           
357200***********  STRUKTUR KONVERTERAD VIA BILD 1221                           
357300             MOVE SPAERRAT TO STRUKTURNR-TYP-SW                           
357400           END-IF                                                         
357500         ELSE                                                             
357600           MOVE NEJ      TO INDATA-SW                                     
357700           MOVE SPAERRAT TO STRUKTURNR-TYP-SW                             
357800         END-IF                                                           
357900       END-IF                                                             
358000     END-IF                                                               
358100     .                                                                    
358200     EJECT                                                                
358300******************************************************************        
358400**   ALLA SEGMENT SOM FINNS I DEN URSPRUNGLIGA STRUKTUREN LÄSES           
358500**   MED ORDIN. PCB OCH LÄGGS ÖVER I EN KONVERTERAD STRUKTUR I            
358600**   K-PCB.                                                               
358700******************************************************************        
358800                                                                          
358900 S02-KONV-STRUKTUR SECTION.                                               
359000                                                                          
359100     PERFORM IMS-GU-SATB-STR                                              
359200     MOVE SATB-STR-WDJ101   TO KONV-STR-WDJ101                            
359300     MOVE MSG-SIGNON-USERID TO KONV-STR-IDUSER                            
359400     MOVE DAGENS-DATUM      TO KONV-STR-TIREGDAT                          
359500     MOVE W-STRNR-KONV      TO KONV-STR-IDARTNR                           
359600     PERFORM IMS-ISRT-SATB-STR-K-PCB                                      
359700                                                                          
359800     MOVE ZERO TO W-IDRADNR                                               
359900     PERFORM IMS-GNP-SATB-RAD                                             
360000                                                                          
360100     PERFORM UNTIL SEGMENT-SAKNAS                                         
360200       MOVE SATB-RAD-IDRADNR TO W-IDRADNR                                 
360300       MOVE SATB-RAD-WDJ111  TO KONV-RAD-WDJ111                           
360400       PERFORM IMS-ISRT-SATB-RAD-K-PCB                                    
360500       PERFORM IMS-GNP-SATB-NOT                                           
360600       IF SEGMENT-FINNS                                                   
360700         MOVE SATB-NOT-WDJ122 TO KONV-NOT-WDJ122                          
360800         PERFORM IMS-ISRT-SATB-NOT-K-PCB                                  
360900       END-IF                                                             
361000       PERFORM IMS-GNP-SATB-RAD                                           
361100     END-PERFORM                                                          
361200     MOVE WS-IDRADNR  TO W-IDRADNR                                        
361300                                                                          
361400     PERFORM IMS-GU-SATB-STR-K-PCB                                        
361500     .                                                                    
361600     EJECT                                                                
361700******************************************************************        
361800**   LETA EFTER EN SPECIELL RAD.                                          
361900******************************************************************        
362000                                                                          
362100 S03-LETA-EFTER-RADEN SECTION.                                            
362200                                                                          
362300     MOVE NEJ TO RAD-SW                                                   
362400     MOVE WS-RADNR  TO W-IDRADNR                                          
362500     PERFORM IMS-GNP-FIRST-KVAL-RAD                                       
362600     IF SEGMENT-FINNS                                                     
362700       MOVE JA      TO RAD-SW                                             
362800     END-IF                                                               
362900     .                                                                    
363000     EJECT                                                                
363100******************************************************************        
363200**   ALLA SEGMENT SOM FINNS I DEN KONVERERADE STRUKTUREN LÄSES            
363300**   MED K-PCB OCH LÄGGS ÖVER I DEN ORDINARIE STRUKTUREN I                
363400**   ORDINARIE PCB OCH NUMRERAS OM.                                       
363500******************************************************************        
363600                                                                          
363700 S04-FLYTTA-OMNUM-RADER SECTION.                                          
363800                                                                          
363900     MOVE +0   TO WS-RADNR                                                
364000     PERFORM IMS-GNP-FIRST-SATB-RAD-K-PCB                                 
364100                                                                          
364200     PERFORM UNTIL SEGMENT-SAKNAS                                         
364300         MOVE KONV-RAD-WDJ111 TO SATB-RAD-WDJ111                          
364400         MOVE KONV-RAD-IDRADNR TO WS-RADNR-GAM                            
364500         ADD +10             TO WS-RADNR-NYTT                             
364600         MOVE WS-RADNR-NYTT  TO SATB-RAD-IDRADNR                          
364700                                  W-IDRADNR                               
364800         PERFORM IMS-ISRT-SATB-RAD                                        
364900         MOVE WS-RADNR-GAM   TO W-IDRADNR                                 
365000         PERFORM IMS-GNP-SATB-NOT-K-PCB                                   
365100         IF SEGMENT-FINNS                                                 
365200           MOVE WS-RADNR-NYTT TO W-IDRADNR                                
365300           MOVE KONV-NOT-WDJ122 TO SATB-NOT-WDJ122                        
365400           PERFORM IMS-ISRT-SATB-NOT                                      
365500         END-IF                                                           
365600         PERFORM IMS-GET-SATB-RAD-K-PCB                                   
365700     END-PERFORM                                                          
365800     .                                                                    
365900     EJECT                                                                
366000******************************************************************        
366100**   RÄKNA UT TIFINLV - 5 VECKOR.                                         
366200******************************************************************        
366300                                                                          
366400 S06-SKAPA-DATUM SECTION.                                                 
366500                                                                          
366510     IF MID-IDARTNR NUMERIC                                               
366600       MOVE MID-IDARTNR TO W-IDARTNR                                      
366610     ELSE                                                                 
366611*****MAKES READING IMS-GET-ARTC-01 GET GE IN RETURN-CODE                  
366612*****SO THAT YOU GET 'F' IN WORK-KDSVAR                                   
366620       MOVE ZERO  TO W-IDARTNR                                            
366630     END-IF                                                               
366700                                                                          
366800     PERFORM IMS-GET-ARTC-01                                              
366900     IF SEGMENT-FINNS                                                     
367000       MOVE ART-TIFINLV TO DAT-I-TIDATUM                                  
367100       MOVE 'AAVVD '    TO DAT-KDDATFORM                                  
367200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
367300                            DAT-O-TIDATUM DAT-KDSVAR                      
367400                                                                          
367500       MOVE 003           TO WORK-KDCALL                                  
367600       MOVE DAT-TIAAMMDD  TO WORK-TIAAMMDD-TOM                            
367700       MOVE 26            TO WORK-KVWORKD                                 
367800       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                      
367900                          WORK-KDSVAR                                     
368000     ELSE                                                                 
368100       MOVE 'F'  TO WORK-KDSVAR                                           
368200     END-IF                                                               
368300     .                                                                    
368400     EJECT                                                                
368500******************************************************************        
368600**   KONTROLLERA ÄO OCH AAVV.                                             
368700******************************************************************        
368800                                                                          
368900 S07-KOLLA-AO-AAVV SECTION.                                               
369000                                                                          
369100     MOVE NEJ TO DATUM-KONTROLLERAT                                       
369200     PERFORM S071-KOLLA-NY-STRUKTUR                                       
369300     IF DATUM-KONTROLLERAT = JA                                           
369400        CONTINUE                                                          
369500     ELSE                                                                 
369600        IF MID-TIAAVV = ALL '+' OR ZERO                                   
369700          PERFORM S06-SKAPA-DATUM                                         
369800          IF WORK-KDSVAR-OK                                               
369900             MOVE WORK-TIAAMMDD-FOM    TO TMP1-YYMMDD                     
370000             MOVE INNEVARANDE-VECKA   TO TMP2-YYMMDD                      
370100             PERFORM WY2000P1                                             
370200             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
370300                MOVE DAGENS-DATUM      TO AKTUELLT-DATUM                  
370400             ELSE                                                         
370500                MOVE WORK-TIAAMMDD-FOM  TO AKTUELLT-DATUM                 
370600             END-IF                                                       
370700             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR               
370800          ELSE                                                            
370900             MOVE DAGENS-DATUM TO AKTUELLT-DATUM                          
371000          END-IF                                                          
371100        ELSE                                                              
371200          PERFORM S13-KONV-TIAAVV                                         
371300          MOVE AKTUELLT-DATUM    TO TMP1-YYMMDD                           
371400          MOVE INNEVARANDE-VECKA TO TMP2-YYMMDD                           
371500          PERFORM WY2000P1                                                
371600          IF DAT-KDSVAR-FEL OR                                            
371700            TMP1-YYMMDD < TMP2-YYMMDD                                     
371800            IF EJ-TID-SIGNAL                                              
371900               MOVE MED24(SPRAK-IX) TO MOD-TEMFSINF                       
372000            END-IF                                                        
372100            MOVE MFS-NUM-FAELT-FEL    TO MOD-TIAAVV-IN-ATTR               
372200            MOVE NEJ TO INDATA-SW                                         
372300          ELSE                                                            
372400            MOVE AKTUELLT-DATUM   TO TMP1-YYMMDD                          
372500            MOVE DAGENS-DATUM     TO TMP2-YYMMDD                          
372600            PERFORM WY2000P1                                              
372700            IF TMP1-YYMMDD < TMP2-YYMMDD                                  
372800              MOVE DAGENS-DATUM TO AKTUELLT-DATUM                         
372900            END-IF                                                        
373000            MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR                
373100          END-IF                                                          
373200        END-IF                                                            
373300     END-IF                                                               
373400                                                                          
373500     IF MID-IDAO = ALL '+' OR SPACE                                       
373600       MOVE SPACE                TO MID-IDAO                              
373700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                      
373800     ELSE                                                                 
373900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                      
374000       IF MID-TIAAVV = ALL '+'                                            
374100         IF EJ-TID-SIGNAL                                                 
374200           MOVE MED25(SPRAK-IX) TO MOD-TEMFSINF                           
374300         END-IF                                                           
374400         MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR                     
374500         MOVE NEJ TO INDATA-SW                                            
374600       END-IF                                                             
374700     END-IF                                                               
374800     .                                                                    
374900     EJECT                                                                
375000 S071-KOLLA-NY-STRUKTUR SECTION.                                          
375100******************************************************************        
375200*  OM STRUKTUREN ÄR NY OCH FÖRSTA INLEVERANS LIGGER FRAMÅT I TIDEN        
375300*  SÄTTS RADERNAS TISTADAT TILL DAGENS-DATUM OM INTE INMATAD DATUM        
375400*  ÄR > FÖRSTA INLEVERANS PÅ STRUKTUREN.                                  
375500******************************************************************        
375600*                                                                         
375700     IF WS-TIUPPDAT-GAM = ZERO                                            
375800        MOVE WS-STRNR TO W-STRNR                                          
375900        PERFORM IMS-GU-ARTC01                                             
376000        IF SEGMENT-FINNS                                                  
376100           MOVE ART-TIFINLV    TO DAT-I-TIDATUM                           
376200           MOVE 'AAVVD '       TO DAT-KDDATFORM                           
376300           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
376400                               DAT-O-TIDATUM DAT-KDSVAR                   
376500           MOVE DAT-TIAAMMDD   TO WS-SPAR-TIFINLV                         
376600           MOVE WS-SPAR-TIFINLV   TO TMP1-YYMMDD                          
376700           MOVE DAGENS-DATUM      TO TMP2-YYMMDD                          
376800           PERFORM WY2000P1                                               
376900           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
377000              IF MID-TIAAVV = ALL '+' OR ZERO                             
377100                 MOVE DAGENS-DATUM TO AKTUELLT-DATUM                      
377200                 MOVE JA TO DATUM-KONTROLLERAT                            
377300              ELSE                                                        
377400                 MOVE MID-TIAAVV     TO DAT-I-TIDATUM                     
377500                 MOVE 'AAVV  '       TO DAT-KDDATFORM                     
377600                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
377700                                     DAT-O-TIDATUM DAT-KDSVAR             
377800                 IF DAT-KDSVAR-OK                                         
377900                   MOVE DAT-TIAAMMDD      TO TMP1-YYMMDD                  
378000                   MOVE WS-SPAR-TIFINLV   TO TMP2-YYMMDD                  
378100                   PERFORM WY2000P1                                       
378200                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
378300                      MOVE DAT-TIAAMMDD TO AKTUELLT-DATUM                 
378400                   ELSE                                                   
378500                      MOVE DAGENS-DATUM TO AKTUELLT-DATUM                 
378600                   END-IF                                                 
378700                   MOVE JA TO DATUM-KONTROLLERAT                          
378800                   MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR         
378900                 ELSE                                                     
379000                   IF EJ-TID-SIGNAL                                       
379100                      MOVE MED24(SPRAK-IX) TO MOD-TEMFSINF                
379200                   END-IF                                                 
379300                   MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR           
379400                   MOVE NEJ TO INDATA-SW                                  
379500                 END-IF                                                   
379600              END-IF                                                      
379700            END-IF                                                        
379800         END-IF                                                           
379900     END-IF                                                               
380000     .                                                                    
380100     EJECT                                                                
380200******************************************************************        
380300**   ALLA SEGMENT SOM FINNS UNDER RADEN LÄSES MED ORDINARIE PCB           
380400**   LÄGGS ÖVER TILL DET NYA RADNUMRET I K-PCB.                           
380500******************************************************************        
380600                                                                          
380700 S09-FLYTTA-BEH-NOT SECTION.                                              
380800                                                                          
380900     PERFORM IMS-GU-SATB-STR-K-PCB                                        
381000     PERFORM IMS-GHU-SATB-KONV-STR                                        
381100                                                                          
381200     MOVE WS-IDRADNR         TO W-IDRADNR                                 
381300     PERFORM IMS-GNP-SATB-NOT                                             
381400     IF SEGMENT-FINNS                                                     
381500       MOVE SATB-NOT-WDJ122  TO KONV-NOT-WDJ122                           
381600       MOVE WS-RAD-IDRADNR   TO W-IDRADNR                                 
381700       PERFORM IMS-ISRT-SATB-NOT-K-PCB                                    
381800     ELSE                                                                 
381900       MOVE WS-RAD-IDRADNR   TO W-IDRADNR                                 
382000     END-IF                                                               
382100     .                                                                    
382200     EJECT                                                                
382300******************************************************************        
382400**   KONVERTERAR DATUM FRÅN AAMMDD TILL AAVV.                             
382500******************************************************************        
382600                                                                          
382700 S10-KONV-TIAAMMDD SECTION.                                               
382800                                                                          
382900     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
383000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
383100                         DAT-O-TIDATUM DAT-KDSVAR                         
383200     MOVE DAT-TIAA-VECKA           TO WS-TIAA                             
383300     MOVE DAT-TIVV                 TO WS-TIVV                             
383400                                                                          
383500     .                                                                    
383600     EJECT                                                                
383700******************************************************************        
383800**   KONTROLL OM USER-ID REDAN HAR ETT KONVERTERAT STRUKTURNR.            
383900******************************************************************        
384000                                                                          
384100 S11-HAR-USERID-KONV-STR SECTION.                                         
384200                                                                          
384300     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
384400                                                                          
384500     PERFORM IMS-GHN-SATB-KONV-IDUSER-STR                                 
384600     PERFORM UNTIL SEGMENT-SAKNAS                                         
384700       IF SEGMENT-FINNS                                                   
384800         MOVE 001               TO WORK-KDCALL                            
384900         MOVE WC-CDC-SE         TO WORK-IDDC                              
385000         MOVE SATB-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                      
385100         MOVE DAGENS-DATUM      TO WORK-TIAAMMDD-TOM                      
385200         CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                    
385300                             WORK-KDSVAR                                  
385400         IF WORK-KVWORKD > 2                                              
385500           MOVE W-STRNR          TO WS-STRNR-SPAR                         
385600           MOVE SATB-STR-IDARTNR TO W-STRNR                               
385700           PERFORM IMS-GHU-SATB-STR                                       
385800           PERFORM IMS-DLET-SATB                                          
385900           MOVE WS-STRNR-SPAR    TO W-STRNR                               
386000           PERFORM IMS-GHN-SATB-KONV-IDUSER-STR                           
386100         ELSE                                                             
386200           IF SATB-STR-IDARTNR NOT = W-STRNR-KONV                         
386300             MOVE FEL17(SPRAK-IX) TO MOD-TEMFSFEL                         
386400             MOVE 'GE'          TO STATUS-WS                              
386500             MOVE NEJ           TO INDATA-SW                              
386600           ELSE                                                           
386700             PERFORM IMS-GHN-SATB-KONV-IDUSER-STR                         
386800           END-IF                                                         
386900         END-IF                                                           
387000       END-IF                                                             
387100     END-PERFORM                                                          
387200     .                                                                    
387300     EJECT                                                                
387400******************************************************************        
387500**   NOLLSTÄLLNING AV STRUKTURTABELLEN.                                   
387600******************************************************************        
387700                                                                          
387800 S12-NOLLST-TAB-STR SECTION.                                              
387900                                                                          
388000     MOVE +1 TO STRIND                                                    
388100     PERFORM UNTIL STRIND > TAB-STR-MAX                                   
388200       MOVE +0 TO TAB-STR-STRNR(STRIND)                                   
388300       ADD +1  TO STRIND                                                  
388400     END-PERFORM                                                          
388500     .                                                                    
388600     EJECT                                                                
388700******************************************************************        
388800**   KONVERTERA AAVV TILL AAMMDD.                                         
388900******************************************************************        
389000                                                                          
389100 S13-KONV-TIAAVV SECTION.                                                 
389200                                                                          
389300     MOVE MID-TIAAVV      TO DAT-I-TIDATUM                                
389400     MOVE 'AAVV  '        TO DAT-KDDATFORM                                
389500     CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                     
389600                          DAT-O-TIDATUM DAT-KDSVAR                        
389700     MOVE DAT-TIAAMMDD    TO AKTUELLT-DATUM                               
389800     .                                                                    
389900     EJECT                                                                
390000******************************************************************        
390100**   ÄNDRAR FLIART TILL 'J' OM DEN INTE REDAN ÄR DET.                     
390200******************************************************************        
390300                                                                          
390400 S14-UPPDAT-FLIART SECTION.                                               
390500                                                                          
390600     PERFORM IMS-GHU-ARTC-01                                              
390700                                                                          
390800     IF SEGMENT-FINNS                                                     
390900       IF ART-FLIART = 'N'                                                
391000         MOVE JA TO ART-FLIART                                            
391100         PERFORM IMS-REPL-ARTC                                            
391200       END-IF                                                             
391300     END-IF                                                               
391400     .                                                                    
391500     EJECT                                                                
391600******************************************************************        
391700**   BERÄKNA PB-SEP TOTALT C1                                             
391800******************************************************************        
391900                                                                          
392000 S15-BERAEKNA-PB-SEP-TOT SECTION.                                         
392100                                                                          
392200     MOVE ZERO TO WS-PB-SEP-TOT                                           
392300                                                                          
392400****************                                                          
392500*    MOVE KONV-RAD-IDARTNR TO W-IDARTNR                                   
392600*    PERFORM IMS-GET-ARTC-01                                              
392700*                                                                         
392800*    IF SEGMENT-FINNS                                                     
392900*      PERFORM IMS-GET-ARTC11                                             
393000*      IF SEGMENT-FINNS                                                   
393100*        ADD CLAG-KVPB-SEP TO WS-PB-SEP-TOT                               
393200*      END-IF                                                             
393300*    END-IF                                                               
393400****************                                                          
393500     .                                                                    
393600     EJECT                                                                
393700 S16-KOLLA-OM-USER-HAR-LAASNING SECTION.                                  
393800                                                                          
393900     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
394000     MOVE '1151'            TO W-IDHTYPX                                  
394100     PERFORM IMS-GET-XXAZ01                                               
394200                                                                          
394300     PERFORM IMS-GHNP-XXAZ11                                              
394400     PERFORM UNTIL SEGMENT-SAKNAS                                         
394500       IF SEGMENT-FINNS                                                   
394600         MOVE 001                TO WORK-KDCALL                           
394700         MOVE WC-CDC-SE          TO WORK-IDDC                             
394800         MOVE XXAZ-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                     
394900         MOVE DAGENS-DATUM       TO WORK-TIAAMMDD-TOM                     
395000         CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                    
395100                             WORK-KDSVAR                                  
395200         IF WORK-KVWORKD > 2 OR                                           
395300            WORK-KDSVAR-FEL                                               
395400           PERFORM IMS-DLET-XXAZ                                          
395500           PERFORM IMS-GHNP-XXAZ11                                        
395600         ELSE                                                             
395700           MOVE 'GE' TO STATUS-WS                                         
395800           MOVE NEJ    TO INDATA-SW                                       
395900         END-IF                                                           
396000       END-IF                                                             
396100     END-PERFORM                                                          
396200     .                                                                    
396300     EJECT                                                                
396400 S17-KOLLA-BEART-KDHOM-AENDRING SECTION.                                  
396500                                                                          
396600     IF (MID-KDBENHOM = ALL '+') AND                                      
396700          (MID-BEART = ALL '+')                                           
396800       CONTINUE                                                           
396900     ELSE                                                                 
397000       IF (MID-KDBENHOM    NOT = ALL '+') AND                             
397100            (MID-BEART    NOT = ALL '+')                                  
397200*******  BEART + KDBENHOM IFYLLDA                                         
397300         IF MID-KDBENHOM    NUMERIC                                       
397400           MOVE WS-IDSKYLT    TO W-IDSKYLT                                
397500           MOVE MID-BEART     TO W-BEART                                  
397600           MOVE MID-KDBENHOM  TO WS-KDBENHOM-NUM                          
397700         ELSE                                                             
397800           MOVE NEJ               TO INDATA-SW                            
397900           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR                 
398000         END-IF                                                           
398100       ELSE                                                               
398200                                                                          
398300         IF (MID-KDBENHOM    NOT = ALL '+')                               
398400*******    BARA KDBENHOM IFYLLD                                           
398500           IF MID-KDBENHOM    NUMERIC                                     
398600             MOVE 'S   '           TO W-IDSKYLT                           
398700             MOVE WS-RAD-BEART-SVE TO W-BEART                             
398800             MOVE MID-KDBENHOM     TO WS-KDBENHOM-NUM                     
398900           ELSE                                                           
399000             MOVE NEJ               TO INDATA-SW                          
399100             MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR               
399200           END-IF                                                         
399300         ELSE                                                             
399400*******    BARA BEART IFYLLD                                              
399500           MOVE WS-IDSKYLT         TO W-IDSKYLT                           
399600           MOVE MID-BEART          TO W-BEART                             
399700           MOVE WS-RAD-KDBENHOM    TO WS-KDBENHOM-NUM                     
399800           END-IF                                                         
399900       END-IF                                                             
400000                                                                          
400100       IF INDATA-OK                                                       
400200         PERFORM IMS-GET-BENA-ASEQ                                        
400300         IF SEGMENT-FINNS                                                 
400400           IF BENA-BEN-KDBENSTAT < 2                                      
400500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-IN-ATTR               
400600           ELSE                                                           
400700             MOVE NEJ                TO INDATA-SW                         
400800             MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                 
400900           END-IF                                                         
401000         ELSE                                                             
401100           MOVE MED3(SPRAK-IX) TO MOD-TEMFSINF                            
401200           MOVE NEJ                TO INDATA-SW                           
401300           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                   
401400         END-IF                                                           
401500                                                                          
401600         IF INDATA-OK                                                     
401700           IF WS-IDSKYLT = 'S  '                                          
401800             IF (MID-BEART    NOT = ALL '+') AND                          
401900                 (MID-KDBENHOM    = ALL '+')                              
402000               PERFORM IMS-GN-BENA-ASEQ                                   
402100               IF SEGMENT-FINNS                                           
402200*************    BENÄMNING FINNS MED FLERA HOMONYMKODER                   
402300*************    HOMONYMKOD MÅSTE ANGES                                   
402400                 MOVE MED49(SPRAK-IX) TO MOD-TEMFSINF                     
402500                 MOVE NEJ               TO INDATA-SW                      
402600                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR           
402700               ELSE                                                       
402800                 MOVE MFS-NUM-FAELT-RAETT TO MOD-BEART-IN-ATTR            
402900               END-IF                                                     
403000             END-IF                                                       
403100           END-IF                                                         
403200         END-IF                                                           
403300                                                                          
403400         IF INDATA-OK                                                     
403500           PERFORM IMS-GET-BENA-ASEQ                                      
403600           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
403700                           (BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM)         
403800             IF SEGMENT-FINNS                                             
403900               IF BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM                    
404000                 CONTINUE                                                 
404100               ELSE                                                       
404200                 PERFORM IMS-GN-BENA-ASEQ                                 
404300               END-IF                                                     
404400             END-IF                                                       
404500           END-PERFORM                                                    
404600                                                                          
404700           IF SEGMENT-FINNS                                               
404800             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-IN-ATTR             
404900             MOVE WS-KDBENHOM-NUM     TO MOD-KDBENHOM-UT                  
405000           ELSE                                                           
405100             MOVE NEJ               TO INDATA-SW                          
405200             MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR               
405300           END-IF                                                         
405400         END-IF                                                           
405500                                                                          
405600         IF INDATA-OK                                                     
405700           MOVE WS-IDSKYLT     TO W-IDSKYLT                               
405800           PERFORM IMS-GET-BENA-A-TEXT                                    
405900           MOVE BENA-TEXT-BEART TO MOD-BEART-UT                           
406000*******************************************                               
406100*   SPARA UNDAN SVENSK ARTIKELBENÄMNING   *                               
406200*******************************************                               
406300           IF WS-IDSKYLT = 'S  '                                          
406400             MOVE MID-BEART    TO WS-BEART                                
406500           ELSE                                                           
406600             MOVE 'S  ' TO W-IDSKYLT                                      
406700             PERFORM IMS-GET-BENA-A-TEXT                                  
406800             MOVE BENA-TEXT-BEART TO WS-BEART                             
406900           END-IF                                                         
407000         END-IF                                                           
407100       END-IF                                                             
407200                                                                          
407300     END-IF                                                               
407400     .                                                                    
407500     EJECT                                                                
407600 S18-KOLLA-BEART-KDHOM-NYUPPL SECTION.                                    
407700                                                                          
407800     IF (MID-BEART    = ALL '+')                                          
407900       MOVE NEJ                TO INDATA-SW                               
408000       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-IN-ATTR                       
408100                                                                          
408200       IF (MID-KDBENHOM    = ALL '+')                                     
408300         CONTINUE                                                         
408400       ELSE                                                               
408500         IF MID-KDBENHOM    NUMERIC                                       
408600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-IN-ATTR               
408700         ELSE                                                             
408800           MOVE NEJ                 TO INDATA-SW                          
408900           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR               
409000         END-IF                                                           
409100       END-IF                                                             
409200     ELSE                                                                 
409300       MOVE WS-IDSKYLT   TO W-IDSKYLT                                     
409400       MOVE MID-BEART    TO W-BEART                                       
409500       PERFORM IMS-GET-BENA-ASEQ                                          
409600       IF SEGMENT-FINNS                                                   
409700         IF BENA-BEN-KDBENSTAT < 2                                        
409800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-IN-ATTR                 
409900                                                                          
410000           IF MID-KDBENHOM    = ALL '+'                                   
410100             PERFORM IMS-GN-BENA-ASEQ                                     
410200             IF SEGMENT-FINNS                                             
410300************** BENÄMNING FINNS MED FLERA HOMONYMKODER                     
410400************** HOMONYMKOD MÅSTE ANGES                                     
410500               MOVE NEJ                 TO INDATA-SW                      
410600               MOVE MFS-NUM-FAELT-FEL   TO MOD-KDBENHOM-IN-ATTR           
410700             ELSE                                                         
410800               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBENHOM-IN-ATTR           
410900               MOVE BENA-BEN-KDHOMONYM  TO MID-KDBENHOM                   
411000                                           MOD-KDBENHOM-UT                
411100               PERFORM IMS-GET-BENA-ASEQ                                  
411200             END-IF                                                       
411300           ELSE                                                           
411400             IF MID-KDBENHOM    NUMERIC                                   
411500               MOVE MID-KDBENHOM    TO WS-KDBENHOM-NUM                    
411600               PERFORM UNTIL (SEGMENT-SAKNAS) OR                          
411700                     (BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM)               
411800                                                                          
411900                 IF SEGMENT-FINNS                                         
412000                   IF BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM                
412100                     CONTINUE                                             
412200                   ELSE                                                   
412300                     PERFORM IMS-GN-BENA-ASEQ                             
412400                   END-IF                                                 
412500                 END-IF                                                   
412600               END-PERFORM                                                
412700                                                                          
412800               IF SEGMENT-FINNS                                           
412900                 MOVE MFS-NUM-FAELT-RAETT TO                              
413000                                           MOD-KDBENHOM-IN-ATTR           
413100                 MOVE WS-KDBENHOM-NUM     TO MOD-KDBENHOM-UT              
413200               ELSE                                                       
413300                 MOVE NEJ               TO INDATA-SW                      
413400                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR           
413500               END-IF                                                     
413600             ELSE                                                         
413700               MOVE NEJ               TO INDATA-SW                        
413800               MOVE MFS-NUM-FAELT-FEL TO MOD-KDBENHOM-IN-ATTR             
413900             END-IF                                                       
414000           END-IF                                                         
414100                                                                          
414200           IF INDATA-OK                                                   
414300*******************************************                               
414400* SPARA UNDAN SVENSK ARTIKELBENÄMNING     *                               
414500*******************************************                               
414600             IF WS-IDSKYLT = 'S  '                                        
414700               MOVE MID-BEART    TO WS-BEART                              
414800             ELSE                                                         
414900               MOVE 'S  ' TO W-IDSKYLT                                    
415000               PERFORM IMS-GET-BENA-A-TEXT                                
415100               MOVE BENA-TEXT-BEART TO WS-BEART                           
415200             END-IF                                                       
415300             MOVE MID-BEART         TO MOD-BEART-UT                       
415400           END-IF                                                         
415500         ELSE                                                             
415600           MOVE NEJ                   TO INDATA-SW                        
415700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-BEART-IN-ATTR                
415800           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-IN-ATTR             
415900         END-IF                                                           
416000       ELSE                                                               
416100         MOVE NEJ                   TO INDATA-SW                          
416200         MOVE MFS-ALFA-FAELT-FEL    TO MOD-BEART-IN-ATTR                  
416300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBENHOM-IN-ATTR               
416400       END-IF                                                             
416500     END-IF                                                               
416600     .                                                                    
416700     EJECT                                                                
416800 MFS-RENSA-FAELT-UT SECTION.                                              
416900                                                                          
417000*    --- ALLA UTDATA-FÄLT                                                 
417100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
417200                             MOD-IDLEVNR-UT                               
417300                             MOD-BELEVART-UT                              
417400                             MOD-BEART-UT                                 
417500                             MOD-KDBENHOM-UT                              
417600                             MOD-IDLEVNR                                  
417700                             MOD-BELEVART                                 
417800                             MOD-TIREGDAT                                 
417900                             MOD-REANTPSA-UT                              
418000                             MOD-IDSTRTYP-UT                              
418100                             MOD-KDSORT-UT                                
418200                             MOD-TESTRNOT-IN(1)                           
418300                             MOD-TESTRNOT-IN(2)                           
418400     .                                                                    
418500     SKIP2                                                                
418600 MFS-RENSA-FAELT-IN SECTION.                                              
418700                                                                          
418800*    --- ALLA INDATA-FÄLT                                                 
418900     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-K                                
419000                             MOD-IDRADNR-F                                
419100                             MOD-IDARTNR-IN                               
419200                             MOD-IDLEVNR-IN                               
419300                             MOD-BELEVART-IN                              
419400                             MOD-BEART-IN                                 
419500                             MOD-KDBENHOM-IN                              
419600                             MOD-REANTPSA-IN                              
419700                             MOD-IDSTRTYP-IN                              
419800                             MOD-KDSORT-IN                                
419900                             MOD-IDAO-IN                                  
420000                             MOD-TIAAVV-IN                                
420100                             MOD-KLAR-IN                                  
420200                             MOD-BORT-IN                                  
420300     .                                                                    
420400     EJECT                                                                
420500 MFS-ROER-EJ-FAELT-UT SECTION.                                            
420600                                                                          
420700*    --- ALLA UTDATA-FÄLT                                                 
420800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
420900                               MOD-IDLEVNR-UT                             
421000                               MOD-BELEVART-UT                            
421100                               MOD-BEART-UT                               
421200                               MOD-KDBENHOM-UT                            
421300                               MOD-IDLEVNR                                
421400                               MOD-BELEVART                               
421500                               MOD-TIREGDAT                               
421600                               MOD-REANTPSA-UT                            
421700                               MOD-IDSTRTYP-UT                            
421800                               MOD-KDSORT-UT                              
421900                               MOD-TESTRNOT-IN(1)                         
422000                               MOD-TESTRNOT-IN(2)                         
422100     .                                                                    
422200     SKIP2                                                                
422300 MFS-ROER-EJ-FAELT-IN SECTION.                                            
422400                                                                          
422500*    --- ALLA INDATA-FÄLT                                                 
422600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-K                              
422700                               MOD-IDRADNR-F                              
422800                               MOD-IDARTNR-IN                             
422900                               MOD-IDLEVNR-IN                             
423000                               MOD-BELEVART-IN                            
423100                               MOD-BEART-IN                               
423200                               MOD-KDBENHOM-IN                            
423300                               MOD-REANTPSA-IN                            
423400                               MOD-IDSTRTYP-IN                            
423500                               MOD-KDSORT-IN                              
423600                               MOD-IDAO-IN                                
423700                               MOD-TIAAVV-IN                              
423800                               MOD-KLAR-IN                                
423900                               MOD-BORT-IN                                
424000     .                                                                    
424100     EJECT                                                                
424200 MFS-FORM-ATTR SECTION.                                                   
424300                                                                          
424400*    --- ALLA INDATA-FÄLT                                                 
424500     MOVE MFS-FORMATETS-ATTR TO  MOD-IDRADNR-K-ATTR                       
424600                                 MOD-IDRADNR-F-ATTR                       
424700                                 MOD-IDARTNR-IN-ATTR                      
424800                                 MOD-IDLEVNR-IN-ATTR                      
424900                                 MOD-BELEVART-IN-ATTR                     
425000                                 MOD-BEART-IN-ATTR                        
425100                                 MOD-KDBENHOM-IN-ATTR                     
425200                                 MOD-REANTPSA-IN-ATTR                     
425300                                 MOD-IDSTRTYP-IN-ATTR                     
425400                                 MOD-KDSORT-IN-ATTR                       
425500                                 MOD-IDAO-IN-ATTR                         
425600                                 MOD-TIAAVV-IN-ATTR                       
425700                                 MOD-KLAR-IN-ATTR                         
425800                                 MOD-BORT-IN-ATTR                         
425900                                 MOD-TESTRNOT-IN-ATTR(1)                  
426000                                 MOD-TESTRNOT-IN-ATTR(2)                  
426100     .                                                                    
426200     SKIP2                                                                
426300 MFS-LAS-IN-IGEN SECTION.                                                 
426400                                                                          
426500*    --- ALLA INDATA-FÄLT                                                 
426600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-K-ATTR                     
426700                                   MOD-IDRADNR-F-ATTR                     
426800                                   MOD-IDARTNR-IN-ATTR                    
426900                                   MOD-IDLEVNR-IN-ATTR                    
427000                                   MOD-BELEVART-IN-ATTR                   
427100                                   MOD-BEART-IN-ATTR                      
427200                                   MOD-KDBENHOM-IN-ATTR                   
427300                                   MOD-REANTPSA-IN-ATTR                   
427400                                   MOD-IDSTRTYP-IN-ATTR                   
427500                                   MOD-KDSORT-IN-ATTR                     
427600                                   MOD-IDAO-IN-ATTR                       
427700                                   MOD-TIAAVV-IN-ATTR                     
427800                                   MOD-KLAR-IN-ATTR                       
427900                                   MOD-BORT-IN-ATTR                       
428000                                   MOD-TESTRNOT-IN-ATTR(1)                
428100                                   MOD-TESTRNOT-IN-ATTR(2)                
428200     .                                                                    
428300     EJECT                                                                
428400* --- IMS SEKTIONER ---                                                   
428500     SKIP3                                                                
428600 IMS-GET-MSG SECTION.                                                     
428700                                                                          
428800     MOVE '  QC' TO GODK-STATUSKODER                                      
428900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
429000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
429100     PERFORM IMS-STATUSKONTROLL                                           
429200     .                                                                    
429300     SKIP3                                                                
429400 IMS-INSERT-MSG SECTION.                                                  
429500                                                                          
429600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
429700       MOVE '0' TO MFS-KDHUVOMR                                           
429800     END-IF                                                               
429900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
430000     MOVE SPACE TO GODK-STATUSKODER                                       
430100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
430200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
430300     PERFORM IMS-STATUSKONTROLL                                           
430400     .                                                                    
430500     EJECT                                                                
430600 IMS-GET-BENA-ASEQ SECTION.                                               
430700                                                                          
430800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
430900                                  W-BEART-X ')'                           
431000          DELIMITED BY SIZE INTO SSA1                                     
431100     MOVE '  GE' TO GODK-STATUSKODER                                      
431200     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
431300     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
431400     PERFORM IMS-STATUSKONTROLL                                           
431500     .                                                                    
431600     SKIP3                                                                
431700 IMS-GN-BENA-ASEQ SECTION.                                                
431800                                                                          
431900     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
432000                                  W-BEART-X ')'                           
432100          DELIMITED BY SIZE INTO SSA1                                     
432200     MOVE '  GE' TO GODK-STATUSKODER                                      
432300     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
432400     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
432500     PERFORM IMS-STATUSKONTROLL                                           
432600     .                                                                    
432700     EJECT                                                                
432800 IMS-GET-BENA-BSEQ SECTION.                                               
432900                                                                          
433000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
433100          DELIMITED BY SIZE INTO SSA1                                     
433200     MOVE '  GE' TO GODK-STATUSKODER                                      
433300     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
433400     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
433500     PERFORM IMS-STATUSKONTROLL                                           
433600     .                                                                    
433700     SKIP3                                                                
433800 IMS-GET-BENA-A-TEXT SECTION.                                             
433900                                                                          
434000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
434100          DELIMITED BY SIZE INTO SSA1                                     
434200     MOVE '  GE' TO GODK-STATUSKODER                                      
434300     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
434400     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
434500     PERFORM IMS-STATUSKONTROLL                                           
434600     .                                                                    
434700     SKIP3                                                                
434800 IMS-GET-BENA-B-TEXT SECTION.                                             
434900                                                                          
435000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
435100          DELIMITED BY SIZE INTO SSA1                                     
435200     MOVE '  GE' TO GODK-STATUSKODER                                      
435300     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
435400     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
435500     PERFORM IMS-STATUSKONTROLL                                           
435600     .                                                                    
435700     EJECT                                                                
435800 IMS-GET-ARTC-01 SECTION.                                                 
435900                                                                          
436000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
436100          DELIMITED BY SIZE INTO SSA1                                     
436200     MOVE '  GE' TO GODK-STATUSKODER                                      
436300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC SSA1                 
436400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
436500     PERFORM IMS-STATUSKONTROLL                                           
436600     .                                                                    
436700     SKIP3                                                                
436800 IMS-GU-ARTC01 SECTION.                                                   
436900                                                                          
437000     STRING 'WLARTC01(IDARTNR  =' W-STRNR-X ')'                           
437100          DELIMITED BY SIZE INTO SSA1                                     
437200     MOVE '  GE' TO GODK-STATUSKODER                                      
437300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC SSA1                 
437400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
437500     PERFORM IMS-STATUSKONTROLL                                           
437600     .                                                                    
437700     SKIP3                                                                
437800 IMS-GHU-ARTC-01 SECTION.                                                 
437900                                                                          
438000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
438100          DELIMITED BY SIZE INTO SSA1                                     
438200     MOVE '  GE' TO GODK-STATUSKODER                                      
438300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-ARTC SSA1                
438400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
438500     PERFORM IMS-STATUSKONTROLL                                           
438600     .                                                                    
438700     EJECT                                                                
438800 IMS-GET-ARTC11 SECTION.                                                  
438900                                                                          
439000     MOVE 'WLARTC11 ' TO SSA1                                             
439100     MOVE '  GE' TO GODK-STATUSKODER                                      
439200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC SSA1                
439300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
439400     PERFORM IMS-STATUSKONTROLL                                           
439500     .                                                                    
439600     EJECT                                                                
439700 IMS-GET-WDF501 SECTION.                                                  
439800                                                                          
439900     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
440000          DELIMITED BY SIZE INTO SSA1                                     
440100     MOVE '  GE' TO GODK-STATUSKODER                                      
440200     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
440300     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
440400     PERFORM IMS-STATUSKONTROLL                                           
440500     .                                                                    
440600     SKIP3                                                                
440700 IMS-GET-WDF502-LAST SECTION.                                             
440800                                                                          
440900     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
441000          DELIMITED BY SIZE INTO SSA1                                     
441100     MOVE '  GE' TO GODK-STATUSKODER                                      
441200     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
441300     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
441400     PERFORM IMS-STATUSKONTROLL                                           
441500     .                                                                    
441600     SKIP3                                                                
441700 IMS-GET-WDF502-OKVAL SECTION.                                            
441800                                                                          
441900     MOVE 'WDF502  ' TO SSA1                                              
442000     MOVE '  GE' TO GODK-STATUSKODER                                      
442100     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
442200     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
442300     PERFORM IMS-STATUSKONTROLL                                           
442400     .                                                                    
442500     EJECT                                                                
442600 IMS-GET-WDF5-ASEQ SECTION.                                               
442700                                                                          
442800     STRING 'WDF501  (WDF5ASEQ =' W-WDF5ASEQ-X ')'                        
442900          DELIMITED BY SIZE INTO SSA1                                     
443000     MOVE '  GE' TO GODK-STATUSKODER                                      
443100     CALL CBLTDLI USING GU WDF5-A-PCB DLI-IO-AREA SSA1                    
443200     MOVE WDF5-A-STATUS-CODE TO STATUS-WS                                 
443300     PERFORM IMS-STATUSKONTROLL                                           
443400     .                                                                    
443500     SKIP3                                                                
443600 IMS-GN-WDF5-ASEQ SECTION.                                                
443700                                                                          
443800     STRING 'WDF501  (WDF5ASEQ =' W-WDF5ASEQ-X ')'                        
443900          DELIMITED BY SIZE INTO SSA1                                     
444000     MOVE '  GE' TO GODK-STATUSKODER                                      
444100     CALL CBLTDLI USING GN WDF5-A-PCB DLI-IO-AREA SSA1                    
444200     MOVE WDF5-A-STATUS-CODE TO STATUS-WS                                 
444300     PERFORM IMS-STATUSKONTROLL                                           
444400     .                                                                    
444500     EJECT                                                                
444600 IMS-GU-SATB-STR SECTION.                                                 
444700                                                                          
444800     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
444900          DELIMITED BY SIZE INTO SSA1                                     
445000     MOVE '  GE' TO GODK-STATUSKODER                                      
445100     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
445200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
445300     PERFORM IMS-STATUSKONTROLL                                           
445400     .                                                                    
445500     SKIP3                                                                
445600 IMS-GU-SATB-STR-K-PCB SECTION.                                           
445700                                                                          
445800     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
445900          DELIMITED BY SIZE INTO SSA1                                     
446000     MOVE '  GE' TO GODK-STATUSKODER                                      
446100     CALL CBLTDLI USING GU SATB-K-PCB DLI-IO-AREA-K SSA1                  
446200     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
446300     PERFORM IMS-STATUSKONTROLL                                           
446400     .                                                                    
446500     EJECT                                                                
446600 IMS-GU-SATB-CSEQ-STR SECTION.                                            
446700                                                                          
446800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
446900                                    W-BELEVART-X                          
447000                                    W-IDARTNR-X ')'                       
447100          DELIMITED BY SIZE INTO SSA1                                     
447200     MOVE 'WLSATB01 ' TO SSA2                                             
447300     MOVE '  GE' TO GODK-STATUSKODER                                      
447400     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA-CSEQ SSA1 SSA2          
447500     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
447600     PERFORM IMS-STATUSKONTROLL                                           
447700     .                                                                    
447800     SKIP3                                                                
447900 IMS-GN-SATB-CSEQ-STR SECTION.                                            
448000                                                                          
448100     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
448200                                    W-BELEVART-X                          
448300                                    W-IDARTNR-X ')'                       
448400          DELIMITED BY SIZE INTO SSA1                                     
448500     MOVE 'WLSATB01 ' TO SSA2                                             
448600     MOVE '  GE' TO GODK-STATUSKODER                                      
448700     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA-CSEQ SSA1 SSA2          
448800     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
448900     PERFORM IMS-STATUSKONTROLL                                           
449000     .                                                                    
449100     EJECT                                                                
449200 IMS-GHU-SATB-STR SECTION.                                                
449300                                                                          
449400     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
449500          DELIMITED BY SIZE INTO SSA1                                     
449600     MOVE '  GE' TO GODK-STATUSKODER                                      
449700     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
449800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
449900     PERFORM IMS-STATUSKONTROLL                                           
450000     .                                                                    
450100     SKIP3                                                                
450200 IMS-GHU-SATB-KONV-STR SECTION.                                           
450300                                                                          
450400     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
450500          DELIMITED BY SIZE INTO SSA1                                     
450600     MOVE '  GE' TO GODK-STATUSKODER                                      
450700     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
450800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
450900     PERFORM IMS-STATUSKONTROLL                                           
451000     .                                                                    
451100     SKIP3                                                                
451200 IMS-GHU-SATB-STR-K-PCB SECTION.                                          
451300                                                                          
451400     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
451500          DELIMITED BY SIZE INTO SSA1                                     
451600     MOVE '  GE' TO GODK-STATUSKODER                                      
451700     CALL CBLTDLI USING GHU SATB-K-PCB DLI-IO-AREA-K SSA1                 
451800     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
451900     PERFORM IMS-STATUSKONTROLL                                           
452000     .                                                                    
452100     EJECT                                                                
452200 IMS-GHN-SATB-KONV-IDUSER-STR SECTION.                                    
452300                                                                          
452400     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
452500                                  W-STRNR-KONV-MIN-X                      
452600                    '&WDJ1DSEQ<=' W-IDUSER-X                              
452700                                  W-STRNR-KONV-MAX-X ')'                  
452800          DELIMITED BY SIZE INTO SSA1                                     
452900     MOVE '  GE' TO GODK-STATUSKODER                                      
453000     CALL CBLTDLI USING GHN SATB-D-PCB DLI-IO-AREA SSA1                   
453100     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
453200     PERFORM IMS-STATUSKONTROLL                                           
453300     .                                                                    
453400     SKIP3                                                                
453500 IMS-GET-SATB-RAD SECTION.                                                
453600                                                                          
453700     MOVE 'WLSATB11 ' TO SSA1                                             
453800     MOVE '  GE' TO GODK-STATUSKODER                                      
453900     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
454000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
454100     PERFORM IMS-STATUSKONTROLL                                           
454200     .                                                                    
454300     SKIP3                                                                
454400 IMS-GET-SATB-RAD-K-PCB SECTION.                                          
454500                                                                          
454600     MOVE 'WLSATB11 ' TO SSA1                                             
454700     MOVE '  GE' TO GODK-STATUSKODER                                      
454800     CALL CBLTDLI USING GNP SATB-K-PCB DLI-IO-AREA-K SSA1                 
454900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
455000     PERFORM IMS-STATUSKONTROLL                                           
455100     .                                                                    
455200     EJECT                                                                
455300 IMS-GNP-SATB-RAD SECTION.                                                
455400                                                                          
455500     STRING 'WLSATB11(WDJ111KY=>' W-WDJ111KY-X ')'                        
455600          DELIMITED BY SIZE INTO SSA1                                     
455700     MOVE '  GE' TO GODK-STATUSKODER                                      
455800     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
455900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
456000     PERFORM IMS-STATUSKONTROLL                                           
456100     .                                                                    
456200     SKIP3                                                                
456300 IMS-GNP-FIRST-SATB-RAD SECTION.                                          
456400                                                                          
456500     MOVE 'WLSATB11*F' TO SSA1                                            
456600     MOVE '  GE' TO GODK-STATUSKODER                                      
456700     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
456800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
456900     PERFORM IMS-STATUSKONTROLL                                           
457000     .                                                                    
457100     SKIP3                                                                
457200 IMS-GNP-FIRST-SATB-RAD-K-PCB SECTION.                                    
457300                                                                          
457400     MOVE 'WLSATB11*F' TO SSA1                                            
457500     MOVE '  GE' TO GODK-STATUSKODER                                      
457600     CALL CBLTDLI USING GNP SATB-K-PCB DLI-IO-AREA-K SSA1                 
457700     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
457800     PERFORM IMS-STATUSKONTROLL                                           
457900     .                                                                    
458000     EJECT                                                                
458100 IMS-GNP-FIRST-KVAL-RAD SECTION.                                          
458200                                                                          
458300     STRING 'WLSATB11*F(WDJ111KY =' W-WDJ111KY-X ')'                      
458400          DELIMITED BY SIZE INTO SSA1                                     
458500     MOVE '  GE' TO GODK-STATUSKODER                                      
458600     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
458700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
458800     PERFORM IMS-STATUSKONTROLL                                           
458900     .                                                                    
459000     SKIP3                                                                
459100 IMS-GU-SATB-RAD SECTION.                                                 
459200                                                                          
459300     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
459400          DELIMITED BY SIZE INTO SSA1                                     
459500     MOVE '  GE' TO GODK-STATUSKODER                                      
459600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
459700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
459800     PERFORM IMS-STATUSKONTROLL                                           
459900     .                                                                    
460000     SKIP3                                                                
460100 IMS-GU-SATB-FORP-RAD SECTION.                                            
460200                                                                          
460300     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY2-X ')'                       
460400          DELIMITED BY SIZE INTO SSA1                                     
460500     MOVE '  GE' TO GODK-STATUSKODER                                      
460600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
460700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
460800     PERFORM IMS-STATUSKONTROLL                                           
460900     .                                                                    
461000     EJECT                                                                
461100 IMS-GHNP-FIRST-SATB-RAD-K-PCB SECTION.                                   
461200                                                                          
461300     STRING 'WLSATB11*F(WDJ111KY =' W-WDJ111KY-X ')'                      
461400          DELIMITED BY SIZE INTO SSA1                                     
461500     MOVE '  GE' TO GODK-STATUSKODER                                      
461600     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-K SSA1                
461700     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
461800     PERFORM IMS-STATUSKONTROLL                                           
461900     .                                                                    
462000     SKIP3                                                                
462100 IMS-GHNP-SATB-RAD SECTION.                                               
462200                                                                          
462300     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
462400          DELIMITED BY SIZE INTO SSA1                                     
462500     MOVE '  GE' TO GODK-STATUSKODER                                      
462600     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
462700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
462800     PERFORM IMS-STATUSKONTROLL                                           
462900     .                                                                    
463000     EJECT                                                                
463100 IMS-GNP-SATB-NOT SECTION.                                                
463200                                                                          
463300     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
463400          DELIMITED BY SIZE INTO SSA1                                     
463500     MOVE 'WLSATB22 ' TO SSA2                                             
463600     MOVE '  GE' TO GODK-STATUSKODER                                      
463700     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
463800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
463900     PERFORM IMS-STATUSKONTROLL                                           
464000     .                                                                    
464100     SKIP3                                                                
464200 IMS-GHNP-FIRST-SATB-NOT-K-PCB SECTION.                                   
464300                                                                          
464400     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
464500          DELIMITED BY SIZE INTO SSA1                                     
464600     MOVE 'WLSATB22*F' TO SSA2                                            
464700     MOVE '  GE' TO GODK-STATUSKODER                                      
464800     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-K SSA1 SSA2           
464900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
465000     PERFORM IMS-STATUSKONTROLL                                           
465100     .                                                                    
465200     SKIP3                                                                
465300 IMS-GNP-SATB-NOT-K-PCB SECTION.                                          
465400                                                                          
465500     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
465600          DELIMITED BY SIZE INTO SSA1                                     
465700     MOVE 'WLSATB22 ' TO SSA2                                             
465800     MOVE '  GE' TO GODK-STATUSKODER                                      
465900     CALL CBLTDLI USING GNP SATB-K-PCB DLI-IO-AREA-K SSA1 SSA2            
466000     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
466100     PERFORM IMS-STATUSKONTROLL                                           
466200     .                                                                    
466300     EJECT                                                                
466400 IMS-ISRT-SATB-STR SECTION.                                               
466500                                                                          
466600     MOVE 'WLSATB01 ' TO SSA1                                             
466700     MOVE '    ' TO GODK-STATUSKODER                                      
466800     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
466900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
467000     PERFORM IMS-STATUSKONTROLL                                           
467100     .                                                                    
467200     SKIP3                                                                
467300 IMS-ISRT-SATB-RAD SECTION.                                               
467400                                                                          
467500     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
467600          DELIMITED BY SIZE INTO SSA1                                     
467700     MOVE 'WLSATB11 ' TO SSA2                                             
467800     MOVE '    ' TO GODK-STATUSKODER                                      
467900     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1 SSA2               
468000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
468100     PERFORM IMS-STATUSKONTROLL                                           
468200     .                                                                    
468300     EJECT                                                                
468400 IMS-ISRT-SATB-NOT SECTION.                                               
468500                                                                          
468600     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
468700          DELIMITED BY SIZE INTO SSA1                                     
468800     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
468900          DELIMITED BY SIZE INTO SSA2                                     
469000     MOVE 'WLSATB22 ' TO SSA3                                             
469100     MOVE '    ' TO GODK-STATUSKODER                                      
469200     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
469300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
469400     PERFORM IMS-STATUSKONTROLL                                           
469500     .                                                                    
469600     EJECT                                                                
469700 IMS-REPL-SATB SECTION.                                                   
469800                                                                          
469900     MOVE '  ' TO GODK-STATUSKODER                                        
470000     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
470100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
470200     PERFORM IMS-STATUSKONTROLL                                           
470300     .                                                                    
470400                                                                          
470500 IMS-REPL-SATB-K-PCB SECTION.                                             
470600                                                                          
470700     MOVE '  ' TO GODK-STATUSKODER                                        
470800     CALL CBLTDLI USING REPL SATB-K-PCB DLI-IO-AREA-K                     
470900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
471000     PERFORM IMS-STATUSKONTROLL                                           
471100     .                                                                    
471200                                                                          
471300 IMS-ISRT-SATB-STR-K-PCB SECTION.                                         
471400                                                                          
471500     MOVE 'WLSATB01 ' TO SSA1                                             
471600     MOVE '    ' TO GODK-STATUSKODER                                      
471700     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-K SSA1                
471800     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
471900     PERFORM IMS-STATUSKONTROLL                                           
472000     .                                                                    
472100     SKIP3                                                                
472200 IMS-ISRT-SATB-RAD-K-PCB SECTION.                                         
472300                                                                          
472400     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
472500          DELIMITED BY SIZE INTO SSA1                                     
472600     MOVE 'WLSATB11 ' TO SSA2                                             
472700     MOVE '    ' TO GODK-STATUSKODER                                      
472800     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-K SSA1 SSA2           
472900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
473000     PERFORM IMS-STATUSKONTROLL                                           
473100     .                                                                    
473200     EJECT                                                                
473300 IMS-ISRT-SATB-NOT-K-PCB SECTION.                                         
473400                                                                          
473500     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
473600          DELIMITED BY SIZE INTO SSA1                                     
473700     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
473800          DELIMITED BY SIZE INTO SSA2                                     
473900     MOVE 'WLSATB22 ' TO SSA3                                             
474000     MOVE '    ' TO GODK-STATUSKODER                                      
474100     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-K SSA1                
474200                                                 SSA2 SSA3                
474300     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
474400     PERFORM IMS-STATUSKONTROLL                                           
474500     .                                                                    
474600     EJECT                                                                
474700                                                                          
474800 IMS-REPL-ARTC SECTION.                                                   
474900                                                                          
475000     MOVE '  ' TO GODK-STATUSKODER                                        
475100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-ARTC                    
475200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
475300     PERFORM IMS-STATUSKONTROLL                                           
475400     .                                                                    
475500                                                                          
475600                                                                          
475700 IMS-DLET-SATB SECTION.                                                   
475800                                                                          
475900     MOVE '  ' TO GODK-STATUSKODER                                        
476000     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
476100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
476200     PERFORM IMS-STATUSKONTROLL                                           
476300     .                                                                    
476400                                                                          
476500 IMS-DLET-SATB-K-PCB SECTION.                                             
476600                                                                          
476700     MOVE '  ' TO GODK-STATUSKODER                                        
476800     CALL CBLTDLI USING DLET SATB-K-PCB DLI-IO-AREA-K                     
476900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
477000     PERFORM IMS-STATUSKONTROLL                                           
477100     .                                                                    
477200     EJECT                                                                
477300 IMS-ISRT-2234-TRANS SECTION.                                             
477400                                                                          
477500     STRING 'WLXXBY01(WDG3KEY  =' W-WDG3KEY-X ')'                         
477600          DELIMITED BY SIZE INTO SSA1                                     
477700     MOVE 'WLXXBY11*L ' TO SSA2                                           
477800     MOVE '  ' TO GODK-STATUSKODER                                        
477900     CALL CBLTDLI USING ISRT XXBY-PCB DLI-IO-AREA-XX SSA1 SSA2            
478000     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
478100     PERFORM IMS-STATUSKONTROLL                                           
478200     .                                                                    
478300                                                                          
478400 IMS-GET-XXAZ01 SECTION.                                                  
478500                                                                          
478600     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
478700          DELIMITED BY SIZE INTO SSA1                                     
478800     MOVE '    ' TO GODK-STATUSKODER                                      
478900     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA-XX SSA1                   
479000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
479100     PERFORM IMS-STATUSKONTROLL                                           
479200     .                                                                    
479300                                                                          
479400 IMS-GHNP-XXAZ11 SECTION.                                                 
479500                                                                          
479600     STRING 'WLXXAZ11(IDUSER   =' W-IDUSER-X ')'                          
479700          DELIMITED BY SIZE INTO SSA1                                     
479800     MOVE '  GE' TO GODK-STATUSKODER                                      
479900     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA-XX SSA1                 
480000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
480100     PERFORM IMS-STATUSKONTROLL                                           
480200     .                                                                    
480300                                                                          
480400 IMS-DLET-XXAZ SECTION.                                                   
480500                                                                          
480600     MOVE '  ' TO GODK-STATUSKODER                                        
480700     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA-XX                      
480800     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
480900     PERFORM IMS-STATUSKONTROLL                                           
481000     .                                                                    
481100                                                                          
481200 IMS-STATUSKONTROLL SECTION.                                              
481300                                                                          
481400     SET STATUS-IX TO 1                                                   
481500     SEARCH GODK-STATUS                                                   
481600       AT END CALL FELLOG                                                 
481700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
481800     END-SEARCH                                                           
481900     .                                                                    
482000     EJECT                                                                
482100*    -COPY WY2000P1                                                       
