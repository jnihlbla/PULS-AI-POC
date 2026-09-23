000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9041300.                                                
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
002300*        TRANSAKTION: W90413T, W90413U                                    
002400*        MID:         W90413I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W90413O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W9041300'.            
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100 77  STRIND                      PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  STRIND2                     PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77  K-STRIND                    PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  RADIND                      PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  RADIND-MAX                  PIC S9(9)   VALUE +13  COMP SYNC.        
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
007200     88  EGEN-MID                            VALUE '9413'.                
007300     88  1213-MID                            VALUE '1213'.                
007400     88  GODK-MID                            VALUE '1211' '9413'          
007500                                                   '1213' '1214'          
007600                                                   '1215'.                
007700     88  GODK-MID-MED-IDSKYLT                VALUE '1211' '9413'          
007800                                                   '1213' '1214'.         
007900     88  GODK-MID-MED-IDRADNR                VALUE '9413' '1213'.         
008000                                                                          
008100 77  KDSORT-SW                   PIC X(2)    VALUE SPACE.                 
008200     88  KDSORT-OK                           VALUE 'ST' 'SA' 'KG'         
008300                                             'M ' ' M' 'L ' ' L'          
008400                                             'MM' 'G ' ' G' 'C2'          
008500                                            'M2' 'ML' 'TM' 'PA'.          
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
013300*                                                                         
013400*01    -COPY WWPRODSL                                                     
013500*      --- VALID IDDC CODES                                               
013600*                                                                         
013700*01    -COPY WWDCKONS                                                     
013800       EJECT                                                              
013900*    --- ARBETSAREOR                                                      
014000 01  ARBETSAREOR.                                                         
014100                                                                          
014200     03  DAGENS-DATUM              PIC 9(6).                              
014300                                                                          
014400     03  INNEVARANDE-VECKA         PIC 9(6).                              
014500                                                                          
014600     03  AKTUELLT-DATUM            PIC 9(6).                              
014700                                                                          
014800     03  WS-SPAR-TIFINLV           PIC 9(6).                              
014900                                                                          
015000     03  WS-KDBENHOM-NUM           PIC 9(1)   VALUE  0.                   
015100     03  WS-STR-IDLEVNR-ARTC       PIC X(5)   VALUE SPACE.                
015200                                                                          
015300     03  WS-RADNR                  PIC S9(5)  VALUE +0.                   
015400     03  WS-RADNR-NYTT             PIC S9(5)  VALUE +0.                   
015500     03  WS-RADNR-GAM              PIC S9(5)  VALUE +0.                   
015600                                                                          
015700     03  WS-RADNR4                 PIC S9(4)  VALUE +0.                   
015800                                                                          
015900     03  WS-STR-NOT-WDJ122         PIC X(141) VALUE SPACE.                
016000                                                                          
016100     03  WS-TIAAVV                 PIC 9(4).                              
016200     03  WS-TIAAVV-A               REDEFINES WS-TIAAVV.                   
016300       05  WS-TIAA                 PIC 99.                                
016400       05  WS-TIVV                 PIC 99.                                
016500                                                                          
016600     03  WS-STRNR-SPAR             PIC S9(9)  VALUE +0    COMP-3.         
016700     03  WS-IDARTNR-SPAR           PIC S9(9)  VALUE +0    COMP-3.         
016800     03  WS-IDARTNR-ART-SPAR       PIC S9(9)  VALUE +0    COMP-3.         
016900                                                                          
017000     03  WS-REANTPSA               PIC S9(2)V9(3) VALUE +0 COMP-3.        
017100                                                                          
017200     03  WS-TISTODAT               PIC S9(7)               COMP-3.        
017300     03  WS-TIUPPDAT-GAM           PIC S9(7)               COMP-3.        
017400                                                                          
017500     03  WS-PB-SEP-TOT             PIC S9(6)V9             COMP-3.        
017600                                                                          
017700     03  WS-IDLEVNR                PIC X(5)   VALUE SPACE.                
017800     03  WS-IDLEVNR-PLUS           PIC X(5)   VALUE '+++++'.              
017900     03  WS-BELEVART               PIC X(30)  VALUE SPACE.                
018000     03  WS-BELEVART-PLUS          PIC X(30)                              
018100                           VALUE '++++++++++++++++++++++++++++++'.        
018200     03  WS-BEART                  PIC X(25)  VALUE SPACE.                
018300     03  WS-IDSTRTYP               PIC X      VALUE SPACE.                
018400     03  WS-KDSORT                 PIC X(2)   VALUE SPACE.                
018500     03  WS-NOLL                   PIC S9(9)  VALUE ZERO COMP-3.          
018600                                                                          
018700     03  SPAERRAT                  PIC X      VALUE 'S'.                  
018800     03  BORTTAGET                 PIC X      VALUE 'B'.                  
018900     03  WS-VAR                    PIC X      VALUE SPACE.                
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
076500*01  MID -COPY W90413I1                                                   
076600     EJECT                                                                
076700*01  -COPY W1I21301    -PRE 1213-                                         
076800     EJECT                                                                
076900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
077000     SKIP3                                                                
077100*01  -COPY WMSGAREA                                                       
077200     EJECT                                                                
077300     03  MOD REDEFINES MSG-AREA.                                          
077400*      05  -COPY W90413O1                                                 
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
089200     03  IO-AREA-ARTC            PIC X(900)  VALUE SPACE.                 
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
092900*01  -COPY W0008 -PRE SATB-                                               
093000     05  FILLER                  PIC X.                                   
093100     EJECT                                                                
093200*01  -COPY W0008 -PRE SATB-K-                                             
093300     05  FILLER                  PIC X.                                   
093400     EJECT                                                                
093500*01  -COPY W0008 -PRE SATB-C-                                             
093600     05  FILLER                  PIC X.                                   
093700     EJECT                                                                
093800*01  -COPY W0008 -PRE SATB-D-                                             
093900     05  FILLER                  PIC X.                                   
094000     EJECT                                                                
094100*01  -COPY W0008 -PRE XXAZ-                                               
094200     05  FILLER                  PIC X.                                   
094300     EJECT                                                                
094400*01  -COPY W0008 -PRE XXBY-                                               
094500     05  FILLER                  PIC X.                                   
094600     EJECT                                                                
094700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
094800                                   BENA-A-PCB BENA-B-PCB                  
094900                           ARTC-PCB WDF5-PCB  SATB-PCB                    
095000                           SATB-K-PCB SATB-C-PCB SATB-D-PCB               
095100                           XXAZ-PCB XXBY-PCB.                             
095200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
095300                                   BENA-A-PCB BENA-B-PCB                  
095400                           ARTC-PCB WDF5-PCB  SATB-PCB                    
095500                           SATB-K-PCB SATB-C-PCB SATB-D-PCB               
095600                           XXAZ-PCB XXBY-PCB.                             
095700                                                                          
095800     PERFORM IMS-GET-MSG                                                  
095900     IF SEGMENT-FINNS                                                     
096000       PERFORM A-INIT                                                     
096100       PERFORM B-KOLLA-NYCKLAR                                            
096200       IF NYCKLAR-OK                                                      
096300         PERFORM S01-KOLLA-STRNR                                          
096400         IF MFS-UPDATE                                                    
096500           PERFORM H-KOLLA-INPUT                                          
096600           IF INDATA-OK                                                   
096700             PERFORM I-UPPDATERA                                          
096800           END-IF                                                         
096900* ABEND PÅ ALLMÄN BEGÄRAN                                                 
097000*    DIVIDE W-STRNR BY WS-NOLL GIVING W-STRNR                             
097100* ABEND PÅ ALLMÄN BEGÄRAN                                                 
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
099300       MOVE LENGTH OF MOD-W90413O1 TO MSG-KVLL                            
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
100500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90413I1                 
100600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
100700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
100800     ELSE                                                                 
100900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90413I1                  
101000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
101100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
101200     END-IF                                                               
101300                                                                          
101400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
101500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
101600     MOVE MFS-IDTRANS TO IDTRANS-SW                                       
101700                                                                          
101800     MOVE LOW-VALUE TO MSG-AREA                                           
101900     MOVE 'W90413O1' TO MFS-IDMOD                                         
102000     MOVE '9413' TO MOD-IDTRANS                                           
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
105100     MOVE MID-W90413I1 TO 1213-MID-W1I21301                               
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
107000                                                                          
107100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
107200     MOVE '001'             TO MSGI-KDCALL                                
107300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
107400                               MSGI-IDLTERM-USER                          
107500     MOVE '9413'            TO MSGI-IDTRANS                               
107600                                                                          
107700     IF MFS-IDTRANS = '9413'                                              
107800     OR (MID-STRNR-IN NUMERIC                                             
107900     AND MID-STRNR-IN > ZERO)                                             
108000        MOVE MID-STRNR-IN TO MSGI-IDARTNR                                 
108100     END-IF                                                               
108200                                                                          
108300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
108400                                                                          
108500     IF MSGI-IDLAND-SPR = 'GB'                                            
108600       MOVE +2 TO SPRAK-IX                                                
108700       MOVE 'GB ' TO MED-IDSKYLT                                          
108800     ELSE                                                                 
108900       MOVE +1 TO SPRAK-IX                                                
109000       MOVE 'S  ' TO MED-IDSKYLT                                          
109100     END-IF                                                               
109200                                                                          
109300                                                                          
109400     MOVE MSGI-IDARTNR   TO WS-STRNR                                      
109500     INSPECT WS-STRNR REPLACING LEADING SPACE BY ZERO                     
109600                                                                          
109700     IF MID-STRNR-IN = ALL '+'                                            
109800       CONTINUE                                                           
109900     ELSE                                                                 
110000       MOVE SPACE        TO MFS-KDTRTYP                                   
110100       MOVE '7'          TO MFS-IDPFK                                     
110200     END-IF                                                               
110300                                                                          
110400     IF (WS-STRNR NUMERIC) AND (WS-STRNR > ZERO) AND                      
110500         (WS-STRNR < 10000000)                                            
110600       MOVE WS-STRNR TO W-STRNR                                           
110700     ELSE                                                                 
110800       MOVE '401'          TO MED-IDMFSFEL                                
110900       CALL WMEDKONV USING MED-WMEDAREA                                   
111000       MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                
111100       MOVE NEJ TO NYCKLAR-SW                                             
111200     END-IF                                                               
111300                                                                          
111400     IF GODK-MID OR NYCKLAR-OK                                            
111500       MOVE WS-STRNR TO MOD-STRNR-UT                                      
111600       INSPECT MOD-STRNR-UT REPLACING LEADING ZERO BY SPACE               
111700     ELSE                                                                 
111800       MOVE MFS-RENSA-FAELT TO MOD-STRNR-UT                               
111900     END-IF                                                               
112000                                                                          
112100***  -- KONTROLL AV IDSKYLT                                               
112200                                                                          
112300     IF MSGI-IDLAND-SPR = 'GB'                                            
112400       MOVE 'GB' TO WS-IDSKYLT                                            
112500     ELSE                                                                 
112600       MOVE 'S'  TO WS-IDSKYLT                                            
112700     END-IF                                                               
112800                                                                          
112900     SET WWLAND03-IX TO +1                                                
113000     SEARCH WWLAND03-IDSKYLT-RAD                                          
113100       AT END                                                             
113200         MOVE '401'        TO MED-IDMFSFEL                                
113300         CALL WMEDKONV USING MED-WMEDAREA                                 
113400         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
113500         MOVE NEJ   TO NYCKLAR-SW                                         
113600       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                    
113700         CONTINUE                                                         
113800     END-SEARCH                                                           
113900                                                                          
114000     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
114100                                                                          
114200***  -- KONTROLL AV RADNR                                                 
114300                                                                          
114400     IF GODK-MID-MED-IDRADNR                                              
114500       IF MID-IDRADNR-IN = ALL '+'                                        
114600         IF MID-STRNR-IN = ALL '+'                                        
114700           MOVE MID-IDRADNR-UT TO WS-IDRADNR                              
114800           INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO             
114900         ELSE                                                             
115000           MOVE '00010'      TO WS-IDRADNR                                
115100         END-IF                                                           
115200       ELSE                                                               
115300         MOVE MID-IDRADNR-IN TO WS-IDRADNR                                
115400         INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO               
115500         MOVE SPACE      TO MFS-KDTRTYP                                   
115600       END-IF                                                             
115700     ELSE                                                                 
115800       MOVE '00010' TO WS-IDRADNR                                         
115900       MOVE '++++'  TO MID-IDRADNR-IN                                     
116000     END-IF                                                               
116100                                                                          
116200     IF WS-IDRADNR NUMERIC AND WS-IDRADNR NOT < ZERO                      
116300       MOVE WS-IDRADNR     TO W-IDRADNR                                   
116400     ELSE                                                                 
116500       MOVE '401'          TO MED-IDMFSFEL                                
116600       CALL WMEDKONV USING MED-WMEDAREA                                   
116700       MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                
116800       MOVE NEJ TO NYCKLAR-SW                                             
116900     END-IF                                                               
117000                                                                          
117100     IF GODK-MID OR NYCKLAR-OK                                            
117200       MOVE W-IDRADNR   TO WS-RADNR4                                      
117300       MOVE WS-RADNR4   TO MOD-IDRADNR-UT                                 
117400                           MOD-IDRADNR-B                                  
117500       INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE             
117600     ELSE                                                                 
117700       MOVE MFS-RENSA-FAELT TO MOD-STRNR-UT                               
117800       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-UT                             
117900       MOVE FEL1(SPRAK-IX)  TO MOD-TEMFSFEL                               
118000     END-IF                                                               
118100                                                                          
118200                                                                          
118300     IF NYCKLAR-FEL                                                       
118400       PERFORM MFS-RENSA-FAELT-IN                                         
118500       PERFORM MFS-RENSA-FAELT-UT                                         
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900******************************************************************        
119000**   VISA RADEN SOM VALDES PÅ BILD 1213.                                  
119100******************************************************************        
119200                                                                          
119300 C-VISA-RAD SECTION.                                                      
119400                                                                          
119500     MOVE JA TO ALLT-SW                                                   
119600     .                                                                    
119700     EJECT                                                                
119800******************************************************************        
119900**   VISA FÖRSTA RADEN I STRUKTUREN.                                      
120000******************************************************************        
120100                                                                          
120200 D-FOERSTA-RADEN SECTION.                                                 
120300                                                                          
120400     IF MID-IDRADNR-IN = ALL '+'                                          
120500*****  OM RADNR EJ IFYLLD LÄS FRÅN BÖRJAN                                 
120600       MOVE ZERO           TO W-IDRADNR                                   
120700     END-IF                                                               
120800     MOVE '006'            TO MED-IDMFSFEL                                
120900     CALL WMEDKONV USING MED-WMEDAREA                                     
121000     MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                                
121100                                                                          
121200     MOVE JA               TO ALLT-SW                                     
121300     .                                                                    
121400     EJECT                                                                
121500******************************************************************        
121600**   VISA NÄSTA RAD I STRUKTUREN.                                         
121700******************************************************************        
121800                                                                          
121900 E-NAESTA-RAD SECTION.                                                    
122000                                                                          
122100     MOVE MID-IDRADNR-B TO W-IDRADNR                                      
122200     ADD +1             TO W-IDRADNR                                      
122300     MOVE JA            TO ALLT-SW                                        
122400     .                                                                    
122500     EJECT                                                                
122600******************************************************************        
122700**   VISA SAMMA RAD SOM MAN ÄR PÅ.                                        
122800******************************************************************        
122900                                                                          
123000 F-SAMMA-RAD SECTION.                                                     
123100     IF MID-INPUT = ALL '+'                                               
123200       MOVE JA             TO ALLT-SW                                     
123300     ELSE                                                                 
123400       MOVE NEJ            TO ALLT-SW                                     
123500       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
123600       CALL WMEDKONV USING MED-WMEDAREA                                   
123700       MOVE MED-TEMFSINF   TO MOD-TEMFSINF                                
123800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
123900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
124000       PERFORM MFS-LAS-IN-IGEN                                            
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400******************************************************************        
124500**   VISA VALD RAD. FINNS RADENS ARTIKELNUMMER PÅ ARTIKELREGISTRET        
124600**   HÄMTAS VÄRDENA FRÅN ARTC, WDF5, BENA OCH RASA ANNARS                 
124700**   HÄMTAS DE BARA FRÅN RASA.                                            
124800******************************************************************        
124900                                                                          
125000 G-LAES-VISA-INFO SECTION.                                                
125100                                                                          
125200     PERFORM GA-LAES-GRUNDDATA                                            
125300                                                                          
125400     IF (STRNR-FINNS OR KONV-FINNS) AND                                   
125500      RADNR-FINNS                                                         
125600       PERFORM IMS-GET-ARTC-01                                            
125700                                                                          
125800       IF SEGMENT-FINNS                                                   
125900**   RADENS ARTIKEL FINNS PÅ ARTIKELREGISTRET                             
126000                                                                          
126100           MOVE ART-IDLEVNR TO WS-IDLEVNR                                 
126200                                                                          
126300           IF ART-KDERS-UTG > 0                                           
126400              MOVE FEL14(SPRAK-IX) TO MOD-TEMFSINF                        
126500           END-IF                                                         
126600                                                                          
126700         PERFORM IMS-GET-BENA-BSEQ                                        
126800         IF SEGMENT-FINNS                                                 
126900           PERFORM IMS-GET-BENA-B-TEXT                                    
127000         ELSE                                                             
127100           MOVE MED3(SPRAK-IX) TO MOD-TEMFSINF                            
127200         END-IF                                                           
127300                                                                          
127400         MOVE WS-IDLEVNR TO W-IDLEVNR                                     
127500         PERFORM IMS-GET-WDF501                                           
127600         IF SEGMENT-FINNS                                                 
127700           IF WS-IDLEVNR NOT = SPACE                                      
127800             MOVE WS-IDLEVNR TO W-IDLEVNR                                 
127900             PERFORM IMS-GET-WDF502-LAST                                  
128000           ELSE                                                           
128100             PERFORM IMS-GET-WDF502-OKVAL                                 
128200             IF SEGMENT-FINNS                                             
128300               MOVE XLEV-IDLEVNR   TO W-IDLEVNR                           
128400               PERFORM IMS-GET-WDF502-LAST                                
128500             END-IF                                                       
128600           END-IF                                                         
128700         END-IF                                                           
128800       ELSE                                                               
128900**   RADENS ARTIKEL FINNS BARA PÅ RASA                                    
129000                                                                          
129100         IF WS-RAD-IDARTNR NOT = ZERO                                     
129200           MOVE WS-RAD-IDARTNR TO MOD-IDARTNR-UT                          
129300           INSPECT MOD-IDARTNR-UT                                         
129400                   REPLACING LEADING ZERO BY SPACE                        
129500         ELSE                                                             
129600           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                         
129700         END-IF                                                           
129800                                                                          
129900         MOVE WS-RAD-KDBENHOM TO WS-KDBENHOM-NUM                          
130000                                                                          
130100         IF WS-IDSKYLT = 'S  '                                            
130200           CONTINUE                                                       
130300         ELSE                                                             
130400******     HÄMTA UTLÄNDSK BENÄMNING                                       
130500           MOVE 'S  ' TO W-IDSKYLT                                        
130600           MOVE WS-RAD-BEART-SVE TO W-BEART                               
130700           PERFORM IMS-GET-BENA-ASEQ                                      
130800           PERFORM UNTIL SEGMENT-SAKNAS OR                                
130900                         BENA-BEN-KDHOMONYM = WS-RAD-KDBENHOM             
131000             IF SEGMENT-FINNS                                             
131100               IF BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM                    
131200                 CONTINUE                                                 
131300               ELSE                                                       
131400                 PERFORM IMS-GN-BENA-ASEQ                                 
131500               END-IF                                                     
131600             END-IF                                                       
131700           END-PERFORM                                                    
131800                                                                          
131900           IF SEGMENT-FINNS                                               
132000             MOVE WS-IDSKYLT TO W-IDSKYLT                                 
132100             PERFORM IMS-GET-BENA-A-TEXT                                  
132200           END-IF                                                         
132300         END-IF                                                           
132400       END-IF                                                             
132500       MOVE WS-RAD-REANTPSA TO MOD-REANTPSA-UT                            
132600                                                                          
132700       PERFORM IMS-GNP-SATB-NOT                                           
132800       IF SEGMENT-FINNS                                                   
132900         MOVE SATB-NOT-TESTRNOT(1) TO MOD-TESTRNOT-IN(1)                  
133000         MOVE SATB-NOT-TESTRNOT(2) TO MOD-TESTRNOT-IN(2)                  
133100       ELSE                                                               
133200         MOVE MFS-RENSA-FAELT      TO MOD-TESTRNOT-IN(1)                  
133300         MOVE MFS-RENSA-FAELT      TO MOD-TESTRNOT-IN(2)                  
133400       END-IF                                                             
133500     END-IF                                                               
133600                                                                          
133700     PERFORM MFS-RENSA-FAELT-IN                                           
133800     IF STRUKTURNR-SPAERRAT OR STRUKTURNR-BORTTAGET                       
133900       IF STRUKTURNR-SPAERRAT                                             
134000         MOVE FEL2(SPRAK-IX) TO MOD-TEMFSFEL                              
134100       ELSE                                                               
134200         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSFEL                              
134300       END-IF                                                             
134400     END-IF                                                               
134500     .                                                                    
134600     EJECT                                                                
134700******************************************************************        
134800**** HÄMTAR VÄRDEN FRÅN ROTEN OCH LÄSER SEDAN VALD RAD OCH LAGRAR         
134900**** UNDAN DEN I WS.                                                      
135000******************************************************************        
135100                                                                          
135200 GA-LAES-GRUNDDATA SECTION.                                               
135300                                                                          
135400     IF STRNR-FINNS OR KONV-FINNS                                         
135500       IF KONV-FINNS                                                      
135600         PERFORM IMS-GHU-SATB-STR-K-PCB                                   
135700       ELSE                                                               
135800         PERFORM IMS-GU-SATB-STR                                          
135900       END-IF                                                             
136000       IF MID-IDRADNR-IN = ALL '+'                                        
136100         PERFORM IMS-GNP-SATB-RAD                                         
136200       ELSE                                                               
136300         PERFORM IMS-GU-SATB-RAD                                          
136400       END-IF                                                             
136500       IF SEGMENT-FINNS AND SATB-RAD-KDSTRRAD NOT = '9'                   
136600         MOVE JA TO RADNR-SW                                              
136700         MOVE SATB-RAD-WDJ111  TO WS-LAGRA-RAD                            
136800         MOVE SATB-RAD-IDARTNR TO W-IDARTNR                               
136900                                  MOD-IDARTNR-UT                          
137000         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
137100         MOVE SATB-RAD-IDRADNR TO W-IDRADNR                               
137200         MOVE SATB-RAD-IDRADNR TO WS-RADNR4                               
137300         MOVE WS-RADNR4        TO MOD-IDRADNR-UT                          
137400                                  MOD-IDRADNR-B                           
137500*      GÖRS VID NYCKELKOLLEN OCKSÅ MEN MÅSTE GÖRAS HÄR OM RADNR           
137600*      EJ ÄR IFYLLT ELLER VID BLÄDDRING                                   
137700         INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE           
137800       ELSE                                                               
137900         IF SATB-RAD-KDSTRRAD = '9'                                       
138000           MOVE FEL4(SPRAK-IX) TO MOD-TEMFSFEL                            
138100           PERFORM MFS-RENSA-FAELT-UT                                     
138200           MOVE SATB-RAD-IDRADNR TO WS-RADNR4                             
138300           MOVE WS-RADNR4        TO MOD-IDRADNR-UT                        
138400                                    MOD-IDRADNR-B                         
138500           INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE         
138600         ELSE                                                             
138700           IF MFS-NEXT                                                    
138800             MOVE FEL5(SPRAK-IX) TO MOD-TEMFSFEL                          
138900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
139000           ELSE                                                           
139100             IF W-IDRADNR = ZERO                                          
139200               MOVE FEL6(SPRAK-IX) TO MOD-TEMFSFEL                        
139300               PERFORM MFS-RENSA-FAELT-UT                                 
139400             ELSE                                                         
139500               MOVE W-IDRADNR TO W-IDRADNR2                               
139600               PERFORM IMS-GU-SATB-FORP-RAD                               
139700               IF SEGMENT-FINNS                                           
139800                 MOVE FEL4(SPRAK-IX) TO MOD-TEMFSFEL                      
139900                 PERFORM MFS-RENSA-FAELT-UT                               
140000               ELSE                                                       
140100                 MOVE 'A' TO WS-VAR                                       
140200                 MOVE FEL8(SPRAK-IX) TO MOD-TEMFSFEL                      
140300                 PERFORM MFS-RENSA-FAELT-UT                               
140400               END-IF                                                     
140500             END-IF                                                       
140600           END-IF                                                         
140700         END-IF                                                           
140800       END-IF                                                             
140900     ELSE                                                                 
141000       MOVE FEL9(SPRAK-IX) TO MOD-TEMFSFEL                                
141100       PERFORM MFS-RENSA-FAELT-UT                                         
141200     END-IF                                                               
141300     .                                                                    
141400     EJECT                                                                
141500******************************************************************        
141600**** KONTROLLERA INRAPPORTERADE FÄLT.                                     
141700******************************************************************        
141800                                                                          
141900 H-KOLLA-INPUT SECTION.                                                   
142000                                                                          
142100     MOVE JA TO INDATA-SW                                                 
142200     IF STRUKTURNR-SPAERRAT OR STRUKTURNR-BORTTAGET                       
142300       MOVE NEJ TO INDATA-SW                                              
142400       MOVE FEL10(SPRAK-IX) TO MOD-TEMFSFEL                               
142500       IF STRUKTURNR-SPAERRAT                                             
142600         MOVE FEL11(SPRAK-IX) TO MOD-TEMFSFEL                             
142700       ELSE                                                               
142800         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSFEL                              
142900       END-IF                                                             
143000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
143100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
143200     ELSE                                                                 
143300       IF MID-INPUT = ALL '+'                                             
143400         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
143500         CALL WMEDKONV USING MED-WMEDAREA                                 
143600         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
143700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
143800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
143900         MOVE NEJ TO INDATA-SW                                            
144000       ELSE                                                               
144100         PERFORM HA-KOLLA-KLAR                                            
144200         IF INDATA-OK                                                     
144300           IF STRNR-FINNS OR KONV-FINNS                                   
144400             IF KONV-FINNS                                                
144500               PERFORM IMS-GHU-SATB-STR-K-PCB                             
144600             ELSE                                                         
144700               PERFORM IMS-GU-SATB-STR                                    
144800             END-IF                                                       
144900             IF INDATA-OK                                                 
145000               PERFORM HH-KOLLA-RAD                                       
145100               IF INDATA-OK                                               
145200                     IF MID-BORT NOT = ALL '+'                            
145300                       PERFORM HD-KOLLA-BORTTAG                           
145400                     ELSE                                                 
145500                       IF MID-UPPDAT  = ALL '+' AND                       
145600                        MID-TESTRNOT(1) = ALL '+' AND                     
145700                        MID-TESTRNOT(2) = ALL '+' AND                     
145800                        (MID-KLAR = 'J' OR 'N' OR 'Y')                    
145900                         PERFORM MFS-ROER-EJ-FAELT-UT                     
146000                         MOVE MFS-ADD-LAES-IN-FAELT                       
146100                                       TO MOD-TESTRNOT-IN-ATTR(1)         
146200                         MOVE MFS-ADD-LAES-IN-FAELT                       
146300                                       TO MOD-TESTRNOT-IN-ATTR(2)         
146400                       ELSE                                               
146500                         PERFORM HE-KOLLA-UPPDAT-FAELT                    
146600                                                                          
146700                         IF RADNR-FINNS                                   
146800                           PERFORM HF-KOLLA-UPPDAT-RAD                    
146900                         ELSE                                             
147000                           PERFORM HG-KOLLA-NY-RAD                        
147100                         END-IF                                           
147200                       END-IF                                             
147300                     END-IF                                               
147400                                                                          
147500******           ÄO KRÄVS VID VISSA UPPDATERINGAR                         
147600******           NÄR DET ÄR EN GAMMAL STRUKTUR.                           
147700                 IF WS-TIUPPDAT-GAM NOT = ZERO                            
147800                   IF MID-IDAO = ALL '+' OR SPACE                         
147900                     IF AO-SKALL-FINNAS                                   
148000                       IF EJ-TID-SIGNAL                                   
148100                         MOVE MED4(SPRAK-IX) TO MOD-TEMFSINF              
148200                       END-IF                                             
148300                       MOVE MFS-ALFA-FAELT-FEL TO                         
148400                                               MOD-IDAO-IN-ATTR           
148500                       MOVE MFS-NUM-FAELT-FEL TO                          
148600                                              MOD-TIAAVV-IN-ATTR          
148700                       MOVE NEJ TO INDATA-SW                              
148800                     END-IF                                               
148900                   END-IF                                                 
149000                 END-IF                                                   
149100                                                                          
149200                 IF INDATA-FEL                                            
149300                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
149400                   CALL WMEDKONV USING MED-WMEDAREA                       
149500                   MOVE MFS-ADD-LAES-IN-FAELT                             
149600                                       TO MOD-TESTRNOT-IN-ATTR(1)         
149700                   MOVE MFS-ADD-LAES-IN-FAELT                             
149800                                       TO MOD-TESTRNOT-IN-ATTR(2)         
149900                   PERFORM MFS-ROER-EJ-FAELT-UT                           
150000                   PERFORM MFS-ROER-EJ-FAELT-IN                           
150100                 ELSE                                                     
150200                   IF KONV-SAKNAS                                         
150300                     PERFORM S02-KONV-STRUKTUR                            
150400                   END-IF                                                 
150500                 END-IF                                                   
150600               END-IF                                                     
150700             ELSE                                                         
150800               MOVE MFS-ADD-LAES-IN-FAELT                                 
150900                                    TO MOD-TESTRNOT-IN-ATTR(1)            
151000               MOVE MFS-ADD-LAES-IN-FAELT                                 
151100                                    TO MOD-TESTRNOT-IN-ATTR(2)            
151200               PERFORM MFS-ROER-EJ-FAELT-UT                               
151300               PERFORM MFS-ROER-EJ-FAELT-IN                               
151400             END-IF                                                       
151500           ELSE                                                           
151600             MOVE FEL13(SPRAK-IX) TO MOD-TEMFSFEL                         
151700             MOVE FEL9(SPRAK-IX)  TO MOD-TEMFSINF                         
151800             MOVE NEJ TO INDATA-SW                                        
151900             PERFORM MFS-RENSA-FAELT-IN                                   
152000             PERFORM MFS-RENSA-FAELT-UT                                   
152100           END-IF                                                         
152200         ELSE                                                             
152300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
152400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
152500           PERFORM MFS-LAS-IN-IGEN                                        
152600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-IN-ATTR                    
152700         END-IF                                                           
152800       END-IF                                                             
152900     END-IF                                                               
153000     .                                                                    
153100     EJECT                                                                
153200******************************************************************        
153300**   KONTROLL OM STRUKTUREN ÄR KLAR.                                      
153400******************************************************************        
153500                                                                          
153600 HA-KOLLA-KLAR SECTION.                                                   
153700                                                                          
153800     IF MID-KLAR NOT = ALL '+'                                            
153900       IF MID-KLAR = 'J' OR 'N' OR 'Y'                                    
154000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KLAR-IN-ATTR                    
154100         IF MID-KLAR = 'J' OR 'Y'                                         
154200           MOVE JA TO KLAR-SW                                             
154300         ELSE                                                             
154400           PERFORM S11-HAR-USERID-KONV-STR                                
154500         END-IF                                                           
154600       ELSE                                                               
154700         IF EJ-TID-SIGNAL                                                 
154800           MOVE MED6(SPRAK-IX) TO MOD-TEMFSINF                            
154900         END-IF                                                           
155000         MOVE MFS-ALFA-FAELT-FEL TO MOD-KLAR-IN-ATTR                      
155100         MOVE NEJ TO INDATA-SW                                            
155200       END-IF                                                             
155300     ELSE                                                                 
155400       IF EJ-TID-SIGNAL                                                   
155500         MOVE MED6(SPRAK-IX) TO MOD-TEMFSINF                              
155600       END-IF                                                             
155700       MOVE MFS-ALFA-FAELT-FEL          TO MOD-KLAR-IN-ATTR               
155800       MOVE NEJ TO INDATA-SW                                              
155900     END-IF                                                               
156000     .                                                                    
156100     EJECT                                                                
156200******************************************************************        
156300**   KONTROLL AV BORTTAG.                                                 
156400******************************************************************        
156500                                                                          
156600 HD-KOLLA-BORTTAG SECTION.                                                
156700                                                                          
156800     IF RADNR-FINNS                                                       
156900       MOVE WS-RAD-TISTODAT   TO TMP1-YYMMDD                              
157000       MOVE DAGENS-DATUM      TO TMP2-YYMMDD                              
157100       PERFORM WY2000P1                                                   
157200       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
157300           IF MID-BORT = 'J' OR 'Y'                                       
157400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORT-IN-ATTR                
157500             IF WS-TIUPPDAT-GAM > 0                                       
157600               PERFORM IMS-GU-SATB-STR                                    
157700               IF SEGMENT-FINNS                                           
157800                 PERFORM IMS-GNP-FIRST-KVAL-RAD                           
157900                 IF SEGMENT-FINNS                                         
158000                   MOVE 'J' TO AO-SW                                      
158100                 END-IF                                                   
158200               END-IF                                                     
158300             END-IF                                                       
158400           ELSE                                                           
158500             MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-IN-ATTR                  
158600             MOVE NEJ TO INDATA-SW                                        
158700           END-IF                                                         
158800           IF MID-UPPDAT NOT = ALL '+' OR                                 
158900              MID-TESTRNOT (1) NOT = ALL '+' OR                           
159000              MID-TESTRNOT (2) NOT = ALL '+'                              
159100             IF EJ-TID-SIGNAL                                             
159200               MOVE MED18(SPRAK-IX) TO MOD-TEMFSINF                       
159300             END-IF                                                       
159400             MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-IN-ATTR                  
159500             MOVE NEJ TO INDATA-SW                                        
159600           END-IF                                                         
159700                                                                          
159800           IF MID-TIAAVV NOT = ALL '+'                                    
159900             PERFORM S13-KONV-TIAAVV                                      
160000             MOVE AKTUELLT-DATUM    TO TMP1-YYMMDD                        
160100             MOVE INNEVARANDE-VECKA TO TMP2-YYMMDD                        
160200             PERFORM WY2000P1                                             
160300             IF DAT-KDSVAR-FEL OR                                         
160400               TMP1-YYMMDD < TMP2-YYMMDD                                  
160500               IF EJ-TID-SIGNAL                                           
160600                 MOVE MED24(SPRAK-IX) TO MOD-TEMFSINF                     
160700               END-IF                                                     
160800               MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR               
160900               MOVE NEJ TO INDATA-SW                                      
161000             ELSE                                                         
161100               MOVE AKTUELLT-DATUM   TO TMP1-YYMMDD                       
161200               MOVE DAGENS-DATUM     TO TMP2-YYMMDD                       
161300               PERFORM WY2000P1                                           
161400               IF TMP1-YYMMDD < TMP2-YYMMDD                               
161500                 MOVE DAGENS-DATUM TO AKTUELLT-DATUM                      
161600               END-IF                                                     
161700               MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR             
161800             END-IF                                                       
161900           ELSE                                                           
162000             IF AO-SKALL-FINNAS                                           
162100               IF EJ-TID-SIGNAL                                           
162200                 MOVE MED25(SPRAK-IX) TO MOD-TEMFSINF                     
162300               END-IF                                                     
162400               MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR               
162500               MOVE NEJ TO INDATA-SW                                      
162600             END-IF                                                       
162700           END-IF                                                         
162800                                                                          
162900           IF MID-IDAO NOT = ALL '+'                                      
163000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                
163100             MOVE AKTUELLT-DATUM    TO TMP1-YYMMDD                        
163200             MOVE INNEVARANDE-VECKA TO TMP2-YYMMDD                        
163300             PERFORM WY2000P1                                             
163400             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
163500               IF EJ-TID-SIGNAL                                           
163600                 MOVE MED25(SPRAK-IX) TO MOD-TEMFSINF                     
163700               END-IF                                                     
163800               MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR               
163900               MOVE NEJ TO INDATA-SW                                      
164000             END-IF                                                       
164100           ELSE                                                           
164200             MOVE SPACE          TO MID-IDAO                              
164300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                
164400           END-IF                                                         
164500                                                                          
164600       ELSE                                                               
164700         MOVE MED27(SPRAK-IX) TO MOD-TEMFSINF                             
164800         MOVE NEJ TO INDATA-SW                                            
164900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
165000         PERFORM MFS-ROER-EJ-FAELT-IN                                     
165100       END-IF                                                             
165200     ELSE                                                                 
165300       MOVE 'D' TO WS-VAR                                                 
165400       MOVE FEL8(SPRAK-IX) TO MOD-TEMFSINF                                
165500       MOVE NEJ TO INDATA-SW                                              
165600     END-IF                                                               
165700     .                                                                    
165800     EJECT                                                                
165900******************************************************************        
166000**   RIMLIGHETSKONTROLL AV INRAPPORTERADE FÄLT.                           
166100******************************************************************        
166200                                                                          
166300 HE-KOLLA-UPPDAT-FAELT SECTION.                                           
166400                                                                          
166500     IF MID-IDARTNR NOT = ALL '+'                                         
166600       MOVE MID-IDARTNR TO DEC-IDFRIDATA                                  
166700       MOVE +9          TO DEC-KVHELTAL                                   
166800       MOVE +0          TO DEC-KVDECIMAL                                  
166900       CALL WDECEDIT USING DEC-WDECAREA                                   
167000       IF DEC-KDSVAR-FEL OR DEC-IDEDITDATA <= ZERO OR                     
167100                            DEC-IDEDITDATA > 99999999                     
167200         IF EJ-TID-SIGNAL                                                 
167300           MOVE MED28(SPRAK-IX)   TO MOD-TEMFSINF                         
167400         END-IF                                                           
167500         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-IN-ATTR                  
167600         MOVE NEJ TO INDATA-SW                                            
167700       ELSE                                                               
167800         MOVE DEC-IDEDITDATA TO W-IDARTNR                                 
167900         MOVE W-IDARTNR      TO MID-IDARTNR                               
168000         IF W-IDARTNR = W-STRNR                                           
168100           IF EJ-TID-SIGNAL                                               
168200            MOVE MED29(SPRAK-IX) TO MOD-TEMFSINF                          
168300           END-IF                                                         
168400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-IN-ATTR                
168500           MOVE NEJ TO INDATA-SW                                          
168600         ELSE                                                             
168700           IF INDATA-OK                                                   
168800             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-IN-ATTR              
168900             MOVE 'J' TO AO-SW                                            
169000           END-IF                                                         
169100         END-IF                                                           
169200       END-IF                                                             
169300     END-IF                                                               
169400                                                                          
169500     IF MID-REANTPSA NOT = ALL '+'                                        
169600       MOVE MID-REANTPSA TO DEC-IDFRIDATA                                 
169700       MOVE +2           TO DEC-KVHELTAL                                  
169800       MOVE +3           TO DEC-KVDECIMAL                                 
169900       CALL WDECEDIT USING DEC-WDECAREA                                   
170000       IF DEC-KDSVAR-FEL    OR DEC-IDEDITDATA = ZERO                      
170100         IF EJ-TID-SIGNAL                                                 
170200           MOVE MED13(SPRAK-IX) TO MOD-TEMFSINF                           
170300         END-IF                                                           
170400         MOVE MFS-NUM-FAELT-FEL   TO MOD-REANTPSA-IN-ATTR                 
170500         MOVE NEJ TO INDATA-SW                                            
170600       ELSE                                                               
170700         MOVE DEC-IDEDITDATA      TO WS-REANTPSA                          
170800         MOVE MFS-NUM-FAELT-RAETT TO MOD-REANTPSA-IN-ATTR                 
170900       END-IF                                                             
171000     END-IF                                                               
171100                                                                          
171200     PERFORM S07-KOLLA-AO-AAVV                                            
171300     .                                                                    
171400     EJECT                                                                
171500******************************************************************        
171600**   UPPDATERING AV REDAN EXISTERANDE RAD.                                
171700******************************************************************        
171800                                                                          
171900 HF-KOLLA-UPPDAT-RAD SECTION.                                             
172000                                                                          
172100     MOVE WS-RAD-TISTODAT   TO TMP1-YYMMDD                                
172200     MOVE DAGENS-DATUM      TO TMP2-YYMMDD                                
172300     PERFORM WY2000P1                                                     
172400     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
172500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
172600       MOVE WS-RAD-IDARTNR TO W-IDARTNR                                   
172700       PERFORM IMS-GET-ARTC-01                                            
172800       IF SEGMENT-FINNS                                                   
172900         IF MID-IDARTNR NOT = ALL '+'                                     
173000           IF EJ-TID-SIGNAL                                               
173100             MOVE MED33(SPRAK-IX) TO MOD-TEMFSINF                         
173200           END-IF                                                         
173300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                  
173400           MOVE NEJ TO INDATA-SW                                          
173500         ELSE                                                             
173600           IF ART-KDERS-UTG > 0                                           
173700             IF WS-REANTPSA > 0                                           
173800               MOVE FEL14(SPRAK-IX) TO MOD-TEMFSINF                       
173900               MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR              
174000               MOVE NEJ TO INDATA-SW                                      
174100             ELSE                                                         
174200               MOVE FEL14(SPRAK-IX) TO MOD-TEMFSFEL                       
174300             END-IF                                                       
174400           ELSE                                                           
174500             IF WS-REANTPSA > 0                                           
174600               MOVE 'J' TO AO-SW                                          
174700             END-IF                                                       
174800           END-IF                                                         
174900         END-IF                                                           
175000       ELSE                                                               
175100         IF MID-IDARTNR NOT = ALL '+'                                     
175200           IF EJ-TID-SIGNAL                                               
175300             MOVE MED33(SPRAK-IX) TO MOD-TEMFSINF                         
175400           END-IF                                                         
175500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                  
175600           MOVE NEJ TO INDATA-SW                                          
175700         ELSE                                                             
175800           IF WS-REANTPSA > 0                                             
175900             MOVE 'J' TO AO-SW                                            
176000           END-IF                                                         
176100                                                                          
176200           PERFORM S17-KOLLA-BEART-KDHOM-AENDRING                         
176300                                                                          
176400         END-IF                                                           
176500       END-IF                                                             
176600     ELSE                                                                 
176700       MOVE MED27(SPRAK-IX) TO MOD-TEMFSINF                               
176800       MOVE NEJ TO INDATA-SW                                              
176900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
177000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
177100     END-IF                                                               
177200     .                                                                    
177300     EJECT                                                                
177400******************************************************************        
177500**   KONTROLL VID INLÄGGNING AV NY RAD.                                   
177600******************************************************************        
177700                                                                          
177800 HG-KOLLA-NY-RAD SECTION.                                                 
177900                                                                          
178000     IF MID-IDARTNR = ALL '+'                                             
178100       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-IN-ATTR                     
178200       MOVE NEJ TO INDATA-SW                                              
178300     ELSE                                                                 
178400         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-IN-ATTR                 
178500         PERFORM IMS-GET-ARTC-01                                          
178600         IF SEGMENT-FINNS                                                 
178700           IF (WS-STR-IDLEVNR-ARTC = '1002 ') AND                         
178800             (ART-KDERS-UTG > 0)                                          
178900             MOVE FEL14(SPRAK-IX)      TO MOD-TEMFSINF                    
179000             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-IN-ATTR             
179100             MOVE NEJ TO INDATA-SW                                        
179200           ELSE                                                           
179300*  911017  KTR PÅ PRODUKTSLAG FÖRPACKNING TILLAGT , GE ******             
179400             MOVE ART-KDPRODSL   TO TEST-KDPRODSL                         
179500             IF KDPRODSL-VOLVO-EMB                                        
179600                MOVE MFS-NUM-FAELT-FEL                                    
179700                                   TO MOD-IDARTNR-IN-ATTR                 
179800                MOVE NEJ TO INDATA-SW                                     
179900             ELSE                                                         
180000*****************************************************************         
180100*  920117 ÄT ARTIKLAR MED EK01 TILLÅTS I SATSER DÄR LEVNR EJ    *         
180200*            SATT (IDLEVNR = SPACE )                            *         
180300*****************************************************************         
180400                PERFORM IMS-GET-ARTC11                                    
180500                IF SEGMENT-FINNS                                          
180600                  IF WS-STR-IDLEVNR-ARTC = '1002 '                        
180700                    IF CLAG-KDERS > 0                                     
180800                    MOVE MED45(SPRAK-IX) TO MOD-TEMFSINF                  
180900                    MOVE MFS-NUM-FAELT-FEL                                
181000                                       TO MOD-IDARTNR-IN-ATTR             
181100                    MOVE NEJ TO INDATA-SW                                 
181200                     ELSE                                                 
181300                       PERFORM HG-A-KOLLA-ARTA-ARTNR                      
181400                       PERFORM HG-B-KOLLA-SATB-ARTNR                      
181500                     END-IF                                               
181600                  ELSE                                                    
181700                     IF WS-STR-IDLEVNR-ARTC = SPACE                       
181800                        IF CLAG-KDERS > 01                                
181900                          MOVE MED45(SPRAK-IX) TO MOD-TEMFSINF            
182000                          MOVE MFS-NUM-FAELT-FEL                          
182100                                       TO MOD-IDARTNR-IN-ATTR             
182200                          MOVE NEJ TO INDATA-SW                           
182300                        ELSE                                              
182400                           PERFORM HG-A-KOLLA-ARTA-ARTNR                  
182500                           PERFORM HG-B-KOLLA-SATB-ARTNR                  
182600                        END-IF                                            
182700                     ELSE                                                 
182800                        PERFORM HG-A-KOLLA-ARTA-ARTNR                     
182900                        PERFORM HG-B-KOLLA-SATB-ARTNR                     
183000                     END-IF                                               
183100                  END-IF                                                  
183200               ELSE                                                       
183300                  PERFORM HG-A-KOLLA-ARTA-ARTNR                           
183400                  PERFORM HG-B-KOLLA-SATB-ARTNR                           
183500               END-IF                                                     
183600             END-IF                                                       
183700           END-IF                                                         
183800         ELSE                                                             
183900             MOVE MED46(SPRAK-IX) TO MOD-TEMFSINF                         
184000             MOVE MFS-NUM-FAELT-FEL      TO MOD-IDARTNR-IN-ATTR           
184100             MOVE NEJ TO INDATA-SW                                        
184200         END-IF                                                           
184300         IF MID-REANTPSA = ALL '+'                                        
184400           IF EJ-TID-SIGNAL                                               
184500             MOVE MED44(SPRAK-IX) TO MOD-TEMFSINF                         
184600           END-IF                                                         
184700           MOVE MFS-NUM-FAELT-FEL TO MOD-REANTPSA-IN-ATTR                 
184800           MOVE NEJ TO INDATA-SW                                          
184900         END-IF                                                           
185000     END-IF                                                               
185100     .                                                                    
185200     EJECT                                                                
185300******************************************************************        
185400**   DE INRAPPORTERADE FÄLTENS VÄRDEN JÄMFÖRES MED DET SOM LIGGER         
185500**   PÅ ARTC.                                                             
185600******************************************************************        
185700                                                                          
185800 HG-A-KOLLA-ARTA-ARTNR SECTION.                                           
185900                                                                          
186000     PERFORM IMS-GET-ARTC-01                                              
186100     MOVE ART-IDLEVNR    TO WS-IDLEVNR                                    
186200                                                                          
186300     PERFORM IMS-GET-WDF501                                               
186400     IF SEGMENT-FINNS                                                     
186500       IF WS-IDLEVNR NOT = SPACE                                          
186600         MOVE WS-IDLEVNR TO W-IDLEVNR                                     
186700         PERFORM IMS-GET-WDF502-LAST                                      
186800       ELSE                                                               
186900         PERFORM IMS-GET-WDF502-OKVAL                                     
187000         IF SEGMENT-FINNS                                                 
187100           MOVE XLEV-IDLEVNR   TO W-IDLEVNR                               
187200           PERFORM IMS-GET-WDF502-LAST                                    
187300         END-IF                                                           
187400       END-IF                                                             
187500     END-IF                                                               
187600                                                                          
187700     PERFORM IMS-GET-BENA-BSEQ                                            
187800                                                                          
187900     PERFORM IMS-GET-BENA-B-TEXT                                          
188000                                                                          
188100     .                                                                    
188200     EJECT                                                                
188300******************************************************************        
188400**   KONTROLL OM ARTIKELNUMRET ÄR EN STRUKTUR.                            
188500******************************************************************        
188600                                                                          
188700 HG-B-KOLLA-SATB-ARTNR SECTION.                                           
188800                                                                          
188900     MOVE W-STRNR     TO WS-STRNR-SPAR                                    
189000     MOVE MID-IDARTNR TO W-STRNR                                          
189100     PERFORM IMS-GU-SATB-STR                                              
189200     IF SEGMENT-FINNS                                                     
189300       MOVE JA TO STRNR-FINNS-SW                                          
189400       IF SATB-STR-TIBORT > 0                                             
189500         MOVE FEL3(SPRAK-IX) TO MOD-TEMFSINF                              
189600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
189700         MOVE NEJ TO INDATA-SW                                            
189800       ELSE                                                               
189900         IF SATB-STR-IDSTRTYP = 'K' OR 'R' OR 'S'                         
190000           PERFORM HG-BB-KOLLA-SATS-I-SATS                                
190100         END-IF                                                           
190200       END-IF                                                             
190300     ELSE                                                                 
190400       IF ART-KDSORT = 'SA' OR 'TM'                                       
190500         MOVE MED39(SPRAK-IX) TO MOD-TEMFSINF                             
190600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
190700         MOVE NEJ TO INDATA-SW                                            
190800       END-IF                                                             
190900     MOVE NEJ TO STRNR-FINNS-SW                                           
191000     MOVE WS-STRNR-SPAR TO W-STRNR                                        
191100     END-IF                                                               
191200     .                                                                    
191300     EJECT                                                                
191400******************************************************************        
191500**   KONTROLLERAR SATS I SATS.                                            
191600******************************************************************        
191700                                                                          
191800 HG-BB-KOLLA-SATS-I-SATS SECTION.                                         
191900                                                                          
192000     PERFORM S12-NOLLST-TAB-STR                                           
192100                                                                          
192200     MOVE 1 TO STRIND                                                     
192300                                                                          
192400     PERFORM IMS-GHU-SATB-STR                                             
192500     IF SEGMENT-FINNS                                                     
192600       PERFORM IMS-GET-SATB-RAD                                           
192700       IF SEGMENT-FINNS                                                   
192800         PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                   
192900           IF SEGMENT-FINNS                                               
193000             MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
193100             MOVE DAGENS-DATUM        TO TMP2-YYMMDD                      
193200             PERFORM WY2000P1                                             
193300             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
193400               IF SATB-RAD-IDARTNR = WS-STRNR-SPAR                        
193500                 MOVE NEJ           TO INDATA-SW                          
193600                 MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF                     
193700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR            
193800               ELSE                                                       
193900                 IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'                 
194000                   PERFORM HG-BBA-LAGRA-I-TAB-STR                         
194100                 END-IF                                                   
194200               END-IF                                                     
194300             END-IF                                                       
194400             IF INDATA-OK                                                 
194500               PERFORM IMS-GET-SATB-RAD                                   
194600             END-IF                                                       
194700           END-IF                                                         
194800         END-PERFORM                                                      
194900                                                                          
195000         IF INDATA-OK                                                     
195100           IF STRIND > 1                                                  
195200             MOVE 1 TO STRIND2                                            
195300             PERFORM UNTIL (STRIND2 > ( STRIND - 1)) OR                   
195400                            INDATA-FEL                                    
195500               MOVE TAB-STR-STRNR(STRIND2) TO W-STRNR                     
195600               PERFORM IMS-GU-SATB-STR                                    
195700               IF SEGMENT-FINNS                                           
195800                 PERFORM IMS-GET-SATB-RAD                                 
195900                 PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)           
196000                   IF SEGMENT-FINNS                                       
196100                     MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD              
196200                     MOVE DAGENS-DATUM        TO TMP2-YYMMDD              
196300                     PERFORM WY2000P1                                     
196400                     IF TMP1-YYMMDD > TMP2-YYMMDD                         
196500                       IF SATB-RAD-IDARTNR = WS-STRNR-SPAR                
196600                         MOVE NEJ         TO INDATA-SW                    
196700                         MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF             
196800                         MOVE MFS-NUM-FAELT-FEL TO                        
196900                                              MOD-IDARTNR-IN-ATTR         
197000                       ELSE                                               
197100                         IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'         
197200                           PERFORM HG-BBA-LAGRA-I-TAB-STR                 
197300                         END-IF                                           
197400                       END-IF                                             
197500                     END-IF                                               
197600                     IF INDATA-OK                                         
197700                       PERFORM IMS-GET-SATB-RAD                           
197800                     END-IF                                               
197900                   END-IF                                                 
198000                 END-PERFORM                                              
198100               END-IF                                                     
198200               ADD 1 TO STRIND2                                           
198300             END-PERFORM                                                  
198400           END-IF                                                         
198500                                                                          
198600           IF INDATA-OK                                                   
198700             PERFORM S12-NOLLST-TAB-STR                                   
198800                                                                          
198900             MOVE 1 TO STRIND                                             
199000                                                                          
199100             MOVE WS-STRNR-SPAR TO W-IDARTNR                              
199200             MOVE SPACE         TO W-IDLEVNR                              
199300                                   W-BELEVART                             
199400             MOVE MID-IDARTNR   TO WS-IDARTNR-SPAR                        
199500             PERFORM IMS-GU-SATB-CSEQ-STR                                 
199600             PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)               
199700               IF SEGMENT-FINNS                                           
199800                 IF CSEQ-STR-IDARTNR > WS-MAX-STRNR-OKONV                 
199900                   MOVE 'GE' TO STATUS-WS                                 
200000                 ELSE                                                     
200100                   IF CSEQ-STR-TIBORT > 0                                 
200200                     PERFORM IMS-GN-SATB-CSEQ-STR                         
200300                   ELSE                                                   
200400                     MOVE CSEQ-RAD-TISTODAT   TO TMP1-YYMMDD              
200500                     MOVE DAGENS-DATUM        TO TMP2-YYMMDD              
200600                     PERFORM WY2000P1                                     
200700                     IF TMP1-YYMMDD > TMP2-YYMMDD                         
200800                       IF CSEQ-STR-IDARTNR = WS-IDARTNR-SPAR              
200900                         MOVE NEJ           TO INDATA-SW                  
201000                         MOVE MED38(SPRAK-IX) TO                          
201100                                             MOD-TEMFSINF                 
201200                         MOVE MFS-NUM-FAELT-FEL TO                        
201300                                               MOD-IDARTNR-IN-ATTR        
201400                       ELSE                                               
201500                         IF CSEQ-STR-IDSTRTYP = 'K' OR 'R' OR 'S'         
201600                           PERFORM HG-BBB-LAGRA-I-TAB-STR                 
201700                         END-IF                                           
201800                         PERFORM IMS-GN-SATB-CSEQ-STR                     
201900                       END-IF                                             
202000                     ELSE                                                 
202100                       PERFORM IMS-GN-SATB-CSEQ-STR                       
202200                     END-IF                                               
202300                   END-IF                                                 
202400                 END-IF                                                   
202500                 IF INDATA-OK                                             
202600                   PERFORM IMS-GN-SATB-CSEQ-STR                           
202700                 END-IF                                                   
202800               END-IF                                                     
202900             END-PERFORM                                                  
203000                                                                          
203100             IF INDATA-OK                                                 
203200               IF STRIND > 1                                              
203300                 MOVE 1 TO STRIND2                                        
203400                 PERFORM UNTIL (STRIND2 > ( STRIND - 1)) OR               
203500                                INDATA-FEL                                
203600                   MOVE TAB-STR-STRNR(STRIND2) TO W-STRNR                 
203700                   PERFORM IMS-GU-SATB-CSEQ-STR                           
203800                   PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)         
203900                     IF SEGMENT-FINNS                                     
204000                       IF CSEQ-STR-IDARTNR > WS-MAX-STRNR-OKONV           
204100                         MOVE 'GE' TO STATUS-WS                           
204200                       ELSE                                               
204300                         IF CSEQ-STR-TIBORT > 0                           
204400                           PERFORM IMS-GN-SATB-CSEQ-STR                   
204500                         ELSE                                             
204600                           MOVE CSEQ-RAD-TISTODAT   TO TMP1-YYMMDD        
204700                           MOVE DAGENS-DATUM        TO TMP2-YYMMDD        
204800                           PERFORM WY2000P1                               
204900                           IF TMP1-YYMMDD > TMP2-YYMMDD                   
205000                             IF CSEQ-STR-IDARTNR = WS-IDARTNR-SPAR        
205100                               MOVE NEJ           TO INDATA-SW            
205200                               MOVE MED38(SPRAK-IX) TO                    
205300                                           MOD-TEMFSINF                   
205400                               MOVE MFS-NUM-FAELT-FEL TO                  
205500                                              MOD-IDARTNR-IN-ATTR         
205600                             ELSE                                         
205700                               IF CSEQ-STR-IDSTRTYP = 'K' OR 'R'          
205800                                                          OR 'S'          
205900                                 PERFORM HG-BBB-LAGRA-I-TAB-STR           
206000                               END-IF                                     
206100                               PERFORM IMS-GN-SATB-CSEQ-STR               
206200                             END-IF                                       
206300                           ELSE                                           
206400                             PERFORM IMS-GN-SATB-CSEQ-STR                 
206500                           END-IF                                         
206600                         END-IF                                           
206700                       END-IF                                             
206800                     END-IF                                               
206900                   END-PERFORM                                            
207000                   ADD 1 TO STRIND2                                       
207100                 END-PERFORM                                              
207200               END-IF                                                     
207300             END-IF                                                       
207400           END-IF                                                         
207500         END-IF                                                           
207600       ELSE                                                               
207700         MOVE NEJ   TO INDATA-SW                                          
207800         MOVE MED50(SPRAK-IX) TO MOD-TEMFSINF                             
207900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                    
208000       END-IF                                                             
208100     END-IF                                                               
208200     MOVE WS-STRNR-SPAR TO W-STRNR                                        
208300     MOVE MID-IDARTNR   TO W-IDARTNR                                      
208400     .                                                                    
208500     EJECT                                                                
208600******************************************************************        
208700**   STRUKTURNUMMER SOM INTE FINNS TIDIGARE I TABELLEN                    
208800**   LÄGGS IN.                                                            
208900******************************************************************        
209000                                                                          
209100 HG-BBA-LAGRA-I-TAB-STR SECTION.                                          
209200                                                                          
209300     MOVE +1 TO K-STRIND                                                  
209400     MOVE NEJ TO STRNR-FINNS-SW                                           
209500     PERFORM UNTIL K-STRIND > STRIND                                      
209600       IF TAB-STR-STRNR(K-STRIND) = SATB-RAD-IDARTNR                      
209700         MOVE JA  TO STRNR-FINNS-SW                                       
209800         MOVE 999 TO K-STRIND                                             
209900       ELSE                                                               
210000         ADD +1   TO K-STRIND                                             
210100       END-IF                                                             
210200     END-PERFORM                                                          
210300                                                                          
210400     IF STRNR-FINNS                                                       
210500       CONTINUE                                                           
210600     ELSE                                                                 
210700       MOVE SATB-RAD-IDARTNR TO TAB-STR-STRNR(STRIND)                     
210800       ADD +1                TO STRIND                                    
210900     END-IF                                                               
211000     .                                                                    
211100     EJECT                                                                
211200******************************************************************        
211300**   STRUKTURNUMMER SOM INTE FINNS TIDIGARE I TABELLEN                    
211400**   LÄGGS IN.                                                            
211500******************************************************************        
211600                                                                          
211700 HG-BBB-LAGRA-I-TAB-STR SECTION.                                          
211800                                                                          
211900     MOVE +1 TO K-STRIND                                                  
212000     MOVE NEJ TO STRNR-FINNS-SW                                           
212100     PERFORM UNTIL K-STRIND > STRIND                                      
212200       IF TAB-STR-STRNR(K-STRIND) = CSEQ-STR-IDARTNR                      
212300         MOVE JA  TO STRNR-FINNS-SW                                       
212400         MOVE 999 TO K-STRIND                                             
212500       ELSE                                                               
212600         ADD +1   TO K-STRIND                                             
212700       END-IF                                                             
212800     END-PERFORM                                                          
212900                                                                          
213000     IF STRNR-FINNS                                                       
213100       CONTINUE                                                           
213200     ELSE                                                                 
213300       MOVE CSEQ-STR-IDARTNR TO TAB-STR-STRNR(STRIND)                     
213400       ADD +1                TO STRIND                                    
213500     END-IF                                                               
213600     .                                                                    
213700     EJECT                                                                
213800******************************************************************        
213900**   NYTT ARTIKELNUMMER SKALL LÄGGAS UPP I RASA.                          
214000******************************************************************        
214100                                                                          
214200 HH-KOLLA-RAD SECTION.                                                    
214300                                                                          
214400     PERFORM IMS-GHNP-SATB-RAD                                            
214500                                                                          
214600     IF SEGMENT-FINNS                                                     
214700       MOVE SATB-RAD-WDJ111 TO WS-RAD-WDJ111                              
214800       MOVE JA TO RADNR-SW                                                
214900     END-IF                                                               
215000     .                                                                    
215100     EJECT                                                                
215200******************************************************************        
215300**   UPPDATERA DET KONVERTERADE STRUKTURNUMRET.                           
215400******************************************************************        
215500                                                                          
215600 I-UPPDATERA SECTION.                                                     
215700                                                                          
215800***  OM RADNUMRET SAKNAS SKAPAS EN NY RAD                                 
215900     IF RADNR-SAKNAS                                                      
216000       MOVE BLANKA-RAD     TO WS-RAD-WDJ111                               
216100       MOVE W-IDRADNR      TO WS-RAD-IDRADNR                              
216200       MOVE MID-IDAO       TO WS-RAD-IDAO-STA                             
216300       IF WS-TIUPPDAT-GAM > 0                                             
216400         MOVE 'N'          TO WS-RAD-KDISATS                              
216500       END-IF                                                             
216600       MOVE DAGENS-DATUM   TO WS-RAD-TIREGDAT                             
216700       MOVE AKTUELLT-DATUM TO WS-RAD-TISTADAT                             
216800       MOVE +999999        TO WS-RAD-TISTODAT                             
216900     END-IF                                                               
217000                                                                          
217100     PERFORM IB-UPPDATERA-RADEN                                           
217200                                                                          
217300     IF STRUKTUR-KLAR                                                     
217400       PERFORM IE-KONV-KLAR                                               
217500     ELSE                                                                 
217600       MOVE FEL15(SPRAK-IX) TO MOD-TEMFSFEL                               
217700       IF UPPDATERAT                                                      
217800         PERFORM IMS-GHU-SATB-STR-K-PCB                                   
217900         IF KONV-STR-IDSTRTYP = 'S'                                       
218000           MOVE W-STRNR TO W-IDARTNR                                      
218100           PERFORM IMS-GET-ARTC-01                                        
218200           IF SEGMENT-FINNS                                               
218300             IF KONV-STR-TIUPPDAT = ZERO                                  
218400                CONTINUE                                                  
218500             ELSE                                                         
218600                IF KONV-STR-IDLEVNR = '1002 '                             
218700                   MOVE JA TO KONV-STR-FLFORPQ                            
218800                ELSE                                                      
218900                   PERFORM IMS-GET-ARTC11                                 
219000                   IF SEGMENT-FINNS                                       
219100                      MOVE CLAG-BEFT TO WS-BEFT-AKTUELL                   
219200                      IF BEFT-AKTUELL                                     
219300                         MOVE JA TO KONV-STR-FLFORPQ                      
219400                      END-IF                                              
219500                   END-IF                                                 
219600                 END-IF                                                   
219700             END-IF                                                       
219800           END-IF                                                         
219900         END-IF                                                           
220000         MOVE DAGENS-DATUM TO KONV-STR-TIREGDAT                           
220100         PERFORM IMS-REPL-SATB-K-PCB                                      
220200       END-IF                                                             
220300     END-IF                                                               
220400                                                                          
220500     IF INDATA-OK                                                         
220600       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
220700       CALL WMEDKONV USING MED-WMEDAREA                                   
220800       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
220900       PERFORM MFS-FORM-ATTR                                              
221000       PERFORM MFS-RENSA-FAELT-IN                                         
221100       IF INGA-NOT                                                        
221200         MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT-IN(1)                       
221300         MOVE MFS-RENSA-FAELT TO MOD-TESTRNOT-IN(2)                       
221400       END-IF                                                             
221500     ELSE                                                                 
221600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
221700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
221800     END-IF                                                               
221900     .                                                                    
222000     EJECT                                                                
222100******************************************************************        
222200**   UPPDATERA RADVÄRDEN.                                                 
222300******************************************************************        
222400                                                                          
222500 IB-UPPDATERA-RADEN SECTION.                                              
222600                                                                          
222700     MOVE '0'       TO WS-RAD-KDSTRRAD                                    
222800                                                                          
222900         IF MID-BORT NOT = ALL '+'                                        
223000           PERFORM IB-C-BORTTAG                                           
223100         ELSE                                                             
223200                                                                          
223300           PERFORM IB-D-UPPDATERA-RADEN                                   
223400         END-IF                                                           
223500                                                                          
223600     PERFORM IB-E-UPPDATERA-NOTERING                                      
223700                                                                          
223800     IF UPPDATERAT                                                        
223900       MOVE JA TO FORP-SW                                                 
224000     END-IF                                                               
224100     .                                                                    
224200     EJECT                                                                
224300******************************************************************        
224400**   UPPDATERA RADVÄRDEN.                                                 
224500******************************************************************        
224600                                                                          
224700 IB-C-BORTTAG SECTION.                                                    
224800                                                                          
224900     PERFORM MFS-ROER-EJ-FAELT-UT                                         
225000                                                                          
225100     IF WS-TIUPPDAT-GAM = ZERO                                            
225200       PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                              
225300       PERFORM IMS-DLET-SATB-K-PCB                                        
225400       MOVE JA TO BORT-SW                                                 
225500     ELSE                                                                 
225600       PERFORM IMS-GU-SATB-STR                                            
225700       PERFORM IMS-GNP-FIRST-KVAL-RAD                                     
225800       IF SEGMENT-FINNS                                                   
225900         MOVE 'U'              TO WS-RAD-KDISATS                          
226000         MOVE MID-IDAO         TO WS-RAD-IDAO-STO                         
226100         MOVE AKTUELLT-DATUM TO WS-RAD-TISTODAT                           
226200         PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                            
226300         MOVE WS-RAD-WDJ111    TO KONV-RAD-WDJ111                         
226400         PERFORM IMS-REPL-SATB-K-PCB                                      
226500         MOVE JA         TO UPPDAT-SW                                     
226600       ELSE                                                               
226700         PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                            
226800         PERFORM IMS-DLET-SATB-K-PCB                                      
226900       END-IF                                                             
227000     END-IF                                                               
227100     .                                                                    
227200     EJECT                                                                
227300******************************************************************        
227400**   UPPDATERA RADVÄRDEN.                                                 
227500******************************************************************        
227600                                                                          
227700 IB-D-UPPDATERA-RADEN SECTION.                                            
227800                                                                          
227900     IF MID-IDARTNR = ALL '+'                                             
228000       CONTINUE                                                           
228100     ELSE                                                                 
228200       MOVE MID-IDARTNR TO WS-RAD-IDARTNR                                 
228300                           MOD-IDARTNR-UT                                 
228400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
228500       MOVE JA TO UPPDAT-SW                                               
228600     END-IF                                                               
228700                                                                          
228800     IF MID-REANTPSA = ALL '+'                                            
228900       CONTINUE                                                           
229000     ELSE                                                                 
229100       IF RADNR-FINNS                                                     
229200         MOVE MID-IDAO         TO WS-RAD-IDAO-STO                         
229300         MOVE 'U'              TO WS-RAD-KDISATS                          
229400         MOVE AKTUELLT-DATUM   TO WS-RAD-TISTODAT                         
229500         PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                            
229600         MOVE WS-RAD-WDJ111    TO KONV-RAD-WDJ111                         
229700         PERFORM IMS-REPL-SATB-K-PCB                                      
229800       END-IF                                                             
229900     END-IF                                                               
230000                                                                          
230100     IF MID-REANTPSA = ALL '+'                                            
230200       CONTINUE                                                           
230300     ELSE                                                                 
230400       IF RADNR-FINNS                                                     
230500         ADD +1                TO WS-RAD-IDRADNR                          
230600         MOVE WS-RAD-IDRADNR   TO W-IDRADNR                               
230700         MOVE W-IDRADNR        TO WS-RADNR4                               
230800         MOVE WS-RADNR4        TO MOD-IDRADNR-UT                          
230900                                  MOD-IDRADNR-B                           
231000         INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE           
231100         MOVE MID-IDAO         TO WS-RAD-IDAO-STA                         
231200         MOVE SPACE            TO WS-RAD-IDAO-STO                         
231300         IF WS-TIUPPDAT-GAM > 0                                           
231400           MOVE 'N'            TO WS-RAD-KDISATS                          
231500         ELSE                                                             
231600           MOVE ' '            TO WS-RAD-KDISATS                          
231700         END-IF                                                           
231800         MOVE WS-REANTPSA      TO WS-RAD-REANTPSA                         
231900                                  MOD-REANTPSA-UT                         
232000         MOVE DAGENS-DATUM     TO WS-RAD-TIREGDAT                         
232100         MOVE AKTUELLT-DATUM   TO WS-RAD-TISTADAT                         
232200         MOVE +999999          TO WS-RAD-TISTODAT                         
232300         MOVE WS-RAD-WDJ111    TO KONV-RAD-WDJ111                         
232400         PERFORM IMS-ISRT-SATB-RAD-K-PCB                                  
232500         MOVE JA  TO UPPDAT-SW                                            
232600         MOVE JA  TO UTSKRIV-SW                                           
232700       ELSE                                                               
232800         MOVE WS-REANTPSA      TO WS-RAD-REANTPSA                         
232900                                  MOD-REANTPSA-UT                         
233000         MOVE JA TO UPPDAT-SW                                             
233100       END-IF                                                             
233200     END-IF                                                               
233300                                                                          
233400     IF MID-UPPDAT NOT = ALL '+'  AND                                     
233500        EJ-UTSKRIVEN                                                      
233600       PERFORM IMS-GHNP-FIRST-SATB-RAD-K-PCB                              
233700       MOVE WS-RAD-WDJ111 TO KONV-RAD-WDJ111                              
233800       IF SEGMENT-FINNS                                                   
233900         PERFORM IMS-REPL-SATB-K-PCB                                      
234000       ELSE                                                               
234100         PERFORM IMS-ISRT-SATB-RAD-K-PCB                                  
234200       END-IF                                                             
234300     END-IF                                                               
234400     .                                                                    
234500     EJECT                                                                
234600******************************************************************        
234700**   UPPDATERA RADVÄRDEN.                                                 
234800******************************************************************        
234900                                                                          
235000 IB-E-UPPDATERA-NOTERING SECTION.                                         
235100                                                                          
235200     IF BORTTAGEN-RAD                                                     
235300       CONTINUE                                                           
235400     ELSE                                                                 
235500       PERFORM IMS-GHNP-FIRST-SATB-NOT-K-PCB                              
235600                                                                          
235700       IF SEGMENT-FINNS                                                   
235800         MOVE KONV-NOT-WDJ122 TO WS-LAGRA-NOT                             
235900       ELSE                                                               
236000         MOVE SPACE         TO WS-LAGRA-NOT                               
236100       END-IF                                                             
236200                                                                          
236300       IF MID-TESTRNOT(1) = ALL '+'                                       
236400         CONTINUE                                                         
236500       ELSE                                                               
236600         MOVE MID-TESTRNOT(1) TO WS-NOT-TESTRNOT(1)                       
236700                              MOD-TESTRNOT-IN(1)                          
236800         MOVE JA            TO NOT-SW                                     
236900       END-IF                                                             
237000                                                                          
237100       IF MID-TESTRNOT(2) = ALL '+'                                       
237200         CONTINUE                                                         
237300       ELSE                                                               
237400         MOVE MID-TESTRNOT(2) TO WS-NOT-TESTRNOT(2)                       
237500                              MOD-TESTRNOT-IN(2)                          
237600         MOVE JA            TO NOT-SW                                     
237700       END-IF                                                             
237800                                                                          
237900       IF NOTERINGAR                                                      
238000         MOVE WS-LAGRA-NOT TO KONV-NOT-WDJ122                             
238100         IF SEGMENT-FINNS                                                 
238200           PERFORM IMS-REPL-SATB-K-PCB                                    
238300         ELSE                                                             
238400           MOVE '1' TO KONV-NOT-IDSTRNOT                                  
238500           PERFORM IMS-ISRT-SATB-NOT-K-PCB                                
238600         END-IF                                                           
238700       END-IF                                                             
238800     END-IF                                                               
238900     .                                                                    
239000     EJECT                                                                
239100******************************************************************        
239200**   KONTROLLERAR OM STRUKTUREN INTE FINNS SEDAN FÖRUT OCH DÅ             
239300**   SKAPAR 2234-TRANSAR FÖR ALL RADER ANNARS                             
239400**   JÄMFÖR KONVERTERAT STRUKTURNUMMER MED DET URSPRUNGLIGA OCH           
239500**   SKAPAR 2234-TRANSAR FÖR ÄNDRINGARNA.                                 
239600**   ALLA SEGMENT SOM FINNS I DEN KONVERTERADE STRUKTUREN FLYTTAS         
239700**   IN I DEN URSPRUNGLIGA.                                               
239800******************************************************************        
239900                                                                          
240000 IE-KONV-KLAR SECTION.                                                    
240100                                                                          
240200     PERFORM IE-A-KOLLA-SATS-I-SATS                                       
240300     IF INDATA-OK                                                         
240400       PERFORM IMS-GHU-SATB-STR                                           
240500       IF SEGMENT-SAKNAS                                                  
240600         PERFORM IMS-GHU-SATB-STR-K-PCB                                   
240700         MOVE KONV-STR-WDJ101 TO SATB-STR-WDJ101                          
240800         MOVE W-STRNR       TO SATB-STR-IDARTNR                           
240900         MOVE DAGENS-DATUM  TO SATB-STR-TIREGDAT                          
241000         MOVE DAGENS-DATUM  TO SATB-STR-TIUPPDAT                          
241100         PERFORM IMS-ISRT-SATB-STR                                        
241200         PERFORM S04-FLYTTA-OMNUM-RADER                                   
241300         PERFORM IE-B-SKAPA-2234-TRANSAR                                  
241400         MOVE JA         TO FORP-SW                                       
241500       ELSE                                                               
241600         PERFORM IMS-GET-SATB-RAD                                         
241700         IF SEGMENT-SAKNAS                                                
241800           PERFORM IMS-GHU-SATB-STR-K-PCB                                 
241900           PERFORM IMS-GHU-SATB-STR                                       
242000           MOVE DAGENS-DATUM TO SATB-STR-TIREGDAT                         
242100           MOVE DAGENS-DATUM TO SATB-STR-TIUPPDAT                         
242200           PERFORM IMS-REPL-SATB                                          
242300           PERFORM S04-FLYTTA-OMNUM-RADER                                 
242400           PERFORM IE-B-SKAPA-2234-TRANSAR                                
242500           MOVE JA         TO FORP-SW                                     
242600         ELSE                                                             
242700           PERFORM IE-C-SKAPA-2234                                        
242800           PERFORM IMS-GHU-SATB-STR                                       
242900           PERFORM IMS-DLET-SATB                                          
243000           PERFORM IMS-GHU-SATB-STR-K-PCB                                 
243100           MOVE SATB-STR-IDUSER TO KONV-STR-IDUSER                        
243200           MOVE SATB-STR-TIREGDAT TO KONV-STR-TIREGDAT                    
243300           MOVE KONV-STR-WDJ101 TO SATB-STR-WDJ101                        
243400           MOVE W-STRNR         TO SATB-STR-IDARTNR                       
243500           MOVE DAGENS-DATUM    TO SATB-STR-TIUPPDAT                      
243600           PERFORM IMS-ISRT-SATB-STR                                      
243700           PERFORM S04-FLYTTA-OMNUM-RADER                                 
243800         END-IF                                                           
243900       END-IF                                                             
244000       IF NY-FORP                                                         
244100         MOVE W-STRNR TO W-IDARTNR                                        
244200         PERFORM IMS-GET-ARTC-01                                          
244300         IF SEGMENT-FINNS                                                 
244400           PERFORM IMS-GHU-SATB-STR                                       
244500           IF SATB-STR-IDSTRTYP = 'S'                                     
244600             IF SATB-STR-TIUPPDAT = ZERO                                  
244700                CONTINUE                                                  
244800             ELSE                                                         
244900                IF SATB-STR-IDLEVNR = '1002 '                             
245000                  MOVE 'J' TO SATB-STR-FLFORPQ                            
245100                  PERFORM IMS-REPL-SATB                                   
245200                ELSE                                                      
245300                  PERFORM IMS-GET-ARTC11                                  
245400                  IF SEGMENT-FINNS                                        
245500                     MOVE CLAG-BEFT TO WS-BEFT-AKTUELL                    
245600                     IF BEFT-AKTUELL                                      
245700                        MOVE 'J' TO SATB-STR-FLFORPQ                      
245800                        PERFORM IMS-REPL-SATB                             
245900                     END-IF                                               
246000                   END-IF                                                 
246100                END-IF                                                    
246200              END-IF                                                      
246300           END-IF                                                         
246400         END-IF                                                           
246500       END-IF                                                             
246600       PERFORM IMS-GHU-SATB-STR-K-PCB                                     
246700       PERFORM IMS-DLET-SATB-K-PCB                                        
246800       MOVE SPACE TO MOD-TEMFSFEL                                         
246900       MOVE NEJ TO KONV-SW                                                
247000     END-IF                                                               
247100     .                                                                    
247200     EJECT                                                                
247300******************************************************************        
247400**   KONTOLLERAR OM SATSEN FINNS I SATSEN                                 
247500******************************************************************        
247600                                                                          
247700 IE-A-KOLLA-SATS-I-SATS SECTION.                                          
247800                                                                          
247900     MOVE W-STRNR TO WS-STRNR-SPAR                                        
248000                                                                          
248100     PERFORM S12-NOLLST-TAB-STR                                           
248200                                                                          
248300     MOVE 1 TO STRIND                                                     
248400                                                                          
248500     PERFORM IMS-GHU-SATB-KONV-STR                                        
248600     IF SEGMENT-FINNS                                                     
248700       PERFORM IMS-GET-SATB-RAD                                           
248800       PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)                     
248900         IF SEGMENT-FINNS                                                 
249000           MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                        
249100           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
249200           PERFORM WY2000P1                                               
249300           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
249400             IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'                     
249500               PERFORM HG-BBA-LAGRA-I-TAB-STR                             
249600             END-IF                                                       
249700           END-IF                                                         
249800           IF INDATA-OK                                                   
249900             PERFORM IMS-GET-SATB-RAD                                     
250000           END-IF                                                         
250100         END-IF                                                           
250200       END-PERFORM                                                        
250300                                                                          
250400       IF INDATA-OK                                                       
250500         IF STRIND > 1                                                    
250600           MOVE 1 TO STRIND2                                              
250700           PERFORM UNTIL (STRIND2 > ( STRIND - 1)) OR                     
250800                          INDATA-FEL                                      
250900             MOVE TAB-STR-STRNR(STRIND2) TO W-STRNR                       
251000             PERFORM IMS-GU-SATB-STR                                      
251100             IF SEGMENT-FINNS                                             
251200               PERFORM IMS-GET-SATB-RAD                                   
251300               PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDATA-FEL)             
251400                 IF SEGMENT-FINNS                                         
251500                   MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                
251600                   MOVE DAGENS-DATUM        TO TMP2-YYMMDD                
251700                   PERFORM WY2000P1                                       
251800                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
251900                     IF SATB-RAD-IDARTNR = WS-STRNR-SPAR                  
252000                       MOVE NEJ           TO INDATA-SW                    
252100                       MOVE MED38(SPRAK-IX) TO MOD-TEMFSINF               
252200                       MOVE MFS-NUM-FAELT-FEL TO                          
252300                                              MOD-IDARTNR-IN-ATTR         
252400                     ELSE                                                 
252500                       IF SATB-RAD-IDSTRTYP = 'K' OR 'R' OR 'S'           
252600                         PERFORM HG-BBA-LAGRA-I-TAB-STR                   
252700                       END-IF                                             
252800                     END-IF                                               
252900                   END-IF                                                 
253000                   IF INDATA-OK                                           
253100                     PERFORM IMS-GET-SATB-RAD                             
253200                   END-IF                                                 
253300                 END-IF                                                   
253400               END-PERFORM                                                
253500             END-IF                                                       
253600             ADD 1 TO STRIND2                                             
253700           END-PERFORM                                                    
253800         END-IF                                                           
253900       END-IF                                                             
254000     END-IF                                                               
254100     MOVE WS-STRNR-SPAR TO W-STRNR                                        
254200     .                                                                    
254300     EJECT                                                                
254400******************************************************************        
254500**   SKAPAR 2234-TRANSAR VID ÖVERFLYTTNING AV EN NY STUKTUR FRÅN          
254600**   KONVERTERAT TILL ORDINARIE STRUKTURNUMMER.                           
254700******************************************************************        
254800                                                                          
254900 IE-B-SKAPA-2234-TRANSAR SECTION.                                         
255000     IF WS-STR-IDLEVNR = '1002 '                                          
255100       MOVE '2233'        TO W-IDHTYP                                     
255200       MOVE W-STRNR       TO 2234-IDARTNR-SATS                            
255300       PERFORM IMS-GHU-SATB-STR                                           
255400       PERFORM IMS-GNP-FIRST-SATB-RAD                                     
255500                                                                          
255600       PERFORM UNTIL SATB-STATUS-CODE = 'GE'                              
255700         MOVE SATB-RAD-IDARTNR TO 2234-IDARTNR-ING                        
255800         PERFORM S15-BERAEKNA-PB-SEP-TOT                                  
255900         IF SATB-RAD-IDARTNR NOT = ZERO                                   
256000           MOVE SATB-RAD-IDARTNR  TO W-IDARTNR                            
256100           MOVE 'N'               TO 2234-KDISATS                         
256200           MOVE WS-PB-SEP-TOT     TO 2234-KVPB-SEP-TOT                    
256300           MOVE SATB-RAD-REANTPSA TO 2234-REANTPSA-NY                     
256400           MOVE ZERO              TO 2234-REANTPSA-GAMMAL                 
256500           MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                        
256600           PERFORM S10-KONV-TIAAMMDD                                      
256700           MOVE WS-TIAAVV         TO 2234-TIBEHDAT                        
256800           PERFORM IMS-ISRT-2234-TRANS                                    
256900           PERFORM S14-UPPDAT-FLIART                                      
257000           PERFORM IMS-GET-SATB-RAD                                       
257100         END-IF                                                           
257200       END-PERFORM                                                        
257300     END-IF                                                               
257400     .                                                                    
257500     EJECT                                                                
257600******************************************************************        
257700**   SKAPAR 2234-TRANSAR FÖR ÄNDRINGARNA.                                 
257800******************************************************************        
257900                                                                          
258000 IE-C-SKAPA-2234 SECTION.                                                 
258100                                                                          
258200     PERFORM IMS-GHU-SATB-STR-K-PCB                                       
258300                                                                          
258400     IF WS-STR-IDLEVNR = '1002 '                                          
258500       MOVE '2233'        TO W-IDHTYP                                     
258600       MOVE W-STRNR       TO 2234-IDARTNR-SATS                            
258700       MOVE ZERO          TO 2234-KVPB-SEP-TOT                            
258800       MOVE ZERO          TO W-IDRADNR                                    
258900       PERFORM IMS-GET-SATB-RAD-K-PCB                                     
259000                                                                          
259100       PERFORM UNTIL SATB-K-STATUS-CODE = 'GE'                            
259200         IF KONV-RAD-KDSTRRAD = '0'                                       
259300           PERFORM S15-BERAEKNA-PB-SEP-TOT                                
259400           MOVE WS-PB-SEP-TOT     TO 2234-KVPB-SEP-TOT                    
259500           MOVE KONV-RAD-IDARTNR  TO 2234-IDARTNR-ING                     
259600           MOVE KONV-RAD-TIREGDAT   TO TMP1-YYMMDD                        
259700           MOVE WS-TIUPPDAT-GAM     TO TMP2-YYMMDD                        
259800           PERFORM WY2000P1                                               
259900           IF TMP1-YYMMDD >= TMP2-YYMMDD   AND                            
260000              KONV-RAD-KDISATS = 'N'                                      
260100             MOVE 'N'                 TO 2234-KDISATS                     
260200             MOVE KONV-RAD-REANTPSA TO 2234-REANTPSA-NY                   
260300             MOVE ZERO                TO 2234-REANTPSA-GAMMAL             
260400             MOVE KONV-RAD-TISTADAT TO DAT-I-TIDATUM                      
260500             PERFORM S10-KONV-TIAAMMDD                                    
260600             MOVE WS-TIAAVV           TO 2234-TIBEHDAT                    
260700             PERFORM IMS-ISRT-2234-TRANS                                  
260800             MOVE JA                  TO FORP-SW                          
260900             MOVE KONV-RAD-IDARTNR    TO W-IDARTNR                        
261000             PERFORM S14-UPPDAT-FLIART                                    
261100           ELSE                                                           
261200             MOVE KONV-RAD-TISTODAT   TO TMP1-YYMMDD                      
261300             MOVE WS-TIUPPDAT-GAM     TO TMP2-YYMMDD                      
261400             PERFORM WY2000P1                                             
261500             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
261600               MOVE KONV-RAD-TISTODAT   TO TMP1-YYMMDD                    
261700               MOVE +999999             TO TMP2-YYMMDD                    
261800               PERFORM WY2000P1                                           
261900               IF TMP1-YYMMDD < TMP2-YYMMDD                               
262000                 IF KONV-RAD-KDISATS = 'U'                                
262100                   MOVE 'U'             TO 2234-KDISATS                   
262200                   MOVE ZERO            TO 2234-REANTPSA-NY               
262300                   MOVE KONV-RAD-REANTPSA TO 2234-REANTPSA-GAMMAL         
262400                   MOVE KONV-RAD-TISTODAT TO DAT-I-TIDATUM                
262500                   PERFORM S10-KONV-TIAAMMDD                              
262600                   MOVE WS-TIAAVV       TO 2234-TIBEHDAT                  
262700                   MOVE JA              TO FORP-SW                        
262800                   PERFORM IMS-ISRT-2234-TRANS                            
262900                 END-IF                                                   
263000               END-IF                                                     
263100             END-IF                                                       
263200           END-IF                                                         
263300         END-IF                                                           
263400         PERFORM IMS-GET-SATB-RAD-K-PCB                                   
263500       END-PERFORM                                                        
263600     END-IF                                                               
263700     .                                                                    
263800     EJECT                                                                
263900******************************************************************        
264000**   VID UPPDATERING KOLLAS OM STRUKTUR ÄR SPÄRRAD.                       
264100**   RÄKNA UT DET KONVERTERADE STRUKTURNUMRET OCH SE OM DET FINNS.        
264200**   FINNS DET OCH USERID STÄMMER OCH DATUM EJ ÖVERSTIGER 2 DAGAR         
264300**   TAS DETTA. STÄMMER USERID KOLLAS DET OM IDET ÄR SPÄRRAT FRÅN         
264400**   ANNAN BILD. STÄMMER INTE USERID LÄGGS EN SIGNAL UT OCH MAN           
264500**   VISAR DET URSPRUNGLIGA STRUKTURNUMRET. ÖVERSTIGER DATERINGEN         
264600**   2 DAGAR TAS DET KONV. STRUKTURNUMRET BORT.                           
264700******************************************************************        
264800                                                                          
264900 S01-KOLLA-STRNR SECTION.                                                 
265000                                                                          
265100     MOVE SPACE TO STRUKTURNR-TYP-SW                                      
265200                                                                          
265300     MOVE W-STRNR TO W-IDARTNR                                            
265400     PERFORM IMS-GET-ARTC-01                                              
265500     IF SEGMENT-FINNS                                                     
265600       MOVE ART-IDLEVNR TO WS-STR-IDLEVNR-ARTC                            
265700     ELSE                                                                 
265800       MOVE HIGH-VALUE  TO WS-STR-IDLEVNR-ARTC                            
265900     END-IF                                                               
266000     MOVE ZERO    TO W-IDARTNR                                            
266100                                                                          
266200     PERFORM IMS-GU-SATB-STR                                              
266300     IF SEGMENT-FINNS                                                     
266400       MOVE JA                TO STRNR-FINNS-SW                           
266500       MOVE SATB-STR-WDJ101   TO WS-LAGRA-STR                             
266600       MOVE SATB-STR-TIUPPDAT TO WS-TIUPPDAT-GAM                          
266700       IF SATB-STR-TIBORT > 0                                             
266800         MOVE BORTTAGET TO STRUKTURNR-TYP-SW                              
266900       END-IF                                                             
267000     ELSE                                                                 
267100       MOVE ZERO TO WS-TIUPPDAT-GAM                                       
267200     END-IF                                                               
267300                                                                          
267400     COMPUTE W-STRNR-KONV = 999999999 - W-STRNR                           
267500     PERFORM IMS-GHU-SATB-KONV-STR                                        
267600     IF SEGMENT-FINNS                                                     
267700       MOVE 001               TO WORK-KDCALL                              
267800       MOVE WC-CDC-SE         TO WORK-IDDC                                
267900       MOVE SATB-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                        
268000       MOVE DAGENS-DATUM      TO WORK-TIAAMMDD-TOM                        
268100       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                      
268200                          WORK-KDSVAR                                     
268300**     STRUKTUREN TAS BORT OM DEN ÄR ÄLDRE ÄN 2 DAGAR                     
268400**     ELLER OM KVWORKD > 999 = KDSVAR-FEL                                
268500       IF WORK-KVWORKD > 2  OR                                            
268600          WORK-KDSVAR-FEL                                                 
268700         PERFORM IMS-DLET-SATB                                            
268800       ELSE                                                               
268900         IF MSG-SIGNON-USERID = SATB-STR-IDUSER                           
269000           PERFORM S16-KOLLA-OM-USER-HAR-LAASNING                         
269100           IF INDATA-OK                                                   
269200             MOVE SATB-STR-WDJ101 TO WS-LAGRA-STR                         
269300             MOVE FEL16(SPRAK-IX) TO MOD-TEMFSFEL                         
269400             MOVE JA TO KONV-SW                                           
269500           ELSE                                                           
269600***********  STRUKTUR KONVERTERAD VIA BILD 1221                           
269700             MOVE SPAERRAT TO STRUKTURNR-TYP-SW                           
269800           END-IF                                                         
269900         ELSE                                                             
270000           MOVE NEJ      TO INDATA-SW                                     
270100           MOVE SPAERRAT TO STRUKTURNR-TYP-SW                             
270200         END-IF                                                           
270300       END-IF                                                             
270400     END-IF                                                               
270500     .                                                                    
270600     EJECT                                                                
270700******************************************************************        
270800**   ALLA SEGMENT SOM FINNS I DEN URSPRUNGLIGA STRUKTUREN LÄSES           
270900**   MED ORDIN. PCB OCH LÄGGS ÖVER I EN KONVERTERAD STRUKTUR I            
271000**   K-PCB.                                                               
271100******************************************************************        
271200                                                                          
271300 S02-KONV-STRUKTUR SECTION.                                               
271400                                                                          
271500     PERFORM IMS-GU-SATB-STR                                              
271600     MOVE SATB-STR-WDJ101   TO KONV-STR-WDJ101                            
271700     MOVE MSG-SIGNON-USERID TO KONV-STR-IDUSER                            
271800     MOVE DAGENS-DATUM      TO KONV-STR-TIREGDAT                          
271900     MOVE W-STRNR-KONV      TO KONV-STR-IDARTNR                           
272000     PERFORM IMS-ISRT-SATB-STR-K-PCB                                      
272100                                                                          
272200     MOVE ZERO TO W-IDRADNR                                               
272300     PERFORM IMS-GNP-SATB-RAD                                             
272400                                                                          
272500     PERFORM UNTIL SEGMENT-SAKNAS                                         
272600       MOVE SATB-RAD-IDRADNR TO W-IDRADNR                                 
272700       MOVE SATB-RAD-WDJ111  TO KONV-RAD-WDJ111                           
272800       PERFORM IMS-ISRT-SATB-RAD-K-PCB                                    
272900       PERFORM IMS-GNP-SATB-NOT                                           
273000       IF SEGMENT-FINNS                                                   
273100         MOVE SATB-NOT-WDJ122 TO KONV-NOT-WDJ122                          
273200         PERFORM IMS-ISRT-SATB-NOT-K-PCB                                  
273300       END-IF                                                             
273400       PERFORM IMS-GNP-SATB-RAD                                           
273500     END-PERFORM                                                          
273600     MOVE WS-IDRADNR  TO W-IDRADNR                                        
273700                                                                          
273800     PERFORM IMS-GU-SATB-STR-K-PCB                                        
273900     .                                                                    
274000     EJECT                                                                
274100******************************************************************        
274200**   ALLA SEGMENT SOM FINNS I DEN KONVERERADE STRUKTUREN LÄSES            
274300**   MED K-PCB OCH LÄGGS ÖVER I DEN ORDINARIE STRUKTUREN I                
274400**   ORDINARIE PCB OCH NUMRERAS OM.                                       
274500******************************************************************        
274600                                                                          
274700 S04-FLYTTA-OMNUM-RADER SECTION.                                          
274800                                                                          
274900     MOVE +0   TO WS-RADNR                                                
275000     PERFORM IMS-GNP-FIRST-SATB-RAD-K-PCB                                 
275100                                                                          
275200     PERFORM UNTIL SEGMENT-SAKNAS                                         
275300         MOVE KONV-RAD-WDJ111 TO SATB-RAD-WDJ111                          
275400         MOVE KONV-RAD-IDRADNR TO WS-RADNR-GAM                            
275500         ADD +10             TO WS-RADNR-NYTT                             
275600         MOVE WS-RADNR-NYTT  TO SATB-RAD-IDRADNR                          
275700                                  W-IDRADNR                               
275800         PERFORM IMS-ISRT-SATB-RAD                                        
275900         MOVE WS-RADNR-GAM   TO W-IDRADNR                                 
276000         PERFORM IMS-GNP-SATB-NOT-K-PCB                                   
276100         IF SEGMENT-FINNS                                                 
276200           MOVE WS-RADNR-NYTT TO W-IDRADNR                                
276300           MOVE KONV-NOT-WDJ122 TO SATB-NOT-WDJ122                        
276400           PERFORM IMS-ISRT-SATB-NOT                                      
276500         END-IF                                                           
276600         PERFORM IMS-GET-SATB-RAD-K-PCB                                   
276700     END-PERFORM                                                          
276800     .                                                                    
276900     EJECT                                                                
277000******************************************************************        
277100**   RÄKNA UT TIFINLV - 5 VECKOR.                                         
277200******************************************************************        
277300                                                                          
277400 S06-SKAPA-DATUM SECTION.                                                 
277500                                                                          
277600     MOVE MID-IDARTNR TO W-IDARTNR                                        
277700                                                                          
277800     PERFORM IMS-GET-ARTC-01                                              
277900     IF SEGMENT-FINNS                                                     
278000       MOVE ART-TIFINLV TO DAT-I-TIDATUM                                  
278100       MOVE 'AAVVD '    TO DAT-KDDATFORM                                  
278200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
278300                            DAT-O-TIDATUM DAT-KDSVAR                      
278400                                                                          
278500       MOVE 003           TO WORK-KDCALL                                  
278600       MOVE DAT-TIAAMMDD  TO WORK-TIAAMMDD-TOM                            
278700       MOVE 26            TO WORK-KVWORKD                                 
278800       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                      
278900                          WORK-KDSVAR                                     
279000     ELSE                                                                 
279100       MOVE 'F'  TO WORK-KDSVAR                                           
279200     END-IF                                                               
279300     .                                                                    
279400     EJECT                                                                
279500******************************************************************        
279600**   KONTROLLERA ÄO OCH AAVV.                                             
279700******************************************************************        
279800                                                                          
279900 S07-KOLLA-AO-AAVV SECTION.                                               
280000                                                                          
280100     MOVE NEJ TO DATUM-KONTROLLERAT                                       
280200     PERFORM S071-KOLLA-NY-STRUKTUR                                       
280300     IF DATUM-KONTROLLERAT = JA                                           
280400        CONTINUE                                                          
280500     ELSE                                                                 
280600        IF MID-TIAAVV = ALL '+' OR ZERO                                   
280700          PERFORM S06-SKAPA-DATUM                                         
280800          IF WORK-KDSVAR-OK                                               
280900             MOVE WORK-TIAAMMDD-FOM    TO TMP1-YYMMDD                     
281000             MOVE INNEVARANDE-VECKA   TO TMP2-YYMMDD                      
281100             PERFORM WY2000P1                                             
281200             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
281300                MOVE DAGENS-DATUM      TO AKTUELLT-DATUM                  
281400             ELSE                                                         
281500                MOVE WORK-TIAAMMDD-FOM  TO AKTUELLT-DATUM                 
281600             END-IF                                                       
281700             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR               
281800          ELSE                                                            
281900             MOVE DAGENS-DATUM TO AKTUELLT-DATUM                          
282000          END-IF                                                          
282100        ELSE                                                              
282200          PERFORM S13-KONV-TIAAVV                                         
282300          MOVE AKTUELLT-DATUM    TO TMP1-YYMMDD                           
282400          MOVE INNEVARANDE-VECKA TO TMP2-YYMMDD                           
282500          PERFORM WY2000P1                                                
282600          IF DAT-KDSVAR-FEL OR                                            
282700            TMP1-YYMMDD < TMP2-YYMMDD                                     
282800            IF EJ-TID-SIGNAL                                              
282900               MOVE MED24(SPRAK-IX) TO MOD-TEMFSINF                       
283000            END-IF                                                        
283100            MOVE MFS-NUM-FAELT-FEL    TO MOD-TIAAVV-IN-ATTR               
283200            MOVE NEJ TO INDATA-SW                                         
283300          ELSE                                                            
283400            MOVE AKTUELLT-DATUM   TO TMP1-YYMMDD                          
283500            MOVE DAGENS-DATUM     TO TMP2-YYMMDD                          
283600            PERFORM WY2000P1                                              
283700            IF TMP1-YYMMDD < TMP2-YYMMDD                                  
283800              MOVE DAGENS-DATUM TO AKTUELLT-DATUM                         
283900            END-IF                                                        
284000            MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR                
284100          END-IF                                                          
284200        END-IF                                                            
284300     END-IF                                                               
284400                                                                          
284500     IF MID-IDAO = ALL '+' OR SPACE                                       
284600       MOVE SPACE                TO MID-IDAO                              
284700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                      
284800     ELSE                                                                 
284900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR                      
285000       IF MID-TIAAVV = ALL '+'                                            
285100         IF EJ-TID-SIGNAL                                                 
285200           MOVE MED25(SPRAK-IX) TO MOD-TEMFSINF                           
285300         END-IF                                                           
285400         MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR                     
285500         MOVE NEJ TO INDATA-SW                                            
285600       END-IF                                                             
285700     END-IF                                                               
285800     .                                                                    
285900     EJECT                                                                
286000 S071-KOLLA-NY-STRUKTUR SECTION.                                          
286100******************************************************************        
286200*  OM STRUKTUREN ÄR NY OCH FÖRSTA INLEVERANS LIGGER FRAMÅT I TIDEN        
286300*  SÄTTS RADERNAS TISTADAT TILL DAGENS-DATUM OM INTE INMATAD DATUM        
286400*  ÄR > FÖRSTA INLEVERANS PÅ STRUKTUREN.                                  
286500******************************************************************        
286600*                                                                         
286700     IF WS-TIUPPDAT-GAM = ZERO                                            
286800        MOVE WS-STRNR TO W-STRNR                                          
286900        PERFORM IMS-GU-ARTC01                                             
287000        IF SEGMENT-FINNS                                                  
287100           MOVE ART-TIFINLV    TO DAT-I-TIDATUM                           
287200           MOVE 'AAVVD '       TO DAT-KDDATFORM                           
287300           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
287400                               DAT-O-TIDATUM DAT-KDSVAR                   
287500           MOVE DAT-TIAAMMDD   TO WS-SPAR-TIFINLV                         
287600           MOVE WS-SPAR-TIFINLV   TO TMP1-YYMMDD                          
287700           MOVE DAGENS-DATUM      TO TMP2-YYMMDD                          
287800           PERFORM WY2000P1                                               
287900           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
288000              IF MID-TIAAVV = ALL '+' OR ZERO                             
288100                 MOVE DAGENS-DATUM TO AKTUELLT-DATUM                      
288200                 MOVE JA TO DATUM-KONTROLLERAT                            
288300              ELSE                                                        
288400                 MOVE MID-TIAAVV     TO DAT-I-TIDATUM                     
288500                 MOVE 'AAVV  '       TO DAT-KDDATFORM                     
288600                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
288700                                     DAT-O-TIDATUM DAT-KDSVAR             
288800                 IF DAT-KDSVAR-OK                                         
288900                   MOVE DAT-TIAAMMDD      TO TMP1-YYMMDD                  
289000                   MOVE WS-SPAR-TIFINLV   TO TMP2-YYMMDD                  
289100                   PERFORM WY2000P1                                       
289200                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
289300                      MOVE DAT-TIAAMMDD TO AKTUELLT-DATUM                 
289400                   ELSE                                                   
289500                      MOVE DAGENS-DATUM TO AKTUELLT-DATUM                 
289600                   END-IF                                                 
289700                   MOVE JA TO DATUM-KONTROLLERAT                          
289800                   MOVE MFS-NUM-FAELT-RAETT TO MOD-TIAAVV-IN-ATTR         
289900                 ELSE                                                     
290000                   IF EJ-TID-SIGNAL                                       
290100                      MOVE MED24(SPRAK-IX) TO MOD-TEMFSINF                
290200                   END-IF                                                 
290300                   MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-ATTR           
290400                   MOVE NEJ TO INDATA-SW                                  
290500                 END-IF                                                   
290600              END-IF                                                      
290700            END-IF                                                        
290800         END-IF                                                           
290900     END-IF                                                               
291000     .                                                                    
291100     EJECT                                                                
291200******************************************************************        
291300**   KONVERTERAR DATUM FRÅN AAMMDD TILL AAVV.                             
291400******************************************************************        
291500                                                                          
291600 S10-KONV-TIAAMMDD SECTION.                                               
291700                                                                          
291800     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
291900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
292000                         DAT-O-TIDATUM DAT-KDSVAR                         
292100     MOVE DAT-TIAA-VECKA           TO WS-TIAA                             
292200     MOVE DAT-TIVV                 TO WS-TIVV                             
292300                                                                          
292400     .                                                                    
292500     EJECT                                                                
292600******************************************************************        
292700**   KONTROLL OM USER-ID REDAN HAR ETT KONVERTERAT STRUKTURNR.            
292800******************************************************************        
292900                                                                          
293000 S11-HAR-USERID-KONV-STR SECTION.                                         
293100                                                                          
293200     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
293300                                                                          
293400     PERFORM IMS-GHN-SATB-KONV-IDUSER-STR                                 
293500     PERFORM UNTIL SEGMENT-SAKNAS                                         
293600       IF SEGMENT-FINNS                                                   
293700         MOVE 001               TO WORK-KDCALL                            
293800         MOVE WC-CDC-SE         TO WORK-IDDC                              
293900         MOVE SATB-STR-TIREGDAT TO WORK-TIAAMMDD-FOM                      
294000         MOVE DAGENS-DATUM      TO WORK-TIAAMMDD-TOM                      
294100         CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                    
294200                             WORK-KDSVAR                                  
294300         IF WORK-KVWORKD > 2                                              
294400           MOVE W-STRNR          TO WS-STRNR-SPAR                         
294500           MOVE SATB-STR-IDARTNR TO W-STRNR                               
294600           PERFORM IMS-GHU-SATB-STR                                       
294700           PERFORM IMS-DLET-SATB                                          
294800           MOVE WS-STRNR-SPAR    TO W-STRNR                               
294900           PERFORM IMS-GHN-SATB-KONV-IDUSER-STR                           
295000         ELSE                                                             
295100           IF SATB-STR-IDARTNR NOT = W-STRNR-KONV                         
295200             MOVE FEL17(SPRAK-IX) TO MOD-TEMFSFEL                         
295300             MOVE 'GE'          TO STATUS-WS                              
295400             MOVE NEJ           TO INDATA-SW                              
295500           ELSE                                                           
295600             PERFORM IMS-GHN-SATB-KONV-IDUSER-STR                         
295700           END-IF                                                         
295800         END-IF                                                           
295900       END-IF                                                             
296000     END-PERFORM                                                          
296100     .                                                                    
296200     EJECT                                                                
296300******************************************************************        
296400**   NOLLSTÄLLNING AV STRUKTURTABELLEN.                                   
296500******************************************************************        
296600                                                                          
296700 S12-NOLLST-TAB-STR SECTION.                                              
296800                                                                          
296900     MOVE +1 TO STRIND                                                    
297000     PERFORM UNTIL STRIND > TAB-STR-MAX                                   
297100       MOVE +0 TO TAB-STR-STRNR(STRIND)                                   
297200       ADD +1  TO STRIND                                                  
297300     END-PERFORM                                                          
297400     .                                                                    
297500     EJECT                                                                
297600******************************************************************        
297700**   KONVERTERA AAVV TILL AAMMDD.                                         
297800******************************************************************        
297900                                                                          
298000 S13-KONV-TIAAVV SECTION.                                                 
298100                                                                          
298200     MOVE MID-TIAAVV      TO DAT-I-TIDATUM                                
298300     MOVE 'AAVV  '        TO DAT-KDDATFORM                                
298400     CALL WDATKONV  USING DAT-KDDATFORM DAT-I-TIDATUM                     
298500                          DAT-O-TIDATUM DAT-KDSVAR                        
298600     MOVE DAT-TIAAMMDD    TO AKTUELLT-DATUM                               
298700     .                                                                    
298800     EJECT                                                                
298900******************************************************************        
299000**   ÄNDRAR FLIART TILL 'J' OM DEN INTE REDAN ÄR DET.                     
299100******************************************************************        
299200                                                                          
299300 S14-UPPDAT-FLIART SECTION.                                               
299400                                                                          
299500     PERFORM IMS-GHU-ARTC-01                                              
299600                                                                          
299700     IF SEGMENT-FINNS                                                     
299800       IF ART-FLIART = 'N'                                                
299900         MOVE JA TO ART-FLIART                                            
300000         PERFORM IMS-REPL-ARTC                                            
300100       END-IF                                                             
300200     END-IF                                                               
300300     .                                                                    
300400     EJECT                                                                
300500******************************************************************        
300600**   BERÄKNA PB-SEP TOTALT C1                                             
300700******************************************************************        
300800                                                                          
300900 S15-BERAEKNA-PB-SEP-TOT SECTION.                                         
301000                                                                          
301100     MOVE ZERO TO WS-PB-SEP-TOT                                           
301200                                                                          
301300****************                                                          
301400*    MOVE KONV-RAD-IDARTNR TO W-IDARTNR                                   
301500*    PERFORM IMS-GET-ARTC-01                                              
301600*                                                                         
301700*    IF SEGMENT-FINNS                                                     
301800*      PERFORM IMS-GET-ARTC11                                             
301900*      IF SEGMENT-FINNS                                                   
302000*        ADD CLAG-KVPB-SEP TO WS-PB-SEP-TOT                               
302100*      END-IF                                                             
302200*    END-IF                                                               
302300****************                                                          
302400     .                                                                    
302500     EJECT                                                                
302600 S16-KOLLA-OM-USER-HAR-LAASNING SECTION.                                  
302700                                                                          
302800     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
302900     MOVE '1151'            TO W-IDHTYPX                                  
303000     PERFORM IMS-GET-XXAZ01                                               
303100                                                                          
303200     PERFORM IMS-GHNP-XXAZ11                                              
303300     PERFORM UNTIL SEGMENT-SAKNAS                                         
303400       IF SEGMENT-FINNS                                                   
303500         MOVE 001                TO WORK-KDCALL                           
303600         MOVE WC-CDC-SE          TO WORK-IDDC                             
303700         MOVE XXAZ-1152-TIREGDAT TO WORK-TIAAMMDD-FOM                     
303800         MOVE DAGENS-DATUM       TO WORK-TIAAMMDD-TOM                     
303900         CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                    
304000                             WORK-KDSVAR                                  
304100         IF WORK-KVWORKD > 2 OR                                           
304200            WORK-KDSVAR-FEL                                               
304300           PERFORM IMS-DLET-XXAZ                                          
304400           PERFORM IMS-GHNP-XXAZ11                                        
304500         ELSE                                                             
304600           MOVE 'GE' TO STATUS-WS                                         
304700           MOVE NEJ    TO INDATA-SW                                       
304800         END-IF                                                           
304900       END-IF                                                             
305000     END-PERFORM                                                          
305100     .                                                                    
305200     EJECT                                                                
305300 S17-KOLLA-BEART-KDHOM-AENDRING SECTION.                                  
305400                                                                          
305500       IF INDATA-OK                                                       
305600         PERFORM IMS-GET-BENA-ASEQ                                        
305700         IF SEGMENT-FINNS                                                 
305800           IF BENA-BEN-KDBENSTAT < 2                                      
305900             CONTINUE                                                     
306000           ELSE                                                           
306100             MOVE NEJ                TO INDATA-SW                         
306200           END-IF                                                         
306300         ELSE                                                             
306400           MOVE MED3(SPRAK-IX) TO MOD-TEMFSINF                            
306500           MOVE NEJ                TO INDATA-SW                           
306600         END-IF                                                           
306700                                                                          
306800         IF INDATA-OK                                                     
306900           PERFORM IMS-GET-BENA-ASEQ                                      
307000           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
307100                           (BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM)         
307200             IF SEGMENT-FINNS                                             
307300               IF BENA-BEN-KDHOMONYM = WS-KDBENHOM-NUM                    
307400                 CONTINUE                                                 
307500               ELSE                                                       
307600                 PERFORM IMS-GN-BENA-ASEQ                                 
307700               END-IF                                                     
307800             END-IF                                                       
307900           END-PERFORM                                                    
308000                                                                          
308100           IF SEGMENT-FINNS                                               
308200             CONTINUE                                                     
308300           ELSE                                                           
308400             MOVE NEJ               TO INDATA-SW                          
308500           END-IF                                                         
308600         END-IF                                                           
308700                                                                          
308800         IF INDATA-OK                                                     
308900           MOVE WS-IDSKYLT     TO W-IDSKYLT                               
309000*******************************************                               
309100*   SPARA UNDAN SVENSK ARTIKELBENÄMNING   *                               
309200*******************************************                               
309300           IF WS-IDSKYLT = 'S  '                                          
309400             CONTINUE                                                     
309500           ELSE                                                           
309600             MOVE 'S  ' TO W-IDSKYLT                                      
309700             PERFORM IMS-GET-BENA-A-TEXT                                  
309800             MOVE BENA-TEXT-BEART TO WS-BEART                             
309900           END-IF                                                         
310000         END-IF                                                           
310100       END-IF                                                             
310200                                                                          
310300     .                                                                    
310400     EJECT                                                                
310500 MFS-RENSA-FAELT-UT SECTION.                                              
310600                                                                          
310700*    --- ALLA UTDATA-FÄLT                                                 
310800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
310900                             MOD-REANTPSA-UT                              
311000                             MOD-TESTRNOT-IN(1)                           
311100                             MOD-TESTRNOT-IN(2)                           
311200     .                                                                    
311300     SKIP2                                                                
311400 MFS-RENSA-FAELT-IN SECTION.                                              
311500                                                                          
311600*    --- ALLA INDATA-FÄLT                                                 
311700     MOVE MFS-RENSA-FAELT TO                                              
311800                             MOD-IDAO-IN                                  
311900                             MOD-KLAR-IN                                  
312000                             MOD-BORT-IN                                  
312100     .                                                                    
312200     EJECT                                                                
312300 MFS-ROER-EJ-FAELT-UT SECTION.                                            
312400                                                                          
312500*    --- ALLA UTDATA-FÄLT                                                 
312600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
312700                               MOD-REANTPSA-UT                            
312800                               MOD-TESTRNOT-IN(1)                         
312900                               MOD-TESTRNOT-IN(2)                         
313000     .                                                                    
313100     SKIP2                                                                
313200 MFS-ROER-EJ-FAELT-IN SECTION.                                            
313300                                                                          
313400*    --- ALLA INDATA-FÄLT                                                 
313500     MOVE MFS-ROER-EJ-FAELT TO                                            
313600                               MOD-IDAO-IN                                
313700                               MOD-KLAR-IN                                
313800                               MOD-BORT-IN                                
313900     .                                                                    
314000     EJECT                                                                
314100 MFS-FORM-ATTR SECTION.                                                   
314200                                                                          
314300*    --- ALLA INDATA-FÄLT                                                 
314400     MOVE MFS-FORMATETS-ATTR TO                                           
314500                                 MOD-IDARTNR-IN-ATTR                      
314600                                 MOD-REANTPSA-IN-ATTR                     
314700                                 MOD-IDAO-IN-ATTR                         
314800                                 MOD-TIAAVV-IN-ATTR                       
314900                                 MOD-KLAR-IN-ATTR                         
315000                                 MOD-BORT-IN-ATTR                         
315100                                 MOD-TESTRNOT-IN-ATTR(1)                  
315200                                 MOD-TESTRNOT-IN-ATTR(2)                  
315300     .                                                                    
315400     SKIP2                                                                
315500 MFS-LAS-IN-IGEN SECTION.                                                 
315600                                                                          
315700*    --- ALLA INDATA-FÄLT                                                 
315800     MOVE MFS-ADD-LAES-IN-FAELT TO                                        
315900                                   MOD-IDARTNR-IN-ATTR                    
316000                                   MOD-REANTPSA-IN-ATTR                   
316100                                   MOD-IDAO-IN-ATTR                       
316200                                   MOD-TIAAVV-IN-ATTR                     
316300                                   MOD-KLAR-IN-ATTR                       
316400                                   MOD-BORT-IN-ATTR                       
316500                                   MOD-TESTRNOT-IN-ATTR(1)                
316600                                   MOD-TESTRNOT-IN-ATTR(2)                
316700     .                                                                    
316800     EJECT                                                                
316900* --- IMS SEKTIONER ---                                                   
317000     SKIP3                                                                
317100 IMS-GET-MSG SECTION.                                                     
317200                                                                          
317300     MOVE '  QC' TO GODK-STATUSKODER                                      
317400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
317500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
317600     PERFORM IMS-STATUSKONTROLL                                           
317700     .                                                                    
317800     SKIP3                                                                
317900 IMS-INSERT-MSG SECTION.                                                  
318000                                                                          
318100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
318200     MOVE SPACE TO GODK-STATUSKODER                                       
318300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
318400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
318500     PERFORM IMS-STATUSKONTROLL                                           
318600     .                                                                    
318700     EJECT                                                                
318800 IMS-GET-BENA-ASEQ SECTION.                                               
318900                                                                          
319000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
319100                                  W-BEART-X ')'                           
319200          DELIMITED BY SIZE INTO SSA1                                     
319300     MOVE '  GE' TO GODK-STATUSKODER                                      
319400     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
319500     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
319600     PERFORM IMS-STATUSKONTROLL                                           
319700     .                                                                    
319800     SKIP3                                                                
319900 IMS-GN-BENA-ASEQ SECTION.                                                
320000                                                                          
320100     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
320200                                  W-BEART-X ')'                           
320300          DELIMITED BY SIZE INTO SSA1                                     
320400     MOVE '  GE' TO GODK-STATUSKODER                                      
320500     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
320600     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
320700     PERFORM IMS-STATUSKONTROLL                                           
320800     .                                                                    
320900     EJECT                                                                
321000 IMS-GET-BENA-BSEQ SECTION.                                               
321100                                                                          
321200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
321300          DELIMITED BY SIZE INTO SSA1                                     
321400     MOVE '  GE' TO GODK-STATUSKODER                                      
321500     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
321600     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
321700     PERFORM IMS-STATUSKONTROLL                                           
321800     .                                                                    
321900     SKIP3                                                                
322000 IMS-GET-BENA-A-TEXT SECTION.                                             
322100                                                                          
322200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
322300          DELIMITED BY SIZE INTO SSA1                                     
322400     MOVE '  GE' TO GODK-STATUSKODER                                      
322500     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
322600     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
322700     PERFORM IMS-STATUSKONTROLL                                           
322800     .                                                                    
322900     SKIP3                                                                
323000 IMS-GET-BENA-B-TEXT SECTION.                                             
323100                                                                          
323200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
323300          DELIMITED BY SIZE INTO SSA1                                     
323400     MOVE '  GE' TO GODK-STATUSKODER                                      
323500     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
323600     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
323700     PERFORM IMS-STATUSKONTROLL                                           
323800     .                                                                    
323900     EJECT                                                                
324000 IMS-GET-ARTC-01 SECTION.                                                 
324100                                                                          
324200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
324300          DELIMITED BY SIZE INTO SSA1                                     
324400     MOVE '  GE' TO GODK-STATUSKODER                                      
324500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC SSA1                 
324600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
324700     PERFORM IMS-STATUSKONTROLL                                           
324800     .                                                                    
324900     SKIP3                                                                
325000 IMS-GU-ARTC01 SECTION.                                                   
325100                                                                          
325200     STRING 'WLARTC01(IDARTNR  =' W-STRNR-X ')'                           
325300          DELIMITED BY SIZE INTO SSA1                                     
325400     MOVE '  GE' TO GODK-STATUSKODER                                      
325500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC SSA1                 
325600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
325700     PERFORM IMS-STATUSKONTROLL                                           
325800     .                                                                    
325900     SKIP3                                                                
326000 IMS-GHU-ARTC-01 SECTION.                                                 
326100                                                                          
326200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
326300          DELIMITED BY SIZE INTO SSA1                                     
326400     MOVE '  GE' TO GODK-STATUSKODER                                      
326500     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-ARTC SSA1                
326600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
326700     PERFORM IMS-STATUSKONTROLL                                           
326800     .                                                                    
326900     EJECT                                                                
327000 IMS-GET-ARTC11 SECTION.                                                  
327100                                                                          
327200     MOVE 'WLARTC11 ' TO SSA1                                             
327300     MOVE '  GE' TO GODK-STATUSKODER                                      
327400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC SSA1                
327500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
327600     PERFORM IMS-STATUSKONTROLL                                           
327700     .                                                                    
327800     EJECT                                                                
327900 IMS-GET-WDF501  SECTION.                                                 
328000                                                                          
328100     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
328200          DELIMITED BY SIZE INTO SSA1                                     
328300     MOVE '  GE' TO GODK-STATUSKODER                                      
328400     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
328500     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
328600     PERFORM IMS-STATUSKONTROLL                                           
328700     .                                                                    
328800     SKIP3                                                                
328900 IMS-GET-WDF502-LAST SECTION.                                             
329000                                                                          
329100     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
329200          DELIMITED BY SIZE INTO SSA1                                     
329300     MOVE '  GE' TO GODK-STATUSKODER                                      
329400     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
329500     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
329600     PERFORM IMS-STATUSKONTROLL                                           
329700     .                                                                    
329800     SKIP3                                                                
329900 IMS-GET-WDF502-OKVAL SECTION.                                            
330000                                                                          
330100     MOVE 'WDF502  ' TO SSA1                                              
330200     MOVE '  GE' TO GODK-STATUSKODER                                      
330300     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
330400     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
330500     PERFORM IMS-STATUSKONTROLL                                           
330600     .                                                                    
330700     EJECT                                                                
330800 IMS-GU-SATB-STR SECTION.                                                 
330900                                                                          
331000     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
331100          DELIMITED BY SIZE INTO SSA1                                     
331200     MOVE '  GE' TO GODK-STATUSKODER                                      
331300     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
331400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
331500     PERFORM IMS-STATUSKONTROLL                                           
331600     .                                                                    
331700     SKIP3                                                                
331800 IMS-GU-SATB-STR-K-PCB SECTION.                                           
331900                                                                          
332000     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
332100          DELIMITED BY SIZE INTO SSA1                                     
332200     MOVE '  GE' TO GODK-STATUSKODER                                      
332300     CALL CBLTDLI USING GU SATB-K-PCB DLI-IO-AREA-K SSA1                  
332400     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
332500     PERFORM IMS-STATUSKONTROLL                                           
332600     .                                                                    
332700     EJECT                                                                
332800 IMS-GU-SATB-CSEQ-STR SECTION.                                            
332900                                                                          
333000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
333100                                    W-BELEVART-X                          
333200                                    W-IDARTNR-X ')'                       
333300          DELIMITED BY SIZE INTO SSA1                                     
333400     MOVE 'WLSATB01 ' TO SSA2                                             
333500     MOVE '  GE' TO GODK-STATUSKODER                                      
333600     CALL CBLTDLI USING GU SATB-C-PCB DLI-IO-AREA-CSEQ SSA1 SSA2          
333700     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
333800     PERFORM IMS-STATUSKONTROLL                                           
333900     .                                                                    
334000     SKIP3                                                                
334100 IMS-GN-SATB-CSEQ-STR SECTION.                                            
334200                                                                          
334300     STRING 'WLSATB11*D(WDJ1CSEQ =' W-IDLEVNR-X                           
334400                                    W-BELEVART-X                          
334500                                    W-IDARTNR-X ')'                       
334600          DELIMITED BY SIZE INTO SSA1                                     
334700     MOVE 'WLSATB01 ' TO SSA2                                             
334800     MOVE '  GE' TO GODK-STATUSKODER                                      
334900     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA-CSEQ SSA1 SSA2          
335000     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
335100     PERFORM IMS-STATUSKONTROLL                                           
335200     .                                                                    
335300     EJECT                                                                
335400 IMS-GHU-SATB-STR SECTION.                                                
335500                                                                          
335600     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
335700          DELIMITED BY SIZE INTO SSA1                                     
335800     MOVE '  GE' TO GODK-STATUSKODER                                      
335900     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
336000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
336100     PERFORM IMS-STATUSKONTROLL                                           
336200     .                                                                    
336300     SKIP3                                                                
336400 IMS-GHU-SATB-KONV-STR SECTION.                                           
336500                                                                          
336600     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
336700          DELIMITED BY SIZE INTO SSA1                                     
336800     MOVE '  GE' TO GODK-STATUSKODER                                      
336900     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
337000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
337100     PERFORM IMS-STATUSKONTROLL                                           
337200     .                                                                    
337300     SKIP3                                                                
337400 IMS-GHU-SATB-STR-K-PCB SECTION.                                          
337500                                                                          
337600     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
337700          DELIMITED BY SIZE INTO SSA1                                     
337800     MOVE '  GE' TO GODK-STATUSKODER                                      
337900     CALL CBLTDLI USING GHU SATB-K-PCB DLI-IO-AREA-K SSA1                 
338000     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
338100     PERFORM IMS-STATUSKONTROLL                                           
338200     .                                                                    
338300     EJECT                                                                
338400 IMS-GHN-SATB-KONV-IDUSER-STR SECTION.                                    
338500                                                                          
338600     STRING 'WLSATB01(WDJ1DSEQ>=' W-IDUSER-X                              
338700                                  W-STRNR-KONV-MIN-X                      
338800                    '&WDJ1DSEQ<=' W-IDUSER-X                              
338900                                  W-STRNR-KONV-MAX-X ')'                  
339000          DELIMITED BY SIZE INTO SSA1                                     
339100     MOVE '  GE' TO GODK-STATUSKODER                                      
339200     CALL CBLTDLI USING GHN SATB-D-PCB DLI-IO-AREA SSA1                   
339300     MOVE SATB-D-STATUS-CODE TO STATUS-WS                                 
339400     PERFORM IMS-STATUSKONTROLL                                           
339500     .                                                                    
339600     SKIP3                                                                
339700 IMS-GET-SATB-RAD SECTION.                                                
339800                                                                          
339900     MOVE 'WLSATB11 ' TO SSA1                                             
340000     MOVE '  GE' TO GODK-STATUSKODER                                      
340100     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
340200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
340300     PERFORM IMS-STATUSKONTROLL                                           
340400     .                                                                    
340500     SKIP3                                                                
340600 IMS-GET-SATB-RAD-K-PCB SECTION.                                          
340700                                                                          
340800     MOVE 'WLSATB11 ' TO SSA1                                             
340900     MOVE '  GE' TO GODK-STATUSKODER                                      
341000     CALL CBLTDLI USING GNP SATB-K-PCB DLI-IO-AREA-K SSA1                 
341100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
341200     PERFORM IMS-STATUSKONTROLL                                           
341300     .                                                                    
341400     EJECT                                                                
341500 IMS-GNP-SATB-RAD SECTION.                                                
341600                                                                          
341700     STRING 'WLSATB11(WDJ111KY=>' W-WDJ111KY-X ')'                        
341800          DELIMITED BY SIZE INTO SSA1                                     
341900     MOVE '  GE' TO GODK-STATUSKODER                                      
342000     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
342100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
342200     PERFORM IMS-STATUSKONTROLL                                           
342300     .                                                                    
342400     SKIP3                                                                
342500 IMS-GNP-FIRST-SATB-RAD SECTION.                                          
342600                                                                          
342700     MOVE 'WLSATB11*F' TO SSA1                                            
342800     MOVE '  GE' TO GODK-STATUSKODER                                      
342900     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
343000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
343100     PERFORM IMS-STATUSKONTROLL                                           
343200     .                                                                    
343300     SKIP3                                                                
343400 IMS-GNP-FIRST-SATB-RAD-K-PCB SECTION.                                    
343500                                                                          
343600     MOVE 'WLSATB11*F' TO SSA1                                            
343700     MOVE '  GE' TO GODK-STATUSKODER                                      
343800     CALL CBLTDLI USING GNP SATB-K-PCB DLI-IO-AREA-K SSA1                 
343900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
344000     PERFORM IMS-STATUSKONTROLL                                           
344100     .                                                                    
344200     EJECT                                                                
344300 IMS-GNP-FIRST-KVAL-RAD SECTION.                                          
344400                                                                          
344500     STRING 'WLSATB11*F(WDJ111KY =' W-WDJ111KY-X ')'                      
344600          DELIMITED BY SIZE INTO SSA1                                     
344700     MOVE '  GE' TO GODK-STATUSKODER                                      
344800     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
344900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
345000     PERFORM IMS-STATUSKONTROLL                                           
345100     .                                                                    
345200     SKIP3                                                                
345300 IMS-GU-SATB-RAD SECTION.                                                 
345400                                                                          
345500     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
345600          DELIMITED BY SIZE INTO SSA1                                     
345700     MOVE '  GE' TO GODK-STATUSKODER                                      
345800     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
345900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
346000     PERFORM IMS-STATUSKONTROLL                                           
346100     .                                                                    
346200     SKIP3                                                                
346300 IMS-GU-SATB-FORP-RAD SECTION.                                            
346400                                                                          
346500     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY2-X ')'                       
346600          DELIMITED BY SIZE INTO SSA1                                     
346700     MOVE '  GE' TO GODK-STATUSKODER                                      
346800     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
346900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
347000     PERFORM IMS-STATUSKONTROLL                                           
347100     .                                                                    
347200     EJECT                                                                
347300 IMS-GHNP-FIRST-SATB-RAD-K-PCB SECTION.                                   
347400                                                                          
347500     STRING 'WLSATB11*F(WDJ111KY =' W-WDJ111KY-X ')'                      
347600          DELIMITED BY SIZE INTO SSA1                                     
347700     MOVE '  GE' TO GODK-STATUSKODER                                      
347800     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-K SSA1                
347900     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
348000     PERFORM IMS-STATUSKONTROLL                                           
348100     .                                                                    
348200     SKIP3                                                                
348300 IMS-GHNP-SATB-RAD SECTION.                                               
348400                                                                          
348500     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
348600          DELIMITED BY SIZE INTO SSA1                                     
348700     MOVE '  GE' TO GODK-STATUSKODER                                      
348800     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA SSA1                    
348900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
349000     PERFORM IMS-STATUSKONTROLL                                           
349100     .                                                                    
349200     EJECT                                                                
349300 IMS-GNP-SATB-NOT SECTION.                                                
349400                                                                          
349500     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
349600          DELIMITED BY SIZE INTO SSA1                                     
349700     MOVE 'WLSATB22 ' TO SSA2                                             
349800     MOVE '  GE' TO GODK-STATUSKODER                                      
349900     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
350000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
350100     PERFORM IMS-STATUSKONTROLL                                           
350200     .                                                                    
350300     SKIP3                                                                
350400 IMS-GHNP-FIRST-SATB-NOT-K-PCB SECTION.                                   
350500                                                                          
350600     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
350700          DELIMITED BY SIZE INTO SSA1                                     
350800     MOVE 'WLSATB22*F' TO SSA2                                            
350900     MOVE '  GE' TO GODK-STATUSKODER                                      
351000     CALL CBLTDLI USING GHNP SATB-K-PCB DLI-IO-AREA-K SSA1 SSA2           
351100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
351200     PERFORM IMS-STATUSKONTROLL                                           
351300     .                                                                    
351400     SKIP3                                                                
351500 IMS-GNP-SATB-NOT-K-PCB SECTION.                                          
351600                                                                          
351700     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
351800          DELIMITED BY SIZE INTO SSA1                                     
351900     MOVE 'WLSATB22 ' TO SSA2                                             
352000     MOVE '  GE' TO GODK-STATUSKODER                                      
352100     CALL CBLTDLI USING GNP SATB-K-PCB DLI-IO-AREA-K SSA1 SSA2            
352200     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
352300     PERFORM IMS-STATUSKONTROLL                                           
352400     .                                                                    
352500     EJECT                                                                
352600 IMS-ISRT-SATB-STR SECTION.                                               
352700                                                                          
352800     MOVE 'WLSATB01 ' TO SSA1                                             
352900     MOVE '    ' TO GODK-STATUSKODER                                      
353000     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1                    
353100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
353200     PERFORM IMS-STATUSKONTROLL                                           
353300     .                                                                    
353400     SKIP3                                                                
353500 IMS-ISRT-SATB-RAD SECTION.                                               
353600                                                                          
353700     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
353800          DELIMITED BY SIZE INTO SSA1                                     
353900     MOVE 'WLSATB11 ' TO SSA2                                             
354000     MOVE '    ' TO GODK-STATUSKODER                                      
354100     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1 SSA2               
354200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
354300     PERFORM IMS-STATUSKONTROLL                                           
354400     .                                                                    
354500     EJECT                                                                
354600 IMS-ISRT-SATB-NOT SECTION.                                               
354700                                                                          
354800     STRING 'WLSATB01(IDARTNR  =' W-STRNR-X ')'                           
354900          DELIMITED BY SIZE INTO SSA1                                     
355000     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
355100          DELIMITED BY SIZE INTO SSA2                                     
355200     MOVE 'WLSATB22 ' TO SSA3                                             
355300     MOVE '    ' TO GODK-STATUSKODER                                      
355400     CALL CBLTDLI USING ISRT SATB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
355500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
355600     PERFORM IMS-STATUSKONTROLL                                           
355700     .                                                                    
355800     EJECT                                                                
355900 IMS-REPL-SATB SECTION.                                                   
356000                                                                          
356100     MOVE '  ' TO GODK-STATUSKODER                                        
356200     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
356300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
356400     PERFORM IMS-STATUSKONTROLL                                           
356500     .                                                                    
356600                                                                          
356700 IMS-REPL-SATB-K-PCB SECTION.                                             
356800                                                                          
356900     MOVE '  ' TO GODK-STATUSKODER                                        
357000     CALL CBLTDLI USING REPL SATB-K-PCB DLI-IO-AREA-K                     
357100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
357200     PERFORM IMS-STATUSKONTROLL                                           
357300     .                                                                    
357400                                                                          
357500 IMS-ISRT-SATB-STR-K-PCB SECTION.                                         
357600                                                                          
357700     MOVE 'WLSATB01 ' TO SSA1                                             
357800     MOVE '    ' TO GODK-STATUSKODER                                      
357900     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-K SSA1                
358000     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
358100     PERFORM IMS-STATUSKONTROLL                                           
358200     .                                                                    
358300     SKIP3                                                                
358400 IMS-ISRT-SATB-RAD-K-PCB SECTION.                                         
358500                                                                          
358600     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
358700          DELIMITED BY SIZE INTO SSA1                                     
358800     MOVE 'WLSATB11 ' TO SSA2                                             
358900     MOVE '    ' TO GODK-STATUSKODER                                      
359000     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-K SSA1 SSA2           
359100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
359200     PERFORM IMS-STATUSKONTROLL                                           
359300     .                                                                    
359400     EJECT                                                                
359500 IMS-ISRT-SATB-NOT-K-PCB SECTION.                                         
359600                                                                          
359700     STRING 'WLSATB01(IDARTNR  =' W-STRNR-KONV-X ')'                      
359800          DELIMITED BY SIZE INTO SSA1                                     
359900     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
360000          DELIMITED BY SIZE INTO SSA2                                     
360100     MOVE 'WLSATB22 ' TO SSA3                                             
360200     MOVE '    ' TO GODK-STATUSKODER                                      
360300     CALL CBLTDLI USING ISRT SATB-K-PCB DLI-IO-AREA-K SSA1                
360400                                                 SSA2 SSA3                
360500     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
360600     PERFORM IMS-STATUSKONTROLL                                           
360700     .                                                                    
360800     EJECT                                                                
360900                                                                          
361000 IMS-REPL-ARTC SECTION.                                                   
361100                                                                          
361200     MOVE '  ' TO GODK-STATUSKODER                                        
361300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-ARTC                    
361400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
361500     PERFORM IMS-STATUSKONTROLL                                           
361600     .                                                                    
361700                                                                          
361800                                                                          
361900 IMS-DLET-SATB SECTION.                                                   
362000                                                                          
362100     MOVE '  ' TO GODK-STATUSKODER                                        
362200     CALL CBLTDLI USING DLET SATB-PCB DLI-IO-AREA                         
362300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
362400     PERFORM IMS-STATUSKONTROLL                                           
362500     .                                                                    
362600                                                                          
362700 IMS-DLET-SATB-K-PCB SECTION.                                             
362800                                                                          
362900     MOVE '  ' TO GODK-STATUSKODER                                        
363000     CALL CBLTDLI USING DLET SATB-K-PCB DLI-IO-AREA-K                     
363100     MOVE SATB-K-STATUS-CODE TO STATUS-WS                                 
363200     PERFORM IMS-STATUSKONTROLL                                           
363300     .                                                                    
363400     EJECT                                                                
363500 IMS-ISRT-2234-TRANS SECTION.                                             
363600                                                                          
363700     STRING 'WLXXBY01(WDG3KEY  =' W-WDG3KEY-X ')'                         
363800          DELIMITED BY SIZE INTO SSA1                                     
363900     MOVE 'WLXXBY11*L ' TO SSA2                                           
364000     MOVE '  ' TO GODK-STATUSKODER                                        
364100     CALL CBLTDLI USING ISRT XXBY-PCB DLI-IO-AREA-XX SSA1 SSA2            
364200     MOVE XXBY-STATUS-CODE TO STATUS-WS                                   
364300     PERFORM IMS-STATUSKONTROLL                                           
364400     .                                                                    
364500                                                                          
364600 IMS-GET-XXAZ01 SECTION.                                                  
364700                                                                          
364800     STRING 'WLXXAZ01(WDGXKEY  =' W-WDGXKEY-X ')'                         
364900          DELIMITED BY SIZE INTO SSA1                                     
365000     MOVE '    ' TO GODK-STATUSKODER                                      
365100     CALL CBLTDLI USING GU XXAZ-PCB DLI-IO-AREA-XX SSA1                   
365200     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
365300     PERFORM IMS-STATUSKONTROLL                                           
365400     .                                                                    
365500                                                                          
365600 IMS-GHNP-XXAZ11 SECTION.                                                 
365700                                                                          
365800     STRING 'WLXXAZ11(IDUSER   =' W-IDUSER-X ')'                          
365900          DELIMITED BY SIZE INTO SSA1                                     
366000     MOVE '  GE' TO GODK-STATUSKODER                                      
366100     CALL CBLTDLI USING GHNP XXAZ-PCB DLI-IO-AREA-XX SSA1                 
366200     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
366300     PERFORM IMS-STATUSKONTROLL                                           
366400     .                                                                    
366500                                                                          
366600 IMS-DLET-XXAZ SECTION.                                                   
366700                                                                          
366800     MOVE '  ' TO GODK-STATUSKODER                                        
366900     CALL CBLTDLI USING DLET XXAZ-PCB DLI-IO-AREA-XX                      
367000     MOVE XXAZ-STATUS-CODE TO STATUS-WS                                   
367100     PERFORM IMS-STATUSKONTROLL                                           
367200     .                                                                    
367300                                                                          
367400 IMS-STATUSKONTROLL SECTION.                                              
367500                                                                          
367600     SET STATUS-IX TO 1                                                   
367700     SEARCH GODK-STATUS                                                   
367800       AT END CALL FELLOG                                                 
367900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
368000     END-SEARCH                                                           
368100     .                                                                    
368200     EJECT                                                                
368300*    -COPY WY2000P1                                                       
