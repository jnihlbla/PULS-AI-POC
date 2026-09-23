000100 ID DIVISION.                                                             
000200*                                                                         
000300 PROGRAM-ID.             W2216200.                                        
000400 AUTHOR.                 ANN JORDEBO.                                     
000500 DATE-WRITTEN.           MARS 1990.                                       
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*                                                                         
001200*            UPPGRADERAT TILL VERSION 3.0 OKTOBER 1991.                   
001300*                                                                         
001400*            INFILEN (W22160), SOM INNEHÅLLER ARTIKLAR SOM                
001500*            SKA SÄNDAS TILL LEVERANTÖR, SORTERAS M.H.A                   
001600*            STD.SORT PÅ LEVNR, GK ,GATE (ADINPORT) OCH ARTNR.            
001700*                                                                         
001800*            GK KAN VARA 1, 2 OCH 9 (=LEVERANS TILL MÅLNING).             
001900*            ALLA ARTIKLARS AVROP LÄSES PÅ WDD9.                          
002000*            INFORMATION HÄMTAS ÄVEN FRÅN WDF1 OCH WDL2.                  
002100*                                                                         
002200*            UTFILEN BESTÅR AV ETT ANTAL OLIKA DELINS-SEGMENT.            
002300*            BRYTNING FÖR NY FIL SKER FÖR LEVERANTÖR OCH FÖRETAGS-        
002400*            KOD, DVS PV-FILER FÖR SIG OCH LV-FILER FÖR SIG.              
002500*                                                                         
002600*            OBS: OM ARTIKELN ÄR GEM-MÄRKT, DVS GEMENSAM ARTIKEL          
002700*            MED LV SOM "ÄGDE" DEN FÖRE SPLITTEN 930601, TAS              
002800*            INTE INLEVERANSER GJORDA FÖRE 930601 MED - DE ÄR             
002900*            OINTRESSANTA EFTERSOM DE GJORDES TILL LV                     
003000*                                                                         
003100*                                                                         
003200*    SUBPROGRAM:                                                          
003300*            WZ20DAYS                                                     
003400*            CBLTDLI                                                      
003500*                                                                         
003600*---------------------------------------------------------------          
003700*    PROGRAMÄNDRINGAR:                                                    
003800*                                                                         
003900*            UPPDATERING SEPTEMBER 2004:                                  
004000*            BELEVART35 TILLAGT I ARD-POST (E- SECTION)                   
004100*            NY BAS TILLAGD (WDF5) ENDAST LÄSNING.                        
004200*            // JOHAN NIHLBLAD                                            
004300*                                                                         
004400*            ÄNDRING NOVEMBER 2014:                                       
004500*            FLYTTAT BELEVART FRÅN POS 62 TILL POS 48.                    
004600*            BELEVART14 TILLAGT I POS 48 ARD-POST (E- SECTION)            
004700*            BELEVART35 BYTT TILL SPACE I POS 62 ARD-POST.                
004800*                                                                         
004900*            2015-06-05 E-TRACKER 10209749                                
005000*                       TAKE AWAY EXTRA DELIVERIES                        
005100*                                                                         
005110*            2016-10-17 E-TRACKER 10261999 (10282597 IHOPSLAGEN)          
005120*                       EDI: REMOVE STATUS 3,ONLY HAVE 1 & 4              
005121*                       EDI: CORRECTION DELFOR-PDN-SEGMENT                
005130*                                                                         
005140*                                                                         
005200     EJECT                                                                
005300 ENVIRONMENT DIVISION.                                                    
005400     SKIP2                                                                
005500 INPUT-OUTPUT SECTION.                                                    
005600                                                                          
005700 FILE-CONTROL.                                                            
005800     SKIP2                                                                
005900*- - - - - - - - - - - - - - INFILER:                                     
006000                                                                          
006100     SELECT  W22160                   ASSIGN TO W22162D1.                 
006200     SKIP2                                                                
006300*- - - - - - - - - - - - - - UTFILER:                                     
006400                                                                          
006500     SELECT  W22162                   ASSIGN TO W22162D2.                 
006600     EJECT                                                                
006700 DATA DIVISION.                                                           
006800     SKIP2                                                                
006900 FILE SECTION.                                                            
007000     SKIP3                                                                
007100 FD  W22160                                                               
007200     LABEL RECORD STANDARD                                                
007300     RECORDING      F                                                     
007400     BLOCK CONTAINS 0.                                                    
007500     SKIP2                                                                
007600*01  -COPY W22160                           -L.                           
007700     SKIP3                                                                
007800 FD  W22162                                                               
007900     LABEL RECORD STANDARD                                                
008000     RECORDING   V                                                        
008100     BLOCK CONTAINS 0.                                                    
008200     SKIP2                                                                
008300 01  W22162-POST                 PIC X(1005).                             
008400*01  HEAD  -COPY W221001                      -L.                         
008500*01  UNB   -COPY W221UNB                      -L.                         
008600*01  UNH   -COPY W221UNH                      -L.                         
008700*01  MID   -COPY W221MID                      -L.                         
008800*01  SDT   -COPY W221SDT                      -L.                         
008900*01  BDT   -COPY W221BDT                      -L.                         
009000*01  ARI   -COPY W221ARI                      -L.                         
009100*01  CSG   -COPY W221CSG                      -L.                         
009200*01  ARD   -COPY W221ARD                      -L.                         
009300*01  PDI   -COPY W221PDI                      -L.                         
009400*01  SAD   -COPY W221SAD                      -L.                         
009500*01  DST   -COPY W221DST                      -L.                         
009600*01  PDN   -COPY W221PDN                      -L.                         
009700*01  DEL   -COPY W221DEL                      -L.                         
009800*01  TRAIL -COPY W221003                      -L.                         
009900     EJECT                                                                
010000 WORKING-STORAGE SECTION.                                                 
010100     SKIP2                                                                
010200*    -COPY WY2000W2                                                       
010300     SKIP3                                                                
010400*    -COPY WY2000W1                                                       
010500     SKIP3                                                                
010600*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
010700 77   PROGRAM-NAMN           VALUE 'W2216200'                             
010800                                 PIC X(8).                                
010900     SKIP2                                                                
011000*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
011100                                                                          
011200 77  JA                          PIC X       VALUE 'J'.                   
011300 77  NEJ                         PIC X       VALUE 'N'.                   
011310 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
011320 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
011400                                                                          
011500     SKIP2                                                                
011600*- - - - - - - - - - - - - -  END-OF-FILE SWITCHAR                        
011700 77  INFIL-EOF                   PIC X       VALUE 'N'.                   
011800     SKIP2                                                                
011900*- - - - - - - - - - - - - -  INDEX                                       
012000                                                                          
012100 77  IX                          PIC S9(3)   VALUE +0 COMP SYNC.          
012200 77  IX-DAG                      PIC S9(3)   VALUE +0 COMP SYNC.          
012300     SKIP2                                                                
012400*- - - - - - - - - - - - - -  LEV SOM VILL HA SENASTE INLEV.              
012500*                             I OMVÄND ORDNING, DVS ÄLDST FÖRST           
012600 77  LEV-KOLL                   PIC X(5)  VALUE SPACE.                    
012700     88  SPECIAL-LEV                      VALUE '6040 '.                  
012800                                                                          
012900*- - - - - - - - - - - - - -  TABELLER                                    
013000 01  PDN-TABELL.                                                          
013100   03  TABELL                 OCCURS 3.                                   
013200     05  TAB-IDNRINL             PIC X(8).                                
013300     05  TAB-TISENINL            PIC 9(7).                                
013400     05  TAB-KVSENINL            PIC 9(7).                                
013500     EJECT                                                                
013600*- - - - - - - - - - - - - -  MOTTAGNINGSADRESSER                         
013700 01  FILLER                      PIC X(8)    VALUE 'ADRESSER'.            
013800 01  ADRESSER.                                                            
013900   03  WS-BEFTAG-SVE             PIC X(35)   VALUE                        
014010       'VOLVO CAR CORP. PS&L             '.                               
014100   03  WS-BEFTAG-NL              PIC X(35)   VALUE                        
014200       'VOLVO CAR PARTS      MAASTRICHT NL '.                             
014300*- - - - - - - - - - - - - -  ARBETS-FÄLT                                 
014400 01  FILLER                      PIC X(16)   VALUE ' ARBETSFÄLT'.         
014500 01  ARBETS-FAELT.                                                        
014600   03  SPAR-KDGK                 PIC S9      VALUE ZERO COMP-3.           
014700   03  SPAR-IDLEVNR              PIC X(5)    VALUE SPACE.                 
014800   03  SPAR-IDFTG                PIC 9(2)    VALUE ZERO.                  
014900   03  SPAR-IDLEVNR-AKTUELL      PIC X(5)    VALUE SPACE.                 
015000   03  SPAR-TILEVDAG             PIC S9      VALUE ZERO.                  
015100   03  SPAR-DAINLEV              PIC 9(16)   VALUE ZERO.                  
015200   03  SPAR-ADINPORT             PIC X(08)   VALUE SPACE.                 
015300   03  SAVE-IDFTG                PIC 9(2)    VALUE ZERO.                  
015400   03  SAVE-IDLEVNR              PIC X(5)    VALUE SPACE.                 
015500   03  SAVE-IDARTNR              PIC S9(9)   VALUE +0  COMP-3.            
015600   03  TRAFF                     PIC X       VALUE 'N'.                   
015700   03  SKAPA-TRAILER             PIC X       VALUE 'N'.                   
015800   03  DEL-SEGMENT               PIC X       VALUE 'N'.                   
015900   03  W-COUNT-W221DEL           PIC S9(5)   VALUE ZERO COMP-3.           
016000   03  TOT-ANTAL                 PIC S9(9)   VALUE ZERO COMP-3.           
016100   03  RAKNARE                   PIC S9(1)   VALUE ZERO COMP-3.           
016400   03  SW-EFT-SLAP               PIC X       VALUE 'N'.                   
016600   03  WS-IDBEST                 PIC S9(12)  VALUE ZERO COMP-3.           
016700   03  WS-OLD-IDARTNR            PIC 9(9)    VALUE ZERO COMP-3.           
016800   03  WS-IDLPLAN                PIC 9(7)    VALUE ZERO.                  
016900   03  WS-IDARTNR                PIC X(9)    VALUE ZERO.                  
017000   03  WS-IDLEVNR                PIC X(5)    VALUE SPACE.                 
017100   03  WS-IDLEVNR-NUM            PIC 9(5)    VALUE ZERO.                  
017200   03  WS-IDOVERFNR              PIC X(5)    VALUE SPACE.                 
017300   03  WS-MESSAGE-REF            PIC S9(3)   VALUE ZERO.                  
017400   03  WS-TIAAVVD                PIC S9(5)   VALUE ZERO.                  
017500   03  WS-TIAVROP-AVS-AAVVD      PIC 9(5)    VALUE ZERO.                  
017600   03  W-DAAVROP-AVS             PIC 9(6).                                
017700   03  FILLER REDEFINES W-DAAVROP-AVS.                                    
017800       05  FILLER                PIC 9(2).                                
017900       05  W-TIAVROP-AVS         PIC 9(4).                                
018000   03  DAINLEV-NYCKEL            PIC 9(16)   VALUE ZERO.                  
018100   03  DAINLEV-MAX             PIC 9(16) VALUE 9999999999999999.          
018200   03  ANT-PALL                  PIC S9(7)   VALUE ZERO COMP-3.           
018300   03  PALL-PER-DAG              PIC S9(7)   VALUE ZERO COMP-3.           
018400   03  REST                      PIC S9(7)   VALUE ZERO COMP-3.           
018500   03  ANTAL-PER-DAG             PIC S9(7)   VALUE ZERO COMP-3.           
018600   03  ANT-VECKODAG OCCURS 5     PIC S9(7)              COMP-3.           
018700   03  WS-KDGODSM-DET.                                                    
018800       05  WS-SUFFIX             PIC X(3)    VALUE SPACE.                 
018900       05  FILLER                PIC X(1)    VALUE SPACE.                 
019000       05  WS-ADINPORT           PIC X(8)    VALUE SPACE.                 
019100       05  FILLER                PIC X(5)    VALUE SPACE.                 
019200   03  W-ANTAL-VECKOR            PIC S9(3)               COMP-3.          
019300   03  WS-TIAAVV-FRYS            PIC S9(5)               COMP-3.          
019400                                                                          
019500*01  -COPY WWDCKONS                                                       
019600                                                                          
019700     EJECT                                                                
019800*- - - - - - - - - - - - - -  MEDDELANDEN                                 
019900 01  FILLER                PIC X(11)  VALUE 'MEDDELANDEN'.                
020000 01  MEDDELANDEN.                                                         
020100   03  DEF-BESTNR                PIC X(12)  VALUE '000000000100'.         
020200     SKIP3                                                                
020300*- - - - - - - - - - - - - -  DATUM- OCH TID-AREA                         
020400 01  MASKINENS-DATUM             PIC 9(6).                                
020410 01  MASKINENS-AAVVD             PIC 9(5).                                
020420 01  MASKINENS-DAGNR             PIC 9(1).                                
020500 01  DAGENS-DATUM                PIC 9(6).                                
020600 01  FILLER REDEFINES DAGENS-DATUM.                                       
020700   03  DAT-AA                    PIC 9(2).                                
020800   03  DAT-MM                    PIC 9(2).                                
020900   03  DAT-DD                    PIC 9(2).                                
021000                                                                          
021100 01  INNEVARANDE-AAVVD           PIC 9(5).                                
021200 01  FILLER REDEFINES INNEVARANDE-AAVVD.                                  
021300   03  INNEV-AA                  PIC 9(2).                                
021400   03  INNEV-VV                  PIC 9(2).                                
021500   03  INNEV-D                   PIC 9(1).                                
021600                                                                          
021700 01  DATUM-AA0101.                                                        
021800   03  DA-AA                     PIC 9(2)    VALUE ZERO.                  
021900   03  DA-0101                   PIC 9(4)    VALUE ZERO.                  
022000                                                                          
022100 01  INNEVARANDE-DAT             PIC 9(16).                               
022200 01  INNEVARANDE-DAT-R  REDEFINES  INNEVARANDE-DAT.                       
022300   03  INNEVARANDE-SEKEL         PIC 9(2).                                
022400   03  INNEVARANDE-AAR           PIC 9(2).                                
022500   03  INNEVARANDE-NOLLOR        PIC 9(12).                               
022600 01  MASKINENS-TID               PIC 9(8).                                
022700                                                                          
022800 01  KONTR-DATUM                 PIC 9(5).                                
022900 01  FILLER REDEFINES KONTR-DATUM.                                        
023000   03  KONTR-TIAAVV              PIC 9(4).                                
023100   03  KONTR-TID                 PIC 9(1).                                
023200     EJECT                                                                
023300*- - - - - - - - - - - - - -  HJÄLPFÄLT VID FILLÄSNINGARNA                
023400 01  FILLER.                                                              
023500   03  INFIL-ID.                                                          
023600      05  IN-IDLEVNR-ID          PIC X(5)    VALUE SPACE.                 
023700      05  IN-IDARTNR-ID          PIC 9(9)    VALUE ZERO COMP-3.           
023800     SKIP2                                                                
023900 01  MAX-VARDE                   PIC S9(14)  VALUE                        
024000                                             +99999999999999.             
024100     SKIP3                                                                
024200 01  INFIL-TRANSID.                                                       
024300     03  INFIL-FDNAMN            PIC X(6)    VALUE 'W22160'.              
024400     03  INFIL-DDNAMN            PIC X(8)    VALUE 'W22162D1'.            
024500     03  INFIL-TRANSTYP          PIC X(4)    VALUE SPACE.                 
024600     SKIP3                                                                
024700 01  W22162-TRANSID.                                                      
024800     03  62-FDNAMN               PIC X(6)    VALUE 'W22162'.              
024900     03  62-DDNAMN               PIC X(8)    VALUE 'W22162D2'.            
025000     03  62-TRANSTYP             PIC X(4)    VALUE SPACE.                 
025100     EJECT                                                                
025200*    -COPY W200EMAB                                                       
025300     EJECT                                                                
025400*    -COPY W200OMLG                                                       
025500     EJECT                                                                
025600 01  DYNAMISKA-SUBPROGRAM.                                                
025700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
025800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
025900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
026000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
026100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG '.             
026200     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
026300     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
026400     SKIP3                                                                
026500*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
026600                                                                          
026700 01  RETURKODER.                                                          
026800     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.        
026900     03  RKOD                    PIC S9(4)   VALUE +0   COMP SYNC.        
027000     SKIP2                                                                
027010 01  FELTEXT.                                                             
027020     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
027030     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
027040     EJECT                                                                
027100*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
027200                                                                          
027300*01  -COPY W0005       -PRE POSTSUM-.                                     
027400     EJECT                                                                
027500*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
027600                                                                          
027700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
027800     SKIP2                                                                
027900*    -COPY WDATKORT                                                       
028000     EJECT                                                                
028100*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS "ADDERA DAGAR DATUM"         
028200 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
028300*01 -COPY WZ20DAYS                                                        
028400     SKIP2                                                                
028500*01  -COPY WDATAREA.                                                      
028600     EJECT                                                                
028700*- - - - - - - - - - - - - -  INPOSTERNA                                  
028800                                                                          
028900 01  FILLER                      PIC X(16)   VALUE 'INFIL'.               
029000     SKIP2                                                                
029100*    -COPY W22160        -PRE IN-.                                        
029200     EJECT                                                                
029300                                                                          
029400*- - - - - - - - - - - - - -  UTPOSTERNA                                  
029500                                                                          
029600 01  FILLER                      PIC X(16)   VALUE 'W221001'.             
029700     SKIP2                                                                
029800*    -COPY W221001       -PRE HEAD-                                       
029900     EJECT                                                                
030000 01  FILLER                      PIC X(16)   VALUE 'W221UNB'.             
030100     SKIP2                                                                
030200*    -COPY W221UNB       -PRE UNB-                                        
030300     EJECT                                                                
030400 01  FILLER                      PIC X(16)   VALUE 'W221UNH'.             
030500     SKIP2                                                                
030600*    -COPY W221UNH       -PRE UNH-                                        
030700     EJECT                                                                
030800 01  FILLER                      PIC X(16)   VALUE 'W221MID'.             
030900     SKIP2                                                                
031000*    -COPY W221MID       -PRE MID-                                        
031100     EJECT                                                                
031200 01  FILLER                      PIC X(16)   VALUE 'W221SDT'.             
031300     SKIP2                                                                
031400*    -COPY W221SDT       -PRE SDT-                                        
031500     EJECT                                                                
031600 01  FILLER                      PIC X(16)   VALUE 'W221BDT'.             
031700     SKIP2                                                                
031800*    -COPY W221BDT       -PRE BDT-                                        
031900     EJECT                                                                
032000 01  FILLER                      PIC X(16)   VALUE 'W221ARI'.             
032100     SKIP2                                                                
032200*    -COPY W221ARI       -PRE ARI-                                        
032300     EJECT                                                                
032400 01  FILLER                      PIC X(16)   VALUE 'W221CSG'.             
032500     SKIP2                                                                
032600*    -COPY W221CSG       -PRE CSG-                                        
032700     EJECT                                                                
032800 01  FILLER                      PIC X(16)   VALUE 'W221ARD'.             
032900     SKIP2                                                                
033000*    -COPY W221ARD       -PRE ARD-                                        
033100     EJECT                                                                
033200 01  FILLER                      PIC X(16)   VALUE 'W221PDI'.             
033300     SKIP2                                                                
033400*    -COPY W221PDI       -PRE PDI-                                        
033500     EJECT                                                                
033600 01  FILLER                      PIC X(16)   VALUE 'W221SAD'.             
033700     SKIP2                                                                
033800*    -COPY W221SAD       -PRE SAD-                                        
033900     EJECT                                                                
034000 01  FILLER                      PIC X(16)   VALUE 'W221DST'.             
034100     SKIP2                                                                
034200*    -COPY W221DST       -PRE DST-                                        
034300     EJECT                                                                
034400 01  FILLER                      PIC X(16)   VALUE 'W221PDN'.             
034500     SKIP2                                                                
034600*    -COPY W221PDN       -PRE PDN-                                        
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)   VALUE 'W221DEL'.             
034900     SKIP2                                                                
035000*    -COPY W221DEL       -PRE DEL-                                        
035100     EJECT                                                                
035200 01  FILLER                      PIC X(16)   VALUE 'W221003'.             
035300     SKIP2                                                                
035400*    -COPY W221003       -PRE TRAIL-                                      
035500     EJECT                                                                
035600*- - - - - - - - - - - - - -  NYCKLAR TILL DLI                            
035700                                                                          
035800 01  NYCKLAR-TILL-DLI.                                                    
035900   03  W-IDARTNR-X.                                                       
036000     05  W-IDARTNR               PIC S9(9)  COMP-3.                       
036100   03  W-WDD901KY-X.                                                      
036200     05  W-IDARTNR-D9            PIC S9(9)  COMP-3.                       
036300     05  W-IDDC-D9               PIC X(2).                                
036400   03  W-IDLEVNR-X.                                                       
036500     05  W-IDLEVNR               PIC X(5).                                
036600   03  W-DAINLEV-X.                                                       
036700     05  W-DAINLEV               PIC 9(16).                               
036800   03  W-KDSEGKEY-X.                                                      
036900     05  W-KDSEGKEY              PIC X      VALUE '1'.                    
037000                                                                          
037100   03  W-WDF502-MIN.                                                      
037200     05  W-IDLEVNR-MIN  PIC X(5).                                         
037300     05  FILLER         PIC S9(1)   VALUE ZERO COMP-3.                    
037400   03  W-WDF502-MAX.                                                      
037500     05  W-IDLEVNR-MAX  PIC X(5).                                         
037600     05  FILLER         PIC S9(1)   VALUE +9 COMP-3.                      
037700                                                                          
037800*- - - - - - - - - - - - - -  ARBETSAREOR TILL IMS-SEKTIONERNA            
037900*                                                                         
038000 01  IMS-WS.                                                              
038100   03  FILLER           PIC X(8) VALUE 'IMS-WS  '.                        
038200*- - - - - - - - - - - - - -  STATUSKOD FRÅN IMS                          
038300   03  STATUS-WS        PIC X(2).                                         
038400      88  SEGMENT-FINNS          VALUE '  '.                              
038500      88  SEGMENT-SAKNAS         VALUE 'GE'.                              
038600                                                                          
038700   03  GODK-STATUSKODER.                                                  
038800      05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.               
038900                                                                          
039000   03  SSA1             PIC X(64).                                        
039100   03  SSA2             PIC X(64).                                        
039200   03  SSA3             PIC X(64).                                        
039300     EJECT                                                                
039400*01   -COPY W0003.                                                        
039500     EJECT                                                                
039600 01  DLI-IO-AREA.                                                         
039700   03  IO-AREA          PIC X(200) VALUE SPACE.                           
039800                                                                          
039900*  03  WLLEVA01  -COPY WDF101 -PRE LEVA- -RED IO-AREA.                    
040000     EJECT                                                                
040100*  03  WLINLE01  -COPY WDL201 -PRE INLE- -RED IO-AREA.                    
040200     EJECT                                                                
040300*  03  WLINLE11  -COPY WDL211 -PRE INLE- -RED IO-AREA.                    
040400     EJECT                                                                
040500*  03  WLINLE21  -COPY WDL221 -PRE INLE- -RED IO-AREA.                    
040600     EJECT                                                                
040700*  03  WLINLB11  -COPY WDD902 -PRE INLB- -RED IO-AREA.                    
041000     EJECT                                                                
041100*  03  WLINLB23  -COPY WDD905 -PRE INLB- -RED IO-AREA.                    
041200     EJECT                                                                
041800 01  DLI-IO-AREA3.                                                        
041900   03  IO-AREA3         PIC X(900) VALUE SPACE.                           
042000                                                                          
042100*  03  WLARTC11  -COPY WDK611  -RED IO-AREA3.                             
042200     EJECT                                                                
042300 01  DLI-IO-AREA-WDF5.                                                    
042400   03  IO-WDF5          PIC X(200) VALUE SPACE.                           
042500                                                                          
042600*  03  WDF501    -COPY WDF501  -RED IO-WDF5.                              
042700     EJECT                                                                
042800*  03  WDF502    -COPY WDF502  -RED IO-WDF5.                              
042900     EJECT                                                                
043000                                                                          
043100 LINKAGE SECTION.                                                         
043200     SKIP3                                                                
043300*01   -COPY W0008  -PRE WLLEVA-                                           
043400      05  FILLER        PIC X(5).                                         
043500     EJECT                                                                
043600*01   -COPY W0008  -PRE WLINLE-                                           
043700      05  FILLER        PIC X(5).                                         
043800     EJECT                                                                
043900*01   -COPY W0008  -PRE WLINLB-                                           
044000      05  FILLER        PIC X(5).                                         
044100     EJECT                                                                
044200*01   -COPY W0008  -PRE WLARTC-                                           
044300      05  FILLER        PIC X(5).                                         
044310     EJECT                                                                
044400*01   -COPY W0008  -PRE WDF5-                                             
044500      05  FILLER        PIC X(5).                                         
044600                                                                          
044700     EJECT                                                                
044800 PROCEDURE DIVISION USING  WLLEVA-PCB WLINLE-PCB WLINLB-PCB               
044900                           WLARTC-PCB WDF5-PCB.                           
045000     ENTRY 'DLITCBL' USING WLLEVA-PCB WLINLE-PCB WLINLB-PCB               
045100                           WLARTC-PCB WDF5-PCB.                           
045200     SKIP2                                                                
045300     PERFORM A-INIT                                                       
045400     PERFORM S01-LAS-INFIL                                                
045500                                                                          
045600     IF INFIL-EOF = NEJ                                                   
045700       PERFORM S03-SKAPA-HEADER                                           
045800       MOVE JA TO SKAPA-TRAILER                                           
045900     END-IF                                                               
046000                                                                          
046100     PERFORM UNTIL INFIL-EOF = JA                                         
046200                                                                          
046300       MOVE IN-IDLEVNR TO SPAR-IDLEVNR                                    
046400       MOVE IN-IDFTG   TO SPAR-IDFTG                                      
046500       PERFORM B-SKAPA-UNB                                                
046600       PERFORM C-SKAPA-UNH-MID-SDT-BDT-ARI                                
046700       PERFORM UNTIL INFIL-EOF      = JA           OR                     
046800                     IN-IDLEVNR NOT = SPAR-IDLEVNR OR                     
046900                     IN-IDFTG   NOT = SPAR-IDFTG                          
047000         MOVE IN-KDGK-SORT TO SPAR-KDGK                                   
047110         MOVE IN-ADINPORT  TO SPAR-ADINPORT                               
047200         PERFORM D-SKAPA-CSG                                              
047300         PERFORM UNTIL INFIL-EOF        = JA            OR                
047400                       IN-IDLEVNR   NOT = SPAR-IDLEVNR  OR                
047500                       IN-IDFTG     NOT = SPAR-IDFTG    OR                
047600                       IN-ADINPORT  NOT = SPAR-ADINPORT OR                
047700                       IN-KDGK-SORT NOT = SPAR-KDGK                       
047800                                                                          
047900           PERFORM S02-HAMTA-INFO-FRAN-WDF1                               
048000           PERFORM E-SKAPA-ARD-PDI-SAD-DST                                
048100           PERFORM F-SKAPA-OCH-SKRIV-PDN                                  
048200           PERFORM G-SKAPA-OCH-SKRIV-DEL                                  
048300                                                                          
048400           MOVE IN-IDFTG   TO SAVE-IDFTG                                  
048500           MOVE IN-IDLEVNR TO SAVE-IDLEVNR                                
048600           MOVE IN-IDARTNR TO SAVE-IDARTNR                                
048700                                                                          
048800           PERFORM S01-LAS-INFIL                                          
048900         END-PERFORM                                                      
049000       END-PERFORM                                                        
049100     END-PERFORM                                                          
049200                                                                          
049300     IF SKAPA-TRAILER = JA                                                
049400       PERFORM S04-SKAPA-TRAILER                                          
049500     END-IF                                                               
049600                                                                          
049700     PERFORM Z-FINIT                                                      
049800     MOVE ZERO TO RETURN-CODE                                             
049900     GOBACK                                                               
050000     EJECT                                                                
050100     .                                                                    
050200 A-INIT SECTION.                                                          
050210     MOVE 'A-INIT '  TO CURRENT-SECTION                                   
050220                                                                          
050300     SKIP2                                                                
050400     OPEN INPUT  W22160                                                   
050500                                                                          
050600     OPEN OUTPUT W22162                                                   
050700                                                                          
050800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
050900                                                                          
051000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
051100     MOVE D-AAR        TO DAT-AA INNEVARANDE-AAR INNEV-AA                 
051200     MOVE D-VECKA      TO INNEV-VV                                        
051300     MOVE D-MAANAD     TO DAT-MM                                          
051400     MOVE D-DAG        TO DAT-DD                                          
051500     MOVE D-DAGNR      TO INNEV-D                                         
051600                                                                          
052000     MOVE ZERO TO INNEVARANDE-NOLLOR                                      
052100     IF INNEVARANDE-AAR < 50                                              
052200       MOVE 20         TO INNEVARANDE-SEKEL                               
052300     ELSE                                                                 
052400       MOVE 19         TO INNEVARANDE-SEKEL                               
052500     END-IF                                                               
052510                                                                          
052511     ACCEPT MASKINENS-DATUM FROM DATE                                     
052512     ACCEPT MASKINENS-TID FROM TIME                                       
052513                                                                          
052514     MOVE MASKINENS-DATUM TO DAYS-TIDATE1                                 
052515     MOVE 'YYMMDD'        TO DAYS-KDDATFMT1                               
052516     MOVE 'YYYYWWD'       TO DAYS-KDDATFMT2                               
052517     MOVE +0              TO DAYS-KVDAYS                                  
052518     MOVE SPACE           TO DAYS-TIDATE2                                 
052519                             DAYS-IDCALEND                                
052521                                                                          
052523     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
052524                                                                          
052525     IF DAYS-KDRC = 8                                                     
052526       MOVE 'FEL VID ANROP TILL WZ20DAYS ' TO FELTEXT-STR                 
052527                                                                          
052528       CALL FELLOG                                                        
052531     ELSE                                                                 
052532       MOVE DAYS-TIDATE2(3:5)   TO MASKINENS-AAVVD                        
052533       MOVE DAYS-TIDATE2(7:1)   TO MASKINENS-DAGNR                        
052534     END-IF                                                               
052535                                                                          
052540                                                                          
052600     COMPUTE DAINLEV-NYCKEL = DAINLEV-MAX - INNEVARANDE-DAT               
052700******************************************************************        
052800*                                                                *        
052900*  DAINLEV-NYCKEL RÄKNAS UT GENOM ATT SUBTRAHERA INNEVARANDE-DAT *        
053000*  FRÅN DAINLEV-MAX. RESULTATET GER DAINLEV-NYCKEL SOM ANVÄNDS   *        
053100*  VID LÄSNING AV WDL2. WDL211 ÄR LAGRAD PÅ FALLANDE NYCKEL      *        
053200*  DAINLEV,OCH EFTER VARJE 211-SEGMENT JÄMFÖRS DAINLEV MED       *        
053300*  DAINLEV-NYCKEL FÖR ATT BESTÄMMA OM INLEVERANSEN SKETT UNDER   *        
053400*  INNEVARANDE ÅR.                                               *        
053500******************************************************************        
053600                                                                          
053700     INITIALIZE PDN-TABELL                                                
053800                                                                          
053900     .                                                                    
054000     EJECT                                                                
054100 B-SKAPA-UNB SECTION.                                                     
054110     MOVE 'B-SKAPA-UNB '  TO CURRENT-SECTION                              
054200     SKIP2                                                                
054210                                                                          
054300*--  NOLLA UNB-SEGMENTET                                                  
054400     PERFORM BA-NOLLA-UNB                                                 
054500                                                                          
054600     MOVE 'UNB' TO UNB-IDPT                                               
054700                   62-TRANSTYP                                            
054800     MOVE '01441' TO UNB-IDFROM                                           
054900                                                                          
055000     MOVE IN-IDLEVNR TO WS-IDLEVNR                                        
055100     IF WS-IDLEVNR (5:1) = SPACE                                          
055200        MOVE ZERO TO TALLY                                                
055300        INSPECT WS-IDLEVNR TALLYING TALLY                                 
055400                FOR CHARACTERS BEFORE INITIAL SPACE                       
055500        IF TALLY = ZERO                                                   
055600           MOVE ZERO TO WS-IDLEVNR-NUM                                    
055700        ELSE                                                              
055800           MOVE WS-IDLEVNR (1:TALLY) TO WS-IDLEVNR-NUM                    
055900        END-IF                                                            
056000        MOVE WS-IDLEVNR-NUM TO UNB-IDLEVNR                                
056100     ELSE                                                                 
056200        MOVE WS-IDLEVNR     TO UNB-IDLEVNR                                
056300     END-IF                                                               
056400                                                                          
056500     MOVE IN-IDOVERFNR-LOP TO WS-IDOVERFNR                                
056600     INSPECT WS-IDOVERFNR TALLYING RAKNARE FOR LEADING '0'                
056700     ADD +1 TO RAKNARE                                                    
056800     UNSTRING WS-IDOVERFNR INTO UNB-IDSNRF WITH POINTER RAKNARE           
056900     MOVE ZERO TO RAKNARE                                                 
057000                                                                          
057100     WRITE UNB FROM UNB-W221UNB                                           
057200     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
057300     CALL POSTSUM USING POSTSUM-PARM                                      
057400                                                                          
057500     MOVE ZERO TO WS-MESSAGE-REF                                          
057600     MOVE SPACE TO 62-TRANSTYP                                            
057700     .                                                                    
057800     EJECT                                                                
057900 BA-NOLLA-UNB SECTION.                                                    
057910     MOVE 'BA-NOLLA-UNB '  TO CURRENT-SECTION                             
058000     SKIP2                                                                
058010                                                                          
058100     MOVE SPACE TO UNB-IDPT                                               
058200                   UNB-IDFROM                                             
058300                   UNB-IDLEVNR                                            
058400                   UNB-IDSNRF                                             
058500     .                                                                    
058600     EJECT                                                                
058700 C-SKAPA-UNH-MID-SDT-BDT-ARI SECTION.                                     
058710     MOVE 'C-SKAPA-UNH-MID-SDT-BDT-ARI ' TO CURRENT-SECTION               
058800     SKIP2                                                                
058810                                                                          
058900     PERFORM CB-SKAPA-OCH-SKRIV-UNH                                       
059000     PERFORM CC-SKAPA-OCH-SKRIV-MID                                       
059100     PERFORM CD-SKAPA-OCH-SKRIV-SDT                                       
059200     PERFORM CE-SKAPA-OCH-SKRIV-BDT                                       
059300     PERFORM CF-SKAPA-OCH-SKRIV-ARI                                       
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700 CB-SKAPA-OCH-SKRIV-UNH SECTION.                                          
059710     MOVE 'CB-SKAPA-OCH-SKRIV-UNH ' TO CURRENT-SECTION                    
059800     SKIP2                                                                
059810                                                                          
059900*--  NOLLA UNH-SEGMENTET                                                  
060000     PERFORM CBA-NOLLA-UNH                                                
060100                                                                          
060200     MOVE 'UNH' TO UNH-IDPT                                               
060300     MOVE 'DELINS' TO UNH-MESSAGE-ID-TYPE                                 
060400     MOVE '  3' TO UNH-MESSAGE-ID-VERSION                                 
060500                                                                          
060600     WRITE UNH FROM UNH-W221UNH                                           
060700     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
060800     CALL POSTSUM USING POSTSUM-PARM                                      
060900                                                                          
061000     .                                                                    
061100     SKIP3                                                                
061200 CBA-NOLLA-UNH SECTION.                                                   
061210     MOVE 'CBA-NOLLA-UNH '  TO CURRENT-SECTION                            
061300     SKIP2                                                                
061310                                                                          
061400     MOVE SPACE TO UNH-IDPT                                               
061500                   UNH-MESSAGE-ID-TYPE                                    
061600                   UNH-MESSAGE-ID-VERSION                                 
061700     .                                                                    
061800     EJECT                                                                
061900 CC-SKAPA-OCH-SKRIV-MID SECTION.                                          
061910     MOVE 'CC-SKAPA-OCH-SKRIV-MID '  TO CURRENT-SECTION                   
062000     SKIP2                                                                
062010                                                                          
062100*--  NOLLA MID-SEGMENTET                                                  
062200     MOVE SPACE TO MID-IDPT                                               
062300                   MID-IDLPLAN                                            
062400     MOVE ZERO  TO MID-TILPLAN                                            
062500                                                                          
062600     MOVE 'MID' TO MID-IDPT                                               
062700     MOVE IN-IDLPLAN-LEV TO WS-IDLPLAN                                    
062800     MOVE WS-IDLPLAN TO MID-IDLPLAN                                       
062900     MOVE DAGENS-DATUM TO MID-TILPLAN                                     
063000                                                                          
063100     WRITE MID FROM MID-W221MID                                           
063200     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
063300     CALL POSTSUM USING POSTSUM-PARM                                      
063400     .                                                                    
063500     EJECT                                                                
063600 CD-SKAPA-OCH-SKRIV-SDT SECTION.                                          
063610     MOVE 'CD-SKAPA-OCH-SKRIV-SDT '  TO CURRENT-SECTION                   
063700     SKIP2                                                                
063710                                                                          
063800*--  NOLLA SDT-SEGMENTET                                                  
063900     MOVE SPACE TO SDT-IDPT                                               
064000                   SDT-IDLEVNR                                            
064100                                                                          
064200     MOVE 'SDT' TO SDT-IDPT                                               
064300     MOVE WS-IDLEVNR TO SDT-IDLEVNR                                       
064400     MOVE ZERO TO RAKNARE                                                 
064500                                                                          
064600     WRITE SDT FROM SDT-W221SDT                                           
064700     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
064800     CALL POSTSUM USING POSTSUM-PARM                                      
064900     .                                                                    
065000     EJECT                                                                
065100 CE-SKAPA-OCH-SKRIV-BDT SECTION.                                          
065110     MOVE 'CE-SKAPA-OCH-SKRIV-BDT'  TO CURRENT-SECTION                    
065200     SKIP2                                                                
065210                                                                          
065300*--  NOLLA BDT-SEGMENTET                                                  
065400     MOVE SPACE TO BDT-IDPT                                               
065500                   BDT-IDBUYER                                            
065600                                                                          
065700     MOVE 'BDT' TO BDT-IDPT                                               
065800                                                                          
065900     MOVE IN-IDLEVNR TO LEV-KOLL                                          
066000                                                                          
066100     MOVE IN-IDLEVKND TO BDT-IDBUYER                                      
066200                                                                          
066300     WRITE BDT FROM BDT-W221BDT                                           
066400     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
066500     CALL POSTSUM USING POSTSUM-PARM                                      
066600                                                                          
066700     .                                                                    
066800     EJECT                                                                
066900 CF-SKAPA-OCH-SKRIV-ARI SECTION.                                          
066910     MOVE 'CF-SKAPA-OCH-SKRIV-ARI' TO CURRENT-SECTION                     
067000     SKIP2                                                                
067010                                                                          
067100*--  NOLLA ARI-SEGMENTET                                                  
067200     MOVE SPACE TO ARI-IDPT                                               
067300     MOVE ZERO  TO ARI-KDRLSTID                                           
067400                   ARI-TIYYMMDD-LPFROM                                    
067500                                                                          
067600     MOVE 'ARI' TO ARI-IDPT                                               
067700     MOVE +1 TO ARI-KDRLSTID                                              
067800                                                                          
067900*--  LEVERANSPLAN BÖRJAR GÄLLA DAGENS-DATUM + 1,                          
068000*--  DVS DAGEN EFTER NATTKÖRNINGEN                                        
068100                                                                          
068200     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
068300     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
068400     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
068500     MOVE SPACE        TO DAYS-TIDATE2                                    
068600                          DAYS-IDCALEND                                   
068700     MOVE +1           TO DAYS-KVDAYS                                     
068800                                                                          
068900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
069000                                                                          
069100     MOVE DAYS-TIDATE2(1:6) TO ARI-TIYYMMDD-LPFROM                        
069200                                                                          
069300     WRITE ARI FROM ARI-W221ARI                                           
069400     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
069500     CALL POSTSUM USING POSTSUM-PARM                                      
069600                                                                          
069700     .                                                                    
069800     EJECT                                                                
069900 D-SKAPA-CSG SECTION.                                                     
069910     MOVE 'D-SKAPA-CSG '  TO CURRENT-SECTION                              
070000     SKIP2                                                                
070010                                                                          
070100     PERFORM DA-NOLLA-CSG                                                 
070200     MOVE 'CSG' TO CSG-IDPT                                               
070300                                                                          
070400       IF IN-KDGK-SORT = +2                                               
070500         MOVE '3324'          TO CSG-KDFORBR                              
071000         MOVE WS-BEFTAG-NL    TO CSG-BEFTAG                               
071100       ELSE                                                               
071200         MOVE '1441'          TO CSG-KDFORBR                              
071700         MOVE WS-BEFTAG-SVE   TO CSG-BEFTAG                               
071800       END-IF                                                             
071900       MOVE 'CDC'             TO CSG-KDGODSM                              
072000                                 CSG-KDGODSM-DET                          
072100       IF IN-ADINPORT > SPACE                                             
072110         MOVE 'CDC-'       TO CSG-KDGODSM(1:4)                            
072120         MOVE IN-ADINPORT  TO CSG-KDGODSM(5:4)                            
072130         MOVE 'CDC-'       TO CSG-KDGODSM-DET(1:4)                        
072140         MOVE IN-ADINPORT  TO CSG-KDGODSM-DET(5:4)                        
072400       END-IF                                                             
072500                                                                          
072600     WRITE CSG FROM CSG-W221CSG                                           
072700     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
072800     CALL POSTSUM USING POSTSUM-PARM                                      
072900                                                                          
073000     .                                                                    
073100     EJECT                                                                
073200                                                                          
073300 DA-NOLLA-CSG SECTION.                                                    
073310     MOVE 'DA-NOLLA-CSG '  TO CURRENT-SECTION                             
073400     SKIP2                                                                
073410                                                                          
073500     MOVE SPACE TO CSG-IDPT                                               
073600                   CSG-BEFTAG                                             
073700                   CSG-KDGODSM                                            
073800                   CSG-KDGODSM-DET                                        
073900                   CSG-KDFORBR                                            
074000     .                                                                    
074100     EJECT                                                                
074200                                                                          
074300 E-SKAPA-ARD-PDI-SAD-DST SECTION.                                         
074310     MOVE 'E-SKAPA-ARD-PDI-SAD-DST'  TO CURRENT-SECTION                   
074400     SKIP2                                                                
074410                                                                          
074500     PERFORM EB-SKAPA-OCH-SKRIV-ARD                                       
074600     PERFORM EC-SKAPA-OCH-SKRIV-PDI                                       
074700     PERFORM ED-SKAPA-OCH-SKRIV-SAD                                       
074800     MOVE ZERO       TO TOT-ANTAL                                         
074900                                                                          
075000     MOVE +0 TO IX                                                        
075100     PERFORM IMS-GET-WDL201                                               
075200     PERFORM UNTIL SEGMENT-SAKNAS OR IX = +3                              
075300        PERFORM IMS-GNP-WDL211                                            
075400        MOVE NEJ TO TRAFF                                                 
075500        PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                        
075600          MOVE INLE-INL-DAINLEV TO SPAR-DAINLEV                           
075700                                   W-DAINLEV                              
075800          PERFORM IMS-GNP-WDL221                                          
075900          IF SEGMENT-FINNS                                                
076000            IF INLE-MOT-IDLEVNR   = IN-IDLEVNR   AND                      
076100               INLE-MOT-KDRT      = 0            AND                      
076200               INLE-MOT-IDAVINR   > 0            AND                      
076300              (INLE-MOT-IDPTYP    = 'R31' OR 'R32') AND                   
076400               INLE-MOT-KDAVVANT NOT = 2                                  
076500              MOVE INLE-MOT-TIAVIDAT   TO TMP1-YYMMDD                     
076600              MOVE 930601              TO TMP2-YYMMDD                     
076700              PERFORM WY2000P1                                            
076800              IF IN-IDKAT = 'GEM' AND TMP1-YYMMDD < TMP2-YYMMDD           
076900                MOVE 'GE' TO STATUS-WS                                    
077000              ELSE                                                        
077100                MOVE JA            TO TRAFF                               
077200                ADD +1 TO IX                                              
077300              END-IF                                                      
077400            ELSE                                                          
077500              PERFORM IMS-GNP-WDL211                                      
077600            END-IF                                                        
077700          ELSE                                                            
077800            PERFORM IMS-GNP-WDL211                                        
077900          END-IF                                                          
078000        END-PERFORM                                                       
078100                                                                          
078200        IF TRAFF = JA                                                     
078300           IF INLE-MOT-IDFS > SPACE                                       
078400              MOVE INLE-MOT-IDFS        TO TAB-IDNRINL(IX)                
078500           ELSE                                                           
078600              MOVE INLE-MOT-IDAVINR     TO TAB-IDNRINL(IX)                
078700           END-IF                                                         
078800           MOVE INLE-MOT-TIAVIDAT       TO TAB-TISENINL(IX)               
078900           IF INLE-MOT-IDPTYP = 'R31'                                     
079000              MOVE INLE-MOT-KVAVIS      TO TAB-KVSENINL(IX)               
079100              IF SPAR-DAINLEV  NOT > DAINLEV-NYCKEL                       
079200                 ADD INLE-MOT-KVAVIS    TO TOT-ANTAL                      
079300              END-IF                                                      
079400           ELSE                                                           
079500              MOVE INLE-MOT-KVANTMOT    TO TAB-KVSENINL(IX)               
079600              IF SPAR-DAINLEV  NOT > DAINLEV-NYCKEL                       
079700                 ADD INLE-MOT-KVANTMOT  TO TOT-ANTAL                      
079800              END-IF                                                      
079900           END-IF                                                         
080000        END-IF                                                            
080100     END-PERFORM                                                          
080200                                                                          
080300     IF IX > 0                                                            
080400        PERFORM IMS-GNP-WDL211                                            
080500        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
080600                INLE-INL-DAINLEV > DAINLEV-NYCKEL                         
080700           MOVE INLE-INL-DAINLEV TO W-DAINLEV                             
080800           PERFORM IMS-GNP-WDL221                                         
080900           IF SEGMENT-FINNS                                               
081000              IF INLE-MOT-IDLEVNR = IN-IDLEVNR AND                        
081100                 INLE-MOT-KDRT    = 0          AND                        
081200                 INLE-MOT-IDAVINR > 0          AND                        
081300                (INLE-MOT-IDPTYP  = 'R31' OR 'R32')                       
081400                 MOVE INLE-MOT-TIAVIDAT   TO TMP1-YYMMDD                  
081500                 MOVE 930601              TO TMP2-YYMMDD                  
081600                 PERFORM WY2000P1                                         
081700                 IF IN-IDKAT = 'GEM' AND                                  
081800                    TMP1-YYMMDD < TMP2-YYMMDD                             
081900                    CONTINUE                                              
082000                 ELSE                                                     
082100                    IF INLE-MOT-IDPTYP = 'R31'                            
082200                       ADD INLE-MOT-KVAVIS TO TOT-ANTAL                   
082300                    ELSE                                                  
082400                       ADD INLE-MOT-KVANTMOT TO TOT-ANTAL                 
082500                    END-IF                                                
082600                 END-IF                                                   
082700              END-IF                                                      
082800           END-IF                                                         
082900           PERFORM IMS-GNP-WDL211                                         
083000        END-PERFORM                                                       
083100     END-IF                                                               
083200                                                                          
083300     PERFORM EE-SKAPA-OCH-SKRIV-DST                                       
083400                                                                          
083500     .                                                                    
083600     EJECT                                                                
083700 EB-SKAPA-OCH-SKRIV-ARD SECTION.                                          
083710     MOVE 'EB-SKAPA-OCH-SKRIV-ARD' TO CURRENT-SECTION                     
083800     SKIP2                                                                
083810                                                                          
083900*--  NOLLA ARD-SEGMENT                                                    
084000     MOVE SPACE TO ARD-IDPT                                               
084100                   ARD-IDART                                              
084200                   ARD-BESTNR                                             
084300                   ARD-BELEVART14                                         
084400                                                                          
084500     MOVE 'ARD' TO ARD-IDPT                                               
084600     MOVE IN-IDARTNR TO WS-IDARTNR                                        
084700                        W-IDARTNR                                         
084800     MOVE IN-IDLEVNR TO W-IDLEVNR-MIN                                     
084900                        W-IDLEVNR-MAX                                     
085000     PERFORM IMS-GU-WDF501                                                
085100     IF SEGMENT-FINNS                                                     
085200        PERFORM IMS-GNP-WDF502                                            
085300        PERFORM UNTIL SEGMENT-SAKNAS                                      
085400                                                                          
085500          MOVE XLEV-BELEVART(1:14) TO ARD-BELEVART14                      
085600          PERFORM IMS-GNP-WDF502                                          
085700        END-PERFORM                                                       
085800     END-IF                                                               
085900                                                                          
086000     INSPECT WS-IDARTNR TALLYING RAKNARE FOR LEADING '0'                  
086100     ADD +1 TO RAKNARE                                                    
086200     UNSTRING WS-IDARTNR INTO ARD-IDART WITH POINTER RAKNARE              
086300     MOVE ZERO TO RAKNARE                                                 
086400     IF IN-IDBEST > 0                                                     
086500       MOVE IN-IDBEST TO WS-IDBEST                                        
086600       MOVE WS-IDBEST TO ARD-BESTNR                                       
086700     ELSE                                                                 
086800       MOVE DEF-BESTNR TO ARD-BESTNR                                      
086900     END-IF                                                               
087000                                                                          
087100     WRITE ARD FROM ARD-W221ARD                                           
087200     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
087300     CALL POSTSUM USING POSTSUM-PARM                                      
087400                                                                          
087500     .                                                                    
087600     EJECT                                                                
087700 EC-SKAPA-OCH-SKRIV-PDI SECTION.                                          
087710     MOVE 'EC-SKAPA-OCH-SKRIV-PDI '  TO CURRENT-SECTION                   
087800     SKIP2                                                                
087810                                                                          
087900     IF IN-IDLPLAN-ART > 0                                                
088000                                                                          
088100*--    NOLLA PDI-SEGMENT                                                  
088200       MOVE SPACE TO PDI-IDPT                                             
088300                     PDI-IDLPLAN                                          
088400                                                                          
088500       MOVE 'PDI' TO PDI-IDPT                                             
088600       MOVE IN-IDLPLAN-ART TO WS-IDLPLAN                                  
088700       MOVE WS-IDLPLAN TO PDI-IDLPLAN                                     
088800                                                                          
088900       WRITE PDI FROM PDI-W221PDI                                         
089000       MOVE W22162-TRANSID TO POSTSUM-TRANSID                             
089100       CALL POSTSUM USING POSTSUM-PARM                                    
089200     END-IF                                                               
089300                                                                          
089400     .                                                                    
089500     EJECT                                                                
089600 ED-SKAPA-OCH-SKRIV-SAD SECTION.                                          
089610     MOVE 'ED-SKAPA-OCH-SKRIV-SAD ' TO CURRENT-SECTION                    
089700     SKIP2                                                                
089710                                                                          
089800*--  NOLLA SAD-SEGMENT                                                    
089900     MOVE SPACE TO SAD-IDPT                                               
090000     MOVE ZERO  TO SAD-KDRLSTYP                                           
090100                                                                          
090200     MOVE 'SAD' TO SAD-IDPT                                               
090300     MOVE 1 TO SAD-KDRLSTYP                                               
090400                                                                          
090500     WRITE SAD FROM SAD-W221SAD                                           
090600     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
090700     CALL POSTSUM USING POSTSUM-PARM                                      
090800                                                                          
090900     .                                                                    
091000     EJECT                                                                
091100 EE-SKAPA-OCH-SKRIV-DST SECTION.                                          
091110     MOVE 'EE-SKAPA-OCH-SKRIV-DST '  TO CURRENT-SECTION                   
091200     SKIP2                                                                
091210                                                                          
091300*--  NOLLA DST-SEGMENT                                                    
091400     MOVE SPACE TO DST-IDPT                                               
091500                   DST-KVART-BREST-ALPHA                                  
091600     MOVE ZERO  TO DST-TIYYMMDD-AVST                                      
091700                   DST-KVINLAAR                                           
091800                                                                          
091900     MOVE 'DST' TO DST-IDPT                                               
092000     MOVE DAGENS-DATUM TO DST-TIYYMMDD-AVST                               
092100     IF TOT-ANTAL > 0                                                     
092200        MOVE TOT-ANTAL TO DST-KVINLAAR                                    
092300     END-IF                                                               
092400                                                                          
093110     MOVE +0        TO DST-KVART-BREST                                    
093200                                                                          
093300     WRITE DST FROM DST-W221DST                                           
093400     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
093500     CALL POSTSUM USING POSTSUM-PARM                                      
093600                                                                          
093700     .                                                                    
093800     EJECT                                                                
093900 F-SKAPA-OCH-SKRIV-PDN SECTION.                                           
093910     MOVE 'F-SKAPA-OCH-SKRIV-PDN '  TO CURRENT-SECTION                    
094000     SKIP2                                                                
094010                                                                          
094100     PERFORM FA-NOLLA-PDN                                                 
094200     IF SPECIAL-LEV                                                       
094300       PERFORM FB-FYLL-I-SIST-FORST                                       
094400     ELSE                                                                 
094500       PERFORM FC-FYLL-I-RATT-ORDNING                                     
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 FA-NOLLA-PDN SECTION.                                                    
094910     MOVE 'FA-NOLLA-PDN '  TO CURRENT-SECTION                             
095000     SKIP2                                                                
095010                                                                          
095100     MOVE SPACE TO PDN-IDPT                                               
095200                   PDN-TISENINL-ALPHA                                     
095300                   PDN-KVSENINL-ALPHA                                     
095400     MOVE ZERO  TO PDN-IDNRINL                                            
095500     .                                                                    
095600     EJECT                                                                
095700 FB-FYLL-I-SIST-FORST SECTION.                                            
095710     MOVE 'FB-FYLL-I-SIST-FORST ' TO CURRENT-SECTION                      
095800                                                                          
095900     MOVE +3 TO IX                                                        
096000     PERFORM UNTIL IX = 0                                                 
096100        IF TAB-IDNRINL(IX)  = 0 OR                                        
096200          (TAB-TISENINL(IX) = 0 AND                                       
096300           TAB-KVSENINL(IX) = 0    )                                      
096400          CONTINUE                                                        
096500        ELSE                                                              
096600          MOVE 'PDN' TO PDN-IDPT                                          
096700          INSPECT TAB-IDNRINL(IX) TALLYING RAKNARE                        
096800                  FOR LEADING '0'                                         
096900          ADD +1 TO RAKNARE                                               
097000          UNSTRING TAB-IDNRINL(IX) INTO PDN-IDNRINL                       
097100                                   WITH POINTER RAKNARE                   
097200          MOVE ZERO TO RAKNARE                                            
097300          MOVE TAB-TISENINL(IX) TO PDN-TISENINL                           
097400          MOVE TAB-KVSENINL(IX) TO PDN-KVSENINL                           
097500          IF PDN-TISENINL = ZERO                                          
097600             MOVE SPACE         TO PDN-TISENINL-ALPHA                     
097700          END-IF                                                          
098100                                                                          
098200          WRITE PDN FROM PDN-W221PDN                                      
098300          MOVE W22162-TRANSID TO POSTSUM-TRANSID                          
098400          CALL POSTSUM USING POSTSUM-PARM                                 
098500                                                                          
098600          MOVE ZERO TO TAB-IDNRINL(IX)                                    
098700                       TAB-TISENINL(IX)                                   
098800                       TAB-KVSENINL(IX)                                   
098900          PERFORM FA-NOLLA-PDN                                            
099000        END-IF                                                            
099100        SUBTRACT 1 FROM IX                                                
099200     END-PERFORM                                                          
099300     .                                                                    
099400     EJECT                                                                
099500 FC-FYLL-I-RATT-ORDNING SECTION.                                          
099510     MOVE 'FC-FYLL-I-RATT-ORDNING ' TO CURRENT-SECTION                    
099600                                                                          
099700     MOVE +1 TO IX                                                        
099800     PERFORM UNTIL IX > +3 OR                                             
099900                    TAB-IDNRINL(IX)  = 0 OR                               
100000                   (TAB-TISENINL(IX) = 0 AND                              
100100                    TAB-KVSENINL(IX) = 0    )                             
100200        MOVE 'PDN' TO PDN-IDPT                                            
100300        INSPECT TAB-IDNRINL(IX) TALLYING RAKNARE FOR LEADING '0'          
100400        ADD +1 TO RAKNARE                                                 
100500        UNSTRING TAB-IDNRINL(IX) INTO PDN-IDNRINL                         
100600                                 WITH POINTER RAKNARE                     
100700        MOVE ZERO TO RAKNARE                                              
100800        MOVE TAB-TISENINL(IX) TO PDN-TISENINL                             
100900        MOVE TAB-KVSENINL(IX) TO PDN-KVSENINL                             
101000        IF PDN-TISENINL = ZERO                                            
101100           MOVE SPACE         TO PDN-TISENINL-ALPHA                       
101200        END-IF                                                            
101600                                                                          
101700        WRITE PDN FROM PDN-W221PDN                                        
101800        MOVE W22162-TRANSID TO POSTSUM-TRANSID                            
101900        CALL POSTSUM USING POSTSUM-PARM                                   
102000                                                                          
102100        MOVE ZERO TO TAB-IDNRINL(IX)                                      
102200                     TAB-TISENINL(IX)                                     
102300                     TAB-KVSENINL(IX)                                     
102400        ADD +1 TO IX                                                      
102500        PERFORM FA-NOLLA-PDN                                              
102600     END-PERFORM                                                          
102700     .                                                                    
102800     EJECT                                                                
102900 G-SKAPA-OCH-SKRIV-DEL SECTION.                                           
102910     MOVE 'G-SKAPA-OCH-SKRIV-DEL '  TO CURRENT-SECTION                    
103000     SKIP2                                                                
103010                                                                          
103100     PERFORM GA-NOLLA-DEL                                                 
103200                                                                          
103300     IF  IN-IDFTG   = SAVE-IDFTG                                          
103400     AND IN-IDLEVNR = SAVE-IDLEVNR                                        
103500     AND IN-IDARTNR = SAVE-IDARTNR                                        
103600        CONTINUE                                                          
103700     ELSE                                                                 
103800        MOVE +0 TO W-COUNT-W221DEL                                        
103900     END-IF                                                               
104000                                                                          
104010     MOVE IN-IDARTNR TO W-IDARTNR-D9                                      
104020     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
104030     MOVE IN-IDLEVNR TO W-IDLEVNR                                         
104040     PERFORM IMS-GET-WDD902                                               
104050                                                                          
104100     IF SEGMENT-FINNS                                                     
104200        MOVE NEJ TO DEL-SEGMENT                                           
104500                                                                          
104600        PERFORM IMS-GET-WDK611                                            
104700        MOVE +1                       TO IX-DAG                           
104800        PERFORM UNTIL IX-DAG > 5                                          
104900           IF CLAG-TILEVDAG (IX-DAG) > ZERO                               
105000              MOVE CLAG-TILEVDAG (IX-DAG) TO SPAR-TILEVDAG                
105100              MOVE +5                 TO IX-DAG                           
105200           END-IF                                                         
105300           ADD +1                     TO IX-DAG                           
105400        END-PERFORM                                                       
109700                                                                          
109800        PERFORM IMS-GNP-WDD905                                            
109900        PERFORM UNTIL SEGMENT-SAKNAS                                      
110000           IF INLB-KDAVROP = +2                                           
110100              MOVE INLB-DAAVROP-AVS TO W-DAAVROP-AVS                      
110200              MOVE IN-IDLEVNR       TO WS-IDLEVNR-EMIL                    
110300              IF EJ-GODK-EMIL-LEVNR                                       
110400                 COMPUTE WS-TIAAVVD = (W-TIAVROP-AVS * 10 ) +             
110500                                       SPAR-TILEVDAG                      
110600              ELSE                                                        
110700                 COMPUTE WS-TIAAVVD = (W-TIAVROP-AVS * 10 ) +             
110800                                       INLB-TILEVDAG                      
110900              END-IF                                                      
111000              MOVE WS-TIAAVVD TO WS-TIAVROP-AVS-AAVVD                     
111100              MOVE WS-TIAAVVD TO DAYS-TIDATE1                             
111200              MOVE 'YYWWD '   TO DAYS-KDDATFMT1                           
111300              MOVE 'YYMMDD'   TO DAYS-KDDATFMT2                           
111400              MOVE SPACE      TO DAYS-TIDATE2                             
111500                                 DAYS-IDCALEND                            
111600              MOVE ZERO       TO DAYS-KVDAYS                              
111700                                                                          
111800              CALL WZ20DAYS USING DAYS-WZ20DAYS                           
111900                                                                          
112000              MOVE DAYS-TIDATE2(1:6) TO DEL-TISTART                       
112100                                                                          
112200              MOVE WS-TIAVROP-AVS-AAVVD   TO TMP1-YYWWD                   
112300              MOVE INNEVARANDE-AAVVD      TO TMP2-YYWWD                   
112400              PERFORM WY2000P2                                            
112500              IF TMP1-YYWWD <= TMP2-YYWWD                                 
112600                 MOVE '3'   TO DEL-KDDELTYP                               
112700              ELSE                                                        
112800                 MOVE SPACE TO DEL-KDDELTYP                               
112900              END-IF                                                      
113000              MOVE INLB-KVAVROP TO DEL-KVART-AVROP                        
113200                                                                          
114000              PERFORM GD-KOLLA-MOT-FRYSTID                                
114100              MOVE 'DEL' TO DEL-IDPT                                      
114200              PERFORM GB-SKRIV-DEL                                        
114300              MOVE JA TO DEL-SEGMENT                                      
114400              PERFORM GA-NOLLA-DEL                                        
114600           END-IF                                                         
114700           PERFORM IMS-GNP-WDD905                                         
114800        END-PERFORM                                                       
114900        IF DEL-SEGMENT = NEJ                                              
115000           MOVE 'DEL'        TO DEL-IDPT                                  
115100           MOVE ZERO         TO DEL-KVART-AVROP                           
115200           MOVE SPACE        TO DEL-KDDELTYP                              
115210           MOVE 4            TO DEL-KDDELIND                              
115900           PERFORM GC-DATUM-PLUS-ETT                                      
116000           PERFORM S09-SKRIV-DEL                                          
116100        END-IF                                                            
116200     ELSE                                                                 
116300        MOVE 'DEL'        TO DEL-IDPT                                     
116400        MOVE ZERO         TO DEL-KVART-AVROP                              
116500        MOVE SPACE        TO DEL-KDDELTYP                                 
116510        MOVE 4            TO DEL-KDDELIND                                 
117200        MOVE DAGENS-DATUM TO DEL-TISTART                                  
117300        PERFORM S09-SKRIV-DEL                                             
117400     END-IF                                                               
117500     .                                                                    
117600     EJECT                                                                
117700                                                                          
117800 GA-NOLLA-DEL SECTION.                                                    
117810     MOVE 'GA-NOLLA-DEL '  TO CURRENT-SECTION                             
117900     SKIP2                                                                
117910                                                                          
118000     MOVE SPACE TO DEL-IDPT                                               
118100                   DEL-TISTOPP                                            
118200                   DEL-KDDELTYP                                           
118300     MOVE ZERO  TO DEL-TISTART                                            
118400                   DEL-KVART-AVROP                                        
118500                   DEL-KDDELIND                                           
118600                                                                          
118700     .                                                                    
118800     EJECT                                                                
118900                                                                          
119000 GB-SKRIV-DEL SECTION.                                                    
119010     MOVE 'GB-SKRIV-DEL '  TO CURRENT-SECTION                             
119100     SKIP2                                                                
119110                                                                          
119200     PERFORM S09-SKRIV-DEL                                                
119300     .                                                                    
119400     EJECT                                                                
119500                                                                          
119600 GC-DATUM-PLUS-ETT SECTION.                                               
119610     MOVE 'GC-DATUM-PLUS-ETT ' TO CURRENT-SECTION                         
119700     SKIP2                                                                
119710                                                                          
119800*--  DATUM SKALL VARA DAGENS-DATUM + 1,                                   
119900*--  DVS DAGEN EFTER NATTKÖRNINGEN                                        
120000                                                                          
120100     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
120200     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
120300     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
120400     MOVE SPACE        TO DAYS-TIDATE2                                    
120500                          DAYS-IDCALEND                                   
120600     MOVE +1           TO DAYS-KVDAYS                                     
120700                                                                          
120800     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
120900                                                                          
121000     MOVE DAYS-TIDATE2(1:6) TO DEL-TISTART                                
122000     .                                                                    
123000     EJECT                                                                
124000 GD-KOLLA-MOT-FRYSTID SECTION.                                            
124001     MOVE 'GD-KOLLA-MOT-FRYSTID '  TO CURRENT-SECTION                     
124010                                                                          
124100     MOVE CLAG-KVVECKOR-LT     TO W-ANTAL-VECKOR                          
124101                                                                          
124107*-- MÅNDAGENS BATCH W221V2 + W221D2 HAR FREDAGENS DATUM I SOP.            
124109*-- PÅ MÅNDAGAR SKALL MAN KOLLA PÅ MASKINDATUM OCH JÄMFÖRA HELA           
124110*-- VECKAN MOT KONTR-DATUM. EJ PÅ DAGNUMMER I VECKAN.                     
124111*-- SÄTT DAGNR=7 I KONTR-DATUM.                                           
124113     IF MASKINENS-DAGNR = 1                                               
124130       MOVE MASKINENS-AAVVD    TO KONTR-DATUM                             
124140       MOVE 7                  TO KONTR-TID                               
124201     ELSE                                                                 
124202       MOVE INNEVARANDE-AAVVD  TO KONTR-DATUM                             
124203       MOVE 7                  TO KONTR-TID                               
124220     END-IF                                                               
124230                                                                          
124300     MOVE KONTR-TIAAVV         TO WS-TIAAVV-FRYS                          
124400     CALL W009VADD USING WS-TIAAVV-FRYS W-ANTAL-VECKOR                    
124500     MOVE WS-TIAAVV-FRYS       TO KONTR-TIAAVV                            
124600     IF WS-TIAVROP-AVS-AAVVD > KONTR-DATUM                                
124610       MOVE 4                  TO DEL-KDDELIND                            
125300     ELSE                                                                 
125400       MOVE 1                  TO DEL-KDDELIND                            
125500     END-IF                                                               
125600     .                                                                    
125700     EJECT                                                                
125800 S01-LAS-INFIL SECTION.                                                   
125810     MOVE 'S01-LAS-INFIL '  TO CURRENT-SECTION                            
125900     SKIP3                                                                
126000                                                                          
126100     READ W22160 INTO IN-W22160 AT END MOVE JA TO INFIL-EOF               
127000                                                                          
128000       IF INFIL-EOF = NEJ                                                 
129000          IF IN-IDARTNR = WS-OLD-IDARTNR                                  
130000             PERFORM UNTIL IN-IDARTNR NOT = WS-OLD-IDARTNR OR             
140000                                  INFIL-EOF = JA                          
150000                READ W22160 INTO IN-W22160 AT END MOVE JA TO              
160000                                  INFIL-EOF                               
170000                END-READ                                                  
180000             END-PERFORM                                                  
190000          END-IF                                                          
200000          MOVE IN-IDARTNR TO WS-OLD-IDARTNR                               
210000       END-IF                                                             
220000                                                                          
230000       IF INFIL-EOF = NEJ                                                 
240000          MOVE IN-IDLEVNR TO IN-IDLEVNR-ID                                
250000                             SPAR-IDLEVNR-AKTUELL                         
260000          MOVE IN-IDARTNR TO IN-IDARTNR-ID                                
270000          MOVE INFIL-TRANSID TO POSTSUM-TRANSID                           
280000          CALL POSTSUM USING POSTSUM-PARM                                 
290000       ELSE                                                               
300000          MOVE MAX-VARDE TO INFIL-ID                                      
300100       END-IF                                                             
300200     END-READ                                                             
300300     .                                                                    
300400     EJECT                                                                
300500 S02-HAMTA-INFO-FRAN-WDF1 SECTION.                                        
300510     MOVE 'S02-HAMTA-INFO-FRAN-WDF1' TO CURRENT-SECTION                   
300600     SKIP2                                                                
300610                                                                          
300700     MOVE IN-IDLEVNR TO W-IDLEVNR                                         
300800     PERFORM IMS-GET-WDF101                                               
300900     IF SEGMENT-FINNS                                                     
301000        MOVE +1 TO IX                                                     
301100        MOVE NEJ TO TRAFF                                                 
301200        PERFORM UNTIL IX > +5 OR TRAFF = JA                               
301300           IF LEVA-LEV-TILEVDAG(IX) > 0                                   
301400              MOVE LEVA-LEV-TILEVDAG(IX) TO SPAR-TILEVDAG                 
301500              MOVE JA TO TRAFF                                            
301600           ELSE                                                           
301700              ADD +1 TO IX                                                
301800           END-IF                                                         
301900        END-PERFORM                                                       
302000        IF TRAFF = NEJ                                                    
302100           MOVE +1 TO SPAR-TILEVDAG                                       
302200        END-IF                                                            
302300     ELSE                                                                 
302400        MOVE +1 TO SPAR-TILEVDAG                                          
302500     END-IF                                                               
302600                                                                          
302700     .                                                                    
302800     EJECT                                                                
302900 S03-SKAPA-HEADER SECTION.                                                
302910     MOVE 'S03-SKAPA-HEADER '  TO CURRENT-SECTION                         
303000     SKIP2                                                                
303010                                                                          
303100     MOVE '001'              TO HEAD-IDPT                                 
303200                                62-TRANSTYP                               
303300     MOVE 'WPAR'             TO HEAD-IDSECN                               
303400     MOVE 'AMOS'             TO HEAD-IDRECN                               
303500     MOVE 'WPAR3201'         TO HEAD-IDFILE                               
303600     MOVE MASKINENS-DATUM    TO HEAD-TIFILE-DAT                           
303700     MOVE MASKINENS-TID(1:6) TO HEAD-TIFILE-KL                            
303800                                                                          
303900     WRITE HEAD FROM HEAD-W221001                                         
304000     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
304100     CALL POSTSUM USING POSTSUM-PARM                                      
304200                                                                          
304300     MOVE SPACE TO 62-TRANSTYP                                            
304400     .                                                                    
304500     EJECT                                                                
304600 S04-SKAPA-TRAILER SECTION.                                               
304610     MOVE 'S04-SKAPA-TRAILER ' TO CURRENT-SECTION                         
304700     SKIP2                                                                
304710                                                                          
304800     MOVE '003' TO TRAIL-IDPT                                             
304900                   62-TRANSTYP                                            
305000                                                                          
305100     WRITE TRAIL FROM TRAIL-W221003                                       
305200     MOVE W22162-TRANSID TO POSTSUM-TRANSID                               
305300     CALL POSTSUM USING POSTSUM-PARM                                      
305400                                                                          
305500     MOVE SPACE TO 62-TRANSTYP                                            
305600     .                                                                    
305700     EJECT                                                                
305800 S09-SKRIV-DEL SECTION.                                                   
305810     MOVE 'S09-SKRIV-DEL '  TO CURRENT-SECTION                            
305900     SKIP2                                                                
305910                                                                          
306000     ADD +1                 TO W-COUNT-W221DEL                            
306100     IF W-COUNT-W221DEL > +200                                            
306200        CONTINUE                                                          
306300     ELSE                                                                 
306400        WRITE DEL         FROM DEL-W221DEL                                
306500        MOVE W22162-TRANSID TO POSTSUM-TRANSID                            
306600        CALL POSTSUM     USING POSTSUM-PARM                               
306700     END-IF                                                               
306800                                                                          
306900     .                                                                    
307000     EJECT                                                                
307100***********************IMS-SEKTIONER                                      
307200 IMS-GET-WDF101 SECTION.                                                  
307210     MOVE 'IMS-GET-WDF101'  TO DBS-SECTION                                
307220                                                                          
307300     SKIP2                                                                
307400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
307500     DELIMITED BY SIZE INTO SSA1                                          
307600     MOVE '  GE' TO GODK-STATUSKODER                                      
307700     CALL CBLTDLI USING GU WLLEVA-PCB DLI-IO-AREA SSA1                    
307800     MOVE WLLEVA-STATUS-CODE TO STATUS-WS                                 
307900     PERFORM IMS-STATUSKONTROLL                                           
308000     SKIP3                                                                
308100     .                                                                    
308200 IMS-GET-WDL201 SECTION.                                                  
308210     MOVE 'IMS-GET-WDL201 '  TO DBS-SECTION.                              
308220                                                                          
308300     SKIP2                                                                
308400     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
308500     DELIMITED BY SIZE INTO SSA1                                          
308600     MOVE '  GE' TO GODK-STATUSKODER                                      
308700     CALL CBLTDLI USING GU WLINLE-PCB DLI-IO-AREA SSA1                    
308800     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
308900     PERFORM IMS-STATUSKONTROLL                                           
309000     SKIP3                                                                
309100     .                                                                    
309200 IMS-GNP-WDL211 SECTION.                                                  
309210     MOVE 'IMS-GNP-WDL211 ' TO DBS-SECTION                                
309220                                                                          
309300     SKIP2                                                                
309400     MOVE 'WLINLE11 ' TO SSA1                                             
309500     MOVE '  GE' TO GODK-STATUSKODER                                      
309600     CALL CBLTDLI USING GNP WLINLE-PCB DLI-IO-AREA SSA1                   
309700     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
309800     PERFORM IMS-STATUSKONTROLL                                           
309900     SKIP3                                                                
310000     .                                                                    
310100 IMS-GNP-WDL221 SECTION.                                                  
310110     MOVE 'IMS-GNP-WDL221 '  TO DBS-SECTION                               
310120                                                                          
310200     SKIP2                                                                
310300     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
310400     DELIMITED BY SIZE INTO SSA1                                          
310500     MOVE 'WLINLE21 ' TO SSA2                                             
310600     MOVE '  GE' TO GODK-STATUSKODER                                      
310700     CALL CBLTDLI USING GNP WLINLE-PCB DLI-IO-AREA SSA1 SSA2              
310800     MOVE WLINLE-STATUS-CODE TO STATUS-WS                                 
310900     PERFORM IMS-STATUSKONTROLL                                           
311000     SKIP3                                                                
311100     .                                                                    
311200 IMS-GET-WDD902 SECTION.                                                  
311210     MOVE 'IMS-GET-WDD902 '  TO DBS-SECTION                               
311220                                                                          
311300     SKIP2                                                                
311400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
311500     DELIMITED BY SIZE INTO SSA1                                          
311600     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
311700     DELIMITED BY SIZE INTO SSA2                                          
311800     MOVE '  GE' TO GODK-STATUSKODER                                      
311900     CALL CBLTDLI USING GU WLINLB-PCB DLI-IO-AREA SSA1 SSA2               
312000     MOVE WLINLB-STATUS-CODE TO STATUS-WS                                 
312100     PERFORM IMS-STATUSKONTROLL                                           
312200     SKIP3                                                                
312300     .                                                                    
314200 IMS-GNP-WDD905 SECTION.                                                  
314210     MOVE 'IMS-GNP-WDD905 '  TO DBS-SECTION                               
314220                                                                          
314300     SKIP2                                                                
314400     MOVE 'WLINLB23 ' TO SSA1                                             
314500     MOVE '  GE' TO GODK-STATUSKODER                                      
314600     CALL CBLTDLI USING GNP WLINLB-PCB DLI-IO-AREA SSA1                   
314700     MOVE WLINLB-STATUS-CODE TO STATUS-WS                                 
314800     PERFORM IMS-STATUSKONTROLL                                           
314900     EJECT                                                                
315000     .                                                                    
315100 IMS-GET-WDK611 SECTION.                                                  
315110     MOVE 'IMS-GET-WDK611 '  TO DBS-SECTION                               
315120                                                                          
315200     SKIP2                                                                
315300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
315400     DELIMITED BY SIZE INTO SSA1                                          
315500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
315600     DELIMITED BY SIZE INTO SSA2                                          
315700     MOVE '  GE' TO GODK-STATUSKODER                                      
315800     CALL CBLTDLI USING GU WLARTC-PCB DLI-IO-AREA3 SSA1 SSA2              
315900     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
316000     PERFORM IMS-STATUSKONTROLL                                           
316100     SKIP3                                                                
316200     .                                                                    
316300 IMS-GU-WDF501 SECTION.                                                   
316310     MOVE 'IMS-GU-WDF501 '  TO DBS-SECTION                                
316320                                                                          
316400     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
316500          DELIMITED BY SIZE INTO SSA1                                     
316600     MOVE '  GE' TO GODK-STATUSKODER                                      
316700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA-WDF5 SSA1                 
316800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
316900     PERFORM IMS-STATUSKONTROLL                                           
317000     .                                                                    
317100     EJECT                                                                
317200 IMS-GNP-WDF502 SECTION.                                                  
317210     MOVE 'IMS-GNP-WDF502 ' TO DBS-SECTION                                
317220                                                                          
317300     STRING 'WDF502  (WDF5KEY =>' W-WDF502-MIN                            
317400                 '&WDF5KEY =<' W-WDF502-MAX ')'                           
317500          DELIMITED BY SIZE INTO SSA1                                     
317600     MOVE '  GE' TO GODK-STATUSKODER                                      
317700     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA-WDF5 SSA1                
317800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
317900     PERFORM IMS-STATUSKONTROLL                                           
318000     .                                                                    
318100     EJECT                                                                
318200 IMS-STATUSKONTROLL SECTION.                                              
318300     SKIP2                                                                
318400     SET STATUS-IX TO 1                                                   
318500     SEARCH GODK-STATUS AT END CALL FELLOG                                
318600     WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                              
318700     CONTINUE                                                             
318800     END-SEARCH                                                           
318900                                                                          
319000     .                                                                    
319100 Z-FINIT   SECTION.                                                       
319200     SKIP3                                                                
319300     CLOSE  W22160                                                        
319400            W22162                                                        
319500*- - - - - - - - - - - - - - -  SKRIV UT ANTAL LÄSTA OCH                  
319600*                               SKRIVNA POSTER                            
319700     MOVE 'S' TO POSTSUM-OPKOD                                            
319800     CALL POSTSUM USING POSTSUM-PARM                                      
319900     .                                                                    
320000     EJECT                                                                
320100*    -COPY WY2000P1                                                       
320200     EJECT                                                                
320300*    -COPY WY2000P2                                                       
