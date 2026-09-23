000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1012200.                                                
000300*AUTHOR.         BODIL LINDAHL.                                           
000400*DATE-WRITTEN.   JULI 1986.                                               
000500*    FUNKTION.                                                            
000600*                ÖVERSÄTTNING AV RS-UNIKA BENÄMNINGAR.                    
000700*                                                                         
000800*                ÄVEN TECHLA-BENÄMNINGAR (F.D. NAMNLEX)                   
000900*                MED FLAGGA 'EJ ÖVERSATT' KAN ÖVERSÄTTAS -                
001000*                FLAGGAN FÖRBLIR 'EJ ÖVERSATT' OCH ÄNDRAS                 
001100*                BARA VID UPPDATERING FRÅN TECHLA.                        
001200     SKIP2                                                                
001300*    INDATA.                                                              
001400*        TRANSAKTION: W1T122                                              
001500*                     W1T122U                                             
001600*        MID:         W1I12201                                            
001700*    UTDATA.                                                              
001800*        MOD:         W1O12201                                            
001900*                                                                         
002000*                                                                         
002100*    ÄNDRINGAR.                                                           
002200*        2000-11-13 NYTT SPRÅK (TURKISKA) TILLKOMMER  /C.E.               
002300*        2003-01-09 SÄTTER FLAENDR TILL JA VID FÖRÄNDR /C.E.              
002400*                                                                         
002500*      2005-06-15  9 nya språk tillkommer. eTracker 2053029.              
002600*                  Förändrad visning av översättningarna krävs för        
002700*                  att få plats med all info på skärmen.                  
002800*                  MFS formaten ändras därför oxå.                        
002900*                                                                         
003000*      2005-10-18  FLAENDR sätts bara till JA vid förändring ifall        
003100*                  KDPRODSL är mellan 11 och 29. (NEVIS-artikel)          
003200*                                                                         
003300*      2005-10-27  FLOVERSATT skall visas vid varje språk.                
003400*                  Installeras 4/12 2005                                  
003500*                  /C.E.              eTracker 2571884                    
003600*                                                                         
003700*      2006-01-31  Databasläsning ändrad m.a.p. PCB. Akut ändring         
003800*                  efter ABEND U0240 i PROD.                              
003900*                  /C.E.              eTracker 2946800                    
004000*                                                                         
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP3                                                                
004400 DATA DIVISION.                                                           
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W1012200'.            
005000 77  JA                          PIC X(1)    VALUE 'J'.                   
005100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005200 77  REGISTRERAD                 PIC X(1)    VALUE '®'.                   
005300 77  SVENSKA                     PIC X(3)    VALUE 'S  '.                 
005400 77  WS-NYCKLAR-RAETT            PIC X(3)    VALUE 'J'.                   
005500 77  WS-FLAENDR                  PIC X       VALUE 'N'.                   
005600 77  NEVIS-ART                   PIC X       VALUE 'N'.                   
005700 77  MAX-HOM                     PIC S9(3)   VALUE +2   COMP-3.           
005800 77  MAX-SPRAK-PLUS-1            PIC S9(3)   VALUE +26  COMP-3.           
005900 77  MAX-UPD-PLUS-1              PIC S9(3)   VALUE +12  COMP-3.           
006000 77  MAX-HOM-PLUS-1              PIC S9(3)   VALUE +3   COMP-3.           
006100 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006200 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
006300 77  MAX-IDSKYLT                 PIC S9(3)   VALUE +26  COMP-3.           
006400 77  IDSKYLT-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
006500 77  BEHANDLA-SW                 PIC X       VALUE 'J'.                   
006600     88  BEHANDLA                            VALUE 'J'.                   
006700     88  BEHANDLA-EJ                         VALUE 'N'.                   
006800     SKIP2                                                                
006900                                                                          
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100   03  WREVERSE                  PIC X(8)    VALUE 'WREVERSE'.            
007200   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
007300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007500                                                                          
007600 01  WS-BEART                    PIC X(25)  VALUE SPACE.                  
007700 01  WS-BEART-SV                 PIC X(25)  VALUE SPACE.                  
007800     SKIP2                                                                
007900 01  WS-KDHOMONYM                        PIC X.                           
008000 01  KDHOMONYM-WS REDEFINES WS-KDHOMONYM PIC 9(1).                        
008100     SKIP2                                                                
008200 01  SPARAD-GB                   PIC X(25) VALUE SPACE.                   
008300 01  SPARAD-FLOVERSATT           PIC X     VALUE SPACE.                   
008400     SKIP3                                                                
008500                                                                          
008600 01  UPD-TABELL.                                                          
008700*     --- ORDNINGEN PÅ MID-BEART ARRAYEN                                  
008800*     --- ANV. I B-UPPDATERA (INDEX BEGR. AV MAX-UPD-PLUS-1 )             
008900     03 FILLER                   PIC X(3)    VALUE 'GB '.                 
009000     03 FILLER                   PIC X(3)    VALUE 'USA'.                 
009100     03 FILLER                   PIC X(3)    VALUE 'D  '.                 
009200     03 FILLER                   PIC X(3)    VALUE 'DK '.                 
009300     03 FILLER                   PIC X(3)    VALUE 'F  '.                 
009400     03 FILLER                   PIC X(3)    VALUE 'E  '.                 
009500     03 FILLER                   PIC X(3)    VALUE 'P  '.                 
009600     03 FILLER                   PIC X(3)    VALUE 'MAL'.                 
009700     03 FILLER                   PIC X(3)    VALUE 'NL '.                 
009800     03 FILLER                   PIC X(3)    VALUE 'I  '.                 
009900     03 FILLER                   PIC X(3)    VALUE 'SF '.                 
010000                                                                          
010100 01  TAB11 REDEFINES UPD-TABELL.                                          
010200     03  FILLER OCCURS 11.                                                
010300        05  UPD-IDSKYLT          PIC X(3).                                
010400     SKIP3                                                                
010500                                                                          
010600                                                                          
010700 01  25-TABELL.                                                           
010800*     --- ORDNINGEN PÅ WDD311 ALLA SPRÅK UTOM SVENSKA                     
010900*     --- ANV. I C-LAES      (INDEX BEGR. AV MAX-SPRAK-PLUS-1 )           
011000     03 FILLER                   PIC X(3)    VALUE 'CZ '.                 
011100     03 FILLER                   PIC X(3)    VALUE 'D  '.                 
011200     03 FILLER                   PIC X(3)    VALUE 'DK '.                 
011300     03 FILLER                   PIC X(3)    VALUE 'E  '.                 
011400     03 FILLER                   PIC X(3)    VALUE 'F  '.                 
011500     03 FILLER                   PIC X(3)    VALUE 'GB '.                 
011600     03 FILLER                   PIC X(3)    VALUE 'GR '.                 
011700     03 FILLER                   PIC X(3)    VALUE 'H  '.                 
011800     03 FILLER                   PIC X(3)    VALUE 'I  '.                 
011900     03 FILLER                   PIC X(3)    VALUE 'IR '.                 
012000     03 FILLER                   PIC X(3)    VALUE 'J  '.                 
012100     03 FILLER                   PIC X(3)    VALUE 'KOR'.                 
012200     03 FILLER                   PIC X(3)    VALUE 'MAL'.                 
012300     03 FILLER                   PIC X(3)    VALUE 'NL '.                 
012400     03 FILLER                   PIC X(3)    VALUE 'P  '.                 
012500     03 FILLER                   PIC X(3)    VALUE 'PL '.                 
012600     03 FILLER                   PIC X(3)    VALUE 'RC '.                 
012700     03 FILLER                   PIC X(3)    VALUE 'RCN'.                 
012800     03 FILLER                   PIC X(3)    VALUE 'RO '.                 
012900     03 FILLER                   PIC X(3)    VALUE 'RUS'.                 
013000     03 FILLER                   PIC X(3)    VALUE 'SF '.                 
013100     03 FILLER                   PIC X(3)    VALUE 'T  '.                 
013200     03 FILLER                   PIC X(3)    VALUE 'TR '.                 
013300     03 FILLER                   PIC X(3)    VALUE 'USA'.                 
013400     03 FILLER                   PIC X(3)    VALUE 'YU '.                 
013500                                                                          
013600 01  TAB25 REDEFINES 25-TABELL.                                           
013700     03  FILLER OCCURS 25.                                                
013800        05  SHOW-IDSKYLT         PIC X(3).                                
013900                                                                          
014000                                                                          
014100 01  IDSKYLT-TABELL.                                                      
014200     03 FILLER   VALUE 'CZ '        PIC X(3).                             
014300     03 FILLER   VALUE 'D  '        PIC X(3).                             
014400     03 FILLER   VALUE 'DK '        PIC X(3).                             
014500     03 FILLER   VALUE 'E  '        PIC X(3).                             
014600     03 FILLER   VALUE 'F  '        PIC X(3).                             
014700     03 FILLER   VALUE 'GB '        PIC X(3).                             
014800     03 FILLER   VALUE 'GR '        PIC X(3).                             
014900     03 FILLER   VALUE 'H  '        PIC X(3).                             
015000     03 FILLER   VALUE 'I  '        PIC X(3).                             
015100     03 FILLER   VALUE 'IR '        PIC X(3).                             
015200     03 FILLER   VALUE 'J  '        PIC X(3).                             
015300     03 FILLER   VALUE 'KOR'        PIC X(3).                             
015400     03 FILLER   VALUE 'MAL'        PIC X(3).                             
015500     03 FILLER   VALUE 'NL '        PIC X(3).                             
015600     03 FILLER   VALUE 'P  '        PIC X(3).                             
015700     03 FILLER   VALUE 'PL '        PIC X(3).                             
015800     03 FILLER   VALUE 'RC '        PIC X(3).                             
015900     03 FILLER   VALUE 'RCN'        PIC X(3).                             
016000     03 FILLER   VALUE 'RO '        PIC X(3).                             
016100     03 FILLER   VALUE 'RUS'        PIC X(3).                             
016200     03 FILLER   VALUE 'S  '        PIC X(3).                             
016300     03 FILLER   VALUE 'SF '        PIC X(3).                             
016400     03 FILLER   VALUE 'T  '        PIC X(3).                             
016500     03 FILLER   VALUE 'TR '        PIC X(3).                             
016600     03 FILLER   VALUE 'USA'        PIC X(3).                             
016700     03 FILLER   VALUE 'YU '        PIC X(3).                             
016800 01  IDSKYLT-TAB REDEFINES IDSKYLT-TABELL.                                
016900     03  FILLER OCCURS 26.                                                
017000      05  WS-IDSKYLT                PIC X(3).                             
017100     SKIP3                                                                
017200                                                                          
017300     SKIP3                                                                
017400 01  TECKEN-KOLL.                                                         
017500     03  WS-MID-BEART-KOLL.                                               
017600         05 MID-BEART-TKN OCCURS 25 INDEXED BY TKN-IX PIC X.              
017700                                                                          
017800     03  RAETT-TECKEN               PIC X   VALUE 'J'.                    
017900         88 TECKEN-FEL                      VALUE 'N'.                    
018000         88 TECKEN-RAETT                    VALUE 'J'.                    
018100                                                                          
018200 01  DAGENS-DATUM                   PIC 9(6).                             
018300     EJECT                                                                
018400*01   -COPY WREVAREA                                                      
018500     EJECT                                                                
018600*01   -COPY WWPRODSL                                                      
018700     EJECT                                                                
018800*                   ****    PARAMETRAR TILL W005INIT                      
018900*01  -COPY WMSGINIT                                                       
019000     EJECT                                                                
019100 01  NYCKLAR-TILL-DLI.                                                    
019200     03  W-IDARTNR-X.                                                     
019300         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
019400                                                                          
019500     03  W-IDBENNR-X.                                                     
019600         05  W-IDBENNR            PIC S9(7) COMP-3.                       
019700                                                                          
019800     03  W-IDSEGMNR-X.                                                    
019900         05  W-IDSEGMNR           PIC S9 COMP-3 VALUE ZERO.               
020000                                                                          
020100     03  W-IDSKYLT-X.                                                     
020200         05  W-IDSKYLT            PIC X(3)  VALUE SPACE.                  
020300             88 TECHLA-SPRAK VALUES ARE                                   
020400                           'S  ', 'GB ', 'CZ ', 'D  ', 'DK ',             
020500                           'GR ', 'E  ', 'IR ', 'SF ', 'F  ',             
020600                           'F  ', 'H  ', 'I  ', 'J  ', 'KOR',             
020700                           'NL ', 'PL ', 'P  ', 'RO ', 'RUS',             
020800                           'YU ', 'TR ', 'USA', 'RCN', 'RC '.             
020900                                                                          
021000             88 UTF8-SPRAK VALUES ARE                                     
021100                           'CZ ', 'GR ', 'IR ', 'H  ', 'J  ',             
021200                           'KOR', 'PL ', 'RO ', 'RUS', 'YU ',             
021300                           'TR ', 'RCN', 'RC ', 'T  '.                    
021400                                                                          
021500     03  W-BEART-X.                                                       
021600         05  W-BEART              PIC X(25) VALUE SPACE.                  
021700                                                                          
021800     03  W-1207-KEY-X.                                                    
021900         05  FILLER               PIC X(4)  VALUE '1207'.                 
022000         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
022100     EJECT                                                                
022200 01  MEDDELANDE.                                                          
022300*                                                                         
022400     03  W-FEL-1.                                                         
022500         05  FILLER                  PIC X(32) VALUE                      
022600            'BENÄMNING SAKNAS                '.                           
022700         05  FILLER                  PIC X(32) VALUE                      
022800            'DESCRIPTION IS MISSING          '.                           
022900     03  FILLER REDEFINES W-FEL-1.                                        
023000         05  FEL-1                   PIC X(32) OCCURS 2.                  
023100                                                                          
023200     03  W-FEL-2.                                                         
023300         05  FILLER                  PIC X(32) VALUE                      
023400            'TECHLA BENÄMNING               '.                            
023500         05  FILLER                  PIC X(32) VALUE                      
023600            'TECHLA DESCRIPTION             '.                            
023700     03  FILLER REDEFINES W-FEL-2.                                        
023800         05  FEL-2                   PIC X(32) OCCURS 2.                  
023900                                                                          
024000     03  W-FEL-3.                                                         
024100         05  FILLER                  PIC X(32) VALUE                      
024200            'HOMONYMKOD EJ NUMERISK          '.                           
024300         05  FILLER                  PIC X(32) VALUE                      
024400            'HOM.CODE NOT NUMERIC            '.                           
024500     03  FILLER REDEFINES W-FEL-3.                                        
024600         05  FEL-3                   PIC X(32) OCCURS 2.                  
024700                                                                          
024800     03  W-FEL-4.                                                         
024900         05  FILLER                  PIC X(32) VALUE                      
025000            'FELAKTIGA TECKEN I BENÄMNING    '.                           
025100         05  FILLER                  PIC X(32) VALUE                      
025200            'WRONG CHARACTERS IN DESCRIPTION '.                           
025300     03  FILLER REDEFINES W-FEL-4.                                        
025400         05  FEL-4                   PIC X(32) OCCURS 2.                  
025500                                                                          
025600     03  W-FEL-5.                                                         
025700         05  FILLER                  PIC X(32) VALUE                      
025800            'ENGELSK BENÄMNING MÅSTE FYLLAS I'.                           
025900         05  FILLER                  PIC X(32) VALUE                      
026000            'ENGLISH DES. MUST BE FILLED IN  '.                           
026100     03  FILLER REDEFINES W-FEL-5.                                        
026200         05  FEL-5                   PIC X(32) OCCURS 2.                  
026300                                                                          
026400     03  W-FEL-6.                                                         
026500         05  FILLER                  PIC X(32) VALUE                      
026600            'HOMONYMKODEN FINNS REDAN        '.                           
026700         05  FILLER                  PIC X(32) VALUE                      
026800            'THE HOM.CODE ALREADY EXISTS     '.                           
026900     03  FILLER REDEFINES W-FEL-6.                                        
027000         05  FEL-6                   PIC X(32) OCCURS 2.                  
027100                                                                          
027200     03  W-MED-1.                                                         
027300         05  FILLER                  PIC X(32) VALUE                      
027400            'UPPDATERING GJORD               '.                           
027500         05  FILLER                  PIC X(32) VALUE                      
027600            'UPDATED                         '.                           
027700     03  FILLER REDEFINES W-MED-1.                                        
027800         05  MED-1                   PIC X(32) OCCURS 2.                  
027900                                                                          
028000     03  W-MED-2.                                                         
028100         05  FILLER                  PIC X(25) VALUE                      
028200            'ÖVERSÄTTNING FINNS REG.  '.                                  
028300         05  FILLER                  PIC X(25) VALUE                      
028400            'TRANSLATION IS REGISTERED'.                                  
028500     03  FILLER REDEFINES W-MED-2.                                        
028600         05  MED-2                   PIC X(25) OCCURS 2.                  
028700     EJECT                                                                
028800*                        ****    MFS OCH SKÄRMHANTERING                   
028900 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
029000     SKIP2                                                                
029100*01  MID -COPY W1I12201                                                   
029200     EJECT                                                                
029300*01  -COPY WMSGAREA                                                       
029400     EJECT                                                                
029500*    03  MOD -COPY W1O12201  -RED MSG-AREA.                               
029600     EJECT                                                                
029700*01  -COPY WMFSAREA.                                                      
029800     EJECT                                                                
029900******************************************************************        
030000*****                                                                     
030100*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030200*****                                                                     
030300 01  IMS-WS.                                                              
030400     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
030500     SKIP3                                                                
030600*****                    **** STATUS-KOD FRÅN IMS                         
030700     03  STATUS-WS               PIC X(2).                                
030800         88  SEGMENT-FINNS                 VALUE '  '.                    
030900         88  SEGMENT-SAKNAS                VALUE 'GE', 'GB'.              
031000     SKIP3                                                                
031100     03  GODK-STATUSKODER.                                                
031200         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
031300     SKIP3                                                                
031400 01  SSA1                        PIC X(64).                               
031500 01  SSA2                        PIC X(64).                               
031600     EJECT                                                                
031700*                            IMS FUNKTIONSKODER                           
031800*01  -COPY W0003                                                          
031900     EJECT                                                                
032000*                            DLI INPUT-OUTPUT AREA                        
032100 01  DLI-IO-AREA-1.                                                       
032200     03  IO-AREA-1               PIC X(200)  VALUE SPACE.                 
032300     SKIP3                                                                
032400*    03  WLBENA  -COPY WDD301   -PRE BENAA-  -RED IO-AREA-1.              
032500     EJECT                                                                
032600 01  DLI-IO-AREA-B.                                                       
032700     03  IO-AREA-B               PIC X(200)  VALUE SPACE.                 
032800     SKIP3                                                                
032900*    03  WLBENA  -COPY WDD301   -PRE BENA-  -RED IO-AREA-B.               
033000     EJECT                                                                
033100 01  DLI-IO-AREA-2.                                                       
033200     03  IO-AREA-2               PIC X(200)  VALUE SPACE.                 
033300*    03  WLBENA  -COPY WDD311   -PRE BENA-  -RED IO-AREA-2.               
033400     EJECT                                                                
033500*    03  WLBENA  -COPY WDD312   -PRE BENA-  -RED IO-AREA-2.               
033600     EJECT                                                                
033700*    03  WLBENA  -COPY WDD313   -PRE BENA-  -RED IO-AREA-2.               
033800     EJECT                                                                
033900 01  DLI-IO-AREA-3.                                                       
034000     03  IO-AREA-3               PIC X(110)  VALUE SPACE.                 
034100*    03  WDK6    -COPY WDK601   -PRE WDK6-  -RED IO-AREA-3.               
034200     EJECT                                                                
034300 01  FILLER         PIC X(24) VALUE 'DLI-IO-XXAI01'.                      
034400 01  DLI-IO-XXAI01.                                                       
034500*    03  -COPY WDGX01                                                     
034600     EJECT                                                                
034700                                                                          
034800 01  FILLER         PIC X(24) VALUE 'DLI-IO-XXAI11'.                      
034900 01  DLI-IO-XXAI11.                                                       
035000*    03  -COPY WDGX1208                                                   
035100     EJECT                                                                
035200                                                                          
035300 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
035400 01  DLI-IO-BENA01.                                                       
035500*    03  -COPY WDD301                                                     
035600     EJECT                                                                
035700                                                                          
035800 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
035900 01  DLI-IO-BENA11.                                                       
036000*    03  -COPY WDD311                                                     
036100     EJECT                                                                
036200                                                                          
036300 LINKAGE SECTION.                                                         
036400*01  -COPY W0009     -PRE MSG-                                            
036500                                                                          
036600*01  -COPY W0008     -PRE BENAA-                                          
036700         05  FILLER              PIC X.                                   
036800*01  -COPY W0008     -PRE BENA-                                           
036900         05  FILLER              PIC X.                                   
037000                                                                          
037100*01  -COPY W0008     -PRE USEA-                                           
037200         05  FILLER              PIC X.                                   
037300     EJECT                                                                
037400*01  -COPY W0008     -PRE WDK6-                                           
037500         05  FILLER              PIC X.                                   
037600     EJECT                                                                
037700*01  -COPY W0008     -PRE XXAI-                                           
037800         05  FILLER              PIC X.                                   
037900     EJECT                                                                
038000                                                                          
038100 PROCEDURE DIVISION USING MSG-PCB BENAA-PCB BENA-PCB USEA-PCB             
038200                                  WDK6-PCB XXAI-PCB.                      
038300 MAIN SECTION.                                                            
038400     ENTRY 'DLITCBL' USING MSG-PCB BENAA-PCB BENA-PCB USEA-PCB            
038500                                  WDK6-PCB XXAI-PCB.                      
038600     SKIP2                                                                
038700     PERFORM IMS-GET-MSG                                                  
038800     IF SEGMENT-FINNS                                                     
038900       PERFORM A-INIT-SPARA-INPUT                                         
039000       IF WS-NYCKLAR-RAETT = JA                                           
039100         IF MFS-UPDATE                                                    
039200           PERFORM D-VISA-BILD-IGEN                                       
039300           IF MID-SKAPA-BEN = 'J'                                         
039400             PERFORM BB-REG-NY-BEN                                        
039500           ELSE                                                           
039600             PERFORM B-UPPDATERA                                          
039700           END-IF                                                         
039800         ELSE                                                             
039900           PERFORM C-LAS-BASEN                                            
040000         END-IF                                                           
040100       ELSE                                                               
040200         MOVE FEL-3(IX) TO MOD-TEMFSFEL                                   
040300       END-IF                                                             
040400       MOVE WS-BEART TO MOD-BEART-UT                                      
040500       MOVE WS-KDHOMONYM TO MOD-KDHOMONYM-UT                              
040600       ADD LENGTH OF MOD-W1O12201 +4 GIVING MSG-KVLL                      
040700       PERFORM IMS-INSERT-MSG                                             
040800     END-IF                                                               
040900     MOVE ZERO TO RETURN-CODE                                             
041000     GOBACK                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 A-INIT-SPARA-INPUT SECTION.                                              
041400     SKIP2                                                                
041500     IF MSG-DUBBLA-TRANSKODER                                             
041600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I12201                 
041700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
041800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
041900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
042000     ELSE                                                                 
042100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I12201                  
042200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
042300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
042400       MOVE ' ' TO MFS-KDTRTYP                                            
042500     END-IF                                                               
042600     INSPECT MID-W1I12201 REPLACING ALL '<' BY SPACE                      
042700     INSPECT MID-W1I12201 REPLACING ALL '>' BY SPACE                      
042800                                                                          
042900     MOVE JA TO WS-NYCKLAR-RAETT                                          
043000                                                                          
043100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
043200     MOVE '001'             TO MSGI-KDCALL                                
043300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
043400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
043500     MOVE '1122'            TO MSGI-IDTRANS                               
043600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043700                                                                          
043800     IF MID-KDHOMONYM-IN = ALL '+'                                        
043900       MOVE MID-KDHOMONYM-UT TO WS-KDHOMONYM                              
044000     ELSE                                                                 
044100       MOVE MID-KDHOMONYM-IN TO WS-KDHOMONYM                              
044200       MOVE SPACE TO MFS-KDTRTYP                                          
044300     END-IF                                                               
044400     IF MID-BEART-IN = ALL '+' OR SPACE                                   
044500       MOVE MID-BEART-UT TO WS-BEART                                      
044600     ELSE                                                                 
044700       MOVE MID-BEART-IN TO WS-BEART                                      
044800       MOVE SPACE TO MFS-KDTRTYP                                          
044900     END-IF                                                               
045000                                                                          
045100     IF MFS-IDTRANS = '1122'                                              
045200       CONTINUE                                                           
045300     ELSE                                                                 
045400       MOVE SPACE TO MFS-KDTRTYP                                          
045500       MOVE ZERO TO WS-KDHOMONYM                                          
045600     END-IF                                                               
045700     IF WS-KDHOMONYM NUMERIC                                              
045800       CONTINUE                                                           
045900     ELSE                                                                 
046000       MOVE NEJ TO WS-NYCKLAR-RAETT                                       
046100     END-IF                                                               
046200     ACCEPT DAGENS-DATUM FROM DATE                                        
046300                                                                          
046400     IF MSGI-IDLAND-SPR = 'SE'                                            
046500       MOVE +1 TO IX                                                      
046600       MOVE 'W1O12201' TO MFS-IDMOD                                       
046700     ELSE                                                                 
046800       MOVE +2 TO IX                                                      
046900       MOVE 'W1O122N1' TO MFS-IDMOD                                       
047000     END-IF                                                               
047100     MOVE LOW-VALUE TO MOD-W1O12201                                       
047200     MOVE '1122' TO MOD-IDTRANS                                           
047300                                                                          
047400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
047500                             MOD-TEMFSINF                                 
047600                             MOD-BEART-IN                                 
047700                             MOD-KDHOMONYM-IN                             
047800     .                                                                    
047900     EJECT                                                                
048000 B-UPPDATERA SECTION.                                                     
048100     SKIP2                                                                
048200     MOVE NEJ TO WS-FLAENDR                                               
048300                 NEVIS-ART                                                
048400     MOVE SPACE TO SPARAD-GB                                              
048500                   SPARAD-FLOVERSATT                                      
048600                                                                          
048700     MOVE WS-BEART TO W-BEART                                             
048800     MOVE SVENSKA TO W-IDSKYLT                                            
048900*    BENAA-PCB + IO-AREA-1                                                
049000     PERFORM IMS-GU-BENA01-ASEQ                                           
049100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
049200                   BENAA-BEN-KDHOMONYM = KDHOMONYM-WS                     
049300*      BENAA-PCB + IO-AREA-1                                              
049400       PERFORM IMS-GN-BENA01-ASEQ                                         
049500     END-PERFORM                                                          
049600                                                                          
049700                                                                          
049800                                                                          
049900     IF SEGMENT-FINNS                                                     
050000       MOVE BENAA-BEN-IDBENNR  TO W-IDBENNR                               
050100                                  MOD-IDBENNR                             
050200*                              BENA-PCB + IO-AREA-B                       
050300       PERFORM IMS-GU-BENA01                                              
050400                                                                          
050500       MOVE +1 TO RAD-IX                                                  
050600*      --- DET FINNS 11 RADER I MID FÖR UPPDATERING AV BEART              
050700       PERFORM UNTIL RAD-IX >= MAX-UPD-PLUS-1                             
050800         IF MID-RAD(RAD-IX) NOT = ALL '+'                                 
050900           IF MID-RAD(RAD-IX) NOT = SPACE                                 
051000             MOVE UPD-IDSKYLT(RAD-IX) TO W-IDSKYLT                        
051100                                                                          
051200*                                     BENA-PCB + IO-AREA-2                
051300             PERFORM IMS-GHNP-BENA11-FIRST                                
051400*                                                                         
051500             IF SEGMENT-SAKNAS                                            
051600               MOVE W-IDSKYLT         TO BENA-TEXT-IDSKYLT                
051700               MOVE 'N'               TO BENA-TEXT-FLOVERSATT             
051800             END-IF                                                       
051900             MOVE MID-BEART(RAD-IX) TO MOD-BEART(RAD-IX)                  
052000                                       BENA-TEXT-BEART                    
052100                                       WS-MID-BEART-KOLL                  
052200             SET TECKEN-RAETT TO TRUE                                     
052300             IF W-IDSKYLT = 'MAL'                                         
052400**             --- KOLLA 'MAL' FÖR FELAKTIGA TECKEN                       
052500               SET TKN-IX TO 1                                            
052600               SEARCH MID-BEART-TKN AT END CONTINUE                       
052700                 WHEN MID-BEART-TKN(TKN-IX) = 'Å' OR 'Ä' OR 'Ö'           
052800                      SET TECKEN-FEL TO TRUE                              
052900                      MOVE FEL-4(IX) TO MOD-TEMFSFEL                      
053000               END-SEARCH                                                 
053100             END-IF                                                       
053200                                                                          
053300             IF TECKEN-RAETT                                              
053400               MOVE DAGENS-DATUM     TO BENA-TEXT-TIUPPDAT                
053500                                                                          
053600*              IF BENA-BEN-KDBENSTAT = 0                                  
053700*              AND TECHLA-SPRAK                                           
053800*                --- Får ej uppdateras här                                
053900*                CONTINUE                                                 
054000*              ELSE                                                       
054100                 MOVE JA TO BENA-TEXT-FLOVERSATT                          
054200*              END-IF                                                     
054300                                                                          
054400               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
054500                                       MOD-BEART-ATTR(RAD-IX)             
054600*                               BENA-PCB + IO-AREA-2                      
054700               PERFORM IMS-REPL-BENA-2                                    
054800               MOVE JA    TO WS-FLAENDR                                   
054900               PERFORM BA-SPARA-TEXT                                      
055000               MOVE MED-1(IX) TO MOD-TEMFSINF                             
055100             END-IF                                                       
055200           END-IF                                                         
055300         END-IF                                                           
055400         ADD +1 TO RAD-IX                                                 
055500       END-PERFORM                                                        
055600                                                                          
055700       IF SPARAD-GB NOT = SPACE                                           
055800         MOVE ' BG' TO W-IDSKYLT                                          
055900*                                 BENA-PCB + IO-AREA-2                    
056000         PERFORM IMS-GHNP-BENA11-FIRST                                    
056100                                                                          
056200         MOVE SPACE TO REV-TETEXT                                         
056300         MOVE SPARAD-GB TO REV-TETEXT                                     
056400         CALL WREVERSE USING REV-TETEXT                                   
056500                                                                          
056600         MOVE REV-TETEXT           TO BENA-TEXT-BEART                     
056700         MOVE DAGENS-DATUM         TO BENA-TEXT-TIUPPDAT                  
056800         MOVE SPARAD-FLOVERSATT    TO BENA-TEXT-FLOVERSATT                
056900                                                                          
057000*                                  BENA-PCB + IO-AREA-2                   
057100         PERFORM IMS-REPL-BENA-2                                          
057200*        -- FLAENDR ÄR I DETTA FALL REDAN SATT I B-                       
057300*        MOVE JA TO WS-FLAENDR                                            
057400*        -- fyll på med engelskan i tomma benämningar                     
057500*        -- som inte är ett UTF8-språk.                                   
057600         MOVE +1 TO RAD-IX                                                
057700         PERFORM UNTIL RAD-IX >= MAX-UPD-PLUS-1                           
057800           MOVE UPD-IDSKYLT(RAD-IX) TO W-IDSKYLT                          
057900                                                                          
058000*                                  BENA-PCB + IO-AREA-2                   
058100           PERFORM IMS-GHNP-BENA11-FIRST                                  
058200           IF  ( BENA-TEXT-FLOVERSATT = NEJ )                             
058300           AND ( TECHLA-SPRAK OR W-IDSKYLT = 'MAL' )                      
058400           AND ( NOT UTF8-SPRAK )                                         
058500               MOVE SPARAD-GB  TO BENA-TEXT-BEART                         
058600                                  MOD-BEART(RAD-IX)                       
058700               MOVE DAGENS-DATUM TO BENA-TEXT-TIUPPDAT                    
058800*                                  BENA-PCB + IO-AREA-2                   
058900               PERFORM IMS-REPL-BENA-2                                    
059000               MOVE JA TO WS-FLAENDR                                      
059100           END-IF                                                         
059200           ADD +1 TO RAD-IX                                               
059300         END-PERFORM                                                      
059400       END-IF                                                             
059500     END-IF                                                               
059600                                                                          
059700*    BENA-PCB + IO-AREA-B                                                 
059800     PERFORM IMS-GU-BENA01                                                
059900     IF SEGMENT-FINNS                                                     
060000*      BENA-PCB + IO-AREA-2                                               
060100       PERFORM IMS-GNP-BENA12                                             
060200       PERFORM UNTIL SEGMENT-SAKNAS                                       
060300               OR NEVIS-ART = JA                                          
060400         MOVE BENA-ART-IDARTNR TO W-IDARTNR                               
060500         PERFORM IMS-GU-WDK601                                            
060600         MOVE WDK6-ART-KDPRODSL TO TEST-KDPRODSL                          
060700         IF KDPRODSL-VOLVO-ALL                                            
060800           MOVE JA TO NEVIS-ART                                           
060900         END-IF                                                           
061000         PERFORM IMS-GNP-BENA12                                           
061100       END-PERFORM                                                        
061200                                                                          
061300       IF WS-FLAENDR = JA                                                 
061400         IF NEVIS-ART = JA                                                
061500*                                  BENA-PCB + IO-AREA-B                   
061600           PERFORM IMS-GHU-BENA01                                         
061700           MOVE JA TO BENA-BEN-FLAENDR                                    
061800*                                  BENA-PCB + IO-AREA-B                   
061900           PERFORM IMS-REPL-BENA-B                                        
062000         END-IF                                                           
062100       END-IF                                                             
062200     END-IF                                                               
062300     IF MID-TEHOMONYM(1) = ALL '+'                                        
062400       CONTINUE                                                           
062500     ELSE                                                                 
062600       PERFORM IMS-GHU-BENA01                                             
062700       IF SEGMENT-FINNS                                                   
062800         MOVE +1   TO W-IDSEGMNR                                          
062900         PERFORM IMS-GHNP-BENA13                                          
063000         IF SEGMENT-FINNS                                                 
063100           MOVE MID-TEHOMONYM(1)  TO BENA-HOM-TEHOMONYM                   
063200           PERFORM IMS-REPL-BENA13                                        
063300         ELSE                                                             
063400           MOVE +1                TO BENA-HOM-IDSEGMNR                    
063500           MOVE MID-TEHOMONYM(1)  TO BENA-HOM-TEHOMONYM                   
063600           PERFORM IMS-ISRT-BENA13                                        
063700         END-IF                                                           
063800       END-IF                                                             
063900     END-IF                                                               
064000     IF MID-TEHOMONYM(2) = ALL '+'                                        
064100       CONTINUE                                                           
064200     ELSE                                                                 
064300       PERFORM IMS-GHU-BENA01                                             
064400       IF SEGMENT-FINNS                                                   
064500         MOVE +2   TO W-IDSEGMNR                                          
064600         PERFORM IMS-GHNP-BENA13                                          
064700         IF SEGMENT-FINNS                                                 
064800           MOVE MID-TEHOMONYM(2)  TO BENA-HOM-TEHOMONYM                   
064900           PERFORM IMS-REPL-BENA13                                        
065000         ELSE                                                             
065100           MOVE +2                TO BENA-HOM-IDSEGMNR                    
065200           MOVE MID-TEHOMONYM(2)  TO BENA-HOM-TEHOMONYM                   
065300           PERFORM IMS-ISRT-BENA13                                        
065400         END-IF                                                           
065500       END-IF                                                             
065600     END-IF                                                               
065700     .                                                                    
065800     EJECT                                                                
065900 BA-SPARA-TEXT SECTION.                                                   
066000                                                                          
066100     IF W-IDSKYLT = 'GB '                                                 
066200       MOVE MID-BEART(RAD-IX)    TO SPARAD-GB                             
066300       MOVE BENA-TEXT-FLOVERSATT TO SPARAD-FLOVERSATT                     
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 BB-REG-NY-BEN SECTION.                                                   
066900                                                                          
067000     MOVE JA  TO BEHANDLA-SW                                              
067100     IF MID-BEART-UT = SPACE                                              
067200       MOVE FEL-1(IX) TO MOD-TEMFSFEL                                     
067300     ELSE                                                                 
067400****                                                                      
067500       MOVE MID-BEART-UT TO W-BEART                                       
067600       MOVE SVENSKA TO W-IDSKYLT                                          
067700       PERFORM IMS-GU-BENA01-ASEQ                                         
067800       PERFORM UNTIL SEGMENT-SAKNAS OR BEHANDLA-EJ                        
067900         IF BENAA-BEN-KDHOMONYM = KDHOMONYM-WS                            
068000           MOVE NEJ TO    BEHANDLA-SW                                     
068100         END-IF                                                           
068200         PERFORM IMS-GN-BENA01-ASEQ                                       
068300       END-PERFORM                                                        
068400****                                                                      
068500       IF BEHANDLA                                                        
068600         PERFORM IMS-GET-XXAI01                                           
068700         PERFORM IMS-GET-XXAI11                                           
068800         ADD +1 TO 1208-IDBENNR                                           
068900         IF 1208-IDBENNR > 1208-IDBENNR-MAX                               
069000           MOVE +1 TO 1208-IDBENNR                                        
069100         END-IF                                                           
069200                                                                          
069300         MOVE 1208-IDBENNR TO W-IDBENNR                                   
069400                              1208-IDBENNR                                
069500                              MOD-IDBENNR                                 
069600         PERFORM IMS-REPL-XXAI11                                          
069700                                                                          
069800         MOVE 1208-IDBENNR  TO BEN-IDBENNR                                
069900         MOVE MID-KDHOMONYM-UT TO BEN-KDHOMONYM                           
070000                                 MOD-KDHOMONYM-UT                         
070100         MOVE DAGENS-DATUM  TO BEN-TIUPPDAT-STOP                          
070200         MOVE +0            TO BEN-KDBENSTAT                              
070300*        IF NEVIS-ARTIKEL                                                 
070400           MOVE JA          TO BEN-FLAENDR                                
070500*        END-IF                                                           
070600         PERFORM IMS-ISRT-BENA01                                          
070700                                                                          
070800         MOVE +1 TO IDSKYLT-IX                                            
070900*          --- ALLA GODK WDD3-SPRÅK SKALL HA ETT 11-SEGMENT               
071000         PERFORM UNTIL IDSKYLT-IX > MAX-IDSKYLT                           
071100           MOVE WS-IDSKYLT(IDSKYLT-IX) TO TEXT-IDSKYLT                    
071200           MOVE SPACE              TO TEXT-BEARTEXT                       
071300           MOVE DAGENS-DATUM       TO TEXT-TIUPPDAT                       
071400           IF WS-IDSKYLT(IDSKYLT-IX) = 'S  '                              
071500             MOVE JA              TO TEXT-FLOVERSATT                      
071600             MOVE MID-BEART-UT    TO TEXT-BEARTEXT                        
071700           ELSE                                                           
071800             MOVE NEJ             TO TEXT-FLOVERSATT                      
071900           END-IF                                                         
072000           PERFORM IMS-ISRT-BENA11                                        
072100*          PERFORM S01-FLYTTA-BEART                                       
072200           ADD +1 TO IDSKYLT-IX                                           
072300         END-PERFORM                                                      
072400                                                                          
072500         MOVE SPACE TO REV-TETEXT                                         
072600         MOVE WS-BEART TO REV-TETEXT                                      
072700         CALL WREVERSE USING REV-TETEXT                                   
072800                                                                          
072900         MOVE ' BG'      TO TEXT-IDSKYLT                                  
073000         MOVE SPACE      TO TEXT-BEARTEXT                                 
073100         MOVE DAGENS-DATUM TO TEXT-TIUPPDAT                               
073200         MOVE NEJ        TO TEXT-FLOVERSATT                               
073300         PERFORM IMS-ISRT-BENA11                                          
073400                                                                          
073500         MOVE '  S'      TO TEXT-IDSKYLT                                  
073600         MOVE REV-TETEXT TO TEXT-BEARTEXT                                 
073700         MOVE DAGENS-DATUM TO TEXT-TIUPPDAT                               
073800         MOVE JA         TO TEXT-FLOVERSATT                               
073900         PERFORM IMS-ISRT-BENA11                                          
074000       ELSE                                                               
074100         MOVE FEL-6(IX) TO MOD-TEMFSFEL                                   
074200       END-IF                                                             
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600                                                                          
074700 C-LAS-BASEN SECTION.                                                     
074800     SKIP2                                                                
074900     MOVE WS-BEART TO W-BEART                                             
075000     MOVE SVENSKA TO W-IDSKYLT                                            
075100                                                                          
075200*                                BENAA-PCB + IO-AREA-1                    
075300     PERFORM IMS-GU-BENA01-ASEQ                                           
075400     PERFORM UNTIL  SEGMENT-SAKNAS OR                                     
075500                    BENAA-BEN-KDHOMONYM = KDHOMONYM-WS                    
075600*                                BENAA-PCB + IO-AREA-1                    
075700       PERFORM IMS-GN-BENA01-ASEQ                                         
075800     END-PERFORM                                                          
075900                                                                          
076000     IF SEGMENT-FINNS                                                     
076100       MOVE BENAA-BEN-IDBENNR  TO W-IDBENNR                               
076200                                  MOD-IDBENNR                             
076300       PERFORM CA-RENSA-BENAMNINGSFALT                                    
076400                                                                          
076500*                                BENA-PCB + IO-AREA-B                     
076600       PERFORM IMS-GU-BENA01                                              
076700       MOVE +1 TO RAD-IX                                                  
076800       PERFORM UNTIL  RAD-IX >= MAX-SPRAK-PLUS-1                          
076900                                                                          
077000         MOVE SHOW-IDSKYLT(RAD-IX) TO W-IDSKYLT                           
077100*                                BENA-PCB + IO-AREA-2                     
077200         PERFORM IMS-GNP-BENA11-FIRST                                     
077300         IF SEGMENT-FINNS                                                 
077400           EVALUATE W-IDSKYLT                                             
077500             WHEN 'GB '                                                   
077600               IF BENA-TEXT-BEART NOT = SPACE                             
077700                 MOVE BENA-TEXT-BEART TO MOD-BEART(1)                     
077800               END-IF                                                     
077900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
078000                 MOVE 'Y'                  TO MOD-FLOVERSATT(1)           
078100               ELSE                                                       
078200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(1)           
078300               END-IF                                                     
078400                                                                          
078500             WHEN 'USA'                                                   
078600               IF BENA-TEXT-BEART NOT = SPACE                             
078700                 MOVE BENA-TEXT-BEART TO MOD-BEART(2)                     
078800               END-IF                                                     
078900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
079000                 MOVE 'Y'                  TO MOD-FLOVERSATT(2)           
079100               ELSE                                                       
079200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(2)           
079300               END-IF                                                     
079400                                                                          
079500             WHEN 'D  '                                                   
079600               IF BENA-TEXT-BEART NOT = SPACE                             
079700                 MOVE BENA-TEXT-BEART TO MOD-BEART(3)                     
079800               END-IF                                                     
079900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
080000                 MOVE 'Y'                  TO MOD-FLOVERSATT(3)           
080100               ELSE                                                       
080200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(3)           
080300               END-IF                                                     
080400                                                                          
080500             WHEN 'DK '                                                   
080600               IF BENA-TEXT-BEART NOT = SPACE                             
080700                 MOVE BENA-TEXT-BEART TO MOD-BEART(4)                     
080800               END-IF                                                     
080900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
081000                 MOVE 'Y'                  TO MOD-FLOVERSATT(4)           
081100               ELSE                                                       
081200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(4)           
081300               END-IF                                                     
081400                                                                          
081500             WHEN 'F  '                                                   
081600               IF BENA-TEXT-BEART NOT = SPACE                             
081700                 MOVE BENA-TEXT-BEART TO MOD-BEART(5)                     
081800               END-IF                                                     
081900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
082000                 MOVE 'Y'                  TO MOD-FLOVERSATT(5)           
082100               ELSE                                                       
082200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(5)           
082300               END-IF                                                     
082400                                                                          
082500             WHEN 'E  '                                                   
082600               IF BENA-TEXT-BEART NOT = SPACE                             
082700                 MOVE BENA-TEXT-BEART TO MOD-BEART(6)                     
082800               END-IF                                                     
082900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
083000                 MOVE 'Y'                  TO MOD-FLOVERSATT(6)           
083100               ELSE                                                       
083200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(6)           
083300               END-IF                                                     
083400                                                                          
083500             WHEN 'P  '                                                   
083600               IF BENA-TEXT-BEART NOT = SPACE                             
083700                 MOVE BENA-TEXT-BEART TO MOD-BEART(7)                     
083800               END-IF                                                     
083900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
084000                 MOVE 'Y'                  TO MOD-FLOVERSATT(7)           
084100               ELSE                                                       
084200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(7)           
084300               END-IF                                                     
084400                                                                          
084500             WHEN 'MAL'                                                   
084600               IF BENA-TEXT-BEART NOT = SPACE                             
084700                 MOVE BENA-TEXT-BEART TO MOD-BEART(8)                     
084800               END-IF                                                     
084900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
085000                 MOVE 'Y'                  TO MOD-FLOVERSATT(8)           
085100               ELSE                                                       
085200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(8)           
085300               END-IF                                                     
085400                                                                          
085500             WHEN 'NL '                                                   
085600               IF BENA-TEXT-BEART NOT = SPACE                             
085700                 MOVE BENA-TEXT-BEART TO MOD-BEART(9)                     
085800               END-IF                                                     
085900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
086000                 MOVE 'Y'                  TO MOD-FLOVERSATT(9)           
086100               ELSE                                                       
086200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(9)           
086300               END-IF                                                     
086400                                                                          
086500             WHEN 'I  '                                                   
086600               IF BENA-TEXT-BEART NOT = SPACE                             
086700                 MOVE BENA-TEXT-BEART TO MOD-BEART(10)                    
086800               END-IF                                                     
086900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
087000                 MOVE 'Y'                  TO MOD-FLOVERSATT(10)          
087100               ELSE                                                       
087200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(10)          
087300               END-IF                                                     
087400                                                                          
087500             WHEN 'SF '                                                   
087600               IF BENA-TEXT-BEART NOT = SPACE                             
087700                 MOVE BENA-TEXT-BEART TO MOD-BEART(11)                    
087800               END-IF                                                     
087900               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
088000                 MOVE 'Y'                  TO MOD-FLOVERSATT(11)          
088100               ELSE                                                       
088200                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT(11)          
088300               END-IF                                                     
088400                                                                          
088500                                                                          
088600             WHEN 'CZ '                                                   
088700               IF BENA-TEXT-BEART NOT = SPACE                             
088800                 MOVE REGISTRERAD TO MOD-BEART-CZ-FINNS                   
088900               END-IF                                                     
089000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
089100                 MOVE 'Y'                  TO MOD-FLOVERSATT-CZ           
089200               ELSE                                                       
089300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-CZ           
089400               END-IF                                                     
089500                                                                          
089600             WHEN 'GR '                                                   
089700               IF BENA-TEXT-BEART NOT = SPACE                             
089800                 MOVE REGISTRERAD TO MOD-BEART-GR-FINNS                   
089900               END-IF                                                     
090000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
090100                 MOVE 'Y'                  TO MOD-FLOVERSATT-GR           
090200               ELSE                                                       
090300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-GR           
090400               END-IF                                                     
090500                                                                          
090600             WHEN 'H  '                                                   
090700               IF BENA-TEXT-BEART NOT = SPACE                             
090800                 MOVE REGISTRERAD TO MOD-BEART-H-FINNS                    
090900               END-IF                                                     
091000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
091100                 MOVE 'Y'                  TO MOD-FLOVERSATT-H            
091200               ELSE                                                       
091300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-H            
091400               END-IF                                                     
091500                                                                          
091600             WHEN 'IR '                                                   
091700               IF BENA-TEXT-BEART NOT = SPACE                             
091800                 MOVE REGISTRERAD TO MOD-BEART-IR-FINNS                   
091900               END-IF                                                     
092000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
092100                 MOVE 'Y'                  TO MOD-FLOVERSATT-IR           
092200               ELSE                                                       
092300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-IR           
092400               END-IF                                                     
092500                                                                          
092600             WHEN 'J  '                                                   
092700               IF BENA-TEXT-BEART NOT = SPACE                             
092800                 MOVE REGISTRERAD TO MOD-BEART-J-FINNS                    
092900               END-IF                                                     
093000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
093100                 MOVE 'Y'                  TO MOD-FLOVERSATT-J            
093200               ELSE                                                       
093300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-J            
093400               END-IF                                                     
093500                                                                          
093600             WHEN 'KOR'                                                   
093700               IF BENA-TEXT-BEART NOT = SPACE                             
093800                 MOVE REGISTRERAD TO MOD-BEART-KOR-FINNS                  
093900               END-IF                                                     
094000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
094100                 MOVE 'Y'                  TO MOD-FLOVERSATT-KOR          
094200               ELSE                                                       
094300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-KOR          
094400               END-IF                                                     
094500                                                                          
094600             WHEN 'PL '                                                   
094700               IF BENA-TEXT-BEART NOT = SPACE                             
094800                 MOVE REGISTRERAD TO MOD-BEART-PL-FINNS                   
094900               END-IF                                                     
095000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
095100                 MOVE 'Y'                  TO MOD-FLOVERSATT-PL           
095200               ELSE                                                       
095300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-PL           
095400               END-IF                                                     
095500                                                                          
095600             WHEN 'RC '                                                   
095700               IF BENA-TEXT-BEART NOT = SPACE                             
095800                 MOVE REGISTRERAD TO MOD-BEART-RC-FINNS                   
095900               END-IF                                                     
096000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
096100                 MOVE 'Y'                  TO MOD-FLOVERSATT-RC           
096200               ELSE                                                       
096300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-RC           
096400               END-IF                                                     
096500                                                                          
096600             WHEN 'RCN'                                                   
096700               IF BENA-TEXT-BEART NOT = SPACE                             
096800                 MOVE REGISTRERAD TO MOD-BEART-RCN-FINNS                  
096900               END-IF                                                     
097000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
097100                 MOVE 'Y'                  TO MOD-FLOVERSATT-RCN          
097200               ELSE                                                       
097300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-RCN          
097400               END-IF                                                     
097500                                                                          
097600             WHEN 'RO '                                                   
097700               IF BENA-TEXT-BEART NOT = SPACE                             
097800                 MOVE REGISTRERAD TO MOD-BEART-RO-FINNS                   
097900               END-IF                                                     
098000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
098100                 MOVE 'Y'                  TO MOD-FLOVERSATT-RO           
098200               ELSE                                                       
098300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-RO           
098400               END-IF                                                     
098500                                                                          
098600             WHEN 'RUS'                                                   
098700               IF BENA-TEXT-BEART NOT = SPACE                             
098800                 MOVE REGISTRERAD TO MOD-BEART-RUS-FINNS                  
098900               END-IF                                                     
099000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
099100                 MOVE 'Y'                  TO MOD-FLOVERSATT-RUS          
099200               ELSE                                                       
099300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-RUS          
099400               END-IF                                                     
099500                                                                          
099600             WHEN 'T  '                                                   
099700               IF BENA-TEXT-BEART NOT = SPACE                             
099800                 MOVE REGISTRERAD TO MOD-BEART-T-FINNS                    
099900               END-IF                                                     
100000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
100100                 MOVE 'Y'                  TO MOD-FLOVERSATT-T            
100200               ELSE                                                       
100300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-T            
100400               END-IF                                                     
100500                                                                          
100600             WHEN 'TR '                                                   
100700               IF BENA-TEXT-BEART NOT = SPACE                             
100800                 MOVE REGISTRERAD TO MOD-BEART-TR-FINNS                   
100900               END-IF                                                     
101000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
101100                 MOVE 'Y'                  TO MOD-FLOVERSATT-TR           
101200               ELSE                                                       
101300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-TR           
101400               END-IF                                                     
101500                                                                          
101600             WHEN 'YU '                                                   
101700               IF BENA-TEXT-BEART NOT = SPACE                             
101800                 MOVE REGISTRERAD TO MOD-BEART-YU-FINNS                   
101900               END-IF                                                     
102000               IF BENA-TEXT-FLOVERSATT = 'J' AND IX = +2                  
102100                 MOVE 'Y'                  TO MOD-FLOVERSATT-YU           
102200               ELSE                                                       
102300                 MOVE BENA-TEXT-FLOVERSATT TO MOD-FLOVERSATT-YU           
102400               END-IF                                                     
102500                                                                          
102600           END-EVALUATE                                                   
102700         END-IF                                                           
102800         ADD +1 TO RAD-IX                                                 
102900       END-PERFORM                                                        
103000                                                                          
103100       MOVE +1 TO RAD-IX                                                  
103200       PERFORM UNTIL RAD-IX >= MAX-HOM-PLUS-1                             
103300*                                BENA-PCB + IO-AREA-2                     
103400         PERFORM IMS-GNP-BENA13                                           
103500         IF SEGMENT-FINNS                                                 
103600           MOVE BENA-HOM-TEHOMONYM TO MOD-TEHOMONYM(RAD-IX)               
103700         ELSE                                                             
103800           MOVE MFS-RENSA-FAELT TO MOD-TEHOMONYM(RAD-IX)                  
103900         END-IF                                                           
104000         ADD +1 TO RAD-IX                                                 
104100       END-PERFORM                                                        
104200     ELSE                                                                 
104300       MOVE FEL-1(IX) TO MOD-TEMFSFEL                                     
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 CA-RENSA-BENAMNINGSFALT SECTION.                                         
104800     SKIP2                                                                
104900     MOVE MFS-RENSA-FAELT TO                                              
105000     MOD-BEART(1)         MOD-FLOVERSATT(1)                               
105100     MOD-BEART(2)         MOD-FLOVERSATT(2)                               
105200     MOD-BEART(3)         MOD-FLOVERSATT(3)                               
105300     MOD-BEART(4)         MOD-FLOVERSATT(4)                               
105400     MOD-BEART(5)         MOD-FLOVERSATT(5)                               
105500     MOD-BEART(6)         MOD-FLOVERSATT(6)                               
105600     MOD-BEART(7)         MOD-FLOVERSATT(7)                               
105700     MOD-BEART(8)         MOD-FLOVERSATT(8)                               
105800     MOD-BEART(9)         MOD-FLOVERSATT(9)                               
105900     MOD-BEART(10)        MOD-FLOVERSATT(10)                              
106000     MOD-BEART(11)        MOD-FLOVERSATT(11)                              
106100     MOD-BEART-CZ-FINNS   MOD-FLOVERSATT-CZ                               
106200     MOD-BEART-GR-FINNS   MOD-FLOVERSATT-GR                               
106300     MOD-BEART-H-FINNS    MOD-FLOVERSATT-H                                
106400     MOD-BEART-IR-FINNS   MOD-FLOVERSATT-IR                               
106500     MOD-BEART-J-FINNS    MOD-FLOVERSATT-J                                
106600     MOD-BEART-KOR-FINNS  MOD-FLOVERSATT-KOR                              
106700     MOD-BEART-PL-FINNS   MOD-FLOVERSATT-PL                               
106800     MOD-BEART-RC-FINNS   MOD-FLOVERSATT-RC                               
106900     MOD-BEART-RCN-FINNS  MOD-FLOVERSATT-RCN                              
107000     MOD-BEART-RO-FINNS   MOD-FLOVERSATT-RO                               
107100     MOD-BEART-RUS-FINNS  MOD-FLOVERSATT-RUS                              
107200     MOD-BEART-T-FINNS    MOD-FLOVERSATT-T                                
107300     MOD-BEART-TR-FINNS   MOD-FLOVERSATT-TR                               
107400     MOD-BEART-YU-FINNS   MOD-FLOVERSATT-YU                               
107500                                                                          
107600                                                                          
107700                                                                          
107800     .                                                                    
107900     EJECT                                                                
108000 D-VISA-BILD-IGEN SECTION.                                                
108100     SKIP2                                                                
108200     MOVE MFS-ROER-EJ-FAELT TO   MOD-TEHOMONYM(1)                         
108300                                 MOD-TEHOMONYM(2)                         
108400         MOD-BEART(1)            MOD-FLOVERSATT(1)                        
108500         MOD-BEART(2)            MOD-FLOVERSATT(2)                        
108600         MOD-BEART(3)            MOD-FLOVERSATT(3)                        
108700         MOD-BEART(4)            MOD-FLOVERSATT(4)                        
108800         MOD-BEART(5)            MOD-FLOVERSATT(5)                        
108900         MOD-BEART(6)            MOD-FLOVERSATT(6)                        
109000         MOD-BEART(7)            MOD-FLOVERSATT(7)                        
109100         MOD-BEART(8)            MOD-FLOVERSATT(8)                        
109200         MOD-BEART(9)            MOD-FLOVERSATT(9)                        
109300         MOD-BEART(10)           MOD-FLOVERSATT(10)                       
109400         MOD-BEART(11)           MOD-FLOVERSATT(11)                       
109500         MOD-BEART-CZ-FINNS      MOD-FLOVERSATT-CZ                        
109600         MOD-BEART-GR-FINNS      MOD-FLOVERSATT-GR                        
109700         MOD-BEART-H-FINNS       MOD-FLOVERSATT-H                         
109800         MOD-BEART-IR-FINNS      MOD-FLOVERSATT-IR                        
109900         MOD-BEART-J-FINNS       MOD-FLOVERSATT-J                         
110000         MOD-BEART-KOR-FINNS     MOD-FLOVERSATT-KOR                       
110100         MOD-BEART-PL-FINNS      MOD-FLOVERSATT-PL                        
110200         MOD-BEART-RC-FINNS      MOD-FLOVERSATT-RC                        
110300         MOD-BEART-RCN-FINNS     MOD-FLOVERSATT-RCN                       
110400         MOD-BEART-RO-FINNS      MOD-FLOVERSATT-RO                        
110500         MOD-BEART-RUS-FINNS     MOD-FLOVERSATT-RUS                       
110600         MOD-BEART-T-FINNS       MOD-FLOVERSATT-T                         
110700         MOD-BEART-TR-FINNS      MOD-FLOVERSATT-TR                        
110800         MOD-BEART-YU-FINNS      MOD-FLOVERSATT-YU                        
110900     .                                                                    
111000     EJECT                                                                
111100                                                                          
111200* IMS SEKTIONER                                                           
111300     SKIP3                                                                
111400 IMS-GET-MSG SECTION.                                                     
111500     SKIP2                                                                
111600     MOVE '  QC' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
111800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111900     PERFORM IMS-STATUS-KONTROLL                                          
112000     .                                                                    
112100     SKIP3                                                                
112200 IMS-INSERT-MSG SECTION.                                                  
112300     SKIP2                                                                
112400     IF MSGI-IDLAND-SPR = 'SE'                                            
112500       MOVE '0' TO MFS-KDHUVOMR                                           
112600     END-IF                                                               
112700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
112800     MOVE SPACE TO GODK-STATUSKODER                                       
112900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
113000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
113100     PERFORM IMS-STATUS-KONTROLL                                          
113200     .                                                                    
113300     EJECT                                                                
113400                                                                          
113500* DLI-IO-AREA-1                                                           
113600                                                                          
113700 IMS-GU-BENA01-ASEQ SECTION.                                              
113800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
113900             W-BEART-X ')'                                                
114000             DELIMITED BY SIZE INTO SSA1                                  
114100     MOVE '  GE' TO GODK-STATUSKODER                                      
114200     CALL CBLTDLI USING GU BENAA-PCB DLI-IO-AREA-1 SSA1                   
114300     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
114400     PERFORM IMS-STATUS-KONTROLL                                          
114500     .                                                                    
114600     SKIP3                                                                
114700 IMS-GN-BENA01-ASEQ SECTION.                                              
114800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
114900             W-BEART-X ')'                                                
115000             DELIMITED BY SIZE INTO SSA1                                  
115100     MOVE '  GE' TO GODK-STATUSKODER                                      
115200     CALL CBLTDLI USING GN BENAA-PCB DLI-IO-AREA-1 SSA1                   
115300     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
115400     PERFORM IMS-STATUS-KONTROLL                                          
115500     .                                                                    
115600     EJECT                                                                
115700                                                                          
115800* DLI-IO-AREA-B  OCH  DLI-IO-AREA-2                                       
115900                                                                          
116000 IMS-GU-BENA01 SECTION.                                                   
116100     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
116200             DELIMITED BY SIZE INTO SSA1                                  
116300     MOVE '  GE' TO GODK-STATUSKODER                                      
116400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-B SSA1                    
116500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUS-KONTROLL                                          
116700     .                                                                    
116800     EJECT                                                                
116900 IMS-GHU-BENA01 SECTION.                                                  
117000     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
117100             DELIMITED BY SIZE INTO SSA1                                  
117200     MOVE '  GE' TO GODK-STATUSKODER                                      
117300     CALL CBLTDLI USING GHU BENA-PCB DLI-IO-AREA-B SSA1                   
117400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
117500     PERFORM IMS-STATUS-KONTROLL                                          
117600     .                                                                    
117700     EJECT                                                                
117800 IMS-GNP-BENA11-FIRST SECTION.                                            
117900     STRING 'WLBENA11*F(IDSKYLT  =' W-IDSKYLT-X ')'                       
118000             DELIMITED BY SIZE INTO SSA1                                  
118100     MOVE '  GE' TO GODK-STATUSKODER                                      
118200     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-2 SSA1                   
118300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
118400     PERFORM IMS-STATUS-KONTROLL                                          
118500     .                                                                    
118600     EJECT                                                                
118700 IMS-GNP-BENA12 SECTION.                                                  
118800     MOVE 'WLBENA12 ' TO SSA1                                             
118900     MOVE '  GE' TO GODK-STATUSKODER                                      
119000     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-2 SSA1                   
119100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
119200     PERFORM IMS-STATUS-KONTROLL                                          
119300     .                                                                    
119400     SKIP3                                                                
119500 IMS-GNP-BENA13 SECTION.                                                  
119600     MOVE 'WLBENA13 ' TO SSA1                                             
119700     MOVE '  GE' TO GODK-STATUSKODER                                      
119800     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-2 SSA1                   
119900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
120000     PERFORM IMS-STATUS-KONTROLL                                          
120100     .                                                                    
120200     SKIP3                                                                
120300 IMS-GHNP-BENA11-FIRST SECTION.                                           
120400     STRING 'WLBENA11*F(IDSKYLT  =' W-IDSKYLT-X ')'                       
120500             DELIMITED BY SIZE INTO SSA1                                  
120600     MOVE '  GE' TO GODK-STATUSKODER                                      
120700     CALL CBLTDLI USING GHNP BENA-PCB DLI-IO-AREA-2 SSA1                  
120800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUS-KONTROLL                                          
121000     .                                                                    
121100     EJECT                                                                
121200 IMS-REPL-BENA-2  SECTION.                                                
121300     MOVE '  ' TO GODK-STATUSKODER                                        
121400     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA-2                       
121500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
121600     PERFORM IMS-STATUS-KONTROLL                                          
121700     .                                                                    
121800     EJECT                                                                
121900 IMS-REPL-BENA-B SECTION.                                                 
122000     MOVE '  ' TO GODK-STATUSKODER                                        
122100     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA-B                       
122200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
122300     PERFORM IMS-STATUS-KONTROLL                                          
122400     .                                                                    
122500     EJECT                                                                
122600 IMS-ISRT-BENA01 SECTION.                                                 
122700     MOVE 'WLBENA01 ' TO SSA1                                             
122800     MOVE '  ' TO GODK-STATUSKODER                                        
122900     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-BENA01 SSA1                  
123000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
123100     PERFORM IMS-STATUS-KONTROLL                                          
123200     .                                                                    
123300     EJECT                                                                
123400 IMS-ISRT-BENA11 SECTION.                                                 
123500     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
123600              DELIMITED BY SIZE INTO SSA1                                 
123700     MOVE 'WLBENA11 ' TO SSA2                                             
123800     MOVE '  ' TO GODK-STATUSKODER                                        
123900     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-BENA11 SSA1 SSA2             
124000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
124100     PERFORM IMS-STATUS-KONTROLL                                          
124200     .                                                                    
124300     SKIP3                                                                
124400 IMS-GHNP-BENA13 SECTION.                                                 
124500     STRING 'WLBENA13(IDSEGMNR =' W-IDSEGMNR-X ')'                        
124600             DELIMITED BY SIZE INTO SSA1                                  
124700     MOVE '  GE' TO GODK-STATUSKODER                                      
124800     CALL CBLTDLI USING GHNP BENA-PCB DLI-IO-AREA-2 SSA1                  
124900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
125000     PERFORM IMS-STATUS-KONTROLL                                          
125100     .                                                                    
125200     SKIP3                                                                
125300 IMS-REPL-BENA13 SECTION.                                                 
125400     MOVE '  ' TO GODK-STATUSKODER                                        
125500     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA-2                       
125600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
125700     PERFORM IMS-STATUS-KONTROLL                                          
125800     .                                                                    
125900     SKIP3                                                                
126000 IMS-ISRT-BENA13 SECTION.                                                 
126100     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
126200           DELIMITED BY SIZE INTO SSA1                                    
126300     MOVE 'WLBENA13 ' TO SSA2                                             
126400     MOVE '  ' TO GODK-STATUSKODER                                        
126500     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA-2  SSA1 SSA2            
126600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
126700     PERFORM IMS-STATUS-KONTROLL                                          
126800     .                                                                    
126900     EJECT                                                                
127000 IMS-GU-WDK601 SECTION.                                                   
127100                                                                          
127200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
127300          DELIMITED BY SIZE INTO SSA1                                     
127400     MOVE '  ' TO GODK-STATUSKODER                                        
127500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-3 SSA1                    
127600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
127700     PERFORM IMS-STATUS-KONTROLL                                          
127800     .                                                                    
127900     EJECT                                                                
128000 IMS-GET-XXAI01 SECTION.                                                  
128100     STRING 'WLXXAI01(WDGXKEY  =' W-1207-KEY-X ')'                        
128200             DELIMITED BY SIZE INTO SSA1                                  
128300     MOVE '  GE' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING GU XXAI-PCB DLI-IO-XXAI01 SSA1                    
128500     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
128600     PERFORM IMS-STATUS-KONTROLL                                          
128700     .                                                                    
128800     SKIP3                                                                
128900 IMS-GET-XXAI11 SECTION.                                                  
129000     MOVE 'WLXXAI11 ' TO SSA1                                             
129100     MOVE '  ' TO GODK-STATUSKODER                                        
129200     CALL CBLTDLI USING GHNP XXAI-PCB DLI-IO-XXAI11 SSA1                  
129300     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUS-KONTROLL                                          
129500     .                                                                    
129600     SKIP3                                                                
129700 IMS-REPL-XXAI11 SECTION.                                                 
129800     MOVE '  ' TO GODK-STATUSKODER                                        
129900     CALL CBLTDLI USING REPL XXAI-PCB DLI-IO-XXAI11                       
130000     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
130100     PERFORM IMS-STATUS-KONTROLL                                          
130200     .                                                                    
130300     EJECT                                                                
130400 IMS-STATUS-KONTROLL SECTION.                                             
130500     SET STATUS-IX TO 1                                                   
130600     SEARCH GODK-STATUS                                                   
130700       AT END                                                             
130800         CALL FELLOG                                                      
130900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
131000       CONTINUE                                                           
131100     END-SEARCH                                                           
131200     .                                                                    
