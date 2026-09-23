000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1590300.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   2002/04/03.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BATCH                                                            
000900*                                                                         
001000*        SKAPAR EN EXTRAKTFIL FRÅN WDD3 FÖR BENÄMNINGAR SOM               
001100*        HAR FLAENDR="J" I ROTEN OCH FÖR DE EVENTUELLA BENNNR             
001200*        SOM KAN KOMMA PÅ INFIL W12114 FRÅN W121B1.                       
001300*        FÖR VIDARE LEVERANS TILL NEVIS-PROJEKTET                         
001400*                                                                         
001500*        Utfilen är ej sorterad, men kan med fördel sorteras på           
001600*        IDBENNR med (8,5,CH,A,5,3,CH,A) om man vill det                  
001700*                                                                         
001800*        PROGRAMMET LÄSER       WDD3                                      
001900*                   LÄSER       WDK6                                      
002000*                   LÄSER FIL   W12114                                    
002100*                   SKRIVER FIL W15904 för rapport till Nevis             
002200*                   SKRIVER FIL W15903 för uppdat av WDD3                 
002300*                                                                         
002400*    ÄNDRINGAR:                                                           
002500*        Tillägg av 9 nya språk till WDD3. Från Namnlex /Techla           
002600*        Nevis skall i detta skede endast ha Simplified Chinese.          
002700*        SCR eTracker = 2053029                                           
002800*    2005-10-11                                                           
002900*        Kolla KDPRODSL innan man skriver ut D12-post                     
003000*                                                                         
003100*    2006-05-15                                                           
003200*        Tillägg av fil W15903 för uppdat av WDD3 i W1590400              
003300*        Detta för att Nevis inte kan förlänga IDBENNR samtidigt.         
003400*                                                                         
003500*    2006-06-22                                                           
003600*        Akut ändring för att Nevis redan hade infört ändringen           
003700*        till 7-ställigt IDBENNR.                                         
003800*        eTracker 3516405.                                                
003900*                                                                         
004000*    ABENDKODER:                                                          
004100*        U0016 -  . . . .                                                 
004200*        U1000 -  . . . .                                                 
004300*                                                                         
004400                                                                          
004500     SKIP3                                                                
004600 ENVIRONMENT DIVISION.                                                    
004700     SKIP2                                                                
004800 INPUT-OUTPUT SECTION.                                                    
004900                                                                          
005000 FILE-CONTROL.                                                            
005100     SKIP2                                                                
005200*          --- BORTTAGNA BENÄMNINGAR I RENSNINGSRUTIN                     
005300     SELECT W12114                     ASSIGN TO W15903D1.                
005400     SKIP2                                                                
005500*          --- WDD3 MED ALLA SEGMENTDATA TILL NEVIS.                      
005600     SELECT W15904                     ASSIGN TO W15903D2.                
005700     SKIP2                                                                
005800*          --- Undantagsord från CASE-konvertering.                       
005900     SELECT W159XCP                    ASSIGN TO W15903D3.                
006000     SKIP2                                                                
006100*          --- WDD3 MED ALLA SEGMENTDATA, SEKVENSIELLT                    
006200     SELECT W15903                     ASSIGN TO W15903D4.                
006300     SKIP2                                                                
006400     EJECT                                                                
006500 DATA DIVISION.                                                           
006600     SKIP2                                                                
006700 FILE SECTION.                                                            
006800     SKIP3                                                                
006900 FD  W12114                                                               
007000     RECORDING      F                                                     
007100     BLOCK CONTAINS 0.                                                    
007200*01  FILLER    -COPY W12114 -L.                                           
007300     EJECT                                                                
007400 FD  W15904                                                               
007500     RECORDING       V                                                    
007600     RECORD IS VARYING FROM 1 TO 222 DEPENDING ON REC-LNGD                
007700     BLOCK CONTAINS  0.                                                   
007800 01  UT04-POST                   PIC X(222).                              
007900     EJECT                                                                
008000 FD  W159XCP                                                              
008100     RECORDING      F                                                     
008200     BLOCK CONTAINS 0.                                                    
008300 01  FILLER                    PIC X(105).                                
008400     EJECT                                                                
008500 FD  W15903                                                               
008600     RECORDING       F                                                    
008700     BLOCK CONTAINS  0.                                                   
008800 01  UT03-POST                   PIC X(12).                               
008900     EJECT                                                                
009000 WORKING-STORAGE SECTION.                                                 
009100                                                                          
009200 77  IDPGM                       PIC X(8)    VALUE 'W1590300'.            
009300 77  JA                          PIC X       VALUE 'J'.                   
009400 77  NEJ                         PIC X       VALUE 'N'.                   
009500 77  OCH                         PIC X       VALUE '&'.                   
009600 77  BENRAK                      PIC S9(4)   VALUE ZERO.                  
009700 77  TKN                         PIC  9(2)   VALUE ZERO.                  
009800 77  TXTL                        PIC  9(2)   VALUE ZERO.                  
009900 77  POS                         PIC  9(2)   VALUE ZERO.                  
010000 77  2-SPACE                     PIC  X(2)   VALUE SPACE.                 
010100                                                                          
010200 77  WS-GEMEN      PIC X(25)   VALUE 'åâáàäçêéèëîíìïñôóòöõüûúùý'.         
010300 77  WS-VERSAL     PIC X(25)   VALUE 'ÅÂÁÀÄÇÊÉÈËÎÍÌÏÑÔÓÒÖÕÜÛÚÙÝ'.         
010400 77  S-GEMEN                   PIC X(5)   VALUE 'åäöüé'.                  
010500 77  S-VERSAL                  PIC X(5)   VALUE 'ÅÄÖÜÉ'.                  
010600                                                                          
010700 77  BASENS-CHAR-FORMAT          PIC X(6)   VALUE 'UTF8  '.               
010800*    --- Benämningar som INTE tillhör LATIN-1 är alltid UTF8              
010900*    --- D.v.s. språk som repr. av LATIN-2 och Dubbel-Byte-Språk          
011000*    --- Från ÄT 05:7 är SYSIN-data som styr detta "CONDITION"            
011100*    --- borttaget och styrning sker på IDSKYLT istället.                 
011200*    --- Se sektion D- och sektion S31-                                   
011300     88  BASEN-EBCDIC                       VALUE 'EBCDIC'.               
011400     88  BASEN-UTF8                         VALUE 'UTF8  '.               
011500                                                                          
011510*                                                                         
011520*01  -COPY WWPRODSL                                                       
011530*                                                                         
012200                                                                          
012300 77  ART-FINNS-SW               PIC X       VALUE 'N'.                    
012400     88  ART-FINNS                          VALUE 'J'.                    
012500     88  ART-SAKNAS                         VALUE 'N'.                    
012600                                                                          
012700 77  W12114-EOF-SW              PIC X       VALUE 'N'.                    
012800     88  END-OF-W12114                      VALUE 'J'.                    
012900                                                                          
013000 77  W159XCP-EOF-SW              PIC X      VALUE 'N'.                    
013100     88  END-OF-W159XCP                     VALUE 'J'.                    
013200                                                                          
013300 77  MODIFIED-SW                 PIC X      VALUE 'N'.                    
013400     88  MODIFIED                           VALUE 'J'.                    
013500                                                                          
013600 77  VALID-S-DELIMITERS         PIC X.                                    
013700     88  VALID-SDLM            VALUES ARE  ' '.                           
013800                                                                          
013900 77  VALID-E-DELIMITERS         PIC X.                                    
014000     88  VALID-EDLM            VALUES ARE  ' ' '-' '+' ','.               
014100     EJECT                                                                
014200*    --- KONV.FEL ELLER TRUNK.FEL                                         
014300 77  W-LOWVALUE                  PIC X(700)  VALUE LOW-VALUE.             
014400 77  W-FELTYP                    PIC X(15)   VALUE SPACE.                 
014500     EJECT                                                                
014600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014700 01  FILLER REDEFINES DAGENS-DATUM.                                       
014800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015100     EJECT                                                                
015200 01  DYNAMISKA-SUBPROGRAM.                                                
015300*                                                                         
015400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015800*  Konverterar från UTF-8 på basen till Char Ent.                         
015900*                                  (använder subpgm WCNVUTFU)             
016000     03  WCNVUNCE                PIC X(8)    VALUE 'WCNVUNCE'.            
016100*  Konverterar från EBCDIC på basen till Char Ent.                        
016200*                            (använder interna tabeller)                  
016300     03  WCNVJAUN                PIC X(8)    VALUE 'WCNVJAUN'.            
016400     03  WCNVKOUN                PIC X(8)    VALUE 'WCNVKOUN'.            
016500     03  WCNVRUUN                PIC X(8)    VALUE 'WCNVRUUN'.            
016600     03  WCNVTHUN                PIC X(8)    VALUE 'WCNVTHUN'.            
016700     03  WCNVTRUN                PIC X(8)    VALUE 'WCNVTRUN'.            
016800     03  WCNVZHUN                PIC X(8)    VALUE 'WCNVZHUN'.            
016900     SKIP2                                                                
017000*    --- PARAMETRAR TILL ABEND                                            
017100                                                                          
017200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017500     SKIP2                                                                
017600 01  FELTEXT.                                                             
017700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017900     EJECT                                                                
018000 01  FILLER              PIC X(24)   VALUE 'W005-START   '.               
018100*01  -COPY W0005   -PRE  POSTSUM-                                         
018200     EJECT                                                                
018300 01  FILLER              PIC X(24)   VALUE 'WCNVAREA-START   '.           
018400*01  -COPY WCNVAREA  -PRE CNV-                                            
018500     EJECT                                                                
018600 01  FILLER              PIC X(24)   VALUE 'CASE-SHIFT-AREA  '.           
018700*    -COPY W121CONV  -PRE CASE-                                           
018800     EJECT                                                                
018900 01  FILLER              PIC X(24)   VALUE 'IN-AREA-START  '.             
019000 01  IN-AREA.                                                             
019100*    03 -COPY W12114                                                      
019200     EJECT                                                                
019300                                                                          
019400                                                                          
019500 01  TABIN-AREA-START    PIC X(24)   VALUE 'W159XCP-AREA-START'.          
019600 01  W159XCP-AREA.                                                        
019700     03  W159XCP-BEART   PIC X(25).                                       
019800     03  W159XCP-GB-DESCR PIC X(40).                                      
019900     03  W159XCP-SV-DESCR PIC X(40).                                      
020000     EJECT                                                                
020100                                                                          
020200                                                                          
020300 01  FILLER              PIC X(24)   VALUE 'UT04-AREA-START  '.           
020400* --- Detta är Nevis-arean enligt den nya längden                         
020500* --- skall skickas till Nevis via VCOM-job W159Z2SE                      
020600 01  UT04AREA.                                                            
020700     03 UT04-AREA.                                                        
020800        05 UT04-IDPTYP   PIC X(3).                                        
020900        05 UT04-IDBENNR  PIC 9(7).                                        
021000        05 FILLER        PIC X(212).                                      
021100*    03 AREA -COPY W1590301 -PRE UT0401- -RED UT04-AREA.                  
021200*    03 AREA -COPY W1590311 -PRE UT0411- -RED UT04-AREA.                  
021300*    03 AREA -COPY W1590312 -PRE UT0412- -RED UT04-AREA.                  
021400*    03 AREA -COPY W1590313 -PRE UT0413- -RED UT04-AREA.                  
021500     EJECT                                                                
021600                                                                          
021700 01  FILLER              PIC X(24)   VALUE 'UT03-AREA-START  '.           
021800* --- Detta är UT-arean enligt den nya IDBENNR-längden                    
021900* --- för uppdatering av FLAENDR i program W1590400                       
022000* --- Det räcker med enbart D01-poster.                                   
022100 01  UT03AREA.                                                            
022200     03 UT03-AREA.                                                        
022300        05 UT03-IDPTYP   PIC X(3).                                        
022400        05 UT03-IDBENNR  PIC 9(7).                                        
022500        05 FILLER        PIC X(2).                                        
022600*    03 AREA -COPY W1590301 -PRE UT0301- -RED UT03-AREA.                  
022700     EJECT                                                                
022800 01  FILLER              PIC X(24)   VALUE 'HJALP-AREA-START'.            
022900 01  HJALP-AREA.                                                          
023000     03 WS-LONG-BEART            PIC X(200) VALUE SPACE.                  
023100     03 WS-TEST-BEART            PIC X(200) VALUE SPACE.                  
023200     03 WS-IDBENNR               PIC 9(7)  VALUE ZERO.                    
023300     03 IX                       PIC S9(4)  BINARY.                       
023400                                                                          
023500 01  SPRAK-TABELL-AREA   PIC X(24)   VALUE 'SPRAK-TABELL-AREA'.           
023600 01  WS-SPRAK-TABELL.                                                     
023700     03 WS-SPRAKTAB.                                                      
023800*                     före ät 05:7             efter ÄT 05:7              
023900       05 FILLER                      PIC X(5) VALUE 'CZ cs'.             
024000       05 FILLER      VALUE 'D  de'   PIC X(5).                           
024100       05 FILLER                      PIC X(5) VALUE 'DK da'.             
024200       05 FILLER      VALUE 'E  es'   PIC X(5).                           
024300       05 FILLER      VALUE 'F  fr'   PIC X(5).                           
024400       05 FILLER      VALUE 'GB en'   PIC X(5).                           
024500       05 FILLER                      PIC X(5) VALUE 'GR el'.             
024600       05 FILLER                      PIC X(5) VALUE 'H  hu'.             
024700       05 FILLER      VALUE 'I  it'   PIC X(5).                           
024800       05 FILLER                      PIC X(5) VALUE 'IR fa'.             
024900       05 FILLER      VALUE 'J  ja'   PIC X(5).                           
025000       05 FILLER      VALUE 'KORko'   PIC X(5).                           
025100       05 FILLER      VALUE 'MALms'   PIC X(5).                           
025200       05 FILLER      VALUE 'NL nl'   PIC X(5).                           
025300       05 FILLER      VALUE 'P  pt'   PIC X(5).                           
025400       05 FILLER                      PIC X(5) VALUE 'PL pl'.             
025500       05 FILLER      VALUE 'RC zh'   PIC X(5).                           
025600       05 FILLER                      PIC X(5) VALUE 'RCNcn'.             
025700       05 FILLER                      PIC X(5) VALUE 'RO ro'.             
025800       05 FILLER      VALUE 'RUSru'   PIC X(5).                           
025900       05 FILLER      VALUE 'S  sv'   PIC X(5).                           
026000       05 FILLER      VALUE 'SF fi'   PIC X(5).                           
026100       05 FILLER      VALUE 'T  th'   PIC X(5).                           
026200       05 FILLER      VALUE 'TR tr'   PIC X(5).                           
026300       05 FILLER      VALUE 'USAus'   PIC X(5).                           
026400       05 FILLER                      PIC X(5) VALUE 'YU sr'.             
026500*    --- Tabell för översättning från IDSKYLT till IDSPRAK                
026600     03 FILLER REDEFINES WS-SPRAKTAB.                                     
026700*      05 TABELL-RAD  OCCURS 17  ASCENDING KEY TAB-IDSKYLT                
026800       05 TABELL-RAD  OCCURS 26  ASCENDING KEY TAB-IDSKYLT                
026900                                 INDEXED BY TAB-IX.                       
027000         07 TAB-IDSKYLT  PIC X(3).                                        
027100         07 TAB-IDSPRAK  PIC X(2).                                        
027200     EJECT                                                                
027300                                                                          
027400 01  DATA-LANGD-START    PIC X(24)   VALUE 'DATA-LANGD'.                  
027500 01  DATA-LANGD.                                                          
027600*    --- POINTER VID DATALÄNGDSBERÄKNING                                  
027700     03 DTA-LNGD                 PIC S9(4)   VALUE ZERO COMP.             
027800                                                                          
027900                                                                          
028000 01  REC-LANGD-START    PIC X(24)   VALUE 'REC-LANGD'.                    
028100 01  REC-LANGD.                                                           
028200*    --- LÄNGD PÅ VARIABELT UT-RECORD I W15904-/W15903-filen              
028300     03 REC-LNGD                 PIC  9(4)   VALUE 0001 COMP.             
028400                                                                          
028500     EJECT                                                                
028600*    --- Tabell med undantagsord för CASE-konverteringen I S25C-          
028700 01  TABIX                       PIC S9(5) COMP-3 VALUE ZERO.             
028800 01  MAX-TABIX                   PIC S9(5) COMP-3 VALUE +500.             
028900 01  TAB-EXCP.                                                            
029000     03  FILLER   OCCURS 500.                                             
029100         05  TAB-EXCP-TEXT     PIC X(25).                                 
029200         05  TAB-EXCP-LNGD     PIC 9(2).                                  
029300                                                                          
029400     EJECT                                                                
029500*                                                                         
029600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029800     SKIP3                                                                
029900     SKIP3                                                                
030000 01  FILLER                      PIC X(16)   VALUE 'DLI-KEY'.             
030100 01  NYCKLAR-TILL-DLI.                                                    
030200                                                                          
030300     03  W-IDARTNR-X.                                                     
030400         05  W-IDARTNR           PIC S9(9)  VALUE ZERO  COMP-3.           
030500                                                                          
030600     03  W-IDBENNR-X.                                                     
030700         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
030800                                                                          
030900     SKIP2                                                                
031000*    --- STATUS-KOD FRÅN IMS                                              
031100 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
031200 01  STATUS-WS                   PIC XX.                                  
031300     88  SEGMENT-FINNS                       VALUE '  '.                  
031400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031600     SKIP2                                                                
031700 01  GODK-STATUSKODER.                                                    
031800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031900     SKIP3                                                                
032000 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
032100 01  SSA1                        PIC X(64).                               
032200 01  SSA2                        PIC X(64).                               
032300 01  SSA3                        PIC X(64).                               
032400     EJECT                                                                
032500*    --- IMS FUNKTIONSKODER                                               
032600*01  -COPY W0003                                                          
032700     EJECT                                                                
032800*    ---  DLI INPUT-OUTPUT AREA                                           
032900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
033000 01  DLI-IO-WDD301.                                                       
033100*    03  -COPY WDD301                                                     
033200     EJECT                                                                
033300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
033400 01  DLI-IO-WDD311.                                                       
033500*    03  -COPY WDD311                                                     
033600     EJECT                                                                
033700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD312'.                      
033800 01  DLI-IO-WDD312.                                                       
033900*    03  -COPY WDD312                                                     
034000     EJECT                                                                
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD313'.                      
034200 01  DLI-IO-WDD313.                                                       
034300*    03  -COPY WDD313                                                     
034400     EJECT                                                                
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
034600 01  DLI-IO-WDK601.                                                       
034700*    03  -COPY WDK601 -PRE WDK6-                                          
034800     EJECT                                                                
034900 LINKAGE SECTION.                                                         
035000*01  -COPY W0008  -PRE WDD3-                                              
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300*01  -COPY W0008  -PRE WDK6-                                              
035400     05  FILLER                  PIC X.                                   
035500     EJECT                                                                
035600                                                                          
035700 PROCEDURE DIVISION  USING  WDD3-PCB WDK6-PCB.                            
035800 MAIN SECTION.                                                            
035900     ENTRY 'DLITCBL' USING  WDD3-PCB WDK6-PCB.                            
036000                                                                          
036100     PERFORM A-INIT                                                       
036200     PERFORM A-XCP-INIT                                                   
036300*    --- Kolla nu hela WDD3 om något blivit ändrat                        
036400     PERFORM IMS-GN-WDD301                                                
036500                                                                          
036600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
036700* 01 ROTSEGMENT                                                           
036800       IF BEN-FLAENDR = JA                                                
036900*        --- lägg till OR NEJ, ovan, vid total överföring                 
037000         MOVE BEN-IDBENNR TO W-IDBENNR                                    
037100         PERFORM C-BEHANDLA-WDD301                                        
037200                                                                          
037300*   11 TEXT-SEGMENT                                                       
037400         PERFORM IMS-GNP-WDD311-FIRST                                     
037500         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
037600*          --- De bakvända texterna skall utlämnas                        
037700*          --- Även de språk som NEVIS inte vill ha ännu.                 
037800           IF TEXT-IDSKYLT NOT = '  S' AND ' BG' AND                      
037900                                 'CZ ' AND 'DK ' AND 'H  ' AND            
038000                                 'IR ' AND 'RO ' AND 'YU ' AND            
038100                                 'GR '                                    
038200             PERFORM D-BEHANDLA-WDD311                                    
038300           END-IF                                                         
038400           PERFORM IMS-GNP-WDD311                                         
038500         END-PERFORM                                                      
038600                                                                          
038700*   12 ARTIKEL-SEGMENT                                                    
038800         PERFORM IMS-GNP-WDD312                                           
038900         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
039000           PERFORM E-BEHANDLA-WDD312                                      
039100           PERFORM IMS-GNP-WDD312                                         
039200         END-PERFORM                                                      
039300                                                                          
039400*   13 HOMONYM-SEGMENT                                                    
039500         PERFORM IMS-GNP-WDD313                                           
039600         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
039700           PERFORM F-BEHANDLA-WDD313                                      
039800           PERFORM IMS-GNP-WDD313                                         
039900         END-PERFORM                                                      
040000       ELSE                                                               
040100*      --- Bortkommenterat vid total överföring                           
040200*        --- INGENTING PÅ DENNA BENÄMNING HAR ÄNDRATS                     
040300         CONTINUE                                                         
040400       END-IF                                                             
040500       PERFORM IMS-GN-WDD301                                              
040600     END-PERFORM                                                          
040700                                                                          
040800*    --- Kolla om det finns poster på W12114                              
040900     PERFORM S01-LAES-W12114                                              
041000     INITIALIZE UT03-AREA                                                 
041100     INITIALIZE UT04-AREA                                                 
041200     MOVE 'D01' TO UT03-IDPTYP                                            
041300                   UT04-IDPTYP                                            
041400                                                                          
041500     PERFORM UNTIL END-OF-W12114                                          
041600       PERFORM B-Behandla-Borttagna-Ben                                   
041700       PERFORM S01-LAES-W12114                                            
041800     END-PERFORM                                                          
041900                                                                          
042000     PERFORM Z-FINIT                                                      
042100                                                                          
042200     MOVE ZERO TO RETURN-CODE                                             
042300     GOBACK                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 A-INIT SECTION.                                                          
042700                                                                          
042800     OPEN INPUT  W12114                                                   
042900     OPEN INPUT  W159XCP                                                  
043000     OPEN OUTPUT W15904                                                   
043100     OPEN OUTPUT W15903                                                   
043200                                                                          
043300***  ACCEPT BASENS-CHAR-FORMAT FROM SYSIN                                 
043400                                                                          
043500     ACCEPT DAGENS-DATUM  FROM DATE                                       
043600                                                                          
043700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
043800                                                                          
043900     INITIALIZE UT03-AREA                                                 
044000     INITIALIZE UT04-AREA                                                 
044100     .                                                                    
044200     EJECT                                                                
044300                                                                          
044400 A-XCP-INIT SECTION.                                                      
044500     SKIP2                                                                
044600*    LÄS IN UNDANTAGEN FÖR CASE-SHIFT TILL TABELL                         
044700*    - ANV. VID KONVERTERING AV VERSALER TILL GEMENER PÅ BENÄMNING        
044800     MOVE +1 TO TABIX                                                     
044900     PERFORM S04-LAES-W159XCP                                             
045000     PERFORM UNTIL END-OF-W159XCP OR TABIX > MAX-TABIX                    
045100                                                                          
045200       MOVE W159XCP-BEART TO TAB-EXCP-TEXT(TABIX)                         
045300                                                                          
045400       MOVE ZERO TO TKN                                                   
045500       INSPECT TAB-EXCP-TEXT(TABIX) TALLYING TKN FOR                      
045600       CHARACTERS BEFORE INITIAL 2-SPACE                                  
045700       MOVE TKN           TO TAB-EXCP-LNGD(TABIX)                         
045800                                                                          
045900       ADD +1 TO TABIX                                                    
046000       PERFORM S04-LAES-W159XCP                                           
046100     END-PERFORM                                                          
046200                                                                          
046300     IF END-OF-W159XCP                                                    
046400       MOVE TABIX TO MAX-TABIX                                            
046500       SUBTRACT 1 FROM MAX-TABIX                                          
046600                                                                          
046700       PERFORM UNTIL TABIX > MAX-TABIX                                    
046800         MOVE SPACE TO TAB-EXCP-TEXT(TABIX)                               
046900         MOVE ZERO  TO TAB-EXCP-LNGD(TABIX)                               
047000         ADD +1 TO TABIX                                                  
047100       END-PERFORM                                                        
047200     ELSE                                                                 
047300       IF TABIX > MAX-TABIX                                               
047400          DISPLAY '* * * * * * * * * * * * * * * * * *'                   
047500          DISPLAY '* * * *                     * * * *'                   
047600          DISPLAY '* * * *  XCP-TABELLEN FULL  * * * *'                   
047700          DISPLAY '* * * *                     * * * *'                   
047800          DISPLAY '* * * * * * * * * * * * * * * * * *'                   
047900          PERFORM S99-ABEND                                               
048000       END-IF                                                             
048100     END-IF                                                               
048200                                                                          
048300     .                                                                    
048400     EJECT                                                                
048500 B-Behandla-Borttagna-Ben  SECTION.                                       
048600     SKIP2                                                                
048700     MOVE DLET-IDBENNR   TO UT0401-IDBENNR                                
048800     MOVE DLET-KDHOMONYM TO UT0401-KDHOMONYM                              
048900     MOVE 3              TO UT0401-KDBENSTAT                              
049000     MOVE LENGTH OF UT0401-W1590301 TO REC-LNGD                           
049100     PERFORM S11-SKRIV-W15904                                             
049200                                                                          
049300     MOVE DLET-IDBENNR   TO UT0301-IDBENNR                                
049400     MOVE DLET-KDHOMONYM TO UT0301-KDHOMONYM                              
049500     MOVE 3              TO UT0301-KDBENSTAT                              
049600     PERFORM S12-SKRIV-W15903                                             
049700     .                                                                    
049800     EJECT                                                                
049900 C-BEHANDLA-WDD301   SECTION.                                             
050000     SKIP2                                                                
050100     MOVE 'D01'          TO UT0401-IDPTYP                                 
050200     MOVE BEN-IDBENNR    TO UT0401-IDBENNR                                
050300     MOVE BEN-KDHOMONYM  TO UT0401-KDHOMONYM                              
050400     MOVE BEN-KDBENSTAT  TO UT0401-KDBENSTAT                              
050500     MOVE LENGTH OF UT0401-W1590301 TO REC-LNGD                           
050600     PERFORM S11-SKRIV-W15904                                             
050700                                                                          
050800     MOVE 'D01'          TO UT0301-IDPTYP                                 
050900     MOVE BEN-IDBENNR    TO UT0301-IDBENNR                                
051000     MOVE BEN-KDHOMONYM  TO UT0301-KDHOMONYM                              
051100     MOVE BEN-KDBENSTAT  TO UT0301-KDBENSTAT                              
051200     PERFORM S12-SKRIV-W15903                                             
051300     .                                                                    
051400     EJECT                                                                
051500 D-BEHANDLA-WDD311   SECTION.                                             
051600     SKIP2                                                                
051700     MOVE 'D11'            TO UT0411-IDPTYP                               
051800     MOVE TEXT-IDSKYLT     TO UT0411-IDSKYLT                              
051900     SEARCH ALL TABELL-RAD                                                
052000       AT END                                                             
052100         MOVE SPACE        TO UT0411-IDSPRAK                              
052200       WHEN TAB-IDSKYLT(TAB-IX) = TEXT-IDSKYLT                            
052300         MOVE TAB-IDSPRAK(TAB-IX)                                         
052400                           TO UT0411-IDSPRAK                              
052500     END-SEARCH                                                           
052600                                                                          
052700*      ---              Alla språk som är lagrade i UTF8-format           
052800     IF TEXT-IDSKYLT = 'CZ ' OR 'GR ' OR 'H  ' OR 'IR ' OR                
052900                       'J  ' OR 'KOR' OR 'PL ' OR 'RC ' OR                
053000                       'RCN' OR 'RO ' OR 'RUS' OR 'T  ' OR                
053100                       'TR ' OR 'YU '                                     
053200                                                                          
053300                                                                          
053400       MOVE TEXT-BEARTEXT  TO CNV-TECONV-FROM                             
053500                                                                          
053600*      --- XML Unicode Character Entities ska produceras                  
053700       PERFORM S31-KONVERTERA-TEXT                                        
053800                                                                          
053900       MOVE CNV-TECONV-TO  TO WS-LONG-BEART                               
054000                                                                          
054100     ELSE                                                                 
054200       MOVE TEXT-BEARTEXT  TO WS-LONG-BEART                               
054300       Perform S25C-FIXA-SNYGG-TEXT                                       
054400       IF MODIFIED                                                        
054500         DISPLAY TEXT-IDSKYLT '   ' WS-TEST-BEART(1:35)                   
054600                ' --> ' WS-LONG-BEART(1:35)                               
054700       END-IF                                                             
054800     END-IF                                                               
054900                                                                          
055000     MOVE WS-LONG-BEART    TO UT0411-NEVIS-BEART                          
055100     IF CNV-KDSVAR = 'F'                                                  
055200       MOVE SPACE          TO UT0411-NEVIS-BEART                          
055300     END-IF                                                               
055400     MOVE SPACE TO CNV-KDSVAR                                             
055500                                                                          
055600     MOVE TEXT-FLOVERSATT  TO UT0411-FLOVERSATT                           
055700     MOVE TEXT-TIUPPDAT    TO UT0411-TIUPPDAT                             
055800                                                                          
055900     MOVE LENGTH OF UT0411-W1590311 TO REC-LNGD                           
056000     PERFORM S11-SKRIV-W15904                                             
056100     .                                                                    
056200     EJECT                                                                
056300 E-BEHANDLA-WDD312   SECTION.                                             
056400     SKIP2                                                                
056500     MOVE 'D12'            TO UT0412-IDPTYP                               
056600                                                                          
056700     MOVE ART-IDARTNR      TO W-IDARTNR                                   
056800     PERFORM IMS-GU-WDK601                                                
056900*    ---- SEGMENT MÅSTE FINNAS                                            
057000     MOVE WDK6-ART-KDPRODSL TO TEST-KDPRODSL                              
057010*-- NOTE: THE SAME PRODSL-S SHOULD BE ALLOWED IN PGM W15901 ALSO          
057100     IF KDPRODSL-VOLVO-UTAN-EMB                                           
057200       MOVE ART-IDARTNR    TO UT0412-IDARTNR                              
057300       MOVE ART-FLFELHOMO  TO UT0412-FLFELHOMO                            
057400                                                                          
057500       MOVE LENGTH OF UT0412-W1590312 TO REC-LNGD                         
057600       PERFORM S11-SKRIV-W15904                                           
057800     END-IF                                                               
057900     .                                                                    
058000     EJECT                                                                
058100 F-BEHANDLA-WDD313   SECTION.                                             
058200     SKIP2                                                                
058300     MOVE 'D13'            TO UT0413-IDPTYP                               
058400                                                                          
058500     MOVE HOM-IDSEGMNR     TO UT0413-IDSEGMNR                             
058600     MOVE HOM-TEHOMONYM    TO UT0413-TEHOMONYM                            
058700                                                                          
058800     MOVE LENGTH OF UT0413-W1590313 TO REC-LNGD                           
058900     PERFORM S11-SKRIV-W15904                                             
059000     .                                                                    
059100     EJECT                                                                
059200 S01-LAES-W12114  SECTION.                                                
059300     SKIP2                                                                
059400     READ W12114 INTO IN-AREA                                             
059500     AT END                                                               
059600        SET END-OF-W12114 TO TRUE                                         
059700                                                                          
059800     NOT AT END                                                           
059900       MOVE '14-'      TO POSTSUM-TRANSTYP                                
060000       MOVE 'W12114'   TO POSTSUM-FDNAMN                                  
060100       MOVE 'W15903D1' TO POSTSUM-DDNAMN2                                 
060200       CALL POSTSUM USING POSTSUM-PARM                                    
060300     END-READ                                                             
060400     .                                                                    
060500     EJECT                                                                
060600 S04-LAES-W159XCP SECTION.                                                
060700     SKIP2                                                                
060800     READ W159XCP INTO W159XCP-AREA                                       
060900     AT END                                                               
061000        SET END-OF-W159XCP TO TRUE                                        
061100                                                                          
061200     NOT AT END                                                           
061300        MOVE 'W159XCP' TO POSTSUM-FDNAMN                                  
061400        MOVE 'W15903D3' TO POSTSUM-DDNAMN2                                
061500        MOVE 'XCP'        TO POSTSUM-TRANSTYP                             
061600        CALL POSTSUM USING POSTSUM-PARM                                   
061700     END-READ                                                             
061800     .                                                                    
061900     EJECT                                                                
062000 S11-SKRIV-W15904 SECTION.                                                
062100                                                                          
062200     IF REC-LNGD NOT = ZERO                                               
062300       WRITE UT04-POST FROM UT04-AREA                                     
062400                                                                          
062500       MOVE '04-'   TO POSTSUM-TRANSTYP                                   
062600       MOVE 'W15904' TO POSTSUM-FDNAMN                                    
062700       MOVE 'W15903D2' TO POSTSUM-DDNAMN2                                 
062800       CALL POSTSUM USING POSTSUM-PARM                                    
062900     ELSE                                                                 
063000       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
063100       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
063200       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
063300       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 S12-SKRIV-W15903 SECTION.                                                
063800                                                                          
063900     IF REC-LNGD NOT = ZERO                                               
064000       WRITE UT03-POST FROM UT03-AREA                                     
064100                                                                          
064200       MOVE '03-'   TO POSTSUM-TRANSTYP                                   
064300       MOVE 'W15903' TO POSTSUM-FDNAMN                                    
064400       MOVE 'W15903D4' TO POSTSUM-DDNAMN2                                 
064500       CALL POSTSUM USING POSTSUM-PARM                                    
064600     ELSE                                                                 
064700       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
064800       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
064900       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
065000       DISPLAY ' ******* ERROR IN THE REC-LNGD FIELD *******'             
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 S25C-FIXA-SNYGG-TEXT   SECTION.                                          
065500     SKIP2                                                                
065600*    FIXAR SNYGG TEXT FÖR ALLA språk med teckenkod LATIN-1                
065700                                                                          
065800*    Texten i urspr. format läggs i WS-TEST-BEART                         
065900*    Texten i WS-LONG-BEART bearbetas så att den får inledande            
066000*    versal och resten gemener.                                           
066100*    Därefter kollas WS-TEST-BEART om den innehåller något av de          
066200*    undantagsord som finns inladdade till tabell TAB-EXCP.               
066300*    Vid träff skall detta VERSALA ord läggas över i rätt position        
066400*    i WS-LONG-BEART.                                                     
066500*    Regeln för att träff skall räknas är att ORDET skall omges av        
066600*    en godkänd delimiter VALID-DLM eller att det står först i            
066700*    fältet och avslutas av en godkänd delimiter.                         
066800                                                                          
066900     MOVE WS-LONG-BEART TO WS-TEST-BEART                                  
067000                                                                          
067100     MOVE NEJ TO MODIFIED-SW                                              
067200     MOVE ZERO TO TXTL                                                    
067300     INSPECT WS-TEST-BEART TALLYING TXTL FOR CHARACTERS                   
067400     BEFORE INITIAL '    '                                                
067500                                                                          
067600*    --- Fixa SNYGG text för normalfallet utan undantag                   
067700     IF WS-LONG-BEART > SPACE                                             
067800       MOVE FUNCTION LOWER-CASE(WS-LONG-BEART(2:))                        
067900                             TO WS-LONG-BEART(2:)                         
068000*      Nu även de "Konstiga" tecknen                                      
068100       INSPECT WS-LONG-BEART(2:) CONVERTING WS-VERSAL TO WS-GEMEN         
068200                                                                          
068300*      POS  = Undersökt startPOS i WS-TEST-BEART                          
068400*      TKN  = Längden på undantagsordet                                   
068500*      TXTL = Text-Längden på benämningen i WS-TEST-BEART                 
068600       MOVE +1 TO TABIX                                                   
068700*      TABELLSNURRA                                                       
068800       PERFORM UNTIL TABIX = MAX-TABIX                                    
068900         MOVE TAB-EXCP-LNGD (TABIX) TO TKN                                
069000         MOVE +1                    TO POS                                
069100                                                                          
069200*        --- Kolla specialfallet i början av benämningen                  
069300         IF WS-TEST-BEART(1:TKN) = TAB-EXCP-TEXT(TABIX)(1:TKN)            
069400           MOVE WS-TEST-BEART((TKN + 1):1) TO VALID-E-DELIMITERS          
069500           IF VALID-EDLM                                                  
069600             MOVE WS-TEST-BEART(1:TKN) TO WS-LONG-BEART(1:TKN)            
069700             SET MODIFIED TO TRUE                                         
069800             ADD TKN TO POS                                               
069900           END-IF                                                         
070000         END-IF                                                           
070100                                                                          
070200*        --- Kolla sedan löpande varje vettig position därefter           
070300         ADD +1 TO POS                                                    
070400*        TEXT-SNURRA                                                      
070500         PERFORM UNTIL POS > TXTL                                         
070600           IF WS-TEST-BEART(POS:TKN)                                      
070700                               = TAB-EXCP-TEXT(TABIX)(1:TKN)              
070800              MOVE WS-TEST-BEART((POS - 1):1)                             
070900                                           TO VALID-S-DELIMITERS          
071000              MOVE WS-TEST-BEART((POS + TKN):1)                           
071100                                           TO VALID-E-DELIMITERS          
071200              IF  VALID-SDLM                                              
071300              AND VALID-EDLM                                              
071400                 MOVE WS-TEST-BEART(POS:TKN)                              
071500                                     TO WS-LONG-BEART(POS:TKN)            
071600                 SET MODIFIED TO TRUE                                     
071700                 ADD TKN TO POS                                           
071800              END-IF                                                      
071900           END-IF                                                         
072000                                                                          
072100           ADD +1 TO POS                                                  
072200         END-PERFORM                                                      
072300                                                                          
072400         ADD +1 TO TABIX                                                  
072500       END-PERFORM                                                        
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 S31-KONVERTERA-TEXT SECTION.                                             
073000     SKIP2                                                                
073100     IF CNV-TECONV-FROM = SPACE                                           
073200       MOVE SPACE TO CNV-TECONV-TO                                        
073300     ELSE                                                                 
073400*      -- Ange att XML Unicode Character Entities ska produceras          
073500       MOVE  JA TO CNV-FLTXTENT                                           
073600*      -- Ange maxlängden för konverteringen.                             
073700       MOVE 200 TO CNV-KVMAXTL                                            
073800                                                                          
073900       IF BASEN-UTF8                                                      
074000*        -- Konvertering gäller från UTF8 för dessa språk                 
074100         MOVE JA TO CNV-FLUTF8                                            
074200                                                                          
074300         CALL WCNVUNCE USING CNV-WCNVAREA                                 
074400                                                                          
074500*      ELSE                                                               
074600*        CONTINUE                                                         
074700*        -- ANGER ATT KONVERTERING GÄLLER FRÅN EBCDIC                     
074800*        -- Egentligen är denna "ELSE" onödig, eftersom                   
074900*        -- beslut numera är taget att lagra benämningar som inte         
075000*        -- tillhör LATIN-1 (västeuropeiska språk) som UTF8.              
075100*        -- D.v.s. BASEN-UTF8.                                            
075200*                                                                         
075300*        MOVE NEJ TO CNV-FLUTF8                                           
075400*                                                                         
075500*        IF TEXT-IDSKYLT = 'J  '                                          
075600*          CALL WCNVJAUN USING CNV-WCNVAREA                               
075700*        END-IF                                                           
075800*        IF TEXT-IDSKYLT = 'KOR'                                          
075900*          CALL WCNVKOUN USING CNV-WCNVAREA                               
076000*        END-IF                                                           
076100*        IF TEXT-IDSKYLT = 'RUS'                                          
076200*          CALL WCNVRUUN USING CNV-WCNVAREA                               
076300*        END-IF                                                           
076400*        IF TEXT-IDSKYLT = 'T  '                                          
076500*          CALL WCNVTHUN USING CNV-WCNVAREA                               
076600*        END-IF                                                           
076700*        IF TEXT-IDSKYLT = 'TR '                                          
076800*          CALL WCNVTRUN USING CNV-WCNVAREA                               
076900*        END-IF                                                           
077000*        IF TEXT-IDSKYLT = 'RC '                                          
077100*          CALL WCNVZHUN USING CNV-WCNVAREA                               
077200*        END-IF                                                           
077300       END-IF                                                             
077400                                                                          
077500       IF CNV-KDSVAR NOT = SPACE                                          
077600         PERFORM S32-DISPLAY-KONV-FEL                                     
077700       END-IF                                                             
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 S32-DISPLAY-KONV-FEL SECTION.                                            
078200     SKIP2                                                                
078300     IF CNV-KDSVAR = 'F'                                                  
078400       MOVE 'OTILLÅTET TKN:' TO W-FELTYP                                  
078500     END-IF                                                               
078600     IF CNV-KDSVAR = 'T'                                                  
078700       MOVE 'DATA TRUNKERAT:' TO W-FELTYP                                 
078800     END-IF                                                               
078900     DISPLAY UT04-IDBENNR ' '                                             
079000             TEXT-IDSKYLT ' '                                             
079100             W-FELTYP ' ' CNV-BEFEL                                       
079200     .                                                                    
079300     EJECT                                                                
079400 S99-ABEND SECTION.                                                       
079500                                                                          
079600     SKIP2                                                                
079700     MOVE 'S' TO POSTSUM-OPKOD                                            
079800     CALL POSTSUM USING POSTSUM-PARM                                      
079900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
080000     .                                                                    
080100     EJECT                                                                
080200 Z-FINIT SECTION.                                                         
080300     CLOSE W12114                                                         
080400     CLOSE W159XCP                                                        
080500     CLOSE W15904                                                         
080600     CLOSE W15903                                                         
080700     SKIP2                                                                
080800     MOVE 'S' TO POSTSUM-OPKOD                                            
080900     CALL POSTSUM USING POSTSUM-PARM                                      
081000     .                                                                    
081100     EJECT                                                                
081200     SKIP3                                                                
081300* --- IMS SEKTIONER ---                                                   
081400                                                                          
081500 IMS-GU-WDK601 SECTION.                                                   
081600                                                                          
081700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
081800            DELIMITED BY SIZE INTO SSA1                                   
081900     MOVE '  ' TO GODK-STATUSKODER                                        
082000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
082100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
082200     PERFORM IMS-STATUSKONTROLL                                           
082300     .                                                                    
082400     SKIP3                                                                
082500 IMS-GN-WDD301      SECTION.                                              
082600                                                                          
082700     MOVE 'WDD301  '        TO SSA1                                       
082800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
082900     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-WDD301 SSA1                    
083000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
083100     PERFORM IMS-STATUSKONTROLL                                           
083200     .                                                                    
083300     SKIP2                                                                
083400 IMS-GNP-WDD311-FIRST SECTION.                                            
083500                                                                          
083600     MOVE   'WDD311  *F'        TO SSA1                                   
083700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
083800     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
083900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
084000     PERFORM IMS-STATUSKONTROLL                                           
084100     .                                                                    
084200     EJECT                                                                
084300 IMS-GNP-WDD311 SECTION.                                                  
084400                                                                          
084500     MOVE   'WDD311  '        TO SSA1                                     
084600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
084700     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
084800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
084900     PERFORM IMS-STATUSKONTROLL                                           
085000     .                                                                    
085100     EJECT                                                                
085200 IMS-GNP-WDD312 SECTION.                                                  
085300                                                                          
085400     MOVE   'WDD312  '        TO SSA1                                     
085500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
085600     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD312 SSA1                   
085700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     .                                                                    
086000     EJECT                                                                
086100 IMS-GNP-WDD313 SECTION.                                                  
086200                                                                          
086300     MOVE   'WDD313  '        TO SSA1                                     
086400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
086500     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD313 SSA1                   
086600     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     EJECT                                                                
087000 IMS-STATUSKONTROLL SECTION.                                              
087100                                                                          
087200     SET STATUS-IX TO 1                                                   
087300     SEARCH GODK-STATUS                                                   
087400       AT END                                                             
087500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
087600           DELIMITED BY SIZE INTO FELTEXT                                 
087700         DISPLAY FELTEXT                                                  
087800         CALL FELLOG                                                      
087900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
088000         CONTINUE                                                         
088100     END-SEARCH                                                           
088200     .                                                                    
