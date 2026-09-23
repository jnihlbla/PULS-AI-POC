000400 ID DIVISION.                                                             
000500 PROGRAM-ID.                 W2252200.                                    
000900 AUTHOR.                     IDK, GÖTEBORG.                               
001000 DATE-WRITTEN.               NOV 1978.                                    
001010 DATE-COMPILED.                                                           
001100                                                                          
001300*    FUNKTION.                                                            
001400*        LISTNINGSPROGRAM FÖR ÅTERSTÅENDE ORDERRADER.                     
001500*        FILER W22523 SORTERAS PÅ ANSKAFFARNR.                            
001600*        RESTORDERRADER OCH ORDERINGÅNGSRADER FÖR                         
001700*        VECKA 1, INNEVARANDE VECKA OCH TOTALT PERIODEN                   
001800*        SUMMERAS PER GRUPP. VID BRYTNING PÅ GRUPP                        
001900*        BERÄKNAS % = RESTORDERRADER / ORDERINGÅNGSRADER.                 
002000*        VÄRDENA SKRIVS UT MED TOTALT GRUPP SOM LÄGSTA                    
002100*        NIVÅ,MED BRYTNING PÅ SEKTION. SIST KOMMER EN                     
002200*        SIDA MED TOTALT SEKTION SAMT EN GRAND TOTAL.                     
002300*                                                                         
002400*        MODULEN W200ANSK ANROPAS FÖR ATT ERHÅLLA GRUPPER                 
002500*        OCH SEKTIONER MED HJÄLP AV ANSKAFFARNR                           
002600*                                                                         
002700*    RETURKODER.                                                          
002800*        U0020       FEL I SORTERING                                      
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 CONFIGURATION SECTION.                                                   
003300 SPECIAL-NAMES.              C01 IS NYSIDA.                               
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*--------------------------------------- LISTRECORD FÖR                   
003900*                                        ÅTERSTÅENDE ORDERRADER           
004000*                                        INPUT                            
004100     SELECT  W22523  ASSIGN      W22522D1.                                
004200*                                                                         
004300*--------------------------------------- LISTA ÅTERSTÅENDE                
004400*                                        ORDERRADER                       
004500     SELECT  AO-LISTA  ASSIGN      W22522D2.                              
004600*                                                                         
004700*--------------------------------------- SORTERINGSFIL                    
004800*                                                                         
004900     SELECT  SORT-FIL  ASSIGN      W22522DS.                              
005000     EJECT                                                                
005100 DATA DIVISION.                                                           
005200 FILE SECTION.                                                            
005300     SKIP2                                                                
005400 FD  W22523                                                               
005500     RECORDING F                                                          
005600     BLOCK 0                                                              
005700                          .                                               
005800*01  -COPY W225LI05      -L                                               
006000     SKIP2                                                                
006100 FD  AO-LISTA                                                             
006200     RECORDING F                                                          
006300                          .                                               
006400 01  AO-POST.                                                             
006500     05  FILLER              PIC X.                                       
006600     05  AO-RAD              PIC X(120).                                  
006700     SKIP2                                                                
006800 SD  SORT-FIL                                                             
006900                .                                                         
007000*01  POST  -COPY W225LI05    -PRE SORT-                                   
007200     EJECT                                                                
007300 WORKING-STORAGE SECTION.                                                 
007301                                                                          
007310*    -- CHECKED BY WY2000                                                 
007400 77  IDPGM                   PIC X(8)    VALUE 'W2252200'.                
007900 01  W.                                                                   
008000     05  W-PROGNAMN          PIC X(6)    VALUE 'W22522'.                  
008100     05  W-GRUPP-INTERVALL   PIC X(7).                                    
008200     05  W-SPAR-IDSEKT       PIC S9(3)               COMP-3.              
008300     SKIP2                                                                
008400*--------------------------------------- TRANSAKTIONSID                   
008500 01  TID.                                                                 
008600     05  TID-IDGRUPP         PIC X(3).                                    
008700     05  TID-IDSEKT          PIC X(3).                                    
008800     SKIP2                                                                
008900*--------------------------------------- LISTID                           
009000 01  LID.                                                                 
009100     05  LID-IDGRUPP         PIC X(3).                                    
009200     05  LID-IDSEKT          PIC X(3).                                    
009300     SKIP2                                                                
009400 01  INDEKS.                                                              
009500     05  IXSEK               PIC S9(9)               COMP SYNC.           
009600     05  IXLAG               PIC S9(9)               COMP SYNC.           
009700     05  IXHOG               PIC S9(9)               COMP SYNC.           
009800     05  IXCL                PIC S9(9)               COMP SYNC.           
009900     SKIP2                                                                
010000 01  RKOD                    PIC S9(4)   VALUE ZERO  COMP SYNC.           
010100     SKIP2                                                                
010200 01  KONSTANTER.                                                          
010300     05  JA                  PIC X       VALUE 'J'.                       
010400     05  NEJ                 PIC X       VALUE 'N'.                       
010500     SKIP2                                                                
010600 01  SWITCHAR.                                                            
010700     05  SW-SORT-FIL-EOF     PIC X       VALUE 'N'.                       
010800     EJECT                                                                
010900*--------------------------------------- TABELL ÖVER RESTNOTERADE         
011000*                                        RADER OCH ORDERINGÅNG            
011100*                                        PER GRUPP (1, X)                 
011200*                                        PER SEKTION (2-9, X)             
011300*                                        C1-LAGER (X, 1)                  
011400*                                        C2-LAGER (X, 2)                  
011500*                                        TOTALT   (X, 3)                  
011600 01  TRO-TABELL.                                                          
011700     05  TRO-MAX             PIC S9(3)   VALUE +16.                       
011800     05  TRO-SEKTION         OCCURS 16.                                   
011900         10  TRO-CLAGER      OCCURS 3.                                    
012000             15  TRO-KVRORAD-KVAR-V1 PIC S9(7)V99    COMP-3.              
012100             15  TRO-KVRORAD-KVAR-IV PIC S9(7)V99    COMP-3.              
012200             15  TRO-KVRORAD-KVAR-P  PIC S9(7)V99    COMP-3.              
012300             15  TRO-KVINORD-KVAR-V1 PIC S9(7)       COMP-3.              
012400             15  TRO-KVINORD-KVAR-IV PIC S9(7)       COMP-3.              
012500             15  TRO-KVINORD-KVAR-P  PIC S9(7)       COMP-3.              
012600     SKIP3                                                                
012700*--------------------------------------- NOLLPOST FÖR NOLLSTÄLL-          
012800*                                        NING AV TABELL                   
012900 01  TRO-NOLL.                                                            
013000     05  FILLER           PIC S9(7)V99   VALUE ZERO  COMP-3.              
013100     05  FILLER           PIC S9(7)V99   VALUE ZERO  COMP-3.              
013200     05  FILLER           PIC S9(7)V99   VALUE ZERO  COMP-3.              
013300     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
013400     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
013500     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
013600     EJECT                                                                
013700 01  FILLER                  PIC X(16)   VALUE ALL 'A'.                   
013800*--------------------------------------- PARAMETRAR TIL W200ANSK          
013900*01  -COPY W009W42  -PRE ANSK-                                            
014100     EJECT                                                                
014200 01  FILLER                  PIC X(16)   VALUE ALL 'B'.                   
014300*--------------------------------------- AREA FÖR W22523-POST             
014400*01  AREA  -COPY W225LI05    -PRE I23-                                    
014600     EJECT                                                                
014700 01  FILLER                  PIC X(16)   VALUE ALL 'C'.                   
014800 01  DYNAMISKA-SUBPROGRAM.                                                
014900     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
015100     05  ABEND               PIC X(8)    VALUE 'ABEND   '.                
015200     05  W200ANSK            PIC X(8)    VALUE 'W200ANSK'.                
015300     SKIP2                                                                
015400*--------------------------------------- PARAMETRAR TILL DATKORT          
015500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
015600*01  -COPY WDATKORT                                                       
015800     EJECT                                                                
016300*--------------------------------------- ARBETSAREOR TILL LISTAN          
016400*                                        ÅTERSTÅENDE ORDERRADER           
016500 01  AOW.                                                                 
016600     05  AOW-RADSTYR         PIC 9(2)                COMP-3.              
016700     05  AOW-RADANT          PIC S9(3)               COMP-3.              
016800     05  AOW-SIDANT          PIC S9(3)               COMP-3.              
016900     05  AOW-RADMAX          PIC S9(3)   VALUE +40   COMP-3.              
017000     SKIP2                                                                
017100*--------------------------------------- RUBRIKER                         
017200     05  AOW-RUB1.                                                        
017300         10  FILLER          PIC X(14)   VALUE 'VOLVO PARTS'.             
017400         10  FILLER          PIC X(13)   VALUE 'W22522-001'.              
017500         10  FILLER          PIC X(12)   VALUE 'ÅTERSTÅENDE'.             
017600         10  FILLER          PIC X(13)   VALUE 'ORDERRADER'.              
017700         10  AOW-SEKT-TXT.                                                
017800             15  FILLER      PIC X(8).                                    
017900             15  AOW-IDSEKT-RUB1 PIC Z9BB.                                
018000         10  FILLER          PIC X(6)    VALUE 'VECKA'.                   
018100         10  AOW-STATVECKA-RUB1                                           
018200                             PIC Z9.                                      
018300         10  FILLER          PIC X(9)    VALUE SPACE.                     
018400         10  FILLER          PIC X(6)    VALUE 'DATUM'.                   
018500         10  AOW-AAR         PIC X(3).                                    
018600         10  AOW-MAANAD      PIC X(3).                                    
018700         10  AOW-DAG         PIC X(12).                                   
018800         10  FILLER          PIC X(4)    VALUE 'SID'.                     
018900         10  AOW-SIDNR       PIC Z(2)9.                                   
019000     SKIP2                                                                
019100     05  AOW-RUB2.                                                        
019200         10  FILLER          PIC X(5)    VALUE SPACE.                     
019300         10  AOW-GRUPP-TXT   PIC X(9).                                    
019400         10  FILLER          PIC X(8)    VALUE 'CL'.                      
019500         10  FILLER          PIC X(16)   VALUE 'V E C K A  1'.            
019600         10  FILLER          PIC X(10)   VALUE 'V E C K A'.               
019700         10  AOW-STATVECKA-RUB2                                           
019800                             PIC Z9.                                      
019900         10  FILLER          PIC X(6)    VALUE SPACE.                     
020000         10  FILLER          PIC X(13)   VALUE 'V E C K A 1 -'.           
020100         10  AOW-STATVECKA-TOM-RUB2                                       
020200                             PIC Z9.                                      
020300     SKIP2                                                                
020400     05  AOW-RUB3.                                                        
020500         10  FILLER          PIC X(22)   VALUE SPACE.                     
020600         10  FILLER          PIC X(8)    VALUE 'RADER'.                   
020700         10  FILLER          PIC X(8)    VALUE 'PROC'.                    
020800         10  FILLER          PIC X(8)    VALUE 'RADER'.                   
020900         10  FILLER          PIC X(10)   VALUE 'PROC'.                    
021000         10  FILLER          PIC X(11)   VALUE 'RADER'.                   
021100         10  FILLER          PIC X(4)    VALUE 'PROC'.                    
021200     SKIP2                                                                
021300*--------------------------------------- DETALJRAD                        
021400         05  AOW-DETRAD1.                                                 
021500             10  FILLER              PIC X(4).                            
021600             10  AOW-GRUPP-INTERVALL.                                     
021700                 15  FILLER          PIC X(3).                            
021800                 15  AOW-IDSEKT      PIC Z(3)BB.                          
021900             10  FILLER              PIC X(3).                            
022000             10  AOW-CLAGER-TXT      PIC X(5).                            
022100             10  AOW-KVRORAD-KVAR-V1 PIC Z(3)BZ(3).                       
022200             10  AOW-SERVPROC-V1     PIC Z(4)9.9.                         
022300             10  AOW-KVRORAD-KVAR-IV PIC Z(5)BZ(3).                       
022400             10  AOW-SERVPROC-IV     PIC Z(4)9.9.                         
022500             10  AOW-KVRORAD-KVAR-P  PIC Z(7)BZ(3).                       
022600             10  AOW-SERVPROC-P      PIC Z(7)9.9.                         
022700     EJECT                                                                
022800 PROCEDURE DIVISION.                                                      
022900 MAIN SECTION.                                                            
022910                                                                          
023000     SORT SORT-FIL                                                        
023100         ASCENDING SORT-IDANSK                                            
023200         USING W22523                                                     
023300     OUTPUT PROCEDURE HUVUDSTYRDEL                                        
023400     SKIP2                                                                
023500     IF  SORT-RETURN > ZERO                                               
023600       MOVE +20 TO RKOD                                                   
023700       DISPLAY '*** W22522, FEL I SORTERING'                              
023800       CALL ABEND USING RKOD                                              
023900     END-IF                                                               
024000     MOVE ZERO TO RETURN-CODE                                             
024100     GOBACK                                                               
024200     .                                                                    
024300     EJECT                                                                
024400 HUVUDSTYRDEL SECTION.                                                    
024500******************************************************************        
024600*                                                                *        
024700*    H U V U D S T Y R D E L                                     *        
024800*                                                                *        
024900******************************************************************        
025000     SKIP2                                                                
025100     PERFORM A-INITIERING                                                 
025200     PERFORM B-LAS-TRANS                                                  
025300     PERFORM C-HAMTA-GRP-SEKT                                             
025400     SKIP2                                                                
025500     IF TID NOT = HIGH-VALUE                                              
025600       PERFORM UNTIL                                                      
025700        NOT ( SW-SORT-FIL-EOF = NEJ )                                     
025800         MOVE AOW-RADMAX TO AOW-RADANT                                    
025900         MOVE TID-IDSEKT    TO AOW-IDSEKT-RUB1                            
026000                                LID-IDSEKT                                
026100                                W-SPAR-IDSEKT                             
026200         PERFORM UNTIL                                                    
026300          NOT ( TID-IDSEKT = LID-IDSEKT )                                 
026400           MOVE TRO-NOLL TO TRO-CLAGER (1, 1)                             
026500                                TRO-CLAGER (1, 2)                         
026600                                TRO-CLAGER (1, 3)                         
026700           MOVE TID-IDGRUPP TO LID-IDGRUPP                                
026800           MOVE ANSK-GRUPP-INTERVALL TO W-GRUPP-INTERVALL                 
026900           PERFORM UNTIL                                                  
027000            NOT ( TID = LID )                                             
027100             PERFORM D-ADDERA-TILL-TAB                                    
027200             PERFORM B-LAS-TRANS                                          
027300             PERFORM C-HAMTA-GRP-SEKT                                     
027400           END-PERFORM                                                    
027500           MOVE W-GRUPP-INTERVALL TO AOW-GRUPP-INTERVALL                  
027600           MOVE 1 TO IXSEK                                                
027700           PERFORM S03-ADDERA-TOTALT-LAGER                                
027800           PERFORM S01-BERAKNA-SKRIV                                      
027900           MOVE 1 TO IXLAG                                                
028000           ADD W-SPAR-IDSEKT IXLAG GIVING IXHOG                           
028100           PERFORM S02-ADDERA-SEKTION                                     
028200         END-PERFORM                                                      
028300         MOVE 'SEKTION' TO AOW-GRUPP-INTERVALL                            
028400         ADD 1 W-SPAR-IDSEKT GIVING IXSEK                                 
028500         PERFORM S01-BERAKNA-SKRIV                                        
028600       END-PERFORM                                                        
028700       PERFORM E-SKRIV-TOTAL-SIDA                                         
028800     ELSE                                                                 
028900       PERFORM F-SKRIV-TOM-LISTA                                          
029000     END-IF                                                               
029100     PERFORM Z-AVSLUTNING                                                 
029200     .                                                                    
029300     EJECT                                                                
029400 A-INITIERING SECTION.                                                    
029500******************************************************************        
029600*                                                                *        
029700*    ÖPPNA FILER                                                 *        
029900*    HÄMTA INFO FRÅN DATUMKORT                                   *        
030000*    INITIERA ARBETSFÄLT                                         *        
030100*                                                                *        
030200******************************************************************        
030300     SKIP2                                                                
030400     OPEN OUTPUT AO-LISTA                                                 
030500                                                                          
030900     CALL DATKORT USING W-PROGNAMN DATUMKORT-ID DATUMKORT                 
031000                                                                          
031100     MOVE D-AAR TO AOW-AAR                                                
031200     MOVE D-MAANAD TO AOW-MAANAD                                          
031300     MOVE D-DAG TO AOW-DAG                                                
031400     MOVE K-STATVECKA TO AOW-STATVECKA-RUB1                               
031500                         AOW-STATVECKA-RUB2                               
031600                         AOW-STATVECKA-TOM-RUB2                           
031700                                                                          
031800     MOVE 'GRUPP' TO AOW-GRUPP-TXT                                        
031900     MOVE 'SEKTION' TO AOW-SEKT-TXT                                       
032000                                                                          
032100     MOVE ZERO   TO AOW-SIDANT                                            
032200     MOVE SPACE  TO AOW-DETRAD1                                           
032300     MOVE +1 TO IXSEK                                                     
032400                                                                          
032500     PERFORM UNTIL                                                        
032600      ( IXSEK > TRO-MAX )                                                 
032700       MOVE TRO-NOLL   TO TRO-CLAGER (IXSEK, 1)                           
032800                              TRO-CLAGER (IXSEK, 2)                       
032900                              TRO-CLAGER (IXSEK, 3)                       
033000       ADD 1 TO IXSEK                                                     
033100     END-PERFORM                                                          
033200     .                                                                    
033300     EJECT                                                                
033400 B-LAS-TRANS SECTION.                                                     
033500******************************************************************        
033600*                                                                *        
033700*    LÄSER TRANSKTION FRÅN SORTERING                             *        
033800*                                                                *        
033900******************************************************************        
034000     SKIP2                                                                
034100     RETURN SORT-FIL INTO I23-AREA                                        
034200         AT END MOVE JA TO SW-SORT-FIL-EOF                                
034300     END-RETURN                                                           
034400     .                                                                    
034700     EJECT                                                                
034800 C-HAMTA-GRP-SEKT SECTION.                                                
034900******************************************************************        
035000*                                                                *        
035100*    HÄMTA GRUPP OCH SEKTION                                     *        
035200*    BILDA ID-BEGREPP VID EOF FLYTTAS HIGH-VALUE TILL ID                  
035300*                                                                *        
035400******************************************************************        
035500     SKIP2                                                                
035600     IF SW-SORT-FIL-EOF = NEJ                                             
035700       MOVE I23-IDANSK TO ANSK-IDANSK                                     
035800       CALL W200ANSK USING ANSK-W009W42                                   
035900       MOVE ANSK-IDGRUPP TO TID-IDGRUPP                                   
036000       MOVE ANSK-IDSEKT    TO TID-IDSEKT                                  
036100     ELSE                                                                 
036200       MOVE HIGH-VALUE TO TID                                             
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 D-ADDERA-TILL-TAB SECTION.                                               
036700******************************************************************        
036800*                                                                *        
036900*    ADDERA VÄRDE TILL TABELL. LAGER BESTÄMS M.H.A               *        
037000*    KDCLAGER                                                    *        
037100******************************************************************        
037200     SKIP2                                                                
037300     IF  I23-KDCLAGER = 1                                                 
037400       MOVE +1 TO IXCL                                                    
037500       PERFORM DA-ADDERA-CLAGER                                           
037600     ELSE                                                                 
037700       MOVE +2 TO IXCL                                                    
037800       PERFORM DA-ADDERA-CLAGER                                           
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 DA-ADDERA-CLAGER SECTION.                                                
038300******************************************************************        
038400*                                                                *        
038500*    ADDERA IN VÄRDE I TABELLEN                                  *        
038600*    IXCL=1 C1-LAGER, IXCL=2 C2-LAGER                            *        
038700******************************************************************        
038800     SKIP2                                                                
038900     ADD I23-KVRORAD-KVAR-V1 TO TRO-KVRORAD-KVAR-V1 (1, IXCL)             
039000     ADD I23-KVRORAD-KVAR-IV TO TRO-KVRORAD-KVAR-IV (1, IXCL)             
039100     ADD I23-KVRORAD-KVAR-P TO TRO-KVRORAD-KVAR-P   (1, IXCL)             
039200     ADD I23-KVINORD-KVAR-V1 TO TRO-KVINORD-KVAR-V1 (1, IXCL)             
039300     ADD I23-KVINORD-KVAR-IV TO TRO-KVINORD-KVAR-IV (1, IXCL)             
039400     ADD I23-KVINORD-KVAR-P  TO TRO-KVINORD-KVAR-P  (1, IXCL)             
039500     .                                                                    
039600     EJECT                                                                
039700 E-SKRIV-TOTAL-SIDA SECTION.                                              
039800******************************************************************        
039900*                                                                *        
040000*    SKRIVER TOTALSIDA, SOM INNEHÅLLER ALLA SEKTIONER OCH TOTAL  *        
040100*    ADDERA SEKTIONSVÄRDE TILL EN TOTAL                          *        
040200*                                                                *        
040300******************************************************************        
040400     SKIP2                                                                
040500     MOVE TRO-NOLL TO TRO-CLAGER (1 , 1)                                  
040600     TRO-CLAGER (1 , 2)                                                   
040700     TRO-CLAGER (1 , 3)                                                   
040800                                                                          
040900     MOVE 2 TO IXSEK                                                      
041000     MOVE 'SEKTION' TO AOW-GRUPP-TXT                                      
041100     MOVE 'TOTALT'  TO AOW-SEKT-TXT                                       
041200     MOVE AOW-RADMAX TO AOW-RADANT                                        
041300     SKIP2                                                                
041400     PERFORM UNTIL                                                        
041500      ( IXSEK > TRO-MAX )                                                 
041600       SUBTRACT 1 FROM IXSEK GIVING AOW-IDSEKT                            
041700       PERFORM S01-BERAKNA-SKRIV                                          
041800       MOVE IXSEK TO IXLAG                                                
041900       MOVE 1 TO IXHOG                                                    
042000       PERFORM S02-ADDERA-SEKTION                                         
042100       ADD 1 TO IXSEK                                                     
042200     END-PERFORM                                                          
042300     MOVE IXHOG TO IXSEK                                                  
042400     MOVE 'TOTALT' TO AOW-GRUPP-INTERVALL                                 
042500     PERFORM S01-BERAKNA-SKRIV                                            
042600     .                                                                    
042700     EJECT                                                                
042800 F-SKRIV-TOM-LISTA SECTION.                                               
042900******************************************************************        
043000*    MARKERAR TOM LISTA                                          *        
043100*                                                                *        
043200******************************************************************        
043300     SKIP2                                                                
043400     MOVE '*** TOM LISTA, INGA TRANSKTIONER' TO AOW-DETRAD1               
043500     PERFORM S04-STYR-SIDA                                                
043600     .                                                                    
043700     EJECT                                                                
043800 Z-AVSLUTNING SECTION.                                                    
044500                                                                          
044700     CLOSE AO-LISTA                                                       
044800     .                                                                    
044900     EJECT                                                                
045000 S01-BERAKNA-SKRIV SECTION.                                               
045100******************************************************************        
045200*                                                                *        
045300*                                                                *        
045400*    BERÄKNAR SERVICEGRADEN                                      *        
045500*    REDIGERAR LISTAN                                            *        
045600*                                                                *        
045700******************************************************************        
045800                                                                          
045900     MOVE 1 TO IXCL                                                       
046000     MOVE 2 TO AOW-RADSTYR                                                
046100                                                                          
046200     PERFORM UNTIL                                                        
046300      ( IXCL > 3 )                                                        
046400       EVALUATE IXCL                                                      
046500       WHEN 1                                                             
046600         MOVE '1' TO AOW-CLAGER-TXT                                       
046700       WHEN 2                                                             
046800         MOVE '2' TO AOW-CLAGER-TXT                                       
046900       WHEN 3                                                             
047000         MOVE 'T' TO AOW-CLAGER-TXT                                       
047100       END-EVALUATE                                                       
047200       IF TRO-KVINORD-KVAR-V1 (IXSEK, IXCL) = ZERO                        
047300         MOVE ZERO TO AOW-SERVPROC-V1                                     
047400       ELSE                                                               
047500         COMPUTE AOW-SERVPROC-V1 ROUNDED                                  
047600         = TRO-KVRORAD-KVAR-V1 (IXSEK, IXCL)                              
047700         * 100                                                            
047800         / TRO-KVINORD-KVAR-V1 (IXSEK, IXCL)                              
047900       END-IF                                                             
048000       IF TRO-KVINORD-KVAR-IV (IXSEK, IXCL) = ZERO                        
048100         MOVE ZERO TO AOW-SERVPROC-IV                                     
048200       ELSE                                                               
048300         COMPUTE AOW-SERVPROC-IV ROUNDED                                  
048400         = TRO-KVRORAD-KVAR-IV (IXSEK, IXCL)                              
048500         * 100                                                            
048600         / TRO-KVINORD-KVAR-IV (IXSEK, IXCL)                              
048700       END-IF                                                             
048800       IF TRO-KVINORD-KVAR-P (IXSEK, IXCL) = ZERO                         
048900         MOVE ZERO TO AOW-SERVPROC-P                                      
049000       ELSE                                                               
049100         COMPUTE AOW-SERVPROC-P ROUNDED                                   
049200         = TRO-KVRORAD-KVAR-P (IXSEK, IXCL)                               
049300         * 100                                                            
049400         / TRO-KVINORD-KVAR-P (IXSEK, IXCL)                               
049500       END-IF                                                             
049600       MOVE TRO-KVRORAD-KVAR-V1 (IXSEK, IXCL)                             
049700       TO AOW-KVRORAD-KVAR-V1                                             
049800       MOVE TRO-KVRORAD-KVAR-IV (IXSEK, IXCL)                             
049900       TO AOW-KVRORAD-KVAR-IV                                             
050000       MOVE TRO-KVRORAD-KVAR-P (IXSEK, IXCL)                              
050100       TO AOW-KVRORAD-KVAR-P                                              
050200                                                                          
050300       PERFORM S04-STYR-SIDA                                              
050400       ADD 1 TO IXCL                                                      
050500     END-PERFORM                                                          
050600     .                                                                    
050700     EJECT                                                                
050800 S02-ADDERA-SEKTION SECTION.                                              
050900******************************************************************        
051000*                                                                *        
051100*    ADDERA TILL HÖGRE NIVÅ (GRUPP TILL SEKTION RESP SEKTION     *        
051200*    TILL TOTAL)                                                          
051300*                                                                *        
051400******************************************************************        
051500     SKIP2                                                                
051600     MOVE 1 TO IXCL                                                       
051700     PERFORM UNTIL                                                        
051800      ( IXCL > 3 )                                                        
051900       ADD TRO-KVRORAD-KVAR-V1 (IXLAG, IXCL)                              
052000                   TO TRO-KVRORAD-KVAR-V1 (IXHOG, IXCL)                   
052100       ADD TRO-KVRORAD-KVAR-IV (IXLAG, IXCL)                              
052200                   TO TRO-KVRORAD-KVAR-IV (IXHOG, IXCL)                   
052300       ADD TRO-KVRORAD-KVAR-P (IXLAG, IXCL)                               
052400                   TO TRO-KVRORAD-KVAR-P (IXHOG, IXCL)                    
052500                                                                          
052600       ADD TRO-KVINORD-KVAR-V1 (IXLAG, IXCL)                              
052700                   TO TRO-KVINORD-KVAR-V1 (IXHOG, IXCL)                   
052800       ADD TRO-KVINORD-KVAR-IV (IXLAG, IXCL)                              
052900                   TO TRO-KVINORD-KVAR-IV (IXHOG, IXCL)                   
053000       ADD TRO-KVINORD-KVAR-P (IXLAG, IXCL)                               
053100                   TO TRO-KVINORD-KVAR-P (IXHOG, IXCL)                    
053200       ADD 1 TO IXCL                                                      
053300     END-PERFORM                                                          
053400     .                                                                    
053500     EJECT                                                                
053600 S03-ADDERA-TOTALT-LAGER SECTION.                                         
053700******************************************************************        
053800*                                                                *        
053900*    ADDERA VÄRDE FÖR C1-LAGER OCH C2-LAGER TILL TOTALT LAGER    *        
054000*                                                                *        
054100******************************************************************        
054200     SKIP2                                                                
054300     ADD TRO-KVRORAD-KVAR-V1 (IXSEK, 1)                                   
054400         TRO-KVRORAD-KVAR-V1 (IXSEK, 2) GIVING                            
054500         TRO-KVRORAD-KVAR-V1 (IXSEK, 3)                                   
054600     ADD TRO-KVRORAD-KVAR-IV (IXSEK, 1)                                   
054700         TRO-KVRORAD-KVAR-IV (IXSEK, 2) GIVING                            
054800         TRO-KVRORAD-KVAR-IV (IXSEK, 3)                                   
054900     ADD TRO-KVRORAD-KVAR-P (IXSEK, 1)                                    
055000         TRO-KVRORAD-KVAR-P (IXSEK, 2) GIVING                             
055100         TRO-KVRORAD-KVAR-P (IXSEK, 3)                                    
055200     SKIP2                                                                
055300     ADD TRO-KVINORD-KVAR-V1 (IXSEK, 1)                                   
055400         TRO-KVINORD-KVAR-V1 (IXSEK, 2) GIVING                            
055500         TRO-KVINORD-KVAR-V1 (IXSEK, 3)                                   
055600     ADD TRO-KVINORD-KVAR-IV (IXSEK, 1)                                   
055700         TRO-KVINORD-KVAR-IV (IXSEK, 2) GIVING                            
055800         TRO-KVINORD-KVAR-IV (IXSEK, 3)                                   
055900     ADD TRO-KVINORD-KVAR-P (IXSEK, 1)                                    
056000         TRO-KVINORD-KVAR-P (IXSEK, 2) GIVING                             
056100         TRO-KVINORD-KVAR-P (IXSEK, 3)                                    
056200     .                                                                    
056300     EJECT                                                                
056400 S04-STYR-SIDA SECTION.                                                   
056500******************************************************************        
056600*                                                                *        
056700*    STYR SIDA FÖR LISTA ÅTERSTÅENDE ORDERRADER                  *        
056800*                                                                *        
056900******************************************************************        
057000     SKIP2                                                                
057100     IF  AOW-RADANT + AOW-RADSTYR >       AOW-RADMAX                      
057200       ADD +1 TO AOW-SIDANT                                               
057300       MOVE AOW-SIDANT TO AOW-SIDNR                                       
057400       MOVE AOW-RUB1   TO AO-RAD                                          
057500       MOVE ZERO TO AOW-RADSTYR                                           
057600       PERFORM S05-SKRIV-RAD                                              
057700                                                                          
057800       MOVE AOW-RUB2 TO AO-RAD                                            
057900       MOVE 2 TO AOW-RADSTYR                                              
058000       PERFORM S05-SKRIV-RAD                                              
058100                                                                          
058200       MOVE AOW-RUB3 TO AO-RAD                                            
058300       PERFORM S05-SKRIV-RAD                                              
058400       MOVE 2 TO AOW-RADSTYR                                              
058500     END-IF                                                               
058600                                                                          
058700*--------------------------------------- TRYCK DETALJRAD                  
058800     MOVE AOW-DETRAD1 TO AO-RAD                                           
058900     PERFORM S05-SKRIV-RAD                                                
059000     MOVE SPACE TO AOW-DETRAD1                                            
059100     .                                                                    
059200     EJECT                                                                
059300 S05-SKRIV-RAD SECTION.                                                   
059400******************************************************************        
059500*                                                                *        
059600*    SKRIV RAD PÅ LISTA ÅTERSTÅENDE ORDERRADER                   *        
059700*                                                                *        
059800******************************************************************        
059900     SKIP2                                                                
060000     IF AOW-RADSTYR = ZERO                                                
060100       WRITE AO-POST AFTER NYSIDA                                         
060200       MOVE ZERO TO AOW-RADANT                                            
060300     ELSE                                                                 
060400       WRITE AO-POST AFTER AOW-RADSTYR                                    
060500       ADD AOW-RADSTYR TO AOW-RADANT                                      
060600     END-IF                                                               
060700     MOVE 1 TO AOW-RADSTYR                                                
060800     .                                                                    
