000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1152700.                                    
000300 AUTHOR.                     GUNNEL ERIKSSON                              
000400 DATE-WRITTEN.               OKTOBER 1988.                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNCTION.                                                            
000800*                                                                         
000900*    MATCHAR W11526(-1) MED W11526(+0),                                   
001000*    UPPDATERAR ALLA FÖRÄNDRINGAR PÅ H-TYPEN 2204                         
001100*    KDPLORS = 50, BETYDER ATT DO/TPO ÄR REGISTRERAD                      
001200*                                                                         
001300*    PROGRAMMET ÄR OMSKRVET TILL BMP.                                     
001400*                                                                         
001500*    ÄNDRING 96-12-20 (97-10-14) ISTÄLLET FÖR DISPLAY AV FEL              
001600*                                SKRIVES EN UTFIL SOM SEDAN SÄNDS         
001700*                                TILL FÖRVALDA MEMOID I JCL               
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SELECT  INFIL-0    ASSIGN      W11527D1.                             
002400*            *** INPUT-FILE ***                                           
002500*                                                                         
002600     SELECT  INFIL-1    ASSIGN      W11527D2.                             
002700*            *** INPUT-FILE ***                                           
002800*                                                                         
002900     SELECT  FELFIL     ASSIGN      W11527D3.                             
003000*            *** ERROR-FILE ***                                           
003100*                                                                         
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  INFIL-0                                                              
003700     LABEL RECORDS STANDARD                                               
003800     RECORDING      F                                                     
003900     BLOCK CONTAINS 0.                                                    
004000                                                                          
004100*01  INPOST-0  -COPY W11526     -L.                                       
004200                                                                          
004300 FD  INFIL-1                                                              
004400     LABEL RECORDS STANDARD                                               
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800*01  INPOST-1  -COPY W11526     -L.                                       
004900     EJECT                                                                
005000                                                                          
005100 FD  FELFIL                                                               
005200     LABEL RECORDS STANDARD                                               
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500                                                                          
005600 01  FELPOST        PIC X(80).                                            
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  PROGRAM-NAMN            PIC X(8)  VALUE 'W1152700'.                  
006300 77  FELTEXT                 PIC X(80) VALUE SPACE.                       
006400 77  W-DIFFERENS             PIC S9(7) VALUE ZERO.                        
006500 77  RAKNARE                 PIC 9(3)  VALUE ZERO.                        
006600 77  W-KVPOST-1              PIC 9(3)  VALUE ZERO.                        
006700 77  W-KVPOST-0              PIC 9(3)  VALUE ZERO.                        
006800*- - - - - - - - - - - - - - - - - DISPLAY FÄLT                           
006900 77  D-IDARTNR               PIC 9(9)  VALUE ZERO.                        
007000 77  D-TIBEHOV               PIC 9(4)  VALUE ZERO.                        
007100 77  D-IN-0-TIBEHOV          PIC 9(4)  VALUE ZERO.                        
007200*- - - - - - - - - - - - - - - - - KONSTANTER.                            
007300 77  MSG-IO-AREA-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
007400 77  MSG-IO-AREA             PIC X(32) VALUE SPACE.                       
007500 77  CHKP-AREA-1-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
007600 77  CHKP-AREA-1             PIC X(32) VALUE SPACE.                       
007700                                                                          
007800 77  DISP-IDARTNR            PIC 9(9)    VALUE ZERO.                      
007900 77  DISP-DIFF               PIC 9(7)    VALUE ZERO.                      
008000 77  MAX-VARDE               PIC S9(15) VALUE                             
008100                                        +999999999999999 COMP-3.          
008200 77  JA                      PIC X       VALUE 'J'.                       
008300 77  NEJ                     PIC X       VALUE 'N'.                       
008400 77  INDX                    PIC S9(9)   VALUE -0 COMP SYNC.              
008500 77  EOF-FIL-1               PIC X.                                       
008600     88    FIL-1-EOF                     VALUE 'J'.                       
008700 77  EOF-FIL-0               PIC X.                                       
008800     88    FIL-0-EOF                     VALUE 'J'.                       
008900                                                                          
009000*- - - - - - - - - - - - - - - - - DYNAMISKA-SUB-PGM                      
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009300     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
009400     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
009500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
009600     03  W411ARTM            PIC X(8)    VALUE 'W411ARTM'.                
009700     EJECT                                                                
009800*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
009900*01  POST              -COPY W11526     -PRE IN-0-                        
010000*                                                                         
010100 01  IN-0-GRUPP-POST.                                                     
010200     02  IN-0-GRUPP.                                                      
010300         05  FILLER          PIC S9(9) COMP-3.                            
010400         05  FILLER          PIC S9(1) COMP-3.                            
010500         05  FILLER          PIC S9(5) COMP-3.                            
010600     02  FILLER              PIC S9(7) COMP-3.                            
010700     02  FILLER              PIC S9(7) COMP-3.                            
010800                                                                          
010900*01  POST              -COPY W11526     -PRE IN-1-                        
011000*                                                                         
011100 01  IN-1-GRUPP-POST.                                                     
011200     02  IN-1-GRUPP.                                                      
011300         05  FILLER          PIC S9(9) COMP-3.                            
011400         05  FILLER          PIC S9(1) COMP-3.                            
011500         05  FILLER          PIC S9(5) COMP-3.                            
011600     02  FILLER              PIC S9(7) COMP-3.                            
011700     02  FILLER              PIC S9(7) COMP-3.                            
011800                                                                          
011900*01            -COPY W0005      -PRE POSTSUM-                             
012000     EJECT                                                                
012100 01  FILLER                          PIC X(8) VALUE 'W411ARTM'.           
012200*  -COPY  W411ARTM                                                        
012300     EJECT                                                                
012400 01  IMS-WS.                                                              
012500     03   FILLER                  PIC X(8)   VALUE 'IMS-WS'.              
012600*---------------------NYCKLAR TILL DLI--------------------------          
012700     03 W-WDG3KEY-X.                                                      
012800        05  W-IDHTYP              PIC  X(4)  VALUE  '2203'.               
012900        05  W-IDDC                PIC  X(2)  VALUE  '11'.                 
013000        05  FILLER                PIC  X(24) VALUE LOW-VALUE.             
013100                                                                          
013200     03  W-IDARTNR-X.                                                     
013300       05  W-IDARTNR           PIC S9(9)  COMP-3.                         
013400                                                                          
013500     03  W-DABEHOV-X.                                                     
013600       05  W-DABEHOV          PIC  9(6).                                  
013700                                                                          
013800     03  W-WDGX1155-X.                                                    
013900       05  W-IDHTYP           PIC X(4)    VALUE '1155'.                   
014000       05  W-NYCKEL-VALFRI    PIC X(26)   VALUE LOW-VALUE.                
014100                                                                          
014200*---------------------STATUS-KODER FRÅN IMS---------------------          
014300     03 STATUS-WS   PIC XX.                                               
014400        88  SEGMENT-FINNS         VALUE '  '.                             
014500        88  SEGMENT-SAKNAS        VALUE 'GE'.                             
014600        88  SEGMENT-SLUT          VALUE 'GB'.                             
014700        88  IMS-EJ-OK             VALUE 'XD'.                             
014800                                                                          
014900     03 SSA1                      PIC X(64).                              
015000     03 SSA2                      PIC X(64).                              
015100                                                                          
015200     03 GODK-STATUSKODER.                                                 
015300        05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.             
015400                                                                          
015500     EJECT                                                                
015600*--------------------------------IMS-CALL FUNCTIONER---------             
015700*01            -COPY W0003                                                
015800     EJECT                                                                
015900                                                                          
016000*--------------------------------IMS-COMM-AREA---------------             
016100 01   DLI-IO-AREA-1.                                                      
016200     03   IO-AREA-1         PIC X(100).                                   
016300                                                                          
016400*03   AREA       -COPY WDGX2204     -PRE XXBJ- -RED IO-AREA-1             
016500*                                                                         
016600                                                                          
016700 01   DLI-IO-AREA-2.                                                      
016800     03   IO-AREA-2         PIC X(100).                                   
016900                                                                          
017000*03              -COPY WDK901                  -RED IO-AREA-2             
017100*                                                                         
017200*03              -COPY WDK911                  -RED IO-AREA-2             
017300*                                                                         
017400                                                                          
017500 01   DLI-IO-AREA-3.                                                      
017600     03   IO-AREA-3         PIC X(100).                                   
017700                                                                          
017800*03   AREA       -COPY WDGX01       -PRE XXAA- -RED IO-AREA-3             
017900*                                                                         
018000*03   AREA       -COPY WDGX1156     -PRE XXAA- -RED IO-AREA-3             
018100*                                                                         
018200 LINKAGE SECTION.                                                         
018300*01  -COPY W0009      -PRE  MSG-                                          
018400     EJECT                                                                
018500*      -COPY     W0008           -PRE XXBJ-                               
018600        05   FILLER  PIC X.                                               
018700     EJECT                                                                
018800*      -COPY     W0008           -PRE ARTM-                               
018900        05   FILLER  PIC X.                                               
019000     EJECT                                                                
019100*      -COPY     W0008           -PRE XXAA-                               
019200        05   FILLER  PIC X.                                               
019300     EJECT                                                                
019400 PROCEDURE DIVISION USING MSG-PCB XXBJ-PCB ARTM-PCB XXAA-PCB.             
019500     ENTRY 'DLITCBL' USING  MSG-PCB XXBJ-PCB ARTM-PCB XXAA-PCB.           
019600                                                                          
019700     PERFORM A-INIT                                                       
019800                                                                          
019900     PERFORM S01-LAES-INFIL-1                                             
020000     PERFORM S02-LAES-INFIL-0                                             
020100     PERFORM UNTIL FIL-1-EOF AND FIL-0-EOF                                
020200        IF IN-0-GRUPP = IN-1-GRUPP                                        
020300           PERFORM BA-TESTA-SALDO                                         
020400           PERFORM BB-ANDRA-SALDO-PA-WDK9                                 
020500           PERFORM S02-LAES-INFIL-0                                       
020600           PERFORM S01-LAES-INFIL-1                                       
020700        ELSE                                                              
020800           IF IN-1-GRUPP < IN-0-GRUPP                                     
020900              IF IN-1-SUTPO-EJPB = ZERO                                   
021000                 CONTINUE                                                 
021100              ELSE                                                        
021200                 MOVE IN-1-IDARTNR TO XXBJ-2204-IDARTNR                   
021300                 PERFORM IMS-ISRT-WLXXBJ                                  
021400                 PERFORM BC-TA-BORT-PA-WDK9                               
021500              END-IF                                                      
021600              PERFORM S01-LAES-INFIL-1                                    
021700           ELSE                                                           
021800              IF IN-0-GRUPP < IN-1-GRUPP                                  
021900                 MOVE IN-0-IDARTNR TO XXBJ-2204-IDARTNR                   
022000                 PERFORM IMS-ISRT-WLXXBJ                                  
022100                 PERFORM BD-LAGG-TILL-PA-WDK9                             
022200                 PERFORM S02-LAES-INFIL-0                                 
022300              END-IF                                                      
022400           END-IF                                                         
022500        END-IF                                                            
022600        IF RAKNARE >= 200                                                 
022700          PERFORM C-CHECKPOINT                                            
022800        END-IF                                                            
022900     END-PERFORM                                                          
023000     PERFORM Z-FINIT                                                      
023100     MOVE ZERO               TO RETURN-CODE                               
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     OPEN INPUT INFIL-0                                                   
023800                INFIL-1                                                   
023900     OPEN OUTPUT FELFIL                                                   
024000                                                                          
024100     MOVE NEJ TO EOF-FIL-1                                                
024200                 EOF-FIL-0                                                
024300                                                                          
024400     MOVE +50     TO XXBJ-2204-KDLPORS                                    
024500                                                                          
024600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
024700     PERFORM IMS-RESTART                                                  
024800     MOVE ZERO      TO W-KVPOST-1                                         
024900                       W-KVPOST-0                                         
025000                       RAKNARE                                            
025100                       W-DIFFERENS                                        
025200     PERFORM IMS-LAS-ATERSTART                                            
025300     IF SEGMENT-SAKNAS                                                    
025400       MOVE 1    TO XXAA-1156-KDSEGKEY                                    
025500       MOVE ZERO TO XXAA-1156-KVPOST-1                                    
025600                    XXAA-1156-KVPOST-0                                    
025700                    XXAA-1156-TIUPPDAT                                    
025800                    XXAA-1156-TIUPPTID                                    
025900       PERFORM IMS-ISRT-ATERSTART                                         
026000     END-IF                                                               
026100     IF XXAA-1156-KVPOST-1 > ZERO OR XXAA-1156-KVPOST-0 > ZERO            
026200       PERFORM AA-ATERSTART-EFTER-ABEND                                   
026300     END-IF                                                               
026400                                                                          
026500     .                                                                    
026600     EJECT                                                                
026700 AA-ATERSTART-EFTER-ABEND   SECTION.                                      
026800                                                                          
026900     PERFORM UNTIL W-KVPOST-1 = XXAA-1156-KVPOST-1 OR FIL-1-EOF           
027000       PERFORM S01-LAES-INFIL-1                                           
027100     END-PERFORM                                                          
027200                                                                          
027300     PERFORM UNTIL W-KVPOST-0 = XXAA-1156-KVPOST-0 OR FIL-0-EOF           
027400       PERFORM S02-LAES-INFIL-0                                           
027500     END-PERFORM                                                          
027600                                                                          
027700     .                                                                    
027800     EJECT                                                                
027900 BA-TESTA-SALDO   SECTION.                                                
028000                                                                          
028100     IF IN-1-SUTPO-EJPB <                                                 
028200                        (IN-0-SUTPO-EJPB + IN-0-SUTPO-EJPB-GEN)           
028300        MOVE IN-1-IDARTNR TO XXBJ-2204-IDARTNR                            
028400        PERFORM IMS-ISRT-WLXXBJ                                           
028500        COMPUTE W-DIFFERENS =                                             
028600      (IN-0-SUTPO-EJPB + IN-0-SUTPO-EJPB-GEN) - IN-1-SUTPO-EJPB           
028700        ADD 1 TO RAKNARE                                                  
028800     ELSE                                                                 
028900        IF (IN-0-SUTPO-EJPB + IN-0-SUTPO-EJPB-GEN) <                      
029000                              IN-1-SUTPO-EJPB                             
029100           MOVE IN-0-IDARTNR TO XXBJ-2204-IDARTNR                         
029200           PERFORM IMS-ISRT-WLXXBJ                                        
029300           COMPUTE W-DIFFERENS =                                          
029400           (IN-0-SUTPO-EJPB + IN-0-SUTPO-EJPB-GEN) -                      
029500                              IN-1-SUTPO-EJPB                             
029600           ADD 1 TO RAKNARE                                               
029700        END-IF                                                            
029800     END-IF                                                               
029900                                                                          
030000     .                                                                    
030100     EJECT                                                                
030200 BB-ANDRA-SALDO-PA-WDK9 SECTION.                                          
030300                                                                          
030400     IF IN-1-SUTPO-EJPB NOT =                                             
030500           (IN-0-SUTPO-EJPB + IN-0-SUTPO-EJPB-GEN)                        
030600       MOVE IN-0-IDARTNR TO W-IDARTNR                                     
030700       PERFORM IMS-GHU-ARTM-WDK901                                        
030800*      -- JUSTERAR SUMMAN PÅ ROTSEGMENTET                                 
030900       ADD W-DIFFERENS TO ART-SUTPO-TOT                                   
031000       PERFORM IMS-REPL-ARTM-WDK9                                         
031100                                                                          
031200       MOVE IN-0-TIBEHOV TO W-DABEHOV                                     
031300       IF IN-0-TIBEHOV NOT = ZERO                                         
031400         IF IN-0-TIBEHOV < 5000                                           
031500           MOVE 20         TO W-DABEHOV (1:2)                             
031600         ELSE                                                             
031700           IF IN-0-TIBEHOV < 9999                                         
031800             MOVE 19       TO W-DABEHOV (1:2)                             
031900           ELSE                                                           
032000             MOVE 999999   TO W-DABEHOV                                   
032100           END-IF                                                         
032200         END-IF                                                           
032300       END-IF                                                             
032400       PERFORM IMS-GHNP-ARTM-WDK911                                       
032500       IF SEGMENT-FINNS                                                   
032600         ADD W-DIFFERENS TO ANT-SUTPO-EJPB                                
032700         PERFORM IMS-REPL-ARTM-WDK9                                       
032800         MOVE ZERO TO W-DIFFERENS                                         
032900         ADD 1 TO RAKNARE                                                 
033000       ELSE                                                               
033100         MOVE W-DIFFERENS  TO DISP-DIFF                                   
033200         MOVE W-IDARTNR    TO D-IDARTNR                                   
033300         MOVE IN-0-TIBEHOV TO D-IN-0-TIBEHOV                              
033400         STRING  'BB- WDK911-SGMENT SAKNAS FÖR ' D-IDARTNR                
033500                 ' TIBEHOV=' D-IN-0-TIBEHOV                               
033600                 ' DIFFERENS=' DISP-DIFF                                  
033700         DELIMITED BY SIZE INTO FELTEXT                                   
033800         PERFORM S20-SKRIV-W11527                                         
033900*                                                                         
034000*        -- ÅTERSTÄLL NU ANTAL PÅ ROTEN EFTERSOM BARNET INTE FANNS        
034100         PERFORM IMS-GHU-ARTM-WDK901                                      
034200         SUBTRACT W-DIFFERENS FROM ART-SUTPO-TOT                          
034300*        -- OM W-DIFFERENS ÄR NEGATIV, BLIR DET SÅLEDES ADDERING          
034400         PERFORM IMS-REPL-ARTM-WDK9                                       
034500                                                                          
034600*        -- NEDAN KOD ERSATT AV OVAN 96-12-18 /C.E.                       
034700*        MOVE 'FEL FRÅN W11527 I SECTION BB ' TO FELTEXT                  
034800*        CALL FELLOG                                                      
034900       END-IF                                                             
035000     END-IF                                                               
035100                                                                          
035200     .                                                                    
035300     EJECT                                                                
035400 BC-TA-BORT-PA-WDK9 SECTION.                                              
035500                                                                          
035600     MOVE IN-1-IDARTNR TO W-IDARTNR                                       
035700     PERFORM IMS-GHU-ARTM-WDK901                                          
035800     SUBTRACT IN-1-SUTPO-EJPB FROM ART-SUTPO-TOT                          
035900     PERFORM IMS-REPL-ARTM-WDK9                                           
036000     MOVE IN-1-TIBEHOV     TO W-DABEHOV                                   
036100     IF IN-1-TIBEHOV NOT = ZERO                                           
036200       IF IN-1-TIBEHOV < 5000                                             
036300         MOVE 20           TO W-DABEHOV (1:2)                             
036400       ELSE                                                               
036500         IF IN-1-TIBEHOV < 9999                                           
036600           MOVE 19         TO W-DABEHOV (1:2)                             
036700         ELSE                                                             
036800           MOVE 999999     TO W-DABEHOV                                   
036900         END-IF                                                           
037000       END-IF                                                             
037100     END-IF                                                               
037200     PERFORM IMS-GHNP-ARTM-WDK911                                         
037300     IF SEGMENT-FINNS                                                     
037400       SUBTRACT IN-1-SUTPO-EJPB FROM ANT-SUTPO-EJPB                       
037500       IF ANT-SUTPO-EJPB = ZERO AND                                       
037600          ANT-SUTPO-PB = ZERO                                             
037700          PERFORM IMS-DLET-ARTM-WDK9                                      
037800       ELSE                                                               
037900         PERFORM IMS-REPL-ARTM-WDK9                                       
038000       END-IF                                                             
038100       ADD 1 TO RAKNARE                                                   
038200     ELSE                                                                 
038300       MOVE W-IDARTNR TO D-IDARTNR                                        
038400       MOVE W-DABEHOV (3:4) TO D-TIBEHOV                                  
038500       STRING  'BC- WDK911 TPO-BEHOV SAKNAS FÖR ART: ' D-IDARTNR          
038600               ' VECKA: ' D-TIBEHOV ' (FRÅN W11526(-1))'                  
038700       DELIMITED BY SIZE INTO FELTEXT                                     
038800       PERFORM S20-SKRIV-W11527                                           
038900     END-IF                                                               
039000                                                                          
039100                                                                          
039200     .                                                                    
039300     EJECT                                                                
039400 BD-LAGG-TILL-PA-WDK9 SECTION.                                            
039500                                                                          
039600     MOVE IN-0-IDARTNR TO W-IDARTNR                                       
039700     PERFORM IMS-GHU-ARTM-WDK901-GE                                       
039800     IF SEGMENT-SAKNAS                                                    
039900       MOVE W-IDARTNR TO ARTM-IDARTNR-IN                                  
040000       CALL W411ARTM USING ARTM-W411ARTM ARTM-PCB                         
040100       PERFORM IMS-GHU-ARTM-WDK901                                        
040200     END-IF                                                               
040300     ADD IN-0-SUTPO-EJPB     TO ART-SUTPO-TOT                             
040400     ADD IN-0-SUTPO-EJPB-GEN TO ART-SUTPO-TOT                             
040500     PERFORM IMS-REPL-ARTM-WDK9                                           
040600     MOVE IN-0-TIBEHOV       TO W-DABEHOV                                 
040700     IF IN-0-TIBEHOV NOT = ZERO                                           
040800       IF IN-0-TIBEHOV < 5000                                             
040900         MOVE 20             TO W-DABEHOV (1:2)                           
041000       ELSE                                                               
041100         IF IN-0-TIBEHOV < 9999                                           
041200           MOVE 19           TO W-DABEHOV (1:2)                           
041300         ELSE                                                             
041400           MOVE 999999       TO W-DABEHOV                                 
041500         END-IF                                                           
041600       END-IF                                                             
041700     END-IF                                                               
041800     PERFORM IMS-GHNP-ARTM-WDK911                                         
041900     IF SEGMENT-FINNS                                                     
042000       ADD IN-0-SUTPO-EJPB     TO ANT-SUTPO-EJPB                          
042100       ADD IN-0-SUTPO-EJPB-GEN TO ANT-SUTPO-EJPB                          
042200       PERFORM IMS-REPL-ARTM-WDK9                                         
042300     ELSE                                                                 
042400       MOVE IN-0-TIBEHOV       TO ANT-DABEHOV                             
042500       IF IN-0-TIBEHOV NOT = ZERO                                         
042600         IF IN-0-TIBEHOV < 5000                                           
042700           MOVE 20             TO ANT-DABEHOV (1:2)                       
042800         ELSE                                                             
042900           IF IN-0-TIBEHOV < 9999                                         
043000             MOVE 19           TO ANT-DABEHOV (1:2)                       
043100           ELSE                                                           
043200             MOVE 999999       TO ANT-DABEHOV                             
043300           END-IF                                                         
043400         END-IF                                                           
043500       END-IF                                                             
043600       MOVE IN-0-SUTPO-EJPB      TO ANT-SUTPO-EJPB                        
043700       ADD  IN-0-SUTPO-EJPB-GEN  TO ANT-SUTPO-EJPB                        
043800       MOVE ZERO                 TO ANT-SUTPO-PB                          
043900       PERFORM IMS-ISRT-ARTM-WDK9                                         
044000     END-IF                                                               
044100     ADD 2 TO RAKNARE                                                     
044200                                                                          
044300     .                                                                    
044400     EJECT                                                                
044500 C-CHECKPOINT SECTION.                                                    
044600                                                                          
044700     PERFORM IMS-LAS-ATERSTART                                            
044800     MOVE W-KVPOST-1 TO XXAA-1156-KVPOST-1                                
044900     MOVE W-KVPOST-0 TO XXAA-1156-KVPOST-0                                
045000     ACCEPT XXAA-1156-TIUPPDAT FROM DATE                                  
045100     ACCEPT XXAA-1156-TIUPPTID FROM TIME                                  
045200     PERFORM IMS-REPL-ATERSTART                                           
045300     PERFORM IMS-CHECKPOINT                                               
045400     MOVE ZERO TO RAKNARE                                                 
045500                                                                          
045600     .                                                                    
045700     EJECT                                                                
045800 Z-FINIT SECTION.                                                         
045900                                                                          
046000     MOVE 'S' TO POSTSUM-OPKOD                                            
046100     CALL POSTSUM USING POSTSUM-PARM                                      
046200     CLOSE  INFIL-0                                                       
046300            INFIL-1                                                       
046400            FELFIL                                                        
046500     PERFORM IMS-LAS-ATERSTART                                            
046600     MOVE ZERO TO XXAA-1156-KVPOST-1                                      
046700                  XXAA-1156-KVPOST-0                                      
046800     ACCEPT XXAA-1156-TIUPPDAT FROM DATE                                  
046900     ACCEPT XXAA-1156-TIUPPTID FROM TIME                                  
047000     PERFORM IMS-REPL-ATERSTART                                           
047100                                                                          
047200     .                                                                    
047300     EJECT                                                                
047400 S01-LAES-INFIL-1 SECTION.                                                
047500     SKIP1                                                                
047600     READ INFIL-1 INTO IN-1-POST                                          
047700                      AT END MOVE JA TO EOF-FIL-1                         
047800                      MOVE MAX-VARDE  TO IN-1-GRUPP                       
047900     END-READ                                                             
048000     IF FIL-1-EOF                                                         
048100        CONTINUE                                                          
048200     ELSE                                                                 
048300        MOVE IN-1-POST TO IN-1-GRUPP-POST                                 
048400        MOVE 'INFIL1'  TO POSTSUM-FDNAMN                                  
048500        MOVE 'W11527D2'   TO POSTSUM-DDNAMN2                              
048600        MOVE '(-1)'       TO POSTSUM-TRANSTYP                             
048700        CALL POSTSUM USING POSTSUM-PARM                                   
048800        ADD 1 TO W-KVPOST-1                                               
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 S02-LAES-INFIL-0 SECTION.                                                
049300     SKIP1                                                                
049400     READ INFIL-0 INTO IN-0-POST                                          
049500                      AT END MOVE JA TO EOF-FIL-0                         
049600                      MOVE MAX-VARDE  TO IN-0-GRUPP                       
049700     END-READ                                                             
049800     IF FIL-0-EOF                                                         
049900        CONTINUE                                                          
050000     ELSE                                                                 
050100        MOVE IN-0-POST TO IN-0-GRUPP-POST                                 
050200        MOVE 'INFIL0'  TO POSTSUM-FDNAMN                                  
050300        MOVE 'W11527D1'   TO POSTSUM-DDNAMN2                              
050400        MOVE '(+0)'       TO POSTSUM-TRANSTYP                             
050500        CALL POSTSUM USING POSTSUM-PARM                                   
050600        ADD 1 TO W-KVPOST-0                                               
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 S20-SKRIV-W11527 SECTION.                                                
051100                                                                          
051200     WRITE FELPOST FROM FELTEXT                                           
051300                                                                          
051400     MOVE 'FEL'         TO POSTSUM-TRANSTYP                               
051500     MOVE 'W11527'      TO POSTSUM-FDNAMN                                 
051600     MOVE 'W11527D3'    TO POSTSUM-DDNAMN2                                
051700     CALL POSTSUM USING POSTSUM-PARM                                      
051800     .                                                                    
051900                                                                          
052000     EJECT                                                                
052100*    ---- IMS SEKTIONER ----                                              
052200                                                                          
052300 IMS-ISRT-WLXXBJ   SECTION.                                               
052400     SKIP1                                                                
052500     STRING 'WLXXBJ01(WDG3KEY  =' W-WDG3KEY-X ')'                         
052600                         DELIMITED BY SIZE INTO SSA1                      
052700     MOVE   'WLXXBJ11 '   TO SSA2                                         
052800     MOVE '  ' TO GODK-STATUSKODER                                        
052900     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA-1  SSA1 SSA2            
053000     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053400 IMS-GHU-ARTM-WDK901 SECTION.                                             
053500                                                                          
053600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
053700          DELIMITED BY SIZE INTO SSA1                                     
053800     MOVE '  ' TO GODK-STATUSKODER                                        
053900     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-2 SSA1                   
054000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
054100     PERFORM IMS-STATUSKONTROLL                                           
054200     .                                                                    
054300     SKIP3                                                                
054400 IMS-GHU-ARTM-WDK901-GE SECTION.                                          
054500                                                                          
054600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
054700          DELIMITED BY SIZE INTO SSA1                                     
054800     MOVE '  GE' TO GODK-STATUSKODER                                      
054900     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-2 SSA1                   
055000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
055100     PERFORM IMS-STATUSKONTROLL                                           
055200     .                                                                    
055300     SKIP3                                                                
055400 IMS-GHNP-ARTM-WDK911 SECTION.                                            
055500                                                                          
055600     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
055700          DELIMITED BY SIZE INTO SSA1                                     
055800     MOVE '  GE' TO GODK-STATUSKODER                                      
055900     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA-2 SSA1                  
056000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     SKIP3                                                                
056400 IMS-REPL-ARTM-WDK9 SECTION.                                              
056500                                                                          
056600     MOVE '  ' TO GODK-STATUSKODER                                        
056700     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-2                       
056800     MOVE ARTM-STATUS-CODE  TO STATUS-WS                                  
056900     PERFORM IMS-STATUSKONTROLL                                           
057000     .                                                                    
057100     SKIP3                                                                
057200 IMS-DLET-ARTM-WDK9 SECTION.                                              
057300                                                                          
057400     MOVE '  ' TO GODK-STATUSKODER                                        
057500     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-AREA-2                       
057600     MOVE ARTM-STATUS-CODE  TO STATUS-WS                                  
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     SKIP3                                                                
058000 IMS-ISRT-ARTM-WDK9 SECTION.                                              
058100                                                                          
058200     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
058300          DELIMITED BY SIZE INTO SSA1                                     
058400     MOVE 'WLARTM11 ' TO SSA2                                             
058500     MOVE '  ' TO GODK-STATUSKODER                                        
058600     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA-2 SSA1 SSA2             
058700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
058800     PERFORM IMS-STATUSKONTROLL                                           
058900     .                                                                    
059000     EJECT                                                                
059100                                                                          
059200*    ---- IMS CHECKPOINTHANTERING ----                                    
059300                                                                          
059400 IMS-RESTART  SECTION.                                                    
059500                                                                          
059600     MOVE SPACE TO MSG-IO-AREA                                            
059700     MOVE '  '  TO GODK-STATUSKODER                                       
059800     CALL CBLTDLI USING XRST MSG-PCB                                      
059900                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
060000                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
060100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060200     PERFORM IMS-STATUSKONTROLL                                           
060300                                                                          
060400     IF IMS-EJ-OK                                                         
060500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG. IMS-RESTART'              
060600                                                TO FELTEXT                
060700       PERFORM S20-SKRIV-W11527                                           
060800       CALL FELLOG                                                        
060900     END-IF                                                               
061000     .                                                                    
061100     SKIP2                                                                
061200 IMS-CHECKPOINT SECTION.                                                  
061300                                                                          
061400     MOVE PROGRAM-NAMN TO MSG-IO-AREA                                     
061500     MOVE '  XD'       TO GODK-STATUSKODER                                
061600     CALL CBLTDLI USING CHKP MSG-PCB                                      
061700                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
061800                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
061900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     IF IMS-EJ-OK                                                         
062200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG. IMS-CHECKPOINT'           
062300                                                TO FELTEXT                
062400       PERFORM S20-SKRIV-W11527                                           
062500       CALL FELLOG                                                        
062600     END-IF                                                               
062700     .                                                                    
062800     SKIP2                                                                
062900 IMS-LAS-ATERSTART SECTION.                                               
063000                                                                          
063100     STRING 'WLXXAA01(WDGXKEY  =' W-WDGX1155-X ')'                        
063200            DELIMITED BY SIZE INTO SSA1                                   
063300     MOVE 'WLXXAA11' TO SSA2                                              
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GHU XXAA-PCB DLI-IO-AREA-3 SSA1 SSA2              
063600     MOVE XXAA-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     SKIP2                                                                
064000 IMS-REPL-ATERSTART SECTION.                                              
064100                                                                          
064200     MOVE '  '       TO GODK-STATUSKODER                                  
064300     CALL CBLTDLI USING REPL XXAA-PCB DLI-IO-AREA-3                       
064400     MOVE XXAA-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     SKIP2                                                                
064800 IMS-ISRT-ATERSTART SECTION.                                              
064900                                                                          
065000     STRING 'WLXXAA01(WDGXKEY  =' W-WDGX1155-X ')'                        
065100            DELIMITED BY SIZE INTO SSA1                                   
065200     MOVE 'WLXXAA11 ' TO SSA2                                             
065300     MOVE '  ' TO GODK-STATUSKODER                                        
065400     CALL CBLTDLI USING ISRT XXAA-PCB DLI-IO-AREA-3 SSA1 SSA2             
065500     MOVE XXAA-STATUS-CODE TO STATUS-WS                                   
065600     PERFORM IMS-STATUSKONTROLL                                           
065700     .                                                                    
065800     SKIP2                                                                
065900     EJECT                                                                
066000 IMS-STATUSKONTROLL SECTION.                                              
066100                                                                          
066200     SET STATUS-IX TO 1                                                   
066300     SEARCH GODK-STATUS                                                   
066400       AT END                                                             
066500       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
066600       DELIMITED BY SIZE INTO FELTEXT                                     
066700       CALL FELLOG                                                        
066800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066900         CONTINUE                                                         
067000     END-SEARCH                                                           
067100     .                                                                    
067200     EJECT                                                                
