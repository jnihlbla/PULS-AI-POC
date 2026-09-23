000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4065200.                                                
000400 AUTHOR.         CAP GEMINI BRA - BOH                                     
000500 DATE-WRITTEN.   DEC   85.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET ÄR ETT UPPDATERINGSPGM.                                   
001100*    UPPDATERING: ANVÄNDS FÖR ATT RAPPORTERA FULL RUTA, OCH               
001200*                 ERHÅLLA EN NY RUTA AV SYSTEMET.                         
001300*    RUTA FULL AVBOKAS OCH FÖRSVINNER NÄR MAN LASTAR UT SISTA             
001400*    KOLLIT PÅ DEN RUTAN. (AVBOKAS AV W403PLAT.)                          
001500*    DET GÅR INTE ATT BOKA AV MANUELLT VIA NÅGON BILD.                    
001600*    NÄR EN RUTA ÄR FULL SÅ SKAPAS DET EN NY SOM ÄR AKTIV.                
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T652                                              
002000*        MID:         W4I65201                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O65201                                            
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77   PROGRAM-NAMN           VALUE 'W4065200'                             
003300                                 PIC X(8).                                
003400 77  JA                          PIC X(1)    VALUE 'J'.                   
003500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003600 77  FAULT                       PIC X(5)    VALUE 'F'.                   
003700 77  OK                          PIC X(5)    VALUE 'O'.                   
003800 77  FULL                        PIC X(4)    VALUE 'FULL'.                
003900 77  INDX                        PIC S9(4)   COMP SYNC.                   
004000 01  WS-IDDC                     PIC X(2).                                
004100 77  WS-ADFLGEO                  PIC X(3).                                
004200 77  WS-ADFLOMR                  PIC X(3).                                
004300 77  WS-ADRUTNIV                 PIC X(3).                                
004400 77  WS-REG-ADCLGEO              PIC X(5).                                
004500 77  WS-REG-ADFLOMR              PIC S9(3)   COMP-3.                      
004600 77  WS-REG-ADRUTNIV             PIC S9(3)   COMP-3.                      
004700 77  WS-REG-IDTRPTNR             PIC 9(3).                                
004800 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +150 COMP SYNC.        
004900 77  WS-IDTRANS                  PIC X(4).                                
005000     88  WS-GODKAEND-BILD                    VALUE '4651' '4652'.         
005100 77    WS-KEYS-TEST              PIC X(01).                               
005200   88  WS-KEYS-WRONG                        VALUE 'F'.                    
005300   88  WS-KEYS-OK                           VALUE 'O'.                    
005400     SKIP2                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI'.             
005700     03 FELLOG                   PIC X(8)    VALUE 'FELLOG'.              
005800     EJECT                                                                
005900     EJECT                                                                
006000 01  WS-TESPAERR.                                                         
006100                                                                          
006200     03  WS-TRPTTEXT             PIC X(4).                                
006300     03  FILLER                  PIC X     VALUE SPACE.                   
006400     03  WS-IDTRPTNR             PIC 9(3).                                
006500     03  FILLER                  PIC X     VALUE SPACE.                   
006600     03  WS-DATUM                PIC 9(6).                                
006700     03  FILLER                  PIC X     VALUE SPACE.                   
006800     03  WS-FULL                 PIC X(4).                                
006900     EJECT                                                                
007000 01  NYCKLAR-TILL-DLI.                                                    
007100                                                                          
007200     03  W-4405-WDGXKEY-X.                                                
007300         05  FILLER              PIC X(4)    VALUE '4405'.                
007400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007500                                                                          
007600     03  W-4406-WDGXKEY-X.                                                
007700         05  W-4406-IDTRPTNR     PIC S9(3)   COMP-3.                      
007800         05  W-4406-IDDC         PIC X(02).                               
007900         05  FILLER              PIC X(6)    VALUE LOW-VALUE.             
008000                                                                          
008100     03  W-4408-WDGXKEY-X.                                                
008200         05  W-4408-ADCLGEO.                                              
008300             07  W-4408-IDDC     PIC X(02).                               
008400             07  W-4408-ADFLGEO  PIC X(3).                                
008500         05  W-4408-ADFLOMR      PIC S9(3)   COMP-3.                      
008600         05  W-4408-ADRUTNIV     PIC S9(3)   COMP-3.                      
008700         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
008800                                                                          
008900     03  W-4411-WDGXKEY-X.                                                
009000         05  FILLER              PIC X(4)    VALUE '4411'.                
009100         05  W-4411-ADCLGEO.                                              
009200             07  W-4411-IDDC     PIC X(02).                               
009300             07  W-4411-ADFLGEO  PIC X(3).                                
009400         05  FILLER              PIC X(21)   VALUE LOW-VALUE.             
009500                                                                          
009600     03  W-4412-WDGXKEY-X.                                                
009700         05  W-4412-ADFLOMR      PIC S9(3)   COMP-3.                      
009800         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
009900                                                                          
010000     03  W-4414-WDGXKEY-X.                                                
010100         05  W-4414-ADRUTNIV     PIC S9(3)   COMP-3.                      
010200         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
010300                                                                          
010400     03  W-4414-TESPAERR-X.                                               
010500         05  W-4414-TESPAERR     PIC X(20)   VALUE SPACE.                 
010600     EJECT                                                                
010700     03  W-SEQ-WDE6C1KY-MIN-X.                                            
010800         05  W-SEQ-IDTRPTNR-MIN  PIC S9(3)   COMP-3.                      
010900         05  W-SEQ-DARFS-MIN     PIC 9(12).                               
011000         05  W-SEQ-ADCLGEO-MIN.                                           
011100             07  W-SEQ-IDDC-MIN      PIC X(02).                           
011200             07  W-SEQ-ADFLGEO-MIN   PIC X(3).                            
011300         05  W-SEQ-ADFLOMR-MIN   PIC S9(3)   COMP-3.                      
011400         05  W-SEQ-ADRUTNIV-MIN  PIC S9(3)   COMP-3.                      
011500         05  FILLER              PIC X(19)   VALUE LOW-VALUE.             
011600                                                                          
011700     03  W-SEQ-WDE6C1KY-MAX-X.                                            
011800         05  W-SEQ-IDTRPTNR-MAX     PIC S9(3)   COMP-3.                   
011900         05  W-SEQ-DARFS-MAX        PIC 9(12).                            
012000         05  W-SEQ-ADCLGEO-MAX.                                           
012100             07  W-SEQ-IDDC-MAX     PIC X(02).                            
012200             07  W-SEQ-ADFLGEO-MAX  PIC X(3).                             
012300         05  W-SEQ-ADFLOMR-MAX      PIC S9(3)   COMP-3.                   
012400         05  W-SEQ-ADRUTNIV-MAX     PIC S9(3)   COMP-3.                   
012500         05  FILLER                 PIC X(19)   VALUE HIGH-VALUE.         
012600     03 W-SEQ-ADCLGEO-X.                                                  
012700        05 W-SEQ-IDDC           PIC X(2).                                 
012800        05 W-SEQ-ADFLGEO        PIC X(3).                                 
012900     03  W-SEQ-ADFLOMR-X.                                                 
013000        05  W-SEQ-ADFLOMR       PIC S9(3) COMP-3.                         
013100     03 W-SEQ-ADRUTNIV-X.                                                 
013200        05  W-SEQ-ADRUTNIV      PIC S9(3) COMP-3.                         
013300     EJECT                                                                
013400 01  MEDDELANDE.                                                          
013500                                                                          
013600     03  FEL-1.                                                           
013700         05  FILLER              PIC X(40)   VALUE                        
013800             '749 FEL NYCKEL                          '.                  
013900         05  FILLER              PIC X(40)   VALUE                        
014000             '749 WRONG KEY                           '.                  
014100     03  FEL-749 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
014200                                                                          
014300     03  FEL-2.                                                           
014400         05  FILLER              PIC X(40)   VALUE                        
014500             '757 INGEN TOM RUTA                      '.                  
014600         05  FILLER              PIC X(40)   VALUE                        
014700             '757 FREE SQUARE MISSING                 '.                  
014800     03  FEL-757 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
014900                                                                          
015000     03  FEL-3.                                                           
015100         05  FILLER              PIC X(40)   VALUE                        
015200             '759 RUTAN REDAN FULL                    '.                  
015300         05  FILLER              PIC X(40)   VALUE                        
015400             '759 SQUARE ALREADY FULL                 '.                  
015500     03  FEL-759 REDEFINES FEL-3 OCCURS 2 PIC X(40).                      
015600                                                                          
015700     03  FEL-4.                                                           
015800         05  FILLER              PIC X(40)   VALUE                        
015900             '825 RUTAN SAKNAR TRANSPORT              '.                  
016000         05  FILLER              PIC X(40)   VALUE                        
016100             '825 NO TRANSPORT ALLOWED TO             '.                  
016200     03  FEL-825 REDEFINES FEL-4 OCCURS 2 PIC X(40).                      
016300                                                                          
016400     03  FEL-5.                                                           
016500         05  FILLER              PIC X(40)   VALUE                        
016600             '831 RUTAN SAKNAR KOLLIN                 '.                  
016700         05  FILLER              PIC X(40)   VALUE                        
016800             '831 SQUARE HAS NO CASSES                '.                  
016900     03  FEL-831 REDEFINES FEL-5 OCCURS 2 PIC X(40).                      
017000     EJECT                                                                
017100 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
017200     SKIP3                                                                
017300 01    FILLER              PIC X(16)   VALUE 'MID W4I652 MID'.            
017400*01  MID -COPY W4I65201.                                                  
017500     EJECT                                                                
017600*01  -COPY WMSGAREA                                                       
017700     EJECT                                                                
017800*    03  MOD -COPY W4O65201  -RED MSG-AREA.                               
017900     EJECT                                                                
018000*01  -COPY WMFSAREA                                                       
018100     EJECT                                                                
018200 01  IMS-WS.                                                              
018300     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
018400     SKIP3                                                                
018500*                        **** STATUS-KOD FRÅN IMS                         
018600     03  STATUS-WS               PIC X(2).                                
018700         88  SEGMENT-FINNS                   VALUE '  '.                  
018800         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
018900     SKIP3                                                                
019000     03  GODK-STATUSKODER.                                                
019100         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
019200     SKIP3                                                                
019300 01  SSA1                        PIC X(192).                              
019400 01  SSA2                        PIC X(64).                               
019500 01  SSA3                        PIC X(64).                               
019600     EJECT                                                                
019700*                            IMS FUNKTIONSKODER                           
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000*                            DLI INPUT-OUTPUT AREA                        
020100 01  DLI-IO-AREA.                                                         
020200     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
020300     SKIP2                                                                
020400*    03  WLXXDN11 -COPY WDGX4406   -RED IO-AREA.                          
020500     EJECT                                                                
020600*    03  WLXXDN21 -COPY WDGX4408   -RED IO-AREA.                          
020700     EJECT                                                                
020800*    03  WLXXDO01 -COPY WDGX4411   -RED IO-AREA.                          
020900     EJECT                                                                
021000*    03  WLXXDO11 -COPY WDGX4412   -RED IO-AREA.                          
021100     EJECT                                                                
021200*    03  WLXXDO21 -COPY WDGX4414   -RED IO-AREA.                          
021300     EJECT                                                                
021400*    03           -COPY WDE6C1     -RED IO-AREA.                          
021500     EJECT                                                                
021600 LINKAGE SECTION.                                                         
021700*01  -COPY W0009     -PRE MSG-                                            
021800     EJECT                                                                
021900*01  -COPY W0008     -PRE WDE6C-                                          
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008     -PRE XXDN-                                           
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008     -PRE XXDO-                                           
022600        05  XXDO-4411-IDHTYP             PIC X(4).                        
022700        05  XXDO-4411-ADCLGEO.                                            
022800            07  XXDO-4411-IDDC           PIC X(2).                        
022900            07  XXDO-4411-ADFLGEO        PIC X(3).                        
023000        05  FILLER                       PIC X(21).                       
023100        05  XXDO-4412-ADFLOMR            PIC S9(3) COMP-3.                
023200        05  FILLER                       PIC X(8).                        
023300        05  XXDO-4414-ADRUTNIV           PIC S9(3) COMP-3.                
023400        05  FILLER                       PIC X(8).                        
023500     EJECT                                                                
023600 PROCEDURE DIVISION USING MSG-PCB WDE6C-PCB XXDN-PCB XXDO-PCB.            
023700     SKIP2                                                                
023800     ENTRY 'DLITCBL' USING MSG-PCB WDE6C-PCB XXDN-PCB XXDO-PCB.           
023900     SKIP2                                                                
024000 STYR SECTION.                                                            
024100     PERFORM IMS-GET-MSG                                                  
024200     SKIP1                                                                
024300     IF SEGMENT-FINNS                                                     
024400         PERFORM A-INIT-SPARA-INPUT                                       
024500         IF MFS-IDTRANS = '4652'                                          
024600             IF  WS-ADFLOMR NUMERIC                                       
024700             AND WS-ADRUTNIV NUMERIC                                      
024800             AND WS-KEYS-OK                                               
024900                 PERFORM B-KOLLA-ID-OCH-SOK-LEDIG-RUTA                    
025000             ELSE                                                         
025100                 MOVE FEL-749(INDX) TO MOD-TEMFSFEL                       
025200             END-IF                                                       
025300         END-IF                                                           
025400         IF NOT WS-GODKAEND-BILD                                          
025500             PERFORM C-RENSA-NYCKLAR                                      
025600         END-IF                                                           
025700         PERFORM   IMS-INSERT-MSG                                         
025800     END-IF                                                               
025900     SKIP1                                                                
026000     MOVE ZERO TO RETURN-CODE                                             
026100     GOBACK                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 A-INIT-SPARA-INPUT SECTION.                                              
026500     SKIP2                                                                
026600     IF MSG-DUBBLA-TRANSKODER                                             
026700         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I65201               
026800         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
026900         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
027000         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
027100         MOVE MSG-IDPFK                     TO MFS-IDPFK                  
027200     ELSE                                                                 
027300         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I65201               
027400         MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                
027500         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
027600         MOVE ' '                           TO MFS-KDTRTYP                
027700                                               MFS-IDPFK                  
027800     END-IF                                                               
027900     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
028000     SKIP1                                                                
028100     MOVE OK                 TO WS-KEYS-TEST                              
028200     IF MID-ADFLGEO-IN = ALL '+'                                          
028300         MOVE MID-ADFLGEO-UT TO WS-ADFLGEO                                
028400     ELSE                                                                 
028500         MOVE MID-ADFLGEO-IN TO WS-ADFLGEO                                
028600     END-IF                                                               
028700     SKIP1                                                                
028800     IF MID-ADFLOMR-IN = ALL '+'                                          
028900         MOVE MID-ADFLOMR-UT TO WS-ADFLOMR                                
029000         INSPECT WS-ADFLOMR REPLACING ALL  SPACE BY ZERO                  
029100     ELSE                                                                 
029200         MOVE MID-ADFLOMR-IN TO WS-ADFLOMR                                
029300     END-IF                                                               
029400     SKIP1                                                                
029500     IF MID-ADRUTNIV-IN = ALL '+'                                         
029600         MOVE MID-ADRUTNIV-UT TO WS-ADRUTNIV                              
029700         INSPECT WS-ADRUTNIV REPLACING ALL  SPACE BY ZERO                 
029800     ELSE                                                                 
029900         MOVE MID-ADRUTNIV-IN TO WS-ADRUTNIV                              
030000     END-IF                                                               
030100     SKIP1                                                                
030200                                                                          
030300     IF SWEDISH-TEXT                                                      
030310         MOVE +1 TO INDX                                                  
030320     ELSE                                                                 
030330         MOVE +2 TO INDX                                                  
030340     END-IF                                                               
030350     SKIP1                                                                
030400     IF MID-IDDC-IN = ALL '+'                                             
030500       IF MID-IDDC-UT = SPACE                                             
030600         MOVE FAULT                       TO WS-KEYS-TEST                 
030700         MOVE FEL-749 (INDX)              TO MOD-TEMFSFEL                 
030800       ELSE                                                               
030900         MOVE MID-IDDC-UT                 TO WS-IDDC                      
031000       END-IF                                                             
031100     ELSE                                                                 
031200       MOVE MID-IDDC-IN                   TO WS-IDDC                      
031300       MOVE '7'                           TO MFS-IDPFK                    
031400       MOVE SPACE                         TO MFS-KDTRTYP                  
031500     END-IF                                                               
031600                                                                          
031700     IF WS-IDDC IS > SPACE                                                
031800       CONTINUE                                                           
031900     ELSE                                                                 
032000       MOVE FAULT                         TO WS-KEYS-TEST                 
032100       MOVE FEL-749 (INDX)                TO MOD-TEMFSFEL                 
032200     END-IF                                                               
032300                                                                          
032400     MOVE LOW-VALUE      TO MSG-AREA                                      
032500     MOVE 'W4O652N1'     TO MFS-IDMOD                                     
032600     MOVE '4652'         TO MOD-IDTRANS                                   
032700     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
032800     SKIP1                                                                
032900     MOVE WS-ADFLGEO TO MOD-ADFLGEO-UT                                    
033000     SKIP1                                                                
033100     MOVE WS-ADFLOMR TO MOD-ADFLOMR-UT                                    
033200     INSPECT MOD-ADFLOMR-UT REPLACING LEADING ZERO BY SPACE               
033300     SKIP1                                                                
033400     MOVE WS-ADRUTNIV TO MOD-ADRUTNIV-UT                                  
033500     INSPECT MOD-ADRUTNIV-UT REPLACING LEADING ZERO BY SPACE              
033600     SKIP1                                                                
033700     MOVE WS-IDDC     TO MOD-IDDC-UT                                      
033800     SKIP1                                                                
034500     MOVE MFS-RENSA-FAELT TO MOD-ADFLGEO-IN                               
034600                             MOD-ADFLOMR-IN                               
034700                             MOD-ADRUTNIV-IN                              
034800                             MOD-IDDC-IN                                  
034900                             MOD-TEMFSFEL                                 
035000                             MOD-TEMFSINF                                 
035100     .                                                                    
035200     EJECT                                                                
035300 B-KOLLA-ID-OCH-SOK-LEDIG-RUTA SECTION.                                   
035400     SKIP2                                                                
035500     MOVE WS-IDDC     TO W-4411-IDDC                                      
035600                         W-SEQ-IDDC                                       
035700     MOVE WS-ADFLGEO  TO W-4411-ADFLGEO                                   
035800                         W-SEQ-ADFLGEO                                    
035900     MOVE WS-ADFLOMR  TO W-4412-ADFLOMR                                   
036000                         W-SEQ-ADFLOMR                                    
036100     MOVE WS-ADRUTNIV TO W-4414-ADRUTNIV                                  
036200                         W-SEQ-ADRUTNIV                                   
036300     SKIP1                                                                
036400     PERFORM IMS-GU-4411                                                  
036500     SKIP1                                                                
036600     IF SEGMENT-SAKNAS                                                    
036700         MOVE FEL-749(INDX) TO MOD-TEMFSFEL                               
036800     ELSE                                                                 
036900         PERFORM IMS-GHNP-RUTA-4412-4414                                  
037000     SKIP1                                                                
037100         IF SEGMENT-SAKNAS                                                
037200             MOVE FEL-749(INDX) TO MOD-TEMFSFEL                           
037300         ELSE                                                             
037400             MOVE RUTA-TESPAERR TO WS-TESPAERR                            
037500             IF WS-FULL = FULL                                            
037600                 MOVE FEL-759(INDX)    TO MOD-TEMFSFEL                    
037700             ELSE                                                         
037800                 IF WS-TRPTTEXT NOT = 'TRPT'                              
037900                     MOVE FEL-825(INDX) TO MOD-TEMFSFEL                   
038000                 ELSE                                                     
038100                     MOVE LOW-VALUE   TO W-SEQ-WDE6C1KY-MIN-X             
038200                     MOVE HIGH-VALUE  TO W-SEQ-WDE6C1KY-MAX-X             
038300                     MOVE WS-IDTRPTNR TO WS-REG-IDTRPTNR                  
038400                                         W-SEQ-IDTRPTNR-MIN               
038500                                         W-SEQ-IDTRPTNR-MAX               
038600                     PERFORM IMS-GN-KOLLI-SEQ                             
038700                     IF SEGMENT-FINNS                                     
038800                       PERFORM BA-SOK-LEDIG-RUTA-INOM-LASTOMR             
038900                     ELSE                                                 
039000                       MOVE FEL-831(INDX) TO MOD-TEMFSFEL                 
039100                     END-IF                                               
039200                 END-IF                                                   
039300             END-IF                                                       
039400         END-IF                                                           
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 BA-SOK-LEDIG-RUTA-INOM-LASTOMR SECTION.                                  
039900     SKIP2                                                                
040000     PERFORM IMS-GHNP-RUTA-4412-FIRST-4414                                
040100     SKIP1                                                                
040200     IF SEGMENT-SAKNAS                                                    
040300         PERFORM BAA-SOK-LEDIG-RUTA-I-GEOG-OMR                            
040400     ELSE                                                                 
040500         PERFORM S01-TESPAERR-TILL-LEDIG-RUTA                             
040600         PERFORM S02-SPARA-NYA-RUTANS-IDENTITET                           
040700         PERFORM S03-MARKERA-RUTA-FULL-I-4414                             
040800         PERFORM S04-UPPDATERA-FLRUTFUL-4408                              
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200 BAA-SOK-LEDIG-RUTA-I-GEOG-OMR SECTION.                                   
041300     SKIP2                                                                
041400     PERFORM IMS-GHNP-RUTA-FIRST-4412-4414                                
041500     SKIP1                                                                
041600     IF SEGMENT-SAKNAS                                                    
041700         MOVE FEL-757(INDX)    TO MOD-TEMFSFEL                            
041800     ELSE                                                                 
041900         PERFORM S01-TESPAERR-TILL-LEDIG-RUTA                             
042000         PERFORM S02-SPARA-NYA-RUTANS-IDENTITET                           
042100         PERFORM S03-MARKERA-RUTA-FULL-I-4414                             
042200         PERFORM S04-UPPDATERA-FLRUTFUL-4408                              
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 C-RENSA-NYCKLAR SECTION.                                                 
042700     MOVE MFS-RENSA-FAELT            TO MOD-ADFLGEO-UT                    
042800                                        MOD-ADFLOMR-UT                    
042900                                        MOD-ADRUTNIV-UT                   
043000     .                                                                    
043100     EJECT                                                                
043200 S01-TESPAERR-TILL-LEDIG-RUTA SECTION.                                    
043300     SKIP2                                                                
043400     ACCEPT WS-DATUM   FROM DATE                                          
043500     MOVE SPACE        TO   WS-FULL                                       
043600     MOVE WS-TESPAERR  TO   RUTA-TESPAERR                                 
043700                            MOD-TESPAERR                                  
043800     PERFORM IMS-REPL-RUTA-4414                                           
043900     .                                                                    
044000     EJECT                                                                
044100 S02-SPARA-NYA-RUTANS-IDENTITET SECTION.                                  
044200     SKIP2                                                                
044300     MOVE XXDO-4411-ADCLGEO  TO WS-REG-ADCLGEO                            
044400     MOVE XXDO-4412-ADFLOMR  TO WS-REG-ADFLOMR                            
044500     MOVE XXDO-4414-ADRUTNIV TO WS-REG-ADRUTNIV                           
044600     .                                                                    
044700     EJECT                                                                
044800 S03-MARKERA-RUTA-FULL-I-4414 SECTION.                                    
044900     SKIP2                                                                
045000     PERFORM IMS-GHNP-4412-4414                                           
045100     SKIP1                                                                
045200     MOVE RUTA-TESPAERR TO WS-TESPAERR                                    
045300     MOVE FULL          TO WS-FULL                                        
045400     MOVE WS-TESPAERR   TO RUTA-TESPAERR                                  
045500     SKIP1                                                                
045600     PERFORM IMS-REPL-RUTA-4414                                           
045700     .                                                                    
045800     EJECT                                                                
045900 S04-UPPDATERA-FLRUTFUL-4408 SECTION.                                     
046000     SKIP2                                                                
046100     MOVE WS-REG-IDTRPTNR  TO W-4406-IDTRPTNR                             
046200     MOVE WS-IDDC          TO W-4406-IDDC                                 
046300                              W-4408-IDDC                                 
046400     MOVE WS-ADFLGEO       TO W-4408-ADFLGEO                              
046500     MOVE WS-ADFLOMR       TO W-4408-ADFLOMR                              
046600     MOVE WS-ADRUTNIV      TO W-4408-ADRUTNIV                             
046700                                                                          
046800     PERFORM IMS-GHU-TRPTRUT-4408                                         
046900     MOVE JA               TO TRPTRUT-FLRUTFUL                            
047000     PERFORM IMS-REPL-TRPTRUT-4408                                        
047100                                                                          
047200     MOVE WS-REG-IDTRPTNR  TO W-4406-IDTRPTNR                             
047300     MOVE WS-REG-ADCLGEO   TO TRPTRUT-ADCLGEO                             
047400     MOVE TRPTRUT-IDDC     TO W-4406-IDDC                                 
047500     MOVE WS-REG-ADFLOMR   TO TRPTRUT-ADFLOMR                             
047600     MOVE WS-REG-ADRUTNIV  TO TRPTRUT-ADRUTNIV                            
047700     MOVE NEJ              TO TRPTRUT-FLRUTFUL                            
047800                                                                          
047900     PERFORM IMS-ISRT-TRPTRUT-4408                                        
048000                                                                          
048100     MOVE TRPTRUT-ADFLOMR  TO MOD-ADFLOMR                                 
048200     MOVE TRPTRUT-ADRUTNIV TO MOD-ADRUTNIV                                
048300                                                                          
048400* IMS SEKTIONER                                                           
048500     SKIP3                                                                
048600     .                                                                    
048700 IMS-GET-MSG SECTION.                                                     
048800     MOVE '  QC' TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING GU                                                
049000                          MSG-PCB                                         
049100                          MSG-IO-AREA                                     
049200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     SKIP3                                                                
049500     .                                                                    
049600 IMS-INSERT-MSG SECTION.                                                  
049700     IF NOT ENGLISH-TEXT                                                  
049800       MOVE '0' TO MFS-KDHUVOMR                                           
049900     END-IF                                                               
050000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050100     MOVE SPACE TO GODK-STATUSKODER                                       
050200     CALL CBLTDLI USING ISRT                                              
050300                          MSG-PCB                                         
050400                          MSG-IO-AREA                                     
050500                          MFS-IDMOD                                       
050600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900     EJECT                                                                
051000 IMS-GU-4411 SECTION.                                                     
051100     STRING 'WLXXDO01(WDGXKEY  =' W-4411-WDGXKEY-X ')'                    
051200            DELIMITED BY SIZE INTO SSA1                                   
051300     MOVE '  GE' TO GODK-STATUSKODER                                      
051400     CALL CBLTDLI USING GU                                                
051500                          XXDO-PCB                                        
051600                          DLI-IO-AREA                                     
051700                          SSA1                                            
051800     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     SKIP3                                                                
052100     .                                                                    
052200 IMS-GHNP-RUTA-4412-4414 SECTION.                                         
052300     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
052400            DELIMITED BY SIZE INTO SSA1                                   
052500     STRING 'WLXXDO21(WDGXKEY  =' W-4414-WDGXKEY-X ')'                    
052600            DELIMITED BY SIZE INTO SSA2                                   
052700     MOVE '  GE' TO GODK-STATUSKODER                                      
052800     CALL CBLTDLI USING GHNP                                              
052900                          XXDO-PCB                                        
053000                          DLI-IO-AREA                                     
053100                          SSA1                                            
053200                          SSA2                                            
053300     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
053400     PERFORM IMS-STATUSKONTROLL                                           
053500     .                                                                    
053600     EJECT                                                                
053700 IMS-GHNP-4412-4414 SECTION.                                              
053800     STRING 'WLXXDO11*F(WDGXKEY  =' W-4412-WDGXKEY-X ')'                  
053900            DELIMITED BY SIZE INTO SSA1                                   
054000     STRING 'WLXXDO21(WDGXKEY  =' W-4414-WDGXKEY-X ')'                    
054100            DELIMITED BY SIZE INTO SSA2                                   
054200     MOVE '  ' TO GODK-STATUSKODER                                        
054300     CALL CBLTDLI USING GHNP                                              
054400                          XXDO-PCB                                        
054500                          DLI-IO-AREA                                     
054600                          SSA1                                            
054700                          SSA2                                            
054800     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
054900     PERFORM IMS-STATUSKONTROLL                                           
055000     SKIP3                                                                
055100     .                                                                    
055200 IMS-GHNP-RUTA-4412-FIRST-4414 SECTION.                                   
055300     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
055400            DELIMITED BY SIZE INTO SSA1                                   
055500     STRING 'WLXXDO21*F(TESPAERR =' W-4414-TESPAERR-X ')'                 
055600            DELIMITED BY SIZE INTO SSA2                                   
055700     MOVE '  GE' TO GODK-STATUSKODER                                      
055800     CALL CBLTDLI USING GHNP                                              
055900                          XXDO-PCB                                        
056000                          DLI-IO-AREA                                     
056100                          SSA1                                            
056200                          SSA2                                            
056300     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     EJECT                                                                
056700 IMS-GHNP-RUTA-FIRST-4412-4414 SECTION.                                   
056800     MOVE 'WLXXDO11*F' TO SSA1                                            
056900     STRING 'WLXXDO21(TESPAERR =' W-4414-TESPAERR-X ')'                   
057000            DELIMITED BY SIZE INTO SSA2                                   
057100     MOVE '  GE' TO GODK-STATUSKODER                                      
057200     CALL CBLTDLI USING GHNP                                              
057300                          XXDO-PCB                                        
057400                          DLI-IO-AREA                                     
057500                          SSA1                                            
057600                          SSA2                                            
057700     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
057800     PERFORM IMS-STATUSKONTROLL                                           
057900     SKIP3                                                                
058000     .                                                                    
058100 IMS-REPL-RUTA-4414 SECTION.                                              
058200     MOVE '  ' TO GODK-STATUSKODER                                        
058300     CALL CBLTDLI USING REPL                                              
058400                          XXDO-PCB                                        
058500                          DLI-IO-AREA                                     
058600     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900     EJECT                                                                
059000 IMS-GHU-TRPTRUT-4408 SECTION.                                            
059100     STRING 'WLXXDN01(WDGXKEY  =' W-4405-WDGXKEY-X ')'                    
059200            DELIMITED BY SIZE INTO SSA1                                   
059300     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
059400            DELIMITED BY SIZE INTO SSA2                                   
059500     STRING 'WLXXDN21(WDGXKEY  =' W-4408-WDGXKEY-X ')'                    
059600            DELIMITED BY SIZE INTO SSA3                                   
059700     MOVE '  ' TO GODK-STATUSKODER                                        
059800     CALL CBLTDLI USING GHU                                               
059900                          XXDN-PCB                                        
060000                          DLI-IO-AREA                                     
060100                          SSA1                                            
060200                          SSA2                                            
060300                          SSA3                                            
060400     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
060500     PERFORM IMS-STATUSKONTROLL                                           
060600     SKIP3                                                                
060700     .                                                                    
060800 IMS-REPL-TRPTRUT-4408 SECTION.                                           
060900     MOVE '  ' TO GODK-STATUSKODER                                        
061000     CALL CBLTDLI USING REPL                                              
061100                          XXDN-PCB                                        
061200                          DLI-IO-AREA                                     
061300     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
061400     PERFORM IMS-STATUSKONTROLL                                           
061500     .                                                                    
061600     EJECT                                                                
061700 IMS-ISRT-TRPTRUT-4408 SECTION.                                           
061800     STRING 'WLXXDN01(WDGXKEY  =' W-4405-WDGXKEY-X ')'                    
061900            DELIMITED BY SIZE INTO SSA1                                   
062000     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
062100            DELIMITED BY SIZE INTO SSA2                                   
062200     MOVE 'WLXXDN21 ' TO SSA3                                             
062300     MOVE '  ' TO GODK-STATUSKODER                                        
062400     CALL CBLTDLI USING ISRT                                              
062500                          XXDN-PCB                                        
062600                          DLI-IO-AREA                                     
062700                          SSA1                                            
062800                          SSA2                                            
062900                          SSA3                                            
063000     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     SKIP3                                                                
063300     .                                                                    
063400 IMS-GN-KOLLI-SEQ SECTION.                                                
063500     STRING 'WDE6C1  (WDE6C1KY>=' W-SEQ-WDE6C1KY-MIN-X                    
063600            '&WDE6C1KY<=' W-SEQ-WDE6C1KY-MAX-X                            
063700            '&ADCLGEO  =' W-SEQ-ADCLGEO-X                                 
063800            '&ADFLOMR  =' W-SEQ-ADFLOMR-X                                 
063900            '&ADRUTNIV =' W-SEQ-ADRUTNIV-X ')'                            
064000            DELIMITED BY SIZE INTO SSA1                                   
064100     MOVE '  GEGP' TO GODK-STATUSKODER                                    
064200     CALL CBLTDLI USING GN                                                
064300                          WDE6C-PCB                                       
064400                          DLI-IO-AREA                                     
064500                          SSA1                                            
064600     MOVE WDE6C-STATUS-CODE TO STATUS-WS                                  
064700     PERFORM IMS-STATUSKONTROLL                                           
064800     .                                                                    
064900     EJECT                                                                
065000 IMS-STATUSKONTROLL SECTION.                                              
065100     SET STATUS-IX TO 1                                                   
065200     SEARCH GODK-STATUS AT END CALL FELLOG                                
065300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
065400     END-SEARCH                                                           
065500     CONTINUE                                                             
065600     .                                                                    
