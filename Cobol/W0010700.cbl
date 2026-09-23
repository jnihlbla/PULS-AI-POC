000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0010700.                                                
000300 AUTHOR.         GUNNEL ERIKSSON.                                         
000400 DATE-WRITTEN.   92/12/21.                                                
000500                 OMSKRIVET 000911 BODIL LINDAHL                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    GRUND-FUNKTION:                                                      
000900**       UPPDATERING CROSSINDEX                                           
001000*                                                                         
001100*                                                                         
001200*=====>> OBS KDCMD = '*'                                                  
001300*        PROGRAMMET TILLÅTER DUBBELUPPDATERING AV LEVERANTÖRS-            
001400*        ARTIKEL.                                                         
001500*        DVS DET FINNS MÖJLIGHET ATT KNYTA SAMMA LEVNR/LEVBET             
001600*        TILL TVÅ OLIKA VOLVOARTIKELNUMMER.                               
001700*        DETTA KRÄVER DOCK ATT MAN MATAT IN EN ASTERISK '*',              
001800*        I UPPDATERINGSKODEN .                                            
001900*                                                                         
002000*        PROGRAMMET UPPDATERAR WDF5                                       
002100*        PROGRAMMET LÄSER      WDF5A                                      
002200*        PROGRAMMET LÄSER      WDF5B                                      
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W0T107                                              
002600*                     W0T107U  FRÅN EGET PGM                              
002700*                     W0T107X  FRÅN ANNAT PGM 1116/1192                   
002800*        MID:         W0I10701                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W0O10701                                            
003200     EJECT                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W0010700'.            
004100 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  WS-E-SAMMA                  PIC X       VALUE 'N'.                   
004500 77  UPPDAT-ARTIKEL              PIC X       VALUE 'N'.                   
004600 77  UPPDAT-LEV-LEVBET           PIC X       VALUE 'N'.                   
004700 77  SW-UPPDAT                   PIC X       VALUE 'N'.                   
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-INDX                    PIC S9(4)  VALUE +9    COMP SYNC.        
005000 77  BE-IX                       PIC S9(9)  VALUE +0    COMP SYNC.        
005200 77  WS-IDLEVNR                  PIC X(5)    VALUE space.                 
005300 77  WS-IDARTNR                  PIC X(9)    VALUE ZERO.                  
005400 77  WS-IDARTNR-UP               PIC X(9)    VALUE ZERO.                  
005500 77  WS-IDLEVNR-UP               PIC X(5)    VALUE space.                 
005600 77  WS-BELEVART                 PIC X(30)   VALUE SPACE.                 
005700 77  WS-IDLEVART                 PIC X(30)   VALUE SPACE.                 
005800 77  WS-IDLEVART-MIN             PIC X(30)   VALUE LOW-VALUE.             
005900 77  WS-IDLEVART-MAX             PIC X(30)   VALUE HIGH-VALUE.            
006000 77  WS-BELEVART-UP              PIC X(30)   VALUE LOW-VALUE.             
006100 77  WS-IDLEVART-UP              PIC X(30)   VALUE LOW-VALUE.             
006200 77  WS-IDLEVART-UP-MIN          PIC X(30)   VALUE LOW-VALUE.             
006300 77  WS-IDLEVART-UP-MAX          PIC X(30)   VALUE HIGH-VALUE.            
006400 77  WS-IDBENR                   PIC X(1)    VALUE SPACE.                 
006500 77  WS-IDBENR-NUM               PIC S9(1)   VALUE ZERO.                  
006600                                                                          
006700 01  W009KSIF-FALT.                                                       
006800     03  RESK-IDART              PIC 9(9).                                
006900     03  RESK-9-POS              PIC 9        VALUE 9.                    
007000     03  RESK-W009KSIFR          PIC 9.                                   
007100                                                                          
007200 77  INDATA-SW                   PIC X       VALUE 'N'.                   
007300     88  INDATA-OK                           VALUE 'J'.                   
007400     88  INDATA-FEL                          VALUE 'N'.                   
007500                                                                          
007600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007700     88  NYCKLAR-OK                          VALUE 'J'.                   
007800     88  NYCKLAR-FEL                         VALUE 'N'.                   
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '0107'.                
008200     88  HELP-MID                            VALUE '0551'.                
008300     EJECT                                                                
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERELLA-SUBPROGRAM.                                                
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
009100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009200     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
009300     EJECT                                                                
009400*01      -COPY WMEDAREA                                                   
009500     EJECT                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009800     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
009900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010100     03  ERR-MISSING-IN-REGISTER PIC X(3)    VALUE '010'.                 
010200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010600     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
010700     EJECT                                                                
010800*01      -COPY WDATAREA                                                   
010900     EJECT                                                                
011000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011300*01  MID -COPY W0I10701                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-AREA'.         
011600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011700*01  -COPY WMSGAREA                                                       
011800*                                                                         
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W0O10701                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300*01  -COPY WMFSAREA                                                       
012400     EJECT                                                                
012500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012800*                                                                         
012900 01  NYCKLAR-TILL-DLI.                                                    
013000     03  W-IDARTNR-X.                                                     
013100         05  W-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.                
013200                                                                          
013300     03  W-WDF5KEY-MIN.                                                   
013400         05  W-IDLEVNR-MIN  PIC X(5)    VALUE LOW-VALUE.                  
013500         05  W-IDBENR-MIN   PIC S9(1)   VALUE    ZERO   COMP-3.           
013600     03  W-WDF5KEY-MAX.                                                   
013700         05  W-IDLEVNR-MAX  PIC X(5)    VALUE HIGH-VALUE.                 
013800         05  W-IDBENR-MAX   PIC S9(1)   VALUE      +9   COMP-3.           
013900                                                                          
014000     03  W-WDF5A1KY-MIN.                                                  
014100         05  W-SEQA-IDLEVNR-MIN    PIC X(5)    VALUE LOW-VALUE.           
014200         05  W-SEQA-IDLEVART-MIN   PIC X(30)   VALUE LOW-VALUE.           
014300         05  W-SEQA-IDARTNR-MIN    PIC s9(9)   VALUE ZERO COMP-3.         
014400         05  W-SEQA-IDBENR-MIN     PIC S9(1)   VALUE ZERO COMP-3.         
014500     03  W-WDF5A1KY-MAX.                                                  
014600         05  W-SEQA-IDLEVNR-MAX    PIC X(5)    VALUE HIGH-VALUE.          
014700         05  W-SEQA-IDLEVART-MAX   PIC X(30)   VALUE HIGH-VALUE.          
014800         05  W-SEQA-IDARTNR-MAX    PIC S9(9)   VALUE +999999999           
014900                                                 COMP-3.                  
015000         05  W-SEQA-IDBENR-MAX     PIC S9(1)   VALUE +9 COMP-3.           
015100                                                                          
015200     03  W-WDF5B1KY-MIN.                                                  
015300         05  W-SEQB-IDLEVART-MIN   PIC X(30)   VALUE LOW-VALUE.           
015400         05  W-SEQB-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.         
015500         05  W-SEQB-IDLEVNR-MIN    PIC X(5)    VALUE LOW-VALUE.           
015600         05  W-SEQB-IDBENR-MIN     PIC S9(1)   VALUE ZERO COMP-3.         
015700     03  W-WDF5B1KY-MAX.                                                  
015800         05  W-SEQB-IDLEVART-MAX   PIC X(30)  VALUE HIGH-VALUE.           
015900         05  W-SEQB-IDARTNR-MAX    PIC S9(9)  VALUE +999999999            
016000                                                      COMP-3.             
016100         05  W-SEQB-IDLEVNR-MAX    PIC X(5)   VALUE HIGH-VALUE.           
016200         05  W-SEQB-IDBENR-MAX     PIC S9(1)  VALUE +9 COMP-3.            
016300                                                                          
016400     03  W-SEQB-IDLEVNR-SOEK.                                             
016500         05  W-SEQB-IDLEVNR        PIC X(5)   VALUE SPACE.                
016600     EJECT                                                                
016700*    --- STATUS-KOD FRÅN IMS                                              
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FINNS                       VALUE '  '.                  
017000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017300*                                                                         
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600 01  SSA1                        PIC X(288).                              
017700     EJECT                                                                
017800*    --- IMS FUNKTIONSKODER                                               
017900*01  -COPY W0003                                                          
018000     EJECT                                                                
018100*01  -COPY W009CIA                                                        
018200     EJECT                                                                
018300*    ---  DLI INPUT-OUTPUT AREA                                           
018400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018500 01  DLI-IO-AREA.                                                         
018600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
018700     03  WDF501 REDEFINES IO-AREA.                                        
018800*        05  -COPY WDF501                                                 
018900     EJECT                                                                
019000     03  WDF502 REDEFINES IO-AREA.                                        
019100*        05  -COPY WDF502                                                 
019200     EJECT                                                                
019300 01  DLI-IO-AREA-2.                                                       
019400     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
019500     03  WDF5A1 REDEFINES IO-AREA-2.                                      
019600*        05  -COPY WDF5A1                                                 
019700     EJECT                                                                
019800 01  DLI-IO-AREA-3.                                                       
019900     03  IO-AREA-3               PIC X(150)  VALUE SPACE.                 
020000     03 WDF5B1 REDEFINES IO-AREA-3.                                       
020100*        05  -COPY WDF5B1                                                 
020200     EJECT                                                                
020300 LINKAGE SECTION.                                                         
020400                                                                          
020500*01  -COPY W0009   -PRE MSG-                                              
020600     EJECT                                                                
020700*01  -COPY W0008   -PRE WDF5-                                             
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000*01  -COPY W0008   -PRE WDF5A-                                            
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300*01  -COPY W0008   -PRE WDF5B-                                            
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION  USING MSG-PCB                                        
021700                           WDF5-PCB WDF5A-PCB WDF5B-PCB.                  
021800 MAIN SECTION.                                                            
021900     ENTRY 'DLITCBL' USING MSG-PCB                                        
022000                           WDF5-PCB WDF5A-PCB WDF5B-PCB.                  
022100     PERFORM IMS-GET-MSG                                                  
022200     IF SEGMENT-FINNS                                                     
022300        PERFORM A-INIT                                                    
022400        if mfs-upd-x                                                      
022500           continue                                                       
022600        else                                                              
022700           PERFORM B-KOLLA-NYCKLAR                                        
022800        end-if                                                            
022900        IF NYCKLAR-OK                                                     
023000           MOVE NEJ TO WS-E-SAMMA                                         
023100           IF MFS-UPDATE                                                  
023200           OR MFS-UPD-X                                                   
023300              PERFORM G-KOLLA-INPUT                                       
023400              IF INDATA-OK                                                
023500                 PERFORM H-UPPDATERA                                      
023600              ELSE                                                        
023700*****            IF MFS-UPD-X                                             
023800*****              MOVE 'FEL VID W0T107X TRANS' TO FELTEXT                
023900*****              DISPLAY FELTEXT                                        
024000*****              CALL FELLOG                                            
024100*****            END-IF                                                   
024200                 MOVE JA TO WS-E-SAMMA                                    
024300              END-IF                                                      
024400           ELSE                                                           
024500              IF MFS-FIRST                                                
024600                 PERFORM C-FOERSTA-SIDA                                   
024700              ELSE                                                        
024800                 IF MFS-NEXT                                              
024900                    PERFORM D-NAESTA-SIDA                                 
025000                 ELSE                                                     
025100                    MOVE JA TO WS-E-SAMMA                                 
025200                    PERFORM E-SAMMA-SIDA                                  
025300                 END-IF                                                   
025400              END-IF                                                      
025500           END-IF                                                         
025600                                                                          
025700           IF WS-E-SAMMA = JA                                             
025800              CONTINUE                                                    
025900           ELSE                                                           
026000              PERFORM F-LAES-VISA-INFO                                    
026100           END-IF                                                         
026200        END-IF                                                            
026300                                                                          
026400        IF MFS-UPD-X                                                      
026500           CONTINUE                                                       
026600        ELSE                                                              
026610           MOVE LENGTH OF MOD-W0O10701 TO MSG-KVLL                        
026700           ADD   +4                    TO MSG-KVLL                        
026800           PERFORM IMS-INSERT-MSG                                         
026900        END-IF                                                            
027000     END-IF                                                               
027100                                                                          
027200     MOVE ZERO TO RETURN-CODE                                             
027300     GOBACK                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 A-INIT SECTION.                                                          
027700                                                                          
027800     IF MSG-DUBBLA-TRANSKODER                                             
027900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I10701                 
028000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
028100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
028200     ELSE                                                                 
028300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I10701                 
028400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
028500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
028600     END-IF                                                               
028700                                                                          
028800     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
028900     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
029000     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
029100                                                                          
029200     MOVE LOW-VALUE       TO MSG-AREA                                     
029300     MOVE 'W0O10701'      TO MFS-IDMOD                                    
029400     MOVE '0107'          TO MOD-IDTRANS                                  
029500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029600                                                                          
029700     IF EGEN-MID OR MFS-UPD-X                                             
029800       CONTINUE                                                           
029900     ELSE                                                                 
030000       MOVE SPACE TO MFS-KDTRTYP                                          
030100       MOVE '7'   TO MFS-IDPFK                                            
030200     END-IF                                                               
030300                                                                          
030400     MOVE 'GB '   TO MED-IDSKYLT                                          
030500     .                                                                    
030600     EJECT                                                                
030700 B-KOLLA-NYCKLAR SECTION.                                                 
030800                                                                          
030900     MOVE JA TO NYCKLAR-SW                                                
031000                                                                          
031100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
031200                             MOD-IDLEVNR-IN                               
031300                             MOD-BELEVART-IN                              
031400                                                                          
031500     IF MID-IDARTNR-IN    = ALL '+' OR SPACE OR ZERO                      
031600       IF MID-IDARTNR-UT  = ALL '+' OR SPACE                              
031700          MOVE '000000000'    TO WS-IDARTNR                               
031800       ELSE                                                               
031900          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
032000          INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO              
032100       END-IF                                                             
032200     ELSE                                                                 
032300       MOVE MID-IDARTNR-IN TO WS-IDARTNR                                  
032400       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
032500       MOVE '7'               TO MFS-IDPFK                                
032600       MOVE SPACE             TO MFS-KDTRTYP                              
032700     END-IF                                                               
032800                                                                          
032900     IF WS-IDARTNR NUMERIC                                                
033000        MOVE WS-IDARTNR TO W-IDARTNR                                      
033100     ELSE                                                                 
033200        MOVE NEJ TO NYCKLAR-SW                                            
033300     END-IF                                                               
033400                                                                          
033500     IF MID-IDLEVNR-IN = ALL '+' OR SPACE                                 
033600        MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                 
033700     ELSE                                                                 
033800        MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                 
033900        MOVE '7'            TO MFS-IDPFK                                  
034000        MOVE SPACE          TO MFS-KDTRTYP                                
034100     END-IF                                                               
034200                                                                          
034300     IF WS-IDLEVNR(1:1) = '+' OR '0'                                      
034400*       --- Räcker att kolla i pos 1, där riktigt data ska finnas         
034410        MOVE NEJ TO NYCKLAR-SW                                            
034420     ELSE                                                                 
034500        CONTINUE                                                          
034800     END-IF                                                               
034900                                                                          
035000     IF MID-BELEVART-IN = ALL '+'                                         
035100        MOVE MID-BELEVART-UT TO WS-BELEVART                               
035200     ELSE                                                                 
035300        MOVE MID-BELEVART-IN TO WS-BELEVART                               
035400        MOVE '7'             TO MFS-IDPFK                                 
035500        MOVE SPACE           TO MFS-KDTRTYP                               
035600     END-IF                                                               
035700                                                                          
035800     CALL W009REDU USING WS-BELEVART WS-IDLEVART                          
035900                                                                          
036000     IF WS-BELEVART = SPACE                                               
036100        MOVE HIGH-VALUE        TO WS-IDLEVART-MAX                         
036200        MOVE LOW-VALUE         TO WS-IDLEVART-MIN                         
036300     ELSE                                                                 
036400        MOVE WS-IDLEVART       TO WS-IDLEVART-MIN                         
036500                                  WS-IDLEVART-MAX                         
036600     END-IF                                                               
036700                                                                          
036800     IF EGEN-MID OR NYCKLAR-OK                                            
036900       PERFORM BA-NOLLA-GAMLA-NYCKLAR                                     
037000       MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                           
037100       MOVE WS-IDLEVNR        TO MOD-IDLEVNR-UT                           
037300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
037400       MOVE WS-BELEVART       TO MOD-BELEVART-UT                          
037500     ELSE                                                                 
037600       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                           
037700                                 MOD-IDLEVNR-UT                           
037800                                 MOD-BELEVART-UT                          
037900     END-IF                                                               
038000                                                                          
038100     IF NYCKLAR-FEL                                                       
038200        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
038300        CALL WMEDKONV USING MED-WMEDAREA                                  
038400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
038500        PERFORM MFS-RENSA-FAELT-IN                                        
038600        PERFORM MFS-RENSA-FAELT-UT                                        
038700     END-IF                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 BA-NOLLA-GAMLA-NYCKLAR SECTION.                                          
039100                                                                          
039200     IF EGEN-MID                                                          
039300        IF MID-IDARTNR-IN NOT = ALL '+'                                   
039400           MOVE LOW-VALUE TO WS-BELEVART                                  
039500                             WS-IDLEVART                                  
039600           MOVE SPACE     TO WS-IDLEVNR                                   
039700        ELSE                                                              
039800           IF MID-IDLEVNR-IN  NOT = ALL '+' AND SPACE                     
039900              MOVE ZERO TO WS-IDARTNR                                     
040000           ELSE                                                           
040100              IF MID-BELEVART-IN  NOT = ALL '+'                           
040200                 MOVE ZERO  TO WS-IDARTNR                                 
040300              END-IF                                                      
040400           END-IF                                                         
040500        END-IF                                                            
040600     ELSE                                                                 
040700        IF WS-IDARTNR NOT = ZERO                                          
040800           MOVE LOW-VALUE TO WS-BELEVART                                  
040900                             WS-IDLEVART                                  
041000        END-IF                                                            
041100        IF WS-IDLEVNR(1:1) = ' ' OR '+' OR '0' OR LOW-VALUE               
041110*          -- Pos 1 MÅSTE ha riktigt tecken. (ALFANUM) och ej NOLL        
041120           MOVE SPACE TO WS-IDLEVNR                                       
041130        ELSE                                                              
041200           CONTINUE                                                       
041500        END-IF                                                            
041600        IF WS-BELEVART = ALL '+'                                          
041700           MOVE SPACE TO WS-BELEVART                                      
041800        END-IF                                                            
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 C-FOERSTA-SIDA SECTION.                                                  
042300                                                                          
042400     MOVE WS-IDARTNR        TO  W-IDARTNR                                 
042500     MOVE WS-IDLEVNR        TO  W-IDLEVNR-MIN                             
042600     MOVE WS-IDBENR         TO  W-IDBENR-MIN                              
042700                                                                          
042800     MOVE WS-IDLEVNR        TO  W-SEQA-IDLEVNR-MIN                        
042900                                W-SEQA-IDLEVNR-MAX                        
043000     MOVE WS-IDLEVART-MIN   TO  W-SEQA-IDLEVART-MIN                       
043100     MOVE WS-IDLEVART-MAX   TO  W-SEQA-IDLEVART-MAX                       
043200     MOVE WS-IDARTNR        TO  W-SEQA-IDARTNR-MIN                        
043300     MOVE WS-IDBENR         TO  W-SEQA-IDBENR-MIN                         
043400                                                                          
043500     MOVE WS-IDLEVART-MIN   TO  W-SEQB-IDLEVART-MIN                       
043600     MOVE WS-IDLEVART-MAX   TO  W-SEQB-IDLEVART-MAX                       
043700     MOVE WS-IDLEVNR        TO  W-SEQB-IDLEVNR-MIN                        
043800     MOVE WS-IDARTNR        TO  W-SEQB-IDARTNR-MIN                        
043900     MOVE WS-IDBENR         TO  W-SEQB-IDBENR-MIN                         
044000                                                                          
044100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
044200     CALL WMEDKONV USING MED-WMEDAREA                                     
044300     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
044400                                                                          
044500     PERFORM MFS-RENSA-FAELT-IN                                           
044600     .                                                                    
044700     EJECT                                                                
044800 D-NAESTA-SIDA SECTION.                                                   
044900                                                                          
045000     MOVE MID-IDARTNR-NEXT  TO  W-IDARTNR                                 
045100     MOVE MID-IDLEVNR-NEXT  TO  W-IDLEVNR-MIN                             
045200     MOVE MID-IDBENR-NEXT   TO  W-IDBENR-MIN                              
045300                                                                          
045400     MOVE MID-IDLEVNR-NEXT  TO  W-SEQA-IDLEVNR-MIN                        
045500                                W-SEQA-IDLEVNR-MAX                        
045600     MOVE MID-IDLEVART-NEXT TO  W-SEQA-IDLEVART-MIN                       
045700     MOVE HIGH-VALUE        TO  W-SEQA-IDLEVART-MAX                       
045800                                                                          
045900     MOVE MID-IDARTNR-NEXT  TO  W-SEQA-IDARTNR-MIN                        
046000     MOVE MID-IDBENR-NEXT   TO  W-SEQA-IDBENR-MIN                         
046100                                                                          
046200     MOVE MID-IDLEVART-NEXT TO  W-SEQB-IDLEVART-MIN                       
046300     MOVE WS-IDLEVART-MAX   TO  W-SEQB-IDLEVART-MAX                       
046400     MOVE MID-IDARTNR-NEXT  TO  W-SEQB-IDARTNR-MIN                        
046500     MOVE MID-IDLEVNR-NEXT  TO  W-SEQB-IDLEVNR-MIN                        
046600     MOVE MID-IDBENR-NEXT   TO  W-SEQB-IDBENR-MIN                         
046700                                                                          
046800     PERFORM MFS-RENSA-FAELT-IN                                           
046900     .                                                                    
047000     EJECT                                                                
047100 E-SAMMA-SIDA SECTION.                                                    
047200                                                                          
047300     IF EGEN-MID                                                          
047400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
047500        IF MID-INPUT = ALL '+'                                            
047600           PERFORM MFS-RENSA-FAELT-IN                                     
047700        ELSE                                                              
047800           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
047900           CALL WMEDKONV USING MED-WMEDAREA                               
048000           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
048100           PERFORM EA-MID-INDATA-TILL-MOD                                 
048200        END-IF                                                            
048300     ELSE                                                                 
048400        PERFORM MFS-RENSA-FAELT-IN                                        
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 EA-MID-INDATA-TILL-MOD SECTION.                                          
048900                                                                          
049000     IF  MID-IDARTNR-UP NOT = ALL '+'                                     
049100        MOVE MID-IDARTNR-UP        TO MOD-IDARTNR-UP                      
049200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-UP-ATTR                 
049300     ELSE                                                                 
049400        MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-UP                      
049500     END-IF                                                               
049600                                                                          
049700     IF MID-KDFTAG-UP NOT = ALL '+'                                       
049800        MOVE MID-KDFTAG-UP         TO MOD-KDFTAG-UP                       
049900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFTAG-UP-ATTR                  
050000     ELSE                                                                 
050100        MOVE MFS-RENSA-FAELT       TO MOD-KDFTAG-UP                       
050200     END-IF                                                               
050300                                                                          
050400     IF MID-IDLEVNR-UP NOT = ALL '+' AND SPACE                            
050500        MOVE MID-IDLEVNR-UP        TO MOD-IDLEVNR-UP                      
050600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-UP-ATTR                 
050700     ELSE                                                                 
050800        MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-UP                      
050900     END-IF                                                               
051000                                                                          
051100     IF MID-IDBENR-UP NOT = ALL '+'                                       
051200        MOVE MID-IDBENR-UP         TO MOD-IDBENR-UP                       
051300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDBENR-UP-ATTR                  
051400     ELSE                                                                 
051500        MOVE MFS-RENSA-FAELT       TO MOD-IDBENR-UP                       
051600     END-IF                                                               
051700     EJECT                                                                
051800                                                                          
051900     IF MID-BELEVART-UP NOT = ALL '+'                                     
052000        MOVE MID-BELEVART-UP       TO MOD-BELEVART-UP                     
052100        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
052200                                MOD-BELEVART-UP-ATTR                      
052300     ELSE                                                                 
052400        MOVE MFS-RENSA-FAELT       TO MOD-BELEVART-UP                     
052500     END-IF                                                               
052600                                                                          
052700     IF MID-FLTLVM-UP NOT = ALL '+'                                       
052800        MOVE MID-FLTLVM-UP         TO MOD-FLTLVM-UP                       
052900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTLVM-UP-ATTR                  
053000     ELSE                                                                 
053100        MOVE MFS-RENSA-FAELT       TO MOD-FLTLVM-UP                       
053200     END-IF                                                               
053300                                                                          
053400                                                                          
053500     IF MID-KDCMD-UP NOT = ALL '+'                                        
053600        MOVE MID-KDCMD-UP           TO MOD-KDCMD-UP                       
053700        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCMD-UP-ATTR                  
053800     ELSE                                                                 
053900        MOVE MFS-RENSA-FAELT        TO MOD-KDCMD-UP                       
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 F-LAES-VISA-INFO SECTION.                                                
054400                                                                          
054500     IF MFS-UPD-X                                                         
054600        IF MID-IDARTNR-UP NOT = ALL '+'                                   
054700           MOVE WS-IDARTNR-UP TO W-IDARTNR                                
054800                                 WS-IDARTNR                               
054900        END-IF                                                            
055000     END-IF                                                               
055100                                                                          
055200     IF WS-IDARTNR NOT = ZERO                                             
055300         PERFORM IMS-GHU-WDF501                                           
055400     ELSE                                                                 
055500        IF WS-IDLEVNR  NOT = SPACE AND LOW-VALUE                          
055600           PERFORM IMS-GN-WDF5A1                                          
055700        ELSE                                                              
055800           IF WS-BELEVART > SPACE                                         
055900              PERFORM IMS-GU-WDF5B1-ACTU                                  
056000           ELSE                                                           
056100              IF MID-IDARTNR-UP NOT = ALL '+'                             
056200                 MOVE WS-IDARTNR-UP TO W-IDARTNR                          
056300                                       WS-IDARTNR                         
056400                 PERFORM IMS-GHU-WDF501                                   
056500**** UPPDATERING DIREKT UTAN ATT FYLLA I NYCKLAR                          
056600              END-IF                                                      
056700           END-IF                                                         
056800        END-IF                                                            
056900     END-IF                                                               
057000                                                                          
057100     IF SEGMENT-SAKNAS                                                    
057200        MOVE ERR-MISSING-IN-REGISTER TO MED-IDMFSINF                      
057300        CALL WMEDKONV USING MED-WMEDAREA                                  
057400        MOVE MED-MFSINF TO MOD-TEMFSFEL                                   
057500        PERFORM MFS-RENSA-FAELT-UT                                        
057600     ELSE                                                                 
057700        MOVE +1 TO INDX                                                   
057800        IF WS-IDARTNR NOT = zero                                          
057900           MOVE XART-IDARTNR      TO MOD-IDARTNR(INDX)                    
058000           MOVE XART-KDFTAG       TO MOD-KDFTAG (INDX)                    
058100           MOVE MFS-RENSA-FAELT   TO MOD-TIV-UPPDAT-RAD5                  
058200                                     MOD-KDFTAG-RAD5                      
058300           PERFORM FA-LAS-WDF5-RADDATA                                    
058400        ELSE                                                              
058500           IF WS-IDLEVNR NOT = SPACE AND LOW-VALUE                        
058600              PERFORM FB-LAS-WDF5A-RADDATA                                
058700           ELSE                                                           
058800              IF WS-BELEVART > SPACE                                      
058900                 PERFORM FC-LAS-WDF5B-RADDATA                             
059000              END-IF                                                      
059100           END-IF                                                         
059200        END-IF                                                            
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 FA-LAS-WDF5-RADDATA SECTION.                                             
059700                                                                          
059800     PERFORM IMS-GHNP-WDF502                                              
059900     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
060000***    PERFORM IMS-GHNP-WDF502                                            
060100       IF SEGMENT-FINNS                                                   
060200          IF INDX > +1                                                    
060300             MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR   (INDX)                
060400                                      MOD-KDFTAG    (INDX)                
060500          END-IF                                                          
060600          MOVE   XLEV-IDLEVNR      TO MOD-IDLEVNR   (INDX)                
060700          MOVE   XLEV-IDBENR       TO MOD-IDBENR    (INDX)                
060800          MOVE   XLEV-BELEVART     TO MOD-BELEVART  (INDX)                
060900          MOVE   XLEV-FLTLVM       TO MOD-FLTLVM    (INDX)                
061000       ELSE                                                               
061100          MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR   (INDX)                
061200                                      MOD-KDFTAG    (INDX)                
061300                                      MOD-IDLEVNR   (INDX)                
061400                                      MOD-IDBENR    (INDX)                
061500                                      MOD-BELEVART  (INDX)                
061600                                      MOD-FLTLVM    (INDX)                
061700       END-IF                                                             
061800       ADD 1 TO INDX                                                      
061900       PERFORM IMS-GHNP-WDF502                                            
062000     END-PERFORM                                                          
062100                                                                          
062200     IF SEGMENT-FINNS                                                     
062300        MOVE W-IDARTNR            TO MOD-IDARTNR-NEXT                     
062400        MOVE XLEV-IDLEVNR         TO MOD-IDLEVNR-NEXT                     
062500        MOVE XLEV-IDLEVART        TO MOD-IDLEVART-NEXT                    
062600        MOVE XLEV-IDBENR          TO MOD-IDBENR-NEXT                      
062700        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
062800        CALL WMEDKONV USING MED-WMEDAREA                                  
062900        MOVE MED-MFSINF         TO MOD-TEMFSINF                           
063000      ELSE                                                                
063100        MOVE WS-IDARTNR           TO MOD-IDARTNR-NEXT                     
063200        MOVE WS-IDLEVNR           TO MOD-IDLEVNR-NEXT                     
063300        MOVE ZERO                 TO MOD-IDBENR-NEXT                      
063400        MOVE WS-IDLEVART-MIN      TO MOD-IDLEVART-NEXT                    
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 FB-LAS-WDF5A-RADDATA SECTION.                                            
063900                                                                          
064000     PERFORM UNTIL INDX > MAX-INDX                                        
064100       IF SEGMENT-FINNS                                                   
064200          MOVE SEQA-IDARTNR  TO MOD-IDARTNR   (INDX)                      
064300                                W-IDARTNR                                 
064400          MOVE SEQA-IDLEVNR  TO MOD-IDLEVNR   (INDX)                      
064500                                W-IDLEVNR-MIN                             
064600          MOVE SEQA-IDBENR   TO MOD-IDBENR    (INDX)                      
064700                                W-IDBENR-MIN                              
064800                                                                          
064900          PERFORM IMS-GHU-WDF501-EJGE                                     
065000          MOVE XART-KDFTAG          TO MOD-KDFTAG    (INDX)               
065100                                                                          
065200          PERFORM IMS-GHU-WDF502-UNIK-EJGE                                
065300          MOVE XLEV-FLTLVM          TO MOD-FLTLVM    (INDX)               
065400          MOVE XLEV-BELEVART        TO MOD-BELEVART  (INDX)               
065500       ELSE                                                               
065600          MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR   (INDX)               
065700                                       MOD-KDFTAG    (INDX)               
065800                                       MOD-IDLEVNR   (INDX)               
065900                                       MOD-IDBENR    (INDX)               
066000                                       MOD-BELEVART  (INDX)               
066100                                       MOD-FLTLVM    (INDX)               
066200       END-IF                                                             
066300       ADD 1 TO INDX                                                      
066400       PERFORM IMS-GN-WDF5A1                                              
066500     END-PERFORM                                                          
066600                                                                          
066700     IF SEGMENT-FINNS                                                     
066800        MOVE SEQA-IDARTNR  TO MOD-IDARTNR-NEXT                            
066900        MOVE SEQA-IDLEVNR  TO MOD-IDLEVNR-NEXT                            
067000        MOVE SEQA-IDLEVART TO MOD-IDLEVART-NEXT                           
067100        MOVE SEQA-IDBENR   TO MOD-IDBENR-NEXT                             
067200        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
067300        CALL WMEDKONV USING MED-WMEDAREA                                  
067400        MOVE MED-MFSINF         TO MOD-TEMFSINF                           
067500     ELSE                                                                 
067600        MOVE WS-IDLEVNR           TO MOD-IDLEVNR-NEXT                     
067700        MOVE WS-IDARTNR           TO MOD-IDARTNR-NEXT                     
067800        MOVE ZERO TO                 MOD-IDBENR-NEXT                      
067900        MOVE WS-IDLEVART-MIN      TO MOD-IDLEVART-NEXT                    
068000     END-IF                                                               
068100     .                                                                    
068200     EJECT                                                                
068300 FC-LAS-WDF5B-RADDATA SECTION.                                            
068400                                                                          
068500     PERFORM UNTIL INDX > MAX-INDX                                        
068600       IF SEGMENT-FINNS                                                   
068700          MOVE SEQB-IDARTNR  TO MOD-IDARTNR   (INDX)                      
068800                                W-IDARTNR                                 
068900          MOVE SEQB-IDLEVNR  TO MOD-IDLEVNR   (INDX)                      
069000                                W-IDLEVNR-MIN                             
069100          MOVE SEQB-IDBENR   TO MOD-IDBENR    (INDX)                      
069200                                W-IDBENR-MIN                              
069300                                                                          
069400          PERFORM IMS-GHU-WDF501-EJGE                                     
069500          MOVE XART-KDFTAG          TO MOD-KDFTAG    (INDX)               
069600                                                                          
069700          PERFORM IMS-GHu-WDF502-UNIK-EJGE                                
069800          MOVE XLEV-FLTLVM          TO MOD-FLTLVM    (INDX)               
069900          MOVE XLEV-BELEVART        TO MOD-BELEVART  (INDX)               
070000       ELSE                                                               
070100          MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR   (INDX)               
070200                                       MOD-KDFTAG    (INDX)               
070300                                       MOD-IDLEVNR   (INDX)               
070400                                       MOD-IDBENR    (INDX)               
070500                                       MOD-BELEVART  (INDX)               
070600                                       MOD-FLTLVM    (INDX)               
070700       END-IF                                                             
070800       ADD 1 TO INDX                                                      
070900       PERFORM IMS-GN-WDF5B1                                              
071000     END-PERFORM                                                          
071100                                                                          
071200     IF SEGMENT-FINNS                                                     
071300        MOVE SEQB-IDARTNR           TO MOD-IDARTNR-NEXT                   
071400        MOVE SEQB-IDLEVNR           TO MOD-IDLEVNR-NEXT                   
071500        MOVE SEQB-IDLEVART          TO MOD-IDLEVART-NEXT                  
071600        MOVE SEQB-IDBENR            TO MOD-IDBENR-NEXT                    
071700        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
071800        CALL WMEDKONV USING MED-WMEDAREA                                  
071900        MOVE MED-MFSINF           TO MOD-TEMFSINF                         
072000     ELSE                                                                 
072100        MOVE WS-IDARTNR             TO MOD-IDARTNR-NEXT                   
072200        MOVE WS-IDLEVNR             TO MOD-IDLEVNR-NEXT                   
072300        MOVE ZERO                   TO MOD-IDBENR-NEXT                    
072400        MOVE WS-IDLEVART-MIN        TO MOD-IDLEVART-NEXT                  
072500     END-IF                                                               
072600     .                                                                    
072700     EJECT                                                                
072800 G-KOLLA-INPUT SECTION.                                                   
072900                                                                          
073000     MOVE NEJ TO UPPDAT-ARTIKEL                                           
073100                 UPPDAT-LEV-LEVBET                                        
073200     MOVE JA  TO INDATA-SW                                                
073300                                                                          
073400     IF MID-INPUT = ALL '+'                                               
073500        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
073600        CALL WMEDKONV USING MED-WMEDAREA                                  
073700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
073800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
073900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
074000        MOVE NEJ TO INDATA-SW                                             
074100     ELSE                                                                 
074200        PERFORM GA-KTR-IDART-KDFTAG                                       
074300                                                                          
074400        IF INDATA-FEL                                                     
074500           IF MID-IDLEVNR-UP NOT = ALL '+' AND SPACE                      
074600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-UP-ATTR            
074700           END-IF                                                         
074800           IF MID-IDBENR-UP NOT = ALL '+'                                 
074900              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBENR-UP-ATTR              
075000           END-IF                                                         
075100        END-IF                                                            
075200                                                                          
075300        IF INDATA-OK                                                      
075400           PERFORM GB-KTR-IDLEVNR-IDBENR                                  
075500        END-IF                                                            
075600                                                                          
075700        IF INDATA-FEL                                                     
075800           IF MID-BELEVART-UP NOT = ALL '+'                               
075900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEVART-UP-ATTR           
076000           END-IF                                                         
076100           IF MID-FLTLVM-UP NOT = ALL '+'                                 
076200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTLVM-UP-ATTR             
076300           END-IF                                                         
076400        END-IF                                                            
076500                                                                          
076600        IF INDATA-OK                                                      
076700           PERFORM GC-KTR-BELEVART-FLTLVM                                 
076800        END-IF                                                            
076900                                                                          
077000        IF INDATA-FEL                                                     
077100           IF MID-kdcmd-up NOT = ALL '+'                                  
077200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-kdcmd-UP-ATTR              
077300           END-IF                                                         
077400        END-IF                                                            
077500                                                                          
077600        IF INDATA-OK                                                      
077700           PERFORM GD-KTR-KDCMD                                           
077800        END-IF                                                            
077900                                                                          
078000        IF INDATA-FEL                                                     
078100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
078200           CALL WMEDKONV USING MED-WMEDAREA                               
078300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
078400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
078500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
078600        END-IF                                                            
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000 GA-KTR-IDART-KDFTAG SECTION.                                             
079100                                                                          
079200     IF (MID-IDARTNR-UP NOT = ALL '+' AND SPACE)                          
079300     OR (MID-IDARTNR-UP NOT = ALL '+' AND ZERO)                           
079400        IF MID-IDARTNR-UP = SPACE OR ZERO                                 
079500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UP-ATTR                 
079600                                       MOD-KDFTAG-UP-ATTR                 
079700           MOVE NEJ TO INDATA-SW                                          
079800        ELSE                                                              
079900           MOVE MID-IDARTNR-UP TO WS-IDARTNR-UP                           
080000           INSPECT WS-IDARTNR-UP REPLACING LEADING SPACE BY ZERO          
080100           IF WS-IDARTNR-UP NOT NUMERIC                                   
080200              MOVE NEJ TO INDATA-SW                                       
080300              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UP-ATTR              
080400              MOVE 'UPDATE NOT ALLOWED' TO MOD-TEMFSINF                   
080500              MOVE 'PART NUMBER NOT NUMERIC' TO MOD-TEMFSFEL              
080600           ELSE                                                           
080700              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-UP-ATTR            
080800              MOVE WS-IDARTNR-UP TO W-IDARTNR                             
080900              PERFORM IMS-GHU-WDF501                                      
081000              IF SEGMENT-FINNS                                            
081100                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-UP-ATTR         
081200              ELSE                                                        
081300                 IF MID-KDFTAG-UP = ALL '+'                               
081400                    MOVE NEJ TO INDATA-SW                                 
081500                    MOVE MFS-alfa-FAELT-FEL TO MOD-IDARTNR-UP-ATTR        
081600                                               MOD-KDFTAG-UP-ATTR         
081700                 ELSE                                                     
081800                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-UP-ATTR        
081900                                              MOD-KDFTAG-UP-ATTR          
082000                 END-IF                                                   
082100              END-IF                                                      
082200           END-IF                                                         
082300        END-IF                                                            
082400     END-IF                                                               
082500                                                                          
082600     IF MID-KDFTAG-UP NOT =   ALL '+'                                     
082700        IF MID-KDFTAG-UP NOT NUMERIC                                      
082800        OR (MID-IDARTNR-UP =  ALL '+' OR SPACE OR ZERO)                   
082900           MOVE NEJ                    TO INDATA-SW                       
083000           MOVE MFS-NUM-FAELT-FEL      TO MOD-KDFTAG-UP-ATTR              
083100           IF (MID-IDARTNR-UP = ALL '+' OR SPACE)                         
083200              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UP-ATTR              
083300           END-IF                                                         
083400        ELSE                                                              
083500           IF MID-KDFTAG-UP = '0' OR '1'                                  
083600              MOVE NEJ                 TO INDATA-SW                       
083700              MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFTAG-UP-ATTR              
083800           ELSE                                                           
083900              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFTAG-UP-ATTR              
084000              MOVE JA                  TO UPPDAT-ARTIKEL                  
084100           END-IF                                                         
084200        END-IF                                                            
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600 GB-KTR-IDLEVNR-IDBENR SECTION.                                           
084700                                                                          
084800     IF MID-IDLEVNR-UP NOT = ALL '+'  AND SPACE                           
084900        IF MID-IDLEVNR-UP(1:1) = SPACE OR LOW-VALUE OR '+'                
085000        OR MID-IDBENR-UP  = ALL '+'                                       
085100           MOVE NEJ TO INDATA-SW                                          
085200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-UP-ATTR                 
085300           IF MID-IDBENR-UP = ALL '+'                                     
085400              MOVE MFS-NUM-FAELT-FEL TO MOD-IDBENR-UP-ATTR                
085500           END-IF                                                         
085600        ELSE                                                              
085700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDLEVNR-UP-ATTR              
085800        END-IF                                                            
085900     END-IF                                                               
086000                                                                          
086100     IF MID-IDBENR-UP NOT = ALL '+'                                       
086200        IF MID-IDBENR-UP NOT NUMERIC                                      
086300        OR MID-IDLEVNR-UP = ALL '+' OR SPACE                              
086400           MOVE NEJ                  TO INDATA-SW                         
086500           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDBENR-UP-ATTR                
086600           IF MID-IDLEVNR-UP = ALL '+' OR SPACE                           
086700              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-UP-ATTR              
086800           END-IF                                                         
086900        ELSE                                                              
087000           MOVE JA TO UPPDAT-LEV-LEVBET                                   
087100           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDBENR-UP-ATTR                
087200        END-IF                                                            
087300     END-IF                                                               
087400                                                                          
087500     IF UPPDAT-LEV-LEVBET = JA                                            
087600        IF  MID-IDARTNR-UP = ALL '+'                                      
087700           MOVE  WS-IDARTNR TO W-IDARTNR                                  
087800        ELSE                                                              
087900           MOVE WS-IDARTNR-UP TO W-IDARTNR                                
088000        END-IF                                                            
088100                                                                          
088200        PERFORM IMS-GHU-WDF501                                            
088300        IF SEGMENT-FINNS                                                  
088400           if w-idartnr = 0                                               
088500              MOVE NEJ TO INDATA-SW                                       
088600              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UP-ATTR              
088700                                         MOD-KDFTAG-UP-ATTR               
088800           else                                                           
088900              CONTINUE                                                    
089000           end-if                                                         
089100        ELSE                                                              
089200           IF UPPDAT-ARTIKEL = JA                                         
089300              CONTINUE                                                    
089400           ELSE                                                           
089500              MOVE NEJ TO INDATA-SW                                       
089600              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-UP-ATTR              
089700                                         MOD-KDFTAG-UP-ATTR               
089800           END-IF                                                         
089900        END-IF                                                            
090000     END-IF                                                               
090100     .                                                                    
090200     EJECT                                                                
090300 GC-KTR-BELEVART-FLTLVM  SECTION.                                         
090400                                                                          
090500     IF MID-BELEVART-UP NOT = ALL '+'                                     
090600        IF UPPDAT-LEV-LEVBET = JA                                         
090700           IF MID-KDCMD-UP = 'D'                                          
090800              CONTINUE                                                    
090900           ELSE                                                           
091000              MOVE MID-BELEVART-UP TO WS-BELEVART                         
091100                                                                          
091200              CALL W009REDU   USING   WS-BELEVART                         
091300                                      WS-IDLEVART                         
091400                                                                          
091500              MOVE WS-IDLEVART     TO WS-IDLEVART-UP                      
091600              MOVE WS-IDLEVART-UP  TO W-SEQB-IDLEVART-MIN                 
091700                                      W-SEQB-IDLEVART-MAX                 
091800              IF  MID-IDARTNR-UP  NOT = ALL '+'                           
091900                 INSPECT MID-IDARTNR-UP REPLACING                         
092000                         LEADING SPACE BY ZERO                            
092100                 MOVE MID-IDARTNR-UP TO W-SEQB-IDARTNR-MIN                
092200                                        W-SEQB-IDARTNR-MAX                
092300              ELSE                                                        
092400                 MOVE WS-IDARTNR    TO W-SEQB-IDARTNR-MIN                 
092500                                       W-SEQB-IDARTNR-MAX                 
092600              END-IF                                                      
092700              MOVE MID-IDLEVNR-UP   TO W-SEQB-IDLEVNR                     
092800                                                                          
092900****  ENDAST IDBENR MIN/MAX VÄRDEN                                        
093000              PERFORM GCA-KTR-BELEVART-DUBELTT                            
093100                                                                          
093200           END-IF                                                         
093300        ELSE                                                              
093400           MOVE NEJ TO INDATA-SW                                          
093500           MOVE MFS-ALFA-FAELT-FEL  TO MOD-BELEVART-UP-ATTR               
093600                                       MOD-IDLEVNR-UP-ATTR                
093700           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDBENR-UP-ATTR                 
093800        END-IF                                                            
093900     ELSE                                                                 
094000        IF UPPDAT-LEV-LEVBET = JA                                         
094100           PERFORM IMS-GHU-WDF501                                         
094200           IF SEGMENT-FINNS                                               
094300              MOVE MID-IDLEVNR-UP       TO W-IDLEVNR-MIN                  
094400              MOVE MID-IDBENR-UP        TO W-IDBENR-MIN                   
094500              PERFORM IMS-GHU-WDF502-UNIK                                 
094600              IF SEGMENT-FINNS                                            
094700                 MOVE MFS-ALFA-FAELT-RAETT                                
094800                                        TO MOD-BELEVART-UP-ATTR           
094900              ELSE                                                        
095000                 MOVE NEJ TO INDATA-SW                                    
095100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-UP-ATTR          
095200                                            MOD-IDLEVNR-UP-ATTR           
095300                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDBENR-UP-ATTR             
095400              END-IF                                                      
095500           ELSE                                                           
095600              MOVE NEJ TO INDATA-SW                                       
095700              MOVE MFS-ALFA-FAELT-FEL   TO MOD-BELEVART-UP-ATTR           
095800           END-IF                                                         
095900        END-IF                                                            
096000     END-IF                                                               
096100                                                                          
096200     IF MID-FLTLVM-UP NOT = ALL '+'                                       
096300        IF UPPDAT-LEV-LEVBET = JA                                         
096400           IF MID-FLTLVM-UP = 'J' OR 'N'                                  
096500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTLVM-UP-ATTR             
096600           ELSE                                                           
096700              MOVE NEJ TO INDATA-SW                                       
096800              MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTLVM-UP-ATTR             
096900           END-IF                                                         
097000        ELSE                                                              
097100           MOVE NEJ TO INDATA-SW                                          
097200           MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLTLVM-UP-ATTR                 
097300                                       MOD-IDLEVNR-UP-ATTR                
097400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDBENR-UP-ATTR                 
097500        END-IF                                                            
097600     END-IF                                                               
097700     .                                                                    
097800     EJECT                                                                
097900 GCA-KTR-BELEVART-DUBELTT SECTION.                                        
098000                                                                          
098100     PERFORM IMS-GU-WDF5B1                                                
098200     IF SEGMENT-FINNS                                                     
098300        MOVE MID-IDBENR-UP               TO WS-IDBENR-NUM                 
098400        IF  WS-IDBENR-NUM = SEQB-IDBENR                                   
098500        AND MID-FLTLVM-UP NOT = ALL '+'                                   
098600            MOVE MFS-ALFA-FAELT-RAETT    TO MOD-BELEVART-UP-ATTR          
098700         ELSE                                                             
098800            MOVE ERR-CONFLICT            TO MED-IDMFSFEL                  
098900            CALL WMEDKONV USING MED-WMEDAREA                              
099000            MOVE MED-TEMFSFEL            TO MOD-TEMFSINF                  
099100            MOVE NEJ                     TO INDATA-SW                     
099200            MOVE MFS-ALFA-FAELT-FEL      TO MOD-BELEVART-UP-ATTR          
099300                                            MOD-IDLEVNR-UP-ATTR           
099400                                            MOD-KDCMD-UP-ATTR             
099500         END-IF                                                           
099600     ELSE                                                                 
099700        MOVE  ZERO                       TO W-SEQB-IDARTNR-MIN            
099800        MOVE  +999999999                 TO W-SEQB-IDARTNR-MAX            
099900        PERFORM IMS-GU-WDF5B1                                             
100000        IF SEGMENT-FINNS                                                  
100100           IF MID-KDCMD-UP = ALL '+'                                      
100200              MOVE ERR-CONFLICT          TO MED-IDMFSFEL                  
100300              CALL WMEDKONV           USING MED-WMEDAREA                  
100400              MOVE MED-TEMFSFEL          TO MOD-TEMFSINF                  
100500              MOVE NEJ TO INDATA-SW                                       
100600              MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDCMD-UP-ATTR             
100700                                            MOD-IDLEVNR-UP-ATTR           
100800                                            MOD-BELEVART-UP-ATTR          
100900           ELSE                                                           
101000              IF MID-KDCMD-UP = '*' OR 'D'                                
101100                 MOVE MFS-ALFA-FAELT-RAETT                                
101200                                         TO MOD-BELEVART-UP-ATTR          
101300              ELSE                                                        
101400                 MOVE ERR-CONFLICT       TO MED-IDMFSFEL                  
101500                 CALL WMEDKONV        USING MED-WMEDAREA                  
101600                 MOVE MED-TEMFSFEL       TO MOD-TEMFSINF                  
101700                 MOVE NEJ                TO INDATA-SW                     
101800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEVART-UP-ATTR          
101900                                            MOD-IDLEVNR-UP-ATTR           
102000                                            MOD-KDCMD-UP-ATTR             
102100              END-IF                                                      
102200           END-IF                                                         
102300        ELSE                                                              
102400           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-BELEVART-UP-ATTR          
102500        END-IF                                                            
102600     END-IF                                                               
102700     .                                                                    
102800     EJECT                                                                
102900 GD-KTR-KDCMD  SECTION.                                                   
103000                                                                          
103100     IF MID-KDCMD-UP NOT = ALL '+'                                        
103200        IF MID-KDCMD-UP = 'D' OR '*'                                      
103300           IF UPPDAT-LEV-LEVBET = JA OR                                   
103400              UPPDAT-ARTIKEL    = JA                                      
103500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-UP-ATTR              
103600              IF MID-KDCMD-UP = 'D'                                       
103700                 IF UPPDAT-ARTIKEL =  JA                                  
103800                    MOVE WS-IDARTNR-UP TO W-IDARTNR                       
103900                    PERFORM IMS-GHU-WDF501                                
104000                    IF SEGMENT-FINNS                                      
104100                       PERFORM IMS-GNP-WDF502-OKVAL                       
104200                       IF SEGMENT-FINNS                                   
104300                          MOVE NEJ TO INDATA-SW                           
104400                          MOVE MFS-ALFA-FAELT-FEL                         
104500                                        TO MOD-KDCMD-UP-ATTR              
104600                       ELSE                                               
104700                          MOVE MFS-ALFA-FAELT-RAETT                       
104800                                        TO MOD-KDCMD-UP-ATTR              
104900                       END-IF                                             
105000                    ELSE                                                  
105100                       MOVE NEJ TO INDATA-SW                              
105200                       MOVE MFS-ALFA-FAELT-FEL                            
105300                                        TO MOD-KDCMD-UP-ATTR              
105400                    END-IF                                                
105500                 END-IF                                                   
105600              END-IF                                                      
105700           ELSE                                                           
105800              IF  MID-IDARTNR-UP NOT = ALL '+'                            
105900              AND MID-KDCMD-UP = 'D'                                      
106000                 MOVE ws-IDARTNR-UP TO W-IDARTNR                          
106100                 PERFORM IMS-GHU-WDF501                                   
106200                 IF SEGMENT-FINNS                                         
106300                    PERFORM IMS-GNP-WDF502-OKVAL                          
106400                    IF SEGMENT-FINNS                                      
106500                       MOVE NEJ TO INDATA-SW                              
106600                       MOVE MFS-ALFA-FAELT-FEL                            
106700                                    TO MOD-KDCMD-UP-ATTR                  
106800                    ELSE                                                  
106900                       MOVE JA TO UPPDAT-ARTIKEL                          
107000                       MOVE MFS-ALFA-FAELT-RAETT                          
107100                                    TO MOD-KDCMD-UP-ATTR                  
107200                    END-IF                                                
107300                 ELSE                                                     
107400                    MOVE NEJ        TO INDATA-SW                          
107500                    MOVE MFS-ALFA-FAELT-FEL                               
107600                                    TO MOD-KDCMD-UP-ATTR                  
107700                 END-IF                                                   
107800              ELSE                                                        
107900                 MOVE NEJ TO INDATA-SW                                    
108000                 MOVE MFS-ALFA-FAELT-FEL                                  
108100                                    TO MOD-KDCMD-UP-ATTR                  
108200              END-IF                                                      
108300           END-IF                                                         
108400        ELSE                                                              
108500           MOVE NEJ                 TO INDATA-SW                          
108600           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-UP-ATTR                  
108700        END-IF                                                            
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100 H-UPPDATERA SECTION.                                                     
109200                                                                          
109300     MOVE NEJ TO SW-UPPDAT                                                
109400                                                                          
109500     IF UPPDAT-ARTIKEL = JA                                               
109600        PERFORM HA-UPPDAT-ARTIKEL                                         
109700     END-IF                                                               
109800                                                                          
109900     IF UPPDAT-LEV-LEVBET = JA                                            
110000        PERFORM HB-UPPDAT-LEV-LEVBET                                      
110100        PERFORM HC-FIX-KEY-EFTER-UPDAT                                    
110200     END-IF                                                               
110300                                                                          
110400     IF SW-UPPDAT = JA                                                    
110500        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
110600        CALL WMEDKONV USING MED-WMEDAREA                                  
110700        MOVE MED-MFSINF      TO MOD-TEMFSFEL                              
110800     END-IF                                                               
110900     PERFORM MFS-FORM-ATTR                                                
111000     PERFORM MFS-RENSA-FAELT-IN                                           
111100     .                                                                    
111200     EJECT                                                                
111300 HA-UPPDAT-ARTIKEL SECTION.                                               
111400                                                                          
111500     MOVE WS-IDARTNR-UP TO W-IDARTNR                                      
111600     PERFORM IMS-GHU-WDF501                                               
111700                                                                          
111800     IF SEGMENT-FINNS                                                     
111900        IF MID-KDCMD-UP NOT = ALL '+'                                     
112000           IF MID-KDCMD-UP = 'D'                                          
112100              PERFORM IMS-DLET-WDF5                                       
112200              MOVE JA TO SW-UPPDAT                                        
112300           ELSE                                                           
112400              IF MID-KDFTAG-UP NOT = ALL '+'                              
112500                 MOVE MID-KDFTAG-UP TO XART-KDFTAG                        
112600                 PERFORM S01-DATKONV                                      
112700                 MOVE DAT-TIAAVVD   TO XART-TIV-UPPDAT                    
112800                 PERFORM IMS-REPL-WDF5                                    
112900                 MOVE JA TO SW-UPPDAT                                     
113000              END-IF                                                      
113100           END-IF                                                         
113200        ELSE                                                              
113300           IF MID-KDFTAG-UP NOT = ALL '+'                                 
113400              MOVE MID-KDFTAG-UP TO XART-KDFTAG                           
113500              PERFORM S01-DATKONV                                         
113600              MOVE DAT-TIAAVVD   TO XART-TIV-UPPDAT                       
113700              PERFORM IMS-REPL-WDF5                                       
113800              MOVE JA TO SW-UPPDAT                                        
113900           END-IF                                                         
114000        END-IF                                                            
114100     ELSE                                                                 
114200         MOVE WS-IDARTNR-UP TO W-IDARTNR                                  
114300                               RESK-IDART                                 
114400         CALL W009KSIF USING RESK-IDART RESK-9-POS RESK-W009KSIFR         
114500         MOVE ZERO                   TO XART-IDARTNR                      
114600         MOVE MID-KDFTAG-up          TO XART-KDFTAG                       
114700                                        XART-TIV-UPPDAT                   
114800        IF MID-KDCMD-UP = ALL '+'  OR '*'                                 
114900           MOVE MID-IDARTNR-UP       TO XART-IDARTNR                      
115000           MOVE RESK-W009KSIFR       TO XART-REKSIFFR                     
115100           PERFORM S01-DATKONV                                            
115200           MOVE DAT-TIAAVVD          TO XART-TIV-UPPDAT                   
115300           PERFORM IMS-ISRT-WDF501                                        
115400           MOVE JA TO SW-UPPDAT                                           
115500        END-IF                                                            
115600     END-IF                                                               
115700     .                                                                    
115800     EJECT                                                                
115900 HB-UPPDAT-LEV-LEVBET SECTION.                                            
116000                                                                          
116100     MOVE MID-IDLEVNR-UP            TO W-IDLEVNR-MIN                      
116200     MOVE MID-IDBENR-UP             TO W-IDBENR-MIN                       
116300                                                                          
116400     PERFORM IMS-GHU-WDF502-UNIK                                          
116500     IF SEGMENT-FINNS                                                     
116600        IF MID-KDCMD-UP NOT = ALL '+' AND '*'                             
116700           PERFORM IMS-DLET-WDF5                                          
116800           MOVE JA TO SW-UPPDAT                                           
116900        ELSE                                                              
117000           IF MID-BELEVART-UP NOT = ALL '+'                               
117100              MOVE MID-BELEVART-UP  TO XLEV-BELEVART                      
117200              MOVE WS-IDLEVART-UP   TO XLEV-IDLEVART                      
117300              MOVE JA TO SW-UPPDAT                                        
117400           END-IF                                                         
117500           IF MID-FLTLVM-UP   NOT = ALL '+'                               
117600              MOVE MID-FLTLVM-UP    TO XLEV-FLTLVM                        
117700              MOVE JA TO SW-UPPDAT                                        
117800           END-IF                                                         
117900           PERFORM IMS-REPL-WDF5                                          
118000        END-IF                                                            
118100     ELSE                                                                 
118300        MOVE ZERO                   TO XLEV-IDBENR                        
118400        MOVE LOW-VALUE              TO XLEV-FLTLVM                        
118410                                       XLEV-IDLEVNR                       
118500                                       XLEV-BELEVART                      
118600                                       XLEV-IDLEVART                      
118700                                                                          
118800        IF MID-KDCMD-UP = ALL '+' OR '*'                                  
118900           MOVE MID-IDLEVNR-UP      TO XLEV-IDLEVNR                       
119000           MOVE MID-IDBENR-UP       TO XLEV-IDBENR                        
119100           IF MID-BELEVART-UP NOT = ALL '+'                               
119200              MOVE MID-BELEVART-UP  TO XLEV-BELEVART                      
119300              MOVE WS-IDLEVART-UP   TO XLEV-IDLEVART                      
119400           END-IF                                                         
119500           IF MID-FLTLVM-UP   NOT = ALL '+'                               
119600              MOVE MID-FLTLVM-UP    TO XLEV-FLTLVM                        
119700           ELSE                                                           
119800              MOVE JA               TO XLEV-FLTLVM                        
119900           END-IF                                                         
120000           PERFORM IMS-ISRT-WDF502                                        
120100           MOVE JA TO SW-UPPDAT                                           
120200        END-IF                                                            
120300     END-IF                                                               
120400     .                                                                    
120500     EJECT                                                                
120600 HC-FIX-KEY-EFTER-UPDAT    SECTION.                                       
120700                                                                          
120800* WDF502-KEY                                                              
120900     MOVE XLEV-IDLEVNR    TO W-IDLEVNR-MIN                                
121000     MOVE XLEV-IDBENR     TO W-IDBENR-MIN                                 
121100                                                                          
121200* WDF5A1-KEY                                                              
121300     MOVE XLEV-IDLEVNR    TO W-SEQA-IDLEVNR-MIN                           
121400                             W-SEQA-IDLEVNR-MAX                           
121500     MOVE XLEV-IDLEVART TO W-SEQA-IDLEVART-MIN                            
121600     MOVE WS-IDLEVART-MAX TO W-SEQA-IDLEVART-MAX                          
121700                                                                          
121800     MOVE W-IDARTNR       TO W-SEQA-IDARTNR-MIN                           
121900     MOVE XLEV-IDBENR     TO W-SEQA-IDBENR-MIN                            
122000* WDF5B1-KEY                                                              
122100     MOVE XLEV-IDLEVNR    TO W-SEQB-IDLEVNR-MIN                           
122200                                                                          
122300     MOVE SPACE           TO W-SEQB-IDLEVART-MIN                          
122400     MOVE XLEV-IDLEVART TO W-SEQB-IDLEVART-MIN                            
122500     MOVE SPACE           TO W-SEQB-IDLEVART-MAX                          
122600     MOVE WS-IDLEVART-MAX TO W-SEQB-IDLEVART-MAX                          
122700                                                                          
122800     MOVE XLEV-IDBENR     TO W-SEQB-IDBENR-MIN                            
122900     MOVE W-IDARTNR       TO W-SEQB-IDARTNR-MIN                           
123000     .                                                                    
123100     EJECT                                                                
123200 S01-DATKONV SECTION.                                                     
123300                                                                          
123400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
123500     CALL WDATKONV USING DAT-KDDATFORM                                    
123600                         DAT-I-TIDATUM                                    
123700                         DAT-O-TIDATUM                                    
123800                         DAT-KDSVAR                                       
123900                                                                          
124000     IF DAT-KDSVAR-OK                                                     
124100        CONTINUE                                                          
124200     ELSE                                                                 
124300        DISPLAY                                                           
124400        'FELAKTIG KDSVAR FRÅN DATKONV : ' DAT-KDSVAR                      
124500        CALL FELLOG                                                       
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900 MFS-RENSA-FAELT-UT SECTION.                                              
125000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-NEXT                             
125100                             MOD-IDLEVNR-NEXT                             
125200                             MOD-IDLEVART-NEXT                            
125300                             MOD-IDBENR-NEXT                              
125400                             MOD-TIV-UPPDAT-RAD5                          
125500                             MOD-KDFTAG-RAD5                              
125600     MOVE +1 TO INDX                                                      
125700     PERFORM UNTIL INDX > MAX-INDX                                        
125800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(INDX)                          
125900                               MOD-KDFTAG(INDX)                           
126000                               MOD-IDLEVNR(INDX)                          
126100                               MOD-IDBENR(INDX)                           
126200                               MOD-BELEVART(INDX)                         
126300                               MOD-FLTLVM(INDX)                           
126400       ADD +1 TO INDX                                                     
126500     END-PERFORM                                                          
126600     .                                                                    
126700     EJECT                                                                
126800 MFS-RENSA-FAELT-IN SECTION.                                              
126900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UP                               
127000                             MOD-KDFTAG-UP                                
127100                             MOD-IDBENR-UP                                
127200                             MOD-IDLEVNR-UP                               
127300                             MOD-BELEVART-UP                              
127400                             MOD-FLTLVM-UP                                
127500                             MOD-KDCMD-UP                                 
127600     .                                                                    
127700     EJECT                                                                
127800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
127900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-NEXT                           
128000                               MOD-IDLEVNR-NEXT                           
128100                               MOD-IDLEVART-NEXT                          
128200                               MOD-IDBENR-NEXT                            
128300                               MOD-TIV-UPPDAT-RAD5                        
128400                               MOD-KDFTAG-RAD5                            
128500                                                                          
128600     MOVE +1 TO INDX                                                      
128700     PERFORM UNTIL INDX > MAX-INDX                                        
128800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(INDX)                        
128900                                 MOD-KDFTAG(INDX)                         
129000                                 MOD-IDLEVNR(INDX)                        
129100                                 MOD-IDBENR(INDX)                         
129200                                 MOD-BELEVART(INDX)                       
129300                                 MOD-FLTLVM(INDX)                         
129400       ADD +1 TO INDX                                                     
129500     END-PERFORM                                                          
129600     .                                                                    
129700     EJECT                                                                
129800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
129900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UP                             
130000                               MOD-KDFTAG-UP                              
130100                               MOD-IDBENR-UP                              
130200                               MOD-IDLEVNR-UP                             
130300                               MOD-BELEVART-UP                            
130400                               MOD-FLTLVM-UP                              
130500                               MOD-KDCMD-UP                               
130600                                                                          
130700     .                                                                    
130800     EJECT                                                                
130900 MFS-FORM-ATTR SECTION.                                                   
131000     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-UP                            
131100                                MOD-KDFTAG-UP                             
131200                                MOD-IDBENR-UP                             
131300                                MOD-IDLEVNR-UP                            
131400                                MOD-BELEVART-UP                           
131500                                MOD-FLTLVM-UP                             
131600                                MOD-KDCMD-UP                              
131700     .                                                                    
131800     EJECT                                                                
131900 IMS-GET-MSG SECTION.                                                     
132000     MOVE '  QC' TO GODK-STATUSKODER                                      
132100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
132200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
132300     PERFORM IMS-STATUSKONTROLL                                           
132400     .                                                                    
132500 IMS-INSERT-MSG SECTION.                                                  
132600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
132700     MOVE SPACE TO GODK-STATUSKODER                                       
132800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
132900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133000     PERFORM IMS-STATUSKONTROLL                                           
133100     .                                                                    
133200     EJECT                                                                
133300 IMS-GHU-WDF501 SECTION.                                                  
133400     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
133500     DELIMITED BY SIZE INTO SSA1                                          
133600     MOVE '  GE' TO GODK-STATUSKODER                                      
133700     CALL CBLTDLI USING GHU WDF5-PCB DLI-IO-AREA SSA1                     
133800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
133900     PERFORM IMS-STATUSKONTROLL                                           
134000     .                                                                    
134100 IMS-GHU-WDF501-EJGE SECTION.                                             
134200     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
134300     DELIMITED BY SIZE INTO SSA1                                          
134400     MOVE '  '   TO GODK-STATUSKODER                                      
134500     CALL CBLTDLI USING GHU WDF5-PCB DLI-IO-AREA SSA1                     
134600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
134700     PERFORM IMS-STATUSKONTROLL                                           
134800     .                                                                    
134900 IMS-ISRT-WDF501 SECTION.                                                 
135000     MOVE 'WDF501  ' TO SSA1                                              
135100     MOVE '  II' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING ISRT WDF5-PCB DLI-IO-AREA SSA1                    
135300     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600 IMS-GHU-WDF502-UNIK SECTION.                                             
135700     STRING 'WDF502  (WDF5KEY  =' W-WDF5KEY-MIN ')'                       
135800       DELIMITED BY SIZE INTO SSA1                                        
135900     MOVE '  GE' TO GODK-STATUSKODER                                      
136000     CALL CBLTDLI USING GHU WDF5-PCB DLI-IO-AREA SSA1                     
136100     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
136200     PERFORM IMS-STATUSKONTROLL                                           
136300     .                                                                    
136400     EJECT                                                                
136500 IMS-GHU-WDF502-UNIK-EJGE SECTION.                                        
136600     STRING 'WDF502  (WDF5KEY  =' W-WDF5KEY-MIN ')'                       
136700        DELIMITED BY SIZE INTO SSA1                                       
136800     MOVE '  '   TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING GHU WDF5-PCB DLI-IO-AREA SSA1                     
137000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300 IMS-GHNP-WDF502 SECTION.                                                 
137400     STRING 'WDF502  (WDF5KEY =>' W-WDF5KEY-MIN                           
137500                 '&WDF5KEY =<' W-WDF5KEY-MAX ')'                          
137600     DELIMITED BY SIZE INTO SSA1                                          
137700     MOVE '  GE' TO GODK-STATUSKODER                                      
137800     CALL CBLTDLI USING GHNP WDF5-PCB DLI-IO-AREA SSA1                    
137900     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
138000     PERFORM IMS-STATUSKONTROLL                                           
138100     .                                                                    
138200 IMS-GNP-WDF502-OKVAL SECTION.                                            
138300     MOVE 'WDF502  ' TO SSA1                                              
138400     MOVE '  GE' TO GODK-STATUSKODER                                      
138500     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
138600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
138700     PERFORM IMS-STATUSKONTROLL                                           
138800     .                                                                    
138900 IMS-ISRT-WDF502 SECTION.                                                 
139000     MOVE 'WDF502   ' TO SSA1                                             
139100     MOVE '  II' TO GODK-STATUSKODER                                      
139200     CALL CBLTDLI USING ISRT WDF5-PCB DLI-IO-AREA SSA1                    
139300     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     .                                                                    
139600     EJECT                                                                
139700 IMS-REPL-WDF5 SECTION.                                                   
139800     MOVE '  ' TO GODK-STATUSKODER                                        
139900     CALL CBLTDLI USING REPL WDF5-PCB DLI-IO-AREA                         
140000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     .                                                                    
140300 IMS-DLET-WDF5 SECTION.                                                   
140400     MOVE '  ' TO GODK-STATUSKODER                                        
140500     CALL CBLTDLI USING DLET WDF5-PCB DLI-IO-AREA                         
140600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900 IMS-GN-WDF5A1 SECTION.                                                   
141000     STRING 'WDF5A1  (WDF5A1KY=>' W-WDF5A1KY-MIN                          
141100                    '&WDF5A1KY=<' W-WDF5A1KY-MAX ')'                      
141200          DELIMITED BY SIZE INTO SSA1                                     
141300     MOVE '  GE' TO GODK-STATUSKODER                                      
141400     CALL CBLTDLI USING GN WDF5A-PCB DLI-IO-AREA-2 SSA1                   
141500     MOVE WDF5A-STATUS-CODE TO STATUS-WS                                  
141600     PERFORM IMS-STATUSKONTROLL                                           
141700     .                                                                    
141800     EJECT                                                                
141900 IMS-GN-WDF5B1       SECTION.                                             
142000     STRING 'WDF5B1  (WDF5B1KY=>' W-WDF5B1KY-MIN                          
142100                    '&WDF5B1KY=<' W-WDF5B1KY-MAX ')'                      
142200          DELIMITED BY SIZE INTO SSA1                                     
142300     MOVE '  GE' TO GODK-STATUSKODER                                      
142400     CALL CBLTDLI USING GN WDF5B-PCB DLI-IO-AREA-3 SSA1                   
142500     MOVE WDF5B-STATUS-CODE TO STATUS-WS                                  
142600     PERFORM IMS-STATUSKONTROLL                                           
142700     .                                                                    
142800 IMS-GU-WDF5B1       SECTION.                                             
142900     STRING 'WDF5B1  (WDF5B1KY=>' W-WDF5B1KY-MIN                          
143000                    '&WDF5B1KY=<' W-WDF5B1KY-MAX                          
143100                    '&IDLEVNR  =' W-SEQB-IDLEVNR-SOEK ')'                 
143200          DELIMITED BY SIZE INTO SSA1                                     
143300     MOVE '  GE' TO GODK-STATUSKODER                                      
143400     CALL CBLTDLI USING GU WDF5B-PCB DLI-IO-AREA-3 SSA1                   
143500     MOVE WDF5B-STATUS-CODE TO STATUS-WS                                  
143600     PERFORM IMS-STATUSKONTROLL                                           
143700     .                                                                    
143800 IMS-GU-WDF5B1-ACTU  SECTION.                                             
143900     STRING 'WDF5B1  (WDF5B1KY=>' W-WDF5B1KY-MIN                          
144000                    '&WDF5B1KY=<' W-WDF5B1KY-MAX ')'                      
144100          DELIMITED BY SIZE INTO SSA1                                     
144200     MOVE '  GE' TO GODK-STATUSKODER                                      
144300     CALL CBLTDLI USING GU WDF5B-PCB DLI-IO-AREA-3 SSA1                   
144400     MOVE WDF5B-STATUS-CODE TO STATUS-WS                                  
144500     PERFORM IMS-STATUSKONTROLL                                           
144600     .                                                                    
144700     EJECT                                                                
144800 IMS-STATUSKONTROLL SECTION.                                              
144900                                                                          
145000     SET STATUS-IX TO 1                                                   
145100     SEARCH GODK-STATUS                                                   
145200       AT END                                                             
145300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
145400         DELIMITED BY SIZE INTO FELTEXT                                   
145500         CALL FELLOG                                                      
145600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
145700         CONTINUE                                                         
145800     END-SEARCH                                                           
145900     .                                                                    
