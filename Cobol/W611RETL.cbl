000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W611RETL.                                                
000500*AUTHOR.         GUNNAR LARSSON IDK.                                      
000600*DATE-WRITTEN.   92/09/28.                                                
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKRIVER RETURLISTA                                               
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        LÄNKAREA:    W611RETL                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        RETURLISTA (VIA W006PRS1)                                        
001800*        MOD:         W6I19101 (PROG-TO-PROG-SW)                          
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002401                                                                          
002410*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W611RETL'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                     
003400 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
003500 77  MAX-MOD-LAENGD-6191         PIC S9(4)  VALUE +1568 COMP SYNC.        
003600                                                                          
003700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
003800     EJECT                                                                
003900*    --- ALLMÄNNA ARBETSFÄLT                                              
004000 01      FILLER               PIC X(16) VALUE 'WS**************'.         
004100 01      WS.                                                              
004200                                                                          
004300*        --- PRINTER-ID                                                   
004400  02     WS-IDPRTLST             PIC X(8)    VALUE SPACE.                 
004500*        --- DAGENS DATUM                                                 
004600  02     WS-TIAAMMDD             PIC 9(6)    VALUE ZERO.                  
004700  02     FILLER                  REDEFINES WS-TIAAMMDD.                   
004800   03    WS-TIAAMMDD-AA          PIC 9(2).                                
004900   03    WS-TIAAMMDD-MM          PIC 9(2).                                
005000   03    WS-TIAAMMDD-DD          PIC 9(2).                                
005100*        -- AKTUELLT LIST-ID                                              
005200  02     WS-AKTID.                                                        
005300   03    WS-AKTID-IDLEVNR-KOLLI  PIC  X(5)   VALUE SPACE.                 
005400   03    WS-AKTID-IDOKOLLI       PIC 9(9)    VALUE ZERO.                  
005500   03    WS-AKTID-IDINLVGN       PIC 9(3)    VALUE ZERO.                  
005600   03    WS-AKTID-ADINLOMR       PIC X(4)    VALUE SPACE.                 
005700   03    WS-AKTID-ADINLOMR-NXT   PIC X(4)    VALUE SPACE.                 
005800*        -- SUMMA VOR-KVANT                                               
005900  02     WS-KVINLART-VOR         PIC S9(7)   VALUE ZERO COMP-3.           
006000*        --- SPARAT FRÅN INLA-RAD FÖRE UPPDAT                             
006100  02     WS-OLD.                                                          
006200   03    WS-OLD-ADINLOMR-NXT     PIC X(4)    VALUE SPACE.                 
006300   03    WS-OLD-KDINLSTA         PIC X(3)    VALUE SPACE.                 
006400     EJECT                                                                
006500 01      FILLER               PIC X(16) VALUE 'SW-SWITCHAR*****'.         
006600 01      SW-SWITCHAR.                                                     
006700                                                                          
006800  02     SW-INIT-GJORD           PIC X(1)    VALUE 'N'.                   
006900  02     SW-1A-6191              PIC X(1)    VALUE SPACE.                 
007000     SKIP3                                                                
007100*    --- KONSTANTER                                                       
007200                                                                          
007300 01      FILLER                  PIC X(8)    VALUE 'K*******'.            
007400                                                                          
007500 01      K-KONSTANTER.                                                    
007600                                                                          
007700  02     K-MAX-6191-KVPOST       PIC S9(9)   VALUE +24  COMP SYNC.        
007800     SKIP3                                                                
007900*    --- INDEXVARIABLER                                                   
008000                                                                          
008100 01      FILLER                  PIC X(8)    VALUE 'IX******'.            
008200                                                                          
008300 01      IX-INDEXVARIABLER.                                               
008400                                                                          
008500  02     IX-6191                 PIC S9(9)   VALUE ZERO COMP SYNC.        
008600     EJECT                                                                
008700*    --- RETURLISTA ARBETSFÄLT                                            
008800 01      FILLER                  PIC X(8)    VALUE 'RLW*****'.            
008900 01      RLW.                                                             
009000                                                                          
009100  02     RLW-SIDANT              PIC S9(3)  VALUE ZERO COMP-3.            
009200  02     RLW-RADMAX              PIC S9(3)  VALUE +48  COMP-3.            
009300  02     RLW-RADANT              PIC S9(3)  VALUE ZERO COMP-3.            
009400  02     RLW-RADSKIP             PIC S9(3)  VALUE ZERO COMP-3.            
009500                                                                          
009600  02     RLW-BRYTID.                                                      
009700   03    RLW-BRYTID-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                 
009800   03    RLW-BRYTID-IDOKOLLI     PIC 9(9)    VALUE ZERO.                  
009900   03    RLW-BRYTID-IDINLVGN     PIC 9(3)    VALUE ZERO.                  
010000   03    RLW-BRYTID-ADINLOMR     PIC X(4)    VALUE SPACE.                 
010100   03    RLW-BRYTID-ADINLOMR-NXT PIC X(4)    VALUE SPACE.                 
010200                                                                          
010300  02     RLW-RAD                 PIC X(132).                              
010400                                                                          
010500  02     RLW-RUB1.                                                        
010600   03    FILLER                  PIC X(1)    VALUE SPACE.                 
010700   03    FILLER                  PIC X(38)   VALUE                        
010800         'VOLVO CAR PARTS     '.                                          
010900   03    RLW-RUB1-IDPGM          PIC X(6).                                
011000   03    FILLER                  PIC X(14)   VALUE '-01'.                 
011100   03    FILLER                  PIC X(40)   VALUE 'RETURLISTA'.          
011200   03    RLW-RUB1-TIAAMMDD-AA    PIC 9(2).                                
011300   03    FILLER                  PIC X(1)    VALUE '-'.                   
011400   03    RLW-RUB1-TIAAMMDD-MM    PIC 9(2).                                
011500   03    FILLER                  PIC X(1)    VALUE '-'.                   
011600   03    RLW-RUB1-TIAAMMDD-DD    PIC 9(2).                                
011700   03    FILLER                  PIC X(3)    VALUE SPACE.                 
011800   03    FILLER                  PIC X(3)    VALUE 'SID'.                 
011900   03    RLW-RUB1-SIDNR          PIC Z(3).                                
012000                                                                          
012100  02     RLW-RUB2.                                                        
012200   03    FILLER                  PIC X(1)    VALUE SPACE.                 
012300   03    FILLER                  PIC X(7)    VALUE 'LEVNR'.               
012400   03    RLW-RUB2-IDLEVNR-KOLLI  PIC X(5)    VALUE SPACE.                 
012500   03    FILLER                  PIC X(2)    VALUE SPACE.                 
012600   03    FILLER                  PIC X(8)    VALUE 'KOLLINR'.             
012700   03    RLW-RUB2-IDOKOLLI       PIC Z(9).                                
012800   03    FILLER                  PIC X(2)    VALUE SPACE.                 
012900   03    FILLER                  PIC X(5)    VALUE 'VAGN'.                
013000   03    RLW-RUB2-IDINLVGN       PIC Z(3).                                
013100   03    FILLER                  PIC X(1)    VALUE SPACE.                 
013200   03    FILLER                  PIC X(5)    VALUE 'PLAC'.                
013300   03    RLW-RUB2-ADINLOMR       PIC X(4).                                
013400   03    FILLER                  PIC X(8)    VALUE SPACE.                 
013500   03    FILLER                  PIC X(13)   VALUE 'RETUR TILL:'.         
013600   03    RLW-RUB2-ADINLOMR-NXT   PIC X(4).                                
013700                                                                          
013800  02     RLW-RUB3.                                                        
013900   03    FILLER                  PIC X(20)   VALUE SPACE.                 
014000   03    FILLER                  PIC X(9)    VALUE 'ARTNR   '.            
014100   03    FILLER                  PIC X(28)   VALUE 'BENÄMNING'.           
014200   03    FILLER                  PIC X(9)    VALUE 'ANTAL'.               
014300   03    FILLER                  PIC X(4)    VALUE 'VOR'.                 
014400   03    FILLER                  PIC X(10)   VALUE 'PRIO'.                
014500   03    FILLER                  PIC X(4)    VALUE 'FT'.                  
014600   03    FILLER                  PIC X(6)    VALUE 'LEVNR'.               
014700   03    FILLER                  PIC X(13)   VALUE 'PARTI/KOLLI'.         
014800   03    FILLER                  PIC X(10)   VALUE 'NOTERINGAR'.          
014900     EJECT                                                                
015000  02     RLW-R01.                                                         
015100   03    FILLER                  PIC X(20).                               
015200   03    RLW-R01-IDARTNR         PIC Z(8).                                
015300   03    FILLER                  PIC X(1).                                
015400   03    RLW-R01-BEART           PIC X(25).                               
015500   03    FILLER                  PIC X(2).                                
015600   03    RLW-R01-KVINLART        PIC Z(6).                                
015700   03    FILLER                  PIC X(1).                                
015800   03    RLW-R01-KVINLART-VOR    PIC Z(6).                                
015900   03    FILLER                  PIC X(3).                                
016000   03    RLW-R01-BEPRIO          PIC X(1).                                
016100   03    FILLER                  PIC X(7).                                
016200   03    RLW-R01-BEFT            PIC Z(2).                                
016300   03    FILLER                  PIC X(2).                                
016400   03    RLW-R01-IDLEVNR-KOLLI   PIC X(5).                                
016500   03    FILLER                  PIC X(3).                                
016600   03    RLW-R01-IDOKOLLI        PIC Z(9).                                
016700   03    RLW-R01-IDLOPNRM        REDEFINES RLW-R01-IDOKOLLI               
016800                                 PIC Z(9).                                
016900     EJECT                                                                
016910*      --- VALID IDDC CODES                                               
016920*                                                                         
016930*01    -COPY WWDCKONS                                                     
016940       EJECT                                                              
017000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017100 01  GENERELLA-SUBPROGRAM.                                                
017200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017500     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
017600     EJECT                                                                
017700*    --- WDATAREA, AREA FÖR WDATKONV                                      
017800 01  FILLER                    PIC X(16) VALUE 'WDATAREA********'.        
017900                                                                          
018000*01  -COPY WDATAREA                                                       
018100     EJECT                                                                
018200*    --- W006PRAR, AREA FÖR W006PRS1                                      
018300 01  FILLER                    PIC X(16) VALUE 'W006PRAR********'.        
018400                                                                          
018500*01  -COPY W006PRAR                                                       
018600     EJECT                                                                
018700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018800*                                                                         
018900     SKIP3                                                                
019000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019100     SKIP3                                                                
019200*01  -COPY WMSGAREA                                                       
019300     EJECT                                                                
019400 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
019500     SKIP3                                                                
019600 01      P-TO-P-SW.                                                       
019700                                                                          
019800  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
019900  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
020000  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
020100  02     P-TO-P-KDTRANS          PIC X(8).                                
020200  02     P-TO-P-IDTRANS          PIC X(4).                                
020300  02     P-TO-P-KDMFSFOR         PIC X(1).                                
020400  02     -COPY W6I19101 -PRE MOD6191-                                     
020500     EJECT                                                                
021100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021200     SKIP3                                                                
021300*01  -COPY WMFSAREA                                                       
021400     EJECT                                                                
021500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021600*                                                                         
021700     SKIP3                                                                
021800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021900     SKIP3                                                                
022000 01  NYCKLAR-TILL-DLI.                                                    
022100     03  W-W6D101KY-X.                                                    
022200         05  W-W6D101KY-IDDC     PIC X(2)    VALUE SPACE.                 
022210         05  W-W6D101KY-IDLEVNR  PIC  X(5)   VALUE SPACE.                 
022300         05  W-W6D101KY-IDFS     PIC X(8)    VALUE SPACE.                 
022400         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
022500     03  W-IDRADNR-INL-X.                                                 
022600         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
022700     03  W-IDRADNR-X.                                                     
022800         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
022900     03  W-IDLEVNR-KOLLI-X.                                               
023000         05  W-IDLEVNR-KOLLI     PIC  X(5)   VALUE SPACE.                 
023100     03  W-IDOKOLLI-X.                                                    
023200         05  W-IDOKOLLI          PIC 9(9)    VALUE ZERO.                  
023300     SKIP2                                                                
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  SEGMENT-FINNS                       VALUE '  '.                  
023700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023900     SKIP2                                                                
024000 01  GODK-STATUSKODER.                                                    
024100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024200     SKIP3                                                                
024300 01  SSA1                        PIC X(64).                               
024400 01  SSA2                        PIC X(64).                               
024500     EJECT                                                                
024600*    --- IMS FUNKTIONSKODER                                               
024700*01  -COPY W0003                                                          
024800     EJECT                                                                
024900*    ---  DLI INPUT-OUTPUT AREA                                           
025000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025100     SKIP3                                                                
025200 01  DLI-IO-AREA.                                                         
025300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
025400     SKIP3                                                                
025500     03  W6INLA11 REDEFINES IO-AREA.                                      
025600*        05  -COPY W6D111  -PRE INLA-                                     
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
025900     SKIP3                                                                
026000 01  DLI-IO-AREA2.                                                        
026100     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
026200     SKIP3                                                                
026300     03  W6INLA21 REDEFINES IO-AREA2.                                     
026400*        05  -COPY W6D121  -PRE INLA-                                     
026500     EJECT                                                                
026600 LINKAGE SECTION.                                                         
026700                                                                          
026800*01  -COPY W611RETL                                                       
026900     EJECT                                                                
027000*01  -COPY W0009   -PRE PRT-                                              
027100     EJECT                                                                
027200*01  -COPY W0009   -PRE 6191-                                             
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE INLA-                                              
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700 PROCEDURE DIVISION  USING RETL-W611RETL                                  
027800                           PRT-PCB                                        
027900                           6191-PCB                                       
028000                           INLA-PCB.                                      
028100     PERFORM A-INIT                                                       
028200     PERFORM B-BEH-INDATA                                                 
028300     PERFORM Z-FINIT                                                      
028400                                                                          
028500     MOVE ZERO TO RETURN-CODE                                             
028600     GOBACK                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 A-INIT SECTION.                                                          
029000                                                                          
029100     IF SW-INIT-GJORD = NEJ                                               
029200                                                                          
029300       MOVE RETL-IDPGM (2:1)     TO W-IDTRANS (1:1)                       
029400       MOVE RETL-IDPGM (4:3)     TO W-IDTRANS (2:3)                       
029500                                                                          
029600       MOVE RETL-IDPGM           TO RLW-RUB1-IDPGM                        
029700                                                                          
029800       MOVE 'IDAG'               TO DAT-KDDATFORM                         
029900       CALL WDATKONV USING       DAT-KDDATFORM                            
030000                                 DAT-I-TIDATUM                            
030100                                 DAT-O-TIDATUM                            
030200                                 DAT-KDSVAR                               
030300       IF DAT-KDSVAR-OK                                                   
030400         MOVE DAT-TIAAMMDD       TO WS-TIAAMMDD                           
030500         MOVE WS-TIAAMMDD-AA     TO RLW-RUB1-TIAAMMDD-AA                  
030600         MOVE WS-TIAAMMDD-MM     TO RLW-RUB1-TIAAMMDD-MM                  
030700         MOVE WS-TIAAMMDD-DD     TO RLW-RUB1-TIAAMMDD-DD                  
030800       ELSE                                                               
030900         STRING 'FEL FRÅN WDATKONV:' DAT-KDSVAR                           
031000           DELIMITED BY SIZE INTO FELTEXT                                 
031100         CALL FELLOG                                                      
031200       END-IF                                                             
031300                                                                          
031400       MOVE RETL-IDPRTLST        TO WS-IDPRTLST                           
031500       PERFORM S01-PRT-OPEN                                               
031600       MOVE +99                  TO RLW-RADANT                            
031700       MOVE ZERO                 TO RLW-RADSKIP                           
031800                                                                          
031900       MOVE JA                   TO SW-1A-6191                            
032000       PERFORM S10-INIT-6191                                              
032100                                                                          
032200       MOVE JA                   TO SW-INIT-GJORD                         
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 B-BEH-INDATA SECTION.                                                    
032700                                                                          
032800     MOVE RETL-IDLEVNR-KOLLI     TO WS-AKTID-IDLEVNR-KOLLI                
032900     MOVE RETL-IDOKOLLI          TO WS-AKTID-IDOKOLLI                     
033000     MOVE RETL-IDINLVGN          TO WS-AKTID-IDINLVGN                     
033100     MOVE RETL-ADINLOMR          TO WS-AKTID-ADINLOMR                     
033200     MOVE RETL-ADINLOMR-NXT      TO WS-AKTID-ADINLOMR-NXT                 
033300                                                                          
033400     IF WS-AKTID NOT = RLW-BRYTID                                         
033500       PERFORM BA-INIT-NYTT-ID                                            
033600     END-IF                                                               
033700                                                                          
033800     PERFORM BB-UPPD-INLA-RAD                                             
033900     PERFORM BC-RED-6191                                                  
034000     PERFORM BD-RED-LISTRAD                                               
034100                                                                          
034200     PERFORM BE-STYR-SIDA-RL                                              
034300     .                                                                    
034400     EJECT                                                                
034500 BA-INIT-NYTT-ID SECTION.                                                 
034600                                                                          
034700     MOVE WS-AKTID               TO RLW-BRYTID                            
034800     MOVE ZERO                   TO RLW-SIDANT                            
034900     MOVE +99                    TO RLW-RADANT                            
035000                                                                          
035100     MOVE RETL-IDLEVNR-KOLLI     TO RLW-RUB2-IDLEVNR-KOLLI                
035200     MOVE RETL-IDOKOLLI          TO RLW-RUB2-IDOKOLLI                     
035300     MOVE RETL-IDINLVGN          TO RLW-RUB2-IDINLVGN                     
035400     MOVE RETL-ADINLOMR          TO RLW-RUB2-ADINLOMR                     
035500     MOVE RETL-ADINLOMR-NXT      TO RLW-RUB2-ADINLOMR-NXT                 
035600     .                                                                    
035700     EJECT                                                                
035800 BB-UPPD-INLA-RAD SECTION.                                                
035900                                                                          
036000     MOVE WC-CDC-SE              TO W-W6D101KY-IDDC                       
036010     MOVE RETL-IDLEVNR           TO W-W6D101KY-IDLEVNR                    
036100     MOVE RETL-IDFS              TO W-W6D101KY-IDFS                       
036200     MOVE RETL-TIAVIDAT          TO W-W6D101KY-TIAVIDAT                   
036300     MOVE RETL-IDRADNR-INL       TO W-IDRADNR-INL                         
036400     MOVE RETL-IDRADNR           TO W-IDRADNR                             
036500     PERFORM IMS-GU-INLA-ART                                              
036600     PERFORM IMS-GHNP-INLA-RAD                                            
036700                                                                          
036800     MOVE INLA-RAD-ADINLOMR-NXT  TO WS-OLD-ADINLOMR-NXT                   
036900     MOVE INLA-RAD-KDINLSTA      TO WS-OLD-KDINLSTA                       
037000                                                                          
037100     MOVE RETL-ADINLOMR-NXT      TO INLA-RAD-ADINLOMR-NXT                 
037200     IF INLA-RAD-KDINLSTA = 'SAK'                                         
037300       MOVE SPACE                TO INLA-RAD-KDINLSTA                     
037400     END-IF                                                               
037500                                                                          
037600     PERFORM IMS-REPL-INLA-RAD                                            
037700     .                                                                    
037800     EJECT                                                                
037900 BC-RED-6191 SECTION.                                                     
038000                                                                          
038100     IF  IX-6191 = K-MAX-6191-KVPOST                                      
038200       PERFORM S11-P-TO-P-6191                                            
038300       PERFORM S10-INIT-6191                                              
038400     END-IF                                                               
038500                                                                          
038600     ADD +1                      TO IX-6191                               
038700                                                                          
038800     MOVE WC-CDC-SE              TO MOD6191-MID-IDDC                      
038810     MOVE INLA-ART-IDLOPNRM      TO MOD6191-MID-IDLOPNRM (IX-6191)        
038900     MOVE INLA-RAD-IDRADNR       TO MOD6191-MID-IDRADNR  (IX-6191)        
039000     MOVE INLA-RAD-KDINLPRIO     TO MOD6191-MID-KDINLPRIO                 
039100                                                         (IX-6191)        
039200     MOVE INLA-ART-PRARTSTD      TO MOD6191-MID-PRARTSTD (IX-6191)        
039210     MOVE +0                     TO MOD6191-MID-KVKOLLI  (IX-6191)        
039220     MOVE 'N'                    TO MOD6191-MID-FLINLI   (IX-6191)        
039300                                                                          
039400     MOVE INLA-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-OLD              
039500                                                         (IX-6191)        
039600     MOVE WS-OLD-ADINLOMR-NXT    TO MOD6191-MID-ADINLOMR-NXT-OLD          
039700                                                         (IX-6191)        
039800     MOVE WS-OLD-KDINLSTA        TO MOD6191-MID-KDINLSTA-OLD              
039900                                                         (IX-6191)        
040000     MOVE INLA-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
040100                                                         (IX-6191)        
040200                                                                          
040300     MOVE INLA-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-NEW              
040400                                                         (IX-6191)        
040500     MOVE INLA-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-NEW          
040600                                                         (IX-6191)        
040700     MOVE INLA-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-NEW              
040800                                                         (IX-6191)        
040900     MOVE INLA-RAD-KVINLART      TO MOD6191-MID-KVINLART-NEW              
041000                                                         (IX-6191)        
041100     .                                                                    
041200     EJECT                                                                
041300 BD-RED-LISTRAD SECTION.                                                  
041400                                                                          
041500     MOVE SPACE                  TO RLW-R01                               
041600                                                                          
041700     MOVE INLA-ART-IDARTNR       TO RLW-R01-IDARTNR                       
041800     MOVE INLA-ART-BEART         TO RLW-R01-BEART                         
041900     MOVE INLA-RAD-KVINLART      TO RLW-R01-KVINLART                      
042000                                                                          
042100     IF INLA-RAD-FLPRIO = JA                                              
042200       MOVE 'P'                  TO RLW-R01-BEPRIO                        
042300     ELSE                                                                 
042400       MOVE SPACE                TO RLW-R01-BEPRIO                        
042500     END-IF                                                               
042600                                                                          
042700     MOVE INLA-ART-BEFT          TO RLW-R01-BEFT                          
042800                                                                          
042900     IF RETL-IDLEVNR-KOLLI = SPACE                                        
043000       MOVE INLA-RAD-IDLEVNR-KOLLI TO RLW-R01-IDLEVNR-KOLLI               
043100       MOVE INLA-RAD-IDOKOLLI    TO RLW-R01-IDOKOLLI                      
043200     ELSE                                                                 
043300       MOVE INLA-ART-IDLOPNRM    TO RLW-R01-IDLOPNRM                      
043400     END-IF                                                               
043500                                                                          
043600     MOVE ZERO                   TO WS-KVINLART-VOR                       
043700     MOVE INLA-RAD-IDLEVNR-KOLLI TO W-IDLEVNR-KOLLI                       
043800     MOVE INLA-RAD-IDOKOLLI      TO W-IDOKOLLI                            
043900     PERFORM IMS-GNP-INLA-RAD-KLI                                         
044000                                                                          
044100     PERFORM UNTIL SEGMENT-SAKNAS                                         
044200                                                                          
044300       IF INLA-RAD-KDINLSTA = 'VOR'                                       
044400         ADD INLA-RAD-KVINLART   TO WS-KVINLART-VOR                       
044500       END-IF                                                             
044600                                                                          
044700       PERFORM IMS-GNP-INLA-RAD-KLI                                       
044800     END-PERFORM                                                          
044900                                                                          
045000     MOVE WS-KVINLART-VOR        TO RLW-R01-KVINLART-VOR                  
045100     .                                                                    
045200     EJECT                                                                
045300 BE-STYR-SIDA-RL SECTION.                                                 
045400                                                                          
045500     IF RLW-RADANT + RLW-RADSKIP > RLW-RADMAX                             
045600                                                                          
045700       ADD +1                    TO RLW-SIDANT                            
045800       MOVE RLW-SIDANT           TO RLW-RUB1-SIDNR                        
045900                                                                          
046000       MOVE PRT-NYSIDA-RAD4      TO PRT-RADSKIP                           
046100       MOVE RLW-RUB1             TO RLW-RAD                               
046200       PERFORM S02-PRT-WRITE                                              
046300                                                                          
046400       MOVE PRT-AFTER-2          TO PRT-RADSKIP                           
046500       MOVE RLW-RUB2             TO RLW-RAD                               
046600       PERFORM S02-PRT-WRITE                                              
046700                                                                          
046800       MOVE PRT-AFTER-2          TO PRT-RADSKIP                           
046900       MOVE RLW-RUB3             TO RLW-RAD                               
047000       PERFORM S02-PRT-WRITE                                              
047100                                                                          
047200       MOVE +9                   TO RLW-RADANT                            
047300       MOVE PRT-AFTER-2          TO RLW-RADSKIP                           
047400     END-IF                                                               
047500                                                                          
047600     MOVE RLW-RADSKIP            TO PRT-RADSKIP                           
047700     MOVE RLW-R01                TO RLW-RAD                               
047800     PERFORM S02-PRT-WRITE                                                
047900     ADD RLW-RADSKIP             TO RLW-RADANT                            
048000                                                                          
048100     MOVE PRT-AFTER-2            TO RLW-RADSKIP                           
048200     .                                                                    
048300     EJECT                                                                
048400 Z-FINIT SECTION.                                                         
048500                                                                          
048600     IF RETL-FLSLUT = JA                                                  
048700       PERFORM S03-PRT-CLOSE                                              
048800                                                                          
048900       IF IX-6191 > ZERO                                                  
049000         PERFORM S11-P-TO-P-6191                                          
049100       END-IF                                                             
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 S01-PRT-OPEN  SECTION.                                                   
049600                                                                          
049700     CALL W006PRS1 USING         PRT-SPOOL-OVR                            
049800                                 PRT-OPEN                                 
049900                                 WS-IDPRTLST                              
050000                                 PRT-PCB                                  
050100                                 PRT-FILLER                               
050200                                 PRT-FILLER                               
050300     .                                                                    
050400     SKIP3                                                                
050500 S02-PRT-WRITE SECTION.                                                   
050600                                                                          
050700     CALL W006PRS1 USING         PRT-SPOOL-OVR                            
050800                                 PRT-WRITE                                
050900                                 WS-IDPRTLST                              
051000                                 PRT-PCB                                  
051100                                 PRT-RADSKIP                              
051200                                 RLW-RAD                                  
051300     .                                                                    
051400     SKIP3                                                                
051500 S03-PRT-CLOSE SECTION.                                                   
051600                                                                          
051700     CALL W006PRS1 USING         PRT-SPOOL-OVR                            
051800                                 PRT-CLOSE                                
051900                                 WS-IDPRTLST                              
052000                                 PRT-PCB                                  
052100                                 PRT-FILLER                               
052200                                 PRT-FILLER                               
052300     .                                                                    
052400     EJECT                                                                
052500 S10-INIT-6191 SECTION.                                                   
052600                                                                          
052700     MOVE SPACE                  TO MOD6191-MID-W6I19101                  
052800     MOVE RETL-IDPGM             TO MOD6191-MID-IDPGM                     
052900     MOVE ZERO                   TO IX-6191                               
053000     .                                                                    
053100     EJECT                                                                
053200 S11-P-TO-P-6191 SECTION.                                                 
053300                                                                          
053400     MOVE IX-6191              TO MOD6191-MID-KVPOST                      
053500                                                                          
053600     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
053700                               + 17 + (MOD6191-MID-KVPOST * 64)           
053800                                                                          
053900     MOVE 'W6T191X '           TO P-TO-P-KDTRANS                          
054000     MOVE W-IDTRANS            TO P-TO-P-IDTRANS                          
054100     MOVE '1'                  TO P-TO-P-KDMFSFOR                         
054200                                                                          
054500     IF  SW-1A-6191 = JA                                                  
054600       PERFORM IMS-ISRT-ALT-MSG-6191                                      
054700       MOVE NEJ                TO SW-1A-6191                              
054800     ELSE                                                                 
054900       PERFORM IMS-PURG-ALT-MSG-6191                                      
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300* --- IMS SEKTIONER ---                                                   
055400     SKIP3                                                                
055500 IMS-ISRT-ALT-MSG-6191 SECTION.                                           
055600                                                                          
055700     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
055800     MOVE    '  '             TO    GODK-STATUSKODER                      
055900     CALL    CBLTDLI          USING ISRT 6191-PCB P-TO-P-SW               
056000     MOVE    6191-STATUS-CODE TO    STATUS-WS                             
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     SKIP3                                                                
056400 IMS-PURG-ALT-MSG-6191 SECTION.                                           
056500                                                                          
056600     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
056700     MOVE    '  '             TO    GODK-STATUSKODER                      
056800     CALL    CBLTDLI          USING PURG 6191-PCB P-TO-P-SW               
056900     MOVE    6191-STATUS-CODE TO    STATUS-WS                             
057000     PERFORM IMS-STATUSKONTROLL                                           
057100     .                                                                    
057200     EJECT                                                                
057300 IMS-GU-INLA-ART SECTION.                                                 
057400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
057500          DELIMITED BY SIZE INTO SSA1                                     
057600     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
057700          DELIMITED BY SIZE INTO SSA2                                     
057800     MOVE '  ' TO GODK-STATUSKODER                                        
057900     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1 SSA2                 
058000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     .                                                                    
058300     SKIP3                                                                
058400 IMS-GHNP-INLA-RAD SECTION.                                               
058500     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
058600          DELIMITED BY SIZE INTO SSA1                                     
058700     MOVE '  ' TO GODK-STATUSKODER                                        
058800     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA2 SSA1                   
058900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
059000     PERFORM IMS-STATUSKONTROLL                                           
059100     .                                                                    
059200     SKIP3                                                                
059300 IMS-GNP-INLA-RAD-KLI SECTION.                                            
059400     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-KOLLI-X                       
059500                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
059600          DELIMITED BY SIZE INTO SSA1                                     
059700     MOVE '  GE' TO GODK-STATUSKODER                                      
059800     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA2 SSA1                    
059900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
060100     .                                                                    
060200     EJECT                                                                
060300 IMS-REPL-INLA-RAD SECTION.                                               
060400                                                                          
060500     MOVE '  ' TO GODK-STATUSKODER                                        
060600     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA2                        
060700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
060800     PERFORM IMS-STATUSKONTROLL                                           
060900     .                                                                    
061000     EJECT                                                                
061100 IMS-STATUSKONTROLL SECTION.                                              
061200                                                                          
061300     SET STATUS-IX TO 1                                                   
061400     SEARCH GODK-STATUS                                                   
061500       AT END                                                             
061600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
061700         DELIMITED BY SIZE INTO FELTEXT                                   
061800         CALL FELLOG                                                      
061900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062000         CONTINUE                                                         
062100     END-SEARCH                                                           
062200     .                                                                    
