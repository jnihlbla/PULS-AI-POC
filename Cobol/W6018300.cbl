000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6018300.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   99/09/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING FÖRPACKNINGSINSTRUKTION                              
000900*        PROGRAMMET UPPDATERAR WDD1                                       
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W6T183                                              
001300*        MID:         W6I18301                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W6O18301                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6018300'.            
002600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  NEJ                         PIC X       VALUE 'N'.                   
002900 77  RAD-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
003000 77  RAD-IX-MAX                  PIC S9(3)   VALUE +10  COMP-3.           
003100 77  REGNR-SOEK                  PIC X       VALUE SPACE.                 
003200 77  ARTNR-SOEK                  PIC X       VALUE SPACE.                 
003300 77  LEVNR-SOEK                  PIC X       VALUE SPACE.                 
003400 77  ARTNR-LEVNR-SOEK            PIC X       VALUE SPACE.                 
003500 77  WS-NYUPPLAGG                PIC X       VALUE SPACE.                 
003600 77  BORTTAG                     PIC X       VALUE 'B'.                   
003700 77  UPPDATERING                 PIC X       VALUE 'R'.                   
003800 77  KOPIERING-ARTNR             PIC X       VALUE 'A'.                   
003900 77  KOPIERING-LEVNR             PIC X       VALUE 'L'.                   
004000 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
004100 77  SW-EAMID                    PIC X       VALUE 'N'.                   
004200 77  WS-NYTT-IDFPINST            PIC S9(7)   VALUE ZERO.                  
004300 77  SW-KOLLA-IDPERSON           PIC X       VALUE 'N'.                   
004400 77  WS-IDUSER                   PIC X(3)    VALUE SPACE.                 
004500 77  WS-TEXT-FINNS               PIC X       VALUE SPACE.                 
004600 77  WS-MOD-INFO                 PIC X(55)   VALUE SPACE.                 
004700                                                                          
004800 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
005100     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
005200                                                                          
005300 77  UPPDAT-SW                   PIC X       VALUE SPACE.                 
005400     88  BORT                                VALUE 'B'.                   
005500     88  UPPDAT                              VALUE 'R'.                   
005600     88  KOP-ARTNR                           VALUE 'A'.                   
005700     88  KOP-LEVNR                           VALUE 'L'.                   
005800                                                                          
005900 01  MOD-ARTNR-TEXT.                                                      
006000     03 FILLER                   PIC X(7)    VALUE 'Artnr. '.             
006100     03 ARTNR-TEXT               PIC Z(9).                                
006200                                                                          
006300 01  MOD-LEVNR-TEXT.                                                      
006400     03 FILLER                   PIC X(7)    VALUE 'Levnr. '.             
006500     03 LEVNR-TEXT               PIC X(5)    VALUE SPACE.                 
006600     03 FILLER                   PIC X(4)    VALUE SPACE.                 
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007300     88  NYCKLAR-OK                          VALUE 'J'.                   
007400     88  NYCKLAR-FEL                         VALUE 'N'.                   
007500     EJECT                                                                
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  EGEN-MID                            VALUE '6183'.                
007800     88  HOPP-MID                            VALUE '6185'.                
007900     88  GODK-MID                            VALUE '6181' '6182'          
008000                                                   '6183' '6184'          
008100                                                   '6185'.                
008200     88  HELP-MID                            VALUE '0551'.                
008300                                                                          
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERELLA-SUBPROGRAM.                                                
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200*01 -COPY WMEDAREA                                                        
009300     SKIP3                                                                
009400 01  MESSAGE-CODES.                                                       
009500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009700     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
009800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010300     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
010400     03  INF-PRINTING-STARTED    PIC X(3)    VALUE '202'.                 
010500     SKIP3                                                                
010600 01  MESSAGE-TEXT1.                                                       
010700     03  UPPDATERA-NYTT-REGNR    PIC X(20) VALUE                          
010800         'UPPDATERA NYTT REGNR'.                                          
010900     EJECT                                                                
011000 01  SPAR-AREA.                                                           
011100     03  SPAR-IDTRANS            PIC X(4).                                
011200     03  SPAR-IDFPINST-ENTER     PIC 9(7).                                
011300     03  SPAR-IDFPINST-NEXT      PIC 9(7).                                
011400     03  SPAR-FLNY-BEKR          PIC X.                                   
011500     03  FILLER                  PIC X(1000).                             
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000     SKIP3                                                                
012100*01 -COPY WMSGINIT                                                        
012200     EJECT                                                                
012300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012600     SKIP3                                                                
012700*01  MID -COPY W6I18301                                                   
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013000     SKIP3                                                                
013100*01  -COPY WMSGAREA                                                       
013200     EJECT                                                                
013300     03  MOD REDEFINES MSG-AREA.                                          
013400*      05  -COPY W6O18301                                                 
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE '618A-MID   '.         
013700     -COPY W6I18A01  -PRE 618A-                                           
013800     EJECT                                                                
013900 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
014000     SKIP3                                                                
014100 01      P-TO-P-SW.                                                       
014200                                                                          
014300  02     P-TO-P-KVLL             PIC S9(4)   COMP SYNC.                   
014400  02     P-TO-P-KDZ1             PIC X(1)    VALUE LOW-VALUE.             
014500  02     P-TO-P-KDZ2             PIC X(1)    VALUE LOW-VALUE.             
014600  02     P-TO-P-KDTRANS          PIC X(8).                                
014700  02     P-TO-P-IDTRANS          PIC X(4).                                
014800  02     P-TO-P-KDMFSFOR         PIC X(1).                                
014900  02     P-TO-P-DATA             PIC X(4079).                             
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015200     SKIP3                                                                
015300*01  -COPY WMFSAREA                                                       
015400     EJECT                                                                
015500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015800*                                                                         
015900 01  NYCKLAR-TILL-DLI.                                                    
016000     03  W-IDFPINST-X.                                                    
016100         05  W-IDFPINST          PIC S9(7)   VALUE ZERO COMP-3.           
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400     03  W-IDARTNR-KOP-X.                                                 
016500         05  W-IDARTNR-KOP       PIC S9(9)   VALUE ZERO COMP-3.           
016510     03  W-IDLAND-X.                                                      
016520         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
016600     03  W-IDLEVNR-X.                                                     
016700         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
016800     03  W-IDLEVNR-KOP-X.                                                 
016900         05  W-IDLEVNR-KOP       PIC  X(5)   VALUE SPACE.                 
017000     03  W-WDD1A1KY-MIN.                                                  
017100         05  W-SEQA-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.           
017200         05  W-SEQA-IDFPINST-MIN  PIC S9(7)  VALUE ZERO COMP-3.           
017300     03  W-WDD1A1KY-MAX.                                                  
017400         05  W-SEQA-IDARTNR-MAX   PIC S9(9)  VALUE ZERO COMP-3.           
017500         05  W-SEQA-IDPFINST-MAX  PIC S9(7)  VALUE 9999999 COMP-3.        
017600     03  W-WDD1B1KY-MIN.                                                  
017700         05  W-SEQB-IDLEVNR-MIN   PIC  X(5)  VALUE SPACE.                 
017800         05  W-SEQB-IDFPINST-MIN  PIC S9(7)  VALUE ZERO COMP-3.           
017900     03  W-WDD1B1KY-MAX.                                                  
018000         05  W-SEQB-IDLEVNR-MAX   PIC  X(5)  VALUE SPACE.                 
018100         05  W-SEQB-IDFPINST-MAX  PIC S9(7)  VALUE 9999999 COMP-3.        
018200     03  W-6311-KEY-X.                                                    
018300         05  FILLER               PIC X(4)   VALUE '6311'.                
018400         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
018500     03  W-KDARBTYP-X.                                                    
018600         05    W-KDARBTYP         PIC X(8)   VALUE 'QUAL'.                
018700     03  W-IDPERSON-X.                                                    
018800         05    W-IDPERSON         PIC S9(3)   COMP-3 VALUE +0.            
018900                                                                          
019000     EJECT                                                                
019100*    --- STATUS-KOD FRÅN IMS                                              
019200 01  STATUS-WS                   PIC XX.                                  
019300     88  SEGMENT-FINNS                       VALUE '  '.                  
019400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019600     SKIP2                                                                
019700 01  GODK-STATUSKODER.                                                    
019800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019900     SKIP3                                                                
020000 01  SSA1                        PIC X(64).                               
020100 01  SSA2                        PIC X(64).                               
020200     EJECT                                                                
020300*    --- IMS FUNKTIONSKODER                                               
020400*01  -COPY W0003                                                          
020500     EJECT                                                                
020600*    ---  DLI INPUT-OUTPUT AREA                                           
020700                                                                          
020800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD101'.                      
020900 01  DLI-IO-WDD101.                                                       
021000*    03  -COPY WDD101                                                     
021100     EJECT                                                                
021200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD111'.                      
021300 01  DLI-IO-WDD111.                                                       
021400*    03  -COPY WDD111                                                     
021500     EJECT                                                                
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-NY-WDD101'.                   
021700 01  DLI-IO-NY-WDD101.                                                    
021800*    03  -COPY WDD101   -PRE NY-                                          
021900     EJECT                                                                
022000 01  FILLER         PIC X(16) VALUE 'DLI-IO-NY-WDD111'.                   
022100 01  DLI-IO-NY-WDD111.                                                    
022200*    03  -COPY WDD111   -PRE NY-                                          
022300     EJECT                                                                
022400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD1A'.                       
022500 01  DLI-IO-WDD1A.                                                        
022600*    03  -COPY WDD1A1                                                     
022700     EJECT                                                                
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD1B'.                       
022900 01  DLI-IO-WDD1B.                                                        
023000*    03  -COPY WDD1B1                                                     
023100     EJECT                                                                
023200 01  FILLER         PIC X(17) VALUE 'DLI-IO-KOP-WDD101'.                  
023300 01  DLI-IO-KOP-WDD101.                                                   
023400*    03  -COPY WDD101   -PRE KOP-                                         
023500     EJECT                                                                
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301'.                      
023700 01  DLI-IO-WDT301.                                                       
023800*    03  -COPY WDT301                                                     
023900     EJECT                                                                
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDt311'.                      
024100 01  DLI-IO-WDT311.                                                       
024200*    03  -COPY WDT311                                                     
024300     EJECT                                                                
024400 01  FILLER         PIC X(16) VALUE 'DLI-IO-6312'.                        
024500 01  DLI-IO-6312.                                                         
024600*    03  -COPY WDGX6312                                                   
024700     EJECT                                                                
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
024900 01  DLI-IO-WDP311.                                                       
025000*    03  -COPY WDP311                                                     
025100     EJECT                                                                
025200 LINKAGE SECTION.                                                         
025300*01  -COPY W0009  -PRE MSG-                                               
025400*01  -COPY W0009  -PRE 618A-                                              
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE USEA-                                              
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008  -PRE WDD1-                                              
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200*01  -COPY W0008  -PRE WDD1A-                                             
026300     05  FILLER                  PIC X.                                   
026400     EJECT                                                                
026500*01  -COPY W0008  -PRE WDD1ASEQ-                                          
026600     05  FILLER                  PIC X.                                   
026700     EJECT                                                                
026800*01  -COPY W0008  -PRE WDT3-                                              
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008  -PRE 6312-                                              
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE WDP3-                                              
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE WDD1B-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000 PROCEDURE DIVISION  USING MSG-PCB 618A-PCB USEA-PCB WDD1-PCB             
028100                           WDD1A-PCB WDD1ASEQ-PCB WDT3-PCB                
028200                           6312-PCB WDP3-PCB WDD1B-PCB.                   
028300 MAIN SECTION.                                                            
028400     ENTRY 'DLITCBL' USING MSG-PCB 618A-PCB USEA-PCB WDD1-PCB             
028500                           WDD1A-PCB WDD1ASEQ-PCB WDT3-PCB                
028600                           6312-PCB WDP3-PCB WDD1B-PCB.                   
028700                                                                          
028800     PERFORM IMS-GET-MSG                                                  
028900     IF SEGMENT-FINNS                                                     
029000       PERFORM A-INIT                                                     
029100       PERFORM B-KOLLA-NYCKLAR                                            
029200       IF NYCKLAR-OK                                                      
029300         IF MFS-UPDATE                                                    
029400           PERFORM G-KOLLA-INPUT                                          
029500           IF INDATA-OK                                                   
029600             PERFORM H-UPPDATERA                                          
029700           END-IF                                                         
029800         ELSE                                                             
029900           IF MFS-FIRST                                                   
030000             PERFORM C-FOERSTA-SIDA                                       
030100           ELSE                                                           
030200             IF MFS-NEXT                                                  
030300                PERFORM D-NAESTA-SIDA                                     
030400             ELSE                                                         
030500                IF MFS-PRINT                                              
030600                   PERFORM L-PRINTA                                       
030700                ELSE                                                      
030800                   PERFORM E-SAMMA-SIDA                                   
030900                END-IF                                                    
031000             END-IF                                                       
031100           END-IF                                                         
031200         END-IF                                                           
031300         PERFORM F-LAES-VISA-INFO                                         
031400       END-IF                                                             
031500       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O18301 + 4                      
031600       PERFORM IMS-INSERT-MSG                                             
031700     END-IF                                                               
031800                                                                          
031900     MOVE ZERO TO RETURN-CODE                                             
032000     GOBACK                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 A-INIT SECTION.                                                          
032400                                                                          
032500     IF MSG-DUBBLA-TRANSKODER                                             
032600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I18301                 
032700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
032800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032900     ELSE                                                                 
033000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I18301                  
033100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
033200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033300     END-IF                                                               
033400                                                                          
033500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
033700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033800                                                                          
033900     MOVE LOW-VALUE TO MSG-AREA                                           
034000     MOVE 'W6O183N1' TO MFS-IDMOD                                         
034100     MOVE '6183' TO MOD-IDTRANS                                           
034200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034300                                                                          
034400     IF MSGI-IDLAND-SPR = 'SE'                                            
034500        MOVE '0' TO MFS-KDHUVOMR                                          
034600     END-IF                                                               
034700                                                                          
034800     IF EGEN-MID OR HELP-MID                                              
034900       CONTINUE                                                           
035000     ELSE                                                                 
035100       MOVE SPACE TO MFS-KDTRTYP                                          
035200       MOVE '7' TO MFS-IDPFK                                              
035300     END-IF                                                               
035400                                                                          
035500     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
035600     .                                                                    
035700     EJECT                                                                
035800 B-KOLLA-NYCKLAR SECTION.                                                 
035900                                                                          
036000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036100     MOVE '001'             TO MSGI-KDCALL                                
036200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036400     MOVE '6183'            TO MSGI-IDTRANS                               
036500     IF EGEN-MID OR HOPP-MID                                              
036600        MOVE MID-IDARTNR-IN        TO MSGI-IDARTNR                        
036700        MOVE MID-IDLEVNR-IN        TO MSGI-IDLEVNR                        
036800        IF HOPP-MID                                                       
036900          INSPECT MID-IDFPINST-IN REPLACING LEADING SPACE BY ZERO         
037000          IF MID-IDFPINST-IN NUMERIC                                      
037100             MOVE MID-IDFPINST-IN  TO MSGI-IDFPINST                       
037200          END-IF                                                          
037300        ELSE                                                              
037400           MOVE MID-IDFPINST-IN    TO MSGI-IDFPINST                       
037500        END-IF                                                            
037600     END-IF                                                               
037700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
037900                                                                          
038000     MOVE JA TO NYCKLAR-SW                                                
038100                                                                          
038200*    -- KONTROLL AV IDFPINST                                              
038300     MOVE MFS-RENSA-FAELT TO MOD-IDFPINST-IN                              
038400     IF MID-IDFPINST-IN NOT = ALL '+'                                     
038500       MOVE '7'         TO MFS-IDPFK                                      
038600       MOVE SPACE       TO MFS-KDTRTYP                                    
038700     END-IF                                                               
038800     INSPECT MSGI-IDFPINST REPLACING LEADING SPACE BY ZERO                
038900     IF MSGI-IDFPINST NUMERIC                                             
039000       MOVE MSGI-IDFPINST TO W-IDFPINST                                   
039100     ELSE                                                                 
039200       MOVE NEJ TO NYCKLAR-SW                                             
039300     END-IF                                                               
039400                                                                          
039500*    -- KONTROLL AV IDARTNR                                               
039600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
039700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
039800       MOVE '7'         TO MFS-IDPFK                                      
039900       MOVE SPACE       TO MFS-KDTRTYP                                    
040000     END-IF                                                               
040100     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
040200     IF MSGI-IDARTNR NUMERIC                                              
040300       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
040400     ELSE                                                                 
040500       MOVE NEJ TO NYCKLAR-SW                                             
040600     END-IF                                                               
040700                                                                          
040800*    -- KONTROLL AV IDLEVNR                                               
040900     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
041000     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
041100       MOVE '7'         TO MFS-IDPFK                                      
041200       MOVE SPACE       TO MFS-KDTRTYP                                    
041300     END-IF                                                               
041400     MOVE MSGI-IDLEVNR  TO W-IDLEVNR                                      
041500                                                                          
041600     MOVE NEJ TO REGNR-SOEK                                               
041700                 ARTNR-SOEK                                               
041800                 LEVNR-SOEK                                               
041900                 ARTNR-LEVNR-SOEK                                         
042000                                                                          
042100     IF NYCKLAR-OK                                                        
042200        IF MSGI-IDFPINST NOT = ZERO                                       
042300           MOVE JA TO REGNR-SOEK                                          
042400           IF MSGI-IDARTNR = ZERO                                         
042500              CONTINUE                                                    
042600           ELSE                                                           
042700              MOVE NEJ TO NYCKLAR-SW                                      
042800           END-IF                                                         
042900           IF MSGI-IDLEVNR = SPACE                                        
043000              CONTINUE                                                    
043100           ELSE                                                           
043200              MOVE NEJ TO NYCKLAR-SW                                      
043300           END-IF                                                         
043400        ELSE                                                              
043500           IF MSGI-IDARTNR = ZERO                                         
043600              IF MSGI-IDLEVNR = SPACE                                     
043700                 MOVE NEJ TO NYCKLAR-SW                                   
043800              ELSE                                                        
043900                 MOVE JA TO LEVNR-SOEK                                    
044000              END-IF                                                      
044100           ELSE                                                           
044200              IF MSGI-IDLEVNR = SPACE                                     
044300                 MOVE JA TO ARTNR-SOEK                                    
044400              ELSE                                                        
044500                 MOVE JA TO ARTNR-LEVNR-SOEK                              
044600              END-IF                                                      
044700           END-IF                                                         
044800        END-IF                                                            
044900     END-IF                                                               
045000                                                                          
045100     IF GODK-MID OR NYCKLAR-OK                                            
045200       MOVE MSGI-IDFPINST TO MOD-IDFPINST-UT                              
045300       INSPECT MOD-IDFPINST-UT REPLACING LEADING ZERO BY SPACE            
045400       MOVE MSGI-IDARTNR  TO MOD-IDARTNR-UT                               
045500       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
045600       MOVE MSGI-IDLEVNR  TO MOD-IDLEVNR-UT                               
045700     ELSE                                                                 
045800       MOVE MFS-RENSA-FAELT TO MOD-IDFPINST-UT                            
045900                               MOD-IDARTNR-UT                             
046000                               MOD-IDLEVNR-UT                             
046100     END-IF                                                               
046200                                                                          
046300     IF NYCKLAR-FEL                                                       
046400        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
046500        CALL WMEDKONV USING MED-WMEDAREA                                  
046600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
046700        PERFORM MFS-RENSA-FAELT-IN                                        
046800        PERFORM MFS-RENSA-FAELT-UT                                        
046900        PERFORM MFS-RENSA-FAELT-IN-UT                                     
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 C-FOERSTA-SIDA SECTION.                                                  
047400                                                                          
047500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
047600     CALL WMEDKONV USING MED-WMEDAREA                                     
047700     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
047800     PERFORM MFS-RENSA-FAELT-IN                                           
047900     PERFORM MFS-RENSA-FAELT-IN-UT                                        
048000     .                                                                    
048100     EJECT                                                                
048200 D-NAESTA-SIDA SECTION.                                                   
048300                                                                          
048400     IF SPAR-IDTRANS = '6183'                                             
048500        IF SPAR-IDFPINST-NEXT NUMERIC                                     
048600           MOVE SPAR-IDFPINST-NEXT TO W-SEQA-IDFPINST-MIN                 
048700                                      W-SEQB-IDFPINST-MIN                 
048800        ELSE                                                              
048900           MOVE ZERO TO W-SEQA-IDFPINST-MIN                               
049000                        W-SEQB-IDFPINST-MIN                               
049100        END-IF                                                            
049200        PERFORM MFS-RENSA-FAELT-IN                                        
049300        PERFORM MFS-RENSA-FAELT-IN-UT                                     
049400     ELSE                                                                 
049500        PERFORM MFS-RENSA-FAELT-IN                                        
049600        PERFORM MFS-RENSA-FAELT-IN-UT                                     
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 E-SAMMA-SIDA SECTION.                                                    
050100                                                                          
050200     IF SPAR-IDTRANS = '6183'                                             
050300        IF SPAR-IDFPINST-ENTER NUMERIC                                    
050400           MOVE SPAR-IDFPINST-ENTER TO W-SEQA-IDFPINST-MIN                
050500                                       W-SEQB-IDFPINST-MIN                
050600        ELSE                                                              
050700           MOVE ZERO                TO W-SEQA-IDFPINST-MIN                
050800                                       W-SEQB-IDFPINST-MIN                
050900        END-IF                                                            
051000        IF MID-INDEL = ALL '+'                                            
051100           PERFORM MFS-RENSA-FAELT-IN                                     
051200           PERFORM MFS-RENSA-FAELT-IN-UT                                  
051300        ELSE                                                              
051400           MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                            
051500           CALL WMEDKONV USING MED-WMEDAREA                               
051600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
051700           PERFORM EA-MID-INDATA-TILL-MOD                                 
051800        END-IF                                                            
051900     ELSE                                                                 
052000        PERFORM MFS-RENSA-FAELT-IN                                        
052100        PERFORM MFS-RENSA-FAELT-IN-UT                                     
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 EA-MID-INDATA-TILL-MOD SECTION.                                          
052600                                                                          
052700     MOVE JA TO SW-EAMID                                                  
052800                                                                          
052900     IF MID-IDARTNR-KOP NOT = ALL '+'                                     
053000        MOVE MID-IDARTNR-KOP       TO MOD-IDARTNR-KOP                     
053100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR                    
053200        INSPECT MOD-IDARTNR-KOP REPLACING LEADING ZERO BY SPACE           
053300     ELSE                                                                 
053400        MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-KOP                     
053500     END-IF                                                               
053600                                                                          
053700     IF MID-IDLEVNR-KOP NOT = ALL '+'                                     
053800        MOVE MID-IDLEVNR-KOP       TO MOD-IDLEVNR-KOP                     
053900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR                    
054000     ELSE                                                                 
054100        MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-KOP                     
054200     END-IF                                                               
054300                                                                          
054400     IF MID-FLNY NOT = ALL '+'                                            
054500        MOVE MID-FLNY              TO MOD-FLNY                            
054600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNY-ATTR                       
054700     ELSE                                                                 
054800        MOVE MFS-RENSA-FAELT       TO MOD-FLNY                            
054900     END-IF                                                               
055000                                                                          
055100     IF MID-FLBORT NOT = ALL '+'                                          
055200        MOVE MID-FLBORT            TO MOD-FLBORT                          
055300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLBORT-ATTR                     
055400     ELSE                                                                 
055500        MOVE MFS-RENSA-FAELT       TO MOD-FLBORT                          
055600     END-IF                                                               
055700                                                                          
055800                                                                          
055900     MOVE MID-IDFPINST             TO MOD-IDFPINST                        
056000     MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-IDFPINST-ATTR                   
056100                                                                          
056200     IF MID-IDLTERM NOT = ALL '+'                                         
056300        MOVE MID-IDLTERM           TO MOD-IDLTERM                         
056400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLTERM-ATTR                    
056500     ELSE                                                                 
056600        MOVE MFS-RENSA-FAELT       TO MOD-IDLTERM                         
056700     END-IF                                                               
056800                                                                          
056900     IF MID-IDPERSON NOT = ALL '+'                                        
057000        MOVE MID-IDPERSON          TO MOD-IDPERSON                        
057100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-ATTR                   
057200        INSPECT MOD-IDPERSON REPLACING LEADING ZERO BY SPACE              
057300     ELSE                                                                 
057400        MOVE MFS-RENSA-FAELT       TO MOD-IDPERSON                        
057500     END-IF                                                               
057600                                                                          
057700     MOVE +1 TO RAD-IX                                                    
057800     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
057900        IF MID-TEFPINST(RAD-IX) NOT = ALL '+'                             
058000           MOVE MID-TEFPINST(RAD-IX)  TO MOD-TEFPINST(RAD-IX)             
058100           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFPINST-ATTR(RAD-IX)        
058200        ELSE                                                              
058300           MOVE MFS-RENSA-FAELT       TO MOD-TEFPINST(RAD-IX)             
058400        END-IF                                                            
058500        ADD +1 TO RAD-IX                                                  
058600     END-PERFORM                                                          
058700     .                                                                    
058800     EJECT                                                                
058900 F-LAES-VISA-INFO SECTION.                                                
059000                                                                          
059100     IF REGNR-SOEK = JA                                                   
059200        PERFORM FA-LAES-REGNR                                             
059300     ELSE                                                                 
059400        IF ARTNR-SOEK = JA                                                
059500           MOVE W-IDARTNR TO W-SEQA-IDARTNR-MIN                           
059600                             W-SEQA-IDARTNR-MAX                           
059700           PERFORM FB-LAES-ARTNR                                          
059800        ELSE                                                              
059900           IF ARTNR-LEVNR-SOEK = JA                                       
060000              MOVE W-IDARTNR TO W-SEQA-IDARTNR-MIN                        
060100                                W-SEQA-IDARTNR-MAX                        
060200              PERFORM FC-LAES-ARTNR-LEVNR                                 
060300           ELSE                                                           
060400              IF LEVNR-SOEK = JA                                          
060500                 MOVE W-IDLEVNR TO W-SEQB-IDLEVNR-MIN                     
060600                                   W-SEQB-IDLEVNR-MAX                     
060700                 PERFORM FD-LAES-LEVNR                                    
060800              END-IF                                                      
060900           END-IF                                                         
061000        END-IF                                                            
061100     END-IF                                                               
061200                                                                          
061300     MOVE NEJ       TO SPAR-FLNY-BEKR                                     
061400     MOVE '002'     TO MSGI-KDCALL                                        
061500     MOVE '6183'    TO SPAR-IDTRANS                                       
061600     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
061700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
061800     .                                                                    
061900     EJECT                                                                
062000 FA-LAES-REGNR SECTION.                                                   
062100                                                                          
062200     PERFORM IMS-GET-WDD101                                               
062300     IF SEGMENT-FINNS                                                     
062400        PERFORM S01-VISA-BILD                                             
062500        MOVE MFS-RENSA-FAELT TO MOD-BEFT                                  
062600     ELSE                                                                 
062700        MOVE URVAL-SAKNAS TO MED-IDMFSFEL                                 
062800        CALL WMEDKONV USING MED-WMEDAREA                                  
062900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
063000        PERFORM MFS-RENSA-FAELT-UT                                        
063100        PERFORM S02-KOLLA-IN-UT                                           
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 FB-LAES-ARTNR SECTION.                                                   
063600                                                                          
063700     PERFORM IMS-GET-WDD1A-FIRST                                          
063800     IF SEGMENT-FINNS                                                     
063900        MOVE SEQA-IDFPINST TO W-IDFPINST                                  
064000        PERFORM IMS-GET-WDD101                                            
064100                                                                          
064200        IF SEGMENT-FINNS                                                  
064300           PERFORM S01-VISA-BILD                                          
064310           MOVE 'SE'               TO W-IDLAND                            
064400           PERFORM IMS-GU-WDT311                                          
064500           IF SEGMENT-FINNS                                               
064600              MOVE FPCK-BEFT       TO MOD-BEFT                            
064700           ELSE                                                           
064800              MOVE MFS-RENSA-FAELT TO MOD-BEFT                            
064900           END-IF                                                         
065000        END-IF                                                            
065100                                                                          
065200        PERFORM IMS-GET-WDD1A-NEXT                                        
065300        IF SEGMENT-FINNS                                                  
065400           MOVE SEQA-IDFPINST TO SPAR-IDFPINST-NEXT                       
065500           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
065600           CALL WMEDKONV USING MED-WMEDAREA                               
065700           IF WS-MOD-INFO = SPACE                                         
065800              MOVE MED-TEMFSINF TO MOD-TEMFSINF                           
065900           ELSE                                                           
066000              MOVE MED-TEMFSINF TO MOD-TEMFSFEL                           
066100           END-IF                                                         
066200        ELSE                                                              
066300           MOVE ZERO TO SPAR-IDFPINST-NEXT                                
066400        END-IF                                                            
066500     ELSE                                                                 
066600        MOVE URVAL-SAKNAS TO MED-IDMFSFEL                                 
066700        CALL WMEDKONV USING MED-WMEDAREA                                  
066800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
066900        PERFORM MFS-RENSA-FAELT-UT                                        
067000        PERFORM S02-KOLLA-IN-UT                                           
067100        MOVE ZERO TO SPAR-IDFPINST-NEXT                                   
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500 FC-LAES-ARTNR-LEVNR SECTION.                                             
067600                                                                          
067700     MOVE NEJ TO TRAEFF-SW                                                
067800                                                                          
067900     PERFORM IMS-GET-WDD1A-FIRST                                          
068000     PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                       
068100        MOVE SEQA-IDFPINST TO W-IDFPINST                                  
068200        PERFORM IMS-GET-WDD101                                            
068300        IF FPI-IDLEVNR = W-IDLEVNR                                        
068400           MOVE JA TO TRAEFF-SW                                           
068500           PERFORM S01-VISA-BILD                                          
068600           MOVE MFS-RENSA-FAELT TO MOD-BEFT                               
068700        END-IF                                                            
068800        PERFORM IMS-GET-WDD1A-NEXT                                        
068900     END-PERFORM                                                          
069000                                                                          
069100     IF SEGMENT-FINNS                                                     
069200        MOVE NEJ TO TRAEFF-SW                                             
069300        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
069400           MOVE SEQA-IDFPINST TO W-IDFPINST                               
069500           PERFORM IMS-GET-WDD101                                         
069600           IF FPI-IDLEVNR = W-IDLEVNR                                     
069700              MOVE FPI-IDFPINST TO SPAR-IDFPINST-NEXT                     
069800              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
069900              CALL WMEDKONV USING MED-WMEDAREA                            
070000              IF WS-MOD-INFO = SPACE                                      
070100                MOVE MED-TEMFSINF TO MOD-TEMFSINF                         
070200              ELSE                                                        
070300                MOVE MED-TEMFSINF TO MOD-TEMFSFEL                         
070400              END-IF                                                      
070500              MOVE JA TO TRAEFF-SW                                        
070600           ELSE                                                           
070700              MOVE ZERO TO SPAR-IDFPINST-NEXT                             
070800           END-IF                                                         
070900           PERFORM IMS-GET-WDD1A-NEXT                                     
071000        END-PERFORM                                                       
071100     ELSE                                                                 
071200        IF TRAEFF-SW = NEJ                                                
071300           MOVE URVAL-SAKNAS TO MED-IDMFSFEL                              
071400           CALL WMEDKONV USING MED-WMEDAREA                               
071500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
071600           PERFORM MFS-RENSA-FAELT-UT                                     
071700           PERFORM S02-KOLLA-IN-UT                                        
071800           MOVE ZERO TO SPAR-IDFPINST-NEXT                                
071900        END-IF                                                            
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 FD-LAES-LEVNR SECTION.                                                   
072400                                                                          
072500     PERFORM IMS-GET-WDD1B-FIRST                                          
072600     IF SEGMENT-FINNS                                                     
072700        MOVE SEQB-IDFPINST TO W-IDFPINST                                  
072800        PERFORM IMS-GET-WDD101                                            
072900        IF SEGMENT-FINNS                                                  
073000           PERFORM S01-VISA-BILD                                          
073100           MOVE MFS-RENSA-FAELT TO MOD-BEFT                               
073200        END-IF                                                            
073300                                                                          
073400        PERFORM IMS-GET-WDD1B-NEXT                                        
073500        IF SEGMENT-FINNS                                                  
073600           MOVE SEQB-IDFPINST TO SPAR-IDFPINST-NEXT                       
073700           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
073800           CALL WMEDKONV USING MED-WMEDAREA                               
073900           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
074000        ELSE                                                              
074100           MOVE ZERO          TO SPAR-IDFPINST-NEXT                       
074200        END-IF                                                            
074300     ELSE                                                                 
074400        MOVE URVAL-SAKNAS TO MED-IDMFSFEL                                 
074500        CALL WMEDKONV USING MED-WMEDAREA                                  
074600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
074700        PERFORM MFS-RENSA-FAELT-UT                                        
074800        PERFORM S02-KOLLA-IN-UT                                           
074900        MOVE ZERO TO SPAR-IDFPINST-NEXT                                   
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 G-KOLLA-INPUT SECTION.                                                   
075400                                                                          
075500     MOVE JA    TO INDATA-SW                                              
075600     MOVE SPACE TO UPPDAT-SW                                              
075700     MOVE NEJ   TO WS-NYUPPLAGG                                           
075800                   SW-KOLLA-IDPERSON                                      
075900                                                                          
076000     IF MID-INDEL = ALL '+'                                               
076100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
076200       CALL WMEDKONV USING MED-WMEDAREA                                   
076300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
076400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
076500       PERFORM MFS-ROER-EJ-FAELT-IN-UT                                    
076600       MOVE NEJ TO INDATA-SW                                              
076700     ELSE                                                                 
076800                                                                          
076900       MOVE SPACE TO W-IDLEVNR-KOP                                        
077000       MOVE ZERO  to W-IDARTNR-KOP                                        
077100                                                                          
077200       IF MID-IDARTNR-KOP NOT = ALL '+'                                   
077300         INSPECT MID-IDARTNR-KOP REPLACING LEADING SPACE BY ZERO          
077400         MOVE MID-IDARTNR-KOP TO W-IDARTNR-KOP                            
077500         IF MID-IDARTNR-KOP NOT NUMERIC                                   
077600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR                     
077700           MOVE NEJ TO INDATA-SW                                          
077800         ELSE                                                             
077900           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR                   
078000         END-IF                                                           
078100       END-IF                                                             
078200                                                                          
078300       IF MID-IDLEVNR-KOP NOT = ALL '+'                                   
078400         MOVE MID-IDLEVNR-KOP TO W-IDLEVNR-KOP                            
078500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR                    
078600       END-IF                                                             
078700                                                                          
078800       IF MID-FLNY = ALL '+' OR SPACE                                     
078900          CONTINUE                                                        
079000       ELSE                                                               
079100          IF MID-FLNY = 'J' OR 'Y'                                        
079200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNY-ATTR                   
079300          ELSE                                                            
079400             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNY-ATTR                   
079500             MOVE NEJ TO INDATA-SW                                        
079600          END-IF                                                          
079700       END-IF                                                             
079800                                                                          
079900       IF MID-FLBORT = ALL '+' OR SPACE                                   
080000          CONTINUE                                                        
080100       ELSE                                                               
080200          IF MID-FLBORT = 'J' OR 'Y'                                      
080300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-ATTR                 
080400          ELSE                                                            
080500             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBORT-ATTR                 
080600             MOVE NEJ TO INDATA-SW                                        
080700          END-IF                                                          
080800       END-IF                                                             
080900                                                                          
081000       IF MID-IDLTERM NOT = ALL '+'                                       
081100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLTERM-ATTR                    
081200       END-IF                                                             
081300                                                                          
081400       IF MID-IDPERSON NOT = ALL '+'                                      
081500         INSPECT MID-IDPERSON REPLACING LEADING SPACE BY ZERO             
081600         IF MID-IDPERSON NUMERIC                                          
081700         AND MID-IDPERSON > ZERO                                          
081800            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ATTR                
081900         ELSE                                                             
082000            MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPERSON-ATTR                
082100            MOVE NEJ TO INDATA-SW                                         
082200         END-IF                                                           
082300       ELSE                                                               
082400         MOVE MFS-RENSA-FAELT TO MOD-IDPERSON                             
082500       END-IF                                                             
082600                                                                          
082700       MOVE +1 TO RAD-IX                                                  
082800       PERFORM UNTIL RAD-IX > RAD-IX-MAX                                  
082900          IF MID-TEFPINST(RAD-IX) NOT = ALL '+'                           
083000             MOVE MFS-ALFA-FAELT-RAETT                                    
083100                               TO MOD-TEFPINST-ATTR(RAD-IX)               
083200          END-IF                                                          
083300          ADD +1 TO RAD-IX                                                
083400       END-PERFORM                                                        
083500                                                                          
083600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFPINST-ATTR                    
083700                                                                          
083800       IF INDATA-FEL                                                      
083900          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
084000          CALL WMEDKONV USING MED-WMEDAREA                                
084100          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
084200          PERFORM MFS-ROER-EJ-FAELT-IN                                    
084300          PERFORM MFS-ROER-EJ-FAELT-IN-UT                                 
084400       ELSE                                                               
084500          PERFORM GA-KOLLA-UPPDAT-TYP                                     
084600          IF INDATA-FEL                                                   
084700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
084800             CALL WMEDKONV USING MED-WMEDAREA                             
084900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
085000             PERFORM MFS-ROER-EJ-FAELT-IN                                 
085100             PERFORM MFS-ROER-EJ-FAELT-IN-UT                              
085200          ELSE                                                            
085300             IF BORT OR UPPDAT                                            
085400                PERFORM GB-KOLLA-BORT-UPPDAT                              
085500                IF SEGMENT-SAKNAS                                         
085600                   IF MID-FLBORT = ALL '+' OR SPACE                       
085700                      CONTINUE                                            
085800                   ELSE                                                   
085900                      MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR          
086000                   END-IF                                                 
086100                   MOVE +1 TO RAD-IX                                      
086200                   PERFORM UNTIL RAD-IX > RAD-IX-MAX                      
086300                     IF MID-TEFPINST(RAD-IX) = ALL '+'                    
086400                        CONTINUE                                          
086500                     ELSE                                                 
086600                        MOVE MFS-ALFA-FAELT-FEL TO                        
086700                                    MOD-TEFPINST-ATTR(RAD-IX)             
086800                     END-IF                                               
086900                     ADD +1 TO RAD-IX                                     
087000                   END-PERFORM                                            
087100                   MOVE NEJ TO INDATA-SW                                  
087200                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
087300                   CALL WMEDKONV USING MED-WMEDAREA                       
087400                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
087500                   PERFORM MFS-ROER-EJ-FAELT-IN                           
087600                   PERFORM MFS-ROER-EJ-FAELT-IN-UT                        
087700                END-IF                                                    
087800             ELSE                                                         
087900                IF KOP-ARTNR OR KOP-LEVNR                                 
088000                   PERFORM GC-KOLLA-KOP                                   
088100                   IF TRAEFF-SW = JA                                      
088200                      IF MID-IDARTNR-KOP NOT = ALL '+'                    
088300                         MOVE MFS-NUM-FAELT-FEL                           
088400                                        TO MOD-IDARTNR-ATTR               
088500                      END-IF                                              
088600                      IF MID-IDLEVNR-KOP NOT = ALL '+'                    
088700                         MOVE MFS-ALFA-FAELT-FEL                          
088800                                        TO MOD-IDLEVNR-ATTR               
088900                      END-IF                                              
089000                      MOVE NEJ TO INDATA-SW                               
089100                      MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL           
089200                      CALL WMEDKONV USING MED-WMEDAREA                    
089300                      MOVE MED-MFSFEL TO MOD-TEMFSFEL                     
089400                      PERFORM MFS-ROER-EJ-FAELT-IN                        
089500                      PERFORM MFS-ROER-EJ-FAELT-IN-UT                     
089600                   END-IF                                                 
089700                ELSE                                                      
089800                   IF WS-NYUPPLAGG = JA                                   
089900                      PERFORM GE-KOLLA-NYUPPLAGG                          
090000                      IF TRAEFF-SW = JA                                   
090100                      OR WS-TEXT-FINNS = NEJ                              
090200                         MOVE NEJ TO INDATA-SW                            
090300                         MOVE ERR-CORR-HILITE-FLDS                        
090400                                    TO MED-IDMFSFEL                       
090500                         CALL WMEDKONV USING MED-WMEDAREA                 
090600                         MOVE MED-MFSFEL TO MOD-TEMFSFEL                  
090700                         PERFORM MFS-ROER-EJ-FAELT-IN                     
090800                         PERFORM MFS-ROER-EJ-FAELT-IN-UT                  
090900                      END-IF                                              
091000                   END-IF                                                 
091100                END-IF                                                    
091200             END-IF                                                       
091300          END-IF                                                          
091400          IF INDATA-OK                                                    
091500             IF SW-KOLLA-IDPERSON = JA                                    
091600                PERFORM GD-KOLLA-PERSONKOD                                
091700             END-IF                                                       
091800             IF INDATA-FEL                                                
091900                MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                 
092000                CALL WMEDKONV USING MED-WMEDAREA                          
092100                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
092200                PERFORM MFS-ROER-EJ-FAELT-IN                              
092300                PERFORM MFS-ROER-EJ-FAELT-IN-UT                           
092400             END-IF                                                       
092500          END-IF                                                          
092600       END-IF                                                             
092700       IF SPAR-IDTRANS = '6183'                                           
092800          IF SPAR-IDFPINST-ENTER NUMERIC                                  
092900             MOVE SPAR-IDFPINST-ENTER TO W-SEQA-IDFPINST-MIN              
093000                                         W-SEQB-IDFPINST-MIN              
093100          ELSE                                                            
093200             MOVE ZERO                TO W-SEQA-IDFPINST-MIN              
093300                                         W-SEQB-IDFPINST-MIN              
093400          END-IF                                                          
093500       end-if                                                             
093600                                                                          
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 GA-KOLLA-UPPDAT-TYP SECTION.                                             
094100                                                                          
094200     IF W-IDARTNR-KOP NOT = ZERO                                          
094300        IF W-IDLEVNR-KOP = SPACE                                          
094400           MOVE KOPIERING-ARTNR TO UPPDAT-SW                              
094500        ELSE                                                              
094600           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR                    
094700           MOVE NEJ TO INDATA-SW                                          
094800        END-IF                                                            
094900     ELSE                                                                 
095000        IF W-IDLEVNR-KOP NOT = SPACE                                      
095100           MOVE KOPIERING-LEVNR TO UPPDAT-SW                              
095200        END-IF                                                            
095300     END-IF                                                               
095400                                                                          
095500     IF KOP-ARTNR OR KOP-LEVNR                                            
095600        IF MID-FLNY = ALL '+'                                             
095700        OR MID-FLNY = SPACE                                               
095800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNY-ATTR                     
095900           MOVE NEJ TO INDATA-SW                                          
096000        ELSE                                                              
096100           IF MID-FLNY = 'J' OR 'Y'                                       
096200              MOVE JA TO WS-NYUPPLAGG                                     
096300           ELSE                                                           
096400              MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNY-ATTR                  
096500              MOVE NEJ TO INDATA-SW                                       
096600           END-IF                                                         
096700        END-IF                                                            
096800        IF MID-FLBORT = ALL '+'                                           
096900        OR MID-FLBORT = SPACE                                             
097000           CONTINUE                                                       
097100        ELSE                                                              
097200           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBORT-ATTR                   
097300           MOVE NEJ TO INDATA-SW                                          
097400        END-IF                                                            
097500     ELSE                                                                 
097600        IF MID-FLBORT = 'J' OR 'Y'                                        
097700           MOVE BORTTAG TO UPPDAT-SW                                      
097800           IF MID-FLNY = 'J' OR 'Y'                                       
097900              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNY-ATTR                    
098000              MOVE NEJ TO INDATA-SW                                       
098100           END-IF                                                         
098200        ELSE                                                              
098300           IF MID-FLNY = 'J' OR 'Y'                                       
098400              MOVE JA TO WS-NYUPPLAGG                                     
098500           END-IF                                                         
098600        END-IF                                                            
098700     END-IF                                                               
098800                                                                          
098900     IF MID-IDPERSON = ALL '+' OR SPACE                                   
099000        IF WS-NYUPPLAGG = JA                                              
099100        OR KOP-ARTNR OR KOP-LEVNR                                         
099200           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDPERSON-ATTR                
099300           MOVE NEJ                   TO INDATA-SW                        
099400        END-IF                                                            
099500     ELSE                                                                 
099600        IF WS-NYUPPLAGG = JA                                              
099700        OR KOP-ARTNR OR KOP-LEVNR                                         
099800           MOVE JA TO SW-KOLLA-IDPERSON                                   
099900           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDPERSON-ATTR                
100000        ELSE                                                              
100100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDPERSON-ATTR                
100200           MOVE NEJ                   TO INDATA-SW                        
100300        END-IF                                                            
100400     END-IF                                                               
100500                                                                          
100600     MOVE +1 TO RAD-IX                                                    
100700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
100800        IF MID-TEFPINST(RAD-IX) = ALL '+'                                 
100900           CONTINUE                                                       
101000        ELSE                                                              
101100           IF BORT                                                        
101200              MOVE MFS-ALFA-FAELT-FEL TO                                  
101300                         MOD-TEFPINST-ATTR(RAD-IX)                        
101400              MOVE NEJ TO INDATA-SW                                       
101500           ELSE                                                           
101600              IF KOP-ARTNR OR KOP-LEVNR                                   
101700              OR WS-NYUPPLAGG = JA                                        
101800                 MOVE MFS-ALFA-FAELT-RAETT TO                             
101900                         MOD-TEFPINST-ATTR(RAD-IX)                        
102000              ELSE                                                        
102100                 MOVE UPPDATERING TO UPPDAT-SW                            
102200                 MOVE MFS-ALFA-FAELT-RAETT TO                             
102300                         MOD-TEFPINST-ATTR(RAD-IX)                        
102400              END-IF                                                      
102500           END-IF                                                         
102600        END-IF                                                            
102700        ADD +1 TO RAD-IX                                                  
102800     END-PERFORM                                                          
102900     .                                                                    
103000     EJECT                                                                
103100 GB-KOLLA-BORT-UPPDAT SECTION.                                            
103200                                                                          
103300***  VID REGNR-SOEK FINNS NYCKEL I W-IDFPINST                             
103400***  VID ARTNR-SOEK/ARTNR-LEVNR-SOEK/LEVNR-SOEK I MID-IDFPINST            
103500                                                                          
103600     IF ARTNR-SOEK = JA                                                   
103700     OR ARTNR-LEVNR-SOEK = JA                                             
103800     OR LEVNR-SOEK = JA                                                   
103900        INSPECT MID-IDFPINST REPLACING LEADING SPACE                      
104000                BY ZERO                                                   
104100        MOVE MID-IDFPINST TO W-IDFPINST                                   
104200     END-IF                                                               
104300     PERFORM IMS-GET-WDD101                                               
104400     .                                                                    
104500     EJECT                                                                
104600 GC-KOLLA-KOP SECTION.                                                    
104700******************************************************************        
104800* KOPIERING TILL ARTNR - GER LEVNR blank - ARTIKEL FÅR INTE FINNAS        
104900*                        MED LEVNR blank                                  
105000* KOPIERING TILL LEVNR - GER LEVNR (W-IDLEVNR-KOP)                        
105100******************************************************************        
105200                                                                          
105300     MOVE NEJ TO TRAEFF-SW                                                
105400                                                                          
105500     IF KOP-ARTNR                                                         
105600        INSPECT MID-IDARTNR-KOP REPLACING LEADING SPACE BY ZERO           
105700        MOVE MID-IDARTNR-KOP TO W-IDARTNR-KOP                             
105800                                                                          
105900        PERFORM IMS-GU-WDD101-ASEQ                                        
106000        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
106100           IF KOP-FPI-IDLEVNR = SPACE                                     
106200              MOVE JA TO TRAEFF-SW                                        
106300           END-IF                                                         
106400           PERFORM IMS-GN-WDD101-ASEQ                                     
106500        END-PERFORM                                                       
106600                                                                          
106700        PERFORM IMS-GU-WDT301                                             
106800        IF NOT SEGMENT-FINNS                                              
106900           MOVE JA TO TRAEFF-SW                                           
107000        END-IF                                                            
107100     END-IF                                                               
107200     .                                                                    
107300     EJECT                                                                
107400 GD-KOLLA-PERSONKOD SECTION.                                              
107500                                                                          
107600     INSPECT MID-IDPERSON REPLACING LEADING SPACE BY ZERO                 
107700     MOVE MID-IDPERSON TO W-IDPERSON                                      
107800     PERFORM IMS-GET-WDP311                                               
107900     IF SEGMENT-SAKNAS                                                    
108000        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-ATTR                      
108100        MOVE NEJ TO INDATA-SW                                             
108200     ELSE                                                                 
108300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-ATTR                    
108400     END-IF                                                               
108500     .                                                                    
108600     EJECT                                                                
108700 GE-KOLLA-NYUPPLAGG SECTION.                                              
108800******************************************************************        
108900* NYUPPLAGG TILL ARTNR - GER LEVNR blank - ARTIKEL FÅR INTE FINNAS        
109000*                        MED LEVNR blank                                  
109100* NYUPPLAGG TILL LEVNR - GER LEVNR FRÅN NYCKEL                            
109200******************************************************************        
109300                                                                          
109400     MOVE NEJ TO TRAEFF-SW                                                
109500                                                                          
109600     IF REGNR-SOEK = JA                                                   
109700     OR ARTNR-LEVNR-SOEK = JA                                             
109800        MOVE JA TO TRAEFF-SW                                              
109900        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNY-ATTR                          
110000     END-IF                                                               
110100                                                                          
110200     IF ARTNR-SOEK = JA                                                   
110300        MOVE W-IDARTNR TO W-IDARTNR-KOP                                   
110400        PERFORM IMS-GU-WDD101-ASEQ                                        
110500        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
110600           IF KOP-FPI-IDLEVNR = SPACE                                     
110700              MOVE JA TO TRAEFF-SW                                        
110800              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNY-ATTR                    
110900           END-IF                                                         
111000           PERFORM IMS-GN-WDD101-ASEQ                                     
111100        END-PERFORM                                                       
111200     END-IF                                                               
111300     MOVE ZERO TO W-IDARTNR-KOP                                           
111400                  W-IDLEVNR-KOP                                           
111500                                                                          
111600     MOVE NEJ TO WS-TEXT-FINNS                                            
111700     MOVE +1 TO RAD-IX                                                    
111800     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
111900        IF MID-TEFPINST(RAD-IX) = ALL '+'                                 
112000           MOVE MFS-RENSA-FAELT TO MOD-TEFPINST(RAD-IX)                   
112100        ELSE                                                              
112200           MOVE MFS-ALFA-FAELT-RAETT TO                                   
112300                      MOD-TEFPINST-ATTR(RAD-IX)                           
112400           MOVE JA TO WS-TEXT-FINNS                                       
112500        END-IF                                                            
112600        ADD +1 TO RAD-IX                                                  
112700     END-PERFORM                                                          
112800     IF WS-TEXT-FINNS = NEJ                                               
112900        MOVE MFS-ALFA-FAELT-FEL TO MOD-TEFPINST-ATTR(1)                   
113000     END-IF                                                               
113100     .                                                                    
113200     EJECT                                                                
113300 H-UPPDATERA SECTION.                                                     
113400******************************************************************        
113500* NYUPPLÄGG                                                               
113600* - KOPIERING TILL IDARTNR (LEVNR blank)                                  
113700* - KOPIERING TILL IDLEVNR                                                
113800* - 'NY' NYUPPLÄGG                                                        
113900* BORTTAG                                                                 
114000* UPPDATERING                                                             
114100******************************************************************        
114200                                                                          
114300***  VID REGNR-SOEK FINNS NYCKEL I W-IDFPINST                             
114400***  VID ARTNR-SOEK/ARTNR-LEVNR-SOEK/LEVNR-SOEK I MID-IDFPINST            
114500                                                                          
114600     IF BORT                                                              
114700        PERFORM HA-BORTTAG                                                
114800     ELSE                                                                 
114900        IF UPPDAT                                                         
115000           PERFORM HB-UPPDATERING                                         
115100        ELSE                                                              
115200           IF WS-NYUPPLAGG = JA                                           
115300              PERFORM HE-NYTT-IDFPINST                                    
115400              IF KOP-ARTNR OR KOP-levnr                                   
115500                 PERFORM HC-KOPIERA                                       
115600              ELSE                                                        
115700                 PERFORM HD-NYUPPLAGG                                     
115800              END-IF                                                      
115900           END-IF                                                         
116000        END-IF                                                            
116100     END-IF                                                               
116200                                                                          
116300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
116400     CALL WMEDKONV USING MED-WMEDAREA                                     
116500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
116600                        WS-MOD-INFO                                       
116700     PERFORM MFS-FORM-ATTR                                                
116800     PERFORM MFS-RENSA-FAELT-IN                                           
116900     PERFORM MFS-RENSA-FAELT-IN-UT                                        
117000     .                                                                    
117100     EJECT                                                                
117200 HA-BORTTAG SECTION.                                                      
117300                                                                          
117400     IF ARTNR-SOEK = JA                                                   
117500     OR ARTNR-LEVNR-SOEK = JA                                             
117600     OR LEVNR-SOEK = JA                                                   
117700        INSPECT MID-IDFPINST REPLACING LEADING SPACE BY ZERO              
117800        MOVE MID-IDFPINST TO W-IDFPINST                                   
117900     END-IF                                                               
118000                                                                          
118100     PERFORM IMS-GET-WDD101                                               
118200     IF SEGMENT-FINNS                                                     
118300        PERFORM IMS-DLET-WDD101                                           
118400     END-IF                                                               
118500     .                                                                    
118600     EJECT                                                                
118700 HB-UPPDATERING SECTION.                                                  
118800                                                                          
118900     IF ARTNR-SOEK = JA                                                   
119000     OR ARTNR-LEVNR-SOEK = JA                                             
119100     OR LEVNR-SOEK = JA                                                   
119200        INSPECT MID-IDFPINST REPLACING LEADING SPACE BY ZERO              
119300        MOVE MID-IDFPINST TO W-IDFPINST                                   
119400     END-IF                                                               
119500                                                                          
119600     PERFORM IMS-GET-WDD101                                               
119700     IF SEGMENT-FINNS                                                     
119800        MOVE +1 TO RAD-IX                                                 
119900        PERFORM UNTIL RAD-IX > RAD-IX-MAX                                 
120000           IF MID-TEFPINST(RAD-IX) = ALL '+'                              
120100              CONTINUE                                                    
120200           ELSE                                                           
120300              MOVE MID-TEFPINST(RAD-IX) TO                                
120400                             FPI-TEFPINST(RAD-IX)                         
120500              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
120600                             MOD-TEFPINST-ATTR(RAD-IX)                    
120700           END-IF                                                         
120800           ADD +1 TO RAD-IX                                               
120900        END-PERFORM                                                       
121000        MOVE DAGENS-DATUM-AAMMDD TO FPI-TIUPPDAT                          
121100        PERFORM IMS-REPL-WDD101                                           
121200     END-IF                                                               
121300     .                                                                    
121400     EJECT                                                                
121500 HC-KOPIERA SECTION.                                                      
121600                                                                          
121700     IF ARTNR-SOEK = JA                                                   
121800     OR ARTNR-LEVNR-SOEK = JA                                             
121900     OR LEVNR-SOEK = JA                                                   
122000        INSPECT MID-IDFPINST REPLACING LEADING SPACE BY ZERO              
122100        MOVE MID-IDFPINST TO W-IDFPINST                                   
122200     END-IF                                                               
122300                                                                          
122400     PERFORM IMS-GET-WDD101                                               
122500                                                                          
122600     MOVE WS-NYTT-IDFPINST TO NY-FPI-IDFPINST                             
122700                              W-IDFPINST                                  
122800     MOVE JA TO REGNR-SOEK                                                
122900                                                                          
123000     MOVE +1 TO RAD-IX                                                    
123100     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
123200        IF MID-TEFPINST(RAD-IX) = ALL '+' OR SPACE                        
123300           MOVE FPI-TEFPINST(RAD-IX) TO NY-FPI-TEFPINST(RAD-IX)           
123400        ELSE                                                              
123500           MOVE MID-TEFPINST(RAD-IX) TO NY-FPI-TEFPINST(RAD-IX)           
123600        END-IF                                                            
123700        ADD +1 TO RAD-IX                                                  
123800     END-PERFORM                                                          
123900                                                                          
124000     MOVE DAGENS-DATUM     TO NY-FPI-DAREGDAT                             
124100     MOVE SPACE            TO NY-FPI-IDUSER                               
124200     INSPECT MID-IDPERSON REPLACING LEADING SPACE BY ZERO                 
124300     MOVE MID-IDPERSON     TO NY-FPI-IDUSER                               
124400     MOVE ZERO             TO NY-FPI-TIUPPDAT                             
124500     IF KOP-ARTNR                                                         
124600        MOVE SPACE         TO NY-FPI-IDLEVNR                              
124700     ELSE                                                                 
124800        MOVE MID-IDLEVNR-KOP TO W-IDLEVNR-KOP                             
124900        MOVE W-IDLEVNR-KOP TO NY-FPI-IDLEVNR                              
125000     END-IF                                                               
125100     PERFORM IMS-ISRT-WDD101                                              
125200                                                                          
125300     IF KOP-ARTNR                                                         
125400        INSPECT MID-IDARTNR-KOP REPLACING LEADING SPACE BY ZERO           
125500        MOVE MID-IDARTNR-KOP TO W-IDARTNR-KOP                             
125600        MOVE W-IDARTNR-KOP   TO NY-FPA-IDARTNR                            
125700        PERFORM IMS-ISRT-WDD111                                           
125800     END-IF                                                               
125900                                                                          
126000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
126100     MOVE '001'             TO MSGI-KDCALL                                
126200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
126300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
126400     MOVE '6183'            TO MSGI-IDTRANS                               
126500     MOVE WS-NYTT-IDFPINST  TO MSGI-IDFPINST                              
126600     MOVE ZERO              TO MSGI-IDARTNR                               
126700     MOVE SPACE             TO MSGI-IDLEVNR                               
126800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
126900                                                                          
127000     MOVE MSGI-IDFPINST     TO MOD-IDFPINST-UT                            
127100     INSPECT MOD-IDFPINST-UT REPLACING LEADING ZERO BY SPACE              
127200     MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                             
127300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
127400     MOVE MSGI-IDLEVNR      TO MOD-IDLEVNR-UT                             
127500     .                                                                    
127600     EJECT                                                                
127700 HD-NYUPPLAGG SECTION.                                                    
127800                                                                          
127900     IF ARTNR-SOEK = JA                                                   
128000        MOVE SPACE           TO NY-FPI-IDLEVNR                            
128100     ELSE                                                                 
128200        IF LEVNR-SOEK = JA                                                
128300           MOVE MSGI-IDLEVNR TO NY-FPI-IDLEVNR                            
128400        END-IF                                                            
128500     END-IF                                                               
128600                                                                          
128700     MOVE WS-NYTT-IDFPINST    TO NY-FPI-IDFPINST                          
128800                                 W-IDFPINST                               
128900     MOVE JA TO REGNR-SOEK                                                
129000                                                                          
129100     MOVE +1 TO RAD-IX                                                    
129200     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
129300        IF MID-TEFPINST(RAD-IX) = ALL '+' OR SPACE                        
129400           MOVE SPACE                TO NY-FPI-TEFPINST(RAD-IX)           
129500        ELSE                                                              
129600           MOVE MID-TEFPINST(RAD-IX) TO NY-FPI-TEFPINST(RAD-IX)           
129700        END-IF                                                            
129800        ADD +1 TO RAD-IX                                                  
129900     END-PERFORM                                                          
130000                                                                          
130100     MOVE SPACE               TO NY-FPI-IDUSER                            
130200     INSPECT MID-IDPERSON REPLACING LEADING SPACE BY ZERO                 
130300     MOVE MID-IDPERSON        TO NY-FPI-IDUSER                            
130400     MOVE DAGENS-DATUM        TO NY-FPI-DAREGDAT                          
130500     MOVE ZERO                TO NY-FPI-TIUPPDAT                          
130600     PERFORM IMS-ISRT-WDD101                                              
130700                                                                          
130800     IF ARTNR-SOEK = JA                                                   
130900        MOVE W-IDARTNR        TO NY-FPA-IDARTNR                           
131000        PERFORM IMS-ISRT-WDD111                                           
131100     END-IF                                                               
131200                                                                          
131300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
131400     MOVE '001'             TO MSGI-KDCALL                                
131500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
131600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
131700     MOVE '6183'            TO MSGI-IDTRANS                               
131800     MOVE WS-NYTT-IDFPINST  TO MSGI-IDFPINST                              
131900     MOVE ZERO              TO MSGI-IDARTNR                               
132000     MOVE SPACE             TO MSGI-IDLEVNR                               
132100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
132200                                                                          
132300     MOVE MSGI-IDFPINST     TO MOD-IDFPINST-UT                            
132400     INSPECT MOD-IDFPINST-UT REPLACING LEADING ZERO BY SPACE              
132500     MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                             
132600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
132700     MOVE MSGI-IDLEVNR      TO MOD-IDLEVNR-UT                             
132800     .                                                                    
132900     EJECT                                                                
133000 HE-NYTT-IDFPINST SECTION.                                                
133100***  MAX-VÄRDE = 9999999 EV KONTROLL                                      
133200                                                                          
133300     PERFORM IMS-GET-6311                                                 
133400     PERFORM IMS-GET-6312                                                 
133500     ADD +1 TO 6312-IDFPINST                                              
133600     MOVE 6312-IDFPINST TO WS-NYTT-IDFPINST                               
133700     PERFORM IMS-REPL-6312                                                
133800     .                                                                    
133900     EJECT                                                                
134000 L-PRINTA SECTION.                                                        
134100                                                                          
134200     COMPUTE P-TO-P-KVLL = LENGTH OF 618A-MID-W6I18A01 + 17               
134300     MOVE 'W6T18AX '         TO P-TO-P-KDTRANS                            
134400     MOVE '6183'             TO P-TO-P-IDTRANS                            
134500     MOVE MFS-KDMFSFOR       TO P-TO-P-KDMFSFOR                           
134600                                                                          
134700     MOVE SPACE              TO 618A-MID-W6I18A01                         
134800     MOVE MID-IDFPINST       TO 618A-MID-IDFPINST                         
134900     MOVE MID-IDLTERM        TO 618A-MID-IDPRTLST                         
135000     MOVE 618A-MID-W6I18A01  TO P-TO-P-DATA                               
135100                                                                          
135200     PERFORM IMS-ISRT-ALT-MSG-618A                                        
135300                                                                          
135400     MOVE INF-PRINTING-STARTED TO MED-IDMFSINF                            
135500     CALL WMEDKONV USING MED-WMEDAREA                                     
135600     MOVE MED-TEMFSINF       TO MOD-TEMFSFEL                              
135700     .                                                                    
135800     EJECT                                                                
135900 S01-VISA-BILD SECTION.                                                   
136000                                                                          
136100     MOVE FPI-DAREGDAT(3:6) TO MOD-TIREGDAT                               
136200     MOVE FPI-TIUPPDAT      TO MOD-TIUPPDAT                               
136300     IF FPI-TIUPPDAT = ZERO                                               
136400        INSPECT MOD-TIUPPDAT REPLACING LEADING ZERO BY SPACE              
136500     END-IF                                                               
136600     MOVE FPI-IDFPINST      TO SPAR-IDFPINST-ENTER                        
136700                               SPAR-IDFPINST-NEXT                         
136800                               MOD-IDFPINST                               
136900     IF FPI-IDLEVNR = SPACE                                               
137000        PERFORM IMS-GET-WDD111                                            
137100        IF SEGMENT-FINNS                                                  
137200           MOVE FPA-IDARTNR     TO ARTNR-TEXT                             
137300           MOVE MOD-ARTNR-TEXT  TO MOD-TEXTRAD                            
137400        ELSE                                                              
137500           MOVE MFS-RENSA-FAELT TO MOD-TEXTRAD                            
137600        END-IF                                                            
137700     ELSE                                                                 
137800        MOVE FPI-IDLEVNR        TO LEVNR-TEXT                             
137900        MOVE MOD-LEVNR-TEXT     TO MOD-TEXTRAD                            
138000     END-IF                                                               
138100                                                                          
138200     IF MFS-UPDATE AND INDATA-FEL                                         
138300     OR SW-EAMID = JA                                                     
138400        IF MID-IDPERSON = ALL '+'                                         
138500           MOVE FPI-IDUSER TO WS-IDUSER                                   
138600           MOVE WS-IDUSER TO MOD-IDPERSON                                 
138700        ELSE                                                              
138800           MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON                         
138900        END-IF                                                            
139000     ELSE                                                                 
139100        MOVE FPI-IDUSER TO WS-IDUSER                                      
139200        MOVE WS-IDUSER TO MOD-IDPERSON                                    
139300     END-IF                                                               
139400                                                                          
139500     MOVE +1 TO RAD-IX                                                    
139600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
139700        IF MFS-UPDATE AND INDATA-FEL                                      
139800        OR SW-EAMID = JA                                                  
139900          IF WS-NYUPPLAGG = JA                                            
140000            IF KOP-ARTNR OR KOP-levnr                                     
140100              IF MID-TEFPINST(RAD-IX) = ALL '+'                           
140200                 MOVE FPI-TEFPINST(RAD-IX) TO MOD-TEFPINST(RAD-IX)        
140300              ELSE                                                        
140400                 MOVE MFS-ROER-EJ-FAELT TO MOD-TEFPINST(RAD-IX)           
140500              END-IF                                                      
140600            ELSE                                                          
140700              IF MID-TEFPINST(RAD-IX) = ALL '+'                           
140800                 CONTINUE                                                 
140900              ELSE                                                        
141000                 MOVE MFS-ROER-EJ-FAELT TO MOD-TEFPINST(RAD-IX)           
141100              END-IF                                                      
141200            END-IF                                                        
141300          ELSE                                                            
141400            IF MID-TEFPINST(RAD-IX) = ALL '+'                             
141500               MOVE FPI-TEFPINST(RAD-IX) TO MOD-TEFPINST(RAD-IX)          
141600            ELSE                                                          
141700               MOVE MFS-ROER-EJ-FAELT TO MOD-TEFPINST(RAD-IX)             
141800            END-IF                                                        
141900          END-IF                                                          
142000        ELSE                                                              
142100           MOVE FPI-TEFPINST(RAD-IX) TO MOD-TEFPINST(RAD-IX)              
142200        END-IF                                                            
142300        ADD +1 TO RAD-IX                                                  
142400     END-PERFORM                                                          
142500     .                                                                    
142600     EJECT                                                                
142700 S02-KOLLA-IN-UT SECTION.                                                 
142800                                                                          
142900     IF MID-IDPERSON = ALL '+'                                            
143000        MOVE MFS-RENSA-FAELT TO MOD-IDPERSON                              
143100     END-IF                                                               
143200     MOVE +1 TO RAD-IX                                                    
143300     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
143400        IF MID-TEFPINST(RAD-IX) = ALL '+'                                 
143500           MOVE MFS-RENSA-FAELT TO MOD-TEFPINST(RAD-IX)                   
143600        END-IF                                                            
143700        ADD +1 TO RAD-IX                                                  
143800     END-PERFORM                                                          
143900     .                                                                    
144000     EJECT                                                                
144100 MFS-RENSA-FAELT-UT SECTION.                                              
144200                                                                          
144300*    --- ALLA UTDATA-FÄLT                                                 
144400     MOVE MFS-RENSA-FAELT TO MOD-IDFPINST                                 
144500                             MOD-BEFT                                     
144600                             MOD-TIREGDAT                                 
144700                             MOD-TIUPPDAT                                 
144800                             MOD-TEXTRAD                                  
144900     .                                                                    
145000     SKIP3                                                                
145100 MFS-RENSA-FAELT-IN SECTION.                                              
145200                                                                          
145300*    --- ALLA INDATA-FÄLT                                                 
145400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-KOP                              
145500                             MOD-IDLEVNR-KOP                              
145600                             MOD-FLNY                                     
145700                             MOD-FLBORT                                   
145800                             MOD-IDLTERM                                  
145900     .                                                                    
146000     SKIP3                                                                
146100 MFS-RENSA-FAELT-IN-UT SECTION.                                           
146200                                                                          
146300     MOVE MFS-RENSA-FAELT    TO MOD-IDPERSON                              
146400     MOVE +1 TO RAD-IX                                                    
146500     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
146600        MOVE MFS-RENSA-FAELT TO MOD-TEFPINST(RAD-IX)                      
146700        ADD +1 TO RAD-IX                                                  
146800     END-PERFORM                                                          
146900     .                                                                    
147000     EJECT                                                                
147100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
147200                                                                          
147300*    --- ALLA INDATA-FÄLT                                                 
147400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-KOP                            
147500                               MOD-IDLEVNR-KOP                            
147600                               MOD-FLNY                                   
147700                               MOD-FLBORT                                 
147800                               MOD-IDLTERM                                
147900     .                                                                    
148000     SKIP3                                                                
148100 MFS-ROER-EJ-FAELT-IN-UT SECTION.                                         
148200                                                                          
148300     IF MID-IDPERSON NOT = ALL '+'                                        
148400        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON                            
148500     END-IF                                                               
148600     MOVE +1 TO RAD-IX                                                    
148700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
148800        IF MID-TEFPINST(RAD-IX) NOT = ALL '+'                             
148900           MOVE MFS-ROER-EJ-FAELT TO MOD-TEFPINST(RAD-IX)                 
149000        END-IF                                                            
149100        ADD +1 TO RAD-IX                                                  
149200     END-PERFORM                                                          
149300     .                                                                    
149400     EJECT                                                                
149500 MFS-FORM-ATTR SECTION.                                                   
149600                                                                          
149700*    --- ALLA INDATA-FÄLT                                                 
149800     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-ATTR                          
149900                                MOD-IDLEVNR-ATTR                          
150000                                MOD-FLNY-ATTR                             
150100                                MOD-FLBORT-ATTR                           
150200                                MOD-IDLTERM-ATTR                          
150300                                MOD-IDFPINST-ATTR                         
150400                                MOD-IDPERSON-ATTR                         
150500     MOVE +1 TO RAD-IX                                                    
150600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
150700        MOVE MFS-FORMATETS-ATTR TO MOD-TEFPINST-ATTR(RAD-IX)              
150800        ADD +1 TO RAD-IX                                                  
150900     END-PERFORM                                                          
151000     .                                                                    
151100     EJECT                                                                
151200* --- IMS SEKTIONER ---                                                   
151300                                                                          
151400 IMS-GET-MSG SECTION.                                                     
151500     MOVE '  QC' TO GODK-STATUSKODER                                      
151600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
151700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000     SKIP3                                                                
152100 IMS-ISRT-ALT-MSG-618A SECTION.                                           
152200                                                                          
152300     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
152400     MOVE    '  '             TO    GODK-STATUSKODER                      
152500     CALL    CBLTDLI          USING ISRT 618A-PCB P-TO-P-SW               
152600     MOVE    618A-STATUS-CODE TO    STATUS-WS                             
152700     PERFORM IMS-STATUSKONTROLL                                           
152800     .                                                                    
152900     SKIP3                                                                
153000 IMS-INSERT-MSG SECTION.                                                  
153100     IF MSGI-IDLAND-SPR = 'SE'                                            
153200       MOVE '0' TO MFS-KDHUVOMR                                           
153300     END-IF                                                               
153400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
153500     MOVE SPACE TO GODK-STATUSKODER                                       
153600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
153700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
153800     PERFORM IMS-STATUSKONTROLL                                           
153900     .                                                                    
154000     EJECT                                                                
154100 IMS-GET-WDD101 SECTION.                                                  
154200     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
154300          DELIMITED BY SIZE INTO SSA1                                     
154400     MOVE '  GE' TO GODK-STATUSKODER                                      
154500     CALL CBLTDLI USING GHU WDD1-PCB DLI-IO-WDD101 SSA1                   
154600     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
154700     PERFORM IMS-STATUSKONTROLL                                           
154800     .                                                                    
154900     SKIP3                                                                
155000 IMS-GET-WDD111 SECTION.                                                  
155100     MOVE 'WDD111  ' TO SSA1                                              
155200     MOVE '  GE' TO GODK-STATUSKODER                                      
155300     CALL CBLTDLI USING GNP WDD1-PCB DLI-IO-WDD111 SSA1                   
155400     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     .                                                                    
155700     SKIP3                                                                
155800 IMS-ISRT-WDD101 SECTION.                                                 
155900     MOVE 'WDD101   ' TO SSA1                                             
156000     MOVE '  ' TO GODK-STATUSKODER                                        
156100     CALL CBLTDLI USING ISRT WDD1-PCB DLI-IO-NY-WDD101 SSA1               
156200     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
156300     PERFORM IMS-STATUSKONTROLL                                           
156400     .                                                                    
156500     eject                                                                
156600 IMS-ISRT-WDD111 SECTION.                                                 
156700     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE 'WDD111   ' TO SSA2                                             
157000     MOVE '  ' TO GODK-STATUSKODER                                        
157100     CALL CBLTDLI USING ISRT WDD1-PCB DLI-IO-NY-WDD111 SSA1 SSA2          
157200     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
157300     PERFORM IMS-STATUSKONTROLL                                           
157400     .                                                                    
157500     SKIP3                                                                
157600 IMS-REPL-WDD101 SECTION.                                                 
157700     MOVE '  ' TO GODK-STATUSKODER                                        
157800     CALL CBLTDLI USING REPL WDD1-PCB DLI-IO-WDD101                       
157900     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
158000     PERFORM IMS-STATUSKONTROLL                                           
158100     .                                                                    
158200     SKIP3                                                                
158300 IMS-DLET-WDD101 SECTION.                                                 
158400     MOVE '  ' TO GODK-STATUSKODER                                        
158500     CALL CBLTDLI USING DLET WDD1-PCB DLI-IO-WDD101                       
158600     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
158700     PERFORM IMS-STATUSKONTROLL                                           
158800     .                                                                    
158900     EJECT                                                                
159000 IMS-GET-WDD1A-FIRST SECTION.                                             
159100     STRING 'WDD1A1  (WDD1A1KY=>' W-WDD1A1KY-MIN                          
159200                    '&WDD1A1KY=<' W-WDD1A1KY-MAX ')'                      
159300          DELIMITED BY SIZE INTO SSA1                                     
159400     MOVE '  GE' TO GODK-STATUSKODER                                      
159500     CALL CBLTDLI USING GU WDD1A-PCB DLI-IO-WDD1A SSA1                    
159600     MOVE WDD1A-STATUS-CODE TO STATUS-WS                                  
159700     PERFORM IMS-STATUSKONTROLL                                           
159800     .                                                                    
159900     SKIP3                                                                
160000 IMS-GET-WDD1A-NEXT SECTION.                                              
160100     STRING 'WDD1A1  (WDD1A1KY=>' W-WDD1A1KY-MIN                          
160200                    '&WDD1A1KY=<' W-WDD1A1KY-MAX ')'                      
160300          DELIMITED BY SIZE INTO SSA1                                     
160400     MOVE '  GE' TO GODK-STATUSKODER                                      
160500     CALL CBLTDLI USING GN WDD1A-PCB DLI-IO-WDD1A SSA1                    
160600     MOVE WDD1A-STATUS-CODE TO STATUS-WS                                  
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900     EJECT                                                                
161000 IMS-GU-WDD101-ASEQ SECTION.                                              
161100     STRING 'WDD101  (WDD1ASEQ =' W-IDARTNR-KOP-X ')'                     
161200             DELIMITED BY SIZE INTO SSA1                                  
161300     MOVE '  GE' TO GODK-STATUSKODER                                      
161400     CALL CBLTDLI USING GU WDD1ASEQ-PCB DLI-IO-KOP-WDD101 SSA1            
161500     MOVE WDD1ASEQ-STATUS-CODE TO STATUS-WS                               
161600     PERFORM IMS-STATUSKONTROLL                                           
161700     .                                                                    
161800     SKIP3                                                                
161900 IMS-GN-WDD101-ASEQ SECTION.                                              
162000     STRING 'WDD101  (WDD1ASEQ =' W-IDARTNR-KOP-X ')'                     
162100             DELIMITED BY SIZE INTO SSA1                                  
162200     MOVE '  GE' TO GODK-STATUSKODER                                      
162300     CALL CBLTDLI USING GN WDD1ASEQ-PCB DLI-IO-KOP-WDD101 SSA1            
162400     MOVE WDD1ASEQ-STATUS-CODE TO STATUS-WS                               
162500     PERFORM IMS-STATUSKONTROLL                                           
162600     .                                                                    
162700     SKIP3                                                                
162800 IMS-GU-WDT301 SECTION.                                                   
162900     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-KOP-X ')'                     
163000          DELIMITED BY SIZE INTO SSA1                                     
163100     MOVE '  GE' TO GODK-STATUSKODER                                      
163200     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT301 SSA1                    
163300     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
163400     PERFORM IMS-STATUSKONTROLL                                           
163500     .                                                                    
163600     SKIP3                                                                
163700 IMS-GU-WDT311 SECTION.                                                   
163800     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
163900          DELIMITED BY SIZE INTO SSA1                                     
164000     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
164010          DELIMITED BY SIZE INTO SSA2                                     
164200     MOVE '  GE' TO GODK-STATUSKODER                                      
164300     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT311 SSA1 SSA2               
164400     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700     EJECT                                                                
164800 IMS-GET-6311 SECTION.                                                    
164900     STRING 'WDR201  (WDGXKEY  =' W-6311-KEY-X ')'                        
165000            DELIMITED BY SIZE INTO SSA1                                   
165100     MOVE '  ' TO GODK-STATUSKODER                                        
165200     CALL CBLTDLI USING GU 6312-PCB DLI-IO-6312 SSA1                      
165300     MOVE 6312-STATUS-CODE TO STATUS-WS                                   
165400     PERFORM IMS-STATUSKONTROLL                                           
165500     .                                                                    
165600     SKIP2                                                                
165700 IMS-GET-6312 SECTION.                                                    
165800     MOVE 'WDGX6312 ' TO SSA1                                             
165900     MOVE '  ' TO GODK-STATUSKODER                                        
166000     CALL CBLTDLI USING GHNP 6312-PCB DLI-IO-6312 SSA1                    
166100     MOVE 6312-STATUS-CODE TO STATUS-WS                                   
166200     PERFORM IMS-STATUSKONTROLL                                           
166300     .                                                                    
166400     SKIP2                                                                
166500 IMS-REPL-6312 SECTION.                                                   
166600     MOVE '  ' TO GODK-STATUSKODER                                        
166700     CALL CBLTDLI USING REPL 6312-PCB DLI-IO-6312                         
166800     MOVE 6312-STATUS-CODE TO STATUS-WS                                   
166900     PERFORM IMS-STATUSKONTROLL                                           
167000     .                                                                    
167100     EJECT                                                                
167200 IMS-GET-WDP311 SECTION.                                                  
167300     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
167400            DELIMITED BY SIZE INTO SSA1                                   
167500     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
167600            DELIMITED BY SIZE INTO SSA2                                   
167700     MOVE '  GE' TO GODK-STATUSKODER                                      
167800     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
167900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
168000     PERFORM IMS-STATUSKONTROLL                                           
168100     .                                                                    
168200     SKIP3                                                                
168300 IMS-GET-WDD1B-FIRST SECTION.                                             
168400     STRING 'WDD1B1  (WDD1B1KY=>' W-WDD1B1KY-MIN                          
168500                    '&WDD1B1KY=<' W-WDD1B1KY-MAX ')'                      
168600          DELIMITED BY SIZE INTO SSA1                                     
168700     MOVE '  GE' TO GODK-STATUSKODER                                      
168800     CALL CBLTDLI USING GU WDD1B-PCB DLI-IO-WDD1B SSA1                    
168900     MOVE WDD1B-STATUS-CODE TO STATUS-WS                                  
169000     PERFORM IMS-STATUSKONTROLL                                           
169100     .                                                                    
169200     EJECT                                                                
169300 IMS-GET-WDD1B-NEXT SECTION.                                              
169400     STRING 'WDD1B1  (WDD1B1KY=>' W-WDD1B1KY-MIN                          
169500                    '&WDD1B1KY=<' W-WDD1B1KY-MAX ')'                      
169600          DELIMITED BY SIZE INTO SSA1                                     
169700     MOVE '  GE' TO GODK-STATUSKODER                                      
169800     CALL CBLTDLI USING GN WDD1B-PCB DLI-IO-WDD1B SSA1                    
169900     MOVE WDD1B-STATUS-CODE TO STATUS-WS                                  
170000     PERFORM IMS-STATUSKONTROLL                                           
170100     .                                                                    
170200     SKIP3                                                                
170300 IMS-STATUSKONTROLL SECTION.                                              
170400                                                                          
170500     SET STATUS-IX TO 1                                                   
170600     SEARCH GODK-STATUS                                                   
170700       AT END                                                             
170800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
170900         DELIMITED BY SIZE INTO FELTEXT                                   
171000         CALL FELLOG                                                      
171100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
171200         CONTINUE                                                         
171300     END-SEARCH                                                           
171400     .                                                                    
