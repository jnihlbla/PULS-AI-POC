000100     SKIP3                                                                
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W1014200.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   MAJ   88.                                                
000700                                                                          
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        ARTIKELNR PER PROJEKT/ÄO                                         
001200*        INMATNINGSFÄLT IDBERED IDAO SAMT IDPROJ                          
001300*        PROGRAMMET LÄSER OCH UPPDATERAR RLARGT01-BASEN                   
001400*        SAMT I VISSA FALL WLZZAC (KDP-KOLA)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W1T142                                              
001800*        MID:         W1I14201                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W1O14201                                            
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002701*    -COPY WY2000W3                                                       
002702     SKIP3                                                                
002703*    -COPY WY2000W1                                                       
002704     SKIP3                                                                
002800 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1014200'.               
002900 77  WS-IDBERED                  PIC X(2).                                
003000 77  WS-IDAO                     PIC X(10) VALUE SPACE.                   
003100 77  WS-IDPROJ                   PIC X(4) VALUE SPACE.                    
003200 77  W-SPAR-TISERLEV             PIC 9(7) VALUE 9999999.                  
003300 77  W-IDLOGLOP                  PIC S9(1) VALUE ZERO.                    
003400 77  W-NY-IDBERED-IN             PIC X(2).                                
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003800 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003900 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004000 77  MAX-RAD                     PIC S9(9)   VALUE +13  COMP SYNC.        
004100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1339 COMP SYNC.        
004200 01  SW-TRAEFF                   PIC X.                                   
004300    88  TRAEFF                               VALUE 'J'.                   
004400 01  SW-NYCKLAR-OK               PIC X.                                   
004500    88  NYCKLAR-OK                           VALUE 'J'.                   
004600 01  SW-INDATA-OK                PIC X.                                   
004700    88  INDATA-OK                            VALUE 'J'.                   
004800 01  SW-INDATA-IFYLLT-EJ-UPPDATE PIC X.                                   
004900    88  INDATA-IFYLLT-EJ-UPPDATE             VALUE 'J'.                   
005000 01  WS-IDTRANS                  PIC X(4).                                
005100    88  GODKAEND-BILD                        VALUE '1142'.                
005200    88  EGEN-BILD                            VALUE '1142'.                
005300 01  NYCKLAR-TILL-DLI.                                                    
005500   03  FILLER                    PIC X(16)   VALUE                        
005600                                            'NYCKLAR-TILL-DLI'.           
006200   03  W-WDD2A1KY-MIN.                                                    
006300     05  W-IDBERED-MIN           PIC S9(3)   COMP-3.                      
006400     05  W-IDAO-MIN              PIC  X(10).                              
006500     05  W-IDARTNR-MIN           PIC S9(9)   COMP-3.                      
006600*                                                                         
006700   03  W-WDD2A1KY-MAX.                                                    
006800     05  W-IDBERED-MAX           PIC S9(3)   COMP-3.                      
006900     05  W-IDAO-MAX              PIC  X(10).                              
007000     05  W-IDARTNR-MAX           PIC S9(9)   COMP-3                       
007100                               VALUE +999999999.                          
007200   03  W-IDPROJ-MIN              PIC  X(4).                               
007300   03  W-IDPROJ-MAX              PIC  X(4).                               
007400*                                                                         
007500  03  W-WDD201KY-X.                                                       
007600     05  W-IDARTNR               PIC S9(9)   COMP-3.                      
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007830   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007840   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007900   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008000*----------------------------------PARAMETRAR TILL DATUMKORT              
008100     SKIP2                                                                
008200*01    -COPY WDATAREA                                                     
008400 01  W-AAVV.                                                              
008500   03  W-AA                      PIC 9(2).                                
008600   03  W-VV                      PIC 9(2).                                
008610 01  W-AAVV-N REDEFINES W-AAVV   PIC 9(4).                                
008700*                                                                         
008800 77  WS-NEDB-X4                  PIC X(4).                                
008900 01  W-DAGENS-AAVV.                                                       
009000   03  W-DAGENS-AA               PIC 9(2).                                
009100   03  W-DAGENS-VV               PIC 9(2).                                
009110 01  W-DAGENS-AAVV-N REDEFINES W-DAGENS-AAVV PIC 9(4).                    
009200     EJECT                                                                
009300*01    -COPY W10111                                                       
009500     EJECT                                                                
009600*01  AREA -COPY W092W001       -PRE W092-                                 
009800     EJECT                                                                
009900 01  MEDDELANDE.                                                          
010000   03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
010100   03  FEL1.                                                              
010200     05 FILLER                   PIC X(40)                                
010300          VALUE 'UPPLYSTA FÄLT FEL'.                                      
010400     05 FILLER                   PIC X(40)                                
010500          VALUE 'CORRECT HIGHLIGHTED FIELDS  '.                           
010600   03  FILLER REDEFINES FEL1.                                             
010700     05  FEL-1                   PIC X(40)   OCCURS 2.                    
010800                                                                          
010900   03  FEL2.                                                              
011000     05 FILLER                   PIC X(40)                                
011100          VALUE 'NYCKLAR FEL'.                                            
011200     05 FILLER                   PIC X(40)                                
011300          VALUE 'KEYS ARE WRONG'.                                         
011400   03  FILLER REDEFINES FEL2.                                             
011500     05  FEL-2                   PIC X(40)   OCCURS 2.                    
011600                                                                          
011700   03  FEL3.                                                              
011800     05 FILLER                   PIC X(40)                                
011900          VALUE 'NYCKLAR SAKNAS'.                                         
012000     05 FILLER                   PIC X(40)                                
012100          VALUE 'KEYS NOT FOUND  '.                                       
012200   03  FILLER REDEFINES FEL3.                                             
012300     05  FEL-3                   PIC X(40)   OCCURS 2.                    
012400                                                                          
012500   03  FEL4.                                                              
012600     05 FILLER                   PIC X(40)                                
012700          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
012800     05 FILLER                   PIC X(40)                                
012900          VALUE 'PRESS PF11 FOR UPDATING'.                                
013000   03  FILLER REDEFINES FEL4.                                             
013100     05  FEL-4                   PIC X(40)   OCCURS 2.                    
013200                                                                          
013300   03  MED1.                                                              
013400     05 FILLER                   PIC X(40)                                
013500          VALUE 'UPPDATERING GJORD        '.                              
013600     05 FILLER                   PIC X(40)                                
013700          VALUE 'UPDATED                       '.                         
013800   03  FILLER REDEFINES MED1.                                             
013900     05  MED-1                   PIC X(40)   OCCURS 2.                    
014000                                                                          
014100   03  MED2.                                                              
014200     05 FILLER                   PIC X(40)                                
014300          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
014400     05 FILLER                   PIC X(40)                                
014500          VALUE 'PRESS PF8 FOR MORE LINES'.                               
014600   03  FILLER REDEFINES MED2.                                             
014700     05  MED-2                   PIC X(40)   OCCURS 2.                    
014800                                                                          
014900   03  MED3.                                                              
015000     05 FILLER                   PIC X(40)                                
015100          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
015200     05 FILLER                   PIC X(40)                                
015300          VALUE 'THIS IS THE FIRST PAGE'.                                 
015400   03  FILLER REDEFINES MED3.                                             
015500     05  MED-3                   PIC X(40)   OCCURS 2.                    
016400     EJECT                                                                
016500******************************************************************        
016600*                                                                         
016700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
016800*                                                                         
016900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
017000     SKIP3                                                                
017100*01  MID -COPY W1I14201                                                   
017300     SKIP3                                                                
017400*01  MID -COPY W1I11601 -PRE 1116-.                                       
017600     EJECT                                                                
017700*01  -COPY WMSGAREA                                                       
017900     EJECT                                                                
018000*  03  MOD -COPY W1O14201  -RED MSG-AREA.                                 
018200     EJECT                                                                
018300*01  -COPY WMFSAREA                                                       
018500     EJECT                                                                
018600******************************************************************        
018700*                                                                         
018800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018900*                                                                         
019000 01  IMS-WS.                                                              
019100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
019200     SKIP3                                                                
019300*                        **** STATUS-KOD FRÅN IMS                         
019400   03  STATUS-WS                 PIC XX.                                  
019500     88  SEGMENT-FINNS                       VALUE '  '.                  
019600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019700     SKIP3                                                                
019800   03  GODK-STATUSKODER.                                                  
019900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020000     SKIP3                                                                
020100 01    SSA1                      PIC X(96).                               
020200 01    SSA2                      PIC X(96).                               
020300     EJECT                                                                
020400*                            IMS FUNKTIONSKODER                           
020500*01    -COPY W0003                                                        
020700     EJECT                                                                
020800*                            DLI INPUT-OUTPUT AREA                        
020900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
021000 01  DLI-IO-AREA.                                                         
021100   03  IO-AREA                   PIC X(550)   VALUE SPACE.                
021200     SKIP3                                                                
021300*03  WLARTG01    -COPY WDD201  -PRE ARTG01-  -RED IO-AREA.                
021500     SKIP3                                                                
021600*03  WLARTH01    -COPY WDD2A1  -PRE ARTH01-  -RED IO-AREA.                
021800     EJECT                                                                
021900     SKIP3                                                                
022300*03  WLZZAC01    -COPY WDGZ01    -PRE   ZZAC-  -RED IO-AREA.              
022500     EJECT                                                                
022600 LINKAGE SECTION.                                                         
022700*01  -COPY W0009     -PRE MSG-                                            
022900     SKIP2                                                                
023000*01  -COPY W0008     -PRE ARTH-                                           
023200     05  FILLER                  PIC X.                                   
023300     SKIP2                                                                
023800*01  -COPY W0008     -PRE ARTG-                                           
024000     05  FILLER                  PIC X.                                   
024100     SKIP2                                                                
024200*01  -COPY W0008     -PRE ZZAC-                                           
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600 PROCEDURE DIVISION   USING  MSG-PCB ARTH-PCB ARTG-PCB                    
024700                                              ZZAC-PCB.                   
024800      ENTRY 'DLITCBL' USING  MSG-PCB ARTH-PCB ARTG-PCB                    
024900                                              ZZAC-PCB.                   
025000                                                                          
025100     PERFORM IMS-GET-MSG                                                  
025200     IF SEGMENT-FINNS                                                     
025300        PERFORM A-INIT                                                    
025400        PERFORM B-KOLLA-NYCKLAR                                           
025500        IF NYCKLAR-OK                                                     
025600           IF MFS-UPDATE                                                  
025700              PERFORM C-KOLLA-INDATA                                      
025800              IF INDATA-OK                                                
025900                 PERFORM D-UPPDATERA                                      
026000                 MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                    
026100                 PERFORM I-FORMATETS-ATTR                                 
026200                 PERFORM IMS-GN-ARTH01                                    
026300                 IF SEGMENT-FINNS                                         
026400                    PERFORM G-VISA-SIDAN                                  
026500                 ELSE                                                     
026600                    MOVE FEL-3 (SPRAK-IX)         TO MOD-TEMFSFEL         
026700                    PERFORM F-RENSA-SIDAN                                 
026800                 END-IF                                                   
026900              ELSE                                                        
027000                 MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                    
027100                 PERFORM E-MFS-ROER-EJ-FAELT                              
027200              END-IF                                                      
027300           ELSE                                                           
027400              PERFORM IMS-GN-ARTH01                                       
027500              IF SEGMENT-FINNS                                            
027600                 MOVE NEJ                 TO                              
027700                                  SW-INDATA-IFYLLT-EJ-UPPDATE             
027800                 IF EGEN-BILD                                             
027900                    IF MFS-IDPFK = ' '                                    
028000                       PERFORM H-KOLLA-ATT-INDATA-EJ-IFYLLD               
028100                    END-IF                                                
028200                 END-IF                                                   
028300                 IF INDATA-IFYLLT-EJ-UPPDATE                              
028400                    MOVE FEL-4 (SPRAK-IX)     TO MOD-TEMFSFEL             
028500                    PERFORM E-MFS-ROER-EJ-FAELT                           
028600                 ELSE                                                     
028700                    PERFORM G-VISA-SIDAN                                  
028800                 END-IF                                                   
028900              ELSE                                                        
029000                 MOVE FEL-3 (SPRAK-IX)         TO MOD-TEMFSFEL            
029100                 PERFORM F-RENSA-SIDAN                                    
029200              END-IF                                                      
029300           END-IF                                                         
029400        ELSE                                                              
029500           PERFORM F-RENSA-SIDAN                                          
029600           PERFORM J-RENSA-NYCKLAR                                        
029700        END-IF                                                            
029800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
029900        PERFORM IMS-INSERT-MSG                                            
030000     END-IF                                                               
030100     MOVE ZERO                            TO RETURN-CODE                  
030200     GOBACK                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 A-INIT SECTION.                                                          
030600                                                                          
030700     IF MSG-DUBBLA-TRANSKODER                                             
030800        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I14201                
030900        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
031000        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
031100        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
031200        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
031300     ELSE                                                                 
031400        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I14201                
031500        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
031600        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
031700        MOVE SPACE                         TO MFS-KDTRTYP                 
031800                                              MFS-IDPFK                   
031900     END-IF                                                               
032000     IF MFS-IDTRANS = '1116'                                              
032100        MOVE MID-W1I14201                  TO 1116-MID-W1I11601           
032200     END-IF                                                               
032300                                                                          
032400     MOVE LOW-VALUE                        TO MSG-AREA                    
032500     MOVE 'W1O142N1'                       TO MFS-IDMOD                   
032600     MOVE '1142'                           TO MOD-IDTRANS                 
032700     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
032800     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
032900                                              MOD-TEMFSINF                
033000                                              MOD-IDBERED-IN              
033100                                              MOD-IDAO-IN                 
033200                                              MOD-IDPROJ-IN               
033300     IF EGEN-BILD                                                         
033400        CONTINUE                                                          
033500     ELSE                                                                 
033600        MOVE SPACE                         TO MFS-KDTRTYP                 
033700        MOVE '7'                           TO MFS-IDPFK                   
033800     END-IF                                                               
033900     IF ENGLISH-TEXT                                                      
034000        MOVE +2                            TO SPRAK-IX                    
034100     ELSE                                                                 
034200        MOVE +1                            TO SPRAK-IX                    
034300     END-IF                                                               
034400     MOVE 'IDAG  '                     TO DAT-KDDATFORM                   
034500     PERFORM S99-CALL-WDATKONV                                            
034600     IF DAT-KDSVAR-OK                                                     
034700        MOVE DAT-TIAA-VECKA                    TO W-DAGENS-AA             
034800        MOVE DAT-TIVV                          TO W-DAGENS-VV             
034900     END-IF                                                               
035000     IF MFS-IDTRANS = '1116'                                              
035100         PERFORM AA-FIXA-1116                                             
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 AA-FIXA-1116   SECTION.                                                  
035600     SKIP2                                                                
035700     MOVE 1116-MID-IDBERED-LAEST        TO MID-IDBERED-UT                 
035800                                           MOD-IDBERED-UT                 
035900     MOVE 1116-MID-IDAO-VALD            TO MID-IDAO-UT                    
036000                                           MOD-IDAO-UT                    
036100     MOVE 1116-MID-IDAO-LAEST           TO MID-IDAO-LO                    
036200     MOVE 1116-MID-IDPROJ-VALD          TO MID-IDPROJ-UT                  
036300                                           MOD-IDPROJ-UT                  
036400     INSPECT 1116-MID-IDARTNR-UT REPLACING LEADING SPACE BY               
036500                                                   ZERO                   
036600     IF 1116-MID-IDARTNR-UT NUMERIC                                       
036700        MOVE 1116-MID-IDARTNR-UT        TO MID-IDARTNR-LO                 
036800     ELSE                                                                 
036900        MOVE ZERO                       TO MID-IDARTNR-LO                 
037000     END-IF                                                               
036600     MOVE SPACE                         TO 1116-MID-IDCDS                 
036600     MOVE SPACE                         TO 1116-MID-KDARTSYS              
037100     MOVE '++'                          TO MID-IDBERED-IN                 
037200     MOVE '++++++++++'                  TO MID-IDAO-IN                    
037300     MOVE '++++'                        TO MID-IDPROJ-IN                  
037400     MOVE ' '                           TO MFS-IDPFK                      
037500     .                                                                    
037600     EJECT                                                                
037700 B-KOLLA-NYCKLAR SECTION.                                                 
037800     SKIP3                                                                
037900     MOVE JA                         TO SW-NYCKLAR-OK                     
038000     SKIP2                                                                
038100     IF MID-IDBERED-IN = ALL '+'                                          
038200        MOVE MID-IDBERED-UT         TO WS-IDBERED                         
038300        INSPECT WS-IDBERED    REPLACING LEADING SPACE                     
038400                                           BY ZERO                        
038500     ELSE                                                                 
038600        MOVE MID-IDBERED-IN          TO WS-IDBERED                        
038700        MOVE '7'                     TO MFS-IDPFK                         
038800        MOVE SPACE                   TO MFS-KDTRTYP                       
038900     END-IF                                                               
039000     IF WS-IDBERED    NUMERIC                                             
039100        MOVE WS-IDBERED              TO  W-IDBERED-MIN                    
039200                                         W-IDBERED-MAX                    
039300     ELSE                                                                 
039400        MOVE NEJ                     TO SW-NYCKLAR-OK                     
039500     END-IF                                                               
039600     IF WS-IDBERED = ZERO                                                 
039700        MOVE ' 0'                    TO MOD-IDBERED-UT                    
039800     ELSE                                                                 
039900        MOVE WS-IDBERED              TO MOD-IDBERED-UT                    
040000        INSPECT MOD-IDBERED-UT REPLACING LEADING ZERO BY SPACE            
040100     END-IF                                                               
040200     SKIP2                                                                
040300     IF MID-IDAO-IN = ALL '+'                                             
040400        MOVE MID-IDAO-UT             TO WS-IDAO                           
040500     ELSE                                                                 
040600        MOVE MID-IDAO-IN             TO WS-IDAO                           
040700        MOVE '7'                     TO MFS-IDPFK                         
040800        MOVE SPACE                   TO MFS-KDTRTYP                       
040900     END-IF                                                               
041000     MOVE WS-IDAO                    TO MOD-IDAO-UT                       
041100     IF WS-IDAO = SPACE                                                   
041200        MOVE LOW-VALUE               TO W-IDAO-MIN                        
041300        MOVE HIGH-VALUE              TO W-IDAO-MAX                        
041400     ELSE                                                                 
041500        MOVE WS-IDAO                 TO W-IDAO-MIN                        
041600                                        W-IDAO-MAX                        
041700     END-IF                                                               
041800     SKIP2                                                                
041900     IF MID-IDPROJ-IN = ALL '+'                                           
042000        MOVE MID-IDPROJ-UT           TO WS-IDPROJ                         
042100     ELSE                                                                 
042200        MOVE MID-IDPROJ-IN           TO WS-IDPROJ                         
042300        MOVE '7'                     TO MFS-IDPFK                         
042400        MOVE SPACE                   TO MFS-KDTRTYP                       
042500     END-IF                                                               
042600     MOVE WS-IDPROJ                  TO MOD-IDPROJ-UT                     
042700     IF WS-IDPROJ = SPACE                                                 
042800        MOVE LOW-VALUE               TO W-IDPROJ-MIN                      
042900        MOVE HIGH-VALUE              TO W-IDPROJ-MAX                      
043000     ELSE                                                                 
043100        MOVE WS-IDPROJ               TO W-IDPROJ-MIN                      
043200                                        W-IDPROJ-MAX                      
043300     END-IF                                                               
043400     IF NYCKLAR-OK                                                        
043500        PERFORM BA-KOLLA-TANGENT-SAMT-LAES                                
043600     ELSE                                                                 
043700        MOVE FEL-2 (SPRAK-IX)         TO MOD-TEMFSFEL                     
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 BA-KOLLA-TANGENT-SAMT-LAES SECTION.                                      
044200     SKIP2                                                                
044300     IF MFS-IDPFK = '8'                                                   
044400       IF MID-IDARTNR-HI NUMERIC                                          
044500          IF MID-IDARTNR-HI ZERO                                          
044600             MOVE '7'                  TO MFS-IDPFK                       
044700          ELSE                                                            
044800             MOVE MID-IDARTNR-HI       TO W-IDARTNR-MIN                   
044900             MOVE MID-IDAO-HI          TO W-IDAO-MIN                      
045000          END-IF                                                          
045100       ELSE                                                               
045200          MOVE '7'                     TO MFS-IDPFK                       
045300       END-IF                                                             
045400     ELSE                                                                 
045500        IF MFS-IDPFK = ' '                                                
045600           IF MID-IDARTNR-LO NUMERIC                                      
045700              MOVE MID-IDARTNR-LO     TO W-IDARTNR-MIN                    
045800              MOVE MID-IDAO-LO        TO W-IDAO-MIN                       
045900           ELSE                                                           
046000              MOVE '7'                TO MFS-IDPFK                        
046100           END-IF                                                         
046200        END-IF                                                            
046300     END-IF                                                               
046400     IF MFS-IDPFK = '7'                                                   
046500        MOVE SPACE                  TO MFS-KDTRTYP                        
046600        MOVE ZERO                   TO W-IDARTNR-MIN                      
046700        MOVE MED-3 (SPRAK-IX)       TO MOD-TEMFSFEL                       
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 C-KOLLA-INDATA SECTION.                                                  
047200     MOVE JA                                   TO SW-INDATA-OK            
047300     MOVE +1                                   TO RAD-INDX                
047400     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
047500        IF MID-BORTTAG-IN (RAD-INDX) = ALL '+'                            
047600           IF MID-NEDB-IN (RAD-INDX) = ALL '+'                            
047700              IF MID-RS-IN   (RAD-INDX) = ALL '+'                         
047800                 IF MID-NY-IDBERED-IN (RAD-INDX) = ALL '+'                
047900                    CONTINUE                                              
048000                 ELSE                                                     
048100                    INSPECT MID-IDARTNR (RAD-INDX) REPLACING              
048200                                        LEADING SPACE BY ZERO             
048300                    IF MID-IDARTNR (RAD-INDX) NUMERIC                     
048400                       MOVE MID-IDARTNR (RAD-INDX)  TO W-IDARTNR          
048500                       PERFORM IMS-GU-ARTG01                              
048600                       IF SEGMENT-FINNS                                   
048700                          MOVE MFS-NUM-FAELT-RAETT  TO                    
048800                              MOD-IDARTNR-ATTR (RAD-INDX)                 
048900                          PERFORM CD-KOLLA-NY-BERED                       
049000                       ELSE                                               
049100                          MOVE NEJ           TO SW-INDATA-OK              
049200                          PERFORM CE-ASAETT-ATTR                          
049300                       END-IF                                             
049400                    ELSE                                                  
049500                       MOVE NEJ              TO SW-INDATA-OK              
049600                       PERFORM CE-ASAETT-ATTR                             
049700                    END-IF                                                
049800                 END-IF                                                   
049900              ELSE                                                        
050000                 INSPECT MID-IDARTNR (RAD-INDX) REPLACING                 
050100                                     LEADING SPACE BY ZERO                
050200                 IF MID-IDARTNR (RAD-INDX) NUMERIC                        
050300                    MOVE MID-IDARTNR (RAD-INDX)    TO W-IDARTNR           
050400                    PERFORM IMS-GU-ARTG01                                 
050500                    IF SEGMENT-FINNS                                      
050600                       MOVE MFS-NUM-FAELT-RAETT  TO                       
050700                              MOD-IDARTNR-ATTR (RAD-INDX)                 
050800                       PERFORM CC-KOLLA-RS-IN                             
050900                       IF MID-NY-IDBERED-IN (RAD-INDX) = ALL '+'          
051000                          CONTINUE                                        
051100                       ELSE                                               
051200                          PERFORM CD-KOLLA-NY-BERED                       
051300                       END-IF                                             
051400                    ELSE                                                  
051500                       MOVE NEJ              TO SW-INDATA-OK              
051600                       PERFORM CE-ASAETT-ATTR                             
051700                    END-IF                                                
051800                 ELSE                                                     
051900                    MOVE NEJ                 TO SW-INDATA-OK              
052000                    PERFORM CE-ASAETT-ATTR                                
052100                 END-IF                                                   
052200              END-IF                                                      
052300           ELSE                                                           
052400              INSPECT MID-IDARTNR (RAD-INDX) REPLACING                    
052500                                  LEADING SPACE BY ZERO                   
052600              IF MID-IDARTNR (RAD-INDX) NUMERIC                           
052700                 MOVE MID-IDARTNR (RAD-INDX)    TO W-IDARTNR              
052800                 PERFORM IMS-GU-ARTG01                                    
052900                 IF SEGMENT-FINNS                                         
053000                    MOVE MFS-NUM-FAELT-RAETT    TO                        
053100                    MOD-IDARTNR-ATTR (RAD-INDX)                           
053200                    PERFORM CB-KOLLA-NEDB                                 
053300                    IF MID-RS-IN      (RAD-INDX) = ALL '+'                
053400                       CONTINUE                                           
053500                    ELSE                                                  
053600                       PERFORM CC-KOLLA-RS-IN                             
053700                    END-IF                                                
053800                    IF MID-NY-IDBERED-IN (RAD-INDX) = ALL '+'             
053900                       CONTINUE                                           
054000                    ELSE                                                  
054100                       PERFORM CD-KOLLA-NY-BERED                          
054200                    END-IF                                                
054300                 ELSE                                                     
054400                    MOVE NEJ                    TO SW-INDATA-OK           
054500                    PERFORM CE-ASAETT-ATTR                                
054600                 END-IF                                                   
054700              ELSE                                                        
054800                 MOVE NEJ                       TO SW-INDATA-OK           
054900                 PERFORM CE-ASAETT-ATTR                                   
055000              END-IF                                                      
055100           END-IF                                                         
055200        ELSE                                                              
055300           PERFORM CA-KOLLA-BORTTAG                                       
055400        END-IF                                                            
055500        ADD +1                                 TO RAD-INDX                
055600     END-PERFORM                                                          
055700     .                                                                    
055800     EJECT                                                                
055900 CA-KOLLA-BORTTAG   SECTION.                                              
056000     SKIP2                                                                
056100     IF MID-NY-IDBERED-IN (RAD-INDX) = ALL '+'                            
056200        CONTINUE                                                          
056300     ELSE                                                                 
056400        MOVE NEJ                               TO SW-INDATA-OK            
056500        MOVE MFS-NUM-FAELT-FEL                 TO                         
056600                              MOD-NY-IDBERED-IN-ATTR (RAD-INDX)           
056700     END-IF                                                               
056800     IF MID-NEDB-IN    (RAD-INDX) = ALL '+'                               
056900        CONTINUE                                                          
057000     ELSE                                                                 
057100        MOVE NEJ                               TO SW-INDATA-OK            
057200        MOVE MFS-NUM-FAELT-FEL                 TO                         
057300                           MOD-NEDB-IN-ATTR (RAD-INDX)                    
057400     END-IF                                                               
057500     IF MID-RS-IN      (RAD-INDX) = ALL '+'                               
057600        CONTINUE                                                          
057700     ELSE                                                                 
057800        MOVE NEJ                               TO SW-INDATA-OK            
057900        MOVE MFS-ALFA-FAELT-FEL                TO                         
058000                           MOD-RS-IN-ATTR (RAD-INDX)                      
058100     END-IF                                                               
058200     IF MID-BORTTAG-IN  (RAD-INDX)  =  'B'                                
058300        MOVE MFS-ALFA-FAELT-RAETT              TO                         
058400                              MOD-BORTTAG-IN-ATTR  (RAD-INDX)             
058500        INSPECT MID-IDARTNR (RAD-INDX) REPLACING LEADING SPACE            
058600                                                       BY ZERO            
058700        IF MID-IDARTNR (RAD-INDX) NUMERIC                                 
058800           MOVE MID-IDARTNR (RAD-INDX)               TO W-IDARTNR         
058900           PERFORM IMS-GU-ARTG01                                          
059000           IF SEGMENT-FINNS                                               
059100              MOVE MFS-NUM-FAELT-RAETT            TO                      
059200                                    MOD-IDARTNR-ATTR (RAD-INDX)           
059300              PERFORM CAA-KOLLA-ARTG01                                    
059400           ELSE                                                           
059500              MOVE NEJ                            TO SW-INDATA-OK         
059600              MOVE MFS-NUM-FAELT-FEL              TO                      
059700                              MOD-IDARTNR-ATTR     (RAD-INDX)             
059800           END-IF                                                         
059900        ELSE                                                              
060000           MOVE NEJ                            TO SW-INDATA-OK            
060100           MOVE MFS-NUM-FAELT-FEL              TO                         
060200                              MOD-IDARTNR-ATTR     (RAD-INDX)             
060300        END-IF                                                            
060400     ELSE                                                                 
060500        MOVE NEJ                               TO SW-INDATA-OK            
060600        MOVE MFS-ALFA-FAELT-FEL                TO                         
060700                                 MOD-BORTTAG-IN-ATTR (RAD-INDX)           
060800        MOVE MFS-NUM-FAELT-RAETT               TO                         
060900                          MOD-IDARTNR-ATTR (RAD-INDX)                     
061000     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061300 CAA-KOLLA-ARTG01        SECTION.                                         
061400     SKIP2                                                                
061500     IF ARTG01-ART-KDRESBED =  ' '                                        
061600        MOVE NEJ                         TO SW-INDATA-OK                  
061700        MOVE MFS-ALFA-FAELT-FEL          TO                               
061800                            MOD-BORTTAG-IN-ATTR (RAD-INDX)                
061900     ELSE                                                                 
062000        IF ARTG01-ART-KDRESBED =  'U'                                     
062100           IF ARTG01-ART-FLAENDR  =  'J'                                  
062200              CONTINUE                                                    
062300           ELSE                                                           
062400              MOVE NEJ                   TO SW-INDATA-OK                  
062500              MOVE MFS-ALFA-FAELT-FEL    TO                               
062600                          MOD-BORTTAG-IN-ATTR  (RAD-INDX)                 
062700           END-IF                                                         
062800        END-IF                                                            
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 CB-KOLLA-NEDB      SECTION.                                              
063300     SKIP2                                                                
063400     MOVE MID-NEDB-IN   (RAD-INDX)    TO WS-NEDB-X4                       
063500     MOVE WS-NEDB-X4                  TO DAT-I-TIDATUM                    
063600     MOVE 'AAVV  '                    TO DAT-KDDATFORM                    
063700     PERFORM S99-CALL-WDATKONV                                            
063800     IF DAT-KDSVAR-OK                                                     
063900        MOVE MFS-NUM-FAELT-RAETT               TO                         
064000                             MOD-NEDB-IN-ATTR (RAD-INDX)                  
064100     ELSE                                                                 
064200        MOVE NEJ                               TO SW-INDATA-OK            
064300        MOVE MFS-NUM-FAELT-FEL                 TO                         
064400                             MOD-NEDB-IN-ATTR (RAD-INDX)                  
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 CC-KOLLA-RS-IN     SECTION.                                              
064900     SKIP3                                                                
065000     IF MID-RS-IN (RAD-INDX)  = '-'                                       
065100        MOVE MFS-ALFA-FAELT-RAETT              TO                         
065200                                     MOD-RS-IN-ATTR  (RAD-INDX)           
065300        PERFORM CCA-KOLLA-MOT-ARTG01                                      
065400     ELSE                                                                 
065500        MOVE NEJ                               TO SW-INDATA-OK            
065600        MOVE MFS-ALFA-FAELT-FEL                TO                         
065700                            MOD-RS-IN-ATTR  (RAD-INDX)                    
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100 CCA-KOLLA-MOT-ARTG01  SECTION.                                           
066200     SKIP2                                                                
066300     IF ARTG01-ART-KDRESBED = ' '                                         
066400        CONTINUE                                                          
066500     ELSE                                                                 
066600        IF  ARTG01-ART-KDRESBED = 'U'                                     
066700        AND ARTG01-ART-FLAENDR  = 'N'                                     
066800           CONTINUE                                                       
066900        ELSE                                                              
067000           MOVE NEJ                      TO SW-INDATA-OK                  
067100           MOVE MFS-ALFA-FAELT-FEL       TO                               
067200                              MOD-RS-IN-ATTR   (RAD-INDX)                 
067300        END-IF                                                            
067400     END-IF                                                               
067500     .                                                                    
067600     EJECT                                                                
067700 CD-KOLLA-NY-BERED  SECTION.                                              
067800     SKIP2                                                                
067900     IF MID-NY-IDBERED-IN (RAD-INDX) NUMERIC                              
068000        MOVE MFS-NUM-FAELT-RAETT               TO                         
068100                      MOD-NY-IDBERED-IN-ATTR (RAD-INDX)                   
068200     ELSE                                                                 
068300        MOVE NEJ                               TO SW-INDATA-OK            
068400        MOVE MFS-NUM-FAELT-FEL                 TO                         
068500                      MOD-NY-IDBERED-IN-ATTR (RAD-INDX)                   
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900 CE-ASAETT-ATTR     SECTION.                                              
069000     SKIP2                                                                
069100     MOVE MFS-NUM-FAELT-FEL                 TO                            
069200                                MOD-IDARTNR-ATTR (RAD-INDX)               
069300     IF MID-BORTTAG-IN    (RAD-INDX) = ALL '+'                            
069400        CONTINUE                                                          
069500     ELSE                                                                 
069600        MOVE MFS-ALFA-FAELT-RAETT           TO                            
069700                             MOD-BORTTAG-IN-ATTR (RAD-INDX)               
069800     END-IF                                                               
069900     IF MID-NY-IDBERED-IN (RAD-INDX) = ALL '+'                            
070000        CONTINUE                                                          
070100     ELSE                                                                 
070200        MOVE MFS-NUM-FAELT-RAETT            TO                            
070300                             MOD-NY-IDBERED-IN-ATTR (RAD-INDX)            
070400     END-IF                                                               
070500     IF MID-RS-IN      (RAD-INDX) = ALL '+'                               
070600        CONTINUE                                                          
070700     ELSE                                                                 
070800        MOVE MFS-ALFA-FAELT-RAETT           TO                            
070900                                     MOD-RS-IN-ATTR (RAD-INDX)            
071000     END-IF                                                               
071100     IF MID-NEDB-IN    (RAD-INDX) = ALL '+'                               
071200        CONTINUE                                                          
071300     ELSE                                                                 
071400        MOVE MFS-NUM-FAELT-RAETT            TO                            
071500                                   MOD-NEDB-IN-ATTR (RAD-INDX)            
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 D-UPPDATERA SECTION.                                                     
072000     SKIP2                                                                
072100     MOVE +1                              TO RAD-INDX                     
072200     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
072300        IF MID-BORTTAG-IN    (RAD-INDX) = ALL '+' AND                     
072400           MID-NY-IDBERED-IN (RAD-INDX) = ALL '+' AND                     
072500           MID-NEDB-IN       (RAD-INDX) = ALL '+' AND                     
072600           MID-RS-IN         (RAD-INDX) = ALL '+'                         
072700           CONTINUE                                                       
072800        ELSE                                                              
072900           MOVE MID-IDARTNR   (RAD-INDX)       TO                         
073000                                             W-IDARTNR                    
073100           PERFORM IMS-GHU-ARTG01                                         
073200           IF MID-BORTTAG-IN     (RAD-INDX) = ALL '+'                     
073300              IF MID-NY-IDBERED-IN     (RAD-INDX) = ALL '+'               
073400                 CONTINUE                                                 
073500              ELSE                                                        
073600                 MOVE MID-NY-IDBERED-IN (RAD-INDX) TO                     
073700                                          ARTG01-ART-IDBERED              
073800              END-IF                                                      
073900              IF MID-NEDB-IN           (RAD-INDX) = ALL '+'               
074000                 CONTINUE                                                 
074100              ELSE                                                        
074200                 MOVE MID-NEDB-IN   (RAD-INDX) TO WS-NEDB-X4              
074300                 MOVE WS-NEDB-X4               TO DAT-I-TIDATUM           
074400                 MOVE 'AAVV  '                 TO DAT-KDDATFORM           
074500                 PERFORM S99-CALL-WDATKONV                                
074600                 IF DAT-KDSVAR-OK                                         
074700                    MOVE DAT-TIAAMMDD          TO                         
074800                                          ARTG01-ART-TINEDBRY             
074900                 END-IF                                                   
075000              END-IF                                                      
075100              IF MID-RS-IN             (RAD-INDX) = ALL '+'               
075200                 PERFORM IMS-REPL-ARTG01                                  
075300              ELSE                                                        
075400                 MOVE MID-RS-IN  (RAD-INDX)    TO                         
075500                                          ARTG01-ART-KDRESBED             
075600                 IF  MID-NEDB-IN  (RAD-INDX) = ALL '+'                    
075700                 AND ARTG01-ART-TINEDBRY     = ZERO                       
075800                    MOVE NEJ                      TO                      
075900                                          ARTG01-ART-FLBERQ               
076000                    MOVE NEJ                      TO                      
076100                                          ARTG01-ART-FLAENDR              
076200                 END-IF                                                   
076300                 IF ARTG01-ART-KDRESBED = SPACE                           
076400                    PERFORM IMS-REPL-ARTG01                               
076500                    PERFORM  DB-SKAPA-KDP-KOLA-TRANS                      
076600                 ELSE                                                     
076700                    PERFORM IMS-REPL-ARTG01                               
076800                 END-IF                                                   
077100              END-IF                                                      
077200           ELSE                                                           
077300              MOVE NEJ                         TO                         
077400                                         ARTG01-ART-FLBERQ                
077500              MOVE NEJ                         TO                         
077600                                          ARTG01-ART-FLAENDR              
077700              MOVE ZERO                        TO                         
077800                                         ARTG01-ART-TINEDBRY              
077900              PERFORM IMS-REPL-ARTG01                                     
078000           END-IF                                                         
078100        END-IF                                                            
078200        ADD +1                                 TO RAD-INDX                
078300     END-PERFORM                                                          
078400     .                                                                    
078500     EJECT                                                                
080500 DB-SKAPA-KDP-KOLA-TRANS   SECTION.                                       
080600     ACCEPT ZZAC-TIKLOCK        FROM TIME                                 
080700     ACCEPT ZZAC-TIAAMMDD       FROM DATE                                 
080800******************************************************************        
080900*    IDLOGLOP = 4, FÖR ATT SKLIJA TRANSAR FRÅN 1113,1115,1117,1142        
081000******************************************************************        
081100     MOVE +4                                   TO W-IDLOGLOP              
081200     MOVE W-IDLOGLOP                           TO ZZAC-IDLOGLOP           
081300     MOVE 'RZU'                                TO KDP-IDPTYP              
081400     MOVE '-'                                  TO KDP-KDUART              
081500     MOVE W-IDARTNR                            TO KDP-IDARTNR             
081600                                                  W092-SORTBGP            
081700     MOVE KDP-W10111                           TO ZZAC-LOGGPOST           
081800     MOVE W092-AREA                            TO ZZAC-SORTPOST           
081900     PERFORM IMS-ISRT-ZZAC                                                
082000     .                                                                    
082100     EJECT                                                                
082200 E-MFS-ROER-EJ-FAELT SECTION.                                             
082300     MOVE MFS-ROER-EJ-FAELT          TO                                   
082400                                    MOD-IDARTNR-LO                        
082500                                    MOD-IDAO-LO                           
082600                                    MOD-IDARTNR-HI                        
082700                                    MOD-IDAO-HI                           
082800     MOVE +1                         TO RAD-INDX                          
082900     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
083000        MOVE MFS-ROER-EJ-FAELT       TO                                   
083100                                    MOD-IDARTNR       (RAD-INDX)          
083200                                    MOD-BEART-SVE     (RAD-INDX)          
083300                                    MOD-IDAO          (RAD-INDX)          
083400                                    MOD-IDFKNGRP      (RAD-INDX)          
083500                                    MOD-RS-IN         (RAD-INDX)          
083600                                    MOD-TISERLEV      (RAD-INDX)          
083700                                    MOD-IDPROJ        (RAD-INDX)          
083800                                    MOD-TISLUBER      (RAD-INDX)          
083900                                    MOD-NEDB-IN       (RAD-INDX)          
084000                                    MOD-NY-IDBERED-IN (RAD-INDX)          
084100                                    MOD-TETEKNIK      (RAD-INDX)          
084200                                    MOD-BORTTAG-IN    (RAD-INDX)          
084300                              MOD-KONVERTERAD-ARTIKEL (RAD-INDX)          
084400     ADD +1                          TO RAD-INDX                          
084500     END-PERFORM                                                          
084600     .                                                                    
084700     EJECT                                                                
084800 F-RENSA-SIDAN SECTION.                                                   
084900     MOVE +1                           TO RAD-INDX                        
085000     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
085100           MOVE MFS-RENSA-FAELT        TO                                 
085200                                   MOD-SELECT-ARTIKEL (RAD-INDX)          
085300                                    MOD-IDARTNR       (RAD-INDX)          
085400                                    MOD-BEART-SVE     (RAD-INDX)          
085500                                    MOD-IDAO          (RAD-INDX)          
085600                                    MOD-IDFKNGRP      (RAD-INDX)          
085700                                    MOD-RS-IN         (RAD-INDX)          
085800                                    MOD-TISERLEV      (RAD-INDX)          
085900                                    MOD-IDPROJ        (RAD-INDX)          
086000                                    MOD-TISLUBER      (RAD-INDX)          
086100                                    MOD-NEDB-IN       (RAD-INDX)          
086200                                    MOD-NY-IDBERED-IN (RAD-INDX)          
086300                                    MOD-TETEKNIK      (RAD-INDX)          
086400                                    MOD-BORTTAG-IN    (RAD-INDX)          
086500                              MOD-KONVERTERAD-ARTIKEL (RAD-INDX)          
086600           ADD +1                      TO RAD-INDX                        
086700     END-PERFORM                                                          
086800     .                                                                    
086900     EJECT                                                                
087000 G-VISA-SIDAN   SECTION.                                                  
087100     MOVE +1                            TO RAD-INDX                       
087200     PERFORM UNTIL SEGMENT-SAKNAS OR RAD-INDX > MAX-RAD                   
087300        MOVE ARTH01-SEQA-IDARTNR        TO                                
087400                                     W-IDARTNR                            
087500        PERFORM IMS-GU-ARTG01                                             
087600        PERFORM GA-TESTA-NEDB-EV-FLYTTA-T-BILD                            
087700        PERFORM IMS-GN-ARTH01                                             
087800     END-PERFORM                                                          
087900     IF RAD-INDX > MAX-RAD                                                
088000        IF SEGMENT-FINNS                                                  
088100           MOVE ARTH01-SEQA-IDARTNR     TO                                
088200                                        MOD-IDARTNR-HI                    
088300           MOVE ARTH01-SEQA-IDAO        TO                                
088400                                        MOD-IDAO-HI                       
088500           IF MFS-UPDATE                                                  
088600              CONTINUE                                                    
088700           ELSE                                                           
088800              MOVE MED-2 (SPRAK-IX)        TO                             
088900                                           MOD-TEMFSINF                   
089000           END-IF                                                         
089100        END-IF                                                            
089200     ELSE                                                                 
089300        PERFORM GB-RENSA-SIDAN                                            
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 GA-TESTA-NEDB-EV-FLYTTA-T-BILD   SECTION.                                
089701                                                                          
089800     IF ARTG01-ART-TINEDBRY  = ZERO                                       
089900        MOVE MFS-RENSA-FAELT           TO MOD-NEDB-IN (RAD-INDX)          
090000        PERFORM GAA-FLYTTA-TILL-BILD                                      
090100     ELSE                                                                 
090200        MOVE ARTG01-ART-TINEDBRY       TO DAT-I-TIDATUM                   
090300        MOVE 'AAMMDD'                  TO DAT-KDDATFORM                   
090400        PERFORM S99-CALL-WDATKONV                                         
090500        IF DAT-KDSVAR-OK                                                  
090600           MOVE DAT-TIAA-VECKA         TO W-AA                            
090700           MOVE DAT-TIVV               TO W-VV                            
090701           MOVE W-DAGENS-AAVV-N TO TMP1-YYWW                              
090702           MOVE W-AAVV-N        TO TMP2-YYWW                              
090710           PERFORM WY2000P3                                               
090800           IF TMP1-YYWW >= TMP2-YYWW                                      
090900              MOVE W-AAVV              TO MOD-NEDB-IN (RAD-INDX)          
091000              PERFORM GAA-FLYTTA-TILL-BILD                                
091100           END-IF                                                         
091200        END-IF                                                            
091300     END-IF                                                               
091400     .                                                                    
091500     EJECT                                                                
091600 GAA-FLYTTA-TILL-BILD SECTION.                                            
091700     IF RAD-INDX = +1                                                     
091800        MOVE ARTG01-ART-IDARTNR         TO                                
091900                                           MOD-IDARTNR-LO                 
092000        MOVE ARTG01-ART-IDAO            TO                                
092100                                           MOD-IDAO-LO                    
092200     END-IF                                                               
092300     MOVE ARTG01-ART-IDARTNR                   TO                         
092400                                     MOD-IDARTNR     (RAD-INDX)           
092500     MOVE ARTG01-ART-BEART-SVE                 TO                         
092600                                     MOD-BEART-SVE   (RAD-INDX)           
092700     MOVE ARTG01-ART-IDAO                      TO                         
092800                                     MOD-IDAO        (RAD-INDX)           
092900     MOVE ARTG01-ART-IDFKNGRP                  TO                         
093000                                     MOD-IDFKNGRP    (RAD-INDX)           
093100     MOVE ARTG01-ART-KDRESBED                  TO                         
093200                                     MOD-RS-IN       (RAD-INDX)           
093300     MOVE +1                                   TO INDX                    
093400     MOVE  9999999                             TO                         
093500                                           W-SPAR-TISERLEV                
093600     PERFORM UNTIL INDX > 5 OR ARTG01-ART-TISERLEV (INDX) = ZERO          
093640        MOVE ARTG01-ART-TISERLEV(INDX) TO TMP1-YYMMDD                     
093650        MOVE W-SPAR-TISERLEV           TO TMP2-YYMMDD                     
093660        PERFORM WY2000P1                                                  
093700        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
093800           MOVE ARTG01-ART-TISERLEV (INDX)     TO                         
093900                                       W-SPAR-TISERLEV                    
093910        END-IF                                                            
094010*****   IF ARTG01-ART-TISERLEV (INDX) <  W-SPAR-TISERLEV                  
094020*****      MOVE ARTG01-ART-TISERLEV (INDX)     TO                         
094030*****                                    W-SPAR-TISERLEV                  
094040*****   END-IF                                                            
094100        ADD +1                                 TO INDX                    
094200     END-PERFORM                                                          
094300     IF W-SPAR-TISERLEV  = 9999999                                        
094400        MOVE MFS-RENSA-FAELT                   TO                         
094500                                        MOD-TISERLEV (RAD-INDX)           
094600     ELSE                                                                 
094700        MOVE W-SPAR-TISERLEV            TO DAT-I-TIDATUM                  
094800        MOVE 'AAMMDD'                   TO DAT-KDDATFORM                  
094900        PERFORM S99-CALL-WDATKONV                                         
095000        IF DAT-KDSVAR-OK                                                  
095100           MOVE DAT-TIAA-VECKA          TO W-AA                           
095200           MOVE DAT-TIVV                TO W-VV                           
095300           MOVE W-AAVV                  TO                                
095400                                        MOD-TISERLEV (RAD-INDX)           
095500        END-IF                                                            
095600     END-IF                                                               
095700     MOVE ARTG01-ART-IDPROJ             TO MOD-IDPROJ (RAD-INDX)          
095800     MOVE ARTG01-ART-TISLUBER           TO DAT-I-TIDATUM                  
095900     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
096000     PERFORM S99-CALL-WDATKONV                                            
096100     IF DAT-KDSVAR-OK                                                     
096200        MOVE DAT-TIAA-VECKA             TO W-AA                           
096300        MOVE DAT-TIVV                   TO W-VV                           
096400        MOVE W-AAVV                   TO MOD-TISLUBER (RAD-INDX)          
096500     ELSE                                                                 
096600        MOVE MFS-RENSA-FAELT          TO MOD-TISLUBER (RAD-INDX)          
096700     END-IF                                                               
096800     IF   ARTG01-ART-TETEKNIK = SPACE                                     
096900        MOVE SPACE                             TO                         
097000                                     MOD-TETEKNIK    (RAD-INDX)           
097100     ELSE                                                                 
097200        MOVE '*'                               TO                         
097300                                     MOD-TETEKNIK    (RAD-INDX)           
097400     END-IF                                                               
097500     IF ARTG01-ART-FLAENDR = JA                                           
097600        MOVE '*'                               TO                         
097700                             MOD-KONVERTERAD-ARTIKEL(RAD-INDX)            
097800     END-IF                                                               
097900     ADD +1                                    TO RAD-INDX                
098000     .                                                                    
098100     EJECT                                                                
098200 GB-RENSA-SIDAN SECTION.                                                  
098300     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
098400           MOVE MFS-RENSA-FAELT                TO                         
098500                                MOD-SELECT-ARTIKEL (RAD-INDX)             
098600                                     MOD-IDARTNR   (RAD-INDX)             
098700                                     MOD-BEART-SVE (RAD-INDX)             
098800                                     MOD-IDAO      (RAD-INDX)             
098900                                     MOD-IDFKNGRP  (RAD-INDX)             
099000                                     MOD-RS-IN     (RAD-INDX)             
099100                                     MOD-TISERLEV  (RAD-INDX)             
099200                                     MOD-IDPROJ    (RAD-INDX)             
099300                                     MOD-TISLUBER  (RAD-INDX)             
099400                                     MOD-NEDB-IN   (RAD-INDX)             
099500                                 MOD-NY-IDBERED-IN (RAD-INDX)             
099600                                     MOD-TETEKNIK  (RAD-INDX)             
099700                                    MOD-BORTTAG-IN (RAD-INDX)             
099800                           MOD-KONVERTERAD-ARTIKEL (RAD-INDX)             
099900           ADD +1                              TO RAD-INDX                
100000     END-PERFORM                                                          
100100     MOVE ZERO                                 TO                         
100200                                     MOD-IDARTNR-HI                       
100300     .                                                                    
100400     EJECT                                                                
100500 H-KOLLA-ATT-INDATA-EJ-IFYLLD SECTION.                                    
100600     SKIP2                                                                
100700     MOVE +1                             TO RAD-INDX                      
100800     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
100900        IF MID-RS-IN         (RAD-INDX) = ALL '+'                         
101000           CONTINUE                                                       
101100        ELSE                                                              
101200           MOVE JA                                 TO                     
101300                                   SW-INDATA-IFYLLT-EJ-UPPDATE            
101400           MOVE MFS-ALFA-FAELT-RAETT               TO                     
101500                     MOD-RS-IN-ATTR         (RAD-INDX)                    
101600        END-IF                                                            
101700        IF MID-NEDB-IN       (RAD-INDX) = ALL '+'                         
101800           CONTINUE                                                       
101900        ELSE                                                              
102000           MOVE JA                                 TO                     
102100                                   SW-INDATA-IFYLLT-EJ-UPPDATE            
102200           MOVE MFS-NUM-FAELT-RAETT               TO                      
102300                     MOD-NEDB-IN-ATTR       (RAD-INDX)                    
102400        END-IF                                                            
102500        IF MID-NY-IDBERED-IN (RAD-INDX) = ALL '+'                         
102600           CONTINUE                                                       
102700        ELSE                                                              
102800           MOVE JA                                 TO                     
102900                                   SW-INDATA-IFYLLT-EJ-UPPDATE            
103000           MOVE MFS-NUM-FAELT-RAETT               TO                      
103100                        MOD-NY-IDBERED-IN-ATTR (RAD-INDX)                 
103200        END-IF                                                            
103300        IF MID-BORTTAG-IN    (RAD-INDX) = ALL '+'                         
103400           CONTINUE                                                       
103500        ELSE                                                              
103600           MOVE JA                                 TO                     
103700                                   SW-INDATA-IFYLLT-EJ-UPPDATE            
103800           MOVE MFS-ALFA-FAELT-RAETT               TO                     
103900                           MOD-BORTTAG-IN-ATTR    (RAD-INDX)              
104000        END-IF                                                            
104100        ADD +1                           TO RAD-INDX                      
104200     END-PERFORM                                                          
104300     .                                                                    
104400     EJECT                                                                
104500 I-FORMATETS-ATTR   SECTION.                                              
104600     SKIP2                                                                
104700     MOVE +1                         TO RAD-INDX                          
104800     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
104900        MOVE MFS-FORMATETS-ATTR      TO                                   
105000                               MOD-IDARTNR-ATTR       (RAD-INDX)          
105100                               MOD-RS-IN-ATTR         (RAD-INDX)          
105200                               MOD-NEDB-IN-ATTR       (RAD-INDX)          
105300                               MOD-NY-IDBERED-IN-ATTR (RAD-INDX)          
105400                               MOD-BORTTAG-IN-ATTR    (RAD-INDX)          
105500     ADD +1                          TO RAD-INDX                          
105600     END-PERFORM                                                          
105700     .                                                                    
105800     EJECT                                                                
105900 J-RENSA-NYCKLAR        SECTION.                                          
106000     SKIP2                                                                
106100     MOVE MFS-RENSA-FAELT              TO                                 
106200                                   MOD-IDBERED-IN                         
106300                                   MOD-IDBERED-UT                         
106400                                   MOD-IDAO-IN                            
106500                                   MOD-IDAO-UT                            
106600                                   MOD-IDPROJ-IN                          
106700                                   MOD-IDPROJ-UT                          
106800     .                                                                    
106900     EJECT                                                                
107000 S99-CALL-WDATKONV    SECTION.                                            
107100     SKIP2                                                                
107200     CALL WDATKONV      USING             DAT-KDDATFORM                   
107300                                          DAT-I-TIDATUM                   
107400                                          DAT-O-TIDATUM                   
107500                                          DAT-KDSVAR                      
107600     .                                                                    
107700     EJECT                                                                
107800* IMS SEKTIONER                                                           
107900     SKIP3                                                                
108000 IMS-GET-MSG SECTION.                                                     
108100     SKIP1                                                                
108200     MOVE '  QC' TO GODK-STATUSKODER                                      
108300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
108400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     SKIP3                                                                
108700     .                                                                    
108800 IMS-INSERT-MSG SECTION.                                                  
108900     SKIP1                                                                
109000     IF NOT ENGLISH-TEXT                                                  
109100       MOVE '0' TO MFS-KDHUVOMR                                           
109200     END-IF                                                               
109300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
109400     MOVE SPACE TO GODK-STATUSKODER                                       
109500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
109600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     EJECT                                                                
109900     .                                                                    
110000 IMS-GN-ARTH01 SECTION.                                                   
110100     SKIP1                                                                
110200     STRING 'WLARTH01(WDD2A1KY>=' W-WDD2A1KY-MIN                          
110300                    '&WDD2A1KY<=' W-WDD2A1KY-MAX                          
110400                    '&IDPROJ  >=' W-IDPROJ-MIN                            
110500                    '&IDPROJ  <=' W-IDPROJ-MAX ')'                        
110600            DELIMITED BY SIZE INTO SSA1                                   
110700     MOVE '  GE' TO GODK-STATUSKODER                                      
110800     CALL CBLTDLI USING GN ARTH-PCB DLI-IO-AREA SSA1                      
110900     MOVE ARTH-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     SKIP3                                                                
111200     .                                                                    
111300 IMS-GU-ARTG01 SECTION.                                                   
111400     SKIP1                                                                
111500     STRING 'WLARTG01(IDARTNR  =' W-WDD201KY-X ')'                        
111600            DELIMITED BY SIZE INTO SSA1                                   
111700     MOVE '  GE' TO GODK-STATUSKODER                                      
111800     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA SSA1                      
111900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
112000     PERFORM IMS-STATUSKONTROLL                                           
112100     SKIP3                                                                
112200     .                                                                    
112300 IMS-GHU-ARTG01 SECTION.                                                  
112400     SKIP1                                                                
112500     STRING 'WLARTG01(IDARTNR  =' W-WDD201KY-X ')'                        
112600            DELIMITED BY SIZE INTO SSA1                                   
112700     MOVE '  ' TO GODK-STATUSKODER                                        
112800     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA SSA1                     
112900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
113000     PERFORM IMS-STATUSKONTROLL                                           
113100     SKIP3                                                                
113200     .                                                                    
116200 IMS-ISRT-ZZAC SECTION.                                                   
116300     SKIP1                                                                
116400     MOVE 'WLZZAC01 '                          TO SSA1                    
116500     MOVE '  '   TO GODK-STATUSKODER                                      
116600     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
116700     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
116800     PERFORM IMS-STATUSKONTROLL                                           
116900     SKIP3                                                                
117000     .                                                                    
117100 IMS-REPL-ARTG01 SECTION.                                                 
117200     SKIP1                                                                
117300     MOVE '  '   TO GODK-STATUSKODER                                      
117400     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA                         
117500     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     SKIP3                                                                
117800     .                                                                    
117900 IMS-STATUSKONTROLL SECTION.                                              
118000     SKIP1                                                                
118100     SET STATUS-IX TO 1                                                   
118200     SEARCH GODK-STATUS                                                   
118210       AT END                                                             
118220         CALL FELLOG                                                      
118300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
118400       CONTINUE                                                           
118500     END-SEARCH                                                           
118600     .                                                                    
118610     EJECT                                                                
118800*    -COPY WY2000P3                                                       
118810     EJECT                                                                
118900*    -COPY WY2000P1                                                       
