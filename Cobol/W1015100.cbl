000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1015100.                                                
000400 AUTHOR.         JANNE MELANDER                                           
000500 DATE-WRITTEN.   DEC.  87.                                                
000600                                                                          
000700***************************************************************           
000800*                                                             *           
000900*    FUNKTION.                                                *           
001000*                                                             *           
001100*        PROGRAMMET UPPDATERAR MARKNADSSTRUKTUREN (BASLAGER)  *           
001200*                                                             *           
001300*        MAN KAN KAN LÄGGA UPP             EN NY MARKNAD  I,N *           
001400*                    TA BORT                  EN MARKNAD  D,B *           
001500*                    ÄNDRA DIST INDELNINGEN I EN MARKNAD  R,Ä *           
001600*                                                             *           
001700*        BASEN SOM BERÖRS HETER:                              *           
001800*                                                             *           
001900*        FYSISKT WDG201  (ROT, KDPRODSL = NYCKEL)             *           
002000*                WDG220  (BARN, KDBASLM = NYCKEL)             *           
002100*                                                             *           
002200*        LOGISKT WLXXAO01 (ROT)                               *           
002300*                WLXXAO11 (BARN)                              *           
002400*                                                             *           
002500***************************************************************           
002600     INDATA.                                                              
002700         TRANSAKTION: W1T151                                              
002800         MID:         W1I15101                                            
002900                                                                          
003000     UTDATA.                                                              
003100         MOD:         W1O15101                                            
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W1015100'.            
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  COL-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004500 77  DIST-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  MAX-IX-PLUS-1               PIC S9(9)   VALUE +15  COMP SYNC.        
004700 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +532 COMP SYNC.        
004800 77  SW-INDATA-OK                PIC X(1)    VALUE SPACE.                 
004900 77  SW-UPPDAT-OK                PIC X(1)    VALUE SPACE.                 
005000                                                                          
005100 77  WS-KDPRODSL                 PIC 9(2)    VALUE ZERO.                  
005200                                                                          
005220 01  DYNAMISKA-SUBPROGRAM.                                                
005230     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005240     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005250                                                                          
005300 01  INDATA-SW                   PIC X       VALUE 'N'.                   
005400    88 INDATA-OK                             VALUE 'J'.                   
005500     SKIP3                                                                
005600 01  NYCKLAR-TILL-DLI.                                                    
005700   03  W-1121-KEY-X.                                                      
005800     05  FILLER                  PIC X(4)   VALUE '1121'.                 
005900     05  W-1121-KDPRODSL         PIC S9(3)  COMP-3 VALUE ZERO.            
006000     05  FILLER                  PIC X(24)  VALUE LOW-VALUE.              
006100                                                                          
006200   03  W-1122-KEY-X.                                                      
006300     05  W-1122-KDBASLM          PIC X(6)   VALUE SPACE.                  
006400     05  FILLER                  PIC X(9)   VALUE LOW-VALUE.              
006500     EJECT                                                                
006600 01  MEDDELANDE.                                                          
006700   03  FEL1.                                                              
006800     05 FILLER                   PIC X(40)                                
006900          VALUE 'UPPLYSTA FÄLT FEL'.                                      
007000     05 FILLER                   PIC X(40)                                
007100          VALUE 'CORRECT HIGHLIGHTED FIELDS'.                             
007200   03  FILLER REDEFINES FEL1.                                             
007300     05  FEL-1                   PIC X(40)   OCCURS 2.                    
007400                                                                          
007500   03  FEL2.                                                              
007600     05 FILLER                   PIC X(40)                                
007700          VALUE 'SÖKT PRODUKTSLAG SAKNAS I BASEN'.                        
007800     05 FILLER                   PIC X(40)                                
007900          VALUE 'PROD.GROUP IS MISSING     '.                             
008000   03  FILLER REDEFINES FEL2.                                             
008100     05  FEL-2                   PIC X(40)   OCCURS 2.                    
008200                                                                          
008300   03  FEL3.                                                              
008400     05 FILLER                   PIC X(40)                                
008500          VALUE 'ANGIVEN MARKNAD SAKNAS I BASEN'.                         
008600     05 FILLER                   PIC X(40)                                
008700          VALUE 'MARKET IS MISSING       '.                               
008800   03  FILLER REDEFINES FEL3.                                             
008900     05  FEL-3                   PIC X(40)   OCCURS 2.                    
009000                                                                          
009100   03  FEL4.                                                              
009200     05 FILLER                   PIC X(40)                                
009300          VALUE 'ANGIVEN MARKNAD FINNS REDAN   '.                         
009400     05 FILLER                   PIC X(40)                                
009500          VALUE 'MARKET ALREADY EXISTS  '.                                
009600   03  FILLER REDEFINES FEL4.                                             
009700     05  FEL-4                   PIC X(40)   OCCURS 2.                    
009800                                                                          
009900   03  MED1.                                                              
010000     05 FILLER                   PIC X(40)                                
010100          VALUE 'UPPDATERING GJORD        '.                              
010200     05 FILLER                   PIC X(40)                                
010300          VALUE 'UPDATED                     '.                           
010400   03  FILLER REDEFINES MED1.                                             
010500     05  MED-1                   PIC X(40)   OCCURS 2.                    
010600                                                                          
010700   03  MED2.                                                              
010800     05 FILLER                   PIC X(40)                                
010900          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
011000     05 FILLER                   PIC X(40)                                
011100          VALUE 'PRESS PF8 FOR MORE LINES'.                               
011200   03  FILLER REDEFINES MED2.                                             
011300     05  MED-2                   PIC X(40)   OCCURS 2.                    
011400                                                                          
011500   03  MED3.                                                              
011600     05 FILLER                   PIC X(40)                                
011700          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
011800     05 FILLER                   PIC X(40)                                
011900          VALUE 'THIS IS THE FIRST PAGE'.                                 
012000   03  FILLER REDEFINES MED3.                                             
012100     05  MED-3                   PIC X(40)   OCCURS 2.                    
012200                                                                          
012300   03  MED4.                                                              
012400     05 FILLER                   PIC X(40)                                
012500          VALUE 'DETTA ÄR SISTA SIDAN'.                                   
012600     05 FILLER                   PIC X(40)                                
012700          VALUE 'THIS IS THE LAST PAGE'.                                  
012800   03  FILLER REDEFINES MED4.                                             
012900     05  MED-4                   PIC X(40)   OCCURS 2.                    
013000     EJECT                                                                
013100******************************************************************        
013200*                                                                         
013300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
013600     SKIP3                                                                
013700*01  MID -COPY W1I15101                                                   
013900     EJECT                                                                
014000*01  -COPY WMSGAREA                                                       
014200     EJECT                                                                
014300*  03  MOD -COPY W1O15101           -RED MSG-AREA.                        
014500     EJECT                                                                
014600*01  -COPY WMFSAREA                                                       
014800     EJECT                                                                
014900******************************************************************        
015000*                                                                         
015100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015200*                                                                         
015300 01  IMS-WS.                                                              
015400   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
015500     SKIP3                                                                
015600*                        **** STATUS-KOD FRÅN IMS                         
015700   03  STATUS-WS                 PIC XX.                                  
015800     88  SEGMENT-FINNS                       VALUE '  '.                  
015900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016000     SKIP3                                                                
016100   03  GODK-STATUSKODER.                                                  
016200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016300     SKIP3                                                                
016400 01    SSA1                      PIC X(64).                               
016500 01    SSA2                      PIC X(64).                               
016600     EJECT                                                                
016700*                            IMS FUNKTIONSKODER                           
016800*01    -COPY W0003                                                        
017000     EJECT                                                                
017100*                            DLI INPUT-OUTPUT AREA                        
017200 01  DLI-IO-AREA.                                                         
017300   03  IO-AREA                   PIC X(300)  VALUE SPACE.                 
017400     SKIP3                                                                
017500*  03  WLXXAO01  -COPY WDGX1121 -PRE XXAO01-   -RED IO-AREA.              
017700     EJECT                                                                
017800*  03  WLXXAO11  -COPY WDGX1122  -RED IO-AREA.                            
018000     EJECT                                                                
018100 LINKAGE SECTION.                                                         
018200*01  -COPY W0009     -PRE MSG-                                            
018400     SKIP2                                                                
018500*01  -COPY W0008     -PRE XXAO-                                           
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900 PROCEDURE DIVISION USING MSG-PCB XXAO-PCB.                               
019000     ENTRY 'DLITCBL' USING MSG-PCB XXAO-PCB.                              
019100                                                                          
019200     PERFORM IMS-GET-MSG                                                  
019300     IF SEGMENT-FINNS                                                     
019400       PERFORM A-INIT-SPARA-INPUT                                         
019500       IF WS-KDPRODSL NUMERIC                                             
019600          MOVE WS-KDPRODSL TO W-1121-KDPRODSL                             
019700          PERFORM IMS-GET-KDPRODSL-ROT                                    
019800          IF SEGMENT-FINNS                                                
019900             IF MFS-UPDATE                                                
020000               PERFORM B-KOLLA-INDATA                                     
020100               IF SW-INDATA-OK = JA                                       
020200                 PERFORM C-UPPDATERA                                      
020300               ELSE                                                       
020400                 PERFORM S2-ROER-EJ-FAELT-IN                              
020500                 MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                    
020600               END-IF                                                     
020700               PERFORM F-LAES-SAMMA                                       
020800             ELSE                                                         
020900               IF MFS-IDPFK = '7'                                         
021000                 PERFORM D-LAES-FOERSTA                                   
021100               ELSE                                                       
021200                 IF MFS-IDPFK = '8'                                       
021300                   PERFORM E-LAES-NAESTA                                  
021400                 ELSE                                                     
021500                   PERFORM F-LAES-SAMMA                                   
021600                 END-IF                                                   
021700               END-IF                                                     
021800             PERFORM S3-RENSA-FAELT-MOD                                   
021900             END-IF                                                       
022000             PERFORM G-LAES-VISA-INFO                                     
022100          ELSE                                                            
022200             MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                        
022300             PERFORM S3-RENSA-FAELT-MOD                                   
022400          END-IF                                                          
022500       ELSE                                                               
022600          MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                           
022700          PERFORM S3-RENSA-FAELT-MOD                                      
022800       END-IF                                                             
022900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
023000       PERFORM IMS-INSERT-MSG                                             
023100     END-IF                                                               
023200                                                                          
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK.                                                              
023500     EJECT                                                                
023600 A-INIT-SPARA-INPUT SECTION.                                              
023700                                                                          
023800     IF MSG-DUBBLA-TRANSKODER                                             
023900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I15101                 
024000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
024100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
024300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
024400     ELSE                                                                 
024500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I15101                  
024600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024800       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
024900     END-IF                                                               
025000                                                                          
025100     IF MFS-IDTRANS NOT = '1151'                                          
025200       MOVE SPACE TO MFS-KDTRTYP                                          
025300       MOVE '7' TO MFS-IDPFK                                              
025400     END-IF                                                               
025500                                                                          
025600     IF MID-KDPRODSL-IN = ALL '+'                                         
025700        INSPECT MID-KDPRODSL-UT REPLACING LEADING SPACE BY ZERO           
025800        MOVE MID-KDPRODSL-UT TO WS-KDPRODSL                               
025900     ELSE                                                                 
026000        MOVE MID-KDPRODSL-IN TO WS-KDPRODSL                               
026100        MOVE ' '             TO MFS-KDTRTYP                               
026200        MOVE '7'             TO MFS-IDPFK                                 
026300     END-IF                                                               
026400                                                                          
026500     IF MFS-KDMFSFOR = '2'                                                
026600       MOVE +2 TO SPRAK-IX                                                
026700     ELSE                                                                 
026800       MOVE +1 TO SPRAK-IX                                                
026900     END-IF                                                               
027000                                                                          
027100     MOVE LOW-VALUE TO MSG-AREA                                           
027200     MOVE 'W1O151N1' TO MFS-IDMOD                                         
027300     MOVE '1151' TO MOD-IDTRANS                                           
027400     INSPECT WS-KDPRODSL REPLACING LEADING SPACE BY ZERO                  
027500     MOVE WS-KDPRODSL TO MOD-KDPRODSL-UT                                  
027600                                                                          
027700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027800                             MOD-KDPRODSL-IN                              
027900     .                                                                    
028000     EJECT                                                                
028100                                                                          
028200                                                                          
028300 B-KOLLA-INDATA SECTION.                                                  
028400                                                                          
028500     MOVE JA TO SW-INDATA-OK                                              
028600                                                                          
028700     IF MID-KDCMD = ALL '+'                                               
028800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                   
028900           MOVE NEJ                TO SW-INDATA-OK                        
029000     ELSE                                                                 
029100        IF MID-KDCMD-DELETE OR MID-KDCMD-INSERT OR                        
029200                               MID-KDCMD-REPLACE                          
029300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
029400        ELSE                                                              
029500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                   
029600           MOVE NEJ                TO SW-INDATA-OK                        
029700        END-IF                                                            
029800     END-IF                                                               
029900                                                                          
030000     IF MID-KDBASLM = ALL '+'                                             
030100        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-IN-ATTR                    
030200        MOVE NEJ                TO SW-INDATA-OK                           
030300     ELSE                                                                 
030400        IF MID-KDBASLM NUMERIC                                            
030500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-IN-ATTR                 
030600           MOVE NEJ                TO SW-INDATA-OK                        
030700        ELSE                                                              
030800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBASLM-IN-ATTR               
030900        END-IF                                                            
031000     END-IF                                                               
031100                                                                          
031200     EVALUATE TRUE                                                        
031300        WHEN MID-KDCMD-DELETE                                             
031400           CONTINUE                                                       
031500        WHEN OTHER                                                        
031600           PERFORM BA-KOLLA-DISTNR                                        
031700     END-EVALUATE                                                         
031800                                                                          
031900     .                                                                    
032000     EJECT                                                                
032100 BA-KOLLA-DISTNR SECTION.                                                 
032200     MOVE +1 TO INDX                                                      
032300                                                                          
032400     PERFORM UNTIL INDX > 6                                               
032500                                                                          
032600       IF INDX = +1                                                       
032700          IF MID-IDDISTR(INDX) = ALL '+'                                  
032800             MOVE MFS-NUM-FAELT-FEL TO                                    
032900                             MOD-IDDISTR-ATTR(INDX)                       
033000             MOVE NEJ               TO SW-INDATA-OK                       
033100          ELSE                                                            
033200             IF MID-IDDISTR(INDX) NOT NUMERIC                             
033300                MOVE MFS-NUM-FAELT-FEL TO                                 
033400                             MOD-IDDISTR-ATTR(INDX)                       
033500                MOVE NEJ               TO SW-INDATA-OK                    
033600             ELSE                                                         
033700                IF MID-IDDISTR(INDX) = ZERO                               
033800                   MOVE MFS-NUM-FAELT-FEL TO                              
033900                                MOD-IDDISTR-ATTR(INDX)                    
034000                   MOVE NEJ               TO SW-INDATA-OK                 
034100                ELSE                                                      
034200                   MOVE MFS-NUM-FAELT-RAETT TO                            
034300                                MOD-IDDISTR-ATTR(INDX)                    
034400                END-IF                                                    
034500             END-IF                                                       
034600          END-IF                                                          
034700       ELSE                                                               
034800          IF MID-IDDISTR(INDX) = ALL '+'                                  
034900             MOVE MFS-NUM-FAELT-RAETT TO                                  
035000                             MOD-IDDISTR-ATTR(INDX)                       
035100          ELSE                                                            
035200             IF MID-IDDISTR(INDX) NOT NUMERIC                             
035300                MOVE MFS-NUM-FAELT-FEL TO                                 
035400                             MOD-IDDISTR-ATTR(INDX)                       
035500                MOVE NEJ               TO SW-INDATA-OK                    
035600             ELSE                                                         
035700                IF MID-IDDISTR(INDX) = ZERO                               
035800                   MOVE MFS-NUM-FAELT-FEL TO                              
035900                                MOD-IDDISTR-ATTR(INDX)                    
036000                   MOVE NEJ               TO SW-INDATA-OK                 
036100                ELSE                                                      
036200                   MOVE MFS-NUM-FAELT-RAETT TO                            
036300                                MOD-IDDISTR-ATTR(INDX)                    
036400                END-IF                                                    
036500             END-IF                                                       
036600          END-IF                                                          
036700       END-IF                                                             
036800       ADD +1                          TO INDX                            
036900     END-PERFORM                                                          
037000     .                                                                    
037100     EJECT                                                                
037200 C-UPPDATERA SECTION.                                                     
037300                                                                          
037400*******************************************************                   
037500*                                                                         
037600*  D,B = DELETE  MARKNAD UPPDATERAS I CA-UPPDATERA                        
037700*               (TAR BORT EN HEL MARKNAD)                                 
037800*                                                                         
037900*  R,Ä = REPLACE MARKNAD UPPDATERAS I CB-UPPDATERA                        
038000*               (ÄNDRAR DISTRIKT-STRUKTUREN FÖR EN                        
038100*                MARKNAD)                                                 
038200*                                                                         
038300*  I,N = INSERT  MARKNAD UPPDATERAS I CC-UPPDATERA                        
038400*              (LÄGGER UPP EN NY MARKNAD MED MINST                        
038500*               ETT DISTRNR)                                              
038600*                                                                         
038700********************************************************                  
038800                                                                          
038900     MOVE JA          TO SW-UPPDAT-OK                                     
039000     MOVE MID-KDBASLM TO W-1122-KDBASLM                                   
039100     PERFORM IMS-GET-MARKNAD-UNIK                                         
039200                                                                          
039300     EVALUATE TRUE                                                        
039400         WHEN MID-KDCMD-DELETE                                            
039500            PERFORM CA-DELETE                                             
039600         WHEN MID-KDCMD-REPLACE                                           
039700            PERFORM CB-REPLACE                                            
039800         WHEN OTHER                                                       
039900            PERFORM CC-INSERT                                             
040000     END-EVALUATE                                                         
040100                                                                          
040200     IF SW-UPPDAT-OK = JA                                                 
040300        PERFORM S3-RENSA-FAELT-MOD                                        
040400        PERFORM S4-FLYTTA-FORM-ATTR                                       
040500     ELSE                                                                 
040600        PERFORM S2-ROER-EJ-FAELT-IN                                       
040700     END-IF                                                               
040800                                                                          
040900                                                                          
041000     .                                                                    
041100     EJECT                                                                
041200 CA-DELETE SECTION.                                                       
041300                                                                          
041400     IF SEGMENT-FINNS                                                     
041500        PERFORM IMS-DLET-MARKNAD                                          
041600        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
041700     ELSE                                                                 
041800        MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                             
041900        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-IN-ATTR                    
042000        MOVE NEJ         TO SW-UPPDAT-OK                                  
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 CB-REPLACE   SECTION.                                                    
042500                                                                          
042600     IF SEGMENT-FINNS                                                     
042700        MOVE MID-KDBASLM  TO 1122-KDBASLM                                 
042800                             MOD-KDBASLM-ENTER                            
042900                                                                          
043000        MOVE +1 TO INDX                                                   
043100                                                                          
043200        PERFORM UNTIL INDX > 6                                            
043300          IF MID-IDDISTR(INDX) NUMERIC                                    
043400             MOVE MID-IDDISTR(INDX) TO 1122-IDDISTR(INDX)                 
043500          ELSE                                                            
043600             MOVE ZERO              TO 1122-IDDISTR(INDX)                 
043700          END-IF                                                          
043800          ADD +1 TO INDX                                                  
043900        END-PERFORM                                                       
044000                                                                          
044100        PERFORM IMS-REPL-MARKNAD                                          
044200        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
044300     ELSE                                                                 
044400        MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                             
044500        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-IN-ATTR                    
044600        MOVE NEJ         TO SW-UPPDAT-OK                                  
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 CC-INSERT    SECTION.                                                    
045100                                                                          
045200     IF SEGMENT-FINNS                                                     
045300        MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL                             
045400        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-IN-ATTR                    
045500        MOVE NEJ         TO SW-UPPDAT-OK                                  
045600     ELSE                                                                 
045700        INITIALIZE  1122-WDGX1122                                         
045800        MOVE MID-KDBASLM  TO 1122-KDBASLM                                 
045900                             MOD-KDBASLM-ENTER                            
046000        MOVE LOW-VALUE    TO 1122-LOWVALUE                                
046100                                                                          
046200        MOVE +1 TO INDX                                                   
046300                                                                          
046400        PERFORM UNTIL INDX > 6                                            
046500          IF MID-IDDISTR(INDX) NUMERIC                                    
046600             MOVE MID-IDDISTR(INDX) TO 1122-IDDISTR(INDX)                 
046700          ELSE                                                            
046800             MOVE ZERO              TO 1122-IDDISTR(INDX)                 
046900          END-IF                                                          
047000          ADD +1 TO INDX                                                  
047100        END-PERFORM                                                       
047200        PERFORM IMS-INSRT-MARKNAD                                         
047300        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 D-LAES-FOERSTA SECTION.                                                  
047800                                                                          
047900                                                                          
048000     PERFORM IMS-GET-MARKNAD-FOERSTA                                      
048100     MOVE 1122-KDBASLM     TO MOD-KDBASLM-ENTER                           
048200     MOVE MED-3 (SPRAK-IX) TO MOD-TEMFSFEL                                
048300     .                                                                    
048400     EJECT                                                                
048500 E-LAES-NAESTA SECTION.                                                   
048600                                                                          
048700     IF MID-KDBASLM-PF8 NOT = ZERO                                        
048800        MOVE MID-KDBASLM-PF8 TO W-1122-KDBASLM                            
048900        PERFORM IMS-GET-MARKNAD-UNIK                                      
049000       IF SEGMENT-SAKNAS                                                  
049100         PERFORM IMS-GET-MARKNAD-FOERSTA                                  
049200         MOVE MED-3 (SPRAK-IX) TO MOD-TEMFSFEL                            
049300       END-IF                                                             
049400       MOVE 1122-KDBASLM     TO MOD-KDBASLM-ENTER                         
049500     ELSE                                                                 
049600       PERFORM IMS-GET-MARKNAD-FOERSTA                                    
049700       MOVE 1122-KDBASLM     TO MOD-KDBASLM-ENTER                         
049800       MOVE MED-3 (SPRAK-IX) TO MOD-TEMFSFEL                              
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 F-LAES-SAMMA SECTION.                                                    
050300                                                                          
050400     PERFORM IMS-GET-KDPRODSL-ROT                                         
050500     IF MID-KDBASLM-ENTER NOT = ZERO                                      
050600        MOVE MID-KDBASLM-ENTER TO W-1122-KDBASLM                          
050700        PERFORM IMS-GET-MARKNAD-UNIK                                      
050800       IF SEGMENT-SAKNAS                                                  
050900         PERFORM IMS-GET-MARKNAD-FOERSTA                                  
051000         MOVE MED-3 (SPRAK-IX) TO MOD-TEMFSFEL                            
051100       END-IF                                                             
051200       MOVE 1122-KDBASLM     TO MOD-KDBASLM-ENTER                         
051300     ELSE                                                                 
051400       PERFORM IMS-GET-MARKNAD-FOERSTA                                    
051500       MOVE 1122-KDBASLM     TO MOD-KDBASLM-ENTER                         
051600       MOVE MED-3 (SPRAK-IX) TO MOD-TEMFSFEL                              
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 G-LAES-VISA-INFO SECTION.                                                
052100                                                                          
052200***********************************************                           
052300*                                                                         
052400*  HÄR BÖRJAR DEN NYA LÖSNINGEN                                           
052500*  LÖSNINGEN BYGGER PÅ 36 RADER OCH INGA KOLUMNER.                        
052600*                                                                         
052700***********************************************                           
052800                                                                          
052900                                                                          
053000     MOVE +1 TO DIST-INDX                                                 
053100     MOVE +1 TO RAD-INDX                                                  
053200                                                                          
053300     PERFORM UNTIL RAD-INDX > 36                                          
053400        IF SEGMENT-FINNS                                                  
053500           PERFORM UNTIL DIST-INDX > 6                                    
053600              IF 1122-IDDISTR(DIST-INDX) = ZERO                           
053700                 ADD +7          TO DIST-INDX                             
053800              ELSE                                                        
053900                 MOVE 1122-KDBASLM  TO                                    
054000                              MOD-KDBASLM(RAD-INDX)                       
054100                 MOVE 1122-IDDISTR(DIST-INDX) TO                          
054200                               MOD-IDDISTR(RAD-INDX)                      
054300                 IF RAD-INDX = 36                                         
054400                    ADD +7 TO DIST-INDX                                   
054500                    MOVE 1122-KDBASLM     TO MOD-KDBASLM-PF8              
054600                 END-IF                                                   
054700                 ADD  +1            TO RAD-INDX                           
054800                 ADD  +1            TO DIST-INDX                          
054900              END-IF                                                      
055000           END-PERFORM                                                    
055100           MOVE +1                     TO DIST-INDX                       
055200           PERFORM IMS-GET-MARKNAD-NEXT                                   
055300        ELSE                                                              
055400           MOVE MFS-RENSA-FAELT TO MOD-KDBASLM(RAD-INDX)                  
055500                                   MOD-IDDISTR(RAD-INDX)                  
055600           ADD +1               TO RAD-INDX                               
055700        END-IF                                                            
055800     END-PERFORM                                                          
055900     IF SEGMENT-FINNS                                                     
056000        MOVE MED-2 (SPRAK-IX)            TO MOD-TEMFSFEL                  
056100     ELSE                                                                 
056200        MOVE MED-4 (SPRAK-IX)            TO MOD-TEMFSFEL                  
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
057900 S2-ROER-EJ-FAELT-IN SECTION.                                             
058000     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                               
058100                               MOD-KDBASLM-IN                             
058200     MOVE +1                TO INDX                                       
058300                                                                          
058400     PERFORM UNTIL INDX > 6                                               
058500        MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-IN(INDX)                    
058600        ADD +1 TO INDX                                                    
058700     END-PERFORM                                                          
058800     .                                                                    
058900     EJECT                                                                
059000 S3-RENSA-FAELT-MOD SECTION.                                              
059100     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN                                 
059200                             MOD-KDBASLM-IN                               
059300     MOVE +1                TO INDX                                       
059400                                                                          
059500     PERFORM UNTIL INDX > 6                                               
059600        MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN(INDX)                      
059700        ADD +1 TO INDX                                                    
059800     END-PERFORM                                                          
059900     .                                                                    
060000     EJECT                                                                
060100 S4-FLYTTA-FORM-ATTR SECTION.                                             
060200     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-IN-ATTR                         
060300                                MOD-KDBASLM-IN-ATTR                       
060400     MOVE +1                TO INDX                                       
060500                                                                          
060600     PERFORM UNTIL INDX > 6                                               
060700        MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-ATTR(INDX)                 
060800        ADD +1 TO INDX                                                    
060900     END-PERFORM                                                          
061000     .                                                                    
061100     EJECT                                                                
061200* IMS SEKTIONER                                                           
061300     SKIP3                                                                
061400 IMS-GET-MSG SECTION.                                                     
061500                                                                          
061600     MOVE '  QC' TO GODK-STATUSKODER                                      
061700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061900     PERFORM IMS-STATUSKONTROLL                                           
062000     .                                                                    
062100     SKIP3                                                                
062200 IMS-INSERT-MSG SECTION.                                                  
062300                                                                          
062400     IF NOT ENGLISH-TEXT                                                  
062500       MOVE '0' TO MFS-KDHUVOMR                                           
062600     END-IF                                                               
062700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062800     MOVE SPACE TO GODK-STATUSKODER                                       
062900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
063000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500 IMS-GET-KDPRODSL-ROT SECTION.                                            
063600                                                                          
063700     STRING 'WLXXAO01(WDGXKEY  =' W-1121-KEY-X ')'                        
063800            DELIMITED BY SIZE INTO SSA1                                   
063900     MOVE '  GE' TO GODK-STATUSKODER                                      
064000     CALL CBLTDLI USING GHU XXAO-PCB DLI-IO-AREA SSA1                     
064100     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
064200     PERFORM IMS-STATUSKONTROLL                                           
064300     .                                                                    
064400     SKIP3                                                                
064500 IMS-GET-MARKNAD-UNIK SECTION.                                            
064600                                                                          
064700     STRING 'WLXXAO11(WDGXKEY  =' W-1122-KEY-X ')'                        
064800            DELIMITED BY SIZE INTO SSA1                                   
064900     MOVE '  GE' TO GODK-STATUSKODER                                      
065000     CALL CBLTDLI USING GHNP XXAO-PCB DLI-IO-AREA SSA1                    
065100     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400 IMS-GET-MARKNAD-NEXT SECTION.                                            
065500                                                                          
065600     MOVE 'WLXXAO11 ' TO SSA1                                             
065700     MOVE '  GE' TO GODK-STATUSKODER                                      
065800     CALL CBLTDLI USING GNP XXAO-PCB DLI-IO-AREA SSA1                     
065900     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     SKIP3                                                                
066300 IMS-GET-MARKNAD-FOERSTA SECTION.                                         
066400                                                                          
066500     MOVE 'WLXXAO11 ' TO SSA1                                             
066600     MOVE '  GE' TO GODK-STATUSKODER                                      
066700     CALL CBLTDLI USING GNP XXAO-PCB DLI-IO-AREA SSA1                     
066800     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
066900     PERFORM IMS-STATUSKONTROLL                                           
067000     .                                                                    
067100     EJECT                                                                
067200 IMS-REPL-MARKNAD SECTION.                                                
067300                                                                          
067400     MOVE '  ' TO GODK-STATUSKODER                                        
067500     CALL CBLTDLI USING REPL XXAO-PCB DLI-IO-AREA                         
067600     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
067700     PERFORM IMS-STATUSKONTROLL                                           
067800     .                                                                    
067900     SKIP3                                                                
068000 IMS-DLET-MARKNAD SECTION.                                                
068100                                                                          
068200     MOVE '  ' TO GODK-STATUSKODER                                        
068300     CALL CBLTDLI USING DLET XXAO-PCB DLI-IO-AREA                         
068400     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
068500     PERFORM IMS-STATUSKONTROLL                                           
068600     .                                                                    
068700     SKIP3                                                                
068800 IMS-INSRT-MARKNAD SECTION.                                               
068900     STRING 'WLXXAO01(WDGXKEY  =' W-1121-KEY-X ')'                        
069000            DELIMITED BY SIZE INTO SSA1                                   
069100     MOVE 'WLXXAO11 ' TO SSA2                                             
069200     MOVE '  ' TO GODK-STATUSKODER                                        
069300     CALL CBLTDLI USING ISRT XXAO-PCB DLI-IO-AREA SSA1 SSA2               
069400     MOVE XXAO-STATUS-CODE TO STATUS-WS                                   
069500     PERFORM IMS-STATUSKONTROLL                                           
069600     .                                                                    
069700     EJECT                                                                
069800 IMS-STATUSKONTROLL SECTION.                                              
069900                                                                          
070000     SET STATUS-IX TO 1                                                   
070100     SEARCH GODK-STATUS                                                   
070110       AT END                                                             
070120         CALL FELLOG                                                      
070200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
070300     END-SEARCH                                                           
070400     .                                                                    
