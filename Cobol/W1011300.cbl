000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1011300.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   JUNI 1987.                                               
000600     REMARKS.                                                             
000700*    FUNKTION.                                                            
000800*                ERSÄTTNINGSREGISTRERING.                                 
000900*                BYTE ERSÄTTNINGSKOD.                                     
001000*                UPPDATERING TILLKOMMANDE ARTIKLAR.                       
001100*                                                                         
001200*     ÄNDRING: I SAMBAND MED EVEREST-PROJEKTET (IDLEVNR) ÄNDRAS           
001300*              OCKSÅ SEG-NAMN OCH CTX-NAMN FÖR WLXXBT                     
001400*              GAMLA XXBT-TRANSEN  BYTER TILL 2304 MED HTYP-2303          
001500*                                                                         
001600*    ÄNDRING:                                                             
001700*        2005-FEB  ETRACKER=1476814.  VISA KAMPANJ-INFO                   
001800*                                     TILLAGT DB2-LÄSNING  /C.E.          
001900*                                                                         
002000*    ÄNDRING:                                                             
002100*        2014-FEB  ETRACKER=10249871                                      
002200*                  RESTRICTIONS ON PRODUCT-GROUP                          
002300*                                                                         
002400*    SKIP2                                                                
002500*    INDATA.                                                              
002600*    . . . . TRANSAKTION: W1T113                                          
002700*                         W1T113U                                         
002800*    . . . . . . . . MID: W1I11301                                        
002900*    UTDATA.                                                              
003000*    . . . . . . . . MOD: W1O11301                                        
003100*    DYNAMISKA SUBPROGRAM.                                                
003200*                         FELLOG                                          
003300*                         CBLTDLI                                         
003400*                         WDECEDIT                                        
003500*                         WDATKONV                                        
003600*                         W111ERSA                                        
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     SKIP3                                                                
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300*    -COPY WY2000W2                                                       
004400     SKIP3                                                                
004500*    -COPY WY2000W1                                                       
004600     SKIP3                                                                
004700*    -COPY WY2000W3                                                       
004800     SKIP3                                                                
004900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W1011300'.            
005000 77  JA                          PIC X(1)    VALUE 'J'.                   
005100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005200 77  SPRAK-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
005300 77  INPUT-RETT                  PIC X(1)    VALUE 'J'.                   
005400 77  WS-FELKOD                   PIC X(3)    VALUE '000'.                 
005500 77  WS-DIERS-TILLK              PIC 9(4)V9(3)  VALUE ZERO.               
005600 77  WS-IDKORTNR-SPAR1           PIC 9(3)    VALUE ZERO.                  
005700 77  WS-IDKORTNR-SPAR2           PIC 9(3)    VALUE ZERO.                  
005800 77  WS-IDKORTNR-SPAR3           PIC 9(3)    VALUE ZERO.                  
005900 77  MAX-RAD                     PIC S9(3)   VALUE +09  COMP-3.           
006000 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77  WS-CURRENT-SECTION          PIC X(64)   VALUE SPACE.                 
006200 77  WS-CURRENT-IMS-SECTION      PIC X(64)   VALUE SPACE.                 
006300                                                                          
006400*      --- VALID IDDC CODES                                               
006500*                                                                         
006600*01    -COPY WWDCKONS                                                     
006700*01    -COPY WWDC99                                                       
006800                                                                          
006900 01  WS-IDARTNR                             PIC X(9) VALUE ZERO.          
007000 01  IDARTNR-WS REDEFINES WS-IDARTNR        PIC 9(9).                     
007100                                                                          
007200 01  WS-JUST-RIGHT-GRP.                                                   
007300     03 WS-IDARTNR-R                        PIC X(9) VALUE SPACES         
007400                                            JUST RIGHT.                   
007500     03 WS-DIERS-R                          PIC X(7) VALUE SPACES         
007600                                            JUST RIGHT.                   
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800     03  WDECEDIT                PIC X(8)       VALUE 'WDECEDIT'.         
007900     03  WDATKONV                PIC X(8)       VALUE 'WDATKONV'.         
008000     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
008100     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
008200     03  W005INIT                PIC X(8)       VALUE 'W005INIT'.         
008300     03  W111ERSA                PIC X(8)       VALUE 'W111ERSA'.         
008400     EJECT                                                                
008500*01  -COPY WDECAREA                                                       
008600     EJECT                                                                
008700*                    ****   PARAMETRAR TILL W005INIT                      
008800*01  -COPY WMSGINIT                                                       
008900     EJECT                                                                
009000******************************************************************        
009100*                AREA TO COMMUNICATE WITH W111ERSA                        
009200*                                                                         
009300 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
009400 01  REQU-AREA.                                                           
009500     03 -COPY WZ01REQU                                                    
009600     03 -COPY W111ERIN                                                    
009700                                                                          
009800 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
009900 01  RESP-AREA.                                                           
010000     03 -COPY WZ01RESP                                                    
010100     03 -COPY W111ERUT                                                    
010200*****************************************************************         
010300*                                                                         
010400     EJECT                                                                
010500 01  MEDDELANDE.                                                          
010600                                                                          
010700     03  W-FEL-1.                                                         
010800         05  FILLER              PIC X(40)  VALUE                         
010900            'ARTIKELNUMMER EJ NUMERISKT              '.                   
011000         05  FILLER              PIC X(40)  VALUE                         
011100            'PARTNUMBER NOT NUMERIC                  '.                   
011200     03  FILLER REDEFINES W-FEL-1.                                        
011300         05  FEL-1               PIC X(40)  OCCURS 2.                     
011400                                                                          
011500     EJECT                                                                
011600*                        ****    MFS OCH SKÄRMHANTERING                   
011700 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
011800*01  MID -COPY W1I11301                                                   
011900     EJECT                                                                
012000*01  -COPY WMSGKOM                                                        
012100     EJECT                                                                
012200*01  -COPY WMSGAREA                                                       
012300     EJECT                                                                
012400*03  MOD -COPY W1O11301  -RED MSG-AREA.                                   
012500     EJECT                                                                
012600*01  -COPY WMFSAREA                                                       
012700     EJECT                                                                
012800******************************************************************        
012900*ERROR FIELDS FOR DISPACTHER ENTY FROM W11181600              **          
013000******************************************************************        
013100 01  MSG-KOM-MESSAGE-CODES.                                               
013200     03  FEL-ERR-IDARTNR         PIC X(3)    VALUE '768'.                 
013300*                                                                         
013400******************************************************************        
013500*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*****                                                                     
013700 01  IMS-WS.                                                              
013800     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
013900     SKIP3                                                                
014000*****                    **** STATUS-KOD FRÅN IMS                         
014100     03  STATUS-WS               PIC X(2).                                
014200         88  SEGMENT-FINNS                   VALUE '  '.                  
014300         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
014400         88  BASEN-SLUT                      VALUE 'GB'.                  
014500         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
014600     SKIP3                                                                
014700     03  GODK-STATUSKODER.                                                
014800         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
014900     SKIP3                                                                
015000*                            IMS FUNKTIONSKODER                           
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300                                                                          
015400 LINKAGE SECTION.                                                         
015500     SKIP2                                                                
015600*01  -COPY W0009     -PRE MSG-                                            
015700     EJECT                                                                
015800*01  -COPY W0009     -PRE MSGKOM-                                         
015900     EJECT                                                                
016000*01  -COPY W0008     -PRE USEA-                                           
016100         05  FILLER              PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008     -PRE ERSA-                                           
016400         05  FILLER              PIC X.                                   
016500     EJECT                                                                
016600*01  -COPY W0008     -PRE ERSB-                                           
016700         05  FILLER              PIC X.                                   
016800     EJECT                                                                
016900*01  -COPY W0008     -PRE ARTC-                                           
017000         05  FILLER              PIC X.                                   
017100     EJECT                                                                
017200*01  -COPY W0008     -PRE BENA-                                           
017300         05  FILLER              PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008     -PRE INLB-                                           
017600         05  FILLER              PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008     -PRE XXAN-                                           
017900         05  FILLER              PIC X.                                   
018000     EJECT                                                                
018100*01  -COPY W0008     -PRE 2303-                                           
018200         05  FILLER              PIC X.                                   
018300     EJECT                                                                
018400*01  -COPY W0008     -PRE XXBJ-                                           
018500         05  FILLER              PIC X.                                   
018600     EJECT                                                                
018700*01  -COPY W0008     -PRE XXID-                                           
018800         05  FILLER              PIC X.                                   
018900     EJECT                                                                
019000*01  -COPY W0008     -PRE ZZAC-                                           
019100         05  FILLER              PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008     -PRE ART2-                                           
019400         05  FILLER              PIC X.                                   
019500     EJECT                                                                
019600*01  -COPY W0008     -PRE XXAV-                                           
019700         05  FILLER              PIC X.                                   
019800     EJECT                                                                
019900*01  -COPY W0008     -PRE ARTG-                                           
020000         05  FILLER              PIC X.                                   
020100     EJECT                                                                
020200*01  -COPY W0008     -PRE XXAW-                                           
020300         05  FILLER              PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008     -PRE SATE-                                           
020600         05  FILLER              PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008     -PRE SATB-                                           
020900         05  FILLER              PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008     -PRE XXBW-                                           
021200         05  FILLER              PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008     -PRE XXCW-                                           
021500         05  FILLER              PIC X.                                   
021600     EJECT                                                                
021700*01  -COPY W0008     -PRE FILC-                                           
021800         05  FILLER              PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008     -PRE WDK7-                                           
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008     -PRE WDB6-                                           
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008     -PRE WDR2-                                           
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008     -PRE WDR5-                                           
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200 PROCEDURE DIVISION USING MSG-PCB MSGKOM-PCB USEA-PCB                     
023300                          ARTC-PCB ERSA-PCB ERSB-PCB                      
023400                          INLB-PCB BENA-PCB XXAN-PCB                      
023500                          2303-PCB XXBJ-PCB                               
023600                          XXID-PCB ZZAC-PCB ART2-PCB                      
023700                          XXAV-PCB ARTG-PCB XXAW-PCB                      
023800                          SATE-PCB SATB-PCB                               
023900                          XXBW-PCB XXCW-PCB                               
024000                          FILC-PCB WDK7-PCB WDB6-PCB                      
024100                          WDR2-PCB WDR5-PCB.                              
024200                                                                          
024300     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB USEA-PCB                    
024400                           ARTC-PCB ERSA-PCB ERSB-PCB                     
024500                           INLB-PCB BENA-PCB XXAN-PCB                     
024600                           2303-PCB XXBJ-PCB                              
024700                           XXID-PCB ZZAC-PCB ART2-PCB                     
024800                           XXAV-PCB ARTG-PCB XXAW-PCB                     
024900                           SATE-PCB SATB-PCB                              
025000                           XXBW-PCB XXCW-PCB                              
025100                           FILC-PCB WDK7-PCB WDB6-PCB                     
025200                           WDR2-PCB WDR5-PCB.                             
025300     SKIP2                                                                
025400     PERFORM IMS-GET-MSG                                                  
025500     IF SEGMENT-FINNS                                                     
025600        PERFORM IMS-GET-WMSGKOM                                           
025700        PERFORM A-INIT                                                    
025800        IF WS-IDARTNR NUMERIC                                             
025900           PERFORM B-INIT-REQU                                            
026000           IF MFS-UPDATE OR MFS-UPD-X                                     
026100              SET REQU-UPDATE   TO TRUE                                   
026200           ELSE                                                           
026300              SET REQU-QUERY    TO TRUE                                   
026400           END-IF                                                         
026500           PERFORM F-CALL-BIZ-LOGIC-W111ERSA                              
026600        ELSE                                                              
026700           MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                           
026800           MOVE FEL-ERR-IDARTNR TO WS-FELKOD                              
026900           MOVE NEJ             TO INPUT-RETT                             
027000        END-IF                                                            
027100                                                                          
027200                                                                          
027300        IF (MFS-IDTRANS = '2129' AND MFS-KDMFSFOR = '3')                  
027400        OR (MFS-IDTRANS = '2459' AND MFS-KDMFSFOR = '3')                  
027500           CONTINUE                                                       
027600        ELSE                                                              
027700           MOVE IDARTNR-WS TO MOD-IDARTNR-UT                              
027800           INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE         
027900           IF MFS-UPD-X                                                   
028000              IF INPUT-RETT = JA  AND RESP-KDSVAR = SPACE                 
028100                MOVE '101'     TO MSG-KOM-IDMFSMED                        
028200              ELSE                                                        
028300                 MOVE WS-FELKOD TO MSG-KOM-IDMFSMED                       
028400                 MOVE '1'       TO MSG-KOM-KDSVAR                         
028500              END-IF                                                      
028600              PERFORM IMS-INSERT-WMSGKOM                                  
028700           ELSE                                                           
028800              COMPUTE MSG-KVLL = LENGTH OF MOD-W1O11301 + 4               
028900              PERFORM IMS-INSERT-MSG                                      
029000           END-IF                                                         
029100        END-IF                                                            
029200     END-IF                                                               
029300                                                                          
029400     MOVE ZERO TO RETURN-CODE                                             
029500     GOBACK                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 A-INIT SECTION.                                                          
029900                                                                          
030000     MOVE 'STA A-INIT'         TO WS-CURRENT-SECTION                      
030100                                                                          
030200*INITIALIZE REQU FIELDS                                                   
030300     INITIALIZE REQU-IDARTNR-KEY                                          
030400                REQU-IDDC                                                 
030500                REQU-IDKORTNR-SPAR1                                       
030600                REQU-IDKORTNR-SPAR2                                       
030700                REQU-IDKORTNR-SPAR3                                       
030800                REQU-DIERS-ERS                                            
030900                REQU-KDERS                                                
031000                REQU-IDAO                                                 
031100                REQU-TIERSDAT-PREL                                        
031200                REQU-TEARTNOT                                             
031300                REQU-FLKLAR                                               
031400                REQU-IDSPRAK                                              
031500                REQU-KDARBTYP-SEC-IDLEV                                   
031600                REQU-KDARTSYS                                             
031700     MOVE 99      TO REQU-KVRADER-MAX9                                    
031800     MOVE +1   TO RAD-IX                                                  
031900     PERFORM UNTIL RAD-IX > REQU-KVRADER-MAX9                             
032000        INITIALIZE REQU-RAD (RAD-IX)                                      
032100        ADD +1  TO RAD-IX                                                 
032200     END-PERFORM                                                          
032300*                                                                         
032400     IF MSG-DUBBLA-TRANSKODER                                             
032500         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11301               
032600         MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                               
032700         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
032800         MOVE MSG-KDTRTYP    TO MFS-KDTRTYP                               
032900     ELSE                                                                 
033000         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W1I11301                 
033100         MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                               
033200         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
033300         IF MSG-KDTRANS-1 = 'W1T113X'                                     
033400            MOVE MSG-KDTRTYP TO MFS-KDTRTYP                               
033500          ELSE                                                            
033600            MOVE ' '         TO MFS-KDTRTYP                               
033700         END-IF                                                           
033800     END-IF                                                               
033900                                                                          
034000     IF MID-IDKORTNR-SPAR1 NUMERIC                                        
034100        MOVE MID-IDKORTNR-SPAR1 TO WS-IDKORTNR-SPAR1                      
034200     ELSE                                                                 
034300        MOVE ZERO TO WS-IDKORTNR-SPAR1                                    
034400     END-IF                                                               
034500                                                                          
034600     IF MID-IDKORTNR-SPAR2 NUMERIC                                        
034700        MOVE MID-IDKORTNR-SPAR2 TO WS-IDKORTNR-SPAR2                      
034800     ELSE                                                                 
034900        MOVE ZERO TO WS-IDKORTNR-SPAR2                                    
035000     END-IF                                                               
035100                                                                          
035200     IF MID-IDKORTNR-SPAR3 NUMERIC                                        
035300        MOVE MID-IDKORTNR-SPAR3 TO WS-IDKORTNR-SPAR3                      
035400     ELSE                                                                 
035500        MOVE ZERO TO WS-IDKORTNR-SPAR3                                    
035600     END-IF                                                               
035700                                                                          
035800     IF MFS-UPD-X                                                         
035900        IF MID-IDARTNR-IN = ALL '+' OR SPACE                              
036000          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
036100        ELSE                                                              
036200          MOVE MID-IDARTNR-IN TO WS-IDARTNR                               
036300          MOVE '7' TO MFS-IDPFK                                           
036400          MOVE SPACE TO MFS-KDTRTYP                                       
036500        END-IF                                                            
036600        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
036700        MOVE WC-CDC-SE TO WS-IDDC                                         
036800     ELSE                                                                 
036900        MOVE ALL '+' TO MSGI-WMSGINIT                                     
037000        MOVE '001'             TO MSGI-KDCALL                             
037100        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
037200        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
037300        MOVE '1113'            TO MSGI-IDTRANS                            
037400        IF MFS-IDTRANS = '1113'                                           
037500        OR (MID-IDARTNR-IN NUMERIC                                        
037600        AND MID-IDARTNR-IN > ZERO)                                        
037700            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
037800        END-IF                                                            
037900        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
038000        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
038100        MOVE MSGI-IDDC    TO WS-IDDC                                      
038200        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
038300     END-IF                                                               
038400                                                                          
038500     MOVE MSGI-KDARBTYP-SEC-IDLEV                                         
038600                          TO REQU-KDARBTYP-SEC-IDLEV                      
038700                                                                          
038800     IF MID-IDARTNR-IN = ALL '+' OR SPACE                                 
038900        CONTINUE                                                          
039000     ELSE                                                                 
039100        MOVE SPACE TO MFS-KDTRTYP                                         
039200        MOVE ZERO TO WS-IDKORTNR-SPAR1                                    
039300                     WS-IDKORTNR-SPAR2                                    
039400                     WS-IDKORTNR-SPAR3                                    
039500     END-IF                                                               
039600                                                                          
039700     IF MFS-IDTRANS = '1113'                                              
039800        CONTINUE                                                          
039900     ELSE                                                                 
040000        IF (MFS-IDTRANS = '2129' AND MFS-KDMFSFOR = '3')                  
040100        OR (MFS-IDTRANS = '2459' AND MFS-KDMFSFOR = '3')                  
040200           MOVE 'U' TO MFS-KDTRTYP                                        
040300           MOVE ZERO TO WS-IDKORTNR-SPAR1                                 
040400                        WS-IDKORTNR-SPAR2                                 
040500                        WS-IDKORTNR-SPAR3                                 
040600        ELSE                                                              
040700           MOVE SPACE TO MFS-KDTRTYP                                      
040800           MOVE ZERO TO WS-IDKORTNR-SPAR1                                 
040900                        WS-IDKORTNR-SPAR2                                 
041000                        WS-IDKORTNR-SPAR3                                 
041100        END-IF                                                            
041200     END-IF                                                               
041300                                                                          
041400     MOVE LOW-VALUE TO MOD-W1O11301                                       
041500     MOVE 'W1O113N1' TO MFS-IDMOD                                         
041600     MOVE '1113' TO MOD-IDTRANS                                           
041700                                                                          
041800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
041900                             MOD-TEMFSINF                                 
042000                             MOD-IDARTNR-IN                               
042100                                                                          
042200     IF MSGI-IDLAND-SPR = 'GB'                                            
042300       MOVE +2    TO SPRAK-IX                                             
042400       MOVE 'EN'  TO REQU-IDSPRAK                                         
042500     ELSE                                                                 
042600       MOVE +1    TO SPRAK-IX                                             
042700       MOVE 'SV'  TO REQU-IDSPRAK                                         
042800     END-IF                                                               
042900                                                                          
043000     .                                                                    
043100     EJECT                                                                
043200 B-INIT-REQU SECTION.                                                     
043300                                                                          
043400     MOVE 'STA B-INIT'             TO WS-CURRENT-SECTION                  
043500                                                                          
043600     MOVE MSG-SIGNON-USERID        TO REQU-IDUSER                         
043700/* TO INDICATE THAT THE MSG IS FROM CLASSIC                               
043800     MOVE '101'                    TO REQU-IDMSGVER                       
043900/*                                                                        
044000     MOVE IDARTNR-WS               TO REQU-IDARTNR-KEY                    
044100/* MAYBE CAN BE USED FA-SECTION INSTEAD OF MSGI-IDDC?                     
044200/* HOW IT SHOULD BE POPULATED FOR TCPLM                                   
044300     MOVE WS-IDDC                  TO REQU-IDDC                           
044400/*                                                                        
044500     MOVE WS-IDKORTNR-SPAR1        TO REQU-IDKORTNR-SPAR1                 
044600     MOVE WS-IDKORTNR-SPAR2        TO REQU-IDKORTNR-SPAR2                 
044700     MOVE WS-IDKORTNR-SPAR3        TO REQU-IDKORTNR-SPAR3                 
044800     MOVE MID-DIERS-ERS            TO REQU-DIERS-ERS                      
044900     MOVE MID-KDERS                TO REQU-KDERS                          
045000     MOVE MID-IDAO                 TO REQU-IDAO                           
045100     MOVE MID-TIERSDAT-PREL        TO REQU-TIERSDAT-PREL                  
045200                                                                          
045300*ROW COUNT WILL BE 9 FOR CLASSIC                                          
045400     MOVE MAX-RAD                  TO REQU-KVRADER-MAX9                   
045500*                                                                         
045600     MOVE +1   TO RAD-IX                                                  
045700     PERFORM UNTIL RAD-IX > MAX-RAD                                       
045800        MOVE MID-IDKORTNR (RAD-IX) TO REQU-IDKORTNR (RAD-IX)              
045900        MOVE MID-FLTEXT (RAD-IX)   TO REQU-FLTEXT (RAD-IX)                
046000        MOVE FUNCTION TRIM(MID-IDARTNR-TILLK (RAD-IX))                    
046100                                   TO WS-IDARTNR-R                        
046200        MOVE WS-IDARTNR-R          TO REQU-IDARTNR-TILLK (RAD-IX)         
046300        MOVE FUNCTION TRIM(MID-DIERS-TILLK (RAD-IX))                      
046400                                   TO WS-DIERS-R                          
046500        MOVE WS-DIERS-R            TO REQU-DIERS-TILLK (RAD-IX)           
046600        MOVE MID-BEERS (RAD-IX)    TO REQU-BEERS (RAD-IX)                 
046700       ADD +1  TO RAD-IX                                                  
046800     END-PERFORM                                                          
046900                                                                          
047000     MOVE MID-TEARTNOT             TO REQU-TEARTNOT                       
047100     MOVE MID-FLKLAR               TO REQU-FLKLAR                         
047200     MOVE SPACE                    TO REQU-KDARTSYS                       
047300     .                                                                    
047400     EJECT                                                                
047500 F-CALL-BIZ-LOGIC-W111ERSA    SECTION.                                    
047600                                                                          
047700     MOVE 'STA F-CALL'        TO WS-CURRENT-SECTION                       
047800                                                                          
047900     CALL W111ERSA USING                                                  
048000          REQU-AREA RESP-AREA                                             
048100          ARTC-PCB ERSA-PCB ERSB-PCB                                      
048200          INLB-PCB BENA-PCB XXAN-PCB                                      
048300          2303-PCB XXBJ-PCB                                               
048400          XXID-PCB ZZAC-PCB ART2-PCB                                      
048500          XXAV-PCB ARTG-PCB XXAW-PCB                                      
048600          SATE-PCB SATB-PCB                                               
048700          XXBW-PCB XXCW-PCB                                               
048800          FILC-PCB WDK7-PCB WDB6-PCB                                      
048900          WDR2-PCB WDR5-PCB                                               
049000                                                                          
049100     PERFORM FA-SET-MSG-AND-HILIGHT                                       
049200     PERFORM FB-MOVE-RESP-TO-MOD                                          
049300     .                                                                    
049400     EJECT                                                                
049500                                                                          
049600 FA-SET-MSG-AND-HILIGHT       SECTION.                                    
049700     MOVE 'STA FA-SET'              TO WS-CURRENT-SECTION                 
049800                                                                          
049900     MOVE RESP-DIERS-ERS-ATTR       TO MOD-DIERS-ERS-ATTR                 
050000     MOVE RESP-KDERS-ATTR           TO MOD-KDERS-ATTR                     
050100     MOVE RESP-IDAO-ATTR            TO MOD-IDAO-ATTR                      
050200     MOVE RESP-TIERSDAT-PREL-ATTR   TO MOD-TIERSDAT-PREL-ATTR             
050300                                                                          
050400     MOVE +1   TO RAD-IX                                                  
050500     PERFORM UNTIL RAD-IX > MAX-RAD                                       
050600        MOVE RESP-IDKORTNR-ATTR (RAD-IX)                                  
050700                                    TO MOD-IDKORTNR-ATTR (RAD-IX)         
050800        MOVE RESP-IDARTNR-TILLK-ATTR (RAD-IX)                             
050900                                 TO MOD-IDARTNR-TILLK-ATTR(RAD-IX)        
050800        MOVE RESP-DIERS-TILLK-ATTR (RAD-IX)                               
050900                                 TO MOD-DIERS-TILLK-ATTR(RAD-IX)          
051000        MOVE RESP-BEERS-ATTR (RAD-IX)                                     
051100                                    TO MOD-BEERS-ATTR (RAD-IX)            
051200       ADD +1  TO RAD-IX                                                  
051300     END-PERFORM                                                          
051400     MOVE RESP-TEARTNOT-ATTR        TO MOD-TEARTNOT-ATTR                  
051500     MOVE RESP-FLKLAR-ATTR          TO MOD-FLKLAR-ATTR                    
051600                                                                          
051700     MOVE RESP-TEMFSFEL             TO MOD-TEMFSFEL                       
051800     MOVE RESP-TEMFSINF             TO MOD-TEMFSINF                       
051900     MOVE RESP-IDMSG-ERROR          TO WS-FELKOD                          
052000     .                                                                    
052100 FB-MOVE-RESP-TO-MOD          SECTION.                                    
052200     MOVE 'STA FB-MOVE'             TO WS-CURRENT-SECTION                 
052300                                                                          
052400     IF RESP-IDKORTNR-SPAR1 = ALL '+'                                     
052500        MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKORTNR-SPAR1                 
052600     ELSE                                                                 
052700       IF RESP-IDKORTNR-SPAR1 = SPACES                                    
052800          MOVE MFS-RENSA-FAELT      TO MOD-IDKORTNR-SPAR1                 
052900       ELSE                                                               
053000          MOVE RESP-IDKORTNR-SPAR1  TO MOD-IDKORTNR-SPAR1                 
053100       END-IF                                                             
053200     END-IF                                                               
053300     IF RESP-IDKORTNR-SPAR2 = ALL '+'                                     
053400        MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKORTNR-SPAR2                 
053500     ELSE                                                                 
053600       IF RESP-IDKORTNR-SPAR2 = SPACES                                    
053700          MOVE MFS-RENSA-FAELT      TO MOD-IDKORTNR-SPAR2                 
053800       ELSE                                                               
053900          MOVE RESP-IDKORTNR-SPAR2  TO MOD-IDKORTNR-SPAR2                 
054000       END-IF                                                             
054100     END-IF                                                               
054200     IF RESP-IDKORTNR-SPAR3 = ALL '+'                                     
054300        MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKORTNR-SPAR3                 
054400     ELSE                                                                 
054500       IF RESP-IDKORTNR-SPAR3 = SPACES                                    
054600          MOVE MFS-RENSA-FAELT      TO MOD-IDKORTNR-SPAR3                 
054700       ELSE                                                               
054800          MOVE RESP-IDKORTNR-SPAR3  TO MOD-IDKORTNR-SPAR3                 
054900       END-IF                                                             
055000     END-IF                                                               
055100     IF RESP-DIERS-ERS      = ALL '+'                                     
055200        MOVE MFS-ROER-EJ-FAELT      TO MOD-DIERS-ERS                      
055300     ELSE                                                                 
055400       IF RESP-DIERS-ERS      = SPACES                                    
055500          MOVE MFS-RENSA-FAELT      TO MOD-DIERS-ERS                      
055600       ELSE                                                               
055700          MOVE RESP-DIERS-ERS       TO MOD-DIERS-ERS                      
055800       END-IF                                                             
055900     END-IF                                                               
056000     IF RESP-KDERS          = ALL '+'                                     
056100        MOVE MFS-ROER-EJ-FAELT      TO MOD-KDERS                          
056200     ELSE                                                                 
056300       IF RESP-KDERS          = SPACES                                    
056400          MOVE MFS-RENSA-FAELT      TO MOD-KDERS                          
056500       ELSE                                                               
056600          MOVE RESP-KDERS           TO MOD-KDERS                          
056700       END-IF                                                             
056800     END-IF                                                               
056900     IF RESP-IDAO           = ALL '+'                                     
057000        MOVE MFS-ROER-EJ-FAELT      TO MOD-IDAO                           
057100     ELSE                                                                 
057200       IF RESP-IDAO           = SPACES                                    
057300          MOVE MFS-RENSA-FAELT      TO MOD-IDAO                           
057400       ELSE                                                               
057500          MOVE RESP-IDAO            TO MOD-IDAO                           
057600       END-IF                                                             
057700     END-IF                                                               
057800     IF RESP-TIERSDAT-PREL  = ALL '+'                                     
057900        MOVE MFS-ROER-EJ-FAELT      TO MOD-TIERSDAT-PREL                  
058000     ELSE                                                                 
058100       IF RESP-TIERSDAT-PREL  = SPACES                                    
058200          MOVE MFS-RENSA-FAELT      TO MOD-TIERSDAT-PREL                  
058300       ELSE                                                               
058400          MOVE RESP-TIERSDAT-PREL   TO MOD-TIERSDAT-PREL                  
058500       END-IF                                                             
058600     END-IF                                                               
058700                                                                          
058800     MOVE +1   TO RAD-IX                                                  
058900     PERFORM UNTIL RAD-IX > MAX-RAD OR                                    
059000        RESP-IDKORTNR (RAD-IX)  = LOW-VALUE                               
059100        IF RESP-IDKORTNR (RAD-IX) = ALL '+'                               
059200           MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKORTNR (RAD-IX)              
059300        ELSE                                                              
059400         IF RESP-IDKORTNR (RAD-IX) = SPACES                               
059500          MOVE MFS-RENSA-FAELT      TO MOD-IDKORTNR (RAD-IX)              
059600         ELSE                                                             
059700           MOVE RESP-IDKORTNR (RAD-IX)                                    
059800                                    TO MOD-IDKORTNR (RAD-IX)              
059900         END-IF                                                           
060000        END-IF                                                            
060100        IF RESP-FLTEXT   (RAD-IX) = ALL '+'                               
060200           MOVE MFS-ROER-EJ-FAELT   TO MOD-FLTEXT   (RAD-IX)              
060300        ELSE                                                              
060400         IF RESP-FLTEXT   (RAD-IX) = SPACES                               
060500          MOVE MFS-RENSA-FAELT      TO MOD-FLTEXT   (RAD-IX)              
060600         ELSE                                                             
060700          MOVE RESP-FLTEXT   (RAD-IX)                                     
060800                                    TO MOD-FLTEXT   (RAD-IX)              
060900         END-IF                                                           
061000        END-IF                                                            
061100        IF RESP-IDARTNR-TILLK (RAD-IX) = ALL '+'                          
061200           MOVE MFS-ROER-EJ-FAELT   TO MOD-IDARTNR-TILLK (RAD-IX)         
061300        ELSE                                                              
061400         IF RESP-IDARTNR-TILLK (RAD-IX) = SPACES                          
061500          MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-TILLK (RAD-IX)         
061600         ELSE                                                             
061700          MOVE RESP-IDARTNR-TILLK (RAD-IX)                                
061800                                    TO MOD-IDARTNR-TILLK (RAD-IX)         
061900         END-IF                                                           
062000        END-IF                                                            
062100        IF RESP-DIERS-TILLK (RAD-IX)   = ALL '+'                          
062200           MOVE MFS-ROER-EJ-FAELT   TO MOD-DIERS-TILLK (RAD-IX)           
062300        ELSE                                                              
062400         IF RESP-DIERS-TILLK (RAD-IX)   = SPACES                          
062500          MOVE MFS-RENSA-FAELT      TO MOD-DIERS-TILLK (RAD-IX)           
062600         ELSE                                                             
062700          MOVE RESP-DIERS-TILLK (RAD-IX) TO DEC-IDFRIDATA                 
062800          MOVE 3                         TO DEC-KVHELTAL                  
062900          MOVE 3                         TO DEC-KVDECIMAL                 
063000          CALL WDECEDIT USING DEC-WDECAREA                                
063100          MOVE DEC-IDEDITDATA    TO                                       
063200                      WS-DIERS-TILLK                                      
063300          MOVE WS-DIERS-TILLK    TO                                       
063400                     MOD-DIERS-TILLK (RAD-IX)                             
063500         END-IF                                                           
063600        END-IF                                                            
063700        IF RESP-BEART (RAD-IX)         = ALL '+'                          
063800           MOVE MFS-ROER-EJ-FAELT   TO MOD-BEART (RAD-IX)                 
063900        ELSE                                                              
064000         IF RESP-BEART (RAD-IX)         = SPACES                          
064100          MOVE MFS-RENSA-FAELT      TO MOD-BEART (RAD-IX)                 
064200         ELSE                                                             
064300          MOVE RESP-BEART (RAD-IX)  TO MOD-BEART (RAD-IX)                 
064400         END-IF                                                           
064500        END-IF                                                            
064600        IF RESP-BEERS (RAD-IX)         = ALL '+'                          
064700           MOVE MFS-ROER-EJ-FAELT   TO MOD-BEERS (RAD-IX)                 
064800        ELSE                                                              
064900         IF RESP-BEERS (RAD-IX)         = SPACES                          
065000          MOVE MFS-RENSA-FAELT      TO MOD-BEERS (RAD-IX)                 
065100         ELSE                                                             
065200          MOVE RESP-BEERS (RAD-IX)  TO MOD-BEERS (RAD-IX)                 
065300         END-IF                                                           
065400        END-IF                                                            
065500       ADD +1  TO RAD-IX                                                  
065600     END-PERFORM                                                          
065700     IF RESP-TEARTNOT       = ALL '+'                                     
065800        MOVE MFS-ROER-EJ-FAELT      TO MOD-TEARTNOT                       
065900     ELSE                                                                 
066000       IF RESP-TEARTNOT       = SPACES                                    
066100          MOVE MFS-RENSA-FAELT      TO MOD-TEARTNOT                       
066200         ELSE                                                             
066300          MOVE RESP-TEARTNOT        TO MOD-TEARTNOT                       
066400       END-IF                                                             
066500     END-IF                                                               
066600     IF RESP-IDUSER         = ALL '+'                                     
066700        MOVE MFS-ROER-EJ-FAELT      TO MOD-IDUSER                         
066800     ELSE                                                                 
066900       IF RESP-IDUSER         = SPACES                                    
067000          MOVE MFS-RENSA-FAELT      TO MOD-IDUSER                         
067100         ELSE                                                             
067200          MOVE RESP-IDUSER          TO MOD-IDUSER                         
067300       END-IF                                                             
067400     END-IF                                                               
067500     IF RESP-FLKLAR         = ALL '+'                                     
067600        MOVE MFS-ROER-EJ-FAELT      TO MOD-FLKLAR                         
067700     ELSE                                                                 
067800       IF RESP-FLKLAR         = SPACES                                    
067900          MOVE MFS-RENSA-FAELT      TO MOD-FLKLAR                         
068000         ELSE                                                             
068100          MOVE RESP-FLKLAR          TO MOD-FLKLAR                         
068200       END-IF                                                             
068300     END-IF                                                               
068400     IF RESP-KDERS-C1       = ALL '+'                                     
068500        MOVE MFS-ROER-EJ-FAELT      TO MOD-KDERS-C1                       
068600     ELSE                                                                 
068700       IF RESP-KDERS-C1       = SPACES                                    
068800          MOVE MFS-RENSA-FAELT      TO MOD-KDERS-C1                       
068900       ELSE                                                               
069000          MOVE RESP-KDERS-C1        TO MOD-KDERS-C1                       
069100       END-IF                                                             
069200     END-IF                                                               
069300     IF RESP-KDERS-C2       = ALL '+'                                     
069400        MOVE MFS-ROER-EJ-FAELT      TO MOD-KDERS-C2                       
069500     ELSE                                                                 
069600       IF RESP-KDERS-C2       = SPACES                                    
069700          MOVE MFS-RENSA-FAELT      TO MOD-KDERS-C2                       
069800         ELSE                                                             
069900          MOVE RESP-KDERS-C2        TO MOD-KDERS-C2                       
070000       END-IF                                                             
070100     END-IF                                                               
070200     IF RESP-TIERSDAT-REG   = ALL '+'                                     
070300        MOVE MFS-ROER-EJ-FAELT      TO MOD-TIERSDAT-REG                   
070400     ELSE                                                                 
070500       IF RESP-TIERSDAT-REG   = SPACES                                    
070600          MOVE MFS-RENSA-FAELT      TO MOD-TIERSDAT-REG                   
070700       ELSE                                                               
070800          MOVE RESP-TIERSDAT-REG    TO MOD-TIERSDAT-REG                   
070900       END-IF                                                             
071000     END-IF                                                               
071100     IF RESP-TIERSDAT-PREL-C1 = ALL '+'                                   
071200        MOVE MFS-ROER-EJ-FAELT      TO MOD-TIERSDAT-PREL-C1               
071300     ELSE                                                                 
071400       IF RESP-TIERSDAT-PREL-C1 = SPACES                                  
071500          MOVE MFS-RENSA-FAELT      TO MOD-TIERSDAT-PREL-C1               
071600       ELSE                                                               
071700          MOVE RESP-TIERSDAT-PREL-C1                                      
071800                                    TO MOD-TIERSDAT-PREL-C1               
071900       END-IF                                                             
072000     END-IF                                                               
072100     IF RESP-TIERSDAT-PREL-C2 = ALL '+'                                   
072200        MOVE MFS-ROER-EJ-FAELT      TO MOD-TIERSDAT-PREL-C2               
072300     ELSE                                                                 
072400       IF RESP-TIERSDAT-PREL-C2 = SPACES                                  
072500          MOVE MFS-RENSA-FAELT      TO MOD-TIERSDAT-PREL-C2               
072600       ELSE                                                               
072700          MOVE RESP-TIERSDAT-PREL-C2                                      
072800                                    TO MOD-TIERSDAT-PREL-C2               
072900       END-IF                                                             
073000     END-IF                                                               
073100     IF RESP-TIERSDAT       = ALL '+'                                     
073200        MOVE MFS-ROER-EJ-FAELT      TO MOD-TIERSDAT                       
073300     ELSE                                                                 
073400       IF RESP-TIERSDAT       = SPACES                                    
073500          MOVE MFS-RENSA-FAELT      TO MOD-TIERSDAT                       
073600       ELSE                                                               
073700          MOVE RESP-TIERSDAT        TO MOD-TIERSDAT                       
073800       END-IF                                                             
073900     END-IF                                                               
074000     .                                                                    
074100                                                                          
074200*** IMS SEKTIONER                                                         
074300     SKIP3                                                                
074400 IMS-GET-MSG SECTION.                                                     
074500     MOVE 'IMS-GET-MSG         '    TO WS-CURRENT-IMS-SECTION             
074600                                                                          
074700     MOVE '  QC' TO GODK-STATUSKODER                                      
074800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
074900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075000     PERFORM IMS-STATUS-KONTROLL                                          
075100     .                                                                    
075200     SKIP3                                                                
075300 IMS-INSERT-MSG SECTION.                                                  
075400     MOVE 'IMS-INSERT-MSG      '    TO WS-CURRENT-IMS-SECTION             
075500                                                                          
075600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
075700       MOVE '0' TO MFS-KDHUVOMR                                           
075800     END-IF                                                               
075900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076000     MOVE SPACE TO GODK-STATUSKODER                                       
076100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076300     PERFORM IMS-STATUS-KONTROLL                                          
076400     .                                                                    
076500     EJECT                                                                
076600 IMS-GET-WMSGKOM SECTION.                                                 
076700     MOVE 'IMS-GET-WMSGKOM     '    TO WS-CURRENT-IMS-SECTION             
076800                                                                          
076900     MOVE '  QD' TO GODK-STATUSKODER                                      
077000     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
077100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077200     PERFORM IMS-STATUS-KONTROLL                                          
077300     .                                                                    
077400     SKIP3                                                                
077500 IMS-INSERT-WMSGKOM SECTION.                                              
077600     MOVE 'IMS-INSERT-WMSGKOM  '    TO WS-CURRENT-IMS-SECTION             
077700                                                                          
077800     MOVE '  ' TO GODK-STATUSKODER                                        
077900     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
078000     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
078100     PERFORM IMS-STATUS-KONTROLL                                          
078200     .                                                                    
078300 IMS-STATUS-KONTROLL SECTION.                                             
078400     SET STATUS-IX TO 1                                                   
078500     SEARCH GODK-STATUS AT END CALL FELLOG                                
078600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
078700     END-SEARCH.                                                          
078800     EJECT                                                                
