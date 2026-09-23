000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4052100.                                                
000400 AUTHOR.         TOMMY JOHANSSON.                                         
000500     DATE-WRITTEN.   NOV 1985.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        DETTA PROGRAM LÄGGER UT 'EJ FAKTURERADE ORDER'.                  
001100*        LÄSER DATABASERNA WDE6A1, WDE6B1 OCH WDE6.                       
001200*        NYCKLAR ÄR PERSONKOD, ORDERSTATUS, FRAKTKOD, DISTRIKT            
001300*        OCH KUNDNR. PERSONKOD OCH ORDERSTATUS ANVÄNDS VID                
001400*        LÄSNING. ÖVRIGA NYCKLAR TESTAS MOT INLÄSTA VÄRDEN FRÅN           
001500*        ROTSEGMENTET.                                                    
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T521                                              
002000*        MID:         W4I52101                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O52101                                            
002400*    SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77   PROGRAM-NAMN           VALUE 'W4052100'                             
003100                               PIC X(8).                                  
003200 77  JA                        PIC X(1)     VALUE 'J'.                    
003300 77  NEJ                       PIC X(1)     VALUE 'N'.                    
003400 77  YES                       PIC X(1)     VALUE 'Y'.                    
003500 77  W-NYSIDA                  PIC X(1)     VALUE '8'.                    
003600 77  INPUT-OK                  PIC X(1)     VALUE 'J'.                    
003700 77  SEGM-SKALL-SKRIVAS        PIC X(1)     VALUE 'N'.                    
003800 77  MINST-1-RAD-SKRIVEN       PIC X(1)     VALUE 'N'.                    
003900 77  KDPERSON-WS               PIC X(3)     VALUE SPACE.                  
004400 77  FLUTSKR-WS                PIC X(1)     VALUE SPACE.                  
004500 77  INDX                      PIC S9(9)    VALUE ZERO  COMP SYNC.        
004600 77  CL-INDEX                  PIC S9(9)    VALUE ZERO  COMP SYNC.        
004700 77  MAX-LINE                  PIC S9(9)    VALUE +14   COMP SYNC.        
004900 77  W-KDPERSON                PIC S9(3)    VALUE ZERO  COMP-3.           
005000 77  W-KDFRAKT                 PIC S9(3)    VALUE ZERO  COMP-3.           
005100 77  W-IDDISTR                 PIC S9(5)    VALUE ZERO  COMP-3.           
005200 77  W-IDKUNDNR                PIC S9(7)    VALUE ZERO  COMP-3.           
005300 77  W-IDDC                    PIC  X(2)    VALUE SPACE.                  
005400 77  WS-IDTRANS                PIC X(4).                                  
005500     88  WS-GODKAEND-BILD                    VALUE '4521'.                
005510     88  EGEN-MID                            VALUE '4521'.                
005600     SKIP3                                                                
005700 01  W-IDKUNDRF                PIC X(10).                                 
005800                                                                          
005900 01  FILLER  REDEFINES  W-IDKUNDRF.                                       
006000     03  W-IDKUNDRF-1-5        PIC 9(5).                                  
006100     03  FILLER                PIC 9(5).                                  
006200     EJECT                                                                
006300                                                                          
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  CBLTDLI               PIC X(8)     VALUE 'CBLTDLI '.             
006600     03  FELLOG                PIC X(8)     VALUE 'FELLOG  '.             
006700     03  W005INIT              PIC X(8)     VALUE 'W005INIT'.             
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007000*01 -COPY WMSGINIT                                                        
007100     EJECT                                                                
007200 01  NYCKLAR-TILL-DLI.                                                    
007300     03  W-WDE6A1KY-MIN-X.                                                
007400         05  W-IDDISTR-MIN-A1  PIC S9(5)    VALUE ZERO  COMP-3.           
007500         05  W-IDDC-MIN-A1     PIC  X(2)    VALUE SPACE.                  
007600         05  W-KDFRAKT-MIN-A1  PIC S9(3)    VALUE ZERO  COMP-3.           
007700         05  W-KDORDSTA-MIN-A1 PIC S9(1)    VALUE ZERO  COMP-3.           
007800         05  W-IDKUNDNR-MIN-A1 PIC S9(7)    VALUE ZERO  COMP-3.           
007900         05  W-IDPRODNR-MIN-A1 PIC S9(7)    COMP-3.                       
008000                                                                          
008100     03  W-WDE6A1KY-MAX-X.                                                
008200         05  W-IDDISTR-MAX-A1  PIC S9(5)    VALUE ZERO  COMP-3.           
008300         05  W-IDDC-MAX-A1     PIC  X(2)    VALUE SPACE.                  
008400         05  W-KDFRAKT-MAX-A1  PIC S9(3)    VALUE ZERO  COMP-3.           
008500         05  W-KDORDSTA-MAX-A1 PIC S9(1)    VALUE ZERO  COMP-3.           
008600         05  W-IDKUNDNR-MAX-A1 PIC S9(7)    VALUE ZERO  COMP-3.           
008700         05  W-IDPRODNR-MAX-A1 PIC S9(7)    COMP-3.                       
008800     SKIP3                                                                
008900     03  W-WDE6B1KY-MIN-X.                                                
009000         05  W-KDPERSON-MIN-B1 PIC S9(3)    VALUE ZERO  COMP-3.           
009100         05  W-DABEGPAC-MIN-B1 PIC  9(8)    VALUE ZERO.                   
009200         05  W-KDORDSTA-MIN-B1 PIC S9(1)    VALUE ZERO  COMP-3.           
009300         05  W-IDPRODNR-MIN-B1 PIC S9(7)    COMP-3.                       
009400                                                                          
009500     03  W-WDE6B1KY-MAX-X.                                                
009600         05  W-KDPERSON-MAX-B1 PIC S9(3)    VALUE ZERO  COMP-3.           
009700         05  W-DABEGPAC-MAX-B1 PIC  9(8)    VALUE ZERO.                   
009800         05  W-KDORDSTA-MAX-B1 PIC S9(1)    VALUE ZERO  COMP-3.           
009900         05  W-IDPRODNR-MAX-B1 PIC S9(7)    COMP-3.                       
010000                                                                          
010010   03    W-WDE4E1KY-MAX-X.                                                
010020     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
010030     05    FILLER                PIC X(19)   VALUE HIGH-VALUE.            
010040                                                                          
010050   03    W-WDE4E1KY-MIN-X.                                                
010060     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
010070     05    FILLER                PIC X(19)   VALUE LOW-VALUE.             
010080                                                                          
010090     EJECT                                                                
010100     03  W-IDPRODNR-X.                                                    
010200         05  W-IDPRODNR        PIC S9(7)    VALUE ZERO  COMP-3.           
010300                                                                          
010400     03  W-KDORDSTA-MIN-X.                                                
010500         05  W-KDORDSTA-MIN    PIC S9(1)    VALUE ZERO  COMP-3.           
010600                                                                          
010700     03  W-KDORDSTA-MAX-X.                                                
010800         05  W-KDORDSTA-MAX    PIC S9(1)    VALUE ZERO  COMP-3.           
010810                                                                          
010820     03  W-IDDC-B6-X.                                                     
010830         05 W-IDDC-B6                  PIC X(2).                          
010840                                                                          
010900     EJECT                                                                
011000                                                                          
011100 01  ANGIVNA-NYCKLAR             PIC S9.                                  
011200                                                                          
011300     88  NYCKEL-DIST                 VALUE +1.                            
011400     88  NYCKEL-FRAK-DIST            VALUE +2.                            
011500     88  NYCKEL-FRAK-DIST-KUND       VALUE +3.                            
011600     88  NYCKEL-PERS                 VALUE +4.                            
011700     88  NYCKEL-PERS-FRAK            VALUE +5.                            
011800     88  NYCKEL-PERS-DIST            VALUE +6.                            
011900     88  NYCKEL-PERS-FRAK-DIST       VALUE +7.                            
012000     88  NYCKEL-PERS-FRAK-DIST-KUND  VALUE +8.                            
012100     88  NYCKEL-PERS-DIST-KUND       VALUE +9.                            
012200                                                                          
012300 01  FEL-MEDDELANDEN.                                                     
012400                                                                          
012500     03  FEL1.                                                            
012600         05  FILLER              PIC X(40)   VALUE                        
012700             '749 FEL NYCKEL                         '.                   
012800         05  FILLER              PIC X(40)   VALUE                        
012900             '749 WRONG KEY                          '.                   
013000     03  FILLER REDEFINES FEL1.                                           
013100         05  FEL-1 OCCURS 2      PIC X(40).                               
013200     SKIP3                                                                
013300     03  FEL2.                                                            
013400         05  FILLER              PIC X(40)   VALUE                        
013500             'ORDER SAKNAS                           '.                   
013600         05  FILLER              PIC X(40)   VALUE                        
013700             'ORDER MISSING                          '.                   
013800     03  FILLER REDEFINES FEL2.                                           
013900         05  FEL-2 OCCURS 2      PIC X(40).                               
014000     EJECT                                                                
014100 01  MEDDELANDEN.                                                         
014200     03  MED1.                                                            
014300         05  FILLER              PIC X(40)   VALUE                        
014400             '    FLER RADER FINNS                   '.                   
014500         05  FILLER              PIC X(40)   VALUE                        
014600            '     MORE LINES                         '.                   
014700     03  FILLER REDEFINES MED1.                                           
014800         05  MED-1 OCCURS 2      PIC X(40).                               
014900     EJECT                                                                
015000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015100                                                                          
015200 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
015300     SKIP3                                                                
015400*01    MID -COPY W4I52101.                                                
015500     EJECT                                                                
015600*01    -COPY WMSGAREA                                                     
015700     EJECT                                                                
015800*  03    MOD -COPY W4O52101  -RED MSG-AREA.                               
015900     EJECT                                                                
016000*01    -COPY WMFSAREA                                                     
016100     EJECT                                                                
016200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016300                                                                          
016400 01    IMS-WS.                                                            
016500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
016600     SKIP3                                                                
016700*                        **** STATUS-KOD FRÅN IMS                         
016800   03    STATUS-WS               PIC XX.                                  
016900     88    SEGMENT-FINNS                    VALUE '  '.                   
017000     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
017100     88    SEGMENT-SLUT                     VALUE 'GB'.                   
017200     SKIP3                                                                
017300   03    GODK-STATUSKODER.                                                
017400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
017500     SKIP3                                                                
017600 01    SSA1                      PIC X(96).                               
017700 01    SSA2                      PIC X(96).                               
017800     EJECT                                                                
017900*                            IMS FUNKTIONSKODER                           
018000*01    -COPY W0003                                                        
018100     EJECT                                                                
018200*                            DLI INPUT-OUTPUT AREA                        
018300 01    DLI-IO-AREA.                                                       
018400   03    IO-AREA                 PIC X(500)  VALUE SPACE.                 
018500     SKIP3                                                                
019000*  03    WDE601   -COPY WDE601   -RED IO-AREA.                            
019100     EJECT                                                                
019200*  03    WDE611   -COPY WDE611   -RED IO-AREA   -L.                       
019700     EJECT                                                                
019710 01  FILLER            PIC X(16) VALUE 'WDE6B1-AREA'.                     
019711 01  DLI-IO-E6B1.                                                         
019712*   03  -COPY WDE6B1  -PRE B1-                                            
019720     EJECT                                                                
019721 01  FILLER            PIC X(16) VALUE 'WDE6A1-AREA'.                     
019722 01  DLI-IO-E6A1.                                                         
019723*   03  -COPY WDE6A1                                                      
019724     EJECT                                                                
019730 01  FILLER            PIC X(16) VALUE 'WDE4E1-AREA'.                     
019741 01  -COPY WDE4E1                                                         
019742                                                                          
019743 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019744 01   DLI-IO-AREA-B601.                                                   
019745*     03  -COPY WDB601                                                    
019746                                                                          
019750     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900*01    -COPY W0009     -PRE MSG-                                          
020000     EJECT                                                                
020100*01    -COPY W0008     -PRE USEA-                                         
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400*01    -COPY W0008     -PRE WDE6A-                                        
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700*01    -COPY W0008     -PRE WDE6B-                                        
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000*01    -COPY W0008     -PRE WDE6-                                         
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021210*01    -COPY W0008     -PRE WDE4E-                                        
021220     05  FILLER                  PIC X.                                   
021230     EJECT                                                                
021240*01    -COPY W0008     -PRE WDB6-                                         
021250     05  FILLER                  PIC X.                                   
021260     EJECT                                                                
021300 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDE6A-PCB                      
021400                          WDE6B-PCB WDE6-PCB WDE4E-PCB                    
021410                          WDB6-PCB.                                       
021500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE6A-PCB                     
021600                           WDE6B-PCB WDE6-PCB WDE4E-PCB                   
021610                          WDB6-PCB.                                       
021700                                                                          
021800     PERFORM IMS-GET-MSG                                                  
021900                                                                          
022000     IF SEGMENT-FINNS                                                     
022100       PERFORM A-INIT-SPARA-INPUT                                         
022200                                                                          
022300       IF INPUT-OK = JA                                                   
022400         IF MFS-IDPFK = W-NYSIDA                                          
022500           PERFORM C-FLYTTA-SPARADE-NYCKLAR                               
022600         ELSE                                                             
022700           PERFORM B-FLYTTA-NYCKLAR                                       
022800         END-IF                                                           
022900                                                                          
023000         PERFORM D-LAES-FOERSTA-PRODNR                                    
023100                                                                          
023200         IF SEGMENT-FINNS                                                 
023300           IF MFS-IDPFK = W-NYSIDA                                        
023400             PERFORM B-FLYTTA-NYCKLAR                                     
023500           END-IF                                                         
023600                                                                          
023700           PERFORM E-LAES-FLYTTA-TILL-MOD                                 
023800                                                                          
023900         ELSE                                                             
024000           MOVE FEL-2 (CL-INDEX) TO MOD-TEMFSFEL                          
024100         END-IF                                                           
024200                                                                          
024300       ELSE                                                               
024400         MOVE FEL-1 (CL-INDEX) TO MOD-TEMFSFEL                            
024500                                                                          
024600         IF NOT WS-GODKAEND-BILD                                          
024700          PERFORM F-RENSA-NYCKLAR                                         
024800         END-IF                                                           
024900       END-IF                                                             
025000                                                                          
025100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O52101 + 4                      
025200       PERFORM IMS-INSERT-MSG                                             
025300     END-IF                                                               
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT-SPARA-INPUT SECTION.                                              
026000                                                                          
026100     IF MSG-DUBBLA-TRANSKODER                                             
026200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I52101               
026300         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
026400         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
026500     ELSE                                                                 
026600         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I52101                 
026700         MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                  
026800         MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                 
026900     END-IF                                                               
027000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
027100     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
027200                                                                          
027300     IF MFS-IDTRANS NOT = '4521'                                          
027400         MOVE ZERO     TO MID-KDPERSON-IN                                 
027900     END-IF                                                               
028000                                                                          
028100     MOVE LOW-VALUE TO MSG-AREA                                           
028200     MOVE 'W4O521N1' TO MFS-IDMOD                                         
028300     MOVE '4521' TO MOD-IDTRANS                                           
028400     MOVE ZERO TO MOD-SPARADE-NYCKLAR                                     
028500                                                                          
029300     MOVE MFS-RENSA-FAELT TO MOD-KDPERSON-IN                              
029400                             MOD-KDFRAKT-IN                               
029500                             MOD-IDDISTR-IN                               
029600                             MOD-IDKUNDNR-IN                              
029700                             MOD-IDDC-IN                                  
029800                             MOD-FLUTSKR-IN                               
029900                             MOD-TEMFSFEL                                 
030000                             MOD-TEMFSINF                                 
030100                                                                          
030200     IF MID-SPARADE-NYCKLAR = ALL '0'                                     
030300       MOVE SPACE TO MFS-IDPFK                                            
030400     END-IF                                                               
030500                                                                          
030600     PERFORM AA-KOLLA-NYCKLAR                                             
030700     .                                                                    
030800     EJECT                                                                
030900 AA-KOLLA-NYCKLAR SECTION.                                                
031000                                                                          
031100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
031200     MOVE '001'             TO MSGI-KDCALL                                
031300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031310     MOVE '4521'            TO MSGI-IDTRANS                               
031320     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
031400     IF EGEN-MID                                                          
031500        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
031600        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
031700        MOVE MID-KDFRAKT-IN  TO MSGI-KDFRAKT                              
031800     END-IF                                                               
031900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032100                                                                          
032110     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
032120         MOVE +1 TO CL-INDEX                                              
032130     ELSE                                                                 
032140         MOVE +2 TO CL-INDEX                                              
032160     END-IF                                                               
032170                                                                          
032200     MOVE JA TO INPUT-OK                                                  
032300                                                                          
032400     IF MID-KDPERSON-IN = ALL '+'                                         
032500         MOVE MID-KDPERSON-UT TO KDPERSON-WS                              
032600         INSPECT KDPERSON-WS REPLACING LEADING SPACE BY ZERO              
032700     ELSE                                                                 
032800         MOVE MID-KDPERSON-IN TO KDPERSON-WS                              
032900         MOVE SPACE TO MFS-IDPFK                                          
033000     END-IF                                                               
033100                                                                          
033200     IF MID-KDFRAKT-IN  NOT = ALL '+'                                     
033400         MOVE SPACE TO MFS-IDPFK                                          
033500     END-IF                                                               
033600                                                                          
033700     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
033900         MOVE SPACE TO MFS-IDPFK                                          
034000     END-IF                                                               
034100                                                                          
034200     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
034400         MOVE SPACE TO MFS-IDPFK                                          
034500     END-IF                                                               
034600                                                                          
034700     IF MID-IDDC-IN = ALL '+'                                             
034800         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
035000     ELSE                                                                 
035100         MOVE MID-IDDC-IN TO W-IDDC-B6                                    
035200         MOVE SPACE TO MFS-IDPFK                                          
035300     END-IF                                                               
035301     PERFORM IMS-GU-WDB601                                                
035302                                                                          
035310     IF DCS-KDDC = SPACE                                                  
035320        MOVE MSGI-IDDC      TO W-IDDC-B6                                  
035321        PERFORM IMS-GU-WDB601                                             
035330     END-IF                                                               
035340                                                                          
035500     IF MID-FLUTSKR-IN = ALL '+'                                          
035600         MOVE MID-FLUTSKR-UT TO FLUTSKR-WS                                
035700     ELSE                                                                 
035800         MOVE MID-FLUTSKR-IN TO FLUTSKR-WS                                
035900         MOVE SPACE TO MFS-IDPFK                                          
036000     END-IF                                                               
036100                                                                          
036200     IF FLUTSKR-WS = JA OR NEJ OR YES                                     
036300     CONTINUE                                                             
036400     ELSE                                                                 
036500         MOVE NEJ TO FLUTSKR-WS                                           
036600     END-IF                                                               
036700                                                                          
036800     MOVE KDPERSON-WS TO MOD-KDPERSON-UT                                  
036900     MOVE MSGI-KDFRAKT  TO MOD-KDFRAKT-UT                                 
037000     MOVE MSGI-IDDISTR  TO MOD-IDDISTR-UT                                 
037100     MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                                
037200     MOVE DCS-IDDC    TO MOD-IDDC-UT                                      
037300     MOVE FLUTSKR-WS  TO MOD-FLUTSKR-UT                                   
037400                                                                          
037500     INSPECT MOD-KDPERSON-UT REPLACING LEADING ZERO BY SPACE              
037600     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
037700     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
037800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
037900     INSPECT MOD-IDDC-UT     REPLACING LEADING ZERO BY SPACE              
038000                                                                          
038100     IF KDPERSON-WS NOT NUMERIC  OR                                       
038200        MSGI-KDFRAKT  NOT NUMERIC  OR                                     
038300        MSGI-IDDISTR  NOT NUMERIC  OR                                     
038400        MSGI-IDKUNDNR NOT NUMERIC  OR                                     
038500        DCS-KDDC = SPACE                                                  
038600       MOVE NEJ TO INPUT-OK                                               
038700     END-IF                                                               
038800                                                                          
038900     IF INPUT-OK = JA                                                     
039000       MOVE KDPERSON-WS     TO W-KDPERSON                                 
039100       MOVE MSGI-KDFRAKT    TO W-KDFRAKT                                  
039200       MOVE MSGI-IDDISTR    TO W-IDDISTR                                  
039300       MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR                                 
039400       MOVE DCS-IDDC        TO W-IDDC                                     
039500                                                                          
039600       PERFORM AAA-KOLLA-NYCKEL-KOMBINATION                               
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 AAA-KOLLA-NYCKEL-KOMBINATION SECTION.                                    
040100                                                                          
040200     EVALUATE TRUE                                                        
040300     WHEN W-KDPERSON  = ZERO  AND                                         
040400          W-KDFRAKT   = ZERO  AND                                         
040500          W-IDDISTR   > ZERO  AND                                         
040600          W-IDKUNDNR  = ZERO                                              
040700         MOVE +1 TO ANGIVNA-NYCKLAR                                       
040800                                                                          
040900     WHEN W-KDPERSON  = ZERO  AND                                         
041000          W-KDFRAKT   > ZERO  AND                                         
041100          W-IDDISTR   > ZERO  AND                                         
041200          W-IDKUNDNR  = ZERO                                              
041300         MOVE +2 TO ANGIVNA-NYCKLAR                                       
041400                                                                          
041500     WHEN W-KDPERSON  = ZERO  AND                                         
041600          W-KDFRAKT   > ZERO  AND                                         
041700          W-IDDISTR   > ZERO  AND                                         
041800          W-IDKUNDNR  > ZERO                                              
041900         MOVE +3 TO ANGIVNA-NYCKLAR                                       
042000                                                                          
042100     WHEN W-KDPERSON  > ZERO  AND                                         
042200          W-KDFRAKT   = ZERO  AND                                         
042300          W-IDDISTR   = ZERO  AND                                         
042400          W-IDKUNDNR  = ZERO                                              
042500         MOVE +4 TO ANGIVNA-NYCKLAR                                       
042600                                                                          
042700     WHEN W-KDPERSON  > ZERO  AND                                         
042800          W-KDFRAKT   > ZERO  AND                                         
042900          W-IDDISTR   = ZERO  AND                                         
043000          W-IDKUNDNR  = ZERO                                              
043100         MOVE +5 TO ANGIVNA-NYCKLAR                                       
043200                                                                          
043300     WHEN W-KDPERSON  > ZERO  AND                                         
043400          W-KDFRAKT   = ZERO  AND                                         
043500          W-IDDISTR   > ZERO  AND                                         
043600          W-IDKUNDNR  = ZERO                                              
043700         MOVE +6 TO ANGIVNA-NYCKLAR                                       
043800                                                                          
043900     WHEN W-KDPERSON  > ZERO  AND                                         
044000          W-KDFRAKT   > ZERO  AND                                         
044100          W-IDDISTR   > ZERO  AND                                         
044200          W-IDKUNDNR  = ZERO                                              
044300         MOVE +7 TO ANGIVNA-NYCKLAR                                       
044400                                                                          
044500     WHEN W-KDPERSON  > ZERO  AND                                         
044600          W-KDFRAKT   > ZERO  AND                                         
044700          W-IDDISTR   > ZERO  AND                                         
044800          W-IDKUNDNR  > ZERO                                              
044900         MOVE +8 TO ANGIVNA-NYCKLAR                                       
045000                                                                          
045100     WHEN W-KDPERSON  > ZERO  AND                                         
045200          W-KDFRAKT   = ZERO  AND                                         
045300          W-IDDISTR   > ZERO  AND                                         
045400          W-IDKUNDNR  > ZERO                                              
045500         MOVE +9 TO ANGIVNA-NYCKLAR                                       
045600     WHEN OTHER                                                           
045700       MOVE NEJ TO INPUT-OK                                               
045800     END-EVALUATE                                                         
045900     .                                                                    
046000     EJECT                                                                
046100 B-FLYTTA-NYCKLAR SECTION.                                                
046200                                                                          
046300     MOVE LOW-VALUE TO W-WDE6A1KY-MIN-X                                   
046400                       W-WDE6B1KY-MIN-X                                   
046500                                                                          
046600     MOVE HIGH-VALUE TO W-WDE6A1KY-MAX-X                                  
046700                        W-WDE6B1KY-MAX-X                                  
046800                                                                          
046900     IF W-KDPERSON  > ZERO                                                
047000       MOVE W-KDPERSON  TO W-KDPERSON-MIN-B1                              
047100                           W-KDPERSON-MAX-B1                              
047200     END-IF                                                               
047300                                                                          
047400     IF W-KDFRAKT  > ZERO                                                 
047500       MOVE W-KDFRAKT   TO W-KDFRAKT-MIN-A1                               
047600                           W-KDFRAKT-MAX-A1                               
047700     ELSE                                                                 
047800       MOVE ZERO        TO W-KDFRAKT-MIN-A1                               
047900       MOVE +999        TO W-KDFRAKT-MAX-A1                               
048000     END-IF                                                               
048100                                                                          
048200     IF W-IDDISTR  > ZERO                                                 
048300       MOVE W-IDDISTR   TO W-IDDISTR-MIN-A1                               
048400                           W-IDDISTR-MAX-A1                               
048500     ELSE                                                                 
048600       MOVE ZERO        TO W-IDDISTR-MIN-A1                               
048700       MOVE +99999      TO W-IDDISTR-MAX-A1                               
048800     END-IF                                                               
048900                                                                          
049000     IF W-IDKUNDNR  > ZERO                                                
049100       MOVE W-IDKUNDNR  TO W-IDKUNDNR-MIN-A1                              
049200                           W-IDKUNDNR-MAX-A1                              
049300     ELSE                                                                 
049400       MOVE ZERO        TO W-IDKUNDNR-MIN-A1                              
049500       MOVE +9999999    TO W-IDKUNDNR-MAX-A1                              
049600     END-IF                                                               
049700     MOVE W-IDDC        TO W-IDDC-MIN-A1                                  
049800                           W-IDDC-MAX-A1                                  
049900                                                                          
050000     MOVE 99999999 TO    W-DABEGPAC-MAX-B1                                
050100                                                                          
050200     IF FLUTSKR-WS = JA OR YES                                            
050300         MOVE +1 TO W-KDORDSTA-MIN-A1                                     
050400                    W-KDORDSTA-MIN-B1                                     
050500                    W-KDORDSTA-MAX-A1                                     
050600                    W-KDORDSTA-MAX-B1                                     
050700                    W-KDORDSTA-MIN                                        
050800                    W-KDORDSTA-MAX                                        
050900     ELSE                                                                 
051000         MOVE +1 TO W-KDORDSTA-MIN-A1                                     
051100                    W-KDORDSTA-MIN-B1                                     
051200                    W-KDORDSTA-MIN                                        
051300         MOVE +3 TO W-KDORDSTA-MAX-A1                                     
051400                    W-KDORDSTA-MAX-B1                                     
051500                    W-KDORDSTA-MAX                                        
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 C-FLYTTA-SPARADE-NYCKLAR SECTION.                                        
052000                                                                          
052100     IF W-KDPERSON = ZERO                                                 
052200       MOVE MID-IDDISTR-SPAR  TO W-IDDISTR-MIN-A1                         
052300       MOVE W-IDDC            TO W-IDDC-MIN-A1                            
052400       MOVE MID-KDFRAKT-SPAR  TO W-KDFRAKT-MIN-A1                         
052500       MOVE MID-KDORDSTA-SPAR TO W-KDORDSTA-MIN-A1                        
052600       MOVE MID-IDKUNDNR-SPAR TO W-IDKUNDNR-MIN-A1                        
052700       MOVE MID-IDPRODNR-SPAR TO W-IDPRODNR-MIN-A1                        
052800     ELSE                                                                 
052900       MOVE W-KDPERSON        TO W-KDPERSON-MIN-B1                        
053000       MOVE MID-TIBEGPAC-SPAR TO W-DABEGPAC-MIN-B1                        
053010       IF MID-TIBEGPAC-SPAR NOT = ZERO                                    
053020         IF MID-TIBEGPAC-SPAR < 500000                                    
053030           MOVE 20            TO W-DABEGPAC-MIN-B1 (1:2)                  
053040         ELSE                                                             
053050           IF MID-TIBEGPAC-SPAR < 999999                                  
053060             MOVE 19          TO W-DABEGPAC-MIN-B1 (1:2)                  
053070           ELSE                                                           
053080             MOVE 99999999    TO W-DABEGPAC-MIN-B1                        
053090           END-IF                                                         
053091         END-IF                                                           
053092       END-IF                                                             
053100       MOVE MID-KDORDSTA-SPAR TO W-KDORDSTA-MIN-B1                        
053200       MOVE MID-IDPRODNR-SPAR TO W-IDPRODNR-MIN-B1                        
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 D-LAES-FOERSTA-PRODNR SECTION.                                           
053700                                                                          
053800     IF MFS-IDPFK = W-NYSIDA                                              
053900       IF W-KDPERSON > ZERO                                               
054000         PERFORM IMS-GET-INDEX-PERSON                                     
054100       ELSE                                                               
054200         PERFORM IMS-GET-INDEX-DISTR                                      
054300       END-IF                                                             
054400                                                                          
054500     ELSE                                                                 
054600       IF W-KDPERSON > ZERO                                               
054700          PERFORM IMS-GET-INDEX-PERSON-B1-NEXT                            
054800       ELSE                                                               
054900          PERFORM IMS-GET-INDEX-DISTR-A1-NEXT                             
055000       END-IF                                                             
055100     END-IF                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 E-LAES-FLYTTA-TILL-MOD SECTION.                                          
055500                                                                          
055600     MOVE +1 TO INDX                                                      
055700                                                                          
055800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
055900                   (INDX NOT < MAX-LINE)                                  
056000         IF W-KDPERSON = ZERO                                             
056100           MOVE SEQA-IDPRODNR TO W-IDPRODNR                               
056200         ELSE                                                             
056300           MOVE B1-SEQB-IDPRODNR TO W-IDPRODNR                            
056400         END-IF                                                           
056500                                                                          
056600         PERFORM IMS-GET-ORDER                                            
056700         PERFORM EA-JMF-NYCKLAR-MED-SEGM                                  
056800                                                                          
056900         IF SEGM-SKALL-SKRIVAS = JA                                       
057000           PERFORM EB-FLYTTA-TILL-MOD                                     
057100           MOVE JA TO MINST-1-RAD-SKRIVEN                                 
057200           ADD +1 TO INDX                                                 
057300         END-IF                                                           
057400                                                                          
057500         IF W-KDPERSON = ZERO                                             
057600            PERFORM IMS-GET-INDEX-DISTR-A1-NEXT                           
057700         ELSE                                                             
057800            PERFORM IMS-GET-INDEX-PERSON-B1-NEXT                          
057900         END-IF                                                           
058000     END-PERFORM                                                          
058100                                                                          
058200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
058300       IF W-KDPERSON = ZERO                                               
058400         MOVE SEQA-IDPRODNR TO W-IDPRODNR                                 
058500       ELSE                                                               
058600         MOVE B1-SEQB-IDPRODNR TO W-IDPRODNR                              
058700       END-IF                                                             
058800       PERFORM IMS-GET-ORDER                                              
058900       IF SEGMENT-FINNS                                                   
059000         PERFORM EA-JMF-NYCKLAR-MED-SEGM                                  
059100                                                                          
059200         IF SEGM-SKALL-SKRIVAS = JA                                       
059300           MOVE VORD-KDORDSTA TO MOD-KDORDSTA-SPAR                        
059400           MOVE VORD-IDPRODNR TO MOD-IDPRODNR-SPAR                        
059500           MOVE VORD-DABEGPAC (3:6) TO MOD-TIBEGPAC-SPAR                  
059600           MOVE VORD-KDFRAKT  TO MOD-KDFRAKT-SPAR                         
059700           MOVE VORD-IDDISTR  TO MOD-IDDISTR-SPAR                         
059800           MOVE VORD-IDKUNDNR TO MOD-IDKUNDNR-SPAR                        
059900           MOVE MED-1 (CL-INDEX) TO MOD-TEMFSINF                          
060000           MOVE 'GE' TO STATUS-WS                                         
060100                                                                          
060200         ELSE                                                             
060300             IF W-KDPERSON = ZERO                                         
060400                PERFORM IMS-GET-INDEX-DISTR-A1-NEXT                       
060500             ELSE                                                         
060600                PERFORM IMS-GET-INDEX-PERSON-B1-NEXT                      
060700             END-IF                                                       
060800         END-IF                                                           
060900       END-IF                                                             
061000     END-PERFORM                                                          
061100                                                                          
061200     PERFORM UNTIL INDX NOT < MAX-LINE                                    
061300         MOVE MFS-RENSA-FAELT TO MOD-RAD(INDX)                            
061400         ADD +1 TO INDX                                                   
061500     END-PERFORM                                                          
061600                                                                          
061700     IF MINST-1-RAD-SKRIVEN = NEJ                                         
061800       MOVE FEL-2 (CL-INDEX) TO MOD-TEMFSFEL                              
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200 EA-JMF-NYCKLAR-MED-SEGM SECTION.                                         
062300                                                                          
062400     MOVE NEJ TO SEGM-SKALL-SKRIVAS                                       
062500                                                                          
062600     IF W-IDDC     = VORD-IDDC                                            
062700       EVALUATE TRUE                                                      
062800       WHEN NYCKEL-DIST                                                   
062900         IF W-IDDISTR       = VORD-IDDISTR                                
063000           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
063100         END-IF                                                           
063200                                                                          
063300       WHEN NYCKEL-FRAK-DIST                                              
063400         IF W-KDFRAKT   = VORD-KDFRAKT  AND                               
063500            W-IDDISTR   = VORD-IDDISTR                                    
063600           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
063700         END-IF                                                           
063800                                                                          
063900       WHEN NYCKEL-FRAK-DIST-KUND                                         
064000         IF W-KDFRAKT   = VORD-KDFRAKT  AND                               
064100            W-IDDISTR   = VORD-IDDISTR  AND                               
064200            W-IDKUNDNR  = VORD-IDKUNDNR                                   
064300           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
064400         END-IF                                                           
064500                                                                          
064600       WHEN NYCKEL-PERS                                                   
064700         IF W-KDPERSON  = VORD-KDPERSON                                   
064800           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
064900         END-IF                                                           
065000                                                                          
065100       WHEN NYCKEL-PERS-FRAK                                              
065200         IF W-KDPERSON  = VORD-KDPERSON  AND                              
065300            W-KDFRAKT   = VORD-KDFRAKT                                    
065400           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
065500         END-IF                                                           
065600                                                                          
065700       WHEN NYCKEL-PERS-DIST                                              
065800         IF W-KDPERSON  = VORD-KDPERSON  AND                              
065900            W-IDDISTR   = VORD-IDDISTR                                    
066000           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
066100         END-IF                                                           
066200                                                                          
066300       WHEN NYCKEL-PERS-FRAK-DIST                                         
066400         IF W-KDPERSON  = VORD-KDPERSON  AND                              
066500            W-KDFRAKT   = VORD-KDFRAKT   AND                              
066600            W-IDDISTR   = VORD-IDDISTR                                    
066700           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
066800         END-IF                                                           
066900                                                                          
067000       WHEN NYCKEL-PERS-FRAK-DIST-KUND                                    
067100         IF W-KDPERSON  = VORD-KDPERSON  AND                              
067200            W-KDFRAKT   = VORD-KDFRAKT   AND                              
067300            W-IDDISTR   = VORD-IDDISTR   AND                              
067400            W-IDKUNDNR  = VORD-IDKUNDNR                                   
067500           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
067600         END-IF                                                           
067700                                                                          
067800       WHEN NYCKEL-PERS-DIST-KUND                                         
067900         IF W-KDPERSON  = VORD-KDPERSON  AND                              
068000            W-IDDISTR   = VORD-IDDISTR   AND                              
068100            W-IDKUNDNR  = VORD-IDKUNDNR                                   
068200           MOVE JA TO SEGM-SKALL-SKRIVAS                                  
068300         END-IF                                                           
068400       END-EVALUATE                                                       
068500     END-IF                                                               
068600     .                                                                    
068700     EJECT                                                                
068800 EB-FLYTTA-TILL-MOD SECTION.                                              
068900                                                                          
069000     MOVE VORD-IDDISTR       TO MOD-IDDISTR (INDX)                        
069100     MOVE VORD-IDKUNDNR      TO MOD-IDKUNDNR (INDX)                       
069200     MOVE VORD-KDFRAKT       TO MOD-KDFRAKT (INDX)                        
069300     MOVE VORD-KDORDKL       TO MOD-KDORDKL (INDX)                        
069400     MOVE VORD-IDPRODNR      TO MOD-IDPRODNR (INDX)                       
069500     MOVE VORD-KVORDRAD      TO MOD-KVORDRAD (INDX)                       
069600     MOVE VORD-KVORDRAD-PACK TO MOD-KVORDRAD-PACK (INDX)                  
069700     MOVE VORD-KVKOLLI       TO MOD-KVKOLLI (INDX)                        
069800     MOVE VORD-DABEGPAC (3:6) TO MOD-TIBEGPAC (INDX)                      
069900     MOVE VORD-KVKOLLI-FAKT  TO MOD-KVKOLLI-FAKT (INDX)                   
070000     MOVE VORD-KVKOLLI-LAST  TO MOD-KVKOLLI-LAST (INDX)                   
070100                                                                          
070110     MOVE VORD-IDPRODNR      TO W-IDPRODNR-WDE4E-MIN                      
070120                                W-IDPRODNR-WDE4E-MAX                      
070210     PERFORM IMS-GU-WDE4E                                                 
070300                                                                          
070400     IF SEGMENT-FINNS                                                     
070500         MOVE SEQE-IDKUNDRF TO W-IDKUNDRF                                 
070600         MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR (INDX)                        
070700     ELSE                                                                 
070800         MOVE ZERO           TO MOD-IDORDNR (INDX)                        
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 F-RENSA-NYCKLAR SECTION.                                                 
071300     MOVE MFS-RENSA-FAELT              TO  MOD-KDPERSON-UT                
071400                                           MOD-KDFRAKT-UT                 
071500                                           MOD-IDDISTR-UT                 
071600                                           MOD-IDKUNDNR-UT                
071700                                           MOD-FLUTSKR-UT                 
071800     .                                                                    
071900     EJECT                                                                
072000* IMS SEKTIONER                                                           
072100                                                                          
072200 IMS-GET-MSG SECTION.                                                     
072300                                                                          
072400     MOVE '  QC' TO GODK-STATUSKODER                                      
072500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072700     PERFORM IMS-STATUSKONTROLL                                           
072800     .                                                                    
072900     SKIP3                                                                
073000 IMS-INSERT-MSG SECTION.                                                  
073100                                                                          
073101     IF  MSGI-IDLAND-SPR NOT = 'GB'                                       
073110         MOVE '0' TO MFS-KDHUVOMR                                         
073120     END-IF                                                               
073130                                                                          
073200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
073300     MOVE SPACE TO GODK-STATUSKODER                                       
073400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
073500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800     EJECT                                                                
073900 IMS-GET-INDEX-DISTR SECTION.                                             
074000                                                                          
074100     STRING 'WDE6A1  (WDE6A1KY =' W-WDE6A1KY-MIN-X ')'                    
074200            DELIMITED BY SIZE INTO SSA1                                   
074300     MOVE '  GE' TO GODK-STATUSKODER                                      
074400     CALL CBLTDLI USING GU WDE6A-PCB DLI-IO-E6A1 SSA1                     
074500     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     SKIP3                                                                
074900 IMS-GET-INDEX-DISTR-A1-NEXT SECTION.                                     
075000                                                                          
075100     STRING 'WDE6A1  (WDE6A1KY>=' W-WDE6A1KY-MIN-X                        
075200                    '&WDE6A1KY<=' W-WDE6A1KY-MAX-X                        
075300                    '&KDORDSTA>=' W-KDORDSTA-MIN-X                        
075400                    '&KDORDSTA<=' W-KDORDSTA-MAX-X ')'                    
075500            DELIMITED BY SIZE INTO SSA1                                   
075600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
075700     CALL CBLTDLI USING GN WDE6A-PCB DLI-IO-E6A1 SSA1                     
075800     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
075900     PERFORM IMS-STATUSKONTROLL                                           
076000     .                                                                    
076100     EJECT                                                                
076200 IMS-GET-INDEX-PERSON SECTION.                                            
076300                                                                          
076400     STRING 'WDE6B1  (WDE6B1KY =' W-WDE6B1KY-MIN-X ')'                    
076500            DELIMITED BY SIZE INTO SSA1                                   
076600     MOVE '  GE' TO GODK-STATUSKODER                                      
076700     CALL CBLTDLI USING GU WDE6B-PCB DLI-IO-E6B1 SSA1                     
076800     MOVE WDE6B-STATUS-CODE TO STATUS-WS                                  
076900     PERFORM IMS-STATUSKONTROLL                                           
077000     .                                                                    
077100     SKIP3                                                                
077200 IMS-GET-INDEX-PERSON-B1-NEXT SECTION.                                    
077300                                                                          
077400     STRING 'WDE6B1  (WDE6B1KY>=' W-WDE6B1KY-MIN-X                        
077500                    '&WDE6B1KY<=' W-WDE6B1KY-MAX-X                        
077600                    '&KDORDSTA>=' W-KDORDSTA-MIN-X                        
077700                    '&KDORDSTA<=' W-KDORDSTA-MAX-X ')'                    
077800            DELIMITED BY SIZE INTO SSA1                                   
077900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
078000     CALL CBLTDLI USING GN WDE6B-PCB DLI-IO-E6B1 SSA1                     
078100     MOVE WDE6B-STATUS-CODE TO STATUS-WS                                  
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400     EJECT                                                                
078500 IMS-GET-ORDER SECTION.                                                   
078600                                                                          
078700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
078800            DELIMITED BY SIZE INTO SSA1                                   
078900     MOVE '  GE' TO GODK-STATUSKODER                                      
079000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
079100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
079200     PERFORM IMS-STATUSKONTROLL                                           
079300     .                                                                    
079400     SKIP3                                                                
080301 IMS-GU-WDE4E SECTION.                                                    
080310     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
080320                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
080330          DELIMITED BY SIZE INTO SSA1                                     
080340     MOVE '  GE' TO GODK-STATUSKODER                                      
080350     CALL CBLTDLI USING GU WDE4E-PCB SEQE-WDE4E1 SSA1                     
080360     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
080370     PERFORM IMS-STATUSKONTROLL                                           
080380     SKIP2                                                                
080390     .                                                                    
080391 IMS-GU-WDB601    SECTION.                                                
080392     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
080393          DELIMITED BY SIZE INTO SSA1                                     
080394     MOVE '  GE' TO GODK-STATUSKODER                                      
080395     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
080396     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
080397     PERFORM IMS-STATUSKONTROLL                                           
080398     IF SEGMENT-SAKNAS                                                    
080399         MOVE SPACE TO DCS-KDDC                                           
080400     END-IF                                                               
080401     .                                                                    
080410 IMS-STATUSKONTROLL SECTION.                                              
080500                                                                          
080600     SET STATUS-IX TO 1                                                   
080700     SEARCH GODK-STATUS AT END CALL FELLOG                                
080800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
080900     END-SEARCH                                                           
081000     .                                                                    
