000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL016400.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   JUNI 2005.                                               
000600                                                                          
000700     REMARKS.                                                             
000800* WL016400 PROGRAM IS A REPLICA OF W4074800 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.BINNINGLISTSTATUS                           
001200*                                                                         
001300                                                                          
001400*    FUNKTION:                                                            
001500*        PROGRAMMET VISAR STATUS FÖR AKTUELLA INLÄGGNINGSLISTOR           
001600*        S.K. I-LISTOR. VEM SOM HAR SKAPAT EN LISTA OCH DATUM NÄR,        
001700*        ÄVEN VEM SOM HAR SKRIVIT UT EN LISTA OCH DATUM NÄR DETTA         
001800*        GJORDES.                                                         
001801*                                                                         
001802*        PGM WL015500 ANROPAS ISTÄLLET FÖR W4079500 FÖR PRINTNING.        
001803*                                                                         
001810*        NY FUNKTION E-TRACKER 829496 MÖJLIGHET ATT PRINTA FLERA          
001820*        I-LISTOR MED KANTKOD.                                            
001900*                                                                         
002000*        PROGRAMMET LÄSER      WDA2E                                      
002100*        PROGRAMMET LÄSER      WDK6                                       
002200*        PROGRAMMET LÄSER      WDK7                                       
002300*        PROGRAMMET UPPDATERAR WDA2                                       
002400*                                                                         
002500*    PGM-ÄNDRING:                                                         
002600*        SCR/ETRACKER NR. 829496  DATUM 2004-09                           
002610*        SCR/ETRACKER NR. 6314197 DATUM 2008-02 RÄTTA EFTER ABEND         
002620*        SCR/ETRACKER NR. 6426835 DATUM 2008-03 RÄTTA PRINT-ID            
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: WL0164                                              
003000*        MID:         WL0164I1                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         WL0164O1                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700                                                                          
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'WL016400'.            
004400 01  FILLER                      PIC X(8) VALUE 'AAAAAAAA'.               
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004800 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
004900 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
005000 01  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  WS-KDBEHX                   PIC X       VALUE SPACE.                 
005500 77  WS-IDILIST                  PIC 9(5)    VALUE ZERO.                  
005600 77  SPARA-IDILIST               PIC 9(5)    VALUE ZERO.                  
005700 77  WS-KVRADER                  PIC 9(7)    VALUE ZERO.                  
005800 77  W-PRINT                     PIC X       VALUE 'J'.                   
005900                                                                          
006000 01  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
006100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006200 77  INDX                        PIC S9(4)  VALUE +0    COMP-3.           
006300 77  L155-IX                     PIC S9(4)  VALUE +0    COMP-3.           
006400 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP-3.           
006500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006800     88  NYCKLAR-OK                          VALUE 'J'.                   
006900     88  NYCKLAR-FEL                         VALUE 'N'.                   
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  SW-FLCMD                    PIC X       VALUE 'N'.                   
007600     88  FLCMD-IFYLLT                        VALUE 'J'.                   
007700                                                                          
007900 77  SW-FLCMD-RAETT-IFYLLD       PIC X       VALUE 'N'.                   
008000     88  FLCMD-RAETT                         VALUE 'J'.                   
008100     88  FLCMD-FEL                           VALUE 'N'.                   
008200                                                                          
009000     EJECT                                                                
009100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009200 01  GENERELLA-SUBPROGRAM.                                                
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010000     EJECT                                                                
010100*                                                                         
010200*    --- PARAMETERS TO ABEND                                              
010300                                                                          
010400 77  FILLER                      PIC X(08)   VALUE 'ABENDKOD'.            
010500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
010600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010800     SKIP2                                                                
010900 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
011000 01  MESSAGE-CODES.                                                       
011100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
011200     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '023'.                 
011300     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
011400     03  INF-NO-MORE-LINES       PIC X(3)    VALUE '316'.                 
011500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '021'.                 
011900     03  ERR-ONLY-ONE-OPTION     PIC X(3)    VALUE '319'.                 
012000     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '027'.                 
012100     03  ERR-URVAL-SAKNA2        PIC X(3)    VALUE '028'.                 
012200     03  ERR-URVAL-SAKNA3        PIC X(3)    VALUE '029'.                 
012300     03  ERR-EMPLOYEE-ID-MISSING PIC X(3)    VALUE '320'.                 
012400     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
012500     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
012600     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
012700     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '293'.                 
012800     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
012900     03  ERR-WRONG-COMMAND-CODE  PIC X(3)    VALUE '304'.                 
013000     EJECT                                                                
016300*                                                                         
016400 01  FILLER              PIC X(16)   VALUE 'P-TO-P-AREA2'.                
016500*                                                                         
017600     EJECT                                                                
017700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017800*                                                                         
017900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
018000     SKIP3                                                                
018100*01  -COPY WZ01SUB                                                        
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018400     SKIP3                                                                
018500 01  REQU-AREA.                                                           
018600*    03  -COPY WZ01REQU                                                   
018700*    03  -COPY WL0164I1                                                   
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
019000     SKIP3                                                                
019100 01  RESP-AREA.                                                           
019200*    03  -COPY WZ01RESP                                                   
019300*    03  -COPY WL0164O1                                                   
019400     SKIP3                                                                
019500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019800     SKIP3                                                                
019900 01  NYCKLAR-TILL-DLI.                                                    
020000*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
020100                                                                          
020200     03  W-WDA2E1KY-MIN-X.                                                
020300         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
020400         05  W-IDILIST-E1-MIN    PIC  9(5)          VALUE ZERO.           
020500         05  W-ADLAGOMR-E1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
020600         05  W-ADGANG-E1-MIN     PIC S9(3)   COMP-3 VALUE ZERO.           
020700         05  W-ADPLATS-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
020800         05  W-IDDISTR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
020900         05  W-IDKUNDNR-E1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
021000         05  W-IDRAPPNR-E1-MIN   PIC  9(7)          VALUE ZERO.           
021100         05  W-IDARTNR-E1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
021200         05  W-IDRADNR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
021300                                                                          
021400     03  W-WDA2E1KY-MAX-X.                                                
021500         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
021600         05  W-IDILIST-E1-MAX    PIC  9(5)          VALUE ZERO.           
021700         05  W-ADLAGOMR-E1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
021800         05  W-ADGANG-E1-MAX     PIC S9(3)   COMP-3 VALUE ZERO.           
021900         05  W-ADPLATS-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
022000         05  W-IDDISTR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
022100         05  W-IDKUNDNR-E1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
022200         05  W-IDRAPPNR-E1-MAX   PIC  9(7)          VALUE ZERO.           
022300         05  W-IDARTNR-E1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
022400         05  W-IDRADNR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
022500                                                                          
022600     03  W-WDA2ESEQ-MIN-X.                                                
022700         05  W-IDDC-ESEQ-MIN     PIC  X(2)          VALUE SPACE.          
022800         05  W-IDILIST-ESEQ-MIN  PIC  9(5)          VALUE ZERO.           
022900         05  W-ADLAGOMR-ESEQ-MIN PIC S9(3)   COMP-3 VALUE ZERO.           
023000         05  W-ADGANG-ESEQ-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
023100         05  W-ADPLATS-ESEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
023200                                                                          
023300     03  W-WDA2ESEQ-MAX-X.                                                
023400         05  W-IDDC-ESEQ-MAX     PIC  X(2)          VALUE SPACE.          
023500         05  W-IDILIST-ESEQ-MAX  PIC  9(5)          VALUE ZERO.           
023600         05  W-ADLAGOMR-ESEQ-MAX PIC S9(3)   COMP-3 VALUE ZERO.           
023700         05  W-ADGANG-ESEQ-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
023800         05  W-ADPLATS-ESEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
023900                                                                          
024000     03  W-IDARTNR-X.                                                     
024100         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
024200                                                                          
024300     03  W-IDDC-X.                                                        
024400         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
024500                                                                          
024600     SKIP2                                                                
024700*    --- STATUS-KOD FRÅN IMS                                              
024800 01  STATUS-WS                   PIC XX.                                  
024900     88  SEGMENT-FINNS                       VALUE '  '.                  
025000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
025300     SKIP2                                                                
025400 01  GODK-STATUSKODER.                                                    
025500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025600     SKIP3                                                                
025700 01  SSA1                        PIC X(128).                              
025800 01  SSA2                        PIC X(128).                              
025900     EJECT                                                                
026000*    --- IMS FUNKTIONSKODER                                               
026100*01  -COPY W0003                                                          
026200     EJECT                                                                
026300*    ---  DLI INPUT-OUTPUT AREA                                           
026400                                                                          
026500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA2E1'.                      
026600 01  DLI-IO-WDA2E1.                                                       
026700*    03  -COPY WDA2E1                                                     
026800     EJECT                                                                
026900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
027000 01  DLI-IO-WDA211.                                                       
027100*    03  -COPY WDA211                                                     
027200     EJECT                                                                
027300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
027400 01  DLI-IO-WDK611.                                                       
027500*    03  -COPY WDK611                                                     
027600     EJECT                                                                
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
027800 01  DLI-IO-WDK711.                                                       
027900*    03  -COPY WDK711                                                     
028000     EJECT                                                                
028100 LINKAGE SECTION.                                                         
028200*01  -COPY W0009   -PRE MSG-                                              
028300     EJECT                                                                
028400*01  -COPY W0008  -PRE WDA2E-                                             
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700*01  -COPY W0008  -PRE WDK6-                                              
028800     05  FILLER                  PIC X.                                   
028900                                                                          
029000*01  -COPY W0008  -PRE WDK7-                                              
029100     05  FILLER                  PIC X.                                   
029200                                                                          
029300*01  -COPY W0008  -PRE WDA2-                                              
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600 PROCEDURE DIVISION  USING MSG-PCB WDA2E-PCB                              
029700     WDK6-PCB WDK7-PCB WDA2-PCB.                                          
029800 MAIN SECTION.                                                            
029900     ENTRY 'DLITCBL' USING MSG-PCB WDA2E-PCB                              
030000     WDK6-PCB WDK7-PCB WDA2-PCB.                                          
030100                                                                          
030200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
030300     IF SUB-KDRC = 0                                                      
030400       PERFORM A-INIT                                                     
030500       PERFORM B-KOLLA-NYCKLAR                                            
030600       IF NYCKLAR-OK                                                      
030700         IF REQU-KDPGMACT = 'E'                                           
030800           PERFORM G-KOLLA-INPUT                                          
030900           IF INDATA-OK                                                   
031000             PERFORM H-UPPDATERA-SKRIV-UT                                 
031100           END-IF                                                         
031201         ELSE                                                             
031210           PERFORM F-LAES-VISA-INFO                                       
031220         END-IF                                                           
031300       END-IF                                                             
031400                                                                          
031440       IF RESP-IDMSG-INFO NOT = SPACE                                     
031450          MOVE SPACE           TO RESP-IDMSG-ERROR                        
031460          MOVE SPACE           TO RESP-IDELMT-ERROR                       
031470       ELSE                                                               
031480         IF RESP-IDMSG-ERROR NOT = SPACE                                  
031491             MOVE ALL '+' TO RESP-WL0164O1                                
031494             MOVE  001    TO RESP-IDMSGVER                                
031495             IF  REQU-KDPGMACT = 'S'                                      
031496                MOVE ZERO              TO RESP-KVRADER                    
031497             ELSE                                                         
031498               IF REQU-KVRADER NUMERIC                                    
031499                 MOVE REQU-KVRADER     TO RESP-KVRADER                    
031500               ELSE                                                       
031510                 MOVE ZERO             TO RESP-KVRADER                    
031520               END-IF                                                     
031530             END-IF                                                       
031540         END-IF                                                           
031550       END-IF                                                             
031600                                                                          
032600       PERFORM S02-RETURN-RESPONSE                                        
032700     END-IF                                                               
032800                                                                          
032900     MOVE ZERO TO RETURN-CODE                                             
033000     GOBACK                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 A-INIT SECTION.                                                          
033400                                                                          
033500     MOVE 'STA A-INIT        ' TO PGM-POS                                 
033600                                                                          
033710     MOVE ALL '+'              TO RESP-AREA                               
033800     MOVE 001                  TO RESP-IDMSGVER                           
033900     MOVE SPACE                TO RESP-IDMSG-ERROR                        
034000                                  RESP-IDMSG-INFO                         
034100                                  RESP-IDELMT-ERROR                       
034200     MOVE  0                   TO WS-KVRADER                              
034300     MOVE  0                   TO RESP-KVRADER                            
034400                                                                          
034500     MOVE LOW-VALUE            TO W-WDA2E1KY-MIN-X                        
034600                                  W-WDA2ESEQ-MIN-X                        
034700                                                                          
034800     MOVE HIGH-VALUE           TO W-WDA2E1KY-MAX-X                        
034900                                  W-WDA2ESEQ-MAX-X                        
035000                                                                          
035100     .                                                                    
035200     EJECT                                                                
035300 B-KOLLA-NYCKLAR SECTION.                                                 
035400     MOVE 'STA B-KOLLA-NYCKLAR'   TO PGM-POS                              
035500                                                                          
035600     MOVE JA TO NYCKLAR-SW                                                
035700                                                                          
035800     IF REQU-KDBEHX-KEY     =  ALL '+'                                    
035900       MOVE ' '               TO WS-KDBEHX                                
036000     ELSE                                                                 
036100       MOVE REQU-KDBEHX-KEY   TO WS-KDBEHX                                
036200     END-IF                                                               
036300                                                                          
036400*    -- KONTROLL AV IDILIST                                               
036600                                                                          
036700     IF REQU-IDILIST-KEY    =  ALL '+'                                    
036800       MOVE ZERO              TO WS-IDILIST                               
036900     ELSE                                                                 
037000       MOVE REQU-IDILIST-KEY  TO WS-IDILIST                               
037100     END-IF                                                               
037200                                                                          
037300     IF WS-IDILIST NUMERIC                                                
037400       MOVE WS-IDILIST   TO W-IDILIST-E1-MIN                              
037500                            W-IDILIST-E1-MAX                              
037600     ELSE                                                                 
037700       MOVE NEJ TO NYCKLAR-SW                                             
037800     END-IF                                                               
037900                                                                          
038000*    -- KONTROLL AV KDBEHX                                                
038100                                                                          
038200     IF REQU-KDBEHX-KEY NOT = ALL '+'                                     
038300       IF REQU-KDBEHX-KEY = 'C' OR 'S' OR 'P' OR 'T'                      
038400         CONTINUE                                                         
038500       ELSE                                                               
038600         MOVE NEJ TO NYCKLAR-SW                                           
038700         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
038800       END-IF                                                             
038900     END-IF                                                               
039000                                                                          
039100     IF REQU-IDDC-KEY    =  ALL '+'                                       
039200       MOVE ZERO              TO RESP-IDDC-KEY                            
039300       MOVE NEJ TO NYCKLAR-SW                                             
039400     ELSE                                                                 
039500       MOVE REQU-IDDC-KEY     TO W-IDDC-E1-MIN                            
039600                                 W-IDDC-E1-MAX                            
039700                                 W-IDDC-ESEQ-MIN                          
039800                                 W-IDDC-ESEQ-MAX                          
039900                                 W-IDDC                                   
040000                                 RESP-IDDC-KEY                            
040100     END-IF                                                               
040200                                                                          
040300                                                                          
040400     IF NYCKLAR-OK                                                        
040500       MOVE WS-IDILIST        TO RESP-IDILIST-KEY                         
040600       INSPECT RESP-IDILIST-KEY REPLACING LEADING ZERO BY SPACE           
040700       IF REQU-KDPGMACT = 'S'                                             
041000         IF  REQU-KDBEHX-KEY = ALL '+'                                    
041100           MOVE SPACE             TO RESP-KDBEHX-KEY                      
041200         ELSE                                                             
041300           MOVE WS-KDBEHX         TO RESP-KDBEHX-KEY                      
041400         END-IF                                                           
043100       END-IF                                                             
043200     END-IF                                                               
043300                                                                          
043400     IF ( REQU-KDBEHX-KEY NOT = ALL '+' ) AND                             
043500        ( REQU-IDILIST-KEY NOT = ALL '+')                                 
043600       MOVE ERR-ONLY-ONE-OPTION    TO RESP-IDMSG-ERROR                    
043700       MOVE NEJ TO NYCKLAR-SW                                             
043800     END-IF                                                               
043900                                                                          
044000     IF NYCKLAR-FEL                                                       
044100       IF RESP-IDMSG-ERROR  = SPACE                                       
044200         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
044300       END-IF                                                             
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
045400 F-LAES-VISA-INFO SECTION.                                                
045500                                                                          
045600     IF WS-KDBEHX = '+' OR ' '                                            
045700*- LÄS UNIK I-LISTA                                                       
045800       CONTINUE                                                           
045900     ELSE                                                                 
046000*- LÄS ALLA I-LISTOR                                                      
046200       MOVE ZERO           TO W-IDILIST-E1-MIN                            
046300       MOVE 99999          TO W-IDILIST-E1-MAX                            
046700     END-IF                                                               
046800                                                                          
046900     PERFORM IMS-GU-WDA2E1                                                
047000                                                                          
047100     IF SEGMENT-SAKNAS                                                    
047200       MOVE ERR-URVAL-SAKNAS       TO RESP-IDMSG-ERROR                    
047600     ELSE                                                                 
047700                                                                          
047800       EVALUATE WS-KDBEHX                                                 
047900         WHEN ' '                                                         
048100           PERFORM FA-VISA-UNIK-ILISTA                                    
048200         WHEN '+'                                                         
048300           PERFORM FA-VISA-UNIK-ILISTA                                    
048400         WHEN 'T'                                                         
048500           PERFORM FB-VISA-ALLA-ILISTOR                                   
048800         WHEN 'C'                                                         
048900           PERFORM FC-VISA-SKAPADE-ILISTOR                                
049000         WHEN 'P'                                                         
049100           PERFORM FD-VISA-PRINTADE-ILISTOR                               
049200       END-EVALUATE                                                       
049300                                                                          
050500       MOVE WS-KVRADER            TO RESP-KVRADER                         
050510                                                                          
050520       IF WS-KVRADER = 500                                                
050530          MOVE '028'  TO RESP-IDMSG-ERROR                                 
050540       END-IF                                                             
050600                                                                          
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000 FA-VISA-UNIK-ILISTA SECTION.                                             
051100                                                                          
051200     MOVE +1 TO INDX                                                      
051300                                                                          
051500     PERFORM UNTIL SEGMENT-SAKNAS                                         
051600       IF SEGMENT-FINNS                                                   
051700         MOVE SEQE-IDILIST        TO RESP-IDILIST (INDX)                  
051800         MOVE SEQE-TIUPPDAT-ILI   TO RESP-TIUPPDAT-ILI (INDX)             
052000         MOVE SEQE-TIUTSKR        TO RESP-TIUTSKR-ILI (INDX)              
052100         ADD 1 TO WS-KVRADER                                              
052200                                                                          
052300         IF SEQE-TIUTSKR > ZERO                                           
052400           MOVE SEQE-IDANSTNR-RET TO RESP-IDANSTNR-RET-PRT (INDX)         
052410         ELSE                                                             
052420           MOVE SEQE-IDANSTNR-RET TO RESP-IDANSTNR-RET-ILI (INDX)         
052500         END-IF                                                           
052600                                                                          
052700*-BARA 1:A FÖREKOMSTEN AV ANGETT I-LISTE NR SKRIVS UT PÅ RAD ETT.         
052800         MOVE 'GE'                TO STATUS-WS                            
053100                                                                          
053200       END-IF                                                             
053300       ADD 1 TO INDX                                                      
053400     END-PERFORM                                                          
053500     .                                                                    
053600     EJECT                                                                
053700 FB-VISA-ALLA-ILISTOR SECTION.                                            
053800                                                                          
053900*- SKALL BARA VISA VARJE LISTNR EN GÅNG.                                  
054000                                                                          
054100     MOVE +1 TO INDX                                                      
054200                                                                          
054300     MOVE SEQE-IDILIST        TO RESP-IDILIST (INDX)                      
054400                                 SPARA-IDILIST                            
054500     MOVE SEQE-TIUPPDAT-ILI   TO RESP-TIUPPDAT-ILI (INDX)                 
054700     MOVE SEQE-TIUTSKR        TO RESP-TIUTSKR-ILI (INDX)                  
054800                                                                          
054900     IF SEQE-TIUTSKR > ZERO                                               
055000       MOVE SEQE-IDANSTNR-RET TO RESP-IDANSTNR-RET-PRT (INDX)             
055001     ELSE                                                                 
055010       MOVE SEQE-IDANSTNR-RET TO RESP-IDANSTNR-RET-ILI (INDX)             
055100     END-IF                                                               
055200     ADD 1 TO INDX                                                        
055300     PERFORM IMS-GN-WDA2E1                                                
055400     ADD 1 TO WS-KVRADER                                                  
055500                                                                          
055600     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                      
055800       IF SEGMENT-FINNS                                                   
055900         IF SEQE-IDILIST = SPARA-IDILIST                                  
056000           CONTINUE                                                       
056100         ELSE                                                             
056300           MOVE SEQE-IDILIST        TO RESP-IDILIST (INDX)                
056400                                       SPARA-IDILIST                      
056500           MOVE SEQE-TIUPPDAT-ILI   TO RESP-TIUPPDAT-ILI (INDX)           
056800           MOVE SEQE-TIUTSKR        TO RESP-TIUTSKR-ILI (INDX)            
056900                                                                          
057000           IF SEQE-TIUTSKR > ZERO                                         
057100             MOVE SEQE-IDANSTNR-RET                                       
057200                               TO RESP-IDANSTNR-RET-PRT (INDX)            
057201           ELSE                                                           
057210             MOVE SEQE-IDANSTNR-RET                                       
057220                               TO RESP-IDANSTNR-RET-ILI (INDX)            
057300           END-IF                                                         
057400           ADD 1 TO INDX                                                  
057500           ADD 1 TO WS-KVRADER                                            
057600         END-IF                                                           
057700         PERFORM IMS-GN-WDA2E1                                            
057800       END-IF                                                             
057900     END-PERFORM                                                          
058000     .                                                                    
058100     EJECT                                                                
058200 FC-VISA-SKAPADE-ILISTOR  SECTION.                                        
058300                                                                          
058400     MOVE +1 TO INDX                                                      
058500                                                                          
058600     IF SEQE-TIUTSKR = ZERO                                               
058700       PERFORM S01-FLYTTA-TILL-RESP                                       
058800       ADD 1 TO WS-KVRADER                                                
058900     ELSE                                                                 
059000       PERFORM UNTIL SEQE-TIUTSKR = ZERO OR SEGMENT-SAKNAS                
059100       PERFORM IMS-GN-WDA2E1                                              
059200         IF SEGMENT-FINNS AND SEQE-TIUTSKR = ZERO                         
059300           PERFORM S01-FLYTTA-TILL-RESP                                   
059400           ADD 1 TO WS-KVRADER                                            
059700         END-IF                                                           
059800       END-PERFORM                                                        
059900     END-IF                                                               
060000                                                                          
060100     IF SEGMENT-SAKNAS                                                    
060200       MOVE ERR-URVAL-SAKNAS     TO RESP-IDMSG-ERROR                      
060300     ELSE                                                                 
060400                                                                          
060500       PERFORM IMS-GN-WDA2E1                                              
060600       ADD +1 TO INDX                                                     
060700                                                                          
060800       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
061000         IF SEGMENT-FINNS                                                 
061100                                                                          
061200           IF SEQE-TIUTSKR > ZERO                                         
061300             PERFORM IMS-GN-WDA2E1                                        
061400           ELSE                                                           
061500             IF SEQE-IDILIST = SPARA-IDILIST                              
061600               CONTINUE                                                   
061700             ELSE                                                         
061800               PERFORM S01-FLYTTA-TILL-RESP                               
061900               ADD 1 TO INDX                                              
062000               ADD 1 TO WS-KVRADER                                        
062100             END-IF                                                       
062200             PERFORM IMS-GN-WDA2E1                                        
062300           END-IF                                                         
062400                                                                          
062700         ELSE                                                             
062800           ADD +1 TO INDX                                                 
062900         END-IF                                                           
063000       END-PERFORM                                                        
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 FD-VISA-PRINTADE-ILISTOR  SECTION.                                       
063500                                                                          
063600     MOVE +1 TO INDX                                                      
063700                                                                          
063800     IF SEQE-TIUTSKR > ZERO                                               
063900       PERFORM S01-FLYTTA-TILL-RESP                                       
064000       ADD 1 TO WS-KVRADER                                                
064100     ELSE                                                                 
064200       PERFORM UNTIL SEQE-TIUTSKR > ZERO OR SEGMENT-SAKNAS                
064300       PERFORM IMS-GN-WDA2E1                                              
064400         IF SEGMENT-FINNS AND SEQE-TIUTSKR > ZERO                         
064500           PERFORM S01-FLYTTA-TILL-RESP                                   
064600           ADD 1 TO WS-KVRADER                                            
064900         END-IF                                                           
065000       END-PERFORM                                                        
065100     END-IF                                                               
065200                                                                          
065300     IF SEGMENT-SAKNAS                                                    
065400       MOVE ERR-URVAL-SAKNAS     TO RESP-IDMSG-ERROR                      
065500     ELSE                                                                 
065600       PERFORM IMS-GN-WDA2E1                                              
065700       ADD +1 TO INDX                                                     
065800                                                                          
065900       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
066100         IF SEGMENT-FINNS                                                 
066200                                                                          
066300           IF SEQE-TIUTSKR = ZERO                                         
066400             PERFORM IMS-GN-WDA2E1                                        
066500           ELSE                                                           
066600                                                                          
066700             IF SEQE-IDILIST = SPARA-IDILIST                              
066800               CONTINUE                                                   
066900             ELSE                                                         
067000               PERFORM S01-FLYTTA-TILL-RESP                               
067100               ADD +1 TO INDX                                             
067200               ADD 1 TO WS-KVRADER                                        
067300             END-IF                                                       
067400             PERFORM IMS-GN-WDA2E1                                        
067500           END-IF                                                         
067600         ELSE                                                             
067700           ADD +1 TO INDX                                                 
067800         END-IF                                                           
067900                                                                          
068200       END-PERFORM                                                        
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600 G-KOLLA-INPUT SECTION.                                                   
068700                                                                          
068800     MOVE JA  TO INDATA-SW                                                
069200                                                                          
069500     PERFORM GA-FORMELL-KONTROLL                                          
069900                                                                          
070700     .                                                                    
070800     EJECT                                                                
070900 GA-FORMELL-KONTROLL SECTION.                                             
071000                                                                          
071610     IF REQU-KVRADER NOT NUMERIC OR REQU-KVRADER = 0                      
071611       MOVE NEJ TO INDATA-SW                                              
071612       MOVE '023'            TO RESP-IDMSG-ERROR                          
071613       MOVE 'CMD'            TO RESP-IDELMT-ERROR                         
071620     ELSE                                                                 
071630       MOVE REQU-KVRADER TO RESP-KVRADER                                  
071700       PERFORM GAA-KOLLA-FLCMD                                            
071800       PERFORM GAC-KOLLA-IDANSTNR                                         
071810     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200 GAA-KOLLA-FLCMD      SECTION.                                            
072300                                                                          
072400     MOVE +1                           TO INDX                            
072500                                                                          
072600     MOVE NEJ                          TO SW-FLCMD                        
072610                                          SW-FLCMD-RAETT-IFYLLD           
072700                                                                          
072800     PERFORM UNTIL INDX                 >  REQU-KVRADER                   
072900        IF REQU-FLCMD(INDX)  = 'N' OR 'J'                                 
073000           MOVE JA                      TO SW-FLCMD                       
073100           IF REQU-FLCMD(INDX) = 'J'                                      
073400             MOVE 'J'                   TO RESP-FLCMD(INDX)               
073500             MOVE JA                    TO SW-FLCMD-RAETT-IFYLLD          
073600           END-IF                                                         
073700        ELSE                                                              
073800           MOVE NEJ                     TO INDATA-SW                      
073900           MOVE ERR-CORR-HILITE-FLDS    TO RESP-IDMSG-ERROR               
074000                                   RESP-IDMSG-ERROR-LINE(INDX)            
074100        END-IF                                                            
074200        ADD +1                          TO INDX                           
074300     END-PERFORM                                                          
074400                                                                          
074500     IF FLCMD-IFYLLT                                                      
074610       IF FLCMD-FEL                                                       
074621         MOVE ERR-NO-LINE-CHOSEN        TO RESP-IDMSG-ERROR               
074622         MOVE 'CMD'                     TO RESP-IDELMT-ERROR              
074630         MOVE NEJ                       TO INDATA-SW                      
074640       END-IF                                                             
074700     ELSE                                                                 
074800        MOVE NEJ                        TO INDATA-SW                      
074900        MOVE ERR-NO-LINE-CHOSEN         TO RESP-IDMSG-ERROR               
075000        MOVE 'CMD'                      TO RESP-IDELMT-ERROR              
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 GAC-KOLLA-IDANSTNR SECTION.                                              
075500                                                                          
075600     IF REQU-IDANSTNR-UPD NOT = ALL '+'                                   
075700        IF REQU-IDANSTNR-UPD NOT NUMERIC                                  
075800           MOVE '023'                     TO RESP-IDMSG-ERROR             
075900           MOVE 'IDANSTN2'                TO RESP-IDELMT-ERROR            
076000           MOVE NEJ                       TO INDATA-SW                    
076200        END-IF                                                            
076300     END-IF                                                               
076400     IF REQU-IDANSTNR-UPD > ZERO                                          
076500        MOVE REQU-IDANSTNR-UPD TO   RESP-IDANSTNR-UPD                     
076800     END-IF                                                               
076900     .                                                                    
077000     EJECT                                                                
077100 H-UPPDATERA-SKRIV-UT SECTION.                                            
077200                                                                          
077300     PERFORM HA-BEHANDLA-VALDA-TILLSTAND                                  
077301                                                                          
077310     MOVE '237'               TO RESP-IDMSG-INFO                          
077600     .                                                                    
077700     EJECT                                                                
077800 HA-BEHANDLA-VALDA-TILLSTAND SECTION.                                     
077900                                                                          
077980                                                                          
078000     MOVE +1                    TO INDX                                   
078100                                                                          
078210     PERFORM UNTIL INDX > REQU-KVRADER                                    
078300                                                                          
078400       IF REQU-FLCMD(INDX)        = W-PRINT                               
078500                                                                          
078600         IF REQU-IDILIST(INDX) NUMERIC AND                                
078700            REQU-IDILIST(INDX) > ZERO                                     
078720           MOVE REQU-IDILIST(INDX) TO RESP-IDILIST (INDX)                 
078800                                      W-IDILIST-ESEQ-MIN                  
078900                                      W-IDILIST-ESEQ-MAX                  
079000                                                                          
079200           PERFORM IMS-GHU-SEQE-WDA211                                    
079300           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
079400                                                                          
079500             PERFORM HAA-KOLLA-PLATS                                      
079600                                                                          
079700             IF REQU-IDANSTNR-UPD NUMERIC                                 
079810               MOVE REQU-IDANSTNR-UPD TO LEV-IDANSTNR-RET                 
079900             END-IF                                                       
080000                                                                          
080100             ACCEPT LEV-TIUTSKR    FROM DATE                              
080200             PERFORM IMS-REPL-SEQE-WDA211                                 
080300                                                                          
080400             PERFORM IMS-GHN-SEQE-WDA211                                  
080500                                                                          
080600           END-PERFORM                                                    
080700         END-IF                                                           
080800       END-IF                                                             
080900       ADD +1                     TO INDX                                 
081000     END-PERFORM                                                          
081100                                                                          
081500     .                                                                    
081600     EJECT                                                                
081700 HAA-KOLLA-PLATS  SECTION.                                                
081800                                                                          
081900     MOVE LEV-IDARTNR          TO W-IDARTNR                               
082000                                                                          
082100     PERFORM IMS-GU-WDK711                                                
082200     IF SEGMENT-FINNS                                                     
082300        IF (SLAG-ADLAGOMR = LEV-ADLAGOMR    AND                           
082400            SLAG-ADGANG   = LEV-ADGANG       AND                          
082500            SLAG-ADPLATS  = LEV-ADPLATS)     OR                           
082600           (SLAG-ADLAGOMR = 22               AND                          
082700            LEV-ADLAGOMR  = 21               AND                          
082800            SLAG-ADGANG   = LEV-ADGANG       AND                          
082900            SLAG-ADPLATS  = LEV-ADPLATS)                                  
083000           CONTINUE                                                       
083100        ELSE                                                              
083200            MOVE SLAG-ADLAGOMR      TO LEV-ADLAGOMR                       
083300            IF LEV-ADLAGOMR = 22                                          
083400              MOVE 21               TO LEV-ADLAGOMR                       
083500            END-IF                                                        
083600            MOVE SLAG-ADGANG        TO LEV-ADGANG                         
083700            MOVE SLAG-ADPLATS       TO LEV-ADPLATS                        
083800        END-IF                                                            
083900     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084700 S01-FLYTTA-TILL-RESP SECTION.                                            
084800                                                                          
084900     MOVE SEQE-IDILIST        TO RESP-IDILIST (INDX)                      
085000                                 SPARA-IDILIST                            
085100     MOVE SEQE-TIUPPDAT-ILI   TO RESP-TIUPPDAT-ILI (INDX)                 
085300     MOVE SEQE-TIUTSKR        TO RESP-TIUTSKR-ILI (INDX)                  
085400                                                                          
085500     IF SEQE-TIUTSKR > ZERO                                               
085600       MOVE SEQE-IDANSTNR-RET TO RESP-IDANSTNR-RET-PRT (INDX)             
085601     ELSE                                                                 
085610       MOVE SEQE-IDANSTNR-RET TO RESP-IDANSTNR-RET-ILI (INDX)             
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
086100     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
086200                                                                          
086300     MOVE 'GETARG'               TO SUB-KDFUNC                            
086400     MOVE 'CARPARTS.LDC.BINNINGLISTSTATUS'  TO SUB-ADDISPABS              
086500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
086600                                                                          
086700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
086800                                                                          
086900     IF SUB-KDRC > 0                                                      
087000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
087100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
087200       DELIMITED BY SIZE INTO FELTEXT                                     
087300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
087400     END-IF                                                               
087500     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
087600     .                                                                    
087700     SKIP3                                                                
087800 S02-RETURN-RESPONSE SECTION.                                             
087900     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
088000                                                                          
088100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
088200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
088300                                                                          
088400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
088500                                                                          
088600     IF SUB-KDRC > 0                                                      
088700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
088800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
088900       DELIMITED BY SIZE INTO FELTEXT                                     
089000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
089100     END-IF                                                               
089200     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
089300     .                                                                    
089400     EJECT                                                                
089500* --- IMS SEKTIONER ---                                                   
089600     SKIP3                                                                
089700 IMS-GU-WDA2E1 SECTION.                                                   
089800                                                                          
089900     STRING 'WDA2E1  (WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
090000                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
090100          DELIMITED BY SIZE INTO SSA1                                     
090200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
090300     CALL CBLTDLI USING GU WDA2E-PCB DLI-IO-WDA2E1 SSA1                   
090400     MOVE WDA2E-STATUS-CODE TO STATUS-WS                                  
090500     PERFORM IMS-STATUSKONTROLL                                           
090600     .                                                                    
090700     EJECT                                                                
090800 IMS-GN-WDA2E1 SECTION.                                                   
090900                                                                          
091000     STRING 'WDA2E1  (WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
091100                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
091200          DELIMITED BY SIZE INTO SSA1                                     
091300     MOVE '  GE'           TO GODK-STATUSKODER                            
091400     CALL CBLTDLI USING GN WDA2E-PCB DLI-IO-WDA2E1 SSA1                   
091500     MOVE WDA2E-STATUS-CODE TO STATUS-WS                                  
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     EJECT                                                                
091900 IMS-GU-WDK711 SECTION.                                                   
092000                                                                          
092100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
092200          DELIMITED BY SIZE INTO SSA1                                     
092300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
092400          DELIMITED BY SIZE INTO SSA2                                     
092500     MOVE '  GE' TO GODK-STATUSKODER                                      
092600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
092700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
092800     PERFORM IMS-STATUSKONTROLL                                           
092900     .                                                                    
093000     SKIP3                                                                
093100 IMS-GHU-SEQE-WDA211    SECTION.                                          
093200                                                                          
093300     STRING 'WDA211  (WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
093400                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
093500          DELIMITED BY SIZE INTO SSA1                                     
093600     MOVE '  GE'           TO GODK-STATUSKODER                            
093700     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA211 SSA1                   
093800     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100                                                                          
094200 IMS-GHN-SEQE-WDA211    SECTION.                                          
094300                                                                          
094400     STRING 'WDA211  (WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
094500                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
094600          DELIMITED BY SIZE INTO SSA1                                     
094700     MOVE '  GEGB'           TO GODK-STATUSKODER                          
094800     CALL CBLTDLI USING GHN WDA2-PCB DLI-IO-WDA211 SSA1                   
094900     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
095000     PERFORM IMS-STATUSKONTROLL                                           
095100     .                                                                    
095200                                                                          
095300 IMS-REPL-SEQE-WDA211   SECTION.                                          
095400                                                                          
095500     MOVE '    '           TO GODK-STATUSKODER                            
095600     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
095700     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000     EJECT                                                                
096100 IMS-STATUSKONTROLL SECTION.                                              
096200                                                                          
096300     SET STATUS-IX TO 1                                                   
096400     SEARCH GODK-STATUS                                                   
096500       AT END                                                             
096600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
096700         DELIMITED BY SIZE INTO FELTEXT                                   
096800         CALL FELLOG                                                      
096900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
097000         CONTINUE                                                         
097100     END-SEARCH                                                           
097200     .                                                                    
