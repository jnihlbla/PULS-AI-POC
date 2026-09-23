000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0055100.                                                
000300 AUTHOR.         THOMAS NILSSON.                                          
000400 DATE-WRITTEN.   JULI  89.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        HELP FÖR IMS PROGRAM. VISAR DOKUMENTATION                        
001000*        FÖR IMS-BILDER, ALLMÄN SYSTEM INFO.                              
001100*        INFO NÅS FRÅN ANDRA PGM VIA PF1.                                 
001200*    INDATA.                                                              
001300*        TRANSAKTION: W0T551                                              
001400*                     W0T551U                                             
001500*                                                                         
001600*        MID:         W0I55101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O55101                                            
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400                                                                          
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77    IDPGM                     PIC X(8)    VALUE 'W0055100'.            
003000 77    INDX                      PIC S9(4)   VALUE ZERO COMP SYNC.        
003100 77    W-TRANSKOD                PIC X(8)    VALUE SPACE.                 
003200 77    JA                        PIC X(1)    VALUE 'J'.                   
003300 77    NEJ                       PIC X(1)    VALUE 'N'.                   
003400 77    IDDOKTYP-W                PIC X(8)    VALUE SPACE.                 
003500 77    IDDOK-W                   PIC X(8)    VALUE SPACE.                 
003600 77    ALLT-OK-SW                PIC X(1)    VALUE 'J'.                   
003700     88  ALLT-OK                             VALUE 'J'.                   
003800 77    W-IDTRANS                 PIC X(4)    VALUE SPACE.                 
003900     88  EGEN-TRANS                          VALUE '0551'.                
004000     88  GODK-TRANS                          VALUE '0551' '0552'          
004100                                                   '0553' '0555'.         
004200                                                                          
004300                                                                          
004400 01  W-CURRDATE                  PIC 9(12)   VALUE ZERO.                  
004500 01  FILLER REDEFINES W-CURRDATE.                                         
004600   03  W-DATE                    PIC 9(6).                                
004700   03  W-TIME                    PIC 9(6).                                
004800                                                                          
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200                                                                          
005300     EJECT                                                                
005400 01    MEDDELANDE.                                                        
005500     03  MED-1.                                                           
005600         05 FILLER               PIC X(32)   VALUE                        
005700             '105 FLER SIDOR FINNS, TRYCK PF8 '.                          
005800         05 FILLER               PIC X(32)   VALUE                        
005900             '105 FOR MORE PAGES, PRESS PF8   '.                          
006000     03  FILLER REDEFINES MED-1.                                          
006100         05 MED1                 PIC X(32)   OCCURS 2.                    
006200                                                                          
006300     03  MED-2.                                                           
006400         05 FILLER               PIC X(32)   VALUE                        
006500             '106 DETTA ÄR SISTA SIDAN        '.                          
006600         05 FILLER               PIC X(32)   VALUE                        
006700             '106 LAST PAGE                   '.                          
006800     03  FILLER REDEFINES MED-2.                                          
006900         05 MED2                 PIC X(32)   OCCURS 2.                    
007000                                                                          
007100     03  MED-3.                                                           
007200         05 FILLER               PIC X(32)   VALUE                        
007300             '413 INFORMATION SAKNAS          '.                          
007400         05 FILLER               PIC X(32)   VALUE                        
007500             '413 INFORMATION MISSING         '.                          
007600     03  FILLER REDEFINES MED-3.                                          
007700         05 MED3                 PIC X(32)   OCCURS 2.                    
007800                                                                          
007900     03  MED-4.                                                           
008000         05 FILLER               PIC X(32)   VALUE                        
008100             'USER/TRANS EJ GODKÄND SOM TYP   '.                          
008200         05 FILLER               PIC X(32)   VALUE                        
008300             'USER/TRANS NOT VALID AS TYPE    '.                          
008400     03  FILLER REDEFINES MED-4.                                          
008500         05 MED4                 PIC X(32)   OCCURS 2.                    
008600                                                                          
008700     EJECT                                                                
008800******************************************************************        
008900*                                                                         
009000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009100*                                                                         
009200 01  FILLER                PIC X(16)   VALUE 'MID-AREA     '.             
009300                                                                          
009400*01  -COPY W0I55101                                                       
009500     EJECT                                                                
009600 01  FILLER                PIC X(16)   VALUE 'MSG-AREA     '.             
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900*  03  -COPY W0O55101           -RED MSG-AREA.                            
010000     EJECT                                                                
010100 01  FILLER                PIC X(16)   VALUE 'MFS-AREA     '.             
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400******************************************************************        
010500*                                                                         
010600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800 01    IMS-WS.                                                            
010900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
011000                                                                          
011100 01    NYCKLAR-TILL-DLI.                                                  
011200   03  W-WDP501KY-X.                                                      
011300     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
011400     05  W-IDDOKTYP              PIC X(8)    VALUE SPACE.                 
011500     05  W-IDDOK                 PIC X(8)    VALUE SPACE.                 
011600   03  W-IDSID-X.                                                         
011700     05  W-IDSID                 PIC S9(3)   VALUE ZERO  COMP-3.          
011800                                                                          
011900*                        **** STATUS-KOD FRÅN IMS                         
012000   03    STATUS-WS               PIC XX.                                  
012100     88    SEGMENT-FINNS                     VALUE '  '.                  
012200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
012300     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
012400     88    NOT-AUTH                          VALUE 'A4'.                  
012410     88    TRANS-MISSING                     VALUE 'A1'.                  
012500                                                                          
012600   03    GODK-STATUSKODER.                                                
012700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
012800                                                                          
012900 01    SSA1                      PIC X(64).                               
013000 01    SSA2                      PIC X(64).                               
013100                                                                          
013200     EJECT                                                                
013300*                            IMS FUNKTIONSKODER                           
013400*01    -COPY W0003                                                        
013500     EJECT                                                                
013600*                            DLI INPUT-OUTPUT AREA                        
013700 01    FILLER                PIC X(16)   VALUE 'DLI-IO-WDP501'.           
013800                                                                          
013900 01    DLI-IO-P501.                                                       
014000*  03    -COPY WDP501                                                     
014100                                                                          
014200     EJECT                                                                
014300 01    FILLER                PIC X(16)   VALUE 'DLI-IO-WDP512'.           
014400                                                                          
014500 01    DLI-IO-P512.                                                       
014600*  03    -COPY WDP512                                                     
014700                                                                          
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000*01    -COPY W0009     -PRE MSG-                                          
015100*01    -COPY W0009     -PRE ALT-                                          
015200*01    -COPY W0008     -PRE WDP5-                                         
015300     05  FILLER                  PIC X.                                   
015400                                                                          
015500     EJECT                                                                
015600 PROCEDURE DIVISION USING MSG-PCB ALT-PCB WDP5-PCB.                       
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP5-PCB.                      
015900                                                                          
016000     PERFORM IMS-GET-MSG                                                  
016100     IF SEGMENT-FINNS                                                     
016200       PERFORM A-INIT-SPARA-INPUT                                         
016300       IF MFS-IDPFK = '3'                                                 
016400         PERFORM D-AVSLUT                                                 
016500       ELSE                                                               
016600         IF MFS-UPDATE                                                    
016700           PERFORM C-SPARA-INMED                                          
016800         ELSE                                                             
016900           MOVE IDDOKTYP-W          TO W-IDDOKTYP                         
017000           MOVE IDDOK-W             TO W-IDDOK                            
017100           IF MFS-IDPFK = '7'                                             
017200             MOVE +1                TO W-IDSID                            
017300           ELSE                                                           
017400             MOVE MID-IDSID         TO W-IDSID                            
017500             IF MFS-IDPFK = '8'                                           
017600               ADD +1               TO W-IDSID                            
017700             END-IF                                                       
017800           END-IF                                                         
017900         END-IF                                                           
018000         PERFORM B-LAS-SIDOR                                              
018100         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55101 + 4                    
018200         PERFORM IMS-INSERT-MSG                                           
018300       END-IF                                                             
018400     END-IF                                                               
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900                                                                          
019000     EJECT                                                                
019100 A-INIT-SPARA-INPUT SECTION.                                              
019200     IF MSG-DUBBLA-TRANSKODER                                             
019300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I55101                 
019400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
019500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
019600       MOVE MSG-KDTRANS-2                 TO W-TRANSKOD                   
019700     ELSE                                                                 
019800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I55101                 
019900       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
020000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
020100       STRING 'W' MFS-IDTRANS (1:1)                                       
020200              'T' MFS-IDTRANS (2:3) '  '                                  
020300              DELIMITED BY SIZE INTO W-TRANSKOD                           
020400       END-STRING                                                         
020500     END-IF                                                               
020600     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
020700     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
020800     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
020900                                                                          
021000     IF ENGLISH-TEXT                                                      
021100       MOVE +2                            TO INDX                         
021200     ELSE                                                                 
021300       MOVE +1                            TO INDX                         
021400     END-IF                                                               
021500     MOVE 'S  '                           TO W-IDSKYLT                    
021600                                                                          
021700     MOVE LOW-VALUE                       TO MSG-AREA                     
021800     MOVE 'W0O55101'                      TO MFS-IDMOD                    
021900     MOVE '0551'                          TO MOD-IDTRANS                  
022000     MOVE MFS-RENSA-FAELT                 TO MOD-IDDOKTYP-IN              
022100                                             MOD-IDDOK-IN                 
022200                                             MOD-TEINFO                   
022300                                             MOD-TEMFSFEL                 
022400                                             MOD-TEMFSINF                 
022500                                                                          
022600     MOVE FUNCTION CURRENT-DATE (3:12) TO W-CURRDATE                      
022700                                                                          
022800     EJECT                                                                
022900                                                                          
023000     IF MFS-UPDATE                                                        
023100       CONTINUE                                                           
023200     ELSE                                                                 
023300       IF GODK-TRANS                                                      
023400         IF MID-IDDOKTYP-IN =  ALL '+'                                    
023500           MOVE MID-IDDOKTYP-UT      TO IDDOKTYP-W                        
023600         ELSE                                                             
023700           MOVE MID-IDDOKTYP-IN      TO IDDOKTYP-W                        
023800           MOVE '7'                  TO MFS-IDPFK                         
023900         END-IF                                                           
024000                                                                          
024100         IF MID-IDDOK-IN =  ALL '+'                                       
024200           MOVE MID-IDDOK-UT         TO IDDOK-W                           
024300         ELSE                                                             
024400           MOVE MID-IDDOK-IN         TO IDDOK-W                           
024500           MOVE '7'                  TO MFS-IDPFK                         
024600         END-IF                                                           
024700         IF EGEN-TRANS                                                    
024710           IF MID-IDSID NOT NUMERIC                                       
024711             MOVE +1                 TO MID-IDSID                         
024712           END-IF                                                         
024800           MOVE MID-IDSID            TO W-IDSID                           
024900         ELSE                                                             
025000           MOVE +1                   TO W-IDSID                           
025100                                        MID-IDSID                         
025200         END-IF                                                           
025300       ELSE                                                               
025400         MOVE 'U'                    TO MFS-KDTRTYP                       
025500       END-IF                                                             
025600     END-IF                                                               
025700     .                                                                    
025800                                                                          
025900     EJECT                                                                
026000 B-LAS-SIDOR SECTION.                                                     
026100                                                                          
026200     MOVE W-IDDOKTYP           TO MOD-IDDOKTYP-UT                         
026300     MOVE W-IDDOK              TO MOD-IDDOK-UT                            
026400     MOVE W-IDSID              TO MOD-IDSID                               
026500     PERFORM IMS-GU-INFO                                                  
026600     IF SEGMENT-FINNS                                                     
026700       PERFORM IMS-GNP-SIDA                                               
026800       IF SEGMENT-FINNS                                                   
026900         MOVE TEXT-TEINFO      TO MOD-TEINFO                              
027000         MOVE TEXT-IDSID       TO MOD-IDSID                               
027100         PERFORM IMS-GNP-SIDA-OKVAL                                       
027200         IF SEGMENT-FINNS                                                 
027300           MOVE MED1 (INDX)    TO MOD-TEMFSINF                            
027400         ELSE                                                             
027500           MOVE MED2 (INDX)    TO MOD-TEMFSINF                            
027600         END-IF                                                           
027700       ELSE                                                               
027800         MOVE MED3 (INDX)      TO MOD-TEMFSFEL                            
027900       END-IF                                                             
028000     ELSE                                                                 
028100       MOVE MED3 (INDX)        TO MOD-TEMFSFEL                            
028200     END-IF                                                               
028300     .                                                                    
028400                                                                          
028500     EJECT                                                                
028600 C-SPARA-INMED SECTION.                                                   
028700                                                                          
028800     MOVE W-IDSKYLT            TO INFO-IDSKYLT                            
028900     MOVE 'USER'               TO INFO-IDDOKTYP                           
029000                                  W-IDDOKTYP                              
029100     MOVE MSG-LTERM-NAME       TO INFO-IDDOK                              
029200                                  W-IDDOK                                 
029300     MOVE W-DATE               TO INFO-TIREGDAT                           
029400     MOVE W-TIME               TO INFO-TIREGTID                           
029500     MOVE W-IDTRANS            TO INFO-IDUSER                             
029600     MOVE SPACE                TO INFO-BEDOK                              
029700                                  INFO-FILLER                             
029800     PERFORM IMS-ISRT-INFO                                                
029900     IF SEGMENT-FINNS-REDAN                                               
030000       PERFORM IMS-GHU-INFO                                               
030100       MOVE W-DATE             TO INFO-TIREGDAT                           
030200       MOVE W-TIME             TO INFO-TIREGTID                           
030300       MOVE W-IDTRANS          TO INFO-IDUSER                             
030400       MOVE SPACE              TO INFO-BEDOK                              
030500                                  INFO-FILLER                             
030600       PERFORM IMS-REPL-INFO                                              
030700     END-IF                                                               
030800     MOVE +1                   TO TEXT-IDSID                              
030900                                  W-IDSID                                 
031000     MOVE W-TRANSKOD           TO TEXT-IDUSER                             
031100     MOVE W-DATE               TO TEXT-TIREGDAT                           
031200     MOVE W-TIME               TO TEXT-TIREGTID                           
031300     MOVE MID-TEINFO           TO TEXT-TEINFO                             
031400     PERFORM IMS-ISRT-SIDA                                                
031500     IF SEGMENT-FINNS-REDAN                                               
031600       PERFORM IMS-GHU-SIDA                                               
031700       MOVE W-TRANSKOD         TO TEXT-IDUSER                             
031800       MOVE W-DATE             TO TEXT-TIREGDAT                           
031900       MOVE W-TIME             TO TEXT-TIREGTID                           
032000       MOVE MID-TEINFO         TO TEXT-TEINFO                             
032100       PERFORM IMS-REPL-SIDA                                              
032200     END-IF                                                               
032300                                                                          
032400     MOVE 'TRANS'              TO W-IDDOKTYP                              
032500     MOVE W-TRANSKOD           TO W-IDDOK                                 
032600     MOVE +1                   TO W-IDSID                                 
032700     .                                                                    
032800                                                                          
032900     EJECT                                                                
033000 D-AVSLUT SECTION.                                                        
033100                                                                          
033200     MOVE 'USER'             TO W-IDDOKTYP                                
033300     MOVE MSG-LTERM-NAME     TO W-IDDOK                                   
033400     PERFORM IMS-GU-INFO                                                  
033500     IF SEGMENT-FINNS                                                     
033600       PERFORM IMS-GNP-SIDA-OKVAL                                         
033700       PERFORM IMS-GHU-INFO                                               
033800       PERFORM IMS-DLET-INFO                                              
033900                                                                          
034000       MOVE TEXT-IDUSER      TO MSG-KDTRANS-1                             
034100       MOVE '0551'           TO MSG-IDTRANS-1                             
034200       MOVE MFS-KDMFSFOR     TO MSG-KDMFSFOR-1                            
034300       MOVE TEXT-TEINFO      TO MSG-INDATA-MINUS-1-TRANSKOD               
034400       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55101 + 4                      
034500       PERFORM IMS-ALT-CHNG                                               
034600       IF NOT-AUTH OR TRANS-MISSING                                       
034700         MOVE MED4 (INDX)      TO MOD-TEMFSFEL                            
034800         MOVE MFS-RENSA-FAELT  TO MOD-IDDOKTYP-UT                         
034900                                  MOD-IDDOK-UT                            
035000                                  MOD-IDSID                               
035100                                  MOD-TEINFO                              
035300         PERFORM IMS-INSERT-MSG                                           
035400       ELSE                                                               
035500         PERFORM IMS-ALT-ISRT                                             
035600       END-IF                                                             
035700     ELSE                                                                 
035800       MOVE MED3 (INDX)      TO MOD-TEMFSFEL                              
035900       MOVE MFS-RENSA-FAELT  TO MOD-IDDOKTYP-UT                           
036000                                MOD-IDDOK-UT                              
036100                                MOD-IDSID                                 
036200                                MOD-TEINFO                                
036300       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55101 + 4                      
036400       PERFORM IMS-INSERT-MSG                                             
036500     END-IF                                                               
036600     .                                                                    
036700                                                                          
036800     EJECT                                                                
036900* IMS SEKTIONER                                                           
037000                                                                          
037100 IMS-GET-MSG SECTION.                                                     
037200                                                                          
037300     MOVE '  QC' TO GODK-STATUSKODER                                      
037400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
037500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037600     PERFORM IMS-STATUSKONTROLL                                           
037700     .                                                                    
037800                                                                          
037900 IMS-INSERT-MSG SECTION.                                                  
038000                                                                          
038100     IF ENGLISH-TEXT                                                      
038200       MOVE 'N' TO MFS-KDHUVOMR                                           
038300     END-IF                                                               
038400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
038500     MOVE SPACE TO GODK-STATUSKODER                                       
038600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
038700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038800     PERFORM IMS-STATUSKONTROLL                                           
038900     .                                                                    
039000                                                                          
039100     EJECT                                                                
039200 IMS-ALT-ISRT SECTION.                                                    
039300                                                                          
039400     IF ENGLISH-TEXT                                                      
039500       MOVE 'N' TO MFS-KDHUVOMR                                           
039600     END-IF                                                               
039700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
039800     MOVE SPACE TO GODK-STATUSKODER                                       
039900     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
040000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
040100     PERFORM IMS-STATUSKONTROLL                                           
040200     .                                                                    
040300                                                                          
040400 IMS-ALT-CHNG SECTION.                                                    
040500                                                                          
040600     MOVE '  A1A4' TO GODK-STATUSKODER                                    
040700     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
040800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
040900     PERFORM IMS-STATUSKONTROLL                                           
041000     .                                                                    
041100                                                                          
041200     EJECT                                                                
041300 IMS-GU-INFO SECTION.                                                     
041400                                                                          
041500     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
041600            DELIMITED BY SIZE INTO SSA1                                   
041700     MOVE '  GE' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GU  WDP5-PCB DLI-IO-P501 SSA1                     
041900     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200                                                                          
042300 IMS-GNP-SIDA-OKVAL SECTION.                                              
042400                                                                          
042500     MOVE 'WDP512   ' TO SSA1                                             
042600     MOVE '  GE' TO GODK-STATUSKODER                                      
042700     CALL CBLTDLI USING GNP WDP5-PCB DLI-IO-P512 SSA1                     
042800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
042900     PERFORM IMS-STATUSKONTROLL                                           
043000     .                                                                    
043100                                                                          
043200 IMS-GNP-SIDA SECTION.                                                    
043300                                                                          
043400     STRING 'WDP512  (IDSID   >=' W-IDSID-X ')'                           
043500            DELIMITED BY SIZE INTO SSA1                                   
043600     MOVE '  GE' TO GODK-STATUSKODER                                      
043700     CALL CBLTDLI USING GNP WDP5-PCB DLI-IO-P512 SSA1                     
043800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
043900     PERFORM IMS-STATUSKONTROLL                                           
044000     .                                                                    
044100                                                                          
044200     EJECT                                                                
044300 IMS-GHU-INFO SECTION.                                                    
044400                                                                          
044500     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
044600            DELIMITED BY SIZE INTO SSA1                                   
044700     MOVE '  GE' TO GODK-STATUSKODER                                      
044800     CALL CBLTDLI USING GHU  WDP5-PCB DLI-IO-P501 SSA1                    
044900     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     .                                                                    
045200                                                                          
045300 IMS-ISRT-INFO SECTION.                                                   
045400                                                                          
045500     MOVE 'WDP501   ' TO SSA1                                             
045600     MOVE '  II' TO GODK-STATUSKODER                                      
045700     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-P501 SSA1                    
045800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
045900     PERFORM IMS-STATUSKONTROLL                                           
046000     .                                                                    
046100                                                                          
046200 IMS-DLET-INFO SECTION.                                                   
046300                                                                          
046400     MOVE '  ' TO GODK-STATUSKODER                                        
046500     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-P501                         
046600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     .                                                                    
046900                                                                          
047000 IMS-REPL-INFO SECTION.                                                   
047100                                                                          
047200     MOVE '  ' TO GODK-STATUSKODER                                        
047300     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-P501                         
047400     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
047500     PERFORM IMS-STATUSKONTROLL                                           
047600     .                                                                    
047700                                                                          
047800     EJECT                                                                
047900 IMS-GHU-SIDA SECTION.                                                    
048000                                                                          
048100     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
048200            DELIMITED BY SIZE INTO SSA1                                   
048300     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
048400            DELIMITED BY SIZE INTO SSA2                                   
048500     MOVE '  ' TO GODK-STATUSKODER                                        
048600     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-P512 SSA1 SSA2                
048700     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
048800     PERFORM IMS-STATUSKONTROLL                                           
048900     .                                                                    
049000                                                                          
049100 IMS-ISRT-SIDA SECTION.                                                   
049200                                                                          
049300     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
049400            DELIMITED BY SIZE INTO SSA1                                   
049500     MOVE 'WDP512   ' TO SSA2                                             
049600     MOVE '  II' TO GODK-STATUSKODER                                      
049700     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-P512 SSA1 SSA2               
049800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
049900     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
050100                                                                          
050200 IMS-REPL-SIDA SECTION.                                                   
050300                                                                          
050400     MOVE '  ' TO GODK-STATUSKODER                                        
050500     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-P512                         
050600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900                                                                          
051000     EJECT                                                                
051100 IMS-STATUSKONTROLL SECTION.                                              
051200                                                                          
051300     SET STATUS-IX TO 1                                                   
051400     SEARCH GODK-STATUS                                                   
051500       AT END                                                             
051600         CALL FELLOG                                                      
051700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051800         CONTINUE                                                         
051900     END-SEARCH                                                           
052000     .                                                                    
