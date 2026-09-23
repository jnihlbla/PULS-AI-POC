000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0055300.                                                
000300 AUTHOR.         THOMAS NILSSON.                                          
000400 DATE-WRITTEN.   JULI  89.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        HELP FÖR IMS PROGRAM (TRANS).                                    
001000*        VISAR/LÄGGER UPP DOKUMENTATION FÖR IMS-BILDER.                   
001100*        HELP FÖR ARTIKELINFORMATION (ARTNOT).                            
001200*        VISAR/LÄGGER UPP INFORMATION OM ARTIKLAR.                        
001300*        ÖVRIGT, ALLMÄN SYSTEM INFO.                                      
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T553                                              
001600*                     W0T553U                                             
001700*        MID:         W0I55301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W0O55301                                            
002010*                                                                         
002020******************************************************************        
002030*****                      C H A N G E L O G                              
002031******************************************************************        
002040*                                                                         
002041* 2011-05-05  SO  E-TRACKER 7936414  STOCK STEERING IN PULS               
002050*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(8)    VALUE 'W0055300'.            
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  W-TRANSKOD                  PIC X(8)    VALUE SPACE.                 
003400 77  IDDOKTYP-W                  PIC X(8)    VALUE SPACE.                 
003500 77  IDDOK-W                     PIC X(8)    VALUE SPACE.                 
003600 77  IDSID-W                     PIC X(3)    VALUE SPACE.                 
003700 77  IDSID-W-IN                  PIC X(3)    VALUE SPACE.                 
003800 77  IDDOK-FIX                   PIC X(9)    VALUE SPACE.                 
003900 77  W-FIX                       PIC X(9)    VALUE SPACE.                 
004000 77  INDX                        PIC S9(9)   VALUE ZERO COMP SYNC.        
004100 77  NYCKLAR                     PIC X       VALUE 'J'.                   
004200     88  NYCKLAR-OK                          VALUE 'J'.                   
004300     88  NYCKLAR-FEL                         VALUE 'N'.                   
004400 77  W-IDTRANS                   PIC X(4).                                
004500     88  EGEN-TRANS                          VALUE '0553'.                
004600     88  GODK-TRANS                          VALUE '0551' '0552'          
004700                                                   '0553' '0555'.         
004800     88  2106-TRANS                          VALUE '2106'.                
004810     88  2406-TRANS                          VALUE '2406'.                
004900     88  2111-TRANS                          VALUE '2111'.                
005000     88  2311-TRANS                          VALUE '2311'.                
005001     88  2313-TRANS                          VALUE '2313'.                
005002     88  2314-TRANS                          VALUE '2314'.                
005003     88  2366-TRANS                          VALUE '2366'.                
005010     88  2367-TRANS                          VALUE '2367'.                
005100                                                                          
005200     EJECT                                                                
005300 01  W-CURRDATE                  PIC 9(12)   VALUE ZERO.                  
005400 01  FILLER REDEFINES W-CURRDATE.                                         
005500   03  W-DATE                    PIC 9(6).                                
005600   03  W-TIME                    PIC 9(6).                                
005700                                                                          
005710 01  W-IDDOKTYP-RED              PIC X(8)   VALUE SPACE.                  
005720 01  FILLER REDEFINES W-IDDOKTYP-RED.                                     
005730   03  W-DOKTYP                  PIC X(6).                                
005740   03  W-IDDC-DOKTYP             PIC X(2).                                
005750                                                                          
005800                                                                          
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
006100   03  WTXTTR                    PIC X(8)    VALUE 'WTXTTR  '.            
006200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006300   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006400                                                                          
006500     EJECT                                                                
006600 01    MEDDELANDE.                                                        
006700     03  MED-1.                                                           
006800         05 FILLER               PIC X(32)   VALUE                        
006900             'TRYCK PF8 FÖR FLER SIDOR        '.                          
007000         05 FILLER               PIC X(32)   VALUE                        
007100             'PRESS PF8 FOR MORE PAGES        '.                          
007200     03  FILLER REDEFINES MED-1.                                          
007300         05 MED1                 PIC X(32)   OCCURS 2.                    
007400                                                                          
007500     03  MED-3.                                                           
007600         05 FILLER               PIC X(32)   VALUE                        
007700             'SIDAN UPPDATERAD                '.                          
007800         05 FILLER               PIC X(32)   VALUE                        
007900             'PAGE HAS BEEN UPDATED           '.                          
008000     03  FILLER REDEFINES MED-3.                                          
008100         05 MED3                 PIC X(32)   OCCURS 2.                    
008200                                                                          
008300     03  MED-4.                                                           
008400         05 FILLER               PIC X(32)   VALUE                        
008500             'SIDAN UPPLAGD                   '.                          
008600         05 FILLER               PIC X(32)   VALUE                        
008700             'PAGE HAS BEEN INSERTED          '.                          
008800     03  FILLER REDEFINES MED-4.                                          
008900         05 MED4                 PIC X(32)   OCCURS 2.                    
009000                                                                          
009100     03  MED-5.                                                           
009200         05 FILLER               PIC X(32)   VALUE                        
009300             'SIDAN BORTTAGEN                 '.                          
009400         05 FILLER               PIC X(32)   VALUE                        
009500             'PAGE HAS BEEN DELETED           '.                          
009600     03  FILLER REDEFINES MED-5.                                          
009700         05 MED5                 PIC X(32)   OCCURS 2.                    
009800                                                                          
009900     03  MED-7.                                                           
010000         05 FILLER               PIC X(33)   VALUE                        
010100             'INFO UPPDATERAD                  '.                         
010200         05 FILLER               PIC X(33)   VALUE                        
010300             'INFO UPDATED                    '.                          
010400     03  FILLER REDEFINES MED-7.                                          
010500         05 MED7                 PIC X(33)   OCCURS 2.                    
010600                                                                          
010700     03  MED-8.                                                           
010800         05 FILLER               PIC X(33)   VALUE                        
010900             'INFO UPPLAGD                     '.                         
011000         05 FILLER               PIC X(33)   VALUE                        
011100             'INFO INSERTED                   '.                          
011200     03  FILLER REDEFINES MED-8.                                          
011300         05 MED8                 PIC X(33)   OCCURS 2.                    
011400                                                                          
011500     03  FEL-1.                                                           
011600         05 FILLER               PIC X(32)   VALUE                        
011700             'NYCKLAR FELAKTIGA               '.                          
011800         05 FILLER               PIC X(32)   VALUE                        
011900             'WRONG KEYS                      '.                          
012000     03  FILLER REDEFINES FEL-1.                                          
012100         05 FEL1                 PIC X(32)   OCCURS 2.                    
012200                                                                          
012300     03  FEL-3.                                                           
012400         05 FILLER               PIC X(32)   VALUE                        
012500             'INFORMATION SAKNAS              '.                          
012600         05 FILLER               PIC X(32)   VALUE                        
012700             'INFORMATION MISSING             '.                          
012800     03  FILLER REDEFINES FEL-3.                                          
012900         05 FEL3                 PIC X(32)   OCCURS 2.                    
013000                                                                          
013100     03  FEL-6.                                                           
013200         05 FILLER               PIC X(32)   VALUE                        
013300             'USER/TRANS EJ GODKÄND SOM TYP '.                            
013400         05 FILLER               PIC X(32)   VALUE                        
013500             'USER/TRANS NOT VALID AS TYPE  '.                            
013600     03  FILLER REDEFINES FEL-6.                                          
013700         05 FEL6                 PIC X(32)   OCCURS 2.                    
013800                                                                          
013810     03  FEL-7.                                                           
013820         05 FILLER               PIC X(32)   VALUE                        
013830             'VORNOT EJ GODKÄND SOM TYP     '.                            
013840         05 FILLER               PIC X(32)   VALUE                        
013850             'VORNOT NOT VALID AS TYPE      '.                            
013860     03  FILLER REDEFINES FEL-7.                                          
013870         05 FEL7                 PIC X(32)   OCCURS 2.                    
013880                                                                          
013900     03  FEL-8.                                                           
014000         05 FILLER               PIC X(32)   VALUE                        
014100             'TRYCK PF11 FÖR UPPDATERING    '.                            
014200         05 FILLER               PIC X(32)   VALUE                        
014300             'PRESS PF11 FOR UPDATE         '.                            
014400     03  FILLER REDEFINES FEL-8.                                          
014500         05 FEL8                 PIC X(32)   OCCURS 2.                    
014600                                                                          
014700     EJECT                                                                
014800 01  FILLER                  PIC X(16)   VALUE 'WTXTAREA'.                
014900 01  WTXTTR-AREA.                                                         
015000*  03 TXT -COPY WTXTAREA                                                  
015100                                                                          
015200     EJECT                                                                
015300 01  FILLER                  PIC X(16)   VALUE 'WMSGINIT'.                
015400*01  -COPY WMSGINIT                                                       
015500                                                                          
015600     EJECT                                                                
015700******************************************************************        
015800*                                                                         
015900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
016000*                                                                         
016100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
016200                                                                          
016300*01    MID -COPY W0I55301                                                 
016400                                                                          
016500     EJECT                                                                
016600*01    -COPY WMSGAREA                                                     
016700                                                                          
016800     EJECT                                                                
016900*  03    MOD -COPY W0O55301           -RED MSG-AREA.                      
017000                                                                          
017100     EJECT                                                                
017200 01    FILLER                    PIC X(16)   VALUE 'WMFSAREA'.            
017300*01    -COPY WMFSAREA                                                     
017400                                                                          
017500     EJECT                                                                
017600******************************************************************        
017700*                                                                         
017800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017900*                                                                         
018000 01    IMS-WS.                                                            
018100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
018200                                                                          
018300 01    NYCKLAR-TILL-DLI.                                                  
018400   03  W-WDP501KY-X.                                                      
018500     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
018600     05  W-IDDOKTYP              PIC X(8)    VALUE SPACE.                 
018700     05  W-IDDOK                 PIC X(8)    VALUE SPACE.                 
018800                                                                          
018900   03  W-IDSID-X.                                                         
019000     05  W-IDSID                 PIC S9(3)   VALUE ZERO  COMP-3.          
019100                                                                          
019200*                        **** STATUS-KOD FRÅN IMS                         
019300   03    STATUS-WS               PIC XX.                                  
019400     88    SEGMENT-FINNS                     VALUE '  '.                  
019500     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
019600     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
019610     88    NOT-AUTH                          VALUE 'A4'.                  
019620     88    TRANS-MISSING                     VALUE 'A1'.                  
019700                                                                          
019800   03    GODK-STATUSKODER.                                                
019900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
020000                                                                          
020100 01    SSA1                      PIC X(64).                               
020200 01    SSA2                      PIC X(64).                               
020300                                                                          
020400     EJECT                                                                
020500*                            IMS FUNKTIONSKODER                           
020600*01    -COPY W0003                                                        
020700                                                                          
020800     EJECT                                                                
020900*                            DLI INPUT-OUTPUT AREA                        
021000 01  FILLER                      PIC X(16)   VALUE 'WDP501  '.            
021100 01  DLI-IO-P501.                                                         
021200*  03  -COPY WDP501                                                       
021300                                                                          
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'WDP512  '.            
021600 01  DLI-IO-P512.                                                         
021700*  03  -COPY WDP512                                                       
021800                                                                          
021900     EJECT                                                                
022000 LINKAGE SECTION.                                                         
022100*01    -COPY W0009     -PRE MSG-                                          
022200*01    -COPY W0009     -PRE ALT-                                          
022300*01    -COPY W0008     -PRE WDP7-                                         
022400     05  FILLER                  PIC X.                                   
022500*01    -COPY W0008     -PRE WDP5-                                         
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800     EJECT                                                                
022900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDP5-PCB.             
023000 MAIN SECTION.                                                            
023100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDP5-PCB.             
023200                                                                          
023300     PERFORM IMS-GET-MSG                                                  
023400     IF SEGMENT-FINNS                                                     
023500       PERFORM A-INIT                                                     
023600       PERFORM B-KOLLA-NYCKLAR                                            
023700       IF MFS-IDPFK = '3'                                                 
023800         PERFORM I-AVSLUT                                                 
023900       ELSE                                                               
024000         IF NYCKLAR-OK                                                    
024100           IF MFS-UPDATE                                                  
024200             PERFORM C-UPPDATERA-SIDOR                                    
024300           ELSE                                                           
024400             PERFORM J-SPARA-INMED                                        
024500             IF MFS-SPLIT                                                 
024600               PERFORM D-SPLIT                                            
024700             ELSE                                                         
024800               IF MFS-FIRST                                               
024900                 PERFORM E-FIRST                                          
025000               ELSE                                                       
025100                 IF MFS-NEXT                                              
025200                   PERFORM F-NEXT                                         
025300                 ELSE                                                     
025400                   PERFORM G-ENTER                                        
025500                 END-IF                                                   
025600               END-IF                                                     
025700             END-IF                                                       
025800             IF NYCKLAR-OK                                                
025900               PERFORM H-LAS-SIDOR                                        
026000             END-IF                                                       
026100           END-IF                                                         
026200         END-IF                                                           
026300         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55301 + 4                    
026400         PERFORM IMS-INSERT-MSG                                           
026500       END-IF                                                             
026600     END-IF                                                               
026700     MOVE ZERO TO RETURN-CODE                                             
026800     GOBACK                                                               
026900     .                                                                    
027000                                                                          
027100     EJECT                                                                
027200 A-INIT SECTION.                                                          
027300     IF MSG-DUBBLA-TRANSKODER                                             
027400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I55301                 
027500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
027600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
027700       MOVE MSG-KDTRANS-2                 TO W-TRANSKOD                   
027800     ELSE                                                                 
027900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I55301                 
028000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
028100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
028200       STRING 'W' MFS-IDTRANS (1:1)                                       
028300              'T' MFS-IDTRANS (2:3) '  '                                  
028400              DELIMITED BY SIZE INTO W-TRANSKOD                           
028500       END-STRING                                                         
028600     END-IF                                                               
028700                                                                          
028800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
028900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
029000     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
029100                                                                          
029200     MOVE FUNCTION CURRENT-DATE (3:12)    TO W-CURRDATE                   
029300                                                                          
029400     MOVE LOW-VALUE                       TO MSG-AREA                     
029500     MOVE 'W0O55301'                      TO MFS-IDMOD                    
029600     MOVE '0553'                          TO MOD-IDTRANS                  
029700                                                                          
029800     MOVE MFS-RENSA-FAELT                 TO MOD-IDDOKTYP-IN              
029900                                             MOD-IDDOK-IN                 
030000                                             MOD-IDSID-IN                 
030100                                             MOD-TEMFSFEL                 
030200                                             MOD-TEMFSINF                 
030300     .                                                                    
030400                                                                          
030500     EJECT                                                                
030600 B-KOLLA-NYCKLAR SECTION.                                                 
030700                                                                          
030800     IF GODK-TRANS                                                        
030900       MOVE 'V'                     TO RTXT-KDTEXTTR                      
031000       IF MID-IDDOKTYP-IN =  ALL '+'                                      
031100         MOVE MID-IDDOKTYP-UT       TO IDDOKTYP-W                         
031200       ELSE                                                               
031300         MOVE MID-IDDOKTYP-IN       TO IDDOKTYP-W                         
031400         MOVE '7'                   TO MFS-IDPFK                          
031500       END-IF                                                             
031600       MOVE IDDOKTYP-W              TO RTXT-TETEXTTR                      
031700       CALL WTXTTR USING RTXT-WTXTAREA                                    
031800       MOVE RTXT-TETEXTTR           TO IDDOKTYP-W                         
031900       IF MID-IDDOK-IN =  ALL '+'                                         
032000         MOVE MID-IDDOK-UT          TO IDDOK-W                            
032100       ELSE                                                               
032200         MOVE MID-IDDOK-IN          TO IDDOK-W                            
032300         MOVE '7'                   TO MFS-IDPFK                          
032400       END-IF                                                             
032500       MOVE IDDOK-W                 TO RTXT-TETEXTTR                      
032600       CALL WTXTTR USING RTXT-WTXTAREA                                    
032700       MOVE RTXT-TETEXTTR           TO IDDOK-W                            
032800       IF EGEN-TRANS                                                      
032900         IF MID-IDSID-IN =  ALL '+'                                       
033000           MOVE MID-IDSID-UT        TO IDSID-W                            
033100           INSPECT IDSID-W REPLACING LEADING SPACE BY ZERO                
033200         ELSE                                                             
033300           MOVE MID-IDSID-IN        TO IDSID-W                            
033400                                       IDSID-W-IN                         
033500         END-IF                                                           
033600         IF IDSID-W NOT NUMERIC                                           
033700           MOVE '001'               TO IDSID-W                            
033800           MOVE '7'                 TO MFS-IDPFK                          
033900         END-IF                                                           
034000       ELSE                                                               
034100         MOVE SPACE                 TO MID-TEINFO                         
034200         MOVE '001'                 TO IDSID-W                            
034300         MOVE '7'                   TO MFS-IDPFK                          
034400       END-IF                                                             
034500     ELSE                                                                 
034600       MOVE SPACE                   TO MID-TEINFO                         
034700       MOVE '001'                   TO IDSID-W                            
034800     END-IF                                                               
034900                                                                          
035000     MOVE ALL '+'                   TO MSGI-WMSGINIT                      
035100     MOVE '001'                     TO MSGI-KDCALL                        
035200     MOVE MSG-SIGNON-USERID         TO MSGI-IDUSER                        
035300     MOVE MSG-LTERM-NAME            TO MSGI-IDLTERM-USER                  
035400     MOVE '0553'                    TO MSGI-IDTRANS                       
035500                                                                          
035600     IF GODK-TRANS                                                        
035700       MOVE IDDOKTYP-W              TO MOD-IDDOKTYP-UT                    
035800       MOVE IDDOK-W                 TO MOD-IDDOK-UT                       
035900       MOVE IDSID-W                 TO MOD-IDSID-UT                       
036000       MOVE MID-TEINFO              TO MOD-TEINFO                         
036100       IF IDDOKTYP-W = SPACE OR IDDOK-W = SPACE                           
036200         MOVE NEJ                   TO NYCKLAR                            
036300         MOVE FEL1 (2)              TO MOD-TEMFSFEL                       
036400       ELSE                                                               
036500         MOVE IDDOK-W TO IDDOK-FIX                                        
036600         PERFORM S01-FIXA-KEY                                             
036700         IF IDDOK-FIX NUMERIC AND IDDOK-FIX > ZERO                        
036800           MOVE IDDOK-FIX TO MSGI-IDARTNR                                 
036900         END-IF                                                           
037000       END-IF                                                             
037100     END-IF                                                               
037200     CALL  W005INIT USING MSGI-WMSGINIT WDP7-PCB                          
037300                                                                          
037400     IF MSGI-IDLAND-SPR = 'SE'                                            
037500       MOVE +1                      TO INDX                               
037600     ELSE                                                                 
037700       MOVE +2                      TO INDX                               
037800     END-IF                                                               
037900     MOVE 'S  '                     TO W-IDSKYLT                          
038000                                                                          
038100     IF MFS-UPDATE                                                        
038200       IF IDDOKTYP-W = 'USER    '                                         
038300         MOVE FEL6 (INDX)           TO MOD-TEMFSFEL                       
038400         MOVE NEJ                   TO NYCKLAR                            
038500       ELSE                                                               
038510         IF IDDOKTYP-W = 'VORNOT  '                                       
038520           MOVE FEL7 (INDX)         TO MOD-TEMFSFEL                       
038530           MOVE NEJ                 TO NYCKLAR                            
038540         ELSE                                                             
038600           IF MID-TEINFO = SPACE                                          
038700             IF MID-IDDOKTYP-IN = ALL '+'                                 
038800                 AND MID-IDDOK-IN = ALL '+'                               
038900               CONTINUE                                                   
039000             ELSE                                                         
039100               MOVE FEL1 (INDX)       TO MOD-TEMFSFEL                     
039200               MOVE NEJ               TO NYCKLAR                          
039300             END-IF                                                       
039400           END-IF                                                         
039500         END-IF                                                           
039510       END-IF                                                             
039600     END-IF                                                               
039700     .                                                                    
039800                                                                          
039900     EJECT                                                                
040000 C-UPPDATERA-SIDOR SECTION.                                               
040100                                                                          
040200     MOVE IDDOKTYP-W                TO W-IDDOKTYP                         
040300     MOVE IDDOK-W                   TO W-IDDOK                            
040400     MOVE IDSID-W                   TO W-IDSID                            
040500                                                                          
040600     MOVE IDDOK-W                   TO IDDOK-FIX                          
040700     PERFORM S01-FIXA-KEY                                                 
040800     PERFORM IMS-GHU-INFO                                                 
040900     IF SEGMENT-FINNS                                                     
041000       IF IDSID-W-IN = '000'                                              
041100         MOVE W-DATE                TO INFO-TIREGDAT                      
041200         MOVE W-TIME                TO INFO-TIREGTID                      
041300         MOVE MSG-SIGNON-USERID     TO INFO-IDUSER                        
041400         MOVE MID-TEINFO            TO INFO-BEDOK                         
041500         PERFORM IMS-REPL-INFO                                            
041600         MOVE MED7(INDX)            TO MOD-TEMFSINF                       
041700       END-IF                                                             
041800     ELSE                                                                 
041900       MOVE W-IDSKYLT               TO INFO-IDSKYLT                       
042000       MOVE W-IDDOKTYP              TO INFO-IDDOKTYP                      
042100       MOVE W-IDDOK                 TO INFO-IDDOK                         
042200       MOVE W-DATE                  TO INFO-TIREGDAT                      
042300       MOVE W-TIME                  TO INFO-TIREGTID                      
042400       MOVE MSG-SIGNON-USERID       TO INFO-IDUSER                        
042500       MOVE MID-TEINFO              TO INFO-BEDOK                         
042600       PERFORM IMS-ISRT-INFO                                              
042700       MOVE MED8(INDX)              TO MOD-TEMFSINF                       
042800     END-IF                                                               
042900     IF IDSID-W-IN = '000'                                                
043000       CONTINUE                                                           
043100     ELSE                                                                 
043200       IF W-IDSID = ZERO                                                  
043300         MOVE +1                    TO W-IDSID                            
043400       END-IF                                                             
043500       PERFORM IMS-GHU-SIDA                                               
043600       IF SEGMENT-FINNS                                                   
043700         IF MID-TEINFO = SPACE                                            
043800           PERFORM IMS-DLET-SIDA                                          
043900           MOVE MED5 (INDX)         TO MOD-TEMFSINF                       
044000           MOVE MID-TEINFO          TO TEXT-TEINFO                        
044100         ELSE                                                             
044200           IF MID-IDDOKTYP-IN = ALL '+'                                   
044300               AND MID-IDDOK-IN = ALL '+'                                 
044400             MOVE MSG-SIGNON-USERID TO TEXT-IDUSER                        
044500             MOVE W-DATE            TO TEXT-TIREGDAT                      
044600             MOVE W-TIME            TO TEXT-TIREGTID                      
044700             MOVE MID-TEINFO        TO TEXT-TEINFO                        
044800             PERFORM IMS-REPL-SIDA                                        
044900             MOVE MED3(INDX)        TO MOD-TEMFSINF                       
045000           ELSE                                                           
045100             MOVE FEL8 (INDX)       TO MOD-TEMFSFEL                       
045200             MOVE MID-TEINFO        TO TEXT-TEINFO                        
045300           END-IF                                                         
045400         END-IF                                                           
045500       ELSE                                                               
045600         IF MID-TEINFO = SPACE                                            
045700           MOVE MED5 (INDX)         TO MOD-TEMFSINF                       
045800           MOVE MID-TEINFO          TO TEXT-TEINFO                        
045900         ELSE                                                             
046000           MOVE W-IDSID             TO TEXT-IDSID                         
046100           MOVE MSG-SIGNON-USERID   TO TEXT-IDUSER                        
046200           MOVE W-DATE              TO TEXT-TIREGDAT                      
046300           MOVE W-TIME              TO TEXT-TIREGTID                      
046400           MOVE MID-TEINFO          TO TEXT-TEINFO                        
046500           PERFORM IMS-ISRT-SIDA                                          
046600           MOVE MED4(INDX)          TO MOD-TEMFSINF                       
046700         END-IF                                                           
046800       END-IF                                                             
046900     END-IF                                                               
047000     MOVE TEXT-TEINFO               TO MOD-TEINFO                         
047100     MOVE W-IDSID                   TO MOD-IDSID-UT                       
047200     .                                                                    
047300                                                                          
047400     EJECT                                                                
047500 D-SPLIT SECTION.                                                         
047600                                                                          
047700     IF 2106-TRANS                                                        
047710     OR 2406-TRANS                                                        
047800       MOVE 'ARTNOT'          TO IDDOKTYP-W                               
047900       MOVE MSGI-IDARTNR(2:8) TO IDDOK-W                                  
048000       INSPECT IDDOK-W REPLACING ALL '+'      BY SPACE                    
048100       INSPECT IDDOK-W REPLACING LEADING ZERO BY SPACE                    
048200     END-IF                                                               
048300     IF 2111-TRANS                                                        
048400       MOVE 'LEVNOT'          TO IDDOKTYP-W                               
048500       MOVE MSGI-IDLEVNR      TO IDDOK-W                                  
048600     END-IF                                                               
048700     IF 2311-TRANS                                                        
048701       MOVE MSGI-IDDC-KEY     TO W-IDDC-DOKTYP                            
048702       MOVE 'CAMPRE'          TO W-DOKTYP                                 
048710       MOVE W-IDDOKTYP-RED    TO IDDOKTYP-W                               
048711       MOVE ZERO              TO IDDOK-W (1:1)                            
048720       MOVE MSGI-IDKAMPRF     TO IDDOK-W (2:7)                            
048721       INSPECT IDDOK-W REPLACING ALL '+'      BY SPACE                    
048722       INSPECT IDDOK-W REPLACING LEADING ZERO BY SPACE                    
048730     END-IF                                                               
048731     IF 2313-TRANS                                                        
048732     OR 2314-TRANS                                                        
048733       MOVE 'CAMPNOT'         TO IDDOKTYP-W                               
048734       MOVE MSGI-IDKAMP       TO IDDOK-W                                  
048735     END-IF                                                               
048736     IF 2366-TRANS                                                        
048737       MOVE 'DESNOT'          TO IDDOKTYP-W                               
048738       MOVE MSGI-IDDC-KEY     TO IDDOK-W                                  
048739     END-IF                                                               
048740     IF 2367-TRANS                                                        
048750       MOVE 'STONOT'          TO IDDOKTYP-W                               
048760       MOVE MSGI-IDDC-KEY     TO IDDOK-W                                  
048770     END-IF                                                               
048800     MOVE ZERO              TO IDSID-W                                    
048900     MOVE IDDOKTYP-W        TO W-IDDOKTYP                                 
049000                               MOD-IDDOKTYP-UT                            
049100     MOVE IDDOK-W           TO W-IDDOK                                    
049200                               MOD-IDDOK-UT                               
049300     MOVE +1                TO W-IDSID                                    
049400     .                                                                    
049500                                                                          
049600     EJECT                                                                
049700 E-FIRST SECTION.                                                         
049800                                                                          
049900     MOVE IDDOKTYP-W      TO W-IDDOKTYP                                   
050000     MOVE IDDOK-W         TO W-IDDOK                                      
050100     MOVE +1              TO W-IDSID                                      
050200     .                                                                    
050300                                                                          
050400                                                                          
050500 F-NEXT SECTION.                                                          
050600                                                                          
050700     MOVE IDDOKTYP-W      TO W-IDDOKTYP                                   
050800     MOVE IDDOK-W         TO W-IDDOK                                      
050900     MOVE IDSID-W         TO W-IDSID                                      
051000     ADD +1               TO W-IDSID                                      
051100     .                                                                    
051200                                                                          
051300                                                                          
051400 G-ENTER SECTION.                                                         
051500                                                                          
051600     IF MID-TEINFO = SPACE                                                
051700         OR IDSID-W-IN = '000'                                            
051800       MOVE IDDOKTYP-W    TO W-IDDOKTYP                                   
051900       MOVE IDDOK-W       TO W-IDDOK                                      
052000       MOVE IDSID-W       TO W-IDSID                                      
052100     ELSE                                                                 
052200       MOVE FEL8(INDX)    TO MOD-TEMFSFEL                                 
052300       MOVE MID-TEINFO    TO MOD-TEINFO                                   
052400       MOVE NEJ           TO NYCKLAR                                      
052500     END-IF                                                               
052600     .                                                                    
052700                                                                          
052800     EJECT                                                                
052900 H-LAS-SIDOR SECTION.                                                     
053000                                                                          
053100     MOVE W-IDDOK TO IDDOK-FIX                                            
053200     PERFORM S01-FIXA-KEY                                                 
053300     PERFORM IMS-GU-INFO                                                  
053400     IF SEGMENT-FINNS                                                     
053500       PERFORM IMS-GNP-SIDA                                               
053600       IF SEGMENT-FINNS                                                   
053700         MOVE TEXT-TEINFO     TO MOD-TEINFO                               
053800         MOVE TEXT-IDSID      TO MOD-IDSID-UT                             
053900         IF IDSID-W-IN = '000'                                            
054000           MOVE INFO-BEDOK    TO MOD-TEMFSFEL                             
054100         END-IF                                                           
054200         PERFORM IMS-GNP-SIDA                                             
054300         IF SEGMENT-FINNS                                                 
054400           MOVE MED1(INDX)    TO MOD-TEMFSINF                             
054500         END-IF                                                           
054600       ELSE                                                               
054700         IF IDSID-W-IN = '000'                                            
054800           MOVE INFO-BEDOK    TO MOD-TEINFO                               
054900         ELSE                                                             
055000           MOVE SPACE         TO MOD-TEINFO                               
055100         END-IF                                                           
055200         MOVE W-IDSID         TO MOD-IDSID-UT                             
055300         MOVE FEL3(INDX)      TO MOD-TEMFSFEL                             
055400       END-IF                                                             
055500     ELSE                                                                 
055600       MOVE SPACE             TO MOD-TEINFO                               
055700       MOVE +1                TO MOD-IDSID-UT                             
055800       MOVE FEL3(INDX)        TO MOD-TEMFSFEL                             
055900     END-IF                                                               
056000     .                                                                    
056100                                                                          
056200     EJECT                                                                
056300 I-AVSLUT SECTION.                                                        
056400                                                                          
056500     MOVE 'USER'             TO W-IDDOKTYP                                
056600     MOVE MSG-LTERM-NAME     TO W-IDDOK                                   
056700     MOVE ZERO               TO W-IDSID                                   
056800     PERFORM IMS-GU-INFO                                                  
056900     IF SEGMENT-FINNS                                                     
057000       PERFORM IMS-GNP-SIDA                                               
057100       PERFORM IMS-GHU-INFO                                               
057200       PERFORM IMS-DLET-INFO                                              
057300                                                                          
057400       MOVE TEXT-IDUSER      TO MSG-KDTRANS-1                             
057500       MOVE '0553'           TO MSG-IDTRANS-1                             
057600       MOVE MFS-KDMFSFOR     TO MSG-KDMFSFOR-1                            
057700       MOVE TEXT-TEINFO      TO MSG-INDATA-MINUS-1-TRANSKOD               
057800       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55301 + 4                      
057900       PERFORM IMS-ALT-CHNG                                               
057910       IF NOT-AUTH OR TRANS-MISSING                                       
057911         MOVE FEL6 (INDX)      TO MOD-TEMFSFEL                            
057912         MOVE MFS-RENSA-FAELT  TO MOD-IDDOKTYP-UT                         
057913                                  MOD-IDDOK-UT                            
057914                                  MOD-IDSID-UT                            
057915                                  MOD-TEINFO                              
057916         COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55301 + 4                    
057917         PERFORM IMS-INSERT-MSG                                           
057920       ELSE                                                               
058000         PERFORM IMS-ALT-ISRT                                             
058010       END-IF                                                             
058100     ELSE                                                                 
058200       MOVE FEL3 (INDX)      TO MOD-TEMFSFEL                              
058300       MOVE MFS-RENSA-FAELT  TO MOD-IDDOKTYP-UT                           
058400                                MOD-IDDOK-UT                              
058500                                MOD-IDSID-UT                              
058600                                MOD-TEINFO                                
058700       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55301 + 4                      
058800       PERFORM IMS-INSERT-MSG                                             
058900     END-IF                                                               
059000     .                                                                    
059100                                                                          
059200     EJECT                                                                
059300 J-SPARA-INMED SECTION.                                                   
059400                                                                          
059500     IF NOT GODK-TRANS                                                    
059600       MOVE W-IDSKYLT            TO INFO-IDSKYLT                          
059700       MOVE 'USER'               TO INFO-IDDOKTYP                         
059800                                    W-IDDOKTYP                            
059900       MOVE MSG-LTERM-NAME       TO INFO-IDDOK                            
060000                                    W-IDDOK                               
060100       MOVE W-DATE               TO INFO-TIREGDAT                         
060200       MOVE W-TIME               TO INFO-TIREGTID                         
060300       MOVE W-IDTRANS            TO INFO-IDUSER                           
060400       MOVE SPACE                TO INFO-BEDOK                            
060500                                    INFO-FILLER                           
060600       PERFORM IMS-ISRT-INFO                                              
060700       IF SEGMENT-FINNS-REDAN                                             
060800         PERFORM IMS-GHU-INFO                                             
060900         MOVE W-DATE             TO INFO-TIREGDAT                         
061000         MOVE W-TIME             TO INFO-TIREGTID                         
061100         MOVE W-IDTRANS          TO INFO-IDUSER                           
061200         MOVE SPACE              TO INFO-BEDOK                            
061300                                    INFO-FILLER                           
061400         PERFORM IMS-REPL-INFO                                            
061500       END-IF                                                             
061600       MOVE +1                   TO TEXT-IDSID                            
061700                                    W-IDSID                               
061800       MOVE W-TRANSKOD           TO TEXT-IDUSER                           
061900       MOVE W-DATE               TO TEXT-TIREGDAT                         
062000       MOVE W-TIME               TO TEXT-TIREGTID                         
062100       MOVE MID-TEINFO           TO TEXT-TEINFO                           
062200       PERFORM IMS-ISRT-SIDA                                              
062300       IF SEGMENT-FINNS-REDAN                                             
062400         PERFORM IMS-GHU-SIDA                                             
062500         MOVE W-TRANSKOD         TO TEXT-IDUSER                           
062600         MOVE W-DATE             TO TEXT-TIREGDAT                         
062700         MOVE W-TIME             TO TEXT-TIREGTID                         
062800         MOVE MID-TEINFO         TO TEXT-TEINFO                           
062900         PERFORM IMS-REPL-SIDA                                            
063000       END-IF                                                             
063100                                                                          
063200       MOVE 'TRANS'              TO W-IDDOKTYP                            
063300       MOVE W-TRANSKOD           TO W-IDDOK                               
063400       MOVE +1                   TO W-IDSID                               
063500     END-IF                                                               
063600     .                                                                    
063700                                                                          
063800     EJECT                                                                
063900 S01-FIXA-KEY SECTION.                                                    
064000                                                                          
064100     IF IDDOKTYP-W = 'ARTNOT  ' OR 'LOSNOT  ' OR 'VORNOT'                 
064200       MOVE ZERO                    TO W-FIX                              
064300       PERFORM UNTIL IDDOK-FIX (9:1) NOT = SPACE                          
064400         MOVE IDDOK-FIX (1:8)       TO W-FIX (2:8)                        
064500         MOVE W-FIX                 TO IDDOK-FIX                          
064600       END-PERFORM                                                        
064700       MOVE IDDOK-FIX (2:8)         TO W-IDDOK                            
064800       INSPECT W-IDDOK REPLACING LEADING ZERO BY SPACE                    
064900     END-IF                                                               
065000     .                                                                    
065100                                                                          
065200     EJECT                                                                
065300* IMS SEKTIONER                                                           
065400                                                                          
065500 IMS-GET-MSG SECTION.                                                     
065600                                                                          
065700     MOVE '  QC' TO GODK-STATUSKODER                                      
065800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
065900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200                                                                          
066300                                                                          
066400 IMS-INSERT-MSG SECTION.                                                  
066500                                                                          
066600     IF ENGLISH-TEXT                                                      
066700       MOVE 'N' TO MFS-KDHUVOMR                                           
066800     END-IF                                                               
066900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
067000     MOVE SPACE TO GODK-STATUSKODER                                       
067100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
067200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500                                                                          
067600     EJECT                                                                
067700 IMS-ALT-ISRT SECTION.                                                    
067800                                                                          
067900     IF ENGLISH-TEXT                                                      
068000       MOVE 'N' TO MFS-KDHUVOMR                                           
068100     END-IF                                                               
068200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
068300     MOVE SPACE TO GODK-STATUSKODER                                       
068400     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
068500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
068600     PERFORM IMS-STATUSKONTROLL                                           
068700     .                                                                    
068800                                                                          
068900 IMS-ALT-CHNG SECTION.                                                    
069000                                                                          
069100     MOVE 'A1A4  ' TO GODK-STATUSKODER                                    
069200     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
069300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600                                                                          
069700     EJECT                                                                
069800 IMS-GU-INFO SECTION.                                                     
069900                                                                          
070000     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
070100            DELIMITED BY SIZE INTO SSA1                                   
070200     MOVE '  GE' TO GODK-STATUSKODER                                      
070300     CALL CBLTDLI USING GU  WDP5-PCB DLI-IO-P501 SSA1                     
070400     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     .                                                                    
070700                                                                          
070800                                                                          
070900 IMS-GNP-SIDA SECTION.                                                    
071000     STRING 'WDP512  (IDSID   =>' W-IDSID-X ')'                           
071100            DELIMITED BY SIZE INTO SSA1                                   
071200     MOVE '  GE' TO GODK-STATUSKODER                                      
071300     CALL CBLTDLI USING GNP WDP5-PCB DLI-IO-P512 SSA1                     
071400     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700                                                                          
071800     EJECT                                                                
071900 IMS-GHU-INFO SECTION.                                                    
072000                                                                          
072100     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
072200            DELIMITED BY SIZE INTO SSA1                                   
072300     MOVE '  GE' TO GODK-STATUSKODER                                      
072400     CALL CBLTDLI USING GHU  WDP5-PCB DLI-IO-P501 SSA1                    
072500     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800                                                                          
072900                                                                          
073000 IMS-ISRT-INFO SECTION.                                                   
073100                                                                          
073200     MOVE 'WDP501   ' TO SSA1                                             
073300     MOVE '  II' TO GODK-STATUSKODER                                      
073400     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-P501 SSA1                    
073500     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800                                                                          
073900                                                                          
074000 IMS-REPL-INFO SECTION.                                                   
074100                                                                          
074200     MOVE '  ' TO GODK-STATUSKODER                                        
074300     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-P501                         
074400     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700                                                                          
074800                                                                          
074900 IMS-DLET-INFO SECTION.                                                   
075000                                                                          
075100     MOVE '  ' TO GODK-STATUSKODER                                        
075200     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-P501                         
075300     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSKONTROLL                                           
075500     .                                                                    
075600                                                                          
075700     EJECT                                                                
075800 IMS-GHU-SIDA SECTION.                                                    
075900                                                                          
076000     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
076100            DELIMITED BY SIZE INTO SSA1                                   
076200     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
076300            DELIMITED BY SIZE INTO SSA2                                   
076400     MOVE '  GE' TO GODK-STATUSKODER                                      
076500     CALL CBLTDLI USING GHU  WDP5-PCB DLI-IO-P512 SSA1 SSA2               
076600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
076700     PERFORM IMS-STATUSKONTROLL                                           
076800     .                                                                    
076900                                                                          
077000                                                                          
077100 IMS-ISRT-SIDA SECTION.                                                   
077200                                                                          
077300     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
077400            DELIMITED BY SIZE INTO SSA1                                   
077500     MOVE 'WDP512   ' TO SSA2                                             
077600     MOVE '  II' TO GODK-STATUSKODER                                      
077700     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-P512 SSA1 SSA2               
077800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100                                                                          
078200                                                                          
078300 IMS-REPL-SIDA SECTION.                                                   
078400                                                                          
078500     MOVE '  ' TO GODK-STATUSKODER                                        
078600     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-P512                         
078700     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000                                                                          
079100                                                                          
079200 IMS-DLET-SIDA SECTION.                                                   
079300                                                                          
079400     MOVE '  ' TO GODK-STATUSKODER                                        
079500     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-P512                         
079600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
079700     PERFORM IMS-STATUSKONTROLL                                           
079800     .                                                                    
079900                                                                          
080000     EJECT                                                                
080100 IMS-STATUSKONTROLL SECTION.                                              
080200                                                                          
080300     SET STATUS-IX TO 1                                                   
080400     SEARCH GODK-STATUS                                                   
080500       AT END                                                             
080600         CALL FELLOG                                                      
080700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
080800         CONTINUE                                                         
080900     END-SEARCH                                                           
081000     .                                                                    
