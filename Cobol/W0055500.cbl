000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0055500.                                                
000300 AUTHOR.         THOMAS NILSSON.                                          
000400 DATE-WRITTEN.   DEC  89.                                                 
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        HELP FÖR IMS PROGRAM. KOPIERA/TA BORT DOKUMENT ELLER             
001000*        SIDOR                                                            
001100*        FÖR IMS-BILDER, ALLMÄN SYSTEM INFO.                              
001200*    INDATA.                                                              
001300*        TRANSAKTION: W0T555                                              
001400*                     W0T555U                                             
001500*        MID:         W0I55501                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W0O55501                                            
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77    IDPGM                     PIC X(8)    VALUE 'W0055500'.            
002700 77    JA                        PIC X       VALUE 'J'.                   
002800 77    YES                       PIC X       VALUE 'Y'.                   
002900 77    NEJ                       PIC X       VALUE 'N'.                   
003000 77    RETT                      PIC X       VALUE 'R'.                   
003100 77    FEL                       PIC X       VALUE 'F'.                   
003200 77    KOPIERA                   PIC X       VALUE 'N'.                   
003300 77    IDDOKTYP-W                PIC X(8)    VALUE SPACE.                 
003400 77    IDDOK-W                   PIC X(8)    VALUE SPACE.                 
003500 77    IDDOKTYP-COPY             PIC X(8)    VALUE SPACE.                 
003600 77    IDDOK-COPY                PIC X(8)    VALUE SPACE.                 
003700 77    IDSID-COPY-FOM            PIC X(3).                                
003800 77    IDSID-COPY-TOM            PIC X(3).                                
003900 77    IDSID-NY-FOM              PIC X(3).                                
004000 77    IDSID-NY-TOM              PIC X(3).                                
004100 77    IDSID-DEL-FOM             PIC X(3).                                
004200 77    IDSID-DEL-TOM             PIC X(3).                                
004300 77    W-IDSID-COPY-FOM          PIC 9(3)    VALUE ZERO.                  
004400 77    W-IDSID-COPY-TOM          PIC 9(3)    VALUE ZERO.                  
004500 77    W-IDSID-NY-FOM            PIC 9(3)    VALUE ZERO.                  
004600 77    W-IDSID-NY-TOM            PIC 9(3)    VALUE ZERO.                  
004700 77    W-IDSID-DEL-FOM           PIC 9(3)    VALUE ZERO.                  
004800 77    W-IDSID-DEL-TOM           PIC 9(3)    VALUE ZERO.                  
004900 77    INDX                      PIC S9(4)   VALUE ZERO.                  
005000 77    NYCKLAR                   PIC X.                                   
005100     88  NYCKLAR-OK                          VALUE 'R'.                   
005200     88  NYCKLAR-FEL                         VALUE 'F'.                   
005300 77    INDATA                    PIC X.                                   
005400     88  INDATA-OK                           VALUE 'R'.                   
005500     88  INDATA-FEL                          VALUE 'F'.                   
005600 77    NY-NYCKLAR                PIC X.                                   
005700     88  NYCKLAR-NYA                         VALUE 'J'.                   
005800 77    IDTRANS                   PIC X(4).                                
005900     88  EGEN-TRANS                          VALUE '0555'.                
006000     88  IDTRANS-OK                          VALUE '0551' '0552'          
006100                                                   '0553' '0555'.         
006200     EJECT                                                                
006300                                                                          
006400 01  TIDEN                       PIC S9(9).                               
006500 01  FILLER REDEFINES TIDEN.                                              
006600     03  TID                     PIC S9(7).                               
006700     03  FILLER                  PIC XX.                                  
006800                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200                                                                          
007300     EJECT                                                                
007400 01    MEDDELANDE.                                                        
007500     03  MED-1.                                                           
007600         05 FILLER               PIC X(32)   VALUE                        
007700             'DOKUMENT BORTTAGET              '.                          
007800         05 FILLER               PIC X(32)   VALUE                        
007900             'DOKUMENT DELETED                '.                          
008000     03  FILLER REDEFINES MED-1.                                          
008100         05 MED1                 PIC X(32)   OCCURS 2.                    
008200                                                                          
008300     03  MED-2.                                                           
008400         05 FILLER               PIC X(32)   VALUE                        
008500             'SIDORNA BORTTAGNA               '.                          
008600         05 FILLER               PIC X(32)   VALUE                        
008700             'PAGES HAS BEEN DELETED          '.                          
008800     03  FILLER REDEFINES MED-2.                                          
008900         05 MED2                 PIC X(32)   OCCURS 2.                    
009000                                                                          
009100     03  MED-3.                                                           
009200         05 FILLER               PIC X(32)   VALUE                        
009300             'SIDORNA KOPIERADE               '.                          
009400         05 FILLER               PIC X(32)   VALUE                        
009500             'PAGES HAS BEEN COPIED           '.                          
009600     03  FILLER REDEFINES MED-3.                                          
009700         05 MED3                 PIC X(32)   OCCURS 2.                    
009800                                                                          
009900     03  FEL-1.                                                           
010000         05 FILLER               PIC X(32)   VALUE                        
010100             'NYCKLAR FELAKTIGA               '.                          
010200         05 FILLER               PIC X(32)   VALUE                        
010300             'WRONG KEYS                      '.                          
010400     03  FILLER REDEFINES FEL-1.                                          
010500         05 FEL1                 PIC X(32)   OCCURS 2.                    
010600                                                                          
010700     03  FEL-2.                                                           
010800         05 FILLER               PIC X(40)   VALUE                        
010900             'UPPLYSTA FÄLT FEL                       '.                  
011000         05 FILLER               PIC X(40)   VALUE                        
011100             'HIGHLIGHTED FIELDS WRONG                '.                  
011200     03  FILLER REDEFINES FEL-2.                                          
011300         05 FEL2                 PIC X(40)   OCCURS 2.                    
011400                                                                          
011500     03  FEL-3.                                                           
011600         05 FILLER               PIC X(32)   VALUE                        
011700             'INFORMATION SAKNAS              '.                          
011800         05 FILLER               PIC X(32)   VALUE                        
011900             'INFORMATION MISSING             '.                          
012000     03  FILLER REDEFINES FEL-3.                                          
012100         05 FEL3                 PIC X(32)   OCCURS 2.                    
012200                                                                          
012300     03  FEL-4.                                                           
012400         05 FILLER               PIC X(40)   VALUE                        
012500             'BÅDE FR.O.M OCH T.O.M SKALL ANGES'.                         
012600         05 FILLER               PIC X(40)   VALUE                        
012700             'BOTH F.O.M AND T.O.M IS REQUIRED'.                          
012800     03  FILLER REDEFINES FEL-4.                                          
012900         05 FEL4                 PIC X(40)   OCCURS 2.                    
013000                                                                          
013100     03  FEL-5.                                                           
013200         05 FILLER               PIC X(32)   VALUE                        
013300             'EJ NYA NYCKLAR OCH UPPDATERING'.                            
013400         05 FILLER               PIC X(32)   VALUE                        
013500             'NOT NEW KEYS AND UPDATE       '.                            
013600     03  FILLER REDEFINES FEL-5.                                          
013700         05 FEL5                 PIC X(32)   OCCURS 2.                    
013800                                                                          
013900     03  FEL-7.                                                           
014000         05 FILLER               PIC X(32)   VALUE                        
014100             'OBEHÖRIG ANVÄNDARE            '.                            
014200         05 FILLER               PIC X(32)   VALUE                        
014300             'USER NOT AUTHORIZED           '.                            
014400     03  FILLER REDEFINES FEL-7.                                          
014500         05 FEL7                 PIC X(32)   OCCURS 2.                    
014600                                                                          
014700     03  FEL-9.                                                           
014800         05 FILLER               PIC X(32)   VALUE                        
014900             'TRYCK PF11 FÖR UPPDATERING    '.                            
015000         05 FILLER               PIC X(32)   VALUE                        
015100             'PRESS PF11 FOR UPDATE         '.                            
015200     03  FILLER REDEFINES FEL-9.                                          
015300         05 FEL9                 PIC X(32)   OCCURS 2.                    
015400                                                                          
015500     EJECT                                                                
015600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
015800                                                                          
015900*01    MID -COPY W0I55501                                                 
016000                                                                          
016100     EJECT                                                                
016200*01    -COPY WMSGAREA                                                     
016300                                                                          
016400     EJECT                                                                
016500*  03    MOD -COPY W0O55501           -RED MSG-AREA.                      
016600                                                                          
016700     EJECT                                                                
016800*01    -COPY WMFSAREA                                                     
016900                                                                          
017000     EJECT                                                                
017100******************************************************************        
017200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300*                                                                         
017400 01    IMS-WS.                                                            
017500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
017600                                                                          
017700 01    NYCKLAR-TILL-DLI.                                                  
017800   03  W-WDP501KY-X.                                                      
017900     05    W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
018000     05    W-IDDOKTYP            PIC X(8)    VALUE SPACE.                 
018100     05    W-IDDOK               PIC X(8)    VALUE SPACE.                 
018200   03  W-WDP501KY-X-COPY.                                                 
018300     05    W-IDSKYLT-COPY        PIC X(3)    VALUE SPACE.                 
018400     05    W-IDDOKTYP-COPY       PIC X(8)    VALUE SPACE.                 
018500     05    W-IDDOK-COPY          PIC X(8)    VALUE SPACE.                 
018600   03  W-IDUSER-X.                                                        
018700     05    W-IDUSER              PIC X(8)    VALUE SPACE.                 
018800   03  W-IDSID-X.                                                         
018900     05    W-IDSID               PIC S9(3)   VALUE ZERO  COMP-3.          
019000   03  W-IDSID-X-COPY.                                                    
019100     05    W-IDSID-COPY          PIC S9(3)   VALUE ZERO  COMP-3.          
019200                                                                          
019300*                        **** STATUS-KOD FRÅN IMS                         
019400   03    STATUS-WS               PIC XX.                                  
019500     88    SEGMENT-FINNS                     VALUE '  '.                  
019600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
019700     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
019800                                                                          
019900   03    GODK-STATUSKODER.                                                
020000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
020100                                                                          
020200 01    SSA1                      PIC X(64).                               
020300 01    SSA2                      PIC X(64).                               
020400                                                                          
020500     EJECT                                                                
020600*                            IMS FUNKTIONSKODER                           
020700*01    -COPY W0003                                                        
020800                                                                          
020900     EJECT                                                                
021000*                            DLI INPUT-OUTPUT AREA                        
021100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-P501-1'.           
021200 01    DLI-IO-P501-1.                                                     
021300*  03  -COPY WDP501                                                       
021400                                                                          
021500     EJECT                                                                
021600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-P512-1'.           
021700 01    DLI-IO-P512-1.                                                     
021800*  03  -COPY WDP512                                                       
021900                                                                          
022000     EJECT                                                                
022100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-P501-2'.           
022200 01    DLI-IO-P501-2.                                                     
022300*  03  -COPY WDP501 -PRE 2-                                               
022400                                                                          
022500     EJECT                                                                
022600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-P512-2'.           
022700 01    DLI-IO-P512-2.                                                     
022800*  03  -COPY WDP512 -PRE 2-                                               
022900                                                                          
023000     EJECT                                                                
023100 LINKAGE SECTION.                                                         
023200*01    -COPY W0009     -PRE MSG-                                          
023300*01    -COPY W0008     -PRE WDP5-                                         
023400     05  FILLER                  PIC X.                                   
023500                                                                          
023600     EJECT                                                                
023700*01    -COPY W0008     -PRE WDP52-                                        
023800     05  FILLER                  PIC X.                                   
023900                                                                          
024000     EJECT                                                                
024100 PROCEDURE DIVISION USING MSG-PCB WDP5-PCB WDP52-PCB.                     
024200 MAIN SECTION.                                                            
024300     ENTRY 'DLITCBL' USING MSG-PCB WDP5-PCB WDP52-PCB.                    
024400                                                                          
024500     PERFORM IMS-GET-MSG                                                  
024600     IF SEGMENT-FINNS                                                     
024700       PERFORM A-INIT                                                     
024800       PERFORM B-KOLLA-NYCKLAR                                            
024900       IF NYCKLAR-OK                                                      
025000         IF MFS-UPDATE                                                    
025100           PERFORM F-KOLLA-INPUT                                          
025200           IF INDATA-OK                                                   
025300             PERFORM C-UPPDATERA-DOKUMENT                                 
025400           END-IF                                                         
025500         ELSE                                                             
025600           PERFORM E-LAS-DOKUMENT                                         
025700         END-IF                                                           
025800       END-IF                                                             
025900       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O55501 + 4                      
026000       PERFORM IMS-INSERT-MSG                                             
026100     END-IF                                                               
026200     MOVE ZERO TO RETURN-CODE                                             
026300     GOBACK                                                               
026400     .                                                                    
026500                                                                          
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800     IF MSG-DUBBLA-TRANSKODER                                             
026900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I55501                 
027000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
027100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
027200     ELSE                                                                 
027300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I55501                 
027400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
027500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
027600     END-IF                                                               
027700     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
027800     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
027900     MOVE MFS-IDTRANS                     TO IDTRANS                      
028000     MOVE NEJ                             TO NY-NYCKLAR                   
028100     MOVE RETT                            TO NYCKLAR                      
028200     MOVE RETT                            TO INDATA                       
028300                                                                          
028400     MOVE LOW-VALUE                       TO MSG-AREA                     
028500     MOVE 'W0O55501'                      TO MFS-IDMOD                    
028600     MOVE '0555'                          TO MOD-IDTRANS                  
028700     IF SWEDISH-TEXT                                                      
028800       MOVE +1                            TO INDX                         
028900     ELSE                                                                 
029000       MOVE +2                            TO INDX                         
029100     END-IF                                                               
029200     MOVE 'S  '                           TO W-IDSKYLT                    
029300                                             W-IDSKYLT-COPY               
029400     IF NOT EGEN-TRANS                                                    
029500       MOVE SPACE                         TO MFS-KDTRTYP                  
029600       MOVE '7'                           TO MFS-IDPFK                    
029700     END-IF                                                               
029800                                                                          
029900     MOVE MFS-RENSA-FAELT                 TO MOD-IDDOKTYP-IN              
030000                                             MOD-IDDOK-IN                 
030100                                             MOD-TEMFSFEL                 
030200                                             MOD-TEMFSINF                 
030300     .                                                                    
030400                                                                          
030500 B-KOLLA-NYCKLAR SECTION.                                                 
030600                                                                          
030700     IF NOT IDTRANS-OK                                                    
030800       MOVE MFS-RENSA-FAELT      TO MOD-IDDOKTYP-UT                       
030900                                    MOD-IDDOK-UT                          
031000                                    MOD-IDDOKTYP-COPY-IN                  
031100                                    MOD-IDDOKTYP-COPY-UT                  
031200                                    MOD-IDDOK-COPY-IN                     
031300                                    MOD-IDDOK-COPY-UT                     
031400                                    MOD-IDSID-COPY-FOM-IN                 
031500                                    MOD-IDSID-COPY-FOM-UT                 
031600                                    MOD-IDSID-COPY-TOM-IN                 
031700                                    MOD-IDSID-COPY-TOM-UT                 
031800                                    MOD-IDSID-DEL-FOM-IN                  
031900                                    MOD-IDSID-DEL-FOM-UT                  
032000                                    MOD-IDSID-DEL-TOM-IN                  
032100                                    MOD-IDSID-DEL-TOM-UT                  
032200                                    MOD-IDSID-NY-FOM-IN                   
032300                                    MOD-IDSID-NY-FOM-UT                   
032400                                    MOD-IDSID-NY-TOM-IN                   
032500                                    MOD-IDSID-NY-TOM-UT                   
032600       MOVE FEL                  TO NYCKLAR                               
032700     ELSE                                                                 
032800       IF MID-IDDOKTYP-IN = ALL '+'                                       
032900         MOVE MID-IDDOKTYP-UT    TO IDDOKTYP-W                            
033000       ELSE                                                               
033100         MOVE MID-IDDOKTYP-IN    TO IDDOKTYP-W                            
033200         MOVE JA                 TO NY-NYCKLAR                            
033300       END-IF                                                             
033400                                                                          
033500       IF MID-IDDOK-IN = ALL '+'                                          
033600         MOVE MID-IDDOK-UT       TO IDDOK-W                               
033700       ELSE                                                               
033800         MOVE MID-IDDOK-IN       TO IDDOK-W                               
033900         MOVE JA                 TO NY-NYCKLAR                            
034000       END-IF                                                             
034100       IF NYCKLAR-FEL                                                     
034200         MOVE FEL1(INDX)         TO MOD-TEMFSFEL                          
034300       ELSE                                                               
034400         MOVE IDDOKTYP-W         TO MOD-IDDOKTYP-UT                       
034500         MOVE IDDOK-W            TO MOD-IDDOK-UT                          
034600       END-IF                                                             
034700     END-IF                                                               
034800     .                                                                    
034900                                                                          
035000     EJECT                                                                
035100 C-UPPDATERA-DOKUMENT SECTION.                                            
035200                                                                          
035300     IF NYCKLAR-NYA                                                       
035400       MOVE FEL5(INDX)               TO MOD-TEMFSFEL                      
035500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDDOKTYP-COPY-IN              
035600                                        MOD-IDDOK-COPY-IN                 
035700                                        MOD-IDSID-COPY-FOM-IN             
035800                                        MOD-IDSID-COPY-TOM-IN             
035900                                        MOD-IDSID-DEL-FOM-IN              
036000                                        MOD-IDSID-DEL-TOM-IN              
036100                                        MOD-IDSID-NY-FOM-IN               
036200                                        MOD-IDSID-NY-TOM-IN               
036300     ELSE                                                                 
036400       MOVE IDDOKTYP-W               TO W-IDDOKTYP                        
036500       MOVE IDDOK-W                  TO W-IDDOK                           
036600       IF MID-FLBORT = JA OR YES                                          
036700         PERFORM IMS-GHU-INFO                                             
036800         IF SEGMENT-SAKNAS                                                
036900           MOVE FEL1(INDX)           TO MOD-TEMFSFEL                      
037000         ELSE                                                             
037100           PERFORM IMS-DLET-INFO                                          
037200           MOVE MED1(INDX)           TO MOD-TEMFSINF                      
037300         END-IF                                                           
037400       ELSE                                                               
037500         IF W-IDSID-DEL-FOM > ZERO                                        
037600           IF W-IDSID-DEL-TOM < 1                                         
037700             MOVE W-IDSID-DEL-FOM    TO W-IDSID-DEL-TOM                   
037800           END-IF                                                         
037900           MOVE W-IDSID-DEL-FOM      TO W-IDSID                           
038000           PERFORM IMS-GHU-SIDA                                           
038100           PERFORM UNTIL W-IDSID > W-IDSID-DEL-TOM                        
038200             IF SEGMENT-FINNS                                             
038300               PERFORM IMS-DLET-SIDA                                      
038400               MOVE MED2(INDX)       TO MOD-TEMFSINF                      
038500             END-IF                                                       
038600             ADD +1                  TO W-IDSID                           
038700             PERFORM IMS-GHU-SIDA                                         
038800           END-PERFORM                                                    
038900         END-IF                                                           
039000       END-IF                                                             
039100     EJECT                                                                
039200       IF KOPIERA = JA                                                    
039300         IF      W-IDSID-NY-FOM > ZERO                                    
039400             AND W-IDSID-NY-TOM > ZERO                                    
039500           MOVE W-IDSID-NY-FOM       TO W-IDSID                           
039600           PERFORM IMS-GU-INFO                                            
039700           IF SEGMENT-FINNS                                               
039800             PERFORM IMS-GNP-SIDA                                         
039900             PERFORM UNTIL W-IDSID > W-IDSID-NY-FOM                       
040000               IF SEGMENT-SAKNAS                                          
040100                 ADD +1              TO W-IDSID                           
040200                 PERFORM IMS-GNP-SIDA                                     
040300               ELSE                                                       
040400                 MOVE +999           TO W-IDSID                           
040500                 MOVE FEL9(INDX)     TO MOD-TEMFSFEL                      
040600                 MOVE FEL            TO KOPIERA                           
040700               END-IF                                                     
040800             END-PERFORM                                                  
040900           END-IF                                                         
041000           IF KOPIERA = JA                                                
041100             MOVE IDDOKTYP-COPY      TO W-IDDOKTYP-COPY                   
041200             MOVE IDDOK-COPY         TO W-IDDOK-COPY                      
041300             MOVE IDSID-COPY-FOM     TO W-IDSID-COPY                      
041400             PERFORM IMS-GU-INFO-COPY                                     
041500             IF SEGMENT-FINNS                                             
041600               PERFORM IMS-GHU-INFO                                       
041700               IF SEGMENT-SAKNAS                                          
041800                 MOVE W-IDDOKTYP     TO INFO-IDDOKTYP                     
041900                 MOVE W-IDDOK        TO INFO-IDDOK                        
042000                 MOVE MSG-SIGNON-USERID TO INFO-IDUSER                    
042100                 ACCEPT INFO-TIREGDAT   FROM DATE                         
042200                 ACCEPT TIDEN           FROM TIME                         
042300                 MOVE TID            TO INFO-TIREGTID                     
042400                 MOVE SPACE          TO INFO-FILLER                       
042500                 MOVE 2-INFO-BEDOK   TO INFO-BEDOK                        
042600                 MOVE 2-INFO-IDSKYLT TO INFO-IDSKYLT                      
042700                 PERFORM IMS-ISRT-INFO                                    
042800               END-IF                                                     
042900               PERFORM IMS-GNP-SIDA-COPY                                  
043000               PERFORM UNTIL W-IDSID-COPY > W-IDSID-COPY-TOM              
043100                 IF SEGMENT-FINNS                                         
043200                   MOVE 2-TEXT-TEINFO     TO TEXT-TEINFO                  
043300                   MOVE MSG-SIGNON-USERID TO TEXT-IDUSER                  
043400                   ACCEPT TEXT-TIREGDAT   FROM DATE                       
043500                   ACCEPT TIDEN           FROM TIME                       
043600                   MOVE TID               TO TEXT-TIREGTID                
043700                   MOVE W-IDSID-NY-FOM    TO TEXT-IDSID                   
043800                   PERFORM IMS-ISRT-SIDA                                  
043900                   MOVE MED3(INDX)        TO MOD-TEMFSINF                 
044000                 END-IF                                                   
044100                 ADD +1                   TO W-IDSID-COPY                 
044200                                             W-IDSID-NY-FOM               
044300                 PERFORM IMS-GNP-SIDA-COPY                                
044400               END-PERFORM                                                
044500             ELSE                                                         
044600               MOVE FEL3(INDX)            TO MOD-TEMFSFEL                 
044700             END-IF                                                       
044800           END-IF                                                         
044900         ELSE                                                             
045000           MOVE FEL4(INDX)                TO MOD-TEMFSFEL                 
045100         END-IF                                                           
045200       END-IF                                                             
045300     END-IF                                                               
045400     .                                                                    
045500                                                                          
045600     EJECT                                                                
045700 E-LAS-DOKUMENT SECTION.                                                  
045800                                                                          
045900     MOVE IDDOKTYP-W           TO W-IDDOKTYP                              
046000     MOVE IDDOK-W              TO W-IDDOK                                 
046100     PERFORM IMS-GU-INFO                                                  
046200     IF SEGMENT-SAKNAS                                                    
046300       MOVE FEL3(INDX)        TO MOD-TEMFSFEL                             
046400     END-IF                                                               
046500     IF MID-IDDOKTYP-COPY-IN      =  ALL '+' AND                          
046600        MID-IDDOK-COPY-IN         =  ALL '+' AND                          
046700        MID-IDSID-COPY-FOM-IN     =  ALL '+' AND                          
046800        MID-IDSID-COPY-TOM-IN     =  ALL '+' AND                          
046900        MID-IDSID-DEL-FOM-IN      =  ALL '+' AND                          
047000        MID-IDSID-DEL-TOM-IN      =  ALL '+' AND                          
047100        MID-IDSID-NY-FOM-IN       =  ALL '+'                              
047200       MOVE MFS-RENSA-FAELT      TO MOD-IDDOKTYP-COPY-IN                  
047300                                    MOD-IDDOKTYP-COPY-UT                  
047400                                    MOD-IDDOK-COPY-IN                     
047500                                    MOD-IDDOK-COPY-UT                     
047600                                    MOD-IDSID-COPY-FOM-IN                 
047700                                    MOD-IDSID-COPY-FOM-UT                 
047800                                    MOD-IDSID-COPY-TOM-IN                 
047900                                    MOD-IDSID-COPY-TOM-UT                 
048000                                    MOD-IDSID-DEL-FOM-IN                  
048100                                    MOD-IDSID-DEL-FOM-UT                  
048200                                    MOD-IDSID-DEL-TOM-IN                  
048300                                    MOD-IDSID-DEL-TOM-UT                  
048400                                    MOD-IDSID-NY-FOM-IN                   
048500                                    MOD-IDSID-NY-FOM-UT                   
048600                                    MOD-IDSID-NY-TOM-IN                   
048700                                    MOD-IDSID-NY-TOM-UT                   
048800     ELSE                                                                 
048900       MOVE FEL9(INDX)           TO MOD-TEMFSFEL                          
049000       MOVE MFS-ADD-READ-FIELD   TO MOD-IDDOKTYP-COPY-ATTR                
049100                                    MOD-IDSID-DEL-FOM-ATTR                
049200                                    MOD-IDDOK-COPY-ATTR                   
049300                                    MOD-IDSID-NY-FOM-ATTR                 
049400                                    MOD-IDSID-DEL-TOM-ATTR                
049500                                    MOD-IDSID-COPY-FOM-ATTR               
049600                                    MOD-IDSID-NY-TOM-ATTR                 
049700                                    MOD-IDSID-COPY-TOM-ATTR               
049800       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDDOKTYP-COPY-IN                  
049900                                    MOD-IDSID-DEL-FOM-IN                  
050000                                    MOD-IDDOK-COPY-IN                     
050100                                    MOD-IDSID-NY-FOM-IN                   
050200                                    MOD-IDSID-DEL-TOM-IN                  
050300                                    MOD-IDSID-COPY-FOM-IN                 
050400                                    MOD-IDSID-NY-TOM-IN                   
050500                                    MOD-IDSID-COPY-TOM-IN                 
050600       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-COPY-FOM-UT                 
050700                                    MOD-IDSID-DEL-TOM-UT                  
050800                                    MOD-IDDOKTYP-COPY-UT                  
050900                                    MOD-IDDOK-COPY-UT                     
051000                                    MOD-IDSID-COPY-TOM-UT                 
051100                                    MOD-IDSID-DEL-FOM-UT                  
051200                                    MOD-IDSID-NY-FOM-UT                   
051300                                    MOD-IDSID-NY-TOM-UT                   
051400     END-IF                                                               
051500     .                                                                    
051600                                                                          
051700     EJECT                                                                
051800 F-KOLLA-INPUT SECTION.                                                   
051900                                                                          
052000     IF MID-IDDOKTYP-COPY-IN =  ALL '+'                                   
052100       MOVE MID-IDDOKTYP-COPY-UT TO IDDOKTYP-COPY                         
052200       MOVE NEJ                  TO KOPIERA                               
052300     ELSE                                                                 
052400       MOVE MID-IDDOKTYP-COPY-IN TO IDDOKTYP-COPY                         
052500       MOVE JA                   TO KOPIERA                               
052600     END-IF                                                               
052700     MOVE IDDOKTYP-COPY           TO MOD-IDDOKTYP-COPY-UT                 
052800     IF MID-IDDOK-COPY-IN   =  ALL '+'                                    
052900       MOVE MID-IDDOK-COPY-UT    TO IDDOK-COPY                            
053000       MOVE NEJ                  TO KOPIERA                               
053100     ELSE                                                                 
053200       MOVE MID-IDDOK-COPY-IN    TO IDDOK-COPY                            
053300       MOVE JA                   TO KOPIERA                               
053400     END-IF                                                               
053500     MOVE IDDOK-COPY              TO MOD-IDDOK-COPY-UT                    
053600                                                                          
053700     IF MID-IDSID-COPY-FOM-IN =  ALL '+'                                  
053800       MOVE MID-IDSID-COPY-FOM-UT TO IDSID-COPY-FOM                       
053900     ELSE                                                                 
054000       MOVE MID-IDSID-COPY-FOM-IN TO IDSID-COPY-FOM                       
054100     END-IF                                                               
054200     INSPECT IDSID-COPY-FOM REPLACING LEADING SPACE BY ZERO               
054300     IF IDSID-COPY-FOM            NOT NUMERIC                             
054400       MOVE FEL                  TO INDATA                                
054500       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDSID-COPY-FOM-ATTR               
054600       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDSID-COPY-FOM-IN                 
054700     ELSE                                                                 
054800       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-COPY-FOM-IN                 
054900       MOVE IDSID-COPY-FOM       TO W-IDSID-COPY-FOM                      
055000                                  MOD-IDSID-COPY-FOM-UT                   
055100       INSPECT MOD-IDSID-COPY-FOM-UT                                      
055200             REPLACING LEADING ZERO BY SPACE                              
055300     END-IF                                                               
055400     IF MID-IDSID-COPY-TOM-IN     =  ALL '+'                              
055500       MOVE MID-IDSID-COPY-TOM-UT TO IDSID-COPY-TOM                       
055600     ELSE                                                                 
055700       MOVE MID-IDSID-COPY-TOM-IN TO IDSID-COPY-TOM                       
055800     END-IF                                                               
055900     INSPECT IDSID-COPY-TOM REPLACING LEADING SPACE BY ZERO               
056000     IF IDSID-COPY-TOM            NOT NUMERIC                             
056100       MOVE FEL                  TO INDATA                                
056200       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDSID-COPY-TOM-ATTR               
056300       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDSID-COPY-TOM-IN                 
056400     ELSE                                                                 
056500       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-COPY-TOM-IN                 
056600       MOVE IDSID-COPY-TOM       TO W-IDSID-COPY-TOM                      
056700                                  MOD-IDSID-COPY-TOM-UT                   
056800       INSPECT MOD-IDSID-COPY-TOM-UT                                      
056900             REPLACING LEADING ZERO BY SPACE                              
057000     END-IF                                                               
057100                                                                          
057200     IF MID-IDSID-DEL-FOM-IN      =  ALL '+'                              
057300       MOVE MID-IDSID-DEL-FOM-UT TO IDSID-DEL-FOM                         
057400     ELSE                                                                 
057500       MOVE MID-IDSID-DEL-FOM-IN TO IDSID-DEL-FOM                         
057600     END-IF                                                               
057700     INSPECT IDSID-DEL-FOM REPLACING LEADING SPACE BY ZERO                
057800     IF IDSID-DEL-FOM             NOT NUMERIC                             
057900       MOVE FEL                  TO INDATA                                
058000       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDSID-DEL-FOM-ATTR                
058100       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDSID-DEL-FOM-IN                  
058200     ELSE                                                                 
058300       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-DEL-FOM-IN                  
058400       MOVE IDSID-DEL-FOM        TO W-IDSID-DEL-FOM                       
058500                                  MOD-IDSID-DEL-FOM-UT                    
058600       INSPECT MOD-IDSID-DEL-FOM-UT                                       
058700             REPLACING LEADING ZERO BY SPACE                              
058800     END-IF                                                               
058900     IF MID-IDSID-DEL-TOM-IN      =  ALL '+'                              
059000       MOVE MID-IDSID-DEL-TOM-UT TO IDSID-DEL-TOM                         
059100     ELSE                                                                 
059200       MOVE MID-IDSID-DEL-TOM-IN TO IDSID-DEL-TOM                         
059300     END-IF                                                               
059400     INSPECT IDSID-DEL-TOM REPLACING LEADING SPACE BY ZERO                
059500     IF IDSID-DEL-TOM             NOT NUMERIC                             
059600       MOVE FEL                  TO INDATA                                
059700       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDSID-DEL-TOM-ATTR                
059800       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDSID-DEL-TOM-IN                  
059900     ELSE                                                                 
060000       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-DEL-TOM-IN                  
060100       MOVE IDSID-DEL-TOM        TO W-IDSID-DEL-TOM                       
060200                                  MOD-IDSID-DEL-TOM-UT                    
060300       INSPECT MOD-IDSID-DEL-TOM-UT                                       
060400             REPLACING LEADING ZERO BY SPACE                              
060500     END-IF                                                               
060600                                                                          
060700     IF MID-IDSID-NY-FOM-IN       =  ALL '+'                              
060800       MOVE MID-IDSID-NY-FOM-UT  TO IDSID-NY-FOM                          
060900     ELSE                                                                 
061000       MOVE MID-IDSID-NY-FOM-IN  TO IDSID-NY-FOM                          
061100     END-IF                                                               
061200     INSPECT IDSID-NY-FOM REPLACING LEADING SPACE BY ZERO                 
061300     IF IDSID-NY-FOM              NOT NUMERIC                             
061400       MOVE FEL                  TO INDATA                                
061500       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDSID-NY-FOM-ATTR                 
061600       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDSID-NY-FOM-IN                   
061700     ELSE                                                                 
061800       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-NY-FOM-IN                   
061900       MOVE IDSID-NY-FOM         TO W-IDSID-NY-FOM                        
062000                                  MOD-IDSID-NY-FOM-UT                     
062100       INSPECT MOD-IDSID-NY-FOM-UT                                        
062200             REPLACING LEADING ZERO BY SPACE                              
062300     END-IF                                                               
062400     IF MID-IDSID-NY-TOM-IN       =  ALL '+'                              
062500       MOVE MID-IDSID-NY-TOM-UT  TO IDSID-NY-TOM                          
062600     ELSE                                                                 
062700       MOVE MID-IDSID-NY-TOM-IN  TO IDSID-NY-TOM                          
062800     END-IF                                                               
062900     INSPECT IDSID-NY-TOM REPLACING LEADING SPACE BY ZERO                 
063000     IF IDSID-NY-TOM              NOT NUMERIC                             
063100       MOVE FEL                  TO INDATA                                
063200       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDSID-NY-TOM-ATTR                 
063300       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDSID-NY-TOM-IN                   
063400     ELSE                                                                 
063500       MOVE MFS-RENSA-FAELT      TO MOD-IDSID-NY-TOM-IN                   
063600       MOVE IDSID-NY-TOM         TO W-IDSID-NY-TOM                        
063700                                  MOD-IDSID-NY-TOM-UT                     
063800       INSPECT MOD-IDSID-NY-TOM-UT                                        
063900             REPLACING LEADING ZERO BY SPACE                              
064000     END-IF                                                               
064100     IF INDATA-FEL                                                        
064200       MOVE FEL2(INDX)              TO MOD-TEMFSFEL                       
064300     END-IF                                                               
064400     .                                                                    
064500                                                                          
064600     EJECT                                                                
064700* IMS SEKTIONER                                                           
064800                                                                          
064900 IMS-GET-MSG SECTION.                                                     
065000                                                                          
065100     MOVE '  QC' TO GODK-STATUSKODER                                      
065200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
065300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600                                                                          
065700                                                                          
065800 IMS-INSERT-MSG SECTION.                                                  
065900                                                                          
066000     IF ENGLISH-TEXT                                                      
066100       MOVE 'N' TO MFS-KDHUVOMR                                           
066200     END-IF                                                               
066300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
066400     MOVE SPACE TO GODK-STATUSKODER                                       
066500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
066600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900                                                                          
067000     EJECT                                                                
067100 IMS-GU-INFO SECTION.                                                     
067200                                                                          
067300     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
067400            DELIMITED BY SIZE INTO SSA1                                   
067500     MOVE '  GE' TO GODK-STATUSKODER                                      
067600     CALL CBLTDLI USING GU  WDP5-PCB DLI-IO-P501-1 SSA1                   
067700     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
067800     PERFORM IMS-STATUSKONTROLL                                           
067900     .                                                                    
068000                                                                          
068100                                                                          
068200 IMS-GU-INFO-COPY SECTION.                                                
068300                                                                          
068400     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X-COPY ')'                   
068500            DELIMITED BY SIZE INTO SSA1                                   
068600     MOVE '  GE' TO GODK-STATUSKODER                                      
068700     CALL CBLTDLI USING GU  WDP52-PCB DLI-IO-P501-2 SSA1                  
068800     MOVE WDP52-STATUS-CODE TO STATUS-WS                                  
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100                                                                          
069200                                                                          
069300     EJECT                                                                
069400 IMS-GNP-SIDA SECTION.                                                    
069500                                                                          
069600     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
069700            DELIMITED BY SIZE INTO SSA1                                   
069800     MOVE '  GE' TO GODK-STATUSKODER                                      
069900     CALL CBLTDLI USING GNP WDP5-PCB DLI-IO-P512-1 SSA1                   
070000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     .                                                                    
070300                                                                          
070400                                                                          
070500 IMS-GNP-SIDA-COPY SECTION.                                               
070600                                                                          
070700     STRING 'WDP512  (IDSID    =' W-IDSID-X-COPY ')'                      
070800            DELIMITED BY SIZE INTO SSA1                                   
070900     MOVE '  GE' TO GODK-STATUSKODER                                      
071000     CALL CBLTDLI USING GNP WDP52-PCB DLI-IO-P512-2 SSA1                  
071100     MOVE WDP52-STATUS-CODE TO STATUS-WS                                  
071200     PERFORM IMS-STATUSKONTROLL                                           
071300     .                                                                    
071400                                                                          
071500                                                                          
071600     EJECT                                                                
071700 IMS-GHU-INFO SECTION.                                                    
071800                                                                          
071900     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
072000            DELIMITED BY SIZE INTO SSA1                                   
072100     MOVE '  GE' TO GODK-STATUSKODER                                      
072200     CALL CBLTDLI USING GHU  WDP5-PCB DLI-IO-P501-1 SSA1                  
072300     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
072400     PERFORM IMS-STATUSKONTROLL                                           
072500     .                                                                    
072600                                                                          
072700                                                                          
072800 IMS-GHU-SIDA SECTION.                                                    
072900                                                                          
073000     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
073100            DELIMITED BY SIZE INTO SSA1                                   
073200     STRING 'WDP512  (IDSID    =' W-IDSID-X ')'                           
073300            DELIMITED BY SIZE INTO SSA2                                   
073400     MOVE '  GE' TO GODK-STATUSKODER                                      
073500     CALL CBLTDLI USING GHU  WDP5-PCB DLI-IO-P512-1 SSA1 SSA2             
073600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
073700     PERFORM IMS-STATUSKONTROLL                                           
073800     .                                                                    
073900                                                                          
074000                                                                          
074100 IMS-ISRT-INFO SECTION.                                                   
074200                                                                          
074300     MOVE 'WDP501   ' TO SSA1                                             
074400     MOVE '  ' TO GODK-STATUSKODER                                        
074500     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-P501-1 SSA1                  
074600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900                                                                          
075000     EJECT                                                                
075100 IMS-ISRT-SIDA SECTION.                                                   
075200                                                                          
075300     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
075400            DELIMITED BY SIZE INTO SSA1                                   
075500     MOVE 'WDP512   ' TO SSA2                                             
075600     MOVE '  II' TO GODK-STATUSKODER                                      
075700     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-P512-1 SSA1 SSA2             
075800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
075900     PERFORM IMS-STATUSKONTROLL                                           
076000     .                                                                    
076100                                                                          
076200                                                                          
076300 IMS-DLET-INFO SECTION.                                                   
076400                                                                          
076500     MOVE '  ' TO GODK-STATUSKODER                                        
076600     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-P501-1                       
076700     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
076800     PERFORM IMS-STATUSKONTROLL                                           
076900     .                                                                    
077000                                                                          
077100                                                                          
077200 IMS-DLET-SIDA SECTION.                                                   
077300                                                                          
077400     MOVE '  ' TO GODK-STATUSKODER                                        
077500     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-P512-1                       
077600     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
077700     PERFORM IMS-STATUSKONTROLL                                           
077800     .                                                                    
077900                                                                          
078000                                                                          
078100     EJECT                                                                
078200 IMS-STATUSKONTROLL SECTION.                                              
078300                                                                          
078400     SET STATUS-IX TO 1                                                   
078500     SEARCH GODK-STATUS                                                   
078600       AT END                                                             
078700         CALL FELLOG                                                      
078800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078900         CONTINUE                                                         
079000     END-SEARCH                                                           
079100     .                                                                    
