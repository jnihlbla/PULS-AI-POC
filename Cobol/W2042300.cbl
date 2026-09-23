000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2042300.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/11/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BESTÄLLNINGS PROGRAM                                             
000900*        FÖR LEVERANTÖRBEDÖMNINGS LISTA                                   
001000*        (OCH EV URVAL)                                                   
001100*                                                                         
001200*        LISTPROGRAMMET BESTÄLLES VIA SOP MED                             
001300*        DETTA PGMS INPUT SOM EV PARAMETRAR                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T423                                              
001700*        MID:         W2I42301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O42301                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                        PIC X(08)   VALUE 'W2042300'.           
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FOR SCROLL LINES                                           
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004000                                                                          
004100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004200     88  KEYS-OK                             VALUE 'J'.                   
004300     88  KEYS-WRONG                          VALUE 'N'.                   
004400                                                                          
004500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004600     88  INDATA-OK                           VALUE 'J'.                   
004700     88  INDATA-FEL                          VALUE 'N'.                   
004800                                                                          
004900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005000     88  OWN-MID                             VALUE '2423'.                
005100     88  GOOD-MID                            VALUE '2421' '2422'          
005200                                                   '2423' '2424'          
005300                                                   '2425' '2426'          
005400                                                   '2427' '2428'          
005500                                                   '2429'.                
005600     88  HELP-MID                            VALUE '0551'.                
005700     EJECT                                                                
005800*01  -COPY WWDCKONS                                                       
005900     EJECT                                                                
006000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     EJECT                                                                
006700 01  FILLER.                                                              
006800     03  LISTANATT.                                                       
006900         05  FILLER              PIC X(30)   VALUE                        
007000                                 'SUPPLIER INFORMATION ORDERED'.          
007100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008200     EJECT                                                                
008300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W2I42301                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W2O42301                                                 
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  KEYS-FOR-DLI.                                                        
011100*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011200   03    W-KDARBTYP-X.                                                    
011300     05    W-KDARBTYP             PIC X(8)    VALUE SPACE.                
011400                                                                          
011500   03    W-IDPERSON-X.                                                    
011600     05    W-IDPERSON             PIC S9(3)   COMP-3 VALUE +0.            
011700                                                                          
011800 01  PROG-TO-PROG-SW.                                                     
011900*    03  -COPY WMSGSOP                                                    
012000     EJECT                                                                
012100 01  PARAMTER.                                                            
012200     03  W-IDDC-X.                                                        
012300         05  W-IDDC              PIC X(2).                                
012400     03  W-IDFTG-X.                                                       
012500         05  W-IDFTG             PIC 9(2)    VALUE 57.                    
012600     03  W-IDLEVNR-X.                                                     
012700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
013000     03  W-IDANSK-FOM-X.                                                  
013100         05  W-IDANSK-FOM        PIC 9(3)    VALUE ZERO.                  
013200     03  W-IDANSK-TOM-X.                                                  
013300         05  W-IDANSK-TOM        PIC 9(3)    VALUE ZERO.                  
013400     03  W-AVROP-VV-X.                                                    
013500         05  W-AVROP-VV          PIC 9(2)    VALUE ZERO.                  
013600     03  W-FLSLAP-X.                                                      
013700         05  W-FLSLAP            PIC X(1)    VALUE SPACE.                 
013800     03  W-FLLEVBESK-X.                                                   
013900         05  W-FLLEVBESK         PIC X(1)    VALUE SPACE.                 
014000     03  WS-IDMAIL.                                                       
014100         05  IDMAIL              PIC X(57) VALUE SPACE.                   
014200     SKIP2                                                                
014300*    --- STATUS CODES FROM IMS                                            
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FOUND                       VALUE '  '.                  
014600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014800     SKIP2                                                                
014900 01  GOOD-STATUSCODES.                                                    
015000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNCTION CODES                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900                                                                          
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
016100 01  DLI-IO-WDP311.                                                       
016200*    03  -COPY WDP311                                                     
016300     EJECT                                                                
016310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
016320 01  DLI-IO-WDB601.                                                       
016330*    03  -COPY WDB601                                                     
016340     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500*01  -COPY W0009   -PRE MSG-                                              
016600                                                                          
016700*01  -COPY W0009   -PRE ALT-                                              
016800                                                                          
016900*01  -COPY W0008   -PRE WDP3-                                             
017000     05  FILLER                  PIC X.                                   
017100                                                                          
017110*01  -COPY W0008   -PRE WDB6-                                             
017120     05  FILLER                  PIC X.                                   
017130                                                                          
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP3-PCB WDB6-PCB.             
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP3-PCB WDB6-PCB.             
017600                                                                          
017700     PERFORM IMS-GET-MSG                                                  
017800     IF SEGMENT-FOUND                                                     
017900       PERFORM A-INIT                                                     
018000       IF MFS-UPDATE                                                      
018100          PERFORM B-CHECK-INPUT                                           
018200          IF INDATA-OK                                                    
018300             PERFORM H-START-ROUTINE-W235B3                               
018400             PERFORM MFS-ERASE-FIELD-OUT                                  
018500          END-IF                                                          
018600       ELSE                                                               
018700         IF OWN-MID                                                       
018800            PERFORM E-SAME-PAGE                                           
018900         ELSE                                                             
019000            PERFORM MFS-ERASE-FIELD-OUT                                   
019100         END-IF                                                           
019200       END-IF                                                             
019300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
019400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
019500       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O42301-CTX + 4                  
019600       PERFORM IMS-INSERT-MSG                                             
019700     END-IF                                                               
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT SECTION.                                                          
020400                                                                          
020500     IF MSG-DOUBLE-TRANSACTIONS                                           
020600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I42301-CTX             
020700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020900     ELSE                                                                 
021000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I42301-CTX              
021100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021300     END-IF                                                               
021400                                                                          
021500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021800                                                                          
021900     MOVE LOW-VALUE TO MSG-AREA                                           
022000     MOVE 'W2O423N1' TO MFS-IDMOD                                         
022100     MOVE '2423' TO MOD-IDTRANS                                           
022200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
022300                                                                          
022400     IF OWN-MID OR HELP-MID                                               
022500       CONTINUE                                                           
022600     ELSE                                                                 
022700       MOVE SPACE TO MFS-KDTRTYP                                          
022800       MOVE '7' TO MFS-IDPFK                                              
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 B-CHECK-INPUT   SECTION.                                                 
023300                                                                          
023400     MOVE JA TO INDATA-SW                                                 
023500                                                                          
023600     IF MID-W2I42301-CTX = ALL '+'                                        
023700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
023800        CALL WMEDKONV USING MED-WMEDAREA                                  
023900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024000        PERFORM MFS-ERASE-FIELD-OUT                                       
024100        MOVE NOO TO INDATA-SW                                             
024200     ELSE                                                                 
024300        PERFORM BB-KONTROLL-URVAL                                         
024400                                                                          
024500        IF INDATA-FEL                                                     
024600          IF MOD-TEMFSFEL NOT > SPACE                                     
024700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
024800             CALL WMEDKONV USING MED-WMEDAREA                             
024900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
025000          END-IF                                                          
025100          PERFORM MFS-DONT-TOUCH-FIELD-OUT                                
025200        END-IF                                                            
025300     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 BB-KONTROLL-URVAL SECTION.                                               
025700                                                                          
025800* MAIL                                                                    
025900     PERFORM BBA-KONTROLL-MAIL                                            
026000                                                                          
026100* IDANSK-FOM                                                              
026200     IF MID-IDANSK-FOM NOT = ALL '+'                                      
026300        IF MID-IDANSK-FOM NOT NUMERIC                                     
026400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSK-FOM-ATTR                
026500           MOVE NOO                 TO INDATA-SW                          
026600        ELSE                                                              
026700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-FOM-ATTR                
026800           MOVE MID-IDANSK-FOM      TO W-IDANSK-FOM                       
026900        END-IF                                                            
027000     ELSE                                                                 
027100        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSK-FOM-ATTR                
027200        MOVE NOO                    TO INDATA-SW                          
027300     END-IF                                                               
027400                                                                          
027500* IDANSK-TOM                                                              
027600     IF MID-IDANSK-TOM NOT = ALL '+'                                      
027700        IF MID-IDANSK-TOM NOT NUMERIC                                     
027800           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSK-TOM-ATTR                
027900           MOVE NOO                 TO INDATA-SW                          
028000        ELSE                                                              
028100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-TOM-ATTR                
028200           MOVE MID-IDANSK-TOM      TO W-IDANSK-TOM                       
028300        END-IF                                                            
028400     ELSE                                                                 
028500        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSK-TOM-ATTR                
028600        MOVE NOO                    TO INDATA-SW                          
028700     END-IF                                                               
028800                                                                          
028900* IDLEVNR                                                                 
029000     IF MID-IDLEVNR NOT = ALL '+'                                         
029100        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-IDLEVNR-ATTR                   
029200        MOVE MID-IDLEVNR            TO W-IDLEVNR                          
029300     ELSE                                                                 
029400        MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDLEVNR-ATTR                   
029500        MOVE NOO                    TO INDATA-SW                          
029600        MOVE MID-IDLEVNR            TO W-IDLEVNR                          
029700     END-IF                                                               
029800                                                                          
029900* IDDC                                                                    
030000     IF MID-IDDC NOT = ALL '+'                                            
031600        MOVE MID-IDDC               TO W-IDDC                             
031700        PERFORM IMS-GU-WDB601                                             
031800        IF SEGMENT-FOUND                                                  
031810       AND (DCS-NDC-CN OR                                                 
031811           (DCS-NDC-NA AND DCS-USA))                                      
031812           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC-ATTR                      
031813           MOVE MID-IDDC            TO W-IDDC                             
031820        ELSE                                                              
031900           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-ATTR                      
032010           MOVE NOO                 TO INDATA-SW                          
032020           MOVE MID-IDDC            TO W-IDDC                             
032800        END-IF                                                            
032810     ELSE                                                                 
032811        MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDDC-ATTR                      
032812        MOVE NOO                    TO INDATA-SW                          
032813        MOVE MID-IDDC               TO W-IDDC                             
032820     END-IF                                                               
032900                                                                          
033000* AVROP-VV                                                                
033100     IF MID-AVROP-VV NOT = ALL '+'                                        
033200        IF MID-AVROP-VV NOT NUMERIC                                       
033300           MOVE MFS-NUM-FAELT-FEL   TO MOD-AVROP-VV-ATTR                  
033400           MOVE NOO                 TO INDATA-SW                          
033500        ELSE                                                              
033600           MOVE MFS-NUM-FAELT-RAETT TO MOD-AVROP-VV-ATTR                  
033700           MOVE MID-AVROP-VV        TO W-AVROP-VV                         
033800        END-IF                                                            
033900     ELSE                                                                 
034000        MOVE MFS-NUM-FAELT-FEL      TO MOD-AVROP-VV-ATTR                  
034100        MOVE NOO                    TO INDATA-SW                          
034200     END-IF                                                               
034300                                                                          
034400* FLLEVBESK                                                               
034500     IF MID-FLLEVBESK  NOT = ALL '+'                                      
034600        IF MID-FLLEVBESK = JA OR YES OR NOO                               
034700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLEVBESK-ATTR                 
034800          MOVE MID-FLLEVBESK        TO W-FLLEVBESK                        
034900          IF W-FLLEVBESK = YES                                            
035000             MOVE JA                TO W-FLLEVBESK                        
035100          END-IF                                                          
035200        ELSE                                                              
035300          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLEVBESK-ATTR                 
035400          MOVE NOO                  TO INDATA-SW                          
035500        END-IF                                                            
035600     ELSE                                                                 
035700        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLLEVBESK-ATTR                 
035800        MOVE NOO                    TO W-FLLEVBESK                        
035900     END-IF                                                               
036000                                                                          
036100* FLSLAP                                                                  
036200     IF MID-FLSLAP     NOT = ALL '+'                                      
036300        IF MID-FLSLAP    = JA OR YES OR NOO                               
036400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSLAP-ATTR                    
036500          MOVE MID-FLSLAP           TO W-FLSLAP                           
036600          IF W-FLSLAP = YES                                               
036700             MOVE JA                TO W-FLSLAP                           
036800          END-IF                                                          
036900        ELSE                                                              
037000          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSLAP-ATTR                    
037100          MOVE NOO                  TO INDATA-SW                          
037200        END-IF                                                            
037300     ELSE                                                                 
037400        MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLSLAP-ATTR                    
037500        MOVE NOO                    TO INDATA-SW                          
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 BBA-KONTROLL-MAIL SECTION.                                               
038100                                                                          
038200       IF  MID-KDARBTYP NOT = ALL '+'                                     
038300       AND MID-IDPERSON NOT = ALL '+'                                     
038400         IF MID-IDPERSON NUMERIC                                          
038500           MOVE MID-KDARBTYP           TO W-KDARBTYP                      
038600           MOVE MID-IDPERSON           TO W-IDPERSON                      
038700           PERFORM IMS-GU-WDP311                                          
038800           IF SEGMENT-FOUND                                               
038900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-ATTR               
039000             MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPERSON-ATTR               
039100             MOVE PERS-IDMAIL          TO IDMAIL                          
039200           ELSE                                                           
039300             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARBTYP-ATTR               
039400             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPERSON-ATTR               
039500             MOVE NOO TO INDATA-SW                                        
039600           END-IF                                                         
039700         ELSE                                                             
039800           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDPERSON-ATTR               
039900           MOVE NOO                    TO INDATA-SW                       
040000         END-IF                                                           
040100       ELSE                                                               
040200         MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDARBTYP-ATTR               
040300         MOVE MFS-NUM-FAELT-FEL        TO MOD-IDPERSON-ATTR               
040400         MOVE NOO                      TO INDATA-SW                       
040500       END-IF                                                             
040600       .                                                                  
040700       EJECT                                                              
040800                                                                          
040900 E-SAME-PAGE SECTION.                                                     
041000                                                                          
041100     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
041200     CALL WMEDKONV    USING MED-WMEDAREA                                  
041300     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
041400                                                                          
041500     PERFORM MFS-LAES-IN-IGEN                                             
041600     PERFORM MFS-DONT-TOUCH-FIELD-OUT                                     
041700     .                                                                    
041800     EJECT                                                                
041900 H-START-ROUTINE-W235B3  SECTION.                                         
042000                                                                          
042100     IF MID-IDLEVNR NOT = ALL '+'                                         
042200       MOVE '2423'   TO MSGSOP-IDTRANS                                    
042300       MOVE '1'      TO MSGSOP-KDMFSFOR                                   
042400       MOVE 'W235B3' TO MSGSOP-IDPROCESS                                  
042500       MOVE 'O'      TO MSGSOP-KDSOPFUNK                                  
042600       STRING 'IDANSK-FOM(' W-IDANSK-FOM                                  
042700              ')IDANSK-TOM(' W-IDANSK-TOM                                 
042800              ')IDLEVNR(' W-IDLEVNR                                       
042900              ')IDDC(' W-IDDC                                             
043000              ')AVR-VV(' W-AVROP-VV                                       
043100              ')FLSLAP(' W-FLSLAP                                         
043200              ')FLLEVBE(' W-FLLEVBESK                                     
043300              ')MAIL(' WS-IDMAIL                                          
043400              ')IDUSER(' MSG-SIGNON-USERID ')'                            
043500              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
043600                                                                          
043700       PERFORM IMS-INSERT-ALTMSG                                          
043800                                                                          
043900       MOVE LISTANATT  TO MOD-TEMFSINF                                    
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 MFS-ERASE-FIELD-OUT SECTION.                                             
044400                                                                          
044500*    --- ALLA        FÄLT                                                 
044600     MOVE MFS-ERASE-FIELD TO MOD-KDARBTYP                                 
044700                             MOD-IDPERSON                                 
044800                             MOD-IDANSK-FOM                               
044900                             MOD-IDANSK-TOM                               
045000                             MOD-IDLEVNR                                  
045100                             MOD-IDDC                                     
045200                             MOD-AVROP-VV                                 
045300                             MOD-FLLEVBESK                                
045400                             MOD-FLSLAP                                   
045500     .                                                                    
045600     EJECT                                                                
045700 MFS-DONT-TOUCH-FIELD-OUT SECTION.                                        
045800                                                                          
045900*    --- ALLA        FÄLT                                                 
046000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDARBTYP                          
046100                                    MOD-IDPERSON                          
046200                                    MOD-IDANSK-FOM                        
046300                                    MOD-IDANSK-TOM                        
046400                                    MOD-IDLEVNR                           
046500                                    MOD-IDDC                              
046600                                    MOD-AVROP-VV                          
046700                                    MOD-FLLEVBESK                         
046800                                    MOD-FLSLAP                            
046900     .                                                                    
047000     EJECT                                                                
047100 MFS-LAES-IN-IGEN SECTION.                                                
047200                                                                          
047300*    --- ALLA INDATA-FÄLT                                                 
047400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDARBTYP-ATTR                      
047500                                   MOD-IDPERSON-ATTR                      
047600                                   MOD-IDANSK-FOM-ATTR                    
047700                                   MOD-IDANSK-TOM-ATTR                    
047800                                   MOD-IDLEVNR-ATTR                       
047900                                   MOD-IDDC-ATTR                          
048000                                   MOD-AVROP-VV-ATTR                      
048100                                   MOD-FLSLAP-ATTR                        
048200                                   MOD-FLLEVBESK-ATTR                     
048300     .                                                                    
048400     EJECT                                                                
048500* --- IMS SECTIONS ---                                                    
048600     SKIP3                                                                
048700 IMS-GET-MSG SECTION.                                                     
048800                                                                          
048900     MOVE '  QC' TO GOOD-STATUSCODES                                      
049000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049200     PERFORM IMS-STATUSCHECK                                              
049300     .                                                                    
049400     SKIP3                                                                
049500 IMS-INSERT-MSG SECTION.                                                  
049600                                                                          
049700     IF ENGLISH-TEXT                                                      
049800       MOVE 'N' TO MFS-KDHUVOMR                                           
049900     END-IF                                                               
050000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050100     MOVE SPACE TO GOOD-STATUSCODES                                       
050200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
050300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUSCHECK                                              
050500     .                                                                    
050600     SKIP3                                                                
050700 IMS-INSERT-ALTMSG SECTION.                                               
050800                                                                          
050900     MOVE SPACE TO GOOD-STATUSCODES                                       
051000     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
051100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
051200     PERFORM IMS-STATUSCHECK                                              
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-GU-WDP311 SECTION.                                                   
051600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
051700            DELIMITED BY SIZE INTO SSA1                                   
051800     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
051900            DELIMITED BY SIZE INTO SSA2                                   
052000     MOVE '  GE' TO GOOD-STATUSCODES                                      
052100     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
052200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUSCHECK                                              
052400     .                                                                    
052500                                                                          
052510 IMS-GU-WDB601  SECTION.                                                  
052540                                                                          
052560     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
052570            DELIMITED BY SIZE INTO SSA1                                   
052580     MOVE '  GE'                TO GOOD-STATUSCODES                       
052590     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
052591     MOVE WDB6-STATUS-CODE      TO STATUS-WS                              
052592     PERFORM IMS-STATUSCHECK                                              
052594     .                                                                    
052595                                                                          
052600 IMS-STATUSCHECK SECTION.                                                 
052700                                                                          
052800     SET STATUS-IX TO 1                                                   
052900     SEARCH GOOD-STATUS                                                   
053000       AT END                                                             
053100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
053200         DELIMITED BY SIZE INTO ERROR-TEXT                                
053300         CALL FELLOG                                                      
053400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
053500         CONTINUE                                                         
053600     END-SEARCH                                                           
053700     .                                                                    
