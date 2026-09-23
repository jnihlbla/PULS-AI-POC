000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013200.                                                
000400*AUTHOR.         LARS THELL / LASSI.                                      
000500*DATE-WRITTEN.   92/06/01   / NOV 2011.                                   
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET HANTERAR MFS-DELEN FÖR 6132-BILDEN                    
001100*        OCH ANROPAR W6013210 SOM INNEHÅLLER AFFÄRSLOGIKEN.               
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W6T132 / W6T132U                                    
001500*        MID:         W6I13201                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W6O13201                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6013200'.            
002600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  YES                         PIC X       VALUE 'Y'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 01  W-PLUSTKN.                                                           
003100    03 FILLER                    PIC X(8)   VALUE '++++++++'.             
003200                                                                          
003300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003700 77  6191-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
003800 77  MAX-6191-IX                 PIC S9(9)  VALUE +24   COMP SYNC.        
003900 77  6197-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004000 77  MAX-6197-IX                 PIC S9(9)  VALUE +15   COMP SYNC.        
004100 77  VAGN-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  MAX-VAGN-IX                 PIC S9(9)  VALUE +51   COMP SYNC.        
004300 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600 77  WS-ADINLOMR                 PIC X(4)    VALUE SPACE.                 
004700 77  WS-ADINLOMR-NXT             PIC X(4)    VALUE SPACE.                 
004800 77  WS-KDINLQ                   PIC X(1)    VALUE SPACE.                 
004900 77  WS-BEFT-FOM                 PIC X(2)    VALUE SPACE.                 
005000 77  WS-BEFT-TOM                 PIC X(2)    VALUE SPACE.                 
005100 77  WS-FLINLFB                  PIC X(1)    VALUE SPACE.                 
005200                                                                          
005300*    --- HOPPNYCKLAR SOM EJ SYNS PÅ SKÄRMEN                               
005400 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
005500 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005600 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
005700 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
005800                                                                          
005900*    --- SWITCHAR                                                         
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '6132'.                
006200     88  GODK-MID                            VALUE '6131' '6132'          
006300                                                   '6133' '6134'          
006400                                                   '6135' '6136'          
006500                                                   '6137' '6139'.         
006600     88  HELP-MID                            VALUE '0551'.                
006700                                                                          
006800     EJECT                                                                
006900 01  WS-IDMSG-ERROR              PIC X(3).                                
007000     88  UPD-NOT-ALLOWED         VALUE '007'.                             
007100     88  PF11-AND-NO-DATA        VALUE '014'.                             
007200     88  CORR-HILITE-FLDS        VALUE '020'.                             
007300     88  WRONG-KEY               VALUE '022'.                             
007400     88  WRONG-VALUE             VALUE '023'.                             
007500     88  MISSING-DATA            VALUE '027'.                             
007600     88  LOT-NOT-ON-LOC          VALUE '224'.                             
007700     88  KIT-MARK-CASE           VALUE '334'.                             
007800 01  WS-IDMSG-INFO               PIC X(3).                                
007900     88  UPDATE-DONE             VALUE '001'.                             
008000     88  FIRST-PAGE              VALUE '010'.                             
008100     88  MORE-INFO-EXISTS        VALUE '011'.                             
008200     88  NO-MORE-INFO            VALUE '012'.                             
008300     88  PRESS-PF11              VALUE '013'.                             
008400     EJECT                                                                
008500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008600 01  GENERELLA-SUBPROGRAM.                                                
008700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  W6013210                PIC X(8)    VALUE 'W6013210'.            
009200                                                                          
009300*    --- PARAMETRAR TILL SUBPROGRAM ANROP                                 
009400 01  FILLER                      PIC X(16)  VALUE 'WMEDKONV'.             
009500*01 -COPY WMEDAREA                                                        
009600     SKIP3                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010200     03  INF-NO-MORE-INFO        PIC X(3)    VALUE '106'.                 
010300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010400     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  ERR-KIT-MARK-CASE       PIC X(3)    VALUE '185'.                 
010700     03  ERR-LOT-NOT-ON-LOC      PIC X(3)    VALUE '190'.                 
010800     03  ERR-MISSING-DATA        PIC X(3)    VALUE '200'.                 
010900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011000     03  ERR-WRONG-VALUE         PIC X(3)    VALUE '416'.                 
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)  VALUE 'W005INIT'.             
011300*01  -COPY WMSGINIT                                                       
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'W6013210'.             
011600 01  REQU-AREA.                                                           
011700*    03 -COPY WZ01REQU                                                    
011800*    03 -COPY W60132I1                                                    
011900 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +12.               
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
012200 01  RESP-AREA.                                                           
012300*    03 -COPY WZ01RESP                                                    
012400*    03 -COPY W60132O1                                                    
012500     EJECT                                                                
012600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012900     SKIP3                                                                
013000*01  MID -COPY W6I13201                                                   
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013300     SKIP3                                                                
013400*01  -COPY WMSGAREA                                                       
013500     EJECT                                                                
013600     03  MOD REDEFINES MSG-AREA.                                          
013700*      05  -COPY W6O13201                                                 
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014000     SKIP3                                                                
014100*01  -COPY WMFSAREA                                                       
014200     EJECT                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600                                                                          
014700 01  GODK-STATUSKODER.                                                    
014800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014900     SKIP3                                                                
015000 01  SSA1                        PIC X(160).                              
015100     EJECT                                                                
015200*    --- IMS FUNKTIONSKODER                                               
015300*01  -COPY W0003                                                          
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700*01  -COPY W0009   -PRE MSG-                                              
015800     EJECT                                                                
015900 01  ALT-PCB                  PIC X.                                      
016000 01  6197-PCB                 PIC X.                                      
016100*01  -COPY W0008  -PRE USEA-                                              
016200     05  FILLER               PIC X.                                      
016300     EJECT                                                                
016400 01  INLA1-PCB                PIC X.                                      
016500 01  INLA2-PCB                PIC X.                                      
016600 01  INLA3-PCB                PIC X.                                      
016700 01  INLC-PCB                 PIC X.                                      
016800 01  INLF-PCB                 PIC X.                                      
016900 01  INLH-PCB                 PIC X.                                      
017000 01  PLAA-PCB                 PIC X.                                      
017100 01  WDK6-PCB                 PIC X.                                      
017200 01  WDK7-PCB                 PIC X.                                      
017300 01  WDB6-PCB                 PIC X.                                      
017400 01  WDD3-PCB                 PIC X.                                      
017500 01  PMRK-INLB-PCB            PIC X.                                      
017600 01  PMRK-INLC-PCB            PIC X.                                      
017700 01  PMRK-PLAA-PCB            PIC X.                                      
017800     EJECT                                                                
017900 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB 6197-PCB USEA-PCB             
018000                           INLA1-PCB INLA2-PCB INLA3-PCB                  
018100                           INLC-PCB INLF-PCB INLH-PCB  PLAA-PCB           
018200                           WDK6-PCB WDK7-PCB WDB6-PCB WDD3-PCB            
018300                           PMRK-INLB-PCB PMRK-INLC-PCB                    
018400                           PMRK-PLAA-PCB.                                 
018500     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB 6197-PCB USEA-PCB             
018600                           INLA1-PCB INLA2-PCB INLA3-PCB                  
018700                           INLC-PCB INLF-PCB INLH-PCB  PLAA-PCB           
018800                           WDK6-PCB WDK7-PCB WDB6-PCB WDD3-PCB            
018900                           PMRK-INLB-PCB PMRK-INLC-PCB                    
019000                           PMRK-PLAA-PCB.                                 
019100                                                                          
019200     PERFORM IMS-GET-MSG                                                  
019300     IF SEGMENT-FINNS                                                     
019400       PERFORM A-INIT                                                     
019500       PERFORM B-INIT-KEYS                                                
019600       PERFORM C-INIT-REQU                                                
019700       IF MFS-UPDATE                                                      
019800         SET REQU-UPDATE TO TRUE                                          
019810         PERFORM E-SAMMA-SIDA                                             
020500       ELSE                                                               
020600         IF MFS-FIRST                                                     
020700           SET REQU-FIRST  TO TRUE                                        
020800           PERFORM MFS-RENSA-FAELT-IN                                     
020900         ELSE                                                             
021000           IF MFS-NEXT                                                    
021100             SET REQU-NEXT  TO TRUE                                       
021200             PERFORM D-NAESTA-SIDA                                        
021300           ELSE                                                           
021400             SET REQU-QUERY  TO TRUE                                      
021500             PERFORM E-SAMMA-SIDA                                         
021600           END-IF                                                         
021700         END-IF                                                           
021800       END-IF                                                             
021900       PERFORM F-CALL-BIZ-LOGIC-W6013210                                  
022000       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O13201 + 4                      
022100       PERFORM IMS-INSERT-MSG                                             
022200     END-IF                                                               
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     IF MSG-DUBBLA-TRANSKODER                                             
023100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13201                 
023200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
023300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
023400     ELSE                                                                 
023500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I13201                 
023600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
023700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
023800     END-IF                                                               
023900                                                                          
024000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
024100     MOVE MSG-IDPFK            TO MFS-IDPFK                               
024200     MOVE MFS-IDTRANS          TO W-IDTRANS                               
024300                                                                          
024400     MOVE LOW-VALUE            TO MSG-AREA                                
024500     MOVE 'W6O132N1'           TO MFS-IDMOD                               
024600     MOVE '6132'               TO MOD-IDTRANS                             
024700     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
024800                                                                          
024900     IF EGEN-MID OR HELP-MID                                              
025000       CONTINUE                                                           
025100      ELSE                                                                
025200       MOVE SPACE              TO MFS-KDTRTYP                             
025300       MOVE '7'                TO MFS-IDPFK                               
025400     END-IF                                                               
025500                                                                          
025600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
025700     MOVE '001'                  TO MSGI-KDCALL                           
025800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
025900     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
026000     MOVE '6132'                 TO MSGI-IDTRANS                          
026100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026200                                                                          
026300     IF MSGI-IDLAND-SPR = 'GB'                                            
026400       MOVE +2                 TO SPRAK-IX                                
026500       MOVE 'GB '              TO MED-IDSKYLT                             
026600     ELSE                                                                 
026700       MOVE +1                 TO SPRAK-IX                                
026800       MOVE 'S  '              TO MED-IDSKYLT                             
026900     END-IF                                                               
027000     MOVE SPACE                TO MED-IDMFSINF                            
027100                                                                          
027200     IF MID-IDRADNR-NEXT  = 99999 AND MFS-NEXT                            
027300        MOVE '7'                TO MFS-IDPFK                              
027400        MOVE SPACE              TO MFS-KDTRTYP                            
027500     END-IF                                                               
027600     MOVE '101'                 TO REQU-IDMSGVER                          
027700     MOVE MSGI-IDUSER           TO REQU-IDUSER                            
027800     .                                                                    
027900     EJECT                                                                
028000 B-INIT-KEYS SECTION.                                                     
028100                                                                          
028200     PERFORM BA-KOLLA-IDDC                                                
028300     PERFORM BH-KOLLA-ADINLOMR                                            
028400     PERFORM BB-KOLLA-ADINLOMR-NXT                                        
028500     PERFORM BC-KOLLA-KDINLQ                                              
028600     PERFORM BD-KOLLA-BEFT                                                
028700     PERFORM BE-KOLLA-FLINLFB                                             
028800                                                                          
028900     IF GODK-MID OR HELP-MID                                              
029000       PERFORM BF-FLYTTA-OEVRIGA-NYCKLAR                                  
029100       MOVE WS-ADINLOMR        TO MOD-ADINLOMR-UT                         
029200       MOVE WS-ADINLOMR-NXT    TO MOD-ADINLOMR-NXT-UT                     
029300       MOVE WS-KDINLQ          TO MOD-KDINLQ-UT                           
029400       MOVE REQU-IDDC-KEY      TO MOD-IDDC-UT                             
029500       MOVE WS-BEFT-FOM        TO MOD-BEFT-FOM-UT                         
029600       MOVE WS-BEFT-TOM        TO MOD-BEFT-TOM-UT                         
029700       MOVE WS-FLINLFB         TO MOD-FLINLFB-UT                          
029800     ELSE                                                                 
029900       MOVE MFS-RENSA-FAELT    TO MOD-ADINLOMR-UT                         
030000                                  MOD-ADINLOMR-NXT-UT                     
030100                                  MOD-KDINLQ-UT                           
030200                                  MOD-BEFT-FOM-UT                         
030300                                  MOD-BEFT-TOM-UT                         
030400                                  MOD-FLINLFB-UT                          
030500                                  MOD-IDINLVGN-UT                         
030600                                  MOD-IDLEVNR-KOLLI-UT                    
030700                                  MOD-IDOKOLLI-UT                         
030800                                  MOD-IDLOPNRM-UT                         
030900                                  MOD-IDDC-UT                             
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 BA-KOLLA-IDDC        SECTION.                                            
031400                                                                          
031500     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
031600                                                                          
031700     IF MID-IDDC-IN             =  ALL '+'                                
031800       MOVE MSGI-IDDC          TO REQU-IDDC-KEY                           
031900     ELSE                                                                 
032000       MOVE MID-IDDC-IN        TO REQU-IDDC-KEY                           
032100       MOVE '7'                TO MFS-IDPFK                               
032200       MOVE SPACE              TO MFS-KDTRTYP                             
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 BB-KOLLA-ADINLOMR-NXT  SECTION.                                          
032700                                                                          
032800     MOVE MFS-RENSA-FAELT       TO MOD-ADINLOMR-NXT-IN                    
032900                                                                          
033000     IF MID-ADINLOMR-NXT-IN     =  ALL '+'                                
033100       MOVE MID-ADINLOMR-NXT-UT TO WS-ADINLOMR-NXT                        
033200     ELSE                                                                 
033300       MOVE MID-ADINLOMR-NXT-IN TO WS-ADINLOMR-NXT                        
033400       MOVE '7'                 TO MFS-IDPFK                              
033500       MOVE SPACE               TO MFS-KDTRTYP                            
033600     END-IF                                                               
033700     MOVE WS-ADINLOMR-NXT       TO REQU-ADINLOMR-NXT-KEY                  
033800     .                                                                    
033900     EJECT                                                                
034000 BC-KOLLA-KDINLQ      SECTION.                                            
034100                                                                          
034200     MOVE MFS-RENSA-FAELT      TO MOD-KDINLQ-IN                           
034300                                                                          
034400     IF MID-KDINLQ-IN          =  ALL '+'                                 
034500       MOVE MID-KDINLQ-UT      TO WS-KDINLQ                               
034600     ELSE                                                                 
034700       MOVE MID-KDINLQ-IN      TO WS-KDINLQ                               
034800       MOVE '7'                TO MFS-IDPFK                               
034900       MOVE SPACE              TO MFS-KDTRTYP                             
035000     END-IF                                                               
035100     MOVE WS-KDINLQ            TO REQU-KDINLQ-KEY                         
035200     .                                                                    
035300     EJECT                                                                
035400 BD-KOLLA-BEFT        SECTION.                                            
035500                                                                          
035600     MOVE MFS-RENSA-FAELT      TO MOD-BEFT-FOM-IN                         
035700                                                                          
035800     IF MID-BEFT-FOM-IN        =  ALL '+'                                 
035900       MOVE MID-BEFT-FOM-UT    TO WS-BEFT-FOM                             
036000     ELSE                                                                 
036100       MOVE MID-BEFT-FOM-IN    TO WS-BEFT-FOM                             
036200       MOVE '7'                TO MFS-IDPFK                               
036300       MOVE SPACE              TO MFS-KDTRTYP                             
036400     END-IF                                                               
036500     MOVE WS-BEFT-FOM          TO REQU-BEFT-FOM-KEY                       
036600                                                                          
036700     MOVE MFS-RENSA-FAELT      TO MOD-BEFT-TOM-IN                         
036800                                                                          
036900     IF MID-BEFT-TOM-IN        =  ALL '+'                                 
037000       MOVE MID-BEFT-TOM-UT    TO WS-BEFT-TOM                             
037100     ELSE                                                                 
037200       MOVE MID-BEFT-TOM-IN    TO WS-BEFT-TOM                             
037300       MOVE '7'                TO MFS-IDPFK                               
037400       MOVE SPACE              TO MFS-KDTRTYP                             
037500     END-IF                                                               
037600     MOVE WS-BEFT-TOM          TO REQU-BEFT-TOM-KEY                       
037700     .                                                                    
037800     EJECT                                                                
037900 BE-KOLLA-FLINLFB     SECTION.                                            
038000                                                                          
038100     MOVE MFS-RENSA-FAELT      TO MOD-FLINLFB-IN                          
038200                                                                          
038300     IF MID-FLINLFB-IN         =  ALL '+'                                 
038400       MOVE MID-FLINLFB-UT     TO WS-FLINLFB                              
038500     ELSE                                                                 
038600       MOVE MID-FLINLFB-IN     TO WS-FLINLFB                              
038700       MOVE '7'                TO MFS-IDPFK                               
038800       MOVE SPACE              TO MFS-KDTRTYP                             
038900     END-IF                                                               
039000     MOVE WS-FLINLFB           TO REQU-FLINLFB-KEY                        
039100     .                                                                    
039200     EJECT                                                                
039300 BF-FLYTTA-OEVRIGA-NYCKLAR    SECTION.                                    
039400                                                                          
039500     MOVE MID-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                        
039600                                                                          
039700     MOVE MFS-RENSA-FAELT      TO MOD-IDINLVGN-IN                         
039800     IF MID-IDINLVGN-IN        = ALL '+'                                  
039900       MOVE MID-IDINLVGN-UT    TO WS-IDINLVGN                             
040000     ELSE                                                                 
040100       MOVE MID-IDINLVGN-IN    TO WS-IDINLVGN                             
040200     END-IF                                                               
040300     MOVE WS-IDINLVGN          TO MOD-IDINLVGN-UT                         
040400                                                                          
040500     MOVE MFS-RENSA-FAELT         TO MOD-IDLEVNR-KOLLI-IN                 
040600     IF MID-IDLEVNR-KOLLI-IN      = ALL '+'                               
040700       MOVE MID-IDLEVNR-KOLLI-UT  TO WS-IDLEVNR-KOLLI                     
040800     ELSE                                                                 
040900       MOVE MID-IDLEVNR-KOLLI-IN  TO WS-IDLEVNR-KOLLI                     
041000     END-IF                                                               
041100     MOVE WS-IDLEVNR-KOLLI        TO MOD-IDLEVNR-KOLLI-UT                 
041200                                                                          
041300     MOVE MFS-RENSA-FAELT      TO MOD-IDOKOLLI-IN                         
041400     IF MID-IDOKOLLI-IN        = ALL '+'                                  
041500       MOVE MID-IDOKOLLI-UT    TO WS-IDOKOLLI                             
041600     ELSE                                                                 
041700       MOVE MID-IDOKOLLI-IN    TO WS-IDOKOLLI                             
041800     END-IF                                                               
041900     MOVE WS-IDOKOLLI          TO MOD-IDOKOLLI-UT                         
042000                                                                          
042100     MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-IN                         
042200     IF MID-IDLOPNRM-IN        = ALL '+'                                  
042300       MOVE MID-IDLOPNRM-UT    TO WS-IDLOPNRM                             
042400     ELSE                                                                 
042500       MOVE MID-IDLOPNRM-IN    TO WS-IDLOPNRM                             
042600     END-IF                                                               
042700                                                                          
042800     MOVE WS-IDLOPNRM          TO MOD-IDLOPNRM-UT                         
042900     .                                                                    
043000     EJECT                                                                
043100 BH-KOLLA-ADINLOMR  SECTION.                                              
043200                                                                          
043300     MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-IN                         
043400                                                                          
043500     IF MID-ADINLOMR-IN        = ALL '+'                                  
043600       MOVE MID-ADINLOMR-UT    TO WS-ADINLOMR                             
043700     ELSE                                                                 
043800       MOVE MID-ADINLOMR-IN    TO WS-ADINLOMR                             
043900       MOVE '7'                TO MFS-IDPFK                               
044000       MOVE SPACE              TO MFS-KDTRTYP                             
044100     END-IF                                                               
044200     MOVE WS-ADINLOMR          TO REQU-ADINLOMR-KEY                       
044300     .                                                                    
044400     EJECT                                                                
044500 C-INIT-REQU SECTION.                                                     
044600                                                                          
044700     MOVE MAX-KVRADER         TO REQU-KVRADER                             
044800                                                                          
044900     MOVE +1                  TO INDX                                     
045000     PERFORM UNTIL INDX       >  MAX-INDX                                 
045100       MOVE MID-KDCMDVAL-RAD(INDX) TO REQU-KDCMDVAL-LINE(INDX)            
045200       MOVE MID-IDLOPNRM-RAD(INDX) TO REQU-IDLOPNRM-LINE(INDX)            
045300       IF MID-IDRADNR-RAD(INDX)     = ALL '+'                             
045400         MOVE W-PLUSTKN             TO REQU-IDRADNR-LINE(INDX)            
045500       ELSE                                                               
045600         MOVE MID-IDRADNR-RAD(INDX) TO REQU-IDRADNR-LINE(INDX)            
045700       END-IF                                                             
045800       ADD +1                      TO INDX                                
045900     END-PERFORM                                                          
046000                                                                          
046100     MOVE +1                              TO VAGN-IX                      
046200     PERFORM UNTIL VAGN-IX                >  MAX-VAGN-IX                  
046300         MOVE MID-IDINLVGN-SPAR(VAGN-IX)  TO                              
046400                               REQU-IDINLVGN-SPAR(VAGN-IX)                
046500         ADD +1                           TO VAGN-IX                      
046600     END-PERFORM                                                          
046700                                                                          
046800     MOVE MID-KVINLCAR        TO REQU-KVINLCAR                            
046900     MOVE MID-KVRADER-HIT     TO REQU-KVRADER-HIT                         
047000     MOVE MID-KVRADER-TOT     TO REQU-KVRADER-TOT                         
047100     MOVE MID-KVRADER-PRIO    TO REQU-KVRADER-PRIO                        
047200     .                                                                    
047300     EJECT                                                                
047400 D-NAESTA-SIDA SECTION.                                                   
047500                                                                          
047600     MOVE MID-IDLEVNR-NEXT     TO REQU-IDLEVNR-START                      
047700     MOVE MID-IDFS-NEXT        TO REQU-IDFS-START                         
047800     MOVE MID-TIAVIDAT-NEXT    TO REQU-TIAVIDAT-START                     
047900     MOVE MID-IDRADNR-INL-NEXT TO REQU-IDRADNR-INL-START                  
048000     MOVE MID-IDRADNR-NEXT     TO REQU-IDRADNR-START                      
048100     MOVE MID-KDINLPRIO-NEXT   TO REQU-KDINLPRIO-START                    
048200                                                                          
048300     PERFORM MFS-RENSA-FAELT-IN                                           
048400     .                                                                    
048500     EJECT                                                                
048600 E-SAMMA-SIDA SECTION.                                                    
048700                                                                          
048800     IF EGEN-MID OR HELP-MID                                              
048900       MOVE MID-IDLEVNR-ENTER     TO REQU-IDLEVNR-START                   
049000       MOVE MID-IDFS-ENTER        TO REQU-IDFS-START                      
049100       MOVE MID-TIAVIDAT-ENTER    TO REQU-TIAVIDAT-START                  
049200       MOVE MID-IDRADNR-INL-ENTER TO REQU-IDRADNR-INL-START               
049300       MOVE MID-IDRADNR-ENTER     TO REQU-IDRADNR-START                   
049400       MOVE MID-KDINLPRIO-ENTER   TO REQU-KDINLPRIO-START                 
049500     ELSE                                                                 
049600       PERFORM MFS-RENSA-FAELT-IN                                         
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 F-CALL-BIZ-LOGIC-W6013210 SECTION.                                       
050100                                                                          
050200     CALL W6013210 USING REQU-AREA RESP-AREA MAX-KVRADER                  
050300                         ALT-PCB 6197-PCB                                 
050400                         INLA1-PCB INLA2-PCB INLA3-PCB                    
050500                         INLC-PCB  INLF-PCB  INLH-PCB PLAA-PCB            
050600                         WDK6-PCB  WDK7-PCB  WDB6-PCB WDD3-PCB            
050700                         PMRK-INLB-PCB PMRK-INLC-PCB                      
050800                         PMRK-PLAA-PCB                                    
050900                                                                          
051000     PERFORM FA-SET-MSG                                                   
051100     IF NOT WRONG-KEY                                                     
051200       PERFORM FB-INIT-MOD                                                
051300*      CALL FELLOG                                                        
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 FA-SET-MSG SECTION.                                                      
051800                                                                          
051900     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
052000     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
052100                                                                          
052200     IF WRONG-KEY                                                         
052300       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
052400       PERFORM MFS-RENSA-FAELT-IN                                         
052500       PERFORM MFS-RENSA-FAELT-UT                                         
052600     ELSE                                                                 
052700       IF PRESS-PF11                                                      
052800         MOVE INF-PRESS-PF11        TO MED-IDMFSINF                       
052900       END-IF                                                             
053000       IF FIRST-PAGE                                                      
053100         MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                       
053200       END-IF                                                             
053300       IF UPDATE-DONE                                                     
053400         MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                       
053500       END-IF                                                             
053600       IF MORE-INFO-EXISTS                                                
053700         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
053800       END-IF                                                             
053900       IF NO-MORE-INFO                                                    
054000         MOVE INF-NO-MORE-INFO      TO MED-IDMFSINF                       
054100       END-IF                                                             
054200                                                                          
054300       IF CORR-HILITE-FLDS                                                
054400         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
054500       END-IF                                                             
054600       IF UPD-NOT-ALLOWED                                                 
054700         MOVE ERR-UPD-NOT-ALLOWED   TO MED-IDMFSFEL                       
054800       END-IF                                                             
054900       IF PF11-AND-NO-DATA                                                
055000         MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                       
055100       END-IF                                                             
055200       IF KIT-MARK-CASE                                                   
055300         MOVE ERR-KIT-MARK-CASE     TO MED-IDMFSFEL                       
055400       END-IF                                                             
055500       IF LOT-NOT-ON-LOC                                                  
055600         MOVE ERR-LOT-NOT-ON-LOC    TO MED-IDMFSFEL                       
055700       END-IF                                                             
055800       IF MISSING-DATA                                                    
055900         MOVE ERR-MISSING-DATA      TO MED-IDMFSFEL                       
056000       END-IF                                                             
056100       IF WRONG-VALUE                                                     
056200         MOVE ERR-WRONG-VALUE       TO MED-IDMFSFEL                       
056300       END-IF                                                             
056400     END-IF                                                               
056500                                                                          
056600     CALL WMEDKONV USING MED-WMEDAREA                                     
056700     MOVE MED-MFSFEL         TO MOD-TEMFSFEL                              
056800     MOVE MED-MFSINF         TO MOD-TEMFSINF                              
056900     .                                                                    
057000     EJECT                                                                
057100 FB-INIT-MOD SECTION.                                                     
057200                                                                          
057300     MOVE RESP-ADINLOMR-KEY       TO MOD-ADINLOMR-UT                      
057400     MOVE RESP-ADINLOMR-NXT-KEY   TO MOD-ADINLOMR-NXT-UT                  
057500                                                                          
057600     MOVE +1                              TO VAGN-IX                      
057700     PERFORM UNTIL VAGN-IX                >  MAX-VAGN-IX                  
057800         MOVE RESP-IDINLVGN-SPAR(VAGN-IX)  TO                             
057900                                MOD-IDINLVGN-SPAR(VAGN-IX)                
058000         ADD +1                           TO VAGN-IX                      
058100     END-PERFORM                                                          
058200                                                                          
058300     MOVE +1                       TO INDX                                
058400     PERFORM UNTIL INDX            >  RESP-KVRADER                        
058500       IF RESP-KDCMDVAL-ATTR(INDX) = ALL '+'                              
058600         MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-RAD-ATTR(INDX)           
058700       ELSE                                                               
058800         MOVE RESP-KDCMDVAL-ATTR(INDX)                                    
058900                                 TO MOD-KDCMDVAL-RAD-ATTR(INDX)           
059000       END-IF                                                             
059100                                                                          
059200       IF RESP-KDCMDVAL-LINE(INDX) = SPACE                                
059300         MOVE MFS-RENSA-FAELT    TO MOD-KDCMDVAL-RAD(INDX)                
059400       ELSE                                                               
059500         IF RESP-KDCMDVAL-LINE(INDX) = ALL '+'                            
059600           MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD(INDX)               
059700         ELSE                                                             
059800           MOVE RESP-KDCMDVAL-LINE(INDX) TO MOD-KDCMDVAL-RAD(INDX)        
059900         END-IF                                                           
060000       END-IF                                                             
060100                                                                          
060200       IF RESP-IDARTNR-LINE (INDX) = SPACE                                
060300         MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-RAD   (INDX)         
060400       ELSE                                                               
060500         IF RESP-IDARTNR-LINE (INDX) = ALL '+'                            
060600           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR-RAD   (INDX)         
060700         ELSE                                                             
060800           MOVE RESP-IDARTNR-LINE(INDX) TO MOD-IDARTNR-RAD (INDX)         
060900         END-IF                                                           
061000       END-IF                                                             
061100                                                                          
061200       IF RESP-FLDIVKLI-LINE (INDX) = SPACE                               
061300         MOVE MFS-RENSA-FAELT         TO MOD-FLDIVKLI-RAD  (INDX)         
061400       ELSE                                                               
061500         IF RESP-FLDIVKLI-LINE (INDX) = ALL '+'                           
061600           MOVE MFS-ROER-EJ-FAELT     TO MOD-FLDIVKLI-RAD  (INDX)         
061700         ELSE                                                             
061800           MOVE RESP-FLDIVKLI-LINE(INDX) TO MOD-FLDIVKLI-RAD(INDX)        
061900         END-IF                                                           
062000       END-IF                                                             
062100                                                                          
062200       IF RESP-IDINLVGN-LINE (INDX) = SPACE                               
062300         MOVE MFS-RENSA-FAELT         TO MOD-IDINLVGN-RAD  (INDX)         
062400       ELSE                                                               
062500         IF RESP-IDINLVGN-LINE (INDX) = ALL '+'                           
062600           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDINLVGN-RAD  (INDX)         
062700         ELSE                                                             
062800           MOVE RESP-IDINLVGN-LINE(INDX) TO MOD-IDINLVGN-RAD(INDX)        
062900         END-IF                                                           
063000       END-IF                                                             
063100                                                                          
063200       MOVE RESP-BEART-LINE (INDX)    TO MOD-BEART-RAD     (INDX)         
063300       IF RESP-BEART-LINE (INDX) = SPACE                                  
063400         MOVE MFS-RENSA-FAELT         TO MOD-BEART-RAD     (INDX)         
063500       END-IF                                                             
063600       IF RESP-BEART-LINE (INDX) = ALL '+'                                
063700         MOVE MFS-ROER-EJ-FAELT       TO MOD-BEART-RAD     (INDX)         
063800       END-IF                                                             
063900                                                                          
064000       IF RESP-IDLOPNRM-LINE (INDX) = SPACE                               
064100         MOVE MFS-RENSA-FAELT         TO MOD-IDLOPNRM-RAD  (INDX)         
064200       ELSE                                                               
064300         IF RESP-IDLOPNRM-LINE (INDX) = ALL '+'                           
064400           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLOPNRM-RAD  (INDX)         
064500         ELSE                                                             
064600           MOVE RESP-IDLOPNRM-LINE(INDX) TO MOD-IDLOPNRM-RAD(INDX)        
064700         END-IF                                                           
064800       END-IF                                                             
064900                                                                          
065000       IF RESP-IDRADNR-LINE (INDX) = SPACE                                
065100         MOVE MFS-RENSA-FAELT         TO MOD-IDRADNR-RAD   (INDX)         
065200       ELSE                                                               
065300         IF RESP-IDRADNR-LINE (INDX) = ALL '+'                            
065400           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDRADNR-RAD   (INDX)         
065500         ELSE                                                             
065600           MOVE RESP-IDRADNR-LINE(INDX) TO MOD-IDRADNR-RAD (INDX)         
065700         END-IF                                                           
065800       END-IF                                                             
065900                                                                          
066000       IF RESP-IDLEVNR-LINE(INDX) = SPACE                                 
066100         MOVE MFS-RENSA-FAELT         TO MOD-IDLEVNR-RAD   (INDX)         
066200       ELSE                                                               
066300         IF RESP-IDLEVNR-LINE(INDX) = ALL '+'                             
066400           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVNR-RAD   (INDX)         
066500         ELSE                                                             
066600           MOVE RESP-IDLEVNR-LINE(INDX) TO MOD-IDLEVNR-RAD (INDX)         
066700         END-IF                                                           
066800       END-IF                                                             
066900                                                                          
067000       IF RESP-IDOKOLLI-LINE (INDX) = SPACE                               
067100         MOVE MFS-RENSA-FAELT         TO MOD-IDOKOLLI-RAD (INDX)          
067200       ELSE                                                               
067300         IF RESP-IDOKOLLI-LINE (INDX) = ALL '+'                           
067400           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDOKOLLI-RAD (INDX)          
067500         ELSE                                                             
067600           MOVE RESP-IDOKOLLI-LINE(INDX) TO MOD-IDOKOLLI-RAD(INDX)        
067700         END-IF                                                           
067800       END-IF                                                             
067900                                                                          
068000       MOVE RESP-FLINLFB-LINE(INDX) TO MOD-FLINLFB-RAD(INDX)              
068100       IF RESP-FLINLFB-LINE(INDX) = YES                                   
068200         IF NOT ENGLISH-TEXT                                              
068300           MOVE JA                  TO MOD-FLINLFB-RAD(INDX)              
068400         END-IF                                                           
068500       END-IF                                                             
068600       IF RESP-FLINLFB-LINE(INDX) = SPACE                                 
068700         MOVE MFS-RENSA-FAELT       TO MOD-FLINLFB-RAD(INDX)              
068800       END-IF                                                             
068900       IF RESP-FLINLFB-LINE(INDX) = ALL '+'                               
069000         MOVE MFS-ROER-EJ-FAELT     TO MOD-FLINLFB-RAD(INDX)              
069100       END-IF                                                             
069200                                                                          
069300       MOVE RESP-FLKVROS-LINE(INDX) TO MOD-FLKVROS-RAD(INDX)              
069400       IF RESP-FLKVROS-LINE(INDX) = YES                                   
069500         IF NOT ENGLISH-TEXT                                              
069600           MOVE JA                  TO MOD-FLKVROS-RAD(INDX)              
069700         END-IF                                                           
069800       END-IF                                                             
069900       IF RESP-FLKVROS-LINE(INDX) = SPACE                                 
070000         MOVE MFS-RENSA-FAELT       TO MOD-FLKVROS-RAD(INDX)              
070100       END-IF                                                             
070200       IF RESP-FLKVROS-LINE(INDX) = ALL '+'                               
070300         MOVE MFS-ROER-EJ-FAELT     TO MOD-FLKVROS-RAD(INDX)              
070400       END-IF                                                             
070500                                                                          
070600       MOVE RESP-FLPREPFF-LINE(INDX) TO MOD-FLPREPFF-RAD (INDX)           
070700       IF RESP-FLPREPFF-LINE (INDX) = SPACE                               
070800         MOVE MFS-RENSA-FAELT        TO MOD-FLPREPFF-RAD (INDX)           
070900       END-IF                                                             
071000       IF RESP-FLPREPFF-LINE (INDX) = ALL '+'                             
071100         MOVE MFS-ROER-EJ-FAELT      TO MOD-FLPREPFF-RAD (INDX)           
071200       END-IF                                                             
071300                                                                          
071400       ADD +1                    TO INDX                                  
071500     END-PERFORM                                                          
071600                                                                          
071700*RENSA RESTERANDE RADER                                                   
071800     PERFORM UNTIL INDX            >  MAX-INDX                            
071900       MOVE MFS-STAENG-FAELT   TO MOD-KDCMDVAL-RAD-ATTR(INDX)             
072000       MOVE MFS-RENSA-FAELT    TO MOD-KDCMDVAL-RAD(INDX)                  
072100                                  MOD-IDARTNR-RAD   (INDX)                
072200                                  MOD-IDLEVNR-RAD   (INDX)                
072300                                  MOD-IDOKOLLI-RAD  (INDX)                
072400                                  MOD-FLPREPFF-RAD  (INDX)                
072500                                  MOD-FLKVROS-RAD   (INDX)                
072600                                  MOD-FLINLFB-RAD   (INDX)                
072700                                  MOD-FLDIVKLI-RAD  (INDX)                
072800                                  MOD-IDINLVGN-RAD  (INDX)                
072900                                  MOD-BEART-RAD     (INDX)                
073000                                  MOD-IDLOPNRM-RAD  (INDX)                
073100                                  MOD-IDRADNR-RAD   (INDX)                
073200       ADD +1                    TO INDX                                  
073300     END-PERFORM                                                          
073400                                                                          
073500     IF RESP-KVINLCAR = ALL '+'                                           
073600       MOVE MFS-ROER-EJ-FAELT       TO MOD-KVINLCAR                       
073700     ELSE                                                                 
073800       IF RESP-KVINLCAR = SPACE                                           
073900         MOVE MFS-RENSA-FAELT       TO MOD-KVINLCAR                       
074000       ELSE                                                               
074100         MOVE RESP-KVINLCAR         TO MOD-KVINLCAR                       
074200       END-IF                                                             
074300     END-IF                                                               
074400                                                                          
074500     IF RESP-KVRADER-HIT = ALL '+'                                        
074600       MOVE MFS-ROER-EJ-FAELT       TO MOD-KVRADER-HIT                    
074700     ELSE                                                                 
074800       IF RESP-KVRADER-HIT = SPACE                                        
074900         MOVE MFS-RENSA-FAELT       TO MOD-KVRADER-HIT                    
075000       ELSE                                                               
075100         MOVE RESP-KVRADER-HIT      TO MOD-KVRADER-HIT                    
075200       END-IF                                                             
075300     END-IF                                                               
075400                                                                          
075500     IF RESP-KVRADER-TOT = ALL '+'                                        
075600       MOVE MFS-ROER-EJ-FAELT       TO MOD-KVRADER-TOT                    
075700     ELSE                                                                 
075800       IF RESP-KVRADER-TOT = SPACE                                        
075900         MOVE MFS-RENSA-FAELT       TO MOD-KVRADER-TOT                    
076000       ELSE                                                               
076100         MOVE RESP-KVRADER-TOT      TO MOD-KVRADER-TOT                    
076200       END-IF                                                             
076300     END-IF                                                               
076400                                                                          
076500     IF RESP-KVRADER-PRIO = ALL '+'                                       
076600       MOVE MFS-ROER-EJ-FAELT       TO MOD-KVRADER-PRIO                   
076700     ELSE                                                                 
076800       IF RESP-KVRADER-PRIO = SPACE                                       
076900         MOVE MFS-RENSA-FAELT       TO MOD-KVRADER-PRIO                   
077000       ELSE                                                               
077100         MOVE RESP-KVRADER-PRIO     TO MOD-KVRADER-PRIO                   
077200       END-IF                                                             
077300     END-IF                                                               
077400                                                                          
077500     IF RESP-IDLEVNR-START      = ALL '+'                                 
077600       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDLEVNR-ENTER                   
077700     ELSE                                                                 
077800       MOVE RESP-IDLEVNR-START     TO MOD-IDLEVNR-ENTER                   
077900     END-IF                                                               
078000     IF RESP-IDFS-START         = ALL '+'                                 
078100       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDFS-ENTER                      
078200     ELSE                                                                 
078300       MOVE RESP-IDFS-START        TO MOD-IDFS-ENTER                      
078400     END-IF                                                               
078500     IF RESP-TIAVIDAT-START     = ALL '+'                                 
078600       MOVE MFS-ROER-EJ-FAELT      TO MOD-TIAVIDAT-ENTER                  
078700     ELSE                                                                 
078800       MOVE RESP-TIAVIDAT-START    TO MOD-TIAVIDAT-ENTER                  
078900     END-IF                                                               
079000     IF RESP-IDRADNR-INL-START  = ALL '+'                                 
079100       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRADNR-INL-ENTER               
079200     ELSE                                                                 
079300       MOVE RESP-IDRADNR-INL-START TO MOD-IDRADNR-INL-ENTER               
079400     END-IF                                                               
079500     IF RESP-IDRADNR-START      = ALL '+'                                 
079600       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRADNR-ENTER                   
079700     ELSE                                                                 
079800       MOVE RESP-IDRADNR-START     TO MOD-IDRADNR-ENTER                   
079900     END-IF                                                               
080000     IF RESP-KDINLPRIO-START     = ALL '+'                                
080100       MOVE MFS-ROER-EJ-FAELT      TO MOD-KDINLPRIO-ENTER                 
080200     ELSE                                                                 
080300       MOVE RESP-KDINLPRIO-START   TO MOD-KDINLPRIO-ENTER                 
080400     END-IF                                                               
080500                                                                          
080600     IF RESP-IDLEVNR-NEXT        = ALL '+'                                
080700       MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVNR-NEXT                   
080800     ELSE                                                                 
080900       MOVE RESP-IDLEVNR-NEXT       TO MOD-IDLEVNR-NEXT                   
081000     END-IF                                                               
081100     IF RESP-IDFS-NEXT           = ALL '+'                                
081200       MOVE MFS-ROER-EJ-FAELT       TO MOD-IDFS-NEXT                      
081300     ELSE                                                                 
081400       MOVE RESP-IDFS-NEXT          TO MOD-IDFS-NEXT                      
081500     END-IF                                                               
081600     IF RESP-TIAVIDAT-NEXT       = ALL '+'                                
081700       MOVE MFS-ROER-EJ-FAELT       TO MOD-TIAVIDAT-NEXT                  
081800     ELSE                                                                 
081900       MOVE RESP-TIAVIDAT-NEXT      TO MOD-TIAVIDAT-NEXT                  
082000     END-IF                                                               
082100     IF RESP-IDRADNR-INL-NEXT    = ALL '+'                                
082200       MOVE MFS-ROER-EJ-FAELT       TO MOD-IDRADNR-INL-NEXT               
082300     ELSE                                                                 
082400       MOVE RESP-IDRADNR-INL-NEXT   TO MOD-IDRADNR-INL-NEXT               
082500     END-IF                                                               
082600     IF RESP-IDRADNR-NEXT        = ALL '+'                                
082700       MOVE MFS-ROER-EJ-FAELT       TO MOD-IDRADNR-NEXT                   
082800     ELSE                                                                 
082900       MOVE RESP-IDRADNR-NEXT       TO MOD-IDRADNR-NEXT                   
083000     END-IF                                                               
083100     IF RESP-KDINLPRIO-NEXT      = ALL '+'                                
083200       MOVE MFS-ROER-EJ-FAELT       TO MOD-KDINLPRIO-NEXT                 
083300     ELSE                                                                 
083400       MOVE RESP-KDINLPRIO-NEXT     TO MOD-KDINLPRIO-NEXT                 
083500     END-IF                                                               
083600                                                                          
083700     IF MFS-UPDATE                                                        
083800       IF RESP-IDLEVNR-KOLLI-KEY = SPACE                                  
083900         MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-KOLLI-UT                  
084000                                    MOD-IDOKOLLI-UT                       
084100       ELSE                                                               
084200         MOVE RESP-IDLEVNR-KOLLI-KEY TO MOD-IDLEVNR-KOLLI-UT              
084300         MOVE RESP-IDOKOLLI-KEY      TO MOD-IDOKOLLI-UT                   
084400       END-IF                                                             
084500                                                                          
084600       MOVE RESP-IDLOPNRM-KEY       TO MOD-IDLOPNRM-UT                    
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 MFS-RENSA-FAELT-UT SECTION.                                              
085100                                                                          
085200     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-ENTER                       
085300                                  MOD-IDLEVNR-NEXT                        
085400                                  MOD-IDFS-ENTER                          
085500                                  MOD-IDFS-NEXT                           
085600                                  MOD-TIAVIDAT-ENTER                      
085700                                  MOD-TIAVIDAT-NEXT                       
085800                                  MOD-IDRADNR-INL-ENTER                   
085900                                  MOD-IDRADNR-INL-NEXT                    
086000                                  MOD-IDRADNR-ENTER                       
086100                                  MOD-IDRADNR-NEXT                        
086200                                  MOD-KDINLPRIO-ENTER                     
086300                                  MOD-KDINLPRIO-NEXT                      
086400                                  MOD-KVINLCAR                            
086500                                  MOD-KVRADER-HIT                         
086600                                  MOD-KVRADER-TOT                         
086700                                  MOD-KVRADER-PRIO                        
086800     MOVE +1                   TO INDX                                    
086900     PERFORM UNTIL INDX        >  MAX-INDX                                
087000         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
087100         ADD +1                TO INDX                                    
087200     END-PERFORM                                                          
087300     .                                                                    
087400     EJECT                                                                
087500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
087600                                                                          
087700     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-RAD   (INDX)                
087800                                  MOD-IDLEVNR-RAD   (INDX)                
087900                                  MOD-IDOKOLLI-RAD  (INDX)                
088000                                  MOD-FLPREPFF-RAD  (INDX)                
088100                                  MOD-FLKVROS-RAD   (INDX)                
088200                                  MOD-FLINLFB-RAD   (INDX)                
088300                                  MOD-FLDIVKLI-RAD  (INDX)                
088400                                  MOD-IDINLVGN-RAD  (INDX)                
088500                                  MOD-BEART-RAD     (INDX)                
088600                                  MOD-IDLOPNRM-RAD  (INDX)                
088700                                  MOD-IDRADNR-RAD   (INDX)                
088800     .                                                                    
088900     EJECT                                                                
089000 MFS-RENSA-FAELT-IN SECTION.                                              
089100                                                                          
089200     MOVE +1                   TO INDX                                    
089300     PERFORM UNTIL INDX        >  MAX-INDX                                
089400         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD  (INDX)                
089500         ADD +1                TO INDX                                    
089600     END-PERFORM                                                          
089700     .                                                                    
089800     EJECT                                                                
089900* --- IMS SEKTIONER ---                                                   
090000     SKIP3                                                                
090100 IMS-GET-MSG SECTION.                                                     
090200                                                                          
090300     MOVE '  QC' TO GODK-STATUSKODER                                      
090400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
090500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     SKIP3                                                                
090900 IMS-INSERT-MSG SECTION.                                                  
091000                                                                          
091100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
091200       MOVE '0' TO MFS-KDHUVOMR                                           
091300     END-IF                                                               
091400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
091500     MOVE SPACE TO GODK-STATUSKODER                                       
091600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
091700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     SKIP3                                                                
092100 IMS-STATUSKONTROLL SECTION.                                              
092200                                                                          
092300     SET STATUS-IX TO 1                                                   
092400     SEARCH GODK-STATUS                                                   
092500       AT END                                                             
092600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
092700         DELIMITED BY SIZE INTO FELTEXT                                   
092800         CALL FELLOG                                                      
092900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
093000         CONTINUE                                                         
093100     END-SEARCH                                                           
093200     .                                                                    
