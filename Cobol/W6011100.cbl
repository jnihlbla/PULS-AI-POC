000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6011100.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/02/17.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR R31 TRANSAKTIONER                                         
001100*        EN LEVERANTÖR EN AVI FLERA ARTIKLAR                              
001200*                                                                         
001300*    DETTA ÄR EN HUVUDMODUL SOM BARA HANTERAR MFS-DELEN OCH               
001400*    ANROPAR W6011110 SOM INNEHÅLLER ALL AFFÄRSLOGIK.                     
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W6T111                                              
001800*        MID:         W6I11101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W6O11101                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(08)   VALUE 'W6011100'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  WS-CURRENT-SECTION          PIC X(32)   VALUE 'CURRENT:'.            
003600 77  WS-CURRENT-IMS-SECTION      PIC X(24)   VALUE SPACE.                 
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 01  FILLER                     PIC X(16)   VALUE 'INDEX FÄLT  '.         
004300                                                                          
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +853  COMP SYNC.        
004600 77  RAD-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  RAD-IX2                     PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  MAX-RAD                     PIC S9(4)  VALUE +36   COMP SYNC.        
004900 77  DC-IX                       PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
005100 77  WS-RAD-IX                   PIC  9(3)  VALUE  0.                     
005200 77  INDX-DISPLAY                PIC  9(9)  VALUE  0.                     
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500 01  FILLER                     PIC X(16)   VALUE 'ARBETSFÄLT  '.         
005600                                                                          
005700 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005800 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005900 77  WS-KDRT                     PIC X(2)    VALUE SPACE.                 
006000 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
006100 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
006200 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
006300 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
006400 77  WS-FLGODK                   PIC X(1)    VALUE SPACE.                 
006500 77  WS-MID-IDARTNR              PIC 9(8)    VALUE ZERO.                  
006600 77  WS-MID-KVAVIS               PIC 9(6)    VALUE ZERO.                  
006700                                                                          
006800                                                                          
006900 01  FILLER                     PIC X(16)   VALUE 'SWITCHAR    '.         
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 01  FILLER                     PIC X(16)   VALUE 'GODK TRANSAR'.         
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  EGEN-MID                            VALUE '6111'.                
007900     88  GODK-MID                            VALUE '6111' '6112'          
008000                                                   '6113' '6114'          
008100                                                   '6115' '6116'          
008200                                                   '6118' '6119'.         
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500 01  FILLER                     PIC X(16)   VALUE 'PARM SUBPGM '.         
008600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009300     03  W6011110                PIC X(8)    VALUE 'W6011110'.            
009400     EJECT                                                                
009500*    --- PARAMETERS TO ABEND                                              
009600                                                                          
009700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010000     SKIP3                                                                
010100*01 -COPY WMSGINIT                                                        
010200     SKIP3                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010400*01 -COPY WMEDAREA                                                        
010500     SKIP3                                                                
010600 01  MESSAGE-CODES.                                                       
010700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  ERR-PART-IS-MISSING     PIC X(3)    VALUE '017'.                 
011000     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
011100     03  ERR-IDFTG-IS-WRONG      PIC X(3)    VALUE '088'.                 
011200     03  ERR-DIRECT-SUPPLIER     PIC X(3)    VALUE '306'.                 
011300     03  ERR-IN-LINE-ONE         PIC X(3)    VALUE '184'.                 
011400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011500     03  ERR-SUPPL-MISSING       PIC X(3)    VALUE '273'.                 
011600     03  ERR-ART-PRIS-NOLL       PIC X(3)    VALUE '301'.                 
011610     03  ERR-ART-WRNG-CURR       PIC X(3)    VALUE '151'.                 
011700     03  ERR-IS-INVALID          PIC X(3)    VALUE '492'.                 
011800     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
011900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012000     03  INF-AVISERING-FINNS     PIC X(3)    VALUE '201'.                 
012100     03  INF-ARTIKEL-ERSATT      PIC X(3)    VALUE '220'.                 
012200     03  INF-AVROP-SAKNAS        PIC X(3)    VALUE '285'.                 
012300                                                                          
012400 01 WS-IDMSG-ERROR                 PIC X(3).                              
012500     88  R-ERR-EXEC-AND-NO-DATA                VALUE '014'.               
012600     88  R-CORR-HILITE-FLDS                    VALUE '020'.               
012700     88  R-ERR-WRONG-KEY                       VALUE '022'.               
012800     88  R-ERR-HAS-TO-BE-NUMERIC               VALUE '024'.               
012900     88  R-ERR-NOT-FOUND                       VALUE '025'.               
013000     88  R-ERR-FINNS-REDAN                     VALUE '030'.               
013200     88  R-ERR-ART-PRIS-NOLL                   VALUE '301'.               
013210     88  R-ERR-ART-WRNG-CURR                   VALUE '151'.               
013300     88  R-ERR-IN-LINE-ONE                     VALUE '337'.               
013400     88  R-ERR-IS-INVALID                      VALUE '023'.               
013500     88  R-PRICE-INFO-MISSING                  VALUE '260'.               
013510     88  R-PRICE-WRNG-CURRENCY                 VALUE '426'.               
013600     88  R-ERR-DIRECT-SUPPLIER                 VALUE '349'.               
013700*                                                                         
013800 01 WS-IDMSG-INFO                  PIC X(3).                              
013900     88  R-INF-UPDATE-DONE                     VALUE '001'.               
014000     88  R-INF-UPDATE-NOT-DONE                 VALUE '004'.               
014100     88  R-INF-FINNS-REDAN                     VALUE '030'.               
014200     88  R-INF-ARTIKEL-ERSATT                  VALUE '223'.               
014300     88  R-INF-AVROP-SAKNAS                    VALUE '336'.               
014400     88  R-INF-PART-OTHER-COMPANY              VALUE '360'.               
014500     EJECT                                                                
014600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014700*                                                                         
014800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014900     SKIP3                                                                
015000*01  MID -COPY W6I11101                                                   
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015300     SKIP3                                                                
015400*01  -COPY WMSGAREA                                                       
015500     EJECT                                                                
015600     03  MOD REDEFINES MSG-AREA.                                          
015700*      05  -COPY W6O11101                                                 
015800     EJECT                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016000     SKIP3                                                                
016100*01  -COPY WMFSAREA                                                       
016200     EJECT                                                                
016300*    --- AREOR TILL W6011110 SUBPROGRAM                                   
016400*                                                                         
016500 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
016600 01  REQU-AREA.                                                           
016700*    03 -COPY WZ01REQU                                                    
016800*    03 -COPY W60111I1                                                    
016900     SKIP3                                                                
017000                                                                          
017100                                                                          
017200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
017300 01  RESP-AREA.                                                           
017400*    03 -COPY WZ01RESP                                                    
017500*    03 -COPY W60111O1                                                    
017600     SKIP3                                                                
017700*                                                                         
017800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100*    --- STATUS-KOD FRÅN IMS                                              
018200 01  SSA1                        PIC X(256).                              
018300                                                                          
018400 01  STATUS-WS                   PIC XX.                                  
018500     88 SEGMENT-FINNS                        VALUE '  '.                  
018600     88 SEGMENT-SAKNAS                       VALUE 'GE'.                  
018700     88 BASEN-SLUT                           VALUE 'GB'.                  
018800                                                                          
018900 01  GODK-STATUSKODER.                                                    
019000     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
019100                                                                          
019200     EJECT                                                                
019300*    --- IMS FUNKTIONSKODER                                               
019400*01  -COPY W0003                                                          
019500     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700                                                                          
019800 01 -COPY W0009     -PRE MSG-                                             
019900                                                                          
020000 01  USEA-PCB                    PIC X.                                   
020100                                                                          
020200 01  WDB6-LEV-PCB                PIC X.                                   
020300                                                                          
020400 01  WDB6-PCB                    PIC X.                                   
020500                                                                          
020600*   PCB'ER FÖR SUB PGM W611REG                                            
020700                                                                          
020800 01  REG-INLA1-PCB               PIC X.                                   
020900                                                                          
021000 01  REG-INLA2-PCB               PIC X.                                   
021100                                                                          
021200 01  REG-INLA3-PCB               PIC X.                                   
021300                                                                          
021400 01  REG-LEVA-PCB                PIC X.                                   
021500                                                                          
021600 01  REG-ARTC-PCB                PIC X.                                   
021700                                                                          
021800 01  REG-BENA-PCB                PIC X.                                   
021900                                                                          
022000 01  REG-INLB-PCB                PIC X.                                   
022100                                                                          
022200 01  REG-ARTS-PCB                PIC X.                                   
022300                                                                          
022310 01  REG-WDK7-PCB                PIC X.                                   
022320                                                                          
022330 01  REG-WDB6-PCB                PIC X.                                   
022331                                                                          
022332*01    -COPY W0008     -PRE 9305-                                         
022333     05  FILLER                  PIC X(30).                               
022340                                                                          
022400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
022500                           WDB6-LEV-PCB  WDB6-PCB                         
022600                           REG-INLA1-PCB REG-INLA2-PCB                    
022700                           REG-INLA3-PCB REG-LEVA-PCB                     
022800                           REG-ARTC-PCB  REG-BENA-PCB                     
022900                           REG-INLB-PCB  REG-ARTS-PCB                     
022910                           REG-WDK7-PCB  REG-WDB6-PCB                     
022920                           9305-PCB.                                      
023000                                                                          
023100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
023200                           WDB6-LEV-PCB  WDB6-PCB                         
023300                           REG-INLA1-PCB REG-INLA2-PCB                    
023400                           REG-INLA3-PCB REG-LEVA-PCB                     
023500                           REG-ARTC-PCB  REG-BENA-PCB                     
023600                           REG-INLB-PCB  REG-ARTS-PCB                     
023610                           REG-WDK7-PCB  REG-WDB6-PCB                     
023620                           9305-PCB.                                      
023700 MAIN SECTION.                                                            
023800                                                                          
023900     PERFORM IMS-GET-MSG                                                  
024000     IF SEGMENT-FINNS                                                     
024100       PERFORM A-INIT                                                     
024200       PERFORM B-FLYTTA-NYCKLAR                                           
024300       PERFORM C-INIT-REQU                                                
024400*                                                                         
024500       IF MFS-ENTER AND EGEN-MID                                          
024600                                                                          
024700         PERFORM G-CALL-SUBROUTINE                                        
024800                                                                          
024900       ELSE                                                               
025000         IF HELP-MID                                                      
025100           PERFORM F-LAES-VISA-INFO                                       
025200         ELSE                                                             
025300           PERFORM MFS-RENSA-NYCKLAR                                      
025400           PERFORM MFS-RENSA-FAELT-IN                                     
025500         END-IF                                                           
025600       END-IF                                                             
025700*                                                                         
025800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025900       PERFORM IMS-INSERT-MSG                                             
026000     END-IF                                                               
026100                                                                          
026200     MOVE ZERO TO RETURN-CODE                                             
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 A-INIT SECTION.                                                          
026700                                                                          
026800     IF MSG-DUBBLA-TRANSKODER                                             
026900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11101                 
027000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027200     ELSE                                                                 
027300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11101                  
027400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027600     END-IF                                                               
027700                                                                          
027800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
028000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028100                                                                          
028200     MOVE LOW-VALUE TO MSG-AREA                                           
028300     MOVE 'W6O111N1' TO MFS-IDMOD                                         
028400     MOVE '6111' TO MOD-IDTRANS                                           
028500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028600                                                                          
028700     IF EGEN-MID OR HELP-MID                                              
028800       CONTINUE                                                           
028900     ELSE                                                                 
029000       MOVE SPACE TO MFS-KDTRTYP                                          
029100       MOVE '7' TO MFS-IDPFK                                              
029200     END-IF                                                               
029300                                                                          
029400     PERFORM MFS-FORM-ATTR                                                
029500                                                                          
029600     PERFORM AA-INIT-NYCKLAR                                              
029700                                                                          
029800     IF MSGI-IDLAND-SPR = 'GB'                                            
029900       MOVE +2 TO SPRAK-IX                                                
030000       MOVE 'GB ' TO MED-IDSKYLT                                          
030100     ELSE                                                                 
030200       MOVE +1 TO SPRAK-IX                                                
030300       MOVE 'S  ' TO MED-IDSKYLT                                          
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700*----------------------------------------------------------------*        
030800 AA-INIT-NYCKLAR SECTION.                                                 
030900                                                                          
031000     MOVE ALL '+' TO MSGI-WMSGINIT                                        
031100     MOVE '013'                  TO MSGI-KDCALL                           
031200     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
031300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
031400     MOVE '6111'                 TO MSGI-IDTRANS                          
031500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031600     .                                                                    
031700     EJECT                                                                
031800 B-FLYTTA-NYCKLAR SECTION.                                                
031900                                                                          
032000     PERFORM BA-FLYTTA-IDLEVNR                                            
032100     PERFORM BB-FLYTTA-KDRT                                               
032200     PERFORM BC-FLYTTA-IDFS                                               
032300     PERFORM BD-FLYTTA-TIAVIDAT                                           
032400     PERFORM BE-FLYTTA-IDLBBET                                            
032500     PERFORM BF-FLYTTA-FLGODK                                             
032600     PERFORM BG-FLYTTA-IDDC                                               
032700                                                                          
032800     MOVE MFS-RENSA-FAELT          TO MOD-ADINLOMR-PRT-IN                 
032900     IF MID-ADINLOMR-PRT-IN        = ALL '+'                              
033000         MOVE MID-ADINLOMR-PRT-UT  TO WS-ADINLOMR-PRT                     
033100     ELSE                                                                 
033200         MOVE MID-ADINLOMR-PRT-IN  TO WS-ADINLOMR-PRT                     
033300         MOVE      SPACE           TO MFS-KDTRTYP                         
033400     END-IF                                                               
033500                                                                          
033600     PERFORM BH-FLYTTA-OEVRIGA-NYCKLAR                                    
033700                                                                          
033800     IF GODK-MID                                                          
033900       CONTINUE                                                           
034000     ELSE                                                                 
034100       PERFORM MFS-RENSA-NYCKLAR                                          
034200     END-IF                                                               
034300                                                                          
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700 BA-FLYTTA-IDLEVNR SECTION.                                               
034800                                                                          
034900     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
035000                                                                          
035100     IF MID-IDLEVNR-IN         = ALL '+'                                  
035200         MOVE MID-IDLEVNR-UT   TO WS-IDLEVNR                              
035300     ELSE                                                                 
035400         MOVE MID-IDLEVNR-IN   TO WS-IDLEVNR                              
035500         MOVE     SPACE        TO MFS-KDTRTYP                             
035600     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900                                                                          
036000 BB-FLYTTA-KDRT   SECTION.                                                
036100                                                                          
036200     MOVE MFS-RENSA-FAELT        TO MOD-KDRT-IN                           
036300                                                                          
036400     IF GODK-MID OR HELP-MID                                              
036500         IF MID-KDRT-IN          = ALL '+'                                
036600             MOVE MID-KDRT-UT    TO WS-KDRT                               
036700         ELSE                                                             
036800             MOVE MID-KDRT-IN    TO WS-KDRT                               
036900             MOVE   SPACE        TO MFS-KDTRTYP                           
037000         END-IF                                                           
037100      ELSE                                                                
037200         MOVE ZERO             TO WS-KDRT                                 
037300     END-IF                                                               
037400                                                                          
037500     .                                                                    
037600     EJECT                                                                
037700                                                                          
037800 BC-FLYTTA-IDFS   SECTION.                                                
037900                                                                          
038000     MOVE MFS-RENSA-FAELT      TO MOD-IDFS-IN                             
038100                                                                          
038200     IF MID-IDFS-IN            = ALL '+'                                  
038300         MOVE MID-IDFS-UT      TO WS-IDFS                                 
038400     ELSE                                                                 
038500         MOVE MID-IDFS-IN      TO WS-IDFS                                 
038600         MOVE   SPACE          TO MFS-KDTRTYP                             
038700     END-IF                                                               
038800                                                                          
038900     .                                                                    
039000     EJECT                                                                
039100                                                                          
039200 BD-FLYTTA-TIAVIDAT SECTION.                                              
039300                                                                          
039400     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-IN                         
039500                                                                          
039600     IF MID-TIAVIDAT-IN        = ALL '+'                                  
039700         MOVE MID-TIAVIDAT-UT  TO WS-TIAVIDAT                             
039800     ELSE                                                                 
039900         MOVE MID-TIAVIDAT-IN  TO WS-TIAVIDAT                             
040000         MOVE   SPACE          TO MFS-KDTRTYP                             
040100     END-IF                                                               
040200                                                                          
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600 BE-FLYTTA-IDLBBET SECTION.                                               
040700                                                                          
040800     MOVE MFS-RENSA-FAELT          TO MOD-IDLBBET-IN                      
040900                                                                          
041000     IF EGEN-MID OR HELP-MID                                              
041100         IF MID-IDLBBET-IN         = ALL '+'                              
041200             MOVE MID-IDLBBET-UT   TO WS-IDLBBET                          
041300         ELSE                                                             
041400             MOVE MID-IDLBBET-IN   TO WS-IDLBBET                          
041500             MOVE   SPACE          TO MFS-KDTRTYP                         
041600         END-IF                                                           
041700      ELSE                                                                
041800         MOVE SPACE                TO WS-IDLBBET                          
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 BF-FLYTTA-FLGODK SECTION.                                                
042300                                                                          
042400     MOVE MFS-RENSA-FAELT      TO MOD-FLGODK-IN                           
042500                                                                          
042600     IF EGEN-MID                                                          
042700         IF MID-FLGODK-IN       = ALL '+'                                 
042800             MOVE SPACE         TO WS-FLGODK                              
042900         ELSE                                                             
043000             MOVE MID-FLGODK-IN TO WS-FLGODK                              
043100             MOVE SPACE         TO MFS-KDTRTYP                            
043200         END-IF                                                           
043300     ELSE                                                                 
043400         MOVE SPACE             TO WS-FLGODK                              
043500     END-IF                                                               
043600                                                                          
043700     .                                                                    
043800     EJECT                                                                
043900                                                                          
044000 BG-FLYTTA-IDDC   SECTION.                                                
044100                                                                          
044200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
044300                                                                          
044400     IF EGEN-MID OR HELP-MID                                              
044500       IF MID-IDDC-IN = ALL '+'                                           
044600         MOVE MSGI-IDDC   TO WS-IDDC                                      
044700       ELSE                                                               
044800         MOVE MID-IDDC-IN TO WS-IDDC                                      
044900         MOVE SPACE       TO MFS-KDTRTYP                                  
045000       END-IF                                                             
045100     ELSE                                                                 
045200       IF GODK-MID                                                        
045300         MOVE MSGI-IDDC   TO WS-IDDC                                      
045400       ELSE                                                               
045500         MOVE SPACE       TO WS-IDDC                                      
045600       END-IF                                                             
045700     END-IF                                                               
045800                                                                          
045900     .                                                                    
046000     EJECT                                                                
046100                                                                          
046200 BH-FLYTTA-OEVRIGA-NYCKLAR  SECTION.                                      
046300                                                                          
046400*    --- ÖVRIGA NYCKLAR I BILDGRUPPEN                                     
046500                                                                          
046600     MOVE MFS-RENSA-FAELT      TO MOD-FLKLIVIS-IN                         
046700                                  MOD-IDLOPNRM-IN                         
046800                                                                          
046900     IF MID-FLKLIVIS-IN   = ALL '+'                                       
047000         MOVE MID-FLKLIVIS-UT  TO MOD-FLKLIVIS-UT                         
047100     ELSE                                                                 
047200         MOVE MID-FLKLIVIS-IN  TO MOD-FLKLIVIS-UT                         
047300     END-IF                                                               
047400                                                                          
047500     IF MID-IDLOPNRM-IN   = ALL '+'                                       
047600         MOVE MID-IDLOPNRM-UT  TO MOD-IDLOPNRM-UT                         
047700     ELSE                                                                 
047800         MOVE MID-IDLOPNRM-IN  TO MOD-IDLOPNRM-UT                         
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200                                                                          
048300 C-INIT-REQU                    SECTION.                                  
048400     MOVE 'C-INIT-REQU          '  TO WS-CURRENT-SECTION                  
048500                                                                          
048600     MOVE WS-IDLEVNR               TO REQU-IDLEVNR-KEY                    
048700     MOVE WS-KDRT                  TO REQU-KDRT-KEY                       
048800     MOVE WS-IDFS                  TO REQU-IDFS-KEY                       
048900     MOVE WS-TIAVIDAT              TO REQU-TIAVIDAT-KEY                   
049000     MOVE WS-ADINLOMR-PRT          TO REQU-ADINLOMR-PRT-KEY               
049100     MOVE WS-IDDC                  TO REQU-IDDC-KEY                       
049200     MOVE WS-IDLBBET               TO REQU-IDLBBET                        
049300     MOVE WS-FLGODK                TO REQU-FLGODK                         
049400                                                                          
049500     MOVE +1 TO RAD-IX                                                    
049600     PERFORM UNTIL RAD-IX > MAX-RAD                                       
049700       INSPECT MID-IDARTNR(RAD-IX)                                        
049800       REPLACING LEADING SPACE BY ZERO                                    
049900       MOVE MID-IDARTNR(RAD-IX) TO REQU-IDARTNR-LINE(RAD-IX)              
050000                                                                          
050100       INSPECT MID-KVAVIS (RAD-IX)                                        
050200       REPLACING LEADING SPACE BY ZERO                                    
050300       MOVE MID-KVAVIS (RAD-IX) TO REQU-KVAVIS-LINE (RAD-IX)              
050400                                                                          
050500       ADD +1                    TO RAD-IX                                
050600     END-PERFORM                                                          
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000 F-LAES-VISA-INFO        SECTION.                                         
051100                                                                          
051200     IF MID-FLGODK-IN               = ALL '+'                             
051300         MOVE MFS-RENSA-FAELT       TO MOD-FLGODK-IN                      
051400     ELSE                                                                 
051500         MOVE MID-FLGODK-IN         TO MOD-FLGODK-IN                      
051600         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLGODK-IN-ATTR                 
051700     END-IF                                                               
051800                                                                          
051900     PERFORM FA-FLYTTA-KOLUMN-FAELT                                       
052000     .                                                                    
052100     EJECT                                                                
052200                                                                          
052300 FA-FLYTTA-KOLUMN-FAELT    SECTION.                                       
052400                                                                          
052500     MOVE +1                        TO RAD-IX                             
052600     PERFORM UNTIL RAD-IX          >  MAX-RAD                             
052700       IF MID-IDARTNR(RAD-IX)     =  ALL '+'                              
052800         MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN(RAD-IX)             
052900       ELSE                                                               
053000         MOVE MID-IDARTNR(RAD-IX) TO MOD-IDARTNR-IN(RAD-IX)               
053100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-IN-ATTR(RAD-IX)        
053200       END-IF                                                             
053300                                                                          
053400       IF MID-KVAVIS(RAD-IX)      =  ALL '+'                              
053500         MOVE MFS-RENSA-FAELT       TO MOD-KVAVIS-IN(RAD-IX)              
053600       ELSE                                                               
053700         MOVE MID-KVAVIS(RAD-IX) TO MOD-KVAVIS-IN   (RAD-IX)              
053800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVIS-IN-ATTR(RAD-IX)         
053900       END-IF                                                             
054000       ADD +1                       TO RAD-IX                             
054100     END-PERFORM                                                          
054200     .                                                                    
054300     EJECT                                                                
054400 G-CALL-SUBROUTINE       SECTION.                                         
054500     MOVE 'G-CALL-SUBROUTINE      ' TO WS-CURRENT-SECTION                 
054600                                                                          
054700     MOVE '001'                TO REQU-IDMSGVER                           
054800     MOVE 'E'                  TO REQU-KDPGMACT                           
054900     MOVE MSGI-IDUSER          TO REQU-IDUSER                             
055000                                                                          
055100     CALL W6011110 USING REQU-AREA     RESP-AREA     MAX-RAD              
055200                         MSG-PCB       WDB6-LEV-PCB  WDB6-PCB             
055300                         REG-INLA1-PCB REG-INLA2-PCB REG-INLA3-PCB        
055400                         REG-LEVA-PCB  REG-ARTC-PCB  REG-BENA-PCB         
055500                         REG-INLB-PCB  REG-ARTS-PCB REG-WDK7-PCB          
055510                         REG-WDB6-PCB  9305-PCB.                          
055600                                                                          
055700     PERFORM GA-SET-MSG-AND-HILIGHT                                       
055800                                                                          
055900     PERFORM GB-MOVE-OUTDATA-TO-MOD-AREA                                  
056000     .                                                                    
056100     EJECT                                                                
056200                                                                          
056300 GA-SET-MSG-AND-HILIGHT   SECTION.                                        
056400     MOVE 'GA-SET-MSG-AND-HILIGHT' TO WS-CURRENT-SECTION                  
056500*                                                                         
056600     MOVE RESP-IDMSG-ERROR         TO WS-IDMSG-ERROR                      
056700     MOVE RESP-IDMSG-INFO          TO WS-IDMSG-INFO                       
056800*                                                                         
056900     IF R-ERR-WRONG-KEY                                                   
057000       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
057100*                                                                         
057200       CALL WMEDKONV USING MED-WMEDAREA                                   
057300       MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                                 
057400*                                                                         
057500*      PERFORM MFS-LAES-IN-IGEN                                           
057600       PERFORM MFS-RENSA-FAELT-IN                                         
057700*      PERFORM MFS-RENSA-FAELT-UT                                         
057800*                                                                         
057900     ELSE                                                                 
058000*                                                                         
058100       IF R-ERR-NOT-FOUND                                                 
058200         IF RESP-IDELMT-ERROR = 'IDARTNR'                                 
058300           MOVE ERR-PART-IS-MISSING    TO MED-IDMFSFEL                    
058400         END-IF                                                           
058500         IF RESP-IDELMT-ERROR = 'IDLEVNR'                                 
058600           MOVE ERR-SUPPL-MISSING      TO MED-IDMFSFEL                    
058700         END-IF                                                           
058800       END-IF                                                             
058900*                                                                         
059000       IF R-ERR-EXEC-AND-NO-DATA                                          
059100         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
059200       END-IF                                                             
059300*                                                                         
059400       IF R-ERR-HAS-TO-BE-NUMERIC                                         
059500         MOVE ERR-NOT-NUMERIC     TO MED-IDMFSFEL                         
059600       END-IF                                                             
059700*                                                                         
059800       IF R-INF-PART-OTHER-COMPANY                                        
059900         MOVE ERR-IDFTG-IS-WRONG  TO MED-IDMFSFEL                         
060000       END-IF                                                             
060100*                                                                         
060800       IF R-CORR-HILITE-FLDS                                              
060900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
061000       END-IF                                                             
061100*                                                                         
061200       IF R-ERR-ART-PRIS-NOLL                                             
061300         MOVE ERR-ART-PRIS-NOLL     TO MED-IDMFSFEL                       
061400       END-IF                                                             
061410*                                                                         
061420       IF R-ERR-ART-WRNG-CURR                                             
061430         MOVE ERR-ART-WRNG-CURR     TO MED-IDMFSFEL                       
061440       END-IF                                                             
061500*                                                                         
061600       IF R-ERR-IN-LINE-ONE                                               
061700         MOVE ERR-IN-LINE-ONE       TO MED-IDMFSFEL                       
061800       END-IF                                                             
061900*                                                                         
062000       IF R-ERR-IS-INVALID                                                
062100         MOVE ERR-IS-INVALID        TO MED-IDMFSFEL                       
062200       END-IF                                                             
062300*                                                                         
062400       IF R-ERR-DIRECT-SUPPLIER                                           
062500         MOVE ERR-DIRECT-SUPPLIER TO MED-IDMFSFEL                         
062600       END-IF                                                             
062700*                                                                         
062800       IF R-INF-ARTIKEL-ERSATT                                            
062900         MOVE INF-ARTIKEL-ERSATT  TO MED-IDMFSINF                         
063000                                                                          
063100         MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLGODK-IN-ATTR                 
063200         PERFORM MFS-STAENG-KOL-FAELT                                     
063300       END-IF                                                             
063400*                                                                         
063500       IF R-PRICE-INFO-MISSING                                            
063600         MOVE ERR-ART-PRIS-NOLL  TO MED-IDMFSFEL                          
063700       END-IF                                                             
063710*                                                                         
063720       IF R-PRICE-WRNG-CURRENCY                                           
063730         MOVE ERR-ART-WRNG-CURR  TO MED-IDMFSFEL                          
063740       END-IF                                                             
063800*                                                                         
063900       IF R-INF-AVROP-SAKNAS                                              
064000         MOVE INF-AVROP-SAKNAS      TO MED-IDMFSFEL                       
064100                                                                          
064200         MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLGODK-IN-ATTR                 
064300         PERFORM MFS-STAENG-KOL-FAELT                                     
064400       END-IF                                                             
064500*                                                                         
064600*                                                                         
064700*INF                                                                      
064800*                                                                         
064900       IF R-INF-UPDATE-DONE                                               
065000         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
065100*                                                                         
065200         PERFORM MFS-FORM-ATTR                                            
065300         PERFORM MFS-RENSA-FAELT-IN                                       
065400       END-IF                                                             
065500*                                                                         
065600       IF R-INF-UPDATE-NOT-DONE                                           
065700         MOVE INF-UPDATE-NOT-DONE TO MED-IDMFSINF                         
065800         PERFORM MFS-LAES-IN-IGEN                                         
065900       END-IF                                                             
066000*                                                                         
066100*                                                                         
066600       IF R-INF-FINNS-REDAN AND RESP-IDELMT-ERROR = 'IDAVINR'             
066700         MOVE INF-AVISERING-FINNS TO MED-IDMFSINF                         
066800         MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLGODK-IN-ATTR                 
066900         PERFORM MFS-STAENG-KOL-FAELT                                     
067000       END-IF                                                             
067100*                                                                         
067200       CALL WMEDKONV USING MED-WMEDAREA                                   
067300       MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                            
067400       MOVE MED-TEMFSINF       TO MOD-TEMFSINF                            
067500*                                                                         
067600     END-IF                                                               
067700*                                                                         
067800     .                                                                    
067900     EJECT                                                                
068000                                                                          
068100 GB-MOVE-OUTDATA-TO-MOD-AREA   SECTION.                                   
068200     MOVE 'GB-MOVE-OUTDATA-TO-RESP-AREA' TO WS-CURRENT-SECTION            
068300                                                                          
068400     IF GODK-MID                                                          
068500                                                                          
068600       MOVE WS-IDLEVNR         TO MOD-IDLEVNR-UT                          
068700       MOVE WS-KDRT            TO MOD-KDRT-UT                             
068800       INSPECT MOD-KDRT-UT REPLACING LEADING ZERO BY SPACE                
068900       IF MOD-KDRT-UT          = SPACE                                    
069000           MOVE ' 0'           TO MOD-KDRT-UT                             
069100       END-IF                                                             
069200       MOVE WS-IDLBBET         TO MOD-IDLBBET-UT                          
069300       MOVE WS-IDFS            TO MOD-IDFS-UT                             
069400       MOVE WS-TIAVIDAT        TO MOD-TIAVIDAT-UT                         
069500       MOVE WS-ADINLOMR-PRT    TO MOD-ADINLOMR-PRT-UT                     
069600       INSPECT MOD-TIAVIDAT-UT REPLACING LEADING ZERO BY SPACE            
069700       MOVE WS-FLGODK          TO MOD-FLGODK-UT                           
069800       MOVE WS-IDDC            TO MOD-IDDC-UT                             
069900*                                                                         
070000       MOVE +1 TO RAD-IX                                                  
070100       PERFORM UNTIL RAD-IX > MAX-RAD                                     
070200                                                                          
070300         IF RESP-IDARTNR-LINE(RAD-IX) = ALL '+'                           
070400           IF MID-IDARTNR(RAD-IX) = ALL '+'                               
070500             MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN(RAD-IX)             
070600           ELSE                                                           
070700             MOVE MID-IDARTNR(RAD-IX) TO MOD-IDARTNR-IN(RAD-IX)           
070800             INSPECT MOD-IDARTNR-IN(RAD-IX)                               
070900                     REPLACING LEADING ZERO BY SPACE                      
071000         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-IN-ATTR(RAD-IX)        
071100           END-IF                                                         
071200                                                                          
071300         ELSE                                                             
071400           IF RESP-IDARTNR-LINE(RAD-IX) = SPACE                           
071500             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN(RAD-IX)               
071600           ELSE                                                           
071700             MOVE RESP-IDARTNR-LINE(RAD-IX)                               
071800               TO MOD-IDARTNR-IN(RAD-IX)                                  
071900             INSPECT MOD-IDARTNR-IN(RAD-IX)                               
072000                     REPLACING LEADING ZERO BY SPACE                      
072100           END-IF                                                         
072200         END-IF                                                           
072300         MOVE RESP-IDARTNR-LINE-ATTR(RAD-IX)                              
072400           TO MOD-IDARTNR-IN-ATTR(RAD-IX)                                 
072500                                                                          
072600         IF RESP-KVAVIS-LINE(RAD-IX) = ALL '+'                            
072700           IF MID-KVAVIS(RAD-IX) = ALL '+'                                
072800             MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS-IN(RAD-IX)             
072900           ELSE                                                           
073000             MOVE MID-KVAVIS(RAD-IX) TO MOD-KVAVIS-IN(RAD-IX)             
073100             INSPECT MOD-KVAVIS-IN(RAD-IX)                                
073200             REPLACING LEADING ZERO BY SPACE                              
073300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVIS-IN-ATTR(RAD-IX)         
073400           END-IF                                                         
073500         ELSE                                                             
073600           IF RESP-KVAVIS-LINE(RAD-IX) = SPACE                            
073700              MOVE MFS-RENSA-FAELT TO MOD-KVAVIS-IN(RAD-IX)               
073800           ELSE                                                           
073900             MOVE RESP-KVAVIS-LINE(RAD-IX)                                
074000               TO MOD-KVAVIS-IN(RAD-IX)                                   
074100             INSPECT MOD-KVAVIS-IN(RAD-IX)                                
074200             REPLACING LEADING ZERO BY SPACE                              
074300           END-IF                                                         
074400         END-IF                                                           
074500         MOVE RESP-KVAVIS-LINE-ATTR(RAD-IX)                               
074600           TO MOD-KVAVIS-IN-ATTR(RAD-IX)                                  
074700         ADD +1                   TO RAD-IX                               
074800       END-PERFORM                                                        
074900                                                                          
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300* --- MFS SEKTIONER ---                                                   
075400* --- MFS SEKTIONER ---                                                   
075500* --- MFS SEKTIONER ---                                                   
075600*                                                                         
075700 MFS-RENSA-NYCKLAR  SECTION.                                              
075800                                                                          
075900     MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-UT                              
076000                              MOD-KDRT-UT                                 
076100                              MOD-IDFS-UT                                 
076200                              MOD-TIAVIDAT-UT                             
076300                              MOD-ADINLOMR-PRT-UT                         
076400                              MOD-IDLBBET-UT                              
076500                              MOD-FLGODK-UT                               
076600                              MOD-IDDC-UT                                 
076700*     + ÖVRIGA SPAR-NYCKLAR                                               
076800                              MOD-FLKLIVIS-UT                             
076900                              MOD-IDLOPNRM-UT                             
077000     .                                                                    
077100     SKIP2                                                                
077200                                                                          
077300 MFS-RENSA-FAELT-IN SECTION.                                              
077400                                                                          
077500*    --- ALLA UTDATA-FÄLT                                                 
077600     MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-IN                          
077700                                  MOD-FLGODK-IN                           
077800                                                                          
077900     PERFORM MFS-RENSA-KOLUMN-FAELT-IN                                    
078000                                                                          
078100     .                                                                    
078200     EJECT                                                                
078300 MFS-RENSA-KOLUMN-FAELT-IN SECTION.                                       
078400                                                                          
078500     MOVE +1                   TO RAD-IX                                  
078600     PERFORM UNTIL RAD-IX      >  MAX-RAD                                 
078700         MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-IN (RAD-IX)                 
078800                                  MOD-KVAVIS-IN  (RAD-IX)                 
078900         ADD +1                TO RAD-IX                                  
079000     END-PERFORM                                                          
079100     .                                                                    
079200     SKIP2                                                                
079300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
079400                                                                          
079500     MOVE +1                     TO RAD-IX                                
079600     PERFORM UNTIL RAD-IX        >  MAX-RAD                               
079700         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-IN (RAD-IX)               
079800                                    MOD-KVAVIS-IN  (RAD-IX)               
079900         ADD +1                  TO RAD-IX                                
080000     END-PERFORM                                                          
080100     .                                                                    
080200     SKIP2                                                                
080300 MFS-FORM-ATTR SECTION.                                                   
080400                                                                          
080500*    --- ALLA INDATA-FÄLT                                                 
080600     MOVE MFS-FORMATETS-ATTR   TO MOD-IDLBBET-IN                          
080700                                  MOD-FLGODK-IN                           
080800                                                                          
080900     MOVE +1                     TO RAD-IX                                
081000     PERFORM UNTIL RAD-IX        >  MAX-RAD                               
081100         MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-IN-ATTR (RAD-IX)          
081200                                    MOD-KVAVIS-IN-ATTR  (RAD-IX)          
081300         ADD +1                  TO RAD-IX                                
081400     END-PERFORM                                                          
081500     .                                                                    
081600     SKIP2                                                                
081700 MFS-LAES-IN-IGEN SECTION.                                                
081800                                                                          
081900     MOVE +1                     TO RAD-IX                                
082000     PERFORM UNTIL RAD-IX        >  MAX-RAD                               
082100         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
082200                                    MOD-IDARTNR-IN-ATTR (RAD-IX)          
082300                                    MOD-KVAVIS-IN-ATTR  (RAD-IX)          
082400         ADD +1                  TO RAD-IX                                
082500     END-PERFORM                                                          
082600     .                                                                    
082700     SKIP2                                                                
082800 MFS-STAENG-KOL-FAELT  SECTION.                                           
082900                                                                          
083000     MOVE +1                          TO RAD-IX                           
083100                                                                          
083200     PERFORM UNTIL RAD-IX             >  MAX-RAD                          
083300       IF RESP-IDMSG-ERROR    > ZERO                                      
083400      AND RESP-IDELMT-ERROR = 'IDARTNR'                                   
083500         MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-IN-ATTR(RAD-IX)             
083600       ELSE                                                               
083700         MOVE MFS-STAENG-FAELT-HI TO MOD-IDARTNR-IN-ATTR(RAD-IX)          
083800       END-IF                                                             
083900       MOVE MFS-STAENG-FAELT TO MOD-KVAVIS-IN-ATTR(RAD-IX)                
084000       ADD +1                  TO RAD-IX                                  
084100     END-PERFORM                                                          
084200     .                                                                    
084300     EJECT                                                                
084400                                                                          
084500* --- IMS SEKTIONER ---                                                   
084600* --- IMS SEKTIONER ---                                                   
084700* --- IMS SEKTIONER ---                                                   
084800                                                                          
084900     SKIP3                                                                
085000 IMS-GET-MSG SECTION.                                                     
085100                                                                          
085200     MOVE '  QC' TO GODK-STATUSKODER                                      
085300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
085400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085500     PERFORM IMS-STATUSKONTROLL                                           
085600     .                                                                    
085700     SKIP3                                                                
085800 IMS-INSERT-MSG SECTION.                                                  
085900                                                                          
086000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
086100       MOVE '0' TO MFS-KDHUVOMR                                           
086200     END-IF                                                               
086300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
086400     MOVE SPACE TO GODK-STATUSKODER                                       
086500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
086600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900 IMS-STATUSKONTROLL SECTION.                                              
087000     SKIP2                                                                
087100     SET STATUS-IX TO 1                                                   
087200     SEARCH GODK-STATUS                                                   
087300       AT END                                                             
087400         MOVE 'FEL VID IMS CALL' TO FELTEXT                               
087500         CALL FELLOG                                                      
087600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
087700         CONTINUE                                                         
087800     END-SEARCH                                                           
087900     .                                                                    
