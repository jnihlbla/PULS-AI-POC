000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9041400.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   APRIL 90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*      - HISTORIK                                                         
001100*                                                                         
001200*      - VISAR STRUKTUREN ENLIGT VALT TIDSINTERVALL.                      
001300*                                                                         
001400****   - RAD SOM HAR STRUKTURTYP KAN MARKERAS FÖR ATT VISA RADENS         
001500****     STRUKTUR . RETURFUNKTION MED PF-TANGENT TILL URSPRUNGLIG         
001600****     STRUKTUR .                                                       
001700*                                                                         
001800*      - LÄSER RASA .                                                     
001900*                                                                         
002000****   - PRINTNING AV LISTA SKER MED HJÄLP AV BAKGRUNDS-                  
002100****     PROGRAMMET W10292.                                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W9T414                                              
002500*        MID:         W90414I1                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W90414O1                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003401*    -COPY WY2000W1                                                       
003402     SKIP3                                                                
003403*    -COPY WY2000W3                                                       
003410     SKIP3                                                                
003500 77  IDPGM                   PIC X(8)    VALUE 'W9041400'.                
003600 77  JA                      PIC X       VALUE 'J'.                       
003700 77  NEJ                     PIC X       VALUE 'N'.                       
003800 77  S-IX                    PIC S9(9)   VALUE +0   COMP SYNC.            
003900 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004000 77  MAX-IDRAD               PIC S9(9)   VALUE +13.                       
004100 77  MAX-SELECT              PIC S9(9)   VALUE +12.                       
004300 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
004400 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
004500 77  BEART-WS                PIC X(25)   VALUE SPACE.                     
004600 77  KDBENHOM-WS             PIC 9       VALUE ZERO.                      
004700 77  SPAR-ARTIKELNR2         PIC 9(9)    VALUE ZERO.                      
004800 77  STOPP-DATUM             PIC 9(5)    VALUE ZERO.                      
004900 77  SATS-HIST-CARP          PIC X(3)    VALUE '231'.                     
005000 77  SATS-HIST-RA            PIC X(3)    VALUE '232'.                     
005100 77  SATS-HIST-RB            PIC X(3)    VALUE '233'.                     
005400 77  SATS-HIST-BERPV         PIC X(3)    VALUE '236'.                     
005600                                                                          
005700 01  SPAR-FAELT              PIC X(30)   VALUE SPACE.                     
005800*                                                                         
005900 01  FILLER REDEFINES SPAR-FAELT.                                         
006000   03 SPAR-ARTIKELNR         PIC X(9).                                    
006100   03 FILLER                 PIC X(21).                                   
006200                                                                          
006300 01  VECKA-TOM               PIC X(4)    VALUE SPACE.                     
006400 01  VECKA-FOM               PIC X(4)    VALUE SPACE.                     
006500                                                                          
006600 01  SPAR-START-DATUM        PIC 9(4).                                    
006700*                                                                         
006800 01  FILLER REDEFINES SPAR-START-DATUM.                                   
006900   03 SPAR-START-AA          PIC 9(2).                                    
007000   03 SPAR-START-VV          PIC 9(2).                                    
007100                                                                          
007200 01  SPAR-STOPP-DATUM        PIC 9(4).                                    
007300*                                                                         
007400 01  FILLER REDEFINES SPAR-STOPP-DATUM.                                   
007500   03 SPAR-STOPP-AA          PIC 9(2).                                    
007600   03 SPAR-STOPP-VV          PIC 9(2).                                    
007700                                                                          
007800 01  VECKA-START             PIC 9(4).                                    
007900*                                                                         
008000 01  FILLER REDEFINES VECKA-START.                                        
008100   03 VECKA-START-AA         PIC 9(2).                                    
008200   03 VECKA-START-VV         PIC 9(2).                                    
008300                                                                          
008400 01  DAGENS-DATUM            PIC 9(6).                                    
008500*                                                                         
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700   03  DAGENS-DATUM-AR       PIC 9(2).                                    
008800   03  DAGENS-DATUM-MANAD    PIC 9(2).                                    
008900   03  DAGENS-DATUM-DAG      PIC 9(2).                                    
009000                                                                          
009100 77  INDATA-SW               PIC X       VALUE 'J'.                       
009200   88  INDATA-OK                         VALUE 'J'.                       
009300   88  INDATA-FEL                        VALUE 'N'.                       
009400                                                                          
009500 77  STOPP-DATUM-SW          PIC X       VALUE 'J'.                       
009600   88  STOPP-DATUM-OK                    VALUE 'J'.                       
009700   88  STOPP-DATUM-FEL                   VALUE 'N'.                       
009800                                                                          
009900 77  START-DATUM-SW          PIC X       VALUE 'J'.                       
010000   88  START-DATUM-OK                    VALUE 'J'.                       
010100   88  START-DATUM-FEL                   VALUE 'N'.                       
010200                                                                          
010300 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
010400   88  NYCKLAR-OK                        VALUE 'J'.                       
010500   88  NYCKLAR-FEL                       VALUE 'N'.                       
010600                                                                          
010700 77  INPUT-SW                PIC X       VALUE 'J'.                       
010800   88  INPUT-FINNS                       VALUE 'J'.                       
010900   88  INPUT-FINNS-EJ                    VALUE 'N'.                       
011000                                                                          
011100 77  PF-TANGENT-SW           PIC X       VALUE 'J'.                       
011200   88  PF-TANGENT-TRYCKT                 VALUE 'J'.                       
011300   88  PF-TANGENT-EJ-TRYCKT              VALUE 'N'.                       
011400                                                                          
011500 77  ALLT-SW                 PIC X       VALUE 'J'.                       
011600   88  ALLT-OK                           VALUE 'J'.                       
011700                                                                          
011800 77  STATUS-SW               PIC X       VALUE 'J'.                       
011900   88  STATUS-OK                         VALUE 'J'.                       
012000                                                                          
012100 77  BEHANDLING-SW           PIC X       VALUE 'J'.                       
012200   88  BEHANDLING-OK                     VALUE 'J'.                       
012300                                                                          
012400 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
012500   88  EGEN-MID                          VALUE '9414'.                    
012600   88  GODK-MID                          VALUE '9413' '9414'              
012700                                               '9415'.                    
012900   88  GODK-MID-MED-IDSKYLT              VALUE '9413' '9414'              
013000                                               '9415'.                    
013100     EJECT                                                                
013200                                                                          
013300                                                                          
013400 01  MEDDELANDE.                                                          
013500   03  MED-1.                                                             
013600       05  FILLER            PIC X(30)   VALUE                            
013700             'MER INFO FINNS TRYCK PF8      '.                            
013800       05  FILLER            PIC X(30)   VALUE                            
013900             'PRESS PF8 FOR MORE INFORMATION'.                            
014000   03  FILLER REDEFINES MED-1.                                            
014100       05  MED1 OCCURS 2     PIC X(30).                                   
014200*                                                                         
014300   03  MED-2.                                                             
014400       05  FILLER            PIC X(30)   VALUE                            
014500             'DETTA ÄR SISTA SIDAN          '.                            
014600       05  FILLER            PIC X(30)   VALUE                            
014700             'LAST PAGE IS SHOWN            '.                            
014800   03  FILLER REDEFINES MED-2.                                            
014900       05  MED2 OCCURS 2     PIC X(30).                                   
015000*                                                                         
015100   03  MED-3.                                                             
015200       05  FILLER            PIC X(30)   VALUE                            
015300             'DETTA ÄR FÖRSTA SIDAN         '.                            
015400       05  FILLER            PIC X(30)   VALUE                            
015500             'FIRST PAGE IS SHOWN           '.                            
015600   03  FILLER REDEFINES MED-3.                                            
015700       05  MED3 OCCURS 2     PIC X(30).                                   
015800*                                                                         
015900   03  FELMED-4.                                                          
016000       05  FILLER            PIC X(61)   VALUE                            
016100       'FEL I W006PRT  DEFINITION, KONTAKTA SYSTEMAVD'.                   
016200       05  FILLER            PIC X(61)   VALUE                            
016300       'MAJOR ERROR IN W006PRT, CONTACT YOUR SYSTEM SUPPORT'.             
016400   03  FILLER REDEFINES FELMED-4.                                         
016500       05  FELMED4 OCCURS 2  PIC X(61).                                   
016600*                                                                         
016700   03  MED-4-AREA.                                                        
016800       05  FILLER            PIC X(27)   VALUE                            
016900             'LISTA KÖAD FÖR UTSKRIFT PÅ '.                               
017000       05  FILLER            PIC X(27)   VALUE                            
017100             'LIST IS QUEUED TO PRINTER  '.                               
017200   03  FILLER REDEFINES MED-4-AREA.                                       
017300       05  MED4TXT OCCURS 2  PIC X(27).                                   
017400   03  MED-4.                                                             
017500       05  MED4-TXT          PIC X(27).                                   
017600       05  MED4-IDLTERM      PIC X(8)    VALUE  SPACE.                    
017700       05  FILLER            PIC X       VALUE  ','.                      
017800       05  MED4-BEPRT        PIC X(25)   VALUE  SPACE.                    
017900*                                                                         
018000   03  FEL-1.                                                             
018100       05  FILLER            PIC X(30)   VALUE                            
018200             'KORRIGERA UPPLYSTA FÄLT       '.                            
018300       05  FILLER            PIC X(30)   VALUE                            
018400             'CORRECT HIGHLIGHTED FIELDS    '.                            
018500   03  FILLER REDEFINES FEL-1.                                            
018600       05  FEL1 OCCURS 2     PIC X(30).                                   
018700*                                                                         
018800   03  FEL-2.                                                             
018900       05  FILLER            PIC X(30)   VALUE                            
019000             'ARTIKEL SAKNAS                '.                            
019100       05  FILLER            PIC X(30)   VALUE                            
019200             'THIS PART IS MISSING          '.                            
019300   03  FILLER REDEFINES FEL-2.                                            
019400       05  FEL2 OCCURS 2     PIC X(30).                                   
019500*                                                                         
019600   03  FEL-3.                                                             
019700       05  FILLER            PIC X(30)   VALUE                            
019800             'STRUKTUR SAKNAS               '.                            
019900       05  FILLER            PIC X(30)   VALUE                            
020000             'STRUCTURE IS MISSING          '.                            
020100   03  FILLER REDEFINES FEL-3.                                            
020200       05  FEL3 OCCURS 2     PIC X(30).                                   
020300*                                                                         
020400   03  FEL-4.                                                             
020500       05  FILLER            PIC X(30)   VALUE                            
020600             'MATA IN NYA NYCKLAR           '.                            
020700       05  FILLER            PIC X(30)   VALUE                            
020800             'ENTER NEW KEYS                '.                            
020900   03  FILLER REDEFINES FEL-4.                                            
021000       05  FEL4 OCCURS 2     PIC X(30).                                   
021100*                                                                         
021200   03  FEL-5.                                                             
021300       05  FILLER            PIC X(30)   VALUE                            
021400             'TRYCK PF16 FÖR NEDBRYTNING    '.                            
021500       05  FILLER            PIC X(30)   VALUE                            
021600             'PRESS PF16 FOR SPLIT-UP       '.                            
021700   03  FILLER REDEFINES FEL-5.                                            
021800       05  FEL5  OCCURS 2    PIC X(30).                                   
021900*                                                                         
022000   03  FEL-6.                                                             
022100       05  FILLER            PIC X(30)   VALUE                            
022200             'NYCKLAR FEL                   '.                            
022300       05  FILLER            PIC X(30)   VALUE                            
022400             'WRONG KEYS                    '.                            
022500   03  FILLER REDEFINES FEL-6.                                            
022600       05  FEL6  OCCURS 2    PIC X(30).                                   
022700                                                                          
022800 01  GENERELLA-SUBPROGRAM.                                                
022900   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
023000   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
023100   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
023200   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
023300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
023400   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
023410   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
023500     EJECT                                                                
023600*01  -COPY WWLAND03                                                       
023700     EJECT                                                                
023800*    -COPY WDECAREA                                                       
023900     EJECT                                                                
024000*    -COPY WMEDAREA                                                       
024100     EJECT                                                                
024200*    -COPY W006PRT                                                        
024300     EJECT                                                                
024400*    -COPY WDATAREA                                                       
024410     EJECT                                                                
024411*                    ****   PARAMETRAR TILL W005INIT                      
024420*    -COPY WMSGINIT                                                       
024500     EJECT                                                                
024600******************************************************************        
024700*                                                                         
024800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024900*                                                                         
025000 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
025100     SKIP3                                                                
025200*01  MID -COPY W90414I1                                                   
025300     EJECT                                                                
025400*01  -COPY WMSGAREA                                                       
025500     EJECT                                                                
025600*  03  MOD -COPY W90414O1 -RED MSG-AREA.                                  
025700     EJECT                                                                
025800*01  -COPY WMFSAREA                                                       
025900     EJECT                                                                
026000 01  FILLER                  PIC X(16)   VALUE 'ALT-AREA'.                
026100 01  W-PROG-TO-PROG-SW.                                                   
026200     03 M-SW-LL              PIC S9(4)   VALUE +556                       
026300                                         COMP SYNC.                       
026400     03 M-SW-Z1-Z2           PIC X(2)    VALUE LOW-VALUE.                 
026500     03 M-SW-KDTRANS         PIC X(8)    VALUE 'W1T292X'.                 
026600     03 M-SW-IDTRANS         PIC X(4)    VALUE '1214'.                    
026700     03 M-SW-KDTRANS         PIC X(1)    VALUE '1'.                       
026800     03 MID-W1I29201.                                                     
026900*        05  MID -COPY W1I21401   -PRE R1292                              
027000     EJECT                                                                
027100                                                                          
027200******************************************************************        
027300*                                                                         
027400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027500*                                                                         
027600 01  IMS-WS.                                                              
027700   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
027800     SKIP3                                                                
027900 01  NYCKLAR-TILL-DLI.                                                    
028000   03  W-IDARTNR-X.                                                       
028100     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
028200   03  W-IDSKYLT-X.                                                       
028300     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
028400   03  W-IDKDSTRRAD-X.                                                    
028500     05  W-IDKDSTRRAD        PIC  X      VALUE SPACE.                     
028600   03  W-IDRADNR-X.                                                       
028700     05  W-IDRADNR           PIC  S9(5)  VALUE ZERO  COMP-3.              
028800   03  W-BEART-X.                                                         
028900     05  W-BEART             PIC  X(25)  VALUE SPACE.                     
029000     SKIP3                                                                
029100*                        **** STATUS-KOD FRÅN IMS                         
029200   03  STATUS-WS             PIC XX.                                      
029300     88  SEGMENT-FINNS                   VALUE '  '.                      
029400     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
029500     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
029600     SKIP3                                                                
029700   03  GODK-STATUSKODER.                                                  
029800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029900     SKIP3                                                                
030000 01    SSA1                  PIC X(64).                                   
030100 01    SSA2                  PIC X(64).                                   
030200 01    SSA3                  PIC X(64).                                   
030300     EJECT                                                                
030400*                            IMS FUNKTIONSKODER                           
030500*01    -COPY W0003                                                        
030600     EJECT                                                                
030700*                            DLI INPUT-OUTPUT AREA                        
030800 01  DLI-IO-AREA.                                                         
030900   03  IO-AREA               PIC X(250)  VALUE SPACE.                     
031000     SKIP3                                                                
031100*  03  WLSATB01  -COPY WDJ101  -PRE SATB-     -RED IO-AREA.               
031200     EJECT                                                                
031300*  03  WLSATB11  -COPY WDJ111  -PRE SATB-     -RED IO-AREA.               
031400     EJECT                                                                
031500*  03  WLBENA01  -COPY WDD301  -PRE BENA01-   -RED IO-AREA.               
031600     EJECT                                                                
031700*  03  WLBENA11  -COPY WDD311  -PRE BENA11-   -RED IO-AREA.               
031800     EJECT                                                                
031900                                                                          
032000 LINKAGE SECTION.                                                         
032100*01  -COPY W0009     -PRE MSG-                                            
032200     EJECT                                                                
032300*01  -COPY W0009     -PRE ALT-                                            
032400     EJECT                                                                
032500*01  -COPY W0008     -PRE USEA-                                           
032600     05  FILLER              PIC X.                                       
032610     EJECT                                                                
032620*01  -COPY W0008     -PRE SATB-                                           
032630     05  FILLER              PIC X.                                       
032700     EJECT                                                                
032800*01  -COPY W0008     -PRE BENA-                                           
032900     05  FILLER              PIC X.                                       
033000     EJECT                                                                
033100                                                                          
033200 PROCEDURE DIVISION USING MSG-PCB ALT-PCB USEA-PCB                        
033210                                          SATB-PCB BENA-PCB.              
033300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
033310                                           SATB-PCB BENA-PCB.             
033400                                                                          
033500     PERFORM IMS-GET-MSG                                                  
033600     IF SEGMENT-FINNS                                                     
033700       MOVE NEJ TO PF-TANGENT-SW                                          
033800       PERFORM A-INIT                                                     
033900***    PERFORM C-KOLLA-INPUT                                              
034000       PERFORM B-KOLLA-NYCKLAR                                            
034100       IF NYCKLAR-OK                                                      
034200         PERFORM IMS-GET-SATB-ART                                         
034300         IF SEGMENT-FINNS                                                 
034400           IF MFS-FIRST                                                   
034500             CONTINUE                                                     
034600           ELSE                                                           
034700             IF MFS-NEXT                                                  
034800               PERFORM D-NAESTA-SIDA                                      
034900             ELSE                                                         
034910               PERFORM E-SAMMA-SIDA                                       
035000***            IF MFS-PRINT                                               
035100***              PERFORM G-KOLLA-INPUT-PRINTER                            
035200***              IF INDATA-OK                                             
035300***                PERFORM H-SKICKA-TRANS-TILL-1292                       
035400***              END-IF                                                   
035500***            ELSE                                                       
035600***              PERFORM E-SAMMA-SIDA                                     
035700***            END-IF                                                     
035800             END-IF                                                       
035900           END-IF                                                         
036000           IF NYCKLAR-OK AND ALLT-OK                                      
036100             PERFORM F-LAES-VISA-INFO                                     
036200             PERFORM MFS-RENSA-FAELT-INMAT                                
036300***          MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                       
036400           END-IF                                                         
036500         ELSE                                                             
036600***        MOVE SPACE TO MOD-IDSATSNR-DOLT                                
036700           MOVE FEL2(S-IX) TO MOD-TEMFSFEL                                
036800           PERFORM MFS-RENSA-FAELT-IN                                     
036900           PERFORM MFS-RENSA-FAELT-UT                                     
037000           PERFORM MFS-FORM-ATTR                                          
037100           MOVE NEJ TO NYCKLAR-SW                                         
037200         END-IF                                                           
037300       ELSE                                                               
037400         PERFORM MFS-FORM-ATTR                                            
037500       END-IF                                                             
037610       COMPUTE MSG-KVLL = LENGTH OF MOD-W90414O1 + 4                      
037700       PERFORM IMS-INSERT-MSG                                             
037800     END-IF                                                               
037900                                                                          
038000     MOVE ZERO TO RETURN-CODE                                             
038100     GOBACK                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 A-INIT SECTION.                                                          
038500                                                                          
038600     IF MSG-DUBBLA-TRANSKODER                                             
038700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90414I1                 
038800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039000       MOVE JA TO PF-TANGENT-SW                                           
039100     ELSE                                                                 
039200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90414I1                  
039300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039500     END-IF                                                               
039600                                                                          
039700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
039800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
039900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
040000                                                                          
040100     MOVE LOW-VALUE TO MSG-AREA                                           
040200     MOVE 'W90414O1' TO MFS-IDMOD                                         
040300     MOVE '9414' TO MOD-IDTRANS                                           
040400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040500                                                                          
040600     IF NOT EGEN-MID                                                      
040700       MOVE SPACE TO MFS-KDTRTYP                                          
040800       MOVE '7'   TO MFS-IDPFK                                            
040900     END-IF                                                               
041000                                                                          
041100     IF ENGLISH-TEXT                                                      
041200       MOVE +2 TO S-IX                                                    
041300       MOVE 'GB ' TO MED-IDSKYLT                                          
041400     ELSE                                                                 
041500       MOVE 'S  ' TO MED-IDSKYLT                                          
041600       MOVE +1 TO S-IX                                                    
041700     END-IF                                                               
041800     ACCEPT DAGENS-DATUM FROM DATE                                        
041900     .                                                                    
042000     EJECT                                                                
042100 B-KOLLA-NYCKLAR SECTION.                                                 
042200                                                                          
042300     MOVE JA TO NYCKLAR-SW                                                
042400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
042500***                          MOD-IDSKYLT-IN                               
042600                             MOD-VECKA-FOM-IN                             
042700                             MOD-VECKA-TOM-IN                             
042800                                                                          
042820     MOVE ALL '+' TO MSGI-WMSGINIT                                        
042830     MOVE '001'             TO MSGI-KDCALL                                
042840     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042841                               MSGI-IDLTERM-USER                          
042842     MOVE '9414'            TO MSGI-IDTRANS                               
042850     IF MFS-IDTRANS = '9414'                                              
042860     OR (MID-IDARTNR-IN NUMERIC                                           
042861     AND MID-IDARTNR-IN > ZERO)                                           
042870         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
042880     END-IF                                                               
042890     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042891     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
042892     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
042893                                                                          
042900     IF MID-IDARTNR-IN = ALL '+'                                          
043200***    IF EGEN-MID                                                        
043300***      MOVE MID-IDSATSNR-DOLT TO MOD-IDSATSNR-DOLT                      
043400***    ELSE                                                               
043500***      MOVE MSGI-IDARTNR      TO MOD-IDSATSNR-DOLT                      
043600***    END-IF                                                             
043610       CONTINUE                                                           
043700     ELSE                                                                 
043900       MOVE '7'             TO MFS-IDPFK                                  
044000       MOVE SPACE           TO MFS-KDTRTYP                                
044100***    MOVE MSGI-IDARTNR    TO MOD-IDSATSNR-DOLT                          
044200     END-IF                                                               
044300                                                                          
044400     IF (IDARTNR-WS NUMERIC) AND (IDARTNR-WS > ZERO)                      
044500       AND (IDARTNR-WS < 99999999)                                        
044600       MOVE IDARTNR-WS TO W-IDARTNR                                       
044700     ELSE                                                                 
044800       MOVE NEJ            TO NYCKLAR-SW                                  
044900       MOVE FEL6(S-IX) TO MOD-TEMFSFEL                                    
045000     END-IF                                                               
045100                                                                          
045200***  IF GODK-MID-MED-IDSKYLT                                              
045300***    IF MID-IDSKYLT-IN = ALL '+'                                        
045400***      IF MID-IDSKYLT-UT = SPACE                                        
045500***        IF ENGLISH-TEXT                                                
045600***          MOVE 'GB' TO IDSKYLT-WS                                      
045700***        ELSE                                                           
045800***          MOVE 'S ' TO IDSKYLT-WS                                      
045900***        END-IF                                                         
046000***      ELSE                                                             
046100***        MOVE MID-IDSKYLT-UT TO IDSKYLT-WS                              
046200***      END-IF                                                           
046300***    ELSE                                                               
046400***      MOVE MID-IDSKYLT-IN TO IDSKYLT-WS                                
046500***      MOVE '7'          TO MFS-IDPFK                                   
046600***      MOVE SPACE        TO MFS-KDTRTYP                                 
046700***    END-IF                                                             
046800***  ELSE                                                                 
046900       IF ENGLISH-TEXT                                                    
047000         MOVE 'GB' TO IDSKYLT-WS                                          
047100       ELSE                                                               
047200         MOVE 'S ' TO IDSKYLT-WS                                          
047300       END-IF                                                             
047400***  END-IF                                                               
047500                                                                          
047600     SET WWLAND03-IX TO +1                                                
047700     SEARCH WWLAND03-IDSKYLT-RAD                                          
047800       AT END                                                             
047900         MOVE NEJ            TO NYCKLAR-SW                                
048000         MOVE FEL6(S-IX) TO MOD-TEMFSFEL                                  
048100       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = IDSKYLT-WS                    
048200         CONTINUE                                                         
048300     END-SEARCH                                                           
048400     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
048500                                                                          
048600     IF EGEN-MID                                                          
048700       IF MID-VECKA-FOM-IN = ALL '+'                                      
048800         MOVE MID-VECKA-FOM-UT TO VECKA-FOM                               
048900         INSPECT VECKA-FOM REPLACING LEADING SPACE BY ZERO                
049000       ELSE                                                               
049100         MOVE MID-VECKA-FOM-IN TO VECKA-FOM                               
049200         INSPECT VECKA-FOM REPLACING LEADING SPACE BY ZERO                
049300         MOVE '7'            TO MFS-IDPFK                                 
049400         MOVE SPACE          TO MFS-KDTRTYP                               
049500       END-IF                                                             
049600                                                                          
049700       IF MID-VECKA-TOM-IN = ALL '+'                                      
049800         MOVE MID-VECKA-TOM-UT TO VECKA-TOM                               
049900         INSPECT VECKA-TOM REPLACING LEADING SPACE BY ZERO                
050000       ELSE                                                               
050100         MOVE MID-VECKA-TOM-IN TO VECKA-TOM                               
050200         MOVE '7'            TO MFS-IDPFK                                 
050300         MOVE SPACE          TO MFS-KDTRTYP                               
050400       END-IF                                                             
050500     ELSE                                                                 
050600       MOVE ZERO TO VECKA-FOM                                             
050700                    VECKA-TOM                                             
050800     END-IF                                                               
050900                                                                          
051000     IF VECKA-FOM = ZERO                                                  
051100       MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                 
051200       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
051300       CALL WDATKONV USING DAT-KDDATFORM                                  
051400                           DAT-I-TIDATUM                                  
051500                           DAT-O-TIDATUM                                  
051600                           DAT-KDSVAR                                     
051700       IF DAT-KDSVAR-OK                                                   
051800         MOVE DAT-TIAA-VECKA TO VECKA-START-AA                            
051900         MOVE DAT-TIVV       TO VECKA-START-VV                            
052000       END-IF                                                             
052100       MOVE VECKA-START TO VECKA-FOM                                      
052200     END-IF                                                               
052300                                                                          
052400     IF VECKA-TOM = ZERO                                                  
052500       MOVE '9999'  TO VECKA-TOM                                          
052600     END-IF                                                               
052700                                                                          
052800     IF (VECKA-FOM  NUMERIC) AND (VECKA-TOM  NUMERIC)                     
052900       MOVE ZERO TO W-IDKDSTRRAD                                          
053000       MOVE ZERO TO W-IDRADNR                                             
053100     ELSE                                                                 
053200       MOVE NEJ            TO NYCKLAR-SW                                  
053300       MOVE FEL6(S-IX) TO MOD-TEMFSFEL                                    
053400     END-IF                                                               
053500                                                                          
053600     IF GODK-MID OR NYCKLAR-OK                                            
053700       MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                  
053800       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
053900****   MOVE IDSKYLT-WS TO MOD-IDSKYLT-UT                                  
054000       MOVE VECKA-FOM  TO MOD-VECKA-FOM-UT                                
054100       INSPECT MOD-VECKA-FOM-UT REPLACING LEADING ZERO BY SPACE           
054200       MOVE VECKA-TOM  TO MOD-VECKA-TOM-UT                                
054300       INSPECT MOD-VECKA-TOM-UT REPLACING LEADING ZERO BY SPACE           
054400     ELSE                                                                 
054500       MOVE FEL4(S-IX)      TO MOD-TEMFSFEL                               
054600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
054700***                            MOD-IDSKYLT-UT                             
054800                               MOD-VECKA-FOM-UT                           
054900                               MOD-VECKA-TOM-UT                           
055000       MOVE NEJ TO NYCKLAR-SW                                             
055100     END-IF                                                               
055200                                                                          
055300     IF NYCKLAR-FEL                                                       
055400       PERFORM MFS-RENSA-FAELT-IN                                         
055500       PERFORM MFS-RENSA-FAELT-UT                                         
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 C-KOLLA-INPUT SECTION.                                                   
056100                                                                          
056200***  MOVE +1 TO INDX                                                      
056300***  MOVE NEJ TO INPUT-SW                                                 
056400***  PERFORM UNTIL INDX > MAX-IDRAD                                       
056500***    IF (MID-SELECT(INDX) = ALL '+') OR                                 
056600***       (MID-SELECT(INDX) = SPACE)                                      
056700***       MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                        
056800***    ELSE                                                               
056900***      IF MID-SELECT(INDX) = 'S'                                        
057000***        MOVE JA TO INPUT-SW                                            
057100***        IF MID-STRTYP(INDX) = 'S' OR 'R' OR 'K'                        
057200***          MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)           
057300***          MOVE MID-ART-LEV(INDX) TO SPAR-FAELT                         
057400***          INSPECT SPAR-ARTIKELNR REPLACING                             
057500***          LEADING SPACE BY ZERO                                        
057600***          MOVE SPAR-ARTIKELNR TO SPAR-ARTIKELNR2                       
057700***          IF SPAR-ARTIKELNR2 NUMERIC                                   
057800***            MOVE SPAR-ARTIKELNR2 TO MID-IDARTNR-UT                     
057900***            MOVE '7'             TO MFS-IDPFK                          
058000***            MOVE SPACE           TO MFS-KDTRTYP                        
058100***            ADD +20 TO INDX                                            
058200***          END-IF                                                       
058300***        ELSE                                                           
058400***          IF MFS-FIRST OR MFS-NEXT                                     
058500***            PERFORM MFS-RENSA-FAELT-INMAT                              
058600***            MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                       
058700***          ELSE                                                         
058800***            MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)           
058900***            MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                 
059000***            MOVE FEL3(S-IX)        TO MOD-TEMFSFEL                     
059100***          END-IF                                                       
059200***        END-IF                                                         
059300***      ELSE                                                             
059400***        IF MFS-FIRST OR MFS-NEXT                                       
059500***          PERFORM MFS-RENSA-FAELT-INMAT                                
059600***          MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                         
059700***        ELSE                                                           
059800***          MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
059900***          MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                   
060000***          MOVE FEL1(S-IX)        TO MOD-TEMFSFEL                       
060100***        END-IF                                                         
060200***      END-IF                                                           
060300***    END-IF                                                             
060400***    ADD +1 TO INDX                                                     
060500**   END-PERFORM                                                          
060600                                                                          
060700***  IF INPUT-FINNS-EJ                                                    
060800***    IF PF-TANGENT-TRYCKT                                               
060900***      IF NOT MFS-FIRST                                                 
061000***        IF NOT MFS-NEXT                                                
061100***          IF NOT MFS-PRINT                                             
061200***            IF EGEN-MID                                                
061300***              IF MID-IDSATSNR-DOLT NOT = ZERO                          
061400***                MOVE MID-IDSATSNR-DOLT TO MID-IDARTNR-UT               
061500***                MOVE MID-IDSATSNR-DOLT TO MOD-IDSATSNR-DOLT            
061600***                MOVE '7'               TO MFS-IDPFK                    
061700***                MOVE SPACE             TO MFS-KDTRTYP                  
061800***              END-IF                                                   
061900***            END-IF                                                     
062000***          END-IF                                                       
062100***        END-IF                                                         
062200***      END-IF                                                           
062300***    END-IF                                                             
062400***  END-IF                                                               
062500     .                                                                    
062600     EJECT                                                                
062700                                                                          
062800 D-NAESTA-SIDA SECTION.                                                   
062900                                                                          
063000     PERFORM MFS-ROER-EJ-FAELT-UT                                         
063100     MOVE MID-IDRADNR-DOLT2 TO W-IDRADNR                                  
063200     .                                                                    
063300     EJECT                                                                
063400 E-SAMMA-SIDA SECTION.                                                    
063500                                                                          
063600***  IF INPUT-FINNS                                                       
063700***    IF PF-TANGENT-TRYCKT                                               
063800***      IF EGEN-MID                                                      
063900***        MOVE NEJ TO ALLT-SW                                            
064000***        MOVE FEL3(S-IX) TO MOD-TEMFSFEL                                
064100***        PERFORM MFS-ROER-EJ-FAELT-IN                                   
064200***        PERFORM MFS-ROER-EJ-FAELT-UT                                   
064300***      END-IF                                                           
064400***    ELSE                                                               
064500***      IF MFS-ENTER                                                     
064600***        MOVE NEJ TO ALLT-SW                                            
064700***        MOVE FEL5(S-IX) TO MOD-TEMFSFEL                                
064800***        PERFORM MFS-ROER-EJ-FAELT-IN                                   
064900***        PERFORM MFS-ROER-EJ-FAELT-UT                                   
065000***      END-IF                                                           
065100***    END-IF                                                             
065200***  ELSE                                                                 
065300       MOVE MID-IDRADNR-DOLT TO W-IDRADNR                                 
065400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
065500       MOVE JA TO ALLT-SW                                                 
065600***  END-IF                                                               
065700     .                                                                    
065800     EJECT                                                                
065900 F-LAES-VISA-INFO SECTION.                                                
066000                                                                          
066100     MOVE +1 TO INDX                                                      
066200     PERFORM IMS-GET-SATB-RAD-FIRST                                       
066300     IF SEGMENT-FINNS                                                     
066400       MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT                          
066500     ELSE                                                                 
066600       MOVE '9' TO W-IDKDSTRRAD                                           
066700       PERFORM IMS-GET-SATB-RAD-FIRST                                     
066800       IF SEGMENT-FINNS                                                   
066900         MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT                        
067000       ELSE                                                               
067100         MOVE ZERO TO MOD-IDRADNR-DOLT                                    
067200       END-IF                                                             
067300     END-IF                                                               
067400                                                                          
067500     PERFORM UNTIL INDX > MAX-IDRAD                                       
067600       IF SEGMENT-FINNS                                                   
067700         PERFORM FA-KOLLA-DATUM                                           
067800         IF STATUS-OK                                                     
067900           PERFORM FB-BEHANDLA-RAD                                        
068000         ELSE                                                             
068100           ADD -1 TO INDX                                                 
068200         END-IF                                                           
068300         PERFORM IMS-GET-SATB-RAD                                         
068400       ELSE                                                               
068500         MOVE '9' TO W-IDKDSTRRAD                                         
068600         PERFORM IMS-GET-SATB-RAD                                         
068700         IF SEGMENT-FINNS                                                 
068800           PERFORM FA-KOLLA-DATUM                                         
068900           IF STATUS-OK                                                   
069000             PERFORM FB-BEHANDLA-RAD                                      
069100           ELSE                                                           
069200             ADD -1 TO INDX                                               
069300           END-IF                                                         
069400           PERFORM IMS-GET-SATB-RAD                                       
069500         ELSE                                                             
069600***        MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                       
069700           MOVE MFS-RENSA-FAELT TO  MOD-REANTPSA(INDX)                    
069800                                    MOD-IDLEVNR(INDX)                     
069900                                    MOD-ART-LEV(INDX)                     
070000                                    MOD-TILLK-IDAO(INDX)                  
070100                                    MOD-START-VECKA(INDX)                 
070200                                    MOD-UTGR-IDAO(INDX)                   
070300                                    MOD-STOPP-VECKA(INDX)                 
070400                                    MOD-STRTYP(INDX)                      
070500         END-IF                                                           
070600       END-IF                                                             
070700       ADD +1 TO INDX                                                     
070800     END-PERFORM                                                          
070900                                                                          
071000     IF SEGMENT-FINNS                                                     
071100       PERFORM FA-KOLLA-DATUM                                             
071200       IF STATUS-OK                                                       
071300         MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT2                       
071400         MOVE MED1(S-IX) TO MOD-TEMFSINF                                  
071500       ELSE                                                               
071600         PERFORM IMS-GET-SATB-RAD                                         
071700         IF SEGMENT-FINNS                                                 
071800           PERFORM UNTIL(SEGMENT-SAKNAS) OR (STATUS-OK)                   
071900             IF SEGMENT-FINNS                                             
072000               PERFORM FA-KOLLA-DATUM                                     
072100               IF STATUS-OK                                               
072200                 MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT2               
072300                 MOVE MED1(S-IX)       TO MOD-TEMFSINF                    
072400               ELSE                                                       
072500                 PERFORM IMS-GET-SATB-RAD                                 
072600               END-IF                                                     
072700             ELSE                                                         
072800               MOVE ZERO TO MOD-IDRADNR-DOLT2                             
072900             END-IF                                                       
073000           END-PERFORM                                                    
073100         ELSE                                                             
073200           MOVE ZERO TO MOD-IDRADNR-DOLT2                                 
073300         END-IF                                                           
073400       END-IF                                                             
073500     ELSE                                                                 
073600       MOVE ZERO  TO MOD-IDRADNR-DOLT2                                    
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 FA-KOLLA-DATUM SECTION.                                                  
074200                                                                          
074300     MOVE SATB-RAD-TISTODAT TO STOPP-DATUM                                
074400                                                                          
074500     IF STOPP-DATUM = '99999'                                             
074600       MOVE JA TO STOPP-DATUM-SW                                          
074700       MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                            
074800       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
074900       CALL WDATKONV USING DAT-KDDATFORM                                  
075000                           DAT-I-TIDATUM                                  
075100                           DAT-O-TIDATUM                                  
075200                           DAT-KDSVAR                                     
075300                                                                          
075400         MOVE DAT-TIAA-VECKA TO SPAR-START-AA                             
075500         MOVE DAT-TIVV     TO SPAR-START-VV                               
075600                                                                          
075601       MOVE SPAR-START-DATUM    TO TMP1-YYWW                              
075602       MOVE VECKA-TOM           TO TMP2-YYWW                              
075610       PERFORM WY2000P3                                                   
075700       IF TMP1-YYWW <= TMP2-YYWW                                          
075800         MOVE JA TO STATUS-SW                                             
075900       ELSE                                                               
076000         MOVE NEJ TO STATUS-SW                                            
076100       END-IF                                                             
076101       MOVE SPAR-START-DATUM   TO TMP1-YYWW                               
076102       MOVE VECKA-FOM          TO TMP2-YYWW                               
076110       PERFORM WY2000P3                                                   
076200       IF TMP1-YYWW < TMP2-YYWW                                           
076300         MOVE JA TO START-DATUM-SW                                        
076400       ELSE                                                               
076500         MOVE NEJ TO START-DATUM-SW                                       
076600       END-IF                                                             
076700     ELSE                                                                 
076800       MOVE NEJ TO STOPP-DATUM-SW                                         
076900       IF SATB-RAD-KDISATS = 'U' OR 'E'                                   
076901         MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                          
076902         MOVE DAGENS-DATUM        TO TMP2-YYMMDD                          
076910         PERFORM WY2000P1                                                 
077000         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
077100             MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                      
077200             MOVE 'AAMMDD' TO DAT-KDDATFORM                               
077300             CALL WDATKONV USING DAT-KDDATFORM                            
077400                                 DAT-I-TIDATUM                            
077500                                 DAT-O-TIDATUM                            
077600                                 DAT-KDSVAR                               
077700                                                                          
077800               MOVE DAT-TIAA-VECKA TO SPAR-START-AA                       
077900               MOVE DAT-TIVV   TO SPAR-START-VV                           
078000                                                                          
078100               MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                    
078200               MOVE 'AAMMDD' TO DAT-KDDATFORM                             
078300               CALL WDATKONV USING DAT-KDDATFORM                          
078400                                   DAT-I-TIDATUM                          
078500                                   DAT-O-TIDATUM                          
078600                                   DAT-KDSVAR                             
078700                                                                          
078800               MOVE DAT-TIAA-VECKA TO SPAR-STOPP-AA                       
078900               MOVE DAT-TIVV TO SPAR-STOPP-VV                             
079000                                                                          
079001               MOVE SPAR-STOPP-DATUM    TO TMP1-YYWW                      
079002               MOVE VECKA-FOM           TO TMP2-YYWW                      
079003               MOVE VECKA-TOM           TO TMP3-YYWW                      
079010               PERFORM WY2000Q3                                           
079100               IF (TMP1-YYWW >= TMP2-YYWW)  AND                           
079200                  (TMP1-YYWW <= TMP3-YYWW)                                
079300                 MOVE JA TO BEHANDLING-SW                                 
079400                 MOVE NEJ TO STOPP-DATUM-SW                               
079500               ELSE                                                       
079600                 MOVE NEJ TO BEHANDLING-SW                                
079700                 MOVE JA TO STOPP-DATUM-SW                                
079800               END-IF                                                     
079900                                                                          
079901               MOVE SPAR-START-DATUM    TO TMP1-YYWW                      
079902               MOVE VECKA-TOM           TO TMP2-YYWW                      
079910               PERFORM WY2000P3                                           
080000               IF TMP1-YYWW <= TMP2-YYWW                                  
080100                 MOVE JA TO BEHANDLING-SW                                 
080200               ELSE                                                       
080300                 MOVE NEJ TO BEHANDLING-SW                                
080400               END-IF                                                     
080500                                                                          
080501               MOVE SPAR-START-DATUM   TO TMP1-YYWW                       
080502               MOVE VECKA-FOM          TO TMP2-YYWW                       
080510               PERFORM WY2000P3                                           
080600               IF TMP1-YYWW < TMP2-YYWW                                   
080700                 MOVE JA TO START-DATUM-SW                                
080800               ELSE                                                       
080900                 MOVE NEJ TO START-DATUM-SW                               
081000               END-IF                                                     
081100         ELSE                                                             
081200           MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                        
081300           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
081400           CALL WDATKONV USING DAT-KDDATFORM                              
081500                               DAT-I-TIDATUM                              
081600                               DAT-O-TIDATUM                              
081700                               DAT-KDSVAR                                 
081800                                                                          
081900             MOVE DAT-TIAA-VECKA TO SPAR-START-AA                         
082000             MOVE DAT-TIVV     TO SPAR-START-VV                           
082100                                                                          
082200           MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                        
082300           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
082400           CALL WDATKONV USING DAT-KDDATFORM                              
082500                               DAT-I-TIDATUM                              
082600                               DAT-O-TIDATUM                              
082700                               DAT-KDSVAR                                 
082800                                                                          
082900           MOVE DAT-TIAA-VECKA TO SPAR-STOPP-AA                           
083000           MOVE DAT-TIVV TO SPAR-STOPP-VV                                 
083100                                                                          
083101           MOVE SPAR-START-DATUM   TO TMP1-YYWW                           
083102           MOVE VECKA-FOM          TO TMP2-YYWW                           
083110           PERFORM WY2000P3                                               
083200           IF TMP1-YYWW < TMP2-YYWW                                       
083300             MOVE JA TO START-DATUM-SW                                    
083400           ELSE                                                           
083500             MOVE NEJ TO START-DATUM-SW                                   
083600           END-IF                                                         
083700                                                                          
083701           MOVE SPAR-STOPP-DATUM    TO TMP1-YYWW                          
083702           MOVE VECKA-FOM           TO TMP2-YYWW                          
083703           MOVE VECKA-TOM           TO TMP3-YYWW                          
083710           PERFORM WY2000Q3                                               
083800           IF (TMP1-YYWW >= TMP2-YYWW)  AND                               
083900              (TMP1-YYWW <= TMP3-YYWW)                                    
084000             MOVE JA TO BEHANDLING-SW                                     
084100           ELSE                                                           
084200             MOVE NEJ TO BEHANDLING-SW                                    
084300           END-IF                                                         
084400         END-IF                                                           
084500       ELSE                                                               
084600         MOVE JA TO BEHANDLING-SW                                         
084700         MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                          
084800         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
084900         CALL WDATKONV USING DAT-KDDATFORM                                
085000                             DAT-I-TIDATUM                                
085100                             DAT-O-TIDATUM                                
085200                             DAT-KDSVAR                                   
085300                                                                          
085400         MOVE DAT-TIAA-VECKA TO SPAR-START-AA                             
085500         MOVE DAT-TIVV         TO SPAR-START-VV                           
085600                                                                          
085700         MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                          
085800         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
085900         CALL WDATKONV USING DAT-KDDATFORM                                
086000                             DAT-I-TIDATUM                                
086100                             DAT-O-TIDATUM                                
086200                             DAT-KDSVAR                                   
086300                                                                          
086400           MOVE DAT-TIAA-VECKA TO SPAR-STOPP-AA                           
086500           MOVE DAT-TIVV       TO SPAR-STOPP-VV                           
086600                                                                          
086700       END-IF                                                             
086800                                                                          
086900                                                                          
087000       IF BEHANDLING-OK                                                   
087001         MOVE SPAR-START-DATUM   TO TMP1-YYWW                             
087002         MOVE VECKA-TOM          TO TMP2-YYWW                             
087003         MOVE SPAR-STOPP-DATUM   TO TMP3-YYWW                             
087004         MOVE VECKA-FOM          TO TMP4-YYWW                             
087010         PERFORM WY2000Q3                                                 
087100         IF (TMP1-YYWW <= TMP2-YYWW)  AND                                 
087200            (TMP3-YYWW >= TMP4-YYWW)                                      
087300           MOVE JA TO STATUS-SW                                           
087400         ELSE                                                             
087500           MOVE NEJ TO STATUS-SW                                          
087600         END-IF                                                           
087700       ELSE                                                               
087800         MOVE NEJ TO STATUS-SW                                            
087900       END-IF                                                             
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300                                                                          
088400 FB-BEHANDLA-RAD SECTION.                                                 
088500                                                                          
088600     MOVE SATB-RAD-REANTPSA    TO MOD-REANTPSA(INDX)                      
088700     IF START-DATUM-OK                                                    
088800       MOVE MFS-RENSA-FAELT    TO MOD-START-VECKA(INDX)                   
088900       MOVE MFS-RENSA-FAELT    TO MOD-TILLK-IDAO(INDX)                    
089000     ELSE                                                                 
089100       MOVE SPAR-START-DATUM   TO MOD-START-VECKA(INDX)                   
089200       MOVE SATB-RAD-IDAO-STA  TO MOD-TILLK-IDAO(INDX)                    
089300     END-IF                                                               
089400     MOVE SATB-RAD-IDSTRTYP    TO MOD-STRTYP(INDX)                        
089500                                                                          
089600     IF SATB-RAD-IDLEVNR NOT = SPACE                                      
089700       MOVE SATB-RAD-IDLEVNR   TO MOD-IDLEVNR(INDX)                       
089800       MOVE SATB-RAD-BELEVART  TO MOD-ART-LEV(INDX)                       
089900     ELSE                                                                 
090000       IF SATB-RAD-IDARTNR = ZERO                                         
090100         IF  IDSKYLT-WS = 'S  '                                           
090200           MOVE SATB-RAD-BEART-SVE TO MOD-ART-LEV(INDX)                   
090300         ELSE                                                             
090400           MOVE SATB-RAD-BEART-SVE TO W-BEART                             
090500           MOVE SATB-RAD-KDBENHOM  TO KDBENHOM-WS                         
090600           PERFORM S01-HAEMTA-BEART-ASEQ                                  
090700           MOVE BEART-WS TO MOD-ART-LEV(INDX)                             
090800         END-IF                                                           
090900         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(INDX)                        
091000       ELSE                                                               
091100         MOVE SATB-RAD-IDARTNR   TO MOD-ART-LEV(INDX)                     
091200         INSPECT MOD-ART-LEV(INDX) REPLACING LEADING                      
091300                 ZERO BY SPACE                                            
091400         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(INDX)                        
091500       END-IF                                                             
091600     END-IF                                                               
091700                                                                          
091800     IF STOPP-DATUM-OK                                                    
091900       MOVE MFS-RENSA-FAELT    TO MOD-STOPP-VECKA(INDX)                   
092000       MOVE MFS-RENSA-FAELT    TO MOD-UTGR-IDAO(INDX)                     
092100     ELSE                                                                 
092200       MOVE SPAR-STOPP-DATUM   TO MOD-STOPP-VECKA(INDX)                   
092300       MOVE SATB-RAD-IDAO-STO  TO MOD-UTGR-IDAO(INDX)                     
092400     END-IF                                                               
092500     .                                                                    
092600     EJECT                                                                
092700 G-KOLLA-INPUT-PRINTER SECTION.                                           
092800                                                                          
092900***  MOVE NEJ TO ALLT-SW                                                  
093000***  IF MID-KDPRTVAL = ALL '+'                                            
093100***    MOVE NEJ TO INDATA-SW                                              
093200***  ELSE                                                                 
093300***    IF MID-KDPRTVAL = 'A' OR 'B' OR 'C' OR 'D'                         
093500***      MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ATTR                   
093600***      EVALUATE MID-KDPRTVAL                                            
093800***         WHEN 'A' MOVE SATS-HIST-RA     TO PRT-IDPRTLST                
093900***         WHEN 'B' MOVE SATS-HIST-RB     TO PRT-IDPRTLST                
094200***         WHEN 'C' MOVE SATS-HIST-BERPV  TO PRT-IDPRTLST                
094310***         WHEN 'D' MOVE SATS-HIST-CARP   TO PRT-IDPRTLST                
094400***      END-EVALUATE                                                     
094500*        **************************************************               
094600*        *  HÄMTAR PRINTERNS LOGISKA NAMN TILL TEMFSINF   *               
094700*        **************************************************               
094800                                                                          
094900***      MOVE 1                TO PRT-KDCALL                              
095000***      CALL W006PRT  USING PRT-W006PRT                                  
095100***      IF PRT-IDLTERM = 'SAKNAS  '                                      
095200***        MOVE NEJ TO INDATA-SW                                          
095300***        MOVE FELMED4(S-IX)  TO MED-4                                   
095400***        MOVE MED-4          TO MOD-TEMFSINF                            
095500***      ELSE                                                             
095600***        MOVE MED4TXT(S-IX)  TO MED4-TXT                                
095700***        MOVE PRT-IDLTERM    TO MED4-IDLTERM                            
095800***        IF S-IX = +1                                                   
095900***          MOVE PRT-BEPRTLST TO MED4-BEPRT                              
096000***        ELSE                                                           
096100***          MOVE SPACE        TO MED4-BEPRT                              
096200***        END-IF                                                         
096300***      END-IF                                                           
096400***    ELSE                                                               
096500***      MOVE NEJ TO INDATA-SW                                            
096600***    END-IF                                                             
096700***  END-IF                                                               
096800                                                                          
096900***  PERFORM MFS-ROER-EJ-FAELT-IN                                         
097000***  PERFORM MFS-ROER-EJ-FAELT-UT                                         
097100***  PERFORM MFS-LAES-IN-IGEN                                             
097200                                                                          
097300***  IF INDATA-FEL                                                        
097400***    MOVE FEL1(S-IX)         TO MOD-TEMFSFEL                            
097500***    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-ATTR                       
097600***  END-IF                                                               
097700     .                                                                    
097800     EJECT                                                                
097900 H-SKICKA-TRANS-TILL-1292 SECTION.                                        
098000****************************************                                  
098100* STARTA BKGRUNDSPGM SOM PRINTAR LISTA *                                  
098200****************************************                                  
098300                                                                          
098400***  MOVE MID-W1I21401 TO MID-W1I29201                                    
098500***  PERFORM IMS-INSERT-ALT-MSG                                           
098600***  MOVE MED-4       TO MOD-TEMFSINF                                     
098700     .                                                                    
098800     EJECT                                                                
098900 S01-HAEMTA-BEART-ASEQ SECTION.                                           
099000                                                                          
099100     MOVE 'S  ' TO W-IDSKYLT                                              
099200     PERFORM IMS-GET-BENA01-ASEQ                                          
099300     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
099400                   KDBENHOM-WS = BENA01-BEN-KDHOMONYM                     
099500       IF SEGMENT-FINNS                                                   
099600         IF KDBENHOM-WS = BENA01-BEN-KDHOMONYM                            
099700           CONTINUE                                                       
099800         ELSE                                                             
099900           PERFORM IMS-GET-BENA01-ASEQ-NEXT                               
100000         END-IF                                                           
100100       END-IF                                                             
100200     END-PERFORM                                                          
100300                                                                          
100400     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
100500     IF SEGMENT-FINNS                                                     
100600       PERFORM IMS-GET-BENA11-ASEQ                                        
100700       IF SEGMENT-FINNS                                                   
100800         MOVE BENA11-TEXT-BEART TO BEART-WS                               
100900       ELSE                                                               
101000         MOVE SPACE             TO BEART-WS                               
101100       END-IF                                                             
101200     ELSE                                                                 
101300       MOVE SPACE TO BEART-WS                                             
101400     END-IF                                                               
101500     .                                                                    
101600     EJECT                                                                
101700 MFS-RENSA-FAELT-IN SECTION.                                              
101800                                                                          
101900***  MOVE +1 TO INDX                                                      
102000***  PERFORM UNTIL INDX > MAX-SELECT                                      
102100***    MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
102200***    ADD +1 TO INDX                                                     
102300***  END-PERFORM                                                          
102400     .                                                                    
102500                                                                          
102600 MFS-RENSA-FAELT-UT SECTION.                                              
102700                                                                          
102800     MOVE MFS-RENSA-FAELT   TO MOD-IDRADNR-DOLT                           
102900                               MOD-IDRADNR-DOLT2                          
103000***                            MOD-IDSATSNR-DOLT                          
103100     MOVE +1 TO INDX                                                      
103200     PERFORM UNTIL INDX > MAX-IDRAD                                       
103300***    MOVE MFS-RENSA-FAELT TO MOD-SELECT (INDX)                          
103400       MOVE MFS-RENSA-FAELT TO MOD-REANTPSA(INDX)                         
103500                               MOD-IDLEVNR(INDX)                          
103600                               MOD-ART-LEV(INDX)                          
103700                               MOD-TILLK-IDAO(INDX)                       
103800                               MOD-START-VECKA(INDX)                      
103900                               MOD-UTGR-IDAO(INDX)                        
104000                               MOD-STOPP-VECKA(INDX)                      
104100                               MOD-STRTYP(INDX)                           
104200       ADD +1 TO INDX                                                     
104300     END-PERFORM                                                          
104400     .                                                                    
104500     SKIP2                                                                
104600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
104700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-DOLT                           
104800                               MOD-IDRADNR-DOLT2                          
104900***                            MOD-IDSATSNR-DOLT                          
105000     MOVE +1 TO INDX                                                      
105100     PERFORM UNTIL INDX > MAX-IDRAD                                       
105200***    MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                         
105300       MOVE MFS-ROER-EJ-FAELT TO MOD-REANTPSA(INDX)                       
105400                               MOD-IDLEVNR(INDX)                          
105500                               MOD-ART-LEV(INDX)                          
105600                               MOD-TILLK-IDAO(INDX)                       
105700                               MOD-START-VECKA(INDX)                      
105800                               MOD-UTGR-IDAO(INDX)                        
105900                               MOD-STOPP-VECKA(INDX)                      
106000                               MOD-STRTYP(INDX)                           
106100       ADD +1 TO INDX                                                     
106200     END-PERFORM                                                          
106300     .                                                                    
106400     SKIP2                                                                
106500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
106600                                                                          
106700***  MOVE +1 TO INDX                                                      
106800***  PERFORM UNTIL INDX > MAX-SELECT                                      
106900***    MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                         
107000***    ADD +1 TO INDX                                                     
107100***  END-PERFORM                                                          
107200***  MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRTVAL                               
107300     .                                                                    
107400     EJECT                                                                
107500                                                                          
107600 MFS-RENSA-FAELT-INMAT SECTION.                                           
107700                                                                          
107800***  MOVE +1 TO INDX                                                      
107900***  PERFORM UNTIL INDX > MAX-SELECT                                      
108000***    MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
108100***    ADD +1 TO INDX                                                     
108200***  END-PERFORM                                                          
108300     .                                                                    
108400     EJECT                                                                
108500 MFS-LAES-IN-IGEN SECTION.                                                
108600                                                                          
108700***  MOVE +1 TO INDX                                                      
108800***  PERFORM UNTIL INDX > MAX-SELECT                                      
108900***    MOVE MFS-ADD-LAES-IN-FAELT TO  MOD-SELECT-ATTR(INDX)               
109000***    ADD +1 TO INDX                                                     
109100***  END-PERFORM                                                          
109200***  MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTVAL-ATTR                      
109300     .                                                                    
109400     EJECT                                                                
109500 MFS-FORM-ATTR SECTION.                                                   
109600                                                                          
109700***  MOVE +1 TO INDX                                                      
109800***  PERFORM UNTIL INDX > MAX-SELECT                                      
109900***    MOVE MFS-FORMATETS-ATTR TO  MOD-SELECT-ATTR(INDX)                  
110000***    ADD +1 TO INDX                                                     
110100***  END-PERFORM                                                          
110200***  MOVE MFS-FORMATETS-ATTR TO MOD-KDPRTVAL-ATTR                         
110300     .                                                                    
110400     EJECT                                                                
110500* IMS SEKTIONER                                                           
110600     SKIP3                                                                
110700 IMS-GET-MSG SECTION.                                                     
110800                                                                          
110900     MOVE '  QC' TO GODK-STATUSKODER                                      
111000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
111100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111200     PERFORM IMS-STATUSKONTROLL                                           
111300     .                                                                    
111400     SKIP3                                                                
111500 IMS-INSERT-MSG SECTION.                                                  
111600                                                                          
111700***  IF SWEDISH-TEXT                                                      
111800***    MOVE '0' TO MFS-KDHUVOMR                                           
111900***  END-IF                                                               
112000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
112100     MOVE SPACE TO GODK-STATUSKODER                                       
112200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
112300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
112400     PERFORM IMS-STATUSKONTROLL                                           
112500     .                                                                    
112600     EJECT                                                                
112700 IMS-INSERT-ALT-MSG SECTION.                                              
112800                                                                          
112900     MOVE SPACE TO GODK-STATUSKODER                                       
113000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
113100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     EJECT                                                                
113500 IMS-GET-SATB-ART SECTION.                                                
113600                                                                          
113700     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
113800          DELIMITED BY SIZE INTO SSA1                                     
113900     MOVE '  GE' TO GODK-STATUSKODER                                      
114000     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
114100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
114200     PERFORM IMS-STATUSKONTROLL                                           
114300     .                                                                    
114400                                                                          
114500 IMS-GET-SATB-RAD SECTION.                                                
114600                                                                          
114700     STRING 'WLSATB11(WDJ111KY=>' W-IDKDSTRRAD-X                          
114800                                  W-IDRADNR-X ')'                         
114900          DELIMITED BY SIZE INTO SSA1                                     
115000     MOVE '  GE' TO GODK-STATUSKODER                                      
115100     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
115200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
115300     PERFORM IMS-STATUSKONTROLL                                           
115400     .                                                                    
115500                                                                          
115600 IMS-GET-SATB-RAD-FIRST SECTION.                                          
115700                                                                          
115800     STRING 'WLSATB11*F(WDJ111KY=>' W-IDKDSTRRAD-X                        
115900                                    W-IDRADNR-X ')'                       
116000          DELIMITED BY SIZE INTO SSA1                                     
116100     MOVE '  GE' TO GODK-STATUSKODER                                      
116200     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
116300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600                                                                          
116700 IMS-GET-BENA01-ASEQ SECTION.                                             
116800                                                                          
116900     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
117000                                  W-BEART-X ')'                           
117100          DELIMITED BY SIZE INTO SSA1                                     
117200     MOVE '  GE' TO GODK-STATUSKODER                                      
117300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
117400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
117500     PERFORM IMS-STATUSKONTROLL                                           
117600     .                                                                    
117700                                                                          
117800 IMS-GET-BENA01-ASEQ-NEXT SECTION.                                        
117900                                                                          
118000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
118100                                  W-BEART-X ')'                           
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE '  GE' TO GODK-STATUSKODER                                      
118400     CALL CBLTDLI USING GN BENA-PCB DLI-IO-AREA SSA1                      
118500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUSKONTROLL                                           
118700     .                                                                    
118800                                                                          
118900 IMS-GET-BENA11-ASEQ SECTION.                                             
119000                                                                          
119100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
119200          DELIMITED BY SIZE INTO SSA1                                     
119300     MOVE '  GE' TO GODK-STATUSKODER                                      
119400     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
119500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
119600     PERFORM IMS-STATUSKONTROLL                                           
119700     .                                                                    
119800                                                                          
119900 IMS-STATUSKONTROLL SECTION.                                              
120000                                                                          
120100     SET STATUS-IX TO 1                                                   
120200     SEARCH GODK-STATUS                                                   
120300       AT END CALL FELLOG                                                 
120400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
120500     END-SEARCH                                                           
120600     .                                                                    
120610     EJECT                                                                
120700*    -COPY WY2000P1                                                       
120710     EJECT                                                                
120800*    -COPY WY2000P3                                                       
120900     EJECT                                                                
121000*    -COPY WY2000Q3                                                       
