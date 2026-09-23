000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2011500.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   FEB 1990.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING LEVERANTÖRSDATA.                                     
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T115                                              
001400*        MID:         W2I11501                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W2O11501                                            
001800*                                                                         
001900******************************************************************        
002000*   ÄNDRINGAR:                                                            
002100*     2016-07-18  SCR 10217639 ÄNDRAD LAYOUT AV ATTENTION/KONTAKT-        
002200*                              INFORMATION TILL FASTA FÄLT: WDF107        
002300*                                                                         
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100*                                                                         
003200*    -- CHECKED BY WY2000                                                 
003300*                                                                         
003400 77  IDPGM                   PIC X(8)    VALUE 'W2011500'.                
003500 77  JA                      PIC X       VALUE 'J'.                       
003600 77  NEJ                     PIC X       VALUE 'N'.                       
003700 77  CURRENT-SECTION         PIC X(30)   VALUE SPACE.                     
003800 77  DBS-SECTION             PIC X(30)   VALUE SPACE.                     
003900 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004000 77  MAX-IDATTENT            PIC S9(9)   VALUE +2   COMP SYNC.            
004100 77  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                     
004200 77  WS-IDLEVNR-NUM          PIC X(5)    VALUE SPACE.                     
004230                                                                          
004400 77  INDATA-SW               PIC X       VALUE 'J'.                       
004500   88  INDATA-OK                         VALUE 'J'.                       
004600   88  INDATA-FEL                        VALUE 'N'.                       
004700                                                                          
004800 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
004900   88  NYCKLAR-OK                        VALUE 'J'.                       
005000   88  NYCKLAR-FEL                       VALUE 'N'.                       
005100                                                                          
005200 77  ATTRAD-IFYLLT-SW        PIC X       VALUE 'N'.                       
005300   88  ATTRAD-IFYLLT                     VALUE 'J'.                       
005400                                                                          
005500 77  VISA-INFO-SW            PIC X       VALUE 'J'.                       
005600   88  VISA-INFO-OK                      VALUE 'J'.                       
005700   88  VISA-INFO-FEL                     VALUE 'N'.                       
005800                                                                          
005900 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006000   88  EGEN-TRANS                        VALUE '2115'.                    
006100   88  GODK-TRANS                        VALUE '2115'                     
006200                                               '2111' '2112'              
006300                                               '2113' '2114'.             
006400                                                                          
006500 01  ARBETSAREOR.                                                         
006600     05  W-DATUM-X.                                                       
006700         10  W-DATUM         PIC 9(6).                                    
006800                                                                          
006900     05  W-TIME-X.                                                        
007000         10  W-TIME-TT       PIC 9(2).                                    
007100         10  FILLER          PIC 9(2).                                    
007200         10  W-TIME-SS       PIC 9(2).                                    
007300         10  FILLER          PIC 9(2).                                    
007400     05  W-TIME-N            REDEFINES W-TIME-X                           
007500                             PIC 9(8).                                    
007600     05  W-TIKLOCK           PIC S9(9)  VALUE 0   COMP-3.                 
007700     05  W-IDSEKVNR          PIC S9(3)  VALUE 0   COMP-3.                 
007800*                                                                         
007900 01  SWITCHAR.                                                            
008000     05  SW-INPUT-RAETT      PIC X(1)    VALUE 'J'.                       
008100     EJECT                                                                
008200                                                                          
008300 01  PARM-W224S2.                                                         
008400     03  FILLER                  PIC X(8)    VALUE 'IDLEVNR('.            
008500     03  PARM-W224S2-IDLEVNR     PIC X(5).                                
008600     03  FILLER                  PIC X(1)    VALUE ')'.                   
008700     03  FILLER                  PIC X(3)    VALUE 'DC('.                 
008800     03  PARM-W224S2-IDDC        PIC X(2).                                
008900     03  FILLER                  PIC X(1)    VALUE ')'.                   
009000     03  FILLER                  PIC X(3)    VALUE 'AT('.                 
009100     03  PARM-W224S2-KVVECKOR-AT PIC X(2).                                
009200     03  FILLER                  PIC X(1)    VALUE ')'.                   
009300     03  FILLER                  PIC X(3)    VALUE 'LT('.                 
009400     03  PARM-W224S2-KVVECKOR-LT PIC X(2).                                
009500     03  FILLER                  PIC X(1)    VALUE ')'.                   
009600     03  FILLER                  PIC X(3)    VALUE 'TT('.                 
009700     03  PARM-W224S2-KVDAGAR-TT  PIC X(2).                                
009800     03  FILLER                  PIC X(1)    VALUE ')'.                   
009900     03  FILLER                  PIC X(42)   VALUE SPACE.                 
010000     EJECT                                                                
010100                                                                          
010200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010300 01  GENERELLA-SUBPROGRAM.                                                
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010710     03  W005MCHK                PIC X(8)    VALUE 'W005MCHK'.            
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011000*01 -COPY WMEDAREA                                                        
011100     SKIP3                                                                
011110*    --- MAILID CHECK    SUBPROGRAM W005MCHK                              
011120*01 -COPY W005MCHK                                                        
011130     SKIP3                                                                
011200 01  MESSAGE-CODES.                                                       
011300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011800     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
011900     03  ERR-KONFLIKT            PIC X(3)    VALUE '002'.                 
012000     03  ERR-PRESS-ENTER         PIC X(3)    VALUE '144'.                 
012100     03  ERR-NO-UPDATE           PIC X(3)    VALUE '034'.                 
012200     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
012300     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '005'.                 
012400     03  ERR-MISSING-IN-REGISTER PIC X(3)    VALUE '010'.                 
012430     03  ERR-ITEMS-MISSING       PIC X(3)    VALUE '029'.                 
012440     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
012500     EJECT                                                                
012600*                                                                         
012700 01  MEDDELANDE.                                                          
012800   03    MED1                PIC X(40)   VALUE                            
012900             'MER INFORMATION FINNS, TRYCK PF8'.                          
013000   03    MED2                PIC X(40)   VALUE                            
013100             'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                     
013200   03    MED3                PIC X(40)   VALUE                            
013300             'UPPDATERING UTFÖRD'.                                        
013400                                                                          
013500   03    FEL0                PIC X(50)   VALUE                            
013600             'PF23 TILLÅTER ENDAST UPPDAT. ADRESSUPPG.'.                  
013700   03    FEL1                PIC X(40)   VALUE                            
013800             'LEVERANTÖR SAKNAS'.                                         
013900   03    FEL2                PIC X(40)   VALUE                            
014000             'LEVERANTÖRSNUMMER FELAKTIGT   '.                            
014100   03    FEL3                PIC X(40)   VALUE                            
014200             'DETTA ÄR FÖRSTA SIDAN'.                                     
014300   03    FEL4                PIC X(40)   VALUE                            
014400             'TRYCK PF11 FÖR UPPDATERING'.                                
014500   03    FEL5                PIC X(40)   VALUE                            
014600             'UPPLYSTA FÄLT FEL'.                                         
014700   03    FEL6                PIC X(50)   VALUE                            
014800             'ADRESSUPPGIFTER KAN EJ UPPDATERAS (SEGMENT SAKNAS)'.        
014900   03    FEL7                PIC X(47) VALUE                              
015000             'ANVÄND MOTSV LEVNR, SE BILD 2111'.                          
015100   03    FEL8                PIC X(40) VALUE                              
015200             'TILLÅTNA VÄRDEN 0, 1 ELLER 2.'.                             
015300   03    FEL9                PIC X(40) VALUE                              
015400             'TILLÅTNA VÄRDEN MÅ,TI,ON,TO EL. FR'.                        
015500   03    FEL30               PIC X(40) VALUE                              
015600             'SKALL VARA NUMERISKT (EV 1 DEC.)  '.                        
015650     EJECT                                                                
015800* ENGELSK TEXT                                                            
015900   03    MED11               PIC X(40)   VALUE                            
016000             'FOR MORE INFORMATION, PRESS PF8'.                           
016100   03    MED12               PIC X(40)   VALUE                            
016200             'FILL IN INPUT FIELD TO UPDATE'.                             
016300   03    MED13               PIC X(40)   VALUE                            
016400             'UPDATED'.                                                   
016500   03    FEL10               PIC X(40)   VALUE                            
016600             'PF23 ALLOWS ONLY UPDATE ADDRESS'.                           
016700   03    FEL11               PIC X(40)   VALUE                            
016800             'SUPPLIER IS MISSING'.                                       
016900   03    FEL12               PIC X(40)   VALUE                            
017000             'WRONG SUPPLIER NO.'.                                        
017100   03    FEL13               PIC X(40)   VALUE                            
017200             'THIS IS THE FIRST PAGE'.                                    
017300   03    FEL14               PIC X(40)   VALUE                            
017400             'PRESS PF11 TO UPDATE'.                                      
017500   03    FEL15               PIC X(40)   VALUE                            
017600             'HIGHLIGHT FIELDS WRONG'.                                    
017700   03    FEL16               PIC X(50)   VALUE                            
017800             'UPDATE ADDRESS NOT POSSIBLE (SEGMENT IS MISSING)'.          
017900   03    FEL17               PIC X(47) VALUE                              
018000             'USE EQUAL SUPPLIER, SEE SCREEN 2111'.                       
018100   03    FEL18               PIC X(40) VALUE                              
018200             'ALLOWED VALUE 0, 1 OR 2.'.                                  
018300   03    FEL19               PIC X(40) VALUE                              
018400             'ALLOWED VALUE MÅ,TI,ON,TO OR FR'.                           
018500   03    FEL40               PIC X(40) VALUE                              
018600             'ALLOWED VALUE MUST BE NUMERIC  '.                           
018700                                                                          
018800     EJECT                                                                
018820 01  WS-MFSFEL-029.                                                       
018830     05  FILLER              PIC X(17).                                   
018840     05  WS-KOLUM-029        PIC X(01).                                   
018850     05  FILLER              PIC X(01).                                   
018860     05  WS-IDTECKEN-029     PIC X(21).                                   
018880 01  WS-MFSFEL-492.                                                       
018890     05  FILLER              PIC X(17).                                   
018891     05  WS-KOLUM-492        PIC X(01).                                   
018892     05  FILLER              PIC X(01).                                   
018893     05  WS-IDTECKEN-492     PIC X(21).                                   
018894                                                                          
018900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019200     SKIP3                                                                
019300*01 -COPY WMSGINIT                                                        
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'WWDC99 '.             
019600*01   -COPY WWDC99                                                        
019700     EJECT                                                                
019800*01  -COPY WWDCKONS                                                       
019900     EJECT                                                                
020000******************************************************************        
020100*    --- AREA FÖR SOP ANROP                                               
020200 01  W-PROG-TO-PROG-SW.                                                   
020300*03 -COPY WMSGSOP                                                         
020400******************************************************************        
020500*                                                                         
020600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020700*                                                                         
020800 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
020900     SKIP3                                                                
021000*01  MID -COPY W2I11501                                                   
021100     EJECT                                                                
021200*01  -COPY WMSGAREA                                                       
021300     EJECT                                                                
021400*  03  MOD -COPY W2O11501 -RED MSG-AREA.                                  
021500     EJECT                                                                
021600*01  -COPY WMFSAREA                                                       
021700     EJECT                                                                
021800******************************************************************        
021900*                                                                         
022000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022100*                                                                         
022200 01  IMS-WS.                                                              
022300   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
022400     SKIP3                                                                
022500*                        **** STATUS-KOD FRÅN IMS                         
022600   03  STATUS-WS             PIC XX.                                      
022700     88  SEGMENT-FINNS                   VALUE '  '.                      
022800     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
022900     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
023000     88  SEGMENT-HOGRE                   VALUE 'GA'.                      
023100     SKIP3                                                                
023200   03  GODK-STATUSKODER.                                                  
023300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023400     SKIP3                                                                
023500 01  NYCKLAR-TILL-DLI.                                                    
023600   03  W-IDLEVNR-X.                                                       
023700     05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                     
023800   03  W-IDATTENT-X.                                                      
023900     05  W-IDATTENT          PIC  S9(3)  VALUE ZERO  COMP-3.              
024000     SKIP3                                                                
024100 01    SSA1                  PIC X(64).                                   
024200 01    SSA2                  PIC X(64).                                   
024300     EJECT                                                                
024400*    ---  IMS FUNKTIONSKODER                                              
024500*01    -COPY W0003                                                        
024600     EJECT                                                                
024700*    ---  DLI INPUT-OUTPUT AREA                                           
024800                                                                          
024900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
025000 01  DLI-IO-WDF101.                                                       
025100*   03  -COPY WDF101                                                      
025200    EJECT                                                                 
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
025400 01  DLI-IO-WDF106.                                                       
025500*   03  -COPY WDF106                                                      
025600    EJECT                                                                 
025700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF107'.                      
025800 01  DLI-IO-WDF107.                                                       
025900*   03  -COPY WDF107                                                      
026000    EJECT                                                                 
026100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF122'.                      
026200 01  DLI-IO-WDF122.                                                       
026300*   03  -COPY WDF122                                                      
026400    EJECT                                                                 
026500 LINKAGE SECTION.                                                         
026600*01  -COPY W0009     -PRE MSG-                                            
026700     EJECT                                                                
026800*01  -COPY W0009     -PRE ALT-                                            
026900                                                                          
027000*01  -COPY W0008     -PRE WDF1-                                           
027100     05  FILLER              PIC X.                                       
027200*01  -COPY W0008     -PRE WDP7-                                           
027300     05  FILLER              PIC X.                                       
027400     EJECT                                                                
027500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDF1-PCB WDP7-PCB.             
027600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDF1-PCB WDP7-PCB.             
027700                                                                          
027800     PERFORM IMS-GET-MSG                                                  
027900     IF SEGMENT-FINNS                                                     
028000       PERFORM A-INIT                                                     
028100       PERFORM B-KOLLA-NYCKLAR                                            
028200       IF NYCKLAR-OK                                                      
028300         PERFORM IMS-GHU-WDF101                                           
028400         IF SEGMENT-FINNS                                                 
028500           IF MFS-UPDATE OR MFS-UPD-V                                     
028600             PERFORM C-KOLLA-INPUT                                        
028700             IF INDATA-OK                                                 
028800               PERFORM D-UPPDATERA                                        
028900             ELSE                                                         
029000               MOVE NEJ TO VISA-INFO-SW                                   
029100             END-IF                                                       
029200           ELSE                                                           
029300             IF MFS-FIRST                                                 
029400               PERFORM E-BEHANDLA-LEVERANTOER                             
029500             ELSE                                                         
029600               IF MFS-NEXT                                                
029700                 PERFORM F-NAESTA-SIDA                                    
029800               ELSE                                                       
029900                 PERFORM G-SAMMA-SIDA                                     
030000               END-IF                                                     
030100             END-IF                                                       
030200           END-IF                                                         
030300           IF VISA-INFO-OK                                                
030400             PERFORM H-LAES-VISA-LEVA-ATT                                 
030500           END-IF                                                         
030600         ELSE                                                             
030700           IF ENGLISH-TEXT                                                
030800              MOVE FEL11          TO MOD-TEMFSFEL                         
030900           ELSE                                                           
031000              MOVE FEL1           TO MOD-TEMFSFEL                         
031100           END-IF                                                         
031200           PERFORM MFS-RENSA-FAELT-IN                                     
031300           PERFORM MFS-RENSA-FAELT-UT-ATTENT                              
031400         END-IF                                                           
031500       END-IF                                                             
031600       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O11501 + 4                      
031700       PERFORM IMS-INSERT-MSG                                             
031800     END-IF                                                               
031900                                                                          
032000     MOVE ZERO TO RETURN-CODE                                             
032100     GOBACK                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 A-INIT SECTION.                                                          
032500                                                                          
032600     IF MSG-DUBBLA-TRANSKODER                                             
032700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I11501                 
032800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033000     ELSE                                                                 
033100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I11501                  
033200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
033300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033400     END-IF                                                               
033500                                                                          
033600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
033700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
033800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033900                                                                          
034000     MOVE LOW-VALUE TO MSG-AREA                                           
034100     MOVE 'W2O11501' TO MFS-IDMOD                                         
034200     MOVE '2115' TO MOD-IDTRANS                                           
034300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034400                                                                          
034500     MOVE SPACE       TO MED-IDMFSINF                                     
034600     MOVE SPACE       TO MED-IDMFSFEL                                     
034700                                                                          
034800     IF NOT EGEN-TRANS                                                    
034900       MOVE SPACE TO MFS-KDTRTYP                                          
035000       MOVE '7' TO MFS-IDPFK                                              
035100     END-IF                                                               
035200                                                                          
035300     ACCEPT W-DATUM-X     FROM DATE                                       
035400     ACCEPT W-TIME-X      FROM TIME                                       
035500     MOVE W-TIME-N        TO W-TIKLOCK                                    
035600                                                                          
035700     .                                                                    
035800     EJECT                                                                
035900 B-KOLLA-NYCKLAR SECTION.                                                 
036000     MOVE 'B-KOLLA-NYCKLAR '  TO CURRENT-SECTION                          
036100                                                                          
036200     MOVE JA TO VISA-INFO-SW                                              
036300     MOVE JA TO NYCKLAR-SW                                                
036400                                                                          
036500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036600     MOVE '001'             TO MSGI-KDCALL                                
036700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036900     MOVE '2115'            TO MSGI-IDTRANS                               
037000     IF  MID-IDLEVNR-IN NOT  = ALL '+'                                    
037100         MOVE MID-IDLEVNR-IN                                              
037200                            TO MSGI-IDLEVNR                               
037300     END-IF                                                               
037400     CALL W005INIT       USING MSGI-WMSGINIT                              
037500                               WDP7-PCB                                   
037600*                                                                         
037700     MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-IN                             
037800                                                                          
037900     IF MID-IDLEVNR-IN = ALL '+'                                          
038000       MOVE MSGI-IDLEVNR    TO WS-IDLEVNR                                 
038100       IF MID-IDLEVNR-UT    = ALL '+'                                     
038200          MOVE '7'         TO MFS-IDPFK                                   
038300          MOVE SPACE       TO MFS-KDTRTYP                                 
038400       END-IF                                                             
038500     ELSE                                                                 
038600       MOVE MID-IDLEVNR-IN  TO WS-IDLEVNR                                 
038700       MOVE '7'             TO MFS-IDPFK                                  
038800       MOVE SPACE           TO MFS-KDTRTYP                                
038900     END-IF                                                               
039000     IF WS-IDLEVNR NOT = SPACE                                            
039100       MOVE WS-IDLEVNR TO W-IDLEVNR                                       
039200     ELSE                                                                 
039300       MOVE NEJ TO NYCKLAR-SW                                             
039400     END-IF                                                               
039500                                                                          
039600     IF GODK-TRANS OR NYCKLAR-OK                                          
039700       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
039800     ELSE                                                                 
039900       IF NYCKLAR-FEL                                                     
040000         MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                
040100       ELSE                                                               
040200         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                           
040300       END-IF                                                             
040400     END-IF                                                               
040500                                                                          
040600     IF NYCKLAR-FEL                                                       
040700       IF ENGLISH-TEXT                                                    
040800          MOVE FEL12          TO MOD-TEMFSFEL                             
040900       ELSE                                                               
041000          MOVE FEL2           TO MOD-TEMFSFEL                             
041100       END-IF                                                             
041200       PERFORM MFS-RENSA-FAELT-IN                                         
041300       PERFORM MFS-RENSA-FAELT-UT-ATTENT                                  
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 C-KOLLA-INPUT SECTION.                                                   
041900     MOVE 'C-KOLLA-INPUT '  TO CURRENT-SECTION                            
042000                                                                          
042100     MOVE JA  TO INDATA-SW                                                
042200                                                                          
042300***** OM IDATTENT-IN ÄR BLANK BEHANDLAS DEN SOM EJ IFYLLD ***             
042400     IF MID-IDATTENT-IN = SPACE                                           
042500       MOVE '++' TO MID-IDATTENT-IN                                       
042600     END-IF                                                               
042700                                                                          
042800     PERFORM S01-KOLLA-ATT-UPD                                            
042900                                                                          
043000     IF (MID-INPUT = ALL '+') AND                                         
043100        ATTRAD-IFYLLT-SW = NEJ  AND                                       
043200        MID-IDATTENT-UPD = ALL '+' AND                                    
043300        MID-FLAGGA-BORT  = ALL '+'                                        
043400                                                                          
043500       IF ENGLISH-TEXT                                                    
043600          MOVE MED12      TO MOD-TEMFSINF                                 
043700       ELSE                                                               
043800          MOVE MED2       TO MOD-TEMFSINF                                 
043900       END-IF                                                             
044000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
044100       CALL WMEDKONV USING MED-WMEDAREA                                   
044200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044300                                                                          
044400       MOVE NEJ    TO INDATA-SW                                           
044500       PERFORM MFS-ROER-EJ-BILD                                           
044600       PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                                
044700     ELSE                                                                 
044800       IF (MID-INPUT = ALL '+') AND MFS-UPD-V                             
044900         IF ENGLISH-TEXT                                                  
045000            MOVE MED12      TO MOD-TEMFSINF                               
045100         ELSE                                                             
045200            MOVE MED2       TO MOD-TEMFSINF                               
045300         END-IF                                                           
045400         MOVE NEJ    TO INDATA-SW                                         
045500         PERFORM MFS-ROER-EJ-BILD                                         
045600         PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                              
045700       END-IF                                                             
045800                                                                          
045900       IF INDATA-OK                                                       
046000         IF MID-IDATTENT-UPD NOT = ALL '+'                                
046100           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDATTENT-UPD-ATTR               
046200           MOVE NEJ TO INDATA-SW                                          
046300                                                                          
046400           MOVE ERR-PRESS-ENTER     TO MED-IDMFSFEL                       
046500           CALL WMEDKONV USING MED-WMEDAREA                               
046600           MOVE MED-MFSFEL          TO MOD-TEMFSFEL                       
046700           PERFORM MFS-ROER-EJ-BILD                                       
046800           PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                            
046900         ELSE                                                             
047000           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDATTENT-UPD-ATTR             
047100         END-IF                                                           
047200       END-IF                                                             
047300                                                                          
047400       IF INDATA-OK                                                       
047500         IF MID-IDATTENT-IN = ALL '+'                                     
047600           IF MID-FLAGGA-BORT NOT = ALL '+'                               
047700             IF MID-FLAGGA-BORT = SPACE                                   
047800               CONTINUE                                                   
047900             ELSE                                                         
048000               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-BORT-ATTR            
048100               MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                    
048200               CALL WMEDKONV USING MED-WMEDAREA                           
048300               MOVE MED-MFSFEL         TO MOD-TEMFSFEL                    
048400                                                                          
048500               MOVE NEJ    TO INDATA-SW                                   
048600               PERFORM MFS-ROER-EJ-BILD                                   
048700               PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                        
048800             END-IF                                                       
048900           END-IF                                                         
049000         END-IF                                                           
049100       END-IF                                                             
049200                                                                          
049300       IF INDATA-OK                                                       
049400         IF ATTRAD-IFYLLT-SW = JA  AND                                    
049500            MID-IDATTENT-IN = ALL '+'                                     
049600                                                                          
049700            MOVE MFS-NUM-FAELT-FEL  TO MOD-IDATTENT-IN-ATTR               
049800            MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                       
049900            CALL WMEDKONV USING MED-WMEDAREA                              
050000            MOVE MED-MFSFEL         TO MOD-TEMFSFEL                       
050100                                                                          
050200            MOVE NEJ    TO INDATA-SW                                      
050300            PERFORM MFS-ROER-EJ-BILD                                      
050400            PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                           
050500            PERFORM MFS-LAS-IN-IGEN-ATTRAD                                
050600         END-IF                                                           
050700       END-IF                                                             
050800                                                                          
050900       IF INDATA-OK                                                       
051000         IF MID-INPUT NOT = ALL '+'                                       
051100           PERFORM IMS-GHU-WDF101                                         
051200           IF SEGMENT-FINNS                                               
051300             MOVE WS-IDLEVNR TO WS-IDLEVNR-NUM                            
051400             INSPECT WS-IDLEVNR-NUM REPLACING                             
051500                                          ALL SPACE BY ZERO               
051600             IF WS-IDLEVNR-NUM NUMERIC                                    
051700               IF LEV-IDLEVNR-MOTSV NOT = SPACE                           
051800                 MOVE NEJ    TO INDATA-SW                                 
051900                 IF ENGLISH-TEXT                                          
052000                    MOVE FEL17      TO MOD-TEMFSFEL                       
052100                 ELSE                                                     
052200                    MOVE FEL7       TO MOD-TEMFSFEL                       
052300                 END-IF                                                   
052400                 PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                      
052500               END-IF                                                     
052600             END-IF                                                       
052700           END-IF                                                         
052800           IF MFS-UPD-V                                                   
052900             PERFORM CA-KOLLA-ADR                                         
053000             PERFORM CE-KOLLA-EJ-UPDATE                                   
053100             IF INDATA-FEL                                                
053200               IF ENGLISH-TEXT                                            
053300                  MOVE FEL15        TO MOD-TEMFSFEL                       
053400               ELSE                                                       
053500                  MOVE FEL5         TO MOD-TEMFSFEL                       
053600               END-IF                                                     
053700               PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                        
053800             END-IF                                                       
053900           ELSE                                                           
054000             IF MFS-UPDATE                                                
054100               PERFORM CB-KOLLA-LEV                                       
054200               PERFORM CC-KOLLA-ATT                                       
054300               PERFORM CF-KOLLA-EJ-UPD-V                                  
054400               IF INDATA-FEL                                              
054500                 IF MED-IDMFSFEL = SPACE                                  
054600                   IF ENGLISH-TEXT                                        
054700                      MOVE FEL15      TO MOD-TEMFSFEL                     
054800                   ELSE                                                   
054900                      MOVE FEL5       TO MOD-TEMFSFEL                     
055000                   END-IF                                                 
055100                 END-IF                                                   
055200                 PERFORM MFS-ROER-EJ-FAELT-UT-ATTENT                      
055300               END-IF                                                     
055400             END-IF                                                       
055500           END-IF                                                         
055600           PERFORM MFS-ROER-EJ-BILD                                       
055700         END-IF                                                           
055800       END-IF                                                             
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 CA-KOLLA-ADR SECTION.                                                    
056300     MOVE 'CA-KOLLA-ADR '  TO CURRENT-SECTION                             
056400                                                                          
056500     IF MID-BELEV-VCC NOT = ALL '+'                                       
056600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-VCC-ATTR                    
056700     END-IF                                                               
056800                                                                          
056900     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
057000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD1-ATTR                   
057100     END-IF                                                               
057200                                                                          
057300     IF MID-ADLEV-RAD2-VCC NOT = ALL '+'                                  
057400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD2-VCC-ATTR               
057500     END-IF                                                               
057600                                                                          
057700     IF MID-ADLEV-ORT-VCC NOT = ALL '+'                                   
057800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-ORT-VCC-ATTR                
057900     END-IF                                                               
058000                                                                          
058100     IF MID-ADLEVLND NOT = ALL '+'                                        
058200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEVLND-ATTR                     
058300     END-IF                                                               
058400                                                                          
058500     IF MID-IDLANDX2 NOT = ALL '+'                                        
058600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-ATTR                     
058700     END-IF                                                               
058800                                                                          
058900     IF MID-IDLEVTLF NOT = ALL '+'                                        
059000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVTLF-ATTR                     
059100     END-IF                                                               
059200                                                                          
059300     IF MID-IDLEVFAX NOT = ALL '+'                                        
059400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVFAX-ATTR                     
059500     END-IF                                                               
059600     .                                                                    
059700     EJECT                                                                
059800                                                                          
059900 CB-KOLLA-LEV SECTION.                                                    
060000     MOVE 'CB-KOLLA-LEV '  TO CURRENT-SECTION                             
060100                                                                          
060200     IF MID-KVVECKOR-LT NOT = ALL '+'                                     
060300       INSPECT MID-KVVECKOR-LT REPLACING LEADING SPACE BY ZERO            
060400       IF MID-KVVECKOR-LT NUMERIC                                         
060500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVVECKOR-LT-IN-ATTR             
060600       ELSE                                                               
060700         MOVE NEJ TO INDATA-SW                                            
060800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVVECKOR-LT-IN-ATTR             
060900       END-IF                                                             
061000     END-IF                                                               
061100                                                                          
061200     IF MID-KVVECKOR-AT NOT = ALL '+'                                     
061300       INSPECT MID-KVVECKOR-AT REPLACING LEADING SPACE BY ZERO            
061400       IF MID-KVVECKOR-AT NUMERIC                                         
061500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVVECKOR-AT-IN-ATTR             
061600       ELSE                                                               
061700         MOVE NEJ TO INDATA-SW                                            
061800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVVECKOR-AT-IN-ATTR             
061900       END-IF                                                             
062000     END-IF                                                               
062100                                                                          
062200     IF MID-KDLEVTYP NOT = ALL '+'                                        
062300       IF MID-KDLEVTYP NUMERIC                                            
062400       AND (MID-KDLEVTYP = ZERO OR 3)                                     
062500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDLEVTYP-IN-ATTR                
062600       ELSE                                                               
062700         MOVE NEJ TO INDATA-SW                                            
062800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDLEVTYP-IN-ATTR                
062900       END-IF                                                             
063000     END-IF                                                               
063100                                                                          
063200     IF MID-FLRSADR NOT = ALL '+'                                         
063300       IF (MID-FLRSADR = 'N' OR 'J' OR 'Y')                               
063400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSADR-IN-ATTR                 
063500         IF MID-FLRSADR = 'Y'                                             
063600           MOVE 'J' TO MID-FLRSADR                                        
063700         END-IF                                                           
063800       ELSE                                                               
063900         MOVE NEJ TO INDATA-SW                                            
064000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLRSADR-IN-ATTR                 
064100       END-IF                                                             
064200     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500 CC-KOLLA-ATT SECTION.                                                    
064600     MOVE 'CC-KOLLA-ATT '  TO CURRENT-SECTION                             
064700                                                                          
064800     MOVE MID-IDATTENT-ENTER TO W-IDATTENT                                
064900     MOVE NEJ                TO ATTRAD-IFYLLT-SW                          
065000                                                                          
065100     IF MID-IDATTENT-IN NOT = ALL '+'                                     
065200       INSPECT MID-IDATTENT-IN REPLACING LEADING SPACE BY ZERO            
065300                                                                          
065400       IF MID-BELEV-UPD   NOT = ALL '+'                                   
065500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-UPD-ATTR                  
065600         MOVE JA             TO ATTRAD-IFYLLT-SW                          
065700       END-IF                                                             
065800                                                                          
065900       IF MID-IDMAIL-UPD   NOT = ALL '+'                                  
065901         MOVE JA                         TO ATTRAD-IFYLLT-SW              
065910         MOVE MID-IDMAIL-UPD             TO MCHK-IDMAIL                   
065920         CALL W005MCHK USING MCHK-W005MCHK                                
065930         IF MCHK-KDFEL = +1                                               
065931            MOVE MFS-NUM-FAELT-FEL       TO MOD-IDMAIL-UPD-ATTR           
065933            MOVE NEJ                     TO INDATA-SW                     
065934                                                                          
065935            MOVE ERR-INVALID-VALUE       TO MED-IDMFSFEL                  
065946            CALL WMEDKONV USING MED-WMEDAREA                              
065947            MOVE MED-MFSFEL              TO WS-MFSFEL-492                 
065948            MOVE ':'                     TO WS-KOLUM-492                  
065949            IF MCHK-BETEXT = SPACE                                        
065950              MOVE 'SPACE'               TO WS-IDTECKEN-492               
065951            ELSE                                                          
065953              MOVE MCHK-BETEXT           TO WS-IDTECKEN-492               
065954            END-IF                                                        
065955            MOVE WS-MFSFEL-492           TO MOD-TEMFSFEL                  
065957         ELSE                                                             
065960            IF MCHK-KDFEL = +2                                            
065961               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDMAIL-UPD-ATTR           
065962               MOVE NEJ                  TO INDATA-SW                     
065963                                                                          
065964               MOVE ERR-ITEMS-MISSING    TO MED-IDMFSFEL                  
065967               CALL WMEDKONV USING MED-WMEDAREA                           
065969               MOVE MED-MFSFEL           TO WS-MFSFEL-029                 
065970               MOVE ':'                  TO WS-KOLUM-029                  
065971               MOVE MCHK-BETEXT          TO WS-IDTECKEN-029               
065972               MOVE WS-MFSFEL-029        TO MOD-TEMFSFEL                  
065980            ELSE                                                          
066000               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL-UPD-ATTR           
066100            END-IF                                                        
066110         END-IF                                                           
066200       END-IF                                                             
066300                                                                          
066400       IF MID-IDLEVTLF-KLEV-UPD   NOT = ALL '+'                           
066500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVTLF-KLEV-UPD-ATTR          
066600         MOVE JA             TO ATTRAD-IFYLLT-SW                          
066700       END-IF                                                             
066800                                                                          
066900       IF MID-TENOTE-UPD   NOT = ALL '+'                                  
067000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-UPD-ATTR                 
067100         MOVE JA             TO ATTRAD-IFYLLT-SW                          
067200       END-IF                                                             
067300                                                                          
067400       IF MID-FLAGGA-BORT NOT = ALL '+' AND                               
067500          ATTRAD-IFYLLT-SW = JA                                           
067600                                                                          
067700          MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLAGGA-BORT-ATTR                
067800          MOVE NEJ TO INDATA-SW                                           
067900                                                                          
068000          MOVE ERR-KONFLIKT        TO MED-IDMFSFEL                        
068100          CALL WMEDKONV USING MED-WMEDAREA                                
068200          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
068300       END-IF                                                             
068400                                                                          
068500       IF MID-IDATTENT-UPD NOT = ALL '+'                                  
068600         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDATTENT-UPD-ATTR                 
068700         MOVE NEJ TO INDATA-SW                                            
068800                                                                          
068900         MOVE ERR-KONFLIKT        TO MED-IDMFSFEL                         
069000         CALL WMEDKONV USING MED-WMEDAREA                                 
069100         MOVE MED-MFSFEL          TO MOD-TEMFSFEL                         
069200       END-IF                                                             
069300                                                                          
069400       IF MID-IDATTENT-IN NUMERIC                                         
069500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDATTENT-IN-ATTR                 
069600                                                                          
069700         IF MID-FLAGGA-BORT NOT = ALL '+'                                 
069800           IF MID-FLAGGA-BORT = SPACE                                     
069900             CONTINUE                                                     
070000           ELSE                                                           
070100             IF MID-FLAGGA-BORT = 'J' OR 'Y'                              
070200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-BORT-ATTR            
070300               MOVE MID-IDATTENT-IN TO W-IDATTENT                         
070400               PERFORM IMS-GHNP-WDF107-UNIK                               
070500               IF SEGMENT-FINNS                                           
070600                 CONTINUE                                                 
070700               ELSE                                                       
070800                 MOVE MID-IDATTENT-ENTER TO W-IDATTENT                    
070900                 MOVE MFS-NUM-FAELT-FEL TO                                
071000                                        MOD-IDATTENT-IN-ATTR              
071100                 MOVE NEJ TO INDATA-SW                                    
071200               END-IF                                                     
071300             ELSE                                                         
071400               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-BORT-ATTR            
071500               MOVE NEJ TO INDATA-SW                                      
071600             END-IF                                                       
071700           END-IF                                                         
071800         ELSE                                                             
071900           IF ATTRAD-IFYLLT-SW = NEJ                                      
072000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDATTENT-IN-ATTR               
072100             MOVE NEJ TO INDATA-SW                                        
072200                                                                          
072300             MOVE ERR-INFO-MISSING  TO MED-IDMFSFEL                       
072400             CALL WMEDKONV USING MED-WMEDAREA                             
072500             MOVE MED-MFSFEL        TO MOD-TEMFSFEL                       
072600           END-IF                                                         
072700         END-IF                                                           
072800       ELSE                                                               
072900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDATTENT-IN-ATTR                   
073000         MOVE NEJ TO INDATA-SW                                            
073100       END-IF                                                             
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
073500 CE-KOLLA-EJ-UPDATE SECTION.                                              
073600     MOVE 'CE-KOLLA-EJ-UPDATE '  TO CURRENT-SECTION                       
073700                                                                          
073800     IF (MID-IDATTENT-IN = ALL '+') AND                                   
073900        (MID-BELEV-UPD   = ALL '+') AND                                   
074000        (MID-IDMAIL-UPD  = ALL '+') AND                                   
074100        (MID-IDLEVTLF-KLEV-UPD = ALL '+') AND                             
074200        (MID-TENOTE-UPD  = ALL '+') AND                                   
074300        (MID-FLAGGA-BORT = ALL '+') AND                                   
074400        (MID-IDATTENT-UPD = ALL '+') AND                                  
074500        (MID-KVVECKOR-LT = ALL '+') AND                                   
074600        (MID-KVVECKOR-AT = ALL '+') AND                                   
074700        (MID-KDLEVTYP    = ALL '+') AND                                   
074800        (MID-FLRSADR     = ALL '+')                                       
074900        CONTINUE                                                          
075000     ELSE                                                                 
075100       MOVE NEJ    TO INDATA-SW                                           
075200*                                                                         
075300         IF MID-IDATTENT-IN NOT = ALL '+'                                 
075400           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDATTENT-IN-ATTR             
075500           MOVE MFS-ROER-EJ-FAELT     TO MID-IDATTENT-IN                  
075600         ELSE                                                             
075700           MOVE MFS-ROER-EJ-FAELT TO MID-IDATTENT-IN                      
075800         END-IF                                                           
075900*                                                                         
076000         IF MID-BELEV-UPD  NOT = ALL '+'                                  
076100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-BELEV-UPD-ATTR               
076200           MOVE MFS-ROER-EJ-FAELT     TO MID-BELEV-UPD                    
076300         ELSE                                                             
076400           MOVE MFS-ROER-EJ-FAELT TO MID-BELEV-UPD                        
076500         END-IF                                                           
076600*                                                                         
076700         IF MID-IDMAIL-UPD  NOT = ALL '+'                                 
076800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDMAIL-UPD-ATTR              
076900           MOVE MFS-ROER-EJ-FAELT     TO MID-IDMAIL-UPD                   
077000         ELSE                                                             
077100           MOVE MFS-ROER-EJ-FAELT TO MID-IDMAIL-UPD                       
077200         END-IF                                                           
077300*                                                                         
077400         IF MID-IDLEVTLF-KLEV-UPD  NOT = ALL '+'                          
077500           MOVE MFS-ALFA-FAELT-FEL    TO                                  
077600                                      MOD-IDLEVTLF-KLEV-UPD-ATTR          
077700           MOVE MFS-ROER-EJ-FAELT     TO MID-IDLEVTLF-KLEV-UPD            
077800         ELSE                                                             
077900           MOVE MFS-ROER-EJ-FAELT TO MID-IDLEVTLF-KLEV-UPD                
078000         END-IF                                                           
078100*                                                                         
078200         IF MID-TENOTE-UPD  NOT = ALL '+'                                 
078300           MOVE MFS-ALFA-FAELT-FEL    TO MOD-TENOTE-UPD-ATTR              
078400           MOVE MFS-ROER-EJ-FAELT     TO MID-TENOTE-UPD                   
078500         ELSE                                                             
078600           MOVE MFS-ROER-EJ-FAELT TO MID-TENOTE-UPD                       
078700         END-IF                                                           
078800*                                                                         
078900         IF MID-FLAGGA-BORT NOT = ALL '+'                                 
079000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLAGGA-BORT-ATTR             
079100           MOVE MFS-ROER-EJ-FAELT     TO MID-FLAGGA-BORT                  
079200         ELSE                                                             
079300           MOVE MFS-ROER-EJ-FAELT TO MID-FLAGGA-BORT                      
079400         END-IF                                                           
079500*                                                                         
079600         IF MID-IDATTENT-UPD NOT = ALL '+'                                
079700           MOVE MFS-NUM-FAELT-FEL    TO                                   
079800                                     MOD-IDATTENT-UPD-ATTR                
079900           MOVE MFS-ROER-EJ-FAELT    TO                                   
080000                                     MID-IDATTENT-UPD                     
080100         ELSE                                                             
080200           MOVE MFS-ROER-EJ-FAELT TO MID-IDATTENT-UPD                     
080300         END-IF                                                           
080400*                                                                         
080500         IF MID-KVVECKOR-LT NOT = ALL '+'                                 
080600           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVVECKOR-LT-IN-ATTR          
080700         END-IF                                                           
080800*                                                                         
080900         IF MID-KVVECKOR-AT NOT = ALL '+'                                 
081000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVVECKOR-AT-IN-ATTR          
081100         END-IF                                                           
081200*                                                                         
081300         IF MID-KDLEVTYP NOT = ALL '+'                                    
081400           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDLEVTYP-IN-ATTR             
081500         END-IF                                                           
081600*                                                                         
081700         IF MID-FLRSADR  NOT = ALL '+'                                    
081800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLRSADR-IN-ATTR              
081900         END-IF                                                           
082000                                                                          
082100         MOVE MFS-ROER-EJ-FAELT       TO MID-KVVECKOR-LT                  
082200                                         MID-KVVECKOR-AT                  
082300                                         MID-KDLEVTYP                     
082400                                         MID-FLRSADR                      
082500*                                                                         
082600       IF ENGLISH-TEXT                                                    
082700          MOVE FEL10        TO MOD-TEMFSINF                               
082800       ELSE                                                               
082900          MOVE FEL0         TO MOD-TEMFSINF                               
083000       END-IF                                                             
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400 CF-KOLLA-EJ-UPD-V SECTION.                                               
083500     MOVE 'CF-KOLLA-EJ-UPD-V '  TO CURRENT-SECTION                        
083600                                                                          
083700     IF MID-INPUT-ADR NOT = ALL '+'                                       
083800       MOVE NEJ    TO INDATA-SW                                           
083900*                                                                         
084000         IF MID-BELEV-VCC NOT = ALL '+'                                   
084100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-BELEV-VCC-ATTR               
084200           MOVE MFS-ROER-EJ-FAELT     TO MID-BELEV-VCC                    
084300         ELSE                                                             
084400           MOVE MFS-ROER-EJ-FAELT TO MID-BELEV-VCC                        
084500         END-IF                                                           
084600*                                                                         
084700         IF MID-ADLEV-RAD1 NOT = ALL '+'                                  
084800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADLEV-RAD1-ATTR              
084900           MOVE MFS-ROER-EJ-FAELT     TO MID-ADLEV-RAD1                   
085000         ELSE                                                             
085100           MOVE MFS-ROER-EJ-FAELT TO MID-ADLEV-RAD1                       
085200         END-IF                                                           
085300*                                                                         
085400         IF MID-ADLEV-RAD2-VCC NOT = ALL '+'                              
085500           MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADLEV-RAD2-VCC-ATTR          
085600           MOVE MFS-ROER-EJ-FAELT     TO MID-ADLEV-RAD2-VCC               
085700         ELSE                                                             
085800           MOVE MFS-ROER-EJ-FAELT TO MID-ADLEV-RAD2-VCC                   
085900         END-IF                                                           
086000*                                                                         
086100         IF MID-ADLEV-ORT-VCC NOT = ALL '+'                               
086200           MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADLEV-ORT-VCC-ATTR           
086300           MOVE MFS-ROER-EJ-FAELT     TO MID-ADLEV-ORT-VCC                
086400         ELSE                                                             
086500           MOVE MFS-ROER-EJ-FAELT TO MID-ADLEV-ORT-VCC                    
086600         END-IF                                                           
086700*                                                                         
086800         IF MID-ADLEVLND NOT = ALL '+'                                    
086900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-ADLEVLND-ATTR                
087000           MOVE MFS-ROER-EJ-FAELT     TO MID-ADLEVLND                     
087100         ELSE                                                             
087200           MOVE MFS-ROER-EJ-FAELT TO MID-ADLEVLND                         
087300         END-IF                                                           
087400*                                                                         
087500         IF MID-IDLANDX2 NOT = ALL '+'                                    
087600           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLANDX2-ATTR                
087700           MOVE MFS-ROER-EJ-FAELT     TO MID-IDLANDX2                     
087800         ELSE                                                             
087900           MOVE MFS-ROER-EJ-FAELT TO MID-IDLANDX2                         
088000         END-IF                                                           
088100*                                                                         
088200         IF MID-IDLEVTLF NOT = ALL '+'                                    
088300           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVTLF-ATTR                
088400           MOVE MFS-ROER-EJ-FAELT     TO MID-IDLEVTLF                     
088500         ELSE                                                             
088600           MOVE MFS-ROER-EJ-FAELT TO MID-IDLEVTLF                         
088700         END-IF                                                           
088800*                                                                         
088900         IF MID-IDLEVFAX NOT = ALL '+'                                    
089000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVFAX-ATTR                
089100           MOVE MFS-ROER-EJ-FAELT     TO MID-IDLEVFAX                     
089200         ELSE                                                             
089300           MOVE MFS-ROER-EJ-FAELT TO MID-IDLEVFAX                         
089400         END-IF                                                           
089500*                                                                         
089600         IF ENGLISH-TEXT                                                  
089700            MOVE FEL16          TO MOD-TEMFSINF                           
089800         ELSE                                                             
089900            MOVE FEL6           TO MOD-TEMFSINF                           
090000         END-IF                                                           
090100     ELSE                                                                 
090200       CONTINUE                                                           
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700 D-UPPDATERA SECTION.                                                     
090800     MOVE 'D-UPPDATERA '  TO CURRENT-SECTION                              
090900                                                                          
091000     PERFORM IMS-GHU-WDF101                                               
091100     IF SEGMENT-FINNS                                                     
091200       IF MID-INPUT NOT = ALL '+'                                         
091300         PERFORM DA-UPPDATERA-LEV                                         
091400         PERFORM DB-UPPDATERA-ADR                                         
091500         PERFORM DC-UPPDATERA-ATT                                         
091600         IF ENGLISH-TEXT                                                  
091700           MOVE MED13 TO MOD-TEMFSINF                                     
091800         ELSE                                                             
091900           MOVE MED3 TO MOD-TEMFSINF                                      
092000         END-IF                                                           
092100       END-IF                                                             
092200       PERFORM MFS-FORM-ATTR                                              
092300     END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
092600 DA-UPPDATERA-LEV SECTION.                                                
092700     MOVE 'DA-UPPDATERA-LEV '  TO CURRENT-SECTION                         
092800                                                                          
092900     IF (MID-INPUT-ADR   NOT = ALL '+') OR                                
093000        (MID-KVVECKOR-LT NOT = ALL '+') OR                                
093100        (MID-KVVECKOR-AT NOT = ALL '+') OR                                
093200        (MID-KDLEVTYP    NOT = ALL '+') OR                                
093300        (MID-FLRSADR     NOT = ALL '+')                                   
093400                                                                          
093500       IF MID-INPUT-ADR NOT = ALL '+'                                     
093600         MOVE 'J' TO LEV-FLRSADR                                          
093700       END-IF                                                             
093800                                                                          
093900       IF MID-KVVECKOR-LT NOT = ALL '+'                                   
094000         INSPECT MID-KVVECKOR-LT REPLACING LEADING                        
094100                                 SPACE BY ZERO                            
094200         MOVE MID-KVVECKOR-LT   TO LEV-KVVECKOR-LT                        
094300                                   MOD-KVVECKOR-LT                        
094400         MOVE MFS-ADD-LYS-UPP-FAELT                                       
094500                                TO MOD-KVVECKOR-LT-IN-ATTR                
094600       ELSE                                                               
094700         MOVE MFS-ROER-EJ-FAELT TO MOD-KVVECKOR-LT                        
094800       END-IF                                                             
094900                                                                          
095000       IF MID-KVVECKOR-AT NOT = ALL '+'                                   
095100         INSPECT MID-KVVECKOR-AT REPLACING LEADING                        
095200                                 SPACE BY ZERO                            
095300         MOVE MID-KVVECKOR-AT   TO LEV-KVVECKOR-AT                        
095400                                   MOD-KVVECKOR-AT                        
095500         MOVE MFS-ADD-LYS-UPP-FAELT                                       
095600                                TO MOD-KVVECKOR-AT-IN-ATTR                
095700       ELSE                                                               
095800         MOVE MFS-ROER-EJ-FAELT TO MOD-KVVECKOR-AT                        
095900       END-IF                                                             
096000                                                                          
096100       IF MID-KDLEVTYP NOT = ALL '+'                                      
096200         MOVE MID-KDLEVTYP      TO LEV-KDLEVTYP                           
096300                                   MOD-KDLEVTYP                           
096400         MOVE MFS-ADD-LYS-UPP-FAELT                                       
096500                                TO MOD-KDLEVTYP-IN-ATTR                   
096600       ELSE                                                               
096700         MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVTYP                           
096800       END-IF                                                             
096900                                                                          
097000       IF MID-FLRSADR NOT = ALL '+'                                       
097100         MOVE MID-FLRSADR       TO LEV-FLRSADR                            
097200         IF MID-FLRSADR = 'Y'                                             
097300           MOVE 'J'             TO LEV-FLRSADR                            
097400         END-IF                                                           
097500                                                                          
097600         IF ENGLISH-TEXT AND MID-FLRSADR = 'J'                            
097700           MOVE 'Y'             TO MOD-FLRSADR                            
097800         ELSE                                                             
097900           MOVE MID-FLRSADR     TO MOD-FLRSADR                            
098000         END-IF                                                           
098100         MOVE MFS-ADD-LYS-UPP-FAELT                                       
098200                                TO MOD-FLRSADR-IN-ATTR                    
098300       ELSE                                                               
098400         MOVE MFS-ROER-EJ-FAELT TO MOD-FLRSADR                            
098500       END-IF                                                             
098600                                                                          
098700       IF MID-KVVECKOR-LT = ALL '+'                                       
098800      AND MID-KVVECKOR-AT = ALL '+'                                       
098900          CONTINUE                                                        
099000       ELSE                                                               
099100          PERFORM DBA-START-W224S2                                        
099200       END-IF                                                             
099300                                                                          
099400       PERFORM IMS-REPL-WDF101                                            
099500     END-IF                                                               
099600     .                                                                    
099700     EJECT                                                                
099800 DBA-START-W224S2 SECTION.                                                
099900     MOVE 'DBA-START-W224S2 '  TO CURRENT-SECTION                         
100000                                                                          
100100     MOVE '2115'            TO MSGSOP-IDTRANS                             
100200     MOVE MFS-KDMFSFOR      TO MSGSOP-KDMFSFOR                            
100300     MOVE 'W224S2    '      TO MSGSOP-IDPROCESS                           
100400     MOVE 'O'               TO MSGSOP-KDSOPFUNK                           
100500     MOVE LEV-IDLEVNR       TO PARM-W224S2-IDLEVNR                        
100600     MOVE '++'              TO PARM-W224S2-IDDC                           
100700     MOVE MID-KVVECKOR-AT   TO PARM-W224S2-KVVECKOR-AT                    
100800     MOVE MID-KVVECKOR-LT   TO PARM-W224S2-KVVECKOR-LT                    
100900     MOVE '++'              TO PARM-W224S2-KVDAGAR-TT                     
101000     MOVE PARM-W224S2       TO MSGSOP-TESYMBV                             
101100     MOVE 'UPDATE STARTED'  TO MOD-TEMFSINF                               
101200     PERFORM IMS-INSERT-ALT-MSG                                           
101300     .                                                                    
101400     EJECT                                                                
101500 DB-UPPDATERA-ADR SECTION.                                                
101600     MOVE 'DB-UPPDATERA-ADR  '  TO CURRENT-SECTION                        
101700                                                                          
101800     IF MID-INPUT-ADR NOT = ALL '+'                                       
101900       PERFORM IMS-GHNP-WDF106                                            
102000                                                                          
102100       IF SEGMENT-FINNS                                                   
102200         IF MID-BELEV-VCC NOT = ALL '+'                                   
102300           MOVE MID-BELEV-VCC         TO ADR-BELEV-VCC                    
102400                                         MOD-BELEV-VCC                    
102500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELEV-VCC-ATTR               
102600         ELSE                                                             
102700           MOVE MFS-ROER-EJ-FAELT TO MOD-BELEV-VCC                        
102800         END-IF                                                           
102900                                                                          
103000         IF MID-ADLEV-RAD1 NOT = ALL '+'                                  
103100           MOVE MID-ADLEV-RAD1        TO ADR-ADLEV-RAD1                   
103200                                         MOD-ADLEV-RAD1                   
103300           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
103400                                     MOD-ADLEV-RAD1-ATTR                  
103500         ELSE                                                             
103600           MOVE MFS-ROER-EJ-FAELT TO MOD-ADLEV-RAD1                       
103700         END-IF                                                           
103800                                                                          
103900         IF MID-ADLEV-RAD2-VCC NOT = ALL '+'                              
104000           MOVE MID-ADLEV-RAD2-VCC    TO ADR-ADLEV-RAD2-VCC               
104100                                         MOD-ADLEV-RAD2-VCC               
104200           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
104300                                     MOD-ADLEV-RAD2-VCC-ATTR              
104400         ELSE                                                             
104500           MOVE MFS-ROER-EJ-FAELT TO MOD-ADLEV-RAD2-VCC                   
104600         END-IF                                                           
104700                                                                          
104800         IF MID-ADLEV-ORT-VCC NOT = ALL '+'                               
104900           MOVE MID-ADLEV-ORT-VCC    TO ADR-ADLEV-ORT-VCC                 
105000                                         MOD-ADLEV-ORT-VCC                
105100           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
105200                                     MOD-ADLEV-ORT-VCC-ATTR               
105300         ELSE                                                             
105400           MOVE MFS-ROER-EJ-FAELT TO MOD-ADLEV-ORT-VCC                    
105500         END-IF                                                           
105600                                                                          
105700         IF MID-ADLEVLND NOT = ALL '+'                                    
105800           MOVE MID-ADLEVLND          TO ADR-ADLEVLND                     
105900                                         MOD-ADLEVLND                     
106000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADLEVLND-ATTR                
106100         ELSE                                                             
106200           MOVE MFS-ROER-EJ-FAELT TO MOD-ADLEVLND                         
106300         END-IF                                                           
106400                                                                          
106500         IF MID-IDLANDX2 NOT = ALL '+'                                    
106600           MOVE MID-IDLANDX2          TO ADR-IDLANDX2                     
106700                                         MOD-IDLANDX2                     
106800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLANDX2-ATTR                
106900         ELSE                                                             
107000           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLANDX2                         
107100         END-IF                                                           
107200                                                                          
107300         IF MID-IDLEVTLF NOT = ALL '+'                                    
107400           MOVE MID-IDLEVTLF          TO ADR-IDLEVTLF                     
107500                                         MOD-IDLEVTLF                     
107600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVTLF-ATTR                
107700         ELSE                                                             
107800           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVTLF                         
107900         END-IF                                                           
108000                                                                          
108100         IF MID-IDLEVFAX NOT = ALL '+'                                    
108200           MOVE MID-IDLEVFAX          TO ADR-IDLEVFAX                     
108300                                         MOD-IDLEVFAX                     
108400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVFAX-ATTR                
108500         ELSE                                                             
108600           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVFAX                         
108700         END-IF                                                           
108710                                                                          
108720         MOVE SPACES                  TO ADR-IDVAT                        
108800                                                                          
108900         PERFORM IMS-REPL-WDF106                                          
109000                                                                          
109100       END-IF                                                             
109200     ELSE                                                                 
109300       PERFORM MFS-ROER-EJ-FAELT-ADR                                      
109400     END-IF                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 DC-UPPDATERA-ATT SECTION.                                                
109800     MOVE 'DC-UPPDATERA-ATT '  TO CURRENT-SECTION                         
109900*--- NYA ATT. VÄRDEN SKALL VISAS FÖRST PÅ BILDEN.                         
110000                                                                          
110100     IF MID-IDATTENT-IN NOT = ALL '+'                                     
110200                                                                          
110300       MOVE MID-IDATTENT-IN TO W-IDATTENT                                 
110400       PERFORM IMS-GHNP-WDF107-UNIK                                       
110500                                                                          
110600       IF (MID-FLAGGA-BORT NOT = ALL '+') AND                             
110700             (MID-FLAGGA-BORT NOT = SPACE)                                
110800                                                                          
110900         IF SEGMENT-FINNS                                                 
111000           PERFORM IMS-GNP-WDF122                                         
111100           IF SEGMENT-FINNS                                               
111200             PERFORM IMS-GHU-WDF107                                       
111300             MOVE SPACE      TO ATT-BELEV                                 
111400                                ATT-IDLEVTLF-KLEV                         
111500                                ATT-IDMAIL                                
111600                                ATT-TENOTE                                
111700                                                                          
111800             PERFORM IMS-REPL-WDF107                                      
111900           ELSE                                                           
112000             PERFORM IMS-GHU-WDF107                                       
112100             PERFORM IMS-DLET-WDF107                                      
112200           END-IF                                                         
112300         END-IF                                                           
112400         MOVE +1 TO W-IDATTENT                                            
112500       ELSE                                                               
112600         IF MID-BELEV-UPD   NOT = ALL '+'                                 
112700           MOVE MID-BELEV-UPD   TO ATT-BELEV                              
112800         END-IF                                                           
112900                                                                          
113000         IF MID-IDMAIL-UPD   NOT = ALL '+'                                
113100           MOVE MID-IDMAIL-UPD  TO ATT-IDMAIL                             
113200         END-IF                                                           
113300                                                                          
113400         IF MID-IDLEVTLF-KLEV-UPD  NOT = ALL '+'                          
113500           MOVE MID-IDLEVTLF-KLEV-UPD  TO ATT-IDLEVTLF-KLEV               
113600         END-IF                                                           
113700                                                                          
113800         IF MID-TENOTE-UPD   NOT = ALL '+'                                
113900           MOVE SPACE           TO ATT-TENOTE                             
114000           MOVE MID-TENOTE-UPD  TO ATT-TENOTE                             
114100         END-IF                                                           
114200                                                                          
114300         IF SEGMENT-FINNS                                                 
114400           PERFORM IMS-REPL-WDF107                                        
114500         ELSE                                                             
114600           IF MID-BELEV-UPD = ALL '+'                                     
114700             MOVE SPACE           TO ATT-BELEV                            
114800           END-IF                                                         
114900                                                                          
115000           IF MID-IDMAIL-UPD = ALL '+'                                    
115100             MOVE SPACE           TO ATT-IDMAIL                           
115200           END-IF                                                         
115300                                                                          
115400           IF MID-IDLEVTLF-KLEV-UPD = ALL '+'                             
115500             MOVE SPACE           TO ATT-IDLEVTLF-KLEV                    
115600           END-IF                                                         
115700                                                                          
115800           IF MID-TENOTE-UPD = ALL '+'                                    
115900             MOVE SPACE           TO ATT-TENOTE                           
116000           END-IF                                                         
116100                                                                          
116200           MOVE MID-IDATTENT-IN   TO ATT-IDATTENT                         
116300           PERFORM IMS-ISRT-WDF107                                        
116400         END-IF                                                           
116500       END-IF                                                             
116600     END-IF                                                               
116700     .                                                                    
116800     EJECT                                                                
116900                                                                          
117000 E-BEHANDLA-LEVERANTOER SECTION.                                          
117100     MOVE 'E-BEHANDLA-LEVERANTOER '  TO CURRENT-SECTION                   
117200                                                                          
117300     PERFORM IMS-GHNP-WDF106                                              
117400     IF SEGMENT-FINNS                                                     
117500       MOVE ADR-BELEV-VCC           TO MOD-BELEV-VCC                      
117600       MOVE ADR-ADLEV-RAD1          TO MOD-ADLEV-RAD1                     
117700       MOVE ADR-ADLEV-RAD2-VCC      TO MOD-ADLEV-RAD2-VCC                 
117800       MOVE ADR-ADLEV-ORT-VCC       TO MOD-ADLEV-ORT-VCC                  
117900       MOVE ADR-ADLEVLND            TO MOD-ADLEVLND                       
118000       MOVE ADR-IDLANDX2            TO MOD-IDLANDX2                       
118100       MOVE ADR-IDLEVTLF            TO MOD-IDLEVTLF                       
118200       MOVE ADR-IDLEVFAX            TO MOD-IDLEVFAX                       
118300                                                                          
118400       PERFORM IMS-GHU-WDF101                                             
118500       IF SEGMENT-FINNS                                                   
118600         MOVE LEV-IDLEVNR-MOTSV     TO MOD-IDLEVNR-MOTSV                  
118700         MOVE LEV-KVVECKOR-LT       TO MOD-KVVECKOR-LT                    
118800         MOVE LEV-KVVECKOR-AT       TO MOD-KVVECKOR-AT                    
118900         MOVE LEV-KDLEVTYP          TO MOD-KDLEVTYP                       
119000         IF ENGLISH-TEXT AND LEV-FLRSADR = 'J'                            
119100           MOVE 'Y'                 TO MOD-FLRSADR                        
119200         ELSE                                                             
119300           MOVE LEV-FLRSADR         TO MOD-FLRSADR                        
119400         END-IF                                                           
119500       ELSE                                                               
119600         MOVE MFS-RENSA-FAELT       TO MOD-KVVECKOR-LT                    
119700                                       MOD-KVVECKOR-AT                    
119800                                       MOD-IDLEVNR-MOTSV                  
119900                                       MOD-KDLEVTYP                       
120000                                       MOD-FLRSADR                        
120100                                       MOD-KVVECKOR-LT-IN                 
120200                                       MOD-KVVECKOR-AT-IN                 
120300                                       MOD-KDLEVTYP-IN                    
120400                                       MOD-FLRSADR-IN                     
120500       END-IF                                                             
120600     ELSE                                                                 
120700       MOVE SPACE                   TO MOD-BELEV-VCC                      
120800                                       MOD-ADLEV-RAD1                     
120900                                       MOD-ADLEV-RAD2-VCC                 
121000                                       MOD-ADLEV-ORT-VCC                  
121100                                       MOD-ADLEVLND                       
121200                                       MOD-IDLANDX2                       
121300                                       MOD-IDLEVTLF                       
121400                                       MOD-IDLEVFAX                       
121500                                       MOD-IDLEVNR-MOTSV                  
121600                                       MOD-FLRSADR                        
121700                                                                          
121800       MOVE ZERO                    TO MOD-KVVECKOR-LT                    
121900                                       MOD-KVVECKOR-AT                    
122000                                       MOD-KDLEVTYP                       
122100     END-IF                                                               
122200     IF ENGLISH-TEXT                                                      
122300        MOVE FEL13          TO MOD-TEMFSFEL                               
122400     ELSE                                                                 
122500        MOVE FEL3           TO MOD-TEMFSFEL                               
122600     END-IF                                                               
122700                                                                          
122800     MOVE ZERO TO W-IDATTENT                                              
122900     .                                                                    
123000     EJECT                                                                
123100 F-NAESTA-SIDA SECTION.                                                   
123200     MOVE 'F-NAESTA-SIDA '  TO CURRENT-SECTION                            
123300                                                                          
123400     PERFORM MFS-ROER-EJ-BILD                                             
123500     MOVE MID-IDATTENT-NEXT TO W-IDATTENT                                 
123600     .                                                                    
123700     EJECT                                                                
123800 G-SAMMA-SIDA SECTION.                                                    
123900     MOVE 'G-SAMMA-SIDA '  TO CURRENT-SECTION                             
124000                                                                          
124100     MOVE MID-IDATTENT-ENTER TO W-IDATTENT                                
124200                                                                          
124300***** OM IDATTENT-IN ÄR BLANK BEHANDLAS DEN SOM EJ IFYLLD ***             
124400     IF MID-IDATTENT-IN = SPACE                                           
124500       MOVE '++' TO MID-IDATTENT-IN                                       
124600     END-IF                                                               
124700                                                                          
124800     IF (MID-INPUT NOT = ALL '+')                                         
124900       MOVE NEJ TO INDATA-SW                                              
125000       IF ENGLISH-TEXT                                                    
125100          MOVE FEL14          TO MOD-TEMFSFEL                             
125200       ELSE                                                               
125300          MOVE FEL4           TO MOD-TEMFSFEL                             
125400       END-IF                                                             
125500       PERFORM MFS-LAS-IN-IGEN                                            
125600       PERFORM MFS-ROER-EJ-BILD                                           
125700     END-IF                                                               
125800                                                                          
125900     PERFORM S01-KOLLA-ATT-UPD                                            
126000                                                                          
126100     IF (MID-FLAGGA-BORT NOT = ALL '+') OR                                
126200        (ATTRAD-IFYLLT-SW = JA)                                           
126300                                                                          
126400       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
126500       CALL WMEDKONV USING MED-WMEDAREA                                   
126600       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
126700                                                                          
126800       MOVE NEJ TO INDATA-SW                                              
126900       PERFORM MFS-LAS-IN-IGEN                                            
127000       PERFORM MFS-ROER-EJ-BILD                                           
127100     END-IF                                                               
127200                                                                          
127300     IF MID-IDATTENT-UPD = ALL '+'                                        
127400       MOVE MFS-RENSA-FAELT  TO MOD-IDATTENT-UPD                          
127500       PERFORM MFS-ROER-EJ-BILD                                           
127600     ELSE                                                                 
127700       IF MID-IDATTENT-UPD NUMERIC                                        
127800         PERFORM GA-MOVE-COPY-ATT-RADER                                   
127900         PERFORM GB-INDATA-TILL-MOD                                       
128000         MOVE MID-IDATTENT-ENTER TO W-IDATTENT                            
128100         PERFORM MFS-ROER-EJ-FAELT-ADR                                    
128200         PERFORM MFS-ROER-EJ-FAELT-LEV                                    
128300       ELSE                                                               
128400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDATTENT-UPD-ATTR                  
128500         MOVE MFS-ROER-EJ-FAELT TO MOD-IDATTENT-UPD                       
128600         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
128700         CALL WMEDKONV USING MED-WMEDAREA                                 
128800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
128900                                                                          
129000         MOVE NEJ TO INDATA-SW                                            
129100         PERFORM GC-INDATA-TILL-MOD                                       
129200         PERFORM MFS-ROER-EJ-FAELT-ADR                                    
129300         PERFORM MFS-ROER-EJ-FAELT-LEV                                    
129400       END-IF                                                             
129500     END-IF                                                               
129600     .                                                                    
129700     EJECT                                                                
129800 GA-MOVE-COPY-ATT-RADER  SECTION.                                         
129900     MOVE 'GA-MOVE-COPY-ATT-RADER '  TO CURRENT-SECTION                   
130000                                                                          
130100     MOVE MID-IDATTENT-UPD TO W-IDATTENT                                  
130200     PERFORM IMS-GHNP-WDF107-UNIK                                         
130300                                                                          
130400     IF SEGMENT-FINNS                                                     
130500       MOVE MFS-RENSA-FAELT    TO MOD-IDATTENT-IN                         
130600       MOVE ATT-BELEV          TO MOD-BELEV-UPD                           
130700       MOVE ATT-IDMAIL         TO MOD-IDMAIL-UPD                          
130800       MOVE ATT-IDLEVTLF-KLEV  TO MOD-IDLEVTLF-KLEV-UPD                   
130900       MOVE ATT-TENOTE(1:35)   TO MOD-TENOTE-UPD                          
131000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-UPD-ATTR                   
131100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL-UPD-ATTR                  
131200       MOVE MFS-ADD-LAES-IN-FAELT TO                                      
131300                                MOD-IDLEVTLF-KLEV-UPD-ATTR                
131400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-UPD-ATTR                  
131500                                                                          
131600       MOVE MFS-RENSA-FAELT    TO MOD-IDATTENT-UPD                        
131700                                                                          
131800       MOVE NEJ TO INDATA-SW                                              
131900     ELSE                                                                 
132000       MOVE MFS-NUM-FAELT-FEL TO MOD-IDATTENT-UPD-ATTR                    
132100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDATTENT-UPD                         
132200       MOVE ERR-MISSING-IN-REGISTER  TO MED-IDMFSFEL                      
132300       CALL WMEDKONV USING MED-WMEDAREA                                   
132400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
132500                                                                          
132600       MOVE NEJ TO INDATA-SW                                              
132700     END-IF                                                               
132800                                                                          
132900     .                                                                    
133000     EJECT                                                                
133100 GB-INDATA-TILL-MOD  SECTION.                                             
133200     MOVE 'GB-INDATA-TILL-MOD '  TO CURRENT-SECTION                       
133300                                                                          
133400     IF MID-BELEV-VCC NOT = ALL '+'                                       
133500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-VCC-ATTR                   
133600     END-IF                                                               
133700                                                                          
133800     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
133900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD1-ATTR                  
134000     END-IF                                                               
134100                                                                          
134200     IF MID-ADLEV-RAD2-VCC NOT = ALL '+'                                  
134300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD2-VCC-ATTR              
134400     END-IF                                                               
134500                                                                          
134600     IF MID-ADLEV-ORT-VCC NOT = ALL '+'                                   
134700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-ORT-VCC-ATTR               
134800     END-IF                                                               
134900                                                                          
135000     IF MID-ADLEVLND NOT = ALL '+'                                        
135100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEVLND-ATTR                    
135200     END-IF                                                               
135300                                                                          
135400     IF MID-IDLANDX2 NOT = ALL '+'                                        
135500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLANDX2-ATTR                    
135600     END-IF                                                               
135700                                                                          
135800     IF MID-IDLEVTLF NOT = ALL '+'                                        
135900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-ATTR                    
136000     END-IF                                                               
136100                                                                          
136200     IF MID-IDLEVFAX NOT = ALL '+'                                        
136300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR                    
136400     END-IF                                                               
136500                                                                          
136600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDATTENT-IN-ATTR                   
136700     MOVE MFS-RENSA-FAELT       TO MOD-IDATTENT-IN                        
136800                                                                          
136900     IF MID-FLAGGA-BORT NOT = ALL '+'                                     
137000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-BORT-ATTR                 
137100       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLAGGA-BORT                      
137200     ELSE                                                                 
137300       MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-BORT                            
137400     END-IF                                                               
137500                                                                          
137600     IF MID-KVVECKOR-LT NOT = ALL '+'                                     
137700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-LT-IN-ATTR              
137800       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVVECKOR-LT-IN                   
137900     ELSE                                                                 
138000       MOVE MFS-RENSA-FAELT TO MOD-KVVECKOR-LT-IN                         
138100     END-IF                                                               
138200                                                                          
138300     IF MID-KVVECKOR-AT NOT = ALL '+'                                     
138400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-AT-IN-ATTR              
138500       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVVECKOR-AT-IN                   
138600     ELSE                                                                 
138700       MOVE MFS-RENSA-FAELT TO MOD-KVVECKOR-AT-IN                         
138800     END-IF                                                               
138900                                                                          
139000     IF MID-KDLEVTYP NOT = ALL '+'                                        
139100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVTYP-IN-ATTR                 
139200       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDLEVTYP-IN                      
139300     ELSE                                                                 
139400       MOVE MFS-RENSA-FAELT TO MOD-KDLEVTYP-IN                            
139500     END-IF                                                               
139600                                                                          
139700     IF MID-FLRSADR NOT = ALL '+'                                         
139800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLRSADR-IN-ATTR                  
139900       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLRSADR-IN                       
140000     ELSE                                                                 
140100       MOVE MFS-RENSA-FAELT TO MOD-FLRSADR-IN                             
140200     END-IF                                                               
140300                                                                          
140400     .                                                                    
140500     EJECT                                                                
140600 GC-INDATA-TILL-MOD  SECTION.                                             
140700     MOVE 'GC-INDATA-TILL-MOD '  TO CURRENT-SECTION                       
140800                                                                          
140900     IF MID-BELEV-VCC NOT = ALL '+'                                       
141000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-VCC-ATTR                   
141100     END-IF                                                               
141200                                                                          
141300     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
141400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD1-ATTR                  
141500     END-IF                                                               
141600                                                                          
141700     IF MID-ADLEV-RAD2-VCC NOT = ALL '+'                                  
141800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD2-VCC-ATTR              
141900     END-IF                                                               
142000                                                                          
142100     IF MID-ADLEV-ORT-VCC NOT = ALL '+'                                   
142200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-ORT-VCC-ATTR               
142300     END-IF                                                               
142400                                                                          
142500     IF MID-ADLEVLND NOT = ALL '+'                                        
142600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEVLND-ATTR                    
142700     END-IF                                                               
142800                                                                          
142900     IF MID-IDLANDX2 NOT = ALL '+'                                        
143000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLANDX2-ATTR                    
143100     END-IF                                                               
143200                                                                          
143300     IF MID-IDLEVTLF NOT = ALL '+'                                        
143400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-ATTR                    
143500     END-IF                                                               
143600                                                                          
143700     IF MID-IDLEVFAX NOT = ALL '+'                                        
143800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR                    
143900     END-IF                                                               
144000                                                                          
144100     IF MID-IDATTENT-IN NOT = ALL '+'                                     
144200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDATTENT-IN-ATTR                 
144300       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDATTENT-IN                      
144400     ELSE                                                                 
144500       MOVE MFS-RENSA-FAELT TO MOD-IDATTENT-IN                            
144600     END-IF                                                               
144700                                                                          
144800     IF MID-FLAGGA-BORT NOT = ALL '+'                                     
144900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-BORT-ATTR                 
145000       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLAGGA-BORT                      
145100     ELSE                                                                 
145200       MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-BORT                            
145300     END-IF                                                               
145400                                                                          
145500     IF MID-BELEV-UPD NOT = ALL '+'                                       
145600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-UPD-ATTR                   
145700       MOVE MFS-ROER-EJ-FAELT     TO MOD-BELEV-UPD                        
145800     ELSE                                                                 
145900       MOVE MFS-RENSA-FAELT TO MOD-BELEV-UPD                              
146000     END-IF                                                               
146100                                                                          
146200     IF MID-IDMAIL-UPD NOT = ALL '+'                                      
146300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL-UPD-ATTR                  
146400       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDMAIL-UPD                       
146500     ELSE                                                                 
146600       MOVE MFS-RENSA-FAELT TO MOD-IDMAIL-UPD                             
146700     END-IF                                                               
146800                                                                          
146900     IF MID-IDLEVTLF-KLEV-UPD NOT = ALL '+'                               
147000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-KLEV-UPD-ATTR           
147100       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVTLF-KLEV-UPD                
147200     ELSE                                                                 
147300       MOVE MFS-RENSA-FAELT TO MOD-IDLEVTLF-KLEV-UPD                      
147400     END-IF                                                               
147500                                                                          
147600     IF MID-TENOTE-UPD NOT = ALL '+'                                      
147700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-UPD-ATTR                  
147800        MOVE MFS-ROER-EJ-FAELT    TO MOD-TENOTE-UPD                       
147900     ELSE                                                                 
148000       MOVE MFS-RENSA-FAELT TO MOD-TENOTE-UPD                             
148100     END-IF                                                               
148200                                                                          
148300     IF MID-KVVECKOR-LT NOT = ALL '+'                                     
148400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-LT-IN-ATTR              
148500       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVVECKOR-LT-IN                   
148600     ELSE                                                                 
148700       MOVE MFS-RENSA-FAELT TO MOD-KVVECKOR-LT-IN                         
148800     END-IF                                                               
148900                                                                          
149000     IF MID-KVVECKOR-AT NOT = ALL '+'                                     
149100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-AT-IN-ATTR              
149200       MOVE MFS-ROER-EJ-FAELT     TO MOD-KVVECKOR-AT-IN                   
149300     ELSE                                                                 
149400       MOVE MFS-RENSA-FAELT TO MOD-KVVECKOR-AT-IN                         
149500     END-IF                                                               
149600                                                                          
149700     IF MID-KDLEVTYP NOT = ALL '+'                                        
149800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVTYP-IN-ATTR                 
149900       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDLEVTYP-IN                      
150000     ELSE                                                                 
150100       MOVE MFS-RENSA-FAELT TO MOD-KDLEVTYP-IN                            
150200     END-IF                                                               
150300                                                                          
150400     IF MID-FLRSADR NOT = ALL '+'                                         
150500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLRSADR-IN-ATTR                  
150600       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLRSADR-IN                       
150700     ELSE                                                                 
150800       MOVE MFS-RENSA-FAELT TO MOD-FLRSADR-IN                             
150900     END-IF                                                               
151000                                                                          
151100     .                                                                    
151200     EJECT                                                                
151300 H-LAES-VISA-LEVA-ATT SECTION.                                            
151400     MOVE 'H-LAES-VISA-LEVA-ATT '  TO CURRENT-SECTION                     
151500                                                                          
151600     PERFORM IMS-GHU-WDF101                                               
151700     IF SEGMENT-FINNS                                                     
151800       MOVE WS-IDLEVNR TO WS-IDLEVNR-NUM                                  
151900       INSPECT WS-IDLEVNR-NUM REPLACING                                   
152000                                    ALL SPACE BY ZERO                     
152100       IF WS-IDLEVNR-NUM NUMERIC                                          
152200         IF LEV-IDLEVNR-MOTSV NOT = SPACE                                 
152300           MOVE NEJ      TO INDATA-SW                                     
152400           IF ENGLISH-TEXT                                                
152500              MOVE FEL17        TO MOD-TEMFSFEL                           
152600           ELSE                                                           
152700              MOVE FEL7         TO MOD-TEMFSFEL                           
152800           END-IF                                                         
152900         END-IF                                                           
153000       END-IF                                                             
153100     END-IF                                                               
153200                                                                          
153300     MOVE +1 TO INDX                                                      
153400     PERFORM IMS-GNP-WDF107-FIRST                                         
153500     IF SEGMENT-FINNS                                                     
153600       MOVE ATT-IDATTENT TO MOD-IDATTENT-ENTER                            
153700     ELSE                                                                 
153800       MOVE ZERO TO MOD-IDATTENT-ENTER                                    
153900     END-IF                                                               
154000                                                                          
154100     PERFORM UNTIL INDX > MAX-IDATTENT                                    
154200       IF SEGMENT-FINNS                                                   
154300         IF ATT-BELEV = SPACE AND                                         
154400            ATT-IDLEVTLF-KLEV = SPACE AND                                 
154500            ATT-IDMAIL = SPACE AND                                        
154600            ATT-TENOTE = SPACE                                            
154700                                                                          
154800           PERFORM IMS-GNP-WDF107                                         
154900         ELSE                                                             
155000           MOVE ATT-IDATTENT      TO MOD-IDATTENT(INDX)                   
155100           MOVE ATT-BELEV         TO MOD-BELEV   (INDX)                   
155200           MOVE ATT-IDLEVTLF-KLEV TO MOD-IDLEVTLF-KLEV(INDX)              
155300           MOVE ATT-IDMAIL        TO MOD-IDMAIL  (INDX)                   
155400           MOVE ATT-TENOTE (1:35) TO MOD-TENOTE(INDX)                     
155500                                                                          
155600           PERFORM IMS-GNP-WDF107                                         
155700           ADD +1 TO INDX                                                 
155800         END-IF                                                           
155900       ELSE                                                               
156000         MOVE MFS-RENSA-FAELT TO MOD-IDATTENT(INDX)                       
156100                                 MOD-BELEV   (INDX)                       
156200                                 MOD-IDMAIL  (INDX)                       
156300                                 MOD-IDLEVTLF-KLEV(INDX)                  
156400                                 MOD-TENOTE  (INDX)                       
156500         ADD +1 TO INDX                                                   
156600       END-IF                                                             
156700     END-PERFORM                                                          
156800                                                                          
156900     IF SEGMENT-FINNS                                                     
157000       MOVE ATT-IDATTENT TO MOD-IDATTENT-NEXT                             
157100                                                                          
157200****** OM INDATA FEL LÄGGS EJ INFOMEDDELANDE UT ******                    
157300       IF INDATA-FEL                                                      
157400         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
157500       ELSE                                                               
157600                                                                          
157700****** LÄGGER EJ ÖVER INFOMEDDELANDEN VID PF11 (MED2 & MED3)***           
157800         IF NOT MFS-UPDATE                                                
157900           IF ENGLISH-TEXT                                                
158000              MOVE MED11  TO MOD-TEMFSINF                                 
158100           ELSE                                                           
158200              MOVE MED1   TO MOD-TEMFSINF                                 
158300           END-IF                                                         
158400         END-IF                                                           
158500                                                                          
158600       END-IF                                                             
158700     ELSE                                                                 
158800       MOVE ZERO TO MOD-IDATTENT-NEXT                                     
158900     END-IF                                                               
159000                                                                          
159100     IF INDATA-OK                                                         
159200       MOVE MFS-RENSA-FAELT TO MOD-IDATTENT-IN                            
159300                               MOD-BELEV-UPD                              
159400                               MOD-IDMAIL-UPD                             
159500                               MOD-IDLEVTLF-KLEV-UPD                      
159600                               MOD-TENOTE-UPD                             
159700                               MOD-FLAGGA-BORT                            
159800                               MOD-IDATTENT-UPD                           
159900                               MOD-KVVECKOR-LT-IN                         
160000                               MOD-KVVECKOR-AT-IN                         
160100                               MOD-KDLEVTYP-IN                            
160200                               MOD-FLRSADR-IN                             
160300     END-IF                                                               
160400                                                                          
160500     .                                                                    
160600     EJECT                                                                
160700                                                                          
160800 S01-KOLLA-ATT-UPD SECTION.                                               
160900     MOVE 'S01-KOLLA-ATT-UPD '  TO CURRENT-SECTION                        
161000                                                                          
161100     MOVE NEJ  TO ATTRAD-IFYLLT-SW                                        
161200                                                                          
161300     IF MID-BELEV-UPD   NOT = ALL '+'                                     
161400       MOVE JA             TO ATTRAD-IFYLLT-SW                            
161500     END-IF                                                               
161600                                                                          
161700     IF MID-IDMAIL-UPD   NOT = ALL '+'                                    
161800       MOVE JA             TO ATTRAD-IFYLLT-SW                            
161900     END-IF                                                               
162000                                                                          
162100     IF MID-IDLEVTLF-KLEV-UPD   NOT = ALL '+'                             
162200       MOVE JA             TO ATTRAD-IFYLLT-SW                            
162300     END-IF                                                               
162400                                                                          
162500     IF MID-TENOTE-UPD   NOT = ALL '+'                                    
162600       MOVE JA             TO ATTRAD-IFYLLT-SW                            
162700     END-IF                                                               
162800     .                                                                    
162900     EJECT                                                                
163000                                                                          
163100 MFS-RENSA-FAELT-UT-ATTENT SECTION.                                       
163200                                                                          
163300     MOVE +1 TO INDX                                                      
163400     PERFORM UNTIL INDX > MAX-IDATTENT                                    
163500       MOVE MFS-RENSA-FAELT TO MOD-IDATTENT(INDX)                         
163600                               MOD-BELEV(INDX)                            
163700                               MOD-IDMAIL(INDX)                           
163800                               MOD-IDLEVTLF-KLEV(INDX)                    
163900                               MOD-TENOTE(INDX)                           
164000       ADD +1 TO INDX                                                     
164100     END-PERFORM                                                          
164200     .                                                                    
164300     SKIP2                                                                
164400 MFS-RENSA-FAELT-IN SECTION.                                              
164500                                                                          
164600     MOVE MFS-RENSA-FAELT   TO MOD-BELEV-VCC                              
164700                               MOD-ADLEV-RAD1                             
164800                               MOD-ADLEV-RAD2-VCC                         
164900                               MOD-ADLEV-ORT-VCC                          
165000                               MOD-ADLEVLND                               
165100                               MOD-IDLANDX2                               
165200                               MOD-IDLEVTLF                               
165300                               MOD-IDLEVFAX                               
165400                               MOD-KVVECKOR-LT-IN                         
165500                               MOD-KVVECKOR-AT-IN                         
165600                               MOD-KDLEVTYP-IN                            
165700                               MOD-FLRSADR-IN                             
165800                               MOD-IDATTENT-ENTER                         
165900                               MOD-IDATTENT-NEXT                          
166000                               MOD-BELEV-UPD                              
166100                               MOD-IDMAIL-UPD                             
166200                               MOD-IDLEVTLF-KLEV-UPD                      
166300                               MOD-TENOTE-UPD                             
166400                               MOD-FLAGGA-BORT                            
166500                               MOD-IDATTENT-UPD                           
166600                                                                          
166700                                                                          
166800     .                                                                    
166900     EJECT                                                                
167000 MFS-ROER-EJ-BILD SECTION.                                                
167100                                                                          
167200     PERFORM MFS-ROER-EJ-FAELT-ADR                                        
167300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDATTENT-ENTER                         
167400                               MOD-IDATTENT-NEXT                          
167500                               MOD-IDATTENT-IN                            
167600                               MOD-BELEV-UPD                              
167700                               MOD-IDMAIL-UPD                             
167800                               MOD-IDLEVTLF-KLEV-UPD                      
167900                               MOD-TENOTE-UPD                             
168000                               MOD-FLAGGA-BORT                            
168100                               MOD-IDATTENT-UPD                           
168200                               MOD-IDLEVNR-MOTSV                          
168300                               MOD-KVVECKOR-LT                            
168400                               MOD-KVVECKOR-LT-IN                         
168500                               MOD-KVVECKOR-AT                            
168600                               MOD-KVVECKOR-AT-IN                         
168700                               MOD-KDLEVTYP                               
168800                               MOD-KDLEVTYP-IN                            
168900                               MOD-FLRSADR                                
169000                               MOD-FLRSADR-IN                             
169100     .                                                                    
169200     SKIP2                                                                
169300 MFS-ROER-EJ-FAELT-ADR SECTION.                                           
169400                                                                          
169500     MOVE MFS-ROER-EJ-FAELT TO MOD-BELEV-VCC                              
169600                               MOD-ADLEV-RAD1                             
169700                               MOD-ADLEV-RAD2-VCC                         
169800                               MOD-ADLEV-ORT-VCC                          
169900                               MOD-ADLEVLND                               
170000                               MOD-IDLANDX2                               
170100                               MOD-IDLEVTLF                               
170200                               MOD-IDLEVFAX                               
170300     .                                                                    
170400     SKIP2                                                                
170500 MFS-ROER-EJ-FAELT-LEV SECTION.                                           
170600                                                                          
170700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDATTENT-ENTER                         
170800                               MOD-IDATTENT-NEXT                          
170900                               MOD-IDLEVNR-MOTSV                          
171000                               MOD-KVVECKOR-LT                            
171100                               MOD-KVVECKOR-AT                            
171200                               MOD-KDLEVTYP                               
171300                               MOD-FLRSADR                                
171400     .                                                                    
171500     SKIP2                                                                
171600 MFS-ROER-EJ-FAELT-UT-ATTENT SECTION.                                     
171700                                                                          
171800     MOVE +1 TO INDX                                                      
171900     PERFORM UNTIL INDX > MAX-IDATTENT                                    
172000       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDATTENT(INDX)                      
172100                                  MOD-BELEV(INDX)                         
172200                                  MOD-IDMAIL(INDX)                        
172300                                  MOD-IDLEVTLF-KLEV(INDX)                 
172400                                  MOD-TENOTE(INDX)                        
172500       ADD +1 TO INDX                                                     
172600     END-PERFORM                                                          
172700     .                                                                    
172800     SKIP2                                                                
172900 MFS-FORM-ATTR SECTION.                                                   
173000                                                                          
173100     MOVE MFS-FORMATETS-ATTR TO MOD-IDATTENT-IN-ATTR                      
173200                                MOD-BELEV-UPD-ATTR                        
173300                                MOD-IDMAIL-UPD-ATTR                       
173400                                MOD-IDLEVTLF-KLEV-UPD-ATTR                
173500                                MOD-TENOTE-UPD-ATTR                       
173600                                MOD-FLAGGA-BORT-ATTR                      
173700                                MOD-IDATTENT-UPD-ATTR                     
173800     .                                                                    
173900     SKIP2                                                                
174000 MFS-LAS-IN-IGEN SECTION.                                                 
174100                                                                          
174200     IF MID-BELEV-VCC NOT = ALL '+'                                       
174300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-VCC-ATTR                   
174400     END-IF                                                               
174500                                                                          
174600     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
174700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD1-ATTR                  
174800     END-IF                                                               
174900                                                                          
175000     IF MID-ADLEV-RAD2-VCC NOT = ALL '+'                                  
175100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD2-VCC-ATTR              
175200     END-IF                                                               
175300                                                                          
175400     IF MID-ADLEV-ORT-VCC NOT = ALL '+'                                   
175500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-ORT-VCC-ATTR               
175600     END-IF                                                               
175700                                                                          
175800     IF MID-ADLEVLND NOT = ALL '+'                                        
175900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEVLND-ATTR                    
176000     END-IF                                                               
176100                                                                          
176200     IF MID-IDLANDX2 NOT = ALL '+'                                        
176300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLANDX2-ATTR                    
176400     END-IF                                                               
176500                                                                          
176600     IF MID-IDLEVTLF NOT = ALL '+'                                        
176700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-ATTR                    
176800     END-IF                                                               
176900                                                                          
177000     IF MID-IDLEVFAX NOT = ALL '+'                                        
177100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR                    
177200     END-IF                                                               
177300                                                                          
177400                                                                          
177500     IF MID-IDATTENT-IN NOT = ALL '+'                                     
177600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDATTENT-IN-ATTR                 
177700     END-IF                                                               
177800                                                                          
177900     IF MID-FLAGGA-BORT NOT = ALL '+'                                     
178000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-BORT-ATTR                 
178100     END-IF                                                               
178200                                                                          
178300     IF MID-BELEV-UPD NOT = ALL '+'                                       
178400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-UPD-ATTR                   
178500     END-IF                                                               
178600                                                                          
178700     IF MID-IDMAIL-UPD NOT = ALL '+'                                      
178800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL-UPD-ATTR                  
178900     END-IF                                                               
179000                                                                          
179100     IF MID-IDLEVTLF-KLEV-UPD NOT = ALL '+'                               
179200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-KLEV-UPD-ATTR           
179300     END-IF                                                               
179400                                                                          
179500     IF MID-TENOTE-UPD NOT = ALL '+'                                      
179600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-UPD-ATTR                  
179700     END-IF                                                               
179800                                                                          
179900     IF MID-IDATTENT-UPD NOT = ALL '+'                                    
180000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDATTENT-UPD-ATTR                
180100     END-IF                                                               
180200                                                                          
180300     IF MID-KVVECKOR-LT NOT = ALL '+'                                     
180400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-LT-IN-ATTR              
180500     END-IF                                                               
180600                                                                          
180700     IF MID-KVVECKOR-AT NOT = ALL '+'                                     
180800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-AT-IN-ATTR              
180900     END-IF                                                               
181000                                                                          
181100     IF MID-KDLEVTYP NOT = ALL '+'                                        
181200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVTYP-IN-ATTR                 
181300     END-IF                                                               
181400                                                                          
181500     IF MID-FLRSADR NOT = ALL '+'                                         
181600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLRSADR-IN-ATTR                  
181700     END-IF                                                               
181800     .                                                                    
181900     EJECT                                                                
182000 MFS-LAS-IN-IGEN-ATTRAD SECTION.                                          
182100                                                                          
182200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-UPD-ATTR                     
182300                                                                          
182400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL-UPD-ATTR                    
182500                                                                          
182600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-KLEV-UPD-ATTR             
182700                                                                          
182800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-UPD-ATTR                    
182900     .                                                                    
183000     EJECT                                                                
183100* IMS SEKTIONER                                                           
183200     SKIP3                                                                
183300 IMS-GET-MSG SECTION.                                                     
183400                                                                          
183500     MOVE '  QC' TO GODK-STATUSKODER                                      
183600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
183700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
183800     PERFORM IMS-STATUSKONTROLL                                           
183900     .                                                                    
184000     SKIP3                                                                
184100 IMS-INSERT-MSG SECTION.                                                  
184200                                                                          
184300     IF ENGLISH-TEXT                                                      
184400        MOVE 'N' TO MFS-KDHUVOMR                                          
184500     END-IF                                                               
184600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
184700     MOVE SPACE TO GODK-STATUSKODER                                       
184800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
184900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
185000     PERFORM IMS-STATUSKONTROLL                                           
185100     .                                                                    
185200     EJECT                                                                
185300 IMS-GHU-WDF101 SECTION.                                                  
185400     MOVE 'IMS-GHU-WDF101 '  TO DBS-SECTION                               
185500                                                                          
185600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
185700          DELIMITED BY SIZE INTO SSA1                                     
185800     MOVE '  GE' TO GODK-STATUSKODER                                      
185900     CALL CBLTDLI USING GHU WDF1-PCB DLI-IO-WDF101 SSA1                   
186000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSKONTROLL                                           
186200     .                                                                    
186300     EJECT                                                                
186400 IMS-REPL-WDF101 SECTION.                                                 
186500     MOVE 'IMS-REPL-WDF101 '  TO DBS-SECTION                              
186600                                                                          
186700     MOVE '  ' TO GODK-STATUSKODER                                        
186800     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF101                       
186900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
187000     PERFORM IMS-STATUSKONTROLL                                           
187100     .                                                                    
187200     EJECT                                                                
187300 IMS-GHNP-WDF106 SECTION.                                                 
187400     MOVE 'IMS-GHNP-WDF106 '  TO CURRENT-SECTION                          
187500                                                                          
187600     MOVE 'WDF106   ' TO SSA1                                             
187700     MOVE '  GE' TO GODK-STATUSKODER                                      
187800     CALL CBLTDLI USING GHNP WDF1-PCB DLI-IO-WDF106 SSA1                  
187900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
188000     PERFORM IMS-STATUSKONTROLL                                           
188100     .                                                                    
188200     EJECT                                                                
188300 IMS-REPL-WDF106  SECTION.                                                
188400     MOVE 'IMS-REPL-WDF106 '  TO DBS-SECTION                              
188500                                                                          
188600     MOVE '  ' TO GODK-STATUSKODER                                        
188700     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF106                       
188800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
188900     PERFORM IMS-STATUSKONTROLL                                           
189000     .                                                                    
189100     EJECT                                                                
189200 IMS-GNP-WDF107 SECTION.                                                  
189300     MOVE 'IMS-GNP-WDF107 '  TO DBS-SECTION                               
189400                                                                          
189500     STRING 'WDF107  (IDATTENT=>' W-IDATTENT-X ')'                        
189600          DELIMITED BY SIZE INTO SSA1                                     
189700     MOVE '  GE' TO GODK-STATUSKODER                                      
189800     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF107 SSA1                   
189900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
190000     PERFORM IMS-STATUSKONTROLL                                           
190100     .                                                                    
190200     EJECT                                                                
190300 IMS-GNP-WDF107-FIRST SECTION.                                            
190400     MOVE 'IMS-GNP-WDF107-FIRST '  TO DBS-SECTION                         
190500                                                                          
190600     STRING 'WDF107  *F(IDATTENT=>' W-IDATTENT-X ')'                      
190700          DELIMITED BY SIZE INTO SSA1                                     
190800     MOVE '  GE' TO GODK-STATUSKODER                                      
190900     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF107 SSA1                   
191000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300     EJECT                                                                
191400 IMS-GHNP-WDF107-UNIK SECTION.                                            
191500     MOVE 'IMS-GHNP-WDF107-UNIK '  TO DBS-SECTION                         
191600                                                                          
191700     STRING 'WDF107  *F(IDATTENT =' W-IDATTENT-X ')'                      
191800          DELIMITED BY SIZE INTO SSA1                                     
191900     MOVE '  GE' TO GODK-STATUSKODER                                      
192000     CALL CBLTDLI USING GHNP WDF1-PCB DLI-IO-WDF107 SSA1                  
192100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400     EJECT                                                                
192500 IMS-GHU-WDF107 SECTION.                                                  
192600     MOVE 'IMS-GHU-WDF107 '  TO DBS-SECTION                               
192700                                                                          
192800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
192900          DELIMITED BY SIZE INTO SSA1                                     
193000     STRING 'WDF107  (IDATTENT =' W-IDATTENT-X ')'                        
193100          DELIMITED BY SIZE INTO SSA2                                     
193200     MOVE '  GE' TO GODK-STATUSKODER                                      
193300     CALL CBLTDLI USING GHU WDF1-PCB DLI-IO-WDF107 SSA1 SSA2              
193400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     .                                                                    
193700     EJECT                                                                
193800 IMS-ISRT-WDF107 SECTION.                                                 
193900     MOVE 'IMS-ISRT-WDF107 '  TO DBS-SECTION                              
194000                                                                          
194100     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
194200          DELIMITED BY SIZE INTO SSA1                                     
194300     MOVE 'WDF107   ' TO SSA2                                             
194400     MOVE '  II' TO GODK-STATUSKODER                                      
194500     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF107 SSA1 SSA2             
194600     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
194700     PERFORM IMS-STATUSKONTROLL                                           
194800     .                                                                    
194900     EJECT                                                                
195000 IMS-REPL-WDF107  SECTION.                                                
195100     MOVE 'IMS-REPL-WDF107 '  TO DBS-SECTION                              
195200                                                                          
195300     MOVE '  ' TO GODK-STATUSKODER                                        
195400     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF107                       
195500     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
195600     PERFORM IMS-STATUSKONTROLL                                           
195700     .                                                                    
195800     EJECT                                                                
195900 IMS-DLET-WDF107 SECTION.                                                 
196000     MOVE 'IMS-DLET-WDF107 '  TO DBS-SECTION                              
196100                                                                          
196200     MOVE '  ' TO GODK-STATUSKODER                                        
196300     CALL CBLTDLI USING DLET WDF1-PCB DLI-IO-WDF107                       
196400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     EJECT                                                                
196800 IMS-GNP-WDF122 SECTION.                                                  
196900     MOVE 'IMS-GNP-WDF122 '  TO DBS-SECTION                               
197000                                                                          
197100     MOVE 'WDF122    '      TO SSA1                                       
197200     MOVE '  GE' TO GODK-STATUSKODER                                      
197300     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF122 SSA1                   
197400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700     EJECT                                                                
197800 IMS-INSERT-ALT-MSG SECTION.                                              
197900     MOVE 'IMS-INSERT-ALT-MSG '  TO DBS-SECTION                           
198000                                                                          
198100     MOVE SPACE TO GODK-STATUSKODER                                       
198200     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
198300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
198400     PERFORM IMS-STATUSKONTROLL                                           
198500     .                                                                    
198600                                                                          
198700 IMS-STATUSKONTROLL SECTION.                                              
198800                                                                          
198900     SET STATUS-IX TO 1                                                   
199000     SEARCH GODK-STATUS                                                   
199100       AT END CALL FELLOG                                                 
199200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
199300     END-SEARCH                                                           
199400     .                                                                    
199500     EJECT                                                                
