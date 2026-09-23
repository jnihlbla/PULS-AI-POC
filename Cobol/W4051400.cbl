000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4051400.                                                
000400 AUTHOR.         STEFANO GIOBBI.                                          
000500 DATE-WRITTEN.   90/08/06.                                                
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001100*                                                                         
001200*        PROGRAMMET VISAR PER ORDER, EV KUND, EV C-LAGER OCH EV           
001300*        FRAKTKOD TRANSPORT PER KUNDORDER. ÄR C-LAGER EJ IFYLLT           
001400*        SÄTTS DETTA TILL 1.                                              
001500*        KONTROLL GÖRS ATT ORDER HAR ORDERRADER, OM INTE SKRIVS           
001600*        RADEN EJ UT PÅ SKÄRMEN.                                          
001700*                                                                         
001800*        TRANSAKTION: W4T514                                              
001900*        MID        : W4I51401C0                                          
002000*        MOD        : W4O51401C0                                          
002100*                                                                         
002200*  BASER:                                                                 
002300*        FYSISKT  LOGISKT    COPYTEXT    PREFIX (COPYTEXT)                
002400*  WDQ2  WDQ201   WLORQI01   WDQ201CCC0  OHUV-                            
002500*            12         12       12      ARB-                             
002600*  WDQ2C     C1   WLORQL01       C1      SEQC- (C-INDEX TILL WDQ2)        
002700*  WDR1  WDR101   WLXXKA01   WDGX4431C0  4431-                            
002800*                     KA11       4432    4432-                            
002900*  WDR1  WDR101   WLXXKB01   WDGX4433C0  4433-                            
003000*                     KB11       4434    4434-                            
003100*  WDB6  WDB601              WDB601      DCS-                             
003200*                                                                         
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP3                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC  X(08) VALUE 'W4051400'.             
004200 77  FELTEXT-VID-CALL-ABEND      PIC  X(64) VALUE SPACE.                  
004300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
004400                                                                          
004500 77  JA                          PIC  X     VALUE 'J'.                    
004600 77  NEJ                         PIC  X     VALUE 'N'.                    
004700                                                                          
004800 77  RAD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  RAD-IX-MAX-13               PIC S9(4)  VALUE +13   COMP SYNC.        
005000                                                                          
005300                                                                          
005400 77  WS-IDTIDZON                 PIC X(2)   VALUE SPACE.                  
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X      VALUE 'J'.                    
005700     88  NYCKLAR-OK                         VALUE 'J'.                    
005800     88  NYCKLAR-FEL                        VALUE 'N'.                    
005900                                                                          
006000 01  SW-ORDERRADER-FINNS         PIC X(1)   VALUE 'N'.                    
006100                                                                          
006200 77  W-IDTRANS                   PIC X(4)   VALUE SPACE.                  
006300     88  EGEN-MID                           VALUE '4514'.                 
006400     88  GODK-MID                           VALUE '4510' '4515'           
006500                                                  '4511' '4516'           
006600                                                  '4512' '4517'           
006700                                                  '4513' '4518'           
006800                                                  '4514' '4519'.          
006900                                                                          
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  WMEDKONV                PIC X(8)   VALUE 'WMEDKONV'.             
007300     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
007400     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
007500     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
007600     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
007700     SKIP2                                                                
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*01 -COPY WMSGINIT                                                        
008200*                                                                         
008300     EJECT                                                                
008400 01  KONSTANT-AREA.                                                       
008500     03  FILLER                  PIC  X(16)        VALUE                  
008600                                             'KONST AREA START'.          
008700     03  K-IDKUNDNR-0            PIC  X(6)         VALUE '     0'.        
008800     03  K-KDFRAKT-0             PIC  X(6)         VALUE ' 0'.            
008900     03  K-IDTRP-ZERO            PIC  X(5)         VALUE '00000'.         
009000     EJECT                                                                
009100*                                                                         
009200 01  SPAR-AREA.                                                           
009300     03  FILLER                  PIC X(16)         VALUE                  
009400                                             'SPAR AREA START'.           
009500     03  SPAR-KDFRAKT            PIC 9(2)          VALUE ZERO.            
009600     03  SPAR-IDDC               PIC X(2)          VALUE SPACE.           
009700*                                                                         
009800     03  SPAR-DATRPAVT.                                                   
009900         05  SPAR-DATRPAVT-DATRPAVD                                       
010000                                 PIC  9(8)         VALUE ZERO.            
010100         05  SPAR-DATRPAVT-TIHHMM                                         
010200                                 PIC  9(4)         VALUE ZERO.            
010300*                                                                         
010400     03  SPAR-TITRPAVG.                                                   
010500         05  SPAR-TITRPAVG-VV    PIC  9(2)         VALUE ZERO.            
010600         05  SPAR-TITRPAVG-A     PIC  9(1)         VALUE ZERO.            
010700         05  SPAR-TITRPAVG-HHMM  PIC  9(4)         VALUE ZERO.            
010800*                                                                         
010900     03  SPAR-TITRPAVG-NUM       REDEFINES SPAR-TITRPAVG                  
011000                                 PIC  9(7).                               
011100*                                                                         
011200     03  SPAR-ARB-DATRPAVT.                                               
011300         05  SPAR-ARB-DATRPAVD   PIC  9(8)         VALUE ZERO.            
011400         05  SPAR-ARB-TIHHMM     PIC S9(5)  COMP-3 VALUE +0.              
011500*                                                                         
011600     03  SPAR-OHUV-KDORDKL       PIC  9(1)         VALUE ZERO.            
011700*                                                                         
011800     03  SPAR-SEQC-IDKUNDNR      PIC S9(7)  COMP-3 VALUE +0.              
011900     03  SPAR-SEQC-IDKUNDRF      PIC  X(10)        VALUE SPACE.           
012000     EJECT                                                                
012100*                                                                         
012200 01  HELP-AREA.                                                           
012300     03  FILLER                  PIC X(16)         VALUE                  
012400                                             'HELP AREA START'.           
012500     03  HELP-RAD-IX             PIC S9(9)  COMP-3 VALUE +0.              
012600     03  HELP-IDKUNDNR           PIC  9(6)         VALUE ZERO.            
012700     03  HELP-KDFRAKT            PIC  9(2)         VALUE ZERO.            
012800*                                                                         
012900     03  HELP-TITRPAVT-TIAAVVD.                                           
013000         05 FILLER               PIC  9(2)         VALUE ZERO.            
013100         05 HELP-TITRPAVT-TIVVD  PIC  9(3)         VALUE ZERO.            
013200*                                                                         
013300     03  HELP-KONV-TITRPAVT.                                              
013400         05  HELP-RAD-TIVVD      PIC  9(3)         VALUE ZERO.            
013500         05  HELP-RAD-TIHHMM     PIC  9(4)         VALUE ZERO.            
013600*                                                                         
013700     03  HELP-RAD-TITRPAVG       REDEFINES HELP-KONV-TITRPAVT             
013800                                 PIC  9(7).                               
013900*                                                                         
014000     03  HELP-TITRPAVG.                                                   
014100         05  HELP-TITRPAVG-VV    PIC  9(2)         VALUE ZERO.            
014200         05  HELP-TITRPAVG-A     PIC  9(1)         VALUE ZERO.            
014300         05  HELP-TITRPAVG-HHMM  PIC  9(4)         VALUE ZERO.            
014400*                                                                         
014500     03  HELP-TITRPAVG-NUM       REDEFINES HELP-TITRPAVG                  
014600                                 PIC  9(7).                               
014700     SKIP2                                                                
014800 01  FELM-AREA.                                                           
014900     03  FILLER                  PIC  X(16)        VALUE                  
015000                                             'FELM AREA START'.           
015100     03  FELM-DISTR-KUND-SAKNAS-040                                       
015200                                 PIC  X(3)         VALUE '040'.           
015300     03  FELM-INFO-SAKNAS-413    PIC  X(3)         VALUE '413'.           
015400     SKIP2                                                                
015500 01  FELT-AREA.                                                           
015600     03  FELT-FELT-AREA          PIC  X(16)        VALUE                  
015700                                             'FELM AREA START'.           
015800     03  FELT-WDATKONV-TITRPAVT  PIC  X(64)        VALUE                  
015900         'KONVERTERING AV ARB-TITRPAVT MHA WDATKONV MISSLYCKADES'.        
016000     SKIP2                                                                
016100 01  MESSAGE-CODES.                                                       
016200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
016310     03  INF-NO-MORE-F6          PIC X(3)    VALUE '368'.                 
016400     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
016500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016800*   -COPY WMEDAREA                                                        
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
017100*   -COPY WDATAREA                                                        
017200     EJECT                                                                
017300*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
017400   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
017500     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
017600                                                                          
017700 01  BILD-HOPP-AREOR.                                                     
017800                                                                          
017900   03    W-BILD               PIC X(4)    VALUE SPACE.                    
018000   03    W-HOPP-IDTRANS.                                                  
018100     05  FILLER               PIC X(1)    VALUE 'W'.                      
018200     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
018300     05  FILLER               PIC X(1)    VALUE 'T'.                      
018400     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
018500     05  FILLER               PIC X(2)    VALUE SPACE.                    
018600                                                                          
018700                                                                          
018800   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
018900   03      P-TO-P-SW.                                                     
019000                                                                          
019100     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
019200     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
019300     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
019400     05  P-TO-P-KDTRANS          PIC X(8).                                
019500     05  P-TO-P-IDTRANS          PIC X(4).                                
019600     05  P-TO-P-KDMFSFOR         PIC X(1).                                
019700     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
019800                                                                          
019900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020000*                                                                         
020100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020200     SKIP3                                                                
020300*01  MID -COPY W4I51401                                                   
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020600     SKIP3                                                                
020700*01  -COPY WMSGAREA                                                       
020800     EJECT                                                                
020900     03  MOD REDEFINES MSG-AREA.                                          
021000*      05  -COPY W4O51401                                                 
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021300     SKIP3                                                                
021400*01  -COPY WMFSAREA                                                       
021500     EJECT                                                                
021600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021700*                                                                         
021800 01  FILLER                          PIC X(16)  VALUE 'IMS-WS'.           
021900     SKIP3                                                                
022000 01  SAVE-AREA.                                                           
022100   03  SAVE-IDTRANS              PIC X(4)    VALUE SPACE.                 
022200   03  PGNO                      PIC 9(2).                                
022210   03  FIRST-SW                  PIC X       VALUE 'J'.                   
022300   03  SAVE-AREA-PREV OCCURS 20 TIMES.                                    
022400         05  SAVE-IDDISTR-PREV    PIC S9(5)   VALUE ZERO COMP-3.          
022500         05  SAVE-IDKUNDNR-PREV   PIC S9(7)   VALUE ZERO COMP-3.          
022600         05  SAVE-IDKUNDRF-PREV   PIC X(10)   VALUE SPACE.                
022700         05  SAVE-IDDC-PREV       PIC X(2)    VALUE SPACE.                
022800         05  SAVE-IDORDER-PREV    PIC 9(7)    VALUE ZERO.                 
022900   03  SAVE-AREA-NEXT.                                                    
023000         05  SAVE-IDDISTR-NEXT    PIC S9(5)   VALUE ZERO COMP-3.          
023100         05  SAVE-IDKUNDNR-NEXT   PIC S9(7)   VALUE ZERO COMP-3.          
023200         05  SAVE-IDKUNDRF-NEXT   PIC X(10)   VALUE SPACE.                
023300         05  SAVE-IDDC-NEXT       PIC X(2)    VALUE SPACE.                
023400         05  SAVE-IDORDER-NEXT    PIC 9(7)    VALUE ZERO.                 
023500                                                                          
023600 01  NYCKLAR-TILL-DLI.                                                    
023700     03  W-IDORDER-X.                                                     
023800         05  W-OHUV-IDORDER          PIC S9(7)  COMP-3 VALUE ZERO.        
023900                                                                          
024000     03  W-IDDC-X.                                                        
024100         05  W-IDDC                  PIC  X(2)  VALUE SPACE.              
024200                                                                          
024300     03  W-SEQC-WDQ2C1KY-MIN-X.                                           
024400         05  W-SEQC-IDGMTREF-MIN-X.                                       
024500             07  W-SEQC-IDDISTR-MIN  PIC S9(5)  COMP-3 VALUE ZERO.        
024600             07  W-SEQC-IDKUNDNR-MIN PIC S9(7)  COMP-3 VALUE ZERO.        
024700             07  W-SEQC-IDKUNDRF-MIN.                                     
024800                 09  W-SEQC-IDORDNR7-MIN                                  
024900                                     PIC  9(7)  VALUE ZERO.               
025000                 09  FILLER          PIC  X(3)  VALUE LOW-VALUE.          
025100                                                                          
025200     03  W-SEQC-WDQ2C1KY-MAX-X.                                           
025300         05  W-SEQC-IDGMTREF-MAX-X.                                       
025400             07  W-SEQC-IDDISTR-MAX  PIC S9(5)  COMP-3 VALUE ZERO.        
025500             07  W-SEQC-IDKUNDNR-MAX PIC S9(7)  COMP-3 VALUE ZERO.        
025600             07  W-SEQC-IDKUNDRF-MAX.                                     
025700                 09  W-SEQC-IDORDNR7-MAX                                  
025800                                     PIC  9(7)  VALUE ZERO.               
025900                 09  FILLER          PIC  X(3)  VALUE HIGH-VALUE.         
026000                                                                          
026100     03  W-4431-WDGXKEY-X.                                                
026200         05  W-4431-IDHTYP           PIC  X(4)  VALUE '4431'.             
026300         05  W-4431-IDDC             PIC  X(2)  VALUE ZERO.               
026400         05  W-4431-LOW-VALUE        PIC  X(24) VALUE LOW-VALUE.          
026500                                                                          
026600     03  W-4432-WDGXKEY-X.                                                
026700         05  W-4432-IDTRP            PIC  X(5)  VALUE SPACE.              
026800                                                                          
026900     03  W-4433-WDGXKEY-X.                                                
027000         05  W-4433-IDHTYP           PIC  X(4)  VALUE '4433'.             
027100         05  W-4433-IDDC             PIC  X(2)  VALUE SPACE.              
027200         05  W-4433-LOW-VALUE        PIC  X(24) VALUE LOW-VALUE.          
027300                                                                          
027400     03  W-4434-WDGXKEY-X.                                                
027500         05  W-4434-IDTRP            PIC  X(5)  VALUE SPACE.              
027600         05  W-4434-TITRPAVG         PIC S9(7)  COMP-3 VALUE ZERO.        
027700         05  W-4434-LOW-VALUE        PIC  X(1)  VALUE LOW-VALUE.          
027800                                                                          
027900     03  W-IDDC-B6-X.                                                     
028000         05 W-IDDC-B6                  PIC X(2).                          
028100                                                                          
028200     EJECT                                                                
028300*    --- STATUS-KOD FRÅN IMS                                              
028400 01  STATUS-WS                   PIC  X(2).                               
028500     88  STATUS-OK                           VALUE '  '.                  
028600     88  SEGMENT-FINNS                       VALUE '  '.                  
028700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028800     88  BASEN-SLUT                          VALUE 'GB'.                  
028900     88  TRANSKOD-FEL                        VALUE 'A1'.                  
029000     88  SECURITY-FEL                        VALUE 'A4'.                  
029100     SKIP2                                                                
029200 01  GODK-STATUSKODER.                                                    
029300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029400     SKIP3                                                                
029500 01  SSA1                        PIC X(128).                              
029600 01  SSA2                        PIC X(128).                              
029700     EJECT                                                                
029800*    --- IMS FUNKTIONSKODER                                               
029900*01  -COPY W0003                                                          
030000     EJECT                                                                
030100*    ---  DLI INPUT-OUTPUT AREA                                           
030200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
030300     SKIP3                                                                
030400 01  DLI-IO-AREA.                                                         
030500     03  IO-AREA                 PIC X(4000) VALUE SPACE.                 
030600     SKIP3                                                                
030700     03  WLORQI01 REDEFINES IO-AREA.                                      
030800*        05  -COPY WDQ201                                                 
030900     EJECT                                                                
031000     03  WLORQI12 REDEFINES IO-AREA.                                      
031100*        05  -COPY WDQ212                                                 
031200     EJECT                                                                
031300     03  WLORQI13 REDEFINES IO-AREA.                                      
031400*        05  -COPY WDQ2C1                                                 
031500     EJECT                                                                
031600     03  WLXXKA11 REDEFINES IO-AREA.                                      
031700*        05  -COPY WDGX4432                                               
031800     EJECT                                                                
031900     03  WLXXKB11 REDEFINES IO-AREA.                                      
032000*        05  -COPY WDGX4434                                               
032100                                                                          
032200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032300 01   DLI-IO-AREA-B601.                                                   
032400*     03  -COPY WDB601                                                    
032500                                                                          
032510 01  FILLER               PIC X(16)   VALUE 'WDQ221-AREA'.                
032520 01  DLI-IO-AREA-Q221.                                                    
032530*    03  -COPY WDQ221                                                     
032540                                                                          
032600     EJECT                                                                
032700 LINKAGE SECTION.                                                         
032800                                                                          
032900*01  -COPY W0009      -PRE MSG-                                           
033000     EJECT                                                                
033100*01  -COPY W0009      -PRE ALT-                                           
033200     EJECT                                                                
033300*01  -COPY W0008      -PRE USEA-                                          
033400     05  FILLER                  PIC X.                                   
033500     EJECT                                                                
033600*01  -COPY W0008      -PRE ORQI-                                          
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900*01  -COPY W0008      -PRE ORQLCSQ-                                       
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034200*01  -COPY W0008      -PRE XXKA-                                          
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008      -PRE XXKB-                                          
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008      -PRE WDB6-                                          
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ORQI-PCB              
035200                                   ORQLCSQ-PCB XXKA-PCB XXKB-PCB          
035300                                   WDB6-PCB.                              
035400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ORQI-PCB              
035500                                   ORQLCSQ-PCB XXKA-PCB XXKB-PCB          
035600                                   WDB6-PCB.                              
035700                                                                          
035800     PERFORM IMS-GET-MSG                                                  
035900     IF SEGMENT-FINNS                                                     
036000       PERFORM A-INIT                                                     
036100       PERFORM B-KOLLA-NYCKLAR                                            
036200       IF NYCKLAR-OK                                                      
036300         IF MFS-FIRST                                                     
036400           PERFORM C-FOERSTA-SIDA                                         
036500         ELSE                                                             
036600           IF MFS-NEXT                                                    
036700             PERFORM D-NAESTA-SIDA                                        
036800           ELSE                                                           
036900             IF MFS-PREVIOUS                                              
037000               PERFORM I-PREV-SIDA                                        
037100             ELSE                                                         
037200               PERFORM E-SAMMA-SIDA                                       
037300             END-IF                                                       
037400           END-IF                                                         
037500         END-IF                                                           
037600         IF STARTA-ANNAN-BILD                                             
037700            CONTINUE                                                      
037800         ELSE                                                             
037900           PERFORM F-LAES-VISA-INFO                                       
038000         END-IF                                                           
038100       END-IF                                                             
038200       IF STARTA-ANNAN-BILD                                               
038300          CONTINUE                                                        
038400       ELSE                                                               
038500          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51401 + 4                   
038600          PERFORM IMS-INSERT-MSG                                          
038700       END-IF                                                             
038800     END-IF                                                               
038900                                                                          
039000     MOVE ZERO TO RETURN-CODE                                             
039100     GOBACK                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 A-INIT SECTION.                                                          
039500                                                                          
039600     MOVE LOW-VALUE                       TO W-SEQC-WDQ2C1KY-MIN-X        
039700     MOVE HIGH-VALUE                      TO W-SEQC-WDQ2C1KY-MAX-X        
039800                                                                          
039900     IF MSG-DUBBLA-TRANSKODER                                             
040000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I51401                 
040100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
040200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
040300     ELSE                                                                 
040400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I51401                 
040500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
040600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
040700     END-IF                                                               
040800                                                                          
040900     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
041000     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
041100     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
041200                                                                          
041300     MOVE LOW-VALUE                       TO MSG-AREA                     
041400     MOVE 'W4O514N1'                      TO MFS-IDMOD                    
041500     MOVE '4514'                          TO MOD-IDTRANS                  
041600     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
041700                                             MOD-TEMFSINF                 
041800                                                                          
041900     IF NOT EGEN-MID                                                      
042000       MOVE SPACE                         TO MFS-KDTRTYP                  
042100       MOVE '7'                           TO MFS-IDPFK                    
042200     END-IF                                                               
042300                                                                          
042400     .                                                                    
042500     EJECT                                                                
042600 B-KOLLA-NYCKLAR SECTION.                                                 
042700                                                                          
042800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
042900     MOVE '001'             TO MSGI-KDCALL                                
043000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
043100     MOVE '4514'            TO MSGI-IDTRANS                               
043200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
043300     IF EGEN-MID                                                          
043400        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
043500        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
043600        MOVE MID-KDFRAKT-IN  TO MSGI-KDFRAKT                              
043700     END-IF                                                               
043800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043900     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
044000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
044100     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
044200                                                                          
044300     MOVE    JA                TO    NYCKLAR-SW                           
044400     MOVE    MFS-RENSA-FAELT   TO    MOD-KDFRAKT-IN                       
044500                                     MOD-IDDC-IN                          
044600                                     MOD-IDKUNDNR-IN                      
044700                                     MOD-IDDISTR-IN                       
044800                                     MOD-KDORDKL-IN                       
044900                                     MOD-KDSTATUS-IN                      
045000                                                                          
045100     PERFORM BA-KONTROLLERA-IDDISTR                                       
045200     PERFORM BB-KONTROLLERA-IDKUNDNR                                      
045300     PERFORM BC-KONTROLLERA-IDDC                                          
045400     PERFORM BD-KONTROLLERA-KDFRAKT                                       
045500     PERFORM BE-BEHANDLA-KDORDKL                                          
045600     PERFORM BF-BEHANDLA-KDSTATUS                                         
045700                                                                          
045800                                                                          
045900     IF NYCKLAR-FEL                                                       
046000       MOVE    ERR-WRONG-KEY   TO    MED-IDMFSFEL                         
046100       CALL    WMEDKONV        USING MED-WMEDAREA                         
046200       MOVE    MED-MFSFEL      TO    MOD-TEMFSFEL                         
046300       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
046400       IF NOT GODK-MID                                                    
046500         MOVE  MFS-RENSA-FAELT TO    MOD-IDDISTR-UT                       
046600                                     MOD-IDKUNDNR-UT                      
046700                                     MOD-IDDC-UT                          
046800                                     MOD-KDFRAKT-UT                       
046900                                     MOD-KDORDKL-UT                       
047000                                     MOD-KDSTATUS-UT                      
047100       END-IF                                                             
047200     END-IF                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 BA-KONTROLLERA-IDDISTR SECTION.                                          
047600                                                                          
047700     IF MID-IDDISTR-IN         NOT = ALL '+'                              
047800       MOVE    '7'             TO        MFS-IDPFK                        
047900       MOVE    SPACE           TO        MFS-KDTRTYP                      
048000     END-IF                                                               
048100                                                                          
048200     MOVE      MSGI-IDDISTR    TO        MOD-IDDISTR-UT                   
048300     INSPECT   MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
048400                                                                          
048500     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
048600       MOVE    MSGI-IDDISTR      TO      W-SEQC-IDDISTR-MIN               
048700                                         W-SEQC-IDDISTR-MAX               
048800     ELSE                                                                 
048900       MOVE    NEJ             TO        NYCKLAR-SW                       
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
049400                                                                          
049500     IF MID-IDKUNDNR-IN        NOT = ALL '+'                              
049600       MOVE '7'                TO MFS-IDPFK                               
049700       MOVE SPACE              TO MFS-KDTRTYP                             
049800     END-IF                                                               
049900                                                                          
050000     IF MSGI-IDKUNDNR = SPACE                                             
050100       MOVE ALL '0'            TO W-SEQC-IDKUNDNR-MIN                     
050200       MOVE ALL '9'            TO W-SEQC-IDKUNDNR-MAX                     
050300     ELSE                                                                 
050400       IF MSGI-IDKUNDNR NUMERIC                                           
050500         MOVE MSGI-IDKUNDNR      TO W-SEQC-IDKUNDNR-MIN                   
050600                                  W-SEQC-IDKUNDNR-MAX                     
050700       ELSE                                                               
050800         MOVE NEJ              TO NYCKLAR-SW                              
050900       END-IF                                                             
051000     END-IF                                                               
051100                                                                          
051200     MOVE    MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                         
051300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
051400     IF MSGI-IDKUNDNR = ZERO                                              
051500       MOVE K-IDKUNDNR-0       TO MOD-IDKUNDNR-UT                         
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 BC-KONTROLLERA-IDDC SECTION.                                             
052000                                                                          
052100     IF MID-IDDC-IN = ALL '+'                                             
052200       MOVE MID-IDDC-UT   TO W-IDDC-B6                                    
052300     ELSE                                                                 
052400       MOVE MID-IDDC-IN       TO W-IDDC-B6                                
052500       MOVE '7'               TO MFS-IDPFK                                
052600       MOVE SPACE             TO MFS-KDTRTYP                              
052700     END-IF                                                               
052800     PERFORM IMS-GU-WDB601                                                
052900                                                                          
053000     IF DCS-KDDC = SPACE                                                  
053100       MOVE MSGI-IDDC         TO W-IDDC-B6                                
053200                                 SPAR-IDDC                                
053300       PERFORM IMS-GU-WDB601                                              
053400     ELSE                                                                 
053500       MOVE W-IDDC-B6         TO SPAR-IDDC                                
053600     END-IF                                                               
053700                                                                          
053800     MOVE W-IDDC-B6           TO MOD-IDDC-UT                              
053900     .                                                                    
054000     EJECT                                                                
054100 BD-KONTROLLERA-KDFRAKT SECTION.                                          
054200                                                                          
054300     IF MID-KDFRAKT-IN         NOT = ALL '+'                              
054400       MOVE '7'                TO        MFS-IDPFK                        
054500       MOVE SPACE              TO        MFS-KDTRTYP                      
054600     END-IF                                                               
054700                                                                          
054800     MOVE    MSGI-KDFRAKT      TO        MOD-KDFRAKT-UT                   
054900                                         SPAR-KDFRAKT                     
055000     INSPECT MOD-KDFRAKT-UT    REPLACING LEADING ZERO BY SPACE            
055100                                                                          
055200     IF MSGI-KDFRAKT NUMERIC                                              
055300       CONTINUE                                                           
055400     ELSE                                                                 
055500       MOVE NEJ                TO        NYCKLAR-SW                       
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900 BE-BEHANDLA-KDORDKL SECTION.                                             
056000                                                                          
056100     IF MID-KDORDKL-IN = ALL '+'                                          
056200       MOVE MID-KDORDKL-UT TO MOD-KDORDKL-UT                              
056300     ELSE                                                                 
056400       MOVE MID-KDORDKL-IN TO MOD-KDORDKL-UT                              
056500     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800 BF-BEHANDLA-KDSTATUS SECTION.                                            
056900                                                                          
057000     IF MID-KDSTATUS-IN = ALL '+'                                         
057100       MOVE MID-KDSTATUS-UT TO MOD-KDSTATUS-UT                            
057200     ELSE                                                                 
057300       MOVE MID-KDSTATUS-IN TO MOD-KDSTATUS-UT                            
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 C-FOERSTA-SIDA SECTION.                                                  
057800                                                                          
057900     INITIALIZE SAVE-AREA                                                 
058000     MOVE 01             TO    PGNO                                       
058010     MOVE JA             TO    FIRST-SW                                   
058100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
058200     CALL WMEDKONV       USING MED-WMEDAREA                               
058300     MOVE MED-MFSINF     TO    MOD-TEMFSINF                               
058400     .                                                                    
058500     EJECT                                                                
058600 D-NAESTA-SIDA SECTION.                                                   
058700                                                                          
058800     IF SAVE-IDTRANS = '4514'                                             
058900       IF SAVE-IDDISTR-NEXT NOT = ZERO                                    
059000         MOVE SAVE-IDDISTR-NEXT  TO W-SEQC-IDDISTR-MIN                    
059100         MOVE SAVE-IDKUNDNR-NEXT TO W-SEQC-IDKUNDNR-MIN                   
059200         MOVE SAVE-IDKUNDRF-NEXT TO W-SEQC-IDKUNDRF-MIN                   
059300                                                                          
059400         IF (SAVE-IDDISTR-PREV (PGNO) NOT = SAVE-IDDISTR-NEXT  OR         
059500             SAVE-IDKUNDNR-PREV(PGNO) NOT = SAVE-IDKUNDNR-NEXT OR         
059600             SAVE-IDKUNDRF-PREV(PGNO) NOT = SAVE-IDKUNDRF-NEXT OR         
059700             SAVE-IDDC-PREV    (PGNO) NOT = SAVE-IDDC-NEXT     OR         
059800             SAVE-IDORDER-PREV (PGNO) NOT = SAVE-IDORDER-NEXT)            
059900           IF PGNO = 20                                                   
060000             PERFORM VARYING PGNO FROM 1 BY 1                             
060100               UNTIL PGNO = 20                                            
060200               IF SAVE-IDDISTR-PREV(PGNO) NOT = ZERO                      
060300                 MOVE SAVE-IDDISTR-PREV(PGNO + 1)                         
060400                                 TO SAVE-IDDISTR-PREV(PGNO)               
060500                 MOVE SAVE-IDKUNDNR-PREV(PGNO + 1)                        
060600                                 TO SAVE-IDKUNDNR-PREV(PGNO)              
060700                 MOVE SAVE-IDKUNDRF-PREV(PGNO + 1)                        
060800                                 TO SAVE-IDKUNDRF-PREV(PGNO)              
060900                 MOVE SAVE-IDDC-PREV    (PGNO + 1)                        
061000                                 TO SAVE-IDDC-PREV    (PGNO)              
061100                 MOVE SAVE-IDORDER-PREV (PGNO + 1)                        
061200                                 TO SAVE-IDORDER-PREV (PGNO)              
061300               END-IF                                                     
061400             END-PERFORM                                                  
061410             MOVE NEJ            TO FIRST-SW                              
061500           ELSE                                                           
061600             ADD 1               TO PGNO                                  
061700           END-IF                                                         
061800         END-IF                                                           
061900       ELSE                                                               
062000         MOVE INF-LAST-PAGE      TO MED-IDMFSFEL                          
062100         CALL WMEDKONV        USING MED-WMEDAREA                          
062200         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
062300         PERFORM MFS-ROER-EJ-BILD                                         
062400       END-IF                                                             
062500     ELSE                                                                 
062600       MOVE 1                    TO PGNO                                  
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000 I-PREV-SIDA SECTION.                                                     
063100                                                                          
063200     IF SAVE-IDTRANS = '4514'                                             
063300       IF PGNO > 1                                                        
063400         COMPUTE PGNO = PGNO - 1                                          
063500         MOVE SAVE-IDDISTR-PREV(PGNO)  TO W-SEQC-IDDISTR-MIN              
063600         MOVE SAVE-IDKUNDNR-PREV(PGNO) TO W-SEQC-IDKUNDNR-MIN             
063700         MOVE SAVE-IDKUNDRF-PREV(PGNO) TO W-SEQC-IDKUNDRF-MIN             
063800       ELSE                                                               
063900         MOVE SAVE-IDDISTR-PREV(1)     TO W-SEQC-IDDISTR-MIN              
064000         MOVE SAVE-IDKUNDNR-PREV(1)    TO W-SEQC-IDKUNDNR-MIN             
064100         MOVE SAVE-IDKUNDRF-PREV(1)    TO W-SEQC-IDKUNDRF-MIN             
064110        IF PGNO = 1                                                       
064120          IF FIRST-SW = JA                                                
064130            MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                           
064140            CALL WMEDKONV    USING MED-WMEDAREA                           
064150            MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                           
064170          ELSE                                                            
064180            MOVE INF-NO-MORE-F6 TO MED-IDMFSFEL                           
064190            CALL WMEDKONV    USING MED-WMEDAREA                           
064191            MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                           
064193          END-IF                                                          
064194        END-IF                                                            
064500       END-IF                                                             
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900 E-SAMMA-SIDA SECTION.                                                    
065000                                                                          
065100     IF SAVE-IDTRANS = '4514'                                             
065200       MOVE SAVE-IDDISTR-PREV (PGNO) TO W-SEQC-IDDISTR-MIN                
065300       MOVE SAVE-IDKUNDNR-PREV(PGNO) TO W-SEQC-IDKUNDNR-MIN               
065400       MOVE SAVE-IDKUNDRF-PREV(PGNO) TO W-SEQC-IDKUNDRF-MIN               
065500     ELSE                                                                 
065510      INITIALIZE SAVE-AREA                                                
065600      MOVE 1                         TO PGNO                              
065610      MOVE JA                        TO FIRST-SW                          
065700     END-IF                                                               
065800     IF EGEN-MID                                                          
065900       MOVE +1                   TO RAD-IX                                
066000       PERFORM UNTIL RAD-IX      >  RAD-IX-MAX-13                         
066100          IF MID-IDTRANS(RAD-IX)   =  ALL '+'                             
066200             CONTINUE                                                     
066300          ELSE                                                            
066400             IF MID-IDTRANS(RAD-IX) NUMERIC                               
066500                PERFORM EA-STARTA-ANNAN-BILD                              
066600                MOVE JA             TO SW-STARTA-ANNAN-BILD               
066700                MOVE RAD-IX-MAX-13  TO RAD-IX                             
066800             END-IF                                                       
066900          END-IF                                                          
067000          ADD +1                 TO RAD-IX                                
067100       END-PERFORM                                                        
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500 EA-STARTA-ANNAN-BILD  SECTION.                                           
067600                                                                          
067700     INSPECT MID-IDKUNDNR(RAD-IX) REPLACING LEADING SPACE BY ZERO         
067800     MOVE MID-IDKUNDNR(RAD-IX)   TO MSGI-IDKUNDNR                         
067900     INSPECT MID-IDORDNR7(RAD-IX) REPLACING LEADING SPACE BY ZERO         
068000     IF  MID-IDORDNR7(RAD-IX)    NUMERIC                                  
068100        MOVE MID-IDORDNR7(RAD-IX) TO MSGI-IDKUNDRF(1:7)                   
068200     END-IF                                                               
068300     MOVE '001'                  TO MSGI-KDCALL                           
068400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
068500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
068600                                                                          
068700     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
068800     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
068900     MOVE MID-IDTRANS(RAD-IX) (1:1) TO W-HOPP-IDTRANS-2                   
069000     MOVE MID-IDTRANS(RAD-IX) (2:3) TO W-HOPP-IDTRANS-4-6                 
069100     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
069200     MOVE '4514'                 TO P-TO-P-IDTRANS                        
069300     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
069400                                                                          
069500     PERFORM S01-INSERT-ALTMSG                                            
069600     .                                                                    
069700     EJECT                                                                
069800 F-LAES-VISA-INFO SECTION.                                                
069900                                                                          
070000     PERFORM IMS-GU-ORQL01-CSQ-INTERV                                     
070100                                                                          
070200     IF SEGMENT-SAKNAS                                                    
070300       MOVE    FELM-DISTR-KUND-SAKNAS-040 TO    MED-IDMFSFEL              
070400       CALL    WMEDKONV                   USING MED-WMEDAREA              
070500       MOVE    MED-MFSFEL                 TO    MOD-TEMFSFEL              
070600       MOVE    MFS-RENSA-FAELT            TO    MOD-TEMFSINF              
070700       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
070800     ELSE                                                                 
070900       MOVE    SEQC-IDDISTR      TO SAVE-IDDISTR-PREV(PGNO)               
071000       MOVE    SEQC-IDKUNDNR     TO SAVE-IDKUNDNR-PREV(PGNO)              
071100       MOVE    SEQC-IDKUNDRF     TO SAVE-IDKUNDRF-PREV(PGNO)              
071200       MOVE    SEQC-IDORDER      TO SAVE-IDORDER-PREV(PGNO)               
071300                                                                          
071400       PERFORM FA-LAES-RADDATA                                            
071500       PERFORM FB-SPARA-BLADDRINGS-NYCKLAR                                
071600       PERFORM FC-BLANKA-BLADDR-RADER-RESTEN                              
071700                                                                          
071800     END-IF                                                               
071900                                                                          
072000     MOVE '002'                  TO MSGI-KDCALL                           
072100     MOVE '4514'                 TO MSGI-IDTRANS                          
072200     MOVE '4514'                 TO SAVE-IDTRANS                          
072300     MOVE SAVE-AREA              TO MSGI-SPAR-AREA                        
072400     CALL W005INIT            USING MSGI-WMSGINIT USEA-PCB                
072500     .                                                                    
072600     EJECT                                                                
072700 FA-LAES-RADDATA SECTION.                                                 
072800                                                                          
072900     MOVE    +1 TO RAD-IX                                                 
073000                                                                          
073100     PERFORM UNTIL                                                        
073200             SEGMENT-SAKNAS              OR                               
073300             BASEN-SLUT                  OR                               
073400             RAD-IX         > RAD-IX-MAX-13                               
073500                                                                          
073600       PERFORM FAA-SPARA-SEQC-INFO                                        
073700                                                                          
073800       PERFORM IMS-GET-ORQI01-OHUV                                        
073900       MOVE    OHUV-KDORDKL       TO MOD-KDORDKL-RAD(RAD-IX)              
074000                                     SPAR-OHUV-KDORDKL                    
074100                                                                          
074200       IF OHUV-KDTPOTYP      = ZERO                                       
074300                                                                          
074400         PERFORM S11-LAS-ARBETSTABELL                                     
074500         IF SEGMENT-FINNS                                                 
074600           MOVE ARB-DATRPAVT TO SPAR-ARB-DATRPAVT                         
074700           MOVE    RAD-IX TO HELP-RAD-IX                                  
074800           PERFORM FAB-BEHANDLA-ARBTAB-INFO                               
074900           IF RAD-IX = HELP-RAD-IX                                        
075000             MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-RAD(RAD-IX)             
075100           END-IF                                                         
075200         ELSE                                                             
075300           MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-RAD(RAD-IX)               
075400         END-IF                                                           
075500       ELSE                                                               
075600         MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-RAD(RAD-IX)                 
075700       END-IF                                                             
075800                                                                          
075900       PERFORM IMS-GN-ORQL01-CSQ-INTERV                                   
076000     END-PERFORM                                                          
076100                                                                          
076200     IF RAD-IX = +1                                                       
076300        MOVE    FELM-INFO-SAKNAS-413 TO    MED-IDMFSINF                   
076400        CALL    WMEDKONV             USING MED-WMEDAREA                   
076500        MOVE    MED-MFSINF           TO    MOD-TEMFSINF                   
076600     ELSE                                                                 
076700        MOVE MOD-IDDC-RAD(1)        TO    SAVE-IDDC-PREV(PGNO)            
076800                                                                          
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 FAA-SPARA-SEQC-INFO SECTION.                                             
077300       MOVE SEQC-IDKUNDNR TO  HELP-IDKUNDNR                               
077400                              SPAR-SEQC-IDKUNDNR                          
077500       MOVE HELP-IDKUNDNR TO  MOD-IDKUNDNR-RAD(RAD-IX)                    
077600       MOVE SEQC-IDORDNR7 TO  MOD-IDORDNR7-RAD(RAD-IX)                    
077700                              SPAR-SEQC-IDKUNDRF                          
077800       INSPECT MOD-IDORDNR7-RAD(RAD-IX)                                   
077900                             REPLACING LEADING ZERO BY SPACE              
078000       MOVE SEQC-IDORDER TO   W-OHUV-IDORDER                              
078100     .                                                                    
078200     EJECT                                                                
078300 FAB-BEHANDLA-ARBTAB-INFO SECTION.                                        
078400                                                                          
078500     PERFORM UNTIL SEGMENT-SAKNAS           OR                            
078600                   HELP-RAD-IX > RAD-IX-MAX-13                            
078700                                                                          
078800       MOVE ARB-IDDC     TO SAVE-IDDC-NEXT                                
078900       PERFORM FABX-KOLLA-OM-ORDERRADER-FINNS                             
079000       IF SW-ORDERRADER-FINNS = JA                                        
079100         IF SPAR-KDFRAKT = ZERO     OR                                    
079200            SPAR-KDFRAKT = ARB-KDFRAKT                                    
079300                                                                          
079400           PERFORM FABA-TA-HAND-ARB-TAB-INFO                              
079500                                                                          
079600           IF ARB-IDTRP NOT = K-IDTRP-ZERO AND                            
079700              ARB-IDTRP NOT = SPACE                                       
079800             PERFORM FABB-HAMTA-TRPDEST                                   
079900             PERFORM FABC-HAMTA-TRANSPORTOR                               
080000           END-IF                                                         
080100                                                                          
080200*          IF RAD-IX > HELP-RAD-IX                                        
080300             MOVE SPAR-SEQC-IDKUNDNR TO HELP-IDKUNDNR                     
080400             MOVE HELP-IDKUNDNR      TO MOD-IDKUNDNR-RAD(RAD-IX)          
080500             MOVE SPAR-SEQC-IDKUNDRF(1:7) TO                              
080600                                         MOD-IDORDNR7-RAD(RAD-IX)         
080700             INSPECT MOD-IDORDNR7-RAD(RAD-IX)                             
080800                                 REPLACING LEADING ZERO BY SPACE          
080900             MOVE SPAR-OHUV-KDORDKL  TO MOD-KDORDKL-RAD (RAD-IX)          
081000*          END-IF                                                         
081100           ADD +1 TO RAD-IX                                               
081200         END-IF                                                           
081300       ELSE                                                               
081400         PERFORM FABD-RAD-EJ-GODKAND                                      
081500       END-IF                                                             
081600       PERFORM S11-LAS-ARBETSTABELL                                       
081700       IF SEGMENT-FINNS                                                   
081800         MOVE ARB-DATRPAVT TO SPAR-ARB-DATRPAVT                           
081900       END-IF                                                             
082000     END-PERFORM                                                          
082100     .                                                                    
082200     EJECT                                                                
082300 FABX-KOLLA-OM-ORDERRADER-FINNS SECTION.                                  
082400                                                                          
082500     MOVE NEJ TO SW-ORDERRADER-FINNS                                      
082600                                                                          
082700     IF ARB-IDRADNR-SISTA > ZERO                                          
082800       MOVE JA TO SW-ORDERRADER-FINNS                                     
082900     ELSE                                                                 
082901                                                                          
082902       MOVE ARB-IDDC     TO W-IDDC                                        
082910       PERFORM IMS-GNP-ORQI21                                             
082920       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
082921                     SW-ORDERRADER-FINNS = JA                             
082922                                                                          
082923          IF LOR-KVRADER     > ZERO                                       
082924            MOVE JA TO SW-ORDERRADER-FINNS                                
082925          END-IF                                                          
082926                                                                          
082940          PERFORM IMS-GNP-ORQI21                                          
082950       END-PERFORM                                                        
084100                                                                          
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 FABA-TA-HAND-ARB-TAB-INFO SECTION.                                       
084600                                                                          
084700     MOVE    ARB-IDDC     TO MOD-IDDC-RAD(RAD-IX)                         
084800                             W-4431-IDDC                                  
084900                             W-4433-IDDC                                  
085000     MOVE    ARB-KDFRAKT  TO HELP-KDFRAKT                                 
085100     MOVE    HELP-KDFRAKT TO MOD-KDFRAKT-RAD (RAD-IX)                     
085200     MOVE    ARB-IDTRP    TO MOD-IDTRP-RAD   (RAD-IX)                     
085300                             W-4432-IDTRP                                 
085400                             W-4434-IDTRP                                 
085500                                                                          
085600     PERFORM FABAA-KONVERTERA-ARB-TITRPAVT                                
085700     .                                                                    
085800     EJECT                                                                
085900 FABAA-KONVERTERA-ARB-TITRPAVT SECTION.                                   
086000                                                                          
086100     IF SPAR-ARB-DATRPAVD = ZERO                                          
086200                                                                          
086300       MOVE ZERO                   TO    HELP-RAD-TIVVD                   
086400       MOVE SPAR-ARB-TIHHMM        TO    HELP-RAD-TIHHMM                  
086500                                                                          
086600       MOVE HELP-RAD-TITRPAVG      TO    SPAR-TITRPAVG                    
086700                                         MOD-TITRPAVG-RAD                 
086800                                                     (RAD-IX)             
086900                                                                          
087000     ELSE                                                                 
087100       MOVE SPAR-ARB-DATRPAVD      TO    SPAR-DATRPAVT-DATRPAVD           
087200       MOVE SPAR-ARB-TIHHMM        TO    SPAR-DATRPAVT-TIHHMM             
087300       MOVE 'AAMMDD'               TO    DAT-KDDATFORM                    
087400       MOVE SPAR-DATRPAVT-DATRPAVD TO    DAT-I-TIDATUM                    
087500       CALL WDATKONV               USING DAT-KDDATFORM                    
087600                                         DAT-I-TIDATUM                    
087700                                         DAT-O-TIDATUM                    
087800                                         DAT-KDSVAR                       
087900       IF DAT-KDSVAR-OK                                                   
088000         MOVE DAT-TIAAVVD          TO    HELP-TITRPAVT-TIAAVVD            
088100         MOVE HELP-TITRPAVT-TIVVD  TO    HELP-RAD-TIVVD                   
088200         MOVE SPAR-DATRPAVT-TIHHMM TO    HELP-RAD-TIHHMM                  
088300         MOVE HELP-RAD-TITRPAVG    TO    SPAR-TITRPAVG                    
088400                                         MOD-TITRPAVG-RAD                 
088500                                                     (RAD-IX)             
088600       ELSE                                                               
088700         MOVE FELT-WDATKONV-TITRPAVT TO FELTEXT-VID-CALL-ABEND            
088800         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
088900       END-IF                                                             
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 FABB-HAMTA-TRPDEST SECTION.                                              
089400                                                                          
089500     PERFORM IMS-GU-XXKA-4431-4432                                        
089600                                                                          
089700     MOVE    4432-BETRPDST TO MOD-BETRPDST-RAD(RAD-IX)                    
089800     .                                                                    
089900     EJECT                                                                
090000 FABC-HAMTA-TRANSPORTOR SECTION.                                          
090100                                                                          
090200     IF SPAR-ARB-DATRPAVD = ZERO AND                                      
090300        SPAR-ARB-TIHHMM   = ZERO                                          
090400       MOVE SPACE                  TO MOD-BETRPFIR-RAD(RAD-IX)            
090500     ELSE                                                                 
090600                                                                          
090700       MOVE    ZERO                TO HELP-TITRPAVG-NUM                   
090800       MOVE    SPAR-TITRPAVG-HHMM  TO HELP-TITRPAVG-HHMM                  
090900       MOVE    HELP-TITRPAVG-NUM   TO W-4434-TITRPAVG                     
091000       PERFORM IMS-GU-XXKB-4433-4434                                      
091100                                                                          
091200       IF SEGMENT-FINNS                                                   
091300         MOVE    4434-BETRPFIR     TO MOD-BETRPFIR-RAD(RAD-IX)            
091400       ELSE                                                               
091500         IF SPAR-ARB-DATRPAVD = ZERO                                      
091600           MOVE SPACE              TO MOD-BETRPFIR-RAD(RAD-IX)            
091700         ELSE                                                             
091800           MOVE    SPAR-TITRPAVG-VV TO HELP-TITRPAVG-VV                   
091900           MOVE    HELP-TITRPAVG-NUM                                      
092000                                   TO W-4434-TITRPAVG                     
092100           PERFORM IMS-GU-XXKB-4433-4434                                  
092200           IF SEGMENT-FINNS                                               
092300             MOVE    4434-BETRPFIR TO MOD-BETRPFIR-RAD(RAD-IX)            
092400           ELSE                                                           
092500             MOVE    SPAR-TITRPAVG-A TO HELP-TITRPAVG-A                   
092600             MOVE    HELP-TITRPAVG-NUM                                    
092700                                   TO W-4434-TITRPAVG                     
092800             PERFORM IMS-GU-XXKB-4433-4434                                
092900             IF SEGMENT-FINNS                                             
093000               MOVE 4434-BETRPFIR  TO MOD-BETRPFIR-RAD(RAD-IX)            
093100             ELSE                                                         
093200               MOVE    ZERO        TO HELP-TITRPAVG-VV                    
093300               MOVE    HELP-TITRPAVG-NUM                                  
093400                                   TO W-4434-TITRPAVG                     
093500               PERFORM IMS-GU-XXKB-4433-4434                              
093600               IF SEGMENT-FINNS                                           
093700                 MOVE 4434-BETRPFIR                                       
093800                                   TO MOD-BETRPFIR-RAD(RAD-IX)            
093900               ELSE                                                       
094000                 MOVE SPACE        TO MOD-BETRPFIR-RAD(RAD-IX)            
094100               END-IF                                                     
094200             END-IF                                                       
094300           END-IF                                                         
094400         END-IF                                                           
094500       END-IF                                                             
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 FABD-RAD-EJ-GODKAND SECTION.                                             
095000                                                                          
095100     MOVE MFS-RENSA-FAELT TO                                              
095200                             MOD-IDKUNDNR-RAD(RAD-IX)                     
095300                             MOD-IDORDNR7-RAD(RAD-IX)                     
095400                             MOD-IDDC-RAD(RAD-IX)                         
095500                             MOD-KDFRAKT-RAD (RAD-IX)                     
095600                             MOD-KDORDKL-RAD (RAD-IX)                     
095700                             MOD-IDTRP-RAD   (RAD-IX)                     
095800                             MOD-BETRPDST-RAD(RAD-IX)                     
095900                             MOD-BETRPFIR-RAD(RAD-IX)                     
096000                             MOD-TITRPAVG-RAD(RAD-IX)                     
096100     .                                                                    
096200     EJECT                                                                
096300 FB-SPARA-BLADDRINGS-NYCKLAR SECTION.                                     
096400                                                                          
096500     IF SEGMENT-FINNS              AND                                    
096600        RAD-IX NOT < RAD-IX-MAX-13                                        
096700       MOVE SEQC-IDDISTR         TO SAVE-IDDISTR-NEXT                     
096800       MOVE SEQC-IDKUNDNR        TO SAVE-IDKUNDNR-NEXT                    
096900       MOVE SEQC-IDKUNDRF        TO SAVE-IDKUNDRF-NEXT                    
097000       MOVE SEQC-IDORDER         TO SAVE-IDORDER-NEXT                     
097100       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
097200       CALL WMEDKONV          USING MED-WMEDAREA                          
097300       MOVE MED-TEMFSINF         TO MOD-TEMFSINF                          
097400     ELSE                                                                 
097500       MOVE INF-LAST-PAGE        TO MED-IDMFSFEL                          
097600       CALL WMEDKONV          USING MED-WMEDAREA                          
097700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100 S01-INSERT-ALTMSG SECTION.                                               
098200                                                                          
098300     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
098400     PERFORM IMS-CHANGE-ALTMSG                                            
098500     IF STATUS-OK                                                         
098600       PERFORM IMS-INSERT-ALTMSG                                          
098700     ELSE                                                                 
098800       MOVE LOW-VALUE          TO MSG-AREA                                
098900       MOVE 'W4O51101'         TO MFS-IDMOD                               
099000       MOVE '4514'             TO MOD-IDTRANS                             
099100       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
099200       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
099300       IF SECURITY-FEL                                                    
099400         STRING 'NOT AUTHORIZED TO USE '                                  
099500                W-BILD                                                    
099600                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
099700       ELSE                                                               
099800         STRING 'WRONG PICTURE '                                          
099900                 W-BILD                                                   
100000                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
100100       END-IF                                                             
100200       PERFORM MFS-ROER-EJ-BILD                                           
100300       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51401 + 4                      
100400       PERFORM IMS-INSERT-MSG                                             
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 FC-BLANKA-BLADDR-RADER-RESTEN SECTION.                                   
100900                                                                          
101000     PERFORM UNTIL RAD-IX > RAD-IX-MAX-13                                 
101100       MOVE MFS-RENSA-FAELT TO                                            
101200                             MOD-IDKUNDNR-RAD(RAD-IX)                     
101300                             MOD-IDORDNR7-RAD(RAD-IX)                     
101400                             MOD-IDDC-RAD(RAD-IX)                         
101500                             MOD-KDFRAKT-RAD (RAD-IX)                     
101600                             MOD-KDORDKL-RAD (RAD-IX)                     
101700                             MOD-IDTRP-RAD   (RAD-IX)                     
101800                             MOD-BETRPDST-RAD(RAD-IX)                     
101900                             MOD-BETRPFIR-RAD(RAD-IX)                     
102000                             MOD-TITRPAVG-RAD(RAD-IX)                     
102100       ADD  +1              TO RAD-IX                                     
102200     END-PERFORM                                                          
102300     .                                                                    
102400     EJECT                                                                
102500 S11-LAS-ARBETSTABELL SECTION.                                            
102600                                                                          
102700     IF MFS-NEXT               AND                                        
102800        RAD-IX                = +1                                        
102900                                                                          
103000       IF SAVE-IDDC-NEXT      NOT = SPACE                                 
103100         MOVE  SAVE-IDDC-NEXT TO W-IDDC                                   
103200         PERFORM IMS-GNP-ORQI12-KVAL-ARB                                  
103300       ELSE                                                               
103400         IF SPAR-IDDC = SPACE                                             
103500           PERFORM IMS-GNP-ORQI12-OKVAL-ARB                               
103600         ELSE                                                             
103700           MOVE SPAR-IDDC     TO W-IDDC                                   
103800           PERFORM IMS-GNP-ORQI12-KVAL-ARB                                
103900         END-IF                                                           
104000       END-IF                                                             
104100     ELSE                                                                 
104200       IF (MFS-ENTER OR MFS-PREVIOUS)  AND                                
104300          RAD-IX                 = +1  AND                                
104400          SAVE-IDDC-PREV(PGNO)  NOT = SPACE                               
104500         MOVE SAVE-IDDC-PREV(PGNO) TO W-IDDC                              
104600         PERFORM IMS-GNP-ORQI12-KVAL-ARB                                  
104700       ELSE                                                               
104800         IF SPAR-IDDC     = SPACE                                         
104900           PERFORM IMS-GNP-ORQI12-OKVAL-ARB                               
105000         ELSE                                                             
105100           MOVE    SPAR-IDDC TO W-IDDC                                    
105200           PERFORM IMS-GNP-ORQI12-KVAL-ARB                                
105300         END-IF                                                           
105400       END-IF                                                             
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
105900                                                                          
106000     MOVE +1 TO RAD-IX                                                    
106100     PERFORM UNTIL RAD-IX > RAD-IX-MAX-13                                 
106200       MOVE MFS-RENSA-FAELT TO                                            
106300                             MOD-IDKUNDNR-RAD(RAD-IX)                     
106400                             MOD-IDORDNR7-RAD(RAD-IX)                     
106500                             MOD-IDDC-RAD(RAD-IX)                         
106600                             MOD-KDFRAKT-RAD (RAD-IX)                     
106700                             MOD-KDORDKL-RAD (RAD-IX)                     
106800                             MOD-IDTRP-RAD   (RAD-IX)                     
106900                             MOD-BETRPDST-RAD(RAD-IX)                     
107000                             MOD-BETRPFIR-RAD(RAD-IX)                     
107100                             MOD-TITRPAVG-RAD(RAD-IX)                     
107200       ADD  +1              TO RAD-IX                                     
107300     END-PERFORM                                                          
107400     .                                                                    
107500     EJECT                                                                
107600 MFS-ROER-EJ-BILD SECTION.                                                
107700                                                                          
107800     MOVE +1 TO RAD-IX                                                    
107900     PERFORM UNTIL RAD-IX > RAD-IX-MAX-13                                 
108000       MOVE MFS-ROER-EJ-FAELT TO                                          
108100                             MOD-IDKUNDNR-RAD(RAD-IX)                     
108200                             MOD-IDORDNR7-RAD(RAD-IX)                     
108300                             MOD-IDDC-RAD(RAD-IX)                         
108400                             MOD-KDFRAKT-RAD (RAD-IX)                     
108500                             MOD-KDORDKL-RAD (RAD-IX)                     
108600                             MOD-IDTRP-RAD   (RAD-IX)                     
108700                             MOD-BETRPDST-RAD(RAD-IX)                     
108800                             MOD-BETRPFIR-RAD(RAD-IX)                     
108900                             MOD-TITRPAVG-RAD(RAD-IX)                     
109000       ADD  +1              TO RAD-IX                                     
109100     END-PERFORM                                                          
109200     .                                                                    
109300     EJECT                                                                
109400*                                                                         
109500*                                                                         
109600*                                                                         
109700*                  IIIIIIIII  MMMMMMMMM  SSSSSSSSS                        
109800*                  II     II  M MMMMM M  SSS   SSS                        
109900*                  IIII IIII  M  MMM  M  SS SSS SS                        
110000*                  IIII IIII  M M M M M  SS  SSSSS                        
110100*                  IIII IIII  M MM MM M  SSSSS  SS                        
110200*                  IIII IIII  M MMMMM M  SS SSS SS                        
110300*                  II     II  M MMMMM M  SSS   SSS                        
110400*                  IIIIIIIII  MMMMMMMMM  SSSSSSSSS                        
110500*                                                                         
110600*                                                                         
110700*                                                                         
110800 IMS-GET-MSG SECTION.                                                     
110900                                                                          
111000     MOVE    '  QC'          TO    GODK-STATUSKODER                       
111100     CALL    CBLTDLI         USING GU MSG-PCB MSG-IO-AREA                 
111200     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500                                                                          
111600 IMS-INSERT-MSG SECTION.                                                  
111700                                                                          
111800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
111900       MOVE '0' TO MFS-KDHUVOMR                                           
112000     END-IF                                                               
112100     MOVE    LOW-VALUE       TO    MSG-KDZ1 MSG-KDZ2                      
112200     MOVE    SPACE           TO    GODK-STATUSKODER                       
112300     CALL    CBLTDLI         USING ISRT MSG-PCB MSG-IO-AREA               
112400                                        MFS-IDMOD                         
112500     MOVE    MSG-STATUS-CODE TO STATUS-WS                                 
112600     PERFORM IMS-STATUSKONTROLL                                           
112700     .                                                                    
112800     EJECT                                                                
112900 IMS-CHANGE-ALTMSG SECTION.                                               
113000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
113100     MOVE '  A1A4' TO GODK-STATUSKODER                                    
113200     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
113300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
113400     PERFORM IMS-STATUSKONTROLL                                           
113500     .                                                                    
113600     SKIP3                                                                
113700 IMS-INSERT-ALTMSG SECTION.                                               
113800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
113900     MOVE SPACE TO GODK-STATUSKODER                                       
114000     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
114100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
114200     PERFORM IMS-STATUSKONTROLL                                           
114300     .                                                                    
114400     EJECT                                                                
114500 IMS-GET-ORQI01-OHUV SECTION.                                             
114600                                                                          
114700     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
114800          DELIMITED BY SIZE INTO SSA1                                     
114900     MOVE    '  '             TO    GODK-STATUSKODER                      
115000     CALL    CBLTDLI          USING GU ORQI-PCB                           
115100                                    DLI-IO-AREA SSA1                      
115200     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
115300     PERFORM IMS-STATUSKONTROLL                                           
115400     .                                                                    
115500                                                                          
115600 IMS-GNP-ORQI12-KVAL-ARB SECTION.                                         
115700                                                                          
115800     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
115900          DELIMITED BY SIZE INTO SSA1                                     
116000     MOVE    '  GE'           TO    GODK-STATUSKODER                      
116100     CALL    CBLTDLI          USING GNP ORQI-PCB                          
116200                                    DLI-IO-AREA SSA1                      
116300     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600                                                                          
116700 IMS-GNP-ORQI12-OKVAL-ARB SECTION.                                        
116800                                                                          
116900     MOVE   'WLORQI12'        TO    SSA1                                  
117000     MOVE    '  GE'           TO    GODK-STATUSKODER                      
117100     CALL    CBLTDLI          USING GNP ORQI-PCB                          
117200                                    DLI-IO-AREA SSA1                      
117300     MOVE    ORQI-STATUS-CODE TO    STATUS-WS                             
117400     PERFORM IMS-STATUSKONTROLL                                           
117500     .                                                                    
117600     EJECT                                                                
117691 IMS-GNP-ORQI21 SECTION.                                                  
117692                                                                          
117693     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
117694            DELIMITED BY SIZE INTO SSA1                                   
117695     MOVE   'WLORQI21'          TO SSA2                                   
117696     MOVE   '  GE'              TO GODK-STATUSKODER                       
117697     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-Q221 SSA1                
117698     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
117699     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117701     EJECT                                                                
117710 IMS-GU-ORQL01-CSQ-INTERV SECTION.                                        
117800                                                                          
117900     STRING 'WLORQL01(WDQ2C1KY>=' W-SEQC-WDQ2C1KY-MIN-X                   
118000                    '&WDQ2C1KY<=' W-SEQC-WDQ2C1KY-MAX-X                   
118100                    '&FLBORT   =N)'                                       
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE    '  GE'              TO GODK-STATUSKODER                      
118400     CALL    CBLTDLI          USING GU  ORQLCSQ-PCB                       
118500                                        DLI-IO-AREA SSA1                  
118600     MOVE    ORQLCSQ-STATUS-CODE TO STATUS-WS                             
118700     PERFORM IMS-STATUSKONTROLL                                           
118800     .                                                                    
118900     SKIP3                                                                
119000 IMS-GN-ORQL01-CSQ-INTERV SECTION.                                        
119100                                                                          
119200     STRING 'WLORQL01(WDQ2C1KY>=' W-SEQC-WDQ2C1KY-MIN-X                   
119300                    '&WDQ2C1KY<=' W-SEQC-WDQ2C1KY-MAX-X                   
119400                    '&FLBORT   =N)'                                       
119500          DELIMITED BY SIZE INTO SSA1                                     
119600     MOVE    '  GEGB'            TO GODK-STATUSKODER                      
119700     CALL    CBLTDLI          USING GN  ORQLCSQ-PCB                       
119800                                        DLI-IO-AREA SSA1                  
119900     MOVE    ORQLCSQ-STATUS-CODE TO STATUS-WS                             
120000     PERFORM IMS-STATUSKONTROLL                                           
120100     .                                                                    
120200     EJECT                                                                
120300 IMS-GU-XXKA-4431-4432 SECTION.                                           
120400                                                                          
120500     STRING 'WLXXKA01(WDGXKEY  =' W-4431-WDGXKEY-X ')'                    
120600          DELIMITED BY SIZE INTO SSA1                                     
120700                                                                          
120800     STRING 'WLXXKA11(WDGXKEY  =' W-4432-WDGXKEY-X ')'                    
120900          DELIMITED BY SIZE INTO SSA2                                     
121000     MOVE    '  '             TO    GODK-STATUSKODER                      
121100     CALL    CBLTDLI          USING GU  XXKA-PCB DLI-IO-AREA              
121200                                        SSA1     SSA2                     
121300     MOVE    XXKA-STATUS-CODE TO    STATUS-WS                             
121400     PERFORM IMS-STATUSKONTROLL                                           
121500     .                                                                    
121600     EJECT                                                                
121700 IMS-GU-XXKB-4433-4434 SECTION.                                           
121800                                                                          
121900     STRING 'WLXXKB01(WDGXKEY  =' W-4433-WDGXKEY-X ')'                    
122000          DELIMITED BY SIZE INTO SSA1                                     
122100                                                                          
122200     STRING 'WLXXKB11(WDGXKEY  =' W-4434-WDGXKEY-X ')'                    
122300          DELIMITED BY SIZE INTO SSA2                                     
122400     MOVE    '  GE'           TO    GODK-STATUSKODER                      
122500     CALL    CBLTDLI          USING GU  XXKB-PCB DLI-IO-AREA              
122600                                        SSA1     SSA2                     
122700     MOVE    XXKB-STATUS-CODE TO    STATUS-WS                             
122800     PERFORM IMS-STATUSKONTROLL                                           
122900     .                                                                    
123000     EJECT                                                                
123100 IMS-GU-WDB601    SECTION.                                                
123200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
123300          DELIMITED BY SIZE INTO SSA1                                     
123400     MOVE '  GE' TO GODK-STATUSKODER                                      
123500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
123600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     IF SEGMENT-SAKNAS                                                    
123900         MOVE SPACE TO DCS-KDDC                                           
124000     END-IF                                                               
124100     .                                                                    
124200 IMS-STATUSKONTROLL SECTION.                                              
124300                                                                          
124400     SET    STATUS-IX TO 1                                                
124500     SEARCH GODK-STATUS                                                   
124600       AT END CALL FELLOG                                                 
124700       WHEN   GODK-STATUS (STATUS-IX) = STATUS-WS                         
124800         CONTINUE                                                         
124900     END-SEARCH                                                           
125000     .                                                                    
