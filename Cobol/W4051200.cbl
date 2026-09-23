000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4051200.                                                
000400 AUTHOR.         STEFANO GIOBBI.                                          
000500 DATE-WRITTEN.   95/02/02.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR BILDEN:  FRÅGA PÅ DISTRIKT RADER             
001100*                                                                         
001200*        DATABASER SOM BEARBETAS.                                         
001300*                                                                         
001400*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001500*        PROGRAMMET LÄSER      WLORQI (WDQ2)                              
001600*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
001700*        PROGRAMMET LÄSER      WDE6                                       
001800*        PROGRAMMET LÄSER      WDB6                                       
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W4T512                                              
002300*        MID:         W4I51201                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W4O51201                                            
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJEcT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4051200'.            
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004000 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
004200 77  MAX-INDX                    PIC S9(3)  VALUE +14   COMP SYNC.        
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
004600                                                                          
004700                                                                          
004800*      --- VALID IDDC CODES                                               
004900*                                                                         
005000*01    -COPY WWDCKONS                                                     
005100       EJECT                                                              
005200 77  WS-IDPRODNR-NUM             PIC S9(7)   VALUE +0     COMP-3.         
005300 77  WS-KVRADER-UTSKR            PIC 9(6)    VALUE ZERO.                  
005400 77  WS-KVRADER-PACK             PIC 9(6)    VALUE ZERO.                  
005500 77  WS-KVRADER-REG              PIC 9(6)    VALUE ZERO.                  
005600 77  WS-KVRADER-REG-Q2           PIC 9(6)    VALUE ZERO.                  
005700 77  WS-KVRADER-REG-E6           PIC 9(6)    VALUE ZERO.                  
005800 77  WS-ODEL-KDODELSTA           PIC X       VALUE SPACE.                 
005810 77  W-GE-WDQ212                 PIC X(1)    VALUE 'N'.                   
005900 77  SPAR-STATUS-WS              PIC X(2)    VALUE SPACE.                 
006000                                                                          
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006600     88  ALLT-OK                             VALUE 'J'.                   
006700                                                                          
006800 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
006900                                                                          
007000 77  SW-DIREKT-LEV               PIC X       VALUE 'N'.                   
007100     88  DIREKT-LEV                          VALUE 'J'.                   
007200                                                                          
007300 77  SW-NEXT-NYCKLAR-FINNS       PIC X(1)    VALUE 'N'.                   
007400     88  NEXT-NYCKLAR-FINNS                  VALUE 'J'.                   
007500     88  NEXT-NYCKLAR-SAKNAS                 VALUE 'N'.                   
007600                                                                          
007700 77  WDQ3-STATUS                 PIC X(2)    VALUE SPACE.                 
007800     88  Q3-SEGMENT-FINNS                    VALUE '  '.                  
007900     88  Q3-SEGMENT-SAKNAS                   VALUE 'GE'.                  
008000     88  Q3-BASEN-SLUT                       VALUE 'GB'.                  
008100                                                                          
008200 77  WDE6-STATUS                 PIC X(2)    VALUE SPACE.                 
008300     88  E6-SEGMENT-FINNS                    VALUE '  '.                  
008400     88  E6-SEGMENT-SAKNAS                   VALUE 'GE'.                  
008500                                                                          
008600 01  ORDERDEL-STATUS.                                                     
008700     03  ORDERDEL-STATUS-1       PIC X       VALUE SPACE.                 
008800     03  ORDERDEL-STATUS-2       PIC X       VALUE SPACE.                 
008900                                                                          
009000 77  WDQ301-SW                   PIC X       VALUE 'N'.                   
009100     88  WDQ301-SAKNAS                       VALUE 'J'.                   
009200                                                                          
009300                                                                          
009400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009500     88  EGEN-MID                            VALUE '4512'.                
009600     88  GODK-MID                            VALUE '4511' '4512'          
009700                                                   '4513' '4514'          
009800                                                   '4515' '4516'          
009900                                                   '4517' '4518'          
010000                                                   '4519'.                
010100     88  HELP-MID                            VALUE '0551'.                
010200                                                                          
010300 01  HELP-PARAMETRAR.                                                     
010400     03  HELP-KDORDSTA           PIC X(2)    VALUE SPACE.                 
010500                                                                          
010600                                                                          
010700     EJECT                                                                
010800                                                                          
010900 01  TEST-IDDISTR              PIC S9(5)    COMP-3.                       
011000 01  FILLER REDEFINES TEST-IDDISTR.                                       
011100*    03     -COPY WWDIST35.                                               
011200     EJECT                                                                
011300 01  FILLER REDEFINES TEST-IDDISTR.                                       
011400*    03     -COPY WWDIST40.                                               
011500     EJECT                                                                
011600 01  FILLER REDEFINES TEST-IDDISTR.                                       
011700*    03     -COPY WWDIS134.                                               
011800     EJECT                                                                
011900                                                                          
012000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012100 01  GENERELLA-SUBPROGRAM.                                                
012200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012800*01 -COPY WMSGINIT                                                        
012900     EJECT                                                                
013000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013100*   -COPY WMEDAREA                                                        
013200     SKIP3                                                                
013300 01  MESSAGE-CODES.                                                       
013400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013600     03  INF-NO-MORE-F6          PIC X(3)    VALUE '368'.                 
013700     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
013800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013900     EJECT                                                                
014000*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
014100   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
014200     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
014300                                                                          
014400 01  BILD-HOPP-AREOR.                                                     
014500                                                                          
014600   03    W-BILD               PIC X(4)    VALUE SPACE.                    
014700   03    W-HOPP-IDTRANS.                                                  
014800     05  FILLER               PIC X(1)    VALUE 'W'.                      
014900     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
015000     05  FILLER               PIC X(1)    VALUE 'T'.                      
015100     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
015200     05  FILLER               PIC X(2)    VALUE SPACE.                    
015300                                                                          
015400                                                                          
015500   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
015600   03      P-TO-P-SW.                                                     
015700                                                                          
015800     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
015900     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
016000     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
016100     05  P-TO-P-KDTRANS          PIC X(8).                                
016200     05  P-TO-P-IDTRANS          PIC X(4).                                
016300     05  P-TO-P-KDMFSFOR         PIC X(1).                                
016400     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
016500                                                                          
016600     EJECT                                                                
016700                                                                          
016800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016900*                                                                         
017000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017100     SKIP3                                                                
017200*01  MID -COPY W4I51201                                                   
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017500     SKIP3                                                                
017600*01  -COPY WMSGAREA                                                       
017700     EJECT                                                                
017800*    03  MOD -COPY W4O51201              -RED MSG-AREA.                   
017900     EJECT                                                                
018000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018100     SKIP3                                                                
018200*01  -COPY WMFSAREA                                                       
018300     EJECT                                                                
018400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018500*                                                                         
018600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018700     SKIP3                                                                
018800                                                                          
018900 01  SAVE-AREA.                                                           
019000   03  SAVE-IDTRANS              PIC X(4)    VALUE SPACE.                 
019100   03  PGNO                      PIC 9(2).                                
019200   03  FIRST-SW                  PIC X        VALUE 'J'.                  
019300   03  SAVE-AREA-PREV OCCURS 20.                                          
019400         05  SAVE-IDKUNDNR-PREV   PIC 9(7)    VALUE ZERO.                 
019500         05  SAVE-IDPRODNR-PREV   PIC 9(7)    VALUE ZERO.                 
019600         05  SAVE-IDORDNR7-PREV   PIC 9(7)    VALUE ZERO.                 
019700         05  SAVE-IDDC-PREV       PIC X(2)    VALUE SPACE.                
019800   03  SAVE-AREA-NEXT.                                                    
019900         05  SAVE-IDKUNDNR-NEXT   PIC 9(7)    VALUE ZERO.                 
020000         05  SAVE-IDPRODNR-NEXT   PIC 9(7)    VALUE ZERO.                 
020100         05  SAVE-IDORDNR7-NEXT   PIC 9(7)    VALUE ZERO.                 
020200         05  SAVE-IDDC-NEXT       PIC X(2)    VALUE SPACE.                
020300                                                                          
020400 01  NYCKLAR-TILL-DLI.                                                    
020500     03  W-IDDISTR-X.                                                     
020600         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
020700                                                                          
020800     03  W-IDKUNDNR-X.                                                    
020900         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
021000                                                                          
021100     03  W-IDDC-X.                                                        
021200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021300                                                                          
021400     03  W-WDQ2CSEQ-MIN-X.                                                
021500         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
021600         05  W-IDKUNDNR-MIN-X.                                            
021700           07  W-IDKUNDNR-MIN    PIC S9(7)    VALUE ZERO COMP-3.          
021800         05  W-IDKUNDRF-MIN      PIC X(10)    VALUE LOW-VALUE.            
021900                                                                          
022000     03  W-WDQ2CSEQ-MAX-X.                                                
022100         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
022200         05  W-IDKUNDNR-MAX-X.                                            
022300           07  W-IDKUNDNR-MAX    PIC S9(7)    VALUE ZERO COMP-3.          
022400         05  FILLER              PIC  X(10)   VALUE HIGH-VALUE.           
022500                                                                          
022600     03  W-WDQ211KY-MIN-X.                                                
022700         05  W-IDDC-Q211-MIN     PIC X(2).                                
022800         05  W-IDLEVNR-MIN       PIC X(5).                                
022900                                                                          
023000     03  W-WDQ211KY-MAX-X.                                                
023100         05  W-IDDC-Q211-MAX     PIC X(2).                                
023200         05  W-IDLEVNR-MAX       PIC X(5).                                
023300                                                                          
023400     03  W-WDQ301KY-MIN-X.                                                
023500         05  W-IDORDER-MIN       PIC S9(7)   COMP-3.                      
023600         05  W-IDDC-MIN          PIC X(2).                                
023700         05  W-IDPRODNR-MIN-X.                                            
023800           07  W-IDPRODNR-MIN    PIC S9(7)   COMP-3.                      
023900         05  W-IDPLKLST-MIN      PIC S9(3)   COMP-3.                      
024000                                                                          
024100     03  W-WDQ301KY-MAX-X.                                                
024200         05  W-IDORDER-MAX       PIC S9(7)   COMP-3.                      
024300         05  W-IDDC-MAX          PIC X(2).                                
024400         05  W-IDPRODNR-MAX-X.                                            
024500           07  W-IDPRODNR-MAX    PIC S9(7)   COMP-3.                      
024600         05  W-IDPLKLST-MAX      PIC S9(3)   COMP-3.                      
024700                                                                          
024800     03  W-IDPRODNR-X.                                                    
024900         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
025000     EJECT                                                                
025100                                                                          
025200     03  W-IDDC-B6-X.                                                     
025300         05 W-IDDC-B6                  PIC X(2).                          
025400                                                                          
025500     EJECT                                                                
025600                                                                          
025700*    --- STATUS-KOD FRÅN IMS                                              
025800 01  STATUS-WS                   PIC XX.                                  
025900     88  STATUS-OK                           VALUE '  '.                  
026000     88  SEGMENT-FINNS                       VALUE '  '.                  
026100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026300     88  BASEN-SLUT                          VALUE 'GB'.                  
026400     88  TRANSKOD-FEL                        VALUE 'A1'.                  
026500     88  SECURITY-FEL                        VALUE 'A4'.                  
026600     SKIP2                                                                
026700 01  GODK-STATUSKODER.                                                    
026800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026900     SKIP3                                                                
027000 01  SSA1                        PIC X(128).                              
027100 01  SSA2                        PIC X(64).                               
027200     EJECT                                                                
027300*    --- IMS FUNKTIONSKODER                                               
027400*01  -COPY W0003                                                          
027500     EJECT                                                                
027600*    ---  DLI INPUT-OUTPUT AREA                                           
027700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027800     SKIP3                                                                
027900 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
028000 01  OHUV-IO-AREA.                                                        
028100*    03  -COPY WDQ201                                                     
028200     EJECT                                                                
028300                                                                          
028400 01  FILLER                      PIC X(16)   VALUE 'WDQ211-AREA'.         
028500 01  DIRL-IO-AREA.                                                        
028600*    03  -COPY WDQ211                                                     
028700     EJECT                                                                
028800                                                                          
028900 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
029000 01  ARB-IO-AREA.                                                         
029100*    03  -COPY WDQ212                                                     
029200     EJECT                                                                
029300                                                                          
029310 01  FILLER                      PIC X(16)   VALUE 'WDQ221-AREA'.         
029320 01  LOR-IO-AREA.                                                         
029330*    03  -COPY WDQ221                                                     
029340     EJECT                                                                
029350                                                                          
029400 01  FILLER                      PIC X(16)   VALUE 'WDQ301-AREA'.         
029500 01  ODEL-IO-AREA.                                                        
029600*    03  -COPY WDQ301                                                     
029700     EJECT                                                                
029800                                                                          
029900 01  FILLER                      PIC X(16)   VALUE 'WDE601-AREA'.         
030000 01  VORD-IO-AREA.                                                        
030100*    03  -COPY WDE601                                                     
030200     EJECT                                                                
030300                                                                          
030400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030500 01   DLI-IO-AREA-B601.                                                   
030600*     03  -COPY WDB601                                                    
030700                                                                          
030800 LINKAGE SECTION.                                                         
030900*                                                                         
031000                                                                          
031100*01  -COPY W0009      -PRE MSG-                                           
031200     EJECT                                                                
031300*01  -COPY W0009      -PRE ALT-                                           
031400     EJECT                                                                
031500*01  -COPY W0008      -PRE USEA-                                          
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008      -PRE ORQI-                                          
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008      -PRE ORQA-                                          
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008      -PRE WDE6-                                          
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700*01  -COPY W0008      -PRE WDB6-                                          
032800     05  FILLER                  PIC X.                                   
032900     EJECT                                                                
033000                                                                          
033100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB  USEA-PCB                      
033200                                   ORQI-PCB                               
033300                                   ORQA-PCB WDE6-PCB WDB6-PCB.            
033400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB  USEA-PCB                      
033500                                   ORQI-PCB                               
033600                                   ORQA-PCB WDE6-PCB WDB6-PCB.            
033700                                                                          
033800     PERFORM IMS-GET-MSG                                                  
033900     IF SEGMENT-FINNS                                                     
034000       PERFORM A-INIT                                                     
034100       PERFORM B-KOLLA-NYCKLAR                                            
034200       IF NYCKLAR-OK                                                      
034300         IF MFS-FIRST                                                     
034400           PERFORM C-FOERSTA-SIDA                                         
034500         ELSE                                                             
034600           IF MFS-NEXT                                                    
034700             PERFORM D-NAESTA-SIDA                                        
034800           ELSE                                                           
034900             IF MFS-PREVIOUS                                              
035000               PERFORM I-PREV-SIDA                                        
035100             ELSE                                                         
035200               PERFORM E-SAMMA-SIDA                                       
035300             END-IF                                                       
035400           END-IF                                                         
035500         END-IF                                                           
035600         IF STARTA-ANNAN-BILD                                             
035700            CONTINUE                                                      
035800         ELSE                                                             
035900            IF ALLT-OK                                                    
036000              PERFORM F-LAES-VISA-INFO                                    
036100            END-IF                                                        
036200         END-IF                                                           
036300       END-IF                                                             
036400       IF STARTA-ANNAN-BILD                                               
036500          CONTINUE                                                        
036600       ELSE                                                               
036700          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51201 + 4                   
036800          PERFORM IMS-INSERT-MSG                                          
036900       END-IF                                                             
037000     END-IF                                                               
037100                                                                          
037200     MOVE ZERO TO RETURN-CODE                                             
037300     GOBACK                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 A-INIT SECTION.                                                          
037700                                                                          
037800     IF MSG-DUBBLA-TRANSKODER                                             
037900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I51201                 
038000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
038100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
038200     ELSE                                                                 
038300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I51201                 
038400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
038500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
038600     END-IF                                                               
038700                                                                          
038800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
038900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
039000     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
039100                                                                          
039200     MOVE LOW-VALUE                       TO MSG-AREA                     
039300                                             W-WDQ211KY-MIN-X             
039400     MOVE HIGH-VALUE                      TO W-WDQ211KY-MAX-X             
039500     MOVE 'W4O512N1'                      TO MFS-IDMOD                    
039600     MOVE '4512'                          TO MOD-IDTRANS                  
039700     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
039800                                             MOD-TEMFSINF                 
039900                                                                          
040000     IF NOT EGEN-MID                                                      
040100       MOVE SPACE TO MFS-KDTRTYP                                          
040200       MOVE '7'   TO MFS-IDPFK                                            
040300     END-IF                                                               
040400     MOVE JA      TO ALLT-SW                                              
040500                                                                          
040600     .                                                                    
040700     EJECT                                                                
040800 B-KOLLA-NYCKLAR SECTION.                                                 
040900                                                                          
041000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041100     MOVE '001'             TO MSGI-KDCALL                                
041200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041300     MOVE '4512'            TO MSGI-IDTRANS                               
041400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041500     IF EGEN-MID OR HELP-MID                                              
041600        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
041700        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
041800     END-IF                                                               
041900                                                                          
042000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042100     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
042200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
042300     MOVE      JA              TO    NYCKLAR-SW                           
042400     MOVE      LOW-VALUE       TO    W-WDQ301KY-MIN-X                     
042500     MOVE      HIGH-VALUE      TO    W-WDQ301KY-MAX-X                     
042600     PERFORM BA-KOLLA-DISTRIKT                                            
042700     PERFORM BB-KOLLA-KUNDNUMMER                                          
042800     PERFORM BC-KOLLA-IDDC                                                
042900     PERFORM BD-FLYTTA-OEVRIGA                                            
043000     IF NYCKLAR-FEL                                                       
043100       MOVE      ERR-WRONG-KEY TO    MED-IDMFSFEL                         
043200       CALL      WMEDKONV      USING MED-WMEDAREA                         
043300       MOVE      MED-MFSFEL    TO    MOD-TEMFSFEL                         
043400       PERFORM MFS-RENSA-FAELT-UT                                         
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 BA-KOLLA-DISTRIKT SECTION.                                               
044000                                                                          
044100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
044200                                                                          
044300     IF MID-IDDISTR-IN    NOT = ALL '+'                                   
044400       MOVE '7'           TO MFS-IDPFK                                    
044500       MOVE SPACE         TO MFS-KDTRTYP                                  
044600     END-IF                                                               
044700     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
044800         MOVE MSGI-IDDISTR       TO W-IDDISTR-MIN                         
044900                                    W-IDDISTR-MAX                         
045000                                    TEST-IDDISTR                          
045100     ELSE                                                                 
045200       MOVE NEJ                  TO NYCKLAR-SW                            
045300     END-IF                                                               
045400     MOVE MSGI-IDDISTR           TO MOD-IDDISTR-UT                        
045500     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900 BB-KOLLA-KUNDNUMMER SECTION.                                             
046000                                                                          
046100     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
046200                                                                          
046300     IF MID-IDKUNDNR-IN     NOT = ALL '+'                                 
046400       MOVE '7'             TO MFS-IDPFK                                  
046500       MOVE SPACE           TO MFS-KDTRTYP                                
046600     END-IF                                                               
046700     IF MSGI-IDKUNDNR NOT = ALL '+' AND                                   
046800        MSGI-IDKUNDNR NOT NUMERIC                                         
046900        MOVE ALL '0'           TO W-IDKUNDNR-MIN                          
047000        MOVE ALL '9'           TO W-IDKUNDNR-MAX                          
047100     ELSE                                                                 
047200       IF MSGI-IDKUNDNR NUMERIC                                           
047300         MOVE MSGI-IDKUNDNR    TO W-IDKUNDNR-MIN                          
047400                                  W-IDKUNDNR-MAX                          
047500       ELSE                                                               
047600         MOVE NEJ TO NYCKLAR-SW                                           
047700       END-IF                                                             
047800     END-IF                                                               
047900     MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                                
048000     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
048100     IF MSGI-IDKUNDNR = ZERO                                              
048200        MOVE '     0' TO MOD-IDKUNDNR-UT                                  
048300     END-IF                                                               
048400     .                                                                    
048500     EJECT                                                                
048600 BC-KOLLA-IDDC SECTION.                                                   
048700                                                                          
048800     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
048900                                                                          
049000     IF MID-IDDC-IN           = ALL '+'                                   
049100       MOVE MID-IDDC-UT      TO W-IDDC-B6                                 
049200     ELSE                                                                 
049300       MOVE MID-IDDC-IN      TO W-IDDC-B6                                 
049400       MOVE '7'              TO MFS-IDPFK                                 
049500       MOVE SPACE            TO MFS-KDTRTYP                               
049600     END-IF                                                               
049700     PERFORM IMS-GU-WDB601                                                
049800                                                                          
049900     MOVE W-IDDC-B6         TO IDDC-WS                                    
050000     INSPECT IDDC-WS REPLACING ALL SPACES BY ZEROS                        
050100     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
050200        IF IDDC-WS = ZERO                                                 
050300          CONTINUE                                                        
050400        ELSE                                                              
050500          MOVE MSGI-IDDC       TO W-IDDC-B6                               
050600          PERFORM IMS-GU-WDB601                                           
050700          MOVE '7'             TO MFS-IDPFK                               
050800          MOVE SPACE           TO MFS-KDTRTYP                             
050900        END-IF                                                            
051000     END-IF                                                               
051100                                                                          
051200     IF IDDC-WS = ZERO                                                    
051300       MOVE WC-DC-ZERO      TO W-IDDC                                     
051400       MOVE LOW-VALUE       TO W-IDDC-MIN                                 
051500       MOVE HIGH-VALUE      TO W-IDDC-MAX                                 
051600     ELSE                                                                 
051700       IF DCS-NDC-NA                                                      
051800         IF DIST40-NDC-NA OR DIST35-REFILL-NA OR                          
051900            DIST35-NA-CDC-RETURN  OR                                      
052000            DIST35-NA-TRANSFER    OR                                      
052100            DIS134-BYTESREN-NA                                            
052200            IF (DIST40-NDC-USA AND                                        
052300               (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA'))                      
052400            OR                                                            
052500               (DIST40-NDC-CAN AND                                        
052600               (DCS-NDC-NA AND DCS-IDLANDX2 = 'US'))                      
052700              MOVE NEJ TO NYCKLAR-SW                                      
052800            ELSE                                                          
052900              MOVE DCS-IDDC TO W-IDDC                                     
053000                               W-IDDC-MIN                                 
053100                               W-IDDC-MAX                                 
053200            END-IF                                                        
053300         ELSE                                                             
053400            MOVE NEJ TO NYCKLAR-SW                                        
053500         END-IF                                                           
053600       ELSE                                                               
053700         MOVE DCS-IDDC      TO W-IDDC                                     
053800                               W-IDDC-MIN                                 
053900                               W-IDDC-MAX                                 
054000       END-IF                                                             
054100     END-IF                                                               
054200     MOVE W-IDDC-B6       TO MOD-IDDC-UT                                  
054300     .                                                                    
054400     EJECT                                                                
054500 BD-FLYTTA-OEVRIGA SECTION.                                               
054600                                                                          
054700     MOVE MFS-RENSA-FAELT   TO MOD-KDORDKL-IN                             
054800                               MOD-KDFRAKT-IN                             
054900                               MOD-KDORDSTA-IN                            
055000                                                                          
055100     IF MID-KDORDKL-IN = ALL '+'                                          
055200        MOVE MID-KDORDKL-UT TO MOD-KDORDKL-UT                             
055300     ELSE                                                                 
055400        IF MID-KDORDKL-IN = '0'                                           
055500           MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-UT                         
055600        ELSE                                                              
055700           MOVE MID-KDORDKL-IN TO MOD-KDORDKL-UT                          
055800        END-IF                                                            
055900     END-IF                                                               
056000                                                                          
056100                                                                          
056200     IF MSGI-KDFRAKT           = '0 ' OR ' 0' OR '00'                     
056300        MOVE MFS-RENSA-FAELT   TO MOD-KDFRAKT-UT                          
056400     ELSE                                                                 
056500        MOVE MSGI-KDFRAKT      TO MOD-KDFRAKT-UT                          
056600     END-IF                                                               
056700                                                                          
056800     IF MID-KDORDSTA-IN = ALL '+'                                         
056900        MOVE MID-KDORDSTA-UT TO MOD-KDORDSTA-UT                           
057000     ELSE                                                                 
057100        IF MID-KDORDSTA-IN = '0'                                          
057200           MOVE MFS-RENSA-FAELT TO MOD-KDORDSTA-UT                        
057300        ELSE                                                              
057400           MOVE MID-KDORDSTA-IN TO MOD-KDORDSTA-UT                        
057500        END-IF                                                            
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 C-FOERSTA-SIDA SECTION.                                                  
058000                                                                          
058100     MOVE INF-FIRST-PAGE         TO MED-IDMFSFEL                          
058200     CALL WMEDKONV            USING MED-WMEDAREA                          
058300     MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                          
058400     INITIALIZE SAVE-AREA                                                 
058500     MOVE 01                     TO PGNO                                  
058600     MOVE JA                     TO FIRST-SW                              
058700                                                                          
058800     MOVE JA                     TO ALLT-SW                               
058900     .                                                                    
059000     EJECT                                                                
059100 D-NAESTA-SIDA SECTION.                                                   
059200                                                                          
059300     IF SAVE-IDTRANS = '4512'                                             
059400       IF (SAVE-IDKUNDNR-NEXT > ZERO OR                                   
059500           SAVE-IDORDNR7-NEXT > ZERO OR                                   
059600           SAVE-IDPRODNR-NEXT > ZERO OR                                   
059700           SAVE-IDDC-NEXT     > ZERO )                                    
059800                                                                          
059900         MOVE SAVE-IDKUNDNR-NEXT TO W-IDKUNDNR-MIN                        
060000         MOVE SAVE-IDORDNR7-NEXT TO W-IDKUNDRF-MIN                        
060100         IF SAVE-IDPRODNR-NEXT > ZERO                                     
060200           MOVE SAVE-IDPRODNR-NEXT                                        
060300                                 TO W-IDPRODNR-MIN                        
060400         END-IF                                                           
060500         MOVE SAVE-IDDC-NEXT     TO W-IDDC-MIN                            
060600                                                                          
060700         IF (SAVE-IDKUNDNR-NEXT NOT = SAVE-IDKUNDNR-PREV(PGNO) OR         
060800             SAVE-IDORDNR7-NEXT NOT = SAVE-IDORDNR7-PREV(PGNO) OR         
060900             SAVE-IDPRODNR-NEXT NOT = SAVE-IDPRODNR-PREV(PGNO) OR         
061000             SAVE-IDDC-NEXT     NOT = SAVE-IDDC-PREV    (PGNO))           
061100           IF PGNO = 20                                                   
061200             PERFORM VARYING PGNO FROM 1 BY 1                             
061300               UNTIL PGNO = 20                                            
061400               MOVE SAVE-IDKUNDNR-PREV(PGNO + 1)                          
061500                                 TO SAVE-IDKUNDNR-PREV(PGNO)              
061600               MOVE SAVE-IDORDNR7-PREV(PGNO + 1)                          
061700                                 TO SAVE-IDORDNR7-PREV(PGNO)              
061800               MOVE SAVE-IDPRODNR-PREV(PGNO + 1)                          
061900                                 TO SAVE-IDPRODNR-PREV(PGNO)              
062000               MOVE SAVE-IDDC-PREV    (PGNO + 1)                          
062100                                 TO SAVE-IDDC-PREV    (PGNO)              
062200             END-PERFORM                                                  
062300             MOVE NEJ            TO FIRST-SW                              
062400           ELSE                                                           
062500             ADD 1               TO PGNO                                  
062600           END-IF                                                         
062700         END-IF                                                           
062800         MOVE JA                 TO ALLT-SW                               
062900       ELSE                                                               
063000         MOVE INF-LAST-PAGE-SHOWN                                         
063100                                 TO MED-IDMFSFEL                          
063200         CALL WMEDKONV        USING MED-WMEDAREA                          
063300         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
063400         PERFORM MFS-ROER-EJ-BILD                                         
063500         MOVE NEJ                TO ALLT-SW                               
063600       END-IF                                                             
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 I-PREV-SIDA SECTION.                                                     
064100                                                                          
064200     IF SAVE-IDTRANS = '4512'                                             
064300       IF PGNO > 1                                                        
064400         COMPUTE PGNO = PGNO - 1                                          
064500         IF SAVE-IDKUNDNR-PREV(PGNO) > ZERO                               
064600          MOVE SAVE-IDKUNDNR-PREV(PGNO)                                   
064700                                 TO W-IDKUNDNR-MIN                        
064800         END-IF                                                           
064900         MOVE SAVE-IDORDNR7-PREV(PGNO)                                    
065000                                 TO W-IDKUNDRF-MIN                        
065100         IF SAVE-IDPRODNR-PREV(PGNO) > ZERO                               
065200           MOVE SAVE-IDPRODNR-PREV(PGNO)                                  
065300                                 TO W-IDPRODNR-MIN                        
065400         END-IF                                                           
065500         MOVE SAVE-IDDC-PREV(PGNO)                                        
065600                                 TO W-IDDC-MIN                            
065700                                    W-IDDC                                
065800       ELSE                                                               
065900         MOVE SAVE-IDKUNDNR-PREV(1)                                       
066000                                 TO W-IDKUNDNR-MIN                        
066100         MOVE SAVE-IDORDNR7-PREV(1)                                       
066200                                 TO W-IDKUNDRF-MIN                        
066300         IF SAVE-IDPRODNR-PREV(1) > ZERO                                  
066400           MOVE SAVE-IDPRODNR-PREV(1)                                     
066500                                 TO W-IDPRODNR-MIN                        
066600         END-IF                                                           
066700         MOVE SAVE-IDDC-PREV(1)  TO W-IDDC-MIN                            
066800         IF PGNO = 1                                                      
066900           IF FIRST-SW = JA                                               
067000             MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                          
067100             CALL WMEDKONV    USING MED-WMEDAREA                          
067200             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
067300             PERFORM MFS-RENSA-FAELT-UT                                   
067400           ELSE                                                           
067500             MOVE INF-NO-MORE-F6 TO MED-IDMFSFEL                          
067600             CALL WMEDKONV    USING MED-WMEDAREA                          
067700             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
067800             PERFORM MFS-RENSA-FAELT-UT                                   
067900           END-IF                                                         
068000         END-IF                                                           
068100       END-IF                                                             
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 E-SAMMA-SIDA SECTION.                                                    
068600                                                                          
068700     IF EGEN-MID OR HELP-MID                                              
068800       IF SAVE-IDTRANS = '4512'                                           
068900         IF SAVE-IDKUNDNR-PREV(PGNO) NUMERIC                              
069000           MOVE SAVE-IDKUNDNR-PREV(PGNO)                                  
069100                                 TO W-IDKUNDNR-MIN                        
069200         END-IF                                                           
069300         IF SAVE-IDORDNR7-PREV(PGNO) NUMERIC                              
069400           MOVE SAVE-IDORDNR7-PREV(PGNO)                                  
069500                                 TO W-IDKUNDRF-MIN                        
069600         END-IF                                                           
069700         IF SAVE-IDPRODNR-PREV(PGNO) NUMERIC                              
069800           MOVE SAVE-IDPRODNR-PREV(PGNO)                                  
069900                                 TO W-IDPRODNR-MIN                        
070000         END-IF                                                           
070100         MOVE SAVE-IDDC-PREV(PGNO)                                        
070200                                 TO W-IDDC                                
070300                                    W-IDDC-MIN                            
070400       ELSE                                                               
070500         INITIALIZE SAVE-AREA                                             
070600         MOVE 1                  TO PGNO                                  
070700         MOVE JA                 TO FIRST-SW                              
070800       END-IF                                                             
070900       MOVE +1                   TO INDX                                  
071000       PERFORM UNTIL INDX        >  MAX-INDX                              
071100         IF MID-IDTRANS(INDX) = ALL '+'                                   
071200           CONTINUE                                                       
071300         ELSE                                                             
071400           IF MID-IDTRANS(INDX) NUMERIC                                   
071500             PERFORM EA-STARTA-ANNAN-BILD                                 
071600             MOVE JA             TO SW-STARTA-ANNAN-BILD                  
071700             MOVE MAX-INDX       TO INDX                                  
071800           END-IF                                                         
071900         END-IF                                                           
072000         ADD +1                  TO INDX                                  
072100       END-PERFORM                                                        
072200     ELSE                                                                 
072300       INITIALIZE SAVE-AREA                                               
072400       MOVE 1                    TO PGNO                                  
072500       MOVE JA                   TO FIRST-SW                              
072600       PERFORM MFS-RENSA-FAELT-IN                                         
072700     END-IF                                                               
072800     .                                                                    
072900     EJECT                                                                
073000                                                                          
073100 EA-STARTA-ANNAN-BILD  SECTION.                                           
073200                                                                          
073300     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
073400     MOVE MID-IDKUNDNR(INDX)     TO MSGI-IDKUNDNR                         
073500     INSPECT MID-IDORDNR7(INDX) REPLACING LEADING SPACE BY ZERO           
073600     MOVE MID-IDORDNR7(INDX)     TO MSGI-IDKUNDRF(1:7)                    
073700     INSPECT MID-IDPRODNR(INDX) REPLACING LEADING SPACE BY ZERO           
073800     MOVE MID-IDPRODNR(INDX)     TO MSGI-IDPRODNR                         
073900     MOVE '001'                  TO MSGI-KDCALL                           
074000     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
074100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
074200                                                                          
074300     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
074400     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
074500     MOVE MID-IDTRANS(INDX) (1:1)  TO W-HOPP-IDTRANS-2                    
074600     MOVE MID-IDTRANS(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                  
074700     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
074800     MOVE '4512'                 TO P-TO-P-IDTRANS                        
074900     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
075000                                                                          
075100     PERFORM S01-INSERT-ALTMSG                                            
075200     .                                                                    
075300     EJECT                                                                
075400 F-LAES-VISA-INFO SECTION.                                                
075500                                                                          
075600     PERFORM IMS-GU-ORQI01-WDQ2C                                          
075700                                                                          
075800     IF SEGMENT-SAKNAS                                                    
075900       MOVE '029'                 TO    MED-IDMFSFEL                      
076000       CALL    WMEDKONV           USING MED-WMEDAREA                      
076100       MOVE    MED-MFSFEL         TO    MOD-TEMFSFEL                      
076200       PERFORM MFS-RENSA-FAELT-UT                                         
076300     ELSE                                                                 
076400       IF MFS-ENTER OR                                                    
076500          MFS-NEXT  OR                                                    
076600          MFS-PREVIOUS                                                    
076700         IF MSGI-IDKUNDNR NOT = ALL '+' AND                               
076800            MSGI-IDKUNDNR NOT NUMERIC                                     
076900           MOVE LOW-VALUE TO W-IDKUNDNR-MIN-X                             
077000         END-IF                                                           
077100         MOVE LOW-VALUE TO W-IDKUNDRF-MIN                                 
077200       END-IF                                                             
077300       MOVE +1             TO INDX                                        
077400       PERFORM UNTIL SEGMENT-SAKNAS  OR                                   
077500                     BASEN-SLUT      OR                                   
077600                     INDX > MAX-INDX                                      
077700         MOVE    OHUV-IDORDER      TO W-IDORDER-MIN                       
077800                                      W-IDORDER-MAX                       
077900         IF  DCS-DDC                                                      
078000           MOVE DCS-IDDC           TO W-IDDC-Q211-MIN                     
078100                                      W-IDDC-Q211-MAX                     
078200           PERFORM IMS-GNP-ORQI11-WDQ2-KVAL                               
078300           IF SEGMENT-FINNS                                               
078400             MOVE DCS-IDDC         TO W-IDDC-MIN                          
078500                                      W-IDDC-MAX                          
078600             MOVE '11'             TO W-IDDC                              
078700             PERFORM IMS-GNP-ORQI12-WDQ2-KVAL                             
078800********************************** FIX FöR ATT KLARA TRASIG BAS           
078900             IF SEGMENT-SAKNAS                                            
079000                MOVE ZERO TO ARB-KDFRAKT                                  
079100             END-IF                                                       
079200********************************** SLUT FIX BS                            
079300             PERFORM FB-LAS-ALLA-Q301-ODEL                                
079400           END-IF                                                         
079500         ELSE                                                             
079510           MOVE 'N'  TO W-GE-WDQ212                                       
079600           PERFORM S05-LAS-ORQI-Q212-ARBTAB                               
079700           PERFORM UNTIL INDX > MAX-INDX  OR                              
079800                         W-GE-WDQ212 = 'Y'                                
080000             MOVE    ARB-IDDC        TO W-IDDC-MIN                        
080100                                        W-IDDC-MAX                        
080200             PERFORM FB-LAS-ALLA-Q301-ODEL                                
080300             PERFORM S05-LAS-ORQI-Q212-ARBTAB                             
080400           END-PERFORM                                                    
080500         END-IF                                                           
080600         PERFORM IMS-GN-ORQI01-WDQ2C                                      
080700       END-PERFORM                                                        
080800       IF SEGMENT-FINNS                                                   
080900         PERFORM FC-LAS-FRAM-FLER-NEXT-NYCKLAR                            
081000       ELSE                                                               
081100         MOVE    ZERO                   TO    SAVE-IDKUNDNR-NEXT          
081200                                              SAVE-IDORDNR7-NEXT          
081300                                              SAVE-IDDC-NEXT              
081400                                              SAVE-IDPRODNR-NEXT          
081500       END-IF                                                             
081600     END-IF                                                               
081700       MOVE '002'     TO MSGI-KDCALL                                      
081800       MOVE '4512'    TO MSGI-IDTRANS                                     
081900       MOVE '4512'    TO SAVE-IDTRANS                                     
082000       MOVE SAVE-AREA TO MSGI-SPAR-AREA                                   
082100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
082200     .                                                                    
082300     EJECT                                                                
082400 FB-LAS-ALLA-Q301-ODEL SECTION.                                           
082500                                                                          
082600     PERFORM IMS-GU-ORQA01-WDQ3                                           
082700     MOVE    STATUS-WS TO WDQ3-STATUS                                     
082800     IF Q3-SEGMENT-FINNS                                                  
082900       IF MFS-ENTER OR                                                    
083000          MFS-NEXT  OR                                                    
083100          MFS-PREVIOUS                                                    
083200         MOVE LOW-VALUE  TO W-IDPRODNR-MIN-X                              
083300       END-IF                                                             
083400       IF ODEL-IDLEVNR = SPACE                                            
083500         MOVE NEJ TO SW-DIREKT-LEV                                        
083600       ELSE                                                               
083700         MOVE JA  TO SW-DIREKT-LEV                                        
083800       END-IF                                                             
083900       PERFORM UNTIL Q3-SEGMENT-SAKNAS  OR                                
084000                     Q3-BASEN-SLUT      OR                                
084100                     INDX > MAX-INDX                                      
084200                                                                          
084300         MOVE    SPACE     TO WDE6-STATUS                                 
084400         PERFORM FBA-SATT-STATUS                                          
084500         IF ORDERDEL-STATUS = 'R ' AND                                    
084600            NOT DIREKT-LEV                                                
084700           CONTINUE                                                       
084800         ELSE                                                             
084900           IF ORDERDEL-STATUS NOT = 'R '                                  
085000             IF WS-IDPRODNR-NUM NOT = W-IDPRODNR                          
085100               MOVE    WS-IDPRODNR-NUM TO W-IDPRODNR                      
085200               PERFORM IMS-GU-WDE601                                      
085300               MOVE    STATUS-WS TO WDE6-STATUS                           
085400             END-IF                                                       
085500             IF E6-SEGMENT-FINNS                                          
085600               ADD VORD-KVORDRAD        TO WS-KVRADER-REG                 
085700                                           WS-KVRADER-REG-E6              
085800               ADD VORD-KVORDRAD        TO WS-KVRADER-UTSKR               
085900               ADD VORD-KVORDRAD-PACK   TO WS-KVRADER-PACK                
086000             END-IF                                                       
086100           END-IF                                                         
086200         END-IF                                                           
086300         IF E6-SEGMENT-FINNS                                              
086400           IF DIREKT-LEV                                                  
086500             MOVE    WS-KVRADER-REG-E6    TO WS-KVRADER-REG               
086600             MOVE    ZERO                 TO WS-KVRADER-REG-E6            
086700           ELSE                                                           
086800             COMPUTE WS-KVRADER-REG       =  WS-KVRADER-REG-Q2            
086900                                          +  WS-KVRADER-REG-E6            
087000             MOVE    ZERO                 TO WS-KVRADER-REG-Q2            
087100                                             WS-KVRADER-REG-E6            
087200           END-IF                                                         
087300           IF OHUV-FLKLAR = NEJ                                           
087400             MOVE    'E '          TO ORDERDEL-STATUS                     
087500           END-IF                                                         
087600           PERFORM S06-FYLL-MOD                                           
087700           PERFORM S07-EV-SPARA-ENTER-NYCKLAR                             
087800         END-IF                                                           
087900         MOVE    ZERO  TO WS-KVRADER-REG                                  
088000                          WS-KVRADER-UTSKR                                
088100                          WS-KVRADER-PACK                                 
088200         MOVE    SPACE TO ORDERDEL-STATUS                                 
088300         IF ODEL-IDLEVNR = SPACE OR                                       
088400            Q3-SEGMENT-SAKNAS                                             
088500           MOVE NEJ TO SW-DIREKT-LEV                                      
088600         ELSE                                                             
088700           MOVE JA  TO SW-DIREKT-LEV                                      
088800         END-IF                                                           
088900       END-PERFORM                                                        
089000     ELSE                                                                 
089100       IF OHUV-FLKLAR     =  NEJ  OR                                      
089200          ORDERDEL-STATUS = 'B '  OR                                      
089300          ORDERDEL-STATUS = 'C '                                          
089400         IF OHUV-FLKLAR = NEJ                                             
089500           MOVE  'E '               TO ORDERDEL-STATUS                    
089600         END-IF                                                           
089700         MOVE    WS-KVRADER-REG-Q2  TO WS-KVRADER-REG                     
089800         PERFORM S06-FYLL-MOD                                             
089900         MOVE    ZERO               TO WS-KVRADER-REG                     
090000                                       WS-KVRADER-REG-Q2                  
090100         MOVE    SPACE              TO ORDERDEL-STATUS                    
090200         PERFORM S07-EV-SPARA-ENTER-NYCKLAR                               
090300       END-IF                                                             
090400       MOVE ZERO                    TO WS-KVRADER-REG                     
090500                                       WS-KVRADER-REG-Q2                  
090600       MOVE SPACE                   TO ORDERDEL-STATUS                    
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 FBA-SATT-STATUS SECTION.                                                 
091100                                                                          
091200     MOVE    SPACE         TO ORDERDEL-STATUS-2                           
091300     MOVE    ODEL-IDPRODNR TO WS-IDPRODNR-NUM                             
091400     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
091500                   BASEN-SLUT      OR                                     
091600                   ODEL-IDPRODNR NOT = WS-IDPRODNR-NUM                    
091700       IF ODEL-KDODELSTA = 'R' AND                                        
091800          NOT DIREKT-LEV                                                  
091900         MOVE 'R'      TO ORDERDEL-STATUS-1                               
092000       ELSE                                                               
092100         IF ORDERDEL-STATUS-1 = 'R' AND                                   
092200            NOT DIREKT-LEV                                                
092300           MOVE    '*' TO ORDERDEL-STATUS-2                               
092400         ELSE                                                             
092500           PERFORM FBAA-SATT-STATUS-FRAN-E6                               
092600         END-IF                                                           
092700       END-IF                                                             
092800       PERFORM IMS-GN-ORQA01-WDQ3                                         
092900     END-PERFORM                                                          
093000                                                                          
093100     MOVE STATUS-WS TO WDQ3-STATUS                                        
093200     .                                                                    
093300     EJECT                                                                
093400 FBAA-SATT-STATUS-FRAN-E6 SECTION.                                        
093500                                                                          
093600     IF WS-IDPRODNR-NUM NOT = W-IDPRODNR                                  
093700       MOVE    WS-IDPRODNR-NUM TO W-IDPRODNR                              
093800       PERFORM IMS-GU-WDE601                                              
093900       MOVE    STATUS-WS TO WDE6-STATUS                                   
094000     END-IF                                                               
094100     IF E6-SEGMENT-FINNS                                                  
094200       IF VORD-KDORDSTA < 3                                               
094300         MOVE 'U' TO ORDERDEL-STATUS-1                                    
094400         IF VORD-KVORDRAD-PACK = ZERO AND                                 
094500            VORD-KVKOLLI-FL    = ZERO                                     
094600           MOVE SPACE TO ORDERDEL-STATUS-2                                
094700         ELSE                                                             
094800           MOVE '*'   TO ORDERDEL-STATUS-2                                
094900         END-IF                                                           
095000       ELSE                                                               
095100         IF VORD-KDORDSTA = 3                                             
095200           MOVE 'P' TO ORDERDEL-STATUS-1                                  
095300           IF VORD-KVKOLLI-FL = ZERO                                      
095400             MOVE SPACE TO ORDERDEL-STATUS-2                              
095500           ELSE                                                           
095600             MOVE '*'   TO ORDERDEL-STATUS-2                              
095700           END-IF                                                         
095800         ELSE                                                             
095900           IF ODEL-KDODELSTA = 'P' AND                                    
096000              VORD-KDORDSTA  = 4                                          
096100             IF VORD-KVKOLLI-FAKT = ZERO                                  
096200               MOVE 'S' TO ORDERDEL-STATUS-1                              
096300               MOVE SPACE TO ORDERDEL-STATUS-2                            
096400             ELSE                                                         
096500               IF VORD-KVKOLLI-FAKT > VORD-KVKOLLI                        
096600                 MOVE 'S' TO ORDERDEL-STATUS-1                            
096700                 MOVE '*' TO ORDERDEL-STATUS-2                            
096800               ELSE                                                       
096900                 MOVE 'S' TO ORDERDEL-STATUS-1                            
097000                 MOVE 'F' TO ORDERDEL-STATUS-2                            
097100               END-IF                                                     
097200             END-IF                                                       
097300           ELSE                                                           
097400             IF ODEL-KDODELSTA = 'P' AND                                  
097500                VORD-KDORDSTA  = 5                                        
097600               MOVE 'S' TO ORDERDEL-STATUS-1                              
097700               MOVE 'F' TO ORDERDEL-STATUS-2                              
097800             END-IF                                                       
097900           END-IF                                                         
098000         END-IF                                                           
098100       END-IF                                                             
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500 FC-LAS-FRAM-FLER-NEXT-NYCKLAR SECTION.                                   
098600                                                                          
098700     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
098800                   BASEN-SLUT      OR                                     
098900                   NEXT-NYCKLAR-FINNS                                     
099000                                                                          
099100       MOVE    OHUV-IDORDER      TO W-IDORDER-MIN                         
099200                                    W-IDORDER-MAX                         
099300       PERFORM S05A-LAS-ARBETSTABELLEN                                    
099400       PERFORM UNTIL SEGMENT-SAKNAS  OR                                   
099500                     BASEN-SLUT      OR                                   
099600                     NEXT-NYCKLAR-FINNS                                   
099700         MOVE    ARB-IDDC        TO W-IDDC-MIN                            
099800                                    W-IDDC-MAX                            
099900         PERFORM IMS-GU-ORQA01-WDQ3                                       
100000         PERFORM UNTIL SEGMENT-SAKNAS  OR                                 
100100                       BASEN-SLUT      OR                                 
100200                       NEXT-NYCKLAR-FINNS                                 
100300           IF SEGMENT-FINNS                                               
100400             MOVE    JA TO SW-NEXT-NYCKLAR-FINNS                          
100500           ELSE                                                           
100600             PERFORM IMS-GN-ORQA01-WDQ3                                   
100700           END-IF                                                         
100800         END-PERFORM                                                      
100900         IF NEXT-NYCKLAR-SAKNAS                                           
101000           PERFORM S05A-LAS-ARBETSTABELLEN                                
101100         END-IF                                                           
101200       END-PERFORM                                                        
101300       IF NEXT-NYCKLAR-SAKNAS                                             
101400         PERFORM IMS-GN-ORQI01-WDQ2C                                      
101500       END-IF                                                             
101600     END-PERFORM                                                          
101700     IF NEXT-NYCKLAR-FINNS                                                
101800       MOVE    OHUV-IDKUNDNR        TO    SAVE-IDKUNDNR-NEXT              
101900       MOVE    OHUV-IDORDNR7        TO    SAVE-IDORDNR7-NEXT              
102000       MOVE    ARB-IDDC             TO    SAVE-IDDC-NEXT                  
102100       MOVE    ODEL-IDPRODNR        TO    SAVE-IDPRODNR-NEXT              
102200       MOVE    INF-MORE-INFO-EXISTS TO    MED-IDMFSINF                    
102300       CALL    WMEDKONV             USING MED-WMEDAREA                    
102400       MOVE    MED-TEMFSINF         TO    MOD-TEMFSINF                    
102500     ELSE                                                                 
102600       MOVE  ZERO                   TO    SAVE-IDKUNDNR-NEXT              
102700                                          SAVE-IDORDNR7-NEXT              
102800                                          SAVE-IDDC-NEXT                  
102900                                          SAVE-IDPRODNR-NEXT              
103000     END-IF                                                               
103100     .                                                                    
103200     EJECT                                                                
103300                                                                          
103400 S01-INSERT-ALTMSG SECTION.                                               
103500                                                                          
103600     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
103700     PERFORM IMS-CHANGE-ALTMSG                                            
103800     IF STATUS-OK                                                         
103900       PERFORM IMS-INSERT-ALTMSG                                          
104000     ELSE                                                                 
104100       MOVE LOW-VALUE          TO MSG-AREA                                
104200       MOVE 'W4O51201'         TO MFS-IDMOD                               
104300       MOVE '4512'             TO MOD-IDTRANS                             
104400       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
104500       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
104600       IF SECURITY-FEL                                                    
104700         STRING 'NOT AUTHORIZED TO USE '                                  
104800                W-BILD                                                    
104900                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
105000       ELSE                                                               
105100         STRING 'WRONG PICTURE '                                          
105200                 W-BILD                                                   
105300                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
105400       END-IF                                                             
105500       PERFORM MFS-ROER-EJ-BILD                                           
105600       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51201 + 4                      
105700       PERFORM IMS-INSERT-MSG                                             
105800     END-IF                                                               
105900     .                                                                    
106000     EJECT                                                                
106100                                                                          
106200 S05-LAS-ORQI-Q212-ARBTAB SECTION.                                        
106300                                                                          
106400     PERFORM S05A-LAS-ARBETSTABELLEN                                      
106500                                                                          
106600     IF SEGMENT-FINNS                                                     
106601                                                                          
106602       MOVE ARB-IDDC TO W-IDDC                                            
106610       PERFORM IMS-GNP-ORQI21                                             
106630       PERFORM UNTIL SEGMENT-SAKNAS                                       
106650          ADD LOR-KVRADER    TO WS-KVRADER-REG-Q2                         
106693          PERFORM IMS-GNP-ORQI21                                          
106695       END-PERFORM                                                        
107200                                                                          
107300       IF ARB-TIRFS    = ZERO   AND                                       
107400          ARB-DATRPAVD = ZERO   AND                                       
107500          ARB-TIHHMM   = ZERO                                             
107600         IF ARB-KDTRPKAT = 'A'                                            
107700           MOVE 'R'          TO ORDERDEL-STATUS-1                         
107800         ELSE                                                             
107900           MOVE ARB-KDTRPKAT TO ORDERDEL-STATUS-1                         
108000         END-IF                                                           
108100         MOVE SPACE          TO ORDERDEL-STATUS-2                         
108200       END-IF                                                             
108210     ELSE                                                                 
108220       MOVE 'Y' TO W-GE-WDQ212                                            
108300     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600 S05A-LAS-ARBETSTABELLEN SECTION.                                         
108700                                                                          
108800     IF MFS-FIRST                                                         
108900       IF IDDC-WS = ZERO                                                  
109000         PERFORM IMS-GNP-ORQI12-WDQ2-OKVAL                                
109100       ELSE                                                               
109200         PERFORM IMS-GNP-ORQI12-WDQ2-KVAL                                 
109300       END-IF                                                             
109400     ELSE                                                                 
109500       IF MFS-NEXT                                                        
109600         IF IDDC-WS = ZERO                                                
109700           IF SAVE-IDDC-NEXT > ZERO                                       
109800* ---        OM DETTA ÄR FALLET ÄR DET LÄSNINGEN FÖR FÖRSTA MOD-          
109900* ---        RADEN SOM SKALL GÖRAS, OCH DEN MÅSTE LÄSAS KVALI-            
110000* ---        FICERAT.                                                     
110100             MOVE    SAVE-IDDC-NEXT TO W-IDDC                             
110200             PERFORM IMS-GNP-ORQI12-WDQ2-KVAL                             
110300             MOVE    ZERO          TO SAVE-IDDC-NEXT                      
110400             MOVE    WC-DC-ZERO    TO W-IDDC                              
110500           ELSE                                                           
110600             PERFORM IMS-GNP-ORQI12-WDQ2-OKVAL                            
110700           END-IF                                                         
110800         ELSE                                                             
110900           PERFORM IMS-GNP-ORQI12-WDQ2-KVAL                               
111000           IF SAVE-IDDC-NEXT > ZERO                                       
111100             MOVE ZERO    TO SAVE-IDDC-NEXT                               
111200           END-IF                                                         
111300         END-IF                                                           
111400       ELSE                                                               
111500         IF IDDC-WS = ZERO                                                
111600           IF INDX = 1                                                    
111700             IF SAVE-IDDC-PREV(PGNO) > ZERO                               
111800* ---          OM DETTA ÄR FALLET ÄR DET LÄSNINGEN FÖR FÖRSTA MOD-        
111900* ---          RADEN SOM SKALL GÖRAS, OCH DEN MÅSTE LÄSAS KVALI-          
112000* ---          FICERAT.                                                   
112100               MOVE SAVE-IDDC-PREV(PGNO)                                  
112200                                      TO W-IDDC                           
112300               PERFORM IMS-GNP-ORQI12-WDQ2-KVAL                           
112400               MOVE WC-DC-ZERO        TO W-IDDC                           
112500             ELSE                                                         
112600               PERFORM IMS-GNP-ORQI12-WDQ2-OKVAL                          
112700             END-IF                                                       
112800           ELSE                                                           
112900             PERFORM IMS-GNP-ORQI12-WDQ2-OKVAL                            
113000           END-IF                                                         
113100         ELSE                                                             
113200           PERFORM IMS-GNP-ORQI12-WDQ2-KVAL                               
113300         END-IF                                                           
113400       END-IF                                                             
113500     END-IF                                                               
113600     .                                                                    
113700     EJECT                                                                
113800 S06-FYLL-MOD SECTION.                                                    
113900                                                                          
114000     MOVE WS-KVRADER-REG              TO MOD-KVORDRAD-REG(INDX)           
114100     MOVE WS-KVRADER-UTSKR            TO MOD-KVORDRAD-UTSKR(INDX)         
114200     MOVE WS-KVRADER-PACK             TO MOD-KVORDRAD-PACK(INDX)          
114300     MOVE ARB-IDDC                    TO MOD-IDDC       (INDX)            
114400     MOVE ARB-KDFRAKT                 TO MOD-KDFRAKT    (INDX)            
114500     MOVE OHUV-IDKUNDNR               TO MOD-IDKUNDNR   (INDX)            
114600     MOVE OHUV-IDORDNR7               TO MOD-IDORDNR7   (INDX)            
114700     MOVE OHUV-KDORDKL                TO MOD-KDORDKL  (INDX)              
114800     MOVE WS-IDPRODNR-NUM             TO MOD-IDPRODNR (INDX)              
114900     IF Q3-SEGMENT-SAKNAS                                                 
115000       IF ORDERDEL-STATUS = 'E ' OR                                       
115100          ORDERDEL-STATUS = 'B ' OR                                       
115200          ORDERDEL-STATUS = 'C '                                          
115300         MOVE    ZERO                 TO MOD-IDPRODNR (INDX)              
115400         INSPECT MOD-IDPRODNR (INDX) REPLACING ALL ZEROES BY SPACE        
115500       END-IF                                                             
115600     END-IF                                                               
115700     MOVE ORDERDEL-STATUS             TO MOD-KDORDSTA (INDX)              
115800                                                                          
115900     ADD +1 TO INDX                                                       
116000     .                                                                    
116100     EJECT                                                                
116200 S07-EV-SPARA-ENTER-NYCKLAR SECTION.                                      
116300     IF INDX - 1 = 1                                                      
116400       MOVE OHUV-IDKUNDNR     TO SAVE-IDKUNDNR-PREV(PGNO)                 
116500       MOVE OHUV-IDORDNR7     TO SAVE-IDORDNR7-PREV(PGNO)                 
116600       MOVE ARB-IDDC          TO SAVE-IDDC-PREV(PGNO)                     
116700       IF ORDERDEL-STATUS = 'E '                                          
116800         MOVE ZERO            TO SAVE-IDPRODNR-PREV(PGNO)                 
116900       ELSE                                                               
117000         MOVE WS-IDPRODNR-NUM TO SAVE-IDPRODNR-PREV(PGNO)                 
117100       END-IF                                                             
117200     END-IF                                                               
117300     .                                                                    
117400     EJECT                                                                
117500 MFS-ROER-EJ-BILD SECTION.                                                
117600                                                                          
117700     MOVE +1 TO INDX                                                      
117800     PERFORM UNTIL INDX                > MAX-INDX OR                      
117900                   MOD-IDKUNDNR (INDX) = SPACE                            
118000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR (INDX)                      
118100                                 MOD-IDORDNR7 (INDX)                      
118200                                 MOD-IDPRODNR (INDX)                      
118300                                 MOD-IDDC     (INDX)                      
118400                                 MOD-KDFRAKT  (INDX)                      
118500                                 MOD-KDORDKL  (INDX)                      
118600                                 MOD-KDORDSTA (INDX)                      
118700                                 MOD-KVORDRAD-REG   (INDX)                
118800                                 MOD-KVORDRAD-PACK  (INDX)                
118900                                 MOD-KVORDRAD-UTSKR (INDX)                
119000       ADD +1 TO INDX                                                     
119100     END-PERFORM                                                          
119200     .                                                                    
119300     EJECT                                                                
119400 MFS-RENSA-FAELT-IN SECTION.                                              
119500*    --- ALLA INDATA-FÄLT                                                 
119600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
119700                             MOD-IDKUNDNR-UT                              
119800                             MOD-IDDC-UT                                  
119900                             MOD-KDORDKL-UT                               
120000                             MOD-KDFRAKT-UT                               
120100                             MOD-KDORDSTA-UT                              
120200                             MOD-IDDISTR-IN                               
120300                             MOD-IDKUNDNR-IN                              
120400                             MOD-IDDC-IN                                  
120500                             MOD-KDORDKL-IN                               
120600                             MOD-KDFRAKT-IN                               
120700                             MOD-KDORDSTA-IN                              
120800     .                                                                    
120900     EJECT                                                                
121000 MFS-RENSA-FAELT-UT SECTION.                                              
121100                                                                          
121200*    --- ALLA UTDATA-FÄLT                                                 
121300     MOVE +1 TO INDX                                                      
121400     PERFORM UNTIL INDX > MAX-INDX                                        
121500       MOVE MFS-RENSA-FAELT   TO MOD-IDKUNDNR       (INDX)                
121600                                 MOD-IDORDNR7       (INDX)                
121700                                 MOD-IDDC           (INDX)                
121800                                 MOD-IDPRODNR       (INDX)                
121900                                 MOD-KDFRAKT        (INDX)                
122000                                 MOD-KDORDKL        (INDX)                
122100                                 MOD-KDORDSTA       (INDX)                
122200                                 MOD-KVORDRAD-REG   (INDX)                
122300                                 MOD-KVORDRAD-UTSKR (INDX)                
122400                                 MOD-KVORDRAD-PACK  (INDX)                
122500       ADD +1 TO INDX                                                     
122600     END-PERFORM                                                          
122700     .                                                                    
122800     EJECT                                                                
122900                                                                          
123000* --- IMS SEKTIONER ---                                                   
123100     SKIP3                                                                
123200 IMS-GET-MSG SECTION.                                                     
123300                                                                          
123400     MOVE '  QC' TO GODK-STATUSKODER                                      
123500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
123600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP3                                                                
124000 IMS-INSERT-MSG SECTION.                                                  
124100                                                                          
124200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
124300       MOVE '0' TO MFS-KDHUVOMR                                           
124400     END-IF                                                               
124500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124600     MOVE SPACE TO GODK-STATUSKODER                                       
124700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
124800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124900     PERFORM IMS-STATUSKONTROLL                                           
125000     .                                                                    
125100     EJECT                                                                
125200                                                                          
125300 IMS-CHANGE-ALTMSG SECTION.                                               
125400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
125500     MOVE '  A1A4' TO GODK-STATUSKODER                                    
125600     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
125700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
125800     PERFORM IMS-STATUSKONTROLL                                           
125900     .                                                                    
126000     SKIP3                                                                
126100 IMS-INSERT-ALTMSG SECTION.                                               
126200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
126300     MOVE SPACE TO GODK-STATUSKODER                                       
126400     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
126500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-GU-ORQI01-WDQ2C SECTION.                                             
127000                                                                          
127100     STRING 'WLORQI01(WDQ2CSEQ=>' W-WDQ2CSEQ-MIN-X                        
127200                    '&WDQ2CSEQ=<' W-WDQ2CSEQ-MAX-X                        
127300                    '&FLBORT   =N)'                                       
127400          DELIMITED BY SIZE INTO SSA1                                     
127500     MOVE '  GE' TO GODK-STATUSKODER                                      
127600     CALL CBLTDLI USING GU ORQI-PCB OHUV-IO-AREA SSA1                     
127700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000     SKIP3                                                                
128100 IMS-GN-ORQI01-WDQ2C SECTION.                                             
128200                                                                          
128300     STRING 'WLORQI01(WDQ2CSEQ=>' W-WDQ2CSEQ-MIN-X                        
128400                    '&WDQ2CSEQ=<' W-WDQ2CSEQ-MAX-X                        
128500                    '&FLBORT   =N)'                                       
128600          DELIMITED BY SIZE INTO SSA1                                     
128700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
128800     CALL CBLTDLI USING GN ORQI-PCB OHUV-IO-AREA SSA1                     
128900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
129000     PERFORM IMS-STATUSKONTROLL                                           
129100     .                                                                    
129200     EJECT                                                                
129300 IMS-GU-ORQA01-WDQ3 SECTION.                                              
129400                                                                          
129500     STRING 'WLORQA01(WDQ301KY=>' W-WDQ301KY-MIN-X                        
129600                    '&WDQ301KY=<' W-WDQ301KY-MAX-X ')'                    
129700          DELIMITED BY SIZE INTO SSA1                                     
129800     MOVE '  GE' TO GODK-STATUSKODER                                      
129900     CALL CBLTDLI USING GU ORQA-PCB ODEL-IO-AREA SSA1                     
130000     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300                                                                          
130400 IMS-GN-ORQA01-WDQ3 SECTION.                                              
130500                                                                          
130600     STRING 'WLORQA01(WDQ301KY=>' W-WDQ301KY-MIN-X                        
130700                    '&WDQ301KY=<' W-WDQ301KY-MAX-X ')'                    
130800          DELIMITED BY SIZE INTO SSA1                                     
130900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
131000     CALL CBLTDLI USING GN ORQA-PCB ODEL-IO-AREA SSA1                     
131100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
131200     PERFORM IMS-STATUSKONTROLL                                           
131300     .                                                                    
131400     EJECT                                                                
131500                                                                          
131600 IMS-GU-WDE601      SECTION.                                              
131700                                                                          
131800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
131900          DELIMITED BY SIZE INTO SSA1                                     
132000     MOVE '  GE' TO GODK-STATUSKODER                                      
132100     CALL CBLTDLI USING GU WDE6-PCB VORD-IO-AREA SSA1                     
132200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
132300     PERFORM IMS-STATUSKONTROLL                                           
132400     .                                                                    
132500     EJECT                                                                
132600 IMS-GNP-ORQI11-WDQ2-KVAL SECTION.                                        
132700                                                                          
132800     STRING  'WLORQI11(WDQ211KY=>' W-WDQ211KY-MIN-X                       
132900                    '&WDQ211KY=<' W-WDQ211KY-MAX-X ')'                    
133000          DELIMITED BY SIZE INTO SSA1                                     
133100     MOVE    '  GE'           TO GODK-STATUSKODER                         
133200     CALL    CBLTDLI USING GNP ORQI-PCB DIRL-IO-AREA SSA1                 
133300     MOVE    ORQI-STATUS-CODE TO STATUS-WS                                
133400     PERFORM IMS-STATUSKONTROLL                                           
133500     .                                                                    
133600     EJECT                                                                
133700                                                                          
133800 IMS-GNP-ORQI12-WDQ2-KVAL SECTION.                                        
133900                                                                          
134000     STRING  'WLORQI12(IDDC     =' W-IDDC-X ')'                           
134100          DELIMITED BY SIZE INTO SSA1                                     
134200     MOVE    '  GE'           TO GODK-STATUSKODER                         
134300     CALL    CBLTDLI USING GNP ORQI-PCB ARB-IO-AREA SSA1                  
134400     MOVE    ORQI-STATUS-CODE TO STATUS-WS                                
134500     PERFORM IMS-STATUSKONTROLL                                           
134600     .                                                                    
134700     EJECT                                                                
134800 IMS-GNP-ORQI12-WDQ2-OKVAL SECTION.                                       
134900                                                                          
135000     MOVE    'WLORQI12'       TO SSA1                                     
135100     MOVE    '  GE'           TO GODK-STATUSKODER                         
135200     CALL    CBLTDLI USING GNP ORQI-PCB ARB-IO-AREA SSA1                  
135300     MOVE    ORQI-STATUS-CODE TO STATUS-WS                                
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     EJECT                                                                
135691 IMS-GNP-ORQI21 SECTION.                                                  
135692                                                                          
135693     STRING  'WLORQI12(IDDC     =' W-IDDC-X ')'                           
135694          DELIMITED BY SIZE INTO SSA1                                     
135695     MOVE    'WLORQI21'       TO SSA2                                     
135696     MOVE    '  GE'           TO GODK-STATUSKODER                         
135697     CALL CBLTDLI USING GNP ORQI-PCB LOR-IO-AREA SSA1 SSA2                
135698     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
135699     PERFORM IMS-STATUSKONTROLL                                           
135700     .                                                                    
135701     EJECT                                                                
135710 IMS-GU-WDB601    SECTION.                                                
135800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
135900          DELIMITED BY SIZE INTO SSA1                                     
136000     MOVE '  GE' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
136200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     IF SEGMENT-SAKNAS                                                    
136500         MOVE SPACE TO DCS-KDDC                                           
136600     END-IF                                                               
136700     .                                                                    
136800 IMS-STATUSKONTROLL SECTION.                                              
136900                                                                          
137000     SET STATUS-IX TO 1                                                   
137100     SEARCH GODK-STATUS                                                   
137200       AT END CALL FELLOG                                                 
137300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
137400     END-SEARCH                                                           
137500     .                                                                    
