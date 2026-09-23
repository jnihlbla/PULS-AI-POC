000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4062200.                                                
000300 AUTHOR.         NILSSON LINDA.                                           
000400 DATE-WRITTEN.   02/04/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000610                                                                          
000700*    FUNKTION:                                                            
000800*        SCREEN TO START PRINT OF TRANSPORT                               
000900*        DOCUMENTS FOR RELEASED SHIPPINGS.                                
001000*                                                                         
001100*        PROGRAM UPDATES WDE1                                             
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T622                                              
001500*        MID:         W4I62201                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O62201                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W4062200'.            
002700                                                                          
002800*    --- WORKING AREAS FOR ERROR MESSAGES FROM ABEND                      
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
003500     88  KDCMD-GIVEN                         VALUE 'J'.                   
003600                                                                          
003700 77  DATE-INPUT-SW               PIC X       VALUE 'N'.                   
003800     88  DATE-INPUT                          VALUE 'J'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  IDTRANS-SW                  PIC X(4)    VALUE SPACE.                 
004900     88  EGEN-MID                            VALUE '4622'.                
005000     88  GODK-MID                            VALUE '4621' '4622'          
005100                                                   '4623' '4624'          
005200                                                   '4625' '4626'          
005300                                                   '4627' '4628'          
005400                                                   '4629'.                
005500     88  HELP-MID                            VALUE '0551'.                
005600     88  4621-MID                            VALUE '4621'.                
005700     EJECT                                                                
005800 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
005900 77  MAX-INDX                    PIC S9(4)   VALUE +12  COMP SYNC.        
006000 77  MAX-KVANTEX                 PIC S9(4)   VALUE +12  COMP SYNC.        
006100                                                                          
006200 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
006300 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
006400                                                                          
006500 77  W-IDPRTLST                  PIC X(8)    VALUE SPACE.                 
006600 77  W-IDDC-REC                  PIC X(2)    VALUE SPACE.                 
006700 77  W-IDSHIPM-PRINT             PIC 9(7)    VALUE ZERO.                  
006710 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006800                                                                          
006900*    --- SUBPROGRAMS AND PARAMETERAREAS                                   
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007800     EJECT                                                                
007900                                                                          
008000*    --- PARAMETERS FOR WMEDKONV                                          
008100*01 -COPY WMEDAREA                                                        
008200 01  MESSAGE-CODES.                                                       
008300     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
008400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008500     03  INF-ENTER-CMD           PIC X(3)    VALUE '048'.                 
008600     03  INF-PRESS-PF4-TO-PRINT  PIC X(3)    VALUE '081'.                 
008700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008900     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '118'.                 
009000     03  INF-PRESS-PF9-TO-SPLIT  PIC X(3)    VALUE '127'.                 
009100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009200     03  ERR-MORE-THAN-ONE-CMD   PIC X(3)    VALUE '097'.                 
009300     03  ERR-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
009400     03  ERR-PF4-AND-NO-CMD      PIC X(3)    VALUE '231'.                 
009500     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
009600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009700     03  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '724'.                 
009800     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
009900     EJECT                                                                
010000                                                                          
010100*    --- PARAMETERS FOR ABEND/FELLOG                                      
010200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010500     SKIP3                                                                
010600                                                                          
010700*    --- PARAMETERS FOR W006PRT                                           
010800*   -COPY W006PRT                                                         
010900     EJECT                                                                
011500                                                                          
011600*    --- PARAMETERS FOR SUBPROGRAM W005INIT                               
011700*01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011800*01 -COPY WMSGINIT                                                        
011900     EJECT                                                                
012000*    --- AREA  FOR WZ01  ------                                           
012100 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
012200*01  -COPY WZ01SEND                                                       
012300                                                                          
012400*    --- AREA WITH DATA TO BE SAVED BETWEEN DIALOGSTEPS                   
012500 01  SPAR-AREA.                                                           
012600     03  SPAR-IDTRANS            PIC X(4)    VALUE '4622'.                
012700     03  SPAR-IDSHIPM-ENTER      PIC 9(7).                                
012800     03  SPAR-IDSHIPM-NEXT       PIC 9(7).                                
012900     03  FILLER OCCURS 12.                                                
013000         05 SPAR-IDSHIPM         PIC 9(7)    VALUE ZERO.                  
013100     EJECT                                                                
013200*    --- AREAS FOR MFS OCH SCREEN INPUT                                   
013300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013400     SKIP3                                                                
013500*01  MID -COPY W4I62201                                                   
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013800     SKIP3                                                                
013900*01  -COPY WMSGAREA                                                       
014000     EJECT                                                                
014100     03  MOD REDEFINES MSG-AREA.                                          
014200*      05  -COPY W4O62201                                                 
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16) VALUE 'P-TO-P-SW-4621'.        
014500 01  P-TO-P-SW-4621.                                                      
014600     03  PTOP-LL                 PIC S9(4)   VALUE 0 COMP SYNC.           
014700     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
014800     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
014900     03  PTOP-KDTRANS            PIC  X(8)   VALUE 'W4T621  '.            
015000     03  PTOP-IDTRANS            PIC  X(4)   VALUE '4622'.                
015100     03  PTOP-KDMFSFOR           PIC  X(1).                               
015200*    03  -COPY W4I62101   -PRE PTOP-                                      
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16) VALUE 'WZ01-SEND 4631'.        
015500*01  MID -COPY W4I63101   -PRE 4631-.                                     
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015800     SKIP3                                                                
015900*01  -COPY WMFSAREA                                                       
016000     EJECT                                                                
016100                                                                          
016200*    --- WORK-AREAS TO IMS-SECTIONS                                       
016300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016400     SKIP3                                                                
016500                                                                          
016600 01  NYCKLAR-TILL-DLI.                                                    
016700                                                                          
016710     03  W-TISKEPPN-X.                                                    
016720         05  W-TISKEPPN          PIC S9(7) COMP-3 VALUE ZERO.             
016721                                                                          
016722     03  W-TISKPTID-X.                                                    
016723         05  W-TISKPTID          PIC S9(7) COMP-3 VALUE ZERO.             
016730                                                                          
016800     03  W-WDE1A1KY-MIN.                                                  
016900         05  W-IDDC-MIN          PIC X(2).                                
017000         05  W-IDTRPTNR-MIN      PIC S9(3) COMP-3.                        
017100         05  W-IDLBBET-MIN       PIC X(12) VALUE LOW-VALUE.               
017200         05  W-TISKEPPN-MIN      PIC S9(7) COMP-3 VALUE ZERO.             
017210         05  W-TISKPTID-MIN      PIC S9(7) COMP-3 VALUE ZERO.             
017300                                                                          
017400     03  W-WDE1A1KY-MAX.                                                  
017500         05  W-IDDC-MAX          PIC X(2).                                
017600         05  W-IDTRPTNR-MAX      PIC S9(3) COMP-3.                        
017700         05  W-IDLBBET-MAX       PIC X(12) VALUE HIGH-VALUE.              
017900         05  W-TISKEPPN-MAX      PIC S9(7) COMP-3 VALUE 9999999.          
017910         05  W-TISKPTID-MAX      PIC S9(7) COMP-3 VALUE 9999999.          
018000                                                                          
018001     03  W-WDE1ASEQ-MIN.                                                  
018002         05  W-IDDC-ASEQ-MIN     PIC X(2).                                
018003         05  W-IDTRPTNR-ASEQ-MIN PIC S9(3) COMP-3.                        
018004         05  W-IDLBBET-ASEQ-MIN  PIC X(12) VALUE LOW-VALUE.               
018005         05  W-TISKEPPN-ASEQ-MIN PIC S9(7) COMP-3 VALUE ZERO.             
018006                                                                          
018007     03  W-WDE1ASEQ-MAX.                                                  
018008         05  W-IDDC-ASEQ-MAX     PIC X(2).                                
018009         05  W-IDTRPTNR-ASEQ-MAX PIC S9(3) COMP-3.                        
018010         05  W-IDLBBET-ASEQ-MAX  PIC X(12) VALUE HIGH-VALUE.              
018011         05  W-TISKEPPN-ASEQ-MAX PIC S9(7) COMP-3 VALUE 9999999.          
018012                                                                          
018020     03  W-IDSHIPM-X.                                                     
018030         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
018300                                                                          
018400*    --- TURN PAGE KEY                                                    
018410     03  W-IDSHIPM-PF-X.                                                  
018420         05  W-IDSHIPM-PF        PIC 9(7)    VALUE ZERO.                  
018700     SKIP2                                                                
018701*    --- DC TABLE                                                         
018710   03    W-IDDC-B6-X.                                                     
018720     05  W-IDDC-B6               PIC  X(2).                               
018730                                                                          
018810*    --- STATUS-CODE FROM IMS                                             
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019300     88  BASEN-SLUT                          VALUE 'GB'.                  
019400     SKIP2                                                                
019500 01  GODK-STATUSKODER.                                                    
019600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     SKIP3                                                                
019800 01  SSA1                        PIC X(160).                              
019900     EJECT                                                                
020000                                                                          
020100*    --- IMS FUNCTIONCODES                                                
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400                                                                          
020500*    ---  DLI INPUT-OUTPUT AREA                                           
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
020700 01  DLI-IO-WDE101.                                                       
020800*    03  -COPY WDE101                                                     
020900     EJECT                                                                
020910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE1A1'.                      
020920 01  DLI-IO-WDE1A1.                                                       
020930*    03  -COPY WDE1A1                                                     
020940     EJECT                                                                
020950 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
020960 01  DLI-IO-WDB601.                                                       
020970*    03  -COPY WDB601                                                     
020980     EJECT                                                                
021000                                                                          
021100 LINKAGE SECTION.                                                         
021200*01  -COPY W0009   -PRE MSG-                                              
021300*01  -COPY W0009   -PRE SH31-                                             
021400*01  -COPY W0009   -PRE ALT-                                              
021500*01  -COPY W0008   -PRE WDP7-                                             
021600     05  FILLER                  PIC X.                                   
021700*01  -COPY W0008   -PRE WDE1-                                             
021701     05  FILLER                  PIC X.                                   
021710*01  -COPY W0008   -PRE WDE1A-                                            
021800     05  FILLER                  PIC X.                                   
021820*01  -COPY W0008   -PRE WDE1P-                                            
021830     05  FILLER                  PIC X.                                   
021840*01  -COPY W0008   -PRE WDB6-                                             
021850     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000                                                                          
022100 PROCEDURE DIVISION  USING MSG-PCB SH31-PCB ALT-PCB WDP7-PCB              
022200                       WDE1-PCB WDE1A-PCB WDE1P-PCB WDB6-PCB.             
022300                                                                          
022400 MAIN SECTION.                                                            
022500     ENTRY 'DLITCBL' USING MSG-PCB SH31-PCB ALT-PCB WDP7-PCB              
022600                       WDE1-PCB WDE1A-PCB WDE1P-PCB WDB6-PCB.             
022700                                                                          
022800     PERFORM IMS-GET-MSG                                                  
022900     IF SEGMENT-FINNS                                                     
023000       PERFORM A-INIT                                                     
023100       PERFORM B-KOLLA-NYCKLAR                                            
023200       IF NYCKLAR-OK                                                      
023300         IF 4621-MID                                                      
023400           PERFORM J-RETURN-FROM-4621                                     
023500         ELSE                                                             
023600           IF MFS-SPLIT                                                   
023700             PERFORM I-SPLIT-TO-W40621                                    
023800           ELSE                                                           
023900             IF MFS-PRINT                                                 
024000               PERFORM G-CHECK-PRINT                                      
024100             ELSE                                                         
024200               IF MFS-FIRST                                               
024300                 PERFORM C-FOERSTA-SIDA                                   
024400               ELSE                                                       
024500                 IF MFS-NEXT                                              
024600                   PERFORM D-NAESTA-SIDA                                  
024700                 ELSE                                                     
024800                   PERFORM E-SAMMA-SIDA                                   
024900                 END-IF                                                   
025000               END-IF                                                     
025100             END-IF                                                       
025200           END-IF                                                         
025300         END-IF                                                           
025400         IF INDATA-OK                                                     
025500           IF NOT MFS-SPLIT                                               
025600             PERFORM F-LAES-VISA-INFO                                     
025700           END-IF                                                         
025800         END-IF                                                           
025900       END-IF                                                             
026000       IF MFS-SPLIT AND INDATA-OK                                         
026100         COMPUTE PTOP-LL = LENGTH OF PTOP-MID-W4I62101 + 17               
026200         PERFORM IMS-ISRT-ALT-MSG-4621                                    
026300       ELSE                                                               
026400         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O62201 + 4                    
026500         PERFORM IMS-INSERT-MSG                                           
026600       END-IF                                                             
026700     END-IF                                                               
026800                                                                          
026900     MOVE ZERO TO RETURN-CODE                                             
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 A-INIT SECTION.                                                          
027400                                                                          
027500     IF MSG-DUBBLA-TRANSKODER                                             
027600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I62201                 
027700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027900     ELSE                                                                 
028000       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I62201                   
028100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
028200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028300     END-IF                                                               
028400                                                                          
028500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
028700     MOVE MFS-IDTRANS TO IDTRANS-SW                                       
028800                                                                          
028900     MOVE LOW-VALUE TO MSG-AREA                                           
029000     MOVE 'W4O622N1' TO MFS-IDMOD                                         
029100     MOVE '4622' TO MOD-IDTRANS                                           
029200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029300                                                                          
029400     IF EGEN-MID OR HELP-MID                                              
029500       CONTINUE                                                           
029600     ELSE                                                                 
029700       MOVE SPACE TO MFS-KDTRTYP                                          
029800       MOVE '7' TO MFS-IDPFK                                              
029900     END-IF                                                               
030000     MOVE +1 TO INDX                                                      
030100     .                                                                    
030200     EJECT                                                                
030300 B-KOLLA-NYCKLAR SECTION.                                                 
030400                                                                          
030500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
030600     MOVE '001' TO MSGI-KDCALL                                            
030700     MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                             
030800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030900     MOVE '4622' TO MSGI-IDTRANS                                          
031000     IF EGEN-MID                                                          
031100       MOVE MID-IDTRPTNR-IN TO MSGI-IDTRPTNR                              
031200       MOVE MID-IDLBBET-IN  TO MSGI-IDLBBET                               
031300       MOVE MID-TISKEPPN-IN TO MSGI-TISKEPPN                              
031400       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
031500     END-IF                                                               
031600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
031700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
031800                                                                          
031900     IF SPAR-IDTRANS NOT NUMERIC                                          
032000       MOVE ZEROES TO SPAR-IDTRANS                                        
032100     END-IF                                                               
032200                                                                          
032300*    --- WMEDKONV LANGUAGE                                                
032400     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
032500                                                                          
032600     MOVE JA TO NYCKLAR-SW                                                
032700                                                                          
032800     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
032900                             MOD-IDLBBET-IN                               
033000                             MOD-TISKEPPN-IN                              
033100                             MOD-IDDC-IN                                  
033200                                                                          
033300*    --- TRANSPORTNUMBER                                                  
033400                                                                          
033500     IF MID-IDTRPTNR-IN  NOT = ALL '+'                                    
033600       MOVE '7' TO MFS-IDPFK                                              
033700       MOVE SPACE TO MFS-KDTRTYP                                          
033800     END-IF                                                               
033900                                                                          
034000     INSPECT MSGI-IDTRPTNR REPLACING LEADING SPACE BY ZERO                
034100     IF MSGI-IDTRPTNR NUMERIC AND MSGI-IDTRPTNR > ZERO                    
034200       MOVE MSGI-IDTRPTNR TO W-IDTRPTNR-MIN                               
034300                             W-IDTRPTNR-MAX                               
034310                             W-IDTRPTNR-ASEQ-MIN                          
034320                             W-IDTRPTNR-ASEQ-MAX                          
034400     ELSE                                                                 
034500       MOVE NEJ TO NYCKLAR-SW                                             
034600     END-IF                                                               
034700                                                                          
034800*    --- CARRIER                                                          
034900                                                                          
035000     IF MID-IDLBBET-IN  NOT = ALL '+'                                     
035100       MOVE '7' TO MFS-IDPFK                                              
035200       MOVE SPACE TO MFS-KDTRTYP                                          
035300     END-IF                                                               
035400                                                                          
035500     IF MSGI-IDLBBET > SPACE                                              
035600       MOVE MSGI-IDLBBET TO W-IDLBBET-MIN                                 
035700                            W-IDLBBET-MAX                                 
035710                            W-IDLBBET-ASEQ-MIN                            
035730                            W-IDLBBET-ASEQ-MAX                            
035800     END-IF                                                               
035900                                                                          
036000*    --- SHIPPINGDATE                                                     
036100                                                                          
036200     IF MID-TISKEPPN-IN  NOT = ALL '+'                                    
036300       MOVE '7' TO MFS-IDPFK                                              
036400       MOVE SPACE TO MFS-KDTRTYP                                          
036500     END-IF                                                               
036600                                                                          
036700     INSPECT MSGI-TISKEPPN REPLACING LEADING SPACE BY ZERO                
036800     IF MSGI-TISKEPPN NUMERIC                                             
036900       MOVE MSGI-TISKEPPN TO W-TISKEPPN                                   
037000     ELSE                                                                 
037100       MOVE NEJ TO NYCKLAR-SW                                             
037200     END-IF                                                               
037300                                                                          
037400     IF W-TISKEPPN > ZERO                                                 
037500       IF W-IDLBBET-MIN <= SPACE                                          
037600         MOVE JA TO DATE-INPUT-SW                                         
037700       END-IF                                                             
037800     END-IF                                                               
037900                                                                          
038000*    --- DC                                                               
038100                                                                          
038200     IF MID-IDDC-IN  NOT = ALL '+'                                        
038300       MOVE '7' TO MFS-IDPFK                                              
038400       MOVE SPACE TO MFS-KDTRTYP                                          
038500     END-IF                                                               
038600                                                                          
038700     MOVE MSGI-IDDC-KEY TO WS-IDDC                                        
038800                           W-IDDC-B6                                      
038810     PERFORM IMS-GU-WDB601                                                
038820     IF DCS-KDDC = SPACE                                                  
039100        MOVE MSGI-IDDC TO WS-IDDC                                         
039200     END-IF                                                               
039300     MOVE WS-IDDC TO W-IDDC-MIN                                           
039400                     W-IDDC-MAX                                           
039410                     W-IDDC-ASEQ-MIN                                      
039420                     W-IDDC-ASEQ-MAX                                      
039430                     W-IDDC-B6                                            
039530                                                                          
039600     IF SPAR-IDTRANS = '4622' OR '4621'                                   
039700       CONTINUE                                                           
039800     ELSE                                                                 
039900       MOVE SPACE TO MFS-KDTRTYP                                          
040000       MOVE '7' TO MFS-IDPFK                                              
040100     END-IF                                                               
040200                                                                          
040300     PERFORM BA-CHECK-IDDC-REC                                            
040400                                                                          
040500     IF GODK-MID OR NYCKLAR-OK                                            
040600       MOVE MSGI-IDTRPTNR   TO MOD-IDTRPTNR-UT                            
040700       MOVE MSGI-IDLBBET    TO MOD-IDLBBET-UT                             
040800       MOVE MSGI-TISKEPPN   TO MOD-TISKEPPN-UT                            
040900       MOVE WS-IDDC         TO MOD-IDDC-UT                                
041000     ELSE                                                                 
041100       MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-UT                            
041200       MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-UT                             
041300       MOVE MFS-RENSA-FAELT TO MOD-TISKEPPN-UT                            
041400       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
041500     END-IF                                                               
041510     IF DATE-INPUT                                                        
041520       MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-UT                             
041530     ELSE                                                                 
041540       MOVE MFS-RENSA-FAELT TO MOD-TISKEPPN-UT                            
041550     END-IF                                                               
041600                                                                          
041700     IF NYCKLAR-FEL                                                       
041800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
041900       CALL WMEDKONV USING MED-WMEDAREA                                   
042000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
042100       PERFORM MFS-RENSA-FAELT-IN                                         
042200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600                                                                          
042700 BA-CHECK-IDDC-REC SECTION.                                               
042800                                                                          
042900     IF MID-IDDC-REC > '++'                                               
043000       MOVE MID-IDDC-REC        TO MOD-IDDC-REC                           
043100*      MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDC-REC                           
043200       MOVE MID-IDDC-REC        TO W-IDDC-REC                             
043300     ELSE                                                                 
043400       MOVE MSGI-IDDC           TO MOD-IDDC-REC                           
043500       MOVE MSGI-IDDC           TO W-IDDC-REC                             
043600     END-IF                                                               
043700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-REC-ATTR                      
043800     .                                                                    
043900     EJECT                                                                
044000                                                                          
044100 C-FOERSTA-SIDA SECTION.                                                  
044200                                                                          
044300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
044400     CALL WMEDKONV USING MED-WMEDAREA                                     
044500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
044600                                                                          
044700     PERFORM MFS-RENSA-FAELT-IN                                           
044800     .                                                                    
044900     EJECT                                                                
045000 D-NAESTA-SIDA SECTION.                                                   
045100                                                                          
045200     IF SPAR-IDTRANS = '4622'                                             
045300       MOVE SPAR-IDSHIPM-NEXT TO W-IDSHIPM-PF                             
045400     ELSE                                                                 
045500       MOVE ERR-LAST-PAGE-SHOWN TO MED-IDMFSFEL                           
045600       CALL WMEDKONV USING MED-WMEDAREA                                   
045700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
045800       PERFORM MFS-RENSA-FAELT-IN                                         
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 E-SAMMA-SIDA SECTION.                                                    
046300                                                                          
046400     IF SPAR-IDTRANS = '4622' OR '0551'                                   
046500       MOVE SPAR-IDSHIPM-ENTER TO W-IDSHIPM-PF                            
046600       IF MID-W4I62201 = ALL '+'                                          
046700         PERFORM MFS-RENSA-FAELT-IN                                       
046800       ELSE                                                               
046900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
047000         PERFORM EA-MID-INDATA-TILL-MOD                                   
047100                                                                          
047200         MOVE +1 TO INDX                                                  
047300         PERFORM UNTIL INDX > MAX-INDX                                    
047400           IF MID-KDCMD (INDX) = 'S'                                      
047500             MOVE INF-PRESS-PF9-TO-SPLIT TO MED-IDMFSFEL                  
047600             CALL WMEDKONV          USING MED-WMEDAREA                    
047700             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
047800           ELSE                                                           
047900             IF MID-KDCMD (INDX) = 'P'                                    
048000               MOVE INF-PRESS-PF4-TO-PRINT TO MED-IDMFSFEL                
048100               CALL WMEDKONV          USING MED-WMEDAREA                  
048200               MOVE MED-MFSFEL           TO MOD-TEMFSFEL                  
048300             ELSE                                                         
048400               IF MID-KDCMD (INDX) NOT = ALL '+'                          
048500                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
048600                 CALL WMEDKONV          USING MED-WMEDAREA                
048700                 MOVE MED-MFSFEL           TO MOD-TEMFSFEL                
048800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)         
048900               END-IF                                                     
049000             END-IF                                                       
049100           END-IF                                                         
049200           ADD 1 TO INDX                                                  
049300         END-PERFORM                                                      
049400                                                                          
049500         IF MID-IDLTERM NOT = ALL '+'                                     
049600           MOVE INF-PRESS-PF4-TO-PRINT TO MED-IDMFSFEL                    
049700           CALL WMEDKONV          USING MED-WMEDAREA                      
049800           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
049900         END-IF                                                           
050000         MOVE NEJ TO INDATA-SW                                            
050100       END-IF                                                             
050200     ELSE                                                                 
050300       PERFORM MFS-RENSA-FAELT-IN                                         
050400     END-IF                                                               
050500     .                                                                    
050600     EJECT                                                                
050700 EA-MID-INDATA-TILL-MOD SECTION.                                          
050800                                                                          
050900     MOVE 1 TO INDX                                                       
051000                                                                          
051100     PERFORM UNTIL INDX > MAX-INDX                                        
051200                                                                          
051300       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
051400         MOVE MID-KDCMD (INDX) TO MOD-KDCMD (INDX)                        
051500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)              
051600       ELSE                                                               
051700         MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                         
051800       END-IF                                                             
051900                                                                          
052000       ADD 1 TO INDX                                                      
052100                                                                          
052200     END-PERFORM                                                          
052300                                                                          
052400     IF MID-IDLTERM  NOT = ALL '+'                                        
052500       MOVE MID-IDLTERM TO MOD-IDLTERM                                    
052600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLTERM-ATTR                     
052700     ELSE                                                                 
052800       MOVE MFS-RENSA-FAELT TO MOD-IDLTERM                                
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200                                                                          
053300 F-LAES-VISA-INFO SECTION.                                                
053400                                                                          
053500     IF MFS-NEXT OR MFS-ENTER OR MFS-PRINT OR 4621-MID                    
053600       PERFORM IMS-GU-WDE101-PF                                           
053601       IF SEGMENT-SAKNAS                                                  
053602         MOVE ZERO    TO SHIP-TISKPTID                                    
053603       ELSE                                                               
053604         MOVE SHIP-TISKPTID  TO W-TISKPTID                                
053605         MOVE SHIP-IDLBBET TO W-IDLBBET-MIN                               
053606         MOVE SHIP-IDLBBET TO W-IDLBBET-ASEQ-MIN                          
053607         MOVE SHIP-TISKEPPN TO W-TISKEPPN-ASEQ-MIN                        
053608         MOVE SHIP-TISKEPPN TO W-TISKEPPN-MIN                             
053609         MOVE SHIP-TISKPTID TO W-TISKPTID-MIN                             
053610       END-IF                                                             
053620       IF NOT DATE-INPUT                                                  
053621         PERFORM IMS-GU-WDE1A1                                            
053622         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
053623         PERFORM IMS-GU-WDE101                                            
053630       ELSE                                                               
053640         MOVE SHIP-TISKPTID  TO W-TISKPTID                                
053650         PERFORM IMS-GU-WDE1A1-DATE-TID                                   
053660         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
053670         PERFORM IMS-GU-WDE101                                            
053680       END-IF                                                             
053690     ELSE                                                                 
053691       IF DATE-INPUT                                                      
053692*LN LÄSNINGEN ÄNDRAD                                                      
053693         PERFORM IMS-GU-WDE1A1-DATE                                       
053694         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
053695         PERFORM IMS-GU-WDE101                                            
053696       ELSE                                                               
053698         PERFORM IMS-GU-WDE1A1                                            
053699         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
053700         PERFORM IMS-GU-WDE101                                            
053800       END-IF                                                             
054300     END-IF                                                               
054400                                                                          
054500     IF SEGMENT-SAKNAS                                                    
054600       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
054700       CALL WMEDKONV USING MED-WMEDAREA                                   
054800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
054900     ELSE                                                                 
055000       MOVE SHIP-IDSHIPM TO SPAR-IDSHIPM-ENTER                            
055010       MOVE SHIP-IDLBBET TO W-IDLBBET-MIN                                 
055020       MOVE SHIP-IDLBBET TO W-IDLBBET-ASEQ-MIN                            
055030       MOVE SHIP-TISKEPPN TO W-TISKEPPN-ASEQ-MIN                          
055040       MOVE SHIP-TISKEPPN TO W-TISKEPPN-MIN                               
055050       MOVE SHIP-TISKPTID TO W-TISKPTID-MIN                               
055100     END-IF                                                               
055200                                                                          
055300     MOVE +1 TO INDX                                                      
055400     PERFORM UNTIL INDX > MAX-INDX                                        
055500       IF SEGMENT-FINNS                                                   
055600         MOVE SHIP-IDLBBET    TO MOD-IDLBBET (INDX)                       
055700         MOVE SHIP-TISKEPPN   TO MOD-TISKEPPN (INDX)                      
055800         MOVE SHIP-TISKPTID   TO MOD-TISKPTID (INDX)                      
055900         MOVE SHIP-IDSHIPM    TO MOD-IDSHIPM (INDX)                       
056000                                 SPAR-IDSHIPM (INDX)                      
056100         MOVE SHIP-KVANTEX    TO MOD-KVANTEX (INDX)                       
056200       ELSE                                                               
056300         MOVE MFS-RENSA-FAELT TO MOD-IDLBBET (INDX)                       
056400         MOVE MFS-RENSA-FAELT TO MOD-TISKEPPN (INDX)                      
056500         MOVE MFS-RENSA-FAELT TO MOD-TISKPTID (INDX)                      
056600         MOVE MFS-RENSA-FAELT TO MOD-IDSHIPM (INDX)                       
056700         MOVE ZERO            TO SPAR-IDSHIPM (INDX)                      
056800         MOVE MFS-RENSA-FAELT TO MOD-KVANTEX (INDX)                       
056900         MOVE MFS-STAENG-FAELT                                            
057000                              TO MOD-KDCMD-ATTR (INDX)                    
057100       END-IF                                                             
057200                                                                          
057300       MOVE MFS-RENSA-FAELT   TO MOD-KDCMD (INDX)                         
057400       ADD 1 TO INDX                                                      
057500                                                                          
057600       IF DATE-INPUT                                                      
057700         PERFORM IMS-GN-WDE1A1-DATE                                       
057710         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
057711         IF SEGMENT-FINNS                                                 
057720           PERFORM IMS-GU-WDE101                                          
057730         END-IF                                                           
057800       ELSE                                                               
057910         PERFORM IMS-GN-WDE1A1                                            
057920         MOVE SEQA-IDSHIPM TO W-IDSHIPM                                   
057930         IF SEGMENT-FINNS                                                 
057940           PERFORM IMS-GU-WDE101                                          
057950         END-IF                                                           
058000       END-IF                                                             
058100                                                                          
058200     END-PERFORM                                                          
058300                                                                          
058400     MOVE MFS-RENSA-FAELT     TO MOD-IDLTERM                              
058500                                                                          
058600     IF SEGMENT-FINNS                                                     
058700       MOVE SHIP-IDSHIPM TO SPAR-IDSHIPM-NEXT                             
058800       IF NOT MFS-PRINT                                                   
058900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
059000         CALL WMEDKONV USING MED-WMEDAREA                                 
059100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
059200       END-IF                                                             
059300     ELSE                                                                 
059400       MOVE SPAR-IDSHIPM-ENTER TO SPAR-IDSHIPM-NEXT                       
059500       IF NOT MFS-PRINT                                                   
059600         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
059700         CALL WMEDKONV USING MED-WMEDAREA                                 
059800         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
059900       END-IF                                                             
060000     END-IF                                                               
060100                                                                          
060200     MOVE '002' TO MSGI-KDCALL                                            
060300     MOVE '4622' TO SPAR-IDTRANS                                          
060400     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
060500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
060600     .                                                                    
060700     EJECT                                                                
060800 G-CHECK-PRINT SECTION.                                                   
060900                                                                          
061000     IF SPAR-IDTRANS = '4622' OR '0551'                                   
061100       MOVE SPAR-IDSHIPM-ENTER TO W-IDSHIPM-PF                            
061200     END-IF                                                               
061300                                                                          
061400     MOVE JA  TO INDATA-SW                                                
061500     MOVE +1  TO INDX                                                     
061600     PERFORM UNTIL INDX > MAX-INDX                                        
061700                                                                          
061800                                                                          
061900       IF MID-KDCMD (INDX) = '+' OR 'P'                                   
062000         IF MID-KDCMD (INDX) = 'P'                                        
062100           IF KDCMD-GIVEN                                                 
062200             MOVE NEJ TO INDATA-SW                                        
062300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                    
062400             CALL WMEDKONV USING MED-WMEDAREA                             
062500             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
062600             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)             
062700           ELSE                                                           
062800             MOVE JA TO KDCMD-SW                                          
062900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)           
063000             MOVE SPAR-IDSHIPM (INDX)  TO W-IDSHIPM-PRINT                 
063100           END-IF                                                         
063200         END-IF                                                           
063300       ELSE                                                               
063400         MOVE JA TO KDCMD-SW                                              
063500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                        
063600         CALL WMEDKONV USING MED-WMEDAREA                                 
063700         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
063800         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)                 
063900         MOVE NEJ TO INDATA-SW                                            
064000       END-IF                                                             
064100       ADD 1 TO INDX                                                      
064200     END-PERFORM                                                          
064300                                                                          
064400     IF INDATA-OK                                                         
064500       IF MID-IDLTERM NOT = ALL '+'                                       
064600         MOVE MID-IDLTERM TO PRT-IDLTERM                                  
064700         MOVE 2 TO PRT-KDCALL                                             
064800         CALL W006PRT USING PRT-W006PRT                                   
064900         IF PRT-KDSVAR = 'F'                                              
065000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLTERM-ATTR                    
065100           MOVE ERR-WRONG-PRINTER TO MED-IDMFSFEL                         
065200           CALL WMEDKONV USING MED-WMEDAREA                               
065300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
065400           MOVE NEJ        TO INDATA-SW                                   
065500         ELSE                                                             
065600           MOVE PRT-IDPRTLST  TO W-IDPRTLST                               
065700         END-IF                                                           
065800       END-IF                                                             
065900                                                                          
066000       MOVE W-IDDC-REC             TO WS-IDDC                             
066010                                      W-IDDC-B6                           
066020       PERFORM IMS-GU-WDB601                                              
066030       IF DCS-KDDC = SPACE                                                
066400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-REC-ATTR                   
066500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                        
066600         CALL WMEDKONV          USING MED-WMEDAREA                        
066700         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
066800         MOVE NEJ                  TO INDATA-SW                           
066900       END-IF                                                             
067000     END-IF                                                               
067100                                                                          
067200     IF NOT KDCMD-GIVEN                                                   
067300       MOVE ERR-PF4-AND-NO-CMD     TO MED-IDMFSFEL                        
067400       CALL WMEDKONV USING MED-WMEDAREA                                   
067500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
067600       MOVE NEJ TO INDATA-SW                                              
067700     END-IF                                                               
067800                                                                          
067900     IF INDATA-OK                                                         
068000       PERFORM GA-START-PRINT-4631                                        
068100     ELSE                                                                 
068200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
068300     END-IF                                                               
068400     .                                                                    
068500     EJECT                                                                
068600 GA-START-PRINT-4631 SECTION.                                             
068700                                                                          
068800     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLTERM-ATTR                        
068900     MOVE INF-PRINT-REQUESTED TO MED-IDMFSINF                             
069000     CALL WMEDKONV USING MED-WMEDAREA                                     
069100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
069200                                                                          
069300     PERFORM S01-OPEN-WZ01                                                
069400     PERFORM S02-SEND-WZ01                                                
069500     PERFORM S03-CLOSE-WZ01                                               
069600                                                                          
069700     PERFORM MFS-FORM-ATTR                                                
069800     .                                                                    
069900     EJECT                                                                
070000 I-SPLIT-TO-W40621 SECTION.                                               
070100                                                                          
070200     MOVE +1 TO INDX                                                      
070300     PERFORM UNTIL INDX > MAX-INDX                                        
070400       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
070500         IF KDCMD-GIVEN                                                   
070600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
070700           CALL WMEDKONV USING MED-WMEDAREA                               
070800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
070900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)               
071000           MOVE NEJ TO INDATA-SW                                          
071100         ELSE                                                             
071200           MOVE JA TO KDCMD-SW                                            
071300           IF MID-KDCMD (INDX) = 'S'                                      
071400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)           
071500             PERFORM IA-BUILD-4621                                        
071600           ELSE                                                           
071700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
071800             CALL WMEDKONV USING MED-WMEDAREA                             
071900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
072000             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)             
072100             MOVE NEJ TO INDATA-SW                                        
072200           END-IF                                                         
072300         END-IF                                                           
072400       END-IF                                                             
072500       ADD 1 TO INDX                                                      
072600     END-PERFORM                                                          
072700                                                                          
072800     IF NOT KDCMD-GIVEN                                                   
072900       MOVE ERR-SELECT-LINE        TO MED-IDMFSFEL                        
073000       CALL WMEDKONV USING MED-WMEDAREA                                   
073100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
073200       MOVE NEJ TO INDATA-SW                                              
073300     END-IF                                                               
073400                                                                          
073500     IF INDATA-FEL                                                        
073600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 IA-BUILD-4621  SECTION.                                                  
074200                                                                          
074300     MOVE ALL '+'                TO PTOP-MID-W4I62101                     
074400                                                                          
074500     IF SPAR-IDSHIPM (INDX) NUMERIC                                       
074600       IF SPAR-IDSHIPM (INDX) > ZERO                                      
074700         MOVE SPAR-IDSHIPM (INDX) TO PTOP-MID-IDSHIPM-IN                  
074800         MOVE LOW-VALUE            TO PTOP-Z1                             
074900         MOVE LOW-VALUE            TO PTOP-Z2                             
075000         MOVE 'W4T621  '           TO PTOP-KDTRANS                        
075100         MOVE '4622'               TO PTOP-IDTRANS                        
075200         MOVE MFS-KDMFSFOR         TO PTOP-KDMFSFOR                       
075300       ELSE                                                               
075400         MOVE NEJ TO INDATA-SW                                            
075500       END-IF                                                             
075600     ELSE                                                                 
075700       MOVE NEJ TO INDATA-SW                                              
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100                                                                          
076200 S01-OPEN-WZ01 SECTION.                                                   
076300                                                                          
076400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
076500     MOVE 'CARPARTS.PULS.SHIPDOK '   TO SEND-ADDISPABS                    
076600     MOVE 'CARPARTS.PULS.STAPRSHIPDOK'                                    
076700                                     TO SEND-ADDISPABS-RETURN             
076800                                                                          
076900     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
077000                                SEND-OPEN-AREA                            
077100     IF SEND-KDRC > 0                                                     
077200       MOVE SEND-KDRC           TO KDRC-DISP                              
077300       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISP                         
077400            DELIMITED BY SIZE INTO FELTEXT                                
077600       CALL FELLOG                                                        
077700     ELSE                                                                 
077800       MOVE SEND-IDCOM               TO WS-IDCOM                          
077900     END-IF                                                               
078000     .                                                                    
078100     EJECT                                                                
078200                                                                          
078300 S02-SEND-WZ01 SECTION.                                                   
078400                                                                          
078500     MOVE W-IDSHIPM-PRINT            TO 4631-MID-IDSHIPM                  
078600     IF W-IDPRTLST  > SPACES                                              
078700       MOVE W-IDPRTLST               TO 4631-MID-IDPRTLST                 
078800     ELSE                                                                 
078900       MOVE ALL '+'                  TO 4631-MID-IDPRTLST                 
079000     END-IF                                                               
079200     IF W-IDDC-REC  > SPACES                                              
079300       MOVE W-IDDC-REC               TO 4631-MID-IDDC-REC                 
079400     ELSE                                                                 
079500       MOVE ALL '+'                  TO 4631-MID-IDDC-REC                 
079600     END-IF                                                               
079610     MOVE IDPGM                      TO 4631-MID-IDPGM                    
079700                                                                          
079800     MOVE 'PUT'                      TO SEND-KDFUNC                       
079900     COMPUTE SEND-KVDLEN = LENGTH OF 4631-MID                             
080000                                                                          
080100     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
080200                                SEND-KVDLEN                               
080300                                4631-MID                                  
080400     IF SEND-KDRC > 0                                                     
080500       MOVE SEND-KDRC           TO KDRC-DISP                              
080600       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISP                          
080700            DELIMITED BY SIZE INTO FELTEXT                                
080900       CALL FELLOG                                                        
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 S03-CLOSE-WZ01  SECTION.                                                 
081400                                                                          
081500     MOVE 'CLOSE'               TO SEND-KDFUNC                            
081600                                                                          
081700     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
081800     IF SEND-KDRC > 0                                                     
081900       MOVE SEND-KDRC           TO KDRC-DISP                              
082000       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISP                        
082100            DELIMITED BY SIZE INTO FELTEXT                                
082300       CALL FELLOG                                                        
082400     END-IF                                                               
082500     .                                                                    
082600     EJECT                                                                
082700 J-RETURN-FROM-4621 SECTION.                                              
082800                                                                          
082900     MOVE SPAR-IDSHIPM-ENTER   TO W-IDSHIPM-PF                            
083000                                                                          
083100     .                                                                    
083200     EJECT                                                                
083300                                                                          
083400*MFS-RENSA-FAELT-UT SECTION.                                              
083500*    --- ALL OUTDATAFIELDS INCLUDING TURN PAGE KEY                        
083600*    MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-UT                              
083700*                            MOD-IDLBBET-UT                               
083800*                            MOD-TISKEPPN-UT                              
083900*                            MOD-IDLTERM                                  
084000*                                                                         
084100*    PERFORM MFS-RENSA-RAD-FAELT-UT                                       
084200*    .                                                                    
084300     SKIP3                                                                
084400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
084500*    --- ALL LINEFIELDS                                                   
084600     MOVE 1 TO INDX                                                       
084700     PERFORM UNTIL INDX > MAX-INDX                                        
084800       MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                           
084900                               MOD-IDLBBET (INDX)                         
085000                               MOD-TISKEPPN (INDX)                        
085100                               MOD-TISKPTID (INDX)                        
085200                               MOD-IDSHIPM (INDX)                         
085300                               MOD-KVANTEX (INDX)                         
085400       ADD +1 TO INDX                                                     
085500     END-PERFORM                                                          
085600     .                                                                    
085700     SKIP3                                                                
085800 MFS-RENSA-FAELT-IN SECTION.                                              
085900*    --- ALL INDATAFIELDS                                                 
086000     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
086100                             MOD-IDLBBET-IN                               
086200                             MOD-TISKEPPN-IN                              
086300                             MOD-IDLTERM                                  
086400     .                                                                    
086500     EJECT                                                                
086600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
086700*    --- ALL OUTDATAFIELDS                                                
086800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDTRPTNR-UT                            
086900                               MOD-IDLBBET-UT                             
087000                               MOD-TISKEPPN-UT                            
087100                               MOD-IDLTERM                                
087200                                                                          
087300     MOVE +1 TO INDX                                                      
087400     PERFORM UNTIL INDX > MAX-INDX                                        
087500       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
087600       ADD +1 TO INDX                                                     
087700     END-PERFORM                                                          
087800     .                                                                    
087900     SKIP2                                                                
088000 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
088100*    ---ALL LINEFIELDS                                                    
088200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)                           
088300                               MOD-IDLBBET (INDX)                         
088400                               MOD-TISKEPPN (INDX)                        
088500                               MOD-TISKPTID (INDX)                        
088600                               MOD-IDSHIPM (INDX)                         
088700                               MOD-KVANTEX (INDX)                         
088800     .                                                                    
088900     SKIP3                                                                
089000                                                                          
089100 MFS-FORM-ATTR  SECTION.                                                  
089200*    --- ALL INDATAFIELDS                                                 
089300     MOVE 1 TO INDX                                                       
089400     PERFORM UNTIL INDX > MAX-INDX                                        
089500       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (INDX)                   
089600       ADD 1 TO INDX                                                      
089700     END-PERFORM                                                          
089800                                                                          
089900     MOVE MFS-FORMATETS-ATTR TO  MOD-IDLTERM-ATTR                         
090000     .                                                                    
090100     EJECT                                                                
090200                                                                          
090300*MFS-LAES-IN-IGEN SECTION.                                                
090400*    --- ALL INDATAFIELDS                                                 
090500*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)                  
090600*                                  MOD-IDLTERM-ATTR                       
090700*    .                                                                    
090800     EJECT                                                                
090900* --- IMS SECTIONS                                                        
091000     SKIP3                                                                
091100 IMS-GET-MSG SECTION.                                                     
091200                                                                          
091300     MOVE '  QC' TO GODK-STATUSKODER                                      
091400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     SKIP3                                                                
091900 IMS-INSERT-MSG SECTION.                                                  
092000                                                                          
092100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
092200     MOVE SPACE TO GODK-STATUSKODER                                       
092300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
092400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     EJECT                                                                
092800 IMS-ISRT-ALT-MSG-4621 SECTION.                                           
092900                                                                          
093000     MOVE SPACE TO GODK-STATUSKODER                                       
093100     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW-4621                       
093200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
093300     PERFORM IMS-STATUSKONTROLL                                           
093400     .                                                                    
093500     SKIP3                                                                
093600 IMS-GU-WDE101-PF SECTION.                                                
093700                                                                          
093800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-PF-X ')'                      
093900          DELIMITED BY SIZE INTO SSA1                                     
094000     MOVE '  GE' TO GODK-STATUSKODER                                      
094100     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
094200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     SKIP3                                                                
094511 IMS-GU-WDE101 SECTION.                                                   
094520                                                                          
094530     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
094540          DELIMITED BY SIZE INTO SSA1                                     
094550     MOVE '  GE' TO GODK-STATUSKODER                                      
094560     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
094570     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
094580     PERFORM IMS-STATUSKONTROLL                                           
094590     .                                                                    
094591     SKIP3                                                                
094592 IMS-GU-WDE1A1 SECTION.                                                   
094593                                                                          
094594     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
094595                    '&WDE1A1KY<=' W-WDE1A1KY-MAX ')'                      
094597          DELIMITED BY SIZE INTO SSA1                                     
094598     MOVE '  GE' TO GODK-STATUSKODER                                      
094599     CALL CBLTDLI USING GU WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
094600     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
094601     PERFORM IMS-STATUSKONTROLL                                           
094602     .                                                                    
094603     SKIP3                                                                
094604 IMS-GN-WDE1A1 SECTION.                                                   
094605                                                                          
094606     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
094607                    '&WDE1A1KY<=' W-WDE1A1KY-MAX ')'                      
094608          DELIMITED BY SIZE INTO SSA1                                     
094609     MOVE '  GE' TO GODK-STATUSKODER                                      
094610     CALL CBLTDLI USING GN WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
094611     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
094612     PERFORM IMS-STATUSKONTROLL                                           
094613     .                                                                    
094614     SKIP3                                                                
094620 IMS-GU-WDE1A1-DATE SECTION.                                              
094700                                                                          
094800     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
094900                    '&WDE1A1KY<=' W-WDE1A1KY-MAX                          
095000                    '&TISKEPPN =' W-TISKEPPN-X ')'                        
095100          DELIMITED BY SIZE INTO SSA1                                     
095200     MOVE '  GE' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GU WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
095400     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     .                                                                    
095700     SKIP3                                                                
095710 IMS-GU-WDE1A1-DATE-TID SECTION.                                          
095720                                                                          
095730     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
095740                    '&WDE1A1KY<=' W-WDE1A1KY-MAX                          
095750                    '&TISKEPPN =' W-TISKEPPN-X                            
095760                    '&TISKPTID =' W-TISKPTID-X ')'                        
095770          DELIMITED BY SIZE INTO SSA1                                     
095780     MOVE '  GE' TO GODK-STATUSKODER                                      
095790     CALL CBLTDLI USING GU WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
095791     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
095792     PERFORM IMS-STATUSKONTROLL                                           
095793     .                                                                    
095794     SKIP3                                                                
096801 IMS-GU-WDE101-ASEQ-START  SECTION.                                       
096802                                                                          
096803     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-PF-X ')'                      
096804          DELIMITED BY SIZE INTO SSA1                                     
096805     MOVE '  GE' TO GODK-STATUSKODER                                      
096806     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
096807     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
096808     PERFORM IMS-STATUSKONTROLL                                           
096809     .                                                                    
096810     SKIP3                                                                
096811 IMS-GN-WDE1A1-DATE SECTION.                                              
096820                                                                          
096830     STRING 'WDE1A1  (WDE1A1KY>=' W-WDE1A1KY-MIN                          
096840                    '&WDE1A1KY<=' W-WDE1A1KY-MAX                          
096850                    '&TISKEPPN =' W-TISKEPPN-X ')'                        
096860          DELIMITED BY SIZE INTO SSA1                                     
096870     MOVE '  GE' TO GODK-STATUSKODER                                      
096880     CALL CBLTDLI USING GN WDE1A-PCB DLI-IO-WDE1A1 SSA1                   
096890     MOVE WDE1A-STATUS-CODE TO STATUS-WS                                  
096891     PERFORM IMS-STATUSKONTROLL                                           
096892     .                                                                    
096893     SKIP3                                                                
099100 IMS-GU-WDB601    SECTION.                                                
099110                                                                          
099120     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
099130     DELIMITED BY SIZE INTO SSA1                                          
099140     MOVE '  GE' TO GODK-STATUSKODER                                      
099150     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
099160     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
099170     PERFORM IMS-STATUSKONTROLL                                           
099180     IF SEGMENT-SAKNAS                                                    
099190        MOVE SPACE TO DCS-KDDC                                            
099191     END-IF                                                               
099192     .                                                                    
099193     EJECT                                                                
099200 IMS-STATUSKONTROLL SECTION.                                              
099300                                                                          
099400     SET STATUS-IX TO 1                                                   
099500     SEARCH GODK-STATUS                                                   
099600       AT END                                                             
099700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
099800         DELIMITED BY SIZE INTO FELTEXT                                   
099900         CALL FELLOG                                                      
100000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100100         CONTINUE                                                         
100200     END-SEARCH                                                           
100300     .                                                                    
