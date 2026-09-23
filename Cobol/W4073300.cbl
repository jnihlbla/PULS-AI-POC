000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0148      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4073300.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/06/15.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        REGISTRERING AV MOTTAGNA RETURER HOS CDC, LDC OCH NDC.           
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001700*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001800*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
001900*        PROGRAMMET LÄSER      WLRETG (WDA3)                              
002000*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
002100*        PROGRAMMET LÄSER      WL4103 (WDR1)                              
002200*                                                                         
002300*    E-TRACKER: 4230251  2006-12  LDC-3                                   
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T733                                              
002700*        MID:         W4I73301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O73301                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4073300'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  W-CDC                       PIC X(3)    VALUE 'CDC'.                 
004800 77  W-US1                       PIC X(3)    VALUE 'US1'.                 
004900 77  W-US2                       PIC X(3)    VALUE 'US2'.                 
005000 77  W-US3                       PIC X(3)    VALUE 'US3'.                 
005100 77  W-CA1                       PIC X(3)    VALUE 'CA1'.                 
005200 77  W-JP1                       PIC X(3)    VALUE 'JP1'.                 
005300 77  W-AU1                       PIC X(3)    VALUE 'AU1'.                 
005400 77  W-SE1                       PIC X(3)    VALUE 'SE1'.                 
005500 77  W-GB1                       PIC X(3)    VALUE 'GB1'.                 
005600 77  W-SE2                       PIC X(3)    VALUE 'SE2'.                 
005700 77  W-GB2                       PIC X(3)    VALUE 'GB2'.                 
005800 77  W-GB3                       PIC X(3)    VALUE 'GB3'.                 
005900 77  W-NL1                       PIC X(3)    VALUE 'NL1'.                 
006000 77  W-IT1                       PIC X(3)    VALUE 'IT1'.                 
006100 77  W-PLUS                      PIC X       VALUE '+'.                   
006200                                                                          
006300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006500 77  INDX2                       PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800                                                                          
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 77  VISA-SW                     PIC X       VALUE 'J'.                   
007400     88  VISA-OK                             VALUE 'J'.                   
007500     88  VISA-EJ                             VALUE 'N'.                   
007600                                                                          
007700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007800     88  NYCKLAR-OK                          VALUE 'J'.                   
007900     88  NYCKLAR-FEL                         VALUE 'N'.                   
008000                                                                          
008100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008200     88  EGEN-MID                            VALUE '4733'.                
008300     88  GODK-MID                            VALUE '4733'.                
008400     88  HELP-MID                            VALUE '0551'.                
008500 77  W-FLFARLIG                  PIC X(1).                                
008510 77  W-FLBUYBAC                  PIC X(1).                                
008600 77  W-IDPERSON                  PIC S9(3)   COMP-3.                      
008700 77  W-KDARBTYP                  PIC X(8).                                
008800 77  W-IDRTLOP-NUM               PIC  9(3)          VALUE ZERO.           
008900 77  W-KVRADER                   PIC S9(5)   COMP-3 VALUE ZERO.           
009000 77  W-SND-SAENT                 PIC X(1)           VALUE '2'.            
009100 77  W-SND-LOSS                  PIC X(1)           VALUE '3'.            
009200 77  W-KLI-SAENT                 PIC S9(1)   COMP-3 VALUE +4.             
009300 77  W-KLI-SAK                   PIC S9(1)   COMP-3 VALUE +6.             
009400 77  W-KLI-AVV                   PIC S9(1)   COMP-3 VALUE +7.             
009500 77  W-KVKOLLI-LOSS              PIC S9(5)   COMP-3 VALUE ZERO.           
009600 77  W-KVKOLLI-MOT               PIC S9(5)   COMP-3 VALUE ZERO.           
009700 77  SPAR-IDKOLLI                PIC S9(5)   COMP-3 VALUE ZERO.           
009800 77  WS-IDKOLLI-FOM              PIC S9(5)   COMP-3 VALUE ZERO.           
009900 77  WS-IDKOLLI-TOM              PIC S9(5)   COMP-3 VALUE ZERO.           
010000 77  SPAR-KDRETSTA               PIC  X(1)          VALUE SPACE.          
010100 77  SPAR-KDKOLSTA               PIC S9(1)   COMP-3 VALUE +0.             
010200                                                                          
010300 77  SW-FOERSTA-RT               PIC X       VALUE 'N'.                   
010400     88  FOERSTA-RT                          VALUE 'J'.                   
010500                                                                          
010600 77  SW-IDRETSND                 PIC X       VALUE 'N'.                   
010700     88  IDRETSND-IFYLLT                     VALUE 'J'.                   
010800                                                                          
010900 77  SW-GAMMAL-SND               PIC X       VALUE 'N'.                   
011000     88  GAMMAL-SND                          VALUE 'J'.                   
011100                                                                          
011200 77  SW-SKAPA-NY-SND             PIC X       VALUE 'N'.                   
011300     88  SKAPA-NY-SND                        VALUE 'J'.                   
011400                                                                          
011500 77  SW-LOSS-INFO                PIC X       VALUE 'N'.                   
011600     88  LOSS-INFO                           VALUE 'J'.                   
011700                                                                          
011800 77  SW-MOT-INFO                 PIC X       VALUE 'N'.                   
011900     88  MOT-INFO                            VALUE 'J'.                   
012000                                                                          
012100 77  SW-KOLLI-INFO               PIC X       VALUE 'N'.                   
012200     88  KOLLI-INFO                          VALUE 'J'.                   
012300                                                                          
012400 77  SW-RAD-INFO                 PIC X       VALUE 'N'.                   
012500     88  RAD-INFO                            VALUE 'J'.                   
012501                                                                          
012510 01  W-KDANMORS                  PIC X(2).                                
012520     88  KDANMORS-BUYBAC-98                  VALUE '98'.                  
012530                                                                          
012600*      --- VALID IDDC CODES                                               
012700*                                                                         
012800*01    -COPY WWDC99                                                       
012900       EJECT                                                              
013000                                                                          
013100 01  -COPY WDA301    -PRE SPAR-                                           
013200     EJECT                                                                
013300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013400 01  GENERELLA-SUBPROGRAM.                                                
013500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013900     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
014000     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
014300*01 -COPY W418ANSV                                                        
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL SUBPROGRAM W418OKOD                              
014600*01 -COPY W418OKOD -PRE  OKOD-                                            
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014900*01 -COPY WMEDAREA                                                        
015000     SKIP3                                                                
015100 01  MESSAGE-CODES.                                                       
015200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015300     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
015400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015600     03  ERR-OTILL-STATUS        PIC X(3)    VALUE '079'.                 
015700     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
015800     03  ERR-ANNAN-SHIPPING      PIC X(3)    VALUE '310'.                 
015900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
016200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016500*                                                                         
016600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016700     SKIP3                                                                
016800*01 -COPY WMSGINIT                                                        
016900     SKIP3                                                                
017000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017300     SKIP3                                                                
017400*01  MID -COPY W4I73301                                                   
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017700     SKIP3                                                                
017800*01  -COPY WMSGAREA                                                       
017900     EJECT                                                                
018000     03  MOD REDEFINES MSG-AREA.                                          
018100*      05  -COPY W4O73301  -PRE MOD-                                      
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018400     SKIP3                                                                
018500*01  -COPY WMFSAREA                                                       
018600     EJECT                                                                
018700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018800*                                                                         
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019100     SKIP3                                                                
019200 01  NYCKLAR-TILL-DLI.                                                    
019300     03  W-IDKOLLI-X.                                                     
019400         05  W-IDKOLLI           PIC S9(5)    VALUE ZERO  COMP-3.         
019500                                                                          
019600     03  W-WDA301KY-X.                                                    
019700         05  W-IDDC              PIC  X(2)    VALUE SPACE.                
019800         05  W-DAREGDAT          PIC  9(8)    VALUE ZERO.                 
019900         05  W-TIKLOCK           PIC S9(9)    VALUE ZERO  COMP-3.         
020000                                                                          
020100     03  W-WDA3F1KY-MIN-X.                                                
020200         05  W-IDDC-MIN          PIC  X(2)    VALUE SPACE.                
020300         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
020400         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO  COMP-3.         
020500         05  W-IDRAPPNR-MIN      PIC  9(7)    VALUE ZERO.                 
020600         05  W-IDRT-MIN          PIC  X(3)    VALUE SPACE.                
020700         05  W-IDRTLOP-MIN       PIC  9(3)    VALUE ZERO.                 
020800         05  W-IDKOLLI-MIN       PIC S9(5)    VALUE ZERO  COMP-3.         
020900         05  W-DAREGDAT-MIN      PIC  9(8)    VALUE ZERO.                 
021000         05  W-TIKLOCK-MIN       PIC S9(9)    VALUE ZERO  COMP-3.         
021100                                                                          
021200     03  W-WDA3F1KY-MAX-X.                                                
021300         05  W-IDDC-MAX          PIC  X(2)    VALUE SPACE.                
021400         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
021500         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO  COMP-3.         
021600         05  W-IDRAPPNR-MAX      PIC  9(7)    VALUE ZERO.                 
021700         05  W-IDRT-MAX          PIC  X(3)    VALUE SPACE.                
021800         05  W-IDRTLOP-MAX       PIC  9(3)    VALUE ZERO.                 
021900         05  W-IDKOLLI-MAX       PIC S9(5)    VALUE ZERO  COMP-3.         
022000         05  W-DAREGDAT-MAX      PIC  9(8)    VALUE ZERO.                 
022100         05  W-TIKLOCK-MAX       PIC S9(9)    VALUE ZERO  COMP-3.         
022200                                                                          
022300     03  W-WDA3BSEQ-MIN-X.                                                
022400         05  W-IDRT-BSEQ-MIN      PIC  X(3)          VALUE SPACE.         
022500         05  W-IDDC-BSEQ-MIN      PIC  X(2)          VALUE SPACE.         
022600         05  W-IDRTLOP-BSEQ-MIN   PIC  9(3)          VALUE ZERO.          
022700         05  W-IDKOLLI-BSEQ-MIN   PIC S9(5)   COMP-3 VALUE ZERO.          
022800                                                                          
022900     03  W-WDA3BSEQ-MAX-X.                                                
023000         05  W-IDRT-BSEQ-MAX      PIC  X(3)          VALUE SPACE.         
023100         05  W-IDDC-BSEQ-MAX      PIC  X(2)          VALUE SPACE.         
023200         05  W-IDRTLOP-BSEQ-MAX   PIC  9(3)          VALUE ZERO.          
023300         05  W-IDKOLLI-BSEQ-MAX   PIC S9(5)   COMP-3 VALUE ZERO.          
023400                                                                          
023500     03  W-IDLEVANM-X.                                                    
023600         05  W-IDDISTR-ANM       PIC S9(5)    VALUE ZERO  COMP-3.         
023700         05  W-IDKUNDNR-ANM      PIC S9(7)    VALUE ZERO  COMP-3.         
023800         05  W-IDRAPPNR-ANM      PIC  9(7)    VALUE ZERO.                 
023900                                                                          
024000     03  W-WDGXKEY-X.                                                     
024100         05  W-IDHTYP            PIC  X(4)   VALUE '4111'.                
024200         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
024300         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
024400                                                                          
024500     SKIP2                                                                
024600*    --- STATUS-KOD FRÅN IMS                                              
024700 01  STATUS-WS                   PIC XX.                                  
024800     88  SEGMENT-FINNS                       VALUE '  '.                  
024900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
025200     SKIP2                                                                
025300 01  GODK-STATUSKODER.                                                    
025400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025500     SKIP3                                                                
025600 01  SSA1                        PIC X(128).                              
025700 01  SSA2                        PIC X(192).                              
025800     EJECT                                                                
025900*    --- IMS FUNKTIONSKODER                                               
026000*01  -COPY W0003                                                          
026100     EJECT                                                                
026200*    ---  DLI INPUT-OUTPUT AREA                                           
026300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026400     SKIP3                                                                
026500 01  DLI-IO-AREA.                                                         
026600     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
026700     SKIP3                                                                
026800     03  WLRETA01 REDEFINES IO-AREA.                                      
026900*        05  -COPY WDA301                                                 
027000     EJECT                                                                
027100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
027200     SKIP3                                                                
027300 01  DLI-IO-AREA2.                                                        
027400     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
027500     SKIP3                                                                
027600     03  WLRETG01 REDEFINES IO-AREA2.                                     
027700*        05  -COPY WDA3F1                                                 
027800     EJECT                                                                
027900     03  WLKREE01 REDEFINES IO-AREA2.                                     
028000*        05  -COPY WDA201                                                 
028100     EJECT                                                                
028200     03  WLKREE11 REDEFINES IO-AREA2.                                     
028300*        05  -COPY WDA211                                                 
028400     EJECT                                                                
028500     SKIP3                                                                
028600 01    FILLER                    PIC X(16) VALUE 'WDGX4111-AREA'.         
028700 01    WL411101  -COPY WDGX4111                                           
028800     EJECT                                                                
028900 01    FILLER                    PIC X(16) VALUE 'WDGX4112-AREA'.         
029000 01    WL411111  -COPY WDGX4112                                           
029100                                                                          
029200     EJECT                                                                
029300 LINKAGE SECTION.                                                         
029400                                                                          
029500*01  -COPY W0009   -PRE MSG-                                              
029600*01  -COPY W0008   -PRE USEA-                                             
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE RETA1-                                             
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE RETA2-                                             
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE RETG-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008  -PRE KREE-                                              
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE 4111-                                              
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008  -PRE 4113-                                              
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700*01  -COPY W0008  -PRE 4115-                                              
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008  -PRE 4117-                                              
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB RETA1-PCB RETA2-PCB          
032400                           RETG-PCB KREE-PCB 4111-PCB                     
032500                           4113-PCB 4115-PCB 4117-PCB.                    
032600     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB RETA1-PCB RETA2-PCB          
032700                           RETG-PCB KREE-PCB 4111-PCB                     
032800                           4113-PCB 4115-PCB 4117-PCB.                    
032900                                                                          
033000     PERFORM IMS-GET-MSG                                                  
033100     IF SEGMENT-FINNS                                                     
033200       PERFORM A-INIT                                                     
033300       PERFORM B-KOLLA-NYCKLAR                                            
033400       IF NYCKLAR-OK                                                      
033500         IF (MFS-ENTER AND EGEN-MID) OR HELP-MID OR MFS-UPDATE            
033600           IF MFS-UPDATE                                                  
033700             PERFORM G-KOLLA-INPUT                                        
033800             IF INDATA-OK                                                 
033900                PERFORM H-UPPDATERA                                       
034000             END-IF                                                       
034100           ELSE                                                           
034200              PERFORM E-SAMMA-SIDA                                        
034300           END-IF                                                         
034400         END-IF                                                           
034500         IF INDATA-OK                                                     
034600           IF VISA-EJ                                                     
034700             CONTINUE                                                     
034800           ELSE                                                           
034900             PERFORM F-LAES-VISA-INFO                                     
035000           END-IF                                                         
035100         END-IF                                                           
035200       END-IF                                                             
035300       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73301 + 4                      
035400       PERFORM IMS-INSERT-MSG                                             
035500     END-IF                                                               
035600                                                                          
035700     MOVE ZERO TO RETURN-CODE                                             
035800     GOBACK                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 A-INIT SECTION.                                                          
036200                                                                          
036300     IF MSG-DUBBLA-TRANSKODER                                             
036400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73301                 
036500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036700     ELSE                                                                 
036800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I73301                  
036900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
037000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037100     END-IF                                                               
037200                                                                          
037300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
037500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037600                                                                          
037700     MOVE LOW-VALUE   TO MSG-AREA                                         
037800     MOVE 'W4O73301'  TO MFS-IDMOD                                        
037900     MOVE '4733'      TO MOD-IDTRANS                                      
038000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
038100                                                                          
038200     IF EGEN-MID OR HELP-MID                                              
038300       CONTINUE                                                           
038400     ELSE                                                                 
038500       MOVE SPACE TO MFS-KDTRTYP                                          
038600       MOVE '7' TO MFS-IDPFK                                              
038700     END-IF                                                               
038800                                                                          
038900     MOVE LOW-VALUE     TO W-WDA3F1KY-MIN-X                               
039000                           W-WDA3BSEQ-MIN-X                               
039100     MOVE HIGH-VALUE    TO W-WDA3F1KY-MAX-X                               
039200                           W-WDA3BSEQ-MAX-X                               
039300     .                                                                    
039400     EJECT                                                                
039500 B-KOLLA-NYCKLAR SECTION.                                                 
039600                                                                          
039700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039800     MOVE '001'             TO MSGI-KDCALL                                
039900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040000     MOVE '4733'            TO MSGI-IDTRANS                               
040100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040200     IF EGEN-MID                                                          
040300        MOVE MID-IDRT-IN    TO MSGI-IDRT                                  
040400        MOVE MID-IDRTLOP-IN TO MSGI-IDRTLOP                               
040500     END-IF                                                               
040600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040700                                                                          
040800     IF MSGI-IDLAND-SPR = 'GB'                                            
040900       MOVE 'GB'                  TO MED-IDSKYLT                          
041000     ELSE                                                                 
041100       MOVE 'S '                  TO MED-IDSKYLT                          
041200     END-IF                                                               
041300                                                                          
041400     MOVE JA TO NYCKLAR-SW                                                
041500                                                                          
041600     PERFORM BA-KOLLA-IDRETSND                                            
041700     MOVE MSGI-IDDC          TO W-IDDC                                    
041800                                WS-IDDC                                   
041900                                W-IDDC-MIN                                
042000                                W-IDDC-MAX                                
042100                                W-IDDC-BSEQ-MIN                           
042200                                W-IDDC-BSEQ-MAX                           
042300                                                                          
042400     IF MID-FLNYSND = JA OR YES                                           
042500       MOVE JA TO NYCKLAR-SW                                              
042600     ELSE                                                                 
042700       IF NYCKLAR-FEL                                                     
042800         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
042900         CALL WMEDKONV USING MED-WMEDAREA                                 
043000         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
043100         PERFORM MFS-RENSA-FAELT-IN                                       
043200       END-IF                                                             
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600 BA-KOLLA-IDRETSND   SECTION.                                             
043700                                                                          
043800     MOVE MFS-RENSA-FAELT      TO MOD-IDRT-IN                             
043900                                  MOD-IDRTLOP-IN                          
044000                                                                          
044100     IF MID-IDRT-IN            NOT = ALL '+'                              
044200       MOVE '7'                TO MFS-IDPFK                               
044300       MOVE SPACE              TO MFS-KDTRTYP                             
044400     END-IF                                                               
044500                                                                          
044600     IF MSGI-IDRTLOP      NUMERIC AND                                     
044700        MSGI-IDRTLOP      > ZERO                                          
044800        MOVE JA           TO SW-IDRETSND                                  
044900        MOVE MSGI-IDRTLOP TO W-IDRTLOP-BSEQ-MIN                           
045000                             W-IDRTLOP-BSEQ-MAX                           
045100                             MOD-IDRTLOP-UT                               
045200     ELSE                                                                 
045300        MOVE NEJ          TO NYCKLAR-SW                                   
045400     END-IF                                                               
045500                                                                          
045600     IF MSGI-IDRT         NOT = SPACE                                     
045700        MOVE MSGI-IDRT    TO W-IDRT-BSEQ-MIN                              
045800                             W-IDRT-BSEQ-MAX                              
045900                             MOD-IDRT-UT                                  
046000     END-IF                                                               
046100                                                                          
046200     MOVE MSGI-IDRT-KEY    TO W-IDRT-4111                                 
046300     PERFORM IMS-GU-WL411101                                              
046400     .                                                                    
046500     EJECT                                                                
046600 E-SAMMA-SIDA SECTION.                                                    
046700                                                                          
046800     MOVE JA             TO VISA-SW                                       
046900                                                                          
047000     IF EGEN-MID OR HELP-MID                                              
047100        IF MID-INPUT            NOT = ALL '+'                             
047200          PERFORM MFS-LAES-IN-IGEN                                        
047300          PERFORM MFS-ROER-EJ-FAELT-IN                                    
047400          MOVE NEJ            TO VISA-SW                                  
047500          MOVE INF-PRESS-PF11 TO MED-IDMFSINF                             
047600          CALL WMEDKONV USING MED-WMEDAREA                                
047700          MOVE MED-MFSINF     TO MOD-TEMFSFEL                             
047800        ELSE                                                              
047900          PERFORM MFS-RENSA-FAELT-IN                                      
048000        END-IF                                                            
048100     ELSE                                                                 
048200        PERFORM MFS-RENSA-FAELT-IN                                        
048300     END-IF                                                               
048400     .                                                                    
048500     EJECT                                                                
048600 F-LAES-VISA-INFO SECTION.                                                
048700                                                                          
048800     PERFORM IMS-GHU-SEQB-WLRETA01                                        
048900                                                                          
049000     IF SEGMENT-SAKNAS                                                    
049100        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
049200        CALL WMEDKONV USING MED-WMEDAREA                                  
049300        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
049400        PERFORM MFS-RENSA-FAELT-UT                                        
049500        PERFORM MFS-RENSA-FAELT-IN                                        
049600        IF SKAPA-NY-SND                                                   
049700          CONTINUE                                                        
049800        ELSE                                                              
049900          PERFORM MFS-STAENG-FAELT-IN                                     
050000        END-IF                                                            
050100     ELSE                                                                 
050200       IF RET-IDRT = 'CDC' OR 'US1' OR 'US2' OR 'US3' OR 'CA1' OR         
050300                     'JP1' OR 'AU1' OR 'SE1' OR 'GB1' OR                  
050400                     'SE2' OR 'GB2' OR 'NL1' OR 'IT1' OR 'GB3'            
050500         MOVE RET-KVKOLLI-LOSS  TO MOD-KVKOLLI-LOSS                       
050600         MOVE RET-KVKOLLI-MOT   TO MOD-KVKOLLI-MOT                        
050700       ELSE                                                               
050800         PERFORM S03-RAEKNA-KOLLI                                         
050900         IF W-KVKOLLI-LOSS > ZERO                                         
051000           MOVE W-KVKOLLI-LOSS  TO MOD-KVKOLLI-LOSS                       
051100         ELSE                                                             
051200           MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-LOSS                       
051300         END-IF                                                           
051400         IF W-KVKOLLI-MOT  > ZERO                                         
051500           MOVE W-KVKOLLI-MOT   TO MOD-KVKOLLI-MOT                        
051600         ELSE                                                             
051700           MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-MOT                        
051800         END-IF                                                           
051900       END-IF                                                             
052000       MOVE RET-IDANSTNR-LOSS   TO MOD-IDANSTNR-LOSS                      
052100       MOVE RET-ADINLOMR-LOSS   TO MOD-ADINLOMR-LOSS                      
052200       MOVE RET-IDFRASED-CDC    TO MOD-IDFRASED-LOSS                      
052300                                                                          
052400       MOVE RET-IDANSTNR-MOT    TO MOD-IDANSTNR-MOT                       
052500       MOVE RET-ADINLOMR-MOT    TO MOD-ADINLOMR-MOT                       
052600       MOVE MFS-RENSA-FAELT     TO MOD-IDFRASED-MOT                       
052700                                                                          
052800       IF MFS-QUERY AND (MID-INPUT NOT = ALL '+')                         
052900         PERFORM  MFS-ROER-EJ-FAELT-IN                                    
053000       ELSE                                                               
053100         PERFORM  MFS-RENSA-FAELT-IN                                      
053200         IF MFS-UPDATE AND RAD-INFO                                       
053300           MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKOLLI                        
053400                                       MOD-IDDISTR                        
053500                                       MOD-IDKOLLI-FOM                    
053600                                       MOD-IDKOLLI-TOM                    
053700         END-IF                                                           
053800       END-IF                                                             
053900                                                                          
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300                                                                          
054400 G-KOLLA-INPUT SECTION.                                                   
054500                                                                          
054600     MOVE ZERO                   TO MED-IDMFSFEL                          
054700     MOVE JA                     TO INDATA-SW                             
054800     IF MID-INPUT = ALL '+'                                               
054900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
055000       CALL WMEDKONV USING MED-WMEDAREA                                   
055100       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
055200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
055300       MOVE NEJ                  TO INDATA-SW                             
055400     ELSE                                                                 
055500                                                                          
055600       PERFORM GA-FORMELL-KONTROLL                                        
055700       IF INDATA-OK                                                       
055800          PERFORM GB-LOGISK-KONTROLL                                      
055900       END-IF                                                             
056000                                                                          
056100       IF INDATA-FEL                                                      
056200         IF MED-IDMFSFEL = ZERO                                           
056300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
056400         END-IF                                                           
056500         CALL WMEDKONV USING MED-WMEDAREA                                 
056600         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
056700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
056800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
056900       END-IF                                                             
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300                                                                          
057400 GA-FORMELL-KONTROLL   SECTION.                                           
057500                                                                          
057600     PERFORM GAA-KOLLA-NY-SND                                             
057700     PERFORM GAB-KOLLA-LOSS-INFO                                          
057800     PERFORM GAC-KOLLA-MOT-INFO                                           
057900     PERFORM GAD-KOLLA-KOLLI-INFO                                         
058000     PERFORM GAE-KOLLA-RAD-INFO                                           
058100     PERFORM GAF-RELATIONS-KONTROLL                                       
058200                                                                          
058300     .                                                                    
058400     EJECT                                                                
058500                                                                          
058600 GAA-KOLLA-NY-SND      SECTION.                                           
058700                                                                          
058800     MOVE NEJ                     TO SW-SKAPA-NY-SND                      
058900                                                                          
059000     IF MID-FLNYSND                NOT = ALL '+'                          
059100       IF MID-FLNYSND              =  JA OR YES                           
059200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNYSND-ATTR                    
059300         MOVE JA                   TO SW-SKAPA-NY-SND                     
059400       ELSE                                                               
059500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNYSND-ATTR                    
059600         MOVE NEJ                  TO INDATA-SW                           
059700       END-IF                                                             
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100                                                                          
060200 GAB-KOLLA-LOSS-INFO   SECTION.                                           
060300                                                                          
060400     MOVE NEJ                          TO SW-LOSS-INFO                    
060500                                                                          
060600     IF MID-LOSS                       NOT = ALL '+'                      
060700        MOVE JA                        TO SW-LOSS-INFO                    
060800                                                                          
060900        PERFORM GABA-KOLLA-KVKOLLI-LOSS                                   
061000        PERFORM GABB-KOLLA-IDANSTNR-LOSS                                  
061100        PERFORM GABC-KOLLA-ADINLOMR-LOSS                                  
061200        PERFORM GABD-KOLLA-IDFRASED-LOSS                                  
061300                                                                          
061400     END-IF                                                               
061500                                                                          
061600     .                                                                    
061700     EJECT                                                                
061800                                                                          
061900 GABA-KOLLA-KVKOLLI-LOSS SECTION.                                         
062000                                                                          
062100     IF MID-KVKOLLI-LOSS               NOT = ALL '+'                      
062200        IF MID-KVKOLLI-LOSS            NUMERIC                            
062300           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVKOLLI-LOSS-ATTR           
062400        ELSE                                                              
062500           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVKOLLI-LOSS-ATTR           
062600           MOVE NEJ                    TO INDATA-SW                       
062700        END-IF                                                            
062800     END-IF                                                               
062900                                                                          
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300 GABB-KOLLA-IDANSTNR-LOSS SECTION.                                        
063400                                                                          
063500     IF MID-IDANSTNR-LOSS              NOT = ALL '+'                      
063600        IF MID-IDANSTNR-LOSS           NUMERIC                            
063700           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDANSTNR-LOSS-ATTR          
063800        ELSE                                                              
063900           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSTNR-LOSS-ATTR          
064000           MOVE NEJ                    TO INDATA-SW                       
064100        END-IF                                                            
064200     ELSE                                                                 
064300        MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDANSTNR-LOSS-ATTR          
064400     END-IF                                                               
064500                                                                          
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 GABC-KOLLA-ADINLOMR-LOSS SECTION.                                        
065000                                                                          
065100     IF MID-ADINLOMR-LOSS              NOT = ALL '+'                      
065200        MOVE MFS-ALFA-FAELT-RAETT      TO MOD-ADINLOMR-LOSS-ATTR          
065300     END-IF                                                               
065400                                                                          
065500     .                                                                    
065600     EJECT                                                                
065700                                                                          
065800 GABD-KOLLA-IDFRASED-LOSS SECTION.                                        
065900                                                                          
066000     IF MID-IDFRASED-LOSS              NOT = ALL '+'                      
066100        MOVE MFS-ALFA-FAELT-RAETT      TO MOD-IDFRASED-LOSS-ATTR          
066200     END-IF                                                               
066300                                                                          
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700                                                                          
066800 GAC-KOLLA-MOT-INFO   SECTION.                                            
066900                                                                          
067000     MOVE NEJ                   TO SW-MOT-INFO                            
067100     IF MID-MOT                        NOT = ALL '+'                      
067200        MOVE JA                        TO SW-MOT-INFO                     
067300                                                                          
067400        PERFORM GACA-KOLLA-KVKOLLI-MOT                                    
067500        PERFORM GACB-KOLLA-IDANSTNR-MOT                                   
067600        PERFORM GACC-KOLLA-ADINLOMR-MOT                                   
067700        PERFORM GACD-KOLLA-IDFRASED-MOT                                   
067800                                                                          
067900     END-IF                                                               
068000                                                                          
068100     .                                                                    
068200     EJECT                                                                
068300                                                                          
068400 GACA-KOLLA-KVKOLLI-MOT SECTION.                                          
068500                                                                          
068600     IF MID-KVKOLLI-MOT                NOT = ALL '+'                      
068700        IF MID-KVKOLLI-MOT             NUMERIC                            
068800           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVKOLLI-MOT-ATTR            
068900        ELSE                                                              
069000           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVKOLLI-MOT-ATTR            
069100           MOVE NEJ                    TO INDATA-SW                       
069200        END-IF                                                            
069300     END-IF                                                               
069400                                                                          
069500     .                                                                    
069600     EJECT                                                                
069700                                                                          
069800 GACB-KOLLA-IDANSTNR-MOT SECTION.                                         
069900                                                                          
070000     IF MID-IDANSTNR-MOT               NOT = ALL '+'                      
070100        IF MID-IDANSTNR-MOT            NUMERIC                            
070200           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDANSTNR-MOT-ATTR           
070300        ELSE                                                              
070400           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSTNR-MOT-ATTR           
070500           MOVE NEJ                    TO INDATA-SW                       
070600        END-IF                                                            
070700     END-IF                                                               
070800                                                                          
070900     .                                                                    
071000     EJECT                                                                
071100                                                                          
071200 GACC-KOLLA-ADINLOMR-MOT SECTION.                                         
071300                                                                          
071400     IF MID-ADINLOMR-MOT               NOT = ALL '+'                      
071500        MOVE MFS-ALFA-FAELT-RAETT      TO MOD-ADINLOMR-MOT-ATTR           
071600     END-IF                                                               
071700                                                                          
071800     .                                                                    
071900     EJECT                                                                
072000                                                                          
072100 GACD-KOLLA-IDFRASED-MOT  SECTION.                                        
072200                                                                          
072300     IF MID-IDFRASED-MOT               NOT = ALL '+'                      
072400        MOVE MFS-ALFA-FAELT-RAETT      TO MOD-IDFRASED-MOT-ATTR           
072500     END-IF                                                               
072600                                                                          
072700     .                                                                    
072800     EJECT                                                                
072900                                                                          
073000 GAD-KOLLA-KOLLI-INFO   SECTION.                                          
073100                                                                          
073200     MOVE NEJ                   TO SW-KOLLI-INFO                          
073300     IF MID-KOLLI           NOT =  ALL '+'                                
073400        MOVE JA                 TO SW-KOLLI-INFO                          
073500        PERFORM GADA-KOLLA-IDKOLLI                                        
073600        PERFORM GADB-KOLLA-IDDISTR                                        
073700        PERFORM GADC-KOLLA-IDKOLLI-FOM-TOM                                
073800        IF MID-IDKOLLI-FOM = ALL '+' AND                                  
073900           MID-IDKOLLI-TOM = ALL '+'                                      
074000           IF MID-IDKOLLI = ALL '+'                                       
074100              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-ATTR                
074200                                          MOD-IDKOLLI-FOM-ATTR            
074300                                          MOD-IDKOLLI-TOM-ATTR            
074400              MOVE NEJ                 TO INDATA-SW                       
074500           END-IF                                                         
074600        ELSE                                                              
074700           IF MID-IDKOLLI = ALL '+' OR                                    
074800             (MID-IDKOLLI NUMERIC   AND                                   
074900              MID-IDKOLLI = ZERO)   OR                                    
075000             (WS-IDKOLLI-FOM = ZERO AND                                   
075100              WS-IDKOLLI-TOM = ZERO)                                      
075200              CONTINUE                                                    
075300           ELSE                                                           
075400              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-ATTR                
075500                                          MOD-IDKOLLI-FOM-ATTR            
075600                                          MOD-IDKOLLI-TOM-ATTR            
075700              MOVE NEJ                 TO INDATA-SW                       
075800           END-IF                                                         
075900        END-IF                                                            
076000        IF WS-IDKOLLI-FOM = ZERO AND                                      
076100           WS-IDKOLLI-TOM = ZERO AND                                      
076200           W-IDKOLLI      = ZERO                                          
076300            MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-ATTR                  
076400                                        MOD-IDKOLLI-FOM-ATTR              
076500                                        MOD-IDKOLLI-TOM-ATTR              
076600            MOVE NEJ                 TO INDATA-SW                         
076700        END-IF                                                            
076800     END-IF                                                               
076900     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200 GADA-KOLLA-IDKOLLI      SECTION.                                         
077300                                                                          
077400     IF MID-IDKOLLI                 NOT = ALL '+'                         
077500        IF MID-IDKOLLI              NOT NUMERIC                           
077600           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-ATTR                   
077700           MOVE NEJ                 TO INDATA-SW                          
077800        ELSE                                                              
077900           IF MID-IDKOLLI > ZERO                                          
078000             MOVE MID-IDKOLLI         TO W-IDKOLLI                        
078100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-ATTR                 
078200           ELSE                                                           
078300              MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKOLLI-ATTR             
078400              MOVE NEJ                    TO INDATA-SW                    
078500           END-IF                                                         
078600        END-IF                                                            
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000                                                                          
079100 GADB-KOLLA-IDDISTR      SECTION.                                         
079200                                                                          
079300     IF MID-IDDISTR                 NOT = ALL '+'                         
079400        IF MID-IDDISTR              NOT NUMERIC                           
079500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR                   
079600           MOVE NEJ                 TO INDATA-SW                          
079700        ELSE                                                              
079800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-ATTR                   
079900        END-IF                                                            
080000     ELSE                                                                 
080100        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-ATTR                   
080200        MOVE NEJ                    TO INDATA-SW                          
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600                                                                          
080700 GADC-KOLLA-IDKOLLI-FOM-TOM SECTION.                                      
080800                                                                          
080900     IF MID-IDKOLLI-FOM             NOT = ALL '+'                         
081000        IF MID-IDKOLLI-FOM          NOT NUMERIC                           
081100           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-FOM-ATTR               
081200           MOVE NEJ                 TO INDATA-SW                          
081300        ELSE                                                              
081400           MOVE MID-IDKOLLI-FOM     TO WS-IDKOLLI-FOM                     
081500           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-FOM-ATTR               
081600        END-IF                                                            
081700     ELSE                                                                 
081800        MOVE ZERO                   TO WS-IDKOLLI-FOM                     
081900        MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDKOLLI-FOM-ATTR               
082000     END-IF                                                               
082100                                                                          
082200     IF MID-IDKOLLI-TOM             NOT = ALL '+'                         
082300        IF MID-IDKOLLI-TOM          NOT NUMERIC                           
082400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-TOM-ATTR               
082500           MOVE NEJ                 TO INDATA-SW                          
082600        ELSE                                                              
082700           MOVE MID-IDKOLLI-TOM     TO WS-IDKOLLI-TOM                     
082800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-TOM-ATTR               
082900        END-IF                                                            
083000     ELSE                                                                 
083100        MOVE ZERO                   TO WS-IDKOLLI-TOM                     
083200        MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDKOLLI-TOM-ATTR               
083300     END-IF                                                               
083400                                                                          
083500     IF WS-IDKOLLI-FOM = ZERO AND                                         
083600        WS-IDKOLLI-TOM = ZERO                                             
083700       CONTINUE                                                           
083800     ELSE                                                                 
083900        IF WS-IDKOLLI-TOM > WS-IDKOLLI-FOM    AND                         
084000           WS-IDKOLLI-FOM > ZERO              AND                         
084100           WS-IDKOLLI-FOM + 20 > WS-IDKOLLI-TOM                           
084200          CONTINUE                                                        
084300        ELSE                                                              
084400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-FOM-ATTR               
084500                                       MOD-IDKOLLI-TOM-ATTR               
084600           MOVE NEJ                 TO INDATA-SW                          
084700        END-IF                                                            
084800     END-IF                                                               
084900                                                                          
085000     .                                                                    
085100     EJECT                                                                
085200                                                                          
085300 GAE-KOLLA-RAD-INFO   SECTION.                                            
085400                                                                          
085500     MOVE NEJ                       TO SW-RAD-INFO                        
085600     MOVE +1                        TO INDX                               
085700     PERFORM UNTIL INDX             > MAX-INDX                            
085800                                                                          
085900       IF MID-INPUT-RAD(INDX)       NOT = ALL '+'                         
086000          MOVE JA                   TO SW-RAD-INFO                        
086100                                                                          
086200          PERFORM GAED-KOLLA-IDKUNDNR                                     
086300          PERFORM GAEA-KOLLA-IDRAPPNR                                     
086400          PERFORM GAEB-KOLLA-KVKOLLI                                      
086500          PERFORM GAEC-KOLLA-RT-PAA-SIDA                                  
086600                                                                          
086700          IF MID-IDFRASED(INDX)       NOT = ALL '+'                       
086800            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFRASED-ATTR(INDX)          
086900          END-IF                                                          
087000                                                                          
087100          IF MID-TERETNOT(INDX)       NOT = ALL '+'                       
087200            MOVE MFS-ALFA-FAELT-RAETT TO MOD-TERETNOT-ATTR(INDX)          
087300          END-IF                                                          
087400       END-IF                                                             
087500                                                                          
087600       ADD +1                        TO INDX                              
087700     END-PERFORM                                                          
087800     .                                                                    
087900     EJECT                                                                
088000                                                                          
088100 GAEA-KOLLA-IDRAPPNR     SECTION.                                         
088200                                                                          
088300     IF MID-IDRAPPNR(INDX)          NOT = ALL '+'                         
088400        IF MID-IDRAPPNR(INDX)       NOT NUMERIC                           
088500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDRAPPNR-ATTR(INDX)            
088600           MOVE NEJ                 TO INDATA-SW                          
088700        ELSE                                                              
088800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRAPPNR-ATTR(INDX)            
088900        END-IF                                                            
089000     ELSE                                                                 
089100        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDRAPPNR-ATTR(INDX)            
089200        MOVE NEJ                    TO INDATA-SW                          
089300     END-IF                                                               
089400                                                                          
089500     .                                                                    
089600     EJECT                                                                
089700 GAEB-KOLLA-KVKOLLI      SECTION.                                         
089800                                                                          
089900     IF MID-KVKOLLI(INDX)           NOT = ALL '+'                         
090000        IF MID-KVKOLLI(INDX)        NOT NUMERIC                           
090100           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVKOLLI-ATTR(INDX)             
090200           MOVE NEJ                 TO INDATA-SW                          
090300        ELSE                                                              
090400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVKOLLI-ATTR(INDX)             
090500        END-IF                                                            
090600     END-IF                                                               
090700                                                                          
090800     .                                                                    
090900     EJECT                                                                
091000 GAEC-KOLLA-RT-PAA-SIDA    SECTION.                                       
091100                                                                          
091200     COMPUTE INDX2                 =  INDX + 1                            
091300     PERFORM UNTIL INDX2           >  MAX-INDX OR                         
091400       (MID-IDKUNDNR(INDX)         =  MID-IDKUNDNR(INDX2)  AND            
091500        MID-IDRAPPNR(INDX)         =  MID-IDRAPPNR(INDX2))                
091600         ADD +1                    TO INDX2                               
091700     END-PERFORM                                                          
091800                                                                          
091900     IF INDX2                      >  MAX-INDX                            
092000         CONTINUE                                                         
092100     ELSE                                                                 
092200         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDRAPPNR-ATTR(INDX)             
092300                                      MOD-IDRAPPNR-ATTR(INDX2)            
092400                                      MOD-IDKUNDNR-ATTR(INDX)             
092500                                      MOD-IDKUNDNR-ATTR(INDX2)            
092600         MOVE NEJ                  TO INDATA-SW                           
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
093000                                                                          
093100 GAED-KOLLA-IDKUNDNR     SECTION.                                         
093200                                                                          
093300     IF MID-IDKUNDNR(INDX)          NOT = ALL '+'                         
093400        IF MID-IDKUNDNR(INDX)       NOT NUMERIC                           
093500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-ATTR(INDX)            
093600           MOVE NEJ                 TO INDATA-SW                          
093700        ELSE                                                              
093800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-ATTR(INDX)            
093900        END-IF                                                            
094000     ELSE                                                                 
094100        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKUNDNR-ATTR(INDX)            
094200        MOVE NEJ                    TO INDATA-SW                          
094300     END-IF                                                               
094400                                                                          
094500     .                                                                    
094600     EJECT                                                                
094700 GAF-RELATIONS-KONTROLL SECTION.                                          
094800                                                                          
094900     IF SKAPA-NY-SND                                                      
095000        IF MOT-INFO OR LOSS-INFO OR RAD-INFO                              
095100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNYSND-ATTR                  
095200           MOVE NEJ                  TO INDATA-SW                         
095300           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
095400        ELSE                                                              
095500           IF KOLLI-INFO                                                  
095600             MOVE NEJ                  TO SW-KOLLI-INFO                   
095700           END-IF                                                         
095800        END-IF                                                            
095900     END-IF                                                               
096000                                                                          
096100     IF LOSS-INFO                                                         
096200        IF MOT-INFO OR KOLLI-INFO OR RAD-INFO                             
096300           PERFORM S01A-FELMARKERA-LOSS-INFO                              
096400           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
096500        END-IF                                                            
096600     END-IF                                                               
096700                                                                          
096800     IF KOLLI-INFO                                                        
096900        IF RAD-INFO                                                       
097000           CONTINUE                                                       
097100        ELSE                                                              
097200           PERFORM S01C-FELMARKERA-KOLLI-INFO                             
097300        END-IF                                                            
097400     END-IF                                                               
097500                                                                          
097600     IF RAD-INFO                                                          
097700        IF KOLLI-INFO                                                     
097800           CONTINUE                                                       
097900        ELSE                                                              
098000           PERFORM S01C-FELMARKERA-KOLLI-INFO                             
098100        END-IF                                                            
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500                                                                          
098600 GB-LOGISK-KONTROLL   SECTION.                                            
098700                                                                          
098800     IF SKAPA-NY-SND                                                      
098900        CONTINUE                                                          
099000     ELSE                                                                 
099100        PERFORM GBA-KOLLA-SNDSTATUS                                       
099200        PERFORM GBB-KOLLA-RELATIONER                                      
099300     END-IF                                                               
099400                                                                          
099500     IF RAD-INFO                                                          
099600        PERFORM GBC-KOLLA-REG-RAPPORTER                                   
099700     END-IF                                                               
099800                                                                          
099900     .                                                                    
100000     EJECT                                                                
100100                                                                          
100200 GBA-KOLLA-SNDSTATUS           SECTION.                                   
100300                                                                          
100400     MOVE MSGI-IDRTLOP              TO W-IDRTLOP-BSEQ-MIN                 
100500                                       W-IDRTLOP-BSEQ-MAX                 
100600                                       W-IDRTLOP-NUM                      
100700     PERFORM GBAA-KOLLA-IDRTLOP                                           
100800                                                                          
100900     IF INDATA-OK                                                         
101000       PERFORM IMS-GHU-SEQB-WLRETA01                                      
101100                                                                          
101200       IF SEGMENT-FINNS                                                   
101300          MOVE JA                     TO SW-GAMMAL-SND                    
101400          IF RET-KDRETSTA             = W-SND-SAENT OR                    
101500                                        W-SND-LOSS                        
101600              CONTINUE                                                    
101700          ELSE                                                            
101800              PERFORM S01-FELMARKERA                                      
101900              MOVE ERR-OTILL-STATUS TO MED-IDMFSFEL                       
102000          END-IF                                                          
102100       END-IF                                                             
102200     END-IF                                                               
102300                                                                          
102400     .                                                                    
102500     EJECT                                                                
102600                                                                          
102700 GBAA-KOLLA-IDRTLOP               SECTION.                                
102800                                                                          
102900     PERFORM IMS-GHNP-WL411111                                            
103000     IF W-IDRTLOP-NUM                  >  4112-IDRTLOP                    
103100         PERFORM S01-FELMARKERA                                           
103200     END-IF                                                               
103300                                                                          
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700 GBB-KOLLA-RELATIONER             SECTION.                                
103800                                                                          
103900     IF LOSS-INFO                                                         
104000        PERFORM GBBA-KOLLA-LOSS-INFO                                      
104100     END-IF                                                               
104200                                                                          
104300     IF MOT-INFO                                                          
104400        PERFORM GBBB-KOLLA-MOT-INFO                                       
104500     END-IF                                                               
104600                                                                          
104700     IF RAD-INFO                                                          
104800        IF MOT-INFO OR GAMMAL-SND                                         
104900           CONTINUE                                                       
105000        ELSE                                                              
105100           PERFORM S01C-FELMARKERA-KOLLI-INFO                             
105200        END-IF                                                            
105300     END-IF                                                               
105400                                                                          
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800 GBBA-KOLLA-LOSS-INFO               SECTION.                              
105900                                                                          
106000     IF GAMMAL-SND                                                        
106100        IF RET-IDRT = W-CDC OR W-US1 OR W-US2 OR W-US3 OR W-CA1 OR        
106200                      W-JP1 OR W-AU1 OR W-SE1 OR W-GB1 OR W-GB3 OR        
106300                      W-SE2 OR W-GB2 OR W-NL1 OR W-IT1                    
106400          CONTINUE                                                        
106500        ELSE                                                              
106600          IF MID-KVKOLLI-LOSS     NOT  = ALL '+'                          
106700             MOVE MFS-NUM-FAELT-FEL    TO MOD-KVKOLLI-LOSS-ATTR           
106800             MOVE NEJ                  TO INDATA-SW                       
106900          END-IF                                                          
107000          IF MID-IDFRASED-LOSS    NOT  = ALL '+'                          
107100             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFRASED-LOSS-ATTR          
107200             MOVE NEJ                  TO INDATA-SW                       
107300          END-IF                                                          
107400        END-IF                                                            
107500     ELSE                                                                 
107600       IF CDC                                                             
107700         IF MID-KVKOLLI-LOSS          = ALL '+'                           
107800            MOVE MFS-NUM-FAELT-FEL    TO MOD-KVKOLLI-LOSS-ATTR            
107900            MOVE NEJ                  TO INDATA-SW                        
108000         END-IF                                                           
108100                                                                          
108200         IF MID-IDANSTNR-LOSS         = ALL '+'                           
108300            MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-LOSS-ATTR           
108400            MOVE NEJ                  TO INDATA-SW                        
108500         END-IF                                                           
108600                                                                          
108700         IF MID-ADINLOMR-LOSS         = ALL '+'                           
108800            MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-LOSS-ATTR           
108900            MOVE NEJ                  TO INDATA-SW                        
109000         END-IF                                                           
109100       END-IF                                                             
109200     END-IF                                                               
109300     .                                                                    
109400     EJECT                                                                
109500                                                                          
109600 GBBB-KOLLA-MOT-INFO               SECTION.                               
109700                                                                          
109800     IF GAMMAL-SND                                                        
109900        IF RET-IDRT = W-CDC OR W-US1 OR W-US2 OR W-US3 OR W-CA1 OR        
110000                      W-JP1 OR W-AU1 OR W-SE1 OR W-GB1 OR                 
110100                      W-SE2 OR W-GB2 OR W-NL1 OR W-IT1 OR W-GB3           
110200          CONTINUE                                                        
110300        ELSE                                                              
110400          IF MID-KVKOLLI-MOT      NOT  = ALL '+'                          
110500             MOVE MFS-NUM-FAELT-FEL    TO MOD-KVKOLLI-MOT-ATTR            
110600             MOVE NEJ                  TO INDATA-SW                       
110700          END-IF                                                          
110800          IF MID-IDFRASED-MOT     NOT  = ALL '+'                          
110900             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFRASED-MOT-ATTR           
111000             MOVE NEJ                  TO INDATA-SW                       
111100          END-IF                                                          
111200        END-IF                                                            
111300     ELSE                                                                 
111400        IF MID-KVKOLLI-MOT           = ALL '+'                            
111500           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVKOLLI-MOT-ATTR              
111600           MOVE NEJ                  TO INDATA-SW                         
111700        END-IF                                                            
111800                                                                          
111900        IF MID-IDANSTNR-MOT          = ALL '+'                            
112000           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-MOT-ATTR             
112100           MOVE NEJ                  TO INDATA-SW                         
112200        END-IF                                                            
112300                                                                          
112400     END-IF                                                               
112500     .                                                                    
112600     EJECT                                                                
112700                                                                          
112800 GBC-KOLLA-REG-RAPPORTER   SECTION.                                       
112900                                                                          
113000     MOVE +1                         TO INDX                              
113100     PERFORM UNTIL INDX              > MAX-INDX                           
113200                                                                          
113300        IF MID-IDRAPPNR(INDX)        NOT = ALL '+'                        
113400          MOVE MID-IDDISTR         TO W-IDDISTR-ANM                       
113500                                      W-IDDISTR-MIN                       
113600                                      W-IDDISTR-MAX                       
113700          MOVE MID-IDKUNDNR(INDX)  TO W-IDKUNDNR-ANM                      
113800                                      W-IDKUNDNR-MIN                      
113900                                      W-IDKUNDNR-MAX                      
114000          MOVE MID-IDRAPPNR(INDX)  TO W-IDRAPPNR-ANM                      
114100                                      W-IDRAPPNR-MIN                      
114200                                      W-IDRAPPNR-MAX                      
114300                                                                          
114400          PERFORM IMS-GU-WLKREE01                                         
114500          IF SEGMENT-SAKNAS OR                                            
114600             (SEGMENT-FINNS AND ANM-KDLEVANM NOT = '4')                   
114700              MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKUNDNR-ATTR(INDX)          
114800                                         MOD-IDRAPPNR-ATTR(INDX)          
114900              MOVE NEJ                TO INDATA-SW                        
115000              IF SEGMENT-SAKNAS                                           
115100                 MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                    
115200              ELSE                                                        
115300                 MOVE ERR-OTILL-STATUS TO MED-IDMFSFEL                    
115400              END-IF                                                      
115500          ELSE                                                            
115600            PERFORM GBCA-LAES-WLKREE11                                    
115700            IF SEGMENT-FINNS AND LEV-IDDC-RET = MSGI-IDDC                 
115800              PERFORM IMS-GU-WLRETG01                                     
115900              PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                
116000                                           OR INDATA-FEL                  
116100                 IF SEQF-IDRT    = W-IDRT-BSEQ-MIN    AND                 
116200                    SEQF-IDRTLOP = W-IDRTLOP-BSEQ-MIN                     
116300                    IF SEQF-IDKOLLI = W-IDKOLLI           OR              
116400                     ((SEQF-IDKOLLI > WS-IDKOLLI-FOM - 1) AND             
116500                      (SEQF-IDKOLLI < WS-IDKOLLI-TOM + 1))                
116600                      MOVE MFS-NUM-FAELT-FEL                              
116700                                       TO MOD-IDKUNDNR-ATTR(INDX)         
116800                                          MOD-IDRAPPNR-ATTR(INDX)         
116900                      MOVE NEJ         TO INDATA-SW                       
117000                      MOVE ERR-ANNAN-SHIPPING TO MED-IDMFSFEL             
117100                    END-IF                                                
117200                 ELSE                                                     
117300                    MOVE MFS-NUM-FAELT-FEL                                
117400                                     TO MOD-IDKUNDNR-ATTR(INDX)           
117500                                        MOD-IDRAPPNR-ATTR(INDX)           
117600                    MOVE NEJ         TO INDATA-SW                         
117700                    MOVE ERR-ANNAN-SHIPPING TO MED-IDMFSFEL               
117800                 END-IF                                                   
117900                 PERFORM IMS-GN-WLRETG01                                  
118000              END-PERFORM                                                 
118100            ELSE                                                          
118200              MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKUNDNR-ATTR(INDX)          
118300                                         MOD-IDRAPPNR-ATTR(INDX)          
118400              MOVE NEJ                TO INDATA-SW                        
118500              IF SEGMENT-SAKNAS                                           
118600                 MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                    
118700              ELSE                                                        
118800                 MOVE ERR-OTILL-STATUS TO MED-IDMFSFEL                    
118900              END-IF                                                      
119000            END-IF                                                        
119100          END-IF                                                          
119200        END-IF                                                            
119300        ADD +1             TO INDX                                        
119400     END-PERFORM                                                          
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119800 GBCA-LAES-WLKREE11          SECTION.                                     
119900                                                                          
120000     MOVE NEJ                    TO OKOD-FL-RETILL                        
120100                                    OKOD-FL-INTERNUPPACKNING              
120200     PERFORM IMS-GNP-WLKREE11                                             
120250                                                                          
120300     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
120400                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
120500        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
120600           LEV-KDKREBEH(1:1) = 'J'   OR                                   
120700           LEV-KDKREBEH(1:1) = 'C'   OR                                   
120800           LEV-KDKREBEH = 'D01' OR                                        
120900           LEV-KDKREBEH = 'D02' OR                                        
121000           LEV-KDKREBEH = 'D03'                                           
121100*--ANROPA KONTROLL AV ORSAKSKODER                                         
121200            MOVE LEV-KDANMORS   TO OKOD-KDANMORS                          
121300            CALL W418OKOD USING OKOD-W418OKOD                             
121400        END-IF                                                            
121500        IF OKOD-FL-RETILL = 'J' OR                                        
121600           OKOD-FL-INTERNUPPACKNING = 'J'                                 
121700           CONTINUE                                                       
121800        ELSE                                                              
121900          PERFORM IMS-GNP-WLKREE11                                        
122000        END-IF                                                            
122100     END-PERFORM                                                          
122200                                                                          
122300     .                                                                    
122400     EJECT                                                                
122500 H-UPPDATERA SECTION.                                                     
122600                                                                          
122700     IF SKAPA-NY-SND                                                      
122800        PERFORM HA-TA-UT-IDRTLOP                                          
122900     END-IF                                                               
123000                                                                          
123100     IF LOSS-INFO                                                         
123200        PERFORM HB-UPPDATERA-LOSS                                         
123300     END-IF                                                               
123400                                                                          
123500     IF MOT-INFO                                                          
123600        PERFORM HC-UPPDATERA-MOT                                          
123700     END-IF                                                               
123800                                                                          
123900     IF RAD-INFO                                                          
124000        PERFORM HD-UPPDATERA-RT                                           
124100     END-IF                                                               
124200                                                                          
124300     MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                            
124400     CALL WMEDKONV USING MED-WMEDAREA                                     
124500     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
124600     PERFORM MFS-FORM-ATTR                                                
124700     IF SKAPA-NY-SND                                                      
124800       IF CDC-SE                                                          
124900         MOVE MFS-ADD-SAETT-CURSOR TO MOD-KVKOLLI-LOSS-ATTR               
125000       ELSE                                                               
125100         MOVE MFS-ADD-SAETT-CURSOR TO MOD-KVKOLLI-MOT-ATTR                
125200       END-IF                                                             
125300     END-IF                                                               
125400     IF RAD-INFO                                                          
125500        MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDKOLLI-ATTR                     
125600     END-IF                                                               
125700     .                                                                    
125800     EJECT                                                                
125900                                                                          
126000 HA-TA-UT-IDRTLOP SECTION.                                                
126100                                                                          
126200     PERFORM IMS-GHNP-WL411111                                            
126300     COMPUTE 4112-IDRTLOP = 4112-IDRTLOP + 001                            
126400     IF 4112-IDRTLOP = ZERO                                               
126500       ADD +001 TO 4112-IDRTLOP                                           
126600     END-IF                                                               
126700                                                                          
126800     PERFORM IMS-REPL-WL411111                                            
126900                                                                          
127000     MOVE 4112-IDRTLOP    TO W-IDRTLOP-NUM                                
127100     MOVE W-IDRTLOP-NUM   TO MOD-IDRTLOP-UT                               
127200                             MSGI-IDRTLOP                                 
127300                             W-IDRTLOP-BSEQ-MIN                           
127400                             W-IDRTLOP-BSEQ-MAX                           
127500                                                                          
127600     MOVE MSGI-IDRT-KEY   TO MOD-IDRT-UT                                  
127700                             MSGI-IDRT                                    
127800                             W-IDRT-BSEQ-MIN                              
127900                             W-IDRT-BSEQ-MAX                              
128000                                                                          
128100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
128200     .                                                                    
128300     EJECT                                                                
128400                                                                          
128500 HB-UPPDATERA-LOSS  SECTION.                                              
128600                                                                          
128700     IF GAMMAL-SND                                                        
128800        PERFORM HBA-UPPDATERA-RT-LOSS                                     
128900     ELSE                                                                 
129000        PERFORM HBB-SKAPA-RT-LOSS                                         
129100     END-IF                                                               
129200     .                                                                    
129300     EJECT                                                                
129400                                                                          
129500 HBA-UPPDATERA-RT-LOSS  SECTION.                                          
129600                                                                          
129700     PERFORM IMS-GHU-SEQB-WLRETA01                                        
129800                                                                          
129900     IF SEGMENT-FINNS                                                     
130000        PERFORM UNTIL SEGMENT-SAKNAS                                      
130100           IF MID-KVKOLLI-LOSS         NOT = ALL '+'                      
130200              MOVE MID-KVKOLLI-LOSS    TO RET-KVKOLLI-LOSS                
130300           END-IF                                                         
130400           IF MID-ADINLOMR-LOSS        NOT = ALL '+'                      
130500              MOVE MID-ADINLOMR-LOSS   TO RET-ADINLOMR-LOSS               
130600           END-IF                                                         
130700                                                                          
130800           IF MID-IDANSTNR-LOSS        NOT = ALL '+'                      
130900              MOVE MID-IDANSTNR-LOSS   TO RET-IDANSTNR-LOSS               
131000           END-IF                                                         
131100                                                                          
131200           IF MID-IDFRASED-LOSS        NOT = ALL '+'                      
131300              MOVE MID-IDFRASED-LOSS   TO RET-IDFRASED-CDC                
131400           END-IF                                                         
131500                                                                          
131600           PERFORM IMS-REPL-SEQB-WLRETA01                                 
131700           PERFORM IMS-GHN-SEQB-WLRETA01                                  
131800        END-PERFORM                                                       
131900     ELSE                                                                 
132000        CALL FELLOG                                                       
132100     END-IF                                                               
132200     .                                                                    
132300     EJECT                                                                
132400                                                                          
132500 HBB-SKAPA-RT-LOSS  SECTION.                                              
132600                                                                          
132700     MOVE MSGI-IDDC          TO RET-IDDC                                  
132800     MOVE FUNCTION CURRENT-DATE (1:8) TO  RET-DAREGDAT                    
132900                                          RET-DASNDDAT                    
133000     ACCEPT RET-TIKLOCK        FROM TIME                                  
133100                                                                          
133200     PERFORM S02-INIT-WLRETA01                                            
133300                                                                          
133400     MOVE MID-ADINLOMR-LOSS    TO RET-ADINLOMR                            
133500                                  RET-ADINLOMR-LOSS                       
133600     MOVE SPACE                TO RET-ADINLOMR-MOT                        
133700     MOVE MID-IDANSTNR-LOSS    TO RET-IDANSTNR-LOSS                       
133800     MOVE ZERO                 TO RET-IDANSTNR-MOT                        
133900     MOVE MID-KVKOLLI-LOSS     TO RET-KVKOLLI-LOSS                        
134000     IF MID-IDFRASED-LOSS      NOT = ALL '+'                              
134100        MOVE MID-IDFRASED-LOSS TO RET-IDFRASED-CDC                        
134200     ELSE                                                                 
134300        MOVE SPACE             TO RET-IDFRASED-CDC                        
134400     END-IF                                                               
134500                                                                          
134600     MOVE ZERO                 TO RET-KVKOLLI-MOT                         
134700                                  RET-KVKOLLI-AAF                         
134800                                                                          
134900                                                                          
135000     PERFORM IMS-ISRT-WLRETA01                                            
135100     PERFORM UNTIL SEGMENT-FINNS                                          
135200       ADD +1  TO  RET-TIKLOCK                                            
135300       PERFORM IMS-ISRT-WLRETA01                                          
135400     END-PERFORM                                                          
135500     .                                                                    
135600     EJECT                                                                
135700                                                                          
135800 HC-UPPDATERA-MOT   SECTION.                                              
135900                                                                          
136000     IF GAMMAL-SND                                                        
136100        PERFORM HCA-UPPDATERA-RT-MOT                                      
136200     ELSE                                                                 
136300        PERFORM HCB-SKAPA-RT-MOT                                          
136400     END-IF                                                               
136500     .                                                                    
136600     EJECT                                                                
136700                                                                          
136800 HCA-UPPDATERA-RT-MOT  SECTION.                                           
136900                                                                          
137000     PERFORM IMS-GHU-SEQB-WLRETA01                                        
137100                                                                          
137200     IF SEGMENT-FINNS                                                     
137300        PERFORM UNTIL SEGMENT-SAKNAS                                      
137400           IF MID-KVKOLLI-MOT          NOT = ALL '+'                      
137500              MOVE MID-KVKOLLI-MOT     TO RET-KVKOLLI-MOT                 
137600           END-IF                                                         
137700                                                                          
137800           IF MID-ADINLOMR-MOT         NOT = ALL '+'                      
137900              MOVE MID-ADINLOMR-MOT    TO RET-ADINLOMR-MOT                
138000                                          RET-ADINLOMR                    
138100           END-IF                                                         
138200                                                                          
138300           IF MID-IDANSTNR-MOT         NOT = ALL '+'                      
138400              MOVE MID-IDANSTNR-MOT    TO RET-IDANSTNR-MOT                
138500           END-IF                                                         
138600                                                                          
138700           IF MID-IDFRASED-MOT         NOT = ALL '+'                      
138800              MOVE MID-IDFRASED-MOT    TO RET-IDFRASED-CDC                
138900           END-IF                                                         
139000                                                                          
139100           PERFORM IMS-REPL-SEQB-WLRETA01                                 
139200           PERFORM IMS-GHN-SEQB-WLRETA01                                  
139300        END-PERFORM                                                       
139400     ELSE                                                                 
139500        CALL FELLOG                                                       
139600     END-IF                                                               
139700     .                                                                    
139800     EJECT                                                                
139900                                                                          
140000 HCB-SKAPA-RT-MOT  SECTION.                                               
140100                                                                          
140200     MOVE MSGI-IDDC            TO RET-IDDC                                
140300     MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                     
140400                                         RET-DASNDDAT                     
140500     ACCEPT RET-TIKLOCK        FROM TIME                                  
140600                                                                          
140700     PERFORM S02-INIT-WLRETA01                                            
140800                                                                          
140900     IF MID-ADINLOMR-MOT NOT = ALL '+'                                    
141000       MOVE MID-ADINLOMR-MOT     TO RET-ADINLOMR                          
141100                                    RET-ADINLOMR-MOT                      
141200     END-IF                                                               
141300     MOVE SPACE                TO RET-ADINLOMR-LOSS                       
141400     MOVE MID-IDANSTNR-MOT     TO RET-IDANSTNR-MOT                        
141500     MOVE ZERO                 TO RET-IDANSTNR-LOSS                       
141600     IF MID-IDFRASED-MOT       NOT = ALL '+'                              
141700        MOVE MID-IDFRASED-MOT  TO RET-IDFRASED-CDC                        
141800     ELSE                                                                 
141900        MOVE SPACE             TO RET-IDFRASED-CDC                        
142000     END-IF                                                               
142100     MOVE MID-KVKOLLI-MOT      TO RET-KVKOLLI-MOT                         
142200     MOVE ZERO                 TO RET-KVKOLLI-LOSS                        
142300                                  RET-KVKOLLI-AAF                         
142400                                                                          
142500     PERFORM IMS-ISRT-WLRETA01                                            
142600     PERFORM UNTIL SEGMENT-FINNS                                          
142700       ADD +1 TO RET-TIKLOCK                                              
142800       PERFORM IMS-ISRT-WLRETA01                                          
142900     END-PERFORM                                                          
143000     .                                                                    
143100     EJECT                                                                
143200                                                                          
143300 HD-UPPDATERA-RT SECTION.                                                 
143400                                                                          
143500     MOVE +1                   TO INDX                                    
143600     MOVE JA                   TO SW-FOERSTA-RT                           
143700     PERFORM UNTIL INDX        > MAX-INDX                                 
143800                                                                          
143900       IF MID-IDKUNDNR(INDX)   NOT = ALL '+'                              
144000          PERFORM HDA-BEHANDLA-RADER                                      
144100       END-IF                                                             
144200       ADD +1                  TO INDX                                    
144300                                                                          
144400     END-PERFORM                                                          
144500     .                                                                    
144600     EJECT                                                                
144700                                                                          
144800 HDA-BEHANDLA-RADER     SECTION.                                          
144900                                                                          
145000     IF FOERSTA-RT                                                        
145100        MOVE NEJ               TO SW-FOERSTA-RT                           
145200        PERFORM IMS-GHU-SEQB-WLRETA01                                     
145300        IF SEGMENT-FINNS AND RET-IDDISTR  = ZERO                          
145400           MOVE RET-WDA301        TO SPAR-RET-WDA301                      
145500           PERFORM HDAA-REDIGERA-WLRETA01                                 
145600           PERFORM IMS-REPL-SEQB-WLRETA01                                 
145700           MOVE RET-KDRETSTA      TO SPAR-KDRETSTA                        
145800           MOVE RET-KDKOLSTA      TO SPAR-KDKOLSTA                        
145900           IF WS-IDKOLLI-FOM > ZERO                                       
146000             ADD +1        TO RET-IDKOLLI                                 
146100             PERFORM UNTIL RET-IDKOLLI > WS-IDKOLLI-TOM                   
146200               MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT           
146300               ACCEPT RET-TIKLOCK  FROM TIME                              
146400               PERFORM IMS-ISRT-WLRETA01                                  
146500               IF SEGMENT-FINNS                                           
146600                 ADD +1        TO RET-IDKOLLI                             
146700               END-IF                                                     
146800             END-PERFORM                                                  
146900           END-IF                                                         
147000        ELSE                                                              
147100           IF SEGMENT-FINNS                                               
147200             MOVE RET-KDRETSTA      TO SPAR-KDRETSTA                      
147300             MOVE RET-KDKOLSTA      TO SPAR-KDKOLSTA                      
147400           END-IF                                                         
147500           PERFORM HDAA-REDIGERA-WLRETA01                                 
147600           IF SPAR-KDRETSTA NOT = SPACE                                   
147700             MOVE SPAR-KDRETSTA        TO RET-KDRETSTA                    
147800             MOVE SPAR-KDKOLSTA        TO RET-KDKOLSTA                    
147900           ELSE                                                           
148000             MOVE W-SND-SAENT          TO RET-KDRETSTA                    
148100             MOVE W-KLI-SAENT          TO RET-KDKOLSTA                    
148200           END-IF                                                         
148300           MOVE MSGI-IDDC      TO RET-IDDC                                
148400           MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT               
148500           ACCEPT RET-TIKLOCK  FROM TIME                                  
148600           MOVE RET-WDA301     TO SPAR-RET-WDA301                         
148700           PERFORM IMS-ISRT-WLRETA01                                      
148800           PERFORM UNTIL SEGMENT-FINNS                                    
148900             ADD +1    TO RET-TIKLOCK                                     
149000             PERFORM IMS-ISRT-WLRETA01                                    
149100           END-PERFORM                                                    
149200           IF WS-IDKOLLI-FOM > ZERO                                       
149300             ADD +1        TO RET-IDKOLLI                                 
149400             PERFORM UNTIL RET-IDKOLLI > WS-IDKOLLI-TOM                   
149500               MOVE MSGI-IDDC      TO RET-IDDC                            
149600               MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT           
149700               ACCEPT RET-TIKLOCK  FROM TIME                              
149800               PERFORM IMS-ISRT-WLRETA01                                  
149900               IF SEGMENT-FINNS                                           
150000                 ADD +1        TO RET-IDKOLLI                             
150100               END-IF                                                     
150200             END-PERFORM                                                  
150300           END-IF                                                         
150400        END-IF                                                            
150500     ELSE                                                                 
150600        MOVE SPAR-RET-WDA301   TO RET-WDA301                              
150700        PERFORM HDAA-REDIGERA-WLRETA01                                    
150800        MOVE MSGI-IDDC         TO RET-IDDC                                
150900        MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                  
151000        ACCEPT RET-TIKLOCK     FROM TIME                                  
151100        IF SPAR-KDRETSTA NOT = SPACE                                      
151200          MOVE SPAR-KDRETSTA        TO RET-KDRETSTA                       
151300          MOVE SPAR-KDKOLSTA        TO RET-KDKOLSTA                       
151400        ELSE                                                              
151500          MOVE W-SND-SAENT          TO RET-KDRETSTA                       
151600          MOVE W-KLI-SAENT          TO RET-KDKOLSTA                       
151700        END-IF                                                            
151800        PERFORM IMS-ISRT-WLRETA01                                         
151900        PERFORM UNTIL SEGMENT-FINNS                                       
152000          ADD +1 TO RET-TIKLOCK                                           
152100          PERFORM IMS-ISRT-WLRETA01                                       
152200        END-PERFORM                                                       
152300        IF WS-IDKOLLI-FOM > ZERO                                          
152400          ADD +1        TO RET-IDKOLLI                                    
152500          PERFORM UNTIL RET-IDKOLLI > WS-IDKOLLI-TOM                      
152600            MOVE MSGI-IDDC         TO RET-IDDC                            
152700            MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT              
152800            ACCEPT RET-TIKLOCK  FROM TIME                                 
152900            PERFORM IMS-ISRT-WLRETA01                                     
153000            IF SEGMENT-FINNS                                              
153100              ADD +1        TO RET-IDKOLLI                                
153200            END-IF                                                        
153300          END-PERFORM                                                     
153400        END-IF                                                            
153500     END-IF                                                               
153600                                                                          
153700     .                                                                    
153800     EJECT                                                                
153900 HDAA-REDIGERA-WLRETA01  SECTION.                                         
154000                                                                          
154100     PERFORM HDAAB-HAEMTA-ANSVARIG                                        
154200     PERFORM HDAAA-HAEMTA-LEV-ANM-UPPG                                    
154300     PERFORM HDAAC-INIT-RT-INFO                                           
154400                                                                          
154500     .                                                                    
154600     EJECT                                                                
154700                                                                          
154800 HDAAA-HAEMTA-LEV-ANM-UPPG SECTION.                                       
154900                                                                          
155000     MOVE MID-IDKUNDNR(INDX)   TO W-IDKUNDNR-ANM                          
155100     MOVE MID-IDRAPPNR(INDX)   TO W-IDRAPPNR-ANM                          
155200                                                                          
155300     PERFORM IMS-GU-WLKREE01                                              
155400     IF SEGMENT-FINNS                                                     
155500        MOVE ANM-FLFARLIG      TO W-FLFARLIG                              
155600        MOVE ANM-KVRADER-RT    TO W-KVRADER                               
155700     END-IF                                                               
155800                                                                          
155900     .                                                                    
156000     EJECT                                                                
156100                                                                          
156200 HDAAB-HAEMTA-ANSVARIG SECTION.                                           
156300                                                                          
156310     MOVE NEJ                  TO W-FLBUYBAC                              
156400     MOVE MID-IDKUNDNR(INDX)   TO W-IDKUNDNR-ANM                          
156500     MOVE MID-IDRAPPNR(INDX)   TO W-IDRAPPNR-ANM                          
156600     PERFORM IMS-GU-WLKREE01                                              
156700     IF SEGMENT-FINNS                                                     
156800       MOVE ANM-IDFTG            TO ANSV-IDFTG                            
156900                                                                          
157000       PERFORM GBCA-LAES-WLKREE11                                         
157100       MOVE 3                    TO ANSV-KDCALL                           
157200       MOVE MID-IDDISTR          TO ANSV-IDDISTR                          
157300       MOVE MSGI-IDDC            TO ANSV-IDDC                             
157400       MOVE MID-IDKUNDNR(INDX)   TO ANSV-IDKUNDNR                         
157600       MOVE LEV-KDANMORS         TO ANSV-KDANMORS                         
157601                                                                          
157602       MOVE LEV-KDANMORS         TO W-KDANMORS                            
157603       IF KDANMORS-BUYBAC-98                                              
157604         MOVE JA                 TO W-FLBUYBAC                            
157607       END-IF                                                             
157608                                                                          
157610       MOVE ZERO                 TO ANSV-KDORDKL                          
157620                                    ANSV-ADLAGOMR                         
157700                                                                          
157800       CALL W418ANSV USING ANSV-W418ANSV 4113-PCB 4115-PCB                
157900                                         4117-PCB                         
158000                                                                          
158100       IF ANSV-OK                                                         
158200          MOVE ANSV-KDARBTYP     TO W-KDARBTYP                            
158300          MOVE ANSV-IDPERSON     TO W-IDPERSON                            
158400       ELSE                                                               
158500          MOVE 'RET'             TO W-KDARBTYP                            
158600          MOVE 9                 TO W-IDPERSON                            
158700       END-IF                                                             
158800     END-IF                                                               
158900     .                                                                    
159000     EJECT                                                                
159100                                                                          
159200 HDAAC-INIT-RT-INFO   SECTION.                                            
159300                                                                          
159400     MOVE MID-IDDISTR          TO RET-IDDISTR                             
159500     MOVE MID-IDKUNDNR(INDX)   TO RET-IDKUNDNR                            
159600     MOVE MID-IDRAPPNR(INDX)   TO RET-IDRAPPNR                            
159700     MOVE W-FLFARLIG           TO RET-FLFARLIG                            
159710     MOVE W-FLBUYBAC           TO RET-FLBUYBAC                            
159800     IF MID-IDKOLLI NUMERIC AND MID-IDKOLLI > ZERO                        
159900       MOVE MID-IDKOLLI          TO RET-IDKOLLI                           
160000     ELSE                                                                 
160100       MOVE MID-IDKOLLI-FOM      TO RET-IDKOLLI                           
160200     END-IF                                                               
160300     MOVE W-IDPERSON           TO RET-IDPERSON                            
160400     MOVE W-KDARBTYP           TO RET-KDARBTYP                            
160500     MOVE W-KVRADER            TO RET-KVRADER                             
160600     IF MID-KVKOLLI(INDX)      NOT = ALL '+'                              
160700       MOVE MID-KVKOLLI(INDX)    TO RET-KVKOLLI-AAF                       
160800     ELSE                                                                 
160900       MOVE ZERO                 TO RET-KVKOLLI-AAF                       
161000     END-IF                                                               
161100                                                                          
161200     IF MID-IDFRASED(INDX)       NOT = ALL '+'                            
161300        MOVE MID-IDFRASED(INDX)  TO RET-IDFRASED-AAF                      
161400     ELSE                                                                 
161500        MOVE SPACE               TO RET-IDFRASED-AAF                      
161600     END-IF                                                               
161700                                                                          
161800     IF MID-TERETNOT(INDX)     NOT = ALL '+'                              
161900        MOVE MID-TERETNOT(INDX) TO RET-TERETNOT                           
162000     ELSE                                                                 
162100        MOVE SPACE             TO RET-TERETNOT                            
162200     END-IF                                                               
162300                                                                          
162400     MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DASNDDAT                     
162500     .                                                                    
162600     EJECT                                                                
162700 S01-FELMARKERA SECTION.                                                  
162800                                                                          
162900     IF LOSS-INFO                                                         
163000        PERFORM S01A-FELMARKERA-LOSS-INFO                                 
163100     END-IF                                                               
163200                                                                          
163300     IF MOT-INFO                                                          
163400        PERFORM S01B-FELMARKERA-MOT-INFO                                  
163500     END-IF                                                               
163600                                                                          
163700     IF KOLLI-INFO                                                        
163800        PERFORM S01C-FELMARKERA-KOLLI-INFO                                
163900     END-IF                                                               
164000                                                                          
164100     .                                                                    
164200     EJECT                                                                
164300                                                                          
164400 S01A-FELMARKERA-LOSS-INFO SECTION.                                       
164500                                                                          
164600     MOVE MFS-NUM-FAELT-FEL   TO MOD-KVKOLLI-LOSS-ATTR                    
164700                                 MOD-IDANSTNR-LOSS-ATTR                   
164800     MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADINLOMR-LOSS-ATTR                   
164900                                 MOD-IDFRASED-LOSS-ATTR                   
165000     MOVE NEJ                 TO INDATA-SW                                
165100     .                                                                    
165200     EJECT                                                                
165300                                                                          
165400 S01B-FELMARKERA-MOT-INFO SECTION.                                        
165500                                                                          
165600     MOVE MFS-NUM-FAELT-FEL   TO MOD-KVKOLLI-MOT-ATTR                     
165700                                 MOD-IDANSTNR-MOT-ATTR                    
165800     MOVE MFS-ALFA-FAELT-FEL  TO MOD-ADINLOMR-MOT-ATTR                    
165900                                 MOD-IDFRASED-MOT-ATTR                    
166000     MOVE NEJ                 TO INDATA-SW                                
166100     .                                                                    
166200     EJECT                                                                
166300                                                                          
166400 S01C-FELMARKERA-KOLLI-INFO SECTION.                                      
166500                                                                          
166600     MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-ATTR                         
166700                                 MOD-IDDISTR-ATTR                         
166800                                 MOD-IDKOLLI-FOM-ATTR                     
166900                                 MOD-IDKOLLI-TOM-ATTR                     
167000     MOVE NEJ                 TO INDATA-SW                                
167100                                                                          
167200     .                                                                    
167300     EJECT                                                                
167400                                                                          
167500                                                                          
167600 S02-INIT-WLRETA01  SECTION.                                              
167700                                                                          
167800     MOVE ZERO                 TO RET-IDDISTR                             
167900                                  RET-IDKUNDNR                            
168000                                  RET-IDRAPPNR                            
168100     MOVE NEJ                  TO RET-FLFARLIG                            
168110     MOVE NEJ                  TO RET-FLBUYBAC                            
168200     MOVE SPACE                TO RET-IDFRASED-AAF                        
168300     MOVE SPACE                TO RET-IDFRASED-CDC                        
168400     MOVE ZERO                 TO RET-IDKOLLI                             
168500     MOVE ZERO                 TO RET-IDPERSON                            
168600     MOVE MSGI-IDRT            TO RET-IDRT                                
168700     MOVE MSGI-IDRTLOP         TO RET-IDRTLOP                             
168800     MOVE ZERO                 TO RET-KDARBTYP                            
168900                                  RET-KDKOLSTA                            
169000     MOVE W-SND-SAENT          TO RET-KDRETSTA                            
169100     MOVE ZERO                 TO RET-KVRADER                             
169200     MOVE SPACE                TO RET-TERETNOT                            
169300     MOVE ZERO                 TO RET-TIINLMOT                            
169400                                  RET-TIKLAR                              
169500                                  RET-TILOSSN                             
169600                                  RET-DARETANK                            
169700                                  RET-TIREGDAT-TRRT                       
169710                                  RET-TISNDDAT-TRRT                       
169720     MOVE SPACE                TO RET-IDRT-TRANSIT                        
169800     .                                                                    
169900     EJECT                                                                
170000                                                                          
170100 S03-RAEKNA-KOLLI         SECTION.                                        
170200                                                                          
170300     MOVE ZERO                    TO W-KVKOLLI-LOSS                       
170400                                     W-KVKOLLI-MOT                        
170500     PERFORM UNTIL SEGMENT-SAKNAS                                         
170600       IF RET-IDKOLLI = SPAR-IDKOLLI                                      
170700         CONTINUE                                                         
170800       ELSE                                                               
170900         IF RET-TILOSSN > ZERO                                            
171000            IF RET-KDKOLSTA = W-KLI-SAK                                   
171100              CONTINUE                                                    
171200            ELSE                                                          
171300              ADD +1         TO W-KVKOLLI-LOSS                            
171400            END-IF                                                        
171500         END-IF                                                           
171600         IF RET-TIINLMOT > ZERO                                           
171700            IF RET-KDKOLSTA = W-KLI-AVV                                   
171800              CONTINUE                                                    
171900            ELSE                                                          
172000              ADD +1         TO W-KVKOLLI-MOT                             
172100            END-IF                                                        
172200         END-IF                                                           
172300       END-IF                                                             
172400                                                                          
172500       MOVE RET-IDKOLLI TO SPAR-IDKOLLI                                   
172600                                                                          
172700       PERFORM IMS-GHN-SEQB-WLRETA01                                      
172800     END-PERFORM                                                          
172900     .                                                                    
173000                                                                          
173100 MFS-RENSA-FAELT-IN SECTION.                                              
173200                                                                          
173300     MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI-LOSS-UPD                         
173400                             MOD-IDANSTNR-LOSS-UPD                        
173500                             MOD-ADINLOMR-LOSS-UPD                        
173600                             MOD-IDFRASED-LOSS-UPD                        
173700                             MOD-KVKOLLI-MOT-UPD                          
173800                             MOD-IDANSTNR-MOT-UPD                         
173900                             MOD-ADINLOMR-MOT-UPD                         
174000                             MOD-IDFRASED-MOT-UPD                         
174100                             MOD-IDKOLLI                                  
174200                             MOD-IDDISTR                                  
174300                             MOD-IDKOLLI-FOM                              
174400                             MOD-IDKOLLI-TOM                              
174500                             MOD-FLNYSND                                  
174600                                                                          
174700     MOVE +1              TO INDX                                         
174800     PERFORM UNTIL INDX   > MAX-INDX                                      
174900       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
175000       ADD +1             TO INDX                                         
175100     END-PERFORM                                                          
175200     .                                                                    
175300                                                                          
175400 MFS-RENSA-RAD-FAELT-IN  SECTION.                                         
175500                                                                          
175600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR   (INDX)                        
175700                             MOD-IDRAPPNR   (INDX)                        
175800                             MOD-KVKOLLI    (INDX)                        
175900                             MOD-IDFRASED   (INDX)                        
176000                             MOD-TERETNOT   (INDX)                        
176100                                                                          
176200     .                                                                    
176300     EJECT                                                                
176400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
176500                                                                          
176600     MOVE MFS-ROER-EJ-FAELT TO MOD-KVKOLLI-LOSS-UPD                       
176700                               MOD-IDANSTNR-LOSS-UPD                      
176800                               MOD-ADINLOMR-LOSS-UPD                      
176900                               MOD-IDFRASED-LOSS-UPD                      
177000                               MOD-KVKOLLI-MOT-UPD                        
177100                               MOD-IDANSTNR-MOT-UPD                       
177200                               MOD-ADINLOMR-MOT-UPD                       
177300                               MOD-IDFRASED-MOT-UPD                       
177400                               MOD-IDKOLLI                                
177500                               MOD-IDDISTR                                
177600                               MOD-IDKOLLI-FOM                            
177700                               MOD-IDKOLLI-TOM                            
177800                               MOD-FLNYSND                                
177900     MOVE +1                TO INDX                                       
178000     PERFORM UNTIL INDX   > MAX-INDX                                      
178100       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
178200       ADD +1             TO INDX                                         
178300     END-PERFORM                                                          
178400     .                                                                    
178500                                                                          
178600 MFS-ROER-EJ-RAD-FAELT-IN  SECTION.                                       
178700                                                                          
178800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR   (INDX)                      
178900                               MOD-IDRAPPNR   (INDX)                      
179000                               MOD-KVKOLLI    (INDX)                      
179100                               MOD-IDFRASED   (INDX)                      
179200                               MOD-TERETNOT   (INDX)                      
179300     .                                                                    
179400     SKIP3                                                                
179500 MFS-RENSA-FAELT-UT SECTION.                                              
179600                                                                          
179700     MOVE MFS-RENSA-FAELT       TO MOD-KVKOLLI-LOSS                       
179800                                   MOD-IDANSTNR-LOSS                      
179900                                   MOD-ADINLOMR-LOSS                      
180000                                   MOD-IDFRASED-LOSS                      
180100                                                                          
180200                                   MOD-KVKOLLI-MOT                        
180300                                   MOD-IDANSTNR-MOT                       
180400                                   MOD-ADINLOMR-MOT                       
180500                                   MOD-IDFRASED-MOT                       
180600     .                                                                    
180700     SKIP3                                                                
180800 MFS-ROER-EJ-FAELT-UT SECTION.                                            
180900                                                                          
181000     MOVE MFS-ROER-EJ-FAELT     TO MOD-KVKOLLI-LOSS                       
181100                                   MOD-IDANSTNR-LOSS                      
181200                                   MOD-ADINLOMR-LOSS                      
181300                                   MOD-IDFRASED-LOSS                      
181400                                                                          
181500                                   MOD-KVKOLLI-MOT                        
181600                                   MOD-IDANSTNR-MOT                       
181700                                   MOD-ADINLOMR-MOT                       
181800                                   MOD-IDFRASED-MOT                       
181900     .                                                                    
182000     SKIP3                                                                
182100 MFS-FORM-ATTR SECTION.                                                   
182200                                                                          
182300     MOVE MFS-FORMATETS-ATTR TO MOD-KVKOLLI-LOSS-ATTR                     
182400                                MOD-IDANSTNR-LOSS-ATTR                    
182500                                MOD-ADINLOMR-LOSS-ATTR                    
182600                                MOD-IDFRASED-LOSS-ATTR                    
182700                                MOD-KVKOLLI-MOT-ATTR                      
182800                                MOD-IDANSTNR-MOT-ATTR                     
182900                                MOD-ADINLOMR-MOT-ATTR                     
183000                                MOD-IDFRASED-MOT-ATTR                     
183100                                MOD-IDKOLLI-ATTR                          
183200                                MOD-IDDISTR-ATTR                          
183300                                MOD-IDKOLLI-FOM-ATTR                      
183400                                MOD-IDKOLLI-TOM-ATTR                      
183500                                MOD-FLNYSND-ATTR                          
183600                                                                          
183700     MOVE +1                 TO INDX                                      
183800     PERFORM UNTIL INDX     > MAX-INDX                                    
183900       PERFORM MFS-FORM-ATTR-RAD-FAELT-IN                                 
184000       ADD +1             TO INDX                                         
184100     END-PERFORM                                                          
184200     .                                                                    
184300                                                                          
184400 MFS-FORM-ATTR-RAD-FAELT-IN  SECTION.                                     
184500                                                                          
184600     MOVE MFS-FORMATETS-ATTR TO MOD-IDRAPPNR-ATTR   (INDX)                
184700                                MOD-KVKOLLI-ATTR (INDX)                   
184800                                MOD-IDFRASED-ATTR   (INDX)                
184900                                MOD-TERETNOT-ATTR   (INDX)                
185000     .                                                                    
185100     EJECT                                                                
185200 MFS-LAES-IN-IGEN SECTION.                                                
185300                                                                          
185400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVKOLLI-LOSS-ATTR                  
185500                                   MOD-IDANSTNR-LOSS-ATTR                 
185600                                   MOD-ADINLOMR-LOSS-ATTR                 
185700                                   MOD-IDFRASED-LOSS-ATTR                 
185800                                   MOD-KVKOLLI-MOT-ATTR                   
185900                                   MOD-IDANSTNR-MOT-ATTR                  
186000                                   MOD-ADINLOMR-MOT-ATTR                  
186100                                   MOD-IDFRASED-MOT-ATTR                  
186200                                   MOD-IDKOLLI-ATTR                       
186300                                   MOD-IDDISTR-ATTR                       
186400                                   MOD-IDKOLLI-FOM-ATTR                   
186500                                   MOD-IDKOLLI-TOM-ATTR                   
186600                                   MOD-FLNYSND-ATTR                       
186700                                                                          
186800     MOVE +1                 TO INDX                                      
186900     PERFORM UNTIL INDX     > MAX-INDX                                    
187000       PERFORM MFS-LAES-IN-IGEN-RAD                                       
187100       ADD +1             TO INDX                                         
187200     END-PERFORM                                                          
187300     .                                                                    
187400                                                                          
187500 MFS-LAES-IN-IGEN-RAD        SECTION.                                     
187600                                                                          
187700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-ATTR   (INDX)             
187800                                   MOD-IDRAPPNR-ATTR   (INDX)             
187900                                   MOD-KVKOLLI-ATTR (INDX)                
188000                                   MOD-IDFRASED-ATTR   (INDX)             
188100                                   MOD-TERETNOT-ATTR   (INDX)             
188200     .                                                                    
188300     EJECT                                                                
188400 MFS-STAENG-FAELT-IN SECTION.                                             
188500                                                                          
188600*    --- ALLA INDATA-FÄLT                                                 
188700     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KVKOLLI-LOSS-ATTR                 
188800                                    MOD-IDANSTNR-LOSS-ATTR                
188900                                    MOD-ADINLOMR-LOSS-ATTR                
189000                                    MOD-IDFRASED-LOSS-ATTR                
189100                                    MOD-KVKOLLI-MOT-ATTR                  
189200                                    MOD-IDANSTNR-MOT-ATTR                 
189300                                    MOD-ADINLOMR-MOT-ATTR                 
189400                                    MOD-IDFRASED-MOT-ATTR                 
189500                                    MOD-IDKOLLI-ATTR                      
189600                                    MOD-IDDISTR-ATTR                      
189700                                    MOD-IDKOLLI-FOM-ATTR                  
189800                                    MOD-IDKOLLI-TOM-ATTR                  
189900                                                                          
190000     MOVE +1                 TO INDX                                      
190100     PERFORM UNTIL INDX     > MAX-INDX                                    
190200       PERFORM MFS-STAENG-FAELT-RAD                                       
190300       ADD +1             TO INDX                                         
190400     END-PERFORM                                                          
190500     .                                                                    
190600     EJECT                                                                
190700 MFS-STAENG-FAELT-RAD        SECTION.                                     
190800                                                                          
190900     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDKUNDNR-ATTR   (INDX)            
191000                                    MOD-IDRAPPNR-ATTR   (INDX)            
191100                                    MOD-KVKOLLI-ATTR    (INDX)            
191200                                    MOD-IDFRASED-ATTR   (INDX)            
191300                                    MOD-TERETNOT-ATTR   (INDX)            
191400     .                                                                    
191500     EJECT                                                                
191600* --- IMS SEKTIONER ---                                                   
191700     SKIP3                                                                
191800 IMS-GET-MSG SECTION.                                                     
191900                                                                          
192000     MOVE '  QC' TO GODK-STATUSKODER                                      
192100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
192200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
192300     PERFORM IMS-STATUSKONTROLL                                           
192400     .                                                                    
192500     SKIP3                                                                
192600 IMS-INSERT-MSG SECTION.                                                  
192700                                                                          
192800     IF MSGI-IDLAND-SPR = 'GB'                                            
192900       MOVE 'N' TO MFS-KDHUVOMR                                           
193000     END-IF                                                               
193100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
193200     MOVE SPACE TO GODK-STATUSKODER                                       
193300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
193400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
193500     PERFORM IMS-STATUSKONTROLL                                           
193600     .                                                                    
193700     EJECT                                                                
193800 IMS-GU-WLRETG01 SECTION.                                                 
193900                                                                          
194000     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
194100                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
194200          DELIMITED BY SIZE INTO SSA1                                     
194300     MOVE '  GE' TO GODK-STATUSKODER                                      
194400     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA2 SSA1                     
194500     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
194600     PERFORM IMS-STATUSKONTROLL                                           
194700     .                                                                    
194800     SKIP3                                                                
194900 IMS-GN-WLRETG01 SECTION.                                                 
195000                                                                          
195100     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
195200                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
195300          DELIMITED BY SIZE INTO SSA1                                     
195400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
195500     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA2 SSA1                     
195600     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
195700     PERFORM IMS-STATUSKONTROLL                                           
195800     .                                                                    
195900     SKIP3                                                                
196000 IMS-ISRT-WLRETA01    SECTION.                                            
196100                                                                          
196200     MOVE 'WLRETA01 ' TO SSA1                                             
196300     MOVE '  II' TO GODK-STATUSKODER                                      
196400     CALL CBLTDLI USING ISRT RETA1-PCB DLI-IO-AREA SSA1                   
196500     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
196600     PERFORM IMS-STATUSKONTROLL                                           
196700     .                                                                    
196800     EJECT                                                                
196900 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
197000                                                                          
197100     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
197200                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
197300          DELIMITED BY SIZE INTO SSA1                                     
197400     MOVE '  GE'           TO GODK-STATUSKODER                            
197500     CALL CBLTDLI USING GHU RETA2-PCB DLI-IO-AREA SSA1                    
197600     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
197700     PERFORM IMS-STATUSKONTROLL                                           
197800     .                                                                    
197900                                                                          
198000 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
198100                                                                          
198200     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
198300                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
198400          DELIMITED BY SIZE INTO SSA1                                     
198500     MOVE '  GE' TO GODK-STATUSKODER                                      
198600     CALL CBLTDLI USING GHN RETA2-PCB DLI-IO-AREA SSA1                    
198700     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
198800     PERFORM IMS-STATUSKONTROLL                                           
198900     .                                                                    
199000     EJECT                                                                
199100                                                                          
199200 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
199300                                                                          
199400     MOVE '    '           TO GODK-STATUSKODER                            
199500     CALL CBLTDLI USING REPL RETA2-PCB DLI-IO-AREA                        
199600     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
199700     PERFORM IMS-STATUSKONTROLL                                           
199800     .                                                                    
199900     EJECT                                                                
200000 IMS-GU-WLKREE01    SECTION.                                              
200100                                                                          
200200     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
200300          DELIMITED BY SIZE INTO SSA1                                     
200400     MOVE '  GE' TO GODK-STATUSKODER                                      
200500     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA2 SSA1                     
200600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
200700     PERFORM IMS-STATUSKONTROLL                                           
200800     .                                                                    
200900     SKIP2                                                                
201000                                                                          
201100 IMS-GNP-WLKREE11    SECTION.                                             
201200                                                                          
201300     MOVE 'WLKREE11 ' TO SSA1                                             
201400     MOVE '  GE' TO GODK-STATUSKODER                                      
201500     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
201600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     .                                                                    
201900     EJECT                                                                
202000                                                                          
202100 IMS-GU-WL411101  SECTION.                                                
202200                                                                          
202300     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
202400                      DELIMITED BY SIZE INTO SSA1                         
202500     MOVE '    ' TO GODK-STATUSKODER                                      
202600     CALL CBLTDLI USING GU 4111-PCB WL411101 SSA1                         
202700     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
202800     PERFORM IMS-STATUSKONTROLL                                           
202900     .                                                                    
203000     SKIP2                                                                
203100 IMS-GHNP-WL411111  SECTION.                                              
203200                                                                          
203300     MOVE  'WL411111*F' TO SSA1                                           
203400     MOVE '    ' TO GODK-STATUSKODER                                      
203500     CALL CBLTDLI USING GHNP 4111-PCB WL411111 SSA1                       
203600     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
203700     PERFORM IMS-STATUSKONTROLL                                           
203800     .                                                                    
203900     SKIP2                                                                
204000 IMS-REPL-WL411111  SECTION.                                              
204100                                                                          
204200     MOVE '  ' TO GODK-STATUSKODER                                        
204300     CALL CBLTDLI USING REPL 4111-PCB WL411111                            
204400     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
204500     PERFORM IMS-STATUSKONTROLL                                           
204600     .                                                                    
204700     SKIP2                                                                
204800 IMS-STATUSKONTROLL SECTION.                                              
204900                                                                          
205000     SET STATUS-IX TO 1                                                   
205100     SEARCH GODK-STATUS                                                   
205200       AT END                                                             
205300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
205400         DELIMITED BY SIZE INTO FELTEXT                                   
205500         CALL FELLOG                                                      
205600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
205700         CONTINUE                                                         
205800     END-SEARCH                                                           
205900     .                                                                    
