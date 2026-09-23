000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0149      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4073400.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/06/21.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        VISAR KOLLIKÖ                                                    
001500*        INLÄGGNING AV HELT KOLLI ELLER VAL AV KOLLI FÖR VIDARE           
001600*        BEHANDLING                                                       
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001900*        PROGRAMMET LÄSER      WLRETB (WDA3)                              
002000*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
002100*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
002200*                                                                         
002300*    E-TRACKER: 8635407  2009-12  RETURN CODE MATRIX                      
002400*                                                                         
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T734                                              
002800*        MID:         W4I73401                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W4O73401                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800*    --  CHECKED BY WY2000                                                
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W4073400'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  W-CDC                       PIC X(3)    VALUE 'CDC'.                 
004900                                                                          
005000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005300 77  4792-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  4792-MAX-INDX               PIC S9(4)  VALUE +24   COMP SYNC.        
005500 77  4797-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  4797-MAX-INDX               PIC S9(4)  VALUE +16   COMP SYNC.        
005700 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005800                                                                          
005900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006000 01  WS-IDANSV                   PIC X(4)   VALUE SPACE.                  
006100 01  WS-IDRETSND.                                                         
006200   03 WS-IDRT                    PIC X(3)   VALUE SPACE.                  
006300   03 WS-IDRTLOP                 PIC X(3)   VALUE SPACE.                  
006400 01  WS-IDKOLLI                  PIC X(5)   VALUE SPACE.                  
006500 01  WS-IDDISTR                  PIC X(5)   VALUE SPACE.                  
006600 01  WS-FLVISAAV                 PIC X(1)   VALUE SPACE.                  
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007300     88  NYCKLAR-OK                          VALUE 'J'.                   
007400     88  NYCKLAR-FEL                         VALUE 'N'.                   
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  EGEN-MID                            VALUE '4734'.                
007800     88  GODK-MID                            VALUE '4734' '4735'.         
007900     88  HELP-MID                            VALUE '0551'.                
008000     EJECT                                                                
008100*    --- KONSTANTER                                                       
008200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
008300*                                                                         
008400 77  W-INLAEGGNING               PIC  X(3)  VALUE 'INL'.                  
008500 77  W-VAELJKOLLI                PIC  X(3)  VALUE 'VK'.                   
008600 77  W-OK                        PIC  X(3)  VALUE 'OK'.                   
008700 77  W-BINNED                    PIC  X(3)  VALUE 'BIN'.                  
008800 77  W-SELECTCASE                PIC  X(3)  VALUE 'SC'.                   
008900 77  W-SND-SAENT                 PIC X(1)   VALUE '2'.                    
009000 77  W-SND-LOSS                  PIC X(1)   VALUE '3'.                    
009100 77  W-SND-MOT                   PIC X(1)   VALUE '4'.                    
009200 77  W-SND-PAAB                  PIC X(1)   VALUE '5'.                    
009300 77  W-SND-INL                   PIC X(1)   VALUE '6'.                    
009400 77  W-ANM-MOT                   PIC X(1)   VALUE '5'.                    
009500 77  W-ANM-PAAB                  PIC X(1)   VALUE '6'.                    
009600 77  W-KLI-LOSS                  PIC S9(1)  VALUE +4 COMP-3.              
009700 77  W-KLI-MOT                   PIC S9(1)  VALUE +5 COMP-3.              
009800 77  W-KLI-SAK                   PIC S9(1)  VALUE +6 COMP-3.              
009900 77  W-KLI-AVV                   PIC S9(1)  VALUE +7 COMP-3.              
010000 77  W-KLI-VALT                  PIC S9(1)  VALUE +8 COMP-3.              
010100 77  W-KLI-BEH                   PIC S9(1)  VALUE +9 COMP-3.              
010200 77  W-IDRT                      PIC  X(3)  VALUE SPACE.                  
010300                                                                          
010400 77  W-IDANSTNR                  PIC S9(5)  VALUE ZERO COMP-3.            
010500 77  W-KVKOLLI-LOSS              PIC S9(5)  VALUE ZERO COMP-3.            
010600 77  W-KVKOLLI-MOT               PIC S9(5)  VALUE ZERO COMP-3.            
010700 77  W-KVKOLLI-SND               PIC S9(5)  VALUE ZERO COMP-3.            
010800 77  W-KVRADER-KVAR              PIC S9(5)  VALUE ZERO COMP-3.            
010900 77  W-IDRTLOP-NUM               PIC  9(3)  VALUE ZERO.                   
011000 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011100 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
011200 77  W-SPAR-IDRT                 PIC X(3)    VALUE SPACE.                 
011300 77  W-SPAR-IDRTLOP              PIC 9(3)    VALUE ZERO.                  
011400 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
011500                                                                          
011600 77  SW-SOEKNING                 PIC X       VALUE '0'.                   
011700     88  INGEN-SOEK                          VALUE '0'.                   
011800     88  IDANSV-SOEK                         VALUE '1'.                   
011900     88  IDRETSND-SOEK                       VALUE '2'.                   
012000     88  IDDISTR-SOEK                        VALUE '3'.                   
012100                                                                          
012200 77  SW-FOERSTA-VALDA            PIC X       VALUE 'J'.                   
012300     88  FOERSTA-VALDA-KOLLI                 VALUE 'J'.                   
012400                                                                          
012500 77  SW-KOLLI-VALT               PIC X       VALUE 'N'.                   
012600     88  KOLLI-VALT                          VALUE 'J'.                   
012700                                                                          
012800 77  SW-KOLLI-INL                PIC X       VALUE 'N'.                   
012900     88  KOLLI-INL                           VALUE 'J'.                   
013000                                                                          
013100 77  SW-KOLLI-OK                 PIC X       VALUE 'N'.                   
013200     88  KOLLI-OK                            VALUE 'J'.                   
013300                                                                          
013400 77  SW-AVVIKELSER               PIC X       VALUE 'N'.                   
013500     88  VISA-AVVIKELSER                     VALUE 'J'.                   
013600     88  VISA-EJ-AVVIKELSER                  VALUE 'N'.                   
013700                                                                          
013800                                                                          
013900*    --- DATUM                                                            
014000                                                                          
014100 01  W-TIAADDD.                                                           
014200     03  FILLER                  PIC 9(1)    VALUE ZERO.                  
014300     03  W-TIAA                  PIC 9(2)    VALUE ZERO.                  
014400     03  W-TIDDD                 PIC 9(3)    VALUE ZERO.                  
014500                                                                          
014600 01  W-TIAADDD-IDAG.                                                      
014700     03  W-TIAA-IDAG             PIC 9(2).                                
014800     03  W-TIDDD-IDAG            PIC 9(3).                                
014900                                                                          
015000 01  W-TIAAAAMMDD-KLIARB.                                                 
015100     03  W-TISEKEL-KLIARB            PIC 9(2).                            
015200     03  W-TIAAMMDD-KLIARB           PIC 9(6).                            
015300 01  W-TIAAAAMMDD-KLIAVV.                                                 
015400     03  W-TISEKEL-KLIAVV            PIC 9(2).                            
015500     03  W-TIAAMMDD-KLIAVV           PIC 9(6).                            
015600                                                                          
015700 01  WS-DATUM-TID.                                                        
015800     03 WS-DATUM               PIC X(8)    VALUE SPACE.                   
015900     03 WS-KLOCKAN             PIC 9(10)   VALUE ZERO.                    
016000                                                                          
016100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016200 01  GENERELLA-SUBPROGRAM.                                                
016300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
016500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016900     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
017000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017100     EJECT                                                                
017200*    ---  LÄNKAREA TILL WDATKONV                                          
017300 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
017400                                                                          
017500*01 -COPY WDATAREA                                                        
017600     EJECT                                                                
017700*    ---  LÄNKAREA TILL W418OKOD                                          
017800 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
017900                                                                          
018000*01 -COPY W418OKOD           -PRE OKOD-.                                  
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018300*01 -COPY WMEDAREA                                                        
018400     SKIP3                                                                
018500 01  MESSAGE-CODES.                                                       
018600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018700     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
018800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
018900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
019100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
019300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019400     03  ERR-LAGER-SAKNAS        PIC X(3)    VALUE '764'.                 
019500     EJECT                                                                
019600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019700*                                                                         
019800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
019900     SKIP3                                                                
020000*01 -COPY WMSGINIT                                                        
020100     SKIP3                                                                
020200*                                                                         
020300*    --- VALID IDDC CODES                                                 
020400*01 -COPY WWDC99                                                          
020500*                                                                         
020600*                                                                         
020700*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
020800   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
020900     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
021000                                                                          
021100 01  BILD-HOPP-AREOR.                                                     
021200                                                                          
021300   03    W-BILD               PIC X(4)    VALUE SPACE.                    
021400   03    W-HOPP-IDTRANS.                                                  
021500     05  FILLER               PIC X(1)    VALUE 'W'.                      
021600     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
021700     05  FILLER               PIC X(1)    VALUE 'T'.                      
021800     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
021900     05  FILLER               PIC X(2)    VALUE SPACE.                    
022000                                                                          
022100                                                                          
022200   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
022300   03      P-TO-P-SW.                                                     
022400                                                                          
022500     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
022600     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
022700     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
022800     05  P-TO-P-KDTRANS          PIC X(8).                                
022900     05  P-TO-P-IDTRANS          PIC X(4).                                
023000     05  P-TO-P-KDMFSFOR         PIC X(1).                                
023100     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
023200                                                                          
023300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023500     SKIP3                                                                
023600*01  MID -COPY W4I73401                                                   
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023900     SKIP3                                                                
024000*01  -COPY WMSGAREA                                                       
024100     EJECT                                                                
024200     03  MOD REDEFINES MSG-AREA.                                          
024300*      05  -COPY W4O73401   -PRE MOD-                                     
024400     EJECT                                                                
024500   03    MOD-MENY            REDEFINES MSG-AREA.                          
024600     05  FILLER              PIC X(4).                                    
024700     05  MOD-KDMFSFOR        PIC X(1).                                    
024800     05  MOD-TEMFSINF2       PIC X(55).                                   
024900     05  FILLER              PIC X(1873).                                 
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025200     SKIP3                                                                
025300*01  -COPY WMFSAREA                                                       
025400     EJECT                                                                
025500                                                                          
025600 77  W-KVKOLLI                   PIC  9(4)   VALUE ZERO.                  
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
025900     SKIP3                                                                
026000 01  KOM-MSG-IO-AREA.                                                     
026100*03  -COPY WMSGKOM                                                        
026200     EJECT                                                                
026300 01  FILLER                   PIC X(16)   VALUE 'MSG/KOM-AREA'.           
026400     SKIP2                                                                
026500*01  -COPY WMSGSNUF           -PRE P-TO-P-                                
026600                                                                          
026700     EJECT                                                                
026800 01      FILLER                  PIC X(24)   VALUE                        
026900                                 'MOD4792-MID-W4I79201'.                  
027000     SKIP2                                                                
027100     -COPY W4I79201 -PRE MOD4792-                                         
027200     EJECT                                                                
027300 01      FILLER                  PIC X(24)   VALUE                        
027400                                 'MOD4797-MID-W4I79701'.                  
027500     SKIP2                                                                
027600     -COPY W4I79701 -PRE MOD4797-                                         
027700     EJECT                                                                
027800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027900*                                                                         
028000     EJECT                                                                
028100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028200     SKIP3                                                                
028300 01  NYCKLAR-TILL-BLAEDDRING.                                             
028400   02    W-MINKEY.                                                        
028500     03  W-MINKEY-IDTRANS         PIC  X(4)          VALUE '4734'.        
028600     03  W-MINKEY-IDSOEK          PIC  X(1)          VALUE ZERO.          
028700     03  W-MINKEY-SSA-ENTER       PIC  X(47).                             
028800     03  W-MINKEY-WDA3BSEQ REDEFINES W-MINKEY-SSA-ENTER.                  
028900         05  W-MINKEYB-IDRT       PIC  X(3).                              
029000         05  W-MINKEYB-IDDC       PIC  X(2).                              
029100         05  W-MINKEYB-IDRTLOP    PIC  9(3).                              
029200         05  W-MINKEYB-IDKOLLI    PIC S9(5)   COMP-3.                     
029300                                                                          
029400     03  W-MINKEY-WDA3FSEQ REDEFINES W-MINKEY-SSA-ENTER.                  
029500         05  W-MINKEYF-IDDC       PIC  X(2).                              
029600         05  W-MINKEYF-IDDISTR    PIC S9(5)   COMP-3.                     
029700         05  W-MINKEYF-IDKUNDNR   PIC S9(7)   COMP-3.                     
029800         05  W-MINKEYF-IDRAPPNR   PIC  9(7).                              
029900         05  W-MINKEYF-IDRT       PIC  X(3).                              
030000         05  W-MINKEYF-IDRTLOP    PIC  9(3).                              
030100         05  W-MINKEYF-IDKOLLI    PIC S9(5)   COMP-3.                     
030200                                                                          
030300     03  W-MINKEY-WDA3G1KY REDEFINES W-MINKEY-SSA-ENTER.                  
030400         05  W-MINKEYG1-IDDC       PIC  X(2).                             
030500         05  W-MINKEYG1-KDRETSTA   PIC  X(1).                             
030600         05  W-MINKEYG1-KDARBTYP   PIC  X(8).                             
030700         05  W-MINKEYG1-IDPERSON   PIC S9(3)   COMP-3.                    
030800         05  W-MINKEYG1-DARETANK   PIC  9(8).                             
030900         05  W-MINKEYG1-IDRT       PIC  X(3).                             
031000         05  W-MINKEYG1-IDRTLOP    PIC  9(3).                             
031100         05  W-MINKEYG1-IDKOLLI    PIC S9(5)   COMP-3.                    
031200         05  W-MINKEYG1-DAREGDAT   PIC  9(8).                             
031300         05  W-MINKEYG1-TIKLOCK    PIC S9(9)   COMP-3.                    
031400     03  W-MINKEY-SSA-NEXT        PIC  X(47).                             
031500     03  W-MINKEY-WDA3BSEQ REDEFINES W-MINKEY-SSA-NEXT.                   
031600         05  W-MINKEYB-IDRT-NEXT       PIC  X(3).                         
031700         05  W-MINKEYB-IDDC-NEXT       PIC  X(2).                         
031800         05  W-MINKEYB-IDRTLOP-NEXT    PIC  9(3).                         
031900         05  W-MINKEYB-IDKOLLI-NEXT    PIC S9(5)   COMP-3.                
032000                                                                          
032100     03  W-MINKEY-WDA3FSEQ REDEFINES W-MINKEY-SSA-NEXT.                   
032200         05  W-MINKEYF-IDDC-NEXT       PIC  X(2).                         
032300         05  W-MINKEYF-IDDISTR-NEXT    PIC S9(5)   COMP-3.                
032400         05  W-MINKEYF-IDKUNDNR-NEXT   PIC S9(7)   COMP-3.                
032500         05  W-MINKEYF-IDRAPPNR-NEXT   PIC  9(7).                         
032600         05  W-MINKEYF-IDRT-NEXT       PIC  X(3).                         
032700         05  W-MINKEYF-IDRTLOP-NEXT    PIC  9(3).                         
032800         05  W-MINKEYF-IDKOLLI-NEXT    PIC S9(5)   COMP-3.                
032900                                                                          
033000     03  W-MINKEY-WDA3G1KY REDEFINES W-MINKEY-SSA-NEXT.                   
033100         05  W-MINKEYG1-IDDC-NEXT      PIC  X(2).                         
033200         05  W-MINKEYG1-KDRETSTA-NEXT  PIC  X(1).                         
033300         05  W-MINKEYG1-KDARBTYP-NEXT  PIC  X(8).                         
033400         05  W-MINKEYG1-IDPERSON-NEXT  PIC S9(3)   COMP-3.                
033500         05  W-MINKEYG1-DARETANK-NEXT  PIC  9(8).                         
033600         05  W-MINKEYG1-IDRT-NEXT      PIC  X(3).                         
033700         05  W-MINKEYG1-IDRTLOP-NEXT   PIC  9(3).                         
033800         05  W-MINKEYG1-IDKOLLI-NEXT   PIC S9(5)   COMP-3.                
033900         05  W-MINKEYG1-DAREGDAT-NEXT  PIC  9(8).                         
034000         05  W-MINKEYG1-TIKLOCK-NEXT   PIC S9(9)   COMP-3.                
034100                                                                          
034200     SKIP3                                                                
034300 01  NYCKLAR-TILL-DLI.                                                    
034400                                                                          
034500     03  W-WDA301KY-X.                                                    
034600         05  W-IDDC                PIC  X(2)          VALUE SPACE.        
034700         05  W-DAREGDAT            PIC  9(8)          VALUE ZERO.         
034800         05  W-TIKLOCK             PIC S9(9)   COMP-3 VALUE ZERO.         
034900                                                                          
035000     03  W-WDA3BSEQ-MIN-X.                                                
035100         05  W-IDRT-BSEQ-MIN      PIC  X(3)          VALUE SPACE.         
035200         05  W-IDDC-BSEQ-MIN      PIC  X(2)          VALUE SPACE.         
035300         05  W-IDRTLOP-BSEQ-MIN   PIC  9(3)          VALUE ZERO.          
035400         05  W-IDKOLLI-BSEQ-MIN   PIC S9(5)   COMP-3 VALUE ZERO.          
035500                                                                          
035600     03  W-WDA3BSEQ-MAX-X.                                                
035700         05  W-IDRT-BSEQ-MAX      PIC  X(3)          VALUE SPACE.         
035800         05  W-IDDC-BSEQ-MAX      PIC  X(2)          VALUE SPACE.         
035900         05  W-IDRTLOP-BSEQ-MAX   PIC  9(3)          VALUE ZERO.          
036000         05  W-IDKOLLI-BSEQ-MAX   PIC S9(5)   COMP-3 VALUE ZERO.          
036100                                                                          
036200     03  W-WDA3G1KY-MIN-X.                                                
036300         05  W-IDDC-G1-MIN       PIC  X(2)          VALUE SPACE.          
036400         05  W-KDRETSTA-G1-MIN   PIC  X(1)          VALUE SPACE.          
036500         05  W-KDARBTYP-G1-MIN   PIC  X(8)          VALUE SPACE.          
036600         05  W-IDPERSON-G1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
036700         05  W-DARETANK-G1-MIN   PIC  9(8)          VALUE ZERO.           
036800         05  W-IDRT-G1-MIN       PIC  X(3)          VALUE SPACE.          
036900         05  W-IDRTLOP-G1-MIN    PIC  9(3)          VALUE ZERO.           
037000         05  W-IDKOLLI-G1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
037100         05  W-DAREGDAT-G1-MIN   PIC  9(8)          VALUE ZERO.           
037200         05  W-TIKLOCK-G1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
037300                                                                          
037400     03  W-WDA3G1KY-MAX-X.                                                
037500         05  W-IDDC-G1-MAX       PIC  X(2)          VALUE SPACE.          
037600         05  W-KDRETSTA-G1-MAX   PIC  X(1)          VALUE SPACE.          
037700         05  W-KDARBTYP-G1-MAX   PIC  X(8)          VALUE SPACE.          
037800         05  W-IDPERSON-G1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
037900         05  W-DARETANK-G1-MAX   PIC  9(8)          VALUE ZERO.           
038000         05  W-IDRT-G1-MAX       PIC  X(3)          VALUE SPACE.          
038100         05  W-IDRTLOP-G1-MAX    PIC  9(3)          VALUE ZERO.           
038200         05  W-IDKOLLI-G1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
038300         05  W-DAREGDAT-G1-MAX   PIC  9(8)          VALUE ZERO.           
038400         05  W-TIKLOCK-G1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
038500                                                                          
038600     03  W-WDA3FSEQ-MIN-X.                                                
038700         05  W-IDDC-FSEQ-MIN       PIC  X(2)          VALUE SPACE.        
038800         05  W-IDDISTR-FSEQ-MIN    PIC S9(5)   COMP-3 VALUE ZERO.         
038900         05  W-IDKUNDNR-FSEQ-MIN   PIC S9(7)   COMP-3 VALUE ZERO.         
039000         05  W-IDRAPPNR-FSEQ-MIN   PIC  9(7)          VALUE ZERO.         
039100                                                                          
039200     03  W-WDA3FSEQ-MAX-X.                                                
039300         05  W-IDDC-FSEQ-MAX       PIC  X(2)          VALUE SPACE.        
039400         05  W-IDDISTR-FSEQ-MAX    PIC S9(5)   COMP-3 VALUE ZERO.         
039500         05  W-IDKUNDNR-FSEQ-MAX   PIC S9(7)   COMP-3 VALUE ZERO.         
039600         05  W-IDRAPPNR-FSEQ-MAX   PIC  9(7)          VALUE ZERO.         
039700                                                                          
039800     03  W-IDLEVANM-X.                                                    
039900         05  W-IDDISTR-ANM       PIC S9(5)    VALUE ZERO  COMP-3.         
040000         05  W-IDKUNDNR-ANM      PIC S9(7)    VALUE ZERO  COMP-3.         
040100         05  W-IDRAPPNR-ANM      PIC  9(7)    VALUE ZERO.                 
040200                                                                          
040300     03  W-WDA3F1KY-MIN-X.                                                
040400         05  W-IDDC-F1-MIN      PIC  X(2)          VALUE SPACE.           
040500         05  W-IDDISTR-F1-MIN   PIC S9(5)   COMP-3 VALUE ZERO.            
040600         05  W-IDKUNDNR-F1-MIN  PIC S9(7)   COMP-3 VALUE ZERO.            
040700         05  W-IDRAPPNR-F1-MIN  PIC  X(7)          VALUE ZERO.            
040800         05  W-IDRT-F1-MIN      PIC  X(3)          VALUE SPACE.           
040900         05  W-IDRTLOP-F1-MIN   PIC  9(3)          VALUE ZERO.            
041000         05  W-IDKOLLI-F1-MIN   PIC S9(5)   COMP-3 VALUE ZERO.            
041100         05  W-DAREGDAT-F1-MIN  PIC  9(8)          VALUE ZERO.            
041200         05  W-TIKLOCK-F1-MIN   PIC S9(9)   COMP-3 VALUE ZERO.            
041300                                                                          
041400     03  W-WDA3F1KY-MAX-X.                                                
041500         05  W-IDDC-F1-MAX      PIC  X(2)          VALUE SPACE.           
041600         05  W-IDDISTR-F1-MAX   PIC S9(5)   COMP-3 VALUE ZERO.            
041700         05  W-IDKUNDNR-F1-MAX  PIC S9(7)   COMP-3 VALUE ZERO.            
041800         05  W-IDRAPPNR-F1-MAX  PIC  X(7)          VALUE ZERO.            
041900         05  W-IDRT-F1-MAX      PIC  X(3)          VALUE SPACE.           
042000         05  W-IDRTLOP-F1-MAX   PIC  9(3)          VALUE ZERO.            
042100         05  W-IDKOLLI-F1-MAX   PIC S9(5)   COMP-3 VALUE ZERO.            
042200         05  W-DAREGDAT-F1-MAX  PIC  9(8)          VALUE ZERO.            
042300         05  W-TIKLOCK-F1-MAX   PIC S9(9)   COMP-3 VALUE ZERO.            
042400                                                                          
042500     03  W-KDARBTYP-X.                                                    
042600         05  W-KDARBTYP         PIC  X(8)          VALUE SPACE.           
042700                                                                          
042800     03  W-IDPERSON-X.                                                    
042900         05  W-IDPERSON         PIC S9(3)   COMP-3 VALUE ZERO.            
043000                                                                          
043100     03  W-WDGX4107-X.                                                    
043200         05  W-IDHTYP-4107      PIC X(4)          VALUE '4107'.           
043300         05  FILLER             PIC X(26)         VALUE LOW-VALUE.        
043400                                                                          
043500     03  W-KDSEGKEY-X.                                                    
043600         05  W-KDSEGKEY         PIC  X(1)          VALUE '1'.             
043700                                                                          
043800     03  W-IDARTNR-X.                                                     
043900         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
044000                                                                          
044100     03  W-IDDC-B6-X.                                                     
044200         05 W-IDDC-B6                  PIC X(2).                          
044300                                                                          
044400     03  W-IDDC-K7-X.                                                     
044500         05  W-IDDC-K7           PIC  X(2)   VALUE SPACE.                 
044600                                                                          
044700                                                                          
044800                                                                          
044900                                                                          
045000     SKIP2                                                                
045100*    --- STATUS-KOD FRÅN IMS                                              
045200 01  STATUS-WS                   PIC XX.                                  
045300     88  STATUS-OK                           VALUE '  '.                  
045400     88  SEGMENT-FINNS                       VALUE '  '.                  
045500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
045600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
045700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
045800     88  TRANSKOD-FEL                        VALUE 'A1'.                  
045900     88  SECURITY-FEL                        VALUE 'A4'.                  
046000     SKIP2                                                                
046100 01  GODK-STATUSKODER.                                                    
046200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
046300     SKIP3                                                                
046400 01  SSA1                        PIC X(256).                              
046500 01  SSA2                        PIC X(64).                               
046600     EJECT                                                                
046700*    --- IMS FUNKTIONSKODER                                               
046800*01  -COPY W0003                                                          
046900     EJECT                                                                
047000*    ---  DLI INPUT-OUTPUT AREA                                           
047100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
047200     SKIP3                                                                
047300 01  DLI-IO-AREA1.                                                        
047400     03  IO-AREA1                 PIC X(200)  VALUE SPACE.                
047500     SKIP3                                                                
047600     03  WLRETA01 REDEFINES IO-AREA1.                                     
047700*        05  -COPY WDA301                                                 
047800     EJECT                                                                
047900     03  WLRETG01 REDEFINES IO-AREA1.                                     
048000*        05  -COPY WDA3F1                                                 
048100     EJECT                                                                
048200 01  DLI-IO-AREA2.                                                        
048300     03  IO-AREA2                 PIC X(300)  VALUE SPACE.                
048400     SKIP3                                                                
048500     03  WLRETA01 REDEFINES IO-AREA2.                                     
048600*        05  -COPY WDA201                                                 
048700     EJECT                                                                
048800     03  WLRETA01 REDEFINES IO-AREA2.                                     
048900*        05  -COPY WDA211                                                 
049000     EJECT                                                                
049100     03  WLRETH01 REDEFINES IO-AREA2.                                     
049200*        05  -COPY WDA3G1                                                 
049300     EJECT                                                                
049400     03  WL410711 REDEFINES IO-AREA2.                                     
049500*        05  -COPY WDGX4108                                               
049600     EJECT                                                                
049700                                                                          
049800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
049900 01  DLI-IO-WDK611.                                                       
050000*    03  -COPY WDK611                                                     
050100     SKIP2                                                                
050200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
050300 01  DLI-IO-WDK711.                                                       
050400*    03  -COPY WDK711                                                     
050500     SKIP2                                                                
050600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
050700 01   DLI-IO-AREA-B601.                                                   
050800*     03  -COPY WDB601                                                    
050900                                                                          
051000 LINKAGE SECTION.                                                         
051100                                                                          
051200*01  -COPY W0009   -PRE MSG-                                              
051300*01  -COPY W0009     -PRE ALT-                                            
051400     EJECT                                                                
051500*01  -COPY W0009   -PRE DISP-                                             
051600     EJECT                                                                
051700*01  -COPY W0008   -PRE USEA-                                             
051800     05  FILLER                  PIC X.                                   
051900     EJECT                                                                
052000*01  -COPY W0008  -PRE RETA1-                                             
052100     05  FILLER                  PIC X.                                   
052200     EJECT                                                                
052300*01  -COPY W0008  -PRE RETA2-                                             
052400     05  FILLER                  PIC X.                                   
052500                                                                          
052600     EJECT                                                                
052700*01  -COPY W0008  -PRE RETA3-                                             
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000*01  -COPY W0008  -PRE RETG-                                              
053100     05  FILLER                  PIC X.                                   
053200     EJECT                                                                
053300*01  -COPY W0008  -PRE RETH-                                              
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600*01  -COPY W0008  -PRE KREE-                                              
053700     05  FILLER                  PIC X.                                   
053800                                                                          
053900*01  -COPY W0008  -PRE RETA4-                                             
054000     05  FILLER                  PIC X.                                   
054100                                                                          
054200*01  -COPY W0008  -PRE RETA5-                                             
054300     05  FILLER                  PIC X.                                   
054400                                                                          
054500*01  -COPY W0008  -PRE 4107-                                              
054600     05  FILLER                  PIC X.                                   
054700                                                                          
054800 01  KOM-KOMA-PCB                PIC X.                                   
054900     EJECT                                                                
055000                                                                          
055100*01  -COPY W0008  -PRE ARTC-                                              
055200     05  FILLER                  PIC X.                                   
055300     EJECT                                                                
055400*01  -COPY W0008  -PRE ARTS-                                              
055500     05  FILLER                  PIC X.                                   
055600     EJECT                                                                
055700*01  -COPY W0008  -PRE WDB6-                                              
055800     05  FILLER                  PIC X.                                   
055900 PROCEDURE DIVISION  USING MSG-PCB   ALT-PCB   DISP-PCB                   
056000                           USEA-PCB  RETA1-PCB RETA2-PCB                  
056100                           RETA3-PCB RETG-PCB  RETH-PCB                   
056200                           KREE-PCB  RETA4-PCB RETA5-PCB                  
056300                           4107-PCB                                       
056400                           KOM-KOMA-PCB                                   
056500                           ARTC-PCB ARTS-PCB WDB6-PCB.                    
056600                                                                          
056700     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB   DISP-PCB                   
056800                           USEA-PCB  RETA1-PCB RETA2-PCB                  
056900                           RETA3-PCB RETG-PCB  RETH-PCB                   
057000                           KREE-PCB  RETA4-PCB RETA5-PCB                  
057100                           4107-PCB                                       
057200                           KOM-KOMA-PCB                                   
057300                           ARTC-PCB ARTS-PCB WDB6-PCB.                    
057400                                                                          
057500     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
057600     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
057700                                                                          
057800     PERFORM IMS-GET-MSG                                                  
057900     IF SEGMENT-FINNS                                                     
058000       PERFORM A-INIT                                                     
058100       PERFORM B-KOLLA-NYCKLAR                                            
058200       IF NYCKLAR-OK                                                      
058300         IF MFS-UPDATE                                                    
058400           PERFORM G-KOLLA-INPUT                                          
058500           IF INDATA-OK                                                   
058600             PERFORM H-UPPDATERA                                          
058700           END-IF                                                         
058800         ELSE                                                             
058900           IF MFS-FIRST                                                   
059000             PERFORM C-FOERSTA-SIDA                                       
059100           ELSE                                                           
059200             IF MFS-NEXT                                                  
059300               PERFORM D-NAESTA-SIDA                                      
059400             ELSE                                                         
059500               PERFORM E-SAMMA-SIDA                                       
059600             END-IF                                                       
059700           END-IF                                                         
059800         END-IF                                                           
059900         IF STARTA-ANNAN-BILD                                             
060000            CONTINUE                                                      
060100         ELSE                                                             
060200            IF INDATA-OK                                                  
060300               PERFORM F-LAES-VISA-INFO                                   
060400            END-IF                                                        
060500         END-IF                                                           
060600       END-IF                                                             
060700       IF STARTA-ANNAN-BILD                                               
060800          CONTINUE                                                        
060900       ELSE                                                               
061000          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73401 + 4                   
061100          PERFORM IMS-INSERT-MSG                                          
061200       END-IF                                                             
061300     END-IF                                                               
061400                                                                          
061500     MOVE ZERO TO RETURN-CODE                                             
061600     GOBACK                                                               
061700     .                                                                    
061800     EJECT                                                                
061900 A-INIT SECTION.                                                          
062000                                                                          
062100     IF MSG-DUBBLA-TRANSKODER                                             
062200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73401                 
062300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
062400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
062500     ELSE                                                                 
062600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I73401                  
062700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
062800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
062900     END-IF                                                               
063000                                                                          
063100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
063200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
063300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
063400                                                                          
063500     MOVE LOW-VALUE   TO MSG-AREA                                         
063600     MOVE 'W4O73401'  TO MFS-IDMOD                                        
063700     MOVE '4734' TO MOD-IDTRANS                                           
063800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
063900                                                                          
064000     IF EGEN-MID OR HELP-MID                                              
064100       CONTINUE                                                           
064200     ELSE                                                                 
064300       MOVE SPACE TO MFS-KDTRTYP                                          
064400       MOVE '7' TO MFS-IDPFK                                              
064500     END-IF                                                               
064600                                                                          
064700     MOVE LOW-VALUE      TO W-WDA3BSEQ-MIN-X                              
064800                            W-WDA3G1KY-MIN-X                              
064900                            W-WDA3FSEQ-MIN-X                              
065000                                                                          
065100     MOVE HIGH-VALUE     TO W-WDA3BSEQ-MAX-X                              
065200                            W-WDA3G1KY-MAX-X                              
065300                            W-WDA3FSEQ-MAX-X                              
065400                                                                          
065500     MOVE '4'            TO W-KDRETSTA-G1-MIN                             
065600     MOVE '5'            TO W-KDRETSTA-G1-MAX                             
065700     .                                                                    
065800     EJECT                                                                
065900 B-KOLLA-NYCKLAR SECTION.                                                 
066000                                                                          
066100     MOVE ALL '+'              TO MSGI-WMSGINIT                           
066200     MOVE '001'                TO MSGI-KDCALL                             
066300     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
066400     MOVE '4734'               TO MSGI-IDTRANS                            
066500     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
066600     IF EGEN-MID                                                          
066700        IF MID-IDANSV-IN NOT = ALL '+'                                    
066800          MOVE MID-IDANSV-IN(1:3)    TO MSGI-KDARBTYP                     
066900          MOVE MID-IDANSV-IN(4:3)    TO MSGI-IDPERSON                     
067000          MOVE SPACE                 TO MSGI-IDRT                         
067100          MOVE ZERO                  TO MSGI-IDRTLOP                      
067200                                        MSGI-IDKOLLI                      
067300                                        MSGI-IDDISTR                      
067400        ELSE                                                              
067500          IF MID-IDRT-IN NOT = ALL '+'                                    
067600            MOVE MID-IDRT-IN           TO MSGI-IDRT                       
067700            MOVE MID-IDRTLOP-IN        TO MSGI-IDRTLOP                    
067800            MOVE SPACE                 TO MSGI-KDARBTYP                   
067900            MOVE ZERO                  TO MSGI-IDPERSON                   
068000                                          MSGI-IDDISTR                    
068100            IF MID-IDKOLLI-IN NOT = ALL '+'                               
068200              MOVE MID-IDKOLLI-IN        TO MSGI-IDKOLLI                  
068300            ELSE                                                          
068400              MOVE ZERO                  TO MSGI-IDKOLLI                  
068500            END-IF                                                        
068600          ELSE                                                            
068700            IF MID-IDKOLLI-IN NOT = ALL '+'                               
068800              MOVE MID-IDKOLLI-IN        TO MSGI-IDKOLLI                  
068900            ELSE                                                          
069000              IF MID-IDDISTR-IN NOT = ALL '+'                             
069100                MOVE MID-IDDISTR-IN        TO MSGI-IDDISTR                
069200                MOVE SPACE                 TO MSGI-IDRT                   
069300                                              MSGI-KDARBTYP               
069400                MOVE ZERO                  TO MSGI-IDRTLOP                
069500                                              MSGI-IDPERSON               
069600                                              MSGI-IDKOLLI                
069700              END-IF                                                      
069800            END-IF                                                        
069900          END-IF                                                          
070000        END-IF                                                            
070100     END-IF                                                               
070200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
070300                                                                          
070400     IF MSGI-IDLAND-SPR = 'GB'                                            
070500       MOVE 'GB'                  TO MED-IDSKYLT                          
070600     ELSE                                                                 
070700       MOVE 'S '                  TO MED-IDSKYLT                          
070800     END-IF                                                               
070900                                                                          
071000     MOVE JA TO NYCKLAR-SW                                                
071100                                                                          
071200     MOVE MSGI-IDDC           TO W-IDDC                                   
071300                                 W-IDDC-BSEQ-MIN                          
071400                                 W-IDDC-BSEQ-MAX                          
071500                                 W-IDDC-FSEQ-MIN                          
071600                                 W-IDDC-FSEQ-MAX                          
071700                                 W-IDDC-G1-MIN                            
071800                                 W-IDDC-G1-MAX                            
071900                                 W-IDDC-F1-MIN                            
072000                                 W-IDDC-F1-MAX                            
072100                                 WS-IDDC                                  
072200     PERFORM BA-KOLLA-IDANSV                                              
072300     PERFORM BB-KOLLA-IDRETSND                                            
072400     PERFORM BC-KOLLA-IDKOLLI                                             
072500     PERFORM BD-KOLLA-IDDISTR                                             
072600     PERFORM BE-KOLLA-FLVISAAV                                            
072700                                                                          
072800     IF INGEN-SOEK                                                        
072900         MOVE NEJ                TO NYCKLAR-SW                            
073000     END-IF                                                               
073100                                                                          
073200     IF GODK-MID OR NYCKLAR-OK                                            
073300        MOVE MSGI-KDARBTYP        TO MOD-IDANSV-UT(1:3)                   
073400        MOVE MSGI-IDPERSON        TO MOD-IDANSV-UT(4:3)                   
073500        IF MOD-IDANSV-UT(4:3) = ZERO                                      
073600           MOVE SPACE             TO MOD-IDANSV-UT(4:3)                   
073700        END-IF                                                            
073800        MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                       
073900        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
074000        MOVE MSGI-IDRT            TO MOD-IDRT-UT                          
074100        MOVE MSGI-IDRTLOP         TO MOD-IDRTLOP-UT                       
074200        IF MOD-IDRTLOP-UT = ZERO                                          
074300           MOVE SPACE TO MOD-IDRTLOP-UT                                   
074400        END-IF                                                            
074500        MOVE MSGI-IDKOLLI         TO MOD-IDKOLLI-UT                       
074600        INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE            
074700     ELSE                                                                 
074800        MOVE MFS-RENSA-FAELT     TO MOD-IDANSV-UT                         
074900                                    MOD-IDRT-UT                           
075000                                    MOD-IDRTLOP-UT                        
075100                                    MOD-IDKOLLI-UT                        
075200                                    MOD-IDDISTR-UT                        
075300                                    MOD-FLVISAAV-UT                       
075400     END-IF                                                               
075500                                                                          
075600     IF NYCKLAR-FEL                                                       
075700       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
075800       CALL WMEDKONV USING MED-WMEDAREA                                   
075900       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
076000       PERFORM MFS-RENSA-FAELT-IN                                         
076100       PERFORM MFS-RENSA-FAELT-UT                                         
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500                                                                          
076600 BA-KOLLA-IDANSV     SECTION.                                             
076700                                                                          
076800     MOVE MFS-RENSA-FAELT       TO MOD-IDANSV-IN                          
076900                                                                          
077000     IF MID-IDANSV-IN           NOT = ALL '+'                             
077100       MOVE '7'                 TO MFS-IDPFK                              
077200       MOVE SPACE               TO MFS-KDTRTYP                            
077300     END-IF                                                               
077400                                                                          
077500     IF MSGI-KDARBTYP               NOT = SPACE                           
077600        IF MSGI-IDPERSON            NUMERIC                               
077700           MOVE '1'                 TO SW-SOEKNING                        
077800           MOVE MSGI-KDARBTYP(1:3)  TO  W-KDARBTYP                        
077900           MOVE MSGI-IDPERSON       TO  W-IDPERSON                        
078000        ELSE                                                              
078100           MOVE NEJ                 TO NYCKLAR-SW                         
078200        END-IF                                                            
078300     END-IF                                                               
078400                                                                          
078500     .                                                                    
078600     EJECT                                                                
078700 BB-KOLLA-IDRETSND   SECTION.                                             
078800                                                                          
078900     MOVE MFS-RENSA-FAELT      TO MOD-IDRT-IN                             
079000                                  MOD-IDRTLOP-IN                          
079100                                                                          
079200     IF MID-IDRT-IN            NOT = ALL '+'                              
079300       MOVE '7'                TO MFS-IDPFK                               
079400       MOVE SPACE              TO MFS-KDTRTYP                             
079500     END-IF                                                               
079600                                                                          
079700     IF MSGI-IDRT              NOT = SPACE                                
079800*       IF INGEN-SOEK                                                     
079900           IF MSGI-IDRTLOP      NUMERIC                                   
080000              MOVE '2'          TO SW-SOEKNING                            
080100              MOVE MSGI-IDRT    TO W-IDRT-BSEQ-MIN                        
080200                                   W-IDRT-BSEQ-MAX                        
080300              MOVE MSGI-IDRTLOP TO W-IDRTLOP-BSEQ-MIN                     
080400                                   W-IDRTLOP-BSEQ-MAX                     
080500           ELSE                                                           
080600              MOVE NEJ          TO NYCKLAR-SW                             
080700           END-IF                                                         
080800*       END-IF                                                            
080900     END-IF                                                               
081000                                                                          
081100     .                                                                    
081200     EJECT                                                                
081300                                                                          
081400 BC-KOLLA-IDKOLLI    SECTION.                                             
081500                                                                          
081600     MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-IN                          
081700                                                                          
081800     IF MID-IDKOLLI-IN         NOT = ALL '+'                              
081900       MOVE '7'                TO MFS-IDPFK                               
082000       MOVE SPACE              TO MFS-KDTRTYP                             
082100     END-IF                                                               
082200                                                                          
082300     IF MSGI-IDKOLLI              NUMERIC AND                             
082400        MSGI-IDKOLLI              >  ZERO                                 
082500        IF IDRETSND-SOEK                                                  
082600           MOVE MSGI-IDKOLLI      TO W-IDKOLLI-BSEQ-MIN                   
082700                                     W-IDKOLLI-BSEQ-MAX                   
082800        ELSE                                                              
082900           MOVE NEJ             TO NYCKLAR-SW                             
083000        END-IF                                                            
083100     END-IF                                                               
083200                                                                          
083300     .                                                                    
083400     EJECT                                                                
083500                                                                          
083600 BD-KOLLA-IDDISTR    SECTION.                                             
083700                                                                          
083800     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
083900                                                                          
084000     IF MID-IDDISTR-IN         NOT = ALL '+'                              
084100       MOVE '7'                TO MFS-IDPFK                               
084200       MOVE SPACE              TO MFS-KDTRTYP                             
084300     END-IF                                                               
084400                                                                          
084500     IF MSGI-IDDISTR            NUMERIC AND                               
084600        MSGI-IDDISTR            >  ZERO                                   
084700        IF INGEN-SOEK                                                     
084800           MOVE '3'             TO SW-SOEKNING                            
084900           MOVE MSGI-IDDISTR    TO W-IDDISTR-FSEQ-MIN                     
085000                                   W-IDDISTR-FSEQ-MAX                     
085100        END-IF                                                            
085200     END-IF                                                               
085300                                                                          
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085700 BE-KOLLA-FLVISAAV   SECTION.                                             
085800                                                                          
085900     MOVE MFS-RENSA-FAELT      TO MOD-FLVISAAV-IN                         
086000                                                                          
086100     IF MID-FLVISAAV-IN        NOT = ALL '+'                              
086200       MOVE '7'                TO MFS-IDPFK                               
086300       MOVE SPACE              TO MFS-KDTRTYP                             
086400       MOVE MID-FLVISAAV-IN    TO WS-FLVISAAV                             
086500     ELSE                                                                 
086600       MOVE MID-FLVISAAV-UT    TO WS-FLVISAAV                             
086700     END-IF                                                               
086800                                                                          
086900     IF WS-FLVISAAV             =  JA                                     
087000        MOVE JA                 TO SW-AVVIKELSER                          
087100                                   MOD-FLVISAAV-UT                        
087200     ELSE                                                                 
087300        MOVE NEJ                TO SW-AVVIKELSER                          
087400                                   MOD-FLVISAAV-UT                        
087500     END-IF                                                               
087600                                                                          
087700     .                                                                    
087800     EJECT                                                                
087900 C-FOERSTA-SIDA SECTION.                                                  
088000                                                                          
088100     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
088200     CALL WMEDKONV USING MED-WMEDAREA                                     
088300     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
088400                                                                          
088500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
088600     PERFORM MFS-RENSA-FAELT-IN                                           
088700     .                                                                    
088800     EJECT                                                                
088900 D-NAESTA-SIDA SECTION.                                                   
089000                                                                          
089100     MOVE MSGI-SPAR-AREA       TO W-MINKEY                                
089200     IF W-MINKEY-IDTRANS = '4734' AND                                     
089300       (W-MINKEY-IDSOEK = '1' OR '2' OR '3')                              
089400        IF W-MINKEY-IDSOEK = '1'                                          
089500          MOVE W-MINKEY-SSA-NEXT   TO W-WDA3BSEQ-MIN-X                    
089600        END-IF                                                            
089700                                                                          
089800        IF W-MINKEY-IDSOEK = '2'                                          
089900          MOVE W-MINKEY-SSA-NEXT   TO W-WDA3FSEQ-MIN-X                    
090000        END-IF                                                            
090100                                                                          
090200        IF W-MINKEY-IDSOEK = '3'                                          
090300          MOVE W-MINKEY-SSA-NEXT    TO W-WDA3G1KY-MIN-X                   
090400        END-IF                                                            
090500                                                                          
090600     ELSE                                                                 
090700       MOVE LOW-VALUE              TO W-WDA3BSEQ-MIN-X                    
090800                                      W-WDA3G1KY-MIN-X                    
090900                                      W-WDA3FSEQ-MIN-X                    
091000       MOVE MSGI-IDDC              TO W-IDDC-BSEQ-MIN                     
091100                                      W-IDDC-FSEQ-MIN                     
091200                                      W-IDDC-G1-MIN                       
091300                                      WS-IDDC                             
091400       PERFORM MFS-RENSA-FAELT-IN                                         
091500     END-IF                                                               
091600     .                                                                    
091700     EJECT                                                                
091800 E-SAMMA-SIDA SECTION.                                                    
091900                                                                          
092000     MOVE MSGI-SPAR-AREA       TO W-MINKEY                                
092100     IF W-MINKEY-IDTRANS = '4734' AND                                     
092200       (W-MINKEY-IDSOEK  = '1' OR '2' OR '3')                             
092300       IF W-MINKEY-IDSOEK = '1'                                           
092400         MOVE W-MINKEY-SSA-ENTER  TO W-WDA3BSEQ-MIN-X                     
092500       END-IF                                                             
092600                                                                          
092700       IF W-MINKEY-IDSOEK = '2'                                           
092800         MOVE W-MINKEY-SSA-ENTER  TO W-WDA3FSEQ-MIN-X                     
092900       END-IF                                                             
093000                                                                          
093100       IF W-MINKEY-IDSOEK = '3'                                           
093200         MOVE W-MINKEY-SSA-ENTER   TO W-WDA3G1KY-MIN-X                    
093300       END-IF                                                             
093400                                                                          
093500       IF MID-INPUT                = ALL '+'                              
093600          PERFORM MFS-RENSA-FAELT-IN                                      
093700       ELSE                                                               
093800          MOVE +1               TO INDX                                   
093900          PERFORM UNTIL INDX    >  MAX-INDX                               
094000             IF MID-KDCMD(INDX) NUMERIC                                   
094100                PERFORM EA-STARTA-ANNAN-BILD                              
094200                MOVE JA         TO SW-STARTA-ANNAN-BILD                   
094300                MOVE MAX-INDX   TO INDX                                   
094400             END-IF                                                       
094500             ADD +1             TO INDX                                   
094600          END-PERFORM                                                     
094700          IF STARTA-ANNAN-BILD                                            
094800             CONTINUE                                                     
094900          ELSE                                                            
095000             MOVE INF-PRESS-PF11    TO MED-IDMFSINF                       
095100             CALL WMEDKONV USING MED-WMEDAREA                             
095200             MOVE MED-MFSINF        TO MOD-TEMFSFEL                       
095300             PERFORM EB-MID-INDATA-TILL-MOD                               
095400          END-IF                                                          
095500       END-IF                                                             
095600     ELSE                                                                 
095700       MOVE LOW-VALUE              TO W-WDA3BSEQ-MIN-X                    
095800                                      W-WDA3G1KY-MIN-X                    
095900                                      W-WDA3FSEQ-MIN-X                    
096000       MOVE MSGI-IDDC              TO W-IDDC-BSEQ-MIN                     
096100                                      W-IDDC-FSEQ-MIN                     
096200                                      W-IDDC-G1-MIN                       
096300                                      WS-IDDC                             
096400       PERFORM MFS-RENSA-FAELT-IN                                         
096500     END-IF                                                               
096600     .                                                                    
096700     EJECT                                                                
096800 EA-STARTA-ANNAN-BILD  SECTION.                                           
096900                                                                          
097000     MOVE MID-IDRT(INDX)         TO MSGI-IDRT                             
097100     MOVE MID-IDRTLOP(INDX)      TO MSGI-IDRTLOP                          
097200     MOVE MID-IDKOLLI(INDX)      TO MSGI-IDKOLLI                          
097300     INSPECT MSGI-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
097400     MOVE '001'                  TO MSGI-KDCALL                           
097500     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
097600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
097700                                                                          
097800     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
097900     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
098000     MOVE MID-KDCMD(INDX) (1:1)  TO W-HOPP-IDTRANS-2                      
098100     MOVE MID-KDCMD(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                    
098200     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
098300     MOVE '4734'                 TO P-TO-P-IDTRANS                        
098400     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
098500                                                                          
098600     PERFORM S01-INSERT-ALTMSG                                            
098700     .                                                                    
098800     EJECT                                                                
098900                                                                          
099000 EB-MID-INDATA-TILL-MOD SECTION.                                          
099100                                                                          
099200     IF  MID-IDANSTNR             =  ALL '+'                              
099300       MOVE MFS-RENSA-FAELT       TO MOD-IDANSTNR                         
099400     ELSE                                                                 
099500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSTNR-ATTR                    
099600       MOVE MID-IDANSTNR          TO MOD-IDANSTNR                         
099700     END-IF                                                               
099800                                                                          
099900     MOVE +1                     TO INDX                                  
100000                                                                          
100100     PERFORM UNTIL (INDX > MAX-INDX)                                      
100200                                                                          
100300       IF  MID-KDCMD  (INDX)     = ALL '+'                                
100400         MOVE MFS-RENSA-FAELT    TO MOD-KDCMD (INDX)                      
100500       ELSE                                                               
100600         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCMD-ATTR(INDX)              
100700         MOVE MID-KDCMD     (INDX)   TO MOD-KDCMD     (INDX)              
100800       END-IF                                                             
100900                                                                          
101000       ADD +1                    TO INDX                                  
101100     END-PERFORM                                                          
101200                                                                          
101300     .                                                                    
101400     EJECT                                                                
101500                                                                          
101600 F-LAES-VISA-INFO SECTION.                                                
101700                                                                          
101800     PERFORM S05-BERAKNA-DATUM                                            
101900                                                                          
102000     MOVE ZERO                  TO W-SPAR-IDRT                            
102100                                   W-SPAR-IDRTLOP                         
102200                                   W-SPAR-IDKOLLI                         
102300     PERFORM FA-LAES-FOERSTA-POSTEN                                       
102400                                                                          
102500     IF SEGMENT-SAKNAS                                                    
102600        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
102700        CALL WMEDKONV USING MED-WMEDAREA                                  
102800        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
102900        PERFORM MFS-RENSA-FAELT-UT                                        
103000     ELSE                                                                 
103100       MOVE +1                  TO INDX                                   
103200       PERFORM FB-FIXA-ENTER-KEY                                          
103300                                                                          
103400       PERFORM UNTIL INDX       > MAX-INDX                                
103500         IF SEGMENT-FINNS                                                 
103600                                                                          
103700           PERFORM FC-REDIGERA-MOD                                        
103800*   FÖR ATT INTE LÄSA VIDARE OM IDKOLLI ÄR IFYLLT                         
103900           IF IDRETSND-SOEK AND MSGI-IDKOLLI NUMERIC AND                  
104000                                MSGI-IDKOLLI > ZERO                       
104100              MOVE 'GE'         TO STATUS-WS                              
104200           ELSE                                                           
104300*   FÖR ATT LÄSA NÄSTA KOLLI                                              
104400              MOVE RET-IDKOLLI  TO W-IDKOLLI-BSEQ-MIN                     
104500                                                                          
104600              PERFORM FD-LAES-NAESTA-POST                                 
104700           END-IF                                                         
104800         ELSE                                                             
104900           MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                  
105000           MOVE MFS-FORMATETS-ATTR TO MOD-IDSNDNR-ATTR(INDX)              
105100                                      MOD-IDKOLLI-ATTR(INDX)              
105200                                      MOD-FLFARLIG-ATTR(INDX)             
105300                                      MOD-TIRETANK-ATTR(INDX)             
105400                                      MOD-ADINLOMR-ATTR(INDX)             
105500                                      MOD-BESTATUS-ATTR(INDX)             
105600           MOVE MFS-RENSA-FAELT  TO MOD-IDRT         (INDX)               
105700                                    MOD-IDRTLOP      (INDX)               
105800                                    MOD-KDCMD        (INDX)               
105900                                    MOD-IDKOLLI      (INDX)               
106000                                    MOD-FLFARLIG     (INDX)               
106100                                    MOD-TIRETANK     (INDX)               
106200                                    MOD-ADINLOMR      (INDX)              
106300                                    MOD-BESTATUS      (INDX)              
106400           ADD 1                 TO INDX                                  
106500         END-IF                                                           
106600       END-PERFORM                                                        
106700                                                                          
106800       PERFORM FE-FIXA-NEXT-KEY                                           
106900                                                                          
107000       MOVE '002'                     TO MSGI-KDCALL                      
107100       MOVE '4734'                    TO MSGI-IDTRANS                     
107200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
107300                                                                          
107400     END-IF                                                               
107500     .                                                                    
107600     EJECT                                                                
107700                                                                          
107800 FA-LAES-FOERSTA-POSTEN    SECTION.                                       
107900                                                                          
108000     EVALUATE TRUE                                                        
108100                                                                          
108200       WHEN IDANSV-SOEK                                                   
108300        PERFORM IMS-GU-RETH-WLRETH01                                      
108400        IF SEGMENT-FINNS                                                  
108500           MOVE SEQG-DAREGDAT      TO W-DAREGDAT                          
108600           MOVE SEQG-TIKLOCK       TO W-TIKLOCK                           
108700                                                                          
108800           PERFORM IMS-GU-RETA-WLRETA01                                   
108900        END-IF                                                            
109000                                                                          
109100       WHEN IDRETSND-SOEK                                                 
109200        PERFORM IMS-GHU-SEQB-WLRETA01                                     
109300                                                                          
109400       WHEN IDDISTR-SOEK                                                  
109500        PERFORM IMS-GU-SEQF-WLRETA01                                      
109600        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                   
109700                      RET-IDKOLLI        > ZERO                           
109800           PERFORM IMS-GN-SEQF-WLRETA01                                   
109900        END-PERFORM                                                       
110000                                                                          
110100     END-EVALUATE                                                         
110200     .                                                                    
110300     EJECT                                                                
110400                                                                          
110500 FB-FIXA-ENTER-KEY        SECTION.                                        
110600                                                                          
110700     IF SEGMENT-FINNS                                                     
110800        EVALUATE TRUE                                                     
110900                                                                          
111000          WHEN IDANSV-SOEK                                                
111100           MOVE SEQG-IDDC           TO W-MINKEYG1-IDDC                    
111200           MOVE SEQG-KDRETSTA       TO W-MINKEYG1-KDRETSTA                
111300           MOVE SEQG-KDARBTYP       TO W-MINKEYG1-KDARBTYP                
111400           MOVE SEQG-IDPERSON       TO W-MINKEYG1-IDPERSON                
111500           MOVE SEQG-DARETANK       TO W-MINKEYG1-DARETANK                
111600           MOVE SEQG-IDRT           TO W-MINKEYG1-IDRT                    
111700           MOVE SEQG-IDRTLOP        TO W-MINKEYG1-IDRTLOP                 
111800           MOVE SEQG-IDKOLLI        TO W-MINKEYG1-IDKOLLI                 
111900           MOVE SEQG-DAREGDAT       TO W-MINKEYG1-DAREGDAT                
112000           MOVE SEQG-TIKLOCK        TO W-MINKEYG1-TIKLOCK                 
112100           MOVE '4734'              TO W-MINKEY-IDTRANS                   
112200           MOVE '3'                 TO W-MINKEY-IDSOEK                    
112300           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
112400                                                                          
112500          WHEN IDRETSND-SOEK                                              
112600           MOVE RET-IDDC            TO W-MINKEYB-IDDC                     
112700           MOVE RET-IDRT            TO W-MINKEYB-IDRT                     
112800           MOVE RET-IDRTLOP         TO W-MINKEYB-IDRTLOP                  
112900           MOVE RET-IDKOLLI         TO W-MINKEYB-IDKOLLI                  
113000           MOVE '4734'              TO W-MINKEY-IDTRANS                   
113100           MOVE '1'                 TO W-MINKEY-IDSOEK                    
113200           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
113300                                                                          
113400          WHEN IDDISTR-SOEK                                               
113500           MOVE RET-IDDC            TO W-MINKEYF-IDDC                     
113600           MOVE RET-IDDISTR         TO W-MINKEYF-IDDISTR                  
113700           MOVE RET-IDKUNDNR        TO W-MINKEYF-IDKUNDNR                 
113800           MOVE RET-IDRAPPNR        TO W-MINKEYF-IDRAPPNR                 
113900           MOVE '4734'              TO W-MINKEY-IDTRANS                   
114000           MOVE '2'                 TO W-MINKEY-IDSOEK                    
114100           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
114200                                                                          
114300        END-EVALUATE                                                      
114400     ELSE                                                                 
114500        EVALUATE TRUE                                                     
114600                                                                          
114700          WHEN IDANSV-SOEK                                                
114800           MOVE MSGI-IDDC           TO W-MINKEYG1-IDDC                    
114900           MOVE W-SND-MOT           TO W-MINKEYG1-KDRETSTA                
115000           MOVE MSGI-KDARBTYP       TO W-MINKEYG1-KDARBTYP                
115100           MOVE MSGI-IDPERSON       TO W-MINKEYG1-IDPERSON                
115200           MOVE SPACE               TO W-MINKEYG1-IDRT                    
115300           MOVE ZERO                TO W-MINKEYG1-DARETANK                
115400                                       W-MINKEYG1-IDRTLOP                 
115500                                       W-MINKEYG1-IDKOLLI                 
115600                                       W-MINKEYG1-DAREGDAT                
115700                                       W-MINKEYG1-TIKLOCK                 
115800           MOVE '4734'              TO W-MINKEY-IDTRANS                   
115900           MOVE '3'                 TO W-MINKEY-IDSOEK                    
116000           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
116100                                                                          
116200          WHEN IDRETSND-SOEK                                              
116300           MOVE MSGI-IDDC           TO W-MINKEYB-IDDC                     
116400           MOVE ZERO                TO W-MINKEYB-IDRT                     
116500                                       W-MINKEYB-IDRTLOP                  
116600                                       W-MINKEYB-IDKOLLI                  
116700           MOVE '4734'              TO W-MINKEY-IDTRANS                   
116800           MOVE '1'                 TO W-MINKEY-IDSOEK                    
116900           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
117000                                                                          
117100          WHEN IDDISTR-SOEK                                               
117200           MOVE MSGI-IDDC           TO W-MINKEYF-IDDC                     
117300           MOVE MSGI-IDDISTR        TO W-MINKEYF-IDDISTR                  
117400           MOVE ZERO                TO W-MINKEYF-IDKUNDNR                 
117500                                       W-MINKEYF-IDRAPPNR                 
117600           MOVE '4734'              TO W-MINKEY-IDTRANS                   
117700           MOVE '2'                 TO W-MINKEY-IDSOEK                    
117800           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
117900                                                                          
118000        END-EVALUATE                                                      
118100     END-IF                                                               
118200                                                                          
118300     .                                                                    
118400     EJECT                                                                
118500                                                                          
118600 FC-REDIGERA-MOD          SECTION.                                        
118700                                                                          
118800     IF RET-KDRETSTA < 6                                                  
118900       IF (VISA-AVVIKELSER    AND (RET-KDKOLSTA = W-KLI-SAK OR            
119000                                   RET-KDKOLSTA = W-KLI-AVV))  OR         
119100          (VISA-EJ-AVVIKELSER AND (RET-KDKOLSTA = W-KLI-LOSS OR           
119200                                   RET-KDKOLSTA = W-KLI-MOT))             
119300                                                                          
119400          MOVE RET-IDRT        TO MOD-IDRT        (INDX)                  
119500                                  W-SPAR-IDRT                             
119600          MOVE RET-IDRTLOP     TO MOD-IDRTLOP     (INDX)                  
119700                                  W-SPAR-IDRTLOP                          
119800          MOVE RET-IDKOLLI     TO MOD-IDKOLLI     (INDX)                  
119900                                  W-SPAR-IDKOLLI                          
120000          IF MSGI-IDLAND-SPR = 'GB '                                      
120100            IF RET-FLFARLIG = JA                                          
120200              MOVE YES           TO MOD-FLFARLIG    (INDX)                
120300            ELSE                                                          
120400              MOVE NEJ           TO MOD-FLFARLIG    (INDX)                
120500            END-IF                                                        
120600          ELSE                                                            
120700            MOVE RET-FLFARLIG    TO MOD-FLFARLIG    (INDX)                
120800          END-IF                                                          
120900          MOVE RET-DARETANK (3:6) TO MOD-TIRETANK    (INDX)               
121000          MOVE RET-ADINLOMR    TO MOD-ADINLOMR    (INDX)                  
121100                                                                          
121200          PERFORM FCA-REDIGERA-BESTATUS                                   
121300                                                                          
121400          IF VISA-AVVIKELSER                                              
121500             IF RET-DARETANK < W-TIAAAAMMDD-KLIAVV                        
121600                PERFORM MFS-LYS-UPP-FAELT                                 
121700             ELSE                                                         
121800                PERFORM MFS-FORM-ATTR-UTRAD                               
121900             END-IF                                                       
122000          ELSE                                                            
122100             IF RET-DARETANK < W-TIAAAAMMDD-KLIARB                        
122200                PERFORM MFS-LYS-UPP-FAELT                                 
122300             ELSE                                                         
122400                PERFORM MFS-FORM-ATTR-UTRAD                               
122500             END-IF                                                       
122600          END-IF                                                          
122700          ADD 1                TO INDX                                    
122800       END-IF                                                             
122900     END-IF                                                               
123000                                                                          
123100     .                                                                    
123200     EJECT                                                                
123300                                                                          
123400 FCA-REDIGERA-BESTATUS     SECTION.                                       
123500                                                                          
123600     EVALUATE RET-KDKOLSTA                                                
123700                                                                          
123800        WHEN 4                                                            
123900         MOVE 'LOSS'                TO MOD-BESTATUS (INDX)                
124000                                                                          
124100        WHEN 5                                                            
124200         IF MSGI-IDLAND-SPR = 'GB '                                       
124300           MOVE 'REC '                TO MOD-BESTATUS (INDX)              
124400         ELSE                                                             
124500           MOVE 'MOT '                TO MOD-BESTATUS (INDX)              
124600         END-IF                                                           
124700                                                                          
124800        WHEN 6                                                            
124900         IF MSGI-IDLAND-SPR = 'GB '                                       
125000           MOVE 'MISS'                TO MOD-BESTATUS (INDX)              
125100         ELSE                                                             
125200           MOVE 'SAK '                TO MOD-BESTATUS (INDX)              
125300         END-IF                                                           
125400                                                                          
125500        WHEN 7                                                            
125600         IF MSGI-IDLAND-SPR = 'GB '                                       
125700           MOVE 'DEV '                TO MOD-BESTATUS (INDX)              
125800         ELSE                                                             
125900           MOVE 'AVV '                TO MOD-BESTATUS (INDX)              
126000         END-IF                                                           
126100                                                                          
126200     END-EVALUATE                                                         
126300     .                                                                    
126400     EJECT                                                                
126500                                                                          
126600 FD-LAES-NAESTA-POST      SECTION.                                        
126700                                                                          
126800     EVALUATE TRUE                                                        
126900                                                                          
127000       WHEN IDANSV-SOEK                                                   
127100        PERFORM IMS-GN-RETH-WLRETH01                                      
127200        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
127300                                     OR SEQG-IDKOLLI = ZERO OR            
127400                      (SEQG-IDRT    NOT = W-SPAR-IDRT   )   OR            
127500                      (SEQG-IDRTLOP NOT = W-SPAR-IDRTLOP)   OR            
127600                      (SEQG-IDKOLLI NOT = W-SPAR-IDKOLLI)                 
127700          PERFORM IMS-GN-RETH-WLRETH01                                    
127800        END-PERFORM                                                       
127900        IF SEGMENT-FINNS                                                  
128000           MOVE SEQG-DAREGDAT      TO W-DAREGDAT                          
128100           MOVE SEQG-TIKLOCK       TO W-TIKLOCK                           
128200                                                                          
128300           PERFORM IMS-GU-RETA-WLRETA01                                   
128400        END-IF                                                            
128500                                                                          
128600       WHEN IDRETSND-SOEK                                                 
128700        PERFORM IMS-GHN-SEQB-WLRETA01                                     
128800        PERFORM UNTIL SEGMENT-SAKNAS OR RET-IDKOLLI = ZERO OR             
128900                      (RET-IDRT    NOT = W-SPAR-IDRT   )   OR             
129000                      (RET-IDRTLOP NOT = W-SPAR-IDRTLOP)   OR             
129100                      (RET-IDKOLLI NOT = W-SPAR-IDKOLLI)                  
129200            PERFORM IMS-GHN-SEQB-WLRETA01                                 
129300        END-PERFORM                                                       
129400                                                                          
129500       WHEN IDDISTR-SOEK                                                  
129600        PERFORM IMS-GN-SEQF-WLRETA01                                      
129700        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
129800                                     OR RET-IDKOLLI = ZERO OR             
129900                      (RET-IDRT    NOT = W-SPAR-IDRT   ) OR               
130000                      (RET-IDRTLOP NOT = W-SPAR-IDRTLOP) OR               
130100                      (RET-IDKOLLI NOT = W-SPAR-IDKOLLI)                  
130200            PERFORM IMS-GN-SEQF-WLRETA01                                  
130300        END-PERFORM                                                       
130400                                                                          
130500     END-EVALUATE                                                         
130600     .                                                                    
130700     EJECT                                                                
130800 FE-FIXA-NEXT-KEY        SECTION.                                         
130900                                                                          
131000     IF SEGMENT-FINNS                                                     
131100        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
131200        CALL WMEDKONV USING MED-WMEDAREA                                  
131300        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
131400                                                                          
131500        EVALUATE TRUE                                                     
131600                                                                          
131700          WHEN IDANSV-SOEK                                                
131800           MOVE SEQG-IDDC           TO W-MINKEYG1-IDDC-NEXT               
131900           MOVE SEQG-KDRETSTA       TO W-MINKEYG1-KDRETSTA-NEXT           
132000           MOVE SEQG-KDARBTYP       TO W-MINKEYG1-KDARBTYP-NEXT           
132100           MOVE SEQG-IDPERSON       TO W-MINKEYG1-IDPERSON-NEXT           
132200           MOVE SEQG-DARETANK       TO W-MINKEYG1-DARETANK-NEXT           
132300           MOVE SEQG-IDRT           TO W-MINKEYG1-IDRT-NEXT               
132400           MOVE SEQG-IDRTLOP        TO W-MINKEYG1-IDRTLOP-NEXT            
132500           MOVE SEQG-IDKOLLI        TO W-MINKEYG1-IDKOLLI-NEXT            
132600           MOVE SEQG-DAREGDAT       TO W-MINKEYG1-DAREGDAT-NEXT           
132700           MOVE SEQG-TIKLOCK        TO W-MINKEYG1-TIKLOCK-NEXT            
132800           MOVE '4734'              TO W-MINKEY-IDTRANS                   
132900           MOVE '3'                 TO W-MINKEY-IDSOEK                    
133000           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
133100                                                                          
133200          WHEN IDRETSND-SOEK                                              
133300           MOVE RET-IDDC            TO W-MINKEYB-IDDC-NEXT                
133400           MOVE RET-IDRT            TO W-MINKEYB-IDRT-NEXT                
133500           MOVE RET-IDRTLOP         TO W-MINKEYB-IDRTLOP-NEXT             
133600           MOVE RET-IDKOLLI         TO W-MINKEYB-IDKOLLI-NEXT             
133700           MOVE '4734'              TO W-MINKEY-IDTRANS                   
133800           MOVE '1'                 TO W-MINKEY-IDSOEK                    
133900           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
134000                                                                          
134100          WHEN IDDISTR-SOEK                                               
134200           MOVE RET-IDDC            TO W-MINKEYF-IDDC-NEXT                
134300           MOVE RET-IDDISTR         TO W-MINKEYF-IDDISTR-NEXT             
134400           MOVE RET-IDKUNDNR        TO W-MINKEYF-IDKUNDNR-NEXT            
134500           MOVE RET-IDRAPPNR        TO W-MINKEYF-IDRAPPNR-NEXT            
134600           MOVE '4734'              TO W-MINKEY-IDTRANS                   
134700           MOVE '2'                 TO W-MINKEY-IDSOEK                    
134800           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
134900                                                                          
135000        END-EVALUATE                                                      
135100     ELSE                                                                 
135200        EVALUATE TRUE                                                     
135300                                                                          
135400          WHEN IDANSV-SOEK                                                
135500           MOVE MSGI-IDDC           TO W-MINKEYG1-IDDC-NEXT               
135600           MOVE W-SND-MOT           TO W-MINKEYG1-KDRETSTA-NEXT           
135700           MOVE MSGI-KDARBTYP       TO W-MINKEYG1-KDARBTYP-NEXT           
135800           MOVE MSGI-IDPERSON       TO W-MINKEYG1-IDPERSON-NEXT           
135900           MOVE SPACE               TO W-MINKEYG1-IDRT-NEXT               
136000           MOVE ZERO                TO W-MINKEYG1-IDRTLOP-NEXT            
136100                                       W-MINKEYG1-DARETANK-NEXT           
136200                                       W-MINKEYG1-IDKOLLI-NEXT            
136300                                       W-MINKEYG1-DAREGDAT-NEXT           
136400                                       W-MINKEYG1-TIKLOCK-NEXT            
136500           MOVE '4734'              TO W-MINKEY-IDTRANS                   
136600           MOVE '3'                 TO W-MINKEY-IDSOEK                    
136700           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
136800                                                                          
136900          WHEN IDRETSND-SOEK                                              
137000           MOVE MSGI-IDDC           TO W-MINKEYB-IDDC-NEXT                
137100           MOVE ZERO                TO W-MINKEYB-IDRT-NEXT                
137200                                       W-MINKEYB-IDRTLOP-NEXT             
137300                                       W-MINKEYB-IDKOLLI-NEXT             
137400           MOVE '4734'              TO W-MINKEY-IDTRANS                   
137500           MOVE '1'                 TO W-MINKEY-IDSOEK                    
137600           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
137700                                                                          
137800          WHEN IDDISTR-SOEK                                               
137900           MOVE MSGI-IDDC           TO W-MINKEYF-IDDC-NEXT                
138000           MOVE MSGI-IDDISTR        TO W-MINKEYF-IDDISTR-NEXT             
138100           MOVE ZERO                TO W-MINKEYF-IDKUNDNR-NEXT            
138200                                       W-MINKEYF-IDRAPPNR-NEXT            
138300           MOVE '4734'              TO W-MINKEY-IDTRANS                   
138400           MOVE '2'                 TO W-MINKEY-IDSOEK                    
138500           MOVE W-MINKEY            TO MSGI-SPAR-AREA                     
138600                                                                          
138700        END-EVALUATE                                                      
138800     END-IF                                                               
138900                                                                          
139000     .                                                                    
139100     EJECT                                                                
139200                                                                          
139300 G-KOLLA-INPUT SECTION.                                                   
139400                                                                          
139500     MOVE JA               TO INDATA-SW                                   
139600                                                                          
139700     PERFORM GA-FORMELL-KONTROLL                                          
139800     IF INDATA-OK                                                         
139900        PERFORM GB-LOGISK-KONTROLL                                        
140000     END-IF                                                               
140100                                                                          
140200     IF INDATA-FEL                                                        
140300        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
140400        CALL WMEDKONV USING MED-WMEDAREA                                  
140500        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
140600        PERFORM MFS-ROER-EJ-FAELT-UT                                      
140700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
140800     ELSE                                                                 
140900        PERFORM GC-EV-PLATS-KONTROLL                                      
141000                                                                          
141100        IF INDATA-FEL                                                     
141200           CALL WMEDKONV USING MED-WMEDAREA                               
141300           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
141400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
141500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
141600        END-IF                                                            
141700     END-IF                                                               
141800                                                                          
141900     .                                                                    
142000     EJECT                                                                
142100                                                                          
142200 GA-FORMELL-KONTROLL SECTION.                                             
142300                                                                          
142400     IF MID-INPUT                = ALL '+'                                
142500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
142600       CALL WMEDKONV USING MED-WMEDAREA                                   
142700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
142800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
142900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
143000       MOVE NEJ                  TO INDATA-SW                             
143100     ELSE                                                                 
143200       PERFORM GAA-KOLLA-IDANSTNR                                         
143300       PERFORM GAB-KOLLA-KDCMD                                            
143400     END-IF                                                               
143500                                                                          
143600     .                                                                    
143700     EJECT                                                                
143800                                                                          
143900 GAA-KOLLA-IDANSTNR   SECTION.                                            
144000                                                                          
144100                                                                          
144200     IF MID-IDANSTNR                 NOT = ALL '+'                        
144300        IF MID-IDANSTNR              NUMERIC                              
144400           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                 
144500           MOVE MID-IDANSTNR         TO W-IDANSTNR                        
144600        ELSE                                                              
144700           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-ATTR                 
144800           MOVE NEJ                  TO INDATA-SW                         
144900        END-IF                                                            
145000     ELSE                                                                 
145100       IF CDC-SE                                                          
145200         MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANSTNR-ATTR                 
145300         MOVE NEJ                    TO INDATA-SW                         
145400       END-IF                                                             
145500     END-IF                                                               
145600                                                                          
145700     .                                                                    
145800     EJECT                                                                
145900 GAB-KOLLA-KDCMD      SECTION.                                            
146000                                                                          
146100     MOVE JA                TO SW-FOERSTA-VALDA                           
146200     MOVE NEJ               TO SW-KOLLI-VALT                              
146300                                                                          
146400     MOVE +1                TO INDX                                       
146500                                                                          
146600     PERFORM UNTIL INDX                 >  MAX-INDX                       
146700        IF MID-KDCMD(INDX)              NOT = ALL '+'                     
146800                                                                          
146900           IF MID-KDCMD(INDX)           = W-INLAEGGNING OR                
147000                                          W-BINNED      OR                
147100                                          W-VAELJKOLLI  OR                
147200                                          W-SELECTCASE  OR                
147300                                          W-OK                            
147400                                                                          
147500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)            
147600             IF FOERSTA-VALDA-KOLLI                                       
147700                MOVE NEJ               TO SW-FOERSTA-VALDA                
147800                IF MID-KDCMD(INDX)     =  W-VAELJKOLLI OR                 
147900                                          W-SELECTCASE                    
148000                   MOVE JA             TO SW-KOLLI-VALT                   
148100                END-IF                                                    
148200                IF MID-KDCMD(INDX)     =  W-INLAEGGNING OR                
148300                                          W-BINNED                        
148400                   MOVE JA             TO SW-KOLLI-INL                    
148500                END-IF                                                    
148600                IF MID-KDCMD(INDX)     =  W-OK                            
148700                   MOVE JA             TO SW-KOLLI-OK                     
148800                END-IF                                                    
148900             ELSE                                                         
149000                IF MID-KDCMD(INDX)     =  W-VAELJKOLLI OR                 
149100                                          W-SELECTCASE OR                 
149200                                          KOLLI-VALT                      
149300                   MOVE MFS-ALFA-FAELT-FEL                                
149400                                       TO MOD-KDCMD-ATTR(INDX)            
149500                   MOVE NEJ            TO INDATA-SW                       
149600                END-IF                                                    
149700                IF MID-KDCMD(INDX)     =  W-INLAEGGNING OR                
149800                                          W-BINNED                        
149900                   IF KOLLI-OK                                            
150000                     MOVE MFS-ALFA-FAELT-FEL                              
150100                                         TO MOD-KDCMD-ATTR(INDX)          
150200                     MOVE NEJ            TO INDATA-SW                     
150300                   END-IF                                                 
150400                END-IF                                                    
150500                IF MID-KDCMD(INDX)     =  W-INLAEGGNING OR                
150600                                          W-BINNED                        
150700                   IF KOLLI-INL                                           
150800                     MOVE MFS-ALFA-FAELT-FEL                              
150900                                         TO MOD-KDCMD-ATTR(INDX)          
151000                     MOVE NEJ            TO INDATA-SW                     
151100                   END-IF                                                 
151200                END-IF                                                    
151300             END-IF                                                       
151400           ELSE                                                           
151500             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)            
151600             MOVE NEJ                  TO INDATA-SW                       
151700           END-IF                                                         
151800        END-IF                                                            
151900        ADD +1             TO INDX                                        
152000     END-PERFORM                                                          
152100                                                                          
152200     .                                                                    
152300     EJECT                                                                
152400                                                                          
152500 GB-LOGISK-KONTROLL SECTION.                                              
152600                                                                          
152700     MOVE +1                 TO INDX                                      
152800     PERFORM UNTIL INDX      >  MAX-INDX                                  
152900        IF MID-KDCMD(INDX)   =  W-INLAEGGNING OR W-BINNED OR              
153000                                W-VAELJKOLLI  OR W-SELECTCASE OR          
153100                                W-OK                                      
153200           PERFORM GBA-KOLLA-VALT-KOLLI                                   
153300        END-IF                                                            
153400        ADD +1               TO INDX                                      
153500     END-PERFORM                                                          
153600                                                                          
153700     .                                                                    
153800     EJECT                                                                
153900 GBA-KOLLA-VALT-KOLLI             SECTION.                                
154000                                                                          
154100     MOVE MID-IDRT(INDX)           TO W-IDRT-BSEQ-MIN                     
154200                                      W-IDRT-BSEQ-MAX                     
154300     MOVE MID-IDRTLOP(INDX)        TO W-IDRTLOP-BSEQ-MIN                  
154400                                      W-IDRTLOP-BSEQ-MAX                  
154500     INSPECT MID-IDKOLLI (INDX) REPLACING LEADING SPACE BY ZERO           
154600     MOVE MID-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                  
154700                                      W-IDKOLLI-BSEQ-MAX                  
154800     PERFORM IMS-GHU-SEQB-WLRETA01                                        
154900     IF SEGMENT-FINNS                                                     
155000                                                                          
155100        IF MID-KDCMD (INDX)     = W-INLAEGGNING OR W-BINNED               
155200          IF RET-KDRETSTA            =  W-SND-MOT AND                     
155300             RET-KDKOLSTA            =  W-KLI-MOT                         
155400              PERFORM GBAA-KOLLA-VALT-RT                                  
155500           ELSE                                                           
155600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)             
155700              MOVE NEJ                TO INDATA-SW                        
155800          END-IF                                                          
155900        END-IF                                                            
156000                                                                          
156100        IF MID-KDCMD (INDX)     = W-VAELJKOLLI OR W-SELECTCASE            
156200          IF RET-KDKOLSTA            =  W-KLI-AVV OR                      
156300                                        W-KLI-SAK OR                      
156400                                        W-KLI-VALT OR                     
156500                                        W-KLI-LOSS OR                     
156600                                        W-KLI-BEH                         
156700              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)             
156800              MOVE NEJ                TO INDATA-SW                        
156900          END-IF                                                          
157000        END-IF                                                            
157100        IF MID-KDCMD (INDX)     = W-OK                                    
157200          IF RET-KDKOLSTA            =  W-KLI-SAK                         
157300*HÄR SKALL EN DATUM KONTROLL IN OXÅ + EV ANDRA KONTROLLER                 
157400*EXEMPELVIS KONTROLL ATT RT'S ÖVRIGA KOLLIN ÄR INLAGDA OSV                
157500              CONTINUE                                                    
157600          ELSE                                                            
157700              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)             
157800              MOVE NEJ                TO INDATA-SW                        
157900          END-IF                                                          
158000        END-IF                                                            
158100     ELSE                                                                 
158200        MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDCMD-ATTR(INDX)             
158300        MOVE NEJ                      TO INDATA-SW                        
158400     END-IF                                                               
158500     .                                                                    
158600     EJECT                                                                
158700 GBAA-KOLLA-VALT-RT                SECTION.                               
158800                                                                          
158900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
159000                                                                          
159100** AKTUELLT RETURTILLSTÅND FÅR BARA FINNAS I ETT KOLLI                    
159200                                                                          
159300        MOVE LOW-VALUE             TO W-WDA3F1KY-MIN-X                    
159400        MOVE HIGH-VALUE            TO W-WDA3F1KY-MAX-X                    
159500                                                                          
159600        MOVE RET-IDDC              TO W-IDDC-F1-MIN                       
159700                                      W-IDDC-F1-MAX                       
159800        MOVE RET-IDDISTR           TO W-IDDISTR-F1-MIN                    
159900                                      W-IDDISTR-F1-MAX                    
160000        MOVE RET-IDKUNDNR          TO W-IDKUNDNR-F1-MIN                   
160100                                      W-IDKUNDNR-F1-MAX                   
160200        MOVE RET-IDRAPPNR          TO W-IDRAPPNR-F1-MIN                   
160300                                      W-IDRAPPNR-F1-MAX                   
160400        PERFORM IMS-GU-WLRETG01                                           
160500        IF SEGMENT-FINNS                                                  
160600           PERFORM IMS-GN-WLRETG01                                        
160700           IF SEGMENT-FINNS                                               
160800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)            
160900               MOVE NEJ                TO INDATA-SW                       
161000           END-IF                                                         
161100        END-IF                                                            
161200                                                                          
161300        PERFORM IMS-GHN-SEQB-WLRETA01                                     
161400     END-PERFORM                                                          
161500                                                                          
161600     .                                                                    
161700     EJECT                                                                
161800                                                                          
161900 GC-EV-PLATS-KONTROLL SECTION.                                            
162000                                                                          
162100     MOVE +1                 TO INDX                                      
162200     PERFORM UNTIL INDX      >  MAX-INDX                                  
162300        IF MID-KDCMD(INDX)   =  W-INLAEGGNING OR W-BINNED                 
162400           PERFORM GCA-KOLLA-INL-KOLLI                                    
162500        END-IF                                                            
162600        ADD +1               TO INDX                                      
162700     END-PERFORM                                                          
162800                                                                          
162900     .                                                                    
163000     EJECT                                                                
163100 GCA-KOLLA-INL-KOLLI             SECTION.                                 
163200                                                                          
163300     MOVE MID-IDRT(INDX)           TO W-IDRT-BSEQ-MIN                     
163400                                      W-IDRT-BSEQ-MAX                     
163500     MOVE MID-IDRTLOP(INDX)        TO W-IDRTLOP-BSEQ-MIN                  
163600                                      W-IDRTLOP-BSEQ-MAX                  
163700     INSPECT MID-IDKOLLI (INDX) REPLACING LEADING SPACE BY ZERO           
163800     MOVE MID-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                  
163900                                      W-IDKOLLI-BSEQ-MAX                  
164000     PERFORM IMS-GU-SEQB-WLRETA01                                         
164100     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
164200                                                                          
164300        MOVE RET-IDDISTR          TO W-IDDISTR-ANM                        
164400        MOVE RET-IDKUNDNR         TO W-IDKUNDNR-ANM                       
164500        MOVE RET-IDRAPPNR         TO W-IDRAPPNR-ANM                       
164600                                                                          
164700        PERFORM IMS-GU-WLKREE01                                           
164800        IF SEGMENT-FINNS                                                  
164900           PERFORM IMS-GNP-WLKREE11                                       
165000           PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                     
165100              IF LEV-IDARTNR NOT = 100                                    
165200                PERFORM GCAA-KOLLA-LAGERPLATS                             
165300              END-IF                                                      
165400              PERFORM IMS-GNP-WLKREE11                                    
165500           END-PERFORM                                                    
165600       END-IF                                                             
165700       PERFORM IMS-GN-SEQB-WLRETA01                                       
165800     END-PERFORM                                                          
165900     .                                                                    
166000     EJECT                                                                
166100 GCAA-KOLLA-LAGERPLATS  SECTION.                                          
166200                                                                          
166300     MOVE LEV-IDARTNR          TO W-IDARTNR                               
166400                                                                          
166500     IF DCS-IDDC NOT = MSGI-IDDC                                          
166600        MOVE MSGI-IDDC TO W-IDDC-B6                                       
166700        PERFORM IMS-GU-WDB601                                             
166800     END-IF                                                               
166900     IF DCS-CDC                                                           
167000       PERFORM IMS-GU-WLARTC11                                            
167100       IF SEGMENT-FINNS                                                   
167200         IF CLAG-ADLAGOMR > ZERO OR                                       
167300           CLAG-ADGANG   > ZERO OR                                        
167400           CLAG-ADPLATS  > ZERO                                           
167500           CONTINUE                                                       
167600         ELSE                                                             
167700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)                
167800           MOVE ERR-LAGER-SAKNAS   TO MED-IDMFSFEL                        
167900           MOVE NEJ                TO INDATA-SW                           
168000         END-IF                                                           
168100       ELSE                                                               
168200         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR(INDX)                 
168300         MOVE ERR-LAGER-SAKNAS    TO MED-IDMFSFEL                         
168400         MOVE NEJ                 TO INDATA-SW                            
168500       END-IF                                                             
168600     ELSE                                                                 
168700       MOVE MSGI-IDDC TO W-IDDC-K7                                        
168800       PERFORM IMS-GU-WLARTS11                                            
168900       IF SEGMENT-FINNS                                                   
169000         IF SLAG-ADLAGOMR > ZERO OR                                       
169100           SLAG-ADGANG   > ZERO OR                                        
169200           SLAG-ADPLATS  > ZERO                                           
169300           CONTINUE                                                       
169400         ELSE                                                             
169500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)                
169600           MOVE ERR-LAGER-SAKNAS   TO MED-IDMFSFEL                        
169700           MOVE NEJ                TO INDATA-SW                           
169800         END-IF                                                           
169900       ELSE                                                               
170000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)                
170100         MOVE ERR-LAGER-SAKNAS     TO MED-IDMFSFEL                        
170200         MOVE NEJ                  TO INDATA-SW                           
170300       END-IF                                                             
170400     END-IF                                                               
170500                                                                          
170600     .                                                                    
170700 H-UPPDATERA SECTION.                                                     
170800                                                                          
170900     MOVE +1                     TO 4792-INDX                             
171000                                    4797-INDX                             
171100     ACCEPT DAGENS-DATUM         FROM DATE                                
171200                                                                          
171300     MOVE +1                 TO INDX                                      
171400     PERFORM UNTIL INDX      >  MAX-INDX                                  
171500        IF MID-KDCMD(INDX)   = W-INLAEGGNING OR W-VAELJKOLLI OR           
171600                               W-OK OR                                    
171700                               W-BINNED OR W-SELECTCASE                   
171800           PERFORM HA-UPPDATERA-KOLLI                                     
171900        END-IF                                                            
172000        ADD +1               TO INDX                                      
172100     END-PERFORM                                                          
172200                                                                          
172300     IF 4797-INDX              >  +1                                      
172400        IF 4792-INDX              >  +1                                   
172500           PERFORM S02A-STARTA-R31-RAPPORTERING                           
172600        END-IF                                                            
172700        PERFORM S03-STARTA-R32-RAPPORTERING                               
172800     END-IF                                                               
172900                                                                          
173000     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
173100     CALL WMEDKONV USING MED-WMEDAREA                                     
173200     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
173300     PERFORM MFS-FORM-ATTR                                                
173400     PERFORM MFS-RENSA-FAELT-IN                                           
173500     .                                                                    
173600     EJECT                                                                
173700                                                                          
173800 HA-UPPDATERA-KOLLI        SECTION.                                       
173900                                                                          
174000     MOVE MID-IDRT(INDX)           TO W-IDRT-BSEQ-MIN                     
174100                                      W-IDRT-BSEQ-MAX                     
174200     MOVE MID-IDRTLOP(INDX)        TO W-IDRTLOP-BSEQ-MIN                  
174300                                      W-IDRTLOP-BSEQ-MAX                  
174400     INSPECT MID-IDKOLLI (INDX) REPLACING LEADING SPACE BY ZERO           
174500     MOVE MID-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                  
174600                                      W-IDKOLLI-BSEQ-MAX                  
174700     PERFORM IMS-GHU-SEQB-WLRETA01                                        
174800                                                                          
174900     IF SEGMENT-FINNS                                                     
175000        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
175100                                                                          
175200           IF RET-DARETANK = ZERO                                         
175300              PERFORM S02-FYLL-R31-MID                                    
175400           END-IF                                                         
175500                                                                          
175600           IF MID-KDCMD (INDX)     =  W-INLAEGGNING OR                    
175700                                      W-OK          OR                    
175800                                      W-BINNED                            
175900              PERFORM HBAA-UPPDATERA-LEV-ANM-REG                          
176000                                                                          
176100              MOVE W-SND-INL       TO RET-KDRETSTA                        
176200              MOVE W-KLI-BEH       TO RET-KDKOLSTA                        
176300              MOVE DAGENS-DATUM    TO RET-TIKLAR                          
176400           ELSE                                                           
176500              MOVE RET-IDDISTR             TO W-IDDISTR-ANM               
176600              MOVE RET-IDKUNDNR            TO W-IDKUNDNR-ANM              
176700              MOVE RET-IDRAPPNR            TO W-IDRAPPNR-ANM              
176800                                                                          
176900              PERFORM IMS-GHU-WLKREE01                                    
177000              IF ANM-KDLEVANM = W-ANM-MOT                                 
177100                MOVE W-ANM-PAAB            TO ANM-KDLEVANM                
177200                PERFORM IMS-REPL-WLKREE01                                 
177300                IF CDC-SE                                                 
177400                  PERFORM S04-LAES-WLKREE11                               
177500                  PERFORM UNTIL SEGMENT-SAKNAS                            
177600                    MOVE W-IDANSTNR        TO LEV-IDANSTNR-RET            
177700                    PERFORM IMS-REPL-WLKREE11                             
177800                    PERFORM S04-LAES-WLKREE11                             
177900                  END-PERFORM                                             
178000                END-IF                                                    
178100              END-IF                                                      
178200                                                                          
178300**- TEST FÖR ATT EJ SKRIVA ÖVER REDAN KLAR LEV.ANM . FÖR ATT              
178400**- KOMMA MED I RENSNINGSPGM W4189000,SÅ MÅSTE RET-KDRETSTA = 6           
178500**- W-KLI-BEH    PIC S9(1)  VALUE +9 COMP-3.                              
178600**- W-SND-INL    PIC X(1)   VALUE '6'.                                    
178700                                                                          
178800              IF RET-KDRETSTA = W-SND-INL                                 
178900                CONTINUE                                                  
179000              ELSE                                                        
179100                MOVE W-SND-PAAB      TO RET-KDRETSTA                      
179200              END-IF                                                      
179300              IF RET-KDKOLSTA = W-KLI-BEH                                 
179400                CONTINUE                                                  
179500              ELSE                                                        
179600                MOVE W-KLI-VALT      TO RET-KDKOLSTA                      
179700              END-IF                                                      
179800           END-IF                                                         
179900                                                                          
180000           PERFORM IMS-REPL-SEQB-WLRETA01                                 
180100           PERFORM IMS-GHN-SEQB-WLRETA01                                  
180200        END-PERFORM                                                       
180300                                                                          
180400        IF KOLLI-VALT                                                     
180500           PERFORM HBAB-STARTA-4735                                       
180600        END-IF                                                            
180700     ELSE                                                                 
180800        CALL FELLOG                                                       
180900     END-IF                                                               
181000     .                                                                    
181100     EJECT                                                                
181200                                                                          
181300 HBAA-UPPDATERA-LEV-ANM-REG  SECTION.                                     
181400                                                                          
181500     MOVE RET-IDDISTR             TO W-IDDISTR-ANM                        
181600     MOVE RET-IDKUNDNR            TO W-IDKUNDNR-ANM                       
181700     MOVE RET-IDRAPPNR            TO W-IDRAPPNR-ANM                       
181800                                                                          
181900     PERFORM IMS-GHU-WLKREE01                                             
182000     MOVE ZERO                    TO ANM-KVRADER-OBEH                     
182100     IF ANM-KDLEVANM = W-ANM-MOT                                          
182200       MOVE W-ANM-PAAB              TO ANM-KDLEVANM                       
182300     END-IF                                                               
182400     PERFORM IMS-REPL-WLKREE01                                            
182500                                                                          
182600     PERFORM S04-LAES-WLKREE11                                            
182700     PERFORM UNTIL SEGMENT-SAKNAS                                         
182800        COMPUTE W-KVRADER-KVAR    =  LEV-KVLEVANM-BEKR -                  
182900                                     LEV-KVRETINL -                       
183000                                     LEV-KVAVV-KVANT -                    
183100                                     LEV-KVRETINL-SKR -                   
183200                                     LEV-KVAVV-KVAL                       
183300                                                                          
183400        COMPUTE LEV-KVRETINL      = LEV-KVRETINL +                        
183500                                     W-KVRADER-KVAR                       
183600                                                                          
183700        IF MID-IDANSTNR NUMERIC                                           
183800          MOVE MID-IDANSTNR       TO LEV-IDANSTNR-RET                     
183900        END-IF                                                            
184000                                                                          
184100        MOVE ZERO                 TO LEV-IDILIST                          
184200                                     LEV-TIUTSKR                          
184300                                     LEV-KVANTAL-ILI                      
184400                                     LEV-TIUPPDAT-ILI                     
184500                                                                          
184600        ACCEPT LEV-TIINLINL FROM DATE                                     
184700                                                                          
184800        MOVE RET-IDDISTR    TO MOD4797-MID-IDDISTR(4797-INDX)             
184900        MOVE RET-IDKUNDNR   TO MOD4797-MID-IDKUNDNR(4797-INDX)            
185000        MOVE RET-IDRAPPNR   TO MOD4797-MID-IDRAPPNR(4797-INDX)            
185100        MOVE LEV-IDARTNR    TO MOD4797-MID-IDARTNR(4797-INDX)             
185200        MOVE LEV-IDRADNR    TO MOD4797-MID-IDRADNR(4797-INDX)             
185300        IF MID-KDCMD (INDX)     = W-OK                                    
185400          MOVE W-KVRADER-KVAR TO                                          
185500                        MOD4797-MID-KVRETINL-TRP(4797-INDX)               
185600                        MOD4797-MID-KVRETINL    (4797-INDX)               
185700          MOVE ZERO           TO                                          
185800                        MOD4797-MID-KVAVV-KVANT (4797-INDX)               
185900                        MOD4797-MID-KVRETINL-SKR(4797-INDX)               
186000        ELSE                                                              
186100          COMPUTE MOD4797-MID-KVRETINL(4797-INDX) =                       
186200                                           LEV-KVRETINL +                 
186300                                           LEV-KVRETINL-SKR               
186400          COMPUTE MOD4797-MID-KVAVV-KVANT(4797-INDX) =                    
186500                  LEV-KVAVV-KVAL + LEV-KVAVV-KVANT                        
186600                                                                          
186700          MOVE ZERO           TO                                          
186800                        MOD4797-MID-KVRETINL-TRP(4797-INDX)               
186900                        MOD4797-MID-KVRETINL-SKR(4797-INDX)               
187000        END-IF                                                            
187100                                                                          
187200        PERFORM IMS-REPL-WLKREE11                                         
187300                                                                          
187400        PERFORM S04-LAES-WLKREE11                                         
187500                                                                          
187600        ADD +1                 TO 4797-INDX                               
187700                                                                          
187800        IF 4797-INDX           >  4797-MAX-INDX                           
187900           IF 4792-INDX              >  +1                                
188000              PERFORM S02A-STARTA-R31-RAPPORTERING                        
188100              MOVE +1             TO 4792-INDX                            
188200           END-IF                                                         
188300           PERFORM S03-STARTA-R32-RAPPORTERING                            
188400           MOVE +1             TO 4797-INDX                               
188500        END-IF                                                            
188600                                                                          
188700     END-PERFORM                                                          
188800     .                                                                    
188900     EJECT                                                                
189000                                                                          
189100 HBAB-STARTA-4735        SECTION.                                         
189200                                                                          
189300     MOVE JA                     TO SW-STARTA-ANNAN-BILD                  
189400                                                                          
189500     MOVE MID-IDRT(INDX)         TO MSGI-IDRT                             
189600     MOVE MID-IDRTLOP(INDX)      TO MSGI-IDRTLOP                          
189700     MOVE MID-IDKOLLI(INDX)      TO MSGI-IDKOLLI                          
189800     INSPECT MSGI-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
189900     MOVE '001'                  TO MSGI-KDCALL                           
190000     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
190100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
190200                                                                          
190300     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
190400     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
190500     MOVE 'W4T735'               TO P-TO-P-KDTRANS                        
190600     MOVE '4734'                 TO P-TO-P-IDTRANS                        
190700     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
190800                                                                          
190900     PERFORM S01-INSERT-ALTMSG                                            
191000     .                                                                    
191100     EJECT                                                                
191200                                                                          
191300 S01-INSERT-ALTMSG SECTION.                                               
191400                                                                          
191500     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
191600     PERFORM IMS-CHANGE-ALTMSG                                            
191700     IF STATUS-OK                                                         
191800       PERFORM IMS-INSERT-ALTMSG                                          
191900     ELSE                                                                 
192000       MOVE LOW-VALUE          TO MSG-AREA                                
192100       MOVE 'W4O73401'         TO MFS-IDMOD                               
192200       MOVE '4734'             TO MOD-IDTRANS                             
192300       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
192400       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
192500       IF SECURITY-FEL                                                    
192600         STRING 'NOT AUTHORIZED TO USE '                                  
192700                W-BILD                                                    
192800                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
192900       ELSE                                                               
193000         STRING 'WRONG PICTURE '                                          
193100                 W-BILD                                                   
193200                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
193300       END-IF                                                             
193400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73401 + 4                      
193500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
193600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
193700       PERFORM IMS-INSERT-MSG                                             
193800     END-IF                                                               
193900     .                                                                    
194000     EJECT                                                                
194100 S02-FYLL-R31-MID                SECTION.                                 
194200                                                                          
194300     MOVE RET-IDDC             TO MOD4792-MID-IDDC                        
194400     MOVE RET-DAREGDAT (3:6)   TO MOD4792-MID-TIREGDAT(4792-INDX)         
194500     MOVE RET-TIKLOCK          TO MOD4792-MID-TIKLOCK (4792-INDX)         
194600     ADD +1                    TO 4792-INDX                               
194700                                                                          
194800     IF 4792-INDX              >  4792-MAX-INDX                           
194900        PERFORM S02A-STARTA-R31-RAPPORTERING                              
195000        MOVE +1                TO 4792-INDX                               
195100     END-IF                                                               
195200     .                                                                    
195300     EJECT                                                                
195400                                                                          
195500 S02A-STARTA-R31-RAPPORTERING    SECTION.                                 
195600                                                                          
195700     ACCEPT DAGENS-DATUM       FROM DATE                                  
195800     ACCEPT DAGENS-TID         FROM TIME                                  
195900                                                                          
196000     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
196100     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
196200     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
196300     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
196400     MOVE SPACE                TO MSG-KOM-KDTRANS                         
196500     MOVE 'W4I79201'           TO MSG-KOM-IDCPYTXT                        
196600     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
196700     MOVE 'W4073400'           TO MSG-KOM-IDSNDJOB                        
196800     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
196900     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
197000     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
197100                                                                          
197200     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
197300                                  LENGTH OF MOD4792-MID-W4I79201          
197400                                                                          
197500     MOVE 'W4T792X '           TO P-TO-P-MSG-KDTRANS                      
197600     MOVE '4734'               TO P-TO-P-MSG-IDTRANS                      
197700     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
197800                                                                          
197900     COMPUTE MOD4792-MID-KVPOST  = 4792-INDX - 1                          
198000                                                                          
198100     MOVE MOD4792-MID-W4I79201 TO P-TO-P-MSG-INDATA                       
198200                                                                          
198300     CALL W006KOM USING MSG-PCB                                           
198400                        DISP-PCB                                          
198500                        KOM-KOMA-PCB                                      
198600                        MSG-KOM-WMSGKOM                                   
198700                        P-TO-P-MSG-IO-AREA-SNUF                           
198800                                                                          
198900     .                                                                    
199000     EJECT                                                                
199100 S03-STARTA-R32-RAPPORTERING     SECTION.                                 
199200                                                                          
199300     ACCEPT DAGENS-DATUM       FROM DATE                                  
199400     ACCEPT DAGENS-TID         FROM TIME                                  
199500                                                                          
199600     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
199700     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
199800     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
199900     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
200000     MOVE SPACE                TO MSG-KOM-KDTRANS                         
200100     MOVE 'W4I79701'           TO MSG-KOM-IDCPYTXT                        
200200     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
200300     MOVE 'W4073400'           TO MSG-KOM-IDSNDJOB                        
200400     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
200500     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
200600     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
200700                                                                          
200800     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
200900                                  LENGTH OF MOD4797-MID-W4I79701          
201000                                                                          
201100     MOVE 'W4T797X '           TO P-TO-P-MSG-KDTRANS                      
201200     MOVE '4734'               TO P-TO-P-MSG-IDTRANS                      
201300     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
201400                                                                          
201500     COMPUTE MOD4797-MID-KVPOST  = 4797-INDX - 1                          
201600                                                                          
201700     MOVE MOD4797-MID-W4I79701 TO P-TO-P-MSG-INDATA                       
201800                                                                          
201900     CALL W006KOM USING MSG-PCB                                           
202000                        DISP-PCB                                          
202100                        KOM-KOMA-PCB                                      
202200                        MSG-KOM-WMSGKOM                                   
202300                        P-TO-P-MSG-IO-AREA-SNUF                           
202400                                                                          
202500     .                                                                    
202600     EJECT                                                                
202700 S04-LAES-WLKREE11        SECTION.                                        
202800                                                                          
202900     MOVE NEJ                    TO OKOD-FL-RETILL                        
203000                                    OKOD-FL-INTERNUPPACKNING              
203100     PERFORM IMS-GHNP-WLKREE11                                            
203200     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
203300                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
203400        IF LEV-KDKREBEH(1:1) = 'Y' OR                                     
203500           LEV-KDKREBEH(1:1) = 'J' OR                                     
203600           LEV-KDKREBEH(1:1) = 'C'                                        
203700          IF LEV-IDARTNR NOT = 100                                        
203800            MOVE LEV-KDANMORS TO OKOD-KDANMORS                            
203900*--ANROPA KONTROLL AV ORSAKSKODER                                         
204000            CALL W418OKOD USING OKOD-W418OKOD                             
204100          END-IF                                                          
204200        END-IF                                                            
204300        IF OKOD-FL-RETILL = 'J' OR                                        
204400           OKOD-FL-INTERNUPPACKNING = 'J'                                 
204500           CONTINUE                                                       
204600        ELSE                                                              
204700          PERFORM IMS-GHNP-WLKREE11                                       
204800        END-IF                                                            
204900     END-PERFORM                                                          
205000     .                                                                    
205100     EJECT                                                                
205200 S05-BERAKNA-DATUM        SECTION.                                        
205300                                                                          
205400     PERFORM IMS-GU-WL410711                                              
205500                                                                          
205600     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
205700                                                                          
205800     CALL WDATKONV USING DAT-KDDATFORM                                    
205900                         DAT-I-TIDATUM                                    
206000                         DAT-O-TIDATUM                                    
206100                         DAT-KDSVAR                                       
206200     IF DAT-KDSVAR-FEL                                                    
206300        MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                              
206400        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
206500     END-IF                                                               
206600                                                                          
206700     MOVE DAT-TIAADDD     TO W-TIAADDD-IDAG                               
206800                                                                          
206900     IF 4108-KVDAGAR-KLIARB >= W-TIDDD-IDAG                               
207000        IF W-TIAA-IDAG       = 00                                         
207100          MOVE 99            TO W-TIAA                                    
207200        ELSE                                                              
207300          COMPUTE W-TIAA     = W-TIAA-IDAG  - 1                           
207400        END-IF                                                            
207500        COMPUTE W-TIDDD         =  365                 -                  
207600                                   4108-KVDAGAR-KLIARB +                  
207700                                   W-TIDDD-IDAG                           
207800        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
207900        MOVE 'AADDD'            TO DAT-KDDATFORM                          
208000                                                                          
208100        CALL WDATKONV USING DAT-KDDATFORM                                 
208200                            DAT-I-TIDATUM                                 
208300                            DAT-O-TIDATUM                                 
208400                            DAT-KDSVAR                                    
208500        IF DAT-KDSVAR-FEL                                                 
208600           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
208700           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
208800        END-IF                                                            
208900                                                                          
209000        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIARB                      
209100        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIARB                       
209200     ELSE                                                                 
209300        MOVE W-TIAA-IDAG        TO W-TIAA                                 
209400        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
209500                                   4108-KVDAGAR-KLIARB                    
209600        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
209700        MOVE 'AADDD'            TO DAT-KDDATFORM                          
209800                                                                          
209900        CALL WDATKONV USING DAT-KDDATFORM                                 
210000                            DAT-I-TIDATUM                                 
210100                            DAT-O-TIDATUM                                 
210200                            DAT-KDSVAR                                    
210300        IF DAT-KDSVAR-FEL                                                 
210400           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
210500           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
210600        END-IF                                                            
210700                                                                          
210800        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIARB                      
210900        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIARB                       
211000     END-IF                                                               
211100                                                                          
211200     IF 4108-KVDAGAR-KLIAVV >= W-TIDDD-IDAG                               
211300        IF W-TIAA-IDAG          = 00                                      
211400          MOVE 99               TO W-TIAA                                 
211500        ELSE                                                              
211600          COMPUTE W-TIAA        = W-TIAA-IDAG  - 1                        
211700        END-IF                                                            
211800        COMPUTE W-TIDDD         =  365                 -                  
211900                                   4108-KVDAGAR-KLIAVV +                  
212000                                   W-TIDDD-IDAG                           
212100        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
212200        MOVE 'AADDD'            TO DAT-KDDATFORM                          
212300                                                                          
212400        CALL WDATKONV USING DAT-KDDATFORM                                 
212500                            DAT-I-TIDATUM                                 
212600                            DAT-O-TIDATUM                                 
212700                            DAT-KDSVAR                                    
212800        IF DAT-KDSVAR-FEL                                                 
212900           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
213000           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
213100        END-IF                                                            
213200                                                                          
213300        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIAVV                      
213400        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIAVV                       
213500     ELSE                                                                 
213600        MOVE W-TIAA-IDAG        TO W-TIAA                                 
213700        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
213800                                   4108-KVDAGAR-KLIARB                    
213900        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
214000        MOVE 'AADDD'            TO DAT-KDDATFORM                          
214100                                                                          
214200        CALL WDATKONV USING DAT-KDDATFORM                                 
214300                            DAT-I-TIDATUM                                 
214400                            DAT-O-TIDATUM                                 
214500                            DAT-KDSVAR                                    
214600        IF DAT-KDSVAR-FEL                                                 
214700           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
214800           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
214900        END-IF                                                            
215000                                                                          
215100        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIAVV                      
215200        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIAVV                       
215300     END-IF                                                               
215400     .                                                                    
215500     EJECT                                                                
215600 MFS-RENSA-FAELT-UT SECTION.                                              
215700                                                                          
215800     MOVE +1                    TO INDX                                   
215900     PERFORM UNTIL INDX         >  MAX-INDX                               
216000        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
216100        ADD +1                  TO INDX                                   
216200     END-PERFORM                                                          
216300     .                                                                    
216400     SKIP3                                                                
216500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
216600                                                                          
216700     MOVE MFS-RENSA-FAELT        TO MOD-IDRT          (INDX)              
216800                                    MOD-IDRTLOP       (INDX)              
216900                                    MOD-FLFARLIG      (INDX)              
217000                                    MOD-TIRETANK      (INDX)              
217100                                    MOD-ADINLOMR      (INDX)              
217200                                    MOD-BESTATUS      (INDX)              
217300     .                                                                    
217400     SKIP3                                                                
217500 MFS-RENSA-FAELT-IN SECTION.                                              
217600                                                                          
217700*    --- ALLA INDATA-FÄLT                                                 
217800     MOVE MFS-RENSA-FAELT       TO MOD-IDANSTNR                           
217900                                                                          
218000     MOVE +1 TO INDX                                                      
218100     PERFORM UNTIL INDX         >  MAX-INDX                               
218200       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
218300       ADD +1                   TO INDX                                   
218400     END-PERFORM                                                          
218500     .                                                                    
218600     EJECT                                                                
218700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
218800                                                                          
218900     MOVE +1                  TO INDX                                     
219000     PERFORM UNTIL INDX       >  MAX-INDX                                 
219100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
219200       ADD +1                 TO INDX                                     
219300     END-PERFORM                                                          
219400     .                                                                    
219500                                                                          
219600                                                                          
219700 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
219800                                                                          
219900     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDRT          (INDX)              
220000                                    MOD-IDRTLOP       (INDX)              
220100                                    MOD-IDKOLLI       (INDX)              
220200                                    MOD-FLFARLIG      (INDX)              
220300                                    MOD-TIRETANK      (INDX)              
220400                                    MOD-ADINLOMR      (INDX)              
220500                                    MOD-BESTATUS      (INDX)              
220600     .                                                                    
220700                                                                          
220800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
220900                                                                          
221000*    --- ALLA INDATA-FÄLT                                                 
221100     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDANSTNR                           
221200                                                                          
221300     MOVE +1 TO INDX                                                      
221400     PERFORM UNTIL INDX         >  MAX-INDX                               
221500       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
221600       ADD +1                   TO INDX                                   
221700     END-PERFORM                                                          
221800     .                                                                    
221900     EJECT                                                                
222000 MFS-FORM-ATTR SECTION.                                                   
222100                                                                          
222200*    --- ALLA INDATA-FÄLT                                                 
222300     MOVE MFS-FORMATETS-ATTR    TO MOD-IDANSTNR-ATTR                      
222400     MOVE +1 TO INDX                                                      
222500     PERFORM UNTIL INDX         >  MAX-INDX                               
222600       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                   
222700                                   MOD-IDSNDNR-ATTR(INDX)                 
222800                                   MOD-IDKOLLI-ATTR(INDX)                 
222900                                   MOD-FLFARLIG-ATTR(INDX)                
223000                                   MOD-TIRETANK-ATTR(INDX)                
223100                                   MOD-ADINLOMR-ATTR(INDX)                
223200                                   MOD-BESTATUS-ATTR(INDX)                
223300       ADD +1                   TO INDX                                   
223400     END-PERFORM                                                          
223500     .                                                                    
223600     EJECT                                                                
223700 MFS-FORM-ATTR-UTRAD SECTION.                                             
223800                                                                          
223900*    --- EJ UPPLYST RAD                                                   
224000     MOVE MFS-FORMATETS-ATTR TO MOD-IDSNDNR-ATTR(INDX)                    
224100                                MOD-IDKOLLI-ATTR(INDX)                    
224200                                MOD-FLFARLIG-ATTR(INDX)                   
224300                                MOD-TIRETANK-ATTR(INDX)                   
224400                                MOD-ADINLOMR-ATTR(INDX)                   
224500                                MOD-BESTATUS-ATTR(INDX)                   
224600     .                                                                    
224700     EJECT                                                                
224800 MFS-LYS-UPP-FAELT   SECTION.                                             
224900                                                                          
225000*    --- UPPLYST RAD                                                      
225100     MOVE MFS-ADD-LYS-UPP-FAELT  TO   MOD-IDSNDNR-ATTR(INDX)              
225200                                      MOD-IDKOLLI-ATTR(INDX)              
225300                                      MOD-FLFARLIG-ATTR(INDX)             
225400                                      MOD-TIRETANK-ATTR(INDX)             
225500                                      MOD-ADINLOMR-ATTR(INDX)             
225600                                      MOD-BESTATUS-ATTR(INDX)             
225700     .                                                                    
225800     EJECT                                                                
225900* --- IMS SEKTIONER ---                                                   
226000     SKIP3                                                                
226100 IMS-GET-MSG SECTION.                                                     
226200                                                                          
226300     MOVE '  QC' TO GODK-STATUSKODER                                      
226400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
226500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
226600     PERFORM IMS-STATUSKONTROLL                                           
226700     .                                                                    
226800     SKIP3                                                                
226900 IMS-INSERT-MSG SECTION.                                                  
227000                                                                          
227100     IF MSGI-IDLAND-SPR = 'GB'                                            
227200       MOVE 'N' TO MFS-KDHUVOMR                                           
227300     END-IF                                                               
227400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
227500     MOVE SPACE TO GODK-STATUSKODER                                       
227600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
227700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
227800     PERFORM IMS-STATUSKONTROLL                                           
227900     .                                                                    
228000     EJECT                                                                
228100 IMS-CHANGE-ALTMSG SECTION.                                               
228200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
228300     MOVE '  A1A4' TO GODK-STATUSKODER                                    
228400     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
228500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
228600     PERFORM IMS-STATUSKONTROLL                                           
228700     .                                                                    
228800     SKIP3                                                                
228900 IMS-INSERT-ALTMSG SECTION.                                               
229000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
229100     MOVE SPACE TO GODK-STATUSKODER                                       
229200     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
229300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
229400     PERFORM IMS-STATUSKONTROLL                                           
229500     .                                                                    
229600     EJECT                                                                
229700 IMS-GU-WL410711            SECTION.                                      
229800                                                                          
229900     STRING 'WL410701(WDGXKEY  =' W-WDGX4107-X ')'                        
230000          DELIMITED BY SIZE INTO SSA1                                     
230100     STRING 'WL410711(KDSEGKEY =' W-KDSEGKEY-X ')'                        
230200          DELIMITED BY SIZE INTO SSA2                                     
230300     MOVE '  '           TO GODK-STATUSKODER                              
230400     CALL CBLTDLI USING GU 4107-PCB DLI-IO-AREA2 SSA1 SSA2                
230500     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
230600     PERFORM IMS-STATUSKONTROLL                                           
230700     .                                                                    
230800 IMS-GU-SEQB-WLRETA01       SECTION.                                      
230900                                                                          
231000     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
231100                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
231200          DELIMITED BY SIZE INTO SSA1                                     
231300     MOVE '  GE'           TO GODK-STATUSKODER                            
231400     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-AREA1 SSA1                    
231500     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
231600     PERFORM IMS-STATUSKONTROLL                                           
231700     .                                                                    
231800                                                                          
231900 IMS-GN-SEQB-WLRETA01 SECTION.                                            
232000                                                                          
232100     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
232200                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
232300          DELIMITED BY SIZE INTO SSA1                                     
232400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
232500     CALL CBLTDLI USING GN RETA1-PCB DLI-IO-AREA1 SSA1                    
232600     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
232700     PERFORM IMS-STATUSKONTROLL                                           
232800     .                                                                    
232900 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
233000                                                                          
233100     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
233200                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
233300          DELIMITED BY SIZE INTO SSA1                                     
233400     MOVE '  GE'           TO GODK-STATUSKODER                            
233500     CALL CBLTDLI USING GHU RETA1-PCB DLI-IO-AREA1 SSA1                   
233600     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
233700     PERFORM IMS-STATUSKONTROLL                                           
233800     .                                                                    
233900                                                                          
234000 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
234100                                                                          
234200     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
234300                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
234400          DELIMITED BY SIZE INTO SSA1                                     
234500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
234600     CALL CBLTDLI USING GHN RETA1-PCB DLI-IO-AREA1 SSA1                   
234700     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
234800     PERFORM IMS-STATUSKONTROLL                                           
234900     .                                                                    
235000     EJECT                                                                
235100                                                                          
235200 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
235300                                                                          
235400     MOVE '    '           TO GODK-STATUSKODER                            
235500     CALL CBLTDLI USING REPL RETA1-PCB DLI-IO-AREA1                       
235600     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900     EJECT                                                                
236000                                                                          
236100 IMS-GU-SEQF-WLRETA01       SECTION.                                      
236200                                                                          
236300     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
236400                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
236500          DELIMITED BY SIZE INTO SSA1                                     
236600     MOVE '  GE'           TO GODK-STATUSKODER                            
236700     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-AREA1 SSA1                    
236800     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
236900     PERFORM IMS-STATUSKONTROLL                                           
237000     .                                                                    
237100                                                                          
237200 IMS-GN-SEQF-WLRETA01 SECTION.                                            
237300                                                                          
237400     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
237500                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
237600          DELIMITED BY SIZE INTO SSA1                                     
237700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
237800     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-AREA1 SSA1                    
237900     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
238000     PERFORM IMS-STATUSKONTROLL                                           
238100     .                                                                    
238200     EJECT                                                                
238300                                                                          
238400 IMS-GU-RETA-WLRETA01       SECTION.                                      
238500                                                                          
238600     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
238700          DELIMITED BY SIZE INTO SSA1                                     
238800     MOVE '  GE'           TO GODK-STATUSKODER                            
238900     CALL CBLTDLI USING GU RETA3-PCB DLI-IO-AREA1 SSA1                    
239000     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
239100     PERFORM IMS-STATUSKONTROLL                                           
239200     .                                                                    
239300                                                                          
239400 IMS-GU-RETH-WLRETH01    SECTION.                                         
239500                                                                          
239600     STRING 'WLRETH01(WDA3G1KY>=' W-WDA3G1KY-MIN-X                        
239700                    '&WDA3G1KY<=' W-WDA3G1KY-MAX-X                        
239800                    '&KDARBTYP =' W-KDARBTYP-X                            
239900                    '&IDPERSON =' W-IDPERSON-X ')'                        
240000          DELIMITED BY SIZE INTO SSA1                                     
240100     MOVE '  GE' TO GODK-STATUSKODER                                      
240200     CALL CBLTDLI USING GU RETH-PCB DLI-IO-AREA2 SSA1                     
240300     MOVE RETH-STATUS-CODE TO STATUS-WS                                   
240400     PERFORM IMS-STATUSKONTROLL                                           
240500     .                                                                    
240600     SKIP2                                                                
240700 IMS-GN-RETH-WLRETH01    SECTION.                                         
240800                                                                          
240900     STRING 'WLRETH01(WDA3G1KY>=' W-WDA3G1KY-MIN-X                        
241000                    '&WDA3G1KY<=' W-WDA3G1KY-MAX-X                        
241100                    '&KDARBTYP =' W-KDARBTYP-X                            
241200                    '&IDPERSON =' W-IDPERSON-X ')'                        
241300          DELIMITED BY SIZE INTO SSA1                                     
241400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
241500     CALL CBLTDLI USING GN RETH-PCB DLI-IO-AREA2 SSA1                     
241600     MOVE RETH-STATUS-CODE TO STATUS-WS                                   
241700     PERFORM IMS-STATUSKONTROLL                                           
241800     .                                                                    
241900     SKIP2                                                                
242000 IMS-GU-WLRETG01    SECTION.                                              
242100                                                                          
242200     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
242300                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
242400          DELIMITED BY SIZE INTO SSA1                                     
242500     MOVE '  ' TO GODK-STATUSKODER                                        
242600     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA1 SSA1                     
242700     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
242800     PERFORM IMS-STATUSKONTROLL                                           
242900     .                                                                    
243000     SKIP2                                                                
243100                                                                          
243200 IMS-GN-WLRETG01    SECTION.                                              
243300                                                                          
243400     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
243500                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
243600          DELIMITED BY SIZE INTO SSA1                                     
243700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
243800     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA1 SSA1                     
243900     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
244000     PERFORM IMS-STATUSKONTROLL                                           
244100     .                                                                    
244200     SKIP2                                                                
244300 IMS-GU-WLKREE01    SECTION.                                              
244400                                                                          
244500     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
244600          DELIMITED BY SIZE INTO SSA1                                     
244700     MOVE '  ' TO GODK-STATUSKODER                                        
244800     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA2 SSA1                     
244900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
245000     PERFORM IMS-STATUSKONTROLL                                           
245100     .                                                                    
245200                                                                          
245300 IMS-GHU-WLKREE01    SECTION.                                             
245400                                                                          
245500     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
245600          DELIMITED BY SIZE INTO SSA1                                     
245700     MOVE '  ' TO GODK-STATUSKODER                                        
245800     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA2 SSA1                    
245900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
246000     PERFORM IMS-STATUSKONTROLL                                           
246100     .                                                                    
246200                                                                          
246300 IMS-REPL-WLKREE01      SECTION.                                          
246400                                                                          
246500     MOVE '    '           TO GODK-STATUSKODER                            
246600     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA2                        
246700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
246800     PERFORM IMS-STATUSKONTROLL                                           
246900     .                                                                    
247000     EJECT                                                                
247100                                                                          
247200 IMS-GNP-WLKREE11    SECTION.                                             
247300                                                                          
247400     MOVE 'WLKREE11 '  TO SSA1                                            
247500     MOVE '  GE' TO GODK-STATUSKODER                                      
247600     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
247700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
247800     PERFORM IMS-STATUSKONTROLL                                           
247900     .                                                                    
248000                                                                          
248100 IMS-GHNP-WLKREE11    SECTION.                                            
248200                                                                          
248300     MOVE 'WLKREE11 '  TO SSA1                                            
248400     MOVE '  GE' TO GODK-STATUSKODER                                      
248500     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA2 SSA1                   
248600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
248700     PERFORM IMS-STATUSKONTROLL                                           
248800     .                                                                    
248900                                                                          
249000 IMS-REPL-WLKREE11      SECTION.                                          
249100                                                                          
249200     MOVE '    '           TO GODK-STATUSKODER                            
249300     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA2                        
249400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
249500     PERFORM IMS-STATUSKONTROLL                                           
249600     .                                                                    
249700     EJECT                                                                
249800     EJECT                                                                
249900 IMS-GU-WDB601    SECTION.                                                
250000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
250100          DELIMITED BY SIZE INTO SSA1                                     
250200     MOVE '  GE' TO GODK-STATUSKODER                                      
250300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
250400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
250500     PERFORM IMS-STATUSKONTROLL                                           
250600     IF SEGMENT-SAKNAS                                                    
250700        MOVE SPACE TO DCS-KDDC                                            
250800     END-IF                                                               
250900     .                                                                    
251000 IMS-GU-WLARTC11     SECTION.                                             
251100                                                                          
251200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
251300          DELIMITED BY SIZE INTO SSA1                                     
251400     MOVE 'WLARTC11'   TO  SSA2                                           
251500     MOVE '  GE' TO GODK-STATUSKODER                                      
251600     CALL CBLTDLI USING GU ARTC-PCB CLAG-WDK611 SSA1 SSA2                 
251700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
251800     PERFORM IMS-STATUSKONTROLL                                           
251900     .                                                                    
252000                                                                          
252100 IMS-GU-WLARTS11     SECTION.                                             
252200                                                                          
252300     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
252400          DELIMITED BY SIZE INTO SSA1                                     
252500     STRING 'WLARTS11(IDDC     =' W-IDDC-K7-X ')'                         
252600          DELIMITED BY SIZE INTO SSA2                                     
252700     MOVE '  GE' TO GODK-STATUSKODER                                      
252800     CALL CBLTDLI USING GU ARTS-PCB SLAG-WDK711 SSA1 SSA2                 
252900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
253000     PERFORM IMS-STATUSKONTROLL                                           
253100     .                                                                    
253200                                                                          
253300     EJECT                                                                
253400 IMS-STATUSKONTROLL SECTION.                                              
253500                                                                          
253600     SET STATUS-IX TO 1                                                   
253700     SEARCH GODK-STATUS                                                   
253800       AT END                                                             
253900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
254000         DELIMITED BY SIZE INTO FELTEXT                                   
254100         CALL FELLOG                                                      
254200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
254300         CONTINUE                                                         
254400     END-SEARCH                                                           
254500     .                                                                    
