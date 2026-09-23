000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4029700.                                                
000400 AUTHOR.         GUNNAR LARSSON, IDK.                                     
000500 DATE-WRITTEN.   AUG 1991.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MPP FÖR BIPACKNING AV ORDERRADER.                                
001100*                                                                         
001200*        - MSG LÄSES. FELAKTIG IDTRANS MEDFÖR FELLOG.                     
001300*        - W411BIPA ANROPAS                                               
001400*          (MAX 3 GGR MED 13 BIPA-RADER PER ANROP).                       
001500*        - FÖR VARJE RAD FRÅN W411BIPA                                    
001600*          (W411BIPA HAR FÖR RESP RAD UPPDATERAT WLORDP/WDA5),            
001700*          LÄGGS ORDERBEKRÄFTELSE UPP PÅ WLORDM/WDQ1.                     
001800*        - OM FLER BIPA-RADER FINNS ATT BEHANDLA;                         
001900*            STARTAR OM SIG SJÄLV                                         
002000*          ANNARS (SLUT PÅ BIPA-RADER);                                   
002100*            STARTAR TRANS FÖR SVARSBILDS-PGM.                            
002200*                                                                         
002300*    INDATA:                                                              
002400*        TRANSAKTION: W4T297X  MID: W4I29701                              
002500*                                                                         
002600*    UTDATA:                                                              
002700*        TRANSAKTION: W4T297X  MOD: W4I29701  FÖR OMSTART                 
002800*        TRANSAKTION: W4T213U  MOD: W4I21301                              
002900*        TRANSAKTION: W4T223U  MOD: W4I22301                              
003000*        TRANSAKTION: W4T243U  MOD: W4I24301                              
003100*                                                                         
003200*    ÖVRIGT:                                                              
003300*        OM YTTERLIGARE IDTRANS SKALL TILLÅTAS, SÅ MÅSTE                  
003400*        FÖLJANDE ÄNDRAS:                                                 
003500*          - 88-NIVÅN GODK-MID.                                           
003600*          - 'EVALUATE W-IDTRANS'                                         
003703*                                                                         
003803*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
003804*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
003900     EJECT                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     SKIP2                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500*    -- CHECKED BY WY2000                                                 
004600     SKIP3                                                                
004700*                                                                         
004800 77  IDPGM                   PIC  X(8)           VALUE 'W4029700'.        
004900 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
005000 77  FELTEXT                 PIC  X(80)          VALUE SPACE.             
005100 77  JA                      PIC  X(1)           VALUE 'J'.               
005200 77  NEJ                     PIC  X(1)           VALUE 'N'.               
005300 77  SPRAK-IX                PIC S9(9) COMP SYNC VALUE +0.                
005400*                                                                         
005500 77  INDATA-SW               PIC  X(1)           VALUE 'J'.               
005600     88  INDATA-OK                               VALUE 'J'.               
005700     88  INDATA-FEL                              VALUE 'N'.               
005800*                                                                         
005900 77  W-IDTRANS               PIC  X(4)           VALUE SPACE.             
006000     88  GODK-MID                                VALUE '4211'             
006100                                                       '4212'             
006200                                                       '4214'             
006300                                                       '4222'             
006400                                                       '4242'.            
006500*                                                                         
006600 77  EGEN-IDTRANS            PIC  X(4)           VALUE '4297'.            
006700     EJECT                                                                
006800                                                                          
006900 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
007000 01  FILLER REDEFINES WS-TIHHMMSS.                                        
007100     03 WS-TIHHMM                PIC 9(4).                                
007200     03 FILLER                   PIC 9(2).                                
007300                                                                          
007400     SKIP2                                                                
007500 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
007600 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
007700     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
007800     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
007900     SKIP3                                                                
008000 01  FILLER                  PIC X(16)   VALUE 'WS-FLT       '.           
008100 01  WS-FLT.                                                              
008200*                                                                         
008300     03  WS-BIPA-ANROP       PIC S9(9)   COMP SYNC VALUE ZERO.            
008400     03  WS-INDEX-BIPA       PIC S9(9)   COMP SYNC VALUE ZERO.            
008500     03  WS-INDEX-LAGOMR     PIC S9(9)   COMP SYNC VALUE ZERO.            
008600*                                                                         
008700     EJECT                                                                
008800 01  FILLER                  PIC X(16)   VALUE 'GEN-SUBPGM   '.           
008900 01  GENERELLA-SUBPROGRAM.                                                
009000*                                                                         
009100     03  CBLTDLI             PIC  X(8)           VALUE 'CBLTDLI '.        
009200     03  FELLOG              PIC  X(8)           VALUE 'FELLOG  '.        
009300     03  W411BIPA            PIC  X(8)           VALUE 'W411BIPA'.        
009400     03  W005INIT            PIC  X(8)           VALUE 'W005INIT'.        
009500     SKIP3                                                                
009600*                                                                         
009700*    ------ PARAMETRAR TILL SUBPROGRAM W005INIT                           
009800*01  -COPY WMSGINIT                                                       
009900     EJECT                                                                
010000                                                                          
010100 01  FILLER                  PIC X(16)   VALUE 'SW-SWITCHAR  '.           
010200 01  SW-SWITCHAR.                                                         
010300*                            SLUT PÅ BIPA-RADER                           
010400     03  SW-EOF-BIPA         PIC X(1)    VALUE 'N'.                       
010500     SKIP3                                                                
010600 01  FILLER                  PIC X(16)   VALUE 'K-KONSTANTER '.           
010700 01  K-KONSTANTER.                                                        
010800*                                                                         
010900     03  K-IDKUNDRF7-NOLL    PIC X(10)   VALUE '0000000   '.              
011000*                            MAX ANTAL RADER FRÅN W411BIPA                
011100*                            PER ANROP                                    
011200     03  K-BIPA-RAD-MAX      PIC S9(9) COMP SYNC VALUE +13.               
011300*                            MAX ANTAL ANROP TILL W411BIPA                
011400*                            PER TASK                                     
011500     03  K-BIPA-ANROP-MAX    PIC S9(9) COMP SYNC VALUE +3.                
011600*                            MAX ANTAL BIPA-LAGOMR                        
011700     03  K-INDEX-LAGOMR-MAX  PIC S9(9) COMP SYNC VALUE +99.               
011800     EJECT                                                                
011900 01  FILLER                  PIC X(16)   VALUE 'BIPA-W411BIPA  '.         
012000     -COPY  W411BIPA                                                      
012100     EJECT                                                                
012200 01  FILLER                  PIC X(16)   VALUE 'W4I29701       '.         
012300******************************************************************        
012400*    MSG - INDATA TILL W4029700 FRÅN 'SÄNDANDE' PROGRAM.                  
012500******************************************************************        
012600     -COPY  W4I29701                                                      
012700     EJECT                                                                
012800 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
012900******************************************************************        
013000*    MSG-IO-AREA.                                                *        
013100******************************************************************        
013200     SKIP2                                                                
013300 01  -COPY WMSGAREA                                                       
013400     EJECT                                                                
013500     05  -COPY W4I21301 -PRE MOD4213-  -RED MSG-MID-OUT                   
013600     EJECT                                                                
013700     05  -COPY W4I22301 -PRE MOD4223-  -RED MSG-MID-OUT                   
013800     EJECT                                                                
013900     05  -COPY W4I24301 -PRE MOD4243-  -RED MSG-MID-OUT                   
014000     EJECT                                                                
014100     05  -COPY W4I29701 -PRE MOD4297-  -RED MSG-MID-OUT                   
014200     EJECT                                                                
014300 01  FILLER               PIC X(16)  VALUE 'WMFSAREA'.                    
014400******************************************************************        
014500*    MFS-AREA.                                                   *        
014600******************************************************************        
014700     SKIP2                                                                
014800 01  -COPY WMFSAREA                                                       
014900     EJECT                                                                
015000******************************************************************        
015100*                        IMS-DEL                                 *        
015200******************************************************************        
015300 01  FILLER              PIC X(16)       VALUE 'IMS-WS         '.         
015400     SKIP2                                                                
015500 01  NYCKLAR-TILL-DLI.                                                    
015600*                                                                         
015700     03  W-WDQ101KY-MIN-X.                                                
015800         05  W-IDORDER-Q1-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
015900         05  W-IDARTNR-Q1-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
016000         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
016100         05  W-IDSEQVNR-Q1-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
016200         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
016300                                                                          
016400     03  W-WDQ101KY-MAX-X.                                                
016500         05  W-IDORDER-Q1-MAX    PIC S9(7)   VALUE ZERO COMP-3.           
016600         05  W-IDARTNR-Q1-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
016700         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
016800         05  W-IDSEQVNR-Q1-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
016900         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
017000                                                                          
017100******************************************************************        
017200*    STATUSKOD FRAN IMS                                          *        
017300******************************************************************        
017400 01  FILLER              PIC X(16)       VALUE 'STATUSKOD      '.         
017500     SKIP2                                                                
017600 01  STATUS-WS           PIC XX.                                          
017700     88  SEGMENT-FINNS                   VALUE '  '.                      
017800     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
017900     88  BASEN-SLUT                      VALUE 'GB'.                      
018000     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
018400     EJECT                                                                
018500******************************************************************        
018600*    SSA-ER.                                                     *        
018700******************************************************************        
018800 01  SSA1                PIC X(96).                                       
018900 01  -COPY W0003                                                          
019000     EJECT                                                                
019100 01  FILLER    PIC X(16)  VALUE 'DLI-IO-AREA'.                            
019200 01  DLI-IO-AREA.                                                         
019300     SKIP2                                                                
019400     03  -COPY WDQ101                                                     
019500     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700*                        MSG                                              
019800     -COPY W0009  -PRE MSG-                                               
019900     EJECT                                                                
020000*                        W4T297X                                          
020100     -COPY W0009 -PRE 4297-                                               
020200     EJECT                                                                
020300*                        W4T213                                           
020400     -COPY W0009 -PRE 4213-                                               
020500     EJECT                                                                
020600*                        W4T223                                           
020700     -COPY W0009 -PRE 4223-                                               
020800     EJECT                                                                
020900*                        W4T243                                           
021000     -COPY W0009 -PRE 4243-                                               
021100     EJECT                                                                
021200     -COPY W0008  -PRE USEA-                                              
021300     05  FILLER PIC X.                                                    
021400*                        ORDERBEKR.                                       
021500     -COPY W0008  -PRE ORQM-                                              
021600     05  FILLER PIC X.                                                    
021700     EJECT                                                                
021800 01  BIPA-ORDP-PCB         PIC X(1).                                      
021900 01  BIPA-WDB6-PCB         PIC X(1).                                      
022000 01  BIPA-WDK6-PCB         PIC X(1).                                      
022100 01  BIPA-WDK7-PCB         PIC X(1).                                      
022200 01  BIPA-LEVF-PCB         PIC X(1).                                      
022210 01  BIPA-LEVG-PCB         PIC X(1).                                      
022220 01  BIPA-WDF8-PCB         PIC X(1).                                      
022300 01  BIPA-WDF8A-PCB        PIC X(1).                                      
022400 01  BIPA-LEVA-PCB         PIC X(1).                                      
022500 01  BIPA-ARTS2-PCB        PIC X(1).                                      
022600     EJECT                                                                
022700 PROCEDURE DIVISION USING  MSG-PCB                                        
022800                           4297-PCB 4213-PCB 4223-PCB 4243-PCB            
022900                           USEA-PCB ORQM-PCB                              
023000                           BIPA-ORDP-PCB                                  
023100                           BIPA-WDB6-PCB                                  
023200                           BIPA-WDK6-PCB                                  
023300                           BIPA-WDK7-PCB                                  
023400                           BIPA-LEVF-PCB                                  
023410                           BIPA-LEVG-PCB                                  
023420                           BIPA-WDF8-PCB                                  
023500                           BIPA-WDF8A-PCB                                 
023600                           BIPA-LEVA-PCB                                  
023700                           BIPA-ARTS2-PCB.                                
023800     ENTRY 'DLITCBL' USING MSG-PCB                                        
023900                           4297-PCB 4213-PCB 4223-PCB 4243-PCB            
024000                           USEA-PCB ORQM-PCB                              
024100                           BIPA-ORDP-PCB                                  
024200                           BIPA-WDB6-PCB                                  
024300                           BIPA-WDK6-PCB                                  
024400                           BIPA-WDK7-PCB                                  
024401                           BIPA-LEVF-PCB                                  
024402                           BIPA-LEVG-PCB                                  
024410                           BIPA-WDF8-PCB                                  
024420                           BIPA-WDF8A-PCB                                 
024700                           BIPA-LEVA-PCB                                  
024800                           BIPA-ARTS2-PCB.                                
024900     SKIP2                                                                
025000     PERFORM IMS-GET-MSG                                                  
025100                                                                          
025200     IF SEGMENT-FINNS                                                     
025300         PERFORM A-INIT                                                   
025400                                                                          
025500         MOVE +1                 TO WS-BIPA-ANROP                         
025600                                                                          
025700         PERFORM UNTIL (WS-BIPA-ANROP > K-BIPA-ANROP-MAX                  
025800                   OR   SW-EOF-BIPA   = JA)                               
025900*          * BIPA-ANROP                                                   
026000           PERFORM S15-BYGG-UPP-BIPA-AREA                                 
026100           PERFORM S01-CALL-W411BIPA                                      
026200           MOVE +1               TO WS-INDEX-BIPA                         
026300                                                                          
026400           PERFORM UNTIL (WS-INDEX-BIPA > BIPA-KVBIPACK                   
026500                      OR  BIPA-IDARTNR (WS-INDEX-BIPA) = ZERO)            
026600*            * BIPA-RAD                                                   
026700             PERFORM S16-REDIGERA-OBKR-FRAN-BIPA                          
026800             PERFORM IMS-ISRT-ORQM-WDQ101                                 
026900             ADD +1              TO WS-INDEX-BIPA                         
027000           END-PERFORM                                                    
027100                                                                          
027200           IF  WS-INDEX-BIPA NOT > BIPA-KVBIPACK                          
027300             MOVE JA             TO SW-EOF-BIPA                           
027400           ELSE                                                           
027500             ADD +1              TO WS-BIPA-ANROP                         
027600           END-IF                                                         
027700                                                                          
027800         END-PERFORM                                                      
027900                                                                          
028000         IF  SW-EOF-BIPA = NEJ                                            
028100*          * FLER BIPA-RADER FINNS, STARTAR OM SIG SJÄLV                  
028200           PERFORM S05-STARTA-EGEN-TRANS                                  
028300         ELSE                                                             
028400*          * SLUT PÅ BIPA-RADER, SVARSBILD-PGM STARTAS UPP                
028500           EVALUATE TRUE                                                  
028600             WHEN W-IDTRANS = '4211'                                      
028700             OR   W-IDTRANS = '4212'                                      
028800             OR   W-IDTRANS = '4214'                                      
028900               PERFORM S06-STARTA-4213                                    
029000             WHEN W-IDTRANS = '4222'                                      
029100               PERFORM S07-STARTA-4223                                    
029200             WHEN OTHER                                                   
029300               PERFORM S08-STARTA-4243                                    
029400           END-EVALUATE                                                   
029500         END-IF                                                           
029600                                                                          
029700     END-IF                                                               
029800                                                                          
029900     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 A-INIT SECTION.                                                          
030400                                                                          
030500     MOVE 'STA A-INIT     '               TO   WS-PGM-POSITION            
030600     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I29701                    
030700     MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                               
030800     MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                              
030900                                                                          
031000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
031100     MOVE MSG-IDPFK            TO MFS-IDPFK                               
031200     MOVE MFS-IDTRANS          TO W-IDTRANS                               
031300                                                                          
031400     IF NOT GODK-MID                                                      
031500       STRING 'FELAKTIG IDTRANS: ' W-IDTRANS                              
031600         DELIMITED BY SIZE INTO FELTEXT                                   
031700       CALL FELLOG                                                        
031800     END-IF                                                               
031900                                                                          
032000     MOVE LOW-VALUE            TO MSG-AREA                                
032100                                                                          
032200     MOVE NEJ                  TO SW-EOF-BIPA                             
032300     PERFORM AB-FIXA-LOKAL-TID                                            
032400     .                                                                    
032500     EJECT                                                                
032600                                                                          
032700 AB-FIXA-LOKAL-TID SECTION.                                               
032800                                                                          
032900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033000     MOVE '001'             TO MSGI-KDCALL                                
033100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033200     MOVE '4299'            TO MSGI-IDTRANS                               
033300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033400                                                                          
033500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 S01-CALL-W411BIPA SECTION.                                               
034000                                                                          
034100     MOVE 'STA S01-BIPA   '               TO   WS-PGM-POSITION            
034200     CALL W411BIPA USING BIPA-W411BIPA                                    
034300                         BIPA-ORDP-PCB                                    
034400                         BIPA-WDB6-PCB                                    
034500                         BIPA-WDK6-PCB                                    
034600                         BIPA-WDK7-PCB                                    
034700                         BIPA-LEVF-PCB                                    
034710                         BIPA-LEVG-PCB                                    
034720                         BIPA-WDF8-PCB                                    
034800                         BIPA-WDF8A-PCB                                   
034900                         BIPA-LEVA-PCB                                    
035000                         BIPA-ARTS2-PCB                                   
035100     .                                                                    
035200     EJECT                                                                
035300 S05-STARTA-EGEN-TRANS SECTION.                                           
035400                                                                          
035500     MOVE 'STA S05-EGEN-TRANS '           TO   WS-PGM-POSITION            
035600     COMPUTE MSG-KVLL = LENGTH OF MOD4297-MID-W4I29701 + 17               
035700     MOVE    'W4T297X '           TO MSG-KDTRANS-1                        
035800                                                                          
035900*    * URSPRUNGLIG UPPSTARTANDE IDTRANS BEHÅLLS                           
036000*    * FÖR ATT SENARE KUNNA STARTA RÄTT TRANS VID EOF-BIPA                
036100     MOVE    W-IDTRANS            TO MSG-IDTRANS-1                        
036200                                                                          
036300     MOVE    MFS-KDMFSFOR         TO MSG-KDMFSFOR-1                       
036400                                                                          
036500     MOVE    MID-W4I29701         TO MOD4297-MID-W4I29701                 
036600                                                                          
036700     PERFORM IMS-ISRT-ALT-MSG-4297                                        
036800     .                                                                    
036900     SKIP3                                                                
037000 S06-STARTA-4213 SECTION.                                                 
037100                                                                          
037200     MOVE 'STA S06-4213       '           TO   WS-PGM-POSITION            
037300     COMPUTE MSG-KVLL = LENGTH OF MOD4213-MID-W4I21301 + 17               
037400     MOVE 'W4T213  '           TO MSG-KDTRANS-1                           
037500     MOVE EGEN-IDTRANS         TO MSG-IDTRANS-1                           
037600     MOVE MFS-KDMFSFOR         TO MSG-KDMFSFOR-1                          
037700                                                                          
037800     MOVE MID-IDDISTR          TO MOD4213-MID-IDDISTR-IN                  
037900     MOVE MID-IDKUNDNR         TO MOD4213-MID-IDKUNDNR-IN                 
038000     MOVE MID-IDKUNDRF (3:5)   TO MOD4213-MID-IDORDNR-IN                  
038100                                                                          
038200     PERFORM IMS-ISRT-ALT-MSG-4213                                        
038300     .                                                                    
038400     EJECT                                                                
038500 S07-STARTA-4223 SECTION.                                                 
038600                                                                          
038700     MOVE 'STA S07-4223       '           TO   WS-PGM-POSITION            
038800     COMPUTE MSG-KVLL = LENGTH OF MOD4223-MID-W4I22301 + 17               
038900     MOVE 'W4T223  '           TO MSG-KDTRANS-1                           
039000     MOVE EGEN-IDTRANS         TO MSG-IDTRANS-1                           
039100     MOVE MFS-KDMFSFOR         TO MSG-KDMFSFOR-1                          
039200                                                                          
039300     MOVE MID-IDDISTR          TO MOD4223-MID-IDDISTR-IN                  
039400     MOVE MID-IDKUNDNR         TO MOD4223-MID-IDKUNDNR-IN                 
039500     MOVE MID-IDKUNDRF (3:5)   TO MOD4223-MID-IDORDNR-IN                  
039600                                                                          
039700     PERFORM IMS-ISRT-ALT-MSG-4223                                        
039800     .                                                                    
039900     SKIP3                                                                
040000 S08-STARTA-4243 SECTION.                                                 
040100                                                                          
040200     MOVE 'STA S08-4243       '           TO   WS-PGM-POSITION            
040300     COMPUTE MSG-KVLL = LENGTH OF MOD4243-MID-W4I24301 + 17               
040400     MOVE 'W4T243  '           TO MSG-KDTRANS-1                           
040500     MOVE EGEN-IDTRANS         TO MSG-IDTRANS-1                           
040600     MOVE MFS-KDMFSFOR         TO MSG-KDMFSFOR-1                          
040700                                                                          
040800     MOVE MID-IDDISTR          TO MOD4243-MID-IDDISTR-IN                  
040900     MOVE MID-IDKUNDNR         TO MOD4243-MID-IDKUNDNR-IN                 
041000     MOVE MID-IDKUNDRF (3:5)   TO MOD4243-MID-IDORDNR-IN                  
041100                                                                          
041200     PERFORM IMS-ISRT-ALT-MSG-4243                                        
041300     .                                                                    
041400     EJECT                                                                
041500 S15-BYGG-UPP-BIPA-AREA SECTION.                                          
041600                                                                          
041700     MOVE 'STA S15-BYGG-BIPA  '           TO   WS-PGM-POSITION            
041800     MOVE +2                   TO BIPA-KDORDBEH                           
041900     MOVE MID-IDDISTR          TO BIPA-IDDISTR                            
042000     MOVE MID-IDKUNDNR         TO BIPA-IDKUNDNR                           
042100     MOVE MID-IDKUNDRF (3:5)   TO BIPA-IDKUNDRF                           
042200     MOVE MID-IDKAMPRF         TO BIPA-IDKAMPRF                           
042300     MOVE MID-KDTPOTYP         TO BIPA-KDTPOTYP                           
042400     MOVE MID-KDORDKL          TO BIPA-KDORDKL                            
042500     MOVE MID-KDFAKTYP         TO BIPA-KDFAKTYP                           
042600     MOVE 'IMS '               TO BIPA-IDSYSTEM                           
042700     MOVE MID-IDKONTO          TO BIPA-IDKONTO                            
042800     MOVE MID-IDDC             TO BIPA-IDDC                               
042900     MOVE MID-IDKST            TO BIPA-IDKST                              
043000     MOVE MID-IDANALYS         TO BIPA-IDANALYS                           
043100     MOVE SPACE                TO BIPA-IDPRC-RAD                          
043200     MOVE K-BIPA-RAD-MAX       TO BIPA-KVBIPACK                           
043300     MOVE MID-FLFORBI          TO BIPA-FLFORBI                            
043400     MOVE NEJ                  TO BIPA-FLORDSPE                           
043500     MOVE MID-BEVARREF         TO BIPA-BEVARREF                           
043600     MOVE NEJ                  TO BIPA-FLOVRLEV                           
043700     MOVE MID-IDBIPREF         TO BIPA-IDBIPREF                           
043800     MOVE MID-KDROPACK         TO BIPA-KDROPACK                           
043900     MOVE MID-KDFRAKT          TO BIPA-KDFRAKT                            
044000     MOVE +1 TO WS-INDEX-LAGOMR                                           
044100     PERFORM UNTIL WS-INDEX-LAGOMR > K-INDEX-LAGOMR-MAX                   
044200        MOVE SPACE          TO BIPA-IDPRC-LAGOMR(WS-INDEX-LAGOMR)         
044300        ADD +1              TO WS-INDEX-LAGOMR                            
044400     END-PERFORM                                                          
044500     .                                                                    
044600     EJECT                                                                
044700 S16-REDIGERA-OBKR-FRAN-BIPA SECTION.                                     
044800                                                                          
044900     MOVE 'STA S16-OBKR-BIPA  '           TO   WS-PGM-POSITION            
045000     MOVE MID-IDORDER          TO OBKR-IDORDER                            
045100     MOVE BIPA-IDARTNR(WS-INDEX-BIPA)                                     
045200                               TO OBKR-IDARTNR                            
045300                                                                          
045400     MOVE OBKR-IDORDER         TO W-IDORDER-Q1-MIN                        
045500                                  W-IDORDER-Q1-MAX                        
045600     MOVE OBKR-IDARTNR         TO W-IDARTNR-Q1-MIN                        
045700                                  W-IDARTNR-Q1-MAX                        
045800     MOVE +1                   TO W-IDLOPNR-Q1-MIN                        
045900                                  W-IDLOPNR-Q1-MAX                        
046000                                  W-IDSEQVNR-Q1-MIN                       
046100                                  W-IDSEQVNR-Q1-MAX                       
046200     PERFORM IMS-GU-ORQM-WDQ101                                           
046300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
046400        ADD +1                 TO W-IDLOPNR-Q1-MIN                        
046500                                  W-IDLOPNR-Q1-MAX                        
046600        PERFORM IMS-GN-ORQM-WDQ101                                        
046700     END-PERFORM                                                          
046800     MOVE W-IDLOPNR-Q1-MIN     TO OBKR-IDLOPNR                            
046900                                                                          
047000     MOVE +1                   TO OBKR-IDSEKVNR                           
047100     MOVE  10                  TO OBKR-KDORDBEK                           
047200     MOVE IDPGM                TO OBKR-IDPGM                              
047300     MOVE SPACE                TO OBKR-BEERS                              
047400     MOVE SPACE                TO OBKR-IDBIL                              
047500     MOVE MID-BEKUNDRF         TO OBKR-BEKUNDRF                           
047600     MOVE BIPA-BERADREF(WS-INDEX-BIPA)                                    
047700                               TO OBKR-BERADREF                           
047800     MOVE BIPA-BEVOLREF(WS-INDEX-BIPA)                                    
047900                               TO OBKR-BEVOLREF                           
048000     MOVE BIPA-IDKAMPRF-UT(WS-INDEX-BIPA)                                 
048100                               TO OBKR-IDKAMPRF                           
048200     MOVE +0                   TO OBKR-DIERS-KVOT                         
048300     MOVE NEJ                  TO OBKR-FLAKPLOC                           
048400     MOVE BIPA-FLINVEST(WS-INDEX-BIPA)                                    
048500                               TO OBKR-FLINVEST                           
048600     MOVE NEJ                  TO OBKR-FLOBOK                             
048700     MOVE JA                   TO OBKR-FLOBTRAN                           
048800     MOVE NEJ                  TO OBKR-FLOBPRT                            
048900     MOVE BIPA-FLPRTILL(WS-INDEX-BIPA)                                    
049000                               TO OBKR-FLPRTILL                           
049100     MOVE JA                   TO OBKR-FLRESTN                            
049200     MOVE JA                   TO OBKR-FLSLATT                            
049300     MOVE BIPA-FLERS(WS-INDEX-BIPA)                                       
049400                               TO OBKR-FLTILLK                            
049500     MOVE +0                   TO OBKR-IDARTNR-TILLK                      
049600     MOVE BIPA-IDDISTR         TO OBKR-IDDISTR                            
049700     MOVE BIPA-IDKUNDNR        TO OBKR-IDKUNDNR                           
049800     MOVE K-IDKUNDRF7-NOLL     TO OBKR-IDKUNDRF                           
049900     MOVE BIPA-IDKUNDRF(1:5)   TO OBKR-IDKUNDRF (3:5)                     
050000     MOVE K-IDKUNDRF7-NOLL     TO OBKR-IDKUNDRF-RO                        
050100     MOVE BIPA-IDKUNDRF-UT(WS-INDEX-BIPA) (1:5)                           
050200                               TO OBKR-IDKUNDRF-RO (3:5)                  
050300     MOVE BIPA-IDLEVNR(WS-INDEX-BIPA)                                     
050400                               TO OBKR-IDLEVNR                            
050500     MOVE BIPA-IDLOPNR(WS-INDEX-BIPA)                                     
050600                               TO OBKR-IDLOPNR-RO                         
050700     MOVE BIPA-IDSYSTEM-UT(WS-INDEX-BIPA)                                 
050800                               TO OBKR-IDSYSTEM                           
050900     MOVE BIPA-IDDC-UT(WS-INDEX-BIPA)                                     
051000                               TO OBKR-IDDC                               
051100     MOVE BIPA-IDDC-RO(WS-INDEX-BIPA)                                     
051200                               TO OBKR-IDDC-RO                            
051300     MOVE BIPA-KDDSP(WS-INDEX-BIPA)                                       
051400                               TO OBKR-KDDSP                              
051500     MOVE +0                   TO OBKR-KDERS                              
051600     MOVE BIPA-KDOI(WS-INDEX-BIPA)                                        
051700                               TO OBKR-KDOI                               
051800     MOVE BIPA-CLEARGROUP(WS-INDEX-BIPA)                                  
051900                               TO OBKR-CLEARGROUP                         
052000     MOVE BIPA-KDKVBRYT(WS-INDEX-BIPA)                                    
052100                               TO OBKR-KDKVBRYT                           
052200     MOVE BIPA-KDPRTYP(WS-INDEX-BIPA)                                     
052300                               TO OBKR-KDPRTYP                            
052400     MOVE BIPA-KDTPOTYP-UT(WS-INDEX-BIPA)                                 
052500                               TO OBKR-KDTPOTYP                           
052600     MOVE BIPA-KDVRINFO(WS-INDEX-BIPA)                                    
052700                               TO OBKR-KDVRINFO                           
052800     MOVE +0                   TO OBKR-KVANNANT                           
052900                                  OBKR-KVAVBART                           
053000     MOVE BIPA-KVART(WS-INDEX-BIPA)                                       
053100                               TO OBKR-KVBEART-Q                          
053200                                  OBKR-KVBEART                            
053300     MOVE +0                   TO OBKR-KVBEART-TILLK                      
053400                                  OBKR-KVPREAVB                           
053500                                  OBKR-KVPRERO                            
053600                                  OBKR-KVQPACK                            
053700                                  OBKR-KVRO                               
053800                                  OBKR-KVSLATT                            
053900     EJECT                                                                
054000     MOVE BIPA-PRARTNTO(WS-INDEX-BIPA)                                    
054100                               TO OBKR-PRARTNTO                           
054110     MOVE BIPA-PRAVCOST(WS-INDEX-BIPA)                                    
054120                               TO OBKR-PRAVCOST                           
054200     MOVE BIPA-DEAL-PR-LINE(WS-INDEX-BIPA)                                
054300                               TO OBKR-DEAL-PR-LINE                       
054400     MOVE +0                   TO OBKR-PRBPRIS                            
054500     MOVE BIPA-REKSIFFR(WS-INDEX-BIPA)                                    
054600                               TO OBKR-REKSIFFR                           
054700     MOVE +0                   TO OBKR-REKSIFFR-TILLK                     
054800     MOVE +0                   TO OBKR-RERF-RAD                           
054900     MOVE +0                   TO OBKR-TIDISPIN                           
055000     MOVE MID-TIREGDAT         TO OBKR-TIORDREG                           
055100     MOVE +0                   TO OBKR-TIPRIS                             
055200     MOVE MSGI-TILOKDAT        TO OBKR-TIREGDAT                           
055300     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
055400     MOVE WS-TIHHMMSS          TO OBKR-TIREGTID                           
055500     MOVE BIPA-TIRODAT(WS-INDEX-BIPA)                                     
055600                               TO OBKR-TIRODAT                            
055700     MOVE OBKR-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
055800     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
055900       MOVE 20                 TO WS-SEKEL-9KOMPL                         
056000     ELSE                                                                 
056100       MOVE 19                 TO WS-SEKEL-9KOMPL                         
056200     END-IF                                                               
056300     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
056400     MOVE BIPA-TITPO(WS-INDEX-BIPA)                                       
056500                               TO OBKR-TITPO                              
056600     MOVE MID-TIREGDAT         TO WS-AAMMDD-9KOMPL                        
056700     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
056800     MOVE BIPA-KDFRAKT-UT(WS-INDEX-BIPA)                                  
056900                               TO OBKR-KDFRAKT                            
057000     MOVE BIPA-KDORDKL-UT(WS-INDEX-BIPA)                                  
057100                               TO OBKR-KDORDKL                            
057200                                                                          
057300     MOVE BIPA-KDORDTYP-LDC(WS-INDEX-BIPA)                                
057400                              TO OBKR-KDORDTYP-LDC                        
057500     MOVE BIPA-TIREPDAT(WS-INDEX-BIPA)                                    
057600                              TO OBKR-TIREPDAT                            
057700     MOVE BIPA-IDKUNDRF-WIP(WS-INDEX-BIPA)                                
057800                              TO OBKR-IDKUNDRF-WIP                        
058000     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
058010     MOVE BIPA-PRAVCOST(WS-INDEX-BIPA)                                    
058020                               TO OBKR-PRAVCOST                           
058030     MOVE BIPA-KDVALISO(WS-INDEX-BIPA)                                    
058040                               TO OBKR-KDVALISO                           
058100     .                                                                    
058200                                                                          
058300 IMS-GET-MSG SECTION.                                                     
058400                                                                          
058500     MOVE    '  QC'          TO    GODK-STATUSKODER                       
058600     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
058700     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
058800     PERFORM IMS-STATUSKONTROLL                                           
058900     .                                                                    
059000     SKIP3                                                                
059100 IMS-ISRT-ALT-MSG-4297 SECTION.                                           
059200                                                                          
059300     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
059400     MOVE    '  '             TO    GODK-STATUSKODER                      
059500     CALL    CBLTDLI          USING ISRT 4297-PCB MSG-IO-AREA             
059600     MOVE    4297-STATUS-CODE TO    STATUS-WS                             
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     EJECT                                                                
060000 IMS-ISRT-ALT-MSG-4213 SECTION.                                           
060100                                                                          
060200     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
060300     MOVE    '  '             TO    GODK-STATUSKODER                      
060400     CALL    CBLTDLI          USING ISRT 4213-PCB MSG-IO-AREA             
060500     MOVE    4213-STATUS-CODE TO    STATUS-WS                             
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-ISRT-ALT-MSG-4223 SECTION.                                           
061000                                                                          
061100     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
061200     MOVE    '  '             TO    GODK-STATUSKODER                      
061300     CALL    CBLTDLI          USING ISRT 4223-PCB MSG-IO-AREA             
061400     MOVE    4223-STATUS-CODE TO    STATUS-WS                             
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700     SKIP3                                                                
061800 IMS-ISRT-ALT-MSG-4243 SECTION.                                           
061900                                                                          
062000     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
062100     MOVE    '  '             TO    GODK-STATUSKODER                      
062200     CALL    CBLTDLI          USING ISRT 4243-PCB MSG-IO-AREA             
062300     MOVE    4243-STATUS-CODE TO    STATUS-WS                             
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     EJECT                                                                
062700 IMS-ISRT-ORQM-WDQ101 SECTION.                                            
062800                                                                          
062900     MOVE 'WLORQM01 '          TO SSA1                                    
063000     MOVE '    '               TO GODK-STATUSKODER                        
063100     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA SSA1                    
063200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     .                                                                    
063500     SKIP2                                                                
063600 IMS-GU-ORQM-WDQ101 SECTION.                                              
063700                                                                          
063800     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
063900                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
064000          DELIMITED BY SIZE INTO SSA1                                     
064100     MOVE '  GE'               TO GODK-STATUSKODER                        
064200     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA SSA1                    
064300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
064400     PERFORM IMS-STATUSKONTROLL                                           
064500     .                                                                    
064600     SKIP2                                                                
064700 IMS-GN-ORQM-WDQ101 SECTION.                                              
064800                                                                          
064900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
065000                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
065100          DELIMITED BY SIZE INTO SSA1                                     
065200     MOVE '  GEGB'             TO GODK-STATUSKODER                        
065300     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA SSA1                    
065400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-STATUSKONTROLL SECTION.                                              
065900                                                                          
066000     SET    STATUS-IX TO 1                                                
066100     SEARCH GODK-STATUS                                                   
066200       AT END                                                             
066300         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
066400           DELIMITED BY SIZE INTO FELTEXT                                 
066500         CALL FELLOG                                                      
066600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066700         CONTINUE                                                         
066800     END-SEARCH                                                           
066900     .                                                                    
