000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010100.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   APRIL 92.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET ÄR EN UPPDATERINGS-MPP, SOM VISAR EN ELLER            
001200*        ALLA VAGNAR.                                                     
001300*                                                                         
001400*        EN VISS VAGN KAN MAN ADMINISTRERA GENOM ATT ANGE NY              
001500*        PLACERING ELLER ADRESS, MAN KAN TÖMMA DEN ELLER ANGE             
001600*        INLÄGGNING.                                                      
001700*                                                                         
001800*        MAN LÄSER : W6G1 - 01 (W6PLAA01 - PLACERINGSREG)                 
001900*                         - 30 (W6PLAA11 - PLACERINGSOMRÅDEN)             
002000*                                                                         
002100*                    W6D1 - 01 (W6INLA01 - INLEVERANSREG)                 
002200*                         - 11 (W6INLA11 - PARTI)                         
002300*                         - 21 (W6INLA21 - ARTIKELRAD/KOLLI)              
002400*                         - F1 (W6INLG01 - VAGN SEK INDEX)                
002500*                                                                         
002600*                    WDB6 - 01 DCREGISTER                                 
002700*                                                                         
002800*        PROGRAMMET UPPDATERAR - W6D121 (W6INLA21 - ARTRAD/KOLLI)         
002900*                              - SKAPAR UPPFÖLJNINGSTRANS (GENOM          
003000*                                ATT ANROPA W6019100)                     
003100*                              - AVSLUTAR PARTIET (GENOM ATT AN-          
003200*                                ROPA W6019300)                           
003300*                                                                         
003400*    INDATA.                                                              
003500*        TRANSAKTION: W6T101  W6T101U                                     
003600*                     W6T191U                                             
003700*                     W6T193U VIA DISPATCHER                              
003800*                                                                         
003900*        MID:         W6I10101                                            
004000*                                                                         
004100*    UTDATA.                                                              
004200*        MOD:         W6O10101                                            
004300*                     W6I19101 (PROG-TO-PROG-SW)                          
004400*                     W6I19301 (VIA DISPATCHER)                           
004500                                                                          
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(08)   VALUE 'W6010100'.            
005400 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005500                                                                          
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  EN-VAGN                     PIC X       VALUE 'E'.                   
005900 77  FLERA-VAGNAR                PIC X       VALUE 'F'.                   
006000                                                                          
006100 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006200 77  MAX-RADER                   PIC S9(9)   VALUE +12  COMP SYNC.        
006300 77  MAX-TRANSAR                 PIC S9(2)   VALUE +23  COMP SYNC.        
006400 77  6193-TRANS-RAKN             PIC S9(2)   VALUE +0   COMP SYNC.        
006500 77  TRANS-INDX                  PIC S9(9)   VALUE +0   COMP SYNC.        
006600 77  MAX-KVPOST                  PIC S9(9)   VALUE +24  COMP SYNC.        
006700 77  6197-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
006800 77  MAX-6197-IX                 PIC S9(9)   VALUE +15  COMP SYNC.        
006900 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
007000 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +404 COMP SYNC.        
007100 77  WS-FLKVAKAR                 PIC X       VALUE SPACE.                 
007200 77  WS-FLKVAFEL                 PIC X       VALUE SPACE.                 
007300                                                                          
007400 77  P-TO-P-PREFIX-LNG           PIC S9(4)   VALUE +17  COMP SYNC.        
007500 77  MID-6191-FASTDEL-LNG        PIC S9(4)   VALUE +17  COMP SYNC.        
007600 77  MID-6191-UPPF-POST-LNG      PIC S9(4)   VALUE +64  COMP SYNC.        
007700                                                                          
007800 77  WS-IDINLVGN-SPAR            PIC 9(3)    VALUE ZERO.                  
007900 77  WS-IDLOPNRM-SPAR            PIC S9(9)   VALUE ZERO COMP-3.           
008000 77  WS-BEFT                     PIC S9(3)   VALUE ZERO COMP-3.           
008100 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
008200                                                                          
008300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008400 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
008500                                                                          
008600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008700     88  INDATA-OK                           VALUE 'J'.                   
008800     88  INDATA-FEL                          VALUE 'N'.                   
008900                                                                          
009000 77  BILDVAGN-SW                 PIC X       VALUE 'J'.                   
009100     88  VAGN-FINNS                          VALUE 'J'.                   
009200     88  VAGN-SAKNAS                         VALUE 'N'.                   
009300                                                                          
009400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009500     88  NYCKLAR-OK                          VALUE 'J'.                   
009600     88  NYCKLAR-FEL                         VALUE 'N'.                   
009700                                                                          
009800 77  UPPDAT-SW                   PIC X       VALUE 'J'.                   
009900     88  UPPDATERING-OK                      VALUE 'J'.                   
010000     88  UPPDATERING-FEL                     VALUE 'N'.                   
010100                                                                          
010200 77  VAGN-SW                     PIC X       VALUE 'E'.                   
010300     88  EN-VISS-VAGN                        VALUE 'E'.                   
010400     88  MANGA-VAGNAR                        VALUE 'F'.                   
010500                                                                          
010600 77  KOLLI-SW                    PIC X       VALUE 'J'.                   
010700     88  KOLLI-SLUT                          VALUE 'J'.                   
010800     88  KOLLI-EJ-SLUT                       VALUE 'N'.                   
010900                                                                          
011000 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
011100     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
011200     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
011300                                                                          
011400 77  FBRAPP-SW                   PIC X       VALUE 'J'.                   
011500     88  FBRAPP                              VALUE 'J'.                   
011600     88  FBRAPP-FINNS                        VALUE 'N'.                   
011700                                                                          
011800 77  PRIOGODS-SW                 PIC X       VALUE 'N'.                   
011900     88  PRIOGODS                            VALUE 'J'.                   
012000                                                                          
012100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
012200     88  ALLT-OK                             VALUE 'J'.                   
012300                                                                          
012400 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
012500     88  OMSTART                             VALUE 'J'.                   
012600                                                                          
012700 77  KVAL-SW                     PIC X       VALUE 'N'.                   
012800     88  KVAL-FEL                            VALUE 'J'.                   
012900                                                                          
013000 77  FOERSTA-6197-SW             PIC X       VALUE 'J'.                   
013100     88  FOERSTA-6197                        VALUE 'J'.                   
013200                                                                          
013300 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
013400     88  FOERSTA-6191                        VALUE 'J'.                   
013500                                                                          
013600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013700     88  EGEN-MID                            VALUE '6101'.                
013800     88  GODK-MID                            VALUE '6101'.                
013900     88  HELP-MID                            VALUE '0551'.                
014000     EJECT                                                                
014100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014200 01  GENERELLA-SUBPROGRAM.                                                
014300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014600     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014800     EJECT                                                                
014900*   -COPY WMSGINIT                                                        
015000     EJECT                                                                
015100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015200*   -COPY WMEDAREA                                                        
015300     EJECT                                                                
015400 01  FILLER                        PIC X(16)   VALUE 'FELM AREA'.         
015500 01  FELM-CODES.                                                          
015600     03  FELM-KOR-UPPLYSTA-FAELT   PIC X(3)    VALUE '001'.               
015700     03  FELM-FINNS-EJ             PIC X(3)    VALUE '010'.               
015800     03  FELM-PF11-O-TOM-INDATRAD  PIC X(3)    VALUE '011'.               
015900     03  FELM-SISTA-SIDAN-VISAD    PIC X(3)    VALUE '115'.               
016000     03  FELM-FEL-NYCKEL           PIC X(3)    VALUE '401'.               
016100     03  FELM-SAKNAS               PIC X(3)    VALUE '999'.               
016200     SKIP3                                                                
016300 01  FILLER                        PIC X(16)   VALUE 'INFO AREA'.         
016400 01  MESSAGE-CODES.                                                       
016500     03  INFO-TRYCK-PF11           PIC X(3)    VALUE '003'.               
016600     03  INFO-FOERSTA-SIDAN        PIC X(3)    VALUE '006'.               
016700     03  INFO-UPPDAT-GJORD         PIC X(3)    VALUE '101'.               
016800     03  INFO-MER-INFO-FINNS-PF8   PIC X(3)    VALUE '105'.               
016900     03  INFO-SISTA-SIDAN          PIC X(3)    VALUE '106'.               
017000     03  INFO-KVALFEL              PIC X(3)    VALUE '189'.               
017100     03  INFO-KOLLI-KVAR           PIC X(3)    VALUE '230'.               
017200     EJECT                                                                
017300 01  FILLER                        PIC X(16)   VALUE 'SPAR AREA'.         
017400 01  SPAR-AREA.                                                           
017500     03  SPAR-ADINLOMR             PIC X(4).                              
017600     03  SPAR-ADINLOMR-NXT         PIC X(4).                              
017700     03  SPAR-KDINLSTA             PIC X(3).                              
017800     03  SPAR-KVINLART             PIC S9(7).                             
017900     EJECT                                                                
018000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018300     SKIP3                                                                
018400*01  MID -COPY W6I10101                                                   
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018700     SKIP3                                                                
018800*01  -COPY WMSGAREA                                                       
018900     EJECT                                                                
019000*    03  MOD -COPY W6O10101   -RED MSG-AREA.                              
019100     EJECT                                                                
019200 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
019300     SKIP3                                                                
019400*01  -COPY WMSGKOM                                                        
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)  VALUE 'MSG/KOM-AREA'.         
019700     SKIP3                                                                
019800*01  -COPY WMSGAREA   -PRE  K                                             
019900     EJECT                                                                
020000*    05  MOD -COPY W6I19301 -PRE 6193-   -RED KMSG-MID-OUT.               
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
020300                                                                          
020400*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
020500     EJECT                                                                
020600                                                                          
020700*01  -COPY W6I19101   -PRE 6191-                                          
020800     EJECT                                                                
020900*01  -COPY W6I19701   -PRE 6197-                                          
021000     EJECT                                                                
021100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021200     SKIP3                                                                
021300*01  -COPY WMFSAREA                                                       
021400     EJECT                                                                
021500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021600*                                                                         
021700 01  FILLER                        PIC X(16)   VALUE 'IMS-WS'.            
021800                                                                          
021900 01  NYCKLAR-TILL-DLI.                                                    
022000                                                                          
022100*--------W6D1F (SEK INGÅNG)                                               
022200     03  W-W6D1FSEQ-X.                                                    
022300         05  W-SEQF-IDINLVGN       PIC 9(3)   VALUE ZERO.                 
022400                                                                          
022500*--------W6D1F (EGEN BAS)                                                 
022600     03  W-W6D1F1KY-MIN-X.                                                
022700         05  W-SEQF-IDINLVGN-MIN   PIC 9(3)   VALUE ZERO.                 
022800         05  FILLER                PIC X(25)  VALUE LOW-VALUE.            
022900                                                                          
023000     03  W-W6D1F1KY-MAX-X.                                                
023100         05  W-SEQF-IDINLVGN-MAX   PIC 9(3)   VALUE 999.                  
023200         05  FILLER                PIC X(25)  VALUE HIGH-VALUE.           
023300                                                                          
023400*--------W6D1                                                             
023500     03  W-W6D101KY-X.                                                    
023600         05  W-INL-IDDC            PIC  X(2)  VALUE SPACE.                
023700         05  W-INL-IDLEVNR         PIC X(5)   VALUE SPACE.                
023800         05  W-INL-IDFS            PIC X(8)   VALUE SPACE.                
023900         05  W-INL-TIAVIDAT        PIC S9(7)  VALUE ZERO COMP-3.          
024000                                                                          
024100     03  W-W6D111KY-X.                                                    
024200         05  W-ART-IDRADNR-INL     PIC S9(5)  VALUE ZERO COMP-3.          
024300                                                                          
024400     03  W-W6D121KY-X.                                                    
024500         05  W-RAD-IDRADNR         PIC S9(5)  VALUE ZERO COMP-3.          
024600                                                                          
024700     03  W-RAD-IDINLVGN-X.                                                
024800         05  W-RAD-IDINLVGN        PIC 9(3)   VALUE ZERO.                 
024900                                                                          
025000     03  W-IDLOPNRM-X.                                                    
025100         05  W-IDLOPNRM            PIC S9(9)   VALUE ZERO COMP-3.         
025200                                                                          
025300     03  W-IDDC-X.                                                        
025400         05  W-IDDC                PIC  X(2)   VALUE SPACE.               
025500                                                                          
025600*--------W6G1                                                             
025700     03  W-W6GXKEY-6005-X.                                                
025800         05  W-IDHTYP-6005         PIC X(4)    VALUE '6005'.              
025900         05  W-IDDC-6005           PIC X(2)    VALUE SPACE.               
026000         05  FILLER                PIC X(24)   VALUE LOW-VALUE.           
026100                                                                          
026200     03  W-W6GXKEY-6006-X.                                                
026300         05  W-ADINLOMR-6006       PIC X(4)    VALUE SPACE.               
026400         05  FILLER                PIC X(1)    VALUE LOW-VALUE.           
026500                                                                          
026600*--------WDB6                                                             
026700                                                                          
026800     03  W-IDDC-B6-X.                                                     
026900         05 W-IDDC-B6            PIC X(2).                                
027000                                                                          
027100*    --- STATUS-KOD FRÅN IMS                                              
027200 01  STATUS-WS                     PIC XX.                                
027300     88  SEGMENT-FINNS                       VALUE '  '.                  
027400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027500     88  BASEN-SLUT                          VALUE 'GB'.                  
027600                                                                          
027700 01  GODK-STATUSKODER.                                                    
027800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027900     SKIP2                                                                
028000 01  SSA1                          PIC X(128).                            
028100 01  SSA2                          PIC X(64).                             
028200 01  SSA3                          PIC X(64).                             
028300     EJECT                                                                
028400*    --- IMS FUNKTIONSKODER                                               
028500*01  -COPY W0003                                                          
028600     EJECT                                                                
028700*    ---  DLI INPUT-OUTPUT AREA                                           
028800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
028900                                                                          
029000 01  DLI-IO-AREA.                                                         
029100     03  IO-AREA               PIC X(150)   VALUE SPACE.                  
029200     SKIP2                                                                
029300 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6G130'.        
029400                                                                          
029500 01  DLI-IO-AREA-W6G130.                                                  
029600     03  W6GX6006.                                                        
029700*        05  -COPY W6GX6006                                               
029800     EJECT                                                                
029900 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D1F1'.        
030000                                                                          
030100 01  DLI-IO-AREA-W6D1F1.                                                  
030200     03  W6INLG01.                                                        
030300*        05  -COPY W6D1F1                                                 
030400     EJECT                                                                
030500 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D1B1'.        
030600                                                                          
030700 01  DLI-IO-AREA-W6D1B1.                                                  
030800     03  W6INLC01.                                                        
030900*        05  -COPY W6D1B1                                                 
031000     EJECT                                                                
031100 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D111'.        
031200                                                                          
031300 01  DLI-IO-AREA-W6D111.                                                  
031400     03  W6INLA11.                                                        
031500*        05  -COPY W6D111                                                 
031600     EJECT                                                                
031700 01  FILLER                    PIC X(16)    VALUE 'DLI-IO-W6D121'.        
031800                                                                          
031900 01  DLI-IO-AREA-W6D121.                                                  
032000     03  W6INLA21.                                                        
032100*        05  -COPY W6D121                                                 
032200     EJECT                                                                
032300 01  DLI-IO-AREA-UPFA01.                                                  
032400     03  W6UPFA01.                                                        
032500*        05  -COPY W6L101                                                 
032600     SKIP3                                                                
032700 01  DLI-IO-AREA-UPFA11.                                                  
032800     03  W6UPFA11.                                                        
032900*        05  -COPY W6L111                                                 
033000     SKIP3                                                                
033100 01  DLI-IO-AREA-UPFA12.                                                  
033200     03  W6UPFA12.                                                        
033300*        05  -COPY W6L112                                                 
033400                                                                          
033500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033600 01   DLI-IO-AREA-B601.                                                   
033700*     03  -COPY WDB601                                                    
033800                                                                          
033900     SKIP3                                                                
034000 LINKAGE SECTION.                                                         
034100                                                                          
034200*01  -COPY W0009      -PRE MSG-                                           
034300     EJECT                                                                
034400*01  -COPY W0009      -PRE ALT-                                           
034500     EJECT                                                                
034600*01  -COPY W0009      -PRE 6191-                                          
034700     EJECT                                                                
034800*01  -COPY W0009      -PRE 6197-                                          
034900     EJECT                                                                
035000*01  -COPY W0009      -PRE DISP-                                          
035100     EJECT                                                                
035200*01  -COPY W0008      -PRE USEA-                                          
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500*01  -COPY W0008      -PRE INLA-F-                                        
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800*01  -COPY W0008      -PRE INLA-                                          
035900     05  FILLER                  PIC X.                                   
036000     EJECT                                                                
036100*01  -COPY W0008      -PRE INLG-                                          
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008      -PRE INLA-B-                                        
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700*01  -COPY W0008      -PRE INLB-                                          
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008      -PRE PLAA-                                          
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008      -PRE KOM-KOMA-                                      
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600*01  -COPY W0008  -PRE UPFA-                                              
037700     05  FILLER                  PIC X.                                   
037800     EJECT                                                                
037900*01  -COPY W0008  -PRE WDB6-                                              
038000     05  FILLER                  PIC X.                                   
038100     EJECT                                                                
038200 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB 6191-PCB 6197-PCB             
038300                                    DISP-PCB USEA-PCB                     
038400                                    INLA-F-PCB       INLA-PCB             
038500                                    INLG-PCB                              
038600                                    PLAA-PCB                              
038700                                    KOM-KOMA-PCB UPFA-PCB                 
038800                                    WDB6-PCB.                             
038900     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB 6191-PCB 6197-PCB             
039000                                    DISP-PCB USEA-PCB                     
039100                                    INLA-F-PCB       INLA-PCB             
039200                                    INLG-PCB                              
039300                                    PLAA-PCB                              
039400                                    KOM-KOMA-PCB UPFA-PCB                 
039500                                    WDB6-PCB.                             
039600                                                                          
039700     PERFORM IMS-GET-MSG                                                  
039800     IF SEGMENT-FINNS                                                     
039900       PERFORM A-INIT                                                     
040000       PERFORM B-KOLLA-NYCKLAR                                            
040100       IF NYCKLAR-OK                                                      
040200         IF MFS-UPDATE                                                    
040300           PERFORM F-UPPDATERA                                            
040400         ELSE                                                             
040500           IF MFS-FIRST                                                   
040600             PERFORM C-FOERSTA-SIDAN                                      
040700           ELSE                                                           
040800             IF MFS-NEXT                                                  
040900               PERFORM D-NAESTA-SIDAN                                     
041000             ELSE                                                         
041100               PERFORM E-SAMMA-SIDA                                       
041200             END-IF                                                       
041300           END-IF                                                         
041400         END-IF                                                           
041500         IF NOT OMSTART                                                   
041600           PERFORM G-LAES-VISA-BILD                                       
041700         END-IF                                                           
041800       ELSE                                                               
041900        IF GODK-MID                                                       
042000          MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                            
042100          CALL WMEDKONV USING MED-WMEDAREA                                
042200          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
042300        END-IF                                                            
042400       END-IF                                                             
042500       IF OMSTART                                                         
042600         MOVE 'W6T101U  '  TO P-TO-P-MSG-KDTRANS                          
042700         MOVE '6101'       TO P-TO-P-MSG-IDTRANS                          
042800         MOVE MFS-KDMFSFOR TO P-TO-P-MSG-KDMFSFOR                         
042900         COMPUTE P-TO-P-MSG-KVLL =  P-TO-P-PREFIX-LNG + 62                
043000         MOVE MID-W6I10101   TO P-TO-P-MSG-INDATA                         
043100         PERFORM IMS-ISRT-ALT-MSG-6101                                    
043200       ELSE                                                               
043300         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
043400         PERFORM IMS-INSERT-MSG                                           
043500       END-IF                                                             
043600     END-IF                                                               
043700     MOVE ZERO TO RETURN-CODE                                             
043800     GOBACK                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 A-INIT SECTION.                                                          
044200                                                                          
044300     IF MSG-DUBBLA-TRANSKODER                                             
044400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10101                 
044500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
044600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
044700     ELSE                                                                 
044800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10101                  
044900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
045000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045100     END-IF                                                               
045200                                                                          
045300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
045400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
045500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
045600                                                                          
045700     MOVE LOW-VALUE TO MSG-AREA                                           
045800     MOVE 'W6O101N1' TO MFS-IDMOD                                         
045900     MOVE '6101' TO MOD-IDTRANS                                           
046000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
046100                             MOD-TEMFSINF                                 
046200     IF EGEN-MID OR HELP-MID                                              
046300       CONTINUE                                                           
046400     ELSE                                                                 
046500       MOVE SPACE TO MFS-KDTRTYP                                          
046600       MOVE '7' TO MFS-IDPFK                                              
046700     END-IF                                                               
046800                                                                          
046900     PERFORM AA-INIT-NYCKLAR                                              
047000     IF MSGI-IDLAND-SPR = 'GB'                                            
047100       MOVE +2    TO SPRAK-IX                                             
047200       MOVE 'GB ' TO MED-IDSKYLT                                          
047300     ELSE                                                                 
047400       MOVE +1    TO SPRAK-IX                                             
047500       MOVE 'S  ' TO MED-IDSKYLT                                          
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900*----------------------------------------------------------------*        
048000 AA-INIT-NYCKLAR SECTION.                                                 
048100                                                                          
048200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
048300     MOVE '001'                  TO MSGI-KDCALL                           
048400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
048500     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
048600     MOVE '6101'                 TO MSGI-IDTRANS                          
048700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048800     .                                                                    
048900     EJECT                                                                
049000 B-KOLLA-NYCKLAR SECTION.                                                 
049100                                                                          
049200     IF GODK-MID OR HELP-MID                                              
049300       MOVE JA TO NYCKLAR-SW                                              
049400       MOVE EN-VAGN TO VAGN-SW                                            
049500       MOVE NEJ TO UPPDAT-SW                                              
049600                                                                          
049700       MOVE MFS-RENSA-FAELT   TO MOD-IDINLVGN-IN                          
049800       MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                              
049900                                                                          
050000       IF MID-IDINLVGN-IN = ALL '+'                                       
050100         MOVE MID-IDINLVGN-UT TO WS-IDINLVGN                              
050200         INSPECT WS-IDINLVGN REPLACING LEADING SPACE BY ZERO              
050300                                                                          
050400       ELSE                                                               
050500         MOVE MID-IDINLVGN-IN TO WS-IDINLVGN                              
050600         IF MID-IDINLVGN-IN = ALL SPACE                                   
050700           INSPECT WS-IDINLVGN REPLACING LEADING SPACE BY ZERO            
050800         END-IF                                                           
050900         MOVE '7'             TO MFS-IDPFK                                
051000         MOVE SPACE           TO MFS-KDTRTYP                              
051100       END-IF                                                             
051200                                                                          
051300       IF WS-IDINLVGN NUMERIC                                             
051400         IF WS-IDINLVGN > ZERO                                            
051500           MOVE WS-IDINLVGN  TO W-SEQF-IDINLVGN                           
051600         ELSE                                                             
051700           MOVE ZERO         TO W-SEQF-IDINLVGN                           
051800           MOVE FLERA-VAGNAR TO VAGN-SW                                   
051900         END-IF                                                           
052000       ELSE                                                               
052100         MOVE NEJ TO NYCKLAR-SW                                           
052200       END-IF                                                             
052300                                                                          
052400       IF MID-IDDC-IN         = ALL '+'                                   
052500         MOVE MSGI-IDDC       TO W-IDDC-B6                                
052600                                                                          
052700       ELSE                                                               
052800         MOVE MID-IDDC-IN     TO W-IDDC-B6                                
052900         MOVE '7'             TO MFS-IDPFK                                
053000         MOVE SPACE           TO MFS-KDTRTYP                              
053100       END-IF                                                             
053200       PERFORM IMS-GU-WDB601                                              
053300                                                                          
053400       IF DCS-KDDC = SPACE OR DCS-DDC                                     
053500           MOVE NEJ TO NYCKLAR-SW                                         
053600       ELSE                                                               
053700           MOVE DCS-IDDC     TO W-IDDC                                    
053800                                W-IDDC-6005                               
053900       END-IF                                                             
054000                                                                          
054100       IF GODK-MID OR NYCKLAR-OK                                          
054200         MOVE WS-IDINLVGN     TO MOD-IDINLVGN-UT                          
054300         INSPECT MOD-IDINLVGN-UT REPLACING LEADING ZERO BY SPACE          
054400         MOVE DCS-IDDC        TO MOD-IDDC-UT                              
054500         INSPECT MOD-IDDC-UT     REPLACING LEADING ZERO BY SPACE          
054600       ELSE                                                               
054700         MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-UT                          
054800         MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                              
054900       END-IF                                                             
055000                                                                          
055100       IF NYCKLAR-FEL                                                     
055200         MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                             
055300         CALL WMEDKONV USING MED-WMEDAREA                                 
055400         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
055500         PERFORM MFS-RENSA-FAELT-IN                                       
055600         PERFORM MFS-RENSA-FAELT-UT                                       
055700       END-IF                                                             
055800                                                                          
055900     ELSE                                                                 
056000       MOVE NEJ               TO NYCKLAR-SW                               
056100       MOVE MFS-RENSA-FAELT   TO MOD-IDINLVGN-IN                          
056200                                 MOD-IDDC-IN                              
056300       PERFORM MFS-RENSA-FAELT-IN                                         
056400       PERFORM MFS-RENSA-FAELT-UT                                         
056500     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800 C-FOERSTA-SIDAN SECTION.                                                 
056900                                                                          
057000     IF MANGA-VAGNAR                                                      
057100       MOVE ZERO TO W-SEQF-IDINLVGN                                       
057200       MOVE INFO-FOERSTA-SIDAN TO MED-IDMFSFEL                            
057300       CALL WMEDKONV USING MED-WMEDAREA                                   
057400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
057500     END-IF                                                               
057600                                                                          
057700     PERFORM MFS-RENSA-FAELT-IN                                           
057800     .                                                                    
057900     EJECT                                                                
058000 D-NAESTA-SIDAN SECTION.                                                  
058100                                                                          
058200     IF MANGA-VAGNAR                                                      
058300       IF MID-IDINLVGN-NEXT = ZERO                                        
058400                                                                          
058410         ADD +1 TO RAD-INDX                                               
058500         PERFORM MFS-RENSA-BILD                                           
058600         MOVE ZERO TO MOD-IDINLVGN-ENTER                                  
058700         MOVE FELM-SISTA-SIDAN-VISAD TO MED-IDMFSFEL                      
058800         CALL WMEDKONV USING MED-WMEDAREA                                 
058900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
059000         MOVE NEJ TO ALLT-SW                                              
059100                                                                          
059200       ELSE                                                               
059300         MOVE MID-IDINLVGN-NEXT TO W-SEQF-IDINLVGN-MIN                    
059400       END-IF                                                             
059500     END-IF                                                               
059600                                                                          
059700     PERFORM MFS-RENSA-FAELT-IN                                           
059800     .                                                                    
059900     EJECT                                                                
060000 E-SAMMA-SIDA SECTION.                                                    
060100                                                                          
060200     IF EGEN-MID OR HELP-MID                                              
060300                                                                          
060400       IF MANGA-VAGNAR                                                    
060500         IF MID-IDINLVGN-ENTER = ZERO                                     
060600           MOVE NEJ TO ALLT-SW                                            
060700         ELSE                                                             
060800           MOVE MID-IDINLVGN-ENTER TO W-SEQF-IDINLVGN-MIN                 
060900         END-IF                                                           
061000       END-IF                                                             
061100                                                                          
061200       IF MID-INPUT NOT = ALL '+'                                         
061300                                                                          
061400         MOVE INFO-TRYCK-PF11 TO MED-IDMFSFEL                             
061500         CALL WMEDKONV USING MED-WMEDAREA                                 
061600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
061700         PERFORM MFS-LAES-IN-IGEN                                         
061800         PERFORM EA-FLYTTA-MID-INDATA-TILL-MOD                            
061900                                                                          
062000       ELSE                                                               
062100         PERFORM MFS-RENSA-FAELT-IN                                       
062200       END-IF                                                             
062300                                                                          
062400     ELSE                                                                 
062500       PERFORM MFS-RENSA-FAELT-IN                                         
062600     END-IF                                                               
062700     .                                                                    
062800     EJECT                                                                
062900 EA-FLYTTA-MID-INDATA-TILL-MOD SECTION.                                   
063000                                                                          
063100     IF MID-KDCMDVAL-UPP NOT = ALL '+'                                    
063200       MOVE MID-KDCMDVAL-UPP TO MOD-KDCMDVAL-UPP                          
063300     ELSE                                                                 
063400       MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-UPP                          
063500     END-IF                                                               
063600                                                                          
063700     IF MID-IDINLVGN-UPP NOT = ALL '+'                                    
063800       MOVE MID-IDINLVGN-UPP TO MOD-IDINLVGN-UPP                          
063900     ELSE                                                                 
064000       MOVE MFS-RENSA-FAELT  TO MOD-IDINLVGN-UPP                          
064100     END-IF                                                               
064200                                                                          
064300     IF MID-ADINLOMR-UPP NOT = ALL '+'                                    
064400       MOVE MID-ADINLOMR-UPP TO MOD-ADINLOMR-UPP                          
064500     ELSE                                                                 
064600       MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR-UPP                          
064700     END-IF                                                               
064800                                                                          
064900     IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                                
065000       MOVE MID-ADINLOMR-NXT-UPP TO MOD-ADINLOMR-NXT-UPP                  
065100     ELSE                                                                 
065200       MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR-NXT-UPP                      
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 F-UPPDATERA SECTION.                                                     
065700                                                                          
065800     PERFORM FA-KOLLA-INPUT                                               
065900                                                                          
066000     IF INDATA-OK AND UPPDATERING-OK                                      
066100                                                                          
066200       PERFORM FB-BEARBETA-UPPDAT-VAL                                     
066300       IF 6197-IX > ZERO                                                  
066400         PERFORM S08-STARTA-6197-TRANS                                    
066500       END-IF                                                             
066600       IF TRANS-INDX > +0                                                 
066700         PERFORM S04-SKICKA-UPPF-TRANS-W60191                             
066800         MOVE ZERO TO TRANS-INDX                                          
066900       END-IF                                                             
067000                                                                          
067100       IF NOT OMSTART                                                     
067200         PERFORM FC-LYS-UPP-UPPDAT-FAELT                                  
067300       END-IF                                                             
067400                                                                          
067500     END-IF                                                               
067600     .                                                                    
067700     EJECT                                                                
067800 FA-KOLLA-INPUT SECTION.                                                  
067900                                                                          
068000     MOVE JA TO INDATA-SW                                                 
068100                 UPPDAT-SW                                                
068200                                                                          
068300     IF MID-INPUT = ALL '+'                                               
068400                                                                          
068500       MOVE FELM-PF11-O-TOM-INDATRAD TO MED-IDMFSFEL                      
068600       CALL WMEDKONV USING MED-WMEDAREA                                   
068700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
068800       MOVE NEJ TO UPPDAT-SW                                              
068900                                                                          
069000     ELSE                                                                 
069100       PERFORM FAA-KOLLA-INPUT-UPPDAT-RAD                                 
069200       IF INDATA-OK                                                       
069300                                                                          
069400         PERFORM FAB-KOLLA-PLAC-ADRESS                                    
069500                                                                          
069600       ELSE                                                               
069700         PERFORM S01-VISA-INPUTFEL                                        
069800         MOVE NEJ TO UPPDAT-SW                                            
069900       END-IF                                                             
070000                                                                          
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 FAA-KOLLA-INPUT-UPPDAT-RAD SECTION.                                      
070500                                                                          
070600     IF MID-KDCMDVAL-UPP NOT = ALL '+'                                    
070700       IF MID-KDCMDVAL-UPP = 'ÄPL' OR 'TOM' OR 'T' OR                     
070800                             'INL' OR 'I' OR                              
070900                             'LOC' OR 'EMP' OR 'BIN'                      
071000         IF (MID-KDCMDVAL-UPP     = 'ÄPL' OR 'LOC')   AND                 
071100            MID-ADINLOMR-UPP NOT = ALL '+' AND                            
071200            MID-ADINLOMR-NXT-UPP = ALL '+'                                
071300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-UPP-ATTR             
071400                                        MOD-ADINLOMR-UPP-ATTR             
071500                                        MOD-ADINLOMR-NXT-UPP-ATTR         
071600         ELSE                                                             
071700           IF (MID-KDCMDVAL-UPP         = 'ÄPL' OR 'LOC')   AND           
071800              MID-ADINLOMR-UPP     NOT = ALL '+' AND                      
071900              MID-ADINLOMR-NXT-UPP NOT = ALL '+'                          
072000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-UPP-ATTR           
072100                                          MOD-ADINLOMR-UPP-ATTR           
072200                                        MOD-ADINLOMR-NXT-UPP-ATTR         
072300           ELSE                                                           
072400             IF (MID-KDCMDVAL-UPP = 'T' OR 'TOM' OR 'EMP' OR              
072500                                        'I' OR 'INL' OR 'BIN') AND        
072600                (MID-ADINLOMR-UPP NOT = ALL '+')       AND                
072700                (MID-ADINLOMR-NXT-UPP = ALL '+')                          
072800               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-UPP-ATTR         
072900                                            MOD-ADINLOMR-UPP-ATTR         
073000                                         MOD-ADINLOMR-NXT-UPP-ATTR        
073100             ELSE                                                         
073200               IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                      
073300                 MOVE MFS-ALFA-FAELT-RAETT TO                             
073400                                           MOD-KDCMDVAL-UPP-ATTR          
073500                                           MOD-ADINLOMR-UPP-ATTR          
073600                 MOVE MFS-ALFA-FAELT-FEL   TO                             
073700                                        MOD-ADINLOMR-NXT-UPP-ATTR         
073800                 MOVE NEJ                  TO INDATA-SW                   
073900               ELSE                                                       
074000                 MOVE MFS-ALFA-FAELT-RAETT TO                             
074100                                        MOD-KDCMDVAL-UPP-ATTR             
074200                                        MOD-ADINLOMR-NXT-UPP-ATTR         
074300                 MOVE MFS-ALFA-FAELT-FEL   TO                             
074400                                        MOD-ADINLOMR-UPP-ATTR             
074500                 MOVE NEJ                  TO INDATA-SW                   
074600               END-IF                                                     
074700             END-IF                                                       
074800           END-IF                                                         
074900         END-IF                                                           
075000       ELSE                                                               
075100         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDCMDVAL-UPP-ATTR             
075200         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-ADINLOMR-UPP-ATTR             
075300                                        MOD-ADINLOMR-NXT-UPP-ATTR         
075400         MOVE NEJ                    TO INDATA-SW                         
075500       END-IF                                                             
075600                                                                          
075700     ELSE                                                                 
075800       IF (MID-ADINLOMR-UPP        = ALL '+' AND                          
075900          MID-ADINLOMR-NXT-UPP NOT = ALL '+') OR                          
076000          (MID-ADINLOMR-UPP    NOT = ALL '+' AND                          
076100          MID-ADINLOMR-NXT-UPP NOT = ALL '+')                             
076200         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDCMDVAL-UPP-ATTR             
076300                                        MOD-ADINLOMR-UPP-ATTR             
076400                                        MOD-ADINLOMR-NXT-UPP-ATTR         
076500       ELSE                                                               
076600         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDCMDVAL-UPP-ATTR             
076700         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-ADINLOMR-UPP-ATTR             
076800         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-ADINLOMR-NXT-UPP-ATTR         
076900         MOVE NEJ                    TO INDATA-SW                         
077000       END-IF                                                             
077100     END-IF                                                               
077200                                                                          
077300     IF MID-IDINLVGN-UPP NOT = ALL '+'                                    
077400       IF MID-IDINLVGN-UPP NOT NUMERIC                                    
077500         MOVE MFS-NUM-FAELT-FEL      TO MOD-IDINLVGN-UPP-ATTR             
077600         MOVE NEJ                    TO INDATA-SW                         
077700       ELSE                                                               
077800         PERFORM FAAA-KOLLA-VAGN-FINNS-PA-BILD                            
077900         PERFORM FAAB-KOLLA-VAGN-FINNS-PA-DB                              
078000         IF VAGN-FINNS                                                    
078100           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDINLVGN-UPP-ATTR             
078200         ELSE                                                             
078300           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDINLVGN-UPP-ATTR             
078400           MOVE NEJ                  TO INDATA-SW                         
078500         END-IF                                                           
078600       END-IF                                                             
078700     ELSE                                                                 
078800       IF MANGA-VAGNAR                                                    
078900         MOVE MFS-NUM-FAELT-FEL      TO MOD-IDINLVGN-UPP-ATTR             
079000         MOVE NEJ                    TO INDATA-SW                         
079100       ELSE                                                               
079200         MOVE WS-IDINLVGN TO W-SEQF-IDINLVGN                              
079300         PERFORM IMS-GU-W6D111-VAGN-UNIK                                  
079400         IF SEGMENT-FINNS                                                 
079500           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDINLVGN-UPP-ATTR           
079600         ELSE                                                             
079700           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDINLVGN-UPP-ATTR             
079800           MOVE NEJ                  TO INDATA-SW                         
079900         END-IF                                                           
080000       END-IF                                                             
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400                                                                          
080500 FAAA-KOLLA-VAGN-FINNS-PA-BILD SECTION.                                   
080600     MOVE +1 TO RAD-INDX                                                  
080700                                                                          
080800     PERFORM UNTIL RAD-INDX > MAX-RADER                                   
080900       MOVE MID-IDINLVGN(RAD-INDX) TO WS-IDINLVGN-SPAR                    
081000       INSPECT WS-IDINLVGN-SPAR REPLACING LEADING SPACE BY ZERO           
081100                                                                          
081200       IF MID-IDINLVGN-UPP = MID-IDINLVGN(RAD-INDX)                       
081300                                                                          
081400         IF MID-IDINLVGN-UPP NOT = ALL '+'                                
081500           MOVE MID-IDINLVGN-UPP TO W-SEQF-IDINLVGN                       
081600                                    W-SEQF-IDINLVGN-MIN                   
081700         END-IF                                                           
081800         MOVE MAX-RADER TO RAD-INDX                                       
081900         MOVE JA  TO BILDVAGN-SW                                          
082000       ELSE                                                               
082100         MOVE NEJ TO BILDVAGN-SW                                          
082200       END-IF                                                             
082300                                                                          
082400       ADD +1 TO RAD-INDX                                                 
082500     END-PERFORM                                                          
082600                                                                          
082700     MOVE ZERO TO WS-IDINLVGN-SPAR                                        
082800     .                                                                    
082900     EJECT                                                                
083000                                                                          
083100 FAAB-KOLLA-VAGN-FINNS-PA-DB SECTION.                                     
083200     PERFORM IMS-GU-W6D111-VAGN-UNIK                                      
083300     IF SEGMENT-FINNS                                                     
083400       MOVE JA  TO BILDVAGN-SW                                            
083500     ELSE                                                                 
083600       MOVE NEJ TO BILDVAGN-SW                                            
083700     END-IF                                                               
083800                                                                          
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 FAB-KOLLA-PLAC-ADRESS SECTION.                                           
084300     IF MID-ADINLOMR-UPP     NOT = ALL '+' AND                            
084400        MID-ADINLOMR-NXT-UPP NOT = ALL '+'                                
084500                                                                          
084600       IF MID-ADINLOMR-UPP = MID-ADINLOMR-NXT-UPP                         
084700         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-UPP-ATTR                 
084800         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-NXT-UPP-ATTR             
084900         MOVE NEJ                TO INDATA-SW                             
085000       END-IF                                                             
085100                                                                          
085200     ELSE                                                                 
085300       IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                              
085400         IF MANGA-VAGNAR                                                  
085500           MOVE MID-IDINLVGN-UPP TO W-SEQF-IDINLVGN                       
085600         END-IF                                                           
085700                                                                          
085800         PERFORM IMS-GU-W6D111-VAGN-UNIK                                  
085900         IF SEGMENT-FINNS                                                 
086000           MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                         
086100           PERFORM IMS-GNP-W6D121-KOLLI-FIRST                             
086200           IF MID-ADINLOMR-NXT-UPP = RAD-ADINLOMR                         
086300             MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-NXT-UPP-ATTR         
086400             MOVE NEJ                TO INDATA-SW                         
086500           END-IF                                                         
086600         ELSE                                                             
086700           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-NXT-UPP-ATTR           
086800           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDINLVGN-UPP-ATTR               
086900           MOVE NEJ                TO INDATA-SW                           
087000         END-IF                                                           
087100       END-IF                                                             
087200     END-IF                                                               
087300                                                                          
087400     IF INDATA-OK                                                         
087500       PERFORM FABA-KOLLA-PLAC-ADRESS-FINNS                               
087600       PERFORM S50-PRIM-CONTROL                                           
087700       IF INDATA-OK                                                       
087800         CONTINUE                                                         
087900       ELSE                                                               
088000         PERFORM S01-VISA-INPUTFEL                                        
088100         MOVE NEJ TO UPPDAT-SW                                            
088200       END-IF                                                             
088300     ELSE                                                                 
088400       PERFORM S01-VISA-INPUTFEL                                          
088500       MOVE NEJ TO UPPDAT-SW                                              
088600     END-IF                                                               
088700     .                                                                    
088800     EJECT                                                                
088900                                                                          
089000 FABA-KOLLA-PLAC-ADRESS-FINNS SECTION.                                    
089100     IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                                
089200       MOVE MID-ADINLOMR-NXT-UPP TO W-ADINLOMR-6006                       
089300       PERFORM IMS-GU-W6G130-PLACERING                                    
089400                                                                          
089500       IF SEGMENT-SAKNAS                                                  
089600         MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                     
089700         CALL WMEDKONV USING MED-WMEDAREA                                 
089800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
089900                                                                          
090000         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-NXT-UPP-ATTR             
090100         MOVE NEJ                TO UPPDAT-SW                             
090200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
090300       END-IF                                                             
090400                                                                          
090500     END-IF                                                               
090600                                                                          
090700     IF MID-ADINLOMR-UPP NOT = ALL '+'                                    
090800       MOVE MID-ADINLOMR-UPP TO W-ADINLOMR-6006                           
090900       PERFORM IMS-GU-W6G130-PLACERING                                    
091000                                                                          
091100       IF SEGMENT-SAKNAS                                                  
091200         MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                     
091300         CALL WMEDKONV USING MED-WMEDAREA                                 
091400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
091500                                                                          
091600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-UPP-ATTR                 
091700         MOVE NEJ                TO UPPDAT-SW                             
091800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
091900       END-IF                                                             
092000                                                                          
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400 FB-BEARBETA-UPPDAT-VAL SECTION.                                          
092500                                                                          
092600     IF MID-KDCMDVAL-UPP = ALL '+' OR                                     
092700                           'ÄPL' OR 'LOC'                                 
092800       IF MID-ADINLOMR-UPP     NOT = ALL '+' AND                          
092900          MID-ADINLOMR-NXT-UPP     = ALL '+'                              
093000         PERFORM FBA-NY-PLACERING                                         
093100                                                                          
093200       ELSE                                                               
093300         IF (MID-ADINLOMR-UPP       = ALL '+' AND                         
093400             MID-ADINLOMR-NXT-UPP NOT = ALL '+'  ) OR                     
093500            (MID-ADINLOMR-UPP   NOT = ALL '+' AND                         
093600             MID-ADINLOMR-NXT-UPP NOT = ALL '+'  )                        
093700           PERFORM FBB-NY-ADRESS-NY-PLACERING                             
093800                                                                          
093900         END-IF                                                           
094000       END-IF                                                             
094100                                                                          
094200     ELSE                                                                 
094300       IF MID-KDCMDVAL-UPP = 'TOM' OR 'T' OR 'EMP'                        
094400         PERFORM FBC-TOEMMA-VAGN                                          
094500                                                                          
094600       ELSE                                                               
094700         IF MID-KDCMDVAL-UPP = 'INL' OR 'I' OR 'BIN'                      
094800           PERFORM FBD-LAEGG-IN-VAGN                                      
094900                                                                          
095000         END-IF                                                           
095100       END-IF                                                             
095200     END-IF                                                               
095300     .                                                                    
095400     EJECT                                                                
095500 FBA-NY-PLACERING SECTION.                                                
095600                                                                          
095700     MOVE NEJ TO KOLLI-SW                                                 
095800     MOVE ZERO TO TRANS-INDX                                              
095900                                                                          
096000     PERFORM IMS-GU-W6D111-VAGN-UNIK                                      
096100                                                                          
096200     PERFORM FBAA-UPPDAT-NY-PLAC                                          
096300                                                                          
096400     .                                                                    
096500     EJECT                                                                
096600                                                                          
096700 FBAA-UPPDAT-NY-PLAC SECTION.                                             
096800     PERFORM UNTIL KOLLI-SLUT                                             
096900       IF TRANS-INDX NOT > MAX-KVPOST                                     
097000         IF SEGMENT-FINNS                                                 
097100           IF ART-IDLOPNRM NOT = WS-IDLOPNRM-SPAR                         
097200                                                                          
097300             MOVE ART-IDLOPNRM TO WS-IDLOPNRM-SPAR                        
097400             MOVE ART-BEFT     TO WS-BEFT                                 
097500             MOVE ART-IDARTNR  TO WS-IDARTNR                              
097600             IF ART-KVAVIS-PRIO > ZERO                                    
097700               MOVE JA TO PRIOGODS-SW                                     
097800             END-IF                                                       
097900                                                                          
098000                                                                          
098100             MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                       
098200             PERFORM IMS-GHNP-W6D121-KOLLI                                
098300                                                                          
098400             PERFORM UNTIL SEGMENT-SAKNAS OR                              
098500                           TRANS-INDX > MAX-KVPOST                        
098600              IF RAD-ADINLOMR     = MID-ADINLOMR-UPP AND                  
098700                 RAD-ADINLOMR-NXT = SPACE                                 
098800               CONTINUE                                                   
098900              ELSE                                                        
099000               PERFORM S02-SPARA-FAELT-W6D1                               
099100               MOVE MID-ADINLOMR-UPP TO RAD-ADINLOMR                      
099200               MOVE SPACE        TO RAD-ADINLOMR-NXT                      
099300               PERFORM IMS-REPL-W6D121-KOLLI                              
099400                                                                          
099500               ADD +1 TO TRANS-INDX                                       
099600               PERFORM S03-SKAPA-UPPF-TRANS-W60191                        
099700              END-IF                                                      
099800              PERFORM IMS-GHNP-W6D121-KOLLI                               
099900             END-PERFORM                                                  
100000                                                                          
100100             PERFORM IMS-GN-W6D111-VAGN                                   
100200           ELSE                                                           
100300             PERFORM IMS-GN-W6D111-VAGN                                   
100400           END-IF                                                         
100500         END-IF                                                           
100600                                                                          
100700       ELSE                                                               
100800         PERFORM S04-SKICKA-UPPF-TRANS-W60191                             
100900         MOVE ZERO TO TRANS-INDX                                          
101000         MOVE ZERO TO WS-IDLOPNRM-SPAR                                    
101100       END-IF                                                             
101200                                                                          
101300       PERFORM S05-KOLLA-OM-KOLLI-FINNS                                   
101400                                                                          
101500     END-PERFORM                                                          
101600     .                                                                    
101700     EJECT                                                                
101800                                                                          
101900 FBB-NY-ADRESS-NY-PLACERING SECTION.                                      
102000     MOVE JA  TO ALLT-SW                                                  
102100     MOVE NEJ TO KOLLI-SW                                                 
102200     MOVE ZERO TO TRANS-INDX                                              
102300                                                                          
102400     PERFORM IMS-GU-W6D111-VAGN-UNIK                                      
102500                                                                          
102600     PERFORM FBBA-UPPDAT-NY-ADR-NY-PLAC                                   
102700     .                                                                    
102800     EJECT                                                                
102900                                                                          
103000 FBBA-UPPDAT-NY-ADR-NY-PLAC SECTION.                                      
103100     PERFORM UNTIL KOLLI-SLUT                                             
103200       IF TRANS-INDX NOT > MAX-KVPOST                                     
103300         IF SEGMENT-FINNS                                                 
103400           IF ART-IDLOPNRM NOT = WS-IDLOPNRM-SPAR                         
103500                                                                          
103600             MOVE ART-IDLOPNRM    TO WS-IDLOPNRM-SPAR                     
103700             MOVE ART-BEFT     TO WS-BEFT                                 
103800             MOVE ART-IDARTNR  TO WS-IDARTNR                              
103900             IF ART-KVAVIS-PRIO > ZERO                                    
104000               MOVE JA TO PRIOGODS-SW                                     
104100             END-IF                                                       
104200                                                                          
104300             MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                       
104400             PERFORM IMS-GHNP-W6D121-KOLLI                                
104500                                                                          
104600             PERFORM UNTIL SEGMENT-SAKNAS OR                              
104700                           TRANS-INDX > MAX-KVPOST                        
104800              IF RAD-ADINLOMR-NXT = MID-ADINLOMR-NXT-UPP AND              
104900                (RAD-ADINLOMR = MID-ADINLOMR-UPP OR                       
105000                 MID-ADINLOMR-UPP = ALL '+')                              
105100               CONTINUE                                                   
105200              ELSE                                                        
105300               PERFORM S02-SPARA-FAELT-W6D1                               
105400               MOVE MID-ADINLOMR-NXT-UPP TO RAD-ADINLOMR-NXT              
105500               IF MID-ADINLOMR-UPP NOT = ALL '+'                          
105600                 MOVE MID-ADINLOMR-UPP TO RAD-ADINLOMR                    
105700               END-IF                                                     
105800               PERFORM IMS-REPL-W6D121-KOLLI                              
105900                                                                          
106000               ADD +1 TO TRANS-INDX                                       
106100               PERFORM S03-SKAPA-UPPF-TRANS-W60191                        
106200              END-IF                                                      
106300              PERFORM IMS-GHNP-W6D121-KOLLI                               
106400             END-PERFORM                                                  
106500                                                                          
106600             PERFORM IMS-GN-W6D111-VAGN                                   
106700           ELSE                                                           
106800             PERFORM IMS-GN-W6D111-VAGN                                   
106900           END-IF                                                         
107000         END-IF                                                           
107100                                                                          
107200       ELSE                                                               
107300         PERFORM S04-SKICKA-UPPF-TRANS-W60191                             
107400         MOVE ZERO TO TRANS-INDX                                          
107500         MOVE ZERO TO WS-IDLOPNRM-SPAR                                    
107600       END-IF                                                             
107700                                                                          
107800       PERFORM S05-KOLLA-OM-KOLLI-FINNS                                   
107900                                                                          
108000     END-PERFORM                                                          
108100     .                                                                    
108200     EJECT                                                                
108300                                                                          
108400 FBC-TOEMMA-VAGN SECTION.                                                 
108500     MOVE NEJ TO KOLLI-SW                                                 
108600     MOVE +1  TO TRANS-INDX                                               
108700                                                                          
108800     PERFORM IMS-GU-W6D111-VAGN-UNIK                                      
108900     MOVE ART-IDLOPNRM TO WS-IDLOPNRM-SPAR                                
109000     MOVE ART-BEFT     TO WS-BEFT                                         
109100     MOVE ART-IDARTNR  TO WS-IDARTNR                                      
109200     IF ART-KVAVIS-PRIO > ZERO                                            
109300       MOVE JA TO PRIOGODS-SW                                             
109400     END-IF                                                               
109500                                                                          
109600     MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                               
109700     PERFORM IMS-GHNP-W6D121-KOLLI                                        
109800                                                                          
109900     PERFORM FBCA-UPPDAT-TOM-VAGN                                         
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300 FBCA-UPPDAT-TOM-VAGN SECTION.                                            
110400     PERFORM UNTIL KOLLI-SLUT                                             
110500       IF TRANS-INDX NOT > MAX-KVPOST                                     
110600         IF SEGMENT-FINNS                                                 
110700                                                                          
110800           PERFORM S02-SPARA-FAELT-W6D1                                   
110900           MOVE MID-ADINLOMR-UPP TO RAD-ADINLOMR                          
111000           MOVE SPACE            TO RAD-ADINLOMR-NXT                      
111100           MOVE ZERO             TO RAD-IDINLVGN                          
111200           PERFORM IMS-REPL-W6D121-KOLLI                                  
111300                                                                          
111400           PERFORM S03-SKAPA-UPPF-TRANS-W60191                            
111500                                                                          
111600           PERFORM IMS-GU-W6D111-VAGN-UNIK                                
111700             IF SEGMENT-FINNS                                             
111800             MOVE ART-IDLOPNRM TO WS-IDLOPNRM-SPAR                        
111900             MOVE ART-BEFT     TO WS-BEFT                                 
112000             MOVE ART-IDARTNR  TO WS-IDARTNR                              
112100                                                                          
112200             IF ART-KVAVIS-PRIO > ZERO                                    
112300               MOVE JA TO PRIOGODS-SW                                     
112400             END-IF                                                       
112500             MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                       
112600             PERFORM IMS-GHNP-W6D121-KOLLI                                
112700           END-IF                                                         
112800         END-IF                                                           
112900                                                                          
113000       ELSE                                                               
113100         PERFORM S04-SKICKA-UPPF-TRANS-W60191                             
113200         MOVE ZERO TO TRANS-INDX                                          
113300       END-IF                                                             
113400                                                                          
113500       PERFORM S05-KOLLA-OM-KOLLI-FINNS                                   
113600                                                                          
113700       ADD +1 TO TRANS-INDX                                               
113800     END-PERFORM                                                          
113900     .                                                                    
114000     EJECT                                                                
114100                                                                          
114200 FBD-LAEGG-IN-VAGN SECTION.                                               
114300     MOVE NEJ TO KOLLI-SW                                                 
114400     MOVE ZERO TO TRANS-INDX                                              
114500                                                                          
114600     PERFORM IMS-GU-W6D111-VAGN-UNIK                                      
114700     MOVE ART-FLKVAKAR TO WS-FLKVAKAR                                     
114800     MOVE ART-FLKVAFEL TO WS-FLKVAFEL                                     
114900     MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                               
115000     PERFORM IMS-GHNP-W6D121-KOLLI                                        
115100                                                                          
115200     PERFORM FBDA-UPPDAT-INLAEGGN-AV-VAGN                                 
115300     .                                                                    
115400     EJECT                                                                
115500                                                                          
115600 FBDA-UPPDAT-INLAEGGN-AV-VAGN SECTION.                                    
115700     PERFORM UNTIL KOLLI-SLUT OR OMSTART                                  
115800       IF TRANS-INDX NOT > MAX-KVPOST                                     
115900         IF SEGMENT-FINNS                                                 
116000                                                                          
116100           IF WS-FLKVAKAR = NEJ AND WS-FLKVAFEL = NEJ                     
116200             PERFORM S02-SPARA-FAELT-W6D1                                 
116300             MOVE SPACE      TO RAD-ADINLOMR                              
116400             MOVE SPACE      TO RAD-ADINLOMR-NXT                          
116500             MOVE ZERO       TO RAD-IDINLVGN                              
116600             MOVE 'INL'      TO RAD-KDINLSTA                              
116700             PERFORM IMS-REPL-W6D121-KOLLI                                
116800             PERFORM FBDAA-AVSLUTA-PARTI-W60193                           
116900             ADD +1 TO TRANS-INDX                                         
117000             PERFORM S03-SKAPA-UPPF-TRANS-W60191                          
117100           ELSE                                                           
117200             MOVE JA TO KVAL-SW                                           
117300           END-IF                                                         
117400                                                                          
117500           PERFORM IMS-GN-W6D111-VAGN                                     
117600           IF SEGMENT-FINNS                                               
117700             MOVE ART-FLKVAKAR TO WS-FLKVAKAR                             
117800             MOVE ART-FLKVAFEL TO WS-FLKVAFEL                             
117900             MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                       
118000             PERFORM IMS-GHNP-W6D121-KOLLI                                
118100           END-IF                                                         
118200         END-IF                                                           
118300       ELSE                                                               
118400         IF TRANS-INDX > ZERO                                             
118500           PERFORM S04-SKICKA-UPPF-TRANS-W60191                           
118600           MOVE ZERO TO TRANS-INDX                                        
118700         END-IF                                                           
118800       END-IF                                                             
118900                                                                          
119000       IF 6193-TRANS-RAKN > MAX-TRANSAR                                   
119100         PERFORM S04-SKICKA-UPPF-TRANS-W60191                             
119200         PERFORM FBDAB-FORBERED-OMSTART                                   
119300       ELSE                                                               
119400         PERFORM S05-KOLLA-OM-KOLLI-FINNS                                 
119500       END-IF                                                             
119600     END-PERFORM                                                          
119700     .                                                                    
119800     EJECT                                                                
119900                                                                          
120000 FBDAA-AVSLUTA-PARTI-W60193 SECTION.                                      
120100     MOVE ART-IDLOPNRM       TO 6193-MID-IDLOPNRM                         
120200     MOVE RAD-IDRADNR        TO 6193-MID-IDRADNR                          
120300                                                                          
120400     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
120500     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
120600     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
120700     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
120800     MOVE SPACE                TO MSG-KOM-KDTRANS                         
120900     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
121000     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
121100     MOVE 'W6010100'           TO MSG-KOM-IDSNDJOB                        
121200     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
121300     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
121400     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
121500                                                                          
121600     MOVE +29                  TO KMSG-KVLL                               
121700*****CTEXTLÄNGD + KOM-MSG-AREA                                            
121800     MOVE 'W6T193X '           TO KMSG-KDTRANS-1                          
121900     MOVE '6101'               TO KMSG-IDTRANS-1                          
122000     MOVE MFS-KDMFSFOR         TO KMSG-KDMFSFOR-1                         
122100     ADD +1                    TO 6193-TRANS-RAKN                         
122200                                                                          
122300     CALL W006KOM USING MSG-PCB                                           
122400                        DISP-PCB                                          
122500                        KOM-KOMA-PCB                                      
122600                        MSG-KOM-WMSGKOM                                   
122700                        KMSG-IO-AREA                                      
122800                                                                          
122900     .                                                                    
123000     EJECT                                                                
123100                                                                          
123200 FBDAB-FORBERED-OMSTART SECTION.                                          
123300     PERFORM IMS-GN-W6D111-VAGN                                           
123400     IF SEGMENT-FINNS                                                     
123500       MOVE JA TO OMSTART-SW                                              
123600     ELSE                                                                 
123700       MOVE JA TO KOLLI-SW                                                
123800     END-IF                                                               
123900                                                                          
124000     .                                                                    
124100     EJECT                                                                
124200                                                                          
124300 FC-LYS-UPP-UPPDAT-FAELT SECTION.                                         
124400     IF MID-KDCMDVAL-UPP = 'INL' OR 'I' OR                                
124500                           'TOM' OR 'T' OR                                
124600                           'EMP' OR 'BIN'                                 
124700       CONTINUE                                                           
124800     ELSE                                                                 
124900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDINLVGN-ATTR(1)                 
125000                                                                          
125100       IF MID-ADINLOMR-UPP NOT = ALL '+'                                  
125200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADINLOMR-ATTR(1)               
125300       END-IF                                                             
125400                                                                          
125500       IF MID-ADINLOMR-NXT-UPP NOT = ALL '+'                              
125600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADINLOMR-NXT-ATTR(1)           
125700       END-IF                                                             
125800     END-IF                                                               
125900                                                                          
126000     .                                                                    
126100     EJECT                                                                
126200 G-LAES-VISA-BILD SECTION.                                                
126300                                                                          
126400     IF ALLT-OK                                                           
126500                                                                          
126600       MOVE +1 TO RAD-INDX                                                
126700                                                                          
126800       IF MANGA-VAGNAR                                                    
126900         PERFORM GA-VISA-ALLA-VAGNAR                                      
127000                                                                          
127100       ELSE                                                               
127200         PERFORM IMS-GU-W6D111-VAGN-UNIK                                  
127300         IF SEGMENT-FINNS                                                 
127400           MOVE W-SEQF-IDINLVGN TO MOD-IDINLVGN-ENTER                     
127500                                   W-RAD-IDINLVGN                         
127600           PERFORM IMS-GNP-W6D121-KOLLI-FIRST                             
127700           PERFORM S06-FLYTTA-TILL-MOD                                    
127800                                                                          
127900           ADD +1 TO RAD-INDX                                             
128000           PERFORM MFS-RENSA-BILD                                         
128100                                                                          
128200         ELSE                                                             
128300           MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                             
128400           CALL WMEDKONV USING MED-WMEDAREA                               
128500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
128600         END-IF                                                           
128700       END-IF                                                             
128800                                                                          
128900       IF UPPDATERING-OK                                                  
129000         IF KVAL-FEL                                                      
129100           MOVE INFO-KVALFEL  TO MED-IDMFSINF                             
129200           CALL WMEDKONV USING MED-WMEDAREA                               
129300           MOVE MED-MFSINF TO MOD-TEMFSINF                                
129400           MOVE INFO-KOLLI-KVAR TO MED-IDMFSFEL                           
129500           CALL WMEDKONV USING MED-WMEDAREA                               
129600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
129700         ELSE                                                             
129800           MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                         
129900           CALL WMEDKONV USING MED-WMEDAREA                               
130000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
130100           IF MID-KDCMDVAL-UPP = 'INL' OR 'I' OR                          
130200                                 'TOM' OR 'T' OR                          
130300                                 'EMP' OR 'BIN'                           
130400             MOVE SPACE TO MOD-TEMFSFEL                                   
130500           END-IF                                                         
130600         END-IF                                                           
130700         PERFORM MFS-FORM-ATTR                                            
130800         PERFORM MFS-RENSA-FAELT-IN                                       
130900       END-IF                                                             
131000     END-IF                                                               
131100     .                                                                    
131200     EJECT                                                                
131300 GA-VISA-ALLA-VAGNAR SECTION.                                             
131400                                                                          
131500     PERFORM IMS-GN-W6D1F1-VAGN-FIRST                                     
131600     IF SEGMENT-FINNS                                                     
131700       PERFORM UNTIL SEGMENT-SAKNAS       OR                              
131800                     BASEN-SLUT           OR                              
131900                     RAD-INDX > MAX-RADER                                 
132000         IF SEQF-IDINLVGN = WS-IDINLVGN-SPAR                              
132100                                                                          
132200           MOVE SEQF-IDINLVGN TO W-SEQF-IDINLVGN-MIN                      
132300           PERFORM UNTIL SEGMENT-SAKNAS   OR                              
132400                         BASEN-SLUT       OR                              
132500                         SEQF-IDINLVGN NOT = WS-IDINLVGN-SPAR             
132600             PERFORM IMS-GN-W6D1F1-VAGN                                   
132700           END-PERFORM                                                    
132800                                                                          
132900         ELSE                                                             
133000           IF RAD-INDX = +1                                               
133100             MOVE SEQF-IDINLVGN TO MOD-IDINLVGN-ENTER                     
133200           END-IF                                                         
133300                                                                          
133400           MOVE SEQF-IDINLVGN TO WS-IDINLVGN-SPAR                         
133500           PERFORM GAA-LAES-W6D121-FOERSTA-KOLLI                          
133600           PERFORM S06-FLYTTA-TILL-MOD                                    
133700           PERFORM IMS-GN-W6D1F1-VAGN                                     
133800           ADD +1 TO RAD-INDX                                             
133900         END-IF                                                           
134000       END-PERFORM                                                        
134100                                                                          
134200       IF RAD-INDX NOT > MAX-RADER                                        
134210         IF RAD-INDX = ZERO                                               
134211           MOVE +1 TO  RAD-INDX                                           
134220         END-IF                                                           
134300         PERFORM MFS-RENSA-BILD                                           
134400       END-IF                                                             
134500       PERFORM GAB-KOLLA-OM-FLER-SIDOR                                    
134600                                                                          
134700     ELSE                                                                 
134800       MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                                 
134900       CALL WMEDKONV USING MED-WMEDAREA                                   
135000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
135100     END-IF                                                               
135200     .                                                                    
135300     EJECT                                                                
135400 GAA-LAES-W6D121-FOERSTA-KOLLI SECTION.                                   
135500                                                                          
135600     MOVE SEQF-IDLEVNR      TO W-INL-IDLEVNR                              
135700     MOVE SEQF-IDFS         TO W-INL-IDFS                                 
135800     MOVE SEQF-TIAVIDAT     TO W-INL-TIAVIDAT                             
135900     MOVE W-IDDC            TO W-INL-IDDC                                 
136000                                                                          
136100     MOVE SEQF-IDRADNR-INL  TO W-ART-IDRADNR-INL                          
136200                                                                          
136300     MOVE SEQF-IDRADNR      TO W-RAD-IDRADNR                              
136400                                                                          
136500     PERFORM IMS-GU-W6D121-KOLLI-FIRST                                    
136600                                                                          
136700     MOVE SEQF-IDINLVGN     TO W-SEQF-IDINLVGN-MIN                        
136800     .                                                                    
136900     EJECT                                                                
137000 GAB-KOLLA-OM-FLER-SIDOR SECTION.                                         
137100                                                                          
137200     IF SEGMENT-FINNS                                                     
137300       IF SEQF-IDINLVGN = WS-IDINLVGN-SPAR                                
137400                                                                          
137500         MOVE SEQF-IDINLVGN TO W-SEQF-IDINLVGN-MIN                        
137600         PERFORM UNTIL SEGMENT-SAKNAS   OR                                
137700                       BASEN-SLUT       OR                                
137800                       SEQF-IDINLVGN NOT = WS-IDINLVGN-SPAR               
137900           PERFORM IMS-GN-W6D1F1-VAGN                                     
138000         END-PERFORM                                                      
138100       END-IF                                                             
138200     END-IF                                                               
138300                                                                          
138400     IF SEQF-IDINLVGN NOT = WS-IDINLVGN-SPAR AND                          
138500        SEGMENT-FINNS                                                     
138600       MOVE SEQF-IDINLVGN TO MOD-IDINLVGN-NEXT                            
138700       MOVE INFO-MER-INFO-FINNS-PF8 TO MED-IDMFSINF                       
138800       CALL WMEDKONV USING MED-WMEDAREA                                   
138900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
139000     ELSE                                                                 
139100       IF SEGMENT-SAKNAS                                                  
139200         MOVE ZERO TO MOD-IDINLVGN-NEXT                                   
139300         IF MFS-NEXT                                                      
139400           MOVE INFO-SISTA-SIDAN TO MED-IDMFSINF                          
139500           CALL WMEDKONV USING MED-WMEDAREA                               
139600           MOVE MED-MFSINF TO MOD-TEMFSINF                                
139700         END-IF                                                           
139800       END-IF                                                             
139900     END-IF                                                               
140000     .                                                                    
140100     EJECT                                                                
140200 S01-VISA-INPUTFEL SECTION.                                               
140300                                                                          
140400     MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                         
140500     CALL WMEDKONV USING MED-WMEDAREA                                     
140600     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
140700                                                                          
140800     IF MANGA-VAGNAR                                                      
140900       MOVE MID-IDINLVGN-ENTER TO W-SEQF-IDINLVGN-MIN                     
141000     END-IF                                                               
141100                                                                          
141200     PERFORM MFS-ROER-EJ-FAELT-IN                                         
141300                                                                          
141400     .                                                                    
141500     EJECT                                                                
141600 S02-SPARA-FAELT-W6D1 SECTION.                                            
141700                                                                          
141800     MOVE RAD-ADINLOMR     TO SPAR-ADINLOMR                               
141900     MOVE RAD-ADINLOMR-NXT TO SPAR-ADINLOMR-NXT                           
142000     MOVE RAD-KDINLSTA     TO SPAR-KDINLSTA                               
142100     MOVE RAD-KVINLART     TO SPAR-KVINLART                               
142200     .                                                                    
142300     EJECT                                                                
142400 S03-SKAPA-UPPF-TRANS-W60191 SECTION.                                     
142500                                                                          
142600     IF TRANS-INDX <= MAX-KVPOST                                          
142700       MOVE ART-IDLOPNRM     TO 6191-MID-IDLOPNRM(TRANS-INDX)             
142800       MOVE RAD-IDRADNR      TO 6191-MID-IDRADNR(TRANS-INDX)              
142900       MOVE RAD-KDINLPRIO    TO 6191-MID-KDINLPRIO(TRANS-INDX)            
143000       MOVE ART-PRARTSTD     TO 6191-MID-PRARTSTD(TRANS-INDX)             
143100       MOVE +0               TO 6191-MID-KVKOLLI (TRANS-INDX)             
143200       MOVE 'N'              TO 6191-MID-FLINLI  (TRANS-INDX)             
143300       MOVE SPAR-ADINLOMR    TO 6191-MID-ADINLOMR-OLD(TRANS-INDX)         
143400       MOVE SPAR-ADINLOMR-NXT  TO                                         
143500                             6191-MID-ADINLOMR-NXT-OLD(TRANS-INDX)        
143600       MOVE SPAR-KDINLSTA    TO 6191-MID-KDINLSTA-OLD(TRANS-INDX)         
143700       MOVE SPAR-KVINLART    TO 6191-MID-KVINLART-OLD(TRANS-INDX)         
143800       MOVE RAD-ADINLOMR     TO 6191-MID-ADINLOMR-NEW(TRANS-INDX)         
143900       MOVE RAD-ADINLOMR-NXT TO                                           
144000                             6191-MID-ADINLOMR-NXT-NEW(TRANS-INDX)        
144100       MOVE RAD-KDINLSTA     TO 6191-MID-KDINLSTA-NEW(TRANS-INDX)         
144200       MOVE RAD-KVINLART     TO 6191-MID-KVINLART-NEW(TRANS-INDX)         
144210     END-IF                                                               
144300     .                                                                    
144400     EJECT                                                                
144500 S04-SKICKA-UPPF-TRANS-W60191 SECTION.                                    
144600                                                                          
144700     MOVE DCS-IDDC     TO 6191-MID-IDDC                                   
144800     MOVE IDPGM        TO 6191-MID-IDPGM                                  
144900     MOVE 'W6T191X  '  TO P-TO-P-MSG-KDTRANS                              
145000     MOVE '6101'       TO P-TO-P-MSG-IDTRANS                              
145100     MOVE MFS-KDMFSFOR TO P-TO-P-MSG-KDMFSFOR                             
145200                                                                          
145300     MOVE 6191-MID-W6I19101  TO P-TO-P-MSG-INDATA                         
145400                                                                          
145500     COMPUTE P-TO-P-MSG-KVLL = P-TO-P-PREFIX-LNG    +                     
145600                         MID-6191-FASTDEL-LNG +                           
145700                      (6191-MID-KVPOST * MID-6191-UPPF-POST-LNG)          
145800                                                                          
145900     IF FOERSTA-6191                                                      
146000         PERFORM IMS-ISRT-ALT-MSG-6191                                    
146100         MOVE NEJ              TO FOERSTA-6191-SW                         
146200      ELSE                                                                
146300         PERFORM IMS-PURG-ALT-MSG-6191                                    
146400     END-IF                                                               
146500     .                                                                    
146600     EJECT                                                                
146700 S05-KOLLA-OM-KOLLI-FINNS SECTION.                                        
146800                                                                          
146900     IF TRANS-INDX = ZERO AND                                             
147000        SEGMENT-SAKNAS                                                    
147100       MOVE JA TO KOLLI-SW                                                
147200     ELSE                                                                 
147300       IF TRANS-INDX NOT > MAX-KVPOST AND                                 
147400          KOLLI-EJ-SLUT               AND                                 
147500          SEGMENT-SAKNAS                                                  
147600         MOVE TRANS-INDX TO 6191-MID-KVPOST                               
147700         MOVE MAX-KVPOST TO TRANS-INDX                                    
147800         ADD +1          TO TRANS-INDX                                    
147900       ELSE                                                               
148000         IF TRANS-INDX = MAX-KVPOST AND                                   
148100            SEGMENT-SAKNAS                                                
148200           MOVE TRANS-INDX TO 6191-MID-KVPOST                             
148300         END-IF                                                           
148400       END-IF                                                             
148500     END-IF                                                               
148600     .                                                                    
148700     EJECT                                                                
148800 S06-FLYTTA-TILL-MOD SECTION.                                             
148900                                                                          
149000     MOVE RAD-IDINLVGN          TO MOD-IDINLVGN(RAD-INDX)                 
149100     MOVE RAD-ADINLOMR          TO MOD-ADINLOMR(RAD-INDX)                 
149200     MOVE RAD-ADINLOMR-NXT      TO MOD-ADINLOMR-NXT(RAD-INDX)             
149300     .                                                                    
149400     EJECT                                                                
149500 S08-STARTA-6197-TRANS  SECTION.                                          
149600                                                                          
149700     MOVE SPACE                 TO 6197-MID-IDPRTLST                      
149800     MOVE '6L'                  TO 6197-MID-IDPRTLST(1:2)                 
149900     MOVE 'TR  '                TO 6197-MID-IDPRTLST(3:4)                 
150000     MOVE IDPGM                 TO 6197-MID-IDPGM                         
150100     MOVE 6197-IX               TO 6197-MID-KVPOST                        
150200     MOVE 'N'                   TO 6197-MID-FLSVS                         
150300     COMPUTE P-TO-P-MSG-KVLL        = P-TO-P-PREFIX-LNG +                 
150400                                  26 + (6197-MID-KVPOST * 24)             
150500     MOVE 'W6T197X '           TO P-TO-P-MSG-KDTRANS                      
150600     MOVE '6101'               TO P-TO-P-MSG-IDTRANS                      
150700     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
150800     MOVE 6197-MID-W6I19701 TO P-TO-P-MSG-INDATA                          
150900     IF FOERSTA-6197                                                      
151000         PERFORM IMS-ISRT-ALT-MSG-6197                                    
151100         MOVE NEJ              TO FOERSTA-6197-SW                         
151200      ELSE                                                                
151300         PERFORM IMS-PURG-ALT-MSG-6197                                    
151400     END-IF                                                               
151500     MOVE ZERO                 TO 6197-IX                                 
151600     .                                                                    
151700     EJECT                                                                
151800                                                                          
151900 S50-PRIM-CONTROL SECTION.                                                
152000* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
152100* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
152200* WE CHECK THE NEW PLACE.                                                 
152300     MOVE W-SEQF-IDINLVGN TO W-RAD-IDINLVGN                               
152400     PERFORM IMS-GHNP-W6D121-KOLLI                                        
152500     MOVE RAD-ADINLOMR         TO W-ADINLOMR-6006                         
152600     PERFORM IMS-GU-W6G130-PLACERING                                      
152700     IF SEGMENT-FINNS                                                     
152800       IF 6006-FLKNTRGK = JA                                              
152900         MOVE MID-ADINLOMR-UPP TO W-ADINLOMR-6006                         
153000         PERFORM IMS-GU-W6G130-PLACERING                                  
153100         IF (6006-FLKNTRGK = JA)                                          
153200         OR 6006-KDINLOMR = 'LPL'                                         
153300           MOVE 'N' TO PRIM-CONTROL-SW                                    
153400         ELSE                                                             
153500           MOVE 'J' TO PRIM-CONTROL-SW                                    
153600         END-IF                                                           
153700       ELSE                                                               
153800         MOVE 'N' TO PRIM-CONTROL-SW                                      
153900       END-IF                                                             
154000     ELSE                                                                 
154100       MOVE 'N' TO PRIM-CONTROL-SW                                        
154200     END-IF                                                               
154300     IF PRIM-CONTROL-YES                                                  
154400       PERFORM S51-CHECK-OLD-PLACE                                        
154500     END-IF                                                               
154600     .                                                                    
154700     EJECT                                                                
154800                                                                          
154900 S51-CHECK-OLD-PLACE     SECTION.                                         
155000     MOVE ART-IDLOPNRM TO W-IDLOPNRM                                      
155100     PERFORM IMS-GU-UPFA01                                                
155200     IF SEGMENT-FINNS                                                     
155300       IF UPPF-KVKVAPRIM > ZERO                                           
155400         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
155500           CONTINUE                                                       
155600         ELSE                                                             
155700           MOVE '605' TO MED-IDMFSFEL                                     
155800           MOVE NEJ TO INDATA-SW                                          
155900         END-IF                                                           
156000       END-IF                                                             
156100       IF UPPF-KVKVASEK > ZERO                                            
156200         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
156300           CONTINUE                                                       
156400         ELSE                                                             
156500           MOVE '605' TO MED-IDMFSFEL                                     
156600           MOVE NEJ TO INDATA-SW                                          
156700         END-IF                                                           
156800       END-IF                                                             
156900     END-IF                                                               
157000                                                                          
157100     IF INDATA-OK                                                         
157200       PERFORM IMS-GU-UPFA01                                              
157300       IF SEGMENT-FINNS                                                   
157400         PERFORM IMS-GNP-UPFA11                                           
157500         PERFORM UNTIL SEGMENT-SAKNAS                                     
157600           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
157700             CONTINUE                                                     
157800           ELSE                                                           
157900             MOVE '605' TO MED-IDMFSFEL                                   
158000             MOVE NEJ TO INDATA-SW                                        
158100           END-IF                                                         
158200           PERFORM IMS-GNP-UPFA11                                         
158300         END-PERFORM                                                      
158400       END-IF                                                             
158500     END-IF                                                               
158600                                                                          
158700     IF INDATA-OK                                                         
158800       PERFORM IMS-GU-UPFA01                                              
158900       IF SEGMENT-FINNS                                                   
159000         PERFORM IMS-GNP-UPFA12                                           
159100         PERFORM UNTIL SEGMENT-SAKNAS                                     
159200           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
159300             CONTINUE                                                     
159400           ELSE                                                           
159500             MOVE '605' TO MED-IDMFSFEL                                   
159600             MOVE NEJ TO INDATA-SW                                        
159700           END-IF                                                         
159800           PERFORM IMS-GNP-UPFA12                                         
159900         END-PERFORM                                                      
160000       END-IF                                                             
160100     END-IF                                                               
160200     IF INDATA-OK                                                         
160300       CONTINUE                                                           
160400     ELSE                                                                 
160500       IF MED-IDMFSFEL = '605'                                            
160600         MOVE MFS-ADD-HILIGHT-FIELD TO MOD-ADINLOMR-UPP-ATTR              
160700       END-IF                                                             
160800     END-IF                                                               
160900     .                                                                    
161000     EJECT                                                                
161100                                                                          
161200 MFS-RENSA-FAELT-UT SECTION.                                              
161300                                                                          
161400*    --- ALLA UTDATA-FÄLT                                                 
161500     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-ENTER                           
161600                             MOD-IDINLVGN-NEXT                            
161700     MOVE +1              TO RAD-INDX                                     
161800     PERFORM UNTIL RAD-INDX > MAX-RADER                                   
161900       MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN(RAD-INDX)                     
162000                               MOD-ADINLOMR(RAD-INDX)                     
162100                               MOD-ADINLOMR-NXT(RAD-INDX)                 
162200       ADD +1 TO RAD-INDX                                                 
162300     END-PERFORM                                                          
162400     .                                                                    
162500     SKIP3                                                                
162600 MFS-RENSA-FAELT-IN SECTION.                                              
162700                                                                          
162800*    --- ALLA INDATA-FÄLT                                                 
162900     MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-UPP                             
163000                             MOD-IDINLVGN-UPP                             
163100                             MOD-ADINLOMR-UPP                             
163200                             MOD-ADINLOMR-NXT-UPP                         
163300     .                                                                    
163400     EJECT                                                                
163500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
163600                                                                          
163700*    --- ALLA INDATA-FÄLT                                                 
163800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-UPP                           
163900                               MOD-IDINLVGN-UPP                           
164000                               MOD-ADINLOMR-UPP                           
164100                               MOD-ADINLOMR-NXT-UPP                       
164200     .                                                                    
164300     SKIP3                                                                
164400 MFS-FORM-ATTR SECTION.                                                   
164500                                                                          
164600*    --- ALLA INDATA-FÄLT                                                 
164700     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-UPP-ATTR                     
164800                                MOD-IDINLVGN-UPP-ATTR                     
164900                                MOD-ADINLOMR-UPP-ATTR                     
165000                                MOD-ADINLOMR-NXT-UPP-ATTR                 
165100     .                                                                    
165200     EJECT                                                                
165300 MFS-LAES-IN-IGEN SECTION.                                                
165400                                                                          
165500*    --- ALLA INDATA-FÄLT                                                 
165600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-UPP-ATTR                  
165700                                   MOD-IDINLVGN-UPP-ATTR                  
165800                                   MOD-ADINLOMR-UPP-ATTR                  
165900                                   MOD-ADINLOMR-NXT-UPP-ATTR              
166000     .                                                                    
166100     SKIP3                                                                
166200 MFS-RENSA-BILD SECTION.                                                  
166300                                                                          
166400     PERFORM UNTIL RAD-INDX > MAX-RADER                                   
166500       MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN(RAD-INDX)                     
166600                               MOD-ADINLOMR(RAD-INDX)                     
166700                               MOD-ADINLOMR-NXT(RAD-INDX)                 
166800                                                                          
166900       ADD +1 TO RAD-INDX                                                 
167000     END-PERFORM                                                          
167100     .                                                                    
167200     EJECT                                                                
167300* --- IMS SEKTIONER ---                                                   
167400     SKIP3                                                                
167500 IMS-GET-MSG SECTION.                                                     
167600                                                                          
167700     MOVE '  QC' TO GODK-STATUSKODER                                      
167800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
167900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
168000     PERFORM IMS-STATUSKONTROLL                                           
168100     .                                                                    
168200     SKIP3                                                                
168300 IMS-INSERT-MSG SECTION.                                                  
168400                                                                          
168500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
168600       MOVE '0' TO MFS-KDHUVOMR                                           
168700     END-IF                                                               
168800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
168900     MOVE SPACE TO GODK-STATUSKODER                                       
169000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
169100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
169200     PERFORM IMS-STATUSKONTROLL                                           
169300     .                                                                    
169400     EJECT                                                                
169500 IMS-ISRT-ALT-MSG-6101  SECTION.                                          
169600     MOVE SPACE TO GODK-STATUSKODER                                       
169700     CALL  CBLTDLI  USING ISRT ALT-PCB P-TO-P-MSG-IO-AREA-SNUF            
169800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
169900     PERFORM IMS-STATUSKONTROLL                                           
170000     .                                                                    
170100     EJECT                                                                
170200 IMS-ISRT-ALT-MSG-6191 SECTION.                                           
170300                                                                          
170400     MOVE SPACE TO GODK-STATUSKODER                                       
170500     CALL CBLTDLI USING ISRT 6191-PCB P-TO-P-MSG-IO-AREA-SNUF             
170600     IF 6191-STATUS-CODE NOT = SPACE                                      
170700       CALL FELLOG                                                        
170800     END-IF                                                               
170900     .                                                                    
171000     EJECT                                                                
171100 IMS-PURG-ALT-MSG-6191 SECTION.                                           
171200                                                                          
171300     MOVE SPACE TO GODK-STATUSKODER                                       
171400     CALL CBLTDLI USING PURG 6191-PCB P-TO-P-MSG-IO-AREA-SNUF             
171500     IF 6191-STATUS-CODE NOT = SPACE                                      
171600       CALL FELLOG                                                        
171700     END-IF                                                               
171800     .                                                                    
171900     EJECT                                                                
172000 IMS-ISRT-ALT-MSG-6197  SECTION.                                          
172100     MOVE SPACE TO GODK-STATUSKODER                                       
172200     CALL  CBLTDLI  USING ISRT 6197-PCB P-TO-P-MSG-IO-AREA-SNUF           
172300     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
172400     PERFORM IMS-STATUSKONTROLL                                           
172500     .                                                                    
172600     SKIP3                                                                
172700 IMS-PURG-ALT-MSG-6197  SECTION.                                          
172800     MOVE SPACE TO GODK-STATUSKODER                                       
172900     CALL  CBLTDLI  USING PURG 6197-PCB P-TO-P-MSG-IO-AREA-SNUF           
173000     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
173100     PERFORM IMS-STATUSKONTROLL                                           
173200     .                                                                    
173300     EJECT                                                                
173400 IMS-GU-W6G130-PLACERING SECTION.                                         
173500                                                                          
173600     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
173700          DELIMITED BY SIZE INTO SSA1                                     
173800     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
173900          DELIMITED BY SIZE INTO SSA2                                     
174000     MOVE '  GE' TO GODK-STATUSKODER                                      
174100     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-W6G130 SSA1 SSA2          
174200     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
174300     PERFORM IMS-STATUSKONTROLL                                           
174400     .                                                                    
174500     SKIP3                                                                
174600 IMS-GU-W6D111-VAGN-UNIK SECTION.                                         
174700                                                                          
174800     STRING 'W6INLA11(W6D1FSEQ =' W-W6D1FSEQ-X                            
174900                    '&IDDC     =' W-IDDC-X ')'                            
175000          DELIMITED BY SIZE INTO SSA1                                     
175100     MOVE '  GE' TO GODK-STATUSKODER                                      
175200     CALL CBLTDLI USING GU INLA-F-PCB DLI-IO-AREA-W6D111 SSA1             
175300     MOVE INLA-F-STATUS-CODE TO STATUS-WS                                 
175400     PERFORM IMS-STATUSKONTROLL                                           
175500     .                                                                    
175600     EJECT                                                                
175700 IMS-GN-W6D111-VAGN SECTION.                                              
175800                                                                          
175900     STRING 'W6INLA11(W6D1FSEQ =' W-W6D1FSEQ-X                            
176000                    '&IDDC     =' W-IDDC-X ')'                            
176100          DELIMITED BY SIZE INTO SSA1                                     
176200     MOVE '  GE' TO GODK-STATUSKODER                                      
176300     CALL CBLTDLI USING GN INLA-F-PCB DLI-IO-AREA-W6D111 SSA1             
176400     MOVE INLA-F-STATUS-CODE TO STATUS-WS                                 
176500     PERFORM IMS-STATUSKONTROLL                                           
176600     .                                                                    
176700     SKIP3                                                                
176800 IMS-GHNP-W6D121-KOLLI SECTION.                                           
176900                                                                          
177000     STRING 'W6INLA21(IDINLVGN =' W-RAD-IDINLVGN-X ')'                    
177100          DELIMITED BY SIZE INTO SSA1                                     
177200     MOVE '  GE' TO GODK-STATUSKODER                                      
177300     CALL CBLTDLI USING GHNP INLA-F-PCB DLI-IO-AREA-W6D121 SSA1           
177400     MOVE INLA-F-STATUS-CODE TO STATUS-WS                                 
177500     PERFORM IMS-STATUSKONTROLL                                           
177600     .                                                                    
177700     EJECT                                                                
177800 IMS-GNP-W6D121-KOLLI-FIRST SECTION.                                      
177900                                                                          
178000     STRING 'W6INLA21(IDINLVGN =' W-RAD-IDINLVGN-X ')'                    
178100          DELIMITED BY SIZE INTO SSA1                                     
178200     MOVE '  GE' TO GODK-STATUSKODER                                      
178300     CALL CBLTDLI USING GNP INLA-F-PCB DLI-IO-AREA-W6D121 SSA1            
178400     MOVE INLA-F-STATUS-CODE TO STATUS-WS                                 
178500     PERFORM IMS-STATUSKONTROLL                                           
178600     .                                                                    
178700     SKIP3                                                                
178800 IMS-GU-W6D121-KOLLI-FIRST SECTION.                                       
178900                                                                          
179000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
179100          DELIMITED BY SIZE INTO SSA1                                     
179200     STRING 'W6INLA11(IDRADNRI =' W-W6D111KY-X ')'                        
179300          DELIMITED BY SIZE INTO SSA2                                     
179400     STRING 'W6INLA21(IDRADNR  =' W-W6D121KY-X ')'                        
179500          DELIMITED BY SIZE INTO SSA3                                     
179600     MOVE '  GE' TO GODK-STATUSKODER                                      
179700     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-W6D121 SSA1 SSA2          
179800                                                        SSA3              
179900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
180000     PERFORM IMS-STATUSKONTROLL                                           
180100     .                                                                    
180200     EJECT                                                                
180300 IMS-GN-W6D1F1-VAGN-FIRST SECTION.                                        
180400                                                                          
180500     STRING 'W6INLG01(W6D1F1KY=>' W-W6D1F1KY-MIN-X                        
180600                    '&W6D1F1KY <' W-W6D1F1KY-MAX-X                        
180700                    '&IDDC    = ' W-IDDC-X ')'                            
180800          DELIMITED BY SIZE INTO SSA1                                     
180900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
181000     CALL CBLTDLI USING GN INLG-PCB DLI-IO-AREA-W6D1F1 SSA1               
181100     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
181200     PERFORM IMS-STATUSKONTROLL                                           
181300     .                                                                    
181400     SKIP3                                                                
181500 IMS-GN-W6D1F1-VAGN SECTION.                                              
181600                                                                          
181700     STRING 'W6INLG01(W6D1F1KY >' W-W6D1F1KY-MIN-X                        
181800                    '&W6D1F1KY <' W-W6D1F1KY-MAX-X                        
181900                    '&IDDC    = ' W-IDDC-X ')'                            
182000          DELIMITED BY SIZE INTO SSA1                                     
182100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
182200     CALL CBLTDLI USING GN INLG-PCB DLI-IO-AREA-W6D1F1 SSA1               
182300     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
182400     PERFORM IMS-STATUSKONTROLL                                           
182500     .                                                                    
182600     SKIP2                                                                
182700 IMS-REPL-W6D121-KOLLI SECTION.                                           
182800                                                                          
182900     MOVE '  ' TO GODK-STATUSKODER                                        
183000     CALL CBLTDLI USING REPL INLA-F-PCB DLI-IO-AREA-W6D121                
183100     MOVE INLA-F-STATUS-CODE TO STATUS-WS                                 
183200     PERFORM IMS-STATUSKONTROLL                                           
183300     .                                                                    
183400     EJECT                                                                
183500 IMS-GU-UPFA01 SECTION.                                                   
183600     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
183700          DELIMITED BY SIZE INTO SSA1                                     
183800     MOVE '  GE' TO GODK-STATUSKODER                                      
183900     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
184000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
184100     PERFORM IMS-STATUSKONTROLL                                           
184200     .                                                                    
184300     SKIP3                                                                
184400 IMS-GNP-UPFA11 SECTION.                                                  
184500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
184600          DELIMITED BY SIZE INTO SSA1                                     
184700     MOVE 'W6UPFA11 ' TO SSA2                                             
184800     MOVE '  GE' TO GODK-STATUSKODER                                      
184900     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
185000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
185100     PERFORM IMS-STATUSKONTROLL                                           
185200     .                                                                    
185300     SKIP3                                                                
185400 IMS-GNP-UPFA12 SECTION.                                                  
185500     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
185600          DELIMITED BY SIZE INTO SSA1                                     
185700     MOVE 'W6UPFA12 ' TO SSA2                                             
185800     MOVE '  GE' TO GODK-STATUSKODER                                      
185900     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
186000     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
186100     PERFORM IMS-STATUSKONTROLL                                           
186200     .                                                                    
186300     SKIP3                                                                
186400 IMS-GU-WDB601    SECTION.                                                
186500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
186600          DELIMITED BY SIZE INTO SSA1                                     
186700     MOVE '  GE' TO GODK-STATUSKODER                                      
186800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
186900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
187000     PERFORM IMS-STATUSKONTROLL                                           
187100     IF SEGMENT-SAKNAS                                                    
187200         MOVE SPACE TO DCS-KDDC                                           
187300     END-IF                                                               
187400     .                                                                    
187500 IMS-STATUSKONTROLL SECTION.                                              
187600                                                                          
187700     SET STATUS-IX TO 1                                                   
187800     SEARCH GODK-STATUS                                                   
187900       AT END CALL FELLOG                                                 
188000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
188100     END-SEARCH                                                           
188200     .                                                                    
