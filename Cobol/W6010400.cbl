000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010400.                                                
000400*AUTHOR.         GUNNAR LARSSON IDK.                                      
000500*DATE-WRITTEN.   92/03/19.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*        REGISTRERA UTPLOCK - VOR                                         
000900*                                                                         
001000*        PROGRAMMET LÄSER      W6INLC (W6D1)                              
001100*        PROGRAMMET LÄSER      W6INLD (W6D1)                              
001200*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001300*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001400*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W6T104                                              
001800*        MID:         W6I10401                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W6O10401  (TILL SKÄRM)                              
002200*        MOD:         W6I19101  (PROG-TO-PROG-SW)                         
002300*        MOD:         W6I19301  (VIA DISPATCHER)                          
002400*        MOD:         W6I19401  (PROG-TO-PROG-SW)                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6010400'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500*   SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                       
004600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +961  COMP SYNC.        
004700*   PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                      
004800 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
004900 77  MAX-MOD-LAENGD-6191         PIC S9(4)  VALUE +1568 COMP SYNC.        
005000 77  MAX-MOD-LAENGD-6193         PIC S9(4)  VALUE +29   COMP SYNC.        
005100                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
005400 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005500 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
005600                                                                          
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006600     88  ALLT-OK                             VALUE 'J'.                   
006700                                                                          
006800 77  SPEC-FELTEXT-SW             PIC X       VALUE 'N'.                   
006900     88  SPEC-FELTEXT                        VALUE 'J'.                   
007000                                                                          
007100 77  NY-LEVNR-SW                 PIC X       VALUE 'N'.                   
007200     88  NY-LEVNR                            VALUE 'J'.                   
007300                                                                          
007400 77  NY-KOLLI-SW                 PIC X       VALUE 'N'.                   
007500     88  NY-KOLLI                            VALUE 'J'.                   
007600                                                                          
007700 77  NY-PARTI-SW                 PIC X       VALUE 'N'.                   
007800     88  NY-PARTI                            VALUE 'J'.                   
007900                                                                          
008000 77  SLUTRAPP-SW                 PIC X       VALUE 'J'.                   
008100     88  SLUT-RAPP                           VALUE 'J'.                   
008200                                                                          
008300 77  PARTI-UPD-SW                PIC X       VALUE 'N'.                   
008400     88  PARTI-UPD                           VALUE 'J'.                   
008500                                                                          
008600 77  KVAL-FEL-SW                 PIC X       VALUE 'N'.                   
008700     88  KVAL-FEL                            VALUE 'J'.                   
008800                                                                          
008900  77 IDDC-SW                     PIC X(1)    VALUE 'J'.                   
009000     88  RAETT-IDDC                          VALUE 'J'.                   
009100     88  FEL-IDDC                            VALUE 'N'.                   
009200                                                                          
009300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009400     88  EGEN-MID                            VALUE '6104'.                
009500     88  GODK-MID                            VALUE '6104'.                
009600     88  HELP-MID                            VALUE '0551'.                
009700     EJECT                                                                
009800*      --- VALID IDDC CODES                                               
009900*                                                                         
010000*01    -COPY WWDCKONS                                                     
010100       EJECT                                                              
010200*    --- DIVERSE ARBETSFÄLT                                               
010300 01      FILLER                  PIC X(8)    VALUE 'WS******'.            
010400 01      WS.                                                              
010500  02     WS-IDOKOLLI-NUM         PIC 9(9)    VALUE ZERO.                  
010600  02     WS-IDLOPNRM-NUM         PIC 9(9)    VALUE ZERO.                  
010700     SKIP3                                                                
010800*    --- ARBETSFÄLT FÖR DATUMKONVERTERING                                 
010900 01  DAGENS-DATUM.                                                        
011000         05   DAGENS-AA          PIC 9(2)    VALUE ZERO.                  
011100         05   DAGENS-MM          PIC 9(2)    VALUE ZERO.                  
011200         05   DAGENS-DD          PIC 9(2)    VALUE ZERO.                  
011300 77  WS-IDLOPNRM-DAT             PIC 9(9)    VALUE ZERO.                  
011400 77  WS-TIINLMOT                 PIC 9(6)    VALUE ZERO.                  
011500 77  WS-TIAAVVD                  PIC 9(5)    VALUE ZERO.                  
011600 77  WS-VECKA                    PIC 9(2)    VALUE ZERO.                  
011700 77  WS-AA                       PIC 9(2)    VALUE ZERO.                  
011800*    --- ARBETSFÄLT FÖR UPPDATERING                                       
011900 01      FILLER                  PIC X(8)    VALUE 'WS-UPD**'.            
012000 01      WS-UPD.                                                          
012100  02     WS-UPD-KVINLART         PIC S9(7)   VALUE ZERO COMP-3.           
012200  02     WS-UPD-IDLOPNRM         PIC S9(9)   VALUE ZERO COMP-3.           
012300  02     WS-UPD-IDRADNR          PIC S9(5)   VALUE ZERO COMP-3.           
012400  02     WS-UPD-ADINLOMR-PRT     PIC X(4)    VALUE SPACE.                 
012500     SKIP3                                                                
012600*    --- SPARAT FRÅN GAMMAL RAD                                           
012700 01      FILLER                  PIC X(8)    VALUE 'WS-GML**'.            
012800 01      WS-GML.                                                          
012900  02     WS-GML-ADINLOMR         PIC X(4)    VALUE SPACE.                 
013000  02     WS-GML-ADINLOMR-NXT     PIC X(4)    VALUE SPACE.                 
013100  02     WS-GML-KDINLSTA         PIC X(3)    VALUE SPACE.                 
013200  02     WS-GML-KVINLART         PIC S9(7)   VALUE ZERO COMP-3.           
013300     EJECT                                                                
013400*    --- SWITCHAR                                                         
013500                                                                          
013600 01      FILLER                  PIC X(8)    VALUE 'SW******'.            
013700                                                                          
013800 01      SW-SWITCHAR.                                                     
013900                                                                          
014000  02     SW-NKLTYP-LEV-KLI       PIC X(1)    VALUE SPACE.                 
014100  02     SW-RADDATA-FINNS        PIC X(1)    VALUE SPACE.                 
014200  02     SW-REDIG-RADDATA        PIC X(1)    VALUE SPACE.                 
014300  02     SW-OK-KVINLART          PIC X(1)    VALUE SPACE.                 
014400  02     SW-OK-IDLOPNRM          PIC X(1)    VALUE SPACE.                 
014500  02     SW-OK-IDRADNR           PIC X(1)    VALUE SPACE.                 
014600  02     SW-OK-ADINLOMR-PRT      PIC X(1)    VALUE SPACE.                 
014700     SKIP3                                                                
014800*    --- INDEXVARIABLER                                                   
014900                                                                          
015000 01      FILLER                  PIC X(8)    VALUE 'SW******'.            
015100                                                                          
015200 01      IX-INDEXVARIABLER.                                               
015300                                                                          
015400  02     IX-6191                 PIC S9(9)   VALUE ZERO COMP SYNC.        
015500  02     IX-6194                 PIC S9(9)   VALUE ZERO COMP SYNC.        
015600  02     IX-6195                 PIC S9(9)   VALUE ZERO COMP SYNC.        
015700     EJECT                                                                
015800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015900 01  GENERELLA-SUBPROGRAM.                                                
016000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
016400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
016500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016700     EJECT                                                                
016800*    --- COPYTEXT TILL SUBPROGRAM WDATKONV                                
016900*01  -COPY WDATAREA                                                       
017000     EJECT                                                                
017100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017200*01 -COPY WMEDAREA                                                        
017300     SKIP3                                                                
017400 01  MESSAGE-CODES.                                                       
017500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017700     03  ERR-007-OTILLATEN-UPD   PIC X(3)    VALUE '007'.                 
017800     03  ERR-010-NOT-IN-REG      PIC X(3)    VALUE '010'.                 
017900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
018200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018300     03  ERR-ON-CASE-LEVEL       PIC X(3)    VALUE '178'.                 
018400     03  ERR-KVAL-FEL            PIC X(3)    VALUE '189'.                 
018500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018600     03  ERR-772-PRT-FEL         PIC X(3)    VALUE '772'.                 
018610     03  ERR-WEIGHT-MISSING      PIC X(3)    VALUE '792'.                 
018620     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
018630     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '794'.                 
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'W006PRT*'.            
018900     SKIP3                                                                
019000*01  -COPY WMSGINIT                                                       
019100     EJECT                                                                
019200*01  -COPY W006PRT                                                        
019300     EJECT                                                                
019400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019500*                                                                         
019600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019700     SKIP3                                                                
019800*01  MID -COPY W6I10401                                                   
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020100     SKIP3                                                                
020200*01  -COPY WMSGAREA                                                       
020300     EJECT                                                                
020400     03  MOD REDEFINES MSG-AREA.                                          
020500*      05  -COPY W6O10401                                                 
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
020800     SKIP3                                                                
020900*01  -COPY WMSGKOM                                                        
021000     EJECT                                                                
021100 01  FILLER                      PIC X(16)  VALUE 'MSG/KOM-AREA'.         
021200     SKIP3                                                                
021300*01  -COPY WMSGAREA   -PRE  K                                             
021400     EJECT                                                                
021500*    05  MOD -COPY W6I19301 -PRE 6193-   -RED KMSG-MID-OUT.               
021600     EJECT                                                                
021700 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
021800     SKIP3                                                                
021900 01      P-TO-P-SW.                                                       
022000                                                                          
022100  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
022200  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
022300  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
022400  02     P-TO-P-KDTRANS          PIC X(8).                                
022500  02     P-TO-P-IDTRANS          PIC X(4).                                
022600  02     P-TO-P-KDMFSFOR         PIC X(1).                                
022700  02     P-TO-P-DATA             PIC X(1500).                             
022800     EJECT                                                                
022900 01      FILLER                  PIC X(24)   VALUE                        
023000                                 'MOD6191-MID-W6I19101'.                  
023100     SKIP2                                                                
023200     -COPY W6I19101 -PRE MOD6191-                                         
023300     EJECT                                                                
023400 01      FILLER                  PIC X(24)   VALUE                        
023500                                 'MOD6194-MID-W6I19401'.                  
023600     SKIP2                                                                
023700*    -COPY W6I19401 -PRE MOD6194-                                         
023800     EJECT                                                                
023900 01      FILLER                  PIC X(24)   VALUE                        
024000                                 'MOD6195-MID-W6I19501'.                  
024100     SKIP2                                                                
024200*    -COPY W6I19501 -PRE MOD6195-                                         
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024500     SKIP3                                                                
024600*01  -COPY WMFSAREA                                                       
024700     EJECT                                                                
024800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024900*                                                                         
025000     SKIP2                                                                
025100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025200     SKIP2                                                                
025300 01  NYCKLAR-TILL-DLI.                                                    
025400                                                                          
025500*    -- W6D1 HUVUDBAS.                                                    
025600     03  W-W6D101KY-X.                                                    
025700         05  W-W6D101KY-IDDC     PIC  X(2)   VALUE SPACE.                 
025800         05  W-W6D101KY-IDLEVNR  PIC  X(5)   VALUE SPACE.                 
025900         05  W-W6D101KY-IDFS     PIC X(8)    VALUE SPACE.                 
026000         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
026100     03  W-IDRADNR-INL-X.                                                 
026200         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
026300     03  W-IDRADNR-X.                                                     
026400         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
026500                                                                          
026600*    -- W6D1 INDEXBAS B. MIN O MAX.                                       
026700     03  W-W6D1BSEQ-X.                                                    
026800         05  W-IDLOPNRM                                                   
026900                                 PIC S9(9)   VALUE ZERO COMP-3.           
027000     03  W-MIN-IDRADNR                                                    
027100                                 PIC S9(5)   VALUE ZERO COMP-3.           
027200                                                                          
027300*    -- W6D1 INDEXBAS C. MIN O MAX.                                       
027400     03  W-W6D1C1KY-MIN-X.                                                
027500         05  W-W6D1C1KY-MIN-IDLEVNR-KOLLI                                 
027600                                 PIC  X(5)   VALUE SPACE.                 
027700         05  W-W6D1C1KY-MIN-IDOKOLLI                                      
027800                                 PIC 9(9)    VALUE ZERO.                  
027900         05  W-W6D1C1KY-MIN-IDRADNR-INL                                   
028000                                 PIC S9(5)   VALUE ZERO COMP-3.           
028100         05  W-W6D1C1KY-MIN-IDDC                                          
028200                                 PIC  X(2)   VALUE SPACE.                 
028300         05  W-W6D1C1KY-MIN-IDLEVNR                                       
028400                                 PIC  X(5)   VALUE SPACE.                 
028500         05  W-W6D1C1KY-MIN-IDFS PIC X(8)    VALUE SPACE.                 
028600         05  W-W6D1C1KY-MIN-TIAVIDAT                                      
028700                                 PIC S9(7)   VALUE ZERO COMP-3.           
028800         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
028900     03  W-W6D1C1KY-MAX-X.                                                
029000         05  W-W6D1C1KY-MAX-IDLEVNR-KOLLI                                 
029100                                 PIC  X(5)   VALUE SPACE.                 
029200         05  W-W6D1C1KY-MAX-IDOKOLLI                                      
029300                                 PIC 9(9)    VALUE ZERO.                  
029400         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
029500         05  FILLER              PIC  X(2)   VALUE SPACE.                 
029600         05  FILLER              PIC X(5)    VALUE SPACE.                 
029700         05  FILLER              PIC X(8)    VALUE SPACE.                 
029800         05  FILLER              PIC S9(7)   VALUE ZERO COMP-3.           
029900         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
030000                                                                          
030100     03  W-W6GXKEY-6005-X.                                                
030200         05  FILLER              PIC X(4)    VALUE '6005'.                
030300         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
030400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
030500     03  W-W6GXKEY-6006-X.                                                
030600         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
030700         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
030800                                                                          
030900     03  W-W6GXKEY-6017-X.                                                
031000         05  FILLER              PIC X(4)    VALUE '6017'.                
031100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
031200                                                                          
031300     03  W-IDDC-X.                                                        
031400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
031500                                                                          
031600     03  W-IDDC-B6-X.                                                     
031700         05 W-IDDC-B6            PIC X(2).                                
031800                                                                          
031900     SKIP2                                                                
032000*    --- STATUS-KOD FRÅN IMS                                              
032100 01  STATUS-WS                   PIC XX.                                  
032200     88  SEGMENT-FINNS                       VALUE '  '.                  
032300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
032500     SKIP2                                                                
032600 01  GODK-STATUSKODER.                                                    
032700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032800     SKIP3                                                                
032900 01  SSA1                        PIC X(128).                              
033000 01  SSA2                        PIC X(64).                               
033100     EJECT                                                                
033200*    --- IMS FUNKTIONSKODER                                               
033300*01  -COPY W0003                                                          
033400     EJECT                                                                
033500*    ---  DLI INPUT-OUTPUT AREA                                           
033600                                                                          
033700*         DLI-IO-AREA    W6INLA11, W6INLC01, W6INLD01                     
033800                                                                          
033900*         DLI-IO-AREA2   W6INLA21 G,R,D GML RAD, ISRT NY RAD              
034000                                                                          
034100*         DLI-IO-AREA4   W6INLA21 GET HÖGSTA RADNR. PRE: INLA-LA-         
034200                                                                          
034300*         DLI-IO-AREA5   W6PLAA11                                         
034400                                                                          
034500*         DLI-IO-AREA6   W6LOPA11                                         
034600     EJECT                                                                
034700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
034800     SKIP3                                                                
034900 01  DLI-IO-AREA.                                                         
035000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
035100     SKIP3                                                                
035200     03  W6INLA11 REDEFINES IO-AREA.                                      
035300*        05  -COPY W6D111  -PRE INLA-                                     
035400     EJECT                                                                
035500     03  W6INLD01 REDEFINES IO-AREA.                                      
035600*        05  -COPY W6D1C1  -PRE INLD-                                     
035700     EJECT                                                                
035800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
035900     SKIP3                                                                
036000 01  DLI-IO-AREA2.                                                        
036100     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
036200     SKIP3                                                                
036300     03  W6INLA21 REDEFINES IO-AREA2.                                     
036400*        05  -COPY W6D121  -PRE INLA-                                     
036500     EJECT                                                                
036600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
036700 01  DLI-IO-AREA4.                                                        
036800     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
036900     SKIP3                                                                
037000     03  W6INLA21 REDEFINES IO-AREA4.                                     
037100*        05  -COPY W6D121  -PRE INLA-LA-                                  
037200     EJECT                                                                
037300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
037400     SKIP3                                                                
037500 01  DLI-IO-AREA5.                                                        
037600     03  IO-AREA5                PIC X(100)  VALUE SPACE.                 
037700     03  W6PLAA11 REDEFINES IO-AREA5.                                     
037800*        05  -COPY W6GX6006 -PRE PLAA-                                    
037900     EJECT                                                                
038000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
038100     SKIP3                                                                
038200 01  DLI-IO-AREA6.                                                        
038300     03  IO-AREA6                PIC X(100)  VALUE SPACE.                 
038400     03  W6LOPA11 REDEFINES IO-AREA6.                                     
038500*        05  -COPY W6GX6018 -PRE LOPA-                                    
038600                                                                          
038700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
038800 01   DLI-IO-AREA-B601.                                                   
038900*     03  -COPY WDB601                                                    
039000                                                                          
039100     EJECT                                                                
039200 LINKAGE SECTION.                                                         
039300                                                                          
039400*01  -COPY W0009   -PRE MSG-                                              
039500     EJECT                                                                
039600*01  -COPY W0009   -PRE 6191-                                             
039700     EJECT                                                                
039800*01  -COPY W0009   -PRE DISP-                                             
039900     EJECT                                                                
040000*01  -COPY W0009   -PRE 6194-                                             
040100     EJECT                                                                
040200*01  -COPY W0009   -PRE 6195-                                             
040300     EJECT                                                                
040400*01  -COPY W0008  -PRE USEA-                                              
040500     05  FILLER                  PIC X.                                   
040600     EJECT                                                                
040700*01  -COPY W0008  -PRE INLD-                                              
040800     05  FILLER                  PIC X.                                   
040900     EJECT                                                                
041000*01  -COPY W0008  -PRE INLA-                                              
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008  -PRE INLA2-                                             
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008  -PRE PLAA-                                              
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900*01  -COPY W0008  -PRE LOPA-                                              
042000     05  FILLER                  PIC X.                                   
042100     EJECT                                                                
042200*01  -COPY W0008  -PRE WDB6-                                              
042300     05  FILLER                  PIC X.                                   
042400     EJECT                                                                
042500 01  KOM-KOMA-PCB                PIC X.                                   
042600     EJECT                                                                
042700 PROCEDURE DIVISION  USING MSG-PCB                                        
042800                           6191-PCB DISP-PCB 6194-PCB 6195-PCB            
042900                           USEA-PCB INLD-PCB INLA-PCB INLA2-PCB           
043000                           PLAA-PCB LOPA-PCB WDB6-PCB                     
043100                           KOM-KOMA-PCB.                                  
043200     ENTRY 'DLITCBL' USING MSG-PCB                                        
043300                           6191-PCB DISP-PCB 6194-PCB 6195-PCB            
043400                           USEA-PCB INLD-PCB INLA-PCB INLA2-PCB           
043500                           PLAA-PCB LOPA-PCB WDB6-PCB                     
043600                           KOM-KOMA-PCB.                                  
043700                                                                          
043800     PERFORM IMS-GET-MSG                                                  
043900     IF SEGMENT-FINNS                                                     
044000       PERFORM A-INIT                                                     
044100       PERFORM B-KOLLA-NYCKLAR                                            
044200       IF NYCKLAR-OK                                                      
044300         IF MFS-UPDATE                                                    
044400           PERFORM G-KOLLA-INPUT                                          
044500           IF INDATA-OK                                                   
044600             PERFORM H-UPPDATERA                                          
044700             PERFORM F-LAES-VISA-INFO                                     
044800           ELSE                                                           
044900             IF KVAL-FEL                                                  
045000               PERFORM F-LAES-VISA-INFO                                   
045100             END-IF                                                       
045200           END-IF                                                         
045300         ELSE                                                             
045400           IF MFS-FIRST                                                   
045500             PERFORM C-FOERSTA-SIDA                                       
045600           ELSE                                                           
045700             IF MFS-NEXT                                                  
045800               PERFORM D-NAESTA-SIDA                                      
045900             ELSE                                                         
046000               PERFORM E-SAMMA-SIDA                                       
046100             END-IF                                                       
046200           END-IF                                                         
046300           IF ALLT-OK                                                     
046400             PERFORM F-LAES-VISA-INFO                                     
046500           END-IF                                                         
046600         END-IF                                                           
046700       END-IF                                                             
046800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
046900       PERFORM IMS-INSERT-MSG                                             
047000     END-IF                                                               
047100                                                                          
047200     MOVE ZERO TO RETURN-CODE                                             
047300     GOBACK                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 A-INIT SECTION.                                                          
047700                                                                          
047800     IF MSG-DUBBLA-TRANSKODER                                             
047900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10401                 
048000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
048100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048200     ELSE                                                                 
048300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10401                  
048400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
048500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
048600     END-IF                                                               
048700                                                                          
048800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
048900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
049000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049100                                                                          
049200     MOVE LOW-VALUE TO MSG-AREA                                           
049300     MOVE 'W6O104N1' TO MFS-IDMOD                                         
049400     MOVE '6104' TO MOD-IDTRANS                                           
049500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
049600                                                                          
049700     IF NOT EGEN-MID AND NOT HELP-MID                                     
049800       MOVE SPACE TO MFS-KDTRTYP                                          
049900       MOVE '7' TO MFS-IDPFK                                              
050000     END-IF                                                               
050100                                                                          
050200     PERFORM MFS-FORM-ATTR                                                
050300     PERFORM AA-INIT-NYCKLAR                                              
050400                                                                          
050500     IF MSGI-IDLAND-SPR = 'GB'                                            
050600       MOVE +2 TO SPRAK-IX                                                
050700       MOVE 'GB ' TO MED-IDSKYLT                                          
050800     ELSE                                                                 
050900       MOVE +1 TO SPRAK-IX                                                
051000       MOVE 'S  ' TO MED-IDSKYLT                                          
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 AA-INIT-NYCKLAR SECTION.                                                 
051500                                                                          
051600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
051700     MOVE '001'                  TO MSGI-KDCALL                           
051800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
051900     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
052000     MOVE '6104'                 TO MSGI-IDTRANS                          
052100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
052200     .                                                                    
052300     EJECT                                                                
052400 B-KOLLA-NYCKLAR SECTION.                                                 
052500                                                                          
052600     MOVE JA TO NYCKLAR-SW                                                
052700                                                                          
052800*    -- KONTROLL AV IDLEVNR-KOLLI                                         
052900     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-IN                         
053000                                                                          
053100     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
053200       MOVE MID-IDLEVNR-KOLLI-UT TO WS-IDLEVNR-KOLLI                      
053300     ELSE                                                                 
053400       MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                      
053500       MOVE '7'         TO MFS-IDPFK                                      
053600       MOVE SPACE       TO MFS-KDTRTYP                                    
053700       MOVE JA          TO NY-LEVNR-SW                                    
053800     END-IF                                                               
053900                                                                          
054000*    -- KONTROLL AV IDOKOLLI                                              
054100     MOVE MFS-RENSA-FAELT TO MOD-IDOKOLLI-IN                              
054200                                                                          
054300     IF MID-IDOKOLLI-IN = ALL '+'                                         
054400       MOVE MID-IDOKOLLI-UT TO WS-IDOKOLLI                                
054500       INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO                
054600     ELSE                                                                 
054700       MOVE MID-IDOKOLLI-IN TO WS-IDOKOLLI                                
054800       MOVE '7'         TO MFS-IDPFK                                      
054900       MOVE SPACE       TO MFS-KDTRTYP                                    
055000       MOVE JA          TO NY-KOLLI-SW                                    
055100     END-IF                                                               
055200                                                                          
055300*    -- KONTROLL AV IDLOPNRM                                              
055400     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
055500                                                                          
055600     IF MID-IDLOPNRM-IN = ALL '+'                                         
055700       MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM                                
055800       INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                
055900     ELSE                                                                 
056000       MOVE MID-IDLOPNRM-IN TO WS-IDLOPNRM                                
056100       MOVE '7'         TO MFS-IDPFK                                      
056200       MOVE SPACE       TO MFS-KDTRTYP                                    
056300       MOVE JA          TO NY-PARTI-SW                                    
056400     END-IF                                                               
056500                                                                          
056600*    -- KONTROLL AV IDDC                                                  
056700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
056800                                                                          
056900     IF MID-IDDC-IN          = ALL '+'                                    
057000       MOVE MSGI-IDDC        TO W-IDDC-B6                                 
057100     ELSE                                                                 
057200       MOVE MID-IDDC-IN      TO W-IDDC-B6                                 
057300       MOVE '7'              TO MFS-IDPFK                                 
057400       MOVE SPACE            TO MFS-KDTRTYP                               
057500       MOVE JA               TO NY-LEVNR-SW                               
057600     END-IF                                                               
057700     PERFORM IMS-GU-WDB601                                                
057800                                                                          
057900     IF DCS-KDDC = SPACE OR DCS-DDC                                       
058000        MOVE NEJ             TO NYCKLAR-SW                                
058100     ELSE                                                                 
058200        IF DCS-IDDC = WC-SDC-NL-ET                                        
058300          MOVE WC-CDC-TR TO W-IDDC-B6                                     
058400          PERFORM IMS-GU-WDB601                                           
058500*DETTA LÖSER PROBLEMET MED ATT DC91 PERSONAL GÖR VOR PLOCKEN              
058600        END-IF                                                            
058700        MOVE DCS-IDDC        TO W-6005-IDDC                               
058800                                W-IDDC                                    
058900        MOVE DCS-IDDC        TO MOD-IDDC-UT                               
059000     END-IF                                                               
059100                                                                          
059200*    -- KONTROLLER SAMBAND MELLAN NYCKLAR                                 
059300     IF (WS-IDLEVNR-KOLLI NOT = SPACE                                     
059400     AND(WS-IDOKOLLI NUMERIC AND                                          
059500         WS-IDOKOLLI > ZERO))                                             
059600     OR (WS-IDLOPNRM NUMERIC AND                                          
059700         WS-IDLOPNRM > ZERO)                                              
059800       IF NY-LEVNR AND NY-KOLLI                                           
059900         PERFORM BA-RED-NKL-LEV-KLI                                       
060000       ELSE                                                               
060100         IF NY-PARTI                                                      
060200           PERFORM BB-RED-NKL-LOPNRM                                      
060300         ELSE                                                             
060400           IF  WS-IDLEVNR-KOLLI NOT = SPACE                               
060500           AND (WS-IDOKOLLI NUMERIC AND                                   
060600                WS-IDOKOLLI > ZERO)                                       
060700             PERFORM BA-RED-NKL-LEV-KLI                                   
060800           ELSE                                                           
060900             IF WS-IDLOPNRM NUMERIC AND                                   
061000                WS-IDLOPNRM > ZERO                                        
061100               PERFORM BB-RED-NKL-LOPNRM                                  
061200             ELSE                                                         
061300               MOVE NEJ TO NYCKLAR-SW                                     
061400             END-IF                                                       
061500           END-IF                                                         
061600         END-IF                                                           
061700       END-IF                                                             
061800     ELSE                                                                 
061900       MOVE NEJ TO NYCKLAR-SW                                             
062000     END-IF                                                               
062100                                                                          
062200     IF NYCKLAR-FEL                                                       
062300       IF EGEN-MID                                                        
062400         MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                    
062500         MOVE WS-IDOKOLLI TO MOD-IDOKOLLI-UT                              
062600         INSPECT MOD-IDOKOLLI-UT                                          
062700         REPLACING LEADING ZERO BY SPACE                                  
062800         MOVE WS-IDLOPNRM TO MOD-IDLOPNRM-UT                              
062900         INSPECT MOD-IDLOPNRM-UT                                          
063000         REPLACING LEADING ZERO BY SPACE                                  
063100         MOVE DCS-IDDC    TO MOD-IDDC-UT                                  
063200         INSPECT MOD-IDDC-UT                                              
063300         REPLACING LEADING ZERO BY SPACE                                  
063400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
063500         CALL WMEDKONV USING MED-WMEDAREA                                 
063600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
063700         PERFORM MFS-RENSA-FAELT-IN                                       
063800         PERFORM MFS-RENSA-FAELT-UT                                       
063900       ELSE                                                               
064000         PERFORM MFS-RENSA-FAELT-IN                                       
064100         PERFORM MFS-RENSA-FAELT-UT                                       
064200         PERFORM MFS-RENSA-NYCKLAR-UT                                     
064300       END-IF                                                             
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700 BA-RED-NKL-LEV-KLI SECTION.                                              
064800                                                                          
064900     MOVE JA                     TO SW-NKLTYP-LEV-KLI                     
065000                                                                          
065100     MOVE LOW-VALUE              TO W-W6D1C1KY-MIN-X                      
065200     MOVE HIGH-VALUE             TO W-W6D1C1KY-MAX-X                      
065300     MOVE WS-IDLEVNR-KOLLI       TO W-W6D1C1KY-MIN-IDLEVNR-KOLLI          
065400                                    W-W6D1C1KY-MAX-IDLEVNR-KOLLI          
065500     MOVE WS-IDOKOLLI            TO W-W6D1C1KY-MIN-IDOKOLLI               
065600                                    W-W6D1C1KY-MAX-IDOKOLLI               
065700                                                                          
065800     MOVE WS-IDOKOLLI            TO WS-IDOKOLLI-NUM                       
065900     MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                        
066000     MOVE WS-IDOKOLLI TO MOD-IDOKOLLI-UT                                  
066100     INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE              
066200     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                              
066300     .                                                                    
066400     EJECT                                                                
066500 BB-RED-NKL-LOPNRM SECTION.                                               
066600                                                                          
066700     MOVE NEJ                    TO SW-NKLTYP-LEV-KLI                     
066800                                                                          
066810     IF WS-IDLOPNRM NOT NUMERIC                                           
066820       MOVE ZERO TO WS-IDLOPNRM                                           
066830     END-IF                                                               
066900     MOVE WS-IDLOPNRM            TO W-IDLOPNRM                            
067000                                                                          
067100     MOVE WS-IDLOPNRM            TO WS-IDLOPNRM-NUM                       
067200     MOVE WS-IDLOPNRM            TO MOD-IDLOPNRM-UT                       
067300     INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE              
067400     MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-KOLLI-UT                  
067500                                    MOD-IDOKOLLI-UT                       
067600     .                                                                    
067700     EJECT                                                                
067800 C-FOERSTA-SIDA SECTION.                                                  
067900                                                                          
068000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
068100     CALL WMEDKONV USING MED-WMEDAREA                                     
068200     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
068300                                                                          
068400*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
068500*        (LOW-VALUE REDAN INLAGT I B-SECTIONEN)                           
068600                                                                          
068700     MOVE JA TO ALLT-SW                                                   
068800     PERFORM MFS-RENSA-FAELT-IN                                           
068900     .                                                                    
069000     EJECT                                                                
069100 D-NAESTA-SIDA SECTION.                                                   
069200                                                                          
069300     MOVE MID-IDRADNR-INL-NEXT TO W-W6D1C1KY-MIN-IDRADNR-INL              
069400     MOVE MID-IDLEVNR-NEXT     TO W-W6D1C1KY-MIN-IDLEVNR                  
069500     MOVE MID-IDFS-NEXT        TO W-W6D1C1KY-MIN-IDFS                     
069600     MOVE MID-TIAVIDAT-NEXT    TO W-W6D1C1KY-MIN-TIAVIDAT                 
069700     MOVE W-IDDC               TO W-W6D1C1KY-MIN-IDDC                     
069800                                                                          
069900     MOVE JA TO ALLT-SW                                                   
070000     PERFORM MFS-RENSA-FAELT-IN                                           
070100     .                                                                    
070200     EJECT                                                                
070300 E-SAMMA-SIDA SECTION.                                                    
070400                                                                          
070500     IF MID-INPUT = ALL '+'                                               
070600       MOVE MID-IDRADNR-INL-ENTER TO W-W6D1C1KY-MIN-IDRADNR-INL           
070700       MOVE W-IDDC                TO W-W6D1C1KY-MIN-IDDC                  
070800       MOVE MID-IDLEVNR-ENTER     TO W-W6D1C1KY-MIN-IDLEVNR               
070900       MOVE MID-IDFS-ENTER        TO W-W6D1C1KY-MIN-IDFS                  
071000       MOVE MID-TIAVIDAT-ENTER    TO W-W6D1C1KY-MIN-TIAVIDAT              
071100       MOVE JA                    TO ALLT-SW                              
071200       PERFORM MFS-RENSA-FAELT-IN                                         
071300     ELSE                                                                 
071400       IF NOT HELP-MID                                                    
071500         MOVE NEJ TO ALLT-SW                                              
071600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
071700         CALL WMEDKONV USING MED-WMEDAREA                                 
071800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
071900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
072000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
072100         PERFORM MFS-LAES-IN-IGEN                                         
072200                                                                          
072300         IF  MID-IDLOPNRM-DEF > ZERO                                      
072400         AND MID-IDRADNR-DEF > ZERO                                       
072500           PERFORM MFS-STAENG-UPD-LOP-RAD                                 
072600         END-IF                                                           
072700                                                                          
072800       ELSE                                                               
072900         MOVE MID-IDRADNR-INL-ENTER TO W-W6D1C1KY-MIN-IDRADNR-INL         
073000         MOVE MID-IDLEVNR-ENTER     TO W-W6D1C1KY-MIN-IDLEVNR             
073100         MOVE MID-IDFS-ENTER        TO W-W6D1C1KY-MIN-IDFS                
073200         MOVE MID-TIAVIDAT-ENTER    TO W-W6D1C1KY-MIN-TIAVIDAT            
073300         MOVE W-IDDC                TO W-W6D1C1KY-MIN-IDDC                
073400         MOVE JA                    TO ALLT-SW                            
073500         PERFORM EA-MID-TILL-MOD                                          
073600       END-IF                                                             
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000 EA-MID-TILL-MOD SECTION.                                                 
074100                                                                          
074200     IF  MID-KVINLART-UPD = ALL '+'                                       
074300       MOVE MFS-RENSA-FAELT      TO MOD-KVINLART-UPD                      
074400     ELSE                                                                 
074500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVINLART-UPD-ATTR                
074600       MOVE MID-KVINLART-UPD      TO MOD-KVINLART-UPD                     
074700     END-IF                                                               
074800                                                                          
074900     IF  MID-IDLOPNRM-UPD = ALL '+'                                       
075000       MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-UPD                      
075100     ELSE                                                                 
075200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLOPNRM-UPD-ATTR                
075300       MOVE MID-IDLOPNRM-UPD     TO MOD-IDLOPNRM-UPD                      
075400     END-IF                                                               
075500                                                                          
075600     IF  MID-IDRADNR-UPD = ALL '+'                                        
075700       MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-UPD                       
075800     ELSE                                                                 
075900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-UPD-ATTR                 
076000       MOVE MID-IDRADNR-UPD      TO MOD-IDRADNR-UPD                       
076100     END-IF                                                               
076200                                                                          
076300     IF  MID-ADINLOMR-PRT-UPD = ALL '+'                                   
076400       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-PRT-UPD                  
076500     ELSE                                                                 
076600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-UPD-ATTR            
076700       MOVE MID-ADINLOMR-PRT-UPD TO MOD-ADINLOMR-PRT-UPD                  
076800     END-IF                                                               
076900     .                                                                    
077000     EJECT                                                                
077100 F-LAES-VISA-INFO SECTION.                                                
077200                                                                          
077300     MOVE ZERO TO MOD-IDLOPNRM-DEF                                        
077400                  MOD-IDRADNR-DEF                                         
077500     MOVE NEJ TO SW-REDIG-RADDATA                                         
077600                                                                          
077700     MOVE +1 TO INDX                                                      
077800     PERFORM FB-LAES-RADDATA                                              
077900     IF SW-RADDATA-FINNS = JA                                             
078000       MOVE INLA-ART-IDRADNR-INL TO MOD-IDRADNR-INL-ENTER                 
078100       MOVE W-W6D101KY-IDLEVNR   TO MOD-IDLEVNR-ENTER                     
078200       MOVE W-W6D101KY-IDFS      TO MOD-IDFS-ENTER                        
078300       MOVE W-W6D101KY-TIAVIDAT  TO MOD-TIAVIDAT-ENTER                    
078400     ELSE                                                                 
078500       MOVE ZERO                TO MOD-IDRADNR-INL-ENTER                  
078600       MOVE SPACE               TO MOD-IDLEVNR-ENTER                      
078700       MOVE SPACE               TO MOD-IDFS-ENTER                         
078800       MOVE ZERO                TO MOD-TIAVIDAT-ENTER                     
078900     END-IF                                                               
079000                                                                          
079100     PERFORM UNTIL INDX > MAX-INDX                                        
079200                                                                          
079300       IF SW-RADDATA-FINNS = JA                                           
079400         PERFORM FC-REDIG-RADDATA                                         
079500                                                                          
079600         ADD 1 TO INDX                                                    
079700         PERFORM FB-LAES-RADDATA                                          
079800       ELSE                                                               
079900         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
080000         ADD 1 TO INDX                                                    
080100       END-IF                                                             
080200                                                                          
080300     END-PERFORM                                                          
080400                                                                          
080500     IF INLA-ART-FLKVAFEL = JA OR INLA-ART-FLKVAKAR = JA                  
080600       MOVE ERR-KVAL-FEL TO MED-IDMFSFEL                                  
080700       CALL WMEDKONV USING MED-WMEDAREA                                   
080800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
080900       PERFORM MFS-STAENG-UPD-ALLA                                        
081000     END-IF                                                               
081100                                                                          
081200     IF SW-RADDATA-FINNS = JA                                             
081300       MOVE INLA-ART-IDRADNR-INL TO MOD-IDRADNR-INL-NEXT                  
081400       MOVE W-W6D101KY-IDLEVNR  TO MOD-IDLEVNR-NEXT                       
081500       MOVE W-W6D101KY-IDFS     TO MOD-IDFS-NEXT                          
081600       MOVE W-W6D101KY-TIAVIDAT TO MOD-TIAVIDAT-NEXT                      
081700       IF MFS-UPDATE                                                      
081800         CONTINUE                                                         
081900       ELSE                                                               
082000         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
082100         CALL WMEDKONV USING MED-WMEDAREA                                 
082200         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
082300         MOVE MFS-ADD-SAETT-CURSOR TO MOD-KVINLART-UPD-ATTR               
082400       END-IF                                                             
082500     ELSE                                                                 
082600       MOVE ZERO                TO MOD-IDRADNR-INL-NEXT                   
082700       MOVE SPACE               TO MOD-IDLEVNR-NEXT                       
082800       MOVE SPACE               TO MOD-IDFS-NEXT                          
082900       MOVE ZERO                TO MOD-TIAVIDAT-NEXT                      
083000       IF SW-REDIG-RADDATA = NEJ                                          
083100         IF SPEC-FELTEXT                                                  
083200           CONTINUE                                                       
083300         ELSE                                                             
083400           MOVE ERR-010-NOT-IN-REG TO MED-IDMFSFEL                        
083500         END-IF                                                           
083600         CALL WMEDKONV USING MED-WMEDAREA                                 
083700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
083800       ELSE                                                               
083900         IF MFS-UPDATE                                                    
084000           CONTINUE                                                       
084100         ELSE                                                             
084200           MOVE MFS-ADD-SAETT-CURSOR TO MOD-KVINLART-UPD-ATTR             
084300         END-IF                                                           
084400       END-IF                                                             
084500     END-IF                                                               
084600     .                                                                    
084700     EJECT                                                                
084800 FB-LAES-RADDATA SECTION.                                                 
084900                                                                          
085000     MOVE NEJ                    TO SW-RADDATA-FINNS                      
085100                                                                          
085200     IF SW-NKLTYP-LEV-KLI        = JA                                     
085300*      -- NYCKEL: IDLEVNR-KOLLI, IDOKOLLI                                 
085400                                                                          
085500       EVALUATE INDX                                                      
085600         WHEN +1                                                          
085700           PERFORM IMS-GU-INLD-SEQC                                       
085800           PERFORM FBA-LAES-RAD-VIA-SEQC                                  
085900         WHEN +2                                                          
086000*          -- 2:A RAD LÄSES ENDAST OM 1:A RAD HAR FLDIVKLI=JA.            
086100           IF INLA-RAD-FLDIVKLI = JA                                      
086200             PERFORM IMS-GN-INLD-SEQC                                     
086300             PERFORM FBA-LAES-RAD-VIA-SEQC                                
086400           ELSE                                                           
086500*            -- EXAKT EN BILDRAD, DEFAULT-NKL FÖR UPPDAT SPARAS.          
086600             MOVE INLA-ART-IDLOPNRM TO MOD-IDLOPNRM-DEF                   
086700             MOVE INLA-RAD-IDRADNR  TO MOD-IDRADNR-DEF                    
086800             PERFORM MFS-STAENG-UPD-LOP-RAD                               
086900           END-IF                                                         
087000         WHEN OTHER                                                       
087100           PERFORM IMS-GN-INLD-SEQC                                       
087200           PERFORM FBA-LAES-RAD-VIA-SEQC                                  
087300       END-EVALUATE                                                       
087400     ELSE                                                                 
087500*      -- NYCKEL: IDLOPNRM                                                
087600       IF INDX = +1                                                       
087700         PERFORM IMS-GU-INLA-ART-SEQ                                      
087800         IF SEGMENT-FINNS                                                 
087900           MOVE INLA-ART-IDRADNR-INL   TO W-IDRADNR-INL                   
088000           PERFORM IMS-GNP-INLA-RAD                                       
088100                                                                          
088200           IF INLA-RAD-IDRADNR = +1                                       
088300             IF INLA-RAD-KDINLSTA = SPACE                                 
088400             OR INLA-RAD-KDINLSTA = 'SAK'                                 
088500             OR INLA-RAD-KDINLSTA = 'FPK'                                 
088600               MOVE NEJ TO SLUTRAPP-SW                                    
088700               MOVE JA           TO SW-RADDATA-FINNS                      
088800               MOVE INLA-ART-IDLOPNRM TO MOD-IDLOPNRM-DEF                 
088900               MOVE INLA-RAD-IDRADNR TO MOD-IDRADNR-DEF                   
089000               PERFORM MFS-STAENG-UPD-LOP-RAD                             
089100             END-IF                                                       
089200           ELSE                                                           
089300             PERFORM UNTIL SEGMENT-SAKNAS OR SW-RADDATA-FINNS = JA        
089400               IF INLA-RAD-KDINLSTA = SPACE                               
089500               OR INLA-RAD-KDINLSTA = 'SAK'                               
089600               OR INLA-RAD-KDINLSTA = 'FPK'                               
089700                 MOVE NEJ TO SLUTRAPP-SW                                  
089800                 IF INLA-RAD-IDOKOLLI = +0                                
089900                   MOVE JA           TO SW-RADDATA-FINNS                  
090000                   MOVE INLA-ART-IDLOPNRM TO MOD-IDLOPNRM-DEF             
090100                   MOVE INLA-RAD-IDRADNR TO MOD-IDRADNR-DEF               
090200                   PERFORM MFS-STAENG-UPD-LOP-RAD                         
090300                 ELSE                                                     
090400                   PERFORM IMS-GNP-INLA-RAD                               
090500                 END-IF                                                   
090600               ELSE                                                       
090700                 PERFORM IMS-GNP-INLA-RAD                                 
090800               END-IF                                                     
090900             END-PERFORM                                                  
091000             IF SEGMENT-SAKNAS AND SLUTRAPP-SW = NEJ                      
091100               MOVE ERR-ON-CASE-LEVEL TO MED-IDMFSFEL                     
091200               MOVE JA TO SPEC-FELTEXT-SW                                 
091300             ELSE                                                         
091400               IF SEGMENT-SAKNAS                                          
091500                 MOVE ERR-010-NOT-IN-REG    TO MED-IDMFSFEL               
091600                 MOVE JA TO SPEC-FELTEXT-SW                               
091700               END-IF                                                     
091800             END-IF                                                       
091900           END-IF                                                         
092000         END-IF                                                           
092100       ELSE                                                               
092200         MOVE NEJ TO SW-RADDATA-FINNS                                     
092300         PERFORM IMS-GNP-INLA-RAD                                         
092400         PERFORM UNTIL SEGMENT-SAKNAS OR SW-RADDATA-FINNS = JA            
092500           IF INLA-RAD-KDINLSTA = SPACE                                   
092600           OR INLA-RAD-KDINLSTA = 'SAK'                                   
092700           OR INLA-RAD-KDINLSTA = 'FPK'                                   
092800             IF INLA-RAD-IDOKOLLI = +0                                    
092900               MOVE JA           TO SW-RADDATA-FINNS                      
093000                                    PARTI-UPD-SW                          
093100               MOVE MFS-RENSA-FAELT   TO MOD-IDRADNR-DEF                  
093200               MOVE MFS-OEPPNA-NUM-FAELT TO MOD-IDRADNR-UPD-ATTR          
093300             ELSE                                                         
093400               PERFORM IMS-GNP-INLA-RAD                                   
093500             END-IF                                                       
093600           ELSE                                                           
093700             PERFORM IMS-GNP-INLA-RAD                                     
093800           END-IF                                                         
093900         END-PERFORM                                                      
094000       END-IF                                                             
094100     END-IF                                                               
094200     .                                                                    
094300     EJECT                                                                
094400 FBA-LAES-RAD-VIA-SEQC SECTION.                                           
094500                                                                          
094600     MOVE JA                     TO IDDC-SW                               
094700                                                                          
094800     PERFORM UNTIL ((NOT SEGMENT-FINNS)                                   
094900               OR   SW-RADDATA-FINNS = JA                                 
095000               OR   FEL-IDDC)                                             
095100                                                                          
095200       MOVE INLD-SEQC-IDLEVNR  TO W-W6D101KY-IDLEVNR                      
095300       MOVE INLD-SEQC-IDFS     TO W-W6D101KY-IDFS                         
095400       MOVE W-IDDC             TO W-W6D101KY-IDDC                         
095500       MOVE INLD-SEQC-TIAVIDAT TO W-W6D101KY-TIAVIDAT                     
095600       MOVE INLD-SEQC-IDRADNR-INL  TO W-IDRADNR-INL                       
095700       MOVE INLD-SEQC-IDRADNR  TO W-IDRADNR                               
095800       PERFORM IMS-GU-INLA2-ART                                           
095900       IF INLA-ART-IDDC        =  W-IDDC                                  
096000          PERFORM IMS-GNP-INLA2-RAD-OK                                    
096100                                                                          
096200          IF INLA-RAD-KDINLSTA = SPACE                                    
096300          OR INLA-RAD-KDINLSTA = 'SAK'                                    
096400          OR INLA-RAD-KDINLSTA = 'FPK'                                    
096500            MOVE JA              TO SW-RADDATA-FINNS                      
096600          ELSE                                                            
096700            PERFORM IMS-GN-INLD-SEQC                                      
096800          END-IF                                                          
096900       ELSE                                                               
097000          MOVE NEJ               TO IDDC-SW                               
097100       END-IF                                                             
097200     END-PERFORM                                                          
097300     .                                                                    
097400     EJECT                                                                
097500 FC-REDIG-RADDATA SECTION.                                                
097600                                                                          
097700     MOVE JA                     TO SW-REDIG-RADDATA                      
097800                                                                          
097900     MOVE INLA-ART-IDARTNR       TO MOD-IDARTNR-RAD  (INDX)               
098000     MOVE INLA-ART-BEART         TO MOD-BEART-RAD    (INDX)               
098100     MOVE INLA-RAD-KVINLART      TO MOD-KVINLART-RAD (INDX)               
098200     MOVE INLA-ART-IDLOPNRM      TO MOD-IDLOPNRM-RAD (INDX)               
098300     MOVE INLA-RAD-IDRADNR       TO MOD-IDRADNR-RAD  (INDX)               
098400     IF MFS-UPDATE  AND PARTI-UPD AND INDATA-OK AND NYCKLAR-OK            
098500       IF INLA-RAD-IDRADNR = WS-UPD-IDRADNR OR +1                         
098600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVINLART-RAD-ATTR(INDX)        
098700       END-IF                                                             
098800     END-IF                                                               
098900     EVALUATE INLA-ART-KDFARLIG                                           
099000       WHEN +4                                                            
099100         IF DCS-CDC                                                       
099200           MOVE 'JA'               TO MOD-BEFARLIG-RAD (INDX)             
099300         ELSE                                                             
099400           MOVE 'YES'              TO MOD-BEFARLIG-RAD (INDX)             
099500         END-IF                                                           
099600       WHEN +5                                                            
099700         IF DCS-CDC                                                       
099800           MOVE 'ASBEST'           TO MOD-BEFARLIG-RAD (INDX)             
099900         ELSE                                                             
100000           MOVE 'ASBEST'           TO MOD-BEFARLIG-RAD (INDX)             
100100         END-IF                                                           
100200       WHEN +6                                                            
100300         IF DCS-CDC                                                       
100400           MOVE 'KEMIKALIER'       TO MOD-BEFARLIG-RAD (INDX)             
100500         ELSE                                                             
100600           MOVE 'CHEMICALS'        TO MOD-BEFARLIG-RAD (INDX)             
100700         END-IF                                                           
100800       WHEN +7                                                            
100900         IF DCS-CDC                                                       
101000           MOVE 'JA'               TO MOD-BEFARLIG-RAD (INDX)             
101100         ELSE                                                             
101200           MOVE 'YES'              TO MOD-BEFARLIG-RAD (INDX)             
101300         END-IF                                                           
101400       WHEN OTHER                                                         
101500         MOVE MFS-RENSA-FAELT    TO MOD-BEFARLIG-RAD (INDX)               
101600     END-EVALUATE                                                         
101700     .                                                                    
101800     EJECT                                                                
101900 G-KOLLA-INPUT SECTION.                                                   
102000                                                                          
102100     MOVE SPACE TO MED-IDMFSFEL                                           
102200                                                                          
102300     MOVE JA  TO INDATA-SW                                                
102400                                                                          
102500     MOVE JA  TO SW-OK-KVINLART                                           
102600     MOVE JA  TO SW-OK-IDLOPNRM                                           
102700     MOVE JA  TO SW-OK-IDRADNR                                            
102800     MOVE JA  TO SW-OK-ADINLOMR-PRT                                       
102900                                                                          
103000     IF MID-INPUT = ALL '+'                                               
103100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
103200       MOVE NEJ TO INDATA-SW                                              
103300                                                                          
103400     ELSE                                                                 
103500       PERFORM GA-FORM-K-KVINLART                                         
103600       PERFORM GB-FORM-K-LOP-RADNR                                        
103700                                                                          
103800       IF SW-OK-IDLOPNRM = JA                                             
103900       AND SW-OK-IDRADNR = JA                                             
104000         PERFORM GC-SAMBAND-KTRL                                          
104100       END-IF                                                             
104200                                                                          
104300       PERFORM GD-KTRL-ADINLOMR-PRT                                       
104400     END-IF                                                               
104500                                                                          
104600     IF INDATA-FEL                                                        
104700       CALL WMEDKONV USING MED-WMEDAREA                                   
104800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
104900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
105000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
105100                                                                          
105200       IF    MID-IDLOPNRM-DEF > ZERO AND                                  
105300             MID-IDRADNR-DEF > ZERO                                       
105400         PERFORM MFS-STAENG-UPD-LOP-RAD                                   
105500       END-IF                                                             
105600     END-IF                                                               
105700     .                                                                    
105800     EJECT                                                                
105900 GA-FORM-K-KVINLART SECTION.                                              
106000                                                                          
106100*    FORMELL KONTROLL AV KVINLART                                         
106200                                                                          
106300     IF MID-KVINLART-UPD NOT = ALL '+'                                    
106400       IF MID-KVINLART-UPD NUMERIC                                        
106500       AND MID-KVINLART-UPD > ZERO                                        
106600*        -- KVINLART FORMELLT RÄTT                                        
106700         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINLART-UPD-ATTR                
106800         MOVE MID-KVINLART-UPD TO WS-UPD-KVINLART                         
106900       ELSE                                                               
107000         MOVE MFS-NUM-FAELT-FEL TO MOD-KVINLART-UPD-ATTR                  
107100         MOVE NEJ TO INDATA-SW                                            
107200         MOVE NEJ TO SW-OK-KVINLART                                       
107300         IF MED-IDMFSFEL = SPACE                                          
107400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
107500         END-IF                                                           
107600       END-IF                                                             
107700                                                                          
107800     ELSE                                                                 
107900*      -- KVINLART EJ ANGIVET                                             
108000       MOVE MFS-NUM-FAELT-FEL TO MOD-KVINLART-UPD-ATTR                    
108100       MOVE NEJ TO INDATA-SW                                              
108200       MOVE NEJ TO SW-OK-KVINLART                                         
108300       IF MED-IDMFSFEL = SPACE                                            
108400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
108500       END-IF                                                             
108600     END-IF                                                               
108700     .                                                                    
108800     EJECT                                                                
108900 GB-FORM-K-LOP-RADNR SECTION.                                             
109000                                                                          
109100*    FORMELL KONTROLL AV IDLOPNRM & IDRADNR                               
109200                                                                          
109300     IF MID-IDLOPNRM-UPD = ALL '+'                                        
109400     AND MID-IDRADNR-UPD = ALL '+'                                        
109500       IF MID-IDLOPNRM-DEF > ZERO                                         
109600       AND MID-IDRADNR-DEF > ZERO                                         
109700*        -- DEFAULT-VÄRDEN ANVÄNDS FÖR IDLOPNRM O IDRADNR                 
109800         MOVE MID-IDLOPNRM-DEF TO WS-UPD-IDLOPNRM                         
109900         MOVE MID-IDRADNR-DEF TO WS-UPD-IDRADNR                           
110000       ELSE                                                               
110100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-UPD-ATTR                  
110200         MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                   
110300         MOVE NEJ TO INDATA-SW                                            
110400         MOVE NEJ TO SW-OK-IDLOPNRM                                       
110500         MOVE NEJ TO SW-OK-IDRADNR                                        
110600         IF MED-IDMFSFEL = SPACE                                          
110700            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
110800         END-IF                                                           
110900       END-IF                                                             
111000     ELSE                                                                 
111100                                                                          
111200*      -- NÅGOT AV IDLOPNRM OCH IDRADNR ANGIVET                           
111300       IF MID-IDLOPNRM-UPD = ALL '+'                                      
111400         IF MID-IDLOPNRM-DEF > ZERO                                       
111500           MOVE MID-IDLOPNRM-DEF TO WS-UPD-IDLOPNRM                       
111600         ELSE                                                             
111700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-UPD-ATTR                
111800           MOVE NEJ TO INDATA-SW                                          
111900           MOVE NEJ TO SW-OK-IDLOPNRM                                     
112000           IF MED-IDMFSFEL = SPACE                                        
112100              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
112200           END-IF                                                         
112300         END-IF                                                           
112400                                                                          
112500       ELSE                                                               
112600         IF MID-IDLOPNRM-UPD NUMERIC                                      
112700         AND MID-IDLOPNRM-UPD > ZERO                                      
112800           IF SW-NKLTYP-LEV-KLI = NEJ                                     
112900           AND MID-IDLOPNRM-UPD NOT = WS-IDLOPNRM                         
113000*            -- IDLOPNRM PÅ UPD-RAD OLIK NYCKEL                           
113100             MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-UPD-ATTR              
113200             MOVE NEJ TO INDATA-SW                                        
113300             MOVE NEJ TO SW-OK-IDLOPNRM                                   
113400             IF MED-IDMFSFEL = SPACE                                      
113500               MOVE ERR-007-OTILLATEN-UPD TO MED-IDMFSFEL                 
113600             END-IF                                                       
113700           ELSE                                                           
113800*            -- IDLOPNRM FORMELLT RÄTT                                    
113900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLOPNRM-UPD-ATTR            
114000             MOVE MID-IDLOPNRM-UPD TO WS-UPD-IDLOPNRM                     
114100           END-IF                                                         
114200                                                                          
114300         ELSE                                                             
114400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-UPD-ATTR                
114500           MOVE NEJ TO INDATA-SW                                          
114600           MOVE NEJ TO SW-OK-IDLOPNRM                                     
114700           IF MED-IDMFSFEL = SPACE                                        
114800             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
114900           END-IF                                                         
115000         END-IF                                                           
115100       END-IF                                                             
115200                                                                          
115300       IF MID-IDRADNR-UPD = ALL '+'                                       
115400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                   
115500         MOVE NEJ TO INDATA-SW                                            
115600         MOVE NEJ TO SW-OK-IDRADNR                                        
115700         IF MED-IDMFSFEL = SPACE                                          
115800            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
115900         END-IF                                                           
116000       ELSE                                                               
116100         IF MID-IDRADNR-UPD NUMERIC                                       
116200         AND MID-IDRADNR-UPD > ZERO                                       
116300*          -- IDRADNR FORMELLT RÄTT                                       
116400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-UPD-ATTR               
116500           MOVE MID-IDRADNR-UPD TO WS-UPD-IDRADNR                         
116600         ELSE                                                             
116700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                 
116800           MOVE NEJ TO INDATA-SW                                          
116900           MOVE NEJ TO SW-OK-IDRADNR                                      
117000           IF MED-IDMFSFEL = SPACE                                        
117100             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
117200           END-IF                                                         
117300         END-IF                                                           
117400       END-IF                                                             
117500     END-IF                                                               
117600     .                                                                    
117700     EJECT                                                                
117800 GC-SAMBAND-KTRL SECTION.                                                 
117900                                                                          
118000*    SAMBANDS-KONTROLL IDLOPNRM, IDRADNRM & KVINLART                      
118100                                                                          
118200     MOVE WS-UPD-IDLOPNRM TO W-IDLOPNRM                                   
118300     PERFORM IMS-GU-INLA-ART-SEQ                                          
118400                                                                          
118500     IF SEGMENT-FINNS                                                     
118600       IF INLA-ART-FLKVAFEL = JA OR INLA-ART-FLKVAKAR = JA                
118700         MOVE ERR-KVAL-FEL TO MED-IDMFSFEL                                
118800         CALL WMEDKONV USING MED-WMEDAREA                                 
118900         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
119000         PERFORM MFS-STAENG-UPD-ALLA                                      
119100         MOVE JA           TO KVAL-FEL-SW                                 
119200         MOVE NEJ          TO INDATA-SW                                   
119300       ELSE                                                               
119400          MOVE WS-UPD-IDRADNR            TO W-IDRADNR                     
119500          PERFORM IMS-GHNP-RAD                                            
119600                                                                          
119700          IF SEGMENT-FINNS                                                
119800          AND (INLA-RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK')               
119900                                                                          
120000            IF SW-NKLTYP-LEV-KLI = JA                                     
120100            AND (INLA-RAD-IDOKOLLI NOT = WS-IDOKOLLI-NUM                  
120200            OR INLA-RAD-IDLEVNR-KOLLI NOT = WS-IDLEVNR-KOLLI)             
120300*             -- IDLEVNR/IDOKOLLI I RAD OLIK NYCKEL                       
120400              MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-UPD-ATTR             
120500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR              
120600              MOVE NEJ TO INDATA-SW                                       
120700              MOVE NEJ TO SW-OK-IDLOPNRM                                  
120800              MOVE NEJ TO SW-OK-IDRADNR                                   
120900              IF MED-IDMFSFEL = SPACE                                     
121000                 MOVE ERR-007-OTILLATEN-UPD TO MED-IDMFSFEL               
121100              END-IF                                                      
121200            ELSE                                                          
121300              IF SW-OK-KVINLART = JA                                      
121400                IF WS-UPD-KVINLART > INLA-RAD-KVINLART                    
121500                  MOVE MFS-NUM-FAELT-FEL TO MOD-KVINLART-UPD-ATTR         
121600                  MOVE NEJ TO INDATA-SW                                   
121700                  MOVE NEJ TO SW-OK-KVINLART                              
121800                  IF MED-IDMFSFEL = SPACE                                 
121900                    MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL             
122000                  END-IF                                                  
122100                END-IF                                                    
122200              END-IF                                                      
122300            END-IF                                                        
122400                                                                          
122500          ELSE                                                            
122600*         -- IDRADNR SAKNAS I INLA-RAD                                    
122700            MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                
122800            MOVE NEJ TO INDATA-SW                                         
122900            MOVE NEJ TO SW-OK-IDRADNR                                     
123000            IF MED-IDMFSFEL = SPACE                                       
123100               MOVE ERR-010-NOT-IN-REG TO MED-IDMFSFEL                    
123200            END-IF                                                        
123300          END-IF                                                          
123400       END-IF                                                             
123500     ELSE                                                                 
123600       MOVE NEJ TO INDATA-SW                                              
123700*        -- IDLOPNRM SAKNAS I INLA-ART                                    
123800       MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-UPD-ATTR                    
123900       MOVE NEJ TO SW-OK-IDLOPNRM                                         
124000       IF MED-IDMFSFEL = SPACE                                            
124100         MOVE ERR-010-NOT-IN-REG TO MED-IDMFSFEL                          
124200       END-IF                                                             
124300     END-IF                                                               
124310     PERFORM GCA-CHECK-VVCO                                               
124400     .                                                                    
124500     EJECT                                                                
124600 GCA-CHECK-VVCO  SECTION.                                                 
124610     IF INLA-ART-VKART = ZERO                                             
124620       MOVE NEJ                TO INDATA-SW                               
124640       MOVE ERR-WEIGHT-MISSING TO MED-IDMFSFEL                            
124650     END-IF                                                               
124660                                                                          
124670     IF INLA-ART-VLARTNTO = ZERO                                          
124680       MOVE NEJ                TO INDATA-SW                               
124691       MOVE ERR-VOLUME-MISSING TO MED-IDMFSFEL                            
124692     END-IF                                                               
124693                                                                          
124694     IF INLA-ART-KDARTURS = SPACE                                         
124695       MOVE NEJ                TO INDATA-SW                               
124697       MOVE ERR-ORIGIN-MISSING TO MED-IDMFSFEL                            
124698     END-IF                                                               
124699     .                                                                    
124700     EJECT                                                                
124710 GD-KTRL-ADINLOMR-PRT SECTION.                                            
124800                                                                          
124900*    KONTROLL ADINLOMR-PRT                                                
125000                                                                          
125100     MOVE SPACE TO WS-UPD-ADINLOMR-PRT                                    
125200     IF MID-ADINLOMR-PRT-UPD = ALL '+'                                    
125300     OR MID-ADINLOMR-PRT-UPD = SPACE                                      
125400       CONTINUE                                                           
125500     ELSE                                                                 
125600       MOVE MID-ADINLOMR-PRT-UPD TO W-6006-ADINLOMR                       
125700       PERFORM IMS-GU-PLAA-6006                                           
125800       IF SEGMENT-FINNS                                                   
125900         MOVE SPACE          TO PRT-IDPRTLST                              
126000         IF SW-NKLTYP-LEV-KLI = JA                                        
126100           MOVE '6F'           TO PRT-IDPRTLST (1:2)                      
126200         ELSE                                                             
126300           MOVE '6E'           TO PRT-IDPRTLST (1:2)                      
126400         END-IF                                                           
126500         MOVE MID-ADINLOMR-PRT-UPD TO PRT-IDPRTLST (3:6)                  
126600         MOVE 1              TO PRT-KDCALL                                
126700         CALL W006PRT USING PRT-W006PRT                                   
126800         IF PRT-KDSVAR = 'F'                                              
126900           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-UPD-ATTR           
127000           MOVE NEJ TO INDATA-SW                                          
127100           MOVE NEJ TO SW-OK-ADINLOMR-PRT                                 
127200           IF MED-IDMFSFEL = SPACE                                        
127300              MOVE ERR-772-PRT-FEL TO MED-IDMFSFEL                        
127400           END-IF                                                         
127500         ELSE                                                             
127600           IF PRT-IDPRTLST (1:2) = '6F'   AND                             
127700              INLA-RAD-FLDIVKLI  = JA                                     
127800             MOVE '6E' TO PRT-IDPRTLST (1:2)                              
127900             CALL W006PRT USING PRT-W006PRT                               
128000             IF PRT-KDSVAR = 'F'                                          
128100               MOVE MFS-ALFA-FAELT-FEL TO                                 
128200                                      MOD-ADINLOMR-PRT-UPD-ATTR           
128300               MOVE NEJ TO INDATA-SW                                      
128400               MOVE NEJ TO SW-OK-ADINLOMR-PRT                             
128500               IF MED-IDMFSFEL = SPACE                                    
128600                  MOVE ERR-772-PRT-FEL TO MED-IDMFSFEL                    
128700               END-IF                                                     
128800             ELSE                                                         
128900               MOVE MFS-ALFA-FAELT-RAETT                                  
129000                                   TO MOD-ADINLOMR-PRT-UPD-ATTR           
129100               MOVE MID-ADINLOMR-PRT-UPD TO WS-UPD-ADINLOMR-PRT           
129200             END-IF                                                       
129300           ELSE                                                           
129400             MOVE MFS-ALFA-FAELT-RAETT                                    
129500                                 TO MOD-ADINLOMR-PRT-UPD-ATTR             
129600             MOVE MID-ADINLOMR-PRT-UPD TO WS-UPD-ADINLOMR-PRT             
129700           END-IF                                                         
129800         END-IF                                                           
129900       ELSE                                                               
130000         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-UPD-ATTR             
130100         MOVE NEJ TO INDATA-SW                                            
130200         MOVE NEJ TO SW-OK-ADINLOMR-PRT                                   
130300         IF MED-IDMFSFEL = SPACE                                          
130400            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
130500         END-IF                                                           
130600       END-IF                                                             
130700     END-IF                                                               
130800     .                                                                    
130900     EJECT                                                                
131000 H-UPPDATERA SECTION.                                                     
131100                                                                          
131200     PERFORM MFS-FORM-ATTR                                                
131300     PERFORM HA-SPARA-GML-RAD                                             
131400     PERFORM HB-INIT-BACKGR-TR                                            
131500                                                                          
131600     IF  INLA-RAD-KDINLSTA = 'SAK'                                        
131700       MOVE SPACE                TO INLA-RAD-KDINLSTA                     
131800     END-IF                                                               
131900                                                                          
132000     SUBTRACT WS-UPD-KVINLART    FROM INLA-RAD-KVINLART                   
132100                                                                          
132200     IF  WS-UPD-ADINLOMR-PRT NOT = SPACE                                  
132300     AND INLA-RAD-IDRADNR > +1                                            
132400      IF INLA-RAD-FLDIVKLI = NEJ OR                                       
132500         INLA-ART-IDDC     = WC-CDC-TR                                    
132600                                                                          
132700       IF INLA-RAD-IDOKOLLI > +0                                          
132800         IF (INLA-RAD-IDLEVNR-KOLLI > '99399'                             
132900         AND INLA-RAD-IDLEVNR-KOLLI < '99600')                            
133000         OR  INLA-RAD-IDLEVNR-KOLLI = '3324 '                             
133100             CONTINUE                                                     
133200         ELSE                                                             
133300           MOVE PLAA-6006-IDLEVNR      TO INLA-RAD-IDLEVNR-KOLLI          
133400           PERFORM IMS-GHU-LOPA-6018                                      
133500           ADD 1                       TO LOPA-6018-IDOKOLLI              
133600           PERFORM IMS-REPL-LOPA                                          
133700           MOVE LOPA-6018-IDOKOLLI     TO INLA-RAD-IDOKOLLI               
133800                                                                          
133900           IF    SW-NKLTYP-LEV-KLI = JA                                   
134000             PERFORM HC-BYT-NKL-LEV-KLI                                   
134100           END-IF                                                         
134200         END-IF                                                           
134300                                                                          
134400         PERFORM HD-TRANS-6194-FLAGG                                      
134500         IF INLA-RAD-FLDIVKLI = JA                                        
134600           PERFORM HL-TRANS-6195-ETIKETT                                  
134700         END-IF                                                           
134800       ELSE                                                               
134900         PERFORM HL-TRANS-6195-ETIKETT                                    
135000       END-IF                                                             
135100      END-IF                                                              
135200     END-IF                                                               
135300                                                                          
135400     PERFORM HE-TRANS-6191-GML                                            
135500                                                                          
135600     IF  INLA-RAD-KVINLART > ZERO                                         
135700       PERFORM IMS-REPL-RAD                                               
135800       PERFORM HH-NYTT-RADNR                                              
135900       PERFORM HG-REDIG-RAD-VOR                                           
136000       PERFORM IMS-ISRT-RAD                                               
136100       PERFORM HL-VISN-FOM-UPPD-RAD                                       
136200     ELSE                                                                 
136300       IF  INLA-RAD-IDRADNR = +1                                          
136400         PERFORM IMS-DLET-RAD                                             
136500         PERFORM HH-NYTT-RADNR                                            
136600         PERFORM HG-REDIG-RAD-VOR                                         
136700         PERFORM IMS-ISRT-RAD                                             
136800         PERFORM HM-VISN-FOM-ENTER-NKL                                    
136900       ELSE                                                               
137000         PERFORM HG-REDIG-RAD-VOR                                         
137100         PERFORM IMS-REPL-RAD                                             
137200         PERFORM HL-VISN-FOM-UPPD-RAD                                     
137300       END-IF                                                             
137400     END-IF                                                               
137500                                                                          
137600     PERFORM HI-TRANS-6191-VOR                                            
137700     PERFORM HJ-TRANS-6193-VOR                                            
137800                                                                          
137900     PERFORM HK-SKRIV-BACKGR-TR                                           
138000                                                                          
138100     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
138200     CALL WMEDKONV USING MED-WMEDAREA                                     
138300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
138400     PERFORM MFS-RENSA-FAELT-IN                                           
138500* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
138600     .                                                                    
138700     EJECT                                                                
138800 HA-SPARA-GML-RAD SECTION.                                                
138900                                                                          
139000     MOVE INLA-RAD-ADINLOMR      TO WS-GML-ADINLOMR                       
139100     MOVE INLA-RAD-ADINLOMR-NXT  TO WS-GML-ADINLOMR-NXT                   
139200     MOVE INLA-RAD-KDINLSTA      TO WS-GML-KDINLSTA                       
139300     MOVE INLA-RAD-KVINLART      TO WS-GML-KVINLART                       
139400     .                                                                    
139500     EJECT                                                                
139600 HB-INIT-BACKGR-TR SECTION.                                               
139700                                                                          
139800     MOVE SPACE                  TO MOD6191-MID-W6I19101                  
139900     MOVE IDPGM                  TO MOD6191-MID-IDPGM                     
140000     MOVE DCS-IDDC               TO MOD6191-MID-IDDC                      
140100     MOVE ZERO                   TO IX-6191                               
140200                                                                          
140300     MOVE SPACE                  TO MOD6194-MID-W6I19401                  
140400     MOVE '6F'                   TO PRT-IDPRTLST (1:2)                    
140500     MOVE PRT-IDPRTLST           TO MOD6194-MID-IDPRTLST                  
140600     MOVE IDPGM                  TO MOD6194-MID-IDPGM                     
140700     MOVE ZERO                   TO IX-6194                               
140800                                                                          
140900     MOVE SPACE                  TO MOD6195-MID-W6I19501                  
141000     MOVE '6E'                   TO PRT-IDPRTLST (1:2)                    
141100     MOVE PRT-IDPRTLST           TO MOD6195-MID-IDPRTLST                  
141200     MOVE IDPGM                  TO MOD6195-MID-IDPGM                     
141300     MOVE ZERO                   TO IX-6195                               
141400     .                                                                    
141500     EJECT                                                                
141600 HC-BYT-NKL-LEV-KLI SECTION.                                              
141700                                                                          
141800     MOVE INLA-RAD-IDLEVNR-KOLLI TO WS-IDLEVNR-KOLLI                      
141900     MOVE INLA-RAD-IDOKOLLI      TO WS-IDOKOLLI                           
142000                                                                          
142100     MOVE WS-IDLEVNR-KOLLI       TO W-W6D1C1KY-MIN-IDLEVNR-KOLLI          
142200                                    W-W6D1C1KY-MAX-IDLEVNR-KOLLI          
142300     MOVE WS-IDOKOLLI            TO W-W6D1C1KY-MIN-IDOKOLLI               
142400                                    W-W6D1C1KY-MAX-IDOKOLLI               
142500                                                                          
142600     MOVE WS-IDOKOLLI            TO WS-IDOKOLLI-NUM                       
142700                                                                          
142800     MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                        
142900     MOVE WS-IDOKOLLI TO MOD-IDOKOLLI-UT                                  
143000     INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE              
143100     .                                                                    
143200     EJECT                                                                
143300 HD-TRANS-6194-FLAGG SECTION.                                             
143400                                                                          
143500     PERFORM HDA-TA-FRAM-TIINLMOT                                         
143600                                                                          
143700     ADD +1                      TO IX-6194                               
143800     MOVE IX-6194                TO MOD6194-MID-KVPOST                    
143900                                                                          
144000     MOVE INLA-ART-IDARTNR       TO MOD6194-MID-IDARTNR  (IX-6194)        
144100     MOVE INLA-ART-IDLOPNRM      TO MOD6194-MID-IDLOPNRM (IX-6194)        
144200     MOVE INLA-RAD-KVINLART      TO MOD6194-MID-KVINLART (IX-6194)        
144300     MOVE INLA-RAD-IDLEVNR-KOLLI TO MOD6194-MID-IDLEVNR-KOLLI             
144400                                                         (IX-6194)        
144500     MOVE INLA-RAD-IDOKOLLI      TO MOD6194-MID-IDOKOLLI (IX-6194)        
144600     MOVE WS-TIINLMOT            TO MOD6194-MID-TIINLMOT (IX-6194)        
144700     COMPUTE MOD6194-MID-VKKOLLIN (IX-6194) ROUNDED                       
144800         = (INLA-RAD-KVINLART * INLA-ART-VKART) / 1000                    
144900     END-COMPUTE                                                          
145000     MOVE ZERO                   TO MOD6194-MID-VKKOLLIB (IX-6194)        
145100     MOVE INLA-ART-ADLAGOMR      TO MOD6194-MID-ADLAGOMR (IX-6194)        
145200     MOVE INLA-ART-ADGANG        TO MOD6194-MID-ADGANG   (IX-6194)        
145300     MOVE INLA-ART-ADPLATS       TO MOD6194-MID-ADPLATS  (IX-6194)        
145400     MOVE INLA-ART-KDSORT        TO MOD6194-MID-KDSORT   (IX-6194)        
145500     MOVE INLA-ART-BEFT          TO MOD6194-MID-BEFT     (IX-6194)        
145600     .                                                                    
145700     EJECT                                                                
145800 HDA-TA-FRAM-TIINLMOT SECTION.                                            
145900                                                                          
146000     ACCEPT DAGENS-DATUM FROM DATE                                        
146100                                                                          
146200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
146300     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
146400                                                                          
146500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
146600                         DAT-O-TIDATUM DAT-KDSVAR                         
146700                                                                          
146800     IF DAT-KDSVAR-OK                                                     
146900       MOVE DAT-TIVV             TO WS-VECKA                              
147000       MOVE INLA-ART-IDLOPNRM    TO WS-IDLOPNRM-DAT                       
147100       IF WS-IDLOPNRM(1:2)       >  WS-VECKA                              
147200** SPECIALLÖSNING FÖR ÅR 2000                                             
147300          IF DAGENS-AA = 0                                                
147400             MOVE +99 TO WS-AA                                            
147500          ELSE                                                            
147600             COMPUTE WS-AA          =  DAGENS-AA - 1                      
147700          END-IF                                                          
147800       ELSE                                                               
147900          COMPUTE WS-AA          =  DAGENS-AA                             
148000       END-IF                                                             
148100       MOVE WS-AA                    TO WS-TIAAVVD(1:2)                   
148200       MOVE WS-IDLOPNRM-DAT(2:2)     TO WS-TIAAVVD(3:2)                   
148300       MOVE WS-IDLOPNRM-DAT(4:1)     TO WS-TIAAVVD(5:1)                   
148400                                                                          
148500       MOVE 'AAVVD '             TO DAT-KDDATFORM                         
148600       MOVE WS-TIAAVVD           TO DAT-I-TIDATUM                         
148700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
148800                           DAT-O-TIDATUM DAT-KDSVAR                       
148900                                                                          
149000       IF DAT-KDSVAR-OK                                                   
149100          MOVE DAT-TIAAMMDD      TO WS-TIINLMOT                           
149200       ELSE                                                               
149300          MOVE 'FEL FRÅN DATKONV1 I HDA- ' TO FELTEXT                     
149400          CALL FELLOG                                                     
149500       END-IF                                                             
149600     ELSE                                                                 
149700       MOVE 'FEL FRÅN DATKONV2 I HDA- ' TO FELTEXT                        
149800       CALL FELLOG                                                        
149900     END-IF                                                               
150000     .                                                                    
150100     EJECT                                                                
150200                                                                          
150300 HE-TRANS-6191-GML SECTION.                                               
150400                                                                          
150500     ADD +1                      TO IX-6191                               
150600     MOVE IX-6191                TO MOD6191-MID-KVPOST                    
150700                                                                          
150800     MOVE INLA-ART-IDLOPNRM      TO MOD6191-MID-IDLOPNRM (IX-6191)        
150900     MOVE INLA-RAD-IDRADNR       TO MOD6191-MID-IDRADNR  (IX-6191)        
151000     MOVE INLA-RAD-KDINLPRIO     TO MOD6191-MID-KDINLPRIO                 
151100                                                         (IX-6191)        
151200     MOVE INLA-ART-PRARTSTD      TO MOD6191-MID-PRARTSTD (IX-6191)        
151300                                                                          
151400     MOVE +0                     TO MOD6191-MID-KVKOLLI  (IX-6191)        
151500     MOVE 'N'                    TO MOD6191-MID-FLINLI   (IX-6191)        
151600                                                                          
151700     MOVE WS-GML-ADINLOMR        TO MOD6191-MID-ADINLOMR-OLD              
151800                                                         (IX-6191)        
151900     MOVE WS-GML-ADINLOMR-NXT    TO MOD6191-MID-ADINLOMR-NXT-OLD          
152000                                                         (IX-6191)        
152100     MOVE WS-GML-KDINLSTA        TO MOD6191-MID-KDINLSTA-OLD              
152200                                                         (IX-6191)        
152300     MOVE WS-GML-KVINLART        TO MOD6191-MID-KVINLART-OLD              
152400                                                         (IX-6191)        
152500                                                                          
152600     MOVE INLA-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-NEW              
152700                                                         (IX-6191)        
152800     MOVE INLA-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-NEW          
152900                                                         (IX-6191)        
153000     MOVE INLA-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-NEW              
153100                                                         (IX-6191)        
153200     MOVE INLA-RAD-KVINLART      TO MOD6191-MID-KVINLART-NEW              
153300                                                         (IX-6191)        
153400     .                                                                    
153500     EJECT                                                                
153600 HG-REDIG-RAD-VOR SECTION.                                                
153700                                                                          
153800*    -- FÄLT LIKA SOM I URSPRUNGSRAD (KVAR OFÖRÄNDRADE I IO-AREA):        
153900*         FLKVAANT,IDLEVNR-KOLLI,IDOKOLLI,KDINLPRIO.                      
154000                                                                          
154100     MOVE SPACE                  TO INLA-RAD-ADINLOMR                     
154200     MOVE SPACE                  TO INLA-RAD-ADINLOMR-NXT                 
154300     MOVE NEJ                    TO INLA-RAD-FLINLFP                      
154400                                                                          
154500     MOVE NEJ                    TO INLA-RAD-FLSVSLS                      
154600     MOVE NEJ                    TO INLA-RAD-FLPRIO                       
154700     MOVE NEJ                    TO INLA-RAD-FLSATS                       
154800     MOVE NEJ                    TO INLA-RAD-FLINLFB                      
154900     MOVE ZERO                   TO INLA-RAD-IDANSTNR                     
155000     MOVE ZERO                   TO INLA-RAD-IDILIRAD                     
155100     MOVE ZERO                   TO INLA-RAD-IDILIST                      
155200     MOVE ZERO                   TO INLA-RAD-IDINLVGN                     
155300                                                                          
155400     MOVE 'VOR'                  TO INLA-RAD-KDINLSTA                     
155500     MOVE WS-UPD-KVINLART        TO INLA-RAD-KVINLART                     
155600     MOVE ZERO                   TO INLA-RAD-TIUPPDAT                     
155700     .                                                                    
155800     EJECT                                                                
155900 HH-NYTT-RADNR SECTION.                                                   
156000                                                                          
156100     PERFORM IMS-GNP-RAD-LAST                                             
156200                                                                          
156300     IF  SEGMENT-FINNS                                                    
156400       COMPUTE INLA-RAD-IDRADNR  = INLA-LA-RAD-IDRADNR + 1                
156500     ELSE                                                                 
156600       ADD +1                    TO INLA-RAD-IDRADNR                      
156700     END-IF                                                               
156800     .                                                                    
156900     EJECT                                                                
157000 HI-TRANS-6191-VOR SECTION.                                               
157100                                                                          
157200     ADD +1                      TO IX-6191                               
157300     MOVE IX-6191                TO MOD6191-MID-KVPOST                    
157400                                                                          
157500     MOVE INLA-ART-IDLOPNRM      TO MOD6191-MID-IDLOPNRM (IX-6191)        
157600     MOVE INLA-RAD-IDRADNR       TO MOD6191-MID-IDRADNR  (IX-6191)        
157700     MOVE INLA-RAD-KDINLPRIO     TO MOD6191-MID-KDINLPRIO                 
157800                                                         (IX-6191)        
157900     MOVE INLA-ART-PRARTSTD      TO MOD6191-MID-PRARTSTD (IX-6191)        
158000     MOVE +0                     TO MOD6191-MID-KVKOLLI  (IX-6191)        
158100     MOVE 'N'                    TO MOD6191-MID-FLINLI   (IX-6191)        
158200                                                                          
158300     MOVE SPACE                  TO MOD6191-MID-ADINLOMR-OLD              
158400                                                         (IX-6191)        
158500     MOVE SPACE                  TO MOD6191-MID-ADINLOMR-NXT-OLD          
158600                                                         (IX-6191)        
158700     MOVE SPACE                  TO MOD6191-MID-KDINLSTA-OLD              
158800                                                         (IX-6191)        
158900     MOVE ZERO                   TO MOD6191-MID-KVINLART-OLD              
159000                                                         (IX-6191)        
159100                                                                          
159200     MOVE INLA-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-NEW              
159300                                                         (IX-6191)        
159400     MOVE INLA-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-NEW          
159500                                                         (IX-6191)        
159600     MOVE INLA-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-NEW              
159700                                                         (IX-6191)        
159800     MOVE INLA-RAD-KVINLART      TO MOD6191-MID-KVINLART-NEW              
159900                                                         (IX-6191)        
160000     .                                                                    
160100     EJECT                                                                
160200 HJ-TRANS-6193-VOR SECTION.                                               
160300                                                                          
160400     MOVE SPACE                  TO 6193-MID-W6I19301                     
160500     MOVE INLA-ART-IDLOPNRM      TO 6193-MID-IDLOPNRM                     
160600     MOVE INLA-RAD-IDRADNR       TO 6193-MID-IDRADNR                      
160700                                                                          
160800     PERFORM S02-P-TO-P-6193                                              
160900     .                                                                    
161000     EJECT                                                                
161100 HK-SKRIV-BACKGR-TR SECTION.                                              
161200                                                                          
161300     IF  IX-6191 > ZERO                                                   
161400       PERFORM S01-P-TO-P-6191                                            
161500     END-IF                                                               
161600                                                                          
161700     IF  IX-6194 > ZERO                                                   
161800       PERFORM S03-P-TO-P-6194                                            
161900       MOVE PRT-BEPRTLST         TO MOD-TEMFSFEL                          
162000     END-IF                                                               
162100                                                                          
162200     IF  IX-6195 > ZERO                                                   
162300       PERFORM S04-P-TO-P-6195                                            
162400       MOVE PRT-BEPRTLST         TO MOD-TEMFSFEL                          
162500     END-IF                                                               
162600     .                                                                    
162700     EJECT                                                                
162800 HL-VISN-FOM-UPPD-RAD SECTION.                                            
162900                                                                          
163000*    -- VISNING EFTER REPL GÖRS FOM UPPDATERAD RAD                        
163100                                                                          
163200     MOVE INLA-ART-IDRADNR-INL   TO W-W6D1C1KY-MIN-IDRADNR-INL            
163300     MOVE W-W6D101KY-IDLEVNR     TO W-W6D1C1KY-MIN-IDLEVNR                
163400     MOVE W-W6D101KY-IDFS        TO W-W6D1C1KY-MIN-IDFS                   
163500     MOVE W-W6D101KY-TIAVIDAT    TO W-W6D1C1KY-MIN-TIAVIDAT               
163600     MOVE W-IDDC                 TO W-W6D1C1KY-MIN-IDDC                   
163700                                                                          
163800* ANTAL LYSES UPP I F- SECTION FÖR UPPDATERING AV (ETIKETTER)             
163900     IF (MID-IDLOPNRM-UPD     = ALL '+') AND                              
164000        (MID-IDRADNR-UPD  NOT = ALL '+')                                  
164100       CONTINUE                                                           
164200     ELSE                                                                 
164300       PERFORM MFS-LYS-UPD-ANT                                            
164400     END-IF                                                               
164500     .                                                                    
164600     EJECT                                                                
164700 HM-VISN-FOM-ENTER-NKL SECTION.                                           
164800                                                                          
164900*    -- VISNING EFTER DLET GÖRS FOM ENTER-NYCKLAR                         
165000                                                                          
165100     MOVE MID-IDRADNR-INL-ENTER  TO W-W6D1C1KY-MIN-IDRADNR-INL            
165200     MOVE MID-IDLEVNR-ENTER      TO W-W6D1C1KY-MIN-IDLEVNR                
165300     MOVE MID-IDFS-ENTER         TO W-W6D1C1KY-MIN-IDFS                   
165400     MOVE MID-TIAVIDAT-ENTER     TO W-W6D1C1KY-MIN-TIAVIDAT               
165500     MOVE W-IDDC                 TO W-W6D1C1KY-MIN-IDDC                   
165600     .                                                                    
165700     EJECT                                                                
165800 HL-TRANS-6195-ETIKETT SECTION.                                           
165900                                                                          
166000     ADD +1                      TO IX-6195                               
166100     MOVE IX-6195                TO MOD6195-MID-KVPOST                    
166200                                                                          
166300     MOVE INLA-ART-IDARTNR       TO MOD6195-MID-IDARTNR  (IX-6195)        
166400     MOVE INLA-ART-IDLOPNRM      TO MOD6195-MID-IDLOPNRM (IX-6195)        
166500     MOVE INLA-RAD-KVINLART      TO MOD6195-MID-KVINLART (IX-6195)        
166600     MOVE INLA-RAD-IDRADNR       TO MOD6195-MID-IDRADNR  (IX-6195)        
166700     MOVE INLA-ART-ADLAGOMR      TO MOD6195-MID-ADLAGOMR (IX-6195)        
166800     MOVE INLA-ART-ADGANG        TO MOD6195-MID-ADGANG   (IX-6195)        
166900     MOVE INLA-ART-ADPLATS       TO MOD6195-MID-ADPLATS  (IX-6195)        
167000     MOVE INLA-ART-BEART         TO MOD6195-MID-BEART    (IX-6195)        
167100     .                                                                    
167200     EJECT                                                                
167300 S01-P-TO-P-6191 SECTION.                                                 
167400                                                                          
167500     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
167600                               + 17 + (MOD6191-MID-KVPOST * 64)           
167700                                                                          
167800     MOVE 'W6T191X '           TO P-TO-P-KDTRANS                          
167900     MOVE '6104'               TO P-TO-P-IDTRANS                          
168000     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
168100                                                                          
168200     MOVE MOD6191-MID-W6I19101 TO P-TO-P-DATA                             
168300                                                                          
168400     PERFORM IMS-ISRT-ALT-MSG-6191                                        
168500     .                                                                    
168600     EJECT                                                                
168700 S02-P-TO-P-6193 SECTION.                                                 
168800                                                                          
168900     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
169000     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
169100     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
169200     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
169300     MOVE SPACE                TO MSG-KOM-KDTRANS                         
169400     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
169500     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
169600     MOVE 'W6010400'           TO MSG-KOM-IDSNDJOB                        
169700     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
169800     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
169900     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
170000                                                                          
170100     MOVE MAX-MOD-LAENGD-6193  TO KMSG-KVLL                               
170200*****CTEXTLÄNGD + KMSG-AREA                                               
170300     MOVE 'W6T193X '           TO KMSG-KDTRANS-1                          
170400     MOVE '6104'               TO KMSG-IDTRANS-1                          
170500     MOVE MFS-KDMFSFOR         TO KMSG-KDMFSFOR-1                         
170600                                                                          
170700     CALL W006KOM USING MSG-PCB                                           
170800                        DISP-PCB                                          
170900                        KOM-KOMA-PCB                                      
171000                        MSG-KOM-WMSGKOM                                   
171100                        KMSG-IO-AREA                                      
171200                                                                          
171300     .                                                                    
171400     EJECT                                                                
171500 S03-P-TO-P-6194 SECTION.                                                 
171600                                                                          
171700     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
171800                               + 23 + (MOD6194-MID-KVPOST * 67)           
171900                                                                          
172000     MOVE 'W6T194X '           TO P-TO-P-KDTRANS                          
172100     MOVE '6104'               TO P-TO-P-IDTRANS                          
172200     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
172300                                                                          
172400     MOVE MOD6194-MID-W6I19401 TO P-TO-P-DATA                             
172500                                                                          
172600     PERFORM IMS-ISRT-ALT-MSG-6194                                        
172700     .                                                                    
172800     EJECT                                                                
172900 S04-P-TO-P-6195 SECTION.                                                 
173000                                                                          
173100     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
173200                               + 23 + (MOD6195-MID-KVPOST * 61)           
173300                                                                          
173400     MOVE 'W6T195X '           TO P-TO-P-KDTRANS                          
173500     MOVE '6104'               TO P-TO-P-IDTRANS                          
173600     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
173700                                                                          
173800     MOVE MOD6195-MID-W6I19501 TO P-TO-P-DATA                             
173900                                                                          
174000     PERFORM IMS-ISRT-ALT-MSG-6195                                        
174100     .                                                                    
174200     EJECT                                                                
174300 MFS-RENSA-FAELT-UT SECTION.                                              
174400                                                                          
174500*    --- ALLA UTDATA-FÄLT                                                 
174600*    --- INKL. BLÄDDRINGSNYCKLAR                                          
174700     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-INL-ENTER                        
174800                             MOD-IDLEVNR-ENTER                            
174900                             MOD-IDFS-ENTER                               
175000                             MOD-TIAVIDAT-ENTER                           
175100                             MOD-IDRADNR-INL-NEXT                         
175200                             MOD-IDLEVNR-NEXT                             
175300                             MOD-IDFS-NEXT                                
175400                             MOD-TIAVIDAT-NEXT                            
175500                             MOD-IDLOPNRM-DEF                             
175600                             MOD-IDLOPNRM-DEF                             
175700     MOVE +1 TO INDX                                                      
175800     PERFORM UNTIL INDX > MAX-INDX                                        
175900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
176000       ADD +1 TO INDX                                                     
176100     END-PERFORM                                                          
176200     .                                                                    
176300     SKIP2                                                                
176400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
176500                                                                          
176600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
176700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD  (INDX)                      
176800                             MOD-BEART-RAD    (INDX)                      
176900                             MOD-KVINLART-RAD (INDX)                      
177000                             MOD-IDLOPNRM-RAD (INDX)                      
177100                             MOD-IDRADNR-RAD  (INDX)                      
177200                             MOD-BEFARLIG-RAD (INDX)                      
177300     .                                                                    
177400     SKIP2                                                                
177500 MFS-RENSA-FAELT-IN SECTION.                                              
177600                                                                          
177700*    --- ALLA INDATA-FÄLT                                                 
177800     MOVE MFS-RENSA-FAELT TO MOD-KVINLART-UPD                             
177900                             MOD-IDLOPNRM-UPD                             
178000                             MOD-IDRADNR-UPD                              
178100                             MOD-ADINLOMR-PRT-UPD                         
178200     .                                                                    
178300     EJECT                                                                
178400 MFS-RENSA-NYCKLAR-UT SECTION.                                            
178500                                                                          
178600*    --- ALLA NYCKELFÄLT UT                                               
178700     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-UT                         
178800                             MOD-IDOKOLLI-UT                              
178900                             MOD-IDLOPNRM-UT                              
179000                             MOD-IDDC-UT                                  
179100     .                                                                    
179200     EJECT                                                                
179300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
179400                                                                          
179500*    --- ALLA UTDATA-FÄLT                                                 
179600*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
179700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-INL-ENTER                      
179800                               MOD-IDLEVNR-ENTER                          
179900                               MOD-IDFS-ENTER                             
180000                               MOD-TIAVIDAT-ENTER                         
180100                               MOD-IDRADNR-INL-NEXT                       
180200                               MOD-IDLEVNR-NEXT                           
180300                               MOD-IDFS-NEXT                              
180400                               MOD-TIAVIDAT-NEXT                          
180500                               MOD-IDLOPNRM-DEF                           
180600                               MOD-IDRADNR-DEF                            
180700     MOVE +1 TO INDX                                                      
180800     PERFORM UNTIL INDX > MAX-INDX                                        
180900       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
181000       ADD +1 TO INDX                                                     
181100     END-PERFORM                                                          
181200     .                                                                    
181300     SKIP2                                                                
181400 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
181500                                                                          
181600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
181700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-RAD  (INDX)                    
181800                               MOD-BEART-RAD    (INDX)                    
181900                               MOD-KVINLART-RAD (INDX)                    
182000                               MOD-IDLOPNRM-RAD (INDX)                    
182100                               MOD-IDRADNR-RAD  (INDX)                    
182200                               MOD-BEFARLIG-RAD (INDX)                    
182300     .                                                                    
182400     SKIP2                                                                
182500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
182600                                                                          
182700*    --- ALLA INDATA-FÄLT                                                 
182800     MOVE MFS-ROER-EJ-FAELT TO MOD-KVINLART-UPD                           
182900                               MOD-IDLOPNRM-UPD                           
183000                               MOD-IDRADNR-UPD                            
183100                               MOD-ADINLOMR-PRT-UPD                       
183200     .                                                                    
183300     EJECT                                                                
183400 MFS-FORM-ATTR SECTION.                                                   
183500                                                                          
183600*    --- ALLA FÄLT MED ATTRIBUT                                           
183700     MOVE MFS-FORMATETS-ATTR TO MOD-KVINLART-UPD-ATTR                     
183800                                MOD-IDLOPNRM-UPD-ATTR                     
183900                                MOD-IDRADNR-UPD-ATTR                      
184000                                MOD-ADINLOMR-PRT-UPD-ATTR                 
184100     MOVE +1 TO INDX                                                      
184200     PERFORM UNTIL INDX > MAX-INDX                                        
184300       MOVE MFS-FORMATETS-ATTR TO MOD-KVINLART-RAD-ATTR (INDX)            
184400       ADD +1 TO INDX                                                     
184500     END-PERFORM                                                          
184600     .                                                                    
184700     SKIP2                                                                
184800 MFS-LAES-IN-IGEN SECTION.                                                
184900                                                                          
185000*    --- ALLA INDATA-FÄLT                                                 
185100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVINLART-UPD-ATTR                  
185200                                   MOD-IDLOPNRM-UPD-ATTR                  
185300                                   MOD-IDRADNR-UPD-ATTR                   
185400                                   MOD-ADINLOMR-PRT-UPD-ATTR              
185500     .                                                                    
185600     EJECT                                                                
185700 MFS-STAENG-UPD-LOP-RAD SECTION.                                          
185800                                                                          
185900*    --- STÄNG UPD-FÄLT LOPNR O RADNR                                     
186000     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDLOPNRM-UPD-ATTR                 
186100                                    MOD-IDRADNR-UPD-ATTR                  
186200     .                                                                    
186300     SKIP2                                                                
186400 MFS-STAENG-UPD-ALLA SECTION.                                             
186500                                                                          
186600*    --- STÄNG UPD-FÄLT LOPNR O RADNR                                     
186700     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDLOPNRM-UPD-ATTR                 
186800                                    MOD-IDRADNR-UPD-ATTR                  
186900                                    MOD-KVINLART-UPD-ATTR                 
187000                                    MOD-ADINLOMR-PRT-UPD-ATTR             
187100     .                                                                    
187200     SKIP2                                                                
187300 MFS-LYS-UPD-ANT SECTION.                                                 
187400                                                                          
187500*    --- LYS UPP KVINLART FÖR UPPDATERAD RAD                              
187600     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVINLART-RAD-ATTR (1)              
187700     .                                                                    
187800     EJECT                                                                
187900* --- IMS SEKTIONER ---                                                   
188000     SKIP3                                                                
188100 IMS-GET-MSG SECTION.                                                     
188200                                                                          
188300     MOVE '  QC' TO GODK-STATUSKODER                                      
188400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
188500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
188600     PERFORM IMS-STATUSKONTROLL                                           
188700     .                                                                    
188800     SKIP3                                                                
188900 IMS-INSERT-MSG SECTION.                                                  
189000                                                                          
189100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
189200       MOVE '0' TO MFS-KDHUVOMR                                           
189300     END-IF                                                               
189400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
189500     MOVE SPACE TO GODK-STATUSKODER                                       
189600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
189700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
189800     PERFORM IMS-STATUSKONTROLL                                           
189900     .                                                                    
190000     EJECT                                                                
190100 IMS-ISRT-ALT-MSG-6191 SECTION.                                           
190200                                                                          
190300     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
190400     MOVE    '  '             TO    GODK-STATUSKODER                      
190500     CALL    CBLTDLI          USING ISRT 6191-PCB P-TO-P-SW               
190600     MOVE    6191-STATUS-CODE TO    STATUS-WS                             
190700     PERFORM IMS-STATUSKONTROLL                                           
190800     .                                                                    
190900     SKIP3                                                                
191000 IMS-ISRT-ALT-MSG-6194 SECTION.                                           
191100                                                                          
191200     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
191300     MOVE    '  '             TO    GODK-STATUSKODER                      
191400     CALL    CBLTDLI          USING ISRT 6194-PCB P-TO-P-SW               
191500     MOVE    6194-STATUS-CODE TO    STATUS-WS                             
191600     PERFORM IMS-STATUSKONTROLL                                           
191700     .                                                                    
191800     EJECT                                                                
191900 IMS-ISRT-ALT-MSG-6195 SECTION.                                           
192000                                                                          
192100     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
192200     MOVE    '  '             TO    GODK-STATUSKODER                      
192300     CALL    CBLTDLI          USING ISRT 6195-PCB P-TO-P-SW               
192400     MOVE    6195-STATUS-CODE TO    STATUS-WS                             
192500     PERFORM IMS-STATUSKONTROLL                                           
192600     .                                                                    
192700     EJECT                                                                
192800 IMS-GU-INLD-SEQC SECTION.                                                
192900     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
193000                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X                        
193100                    '&IDDC     =' W-IDDC-X ')'                            
193200          DELIMITED BY SIZE INTO SSA1                                     
193300     MOVE '  GE' TO GODK-STATUSKODER                                      
193400     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
193500     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
193600     PERFORM IMS-STATUSKONTROLL                                           
193700     .                                                                    
193800     SKIP3                                                                
193900 IMS-GN-INLD-SEQC SECTION.                                                
194000     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
194100                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X                        
194200                    '&IDDC     =' W-IDDC-X ')'                            
194300          DELIMITED BY SIZE INTO SSA1                                     
194400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
194500     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA SSA1                      
194600     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
194700     PERFORM IMS-STATUSKONTROLL                                           
194800     .                                                                    
194900     SKIP3                                                                
195000 IMS-GU-INLA2-ART SECTION.                                                
195100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
195200          DELIMITED BY SIZE INTO SSA1                                     
195300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
195400          DELIMITED BY SIZE INTO SSA2                                     
195500     MOVE '  ' TO GODK-STATUSKODER                                        
195600     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA SSA1 SSA2                
195700     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
195800     PERFORM IMS-STATUSKONTROLL                                           
195900     .                                                                    
196000     SKIP3                                                                
196100 IMS-GNP-INLA2-RAD-OK SECTION.                                            
196200     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
196300          DELIMITED BY SIZE INTO SSA1                                     
196400     MOVE '  ' TO GODK-STATUSKODER                                        
196500     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
196600     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
196700     PERFORM IMS-STATUSKONTROLL                                           
196800     .                                                                    
196900     SKIP3                                                                
197000 IMS-GU-INLA-ART-SEQ SECTION.                                             
197100     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
197200                    '&IDDC     =' W-IDDC ')'                              
197300          DELIMITED BY SIZE INTO SSA1                                     
197400     MOVE '  GE' TO GODK-STATUSKODER                                      
197500     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1                      
197600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
197700     PERFORM IMS-STATUSKONTROLL                                           
197800     .                                                                    
197900     EJECT                                                                
198000 IMS-GNP-INLA-RAD SECTION.                                                
198100     MOVE 'W6INLA21' TO SSA1                                              
198200     MOVE '  GE' TO GODK-STATUSKODER                                      
198300     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA2 SSA1                    
198400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
198500     PERFORM IMS-STATUSKONTROLL                                           
198600     .                                                                    
198700     EJECT                                                                
198800 IMS-GHNP-RAD SECTION.                                                    
198900     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
199000          DELIMITED BY SIZE INTO SSA2                                     
199100     MOVE '  GE' TO GODK-STATUSKODER                                      
199200     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA2 SSA1 SSA2              
199300     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
199400     PERFORM IMS-STATUSKONTROLL                                           
199500     .                                                                    
199600     SKIP3                                                                
199700 IMS-GNP-RAD-LAST SECTION.                                                
199800     MOVE 'W6INLA21*L ' TO SSA2                                           
199900     MOVE '  GE' TO GODK-STATUSKODER                                      
200000     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA4 SSA2                    
200100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
200200     PERFORM IMS-STATUSKONTROLL                                           
200300     .                                                                    
200400     EJECT                                                                
200500 IMS-ISRT-RAD SECTION.                                                    
200600                                                                          
200700     MOVE 'W6INLA21 ' TO SSA1                                             
200800     MOVE '  ' TO GODK-STATUSKODER                                        
200900     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA2 SSA1                   
201000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
201100     PERFORM IMS-STATUSKONTROLL                                           
201200     .                                                                    
201300     SKIP3                                                                
201400 IMS-REPL-RAD SECTION.                                                    
201500                                                                          
201600     MOVE '  ' TO GODK-STATUSKODER                                        
201700     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA2                        
201800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
201900     PERFORM IMS-STATUSKONTROLL                                           
202000     .                                                                    
202100     SKIP3                                                                
202200 IMS-DLET-RAD SECTION.                                                    
202300                                                                          
202400     MOVE '  ' TO GODK-STATUSKODER                                        
202500     CALL CBLTDLI USING DLET INLA-PCB DLI-IO-AREA2                        
202600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
202700     PERFORM IMS-STATUSKONTROLL                                           
202800     .                                                                    
202900     EJECT                                                                
203000 IMS-GU-PLAA-6006 SECTION.                                                
203100     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
203200          DELIMITED BY SIZE INTO SSA1                                     
203300     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
203400          DELIMITED BY SIZE INTO SSA2                                     
203500     MOVE '  GE' TO GODK-STATUSKODER                                      
203600     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA5 SSA1 SSA2                
203700     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
203800     PERFORM IMS-STATUSKONTROLL                                           
203900     .                                                                    
204000     EJECT                                                                
204100 IMS-GHU-LOPA-6018 SECTION.                                               
204200     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
204300          DELIMITED BY SIZE INTO SSA1                                     
204400     MOVE 'W6LOPA11 ' TO SSA2                                             
204500     MOVE '  ' TO GODK-STATUSKODER                                        
204600     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA6 SSA1 SSA2               
204700     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
204800     PERFORM IMS-STATUSKONTROLL                                           
204900     .                                                                    
205000     SKIP3                                                                
205100 IMS-REPL-LOPA SECTION.                                                   
205200     MOVE '  ' TO GODK-STATUSKODER                                        
205300     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA6                        
205400     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
205500     PERFORM IMS-STATUSKONTROLL                                           
205600     .                                                                    
205700     EJECT                                                                
205800 IMS-GU-WDB601    SECTION.                                                
205900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
206000          DELIMITED BY SIZE INTO SSA1                                     
206100     MOVE '  GE' TO GODK-STATUSKODER                                      
206200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
206300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
206400     PERFORM IMS-STATUSKONTROLL                                           
206500     IF SEGMENT-SAKNAS                                                    
206600         MOVE SPACE TO DCS-KDDC                                           
206700     END-IF                                                               
206800     .                                                                    
206900 IMS-STATUSKONTROLL SECTION.                                              
207000                                                                          
207100     SET STATUS-IX TO 1                                                   
207200     SEARCH GODK-STATUS                                                   
207300       AT END                                                             
207400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
207500         DELIMITED BY SIZE INTO FELTEXT                                   
207600         CALL FELLOG                                                      
207700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
207800         CONTINUE                                                         
207900     END-SEARCH                                                           
208000     .                                                                    
