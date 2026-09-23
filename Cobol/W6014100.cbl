000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014100.                                                
000400*AUTHOR.         PER BERGH.                                               
000500*DATE-WRITTEN.   92/11/17.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SKAPAR UNDERLAG FÖR UTSKRIFT AV                       
001100*        INLÄGGNINGS- PLATSSÄTTAR- OCH RETURLISTA.                        
001200*        (UTSKRIFTSPGM: W60199; W6019A; W611RETL)                         
001300*        DIVERSEKOLLI   BEHANDLAS SEPARAT.                                
001400*        SCANNADE KOLLI BEHANDLAS SEPARAT.                                
001500*                                                                         
001600*                                                                         
001700*        PROGRAMMET LÄSER OCH UPPDATERAR W6INLA (W6D1)                    
001800*        PROGRAMMET LÄSER                W6PLAA (W6G1) W6GX6006           
001900*        PROGRAMMET LÄSER                R6LOGA (W6G1) W6GX6018           
002000*        PROGRAMMET LÄSER                W6UPFA (W6L1)                    
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T141 W6T141                                       
002400*        MID:         W6I14101                                            
002500*        MID:         W6I14102 FRÅN VCOM                                  
002600*        MID:         W6I14103 FRÅN HANDDATOR                             
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W6O14101                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W6014100'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600*    --- INDEX FÖR RADER OCH KONTROLLER                                   
004700                                                                          
004800 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  RAD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  RAD-MAX-IX                  PIC S9(4)  VALUE +12   COMP SYNC.        
005100 77  6199-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  6199-MAX-IX                 PIC S9(4)  VALUE +12   COMP SYNC.        
005300 77  619A-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  619A-MAX-IX                 PIC S9(4)  VALUE +12   COMP SYNC.        
005500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +612  COMP SYNC.        
005700 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005800                                                                          
005900*    --- TABELL-INDEX                                                     
006000                                                                          
006100 77  D-TAB-IX                    PIC S9(4)  VALUE +0    COMP SYNC.        
006200 77  D-TAB-IX-MAX                PIC S9(4)  VALUE +0    COMP SYNC.        
006300 77  D-TAB-MAX-IX                PIC S9(4)  VALUE +200  COMP SYNC.        
006400                                                                          
006500 77  I-TAB-IX                    PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  I-TAB-IX-MAX                PIC S9(4)  VALUE +0    COMP SYNC.        
006700 77  I-TAB-MAX-IX                PIC S9(4)  VALUE +700  COMP SYNC.        
006800                                                                          
006900 77  P-TAB-IX                    PIC S9(4)  VALUE +0    COMP SYNC.        
007000 77  P-TAB-IX-MAX                PIC S9(4)  VALUE +0    COMP SYNC.        
007100 77  P-TAB-MAX-IX                PIC S9(4)  VALUE +96   COMP SYNC.        
007200                                                                          
007300 77  R-TAB-IX                    PIC S9(4)  VALUE +0    COMP SYNC.        
007400 77  R-TAB-IX-MAX                PIC S9(4)  VALUE +0    COMP SYNC.        
007500 77  R-TAB-MAX-IX                PIC S9(4)  VALUE +150  COMP SYNC.        
007600     EJECT                                                                
007700*    --- SWITCHAR                                                         
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '6141'.                
008100     88  GODK-MID                            VALUE '6141' '6142'          
008200                                                   '6143' '6144'          
008300                                                   '6145'.                
008400     88  HELP-MID                            VALUE '0551'.                
008500     88  VCOM-MID                            VALUE '0694'.                
008600                                                                          
008700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008800     88  INDATA-OK                           VALUE 'J'.                   
008900     88  INDATA-FEL                          VALUE 'N'.                   
009000                                                                          
009100 77  RAD-SW                      PIC X       VALUE 'J'.                   
009200     88  RAD-OK                              VALUE 'J'.                   
009300     88  RAD-FEL                             VALUE 'N'.                   
009400                                                                          
009500 77  TAB-TRAEFF-SW               PIC X       VALUE 'N'.                   
009600     88  TAB-TRAEFF                          VALUE 'J'.                   
009700     88  EJ-TAB-TRAEFF                       VALUE 'N'.                   
009800                                                                          
009900 77  TORG-SW                     PIC X       VALUE 'N'.                   
010000     88  SAMMA-TORG                          VALUE 'J'.                   
010100     88  NYTT-TORG                           VALUE 'N'.                   
010200                                                                          
010300 77  PRINTER-SW                  PIC X       VALUE 'N'.                   
010400     88  PRINTER-VALD                        VALUE 'J'.                   
010500     88  PRINTER-EJ-VALD                     VALUE 'N'.                   
010600                                                                          
010700 77  KOLLIRAD-SW                 PIC X       VALUE 'N'.                   
010800     88  KOLLIRAD-FINNS                      VALUE 'J'.                   
010900     88  INGEN-KOLLIRAD-FINNS                VALUE 'N'.                   
011000                                                                          
011100 77  UTSKRIFT-SW                 PIC X       VALUE 'N'.                   
011200     88  RAD-FINNS-ATT-SKRIVA-UT             VALUE 'J'.                   
011300     88  ALLA-RADER-UTSKRIVNA                VALUE 'N'.                   
011400                                                                          
011500 77  REDAN-PRINTAD-SW            PIC X       VALUE 'N'.                   
011600     88  REDAN-PRINTAD                       VALUE 'J'.                   
011700                                                                          
011800 77  SW-1A-6199                  PIC X       VALUE 'J'.                   
011900 77  SW-1A-619A                  PIC X       VALUE 'J'.                   
012000                                                                          
012100 77  DIVKOLLI-SW                 PIC X       VALUE 'N'.                   
012200     88  DIVKOLLI-FINNS                      VALUE 'J'.                   
012300     88  DIVKOLLI-FINNS-EJ                   VALUE 'N'.                   
012400     EJECT                                                                
012500                                                                          
012600 77  NYCKEL-VAERDE               PIC X       VALUE 'N'.                   
012700     88  NYCKLAR-SAKNAS                      VALUE 'N'.                   
012800     88  NYCKLAR-FINNS                       VALUE 'J'.                   
012900     88  NYCKEL-VAGN                         VALUE 'V'.                   
013000     88  NYCKEL-PLAC                         VALUE 'P'.                   
013100     88  NYCKEL-KOLLI                        VALUE 'K'.                   
013200                                                                          
013300 77  WS-FL-PRIM-SEK              PIC X       VALUE 'N'.                   
013400                                                                          
013500*      --- VALID IDDC CODES                                               
013600*                                                                         
013700*01    -COPY WWDC99                                                       
013700*01    -COPY WWDCKONS                                                     
013800       EJECT                                                              
013900*--- PARAMETRAR TILL ABEND                                                
014000                                                                          
014100 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4) VALUE +33 COMP SYNC.           
014200 01  RKOD-ABEND-MED-DUMP         PIC S9(4) VALUE +1234 COMP SYNC.         
014300                                                                          
014400     EJECT                                                                
014500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014600 01  GENERELLA-SUBPROGRAM.                                                
014700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015100     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
015200     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
015300     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
015400     03  W611RETL                PIC X(8)    VALUE 'W611RETL'.            
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015700*01 -COPY WMEDAREA                                                        
015800     SKIP3                                                                
015900 01  MESSAGE-CODES.                                                       
016000     03  ERR-NOT-ON-REGISTER     PIC X(3)    VALUE '010'.                 
016100***      NYTT MEDDELANDE BEHÖVS ISTF NR 11                                
016200     03  ERR-NO-LINE             PIC X(3)    VALUE '231'.                 
016300     03  ERR-NO-PRINTING         PIC X(3)    VALUE '067'.                 
016400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '409'.                 
016600     03  INF-PF4-FOR-PRINTING    PIC X(3)    VALUE '081'.                 
016700     03  INF-PRINTING-STARTED    PIC X(3)    VALUE '202'.                 
016800     03  INF-ALREADY-PRINTED     PIC X(3)    VALUE '205'.                 
016900     03  ERR-CONTROL-NOT-COMPL   PIC X(3)    VALUE '215'.                 
017000     EJECT                                                                
017100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017200*                                                                         
017300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017400     SKIP3                                                                
017500*01  -COPY W6I14101                                                       
017600     EJECT                                                                
017700*01  -COPY W6I14102 -PRE VCOM-                                            
017800     EJECT                                                                
017900*01  -COPY W6I14103                                                       
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018200     SKIP3                                                                
018300*   -COPY WMSGMAIL                                                        
018400     EJECT                                                                
018500*01  -COPY WMSGAREA                                                       
018600     EJECT                                                                
018700     03  MOD REDEFINES MSG-AREA.                                          
018800*      05  -COPY W6O14101                                                 
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019100     SKIP3                                                                
019200*01  -COPY WMFSAREA                                                       
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'W006PRT-AREA'.        
019500     SKIP3                                                                
019600*01  -COPY W006PRT                                                        
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)  VALUE 'W611STYR-AREA'.        
019900     SKIP3                                                                
020000*01  -COPY W611STYR                                                       
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)  VALUE 'W611RETL-AREA'.        
020300     SKIP3                                                                
020400*01  -COPY W611RETL                                                       
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
020700     SKIP3                                                                
020800*01  -COPY WMSGSNUF            -PRE P-TO-P-                               
020900     EJECT                                                                
021000 01      FILLER                  PIC X(24)   VALUE                        
021100                                 'MOD6199-MID-W6I19901'.                  
021200     SKIP2                                                                
021300     -COPY W6I19901 -PRE MOD6199-                                         
021400     EJECT                                                                
021500 01      FILLER                  PIC X(24)   VALUE                        
021600                                 'MOD619A-MID-W6I19A01'.                  
021700     SKIP2                                                                
021800     -COPY W6I19A01 -PRE MOD619A-                                         
021900     EJECT                                                                
022000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022100*                                                                         
022200     SKIP2                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022400     SKIP2                                                                
022500 01  NYCKLAR-TILL-DLI.                                                    
022600     SKIP2                                                                
022700*--------W6D1  TILL W6INLA21                                              
022800     03  W-W6D101KY-X.                                                    
022900         05  W-W6D101-IDDC       PIC  X(2)   VALUE '11'.                  
023000         05  W-W6D101-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
023100         05  W-W6D101-IDFS       PIC  X(8)   VALUE SPACE.                 
023200         05  W-W6D101-TIAVIDAT   PIC S9(7)   VALUE ZERO COMP-3.           
023300*                                                                         
023400     03  W-W6D111-IDRADNR-INL-X.                                          
023500         05  W-W6D111-IDRADNR-INL PIC S9(5)  VALUE ZERO COMP-3.           
023600*                                                                         
023700     03  W-W6D121-IDRADNR-X.                                              
023800         05  W-W6D121-IDRADNR    PIC S9(5)   VALUE ZERO COMP-3.           
023900*                                                                         
024000*--------FYSISK NYCKEL TILL INLG                                          
024100     03  W-W6D1F1KY-MIN-X.                                                
024200         05  W-F-MIN-IDINLVGN    PIC 9(3).                                
024300         05  W-F-MIN-IDRADNR-INL PIC S9(5)    VALUE ZERO COMP-3.          
024400         05  W-F-MIN-IDDC        PIC  X(2)    VALUE '11'.                 
024500         05  W-F-MIN-IDLEVNR     PIC  X(5)    VALUE SPACE.                
024600         05  W-F-MIN-IDFS        PIC X(8)     VALUE SPACE.                
024700         05  W-F-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
024800         05  W-F-MIN-IDRADNR     PIC S9(5)    VALUE ZERO COMP-3.          
024900                                                                          
025000     03  W-W6D1F1KY-MAX-X.                                                
025100         05  W-F-MAX-IDINLVGN    PIC 9(3).                                
025200         05  W-F-MAX-IDRADNR-INL PIC S9(5)    VALUE ZERO COMP-3.          
025300         05  W-F-MAX-IDDC        PIC  X(2)    VALUE '11'.                 
025400         05  W-F-MAX-IDLEVNR     PIC  X(5)    VALUE SPACE.                
025500         05  W-F-MAX-IDFS        PIC X(8)     VALUE SPACE.                
025600         05  W-F-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
025700         05  W-F-MAX-IDRADNR     PIC S9(5)    VALUE ZERO COMP-3.          
025800     EJECT                                                                
025900*--------FYSISK NYCKEL TILL INLF                                          
026000     03  W-W6D1E1KY-MIN-X.                                                
026100         05  W-E-MIN-ADINLOMR    PIC X(4)     VALUE SPACE.                
026200         05  W-E-MIN-KDINLPRIO   PIC S9(3)    VALUE ZERO COMP-3.          
026300         05  W-E-MIN-IDRADNR-INL PIC S9(5)    VALUE ZERO COMP-3.          
026400         05  W-E-MIN-IDDC        PIC  X(2)    VALUE '11'.                 
026500         05  W-E-MIN-IDLEVNR     PIC  X(5)    VALUE SPACE.                
026600         05  W-E-MIN-IDFS        PIC X(8)     VALUE SPACE.                
026700         05  W-E-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
026800         05  W-E-MIN-IDRADNR     PIC S9(5)    VALUE ZERO COMP-3.          
026900                                                                          
027000     03  W-W6D1E1KY-MAX-X.                                                
027100         05  W-E-MAX-ADINLOMR    PIC X(4)     VALUE SPACE.                
027200         05  W-E-MAX-KDINLPRIO   PIC S9(3)    VALUE ZERO COMP-3.          
027300         05  W-E-MAX-IDRADNR-INL PIC S9(5)    VALUE ZERO COMP-3.          
027400         05  W-E-MAX-IDDC        PIC  X(2)    VALUE '11'.                 
027500         05  W-E-MAX-IDLEVNR     PIC  X(5)    VALUE SPACE.                
027600         05  W-E-MAX-IDFS        PIC X(8)     VALUE SPACE.                
027700         05  W-E-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
027800         05  W-E-MAX-IDRADNR     PIC S9(5)    VALUE ZERO COMP-3.          
027900*    EJECT                                                                
028000*--------FYSISK NYCKEL TILL INLD                                          
028100     03  W-W6D1C1KY-MIN-X.                                                
028200         05  W-C-MIN-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                
028300         05  W-C-MIN-IDOKOLLI    PIC  9(9)    VALUE ZERO.                 
028400         05  W-C-MIN-IDRADNR-INL PIC S9(5)    VALUE ZERO COMP-3.          
028500         05  W-C-MIN-IDDC        PIC  X(2)    VALUE '11'.                 
028600         05  W-C-MIN-IDLEVNR     PIC  X(5)    VALUE SPACE.                
028700         05  W-C-MIN-IDFS        PIC X(8)     VALUE SPACE.                
028800         05  W-C-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
028900         05  W-C-MIN-IDRADNR     PIC S9(5)    VALUE ZERO COMP-3.          
029000                                                                          
029100     03  W-W6D1C1KY-MAX-X.                                                
029200         05  W-C-MAX-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                
029300         05  W-C-MAX-IDOKOLLI    PIC  9(9)    VALUE ZERO.                 
029400         05  W-C-MAX-IDRADNR-INL PIC S9(5)    VALUE ZERO COMP-3.          
029500         05  W-C-MAX-IDDC        PIC  X(2)    VALUE '11'.                 
029600         05  W-C-MAX-IDLEVNR     PIC  X(5)    VALUE SPACE.                
029700         05  W-C-MAX-IDFS        PIC X(8)     VALUE SPACE.                
029800         05  W-C-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
029900         05  W-C-MAX-IDRADNR     PIC S9(5)    VALUE ZERO COMP-3.          
030000     EJECT                                                                
030100*--------W6D1D TILL W6INLA11 SEK.INGÅNG                                   
030200     03  W-W6D1DSEQ-MIN-X.                                                
030300         05  W-D-MIN-IDILIST  PIC  9(5).                                  
030400         05  W-D-MIN-IDILIRAD PIC S9(5) COMP-3.                           
030500*                                                                         
030600     03  W-W6D1DSEQ-MAX-X.                                                
030700         05  W-D-MAX-IDILIST  PIC  9(5).                                  
030800         05  W-D-MAX-IDILIRAD PIC S9(5) COMP-3.                           
030900*                                                                         
031000*--------W6G1  TILL W6PLAA11                                              
031100     03  W-W6GXKEY-6005-X.                                                
031200         05  W-IDHTYP-6005       PIC X(4)     VALUE '6005'.               
031300         05  W-IDDC-6005         PIC X(2)     VALUE '11'.                 
031400         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
031500     03  W-W6GXKEY-6006-X.                                                
031600         05  W-6006-ADINLOMR     PIC X(4)     VALUE SPACE.                
031700         05  FILLER              PIC X(1)     VALUE LOW-VALUE.            
031800*                                                                         
031900     03  W-6006-ADINLOMR-PAR-X.                                           
032000         05  W-6006-ADINLOMR-PAR PIC X(4)    VALUE SPACE.                 
032100*                                                                         
032200*--------W6G1  TILL W6LOPA11                                              
032300     03  W-W6GXKEY-6017-X.                                                
032400         05  W-IDHTYP-6017       PIC X(4)     VALUE '6017'.               
032500         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
032600     03  W-W6GXKEY-6018-X.                                                
032700         05  W-6018-KDSEGKEY     PIC X        VALUE '1'.                  
032800*                                                                         
032900*--------W6L101                                                           
033000     03  W-IDLOPNRM-X.                                                    
033100         05  W-IDLOPNRM          PIC S9(9) COMP-3 VALUE ZERO.             
033200*                                                                         
033300 EJECT                                                                    
033400*    --- ARBETSFÄLT OCH -TABELLER                                         
033500                                                                          
033600 01  DAGENS-DATUM                PIC 9(6)     VALUE ZEROS.                
033700 01  W-ADINLOMR-PRT              PIC X(4)     VALUE SPACE.                
033800 01  W-BEPRTLST                  PIC X(25)    VALUE SPACE.                
033900 01  W-IDLEVNR                   PIC X(5)     VALUE SPACE.                
034000 01  W-IDFS                      PIC X(8)     VALUE SPACE.                
034100 01  W-TIAVIDAT                  PIC 9(6)     VALUE ZEROS.                
034200 01  W-SPAR-TORG                 PIC X(4)     VALUE SPACE.                
034300 01  W-SPAR-ADLAGOMR             PIC 9(2)     VALUE ZEROS.                
034400 01  W-JFR-ADLAGOMR              PIC S9(3)    VALUE ZEROS COMP-3.         
034500 01  W-SPAR-IDARTNR              PIC 9(9)     VALUE ZEROS COMP-3.         
034600 01  W-IDILIST                   PIC 9(5)     VALUE ZEROS.                
034700 01  W-KVRADER                   PIC 9(5)     VALUE ZEROS.                
034800 01  W-KVPOST-I-LISTA            PIC 9(7)     VALUE ZEROS.                
034900 01  W-KVPOST-P-LISTA            PIC 9(7)     VALUE ZEROS.                
035000*                                                                         
035100 01  TABENTRY-PARM.                                                       
035200     03  STEGLNGD                PIC S9(9)    COMP.                       
035300     03  ANTAL                   PIC S9(9)    COMP.                       
035400     03  NYCKELLNGD              PIC S9(9)    COMP.                       
035500 SKIP2                                                                    
035600 01  FILLER      PIC X(16) VALUE 'RAD-TAB  '.                             
035700 01  RAD-TAB.                                                             
035800     03  RAD-POST  OCCURS 12.                                             
035900         05  W-IDLEVNR-KOLLI      PIC  X(5)   VALUE SPACE.                
036000         05  W-IDOKOLLI           PIC S9(9)   VALUE ZERO.                 
036100         05  W-TEMFSMED           PIC X(20)   VALUE SPACE.                
036200 SKIP2                                                                    
036300 01  TAB-FRAAN-BILD.                                                      
036400     03  TAB-HUVUD.                                                       
036500         05  TAB-IDLEVNR-KOLLI    PIC  X(5)  VALUE SPACE.                 
036600         05  TAB-IDOKOLLI         PIC  9(9)   VALUE ZERO.                 
036700         05  TAB-IDINLVGN         PIC  9(3)   VALUE ZERO.                 
036800         05  TAB-ADINLOMR         PIC  X(4)   VALUE SPACE.                
036900         05  TAB-IDPRTLST         PIC  X(8)   VALUE SPACE.                
037000*                                                                         
037100 EJECT                                                                    
037200*----TABELL FÖR BEHANDLING AV DIVERSEKOLLIN                               
037300 01  FILLER      PIC X(16) VALUE 'DIV-TAB  '.                             
037400 01  DIV-TAB.                                                             
037500     03  DIV-POST  OCCURS 200.                                            
037600         05  D-TAB-SORTNYCKEL.                                            
037700           07  D-TAB-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                
037800           07  D-TAB-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
037900         05  D-TAB-ADLAGOMR       PIC S9(3)   VALUE ZERO COMP-3.          
038000         05  D-TAB-TORG           PIC  X(4)   VALUE '0000'.               
038100         05  D-TAB-ADGANG         PIC S9(3)   VALUE ZERO COMP-3.          
038200         05  D-TAB-ADPLATS        PIC S9(5)   VALUE ZERO COMP-3.          
038300         05  D-TAB-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.          
038400         05  D-TAB-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
038500         05  D-TAB-IDLEVNR        PIC  X(5)   VALUE SPACE.                
038600         05  D-TAB-IDFS           PIC  X(8)   VALUE SPACE.                
038700         05  D-TAB-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
038800         05  D-TAB-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
038900         05  D-TAB-RETL           PIC  X(1)   VALUE SPACE.                
039000         05  D-TAB-IDFKNGRP       PIC S9(5)   VALUE ZERO COMP-3.          
039100*                                                                         
039200 SKIP2                                                                    
039300*----TOMTABELL FÖR DIVERSEKOLLITABELL                                     
039400 01  FILLER      PIC X(16) VALUE 'DIV-TAB-TOM'.                           
039500 01  DIV-TAB-TOM.                                                         
039600     03  DIV-TOM-POST  OCCURS 200.                                        
039700         05  D-TOM-SORTNYCKEL.                                            
039800           07  D-TOM-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                
039900           07  D-TOM-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
040000         05  D-TOM-ADLAGOMR       PIC S9(3)   VALUE ZERO COMP-3.          
040100         05  D-TOM-TORG           PIC  X(4)   VALUE '0000'.               
040200         05  D-TOM-ADGANG         PIC S9(3)   VALUE ZERO COMP-3.          
040300         05  D-TOM-ADPLATS        PIC S9(5)   VALUE ZERO COMP-3.          
040400         05  D-TOM-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.          
040500         05  D-TOM-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
040600         05  D-TOM-IDLEVNR        PIC  X(5)   VALUE SPACE.                
040700         05  D-TOM-IDFS           PIC  X(8)   VALUE SPACE.                
040800         05  D-TOM-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
040900         05  D-TOM-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
041000         05  D-TOM-RETL           PIC  X(1)   VALUE SPACE.                
041100         05  D-TOM-IDFKNGRP       PIC S9(5)   VALUE ZERO COMP-3.          
041200*                                                                         
041300 EJECT                                                                    
041400*----TABELL FÖR SKAPANDE AV INLÄGGNINGSLISTA                              
041500 01  FILLER      PIC X(16) VALUE 'INL-TAB    '.                           
041600 01  INL-TAB.                                                             
041700     03  INL-POST  OCCURS 700.                                            
041800         05  I-TAB-SORTNYCKEL.                                            
041900           07  I-TAB-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.          
042000           07  I-TAB-TORG         PIC  X(4)   VALUE '0000'.               
042100           07  I-TAB-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.          
042200           07  I-TAB-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.          
042300           07  I-TAB-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
042400           07  I-TAB-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                
042500           07  I-TAB-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
042600         05  I-TAB-IDLEVNR        PIC  X(5)   VALUE SPACE.                
042700         05  I-TAB-IDFS           PIC  X(8)   VALUE SPACE.                
042800         05  I-TAB-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
042900         05  I-TAB-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
043000         05  I-TAB-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
043100         05  I-TAB-IDILIST        PIC  9(5)   VALUE ZERO.                 
043200         05  I-TAB-IDILIRAD       PIC S9(5)   VALUE ZERO COMP-3.          
043300*                                                                         
043400 SKIP2                                                                    
043500*----TOMTABELL FÖR INLÄGGNINGSLISTETABELL                                 
043600 01  FILLER      PIC X(16) VALUE 'INL-TAB-TOM'.                           
043700 01  INL-TAB-TOM.                                                         
043800     03  INL-TOM-POST  OCCURS 700.                                        
043900         05  I-TOM-SORTNYCKEL.                                            
044000           07  I-TOM-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.          
044100           07  I-TOM-TORG         PIC  X(4)   VALUE '0000'.               
044200           07  I-TOM-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.          
044300           07  I-TOM-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.          
044400           07  I-TOM-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
044500           07  I-TOM-IDLEVNR-KOLLI PIC  X(5)  VALUE SPACE.                
044600           07  I-TOM-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
044700         05  I-TOM-IDLEVNR        PIC  X(5)   VALUE SPACE.                
044800         05  I-TOM-IDFS           PIC  X(8)   VALUE SPACE.                
044900         05  I-TOM-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
045000         05  I-TOM-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
045100         05  I-TOM-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
045200         05  I-TOM-IDILIST        PIC  9(5)   VALUE ZERO.                 
045300         05  I-TOM-IDILIRAD       PIC S9(5)   VALUE ZERO COMP-3.          
045400*                                                                         
045500 EJECT                                                                    
045600*----TABELL FÖR SKAPANDE AV PLATSSÄTTARLISTA                              
045700 01  FILLER      PIC X(16) VALUE 'PLA-TAB    '.                           
045800 01  PLA-TAB.                                                             
045900     03  PLA-POST  OCCURS 96.                                             
046000         05  P-TAB-SORTNYCKEL.                                            
046100            07  P-TAB-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.          
046200            07  P-TAB-IDLEVNR-KOLLI PIC  X(5) VALUE SPACE.                
046300            07  P-TAB-IDOKOLLI      PIC  9(9) VALUE ZERO.                 
046400         05  P-TAB-IDLEVNR          PIC  X(5) VALUE SPACE.                
046500         05  P-TAB-IDFS             PIC  X(8) VALUE SPACE.                
046600         05  P-TAB-TIAVIDAT         PIC S9(7) VALUE ZERO COMP-3.          
046700         05  P-TAB-IDRADNR-INL      PIC S9(5) VALUE ZERO COMP-3.          
046800         05  P-TAB-IDRADNR          PIC S9(5) VALUE ZERO COMP-3.          
046900         05  P-TAB-IDILIST          PIC  9(5) VALUE ZERO.                 
047000         05  P-TAB-IDILIRAD         PIC S9(5) VALUE ZERO COMP-3.          
047100*                                                                         
047200 SKIP2                                                                    
047300*----TOMTABELL FÖR PLATSSÄTTARLISTETABELL                                 
047400 01  FILLER      PIC X(16) VALUE 'PLA-TAB-TOM'.                           
047500 01  PLA-TAB-TOM.                                                         
047600     03  PLA-TOM-POST  OCCURS 96.                                         
047700         05  P-TOM-SORTNYCKEL.                                            
047800            07  P-TOM-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.          
047900            07  P-TOM-IDLEVNR-KOLLI PIC  X(5) VALUE SPACE.                
048000            07  P-TOM-IDOKOLLI      PIC  9(9) VALUE ZERO.                 
048100         05  P-TOM-IDLEVNR          PIC  X(5) VALUE SPACE.                
048200         05  P-TOM-IDFS             PIC  X(8) VALUE SPACE.                
048300         05  P-TOM-TIAVIDAT         PIC S9(7) VALUE ZERO COMP-3.          
048400         05  P-TOM-IDRADNR-INL      PIC S9(5) VALUE ZERO COMP-3.          
048500         05  P-TOM-IDRADNR          PIC S9(5) VALUE ZERO COMP-3.          
048600         05  P-TOM-IDILIST          PIC  9(5) VALUE ZERO.                 
048700         05  P-TOM-IDILIRAD         PIC S9(5) VALUE ZERO COMP-3.          
048800*                                                                         
048900 EJECT                                                                    
049000*----TABELL FÖR SKAPANDE AV RETURLISTA                                    
049100 01  FILLER      PIC X(16) VALUE 'RET-TAB    '.                           
049200 01  RET-TAB.                                                             
049300     03  RET-POST  OCCURS 150.                                            
049400         05  R-TAB-SORTNYCKEL.                                            
049500            07  R-TAB-ADINLOMR-FB   PIC  X(4) VALUE SPACE.                
049600            07  R-TAB-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.          
049700            07  R-TAB-IDLEVNR-KOLLI PIC  X(5) VALUE SPACE.                
049800            07  R-TAB-IDOKOLLI      PIC  9(9) VALUE ZERO.                 
049900         05  R-TAB-IDLEVNR          PIC  X(5) VALUE SPACE.                
050000         05  R-TAB-IDFS             PIC  X(8) VALUE SPACE.                
050100         05  R-TAB-TIAVIDAT         PIC S9(7) VALUE ZERO COMP-3.          
050200         05  R-TAB-IDRADNR-INL      PIC S9(5) VALUE ZERO COMP-3.          
050300         05  R-TAB-IDRADNR          PIC S9(5) VALUE ZERO COMP-3.          
050400         05  R-TAB-IDFKNGRP         PIC S9(5) VALUE ZERO COMP-3.          
050500*                                                                         
050600 SKIP2                                                                    
050700*----TOMTABELL FÖR RETURLISTETABELL                                       
050800 01  FILLER      PIC X(16) VALUE 'RET-TAB-TOM'.                           
050900 01  RET-TAB-TOM.                                                         
051000     03  RET-TOM-POST  OCCURS 150.                                        
051100         05  R-TOM-SORTNYCKEL.                                            
051200            07  R-TOM-ADINLOMR-FB   PIC  X(4) VALUE SPACE.                
051300            07  R-TOM-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.          
051400            07  R-TOM-IDLEVNR-KOLLI PIC  X(5) VALUE SPACE.                
051500            07  R-TOM-IDOKOLLI      PIC  9(9) VALUE ZERO.                 
051600         05  R-TOM-IDLEVNR          PIC  X(5) VALUE SPACE.                
051700         05  R-TOM-IDFS             PIC  X(8) VALUE SPACE.                
051800         05  R-TOM-TIAVIDAT         PIC S9(7) VALUE ZERO COMP-3.          
051900         05  R-TOM-IDRADNR-INL      PIC S9(5) VALUE ZERO COMP-3.          
052000         05  R-TOM-IDRADNR          PIC S9(5) VALUE ZERO COMP-3.          
052100         05  R-TOM-IDFKNGRP         PIC S9(5) VALUE ZERO COMP-3.          
052200*                                                                         
052300 EJECT                                                                    
052400*    --- STATUS-KOD FRÅN IMS                                              
052500 01  STATUS-WS                   PIC XX.                                  
052600     88  SEGMENT-FINNS                       VALUE '  '.                  
052700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
052800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
052900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
053000     SKIP2                                                                
053100 01  GODK-STATUSKODER.                                                    
053200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
053300     SKIP3                                                                
053400 01  SSA1                        PIC X(128).                              
053500 01  SSA2                        PIC X(64).                               
053600 01  SSA3                        PIC X(64).                               
053700     EJECT                                                                
053800*    --- IMS FUNKTIONSKODER                                               
053900*01  -COPY W0003                                                          
054000     EJECT                                                                
054100*    ---  DLI INPUT-OUTPUT AREA                                           
054200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
054300     SKIP3                                                                
054400 01  DLI-IO-AREA1.                                                        
054500     03  IO-AREA1                PIC X(200)  VALUE SPACE.                 
054600     SKIP3                                                                
054700     03  W6INLA01 REDEFINES IO-AREA1.                                     
054800*        05  -COPY W6D101                                                 
054900     EJECT                                                                
055000     03  W6INLA11 REDEFINES IO-AREA1.                                     
055100*        05  -COPY W6D111                                                 
055200     EJECT                                                                
055300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
055400     SKIP3                                                                
055500 01  DLI-IO-AREA2.                                                        
055600     03  IO-AREA2                PIC X(250)  VALUE SPACE.                 
055700     SKIP3                                                                
055800     03  W6INLA21 REDEFINES IO-AREA2.                                     
055900*        05  -COPY W6D121                                                 
056000     EJECT                                                                
056100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
056200     SKIP3                                                                
056300 01  DLI-IO-AREA3.                                                        
056400     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
056500     SKIP3                                                                
056600     03  W6PLAA11 REDEFINES IO-AREA3.                                     
056700*        05  -COPY W6GX6006                                               
056800     EJECT                                                                
056900     03  W6LOPA11 REDEFINES IO-AREA3.                                     
057000*        05  -COPY W6GX6018                                               
057100     EJECT                                                                
057200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
057300     SKIP3                                                                
057400 01  DLI-IO-AREA4.                                                        
057500     03  IO-AREA4                PIC X(50)   VALUE SPACE.                 
057600     SKIP3                                                                
057700     03  W6INLD01 REDEFINES IO-AREA4.                                     
057800*        05  -COPY W6D1C1                                                 
057900     EJECT                                                                
058000     03  W6INLF01 REDEFINES IO-AREA4.                                     
058100*        05  -COPY W6D1E1                                                 
058200     EJECT                                                                
058300     03  W6INLG01 REDEFINES IO-AREA4.                                     
058400*        05  -COPY W6D1F1                                                 
058500     EJECT                                                                
058600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-UPFA'.         
058700     SKIP3                                                                
058800 01  DLI-IO-UPFA.                                                         
058900     03  IO-UPFA                PIC X(100)  VALUE SPACE.                  
059000     03  W6UPFA01 REDEFINES IO-UPFA.                                      
059100*        05  -COPY W6L101                                                 
059200     EJECT                                                                
059300 01  DLI-IO-AREA-UPFA11.                                                  
059400     03  W6UPFA11.                                                        
059500*        05  -COPY W6L111                                                 
059600     SKIP3                                                                
059700 01  DLI-IO-AREA-UPFA12.                                                  
059800     03  W6UPFA12.                                                        
059900*        05  -COPY W6L112                                                 
060000     EJECT                                                                
060100 LINKAGE SECTION.                                                         
060200                                                                          
060300*01  -COPY W0009   -PRE MSG-                                              
060400     EJECT                                                                
060500*01  -COPY W0009   -PRE ALTMAIL-                                          
060600     EJECT                                                                
060700*01  -COPY W0009   -PRE ALT-6199-                                         
060800     EJECT                                                                
060900*01  -COPY W0009   -PRE ALT-619A-                                         
061000     EJECT                                                                
061100 01  RETL-6191-PCB               PIC X.                                   
061200     SKIP3                                                                
061300 01  RETL-PRT-PCB                PIC X.                                   
061400     SKIP3                                                                
061500 01  RETL-INLA-PCB               PIC X.                                   
061600     EJECT                                                                
061700*01  -COPY W0008  -PRE STYR-HANA-                                         
061800     05  FILLER                  PIC X.                                   
061900     EJECT                                                                
062000*01  -COPY W0008  -PRE STYR-PLAA-                                         
062100     05  FILLER                  PIC X.                                   
062200     EJECT                                                                
062300*01  -COPY W0008  -PRE INLA1-                                             
062400     05  FILLER                  PIC X.                                   
062500     EJECT                                                                
062600*01  -COPY W0008  -PRE INLG-                                              
062700     05  FILLER                  PIC X.                                   
062800     EJECT                                                                
062900*01  -COPY W0008  -PRE INLF-                                              
063000     05  FILLER                  PIC X.                                   
063100     EJECT                                                                
063200*01  -COPY W0008  -PRE INLD-                                              
063300     05  FILLER                  PIC X.                                   
063400     EJECT                                                                
063500*01  -COPY W0008  -PRE INLA-D-                                            
063600     05  FILLER                  PIC X.                                   
063700     EJECT                                                                
063800*01  -COPY W0008  -PRE PLAA-                                              
063900     05  FILLER                  PIC X.                                   
064000     EJECT                                                                
064100*01  -COPY W0008  -PRE LOPA-                                              
064200     05  FILLER                  PIC X.                                   
064300     EJECT                                                                
064400*01  -COPY W0008  -PRE UPFA-                                              
064500     05  FILLER                  PIC X.                                   
064600     EJECT                                                                
064700 PROCEDURE DIVISION  USING MSG-PCB ALTMAIL-PCB ALT-6199-PCB               
064800                          ALT-619A-PCB                                    
064900                          RETL-6191-PCB RETL-PRT-PCB RETL-INLA-PCB        
065000                          STYR-HANA-PCB                                   
065100                          STYR-PLAA-PCB                                   
065200                          INLA1-PCB INLG-PCB INLF-PCB                     
065300                          INLD-PCB INLA-D-PCB                             
065400                          PLAA-PCB LOPA-PCB UPFA-PCB.                     
065500     ENTRY 'DLITCBL' USING MSG-PCB ALTMAIL-PCB ALT-6199-PCB               
065600                          ALT-619A-PCB                                    
065700                          RETL-6191-PCB RETL-PRT-PCB RETL-INLA-PCB        
065800                          STYR-HANA-PCB                                   
065900                          STYR-PLAA-PCB                                   
066000                          INLA1-PCB INLG-PCB INLF-PCB                     
066100                          INLD-PCB INLA-D-PCB                             
066200                          PLAA-PCB LOPA-PCB UPFA-PCB.                     
066300                                                                          
066400     PERFORM IMS-GET-MSG                                                  
066500     IF SEGMENT-FINNS                                                     
066600       PERFORM A-INIT                                                     
066700       IF MFS-PRINT OR MFS-UPD-V  OR MFS-UPD-X                            
066800         IF MFS-UPD-V                                                     
066900           PERFORM S24-FLYTTA-MID-TILL-MOD                                
067000         END-IF                                                           
067100         PERFORM B-FORMELL-KONTROLL                                       
067200         IF INDATA-OK                                                     
067300           PERFORM F-KONTROLLERA-BEHANDLA-INDATA                          
067400                                                                          
067500* * * FÖR VAGN ELLER PLACERING * * *                                      
067600                                                                          
067700           IF I-TAB-IX-MAX > ZERO                                         
067800             PERFORM S08-INIT-I-LISTA                                     
067900                                                                          
068000             PERFORM G-LAEGG-UPP-I-LISTA                                  
068100                                                                          
068200             IF 6199-IX > ZERO                                            
068300               PERFORM S21-P-TO-P-6199                                    
068400             END-IF                                                       
068500           END-IF                                                         
068600           IF P-TAB-IX-MAX > ZERO                                         
068700             PERFORM S09-INIT-P-LISTA                                     
068800                                                                          
068900             PERFORM H-LAEGG-UPP-P-LISTA                                  
069000                                                                          
069100             IF 619A-IX > ZERO                                            
069200               PERFORM S23-P-TO-P-619A                                    
069300             END-IF                                                       
069400           END-IF                                                         
069500           IF R-TAB-IX-MAX > ZERO                                         
069600                                                                          
069700             PERFORM I-LAEGG-UPP-R-LISTA                                  
069800             PERFORM MFS-RENSA-FAELT-IN                                   
069900                                                                          
070000           END-IF                                                         
070100                                                                          
070200* * * FÖR DIVERSEKOLLI UNDER VAGN ELLER PLACERING * * *                   
070300                                                                          
070400           IF DIVKOLLI-FINNS                                              
070500             IF SW-1A-6199 = JA                                           
070600               PERFORM S08-INIT-I-LISTA                                   
070700             END-IF                                                       
070800             IF SW-1A-619A = JA                                           
070900               PERFORM S09-INIT-P-LISTA                                   
071000             END-IF                                                       
071100             PERFORM J-SORT-D-TAB                                         
071200             MOVE D-TAB-IX  TO  D-TAB-IX-MAX                              
071300             MOVE +1 TO D-TAB-IX                                          
071400             PERFORM S06-RENSA-TAB-O-INDEX                                
071500             PERFORM UNTIL D-TAB-IX > D-TAB-IX-MAX                        
071600                                                                          
071700               PERFORM K-BEHANDLA-DIVKOLLI                                
071800                                                                          
071900               PERFORM S07-SAETT-MAX-INDEX                                
072000               IF I-TAB-IX-MAX > ZERO                                     
072100                                                                          
072200                 PERFORM G-LAEGG-UPP-I-LISTA                              
072300                                                                          
072400               END-IF                                                     
072500               IF P-TAB-IX-MAX > ZERO                                     
072600                                                                          
072700                 PERFORM H-LAEGG-UPP-P-LISTA                              
072800                                                                          
072900               END-IF                                                     
073000               IF R-TAB-IX-MAX > ZERO                                     
073100                                                                          
073200                 PERFORM I-LAEGG-UPP-R-LISTA                              
073300                                                                          
073400               END-IF                                                     
073500             END-PERFORM                                                  
073600             PERFORM MFS-RENSA-FAELT-IN                                   
073700             IF 6199-IX > ZERO                                            
073800               PERFORM S21-P-TO-P-6199                                    
073900             END-IF                                                       
074000             IF 619A-IX > ZERO                                            
074100               PERFORM S23-P-TO-P-619A                                    
074200             END-IF                                                       
074300           END-IF                                                         
074400                                                                          
074500* * * FÖR SCANNADE KOLLI * * *                                            
074600                                                                          
074700           IF NYCKEL-KOLLI AND INDATA-OK                                  
074800             PERFORM S08-INIT-I-LISTA                                     
074900             PERFORM S09-INIT-P-LISTA                                     
075000             PERFORM S20-INIT-6199                                        
075100             PERFORM S22-INIT-619A                                        
075200             MOVE +1 TO RAD-IX                                            
075300             PERFORM UNTIL RAD-IX > RAD-MAX-IX                            
075400               PERFORM S06-RENSA-TAB-O-INDEX                              
075500                                                                          
075600               PERFORM L-BEHANDLA-KOLLI                                   
075700                                                                          
075800               PERFORM S07-SAETT-MAX-INDEX                                
075900               IF I-TAB-IX-MAX > ZERO                                     
076000                                                                          
076100                 PERFORM G-LAEGG-UPP-I-LISTA                              
076200                                                                          
076300               END-IF                                                     
076400               IF P-TAB-IX-MAX > ZERO                                     
076500                                                                          
076600                 PERFORM H-LAEGG-UPP-P-LISTA                              
076700                                                                          
076800               END-IF                                                     
076900               IF R-TAB-IX-MAX > ZERO                                     
077000                                                                          
077100                 PERFORM I-LAEGG-UPP-R-LISTA                              
077200                                                                          
077300               END-IF                                                     
077400               ADD 1 TO RAD-IX                                            
077500             END-PERFORM                                                  
077600             PERFORM MFS-RENSA-FAELT-IN                                   
077700             IF 6199-IX > ZERO                                            
077800               PERFORM S21-P-TO-P-6199                                    
077900             END-IF                                                       
078000             IF 619A-IX > ZERO                                            
078100               PERFORM S23-P-TO-P-619A                                    
078200             END-IF                                                       
078300           END-IF                                                         
078400*        ELSE                                                             
078500*          IF VCOM-MID                                                    
078600*            CALL ABEND USING RKOD-ABEND-MED-DUMP                         
078700*          END-IF                                                         
078800         END-IF                                                           
078900       ELSE                                                               
079100         IF MFS-ENTER                                                     
079200           IF HELP-MID                                                    
079300             PERFORM C-FLYTTA-MID-TILL-MOD                                
079400           ELSE                                                           
079500             PERFORM S02-INF-PF4-FOR-PRINTING                             
079600           END-IF                                                         
079700         END-IF                                                           
079800       END-IF                                                             
079900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
080000       IF VCOM-MID                                                        
080100         CONTINUE                                                         
080200       ELSE                                                               
080300         PERFORM IMS-INSERT-MSG                                           
080400       END-IF                                                             
080500     END-IF                                                               
080600     MOVE ZERO TO RETURN-CODE                                             
080700     GOBACK                                                               
080800     .                                                                    
080900     EJECT                                                                
081000 A-INIT SECTION.                                                          
081100                                                                          
081200     IF MSG-DUBBLA-TRANSKODER                                             
081300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14101                 
081400                                             VCOM-MID-W6I14102            
081500                                             HTERM-MID-W6I14103           
081600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
081700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
081800     ELSE                                                                 
081900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14101                  
082000                                            VCOM-MID-W6I14102             
082100                                            HTERM-MID-W6I14103            
082200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
082300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
082400     END-IF                                                               
082500                                                                          
082600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
082700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
082800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
082900                                                                          
083000     IF VCOM-MID                                                          
083100       MOVE ALL '+'                TO MID-W6I14101                        
083200       MOVE VCOM-MID-IDLEVNR-KOLLI TO MID-IDLEVNR-KOLLI(1)                
083300       MOVE VCOM-MID-IDOKOLLI      TO MID-IDOKOLLI(1)                     
083400       IF VCOM-MID-IDPTYP = '002'                                         
083500         MOVE 'EHL '               TO MID-ADINLOMR-PRT-UT                 
083600       ELSE                                                               
083700         IF VCOM-MID-IDPTYP = '003'                                       
083800           MOVE 'EGB '               TO MID-ADINLOMR-PRT-UT               
083900         ELSE                                                             
084000** FEL FRÅN VCOM                                                          
084100           PERFORM S90-SEND-MAIL                                          
084200         END-IF                                                           
084300       END-IF                                                             
084400     END-IF                                                               
084500                                                                          
084600     IF MFS-UPD-V                                                         
084700       MOVE ALL '+'                TO MID-W6I14101                        
084800       MOVE HTERM-MID-ADINLOMR-PRT TO MID-ADINLOMR-PRT-UT                 
084900       MOVE +1 TO IX                                                      
085000       PERFORM UNTIL IX > 12 OR                                           
085100               HTERM-MID-IDLEVNR-KOLLI (IX) = LOW-VALUE                   
085200         IF HTERM-MID-IDLEVNR-KOLLI(IX) = ALL '-'                         
085300           MOVE '+++++'                     TO                            
085400                                 MID-IDLEVNR-KOLLI(IX)                    
085500         ELSE                                                             
085600           MOVE HTERM-MID-IDLEVNR-KOLLI(IX) TO                            
085700                                 MID-IDLEVNR-KOLLI(IX)                    
085800         END-IF                                                           
085900         IF HTERM-MID-IDOKOLLI     (IX) = ALL '-'                         
086000           MOVE '+++++++++'                 TO                            
086100                                 MID-IDOKOLLI(IX)                         
086200         ELSE                                                             
086300           MOVE HTERM-MID-IDOKOLLI(IX)      TO                            
086400                                 MID-IDOKOLLI(IX)                         
086500         END-IF                                                           
086600         ADD +1 TO IX                                                     
086700       END-PERFORM                                                        
086800     END-IF                                                               
086900                                                                          
087000     MOVE LOW-VALUE TO MSG-AREA                                           
087100     MOVE 'W6O14101' TO MFS-IDMOD                                         
087200     MOVE '6141' TO MOD-IDTRANS                                           
087300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
087400                                                                          
087500     IF EGEN-MID OR HELP-MID                                              
087600       CONTINUE                                                           
087700     END-IF                                                               
087800                                                                          
087900     IF ENGLISH-TEXT                                                      
088000       MOVE +2 TO SPRAK-IX                                                
088100       MOVE 'GB ' TO MED-IDSKYLT                                          
088200     ELSE                                                                 
088300       MOVE +1 TO SPRAK-IX                                                
088400       MOVE 'S  ' TO MED-IDSKYLT                                          
088500     END-IF                                                               
088600     MOVE LOW-VALUE      TO  W-W6D1F1KY-MIN-X                             
088700                             W-W6D1E1KY-MIN-X                             
088800                             W-W6D1C1KY-MIN-X                             
088900                             W-W6D1DSEQ-MIN-X                             
089000                                                                          
089100     MOVE HIGH-VALUE     TO  W-W6D1F1KY-MAX-X                             
089200                             W-W6D1E1KY-MAX-X                             
089300                             W-W6D1C1KY-MAX-X                             
089400                             W-W6D1DSEQ-MAX-X                             
089500     PERFORM AA-SKAPA-TOM-D-TAB                                           
089600     PERFORM AB-SKAPA-TOM-I-TAB                                           
089700     PERFORM AC-SKAPA-TOM-P-TAB                                           
089800     PERFORM AD-SKAPA-TOM-R-TAB                                           
089900     .                                                                    
090000     EJECT                                                                
090100 AA-SKAPA-TOM-D-TAB SECTION.                                              
090200                                                                          
090300     MOVE +1 TO D-TAB-IX                                                  
090400     PERFORM UNTIL D-TAB-IX > D-TAB-MAX-IX                                
090500       MOVE ZEROS  TO D-TOM-IDOKOLLI(D-TAB-IX)                            
090600                      D-TOM-ADLAGOMR(D-TAB-IX)                            
090700                      D-TOM-ADGANG(D-TAB-IX)                              
090800                      D-TOM-ADPLATS(D-TAB-IX)                             
090900                      D-TOM-IDARTNR(D-TAB-IX)                             
091000                      D-TOM-IDRADNR-INL(D-TAB-IX)                         
091100                      D-TOM-TIAVIDAT(D-TAB-IX)                            
091200                      D-TOM-IDRADNR(D-TAB-IX)                             
091300       MOVE SPACE  TO D-TOM-IDFS(D-TAB-IX)                                
091400                      D-TOM-RETL(D-TAB-IX)                                
091500                      D-TOM-IDLEVNR-KOLLI(D-TAB-IX)                       
091600                      D-TOM-IDLEVNR(D-TAB-IX)                             
091700       MOVE '0000' TO D-TOM-TORG(D-TAB-IX)                                
091800       ADD 1 TO D-TAB-IX                                                  
091900     END-PERFORM                                                          
092000     .                                                                    
092100     EJECT                                                                
092200 AB-SKAPA-TOM-I-TAB SECTION.                                              
092300                                                                          
092400     MOVE +1 TO I-TAB-IX                                                  
092500     PERFORM UNTIL I-TAB-IX > I-TAB-MAX-IX                                
092600       MOVE ZEROS  TO I-TOM-IDOKOLLI(I-TAB-IX)                            
092700                      I-TOM-ADLAGOMR(I-TAB-IX)                            
092800                      I-TOM-ADGANG(I-TAB-IX)                              
092900                      I-TOM-ADPLATS(I-TAB-IX)                             
093000                      I-TOM-IDARTNR(I-TAB-IX)                             
093100                      I-TOM-IDRADNR-INL(I-TAB-IX)                         
093200                      I-TOM-TIAVIDAT(I-TAB-IX)                            
093300                      I-TOM-IDRADNR(I-TAB-IX)                             
093400                      I-TOM-IDILIST(I-TAB-IX)                             
093500                      I-TOM-IDILIRAD(I-TAB-IX)                            
093600       MOVE SPACE  TO I-TOM-IDFS(I-TAB-IX)                                
093700                      I-TOM-IDLEVNR-KOLLI(I-TAB-IX)                       
093800                      I-TOM-IDLEVNR(I-TAB-IX)                             
093900       MOVE '0000' TO I-TOM-TORG(I-TAB-IX)                                
094000       ADD 1 TO I-TAB-IX                                                  
094100     END-PERFORM                                                          
094200     MOVE ZERO     TO I-TAB-IX                                            
094300     .                                                                    
094400     EJECT                                                                
094500 AC-SKAPA-TOM-P-TAB SECTION.                                              
094600                                                                          
094700     MOVE +1 TO P-TAB-IX                                                  
094800     PERFORM UNTIL P-TAB-IX > P-TAB-MAX-IX                                
094900       MOVE ZEROS  TO P-TOM-IDARTNR(P-TAB-IX)                             
095000                      P-TOM-IDRADNR-INL(P-TAB-IX)                         
095100                      P-TOM-IDOKOLLI(P-TAB-IX)                            
095200                      P-TOM-TIAVIDAT(P-TAB-IX)                            
095300                      P-TOM-IDRADNR(P-TAB-IX)                             
095400                      P-TOM-IDILIST(P-TAB-IX)                             
095500                      P-TOM-IDILIRAD(P-TAB-IX)                            
095600       MOVE SPACE  TO P-TOM-IDFS(P-TAB-IX)                                
095700                      P-TOM-IDLEVNR-KOLLI(P-TAB-IX)                       
095800                      P-TOM-IDLEVNR(P-TAB-IX)                             
095900       ADD 1 TO P-TAB-IX                                                  
096000     END-PERFORM                                                          
096100     MOVE ZERO     TO P-TAB-IX                                            
096200     MOVE PLA-TAB-TOM  TO  PLA-TAB                                        
096300     .                                                                    
096400     EJECT                                                                
096500 AD-SKAPA-TOM-R-TAB SECTION.                                              
096600                                                                          
096700     MOVE +1 TO R-TAB-IX                                                  
096800     PERFORM UNTIL R-TAB-IX > R-TAB-MAX-IX                                
096900       MOVE ZEROS  TO R-TOM-IDARTNR(R-TAB-IX)                             
097000                      R-TOM-IDRADNR-INL(R-TAB-IX)                         
097100                      R-TOM-IDOKOLLI(R-TAB-IX)                            
097200                      R-TOM-TIAVIDAT(R-TAB-IX)                            
097300                      R-TOM-IDRADNR(R-TAB-IX)                             
097400       MOVE SPACE  TO R-TOM-IDFS(R-TAB-IX)                                
097500                      R-TOM-ADINLOMR-FB(R-TAB-IX)                         
097600                      R-TOM-IDLEVNR-KOLLI(R-TAB-IX)                       
097700                      R-TOM-IDLEVNR(R-TAB-IX)                             
097800       ADD 1 TO R-TAB-IX                                                  
097900     END-PERFORM                                                          
098000     MOVE ZERO     TO R-TAB-IX                                            
098100     .                                                                    
098200     EJECT                                                                
098300 B-FORMELL-KONTROLL SECTION.                                              
098400                                                                          
098500     MOVE JA     TO INDATA-SW                                             
098600     MOVE SPACE  TO MED-IDMFSFEL                                          
098700     IF (MID-IDINLVGN   = ALL '+' OR ALL SPACE) AND                       
098800        (MID-ADINLOMR   = ALL '+' OR ALL SPACE)                           
098900                                                                          
099000        PERFORM BA-KOLL-KOLLI                                             
099100                                                                          
099200        IF NYCKLAR-SAKNAS                                                 
099300           IF INDATA-FEL                                                  
099400              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
099500           ELSE                                                           
099600              MOVE ERR-NO-LINE          TO MED-IDMFSFEL                   
099700              MOVE NEJ TO INDATA-SW                                       
099800           END-IF                                                         
099900        ELSE                                                              
100000           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDINLVGN-ATTR                 
100100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-ATTR                 
100200           MOVE 'K' TO NYCKEL-VAERDE                                      
100300        END-IF                                                            
100400     ELSE                                                                 
100500        IF (MID-IDINLVGN     = ALL '+' OR ALL SPACE) AND                  
100600           (MID-ADINLOMR NOT = ALL '+' OR ALL SPACE)                      
100700                                                                          
100800           PERFORM BB-KOLL-PLAC                                           
100900                                                                          
101000        ELSE                                                              
101100           IF MID-IDINLVGN NUMERIC   AND                                  
101200             (MID-ADINLOMR = ALL '+' OR ALL SPACE)                        
101300              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDINLVGN-ATTR               
101400              MOVE 'V' TO NYCKEL-VAERDE                                   
101500              MOVE MID-IDINLVGN TO W-F-MIN-IDINLVGN                       
101600                                   W-F-MAX-IDINLVGN                       
101700                                   TAB-IDINLVGN                           
101800           ELSE                                                           
101900             IF MID-ADINLOMR NOT = ALL '+' OR ALL SPACE                   
102000                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDINLVGN-ATTR             
102100                                            MOD-ADINLOMR-ATTR             
102200             ELSE                                                         
102300                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDINLVGN-ATTR             
102400             END-IF                                                       
102500             MOVE NEJ TO INDATA-SW                                        
102600           END-IF                                                         
102700        END-IF                                                            
102800     END-IF                                                               
102900                                                                          
103000     PERFORM BC-KOLL-PRINTER                                              
103100                                                                          
103200     IF INDATA-OK                                                         
103300        IF NYCKEL-VAGN                                                    
103400          MOVE TAB-IDINLVGN TO MOD-IDINLVGN                               
103500          INSPECT MOD-IDINLVGN REPLACING LEADING ZERO BY SPACE            
103600        ELSE                                                              
103700          IF NYCKEL-PLAC                                                  
103800            MOVE TAB-ADINLOMR TO MOD-ADINLOMR                             
103900          ELSE                                                            
104000            IF NYCKEL-KOLLI                                               
104100              MOVE +1 TO RAD-IX                                           
104200              PERFORM UNTIL RAD-IX > RAD-MAX-IX                           
104300                MOVE W-IDLEVNR-KOLLI(RAD-IX) TO                           
104400                        MOD-IDLEVNR-KOLLI(RAD-IX)                         
104500                MOVE W-IDOKOLLI(RAD-IX) TO MOD-IDOKOLLI(RAD-IX)           
104600                INSPECT MOD-IDOKOLLI(RAD-IX)                              
104700                        REPLACING LEADING ZERO BY SPACE                   
104800                MOVE W-TEMFSMED(RAD-IX) TO MOD-TEMFSMED(RAD-IX)           
104900                ADD 1 TO RAD-IX                                           
105000              END-PERFORM                                                 
105100            END-IF                                                        
105200          END-IF                                                          
105300        END-IF                                                            
105400     ELSE                                                                 
105500        IF MED-IDMFSFEL = SPACE                                           
105600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
105700        END-IF                                                            
105800        CALL WMEDKONV USING MED-WMEDAREA                                  
105900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
106000        IF NOT MFS-UPD-V                                                  
106100          PERFORM MFS-ROER-EJ-FAELT-IN                                    
106200        END-IF                                                            
106300     END-IF                                                               
106400     .                                                                    
106500     EJECT                                                                
106600 BA-KOLL-KOLLI SECTION.                                                   
106700                                                                          
106800     MOVE +1 TO RAD-IX                                                    
106900     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
107000      IF (MID-IDLEVNR-KOLLI(RAD-IX)  = ALL '+' OR ALL SPACE) AND          
107100         (MID-IDOKOLLI(RAD-IX)       = ALL '+' OR ALL SPACE)              
107200         MOVE MFS-ALFA-FAELT-RAETT   TO                                   
107300                              MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)              
107400         MOVE MFS-NUM-FAELT-RAETT   TO                                    
107500                              MOD-IDOKOLLI-ATTR(RAD-IX)                   
107600         MOVE ZEROS TO  W-IDOKOLLI(RAD-IX)                                
107700         MOVE SPACE TO  W-TEMFSMED(RAD-IX)                                
107800                        MOD-TEMFSMED(RAD-IX)                              
107900                        W-IDLEVNR-KOLLI(RAD-IX)                           
108000      ELSE                                                                
108100         INSPECT MID-IDOKOLLI(RAD-IX)                                     
108200                                 REPLACING LEADING SPACE BY ZERO          
108300         IF MID-IDLEVNR-KOLLI(RAD-IX)  NOT = SPACE AND                    
108400            MID-IDOKOLLI(RAD-IX)       NUMERIC                            
108500            MOVE MFS-ALFA-FAELT-RAETT   TO                                
108600                                 MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)           
108700            MOVE MFS-NUM-FAELT-RAETT   TO                                 
108800                                 MOD-IDOKOLLI-ATTR(RAD-IX)                
108900            MOVE MID-IDLEVNR-KOLLI(RAD-IX)  TO                            
109000                                 W-IDLEVNR-KOLLI(RAD-IX)                  
109100            MOVE MID-IDOKOLLI(RAD-IX)  TO  W-IDOKOLLI(RAD-IX)             
109200            MOVE 'J' TO NYCKEL-VAERDE                                     
109300         ELSE                                                             
109400            MOVE MFS-ALFA-FAELT-FEL     TO                                
109500                                 MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)           
109600            MOVE MFS-NUM-FAELT-FEL     TO                                 
109700                                 MOD-IDOKOLLI-ATTR(RAD-IX)                
109800            MOVE NEJ TO INDATA-SW                                         
109900            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
110000         END-IF                                                           
110100      END-IF                                                              
110200      ADD 1 TO RAD-IX                                                     
110300     END-PERFORM                                                          
110400     .                                                                    
110500     EJECT                                                                
110600 BB-KOLL-PLAC SECTION.                                                    
110700                                                                          
110800     MOVE MID-ADINLOMR TO W-E-MIN-ADINLOMR                                
110900                          W-E-MAX-ADINLOMR                                
111000                          W-6006-ADINLOMR                                 
111100                          TAB-ADINLOMR                                    
111200     PERFORM IMS-GU-PLAA-PLAA11                                           
111300     IF SEGMENT-FINNS AND 6006-KDINLOMR = 'LO ' OR                        
111400                          6006-KDINLOMR = 'TRG' OR                        
111500                          6006-KDINLOMR = 'BO ' OR                        
111600                          6006-KDINLOMR = 'FB ' OR                        
111700                          6006-KDINLOMR = 'FBP' OR                        
111800                          6006-KDINLOMR = 'RTA'                           
111900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-ATTR                     
112000       MOVE 'P' TO NYCKEL-VAERDE                                          
112100     ELSE                                                                 
112200       MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-ATTR                     
112300       MOVE NEJ TO INDATA-SW                                              
112400     END-IF                                                               
112500     .                                                                    
112600     EJECT                                                                
112700 BC-KOLL-PRINTER SECTION.                                                 
112800                                                                          
112900     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-UT-ATTR                
113000     IF MID-ADINLOMR-PRT-IN = ALL '+' OR ALL SPACE                        
113100       IF MID-ADINLOMR-PRT-UT = ALL SPACE                                 
113200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-PRT-IN-ATTR            
113300         MOVE NEJ TO INDATA-SW                                            
113400       ELSE                                                               
113500         MOVE MID-ADINLOMR-PRT-UT TO W-ADINLOMR-PRT                       
113600         MOVE JA TO PRINTER-SW                                            
113700       END-IF                                                             
113800     ELSE                                                                 
113900       MOVE MID-ADINLOMR-PRT-IN TO W-ADINLOMR-PRT                         
114000       MOVE JA TO PRINTER-SW                                              
114100     END-IF                                                               
114200     IF PRINTER-VALD                                                      
114300        MOVE SPACE                  TO PRT-IDPRTLST                       
114400        MOVE '6M'                   TO PRT-IDPRTLST (1:2)                 
114500        MOVE W-ADINLOMR-PRT         TO PRT-IDPRTLST (3:6)                 
114600        MOVE  1                     TO PRT-KDCALL                         
114700        CALL W006PRT  USING PRT-W006PRT                                   
114800        IF PRT-KDSVAR               = 'F'                                 
114900           MOVE NEJ                TO INDATA-SW                           
115000           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-IN-ATTR            
115100           MOVE W-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT-IN                 
115200        ELSE                                                              
115300           MOVE PRT-IDPRTLST         TO TAB-IDPRTLST                      
115400           MOVE PRT-BEPRTLST         TO W-BEPRTLST                        
115500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-UT-ATTR          
115600           MOVE W-ADINLOMR-PRT       TO MOD-ADINLOMR-PRT-UT               
115700        END-IF                                                            
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100 C-FLYTTA-MID-TILL-MOD SECTION.                                           
116200                                                                          
116300     MOVE INF-PF4-FOR-PRINTING      TO MED-IDMFSINF                       
116400     CALL WMEDKONV USING MED-WMEDAREA                                     
116500     MOVE MED-MFSINF                TO  MOD-TEMFSINF                      
116600                                                                          
116700     IF MID-IDINLVGN                =   ALL '+'                           
116800        MOVE MFS-RENSA-FAELT        TO  MOD-IDINLVGN                      
116900     ELSE                                                                 
117000        MOVE MID-IDINLVGN           TO  MOD-IDINLVGN                      
117100        MOVE MFS-ADD-LAES-IN-FAELT  TO  MOD-IDINLVGN-ATTR                 
117200     END-IF                                                               
117300                                                                          
117400     IF MID-ADINLOMR                =   ALL '+'                           
117500        MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR                      
117600     ELSE                                                                 
117700        MOVE MID-ADINLOMR           TO  MOD-ADINLOMR                      
117800        MOVE MFS-ADD-LAES-IN-FAELT  TO  MOD-ADINLOMR-ATTR                 
117900     END-IF                                                               
118000                                                                          
118100     IF MID-ADINLOMR-PRT-IN         =   ALL '+'                           
118200        MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-PRT-IN               
118300     ELSE                                                                 
118400        MOVE MID-ADINLOMR-PRT-IN    TO  MOD-ADINLOMR-PRT-IN               
118500        MOVE MFS-ADD-LAES-IN-FAELT  TO  MOD-ADINLOMR-PRT-IN-ATTR          
118600     END-IF                                                               
118700                                                                          
118800     IF MID-ADINLOMR-PRT-UT         =   ALL '+'                           
118900        MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-PRT-UT               
119000     ELSE                                                                 
119100        MOVE MID-ADINLOMR-PRT-UT    TO  MOD-ADINLOMR-PRT-UT               
119200        MOVE MFS-ADD-LAES-IN-FAELT  TO  MOD-ADINLOMR-PRT-UT-ATTR          
119300     END-IF                                                               
119400                                                                          
119500     MOVE  +1                       TO  RAD-IX                            
119600     PERFORM UNTIL RAD-IX           >   RAD-MAX-IX                        
119700       IF MID-IDLEVNR-KOLLI(RAD-IX) =   ALL '+'                           
119800          MOVE MFS-RENSA-FAELT      TO  MOD-IDLEVNR-KOLLI(RAD-IX)         
119900       ELSE                                                               
120000          MOVE MID-IDLEVNR-KOLLI(RAD-IX) TO                               
120100                                   MOD-IDLEVNR-KOLLI(RAD-IX)              
120200          MOVE MFS-ADD-LAES-IN-FAELT TO                                   
120300                                   MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)         
120400       END-IF                                                             
120500                                                                          
120600       IF MID-IDOKOLLI(RAD-IX)      =   ALL '+'                           
120700          MOVE MFS-RENSA-FAELT      TO  MOD-IDOKOLLI(RAD-IX)              
120800       ELSE                                                               
120900          MOVE MID-IDOKOLLI(RAD-IX)  TO                                   
121000                                   MOD-IDOKOLLI(RAD-IX)                   
121100          MOVE MFS-ADD-LAES-IN-FAELT TO                                   
121200                                   MOD-IDOKOLLI-ATTR(RAD-IX)              
121300       END-IF                                                             
121400                                                                          
121500       IF MID-TEMFSMED(RAD-IX)      =   ALL '+'                           
121600          MOVE MFS-RENSA-FAELT      TO  MOD-TEMFSMED(RAD-IX)              
121700       ELSE                                                               
121800          MOVE MID-TEMFSMED(RAD-IX)  TO                                   
121900                                   MOD-TEMFSMED(RAD-IX)                   
122000          MOVE MFS-ADD-LAES-IN-FAELT TO                                   
122100                                   MOD-TEMFSMED-ATTR(RAD-IX)              
122200       END-IF                                                             
122300                                                                          
122400       ADD  1                       TO  RAD-IX                            
122500     END-PERFORM                                                          
122600     .                                                                    
122700     EJECT                                                                
122800 F-KONTROLLERA-BEHANDLA-INDATA SECTION.                                   
122900                                                                          
123000     MOVE NEJ TO  UTSKRIFT-SW                                             
123100                  KOLLIRAD-SW                                             
123200                  DIVKOLLI-SW                                             
123300     MOVE ZERO TO D-TAB-IX  D-TAB-IX-MAX                                  
123400                  I-TAB-IX  I-TAB-IX-MAX                                  
123500                  P-TAB-IX  P-TAB-IX-MAX                                  
123600                  R-TAB-IX  R-TAB-IX-MAX                                  
123700     IF NYCKEL-VAGN                                                       
123800        PERFORM FA-BEHANDLA-VAGN                                          
123900        IF ALLA-RADER-UTSKRIVNA AND DIVKOLLI-FINNS-EJ                     
124000           PERFORM S01-ERR-NO-PRINTING-FIELD                              
124100        END-IF                                                            
124200     ELSE                                                                 
124300       IF NYCKEL-PLAC                                                     
124400          PERFORM FB-BEHANDLA-PLAC                                        
124500          IF ALLA-RADER-UTSKRIVNA AND DIVKOLLI-FINNS-EJ                   
124600             PERFORM S01-ERR-NO-PRINTING-FIELD                            
124700          END-IF                                                          
124800       ELSE                                                               
124900         IF NYCKEL-KOLLI                                                  
125000            PERFORM FC-KONTROLL-KOLLI                                     
125100            IF INDATA-FEL                                                 
125200              IF NOT MFS-UPD-V                                            
125300                PERFORM MFS-ROER-EJ-FAELT-IN                              
125400              END-IF                                                      
125500*             PERFORM MFS-ROER-EJ-FAELT-UT                                
125600*             PERFORM MFS-LAES-IN-IGEN                                    
125700            END-IF                                                        
125800         END-IF                                                           
125900       END-IF                                                             
126000     END-IF                                                               
126100     IF INDATA-FEL AND (NYCKEL-VAGN OR NYCKEL-PLAC)                       
126200       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
126300       CALL WMEDKONV USING MED-WMEDAREA                                   
126400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
126500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
126600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
126700       PERFORM MFS-LAES-IN-IGEN                                           
126800     END-IF                                                               
126900     .                                                                    
127000     EJECT                                                                
127100 FA-BEHANDLA-VAGN SECTION.                                                
127200                                                                          
127300     PERFORM IMS-GU-INLG01                                                
127400                                                                          
127500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
127600       MOVE SEQF-IDRADNR-INL TO W-W6D111-IDRADNR-INL                      
127700       MOVE SEQF-IDLEVNR  TO  W-W6D101-IDLEVNR                            
127800                              W-IDLEVNR                                   
127900       MOVE SEQF-IDFS     TO  W-W6D101-IDFS                               
128000                              W-IDFS                                      
128100       MOVE SEQF-TIAVIDAT TO  W-W6D101-TIAVIDAT                           
128200                              W-TIAVIDAT                                  
128300       MOVE SEQF-IDRADNR  TO  W-W6D121-IDRADNR                            
128400       MOVE SEQF-IDDC     TO  WS-IDDC                                     
128500       IF CDC-SE                                                          
128600         PERFORM IMS-GU-INLA11                                            
128700         PERFORM IMS-GU-INLA21                                            
128800                                                                          
128900         PERFORM FAA-BEHANDLA-INL-REG-VAGN                                
129000       END-IF                                                             
129100       PERFORM IMS-GN-INLG01                                              
129200     END-PERFORM                                                          
129300                                                                          
129400     PERFORM S07-SAETT-MAX-INDEX                                          
129500     .                                                                    
129600     EJECT                                                                
129700 FAA-BEHANDLA-INL-REG-VAGN SECTION.                                       
129800                                                                          
129900     IF (RAD-KDINLSTA = 'SAK' OR 'FPK' OR '   ') AND                      
130000        (RAD-IDILIST  =  ZERO)                                            
130100        IF RAD-FLDIVKLI = JA                                              
130200           PERFORM S10-FLYTTA-TILL-D-TAB                                  
130300           MOVE JA  TO DIVKOLLI-SW                                        
130400        ELSE                                                              
130500           MOVE JA  TO UTSKRIFT-SW                                        
130600           PERFORM S30-PRIM-SEK-KOLL                                      
130700           IF ART-FLKVAFEL   = JA   OR                                    
130800              ART-FLKVAKAR   = JA   OR                                    
130900              RAD-FLSATS     = JA   OR                                    
131000              ART-VKART      = ZERO OR                                    
131100              ART-VLARTNTO   = ZERO OR                                    
131200              ART-KDARTURS   = SPACE OR                                   
131300              WS-FL-PRIM-SEK = JA                                         
131400                                                                          
131500              PERFORM S14-FLYTTA-TILL-R-TAB                               
131600                                                                          
131700           ELSE                                                           
131800                                                                          
131900              IF ART-ADLAGOMR = ZERO OR                                   
132000                 ART-ADPLATS = ZERO                                       
132100                                                                          
132200                 PERFORM S15-FLYTTA-TILL-P-TAB                            
132300                                                                          
132400              ELSE                                                        
132500                                                                          
132600                 PERFORM S16-FLYTTA-TILL-I-TAB                            
132700                                                                          
132800              END-IF                                                      
132900           END-IF                                                         
133000        END-IF                                                            
133100     END-IF                                                               
133200     .                                                                    
133300     EJECT                                                                
133400 FB-BEHANDLA-PLAC SECTION.                                                
133500                                                                          
133600     PERFORM IMS-GU-INLF01                                                
133700                                                                          
133800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
133900       MOVE SEQE-IDRADNR-INL TO W-W6D111-IDRADNR-INL                      
134000       MOVE SEQE-IDLEVNR  TO  W-W6D101-IDLEVNR                            
134100                              W-IDLEVNR                                   
134200       MOVE SEQE-IDFS     TO  W-W6D101-IDFS                               
134300                              W-IDFS                                      
134400       MOVE SEQE-TIAVIDAT TO  W-W6D101-TIAVIDAT                           
134500                              W-TIAVIDAT                                  
134600       MOVE SEQE-IDRADNR  TO  W-W6D121-IDRADNR                            
134700       MOVE SEQE-IDDC     TO  WS-IDDC                                     
134800       IF CDC-SE                                                          
134900         PERFORM IMS-GU-INLA11                                            
135000         PERFORM IMS-GU-INLA21                                            
135100                                                                          
135200         PERFORM FBA-BEHANDLA-INL-REG-PLAC                                
135300       END-IF                                                             
135400       PERFORM IMS-GN-INLF01                                              
135500     END-PERFORM                                                          
135600                                                                          
135700     PERFORM S07-SAETT-MAX-INDEX                                          
135800     .                                                                    
135900     EJECT                                                                
136000 FBA-BEHANDLA-INL-REG-PLAC SECTION.                                       
136100                                                                          
136200     IF (RAD-KDINLSTA = 'SAK' OR 'FPK' OR '   ') AND                      
136300        (RAD-IDILIST  =  ZERO)                                            
136400        IF RAD-FLDIVKLI = JA                                              
136500           PERFORM S10-FLYTTA-TILL-D-TAB                                  
136600           MOVE JA  TO DIVKOLLI-SW                                        
136700        ELSE                                                              
136800           MOVE JA  TO UTSKRIFT-SW                                        
136900           PERFORM S30-PRIM-SEK-KOLL                                      
137000           IF ART-FLKVAFEL = JA  OR                                       
137100              ART-FLKVAKAR = JA  OR                                       
137200              RAD-FLSATS   = JA  OR                                       
137300              ART-VKART      = ZERO OR                                    
137400              ART-VLARTNTO   = ZERO OR                                    
137500              ART-KDARTURS   = SPACE OR                                   
137600              WS-FL-PRIM-SEK = JA                                         
137700                                                                          
137800              PERFORM S14-FLYTTA-TILL-R-TAB                               
137900                                                                          
138000           ELSE                                                           
138100***  REGLER FÖR PLATSSÄTTARLISTAN KOMMER                                  
138200              IF ART-ADLAGOMR = ZERO OR                                   
138300                 ART-ADPLATS = ZERO                                       
138400                                                                          
138500                 PERFORM S15-FLYTTA-TILL-P-TAB                            
138600                                                                          
138700              ELSE                                                        
138800                                                                          
138900                 PERFORM S16-FLYTTA-TILL-I-TAB                            
139000                                                                          
139100              END-IF                                                      
139200           END-IF                                                         
139300        END-IF                                                            
139400     END-IF                                                               
139500     .                                                                    
139600     EJECT                                                                
139700 FC-KONTROLL-KOLLI SECTION.                                               
139800                                                                          
139900     MOVE +1 TO RAD-IX                                                    
140000     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
140100       MOVE NEJ TO RAD-SW                                                 
140200       PERFORM UNTIL RAD-IX > RAD-MAX-IX    OR                            
140300         (W-IDLEVNR-KOLLI(RAD-IX) NOT = SPACE     AND                     
140400          W-IDOKOLLI(RAD-IX)      NOT = ALL ZEROS)                        
140500          ADD 1 TO RAD-IX                                                 
140600       END-PERFORM                                                        
140700                                                                          
140800       IF RAD-IX NOT > RAD-MAX-IX                                         
140900         MOVE LOW-VALUE               TO  W-W6D1C1KY-MIN-X                
141000         MOVE HIGH-VALUE              TO  W-W6D1C1KY-MAX-X                
141100         MOVE W-IDLEVNR-KOLLI(RAD-IX) TO  W-C-MIN-IDLEVNR-KOLLI           
141200                                          W-C-MAX-IDLEVNR-KOLLI           
141300         MOVE W-IDOKOLLI(RAD-IX)      TO  W-C-MIN-IDOKOLLI                
141400                                          W-C-MAX-IDOKOLLI                
141500         PERFORM IMS-GU-INLD01                                            
141600         IF SEGMENT-FINNS                                                 
141700            PERFORM UNTIL SEGMENT-SAKNAS OR                               
141800                          SEGMENT-SLUT   OR                               
141900                          RAD-OK                                          
142000              MOVE SEQC-IDRADNR-INL TO W-W6D111-IDRADNR-INL               
142100              MOVE SEQC-IDLEVNR  TO  W-W6D101-IDLEVNR                     
142200                                     W-IDLEVNR                            
142300              MOVE SEQC-IDFS     TO  W-W6D101-IDFS                        
142400                                     W-IDFS                               
142500              MOVE SEQC-TIAVIDAT TO  W-W6D101-TIAVIDAT                    
142600                                     W-TIAVIDAT                           
142700              MOVE SEQC-IDRADNR  TO  W-W6D121-IDRADNR                     
142800              MOVE SEQC-IDDC     TO  WS-IDDC                              
142900              IF CDC-SE                                                   
143000                PERFORM IMS-GU-INLA21                                     
143100                                                                          
143200                PERFORM FCA-KONTROLL-INL-REG-KOLLI                        
143300              END-IF                                                      
143400                                                                          
143500              PERFORM IMS-GN-INLD01                                       
143600            END-PERFORM                                                   
143700            IF REDAN-PRINTAD AND RAD-FEL                                  
143800              PERFORM S05-INF-ALREADY-PRINTED                             
143900              MOVE NEJ TO REDAN-PRINTAD-SW                                
144000            END-IF                                                        
144100            IF RAD-FEL    AND                                             
144200               MOD-TEMFSMED(RAD-IX) = SPACE                               
144300               PERFORM S03-ERR-CORR-HILITE-FLDS                           
144400            END-IF                                                        
144500         ELSE                                                             
144600           PERFORM S04-ERR-NOT-ON-REGISTER                                
144700         END-IF                                                           
144800         ADD 1 TO RAD-IX                                                  
144900       END-IF                                                             
145000     END-PERFORM                                                          
145100     .                                                                    
145200     EJECT                                                                
145300 FCA-KONTROLL-INL-REG-KOLLI SECTION.                                      
145400                                                                          
145500     IF SEGMENT-FINNS                                                     
145600        IF RAD-KDINLSTA   =  'SAK' OR 'FPK' OR '   '                      
145700           IF RAD-IDILIST > +0                                            
145800             MOVE JA  TO REDAN-PRINTAD-SW                                 
145900           ELSE                                                           
146000             MOVE JA                    TO RAD-SW                         
146100             MOVE MFS-ADD-LAES-IN-FAELT TO                                
146200                                    MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)        
146300                                    MOD-IDOKOLLI-ATTR(RAD-IX)             
146400           END-IF                                                         
146500        END-IF                                                            
146600     ELSE                                                                 
146700        PERFORM S04-ERR-NOT-ON-REGISTER                                   
146800     END-IF                                                               
146900     .                                                                    
147000     EJECT                                                                
147100 G-LAEGG-UPP-I-LISTA SECTION.                                             
147200                                                                          
147300     PERFORM GA-SORT-I-LIST                                               
147400     PERFORM GB-REGISTRERA-TORG                                           
147500     PERFORM GA-SORT-I-LIST                                               
147600     PERFORM GC-REG-LISTNR-RADNR-DAT-REPL                                 
147700     MOVE ZEROS TO I-TAB-IX                                               
147800                   I-TAB-IX-MAX                                           
147900     .                                                                    
148000     EJECT                                                                
148100 GA-SORT-I-LIST SECTION.                                                  
148200                                                                          
148300     MOVE +61            TO  STEGLNGD                                     
148400     MOVE I-TAB-IX-MAX   TO  ANTAL                                        
148500     MOVE +30            TO  NYCKELLNGD                                   
148600                                                                          
148700     CALL WINTSOR        USING INL-TAB STEGLNGD ANTAL                     
148800                               I-TAB-SORTNYCKEL(1) NYCKELLNGD             
148900     .                                                                    
149000     EJECT                                                                
149100 GB-REGISTRERA-TORG SECTION.                                              
149200                                                                          
149300     MOVE +1 TO I-TAB-IX                                                  
149400     MOVE I-TAB-ADLAGOMR(I-TAB-IX)   TO W-SPAR-ADLAGOMR                   
149500                                        W-JFR-ADLAGOMR                    
149600     MOVE SPACE                      TO W-6006-ADINLOMR-PAR               
149700     MOVE W-SPAR-ADLAGOMR            TO W-6006-ADINLOMR-PAR               
149800     PERFORM IMS-GU-PLAA11-PAR                                            
149900     PERFORM UNTIL I-TAB-IX > I-TAB-IX-MAX                                
150000       MOVE NEJ TO TAB-TRAEFF-SW                                          
150100       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR TAB-TRAEFF         
150200         IF 6006-KDINLOMR     = 'TRG'                                     
150300           IF I-TAB-ADGANG(I-TAB-IX) = ZERO                               
150400             IF (6006-ADPLATS-FOM <= I-TAB-ADPLATS(I-TAB-IX) AND          
150500                 6006-ADPLATS-TOM >= I-TAB-ADPLATS(I-TAB-IX))             
150600                MOVE 6006-ADINLOMR TO I-TAB-TORG(I-TAB-IX)                
150700                MOVE JA TO TAB-TRAEFF-SW                                  
150800             ELSE                                                         
150900                PERFORM IMS-GN-PLAA11-PAR                                 
151000             END-IF                                                       
151100           ELSE                                                           
151200             IF (6006-ADGANG-FOM  <= I-TAB-ADGANG (I-TAB-IX)  AND         
151300                 6006-ADGANG-TOM  >= I-TAB-ADGANG (I-TAB-IX))             
151400                MOVE 6006-ADINLOMR TO I-TAB-TORG(I-TAB-IX)                
151500                MOVE JA TO TAB-TRAEFF-SW                                  
151600             ELSE                                                         
151700                PERFORM IMS-GN-PLAA11-PAR                                 
151800             END-IF                                                       
151900           END-IF                                                         
152000         ELSE                                                             
152100           PERFORM IMS-GN-PLAA11-PAR                                      
152200         END-IF                                                           
152300       END-PERFORM                                                        
152400                                                                          
152500       IF TAB-TRAEFF                                                      
152600         MOVE NEJ TO TORG-SW                                              
152700         ADD 1 TO I-TAB-IX                                                
152800         IF I-TAB-ADLAGOMR(I-TAB-IX) = W-JFR-ADLAGOMR                     
152900            IF 6006-ADGANG-TOM = ZERO AND                                 
153000               I-TAB-ADGANG(I-TAB-IX) = ZERO                              
153100               IF (I-TAB-ADPLATS(I-TAB-IX) < 6006-ADPLATS-TOM OR          
153200                   I-TAB-ADPLATS(I-TAB-IX) = 6006-ADPLATS-TOM)            
153300                   MOVE JA TO TORG-SW                                     
153400               END-IF                                                     
153500            ELSE                                                          
153600               IF (I-TAB-ADGANG(I-TAB-IX) < 6006-ADGANG-TOM OR            
153700                   I-TAB-ADGANG(I-TAB-IX) = 6006-ADGANG-TOM)              
153800                   MOVE JA TO TORG-SW                                     
153900               END-IF                                                     
154000            END-IF                                                        
154100         END-IF                                                           
154200                                                                          
154300         PERFORM UNTIL (I-TAB-IX > I-TAB-IX-MAX)  OR                      
154400                       (I-TAB-ADLAGOMR(I-TAB-IX)  NOT =                   
154500                            W-JFR-ADLAGOMR)       OR                      
154600                       NYTT-TORG                                          
154700           MOVE 6006-ADINLOMR TO I-TAB-TORG(I-TAB-IX)                     
154800           ADD 1 TO I-TAB-IX                                              
154900           IF 6006-ADGANG-TOM = ZERO                                      
155000              IF  I-TAB-ADPLATS(I-TAB-IX) > 6006-ADPLATS-TOM              
155100                  MOVE NEJ TO TORG-SW                                     
155200              END-IF                                                      
155300           END-IF                                                         
155400           IF  I-TAB-ADGANG(I-TAB-IX) >  6006-ADGANG-TOM                  
155500               MOVE NEJ TO TORG-SW                                        
155600           END-IF                                                         
155700         END-PERFORM                                                      
155800       ELSE                                                               
155900         ADD 1 TO I-TAB-IX                                                
156000       END-IF                                                             
156100       IF I-TAB-ADLAGOMR(I-TAB-IX) = W-JFR-ADLAGOMR                       
156200         PERFORM IMS-GU-PLAA11-PAR                                        
156300       ELSE                                                               
156400         MOVE I-TAB-ADLAGOMR(I-TAB-IX) TO W-SPAR-ADLAGOMR                 
156500                                          W-JFR-ADLAGOMR                  
156600         MOVE SPACE                    TO W-6006-ADINLOMR-PAR             
156700         MOVE W-SPAR-ADLAGOMR          TO W-6006-ADINLOMR-PAR             
156800         PERFORM IMS-GU-PLAA11-PAR                                        
156900       END-IF                                                             
157000     END-PERFORM                                                          
157100     .                                                                    
157200     EJECT                                                                
157300 GC-REG-LISTNR-RADNR-DAT-REPL SECTION.                                    
157400                                                                          
157500     IF NYCKEL-KOLLI                                                      
157600       CONTINUE                                                           
157700     ELSE                                                                 
157800       PERFORM S20-INIT-6199                                              
157900     END-IF                                                               
158000     MOVE +0 TO I-TAB-IX                                                  
158100                W-KVRADER                                                 
158200     PERFORM UNTIL I-TAB-IX NOT < I-TAB-IX-MAX                            
158300       ADD   1 TO I-TAB-IX                                                
158400       MOVE I-TAB-ADLAGOMR(I-TAB-IX)  TO W-SPAR-ADLAGOMR                  
158500                                         W-JFR-ADLAGOMR                   
158600       MOVE I-TAB-TORG(I-TAB-IX)      TO W-SPAR-TORG                      
158700       PERFORM IMS-GHU-LOPA-LOPA11                                        
158800       IF SEGMENT-FINNS                                                   
158900         MOVE 6018-IDILIST  TO  W-D-MIN-IDILIST                           
159000                                W-D-MAX-IDILIST                           
159100         PERFORM IMS-GU-INLA11-D                                          
159200         PERFORM UNTIL SEGMENT-SAKNAS                                     
159300           ADD 1 TO W-D-MIN-IDILIST                                       
159400                    W-D-MAX-IDILIST                                       
159500           IF W-D-MIN-IDILIST > +0                                        
159600             PERFORM IMS-GU-INLA11-D                                      
159700           END-IF                                                         
159800         END-PERFORM                                                      
159900         MOVE W-D-MIN-IDILIST TO W-IDILIST                                
160000                               6018-IDILIST                               
160100         ADD  1             TO 6018-IDILIST                               
160200         PERFORM IMS-REPL-LOPA-LOPA11                                     
160300                                                                          
160400         PERFORM GCA-FYLL-LISTA                                           
160500                                                                          
160600       END-IF                                                             
160700     END-PERFORM                                                          
160800     .                                                                    
160900     EJECT                                                                
161000 GCA-FYLL-LISTA SECTION.                                                  
161100                                                                          
161200     PERFORM UNTIL I-TAB-ADLAGOMR(I-TAB-IX) NOT = W-JFR-ADLAGOMR          
161300             OR    I-TAB-TORG(I-TAB-IX)     NOT = W-SPAR-TORG             
161400       MOVE W-IDILIST TO I-TAB-IDILIST(I-TAB-IX)                          
161500       ADD 1 TO W-KVRADER                                                 
161600       MOVE W-KVRADER    TO I-TAB-IDILIRAD(I-TAB-IX)                      
161700                                                                          
161800       PERFORM GCAA-REPL-MOT-W6D121                                       
161900                                                                          
162000       ADD 1 TO I-TAB-IX                                                  
162100     END-PERFORM                                                          
162200     SUBTRACT 1 FROM I-TAB-IX                                             
162300                                                                          
162400     PERFORM GCAB-SKAPA-6199-MID                                          
162500                                                                          
162600     MOVE  0  TO W-KVRADER                                                
162700     .                                                                    
162800     EJECT                                                                
162900 GCAA-REPL-MOT-W6D121 SECTION.                                            
163000                                                                          
163100     MOVE I-TAB-IDLEVNR(I-TAB-IX)  TO W-W6D101-IDLEVNR                    
163200     MOVE I-TAB-IDFS(I-TAB-IX)     TO W-W6D101-IDFS                       
163300     MOVE I-TAB-TIAVIDAT(I-TAB-IX) TO W-W6D101-TIAVIDAT                   
163400     MOVE I-TAB-IDRADNR-INL(I-TAB-IX) TO W-W6D111-IDRADNR-INL             
163500     MOVE I-TAB-IDRADNR(I-TAB-IX)  TO W-W6D121-IDRADNR                    
163600     PERFORM IMS-GHU-INLA21                                               
163700     IF SEGMENT-FINNS                                                     
163800       MOVE I-TAB-IDILIRAD(I-TAB-IX) TO RAD-IDILIRAD                      
163900       MOVE I-TAB-IDILIST(I-TAB-IX)  TO RAD-IDILIST                       
164000       MOVE DAGENS-DATUM             TO RAD-TIUPPDAT                      
164100       IF VCOM-MID                                                        
164200         MOVE '10  ' TO RAD-ADINLOMR                                      
164300         MOVE SPACE  TO RAD-ADINLOMR-NXT                                  
164400       END-IF                                                             
164500       PERFORM IMS-REPL-INLA21                                            
164600     ELSE                                                                 
164700       MOVE 'FEL I I-LISTE TABELL' TO FELTEXT                             
164800       CALL FELLOG                                                        
164900     END-IF                                                               
165000     .                                                                    
165100     EJECT                                                                
165200 GCAB-SKAPA-6199-MID SECTION.                                             
165300                                                                          
165400     ADD   +1               TO  6199-IX                                   
165500                                                                          
165600     MOVE TAB-IDLEVNR-KOLLI                                               
165700                            TO MOD6199-MID-IDLEVNR-KOLLI(6199-IX)         
165800     MOVE TAB-IDOKOLLI                                                    
165900                            TO MOD6199-MID-IDOKOLLI(6199-IX)              
166000     MOVE TAB-IDINLVGN      TO MOD6199-MID-IDINLVGN(6199-IX)              
166100     MOVE TAB-ADINLOMR      TO MOD6199-MID-ADINLOMR(6199-IX)              
166200                                                                          
166300     IF W-SPAR-TORG = '0000'                                              
166400       MOVE SPACE                                                         
166500                            TO MOD6199-MID-ADINLOMR-TORG(6199-IX)         
166600     ELSE                                                                 
166700       MOVE W-SPAR-TORG                                                   
166800                            TO MOD6199-MID-ADINLOMR-TORG(6199-IX)         
166900     END-IF                                                               
167000                                                                          
167100     MOVE W-SPAR-ADLAGOMR                                                 
167200                            TO MOD6199-MID-ADLAGOMR(6199-IX)              
167300     MOVE I-TAB-IDILIST(I-TAB-IX)                                         
167400                            TO MOD6199-MID-IDILIST(6199-IX)               
167500     MOVE I-TAB-IDILIRAD(I-TAB-IX)                                        
167600                            TO MOD6199-MID-KVRADER(6199-IX)               
167700                                                                          
167800     ADD 1 TO W-KVPOST-I-LISTA                                            
167900                                                                          
168000     IF 6199-IX = 6199-MAX-IX                                             
168100       PERFORM S21-P-TO-P-6199                                            
168200       PERFORM S20-INIT-6199                                              
168300     END-IF                                                               
168400     .                                                                    
168500     EJECT                                                                
168600 H-LAEGG-UPP-P-LISTA SECTION.                                             
168700                                                                          
168800     PERFORM HA-SORT-P-LIST                                               
168900     PERFORM HB-REG-LISTNR-RADNR-DAT-REPL                                 
169000     MOVE ZEROS TO P-TAB-IX                                               
169100                   P-TAB-IX-MAX                                           
169200     .                                                                    
169300     EJECT                                                                
169400 HA-SORT-P-LIST SECTION.                                                  
169500                                                                          
169600     MOVE +50            TO  STEGLNGD                                     
169700     MOVE P-TAB-IX-MAX   TO  ANTAL                                        
169800     MOVE +19            TO  NYCKELLNGD                                   
169900                                                                          
170000     CALL WINTSOR        USING PLA-TAB STEGLNGD ANTAL                     
170100                               P-TAB-SORTNYCKEL(1) NYCKELLNGD             
170200     .                                                                    
170300     EJECT                                                                
170400 HB-REG-LISTNR-RADNR-DAT-REPL SECTION.                                    
170500                                                                          
170600     IF NYCKEL-KOLLI                                                      
170700        CONTINUE                                                          
170800     ELSE                                                                 
170900       PERFORM S22-INIT-619A                                              
171000     END-IF                                                               
171100     MOVE +0 TO P-TAB-IX                                                  
171200     MOVE +0 TO W-KVRADER                                                 
171300     PERFORM UNTIL P-TAB-IX = P-TAB-IX-MAX                                
171400       ADD  1 TO P-TAB-IX                                                 
171500       MOVE P-TAB-IDARTNR(P-TAB-IX) TO W-SPAR-IDARTNR                     
171600       PERFORM IMS-GHU-LOPA-LOPA11                                        
171700       IF SEGMENT-FINNS                                                   
171800         MOVE 6018-IDILIST  TO  W-D-MIN-IDILIST                           
171900                                W-D-MAX-IDILIST                           
172000         PERFORM IMS-GU-INLA11-D                                          
172100         PERFORM UNTIL SEGMENT-SAKNAS                                     
172200           ADD 1 TO W-D-MIN-IDILIST                                       
172300                    W-D-MAX-IDILIST                                       
172400           IF W-D-MIN-IDILIST > +0                                        
172500             PERFORM IMS-GU-INLA11-D                                      
172600           END-IF                                                         
172700         END-PERFORM                                                      
172800         MOVE W-D-MIN-IDILIST TO W-IDILIST                                
172900                                 6018-IDILIST                             
173000         ADD  1               TO 6018-IDILIST                             
173100         PERFORM IMS-REPL-LOPA-LOPA11                                     
173200                                                                          
173300         PERFORM HBA-FYLL-LISTA                                           
173400                                                                          
173500       END-IF                                                             
173600     END-PERFORM                                                          
173700     .                                                                    
173800     EJECT                                                                
173900 HBA-FYLL-LISTA SECTION.                                                  
174000                                                                          
174100     PERFORM UNTIL P-TAB-IDARTNR(P-TAB-IX) NOT = W-SPAR-IDARTNR           
174200       MOVE W-IDILIST TO P-TAB-IDILIST(P-TAB-IX)                          
174300       ADD 1 TO W-KVRADER                                                 
174400       MOVE W-KVRADER    TO P-TAB-IDILIRAD(P-TAB-IX)                      
174500                                                                          
174600       PERFORM HBAA-REPL-MOT-W6D121                                       
174700                                                                          
174800       ADD 1 TO P-TAB-IX                                                  
174900     END-PERFORM                                                          
175000     SUBTRACT 1 FROM P-TAB-IX                                             
175100                                                                          
175200     PERFORM HBAB-SKAPA-619A-MID                                          
175300                                                                          
175400     MOVE  0  TO W-KVRADER                                                
175500     .                                                                    
175600     EJECT                                                                
175700 HBAA-REPL-MOT-W6D121 SECTION.                                            
175800                                                                          
175900     MOVE P-TAB-IDLEVNR(P-TAB-IX)  TO W-W6D101-IDLEVNR                    
176000     MOVE P-TAB-IDFS(P-TAB-IX)     TO W-W6D101-IDFS                       
176100     MOVE P-TAB-TIAVIDAT(P-TAB-IX) TO W-W6D101-TIAVIDAT                   
176200     MOVE P-TAB-IDRADNR-INL(P-TAB-IX) TO W-W6D111-IDRADNR-INL             
176300     MOVE P-TAB-IDRADNR(P-TAB-IX)  TO W-W6D121-IDRADNR                    
176400     PERFORM IMS-GHU-INLA21                                               
176500     IF SEGMENT-FINNS                                                     
176600       MOVE P-TAB-IDILIRAD(P-TAB-IX) TO RAD-IDILIRAD                      
176700       MOVE P-TAB-IDILIST(P-TAB-IX)  TO RAD-IDILIST                       
176800       MOVE DAGENS-DATUM             TO RAD-TIUPPDAT                      
176900       IF VCOM-MID                                                        
177000         MOVE '10  ' TO RAD-ADINLOMR                                      
177100         MOVE SPACE  TO RAD-ADINLOMR-NXT                                  
177200       END-IF                                                             
177300       PERFORM IMS-REPL-INLA21                                            
177400     ELSE                                                                 
177500       MOVE 'FEL I P-LISTE TABELL' TO FELTEXT                             
177600       CALL FELLOG                                                        
177700     END-IF                                                               
177800     .                                                                    
177900     EJECT                                                                
178000 HBAB-SKAPA-619A-MID SECTION.                                             
178100                                                                          
178200     ADD   +1               TO  619A-IX                                   
178300                                                                          
178400     MOVE TAB-IDLEVNR-KOLLI TO MOD619A-MID-IDLEVNR-KOLLI(619A-IX)         
178500     MOVE TAB-IDOKOLLI      TO MOD619A-MID-IDOKOLLI(619A-IX)              
178600     MOVE TAB-IDINLVGN      TO MOD619A-MID-IDINLVGN(619A-IX)              
178700     MOVE TAB-ADINLOMR      TO MOD619A-MID-ADINLOMR(619A-IX)              
178800     MOVE P-TAB-IDARTNR(P-TAB-IX)                                         
178900                            TO MOD619A-MID-IDARTNR(619A-IX)               
179000     MOVE P-TAB-IDILIST(P-TAB-IX)                                         
179100                            TO MOD619A-MID-IDILIST(619A-IX)               
179200     MOVE P-TAB-IDILIRAD(P-TAB-IX)                                        
179300                            TO MOD619A-MID-KVRADER(619A-IX)               
179400                                                                          
179500     ADD 1 TO W-KVPOST-P-LISTA                                            
179600                                                                          
179700     IF 619A-IX = 619A-MAX-IX                                             
179800       PERFORM S23-P-TO-P-619A                                            
179900       PERFORM S22-INIT-619A                                              
180000     END-IF                                                               
180100     .                                                                    
180200     EJECT                                                                
180300 I-LAEGG-UPP-R-LISTA SECTION.                                             
180400                                                                          
180500     PERFORM IA-SORT-R-LIST                                               
180600                                                                          
180700     MOVE  +1  TO R-TAB-IX                                                
180800     PERFORM UNTIL R-TAB-IX > R-TAB-IX-MAX                                
180900       MOVE TAB-IDPRTLST                TO RETL-IDPRTLST                  
181000       MOVE IDPGM                       TO RETL-IDPGM                     
181100       MOVE TAB-IDLEVNR-KOLLI           TO RETL-IDLEVNR-KOLLI             
181200       MOVE TAB-IDOKOLLI                TO RETL-IDOKOLLI                  
181300       MOVE TAB-IDINLVGN                TO RETL-IDINLVGN                  
181400       MOVE TAB-ADINLOMR                TO RETL-ADINLOMR                  
181500       MOVE R-TAB-ADINLOMR-FB(R-TAB-IX) TO RETL-ADINLOMR-NXT              
181600       MOVE R-TAB-IDRADNR-INL(R-TAB-IX) TO RETL-IDRADNR-INL               
181700       MOVE R-TAB-IDLEVNR(R-TAB-IX)     TO RETL-IDLEVNR                   
181800       MOVE R-TAB-IDFS(R-TAB-IX)        TO RETL-IDFS                      
181900       MOVE R-TAB-TIAVIDAT(R-TAB-IX)    TO RETL-TIAVIDAT                  
182000       MOVE R-TAB-IDRADNR(R-TAB-IX)     TO RETL-IDRADNR                   
182100       IF R-TAB-IX  =  R-TAB-IX-MAX                                       
182200         MOVE JA                        TO RETL-FLSLUT                    
182300       ELSE                                                               
182400         MOVE NEJ                       TO RETL-FLSLUT                    
182500       END-IF                                                             
182600       CALL W611RETL USING RETL-W611RETL  RETL-PRT-PCB                    
182700                           RETL-6191-PCB  RETL-INLA-PCB                   
182800       ADD    1  TO R-TAB-IX                                              
182900     END-PERFORM                                                          
183000                                                                          
183100     MOVE INF-PRINTING-STARTED TO MED-IDMFSINF                            
183200     CALL WMEDKONV USING MED-WMEDAREA                                     
183300     MOVE MED-TEMFSINF         TO MOD-TEMFSINF                            
183400     MOVE W-BEPRTLST           TO MOD-TEMFSFEL(1:25)                      
183500     MOVE ' R '                TO MOD-TEMFSFEL(26:3)                      
183600     MOVE ZEROS  TO R-TAB-IX                                              
183700                    R-TAB-IX-MAX                                          
183800     .                                                                    
183900     EJECT                                                                
184000 IA-SORT-R-LIST SECTION.                                                  
184100                                                                          
184200     MOVE +49            TO  STEGLNGD                                     
184300     MOVE R-TAB-IX-MAX   TO  ANTAL                                        
184400     MOVE +23            TO  NYCKELLNGD                                   
184500                                                                          
184600     CALL WINTSOR        USING PLA-TAB STEGLNGD ANTAL                     
184700                               R-TAB-SORTNYCKEL(1) NYCKELLNGD             
184800     .                                                                    
184900     SKIP3                                                                
185000 J-SORT-D-TAB SECTION.                                                    
185100                                                                          
185200     MOVE +57            TO  STEGLNGD                                     
185300     MOVE D-TAB-IX-MAX   TO  ANTAL                                        
185400     MOVE +14            TO  NYCKELLNGD                                   
185500                                                                          
185600     CALL WINTSOR        USING PLA-TAB STEGLNGD ANTAL                     
185700                               D-TAB-SORTNYCKEL(1) NYCKELLNGD             
185800     .                                                                    
185900     EJECT                                                                
186000 K-BEHANDLA-DIVKOLLI SECTION.                                             
186100                                                                          
186200     MOVE D-TAB-IDLEVNR-KOLLI(D-TAB-IX)                                   
186300                          TO TAB-IDLEVNR-KOLLI                            
186400     MOVE D-TAB-IDOKOLLI(D-TAB-IX)                                        
186500                          TO TAB-IDOKOLLI                                 
186600     PERFORM UNTIL D-TAB-IDLEVNR-KOLLI(D-TAB-IX) NOT =                    
186700                     TAB-IDLEVNR-KOLLI           AND                      
186800                   D-TAB-IDOKOLLI(D-TAB-IX)      NOT =                    
186900                     TAB-IDOKOLLI                                         
187000       IF D-TAB-RETL(D-TAB-IX) = JA                                       
187100                                                                          
187200          PERFORM S11-FLYTTA-D-TAB-TILL-R-TAB                             
187300                                                                          
187400       ELSE                                                               
187500          IF D-TAB-ADPLATS(D-TAB-IX) = ZERO                               
187600***   REGLER FÖR PLATSSÄTTARLISTAN KOMMER                                 
187700             PERFORM S12-FLYTTA-D-TAB-TILL-P-TAB                          
187800                                                                          
187900          ELSE                                                            
188000                                                                          
188100             PERFORM S13-FLYTTA-D-TAB-TILL-I-TAB                          
188200                                                                          
188300          END-IF                                                          
188400       END-IF                                                             
188500       ADD 1 TO D-TAB-IX                                                  
188600     END-PERFORM                                                          
188700     .                                                                    
188800     EJECT                                                                
188900 L-BEHANDLA-KOLLI SECTION.                                                
189000                                                                          
189100     IF W-IDLEVNR-KOLLI(RAD-IX) = SPACE                                   
189200       PERFORM UNTIL                                                      
189300          RAD-IX > RAD-MAX-IX       OR                                    
189400          (W-IDLEVNR-KOLLI(RAD-IX) NOT = SPACE     AND                    
189500          W-IDOKOLLI(RAD-IX) NOT = ALL ZEROS)                             
189600          ADD 1 TO RAD-IX                                                 
189700       END-PERFORM                                                        
189800     END-IF                                                               
189900     IF RAD-IX NOT > RAD-MAX-IX                                           
190000       MOVE W-IDLEVNR-KOLLI(RAD-IX)  TO                                   
190100                                   W-C-MIN-IDLEVNR-KOLLI                  
190200                                   W-C-MAX-IDLEVNR-KOLLI                  
190300                                   TAB-IDLEVNR-KOLLI                      
190400           MOVE W-IDOKOLLI(RAD-IX)      TO                                
190500                                   W-C-MIN-IDOKOLLI                       
190600                                   W-C-MAX-IDOKOLLI                       
190700                                   TAB-IDOKOLLI                           
190800       PERFORM IMS-GU-INLD01                                              
190900       IF SEGMENT-FINNS                                                   
191000          PERFORM UNTIL SEGMENT-SAKNAS                                    
191100            MOVE SEQC-IDRADNR-INL TO W-W6D111-IDRADNR-INL                 
191200            MOVE SEQC-IDLEVNR  TO  W-W6D101-IDLEVNR                       
191300                                   W-IDLEVNR                              
191400            MOVE SEQC-IDFS     TO  W-W6D101-IDFS                          
191500                                   W-IDFS                                 
191600            MOVE SEQC-TIAVIDAT TO  W-W6D101-TIAVIDAT                      
191700                                   W-TIAVIDAT                             
191800            MOVE SEQC-IDRADNR  TO  W-W6D121-IDRADNR                       
191900            MOVE SEQC-IDDC     TO  WS-IDDC                                
192000            IF CDC-SE                                                     
192100              PERFORM IMS-GU-INLA11                                       
192200              PERFORM IMS-GU-INLA21                                       
192300              IF (RAD-KDINLSTA = 'SAK' OR 'FPK' OR '   ') AND             
192400                 (RAD-IDILIST  =  ZERO)                                   
192500                MOVE JA  TO KOLLIRAD-SW                                   
192600                PERFORM S30-PRIM-SEK-KOLL                                 
192700                IF ART-FLKVAFEL = JA  OR                                  
192800                   ART-FLKVAKAR = JA  OR                                  
192900                   RAD-FLSATS   = JA  OR                                  
193000                   ART-VKART      = ZERO OR                               
193100                   ART-VLARTNTO   = ZERO OR                               
193200                   ART-KDARTURS   = SPACE OR                              
193300                   WS-FL-PRIM-SEK = JA                                    
193400                                                                          
193500                   PERFORM S14-FLYTTA-TILL-R-TAB                          
193600                                                                          
193700                ELSE                                                      
193800                   IF ART-ADLAGOMR = ZERO OR                              
193900                      ART-ADPLATS = ZERO                                  
194000***   REGLER FÖR PLATSSÄTTARLISTAN KOMMER                                 
194100                                                                          
194200                      PERFORM S15-FLYTTA-TILL-P-TAB                       
194300                                                                          
194400                   ELSE                                                   
194500                                                                          
194600                      PERFORM S16-FLYTTA-TILL-I-TAB                       
194700                                                                          
194800                   END-IF                                                 
194900                END-IF                                                    
195000              END-IF                                                      
195100            END-IF                                                        
195200            PERFORM IMS-GN-INLD01                                         
195300          END-PERFORM                                                     
195400       END-IF                                                             
195500     END-IF                                                               
195600     .                                                                    
195700     EJECT                                                                
195800 S01-ERR-NO-PRINTING-FIELD  SECTION.                                      
195900                                                                          
196000     MOVE ERR-NO-PRINTING TO MED-IDMFSFEL                                 
196100     CALL WMEDKONV USING MED-WMEDAREA                                     
196200     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
196300     MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT-IN                        
196400     PERFORM MFS-RENSA-FAELT-UT                                           
196500     .                                                                    
196600     SKIP3                                                                
196700 S02-INF-PF4-FOR-PRINTING   SECTION.                                      
196800                                                                          
196900     MOVE +1 TO RAD-IX                                                    
197000     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
197100       MOVE SPACE        TO MOD-TEMFSMED(RAD-IX)                          
197200       ADD 1 TO RAD-IX                                                    
197300     END-PERFORM                                                          
197400     MOVE INF-PF4-FOR-PRINTING TO MED-IDMFSINF                            
197500     CALL WMEDKONV USING MED-WMEDAREA                                     
197600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
197700     PERFORM MFS-ROER-EJ-FAELT-IN                                         
197800     MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT-IN                        
197900     PERFORM MFS-ROER-EJ-FAELT-UT                                         
198000     PERFORM MFS-LAES-IN-IGEN                                             
198100     .                                                                    
198200     EJECT                                                                
198300 S03-ERR-CORR-HILITE-FLDS   SECTION.                                      
198400                                                                          
198500     MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)          
198600     MOVE MFS-NUM-FAELT-FEL    TO MOD-IDOKOLLI-ATTR(RAD-IX)               
198700     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
198800     CALL WMEDKONV USING MED-WMEDAREA                                     
198900     MOVE MED-MFSFEL(5:20)     TO MOD-TEMFSMED(RAD-IX)                    
199000     MOVE NEJ                  TO INDATA-SW                               
199100     .                                                                    
199200     SKIP3                                                                
199300 S04-ERR-NOT-ON-REGISTER    SECTION.                                      
199400                                                                          
199500     MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)           
199600     MOVE MFS-NUM-FAELT-FEL   TO MOD-IDOKOLLI-ATTR(RAD-IX)                
199700     MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                             
199800     CALL WMEDKONV USING MED-WMEDAREA                                     
199900     MOVE MED-MFSFEL(5:20) TO MOD-TEMFSMED(RAD-IX)                        
200000     MOVE NEJ              TO INDATA-SW                                   
200100     .                                                                    
200200     SKIP3                                                                
200300 S05-INF-ALREADY-PRINTED SECTION.                                         
200400                                                                          
200500     MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)           
200600     MOVE MFS-NUM-FAELT-FEL   TO MOD-IDOKOLLI-ATTR(RAD-IX)                
200700     MOVE INF-ALREADY-PRINTED TO MED-IDMFSFEL                             
200800     CALL WMEDKONV USING MED-WMEDAREA                                     
200900     MOVE MED-MFSFEL(5:20)    TO MOD-TEMFSMED(RAD-IX)                     
201000     MOVE '. IL: '            TO MOD-TEMFSMED(RAD-IX)(10:6)               
201100     MOVE RAD-IDILIST         TO MOD-TEMFSMED(RAD-IX)(16:5)               
201200     MOVE NEJ                 TO INDATA-SW                                
201300     .                                                                    
201400     EJECT                                                                
201500 S06-RENSA-TAB-O-INDEX SECTION.                                           
201600                                                                          
201700     MOVE INL-TAB-TOM  TO  INL-TAB                                        
201800     MOVE PLA-TAB-TOM  TO  PLA-TAB                                        
201900     MOVE RET-TAB-TOM  TO  RET-TAB                                        
202000     MOVE ZEROS        TO  I-TAB-IX-MAX                                   
202100     MOVE ZEROS        TO  P-TAB-IX-MAX                                   
202200     MOVE ZEROS        TO  R-TAB-IX-MAX                                   
202300     .                                                                    
202400     SKIP3                                                                
202500 S07-SAETT-MAX-INDEX SECTION.                                             
202600                                                                          
202700     MOVE R-TAB-IX  TO  R-TAB-IX-MAX                                      
202800     MOVE P-TAB-IX  TO  P-TAB-IX-MAX                                      
202900     MOVE I-TAB-IX  TO  I-TAB-IX-MAX                                      
203000     .                                                                    
203100     EJECT                                                                
203200 S08-INIT-I-LISTA SECTION.                                                
203300                                                                          
203400     MOVE  JA TO SW-1A-6199                                               
203500     MOVE  0 TO W-KVPOST-I-LISTA                                          
203600     MOVE ZEROS TO 6199-IX                                                
203700     ACCEPT DAGENS-DATUM FROM DATE                                        
203800     .                                                                    
203900     SKIP3                                                                
204000 S09-INIT-P-LISTA SECTION.                                                
204100                                                                          
204200     MOVE  JA TO SW-1A-619A                                               
204300     MOVE  0 TO W-KVPOST-P-LISTA                                          
204400     MOVE ZEROS TO 619A-IX                                                
204500     ACCEPT DAGENS-DATUM FROM DATE                                        
204600     .                                                                    
204700     EJECT                                                                
204800 S10-FLYTTA-TILL-D-TAB SECTION.                                           
204900                                                                          
205000     IF D-TAB-IX < D-TAB-MAX-IX                                           
205100       ADD 1                    TO   D-TAB-IX                             
205200       MOVE ART-ADLAGOMR        TO   D-TAB-ADLAGOMR(D-TAB-IX)             
205300       MOVE '0000'              TO   D-TAB-TORG(D-TAB-IX)                 
205400       MOVE ART-ADGANG          TO   D-TAB-ADGANG(D-TAB-IX)               
205500       MOVE ART-ADPLATS         TO   D-TAB-ADPLATS(D-TAB-IX)              
205600       MOVE ART-IDARTNR         TO   D-TAB-IDARTNR(D-TAB-IX)              
205700       MOVE ART-IDRADNR-INL     TO   D-TAB-IDRADNR-INL(D-TAB-IX)          
205800       MOVE RAD-IDLEVNR-KOLLI   TO   D-TAB-IDLEVNR-KOLLI(D-TAB-IX)        
205900       MOVE RAD-IDOKOLLI        TO   D-TAB-IDOKOLLI(D-TAB-IX)             
206000       MOVE W-IDLEVNR           TO   D-TAB-IDLEVNR(D-TAB-IX)              
206100       MOVE W-IDFS              TO   D-TAB-IDFS(D-TAB-IX)                 
206200       MOVE W-TIAVIDAT          TO   D-TAB-TIAVIDAT(D-TAB-IX)             
206300       MOVE RAD-IDRADNR         TO   D-TAB-IDRADNR(D-TAB-IX)              
206400       IF ART-FLKVAFEL =  JA  OR                                          
206500          ART-FLKVAKAR =  JA  OR                                          
206600          RAD-FLSATS   =  JA                                              
206700          MOVE JA      TO D-TAB-RETL(D-TAB-IX)                            
206800       END-IF                                                             
206900     ELSE                                                                 
207000       MOVE 'DIV-KOLLI-TABELLEN ÖVERSKRIDEN SEKT. S10' TO FELTEXT         
207100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
207200     END-IF                                                               
207300     .                                                                    
207400     EJECT                                                                
207500 S11-FLYTTA-D-TAB-TILL-R-TAB SECTION.                                     
207600                                                                          
207700     IF R-TAB-IX < R-TAB-MAX-IX                                           
207800       ADD  1                  TO R-TAB-IX                                
207900       MOVE WC-CDC-SE                TO STYR-IDDC                         
208000       MOVE D-TAB-IDARTNR(D-TAB-IX)  TO STYR-IDARTNR                      
208100       MOVE D-TAB-IDFKNGRP(D-TAB-IX) TO STYR-IDFKNGRP                     
208200       MOVE D-TAB-IDLEVNR(D-TAB-IX)  TO STYR-IDLEVNR                      
208300       MOVE ZERO                     TO STYR-BEFT                         
208400       CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                    
208500                                         STYR-PLAA-PCB                    
208600       IF STYR-KDSVAR-OK                                                  
208700          MOVE STYR-ADINLOMR-FB                 TO                        
208800                                    R-TAB-ADINLOMR-FB(R-TAB-IX)           
208900       END-IF                                                             
209000       MOVE D-TAB-IDARTNR(D-TAB-IX)             TO                        
209100                                    R-TAB-IDARTNR(R-TAB-IX)               
209200       MOVE D-TAB-IDRADNR-INL(D-TAB-IX)         TO                        
209300                                    R-TAB-IDRADNR-INL(R-TAB-IX)           
209400       MOVE D-TAB-IDLEVNR-KOLLI(D-TAB-IX)       TO                        
209500                                    R-TAB-IDLEVNR-KOLLI(R-TAB-IX)         
209600       MOVE D-TAB-IDOKOLLI(D-TAB-IX)            TO                        
209700                                    R-TAB-IDOKOLLI(R-TAB-IX)              
209800       MOVE D-TAB-IDLEVNR(D-TAB-IX)             TO                        
209900                                    R-TAB-IDLEVNR(R-TAB-IX)               
210000       MOVE D-TAB-IDFS(D-TAB-IX)                TO                        
210100                                    R-TAB-IDFS(R-TAB-IX)                  
210200       MOVE D-TAB-TIAVIDAT(D-TAB-IX)            TO                        
210300                                    R-TAB-TIAVIDAT(R-TAB-IX)              
210400       MOVE D-TAB-IDRADNR(D-TAB-IX)             TO                        
210500                                    R-TAB-IDRADNR(R-TAB-IX)               
210600       MOVE D-TAB-IDFKNGRP(D-TAB-IX)            TO                        
210700                                    R-TAB-IDFKNGRP(R-TAB-IX)              
210800     ELSE                                                                 
210900       MOVE 'RETUR-TABELLEN ÖVERSKRIDEN SEKT. S11' TO FELTEXT             
211000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
211100     END-IF                                                               
211200     .                                                                    
211300     EJECT                                                                
211400 S12-FLYTTA-D-TAB-TILL-P-TAB SECTION.                                     
211500                                                                          
211600     IF P-TAB-IX < P-TAB-MAX-IX                                           
211700       ADD  1    TO P-TAB-IX                                              
211800       MOVE D-TAB-IDARTNR(D-TAB-IX)       TO                              
211900                                   P-TAB-IDARTNR(P-TAB-IX)                
212000       MOVE D-TAB-IDRADNR-INL(D-TAB-IX)   TO                              
212100                                   P-TAB-IDRADNR-INL(P-TAB-IX)            
212200       MOVE D-TAB-IDLEVNR-KOLLI(D-TAB-IX) TO                              
212300                                   P-TAB-IDLEVNR-KOLLI(P-TAB-IX)          
212400       MOVE D-TAB-IDOKOLLI(D-TAB-IX)      TO                              
212500                                   P-TAB-IDOKOLLI(P-TAB-IX)               
212600       MOVE D-TAB-IDLEVNR(D-TAB-IX)       TO                              
212700                                   P-TAB-IDLEVNR(P-TAB-IX)                
212800       MOVE D-TAB-IDFS(D-TAB-IX)          TO                              
212900                                   P-TAB-IDFS(P-TAB-IX)                   
213000       MOVE D-TAB-TIAVIDAT(D-TAB-IX)      TO                              
213100                                   P-TAB-TIAVIDAT(P-TAB-IX)               
213200       MOVE D-TAB-IDRADNR(D-TAB-IX)       TO                              
213300                                   P-TAB-IDRADNR(P-TAB-IX)                
213400     ELSE                                                                 
213500       MOVE 'PLACERINGS-TABELLEN ÖVERSKRIDEN SEKT. S12' TO FELTEXT        
213600       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
213700     END-IF                                                               
213800     .                                                                    
213900     EJECT                                                                
214000 S13-FLYTTA-D-TAB-TILL-I-TAB SECTION.                                     
214100                                                                          
214200     IF I-TAB-IX < I-TAB-MAX-IX                                           
214300       ADD  1    TO I-TAB-IX                                              
214400       MOVE D-TAB-ADLAGOMR(D-TAB-IX)      TO                              
214500                                    I-TAB-ADLAGOMR(I-TAB-IX)              
214600       MOVE '0000'               TO I-TAB-TORG(I-TAB-IX)                  
214700       MOVE D-TAB-ADGANG(D-TAB-IX)        TO                              
214800                                    I-TAB-ADGANG(I-TAB-IX)                
214900       MOVE D-TAB-ADPLATS(D-TAB-IX)       TO                              
215000                                    I-TAB-ADPLATS(I-TAB-IX)               
215100       MOVE D-TAB-IDARTNR(D-TAB-IX)       TO                              
215200                                    I-TAB-IDARTNR(I-TAB-IX)               
215300       MOVE D-TAB-IDRADNR-INL(D-TAB-IX)   TO                              
215400                                    I-TAB-IDRADNR-INL(I-TAB-IX)           
215500       MOVE D-TAB-IDLEVNR-KOLLI(D-TAB-IX) TO                              
215600                                    I-TAB-IDLEVNR-KOLLI(I-TAB-IX)         
215700       MOVE D-TAB-IDOKOLLI(D-TAB-IX)      TO                              
215800                                    I-TAB-IDOKOLLI(I-TAB-IX)              
215900       MOVE D-TAB-IDLEVNR(D-TAB-IX)       TO                              
216000                                    I-TAB-IDLEVNR(I-TAB-IX)               
216100       MOVE D-TAB-IDFS(D-TAB-IX)          TO                              
216200                                    I-TAB-IDFS(I-TAB-IX)                  
216300       MOVE D-TAB-TIAVIDAT(D-TAB-IX)      TO                              
216400                                    I-TAB-TIAVIDAT(I-TAB-IX)              
216500       MOVE D-TAB-IDRADNR(D-TAB-IX)       TO                              
216600                                    I-TAB-IDRADNR(I-TAB-IX)               
216700     ELSE                                                                 
216800       MOVE 'INLEVERANS-TABELLEN ÖVERSKRIDEN SEKT. S13' TO FELTEXT        
216900       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
217000     END-IF                                                               
217100     .                                                                    
217200     EJECT                                                                
217300 S14-FLYTTA-TILL-R-TAB SECTION.                                           
217400                                                                          
217500     IF R-TAB-IX < R-TAB-MAX-IX                                           
217600       ADD  1                    TO R-TAB-IX                              
217700       MOVE ART-IDDC             TO STYR-IDDC                             
217800       MOVE ART-IDARTNR          TO STYR-IDARTNR                          
217900       MOVE ART-IDFKNGRP         TO STYR-IDFKNGRP                         
218000       MOVE W-IDLEVNR            TO STYR-IDLEVNR                          
218100       MOVE ZERO                 TO STYR-BEFT                             
218200       CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                    
218300                                         STYR-PLAA-PCB                    
218400       IF STYR-KDSVAR-OK                                                  
218500          MOVE STYR-ADINLOMR-FB TO                                        
218600                               R-TAB-ADINLOMR-FB(R-TAB-IX)                
218700       END-IF                                                             
218800       MOVE ART-IDARTNR          TO R-TAB-IDARTNR(R-TAB-IX)               
218900       MOVE ART-IDRADNR-INL      TO R-TAB-IDRADNR-INL(R-TAB-IX)           
219000       MOVE RAD-IDLEVNR-KOLLI    TO R-TAB-IDLEVNR-KOLLI(R-TAB-IX)         
219100       MOVE RAD-IDOKOLLI         TO R-TAB-IDOKOLLI(R-TAB-IX)              
219200       MOVE W-IDLEVNR            TO R-TAB-IDLEVNR(R-TAB-IX)               
219300       MOVE W-IDFS               TO R-TAB-IDFS(R-TAB-IX)                  
219400       MOVE W-TIAVIDAT           TO R-TAB-TIAVIDAT(R-TAB-IX)              
219500       MOVE RAD-IDRADNR          TO R-TAB-IDRADNR(R-TAB-IX)               
219600       MOVE ART-IDFKNGRP         TO R-TAB-IDFKNGRP(R-TAB-IX)              
219700     ELSE                                                                 
219800       MOVE 'RETUR-TABELLEN ÖVERSKRIDEN SEKT. S14' TO FELTEXT             
219900       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
220000     END-IF                                                               
220100     .                                                                    
220200     EJECT                                                                
220300 S15-FLYTTA-TILL-P-TAB SECTION.                                           
220400                                                                          
220500     IF P-TAB-IX < P-TAB-MAX-IX                                           
220600       ADD  1                  TO P-TAB-IX                                
220700       MOVE ART-IDARTNR        TO P-TAB-IDARTNR(P-TAB-IX)                 
220800       MOVE ART-IDRADNR-INL    TO P-TAB-IDRADNR-INL(P-TAB-IX)             
220900       MOVE RAD-IDLEVNR-KOLLI  TO P-TAB-IDLEVNR-KOLLI(P-TAB-IX)           
221000       MOVE RAD-IDOKOLLI       TO P-TAB-IDOKOLLI(P-TAB-IX)                
221100       MOVE W-IDLEVNR          TO P-TAB-IDLEVNR(P-TAB-IX)                 
221200       MOVE W-IDFS             TO P-TAB-IDFS(P-TAB-IX)                    
221300       MOVE W-TIAVIDAT         TO P-TAB-TIAVIDAT(P-TAB-IX)                
221400       MOVE RAD-IDRADNR        TO P-TAB-IDRADNR(P-TAB-IX)                 
221500     ELSE                                                                 
221600       MOVE 'PLACERINGS-TABELLEN ÖVERSKRIDEN SEKT. S15' TO FELTEXT        
221700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
221800     END-IF                                                               
221900     .                                                                    
222000     EJECT                                                                
222100 S16-FLYTTA-TILL-I-TAB SECTION.                                           
222200                                                                          
222300     IF I-TAB-IX < I-TAB-MAX-IX                                           
222400       ADD  1                  TO I-TAB-IX                                
222500       MOVE ART-ADLAGOMR       TO I-TAB-ADLAGOMR(I-TAB-IX)                
222600       MOVE '0000'             TO I-TAB-TORG(I-TAB-IX)                    
222700       MOVE ART-ADGANG         TO I-TAB-ADGANG(I-TAB-IX)                  
222800       MOVE ART-ADPLATS        TO I-TAB-ADPLATS(I-TAB-IX)                 
222900       MOVE ART-IDARTNR        TO I-TAB-IDARTNR(I-TAB-IX)                 
223000       MOVE ART-IDRADNR-INL    TO I-TAB-IDRADNR-INL(I-TAB-IX)             
223100       MOVE RAD-IDLEVNR-KOLLI  TO I-TAB-IDLEVNR-KOLLI(I-TAB-IX)           
223200       MOVE RAD-IDOKOLLI       TO I-TAB-IDOKOLLI(I-TAB-IX)                
223300       MOVE W-IDLEVNR          TO I-TAB-IDLEVNR(I-TAB-IX)                 
223400       MOVE W-IDFS             TO I-TAB-IDFS(I-TAB-IX)                    
223500       MOVE W-TIAVIDAT         TO I-TAB-TIAVIDAT(I-TAB-IX)                
223600       MOVE RAD-IDRADNR        TO I-TAB-IDRADNR(I-TAB-IX)                 
223700     ELSE                                                                 
223800       MOVE 'INLÄGGNINGS-TABELLEN ÖVERSKRIDEN SEKT S16' TO FELTEXT        
223900       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
224000     END-IF                                                               
224100     .                                                                    
224200     EJECT                                                                
224300 S20-INIT-6199 SECTION.                                                   
224400                                                                          
224500     MOVE SPACE                TO  MOD6199-MID-W6I19901                   
224600     MOVE TAB-IDPRTLST         TO  MOD6199-MID-IDPRTLST                   
224700     MOVE IDPGM                TO  MOD6199-MID-IDPGM                      
224800     MOVE ZERO                 TO  W-KVPOST-I-LISTA                       
224900     .                                                                    
225000     SKIP2                                                                
225100 S21-P-TO-P-6199 SECTION.                                                 
225200                                                                          
225300     MOVE W-KVPOST-I-LISTA     TO  MOD6199-MID-KVPOST                     
225400     COMPUTE P-TO-P-MSG-KVLL   = LNG-P-TO-P-PREFIX                        
225500                               + 23 + (MOD6199-MID-KVPOST * 37)           
225600                                                                          
225700     MOVE LOW-VALUE            TO P-TO-P-MSG-KDZ1                         
225800     MOVE LOW-VALUE            TO P-TO-P-MSG-KDZ2                         
225900     MOVE 'W6T199X '           TO P-TO-P-MSG-KDTRANS                      
226000     MOVE '6141'               TO P-TO-P-MSG-IDTRANS                      
226100     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
226200                                                                          
226300     MOVE MOD6199-MID-W6I19901 TO P-TO-P-MSG-INDATA                       
226400                                                                          
226500     IF  SW-1A-6199 = JA                                                  
226600       PERFORM IMS-ISRT-ALT-MSG-6199                                      
226700       MOVE NEJ                TO SW-1A-6199                              
226800     ELSE                                                                 
226900       PERFORM IMS-PURG-ALT-MSG-6199                                      
227000     END-IF                                                               
227100                                                                          
227200     MOVE +0                   TO 6199-IX                                 
227300                                                                          
227400     IF (NYCKEL-KOLLI        AND                                          
227500         RAD-IX NOT > RAD-MAX-IX)                                         
227600        CONTINUE                                                          
227700     ELSE                                                                 
227800        MOVE INF-PRINTING-STARTED TO MED-IDMFSINF                         
227900        CALL WMEDKONV USING MED-WMEDAREA                                  
228000        MOVE MED-TEMFSINF      TO MOD-TEMFSINF                            
228100        MOVE W-BEPRTLST        TO MOD-TEMFSFEL(1:25)                      
228200        MOVE ' I '             TO MOD-TEMFSFEL(29:3)                      
228300        PERFORM MFS-RENSA-FAELT-IN                                        
228400     END-IF                                                               
228500     .                                                                    
228600     EJECT                                                                
228700 S22-INIT-619A SECTION.                                                   
228800                                                                          
228900     MOVE SPACE                TO  MOD619A-MID-W6I19A01                   
229000     MOVE TAB-IDPRTLST         TO  MOD619A-MID-IDPRTLST                   
229100     MOVE IDPGM                TO  MOD619A-MID-IDPGM                      
229200     MOVE ZERO                 TO  W-KVPOST-P-LISTA                       
229300     .                                                                    
229400     SKIP3                                                                
229500 S23-P-TO-P-619A SECTION.                                                 
229600                                                                          
229700     MOVE W-KVPOST-P-LISTA     TO  MOD619A-MID-KVPOST                     
229800     COMPUTE P-TO-P-MSG-KVLL   = LNG-P-TO-P-PREFIX                        
229900                               + 23 + (MOD619A-MID-KVPOST * 39)           
230000                                                                          
230100     MOVE LOW-VALUE            TO P-TO-P-MSG-KDZ1                         
230200     MOVE LOW-VALUE            TO P-TO-P-MSG-KDZ2                         
230300     MOVE 'W6T19AX '           TO P-TO-P-MSG-KDTRANS                      
230400     MOVE '6141'               TO P-TO-P-MSG-IDTRANS                      
230500     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
230600                                                                          
230700     MOVE MOD619A-MID-W6I19A01 TO P-TO-P-MSG-INDATA                       
230800                                                                          
230900     IF  SW-1A-619A = JA                                                  
231000       PERFORM IMS-ISRT-ALT-MSG-619A                                      
231100       MOVE NEJ                TO SW-1A-619A                              
231200     ELSE                                                                 
231300       PERFORM IMS-PURG-ALT-MSG-619A                                      
231400     END-IF                                                               
231500                                                                          
231600     MOVE +0                   TO 619A-IX                                 
231700                                                                          
231800     IF (NYCKEL-KOLLI        AND                                          
231900         RAD-IX NOT > RAD-MAX-IX)                                         
232000        CONTINUE                                                          
232100     ELSE                                                                 
232200        MOVE INF-PRINTING-STARTED TO MED-IDMFSINF                         
232300        CALL WMEDKONV USING MED-WMEDAREA                                  
232400        MOVE MED-TEMFSINF      TO MOD-TEMFSINF                            
232500        MOVE W-BEPRTLST        TO MOD-TEMFSFEL(1:25)                      
232600        MOVE ' P '             TO MOD-TEMFSFEL(32:3)                      
232700        PERFORM MFS-RENSA-FAELT-IN                                        
232800     END-IF                                                               
232900     .                                                                    
233000     EJECT                                                                
233100 S24-FLYTTA-MID-TILL-MOD SECTION.                                         
233200                                                                          
233300     IF MID-ADINLOMR-PRT-IN   =   ALL '+'                                 
233400        MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-PRT-IN               
233500     ELSE                                                                 
233600        MOVE MID-ADINLOMR-PRT-IN TO  MOD-ADINLOMR-PRT-IN                  
233700        MOVE MFS-ADD-LAES-IN-FAELT  TO  MOD-ADINLOMR-PRT-IN-ATTR          
233800     END-IF                                                               
233900                                                                          
234000     MOVE  +1                       TO  RAD-IX                            
234100     PERFORM UNTIL RAD-IX           >   RAD-MAX-IX                        
234200       IF MID-IDLEVNR-KOLLI(RAD-IX) =   ALL '+'                           
234300          MOVE MFS-RENSA-FAELT      TO  MOD-IDLEVNR-KOLLI(RAD-IX)         
234400       ELSE                                                               
234500          MOVE MID-IDLEVNR-KOLLI(RAD-IX) TO                               
234600                                   MOD-IDLEVNR-KOLLI(RAD-IX)              
234700       END-IF                                                             
234800                                                                          
234900       IF MID-IDOKOLLI(RAD-IX)      =   ALL '+'                           
235000          MOVE MFS-RENSA-FAELT      TO  MOD-IDOKOLLI(RAD-IX)              
235100       ELSE                                                               
235200          MOVE MID-IDOKOLLI(RAD-IX)  TO                                   
235300                                   MOD-IDOKOLLI(RAD-IX)                   
235400       END-IF                                                             
235500                                                                          
235600       ADD  1                       TO  RAD-IX                            
235700     END-PERFORM                                                          
235800     .                                                                    
235900     EJECT                                                                
236000 S30-PRIM-SEK-KOLL SECTION.                                               
236100                                                                          
236200     MOVE NEJ       TO WS-FL-PRIM-SEK                                     
236300** KOLLAR OM KONTROLLERAD PÅ 6139                                         
236400     MOVE ART-IDLOPNRM     TO W-IDLOPNRM                                  
236500     PERFORM IMS-GU-UPFA-01                                               
236600     IF SEGMENT-FINNS                                                     
236700       IF UPPF-KVKVAPRIM > 0                                              
236800** ARTIKEL UTTAGEN FÖR PRIMÄRKONTROLL                                     
236900          IF UPPF-KDKVASTA-PRI = '2' OR '3'                               
237000** PRIMÄRKONTROLL SATT SOM JA/NEJ. (OM NEJ HAR KR SKAPATS).               
237100             CONTINUE                                                     
237200          ELSE                                                            
237300             MOVE JA        TO WS-FL-PRIM-SEK                             
237400          END-IF                                                          
237500       END-IF                                                             
237600       IF UPPF-KVKVASEK > 0                                               
237700          IF UPPF-KDKVASTA-SEK = '2' OR '3'                               
237800             CONTINUE                                                     
237900          ELSE                                                            
238000             MOVE JA        TO WS-FL-PRIM-SEK                             
238100          END-IF                                                          
238200       END-IF                                                             
238300     END-IF                                                               
238400                                                                          
238500** KOLLAR ATT EVENTUELLT GAMLA KR BLIVIT BEDÖMDA                          
238600     IF INDATA-OK                                                         
238700       PERFORM IMS-GU-UPFA-01                                             
238800       IF SEGMENT-FINNS                                                   
238900         PERFORM IMS-GNP-UPFA11                                           
239000         PERFORM UNTIL SEGMENT-SAKNAS                                     
239100           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
239200             CONTINUE                                                     
239300           ELSE                                                           
239400             MOVE JA        TO WS-FL-PRIM-SEK                             
239500           END-IF                                                         
239600           PERFORM IMS-GNP-UPFA11                                         
239700         END-PERFORM                                                      
239800       END-IF                                                             
239900     END-IF                                                               
240000                                                                          
240100** KOLLAR ATT EVENTUELL SPECIALKONTROLL ÄR GJORD                          
240200     IF INDATA-OK                                                         
240300       PERFORM IMS-GU-UPFA-01                                             
240400       IF SEGMENT-FINNS                                                   
240500         PERFORM IMS-GNP-UPFA12                                           
240600         PERFORM UNTIL SEGMENT-SAKNAS                                     
240700           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
240800             CONTINUE                                                     
240900           ELSE                                                           
241000             MOVE JA        TO WS-FL-PRIM-SEK                             
241100           END-IF                                                         
241200           PERFORM IMS-GNP-UPFA12                                         
241300         END-PERFORM                                                      
241400       END-IF                                                             
241500     END-IF                                                               
241600     .                                                                    
241700     EJECT                                                                
241800 S90-SEND-MAIL SECTION.                                                   
241900                                                                          
242000     MOVE '6141'            TO MAIL-IDTRANS                               
242100     MOVE '1'               TO MAIL-KDMFSFOR                              
242210     MOVE 'DCIDHELP@VOLVOCARS.COM' TO MAIL-IDMAIL                         
242300     MOVE +01               TO MAIL-KVMAILLN                              
242400     MOVE 'FEL PTYP SKICKAS FRÅN VCOM.'                                   
242500                            TO MAIL-TEMAIL (1)                            
242600                                                                          
242700     PERFORM IMS-ISRT-MAIL                                                
242800     .                                                                    
242900     EJECT                                                                
243000 MFS-RENSA-FAELT-UT SECTION.                                              
243100                                                                          
243200*    --- ALLA UTDATA-FÄLT                                                 
243300     MOVE +1 TO RAD-IX                                                    
243400     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
243500       MOVE MFS-RENSA-FAELT TO MOD-TEMFSMED(RAD-IX)                       
243600       ADD 1 TO RAD-IX                                                    
243700     END-PERFORM                                                          
243800     MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-PRT-UT                          
243900     .                                                                    
244000     SKIP2                                                                
244100 MFS-RENSA-FAELT-IN SECTION.                                              
244200                                                                          
244300*    --- ALLA INDATA-FÄLT PÅ RAD                                          
244400     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN                                 
244500                             MOD-ADINLOMR                                 
244600     MOVE +1 TO RAD-IX                                                    
244700     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
244800       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI(RAD-IX)                  
244900                               MOD-IDOKOLLI(RAD-IX)                       
245000       ADD 1 TO RAD-IX                                                    
245100     END-PERFORM                                                          
245200     .                                                                    
245300     EJECT                                                                
245400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
245500                                                                          
245600*    --- ALLA UTDATA-FÄLT                                                 
245700     MOVE +1 TO RAD-IX                                                    
245800     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
245900       MOVE MFS-ROER-EJ-FAELT TO MOD-TEMFSMED(RAD-IX)                     
246000       ADD 1 TO RAD-IX                                                    
246100     END-PERFORM                                                          
246200     MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT-UT                        
246300     .                                                                    
246400     SKIP2                                                                
246500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
246600                                                                          
246700*    --- ALLA INDATA-FÄLT                                                 
246800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDINLVGN                               
246900                               MOD-ADINLOMR                               
247000     MOVE +1 TO RAD-IX                                                    
247100     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
247200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-KOLLI(RAD-IX)                
247300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDOKOLLI(RAD-IX)                     
247400       ADD 1 TO RAD-IX                                                    
247500     END-PERFORM                                                          
247600     .                                                                    
247700     SKIP3                                                                
247800 MFS-LAES-IN-IGEN SECTION.                                                
247900                                                                          
248000*    --- ALLA INDATA-FÄLT                                                 
248100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDINLVGN-ATTR                      
248200                                   MOD-ADINLOMR-ATTR                      
248300                                   MOD-ADINLOMR-PRT-IN-ATTR               
248400     MOVE +1 TO RAD-IX                                                    
248500     PERFORM UNTIL RAD-IX > RAD-MAX-IX                                    
248600      MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-KOLLI-ATTR(RAD-IX)        
248700                                    MOD-IDOKOLLI-ATTR(RAD-IX)             
248800       ADD 1 TO RAD-IX                                                    
248900     END-PERFORM                                                          
249000     .                                                                    
249100     EJECT                                                                
249200* --- IMS SEKTIONER ---                                                   
249300     SKIP3                                                                
249400 IMS-GET-MSG SECTION.                                                     
249500                                                                          
249600     MOVE '  QC' TO GODK-STATUSKODER                                      
249700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
249800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
249900     PERFORM IMS-STATUSKONTROLL                                           
250000     .                                                                    
250100     SKIP3                                                                
250200 IMS-INSERT-MSG SECTION.                                                  
250300                                                                          
250400*    IF ENGLISH-TEXT                                                      
250500*      MOVE 'N' TO MFS-KDHUVOMR                                           
250600*    END-IF                                                               
250700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
250800     MOVE SPACE TO GODK-STATUSKODER                                       
250900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
251000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
251100     PERFORM IMS-STATUSKONTROLL                                           
251200     .                                                                    
251300     EJECT                                                                
251400 IMS-ISRT-ALT-MSG-6199 SECTION.                                           
251500                                                                          
251600     MOVE SPACE TO GODK-STATUSKODER                                       
251700     CALL CBLTDLI USING ISRT ALT-6199-PCB P-TO-P-MSG-IO-AREA-SNUF         
251800     MOVE ALT-6199-STATUS-CODE TO STATUS-WS                               
251900     PERFORM IMS-STATUSKONTROLL                                           
252000     .                                                                    
252100     SKIP2                                                                
252200 IMS-PURG-ALT-MSG-6199 SECTION.                                           
252300                                                                          
252400     MOVE SPACE TO GODK-STATUSKODER                                       
252500     CALL CBLTDLI USING PURG ALT-6199-PCB P-TO-P-MSG-IO-AREA-SNUF         
252600     MOVE ALT-6199-STATUS-CODE TO STATUS-WS                               
252700     PERFORM IMS-STATUSKONTROLL                                           
252800     .                                                                    
252900     SKIP3                                                                
253000 IMS-ISRT-ALT-MSG-619A SECTION.                                           
253100                                                                          
253200     MOVE SPACE TO GODK-STATUSKODER                                       
253300     CALL CBLTDLI USING ISRT ALT-619A-PCB P-TO-P-MSG-IO-AREA-SNUF         
253400     MOVE ALT-619A-STATUS-CODE TO STATUS-WS                               
253500     PERFORM IMS-STATUSKONTROLL                                           
253600     .                                                                    
253700     SKIP2                                                                
253800 IMS-PURG-ALT-MSG-619A SECTION.                                           
253900                                                                          
254000     MOVE SPACE TO GODK-STATUSKODER                                       
254100     CALL CBLTDLI USING PURG ALT-619A-PCB P-TO-P-MSG-IO-AREA-SNUF         
254200     MOVE ALT-619A-STATUS-CODE TO STATUS-WS                               
254300     PERFORM IMS-STATUSKONTROLL                                           
254400     .                                                                    
254500     EJECT                                                                
254600 IMS-GU-INLG01      SECTION.                                              
254700     STRING 'W6INLG01(W6D1F1KY>=' W-W6D1F1KY-MIN-X                        
254800                    '&W6D1F1KY<=' W-W6D1F1KY-MAX-X ')'                    
254900          DELIMITED BY SIZE INTO SSA1                                     
255000     MOVE '  GE' TO GODK-STATUSKODER                                      
255100     CALL CBLTDLI USING GU INLG-PCB DLI-IO-AREA4 SSA1                     
255200     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
255300     PERFORM IMS-STATUSKONTROLL                                           
255400     .                                                                    
255500     SKIP3                                                                
255600 IMS-GN-INLG01      SECTION.                                              
255700     STRING 'W6INLG01(W6D1F1KY>=' W-W6D1F1KY-MIN-X                        
255800                    '&W6D1F1KY<=' W-W6D1F1KY-MAX-X ')'                    
255900          DELIMITED BY SIZE INTO SSA1                                     
256000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
256100     CALL CBLTDLI USING GN INLG-PCB DLI-IO-AREA4 SSA1                     
256200     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
256300     PERFORM IMS-STATUSKONTROLL                                           
256400     .                                                                    
256500     EJECT                                                                
256600 IMS-GU-INLF01      SECTION.                                              
256700     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
256800                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X ')'                    
256900          DELIMITED BY SIZE INTO SSA1                                     
257000     MOVE '  GE' TO GODK-STATUSKODER                                      
257100     CALL CBLTDLI USING GU INLF-PCB DLI-IO-AREA4 SSA1                     
257200     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
257300     PERFORM IMS-STATUSKONTROLL                                           
257400     .                                                                    
257500     SKIP3                                                                
257600 IMS-GN-INLF01      SECTION.                                              
257700     STRING 'W6INLF01(W6D1E1KY>=' W-W6D1E1KY-MIN-X                        
257800                    '&W6D1E1KY<=' W-W6D1E1KY-MAX-X ')'                    
257900          DELIMITED BY SIZE INTO SSA1                                     
258000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
258100     CALL CBLTDLI USING GN INLF-PCB DLI-IO-AREA4 SSA1                     
258200     MOVE INLF-STATUS-CODE TO STATUS-WS                                   
258300     PERFORM IMS-STATUSKONTROLL                                           
258400     .                                                                    
258500     EJECT                                                                
258600 IMS-GU-INLD01      SECTION.                                              
258700     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
258800                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X ')'                    
258900          DELIMITED BY SIZE INTO SSA1                                     
259000     MOVE '  GE' TO GODK-STATUSKODER                                      
259100     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA4 SSA1                     
259200     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
259300     PERFORM IMS-STATUSKONTROLL                                           
259400     .                                                                    
259500     SKIP3                                                                
259600 IMS-GN-INLD01      SECTION.                                              
259700     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
259800                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X ')'                    
259900          DELIMITED BY SIZE INTO SSA1                                     
260000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
260100     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA4 SSA1                     
260200     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
260300     PERFORM IMS-STATUSKONTROLL                                           
260400     .                                                                    
260500     EJECT                                                                
260600 IMS-GU-INLA11      SECTION.                                              
260700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
260800          DELIMITED BY SIZE INTO SSA1                                     
260900     STRING 'W6INLA11(IDRADNRI =' W-W6D111-IDRADNR-INL-X ')'              
261000          DELIMITED BY SIZE INTO SSA2                                     
261100     MOVE '    ' TO GODK-STATUSKODER                                      
261200     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA1 SSA1 SSA2               
261300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
261400     PERFORM IMS-STATUSKONTROLL                                           
261500     .                                                                    
261600     SKIP3                                                                
261700 IMS-GU-INLA11-D    SECTION.                                              
261800     STRING 'W6INLA11(W6D1DSEQ>=' W-W6D1DSEQ-MIN-X                        
261900                    '&W6D1DSEQ<=' W-W6D1DSEQ-MAX-X ')'                    
262000          DELIMITED BY SIZE INTO SSA1                                     
262100     MOVE '  GE' TO GODK-STATUSKODER                                      
262200     CALL CBLTDLI USING GU INLA-D-PCB DLI-IO-AREA1 SSA1                   
262300     MOVE INLA-D-STATUS-CODE TO STATUS-WS                                 
262400     PERFORM IMS-STATUSKONTROLL                                           
262500     .                                                                    
262600     EJECT                                                                
262700 IMS-GU-INLA21    SECTION.                                                
262800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
262900          DELIMITED BY SIZE INTO SSA1                                     
263000     STRING 'W6INLA11(IDRADNRI =' W-W6D111-IDRADNR-INL-X ')'              
263100          DELIMITED BY SIZE INTO SSA2                                     
263200     STRING 'W6INLA21(IDRADNR  =' W-W6D121-IDRADNR-X ')'                  
263300          DELIMITED BY SIZE INTO SSA3                                     
263400     MOVE '    ' TO GODK-STATUSKODER                                      
263500     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3          
263600     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
263700     PERFORM IMS-STATUSKONTROLL                                           
263800     .                                                                    
263900     SKIP3                                                                
264000 IMS-GHU-INLA21    SECTION.                                               
264100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
264200          DELIMITED BY SIZE INTO SSA1                                     
264300     STRING 'W6INLA11(IDRADNRI =' W-W6D111-IDRADNR-INL-X ')'              
264400          DELIMITED BY SIZE INTO SSA2                                     
264500     STRING 'W6INLA21(IDRADNR  =' W-W6D121-IDRADNR-X ')'                  
264600          DELIMITED BY SIZE INTO SSA3                                     
264700     MOVE '  GE' TO GODK-STATUSKODER                                      
264800     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3         
264900     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
265000     PERFORM IMS-STATUSKONTROLL                                           
265100     .                                                                    
265200     SKIP3                                                                
265300 IMS-REPL-INLA21   SECTION.                                               
265400                                                                          
265500     MOVE '  ' TO GODK-STATUSKODER                                        
265600     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA2                       
265700     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
265800     PERFORM IMS-STATUSKONTROLL                                           
265900     .                                                                    
266000     EJECT                                                                
266100 IMS-GU-PLAA-PLAA11 SECTION.                                              
266200     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
266300          DELIMITED BY SIZE INTO SSA1                                     
266400     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
266500          DELIMITED BY SIZE INTO SSA2                                     
266600     MOVE '  GE' TO GODK-STATUSKODER                                      
266700     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA3 SSA1 SSA2                
266800     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
266900     PERFORM IMS-STATUSKONTROLL                                           
267000     .                                                                    
267100     SKIP3                                                                
267200 IMS-GU-PLAA11-PAR  SECTION.                                              
267300     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
267400          DELIMITED BY SIZE INTO SSA1                                     
267500     STRING 'W6PLAA11(ADINLOMP =' W-6006-ADINLOMR-PAR-X ')'               
267600          DELIMITED BY SIZE INTO SSA2                                     
267700     MOVE '  GE' TO GODK-STATUSKODER                                      
267800     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA3 SSA1 SSA2                
267900     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
268000     PERFORM IMS-STATUSKONTROLL                                           
268100     .                                                                    
268200     SKIP3                                                                
268300 IMS-GN-PLAA11-PAR SECTION.                                               
268400     STRING 'W6PLAA11(ADINLOMP =' W-6006-ADINLOMR-PAR-X ')'               
268500          DELIMITED BY SIZE INTO SSA1                                     
268600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
268700     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA3  SSA1                    
268800     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
268900     PERFORM IMS-STATUSKONTROLL                                           
269000     .                                                                    
269100     EJECT                                                                
269200 IMS-GHU-LOPA-LOPA11 SECTION.                                             
269300     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
269400          DELIMITED BY SIZE INTO SSA1                                     
269500     MOVE 'W6LOPA11  ' TO SSA2                                            
269600     MOVE '  GE' TO GODK-STATUSKODER                                      
269700     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2               
269800     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
269900     PERFORM IMS-STATUSKONTROLL                                           
270000     .                                                                    
270100     SKIP3                                                                
270200 IMS-REPL-LOPA-LOPA11 SECTION.                                            
270300                                                                          
270400     MOVE '  ' TO GODK-STATUSKODER                                        
270500     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA3                        
270600     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
270700     PERFORM IMS-STATUSKONTROLL                                           
270800     .                                                                    
270900     EJECT                                                                
271000 IMS-GU-UPFA-01  SECTION.                                                 
271100     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
271200          DELIMITED BY SIZE INTO SSA1                                     
271300     MOVE '  GE' TO GODK-STATUSKODER                                      
271400     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-UPFA SSA1                      
271500     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
271600     PERFORM IMS-STATUSKONTROLL                                           
271700     .                                                                    
271800     EJECT                                                                
271900 IMS-GNP-UPFA11 SECTION.                                                  
272000                                                                          
272100     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
272200          DELIMITED BY SIZE INTO SSA1                                     
272300     MOVE 'W6UPFA11 ' TO SSA2                                             
272400     MOVE '  GE' TO GODK-STATUSKODER                                      
272500     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
272600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
272700     PERFORM IMS-STATUSKONTROLL                                           
272800     .                                                                    
272900     SKIP3                                                                
273000 IMS-GNP-UPFA12 SECTION.                                                  
273100                                                                          
273200     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
273300          DELIMITED BY SIZE INTO SSA1                                     
273400     MOVE 'W6UPFA12 ' TO SSA2                                             
273500     MOVE '  GE' TO GODK-STATUSKODER                                      
273600     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
273700     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
273800     PERFORM IMS-STATUSKONTROLL                                           
273900     .                                                                    
274000     SKIP3                                                                
274100 IMS-ISRT-MAIL SECTION.                                                   
274200                                                                          
274300     MOVE SPACE TO GODK-STATUSKODER                                       
274400     CALL CBLTDLI USING ISRT ALTMAIL-PCB MAIL-WMSGMAIL                    
274500     MOVE ALTMAIL-STATUS-CODE TO STATUS-WS                                
274600     PERFORM IMS-STATUSKONTROLL                                           
274700     .                                                                    
274800     EJECT                                                                
274900 IMS-STATUSKONTROLL SECTION.                                              
275000                                                                          
275100     SET STATUS-IX TO 1                                                   
275200     SEARCH GODK-STATUS                                                   
275300       AT END                                                             
275400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
275500         DELIMITED BY SIZE INTO FELTEXT                                   
275600         CALL FELLOG                                                      
275700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
275800         CONTINUE                                                         
275900     END-SEARCH                                                           
276000     .                                                                    
