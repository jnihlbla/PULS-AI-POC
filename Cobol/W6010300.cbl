000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010300.                                                
000400*AUTHOR.         GUNNAR LARSSON IDK.                                      
000500*DATE-WRITTEN.   92/05/04.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ERSÄTTA FLAGGA - INLEVERANS                                      
000900*                                                                         
001000*        PROGRAMMET LÄSER      W6INLD (W6D1)                              
001100*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001200*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001300*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
001310*        PROGRAMMET LÄSER              WDB6                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W6T103                                              
001700*        MID:         W6I10301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W6O10301  (TILL SKÄRM)                              
002100*        MOD:         W6I19101  (PROG-TO-PROG-SW)                         
002200*        MOD:         W6I19401  (PROG-TO-PROG-SW)                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002801                                                                          
002810*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6010300'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004100*   SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                       
004200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +339  COMP SYNC.        
004300*   PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                      
004400 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
004500 77  MAX-MOD-LAENGD-6191         PIC S9(4)  VALUE +1448 COMP SYNC.        
004600 77  MAX-MOD-LAENGD-6194         PIC S9(4)  VALUE +1045 COMP SYNC.        
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300     88  INDATA-OK                           VALUE 'J'.                   
005400     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006100     88  ALLT-OK                             VALUE 'J'.                   
006200                                                                          
006300 77  DIVERSEKOLLI-SW             PIC X       VALUE 'N'.                   
006400     88  DIVERSEKOLLI                        VALUE 'J'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '6103'.                
006800     88  GODK-MID                            VALUE '6103'.                
006900     88  HELP-MID                            VALUE '0551'.                
007000     EJECT                                                                
007100*    --- GENERELLA ARBETSFÄLT                                             
007200 01      FILLER                  PIC X(8)    VALUE 'WS******'.            
007300 01      WS.                                                              
007400  02     WS-VKKOLLIN-GRAM        PIC S9(9)   VALUE ZERO COMP-3.           
007500     EJECT                                                                
007600*    --- SPARAT FRÅN GAMMAL RAD                                           
007700 01      FILLER                  PIC X(8)    VALUE 'WS-GML**'.            
007800 01      WS-GML.                                                          
007900  02     WS-GML-ADINLOMR         PIC X(4)    VALUE SPACE.                 
008000  02     WS-GML-ADINLOMR-NXT     PIC X(4)    VALUE SPACE.                 
008100  02     WS-GML-KDINLSTA         PIC X(3)    VALUE SPACE.                 
008200  02     WS-GML-KVINLART         PIC S9(7)   VALUE ZERO COMP-3.           
008300     EJECT                                                                
008400*    --- KONSTANTER                                                       
008500                                                                          
008600 01      FILLER                  PIC X(8)    VALUE 'K*******'.            
008700                                                                          
008800 01      K-KONSTANTER.                                                    
008900                                                                          
009000  02     K-MAX-6191-KVPOST       PIC S9(9)   VALUE +24  COMP SYNC.        
009100  02     K-MAX-6194-KVPOST       PIC S9(9)   VALUE +15  COMP SYNC.        
009200     SKIP3                                                                
009300*    --- SWITCHAR                                                         
009400                                                                          
009500 01      FILLER                  PIC X(8)    VALUE 'SW******'.            
009600                                                                          
009700 01      SW-SWITCHAR.                                                     
009800                                                                          
009900  02     SW-OK-BILDRAD           PIC X(1)    VALUE SPACE.                 
010000  02     SW-BILDRADER-INMATADE   PIC X(1)    VALUE SPACE.                 
010100  02     SW-KDINLSTA-GILTIG      PIC X(1)    VALUE SPACE.                 
010200  02     SW-UPPDAT-INLA-RAD      PIC X(1)    VALUE SPACE.                 
010300  02     SW-UPPDAT-LOPA          PIC X(1)    VALUE SPACE.                 
010400  02     SW-6194-REDIGERAD       PIC X(1)    VALUE SPACE.                 
010500  02     SW-1A-6191              PIC X(1)    VALUE SPACE.                 
010600     SKIP3                                                                
010700*    --- INDEXVARIABLER                                                   
010800                                                                          
010900 01      FILLER                  PIC X(8)    VALUE 'IX******'.            
011000                                                                          
011100 01      IX-INDEXVARIABLER.                                               
011200                                                                          
011300  02     IX-6191                 PIC S9(9)   VALUE ZERO COMP SYNC.        
011400  02     IX-6194                 PIC S9(9)   VALUE ZERO COMP SYNC.        
011500     EJECT                                                                
011600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011700 01  GENERELLA-SUBPROGRAM.                                                
011800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012100     03  W006PRT                 PIC X(8)    VALUE 'W006PRT'.             
012110     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012200     EJECT                                                                
012210*01 -COPY WMSGINIT                                                        
012220     SKIP3                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012400*01 -COPY WMEDAREA                                                        
012500     SKIP3                                                                
012600 01  MESSAGE-CODES.                                                       
012700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012900     03  ERR-007-OTILLATEN-UPD   PIC X(3)    VALUE '007'.                 
013000     03  ERR-010-NOT-IN-REG      PIC X(3)    VALUE '010'.                 
013100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013500     03  ERR-772-PRT-FEL         PIC X(3)    VALUE '772'.                 
013600     03  INF-FLAG-PRINTED        PIC X(3)    VALUE '790'.                 
013700     EJECT                                                                
013800 01  FILLER                    PIC X(16) VALUE 'W006PRT*********'.        
013900     SKIP3                                                                
014000*01  -COPY W006PRT                                                        
014100     EJECT                                                                
014200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014500     SKIP3                                                                
014600*01  MID -COPY W6I10301                                                   
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014900     SKIP3                                                                
015000*01  -COPY WMSGAREA                                                       
015100     EJECT                                                                
015200     03  MOD REDEFINES MSG-AREA.                                          
015300*      05  -COPY W6O10301                                                 
015400     EJECT                                                                
015500 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
015600     SKIP3                                                                
015700 01      P-TO-P-SW.                                                       
015800                                                                          
015900  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
016000  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
016100  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
016200  02     P-TO-P-KDTRANS          PIC X(8).                                
016300  02     P-TO-P-IDTRANS          PIC X(4).                                
016400  02     P-TO-P-KDMFSFOR         PIC X(1).                                
016500  02     P-TO-P-DATA             PIC X(1500).                             
016600     EJECT                                                                
016700 01      FILLER                  PIC X(24)   VALUE                        
016800                                 'MOD6191-MID-W6I19101'.                  
016900     SKIP2                                                                
017000     -COPY W6I19101 -PRE MOD6191-                                         
017100     EJECT                                                                
017200 01      FILLER                  PIC X(24)   VALUE                        
017300                                 'MOD6194-MID-W6I19401'.                  
017400     SKIP2                                                                
017500*    -COPY W6I19401 -PRE MOD6194-                                         
017600     EJECT                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017800     SKIP3                                                                
017900*01  -COPY WMFSAREA                                                       
018000     EJECT                                                                
018100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018200*                                                                         
018300     SKIP2                                                                
018400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018500     SKIP2                                                                
018600 01  NYCKLAR-TILL-DLI.                                                    
018700                                                                          
018800*    -- W6D1 HUVUDBAS.                                                    
018900     03  W-W6D101KY-X.                                                    
019000         05  W-W6D101KY-IDDC     PIC  X(2)   VALUE SPACE.                 
019010         05  W-W6D101KY-IDLEVNR  PIC  X(5)   VALUE SPACE.                 
019100         05  W-W6D101KY-IDFS     PIC X(8)    VALUE SPACE.                 
019200         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
019300     03  W-IDRADNR-INL-X.                                                 
019400         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
019500     03  W-IDRADNR-X.                                                     
019600         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
019700                                                                          
019800*    -- W6D1 INDEXBAS C. MIN O MAX.                                       
019900     03  W-W6D1C1KY-MIN-X.                                                
020000         05  W-W6D1C1KY-MIN-IDLEVNR-KOLLI                                 
020100                                 PIC  X(5)   VALUE SPACE.                 
020200         05  W-W6D1C1KY-MIN-IDOKOLLI                                      
020300                                 PIC 9(9)    VALUE ZERO.                  
020400         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
020410         05  FILLER              PIC  X(2)   VALUE SPACE.                 
020500         05  FILLER              PIC X(5)    VALUE SPACE.                 
020600         05  FILLER              PIC X(8)    VALUE SPACE.                 
020700         05  FILLER              PIC S9(7)   VALUE ZERO COMP-3.           
020800         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
020900     03  W-W6D1C1KY-MAX-X.                                                
021000         05  W-W6D1C1KY-MAX-IDLEVNR-KOLLI                                 
021010                                 PIC  X(5)   VALUE SPACE.                 
021200         05  W-W6D1C1KY-MAX-IDOKOLLI                                      
021300                                 PIC 9(9)    VALUE ZERO.                  
021400         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
021410         05  FILLER              PIC  X(2)   VALUE SPACE.                 
021500         05  FILLER              PIC X(5)    VALUE SPACE.                 
021600         05  FILLER              PIC X(8)    VALUE SPACE.                 
021700         05  FILLER              PIC S9(7)   VALUE ZERO COMP-3.           
021800         05  FILLER              PIC S9(5)   VALUE ZERO COMP-3.           
021900                                                                          
022000     03  W-IDDC-X.                                                        
022100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
022200                                                                          
022300     03  W-W6GXKEY-6005-X.                                                
022400         05  FILLER              PIC X(4)    VALUE '6005'.                
022500         05  W-IDDC-6005         PIC X(2)    VALUE SPACE.                 
022600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
022700     03  W-W6GXKEY-6006-X.                                                
022800         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
022900         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
023000                                                                          
023100     03  W-W6GXKEY-6017-X.                                                
023200         05  FILLER              PIC X(4)    VALUE '6017'.                
023300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
023310                                                                          
023320     03  W-IDDC-B6-X.                                                     
023330         05 W-IDDC-B6            PIC X(2).                                
023340                                                                          
023400     SKIP2                                                                
023500*    --- STATUS-KOD FRÅN IMS                                              
023600 01  STATUS-WS                   PIC XX.                                  
023700     88  SEGMENT-FINNS                       VALUE '  '.                  
023800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024000     SKIP2                                                                
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  SSA1                        PIC X(192).                              
024500 01  SSA2                        PIC X(64).                               
024600 01  SSA3                        PIC X(64).                               
024700     EJECT                                                                
024800*    --- IMS FUNKTIONSKODER                                               
024900*01  -COPY W0003                                                          
025000     EJECT                                                                
025100*    ---  DLI INPUT-OUTPUT AREA                                           
025200                                                                          
025300*         DLI-IO-AREA    W6INLA11, W6INLD01                               
025400                                                                          
025500*         DLI-IO-AREA2   W6INLA21  GR                                     
025600                                                                          
025700*         DLI-IO-AREA3   W6INLA01                                         
025800                                                                          
025900*         DLI-IO-AREA4   W6PLAA11                                         
026000                                                                          
026100*         DLI-IO-AREA5   W6LOPA11  GR                                     
026200     EJECT                                                                
026300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026400     SKIP3                                                                
026500 01  DLI-IO-AREA.                                                         
026600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
026700     SKIP3                                                                
026800     03  W6INLA11 REDEFINES IO-AREA.                                      
026900*        05  -COPY W6D111  -PRE INLA-                                     
027000     EJECT                                                                
027100     03  W6INLD01 REDEFINES IO-AREA.                                      
027200*        05  -COPY W6D1C1  -PRE INLD-                                     
027300     EJECT                                                                
027400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
027500     SKIP3                                                                
027600 01  DLI-IO-AREA2.                                                        
027700     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
027800     SKIP3                                                                
027900     03  W6INLA21 REDEFINES IO-AREA2.                                     
028000*        05  -COPY W6D121  -PRE INLA-                                     
028100     EJECT                                                                
028200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
028300     SKIP3                                                                
028400 01  DLI-IO-AREA3.                                                        
028500     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
028600     SKIP3                                                                
028700     03  W6INLA01 REDEFINES IO-AREA3.                                     
028800*        05  -COPY W6D101  -PRE INLA-                                     
028900     EJECT                                                                
029000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
029100     SKIP3                                                                
029200 01  DLI-IO-AREA4.                                                        
029300     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
029400     SKIP3                                                                
029500     03  W6PLAA11 REDEFINES IO-AREA4.                                     
029600*        05  -COPY W6GX6006 -PRE PLAA-                                    
029700     EJECT                                                                
029800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
029900     SKIP3                                                                
030000 01  DLI-IO-AREA5.                                                        
030100     03  IO-AREA5                PIC X(100)  VALUE SPACE.                 
030200     03  W6LOPA11 REDEFINES IO-AREA5.                                     
030300*        05  -COPY W6GX6018 -PRE LOPA-                                    
030310                                                                          
030320 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030330 01   DLI-IO-AREA-B601.                                                   
030340*     03  -COPY WDB601                                                    
030350                                                                          
030400     EJECT                                                                
030500 LINKAGE SECTION.                                                         
030600                                                                          
030700*01  -COPY W0009   -PRE MSG-                                              
030800     EJECT                                                                
030900*01  -COPY W0009   -PRE 6191-                                             
031000     EJECT                                                                
031100*01  -COPY W0009   -PRE 6194-                                             
031200     EJECT                                                                
031300*01  -COPY W0008  -PRE USEA-                                              
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031510*01  -COPY W0008  -PRE INLD-                                              
031520     05  FILLER                  PIC X.                                   
031530     EJECT                                                                
031600*01  -COPY W0008  -PRE INLA-                                              
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE PLAA-                                              
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE LOPA-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032410*01  -COPY W0008  -PRE WDB6-                                              
032420     05  FILLER                  PIC X.                                   
032430     EJECT                                                                
032500 PROCEDURE DIVISION  USING MSG-PCB                                        
032600                           6191-PCB 6194-PCB USEA-PCB                     
032700                           INLD-PCB INLA-PCB                              
032800                           PLAA-PCB LOPA-PCB WDB6-PCB.                    
032900     ENTRY 'DLITCBL' USING MSG-PCB                                        
033000                           6191-PCB 6194-PCB USEA-PCB                     
033100                           INLD-PCB INLA-PCB                              
033200                           PLAA-PCB LOPA-PCB WDB6-PCB.                    
033300                                                                          
033400     PERFORM IMS-GET-MSG                                                  
033500     IF SEGMENT-FINNS                                                     
033600       PERFORM A-INIT                                                     
033700       PERFORM B-KOLLA-NYCKLAR                                            
033800       IF NYCKLAR-OK                                                      
033900         IF MFS-UPDATE                                                    
034000           PERFORM G-KOLLA-INPUT                                          
034100           IF INDATA-OK                                                   
034200             PERFORM H-UPPDATERA                                          
034300           END-IF                                                         
034400         ELSE                                                             
034500           IF MFS-FIRST                                                   
034600             PERFORM C-TOM-SIDA                                           
034700           ELSE                                                           
034800             PERFORM E-SAMMA-SIDA                                         
034900           END-IF                                                         
035000         END-IF                                                           
035100       END-IF                                                             
035200       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
035300       PERFORM IMS-INSERT-MSG                                             
035400     END-IF                                                               
035500                                                                          
035600     MOVE ZERO TO RETURN-CODE                                             
035700     GOBACK                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 A-INIT SECTION.                                                          
036100                                                                          
036200     IF MSG-DUBBLA-TRANSKODER                                             
036300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10301                 
036400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
036500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036600     ELSE                                                                 
036700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10301                  
036800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
036900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037000     END-IF                                                               
037100                                                                          
037200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
037400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037500                                                                          
037600     MOVE LOW-VALUE TO MSG-AREA                                           
037700     MOVE 'W6O103N1' TO MFS-IDMOD                                         
037800     MOVE '6103' TO MOD-IDTRANS                                           
037900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
038000                                                                          
038100     IF NOT EGEN-MID AND NOT HELP-MID                                     
038200       MOVE SPACE TO MFS-KDTRTYP                                          
038300       MOVE '7' TO MFS-IDPFK                                              
038400     END-IF                                                               
039300                                                                          
039400     PERFORM MFS-FORM-ATTR                                                
039401     PERFORM AA-INIT-NYCKLAR                                              
039402                                                                          
039403     IF MSGI-IDLAND-SPR = 'GB'                                            
039404       MOVE +2 TO SPRAK-IX                                                
039405       MOVE 'GB ' TO MED-IDSKYLT                                          
039406     ELSE                                                                 
039407       MOVE +1 TO SPRAK-IX                                                
039408       MOVE 'S  ' TO MED-IDSKYLT                                          
039409     END-IF                                                               
039410     .                                                                    
039411     EJECT                                                                
039412*----------------------------------------------------------------*        
039413 AA-INIT-NYCKLAR SECTION.                                                 
039414                                                                          
039415     MOVE ALL '+' TO MSGI-WMSGINIT                                        
039416     MOVE '001'                  TO MSGI-KDCALL                           
039417     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
039418     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
039419     MOVE '6103'                 TO MSGI-IDTRANS                          
039420     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039421     .                                                                    
039430     EJECT                                                                
039700 B-KOLLA-NYCKLAR SECTION.                                                 
039800                                                                          
039900     IF GODK-MID OR HELP-MID                                              
040000       MOVE JA TO NYCKLAR-SW                                              
040100       MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                              
040200                                                                          
040300                                                                          
040400       IF MID-IDDC-IN         = ALL '+'                                   
040500         MOVE MSGI-IDDC       TO W-IDDC-B6                                
040700                                                                          
040800       ELSE                                                               
040900         MOVE MID-IDDC-IN     TO W-IDDC-B6                                
041000         MOVE '7'             TO MFS-IDPFK                                
041100         MOVE SPACE           TO MFS-KDTRTYP                              
041200       END-IF                                                             
041210       PERFORM IMS-GU-WDB601                                              
041300                                                                          
041400       IF DCS-KDDC = SPACE OR DCS-DDC                                     
041500           MOVE NEJ TO NYCKLAR-SW                                         
041510       ELSE                                                               
041600           MOVE DCS-IDDC     TO W-IDDC                                    
041700                                W-IDDC-6005                               
042000       END-IF                                                             
042100                                                                          
042200       IF GODK-MID OR NYCKLAR-OK                                          
042400         MOVE DCS-IDDC        TO MOD-IDDC-UT                              
042500         INSPECT MOD-IDDC-UT   REPLACING LEADING ZERO BY SPACE            
042600       ELSE                                                               
042800         MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                              
042900       END-IF                                                             
043000                                                                          
043100       IF NYCKLAR-FEL                                                     
043200         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
043300         CALL WMEDKONV USING MED-WMEDAREA                                 
043400         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
043500         PERFORM MFS-RENSA-FAELT-IN                                       
043700       END-IF                                                             
043800                                                                          
043900     ELSE                                                                 
044000       MOVE NEJ               TO NYCKLAR-SW                               
044100       MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                              
044200       PERFORM MFS-RENSA-FAELT-IN                                         
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 C-TOM-SIDA SECTION.                                                      
044800                                                                          
044900     MOVE JA TO ALLT-SW                                                   
045000     PERFORM MFS-RENSA-FAELT-IN                                           
045100     .                                                                    
045200     EJECT                                                                
045300 E-SAMMA-SIDA SECTION.                                                    
045400                                                                          
045500     IF MID-INPUT = ALL '+'                                               
045600       MOVE JA TO ALLT-SW                                                 
045700       PERFORM MFS-RENSA-FAELT-IN                                         
045800       PERFORM EB-LAES-IN-HUVUD-FLT                                       
045900     ELSE                                                                 
046000       IF EGEN-MID                                                        
046100         MOVE NEJ TO ALLT-SW                                              
046200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
046300         CALL WMEDKONV USING MED-WMEDAREA                                 
046400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
046500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
046600         PERFORM MFS-LAES-IN-IGEN                                         
046700         PERFORM EB-LAES-IN-HUVUD-FLT                                     
046800                                                                          
046900       ELSE                                                               
047000         MOVE JA TO ALLT-SW                                               
047100         PERFORM EA-MID-TILL-MOD                                          
047200       END-IF                                                             
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 EA-MID-TILL-MOD SECTION.                                                 
047700                                                                          
047800     IF  MID-ADINLOMR-PRT = ALL '+'                                       
047900       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-PRT                      
048000     ELSE                                                                 
048100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR                
048200       MOVE MID-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                      
048300     END-IF                                                               
048400                                                                          
048500     MOVE +1                     TO INDX                                  
048600                                                                          
048700     PERFORM UNTIL (INDX > MAX-INDX)                                      
048800                                                                          
048900       IF  MID-IDOKOLLI (INDX) = ALL '+'                                  
049000         MOVE MFS-RENSA-FAELT    TO MOD-IDOKOLLI (INDX)                   
049100       ELSE                                                               
049200         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDOKOLLI-ATTR (INDX)           
049300         MOVE MID-IDOKOLLI (INDX) TO MOD-IDOKOLLI (INDX)                  
049400       END-IF                                                             
049500                                                                          
049600       IF  MID-IDLEVNR-KOLLI (INDX) = ALL '+'                             
049700         MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-KOLLI (INDX)              
049800       ELSE                                                               
049900         MOVE MFS-ADD-LAES-IN-FAELT                                       
050000                                 TO MOD-IDLEVNR-KOLLI-ATTR (INDX)         
050100         MOVE MID-IDLEVNR-KOLLI (INDX)                                    
050200                                 TO MOD-IDLEVNR-KOLLI (INDX)              
050300       END-IF                                                             
050400                                                                          
050500       ADD +1                    TO INDX                                  
050600     END-PERFORM                                                          
050700     .                                                                    
050800     EJECT                                                                
050900 EB-LAES-IN-HUVUD-FLT SECTION.                                            
051000                                                                          
051100     IF MID-ADINLOMR-PRT NOT = ALL '+'                                    
051200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR                
051300       MOVE MID-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                      
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 G-KOLLA-INPUT SECTION.                                                   
051800                                                                          
051900     MOVE SPACE TO MED-IDMFSFEL                                           
052000                                                                          
052100     MOVE JA  TO INDATA-SW                                                
052200                                                                          
052300     PERFORM GA-REDIG-HUVUD-FLT                                           
052400     PERFORM GB-KOLLA-HUVUD-FLT                                           
052500     PERFORM GC-KOLLA-BILDRADER                                           
052600                                                                          
052700     IF INDATA-FEL                                                        
052800       CALL WMEDKONV USING MED-WMEDAREA                                   
052900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
053000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
053100     END-IF                                                               
053200     .                                                                    
053300     EJECT                                                                
053400 GA-REDIG-HUVUD-FLT SECTION.                                              
053500                                                                          
053600*    -- REDIG. AV ADINLOMR-PRT                                            
053700                                                                          
053800     IF MID-ADINLOMR-PRT = ALL '+'                                        
053900       MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                   
054000       MOVE NEJ TO INDATA-SW                                              
054100     ELSE                                                                 
054200       MOVE MID-ADINLOMR-PRT TO WS-ADINLOMR-PRT                           
054300                                MOD-ADINLOMR-PRT                          
054400     END-IF                                                               
054500                                                                          
054600     .                                                                    
054700     EJECT                                                                
054800 GB-KOLLA-HUVUD-FLT SECTION.                                              
054900                                                                          
055000*    KONTROLL AV HUVUDFÄLT                                                
055100                                                                          
055200     MOVE +1                     TO INDX                                  
055300                                                                          
055400     PERFORM UNTIL (INDX > MAX-INDX                                       
055920               OR  (MID-IDLEVNR-KOLLI (INDX) NOT = ALL '+'                
055940                AND MID-IDLEVNR-KOLLI (INDX) NOT = SPACE                  
055950                AND NOT (MID-IDLEVNR-KOLLI (INDX) > '99399'               
055960                 AND MID-IDLEVNR-KOLLI (INDX) < '99600')                  
055970                AND MID-IDLEVNR-KOLLI (INDX) NOT = '3324 '))              
056000       ADD +1                    TO INDX                                  
056100     END-PERFORM                                                          
056200                                                                          
056300     IF  INDX NOT > MAX-INDX                                              
056400*      -- KONTROLL MOT PLAA                                               
056500       MOVE WS-ADINLOMR-PRT TO W-6006-ADINLOMR                            
056600       PERFORM IMS-GU-PLAA-6006                                           
056700                                                                          
056800       IF SEGMENT-SAKNAS                                                  
056900*        -- SAKNAS I PLAA                                                 
057000         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                 
057100         MOVE NEJ TO INDATA-SW                                            
057200         IF MED-IDMFSFEL = SPACE                                          
057300            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
057400         END-IF                                                           
057500       END-IF                                                             
057600     END-IF                                                               
057700                                                                          
057800     IF INDATA-OK                                                         
057900       MOVE SPACE                TO PRT-IDPRTLST                          
058000       MOVE '6F'                 TO PRT-IDPRTLST (1:2)                    
058100       MOVE WS-ADINLOMR-PRT      TO PRT-IDPRTLST (3:6)                    
058200       MOVE 1                    TO PRT-KDCALL                            
058300       CALL W006PRT USING PRT-W006PRT                                     
058400       IF PRT-KDSVAR = 'F'                                                
058500*        -- SAKNAS ENLIGT W006PRT                                         
058600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                 
058700         MOVE NEJ TO INDATA-SW                                            
058800         IF MED-IDMFSFEL = SPACE                                          
058900            MOVE ERR-772-PRT-FEL TO MED-IDMFSFEL                          
059000         END-IF                                                           
059100       ELSE                                                               
059200*        -- OK                                                            
059300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-ATTR               
059400         MOVE PRT-BEPRTLST TO MOD-TEMFSFEL                                
059500       END-IF                                                             
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900 GC-KOLLA-BILDRADER SECTION.                                              
060000                                                                          
060100*    KONTROLL AV BILDRADER (IDLEVNR-KOLLI & IDOKOLLI)                     
060200                                                                          
060300     MOVE NEJ                    TO SW-BILDRADER-INMATADE                 
060400     MOVE +1                     TO INDX                                  
060500                                                                          
060600     PERFORM UNTIL (INDX > MAX-INDX)                                      
060700                                                                          
060800       IF  MID-IDLEVNR-KOLLI (INDX) NOT = ALL '+'                         
060900       OR  MID-IDOKOLLI      (INDX) NOT = ALL '+'                         
061000         MOVE JA                 TO SW-BILDRADER-INMATADE                 
061100         MOVE JA                 TO SW-OK-BILDRAD                         
061200                                                                          
061300         IF  MID-IDLEVNR-KOLLI (INDX) = SPACE                             
061500           MOVE MFS-ALFA-FAELT-FEL                                        
061510                                 TO MOD-IDLEVNR-KOLLI-ATTR (INDX)         
061600           MOVE NEJ              TO INDATA-SW                             
061700           MOVE NEJ              TO SW-OK-BILDRAD                         
061800           IF MED-IDMFSFEL = SPACE                                        
061900              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
062000           END-IF                                                         
062100         ELSE                                                             
062200           MOVE MFS-ALFA-FAELT-RAETT                                      
062300                                 TO MOD-IDLEVNR-KOLLI-ATTR (INDX)         
062400         END-IF                                                           
062500                                                                          
062600         IF  MID-IDOKOLLI (INDX) NOT NUMERIC                              
062700         OR  MID-IDOKOLLI (INDX) = ZERO                                   
062800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDOKOLLI-ATTR (INDX)             
062900           MOVE NEJ              TO INDATA-SW                             
063000           MOVE NEJ              TO SW-OK-BILDRAD                         
063100           IF MED-IDMFSFEL = SPACE                                        
063200              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
063300           END-IF                                                         
063400         ELSE                                                             
063500           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDOKOLLI-ATTR (INDX)           
063600         END-IF                                                           
063700                                                                          
063800         IF  SW-OK-BILDRAD = JA                                           
063900           PERFORM GCA-GU-INLD-SEQC                                       
064000                                                                          
064100           IF  SEGMENT-FINNS                                              
064200             PERFORM GCB-GU-INLA-RAD                                      
064300                                                                          
064400             IF  INLA-RAD-FLDIVKLI = NEJ                                  
064500                                                                          
064600               IF  INLA-RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'            
064700                 CONTINUE                                                 
064800               ELSE                                                       
064900                 MOVE MFS-ALFA-FAELT-FEL                                  
065000                                 TO MOD-IDLEVNR-KOLLI-ATTR (INDX)         
065100                 MOVE MFS-NUM-FAELT-FEL                                   
065200                                 TO MOD-IDOKOLLI-ATTR (INDX)              
065300                 MOVE NEJ        TO INDATA-SW                             
065400                 IF MED-IDMFSFEL = SPACE                                  
065500                   MOVE ERR-007-OTILLATEN-UPD TO MED-IDMFSFEL             
065600                 END-IF                                                   
065700               END-IF                                                     
065800             ELSE                                                         
065900*              -- FLDIVKLI=JA:  SÖK TILLS GILTIG KDINLSTA HITTAS.         
066000               MOVE NEJ          TO SW-KDINLSTA-GILTIG                    
066100               PERFORM UNTIL ((NOT SEGMENT-FINNS)                         
066200                          OR  SW-KDINLSTA-GILTIG = JA)                    
066300                                                                          
066400                 IF  INLA-RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'          
066500                   MOVE JA       TO SW-KDINLSTA-GILTIG                    
066600                 ELSE                                                     
066700                   PERFORM IMS-GN-INLD-SEQC                               
066800                                                                          
066900                   IF  SEGMENT-FINNS                                      
067000                     PERFORM GCB-GU-INLA-RAD                              
067100                   END-IF                                                 
067200                 END-IF                                                   
067300               END-PERFORM                                                
067400                                                                          
067500               IF SW-KDINLSTA-GILTIG = NEJ                                
067600                 MOVE MFS-ALFA-FAELT-FEL                                  
067700                                 TO MOD-IDLEVNR-KOLLI-ATTR (INDX)         
067800                 MOVE MFS-NUM-FAELT-FEL                                   
067900                                 TO MOD-IDOKOLLI-ATTR (INDX)              
068000                 MOVE NEJ        TO INDATA-SW                             
068100                 IF MED-IDMFSFEL = SPACE                                  
068200                   MOVE ERR-007-OTILLATEN-UPD TO MED-IDMFSFEL             
068300                 END-IF                                                   
068400               END-IF                                                     
068500             END-IF                                                       
068600           ELSE                                                           
068700*            -- INGEN TRÄFF MED KDMFSFOR,IDLEVNR-KOLLI,IDOKOLLI.          
068800             MOVE MFS-ALFA-FAELT-FEL                                      
068900                                 TO MOD-IDLEVNR-KOLLI-ATTR (INDX)         
069000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDOKOLLI-ATTR (INDX)           
069100             MOVE NEJ            TO INDATA-SW                             
069200             IF MED-IDMFSFEL = SPACE                                      
069300                MOVE ERR-010-NOT-IN-REG TO MED-IDMFSFEL                   
069400             END-IF                                                       
069500           END-IF                                                         
069600         END-IF                                                           
069700       END-IF                                                             
069800                                                                          
069900       ADD +1 TO INDX                                                     
070000     END-PERFORM                                                          
070100                                                                          
070200     IF  SW-BILDRADER-INMATADE = NEJ                                      
070300       MOVE NEJ              TO INDATA-SW                                 
070400       IF MED-IDMFSFEL = SPACE                                            
070500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
070600       END-IF                                                             
070700     END-IF                                                               
070800                                                                          
070900     .                                                                    
071000     EJECT                                                                
071100 GCA-GU-INLD-SEQC SECTION.                                                
071200                                                                          
071300     MOVE LOW-VALUE              TO W-W6D1C1KY-MIN-X                      
071400     MOVE HIGH-VALUE             TO W-W6D1C1KY-MAX-X                      
071500     MOVE MID-IDLEVNR-KOLLI (INDX)                                        
071600                                 TO W-W6D1C1KY-MIN-IDLEVNR-KOLLI          
071700                                    W-W6D1C1KY-MAX-IDLEVNR-KOLLI          
071800     MOVE MID-IDOKOLLI (INDX)    TO W-W6D1C1KY-MIN-IDOKOLLI               
071900                                    W-W6D1C1KY-MAX-IDOKOLLI               
072000                                                                          
072100     PERFORM IMS-GU-INLD-SEQC                                             
072200     .                                                                    
072300     EJECT                                                                
072400 GCB-GU-INLA-RAD SECTION.                                                 
072500                                                                          
072600     MOVE INLD-SEQC-IDLEVNR      TO W-W6D101KY-IDLEVNR                    
072700     MOVE INLD-SEQC-IDFS         TO W-W6D101KY-IDFS                       
072710     MOVE W-IDDC                 TO W-W6D101KY-IDDC                       
072800     MOVE INLD-SEQC-TIAVIDAT     TO W-W6D101KY-TIAVIDAT                   
072900     MOVE INLD-SEQC-IDRADNR-INL  TO W-IDRADNR-INL                         
073000     MOVE INLD-SEQC-IDRADNR      TO W-IDRADNR                             
073100                                                                          
073200     PERFORM IMS-GU-INLA-RAD                                              
073300     .                                                                    
073400     EJECT                                                                
073500 H-UPPDATERA SECTION.                                                     
073600                                                                          
073700     PERFORM MFS-FORM-ATTR                                                
073800                                                                          
073900     MOVE NEJ                    TO SW-UPPDAT-LOPA                        
074000     MOVE JA                     TO SW-1A-6191                            
074100     PERFORM S11-INIT-6191                                                
074200     PERFORM S12-INIT-6194                                                
074300                                                                          
074400     MOVE +1                     TO INDX                                  
074500                                                                          
074600     PERFORM UNTIL (INDX > MAX-INDX)                                      
074700                                                                          
074800       IF  MID-IDLEVNR-KOLLI (INDX) NOT = ALL '+'                         
074900       OR  MID-IDOKOLLI      (INDX) NOT = ALL '+'                         
075000                                                                          
075100         PERFORM HA-GU-INLD-SEQC-OK                                       
075200         PERFORM HB-LAES-INLA                                             
075300         MOVE NEJ                TO SW-6194-REDIGERAD                     
075400                                    DIVERSEKOLLI-SW                       
075500                                                                          
075600         IF  INLA-RAD-FLDIVKLI = NEJ                                      
075700                                                                          
075800           PERFORM HC-UPPDAT-RAD                                          
075900         ELSE                                                             
076000*          -- FLDIVKLI=JA:  SÖK TILLS GILTIG KDINLSTA HITTAS.             
076100           PERFORM UNTIL (NOT SEGMENT-FINNS)                              
076200                                                                          
076300             IF  INLA-RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'              
076400               PERFORM HC-UPPDAT-RAD                                      
076500             END-IF                                                       
076600                                                                          
076700             PERFORM IMS-GN-INLD-SEQC                                     
076800             IF  SEGMENT-FINNS                                            
076900               PERFORM HB-LAES-INLA                                       
077000               MOVE JA TO DIVERSEKOLLI-SW                                 
077100             END-IF                                                       
077200           END-PERFORM                                                    
077300         END-IF                                                           
077400       END-IF                                                             
077500                                                                          
077600       ADD +1 TO INDX                                                     
077700     END-PERFORM                                                          
077800                                                                          
077900     IF  SW-UPPDAT-LOPA = JA                                              
078000       PERFORM IMS-REPL-LOPA                                              
078100     END-IF                                                               
078200                                                                          
078300     PERFORM HD-SKRIV-BACKGR-TR                                           
078400                                                                          
078500     MOVE INF-FLAG-PRINTED TO MED-IDMFSINF                                
078600     CALL WMEDKONV USING MED-WMEDAREA                                     
078700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
078800     PERFORM MFS-RENSA-FAELT-IN                                           
078900     .                                                                    
079000     EJECT                                                                
079100 HA-GU-INLD-SEQC-OK SECTION.                                              
079200                                                                          
079300     MOVE LOW-VALUE              TO W-W6D1C1KY-MIN-X                      
079400     MOVE HIGH-VALUE             TO W-W6D1C1KY-MAX-X                      
079500     MOVE MID-IDLEVNR-KOLLI (INDX)                                        
079600                                 TO W-W6D1C1KY-MIN-IDLEVNR-KOLLI          
079700                                    W-W6D1C1KY-MAX-IDLEVNR-KOLLI          
079800     MOVE MID-IDOKOLLI (INDX)    TO W-W6D1C1KY-MIN-IDOKOLLI               
079900                                    W-W6D1C1KY-MAX-IDOKOLLI               
080000                                                                          
080100     PERFORM IMS-GU-INLD-SEQC-OK                                          
080200     .                                                                    
080300     EJECT                                                                
080400 HB-LAES-INLA SECTION.                                                    
080500                                                                          
080600     MOVE INLD-SEQC-IDLEVNR      TO W-W6D101KY-IDLEVNR                    
080700     MOVE INLD-SEQC-IDFS         TO W-W6D101KY-IDFS                       
080710     MOVE W-IDDC                 TO W-W6D101KY-IDDC                       
080800     MOVE INLD-SEQC-TIAVIDAT     TO W-W6D101KY-TIAVIDAT                   
080900     MOVE INLD-SEQC-IDRADNR-INL  TO W-IDRADNR-INL                         
081000     MOVE INLD-SEQC-IDRADNR      TO W-IDRADNR                             
081100                                                                          
081200     PERFORM IMS-GU-INLA-INL                                              
081300     PERFORM IMS-GNP-INLA-ART                                             
081400     PERFORM IMS-GHNP-INLA-RAD                                            
081500     .                                                                    
081600     EJECT                                                                
081700 HC-UPPDAT-RAD SECTION.                                                   
081800                                                                          
081900     PERFORM HCA-SPARA-GML-RAD                                            
082000                                                                          
082100     PERFORM HCB-REDIG-INLA-RAD                                           
082200                                                                          
082300     IF  SW-UPPDAT-INLA-RAD = JA                                          
082400       PERFORM IMS-REPL-INLA-RAD                                          
082500     END-IF                                                               
082600                                                                          
082700     IF  WS-GML-KDINLSTA = 'SAK'                                          
082800       PERFORM HCD-TRANS-6191                                             
082900     END-IF                                                               
083000                                                                          
083100     IF  SW-6194-REDIGERAD = NEJ                                          
083200       PERFORM HCE-TRANS-6194                                             
083300       MOVE JA                   TO SW-6194-REDIGERAD                     
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 HCA-SPARA-GML-RAD SECTION.                                               
083800                                                                          
083900     MOVE INLA-RAD-ADINLOMR      TO WS-GML-ADINLOMR                       
084000     MOVE INLA-RAD-ADINLOMR-NXT  TO WS-GML-ADINLOMR-NXT                   
084100     MOVE INLA-RAD-KDINLSTA      TO WS-GML-KDINLSTA                       
084200     MOVE INLA-RAD-KVINLART      TO WS-GML-KVINLART                       
084300     .                                                                    
084400     EJECT                                                                
084500 HCB-REDIG-INLA-RAD SECTION.                                              
084600                                                                          
084700     MOVE NEJ                    TO SW-UPPDAT-INLA-RAD                    
084800                                                                          
084900     IF  INLA-RAD-KDINLSTA = 'SAK'                                        
085000       MOVE SPACE                TO INLA-RAD-KDINLSTA                     
085100       MOVE JA                   TO SW-UPPDAT-INLA-RAD                    
085200     END-IF                                                               
085300                                                                          
085400     IF  INLA-RAD-IDLEVNR-KOLLI > '99299'                                 
085500     AND INLA-RAD-IDLEVNR-KOLLI < '99600'                                 
085510       CONTINUE                                                           
085520     ELSE                                                                 
085600       MOVE JA                   TO SW-UPPDAT-INLA-RAD                    
085700       MOVE PLAA-6006-IDLEVNR    TO INLA-RAD-IDLEVNR-KOLLI                
085800                                                                          
085900       IF  SW-UPPDAT-LOPA = NEJ                                           
086000         PERFORM IMS-GHU-LOPA-6018                                        
086100         MOVE JA                 TO SW-UPPDAT-LOPA                        
086200       END-IF                                                             
086300                                                                          
086400       IF NOT DIVERSEKOLLI                                                
086500         ADD 1                       TO LOPA-6018-IDOKOLLI                
086600       END-IF                                                             
086700                                                                          
086800       MOVE LOPA-6018-IDOKOLLI     TO INLA-RAD-IDOKOLLI                   
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 HCD-TRANS-6191 SECTION.                                                  
087300                                                                          
087400     IF  IX-6191 >= K-MAX-6191-KVPOST                                     
087500       PERFORM S01-P-TO-P-6191                                            
087600       PERFORM S11-INIT-6191                                              
087700     END-IF                                                               
087800                                                                          
087900     ADD +1                      TO IX-6191                               
088000     MOVE IX-6191                TO MOD6191-MID-KVPOST                    
088100                                                                          
088200     MOVE INLA-ART-IDLOPNRM      TO MOD6191-MID-IDLOPNRM (IX-6191)        
088300     MOVE INLA-RAD-IDRADNR       TO MOD6191-MID-IDRADNR  (IX-6191)        
088400     MOVE INLA-RAD-KDINLPRIO     TO MOD6191-MID-KDINLPRIO                 
088500                                                         (IX-6191)        
088600     MOVE INLA-ART-PRARTSTD      TO MOD6191-MID-PRARTSTD (IX-6191)        
088700                                                                          
088800     MOVE WS-GML-ADINLOMR        TO MOD6191-MID-ADINLOMR-OLD              
088900                                                         (IX-6191)        
089000     MOVE WS-GML-ADINLOMR-NXT    TO MOD6191-MID-ADINLOMR-NXT-OLD          
089100                                                         (IX-6191)        
089200     MOVE WS-GML-KDINLSTA        TO MOD6191-MID-KDINLSTA-OLD              
089300                                                         (IX-6191)        
089400     MOVE WS-GML-KVINLART        TO MOD6191-MID-KVINLART-OLD              
089500                                                         (IX-6191)        
089600                                                                          
089700     MOVE INLA-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-NEW              
089800                                                         (IX-6191)        
089900     MOVE INLA-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-NEW          
090000                                                         (IX-6191)        
090100     MOVE INLA-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-NEW              
090200                                                         (IX-6191)        
090300     MOVE INLA-RAD-KVINLART      TO MOD6191-MID-KVINLART-NEW              
090400                                                         (IX-6191)        
090500     .                                                                    
090600     EJECT                                                                
090700 HCE-TRANS-6194 SECTION.                                                  
090800                                                                          
090900     ADD +1                      TO IX-6194                               
091000     MOVE IX-6194                TO MOD6194-MID-KVPOST                    
091100                                                                          
091200     IF  INLA-RAD-FLDIVKLI = NEJ                                          
091300       MOVE INLA-ART-IDARTNR     TO MOD6194-MID-IDARTNR  (IX-6194)        
091400       MOVE INLA-ART-IDLOPNRM    TO MOD6194-MID-IDLOPNRM (IX-6194)        
091500       MOVE INLA-ART-KDSORT      TO MOD6194-MID-KDSORT   (IX-6194)        
091600       MOVE INLA-RAD-KVINLART    TO MOD6194-MID-KVINLART (IX-6194)        
091700       COMPUTE WS-VKKOLLIN-GRAM                                           
091800           = INLA-RAD-KVINLART * INLA-ART-VKART                           
091900       END-COMPUTE                                                        
092000       COMPUTE MOD6194-MID-VKKOLLIN (IX-6194) ROUNDED                     
092100           = WS-VKKOLLIN-GRAM / 1000                                      
092200       END-COMPUTE                                                        
092300       MOVE INLA-ART-ADLAGOMR    TO MOD6194-MID-ADLAGOMR (IX-6194)        
092400       MOVE INLA-ART-ADGANG      TO MOD6194-MID-ADGANG   (IX-6194)        
092500       MOVE INLA-ART-ADPLATS     TO MOD6194-MID-ADPLATS  (IX-6194)        
092510       MOVE INLA-ART-BEFT        TO MOD6194-MID-BEFT     (IX-6194)        
092600     ELSE                                                                 
092700       MOVE ZERO                 TO MOD6194-MID-IDARTNR  (IX-6194)        
092800       MOVE ZERO                 TO MOD6194-MID-IDLOPNRM (IX-6194)        
092900       MOVE ZERO                 TO MOD6194-MID-KVINLART (IX-6194)        
093000       MOVE ZERO                 TO MOD6194-MID-VKKOLLIN (IX-6194)        
093100       MOVE INLA-ART-ADLAGOMR    TO MOD6194-MID-ADLAGOMR (IX-6194)        
093200       MOVE ZERO                 TO MOD6194-MID-ADGANG   (IX-6194)        
093300       MOVE ZERO                 TO MOD6194-MID-ADPLATS  (IX-6194)        
093310       MOVE ZERO                 TO MOD6194-MID-BEFT     (IX-6194)        
093400       MOVE SPACE                TO MOD6194-MID-KDSORT   (IX-6194)        
093500     END-IF                                                               
093600                                                                          
093700     MOVE INLA-RAD-IDLEVNR-KOLLI TO MOD6194-MID-IDLEVNR-KOLLI             
093800                                                         (IX-6194)        
093900     MOVE INLA-RAD-IDOKOLLI      TO MOD6194-MID-IDOKOLLI (IX-6194)        
094000     MOVE INLA-INL-TIINLMOT      TO MOD6194-MID-TIINLMOT (IX-6194)        
094100     MOVE ZERO                   TO MOD6194-MID-VKKOLLIB (IX-6194)        
094200     .                                                                    
094300     EJECT                                                                
094400 HD-SKRIV-BACKGR-TR SECTION.                                              
094500                                                                          
094600     IF  IX-6191 > ZERO                                                   
094700       PERFORM S01-P-TO-P-6191                                            
094800     END-IF                                                               
094900                                                                          
095000     IF  IX-6194 > ZERO                                                   
095100       PERFORM S02-P-TO-P-6194                                            
095200     END-IF                                                               
095300     .                                                                    
095400     EJECT                                                                
095500 S01-P-TO-P-6191 SECTION.                                                 
095600                                                                          
095700     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
095800                               + 17 + (MOD6191-MID-KVPOST * 64)           
095900                                                                          
096000     MOVE 'W6T191X '           TO P-TO-P-KDTRANS                          
096100     MOVE '6103'               TO P-TO-P-IDTRANS                          
096200     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
096300                                                                          
096400     MOVE MOD6191-MID-W6I19101 TO P-TO-P-DATA                             
096500                                                                          
096600     IF  SW-1A-6191 = JA                                                  
096700       PERFORM IMS-ISRT-ALT-MSG-6191                                      
096800       MOVE NEJ                TO SW-1A-6191                              
096900     ELSE                                                                 
097000       PERFORM IMS-PURG-ALT-MSG-6191                                      
097100     END-IF                                                               
097200     .                                                                    
097300     SKIP3                                                                
097400 S02-P-TO-P-6194 SECTION.                                                 
097500                                                                          
097600     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
097700                               + 23 + (MOD6194-MID-KVPOST * 67)           
097800                                                                          
097900     MOVE 'W6T194X '           TO P-TO-P-KDTRANS                          
098000     MOVE '6103'               TO P-TO-P-IDTRANS                          
098100     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
098200                                                                          
098300     MOVE MOD6194-MID-W6I19401 TO P-TO-P-DATA                             
098400                                                                          
098500     PERFORM IMS-ISRT-ALT-MSG-6194                                        
098600     .                                                                    
098700     EJECT                                                                
098800 S11-INIT-6191 SECTION.                                                   
098900                                                                          
099000     MOVE SPACE                  TO MOD6191-MID-W6I19101                  
099100     MOVE IDPGM                  TO MOD6191-MID-IDPGM                     
099110     MOVE DCS-IDDC               TO MOD6191-MID-IDDC                      
099200     MOVE ZERO                   TO IX-6191                               
099300     .                                                                    
099400     SKIP3                                                                
099500 S12-INIT-6194 SECTION.                                                   
099600                                                                          
099700     MOVE SPACE                  TO MOD6194-MID-W6I19401                  
099800     MOVE PRT-IDPRTLST           TO MOD6194-MID-IDPRTLST                  
099900     MOVE IDPGM                  TO MOD6194-MID-IDPGM                     
100000     MOVE ZERO                   TO IX-6194                               
100100     .                                                                    
100200     EJECT                                                                
100300 MFS-RENSA-FAELT-IN SECTION.                                              
100400                                                                          
100500*    --- ALLA INDATA-FÄLT                                                 
100600     MOVE +1 TO INDX                                                      
100700     PERFORM UNTIL INDX > MAX-INDX                                        
100800       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI (INDX)                   
100900                               MOD-IDOKOLLI      (INDX)                   
101000       ADD +1 TO INDX                                                     
101100     END-PERFORM                                                          
101200     .                                                                    
101300     SKIP2                                                                
101400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
101500                                                                          
101600*    --- ALLA INDATA-FÄLT                                                 
101700     MOVE +1 TO INDX                                                      
101800     PERFORM UNTIL INDX > MAX-INDX                                        
101900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-KOLLI (INDX)                 
102000                                 MOD-IDOKOLLI      (INDX)                 
102100       ADD +1 TO INDX                                                     
102200     END-PERFORM                                                          
102300     .                                                                    
102400     EJECT                                                                
102500 MFS-FORM-ATTR SECTION.                                                   
102600                                                                          
102700*    --- ALLA FÄLT MED ATTRIBUT                                           
102800     MOVE MFS-FORMATETS-ATTR TO MOD-ADINLOMR-PRT-ATTR                     
102900     MOVE +1 TO INDX                                                      
103000     PERFORM UNTIL INDX > MAX-INDX                                        
103100       MOVE MFS-FORMATETS-ATTR TO MOD-IDLEVNR-KOLLI-ATTR (INDX)           
103200                                  MOD-IDOKOLLI-ATTR (INDX)                
103300       ADD +1 TO INDX                                                     
103400     END-PERFORM                                                          
103500     .                                                                    
103600     SKIP2                                                                
103700 MFS-LAES-IN-IGEN SECTION.                                                
103800                                                                          
103900*    --- ALLA INDATA-FÄLT                                                 
104000     MOVE +1 TO INDX                                                      
104100     PERFORM UNTIL INDX > MAX-INDX                                        
104200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-KOLLI-ATTR (INDX)        
104300                                     MOD-IDOKOLLI-ATTR (INDX)             
104400       ADD +1 TO INDX                                                     
104500     END-PERFORM                                                          
104600     .                                                                    
104700     EJECT                                                                
104800* --- IMS SEKTIONER ---                                                   
105000 IMS-GET-MSG SECTION.                                                     
105100                                                                          
105200     MOVE '  QC' TO GODK-STATUSKODER                                      
105300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
105400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700     SKIP3                                                                
105800 IMS-INSERT-MSG SECTION.                                                  
105900                                                                          
105910     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
105920       MOVE '0' TO MFS-KDHUVOMR                                           
106200     END-IF                                                               
106300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
106400     MOVE SPACE TO GODK-STATUSKODER                                       
106500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
106600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106700     PERFORM IMS-STATUSKONTROLL                                           
106800     .                                                                    
106900     EJECT                                                                
107000 IMS-ISRT-ALT-MSG-6191 SECTION.                                           
107100                                                                          
107200     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
107300     MOVE    '  '             TO    GODK-STATUSKODER                      
107400     CALL    CBLTDLI          USING ISRT 6191-PCB P-TO-P-SW               
107500     MOVE    6191-STATUS-CODE TO    STATUS-WS                             
107600     PERFORM IMS-STATUSKONTROLL                                           
107700     .                                                                    
107800     SKIP3                                                                
107900 IMS-PURG-ALT-MSG-6191 SECTION.                                           
108000                                                                          
108100     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
108200     MOVE    '  '             TO    GODK-STATUSKODER                      
108300     CALL    CBLTDLI          USING PURG 6191-PCB P-TO-P-SW               
108400     MOVE    6191-STATUS-CODE TO    STATUS-WS                             
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     .                                                                    
108700     SKIP3                                                                
108800 IMS-ISRT-ALT-MSG-6194 SECTION.                                           
108900                                                                          
109000     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
109100     MOVE    '  '             TO    GODK-STATUSKODER                      
109200     CALL    CBLTDLI          USING ISRT 6194-PCB P-TO-P-SW               
109300     MOVE    6194-STATUS-CODE TO    STATUS-WS                             
109400     PERFORM IMS-STATUSKONTROLL                                           
109500     .                                                                    
109600     EJECT                                                                
109700 IMS-GU-INLD-SEQC SECTION.                                                
109800     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
109900                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X                        
109910                    '&IDDC     =' W-IDDC-X ')'                            
110000          DELIMITED BY SIZE INTO SSA1                                     
110100     MOVE '  GE' TO GODK-STATUSKODER                                      
110200     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
110300     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
110400     PERFORM IMS-STATUSKONTROLL                                           
110500     .                                                                    
110600     SKIP3                                                                
110700 IMS-GU-INLD-SEQC-OK SECTION.                                             
110800     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
110900                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X                        
110910                    '&IDDC     =' W-IDDC-X ')'                            
111000          DELIMITED BY SIZE INTO SSA1                                     
111100     MOVE '  ' TO GODK-STATUSKODER                                        
111200     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA SSA1                      
111300     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
111400     PERFORM IMS-STATUSKONTROLL                                           
111500     .                                                                    
111600     SKIP3                                                                
111700 IMS-GN-INLD-SEQC SECTION.                                                
111800     STRING 'W6INLD01(W6D1C1KY>=' W-W6D1C1KY-MIN-X                        
111900                    '&W6D1C1KY<=' W-W6D1C1KY-MAX-X                        
111910                    '&IDDC     =' W-IDDC-X ')'                            
112000          DELIMITED BY SIZE INTO SSA1                                     
112100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
112200     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA SSA1                      
112300     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
112400     PERFORM IMS-STATUSKONTROLL                                           
112500     .                                                                    
112600     EJECT                                                                
112700 IMS-GU-INLA-INL SECTION.                                                 
112800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
112900          DELIMITED BY SIZE INTO SSA1                                     
113000     MOVE '  ' TO GODK-STATUSKODER                                        
113100     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA3 SSA1                     
113200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     SKIP3                                                                
113600 IMS-GNP-INLA-ART SECTION.                                                
113700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
113800          DELIMITED BY SIZE INTO SSA1                                     
113900     MOVE '  ' TO GODK-STATUSKODER                                        
114000     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
114100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
114200     PERFORM IMS-STATUSKONTROLL                                           
114300     .                                                                    
114400     EJECT                                                                
114500 IMS-GU-INLA-RAD SECTION.                                                 
114600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
114700          DELIMITED BY SIZE INTO SSA1                                     
114800     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
114900          DELIMITED BY SIZE INTO SSA2                                     
115000     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
115100          DELIMITED BY SIZE INTO SSA3                                     
115200     MOVE '  ' TO GODK-STATUSKODER                                        
115300     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3           
115400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
115500     PERFORM IMS-STATUSKONTROLL                                           
115600     .                                                                    
115700     SKIP3                                                                
115800 IMS-GHNP-INLA-RAD SECTION.                                               
115900     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
116000          DELIMITED BY SIZE INTO SSA1                                     
116100     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
116200          DELIMITED BY SIZE INTO SSA2                                     
116300     MOVE '  ' TO GODK-STATUSKODER                                        
116400     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA2 SSA1 SSA2              
116500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     .                                                                    
116800     SKIP3                                                                
116900 IMS-REPL-INLA-RAD SECTION.                                               
117000                                                                          
117100     MOVE '  ' TO GODK-STATUSKODER                                        
117200     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA2                        
117300     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
117400     PERFORM IMS-STATUSKONTROLL                                           
117500     .                                                                    
117600     EJECT                                                                
117700 IMS-GU-PLAA-6006 SECTION.                                                
117800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
117900          DELIMITED BY SIZE INTO SSA1                                     
118000     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
118100          DELIMITED BY SIZE INTO SSA2                                     
118200     MOVE '  GE' TO GODK-STATUSKODER                                      
118300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
118400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-GHU-LOPA-6018 SECTION.                                               
118900     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
119000          DELIMITED BY SIZE INTO SSA1                                     
119100     MOVE 'W6LOPA11 ' TO SSA2                                             
119200     MOVE '  ' TO GODK-STATUSKODER                                        
119300     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA5 SSA1 SSA2               
119400     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
119500     PERFORM IMS-STATUSKONTROLL                                           
119600     .                                                                    
119700     SKIP3                                                                
119800 IMS-REPL-LOPA SECTION.                                                   
119900     MOVE '  ' TO GODK-STATUSKODER                                        
120000     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA5                        
120100     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSKONTROLL                                           
120300     .                                                                    
120400     EJECT                                                                
120410 IMS-GU-WDB601    SECTION.                                                
120420     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
120430          DELIMITED BY SIZE INTO SSA1                                     
120440     MOVE '  GE' TO GODK-STATUSKODER                                      
120450     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
120460     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
120470     PERFORM IMS-STATUSKONTROLL                                           
120480     IF SEGMENT-SAKNAS                                                    
120490         MOVE SPACE TO DCS-KDDC                                           
120491     END-IF                                                               
120492     .                                                                    
120500 IMS-STATUSKONTROLL SECTION.                                              
120600                                                                          
120700     SET STATUS-IX TO 1                                                   
120800     SEARCH GODK-STATUS                                                   
120900       AT END                                                             
121000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
121100         DELIMITED BY SIZE INTO FELTEXT                                   
121200         CALL FELLOG                                                      
121300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
121400         CONTINUE                                                         
121500     END-SEARCH                                                           
121600     .                                                                    
