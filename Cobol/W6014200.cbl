000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014200.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/08/13.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RAPPORTERA RADER SOM INLAGDA EFTER INLÄGGNINGSLISTA.             
001100*        OM MAN RAPPORTERAR LISTA KLAR OCH LISTAN INNEHÅLLER              
001200*        FLER ÄN 20 RADER, STARTA PROGRAMMET OM SIG SJÄLV.                
001300*                                                                         
001400*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
001500*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001600*    SUB PROGRAMMET W611PMRK UPDATERAR  W6INLA (W6D1)                     
001700*                            LÄSER      W6PLAA (W6G1)                     
001800*    SUB PROGRAMMET W611STYR LÄSER      W6HANA (W6G1)                     
001810*                                       W6PLAA (W6G1)                     
001900*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W6T142                                              
002300*        MID:         W6I14201                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W6O14201                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6014200'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004200                                                                          
004300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004500 77  6191-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-6191-IX                 PIC S9(9)  VALUE +24   COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1101 COMP SYNC.        
004900 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005000                                                                          
005100 77  W-CHKP                      PIC S9(3)  VALUE ZERO COMP-3.            
005200 77  MAX-CHKP                    PIC S9(3)  VALUE +20  COMP-3.            
005300                                                                          
005400 77  W-KVINLART-UPD              PIC S9(7)  VALUE ZERO  COMP-3.           
005500 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
005610 77  SPAR-KDRT                   PIC 9(2)    VALUE ZERO.                  
005620 77  SPAR-ADLAGOMR               PIC 9(2)    VALUE ZERO.                  
005630 77  SPAR-ADGANG                 PIC 9(2)    VALUE ZERO.                  
005640 77  SPAR-ADPLATS                PIC 9(5)    VALUE ZERO.                  
005700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800 77  WS-IDILIST                  PIC X(5)   VALUE SPACE.                  
005900                                                                          
005901 77  WS-IDILIRAD                 PIC X(5)   VALUE SPACE.                  
005902                                                                          
005910 77  UPPDATERING-SW              PIC X       VALUE 'N'.                   
005920     88  UPPDATERING-UTF                     VALUE 'J'.                   
005930                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  RAD-SW                      PIC X       VALUE 'J'.                   
006900     88  RAD-SAKNAS                          VALUE 'N'.                   
007000                                                                          
007010 77  LISTA-SW                    PIC X       VALUE 'J'.                   
007020     88  LISTA-SAKNAS                        VALUE 'N'.                   
007030                                                                          
007100 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
007200     88  FOERSTA-6191                        VALUE 'J'.                   
007300                                                                          
007400 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
007500     88  OMSTART                             VALUE 'J'.                   
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  EGEN-MID                            VALUE '6142'.                
007900     88  GODK-MID                            VALUE '6141'.                
008000     88  HELP-MID                            VALUE '0551'.                
008100     EJECT                                                                
008110*      --- VALID IDDC CODES                                               
008120*                                                                         
008130*01    -COPY WWDCKONS                                                     
008140       EJECT                                                              
008200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
008800     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
008900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200*01 -COPY WMEDAREA                                                        
009300     SKIP3                                                                
009400 01  MESSAGE-CODES.                                                       
009500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009610     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009700     03  ERR-MISSING             PIC X(3)    VALUE '010'.                 
009710     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009810     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
009820     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010010     03  ERR-KIT-MARKED-CASE     PIC X(3)    VALUE '185'.                 
010020     03  ERR-QUALITY             PIC X(3)    VALUE '189'.                 
010100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010110     03  ERR-PLACE-MISSING       PIC X(3)    VALUE '764'.                 
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM W611STYR                              
010400*01 -COPY W611STYR                                                        
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL SUBPROGRAM W611PMRK                              
010700*01 -COPY W611PMRK                                                        
010800     EJECT                                                                
010900*    --- SPAR AREA  FÖR INLA21                                            
011000*01  -COPY W6D121   -PRE SPAR-                                            
011100     EJECT                                                                
011200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011500     SKIP3                                                                
011600*01  MID -COPY W6I14201                                                   
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011900     SKIP3                                                                
012000*01  -COPY WMSGAREA                                                       
012100     EJECT                                                                
012200     03  MOD REDEFINES MSG-AREA.                                          
012300*      05  -COPY W6O14201                                                 
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012600     SKIP3                                                                
012700*01  -COPY WMFSAREA                                                       
012800 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
012900     SKIP3                                                                
013000 01  KOM-MSG-IO-AREA.                                                     
013100*03  -COPY WMSGKOM                                                        
013200     EJECT                                                                
013300*01  -COPY WMSGSNUF -PRE P-TO-P-                                          
014200     EJECT                                                                
014300 01      FILLER                  PIC X(24)   VALUE                        
014400                                 'MOD6191-MID-W6I19101'.                  
014500     SKIP2                                                                
014600     -COPY W6I19101 -PRE MOD6191-                                         
014700     EJECT                                                                
014800 01      FILLER                  PIC X(24)   VALUE                        
014900                                 'MOD6193-MID-W6I19301'.                  
015000     SKIP2                                                                
015100     -COPY W6I19301 -PRE MOD6193-                                         
015200     EJECT                                                                
015300     EJECT                                                                
015400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015500*                                                                         
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015800     SKIP3                                                                
015900 01  NYCKLAR-TILL-DLI.                                                    
016000     03  W-W6D1DSEQ-X.                                                    
016100        05  W-D1DSEQ-IDILIST         PIC  9(5) VALUE ZERO.                
016200        05  W-D1DSEQ-IDILIRAD        PIC S9(5) COMP-3 VALUE ZERO.         
016300                                                                          
016400     03  W-W6D1DSEQ-MIN-X.                                                
016500        05  W-D1DSEQ-IDILIST-MIN     PIC  9(5) VALUE ZERO.                
016600        05  W-D1DSEQ-IDILIRAD-MIN    PIC S9(5) COMP-3 VALUE ZERO.         
016700                                                                          
016800     03  W-W6D1DSEQ-MAX-X.                                                
016900        05  W-D1DSEQ-IDILIST-MAX     PIC  9(5) VALUE ZERO.                
017000        05  W-D1DSEQ-IDILIRAD-MAX    PIC S9(5) COMP-3 VALUE 99999.        
017100                                                                          
017200     03  W-W6D1D1KY-MIN-X.                                                
017300        05  W-D1D1KY-IDILIST-MIN     PIC  9(5) VALUE ZERO.                
017400        05  W-D1D1KY-IDILIRAD-MIN    PIC S9(5) COMP-3 VALUE ZERO.         
017500        05  W-D1D1KY-IDRADNR-INL-MIN PIC S9(5) COMP-3 VALUE ZERO.         
017600        05  W-D1D1KY-IDDC-MIN        PIC X(2)  VALUE '11'.                
017610        05  W-D1D1KY-IDLEVNR-MIN     PIC  X(5) VALUE SPACE.               
017700        05  W-D1D1KY-IDFS-MIN        PIC  X(8) VALUE SPACE.               
017800        05  W-D1D1KY-TIAVIDAT-MIN    PIC S9(7) COMP-3 VALUE ZERO.         
017900        05  W-D1D1KY-IDRADNR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
018000                                                                          
018100     03  W-W6D1D1KY-MAX-X.                                                
018200        05  W-D1D1KY-IDILIST-MAX     PIC  9(5) VALUE ZERO.                
018300        05  W-D1D1KY-IDILIRAD-MAX    PIC S9(5) COMP-3 VALUE ZERO.         
018400        05  W-D1D1KY-IDRADNR-INL-MAX PIC S9(5) COMP-3 VALUE ZERO.         
018500        05  W-D1D1KY-IDDC-MAX        PIC X(2)  VALUE '11'.                
018510        05  W-D1D1KY-IDLEVNR-MAX     PIC  X(5) VALUE SPACE.               
018600        05  W-D1D1KY-IDFS-MAX        PIC  X(8) VALUE SPACE.               
018700        05  W-D1D1KY-TIAVIDAT-MAX    PIC S9(7) COMP-3 VALUE ZERO.         
018800        05  W-D1D1KY-IDRADNR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
018900                                                                          
019000     03  W-W6D101KY-X.                                                    
019100         05  W-D101KY-IDDC       PIC X(2)     VALUE '11'.                 
019110         05  W-D101KY-IDLEVNR    PIC  X(5)    VALUE SPACE.                
019200         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
019300         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
019400                                                                          
019500     03  W-W6D1BSEQ-X.                                                    
019600         05  W-D1BSEQ-IDLOPNRM   PIC S9(9)    COMP-3 VALUE ZERO.          
019700                                                                          
019800     03  W-IDRADNR-INL-X.                                                 
019900        05  W-IDRADNR-INL            PIC S9(5) VALUE ZERO COMP-3.         
020000                                                                          
020100     03  W-IDRADNR-X.                                                     
020200        05  W-IDRADNR                PIC S9(5) VALUE ZERO COMP-3.         
020300                                                                          
020400     03  W-IDILIST-X.                                                     
020500        05  W-IDILIST                PIC  9(5) VALUE ZERO.                
020600                                                                          
020700     03  W-IDILIRAD-X.                                                    
020800        05  W-IDILIRAD               PIC S9(5) COMP-3 VALUE ZERO.         
020900                                                                          
021000     03  W-W6GXKEY-6005-X.                                                
021100         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
021110         05  W-6005-IDDC         PIC X(2)    VALUE '11'.                  
021200         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
021300                                                                          
021400     03  W-W6GXKEY-6006-X.                                                
021500         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
021600         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
021700     SKIP2                                                                
021800*    --- STATUS-KOD FRÅN IMS                                              
021900 01  STATUS-WS                   PIC XX.                                  
022000     88  SEGMENT-FINNS                       VALUE '  '.                  
022100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022400     SKIP2                                                                
022500 01  GODK-STATUSKODER.                                                    
022600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022700     SKIP3                                                                
022800 01  SSA1                        PIC X(128).                              
022900 01  SSA2                        PIC X(64).                               
023000     EJECT                                                                
023100*    --- IMS FUNKTIONSKODER                                               
023200*01  -COPY W0003                                                          
023300     EJECT                                                                
023400*    ---  DLI INPUT-OUTPUT AREA                                           
023500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023600     SKIP3                                                                
023700 01  DLI-IO-AREA1.                                                        
023800     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
023900     SKIP3                                                                
024000     03  W6INLE01 REDEFINES IO-AREA1.                                     
024100*        05  -COPY W6D1D1                                                 
024200     EJECT                                                                
024300 01  DLI-IO-AREA2.                                                        
024400     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
024500     SKIP3                                                                
024600     03  W6INLA21 REDEFINES IO-AREA2.                                     
024700*        05  -COPY W6D111                                                 
024800     EJECT                                                                
024900 01  DLI-IO-AREA3.                                                        
025000     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
025100     SKIP3                                                                
025200     03  W6INLA21 REDEFINES IO-AREA3.                                     
025300*        05  -COPY W6D121                                                 
025400     EJECT                                                                
025500 01  DLI-IO-AREA4.                                                        
025600     03  IO-AREA4                PIC X(150)  VALUE SPACE.                 
025700     SKIP3                                                                
025800     03  W6PLAA11 REDEFINES IO-AREA4.                                     
025900*        05  -COPY W6GX6006 -PRE PLAA-                                    
026000     EJECT                                                                
026100                                                                          
026200 LINKAGE SECTION.                                                         
026300*01  -COPY W0009   -PRE MSG-                                              
026400     EJECT                                                                
026500*01  -COPY W0009   -PRE ALT1-                                             
026600     EJECT                                                                
026700*01  -COPY W0009   -PRE ALT2-                                             
026800     EJECT                                                                
026900*01  -COPY W0009   -PRE DISP-                                             
027000     EJECT                                                                
027100*01  -COPY W0008   -PRE INLE-                                             
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008   -PRE INLA1-                                            
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008   -PRE INLA2-                                            
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008   -PRE INLA3-                                            
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008   -PRE PLAA-                                             
028400     05  FILLER                  PIC X.                                   
028500                                                                          
028600**  PCB'ER FÖR SUBPGM                                                     
028700 01  PMRK-INLB-PCB               PIC X.                                   
028800                                                                          
028900 01  PMRK-INLC-PCB               PIC X.                                   
029000                                                                          
029100 01  PMRK-PLAA-PCB               PIC X.                                   
029200                                                                          
029300 01  STYR-HANA-PCB               PIC X.                                   
029400                                                                          
029410 01  STYR-PLAA-PCB               PIC X.                                   
029420                                                                          
029500 01  KOM-KOMA-PCB                PIC X.                                   
029600                                                                          
029700     EJECT                                                                
029800 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB DISP-PCB             
029900                           INLE-PCB INLA1-PCB INLA2-PCB                   
030000                           INLA3-PCB PLAA-PCB                             
030100                           PMRK-INLB-PCB PMRK-INLC-PCB                    
030200                           PMRK-PLAA-PCB                                  
030300                           STYR-HANA-PCB                                  
030310                           STYR-PLAA-PCB                                  
030400                           KOM-KOMA-PCB.                                  
030500     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB DISP-PCB             
030600                           INLE-PCB INLA1-PCB INLA2-PCB                   
030700                           INLA3-PCB PLAA-PCB                             
030800                           PMRK-INLB-PCB PMRK-INLC-PCB                    
030900                           PMRK-PLAA-PCB                                  
031000                           STYR-HANA-PCB                                  
031010                           STYR-PLAA-PCB                                  
031100                           KOM-KOMA-PCB.                                  
031200                                                                          
031300     PERFORM IMS-GET-MSG                                                  
031400     IF SEGMENT-FINNS                                                     
031500       PERFORM A-INIT                                                     
031600       PERFORM B-KOLLA-NYCKLAR                                            
031700       IF NYCKLAR-OK                                                      
031800         IF MFS-UPDATE                                                    
031900           PERFORM G-KOLLA-INPUT                                          
032000           IF INDATA-OK                                                   
032100             PERFORM H-UPPDATERA                                          
032200           END-IF                                                         
032300         ELSE                                                             
032400           IF MFS-FIRST                                                   
032500             PERFORM C-FOERSTA-SIDA                                       
032600           ELSE                                                           
032700             IF MFS-NEXT                                                  
032800               PERFORM D-NAESTA-SIDA                                      
032900             ELSE                                                         
033000               PERFORM E-SAMMA-SIDA                                       
033100             END-IF                                                       
033200           END-IF                                                         
033300         END-IF                                                           
033400       END-IF                                                             
033500       IF INDATA-OK AND NYCKLAR-OK AND NOT OMSTART                        
033600         PERFORM F-LAES-VISA-INFO                                         
033700       END-IF                                                             
033800       IF OMSTART                                                         
033900           COMPUTE P-TO-P-MSG-KVLL = LNG-P-TO-P-PREFIX + 210              
034000           MOVE 'W6T142U '     TO P-TO-P-MSG-KDTRANS                      
034100           MOVE '6142'         TO P-TO-P-MSG-IDTRANS                      
034200           MOVE MFS-KDMFSFOR TO P-TO-P-MSG-KDMFSFOR                       
034300           MOVE MID-W6I14201 TO P-TO-P-MSG-INDATA                         
034400           PERFORM IMS-ISRT-ALT1-MSG-6142                                 
034500        ELSE                                                              
034600           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
034700           PERFORM IMS-INSERT-MSG                                         
034800       END-IF                                                             
034910     END-IF                                                               
035000                                                                          
035100     MOVE ZERO TO RETURN-CODE                                             
035200     GOBACK                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 A-INIT SECTION.                                                          
035600                                                                          
035700     MOVE JA                   TO INDATA-SW                               
035800     MOVE ZERO                 TO W-CHKP                                  
035900     MOVE NEJ                  TO OMSTART-SW                              
036000     IF MSG-DUBBLA-TRANSKODER                                             
036100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14201                 
036200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
036300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036400     ELSE                                                                 
036500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14201                  
036600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
036700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036800     END-IF                                                               
036900                                                                          
037000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
037200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037300                                                                          
037400     MOVE LOW-VALUE       TO MSG-AREA                                     
037500     MOVE 'W6O14201'      TO MFS-IDMOD                                    
037600     MOVE '6142'          TO MOD-IDTRANS                                  
037700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
037710     ACCEPT DAGENS-DATUM FROM DATE                                        
037800                                                                          
037900     IF EGEN-MID OR HELP-MID                                              
038000       CONTINUE                                                           
038100     ELSE                                                                 
038200       MOVE SPACE TO MFS-KDTRTYP                                          
038300       MOVE '7' TO MFS-IDPFK                                              
038400     END-IF                                                               
038500                                                                          
038600     IF ENGLISH-TEXT                                                      
038700       MOVE +2 TO SPRAK-IX                                                
038800       MOVE 'GB ' TO MED-IDSKYLT                                          
038900     ELSE                                                                 
039000       MOVE +1 TO SPRAK-IX                                                
039100       MOVE 'S  ' TO MED-IDSKYLT                                          
039200     END-IF                                                               
039300     MOVE JA                   TO FOERSTA-6191-SW                         
039400     .                                                                    
039500     EJECT                                                                
039600 B-KOLLA-NYCKLAR SECTION.                                                 
039700                                                                          
039800     MOVE JA TO NYCKLAR-SW                                                
039900                                                                          
040000*    -- KONTROLL AV LISTNR                                                
040100     MOVE MFS-RENSA-FAELT      TO MOD-IDILIST-IN                          
040110                                  MOD-IDILIRAD-IN                         
040200                                                                          
040300     IF MID-IDILIST-IN         = ALL '+'                                  
040400         MOVE MID-IDILIST-UT   TO WS-IDILIST                              
040500         INSPECT WS-IDILIST REPLACING LEADING SPACE BY ZERO               
040600      ELSE                                                                
040700         MOVE MID-IDILIST-IN   TO WS-IDILIST                              
040800         MOVE '7'              TO MFS-IDPFK                               
040900         MOVE SPACE            TO MFS-KDTRTYP                             
041000     END-IF                                                               
041100                                                                          
041200     IF WS-IDILIST NUMERIC AND WS-IDILIST > ZERO                          
041300         CONTINUE                                                         
041400      ELSE                                                                
041500         MOVE NEJ              TO NYCKLAR-SW                              
041600     END-IF                                                               
041700                                                                          
041710     IF MID-IDILIRAD-IN        = ALL '+'                                  
041720         MOVE MID-IDILIRAD-UT  TO WS-IDILIRAD                             
041730         INSPECT WS-IDILIRAD REPLACING LEADING SPACE BY ZERO              
041740      ELSE                                                                
041750         MOVE MID-IDILIRAD-IN  TO WS-IDILIRAD                             
041760         MOVE SPACE            TO MFS-KDTRTYP                             
041780     END-IF                                                               
041790                                                                          
041791     IF WS-IDILIRAD NUMERIC                                               
041792         CONTINUE                                                         
041793      ELSE                                                                
041794         MOVE +0               TO WS-IDILIRAD                             
041795     END-IF                                                               
041796                                                                          
041800     IF GODK-MID OR NYCKLAR-OK                                            
041900         MOVE WS-IDILIST       TO MOD-IDILIST-UT                          
042000         INSPECT MOD-IDILIST-UT REPLACING LEADING ZERO BY SPACE           
042010         MOVE WS-IDILIRAD      TO MOD-IDILIRAD-UT                         
042020         INSPECT MOD-IDILIRAD-UT REPLACING LEADING ZERO BY SPACE          
042100     ELSE                                                                 
042200         MOVE MFS-RENSA-FAELT  TO MOD-IDILIST-UT                          
042210                                  MOD-IDILIRAD-UT                         
042300     END-IF                                                               
042400                                                                          
042500     IF NYCKLAR-FEL                                                       
042600         MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                            
042700         CALL WMEDKONV USING MED-WMEDAREA                                 
042800         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
042900         PERFORM MFS-RENSA-FAELT-IN                                       
043000         PERFORM MFS-RENSA-FAELT-UT                                       
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 C-FOERSTA-SIDA SECTION.                                                  
043500                                                                          
043600     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
043700     CALL WMEDKONV USING MED-WMEDAREA                                     
043800     MOVE MED-MFSINF           TO MOD-TEMFSFEL                            
043900                                                                          
044000     MOVE ZERO                 TO W-IDILIRAD                              
044100     PERFORM MFS-RENSA-FAELT-IN-EJ-ANSTNR                                 
044200     .                                                                    
044300     EJECT                                                                
044400 D-NAESTA-SIDA SECTION.                                                   
044500                                                                          
044600     MOVE MID-IDILIRAD-NEXT    TO W-IDILIRAD                              
044700     PERFORM MFS-RENSA-FAELT-IN-EJ-ANSTNR                                 
044800     .                                                                    
044900     EJECT                                                                
045000 E-SAMMA-SIDA SECTION.                                                    
045100                                                                          
045200     IF EGEN-MID OR HELP-MID                                              
045300         MOVE MID-IDILIRAD-ENTER   TO W-IDILIRAD                          
045400         IF MID-INPUT              = ALL '+'                              
045410             IF WS-IDILIRAD NUMERIC AND WS-IDILIRAD > +0                  
045420               MOVE WS-IDILIRAD TO W-IDILIRAD                             
045430             END-IF                                                       
045500             PERFORM MFS-RENSA-FAELT-IN                                   
045600          ELSE                                                            
045700             MOVE INF-PRESS-PF11   TO MED-IDMFSINF                        
045800             CALL WMEDKONV USING MED-WMEDAREA                             
045900             MOVE MED-MFSINF       TO MOD-TEMFSFEL                        
046000             PERFORM EA-MID-INDATA-TILL-MOD                               
046100         END-IF                                                           
046200      ELSE                                                                
046300          PERFORM MFS-RENSA-FAELT-IN                                      
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 EA-MID-INDATA-TILL-MOD SECTION.                                          
046800                                                                          
046900     IF MID-FLKLAR             =  ALL '+'                                 
047000         MOVE MFS-RENSA-FAELT  TO MOD-FLKLAR                              
047100      ELSE                                                                
047200         MOVE MID-FLKLAR       TO MOD-FLKLAR                              
047300         MOVE MFS-ADD-LAES-IN-FAELT                                       
047400                               TO MOD-FLKLAR-ATTR                         
047500     END-IF                                                               
047600                                                                          
047700     IF MID-IDANSTNR           =  ALL '+'                                 
047800         MOVE MFS-RENSA-FAELT  TO MOD-IDANSTNR                            
047900      ELSE                                                                
048000         MOVE MID-IDANSTNR     TO MOD-IDANSTNR                            
048100         MOVE MFS-ADD-LAES-IN-FAELT                                       
048200                               TO MOD-IDANSTNR-ATTR                       
048300     END-IF                                                               
048400                                                                          
048410     IF MID-FLKLAR-MAK         =  ALL '+'                                 
048420         MOVE MFS-RENSA-FAELT  TO MOD-FLKLAR-MAK                          
048430      ELSE                                                                
048440         MOVE MID-FLKLAR-MAK   TO MOD-FLKLAR-MAK                          
048450         MOVE MFS-ADD-LAES-IN-FAELT                                       
048460                               TO MOD-FLKLAR-MAK-ATTR                     
048470     END-IF                                                               
048480                                                                          
048500     MOVE +1                        TO INDX                               
048600     PERFORM UNTIL INDX             >  MAX-INDX                           
048700         IF MID-KDCMDVAL-RAD(INDX)  =  ALL '+'                            
048800             MOVE MFS-RENSA-FAELT   TO MOD-KDCMDVAL-RAD(INDX)             
048900          ELSE                                                            
049000             MOVE MID-KDCMDVAL-RAD(INDX)                                  
049100                                    TO MOD-KDCMDVAL-RAD(INDX)             
049200             MOVE MFS-ADD-LAES-IN-FAELT                                   
049300                                    TO MOD-KDCMDVAL-RAD-ATTR(INDX)        
049400         END-IF                                                           
049500                                                                          
049600         IF MID-KVINLART-UPD(INDX)  =  ALL '+'                            
049700             MOVE MFS-RENSA-FAELT   TO MOD-KVINLART-UPD(INDX)             
049800          ELSE                                                            
049900             MOVE MID-KVINLART-UPD(INDX)                                  
050000                                    TO MOD-KVINLART-UPD(INDX)             
050100             MOVE MFS-ADD-LAES-IN-FAELT                                   
050200                                    TO MOD-KVINLART-UPD-ATTR(INDX)        
050300         END-IF                                                           
050400                                                                          
050500         IF MID-ADINLOMR-NXT-UPD(INDX) =  ALL '+'                         
050600             MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR-NXT-UPD(INDX)          
050700          ELSE                                                            
050800             MOVE MID-ADINLOMR-NXT-UPD(INDX)                              
050900                                   TO MOD-ADINLOMR-NXT-UPD(INDX)          
051000             MOVE MFS-ADD-LAES-IN-FAELT TO                                
051100                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
051200         END-IF                                                           
051300                                                                          
051400         ADD +1                     TO INDX                               
051500     END-PERFORM                                                          
051600     .                                                                    
051700     EJECT                                                                
051800 F-LAES-VISA-INFO SECTION.                                                
051900                                                                          
052100     MOVE LOW-VALUE            TO W-W6D1D1KY-MIN-X                        
052200     MOVE HIGH-VALUE           TO W-W6D1D1KY-MAX-X                        
052300     MOVE WS-IDILIST           TO W-D1D1KY-IDILIST-MIN                    
052400                                  W-D1D1KY-IDILIST-MAX                    
052500                                                                          
052600     IF MFS-ENTER OR MFS-NEXT OR MFS-UPDATE                               
052700         MOVE W-IDILIRAD       TO W-D1D1KY-IDILIRAD-MIN                   
052800     END-IF                                                               
052900                                                                          
053000     PERFORM IMS-GU-INLE-INLE01                                           
053100     IF SEGMENT-FINNS                                                     
053200         MOVE SEQD-IDILIRAD    TO MOD-IDILIRAD-ENTER                      
053210                                                                          
053220         MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-IDANSTNR-ATTR                  
053230         MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLKLAR-ATTR                    
053240                                       MOD-FLKLAR-MAK-ATTR                
053250                                                                          
053260         MOVE +1                   TO INDX                                
053270         PERFORM UNTIL INDX        >  MAX-INDX                            
053280           MOVE MFS-OEPPNA-ALFA-FAELT  TO                                 
053290                                MOD-KDCMDVAL-RAD-ATTR(INDX)               
053291                                MOD-KVINLART-UPD-ATTR(INDX)               
053292                                MOD-ADINLOMR-NXT-UPD-ATTR(INDX)           
053293           ADD +1       TO INDX                                           
053294         END-PERFORM                                                      
053300      ELSE                                                                
053400         IF MFS-UPDATE                                                    
053500             CONTINUE                                                     
053600          ELSE                                                            
053700             MOVE '010'        TO MED-IDMFSFEL                            
053800             CALL WMEDKONV USING MED-WMEDAREA                             
053900             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
054000         END-IF                                                           
054100         PERFORM MFS-RENSA-FAELT-UT                                       
054200     END-IF                                                               
054300                                                                          
054310     MOVE +1                   TO INDX                                    
054400     PERFORM UNTIL INDX          >  MAX-INDX                              
054500         IF MFS-UPDATE OR MFS-FIRST                                       
054600             MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-RAD(INDX)               
054700                                     MOD-KVINLART-UPD(INDX)               
054800                                     MOD-ADINLOMR-NXT-UPD(INDX)           
054900         END-IF                                                           
055000         IF SEGMENT-FINNS                                                 
055100             MOVE SEQD-IDLEVNR   TO W-D101KY-IDLEVNR                      
055200             MOVE SEQD-IDFS      TO W-D101KY-IDFS                         
055300             MOVE SEQD-TIAVIDAT  TO W-D101KY-TIAVIDAT                     
055400             MOVE SEQD-IDRADNR-INL TO W-IDRADNR-INL                       
055500             MOVE SEQD-IDRADNR   TO W-IDRADNR                             
055600             PERFORM IMS-GU-INLA1-INLA11                                  
055700             IF ART-FLKVAFEL     = JA                                     
055800                 PERFORM FA-ANROPA-W611STYR                               
055900             END-IF                                                       
056000             PERFORM IMS-GNP-INLA1-INLA21                                 
056100             PERFORM FB-FYLL-I-MOD                                        
056200             PERFORM IMS-GN-INLE-INLE01                                   
056300          ELSE                                                            
056400             MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
056500                                      MOD-KVINLART-UPD-ATTR(INDX)         
056600                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
056700             MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-RAD(INDX)               
056800                                     MOD-KVINLART-UPD(INDX)               
056900                                     MOD-ADINLOMR-NXT-UPD(INDX)           
057000                                     MOD-ADLAGOMR-RAD(INDX)               
057100                                     MOD-ADPLATS-RAD (INDX)               
057200                                     MOD-ADGANG-RAD  (INDX)               
057300                                     MOD-IDARTNR-RAD (INDX)               
057400                                     MOD-BEART-RAD   (INDX)               
057500                                     MOD-KVINLART-RAD(INDX)               
057600                                     MOD-IDILIRAD-RAD(INDX)               
057700                                     MOD-ADINLOMR-FB-RAD(INDX)            
057800         END-IF                                                           
057900         ADD +1                TO INDX                                    
058000     END-PERFORM                                                          
058100                                                                          
058200     IF SEGMENT-FINNS                                                     
058300         MOVE SEQD-IDILIRAD         TO MOD-IDILIRAD-NEXT                  
058400         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
058500         CALL WMEDKONV USING MED-WMEDAREA                                 
058600         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
058700       ELSE                                                               
058800         MOVE ZERO                 TO MOD-IDILIRAD-NEXT                   
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 FA-ANROPA-W611STYR   SECTION.                                            
059300                                                                          
059400     MOVE ART-IDDC             TO STYR-IDDC                               
059410     MOVE ART-IDARTNR          TO STYR-IDARTNR                            
059500     MOVE ART-IDFKNGRP         TO STYR-IDFKNGRP                           
059600     MOVE SEQD-IDLEVNR         TO STYR-IDLEVNR                            
059700     MOVE ART-BEFT             TO STYR-BEFT                               
059800     CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                      
059810                                       STYR-PLAA-PCB                      
059900     .                                                                    
060000     EJECT                                                                
060100 FB-FYLL-I-MOD   SECTION.                                                 
060200                                                                          
060300     IF MFS-UPDATE OR MFS-FIRST                                           
060400         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD    (INDX)              
060500                                  MOD-KVINLART-UPD    (INDX)              
060600                                  MOD-ADINLOMR-NXT-UPD(INDX)              
060700     END-IF                                                               
060800                                                                          
060900     MOVE ART-ADLAGOMR         TO MOD-ADLAGOMR-RAD(INDX)                  
061000     MOVE ART-ADPLATS          TO MOD-ADPLATS-RAD (INDX)                  
061100     MOVE ART-ADGANG           TO MOD-ADGANG-RAD  (INDX)                  
061200     MOVE ART-IDARTNR          TO MOD-IDARTNR-RAD (INDX)                  
061300     MOVE ART-BEART            TO MOD-BEART-RAD   (INDX)                  
061400     MOVE RAD-KVINLART         TO MOD-KVINLART-RAD(INDX)                  
061500     MOVE RAD-IDILIRAD         TO MOD-IDILIRAD-RAD(INDX)                  
061600                                                                          
061700     IF ART-FLKVAFEL                = JA            OR                    
061701       (RAD-FLSATS = JA AND ART-ADLAGOMR NOT = 30)                        
061710         IF ART-FLKVAFEL                = JA                              
061720           MOVE '189'                 TO MED-IDMFSFEL                     
061730         ELSE                                                             
061731           MOVE '185'                 TO MED-IDMFSFEL                     
061740         END-IF                                                           
061800         MOVE STYR-ADINLOMR-FB      TO MOD-ADINLOMR-FB-RAD(INDX)          
061900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-RAD-ATTR(INDX)         
062100         CALL WMEDKONV USING MED-WMEDAREA                                 
062200         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 G-KOLLA-INPUT SECTION.                                                   
062700                                                                          
062800     MOVE SPACE                     TO MED-IDMFSFEL                       
062900     IF MID-INPUT                   =  ALL '+'                            
063000         MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                       
063100         CALL WMEDKONV USING MED-WMEDAREA                                 
063200         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
063300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
063400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
063500         MOVE NEJ                   TO INDATA-SW                          
063600      ELSE                                                                
063610        IF MID-FLKLAR-MAK           = ALL '+' OR SPACE OR NEJ             
063700         IF MID-FLKLAR              = ALL '+' OR SPACE OR NEJ             
063800             PERFORM GA-KOLLA-RADBEHANDLING                               
063900          ELSE                                                            
064000             PERFORM GB-KOLLA-LISTA-KLART                                 
064100         END-IF                                                           
064110        ELSE                                                              
064120          PERFORM GC-KOLLA-MAKULERING-LISTAN                              
064130        END-IF                                                            
064200                                                                          
064300         IF INDATA-FEL                                                    
064400             IF MED-IDMFSFEL           =  SPACE                           
064500                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
064600             END-IF                                                       
064700             CALL WMEDKONV USING MED-WMEDAREA                             
064800             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
064900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
065000             PERFORM MFS-ROER-EJ-FAELT-IN                                 
065100         END-IF                                                           
065200     END-IF                                                               
065300     .                                                                    
065400     EJECT                                                                
065500 GA-KOLLA-RADBEHANDLING SECTION.                                          
065600                                                                          
065700     MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                       
065800     MOVE +1                   TO INDX                                    
065900     PERFORM UNTIL INDX        >  MAX-INDX                                
066000         IF (MID-KDCMDVAL-RAD(INDX)     = ALL '+' OR SPACE) AND           
066100            (MID-KVINLART-UPD(INDX)     = ALL '+' OR SPACE) AND           
066200            (MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE)               
066310          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-RAD-ATTR(INDX)        
066320                                   MOD-ADINLOMR-NXT-UPD-ATTR(INDX)        
066330          MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVINLART-UPD-ATTR(INDX)        
066400         ELSE                                                             
066500           PERFORM GAA-KOLLA-ATT-RAD-FINNS                                
066600           IF SEGMENT-FINNS                                               
066601             PERFORM IMS-GU-INLA1-INLA11                                  
066602             MOVE ART-KDRT         TO SPAR-KDRT                           
066603             MOVE ART-ADLAGOMR     TO SPAR-ADLAGOMR                       
066604             MOVE ART-ADGANG       TO SPAR-ADGANG                         
066605             MOVE ART-ADPLATS      TO SPAR-ADPLATS                        
066606             PERFORM IMS-GNP-INLA1-INLA21                                 
066607             PERFORM GAB-KOLLA-KDCMDVAL                                   
066608             PERFORM GAC-KOLLA-KVINLART-UPD                               
066609             PERFORM GAD-KOLLA-ADINLOMR-NXT-UPD                           
066610                                                                          
066611             IF MID-KDCMDVAL-RAD(INDX) = 'AVV'                            
066612                 PERFORM GAE-KOLLA-IDANSTNR-O-KDRT                        
066613             END-IF                                                       
066614                                                                          
066615             IF MID-KDCMDVAL-RAD(INDX) =  ALL '+' OR                      
066616                MID-KDCMDVAL-RAD(INDX) =  SPACE                           
066617                CONTINUE                                                  
066618             ELSE                                                         
066619                PERFORM GAF-KOLLA-PLATS                                   
066620             END-IF                                                       
066621                                                                          
066622           ELSE                                                           
066623            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-RAD-ATTR(INDX)        
066630                                   MOD-ADINLOMR-NXT-UPD-ATTR(INDX)        
066640            MOVE MFS-NUM-FAELT-FEL  TO MOD-KVINLART-UPD-ATTR(INDX)        
066650           END-IF                                                         
067310         END-IF                                                           
067400         ADD +1                TO INDX                                    
067500     END-PERFORM                                                          
067600     .                                                                    
067700     EJECT                                                                
067800 GAA-KOLLA-ATT-RAD-FINNS SECTION.                                         
067900                                                                          
068000     MOVE LOW-VALUE              TO W-W6D1D1KY-MIN-X                      
068100     MOVE HIGH-VALUE             TO W-W6D1D1KY-MAX-X                      
068200     INSPECT MID-IDILIRAD-RAD(INDX) REPLACING                             
068300                                         LEADING SPACE BY ZERO            
068400     MOVE WS-IDILIST             TO W-D1D1KY-IDILIST-MIN                  
068500                                    W-D1D1KY-IDILIST-MAX                  
068600     MOVE MID-IDILIRAD-RAD(INDX) TO W-D1D1KY-IDILIRAD-MIN                 
068700                                    W-D1D1KY-IDILIRAD-MAX                 
068800     PERFORM IMS-GU-INLE-INLE01                                           
068900     IF SEGMENT-FINNS                                                     
069000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
069100         MOVE JA                   TO RAD-SW                              
069110         MOVE SEQD-IDLEVNR         TO W-D101KY-IDLEVNR                    
069120         MOVE SEQD-IDFS            TO W-D101KY-IDFS                       
069130         MOVE SEQD-TIAVIDAT        TO W-D101KY-TIAVIDAT                   
069140         MOVE SEQD-IDRADNR-INL     TO W-IDRADNR-INL                       
069150         MOVE SEQD-IDRADNR         TO W-IDRADNR                           
069200      ELSE                                                                
069300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
069400         MOVE NEJ                  TO INDATA-SW                           
069500         MOVE '010'                TO MED-IDMFSFEL                        
069600         MOVE NEJ                  TO RAD-SW                              
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 GAB-KOLLA-KDCMDVAL   SECTION.                                            
070100                                                                          
070200     IF MID-KDCMDVAL-RAD(INDX)     = ALL '+' OR SPACE OR 'DIN' OR         
070300                                     'DI' OR 'AVV' OR 'I' OR 'INL'        
070400         CONTINUE                                                         
070500      ELSE                                                                
070600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
070700         MOVE NEJ                  TO INDATA-SW                           
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 GAC-KOLLA-KVINLART-UPD   SECTION.                                        
071200                                                                          
071300     IF MID-KDCMDVAL-RAD(INDX) =  'AVV' OR 'DIN' OR 'DI'                  
071400         IF MID-KVINLART-UPD(INDX) NUMERIC                                
071401             MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                
071410             IF W-KVINLART-UPD NOT = RAD-KVINLART                         
071411               MOVE MFS-NUM-FAELT-RAETT                                   
071412                                 TO MOD-KVINLART-UPD-ATTR(INDX)           
071413               IF SPAR-KDRT = 7                                           
071414                 IF W-KVINLART-UPD > RAD-KVINLART                         
071415                   MOVE NEJ          TO INDATA-SW                         
071416                   MOVE MFS-NUM-FAELT-FEL                                 
071417                                    TO MOD-KVINLART-UPD-ATTR(INDX)        
071418                 END-IF                                                   
071419               END-IF                                                     
071420             ELSE                                                         
071421               MOVE NEJ          TO INDATA-SW                             
071422               MOVE MFS-NUM-FAELT-FEL                                     
071423                                 TO MOD-KVINLART-UPD-ATTR(INDX)           
071430             END-IF                                                       
071431             IF MID-KDCMDVAL-RAD(INDX) =  'DIN' OR 'DI'                   
071440               IF W-KVINLART-UPD   = ZERO  OR                             
071450                  W-KVINLART-UPD   > RAD-KVINLART                         
071480                 MOVE NEJ          TO INDATA-SW                           
071490                 MOVE MFS-NUM-FAELT-FEL                                   
071500                                   TO MOD-KVINLART-UPD-ATTR(INDX)         
071600               END-IF                                                     
071610             END-IF                                                       
071700          ELSE                                                            
071800             MOVE NEJ          TO INDATA-SW                               
071900             MOVE MFS-NUM-FAELT-FEL                                       
072000                               TO MOD-KVINLART-UPD-ATTR(INDX)             
072100         END-IF                                                           
072200     ELSE                                                                 
072300         IF MID-KVINLART-UPD(INDX) = ALL '+' OR SPACE                     
072400             MOVE MFS-NUM-FAELT-RAETT                                     
072500                               TO MOD-KVINLART-UPD-ATTR(INDX)             
072600          ELSE                                                            
072700             MOVE MFS-NUM-FAELT-FEL                                       
072800                               TO MOD-KVINLART-UPD-ATTR(INDX)             
072900             MOVE NEJ          TO INDATA-SW                               
073000         END-IF                                                           
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400 GAD-KOLLA-ADINLOMR-NXT-UPD  SECTION.                                     
073500                                                                          
073600     IF MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE                     
073700             MOVE MFS-ALFA-FAELT-RAETT                                    
073800                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
073900      ELSE                                                                
074000         IF MID-KDCMDVAL-RAD(INDX) = 'INL' OR 'I' OR 'AVV'                
074100             MOVE MFS-ALFA-FAELT-FEL                                      
074200                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
074300             MOVE NEJ          TO INDATA-SW                               
074400          ELSE                                                            
074500             PERFORM GADA-KOLLA-PLAA                                      
074600             PERFORM GADB-KOLLA-ADINLOMR                                  
074700         END-IF                                                           
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 GADA-KOLLA-PLAA              SECTION.                                    
075200                                                                          
075300     MOVE MID-ADINLOMR-NXT-UPD(INDX) TO W-6006-ADINLOMR                   
075400     PERFORM IMS-GU-PLAA-PLAA11                                           
075500     IF SEGMENT-FINNS                                                     
075600         MOVE MFS-ALFA-FAELT-RAETT                                        
075700                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
075800      ELSE                                                                
075900         MOVE MFS-ALFA-FAELT-FEL                                          
076000                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
076100         MOVE NEJ              TO INDATA-SW                               
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 GADB-KOLLA-ADINLOMR          SECTION.                                    
076600                                                                          
076700     IF RAD-SAKNAS OR                                                     
076800        RAD-ADINLOMR = MID-ADINLOMR-NXT-UPD(INDX)                         
076900         MOVE MFS-ALFA-FAELT-FEL                                          
077000                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
077100         MOVE NEJ              TO INDATA-SW                               
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
077500 GAE-KOLLA-IDANSTNR-O-KDRT   SECTION.                                     
077600                                                                          
077700     INSPECT MID-IDANSTNR REPLACING LEADING SPACE BY ZERO                 
077800     IF MID-IDANSTNR               NUMERIC                                
077900         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                   
078000      ELSE                                                                
078100         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-ATTR                   
078200         MOVE NEJ                  TO INDATA-SW                           
078300     END-IF                                                               
078310     IF SPAR-KDRT = 3                                                     
078311         MOVE MFS-NUM-FAELT-FEL    TO MOD-KVINLART-UPD-ATTR(INDX)         
078312         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
078313         MOVE NEJ                  TO INDATA-SW                           
078314         MOVE '227'                TO MED-IDMFSFEL                        
078320     END-IF                                                               
078400     .                                                                    
078500     EJECT                                                                
078510 GAF-KOLLA-PLATS        SECTION.                                          
078520                                                                          
078521*    SKALL KOMPLETTERAS MED REGLER FÖR NÄR PLATS 0 ÄR TILLÅTEN            
078522                                                                          
078523     IF SPAR-KDRT = 7 OR 77                                               
078524       CONTINUE                                                           
078525     ELSE                                                                 
078530       IF SPAR-ADPLATS = ZERO                                             
078531          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)        
078533          MOVE NEJ                  TO INDATA-SW                          
078534          MOVE '764'                TO MED-IDMFSFEL                       
078540       END-IF                                                             
078541     END-IF                                                               
078550     .                                                                    
078560     EJECT                                                                
078600 GB-KOLLA-LISTA-KLART   SECTION.                                          
078700                                                                          
078800     IF MID-FLKLAR                 = ALL 'J'                              
078900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                     
079000      ELSE                                                                
079100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                     
079200         MOVE NEJ                  TO INDATA-SW                           
079300     END-IF                                                               
079400                                                                          
079500     IF MID-IDANSTNR                =  ALL '+' OR SPACE                   
079600         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDANSTNR-ATTR                  
079700      ELSE                                                                
079800         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDANSTNR-ATTR                  
080000     END-IF                                                               
080100                                                                          
080200     MOVE +1                   TO INDX                                    
080300     PERFORM UNTIL INDX        >  MAX-INDX                                
080400         PERFORM GBA-KOLLA-KDCMDVAL                                       
080500         PERFORM GBB-KOLLA-KVINLART-UPD                                   
080600         PERFORM GBC-KOLLA-ADINLOMR-NXT-UPD                               
080700         ADD +1                TO INDX                                    
080800     END-PERFORM                                                          
080810                                                                          
080811     MOVE WS-IDILIST       TO W-D1DSEQ-IDILIST-MIN                        
080812                              W-D1DSEQ-IDILIST-MAX                        
080813     MOVE NEJ              TO LISTA-SW                                    
080814                                                                          
080815     PERFORM IMS-GU-INLA2-INLA11-MIN-MAX                                  
080817                                                                          
080818     IF SEGMENT-FINNS                                                     
080819       MOVE JA       TO LISTA-SW                                          
080821                                                                          
080822       IF ART-KDRT = 7 OR 77                                              
080823         CONTINUE                                                         
080824       ELSE                                                               
080825         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
080826                                         INDATA-FEL                       
080830           IF ART-ADPLATS = ZERO                                          
080840             MOVE NEJ    TO INDATA-SW                                     
080850             MOVE '764'  TO MED-IDMFSFEL                                  
080860           END-IF                                                         
080870           PERFORM IMS-GN-INLA2-INLA11                                    
080880         END-PERFORM                                                      
080881       END-IF                                                             
080882     END-IF                                                               
080890                                                                          
080891     IF LISTA-SAKNAS                                                      
080892        MOVE NEJ    TO INDATA-SW                                          
080893        MOVE '010'  TO MED-IDMFSFEL                                       
080894     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100 GBA-KOLLA-KDCMDVAL   SECTION.                                            
081200                                                                          
081300     IF MID-KDCMDVAL-RAD(INDX)     = ALL '+' OR SPACE                     
081400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
081500      ELSE                                                                
081600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
081700         MOVE NEJ                  TO INDATA-SW                           
081800     END-IF                                                               
081900     .                                                                    
082000     EJECT                                                                
082100 GBB-KOLLA-KVINLART-UPD   SECTION.                                        
082200                                                                          
082300     IF MID-KVINLART-UPD(INDX)     =  ALL '+' OR SPACE                    
082400         MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVINLART-UPD-ATTR(INDX)         
082500      ELSE                                                                
082600         MOVE MFS-NUM-FAELT-FEL    TO MOD-KVINLART-UPD-ATTR(INDX)         
082700         MOVE NEJ                  TO INDATA-SW                           
082800     END-IF                                                               
082900     .                                                                    
083000     EJECT                                                                
083100 GBC-KOLLA-ADINLOMR-NXT-UPD  SECTION.                                     
083200                                                                          
083300     IF MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE                     
083400         MOVE MFS-ALFA-FAELT-RAETT                                        
083500                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
083600      ELSE                                                                
083700         MOVE MFS-ALFA-FAELT-FEL                                          
083800                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
083900         MOVE NEJ              TO INDATA-SW                               
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084210 GC-KOLLA-MAKULERING-LISTAN SECTION.                                      
084220                                                                          
084230     IF MID-FLKLAR-MAK = JA AND                                           
084231        MID-FLKLAR = ALL '+' OR NEJ OR SPACE                              
084232         MOVE MFS-ALFA-FAELT-RAETT                                        
084233                               TO MOD-FLKLAR-MAK-ATTR                     
084240     ELSE                                                                 
084241         MOVE MFS-ALFA-FAELT-FEL                                          
084242                               TO MOD-FLKLAR-MAK-ATTR                     
084243                                  MOD-FLKLAR-ATTR                         
084244         MOVE NEJ              TO INDATA-SW                               
084250     END-IF                                                               
084251                                                                          
084252     MOVE WS-IDILIST       TO W-D1DSEQ-IDILIST-MIN                        
084253                              W-D1DSEQ-IDILIST-MAX                        
084254     MOVE NEJ              TO LISTA-SW                                    
084255                                                                          
084256     PERFORM IMS-GU-INLA2-INLA11-MIN-MAX                                  
084257                                                                          
084259     IF SEGMENT-FINNS                                                     
084264       MOVE JA      TO LISTA-SW                                           
084265       IF ART-KDRT = 7 OR 77                                              
084266         MOVE NEJ   TO INDATA-SW                                          
084267         MOVE '007' TO MED-IDMFSFEL                                       
084268       END-IF                                                             
084269     END-IF                                                               
084270                                                                          
084271     IF LISTA-SAKNAS                                                      
084272        MOVE NEJ    TO INDATA-SW                                          
084273        MOVE '010'  TO MED-IDMFSFEL                                       
084274     END-IF                                                               
084275     .                                                                    
084280     EJECT                                                                
084300 H-UPPDATERA SECTION.                                                     
084400                                                                          
084500     MOVE +1                   TO 6191-IX                                 
084510     IF MID-FLKLAR-MAK = ALL '+' OR NEJ OR SPACE                          
084511       IF MID-FLKLAR             = ALL '+' OR NEJ OR SPACE                
084512           PERFORM HA-UPPDATERA-RADER                                     
084513        ELSE                                                              
084514           PERFORM HB-UPPDATERA-HELA-LISTAN                               
084515           MOVE JA         TO UPPDATERING-SW                              
084516       END-IF                                                             
084520     ELSE                                                                 
084530       PERFORM HC-MAKULERA-HELA-LISTAN                                    
084531       MOVE JA         TO UPPDATERING-SW                                  
084540     END-IF                                                               
085100                                                                          
085200     IF 6191-IX                > 1                                        
085300         PERFORM S05-STARTA-W6T191                                        
085400     END-IF                                                               
085500                                                                          
085510     IF UPPDATERING-UTF                                                   
085511       MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                          
085520     ELSE                                                                 
085521       MOVE INF-UPDATE-NOT-DONE  TO MED-IDMFSINF                          
085530     END-IF                                                               
085700     CALL WMEDKONV USING MED-WMEDAREA                                     
085800     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
085900     PERFORM MFS-FORM-ATTR                                                
086000     PERFORM MFS-RENSA-FAELT-IN                                           
086100     .                                                                    
086200     EJECT                                                                
086300 HA-UPPDATERA-RADER SECTION.                                              
086400                                                                          
086500     MOVE +1                       TO INDX                                
086600     PERFORM UNTIL INDX            >  MAX-INDX                            
086700         IF MID-KDCMDVAL-RAD(INDX) = 'AVV'                                
086800             PERFORM HAA-UPPDATERA-AVVIKELSE                              
086810             MOVE JA         TO UPPDATERING-SW                            
086900         END-IF                                                           
087000                                                                          
087010         IF MID-KDCMDVAL-RAD(INDX) = 'DIN' OR 'DI'                        
087020             PERFORM HAC-UPPDATERA-DELINLAEGGNING                         
087030             MOVE JA         TO UPPDATERING-SW                            
087040         END-IF                                                           
087050                                                                          
087100         IF MID-KDCMDVAL-RAD(INDX) = 'INL' OR 'I'                         
087200             PERFORM S02-UPPDATERA-INLAEGGNING                            
087210             MOVE JA         TO UPPDATERING-SW                            
087300         END-IF                                                           
087400                                                                          
087500         IF MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE                 
087600             CONTINUE                                                     
087700          ELSE                                                            
087800             PERFORM HAB-UPPDATERA-RETUR                                  
087810             MOVE JA         TO UPPDATERING-SW                            
087900         END-IF                                                           
088000         ADD +1                    TO INDX                                
088100     END-PERFORM                                                          
088200     .                                                                    
088300     EJECT                                                                
088400 HAA-UPPDATERA-AVVIKELSE SECTION.                                         
088500                                                                          
088600     INSPECT MID-IDILIRAD-RAD(INDX)  REPLACING                            
088700                                    LEADING SPACE BY ZERO                 
088800     MOVE MID-IDILIRAD-RAD(INDX)     TO W-D1DSEQ-IDILIRAD                 
088900                                        W-IDILIRAD                        
089000     PERFORM S03-LAES-INLA21                                              
089100     IF MID-KVINLART-UPD(INDX)       =  ZERO AND RAD-IDRADNR > +1         
089200         MOVE 'AVV'                  TO RAD-KDINLSTA                      
089300         MOVE MID-IDANSTNR           TO RAD-IDANSTNR                      
089310         MOVE SPACE                  TO RAD-ADINLOMR                      
089320                                        RAD-ADINLOMR-NXT                  
089330         MOVE ZERO                   TO RAD-IDILIRAD                      
089340                                        RAD-IDILIST                       
089350                                        RAD-IDINLVGN                      
089351         MOVE DAGENS-DATUM           TO RAD-TIUPPDAT                      
089360         MOVE 31                     TO RAD-KDINLPRIO                     
089370         MOVE NEJ                    TO RAD-FLPRIO                        
089400         PERFORM IMS-REPL-INLA2-INLA21                                    
089500         PERFORM S04-SKAPA-6191-MID                                       
089600         PERFORM S06-SKAPA-6193-MID                                       
089700      ELSE                                                                
089800         MOVE RAD-IDRADNR      TO W-IDRADNR                               
089900         PERFORM HAAA-SKAPA-NY-AVV-RAD                                    
089911         PERFORM S07-SKAPA-6191-MID-NY-RAD                                
090100         IF W-IDRADNR          =  1                                       
090200             IF MID-KVINLART-UPD(INDX) NOT = ZERO                         
090300               MOVE ART-IDLOPNRM           TO W-D1BSEQ-IDLOPNRM           
090400               PERFORM IMS-GU-INLA3-INLA21-LAST                           
090500               IF SEGMENT-SAKNAS                                          
090600                   MOVE +2                 TO SPAR-RAD-IDRADNR            
090700                ELSE                                                      
090800                   COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1             
090900               END-IF                                                     
091000               MOVE SPAR-RAD-W6D121        TO RAD-W6D121                  
091100               MOVE MID-KVINLART-UPD(INDX) TO RAD-KVINLART                
091200               MOVE 'INL'                  TO RAD-KDINLSTA                
091300               MOVE SPACE                  TO RAD-ADINLOMR                
091400                                              RAD-ADINLOMR-NXT            
091500               MOVE ZERO                   TO RAD-IDINLVGN                
091600                                              RAD-IDILIST                 
091700                                              RAD-IDILIRAD                
091710               MOVE DAGENS-DATUM           TO RAD-TIUPPDAT                
091800               PERFORM IMS-ISRT-INLA2-INLA21                              
091801               PERFORM S07-SKAPA-6191-MID-NY-RAD                          
091810             END-IF                                                       
091900             PERFORM S06-SKAPA-6193-MID                                   
092000                                                                          
092200             PERFORM IMS-GHU-INLA2-INLA21                                 
092500             PERFORM S08-SKAPA-6191-MID-BORT-RAD                          
092510             PERFORM IMS-DLET-INLA2-INLA21                                
092670          ELSE                                                            
092700             PERFORM IMS-GHU-INLA2-INLA21                                 
092800             MOVE MID-KVINLART-UPD(INDX) TO RAD-KVINLART                  
092900             MOVE 'INL'                  TO RAD-KDINLSTA                  
093000             MOVE SPACE                  TO RAD-ADINLOMR                  
093100                                            RAD-ADINLOMR-NXT              
093200             MOVE ZERO                   TO RAD-IDINLVGN                  
093300                                            RAD-IDILIST                   
093400                                            RAD-IDILIRAD                  
093410             MOVE DAGENS-DATUM           TO RAD-TIUPPDAT                  
093500             PERFORM IMS-REPL-INLA2-INLA21                                
093600             PERFORM S04-SKAPA-6191-MID                                   
093700             PERFORM S06-SKAPA-6193-MID                                   
093800         END-IF                                                           
093900     END-IF                                                               
094000                                                                          
094100     IF (RAD-KDINLPRIO          < 31 AND RAD-FLPRIO = NEJ) OR             
094200        (RAD-FLPRIO             = JA)                                     
094300         PERFORM S09-CALL-W611PMRK                                        
094400     END-IF                                                               
094500     .                                                                    
094600     EJECT                                                                
094700 HAAA-SKAPA-NY-AVV-RAD SECTION.                                           
094800                                                                          
094900     MOVE ART-IDLOPNRM           TO W-D1BSEQ-IDLOPNRM                     
095000     PERFORM IMS-GU-INLA3-INLA21-LAST                                     
095100     IF  SEGMENT-FINNS                                                    
095200         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
095300      ELSE                                                                
095400         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
095500     END-IF                                                               
095600     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
095700     MOVE SPACE                  TO RAD-ADINLOMR                          
095800                                    RAD-ADINLOMR-NXT                      
095900     MOVE MID-IDANSTNR           TO RAD-IDANSTNR                          
096000     MOVE ZERO                   TO RAD-IDILIRAD                          
096100                                    RAD-IDILIST                           
096200                                    RAD-IDINLVGN                          
096300     MOVE DAGENS-DATUM           TO RAD-TIUPPDAT                          
096400     MOVE 31                     TO RAD-KDINLPRIO                         
096410     MOVE NEJ                    TO RAD-FLPRIO                            
096500     MOVE 'AVV'                  TO RAD-KDINLSTA                          
096600     MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                        
096700     COMPUTE RAD-KVINLART        =  SPAR-RAD-KVINLART -                   
096800                                    W-KVINLART-UPD                        
097000                                                                          
097100     PERFORM IMS-ISRT-INLA2-INLA21                                        
097200     .                                                                    
097300     EJECT                                                                
098200 HAB-UPPDATERA-RETUR     SECTION.                                         
098300                                                                          
098400     INSPECT MID-IDILIRAD-RAD(INDX)  REPLACING                            
098500                                    LEADING SPACE BY ZERO                 
098600     MOVE MID-IDILIRAD-RAD(INDX)     TO W-D1DSEQ-IDILIRAD                 
098700                                        W-IDILIRAD                        
098800     PERFORM S03-LAES-INLA21                                              
098900     MOVE MID-ADINLOMR-NXT-UPD(INDX) TO RAD-ADINLOMR-NXT                  
098910     MOVE ZERO                       TO RAD-IDILIRAD                      
098920                                        RAD-IDILIST                       
098930                                        RAD-IDINLVGN                      
098940                                        RAD-TIUPPDAT                      
099000     PERFORM IMS-REPL-INLA2-INLA21                                        
099100     PERFORM S04-SKAPA-6191-MID                                           
099200     .                                                                    
099300     EJECT                                                                
099310 HAC-UPPDATERA-DELINLAEGGNING SECTION.                                    
099320                                                                          
099330     INSPECT MID-IDILIRAD-RAD(INDX)  REPLACING                            
099340                                    LEADING SPACE BY ZERO                 
099350     MOVE MID-IDILIRAD-RAD(INDX)     TO W-D1DSEQ-IDILIRAD                 
099360                                        W-IDILIRAD                        
099361     MOVE MID-KVINLART-UPD(INDX)     TO W-KVINLART-UPD                    
099362                                                                          
099370     PERFORM S03-LAES-INLA21                                              
099404     MOVE RAD-IDRADNR      TO W-IDRADNR                                   
099405                                                                          
099406     COMPUTE RAD-KVINLART  =  RAD-KVINLART - W-KVINLART-UPD               
099407     PERFORM IMS-REPL-INLA2-INLA21                                        
099409     PERFORM S04-SKAPA-6191-MID                                           
099410                                                                          
099411     PERFORM HACA-SKAPA-NY-RAD                                            
099412     PERFORM S07-SKAPA-6191-MID-NY-RAD                                    
099413     PERFORM S06-SKAPA-6193-MID                                           
099454                                                                          
099455     IF (RAD-KDINLPRIO          < 31 AND RAD-FLPRIO = NEJ) OR             
099456        (RAD-FLPRIO             = JA)                                     
099457         PERFORM S09-CALL-W611PMRK                                        
099458     END-IF                                                               
099459     .                                                                    
099460     EJECT                                                                
099461 HACA-SKAPA-NY-RAD SECTION.                                               
099462                                                                          
099463     MOVE ART-IDLOPNRM           TO W-D1BSEQ-IDLOPNRM                     
099464     PERFORM IMS-GU-INLA3-INLA21-LAST                                     
099465     IF  SEGMENT-FINNS                                                    
099466         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
099467      ELSE                                                                
099468         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
099469     END-IF                                                               
099470     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
099471     MOVE SPACE                  TO RAD-ADINLOMR                          
099472                                    RAD-ADINLOMR-NXT                      
099474     MOVE ZERO                   TO RAD-IDILIRAD                          
099475                                    RAD-IDILIST                           
099476                                    RAD-IDINLVGN                          
099477     MOVE DAGENS-DATUM           TO RAD-TIUPPDAT                          
099479     MOVE NEJ                    TO RAD-FLPRIO                            
099480     MOVE 'INL'                  TO RAD-KDINLSTA                          
099483     MOVE W-KVINLART-UPD         TO RAD-KVINLART                          
099484                                                                          
099485     PERFORM IMS-ISRT-INLA2-INLA21                                        
099486     .                                                                    
099487     EJECT                                                                
099490 HB-UPPDATERA-HELA-LISTAN SECTION.                                        
099500                                                                          
099600     MOVE LOW-VALUE            TO W-W6D1D1KY-MIN-X                        
099700     MOVE HIGH-VALUE           TO W-W6D1D1KY-MAX-X                        
099800     MOVE WS-IDILIST           TO W-D1D1KY-IDILIST-MIN                    
099900                                  W-D1D1KY-IDILIST-MAX                    
100000                                                                          
100100     PERFORM IMS-GN-INLE-INLE01                                           
100200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
100300                   W-CHKP > MAX-CHKP                                      
100400         MOVE SEQD-IDILIRAD    TO W-D1DSEQ-IDILIRAD                       
100500                                  W-IDILIRAD                              
100600         PERFORM S02-UPPDATERA-INLAEGGNING                                
100700         PERFORM IMS-GN-INLE-INLE01                                       
100800     END-PERFORM                                                          
100900                                                                          
101000     IF SEGMENT-FINNS                                                     
101100         MOVE JA               TO OMSTART-SW                              
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101410 HC-MAKULERA-HELA-LISTAN SECTION.                                         
101420                                                                          
101430     MOVE LOW-VALUE            TO W-W6D1D1KY-MIN-X                        
101440     MOVE HIGH-VALUE           TO W-W6D1D1KY-MAX-X                        
101450     MOVE WS-IDILIST           TO W-D1D1KY-IDILIST-MIN                    
101460                                  W-D1D1KY-IDILIST-MAX                    
101461                                  W-D1DSEQ-IDILIST                        
101470                                                                          
101480     PERFORM IMS-GN-INLE-INLE01                                           
101490     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
101491                   W-CHKP > MAX-CHKP                                      
101492         MOVE SEQD-IDILIRAD    TO W-D1DSEQ-IDILIRAD                       
101493                                  W-IDILIRAD                              
101494         MOVE SEQD-IDRADNR     TO W-IDRADNR                               
101495         PERFORM HCA-MAKULERA-RAD                                         
101496         PERFORM IMS-GN-INLE-INLE01                                       
101497     END-PERFORM                                                          
101498                                                                          
101499     IF SEGMENT-FINNS                                                     
101500         MOVE JA               TO OMSTART-SW                              
101501     END-IF                                                               
101502     .                                                                    
101503     EJECT                                                                
101504 HCA-MAKULERA-RAD SECTION.                                                
101505                                                                          
101506     PERFORM IMS-GHU-INLA2-INLA21                                         
101508     MOVE ZERO                TO RAD-IDILIST                              
101509                                 RAD-IDILIRAD                             
101510                                 RAD-TIUPPDAT                             
101511     PERFORM IMS-REPL-INLA2-INLA21                                        
101512     .                                                                    
101513     EJECT                                                                
101520 S02-UPPDATERA-INLAEGGNING  SECTION.                                      
101600                                                                          
101700     IF MID-FLKLAR              = ALL '+' OR NEJ OR SPACE                 
101800         INSPECT MID-IDILIRAD-RAD(INDX) REPLACING                         
101900                                    LEADING SPACE BY ZERO                 
102000         MOVE MID-IDILIRAD-RAD(INDX) TO W-D1DSEQ-IDILIRAD                 
102100                                        W-IDILIRAD                        
102200     END-IF                                                               
102300     PERFORM S03-LAES-INLA21                                              
102400     IF RAD-IDRADNR            =  1                                       
102500                                                                          
102600         MOVE ART-IDLOPNRM     TO W-D1BSEQ-IDLOPNRM                       
102700         PERFORM IMS-GU-INLA3-INLA21-LAST                                 
102800         IF SEGMENT-SAKNAS                                                
102900             MOVE +2           TO SPAR-RAD-IDRADNR                        
103000          ELSE                                                            
103100             COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                   
103200         END-IF                                                           
103300         MOVE SPAR-RAD-W6D121  TO RAD-W6D121                              
103400         MOVE 'INL'            TO RAD-KDINLSTA                            
103500         MOVE SPACE            TO RAD-ADINLOMR                            
103600                                  RAD-ADINLOMR-NXT                        
103700         MOVE ZERO             TO RAD-IDINLVGN                            
103800                                  RAD-IDILIST                             
103900                                  RAD-IDILIRAD                            
103910         MOVE DAGENS-DATUM     TO RAD-TIUPPDAT                            
104000         PERFORM IMS-ISRT-INLA2-INLA21                                    
104100         PERFORM S07-SKAPA-6191-MID-NY-RAD                                
104110         PERFORM S06-SKAPA-6193-MID                                       
104200                                                                          
104300         MOVE +1               TO W-IDRADNR                               
104400         PERFORM IMS-GHU-INLA2-INLA21                                     
104410         PERFORM S08-SKAPA-6191-MID-BORT-RAD                              
104500         PERFORM IMS-DLET-INLA2-INLA21                                    
104800      ELSE                                                                
104900         MOVE 'INL'            TO RAD-KDINLSTA                            
105000         MOVE SPACE            TO RAD-ADINLOMR                            
105100                                  RAD-ADINLOMR-NXT                        
105200         MOVE ZERO             TO RAD-IDINLVGN                            
105300                                  RAD-IDILIST                             
105400                                  RAD-IDILIRAD                            
105410         MOVE DAGENS-DATUM     TO RAD-TIUPPDAT                            
105500         PERFORM IMS-REPL-INLA2-INLA21                                    
105600                                                                          
105700         PERFORM S04-SKAPA-6191-MID                                       
105800         PERFORM S06-SKAPA-6193-MID                                       
105900     END-IF                                                               
106000     .                                                                    
106100     EJECT                                                                
106200 S03-LAES-INLA21         SECTION.                                         
106300                                                                          
106400     MOVE WS-IDILIST           TO W-D1DSEQ-IDILIST                        
106500                                  W-IDILIST                               
106600                                                                          
106700     PERFORM IMS-GU-INLA2-INLA11                                          
106800     PERFORM IMS-GHNP-INLA2-INLA21                                        
106900     MOVE RAD-W6D121           TO SPAR-RAD-W6D121                         
107000     .                                                                    
107100     EJECT                                                                
107200 S04-SKAPA-6191-MID   SECTION.                                            
107300                                                                          
107400     MOVE 'W6014200'           TO MOD6191-MID-IDPGM                       
107410     MOVE WC-CDC-SE            TO MOD6191-MID-IDDC                        
107500     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
107600     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
107700     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
107800     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
107810     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
107820     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
107900                                                                          
108000     MOVE SPAR-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-OLD              
108100                                                   (6191-IX)              
108200     MOVE SPAR-RAD-ADINLOMR-NXT                                           
108300                                 TO MOD6191-MID-ADINLOMR-NXT-OLD          
108400                                                   (6191-IX)              
108500     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD              
108600                                                   (6191-IX)              
108700     MOVE SPAR-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
108800                                                   (6191-IX)              
108900                                                                          
109000     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
109100                                                   (6191-IX)              
109200     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
109300                                                   (6191-IX)              
109400     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
109500                                                   (6191-IX)              
109600     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
109700                                                   (6191-IX)              
109800     ADD +1                    TO 6191-IX                                 
109900     IF 6191-IX                > MAX-6191-IX                              
110000         PERFORM S05-STARTA-W6T191                                        
110100     END-IF                                                               
110200     .                                                                    
110300     EJECT                                                                
110400 S05-STARTA-W6T191         SECTION.                                       
110500                                                                          
110600     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
110700     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
110800                                  17 + (MOD6191-MID-KVPOST * 64)          
110900     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
111000     MOVE '6142'               TO P-TO-P-MSG-IDTRANS                      
111100     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
111200                                                                          
111300     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
111400                                                                          
111500     IF FOERSTA-6191                                                      
111600         PERFORM IMS-ISRT-ALT2-MSG-6191                                   
111700         MOVE NEJ               TO FOERSTA-6191-SW                        
111800      ELSE                                                                
111900         PERFORM IMS-PURG-ALT2-MSG-6191                                   
112000     END-IF                                                               
112100     MOVE +1                   TO 6191-IX                                 
112200     .                                                                    
112300     EJECT                                                                
112400 S06-SKAPA-6193-MID   SECTION.                                            
112500                                                                          
112600     ADD +1                    TO W-CHKP                                  
112700                                                                          
112800     ACCEPT DAGENS-DATUM       FROM DATE                                  
112900     ACCEPT DAGENS-TID         FROM TIME                                  
113000                                                                          
113100     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
113300     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
113400     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
113500     MOVE SPACE                TO MSG-KOM-KDTRANS                         
113600     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
113700     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
113800     MOVE 'W6014200'           TO MSG-KOM-IDSNDJOB                        
113900     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
114000     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
114100     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
114200                                                                          
114300     MOVE ART-IDLOPNRM         TO MOD6193-MID-IDLOPNRM                    
114400     MOVE RAD-IDRADNR          TO MOD6193-MID-IDRADNR                     
114500                                                                          
114600     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX + 12                  
114700     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
114800     MOVE '6142'               TO P-TO-P-MSG-IDTRANS                      
114900     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
115000                                                                          
115100     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
115200                                                                          
115300     CALL W006KOM USING MSG-PCB                                           
115400                        DISP-PCB                                          
115500                        KOM-KOMA-PCB                                      
115600                        MSG-KOM-WMSGKOM                                   
115700                        P-TO-P-MSG-IO-AREA-SNUF                           
115800     .                                                                    
115900     EJECT                                                                
116000 S07-SKAPA-6191-MID-NY-RAD   SECTION.                                     
116100                                                                          
116200     MOVE 'W6014200'           TO MOD6191-MID-IDPGM                       
116210     MOVE WC-CDC-SE            TO MOD6191-MID-IDDC                        
116300     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
116400     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
116500     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
116600     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
116610     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
116620     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
116700                                                                          
116800     MOVE SPAR-RAD-ADINLOMR    TO MOD6191-MID-ADINLOMR-OLD                
116900                                                   (6191-IX)              
117000     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-OLD            
117100                                                   (6191-IX)              
117200                                  MOD6191-MID-KDINLSTA-OLD                
117300                                                   (6191-IX)              
117400     MOVE ZERO                 TO MOD6191-MID-KVINLART-OLD                
117500                                                   (6191-IX)              
117600                                                                          
117700     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
117800                                                   (6191-IX)              
117900     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
118000                                                   (6191-IX)              
118100     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
118200                                                   (6191-IX)              
118300     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
118400                                                   (6191-IX)              
118500     ADD +1                    TO 6191-IX                                 
118600     IF 6191-IX                > MAX-6191-IX                              
118700         PERFORM S05-STARTA-W6T191                                        
118800     END-IF                                                               
118900     .                                                                    
119000     EJECT                                                                
119100 S08-SKAPA-6191-MID-BORT-RAD   SECTION.                                   
119200                                                                          
119300     MOVE 'W6014200'           TO MOD6191-MID-IDPGM                       
119310     MOVE WC-CDC-SE            TO MOD6191-MID-IDDC                        
119400     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
119500     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
119600     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
119700     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
119710     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
119720     MOVE 'J'                  TO MOD6191-MID-FLINLI   (6191-IX)          
119800                                                                          
119900     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-OLD                
120000                                                   (6191-IX)              
120100     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-OLD            
120200                                                   (6191-IX)              
120300     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-OLD                
120400                                                   (6191-IX)              
120500     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
120600                                                   (6191-IX)              
120700                                                                          
120800     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
120900                                                   (6191-IX)              
121000                                  MOD6191-MID-ADINLOMR-NXT-NEW            
121100                                                   (6191-IX)              
121200                                  MOD6191-MID-KDINLSTA-NEW                
121300                                                   (6191-IX)              
121400     MOVE ZERO                 TO MOD6191-MID-KVINLART-NEW                
121500                                                   (6191-IX)              
121600     ADD +1                    TO 6191-IX                                 
121700     IF 6191-IX                > MAX-6191-IX                              
121800         PERFORM S05-STARTA-W6T191                                        
121900     END-IF                                                               
122000     .                                                                    
122100     EJECT                                                                
122110 S09-CALL-W611PMRK      SECTION.                                          
122120                                                                          
122130     MOVE RAD-IDLEVNR-KOLLI    TO PMRK-IDLEVNR                            
122140     MOVE RAD-IDOKOLLI         TO PMRK-IDOKOLLI                           
122150     MOVE ZERO                 TO PMRK-IDLOPNRM                           
122160                                  PMRK-IDRADNR                            
122170     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
122180                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
122190     .                                                                    
122191     EJECT                                                                
122200 MFS-RENSA-FAELT-UT SECTION.                                              
122300                                                                          
122400     MOVE MFS-RENSA-FAELT      TO MOD-IDILIRAD-ENTER                      
122500                                  MOD-IDILIRAD-NEXT                       
122600     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
122700     .                                                                    
122800     SKIP2                                                                
122900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
123000                                                                          
123100     MOVE +1                   TO INDX                                    
123200     PERFORM UNTIL INDX        >  MAX-INDX                                
123300         MOVE MFS-RENSA-FAELT  TO MOD-IDILIRAD-RAD(INDX)                  
123400                                  MOD-ADLAGOMR-RAD(INDX)                  
123500                                  MOD-ADPLATS-RAD (INDX)                  
123600                                  MOD-ADGANG-RAD  (INDX)                  
123700                                  MOD-IDARTNR-RAD (INDX)                  
123800                                  MOD-BEART-RAD   (INDX)                  
123900                                  MOD-KVINLART-RAD(INDX)                  
124000                                  MOD-ADINLOMR-FB-RAD(INDX)               
124100         ADD +1                TO INDX                                    
124200     END-PERFORM                                                          
124300     .                                                                    
124400     EJECT                                                                
124500 MFS-RENSA-FAELT-IN SECTION.                                              
124600                                                                          
124700     MOVE MFS-RENSA-FAELT      TO MOD-FLKLAR                              
124800                                  MOD-IDANSTNR                            
124810                                  MOD-FLKLAR-MAK                          
124900     PERFORM MFS-RENSA-RAD-FAELT-IN                                       
125000     .                                                                    
125100     SKIP2                                                                
125110 MFS-RENSA-FAELT-IN-EJ-ANSTNR SECTION.                                    
125120                                                                          
125130     MOVE MFS-RENSA-FAELT      TO MOD-FLKLAR                              
125150                                  MOD-FLKLAR-MAK                          
125153                                                                          
125154     IF MID-IDANSTNR           =  ALL '+'                                 
125155         MOVE MFS-RENSA-FAELT  TO MOD-IDANSTNR                            
125156      ELSE                                                                
125157         MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR                           
125158         MOVE MFS-ADD-LAES-IN-FAELT                                       
125159                               TO MOD-IDANSTNR-ATTR                       
125160     END-IF                                                               
125161                                                                          
125162     PERFORM MFS-RENSA-RAD-FAELT-IN                                       
125170     .                                                                    
125180     SKIP2                                                                
125200 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
125300                                                                          
125400     MOVE +1                   TO INDX                                    
125500     PERFORM UNTIL INDX        >  MAX-INDX                                
125600         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD    (INDX)              
125700                                  MOD-KVINLART-UPD    (INDX)              
125800                                  MOD-ADINLOMR-NXT-UPD(INDX)              
125900         ADD +1                TO INDX                                    
126000     END-PERFORM                                                          
126100     .                                                                    
126200     EJECT                                                                
126300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
126400                                                                          
126500     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDILIRAD-ENTER                      
126600                                  MOD-IDILIRAD-NEXT                       
126700     PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                     
126800     .                                                                    
126900     SKIP2                                                                
127000 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
127100                                                                          
127200     MOVE +1                    TO INDX                                   
127300     PERFORM UNTIL INDX         >  MAX-INDX                               
127400         MOVE MFS-ROER-EJ-FAELT TO MOD-IDILIRAD-RAD(INDX)                 
127500                                   MOD-ADLAGOMR-RAD(INDX)                 
127600                                   MOD-ADPLATS-RAD (INDX)                 
127700                                   MOD-ADGANG-RAD  (INDX)                 
127800                                   MOD-IDARTNR-RAD (INDX)                 
127900                                   MOD-BEART-RAD   (INDX)                 
128000                                   MOD-KVINLART-RAD(INDX)                 
128100                                   MOD-ADINLOMR-FB-RAD(INDX)              
128200         ADD +1                 TO INDX                                   
128300     END-PERFORM                                                          
128400     .                                                                    
128500     EJECT                                                                
128600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
128700                                                                          
128800     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLKLAR                              
128900                                  MOD-IDANSTNR                            
128910                                  MOD-FLKLAR-MAK                          
129000     PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                     
129100     .                                                                    
129200     SKIP2                                                                
129300 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
129400                                                                          
129500     MOVE +1                    TO INDX                                   
129600     PERFORM UNTIL INDX         >  MAX-INDX                               
129700         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD    (INDX)             
129800                                   MOD-KVINLART-UPD    (INDX)             
129900                                   MOD-ADINLOMR-NXT-UPD(INDX)             
130000         ADD +1                 TO INDX                                   
130100     END-PERFORM                                                          
130200     .                                                                    
130300     EJECT                                                                
130400 MFS-FORM-ATTR SECTION.                                                   
130500                                                                          
130600     MOVE MFS-FORMATETS-ATTR TO MOD-FLKLAR-ATTR                           
130700                                MOD-IDANSTNR-ATTR                         
130710                                MOD-FLKLAR-MAK-ATTR                       
130800     PERFORM MFS-FORM-ATTR-RAD                                            
130900     .                                                                    
131000     SKIP2                                                                
131100 MFS-FORM-ATTR-RAD        SECTION.                                        
131200                                                                          
131300     MOVE +1                   TO INDX                                    
131400     PERFORM UNTIL INDX        >  MAX-INDX                                
131500         MOVE MFS-FORMATETS-ATTR                                          
131600                               TO MOD-KDCMDVAL-RAD-ATTR    (INDX)         
131700                                  MOD-KVINLART-UPD-ATTR    (INDX)         
131800                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
131900         ADD +1                TO INDX                                    
132000     END-PERFORM                                                          
132100     .                                                                    
132200     EJECT                                                                
134200* --- IMS SEKTIONER ---                                                   
134300     SKIP3                                                                
134400 IMS-GET-MSG SECTION.                                                     
134500                                                                          
134600     MOVE '  QC' TO GODK-STATUSKODER                                      
134700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
134800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     SKIP3                                                                
135200 IMS-INSERT-MSG SECTION.                                                  
135300                                                                          
135400*    IF ENGLISH-TEXT                                                      
135500*      MOVE 'N' TO MFS-KDHUVOMR                                           
135600*    END-IF                                                               
135700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
135800     MOVE SPACE TO GODK-STATUSKODER                                       
135900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
136000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     EJECT                                                                
136400 IMS-ISRT-ALT1-MSG-6142  SECTION.                                         
136500     MOVE SPACE TO GODK-STATUSKODER                                       
136600     CALL  CBLTDLI  USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
136700     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000     SKIP3                                                                
137100 IMS-ISRT-ALT2-MSG-6191  SECTION.                                         
137200     MOVE SPACE TO GODK-STATUSKODER                                       
137300     CALL  CBLTDLI  USING ISRT ALT2-PCB P-TO-P-MSG-IO-AREA-SNUF           
137400     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
137500     PERFORM IMS-STATUSKONTROLL                                           
137600     .                                                                    
137700     SKIP3                                                                
137800 IMS-PURG-ALT2-MSG-6191  SECTION.                                         
137900     MOVE SPACE TO GODK-STATUSKODER                                       
138000     CALL  CBLTDLI  USING PURG ALT2-PCB P-TO-P-MSG-IO-AREA-SNUF           
138100     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     SKIP3                                                                
138500 IMS-GU-INLE-INLE01 SECTION.                                              
138600     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
138700                    '&W6D1D1KY<=' W-W6D1D1KY-MAX-X ')'                    
138800          DELIMITED BY SIZE INTO SSA1                                     
138900     MOVE '  GE' TO GODK-STATUSKODER                                      
139000     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA1 SSA1                     
139100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     SKIP2                                                                
139500 IMS-GN-INLE-INLE01 SECTION.                                              
139600     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
139700                    '&W6D1D1KY<=' W-W6D1D1KY-MAX-X ')'                    
139800          DELIMITED BY SIZE INTO SSA1                                     
139900     MOVE '  GE' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GN INLE-PCB DLI-IO-AREA1 SSA1                     
140100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     SKIP2                                                                
140500 IMS-GU-INLA1-INLA11 SECTION.                                             
140600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
140700          DELIMITED BY SIZE INTO SSA1                                     
140800     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
140900          DELIMITED BY SIZE INTO SSA2                                     
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2               
141200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP2                                                                
141600 IMS-GNP-INLA1-INLA21 SECTION.                                            
141700     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
141800          DELIMITED BY SIZE INTO SSA1                                     
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1                   
142100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-GU-INLA2-INLA11 SECTION.                                             
142600     STRING 'W6INLA11(W6D1DSEQ =' W-W6D1DSEQ-X ')'                        
142700          DELIMITED BY SIZE INTO SSA1                                     
142800     MOVE '  GE' TO GODK-STATUSKODER                                      
142900     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA2 SSA1                    
143000     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
143100     PERFORM IMS-STATUSKONTROLL                                           
143200     .                                                                    
143300     SKIP3                                                                
143400 IMS-GHU-INLA2-INLA21 SECTION.                                            
143500     STRING 'W6INLA11(W6D1DSEQ =' W-W6D1DSEQ-X ')'                        
143600          DELIMITED BY SIZE INTO SSA1                                     
143700     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
143800          DELIMITED BY SIZE INTO SSA2                                     
143900     MOVE '  GE' TO GODK-STATUSKODER                                      
144000     CALL CBLTDLI USING GHU INLA2-PCB DLI-IO-AREA3 SSA1 SSA2              
144100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
144200     PERFORM IMS-STATUSKONTROLL                                           
144300     .                                                                    
144400     SKIP3                                                                
144500 IMS-GHNP-INLA2-INLA21 SECTION.                                           
144600     STRING 'W6INLA21(IDILIST  =' W-IDILIST-X                             
144700                    '&IDILIRAD =' W-IDILIRAD-X ')'                        
144800          DELIMITED BY SIZE INTO SSA1                                     
144900     MOVE '  GE' TO GODK-STATUSKODER                                      
145000     CALL CBLTDLI USING GHNP INLA2-PCB DLI-IO-AREA3 SSA1                  
145100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     SKIP3                                                                
145410 IMS-GU-INLA2-INLA11-MIN-MAX SECTION.                                     
145420     STRING 'W6INLA11(W6D1DSEQ>=' W-W6D1DSEQ-MIN-X                        
145421                    '&W6D1DSEQ<=' W-W6D1DSEQ-MAX-X ')'                    
145430          DELIMITED BY SIZE INTO SSA1                                     
145440     MOVE '  GE' TO GODK-STATUSKODER                                      
145450     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA2 SSA1                    
145460     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
145470     PERFORM IMS-STATUSKONTROLL                                           
145480     .                                                                    
145490     SKIP3                                                                
145491 IMS-GN-INLA2-INLA11 SECTION.                                             
145492     STRING 'W6INLA11(W6D1DSEQ>=' W-W6D1DSEQ-MIN-X                        
145493                    '&W6D1DSEQ<=' W-W6D1DSEQ-MAX-X ')'                    
145494          DELIMITED BY SIZE INTO SSA1                                     
145497     MOVE '  GEGB' TO GODK-STATUSKODER                                    
145498     CALL CBLTDLI USING GN INLA2-PCB DLI-IO-AREA2 SSA1                    
145499     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
145500     PERFORM IMS-STATUSKONTROLL                                           
145501     .                                                                    
145502     SKIP3                                                                
145510 IMS-DLET-INLA2-INLA21 SECTION.                                           
145600     MOVE '    ' TO GODK-STATUSKODER                                      
145700     CALL CBLTDLI USING DLET INLA2-PCB DLI-IO-AREA3                       
145800     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
145900     PERFORM IMS-STATUSKONTROLL                                           
146000     .                                                                    
146100     SKIP3                                                                
146200 IMS-REPL-INLA2-INLA21 SECTION.                                           
146300     MOVE '    ' TO GODK-STATUSKODER                                      
146400     CALL CBLTDLI USING REPL INLA2-PCB DLI-IO-AREA3                       
146500     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
146600     PERFORM IMS-STATUSKONTROLL                                           
146700     .                                                                    
146800     SKIP3                                                                
146900 IMS-ISRT-INLA2-INLA21 SECTION.                                           
147000     STRING 'W6INLA11(W6D1DSEQ =' W-W6D1DSEQ-X ')'                        
147100          DELIMITED BY SIZE INTO SSA1                                     
147200     MOVE 'W6INLA21' TO SSA2                                              
147300     MOVE '    ' TO GODK-STATUSKODER                                      
147400     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-AREA3 SSA1 SSA2             
147500     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
147600     PERFORM IMS-STATUSKONTROLL                                           
147700     .                                                                    
147800     SKIP3                                                                
147900 IMS-GU-INLA3-INLA21-LAST SECTION.                                        
148000     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
148100          DELIMITED BY SIZE INTO SSA1                                     
148200     MOVE 'W6INLA21*L' TO SSA2                                            
148300     MOVE '  GE' TO GODK-STATUSKODER                                      
148400     CALL CBLTDLI USING GU INLA3-PCB DLI-IO-AREA3 SSA1 SSA2               
148500     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800     SKIP3                                                                
148900 IMS-GU-PLAA-PLAA11 SECTION.                                              
149000     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
149100          DELIMITED BY SIZE INTO SSA1                                     
149200     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
149300          DELIMITED BY SIZE INTO SSA2                                     
149400     MOVE '  GE' TO GODK-STATUSKODER                                      
149500     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA4 SSA1 SSA2                
149600     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
149700     PERFORM IMS-STATUSKONTROLL                                           
149800     .                                                                    
149900     SKIP3                                                                
150000 IMS-STATUSKONTROLL SECTION.                                              
150100                                                                          
150200     SET STATUS-IX TO 1                                                   
150300     SEARCH GODK-STATUS                                                   
150400       AT END                                                             
150500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
150600         DELIMITED BY SIZE INTO FELTEXT                                   
150700         CALL FELLOG                                                      
150800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
150900         CONTINUE                                                         
151000     END-SEARCH                                                           
151100     .                                                                    
