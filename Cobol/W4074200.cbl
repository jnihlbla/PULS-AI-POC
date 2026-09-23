000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0157      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4074200.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/05/29.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        SKAPAR OCH FYLLER PÅ KOLLIN MED RETURTILLSTÅND HOS RETUR-        
001500*        TERMINALER.                                                      
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001800*        PROGRAMMET LÄSER      WLRETC (WDA3)                              
001900*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
002000*                                                                         
002100*  OBS SKULLE PROGRAMMET BLI DYRT ELLER SEGT. SKAPA NYTT                  
002200*  INDEX MED IDKOLLI SOM INGÅNG OCH LÄS DETTA INDEX ISTÄLLET              
002300*  FÖR WLRETC01.                                                          
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T742                                              
002700*        MID:         W4I74201                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O74201                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4074200'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  W-BORTTAG                   PIC X       VALUE 'B'.                   
004800 77  W-DELETE                    PIC X       VALUE 'D'.                   
004900 77  W-PACKA                     PIC X       VALUE 'P'.                   
005000 77  W-PLUS                      PIC X       VALUE '+'.                   
005100*  INNEHÅLLER X'3F'                                                       
005200 77  W-X3F                       PIC X       VALUE ''.                   
005300 01  W-ANTAL-KOLLI               PIC S9(3)   VALUE +0 COMP-3.             
005400                                                                          
005500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005700 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006900     88  EGEN-MID                            VALUE '4742'.                
007000     88  GODK-MID                            VALUE '4742'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200     EJECT                                                                
007300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007400 01  GENERELLA-SUBPROGRAM.                                                
007500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*01 -COPY WMEDAREA                                                        
008200     SKIP3                                                                
008300 01  MESSAGE-CODES.                                                       
008400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008600     03  ERR-OTILL-UPD           PIC X(3)    VALUE '007'.                 
008700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009200     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
009300     03  ERR-INFO-SAKNAS         PIC X(3)    VALUE '005'.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009800     SKIP3                                                                
009900*01 -COPY WMSGINIT                                                        
010000     SKIP3                                                                
010100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010400     SKIP3                                                                
010500*01  MID -COPY W4I74201                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010800     SKIP3                                                                
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100     03  MOD REDEFINES MSG-AREA.                                          
011200*      05  -COPY W4O74201   -PRE MOD-                                     
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011500     SKIP3                                                                
011600*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800 77  SW-GAMMALT-KOLLI            PIC X       VALUE 'N'.                   
011900     88  GAMMALT-KOLLI                       VALUE 'J'.                   
012000                                                                          
012100 77  SW-VISA-KOLLI               PIC X       VALUE 'N'.                   
012200     88  VISA-KOLLI                          VALUE 'J'.                   
012300                                                                          
012400 77  SW-SKAPA-NYTT-KOLLI         PIC X       VALUE 'N'.                   
012500     88  SKAPA-NYTT-KOLLI                    VALUE 'J'.                   
012600                                                                          
012700 77  SW-SKAPA-FLERA-KOLLI        PIC X       VALUE 'N'.                   
012800     88  SKAPA-FLERA-KOLLI                   VALUE 'J'.                   
012900                                                                          
013000 77  SW-PACKA-I-FLERA-KOLLI      PIC X       VALUE 'N'.                   
013100     88  PACKA-I-FLERA-KOLLI                 VALUE 'J'.                   
013200                                                                          
013300 77  SW-RAD-CMD                  PIC X       VALUE 'N'.                   
013400     88  RAD-CMD                             VALUE 'J'.                   
013500                                                                          
013600 77  SW-INM-RAD                  PIC X       VALUE 'N'.                   
013700     88  INM-RAD                             VALUE 'J'.                   
013800                                                                          
013900 77  SW-KOLLI                    PIC X       VALUE 'N'.                   
014000     88  KOLLI-FINNS                         VALUE 'J'.                   
014100                                                                          
014200 77  SW-FORSTA-VALDA-RAD         PIC X       VALUE 'N'.                   
014300     88  FORSTA-VALDA-RAD                    VALUE 'J'.                   
014400*      --- VALID IDDC CODES                                               
014500*                                                                         
014600*01    -COPY WWDCKONS                                                     
014700       EJECT                                                              
014800                                                                          
014900 77  W-FLVISA                    PIC X       VALUE 'N'.                   
015000 77  W-IDPERSON                  PIC S9(3)   VALUE ZERO COMP-3.           
015100 77  W-KDARBTYP                  PIC X(8)    VALUE SPACE.                 
015200 77  W-KLI-PACKAT                PIC S9(1)   VALUE +1   COMP-3.           
015300 77  W-IDKOLLI-NUM               PIC  9(5)   VALUE ZERO.                  
015400 77  W-IDKOLLI-FOM               PIC  9(5)   VALUE ZERO.                  
015500 77  W-IDKOLLI-TOM               PIC  9(5)   VALUE ZERO.                  
015600 77  W-KVKOLLI                   PIC  9(4)   VALUE ZERO.                  
015700 77  W-SPAR-FLFARLIG             PIC X       VALUE SPACE.                 
015800     EJECT                                                                
015900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016000*                                                                         
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016300     SKIP3                                                                
016400 01  W-MINKEY-X.                                                          
016500     03  W-MINKEY-IDTRANS          PIC  X(4)   VALUE '4742'.              
016600     03  W-MINKEY-WDA3ASEQ-ENTER.                                         
016800         05  W-MINKEYA1-IDRT       PIC  X(3)          VALUE SPACE.        
016810         05  W-MINKEYA1-IDDC       PIC  X(2)          VALUE SPACE.        
016900         05  W-MINKEYA1-IDDISTR    PIC S9(5)   COMP-3 VALUE ZERO.         
017000         05  W-MINKEYA1-IDKUNDNR   PIC S9(7)   COMP-3 VALUE ZERO.         
017100         05  W-MINKEYA1-IDRAPPNR   PIC  9(7)          VALUE ZERO.         
017200                                                                          
017300     03  W-MINKEY-WDA3BSEQ-ENTER.                                         
017500         05  W-MINKEYB1-IDRT       PIC  X(3)          VALUE SPACE.        
017600         05  W-MINKEYB1-IDDC       PIC  X(2)          VALUE SPACE.        
017700         05  W-MINKEYB1-IDRTLOP    PIC  9(3)          VALUE ZERO.         
017800         05  W-MINKEYB1-IDKOLLI-X.                                        
017900          07  W-MINKEYB1-IDKOLLI    PIC S9(5)   COMP-3 VALUE ZERO.        
018000     03  W-MINKEY-WDA3ASEQ-NEXT.                                          
018200         05  W-MINKEYA1-IDRT-NEXT    PIC  X(3)        VALUE SPACE.        
018210         05  W-MINKEYA1-IDDC-NEXT    PIC  X(2)        VALUE SPACE.        
018300         05  W-MINKEYA1-IDDISTR-NEXT PIC S9(5) COMP-3 VALUE ZERO.         
018400         05  W-MINKEYA1-IDKUNDNR-NEXT PIC S9(7) COMP-3 VALUE ZERO.        
018500         05  W-MINKEYA1-IDRAPPNR-NEXT PIC  9(7)        VALUE ZERO.        
018600                                                                          
018700     03  W-MINKEY-WDA3BSEQ-NEXT.                                          
018900         05  W-MINKEYB1-IDRT-NEXT  PIC  X(3)          VALUE SPACE.        
018910         05  W-MINKEYB1-IDDC-NEXT  PIC  X(2)          VALUE SPACE.        
019000         05  W-MINKEYB1-IDRTLOP-NEXT PIC  9(3)        VALUE ZERO.         
019100         05  W-MINKEYB1-IDKOLLI-NEXT-X.                                   
019200          07 W-MINKEYB1-IDKOLLI-NEXT PIC S9(5)  COMP-3 VALUE ZERO.        
019300         05  W-MINKEYB1-ANTAL-VISADE PIC S9(5)  COMP-3 VALUE ZERO.        
019400     SKIP3                                                                
019500 01  NYCKLAR-TILL-DLI.                                                    
019600     03  W-WDA301KY-X.                                                    
019700         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
019800         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
019900         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
020000                                                                          
020100     03  W-WDA3ASEQ-MIN-X.                                                
020200         05  W-IDRT-ASEQ-MIN     PIC  X(3)   VALUE SPACE.                 
020300         05  W-IDDC-ASEQ-MIN     PIC  X(2)   VALUE SPACE.                 
020400         05  W-IDDISTR-ASEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
020500         05  W-IDKUNDNR-ASEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
020600         05  W-IDRAPPNR-ASEQ-MIN PIC  9(7)   VALUE ZERO.                  
020700                                                                          
020800     03  W-WDA3ASEQ-MAX-X.                                                
020900         05  W-IDRT-ASEQ-MAX     PIC  X(3)   VALUE SPACE.                 
021000         05  W-IDDC-ASEQ-MAX     PIC  X(2)   VALUE SPACE.                 
021100         05  W-IDDISTR-ASEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
021200         05  W-IDKUNDNR-ASEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
021300         05  W-IDRAPPNR-ASEQ-MAX PIC  9(7)   VALUE ZERO.                  
021400                                                                          
021500     03  W-WDA3FSEQ-MIN-X.                                                
021600         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
021700         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
021800         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
021900         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
022000                                                                          
022100     03  W-WDA3FSEQ-MAX-X.                                                
022200         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
022300         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
022400         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
022500         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
022600                                                                          
022700     03  W-WDA3BSEQ-MIN-X.                                                
022800         05  W-IDRT-BSEQ-MIN     PIC  X(3)          VALUE SPACE.          
022900         05  W-IDDC-BSEQ-MIN     PIC  X(2)          VALUE SPACE.          
023000         05  W-IDRTLOP-BSEQ-MIN  PIC  9(3)          VALUE ZERO.           
023100         05  W-IDKOLLI-BSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
023200                                                                          
023300     03  W-WDA3BSEQ-MAX-X.                                                
023400         05  W-IDRT-BSEQ-MAX     PIC  X(3)          VALUE SPACE.          
023500         05  W-IDDC-BSEQ-MAX     PIC  X(2)          VALUE SPACE.          
023600         05  W-IDRTLOP-BSEQ-MAX  PIC  9(3)          VALUE ZERO.           
023700         05  W-IDKOLLI-BSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
023800                                                                          
023900     03  W-WDA3B1KY-MIN-X.                                                
024000         05  W-IDRT-B1-MIN       PIC  X(3)          VALUE SPACE.          
024100         05  W-IDDC-B1-MIN       PIC  X(2)          VALUE SPACE.          
024200         05  W-IDRTLOP-B1-MIN    PIC  9(3)          VALUE ZERO.           
024300         05  W-IDKOLLI-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
024400         05  W-DAREGDAT-B1-MIN   PIC  9(8)          VALUE ZERO.           
024500         05  W-TIKLOCK-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
024600                                                                          
024700     03  W-WDA3B1KY-MAX-X.                                                
024800         05  W-IDRT-B1-MAX       PIC  X(3)          VALUE SPACE.          
024900         05  W-IDDC-B1-MAX       PIC  X(2)          VALUE SPACE.          
025000         05  W-IDRTLOP-B1-MAX    PIC  9(3)          VALUE ZERO.           
025100         05  W-IDKOLLI-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
025200         05  W-DAREGDAT-B1-MAX   PIC  9(8)          VALUE ZERO.           
025300         05  W-TIKLOCK-B1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
025400                                                                          
025500     03  W-WDGXKEY-X.                                                     
025600         05  W-IDHTYP            PIC  X(4)   VALUE '4111'.                
025700         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
025800         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
025900                                                                          
026000     03  W-IDKOLLI-X.                                                     
026100         05  W-IDKOLLI           PIC S9(5)   COMP-3 VALUE ZERO.           
026200     SKIP2                                                                
026300*    --- STATUS-KOD FRÅN IMS                                              
026400 01  STATUS-WS                   PIC XX.                                  
026500     88  SEGMENT-FINNS                       VALUE '  '.                  
026600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
026900     SKIP2                                                                
027000 01  GODK-STATUSKODER.                                                    
027100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027200     SKIP3                                                                
027300 01  SSA1                        PIC X(128).                              
027400 01  SSA2                        PIC X(64).                               
027500     EJECT                                                                
027600*    --- IMS FUNKTIONSKODER                                               
027700*01  -COPY W0003                                                          
027800     EJECT                                                                
027900*    ---  DLI INPUT-OUTPUT AREA                                           
028000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
028100     SKIP3                                                                
028200 01  DLI-IO-AREA.                                                         
028300     03  IO-AREA                 PIC X(192)  VALUE SPACE.                 
028400     SKIP3                                                                
028500     03  WLRETA01 REDEFINES IO-AREA.                                      
028600*        05  -COPY WDA301                                                 
028700     EJECT                                                                
028800     03  WLRETA01 REDEFINES IO-AREA.                                      
028900*        05  -COPY WDA3B1                                                 
029000     EJECT                                                                
029100     03  WL411111 REDEFINES IO-AREA.                                      
029200*        05  -COPY WDGX4112                                               
029300     EJECT                                                                
029400 LINKAGE SECTION.                                                         
029500                                                                          
029600*01  -COPY W0009   -PRE MSG-                                              
029700*01  -COPY W0008   -PRE USEA-                                             
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE RETA1-                                             
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE RETA2-                                             
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE RETA3-                                             
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008  -PRE RETA4-                                             
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008  -PRE RETC-                                              
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE 4111-                                              
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
031900                           RETA1-PCB RETA2-PCB RETA3-PCB                  
032000                           RETA4-PCB RETC-PCB 4111-PCB.                   
032100     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
032200                           RETA1-PCB RETA2-PCB RETA3-PCB                  
032300                           RETA4-PCB RETC-PCB 4111-PCB.                   
032400                                                                          
032500     PERFORM IMS-GET-MSG                                                  
032600     IF SEGMENT-FINNS                                                     
032700       PERFORM A-INIT                                                     
032800       PERFORM B-KOLLA-NYCKLAR                                            
032900       IF NYCKLAR-OK                                                      
033000         MOVE MID-MODFAELT-IN    TO MOD-INPUT                             
033100         INSPECT MOD-INPUT REPLACING ALL W-PLUS BY W-X3F                  
033200         IF (EGEN-MID AND NOT MFS-FIRST) OR HELP-MID                      
033300           PERFORM G-KOLLA-INPUT                                          
033400         END-IF                                                           
033500         IF MFS-UPDATE                                                    
033600           IF INDATA-OK                                                   
033700             PERFORM H-UPPDATERA                                          
033800           END-IF                                                         
033900         ELSE                                                             
034000           IF MFS-FIRST                                                   
034100             PERFORM C-FOERSTA-SIDA                                       
034200           ELSE                                                           
034300             IF MFS-NEXT                                                  
034400               PERFORM D-NAESTA-SIDA                                      
034500             ELSE                                                         
034600               PERFORM E-SAMMA-SIDA                                       
034700             END-IF                                                       
034800           END-IF                                                         
034900         END-IF                                                           
035000         PERFORM F-LAES-VISA-INFO                                         
035100       END-IF                                                             
035200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O74201 + 4                      
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
036300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74201                 
036400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036600     ELSE                                                                 
036700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I74201                  
036800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037000     END-IF                                                               
037100                                                                          
037200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
037400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037500                                                                          
037600     MOVE LOW-VALUE   TO MSG-AREA                                         
037700     MOVE 'W4O74201'  TO MFS-IDMOD                                        
037800     MOVE '4742' TO MOD-IDTRANS                                           
037900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
038000                                                                          
038100     IF EGEN-MID OR HELP-MID                                              
038200       CONTINUE                                                           
038300     ELSE                                                                 
038400       MOVE SPACE TO MFS-KDTRTYP                                          
038500       MOVE '7' TO MFS-IDPFK                                              
038600     END-IF                                                               
038700                                                                          
038800     MOVE LOW-VALUE      TO W-WDA3ASEQ-MIN-X                              
038900                            W-WDA3BSEQ-MIN-X                              
039000                            W-WDA3FSEQ-MIN-X                              
039100                            W-WDA3B1KY-MIN-X                              
039200                                                                          
039300     MOVE HIGH-VALUE     TO W-WDA3ASEQ-MAX-X                              
039400                            W-WDA3BSEQ-MAX-X                              
039500                            W-WDA3FSEQ-MAX-X                              
039600                            W-WDA3B1KY-MAX-X                              
039700                                                                          
039800     .                                                                    
039900     EJECT                                                                
040000 B-KOLLA-NYCKLAR SECTION.                                                 
040100                                                                          
040200     MOVE ALL '+'              TO MSGI-WMSGINIT                           
040300     MOVE '001'                TO MSGI-KDCALL                             
040400     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
040500     MOVE '4742'               TO MSGI-IDTRANS                            
040600     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
040700     IF GODK-MID                                                          
040800         MOVE MID-IDKOLLI-IN   TO MSGI-IDKOLLI                            
040900     END-IF                                                               
041000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041100                                                                          
041200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
041300                                                                          
041400     MOVE JA TO NYCKLAR-SW                                                
041500                                                                          
041600     MOVE WC-CDC-SE      TO W-IDDC-ASEQ-MIN                               
041700                            W-IDDC-BSEQ-MIN                               
041800                            W-IDDC-FSEQ-MIN                               
041900                            W-IDDC-B1-MIN                                 
042000                            W-IDDC                                        
042100                            W-IDDC-ASEQ-MAX                               
042200                            W-IDDC-BSEQ-MAX                               
042300                            W-IDDC-FSEQ-MAX                               
042400                            W-IDDC-B1-MAX                                 
042500                                                                          
042600     MOVE MSGI-IDRT-KEY    TO W-IDRT-ASEQ-MIN                             
042700                              W-IDRT-ASEQ-MAX                             
042800                              W-IDRT-BSEQ-MIN                             
042900                              W-IDRT-BSEQ-MAX                             
043000                              W-IDRT-B1-MIN                               
043100                              W-IDRT-B1-MAX                               
043200                              W-IDRT-4111                                 
043300                                                                          
043400*    -- KONTROLL AV IDKOLLI                                               
043500     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
043600     MOVE NEJ             TO SW-GAMMALT-KOLLI                             
043700                                                                          
043800     IF MID-IDKOLLI-IN    NOT = ALL '+'                                   
043900       MOVE '7'           TO MFS-IDPFK                                    
044000       MOVE SPACE         TO MFS-KDTRTYP                                  
044100     END-IF                                                               
044200                                                                          
044300     IF MSGI-IDKOLLI      NUMERIC AND                                     
044400        MSGI-IDKOLLI      > ZERO                                          
044500        MOVE JA           TO SW-GAMMALT-KOLLI                             
044600        MOVE MSGI-IDKOLLI TO W-IDKOLLI-BSEQ-MIN                           
044700                             W-IDKOLLI-BSEQ-MAX                           
044800                             W-IDKOLLI                                    
044900     END-IF                                                               
045000                                                                          
045100*    -- KONTROLL AV FLAGGA VISA KOLLIINNEHÅLL                             
045200     MOVE MFS-RENSA-FAELT TO MOD-FLVISA-IN                                
045300                                                                          
045400     IF MID-FLVISA-IN      = ALL '+'                                      
045500       MOVE MID-FLVISA-UT TO W-FLVISA                                     
045600     ELSE                                                                 
045700       MOVE '7'           TO MFS-IDPFK                                    
045800       MOVE SPACE         TO MFS-KDTRTYP                                  
045900       MOVE MID-FLVISA-IN TO W-FLVISA                                     
046000     END-IF                                                               
046100                                                                          
046200     IF W-FLVISA          = JA OR YES                                     
046300        MOVE JA           TO SW-VISA-KOLLI                                
046400     ELSE                                                                 
046500        MOVE NEJ          TO SW-VISA-KOLLI                                
046600                             W-FLVISA                                     
046700     END-IF                                                               
046800                                                                          
046900     IF VISA-KOLLI AND MSGI-IDKOLLI NOT NUMERIC                           
047000        MOVE NEJ          TO NYCKLAR-SW                                   
047100     END-IF                                                               
047200                                                                          
047300     IF GODK-MID OR NYCKLAR-OK                                            
047400       IF MSGI-IDKOLLI NUMERIC                                            
047500          MOVE MSGI-IDKOLLI      TO MOD-IDKOLLI-UT                        
047600          INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE          
047700       ELSE                                                               
047800          MOVE MFS-RENSA-FAELT   TO MOD-IDKOLLI-UT                        
047900       END-IF                                                             
048000       IF W-FLVISA = JA                                                   
048100         IF MSGI-IDLAND-SPR = 'GB '                                       
048200           MOVE YES                  TO MOD-FLVISA-UT                     
048300         ELSE                                                             
048400           MOVE W-FLVISA             TO MOD-FLVISA-UT                     
048500         END-IF                                                           
048600       ELSE                                                               
048700         MOVE W-FLVISA             TO MOD-FLVISA-UT                       
048800       END-IF                                                             
048900     ELSE                                                                 
049000       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-UT                        
049100                                    MOD-FLVISA-UT                         
049200     END-IF                                                               
049300                                                                          
049400     IF MSGI-IDRT-KEY = SPACE OR                                          
049500        MSGI-IDRT-KEY = 'CDC' OR                                          
049600        MSGI-IDRT-KEY = 'US1' OR                                          
049800        MSGI-IDRT-KEY = 'US3' OR                                          
049810        MSGI-IDRT-KEY = 'US4' OR                                          
049820        MSGI-IDRT-KEY = 'US5' OR                                          
049830        MSGI-IDRT-KEY = 'US6' OR                                          
049840        MSGI-IDRT-KEY = 'ET2' OR                                          
049900        MSGI-IDRT-KEY = 'CA1'                                             
050000        MOVE NEJ            TO NYCKLAR-SW                                 
050100     END-IF                                                               
050200                                                                          
050300     IF NYCKLAR-FEL                                                       
050400       IF MSGI-IDRT-KEY = SPACE OR                                        
050500          MSGI-IDRT-KEY = 'CDC' OR                                        
050600          MSGI-IDRT-KEY = 'US1' OR                                        
050800          MSGI-IDRT-KEY = 'US3' OR                                        
050810          MSGI-IDRT-KEY = 'US4' OR                                        
050820          MSGI-IDRT-KEY = 'US5' OR                                        
050830          MSGI-IDRT-KEY = 'US6' OR                                        
050840          MSGI-IDRT-KEY = 'ET2' OR                                        
050900          MSGI-IDRT-KEY = 'CA1'                                           
051000         MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-UT                           
051100                                 MOD-FLVISA-UT                            
051200         MOVE ERR-OTILL-UPD   TO MED-IDMFSFEL                             
051300       ELSE                                                               
051400         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
051500       END-IF                                                             
051600       CALL WMEDKONV USING MED-WMEDAREA                                   
051700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
051800       PERFORM MFS-RENSA-FAELT-IN                                         
051900       PERFORM MFS-RENSA-FAELT-UT                                         
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300 C-FOERSTA-SIDA SECTION.                                                  
052400                                                                          
052500     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
052600     CALL WMEDKONV USING MED-WMEDAREA                                     
052700     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
052800                                                                          
052900*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
053000     PERFORM MFS-RENSA-FAELT-IN                                           
053100     .                                                                    
053200     EJECT                                                                
053300 D-NAESTA-SIDA SECTION.                                                   
053400                                                                          
053500     IF VISA-KOLLI                                                        
053600        MOVE MSGI-SPAR-AREA      TO W-MINKEY-X                            
053700        IF W-MINKEY-IDTRANS = '4742'                                      
053800          MOVE W-MINKEY-WDA3BSEQ-NEXT    TO W-WDA3BSEQ-MIN-X              
053900          MOVE W-MINKEYB1-IDKOLLI-NEXT-X TO W-IDKOLLI-X                   
054000        ELSE                                                              
054100          MOVE LOW-VALUE              TO W-WDA3ASEQ-MIN-X                 
054200                                         W-WDA3BSEQ-MIN-X                 
054300          MOVE WC-CDC-SE              TO W-IDDC-ASEQ-MIN                  
054400                                         W-IDDC-BSEQ-MIN                  
054500          PERFORM MFS-RENSA-FAELT-IN                                      
054600        END-IF                                                            
054700     ELSE                                                                 
054800        MOVE MSGI-SPAR-AREA      TO W-MINKEY-X                            
054900        IF W-MINKEY-IDTRANS = '4742'                                      
055000          MOVE W-MINKEY-WDA3ASEQ-NEXT TO W-WDA3ASEQ-MIN-X                 
055100        ELSE                                                              
055200          MOVE LOW-VALUE              TO W-WDA3ASEQ-MIN-X                 
055300                                         W-WDA3BSEQ-MIN-X                 
055400          MOVE WC-CDC-SE              TO W-IDDC-ASEQ-MIN                  
055500                                         W-IDDC-BSEQ-MIN                  
055600          PERFORM MFS-RENSA-FAELT-IN                                      
055700        END-IF                                                            
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 E-SAMMA-SIDA SECTION.                                                    
056200                                                                          
056300     IF VISA-KOLLI                                                        
056400        MOVE MSGI-SPAR-AREA      TO W-MINKEY-X                            
056500        IF W-MINKEY-IDTRANS = '4742'                                      
056600          MOVE W-MINKEY-WDA3BSEQ-ENTER TO W-WDA3BSEQ-MIN-X                
056700          MOVE W-MINKEYB1-IDKOLLI-X    TO W-IDKOLLI-X                     
056800        ELSE                                                              
056900          MOVE LOW-VALUE              TO W-WDA3ASEQ-MIN-X                 
057000                                         W-WDA3BSEQ-MIN-X                 
057100          MOVE WC-CDC-SE              TO W-IDDC-ASEQ-MIN                  
057200                                         W-IDDC-BSEQ-MIN                  
057300          PERFORM MFS-RENSA-FAELT-IN                                      
057400        END-IF                                                            
057500     ELSE                                                                 
057600        MOVE MSGI-SPAR-AREA      TO W-MINKEY-X                            
057700        IF W-MINKEY-IDTRANS = '4742'                                      
057800          MOVE W-MINKEY-WDA3ASEQ-ENTER TO W-WDA3ASEQ-MIN-X                
057900        ELSE                                                              
058000          MOVE LOW-VALUE              TO W-WDA3ASEQ-MIN-X                 
058100                                         W-WDA3BSEQ-MIN-X                 
058200          MOVE WC-CDC-SE              TO W-IDDC-ASEQ-MIN                  
058300                                         W-IDDC-BSEQ-MIN                  
058400          PERFORM MFS-RENSA-FAELT-IN                                      
058500        END-IF                                                            
058600     END-IF                                                               
058700     IF MID-INPUT                = ALL '+'                                
058800       PERFORM MFS-RENSA-FAELT-IN                                         
058900     ELSE                                                                 
059000       MOVE INF-PRESS-PF11       TO MED-IDMFSINF                          
059100       CALL WMEDKONV USING MED-WMEDAREA                                   
059200       MOVE MED-MFSINF           TO MOD-TEMFSFEL                          
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 F-LAES-VISA-INFO SECTION.                                                
059700                                                                          
059800     IF VISA-KOLLI                                                        
059900        PERFORM FA-LAES-PACKAT-KOLLI                                      
060000     ELSE                                                                 
060100        PERFORM FB-LAES-OPACKADE-RAPPORTER                                
060200     END-IF                                                               
060300     MOVE '002'                     TO MSGI-KDCALL                        
060400     MOVE '4742'                    TO MSGI-IDTRANS                       
060500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
060600     .                                                                    
060700     EJECT                                                                
060800 FA-LAES-PACKAT-KOLLI SECTION.                                            
060900                                                                          
061000     MOVE ZERO                  TO W-IDRTLOP-BSEQ-MIN                     
061100     MOVE 999                   TO W-IDRTLOP-BSEQ-MAX                     
061200     PERFORM IMS-GU-SEQB-WLRETA01                                         
061300     IF W-MINKEYB1-ANTAL-VISADE > ZERO                                    
061400       MOVE +1 TO INDX                                                    
061500       PERFORM UNTIL INDX  > W-MINKEYB1-ANTAL-VISADE                      
061600                  OR SEGMENT-SAKNAS                                       
061700           PERFORM IMS-GN-SEQB-WLRETA01                                   
061800           ADD  +1 TO INDX                                                
061900       END-PERFORM                                                        
062000     END-IF                                                               
062100                                                                          
062200     PERFORM FAA-FIXA-ENTER-KEY                                           
062300                                                                          
062400     IF SEGMENT-SAKNAS                                                    
062500        MOVE ERR-KOLLI-SAKNAS   TO MED-IDMFSFEL                           
062600        CALL WMEDKONV USING MED-WMEDAREA                                  
062700        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
062800        PERFORM MFS-RENSA-FAELT-UT                                        
062900     ELSE                                                                 
063000       MOVE RET-FLFARLIG        TO MOD-FLFARLIG-KOLLI                     
063100       MOVE +1                  TO INDX                                   
063200                                                                          
063300       PERFORM UNTIL INDX       > MAX-INDX                                
063400         IF SEGMENT-FINNS                                                 
063500                                                                          
063600           PERFORM S01-REDIGERA-MOD                                       
063700           PERFORM IMS-GN-SEQB-WLRETA01                                   
063800         ELSE                                                             
063900           MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR  (INDX)                
064000           MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR     (INDX)                
064100                                    MOD-IDKUNDNR    (INDX)                
064200                                    MOD-IDRAPPNR    (INDX)                
064300                                    MOD-KVKOLLI-AAF (INDX)                
064400                                    MOD-FLFARLIG    (INDX)                
064500         END-IF                                                           
064600         ADD 1 TO INDX                                                    
064700       END-PERFORM                                                        
064800                                                                          
064900       PERFORM FAB-FIXA-NEXT-KEY                                          
065000                                                                          
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400 FAA-FIXA-ENTER-KEY        SECTION.                                       
065500                                                                          
065600     IF SEGMENT-FINNS                                                     
065700        MOVE RET-IDDC               TO W-MINKEYB1-IDDC                    
065800        MOVE RET-IDRT               TO W-MINKEYB1-IDRT                    
065900        MOVE RET-IDRTLOP            TO W-MINKEYB1-IDRTLOP                 
066000        MOVE RET-IDKOLLI            TO W-MINKEYB1-IDKOLLI                 
066100     ELSE                                                                 
066200        MOVE WC-CDC-SE              TO W-MINKEYB1-IDDC                    
066300        MOVE ZERO                   TO W-MINKEYB1-IDRT                    
066400                                       W-MINKEYB1-IDRTLOP                 
066500                                       W-MINKEYB1-IDKOLLI                 
066600                                       W-MINKEYB1-ANTAL-VISADE            
066700     END-IF                                                               
066800     MOVE '4742'                    TO W-MINKEY-IDTRANS                   
066900     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
067000                                                                          
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 FAB-FIXA-NEXT-KEY        SECTION.                                        
067500                                                                          
067600     IF SEGMENT-FINNS                                                     
067700        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
067800        CALL WMEDKONV USING MED-WMEDAREA                                  
067900        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
068000                                                                          
068100        MOVE RET-IDDC               TO W-MINKEYB1-IDDC-NEXT               
068200        MOVE RET-IDRT               TO W-MINKEYB1-IDRT-NEXT               
068300        MOVE RET-IDRTLOP            TO W-MINKEYB1-IDRTLOP-NEXT            
068400        MOVE RET-IDKOLLI            TO W-MINKEYB1-IDKOLLI-NEXT            
068500        COMPUTE W-MINKEYB1-ANTAL-VISADE =                                 
068600                W-MINKEYB1-ANTAL-VISADE + 10                              
068700     ELSE                                                                 
068800        MOVE WC-CDC-SE              TO W-MINKEYB1-IDDC-NEXT               
068900        MOVE ZERO                   TO W-MINKEYB1-IDRT-NEXT               
069000                                       W-MINKEYB1-IDRTLOP-NEXT            
069100                                       W-MINKEYB1-IDKOLLI-NEXT            
069200                                       W-MINKEYB1-ANTAL-VISADE            
069300     END-IF                                                               
069400     MOVE '4742'                    TO W-MINKEY-IDTRANS                   
069500     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
069600                                                                          
069700     .                                                                    
069800     EJECT                                                                
069900 FB-LAES-OPACKADE-RAPPORTER SECTION.                                      
070000                                                                          
070100     PERFORM FBA-LAES-KOLLI                                               
070200                                                                          
070300     PERFORM IMS-GU-SEQA-WLRETA01                                         
070400     PERFORM FBB-FIXA-ENTER-KEY                                           
070500     IF SEGMENT-SAKNAS                                                    
070600        MOVE ERR-INFO-SAKNAS    TO MED-IDMFSFEL                           
070700        CALL WMEDKONV USING MED-WMEDAREA                                  
070800        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
070900        PERFORM MFS-RENSA-FAELT-UT                                        
071000     ELSE                                                                 
071100        MOVE +1                  TO INDX                                  
071200                                                                          
071300        PERFORM UNTIL INDX       > MAX-INDX                               
071400          IF SEGMENT-FINNS                                                
071500                                                                          
071600            PERFORM S01-REDIGERA-MOD                                      
071700            PERFORM IMS-GN-SEQA-WLRETA01                                  
071800          ELSE                                                            
071900            MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR (INDX)                
072000            MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR  (INDX)                
072100                                     MOD-IDKUNDNR (INDX)                  
072200                                     MOD-IDRAPPNR (INDX)                  
072300                                     MOD-KVKOLLI-AAF (INDX)               
072400                                     MOD-FLFARLIG (INDX)                  
072500          END-IF                                                          
072600          ADD 1 TO INDX                                                   
072700        END-PERFORM                                                       
072800                                                                          
072900        PERFORM FBC-FIXA-NEXT-KEY                                         
073000                                                                          
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400                                                                          
073500 FBA-LAES-KOLLI           SECTION.                                        
073600                                                                          
073700     MOVE ZERO                     TO W-IDRTLOP-BSEQ-MIN                  
073800                                      W-IDRTLOP-BSEQ-MAX                  
073900     PERFORM IMS-GU-SEQB-WLRETA01                                         
074000     IF SEGMENT-FINNS                                                     
074100        MOVE RET-FLFARLIG          TO MOD-FLFARLIG-KOLLI                  
074200     ELSE                                                                 
074300        MOVE MFS-RENSA-FAELT       TO MOD-FLFARLIG-KOLLI                  
074400     END-IF                                                               
074500                                                                          
074600     .                                                                    
074700     EJECT                                                                
074800 FBB-FIXA-ENTER-KEY        SECTION.                                       
074900                                                                          
075000     IF SEGMENT-FINNS                                                     
075100        MOVE RET-IDDC               TO W-MINKEYA1-IDDC                    
075200        MOVE RET-IDRT               TO W-MINKEYA1-IDRT                    
075300        MOVE RET-IDDISTR            TO W-MINKEYA1-IDDISTR                 
075400        MOVE RET-IDKUNDNR           TO W-MINKEYA1-IDKUNDNR                
075500        MOVE RET-IDRAPPNR           TO W-MINKEYA1-IDRAPPNR                
075600     ELSE                                                                 
075700        MOVE WC-CDC-SE              TO W-MINKEYA1-IDDC                    
075800        MOVE MSGI-IDRT-KEY          TO W-MINKEYA1-IDRT                    
075900        MOVE ZERO                   TO W-MINKEYA1-IDDISTR                 
076000                                       W-MINKEYA1-IDKUNDNR                
076100                                       W-MINKEYA1-IDRAPPNR                
076200     END-IF                                                               
076300     MOVE '4742'                    TO W-MINKEY-IDTRANS                   
076400     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
076500                                                                          
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076900 FBC-FIXA-NEXT-KEY        SECTION.                                        
077000                                                                          
077100     IF SEGMENT-FINNS                                                     
077200        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
077300        CALL WMEDKONV USING MED-WMEDAREA                                  
077400        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
077500                                                                          
077600        MOVE RET-IDDC              TO W-MINKEYA1-IDDC-NEXT                
077700        MOVE RET-IDRT              TO W-MINKEYA1-IDRT-NEXT                
077800        MOVE RET-IDDISTR           TO W-MINKEYA1-IDDISTR-NEXT             
077900        MOVE RET-IDKUNDNR          TO W-MINKEYA1-IDKUNDNR-NEXT            
078000        MOVE RET-IDRAPPNR          TO W-MINKEYA1-IDRAPPNR-NEXT            
078100     ELSE                                                                 
078200        MOVE WC-CDC-SE              TO W-MINKEYA1-IDDC-NEXT               
078300        MOVE MSGI-IDRT-KEY          TO W-MINKEYA1-IDRT-NEXT               
078400        MOVE ZERO                   TO W-MINKEYA1-IDDISTR-NEXT            
078500                                       W-MINKEYA1-IDKUNDNR-NEXT           
078600                                       W-MINKEYA1-IDRAPPNR-NEXT           
078700     END-IF                                                               
078800     MOVE '4742'                    TO W-MINKEY-IDTRANS                   
078900     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
079000                                                                          
079100     .                                                                    
079200     EJECT                                                                
079300                                                                          
079400                                                                          
079500 G-KOLLA-INPUT SECTION.                                                   
079600                                                                          
079700     MOVE JA               TO INDATA-SW                                   
079800     MOVE NEJ              TO SW-KOLLI                                    
079900                                                                          
080000     PERFORM GA-FORMELL-KONTROLL                                          
080100     IF INDATA-OK                                                         
080200        PERFORM GB-LOGISK-KONTROLL                                        
080300     END-IF                                                               
080400                                                                          
080500     IF INDATA-FEL                                                        
080600        IF MED-IDMFSFEL = ZERO                                            
080700          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
080800        END-IF                                                            
080900        CALL WMEDKONV USING MED-WMEDAREA                                  
081000        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
081100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
081200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
081300     END-IF                                                               
081400                                                                          
081500     .                                                                    
081600     EJECT                                                                
081700                                                                          
081800 GA-FORMELL-KONTROLL SECTION.                                             
081900                                                                          
082000     MOVE ZERO       TO MED-IDMFSFEL                                      
082100                                                                          
082200     IF MID-INPUT                = ALL '+' AND MFS-UPDATE                 
082300       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
082400       CALL WMEDKONV USING MED-WMEDAREA                                   
082500       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
082600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
082700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
082800       MOVE NEJ                  TO INDATA-SW                             
082900     ELSE                                                                 
083000                                                                          
083100       MOVE NEJ                   TO SW-PACKA-I-FLERA-KOLLI               
083200       INSPECT MID-IDKOLLI-FOM REPLACING LEADING SPACE BY ZERO            
083300       INSPECT MID-IDKOLLI-TOM REPLACING LEADING SPACE BY ZERO            
083400       IF MID-IDKOLLI-FOM         NUMERIC AND                             
083500          MID-IDKOLLI-FOM         > ZERO                                  
083600           MOVE JA                TO SW-PACKA-I-FLERA-KOLLI               
083700           MOVE MID-IDKOLLI-FOM   TO MOD-IDKOLLI-FOM                      
083800           MOVE MID-IDKOLLI-TOM   TO MOD-IDKOLLI-TOM                      
083900       END-IF                                                             
084000                                                                          
084100       PERFORM GAA-KOLLA-NYTT-KOLLI                                       
084200                                                                          
084300       PERFORM GAB-KOLLA-KDCMD                                            
084400                                                                          
084500       PERFORM GAC-KOLLA-INM-RAD                                          
084600                                                                          
084700       PERFORM GAD-KOLLA-ANT-FUNKTIONER                                   
084800     END-IF                                                               
084900                                                                          
085000     .                                                                    
085100     EJECT                                                                
085200                                                                          
085300 GAA-KOLLA-NYTT-KOLLI SECTION.                                            
085400                                                                          
085500     MOVE NEJ                      TO SW-SKAPA-NYTT-KOLLI                 
085600                                      SW-SKAPA-FLERA-KOLLI                
085700                                                                          
085800     IF MID-FLNYKLI                NOT = ALL '+'                          
085900       IF MID-FLNYKLI              = JA OR YES                            
086000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNYKLI-ATTR                    
086100         MOVE JA                   TO SW-SKAPA-NYTT-KOLLI                 
086200       ELSE                                                               
086300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNYKLI-ATTR                    
086400         MOVE NEJ                  TO INDATA-SW                           
086500       END-IF                                                             
086600     END-IF                                                               
086700                                                                          
086800     IF MID-KVKOLLI               NOT = ALL '+'                           
086900       IF MID-KVKOLLI             NOT NUMERIC                             
087000         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVKOLLI-ATTR                     
087100         MOVE NEJ                 TO INDATA-SW                            
087200       ELSE                                                               
087300         IF SKAPA-NYTT-KOLLI                                              
087400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVKOLLI-ATTR                   
087500           MOVE JA                  TO SW-SKAPA-FLERA-KOLLI               
087600           MOVE MID-KVKOLLI         TO W-KVKOLLI                          
087700         ELSE                                                             
087800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNYKLI-ATTR                  
087900           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVKOLLI-ATTR                  
088000           MOVE NEJ                  TO INDATA-SW                         
088100         END-IF                                                           
088200       END-IF                                                             
088300     END-IF                                                               
088400                                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 GAB-KOLLA-KDCMD      SECTION.                                            
088800                                                                          
088900     MOVE NEJ               TO  SW-RAD-CMD                                
089000     MOVE +1                TO INDX                                       
089100                                                                          
089200     PERFORM UNTIL INDX     >  MAX-INDX                                   
089300        IF MID-KDCMD(INDX)  = ALL '+' OR SPACE                            
089400           CONTINUE                                                       
089500        ELSE                                                              
089600           MOVE JA          TO  SW-RAD-CMD                                
089700                                                                          
089800           IF VISA-KOLLI                                                  
089900              IF MID-KDCMD(INDX)      = W-BORTTAG OR W-DELETE             
090000                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)        
090100              ELSE                                                        
090200                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)        
090300                 MOVE NEJ                  TO INDATA-SW                   
090400              END-IF                                                      
090500           ELSE                                                           
090600              IF MID-KDCMD(INDX)           =  W-PACKA                     
090700                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)        
090800              ELSE                                                        
090900                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)        
091000                 MOVE NEJ                  TO INDATA-SW                   
091100              END-IF                                                      
091200           END-IF                                                         
091300        END-IF                                                            
091400        ADD +1             TO INDX                                        
091500     END-PERFORM                                                          
091600                                                                          
091700     .                                                                    
091800     EJECT                                                                
091900 GAC-KOLLA-INM-RAD    SECTION.                                            
092000                                                                          
092100     MOVE NEJ                         TO  SW-INM-RAD                      
092200     IF MID-IDDISTR-IN                NOT = ALL '+' OR                    
092300        MID-IDKUNDNR-IN               NOT = ALL '+' OR                    
092400        MID-IDRAPPNR-IN               NOT = ALL '+'                       
092500        MOVE JA                       TO  SW-INM-RAD                      
092600                                                                          
092700        IF MID-IDDISTR-IN              NUMERIC                            
092800           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDDISTR-IN-ATTR             
092900        ELSE                                                              
093000          MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-IN-ATTR             
093100          MOVE NEJ                     TO INDATA-SW                       
093200        END-IF                                                            
093300                                                                          
093400        IF MID-IDKUNDNR-IN             NUMERIC                            
093500           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDKUNDNR-IN-ATTR            
093600        ELSE                                                              
093700          MOVE MFS-NUM-FAELT-FEL       TO MOD-IDKUNDNR-IN-ATTR            
093800          MOVE NEJ                     TO INDATA-SW                       
093900        END-IF                                                            
094000                                                                          
094100        IF MID-IDRAPPNR-IN             NUMERIC                            
094200           MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDRAPPNR-IN-ATTR            
094300        ELSE                                                              
094400          MOVE MFS-NUM-FAELT-FEL       TO MOD-IDRAPPNR-IN-ATTR            
094500          MOVE NEJ                     TO INDATA-SW                       
094600        END-IF                                                            
094700                                                                          
094800     ELSE                                                                 
094900        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-IN-ATTR                   
095000                                    MOD-IDKUNDNR-IN-ATTR                  
095100                                    MOD-IDRAPPNR-IN-ATTR                  
095200     END-IF                                                               
095300                                                                          
095400     .                                                                    
095500     EJECT                                                                
095600 GAD-KOLLA-ANT-FUNKTIONER SECTION.                                        
095700                                                                          
095800     IF SKAPA-NYTT-KOLLI                                                  
095900        IF RAD-CMD OR INM-RAD                                             
096000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYKLI-ATTR                    
096100           MOVE NEJ                TO INDATA-SW                           
096200        END-IF                                                            
096300     END-IF                                                               
096400                                                                          
096500     IF RAD-CMD                                                           
096600        IF SKAPA-NYTT-KOLLI                                               
096700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYKLI-ATTR                    
096800           MOVE NEJ                TO INDATA-SW                           
096900        END-IF                                                            
097000                                                                          
097100        IF INM-RAD                                                        
097200           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR                 
097300                                      MOD-IDKUNDNR-IN-ATTR                
097400                                      MOD-IDRAPPNR-IN-ATTR                
097500           MOVE NEJ                TO INDATA-SW                           
097600        END-IF                                                            
097700     END-IF                                                               
097800                                                                          
097900     IF INM-RAD                                                           
098000        IF SKAPA-NYTT-KOLLI                                               
098100           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYKLI-ATTR                    
098200           MOVE NEJ                TO INDATA-SW                           
098300        END-IF                                                            
098400                                                                          
098500        IF RAD-CMD                                                        
098600           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR                 
098700                                      MOD-IDKUNDNR-IN-ATTR                
098800                                      MOD-IDRAPPNR-IN-ATTR                
098900           MOVE NEJ                TO INDATA-SW                           
099000        END-IF                                                            
099100     END-IF                                                               
099200                                                                          
099300     .                                                                    
099400     EJECT                                                                
099500 GB-LOGISK-KONTROLL SECTION.                                              
099600                                                                          
099700     IF RAD-CMD                                                           
099800       PERFORM GBA-KOLLA-VALDA-RADER                                      
099900     END-IF                                                               
100000                                                                          
100100     IF INM-RAD                                                           
100200       PERFORM GBB-KOLLA-INMATAT-TILLSTAND                                
100300     END-IF                                                               
100400                                                                          
100500     .                                                                    
100600     EJECT                                                                
100700 GBA-KOLLA-VALDA-RADER    SECTION.                                        
100800                                                                          
100900     IF PACKA-I-FLERA-KOLLI                                               
101000        CONTINUE                                                          
101100     ELSE                                                                 
101200        PERFORM S02-KOLLA-KOLLISTATUS                                     
101300     END-IF                                                               
101400                                                                          
101500     MOVE JA                 TO SW-FORSTA-VALDA-RAD                       
101600     MOVE +1                 TO INDX                                      
101700     PERFORM UNTIL INDX      >  MAX-INDX                                  
101800        IF MID-KDCMD(INDX)   =  W-PACKA OR W-BORTTAG OR W-DELETE          
101900           PERFORM GBAB-KOLLA-VALT-RETURTILLSTAND                         
102000        END-IF                                                            
102100        ADD +1               TO INDX                                      
102200     END-PERFORM                                                          
102300                                                                          
102400     .                                                                    
102500     EJECT                                                                
102600 GBAB-KOLLA-VALT-RETURTILLSTAND    SECTION.                               
102700                                                                          
102800     MOVE MID-IDDISTR(INDX)        TO W-IDDISTR-FSEQ-MIN                  
102900                                      W-IDDISTR-FSEQ-MAX                  
103000     MOVE MID-IDKUNDNR(INDX)       TO W-IDKUNDNR-FSEQ-MIN                 
103100                                      W-IDKUNDNR-FSEQ-MAX                 
103200     MOVE MID-IDRAPPNR(INDX)       TO W-IDRAPPNR-FSEQ-MIN                 
103300                                      W-IDRAPPNR-FSEQ-MAX                 
103400     PERFORM IMS-GHU-SEQF-WLRETA01                                        
103500     IF SEGMENT-FINNS                                                     
103600        IF MID-KDCMD(INDX)         = W-PACKA                              
103700           PERFORM GBABA-KOLLA-PACKNING                                   
103800        END-IF                                                            
103900        IF MID-KDCMD(INDX)         = W-BORTTAG OR W-DELETE                
104000           PERFORM GBABB-KOLLA-BORTTAG                                    
104100        END-IF                                                            
104200     ELSE                                                                 
104300        MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDCMD-ATTR(INDX)               
104400        MOVE NEJ                    TO INDATA-SW                          
104500     END-IF                                                               
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 GBABA-KOLLA-PACKNING   SECTION.                                          
105000                                                                          
105100     IF RET-IDKOLLI                NOT = ZERO                             
105200        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)                   
105300        MOVE NEJ                   TO INDATA-SW                           
105400     END-IF                                                               
105500                                                                          
105600     IF RET-FLFARLIG               NOT = W-SPAR-FLFARLIG AND              
105700        KOLLI-FINNS                                                       
105800        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)                   
105900        MOVE NEJ                   TO INDATA-SW                           
106000     END-IF                                                               
106100                                                                          
106200     IF KOLLI-FINNS                                                       
106300        IF RET-IDPERSON             = W-IDPERSON AND                      
106400           RET-KDARBTYP             = W-KDARBTYP                          
106500            CONTINUE                                                      
106600        ELSE                                                              
106700            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)               
106800            MOVE NEJ                   TO INDATA-SW                       
106900        END-IF                                                            
107000     ELSE                                                                 
107100        IF FORSTA-VALDA-RAD                                               
107200           MOVE RET-IDPERSON        TO W-IDPERSON                         
107300           MOVE RET-KDARBTYP        TO W-KDARBTYP                         
107400           MOVE NEJ                 TO SW-FORSTA-VALDA-RAD                
107500        ELSE                                                              
107600           IF RET-IDPERSON          = W-IDPERSON AND                      
107700              RET-KDARBTYP          = W-KDARBTYP                          
107800               CONTINUE                                                   
107900           ELSE                                                           
108000               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)            
108100               MOVE NEJ                   TO INDATA-SW                    
108200           END-IF                                                         
108300        END-IF                                                            
108400     END-IF                                                               
108500     .                                                                    
108600     EJECT                                                                
108700                                                                          
108800 GBABB-KOLLA-BORTTAG    SECTION.                                          
108900                                                                          
109000     IF RET-KDKOLSTA               NOT = W-KLI-PACKAT                     
109100        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)                   
109200        MOVE NEJ                   TO INDATA-SW                           
109300     END-IF                                                               
109400                                                                          
109500     .                                                                    
109600     EJECT                                                                
109700                                                                          
109800 GBB-KOLLA-INMATAT-TILLSTAND  SECTION.                                    
109900                                                                          
110000     PERFORM S02-KOLLA-KOLLISTATUS                                        
110100                                                                          
110200     MOVE MID-IDDISTR-IN           TO W-IDDISTR-FSEQ-MIN                  
110300                                      W-IDDISTR-FSEQ-MAX                  
110400     MOVE MID-IDKUNDNR-IN          TO W-IDKUNDNR-FSEQ-MIN                 
110500                                      W-IDKUNDNR-FSEQ-MAX                 
110600     MOVE MID-IDRAPPNR-IN          TO W-IDRAPPNR-FSEQ-MIN                 
110700                                      W-IDRAPPNR-FSEQ-MAX                 
110800     PERFORM IMS-GHU-SEQF-WLRETA01                                        
110900     IF SEGMENT-FINNS                                                     
111000        IF RET-IDKOLLI             NOT = ZERO                             
111100           IF RET-KDKOLSTA NOT = W-KLI-PACKAT                             
111200              MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR              
111300                                         MOD-IDKUNDNR-IN-ATTR             
111400                                         MOD-IDRAPPNR-IN-ATTR             
111500              MOVE NEJ                TO INDATA-SW                        
111600           END-IF                                                         
111700        END-IF                                                            
111800                                                                          
111900        IF RET-FLFARLIG            NOT = W-SPAR-FLFARLIG AND              
112000           KOLLI-FINNS                                                    
112100           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR                 
112200                                      MOD-IDKUNDNR-IN-ATTR                
112300                                      MOD-IDRAPPNR-IN-ATTR                
112400           MOVE NEJ                TO INDATA-SW                           
112500        END-IF                                                            
112600                                                                          
112700        IF KOLLI-FINNS                                                    
112800           IF RET-IDPERSON          = W-IDPERSON AND                      
112900              RET-KDARBTYP          = W-KDARBTYP                          
113000               CONTINUE                                                   
113100           ELSE                                                           
113200               MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR             
113300                                          MOD-IDKUNDNR-IN-ATTR            
113400                                          MOD-IDRAPPNR-IN-ATTR            
113500               MOVE NEJ                TO INDATA-SW                       
113600           END-IF                                                         
113700        END-IF                                                            
113800     ELSE                                                                 
113900        MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR                    
114000                                   MOD-IDKUNDNR-IN-ATTR                   
114100                                   MOD-IDRAPPNR-IN-ATTR                   
114200        MOVE NEJ                TO INDATA-SW                              
114300     END-IF                                                               
114400     .                                                                    
114500     EJECT                                                                
114600 H-UPPDATERA SECTION.                                                     
114700                                                                          
114800     IF SKAPA-NYTT-KOLLI                                                  
114900        PERFORM HA-TA-UT-KOLLINR                                          
115000     END-IF                                                               
115100                                                                          
115200     IF RAD-CMD                                                           
115300        PERFORM HB-UPPDATERA-VALDA-TILLSTAND                              
115400     END-IF                                                               
115500                                                                          
115600     IF INM-RAD                                                           
115700       PERFORM HC-UPPDATERA-INMATAT-TILLSTAND                             
115800     END-IF                                                               
115900                                                                          
116000     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
116100     CALL WMEDKONV USING MED-WMEDAREA                                     
116200     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
116300     PERFORM MFS-FORM-ATTR                                                
116400     PERFORM MFS-RENSA-FAELT-IN                                           
116500     .                                                                    
116600     EJECT                                                                
116700                                                                          
116800 HA-TA-UT-KOLLINR SECTION.                                                
116900                                                                          
117000     PERFORM IMS-GHU-WL411111                                             
117100     IF SKAPA-FLERA-KOLLI                                                 
117200        COMPUTE W-IDKOLLI-NUM  =  4112-IDKOLLI + 1                        
117300        MOVE W-IDKOLLI-NUM     TO MOD-IDKOLLI-FOM                         
117400        COMPUTE 4112-IDKOLLI   =  4112-IDKOLLI + W-KVKOLLI                
117500        MOVE 4112-IDKOLLI      TO MOD-IDKOLLI-TOM                         
117600        MOVE MFS-RENSA-FAELT   TO MOD-IDKOLLI-UT                          
117700        INSPECT MOD-IDKOLLI-FOM REPLACING LEADING ZERO BY SPACE           
117800        INSPECT MOD-IDKOLLI-TOM REPLACING LEADING ZERO BY SPACE           
117900     ELSE                                                                 
118000        COMPUTE 4112-IDKOLLI   =  4112-IDKOLLI + 1                        
118100        MOVE 4112-IDKOLLI      TO W-IDKOLLI-NUM                           
118200        MOVE W-IDKOLLI-NUM     TO MOD-IDKOLLI-UT                          
118300                                  MSGI-IDKOLLI                            
118400        INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE            
118500                                                                          
118600        MOVE ALL '+'           TO MSGI-WMSGINIT                           
118700        MOVE '001'             TO MSGI-KDCALL                             
118800        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
118900        MOVE W-IDKOLLI-NUM     TO MSGI-IDKOLLI                            
119000        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
119100     END-IF                                                               
119200     PERFORM IMS-REPL-WL411111                                            
119300                                                                          
119400                                                                          
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119800 HB-UPPDATERA-VALDA-TILLSTAND SECTION.                                    
119900                                                                          
120000     MOVE +1                 TO INDX                                      
120100     PERFORM UNTIL INDX      >  MAX-INDX                                  
120200        IF MID-KDCMD(INDX)   =  W-PACKA                                   
120300           PERFORM HBA-PACKA-VALT-RT-I-KOLLI                              
120400        END-IF                                                            
120500                                                                          
120600        IF MID-KDCMD(INDX)   =  W-BORTTAG OR W-DELETE                     
120700           PERFORM HBB-TA-BORT-VALT-RT-UR-KOLLI                           
120800        END-IF                                                            
120900        ADD +1               TO INDX                                      
121000     END-PERFORM                                                          
121100                                                                          
121200     .                                                                    
121300     EJECT                                                                
121400                                                                          
121500 HBA-PACKA-VALT-RT-I-KOLLI   SECTION.                                     
121600                                                                          
121700     MOVE MID-IDDISTR(INDX)        TO W-IDDISTR-FSEQ-MIN                  
121800                                      W-IDDISTR-FSEQ-MAX                  
121900     MOVE MID-IDKUNDNR(INDX)       TO W-IDKUNDNR-FSEQ-MIN                 
122000                                      W-IDKUNDNR-FSEQ-MAX                 
122100     MOVE MID-IDRAPPNR(INDX)       TO W-IDRAPPNR-FSEQ-MIN                 
122200                                      W-IDRAPPNR-FSEQ-MAX                 
122300     PERFORM IMS-GHU-SEQF-WLRETA01                                        
122400                                                                          
122500     IF SEGMENT-FINNS                                                     
122600        IF PACKA-I-FLERA-KOLLI                                            
122700           PERFORM HBAA-PACKA-I-FLERA-KOLLI                               
122800        ELSE                                                              
122900          MOVE MSGI-IDKOLLI        TO RET-IDKOLLI                         
123000          MOVE W-KLI-PACKAT        TO RET-KDKOLSTA                        
123100          PERFORM IMS-REPL-SEQF-WLRETA01                                  
123200        END-IF                                                            
123300     ELSE                                                                 
123400        CALL FELLOG                                                       
123500     END-IF                                                               
123600     .                                                                    
123700     EJECT                                                                
123800                                                                          
123900 HBAA-PACKA-I-FLERA-KOLLI   SECTION.                                      
124000                                                                          
124100     MOVE MID-IDKOLLI-FOM          TO W-IDKOLLI-FOM                       
124200     MOVE MID-IDKOLLI-TOM          TO W-IDKOLLI-TOM                       
124300                                                                          
124400     MOVE MID-IDKOLLI-FOM          TO RET-IDKOLLI                         
124500     MOVE W-KLI-PACKAT             TO RET-KDKOLSTA                        
124600                                                                          
124700     PERFORM IMS-REPL-SEQF-WLRETA01                                       
124800     COMPUTE W-IDKOLLI-NUM         =  W-IDKOLLI-FOM + 1                   
124900     PERFORM UNTIL W-IDKOLLI-NUM   >  W-IDKOLLI-TOM                       
125000        MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                  
125100        ACCEPT RET-TIKLOCK         FROM TIME                              
125200        MOVE W-IDKOLLI-NUM         TO RET-IDKOLLI                         
125300        PERFORM IMS-ISRT-WLRETA01                                         
125400        IF SEGMENT-FINNS-REDAN                                            
125500           PERFORM UNTIL SEGMENT-FINNS                                    
125600              ADD +1               TO RET-TIKLOCK                         
125700              PERFORM IMS-ISRT-WLRETA01                                   
125800           END-PERFORM                                                    
125900        END-IF                                                            
126000        ADD +1                     TO W-IDKOLLI-NUM                       
126100     END-PERFORM                                                          
126200                                                                          
126300     .                                                                    
126400     EJECT                                                                
126500 HBB-TA-BORT-VALT-RT-UR-KOLLI SECTION.                                    
126600                                                                          
126700     MOVE MID-IDDISTR(INDX)        TO W-IDDISTR-FSEQ-MIN                  
126800                                      W-IDDISTR-FSEQ-MAX                  
126900     MOVE MID-IDKUNDNR(INDX)       TO W-IDKUNDNR-FSEQ-MIN                 
127000                                      W-IDKUNDNR-FSEQ-MAX                 
127100     MOVE MID-IDRAPPNR(INDX)       TO W-IDRAPPNR-FSEQ-MIN                 
127200                                      W-IDRAPPNR-FSEQ-MAX                 
127300     MOVE +0    TO W-ANTAL-KOLLI                                          
127400     PERFORM IMS-GHU-SEQF-WLRETA01                                        
127500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
127600        ADD +1          TO W-ANTAL-KOLLI                                  
127700        IF RET-IDKOLLI = W-IDKOLLI                                        
127800          MOVE RET-DAREGDAT          TO W-DAREGDAT                        
127900          MOVE RET-TIKLOCK           TO W-TIKLOCK                         
128000        END-IF                                                            
128100        PERFORM IMS-GHN-SEQF-WLRETA01                                     
128200     END-PERFORM                                                          
128300                                                                          
128400     PERFORM IMS-GHU-WLRETA01                                             
128500     IF SEGMENT-FINNS                                                     
128600       IF W-ANTAL-KOLLI > +1                                              
128700          PERFORM IMS-DLET-WLRETA01                                       
128800       ELSE                                                               
128900          MOVE ZERO         TO RET-IDKOLLI                                
129000                               RET-KDKOLSTA                               
129100          PERFORM IMS-REPL-WLRETA01                                       
129200       END-IF                                                             
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600                                                                          
129700 HC-UPPDATERA-INMATAT-TILLSTAND  SECTION.                                 
129800                                                                          
129900     MOVE MID-IDDISTR-IN           TO W-IDDISTR-FSEQ-MIN                  
130000                                      W-IDDISTR-FSEQ-MAX                  
130100     MOVE MID-IDKUNDNR-IN          TO W-IDKUNDNR-FSEQ-MIN                 
130200                                      W-IDKUNDNR-FSEQ-MAX                 
130300     MOVE MID-IDRAPPNR-IN          TO W-IDRAPPNR-FSEQ-MIN                 
130400                                      W-IDRAPPNR-FSEQ-MAX                 
130500     PERFORM IMS-GHU-SEQF-WLRETA01                                        
130600                                                                          
130700     IF SEGMENT-FINNS                                                     
130800        IF RET-IDKOLLI = ZERO                                             
130900          MOVE MSGI-IDKOLLI          TO RET-IDKOLLI                       
131000          MOVE W-KLI-PACKAT          TO RET-KDKOLSTA                      
131100          PERFORM IMS-REPL-SEQF-WLRETA01                                  
131200        ELSE                                                              
131300          MOVE FUNCTION CURRENT-DATE (1:8) TO RET-DAREGDAT                
131400          ACCEPT RET-TIKLOCK  FROM TIME                                   
131500          MOVE MSGI-IDKOLLI          TO RET-IDKOLLI                       
131600          PERFORM IMS-ISRT-WLRETA01                                       
131700          IF SEGMENT-FINNS-REDAN                                          
131800             PERFORM UNTIL SEGMENT-FINNS                                  
131900                ADD +1             TO RET-TIKLOCK                         
132000                PERFORM IMS-ISRT-WLRETA01                                 
132100             END-PERFORM                                                  
132200          END-IF                                                          
132300        END-IF                                                            
132400     ELSE                                                                 
132500        CALL FELLOG                                                       
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900                                                                          
133000 S01-REDIGERA-MOD          SECTION.                                       
133100                                                                          
133200     MOVE RET-IDDISTR              TO MOD-IDDISTR     (INDX)              
133300     MOVE RET-IDKUNDNR             TO MOD-IDKUNDNR    (INDX)              
133400     MOVE RET-IDRAPPNR             TO MOD-IDRAPPNR    (INDX)              
133500     MOVE RET-KVKOLLI-AAF          TO MOD-KVKOLLI-AAF (INDX)              
133600     MOVE RET-FLFARLIG             TO MOD-FLFARLIG    (INDX)              
133700                                                                          
133800     .                                                                    
133900     EJECT                                                                
134000 S02-KOLLA-KOLLISTATUS           SECTION.                                 
134100                                                                          
134200     PERFORM S02A-KOLLA-IDKOLLI                                           
134300                                                                          
134400     PERFORM IMS-GU-WLRETC01                                              
134500                                                                          
134600     IF SEGMENT-FINNS                                                     
134700        MOVE SEQB-DAREGDAT             TO W-DAREGDAT                      
134800        MOVE SEQB-TIKLOCK              TO W-TIKLOCK                       
134900        PERFORM IMS-GU-WLRETA01                                           
135000        MOVE JA                        TO SW-KOLLI                        
135100        MOVE RET-FLFARLIG              TO W-SPAR-FLFARLIG                 
135200        IF RET-KDKOLSTA                NOT = W-KLI-PACKAT                 
135300            IF INM-RAD                                                    
135400               MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR             
135500                                          MOD-IDKUNDNR-IN-ATTR            
135600                                          MOD-IDRAPPNR-IN-ATTR            
135700            ELSE                                                          
135800               MOVE +1                 TO INDX                            
135900               PERFORM UNTIL INDX      >  MAX-INDX                        
136000                  IF MID-KDCMD(INDX)   =  W-PACKA                         
136100                      MOVE MFS-ALFA-FAELT-FEL                             
136200                                       TO MOD-KDCMD-ATTR(INDX)            
136300                  END-IF                                                  
136400                  ADD +1               TO INDX                            
136500               END-PERFORM                                                
136600            END-IF                                                        
136700            MOVE NEJ                   TO INDATA-SW                       
136800        END-IF                                                            
136900                                                                          
137000        MOVE RET-IDPERSON          TO W-IDPERSON                          
137100        MOVE RET-KDARBTYP          TO W-KDARBTYP                          
137200     END-IF                                                               
137300                                                                          
137400     .                                                                    
137500     EJECT                                                                
137600 S02A-KOLLA-IDKOLLI               SECTION.                                
137700                                                                          
137800     PERFORM IMS-GHU-WL411111                                             
137900     IF W-IDKOLLI                   >  4112-IDKOLLI                       
138000         IF INM-RAD                                                       
138100            MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-IN-ATTR                
138200                                       MOD-IDKUNDNR-IN-ATTR               
138300                                       MOD-IDRAPPNR-IN-ATTR               
138400            MOVE NEJ                TO INDATA-SW                          
138500         ELSE                                                             
138600            MOVE +1                       TO INDX                         
138700            PERFORM UNTIL INDX            >  MAX-INDX                     
138800               IF MID-KDCMD(INDX)         =  W-PACKA                      
138900                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)         
139000                  MOVE NEJ                TO INDATA-SW                    
139100               END-IF                                                     
139200               ADD +1                     TO INDX                         
139300            END-PERFORM                                                   
139400         END-IF                                                           
139500     END-IF                                                               
139600                                                                          
139700     .                                                                    
139800     EJECT                                                                
139900 MFS-RENSA-FAELT-UT SECTION.                                              
140000                                                                          
140100     MOVE MFS-RENSA-FAELT       TO MOD-FLFARLIG-KOLLI                     
140200                                   MOD-IDKOLLI                            
140300                                   MOD-IDKOLLI-FOM                        
140400                                   MOD-IDKOLLI-TOM                        
140500     MOVE +1                    TO INDX                                   
140600     PERFORM UNTIL INDX         >  MAX-INDX                               
140700        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
140800        ADD +1                  TO INDX                                   
140900     END-PERFORM                                                          
141000     .                                                                    
141100     SKIP3                                                                
141200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
141300                                                                          
141400     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR     (INDX)                 
141500                                   MOD-IDKUNDNR    (INDX)                 
141600                                   MOD-IDRAPPNR    (INDX)                 
141700                                   MOD-KVKOLLI-AAF (INDX)                 
141800                                   MOD-FLFARLIG    (INDX)                 
141900     .                                                                    
142000     SKIP3                                                                
142100 MFS-RENSA-FAELT-IN SECTION.                                              
142200                                                                          
142300*    --- ALLA INDATA-FÄLT                                                 
142400     MOVE MFS-RENSA-FAELT       TO MOD-FLNYKLI                            
142500                                   MOD-KVKOLLI                            
142600                                   MOD-IDDISTR-IN                         
142700                                   MOD-IDKUNDNR-IN                        
142800                                   MOD-IDRAPPNR-IN                        
142900     MOVE +1 TO INDX                                                      
143000     PERFORM UNTIL INDX         >  MAX-INDX                               
143100       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
143200       ADD +1                   TO INDX                                   
143300     END-PERFORM                                                          
143400     .                                                                    
143500     EJECT                                                                
143600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
143700                                                                          
143800     MOVE MFS-ROER-EJ-FAELT   TO MOD-FLFARLIG-KOLLI                       
143900                                 MOD-IDKOLLI                              
144000                                 MOD-KVKOLLI                              
144100     MOVE +1                  TO INDX                                     
144200     PERFORM UNTIL INDX       >  MAX-INDX                                 
144300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
144400       ADD +1                 TO INDX                                     
144500     END-PERFORM                                                          
144600     .                                                                    
144700                                                                          
144800                                                                          
144900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
145000                                                                          
145100     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDISTR     (INDX)                 
145200                                   MOD-IDKUNDNR    (INDX)                 
145300                                   MOD-IDRAPPNR    (INDX)                 
145400                                   MOD-KVKOLLI-AAF (INDX)                 
145500                                   MOD-FLFARLIG    (INDX)                 
145600     .                                                                    
145700                                                                          
145800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
145900                                                                          
146000*    --- ALLA INDATA-FÄLT                                                 
146100     MOVE MFS-ROER-EJ-FAELT     TO MOD-FLNYKLI                            
146200                                   MOD-KVKOLLI                            
146300                                   MOD-IDDISTR-IN                         
146400                                   MOD-IDKUNDNR-IN                        
146500                                   MOD-IDRAPPNR-IN                        
146600     MOVE +1 TO INDX                                                      
146700     PERFORM UNTIL INDX         >  MAX-INDX                               
146800       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
146900       ADD +1                   TO INDX                                   
147000     END-PERFORM                                                          
147100     .                                                                    
147200     EJECT                                                                
147300 MFS-FORM-ATTR SECTION.                                                   
147400                                                                          
147500*    --- ALLA INDATA-FÄLT                                                 
147600     MOVE MFS-FORMATETS-ATTR    TO MOD-FLNYKLI-ATTR                       
147700                                   MOD-KVKOLLI-ATTR                       
147800                                   MOD-IDDISTR-IN-ATTR                    
147900                                   MOD-IDKUNDNR-IN-ATTR                   
148000                                   MOD-IDRAPPNR-IN-ATTR                   
148100     MOVE +1 TO INDX                                                      
148200     PERFORM UNTIL INDX         >  MAX-INDX                               
148300       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                   
148400       ADD +1                   TO INDX                                   
148500     END-PERFORM                                                          
148600     .                                                                    
148700     EJECT                                                                
148800* --- IMS SEKTIONER ---                                                   
148900     SKIP3                                                                
149000 IMS-GET-MSG SECTION.                                                     
149100                                                                          
149200     MOVE '  QC' TO GODK-STATUSKODER                                      
149300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
149400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
149500     PERFORM IMS-STATUSKONTROLL                                           
149600     .                                                                    
149700     SKIP3                                                                
149800 IMS-INSERT-MSG SECTION.                                                  
149900                                                                          
150000     IF ENGLISH-TEXT                                                      
150100       MOVE 'N' TO MFS-KDHUVOMR                                           
150200     END-IF                                                               
150300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
150400     MOVE SPACE TO GODK-STATUSKODER                                       
150500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
150600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     EJECT                                                                
151000 IMS-GU-SEQA-WLRETA01       SECTION.                                      
151100                                                                          
151200     STRING 'WLRETA01(WDA3ASEQ>=' W-WDA3ASEQ-MIN-X                        
151300                    '&WDA3ASEQ<=' W-WDA3ASEQ-MAX-X ')'                    
151400          DELIMITED BY SIZE INTO SSA1                                     
151500     MOVE '  GE'           TO GODK-STATUSKODER                            
151600     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-AREA SSA1                     
151700     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000                                                                          
152100 IMS-GN-SEQA-WLRETA01 SECTION.                                            
152200                                                                          
152300     STRING 'WLRETA01(WDA3ASEQ>=' W-WDA3ASEQ-MIN-X                        
152400                    '&WDA3ASEQ<=' W-WDA3ASEQ-MAX-X ')'                    
152500          DELIMITED BY SIZE INTO SSA1                                     
152600     MOVE '  GE' TO GODK-STATUSKODER                                      
152700     CALL CBLTDLI USING GN RETA1-PCB DLI-IO-AREA SSA1                     
152800     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     EJECT                                                                
153200 IMS-GU-SEQB-WLRETA01       SECTION.                                      
153300                                                                          
153400     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
153500                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X                        
153600                    '&IDKOLLI  =' W-IDKOLLI-X      ')'                    
153700          DELIMITED BY SIZE INTO SSA1                                     
153800     MOVE '  GE'           TO GODK-STATUSKODER                            
153900     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-AREA SSA1                     
154000     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300                                                                          
154400 IMS-GN-SEQB-WLRETA01 SECTION.                                            
154500                                                                          
154600     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
154700                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X                        
154800                    '&IDKOLLI  =' W-IDKOLLI-X      ')'                    
154900          DELIMITED BY SIZE INTO SSA1                                     
155000     MOVE '  GE' TO GODK-STATUSKODER                                      
155100     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-AREA SSA1                     
155200     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     EJECT                                                                
155600                                                                          
155700 IMS-GHU-SEQF-WLRETA01       SECTION.                                     
155800                                                                          
155900     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
156000                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
156100          DELIMITED BY SIZE INTO SSA1                                     
156200     MOVE '  GE'           TO GODK-STATUSKODER                            
156300     CALL CBLTDLI USING GHU RETA3-PCB DLI-IO-AREA SSA1                    
156400     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
156500     PERFORM IMS-STATUSKONTROLL                                           
156600     .                                                                    
156700                                                                          
156800                                                                          
156900 IMS-GHN-SEQF-WLRETA01       SECTION.                                     
157000                                                                          
157100     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
157200                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
157300          DELIMITED BY SIZE INTO SSA1                                     
157400     MOVE '  GEGB'         TO GODK-STATUSKODER                            
157500     CALL CBLTDLI USING GHN RETA3-PCB DLI-IO-AREA SSA1                    
157600     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
157700     PERFORM IMS-STATUSKONTROLL                                           
157800     .                                                                    
157900                                                                          
158000                                                                          
158100 IMS-REPL-SEQF-WLRETA01      SECTION.                                     
158200                                                                          
158300     MOVE '    '           TO GODK-STATUSKODER                            
158400     CALL CBLTDLI USING REPL RETA3-PCB DLI-IO-AREA                        
158500     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
158600     PERFORM IMS-STATUSKONTROLL                                           
158700     .                                                                    
158800     EJECT                                                                
158900                                                                          
159000 IMS-GU-WLRETA01       SECTION.                                           
159100                                                                          
159200     STRING 'WLRETA01(WDA301KY>=' W-WDA301KY-X ')'                        
159300          DELIMITED BY SIZE INTO SSA1                                     
159400     MOVE '  '           TO GODK-STATUSKODER                              
159500     CALL CBLTDLI USING GU RETA4-PCB DLI-IO-AREA SSA1                     
159600     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
159700     PERFORM IMS-STATUSKONTROLL                                           
159800     .                                                                    
159900                                                                          
160000 IMS-GHU-WLRETA01      SECTION.                                           
160100                                                                          
160200     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
160300          DELIMITED BY SIZE INTO SSA1                                     
160400     MOVE '  GE'         TO GODK-STATUSKODER                              
160500     CALL CBLTDLI USING GHU RETA4-PCB DLI-IO-AREA SSA1                    
160600     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
160700     PERFORM IMS-STATUSKONTROLL                                           
160800     .                                                                    
160900                                                                          
161000 IMS-ISRT-WLRETA01    SECTION.                                            
161100                                                                          
161200     MOVE 'WLRETA01 ' TO SSA1                                             
161300     MOVE '  II' TO GODK-STATUSKODER                                      
161400     CALL CBLTDLI USING ISRT RETA4-PCB DLI-IO-AREA SSA1                   
161500     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
161600     PERFORM IMS-STATUSKONTROLL                                           
161700     .                                                                    
161800     SKIP2                                                                
161900 IMS-REPL-WLRETA01    SECTION.                                            
162000                                                                          
162100     MOVE '  ' TO GODK-STATUSKODER                                        
162200     CALL CBLTDLI USING REPL RETA4-PCB DLI-IO-AREA                        
162300     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600     SKIP2                                                                
162700 IMS-DLET-WLRETA01    SECTION.                                            
162800                                                                          
162900     MOVE '  ' TO GODK-STATUSKODER                                        
163000     CALL CBLTDLI USING DLET RETA4-PCB DLI-IO-AREA                        
163100     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
163200     PERFORM IMS-STATUSKONTROLL                                           
163300     .                                                                    
163400     EJECT                                                                
163500 IMS-GHU-WL411111  SECTION.                                               
163600                                                                          
163700     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
163800                      DELIMITED BY SIZE INTO SSA1                         
163900     MOVE  'WL411111 ' TO SSA2                                            
164000     MOVE '  ' TO GODK-STATUSKODER                                        
164100     CALL CBLTDLI USING GHU 4111-PCB IO-AREA SSA1 SSA2                    
164200     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     .                                                                    
164500     SKIP2                                                                
164600 IMS-REPL-WL411111  SECTION.                                              
164700                                                                          
164800     MOVE '  ' TO GODK-STATUSKODER                                        
164900     CALL CBLTDLI USING REPL 4111-PCB IO-AREA                             
165000     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
165100     PERFORM IMS-STATUSKONTROLL                                           
165200     .                                                                    
165300     SKIP2                                                                
165400 IMS-GU-WLRETC01       SECTION.                                           
165500                                                                          
165600     STRING 'WLRETC01(WDA3B1KY>=' W-WDA3B1KY-MIN-X                        
165700                    '&WDA3B1KY<=' W-WDA3B1KY-MAX-X                        
165800                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
165900          DELIMITED BY SIZE INTO SSA1                                     
166000     MOVE '  GE'           TO GODK-STATUSKODER                            
166100     CALL CBLTDLI USING GU RETC-PCB DLI-IO-AREA SSA1                      
166200     MOVE RETC-STATUS-CODE TO STATUS-WS                                   
166300     PERFORM IMS-STATUSKONTROLL                                           
166400     .                                                                    
166500                                                                          
166600 IMS-STATUSKONTROLL SECTION.                                              
166700                                                                          
166800     SET STATUS-IX TO 1                                                   
166900     SEARCH GODK-STATUS                                                   
167000       AT END                                                             
167100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
167200         DELIMITED BY SIZE INTO FELTEXT                                   
167300         CALL FELLOG                                                      
167400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
167500         CONTINUE                                                         
167600     END-SEARCH                                                           
167700     .                                                                    
