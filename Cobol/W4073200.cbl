000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0147      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4073200.                                                
000800 AUTHOR.         EVA LUNDELL.                                             
000900 DATE-WRITTEN.   95/06/08.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*    FUNKTION:                                                            
001300*        VISA SÄNDNINGSINNEHÅLL. BILDEN ANVÄNDS FÖR ATT SE VAD EN         
001400*        SÄNDNING INNEHÅLLER ELLER FÖR ATT REGISTRERA AVVIKELSER          
001500*        OM ANTAL KOLLIN EJ STÄMMER VID LOSSNING /MOTTAGNING AV           
001600*        SÄNDNING                                                         
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T732                                              
002200*        MID:         W4I73201                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O73201                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4073200'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  UPPDATERA-KOLLI-SW          PIC X       VALUE 'J'.                   
005300     88  KOLLI-OK                            VALUE 'J'.                   
005400     88  KOLLI-FEL                           VALUE 'N'.                   
005500                                                                          
005600 77  IDKOLLI-IFYLLT-SW           PIC X       VALUE 'N'.                   
005700     88  IDKOLLI-IFYLLT                      VALUE 'J'.                   
005800     88  IDKOLLI-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  INPUT-FINNS-SW              PIC X       VALUE 'J'.                   
006100     88  INPUT-FINNS                         VALUE 'J'.                   
006200     88  INPUT-FINNS-INTE                    VALUE 'N'.                   
006300                                                                          
006400                                                                          
006500 77  RENSA-FAELT-IN-SW           PIC X       VALUE 'N'.                   
006600     88  RENSA-FAELT-IN                      VALUE 'J'.                   
006700     88  RENSA-FAELT-FEL                     VALUE 'N'.                   
006800                                                                          
006900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007000     88  EGEN-MID                            VALUE '4732'.                
007100     88  GODK-MID                            VALUE '4731' '4732'          
007200                                                   '4733' '4734'          
007300                                                   '4735' '4736'          
007400                                                   '4737' '4738'          
007500                                                   '4739'.                
007600     88  HELP-MID                            VALUE '0551'.                
007700                                                                          
007800 77  RAD-IX                      PIC S9(4)   VALUE ZERO COMP SYNC.        
007900 77  4792-INDX                   PIC S9(4)   VALUE ZERO COMP SYNC.        
008000 77  4792-MAX-INDX               PIC S9(4)   VALUE +24  COMP SYNC.        
008100 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
008200 77  MAX-INDX                    PIC S9(4)   VALUE ZERO COMP SYNC.        
008300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008500 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17 COMP SYNC.         
008600 01  W-KLI-SAK                   PIC 9       VALUE 6.                     
008700 01  W-KLI-AVV                   PIC 9       VALUE 7.                     
008800 77  W-KDKOLSTA                  PIC X       VALUE '1'.                   
008900     88  W-PACK-KEY                          VALUE '1'.                   
009000     88  W-LAST-KEY                          VALUE '2'.                   
009100     88  W-SANT-KEY                          VALUE '3'.                   
009200     88  W-LOSS-KEY                          VALUE '4'.                   
009300     88  W-MOTT-KEY                          VALUE '5'.                   
009400     88  W-SAKN-KEY                          VALUE '6'.                   
009500     88  W-AVVI-KEY                          VALUE '7'.                   
009600     88  W-BEHA-KEY                          VALUE '8' '9'.               
009700 01  W-AATGAERDER.                                                        
009800     05  W-MOTTAGET              PIC X(1)    VALUE 'M'.                   
009900     05  W-AVVIKELSE             PIC X(4)    VALUE 'AVV'.                 
010000     05  W-BORTTAG               PIC X(4)    VALUE 'B  '.                 
010100     05  W-RECEIVED              PIC X(1)    VALUE 'R'.                   
010200     05  W-DEVIATION             PIC X(4)    VALUE 'DEV'.                 
010300     05  W-DELETE                PIC X(4)    VALUE 'D  '.                 
010400 01  ARBETSAREOR.                                                         
010500     03  W-IDSNDNNR.                                                      
010600         05  W-IDRT-UT           PIC X(3)   VALUE SPACE.                  
010700         05  W-IDRTLOP-UT        PIC X(3)   VALUE SPACE.                  
010800     EJECT                                                                
010900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011000 01  GENERELLA-SUBPROGRAM.                                                
011100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
011300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011800*01  -COPY WMEDAREA                                                       
011900     SKIP3                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012700     03  INF-MORE-INFO-FINNS     PIC X(3)    VALUE '105'.                 
012800     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
012900     EJECT                                                                
013000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013100*                                                                         
013200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013300     EJECT                                                                
013400                                                                          
013500 77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                   
013600     88  STARTA-ANNAN-BILD                   VALUE 'J'.                   
013700                                                                          
013800 01  BILD-HOPP-AREROR.                                                    
013900     SKIP2                                                                
014000     03  W-BILD                  PIC X(4).                                
014100                                                                          
014200     03  W-HOPP-IDTRANS.                                                  
014300         05  FILLER              PIC X(1)    VALUE 'W'.                   
014400         05  W-HOPP-IDTRANS-2    PIC X(1).                                
014500         05  FILLER              PIC X(1)    VALUE 'T'.                   
014600         05  W-HOPP-IDTRANS-4-6  PIC X(3).                                
014700         05  FILLER              PIC X(2)    VALUE SPACE.                 
014800                                                                          
014900                                                                          
015000     03  FILLER                  PIC X(16)   VALUE 'P-TO-P-AREA'.         
015100     03  P-TO-P-SW.                                                       
015200         05  P-TO-P-KVLL         PIC S9(4)   VALUE +117 COMP SYNC.        
015300         05  P-TO-P-KDZ1         PIC X(1)    VALUE LOW-VALUE.             
015400                                                                          
015500         05  P-TO-P-KDZ2         PIC X(1)    VALUE LOW-VALUE.             
015600                                                                          
015700         05  P-TO-P-KDTRANS      PIC X(8).                                
015800         05  P-TO-P-IDTRANS      PIC X(4).                                
015900         05  P-TO-P-KDMFSFOR     PIC X(1).                                
016000         05  P-TO-P-DATA         PIC X(100)  VALUE ALL '+'.               
016100                                                                          
016200     EJECT                                                                
016300*01 -COPY WMSGINIT                                                        
016400     SKIP3                                                                
016500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016800     SKIP3                                                                
016900*01  MID -COPY W4I73201                                                   
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017200     SKIP3                                                                
017300*01  -COPY WMSGAREA                                                       
017400     EJECT                                                                
017500     03  MOD REDEFINES MSG-AREA.                                          
017600*      05  -COPY W4O73201  -PRE MOD-                                      
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017900     SKIP3                                                                
018000*01  -COPY WMFSAREA                                                       
018100     EJECT                                                                
018200 77  KVKOLLI                     PIC 9(4)  VALUE ZERO.                    
018300     EJECT                                                                
018400 01  FILLER                      PIC X(16) VALUE 'KOM-IO-AREA'.           
018500 01  KOM-MSG-IO-AREA.                                                     
018600*03  -COPY WMSGKOM                                                        
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16) VALUE 'MSG/KOM-AREA'.          
018900 01  -COPY WMSGSNUF     -PRE P-TO-P-                                      
019000     EJECT                                                                
019100 01  FILLER                      PIC X(24) VALUE                          
019200                                 'MOD4792-MID-W4I79201'.                  
019300*    -COPY W4I79201  -PRE MOD4792-                                        
019400     EJECT                                                                
019500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019600*                                                                         
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019900     SKIP3                                                                
020000 01  W-MINKEY-X.                                                          
020100     03  W-MINKEY-IDTRANS          PIC X(4)    VALUE '4732'.              
020200     03  W-MINKEYB1-ENTER.                                                
020400         05  W-MINKEYB1-IDRT-ENTER    PIC X(3)        VALUE SPACE.        
020500         05  W-MINKEYB1-IDDC-ENTER    PIC X(2)        VALUE SPACE.        
020600         05  W-MINKEYB1-IDRTLOP-ENTER PIC 9(3).                           
020700         05  W-MINKEYB1-IDKOLLI-ENTER PIC S9(5) COMP-3 VALUE ZERO.        
020800     03  W-MINKEYB1-NEXT.                                                 
021000         05  W-MINKEYB1-IDRT-NEXT     PIC X(3)        VALUE SPACE.        
021010         05  W-MINKEYB1-IDDC-NEXT     PIC X(2)        VALUE SPACE.        
021100         05  W-MINKEYB1-IDRTLOP-NEXT  PIC 9(3).                           
021200         05  W-MINKEYB1-IDKOLLI-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
021300                                                                          
021400 01  NYCKLAR-TILL-DLI.                                                    
021500     03  W-IDUSER-X.                                                      
021600         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
021700                                                                          
021800     03  W-IDRETSND-X.                                                    
021900         05  W-IDRETSND          PIC X(8)    VALUE SPACE.                 
022000                                                                          
022100     03  W-WDA301KY-X.                                                    
022200         05  W-IDDC-301          PIC  X(2)   VALUE SPACE.                 
022300         05  W-DAREGDAT          PIC  9(8)   VALUE ZERO.                  
022400         05  W-TIKLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
022500                                                                          
022600     03  W-WDA3BSEQ-MIN-X.                                                
022700         05  W-IDRT-BSEQ-MIN     PIC X(3)            VALUE SPACE.         
022800         05  W-IDDC-BSEQ-MIN     PIC X(2)            VALUE SPACE.         
022900         05  W-IDRTLOP-BSEQ-MIN  PIC 9(3).                                
023000         05  W-IDKOLLI-BSEQ-MIN  PIC S9(5)   COMP-3  VALUE ZERO.          
023100                                                                          
023200     03  W-WDA3BSEQ-MAX-X.                                                
023300         05  W-IDRT-BSEQ-MAX     PIC X(3)            VALUE SPACE.         
023400         05  W-IDDC-BSEQ-MAX     PIC X(2)            VALUE SPACE.         
023500         05  W-IDRTLOP-BSEQ-MAX  PIC 9(3).                                
023600         05  W-IDKOLLI-BSEQ-MAX  PIC S9(5)   COMP-3  VALUE ZERO.          
023700     SKIP2                                                                
023800                                                                          
023900*    --- STATUS-KOD FRÅN IMS                                              
024000 01  STATUS-WS                   PIC XX.                                  
024100     88  STATUS-OK                           VALUE '  '.                  
024200     88  SEGMENT-FINNS                       VALUE '  '.                  
024300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024500     88  TRANSKOD-FEL                        VALUE 'A1'.                  
024600     88  SECURITY-FEL                        VALUE 'A4'.                  
024700     SKIP2                                                                
024800 01  GODK-STATUSKODER.                                                    
024900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025000     SKIP3                                                                
025100 01  SSA1                        PIC X(96).                               
025200 01  SSA2                        PIC X(96).                               
025300     EJECT                                                                
025400*    --- IMS FUNKTIONSKODER                                               
025500*01  -COPY W0003                                                          
025600     EJECT                                                                
025700*    ---  DLI INPUT-OUTPUT AREA                                           
025800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-RETA01'.           
025900                                                                          
026000 01  DLI-IO-RETA01.                                                       
026100*  03  -COPY WDA301                                                       
026200     EJECT                                                                
026300 LINKAGE SECTION.                                                         
026400                                                                          
026500*01  -COPY W0009   -PRE MSG-                                              
026600     EJECT                                                                
026700*01  -COPY W0009   -PRE ALT-                                              
026800     EJECT                                                                
026900*01  -COPY W0009   -PRE DISP-                                             
027000     EJECT                                                                
027100*01  -COPY W0008   -PRE USEA-                                             
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008   -PRE RETA-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008   -PRE SEQB-                                             
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008   -PRE KOM-KOMA-                                         
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DISP-PCB USEA-PCB              
028400                           RETA-PCB SEQB-PCB KOM-KOMA-PCB.                
028500 MAIN SECTION.                                                            
028600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DISP-PCB USEA-PCB              
028700                           RETA-PCB SEQB-PCB KOM-KOMA-PCB.                
028800                                                                          
028900     PERFORM IMS-GET-MSG                                                  
029000     IF SEGMENT-FINNS                                                     
029100       PERFORM A-INIT                                                     
029200       PERFORM B-KOLLA-NYCKLAR                                            
029300       IF NYCKLAR-OK                                                      
029400         IF MFS-UPDATE                                                    
029500           PERFORM G-KOLLA-INPUT                                          
029600           IF INDATA-OK                                                   
029700             PERFORM H-UPPDATERA                                          
029800           END-IF                                                         
029900         ELSE                                                             
030000           IF MFS-FIRST                                                   
030100             PERFORM C-FOERSTA-SIDA                                       
030200           ELSE                                                           
030300             IF MFS-NEXT                                                  
030400               PERFORM D-NAESTA-SIDA                                      
030500             ELSE                                                         
030600               PERFORM E-SAMMA-SIDA                                       
030700             END-IF                                                       
030800           END-IF                                                         
030900         END-IF                                                           
031000         IF STARTA-ANNAN-BILD                                             
031100             CONTINUE                                                     
031200         ELSE                                                             
031300             IF INDATA-OK                                                 
031400                 PERFORM F-LAES-VISA-INFO                                 
031500             END-IF                                                       
031600         END-IF                                                           
031700       END-IF                                                             
031800       IF STARTA-ANNAN-BILD                                               
031900           CONTINUE                                                       
032000       ELSE                                                               
032100           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73201 + 4                  
032200           PERFORM IMS-INSERT-MSG                                         
032300       END-IF                                                             
032400     END-IF                                                               
032500                                                                          
032600     MOVE ZERO TO RETURN-CODE                                             
032700     GOBACK                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 A-INIT SECTION.                                                          
033100                                                                          
033200     IF MSG-DUBBLA-TRANSKODER                                             
033300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73201                 
033400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
033500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033600     ELSE                                                                 
033700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I73201                  
033800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
033900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034000     END-IF                                                               
034100                                                                          
034200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
034300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
034400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034500                                                                          
034600     MOVE LOW-VALUE TO MSG-AREA                                           
034700     MOVE 'W4O73201' TO MFS-IDMOD                                         
034800     MOVE '4732' TO MOD-IDTRANS                                           
034900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035000                                                                          
035100     IF EGEN-MID OR HELP-MID                                              
035200       CONTINUE                                                           
035300     ELSE                                                                 
035400       MOVE SPACE TO MFS-KDTRTYP                                          
035500       MOVE '7' TO MFS-IDPFK                                              
035600     END-IF                                                               
035700                                                                          
035800     MOVE LOW-VALUE         TO W-WDA3BSEQ-MIN-X                           
035900     MOVE HIGH-VALUE        TO W-WDA3BSEQ-MAX-X                           
036000     .                                                                    
036100     EJECT                                                                
036200 B-KOLLA-NYCKLAR SECTION.                                                 
036300                                                                          
036400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036500     MOVE '001'             TO MSGI-KDCALL                                
036600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036700     MOVE '4732'            TO MSGI-IDTRANS                               
036800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036900*                                                                         
037000     IF EGEN-MID                                                          
037100         MOVE MID-IDRT-IN         TO MSGI-IDRT                            
037200         MOVE MID-IDRTLOP-IN      TO MSGI-IDRTLOP                         
037300         IF MID-IDRT-IN = ALL '+'                                         
037400           MOVE MID-IDKOLLI-IN      TO MSGI-IDKOLLI                       
037500         ELSE                                                             
037600           MOVE ZERO                TO MSGI-IDKOLLI                       
037700         END-IF                                                           
037800     END-IF                                                               
037900                                                                          
038000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038100                                                                          
038200     IF MSGI-IDLAND-SPR = 'GB'                                            
038300       MOVE 'GB'                 TO MED-IDSKYLT                           
038400     ELSE                                                                 
038500       MOVE 'S '                 TO MED-IDSKYLT                           
038600     END-IF                                                               
038700                                                                          
038800     MOVE JA TO NYCKLAR-SW                                                
038900                                                                          
039000     PERFORM BA-KOLLA-IDRT-IDRTLOP                                        
039100     PERFORM BB-KOLLA-IDKOLLI                                             
039200     MOVE MSGI-IDDC     TO W-IDDC-301                                     
039300                           W-IDDC-BSEQ-MIN                                
039400                           W-IDDC-BSEQ-MAX                                
039500                                                                          
039600     IF GODK-MID OR NYCKLAR-OK                                            
039700       MOVE MSGI-IDRT            TO W-IDRT-UT                             
039800       MOVE MSGI-IDRTLOP         TO W-IDRTLOP-UT                          
039900                                                                          
040000       MOVE W-IDSNDNNR           TO MOD-IDSNDNNR-UT                       
040100       MOVE MSGI-IDKOLLI         TO MOD-IDKOLLI-UT                        
040200       INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE             
040300     ELSE                                                                 
040400       MOVE MFS-RENSA-FAELT TO MOD-IDSNDNNR-UT                            
040500                               MOD-IDKOLLI-UT                             
040600     END-IF                                                               
040700                                                                          
040800     IF NYCKLAR-FEL                                                       
040900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
041000       CALL WMEDKONV USING MED-WMEDAREA                                   
041100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041200       PERFORM MFS-RENSA-FAELT-IN                                         
041300       PERFORM MFS-RENSA-FAELT-UT                                         
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 BA-KOLLA-IDRT-IDRTLOP SECTION.                                           
041800     SKIP2                                                                
041900*    -- KONTROLL AV IDRT-IN                                               
042000     MOVE MFS-RENSA-FAELT TO MOD-IDSNDNNR-IN                              
042100                                                                          
042200     IF MID-IDRT-IN NOT = ALL '+'                                         
042300       MOVE '7'         TO MFS-IDPFK                                      
042400       MOVE SPACE       TO MFS-KDTRTYP                                    
042500     END-IF                                                               
042600                                                                          
042700     MOVE MSGI-IDRT     TO W-IDRT-BSEQ-MIN                                
042800                           W-IDRT-BSEQ-MAX                                
042900                                                                          
043000                                                                          
043100*    -- KONTROLL AV IDRTLOP-IN                                            
043200     MOVE MFS-RENSA-FAELT TO MOD-IDSNDNNR-IN                              
043300                                                                          
043400                                                                          
043500                                                                          
043600     IF MSGI-IDRTLOP NUMERIC AND                                          
043700        MSGI-IDRTLOP > 0                                                  
043800                                                                          
043900         MOVE MSGI-IDRTLOP  TO W-IDRTLOP-BSEQ-MIN                         
044000                               W-IDRTLOP-BSEQ-MAX                         
044100     ELSE                                                                 
044200         MOVE MFS-RENSA-FAELT TO MOD-IDSNDNNR-IN                          
044300         MOVE NEJ TO NYCKLAR-SW                                           
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 BB-KOLLA-IDKOLLI SECTION.                                                
044800     SKIP2                                                                
044900                                                                          
045000***                                                                       
045100**    KONTROLL AV IDKOLLI-IN                                              
045200***                                                                       
045300     IF MID-IDKOLLI-IN NOT = ALL '+'                                      
045400       MOVE '7'         TO MFS-IDPFK                                      
045500       MOVE SPACE       TO MFS-KDTRTYP                                    
045600     END-IF                                                               
045700                                                                          
045800     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
045900                                                                          
046000     MOVE JA TO IDKOLLI-IFYLLT-SW                                         
046100     IF MSGI-IDKOLLI NUMERIC AND                                          
046200        MSGI-IDKOLLI > 0                                                  
046300         MOVE MSGI-IDKOLLI  TO W-IDKOLLI-BSEQ-MIN                         
046400     ELSE                                                                 
046500         MOVE NEJ TO IDKOLLI-IFYLLT-SW                                    
046600     END-IF                                                               
046700     .                                                                    
046800     EJECT                                                                
046900 C-FOERSTA-SIDA SECTION.                                                  
047000     SKIP2                                                                
047100     MOVE INF-FIRST-PAGE             TO MED-IDMFSINF                      
047200     CALL WMEDKONV USING MED-WMEDAREA                                     
047300     MOVE MED-MFSINF                 TO MOD-TEMFSFEL                      
047400**   BLANKA/NOLLA BLÄDDRINGSNYCKEL                                        
047500                                                                          
047600     PERFORM MFS-RENSA-FAELT-IN                                           
047700     .                                                                    
047800     EJECT                                                                
047900 D-NAESTA-SIDA SECTION.                                                   
048000     SKIP2                                                                
048100     MOVE MSGI-SPAR-AREA            TO W-MINKEY-X                         
048200     IF W-MINKEY-IDTRANS = '4732'                                         
048300       MOVE W-MINKEYB1-NEXT         TO W-WDA3BSEQ-MIN-X                   
048400     ELSE                                                                 
048500       MOVE LOW-VALUE               TO W-WDA3BSEQ-MIN-X                   
048600       MOVE MSGI-IDDC               TO W-IDDC-BSEQ-MIN                    
048700       PERFORM MFS-RENSA-FAELT-IN                                         
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 E-SAMMA-SIDA SECTION.                                                    
049200                                                                          
049300     MOVE MSGI-SPAR-AREA            TO W-MINKEY-X                         
049400     IF W-MINKEY-IDTRANS = '4732'                                         
049500       MOVE W-MINKEYB1-ENTER        TO W-WDA3BSEQ-MIN-X                   
049600     ELSE                                                                 
049700       MOVE +0                      TO W-IDRT-BSEQ-MIN                    
049800                                       W-IDRTLOP-BSEQ-MIN                 
049900                                       W-IDKOLLI-BSEQ-MIN                 
050000     END-IF                                                               
050100                                                                          
050200     MOVE +1               TO RAD-IX                                      
050300     PERFORM UNTIL RAD-IX  >  13                                          
050400        IF MID-KDCMD (RAD-IX) NUMERIC                                     
050500           PERFORM EA-STARTA-ANNAN-BILD                                   
050600           MOVE JA TO SW-STARTA-ANNAN-BILD                                
050700           MOVE 13         TO RAD-IX                                      
050800        END-IF                                                            
050900        ADD +1             TO RAD-IX                                      
051000     END-PERFORM                                                          
051100     IF STARTA-ANNAN-BILD                                                 
051200        CONTINUE                                                          
051300     ELSE                                                                 
051400        PERFORM MFS-LAES-IN-IGEN                                          
051500        PERFORM MFS-ROER-EJ-FAELT-IN                                      
051600        MOVE INF-PRESS-PF11    TO MED-IDMFSINF                            
051700        CALL WMEDKONV USING MED-WMEDAREA                                  
051800        MOVE MED-MFSINF        TO MOD-TEMFSFEL                            
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 EA-STARTA-ANNAN-BILD  SECTION.                                           
052300                                                                          
052400                                                                          
052500     MOVE MID-IDRT-IN            TO MSGI-IDRT                             
052600     MOVE MID-IDRTLOP-IN         TO MSGI-IDRTLOP                          
052700     MOVE MID-IDKOLLI (RAD-IX)   TO MSGI-IDKOLLI                          
052800     MOVE '001'                  TO MSGI-KDCALL                           
052900     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
053000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
053100                                                                          
053200     MOVE MID-IDRTLOP-IN         TO MSGI-IDRTLOP                          
053300     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
053400     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
053500     MOVE MID-KDCMD (RAD-IX) (1:1)  TO W-HOPP-IDTRANS-2                   
053600     MOVE MID-KDCMD (RAD-IX) (2:3)  TO W-HOPP-IDTRANS-4-6                 
053700     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
053800     MOVE '4732'                 TO P-TO-P-IDTRANS                        
053900     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
054000                                                                          
054100     PERFORM S01-INSERT-ALTMSG                                            
054200     .                                                                    
054300     EJECT                                                                
054400 F-LAES-VISA-INFO SECTION.                                                
054500                                                                          
054600                                                                          
054700     IF IDKOLLI-IFYLLT                                                    
054800         CONTINUE                                                         
054900     ELSE                                                                 
055000         MOVE HIGH-VALUE TO W-WDA3BSEQ-MAX-X                              
055100         MOVE W-IDRT-BSEQ-MIN TO W-IDRT-BSEQ-MAX                          
055200         MOVE W-IDRTLOP-BSEQ-MIN TO W-IDRTLOP-BSEQ-MAX                    
055300     END-IF                                                               
055400     PERFORM IMS-GU-SEQB-WLRETA01                                         
055500                                                                          
055600     IF SEGMENT-SAKNAS                                                    
055700        MOVE ERR-KOLLI-SAKNAS TO  MED-IDMFSFEL                            
055800        CALL WMEDKONV USING MED-WMEDAREA                                  
055900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
056000        PERFORM MFS-RENSA-FAELT-UT                                        
056100     ELSE                                                                 
056200         PERFORM FA-REDIGERA-UTDATA                                       
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 FA-REDIGERA-UTDATA SECTION.                                              
056700     SKIP2                                                                
056800                                                                          
056900     MOVE RET-IDFRASED-CDC TO MOD-IDFRASED                                
057000     PERFORM FAA-FIXA-ENTER-KEY                                           
057100     MOVE +1 TO RAD-IX                                                    
057200                                                                          
057300     PERFORM UNTIL RAD-IX > 13                                            
057400                                                                          
057500       IF SEGMENT-FINNS                                                   
057600         MOVE RET-ADINLOMR          TO MOD-ADINLOMR (RAD-IX)              
057700         MOVE RET-IDKOLLI           TO MOD-IDKOLLI (RAD-IX)               
057800         MOVE RET-FLFARLIG          TO MOD-FLFARLIG (RAD-IX)              
057900         PERFORM FAC-RED-KDKOLSTA                                         
057910                                                                          
057920         IF   RET-FLBUYBAC = JA                                           
057930           MOVE RET-FLBUYBAC        TO MOD-FLBUYBAC (RAD-IX)              
057940         ELSE                                                             
057950           MOVE SPACE               TO MOD-FLBUYBAC (RAD-IX)              
057960         END-IF                                                           
057970                                                                          
058000         MOVE RET-IDKOLLI           TO W-IDKOLLI-BSEQ-MIN                 
058100         PERFORM IMS-GN-SEQB-WLRETA01                                     
058200                                                                          
058300       ELSE                                                               
058400         MOVE MFS-RENSA-FAELT       TO MOD-ADINLOMR (RAD-IX)              
058500                                       MOD-IDKOLLI (RAD-IX)               
058600                                       MOD-FLFARLIG (RAD-IX)              
058700                                       MOD-BESTATUS (RAD-IX)              
058710                                       MOD-FLBUYBAC (RAD-IX)              
058800       END-IF                                                             
058900                                                                          
059000       ADD +1 TO RAD-IX                                                   
059100     END-PERFORM                                                          
059200                                                                          
059300     PERFORM FAB-FIXA-NEXT-KEY                                            
059400     MOVE '002'            TO MSGI-KDCALL                                 
059500     MOVE '4732'           TO MSGI-IDTRANS                                
059600     CALL W005INIT  USING MSGI-WMSGINIT USEA-PCB                          
059700     .                                                                    
059800     EJECT                                                                
059900 FAA-FIXA-ENTER-KEY SECTION.                                              
060000     SKIP2                                                                
060100     IF SEGMENT-FINNS                                                     
060200         MOVE RET-IDDC              TO W-MINKEYB1-IDDC-ENTER              
060300         MOVE RET-IDRT              TO W-MINKEYB1-IDRT-ENTER              
060400         MOVE RET-IDRTLOP           TO W-MINKEYB1-IDRTLOP-ENTER           
060500         MOVE RET-IDKOLLI           TO W-MINKEYB1-IDKOLLI-ENTER           
060600     ELSE                                                                 
060700         MOVE MSGI-IDDC             TO W-MINKEYB1-IDDC-ENTER              
060800         MOVE ZERO                  TO W-MINKEYB1-IDRT-ENTER              
060900                                       W-MINKEYB1-IDRTLOP-ENTER           
061000                                       W-MINKEYB1-IDKOLLI-ENTER           
061100     END-IF                                                               
061200     MOVE '4732'                    TO W-MINKEY-IDTRANS                   
061300     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
061400     .                                                                    
061500     EJECT                                                                
061600 FAB-FIXA-NEXT-KEY SECTION.                                               
061700     SKIP2                                                                
061800     IF SEGMENT-FINNS                                                     
061900         MOVE INF-MORE-INFO-FINNS   TO MED-IDMFSINF                       
062000         CALL WMEDKONV USING MED-WMEDAREA                                 
062100         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
062200                                                                          
062300         MOVE RET-IDDC              TO W-MINKEYB1-IDDC-NEXT               
062400         MOVE RET-IDRT              TO W-MINKEYB1-IDRT-NEXT               
062500         MOVE RET-IDRTLOP           TO W-MINKEYB1-IDRTLOP-NEXT            
062600         MOVE RET-IDKOLLI           TO W-MINKEYB1-IDKOLLI-NEXT            
062700     ELSE                                                                 
062800         MOVE MSGI-IDDC             TO W-MINKEYB1-IDDC-NEXT               
062900         MOVE ZERO                  TO W-MINKEYB1-IDRT-NEXT               
063000                                       W-MINKEYB1-IDRTLOP-NEXT            
063100                                       W-MINKEYB1-IDKOLLI-NEXT            
063200     END-IF                                                               
063300     MOVE '4732'                    TO W-MINKEY-IDTRANS                   
063400     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
063500     .                                                                    
063600     EJECT                                                                
063700 FAC-RED-KDKOLSTA SECTION.                                                
063800     SKIP2                                                                
063900     MOVE RET-KDKOLSTA          TO W-KDKOLSTA                             
064000     IF W-PACK-KEY OR W-SANT-KEY OR W-LOSS-KEY OR W-MOTT-KEY OR           
064100        W-SAKN-KEY OR W-AVVI-KEY OR W-BEHA-KEY                            
064200       IF MSGI-IDLAND-SPR = 'GB'                                          
064300         EVALUATE TRUE                                                    
064400         WHEN W-PACK-KEY                                                  
064500            MOVE 'PACK'           TO MOD-BESTATUS (RAD-IX)                
064600         WHEN W-SANT-KEY                                                  
064700            MOVE 'SENT'           TO MOD-BESTATUS (RAD-IX)                
064800         WHEN W-LOSS-KEY                                                  
064900            MOVE 'UNL '           TO MOD-BESTATUS (RAD-IX)                
065000         WHEN W-MOTT-KEY                                                  
065100            MOVE 'REC '           TO MOD-BESTATUS (RAD-IX)                
065200         WHEN W-SAKN-KEY                                                  
065300            MOVE 'MIS '           TO MOD-BESTATUS (RAD-IX)                
065400         WHEN W-AVVI-KEY                                                  
065500            MOVE 'DEV '           TO MOD-BESTATUS (RAD-IX)                
065600         WHEN W-BEHA-KEY                                                  
065700            MOVE 'TREA'           TO MOD-BESTATUS (RAD-IX)                
065800         END-EVALUATE                                                     
065900       ELSE                                                               
066000         EVALUATE TRUE                                                    
066100         WHEN W-PACK-KEY                                                  
066200            MOVE 'PACK'           TO MOD-BESTATUS (RAD-IX)                
066300         WHEN W-SANT-KEY                                                  
066400            MOVE 'SÄNT'           TO MOD-BESTATUS (RAD-IX)                
066500         WHEN W-LOSS-KEY                                                  
066600            MOVE 'LOSS'           TO MOD-BESTATUS (RAD-IX)                
066700         WHEN W-MOTT-KEY                                                  
066800            MOVE 'MOT '           TO MOD-BESTATUS (RAD-IX)                
066900         WHEN W-SAKN-KEY                                                  
067000            MOVE 'SAK '           TO MOD-BESTATUS (RAD-IX)                
067100         WHEN W-AVVI-KEY                                                  
067200            MOVE 'AVV '           TO MOD-BESTATUS (RAD-IX)                
067300         WHEN W-BEHA-KEY                                                  
067400            MOVE 'BEH '           TO MOD-BESTATUS (RAD-IX)                
067500         END-EVALUATE                                                     
067600       END-IF                                                             
067700     END-IF                                                               
067800     .                                                                    
067900     EJECT                                                                
068000 G-KOLLA-INPUT SECTION.                                                   
068100                                                                          
068200     MOVE JA  TO INDATA-SW                                                
068300                                                                          
068400     PERFORM GA-FORMELL-KONTROLL                                          
068500     IF KOLLI-OK                                                          
068600         PERFORM GB-LOGISK-KONTROLL                                       
068700     END-IF                                                               
068800                                                                          
068900                                                                          
069000     IF KOLLI-FEL OR INDATA-FEL                                           
069100       MOVE NEJ       TO INDATA-SW                                        
069200       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
069300       CALL WMEDKONV USING MED-WMEDAREA                                   
069400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
069500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
069600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 GA-FORMELL-KONTROLL SECTION.                                             
070100     SKIP2                                                                
070200     MOVE +1 TO RAD-IX                                                    
070300     MOVE NEJ TO INPUT-FINNS-SW                                           
070400     PERFORM UNTIL RAD-IX > 13                                            
070500         IF MID-INPUT (RAD-IX) NOT = ALL '+'                              
070600             MOVE JA TO INPUT-FINNS-SW                                    
070700         END-IF                                                           
070800         ADD +1 TO RAD-IX                                                 
070900     END-PERFORM                                                          
071000                                                                          
071100     IF INPUT-FINNS                                                       
071200       PERFORM GAA-KOLLA-KDCMD-ADINLOMR                                   
071300     ELSE                                                                 
071400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
071500       CALL WMEDKONV USING MED-WMEDAREA                                   
071600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
071700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
071800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
071900       MOVE NEJ TO INDATA-SW                                              
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 GAA-KOLLA-KDCMD-ADINLOMR SECTION.                                        
072400     SKIP2                                                                
072500     MOVE NEJ TO UPPDATERA-KOLLI-SW                                       
072600     MOVE +1  TO RAD-IX                                                   
072700                                                                          
072800     PERFORM UNTIL RAD-IX > 13                                            
072900                                                                          
073000         IF MID-KDCMD (RAD-IX)  NOT = ALL '+'                             
073100                                                                          
073200             IF MID-KDCMD (RAD-IX) = W-AVVIKELSE OR W-DEVIATION           
073300                                                                          
073400                 IF MID-ADINLOMR-UPD (RAD-IX) = ALL '+'                   
073500                     MOVE MFS-ALFA-FAELT-RAETT                            
073600                         TO MOD-KDCMD-ATTR (RAD-IX)                       
073700                            MOD-ADINLOMR-UPD-ATTR (RAD-IX)                
073800                            MOVE JA TO UPPDATERA-KOLLI-SW                 
073900                 ELSE                                                     
074000                     MOVE MFS-ALFA-FAELT-FEL                              
074100                         TO MOD-KDCMD-ATTR (RAD-IX)                       
074200                            MOD-ADINLOMR-UPD-ATTR (RAD-IX)                
074300                     MOVE NEJ      TO INDATA-SW                           
074400                 END-IF                                                   
074500*                                                                         
074600             ELSE                                                         
074700*                                                                         
074800                 IF MID-KDCMD (RAD-IX) = W-MOTTAGET OR W-DELETE OR        
074900                                         W-RECEIVED OR W-BORTTAG          
075000                     MOVE MFS-ALFA-FAELT-RAETT                            
075100                          TO MOD-KDCMD-ATTR (RAD-IX)                      
075200                             MOD-ADINLOMR-UPD (RAD-IX)                    
075300                     MOVE JA TO UPPDATERA-KOLLI-SW                        
075400                 ELSE                                                     
075500*                                                                         
075600                    IF MID-KDCMD (RAD-IX) NUMERIC                         
075700*                                                                         
075800                       IF MID-ADINLOMR-UPD (RAD-IX) = ALL '+'             
075900                          MOVE MFS-ALFA-FAELT-RAETT                       
076000                               TO MOD-KDCMD-ATTR (RAD-IX)                 
076100                                  MOD-ADINLOMR-UPD (RAD-IX)               
076200                       ELSE                                               
076300                          MOVE MFS-ALFA-FAELT-FEL                         
076400                              TO MOD-KDCMD-ATTR (RAD-IX)                  
076500                                 MOD-ADINLOMR-UPD-ATTR (RAD-IX)           
076600                          MOVE NEJ      TO INDATA-SW                      
076700                       END-IF                                             
076800                    ELSE                                                  
076900                       MOVE MFS-ALFA-FAELT-FEL                            
077000                           TO MOD-KDCMD-ATTR (RAD-IX)                     
077100                              MOD-ADINLOMR-UPD-ATTR (RAD-IX)              
077200                       MOVE NEJ      TO INDATA-SW                         
077300*                                                                         
077400                    END-IF                                                
077500                 END-IF                                                   
077600             END-IF                                                       
077700         ELSE                                                             
077800             IF MID-ADINLOMR-UPD (RAD-IX) NOT = ALL '+'                   
077900                 MOVE MFS-ALFA-FAELT-RAETT TO                             
078000                      MOD-ADINLOMR-UPD-ATTR (RAD-IX)                      
078100                      MOVE JA TO UPPDATERA-KOLLI-SW                       
078200             END-IF                                                       
078300         END-IF                                                           
078400         ADD +1 TO RAD-IX                                                 
078500     END-PERFORM                                                          
078600     .                                                                    
078700     EJECT                                                                
078800 GB-LOGISK-KONTROLL SECTION.                                              
078900     SKIP2                                                                
079000     MOVE MSGI-IDRT              TO W-IDRT-BSEQ-MIN                       
079100                                    W-IDRT-BSEQ-MAX                       
079200     MOVE MSGI-IDRTLOP           TO W-IDRTLOP-BSEQ-MIN                    
079300                                    W-IDRTLOP-BSEQ-MAX                    
079400     MOVE +1 TO RAD-IX                                                    
079500     PERFORM UNTIL RAD-IX > 13                                            
079600         IF MID-INPUT (RAD-IX) = ALL '+'                                  
079700         OR MID-KDCMD (RAD-IX) NUMERIC                                    
079800            CONTINUE                                                      
079900         ELSE                                                             
080000                MOVE MID-IDKOLLI (RAD-IX)    TO W-IDKOLLI-BSEQ-MIN        
080100                                                W-IDKOLLI-BSEQ-MAX        
080200                PERFORM IMS-GU-SEQB-WLRETA01                              
080300                                                                          
080400                IF SEGMENT-FINNS                                          
080500                  IF RET-IDKOLLI > ZERO                                   
080600                    IF MID-KDCMD (RAD-IX) = W-AVVIKELSE OR                
080700                                            W-DEVIATION                   
080800                        IF RET-KDRETSTA = 2 OR 3 OR 4                     
080900                            CONTINUE                                      
081000                        ELSE                                              
081100                            MOVE MFS-ALFA-FAELT-FEL TO                    
081200                                 MOD-KDCMD-ATTR (RAD-IX)                  
081300                            MOVE NEJ TO UPPDATERA-KOLLI-SW                
081400                        END-IF                                            
081500                    ELSE                                                  
081600                        IF MID-KDCMD (RAD-IX) = W-MOTTAGET OR             
081700                                                W-RECEIVED                
081800                            IF RET-KDKOLSTA = W-KLI-SAK  OR               
081900                                              W-KLI-AVV                   
082000                                CONTINUE                                  
082100                            ELSE                                          
082200                                MOVE MFS-ALFA-FAELT-FEL TO                
082300                                 MOD-KDCMD-ATTR (RAD-IX)                  
082400                                MOVE NEJ TO UPPDATERA-KOLLI-SW            
082500                            END-IF                                        
082600                        END-IF                                            
082700                        IF MID-KDCMD (RAD-IX) = W-BORTTAG OR              
082800                                                W-DELETE                  
082900                          IF RET-KDRETSTA = 2   OR                        
083000                            (RET-KDRETSTA = 3   AND                       
083100                             RET-IDDISTR  = ZERO)                         
083200                              CONTINUE                                    
083300                          ELSE                                            
083400                              MOVE MFS-ALFA-FAELT-FEL TO                  
083500                                   MOD-KDCMD-ATTR (RAD-IX)                
083600                                MOVE NEJ TO UPPDATERA-KOLLI-SW            
083700                          END-IF                                          
083800                        END-IF                                            
083900                    END-IF                                                
084000                  ELSE                                                    
084100                      MOVE MFS-ALFA-FAELT-FEL TO                          
084200                           MOD-KDCMD-ATTR (RAD-IX)                        
084300                           MOD-ADINLOMR-UPD-ATTR (RAD-IX)                 
084400                      MOVE NEJ TO UPPDATERA-KOLLI-SW                      
084500                  END-IF                                                  
084600                ELSE                                                      
084700                    MOVE MFS-ALFA-FAELT-FEL TO                            
084800                         MOD-KDCMD-ATTR (RAD-IX)                          
084900                         MOD-ADINLOMR-UPD-ATTR (RAD-IX)                   
085000                    MOVE NEJ TO UPPDATERA-KOLLI-SW                        
085100                END-IF                                                    
085200         END-IF                                                           
085300         ADD +1 TO RAD-IX                                                 
085400     END-PERFORM                                                          
085500     .                                                                    
085600     EJECT                                                                
085700 H-UPPDATERA SECTION.                                                     
085800                                                                          
085900     MOVE +1 TO RAD-IX                                                    
086000                4792-INDX                                                 
086100     PERFORM UNTIL RAD-IX > 13                                            
086200         IF MID-KDCMD (RAD-IX) = W-MOTTAGET                               
086300         OR MID-KDCMD (RAD-IX) = W-AVVIKELSE                              
086400         OR MID-KDCMD (RAD-IX) = W-BORTTAG                                
086500         OR MID-KDCMD (RAD-IX) = W-RECEIVED                               
086600         OR MID-KDCMD (RAD-IX) = W-DEVIATION                              
086700         OR MID-KDCMD (RAD-IX) = W-DELETE                                 
086800         OR MID-ADINLOMR-UPD (RAD-IX) NOT = ALL '+'                       
086900             MOVE MSGI-IDRT TO  W-IDRT-BSEQ-MIN                           
087000                                W-IDRT-BSEQ-MAX                           
087100             MOVE MSGI-IDRTLOP    TO  W-IDRTLOP-BSEQ-MIN                  
087200                                      W-IDRTLOP-BSEQ-MAX                  
087300             MOVE MID-IDKOLLI (RAD-IX) TO W-IDKOLLI-BSEQ-MIN              
087400                                          W-IDKOLLI-BSEQ-MAX              
087500*                                                                         
087600             PERFORM IMS-GHU-SEQB-WLRETA01                                
087700*                                                                         
087800             PERFORM UNTIL SEGMENT-SAKNAS                                 
087900*                                                                         
088000                 IF MID-KDCMD (RAD-IX) = W-MOTTAGET OR W-RECEIVED         
088100                                                                          
088200                     IF RET-KDRETSTA = 2 OR 3                             
088300                         IF RET-KDKOLSTA = W-KLI-SAK                      
088400                             PERFORM S02-FYLL-R31-MID                     
088500                         END-IF                                           
088600                     END-IF                                               
088700                                                                          
088800                     MOVE 5 TO RET-KDKOLSTA                               
088900                     MOVE 4 TO RET-KDRETSTA                               
089000                     IF RET-TILOSSN  = ZERO                               
089100                        ACCEPT RET-TILOSSN  FROM DATE                     
089200                     END-IF                                               
089300                     IF RET-DARETANK = ZERO                               
089400                       MOVE FUNCTION CURRENT-DATE (1:8)                   
089500                                     TO RET-DARETANK                      
089600                     END-IF                                               
089700                     IF RET-TIINLMOT = ZERO                               
089800                        ACCEPT RET-TIINLMOT FROM DATE                     
089900                     END-IF                                               
090000                                                                          
090100                     IF MID-ADINLOMR-UPD (RAD-IX) NOT = ALL '+'           
090200                         MOVE MID-ADINLOMR-UPD (RAD-IX)                   
090300                             TO RET-ADINLOMR                              
090400                     END-IF                                               
090500                                                                          
090600                     PERFORM IMS-REPL-SEQB-WLRETA01                       
090700                 ELSE                                                     
090800                                                                          
090900                     IF MID-KDCMD (RAD-IX) = W-AVVIKELSE OR               
091000                                             W-DEVIATION                  
091100                         IF RET-KDRETSTA = 2                              
091200                             MOVE W-KLI-SAK TO RET-KDKOLSTA               
091300                         ELSE                                             
091400                             IF RET-KDRETSTA = 3 OR 4                     
091500                                 MOVE W-KLI-AVV   TO RET-KDKOLSTA         
091600                             END-IF                                       
091700                         END-IF                                           
091800                         PERFORM IMS-REPL-SEQB-WLRETA01                   
091900                     ELSE                                                 
092000                       IF MID-KDCMD (RAD-IX) = W-BORTTAG OR               
092100                                               W-DELETE                   
092200                          MOVE RET-DAREGDAT TO W-DAREGDAT                 
092300                          MOVE RET-TIKLOCK  TO W-TIKLOCK                  
092400                          PERFORM IMS-GHU-WLRETA01                        
092500                          PERFORM IMS-DLET-WLRETA01                       
092600                       ELSE                                               
092700                           IF MID-KDCMD (RAD-IX) = ALL '+'                
092800                           AND MID-ADINLOMR-UPD (RAD-IX)                  
092900                                   NOT = ALL '+'                          
093000                               MOVE MID-ADINLOMR-UPD (RAD-IX) TO          
093100                                   RET-ADINLOMR                           
093200                               PERFORM IMS-REPL-SEQB-WLRETA01             
093300                           END-IF                                         
093400                       END-IF                                             
093500                     END-IF                                               
093600                 END-IF                                                   
093700                 PERFORM IMS-GHN-SEQB-WLRETA01                            
093800             END-PERFORM                                                  
093900           END-IF                                                         
094000         ADD +1 TO RAD-IX                                                 
094100     END-PERFORM                                                          
094200                                                                          
094300     IF 4792-INDX  > +1                                                   
094400         PERFORM S03-STARTA-R31-RAPPORTERING                              
094500     END-IF                                                               
094600                                                                          
094700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
094800     CALL WMEDKONV USING MED-WMEDAREA                                     
094900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
095000     PERFORM MFS-FORM-ATTR                                                
095100     PERFORM MFS-RENSA-FAELT-IN                                           
095200     .                                                                    
095300     EJECT                                                                
095400 S01-INSERT-ALTMSG SECTION.                                               
095500                                                                          
095600     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
095700     PERFORM IMS-CHANGE-ALTMSG                                            
095800     IF STATUS-OK                                                         
095900       PERFORM IMS-INSERT-ALTMSG                                          
096000     ELSE                                                                 
096100       MOVE LOW-VALUE          TO MSG-AREA                                
096200       MOVE 'W4O73201'         TO MFS-IDMOD                               
096300       MOVE '4732'             TO MOD-IDTRANS                             
096400       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
096500       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
096600       IF SECURITY-FEL                                                    
096700         STRING 'NOT AUTHORIZED TO USE '                                  
096800                W-BILD                                                    
096900                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
097000       ELSE                                                               
097100         STRING 'WRONG PICTURE '                                          
097200                 W-BILD                                                   
097300                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
097400       END-IF                                                             
097500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73201 + 4                      
097600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
097700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
097800       PERFORM IMS-INSERT-MSG                                             
097900     END-IF                                                               
098000     .                                                                    
098100     EJECT                                                                
098200                                                                          
098300 S02-FYLL-R31-MID                SECTION.                                 
098400                                                                          
098500     MOVE RET-IDDC             TO MOD4792-MID-IDDC                        
098600     MOVE RET-DAREGDAT (3:6)   TO MOD4792-MID-TIREGDAT(4792-INDX)         
098700     MOVE RET-TIKLOCK          TO MOD4792-MID-TIKLOCK (4792-INDX)         
098800     ADD +1                    TO 4792-INDX                               
098900                                                                          
099000     IF 4792-INDX              >  4792-MAX-INDX                           
099100        PERFORM S03-STARTA-R31-RAPPORTERING                               
099200        MOVE +1                TO 4792-INDX                               
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700 S03-STARTA-R31-RAPPORTERING     SECTION.                                 
099800                                                                          
099900     ACCEPT DAGENS-DATUM       FROM DATE                                  
100000     ACCEPT DAGENS-TID         FROM TIME                                  
100100                                                                          
100200     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
100300     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
100400     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
100500     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
100600     MOVE SPACE                TO MSG-KOM-KDTRANS                         
100700     MOVE 'W4I79201'           TO MSG-KOM-IDCPYTXT                        
100800     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
100900     MOVE 'W4073200'           TO MSG-KOM-IDSNDJOB                        
101000     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
101100     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
101200     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
101300                                                                          
101400     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
101500                                  LENGTH OF MOD4792-MID-W4I79201          
101600                                                                          
101700     MOVE 'W4T792X '           TO P-TO-P-MSG-KDTRANS                      
101800     MOVE '4732'               TO P-TO-P-MSG-IDTRANS                      
101900     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
102000                                                                          
102100     COMPUTE MOD4792-MID-KVPOST  = 4792-INDX - 1                          
102200                                                                          
102300     MOVE MOD4792-MID-W4I79201 TO P-TO-P-MSG-INDATA                       
102400                                                                          
102500     CALL W006KOM USING MSG-PCB                                           
102600                        DISP-PCB                                          
102700                        KOM-KOMA-PCB                                      
102800                        MSG-KOM-WMSGKOM                                   
102900                        P-TO-P-MSG-IO-AREA-SNUF                           
103000                                                                          
103100     .                                                                    
103200     EJECT                                                                
103300 MFS-RENSA-FAELT-UT SECTION.                                              
103400                                                                          
103500*    --- ALLA UTDATA-FÄLT                                                 
103600     MOVE MFS-RENSA-FAELT TO MOD-IDFRASED                                 
103700                                                                          
103800     MOVE +1              TO RAD-IX                                       
103900     PERFORM UNTIL RAD-IX > 13                                            
104000        MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                        
104100                                MOD-ADINLOMR-UPD (RAD-IX)                 
104200                                MOD-ADINLOMR (RAD-IX)                     
104300                                MOD-IDKOLLI (RAD-IX)                      
104400                                MOD-FLFARLIG (RAD-IX)                     
104500                                MOD-BESTATUS (RAD-IX)                     
104510                                MOD-FLBUYBAC (RAD-IX)                     
104600         ADD +1 TO RAD-IX                                                 
104700     END-PERFORM                                                          
104800     .                                                                    
104900     SKIP3                                                                
105000                                                                          
105100 MFS-RENSA-FAELT-IN SECTION.                                              
105200                                                                          
105300*    --- ALLA INDATA-FÄLT                                                 
105400     MOVE +1 TO RAD-IX                                                    
105500     PERFORM UNTIL RAD-IX > 13                                            
105600         MOVE MFS-RENSA-FAELT TO  MOD-KDCMD (RAD-IX)                      
105700                                  MOD-ADINLOMR-UPD (RAD-IX)               
105800         ADD +1 TO RAD-IX                                                 
105900     END-PERFORM                                                          
106000     SKIP3                                                                
106100     .                                                                    
106200     EJECT                                                                
106300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
106400                                                                          
106500*    --- ALLA UTDATA-FÄLT                                                 
106600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFRASED                               
106700                                                                          
106800     MOVE +1 TO RAD-IX                                                    
106900     PERFORM UNTIL RAD-IX > 13                                            
107000         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (RAD-IX)                     
107100                                   MOD-ADINLOMR-UPD (RAD-IX)              
107200                                   MOD-ADINLOMR (RAD-IX)                  
107300                                   MOD-IDKOLLI (RAD-IX)                   
107400                                   MOD-FLFARLIG (RAD-IX)                  
107500                                   MOD-BESTATUS (RAD-IX)                  
107510                                   MOD-FLBUYBAC (RAD-IX)                  
107600         ADD +1 TO RAD-IX                                                 
107700     END-PERFORM                                                          
107800     .                                                                    
107900     EJECT                                                                
108000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
108100                                                                          
108200     MOVE +1 TO RAD-IX                                                    
108300     PERFORM UNTIL RAD-IX > 13                                            
108400         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (RAD-IX)                     
108500                                   MOD-ADINLOMR-UPD (RAD-IX)              
108600         ADD +1 TO RAD-IX                                                 
108700     END-PERFORM                                                          
108800     .                                                                    
108900     EJECT                                                                
109000 MFS-FORM-ATTR SECTION.                                                   
109100                                                                          
109200     MOVE +1 TO RAD-IX                                                    
109300     PERFORM UNTIL RAD-IX > 13                                            
109400       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (RAD-IX)                 
109500                                  MOD-ADINLOMR-UPD-ATTR (RAD-IX)          
109600       ADD +1 TO RAD-IX                                                   
109700     END-PERFORM                                                          
109800     .                                                                    
109900     SKIP2                                                                
110000 MFS-LAES-IN-IGEN SECTION.                                                
110100                                                                          
110200     MOVE +1 TO RAD-IX                                                    
110300     PERFORM UNTIL RAD-IX > 13                                            
110400       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCMD-ATTR (RAD-IX)             
110500                                  MOD-ADINLOMR-UPD-ATTR (RAD-IX)          
110600       ADD +1 TO RAD-IX                                                   
110700     END-PERFORM                                                          
110800     .                                                                    
110900     EJECT                                                                
111000* --- IMS SEKTIONER ---                                                   
111100     SKIP3                                                                
111200 IMS-GET-MSG SECTION.                                                     
111300                                                                          
111400     MOVE '  QC' TO GODK-STATUSKODER                                      
111500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
111600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111700     PERFORM IMS-STATUSKONTROLL                                           
111800     .                                                                    
111900     SKIP3                                                                
112000 IMS-INSERT-MSG SECTION.                                                  
112100                                                                          
112200     IF MSGI-IDLAND-SPR = 'GB'                                            
112300       MOVE 'N' TO MFS-KDHUVOMR                                           
112400     END-IF                                                               
112500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
112600     MOVE SPACE TO GODK-STATUSKODER                                       
112700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
112800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     EJECT                                                                
113200 IMS-GHU-WLRETA01 SECTION.                                                
113300                                                                          
113400     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
113500          DELIMITED BY SIZE INTO SSA1                                     
113600     MOVE '  ' TO GODK-STATUSKODER                                        
113700     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-RETA01 SSA1                   
113800     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
113900     PERFORM IMS-STATUSKONTROLL                                           
114000     .                                                                    
114100     SKIP3                                                                
114200 IMS-DLET-WLRETA01 SECTION.                                               
114300                                                                          
114400     MOVE '  ' TO GODK-STATUSKODER                                        
114500     CALL CBLTDLI USING DLET RETA-PCB DLI-IO-RETA01                       
114600     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
114700     PERFORM IMS-STATUSKONTROLL                                           
114800     .                                                                    
114900     EJECT                                                                
115000 IMS-GU-SEQB-WLRETA01 SECTION.                                            
115100                                                                          
115200     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
115300                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
115400          DELIMITED BY SIZE INTO SSA1                                     
115500     MOVE '  GE' TO GODK-STATUSKODER                                      
115600     CALL CBLTDLI USING GU SEQB-PCB DLI-IO-RETA01 SSA1                    
115700     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     SKIP3                                                                
116100 IMS-GN-SEQB-WLRETA01 SECTION.                                            
116200                                                                          
116300     STRING 'WLRETA01(WDA3BSEQ> ' W-WDA3BSEQ-MIN-X                        
116400                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
116500          DELIMITED BY SIZE INTO SSA1                                     
116600     MOVE '  GE' TO GODK-STATUSKODER                                      
116700     CALL CBLTDLI USING GN SEQB-PCB DLI-IO-RETA01 SSA1                    
116800     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
116900     PERFORM IMS-STATUSKONTROLL                                           
117000     .                                                                    
117100     EJECT                                                                
117200 IMS-GHU-SEQB-WLRETA01 SECTION.                                           
117300                                                                          
117400     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
117500                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
117600          DELIMITED BY SIZE INTO SSA1                                     
117700     MOVE '  GE' TO GODK-STATUSKODER                                      
117800     CALL CBLTDLI USING GHU SEQB-PCB DLI-IO-RETA01 SSA1                   
117900     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     .                                                                    
118200     SKIP3                                                                
118300 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
118400                                                                          
118500     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
118600                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
118700          DELIMITED BY SIZE INTO SSA1                                     
118800     MOVE '  GE' TO GODK-STATUSKODER                                      
118900     CALL CBLTDLI USING GHN SEQB-PCB DLI-IO-RETA01 SSA1                   
119000     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     .                                                                    
119300     EJECT                                                                
119400 IMS-REPL-SEQB-WLRETA01 SECTION.                                          
119500                                                                          
119600     MOVE '  ' TO GODK-STATUSKODER                                        
119700     CALL CBLTDLI USING REPL SEQB-PCB DLI-IO-RETA01                       
119800     MOVE SEQB-STATUS-CODE TO STATUS-WS                                   
119900     PERFORM IMS-STATUSKONTROLL                                           
120000     .                                                                    
120100     EJECT                                                                
120200 IMS-CHANGE-ALTMSG SECTION.                                               
120300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
120400     MOVE '  A1A4' TO GODK-STATUSKODER                                    
120500     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
120600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900     SKIP3                                                                
121000 IMS-INSERT-ALTMSG SECTION.                                               
121100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
121200     MOVE SPACE TO GODK-STATUSKODER                                       
121300     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
121400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     .                                                                    
121700     EJECT                                                                
121800 IMS-STATUSKONTROLL SECTION.                                              
121900                                                                          
122000     SET STATUS-IX TO 1                                                   
122100     SEARCH GODK-STATUS                                                   
122200       AT END                                                             
122300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
122400         DELIMITED BY SIZE INTO FELTEXT                                   
122500         CALL FELLOG                                                      
122600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
122700         CONTINUE                                                         
122800     END-SEARCH                                                           
122900     .                                                                    
