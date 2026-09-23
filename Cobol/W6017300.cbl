000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0110      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W6017300.                                                
000800 AUTHOR.         BERT ANDERSSON.                                          
000900 DATE-WRITTEN.   92/05/06.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*    FUNKTION:                                                            
001300*        PROGRAMMET UTFÖR UTSKRIFT AV NYA FLAGGOR FÖR EGET                
001400*        C-LAGER EFTER PÅFYLLNING.                                        
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR W6PLAA (W6G1)                              
001700*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W6T173                                              
002200*        MID:         W6I17301                                            
002300*                     W6I17302 FROM VCOM VIA W00694                       
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W6O17301                                            
002700*                                                                         
002800*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
002900*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
003000*    E-TRACKER:10263392  2015       RAHL AND PULS.                        
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W6017300'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  IX                          PIC S9(9)   VALUE +0 COMP SYNC.          
004700 77  6194-IX                     PIC S9(9)   VALUE +0 COMP SYNC.          
004800 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17 COMP SYNC.          
004900                                                                          
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100 77  WS-KVINLART-X               PIC X(6)    VALUE SPACE.                 
005200*    --- ARBETSFÄLT FÖR AKTUELLA INDATAVÄRDEN FRÅN SKÄRMEN                
005300 77  WS-KVINLART                 PIC 9(6)    VALUE ZERO.                  
005400 77  WS-KVINLART-LAST            PIC 9(6)    VALUE ZERO.                  
005500 77  WS-TIINLMOT                 PIC 9(6)    VALUE ZERO COMP-3.           
005600 77  WS-KVFLETI                  PIC 9(2)    VALUE ZERO COMP-3.           
005700*    --- ÖVRIGA ARBETSFÄLT                                                
005800 77  WS-ADLAGOMR                 PIC 9(2)    VALUE ZERO.                  
005900 77  WS-ADGANG                   PIC 9(2)    VALUE ZERO.                  
006000 77  WS-ADPLATS                  PIC 9(5)    VALUE ZERO.                  
006100 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
006200 77  WS-VKKOLLIN                 PIC 9(7)    VALUE ZERO.                  
006300 77  WS-VKKOLLIB                 PIC 9(7)    VALUE ZERO.                  
006400 77  WS-BEFT                     PIC 9(3)    VALUE ZERO.                  
006500 77  WS-IDPRTLST                 PIC X(8)    VALUE SPACE.                 
006510 77  WS-FL-WDK712                PIC X       VALUE 'N'.                   
006600                                                                          
006700* -COPY WY2000W1                                                          
006800                                                                          
006900* -COPY WWOMVAND                                                          
007000                                                                          
007100*      --- VALID IDDC                                                     
007200*                                                                         
007300*01    -COPY WWDC99                                                       
007301*      -COPY WWLNDKON                                                     
007310                                                                          
007320*      --- VALID IDDC CODES                                               
007330*                                                                         
007340*01    -COPY WWDCKONS                                                     
007400       EJECT                                                              
007500                                                                          
007600 01  WS-ADINLOMR-PRT-GRP.                                                 
007700     03  WS-KONSTANT-F           PIC X       VALUE 'F'.                   
007800     03  WS-ADINLOMR-PRT         PIC X(7)    VALUE SPACE.                 
007900                                                                          
008000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008100     88  INDATA-OK                           VALUE 'J'.                   
008200     88  INDATA-FEL                          VALUE 'N'.                   
008300                                                                          
008400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008500     88  NYCKLAR-OK                          VALUE 'J'.                   
008600     88  NYCKLAR-FEL                         VALUE 'N'.                   
008700                                                                          
008800 77  LEV-SW                      PIC X       VALUE 'J'.                   
008900     88  LEV-OK                              VALUE 'J'.                   
009000     88  LEV-FEL                             VALUE 'N'.                   
009100                                                                          
009200 77  KOLLI-SW                    PIC X       VALUE 'J'.                   
009300     88  KOLLI-OK                            VALUE 'J'.                   
009400     88  KOLLI-FEL                           VALUE 'N'.                   
009500                                                                          
009600 77  EGET-LEVNR-SW               PIC X       VALUE 'J'.                   
009700     88  EGET-LEVNR                          VALUE 'J'.                   
009800     88  EXTERNT-LEVNR                       VALUE 'N'.                   
009900                                                                          
010000 77  KOLLINR-SW                  PIC X       VALUE 'J'.                   
010100     88  KOLLINR-FINNS                       VALUE 'J'.                   
010200     88  KOLLINR-SAKNAS                      VALUE 'N'.                   
010300                                                                          
010400 77  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
010500     88  ARTIKEL-SAKNAS                      VALUE 'J'.                   
010600                                                                          
010700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010800     88  ALLT-OK                             VALUE 'J'.                   
010900                                                                          
011000 77  6194-SW                     PIC X       VALUE 'J'.                   
011100     88  FOERSTA-6194                        VALUE 'J'.                   
011200                                                                          
011300 77  FL-LOPA-UPPDATERAD          PIC X       VALUE 'N'.                   
011400                                                                          
011500 77  MID-INPUT-SW                PIC X       VALUE 'N'.                   
011600     88  MID-INPUT-SAKNAS                    VALUE 'J'.                   
011700                                                                          
011800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011900     88  EGEN-MID                            VALUE '6173'.                
012000     88  GODK-MID                            VALUE '6173'.                
012100     88  HELP-MID                            VALUE '0551'.                
012200     88  VCOM-MID                            VALUE '0694'.                
012300       EJECT                                                              
012400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012500 01  GENERELLA-SUBPROGRAM.                                                
012600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
013100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013200     EJECT                                                                
013300*01 -COPY WMSGINIT                                                        
013400     EJECT                                                                
013500*    --- SUBPROGRAM WDATKONV                                              
013600*01 -COPY WDATAREA                                                        
013700     EJECT                                                                
013800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013900*01 -COPY WMEDAREA                                                        
014000     EJECT                                                                
014100 01  MESSAGE-CODES.                                                       
014200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014500     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
014600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014700     03  ERR-CASE-NOT-STORED     PIC X(3)    VALUE '229'.                 
014800     03  ERR-WRONG-CASE-OR-SUPP  PIC X(3)    VALUE '233'.                 
014900     03  ERR-PRT-MISSING         PIC X(3)    VALUE '772'.                 
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'W006PRT'.             
015200     SKIP2                                                                
015300*01  -COPY W006PRT                                                        
015400     EJECT                                                                
015500 01      FILLER                  PIC X(24)   VALUE                        
015600                                 'MOD6194-MID-W6I19401'.                  
015700*01  -COPY W6I19401 -PRE MOD6194-                                         
015800     EJECT                                                                
015900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016000*                                                                         
016100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016200     SKIP3                                                                
016300*01  MID -COPY W6I17301                                                   
016400     EJECT                                                                
016500*01  -COPY W6I17302 -PRE VCOM-                                            
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016800     SKIP3                                                                
016900*01  -COPY WMSGAREA                                                       
017000     EJECT                                                                
017100     03  MOD REDEFINES MSG-AREA.                                          
017200*      05  -COPY W6O17301                                                 
017300     EJECT                                                                
017400 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
017500     SKIP3                                                                
017600 01      P-TO-P-SW.                                                       
017700                                                                          
017800  02     P-TO-P-KVLL             PIC S9(4)   COMP SYNC.                   
017900  02     P-TO-P-KDZ1             PIC X(1)    VALUE LOW-VALUE.             
018000  02     P-TO-P-KDZ2             PIC X(1)    VALUE LOW-VALUE.             
018100  02     P-TO-P-KDTRANS          PIC X(8).                                
018200  02     P-TO-P-IDTRANS          PIC X(4).                                
018300  02     P-TO-P-KDMFSFOR         PIC X(1).                                
018400  02     P-TO-P-DATA             PIC X(4079).                             
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018700     SKIP3                                                                
018800*01  -COPY WMFSAREA                                                       
018900     EJECT                                                                
019000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019100*                                                                         
019200     SKIP3                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019400     SKIP3                                                                
019500 01  NYCKLAR-TILL-DLI.                                                    
019600     03  W-W6GXKEY-6005-X.                                                
019700         05  W-IDHTYP-6005       PIC  X(4)   VALUE '6005'.                
019800         05  W-IDDC-6005         PIC  X(2)   VALUE SPACE.                 
019900         05  W-FILLER            PIC  X(24)  VALUE LOW-VALUE.             
020000     03  W-W6GXKEY-6006-X.                                                
020100         05  W-ADINLOMR-6006     PIC  X(4)   VALUE SPACE.                 
020200         05  W-FILLER            PIC  X      VALUE LOW-VALUE.             
020300     03  W-W6GX01KEY-X.                                                   
020400         05  W-IDHTYP-01         PIC  X(4)   VALUE SPACE.                 
020500         05  W-FILLER            PIC  X(26)  VALUE LOW-VALUE.             
020600     03  W-KDSEGKEY-X.                                                    
020700         05  W-KDSEGKEY          PIC  X      VALUE '1'.                   
020800     03  W-IDARTNR-X.                                                     
020900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021000     03  W-W6D1CSEQ-X.                                                    
021100         05  W-IDLEVNRK          PIC  X(5)   VALUE SPACE.                 
021200         05  W-IDOKOLLIK         PIC  9(9).                               
021300     03  W-IDLEVNR-X.                                                     
021400         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
021500     03  W-IDOKOLLI-X.                                                    
021600         05  W-IDOKOLLI          PIC 9(9).                                
021700     03  W-IDDC-X.                                                        
021800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021900     03  W-IDDC-B6-X.                                                     
022000         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
022020     03  W-IDLAND-X.                                                      
022030         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
022100                                                                          
022200     SKIP2                                                                
022300*    --- STATUS-KOD FRÅN IMS                                              
022400 01  STATUS-WS                   PIC XX.                                  
022500     88  SEGMENT-FINNS                       VALUE '  '.                  
022600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022800     SKIP2                                                                
022900 01  GODK-STATUSKODER.                                                    
023000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023100     SKIP3                                                                
023200 01  SSA1                        PIC X(64).                               
023300 01  SSA2                        PIC X(64).                               
023400     EJECT                                                                
023500*    --- IMS FUNKTIONSKODER                                               
023600*01  -COPY W0003                                                          
023700     EJECT                                                                
023800*    ---  DLI INPUT-OUTPUT AREA  1                                        
023900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-1'.         
024000     SKIP3                                                                
024100 01  DLI-IO-AREA-1.                                                       
024200     03  IO-AREA-1             PIC X(900)  VALUE SPACE.                   
024300     SKIP3                                                                
024400     03  W6PLAA01 REDEFINES IO-AREA-1.                                    
024500*        05  -COPY W6GX01                                                 
024600     SKIP3                                                                
024700     03  W6PLAA11 REDEFINES IO-AREA-1.                                    
024800*        05  -COPY W6GX6006                                               
024900     EJECT                                                                
025000     03  WLARTC01 REDEFINES IO-AREA-1.                                    
025100*        05  -COPY WDK601 -PRE ARTC01-                                    
025200     SKIP3                                                                
025300     03  WLARTC11 REDEFINES IO-AREA-1.                                    
025400*        05  -COPY WDK611 -PRE ARTC11-                                    
025500     SKIP3                                                                
025600     03  WLINLA11 REDEFINES IO-AREA-1.                                    
025700*        05  -COPY W6D111                                                 
025800     SKIP3                                                                
025900     03  WLINLA21 REDEFINES IO-AREA-1.                                    
026000*        05  -COPY W6D121                                                 
026100     EJECT                                                                
026200*    ---  DLI INPUT-OUTPUT AREA  2                                        
026300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
026400     SKIP3                                                                
026500 01  DLI-IO-AREA-2.                                                       
026600     03  IO-AREA-2             PIC X(150)  VALUE SPACE.                   
026700     SKIP3                                                                
026800     03  W6LOPA01 REDEFINES IO-AREA-2.                                    
026900*        05  -COPY W6GX01                                                 
027000     SKIP3                                                                
027100     03  W6LOPA11 REDEFINES IO-AREA-2.                                    
027200*        05  -COPY W6GX6018                                               
027300     EJECT                                                                
027400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
027500     SKIP3                                                                
027600 01  DLI-IO-AREA-WDK701.                                                  
027700*    03  -COPY WDK701                                                     
027800     EJECT                                                                
027900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
028000     SKIP3                                                                
028100 01  DLI-IO-AREA-WDK711.                                                  
028200*    03  -COPY WDK711                                                     
028210     EJECT                                                                
028220 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK712'.           
028230     SKIP3                                                                
028240 01  DLI-IO-AREA-WDK712.                                                  
028250*    03  -COPY WDK712                                                     
028300     EJECT                                                                
028400     EJECT                                                                
028500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
028600 01   DLI-IO-AREA-B601.                                                   
028700*     03  -COPY WDB601                                                    
028800                                                                          
028900     EJECT                                                                
029000 LINKAGE SECTION.                                                         
029100                                                                          
029200*01  -COPY W0009  -PRE MSG-                                               
029300     SKIP2                                                                
029400*01  -COPY W0008  -PRE 6194-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE USEA-                                              
029800     05  FILLER                  PIC X.                                   
029900     SKIP2                                                                
030000*01  -COPY W0008  -PRE PLAA-                                              
030100     05  FILLER                  PIC X.                                   
030200     SKIP2                                                                
030300*01  -COPY W0008  -PRE ARTC-                                              
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE LOPA-                                              
030700     05  FILLER                  PIC X.                                   
030800     SKIP2                                                                
030900*01  -COPY W0008  -PRE INLA-                                              
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008  -PRE WDK7-                                              
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE WDB6-                                              
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800     EJECT                                                                
031900 PROCEDURE DIVISION  USING MSG-PCB 6194-PCB USEA-PCB PLAA-PCB             
032000                                   ARTC-PCB LOPA-PCB INLA-PCB             
032100                                   WDK7-PCB WDB6-PCB.                     
032200 W60173 SECTION.                                                          
032300     ENTRY 'DLITCBL' USING MSG-PCB 6194-PCB USEA-PCB PLAA-PCB             
032400                                   ARTC-PCB LOPA-PCB INLA-PCB             
032500                                   WDK7-PCB WDB6-PCB.                     
032600                                                                          
032700     PERFORM IMS-GET-MSG                                                  
032800     IF SEGMENT-FINNS                                                     
032900       PERFORM A-INIT                                                     
033000       PERFORM B-KOLLA-NYCKLAR                                            
033100       IF NYCKLAR-OK                                                      
033200         IF MFS-UPDATE OR MFS-UPD-X                                       
033300           PERFORM G-KOLLA-INPUT                                          
033400           IF INDATA-OK                                                   
033500             PERFORM H-UPPDATERA                                          
033600           END-IF                                                         
033700         ELSE                                                             
033800           PERFORM E-SAMMA-SIDA                                           
033900         END-IF                                                           
034000       END-IF                                                             
034100       IF NOT MFS-UPD-X                                                   
034110*        DISPLAY 'RRR*NOT UPD-X'                                          
034200         COMPUTE MSG-KVLL = LENGTH OF MOD-W6O17301 + 4                    
034300         PERFORM IMS-INSERT-MSG                                           
034400* UNCOMMENT BELOW TO ABEND IN CASE OF ERRORS WHEN INVOKED FROM            
034500* PROGRAMS (MFS-UPD-X).                                                   
034600*      ELSE                                                               
034700*        IF NYCKLAR-OK AND INDATA-OK                                      
034800*          CONTINUE                                                       
034810*          DISPLAY 'RRR*OK   !!!'                                         
034900*        ELSE                                                             
034910*          DISPLAY 'RRR*ERROR!!!'                                         
035000*          CALL FELLOG                                                    
035100*        END-IF                                                           
035200       END-IF                                                             
035300                                                                          
035400     END-IF                                                               
035500                                                                          
035600     MOVE ZERO TO RETURN-CODE                                             
035700     GOBACK                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 A-INIT SECTION.                                                          
036100                                                                          
036200     IF MSG-DUBBLA-TRANSKODER                                             
036300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I17301                 
036400                                             VCOM-MID-W6I17302            
036500       MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                      
036600       MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                     
036700     ELSE                                                                 
036800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I17301                  
036900                                            VCOM-MID-W6I17302             
037000       MOVE MSG-IDTRANS-1             TO MFS-IDTRANS                      
037100       MOVE MSG-KDMFSFOR-1            TO MFS-KDMFSFOR                     
037200     END-IF                                                               
037300                                                                          
037400     MOVE MSG-KDTRTYP                 TO MFS-KDTRTYP                      
037500     MOVE MSG-IDPFK                   TO MFS-IDPFK                        
037600     MOVE MFS-IDTRANS                 TO W-IDTRANS                        
037700                                                                          
037800     IF VCOM-MID                                                          
037900       MOVE ALL '+'                   TO MID-W6I17301                     
038000       MOVE VCOM-MID-IDARTNR          TO MID-IDARTNR-UT                   
038100       MOVE VCOM-MID-KVINLART         TO MID-KVINLART                     
038200       MOVE VCOM-MID-IDLEVNR          TO MID-IDLEVNR-UT                   
038300       MOVE VCOM-MID-IDOKOLLI         TO MID-IDOKOLLI-UT                  
038400       MOVE VCOM-MID-TIINLMOT         TO MID-TIINLMOT                     
038500       MOVE VCOM-MID-ADINLOMR-PRT     TO MID-ADINLOMR-PRT                 
038600       MOVE WC-CDC-SE                 TO MID-IDDC-IN                      
038700       MOVE '01'                      TO MID-KVFLETI                      
038800     END-IF                                                               
038900                                                                          
039000     MOVE LOW-VALUE                   TO MSG-AREA                         
039100     MOVE 'W6O173N1'                  TO MFS-IDMOD                        
039200     MOVE '6173'                      TO MOD-IDTRANS                      
039300     MOVE MFS-RENSA-FAELT             TO MOD-TEMFSFEL                     
039400                                         MOD-TEMFSINF                     
039500     MOVE '1'                         TO W-KDSEGKEY                       
039600                                                                          
039700     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
039800       CONTINUE                                                           
039900     ELSE                                                                 
040000       MOVE SPACE                     TO MFS-KDTRTYP                      
040100       MOVE '7'                       TO MFS-IDPFK                        
040200     END-IF                                                               
040300                                                                          
040400     PERFORM AA-INIT-NYCKLAR                                              
040500                                                                          
040600     IF MSGI-IDLAND-SPR = 'GB'                                            
040700       MOVE 'GB '                     TO MED-IDSKYLT                      
040800     ELSE                                                                 
040900       MOVE 'S  '                     TO MED-IDSKYLT                      
041000     END-IF                                                               
041100     .                                                                    
041200                                                                          
041300     EJECT                                                                
041400 AA-INIT-NYCKLAR SECTION.                                                 
041500                                                                          
041600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
041700     MOVE '001'             TO MSGI-KDCALL                                
041800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041900     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
042000     MOVE '6173'                 TO MSGI-IDTRANS                          
042100                                                                          
042200     IF MFS-IDTRANS = '6173'                                              
042300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
042400         MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                              
042500         MOVE MID-IDOKOLLI-IN TO MSGI-IDOKOLLI                            
042600         IF MID-IDARTNR-IN NOT = ALL '+'                                  
042700           IF  MID-IDLEVNR-IN = ALL '+' OR                                
042800               MID-IDOKOLLI-IN = ALL '+'                                  
042900             MOVE SPACE       TO MSGI-IDLEVNR                             
043000             MOVE ZERO        TO MSGI-IDOKOLLI                            
043100           END-IF                                                         
043200         END-IF                                                           
043300     ELSE                                                                 
043400       IF MFS-UPD-X                                                       
043500         MOVE MID-IDARTNR-UT  TO MSGI-IDARTNR                             
043600         MOVE MID-IDLEVNR-UT  TO MSGI-IDLEVNR                             
043700         MOVE MID-IDOKOLLI-UT TO MSGI-IDOKOLLI                            
043800       ELSE                                                               
043900         IF MID-IDARTNR-IN NUMERIC                                        
044000         AND MID-IDARTNR-IN > ZERO                                        
044100           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
044200         END-IF                                                           
044300         MOVE SPACE        TO MSGI-IDLEVNR                                
044400         MOVE ZERO         TO MSGI-IDOKOLLI                               
044500       END-IF                                                             
044600     END-IF                                                               
044700     MOVE FUNCTION CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                    
044800     MOVE FUNCTION CURRENT-DATE(9:4)  TO MSGI-TILOKTID                    
044900     CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                         
045000     IF MSGI-KDSVAR = 'F'                                                 
045100        MOVE 'FEL FRÅN DATUMKONV I W005INIT' TO FELTEXT                   
045200        CALL FELLOG                                                       
045300     END-IF                                                               
045400                                                                          
045500     .                                                                    
045600     EJECT                                                                
045700 B-KOLLA-NYCKLAR SECTION.                                                 
045800                                                                          
045900     MOVE JA                          TO NYCKLAR-SW                       
046000                                                                          
046100     MOVE NEJ                         TO ARTIKEL-SW                       
046200                                         MID-INPUT-SW                     
046300                                                                          
046400     MOVE SPACE                       TO MED-MFSINF                       
046500                                         MED-MFSFEL                       
046600                                                                          
046700     PERFORM BF-KONTROLL-MID-IDDC                                         
046800                                                                          
046900     PERFORM BA-KONTROLL-MID-IDARTNR                                      
047000                                                                          
047100     PERFORM BB-KONTROLL-MID-KVINLART                                     
047200                                                                          
047300     PERFORM BC-KONTROLL-MID-IDLEVNR                                      
047400                                                                          
047500     PERFORM BD-KONTROLL-MID-IDOKOLLI                                     
047600                                                                          
047610     IF MFS-UPD-X                                                         
047620        CONTINUE                                                          
047630     ELSE                                                                 
047700        IF NYCKLAR-OK                                                     
047800           PERFORM BE-KONTROLL-IDLEVNR-IDOKOLLI                           
047900        END-IF                                                            
047910     END-IF                                                               
048000                                                                          
048100                                                                          
048200     IF GODK-MID OR NYCKLAR-OK                                            
048300       MOVE MSGI-IDARTNR                TO MOD-IDARTNR-UT                 
048400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
048500       MOVE WS-KVINLART-X             TO MOD-KVINLART-UT                  
048600       INSPECT MOD-KVINLART-UT REPLACING LEADING ZERO BY SPACE            
048700       MOVE MSGI-IDLEVNR                TO MOD-IDLEVNR-UT                 
048800       MOVE MSGI-IDOKOLLI               TO MOD-IDOKOLLI-UT                
048900       INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE            
049000       MOVE MSGI-IDDC                   TO MOD-IDDC-UT                    
049100     ELSE                                                                 
049200       PERFORM MFS-RENSA-NKL-MOD-UT-FAELT                                 
049300       MOVE MSGI-IDARTNR                TO MOD-IDARTNR-UT                 
049400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
049500     END-IF                                                               
049600                                                                          
049700     IF NYCKLAR-FEL                                                       
049800        IF ARTIKEL-SAKNAS                                                 
049900          MOVE ERR-ARTIKEL-SAKNAS       TO MED-IDMFSFEL                   
050000          CALL WMEDKONV USING MED-WMEDAREA                                
050100          MOVE MED-MFSFEL               TO MOD-TEMFSFEL                   
050200          PERFORM MFS-RENSA-NKL-FAELT-UT                                  
050300        END-IF                                                            
050400                                                                          
050500        IF LEV-FEL OR KOLLI-FEL                                           
050600           MOVE ERR-WRONG-CASE-OR-SUPP  TO MED-IDMFSFEL                   
050700           CALL WMEDKONV USING MED-WMEDAREA                               
050800           MOVE MED-MFSFEL              TO MOD-TEMFSINF                   
050900        END-IF                                                            
051000                                                                          
051100        IF MED-IDMFSINF NOT = SPACE                                       
051200           CALL WMEDKONV USING MED-WMEDAREA                               
051300           MOVE MED-MFSINF              TO MOD-TEMFSINF                   
051400        END-IF                                                            
051500                                                                          
051600        PERFORM MFS-RENSA-FAELT-IN-OCH-UT                                 
051700        MOVE MFS-ROER-EJ-FAELT          TO MOD-ADINLOMR-PRT               
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 BA-KONTROLL-MID-IDARTNR SECTION.                                         
052200                                                                          
052300     MOVE MFS-RENSA-FAELT             TO MOD-IDARTNR-IN                   
052400                                                                          
052500     IF MID-IDARTNR-IN = ALL '+'                                          
052600       CONTINUE                                                           
052700     ELSE                                                                 
052800       MOVE '7'                       TO MFS-IDPFK                        
052900       MOVE SPACE                     TO MFS-KDTRTYP                      
053000     END-IF                                                               
053100                                                                          
053200     INSPECT MSGI-IDARTNR REPLACING ALL SPACE BY ZERO                     
053300                                                                          
053400     IF MSGI-IDARTNR  NUMERIC                                             
053500       MOVE MSGI-IDARTNR                TO W-IDARTNR                      
053600       PERFORM IMS-GU-ARTC01                                              
053700       IF SEGMENT-SAKNAS                                                  
053800         MOVE NEJ                       TO NYCKLAR-SW                     
053900         MOVE JA                        TO ARTIKEL-SW                     
054000*NDC                                                                      
054100       ELSE                                                               
054200         IF DCS-SDC                                                       
054300         OR DCS-NDC-NA                                                    
054400         OR DCS-NDC-PF                                                    
054410         OR DCS-NDC-OTHERS                                                
054500           PERFORM IMS-GU-WDK711                                          
054600           IF SEGMENT-SAKNAS                                              
054700             MOVE NEJ                   TO NYCKLAR-SW                     
054800             MOVE JA                    TO ARTIKEL-SW                     
054900           END-IF                                                         
055000         END-IF                                                           
055100       END-IF                                                             
055200     ELSE                                                                 
055300       MOVE NEJ                       TO NYCKLAR-SW                       
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700 BB-KONTROLL-MID-KVINLART SECTION.                                        
055800                                                                          
055900     MOVE MFS-RENSA-FAELT             TO MOD-KVINLART-IN                  
056000                                                                          
056100     IF MID-KVINLART-IN = ALL '+'                                         
056200       MOVE MID-KVINLART-UT           TO WS-KVINLART-X                    
056300       INSPECT WS-KVINLART-X REPLACING LEADING SPACE BY ZERO              
056400     ELSE                                                                 
056500       MOVE MID-KVINLART-IN           TO WS-KVINLART-X                    
056600       MOVE '7'                       TO MFS-IDPFK                        
056700       MOVE SPACE                     TO MFS-KDTRTYP                      
056800     END-IF                                                               
056900                                                                          
057000     IF WS-KVINLART-X NOT NUMERIC                                         
057100        MOVE SPACE  TO WS-KVINLART-X                                      
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 BC-KONTROLL-MID-IDLEVNR SECTION.                                         
057600                                                                          
057700     MOVE MFS-RENSA-FAELT             TO MOD-IDLEVNR-IN                   
057800                                                                          
057900     IF MID-IDLEVNR-IN = ALL '+'                                          
058000       CONTINUE                                                           
058100     ELSE                                                                 
058200       MOVE '7'                       TO MFS-IDPFK                        
058300       MOVE SPACE                     TO MFS-KDTRTYP                      
058400     END-IF                                                               
058500                                                                          
058600     IF MSGI-IDLEVNR NOT = SPACE                                          
058700        IF MSGI-IDLEVNR > '99399' AND                                     
058800           MSGI-IDLEVNR < '99600'                                         
058900           MOVE JA       TO  EGET-LEVNR-SW                                
059000                             LEV-SW                                       
059100        ELSE                                                              
059200           MOVE NEJ      TO  EGET-LEVNR-SW                                
059300           MOVE JA       TO  LEV-SW                                       
059400        END-IF                                                            
059500     ELSE                                                                 
059600        MOVE JA          TO  EGET-LEVNR-SW                                
059700                             LEV-SW                                       
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 BD-KONTROLL-MID-IDOKOLLI SECTION.                                        
060200                                                                          
060300     MOVE MFS-RENSA-FAELT             TO MOD-IDOKOLLI-IN                  
060400                                                                          
060500     IF MID-IDOKOLLI-IN = ALL '+'                                         
060600       CONTINUE                                                           
060700     ELSE                                                                 
060800       MOVE '7'                       TO MFS-IDPFK                        
060900       MOVE SPACE                     TO MFS-KDTRTYP                      
061000     END-IF                                                               
061100                                                                          
061200     INSPECT MSGI-IDOKOLLI REPLACING LEADING SPACE BY ZERO                
061300                                                                          
061400     IF MSGI-IDOKOLLI NUMERIC                                             
061500        IF MSGI-IDOKOLLI > ZERO                                           
061600           MOVE JA  TO  KOLLINR-SW                                        
061700                        KOLLI-SW                                          
061800        ELSE                                                              
061900           MOVE NEJ TO  KOLLINR-SW                                        
062000           MOVE JA  TO  KOLLI-SW                                          
062100        END-IF                                                            
062200     ELSE                                                                 
062300        MOVE NEJ    TO  NYCKLAR-SW                                        
062400        MOVE NEJ    TO  KOLLI-SW                                          
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 BE-KONTROLL-IDLEVNR-IDOKOLLI SECTION.                                    
062900                                                                          
063000     IF MSGI-IDLEVNR NOT = SPACE                                          
063100        IF KOLLINR-FINNS                                                  
063200           PERFORM BEA-KONTROLL-IDOKOLLI                                  
063300           PERFORM BEB-KONTROLL-INLEV                                     
063400        ELSE                                                              
063500           MOVE ERR-WRONG-CASE-OR-SUPP TO MED-IDMFSINF                    
063600           MOVE NEJ                    TO NYCKLAR-SW                      
063700        END-IF                                                            
063800     ELSE                                                                 
063900        IF KOLLINR-FINNS                                                  
064000           MOVE ERR-WRONG-CASE-OR-SUPP    TO MED-IDMFSINF                 
064100           MOVE NEJ                       TO NYCKLAR-SW                   
064200        END-IF                                                            
064300     END-IF                                                               
064400     .                                                                    
064500     EJECT                                                                
064600 BEA-KONTROLL-IDOKOLLI SECTION.                                           
064700                                                                          
064800     MOVE '6017'                    TO W-IDHTYP-01                        
064900     PERFORM IMS-GU-LOPA11                                                
065000     IF MSGI-IDOKOLLI > 6018-IDOKOLLI AND EGET-LEVNR                      
065100       MOVE NEJ                     TO NYCKLAR-SW                         
065200       MOVE ERR-WRONG-CASE-OR-SUPP  TO MED-IDMFSINF                       
065300     END-IF                                                               
065400     .                                                                    
065500     SKIP3                                                                
065600 BEB-KONTROLL-INLEV SECTION.                                              
065700                                                                          
065800     MOVE MSGI-IDOKOLLI              TO W-IDOKOLLIK                       
065900                                        W-IDOKOLLI                        
066000     MOVE MSGI-IDLEVNR               TO W-IDLEVNRK                        
066100                                        W-IDLEVNR                         
066200     PERFORM IMS-GU-INLA11-W6D1                                           
066300     IF SEGMENT-FINNS                                                     
066400       PERFORM IMS-GNP-INLA21-W6D1                                        
066500       PERFORM UNTIL SEGMENT-SAKNAS                                       
066600         IF RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'                        
066700           MOVE NEJ                     TO NYCKLAR-SW                     
066800           MOVE ERR-CASE-NOT-STORED     TO MED-IDMFSINF                   
066900         END-IF                                                           
067000         PERFORM IMS-GNP-INLA21-W6D1                                      
067100       END-PERFORM                                                        
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500 BF-KONTROLL-MID-IDDC    SECTION.                                         
067600                                                                          
067700     MOVE MFS-RENSA-FAELT             TO MOD-IDDC-IN                      
067800                                                                          
067900     IF MID-IDDC-IN = ALL '+'                                             
068000       MOVE MSGI-IDDC        TO W-IDDC-B6                                 
068100     ELSE                                                                 
068200       MOVE MID-IDDC-IN      TO W-IDDC-B6                                 
068300     END-IF                                                               
068400     PERFORM IMS-GU-WDB601                                                
068500                                                                          
068600     IF SEGMENT-FINNS                                                     
068700     AND (DCS-CDC                                                         
068800     OR   DCS-SDC                                                         
068900     OR   DCS-NDC-NA                                                      
069000     OR   DCS-NDC-PF                                                      
069010     OR   DCS-NDC-OTHERS)                                                 
069100       MOVE W-IDDC-B6        TO W-IDDC                                    
069110                                WS-IDDC                                   
069120       IF NDC-CN OR LDC-CN                                                
069130         MOVE WC-LAND-CN   TO W-IDLAND                                    
069140       ELSE                                                               
069150         IF NDC-US                                                        
069160           MOVE WC-LAND-US TO W-IDLAND                                    
069170         ELSE                                                             
069180           MOVE WC-LAND-SE TO W-IDLAND                                    
069190         END-IF                                                           
069191       END-IF                                                             
069200     ELSE                                                                 
069300       MOVE NEJ              TO NYCKLAR-SW                                
069400     END-IF                                                               
069500     .                                                                    
069600     EJECT                                                                
069700 E-SAMMA-SIDA SECTION.                                                    
069800                                                                          
069900     PERFORM S01-KONTROLL-OM-INDATA                                       
070000                                                                          
070100     IF MID-INPUT-SAKNAS AND EGEN-MID                                     
070200       MOVE JA                        TO ALLT-SW                          
070300       PERFORM MFS-RENSA-FAELT-IN-OCH-UT                                  
070400       MOVE MFS-ROER-EJ-FAELT         TO MOD-ADINLOMR-PRT                 
070500       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-ADINLOMR-PRT-ATTR            
070600     ELSE                                                                 
070700       IF EGEN-MID                                                        
070800         MOVE NEJ                       TO ALLT-SW                        
070900         MOVE INF-PRESS-PF11            TO MED-IDMFSINF                   
071000         CALL WMEDKONV USING MED-WMEDAREA                                 
071100         MOVE MED-MFSINF                TO MOD-TEMFSINF                   
071200         PERFORM MFS-ROER-EJ-FAELT-IN-OCH-UT                              
071300         PERFORM MFS-LAES-IN-IGEN                                         
071400       ELSE                                                               
071500         MOVE JA                        TO ALLT-SW                        
071600         PERFORM MFS-RENSA-FAELT-IN-OCH-UT                                
071700       END-IF                                                             
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 G-KOLLA-INPUT SECTION.                                                   
072200                                                                          
072300     MOVE JA                          TO INDATA-SW                        
072400     PERFORM S01-KONTROLL-OM-INDATA                                       
072500                                                                          
072600     IF MID-INPUT-SAKNAS                                                  
072700       MOVE ERR-PF11-AND-NO-DATA      TO MED-IDMFSFEL                     
072800       CALL WMEDKONV USING MED-WMEDAREA                                   
072900       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
073000       PERFORM MFS-ROER-EJ-FAELT-IN-OCH-UT                                
073100       MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-ADINLOMR-PRT-ATTR            
073200       MOVE NEJ                       TO INDATA-SW                        
073300     ELSE                                                                 
073400       PERFORM IMS-GU-ARTC01                                              
073500       IF SEGMENT-FINNS                                                   
073600           IF DCS-SDC                                                     
073700           OR DCS-NDC-NA                                                  
073800           OR DCS-NDC-PF                                                  
073810           OR DCS-NDC-OTHERS                                              
073900             PERFORM IMS-GU-WDK701                                        
074000           END-IF                                                         
074100           IF SEGMENT-FINNS                                               
074200                                                                          
074300             PERFORM GA-KONTROLL-ADINLOMR-PRT                             
074400                                                                          
074500             PERFORM GB-KONTROLL-KVINLART                                 
074600                                                                          
074700             PERFORM GC-KONTROLL-TIINLMOT                                 
074800                                                                          
074900             PERFORM GD-KONTROLL-IDLEVNR                                  
075000                                                                          
075100             PERFORM GE-KONTROLL-OMRADE                                   
075200                                                                          
075300             PERFORM GG-KONTROLL-KVFLETI                                  
075400           ELSE                                                           
075500             MOVE NEJ TO INDATA-SW                                        
075600           END-IF                                                         
075700                                                                          
075800         IF INDATA-FEL                                                    
075900           CALL WMEDKONV USING MED-WMEDAREA                               
076000           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
076100           PERFORM MFS-ROER-EJ-FAELT-IN-OCH-UT                            
076200           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR            
076300         END-IF                                                           
076400       ELSE                                                               
076500         MOVE NEJ TO INDATA-SW                                            
076600         MOVE ERR-ARTIKEL-SAKNAS      TO MED-IDMFSFEL                     
076700         CALL WMEDKONV USING MED-WMEDAREA                                 
076800         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
076900         PERFORM MFS-RENSA-FAELT-IN-OCH-UT                                
077000       END-IF                                                             
077100     END-IF                                                               
077200     .                                                                    
077300     SKIP3                                                                
077400 GA-KONTROLL-ADINLOMR-PRT SECTION.                                        
077500                                                                          
077600     IF MID-ADINLOMR-PRT = ALL '+'                                        
077700       MOVE MFS-ALFA-FAELT-FEL        TO MOD-ADINLOMR-PRT-ATTR            
077800       MOVE NEJ                       TO INDATA-SW                        
077900       MOVE ERR-CORR-HILITE-FLDS      TO MED-IDMFSFEL                     
078000     ELSE                                                                 
078100       MOVE 001              TO PRT-KDCALL                                
078200       MOVE SPACE            TO PRT-IDPRTLST                              
078300       MOVE '6F'             TO PRT-IDPRTLST(1:2)                         
078400       MOVE MID-ADINLOMR-PRT TO PRT-IDPRTLST(3:4)                         
078500                                W-ADINLOMR-6006                           
078600       CALL W006PRT USING PRT-W006PRT                                     
078700       IF PRT-KDSVAR = 'F'                                                
078800         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                 
078900         MOVE NEJ TO INDATA-SW                                            
079000         MOVE ERR-PRT-MISSING TO MED-IDMFSFEL                             
079100       ELSE                                                               
079200         MOVE PRT-BEPRTLST TO MOD-TEMFSFEL                                
079300         MOVE PRT-IDPRTLST TO WS-IDPRTLST                                 
079400         MOVE MFS-ALFA-FAELT-RAETT      TO MOD-ADINLOMR-PRT-ATTR          
079500       END-IF                                                             
079600     END-IF                                                               
079700                                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 GB-KONTROLL-KVINLART SECTION.                                            
080100                                                                          
080200     IF MID-KVINLART = ALL '+'                                            
080300       MOVE MFS-NUM-FAELT-FEL         TO MOD-KVINLART-ATTR                
080400       MOVE NEJ                       TO INDATA-SW                        
080500     ELSE                                                                 
080600       MOVE MID-KVINLART              TO WS-KVINLART                      
080700       IF WS-KVINLART NUMERIC                                             
080800         MOVE MFS-NUM-FAELT-RAETT     TO MOD-KVINLART-ATTR                
080900       ELSE                                                               
081000         MOVE MFS-NUM-FAELT-FEL       TO MOD-KVINLART-ATTR                
081100         MOVE NEJ                     TO INDATA-SW                        
081200         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
081300       END-IF                                                             
081400     END-IF                                                               
081500                                                                          
081600     IF MID-KVINLART-LAST = ALL '+'                                       
081700       MOVE MFS-NUM-FAELT-RAETT       TO MOD-KVINLART-LAST-ATTR           
081800     ELSE                                                                 
081900       MOVE MID-KVINLART-LAST         TO WS-KVINLART-LAST                 
082000       IF WS-KVINLART-LAST NUMERIC                                        
082100         MOVE MFS-NUM-FAELT-RAETT     TO MOD-KVINLART-LAST-ATTR           
082200       ELSE                                                               
082300         MOVE MFS-NUM-FAELT-FEL       TO MOD-KVINLART-LAST-ATTR           
082400         MOVE NEJ                     TO INDATA-SW                        
082500         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
082600       END-IF                                                             
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 GC-KONTROLL-TIINLMOT SECTION.                                            
083100                                                                          
083200     IF MID-TIINLMOT = ALL '+'                                            
083300       MOVE MFS-NUM-FAELT-FEL         TO MOD-TIINLMOT-ATTR                
083400       MOVE NEJ                       TO INDATA-SW                        
083500     ELSE                                                                 
083600       IF MID-TIINLMOT NUMERIC                                            
083700         MOVE MID-TIINLMOT            TO DAT-I-TIDATUM                    
083800         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
083900         CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                 
084000                             DAT-O-TIDATUM, DAT-KDSVAR                    
084100         IF DAT-KDSVAR-OK                                                 
084200           MOVE MFS-NUM-FAELT-RAETT   TO MOD-TIINLMOT-ATTR                
084300*DATUM-JÄMFÖRELSE FÖR ATT KLARA ÅR 2000                                   
084400           MOVE MID-TIINLMOT          TO TMP1-YYMMDD                      
084500           MOVE MSGI-TILOKDAT         TO TMP2-YYMMDD                      
084600           PERFORM WY2000P1                                               
084700           IF TMP1-YYMMDD  <= TMP2-YYMMDD                                 
084800             MOVE MID-TIINLMOT        TO WS-TIINLMOT                      
084900             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIINLMOT-ATTR                
085000           ELSE                                                           
085100             MOVE MFS-NUM-FAELT-FEL   TO MOD-TIINLMOT-ATTR                
085200             MOVE NEJ                 TO INDATA-SW                        
085300             MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                   
085400           END-IF                                                         
085500         ELSE                                                             
085600           MOVE MFS-NUM-FAELT-FEL     TO MOD-TIINLMOT-ATTR                
085700           MOVE NEJ                   TO INDATA-SW                        
085800           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
085900         END-IF                                                           
086000       ELSE                                                               
086100         MOVE MFS-NUM-FAELT-FEL       TO MOD-TIINLMOT-ATTR                
086200         MOVE NEJ                     TO INDATA-SW                        
086300         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
086400       END-IF                                                             
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800 GD-KONTROLL-IDLEVNR SECTION.                                             
086900                                                                          
087000     IF MSGI-IDLEVNR    = SPACE                                           
087100       MOVE W-IDDC       TO W-IDDC-6005                                   
087200       PERFORM IMS-GU-PLAA01                                              
087300       PERFORM IMS-GNP-PLAA11                                             
087400       IF SEGMENT-FINNS                                                   
087500         MOVE 6006-IDLEVNR            TO MSGI-IDLEVNR                     
087600       ELSE                                                               
087700*******  FEL PRINTER-OMR ANGIVET                                          
087800         MOVE MFS-ALFA-FAELT-FEL      TO MOD-ADINLOMR-PRT-ATTR            
087900         MOVE NEJ                     TO INDATA-SW                        
088000         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
088100       END-IF                                                             
088200     ELSE                                                                 
088300       MOVE MSGI-IDLEVNR TO MOD6194-MID-IDLEVNR-KOLLI(1)                  
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700 GE-KONTROLL-OMRADE SECTION.                                              
088800     IF MID-ADTRDEST = ALL '+' OR 'SVS' OR 'HF'                           
088900       MOVE MFS-ALFA-FAELT-RAETT      TO MOD-ADTRDEST-ATTR                
089000     ELSE                                                                 
089100       MOVE MFS-ALFA-FAELT-FEL        TO MOD-ADTRDEST-ATTR                
089200       MOVE NEJ                       TO INDATA-SW                        
089300       MOVE ERR-CORR-HILITE-FLDS      TO MED-IDMFSFEL                     
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 GG-KONTROLL-KVFLETI SECTION.                                             
089800                                                                          
089900     MOVE MFS-NUM-FAELT-RAETT TO MOD-KVFLETI-ATTR                         
090000                                                                          
090100     IF MID-KVFLETI = ALL '+'                                             
090200       MOVE +1 TO WS-KVFLETI                                              
090300     ELSE                                                                 
090400       IF MID-KVFLETI NUMERIC                                             
090500         MOVE MID-KVFLETI TO WS-KVFLETI                                   
090600         IF WS-KVFLETI > 1                                                
090700           IF MSGI-IDOKOLLI = ZERO                                        
090800             CONTINUE                                                     
090900           ELSE                                                           
091000              MOVE MFS-NUM-FAELT-FEL     TO MOD-KVFLETI-ATTR              
091100              MOVE NEJ                   TO INDATA-SW                     
091200              MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                  
091300           END-IF                                                         
091400         END-IF                                                           
091500       ELSE                                                               
091600         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVFLETI-ATTR                   
091700         MOVE NEJ                   TO INDATA-SW                          
091800         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
091900       END-IF                                                             
092000     END-IF                                                               
092100     .                                                                    
092200     EJECT                                                                
092300 H-UPPDATERA SECTION.                                                     
092400                                                                          
092500     MOVE SPACE                       TO MOD6194-MID-W6I19401             
092600     MOVE 1                           TO MOD6194-MID-KVPOST               
092700     MOVE 'W6017300'                  TO MOD6194-MID-IDPGM                
092800     MOVE WS-IDPRTLST                 TO MOD6194-MID-IDPRTLST             
092900                                                                          
093000     PERFORM IMS-GU-ARTC01                                                
093100     IF SEGMENT-FINNS                                                     
093200       MOVE ARTC01-ART-KDSORT         TO WS-KDSORT                        
093300       PERFORM IMS-GNP-ARTC11                                             
093400       IF SEGMENT-FINNS                                                   
093500         COMPUTE WS-VKKOLLIN = ARTC11-CLAG-VKART * WS-KVINLART            
093600                                              / 1000                      
093700         IF DCS-CDC                                                       
093800           IF MID-ADTRDEST = 'SVS'                                        
093900             MOVE ARTC11-CLAG-ADLAGOMR-SVS  TO WS-ADLAGOMR                
094000             MOVE ARTC11-CLAG-ADGANG-SVS    TO WS-ADGANG                  
094100             MOVE ARTC11-CLAG-ADPLATS-SVS   TO WS-ADPLATS                 
094200            ELSE                                                          
094300             MOVE ARTC11-CLAG-ADLAGOMR      TO WS-ADLAGOMR                
094400             MOVE ARTC11-CLAG-ADGANG        TO WS-ADGANG                  
094500             MOVE ARTC11-CLAG-ADPLATS       TO WS-ADPLATS                 
094600            END-IF                                                        
094700         ELSE                                                             
094710           PERFORM IMS-GU-WDK712                                          
094720           IF SEGMENT-FINNS                                               
094730             IF LART-VKART > 0                                            
094740                                                                          
094750               COMPUTE WS-VKKOLLIN = (LART-VKART *                        
094760                                     WS-KVINLART) / 1000                  
094780               MOVE JA TO WS-FL-WDK712                                    
094790             END-IF                                                       
094791           END-IF                                                         
094800           PERFORM IMS-GU-WDK711                                          
094900           IF SEGMENT-FINNS                                               
095000             MOVE SLAG-ADLAGOMR    TO WS-ADLAGOMR                         
095100             MOVE SLAG-ADGANG      TO WS-ADGANG                           
095200             MOVE SLAG-ADPLATS     TO WS-ADPLATS                          
095300             COMPUTE WS-VKKOLLIN = WS-VKKOLLIN * CONV-GR-TO-LB            
095400           END-IF                                                         
095500         END-IF                                                           
095600         MOVE ARTC11-CLAG-BEFT            TO WS-BEFT                      
095700       END-IF                                                             
095800     END-IF                                                               
095900                                                                          
096000     IF WS-KVFLETI = ZERO                                                 
096100       MOVE +1           TO WS-KVFLETI                                    
096200     END-IF                                                               
096300                                                                          
096400     MOVE +1             TO IX                                            
096500                                                                          
096600     PERFORM UNTIL IX > WS-KVFLETI                                        
096700                                                                          
096800       ADD +1            TO 6194-IX                                       
096900                                                                          
097000       IF MSGI-IDOKOLLI = ZERO                                            
097100         PERFORM HB-BERAKNA-NYTT-IDOKOLLI                                 
097200       ELSE                                                               
097300         MOVE MSGI-IDOKOLLI     TO MOD6194-MID-IDOKOLLI(6194-IX)          
097400       END-IF                                                             
097500                                                                          
097600       MOVE WS-ADLAGOMR         TO MOD6194-MID-ADLAGOMR(6194-IX)          
097700       MOVE WS-ADGANG           TO MOD6194-MID-ADGANG  (6194-IX)          
097800       MOVE WS-ADPLATS          TO MOD6194-MID-ADPLATS (6194-IX)          
097900       MOVE WS-KDSORT           TO MOD6194-MID-KDSORT  (6194-IX)          
098000       MOVE WS-BEFT             TO MOD6194-MID-BEFT    (6194-IX)          
098100                                                                          
098200       MOVE MSGI-IDLEVNR    TO MOD6194-MID-IDLEVNR-KOLLI(6194-IX)         
098300                                                                          
098400       MOVE MSGI-IDARTNR         TO MOD6194-MID-IDARTNR(6194-IX)          
098500                                                                          
098600       MOVE WS-KVINLART          TO MOD6194-MID-KVINLART(6194-IX)         
098700       IF IX = WS-KVFLETI AND WS-KVFLETI > +1                             
098800         IF WS-KVINLART-LAST > +0                                         
098900            MOVE WS-KVINLART-LAST TO MOD6194-MID-KVINLART(6194-IX)        
098910            IF WS-FL-WDK712 = JA                                          
098920              COMPUTE WS-VKKOLLIN = (LART-VKART                           
098930                                    * WS-KVINLART-LAST) / 1000            
098931            ELSE                                                          
099000              COMPUTE WS-VKKOLLIN = ARTC11-CLAG-VKART                     
099001                                    * WS-KVINLART-LAST / 1000             
099010            END-IF                                                        
099200         END-IF                                                           
099300       END-IF                                                             
099400                                                                          
099500       MOVE WS-TIINLMOT          TO MOD6194-MID-TIINLMOT(6194-IX)         
099600                                                                          
099700       MOVE WS-VKKOLLIN          TO MOD6194-MID-VKKOLLIN(6194-IX)         
099800                                                                          
099900       MOVE ZERO                 TO MOD6194-MID-VKKOLLIB(6194-IX)         
100000                                    MOD6194-MID-IDLOPNRM(6194-IX)         
100100       IF WS-KVFLETI > +1                                                 
100200          MOVE WS-KVFLETI       TO  MOD6194-MID-IDLOPNRM(6194-IX)         
100300          ADD  +900             TO  MOD6194-MID-IDLOPNRM(6194-IX)         
100400       END-IF                                                             
100500                                                                          
100600       IF 6194-IX > 14                                                    
100700         PERFORM HA-P-TO-P-6194                                           
100800                                                                          
100900         IF FOERSTA-6194                                                  
101000           PERFORM IMS-ISRT-ALT-MSG-6194                                  
101100           MOVE NEJ TO 6194-SW                                            
101200         ELSE                                                             
101300           PERFORM IMS-PURG-6194-MSG                                      
101400         END-IF                                                           
101500       END-IF                                                             
101600                                                                          
101700       ADD +1                   TO IX                                     
101800                                                                          
101900     END-PERFORM                                                          
102000                                                                          
102100     IF 6194-IX > 0                                                       
102200       PERFORM HA-P-TO-P-6194                                             
102300                                                                          
102400       IF FOERSTA-6194                                                    
102500         PERFORM IMS-ISRT-ALT-MSG-6194                                    
102600         MOVE NEJ TO 6194-SW                                              
102700       ELSE                                                               
102800         PERFORM IMS-PURG-6194-MSG                                        
102900       END-IF                                                             
103000     END-IF                                                               
103100                                                                          
103200     IF FL-LOPA-UPPDATERAD = JA                                           
103300       PERFORM IMS-REPL-LOPA                                              
103400     END-IF                                                               
103500                                                                          
103600     MOVE MFS-ROER-EJ-FAELT           TO MOD-ADINLOMR-PRT                 
103700     MOVE MFS-RENSA-FAELT             TO MOD-KVINLART                     
103800                                         MOD-TIINLMOT                     
103900                                         MOD-KVFLETI                      
104000                                         MOD-KVINLART-LAST                
104100                                         MOD-ADTRDEST                     
104100                                         MOD-IDARTNR-UT                   
104200                                                                          
104300     MOVE INF-UPDATE-DONE             TO MED-IDMFSINF                     
104400     CALL WMEDKONV USING MED-WMEDAREA                                     
104500     MOVE MED-MFSINF                  TO MOD-TEMFSINF                     
104600     PERFORM MFS-FORM-ATTR                                                
104700     .                                                                    
104800     EJECT                                                                
104900 HA-P-TO-P-6194 SECTION.                                                  
105000                                                                          
105100     COMPUTE P-TO-P-KVLL = LNG-P-TO-P-PREFIX + 23 + (67 * 6194-IX)        
105200                                                                          
105300     MOVE 'W6T194X '                  TO P-TO-P-KDTRANS                   
105400     MOVE '6173'                      TO P-TO-P-IDTRANS                   
105500     MOVE MFS-KDMFSFOR                TO P-TO-P-KDMFSFOR                  
105600                                                                          
105700     MOVE 6194-IX                     TO MOD6194-MID-KVPOST               
105800     MOVE MOD6194-MID-W6I19401        TO P-TO-P-DATA                      
105900                                                                          
106000     MOVE +0                          TO 6194-IX                          
106100     .                                                                    
106200     SKIP3                                                                
106300 HB-BERAKNA-NYTT-IDOKOLLI SECTION.                                        
106400                                                                          
106500     IF FL-LOPA-UPPDATERAD = NEJ                                          
106600       MOVE '6017'                      TO W-IDHTYP-01                    
106700       PERFORM IMS-GHU-LOPA11                                             
106800                                                                          
106900       ADD 1                    TO 6018-IDOKOLLI                          
107000       MOVE 6018-IDOKOLLI       TO MOD6194-MID-IDOKOLLI(6194-IX)          
107100                                                                          
107200       MOVE JA                  TO FL-LOPA-UPPDATERAD                     
107300     ELSE                                                                 
107400       ADD 1                    TO 6018-IDOKOLLI                          
107500       MOVE 6018-IDOKOLLI       TO MOD6194-MID-IDOKOLLI(6194-IX)          
107600     END-IF                                                               
107700     .                                                                    
107800     EJECT                                                                
107900 S01-KONTROLL-OM-INDATA SECTION.                                          
108000                                                                          
108100     IF MID-KVINLART     = ALL '+' AND                                    
108200        MID-TIINLMOT     = ALL '+' AND                                    
108300        MID-KVFLETI      = ALL '+' AND                                    
108400        MID-KVINLART-LAST = ALL '+'                                       
108500       MOVE JA                        TO MID-INPUT-SW                     
108600     ELSE                                                                 
108700       MOVE NEJ                       TO MID-INPUT-SW                     
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100* -COPY WY2000P1                                                          
109200                                                                          
109300 MFS-RENSA-FAELT-IN-OCH-UT SECTION.                                       
109400                                                                          
109500*    --- IN- OCH UTDATA-FÄLT ÄR GEMENSAMMA. PRINTERUPPGIFT                
109600*        SPARAS ALLTID I BILDEN.                                          
109700     MOVE MFS-RENSA-FAELT             TO MOD-KVINLART                     
109800                                         MOD-TIINLMOT                     
109900                                         MOD-KVFLETI                      
110000                                         MOD-KVINLART-LAST                
110100                                         MOD-ADTRDEST                     
110200     .                                                                    
110300     SKIP3                                                                
110400 MFS-RENSA-NKL-FAELT-UT SECTION.                                          
110500                                                                          
110600*    ---  NYCKEL-UT-FÄLT UTOM IDARTNR                                     
110700     MOVE MFS-RENSA-FAELT             TO MOD-KVINLART-UT                  
110800                                         MOD-IDLEVNR-UT                   
110900                                         MOD-IDOKOLLI-UT                  
111000     .                                                                    
111100     SKIP3                                                                
111200 MFS-RENSA-NKL-MOD-UT-FAELT SECTION.                                      
111300                                                                          
111400*    ---  NYCKLAR UT-FÄLT                                                 
111500     MOVE MFS-RENSA-FAELT             TO MOD-KVINLART-UT                  
111600                                         MOD-IDLEVNR-UT                   
111700                                         MOD-IDOKOLLI-UT                  
111800                                         MOD-IDDC-UT                      
111900     .                                                                    
112000     SKIP3                                                                
112100 MFS-ROER-EJ-FAELT-IN-OCH-UT  SECTION.                                    
112200                                                                          
112300*    --- IN- OCH UTDATA-FÄLT ÄR GEMENSAMMA.                               
112400     MOVE MFS-ROER-EJ-FAELT           TO MOD-ADINLOMR-PRT                 
112500                                         MOD-KVINLART                     
112600                                         MOD-TIINLMOT                     
112700                                         MOD-KVFLETI                      
112800                                         MOD-KVINLART-LAST                
112900                                         MOD-ADTRDEST                     
113000     .                                                                    
113100     SKIP3                                                                
113200 MFS-FORM-ATTR SECTION.                                                   
113300                                                                          
113400*    --- ALLA INDATA-FÄLT                                                 
113500     MOVE MFS-FORMATETS-ATTR          TO MOD-ADINLOMR-PRT-ATTR            
113600                                         MOD-KVINLART-ATTR                
113700                                         MOD-TIINLMOT-ATTR                
113800                                         MOD-KVFLETI-ATTR                 
113900                                         MOD-KVINLART-LAST-ATTR           
114000                                         MOD-ADTRDEST-ATTR                
114100     .                                                                    
114200     SKIP3                                                                
114300 MFS-LAES-IN-IGEN SECTION.                                                
114400                                                                          
114500*    --- ALLA INDATA-FÄLT                                                 
114600     MOVE MFS-ADD-LAES-IN-FAELT       TO MOD-ADINLOMR-PRT-ATTR            
114700                                         MOD-KVINLART-ATTR                
114800                                         MOD-TIINLMOT-ATTR                
114900                                         MOD-KVFLETI-ATTR                 
115000                                         MOD-KVINLART-LAST-ATTR           
115100                                         MOD-ADTRDEST-ATTR                
115200     .                                                                    
115300     EJECT                                                                
115400* --- IMS SEKTIONER ---                                                   
115500     SKIP3                                                                
115600 IMS-GET-MSG SECTION.                                                     
115700                                                                          
115800     MOVE '  QC' TO GODK-STATUSKODER                                      
115900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
116000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300     SKIP3                                                                
116400 IMS-INSERT-MSG SECTION.                                                  
116500                                                                          
116600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
116700       MOVE '0' TO MFS-KDHUVOMR                                           
116800     END-IF                                                               
116900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
117000     MOVE SPACE TO GODK-STATUSKODER                                       
117100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
117200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500     EJECT                                                                
117600 IMS-ISRT-ALT-MSG-6194 SECTION.                                           
117700                                                                          
117800     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
117900     MOVE    '  '             TO    GODK-STATUSKODER                      
118000     CALL    CBLTDLI          USING ISRT 6194-PCB P-TO-P-SW               
118100     MOVE    6194-STATUS-CODE TO    STATUS-WS                             
118200     PERFORM IMS-STATUSKONTROLL                                           
118300     .                                                                    
118400     SKIP3                                                                
118500 IMS-PURG-6194-MSG SECTION.                                               
118600     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
118700     MOVE SPACE TO GODK-STATUSKODER                                       
118800     CALL CBLTDLI USING PURG 6194-PCB P-TO-P-SW                           
118900     MOVE 6194-STATUS-CODE TO STATUS-WS                                   
119000     PERFORM IMS-STATUSKONTROLL                                           
119100     .                                                                    
119200     EJECT                                                                
119300 IMS-GU-PLAA01 SECTION.                                                   
119400                                                                          
119500     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     MOVE '  GE' TO GODK-STATUSKODER                                      
119800     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-1 SSA1                    
119900     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
120000     PERFORM IMS-STATUSKONTROLL                                           
120100     .                                                                    
120200     SKIP3                                                                
120300 IMS-GNP-PLAA11 SECTION.                                                  
120400                                                                          
120500     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
120600          DELIMITED BY SIZE INTO SSA2                                     
120700     MOVE '  GE' TO GODK-STATUSKODER                                      
120800     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA-1 SSA1 SSA2              
120900     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
121000     PERFORM IMS-STATUSKONTROLL                                           
121100     .                                                                    
121200     EJECT                                                                
121300 IMS-GHU-LOPA11 SECTION.                                                  
121400                                                                          
121500     STRING 'W6LOPA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
121600          DELIMITED BY SIZE INTO SSA1                                     
121700     MOVE 'W6LOPA11 '      TO SSA2                                        
121800     MOVE '  GE' TO GODK-STATUSKODER                                      
121900     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA-2 SSA1 SSA2              
122000     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSKONTROLL                                           
122200     .                                                                    
122300     SKIP2                                                                
122400 IMS-GU-LOPA11 SECTION.                                                   
122500                                                                          
122600     STRING 'W6LOPA01(W6GXKEY  =' W-W6GX01KEY-X ')'                       
122700          DELIMITED BY SIZE INTO SSA1                                     
122800     MOVE 'W6LOPA11 '      TO SSA2                                        
122900     MOVE '  ' TO GODK-STATUSKODER                                        
123000     CALL CBLTDLI USING GU LOPA-PCB DLI-IO-AREA-2 SSA1 SSA2               
123100     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
123200     PERFORM IMS-STATUSKONTROLL                                           
123300     .                                                                    
123400     SKIP2                                                                
123500 IMS-REPL-LOPA SECTION.                                                   
123600                                                                          
123700     MOVE '  ' TO GODK-STATUSKODER                                        
123800     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA-2                       
123900     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
124000     PERFORM IMS-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300 IMS-GU-ARTC01 SECTION.                                                   
124400                                                                          
124500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
124600          DELIMITED BY SIZE INTO SSA1                                     
124700     MOVE '  GE' TO GODK-STATUSKODER                                      
124800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1                    
124900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
125000     PERFORM IMS-STATUSKONTROLL                                           
125100     .                                                                    
125200     SKIP2                                                                
125300 IMS-GNP-ARTC11 SECTION.                                                  
125400                                                                          
125500     MOVE 'WLARTC11 ' TO SSA1                                             
125600     MOVE '  GE' TO GODK-STATUSKODER                                      
125700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-1 SSA1                   
125800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     SKIP2                                                                
126200 IMS-GU-WDK701 SECTION.                                                   
126300                                                                          
126400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
126500          DELIMITED BY SIZE INTO SSA1                                     
126600     MOVE '  GE' TO GODK-STATUSKODER                                      
126700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
126800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126900     PERFORM IMS-STATUSKONTROLL                                           
127000     .                                                                    
127100     SKIP2                                                                
127200 IMS-GU-WDK711 SECTION.                                                   
127300                                                                          
127400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
127500          DELIMITED BY SIZE INTO SSA1                                     
127600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
127700          DELIMITED BY SIZE INTO SSA2                                     
127800     MOVE '  GE' TO GODK-STATUSKODER                                      
127900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
128000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
128100     PERFORM IMS-STATUSKONTROLL                                           
128200     .                                                                    
128300     SKIP2                                                                
128310 IMS-GU-WDK712 SECTION.                                                   
128320     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
128330          DELIMITED BY SIZE INTO SSA1                                     
128340     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
128350          DELIMITED BY SIZE INTO SSA2                                     
128360     MOVE '  GE' TO GODK-STATUSKODER                                      
128370     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
128380     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
128390     PERFORM IMS-STATUSKONTROLL                                           
128391     .                                                                    
128400 IMS-GU-INLA11-W6D1 SECTION.                                              
128500     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
128600          DELIMITED BY SIZE INTO SSA1                                     
128700     MOVE '  GE' TO GODK-STATUSKODER                                      
128800     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-1 SSA1                    
128900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
129000     PERFORM IMS-STATUSKONTROLL                                           
129100     .                                                                    
129200     SKIP3                                                                
129300 IMS-GNP-INLA21-W6D1 SECTION.                                             
129400     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-X                             
129500            '&IDOKOLLI =' W-IDOKOLLI-X ')'                                
129600          DELIMITED BY SIZE INTO SSA1                                     
129700     MOVE '  GE' TO GODK-STATUSKODER                                      
129800     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA-1 SSA1                   
129900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 IMS-GU-WDB601    SECTION.                                                
130500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
130600          DELIMITED BY SIZE INTO SSA1                                     
130700     MOVE '  GE' TO GODK-STATUSKODER                                      
130800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
130900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     EJECT                                                                
131300 IMS-STATUSKONTROLL SECTION.                                              
131400                                                                          
131500     SET STATUS-IX TO 1                                                   
131600     SEARCH GODK-STATUS                                                   
131700       AT END                                                             
131800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
131900         DELIMITED BY SIZE INTO FELTEXT                                   
132000         CALL FELLOG                                                      
132100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
132200         CONTINUE                                                         
132300     END-SEARCH                                                           
132400     .                                                                    
